##########################################################
################### VENTAS MENSUALES #####################
##########################################################

# =========================
# Librerías
# =========================
library(shiny)
library(shinyWidgets)
library(DT)
library(dplyr)
library(tidyr)
library(purrr)
library(shinyjs)
library(formattable)
library(ggplot2)
library(plotly)
library(lubridate)
library(stringr)
library(stringi)
library(scales)
library(rlang)
library(digest)

# =========================
# Datos
# =========================
base <- readRDS("Ventas_mensuales.rds") %>%
  mutate(
    SKU = as.character(SKU),
    Codigo_Femaco = as.character(Codigo_Femaco),
    Producto = paste0(SKU, "-", Codigo_Femaco, "-", Nombre_Producto),
    Fecha_carga = as.Date(Fecha_carga, format = "%d/%m/%Y")
  )

# Obtener fecha más reciente de la base
fecha_base_date <- base %>%
  pull(`Fecha_carga`) %>%
  max(na.rm = TRUE)

# Versión texto solo para mostrar en el UI
fecha_base <- as.Date(fecha_base_date) - 1

# Extraer año actual desde la Date
anio_actual <- lubridate::year(fecha_base_date)

# Extraer año anterior
anio_ante <- anio_actual - 1

meses_es <- c(
  "enero","febrero","marzo","abril","mayo","junio",
  "julio","agosto","septiembre","octubre","noviembre","diciembre"
)

# =========================
# Helpers generales
# =========================
obtener_opciones_producto <- function(df) {
  df %>% arrange(Producto) %>% pull(Producto) %>% unique()
}

# Normaliza una selección múltiple haciendo "Todos" exclusivo
exclusivo_todos <- function(sel, all_label = "Todos"){
  sel <- sel[!is.na(sel)]
  if (!length(sel)) return(all_label)
  if (all_label %in% sel && length(sel) > 1) return(all_label)
  sel
}

# =========================
# Token / Validación portal
# =========================
TOKEN_SECRET  <- Sys.getenv("TOKEN_SECRET", unset = "clave_ultra_segura_2024")
TOKEN_TTL_MIN <- as.numeric(Sys.getenv("TOKEN_TTL_MIN", unset = "480"))

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0) return(y)
  if (is.character(x) && length(x) == 1 && !nzchar(x)) return(y)
  x
}

.get_token_from_query <- function(session) {
  q <- shiny::isolate(session$clientData$url_search %||% "")
  if (!nzchar(q)) return("")
  q <- sub("^\\?", "", q)  # quita el "?"
  m <- regmatches(q, regexpr("token=([^&#]+)", q, perl = TRUE))
  if (!length(m) || m[1] == "") return("")
  sub("^.*token=", "", m[1])
}

.deny <- function(msg = "[DENY] Acceso denegado.") {
  message(msg)
  stop(msg, call. = FALSE)
}

validate_portal_token <- function(session, require_token = TRUE) {
  tok <- .get_token_from_query(session)
  
  if (!nzchar(tok)) {
    if (require_token) .deny("[DENY] Falta token.")
    return(invisible(NULL))
  }
  
  raw   <- utils::URLdecode(tok)
  parts <- strsplit(raw, "\\|")[[1]]
  if (length(parts) != 3) .deny("[DENY] Token mal formado.")
  
  user   <- parts[1]
  ts_str <- parts[2]
  firma  <- parts[3]
  
  firma_ok <- digest::hmac(
    key    = TOKEN_SECRET,
    object = paste(user, ts_str, sep = "|"),
    algo   = "sha256"
  )
  if (!identical(firma_ok, firma)) .deny("[DENY] Firma inválida.")
  
  ts <- suppressWarnings(as.POSIXct(ts_str, tz = Sys.timezone()))
  if (is.na(ts)) ts <- suppressWarnings(as.POSIXct(ts_str, tz = "UTC"))
  if (!is.na(ts)) {
    age_min <- as.numeric(difftime(Sys.time(), ts, units = "mins"))
    if (is.finite(age_min) && age_min > TOKEN_TTL_MIN) .deny("[DENY] Token expirado.")
  }
  
  session$userData$portal_user <- user
  invisible(TRUE)
}

# ==========================================================
# YoY mensual: año abierto (últimos 12 meses) / año cerrado (año completo)
# ==========================================================
mes_map <- c(
  enero=1, febrero=2, marzo=3, abril=4, mayo=5, junio=6,
  julio=7, agosto=8, septiembre=9, setiembre=9, octubre=10, noviembre=11, diciembre=12
)

tabla_yoy_mensual <- function(data, valor_col, tipo = c("abierto", "cerrado"), anio = NULL, fecha_ref = NULL) {
  tipo <- match.arg(tipo)
  
  df <- data %>%
    mutate(
      Mes_clean = tolower(stringi::stri_trans_general(Mes, "Latin-ASCII")),
      mes_num   = unname(mes_map[Mes_clean]),
      fecha_mes = as.Date(sprintf("%04d-%02d-01", Año, mes_num))
    )
  
  fin <- if (!is.null(fecha_ref)) {
    lubridate::floor_date(as.Date(fecha_ref), "month")
  } else {
    lubridate::floor_date(max(df$fecha_mes, na.rm = TRUE), "month")
  }
  
  meses_base <- if (tipo == "abierto") {
    inicio <- fin %m-% months(11)
    seq(inicio, fin, by = "1 month")
  } else {
    if (is.null(anio)) stop("Para tipo='cerrado' debes pasar anio=YYYY.")
    seq(
      as.Date(sprintf("%04d-01-01", anio)),
      as.Date(sprintf("%04d-12-01", anio)),
      by = "1 month"
    )
  }
  
  df %>%
    filter(fecha_mes %in% c(meses_base, meses_base %m-% years(1))) %>%
    mutate(
      periodo = if_else(fecha_mes %in% meses_base, "actual", "anterior"),
      fecha_base = if_else(periodo == "actual", fecha_mes, fecha_mes %m+% years(1))
    ) %>%
    group_by(fecha_base, periodo) %>%
    summarise(valor = sum(.data[[valor_col]], na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = periodo, values_from = valor, values_fill = 0) %>%
    mutate(
      anterior = if (!"anterior" %in% names(.)) 0 else anterior,
      variacion     = actual - anterior,
      variacion_pct = if_else(anterior == 0, NA_real_, (actual - anterior) / anterior),
      Mes_lbl       = lubridate::month(fecha_base, label = TRUE, abbr = TRUE),
      Año_actual    = lubridate::year(fecha_base),
      Año_anterior  = Año_actual - 1,
      MesEtiqueta   = paste0(Mes_lbl, "-", Año_actual)
    ) %>%
    arrange(fecha_base) %>%
    select(fecha_base, MesEtiqueta, actual, anterior, variacion, variacion_pct, Año_actual, Año_anterior)
}

# ==========================================================
# NUEVO: Tabla DT estilo "Serie" en filas y meses en columnas
# ==========================================================
meses_abbr_es <- c("Ene","Feb","Mar","Abr","May","Jun","Jul","Ago","Sep","Oct","Nov","Dic")

armar_tabla_dt_formato <- function(df_yoy, tipo_periodo, anio_ante) {
  df_cols <- df_yoy %>%
    mutate(
      mes_num = lubridate::month(fecha_base),
      MesCol  = meses_abbr_es[mes_num]
    ) %>%
    arrange(fecha_base)
  
  # En "cerrado", siempre será Ene..Dic; en "abierto", será el orden cronológico de los 12 meses
  cols_orden <- df_cols %>% pull(MesCol)
  
  if (tipo_periodo == "cerrado") {
    serie_prev <- as.character(anio_ante - 1)
    serie_act  <- as.character(anio_ante)
  } else {
    serie_prev <- "Año anterior"
    serie_act  <- "Año de referencia"
  }
  
  row_prev <- tibble::tibble(Serie = serie_prev) %>%
    bind_cols(as_tibble(setNames(as.list(df_cols$anterior), cols_orden)))
  
  row_act <- tibble::tibble(Serie = serie_act) %>%
    bind_cols(as_tibble(setNames(as.list(df_cols$actual), cols_orden)))
  
  row_var <- tibble::tibble(Serie = "Variación") %>%
    bind_cols(as_tibble(setNames(as.list(df_cols$variacion_pct), cols_orden)))
  
  out <- bind_rows(row_prev, row_act, row_var) %>%
    select(Serie, all_of(cols_orden))
  
  out
}

# =========================
# UI
# =========================
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$title("Femaco · Ventas Mensuales"),
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$style(HTML(
      ".custom-header{display:flex;align-items:center;margin-bottom:10px;}
       .custom-title{flex-grow:1;text-align:center;font-size:24px;font-weight:bold;}
       #main-panel{transition:all .3s ease;}
       #sidebar.hidden{display:none!important;}
       #main-panel.expanded{width:100%!important;}
       .hamburger-button{background:none;border:none;font-size:24px;margin-right:10px;cursor:pointer;}
       .nav-tabs{display:flex;justify-content:center;width:100%;}
       .nav-tabs>li{flex:1;text-align:center;}
       .nav-tabs>li>a{width:100%;border:1px solid #ccc!important;border-radius:4px 4px 0 0;margin-right:2px;background:#C1C2C4;font-weight:bold;color:#333;}
       .nav-tabs>li.active>a,.nav-tabs>li.active>a:focus,.nav-tabs>li.active>a:hover{background:#7ab943;color:#fff;font-weight:bold;border-color:#aaa #aaa transparent;}
       div.dataTables_wrapper .dataTables_length{float:left;margin-top:10px;}
       div.dataTables_wrapper .dt-buttons{float:right;margin-top:10px;}
       div.dataTables_wrapper .dataTables_filter{float:right;margin-top:10px;}
       @media screen and (max-width:768px){
         div.dataTables_wrapper .dataTables_length,
         div.dataTables_wrapper .dt-buttons,
         div.dataTables_wrapper .dataTables_filter{float:none;display:block;width:100%;text-align:left;margin-bottom:10px;}
         div.dataTables_wrapper .dataTables_filter{text-align:right;}
       }"
    )),
    tags$script(HTML("
      $(document).on('shiny:inputchanged', function(event) {
        if (event.name === 'toggle_sidebar') {
          setTimeout(function() {
            $('table.dataTable').DataTable().columns.adjust().draw();
          }, 400);
        }
      });
    "))
  ),
  div(class="custom-header",
      tags$img(src="logo.jpg", height="50px"),
      actionButton("toggle_sidebar", label = "\u2630", class="hamburger-button"),
      div(class="custom-title","Ventas Mensuales")
  ),
  tags$p(style="margin-left: 15px; font-style: italic;",
         paste("Datos actualizados al", format(fecha_base, "%d/%m/%Y"))),
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      tags$div(style="background-color:#7ab943; padding: 10px; border-radius: 5px; color: white;",
               tags$strong("Visualización de valores"),
               radioButtons("modo", NULL,
                            choices = c("Ventas","Unidades"),
                            selected = "Ventas", inline = TRUE)
      ),
      tags$h4("Insumo"),
      tags$div(style="margin-bottom: 10px; background-color:#7ab943; padding: 5px; border-radius: 5px; color: white;",
               tags$strong("¿Qué insumo deseas ocupar?"),
               radioButtons("modo_insumo", NULL,
                            choices = c("Sell Out" = "sellout", "Sell In" = "sellin"),
                            selected = "sellout", inline = TRUE)
      ),
      
      # ==========================================
      # Selector periodo: SOLO 2 opciones
      # ==========================================
      selectInput(
        "agno", "Seleccione periodo",
        choices  = c("Últimos 12 meses" = "abierto",
                     "Año anterior"     = "cerrado_ante"),
        selected = "abierto",
        width    = "100%"
      ),
      
      tags$h4("Jerarquía de Producto"),
      selectInput("categoria", "Categoría", choices = c("Todas"), width = "100%"),
      selectInput("subcategoria", "Subcategoría", choices = c("Todas"), width = "100%"),
      selectInput("conjunto", "Conjunto", choices = c("Todos"), width = "100%"),
      selectInput("estado", "Estado Producto", choices = c("Todos"), width = "100%"),
      tags$h4("Producto específico"),
      selectInput("producto_id", "Producto (SKU - Código - Nombre)",
                  choices = c("Todos"), multiple = TRUE, selectize = TRUE, width = "100%"),
      selectInput("formato", "Formato", choices = c("Todos"), width = "100%"),
      tags$h4("Ubicación"),
      selectInput("zona", "Zona", choices = c("Todas"), multiple = TRUE, selectize = TRUE, width = "100%"),
      selectInput("local", "Local (ID - Nombre)", choices = c("Todos"), multiple = TRUE, selectize = TRUE, width = "100%"),
      actionButton("reset_filtros", "Limpiar filtros", icon = icon("broom"),
                   style = "background-color: #7ab943; border-color: #7ab943; color: white;")
    ),
    mainPanel(
      id = "main-panel",
      div(
        plotlyOutput("grafico_total", height = "430px"),
        tags$hr(),
        DTOutput("tabla_total")
      )
    )
  )
)

# =========================
# SERVER
# =========================
server <- function(input, output, session) {
  
  # -------- Validar Ingreso --------
  validate_portal_token(session, require_token = !interactive())
  usr <- session$userData$portal_user
  
  # --- Toggle sidebar
  observeEvent(input$toggle_sidebar, {
    toggleClass("sidebar", "hidden")
    toggleClass("main-panel", "expanded")
  })
  
  # --- Insumo activo (por ahora base completa)
  dfx <- reactive({ base })
  
  # ==========================================
  # Periodo elegido (abierto / año anterior)
  # ==========================================
  tipo_periodo <- reactive({
    if (identical(input$agno, "abierto")) "abierto" else "cerrado"
  })
  
  # (compatibilidad con variables previas)
  agno_num <- reactive({
    if (tipo_periodo() == "abierto") anio_actual else anio_ante
  })
  
  agno_prev <- reactive({
    agno_num() - 1
  })
  
  # --- Base para poblar choices (solo acota los años necesarios)
  df_base_choices <- reactive({
    df <- dfx()
    
    if (tipo_periodo() == "abierto") {
      # rolling 12m + YoY: requiere hasta el año actual (y anteriores)
      df <- df %>% filter(Año <= anio_actual)
    } else {
      # año anterior completo + YoY: requiere año_ante y año_ante-1
      df <- df %>% filter(Año %in% c(anio_ante, anio_ante - 1))
    }
    
    df
  })
  
  # --- Filtro central aplicado a dfx()
  filtrar_datos <- function(df) {
    
    if (tipo_periodo() == "abierto") {
      df <- df %>% filter(Año <= anio_actual)
    } else {
      df <- df %>% filter(Año %in% c(anio_ante, anio_ante - 1))
    }
    
    if (input$subcategoria != "Todas") df <- df %>% filter(Subcategoria == input$subcategoria)
    if (input$conjunto != "Todos")     df <- df %>% filter(Conjunto == input$conjunto)
    if (input$categoria != "Todas")    df <- df %>% filter(Categoria == input$categoria)
    if (input$formato != "Todos")      df <- df %>% filter(Formato == input$formato)
    if (input$estado != "Todos")       df <- df %>% filter(Estado == input$estado)
    # if (input$zona != "Todas")         df <- df %>% filter(Zona == input$zona)
    
    if (!is.null(input$zona) && length(input$zona) > 0 && !"Todas" %in% input$zona) {
      df <- df %>%
        filter(Zona %in% as.character(input$zona))
    }
    
    if (!is.null(input$local) && length(input$local) > 0 && !"Todos" %in% input$local) {
      df <- df %>%
        mutate(Local_ID = as.character(Local_ID)) %>%
        filter(Local_ID %in% as.character(input$local))
    }
    
    if (!is.null(input$producto_id) && !"Todos" %in% input$producto_id) {
      df <- df %>% filter(Producto %in% input$producto_id)
    }
    
    df
  }
  
  data_filtrada <- reactive(filtrar_datos(dfx()))
  
  # ==========================================
  # Resumen YoY mensual (base para tabla y gráfico)
  # ==========================================
  valor_col_activa <- reactive({
    if (input$modo_insumo == "sellout") {
      if (input$modo == "Ventas") "Venta_Neta" else "Cantidad"
    } else {
      if (input$modo == "Ventas") "Monto_Neto" else "Unidades_Confirmadas"
    }
  })
  
  resumen_yoy <- reactive({
    df <- data_filtrada()
    req(nrow(df) > 0)
    
    tabla_yoy_mensual(
      data      = df,
      valor_col = valor_col_activa(),
      tipo      = tipo_periodo(),
      anio      = if (tipo_periodo() == "cerrado") anio_ante else NULL,
      fecha_ref = fecha_base_date
    )
  })
  
  # =========================
  # Tabla (DT) estilo "Serie" como tu imagen
  # =========================
  output$tabla_total <- renderDT({
    df_yoy <- resumen_yoy()
    if (nrow(df_yoy) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para mostrar"), options = list(dom = "t")))
    }
    
    dt_df <- armar_tabla_dt_formato(
      df_yoy       = df_yoy,
      tipo_periodo = tipo_periodo(),
      anio_ante    = anio_ante
    )
    
    # Columnas numéricas: todas excepto "Serie"
    num_idx <- 1:(ncol(dt_df) - 1)
    prefijo <- if (input$modo == "Ventas") "$" else ""
    
    render_js <- JS(sprintf(
      "function(data, type, row, meta){
     // data viene como número (o string numérico)
     if(data === null || data === undefined || data === '') return '';

     var v = Number(data);
     if(!isFinite(v)) return '';

     // Para exportación: devolver número sin formato
     if(type === 'export' || type === 'filter' || type === 'sort' || type === 'type'){
       return v;
     }

     // Para display: formatear
     if(type === 'display'){
       if(row[0] === 'Variación'){
         return (v*100).toFixed(1).replace('.', ',') + '%%';
       }
       var s = Math.round(v).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, '.');
       return '%s' + s;
     }

     return v;
   }", prefijo))
    
    datatable(
      dt_df,
      options = list(
        paging = FALSE, info = FALSE, scrollX = TRUE,
        dom = "Blrtip",
        buttons = list(
          "copy",
          "csv",
          list(
            extend = "excel",
            filename = "reporte_yoy_mensual",
            exportOptions = list(orthogonal = "export")
          )
        ),
        columnDefs = list(
          list(targets = num_idx, render = render_js, defaultContent = "", className = "dt-right"),
          list(targets = 0, className = "dt-left")
        ),
        fixedColumns = list(leftColumns = 1)
      ),
      extensions = c("Buttons", "FixedColumns"),
      rownames = FALSE
    )
  })
  
  # =========================
  # Gráfico (Plotly) usando el mismo resumen_yoy
  # =========================
  output$grafico_total <- renderPlotly({
    df <- resumen_yoy()
    req(nrow(df) > 0)
    
    df <- df %>% arrange(fecha_base)
    
    # X mostrado: Mes-Año (y mantiene orden cronológico)
    df <- df %>%
      mutate(
        MesX = paste0(meses_abbr_es[lubridate::month(fecha_base)], "-", lubridate::year(fecha_base)),
        MesX = factor(MesX, levels = MesX),
        
        # Periodo real para hover en cada barra
        Periodo_act  = MesX,
        Periodo_prev = paste0(
          meses_abbr_es[lubridate::month(fecha_base %m-% years(1))], "-",
          lubridate::year(fecha_base %m-% years(1))
        )
      )
    
    titulo <- if (tipo_periodo() == "abierto") "Últimos 12 meses" else "Año anterior"
    
    # Colores
    col_gris  <- "#C1C2C4"
    col_verde <- "#7AB943"
    col_linea <- "#000000"
    
    # Leyenda (nombres)
    if (tipo_periodo() == "cerrado") {
      name_prev <- as.character(anio_ante - 1)
      name_act  <- as.character(anio_ante)
    } else {
      name_prev <- "Mes año anterior"
      name_act  <- "Mes de referencia"
    }
    
    etiqueta_valor <- if (input$modo == "Ventas") "Monto" else "Unidades"
    
    # Barras
    p <- plot_ly() %>%
      add_bars(
        data = df, x = ~MesX, y = ~anterior,
        name = name_prev,
        marker = list(color = col_gris),
        customdata = ~Periodo_prev,
        hovertemplate = paste0(
          "Periodo: %{customdata}<br>",
          etiqueta_valor, ": %{y}<extra></extra>"
        )
      ) %>%
      add_bars(
        data = df, x = ~MesX, y = ~actual,
        name = name_act,
        marker = list(color = col_verde),
        customdata = ~Periodo_act,
        hovertemplate = paste0(
          "Periodo: %{customdata}<br>",
          etiqueta_valor, ": %{y}<extra></extra>"
        )
      ) %>%
      layout(
        title  = titulo,
        barmode = "group",
        xaxis  = list(title = "", type = "category"),
        yaxis  = list(title = "Ventas", separatethousands = TRUE, zeroline = FALSE),
        margin = list(t = 60, r = 140, b = 90, l = 70),
        legend = list(
          orientation = "v",
          x = 1.02, xanchor = "left",
          y = 1,    yanchor = "top"
        )
      )
    
    # Línea variación + markers
    df_line <- df %>% filter(is.finite(variacion_pct))
    if (nrow(df_line) > 0) {
      p <- p %>%
        add_lines(
          data = df_line, x = ~MesX, y = ~variacion_pct, yaxis = "y2",
          line = list(color = col_linea, width = 2),
          connectgaps = FALSE,
          hovertemplate = "Variación: %{y:.1%}<extra></extra>",
          showlegend = FALSE
        ) %>%
        add_markers(
          data = df_line, x = ~MesX, y = ~variacion_pct, yaxis = "y2",
          marker = list(color = col_linea, size = 6),
          hoverinfo = "skip",
          showlegend = FALSE
        )
      
      # Rango simétrico para eje derecho (como tu versión anterior)
      max_abs_var <- max(abs(df_line$variacion_pct), na.rm = TRUE)
      if (!is.finite(max_abs_var) || max_abs_var <= 0) max_abs_var <- 0.1
      
      p <- p %>% layout(
        yaxis2 = list(
          title = "Variación (%)",
          overlaying = "y",
          side = "right",
          tickformat = ".0%",
          range = c(-max_abs_var, max_abs_var),
          showgrid = FALSE,
          zeroline = FALSE
        )
      )
      
      # ======= Cajitas blancas con % (annotations) =======
      df_lab <- df_line %>%
        mutate(lbl = scales::percent(variacion_pct, accuracy = 0.1, decimal.mark = ",", big.mark = "."))
      
      if (nrow(df_lab) > 0) {
        for (i in seq_len(nrow(df_lab))) {
          p <- p %>% add_annotations(
            x = df_lab$MesX[i], y = df_lab$variacion_pct[i], yref = "y2",
            text = df_lab$lbl[i],
            showarrow = FALSE,
            bgcolor = "white",
            bordercolor = "rgba(0,0,0,0)",
            borderpad = 8,
            xanchor = "center",
            yanchor = "bottom",
            yshift = 8,
            font = list(size = 12, color = "black")
          )
        }
      }
    }
    
    p %>% config(locale = "de", displaylogo = FALSE)
  })
  
  
  
  
  # =========================
  # ACTUALIZACIÓN DE FILTROS (jerarquía + exclusividad)
  # =========================
  actualizar_select_conservar <- function(input_id, choices, session, input_actual) {
    seleccion_valida <- intersect(input_actual, choices)
    selected_final <- if (length(seleccion_valida) > 0) seleccion_valida else choices[1]
    updateSelectInput(session, input_id, choices = choices, selected = selected_final)
  }
  
  actualizar_productos <- function() {
    seleccion_previa <- isolate(input$producto_id)
    
    df <- df_base_choices()
    
    if (input$categoria != "Todas") df <- df %>% filter(Categoria == input$categoria)
    if (input$subcategoria != "Todas") df <- df %>% filter(Subcategoria == input$subcategoria)
    if (input$estado != "Todos") df <- df %>% filter(Estado == input$estado)
    if (input$conjunto != "Todos") df <- df %>% filter(Conjunto == input$conjunto)
    
    df <- df %>%
      mutate(
        SKU = as.character(SKU),
        Codigo_Femaco = as.character(Codigo_Femaco),
        Nombre_Producto = as.character(Nombre_Producto),
        Producto = paste0(SKU, "-", Codigo_Femaco, "-", Nombre_Producto)
      )
    
    opciones <- obtener_opciones_producto(df)
    seleccion_valida <- intersect(seleccion_previa, opciones)
    seleccion_final  <- exclusivo_todos(seleccion_valida, all_label = "Todos")
    
    updateSelectInput(
      session, "producto_id",
      choices  = c("Todos", opciones),
      selected = seleccion_final
    )
  }
  
  actualizar_locales <- function() {
    df <- df_base_choices()
    
    # Respeta zona si está seleccionada
    if (!is.null(input$zona) && !"Todas" %in% input$zona) {
      df <- df %>% filter(Zona %in% input$zona)
    }
    
    locs <- df %>%
      distinct(Local_ID, Local) %>%
      mutate(
        Local_ID = as.character(Local_ID),
        label = paste0(Local_ID, " - ", Local)
      ) %>%
      arrange(label)
    
    if (nrow(locs) == 0) {
      updateSelectInput(session, "local", choices = c("Todos" = "Todos"), selected = "Todos")
      return(invisible(NULL))
    }
    
    choices <- setNames(locs$Local_ID, locs$label)
    
    prev <- isolate(input$local)
    
    # Si venía "Todos" o venía vacío, mantenemos "Todos"
    if (is.null(prev) || length(prev) == 0 || "Todos" %in% prev) {
      selected <- "Todos"
    } else {
      # conservar solo los IDs que sigan siendo válidos
      prev_ok <- prev[prev %in% locs$Local_ID]
      selected <- if (length(prev_ok) > 0) prev_ok else "Todos"
    }
    
    updateSelectInput(
      session, "local",
      choices  = c("Todos" = "Todos", choices),
      selected = selected
    )
  }
  
  # 1) Poblar Local al iniciar y cada vez que cambie df_base_choices()
  observeEvent(df_base_choices(), {
    actualizar_locales()
  }, ignoreInit = FALSE)
  
  # 2) Poblar Producto al iniciar y cada vez que cambie df_base_choices()
  observeEvent(df_base_choices(), {
    actualizar_productos()
  }, ignoreInit = FALSE)
  
  observeEvent(input$producto_id, {
    df <- filtrar_datos(df_base_choices())
    updateSelectInput(session, "formato", choices = c("Todos", sort(unique(df$Formato))))
    if ("Todos" %in% input$producto_id && length(input$producto_id) > 1) {
      updateSelectInput(session, "producto_id", selected = "Todos")
    }
  })
  
  observeEvent(input$zona, {
    
    # --- Exclusión mutua "Todas" vs zonas específicas ---
    if ("Todas" %in% input$zona && length(input$zona) > 1) {
      if (tail(input$zona, 1) == "Todas") {
        # "Todas" fue seleccionada al final → limpiar el resto
        updateSelectInput(session, "zona", selected = "Todas")
      } else {
        # Se eligió una zona específica estando "Todas" → quitar "Todas"
        updateSelectInput(session, "zona", selected = setdiff(input$zona, "Todas"))
      }
      return()
    }
    
    # --- Actualizar locales al cambiar zona ---
    actualizar_locales()
    
  }, ignoreInit = TRUE)
  
  observeEvent(input$local, {
    if ("Todos" %in% input$local && length(input$local) > 1) {
      updateSelectInput(session, "local", selected = "Todos")
    }
    actualizar_productos()
  }, ignoreInit = TRUE)
    
  observe({
    updateSelectInput(session, "subcategoria", choices = c("Todas", sort(unique(df_base_choices()$Subcategoria))))
    updateSelectInput(session, "categoria", choices = c("Todas", sort(unique(df_base_choices()$Categoria))))
    updateSelectInput(session, "conjunto", choices = c("Todos", sort(unique(df_base_choices()$Conjunto))))
    updateSelectInput(session, "estado", choices = c("Todos", sort(unique(df_base_choices()$Estado))))
    updateSelectInput(session, "zona", choices = c("Todas", sort(unique(df_base_choices()$Zona))), selected = "Todas")
    updateSelectInput(session, "formato", choices = c("Todos", sort(unique(df_base_choices()$Formato))))
  })
  
  observeEvent(input$categoria, {
    df <- df_base_choices()
    if (input$categoria != "Todos") df <- df %>% filter(Categoria == input$categoria)
    opciones_subcategoria <- c("Todas", sort(unique(df$Subcategoria)))
    actualizar_select_conservar("subcategoria", opciones_subcategoria, session, isolate(input$subcategoria))
    actualizar_productos()
  })
  
  observeEvent(input$subcategoria, {
    df <- df_base_choices()
    if (input$subcategoria != "Todos") df <- df %>% filter(Subcategoria == input$subcategoria)
    actualizar_productos()
  })
  
  observeEvent(
    list(input$subcategoria, input$categoria, input$conjunto, input$estado, input$local),
    {
      actualizar_productos()
    },
    ignoreInit = TRUE
  )
  
  observeEvent(input$reset_filtros, {
    updateSelectInput(session, "subcategoria", selected = "Todas")
    updateSelectInput(session, "categoria", selected = "Todas")
    updateSelectInput(session, "conjunto", selected = "Todos")
    updateSelectInput(session, "estado", selected = "Todos")
    updateSelectInput(session, "producto_id", selected = "Todos")
    updateSelectInput(session, "formato", selected = "Todos")
    updateSelectInput(session, "zona", selected = "Todas")
    updateSelectInput(session, "local", selected = "Todos")
    updateRadioButtons(session, "modo", selected = "Ventas")
  })
}

shinyApp(ui, server)
