
##########################################################
######################## MONITOREO #######################
##########################################################

# =========================
# Librerías
# =========================
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(plotly)
library(dplyr)
library(tidyr)
library(lubridate)

# =========================
# Datos
# =========================
base <- readRDS("monitoreo.rds") %>% 
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

meses_nombres <- c(
  "enero" = 1, "febrero" = 2, "marzo" = 3,
  "abril" = 4, "mayo" = 5, "junio" = 6,
  "julio" = 7, "agosto" = 8, "septiembre" = 9,
  "octubre" = 10, "noviembre" = 11, "diciembre" = 12
)

# Helpers
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

###### Validar clave de ingreso #######

# Debe coincidir con el portal
TOKEN_SECRET  <- Sys.getenv("TOKEN_SECRET", unset = "clave_ultra_segura_2024")
# Opcional: vigencia del token en minutos (por defecto 8 h)
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
  
  # Verifica firma HMAC (debe usar el mismo TOKEN_SECRET del portal)
  firma_ok <- digest::hmac(key = TOKEN_SECRET,
                           object = paste(user, ts_str, sep = "|"),
                           algo = "sha256")
  if (!identical(firma_ok, firma)) .deny("[DENY] Firma inválida.")
  
  # Verifica vigencia (tolerante a zona horaria)
  ts <- suppressWarnings(as.POSIXct(ts_str, tz = Sys.timezone()))
  if (is.na(ts)) ts <- suppressWarnings(as.POSIXct(ts_str, tz = "UTC"))
  if (!is.na(ts)) {
    age_min <- as.numeric(difftime(Sys.time(), ts, units = "mins"))
    if (is.finite(age_min) && age_min > TOKEN_TTL_MIN) .deny("[DENY] Token expirado.")
  }
  
  # Deja el usuario disponible para el módulo
  session$userData$portal_user <- user
  invisible(TRUE)
}

##########################################

# =========================
# UI
# =========================
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$title("Femaco · Monitoreo"),
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
      div(class="custom-title","Monitoreo")
  ),
  tags$p(style="margin-left: 15px; font-style: italic;",
        paste("Datos actualizados al", format(fecha_base, "%d/%m/%Y"))),
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      # tags$div(style="background-color:#7ab943; padding: 10px; border-radius: 5px; color: white;",
      #          tags$strong("Visualización de valores"),
      #          radioButtons("modo", NULL,
      #                       choices = c("Ventas","Unidades"),
      #                       selected = "Ventas", inline = TRUE)
      # ),
      # conditionalPanel(
      #   condition = "input.modo_insumo == 'sellin'",
      #   tags$h4("Tipo de OC a incluir"),
      #   tags$div(style="margin-bottom: 10px; background-color:#7ab943; padding: 5px; border-radius: 5px; color: white;",
      #            tags$strong("¿Qué tipo de OC deseas usar?"),
      #            radioButtons("modo_oc", NULL,
      #                         choices = c("Usar solo OC cerradas" = "oc_cerradas",
      #                                     "Usar OC cerradas y abiertas" = "oc_cerradas_abiertas"),
      #                         selected = "oc_cerradas", inline = TRUE)
      #   )
      # ),
      # selectInput("agno", "Seleccione Año", choices = sort(c(anio_ante, anio_actual)),
      #             selected = anio_actual, width = "100%"),
      # tags$h4("Métrica a mostrar"),
      tags$div(style = "background-color:#7ab943; padding: 5px; border-radius: 5px; color: white;",
               tags$strong("Visualización de valores"),
               radioButtons("metrica", NULL,
                            choices = c(
                              "Ventas" = "Ventas",
                              "Unidades" = "Unidades"
                            ),
                            selected = "Ventas", inline = TRUE)
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
      selectInput("zona", "Zona", choices = c("Todas"), width = "100%"),
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
  
  ############# Validar Ingreso ################
  # En local (RStudio interactivo) NO exige token; en producción SÍ.
  validate_portal_token(session, require_token = !interactive())
  usr <- session$userData$portal_user
  ###############################################
  
  # --- Toggle sidebar
  observeEvent(input$toggle_sidebar, {
    toggleClass("sidebar", "hidden")
    toggleClass("main-panel", "expanded")
  })
  
  # --- Insumo activo
  dfx <- reactive({
    base
  })

  # --- Base para poblar choices (aplica Año y OC si corresponde)
  df_base_choices <- reactive({
    df <- dfx()
    # if (input$modo_insumo == "sellin") {
    #   df <- if (identical(input$modo_oc, "oc_cerradas")) {
    #     df %>% filter(`Estado OC` %in% "Recepción total")
    #   } else {
    #     df %>% filter(`Estado OC` %in% c("Recepción total","Cursado"))
    #   }
    # }
    df
  })
  
   # --- Filtro central aplicado a dfx()
  filtrar_datos <- function(df) {
    df <- df
    # if (input$modo_insumo == "sellin") {
    #   df <- if (identical(input$modo_oc, "oc_cerradas")) {
    #     df %>% filter(`Estado OC` %in% "Recepción total")
    #   } else {
    #     df %>% filter(`Estado OC` %in% c("Recepción total","Cursado"))
    #   }
    # }
    if (input$subcategoria != "Todas") df <- df %>% filter(Subcategoria == input$subcategoria)
    if (input$conjunto != "Todos")     df <- df %>% filter(Conjunto == input$conjunto)
    if (input$categoria != "Todas")    df <- df %>% filter(Categoria == input$categoria)
    if (input$formato != "Todos")      df <- df %>% filter(Formato == input$formato)
    if (input$estado != "Todos")       df <- df %>% filter(Estado == input$estado)
    if (input$zona != "Todas")         df <- df %>% filter(Zona == input$zona)
    if (!is.null(input$local) && length(input$local) > 0 && !"Todos" %in% input$local) {
      ids_locales <- sapply(strsplit(input$local, "-"), `[`, 1)
      df <- df %>% filter(Local_ID %in% ids_locales)
    }
    if (!is.null(input$producto_id) && !"Todos" %in% input$producto_id) {
      df <- df %>% filter(Producto %in% input$producto_id)
    }
    df
  }
  
  data_filtrada <- reactive(filtrar_datos(dfx()))
  
  crear_datos <- function(df) {
    df <- df
    # Crear fecha y filtrar últimos 12 meses
    monitoreo_12m <- df %>%
      mutate(
        Mes_num = meses_nombres[Mes],
        Fecha = as.Date(paste(Año, Mes_num, "01", sep = "-"))
      ) %>%
      filter(
        Fecha >= max(Fecha, na.rm = TRUE) %m-% months(11)
      )
    
    # Agregar por mes
    monitoreo_mes <- monitoreo_12m %>%
      group_by(Año,Mes) %>%
      summarise(
        Valor_Stock    = sum(Valor_Stock, na.rm = TRUE),
        ventas_max_mes = sum(ventas_max_mes, na.rm = TRUE),
        Venta_Neta     = sum(Venta_Neta, na.rm = TRUE),
        Monto_Neto     = sum(Monto_Neto, na.rm = TRUE),
        Stock_max_mes        = sum(Stock_max_mes, na.rm = TRUE),
        cantidades_max_mes   = sum(cantidades_max_mes, na.rm = TRUE),
        Cantidad             = sum(Cantidad, na.rm = TRUE),
        Unidades_Confirmadas = sum(Unidades_Confirmadas, na.rm = TRUE),
        .groups = "drop") %>% 
      mutate(Mes = factor(substr(Mes, 1, 3),
                          levels = c("ene","feb","mar","abr","may","jun","jul","ago","sep","oct","nov","dic"))) %>% 
      mutate(Mes_key = paste0(Año, "-", as.character(Mes))) %>%  # <- NUEVO
      arrange(Año, Mes)                                          # <- CAMBIO
    
    monitoreo_mes
  }
  
  
  # =========================
  # Agregación / Tabla / Gráfico
  # =========================
  
  tabla <- function(resumen) {
    
    orden_filas <- c(
      "Sellout máximo histórico",
      "Sellout mes",
      "Sellin mes",
      "Stock máximo mes"
    )
    
    resumen %>%
      select(Mes_key, Valor_Stock, ventas_max_mes, Venta_Neta, Monto_Neto,
             Stock_max_mes, cantidades_max_mes, Cantidad, Unidades_Confirmadas) %>%
      pivot_longer(
        cols = c(Valor_Stock, ventas_max_mes, Venta_Neta, Monto_Neto,
                 Stock_max_mes, cantidades_max_mes, Cantidad, Unidades_Confirmadas),
        names_to = "Variable",
        values_to = "Valor") %>%
      mutate(Unidad = case_when(Variable %in% c("Valor_Stock","ventas_max_mes","Venta_Neta","Monto_Neto") ~ "Ventas",
                                TRUE ~ "Unidades")) %>%
      filter(Unidad == input$metrica) %>%
      dplyr::select(-Unidad) %>% 
      pivot_wider(
        names_from = Mes_key,
        values_from = Valor) %>%
      arrange(Variable) %>%
      mutate(Variable = recode(
        Variable,
        ventas_max_mes = "Sellout máximo histórico",
        Venta_Neta     = "Sellout mes",
        Monto_Neto     = "Sellin mes",
        Valor_Stock  = "Stock máximo mes",
        cantidades_max_mes   = "Sellout máximo histórico",
        Cantidad             = "Sellout mes",
        Unidades_Confirmadas = "Sellin mes",
        Stock_max_mes        = "Stock máximo mes")) %>% 
      arrange(match(Variable, orden_filas))
  }
  
  grafico <- function(resumen) {
    
    resumen <- resumen %>%
      arrange(Año, Mes)
    
    fmt_pesos <- function(x) 
      paste0("$ ", formatC(x, format = "f", big.mark = ".", decimal.mark = ",", digits = 0))
    
    fmt_unid <- function(x) 
      formatC(x, format = "f", big.mark = ".", decimal.mark = ",", digits = 0)
    
    es_plata <- identical(input$metrica, "Ventas")
    fmt <- if (es_plata) fmt_pesos else fmt_unid
    
    # opcional: asegura orden de meses si vienen como texto
    # resumen <- resumen %>% mutate(Mes = factor(Mes, levels = c("enero","febrero",...)))
    
    orden_x <- resumen$Mes_key
    p <- plot_ly(resumen, x = ~Mes_key)
    
    if (es_plata) {
      
      p <- p %>%
        add_trace(
          y = ~ventas_max_mes,
          name = "Sellout máximo histórico",
          type = "scatter", mode = "lines+markers",
          text = ~fmt(ventas_max_mes), hoverinfo = "text"
        ) %>%
        add_trace(
          y = ~Venta_Neta,
          name = "Sellout mes",
          type = "scatter", mode = "lines+markers",
          text = ~fmt(Venta_Neta), hoverinfo = "text"
        ) %>%
        add_trace(
          y = ~Monto_Neto,
          name = "Sellin mes",
          type = "scatter", mode = "lines+markers",
          text = ~fmt(Monto_Neto), hoverinfo = "text"
        ) %>%
        add_trace(
          y = ~Valor_Stock,
          name = "Stock máximo mes",
          type = "scatter", mode = "lines+markers",
          text = ~fmt(Valor_Stock), hoverinfo = "text"
        )
      
    } else {
      
      p <- p %>%
        add_trace(
          y = ~cantidades_max_mes,
          name = "Sellout máximo histórico",
          type = "scatter", mode = "lines+markers",
          text = ~fmt(cantidades_max_mes), hoverinfo = "text"
        ) %>%
        add_trace(
          y = ~Cantidad,
          name = "Sellout mes",
          type = "scatter", mode = "lines+markers",
          text = ~fmt(Cantidad), hoverinfo = "text"
        ) %>%
        add_trace(
          y = ~Unidades_Confirmadas,
          name = "Sellin mes",
          type = "scatter", mode = "lines+markers",
          text = ~fmt(Unidades_Confirmadas), hoverinfo = "text"
        ) %>%
        add_trace(
          y = ~Stock_max_mes,
          name = "Stock máximo mes",
          type = "scatter", mode = "lines+markers",
          text = ~fmt(Stock_max_mes), hoverinfo = "text"
        )
      
    }
    
    p %>%
      layout(
        legend = list(orientation = "h", x = 0, y = 1.15),
        xaxis = list(title = "", categoryorder = "array", categoryarray = orden_x),
        yaxis  = list(title = "", showticklabels = FALSE),
        yaxis2 = list(title = "", overlaying = "y", side = "right", showticklabels = FALSE)
      )
  }
  
  
  # ---------- Tabla ----------
  output$tabla_total <- renderDT({
    df <- data_filtrada()
    if (nrow(df) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para mostrar"), options = list(dom = 't')))
    } else {
      resumen <- tabla(crear_datos(df))
      
      resumen_tab <- resumen
      
      if (nrow(resumen_tab) == 0) {
        datatable(
          data.frame(Mensaje = "No hay datos disponibles"),
          options = list(dom = "t"),
          rownames = FALSE
        )
      } else {
        
        # columnas numéricas: desde la 2da (meses) hasta la última
        num_idx <- 2:ncol(resumen_tab)
        
        render_js <- JS("
          function(data, type, row, meta){
            if(type !== 'display') return data;
          
            if(data === null || data === undefined || data === '') return '';
            var v = Number(data);
            if(!isFinite(v)) return '';
          
            // Nombre de la fila (primera columna)
            var varName = row[0] || '';
          
            // Caso especial
            if(varName === 'Variación'){
              return (v*100).toFixed(1).replace('.', ',') + '%';
            }
          
            // Lee el radiobutton de Shiny
            var metrica = (window.Shiny && Shiny.shinyapp)
              ? Shiny.shinyapp.$inputValues['metrica']
              : null;
          
            var esPlata = (metrica === 'Ventas');
          
            // miles con punto
            var s = Math.round(v).toString().replace(/\\B(?=(\\d{3})+(?!\\d))/g, '.');
          
            return (esPlata ? '$' :'') + s;
          }
          ")
        
        
        datatable(
          resumen_tab,
          options = list(
            paging = FALSE, info = FALSE, scrollX = TRUE,
            dom = "Blrtip",
            buttons = list(
              "copy",
              "csv",
              list(
                extend = "excel",
                filename = "reporte_monitoreo",
                exportOptions = list(orthogonal = "export", stripHtml = TRUE)
              )
            ),
            columnDefs = list(
              list(
                targets = num_idx - 1,   # DT = base 0
                render = render_js,
                defaultContent = ""
              )
            )
          ),
          extensions = "Buttons",
          rownames = FALSE
        )
      }
      

    }
  })
  
  # ---------- Gráfico ----------
  output$grafico_total <- renderPlotly({
    df <- data_filtrada(); req(nrow(df) > 0)
    resumen <- crear_datos(df)
    grafico(resumen)
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
    
    updateSelectInput(session, "producto_id",
                      choices = c("Todos", opciones),
                      selected = if (length(seleccion_valida) > 0) seleccion_valida else "Todos")
  }
  
  observeEvent(input$producto_id, {
    df <- filtrar_datos(df_base_choices())
    updateSelectInput(session, "formato", choices = c("Todos", sort(unique(df$Formato))))
    if ("Todos" %in% input$producto_id && length(input$producto_id) > 1) {
      updateSelectInput(session, "producto_id", selected = "Todos")
    }
  })
  
  observeEvent(input$local, {
    if ("Todos" %in% input$local && length(input$local) > 1) {
      updateSelectInput(session, "local", selected = "Todos")
    }
    
    seleccion_previa <- isolate(input$producto_id)
    df <- df_base_choices()
    if (input$subcategoria != "Todas") df <- df %>% filter(Subcategoria == input$subcategoria)
    if (input$categoria != "Todas") df <- df %>% filter(Categoria == input$categoria)
    if (input$conjunto != "Todos") df <- df %>% filter(Conjunto == input$conjunto)
    if (input$estado != "Todos") df <- df %>% filter(Estado == input$estado)
    opciones <- obtener_opciones_producto(df)
    seleccion_valida <- intersect(seleccion_previa, opciones)
    updateSelectInput(session, "producto_id",
                      choices = c("Todos", opciones),
                      selected = if (length(seleccion_valida) > 0) seleccion_valida else "Todos")
  })
  
  observe({
    updateSelectInput(session, "subcategoria", choices = c("Todas", sort(unique(df_base_choices()$Subcategoria))))
    updateSelectInput(session, "categoria", choices = c("Todas", sort(unique(df_base_choices()$Categoria))))
    updateSelectInput(session, "conjunto", choices = c("Todos", sort(unique(df_base_choices()$Conjunto))))
    updateSelectInput(session, "estado", choices = c("Todos", sort(unique(df_base_choices()$Estado))))
    updateSelectInput(session, "zona", choices = c("Todas", sort(unique(df_base_choices()$Zona))))
    updateSelectInput(session, "local", choices = c("Todos", sort(unique(df_base_choices()$Local))))
    updateSelectInput(session, "producto_id", choices = c("Todos", obtener_opciones_producto(df_base_choices())))
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
  
  observeEvent(input$zona, {
    df <- df_base_choices()
    if (input$zona != "Todas") df <- df %>% filter(Zona == input$zona)
    locales <- df %>% mutate(OpcionLocal = paste0(Local_ID, "-", Local)) %>%
      arrange(OpcionLocal) %>%
      pull(OpcionLocal) %>% unique()
    actualizar_select_conservar("local", c("Todos", locales), session, isolate(input$local))
  })
  
  observeEvent(
    c(input$subcategoria, input$categoria, input$conjunto, input$estado, input$local),
    {
      seleccion_previa <- isolate(input$producto_id)
      df <- df_base_choices()
      if (input$subcategoria != "Todas") df <- df %>% filter(Subcategoria == input$subcategoria)
      if (input$categoria != "Todas") df <- df %>% filter(Categoria == input$categoria)
      if (input$conjunto != "Todos") df <- df %>% filter(Conjunto == input$conjunto)
      if (input$estado != "Todos") df <- df %>% filter(Estado == input$estado)
      opciones <- obtener_opciones_producto(df)
      seleccion_valida <- intersect(seleccion_previa, opciones)
      updateSelectInput(session, "producto_id",
                        choices = c("Todos", opciones),
                        selected = if (length(seleccion_valida) > 0) seleccion_valida else "Todos")
    })
  
  
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
