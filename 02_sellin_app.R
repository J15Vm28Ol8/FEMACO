
##########################################################
##################### SELL IN ############################
##########################################################

# Carga de librerías
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
library(digest)   

# Carga de datos
base_sellin <- readRDS("base_historica_sellin.rds") %>%
  mutate(
    SKU = as.character(SKU),
    Codigo_Femaco = as.character(Codigo_Femaco),
    Nombre_Producto = as.character(Nombre_Producto),
    Producto = paste0(SKU, "-", Codigo_Femaco, "-", Nombre_Producto)
  )

# Obtener fecha más reciente de la base

fecha_base_date <- base_sellin %>%
  pull(`Fecha_carga`) %>%
  max(na.rm = TRUE)

# Versión texto solo para mostrar en el UI
fecha_base <- as.Date(fecha_base_date) - 1

# Extraer año actual desde la Date
anio_actual <- lubridate::year(fecha_base_date)

# Extraer año anterior
anio_ante <- anio_actual - 1

# Nombres de columnas en la tabla
col_actual  <- as.character(anio_actual)
col_ante    <- as.character(anio_ante)

# Nombre de la columna de variación
nombre_variacion <- paste0("Variación ", col_ante, " - ", col_actual)

# Función auxiliar para generar opciones de producto
obtener_opciones_producto <- function(df) {
  df %>%
    arrange(Producto) %>%
    pull(Producto) %>%
    unique()
}

meses_es <- c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio",
              "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")


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

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$title("Femaco · Sell In"),
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$style(HTML(
      ".custom-header {
        display: flex;
        align-items: center;
        margin-bottom: 10px;
      }
      .custom-title {
        flex-grow: 1;
        text-align: center;
        font-size: 24px;
        font-weight: bold;
      }
      #main-panel {
        transition: all 0.3s ease;
      }
      #sidebar.hidden {
        display: none !important;
      }
      #main-panel.expanded {
        width: 100% !important;
      }
      .hamburger-button {
        background: none;
        border: none;
        font-size: 24px;
        margin-right: 10px;
        cursor: pointer;
      }
    .nav-tabs {
      display: flex;
      justify-content: center;
      width: 100%;
    }
    .nav-tabs > li {
      flex: 1;
      text-align: center;
    }
    .nav-tabs > li > a {
      width: 100%;
      border: 1px solid #ccc !important;
      border-radius: 4px 4px 0 0;
      margin-right: 2px;
      background-color: #C1C2C4;
      font-weight: bold;
      color: #333;
    }
    .nav-tabs > li.active > a,
    .nav-tabs > li.active > a:focus,
    .nav-tabs > li.active > a:hover {
      background-color: #7ab943;
      color: white;
      font-weight: bold;
      border-color: #aaa #aaa transparent;
    }
    
    
    /* Distribuir lengthMenu a la izquierda y botones a la derecha */
    div.dataTables_wrapper .dataTables_length {
      float: left;
      margin-top: 10px;
    }
    div.dataTables_wrapper .dt-buttons {
      float: right;
      margin-top: 10px;
    }
    div.dataTables_wrapper .dataTables_filter {
      float: right;
      margin-top: 10px;
    }
    
    /* Para pantallas pequeñas, hacer que se apilen */
    @media screen and (max-width: 768px) {
      div.dataTables_wrapper .dataTables_length,
      div.dataTables_wrapper .dt-buttons,
      div.dataTables_wrapper .dataTables_filter {
        float: none;
        display: block;
        width: 100%;
        text-align: left;
        margin-bottom: 10px;
      }

      div.dataTables_wrapper .dataTables_filter {
      text-align: right;
      }
    
      }

    "
      
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
  div(class = "custom-header",
      tags$img(src = "logo.jpg", height = "50px"),
      actionButton("toggle_sidebar", label = "\u2630", class = "hamburger-button"),
      div(class = "custom-title", "Sell In por Producto y Local")
  ),
  tags$p(style = "margin-left: 15px; font-style: italic;",
         paste("Datos actualizados al", format(fecha_base, "%d/%m/%Y"))),
  sidebarLayout(
    sidebarPanel(
      id = "sidebar",
      tags$div(style = "background-color:#7ab943; padding: 10px; border-radius: 5px; color: white;",
               tags$strong("Visualización de valores"),
               radioButtons("modo", NULL,
                            choices = c("Ventas", "Unidades"),
                            selected = "Ventas", inline = TRUE)
      ),
      tags$h4("Tipo de OC a incluir"),
      tags$div(style = "background-color:#7ab943; padding: 5px; border-radius: 5px; color: white;",
               tags$strong("¿Qué tipo de OC deseas usar?"),
               radioButtons("modo_oc", NULL,
                            choices = c(
                              "Usar solo OC cerradas" = "oc_cerradas",
                              "Usar OC cerradas y abiertas" = "oc_cerradas_abiertas"
                            ),
                            selected = "oc_cerradas", inline = TRUE)
      ),
      tags$h4("Intervalo de Tiempo"),
      tags$div(style = "background-color:#7ab943; padding: 5px; border-radius: 5px; color: white;",
               tags$strong("¿Cómo deseas filtrar las fechas?"),
               radioButtons("modo_fecha", NULL,
                            choices = c(
                              "Comparar entre años" = "comparar_anios",
                              "Rango absoluto de fechas" = "rango_absoluto"
                            ),
                            selected = "comparar_anios", inline = TRUE)
      ),
      conditionalPanel(
        condition = "input.modo_fecha == 'comparar_anios'",
        fluidRow(
          column(12,
                 tags$h5("Selecciona un rango de días/meses para comparar entre años:",
                         style = "margin-bottom: 10px; font-weight: bold;"),
                 tags$div(style = "display: flex; align-items: center; gap: 10px;",
                          selectInput("dia_inicio", NULL, choices = 1:31, selected = 1, width = "80px"),
                          selectInput("mes_inicio", NULL, choices = meses_es, selected = "Enero", width = "130px"),
                          tags$span("a", style = "margin: 0 10px; font-weight: bold;"),
                          selectInput("dia_fin", NULL, choices = 1:31, selected = 31, width = "80px"),
                          selectInput("mes_fin", NULL, choices = meses_es, selected = "Enero", width = "130px")
                 )
          )
        )
      ),
      conditionalPanel(
        condition = "input.modo_fecha == 'rango_absoluto'",
        div(style = "margin-top: 10px; margin-bottom: 10px;",
            dateRangeInput(
              inputId = "rango_fechas",
              label = "Selecciona el rango de fechas:",
              start = as.Date(min(base_sellin$Fecha_cierre_estimada, na.rm = TRUE)),
              end = as.Date(max(base_sellin$Fecha_cierre_estimada, na.rm = TRUE)),
              min = as.Date(min(base_sellin$Fecha_cierre_estimada, na.rm = TRUE)),
              max = as.Date(max(base_sellin$Fecha_cierre_estimada, na.rm = TRUE)),
              format = "dd-MM-yyyy",
              language = "es", separator = "a"
            )
          )
      ),
      tags$h4("Jerarquía de Producto"),
      #selectInput("grupo", "Grupo", choices = c("Todos"), width = "100%"),
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
      tabsetPanel(id = "nivel",
                  
                  tabPanel("Producto", value = "SKU",
                           fluidRow(
                             column(12,
                                    div(style = "margin-top: 5px;", DTOutput("tabla_sku"))
                             )
                           )
                  ),
                  
                  tabPanel("Local", value = "Local",
                           fluidRow(
                             column(12, DTOutput("tabla_local"))
                           )
                  ),
                  tabPanel("Categoría", value = "Categoria",
                           fluidRow(
                             column(12,
                                    div(style = "margin-top: 5px;", DTOutput("tabla_categoria"))
                             )
                           )
                  ),
                  tabPanel("Subcategoría", value = "Subcategoria",
                           fluidRow(
                             column(12,
                                    div(style = "margin-top: 5px;", DTOutput("tabla_subcategoria"))
                             )
                           )
                  ),
                  tabPanel("Total", value = "Total",
                           fluidRow(
                             column(12,
                                    div(style = "margin-top: 5px;", DTOutput("tabla_total"))
                             )
                           )
                  )
      )
    )
  )
)


# SERVER
server <- function(input, output, session) {
  
  ############# Validar Ingreso ################
  # En local (RStudio interactivo) NO exige token; en producción SÍ.
  validate_portal_token(session, require_token = !interactive())
  usr <- session$userData$portal_user
  ###############################################
  
  observeEvent(input$toggle_sidebar, {
    toggleClass("sidebar", "hidden")
    toggleClass("main-panel", "expanded")
  })
  
  # Actualiza días válidos para el mes de inicio
  observeEvent(list(input$mes_inicio, input$anio), {
    req(input$mes_inicio, input$anio)
    
    anio_sel  <- as.numeric(input$anio)
    mes_sel   <- match(input$mes_inicio, meses_es)  # "Enero" -> 1, etc.
    
    dias_validos <- days_in_month(
      ymd(sprintf("%04d-%02d-01", anio_sel, mes_sel))
    )
    
    updateSelectInput(
      session, "dia_inicio",
      choices  = 1:dias_validos,
      selected = min(as.numeric(input$dia_inicio), dias_validos)
    )
  })
  
  # Actualiza días válidos para el mes de fin
  observeEvent(list(input$mes_fin, input$anio), {
    req(input$mes_fin, input$anio)
    
    anio_sel  <- as.numeric(input$anio)
    mes_sel   <- match(input$mes_fin, meses_es)
    
    dias_validos <- days_in_month(
      ymd(sprintf("%04d-%02d-01", anio_sel, mes_sel))
    )
    
    updateSelectInput(
      session, "dia_fin",
      choices  = 1:dias_validos,
      selected = min(as.numeric(input$dia_fin), dias_validos)
    )
  })
  
  actualizar_select_conservar <- function(input_id, choices, session, input_actual) {
    seleccion_valida <- intersect(input_actual, choices)
    selected_final <- if (length(seleccion_valida) > 0) seleccion_valida else choices[1]
    updateSelectInput(session, input_id, choices = choices, selected = selected_final)
  }
  
  actualizar_productos <- function() {
    seleccion_previa <- isolate(input$producto_id)
    
    # Usar base_sellin filtrado por jerarquías, pero sin aplicar el filtro por producto_id
    df <- base_sellin
    #if (input$grupo != "Todos") df <- df %>% filter(Grupo == input$grupo)
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
  
  
  filtrar_datos <- function(df) {
    if (input$modo_fecha == "rango_absoluto") {
      df <- df %>%
        filter(Fecha_cierre_estimada >= input$rango_fechas[1],
               Fecha_cierre_estimada <= input$rango_fechas[2])
      
    } else if (input$modo_fecha == "comparar_anios") {
      inicio <- sprintf("%02d-%02d", match(input$mes_inicio, meses_es), as.numeric(input$dia_inicio))
      fin    <- sprintf("%02d-%02d", match(input$mes_fin, meses_es), as.numeric(input$dia_fin))
      
      df <- df %>%
        filter(format(Fecha_cierre_estimada, "%m-%d") >= inicio,
               format(Fecha_cierre_estimada, "%m-%d") <= fin)
    }
    if (input$modo_oc == "oc_cerradas") {
      df <- df %>%
        filter(`Estado OC` %in% c("Recepción total"))
    } else if (input$modo_oc == "oc_cerradas_abiertas") {
      df <- df %>%
        filter(`Estado OC` %in% c("Recepción total", "Cursado", "Recepción parcial"))
    }
    if (input$subcategoria != "Todas") df <- df %>% filter(Subcategoria == input$subcategoria)
    #if (input$grupo != "Todos") df <- df %>% filter(Grupo == input$grupo)
    if (input$conjunto != "Todos") df <- df %>% filter(Conjunto == input$conjunto)
    if (input$categoria != "Todas") df <- df %>% filter(Categoria == input$categoria)
    if (input$formato != "Todos") df <- df %>% filter(Formato == input$formato)
    if (input$estado != "Todos") df <- df %>% filter(Estado == input$estado)
    # if (input$zona != "Todas") df <- df %>% filter(Zona == input$zona)
    if (!is.null(input$zona) && length(input$zona) > 0 && !"Todas" %in% input$zona) {
      df <- df %>% filter(Zona %in% input$zona)
    }
    if (!is.null(input$local) && length(input$local) > 0 && !"Todos" %in% input$local) {
      ids_locales <- sapply(strsplit(input$local, "-"), function(x) x[1])
      df <- df %>% filter(Local_ID %in% ids_locales)
    }
    
    if (!is.null(input$producto_id) && !"Todos" %in% input$producto_id) {
      df <- df %>%
        #mutate(Producto = paste0(SKU, "-", Codigo_Femaco, "-", Nombre_Producto)) %>%
        filter(Producto %in% input$producto_id)
    }
    
    df
  }
  

  # Funciones ---------------------------------------------------------------
  
  agregar_fila_total <- function(df, nivel) {
    columnas_sumar <- names(df)[sapply(df, is.numeric)]
    
    # Calcular sumas por columnas numéricas
    totales <- colSums(df[, columnas_sumar, drop = FALSE], na.rm = TRUE)
    
    # Crear fila vacía con NA
    fila_total <- df[1, , drop = FALSE]
    fila_total[,] <- NA
    fila_total[[nivel]] <- "Total"
    fila_total[, columnas_sumar] <- as.list(totales)
    
    # Calcular variación global si ambas columnas están presentes
    # Verificar que ambas columnas existan
    if (all(c(col_ante, col_actual) %in% columnas_sumar)) {
      
      variacion_total <- if (totales[col_ante] == 0) {
        NA_real_
      } else {
        (totales[col_actual] - totales[col_ante]) / totales[col_ante]
      }
      
      fila_total[[nombre_variacion]] <- variacion_total
    }
    
    df <- bind_rows(fila_total, df)
    return(df)
  }
  

  data_filtrada <- reactive({
    filtrar_datos(base_sellin)
  })
  
  agregar_tabla <- function(df, nivel, variable) {
    variable <- rlang::sym(variable)
    
    if (nivel == "") {
      total_nivel <- df %>%
        group_by(Año) %>%
        summarise(Total_nivel = sum(!!variable, na.rm = TRUE), .groups = "drop")
      
      total_year <- total_nivel %>%
        rename(Total = Total_nivel) %>%
        mutate(Grupo = "Total")
      
      resumen <- total_year %>%
        mutate(
          part = 100,
          Total_nivel_fmt = formattable::currency(Total, symbol = "", digits = 0, big.mark = ".", decimal.mark = ","),
          var = Total
        ) %>%
        select(Año, Grupo, var) %>%
        pivot_wider(names_from = Año, values_from = var) 

    } else {
      agrupador <- rlang::sym(nivel)
      
      total_nivel <- df %>%
        group_by(Año, !!agrupador) %>%
        summarise(Total_nivel = sum(!!variable, na.rm = TRUE), .groups = "drop")
      
      total_year <- df %>%
        group_by(Año) %>%
        summarise(Total = sum(!!variable, na.rm = TRUE), .groups = "drop")
      
      resumen <- total_nivel %>%
        left_join(total_year, by = "Año") %>%
        mutate(
          part = round(100 * Total_nivel / Total, 1),
          Total_nivel_fmt = formattable::currency(Total_nivel, symbol = "", digits = 0, big.mark = ".", decimal.mark = ","),
          var = Total_nivel
        ) %>%
        select(Año, !!agrupador, var) %>%
        rename_with(~ c("Año", nivel, "var")) %>%
        pivot_wider(names_from = Año, values_from = var) 

    }
    
    return(resumen)
  }
  
  # Tablas ------------------------------------------------------------------
  
  excel_btn <- list(
    extend = "excel",
    filename = "reporte_sellin",  # nombre descarga
    exportOptions = list(
      orthogonal = "export",   # <-- usa el valor crudo para exportar
      stripHtml  = TRUE        # <-- por si hay estilos/formatos en celdas
    )
  )
  
  output$tabla_sku <- renderDT({
    req(input$nivel == "SKU")
    df <- data_filtrada()
    if (nrow(df) == 0) return(datatable(data.frame(Mensaje = "No hay datos para mostrar"), options = list(dom = 't')))
    
    resumen <- if (input$modo == "Ventas") {
      agregar_tabla(df, nivel = "Producto", variable = "Monto_Neto")
    } else {
      agregar_tabla(df, nivel = "Producto", variable = "Unidades_Confirmadas")
    }
    
    resumen <- resumen %>%
      left_join(df %>% distinct(Producto, SKU), by = "Producto") %>%
      relocate(SKU, .before = Producto)
    
    numeric_cols <- which(sapply(resumen, is.numeric)) - 1
    
    render_rules <- unname(lapply(numeric_cols, function(i) {
      structure(
        list(
          targets = i,
          render = JS(
            "function(data, type, row, meta) {",
            "  if(type === 'display') {",
            "    if(data === null || data === undefined || data === '') return '';",
            "    let num = Number(data);",
            "    if (isNaN(num)) return data;",
            "    return num.toFixed(0).replace(/\\B(?=(\\d{3})+(?!\\d))/g, '.');",
            "  } else {",
            "    return data;",
            "  }",
            "}"
          )
        ),
        class = "list"
      )
    }))
    
    coldefs <- c(render_rules, list(list(visible = FALSE, targets = 0)))  # 0 = SKU
    
    if (input$modo_fecha == "comparar_anios"){
      
      resumen <- agregar_fila_total(resumen, nivel = "Producto")
      
      # Crear columna con NA si falta la columna del año actual o del anterior
      if (!col_actual %in% names(resumen) || !col_ante %in% names(resumen)) {
        
        resumen[[nombre_variacion]] <- NA_real_
        
      } else {
        
        resumen <- resumen %>%
          mutate(
            !!nombre_variacion := ifelse(
              is.na(.data[[col_actual]]) |
                is.na(.data[[col_ante]])  |
                .data[[col_ante]] == 0,
              NA_real_,
              (.data[[col_actual]] - .data[[col_ante]]) / .data[[col_ante]]
            )
          )
      }
     
      datatable(
        resumen,
        filter = list(position = 'top', clear = FALSE),
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          dom = 'Blrtip',
          buttons = list('copy', 'csv', excel_btn),
          columnDefs = coldefs
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>% 
        formatPercentage(nombre_variacion, digits = 1) %>%
        formatStyle(
          columns = names(resumen),
          valueColumns = "Producto",
          target = "row",
          fontWeight = styleEqual("Total", "bold")
        )
      
    } else if (input$modo_fecha == "rango_absoluto") {
      
      resumen <- agregar_fila_total(resumen, nivel = "Producto") 
      
      if (nombre_variacion %in% names(resumen)) {
        resumen <- resumen %>% select(-all_of(nombre_variacion))
      }
      
      datatable(
        resumen,
        filter = list(position = 'top', clear = FALSE),
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          dom = 'Blrtip',
          buttons = list('copy', 'csv', excel_btn),
          columnDefs = coldefs
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
        formatStyle(
          columns = names(resumen),
          valueColumns = "Producto",
          target = "row",
          fontWeight = styleEqual("Total", "bold")
        )
    }
    

  })
  
  output$tabla_local <- renderDT({
    req(input$nivel == "Local")
    df <- data_filtrada()
    if (nrow(df) == 0) return(datatable(data.frame(Mensaje = "No hay datos para mostrar"), options = list(dom = 't')))
    
    resumen <- if (input$modo == "Ventas") {
      agregar_tabla(df, nivel = "Local", variable = "Monto_Neto")
    } else {
      agregar_tabla(df, nivel = "Local", variable = "Unidades_Confirmadas")
    }
    
    numeric_cols <- which(sapply(resumen, is.numeric)) - 1
    
    render_rules <- unname(lapply(numeric_cols, function(i) {
      structure(
        list(
          targets = i,
          render = JS(
            "function(data, type, row, meta) {",
            "  if(type === 'display') {",
            "    if(data === null || data === undefined || data === '') return '';",
            "    let num = Number(data);",
            "    if (isNaN(num)) return data;",
            "    return num.toFixed(0).replace(/\\B(?=(\\d{3})+(?!\\d))/g, '.');",
            "  } else {",
            "    return data;",
            "  }",
            "}"
          )
        ),
        class = "list"
      )
    }))
    
    if (input$modo_fecha == "comparar_anios"){
      
      resumen <- agregar_fila_total(resumen, nivel = "Local")
      
      # Si no existe la columna del año actual o del anterior → variación = NA
      if (!col_actual %in% names(resumen) || !col_ante %in% names(resumen)) {
        
        resumen[[nombre_variacion]] <- NA_real_
        
      } else {
        
        resumen <- resumen %>%
          mutate(
            !!nombre_variacion := ifelse(
              is.na(.data[[col_actual]]) |
                is.na(.data[[col_ante]])  |
                .data[[col_ante]] == 0,
              NA_real_,
              (.data[[col_actual]] - .data[[col_ante]]) / .data[[col_ante]]
            )
          )
      }
      
     
      datatable(
        resumen,
        filter = list(position = 'top', clear = FALSE),
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          dom = 'Blrtip',
          buttons = list('copy', 'csv', excel_btn),
          columnDefs = render_rules
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>% 
        formatPercentage(nombre_variacion, digits = 1) %>%
        formatStyle(
          columns = names(resumen),
          valueColumns = "Local",
          target = "row",
          fontWeight = styleEqual("Total", "bold")
        )
      
    } else if (input$modo_fecha == "rango_absoluto") {
      
      resumen <- agregar_fila_total(resumen, nivel = "Local") 
      
      if (nombre_variacion %in% names(resumen)) {
        resumen <- resumen %>% select(-all_of(nombre_variacion))
      }
      
      datatable(
        resumen,
        filter = list(position = 'top', clear = FALSE),
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          dom = 'Blrtip',
          buttons = list('copy', 'csv', excel_btn),
          columnDefs = render_rules
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
        formatStyle(
          columns = names(resumen),
          valueColumns = "Local",
          target = "row",
          fontWeight = styleEqual("Total", "bold")
        )
      
    }
  })
  
  output$tabla_categoria <- renderDT({
    req(input$nivel == "Categoria")
    df <- data_filtrada()
    if (nrow(df) == 0) return(datatable(data.frame(Mensaje = "No hay datos para mostrar"), options = list(dom = 't')))
    
    resumen <- if (input$modo == "Ventas") {
      agregar_tabla(df, nivel = "Categoria", variable = "Monto_Neto")
    } else {
      agregar_tabla(df, nivel = "Categoria", variable = "Unidades_Confirmadas")
    }
    
    numeric_cols <- which(sapply(resumen, is.numeric)) - 1
    
    render_rules <- unname(lapply(numeric_cols, function(i) {
      structure(
        list(
          targets = i,
          render = JS(
            "function(data, type, row, meta) {",
            "  if(type === 'display') {",
            "    if(data === null || data === undefined || data === '') return '';",
            "    let num = Number(data);",
            "    if (isNaN(num)) return data;",
            "    return num.toFixed(0).replace(/\\B(?=(\\d{3})+(?!\\d))/g, '.');",
            "  } else {",
            "    return data;",
            "  }",
            "}"
          )
        ),
        class = "list"
      )
    }))
    
    if (input$modo_fecha == "comparar_anios"){
      
      resumen <- agregar_fila_total(resumen, nivel = "Categoria")
      
      if (!col_actual %in% names(resumen) || !col_ante %in% names(resumen)) {
        
        resumen[[nombre_variacion]] <- NA_real_
        
      } else {
        
        resumen <- resumen %>%
          mutate(
            !!nombre_variacion := ifelse(
              is.na(.data[[col_actual]]) |
                is.na(.data[[col_ante]])  |
                .data[[col_ante]] == 0,
              NA_real_,
              (.data[[col_actual]] - .data[[col_ante]]) / .data[[col_ante]]
            )
          )
      }
      
      datatable(
        resumen,
        filter = list(position = 'top', clear = FALSE),
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          dom = 'Blrtip',
          buttons = list('copy', 'csv', excel_btn),
          columnDefs = render_rules
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>% 
        formatPercentage(nombre_variacion, digits = 1) %>%
        formatStyle(
          columns = names(resumen),
          valueColumns = "Categoria",
          target = "row",
          fontWeight = styleEqual("Total", "bold")
        )
      
    } else if (input$modo_fecha == "rango_absoluto") {
      
      resumen <- agregar_fila_total(resumen, nivel = "Categoria") 
      
      if (nombre_variacion %in% names(resumen)) {
        resumen <- resumen %>% select(-all_of(nombre_variacion))
      }
      
      datatable(
        resumen,
        filter = list(position = 'top', clear = FALSE),
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          dom = 'Blrtip',
          buttons = list('copy', 'csv', excel_btn),
          columnDefs = render_rules
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
        formatStyle(
          columns = names(resumen),
          valueColumns = "Categoria",
          target = "row",
          fontWeight = styleEqual("Total", "bold")
        )
      
    }
  })
  
  output$tabla_subcategoria <- renderDT({
    req(input$nivel == "Subcategoria")
    df <- data_filtrada()
    if (nrow(df) == 0) return(datatable(data.frame(Mensaje = "No hay datos para mostrar"), options = list(dom = 't')))
    
    resumen <- if (input$modo == "Ventas") {
      agregar_tabla(df, nivel = "Subcategoria", variable = "Monto_Neto")
    } else {
      agregar_tabla(df, nivel = "Subcategoria", variable = "Unidades_Confirmadas")
    }
    
    numeric_cols <- which(sapply(resumen, is.numeric)) - 1
    
    render_rules <- unname(lapply(numeric_cols, function(i) {
      structure(
        list(
          targets = i,
          render = JS(
            "function(data, type, row, meta) {",
            "  if(type === 'display') {",
            "    if(data === null || data === undefined || data === '') return '';",
            "    let num = Number(data);",
            "    if (isNaN(num)) return data;",
            "    return num.toFixed(0).replace(/\\B(?=(\\d{3})+(?!\\d))/g, '.');",
            "  } else {",
            "    return data;",
            "  }",
            "}"
          )
        ),
        class = "list"
      )
    }))
    
    if (input$modo_fecha == "comparar_anios"){
      
      resumen <- agregar_fila_total(resumen, nivel = "Subcategoria")
      
      if (!col_actual %in% names(resumen) || !col_ante %in% names(resumen)) {
        
        resumen[[nombre_variacion]] <- NA_real_
        
      } else {
        
        resumen <- resumen %>%
          mutate(
            !!nombre_variacion := ifelse(
              is.na(.data[[col_actual]]) |
                is.na(.data[[col_ante]])  |
                .data[[col_ante]] == 0,
              NA_real_,
              (.data[[col_actual]] - .data[[col_ante]]) / .data[[col_ante]]
            )
          )
      }
      
      datatable(
        resumen,
        filter = list(position = 'top', clear = FALSE),
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          dom = 'Blrtip',
          buttons = list('copy', 'csv', excel_btn),
          columnDefs = render_rules
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>% 
        formatPercentage(nombre_variacion, digits = 1) %>%
        formatStyle(
          columns = names(resumen),
          valueColumns = "Subcategoria",
          target = "row",
          fontWeight = styleEqual("Total", "bold")
        )
      
    } else if (input$modo_fecha == "rango_absoluto") {
      
      resumen <- agregar_fila_total(resumen, nivel = "Subcategoria") 
      
      if (nombre_variacion %in% names(resumen)) {
        resumen <- resumen %>% select(-all_of(nombre_variacion))
      }
      
      datatable(
        resumen,
        filter = list(position = 'top', clear = FALSE),
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          dom = 'Blrtip',
          buttons = list('copy', 'csv', excel_btn),
          columnDefs = render_rules
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
        formatStyle(
          columns = names(resumen),
          valueColumns = "Subcategoria",
          target = "row",
          fontWeight = styleEqual("Total", "bold")
        )
      
    }
  })
  
  output$tabla_total <- renderDT({
    req(input$nivel == "Total")
    df <- data_filtrada()
    if (nrow(df) == 0) return(datatable(data.frame(Mensaje = "No hay datos para mostrar"), options = list(dom = 't')))
    
    resumen <- if (input$modo == "Ventas") {
      agregar_tabla(df, nivel = "", variable = "Monto_Neto")
    } else {
      agregar_tabla(df, nivel = "", variable = "Unidades_Confirmadas")
    }
    
    if (nrow(resumen) == 0) return(datatable(data.frame(Mensaje = "No hay datos disponibles"), options = list(dom = 't')))
    
    numeric_cols <- which(sapply(resumen, is.numeric)) - 1
    
    render_rules <- unname(lapply(numeric_cols, function(i) {
      structure(
        list(
          targets = i,
          render = JS(
            "function(data, type, row, meta) {",
            "  if(type === 'display') {",
            "    if(data === null || data === undefined || data === '') return '';",
            "    let num = Number(data);",
            "    if (isNaN(num)) return data;",
            "    return num.toFixed(0).replace(/\\B(?=(\\d{3})+(?!\\d))/g, '.');",
            "  } else {",
            "    return data;",
            "  }",
            "}"
          )
        ),
        class = "list"
      )
    }))
    
    if (input$modo_fecha == "comparar_anios"){
      
      if (!col_actual %in% names(resumen) || !col_ante %in% names(resumen)) {
        
        resumen[[nombre_variacion]] <- NA_real_
        
      } else {
        
        resumen <- resumen %>%
          mutate(
            !!nombre_variacion := ifelse(
              is.na(.data[[col_actual]]) |
                is.na(.data[[col_ante]])  |
                .data[[col_ante]] == 0,
              NA_real_,
              (.data[[col_actual]] - .data[[col_ante]]) / .data[[col_ante]]
            )
          )
      }
      
     datatable(
        resumen,
        filter = list(position = 'top', clear = FALSE),
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          dom = 'Blrtip',
          buttons = list('copy', 'csv', excel_btn),
          columnDefs = render_rules
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>% 
        formatPercentage(nombre_variacion, digits = 1) %>%
       formatStyle(
         columns = names(resumen),
         valueColumns = "Grupo",
         target = "row",
         fontWeight = styleEqual("Total", "bold")
       )
      
    } else if (input$modo_fecha == "rango_absoluto") {
      
     datatable(
        resumen,
        filter = list(position = 'top', clear = FALSE),
        options = list(
          paging = FALSE,
          scrollX = TRUE,
          dom = 'Blrtip',
          buttons = list('copy', 'csv', excel_btn),
          columnDefs = render_rules
        ),
        extensions = 'Buttons',
        rownames = FALSE
      ) %>%
        formatStyle(
          columns = names(resumen),
          valueColumns = "Grupo",
          target = "row",
          fontWeight = styleEqual("Total", "bold")
        )
      
    }
  })
  

  # Actualizaciones de filtros ----------------------------------------------
  observeEvent(input$producto_id, {
    df <- filtrar_datos(base_sellin)
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
    df <- base_sellin
    if (input$subcategoria != "Todas") df <- df %>% filter(Subcategoria == input$subcategoria)
    #if (input$grupo != "Todos") df <- df %>% filter(Grupo == input$grupo)
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
    updateSelectInput(session, "subcategoria", choices = c("Todas", sort(unique(base_sellin$Subcategoria))))
    # updateSelectInput(session, "grupo", choices = c("Todos", sort(unique(base_sellin$Grupo))))
    updateSelectInput(session, "categoria", choices = c("Todas", sort(unique(base_sellin$Categoria))))
    updateSelectInput(session, "conjunto", choices = c("Todos", sort(unique(base_sellin$Conjunto))))
    updateSelectInput(session, "estado", choices = c("Todos", sort(unique(base_sellin$Estado))))
    # updateSelectInput(session, "zona", choices = c("Todas", sort(unique(base_sellin$Zona))))
    updateSelectInput(session, "zona", choices = c("Todas", sort(unique(base_sellin$Zona))), selected = "Todas")
    updateSelectInput(session, "local", choices = c("Todos", sort(unique(base_sellin$Local))))
    updateSelectInput(session, "producto_id", choices = c("Todos", obtener_opciones_producto(base_sellin)))
    updateSelectInput(session, "formato", choices = c("Todos", sort(unique(base_sellin$Formato))))
  })
  
  # observeEvent(input$categoria, {
  #   df <- base_sellout
  #   if (input$categoria != "Todas") df <- df %>% filter(Categoria == input$categoria)
  #   opciones_grupo <- c("Todos", sort(unique(df$Subcategoria)))
  #   actualizar_select_conservar("subcategoria", opciones_grupo, session, isolate(input$subcategoria))
  #   actualizar_productos()
  # })
  
  # observeEvent(input$grupo, {
  #   df <- base_sellout
  #   if (input$grupo != "Todos") df <- df %>% filter(Grupo == input$grupo)
  #   opciones_categoria <- c("Todas", sort(unique(df$Categoria)))
  #   opciones_conjunto <- c("Todos", sort(unique(df$Conjunto)))
  #   actualizar_select_conservar("categoria", opciones_categoria, session, isolate(input$categoria))
  #   actualizar_select_conservar("conjunto", opciones_conjunto, session, isolate(input$conjunto))
  #   actualizar_productos()
  # })
  
  observeEvent(input$categoria, {
    df <- base_sellin
    if (input$categoria != "Todos") df <- df %>% filter(Categoria == input$categoria)
    opciones_subcategoria <- c("Todas", sort(unique(df$Subcategoria)))
    # opciones_conjunto <- c("Todos", sort(unique(df$Conjunto)))
    actualizar_select_conservar("subcategoria", opciones_subcategoria, session, isolate(input$subcategoria))
    # actualizar_select_conservar("conjunto", opciones_conjunto, session, isolate(input$conjunto))
    actualizar_productos()
  })
  
  observeEvent(input$subcategoria, {
    df <- base_sellin
    if (input$subcategoria != "Todos") df <- df %>% filter(Subcategoria == input$subcategoria)
    # opciones_conjunto <- c("Todos", sort(unique(df$Conjunto)))
    # actualizar_select_conservar("conjunto", opciones_conjunto, session, isolate(input$conjunto))
    actualizar_productos()
  })
  
  # observeEvent(input$zona, {
  #   df <- base_sellin
  #   if (!is.null(input$zona) && length(input$zona) > 0 && !"Todas" %in% input$zona) {
  #     df <- df %>% filter(Zona %in% input$zona)
  #   }
  #   locales <- df %>% mutate(OpcionLocal = paste0(Local_ID, "-", Local)) %>%
  #     arrange(OpcionLocal) %>%
  #     pull(OpcionLocal) %>% unique()
  #   actualizar_select_conservar("local", c("Todos", locales), session, isolate(input$local))
  # })
  
  observeEvent(input$zona, {
    
    if ("Todas" %in% input$zona && length(input$zona) > 1) {
      if (tail(input$zona, 1) == "Todas") {
        updateSelectInput(session, "zona", selected = "Todas")
        return()
      } else {
        updateSelectInput(session, "zona", selected = setdiff(input$zona, "Todas"))
        return()
      }
    }
    
    df <- base_sellin
    if (!is.null(input$zona) && length(input$zona) > 0 && !"Todas" %in% input$zona) {
      df <- df %>% filter(Zona %in% input$zona)
    }
    
    locales <- df %>%
      mutate(OpcionLocal = paste0(Local_ID, "-", Local)) %>%
      arrange(OpcionLocal) %>%
      pull(OpcionLocal) %>%
      unique()
    
    actualizar_select_conservar("local", c("Todos", locales), session, isolate(input$local))
  })
  
  observeEvent(
    c(input$subcategoria, input$categoria, input$conjunto, input$estado, input$local),
    {
      seleccion_previa <- isolate(input$producto_id)
      df <- base_sellin
      if (input$subcategoria != "Todas") df <- df %>% filter(Subcategoria == input$subcategoria)
      #if (input$grupo != "Todos") df <- df %>% filter(Grupo == input$grupo)
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
    #updateSelectInput(session, "grupo", selected = "Todos")
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
