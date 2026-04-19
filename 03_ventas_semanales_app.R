
##########################################################
################## VENTAS SEMANALES ######################
##########################################################

# Carga de librerías
library(shiny)
library(DT)
library(dplyr)
library(tidyr)
library(purrr)
library(shinyjs)
library(digest)

# Carga de datos
base_sellout_semanal <- readRDS("sellout_semanal.rds") %>% 
 mutate(
    SKU = as.character(SKU),
    Codigo_Femaco = as.character(Codigo_Femaco),
    Nombre_Producto = as.character(Nombre_Producto),
    Producto = paste0(SKU, "-", Codigo_Femaco, "-", Nombre_Producto)
  )

# Obtener fecha más reciente de la base
fecha_base <- base_sellout_semanal %>%
  pull(`Fecha Carga`) %>%
  max(na.rm = TRUE) %>%
  as.Date() - 1 

# Función auxiliar para generar opciones de producto
obtener_opciones_producto <- function(df) {
  df %>%
    arrange(Producto) %>%
    pull(Producto) %>%
    unique()
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

# UI
ui <- fluidPage(
  useShinyjs(),
  tags$head(
    tags$title("Femaco · Ventas Semanales"),
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
      div(class = "custom-title", "Resumen por Semana de Ventas y Stock")
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
      tags$h4("Jerarquía de Producto"),
      selectInput("categoria", "Categoría", choices = c("Todas"), width = "100%"),
      #selectInput("grupo", "Grupo", choices = c("Todos"), width = "100%"),
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
      tabsetPanel(id = "nivel",
                  tabPanel("Producto", value = "SKU",
                           div(style = "position: relative;", DTOutput("tabla_sku"))),
                  tabPanel("Local", value = "Local",
                           div(style = "position: relative;", DTOutput("tabla_local"))),
                  tabPanel("Categoría", value = "Categoria",
                           div(style = "position: relative;", DTOutput("tabla_categoria"))),
                  tabPanel("Subcategoría", value = "Subcategoria",
                           div(style = "position: relative;", DTOutput("tabla_subcategoria"))),
                  tabPanel("Total", value = "Total",
                           div(style = "position: relative;", DTOutput("tabla_total")))
      )
    )
  )
)


# SERVER
formatear_rango_lindo <- function(rango) {
  fechas <- strsplit(rango, " al ")[[1]]
  inicio <- as.Date(fechas[1])
  fin <- as.Date(fechas[2])
  
  dia_inicio <- format(inicio, "%d") %>% sub("^0", "", .)
  mes_inicio <- format(inicio, "%b") %>% tolower()
  
  dia_fin <- format(fin, "%d") %>% sub("^0", "", .)
  mes_fin <- format(fin, "%b") %>% tolower()
  
  paste0(dia_inicio, " ", mes_inicio, "–", dia_fin, " ", mes_fin)
}

nombres_columnas_con_fechas <- function(df, modo) {
  rangos <- df %>%
    slice(1) %>%
    transmute(
      s4 = `rango_semana_4`,
      s3 = `rango_semana_3`,
      s2 = `rango_semana_2`,
      s1 = `rango_semana_1`,
      sa = `rango_semana_actual`
    ) %>%
    as.character()
  
  etiquetas <- sapply(rangos, formatear_rango_lindo)
  
  etiquetas_html <- paste0(
    if (modo == "Ventas") "Ventas" else "Unidades",
    "<br><span style='font-size: 10px; color: grey;'>", etiquetas, "</span>"
  )
  
  c(
    etiquetas_html,
    if (modo == "Ventas") "Total Ventas Semanas Cerradas" else "Total Unidades Semanas Cerradas",
    "Stock Físico"
  )
}

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
  
  actualizar_select_conservar <- function(input_id, choices, session, input_actual) {
    seleccion_valida <- intersect(input_actual, choices)
    selected_final <- if (length(seleccion_valida) > 0) seleccion_valida else choices[1]
    updateSelectInput(session, input_id, choices = choices, selected = selected_final)
  }
  
  actualizar_productos <- function() {
    seleccion_previa <- isolate(input$producto_id)
    df <- base_sellout_semanal
    if (input$categoria != "Todas") df <- df %>% filter(Categoria == input$categoria)
    #if (input$grupo != "Todos") df <- df %>% filter(Grupo == input$grupo)
    if (input$subcategoria != "Todas") df <- df %>% filter(Subcategoria == input$subcategoria)
    if (input$conjunto != "Todos") df <- df %>% filter(Conjunto == input$conjunto)
    if (input$estado != "Todos") df <- df %>% filter(Estado == input$estado)
    
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
    if (input$categoria != "Todas") df <- df %>% filter(Categoria == input$categoria)
    if (input$conjunto != "Todos") df <- df %>% filter(Conjunto == input$conjunto)
    if (input$subcategoria != "Todas") df <- df %>% filter(Subcategoria == input$subcategoria)
    if (input$formato != "Todos") df <- df %>% filter(Formato == input$formato)
    if (input$estado != "Todos") df <- df %>% filter(Estado == input$estado)
    if (input$zona != "Todas") df <- df %>% filter(Zona == input$zona)
    if (!is.null(input$local) && length(input$local) > 0 && !"Todos" %in% input$local) {
      ids_locales <- sapply(strsplit(input$local, "-"), function(x) x[1])
      df <- df %>% filter(Local_ID %in% ids_locales)
    }
    if (!is.null(input$producto_id) && !"Todos" %in% input$producto_id) {
      df <- df %>% filter(Producto %in% input$producto_id)
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
    
    # Insertar fila arriba
    df <- bind_rows(fila_total, df)
    return(df)
  }
  
  data_filtrada <- reactive({
    filtrar_datos(base_sellout_semanal)
  })
  
  
  # Tablas ------------------------------------------------------------------
  
  excel_btn <- list(
    extend = "excel",
    filename = "resumen_semana",   # ← nombre de la descarga
    exportOptions = list(
      orthogonal = "export",   # <-- usa el valor crudo para exportar
      stripHtml  = TRUE        # <-- por si hay estilos/formatos en celdas
    )
  )
  
  output$tabla_sku <- renderDT({
    req(input$nivel == "SKU")
    df <- data_filtrada()
    
    if (nrow(df) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para mostrar"), options = list(dom = 't')))
    }
    
    columnas_valores <- if (input$modo == "Ventas") {
      c("Ventas Semana 4", "Ventas Semana 3", "Ventas Semana 2", "Ventas Semana 1", "Ventas Semana Actual", "Total Ventas Semanas Cerradas", "Stock Físico")
    } else {
      c("Unidades Semana 4", "Unidades Semana 3", "Unidades Semana 2", "Unidades Semana 1", "Unidades Semana Actual", "Total Unidades Semanas Cerradas", "Stock Físico")
    }
    
    resumen <- df %>%
      group_by(SKU, Producto) %>%
      summarise(across(all_of(columnas_valores), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
    
    colnames(resumen)[3:ncol(resumen)] <- nombres_columnas_con_fechas(df, input$modo)
    
    resumen <- agregar_fila_total(resumen, nivel = "Producto")
    
    datatable(resumen,
              escape = FALSE,
              filter = list(position = 'top', clear = FALSE),
              options = list(
                paging = FALSE,
                scrollY = "600px",
                scrollX = TRUE,
                dom = 'Blrtip',
                buttons = list('copy', 'csv', excel_btn),
                language = list(decimal = ",", thousands = "."),
                # 👉 Oculta la 1ª columna (SKU) en la vista
                columnDefs = list(list(visible = FALSE, targets = 0))
              ),
              extensions = 'Buttons',
              rownames = FALSE) %>%
      formatRound(columns = 3:ncol(resumen), digits = 0, mark = ".", dec.mark = ",") %>%
      formatStyle(
        columns = names(resumen),
        valueColumns = "Producto",
        target = "row",
        fontWeight = styleEqual("Total", "bold")
      )
  })
  
  output$tabla_local <- renderDT({
    req(input$nivel == "Local")
    df <- data_filtrada()
    
    if (nrow(df) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para mostrar"), options = list(dom = 't')))
    }
    
    columnas_valores <- if (input$modo == "Ventas") {
      c("Ventas Semana 4", "Ventas Semana 3", "Ventas Semana 2", "Ventas Semana 1", "Ventas Semana Actual", "Total Ventas Semanas Cerradas", "Stock Físico")
    } else {
      c("Unidades Semana 4", "Unidades Semana 3", "Unidades Semana 2", "Unidades Semana 1", "Unidades Semana Actual", "Total Unidades Semanas Cerradas", "Stock Físico")
    }
    
    resumen <- df %>%
      group_by(Local) %>%
      summarise(across(all_of(columnas_valores), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
    
    colnames(resumen)[2:ncol(resumen)] <- nombres_columnas_con_fechas(df, input$modo)
    
    resumen <- agregar_fila_total(resumen, nivel = "Local")
    
    datatable(resumen,
              escape = FALSE,
              options = list(
                paging = FALSE,
                scrollY = "600px",
                scrollX = TRUE,
                dom = 'Blrtip',
                buttons = list('copy', 'csv', excel_btn),
                language = list(decimal = ",", thousands = ".")
              ),
              extensions = 'Buttons',
              rownames = FALSE) %>%
      formatRound(columns = 2:ncol(resumen), digits = 0, mark = ".", dec.mark = ",") %>%
      formatStyle(
        columns = names(resumen),
        valueColumns = "Local",
        target = "row",
        fontWeight = styleEqual("Total", "bold")
      )
  })
  
  output$tabla_categoria <- renderDT({
    req(input$nivel == "Categoria")
    df <- data_filtrada()
    
    if (nrow(df) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para mostrar"), options = list(dom = 't')))
    }
    
    columnas_valores <- if (input$modo == "Ventas") {
      c("Ventas Semana 4", "Ventas Semana 3", "Ventas Semana 2", "Ventas Semana 1", "Ventas Semana Actual", "Total Ventas Semanas Cerradas", "Stock Físico")
    } else {
      c("Unidades Semana 4", "Unidades Semana 3", "Unidades Semana 2", "Unidades Semana 1", "Unidades Semana Actual", "Total Unidades Semanas Cerradas", "Stock Físico")
    }
    
    resumen <- df %>%
      group_by(Categoria) %>%
      summarise(across(all_of(columnas_valores), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
    
    colnames(resumen)[2:ncol(resumen)] <- nombres_columnas_con_fechas(df, input$modo)
    
    resumen <- agregar_fila_total(resumen, nivel = "Categoria")
    
    datatable(resumen,
              escape = FALSE,
              options = list(
                paging = FALSE,
                scrollY = "600px",
                scrollX = TRUE,
                dom = 'Blrtip',
                buttons = list('copy', 'csv', excel_btn),
                language = list(decimal = ",", thousands = ".")
              ),
              extensions = 'Buttons',
              rownames = FALSE) %>%
      formatRound(columns = 2:ncol(resumen), digits = 0, mark = ".", dec.mark = ",") %>%
      formatStyle(
        columns = names(resumen),
        valueColumns = "Categoria",
        target = "row",
        fontWeight = styleEqual("Total", "bold")
      )
  })
  
  output$tabla_subcategoria <- renderDT({
    req(input$nivel == "Subcategoria")
    df <- data_filtrada()
    
    if (nrow(df) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para mostrar"), options = list(dom = 't')))
    }
    
    columnas_valores <- if (input$modo == "Ventas") {
      c("Ventas Semana 4", "Ventas Semana 3", "Ventas Semana 2", "Ventas Semana 1", "Ventas Semana Actual", "Total Ventas Semanas Cerradas", "Stock Físico")
    } else {
      c("Unidades Semana 4", "Unidades Semana 3", "Unidades Semana 2", "Unidades Semana 1", "Unidades Semana Actual", "Total Unidades Semanas Cerradas", "Stock Físico")
    }
    
    resumen <- df %>%
      group_by(Subcategoria) %>%
      summarise(across(all_of(columnas_valores), ~ sum(.x, na.rm = TRUE)), .groups = "drop")
    
    colnames(resumen)[2:ncol(resumen)] <- nombres_columnas_con_fechas(df, input$modo)
    
    resumen <- agregar_fila_total(resumen, nivel = "Subcategoria")
    
    datatable(resumen,
              escape = FALSE,
              options = list(
                paging = FALSE,
                scrollY = "600px",
                scrollX = TRUE,
                dom = 'Blrtip',
                buttons = list('copy', 'csv', excel_btn),
                language = list(decimal = ",", thousands = ".")
              ),
              extensions = 'Buttons',
              rownames = FALSE) %>%
      formatRound(columns = 2:ncol(resumen), digits = 0, mark = ".", dec.mark = ",") %>%
      formatStyle(
        columns = names(resumen),
        valueColumns = "Subcategoria",
        target = "row",
        fontWeight = styleEqual("Total", "bold")
      )
  })
  
  output$tabla_total <- renderDT({
    req(input$nivel == "Total")
    
    df <- data_filtrada()
    
    if (nrow(df) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para mostrar"), options = list(dom = 't')))
    }
    
    columnas_valores <- if (input$modo == "Ventas") {
      c("Ventas Semana 4", "Ventas Semana 3", "Ventas Semana 2", "Ventas Semana 1", "Ventas Semana Actual", "Total Ventas Semanas Cerradas", "Stock Físico")
    } else {
      c("Unidades Semana 4", "Unidades Semana 3", "Unidades Semana 2", "Unidades Semana 1", "Unidades Semana Actual", "Total Unidades Semanas Cerradas", "Stock Físico")
    }
    
    resumen <- df %>%
      summarise(across(all_of(columnas_valores), ~ sum(.x, na.rm = TRUE))) %>% 
      mutate(Resumen = "Total") %>%
      relocate(Resumen)
    
    colnames(resumen)[2:ncol(resumen)] <- nombres_columnas_con_fechas(df, input$modo)
    
    datatable(resumen,
              escape = FALSE,
              options = list(
                paging = FALSE,
                scrollY = "600px",
                scrollX = TRUE,
                dom = 'Blrtip',
                buttons = list('copy', 'csv', excel_btn),
                language = list(decimal = ",", thousands = ".")
              ),
              extensions = 'Buttons',
              rownames = FALSE) %>%
      formatRound(columns = 2:ncol(resumen), digits = 0, mark = ".", dec.mark = ",") %>%
      formatStyle(
        columns = 1,
        target = "row",
        fontWeight = styleEqual("Total", "bold")
      )
  })
  
  # Actualizaciones de filtros ----------------------------------------------
  observeEvent(input$producto_id, {
    df <- filtrar_datos(base_sellout_semanal)
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
    df <- base_sellout_semanal
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
    updateSelectInput(session, "subcategoria", choices = c("Todas", sort(unique(base_sellout_semanal$Subcategoria))))
    updateSelectInput(session, "categoria", choices = c("Todas", sort(unique(base_sellout_semanal$Categoria))))
    updateSelectInput(session, "conjunto", choices = c("Todos", sort(unique(base_sellout_semanal$Conjunto))))
    updateSelectInput(session, "estado", choices = c("Todos", sort(unique(base_sellout_semanal$Estado))))
    updateSelectInput(session, "zona", choices = c("Todas", sort(unique(base_sellout_semanal$Zona))))
    updateSelectInput(session, "local", choices = c("Todos", sort(unique(base_sellout_semanal$Local))))
    updateSelectInput(session, "producto_id", choices = c("Todos", obtener_opciones_producto(base_sellout_semanal)))
    updateSelectInput(session, "formato", choices = c("Todos", sort(unique(base_sellout_semanal$Formato))))
  })
  

  observeEvent(input$categoria, {
    df <- base_sellout_semanal
    if (input$categoria != "Todos") df <- df %>% filter(Categoria == input$categoria)
    opciones_subcategoria <- c("Todas", sort(unique(df$Subcategoria)))
    actualizar_select_conservar("subcategoria", opciones_subcategoria, session, isolate(input$subcategoria))
    actualizar_productos()
  })
  
  observeEvent(input$subcategoria, {
    df <- base_sellout_semanal
    if (input$subcategoria != "Todos") df <- df %>% filter(Subcategoria == input$subcategoria)
    actualizar_productos()
  })
  
  observeEvent(input$zona, {
    df <- base_sellout_semanal
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
      df <- base_sellout_semanal
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
