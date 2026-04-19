
##########################################################
####################### OBJETIVOS ########################
##########################################################

# Carga de librerías
library(shiny)
library(DT)
library(dplyr)
library(purrr)
library(digest)
library(tidyr)
library(rlang)


# Carga de datos
reposicion <- readRDS("objetivos.rds") %>% 
  mutate(
    SKU = as.character(SKU),
    #Local = paste0(Local_ID, "-", Local, "-", dias_lead_time),
    #Local = paste0(Local_ID, "-", Local),
    Codigo_Femaco = as.character(Codigo_Femaco),
    Producto = paste0(SKU, "-", Codigo_Femaco, "-", Nombre_Producto),
    Locales = paste0(Local_ID, "-", Local)
  )

# Obtener fecha más reciente de la base
fecha_base <- reposicion %>%
  pull(`Fecha Carga`) %>%
  max(na.rm = TRUE) %>%
  as.Date() - 1

# Orden fijo de locales (por nombre de columna que usarás en la tabla)
orden_locales <- reposicion %>%
  distinct(Local) %>%        
  arrange(Local) %>%         # Orden alfabético
  pull(Local)

# Función auxiliar para generar opciones de producto
obtener_opciones_producto <- function(df) {
  df %>%
    distinct(Producto) %>%
    arrange(Producto) %>%
    pull(Producto)
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
  tags$head(
    tags$title("Femaco · Objetivos"),
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
    
      div.dt-buttons {
      position: relative;
      margin-bottom: 20px;
      }

      .dataTables_wrapper {
      position: relative !important;
      }
    
      div.dataTables_wrapper div.dataTables_filter {
      float: right;
      text-align: right;
      margin-top: 10px;
      } 
      
      .dataTables_wrapper {
      width: 100% !important;
      overflow-x: auto;
      }
      table.dataTable {
      width: 100% !important;
      word-wrap: break-word;
      }
    "
    ))
  ),
  
  div(class = "custom-header",
      tags$img(src = "logo.jpg", height = "50px"),
      div(class = "custom-title", "Base para Objetivos")
  ),
  tags$p(style = "margin-left: 15px; font-style: italic;",
         paste("Datos actualizados al", format(fecha_base, "%d/%m/%Y"))),
  # Fila 1: filtros
  fluidRow(
    column(2, selectInput("zona",       "Zona",         choices = c("Todas"), width = "100%")),
    column(2, selectInput("ruta",       "Rutas",        choices = c("Todas"), width = "100%")),
    column(2, selectInput("categoria",  "Categoría",    choices = c("Todas"), width = "100%")),
    column(2, selectInput("subcategoria","Subcategoría",choices = c("Todas"), width = "100%")),
    column(2, selectInput("conjunto", "Conjunto", choices = c("Todos"), width = "100%")),
    column(2, selectInput("estado", "Estado", choices = c("Todos"), width = "100%"))
  ),
  fluidRow(
    column(2, selectInput("formato",    "Formato",      choices = c("Todos"), width = "100%")),
    column(3, selectInput("local", "Local (ID - Nombre)", choices = c("Todos"), multiple = TRUE, selectize = TRUE, width = "100%")),
    column(3, selectInput("producto_id", "Producto (SKU - Código - Nombre)",
                          choices = c("Todos"), multiple = TRUE, selectize = TRUE, width = "100%")),
    column(2, selectInput("producto_crit", "Productos Críticos", choices = c("Todos","Productos críticos con stock insuficiente", "Productos críticos con stock suficiente"), width = "100%")),
    column(2, br(), actionButton("reset_filtros", "Limpiar filtros", icon = icon("broom"),
                                 style = "background-color: #7ab943; border-color: #7ab943; color: white;"))
  ),
  # Fila 2: radio buttons
  fluidRow(
    column(12,
           tags$div(
             id = "box-metrica",
             style = "background-color:#7ab943; padding: 5px 10px; border-radius: 5px; color: white;",
             tags$strong("Métrica a mostrar"),
             radioButtons(
               inputId = "metrica", label = NULL, inline = TRUE,
               choices = c(
                 "Stock"                      = "Stock",
                 "Máximo sell out mensual"    = "ventas_max_hist",
                 "Ventas último mes cerrado"  = "ventas_mes_anterior",
                 "Ventas mes en curso" = "venta_mes_actual",
                 "Sugerencia cuadrada en u/e" = "Sugerencia"
               ),
               selected = "Stock"
             )
           ),
           
           tags$head(tags$style(HTML("
  #box-metrica .shiny-options-group {
    display: flex;
    justify-content: space-between; /* reparte el espacio horizontal */
    gap: 8px;
    flex-wrap: wrap;                 /* permite salto de línea si no caben */
    align-items: center;
  }
  #box-metrica .radio-inline { color: #fff; margin: 0; }
  #box-metrica .radio-inline input[type='radio'] { margin-right: 6px; }
")))
           
           
    )
  ),
  br(),
  fluidRow(
    column(
      width = 12,
      id = "main-panel",
      div(style = "overflow-x: auto;", DTOutput("tabla_sku"))
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
  
  filtrar_datos <- function(df,var) {
    if (input$zona != "Todas") {
      df <- df %>% filter(Zona == input$zona)
    }
    
    if (!is.null(input$ruta) && input$ruta != "Todas") {
      df <- df %>% filter(Rutas == input$ruta)
    }
    
    if (!is.null(input$categoria) && input$categoria != "Todas") {
      df <- df %>% filter(Categoria == input$categoria)
    }
    
    if (!is.null(input$subcategoria) && input$subcategoria != "Todas") {
      df <- df %>% filter(Subcategoria == input$subcategoria)
    }
    
    if (!is.null(input$conjunto) && input$conjunto != "Todos") {
      df <- df %>% filter(Conjunto == input$conjunto)
    }
    
    if (!is.null(input$estado) && input$estado != "Todos") {
      df <- df %>% filter(Estado == input$estado)
    }
    
    if (!is.null(input$formato) && input$formato != "Todos") {
      df <- df %>% filter(Formato == input$formato)
    }
    
    if (!is.null(input$producto_id) && !"Todos" %in% input$producto_id) {
      df <- df %>% filter(Producto %in% input$producto_id)
    }
    
    if (!is.null(input$local) && !"Todos" %in% input$local) {
      df <- df %>% filter(Local %in% input$local)
    }
    
    if (!is.null(input$producto_crit) && !"Todos" %in% input$producto_crit) {
      if (input$producto_crit %in% "Productos críticos con stock insuficiente"){
        df <- df %>% filter(producto_critico %in% 1, Sugerencia_Unidades > 0)
      }
      if (input$producto_crit %in% "Productos críticos con stock suficiente"){
        df <- df %>% filter(producto_critico %in% 1, Sugerencia_Unidades <= 0)
      }
    }
    
    df <- df %>%
      rename(
        Stock = `Stock Físico`) %>%
        # `Máximo sell out mensual` = ventas_max_hist,
        # `Ventas Último Mes Cerrado` = ventas_mes_anterior
      mutate(Sugerencia = pmax(Sugerencia_Empaques * `Unidades X Pallet`, 0)) %>% 
      dplyr::select(
        SKU, Producto,  # <-- conservarlas
        Local, Stock, ventas_max_hist, ventas_mes_anterior, venta_mes_actual, Sugerencia
      ) %>% 
      tidyr::pivot_wider(
        id_cols     = c(SKU, Producto), 
        names_from  = Local,
        values_from = !!var,
        values_fill = NA
      ) 
    
    # --- Reordenar SIEMPRE las columnas de locales según orden_locales ---
    # columnas presentes en este df que son locales
    local_cols_presentes <- intersect(orden_locales, names(df))

    df <- df %>%
      select(
        SKU, Producto,
        dplyr::all_of(local_cols_presentes)   # mismos locales, mismo orden SIEMPRE
      ) %>%
      mutate(
        Total = rowSums(across(dplyr::all_of(local_cols_presentes)), na.rm = TRUE)
      )
    
    return(df)
  }
  
  output$tabla_sku <- renderDT({
    
    df <- filtrar_datos(reposicion,input$metrica) # Stock,ventas_max_hist,ventas_mes_anterior,Sugerencia
    
    if (nrow(df) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para mostrar"), options = list(dom = 't')))
    }
    
    datatable(
      df,
      escape = FALSE,
      filter = list(position = 'top', clear = FALSE),
      extensions = c('Buttons', 'FixedHeader'),
      rownames = FALSE,
      options = list(
        dom = 'Bfrtip',
        #searching = FALSE,
        paging = FALSE,
        scrollY = "500px",
        scrollX = TRUE,
        scrollCollapse = TRUE,
        fixedHeader = TRUE,
        buttons = list(
          'copy',        # deja copy tal cual
          'csv',         # deja csv tal cual
          list(          # personalizas solo excel
            extend = "excel",
            filename = "reporte_objetivos"  # ← nombre de la descarga
          )
        ),
        language = list(decimal = ",", thousands = "."),
        columnDefs = list(
          list(visible = FALSE, targets = 0)  # 0 = primera columna (SKU)
        )
      )
    )
  })
  

  observe({
    zonas <- sort(unique(reposicion$Zona))
    updateSelectInput(session, "zona", choices = c("Todas", zonas), selected = "Todas")
  })
  
  observe({
    df <- reposicion
    updateSelectInput(session, "local",        choices = c("Todos", sort(unique(df$Local))),        selected = "Todos")
    updateSelectInput(session, "ruta",         choices = c("Todas", sort(unique(df$Rutas))),        selected = "Todas")
    updateSelectInput(session, "categoria",    choices = c("Todas", sort(unique(df$Categoria))),    selected = "Todas")
    updateSelectInput(session, "subcategoria", choices = c("Todas", sort(unique(df$Subcategoria))), selected = "Todas")
    updateSelectInput(session, "conjunto",     choices = c("Todas", sort(unique(df$Conjunto))),     selected = "Todas")
    updateSelectInput(session, "estado",       choices = c("Todas", sort(unique(df$Estado))),       selected = "Todas")
    updateSelectInput(session, "formato",      choices = c("Todos", sort(unique(df$Formato))),      selected = "Todos")
    updateSelectInput(session, "producto_id",  choices = c("Todos", sort(unique(df$Producto))),     selected = "Todos")
  })
  
  

  observeEvent(input$zona, {
    df <- if (is.null(input$zona) || input$zona == "Todas") reposicion else reposicion %>% dplyr::filter(Zona == input$zona)
    rutas <- sort(unique(df$Rutas))
    # locales <- sort(unique(df$Local))
    updateSelectInput(session, "ruta", choices = c("Todas", rutas), selected = "Todas")
    # updateSelectInput(session, "local", choices = c("Todos", locales), selected = "Todos")
  })
  
  
  observeEvent(input$ruta, {

      df <- reposicion
      if (input$zona != "Todas") df <- df %>% filter(Zona == input$zona)
    
      locales <- sort(unique(df$Local))
      updateSelectInput(session, "local",
                        choices = c("Todos", locales),
                        selected = "Todos")

      categorias <- sort(unique(df$Categoria))
      updateSelectInput(session, "categoria",
                        choices = c("Todas", categorias),
                        selected = "Todas")
      
      subcategorias <- sort(unique(df$Subcategoria))
      updateSelectInput(session, "subcategoria",
                        choices = c("Todas", subcategorias),
                        selected = "Todas")
      
      conjuntos <- sort(unique(df$Conjunto))  
      updateSelectInput(session, "conjunto",  
                        choices = c("Todos", conjuntos),
                        selected = "Todos")
      
      estados <- sort(unique(df$Estado))  
      updateSelectInput(session, "estado",  
                        choices = c("Todos", estados),
                        selected = "Todos")
      
      formatos <- sort(unique(df$Formato))  
      updateSelectInput(session, "formato",  
                        choices = c("Todos", formatos),
                        selected = "Todos")
      
      updateSelectInput(session, "producto_id", choices = c("Todos"), selected = "Todos")

  })
  
  observeEvent(c(input$ruta, input$categoria, input$conjunto, input$estado, input$formato, input$subcategoria, input$zona), {
    
    df_all <- reposicion
    
    # --- 1) Filtrado aguas arriba común (zona + ruta + categoría) ---
    if (!is.null(input$zona)      && input$zona      != "Todas") df_all <- df_all %>% dplyr::filter(Zona     == input$zona)
    if (!is.null(input$ruta)      && input$ruta      != "Todas") df_all <- df_all %>% dplyr::filter(Rutas    == input$ruta)
    if (!is.null(input$categoria) && input$categoria != "Todas") df_all <- df_all %>% dplyr::filter(Categoria == input$categoria)

    
    locales_actuales <- sort(unique(df_all$Local))
    local_actual <- isolate(input$local)
    
    if (is.null(local_actual) || length(local_actual) == 0 ||
        any(!local_actual %in% c("Todos", locales_actuales))) {
      local_actual <- "Todos"
    }
    
    updateSelectInput(session, "local",
                      choices  = c("Todos", locales_actuales),
                      selected = local_actual)
    
    df_sc <- df_all
    if (!is.null(input$local) && !"Todos" %in% input$local) {
      df_sc <- df_sc %>% dplyr::filter(Local %in% input$local)
    }
    
    # --- 2) Subcategoría: calcular opciones disponibles y preservar selección ---
    #     (mismo patrón que ya tenías para Formato)
    #     Importante: no uses aún el filtro de subcategoría para calcular sus propias opciones
    subcats_actuales <- sort(unique(df_all$Subcategoria))
    subcat_actual <- isolate(input$subcategoria)
    if (is.null(subcat_actual) || !(subcat_actual %in% c("Todas", subcats_actuales))) {
      subcat_actual <- "Todas"
    }
    updateSelectInput(session, "subcategoria",
                      choices  = c("Todas", subcats_actuales),
                      selected = subcat_actual)
    
    # Ahora sí, aplica subcategoría si corresponde para el resto de cálculos
    df_sc <- df_all
    if (!is.null(input$subcategoria) && input$subcategoria != "Todas") {
      df_sc <- df_sc %>% dplyr::filter(Subcategoria == input$subcategoria)
    }
    
    # --- 3) Formato: calcular opciones disponibles y preservar selección ---
    conjuntos_actuales <- sort(unique(df_sc$Conjunto))
    conjunto_actual <- isolate(input$conjunto)
    if (is.null(conjunto_actual) || !(conjunto_actual %in% c("Todos", conjuntos_actuales))) {
      conjunto_actual <- "Todos"
    }
    updateSelectInput(session, "conjunto",
                      choices  = c("Todos", conjuntos_actuales),
                      selected = conjunto_actual)
    df_sf <- df_sc
    if (!is.null(input$conjunto) && input$conjunto != "Todos") {
      df_sf <- df_sf %>% dplyr::filter(Conjunto == input$conjunto)
    }
    
    estados_actuales <- sort(unique(df_sc$Estado))
    estado_actual <- isolate(input$estado)
    if (is.null(estado_actual) || !(estado_actual %in% c("Todos", estados_actuales))) {
      estado_actual <- "Todos"
    }
    updateSelectInput(session, "estado",
                      choices  = c("Todos", estados_actuales),
                      selected = estado_actual)
    df_sf <- df_sc
    if (!is.null(input$estado) && input$estado != "Todos") {
      df_sf <- df_sf %>% dplyr::filter(Estado == input$estado)
    }
    
    formatos_actuales <- sort(unique(df_sc$Formato))
    formato_actual <- isolate(input$formato)
    if (is.null(formato_actual) || !(formato_actual %in% c("Todos", formatos_actuales))) {
      formato_actual <- "Todos"
    }
    updateSelectInput(session, "formato",
                      choices  = c("Todos", formatos_actuales),
                      selected = formato_actual)
    df_sf <- df_sc
    if (!is.null(input$formato) && input$formato != "Todos") {
      df_sf <- df_sf %>% dplyr::filter(Formato == input$formato)
    }
    
    # --- 4) Productos disponibles según todo lo anterior ---
    productos <- sort(unique(df_sf$Producto))
    updateSelectInput(session, "producto_id",
                      choices  = c("Todos", productos),
                      selected = "Todos")
  })
  
  observeEvent(input$producto_id, {
    if ("Todos" %in% input$producto_id && length(input$producto_id) > 1) {
      updateSelectInput(session, "producto_id", selected = "Todos")
    }
  })
  
  
  
  observeEvent(input$local, {
    if ("Todos" %in% input$local && length(input$local) > 1) {
      updateSelectInput(session, "local", selected = "Todos")
    }
  })
  
  
  observeEvent(input$reset_filtros, {
    session$reload()
  })
  
}

shinyApp(ui, server)
