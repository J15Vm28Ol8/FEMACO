
##########################################################
##################### REPOSICIÓN #########################
##########################################################

# Carga de librerías
library(shiny)
library(DT)
library(dplyr)
library(purrr)
library(digest)

# Carga de datos
reposicion <- readRDS("reposicion.rds") %>% 
  mutate(
    SKU = as.character(SKU),
    Local = paste0(Local_ID, "-", Local, "-", dias_lead_time),
    Codigo_Femaco = as.character(Codigo_Femaco),
    Producto = paste0(SKU, "-", Codigo_Femaco, "-", Nombre_Producto)
  )

# Obtener fecha más reciente de la base
fecha_base <- reposicion %>%
  pull(`Fecha Carga`) %>%
  max(na.rm = TRUE) %>%
  as.Date() - 1

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
    tags$title("Femaco · Reposición"),
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
      div(class = "custom-title", "Sugerencias de Reposición")
  ),
  tags$p(style = "margin-left: 15px; font-style: italic;",
         paste("Datos actualizados al", format(fecha_base, "%d/%m/%Y"))),
  fluidRow(
    column(2,
           selectInput("categoria", "Categoría", choices = c("Todas"), width = "100%")
    ),
    column(2,
           selectInput("subcategoria", "Subcategoría", choices = c("Todas"), width = "100%")
    ),
    column(2,
           selectInput("conjunto", "Conjunto", choices = c("Todas"), width = "100%")
    ),
    column(2,
           selectInput("estado", "Estado", choices = c("Todas"), width = "100%")
    ),
    column(2,
           selectInput("formato", "Formato", choices = c("Todas"), width = "100%")
    )
  ),
  fluidRow(
    column(2,
           selectInput("zona", "Zona", choices = c("Todas"), width = "100%")
    ),
    column(2,
           selectInput("local", "Local (ID - Nombre - Lead Time Días)", choices = NULL, width = "100%")
    ),
    column(3,
           selectInput("producto_id", "Producto (SKU - Código - Nombre)",
                       choices = c("Todos"), multiple = TRUE, selectize = TRUE, width = "100%")
    ),
    column(2,
           selectInput("producto_crit", "Productos Críticos", choices = c("Todos","Productos críticos con stock insuficiente", "Productos críticos con stock suficiente"), width = "100%")
    ),
    column(3,
           br(),
           actionButton("reset_filtros", "Limpiar filtros", icon = icon("broom"),
                        style = "background-color: #7ab943; border-color: #7ab943; color: white;")
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
  
  filtrar_datos <- function(df) {
    df <- df %>% filter(Local == input$local)
    
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
    
    if (input$zona != "Todas") {
      df <- df %>% filter(Zona == input$zona)
    }
    
    if (is.null(input$local) || input$local == "") {
      return(df[0, ])
    }
    
    if (!is.null(input$producto_id) && !"Todos" %in% input$producto_id) {
      df <- df %>% filter(Producto %in% input$producto_id)
    }
    
    if (!is.null(input$producto_crit) && !"Todos" %in% input$producto_crit) {
      if (input$producto_crit %in% "Productos críticos con stock insuficiente"){
        df <- df %>% filter(producto_critico %in% 1, Sugerencia_Unidades > 0)
      }
      if (input$producto_crit %in% "Productos críticos con stock suficiente"){
        df <- df %>% filter(producto_critico %in% 1, Sugerencia_Unidades <= 0)
      }
    }
    
    return(df)
  }
  
  data_filtrada <- reactive({
    
    filtrar_datos(reposicion)
  })
  
  output$tabla_sku <- renderDT({
    
    # Verificamos si el input$local está vacío o no tiene datos en la zona seleccionada
    df_check <- reposicion
    if (input$zona != "Todas") {
      df_check <- df_check %>% filter(Zona == input$zona)
    }
    
    if (is.null(input$local) || input$local == "" || !(input$local %in% df_check$Local)) {
      return(datatable(
        data.frame(Mensaje = "<div style='text-align:center; font-style:italic;'>Seleccione un local</div>"),
        escape = FALSE,
        options = list(dom = 't', paging = FALSE),
        rownames = FALSE,
        colnames = NULL
      ))
    }
    
    df <- data_filtrada()
    
    if (nrow(df) == 0) {
      return(datatable(data.frame(Mensaje = "No hay datos para mostrar"), options = list(dom = 't')))
    }
    
    resumen <- df %>%
      transmute(
        SKU,
        Producto,
        `Ventas Semana 1`,
        `Ventas Semana 2`,
        `Ventas Semana 3`,
        `Ventas Semana 4`,
        `% Participación en tienda` = por_part_tienda,
        `Tránsito` = Movimientos_Pendientes,
        `Unidades por Empaque` = `Unidades X Pallet`,
        `Máximo sell in mensual` = despacho_max_hist,
        `Mes máximo sell in` = Fecha_despacho_max_hist,
        `Ventas Último Mes Cerrado` = ventas_mes_anterior,
        `Máximo sell out mensual` = ventas_max_hist, # mes con mayor venta en la historia
        `Mes máximo sell out` = Fecha_max_hist, # mes con mayor venta en la historia
        `Ventas Mes en Curso` = venta_mes_actual,
        `Ventas Mismo Mes Año Anterior` = ventas_mismo_mes_año_anterior,
        `Stock` = `Stock Físico`,
        `Lead Time (días)` = dias_lead_time,
        Gancheras = Gancheras,
        `Sugerencia de reposición unidades` = Sugerencia_Unidades,
        `Sugerencia de reposición empaques` = Sugerencia_Empaques,
        `Movimientos Pendientes` = Movimientos_Pendientes,
        `Total Unidades Mes Cerrado` = `Total Unidades Semanas Cerradas`
      ) %>%
      mutate(
        `Sobre Stock` = `Stock` - (`Máximo sell out mensual`* 1.5),
        `Exceso Pallet` = `Stock` > (`Máximo sell out mensual`* 1.5),
        `Sugerencia cuadrada en u/e` = `Sugerencia de reposición empaques` * `Unidades por Empaque`
      ) 
    
    resumen <- resumen %>%
      arrange(`Sobre Stock` == 0, `Sobre Stock`) %>%  
      select(
        SKU,
        Producto,
        `Máximo sell in mensual`,
        `Mes máximo sell in`,
        `Máximo sell out mensual`,
        `Mes máximo sell out`,
        `Ventas Semana 1`,
        `Ventas Semana 2`,
        `Ventas Semana 3`,
        `Ventas Semana 4`,
        `% Participación en tienda`,
        `Tránsito`,
        `Ventas Mismo Mes Año Anterior`,
        `Ventas Último Mes Cerrado`,
        `Ventas Mes en Curso`,
        Stock,
        `Lead Time (días)`,
        Gancheras,
        `Sugerencia de reposición unidades`,
        `Sugerencia cuadrada en u/e`,
        `Exceso Pallet`,  # <- Necesaria para el formatStyle
        `Sobre Stock`) %>%
      mutate(`Sugerencia de reposición unidades` = ifelse(`Sugerencia de reposición unidades` < 0, 0, `Sugerencia de reposición unidades`),
             `Sugerencia cuadrada en u/e` = ifelse(`Sugerencia cuadrada en u/e` < 0, 0, `Sugerencia cuadrada en u/e`))

    datatable(
      resumen %>% select(- `Lead Time (días)`),
      escape = FALSE,
      filter = list(position = 'top', clear = FALSE),
        options = list(
          #searching = FALSE,
          paging = FALSE,
          scrollY = "500px",
          scrollX = TRUE,
          scrollCollapse = TRUE,
          fixedHeader = TRUE,
          dom = 'Bfrtip',
          buttons = list(
            'copy',        # deja copy tal cual
            'csv',         # deja csv tal cual
            list(          # personalizas solo excel
              extend = "excel",
              filename = "reporte_reposicion"  # ← nombre del archivo sin extensión
            )
          ),
          language = list(decimal = ",", thousands = "."),
          columnDefs = list(list(visible = FALSE, targets = c(0, 1, 20, 21)))  # índices de las columnas Exceso Pallet y Sobre Stock
        )
    ) %>% 
      formatRound(columns = c(3,5,7:10,13:19,21), digits = 0, mark = ".", dec.mark = ",") %>%
      formatStyle(
        columns = "Producto",
        valueColumns = "Sugerencia de reposición unidades",
        backgroundColor = styleInterval(0, c(NA, "#bbe696"))
      ) %>%
      formatStyle(
        columns = "Producto",
        valueColumns = "Exceso Pallet",
        backgroundColor = styleEqual(c(FALSE, TRUE), c(NA, "lightcoral")),
        color = styleEqual(TRUE, "white")
      )

  })
  


# Filtros -----------------------------------------------------------------

  observe({
    zonas <- sort(unique(reposicion$Zona))
    updateSelectInput(session, "zona", choices = c("Todas", zonas), selected = "Todas")
  })
  
  observeEvent(input$zona, {
    df <- if (input$zona == "Todas") reposicion else reposicion %>% filter(Zona == input$zona)
    
    locales <- sort(unique(df$Local))
    
    # Mantener local si sigue siendo válido
    selected_local <- isolate(input$local)
    if (!(selected_local %in% locales)) {
      selected_local <- ""
    }
    
    updateSelectInput(session, "local",
                      choices = c("Seleccione un local..." = "", locales),
                      selected = selected_local)
    
    # Si se borra el local, también reseteamos productos
    if (selected_local == "") {
      updateSelectInput(session, "producto_id", choices = c("Todos"), selected = "Todos")
    } else {
      # Mantener productos si el local sigue siendo válido
      df_local <- df %>% filter(Local == selected_local)
      opciones <- obtener_opciones_producto(df_local)
      updateSelectInput(session, "producto_id",
                        choices = c("Todos", opciones),
                        selected = isolate(input$producto_id))
    }
  })
  
  # Cuando cambia el local, actualizar Categoría
  observeEvent(input$local, {
    if (!is.null(input$local) && input$local != "") {
      df <- reposicion
      if (input$zona != "Todas") df <- df %>% filter(Zona == input$zona)
      df <- df %>% filter(Local == input$local)
      
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
                        choices = c("Todas", conjuntos),
                        selected = "Todas")
      
      estados <- sort(unique(df$Estado))
      updateSelectInput(session, "estados",
                        choices = c("Todas", estados),
                        selected = "Todas")
      
      formatos <- sort(unique(df$Formato))  
      updateSelectInput(session, "formato",  
                        choices = c("Todos", formatos),
                        selected = "Todos")
      
      updateSelectInput(session, "producto_id", choices = c("Todos"), selected = "Todos")
    } else {
      updateSelectInput(session, "categoria", choices = c("Todas"), selected = "Todas")
      updateSelectInput(session, "subcategoria", choices = c("Todas"), selected = "Todas")
      updateSelectInput(session, "conjunto", choices = c("Todas"), selected = "Todas")
      updateSelectInput(session, "estado", choices = c("Todas"), selected = "Todas")
      updateSelectInput(session, "formato", choices = c("Todos"), selected = "Todos")
      updateSelectInput(session, "producto_id", choices = c("Todos"), selected = "Todos")
    }
  })
  
  observeEvent(c(input$local, input$categoria, input$subcategoria, input$conjunto, input$estado,input$formato), {
    req(input$local)
    
    df <- reposicion
    
    if (input$zona != "Todas") df <- df %>% filter(Zona == input$zona)
    df <- df %>% filter(Local == input$local)
    
    if (!is.null(input$categoria) && input$categoria != "Todas") {
      df <- df %>% filter(Categoria == input$categoria)
    }
    

    subcategorias_actuales <- sort(unique(df$Subcategoria))
    subcategoria_actual <- isolate(input$subcategoria)
    if (!(subcategoria_actual %in% subcategorias_actuales)) {
      subcategoria_actual <- "Todas"
    }
    updateSelectInput(session, "subcategoria",
                      choices = c("Todas", subcategorias_actuales),
                      selected = subcategoria_actual)
    if (!is.null(input$subcategoria) && input$subcategoria != "Todas") {
      df <- df %>% filter(Subcategoria == input$subcategoria)
    }
    
    conjuntos_actuales <- sort(unique(df$Conjunto))
    conjunto_actual <- isolate(input$conjunto)
    if (!(conjunto_actual %in% conjuntos_actuales)) {
      conjunto_actual <- "Todos"
    }
    updateSelectInput(session, "conjunto",
                      choices = c("Todos", conjuntos_actuales),
                      selected = conjunto_actual)
    if (!is.null(input$conjunto) && input$conjunto != "Todos") {
      df <- df %>% filter(Conjunto == input$conjunto)
    }
    
    estados_actuales <- sort(unique(df$Estado))
    estado_actual <- isolate(input$estado)
    if (!(estado_actual %in% estados_actuales)) {
      estado_actual <- "Todos"
    }
    updateSelectInput(session, "estado",
                      choices = c("Todos", estados_actuales),
                      selected = estado_actual)
    if (!is.null(input$estado) && input$estado != "Todos") {
      df <- df %>% filter(Estado == input$estado)
    }
    
    formatos_actuales <- sort(unique(df$Formato))
    formato_actual <- isolate(input$formato)
    if (!(formato_actual %in% formatos_actuales)) {
      formato_actual <- "Todos"
    }
    updateSelectInput(session, "formato",
                      choices = c("Todos", formatos_actuales),
                      selected = formato_actual)
    if (!is.null(input$formato) && input$formato != "Todos") {
      df <- df %>% filter(Formato == input$formato)
    }
    
    productos <- sort(unique(df$Producto))
    
    updateSelectInput(session, "producto_id",
                      choices = c("Todos", productos),
                      selected = "Todos")
  })
  
  
  observeEvent(input$producto_id, {
    if ("Todos" %in% input$producto_id && length(input$producto_id) > 1) {
      updateSelectInput(session, "producto_id", selected = "Todos")
    }
  })
  
  
  observeEvent(input$reset_filtros, {
    session$reload()
  })
  
}

shinyApp(ui, server)
