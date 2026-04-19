
##########################################################
##################### PORTAL #############################
##########################################################

library(shiny)
library(shinymanager)
library(shinyjs)
library(digest)
library(stringr)

# ===================== CONFIG =====================
RUTA_SQLITE  <- "credenciales.sqlite"

# Secreto para token (idealmente por variable de entorno)
TOKEN_SECRET <- Sys.getenv("TOKEN_SECRET", unset = "clave_ultra_segura_2024")

# Passphrase usada al crear el SQLite (create_db)
SM_PASSPHRASE <- Sys.getenv("SM_PASSPHRASE", unset = "cambia_esta_frase_super_secreta")

# URLs de cada módulo (ajusta a las rutas correspondientes)
MOD_URL <- c(
 Modulo_1 = "https://femaco.shinyapps.io/Sellout_app/",
 Modulo_2 = "https://femaco.shinyapps.io/Sellin_app/",
 Modulo_3 = "https://femaco.shinyapps.io/Ventas_semanales_app/",
 Modulo_4 = "https://femaco.shinyapps.io/Reposicion_app/",
 Modulo_5 = "https://femaco.shinyapps.io/Ventas_mensuales_app/",
 Modulo_6 = "https://femaco.shinyapps.io/Objetivos_app/",
 Modulo_7 = "https://femaco.shinyapps.io/Datos_app/",
 Modulo_8 = "https://femaco.shinyapps.io/Monitoreo_app/"
)

# URLs de cada módulo (ajusta a tus rutas reales)
# MOD_URL <- c(
#  Modulo_1 = "https://cargomir.shinyapps.io/Sellout_app/",
#  Modulo_2 = "https://cargomir.shinyapps.io/Sellin_app/",
#  Modulo_3 = "https://k51lft-carlos-gonz0lez.shinyapps.io/Ventas_semanales_app/",
#  Modulo_4 = "https://cargomir.shinyapps.io/Reposicion_app/",
#  Modulo_5 = "https://cargomir.shinyapps.io/Ventas_mensuales_app/",
#  Modulo_6 = "https://k51lft-carlos-gonz0lez.shinyapps.io/Movimiento_tiendas_app/",
#  Modulo_7 = "https://k51lft-carlos-gonz0lez.shinyapps.io/09_csv_femaco/",
#  Modulo_8 = "https://k51lft-carlos-gonz0lez.shinyapps.io/Monitoreo_app/"
#)

MOD_LABELS <- c(
  Modulo_1 = "Sell Out",
  Modulo_2 = "Sell In",
  Modulo_3 = "Ventas semanales",
  Modulo_4 = "Reposición",
  Modulo_5 = "Ventas mensuales",
  Modulo_6 = "Objetivos",
  Modulo_7 = "Bases de datos",
  Modulo_8 = "Monitoreo"
)

# (Opcional) Falla temprano si falta el SQLite
if (!file.exists(RUTA_SQLITE)) {
  stop("No se encontró '", RUTA_SQLITE, "'. Súbelo junto a app.R o ajusta RUTA_SQLITE.")
}

# Función para crear token firmado
generar_token <- function(user) {
  timestamp <- as.character(Sys.time())
  datos <- paste(user, timestamp, sep = "|")
  firma <- hmac(key = TOKEN_SECRET, object = datos, algo = "sha256")
  URLencode(paste(datos, firma, sep = "|"))
}

# ===================== UI =========================
set_labels(
  language = "es",
  "Please authenticate" = HTML('
    <div style="text-align:center;">
      <img src="logo.jpg" height="100px"><br>
      <p style="margin-top:10px; font-size:20px; color:#444;">Por favor inicia sesión</p>
    </div>
  '),
  "Username:" = "Usuario:",
  "Password:" = "Contraseña:",
  "Login" = "Ingresar"
)

ui <- secure_app(
  fluidPage(
    useShinyjs(),
    
    # ---- Estilos ----
    tags$head(
      tags$title("Femaco · Portal"), 
      tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
      tags$link(
        rel = "stylesheet",
        href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;600;700&display=swap"
      ),
      tags$style(HTML("
        :root { --brand: #7ab943; }
        html, body { height: 100%; }
        body, body * {
          font-family:'Inter',system-ui,-apple-system,'Segoe UI',Roboto,'Helvetica Neue',Arial,sans-serif !important;
        }

        /* Contenedor a pantalla completa y centrado vertical/horizontal */
        .portal-viewport {
          min-height: 100vh;
          display: flex;
          align-items: center;      /* centra vertical */
          justify-content: center;  /* centra horizontal */
          padding: 24px 0;
        }

        /* Caja interior */
        .portal-wrap     { width: 100%; max-width: 1000px; padding: 0 20px; text-align: center; }
        .portal-header   { display:flex; flex-direction:column; align-items:center; gap:40px; margin-bottom: 20px; }
        .portal-logo     { max-height: 150px; height:auto; width:auto; }
        .portal-subtitle { font-size: 24px; color:#444; text-align:center; margin-top: 6px; font-weight: 700;}

        .portal-actions  { display:flex; flex-wrap:wrap; gap:12px; justify-content:center; }
        
        .saludo-fijo {
          position: fixed;
          top: 50px; 
          left: 50%;
          transform: translateX(-50%);
          font-size: 14px;
          font-weight: 600;
          z-index: 9999;
          text-align: center;
          width: 100%;
          pointer-events: none;
          color: #4d4d4d; /* Gris */
        }

        .btn-brand {
          background-color: var(--brand) !important;
          border-color: var(--brand) !important;
          color: #fff !important;
          border-radius: 999px !important;
          padding: 9px 16px !important;
          box-shadow: 0 4px 12px rgba(122,185,67,.25);
          transition: transform .12s ease, box-shadow .12s ease, filter .12s ease;
        }
        .btn-brand:hover { transform: translateY(-1px); box-shadow: 0 8px 22px rgba(122,185,67,.35); filter: brightness(0.96); }
        .btn-brand:focus { box-shadow: 0 0 0 3px rgba(122,185,67,.35) !important; }

        .btn-disabled {
          background: #cfd8dc !important; border-color:#cfd8dc !important; color:#fff !important;
          border-radius:999px !important; opacity:.7; pointer-events:none; cursor:not-allowed !important;
        }
      "))
    ),
    
    # Saludo flotante arriba
    div(class = "saludo-fijo", uiOutput("saludo")),
    
    # Contenido principal centrado
    div(class = "portal-viewport",
        div(class = "portal-wrap",
            div(class = "portal-header",
                tags$img(src = "logo.jpg", class = "portal-logo"),
                div(class = "portal-subtitle", "Selecciona un módulo para continuar")
            ),
            div(class = "portal-actions", uiOutput("botones"))
        )
    )
    ),
  
  # ---- Estilos y placeholders del login ----
  head_auth = tags$head(
    tags$title("Femaco · Portal"), 
    tags$link(rel = "icon", type = "image/x-icon", href = "favicon.ico"),
    tags$style(HTML("
    .panel-auth, .panel-auth * {
      font-family:'Inter',system-ui,-apple-system,'Segoe UI',Roboto,'Helvetica Neue',Arial,sans-serif !important;
    }
    .auth-content, .auth-container, .content-auth {
      min-height:100vh; display:flex; align-items:center; justify-content:center;
    }
    .panel-auth {
      border: 2px solid #000 !important;
      background-color: #fff !important;
      color: #000 !important;
      border-radius: 8px;
    }
    .panel-auth .btn-primary {
      background-color: #7ab943 !important;
      border-color: #7ab943 !important;
      color: #fff !important;
    }
    .panel-auth input::placeholder { color:#9aa0a6; opacity:1; }
  ")),
    tags$script(HTML("
    function setUserPlaceholder(){
      var $panel = $('.panel-auth');
      if(!$panel.length) return false;

      // Buscar label 'Usuario:' y asociar input
      var $labelUsuario = $panel.find('label[for]').filter(function(){
        return $(this).text().trim().toLowerCase().startsWith('usuario');
      }).first();

      var $u = $();
      if($labelUsuario.length){
        var forId = $labelUsuario.attr('for');
        if(forId){ $u = $panel.find('#'+forId); }
      }
      if(!$u.length){
        $u = $panel.find('input[type=\"text\"]:visible, input[type=\"email\"]:visible').first();
      }

      if($u.length){
        if(!$u.val()){ $u.val(''); } // limpiar si viene autocompletado
        $u.attr('placeholder','Ej: 12345678-9 (sin puntos, con guión)');
        $u.attr('autocomplete','username');
        $u.attr('inputmode','numeric');
        return true;
      }
      return false;
    }

    function applyPlaceholders(){
      return setUserPlaceholder(); // solo usuario
    }

    var observer = new MutationObserver(function(mutations){
      mutations.forEach(function(m){
        if($(m.addedNodes).find('input').length || $(m.target).find('input').length){
          applyPlaceholders();
        }
      });
    });

    $(document).on('shiny:connected', function(){
      applyPlaceholders();
      observer.observe(document.body, {childList:true, subtree:true});
      var tries=0, h=setInterval(function(){
        if(applyPlaceholders() || (++tries>60)) clearInterval(h);
      }, 150);
    });
  "))
  ),
  
  tags_bottom = tags$div(
    style = "text-align:center; font-size: 13px; color: gray;",
    tags$p("¿Problemas para ingresar?"),
    tags$a(href = "mailto:felipe.murillo1@femaco.cl", "Contáctanos")
  ),
  language = "es"
)

# ===================== SERVER =====================
server <- function(input, output, session) {
  # Autenticación contra SQLite (contraseñas hasheadas + columnas extra)
  res_auth <- secure_server(
    check_credentials = check_credentials(
      db = RUTA_SQLITE,
      passphrase = SM_PASSPHRASE
    )
  )
  # Saludo dinámico
  output$saludo <- renderUI({
    # Extrae el primer nombre
    primer_nombre <- strsplit(res_auth$Nombre, "\\s+")[[1]][1]
    
    # Devuelve HTML con colores diferenciados
    HTML(paste0(
      '<span style="color:#4d4d4d;">Hola </span>',
      '<span style="color:#7ab943;">', primer_nombre, '</span>',
      '<span style="color:#4d4d4d;">, te damos la bienvenida al sistema de datos corporativo de Femaco</span>'
    ))
  })
  
  output$botones <- renderUI({
    auth_vals <- reactiveValuesToList(res_auth)
    mods <- grep("^Modulo_", names(auth_vals), value = TRUE)
    visibles <- mods[as.logical(unlist(auth_vals[mods]))]
    
    if (!length(visibles)) return(tags$p("No tienes módulos asignados."))
    
    # ORDEN PERSONALIZADO
    orden_modulos <- c(
      "Modulo_1",
      "Modulo_2",
      "Modulo_3",
      "Modulo_4",
      "Modulo_5",
      "Modulo_6",
      "Modulo_8",
      "Modulo_7"
    )
    
    visibles <- visibles[match(orden_modulos, visibles, nomatch = 0)]
    
    tagList(lapply(visibles, function(m) {
      
      label <- if (!is.null(MOD_LABELS) && !is.null(MOD_LABELS[[m]])) {
        MOD_LABELS[[m]]
      } else {
        gsub("_", " ", m)
      }
      
      actionButton(
        inputId = paste0("btn_", m),
        label   = label,
        class   = "btn-brand"
      )
    }))
  })
  
  # Observers dinámicos para abrir cada módulo con token
  observe({
    auth_vals <- reactiveValuesToList(res_auth)
    mods <- grep("^Modulo_", names(auth_vals), value = TRUE)
    
    lapply(mods, function(m) {
      observeEvent(input[[paste0("btn_", m)]], {
        # fuerza a lógico también aquí
        if (isTRUE(as.logical(auth_vals[[m]]))) {
          token <- generar_token(res_auth$user)
          url <- MOD_URL[[m]]
          if (!is.null(url) && nzchar(url)) {
            runjs(sprintf("window.open('%s?token=%s','_blank');", url, token))
          } else {
            showNotification(paste("No hay URL configurada para", m), type = "error")
          }
        } else {
          showNotification("No tienes acceso a este módulo.", type = "error")
        }
      }, ignoreInit = TRUE)
    })
  })
}

shinyApp(ui, server)