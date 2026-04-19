
##########################################################
##################### DATA INPUT #########################
##########################################################
cat("Inicio actualización bases de datos Matrix Femaco")

library(readr)
library(dplyr)
library(fs)
library(lubridate)
library(readxl)
library(writexl)
library(tidyr)
library(stringr)
library(ISOweek)
library(bizdays)
library(purrr)

#Usuario

# drive <- "C:/Users/Felipe/Desktop/Modulos/"
# drive <- "C:/Users/MAX/Desktop/Femaco/"
# drive <- "C:/Users/vamon/Documents/Femaco/"
drive <- "C:/Dropbox/Matrix/"

# Bases de referencia
locales   <- readRDS(paste0(drive,"Tablas/locales.rds"))
productos <- readRDS(paste0(drive,"Tablas/productos.rds"))
precios   <- readRDS(paste0(drive,"Tablas/precios.rds"))

##########################################################################
############################### SELL OUT #################################
##########################################################################

ruta_respaldo_sellout <- paste0(drive, "01_Sell_out/respaldo/base_historica_sellout_backup.rds")
carpeta_csv_sellout <- paste0(drive, "Bases_datos/actualizacion_ventas_diarias")
archivos_nuevos_sellout <- dir_ls(carpeta_csv_sellout, regexp = "\\.csv$")

if (length(archivos_nuevos_sellout) == 0) {
  cat("⚠️ No se encontraron archivos nuevos para procesar (Sell Out).\n")
} else if (length(archivos_nuevos_sellout) > 1) {
  stop("❌ Error: Hay más de un archivo CSV en la carpeta de ventas semanales. Solo debe haber uno.")
} else {
  Sell_out_hist <- readRDS(paste0(drive, "01_Sell_out/Sellout_app/base_historica_sellout.rds"))
  
  archivo_sellout <- archivos_nuevos_sellout[1]
  
  # Leer el archivo encontrado como Sell_out_curr
  Sell_out_curr <- read_csv(
    archivo_sellout, 
    col_types = cols(
      `Venta Neta` = col_character(),
      `Item ID` = col_character(),
      Date = col_date(format = "%Y-%m-%d")
    )
  )

  Sell_out_curr <- Sell_out_curr %>%
    mutate(`Venta Neta` = str_trim(`Venta Neta`),                            # limpia espacios al inicio y fin
           `Venta Neta` = str_remove(`Venta Neta`, ",.*"),                   # elimina coma y todo lo posterior
           `Venta Neta` = str_remove_all(`Venta Neta`, fixed(".")),          # elimina puntos
           `Venta Neta` = as.integer(`Venta Neta`))
  
  fecha_max_his_sellout <- as.Date(max(Sell_out_hist$Fecha))
  
  # Establecer idioma español en Windows para fechas
  Sys.setlocale("LC_TIME", "Spanish")
  
  Sell_out_curr <- Sell_out_curr %>%
    rename(
      SKU = `Item ID`,
      Local_ID = `ID Local`,
      Fecha = Date,
      Vendedor_ID = `Vendor ID`,
      Venta_Neta = `Venta Neta`
    ) %>%
    mutate(
      Local_ID = sprintf("%03d", as.integer(Local_ID)),
      Dia = day(Fecha),
      Mes = tolower(format(Fecha, "%B")),  # nombre del mes en minúsculas
      Año = year(Fecha)
    ) %>%
    left_join(productos %>% select(SKU, Codigo_Femaco, Nombre_Producto, Categoria, Subcategoria, Formato), by = "SKU") %>%
    left_join(locales %>% select(Local_ID, Zona), by = "Local_ID") %>%
    select(Fecha, Dia, Mes, Año, Vendedor_ID, Departamento, Familia, SubFamilia, Grupo, Conjunto, 
           Categoria, Subcategoria, Formato, SKU, Nombre_Producto, Codigo_Femaco, Local_ID, Local, Zona, 
           Canal, Cantidad, Venta_Neta, Marca) %>%
    arrange(desc(Fecha))

  
  base_respaldo_sellout <- Sell_out_hist 
  
  tryCatch({
    saveRDS(base_respaldo_sellout, ruta_respaldo_sellout)
    cat("🗂️  Respaldo actualizado correctamente en:", ruta_respaldo_sellout, "\n")
  }, error = function(e) {
    cat("❌ Error al crear el respaldo:", conditionMessage(e), "\n")
  })
  
  Sell_out_hist <- Sell_out_hist %>%
    select(-Estado)
  
  Sell_out <- bind_rows(
    Sell_out_curr %>% mutate(origen = 0),
    Sell_out_hist %>% mutate(origen = 1)
  ) %>%
    arrange(Fecha, SKU, Local_ID, Canal, origen) %>%
    distinct(Fecha, SKU, Local_ID, Canal, .keep_all = TRUE) %>%
    select(-origen) %>%
    arrange(desc(Fecha))
  
  Sell_out <- Sell_out %>%
    left_join(productos %>% select(SKU, Estado) %>% distinct(), by = "SKU")
  
  # Mover archivos procesados
  carpeta_procesados_sellout <- paste0(drive, "Bases_datos/actualizacion_ventas_diarias/procesados")
  file_move(archivos_nuevos_sellout, carpeta_procesados_sellout)
  
  # Guardar base actualizada
  saveRDS(Sell_out, paste0(drive, "01_Sell_out/Sellout_app/base_historica_sellout.rds"))
  saveRDS(Sell_out, paste0(drive, "07_Datos/Datos_app/base_historica_sellout.rds"))
  
  filas_agregadas_sellout <- nrow(Sell_out) - nrow(base_respaldo_sellout)
  cat("➕ Se agregaron", filas_agregadas_sellout, "filas nuevas a la base histórica de Sell Out.\n")
  
}

cat("Fin actualización base histórica Sell Out\n")

###################################################################################
############################### SELL OUT SEMANAL #################################
###################################################################################

carpeta_csv_semanal <- paste0(drive, "Bases_datos/actualizacion_ventas_semanales_y_stock")
archivo_nuevo_semanal <- dir_ls(carpeta_csv_semanal, regexp = "\\.csv$")

if (length(archivo_nuevo_semanal) == 0) {
  cat("⚠️ No se encontraron archivos nuevos para procesar (Sell Out Semanal ni Stock).\n")
} else if (length(archivo_nuevo_semanal) > 1) {
  stop("❌ Error: Hay más de un archivo CSV en la carpeta de ventas semanales. Solo debe haber uno.")
} else {
  archivo_semana <- archivo_nuevo_semanal[1]
  sellout_semanal <- read_csv(archivo_semana, show_col_types = FALSE,
                               col_types = cols(
                                 `Venta Sactual` = col_character(),
                                 `Venta Neta S1` = col_character(),
                                 `Venta Neta S2` = col_character(),
                                 `Venta Neta S3` = col_character(),
                                 `Venta NetaS4`  = col_character(),
                                 SKU             = col_character()
                               ))

  sellout_semanal <- sellout_semanal %>%
    mutate(Local_ID = sprintf("%03d", as.integer(`ID Local`))) %>%
    select(-`...1`, -`ID Local`, -Conjunto)
  
  fecha_carga <- file.info(archivo_semana)$mtime %>% as.Date()
 
  sellout_semanal <- sellout_semanal %>%
    mutate(across(
      c(`Venta Sactual`, `Venta Neta S1`, `Venta Neta S2`, `Venta Neta S3`, `Venta NetaS4`, "Stock Fisico"),
      ~ .x %>%
        str_trim() %>%                          # eliminar espacios
        str_remove_all(fixed(".")) %>%          # eliminar puntos (separador de miles)
        str_replace(",", ".") %>%               # reemplazar coma decimal por punto
        as.numeric() %>%                        # convertir a número
        round()                                 # redondear al entero más cercano
    ))
  
  sellout_semanal <- sellout_semanal %>%
    mutate(
      semana_actual = isoweek(fecha_carga),
      
      # Rango semana actual (sin guardar semana_actual_codigo como columna)
      rango_semana_actual = paste(
        ISOweek2date(paste0(ISOweek(fecha_carga), "-1")),
        "al",
        ISOweek2date(paste0(ISOweek(fecha_carga), "-7"))
      ),
      
      # Rangos semanas anteriores (sin guardar fechas ni códigos intermedios)
      rango_semana_1 = paste(
        ISOweek2date(paste0(ISOweek(fecha_carga - weeks(1)), "-1")),
        "al",
        ISOweek2date(paste0(ISOweek(fecha_carga - weeks(1)), "-7"))
      ),
      rango_semana_2 = paste(
        ISOweek2date(paste0(ISOweek(fecha_carga - weeks(2)), "-1")),
        "al",
        ISOweek2date(paste0(ISOweek(fecha_carga - weeks(2)), "-7"))
      ),
      rango_semana_3 = paste(
        ISOweek2date(paste0(ISOweek(fecha_carga - weeks(3)), "-1")),
        "al",
        ISOweek2date(paste0(ISOweek(fecha_carga - weeks(3)), "-7"))
      ),
      rango_semana_4 = paste(
        ISOweek2date(paste0(ISOweek(fecha_carga - weeks(4)), "-1")),
        "al",
        ISOweek2date(paste0(ISOweek(fecha_carga - weeks(4)), "-7"))
      ),
      
      # Otras columnas útiles
      `Fecha Carga` = fecha_carga,
      
      `Total Ventas Semanas Cerradas` = rowSums(select(., `Venta Neta S1`, `Venta Neta S2`, `Venta Neta S3`, `Venta NetaS4`), na.rm = TRUE),
      `Total Unidades Semanas Cerradas` = rowSums(select(., `Unidades S1`, `Unidades S2`, `Unidades S3`, `Unidades S4`), na.rm = TRUE)
    ) 
  
  
  # Agregar Categoria, Subcategoria, Formato, Codigo_Femaco, Nombre_Producto y Conjunto desde tabla_productos
  sellout_semanal <- sellout_semanal %>%
    left_join(productos %>%
                select(SKU, Categoria, Subcategoria, Formato, Codigo_Femaco, Nombre_Producto, Conjunto, Estado),
              by = "SKU")
  
  # Agregar ZONA desde tabla_sucursales usando LOCAL DESTINO
  sellout_semanal <- sellout_semanal %>%
    left_join(locales %>%
                select(Local, Zona),
              by = "Local")
  
  # Cambiar nombre columnas 
  sellout_semanal <- sellout_semanal %>%
    rename(
      `Producto` = `Item Name`,
      `Marca` = `Brand Name`,
      `Ventas Semana 1` = `Venta Neta S1`,
      `Ventas Semana 2` = `Venta Neta S2`,
      `Ventas Semana 3` = `Venta Neta S3`,
      `Ventas Semana 4` = `Venta NetaS4`,
      `Ventas Semana Actual` = `Venta Sactual`,
      `Unidades Semana 1` = `Unidades S1`,          
      `Unidades Semana 2` = `Unidades S2`,      
      `Unidades Semana 3` = `Unidades S3`,         
      `Unidades Semana 4` = `Unidades S4`,
      `Unidades Semana Actual` = `Unidades SActual`,
      `Stock Físico` = `Stock Fisico`
    ) %>%
    select(all_of(c(
      "Fecha Carga", "semana_actual", "rango_semana_actual",
      "rango_semana_1", "rango_semana_2", "rango_semana_3", "rango_semana_4",
      "SKU", "Producto", "Local_ID", "Local", "Marca", "Vigencia", "Departamento",
      "Familia", "SubFamilia", "Grupo", "Conjunto", "Unidades X Pallet",
      "Estado Descontinuado", "Stock Disponible", "TRF_POR_RECIBIR", "OC_X_RECIBIR",
      "TRF ENVIADO", "Stock Físico", "Ventas Semana Actual", "Ventas Semana 1",
      "Ventas Semana 2", "Ventas Semana 3", "Ventas Semana 4",
      "Total Ventas Semanas Cerradas", "Unidades Semana Actual", "Unidades Semana 1",
      "Unidades Semana 2", "Unidades Semana 3", "Unidades Semana 4",
      "Total Unidades Semanas Cerradas", "Reservado", "Categoria", "Subcategoria", "Formato",
      "Codigo_Femaco", "Zona", "Nombre_Producto", "Estado"
    )))
  
  carpeta_procesados_semanales <- paste0(drive, "Bases_datos/actualizacion_ventas_semanales_y_stock/procesados")
  file_move(archivo_nuevo_semanal, carpeta_procesados_semanales)
  
  # Guardar el objeto como .rds
  saveRDS(sellout_semanal, 
          file = paste0(drive, "03_Ventas_semanales/Ventas_semanales_app/sellout_semanal.rds"))
  saveRDS(sellout_semanal, 
          file = paste0(drive, "01_Sell_out/Sellout_app/sellout_semanal.rds"))
  saveRDS(sellout_semanal, paste0(drive, "07_Datos/Datos_app/sellout_semanal.rds"))
}

cat("Fin actualización base Sell Out semanal\n")

##########################################################################
############################### STOCK ####################################
##########################################################################

# 1. Rutas relevantes
ruta_stock <- paste0(drive, "Stock")
ruta_base_historica <- file.path(ruta_stock, "base_historica_stock.rds")
ruta_respaldo_stock <- file.path(ruta_stock, "respaldo", "base_historica_stock_backup.rds")

# 2. Leer base histórica si existe
Stock_hist <- readRDS(ruta_base_historica)

# 3. # Si sellout_semanal no existe, abrir desde RDS
if (!exists("sellout_semanal")) {
  sellout_semanal <- readRDS(paste0(drive, "03_Ventas_semanales/Ventas_semanales_app/sellout_semanal.rds"))
}

# 4. Base "nuevas filas"
Stock_curr<- sellout_semanal %>%
  select("Fecha Carga", SKU, Producto,  Marca, Vigencia, Departamento, Familia, SubFamilia,
         Conjunto, Local, Local_ID, "Unidades X Pallet", "Estado Descontinuado", "Stock Disponible", 
         "TRF_POR_RECIBIR", "OC_X_RECIBIR", "TRF ENVIADO", "Stock Físico", Formato, Zona, 
         Categoria, Codigo_Femaco
          ) %>%
  mutate(Local_ID = sprintf("%03d", as.integer(Local_ID)))

# 5. Guardar respaldo (único archivo, sobreescrito)

base_respaldo_stock <- Stock_hist

tryCatch({
  saveRDS(base_respaldo_stock, ruta_respaldo_stock)
  cat("🗂️  Respaldo de stock guardado en:", ruta_respaldo_stock, "\n")
}, error = function(e) {
  cat("❌ Error al guardar el respaldo de stock:", conditionMessage(e), "\n")
})

# 6. Agregar Precio (desde tabla precios) y calcular Valor_Stock

# Stock historico, esto debería poder borrarse en un tiempo

#Stock_hist <- Stock_hist %>%
 #{ if ("Valor_Stock" %in% names(.)) select(., -Valor_Stock) else . } %>%
  #left_join(
    #precios %>% select(SKU, Precio),
    #by = "SKU"
  #) %>%
  #mutate(Valor_Stock = `Stock Físico` * Precio)

# Stock a ingresar en la base
Stock_curr <- Stock_curr %>%
  # agrega Precio desde precios (Stock_curr no lo trae, se incorpora aquí)
  left_join(precios %>% select(SKU, Precio), by = "SKU") %>%
  # calcula Valor_Stock SOLO para el bloque nuevo
  mutate(Valor_Stock = `Stock Físico` * Precio)

# 7. Actualizar base histórica
# Stock <- bind_rows(Stock_hist, Stock_curr) %>%
#   distinct(SKU, Local_ID, `Fecha Carga`, .keep_all = TRUE) %>%
#   distinct()

Stock <- bind_rows(
  Stock_curr %>% mutate(origen = 0),
  Stock_hist %>% mutate(origen = 1)
) %>%
  arrange(SKU, Local_ID, `Fecha Carga`, origen) %>%
  distinct(SKU, Local_ID, `Fecha Carga`, .keep_all = TRUE) %>%
  select(-origen)

# 8. Actualizar Estado
Stock <- Stock %>%
  # Si existe, la quita (si no existe, no pasa nada y continúa)
  { if ("Estado" %in% names(.)) select(., -Estado) else . } %>%
  # Luego agrega/actualiza Estado desde productos
  left_join(
    productos %>% select(SKU, Estado),
    by = "SKU"
  )

# 9. Guardar base actualizada
saveRDS(Stock, ruta_base_historica)
saveRDS(Stock, paste0(drive, "07_Datos/Datos_app/base_historica_stock.rds"))
filas_agregadas_stock <- nrow(Stock) - nrow(base_respaldo_stock)
cat("➕ Se agregaron", filas_agregadas_stock, "filas nuevas a la base histórica de stock.\n")

cat("Fin actualización base histórica Stock\n")

##########################################################################
############################### SELL IN ##################################
##########################################################################

ruta_respaldo_sellin <- paste0(drive, "02_Sell_in/respaldo/base_historica_sellin_backup.rds")
carpeta_csv_oc <- paste0(drive, "Bases_datos/actualizacion_oc")
archivos_nuevos_oc <- dir_ls(carpeta_csv_oc, regexp = "\\.csv$")

# 1) Definir calendario 2025-2026 + enero 2027: fines de semana sábados y domingos, y feriados

# --- Feriados Chile 2025-2026 ---

feriados_2025_2026 <- as.Date(c(
  #2025
  "2025-01-01", # Año Nuevo 
  "2025-04-18", # Viernes Santo 
  "2025-04-19", # Sábado Santo 
  "2025-05-01", # Día del Trabajo 
  "2025-05-21", # Glorias Navales 
  "2025-06-20", # Pueblos Indígenas
  "2025-06-29", # San Pedro y San Pablo 
  "2025-07-16", # Virgen del Carmen 
  "2025-08-15", # Asunción de la Virgen 
  "2025-09-18", # Independencia 
  "2025-09-19", # Glorias del Ejército 
  "2025-10-12", # Encuentro de Dos Mundos
  "2025-10-31", # Iglesias Evangélicas 
  "2025-11-01", # Todos los Santos 
  "2025-12-08", # Inmaculada Concepción
  "2025-12-25", # Navidad 
  #2026
  "2026-01-01", # Año Nuevo 
  "2026-04-03", # Viernes Santo 
  "2026-04-04", # Sábado Santo 
  "2026-05-01", # Día del Trabajo 
  "2026-05-21", # Glorias Navales 
  "2026-06-21", # Pueblos Indígenas
  "2026-06-29", # San Pedro y San Pablo 
  "2026-07-16", # Virgen del Carmen 
  "2026-08-15", # Asunción de la Virgen 
  "2026-09-18", # Independencia 
  "2026-09-19", # Glorias del Ejército 
  "2026-10-12", # Encuentro de Dos Mundos
  "2026-10-31", # Iglesias Evangélicas 
  "2026-11-01", # Todos los Santos 
  "2026-12-08", # Inmaculada Concepción
  "2026-12-25", # Navidad 
  "2027-01-01" # Año Nuevo 
))

# --- Crear el calendario laboral ---

try(bizdays::remove.calendar("cal_chile_2025_2026"), silent = TRUE)
bizdays::create.calendar(
  name        = "cal_chile_2025_2026",
  weekdays    = c("saturday", "sunday"),
  holidays    = feriados_2025_2026,
  start.date  = as.Date("2025-01-01"),
  end.date    = as.Date("2027-01-31"),
  adjust.from = "next",
  adjust.to   = "previous"
)

# Definir el calendario por defecto
bizdays.options$set(default.calendar = "cal_chile_2025_2026")

if (length(archivos_nuevos_oc) == 0) {
  cat("⚠️ No se encontraron archivos nuevos para procesar (Sell In).\n")
} else {
  # Leer y procesar
  Sell_in_hist  <- readRDS(paste0(drive, "02_Sell_in/Sellin_app/base_historica_sellin.rds"))
  
  #fecha_emision_max_his_sellin <- as.Date(max(Sell_in_hist$Fecha_emision_OC))
  
  Sell_in_curr <- archivos_nuevos_oc %>%
    map(~ read_csv(.x, show_col_types = FALSE)) %>%
    map(~ mutate(.x, across(any_of(c("SKU","Local_ID","Codigo_Femaco")), as.character))) %>%
    bind_rows() %>%
    mutate(
      OC = `Número OC`,
      Fecha_carga = Sys.Date(),
      Fecha_emision_OC = dmy(`Fecha de emisión`),
      Fecha_cierre_estimada = add.bizdays(Fecha_emision_OC, 3),
      Mes = tolower(format(Fecha_cierre_estimada, "%B")),
      Año = year(Fecha_cierre_estimada),
      #Nombre_Producto = Modelo,
      #Codigo_Femaco = VIN,
     `%Concordancia` = NA_real_,
      Costo = NA_real_,
      Neto_OC = NA_real_,
      Monto_Neto = NA_real_,
      Local_ID = `Id de local`,
      Unidades_Informadas = `Unidades compradas`,
      Unidades_Confirmadas = `Unidades recibidas`
    ) %>%
    mutate(Unidades_Confirmadas = case_when(
      `Estado OC` != "Recepción total" ~ Unidades_Informadas,
      TRUE ~ Unidades_Confirmadas)) %>%
    mutate(Local_ID = sprintf("%03d", as.integer(trimws(Local_ID)))) %>%
    left_join(
      productos %>% select(SKU, Codigo_Femaco, Nombre_Producto,
                           Categoria, Subcategoria, Formato, Departamento, Familia,
                           SubFamilia, Grupo, Conjunto),
      by = "SKU"
    ) %>%
    mutate(Nombre_Producto = coalesce(Nombre_Producto, Modelo)) %>% 
    left_join(
      locales %>% select(Local, Zona),
      by = "Local"
    ) %>%
    left_join(
      precios %>% select(SKU, Precio),
      by = "SKU"
    ) %>%
    mutate(
      `%Concordancia` = if_else(Unidades_Informadas > 0,
                                round(Unidades_Confirmadas / Unidades_Informadas * 100, 0),
                                NA_real_),
      Costo = coalesce(Costo, Precio),
      Neto_OC = round(Costo * Unidades_Informadas, 0),
      Monto_Neto = round(Costo * Unidades_Confirmadas, 0)
    ) %>%
    # left_join(
    #   sellout_semanal %>%
    #     select(SKU, Local, `Stock Físico`),
    #   by = c("SKU", "Local")
    # ) %>%
    # mutate(
    #   Stock = coalesce(`Stock Físico`, Stock)
    # ) %>%
    select(
      OC, Fecha_carga, Fecha_emision_OC, Fecha_cierre_estimada, Mes, Año,
      Departamento, Familia, SubFamilia, Grupo, Conjunto,
      Categoria, Subcategoria, Formato,
      SKU, Nombre_Producto, Codigo_Femaco, 
      Local_ID, Local, Zona,
      Unidades_Informadas, Unidades_Confirmadas, `%Concordancia`,
      Costo, Neto_OC, Monto_Neto, `Estado OC`) #%>%
    #filter(Fecha_emision_OC >= fecha_emision_max_his_sellin) %>%
    #arrange(desc(Fecha_emision_OC))
    
    base_respaldo_sellin <- Sell_in_hist 
  
    tryCatch({
    saveRDS(base_respaldo_sellin , ruta_respaldo_sellin)
    cat("🗂️  Respaldo actualizado correctamente en:", ruta_respaldo_sellin, "\n")
     }, error = function(e) {
    cat("❌ Error al crear el respaldo:", conditionMessage(e), "\n")
     })
  
  Sell_in_hist <- Sell_in_hist %>%
      select(-Estado) 
    
  prioridad <- c("Cursado" = 1L, "Recepción parcial" = 2L, "Recepción total" = 3L)
  
  Sell_in <- bind_rows(Sell_in_hist, Sell_in_curr) %>%
    mutate(.pri = prioridad[`Estado OC`] %>% tidyr::replace_na(0L)) %>%
    group_by(OC) %>%
    # 1) estado prioritario
    filter(.pri == max(.pri, na.rm = TRUE)) %>%
    # 2) si empata el estado, última carga
    filter(Fecha_carga == max(Fecha_carga, na.rm = TRUE)) %>%
    ungroup() %>%
    select(-.pri) %>%
    arrange(desc(Fecha_emision_OC)) %>% 
    distinct(across(-Nombre_Producto), .keep_all = TRUE)  # Elimina filas duplicadas
  
  Sell_in <- Sell_in %>%
    left_join(productos %>% select(SKU, Estado) %>% distinct(),
              by = "SKU")
      
    carpeta_procesados_oc <- paste0(drive, "Bases_datos/actualizacion_oc/procesados")
    file_move(archivos_nuevos_oc, carpeta_procesados_oc)
  
    saveRDS(Sell_in, paste0(drive, "02_Sell_in/Sellin_app/base_historica_sellin.rds"))
    saveRDS(Sell_in, paste0(drive, "07_Datos/Datos_app/base_historica_sellin.rds"))
    
    filas_agregadas_sellin <- nrow(Sell_in) - nrow(base_respaldo_sellin)
    cat("➕ Se agregaron", filas_agregadas_sellin, "filas nuevas a la base histórica de Sell In.\n")
  }

cat("Fin actualización base histórica Sell In\n")

##########################################################################
############################### REPOSICIÓN ###############################
##########################################################################

# Variables de fecha actual
anio_actual <- year(Sys.Date())
mes_actual <- month(Sys.Date())

# Determinar trimestre actual
trimestre_actual <- case_when(
  mes_actual %in% 1:3 ~ "T1",
  mes_actual %in% 4:6 ~ "T2",
  mes_actual %in% 7:9 ~ "T3",
  mes_actual %in% 10:12 ~ "T4",
  TRUE ~ NA_character_
)

# Vector de meses a número
meses_nombres <- c(
  "enero" = 1, "febrero" = 2, "marzo" = 3,
  "abril" = 4, "mayo" = 5, "junio" = 6,
  "julio" = 7, "agosto" = 8, "septiembre" = 9,
  "octubre" = 10, "noviembre" = 11, "diciembre" = 12
)

Sell_out_aux <- Sell_out %>%
  mutate(
    Mes = tolower(Mes),
    Mes_num = meses_nombres[Mes],
    Fecha_Mensual = make_date(Año, Mes_num, 1)
  )

top50 <- Sell_out %>%
  filter(Fecha >= fecha_max_his_sellout %m-% months(3)) %>% 
  group_by(SKU) %>%
  summarise(
    venta_3m = sum(Venta_Neta, na.rm = TRUE),
    .groups = "drop") %>% 
  arrange(desc(venta_3m)) %>%
  slice_head(n = 50) %>%
  mutate(producto_critico = 1L) %>%
  select(SKU, producto_critico)

# Fecha del primer día del mes actual
fecha_actual_mes <- floor_date(Sys.Date(), "month")

# Fecha del mes anterior
fecha_mes_anterior <- fecha_actual_mes %m-% months(1)

# Fecha del mismo mes del año anterior
fecha_mismo_mes_anio_anterior <- fecha_actual_mes %m-% years(1)

ventas_mensuales <- Sell_out_aux %>%
  group_by(SKU, Local_ID, Fecha_Mensual) %>%
  summarise(ventas_mes = sum(Cantidad, na.rm = TRUE), .groups = "drop")

ventas_mensuales_nacional <- Sell_out_aux %>%
  group_by(SKU, Fecha_Mensual) %>%
  summarise(
    ventas_mes = sum(Cantidad, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(Local_ID = "000") %>%
  relocate(Local_ID, .after = SKU)

ventas_mensuales_total <- bind_rows(
  ventas_mensuales,
  ventas_mensuales_nacional
)

ventas_adicionales <- ventas_mensuales_total %>%
  mutate(
    Año = year(Fecha_Mensual),
    Mes_num = month(Fecha_Mensual),
    tipo = case_when(
      Fecha_Mensual == fecha_mes_anterior ~ "ventas_mes_anterior",
      Fecha_Mensual == fecha_actual_mes ~ "venta_mes_actual",
      Fecha_Mensual == fecha_mismo_mes_anio_anterior ~ "ventas_mismo_mes_año_anterior",
      TRUE ~ NA_character_
    )
  ) 

ventas_max_hist <- ventas_adicionales %>%
  mutate(Fecha_Mensual = as.Date(Fecha_Mensual)) %>%
  group_by(SKU, Local_ID) %>%
  slice_max(order_by = ventas_mes, n = 1, with_ties = TRUE) %>% 
  slice_max(order_by = Fecha_Mensual, n = 1) %>%                 
  ungroup() %>%
  mutate(Fecha_max_hist = toupper(format(Fecha_Mensual, "%b-%y")),
         Fecha_max_hist = gsub("\\.", "", Fecha_max_hist)) %>% 
  select(SKU, Local_ID, Fecha_max_hist, ventas_mes) %>% 
  rename(ventas_max_hist = ventas_mes)

ventas_max_mes <- ventas_adicionales %>%
  filter(Mes_num == mes_actual) %>% 
  mutate(Fecha_Mensual = as.Date(Fecha_Mensual)) %>%
  group_by(SKU, Local_ID) %>%
  slice_max(order_by = ventas_mes, n = 1, with_ties = TRUE) %>% 
  slice_max(order_by = Fecha_Mensual, n = 1) %>%                 
  ungroup() %>%
  mutate(Fecha_max_mes = toupper(format(Fecha_Mensual, "%b-%y")),
         Fecha_max_mes = gsub("\\.", "", Fecha_max_mes)) %>% 
  select(SKU, Local_ID, Fecha_max_mes, ventas_mes) %>% 
  rename(ventas_max_mes = ventas_mes)


ventas_adicionales <- ventas_adicionales %>%
  filter(!is.na(tipo) | Mes_num == mes_actual) %>%  # importante: conservar mes actual para calcular máximo
  group_by(SKU, Local_ID) %>%
  summarise(
    venta_mes_actual = sum(ventas_mes[tipo == "venta_mes_actual"], na.rm = TRUE),
    ventas_mes_anterior = sum(ventas_mes[tipo == "ventas_mes_anterior"], na.rm = TRUE),
    ventas_mismo_mes_año_anterior = sum(ventas_mes[tipo == "ventas_mismo_mes_año_anterior"], na.rm = TRUE),
    ventas_maximo_mes = if (any(Mes_num == mes_actual)) {
      max(ventas_mes[Mes_num == mes_actual], na.rm = TRUE)
    } else {
      NA_real_
    },
    .groups = "drop"
  )

# Si Sell_in no existe, abrir desde RDS
if (!exists("Sell_in")) {
  Sell_in <- readRDS(paste0(drive, "02_Sell_in/Sellin_app/base_historica_sellin.rds"))
}

# 1) Resumen por local
despacho_base_local <- Sell_in %>% 
  filter(`Estado OC` %in% c("Recepción total")) %>%
  group_by(SKU, Local_ID, Mes, Año) %>% 
  summarise(despacho_mes = sum(Unidades_Confirmadas), .groups = "drop") 

# 2) Resumen nacional (Local_ID = "000")
despacho_base_nacional <- Sell_in %>% 
  filter(`Estado OC` %in% c("Recepción total")) %>%
  group_by(SKU, Mes, Año) %>% 
  summarise(despacho_mes = sum(Unidades_Confirmadas, na.rm = TRUE), .groups = "drop") %>%
  mutate(Local_ID = "000") %>%
  relocate(Local_ID, .after = SKU)

# 3) Unir (local + nacional) y aplicar lógica de máximos
despacho_max_hist <- bind_rows(despacho_base_local, despacho_base_nacional) %>%
  mutate(
    Mes_num = meses_nombres[tolower(Mes)],
    Fecha_aux = as.Date(paste(Año, Mes_num, "01", sep = "-"))
  ) %>%
  group_by(SKU, Local_ID) %>%
  slice_max(order_by = despacho_mes, n = 1, with_ties = TRUE) %>%
  slice_max(order_by = Fecha_aux, n = 1) %>%                 
  ungroup() %>%
  mutate(
    Fecha_despacho_max_hist = paste0(toupper(substr(Mes, 1, 3)), "-", substr(Año, 3, 4)),
    despacho_max_hist = despacho_mes
  ) %>% 
  select(SKU, Local_ID, Fecha_despacho_max_hist, despacho_max_hist)

# === 1. Promedios por SKU–Local ===
promedios_trimestre_actual <- Sell_out %>%
  mutate(
    Mes = tolower(Mes),
    Mes_num = meses_nombres[Mes],
    Trimestre = case_when(
      Mes_num %in% 1:3 ~ "T1",
      Mes_num %in% 4:6 ~ "T2",
      Mes_num %in% 7:9 ~ "T3",
      Mes_num %in% 10:12 ~ "T4"
    )
  ) %>%
  filter(!(Año == anio_actual & Trimestre == trimestre_actual)) %>%
  group_by(SKU, Local_ID, Local, Año, Trimestre) %>%
  summarise(venta_trimestral = sum(Cantidad, na.rm = TRUE), .groups = "drop") %>%
  filter(Trimestre == trimestre_actual) %>%
  group_by(SKU, Local_ID, Local, Trimestre) %>%
  summarise(promedio_venta_trimestral = round(mean(venta_trimestral), 0), .groups = "drop") %>%
  rename(Trimestre_Actual = Trimestre) %>%
  mutate(
    promedio_mensual_trimestral = round(promedio_venta_trimestral / 3, 0)
  )

# === 2. Promedio nacional por SKU ===
promedios_sku_nacional <- Sell_out %>%
  mutate(
    Mes = tolower(Mes),
    Mes_num = meses_nombres[Mes],
    Trimestre = case_when(
      Mes_num %in% 1:3 ~ "T1",
      Mes_num %in% 4:6 ~ "T2",
      Mes_num %in% 7:9 ~ "T3",
      Mes_num %in% 10:12 ~ "T4"
    )
  ) %>%
  filter(!(Año == anio_actual & Trimestre == trimestre_actual),
         Trimestre == trimestre_actual) %>%
  group_by(SKU) %>%
  summarise(
    promedio_venta_trimestral_nacional = round(mean(Cantidad, na.rm = TRUE), 0),
    promedio_mensual_trimestral_nacional = round(mean(Cantidad, na.rm = TRUE)/3, 0),
    .groups = "drop"
  )

# === 3. Preparar base reposición ===

# 1) Base por local
reposicion_local <- sellout_semanal %>%
  select("Fecha Carga", semana_actual, rango_semana_actual, rango_semana_1, rango_semana_2, rango_semana_3, rango_semana_4, SKU, Nombre_Producto, Codigo_Femaco, Vigencia, Departamento, Familia, SubFamilia, Grupo,
         Conjunto, Categoria, Subcategoria, Formato, Estado, Local, Local_ID, Zona, 
         "Ventas Semana Actual", "Ventas Semana 1", "Ventas Semana 2", "Ventas Semana 3", "Ventas Semana 4",
         "Total Ventas Semanas Cerradas", "Unidades Semana Actual", "Unidades Semana 1", "Unidades Semana 2", 
         "Unidades Semana 3", "Unidades Semana 4", "Total Unidades Semanas Cerradas", 
         "Unidades X Pallet", "Stock Disponible", TRF_POR_RECIBIR, OC_X_RECIBIR, 
         "TRF ENVIADO", "Stock Físico") %>%
  mutate(
    rango_semanas_1_a_4 = paste(
      str_sub(rango_semana_4, 1, 10),
      "al",
      str_sub(rango_semana_1, -10)
    ),
    ventas_totales_mensuales = rowSums(select(., 
                                              `Ventas Semana 1`, `Ventas Semana 2`, `Ventas Semana 3`, `Ventas Semana 4`), na.rm = TRUE),
    unidades_totales_mensuales = rowSums(select(., 
                                                `Unidades Semana 1`, `Unidades Semana 2`, `Unidades Semana 3`, `Unidades Semana 4`), na.rm = TRUE)
  )

# 2) Versión nacional (Local_ID = "000")
reposicion_nacional <- reposicion_local %>%
  group_by(SKU) %>%
  summarise(
    `Fecha Carga` = max(`Fecha Carga`, na.rm = TRUE),
    semana_actual = first(semana_actual),
    rango_semana_actual = first(rango_semana_actual),
    rango_semana_1 = first(rango_semana_1),
    rango_semana_2 = first(rango_semana_2),
    rango_semana_3 = first(rango_semana_3),
    rango_semana_4 = first(rango_semana_4),
    
    Nombre_Producto = first(Nombre_Producto),
    Codigo_Femaco = first(Codigo_Femaco),
    Vigencia = first(Vigencia),
    Departamento = first(Departamento),
    Familia = first(Familia),
    SubFamilia = first(SubFamilia),
    Grupo = first(Grupo),
    Conjunto = first(Conjunto),
    Categoria = first(Categoria),
    Subcategoria = first(Subcategoria),
    Formato = first(Formato),
    Estado  = first(Estado),
    Local = "Nacional",
    Local_ID = "000",
    Zona = "Nacional",
    
    `Ventas Semana Actual` = sum(`Ventas Semana Actual`, na.rm = TRUE),
    `Ventas Semana 1` = sum(`Ventas Semana 1`, na.rm = TRUE),
    `Ventas Semana 2` = sum(`Ventas Semana 2`, na.rm = TRUE),
    `Ventas Semana 3` = sum(`Ventas Semana 3`, na.rm = TRUE),
    `Ventas Semana 4` = sum(`Ventas Semana 4`, na.rm = TRUE),
    `Total Ventas Semanas Cerradas` = sum(`Total Ventas Semanas Cerradas`, na.rm = TRUE),
    
    `Unidades Semana Actual` = sum(`Unidades Semana Actual`, na.rm = TRUE),
    `Unidades Semana 1` = sum(`Unidades Semana 1`, na.rm = TRUE),
    `Unidades Semana 2` = sum(`Unidades Semana 2`, na.rm = TRUE),
    `Unidades Semana 3` = sum(`Unidades Semana 3`, na.rm = TRUE),
    `Unidades Semana 4` = sum(`Unidades Semana 4`, na.rm = TRUE),
    `Total Unidades Semanas Cerradas` = sum(`Total Unidades Semanas Cerradas`, na.rm = TRUE),
    
    `Unidades X Pallet` = first(`Unidades X Pallet`),   # normalmente NO se suma
    `Stock Disponible` = sum(`Stock Disponible`, na.rm = TRUE),
    TRF_POR_RECIBIR = sum(TRF_POR_RECIBIR, na.rm = TRUE),
    OC_X_RECIBIR = sum(OC_X_RECIBIR, na.rm = TRUE),
    `TRF ENVIADO` = sum(`TRF ENVIADO`, na.rm = TRUE),
    `Stock Físico` = sum(`Stock Físico`, na.rm = TRUE),
    
    rango_semanas_1_a_4 = first(rango_semanas_1_a_4),
    ventas_totales_mensuales = sum(ventas_totales_mensuales, na.rm = TRUE),
    unidades_totales_mensuales = sum(unidades_totales_mensuales, na.rm = TRUE),
    
    .groups = "drop"
  )

# 3) Unir local + nacional
reposicion <- bind_rows(reposicion_local, reposicion_nacional)

# === 4. Unir promedios históricos locales y nacionales ===
reposicion <- reposicion %>%
  left_join(promedios_trimestre_actual, by = c("SKU", "Local_ID", "Local")) %>%
  left_join(promedios_sku_nacional, by = c("SKU")) %>%
  mutate(
    Trimestre_Actual = replace_na(Trimestre_Actual, trimestre_actual),
    promedio_venta_trimestral = coalesce(promedio_venta_trimestral, promedio_venta_trimestral_nacional),
    promedio_mensual_trimestral = coalesce(promedio_mensual_trimestral, promedio_mensual_trimestral_nacional),
    fuente_promedio = case_when(
      is.na(promedio_venta_trimestral) ~ "Sin historial",
      !is.na(promedio_mensual_trimestral_nacional) & is.na(promedio_venta_trimestral) == FALSE ~ "Historial nacional",
      TRUE ~ "Historial local"
    )
  ) %>%
  select(-promedio_venta_trimestral_nacional, -promedio_mensual_trimestral_nacional) %>%
  left_join(
    locales %>% select(Local, Local_ID, dias_lead_time, Rutas),
    by = c("Local", "Local_ID"))%>%
  mutate(
    Movimientos_Pendientes = TRF_POR_RECIBIR + OC_X_RECIBIR
  ) 

# === 5. Unir estadísticas de ventas mensuales ===

reposicion <- reposicion %>%
  left_join(ventas_adicionales %>% distinct(SKU, Local_ID, .keep_all = TRUE), by = c("SKU", "Local_ID"))

# === 6. Agregar Sugerencias de Reposición ===

reposicion <- reposicion %>%
  left_join(despacho_max_hist, by = c("SKU", "Local_ID")) %>% 
  left_join(ventas_max_hist, by = c("SKU", "Local_ID")) %>% 
  left_join(ventas_max_mes, by = c("SKU", "Local_ID"))

reposicion <- reposicion %>%
  mutate(
    Sugerencia_Unidades = ceiling(ventas_max_hist - (`Stock Físico` + Movimientos_Pendientes)),
    Sugerencia_Empaques = ceiling(Sugerencia_Unidades / `Unidades X Pallet`)
  ) 

# === 7. Agregar Gancheras y productos críticos

reposicion <- reposicion %>% 
  left_join(productos %>%
              select(SKU, Gancheras), by = "SKU") %>% 
  left_join(top50, by = "SKU") %>% 
  mutate(
    promedio_venta_ult_4_semanas = rowMeans(across(c(`Ventas Semana 1`, `Ventas Semana 2`, `Ventas Semana 3`, `Ventas Semana 4`)),na.rm = TRUE)) %>% 
  group_by(Local) %>% 
  mutate(venta_local = sum(venta_mes_actual, na.rm = TRUE)) %>% 
  ungroup() %>% 
  mutate(
    por_part_tienda = if_else(
      is.na(venta_mes_actual) | is.na(venta_local) | venta_local == 0,
      NA_character_,
      paste0(round(100 * venta_mes_actual / venta_local, 2), "%")))

# === 8. Quitar tienda nacional para app Objetivos y Datos

reposicion_sin_nacional <- reposicion %>%
  filter(Local_ID != "000")

# === 9. Guardar bases actualizadas

saveRDS(reposicion, paste0(drive, "04_Reposicion/Reposicion_app/reposicion.rds"))
saveRDS(reposicion_sin_nacional, paste0(drive, "07_Datos/Datos_app/reposicion.rds"))

cat("Fin actualización base Reposición\n")

##########################################################################
################################ OBJETIVOS ###############################
##########################################################################

# === 1. Guardar base actualizada


saveRDS(reposicion_sin_nacional, paste0(drive, "06_Objetivos/Objetivos_app/objetivos.rds"))

cat("Fin actualización base Objetivos\n")

##########################################################################
########################### VENTAS MENSUALES #############################
##########################################################################

# 1) Resumir Sell_out (ya traía las columnas descriptivas)
Sell_out_hist_mensual <- Sell_out %>%
  group_by(Mes, Año, SKU, Codigo_Femaco, Nombre_Producto, Local_ID, Local) %>%
  summarise(
    Cantidad     = sum(Cantidad,   na.rm = TRUE),
    Venta_Neta   = sum(Venta_Neta, na.rm = TRUE),
    SubFamilia   = first(SubFamilia),
    Grupo        = first(Grupo),
    Conjunto     = first(Conjunto),
    Categoria    = first(Categoria),
    Subcategoria = first(Subcategoria),
    Formato      = first(Formato),
    Estado         = first(Estado),
    Zona         = first(Zona),
    .groups = "drop"
  )

# 2) Resumir Sell_in **incluyendo también las descripciones**
Sell_in_hist_mensual <- Sell_in %>%
  group_by(Mes, Año, SKU, Codigo_Femaco, Nombre_Producto, Local_ID, Local) %>%
  summarise(
    Unidades_Confirmadas = sum(Unidades_Confirmadas, na.rm = TRUE),
    Monto_Neto           = sum(Monto_Neto,           na.rm = TRUE),
    SubFamilia   = first(SubFamilia),
    Grupo        = first(Grupo),
    Conjunto     = first(Conjunto),
    Categoria    = first(Categoria),
    Subcategoria = first(Subcategoria),
    Formato      = first(Formato),
    Estado         = first(Estado),
    Zona         = first(Zona),
    .groups = "drop"
  )

# 3) Unir, unificar columnas descriptivas y dejar el orden pedido
keys <- c("Año","Mes","SKU","Local_ID","Codigo_Femaco")  # evita textos en las keys

Ventas_mensuales <- Sell_out_hist_mensual %>%
  full_join(Sell_in_hist_mensual, by = keys, suffix = c(".out", ".in")) %>%
  mutate(
    # Métricas numéricas
    Cantidad             = replace_na(Cantidad, 0),
    Venta_Neta           = replace_na(Venta_Neta, 0),
    Unidades_Confirmadas = replace_na(Unidades_Confirmadas, 0),
    Monto_Neto           = replace_na(Monto_Neto, 0),
    
    # Unificar descripciones (prefiere Sell_out; si falta, toma Sell_in)
    SubFamilia   = coalesce(SubFamilia.out,   SubFamilia.in),
    Grupo        = coalesce(Grupo.out,        Grupo.in),
    Conjunto     = coalesce(Conjunto.out,     Conjunto.in),
    Categoria    = coalesce(Categoria.out,    Categoria.in),
    Subcategoria = coalesce(Subcategoria.out, Subcategoria.in),
    Formato      = coalesce(Formato.out,      Formato.in),
    Estado      = coalesce(Estado.out,      Estado.in),
    Zona         = coalesce(Zona.out,         Zona.in),
    # Estos también pueden diferir, por eso los coalesce:
    Nombre_Producto = coalesce(Nombre_Producto.out, Nombre_Producto.in),
    Local           = coalesce(Local.out,           Local.in)
  ) %>%
  select(
    Mes, Año, SubFamilia, Grupo, Conjunto, Categoria, Subcategoria, Formato, Estado,
    SKU, Local_ID, Codigo_Femaco, Nombre_Producto, Local, Zona,
    Cantidad, Venta_Neta, Unidades_Confirmadas, Monto_Neto
  ) %>%
  mutate(Fecha_carga = format(Sys.Date(), "%d/%m/%Y"))

# Guardar Ventas_mensuales como .rds en la carpeta deseada
saveRDS(Ventas_mensuales,
        paste0(drive, "05_Ventas_mensuales/Ventas_mensuales_app/Ventas_mensuales.rds"))
saveRDS(Ventas_mensuales, paste0(drive, "07_Datos/Datos_app/Ventas_mensuales.rds"))

# Guardar copias de bases de referencia
saveRDS(locales, paste0(drive, "07_Datos/Datos_app/locales.rds"))
saveRDS(productos, paste0(drive, "07_Datos/Datos_app/productos.rds"))
saveRDS(precios, paste0(drive, "07_Datos/Datos_app/precios.rds"))

cat("Fin actualización base Ventas Mensuales\n")

##########################################################################
############################### MONITOREO ################################
##########################################################################

monitoreo <- Ventas_mensuales %>% 
  left_join(Sell_out %>%
              group_by(Año, Mes, SKU, Local_ID) %>%
              summarise(
                ventas_mes = sum(Venta_Neta, na.rm = TRUE),
                cantidades_mes = sum(Cantidad, na.rm = TRUE),
                .groups = "drop"
              ) %>%
              group_by(Mes, SKU, Local_ID) %>%
              summarise(
                ventas_max_mes = max(ventas_mes, na.rm = TRUE),
                anio_max_ventas = Año[which.max(ventas_mes)],
                cantidades_max_mes = max(cantidades_mes, na.rm = TRUE),
                anio_max_cantidades = Año[which.max(cantidades_mes)],
                .groups = "drop"),
            by = c("Mes", "SKU", "Local_ID")) %>% 
  left_join(Stock %>% 
              mutate(Fecha_stock = `Fecha Carga`,
                     Año = as.numeric(format(Fecha_stock, "%Y")),
                     Mes = names(meses_nombres)[as.numeric(format(Fecha_stock, "%m"))]) %>%
              group_by(Año, Mes, SKU, Local_ID) %>%
              slice_max(order_by = `Stock Físico`, n = 1, with_ties = FALSE) %>%
              ungroup() %>%
              transmute(
                Año, Mes, SKU, Local_ID,
                Stock_max_mes = `Stock Físico`,
                Valor_Stock,
                Fecha_stock_max = Fecha_stock),
            by = c("Año", "Mes", "SKU", "Local_ID"))

saveRDS(monitoreo, paste0(drive, "08_Monitoreo/Monitoreo_app/monitoreo.rds"))

cat("Fin actualización base Monitoreo\n")
cat("Fin actualización de todas las bases de datos de la Matrix\n")