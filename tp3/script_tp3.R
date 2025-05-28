# Función de limpieza común para los grupos de edad
clean_age_group <- function(age_string) {
  age_string <- str_replace(age_string, "^\\d{2}_", "") # Eliminar el prefijo "XX_"
  age_string <- str_trim(age_string) # Eliminar espacios
  age_string <- str_replace_all(age_string, "\\s+", " ")# Reemplazar múltiples espacios internos con un solo espacio
  # Usar un "negative lookahead" para asegurar que "a" no esté seguido de "y" (para preservar "y más")
  age_string <- str_replace(age_string, " a (?![y])", "-")
  age_string <- str_replace(age_string, "ymás", " y más")# Corregir casos como "80ymás" o "100ymás" para que tengan el espacio correcto
  return(age_string)
}

####### CARGAR LIBRERÍAS Y DATOS ###########
library(readxl)
library(readr)
library(stringr)
library(dplyr)

######################## Tratamiento de defunciones ############################
defunciones <- read_delim("tp3/defweb23.csv",
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
causa <- 'I21' # Causa de defunción: Infarto agudo del miocardio

# Aplicar la función de limpieza a GRUPEDAD en 'defunciones' antes de agrupar
defunciones <- defunciones %>%
  mutate(GRUPEDAD = clean_age_group(GRUPEDAD))

# Filtrar por causa, agrupar por provincia y grupo de edad, y sumar 'CUENTA'
defunciones <- defunciones %>%
  filter(CAUSA == causa) %>%
  group_by(PROVRES, GRUPEDAD) %>%
  summarise(
    cuenta_total = sum(CUENTA, na.rm = TRUE)
  ) %>%
  ungroup()

# Cargar el archivo de códigos de provincia
cod_provincias <- read_excel("tp3/descdef1.xlsx",
                             sheet = "PROVRES")

# Realizar la unión (merge) de 'defunciones' con los nombres de provincia
defunciones <- defunciones %>%
  left_join(cod_provincias %>% select(CODIGO, VALOR), by = c("PROVRES" = "CODIGO"))

################################################################################


# Cargar y limpiar el dataframe 'edades'
edades <- read_excel("tp3/c2_proyecciones_prov_2010_2040.xls",
                     sheet = "01-TOTAL DEL PAÍS", range = "A59:A84")
edades <- edades[!apply(is.na(edades), 1, all), ]

# Aplicar la función de limpieza a la columna 'Edad' en 'edades'
edades <- edades %>%
  mutate(Edad = clean_age_group(Edad))


# Define la ruta de tu archivo principal (proyecciones provinciales)
ruta_archivo <- "tp3/c2_proyecciones_prov_2010_2040.xls"

# Obtener nombres de las hojas del archivo Excel y filtrar "GraphData"
nombres_hojas <- excel_sheets(ruta_archivo)
nombres_hojas <- nombres_hojas[nombres_hojas != "GraphData"]

# Inicializar un vector para almacenar los nombres de los data frames generados
nombres_dfs_generados <- c()

# Bucle para procesar cada hoja del archivo de proyecciones
for (hoja in nombres_hojas) {
  # print(hoja) # Descomentar para depuración
  
  # 1. Leer los datos de la hoja actual
  datos_hoja <- read_excel(ruta_archivo,
                           sheet = hoja,
                           range = "F60:H84")
  
  # Eliminar filas donde TODAS las columnas son NA
  datos_hoja <- datos_hoja[!apply(is.na(datos_hoja), 1, all), ]
  
  # Verificar y unir 'edades' al data frame actual
  if (nrow(edades) != nrow(datos_hoja)) {
    warning(paste("El número de filas en 'edades' (", nrow(edades), ") no coincide con 'datos_hoja' (", nrow(datos_hoja), ") para la hoja:", hoja, ". Saltando esta hoja."))
    next # Saltar a la siguiente iteración del bucle
  }
  datos_hoja <- cbind(Edad = edades$Edad, datos_hoja) # Asegurar que la columna se llame 'Edad'
  
  # Extraer los dos primeros dígitos del nombre de la hoja para 'codigo_prov'
  codigo_provincia <- str_extract(hoja, "^\\d{2}")
  
  # Agregar la columna 'codigo_prov' al data.frame (como carácter)
  datos_hoja$codigo_prov <- codigo_provincia
  
  # Limpiar el nombre de la hoja para usarlo como nombre de variable en R
  nombre_limpio <- tolower(hoja)
  nombre_limpio <- str_replace_all(nombre_limpio, "^\\d{2}-", "") # Eliminar prefijo numérico
  nombre_limpio <- str_replace_all(nombre_limpio, "ñ", "n") # Reemplazar 'ñ'
  nombre_limpio <- str_replace_all(nombre_limpio, "[áéíóúü]", function(x) { # Quitar tildes
    chartr("áéíóúü", "aeiouu", x)
  })
  nombre_limpio <- str_replace_all(nombre_limpio, "[[:space:]]", "_") # Reemplazar espacios por guiones bajos
  nombre_limpio <- str_replace_all(nombre_limpio, "[^a-z0-9_]", "") # Eliminar caracteres no alfanuméricos
  
  # Eliminar la primera fila (asumiendo que es una fila de totales)
  datos_hoja <- datos_hoja[-1, ]
  
  # Convertir columnas numéricas a tipo numérico para cálculos
  cols_numericas <- c("Ambos sexos", "Varones", "Mujeres")
  for (col in cols_numericas) {
    if (col %in% colnames(datos_hoja)) {
      datos_hoja[[col]] <- as.numeric(datos_hoja[[col]])
    }
  }
  
  # Calcular la columna '%poblacion'
  datos_hoja$`%poblacion` <- (
    datos_hoja$`Ambos sexos` / sum(datos_hoja$`Ambos sexos`, na.rm = TRUE)
  ) * 100
  
  # Lógica para colapsar grupos de edad "80 y más"
  grupos_a_colapsar <- c("80-84", "85-89", "90-94", "95-99", "100 y más")
  nombre_columna_grupo_edad <- "Edad"
  
  filas_a_colapsar <- datos_hoja %>%
    filter(get(nombre_columna_grupo_edad) %in% grupos_a_colapsar)
  
  filas_no_colapsadas <- datos_hoja %>%
    filter(!get(nombre_columna_grupo_edad) %in% grupos_a_colapsar)
  
  if (nrow(filas_a_colapsar) > 0) {
    fila_colapsada <- filas_a_colapsar %>%
      summarise(
        !!nombre_columna_grupo_edad := "80 y más",
        `Ambos sexos` = sum(`Ambos sexos`, na.rm = TRUE),
        `Varones` = sum(`Varones`, na.rm = TRUE),
        `Mujeres` = sum(`Mujeres`, na.rm = TRUE),
        `%poblacion` = sum(`%poblacion`, na.rm = TRUE),
        codigo_prov = first(codigo_prov) # Tomar el código de provincia
      )
    datos_hoja <- bind_rows(filas_no_colapsadas, fila_colapsada)
  }
  
  # Realizar el merge con 'defunciones'
  datos_hoja <- datos_hoja %>%
    left_join(defunciones,
              by = c("codigo_prov" = "PROVRES", "Edad" = "GRUPEDAD"))
  
  # Asignar el data frame procesado al entorno global con el nombre limpio
  assign(nombre_limpio, datos_hoja, envir = .GlobalEnv)
  
  # Mensaje de confirmación y almacenamiento del nombre
  cat("Creado data frame:", nombre_limpio, "\n")
  nombres_dfs_generados <- c(nombres_dfs_generados, nombre_limpio)
}

######################### Estandarizacion de Tasas ###########################