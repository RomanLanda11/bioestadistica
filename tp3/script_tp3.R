####### CARGAR DATOS ###########
library(readxl)
library(readr)
library(stringr)
library(dplyr)

######################## Tratamiento de defunciones ############################
defunciones <- read_delim("tp3/defweb23.csv", 
                          delim = ";", escape_double = FALSE, trim_ws = TRUE)
causa <- 'I21' # Infarto agudo del miocardio

defunciones <- defunciones %>%
  filter(CAUSA == causa) %>% 
  group_by(PROVRES, GRUPEDAD) %>%
  summarise(
    cuenta_total = sum(CUENTA, na.rm = TRUE)
  ) %>%
  ungroup()

cod_provincias <- read_excel("tp3/descdef1.xlsx", 
                             sheet = "PROVRES")

# Realizar la unión (merge)
defunciones <- defunciones %>%
  left_join(cod_provincias %>% select(CODIGO, VALOR), by = c("PROVRES" = "CODIGO"))

defunciones <- defunciones %>%
  mutate(
    GRUPEDAD = str_replace(GRUPEDAD, "^\\d{2}_", ""),        # 1. Eliminar los 3 primeros caracteres (ej. "16_")
    GRUPEDAD = str_replace_all(GRUPEDAD, "[[:space:]]", ""), # 2. Eliminar *todos* los espacios en blanco
    
    # 3. Reemplazar "a" por "-" SOLAMENTE si no le sigue "y"
    GRUPEDAD = str_replace(GRUPEDAD, "a(?![y])", "-"),
    
    # 4. Finalmente, corregir "80ymás" a "80 y más"
    GRUPEDAD = str_replace(GRUPEDAD, "80ymás", "80 y más")
  )
################################################################################


edades <- read_excel("tp3/c2_proyecciones_prov_2010_2040.xls", 
                      sheet = "01-TOTAL DEL PAÍS", range = "A59:A84")
edades <- edades[!apply(is.na(edades), 1, all), ]
  

# Define la ruta de tu archivo
ruta_archivo <- "tp3/c2_proyecciones_prov_2010_2040.xls"

nombres_hojas <- excel_sheets(ruta_archivo) # Nombres de las hojas del .xlsx
nombres_hojas <- nombres_hojas[nombres_hojas != "GraphData"]

nombres_dfs_generados <- c()

for (hoja in nombres_hojas) {
  # print(hoja)
  # 1. Lee los datos de la hoja actual
  datos_hoja <- read_excel(ruta_archivo,
                           sheet = hoja,
                           range = "F60:H84")
  # Eliminamos filas donde TODAS las columnas son NA
  datos_hoja <- datos_hoja[!apply(is.na(datos_hoja), 1, all), ]
  datos_hoja <- cbind(edades,datos_hoja)
  # 2. Limpia el nombre de la hoja para usarlo como nombre de variable
  
  codigo_provincia <- str_extract(hoja, "^\\d{2}")
  
  # 2. Limpia el nombre de la hoja para usarlo como nombre de variable
  # Extraer los dos primeros dígitos del nombre de la hoja
  codigo_provincia <- str_extract(hoja, "^\\d{2}")
  
  # Agrega la columna 'codigo_prov' al data.frame
  datos_hoja$codigo_prov <- codigo_provincia
  
  nombre_limpio <- tolower(hoja)
  nombre_limpio <- str_replace_all(nombre_limpio, "^\\d{2}-", "")
  nombre_limpio <- str_replace_all(nombre_limpio, "ñ", "n")
  nombre_limpio <- str_replace_all(nombre_limpio, "[áéíóúü]", function(x) {
    chartr("áéíóúü", "aeiouu", x)
  })
  nombre_limpio <- str_replace_all(nombre_limpio, "[[:space:]]", "_")
  nombre_limpio <- str_replace_all(nombre_limpio, "[^a-z0-9_]", "")
  
  
  datos_hoja <- datos_hoja[-1, ] # Elimina totales
  datos_hoja$`%poblacion` <- (
    datos_hoja$`Ambos sexos`/sum(datos_hoja$`Ambos sexos`)
    )*100 # Calcula %
  
  
  grupos_a_colapsar <- c("80-84", "85-89", "90-94", "95-99", "100 y más")
  
  # Filtrar las filas a colapsar
  # Aquí uso la primera columna como ejemplo si no tiene nombre, o puedes usar un nombre específico como "GRUPO DE EDAD"
  nombre_columna_grupo_edad <- "Edad" # Ajusta si tu columna de edad tiene otro nombre
  
  filas_a_colapsar <- datos_hoja %>%
    filter(get(nombre_columna_grupo_edad) %in% grupos_a_colapsar)
  
  # Filtrar las filas que NO se van a colapsar (las que se mantienen igual)
  filas_no_colapsadas <- datos_hoja %>%
    filter(!get(nombre_columna_grupo_edad) %in% grupos_a_colapsar)
  
  # Crear la nueva fila "80 y más" sumando los valores de las columnas numéricas
  fila_colapsada <- filas_a_colapsar %>%
    summarise(
      # La primera columna (grupo de edad) se establece como "80 y más"
      !!nombre_columna_grupo_edad := "80 y más",
      # Sumar las columnas numéricas relevantes
      `Ambos sexos` = sum(`Ambos sexos`, na.rm = TRUE),
      `Varones` = sum(`Varones`, na.rm = TRUE),
      `Mujeres` = sum(`Mujeres`, na.rm = TRUE),
      `%poblacion` = sum(`%poblacion`, na.rm = TRUE) # Sumar porcentajes también
      # Agrega aquí cualquier otra columna numérica que necesites sumar
    )
  datos_hoja <- bind_rows(filas_no_colapsadas, fila_colapsada)
  
  # Merge con defunciones
  datos_hoja <- datos_hoja %>%
    left_join(defunciones,
              by = c("codigo_prov" = "PROVRES", "Edad" = "GRUPEDAD"))
  # TODO: resolver 80 y más
  # 3. Asigna el data frame al entorno global con el nombre limpio
  assign(nombre_limpio, datos_hoja, envir = .GlobalEnv)
  
  # Opcional: Mensaje para ver qué data frame se ha creado
  cat("Creado data frame:", nombre_limpio, "\n")
  nombres_dfs_generados <- c(nombres_dfs_generados, nombre_limpio)
}

####################### Casos #####################################


