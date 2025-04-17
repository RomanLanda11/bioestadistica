library(readxl)
library(lubridate)
library(tidyverse)
library(writexl)

dd <- read_excel("tp1/datos/dd.xlsx")
adm <- read_excel("tp1/datos/adm.xlsx") %>%
  mutate(
    # Extraer y convertir fechas con formato dd/mm/yyyy
    fechaid = dmy(sub("-.*", "", patientid)),  # se queda con fecha
    inicialesid = sub(".*-", "", patientid), # se queda con iniciales
    interview = dmy(interview),  # Convertir interview a dmy
    
    # Convertir columnas numéricas
    across(c(countrycode, ethnicgroup, scr, usscr, consent), 
           ~ as.integer(.)),
  )

# Reglas de validación
reglas <- tribble(
  ~id, ~desc, ~cond,
  "r1", "(patientid) es faltante", "!is.na(patientid)",
  "r2", "(countrycode) es faltante", "!is.na(countrycode)",
  "r3", "(countrycode) es entero", "is.integer(countrycode)",
  "r4", "(countrycode) fuera de rango", "countrycode %in% c(4,11,14,23,31,48,54,65,72,97)",
  "r5", "(fechaid) es faltante", "!is.na(fechaid)",  
  "r6", "(fechaid) es fecha", "is.Date(fechaid)",
  "r7", "(inicialesid) es faltante", "!is.na(inicialesid)",
  "r8", "(inicialesid) es caracter", "is.character(inicialesid)", 
  "r9", "(inicialesid) es mayuscula", "grepl('^[A-Z]{2}$', inicialesid)",
  "r10", "(interview) es faltante", "!is.na(interview)",  
  "r11", "(interview) es fecha","is.Date(interview)",
  "r12", "(ethnicgroup) es faltante", "!is.na(ethnicgroup)",
  "r13", "(ethnicgroup) es entero", "is.integer(ethnicgroup)",
  "r14", "(ethnicgroup) fuera de rango", "ethnicgroup %in% c(1:4)",
  "r15", "(scr) es faltante", "!is.na(scr)",
  "r16", "(scr) fuera de rango", "scr %in% c(1,2)",
  "r17", "(usscr) es faltante", "!is.na(usscr)",
  "r18", "(usscr) dentro de rango", "usscr %in% c(1,2)",
  "r19", "(consent) es faltante", "!is.na(consent)",
  "r20", "(consent) dentro de rango", "consent %in% c(1,2)",
  "r21", "(subjectnumber) es faltante", "!is.na(subjectnumber)",
  "r22", "(subjectnumber) obligatorio si pasa screening", "ifelse(scr == 2 & usscr == 2 & consent == 2, !is.na(subjectnumber), TRUE)",
  "r23", "(subjectnumber) coincide con cc", "as.integer(substr(subjectnumber,1,3)) == countrycode",
  "r24", "(subjectnumber) no debería estar si scr=1", "ifelse(scr == 1, is.na(subjectnumber), TRUE)",
  "r25", "(subjectnumber) no debería estar si usscr=1", "ifelse(usscr == 1, is.na(subjectnumber), TRUE)",
  "r26", "(subjectnumber) no debería estar si consent=1", "ifelse(consent == 1, is.na(subjectnumber), TRUE)",
  "r27", "paciente mayor de edad", "!is.na(fechaid) & !is.na(interview) & (interval(fechaid, interview) / years(1)) >= 18"
)

write_xlsx(reglas, "tp1/report/reglas_validacion.xlsx")

# Función validador
validador <- function(datos, id, cond) {
  reglas <- datos[[cond]]
  names(reglas) <- datos[[id]]
  reglas
}

# Función validar
validar <- function(datos, id, validador) {
  resultado <- sapply(
    validador,
    function(x) {
      tryCatch(
        eval(parse(text = x), datos),
        error = function(e) NA
      )
    }
  )
  as_tibble(resultado) %>%
    mutate(registro = datos[[id]], .before = 1)
}

# Aplicar validación
validacion <- validar(adm, "patientid", validador(reglas, "id", "cond"))


#################### ANALISIS DE RESULTADOS ###############################
# Convertir a formato largo para análisis
validacion_largo <- validacion %>%
  pivot_longer(-registro, names_to = "regla", values_to = "error")

errores_por_registro <- validacion_largo %>%
  filter(!error) %>%  # Filtramos los casos donde la condición NO se cumplió (error)
  group_by(registro) %>%
  summarise(
    n_errores = n(),
    reglas_fallidas = str_c(regla, collapse = ", "),
    .groups = 'drop'
  ) %>%
  left_join(
    reglas %>% select(id, desc),
    by = c("reglas_fallidas" = "id")
  )

errores_por_regla <- validacion_largo %>%
  filter(!error) %>%  # Filtramos los FALSE
  count(regla, name = "errores") %>%
  left_join(reglas, by = c("regla" = "id"))

detalles_errores <- validacion_largo %>%
  filter(!error) %>%  # Filtramos los FALSE
  left_join(reglas, by = c("regla" = "id")) %>%
  left_join(adm, by = c("registro" = "patientid"))

reglas <- reglas %>%
  mutate(
    campo = case_when(
      str_detect(cond, "patientid") ~ "patientid",
      str_detect(cond, "countrycode") ~ "countrycode",
      str_detect(cond, "fechaid") ~ "fechaid",
      str_detect(cond, "inicialesid") ~ "inicialesid",
      str_detect(cond, "interview") ~ "interview",
      str_detect(cond, "ethnicgroup") ~ "ethnicgroup",
      str_detect(cond, "scr") ~ "scr",
      str_detect(cond, "usscr") ~ "usscr",
      str_detect(cond, "consent") ~ "consent",
      str_detect(cond, "subjectnumber") ~ "subjectnumber",
      TRUE ~ NA_character_
    )
  )

errores_por_registro <- validacion_largo %>%
  filter(!error) %>%
  left_join(reglas, by = c("regla" = "id")) %>%
  group_by(registro) %>%
  summarise(
    n_errores = n(),
    reglas_fallidas = str_c(regla, collapse = ", "),
    campos_con_error = str_c(unique(campo), collapse = ", "),
    .groups = 'drop'
  )

campo_mas_frecuente <- errores_por_registro %>%
  separate_rows(campos_con_error, sep = ",\\s*") %>%  # separa por coma + espacio opcional
  count(campos_con_error, sort = TRUE)  # cuenta ocurrencias y ordena

campo_mas_frecuente
