library(readxl)
library(lubridate)
library(tidyverse)

dd <- read_excel("tp1/dd.xlsx")
adm <- read_excel("tp1/adm.xlsx") %>%
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
reglas = tribble(
  ~id, ~desc, ~cond,
  "r1", "(cc) es faltante", "!is.na(countrycode)",
  "r2", "(cc) es entero", "is.integer(countrycode)",
  "r3", "(cc) fuera de rango", "countrycode %in% c(4,11,14,23,31,48,54,65,72,97)",
  "r4", "(fid) es faltante", "!is.na(fechaid)",  
  "r5", "(fid) es fecha", "is.Date(fechaid)",
  "r6", "(iid) es faltante", "!is.na(inicialesid)",
  "r7", "(iid) es caracter", "is.character(inicialesid)", 
  "r8", "(iid) es mayuscula", "grepl('^[A-Z]{2}$', inicialesid)",
  "r9", "(interview) es faltante", "!is.na(interview)",  
  "r10", "(interview) es fecha","is.Date(interview)",
  "r11", "(eg) es faltante", "!is.na(ethnicgroup)",
  "r12", "(eg) es entero", "is.integer(ethnicgroup)",
  "r13", "(eg) fuera de rango", "ethnicgroup %in% c(1:4)",
  "r14", "(scr) es faltante", "!is.na(scr)",
  "r15", "scr fuera de rango", "scr %in% c(1,2)",
  "r16", "(usscr) es faltante", "!is.na(usscr)",
  "r17", "(usscr) dentro de rango", "usscr %in% c(1,2)",
  "r18", "(concent) es faltante", "!is.na(concent)",
  "r19", "(concent) dentro de rango", "concent %in% c(1,2)",
  "r20", "(sn) es faltante", "!is.na(subjectnumber)",
  "r21", "(sn) obligatorio si pasa screening", "ifelse(scr == 2 & usscr == 2 & concent == 2, !is.na(subjectnumber), TRUE)",
  "r22", "(sn) coincide con cc", "as.integer(substr(subjectnumber,1,3)) == countrycode",
  "r23", "(sn) no debería estar si scr=1", "ifelse(scr == 1, is.na(subjectnumber), TRUE)",
  "r24", "(sn) no debería estar si usscr=1", "ifelse(usscr == 1, is.na(subjectnumber), TRUE)",
  "r25", "(sn) no debería estar si concent=1", "ifelse(concent == 1, is.na(subjectnumber), TRUE)"
)

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

#TODO: corroborar que los errores y los conteos de errores esten bien
