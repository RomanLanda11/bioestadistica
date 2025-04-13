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
  "r1", "(cc) es entero", "is.integer(countrycode) & !is.na(countrycode)",
  "r2", "(cc) fuera de rango", "countrycode %in% c(4,11,14,23,31,48,54,65,72,97)",
  "r3", "(fid) es fecha", "is.Date(fechaid) & !is.na(fechaid)",
  "r4", "(iid) es caracter", "is.character(inicialesid) & !is.na(inicialesid)",
  "r5", "(iid) es mayuscula", "grepl('^[A-Z]{2}$', inicialesid)",
  "r6", "(interview) es fecha", "is.Date(interview) & !is.na(interview)",
  "r7", "(eg) es entero", "is.integer(ethnicgroup) & !is.na(ethnicgroup)",
  "r8", "(eg) fuera de rango", "ethnicgroup %in% c(1:4)",
  "r9", "scr fuera de rango", "!is.na(scr) & scr %in% 1:2",
  "r10", "usscr fuera de rango", "!is.na(usscr) & usscr %in% 1:2",
  "r11", "concent fuera de rango", "!is.na(consent) & consent %in% 1:2",
  "r12", "(sn) esta completo", "ifelse((scr == 2) & (usscr == 2) & (consent == 2), !is.na(subjectnumber), TRUE)",
  "r13", "(sn) es correcto", "!is.na(subjectnumber) & as.integer(substr(subjectnumber,1,3))==countrycode",
  "r14", "mayor de edad", "!is.na(fechaid) & !is.na(interview) & (interval(fechaid, interview) / years(1)) >= 18"
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
