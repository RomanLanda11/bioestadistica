library(readxl)
library(lubridate)

adm <- read_excel("tp1/adm.xlsx")
dd <- read_excel("tp1/dd.xlsx")

adm$fechaid <- sub("-.*", "", adm$patientid)  # Extrae todo antes del "-"
adm$inicialesid <- sub(".*-", "", adm$patientid)  # Extrae todo después del "-"


reglas = tribble(
  ~id, ~desc, ~cond,
  "r2", "(cc) es entero", "is.integer(countrycode)", # si es NA, no es entero
  "r3", "(cc) fuera de rango", "countrycode %in% c(4,11,14,23,31,48,54,65,72,97)",
  "r4", "(fid) es fecha", "is.Date(fechaid)", # si es NA; no es fecha.
  "r5", "(iid) es caracter", "is.character(inicialesid)", # si es NA, no es char.
  "r6", "(iid) es mayuscula", "grepl('^[A-Z]{2}$', inicialesid)",
  "r7", "(interview) es fecha","is.Date(interview)", # si es NA; no es fecha.
  "r8", "(eg) es entero", "is.integer(ethnicgroup)", # si es NA, no es entero
  "r10", "(eg) fuera de rango", "ethnicgroup %in% c(1:4)",
  "r11", "scr fuera de rango", "scr %in% c(1,2)", # Si no es int no va a estar
  "r12", "usscr fuera de rango", "usscr %in% c(1,2)",
  "r13", "concent fuera de rango", "concent %in% c(1,2)",
  "r14", "(sn) esta completo", "ifelse((scr == 2) & (usscr == 2) & (concent == 2), !is.na(objectnumber), FALSE)",
  "r15", "(sn) es correcto", "as.integer(substr(subjectnumber,1,3))==countrycode",
  "r16", "mayor de edad", "as.integer(interval(fechaid, interview) / years(1)) > 18"
)


# TODO: adaptar funcion
# Funcion validador()
# argumentos:
# - datos: conjunto de validacion
# - id : nombre de la columna en (datos) con el identificador
# - cond : nombre de la columna en (datos) con la condicion
# salida: vector nombrado
validador = function(datos, id, cond){
  reglas = datos[[cond]]
  names(reglas) = datos[[id]]
  reglas
}
# Funcion validar()
# argumentos:
# - datos : conjunto de datos a validar
# - id : nombre de la columna en (datos) con el identificador
# - validador: salida de validador()
# salida: tibble con el resultado de la validación
validar = function(datos, id, validador){
  sapply(
    validador,
    function(x) eval(parse(text = x), datos)
  ) |>
    dplyr::as_tibble() |>
    dplyr::mutate(registro = datos[[id]], .before = 0)
}