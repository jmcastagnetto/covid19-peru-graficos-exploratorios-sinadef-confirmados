library(tidyverse)

# de: https://github.com/jmcastagnetto/sinadef-peru-fallecimientos
# read on demand
rds_url <- "https://github.com/jmcastagnetto/sinadef-peru-fallecimientos/blob/main/datos/sinadef-procesado.rds?raw=true"
sinadef <- readRDS(url(rds_url))
cat("Rango de fechas SINADEF\n")
range(sinadef$fecha)

# muertes violentas ICD 10 (fuente: Antonio Quispe, 2021-01-24)
icd10_violent <- unique(c(
  # homicidios
  paste0("X", 85:99),
  sprintf("Y%02d", 00:99),
  "Y87.1",
  # suicidios
  paste0("Y", 60:84),
  "Y87.0",
  # accidentes y causas externas
  sprintf("V%02d", 0:99),
  sprintf("W%02d", 0:99),
  "V09.0",
  "V09.2",
  "V19.0",
  "V19.1",
  "V19.2",
  "V19.4",
  "V19.6",
  "V80.3",
  "V80.4",
  "V80.5",
  "V81.0",
  "V81.1",
  "V82.0",
  "V82.1",
  "V87.0",
  "V87.8",
  "V88.8",
  "V88.9",
  "V89.0",
  "V89.1",
  "V89.2"
))

sinadef_acumulados <- sinadef %>%
  mutate(
    causa_violenta = (
      causa_a_cie_x %in% icd10_violent |
      causa_b_cie_x %in% icd10_violent |
      causa_c_cie_x %in% icd10_violent |
      causa_d_cie_x %in% icd10_violent |
      causa_e_cie_x %in% icd10_violent |
      causa_f_cie_x %in% icd10_violent
    ),
    menor_de_edad = (edad_anhos < 18)
  ) %>%
  group_by(fecha) %>%
  summarise(
    n = n(),
    menores = sum(menor_de_edad, na.rm = TRUE),
    en_casa = sum((tipo_lugar == "DOMICILIO"), na.rm = TRUE),
  ) %>%
  mutate(
    sunday_of_week = lubridate::floor_date(
      fecha,
      "weeks",
      week_start = 7), # epiweeks comienzan en Domingo
    monday_of_week = sunday_of_week + 1,
    epi_year = lubridate::epiyear(sunday_of_week),
    epi_week = lubridate::epiweek(sunday_of_week)
  )


saveRDS(sinadef_acumulados, "datos/datos_acumulados.rds")
