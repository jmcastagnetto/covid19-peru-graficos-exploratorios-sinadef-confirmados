library(tidyverse)

load("datos/datos_abiertos_minsa_covid-19_peru.Rdata")

sinadef_selected <- readRDS("datos/datos_acumulados.rds") %>%
  filter(fecha >= "2020-05-01")

sinadef_per_week <- sinadef_selected %>%
  ungroup() %>%
  group_by(epi_year, epi_week) %>%
  summarise(
    n_dias = n(),
    sunday_of_week = unique(sunday_of_week),
    monday_of_week = unique(monday_of_week),
    epi_year = unique(epi_year),
    epi_week = unique(epi_week),
    fallecidos = sum(n, na.rm = TRUE)
  )

confirmados_per_week <- fallecimientos %>%
  filter(fecha_fallecimiento >= "2020-05-01") %>%
  mutate(
    sunday_of_week = lubridate::floor_date(
      fecha_fallecimiento,
      "weeks",
      week_start = 7), # epiweeks comienzan en Domingo
    monday_of_week = sunday_of_week + 1,
    epi_year = lubridate::epiyear(sunday_of_week),
    epi_week = lubridate::epiweek(sunday_of_week)
  ) %>%
  group_by(epi_year, epi_week) %>%
  summarise(
    fall_oficial = n()
  )

combined_df <- sinadef_per_week %>%
  left_join(
    confirmados_per_week,
    by = c("epi_year", "epi_week")
  ) %>%
  ungroup() %>%
  mutate(
    ratio = fallecidos / fall_oficial
  )

Sys.setlocale("LC_TIME", "es_PE.utf8")
p1 <- ggplot(
  combined_df,
  aes(x = monday_of_week, y = ratio,
      color = factor(epi_year))
) +
  ggalt::geom_lollipop(show.legend = FALSE, point.size = 6, size = 3) +
  geom_vline(
    xintercept = as.Date("2020-12-31"),
    linetype = "dashed",
    size = 1
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_date(date_labels = "%b\n%Y",
               date_breaks = "1 month") +
  scale_color_brewer(palette = "Dark2") +
  labs(
    y = "Tasa por semana",
    x = "",
    title = "Tasa de fallecidos (no violentos) en SINADEF vs confirmados COVID-19",
    caption = "Fuentes: SINADEF y Fallecidos por COVID-19 (Datos Abiertos, 2021-01-24) // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  theme_classic(28) +
  theme(
    plot.caption = element_text(family = "Inconsolata", size = 21)
  )

ggsave(
  p1,
  file = "plots/sinadef-noviolentas-vs-fallecidos-covid19-por-semana.png",
  width = 18,
  height = 10
)

