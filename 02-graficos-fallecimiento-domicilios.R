library(tidyverse)

# datos filtrados desde el 2020-11-01 a la fecha
sinadef_nov1_hoy <- readRDS("datos/datos_acumulados.rds") %>%
  filter(fecha >= "2020-11-01")

per_week <- sinadef_nov1_hoy %>%
  ungroup() %>%
  group_by(epi_year, epi_week) %>%
  summarise(
    n_dias = n(),
    sunday_of_week = unique(sunday_of_week),
    monday_of_week = unique(monday_of_week),
    epi_year = unique(epi_year),
    epi_week = unique(epi_week),
    en_casa = sum(en_casa)
  )

p1 <- ggplot(
  per_week,
  aes(x = monday_of_week, y = en_casa,
      fill = factor(epi_year))
) +
  geom_col(show.legend = FALSE, width = 5) +
  geom_vline(
    xintercept = as.Date("2020-12-31"),
    linetype = "dashed",
    size = 1
  ) +
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_date(date_labels = "S: %V\n%Y",
               date_breaks = "1 week") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  labs(
    y = "Número de fallecidos por semana",
    x = "",
    title = "Fallecimientos en domicilios por causas no violentas",
    #subtitle = "Del 2020-11-01 a la fecha, acumulados por semana epidemiológica",
    caption = "Fuente: SINADEF (Datos Abiertos, 2021-01-24) // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  theme_classic(32) +
  theme(
    plot.caption = element_text(family = "Inconsolata", size = 24)
  )

ggsave(
  p1,
  file = "plots/sinadef-fallecidos-domicilio-causas-noviolentas-por-semana.png",
  width = 16,
  height = 9
)
