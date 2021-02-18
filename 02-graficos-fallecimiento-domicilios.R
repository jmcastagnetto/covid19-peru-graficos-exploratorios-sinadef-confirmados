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
  ) %>%
  ungroup() %>%
  mutate(
    label = if_else(n_dias == 7, "", "Semana\nincompleta")
  )

updated <- Sys.Date()

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
  geom_text(aes(label = label), angle = 90,
            hjust = 0, size = 6,
            nudge_y = 100, color = "black",
            show.legend = FALSE) +
  coord_cartesian(ylim = c(0, NA)) +
  scale_x_date(date_labels = "S: %V\n%Y",
               date_breaks = "1 week") +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_brewer(palette = "Set1", direction = -1) +
  labs(
    y = "Número de fallecidos por semana",
    x = "",
    title = "Fallecimientos en domicilios (todas las causas)",
    #subtitle = "Del 2020-11-01 a la fecha, acumulados por semana epidemiológica",
    caption = glue::glue("Fuente: SINADEF (Datos Abiertos)\n{updated}, @jmcastagnetto, Jesus M. Castagnetto")
  ) +
  theme_classic(28) +
  theme(
    plot.caption = element_text(family = "Inconsolata", size = 20),
    plot.margin = unit(rep(1, 4), "cm")
  )
p1
ggsave(
  p1,
  file = "plots/sinadef-fallecidos-domicilio-todas-causas-por-semana.png",
  width = 18,
  height = 10
)

