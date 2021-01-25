library(tidyverse)

sinadef_acumulados <- readRDS("datos/datos_acumulados.rds")

per_week <- sinadef_acumulados %>%
#  filter(fecha >= "2020-01-01") %>%
  ungroup() %>%
  group_by(epi_year, epi_week) %>%
  summarise(
    n_dias = n(),
    sunday_of_week = unique(sunday_of_week),
    monday_of_week = unique(monday_of_week),
    epi_year = unique(epi_year),
    epi_week = unique(epi_week),
    menores = sum(menores),
    fallecidos = sum(n),
    en_casa = sum(en_casa)
  )

Sys.setlocale("LC_TIME", "es_PE.utf8")

p1 <- ggplot(
  per_week,
  aes(x = monday_of_week, y = menores,
      fill = factor(epi_year), color = factor(epi_year))
) +
  geom_line(size = 3, show.legend = FALSE) +
  #geom_col(show.legend = FALSE) +
  # geom_vline(
  #    xintercept = as.Date("2020-12-31"),
  #    linetype = "dashed",
  #    size = 1
  #  ) +
  geom_smooth(aes(group = 1), method = "gam", show.legend = FALSE) +
  annotate(
    geom = "errorbarh",
    xmin = as.Date("2020-01-01"),
    xmax = as.Date("2020-12-31"),
    y = 200
  ) +
  annotate(
    geom = "text",
    x = as.Date("2020-07-01"),
    y = 210,
    label = "2020",
    size = 8
  ) +
  annotate(
    geom = "errorbarh",
    xmin = as.Date("2019-01-01"),
    xmax = as.Date("2019-12-31"),
    y = 100
  ) +
  annotate(
    geom = "text",
    x = as.Date("2019-07-01"),
    y = 90,
    label = "2019",
    size = 8
  ) +
  annotate(
    geom = "errorbarh",
    xmin = as.Date("2018-01-01"),
    xmax = as.Date("2018-12-31"),
    y = 230
  ) +
  annotate(
    geom = "text",
    x = as.Date("2018-07-01"),
    y = 240,
    label = "2018",
    size = 8
  ) +
  annotate(
    geom = "errorbarh",
    xmin = as.Date("2017-01-01"),
    xmax = as.Date("2017-12-31"),
    y = 100
  ) +
  annotate(
    geom = "text",
    x = as.Date("2017-07-01"),
    y = 90,
    label = "2017",
    size = 8
  ) +
  scale_x_date(date_labels = "%b\n%Y",
               date_breaks = "3 months") +
  scale_y_continuous(limits = c(0, NA)) +
  scale_color_brewer(palette = "Dark2") +
  labs(
    y = "Número de fallecidos por semana",
    x = "",
    title = "Fallecimientos de menores de 18 años por causas no violentas",
    caption = "En azul: modelo GAM [y ~ s(x, bs = 'cs')]\nFuente: SINADEF (Datos abiertos, 2021-01-24) // @jmcastagnetto, Jesus M. Castagnetto"
  ) +
  theme_classic(32) +
  theme(
    plot.caption = element_text(family = "Inconsolata", size = 24),
    axis.text.x = element_text(size = 20),
    plot.margin = unit(rep(1, 4), "cm")
  )

# de 2017 - 2021
ggsave(
  p1,
  file = "plots/sinadef-menores-causas-noviolentas-por-semana-2017-2021-lineas-gam.png",
  width = 18,
  height = 10
)

# de 2020 - 2021
ggsave(
  p1 +
    scale_x_date(
      limits = c(as.Date("2020-01-01"), NA),
      date_labels = "%b\n%Y",
      date_breaks = "1 month"
    ),
  file = "plots/sinadef-menores-causas-noviolentas-por-semana-2020-2021-lineas-gam.png",
  width = 18,
  height = 10
)
