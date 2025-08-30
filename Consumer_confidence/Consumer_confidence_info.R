# Artigo original https://www.visualcapitalist.com/cp/charting-the-global-decline-in-consumer-confidence/
# https://r-graph-gallery.com/web-line-chart-small-multiple-all-group-greyed-out.html
# --------------------------------------------
# 0) Preparação do ambiente (opcional)
# --------------------------------------------
rm(list = ls(all.names = TRUE))
unlink(".RData")
graphics.off()
cat("\014")

options(repos = c(CRAN = "https://cloud.r-project.org"))
# setwd("C:/.../Consumer_confidence") # se precisar

# --------------------------------------------
# 1) Pacotes
# --------------------------------------------
need <- c("tidyverse","janitor","showtext","MetBrewer","scico",
          "ggtext","patchwork","gghighlight","lubridate","glue")
new  <- need[!sapply(need, requireNamespace, quietly = TRUE)]
if (length(new)) install.packages(new, dependencies = TRUE)

library(tidyverse)
library(janitor)
library(showtext)
library(MetBrewer)
library(scico)
library(ggtext)
library(patchwork)
library(gghighlight)
library(lubridate)
library(glue)

# --------------------------------------------
# 2) Dados
# --------------------------------------------
df1 <- readr::read_csv(
  "https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/dataConsumerConfidence.csv",
  show_col_types = FALSE
) %>% 
  mutate(date = lubridate::my(Time)) %>%      # <- formato "m-y" correto para este arquivo
  dplyr::select(-Time) %>%                    # força dplyr::select()
  pivot_longer(cols = -date, names_to = "country", values_to = "value") %>% 
  mutate(
    country = dplyr::recode(country,
                            "United States"  = "USA",
                            "United Kingdom" = "UK",
                            "Korea"          = "South Korea",
                            "Korea, Rep."    = "South Korea"
    )
  ) %>% 
  tidyr::drop_na(value)                       # remove apenas valores faltantes

stopifnot(nrow(df1) > 0)                      # sanity check

# --------------------------------------------
# 3) Tema / fontes
# --------------------------------------------
font <- "Gudea"
showtext_auto(enable = TRUE)
font_add_google(family = font, font, db_cache = TRUE)

bg <- "#F4F5F1"
txt_col <- "black"
theme_set(theme_minimal(base_family = font, base_size = 10))

caption_text  <- glue("**Design:** Gilbert Fontana<br>**Data:** OECD, 2022")

# --------------------------------------------
# 4) Gráfico principal
# --------------------------------------------
# Ordem de interesse e apenas países presentes no dataset
wanted <- c("USA","China","Japan","Germany","UK","France","Italy","South Korea","Australia")
present <- intersect(wanted, unique(df1$country))
df1 <- df1 %>% mutate(country = factor(country, levels = present))

# Paleta com o nº de países presentes
pal <- MetBrewer::met.brewer("Redon", n = length(present))

p1 <- ggplot(df1) +
  geom_hline(yintercept = 100, linetype = "solid", linewidth = .25) +
  geom_line(aes(x = date, y = value, color = country)) +
  gghighlight(
    use_direct_label = FALSE,
    unhighlighted_params = list(colour = scales::alpha("grey85", 1))
  ) +
  geom_point(
    data = df1 %>% dplyr::group_by(country) %>% dplyr::slice_max(date, with_ties = FALSE),
    aes(x = date, y = value, color = country), shape = 16
  ) +
  geom_text(
    data = df1 %>% dplyr::group_by(country) %>% dplyr::slice_max(date, with_ties = FALSE),
    aes(x = date, y = value, color = country, label = round(value)),
    hjust = -0.5, vjust = 0.5, size = 2.5, family = font, fontface = "bold"
  ) +
  scale_color_manual(values = pal) +
  scale_x_date(date_labels = "%y") +
  scale_y_continuous(breaks = c(90,95,100,105,110), labels = c("","","100","","")) +
  facet_wrap(~ country, ncol = 3) +
  coord_cartesian(clip = "off") +
  theme(
    axis.title = element_blank(),
    axis.text = element_text(color = txt_col, size = 7),
    strip.text.x = element_text(face = "bold"),
    plot.title = element_markdown(
      hjust = .5, size = 34, color = txt_col, lineheight = .8, face = "bold",
      margin = ggplot2::margin(20, 0, 30, 0)
    ),
    plot.subtitle = element_markdown(
      hjust = .5, size = 18, color = txt_col, lineheight = 1,
      margin = ggplot2::margin(10, 0, 30, 0)
    ),
    plot.caption = element_markdown(
      hjust = .5, size = 8, color = txt_col, lineheight = 1.2,
      margin = ggplot2::margin(60, 0, 0, 0)
    ),
    plot.caption.position = "plot",
    plot.background = element_rect(color = bg, fill = bg),
    plot.margin = ggplot2::margin(10, 10, 10, 10),
    legend.position = "none",
    legend.title = element_text(face = "bold")
  )

# ----------
p1

# pasta de saída
out_dir <- "C:/Users/g_mac/OneDrive/Documentos/00_Projetos_Compartilhados/00_Visulizalizacao_de_dados/R/Consumer_confidence"
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

# salva p1 em PDF
ggsave(
  filename = "consumer_confidence.pdf",
  path     = out_dir,
  plot     = p1,
  device   = grDevices::cairo_pdf,  # se o Cairo não estiver disponível, use device = "pdf"
  width    = 8, height = 5, units = "in",
  bg       = bg
)


ggsave(
  filename = "consumer_confidence.png",
  path     = out_dir,
  plot     = p1,
  width    = 8, height = 5, units = "in",
  dpi      = 300,
  bg       = bg
)



