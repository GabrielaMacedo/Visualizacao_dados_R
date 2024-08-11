#https://www.databrewer.co/R/gallery/ggplot2-line-plot-cigarettes




# Pacotes e limpeza de dados
library(ggplot2)
library(dplyr)    # 
library(tidyr)    # tidy up data structure   
library(stringr)  # string manipulation

theme_set(theme_minimal(base_size = 15)) # global theme


# Os dados são provenientes do Our World in Data
d <- read.csv("C:/Users/g_mac/OneDrive/Documentos/00_Projetos_Compartilhados/00_Visulizalizacao_de_dados/R/Mudanca_popularidade_tabagismo/cigarettes.csv")

d.tidy <- d %>% 
  pivot_longer(-year, names_to = "country", values_to = "cigarettes") %>% 
  mutate(cigarettes = as.numeric(cigarettes))

head(d.tidy, n = 3)

# Visualização
# gráfico de linhas simples para visualizar as vendas de cigarros para cada país.
p1 <- d.tidy %>% 
  ggplot(aes(x = year, y = cigarettes, group = country)) +
  geom_line(color = "snow3") 
p1


#Destaque as vendas de cigarros em três países selecionados.

countries.selected <- c("United.States", "France", "Germany")

p2 <- p1 +
  geom_line(
    data = d.tidy %>% filter(country %in% countries.selected),
    aes(color = country),
    linewidth = 1) +
  scale_color_manual(values = c("skyblue3", "steelblue4", "firebrick"))
p2

#Adicione anotações de texto às três linhas de destaque em substituição à legenda padrão
p3 <- p2 +
  annotate(
    geom = "text",
    x = c(1933, 1948, 1955, 1880),
    y = c(6, 1, 3, 10),
    hjust = c(1, 0, 0, 0), # 1, right justify;  0, left justify
    label = c("United States", "France", "Germany", 
              "Cigarettes sold\nper day per adult"),
    color = c("firebrick", "skyblue3", "steelblue4", "snow4"),
    fontface = "bold", size = c(5, 5, 5, 4)) 
p3

# Ajustes
p4 <- p3 +
  # adjust axis breaks
  scale_x_continuous(breaks = seq(1880, 2000, 20)) +
  scale_y_continuous(breaks = seq(0, 10, 2)) +
  # expand to fill up the entire plotting range
  coord_cartesian(expand = 0) +
  # update the axial and plot titles
  labs(
    # remove the axial titles
    y = NULL, x = NULL, 
    # add plot title and subtitle
    title = "Cigarette consumption in developed countries",
    subtitle = str_wrap(
      "Smoking became increasingly popular since 1920s, peaked around 1960s ~ 1980s, and significant decreased in 1990s.", 
      width = 60) # number of characters per line
  ) 

p4

#Acabamento final do tema.
p5 <- p4 + 
  theme(
    legend.position = "none",
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(linewidth = .1, linetype = "dashed"),
    axis.line.x = element_line(color = "snow4"),
    axis.ticks.x = element_line(color = "snow4"),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 12, color = "orange4",
                                 margin = margin(b = 15))) 
p5




ggsave(filename = "Mudanca_popularidade_tabagismo.png",
       path = "C:/Users/g_mac/OneDrive/Documentos/00_Projetos_Compartilhados/00_Visulizalizacao_de_dados/R/Mudanca_popularidade_tabagismo",
       width = 8, height = 5)



