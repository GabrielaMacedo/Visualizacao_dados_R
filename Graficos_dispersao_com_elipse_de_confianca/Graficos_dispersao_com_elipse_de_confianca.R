#Importa Bibliotecas
library(ggplot2)
library(dplyr)
install.packages("palmerpenguins")
library(palmerpenguins) # data package

# set as default theme
theme_set(theme_minimal(base_size = 16))

head(penguins, 4)

# Crie um gráfico de dispersão, mostrando a relação entre o comprimento e a profundidade do bico de um pinguim
p1 <- penguins %>% 
  ggplot(aes(x = bill_length_mm, y = bill_depth_mm, 
             color = species)) + 
  
  # add small amount of random noise to points position to reduce overlap
  geom_point(position = position_jitter(
    width = .2, height = 0.2, seed = 1)) +
  
  # color scale for categorical variable
  scale_color_brewer(palette = "Set2") 
p1

#Draw confidence ellipse that encircles 95% of data points.
p2 <- p1 + 
  stat_ellipse(level = .95, show.legend = F, linewidth = .7) 
p2

#Desenhe uma elipse de confiança que circunde 95% dos pontos de dados.
species <- tibble(
  x = c(34, 56.5, 56),
  y = c(20, 16.5, 19),
  species = c("Adelie", "Gentoo",  "Chinstrap"))

p3 <- p2 + geom_text(
  data = species, 
  aes(x = x, y = y, label = species, color = species),
  # not inherit aesthetic mapping from the ggplot line
  inherit.aes = F, 
  fontface = "bold.italic", size = 6)
p3

#Refine mais detalhes conforme comentado abaixo.
p4 <- p3 +
  # rename axes' titles
  labs(x = "bill length (mm)", y = "bill depth (mm)") + 
  # adjust axis breaks 
  scale_x_continuous(breaks = seq(from = 20, to = 60, by = 4)) + 
  scale_y_continuous(breaks = seq(from = 10, to = 25, by = 2)) + 
  
  theme(
    # remove legend
    legend.position = "none",
    # remove the minor grids, and adjust the width of the major grids
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linewidth = .3),
    # bold axis titles
    axis.title = element_text(face = "bold"),
    # increase the margin on the right side of y-axis title, and on top of x-axis title
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10))) + 
  
  # center justify the legend title
  guides(color = guide_legend(title.hjust = .5)) 

p4

#Visualize a distribuição marginal.
p5 <- p4 +
  geom_rug(
    # apply the same "jitter" position to align with points
    position = position_jitter(width = .2, height = 0.2, seed = 1), 
    sides = "tr", # t, top; r, right; b, bottom; l, left       
    length = unit(10, "pt"), # shorter bar line length 
    linewidth = .1) # smaller width to reduce overlap

p5

#install.packages("ggExtra")
library(ggExtra)
ggMarginal(p5, groupFill = T, size = 10, linewidth = 0, alpha = .4) 


# Salve o gráfico. Por padrão, o gráfico exibido mais recentemente será salvo.
# ggsave(filename = "Graficos_dispersao_com_elipse_de_confianca.pdf",
#        path = "C:/Users/g_mac/OneDrive/Documentos/00_Projetos_Compartilhados/00_Visulizalizacao_de_dados/R/Graficos_dispersao_com_elipse_de_confianca",
#        width = 8, height = 5)



