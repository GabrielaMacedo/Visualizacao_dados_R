
#Import bibliotecas
#install.packages("tidyr")
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)

d <- read.csv("C:/Users/g_mac/OneDrive/Documentos/00_Projetos_Compartilhados/00_Visulizalizacao_de_dados/R/Votacao_demografica/Trump_vs_Clinton.csv")

d.ordered <- d %>%
  mutate(category = factor(category, levels = rev(d$category))) 

d.tidy <- d.ordered %>% 
  # tidy up
  pivot_longer(contains("Vote"), 
               names_to = "candidate", values_to = "percent") %>% 
  mutate(candidate = str_remove(candidate, "Vote.for.")) %>% 
  
  # mark the candidate with the most votes in each voting group
  group_by(category) %>% 
  mutate(is.max = percent == max(percent))

head(d.tidy, n = 3)


#Visualização
# 1. Crie segmentos de linha e pontos. 
p1 <- d.tidy %>% 
  ggplot(aes(x = category, y = percent)) +
  
  # using the dataset before tidy up to create the segments
  geom_segment(data = d.ordered,
               aes(x = category, xend = category,
                   y = Vote.for.Clinton, yend = Vote.for.Trump), 
               color = "snow3", linewidth = 2) +
  
  # points created using the "global" d.tidy dataset
  geom_point(aes(color = candidate), size = 3) +
  scale_color_manual(values = c("Clinton" = "steelblue", 
                                "Trump" = "firebrick3"))
p1

#2. Inverta e facete o gráfico.
p2 <- p1 + 
  coord_flip(clip = "off") + # flip the plot
  
  # facet the plot based on voting groups
  facet_grid(group ~ ., scales = "free", space = "free_y")  
p2

# 3. Adicione títulos de painel manualmente no canto superior esquerdo, em substituição aos padrões
p3 <- p2 + 
  # use the same `group` variable which has been used for faceting
  geom_text(data = tibble(group = c("Age", "Education", "Gender", "Race/Ethnicity")),
            aes(x = c(5.2, 5.2, 3.2, 5.2), 
                y = -20, 
                label = group), 
            hjust = 0, fontface = "bold", size = 3.5) 
p3

#4. Adicione manualmente os rótulos
p4 <- p3 +
  geom_text(aes(y = -20, label = category),
            hjust = 0, size = 3, color = "grey30")
p4

#5. Remova os títulos dos painéis padrão

p5 <- p4 + 
  theme_minimal() +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        strip.text = element_blank(),
        
        # adjust distance between subplots
        panel.spacing.y = unit(15, "pt")) 
p5

#6. Ajuste a grade do painel vertical
p6 <- p5 + scale_y_continuous(
  breaks = seq(0, 100, 20),
  minor_breaks = NULL) + # remove the minor vertical grids 
  theme(panel.grid.major.y = element_line(linewidth = .2))
p6

#7. Adicione rótulos de texto da porcentagem de votação.
p7 <- p6 + 
  geom_text(
    aes(label = paste(percent, "%"), 
        y = ifelse(is.max == T, percent + 8, percent - 8),
        color = candidate),
    show.legend = F) 
p7


#8. Adicione anotações de texto
p8 <- p7 +
  # add text annotation in the top first panel
  geom_text(
    data = tibble(percent = c(12, 80), 
                  category = "18-29",
                  text = c("vote for Trump", "vote for Clinton"),
                  group = "Age"), # texts added only to the 1st panel "Age"
    aes(x = category, y = percent, label = text),
    
    color = c("firebrick3", "steelblue"), 
    fontface = "bold", size = 4,
    vjust = -2) + # text shifted upward
  
  # remove default legend
  theme(legend.position = "none") + 
  
  # adjust the plot title
  labs(title = "Voter Demographics in 2016\n")  +
  theme(plot.title = element_text(face = "bold", hjust = .5, size = 15))

p8



ggsave(filename = "Votacao_demografica.pdf",
      path = "C:/Users/g_mac/OneDrive/Documentos/00_Projetos_Compartilhados/00_Visulizalizacao_de_dados/R/Votacao_demografica",
      width = 8, height = 5)