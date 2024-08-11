# Instala os pacotes necessários
install.packages("ggplot2")
install.packages("dplyr")
install.packages("gapminder")
install.packages("showtext")

# Chama as bibliotecas
library(ggplot2)
library(dplyr)
library(gapminder) # Carrega o dataset "gapminder"
library(showtext)

# Configura o tema global padrão
theme_set(theme_classic(base_size = 12))
head(gapminder, 3)

# Define a ordem dos painéis facetados:
ordered.continent <- c("Africa", "Asia", "Americas", "Europe", "Oceania")

# Converte "continent" para um fator com a ordem de nível especificada
g <- gapminder %>% 
  mutate(continent = factor(continent, levels = ordered.continent))

head(g, n = 3) # Pronto para visualização

# Visualização
# 1. Crie um gráfico de linha mostrando as mudanças na expectativa de vida ao longo dos anos em cada país.
p1 <- g %>% 
  ggplot(aes(x = year, y = lifeExp, color = continent, fill = continent)) +
  geom_line(aes(group = country), alpha = .3)
p1

# 2. Crie linhas de tendência média por continente usando a função stat_summary().
p2 <- p1 +
  stat_summary(fun = mean, geom = "line", size = 2)
p2

# 3. Desenhe faixas para representar um desvio padrão (SD) acima e abaixo da média da expectativa de vida.
p3 <- p2 +  
  stat_summary(
    fun.data = mean_sdl, fun.args = list(mult = 1),
    geom = "ribbon", alpha = .3, color = NA)

p3

# 4. Divida o gráfico em subgráficos, com cada continente em um painel separado.
p4 <- p3 + 
  facet_wrap(~continent, nrow = 1) +
  theme(legend.position = "none") +
  scale_color_manual(values = continent_colors) +
  scale_fill_manual(values = continent_colors) 

p4

# 5. Adicione linhas de anotação e textos para marcar os anos de início e término (1952 e 2007).
p5 <- p4 +
  geom_vline(xintercept = 1952, linetype = "dashed", color = "orange3") +
  geom_vline(xintercept = 2007, linetype = "dashed", color = "skyblue3") +
  annotate(geom = "text", x = 1952, y = 20, label = " 1952", 
           fontface = "bold", hjust = 0, color = "orange3") +
  annotate(geom = "text", x = 2007, y = 20, label = "2007 ", 
           fontface = "bold", hjust = 1, color = "skyblue3") 
p5

# 6. Anote o gráfico com a expectativa de vida média em 1952 e 2007 em cada continente.
life.1952_2007 <- g %>% 
  filter(year %in% c(1952, 2007)) %>% 
  group_by(continent, year) %>% 
  summarise(lifeExp = mean(lifeExp) %>% round())

head(life.1952_2007, 3)

p6 <- p5 + 
  geom_label(
    data = life.1952_2007, aes(label = lifeExp), 
    color = "white", 
    hjust = c(1, 0) %>% rep(5)) 
p6

# 7. Exiba completamente os textos (ou outros elementos gráficos) além do limite do painel.
p7 <- p6 + 
  coord_cartesian(clip = "off") + 
  theme(
    plot.margin = margin(rep(20, 4)),
    panel.spacing = unit(20, "pt"))
p7

# 8. Rotule os painéis com os nomes dos continentes.
panel.titles <- tibble(continent = factor(ordered.continent))

p8 <- p7 + 
  geom_text(
    data = panel.titles,
    aes(x = 1980, y = 30, label = continent),
    size = 7, fontface = "bold")
p8

# 9. Carregue fontes padrão do sistema (sem usar fontes do Google).
# Isso evita problemas de fontes ao salvar o gráfico.
p9 <- p8 + 
  labs(
    title = "Steady increase of Human Life Expectancy", 
    caption = "Each line represents one country; central line: the average; \nribbon, one standard deviation around the mean.",
    x = NULL)  +
  theme(
    strip.text = element_blank(), 
    axis.line.y = element_blank(),
    axis.line.x = element_blank(),
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.length.y = unit(20, "pt"),
    axis.ticks.y = element_line(linetype = "dashed"),
    plot.title = element_text(size = 18, family = "sans"),
    plot.caption = element_text(hjust = 0, size = 10, color = "grey60"),
    plot.background = element_rect(fill = "azure"),
    panel.background = element_rect(fill = "azure")
  ) 

p9

# Salve o gráfico. Por padrão, o gráfico exibido mais recentemente será salvo.
ggsave(filename = "line_plot_life_expectancy.pdf",
       path = "C:/Users/g_mac/OneDrive/Documentos/00_Projetos_Compartilhados/00_Visulizalizacao_de_dados/R/Mudanca_global_expectativa_vida",
       width = 8, height = 5)
