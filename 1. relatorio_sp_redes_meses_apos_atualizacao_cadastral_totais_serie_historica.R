library(igraph)
library(readxl)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(extrafont)
library(RColorBrewer)
library(ggrepel)
library(reshape2)

###### Atualização Cadastral

sp_totais_2021_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2021.xlsx", 
                                        sheet="Atualização")
sp_totais_2021_atua_cadas   <- data.frame(sp_totais_2021_atua_cadas)
sp_totais_2021_atua_cadas$...1 <- NULL
sp_totais_2021_atua_cadas$Ano <- "2021"
sp_totais_2021_atua_cadas$Min <- min(sp_totais_2021_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
sp_totais_2021_atua_cadas$Max <- max(sp_totais_2021_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)

sp_totais_2020_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2020.xlsx", 
                                        sheet="Atualização")
sp_totais_2020_atua_cadas   <- data.frame(sp_totais_2020_atua_cadas)
sp_totais_2020_atua_cadas$...1 <- NULL
sp_totais_2020_atua_cadas$Ano <- "2020"
sp_totais_2020_atua_cadas$Min <- min(sp_totais_2020_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
sp_totais_2020_atua_cadas$Max <- max(sp_totais_2020_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)


sp_totais_2019_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2019.xlsx", 
                                        sheet="Atualização")
sp_totais_2019_atua_cadas   <- data.frame(sp_totais_2019_atua_cadas)
sp_totais_2019_atua_cadas$...1 <- NULL
sp_totais_2019_atua_cadas$Ano <- "2019"
sp_totais_2019_atua_cadas$Min <- min(sp_totais_2019_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
sp_totais_2019_atua_cadas$Max <- max(sp_totais_2019_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)



sp_totais_2018_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2018.xlsx", 
                                        sheet="Atualização")
sp_totais_2018_atua_cadas   <- data.frame(sp_totais_2018_atua_cadas)
sp_totais_2018_atua_cadas$...1 <- NULL
sp_totais_2018_atua_cadas$Ano <- "2018"
sp_totais_2018_atua_cadas$Min <- min(sp_totais_2018_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
sp_totais_2018_atua_cadas$Max <- max(sp_totais_2018_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)


sp_totais_2017_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2017.xlsx", 
                                        sheet="Atualização")
sp_totais_2017_atua_cadas   <- data.frame(sp_totais_2017_atua_cadas)
sp_totais_2017_atua_cadas$...1 <- NULL
sp_totais_2017_atua_cadas$Ano <- "2017"
sp_totais_2017_atua_cadas$Min <- min(sp_totais_2017_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
sp_totais_2017_atua_cadas$Max <- max(sp_totais_2017_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)


sp_totais_2016_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2016.xlsx", 
                                        sheet="Atualização")
sp_totais_2016_atua_cadas   <- data.frame(sp_totais_2016_atua_cadas)
sp_totais_2016_atua_cadas$...1 <- NULL
sp_totais_2016_atua_cadas$Ano <- "2016"
sp_totais_2016_atua_cadas$Min <- min(sp_totais_2016_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
sp_totais_2016_atua_cadas$Max <- max(sp_totais_2016_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)


sp_totais_2015_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2015.xlsx", 
                                        sheet="Atualização")
sp_totais_2015_atua_cadas   <- data.frame(sp_totais_2015_atua_cadas)
sp_totais_2015_atua_cadas$...1 <- NULL
sp_totais_2015_atua_cadas$Ano <- "2015"
sp_totais_2015_atua_cadas$Min <- min(sp_totais_2015_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
sp_totais_2015_atua_cadas$Max <- max(sp_totais_2015_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)


sp_totais_2014_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2014.xlsx", 
                                        sheet="Atualização")
sp_totais_2014_atua_cadas   <- data.frame(sp_totais_2014_atua_cadas)
sp_totais_2014_atua_cadas$...1 <- NULL
sp_totais_2014_atua_cadas$Ano <- "2014"
sp_totais_2014_atua_cadas$Min <- min(sp_totais_2014_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
sp_totais_2014_atua_cadas$Max <- max(sp_totais_2014_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)


sp_totais_2013_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2013.xlsx", 
                                        sheet="Atualização")
sp_totais_2013_atua_cadas   <- data.frame(sp_totais_2013_atua_cadas)
sp_totais_2013_atua_cadas$...1 <- NULL
sp_totais_2013_atua_cadas$Ano <- "2013"
sp_totais_2013_atua_cadas$Min <- min(sp_totais_2013_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
sp_totais_2013_atua_cadas$Max <- max(sp_totais_2013_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)



sp_totais_2012_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2012.xlsx", 
                                        sheet="Atualização")
sp_totais_2012_atua_cadas   <- data.frame(sp_totais_2012_atua_cadas)
sp_totais_2012_atua_cadas$...1 <- NULL
sp_totais_2012_atua_cadas$Ano <- "2012"
sp_totais_2012_atua_cadas$Min <- min(sp_totais_2012_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
sp_totais_2012_atua_cadas$Max <- max(sp_totais_2012_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)



serie_historica_cadastro <- rbind(sp_totais_2021_atua_cadas, 
                                  sp_totais_2020_atua_cadas, 
                                  sp_totais_2019_atua_cadas,
                                  sp_totais_2018_atua_cadas,
                                  sp_totais_2017_atua_cadas,
                                  sp_totais_2016_atua_cadas,
                                  sp_totais_2015_atua_cadas,
                                  sp_totais_2014_atua_cadas,
                                  sp_totais_2013_atua_cadas,
                                  sp_totais_2012_atua_cadas)



# Adding column based on other column:
serie_historica_cadastro <- serie_historica_cadastro %>%
  mutate(Nível = case_when(
              MESES_APOS_ULT_ATUALIZACAO <= 12 ~ "Atualizado", 
              MESES_APOS_ULT_ATUALIZACAO <= 36 ~ "A ser atualizado",
              MESES_APOS_ULT_ATUALIZACAO >= 37 ~ "Não atualizado")
  )

serie_historica_cadastro <- serie_historica_cadastro %>%
  mutate("Tempo de Rua" = case_when(
    MESES_APOS_ULT_ATUALIZACAO <= 12 ~ "12 Meses", 
    MESES_APOS_ULT_ATUALIZACAO <= 36 ~ "Entre 12 e 36 Meses",
    MESES_APOS_ULT_ATUALIZACAO >= 37 ~ "Mais de 36 Meses")
  )

library(ggraph)
library(network)
library(ggnetwork)
library(GGally)
library(sna)
library(tsna)
library(ndtv)

library(tidyverse)
edge_list <- tibble(from = serie_historica_cadastro$Ano, to = serie_historica_cadastro$freq)
node_list <- tibble(id = 1:578)


sources <- serie_historica_cadastro %>%
  distinct(MESES_APOS_ULT_ATUALIZACAO) %>%
  rename(label = MESES_APOS_ULT_ATUALIZACAO)

destinations <- serie_historica_cadastro %>%
  distinct(freq) %>%
  rename(label = freq)

nodes <- full_join(sources, destinations, by = "label")
nodes

nodes <- nodes %>% rowid_to_column("id")
nodes


per_route <- serie_historica_cadastro %>%  
  group_by(MESES_APOS_ULT_ATUALIZACAO, freq) %>%
  summarise(weight = MESES_APOS_ULT_ATUALIZACAO) %>% 
  ungroup()
per_route


edges <- per_route %>% 
  left_join(nodes, by = c(MESES_APOS_ULT_ATUALIZACAO = "label")) %>% 
  rename(from = id)

edges <- edges %>% 
  left_join(nodes, by = c(freq = "label")) %>% 
  rename(to = id)

edges <- select(edges, from, to, weight)
edges

routes_network <- network(edges, vertex.attr = nodes, matrix.type = "edgelist", ignore.eval = FALSE, loops = TRUE)
class(routes_network)

plot(routes_network, vertex.cex = 3)



library(tidygraph)
library(ggraph)

routes_tidy <- tbl_graph(nodes = nodes, edges = edges, directed = TRUE)
routes_igraph_tidy <- as_tbl_graph(routes_igraph)

routes_tidy %>% 
  activate(edges) %>% 
  arrange(desc(weight))

ggraph(routes_tidy) + geom_edge_link() + geom_node_point() + theme_graph()

set.seed(12345)

ggraph(routes_tidy, layout='graphopt') + 
  geom_node_point(color="#2F4F4F") +
  geom_edge_link(aes(width = weight), alpha = 0.3, color="#708090") + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label), repel = TRUE) +
  labs(edge_width = "Meses Após a Atualização") +
  labs(x ="", y = "Meses Após a Útlima Atualização",
       subtitle = "Série Histórica 2012-2021 de Pessoas em Situação de Rua em São Paulo, Atualização Cadastral",
       caption = "Fonte: Ministério da Cidadania. Elaborado por W. Migliari, 2022.") +
  theme_graph(base_family = "Helvetica")


ggraph(routes_igraph, layout = "linear") + 
  geom_edge_arc(aes(width = weight), alpha = 0.2) + 
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_text(aes(label = label)) +
  labs(edge_width = "Meses Após a Atualização") +
  theme_graph(base_family = "Helvetica")


