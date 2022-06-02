library(readxl)
library(dplyr)
library(plyr)
library(stringr)
library(readr)
library(tidyverse)
library(writexl)
library(RColorBrewer)




### Tabela 2021 do Ministério das Cidades

brasil_munic_2021  <- read_excel("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_202112_abril.xlsx", 
                                 col_names = TRUE)

names(brasil_munic_2021)[4]<- "codigo_ibge"

munic_2021_pop_rua  <- data.frame(count(brasil_munic_2021, "codigo_ibge"))

munic_2021_pop_rua$Ano <- "2021"

### Tabela 2020 do Ministério das Cidades

brasil_munic_2020  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_202012.csv", 
                               col_names = TRUE)

names(brasil_munic_2020)[1]<- "codigo_ibge"

munic_2020_pop_rua  <- data.frame(count(brasil_munic_2020, "codigo_ibge"))

munic_2020_pop_rua$Ano <- "2020"

### Tabela 2019 do Ministério das Cidades

brasil_munic_2019  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201912.csv", 
                               col_names = TRUE)

names(brasil_munic_2019)[1]<- "codigo_ibge"

munic_2019_pop_rua  <- data.frame(count(brasil_munic_2019, "codigo_ibge"))

munic_2019_pop_rua$Ano <- "2019"

### Tabela 2018 do Ministério das Cidades

brasil_munic_2018  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201812.csv", 
                               col_names = TRUE)

names(brasil_munic_2018)[1]<- "codigo_ibge"

munic_2018_pop_rua  <- data.frame(count(brasil_munic_2018, "codigo_ibge"))

munic_2018_pop_rua$Ano <- "2018"

### Tabela 2017 do Ministério das Cidades

brasil_munic_2017  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201712.csv", 
                               col_names = TRUE)

names(brasil_munic_2017)[1]<- "codigo_ibge"

munic_2017_pop_rua  <- data.frame(count(brasil_munic_2017, "codigo_ibge"))

munic_2017_pop_rua$Ano <- "2017"

### Tabela 2016 do Ministério das Cidades

brasil_munic_2016  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201612.csv", 
                               col_names = TRUE)

names(brasil_munic_2016)[1]<- "codigo_ibge"

munic_2016_pop_rua  <- data.frame(count(brasil_munic_2016, "codigo_ibge"))

munic_2016_pop_rua$Ano <- "2016"

### Tabela 2015 do Ministério das Cidades

brasil_munic_2015  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201512.csv", 
                               col_names = TRUE)

names(brasil_munic_2015)[1]<- "codigo_ibge"

munic_2015_pop_rua  <- data.frame(count(brasil_munic_2015, "codigo_ibge"))

munic_2015_pop_rua$Ano <- "2015"

### Tabela 2014 do Ministério das Cidades

brasil_munic_2014  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201412.csv", 
                               col_names = TRUE)

names(brasil_munic_2014)[1]<- "codigo_ibge"

munic_2014_pop_rua  <- data.frame(count(brasil_munic_2014, "codigo_ibge"))

munic_2014_pop_rua$Ano <- "2014"

### Tabela 2013 do Ministério das Cidades

brasil_munic_2013  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201312.csv", 
                               col_names = TRUE)

names(brasil_munic_2013)[1]<- "codigo_ibge"

munic_2013_pop_rua  <- data.frame(count(brasil_munic_2013, "codigo_ibge"))

munic_2013_pop_rua$Ano <- "2013"

### Tabela 2012 do Ministério das Cidades

brasil_munic_2012  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201212.csv", 
                               col_names = TRUE)

names(brasil_munic_2012)[1]<- "codigo_ibge"

munic_2012_pop_rua  <- data.frame(count(brasil_munic_2012, "codigo_ibge"))

munic_2012_pop_rua$Ano <- "2012"

##########


serie_historica_cidades <- rbind(munic_2021_pop_rua,
                                 munic_2020_pop_rua,
                                 munic_2019_pop_rua, 
                                 munic_2018_pop_rua, 
                                 munic_2017_pop_rua, 
                                 munic_2016_pop_rua, 
                                 munic_2015_pop_rua, 
                                 munic_2014_pop_rua, 
                                 munic_2013_pop_rua, 
                                 munic_2012_pop_rua)
sum(serie_historica_cidades$freq)

serie_historica_cidades$Ano <- as.numeric(serie_historica_cidades$Ano)

serie_historica_cidades <- serie_historica_cidades[order(serie_historica_cidades$Ano),]

serie_historica_totais_cidades  <- data.frame(count(serie_historica_cidades, "codigo_ibge"))

###### Merging data
library(sf)

brasil <- read_sf("/Users/wemigliari/Documents/R/data/BR_municipios_2021.shp")
names(brasil)[1] <- "codigo_ibge"

brasil_munic_pop_rua_serie_historica <- merge(serie_historica_cidades, brasil, by = "codigo_ibge")

brasil_munic_pop_rua_serie_historica  <- st_as_sf(brasil_munic_pop_rua_serie_historica) 

brasil_munic_pop_rua_serie_historica <- brasil_munic_pop_rua_serie_historica[order(brasil_munic_pop_rua_serie_historica$Ano, decreasing = TRUE),]
brasil_munic_pop_rua_serie_historica$geometry <- NULL


######

library(xlsx)

write.xlsx(brasil_munic_pop_rua_serie_historica, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/serie_historica_pop_rua_cidades.xlsx",
           sheetName="Cidades", append=TRUE)


##########

brasil_munic_pop_rua_serie_historica <- merge(serie_historica_cidades, brasil, by = "codigo_ibge")

brasil_munic_pop_rua_serie_historica  <- st_as_sf(brasil_munic_pop_rua_serie_historica) 

brasil_munic_pop_rua_serie_historica <- brasil_munic_pop_rua_serie_historica[order(brasil_munic_pop_rua_serie_historica$Ano, decreasing = TRUE),]

library(ggplot2)
library(gganimate)
library(ggthemes)

serie_historica_totais_cidades <- ggplot(serie_historica_cidades, 
  aes(Ano, freq, fill = freq)) +
  geom_point() +
  geom_line() +
  theme_economist() + 
  scale_color_economist()+
  theme(legend.position = "none") +
  #scale_size(range = c(2, 12)) +
  #scale_x_log10() +
  labs(subtitle = "População em Situação de Rua nos Municípios Brasileiros", 
       x = "",
       y = "",
       caption = "Fonte: Ministério da Cidadania. Série Histórica 2021-2021. Elaborado por W. Migliari.") +
  transition_states(as.factor(Ano), state_length = 0) +
  shadow_wake(wake_length = 0.3, alpha = FALSE)

anim_save("/Users/wemigliari/Documents/R/R_Scripts/pop_rua_2022/totais_cidades.gif", serie_historica_totais_cidades)

#########

serie_historica_totais_cidades2 <- ggplot(serie_historica_cidades, 
                                         aes(Ano, freq, fill = freq)) +
  geom_point() +
  geom_line() +
  theme_economist() + 
  scale_color_economist()+
  theme(legend.position = "none") +
  #scale_size(range = c(2, 12)) +
  #scale_x_log10() +
  labs(subtitle = "População em Situação de Rua nos Municípios Brasileiros", 
       x = "",
       y = "",
       caption = "Fonte: Ministério da Cidadania. Série Histórica 2021-2021. Elaborado por W. Migliari.")

########



########

serie_historica_cidades %>%
  ggplot(aes(Ano, freq)) + geom_point() + geom_line() +
  theme_minimal() +
  transition_reveal(Ano) 






