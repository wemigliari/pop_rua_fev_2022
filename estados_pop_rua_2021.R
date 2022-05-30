library(readxl)
library(dplyr)
library(plyr)
library(stringr)
library(readr)
library(tidyverse)
library(writexl)
library(RColorBrewer)
library(leaflet)
library(rgdal)
library(sf)


### Tabela 2021 do Ministério das Cidades

brasil_munic_2021  <- read_excel("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_202112_abril.xlsx", 
                               col_names = TRUE)

names(brasil_munic_2021)[4]<- "codigo_ibge"

munic_2021_pop_rua  <- data.frame(count(brasil_munic_2021, "codigo_ibge"))
sum(munic_2021_pop_rua$freq)

######### Municípios

brasil_municipios <- read_sf("/Users/wemigliari/Documents/R/data/BR_municipios_2021.shp")
names(brasil_municipios)[1] <- "codigo_ibge"

brasil_municipios_2021_pop_rua <- data <- merge(munic_2021_pop_rua, brasil_municipios, by = "codigo_ibge")

brasil_municipios_2021_pop_rua <- brasil_municipios_2021_pop_rua%>%
  select(codigo_ibge, freq, NM_MUN, SIGLA)


######### Estados

brasil_municipios_2021_pop_rua$NM_MUN <- NULL

brasil_estados_2021_pop_rua<- aggregate(brasil_municipios_2021_pop_rua["freq"],
                                        by=brasil_municipios_2021_pop_rua["SIGLA"],
                                        sum)

sum(brasil_municipios_2021_pop_rua$freq)

###### Para Datawrapper

write_xlsx(brasil_estados_2021_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/estados_pop_rua_2021.xlsx")

###### Mapas no R

brasil_estados <- read_sf("/Users/wemigliari/Documents/R/data/BR_municipios_2021.shp")

brasil_estados_2021_pop_rua <- data <- merge(brasil_estados_2021_pop_rua, brasil_estados, by = "SIGLA")


###### Teste de soma na Regiao Centro-Oeste/ Confere

test <- subset(brasil_estados_2021_pop_rua,
               subset = sigla == "DF" | sigla == "GO" | sigla == "MT" | sigla == "MS")
sum(test$freq)

