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


### Tabela 2012 do Ministério das Cidades

brasil_munic_2012  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201212.csv", 
                               col_names = TRUE)

names(brasil_munic_2012)[1]<- "codigo_ibge"

munic_2012_pop_rua  <- data.frame(count(brasil_munic_2012, "codigo_ibge"))

brasil <- read_sf("/Users/wemigliari/Documents/R/data/BR_municipios_2021.shp")
names(brasil)[1] <- "codigo_ibge"

brasil_munic_2012_pop_rua <- data <- merge(munic_2012_pop_rua, brasil, by = "codigo_ibge")

brasil_munic_2012_pop_rua <- brasil_munic_2012_pop_rua%>%
  select(codigo_ibge, freq, NM_MUN, SIGLA)

shp_joined_2012 <- st_as_sf(data) 

shp_joined_2012 <- shp_joined_2012%>% 
  select(freq, codigo_ibge, NM_MUN, SIGLA)

shp_joined_2012$geometry <- NULL

shp_joined_2012 <- shp_joined_2012[order(shp_joined_2012$freq, decreasing = TRUE),]

sum(shp_joined_2012$freq)

write.xlsx(shp_joined_2012, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/municipios_pop_rua_2012.xlsx",
           sheetName="Municípios Participantes", append=TRUE)


