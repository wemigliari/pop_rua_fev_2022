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


### Tabela 2015 do Ministério das Cidades

brasil_munic_2015  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201512.csv", 
                               col_names = TRUE)

names(brasil_munic_2015)[1]<- "codigo_ibge"

munic_2015_pop_rua  <- data.frame(count(brasil_munic_2015, "codigo_ibge"))

brasil <- read_sf("/Users/wemigliari/Documents/R/data/BR_municipios_2021.shp")
names(brasil)[1] <- "codigo_ibge"

brasil_munic_2015_pop_rua <- data <- merge(munic_2015_pop_rua, brasil, by = "codigo_ibge")

brasil_munic_2015_pop_rua <- brasil_munic_2015_pop_rua%>%
  select(codigo_ibge, freq, NM_MUN, SIGLA)

shp_joined_2015 <- st_as_sf(data) 

shp_joined_2015 <- shp_joined_2015%>% 
  select(freq, codigo_ibge, NM_MUN, SIGLA)

shp_joined_2015$geometry <- NULL

shp_joined_2015 <- shp_joined_2015[order(shp_joined_2015$freq, decreasing = TRUE),]

sum(shp_joined_2015$freq)

write.xlsx(shp_joined_2015, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/municipios_pop_rua_2015.xlsx",
           sheetName="Municípios Participantes", append=TRUE)


