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


### Tabela 2017 do Ministério das Cidades

brasil_munic_2017  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201712.csv", 
                               col_names = TRUE)

names(brasil_munic_2017)[1]<- "codigo_ibge"

munic_2017_pop_rua  <- data.frame(count(brasil_munic_2017, "codigo_ibge"))

######### Municípios

brasil_municipios <- read_sf("/Users/wemigliari/Documents/R/data/BR_municipios_2021.shp")
names(brasil_municipios)[1] <- "codigo_ibge"

brasil_municipios_2017_pop_rua <- data <- merge(munic_2017_pop_rua, brasil_municipios, by = "codigo_ibge")

brasil_municipios_2017_pop_rua <- brasil_municipios_2017_pop_rua%>%
  select(codigo_ibge, freq, NM_MUN, SIGLA)


######### Estados

brasil_municipios_2017_pop_rua$NM_MUN <- NULL

brasil_estados_2017_pop_rua<- aggregate(brasil_municipios_2017_pop_rua["freq"],
                                        by=brasil_municipios_2017_pop_rua["SIGLA"],
                                        sum)

sum(brasil_municipios_2017_pop_rua$freq)

######### Regiões

brasil_regioes_2017_pop_rua_sudeste <- subset(brasil_estados_2017_pop_rua, 
                                              subset = SIGLA == "SP" | SIGLA == "MG" | SIGLA == "RJ" | SIGLA == "ES")

brasil_regioes_2017_pop_rua_sudeste  <- data.frame(sum(brasil_regioes_2017_pop_rua_sudeste$freq))
colnames(brasil_regioes_2017_pop_rua_sudeste)[1] <- "freq"
brasil_regioes_2017_pop_rua_sudeste$sigla <- "SE"


brasil_regioes_2017_pop_rua_sul <- subset(brasil_estados_2017_pop_rua, 
                                          subset = SIGLA == "RS" | SIGLA == "SC" | SIGLA == "PR")

brasil_regioes_2017_pop_rua_sul  <- data.frame(sum(brasil_regioes_2017_pop_rua_sul$freq))
colnames(brasil_regioes_2017_pop_rua_sul)[1] <- "freq"
brasil_regioes_2017_pop_rua_sul$sigla <- "S"


brasil_regioes_2017_pop_rua_nordeste <- subset(brasil_estados_2017_pop_rua, 
                                               subset = SIGLA == "BA" | SIGLA == "PE" | SIGLA == "CE" | SIGLA == "AL" | SIGLA == "MA" | SIGLA == "PB" | SIGLA == "RN" | SIGLA == "PI" | SIGLA == "SE")

brasil_regioes_2017_pop_rua_nordeste  <- data.frame(sum(brasil_regioes_2017_pop_rua_nordeste$freq))
colnames(brasil_regioes_2017_pop_rua_nordeste)[1] <- "freq"
brasil_regioes_2017_pop_rua_nordeste$sigla <- "NE"


brasil_regioes_2017_pop_rua_norte <- subset(brasil_estados_2017_pop_rua, 
                                            subset = SIGLA == "AC" | SIGLA == "AP" | SIGLA == "AM" | SIGLA == "PA" | SIGLA == "RO" | SIGLA == "RR" | SIGLA == "TO")

brasil_regioes_2017_pop_rua_norte  <- data.frame(sum(brasil_regioes_2017_pop_rua_norte$freq))
colnames(brasil_regioes_2017_pop_rua_norte)[1] <- "freq"
brasil_regioes_2017_pop_rua_norte$sigla <- "N"


brasil_regioes_2017_pop_rua_co <- subset(brasil_estados_2017_pop_rua,
                                         subset = SIGLA == "DF" | SIGLA == "GO" | SIGLA == "MT" | SIGLA == "MS")

brasil_regioes_2017_pop_rua_co  <- data.frame(sum(brasil_regioes_2017_pop_rua_co$freq))
colnames(brasil_regioes_2017_pop_rua_co)[1] <- "freq"
brasil_regioes_2017_pop_rua_co$sigla <- "CO"

brasil_regioes_2017_pop_rua_soma <- rbind(
  brasil_regioes_2017_pop_rua_sudeste,
  brasil_regioes_2017_pop_rua_sul,
  brasil_regioes_2017_pop_rua_nordeste,
  brasil_regioes_2017_pop_rua_norte,
  brasil_regioes_2017_pop_rua_co
)

sum(brasil_regioes_2017_pop_rua_soma$freq)

####



brasil_regioes <- read_sf("/Users/wemigliari/Documents/R/data/regioes_2010.shp", 
                          options = "ENCODING=windows-1252")

brasil_regioes_2017_pop_rua <- data <- merge(brasil_regioes_2017_pop_rua_soma, brasil_regioes, by = "sigla")



###### Para Datawrapper

write_xlsx(brasil_regioes_2017_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/regioes_pop_rua_2017.xlsx")

######
