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
library(scales)


### Totais Estados 2021 do Ministério das Cidades

brasil_munic_2021  <- read_excel("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_202112_abril.xlsx", 
                                 col_names = TRUE)

names(brasil_munic_2021)[4]<- "codigo_ibge"

munic_2021_pop_rua  <- data.frame(count(brasil_munic_2021, "codigo_ibge"))
sum(munic_2021_pop_rua$freq)

######### Municípios

brasil_municipios <- read_sf("/Users/wemigliari/Documents/R/data/BR_municipios_2021.shp")
names(brasil_municipios)[1] <- "codigo_ibge"

brasil_municipios$geometry <- NULL

brasil_municipios_2021_pop_rua <- data <- merge(munic_2021_pop_rua, brasil_municipios, by = "codigo_ibge")

brasil_municipios_2021_pop_rua <- brasil_municipios_2021_pop_rua%>%
  select(SIGLA, NM_MUN, freq)

brasil_municipios_2021_pop_rua$Ano <- "2021"

######### Estados

brasil_estados_2021_pop_rua<- aggregate(brasil_municipios_2021_pop_rua["freq"],
                                        by=brasil_municipios_2021_pop_rua["SIGLA"],
                                        sum)
totais_estados_2021 <-merge(brasil_municipios_2021_pop_rua, brasil_estados_2021_pop_rua, by = "SIGLA")

totais_estados_2021 <- totais_estados_2021[, c(1, 5, 2, 3, 4)]

totais_estados_2021$Percentual <- percent(totais_estados_2021$freq.x/totais_estados_2021$freq.y, accuracy= 0.01)


##################################################
### Totais Estados 2020 do Ministério das Cidades

brasil_munic_2020  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_202012.csv", 
                               col_names = TRUE)

names(brasil_munic_2020)[1]<- "codigo_ibge"

munic_2020_pop_rua  <- data.frame(count(brasil_munic_2020, "codigo_ibge"))

######### Municípios

brasil_municipios <- read_sf("/Users/wemigliari/Documents/R/data/BR_municipios_2021.shp")
names(brasil_municipios)[1] <- "codigo_ibge"

brasil_municipios$geometry <- NULL

brasil_municipios_2020_pop_rua <- data <- merge(munic_2020_pop_rua, brasil_municipios, by = "codigo_ibge")

brasil_municipios_2020_pop_rua <- brasil_municipios_2020_pop_rua%>%
  select(SIGLA, NM_MUN, freq)

brasil_municipios_2020_pop_rua$Ano <- "2020"

######### Estados

brasil_estados_2020_pop_rua<- aggregate(brasil_municipios_2020_pop_rua["freq"],
                                        by=brasil_municipios_2020_pop_rua["SIGLA"],
                                        sum)
totais_estados_2020 <-merge(brasil_municipios_2020_pop_rua, brasil_estados_2020_pop_rua, by = "SIGLA")

totais_estados_2020 <- totais_estados_2020[, c(1, 5, 2, 3, 4)]

totais_estados_2020$Percentual <- percent(totais_estados_2020$freq.x/totais_estados_2020$freq.y, accuracy= 0.01)



#################################################
### Totais Estados 2019 do Ministério das Cidades

brasil_munic_2019  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201912.csv", 
                               col_names = TRUE)

names(brasil_munic_2019)[1]<- "codigo_ibge"

munic_2019_pop_rua  <- data.frame(count(brasil_munic_2019, "codigo_ibge"))

######### Municípios

brasil_municipios <- read_sf("/Users/wemigliari/Documents/R/data/BR_municipios_2021.shp")
names(brasil_municipios)[1] <- "codigo_ibge"

brasil_municipios$geometry <- NULL

brasil_municipios_2019_pop_rua <- data <- merge(munic_2019_pop_rua, brasil_municipios, by = "codigo_ibge")

brasil_municipios_2019_pop_rua <- brasil_municipios_2019_pop_rua%>%
  select(SIGLA, NM_MUN, freq)

brasil_municipios_2019_pop_rua$Ano <- "2019"

######### Estados

brasil_estados_2019_pop_rua<- aggregate(brasil_municipios_2019_pop_rua["freq"],
                                        by=brasil_municipios_2019_pop_rua["SIGLA"],
                                        sum)
totais_estados_2019 <-merge(brasil_municipios_2019_pop_rua, brasil_estados_2019_pop_rua, by = "SIGLA")

totais_estados_2019 <- totais_estados_2019[, c(1, 5, 2, 3, 4)]

totais_estados_2019$Percentual <- percent(totais_estados_2019$freq.x/totais_estados_2019$freq.y, accuracy= 0.01)

sum(totais_estados_2019$freq.x)

#################################################
### Totais Estados 2018 do Ministério das Cidades

brasil_munic_2018  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201812.csv", 
                               col_names = TRUE)

names(brasil_munic_2018)[1]<- "codigo_ibge"

munic_2018_pop_rua  <- data.frame(count(brasil_munic_2018, "codigo_ibge"))

######### Municípios

brasil_municipios <- read_sf("/Users/wemigliari/Documents/R/data/BR_municipios_2021.shp")
names(brasil_municipios)[1] <- "codigo_ibge"

brasil_municipios$geometry <- NULL

brasil_municipios_2018_pop_rua <- data <- merge(munic_2018_pop_rua, brasil_municipios, by = "codigo_ibge")

brasil_municipios_2018_pop_rua <- brasil_municipios_2018_pop_rua%>%
  select(SIGLA, NM_MUN, freq)

brasil_municipios_2018_pop_rua$Ano <- "2018"

######### Estados

brasil_estados_2018_pop_rua<- aggregate(brasil_municipios_2018_pop_rua["freq"],
                                        by=brasil_municipios_2018_pop_rua["SIGLA"],
                                        sum)
totais_estados_2018 <-merge(brasil_municipios_2018_pop_rua, brasil_estados_2018_pop_rua, by = "SIGLA")

totais_estados_2018 <- totais_estados_2018[, c(1, 5, 2, 3, 4)]

totais_estados_2018$Percentual <- percent(totais_estados_2018$freq.x/totais_estados_2018$freq.y, accuracy= 0.01)

sum(totais_estados_2018$freq.x)

#################################################
### Totais Estados 2017 do Ministério das Cidades

brasil_munic_2017  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201712.csv", 
                               col_names = TRUE)

names(brasil_munic_2017)[1]<- "codigo_ibge"

munic_2017_pop_rua  <- data.frame(count(brasil_munic_2017, "codigo_ibge"))

######### Municípios

brasil_municipios <- read_sf("/Users/wemigliari/Documents/R/data/BR_municipios_2021.shp")
names(brasil_municipios)[1] <- "codigo_ibge"

brasil_municipios$geometry <- NULL

brasil_municipios_2017_pop_rua <- data <- merge(munic_2017_pop_rua, brasil_municipios, by = "codigo_ibge")

brasil_municipios_2017_pop_rua <- brasil_municipios_2017_pop_rua%>%
  select(SIGLA, NM_MUN, freq)

brasil_municipios_2017_pop_rua$Ano <- "2017"

######### Estados

brasil_estados_2017_pop_rua<- aggregate(brasil_municipios_2017_pop_rua["freq"],
                                        by=brasil_municipios_2017_pop_rua["SIGLA"],
                                        sum)
totais_estados_2017 <-merge(brasil_municipios_2017_pop_rua, brasil_estados_2017_pop_rua, by = "SIGLA")

totais_estados_2017 <- totais_estados_2017[, c(1, 5, 2, 3, 4)]

totais_estados_2017$Percentual <- percent(totais_estados_2017$freq.x/totais_estados_2017$freq.y, accuracy= 0.01)

sum(totais_estados_2017$freq.x)

#################################################
### Totais Estados 2016 do Ministério das Cidades

brasil_munic_2016  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201612.csv", 
                               col_names = TRUE)

names(brasil_munic_2016)[1]<- "codigo_ibge"

munic_2016_pop_rua  <- data.frame(count(brasil_munic_2016, "codigo_ibge"))

######### Municípios

brasil_municipios <- read_sf("/Users/wemigliari/Documents/R/data/BR_municipios_2021.shp")
names(brasil_municipios)[1] <- "codigo_ibge"

brasil_municipios$geometry <- NULL

brasil_municipios_2016_pop_rua <- data <- merge(munic_2016_pop_rua, brasil_municipios, by = "codigo_ibge")

brasil_municipios_2016_pop_rua <- brasil_municipios_2016_pop_rua%>%
  select(SIGLA, NM_MUN, freq)

brasil_municipios_2016_pop_rua$Ano <- "2016"

######### Estados

brasil_estados_2016_pop_rua<- aggregate(brasil_municipios_2016_pop_rua["freq"],
                                        by=brasil_municipios_2016_pop_rua["SIGLA"],
                                        sum)
totais_estados_2016 <-merge(brasil_municipios_2016_pop_rua, brasil_estados_2016_pop_rua, by = "SIGLA")

totais_estados_2016 <- totais_estados_2016[, c(1, 5, 2, 3, 4)]

totais_estados_2016$Percentual <- percent(totais_estados_2016$freq.x/totais_estados_2016$freq.y, accuracy= 0.01)

sum(totais_estados_2016$freq.x)

#################################################
### Totais Estados 2015 do Ministério das Cidades


brasil_munic_2015  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201512.csv", 
                               col_names = TRUE)

names(brasil_munic_2015)[1]<- "codigo_ibge"

munic_2015_pop_rua  <- data.frame(count(brasil_munic_2015, "codigo_ibge"))

######### Municípios

brasil_municipios <- read_sf("/Users/wemigliari/Documents/R/data/BR_municipios_2021.shp")
names(brasil_municipios)[1] <- "codigo_ibge"

brasil_municipios$geometry <- NULL

brasil_municipios_2015_pop_rua <- data <- merge(munic_2015_pop_rua, brasil_municipios, by = "codigo_ibge")

brasil_municipios_2015_pop_rua <- brasil_municipios_2015_pop_rua%>%
  select(SIGLA, NM_MUN, freq)

brasil_municipios_2015_pop_rua$Ano <- "2015"

######### Estados

brasil_estados_2015_pop_rua<- aggregate(brasil_municipios_2015_pop_rua["freq"],
                                        by=brasil_municipios_2015_pop_rua["SIGLA"],
                                        sum)
totais_estados_2015 <-merge(brasil_municipios_2015_pop_rua, brasil_estados_2015_pop_rua, by = "SIGLA")

totais_estados_2015 <- totais_estados_2015[, c(1, 5, 2, 3, 4)]

totais_estados_2015$Percentual <- percent(totais_estados_2015$freq.x/totais_estados_2015$freq.y, accuracy= 0.01)

sum(totais_estados_2015$freq.x)


#################################################
### Totais Estados 2014 do Ministério das Cidades

brasil_munic_2014  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201412.csv", 
                               col_names = TRUE)

names(brasil_munic_2014)[1]<- "codigo_ibge"

munic_2014_pop_rua  <- data.frame(count(brasil_munic_2014, "codigo_ibge"))

######### Municípios

brasil_municipios <- read_sf("/Users/wemigliari/Documents/R/data/BR_municipios_2021.shp")
names(brasil_municipios)[1] <- "codigo_ibge"

brasil_municipios$geometry <- NULL

brasil_municipios_2014_pop_rua <- data <- merge(munic_2014_pop_rua, brasil_municipios, by = "codigo_ibge")

brasil_municipios_2014_pop_rua <- brasil_municipios_2014_pop_rua%>%
  select(SIGLA, NM_MUN, freq)

brasil_municipios_2014_pop_rua$Ano <- "2014"

######### Estados

brasil_estados_2014_pop_rua<- aggregate(brasil_municipios_2014_pop_rua["freq"],
                                        by=brasil_municipios_2014_pop_rua["SIGLA"],
                                        sum)
totais_estados_2014 <-merge(brasil_municipios_2014_pop_rua, brasil_estados_2014_pop_rua, by = "SIGLA")

totais_estados_2014 <- totais_estados_2014[, c(1, 5, 2, 3, 4)]

totais_estados_2014$Percentual <- percent(totais_estados_2014$freq.x/totais_estados_2014$freq.y, accuracy= 0.01)

sum(totais_estados_2014$freq.x)

#################################################
### Totais Estados 2013 do Ministério das Cidades

brasil_munic_2013  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201312.csv", 
                               col_names = TRUE)

names(brasil_munic_2013)[1]<- "codigo_ibge"

munic_2013_pop_rua  <- data.frame(count(brasil_munic_2013, "codigo_ibge"))

######### Municípios

brasil_municipios <- read_sf("/Users/wemigliari/Documents/R/data/BR_municipios_2021.shp")
names(brasil_municipios)[1] <- "codigo_ibge"

brasil_municipios$geometry <- NULL

brasil_municipios_2013_pop_rua <- data <- merge(munic_2013_pop_rua, brasil_municipios, by = "codigo_ibge")

brasil_municipios_2013_pop_rua <- brasil_municipios_2013_pop_rua%>%
  select(SIGLA, NM_MUN, freq)

brasil_municipios_2013_pop_rua$Ano <- "2013"

######### Estados

brasil_estados_2013_pop_rua<- aggregate(brasil_municipios_2013_pop_rua["freq"],
                                        by=brasil_municipios_2013_pop_rua["SIGLA"],
                                        sum)
totais_estados_2013 <-merge(brasil_municipios_2013_pop_rua, brasil_estados_2013_pop_rua, by = "SIGLA")

totais_estados_2013 <- totais_estados_2013[, c(1, 5, 2, 3, 4)]

totais_estados_2013$Percentual <- percent(totais_estados_2013$freq.x/totais_estados_2013$freq.y, accuracy= 0.01)

sum(totais_estados_2013$freq.x)

#################################################
### Totais Estados 2012 do Ministério das Cidades


brasil_munic_2012  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201212.csv", 
                               col_names = TRUE)

names(brasil_munic_2012)[1]<- "codigo_ibge"

munic_2012_pop_rua  <- data.frame(count(brasil_munic_2012, "codigo_ibge"))

######### Municípios

brasil_municipios <- read_sf("/Users/wemigliari/Documents/R/data/BR_municipios_2021.shp")
names(brasil_municipios)[1] <- "codigo_ibge"

brasil_municipios$geometry <- NULL

brasil_municipios_2012_pop_rua <- data <- merge(munic_2012_pop_rua, brasil_municipios, by = "codigo_ibge")

brasil_municipios_2012_pop_rua <- brasil_municipios_2012_pop_rua%>%
  select(SIGLA, NM_MUN, freq)

brasil_municipios_2012_pop_rua$Ano <- "2012"

######### Estados

brasil_estados_2012_pop_rua<- aggregate(brasil_municipios_2012_pop_rua["freq"],
                                        by=brasil_municipios_2012_pop_rua["SIGLA"],
                                        sum)
totais_estados_2012 <-merge(brasil_municipios_2012_pop_rua, brasil_estados_2012_pop_rua, by = "SIGLA")

totais_estados_2012 <- totais_estados_2012[, c(1, 5, 2, 3, 4)]

totais_estados_2012$Percentual <- percent(totais_estados_2012$freq.x/totais_estados_2012$freq.y, accuracy= 0.01)

sum(totais_estados_2012$freq.x)

########### Série Histórica

serie_historica_totais_estados <- rbind(totais_estados_2021,
                                       totais_estados_2020,
                                       totais_estados_2019,
                                       totais_estados_2018,
                                       totais_estados_2017,
                                       totais_estados_2016,
                                       totais_estados_2015,
                                       totais_estados_2014,
                                       totais_estados_2013,
                                       totais_estados_2012)
serie_historica_totais_estados <- serie_historica_totais_estados[,c(5, 1, 2, 3, 4, 6)]

write_xlsx(serie_historica_totais_estados, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/estados_pop_rua_serie_historica.xlsx")
















