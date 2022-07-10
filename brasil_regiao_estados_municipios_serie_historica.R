library(readxl)
library(dplyr)
library(plyr)
library(stringr)
library(readr)
library(tidyverse)
library(writexl)
library(RColorBrewer)
library(leaflet)
library(sf)



### Brasil Shapefile

brasil <- read_sf("/Users/wemigliari/Documents/R/data/BR_Municipios_2021.shp")
names(brasil)[1] <- "codigo_ibge"

### Tabela 2021 do Ministério das Cidades

brasil_munic_2021  <- read_excel("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_202112_abril.xlsx", 
                                 col_names = TRUE)
names(brasil_munic_2021)[4]<- "codigo_ibge"

total <- data.frame(count(brasil_munic_2021, "codigo_ibge"))


###10 Seleção de Dados por Sexo

test <- brasil_munic_2021 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test$CO_SEXO_PESSOA[which(test$CO_SEXO_PESSOA=="1")] <- "Masculino"
test$CO_SEXO_PESSOA[which(test$CO_SEXO_PESSOA=="2")] <- "Feminino"

munic_2021_pop_rua <- merge(test, brasil, by = "codigo_ibge")
munic_2021_pop_rua$geometry <- NULL
munic_2021_pop_rua$Ano <- "2021"
munic_2021_pop_rua <- na.omit(munic_2021_pop_rua)

total <- data.frame(count(brasil_munic_2021, "codigo_ibge"))
munic_2021_pop_rua <- merge(munic_2021_pop_rua, total, by="codigo_ibge")
munic_2021_pop_rua$proporcao <- (munic_2021_pop_rua$n/munic_2021_pop_rua$freq)*100

#Brasil
brasil_total_2021 <-count(munic_2021_pop_rua, "codigo_ibge", "n")
brasil_total_2021 <- sum(brasil_total_2021$freq)
brasil_total_2021 <- data.frame(brasil_total_2021)
names(brasil_total_2021)[1] <- "Total Brasil"

munic_2021_pop_rua$`Total Brasil` <- brasil_total_2021$`Total Brasil`

#Estados
estados_total_2021 <-count(munic_2021_pop_rua, "SIGLA", "n")
munic_2021_pop_rua <- merge(munic_2021_pop_rua, estados_total_2021, by = "SIGLA")


#Regiões

brasil_regioes_2021_pop_rua_sudeste <- subset(estados_total_2021, 
                                              subset = SIGLA == "SP" | SIGLA == "MG" | SIGLA == "RJ" | SIGLA == "ES")

brasil_regioes_2021_pop_rua_sudeste$Região <- "Sudeste"
total_regiao_sudeste_2021 <- sum(brasil_regioes_2021_pop_rua_sudeste$freq)
brasil_regioes_2021_pop_rua_sudeste$`Total Região`  <- total_regiao_sudeste_2021

#

brasil_regioes_2021_pop_rua_sul <- subset(estados_total_2021, 
                                          subset = SIGLA == "RS" | SIGLA == "SC" | SIGLA == "PR")

brasil_regioes_2021_pop_rua_sul$Região <- "Sul"
total_regiao_sul_2021 <- sum(brasil_regioes_2021_pop_rua_sul$freq)
brasil_regioes_2021_pop_rua_sul$`Total Região`  <- total_regiao_sul_2021

#

brasil_regioes_2021_pop_rua_nordeste <- subset(estados_total_2021, 
                                               subset = SIGLA == "BA" | SIGLA == "PE" | SIGLA == "CE" | SIGLA == "AL" | SIGLA == "MA" | SIGLA == "PB" | SIGLA == "RN" | SIGLA == "PI" | SIGLA == "SE")

brasil_regioes_2021_pop_rua_nordeste$Região <- "Nordeste"
total_regiao_nordeste_2021 <- sum(brasil_regioes_2021_pop_rua_nordeste$freq)
brasil_regioes_2021_pop_rua_nordeste$`Total Região`  <- total_regiao_nordeste_2021

#

brasil_regioes_2021_pop_rua_norte <- subset(estados_total_2021, 
                                            subset = SIGLA == "AC" | SIGLA == "AP" | SIGLA == "AM" | SIGLA == "PA" | SIGLA == "RO" | SIGLA == "RR" | SIGLA == "TO")

brasil_regioes_2021_pop_rua_norte$Região <- "Norte"
total_regiao_norte_2021 <- sum(brasil_regioes_2021_pop_rua_norte$freq)
brasil_regioes_2021_pop_rua_norte$`Total Região`  <- total_regiao_norte_2021

#

brasil_regioes_2021_pop_rua_centro_oeste <- subset(estados_total_2021,
                                         subset = SIGLA == "DF" | SIGLA == "GO" | SIGLA == "MT" | SIGLA == "MS")

brasil_regioes_2021_pop_rua_centro_oeste$Região <- "Centro-Oeste"
total_regiao_centro_oeste_2021 <- sum(brasil_regioes_2021_pop_rua_centro_oeste$freq)
brasil_regioes_2021_pop_rua_centro_oeste$`Total Região`  <- total_regiao_centro_oeste_2021

brasil_regioes_2021_pop_rua_soma <- rbind(
  brasil_regioes_2021_pop_rua_sudeste,
  brasil_regioes_2021_pop_rua_sul,
  brasil_regioes_2021_pop_rua_nordeste,
  brasil_regioes_2021_pop_rua_norte,
  brasil_regioes_2021_pop_rua_centro_oeste
)

munic_2021_pop_rua <- merge(brasil_regioes_2021_pop_rua_soma, munic_2021_pop_rua, by = "SIGLA")



### Tabela 2020 do Ministério das Cidades

brasil_munic_2020  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_202012.csv", 
                               col_names = TRUE)
names(brasil_munic_2020)[1]<- "codigo_ibge"

total_2020 <- data.frame(count(brasil_munic_2020, "codigo_ibge"))


###10 Seleção de Dados por Sexo

test_2020 <- brasil_munic_2020 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test_2020$CO_SEXO_PESSOA[which(test_2020$CO_SEXO_PESSOA=="1")] <- "Masculino"
test_2020$CO_SEXO_PESSOA[which(test_2020$CO_SEXO_PESSOA=="2")] <- "Feminino"

munic_2020_pop_rua <- merge(test_2020, brasil, by = "codigo_ibge")
munic_2020_pop_rua $geometry <- NULL
munic_2020_pop_rua $Ano <- "2020"
munic_2020_pop_rua  <- na.omit(munic_2020_pop_rua )

total <- data.frame(count(brasil_munic_2020, "codigo_ibge"))
munic_2020_pop_rua  <- merge(munic_2020_pop_rua , total, by="codigo_ibge")
munic_2020_pop_rua $proporcao <- (munic_2020_pop_rua $n/munic_2020_pop_rua $freq)*100

#Brasil
brasil_total_2020 <-count(munic_2020_pop_rua , "codigo_ibge", "n")
brasil_total_2020 <- sum(brasil_total_2020$freq)
brasil_total_2020 <- data.frame(brasil_total_2020)
names(brasil_total_2020)[1] <- "Total Brasil"

munic_2020_pop_rua $`Total Brasil` <- brasil_total_2020$`Total Brasil`

#Estados
estados_total_2020 <-count(munic_2020_pop_rua, "SIGLA", "n")
munic_2020_pop_rua  <- merge(munic_2020_pop_rua, estados_total_2020, by = "SIGLA")

#Regiões

brasil_regioes_2020_pop_rua_sudeste <- subset(estados_total_2020, 
                                              subset = SIGLA == "SP" | SIGLA == "MG" | SIGLA == "RJ" | SIGLA == "ES")

brasil_regioes_2020_pop_rua_sudeste$Região <- "Sudeste"
total_regiao_sudeste_2020 <- sum(brasil_regioes_2020_pop_rua_sudeste$freq)
brasil_regioes_2020_pop_rua_sudeste$`Total Região`  <- total_regiao_sudeste_2020

#

brasil_regioes_2020_pop_rua_sul <- subset(estados_total_2020, 
                                          subset = SIGLA == "RS" | SIGLA == "SC" | SIGLA == "PR")

brasil_regioes_2020_pop_rua_sul$Região <- "Sul"
total_regiao_sul_2020 <- sum(brasil_regioes_2020_pop_rua_sul$freq)
brasil_regioes_2020_pop_rua_sul$`Total Região`  <- total_regiao_sul_2020

#

brasil_regioes_2020_pop_rua_nordeste <- subset(estados_total_2020, 
                                               subset = SIGLA == "BA" | SIGLA == "PE" | SIGLA == "CE" | SIGLA == "AL" | SIGLA == "MA" | SIGLA == "PB" | SIGLA == "RN" | SIGLA == "PI" | SIGLA == "SE")

brasil_regioes_2020_pop_rua_nordeste$Região <- "Nordeste"
total_regiao_nordeste_2020 <- sum(brasil_regioes_2020_pop_rua_nordeste$freq)
brasil_regioes_2020_pop_rua_nordeste$`Total Região`  <- total_regiao_nordeste_2020

#

brasil_regioes_2020_pop_rua_norte <- subset(estados_total_2020, 
                                            subset = SIGLA == "AC" | SIGLA == "AP" | SIGLA == "AM" | SIGLA == "PA" | SIGLA == "RO" | SIGLA == "RR" | SIGLA == "TO")

brasil_regioes_2020_pop_rua_norte$Região <- "Norte"
total_regiao_norte_2020 <- sum(brasil_regioes_2020_pop_rua_norte$freq)
brasil_regioes_2020_pop_rua_norte$`Total Região`  <- total_regiao_norte_2020

#

brasil_regioes_2020_pop_rua_centro_oeste <- subset(estados_total_2020,
                                                   subset = SIGLA == "DF" | SIGLA == "GO" | SIGLA == "MT" | SIGLA == "MS")

brasil_regioes_2020_pop_rua_centro_oeste$Região <- "Centro-Oeste"
total_regiao_centro_oeste_2020 <- sum(brasil_regioes_2020_pop_rua_centro_oeste$freq)
brasil_regioes_2020_pop_rua_centro_oeste$`Total Região`  <- total_regiao_centro_oeste_2020

brasil_regioes_2020_pop_rua_soma <- rbind(
  brasil_regioes_2020_pop_rua_sudeste,
  brasil_regioes_2020_pop_rua_sul,
  brasil_regioes_2020_pop_rua_nordeste,
  brasil_regioes_2020_pop_rua_norte,
  brasil_regioes_2020_pop_rua_centro_oeste
)

munic_2020_pop_rua <- merge(brasil_regioes_2020_pop_rua_soma, munic_2020_pop_rua, by = "SIGLA")


### Tabela 2019 do Ministério das Cidades


brasil_munic_2019  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201912.csv", 
                               col_names = TRUE)
names(brasil_munic_2019)[1]<- "codigo_ibge"

total_2019 <- data.frame(count(brasil_munic_2019, "codigo_ibge"))


###10 Seleção de Dados por Sexo

test_2019 <- brasil_munic_2019 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test_2019$CO_SEXO_PESSOA[which(test_2019$CO_SEXO_PESSOA=="1")] <- "Masculino"
test_2019$CO_SEXO_PESSOA[which(test_2019$CO_SEXO_PESSOA=="2")] <- "Feminino"

munic_2019_pop_rua <- merge(test_2019, brasil, by = "codigo_ibge")
munic_2019_pop_rua $geometry <- NULL
munic_2019_pop_rua $Ano <- "2019"
munic_2019_pop_rua  <- na.omit(munic_2019_pop_rua )

total <- data.frame(count(brasil_munic_2019, "codigo_ibge"))
munic_2019_pop_rua  <- merge(munic_2019_pop_rua , total, by="codigo_ibge")
munic_2019_pop_rua $proporcao <- (munic_2019_pop_rua $n/munic_2019_pop_rua $freq)*100

#Brasil
brasil_total_2019 <-count(munic_2019_pop_rua , "codigo_ibge", "n")
brasil_total_2019 <- sum(brasil_total_2019$freq)
brasil_total_2019 <- data.frame(brasil_total_2019)
names(brasil_total_2019)[1] <- "Total Brasil"

munic_2019_pop_rua $`Total Brasil` <- brasil_total_2019$`Total Brasil`

#Estados
estados_total_2019 <-count(munic_2019_pop_rua, "SIGLA", "n")
munic_2019_pop_rua  <- merge(munic_2019_pop_rua, estados_total_2019, by = "SIGLA")


#Regiões

brasil_regioes_2019_pop_rua_sudeste <- subset(estados_total_2019, 
                                              subset = SIGLA == "SP" | SIGLA == "MG" | SIGLA == "RJ" | SIGLA == "ES")

brasil_regioes_2019_pop_rua_sudeste$Região <- "Sudeste"
total_regiao_sudeste_2019 <- sum(brasil_regioes_2019_pop_rua_sudeste$freq)
brasil_regioes_2019_pop_rua_sudeste$`Total Região`  <- total_regiao_sudeste_2019

#

brasil_regioes_2019_pop_rua_sul <- subset(estados_total_2019, 
                                          subset = SIGLA == "RS" | SIGLA == "SC" | SIGLA == "PR")

brasil_regioes_2019_pop_rua_sul$Região <- "Sul"
total_regiao_sul_2019 <- sum(brasil_regioes_2019_pop_rua_sul$freq)
brasil_regioes_2019_pop_rua_sul$`Total Região`  <- total_regiao_sul_2019

#

brasil_regioes_2019_pop_rua_nordeste <- subset(estados_total_2019, 
                                               subset = SIGLA == "BA" | SIGLA == "PE" | SIGLA == "CE" | SIGLA == "AL" | SIGLA == "MA" | SIGLA == "PB" | SIGLA == "RN" | SIGLA == "PI" | SIGLA == "SE")

brasil_regioes_2019_pop_rua_nordeste$Região <- "Nordeste"
total_regiao_nordeste_2019 <- sum(brasil_regioes_2019_pop_rua_nordeste$freq)
brasil_regioes_2019_pop_rua_nordeste$`Total Região`  <- total_regiao_nordeste_2019

#

brasil_regioes_2019_pop_rua_norte <- subset(estados_total_2019, 
                                            subset = SIGLA == "AC" | SIGLA == "AP" | SIGLA == "AM" | SIGLA == "PA" | SIGLA == "RO" | SIGLA == "RR" | SIGLA == "TO")

brasil_regioes_2019_pop_rua_norte$Região <- "Norte"
total_regiao_norte_2019 <- sum(brasil_regioes_2019_pop_rua_norte$freq)
brasil_regioes_2019_pop_rua_norte$`Total Região`  <- total_regiao_norte_2019

#

brasil_regioes_2019_pop_rua_centro_oeste <- subset(estados_total_2019,
                                                   subset = SIGLA == "DF" | SIGLA == "GO" | SIGLA == "MT" | SIGLA == "MS")

brasil_regioes_2019_pop_rua_centro_oeste$Região <- "Centro-Oeste"
total_regiao_centro_oeste_2019 <- sum(brasil_regioes_2019_pop_rua_centro_oeste$freq)
brasil_regioes_2019_pop_rua_centro_oeste$`Total Região`  <- total_regiao_centro_oeste_2019

brasil_regioes_2019_pop_rua_soma <- rbind(
  brasil_regioes_2019_pop_rua_sudeste,
  brasil_regioes_2019_pop_rua_sul,
  brasil_regioes_2019_pop_rua_nordeste,
  brasil_regioes_2019_pop_rua_norte,
  brasil_regioes_2019_pop_rua_centro_oeste
)

munic_2019_pop_rua <- merge(brasil_regioes_2019_pop_rua_soma, munic_2019_pop_rua, by = "SIGLA")


### Tabela 2018 do Ministério das Cidades

brasil_munic_2018  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201812.csv", 
                               col_names = TRUE)
names(brasil_munic_2018)[1]<- "codigo_ibge"

total_2018 <- data.frame(count(brasil_munic_2018, "codigo_ibge"))


###10 Seleção de Dados por Sexo

test_2018 <- brasil_munic_2018 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test_2018$CO_SEXO_PESSOA[which(test_2018$CO_SEXO_PESSOA=="1")] <- "Masculino"
test_2018$CO_SEXO_PESSOA[which(test_2018$CO_SEXO_PESSOA=="2")] <- "Feminino"

munic_2018_pop_rua <- merge(test_2018, brasil, by = "codigo_ibge")
munic_2018_pop_rua $geometry <- NULL
munic_2018_pop_rua $Ano <- "2018"
munic_2018_pop_rua  <- na.omit(munic_2018_pop_rua )

total <- data.frame(count(brasil_munic_2018, "codigo_ibge"))
munic_2018_pop_rua  <- merge(munic_2018_pop_rua , total, by="codigo_ibge")
munic_2018_pop_rua $proporcao <- (munic_2018_pop_rua $n/munic_2018_pop_rua $freq)*100

#Brasil
brasil_total_2018 <-count(munic_2018_pop_rua , "codigo_ibge", "n")
brasil_total_2018 <- sum(brasil_total_2018$freq)
brasil_total_2018 <- data.frame(brasil_total_2018)
names(brasil_total_2018)[1] <- "Total Brasil"

munic_2018_pop_rua $`Total Brasil` <- brasil_total_2018$`Total Brasil`

#Estados
estados_total_2018 <-count(munic_2018_pop_rua, "SIGLA", "n")
munic_2018_pop_rua  <- merge(munic_2018_pop_rua, estados_total_2018, by = "SIGLA")


#Regiões

brasil_regioes_2018_pop_rua_sudeste <- subset(estados_total_2018, 
                                              subset = SIGLA == "SP" | SIGLA == "MG" | SIGLA == "RJ" | SIGLA == "ES")

brasil_regioes_2018_pop_rua_sudeste$Região <- "Sudeste"
total_regiao_sudeste_2018 <- sum(brasil_regioes_2018_pop_rua_sudeste$freq)
brasil_regioes_2018_pop_rua_sudeste$`Total Região`  <- total_regiao_sudeste_2018

#

brasil_regioes_2018_pop_rua_sul <- subset(estados_total_2018, 
                                          subset = SIGLA == "RS" | SIGLA == "SC" | SIGLA == "PR")

brasil_regioes_2018_pop_rua_sul$Região <- "Sul"
total_regiao_sul_2018 <- sum(brasil_regioes_2018_pop_rua_sul$freq)
brasil_regioes_2018_pop_rua_sul$`Total Região`  <- total_regiao_sul_2018

#

brasil_regioes_2018_pop_rua_nordeste <- subset(estados_total_2018, 
                                               subset = SIGLA == "BA" | SIGLA == "PE" | SIGLA == "CE" | SIGLA == "AL" | SIGLA == "MA" | SIGLA == "PB" | SIGLA == "RN" | SIGLA == "PI" | SIGLA == "SE")

brasil_regioes_2018_pop_rua_nordeste$Região <- "Nordeste"
total_regiao_nordeste_2018 <- sum(brasil_regioes_2018_pop_rua_nordeste$freq)
brasil_regioes_2018_pop_rua_nordeste$`Total Região`  <- total_regiao_nordeste_2018

#

brasil_regioes_2018_pop_rua_norte <- subset(estados_total_2018, 
                                            subset = SIGLA == "AC" | SIGLA == "AP" | SIGLA == "AM" | SIGLA == "PA" | SIGLA == "RO" | SIGLA == "RR" | SIGLA == "TO")

brasil_regioes_2018_pop_rua_norte$Região <- "Norte"
total_regiao_norte_2018 <- sum(brasil_regioes_2018_pop_rua_norte$freq)
brasil_regioes_2018_pop_rua_norte$`Total Região`  <- total_regiao_norte_2018

#

brasil_regioes_2018_pop_rua_centro_oeste <- subset(estados_total_2018,
                                                   subset = SIGLA == "DF" | SIGLA == "GO" | SIGLA == "MT" | SIGLA == "MS")

brasil_regioes_2018_pop_rua_centro_oeste$Região <- "Centro-Oeste"
total_regiao_centro_oeste_2018 <- sum(brasil_regioes_2018_pop_rua_centro_oeste$freq)
brasil_regioes_2018_pop_rua_centro_oeste$`Total Região`  <- total_regiao_centro_oeste_2018

brasil_regioes_2018_pop_rua_soma <- rbind(
  brasil_regioes_2018_pop_rua_sudeste,
  brasil_regioes_2018_pop_rua_sul,
  brasil_regioes_2018_pop_rua_nordeste,
  brasil_regioes_2018_pop_rua_norte,
  brasil_regioes_2018_pop_rua_centro_oeste
)

munic_2018_pop_rua <- merge(brasil_regioes_2018_pop_rua_soma, munic_2018_pop_rua, by = "SIGLA")



### Tabela 2017 do Ministério das Cidades

brasil_munic_2017  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201712.csv", 
                               col_names = TRUE)
names(brasil_munic_2017)[1]<- "codigo_ibge"

total_2017 <- data.frame(count(brasil_munic_2017, "codigo_ibge"))


###10 Seleção de Dados por Sexo

test_2017 <- brasil_munic_2017 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test_2017$CO_SEXO_PESSOA[which(test_2017$CO_SEXO_PESSOA=="1")] <- "Masculino"
test_2017$CO_SEXO_PESSOA[which(test_2017$CO_SEXO_PESSOA=="2")] <- "Feminino"

munic_2017_pop_rua <- merge(test_2017, brasil, by = "codigo_ibge")
munic_2017_pop_rua $geometry <- NULL
munic_2017_pop_rua $Ano <- "2017"
munic_2017_pop_rua  <- na.omit(munic_2017_pop_rua )

total <- data.frame(count(brasil_munic_2017, "codigo_ibge"))
munic_2017_pop_rua  <- merge(munic_2017_pop_rua , total, by="codigo_ibge")
munic_2017_pop_rua $proporcao <- (munic_2017_pop_rua $n/munic_2017_pop_rua $freq)*100

#Brasil
brasil_total_2017 <-count(munic_2017_pop_rua , "codigo_ibge", "n")
brasil_total_2017 <- sum(brasil_total_2017$freq)
brasil_total_2017 <- data.frame(brasil_total_2017)
names(brasil_total_2017)[1] <- "Total Brasil"

munic_2017_pop_rua $`Total Brasil` <- brasil_total_2017$`Total Brasil`

#Estados
estados_total_2017 <-count(munic_2017_pop_rua, "SIGLA", "n")
munic_2017_pop_rua  <- merge(munic_2017_pop_rua, estados_total_2017, by = "SIGLA")


#Regiões

brasil_regioes_2017_pop_rua_sudeste <- subset(estados_total_2017, 
                                              subset = SIGLA == "SP" | SIGLA == "MG" | SIGLA == "RJ" | SIGLA == "ES")

brasil_regioes_2017_pop_rua_sudeste$Região <- "Sudeste"
total_regiao_sudeste_2017 <- sum(brasil_regioes_2017_pop_rua_sudeste$freq)
brasil_regioes_2017_pop_rua_sudeste$`Total Região`  <- total_regiao_sudeste_2017

#

brasil_regioes_2017_pop_rua_sul <- subset(estados_total_2017, 
                                          subset = SIGLA == "RS" | SIGLA == "SC" | SIGLA == "PR")

brasil_regioes_2017_pop_rua_sul$Região <- "Sul"
total_regiao_sul_2017 <- sum(brasil_regioes_2017_pop_rua_sul$freq)
brasil_regioes_2017_pop_rua_sul$`Total Região`  <- total_regiao_sul_2017

#

brasil_regioes_2017_pop_rua_nordeste <- subset(estados_total_2017, 
                                               subset = SIGLA == "BA" | SIGLA == "PE" | SIGLA == "CE" | SIGLA == "AL" | SIGLA == "MA" | SIGLA == "PB" | SIGLA == "RN" | SIGLA == "PI" | SIGLA == "SE")

brasil_regioes_2017_pop_rua_nordeste$Região <- "Nordeste"
total_regiao_nordeste_2017 <- sum(brasil_regioes_2017_pop_rua_nordeste$freq)
brasil_regioes_2017_pop_rua_nordeste$`Total Região`  <- total_regiao_nordeste_2017

#

brasil_regioes_2017_pop_rua_norte <- subset(estados_total_2017, 
                                            subset = SIGLA == "AC" | SIGLA == "AP" | SIGLA == "AM" | SIGLA == "PA" | SIGLA == "RO" | SIGLA == "RR" | SIGLA == "TO")

brasil_regioes_2017_pop_rua_norte$Região <- "Norte"
total_regiao_norte_2017 <- sum(brasil_regioes_2017_pop_rua_norte$freq)
brasil_regioes_2017_pop_rua_norte$`Total Região`  <- total_regiao_norte_2017

#

brasil_regioes_2017_pop_rua_centro_oeste <- subset(estados_total_2017,
                                                   subset = SIGLA == "DF" | SIGLA == "GO" | SIGLA == "MT" | SIGLA == "MS")

brasil_regioes_2017_pop_rua_centro_oeste$Região <- "Centro-Oeste"
total_regiao_centro_oeste_2017 <- sum(brasil_regioes_2017_pop_rua_centro_oeste$freq)
brasil_regioes_2017_pop_rua_centro_oeste$`Total Região`  <- total_regiao_centro_oeste_2017

brasil_regioes_2017_pop_rua_soma <- rbind(
  brasil_regioes_2017_pop_rua_sudeste,
  brasil_regioes_2017_pop_rua_sul,
  brasil_regioes_2017_pop_rua_nordeste,
  brasil_regioes_2017_pop_rua_norte,
  brasil_regioes_2017_pop_rua_centro_oeste
)

munic_2017_pop_rua <- merge(brasil_regioes_2017_pop_rua_soma, munic_2017_pop_rua, by = "SIGLA")


### Tabela 2016 do Ministério das Cidades

brasil_munic_2016  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201612.csv", 
                               col_names = TRUE)
names(brasil_munic_2016)[1]<- "codigo_ibge"

total_2016 <- data.frame(count(brasil_munic_2016, "codigo_ibge"))


###10 Seleção de Dados por Sexo

test_2016 <- brasil_munic_2016 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test_2016$CO_SEXO_PESSOA[which(test_2016$CO_SEXO_PESSOA=="1")] <- "Masculino"
test_2016$CO_SEXO_PESSOA[which(test_2016$CO_SEXO_PESSOA=="2")] <- "Feminino"

munic_2016_pop_rua <- merge(test_2016, brasil, by = "codigo_ibge")
munic_2016_pop_rua $geometry <- NULL
munic_2016_pop_rua $Ano <- "2016"
munic_2016_pop_rua  <- na.omit(munic_2016_pop_rua )

total <- data.frame(count(brasil_munic_2016, "codigo_ibge"))
munic_2016_pop_rua  <- merge(munic_2016_pop_rua , total, by="codigo_ibge")
munic_2016_pop_rua $proporcao <- (munic_2016_pop_rua $n/munic_2016_pop_rua $freq)*100

#Brasil
brasil_total_2016 <-count(munic_2016_pop_rua , "codigo_ibge", "n")
brasil_total_2016 <- sum(brasil_total_2016$freq)
brasil_total_2016 <- data.frame(brasil_total_2016)
names(brasil_total_2016)[1] <- "Total Brasil"

munic_2016_pop_rua $`Total Brasil` <- brasil_total_2016$`Total Brasil`

#Estados
estados_total_2016 <-count(munic_2016_pop_rua, "SIGLA", "n")
munic_2016_pop_rua  <- merge(munic_2016_pop_rua, estados_total_2016, by = "SIGLA")


#Regiões

brasil_regioes_2016_pop_rua_sudeste <- subset(estados_total_2016, 
                                              subset = SIGLA == "SP" | SIGLA == "MG" | SIGLA == "RJ" | SIGLA == "ES")

brasil_regioes_2016_pop_rua_sudeste$Região <- "Sudeste"
total_regiao_sudeste_2016 <- sum(brasil_regioes_2016_pop_rua_sudeste$freq)
brasil_regioes_2016_pop_rua_sudeste$`Total Região`  <- total_regiao_sudeste_2016

#

brasil_regioes_2016_pop_rua_sul <- subset(estados_total_2016, 
                                          subset = SIGLA == "RS" | SIGLA == "SC" | SIGLA == "PR")

brasil_regioes_2016_pop_rua_sul$Região <- "Sul"
total_regiao_sul_2016 <- sum(brasil_regioes_2016_pop_rua_sul$freq)
brasil_regioes_2016_pop_rua_sul$`Total Região`  <- total_regiao_sul_2016

#

brasil_regioes_2016_pop_rua_nordeste <- subset(estados_total_2016, 
                                               subset = SIGLA == "BA" | SIGLA == "PE" | SIGLA == "CE" | SIGLA == "AL" | SIGLA == "MA" | SIGLA == "PB" | SIGLA == "RN" | SIGLA == "PI" | SIGLA == "SE")

brasil_regioes_2016_pop_rua_nordeste$Região <- "Nordeste"
total_regiao_nordeste_2016 <- sum(brasil_regioes_2016_pop_rua_nordeste$freq)
brasil_regioes_2016_pop_rua_nordeste$`Total Região`  <- total_regiao_nordeste_2016

#

brasil_regioes_2016_pop_rua_norte <- subset(estados_total_2016, 
                                            subset = SIGLA == "AC" | SIGLA == "AP" | SIGLA == "AM" | SIGLA == "PA" | SIGLA == "RO" | SIGLA == "RR" | SIGLA == "TO")

brasil_regioes_2016_pop_rua_norte$Região <- "Norte"
total_regiao_norte_2016 <- sum(brasil_regioes_2016_pop_rua_norte$freq)
brasil_regioes_2016_pop_rua_norte$`Total Região`  <- total_regiao_norte_2016

#

brasil_regioes_2016_pop_rua_centro_oeste <- subset(estados_total_2016,
                                                   subset = SIGLA == "DF" | SIGLA == "GO" | SIGLA == "MT" | SIGLA == "MS")

brasil_regioes_2016_pop_rua_centro_oeste$Região <- "Centro-Oeste"
total_regiao_centro_oeste_2016 <- sum(brasil_regioes_2016_pop_rua_centro_oeste$freq)
brasil_regioes_2016_pop_rua_centro_oeste$`Total Região`  <- total_regiao_centro_oeste_2016

brasil_regioes_2016_pop_rua_soma <- rbind(
  brasil_regioes_2016_pop_rua_sudeste,
  brasil_regioes_2016_pop_rua_sul,
  brasil_regioes_2016_pop_rua_nordeste,
  brasil_regioes_2016_pop_rua_norte,
  brasil_regioes_2016_pop_rua_centro_oeste
)

munic_2016_pop_rua <- merge(brasil_regioes_2016_pop_rua_soma, munic_2016_pop_rua, by = "SIGLA")


### Tabela 2015 do Ministério das Cidades

brasil_munic_2015  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201512.csv", 
                               col_names = TRUE)
names(brasil_munic_2015)[1]<- "codigo_ibge"

total_2015 <- data.frame(count(brasil_munic_2015, "codigo_ibge"))


###10 Seleção de Dados por Sexo

test_2015 <- brasil_munic_2015 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test_2015$CO_SEXO_PESSOA[which(test_2015$CO_SEXO_PESSOA=="1")] <- "Masculino"
test_2015$CO_SEXO_PESSOA[which(test_2015$CO_SEXO_PESSOA=="2")] <- "Feminino"

munic_2015_pop_rua <- merge(test_2015, brasil, by = "codigo_ibge")
munic_2015_pop_rua $geometry <- NULL
munic_2015_pop_rua $Ano <- "2015"
munic_2015_pop_rua  <- na.omit(munic_2015_pop_rua )

total <- data.frame(count(brasil_munic_2015, "codigo_ibge"))
munic_2015_pop_rua  <- merge(munic_2015_pop_rua , total, by="codigo_ibge")
munic_2015_pop_rua $proporcao <- (munic_2015_pop_rua $n/munic_2015_pop_rua $freq)*100

#Brasil
brasil_total_2015 <-count(munic_2015_pop_rua , "codigo_ibge", "n")
brasil_total_2015 <- sum(brasil_total_2015$freq)
brasil_total_2015 <- data.frame(brasil_total_2015)
names(brasil_total_2015)[1] <- "Total Brasil"

munic_2015_pop_rua $`Total Brasil` <- brasil_total_2015$`Total Brasil`

#Estados
estados_total_2015 <-count(munic_2015_pop_rua, "SIGLA", "n")
munic_2015_pop_rua  <- merge(munic_2015_pop_rua, estados_total_2015, by = "SIGLA")


#Regiões

brasil_regioes_2015_pop_rua_sudeste <- subset(estados_total_2015, 
                                              subset = SIGLA == "SP" | SIGLA == "MG" | SIGLA == "RJ" | SIGLA == "ES")

brasil_regioes_2015_pop_rua_sudeste$Região <- "Sudeste"
total_regiao_sudeste_2015 <- sum(brasil_regioes_2015_pop_rua_sudeste$freq)
brasil_regioes_2015_pop_rua_sudeste$`Total Região`  <- total_regiao_sudeste_2015

#

brasil_regioes_2015_pop_rua_sul <- subset(estados_total_2015, 
                                          subset = SIGLA == "RS" | SIGLA == "SC" | SIGLA == "PR")

brasil_regioes_2015_pop_rua_sul$Região <- "Sul"
total_regiao_sul_2015 <- sum(brasil_regioes_2015_pop_rua_sul$freq)
brasil_regioes_2015_pop_rua_sul$`Total Região`  <- total_regiao_sul_2015

#

brasil_regioes_2015_pop_rua_nordeste <- subset(estados_total_2015, 
                                               subset = SIGLA == "BA" | SIGLA == "PE" | SIGLA == "CE" | SIGLA == "AL" | SIGLA == "MA" | SIGLA == "PB" | SIGLA == "RN" | SIGLA == "PI" | SIGLA == "SE")

brasil_regioes_2015_pop_rua_nordeste$Região <- "Nordeste"
total_regiao_nordeste_2015 <- sum(brasil_regioes_2015_pop_rua_nordeste$freq)
brasil_regioes_2015_pop_rua_nordeste$`Total Região`  <- total_regiao_nordeste_2015

#

brasil_regioes_2015_pop_rua_norte <- subset(estados_total_2015, 
                                            subset = SIGLA == "AC" | SIGLA == "AP" | SIGLA == "AM" | SIGLA == "PA" | SIGLA == "RO" | SIGLA == "RR" | SIGLA == "TO")

brasil_regioes_2015_pop_rua_norte$Região <- "Norte"
total_regiao_norte_2015 <- sum(brasil_regioes_2015_pop_rua_norte$freq)
brasil_regioes_2015_pop_rua_norte$`Total Região`  <- total_regiao_norte_2015

#

brasil_regioes_2015_pop_rua_centro_oeste <- subset(estados_total_2015,
                                                   subset = SIGLA == "DF" | SIGLA == "GO" | SIGLA == "MT" | SIGLA == "MS")

brasil_regioes_2015_pop_rua_centro_oeste$Região <- "Centro-Oeste"
total_regiao_centro_oeste_2015 <- sum(brasil_regioes_2015_pop_rua_centro_oeste$freq)
brasil_regioes_2015_pop_rua_centro_oeste$`Total Região`  <- total_regiao_centro_oeste_2015

brasil_regioes_2015_pop_rua_soma <- rbind(
  brasil_regioes_2015_pop_rua_sudeste,
  brasil_regioes_2015_pop_rua_sul,
  brasil_regioes_2015_pop_rua_nordeste,
  brasil_regioes_2015_pop_rua_norte,
  brasil_regioes_2015_pop_rua_centro_oeste
)

munic_2015_pop_rua <- merge(brasil_regioes_2015_pop_rua_soma, munic_2015_pop_rua, by = "SIGLA")


### Tabela 2014 do Ministério das Cidades

brasil_munic_2014  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201412.csv", 
                               col_names = TRUE)
names(brasil_munic_2014)[1]<- "codigo_ibge"

total_2014 <- data.frame(count(brasil_munic_2014, "codigo_ibge"))


###10 Seleção de Dados por Sexo

test_2014 <- brasil_munic_2014 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test_2014$CO_SEXO_PESSOA[which(test_2014$CO_SEXO_PESSOA=="1")] <- "Masculino"
test_2014$CO_SEXO_PESSOA[which(test_2014$CO_SEXO_PESSOA=="2")] <- "Feminino"

munic_2014_pop_rua <- merge(test_2014, brasil, by = "codigo_ibge")
munic_2014_pop_rua $geometry <- NULL
munic_2014_pop_rua $Ano <- "2014"
munic_2014_pop_rua  <- na.omit(munic_2014_pop_rua )

total <- data.frame(count(brasil_munic_2014, "codigo_ibge"))
munic_2014_pop_rua  <- merge(munic_2014_pop_rua , total, by="codigo_ibge")
munic_2014_pop_rua $proporcao <- (munic_2014_pop_rua $n/munic_2014_pop_rua $freq)*100

#Brasil
brasil_total_2014 <-count(munic_2014_pop_rua , "codigo_ibge", "n")
brasil_total_2014 <- sum(brasil_total_2014$freq)
brasil_total_2014 <- data.frame(brasil_total_2014)
names(brasil_total_2014)[1] <- "Total Brasil"

munic_2014_pop_rua $`Total Brasil` <- brasil_total_2014$`Total Brasil`

#Estados
estados_total_2014 <-count(munic_2014_pop_rua, "SIGLA", "n")
munic_2014_pop_rua  <- merge(munic_2014_pop_rua, estados_total_2014, by = "SIGLA")


#Regiões

brasil_regioes_2014_pop_rua_sudeste <- subset(estados_total_2014, 
                                              subset = SIGLA == "SP" | SIGLA == "MG" | SIGLA == "RJ" | SIGLA == "ES")

brasil_regioes_2014_pop_rua_sudeste$Região <- "Sudeste"
total_regiao_sudeste_2014 <- sum(brasil_regioes_2014_pop_rua_sudeste$freq)
brasil_regioes_2014_pop_rua_sudeste$`Total Região`  <- total_regiao_sudeste_2014

#

brasil_regioes_2014_pop_rua_sul <- subset(estados_total_2014, 
                                          subset = SIGLA == "RS" | SIGLA == "SC" | SIGLA == "PR")

brasil_regioes_2014_pop_rua_sul$Região <- "Sul"
total_regiao_sul_2014 <- sum(brasil_regioes_2014_pop_rua_sul$freq)
brasil_regioes_2014_pop_rua_sul$`Total Região`  <- total_regiao_sul_2014

#

brasil_regioes_2014_pop_rua_nordeste <- subset(estados_total_2014, 
                                               subset = SIGLA == "BA" | SIGLA == "PE" | SIGLA == "CE" | SIGLA == "AL" | SIGLA == "MA" | SIGLA == "PB" | SIGLA == "RN" | SIGLA == "PI" | SIGLA == "SE")

brasil_regioes_2014_pop_rua_nordeste$Região <- "Nordeste"
total_regiao_nordeste_2014 <- sum(brasil_regioes_2014_pop_rua_nordeste$freq)
brasil_regioes_2014_pop_rua_nordeste$`Total Região`  <- total_regiao_nordeste_2014

#

brasil_regioes_2014_pop_rua_norte <- subset(estados_total_2014, 
                                            subset = SIGLA == "AC" | SIGLA == "AP" | SIGLA == "AM" | SIGLA == "PA" | SIGLA == "RO" | SIGLA == "RR" | SIGLA == "TO")

brasil_regioes_2014_pop_rua_norte$Região <- "Norte"
total_regiao_norte_2014 <- sum(brasil_regioes_2014_pop_rua_norte$freq)
brasil_regioes_2014_pop_rua_norte$`Total Região`  <- total_regiao_norte_2014

#

brasil_regioes_2014_pop_rua_centro_oeste <- subset(estados_total_2014,
                                                   subset = SIGLA == "DF" | SIGLA == "GO" | SIGLA == "MT" | SIGLA == "MS")

brasil_regioes_2014_pop_rua_centro_oeste$Região <- "Centro-Oeste"
total_regiao_centro_oeste_2014 <- sum(brasil_regioes_2014_pop_rua_centro_oeste$freq)
brasil_regioes_2014_pop_rua_centro_oeste$`Total Região`  <- total_regiao_centro_oeste_2014

brasil_regioes_2014_pop_rua_soma <- rbind(
  brasil_regioes_2014_pop_rua_sudeste,
  brasil_regioes_2014_pop_rua_sul,
  brasil_regioes_2014_pop_rua_nordeste,
  brasil_regioes_2014_pop_rua_norte,
  brasil_regioes_2014_pop_rua_centro_oeste
)

munic_2014_pop_rua <- merge(brasil_regioes_2014_pop_rua_soma, munic_2014_pop_rua, by = "SIGLA")


### Tabela 2013 do Ministério das Cidades

brasil_munic_2013  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201312.csv", 
                               col_names = TRUE)
names(brasil_munic_2013)[1]<- "codigo_ibge"

total_2013 <- data.frame(count(brasil_munic_2013, "codigo_ibge"))


###10 Seleção de Dados por Sexo

test_2013 <- brasil_munic_2013 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test_2013$CO_SEXO_PESSOA[which(test_2013$CO_SEXO_PESSOA=="1")] <- "Masculino"
test_2013$CO_SEXO_PESSOA[which(test_2013$CO_SEXO_PESSOA=="2")] <- "Feminino"

munic_2013_pop_rua <- merge(test_2013, brasil, by = "codigo_ibge")
munic_2013_pop_rua $geometry <- NULL
munic_2013_pop_rua $Ano <- "2013"
munic_2013_pop_rua  <- na.omit(munic_2013_pop_rua )

total <- data.frame(count(brasil_munic_2013, "codigo_ibge"))
munic_2013_pop_rua  <- merge(munic_2013_pop_rua , total, by="codigo_ibge")
munic_2013_pop_rua $proporcao <- (munic_2013_pop_rua $n/munic_2013_pop_rua $freq)*100

#Brasil
brasil_total_2013 <-count(munic_2013_pop_rua , "codigo_ibge", "n")
brasil_total_2013 <- sum(brasil_total_2013$freq)
brasil_total_2013 <- data.frame(brasil_total_2013)
names(brasil_total_2013)[1] <- "Total Brasil"

munic_2013_pop_rua $`Total Brasil` <- brasil_total_2013$`Total Brasil`

#Estados
estados_total_2013 <-count(munic_2013_pop_rua, "SIGLA", "n")
munic_2013_pop_rua  <- merge(munic_2013_pop_rua, estados_total_2013, by = "SIGLA")


#Regiões

brasil_regioes_2013_pop_rua_sudeste <- subset(estados_total_2013, 
                                              subset = SIGLA == "SP" | SIGLA == "MG" | SIGLA == "RJ" | SIGLA == "ES")

brasil_regioes_2013_pop_rua_sudeste$Região <- "Sudeste"
total_regiao_sudeste_2013 <- sum(brasil_regioes_2013_pop_rua_sudeste$freq)
brasil_regioes_2013_pop_rua_sudeste$`Total Região`  <- total_regiao_sudeste_2013

#

brasil_regioes_2013_pop_rua_sul <- subset(estados_total_2013, 
                                          subset = SIGLA == "RS" | SIGLA == "SC" | SIGLA == "PR")

brasil_regioes_2013_pop_rua_sul$Região <- "Sul"
total_regiao_sul_2013 <- sum(brasil_regioes_2013_pop_rua_sul$freq)
brasil_regioes_2013_pop_rua_sul$`Total Região`  <- total_regiao_sul_2013

#

brasil_regioes_2013_pop_rua_nordeste <- subset(estados_total_2013, 
                                               subset = SIGLA == "BA" | SIGLA == "PE" | SIGLA == "CE" | SIGLA == "AL" | SIGLA == "MA" | SIGLA == "PB" | SIGLA == "RN" | SIGLA == "PI" | SIGLA == "SE")

brasil_regioes_2013_pop_rua_nordeste$Região <- "Nordeste"
total_regiao_nordeste_2013 <- sum(brasil_regioes_2013_pop_rua_nordeste$freq)
brasil_regioes_2013_pop_rua_nordeste$`Total Região`  <- total_regiao_nordeste_2013

#

brasil_regioes_2013_pop_rua_norte <- subset(estados_total_2013, 
                                            subset = SIGLA == "AC" | SIGLA == "AP" | SIGLA == "AM" | SIGLA == "PA" | SIGLA == "RO" | SIGLA == "RR" | SIGLA == "TO")

brasil_regioes_2013_pop_rua_norte$Região <- "Norte"
total_regiao_norte_2013 <- sum(brasil_regioes_2013_pop_rua_norte$freq)
brasil_regioes_2013_pop_rua_norte$`Total Região`  <- total_regiao_norte_2013

#

brasil_regioes_2013_pop_rua_centro_oeste <- subset(estados_total_2013,
                                                   subset = SIGLA == "DF" | SIGLA == "GO" | SIGLA == "MT" | SIGLA == "MS")

brasil_regioes_2013_pop_rua_centro_oeste$Região <- "Centro-Oeste"
total_regiao_centro_oeste_2013 <- sum(brasil_regioes_2013_pop_rua_centro_oeste$freq)
brasil_regioes_2013_pop_rua_centro_oeste$`Total Região`  <- total_regiao_centro_oeste_2013

brasil_regioes_2013_pop_rua_soma <- rbind(
  brasil_regioes_2013_pop_rua_sudeste,
  brasil_regioes_2013_pop_rua_sul,
  brasil_regioes_2013_pop_rua_nordeste,
  brasil_regioes_2013_pop_rua_norte,
  brasil_regioes_2013_pop_rua_centro_oeste
)

munic_2013_pop_rua <- merge(brasil_regioes_2013_pop_rua_soma, munic_2013_pop_rua, by = "SIGLA")


### Tabela 2012 do Ministério das Cidades


brasil_munic_2012  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201212.csv", 
                               col_names = TRUE)
names(brasil_munic_2012)[1]<- "codigo_ibge"

total_2012 <- data.frame(count(brasil_munic_2012, "codigo_ibge"))


###10 Seleção de Dados por Sexo

test_2012 <- brasil_munic_2012 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test_2012$CO_SEXO_PESSOA[which(test_2012$CO_SEXO_PESSOA=="1")] <- "Masculino"
test_2012$CO_SEXO_PESSOA[which(test_2012$CO_SEXO_PESSOA=="2")] <- "Feminino"

munic_2012_pop_rua <- merge(test_2012, brasil, by = "codigo_ibge")
munic_2012_pop_rua $geometry <- NULL
munic_2012_pop_rua $Ano <- "2012"
munic_2012_pop_rua  <- na.omit(munic_2012_pop_rua )

total <- data.frame(count(brasil_munic_2012, "codigo_ibge"))
munic_2012_pop_rua  <- merge(munic_2012_pop_rua , total, by="codigo_ibge")
munic_2012_pop_rua $proporcao <- (munic_2012_pop_rua $n/munic_2012_pop_rua $freq)*100

#Brasil
brasil_total_2012 <-count(munic_2012_pop_rua , "codigo_ibge", "n")
brasil_total_2012 <- sum(brasil_total_2012$freq)
brasil_total_2012 <- data.frame(brasil_total_2012)
names(brasil_total_2012)[1] <- "Total Brasil"

munic_2012_pop_rua $`Total Brasil` <- brasil_total_2012$`Total Brasil`

#Estados
estados_total_2012 <-count(munic_2012_pop_rua, "SIGLA", "n")
munic_2012_pop_rua  <- merge(munic_2012_pop_rua, estados_total_2012, by = "SIGLA")


#Regiões

brasil_regioes_2012_pop_rua_sudeste <- subset(estados_total_2012, 
                                              subset = SIGLA == "SP" | SIGLA == "MG" | SIGLA == "RJ" | SIGLA == "ES")

brasil_regioes_2012_pop_rua_sudeste$Região <- "Sudeste"
total_regiao_sudeste_2012 <- sum(brasil_regioes_2012_pop_rua_sudeste$freq)
brasil_regioes_2012_pop_rua_sudeste$`Total Região`  <- total_regiao_sudeste_2012

#

brasil_regioes_2012_pop_rua_sul <- subset(estados_total_2012, 
                                          subset = SIGLA == "RS" | SIGLA == "SC" | SIGLA == "PR")

brasil_regioes_2012_pop_rua_sul$Região <- "Sul"
total_regiao_sul_2012 <- sum(brasil_regioes_2012_pop_rua_sul$freq)
brasil_regioes_2012_pop_rua_sul$`Total Região`  <- total_regiao_sul_2012

#

brasil_regioes_2012_pop_rua_nordeste <- subset(estados_total_2012, 
                                               subset = SIGLA == "BA" | SIGLA == "PE" | SIGLA == "CE" | SIGLA == "AL" | SIGLA == "MA" | SIGLA == "PB" | SIGLA == "RN" | SIGLA == "PI" | SIGLA == "SE")

brasil_regioes_2012_pop_rua_nordeste$Região <- "Nordeste"
total_regiao_nordeste_2012 <- sum(brasil_regioes_2012_pop_rua_nordeste$freq)
brasil_regioes_2012_pop_rua_nordeste$`Total Região`  <- total_regiao_nordeste_2012

#

brasil_regioes_2012_pop_rua_norte <- subset(estados_total_2012, 
                                            subset = SIGLA == "AC" | SIGLA == "AP" | SIGLA == "AM" | SIGLA == "PA" | SIGLA == "RO" | SIGLA == "RR" | SIGLA == "TO")

brasil_regioes_2012_pop_rua_norte$Região <- "Norte"
total_regiao_norte_2012 <- sum(brasil_regioes_2012_pop_rua_norte$freq)
brasil_regioes_2012_pop_rua_norte$`Total Região`  <- total_regiao_norte_2012

#

brasil_regioes_2012_pop_rua_centro_oeste <- subset(estados_total_2012,
                                                   subset = SIGLA == "DF" | SIGLA == "GO" | SIGLA == "MT" | SIGLA == "MS")

brasil_regioes_2012_pop_rua_centro_oeste$Região <- "Centro-Oeste"
total_regiao_centro_oeste_2012 <- sum(brasil_regioes_2012_pop_rua_centro_oeste$freq)
brasil_regioes_2012_pop_rua_centro_oeste$`Total Região`  <- total_regiao_centro_oeste_2012

brasil_regioes_2012_pop_rua_soma <- rbind(
  brasil_regioes_2012_pop_rua_sudeste,
  brasil_regioes_2012_pop_rua_sul,
  brasil_regioes_2012_pop_rua_nordeste,
  brasil_regioes_2012_pop_rua_norte,
  brasil_regioes_2012_pop_rua_centro_oeste
)

munic_2012_pop_rua <- merge(brasil_regioes_2012_pop_rua_soma, munic_2012_pop_rua, by = "SIGLA")


##########


serie_historica_brasil_estados_regioes_muni <- rbind(munic_2021_pop_rua,
                                        munic_2020_pop_rua,
                                        munic_2019_pop_rua, 
                                        munic_2018_pop_rua, 
                                        munic_2017_pop_rua, 
                                        munic_2016_pop_rua, 
                                        munic_2015_pop_rua, 
                                        munic_2014_pop_rua, 
                                        munic_2013_pop_rua, 
                                        munic_2012_pop_rua)

serie_historica_brasil_estados_regioes_muni$AREA_KM2 <- NULL
serie_historica_brasil_estados_regioes_muni$freq.y <- NULL
serie_historica_brasil_estados_regioes_muni$País <- "Brasil"
serie_historica_brasil_estados_regioes_muni <- serie_historica_brasil_estados_regioes_muni[,c(9, 13, 12, 3, 4, 1, 2, 8, 10, 6, 7, 11)]

names(serie_historica_brasil_estados_regioes_muni)[3] <- "Total Brasil por Ano"
names(serie_historica_brasil_estados_regioes_muni)[5] <- "Total Região por Ano"
names(serie_historica_brasil_estados_regioes_muni)[6] <- "Estado (Sigla)"
names(serie_historica_brasil_estados_regioes_muni)[7] <- "Total Estado por Ano"
names(serie_historica_brasil_estados_regioes_muni)[8] <- "Município"
names(serie_historica_brasil_estados_regioes_muni)[9] <- "Total Município por Ano"
names(serie_historica_brasil_estados_regioes_muni)[10] <- "Perfil (Sexo)"
names(serie_historica_brasil_estados_regioes_muni)[11] <- "Total Perfil (Sexo)"
names(serie_historica_brasil_estados_regioes_muni)[12] <- "Proporção Perfil (Sexo)"

serie_historica <- serie_historica_brasil_estados_regioes_muni[with(serie_historica_brasil_estados_regioes_muni, order(serie_historica_brasil_estados_regioes_muni$Município)), ]

############# Brasil


write_xlsx(serie_historica, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/serie_historica_brasil_regioes_estados_municipios.xlsx")
