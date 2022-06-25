library(readxl)
library(dplyr)
library(plyr)
library(stringr)
library(readr)
library(tidyverse)
library(writexl)
library(RColorBrewer)
library(xlsx)
library(sf)



### Brasil Shapefile

brasil <- read_sf("/Users/wemigliari/Documents/R/data/BR_Municipios_2021.shp")
names(brasil)[1] <- "codigo_ibge"

brasil$geometry <- NULL


### Tabela 2021 do Ministério das Cidades

brasil_munic_2021  <- read_excel("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_202112_abril.xlsx", 
                                 col_names = TRUE)

names(brasil_munic_2021)[4]<- "codigo_ibge"

munic_2021_pop_rua <- merge(brasil_munic_2021, brasil, by = "codigo_ibge")
munic_2021_pop_rua <- munic_2021_pop_rua[,-c(2:30)]

### Contando os totais para depois anexá-los a última tabela

totais <- data.frame(count(munic_2021_pop_rua, "codigo_ibge"))
names(totais)[2] <- "Total de Pessoas em Situacção de Rua"


###15 Seleção de Dados sem trabalho nos últimos 12 meses

trabalho_12_meses_2021 <- munic_2021_pop_rua%>% replace_na(list(CO_PRINCIPAL_TRAB_MEMB = "Sem Dados"))

trabalho_12_meses_2021$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2021$CO_TRABALHO_12_MESES_MEMB=="1")] <- "Sim"

trabalho_12_meses_2021$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2021$CO_TRABALHO_12_MESES_MEMB=="2")] <- "Não"

trabalho_12_meses_2021 <-subset(trabalho_12_meses_2021, CO_TRABALHO_12_MESES_MEMB!="Sim")
trabalho_12_meses_2021 <- data.frame(count(trabalho_12_meses_2021, "codigo_ibge"))
trabalho_12_meses_2021$CO_TRABALHO_12_MESES_MEMB <- "Não"

sem_trabalho_2021 <- merge(brasil,trabalho_12_meses_2021, "codigo_ibge")
sem_trabalho_2021 <- merge(sem_trabalho_2021, totais, "codigo_ibge")
sem_trabalho_2021$Desempregados <- (sem_trabalho_2021$freq/sem_trabalho_2021$`Total de Pessoas em Situacção de Rua`)*100
sem_trabalho_2021$Ano <- "2021"

write.xlsx(sem_trabalho_2021, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sem_trabalho_2021_serie_historica.xlsx")


### Tabela 2020 do Ministério das Cidades

brasil_munic_2020  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_202012.csv", 
                              col_names = TRUE)

names(brasil_munic_2020)[1]<- "codigo_ibge"

munic_2020_pop_rua  <- data.frame(count(brasil_munic_2020, "codigo_ibge"))

munic_2020_pop_rua$Ano <- "2020"

munic_2020_pop_rua <- merge(brasil_munic_2020, brasil, by = "codigo_ibge")
munic_2020_pop_rua <- munic_2020_pop_rua[,-c(2:31)]

### Contando os totais para depois anexá-los a última tabela

totais <- data.frame(count(munic_2020_pop_rua, "codigo_ibge"))
names(totais)[2] <- "Total de Pessoas em Situacção de Rua"


###15 Seleção de Dados sem trabalho nos últimos 12 meses

trabalho_12_meses_2020 <- munic_2020_pop_rua%>% replace_na(list(CO_PRINCIPAL_TRAB_MEMB = "Sem Dados"))

trabalho_12_meses_2020$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2020$CO_TRABALHO_12_MESES_MEMB=="1")] <- "Sim"

trabalho_12_meses_2020$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2020$CO_TRABALHO_12_MESES_MEMB=="2")] <- "Não"

trabalho_12_meses_2020 <-subset(trabalho_12_meses_2020, CO_TRABALHO_12_MESES_MEMB!="Sim")
trabalho_12_meses_2020 <- data.frame(count(trabalho_12_meses_2020, "codigo_ibge"))
trabalho_12_meses_2020$CO_TRABALHO_12_MESES_MEMB <- "Não"

sem_trabalho_2020 <- merge(brasil,trabalho_12_meses_2020, "codigo_ibge")
sem_trabalho_2020 <- merge(sem_trabalho_2020, totais, "codigo_ibge")
sem_trabalho_2020$Desempregados <- (sem_trabalho_2020$freq/sem_trabalho_2020$`Total de Pessoas em Situacção de Rua`)*100
sem_trabalho_2020$Ano <- "2020"

write.xlsx(sem_trabalho_2020, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sem_trabalho_2020_serie_historica.xlsx")


### Tabela 2019 do Ministério das Cidades


brasil_munic_2019  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201912.csv", 
                              col_names = TRUE)

names(brasil_munic_2019)[1]<- "codigo_ibge"

munic_2019_pop_rua  <- data.frame(count(brasil_munic_2019, "codigo_ibge"))

munic_2019_pop_rua$Ano <- "2019"

munic_2019_pop_rua <- merge(brasil_munic_2019, brasil, by = "codigo_ibge")
munic_2019_pop_rua <- munic_2019_pop_rua[,-c(2:31)]

### Contando os totais para depois anexá-los a última tabela

totais <- data.frame(count(munic_2019_pop_rua, "codigo_ibge"))
names(totais)[2] <- "Total de Pessoas em Situacção de Rua"


###15 Seleção de Dados sem trabalho nos últimos 12 meses

trabalho_12_meses_2019 <- munic_2019_pop_rua%>% replace_na(list(CO_PRINCIPAL_TRAB_MEMB = "Sem Dados"))

trabalho_12_meses_2019$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2019$CO_TRABALHO_12_MESES_MEMB=="1")] <- "Sim"

trabalho_12_meses_2019$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2019$CO_TRABALHO_12_MESES_MEMB=="2")] <- "Não"

trabalho_12_meses_2019 <-subset(trabalho_12_meses_2019, CO_TRABALHO_12_MESES_MEMB!="Sim")
trabalho_12_meses_2019 <- data.frame(count(trabalho_12_meses_2019, "codigo_ibge"))
trabalho_12_meses_2019$CO_TRABALHO_12_MESES_MEMB <- "Não"

sem_trabalho_2019 <- merge(brasil,trabalho_12_meses_2019, "codigo_ibge")
sem_trabalho_2019 <- merge(sem_trabalho_2019, totais, "codigo_ibge")
sem_trabalho_2019$Desempregados <- (sem_trabalho_2019$freq/sem_trabalho_2019$`Total de Pessoas em Situacção de Rua`)*100
sem_trabalho_2019$Ano <- "2019"

write.xlsx(sem_trabalho_2019, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sem_trabalho_2019_serie_historica.xlsx")


### Tabela 2018 do Ministério das Cidades


brasil_munic_2018  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201812.csv", 
                              col_names = TRUE)

names(brasil_munic_2018)[1]<- "codigo_ibge"

munic_2018_pop_rua  <- data.frame(count(brasil_munic_2018, "codigo_ibge"))

munic_2018_pop_rua$Ano <- "2018"

munic_2018_pop_rua <- merge(brasil_munic_2018, brasil, by = "codigo_ibge")
munic_2018_pop_rua <- munic_2018_pop_rua[,-c(2:31)]

### Contando os totais para depois anexá-los a última tabela

totais <- data.frame(count(munic_2018_pop_rua, "codigo_ibge"))
names(totais)[2] <- "Total de Pessoas em Situacção de Rua"


###15 Seleção de Dados sem trabalho nos últimos 12 meses

trabalho_12_meses_2018 <- munic_2018_pop_rua%>% replace_na(list(CO_PRINCIPAL_TRAB_MEMB = "Sem Dados"))

trabalho_12_meses_2018$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2018$CO_TRABALHO_12_MESES_MEMB=="1")] <- "Sim"

trabalho_12_meses_2018$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2018$CO_TRABALHO_12_MESES_MEMB=="2")] <- "Não"

trabalho_12_meses_2018 <-subset(trabalho_12_meses_2018, CO_TRABALHO_12_MESES_MEMB!="Sim")
trabalho_12_meses_2018 <- data.frame(count(trabalho_12_meses_2018, "codigo_ibge"))
trabalho_12_meses_2018$CO_TRABALHO_12_MESES_MEMB <- "Não"

sem_trabalho_2018 <- merge(brasil,trabalho_12_meses_2018, "codigo_ibge")
sem_trabalho_2018 <- merge(sem_trabalho_2018, totais, "codigo_ibge")
sem_trabalho_2018$Desempregados <- (sem_trabalho_2018$freq/sem_trabalho_2018$`Total de Pessoas em Situacção de Rua`)*100
sem_trabalho_2018$Ano <- "2018"

write.xlsx(sem_trabalho_2018, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sem_trabalho_2018_serie_historica.xlsx")


### Tabela 2017 do Ministério das Cidades


brasil_munic_2017  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201712.csv", 
                              col_names = TRUE)

names(brasil_munic_2017)[1]<- "codigo_ibge"

munic_2017_pop_rua  <- data.frame(count(brasil_munic_2017, "codigo_ibge"))

munic_2017_pop_rua$Ano <- "2017"

munic_2017_pop_rua <- merge(brasil_munic_2017, brasil, by = "codigo_ibge")
munic_2017_pop_rua <- munic_2017_pop_rua[,-c(2:31)]

### Contando os totais para depois anexá-los a última tabela

totais <- data.frame(count(munic_2017_pop_rua, "codigo_ibge"))
names(totais)[2] <- "Total de Pessoas em Situacção de Rua"


###15 Seleção de Dados sem trabalho nos últimos 12 meses

trabalho_12_meses_2017 <- munic_2017_pop_rua%>% replace_na(list(CO_PRINCIPAL_TRAB_MEMB = "Sem Dados"))

trabalho_12_meses_2017$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2017$CO_TRABALHO_12_MESES_MEMB=="1")] <- "Sim"

trabalho_12_meses_2017$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2017$CO_TRABALHO_12_MESES_MEMB=="2")] <- "Não"

trabalho_12_meses_2017 <-subset(trabalho_12_meses_2017, CO_TRABALHO_12_MESES_MEMB!="Sim")
trabalho_12_meses_2017 <- data.frame(count(trabalho_12_meses_2017, "codigo_ibge"))
trabalho_12_meses_2017$CO_TRABALHO_12_MESES_MEMB <- "Não"

sem_trabalho_2017 <- merge(brasil,trabalho_12_meses_2017, "codigo_ibge")
sem_trabalho_2017 <- merge(sem_trabalho_2017, totais, "codigo_ibge")
sem_trabalho_2017$Desempregados <- (sem_trabalho_2017$freq/sem_trabalho_2017$`Total de Pessoas em Situacção de Rua`)*100
sem_trabalho_2017$Ano <- "2017"

write.xlsx(sem_trabalho_2017, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sem_trabalho_2017_serie_historica.xlsx")




### Tabela 2016 do Ministério das Cidades


brasil_munic_2016  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201612.csv", 
                              col_names = TRUE)

names(brasil_munic_2016)[1]<- "codigo_ibge"

munic_2016_pop_rua  <- data.frame(count(brasil_munic_2016, "codigo_ibge"))

munic_2016_pop_rua$Ano <- "2016"

munic_2016_pop_rua <- merge(brasil_munic_2016, brasil, by = "codigo_ibge")
munic_2016_pop_rua <- munic_2016_pop_rua[,-c(2:31)]

### Contando os totais para depois anexá-los a última tabela

totais <- data.frame(count(munic_2016_pop_rua, "codigo_ibge"))
names(totais)[2] <- "Total de Pessoas em Situacção de Rua"


###15 Seleção de Dados sem trabalho nos últimos 12 meses

trabalho_12_meses_2016 <- munic_2016_pop_rua%>% replace_na(list(CO_PRINCIPAL_TRAB_MEMB = "Sem Dados"))

trabalho_12_meses_2016$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2016$CO_TRABALHO_12_MESES_MEMB=="1")] <- "Sim"

trabalho_12_meses_2016$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2016$CO_TRABALHO_12_MESES_MEMB=="2")] <- "Não"

trabalho_12_meses_2016 <-subset(trabalho_12_meses_2016, CO_TRABALHO_12_MESES_MEMB!="Sim")
trabalho_12_meses_2016 <- data.frame(count(trabalho_12_meses_2016, "codigo_ibge"))
trabalho_12_meses_2016$CO_TRABALHO_12_MESES_MEMB <- "Não"

sem_trabalho_2016 <- merge(brasil,trabalho_12_meses_2016, "codigo_ibge")
sem_trabalho_2016 <- merge(sem_trabalho_2016, totais, "codigo_ibge")
sem_trabalho_2016$Desempregados <- (sem_trabalho_2016$freq/sem_trabalho_2016$`Total de Pessoas em Situacção de Rua`)*100
sem_trabalho_2016$Ano <- "2016"

write.xlsx(sem_trabalho_2016, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sem_trabalho_2016_serie_historica.xlsx")




### Tabela 2015 do Ministério das Cidades


brasil_munic_2015  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201512.csv", 
                              col_names = TRUE)

names(brasil_munic_2015)[1]<- "codigo_ibge"

munic_2015_pop_rua  <- data.frame(count(brasil_munic_2015, "codigo_ibge"))

munic_2015_pop_rua$Ano <- "2015"

munic_2015_pop_rua <- merge(brasil_munic_2015, brasil, by = "codigo_ibge")
munic_2015_pop_rua <- munic_2015_pop_rua[,-c(2:31)]

### Contando os totais para depois anexá-los a última tabela

totais <- data.frame(count(munic_2015_pop_rua, "codigo_ibge"))
names(totais)[2] <- "Total de Pessoas em Situacção de Rua"


###15 Seleção de Dados sem trabalho nos últimos 12 meses

trabalho_12_meses_2015 <- munic_2015_pop_rua%>% replace_na(list(CO_PRINCIPAL_TRAB_MEMB = "Sem Dados"))

trabalho_12_meses_2015$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2015$CO_TRABALHO_12_MESES_MEMB=="1")] <- "Sim"

trabalho_12_meses_2015$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2015$CO_TRABALHO_12_MESES_MEMB=="2")] <- "Não"

trabalho_12_meses_2015 <-subset(trabalho_12_meses_2015, CO_TRABALHO_12_MESES_MEMB!="Sim")
trabalho_12_meses_2015 <- data.frame(count(trabalho_12_meses_2015, "codigo_ibge"))
trabalho_12_meses_2015$CO_TRABALHO_12_MESES_MEMB <- "Não"

sem_trabalho_2015 <- merge(brasil,trabalho_12_meses_2015, "codigo_ibge")
sem_trabalho_2015 <- merge(sem_trabalho_2015, totais, "codigo_ibge")
sem_trabalho_2015$Desempregados <- (sem_trabalho_2015$freq/sem_trabalho_2015$`Total de Pessoas em Situacção de Rua`)*100
sem_trabalho_2015$Ano <- "2015"

write.xlsx(sem_trabalho_2015, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sem_trabalho_2015_serie_historica.xlsx")




### Tabela 2014 do Ministério das Cidades


brasil_munic_2014  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201412.csv", 
                              col_names = TRUE)

names(brasil_munic_2014)[1]<- "codigo_ibge"

munic_2014_pop_rua  <- data.frame(count(brasil_munic_2014, "codigo_ibge"))

munic_2014_pop_rua$Ano <- "2014"

munic_2014_pop_rua <- merge(brasil_munic_2014, brasil, by = "codigo_ibge")
munic_2014_pop_rua <- munic_2014_pop_rua[,-c(2:31)]

### Contando os totais para depois anexá-los a última tabela

totais <- data.frame(count(munic_2014_pop_rua, "codigo_ibge"))
names(totais)[2] <- "Total de Pessoas em Situacção de Rua"


###15 Seleção de Dados sem trabalho nos últimos 12 meses

trabalho_12_meses_2014 <- munic_2014_pop_rua%>% replace_na(list(CO_PRINCIPAL_TRAB_MEMB = "Sem Dados"))

trabalho_12_meses_2014$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2014$CO_TRABALHO_12_MESES_MEMB=="1")] <- "Sim"

trabalho_12_meses_2014$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2014$CO_TRABALHO_12_MESES_MEMB=="2")] <- "Não"

trabalho_12_meses_2014 <-subset(trabalho_12_meses_2014, CO_TRABALHO_12_MESES_MEMB!="Sim")
trabalho_12_meses_2014 <- data.frame(count(trabalho_12_meses_2014, "codigo_ibge"))
trabalho_12_meses_2014$CO_TRABALHO_12_MESES_MEMB <- "Não"

sem_trabalho_2014 <- merge(brasil,trabalho_12_meses_2014, "codigo_ibge")
sem_trabalho_2014 <- merge(sem_trabalho_2014, totais, "codigo_ibge")
sem_trabalho_2014$Desempregados <- (sem_trabalho_2014$freq/sem_trabalho_2014$`Total de Pessoas em Situacção de Rua`)*100
sem_trabalho_2014$Ano <- "2014"

write.xlsx(sem_trabalho_2014, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sem_trabalho_2014_serie_historica.xlsx")



### Tabela 2013 do Ministério das Cidades


brasil_munic_2013  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201312.csv", 
                              col_names = TRUE)

names(brasil_munic_2013)[1]<- "codigo_ibge"

munic_2013_pop_rua  <- data.frame(count(brasil_munic_2013, "codigo_ibge"))

munic_2013_pop_rua$Ano <- "2013"

munic_2013_pop_rua <- merge(brasil_munic_2013, brasil, by = "codigo_ibge")
munic_2013_pop_rua <- munic_2013_pop_rua[,-c(2:31)]

### Contando os totais para depois anexá-los a última tabela

totais <- data.frame(count(munic_2013_pop_rua, "codigo_ibge"))
names(totais)[2] <- "Total de Pessoas em Situacção de Rua"


###15 Seleção de Dados sem trabalho nos últimos 12 meses

trabalho_12_meses_2013 <- munic_2013_pop_rua%>% replace_na(list(CO_PRINCIPAL_TRAB_MEMB = "Sem Dados"))

trabalho_12_meses_2013$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2013$CO_TRABALHO_12_MESES_MEMB=="1")] <- "Sim"

trabalho_12_meses_2013$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2013$CO_TRABALHO_12_MESES_MEMB=="2")] <- "Não"

trabalho_12_meses_2013 <-subset(trabalho_12_meses_2013, CO_TRABALHO_12_MESES_MEMB!="Sim")
trabalho_12_meses_2013 <- data.frame(count(trabalho_12_meses_2013, "codigo_ibge"))
trabalho_12_meses_2013$CO_TRABALHO_12_MESES_MEMB <- "Não"

sem_trabalho_2013 <- merge(brasil,trabalho_12_meses_2013, "codigo_ibge")
sem_trabalho_2013 <- merge(sem_trabalho_2013, totais, "codigo_ibge")
sem_trabalho_2013$Desempregados <- (sem_trabalho_2013$freq/sem_trabalho_2013$`Total de Pessoas em Situacção de Rua`)*100
sem_trabalho_2013$Ano <- "2013"

write.xlsx(sem_trabalho_2013, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sem_trabalho_2013_serie_historica.xlsx")


### Tabela 2012 do Ministério das Cidades


brasil_munic_2012  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201212.csv", 
                              col_names = TRUE)

names(brasil_munic_2012)[1]<- "codigo_ibge"

munic_2012_pop_rua  <- data.frame(count(brasil_munic_2012, "codigo_ibge"))

munic_2012_pop_rua$Ano <- "2012"

munic_2012_pop_rua <- merge(brasil_munic_2012, brasil, by = "codigo_ibge")
munic_2012_pop_rua <- munic_2012_pop_rua[,-c(2:31)]

### Contando os totais para depois anexá-los a última tabela

totais <- data.frame(count(munic_2012_pop_rua, "codigo_ibge"))
names(totais)[2] <- "Total de Pessoas em Situacção de Rua"


###15 Seleção de Dados sem trabalho nos últimos 12 meses

trabalho_12_meses_2012 <- munic_2012_pop_rua%>% replace_na(list(CO_PRINCIPAL_TRAB_MEMB = "Sem Dados"))

trabalho_12_meses_2012$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2012$CO_TRABALHO_12_MESES_MEMB=="1")] <- "Sim"

trabalho_12_meses_2012$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2012$CO_TRABALHO_12_MESES_MEMB=="2")] <- "Não"

trabalho_12_meses_2012 <-subset(trabalho_12_meses_2012, CO_TRABALHO_12_MESES_MEMB!="Sim")
trabalho_12_meses_2012 <- data.frame(count(trabalho_12_meses_2012, "codigo_ibge"))
trabalho_12_meses_2012$CO_TRABALHO_12_MESES_MEMB <- "Não"

sem_trabalho_2012 <- merge(brasil,trabalho_12_meses_2012, "codigo_ibge")
sem_trabalho_2012 <- merge(sem_trabalho_2012, totais, "codigo_ibge")
sem_trabalho_2012$Desempregados <- (sem_trabalho_2012$freq/sem_trabalho_2012$`Total de Pessoas em Situacção de Rua`)*100
sem_trabalho_2012$Ano <- "2012"

write.xlsx(sem_trabalho_2012, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sem_trabalho_2012_serie_historica.xlsx")


##########


serie_historica_cidades <- rbind(sem_trabalho_2021,
                                 sem_trabalho_2020,
                                 sem_trabalho_2019, 
                                 sem_trabalho_2018, 
                                 sem_trabalho_2017, 
                                 sem_trabalho_2016, 
                                 sem_trabalho_2015, 
                                 sem_trabalho_2014, 
                                 sem_trabalho_2013, 
                                 sem_trabalho_2012)

write_xlsx(serie_historica_cidades, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sem_trabalho_2012_2021_serie_historica.xlsx")
