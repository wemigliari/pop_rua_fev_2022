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
library(janitor)


### Brasil Shapefile

brasil <- read_sf("/Users/wemigliari/Documents/R/data/BR_Municipios_2021.shp")
names(brasil)[1] <- "codigo_ibge"

### Tabela 2021 do Ministério das Cidades

brasil_munic_2021  <- read_excel("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_202112_abril.xlsx", 
                                 col_names = TRUE)
names(brasil_munic_2021)[4]<- "codigo_ibge"

###10 Seleção de Dados COR

test_cor_2021 <- brasil_munic_2021 %>% group_by(codigo_ibge,CO_SEXO_PESSOA, CO_RACA_COR_PESSOA) %>% tally()
test_cor_2021$CO_RACA_COR_PESSOA[which(test_cor_2021$CO_RACA_COR_PESSOA=="1")] <- "Branca"
test_cor_2021$CO_RACA_COR_PESSOA[which(test_cor_2021$CO_RACA_COR_PESSOA=="2")] <- "Preta"
test_cor_2021$CO_RACA_COR_PESSOA[which(test_cor_2021$CO_RACA_COR_PESSOA=="3")] <- "Amarela"
test_cor_2021$CO_RACA_COR_PESSOA[which(test_cor_2021$CO_RACA_COR_PESSOA=="4")] <- "Parda"
test_cor_2021$CO_RACA_COR_PESSOA[which(test_cor_2021$CO_RACA_COR_PESSOA=="5")] <- "Indígena"
test_cor_2021$CO_SEXO_PESSOA[which(test_cor_2021$CO_SEXO_PESSOA=="1")] <- "masculino"
test_cor_2021$CO_SEXO_PESSOA[which(test_cor_2021$CO_SEXO_PESSOA=="2")] <- "feminino"

x_2021 <- count(test_cor_2021, "codigo_ibge")
x_2021 <- test_cor_2021 %>% add_count(codigo_ibge, wt = n)
x_2021 <- merge(x_2021, brasil, by = "codigo_ibge")
x_2021$geometry <- NULL
x_2021<-subset(x_2021, CO_SEXO_PESSOA!="feminino")
x_2021 <- na.omit(x_2021)
x_2021$CO_RACA_COR_PESSOA[which(x_2021$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
x_2021$CO_RACA_COR_PESSOA[which(x_2021$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"
x_2021 <-subset(x_2021, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                & CO_RACA_COR_PESSOA!="Indígena")
x_2021 <- x_2021[!duplicated(x_2021$NM_MUN), ]
x_2021$n <- NULL


test_2021<-subset(test_cor_2021, CO_SEXO_PESSOA!="feminino")
test_2021$CO_RACA_COR_PESSOA[which(test_2021$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
test_2021$CO_RACA_COR_PESSOA[which(test_2021$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"


munic_2021_pop_rua <- merge(test_2021, brasil, by = "codigo_ibge")
munic_2021_pop_rua$geometry <- NULL
munic_2021_pop_rua$Ano <- "2021"
munic_2021_pop_rua <- na.omit(munic_2021_pop_rua)
munic_2021_pop_rua <-subset(munic_2021_pop_rua, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                            & CO_RACA_COR_PESSOA!="Indígena")

names(munic_2021_pop_rua)[4]<- "Total"

munic_2021_pop_rua <- munic_2021_pop_rua %>% add_count(codigo_ibge, wt = Total)
munic_2021_pop_rua$Total <- NULL
munic_2021_pop_rua <- munic_2021_pop_rua[!duplicated(munic_2021_pop_rua$NM_MUN), ]

munic_2021_pop_rua <- munic_2021_pop_rua[,-c(2:7)]

munic_2021_pop_rua <- merge(munic_2021_pop_rua, x_2021, by = "codigo_ibge")
names(munic_2021_pop_rua)[5]<- "Total"
munic_2021_pop_rua <- munic_2021_pop_rua[,c(1,3,5,4,2,6,7,8)]

total_2021 <- data.frame(count(brasil_munic_2021, "codigo_ibge"))
munic_2021_pop_rua <- merge(munic_2021_pop_rua, total_2021, by="codigo_ibge")
munic_2021_pop_rua$proporcao <- (munic_2021_pop_rua$n/munic_2021_pop_rua$Total)*100
munic_2021_pop_rua$Ano <- "2021"

write_xlsx(munic_2021_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/homens_pretos_pardos_cidades_2021.xlsx")


### Tabela 2020 do Ministério das Cidades

brasil_munic_2020  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_202012.csv", 
                               col_names = TRUE)
names(brasil_munic_2020)[1]<- "codigo_ibge"

###10 Seleção de Dados COR

test_cor_2020 <- brasil_munic_2020 %>% group_by(codigo_ibge,CO_SEXO_PESSOA, CO_RACA_COR_PESSOA) %>% tally()
test_cor_2020$CO_RACA_COR_PESSOA[which(test_cor_2020$CO_RACA_COR_PESSOA=="1")] <- "Branca"
test_cor_2020$CO_RACA_COR_PESSOA[which(test_cor_2020$CO_RACA_COR_PESSOA=="2")] <- "Preta"
test_cor_2020$CO_RACA_COR_PESSOA[which(test_cor_2020$CO_RACA_COR_PESSOA=="3")] <- "Amarela"
test_cor_2020$CO_RACA_COR_PESSOA[which(test_cor_2020$CO_RACA_COR_PESSOA=="4")] <- "Parda"
test_cor_2020$CO_RACA_COR_PESSOA[which(test_cor_2020$CO_RACA_COR_PESSOA=="5")] <- "Indígena"
test_cor_2020$CO_SEXO_PESSOA[which(test_cor_2020$CO_SEXO_PESSOA=="1")] <- "masculino"
test_cor_2020$CO_SEXO_PESSOA[which(test_cor_2020$CO_SEXO_PESSOA=="2")] <- "feminino"

x_2020 <- count(test_cor_2020, "codigo_ibge")
x_2020 <- test_cor_2020 %>% add_count(codigo_ibge, wt = n)
x_2020 <- merge(x_2020, brasil, by = "codigo_ibge")
x_2020$geometry <- NULL
x_2020<-subset(x_2020, CO_SEXO_PESSOA!="feminino")
x_2020 <- na.omit(x_2020)
x_2020$CO_RACA_COR_PESSOA[which(x_2020$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
x_2020$CO_RACA_COR_PESSOA[which(x_2020$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"
x_2020 <-subset(x_2020, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                & CO_RACA_COR_PESSOA!="Indígena")
x_2020 <- x_2020[!duplicated(x_2020$NM_MUN), ]
x_2020$n <- NULL


test_2020<-subset(test_cor_2020, CO_SEXO_PESSOA!="feminino")
test_2020$CO_RACA_COR_PESSOA[which(test_2020$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
test_2020$CO_RACA_COR_PESSOA[which(test_2020$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"


munic_2020_pop_rua <- merge(test_2020, brasil, by = "codigo_ibge")
munic_2020_pop_rua$geometry <- NULL
munic_2020_pop_rua$Ano <- "2020"
munic_2020_pop_rua <- na.omit(munic_2020_pop_rua)
munic_2020_pop_rua <-subset(munic_2020_pop_rua, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                            & CO_RACA_COR_PESSOA!="Indígena")

names(munic_2020_pop_rua)[4]<- "Total"

munic_2020_pop_rua <- munic_2020_pop_rua %>% add_count(codigo_ibge, wt = Total)
munic_2020_pop_rua$Total <- NULL
munic_2020_pop_rua <- munic_2020_pop_rua[!duplicated(munic_2020_pop_rua$NM_MUN), ]

munic_2020_pop_rua <- munic_2020_pop_rua[,-c(2:7)]

munic_2020_pop_rua <- merge(munic_2020_pop_rua, x_2020, by = "codigo_ibge")
names(munic_2020_pop_rua)[5]<- "Total"
munic_2020_pop_rua <- munic_2020_pop_rua[,c(1,3,5,4,2,6,7,8)]

total_2020 <- data.frame(count(brasil_munic_2020, "codigo_ibge"))
munic_2020_pop_rua <- merge(munic_2020_pop_rua, total_2020, by="codigo_ibge")
munic_2020_pop_rua$proporcao <- (munic_2020_pop_rua$n/munic_2020_pop_rua$Total)*100
munic_2020_pop_rua$Ano <- "2020"

write_xlsx(munic_2020_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/homens_pretos_pardos_cidades_2020.xlsx")



### Tabela 2019 do Ministério das Cidades

brasil_munic_2019  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201912.csv", 
                               col_names = TRUE)
names(brasil_munic_2019)[1]<- "codigo_ibge"


###10 Seleção de Dados COR

test_cor_2019 <- brasil_munic_2019 %>% group_by(codigo_ibge,CO_SEXO_PESSOA, CO_RACA_COR_PESSOA) %>% tally()
test_cor_2019$CO_RACA_COR_PESSOA[which(test_cor_2019$CO_RACA_COR_PESSOA=="1")] <- "Branca"
test_cor_2019$CO_RACA_COR_PESSOA[which(test_cor_2019$CO_RACA_COR_PESSOA=="2")] <- "Preta"
test_cor_2019$CO_RACA_COR_PESSOA[which(test_cor_2019$CO_RACA_COR_PESSOA=="3")] <- "Amarela"
test_cor_2019$CO_RACA_COR_PESSOA[which(test_cor_2019$CO_RACA_COR_PESSOA=="4")] <- "Parda"
test_cor_2019$CO_RACA_COR_PESSOA[which(test_cor_2019$CO_RACA_COR_PESSOA=="5")] <- "Indígena"
test_cor_2019$CO_SEXO_PESSOA[which(test_cor_2019$CO_SEXO_PESSOA=="1")] <- "masculino"
test_cor_2019$CO_SEXO_PESSOA[which(test_cor_2019$CO_SEXO_PESSOA=="2")] <- "feminino"

x_2019 <- count(test_cor_2019, "codigo_ibge")
x_2019 <- test_cor_2019 %>% add_count(codigo_ibge, wt = n)
x_2019 <- merge(x_2019, brasil, by = "codigo_ibge")
x_2019$geometry <- NULL
x_2019<-subset(x_2019, CO_SEXO_PESSOA!="feminino")
x_2019 <- na.omit(x_2019)
x_2019$CO_RACA_COR_PESSOA[which(x_2019$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
x_2019$CO_RACA_COR_PESSOA[which(x_2019$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"
x_2019 <-subset(x_2019, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                & CO_RACA_COR_PESSOA!="Indígena")
x_2019 <- x_2019[!duplicated(x_2019$NM_MUN), ]
x_2019$n <- NULL


test_2019<-subset(test_cor_2019, CO_SEXO_PESSOA!="feminino")
test_2019$CO_RACA_COR_PESSOA[which(test_2019$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
test_2019$CO_RACA_COR_PESSOA[which(test_2019$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"


munic_2019_pop_rua <- merge(test_2019, brasil, by = "codigo_ibge")
munic_2019_pop_rua$geometry <- NULL
munic_2019_pop_rua$Ano <- "2019"
munic_2019_pop_rua <- na.omit(munic_2019_pop_rua)
munic_2019_pop_rua <-subset(munic_2019_pop_rua, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                            & CO_RACA_COR_PESSOA!="Indígena")

names(munic_2019_pop_rua)[4]<- "Total"

munic_2019_pop_rua <- munic_2019_pop_rua %>% add_count(codigo_ibge, wt = Total)
munic_2019_pop_rua$Total <- NULL
munic_2019_pop_rua <- munic_2019_pop_rua[!duplicated(munic_2019_pop_rua$NM_MUN), ]

munic_2019_pop_rua <- munic_2019_pop_rua[,-c(2:6)]

munic_2019_pop_rua <- merge(munic_2019_pop_rua, x_2019, by = "codigo_ibge")
names(munic_2019_pop_rua)[6]<- "Total"

total_2019 <- data.frame(count(brasil_munic_2019, "codigo_ibge"))
munic_2019_pop_rua <- merge(munic_2019_pop_rua, total_2019, by="codigo_ibge")
munic_2019_pop_rua$proporcao <- (munic_2019_pop_rua$n/munic_2019_pop_rua$Total)*100
munic_2019_pop_rua$Ano <- "2019"

write_xlsx(munic_2019_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/homens_pretos_pardos_cidades_2019.xlsx")

### Tabela 2018 do Ministério das Cidades

brasil_munic_2018  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201812.csv", 
                               col_names = TRUE)
names(brasil_munic_2018)[1]<- "codigo_ibge"

###10 Seleção de Dados COR

test_cor_2018 <- brasil_munic_2018 %>% group_by(codigo_ibge,CO_SEXO_PESSOA, CO_RACA_COR_PESSOA) %>% tally()
test_cor_2018$CO_RACA_COR_PESSOA[which(test_cor_2018$CO_RACA_COR_PESSOA=="1")] <- "Branca"
test_cor_2018$CO_RACA_COR_PESSOA[which(test_cor_2018$CO_RACA_COR_PESSOA=="2")] <- "Preta"
test_cor_2018$CO_RACA_COR_PESSOA[which(test_cor_2018$CO_RACA_COR_PESSOA=="3")] <- "Amarela"
test_cor_2018$CO_RACA_COR_PESSOA[which(test_cor_2018$CO_RACA_COR_PESSOA=="4")] <- "Parda"
test_cor_2018$CO_RACA_COR_PESSOA[which(test_cor_2018$CO_RACA_COR_PESSOA=="5")] <- "Indígena"
test_cor_2018$CO_SEXO_PESSOA[which(test_cor_2018$CO_SEXO_PESSOA=="1")] <- "masculino"
test_cor_2018$CO_SEXO_PESSOA[which(test_cor_2018$CO_SEXO_PESSOA=="2")] <- "feminino"

x_2018 <- count(test_cor_2018, "codigo_ibge")
x_2018 <- test_cor_2018 %>% add_count(codigo_ibge, wt = n)
x_2018 <- merge(x_2018, brasil, by = "codigo_ibge")
x_2018$geometry <- NULL
x_2018<-subset(x_2018, CO_SEXO_PESSOA!="feminino")
x_2018 <- na.omit(x_2018)
x_2018$CO_RACA_COR_PESSOA[which(x_2018$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
x_2018$CO_RACA_COR_PESSOA[which(x_2018$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"
x_2018 <-subset(x_2018, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                & CO_RACA_COR_PESSOA!="Indígena")
x_2018 <- x_2018[!duplicated(x_2018$NM_MUN), ]
x_2018$n <- NULL


test_2018<-subset(test_cor_2018, CO_SEXO_PESSOA!="feminino")
test_2018$CO_RACA_COR_PESSOA[which(test_2018$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
test_2018$CO_RACA_COR_PESSOA[which(test_2018$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"


munic_2018_pop_rua <- merge(test_2018, brasil, by = "codigo_ibge")
munic_2018_pop_rua$geometry <- NULL
munic_2018_pop_rua$Ano <- "2018"
munic_2018_pop_rua <- na.omit(munic_2018_pop_rua)
munic_2018_pop_rua <-subset(munic_2018_pop_rua, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                            & CO_RACA_COR_PESSOA!="Indígena")

names(munic_2018_pop_rua)[4]<- "Total"

munic_2018_pop_rua <- munic_2018_pop_rua %>% add_count(codigo_ibge, wt = Total)
munic_2018_pop_rua$Total <- NULL
munic_2018_pop_rua <- munic_2018_pop_rua[!duplicated(munic_2018_pop_rua$NM_MUN), ]

munic_2018_pop_rua <- munic_2018_pop_rua[,-c(2:6)]

munic_2018_pop_rua <- merge(munic_2018_pop_rua, x_2018, by = "codigo_ibge")
names(munic_2018_pop_rua)[6]<- "Total"

total_2018 <- data.frame(count(brasil_munic_2018, "codigo_ibge"))
munic_2018_pop_rua <- merge(munic_2018_pop_rua, total_2018, by="codigo_ibge")
munic_2018_pop_rua$proporcao <- (munic_2018_pop_rua$n/munic_2018_pop_rua$Total)*100
munic_2018_pop_rua$Ano <- "2018"

write_xlsx(munic_2018_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/homens_pretos_pardos_cidades_2018.xlsx")


### Tabela 2017 do Ministério das Cidades

brasil_munic_2017  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201712.csv", 
                               col_names = TRUE)
names(brasil_munic_2017)[1]<- "codigo_ibge"

###10 Seleção de Dados COR

test_cor_2017 <- brasil_munic_2017 %>% group_by(codigo_ibge,CO_SEXO_PESSOA, CO_RACA_COR_PESSOA) %>% tally()
test_cor_2017$CO_RACA_COR_PESSOA[which(test_cor_2017$CO_RACA_COR_PESSOA=="1")] <- "Branca"
test_cor_2017$CO_RACA_COR_PESSOA[which(test_cor_2017$CO_RACA_COR_PESSOA=="2")] <- "Preta"
test_cor_2017$CO_RACA_COR_PESSOA[which(test_cor_2017$CO_RACA_COR_PESSOA=="3")] <- "Amarela"
test_cor_2017$CO_RACA_COR_PESSOA[which(test_cor_2017$CO_RACA_COR_PESSOA=="4")] <- "Parda"
test_cor_2017$CO_RACA_COR_PESSOA[which(test_cor_2017$CO_RACA_COR_PESSOA=="5")] <- "Indígena"
test_cor_2017$CO_SEXO_PESSOA[which(test_cor_2017$CO_SEXO_PESSOA=="1")] <- "masculino"
test_cor_2017$CO_SEXO_PESSOA[which(test_cor_2017$CO_SEXO_PESSOA=="2")] <- "feminino"

x_2017 <- count(test_cor_2017, "codigo_ibge")
x_2017 <- test_cor_2017 %>% add_count(codigo_ibge, wt = n)
x_2017 <- merge(x_2017, brasil, by = "codigo_ibge")
x_2017$geometry <- NULL
x_2017<-subset(x_2017, CO_SEXO_PESSOA!="feminino")
x_2017 <- na.omit(x_2017)
x_2017$CO_RACA_COR_PESSOA[which(x_2017$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
x_2017$CO_RACA_COR_PESSOA[which(x_2017$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"
x_2017 <-subset(x_2017, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                & CO_RACA_COR_PESSOA!="Indígena")
x_2017 <- x_2017[!duplicated(x_2017$NM_MUN), ]
x_2017$n <- NULL


test_2017<-subset(test_cor_2017, CO_SEXO_PESSOA!="feminino")
test_2017$CO_RACA_COR_PESSOA[which(test_2017$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
test_2017$CO_RACA_COR_PESSOA[which(test_2017$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"


munic_2017_pop_rua <- merge(test_2017, brasil, by = "codigo_ibge")
munic_2017_pop_rua$geometry <- NULL
munic_2017_pop_rua$Ano <- "2017"
munic_2017_pop_rua <- na.omit(munic_2017_pop_rua)
munic_2017_pop_rua <-subset(munic_2017_pop_rua, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                            & CO_RACA_COR_PESSOA!="Indígena")

names(munic_2017_pop_rua)[4]<- "Total"

munic_2017_pop_rua <- munic_2017_pop_rua %>% add_count(codigo_ibge, wt = Total)
munic_2017_pop_rua$Total <- NULL
munic_2017_pop_rua <- munic_2017_pop_rua[!duplicated(munic_2017_pop_rua$NM_MUN), ]

munic_2017_pop_rua <- munic_2017_pop_rua[,-c(2:6)]

munic_2017_pop_rua <- merge(munic_2017_pop_rua, x_2017, by = "codigo_ibge")
names(munic_2017_pop_rua)[6]<- "Total"

total_2017 <- data.frame(count(brasil_munic_2017, "codigo_ibge"))
munic_2017_pop_rua <- merge(munic_2017_pop_rua, total_2017, by="codigo_ibge")
munic_2017_pop_rua$proporcao <- (munic_2017_pop_rua$n/munic_2017_pop_rua$Total)*100
munic_2017_pop_rua$Ano <- "2017"

write_xlsx(munic_2017_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/homens_pretos_pardos_cidades_2017.xlsx")

### Tabela 2016 do Ministério das Cidades


brasil_munic_2016  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201612.csv", 
                               col_names = TRUE)
names(brasil_munic_2016)[1]<- "codigo_ibge"


###10 Seleção de Dados COR

test_cor_2016 <- brasil_munic_2016 %>% group_by(codigo_ibge,CO_SEXO_PESSOA, CO_RACA_COR_PESSOA) %>% tally()
test_cor_2016$CO_RACA_COR_PESSOA[which(test_cor_2016$CO_RACA_COR_PESSOA=="1")] <- "Branca"
test_cor_2016$CO_RACA_COR_PESSOA[which(test_cor_2016$CO_RACA_COR_PESSOA=="2")] <- "Preta"
test_cor_2016$CO_RACA_COR_PESSOA[which(test_cor_2016$CO_RACA_COR_PESSOA=="3")] <- "Amarela"
test_cor_2016$CO_RACA_COR_PESSOA[which(test_cor_2016$CO_RACA_COR_PESSOA=="4")] <- "Parda"
test_cor_2016$CO_RACA_COR_PESSOA[which(test_cor_2016$CO_RACA_COR_PESSOA=="5")] <- "Indígena"
test_cor_2016$CO_SEXO_PESSOA[which(test_cor_2016$CO_SEXO_PESSOA=="1")] <- "masculino"
test_cor_2016$CO_SEXO_PESSOA[which(test_cor_2016$CO_SEXO_PESSOA=="2")] <- "feminino"

x_2016 <- count(test_cor_2016, "codigo_ibge")
x_2016 <- test_cor_2016 %>% add_count(codigo_ibge, wt = n)
x_2016 <- merge(x_2016, brasil, by = "codigo_ibge")
x_2016$geometry <- NULL
x_2016<-subset(x_2016, CO_SEXO_PESSOA!="feminino")
x_2016 <- na.omit(x_2016)
x_2016$CO_RACA_COR_PESSOA[which(x_2016$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
x_2016$CO_RACA_COR_PESSOA[which(x_2016$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"
x_2016 <-subset(x_2016, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                & CO_RACA_COR_PESSOA!="Indígena")
x_2016 <- x_2016[!duplicated(x_2016$NM_MUN), ]
x_2016$n <- NULL


test_2016<-subset(test_cor_2016, CO_SEXO_PESSOA!="feminino")
test_2016$CO_RACA_COR_PESSOA[which(test_2016$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
test_2016$CO_RACA_COR_PESSOA[which(test_2016$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"


munic_2016_pop_rua <- merge(test_2016, brasil, by = "codigo_ibge")
munic_2016_pop_rua$geometry <- NULL
munic_2016_pop_rua$Ano <- "2016"
munic_2016_pop_rua <- na.omit(munic_2016_pop_rua)
munic_2016_pop_rua <-subset(munic_2016_pop_rua, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                            & CO_RACA_COR_PESSOA!="Indígena")

names(munic_2016_pop_rua)[4]<- "Total"

munic_2016_pop_rua <- munic_2016_pop_rua %>% add_count(codigo_ibge, wt = Total)
munic_2016_pop_rua$Total <- NULL
munic_2016_pop_rua <- munic_2016_pop_rua[!duplicated(munic_2016_pop_rua$NM_MUN), ]

munic_2016_pop_rua <- munic_2016_pop_rua[,-c(2:6)]

munic_2016_pop_rua <- merge(munic_2016_pop_rua, x_2016, by = "codigo_ibge")
names(munic_2016_pop_rua)[6]<- "Total"

total_2016 <- data.frame(count(brasil_munic_2016, "codigo_ibge"))
munic_2016_pop_rua <- merge(munic_2016_pop_rua, total_2016, by="codigo_ibge")
munic_2016_pop_rua$proporcao <- (munic_2016_pop_rua$n/munic_2016_pop_rua$Total)*100
munic_2016_pop_rua$Ano <- "2016"

write_xlsx(munic_2016_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/homens_pretos_pardos_cidades_2016.xlsx")

### Tabela 2015 do Ministério das Cidades

brasil_munic_2015  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201512.csv", 
                               col_names = TRUE)
names(brasil_munic_2015)[1]<- "codigo_ibge"

###10 Seleção de Dados COR

test_cor_2015 <- brasil_munic_2015 %>% group_by(codigo_ibge,CO_SEXO_PESSOA, CO_RACA_COR_PESSOA) %>% tally()
test_cor_2015$CO_RACA_COR_PESSOA[which(test_cor_2015$CO_RACA_COR_PESSOA=="1")] <- "Branca"
test_cor_2015$CO_RACA_COR_PESSOA[which(test_cor_2015$CO_RACA_COR_PESSOA=="2")] <- "Preta"
test_cor_2015$CO_RACA_COR_PESSOA[which(test_cor_2015$CO_RACA_COR_PESSOA=="3")] <- "Amarela"
test_cor_2015$CO_RACA_COR_PESSOA[which(test_cor_2015$CO_RACA_COR_PESSOA=="4")] <- "Parda"
test_cor_2015$CO_RACA_COR_PESSOA[which(test_cor_2015$CO_RACA_COR_PESSOA=="5")] <- "Indígena"
test_cor_2015$CO_SEXO_PESSOA[which(test_cor_2015$CO_SEXO_PESSOA=="1")] <- "masculino"
test_cor_2015$CO_SEXO_PESSOA[which(test_cor_2015$CO_SEXO_PESSOA=="2")] <- "feminino"

x_2015 <- count(test_cor_2015, "codigo_ibge")
x_2015 <- test_cor_2015 %>% add_count(codigo_ibge, wt = n)
x_2015 <- merge(x_2015, brasil, by = "codigo_ibge")
x_2015$geometry <- NULL
x_2015<-subset(x_2015, CO_SEXO_PESSOA!="feminino")
x_2015 <- na.omit(x_2015)
x_2015$CO_RACA_COR_PESSOA[which(x_2015$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
x_2015$CO_RACA_COR_PESSOA[which(x_2015$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"
x_2015 <-subset(x_2015, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                & CO_RACA_COR_PESSOA!="Indígena")
x_2015 <- x_2015[!duplicated(x_2015$NM_MUN), ]
x_2015$n <- NULL


test_2015<-subset(test_cor_2015, CO_SEXO_PESSOA!="feminino")
test_2015$CO_RACA_COR_PESSOA[which(test_2015$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
test_2015$CO_RACA_COR_PESSOA[which(test_2015$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"


munic_2015_pop_rua <- merge(test_2015, brasil, by = "codigo_ibge")
munic_2015_pop_rua$geometry <- NULL
munic_2015_pop_rua$Ano <- "2015"
munic_2015_pop_rua <- na.omit(munic_2015_pop_rua)
munic_2015_pop_rua <-subset(munic_2015_pop_rua, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                            & CO_RACA_COR_PESSOA!="Indígena")

names(munic_2015_pop_rua)[4]<- "Total"

munic_2015_pop_rua <- munic_2015_pop_rua %>% add_count(codigo_ibge, wt = Total)
munic_2015_pop_rua$Total <- NULL
munic_2015_pop_rua <- munic_2015_pop_rua[!duplicated(munic_2015_pop_rua$NM_MUN), ]

munic_2015_pop_rua <- munic_2015_pop_rua[,-c(2:6)]

munic_2015_pop_rua <- merge(munic_2015_pop_rua, x_2015, by = "codigo_ibge")
names(munic_2015_pop_rua)[6]<- "Total"

total_2015 <- data.frame(count(brasil_munic_2015, "codigo_ibge"))
munic_2015_pop_rua <- merge(munic_2015_pop_rua, total_2015, by="codigo_ibge")
munic_2015_pop_rua$proporcao <- (munic_2015_pop_rua$n/munic_2015_pop_rua$Total)*100
munic_2015_pop_rua$Ano <- "2015"

write_xlsx(munic_2015_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/homens_pretos_pardos_cidades_2015.xlsx")

### Tabela 2014 do Ministério das Cidades

brasil_munic_2014  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201412.csv", 
                               col_names = TRUE)
names(brasil_munic_2014)[1]<- "codigo_ibge"

###10 Seleção de Dados COR

test_cor_2014 <- brasil_munic_2014 %>% group_by(codigo_ibge,CO_SEXO_PESSOA, CO_RACA_COR_PESSOA) %>% tally()
test_cor_2014$CO_RACA_COR_PESSOA[which(test_cor_2014$CO_RACA_COR_PESSOA=="1")] <- "Branca"
test_cor_2014$CO_RACA_COR_PESSOA[which(test_cor_2014$CO_RACA_COR_PESSOA=="2")] <- "Preta"
test_cor_2014$CO_RACA_COR_PESSOA[which(test_cor_2014$CO_RACA_COR_PESSOA=="3")] <- "Amarela"
test_cor_2014$CO_RACA_COR_PESSOA[which(test_cor_2014$CO_RACA_COR_PESSOA=="4")] <- "Parda"
test_cor_2014$CO_RACA_COR_PESSOA[which(test_cor_2014$CO_RACA_COR_PESSOA=="5")] <- "Indígena"
test_cor_2014$CO_SEXO_PESSOA[which(test_cor_2014$CO_SEXO_PESSOA=="1")] <- "masculino"
test_cor_2014$CO_SEXO_PESSOA[which(test_cor_2014$CO_SEXO_PESSOA=="2")] <- "feminino"

x_2014 <- count(test_cor_2014, "codigo_ibge")
x_2014 <- test_cor_2014 %>% add_count(codigo_ibge, wt = n)
x_2014 <- merge(x_2014, brasil, by = "codigo_ibge")
x_2014$geometry <- NULL
x_2014<-subset(x_2014, CO_SEXO_PESSOA!="feminino")
x_2014 <- na.omit(x_2014)
x_2014$CO_RACA_COR_PESSOA[which(x_2014$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
x_2014$CO_RACA_COR_PESSOA[which(x_2014$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"
x_2014 <-subset(x_2014, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                & CO_RACA_COR_PESSOA!="Indígena")
x_2014 <- x_2014[!duplicated(x_2014$NM_MUN), ]
x_2014$n <- NULL


test_2014<-subset(test_cor_2014, CO_SEXO_PESSOA!="feminino")
test_2014$CO_RACA_COR_PESSOA[which(test_2014$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
test_2014$CO_RACA_COR_PESSOA[which(test_2014$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"


munic_2014_pop_rua <- merge(test_2014, brasil, by = "codigo_ibge")
munic_2014_pop_rua$geometry <- NULL
munic_2014_pop_rua$Ano <- "2014"
munic_2014_pop_rua <- na.omit(munic_2014_pop_rua)
munic_2014_pop_rua <-subset(munic_2014_pop_rua, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                            & CO_RACA_COR_PESSOA!="Indígena")

names(munic_2014_pop_rua)[4]<- "Total"

munic_2014_pop_rua <- munic_2014_pop_rua %>% add_count(codigo_ibge, wt = Total)
munic_2014_pop_rua$Total <- NULL
munic_2014_pop_rua <- munic_2014_pop_rua[!duplicated(munic_2014_pop_rua$NM_MUN), ]

munic_2014_pop_rua <- munic_2014_pop_rua[,-c(2:6)]

munic_2014_pop_rua <- merge(munic_2014_pop_rua, x_2014, by = "codigo_ibge")
names(munic_2014_pop_rua)[6]<- "Total"

total_2014 <- data.frame(count(brasil_munic_2014, "codigo_ibge"))
munic_2014_pop_rua <- merge(munic_2014_pop_rua, total_2014, by="codigo_ibge")
munic_2014_pop_rua$proporcao <- (munic_2014_pop_rua$n/munic_2014_pop_rua$Total)*100
munic_2014_pop_rua$Ano <- "2014"

write_xlsx(munic_2014_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/homens_pretos_pardos_cidades_2014.xlsx")


### Tabela 2013 do Ministério das Cidades

brasil_munic_2013  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201312.csv", 
                               col_names = TRUE)
names(brasil_munic_2013)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

###10 Seleção de Dados COR

test_cor_2013 <- brasil_munic_2013 %>% group_by(codigo_ibge,CO_SEXO_PESSOA, CO_RACA_COR_PESSOA) %>% tally()
test_cor_2013$CO_RACA_COR_PESSOA[which(test_cor_2013$CO_RACA_COR_PESSOA=="1")] <- "Branca"
test_cor_2013$CO_RACA_COR_PESSOA[which(test_cor_2013$CO_RACA_COR_PESSOA=="2")] <- "Preta"
test_cor_2013$CO_RACA_COR_PESSOA[which(test_cor_2013$CO_RACA_COR_PESSOA=="3")] <- "Amarela"
test_cor_2013$CO_RACA_COR_PESSOA[which(test_cor_2013$CO_RACA_COR_PESSOA=="4")] <- "Parda"
test_cor_2013$CO_RACA_COR_PESSOA[which(test_cor_2013$CO_RACA_COR_PESSOA=="5")] <- "Indígena"
test_cor_2013$CO_SEXO_PESSOA[which(test_cor_2013$CO_SEXO_PESSOA=="1")] <- "masculino"
test_cor_2013$CO_SEXO_PESSOA[which(test_cor_2013$CO_SEXO_PESSOA=="2")] <- "feminino"

x_2013 <- count(test_cor_2013, "codigo_ibge")
x_2013 <- test_cor_2013 %>% add_count(codigo_ibge, wt = n)
x_2013 <- merge(x_2013, brasil, by = "codigo_ibge")
x_2013$geometry <- NULL
x_2013<-subset(x_2013, CO_SEXO_PESSOA!="feminino")
x_2013 <- na.omit(x_2013)
x_2013$CO_RACA_COR_PESSOA[which(x_2013$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
x_2013$CO_RACA_COR_PESSOA[which(x_2013$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"
x_2013 <-subset(x_2013, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                & CO_RACA_COR_PESSOA!="Indígena")
x_2013 <- x_2013[!duplicated(x_2013$NM_MUN), ]
x_2013$n <- NULL


test_2013<-subset(test_cor_2013, CO_SEXO_PESSOA!="feminino")
test_2013$CO_RACA_COR_PESSOA[which(test_2013$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
test_2013$CO_RACA_COR_PESSOA[which(test_2013$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"


munic_2013_pop_rua <- merge(test_2013, brasil, by = "codigo_ibge")
munic_2013_pop_rua$geometry <- NULL
munic_2013_pop_rua$Ano <- "2013"
munic_2013_pop_rua <- na.omit(munic_2013_pop_rua)
munic_2013_pop_rua <-subset(munic_2013_pop_rua, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                            & CO_RACA_COR_PESSOA!="Indígena")

names(munic_2013_pop_rua)[4]<- "Total"

munic_2013_pop_rua <- munic_2013_pop_rua %>% add_count(codigo_ibge, wt = Total)
munic_2013_pop_rua$Total <- NULL
munic_2013_pop_rua <- munic_2013_pop_rua[!duplicated(munic_2013_pop_rua$NM_MUN), ]

munic_2013_pop_rua <- munic_2013_pop_rua[,-c(2:6)]

munic_2013_pop_rua <- merge(munic_2013_pop_rua, x_2013, by = "codigo_ibge")
names(munic_2013_pop_rua)[6]<- "Total"

total_2013 <- data.frame(count(brasil_munic_2013, "codigo_ibge"))
munic_2013_pop_rua <- merge(munic_2013_pop_rua, total_2013, by="codigo_ibge")
munic_2013_pop_rua$proporcao <- (munic_2013_pop_rua$n/munic_2013_pop_rua$Total)*100
munic_2013_pop_rua$Ano <- "2013"

write_xlsx(munic_2013_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/homens_pretos_pardos_cidades_2013.xlsx")

### Tabela 2012 do Ministério das Cidades


brasil_munic_2012  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201212.csv", 
                               col_names = TRUE)
names(brasil_munic_2012)[1]<- "codigo_ibge"

###10 Seleção de Dados COR

test_cor_2012 <- brasil_munic_2012 %>% group_by(codigo_ibge,CO_SEXO_PESSOA, CO_RACA_COR_PESSOA) %>% tally()
test_cor_2012$CO_RACA_COR_PESSOA[which(test_cor_2012$CO_RACA_COR_PESSOA=="1")] <- "Branca"
test_cor_2012$CO_RACA_COR_PESSOA[which(test_cor_2012$CO_RACA_COR_PESSOA=="2")] <- "Preta"
test_cor_2012$CO_RACA_COR_PESSOA[which(test_cor_2012$CO_RACA_COR_PESSOA=="3")] <- "Amarela"
test_cor_2012$CO_RACA_COR_PESSOA[which(test_cor_2012$CO_RACA_COR_PESSOA=="4")] <- "Parda"
test_cor_2012$CO_RACA_COR_PESSOA[which(test_cor_2012$CO_RACA_COR_PESSOA=="5")] <- "Indígena"
test_cor_2012$CO_SEXO_PESSOA[which(test_cor_2012$CO_SEXO_PESSOA=="1")] <- "masculino"
test_cor_2012$CO_SEXO_PESSOA[which(test_cor_2012$CO_SEXO_PESSOA=="2")] <- "feminino"

x_2012 <- count(test_cor_2012, "codigo_ibge")
x_2012 <- test_cor_2012 %>% add_count(codigo_ibge, wt = n)
x_2012 <- merge(x_2012, brasil, by = "codigo_ibge")
x_2012$geometry <- NULL
x_2012<-subset(x_2012, CO_SEXO_PESSOA!="feminino")
x_2012 <- na.omit(x_2012)
x_2012$CO_RACA_COR_PESSOA[which(x_2012$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
x_2012$CO_RACA_COR_PESSOA[which(x_2012$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"
x_2012 <-subset(x_2012, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                & CO_RACA_COR_PESSOA!="Indígena")
x_2012 <- x_2012[!duplicated(x_2012$NM_MUN), ]
x_2012$n <- NULL


test_2012<-subset(test_cor_2012, CO_SEXO_PESSOA!="feminino")
test_2012$CO_RACA_COR_PESSOA[which(test_2012$CO_RACA_COR_PESSOA=="Preta")] <- "Preta & Parda"
test_2012$CO_RACA_COR_PESSOA[which(test_2012$CO_RACA_COR_PESSOA=="Parda")] <- "Preta & Parda"


munic_2012_pop_rua <- merge(test_2012, brasil, by = "codigo_ibge")
munic_2012_pop_rua$geometry <- NULL
munic_2012_pop_rua$Ano <- "2012"
munic_2012_pop_rua <- na.omit(munic_2012_pop_rua)
munic_2012_pop_rua <-subset(munic_2012_pop_rua, CO_RACA_COR_PESSOA!="Branca" & CO_RACA_COR_PESSOA!="Amarela" 
                            & CO_RACA_COR_PESSOA!="Indígena")

names(munic_2012_pop_rua)[4]<- "Total"

munic_2012_pop_rua <- munic_2012_pop_rua %>% add_count(codigo_ibge, wt = Total)
munic_2012_pop_rua$Total <- NULL
munic_2012_pop_rua <- munic_2012_pop_rua[!duplicated(munic_2012_pop_rua$NM_MUN), ]

munic_2012_pop_rua <- munic_2012_pop_rua[,-c(2:6)]

munic_2012_pop_rua <- merge(munic_2012_pop_rua, x_2012, by = "codigo_ibge")
names(munic_2012_pop_rua)[6]<- "Total"

total_2012 <- data.frame(count(brasil_munic_2012, "codigo_ibge"))
munic_2012_pop_rua <- merge(munic_2012_pop_rua, total_2012, by="codigo_ibge")
munic_2012_pop_rua$proporcao <- (munic_2012_pop_rua$n/munic_2012_pop_rua$Total)*100
munic_2012_pop_rua$Ano <- "2012"

write_xlsx(munic_2012_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/homens_pretos_pardos_cidades_2012.xlsx")

##########


serie_historica_homens_pretos_pardos_cidades <- rbind(munic_2021_pop_rua,
                                                        munic_2020_pop_rua,
                                                        munic_2019_pop_rua, 
                                                        munic_2018_pop_rua, 
                                                        munic_2017_pop_rua, 
                                                        munic_2016_pop_rua, 
                                                        munic_2015_pop_rua, 
                                                        munic_2014_pop_rua, 
                                                        munic_2013_pop_rua, 
                                                        munic_2012_pop_rua)


write_xlsx(serie_historica_homens_pretos_pardos_cidades, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/homens_pretos_pardos_serie_historica_cidades.xlsx")

####### 10 Cidades com maiores casos

munic_2021_pop_rua_10 <- munic_2021_pop_rua[order(munic_2021_pop_rua$n, decreasing = TRUE),]
munic_2021_pop_rua_10 <- munic_2021_pop_rua_10[1:10, ]

munic_2020_pop_rua_10 <- munic_2020_pop_rua[order(munic_2020_pop_rua$n, decreasing = TRUE),]
munic_2020_pop_rua_10 <- munic_2020_pop_rua_10[1:10, ]

munic_2019_pop_rua_10 <- munic_2019_pop_rua[order(munic_2019_pop_rua$n, decreasing = TRUE),]
munic_2019_pop_rua_10 <- munic_2019_pop_rua_10[1:10, ]

munic_2018_pop_rua_10 <- munic_2018_pop_rua[order(munic_2018_pop_rua$n, decreasing = TRUE),]
munic_2018_pop_rua_10 <- munic_2018_pop_rua_10[1:10, ]

munic_2017_pop_rua_10 <- munic_2017_pop_rua[order(munic_2017_pop_rua$n, decreasing = TRUE),]
munic_2017_pop_rua_10 <- munic_2017_pop_rua_10[1:10, ]

munic_2016_pop_rua_10 <- munic_2016_pop_rua[order(munic_2016_pop_rua$n, decreasing = TRUE),]
munic_2016_pop_rua_10 <- munic_2016_pop_rua_10[1:10, ]

munic_2015_pop_rua_10 <- munic_2015_pop_rua[order(munic_2015_pop_rua$n, decreasing = TRUE),]
munic_2015_pop_rua_10 <- munic_2015_pop_rua_10[1:10, ]

munic_2014_pop_rua_10 <- munic_2014_pop_rua[order(munic_2014_pop_rua$n, decreasing = TRUE),]
munic_2014_pop_rua_10 <- munic_2014_pop_rua_10[1:10, ]

munic_2013_pop_rua_10 <- munic_2013_pop_rua[order(munic_2013_pop_rua$n, decreasing = TRUE),]
munic_2013_pop_rua_10 <- munic_2013_pop_rua_10[1:10, ]

munic_2012_pop_rua_10 <- munic_2012_pop_rua[order(munic_2012_pop_rua$n, decreasing = TRUE),]
munic_2012_pop_rua_10 <- munic_2012_pop_rua_10[1:10, ]



serie_historica_homens_pretos_pardos_cidades_10 <- rbind(munic_2012_pop_rua_10,
                                                           munic_2013_pop_rua_10,
                                                           munic_2014_pop_rua_10, 
                                                           munic_2015_pop_rua_10, 
                                                           munic_2016_pop_rua_10, 
                                                           munic_2017_pop_rua_10, 
                                                           munic_2018_pop_rua_10, 
                                                           munic_2019_pop_rua_10, 
                                                           munic_2020_pop_rua_10, 
                                                           munic_2021_pop_rua_10)

write_xlsx(serie_historica_homens_pretos_pardos_cidades_10, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/homens_pretos_pardos_serie_historica_10_cidades.xlsx")
