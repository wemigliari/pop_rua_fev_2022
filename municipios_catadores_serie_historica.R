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


###Seleção de Dados Família

test <- brasil_munic_2021 %>% group_by(codigo_ibge,IN_PARC_MDS_FAM) %>% tally()
test$IN_PARC_MDS_FAM[which(test$IN_PARC_MDS_FAM=="NA")] <- "Sem Dados"
test$IN_PARC_MDS_FAM[which(test$IN_PARC_MDS_FAM=="0")] <- "Sem Código"
test$IN_PARC_MDS_FAM[which(test$IN_PARC_MDS_FAM=="000")] <- "Nenhuma"
test$IN_PARC_MDS_FAM[which(test$IN_PARC_MDS_FAM=="101")] <- "Família Cigana"
test$IN_PARC_MDS_FAM[which(test$IN_PARC_MDS_FAM=="201")] <- "Família Extrativista"
test$IN_PARC_MDS_FAM[which(test$IN_PARC_MDS_FAM=="202")] <- "Pescadores Artesanais"
test$IN_PARC_MDS_FAM[which(test$IN_PARC_MDS_FAM=="203")] <- "Família Pertencente a Comunidade de Terreiro"
test$IN_PARC_MDS_FAM[which(test$IN_PARC_MDS_FAM=="204")] <- "Família Ribeirinha"
test$IN_PARC_MDS_FAM[which(test$IN_PARC_MDS_FAM=="205")] <- "Família de Agricultores Familiares"
test$IN_PARC_MDS_FAM[which(test$IN_PARC_MDS_FAM=="301")] <- "Família Assentada da Reforma Agrária"
test$IN_PARC_MDS_FAM[which(test$IN_PARC_MDS_FAM=="302")] <- "Família Beneficiária do Programa Nacional de Crédito Fundiário"
test$IN_PARC_MDS_FAM[which(test$IN_PARC_MDS_FAM=="303")] <- "Família Acampada"
test$IN_PARC_MDS_FAM[which(test$IN_PARC_MDS_FAM=="304")] <- "Família Atingida por Empreendimentos de Infraestrutura"
test$IN_PARC_MDS_FAM[which(test$IN_PARC_MDS_FAM=="305")] <- "Família de Preso do Sistema Carcerário"
test$IN_PARC_MDS_FAM[which(test$IN_PARC_MDS_FAM=="306")] <- "Família de Catadores de Material Reciclável"

test <-subset(test, IN_PARC_MDS_FAM!="Sem Dados" & IN_PARC_MDS_FAM!="Sem Código" 
              & IN_PARC_MDS_FAM!="Nenhuma" & IN_PARC_MDS_FAM!="Família Cigana"
              & IN_PARC_MDS_FAM!="Família Extrativista" & IN_PARC_MDS_FAM!="Pescadores Artesanais"
              & IN_PARC_MDS_FAM!="Família Pertencente a Comunidade de Terreiro" & IN_PARC_MDS_FAM!="Família Ribeirinha"
              & IN_PARC_MDS_FAM!="Família de Agricultores Familiares" & IN_PARC_MDS_FAM!="Família Assentada da Reforma Agrária"
              & IN_PARC_MDS_FAM!="Família Beneficiária do Programa Nacional de Crédito Fundiário" & IN_PARC_MDS_FAM!="Família Acampada"
              & IN_PARC_MDS_FAM!="Família Atingida por Empreendimentos de Infraestrutura" & IN_PARC_MDS_FAM!="Família de Preso do Sistema Carcerário")

munic_2021_pop_rua <- merge(test, brasil, by = "codigo_ibge")
munic_2021_pop_rua$geometry <- NULL
munic_2021_pop_rua$Ano <- "2021"
munic_2021_pop_rua <- na.omit(munic_2021_pop_rua)

write_xlsx(munic_2021_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/catadores_cidades_2021.xlsx")


### Tabela 2020 do Ministério das Cidades

brasil_munic_2020  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_202012.csv", 
                               col_names = TRUE)
names(brasil_munic_2020)[1]<- "codigo_ibge"

###Seleção de Dados Família


test2 <- brasil_munic_2020 %>% group_by(codigo_ibge,IN_PARC_MDS_FAM) %>% tally()
test2$IN_PARC_MDS_FAM[which(test2$IN_PARC_MDS_FAM=="NA")] <- "Sem Dados"
test2$IN_PARC_MDS_FAM[which(test2$IN_PARC_MDS_FAM=="0")] <- "Sem Código"
test2$IN_PARC_MDS_FAM[which(test2$IN_PARC_MDS_FAM=="000")] <- "Nenhuma"
test2$IN_PARC_MDS_FAM[which(test2$IN_PARC_MDS_FAM=="101")] <- "Família Cigana"
test2$IN_PARC_MDS_FAM[which(test2$IN_PARC_MDS_FAM=="201")] <- "Família Extrativista"
test2$IN_PARC_MDS_FAM[which(test2$IN_PARC_MDS_FAM=="202")] <- "Pescadores Artesanais"
test2$IN_PARC_MDS_FAM[which(test2$IN_PARC_MDS_FAM=="203")] <- "Família Pertencente a Comunidade de Terreiro"
test2$IN_PARC_MDS_FAM[which(test2$IN_PARC_MDS_FAM=="204")] <- "Família Ribeirinha"
test2$IN_PARC_MDS_FAM[which(test2$IN_PARC_MDS_FAM=="205")] <- "Família de Agricultores Familiares"
test2$IN_PARC_MDS_FAM[which(test2$IN_PARC_MDS_FAM=="301")] <- "Família Assentada da Reforma Agrária"
test2$IN_PARC_MDS_FAM[which(test2$IN_PARC_MDS_FAM=="302")] <- "Família Beneficiária do Programa Nacional de Crédito Fundiário"
test2$IN_PARC_MDS_FAM[which(test2$IN_PARC_MDS_FAM=="303")] <- "Família Acampada"
test2$IN_PARC_MDS_FAM[which(test2$IN_PARC_MDS_FAM=="304")] <- "Família Atingida por Empreendimentos de Infraestrutura"
test2$IN_PARC_MDS_FAM[which(test2$IN_PARC_MDS_FAM=="305")] <- "Família de Preso do Sistema Carcerário"
test2$IN_PARC_MDS_FAM[which(test2$IN_PARC_MDS_FAM=="306")] <- "Família de Catadores de Material Reciclável"

test2 <-subset(test2, IN_PARC_MDS_FAM!="Sem Dados" & IN_PARC_MDS_FAM!="Sem Código" 
               & IN_PARC_MDS_FAM!="Nenhuma" & IN_PARC_MDS_FAM!="Família Cigana"
               & IN_PARC_MDS_FAM!="Família Extrativista" & IN_PARC_MDS_FAM!="Pescadores Artesanais"
               & IN_PARC_MDS_FAM!="Família Pertencente a Comunidade de Terreiro" & IN_PARC_MDS_FAM!="Família Ribeirinha"
               & IN_PARC_MDS_FAM!="Família de Agricultores Familiares" & IN_PARC_MDS_FAM!="Família Assentada da Reforma Agrária"
               & IN_PARC_MDS_FAM!="Família Beneficiária do Programa Nacional de Crédito Fundiário" & IN_PARC_MDS_FAM!="Família Acampada"
               & IN_PARC_MDS_FAM!="Família Atingida por Empreendimentos de Infraestrutura" & IN_PARC_MDS_FAM!="Família de Preso do Sistema Carcerário")


munic_2020_pop_rua <- merge(test2, brasil, by = "codigo_ibge")
munic_2020_pop_rua$geometry <- NULL
munic_2020_pop_rua$Ano <- "2020"
munic_2020_pop_rua <- na.omit(munic_2020_pop_rua)

write_xlsx(munic_2020_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/catadores_cidades_2020.xlsx")


### Tabela 2019 do Ministério das Cidades

brasil_munic_2019  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201912.csv", 
                               col_names = TRUE)
names(brasil_munic_2019)[1]<- "codigo_ibge"

###Seleção de Dados Família


test3 <- brasil_munic_2019 %>% group_by(codigo_ibge,IN_PARC_MDS_FAM) %>% tally()
test3$IN_PARC_MDS_FAM[which(test3$IN_PARC_MDS_FAM=="NA")] <- "Sem Dados"
test3$IN_PARC_MDS_FAM[which(test3$IN_PARC_MDS_FAM=="0")] <- "Sem Código"
test3$IN_PARC_MDS_FAM[which(test3$IN_PARC_MDS_FAM=="000")] <- "Nenhuma"
test3$IN_PARC_MDS_FAM[which(test3$IN_PARC_MDS_FAM=="101")] <- "Família Cigana"
test3$IN_PARC_MDS_FAM[which(test3$IN_PARC_MDS_FAM=="201")] <- "Família Extrativista"
test3$IN_PARC_MDS_FAM[which(test3$IN_PARC_MDS_FAM=="202")] <- "Pescadores Artesanais"
test3$IN_PARC_MDS_FAM[which(test3$IN_PARC_MDS_FAM=="203")] <- "Família Pertencente a Comunidade de Terreiro"
test3$IN_PARC_MDS_FAM[which(test3$IN_PARC_MDS_FAM=="204")] <- "Família Ribeirinha"
test3$IN_PARC_MDS_FAM[which(test3$IN_PARC_MDS_FAM=="205")] <- "Família de Agricultores Familiares"
test3$IN_PARC_MDS_FAM[which(test3$IN_PARC_MDS_FAM=="301")] <- "Família Assentada da Reforma Agrária"
test3$IN_PARC_MDS_FAM[which(test3$IN_PARC_MDS_FAM=="302")] <- "Família Beneficiária do Programa Nacional de Crédito Fundiário"
test3$IN_PARC_MDS_FAM[which(test3$IN_PARC_MDS_FAM=="303")] <- "Família Acampada"
test3$IN_PARC_MDS_FAM[which(test3$IN_PARC_MDS_FAM=="304")] <- "Família Atingida por Empreendimentos de Infraestrutura"
test3$IN_PARC_MDS_FAM[which(test3$IN_PARC_MDS_FAM=="305")] <- "Família de Preso do Sistema Carcerário"
test3$IN_PARC_MDS_FAM[which(test3$IN_PARC_MDS_FAM=="306")] <- "Família de Catadores de Material Reciclável"

test3 <-subset(test3, IN_PARC_MDS_FAM!="Sem Dados" & IN_PARC_MDS_FAM!="Sem Código" 
               & IN_PARC_MDS_FAM!="Nenhuma" & IN_PARC_MDS_FAM!="Família Cigana"
               & IN_PARC_MDS_FAM!="Família Extrativista" & IN_PARC_MDS_FAM!="Pescadores Artesanais"
               & IN_PARC_MDS_FAM!="Família Pertencente a Comunidade de Terreiro" & IN_PARC_MDS_FAM!="Família Ribeirinha"
               & IN_PARC_MDS_FAM!="Família de Agricultores Familiares" & IN_PARC_MDS_FAM!="Família Assentada da Reforma Agrária"
               & IN_PARC_MDS_FAM!="Família Beneficiária do Programa Nacional de Crédito Fundiário" & IN_PARC_MDS_FAM!="Família Acampada"
               & IN_PARC_MDS_FAM!="Família Atingida por Empreendimentos de Infraestrutura" & IN_PARC_MDS_FAM!="Família de Preso do Sistema Carcerário")

munic_2019_pop_rua <- merge(test3, brasil, by = "codigo_ibge")
munic_2019_pop_rua$geometry <- NULL
munic_2019_pop_rua$Ano <- "2019"
munic_2019_pop_rua <- na.omit(munic_2019_pop_rua)

write_xlsx(munic_2019_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/catadores_cidades_2019.xlsx")

### Tabela 2018 do Ministério das Cidades

brasil_munic_2018  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201812.csv", 
                               col_names = TRUE)
names(brasil_munic_2018)[1]<- "codigo_ibge"


###Seleção de Dados Família


test4 <- brasil_munic_2018 %>% group_by(codigo_ibge,IN_PARC_MDS_FAM) %>% tally()
test4$IN_PARC_MDS_FAM[which(test4$IN_PARC_MDS_FAM=="NA")] <- "Sem Dados"
test4$IN_PARC_MDS_FAM[which(test4$IN_PARC_MDS_FAM=="0")] <- "Sem Código"
test4$IN_PARC_MDS_FAM[which(test4$IN_PARC_MDS_FAM=="000")] <- "Nenhuma"
test4$IN_PARC_MDS_FAM[which(test4$IN_PARC_MDS_FAM=="101")] <- "Família Cigana"
test4$IN_PARC_MDS_FAM[which(test4$IN_PARC_MDS_FAM=="201")] <- "Família Extrativista"
test4$IN_PARC_MDS_FAM[which(test4$IN_PARC_MDS_FAM=="202")] <- "Pescadores Artesanais"
test4$IN_PARC_MDS_FAM[which(test4$IN_PARC_MDS_FAM=="203")] <- "Família Pertencente a Comunidade de Terreiro"
test4$IN_PARC_MDS_FAM[which(test4$IN_PARC_MDS_FAM=="204")] <- "Família Ribeirinha"
test4$IN_PARC_MDS_FAM[which(test4$IN_PARC_MDS_FAM=="205")] <- "Família de Agricultores Familiares"
test4$IN_PARC_MDS_FAM[which(test4$IN_PARC_MDS_FAM=="301")] <- "Família Assentada da Reforma Agrária"
test4$IN_PARC_MDS_FAM[which(test4$IN_PARC_MDS_FAM=="302")] <- "Família Beneficiária do Programa Nacional de Crédito Fundiário"
test4$IN_PARC_MDS_FAM[which(test4$IN_PARC_MDS_FAM=="303")] <- "Família Acampada"
test4$IN_PARC_MDS_FAM[which(test4$IN_PARC_MDS_FAM=="304")] <- "Família Atingida por Empreendimentos de Infraestrutura"
test4$IN_PARC_MDS_FAM[which(test4$IN_PARC_MDS_FAM=="305")] <- "Família de Preso do Sistema Carcerário"
test4$IN_PARC_MDS_FAM[which(test4$IN_PARC_MDS_FAM=="306")] <- "Família de Catadores de Material Reciclável"

test4 <-subset(test4, IN_PARC_MDS_FAM!="Sem Dados" & IN_PARC_MDS_FAM!="Sem Código" 
               & IN_PARC_MDS_FAM!="Nenhuma" & IN_PARC_MDS_FAM!="Família Cigana"
               & IN_PARC_MDS_FAM!="Família Extrativista" & IN_PARC_MDS_FAM!="Pescadores Artesanais"
               & IN_PARC_MDS_FAM!="Família Pertencente a Comunidade de Terreiro" & IN_PARC_MDS_FAM!="Família Ribeirinha"
               & IN_PARC_MDS_FAM!="Família de Agricultores Familiares" & IN_PARC_MDS_FAM!="Família Assentada da Reforma Agrária"
               & IN_PARC_MDS_FAM!="Família Beneficiária do Programa Nacional de Crédito Fundiário" & IN_PARC_MDS_FAM!="Família Acampada"
               & IN_PARC_MDS_FAM!="Família Atingida por Empreendimentos de Infraestrutura" & IN_PARC_MDS_FAM!="Família de Preso do Sistema Carcerário")

munic_2018_pop_rua <- merge(test4, brasil, by = "codigo_ibge")
munic_2018_pop_rua$geometry <- NULL
munic_2018_pop_rua$Ano <- "2018"
munic_2018_pop_rua <- na.omit(munic_2018_pop_rua)

write_xlsx(munic_2018_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/catadores_cidades_2018.xlsx")


### Tabela 2017 do Ministério das Cidades

brasil_munic_2017  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201712.csv", 
                               col_names = TRUE)
names(brasil_munic_2017)[1]<- "codigo_ibge"
###Seleção de Dados Família


test5 <- brasil_munic_2017 %>% group_by(codigo_ibge,IN_PARC_MDS_FAM) %>% tally()
test5$IN_PARC_MDS_FAM[which(test5$IN_PARC_MDS_FAM=="NA")] <- "Sem Dados"
test5$IN_PARC_MDS_FAM[which(test5$IN_PARC_MDS_FAM=="0")] <- "Sem Código"
test5$IN_PARC_MDS_FAM[which(test5$IN_PARC_MDS_FAM=="000")] <- "Nenhuma"
test5$IN_PARC_MDS_FAM[which(test5$IN_PARC_MDS_FAM=="101")] <- "Família Cigana"
test5$IN_PARC_MDS_FAM[which(test5$IN_PARC_MDS_FAM=="201")] <- "Família Extrativista"
test5$IN_PARC_MDS_FAM[which(test5$IN_PARC_MDS_FAM=="202")] <- "Pescadores Artesanais"
test5$IN_PARC_MDS_FAM[which(test5$IN_PARC_MDS_FAM=="203")] <- "Família Pertencente a Comunidade de Terreiro"
test5$IN_PARC_MDS_FAM[which(test5$IN_PARC_MDS_FAM=="204")] <- "Família Ribeirinha"
test5$IN_PARC_MDS_FAM[which(test5$IN_PARC_MDS_FAM=="205")] <- "Família de Agricultores Familiares"
test5$IN_PARC_MDS_FAM[which(test5$IN_PARC_MDS_FAM=="301")] <- "Família Assentada da Reforma Agrária"
test5$IN_PARC_MDS_FAM[which(test5$IN_PARC_MDS_FAM=="302")] <- "Família Beneficiária do Programa Nacional de Crédito Fundiário"
test5$IN_PARC_MDS_FAM[which(test5$IN_PARC_MDS_FAM=="303")] <- "Família Acampada"
test5$IN_PARC_MDS_FAM[which(test5$IN_PARC_MDS_FAM=="304")] <- "Família Atingida por Empreendimentos de Infraestrutura"
test5$IN_PARC_MDS_FAM[which(test5$IN_PARC_MDS_FAM=="305")] <- "Família de Preso do Sistema Carcerário"
test5$IN_PARC_MDS_FAM[which(test5$IN_PARC_MDS_FAM=="306")] <- "Família de Catadores de Material Reciclável"

test5 <-subset(test5, IN_PARC_MDS_FAM!="Sem Dados" & IN_PARC_MDS_FAM!="Sem Código" 
               & IN_PARC_MDS_FAM!="Nenhuma" & IN_PARC_MDS_FAM!="Família Cigana"
               & IN_PARC_MDS_FAM!="Família Extrativista" & IN_PARC_MDS_FAM!="Pescadores Artesanais"
               & IN_PARC_MDS_FAM!="Família Pertencente a Comunidade de Terreiro" & IN_PARC_MDS_FAM!="Família Ribeirinha"
               & IN_PARC_MDS_FAM!="Família de Agricultores Familiares" & IN_PARC_MDS_FAM!="Família Assentada da Reforma Agrária"
               & IN_PARC_MDS_FAM!="Família Beneficiária do Programa Nacional de Crédito Fundiário" & IN_PARC_MDS_FAM!="Família Acampada"
               & IN_PARC_MDS_FAM!="Família Atingida por Empreendimentos de Infraestrutura" & IN_PARC_MDS_FAM!="Família de Preso do Sistema Carcerário")
               & IN_PARC_MDS_FAM!="Família Atingida por Empreendimentos de Infraestrutura" & IN_PARC_MDS_FAM!="Família de Catadores de Material Reciclável")


munic_2017_pop_rua <- merge(test5, brasil, by = "codigo_ibge")
munic_2017_pop_rua$geometry <- NULL
munic_2017_pop_rua$Ano <- "2017"
munic_2017_pop_rua <- na.omit(munic_2017_pop_rua)

write_xlsx(munic_2017_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/catadores_cidades_2017.xlsx")

### Tabela 2016 do Ministério das Cidades


brasil_munic_2016  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201612.csv", 
                               col_names = TRUE)
names(brasil_munic_2016)[1]<- "codigo_ibge"

###Seleção de Dados Família


test6 <- brasil_munic_2016 %>% group_by(codigo_ibge,IN_PARC_MDS_FAM) %>% tally()
test6$IN_PARC_MDS_FAM[which(test6$IN_PARC_MDS_FAM=="NA")] <- "Sem Dados"
test6$IN_PARC_MDS_FAM[which(test6$IN_PARC_MDS_FAM=="0")] <- "Sem Código"
test6$IN_PARC_MDS_FAM[which(test6$IN_PARC_MDS_FAM=="000")] <- "Nenhuma"
test6$IN_PARC_MDS_FAM[which(test6$IN_PARC_MDS_FAM=="101")] <- "Família Cigana"
test6$IN_PARC_MDS_FAM[which(test6$IN_PARC_MDS_FAM=="201")] <- "Família Extrativista"
test6$IN_PARC_MDS_FAM[which(test6$IN_PARC_MDS_FAM=="202")] <- "Pescadores Artesanais"
test6$IN_PARC_MDS_FAM[which(test6$IN_PARC_MDS_FAM=="203")] <- "Família Pertencente a Comunidade de Terreiro"
test6$IN_PARC_MDS_FAM[which(test6$IN_PARC_MDS_FAM=="204")] <- "Família Ribeirinha"
test6$IN_PARC_MDS_FAM[which(test6$IN_PARC_MDS_FAM=="205")] <- "Família de Agricultores Familiares"
test6$IN_PARC_MDS_FAM[which(test6$IN_PARC_MDS_FAM=="301")] <- "Família Assentada da Reforma Agrária"
test6$IN_PARC_MDS_FAM[which(test6$IN_PARC_MDS_FAM=="302")] <- "Família Beneficiária do Programa Nacional de Crédito Fundiário"
test6$IN_PARC_MDS_FAM[which(test6$IN_PARC_MDS_FAM=="303")] <- "Família Acampada"
test6$IN_PARC_MDS_FAM[which(test6$IN_PARC_MDS_FAM=="304")] <- "Família Atingida por Empreendimentos de Infraestrutura"
test6$IN_PARC_MDS_FAM[which(test6$IN_PARC_MDS_FAM=="305")] <- "Família de Preso do Sistema Carcerário"
test6$IN_PARC_MDS_FAM[which(test6$IN_PARC_MDS_FAM=="306")] <- "Família de Catadores de Material Reciclável"

test6 <-subset(test6, IN_PARC_MDS_FAM!="Sem Dados" & IN_PARC_MDS_FAM!="Sem Código" 
               & IN_PARC_MDS_FAM!="Nenhuma" & IN_PARC_MDS_FAM!="Família Cigana"
               & IN_PARC_MDS_FAM!="Família Extrativista" & IN_PARC_MDS_FAM!="Pescadores Artesanais"
               & IN_PARC_MDS_FAM!="Família Pertencente a Comunidade de Terreiro" & IN_PARC_MDS_FAM!="Família Ribeirinha"
               & IN_PARC_MDS_FAM!="Família de Agricultores Familiares" & IN_PARC_MDS_FAM!="Família Assentada da Reforma Agrária"
               & IN_PARC_MDS_FAM!="Família Beneficiária do Programa Nacional de Crédito Fundiário" & IN_PARC_MDS_FAM!="Família Acampada"
               & IN_PARC_MDS_FAM!="Família Atingida por Empreendimentos de Infraestrutura" & IN_PARC_MDS_FAM!="Família de Preso do Sistema Carcerário")


munic_2016_pop_rua <- merge(test6, brasil, by = "codigo_ibge")
munic_2016_pop_rua$geometry <- NULL
munic_2016_pop_rua$Ano <- "2016"
munic_2016_pop_rua <- na.omit(munic_2016_pop_rua)

write_xlsx(munic_2016_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/catadores_cidades_2016.xlsx")

### Tabela 2015 do Ministério das Cidades

brasil_munic_2015  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201512.csv", 
                               col_names = TRUE)
names(brasil_munic_2015)[1]<- "codigo_ibge"

###Seleção de Dados Família


test7 <- brasil_munic_2015 %>% group_by(codigo_ibge,IN_PARC_MDS_FAM) %>% tally()
test7$IN_PARC_MDS_FAM[which(test7$IN_PARC_MDS_FAM=="NA")] <- "Sem Dados"
test7$IN_PARC_MDS_FAM[which(test7$IN_PARC_MDS_FAM=="0")] <- "Sem Código"
test7$IN_PARC_MDS_FAM[which(test7$IN_PARC_MDS_FAM=="000")] <- "Nenhuma"
test7$IN_PARC_MDS_FAM[which(test7$IN_PARC_MDS_FAM=="101")] <- "Família Cigana"
test7$IN_PARC_MDS_FAM[which(test7$IN_PARC_MDS_FAM=="201")] <- "Família Extrativista"
test7$IN_PARC_MDS_FAM[which(test7$IN_PARC_MDS_FAM=="202")] <- "Pescadores Artesanais"
test7$IN_PARC_MDS_FAM[which(test7$IN_PARC_MDS_FAM=="203")] <- "Família Pertencente a Comunidade de Terreiro"
test7$IN_PARC_MDS_FAM[which(test7$IN_PARC_MDS_FAM=="204")] <- "Família Ribeirinha"
test7$IN_PARC_MDS_FAM[which(test7$IN_PARC_MDS_FAM=="205")] <- "Família de Agricultores Familiares"
test7$IN_PARC_MDS_FAM[which(test7$IN_PARC_MDS_FAM=="301")] <- "Família Assentada da Reforma Agrária"
test7$IN_PARC_MDS_FAM[which(test7$IN_PARC_MDS_FAM=="302")] <- "Família Beneficiária do Programa Nacional de Crédito Fundiário"
test7$IN_PARC_MDS_FAM[which(test7$IN_PARC_MDS_FAM=="303")] <- "Família Acampada"
test7$IN_PARC_MDS_FAM[which(test7$IN_PARC_MDS_FAM=="304")] <- "Família Atingida por Empreendimentos de Infraestrutura"
test7$IN_PARC_MDS_FAM[which(test7$IN_PARC_MDS_FAM=="305")] <- "Família de Preso do Sistema Carcerário"
test7$IN_PARC_MDS_FAM[which(test7$IN_PARC_MDS_FAM=="306")] <- "Família de Catadores de Material Reciclável"

test7 <-subset(test7, IN_PARC_MDS_FAM!="Sem Dados" & IN_PARC_MDS_FAM!="Sem Código" 
               & IN_PARC_MDS_FAM!="Nenhuma" & IN_PARC_MDS_FAM!="Família Cigana"
               & IN_PARC_MDS_FAM!="Família Extrativista" & IN_PARC_MDS_FAM!="Pescadores Artesanais"
               & IN_PARC_MDS_FAM!="Família Pertencente a Comunidade de Terreiro" & IN_PARC_MDS_FAM!="Família Ribeirinha"
               & IN_PARC_MDS_FAM!="Família de Agricultores Familiares" & IN_PARC_MDS_FAM!="Família Assentada da Reforma Agrária"
               & IN_PARC_MDS_FAM!="Família Beneficiária do Programa Nacional de Crédito Fundiário" & IN_PARC_MDS_FAM!="Família Acampada"
               & IN_PARC_MDS_FAM!="Família Atingida por Empreendimentos de Infraestrutura" & IN_PARC_MDS_FAM!="Família de Preso do Sistema Carcerário")

munic_2015_pop_rua <- merge(test7, brasil, by = "codigo_ibge")
munic_2015_pop_rua$geometry <- NULL
munic_2015_pop_rua$Ano <- "2015"
munic_2015_pop_rua <- na.omit(munic_2015_pop_rua)

write_xlsx(munic_2015_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/catadores_cidades_2015.xlsx")

### Tabela 2014 do Ministério das Cidades

brasil_munic_2014  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201412.csv", 
                               col_names = TRUE)
names(brasil_munic_2014)[1]<- "codigo_ibge"

###Seleção de Dados Família


test8 <- brasil_munic_2014 %>% group_by(codigo_ibge,IN_PARC_MDS_FAM) %>% tally()
test8$IN_PARC_MDS_FAM[which(test8$IN_PARC_MDS_FAM=="NA")] <- "Sem Dados"
test8$IN_PARC_MDS_FAM[which(test8$IN_PARC_MDS_FAM=="0")] <- "Sem Código"
test8$IN_PARC_MDS_FAM[which(test8$IN_PARC_MDS_FAM=="000")] <- "Nenhuma"
test8$IN_PARC_MDS_FAM[which(test8$IN_PARC_MDS_FAM=="101")] <- "Família Cigana"
test8$IN_PARC_MDS_FAM[which(test8$IN_PARC_MDS_FAM=="201")] <- "Família Extrativista"
test8$IN_PARC_MDS_FAM[which(test8$IN_PARC_MDS_FAM=="202")] <- "Pescadores Artesanais"
test8$IN_PARC_MDS_FAM[which(test8$IN_PARC_MDS_FAM=="203")] <- "Família Pertencente a Comunidade de Terreiro"
test8$IN_PARC_MDS_FAM[which(test8$IN_PARC_MDS_FAM=="204")] <- "Família Ribeirinha"
test8$IN_PARC_MDS_FAM[which(test8$IN_PARC_MDS_FAM=="205")] <- "Família de Agricultores Familiares"
test8$IN_PARC_MDS_FAM[which(test8$IN_PARC_MDS_FAM=="301")] <- "Família Assentada da Reforma Agrária"
test8$IN_PARC_MDS_FAM[which(test8$IN_PARC_MDS_FAM=="302")] <- "Família Beneficiária do Programa Nacional de Crédito Fundiário"
test8$IN_PARC_MDS_FAM[which(test8$IN_PARC_MDS_FAM=="303")] <- "Família Acampada"
test8$IN_PARC_MDS_FAM[which(test8$IN_PARC_MDS_FAM=="304")] <- "Família Atingida por Empreendimentos de Infraestrutura"
test8$IN_PARC_MDS_FAM[which(test8$IN_PARC_MDS_FAM=="305")] <- "Família de Preso do Sistema Carcerário"
test8$IN_PARC_MDS_FAM[which(test8$IN_PARC_MDS_FAM=="306")] <- "Família de Catadores de Material Reciclável"

test8 <-subset(test8, IN_PARC_MDS_FAM!="Sem Dados" & IN_PARC_MDS_FAM!="Sem Código" 
               & IN_PARC_MDS_FAM!="Nenhuma" & IN_PARC_MDS_FAM!="Família Cigana"
               & IN_PARC_MDS_FAM!="Família Extrativista" & IN_PARC_MDS_FAM!="Pescadores Artesanais"
               & IN_PARC_MDS_FAM!="Família Pertencente a Comunidade de Terreiro" & IN_PARC_MDS_FAM!="Família Ribeirinha"
               & IN_PARC_MDS_FAM!="Família de Agricultores Familiares" & IN_PARC_MDS_FAM!="Família Assentada da Reforma Agrária"
               & IN_PARC_MDS_FAM!="Família Beneficiária do Programa Nacional de Crédito Fundiário" & IN_PARC_MDS_FAM!="Família Acampada"
               & IN_PARC_MDS_FAM!="Família Atingida por Empreendimentos de Infraestrutura" & IN_PARC_MDS_FAM!="Família de Preso do Sistema Carcerário")

munic_2014_pop_rua <- merge(test8, brasil, by = "codigo_ibge")
munic_2014_pop_rua$geometry <- NULL
munic_2014_pop_rua$Ano <- "2014"
munic_2014_pop_rua <- na.omit(munic_2014_pop_rua)

write_xlsx(munic_2014_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/catadores_cidades_2014.xlsx")


### Tabela 2013 do Ministério das Cidades

brasil_munic_2013  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201312.csv", 
                               col_names = TRUE)
names(brasil_munic_2013)[1]<- "codigo_ibge"

###Seleção de Dados Família


test9 <- brasil_munic_2013 %>% group_by(codigo_ibge,IN_PARC_MDS_FAM) %>% tally()
test9$IN_PARC_MDS_FAM[which(test9$IN_PARC_MDS_FAM=="NA")] <- "Sem Dados"
test9$IN_PARC_MDS_FAM[which(test9$IN_PARC_MDS_FAM=="0")] <- "Sem Código"
test9$IN_PARC_MDS_FAM[which(test9$IN_PARC_MDS_FAM=="000")] <- "Nenhuma"
test9$IN_PARC_MDS_FAM[which(test9$IN_PARC_MDS_FAM=="101")] <- "Família Cigana"
test9$IN_PARC_MDS_FAM[which(test9$IN_PARC_MDS_FAM=="201")] <- "Família Extrativista"
test9$IN_PARC_MDS_FAM[which(test9$IN_PARC_MDS_FAM=="202")] <- "Pescadores Artesanais"
test9$IN_PARC_MDS_FAM[which(test9$IN_PARC_MDS_FAM=="203")] <- "Família Pertencente a Comunidade de Terreiro"
test9$IN_PARC_MDS_FAM[which(test9$IN_PARC_MDS_FAM=="204")] <- "Família Ribeirinha"
test9$IN_PARC_MDS_FAM[which(test9$IN_PARC_MDS_FAM=="205")] <- "Família de Agricultores Familiares"
test9$IN_PARC_MDS_FAM[which(test9$IN_PARC_MDS_FAM=="301")] <- "Família Assentada da Reforma Agrária"
test9$IN_PARC_MDS_FAM[which(test9$IN_PARC_MDS_FAM=="302")] <- "Família Beneficiária do Programa Nacional de Crédito Fundiário"
test9$IN_PARC_MDS_FAM[which(test9$IN_PARC_MDS_FAM=="303")] <- "Família Acampada"
test9$IN_PARC_MDS_FAM[which(test9$IN_PARC_MDS_FAM=="304")] <- "Família Atingida por Empreendimentos de Infraestrutura"
test9$IN_PARC_MDS_FAM[which(test9$IN_PARC_MDS_FAM=="305")] <- "Família de Preso do Sistema Carcerário"
test9$IN_PARC_MDS_FAM[which(test9$IN_PARC_MDS_FAM=="306")] <- "Família de Catadores de Material Reciclável"

test9 <-subset(test9, IN_PARC_MDS_FAM!="Sem Dados" & IN_PARC_MDS_FAM!="Sem Código" 
               & IN_PARC_MDS_FAM!="Nenhuma" & IN_PARC_MDS_FAM!="Família Cigana"
               & IN_PARC_MDS_FAM!="Família Extrativista" & IN_PARC_MDS_FAM!="Pescadores Artesanais"
               & IN_PARC_MDS_FAM!="Família Pertencente a Comunidade de Terreiro" & IN_PARC_MDS_FAM!="Família Ribeirinha"
               & IN_PARC_MDS_FAM!="Família de Agricultores Familiares" & IN_PARC_MDS_FAM!="Família Assentada da Reforma Agrária"
               & IN_PARC_MDS_FAM!="Família Beneficiária do Programa Nacional de Crédito Fundiário" & IN_PARC_MDS_FAM!="Família Acampada"
               & IN_PARC_MDS_FAM!="Família Atingida por Empreendimentos de Infraestrutura" & IN_PARC_MDS_FAM!="Família de Preso do Sistema Carcerário")

munic_2013_pop_rua <- merge(test9, brasil, by = "codigo_ibge")
munic_2013_pop_rua$geometry <- NULL
munic_2013_pop_rua$Ano <- "2013"
munic_2013_pop_rua <- na.omit(munic_2013_pop_rua)

write_xlsx(munic_2013_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/catadores_cidades_2013.xlsx")

### Tabela 2012 do Ministério das Cidades


brasil_munic_2012  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201212.csv", 
                               col_names = TRUE)
names(brasil_munic_2012)[1]<- "codigo_ibge"

###Seleção de Dados Família


test10 <- brasil_munic_2012 %>% group_by(codigo_ibge,IN_PARC_MDS_FAM) %>% tally()
test10$IN_PARC_MDS_FAM[which(test10$IN_PARC_MDS_FAM=="NA")] <- "Sem Dados"
test10$IN_PARC_MDS_FAM[which(test10$IN_PARC_MDS_FAM=="0")] <- "Sem Código"
test10$IN_PARC_MDS_FAM[which(test10$IN_PARC_MDS_FAM=="000")] <- "Nenhuma"
test10$IN_PARC_MDS_FAM[which(test10$IN_PARC_MDS_FAM=="101")] <- "Família Cigana"
test10$IN_PARC_MDS_FAM[which(test10$IN_PARC_MDS_FAM=="201")] <- "Família Extrativista"
test10$IN_PARC_MDS_FAM[which(test10$IN_PARC_MDS_FAM=="202")] <- "Pescadores Artesanais"
test10$IN_PARC_MDS_FAM[which(test10$IN_PARC_MDS_FAM=="203")] <- "Família Pertencente a Comunidade de Terreiro"
test10$IN_PARC_MDS_FAM[which(test10$IN_PARC_MDS_FAM=="204")] <- "Família Ribeirinha"
test10$IN_PARC_MDS_FAM[which(test10$IN_PARC_MDS_FAM=="205")] <- "Família de Agricultores Familiares"
test10$IN_PARC_MDS_FAM[which(test10$IN_PARC_MDS_FAM=="301")] <- "Família Assentada da Reforma Agrária"
test10$IN_PARC_MDS_FAM[which(test10$IN_PARC_MDS_FAM=="302")] <- "Família Beneficiária do Programa Nacional de Crédito Fundiário"
test10$IN_PARC_MDS_FAM[which(test10$IN_PARC_MDS_FAM=="303")] <- "Família Acampada"
test10$IN_PARC_MDS_FAM[which(test10$IN_PARC_MDS_FAM=="304")] <- "Família Atingida por Empreendimentos de Infraestrutura"
test10$IN_PARC_MDS_FAM[which(test10$IN_PARC_MDS_FAM=="305")] <- "Família de Preso do Sistema Carcerário"
test10$IN_PARC_MDS_FAM[which(test10$IN_PARC_MDS_FAM=="306")] <- "Família de Catadores de Material Reciclável"

test10 <-subset(test10, IN_PARC_MDS_FAM!="Sem Dados" & IN_PARC_MDS_FAM!="Sem Código" 
                & IN_PARC_MDS_FAM!="Nenhuma" & IN_PARC_MDS_FAM!="Família Cigana"
                & IN_PARC_MDS_FAM!="Família Extrativista" & IN_PARC_MDS_FAM!="Pescadores Artesanais"
                & IN_PARC_MDS_FAM!="Família Pertencente a Comunidade de Terreiro" & IN_PARC_MDS_FAM!="Família Ribeirinha"
                & IN_PARC_MDS_FAM!="Família de Agricultores Familiares" & IN_PARC_MDS_FAM!="Família Assentada da Reforma Agrária"
                & IN_PARC_MDS_FAM!="Família Beneficiária do Programa Nacional de Crédito Fundiário" & IN_PARC_MDS_FAM!="Família Acampada"
                & IN_PARC_MDS_FAM!="Família Atingida por Empreendimentos de Infraestrutura" & IN_PARC_MDS_FAM!="Família de Preso do Sistema Carcerário")

munic_2012_pop_rua <- merge(test10, brasil, by = "codigo_ibge")
munic_2012_pop_rua$geometry <- NULL
munic_2012_pop_rua$Ano <- "2012"
munic_2012_pop_rua <- na.omit(munic_2012_pop_rua)

write_xlsx(munic_2012_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/catadores_cidades_2012.xlsx")


##########


serie_historica_catadores_cidades <- rbind(munic_2021_pop_rua,
                                             munic_2020_pop_rua,
                                             munic_2019_pop_rua, 
                                             munic_2018_pop_rua, 
                                             munic_2017_pop_rua, 
                                             munic_2016_pop_rua, 
                                             munic_2015_pop_rua, 
                                             munic_2014_pop_rua, 
                                             munic_2013_pop_rua)


write_xlsx(serie_historica_carcerarios_cidades, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/catadores_familias_serie_historica_cidades.xlsx")

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


serie_historica_catadores_cidades_10 <- rbind(munic_2013_pop_rua_10,
                                                munic_2014_pop_rua_10, 
                                                munic_2015_pop_rua_10, 
                                                munic_2016_pop_rua_10, 
                                                munic_2017_pop_rua_10, 
                                                munic_2018_pop_rua_10, 
                                                munic_2019_pop_rua_10, 
                                                munic_2020_pop_rua_10, 
                                                munic_2021_pop_rua_10)

serie_historica_catadores_cidades_10 <- na.omit(serie_historica_catadores_cidades_10)

write_xlsx(serie_historica_catadores_cidades_10, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/catadores_familias_serie_historica_10_cidades.xlsx")
