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


###10 Seleção de Dados pa FX_ETARIA

test <- brasil_munic_2021 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test$CO_SEXO_PESSOA[which(test$CO_SEXO_PESSOA=="1")] <- "masculino"
test$CO_SEXO_PESSOA[which(test$CO_SEXO_PESSOA=="2")] <- "feminino"

test<-subset(test, CO_SEXO_PESSOA!="masculino")

munic_2021_pop_rua <- merge(test, brasil, by = "codigo_ibge")
munic_2021_pop_rua$geometry <- NULL
munic_2021_pop_rua$Ano <- "2021"
munic_2021_pop_rua <- na.omit(munic_2021_pop_rua)

write_xlsx(munic_2021_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/mulheres_cidades_2021.xlsx")


### Tabela 2020 do Ministério das Cidades

brasil_munic_2020  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_202012.csv", 
                               col_names = TRUE)
names(brasil_munic_2020)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test2 <- brasil_munic_2020 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test2$CO_SEXO_PESSOA[which(test2$CO_SEXO_PESSOA=="1")] <- "masculino"
test2$CO_SEXO_PESSOA[which(test2$CO_SEXO_PESSOA=="2")] <- "feminino"

test2<-subset(test2, CO_SEXO_PESSOA!="masculino")

munic_2020_pop_rua <- merge(test2, brasil, by = "codigo_ibge")
munic_2020_pop_rua$geometry <- NULL
munic_2020_pop_rua$Ano <- "2020"
munic_2020_pop_rua <- na.omit(munic_2020_pop_rua)

write_xlsx(munic_2020_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/mulheres_cidades_2020.xlsx")


### Tabela 2019 do Ministério das Cidades

brasil_munic_2019  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201912.csv", 
                               col_names = TRUE)
names(brasil_munic_2019)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test3 <- brasil_munic_2019 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test3$CO_SEXO_PESSOA[which(test3$CO_SEXO_PESSOA=="1")] <- "masculino"
test3$CO_SEXO_PESSOA[which(test3$CO_SEXO_PESSOA=="2")] <- "feminino"

test3<-subset(test3, CO_SEXO_PESSOA!="masculino")

munic_2019_pop_rua <- merge(test3, brasil, by = "codigo_ibge")
munic_2019_pop_rua$geometry <- NULL
munic_2019_pop_rua$Ano <- "2019"
munic_2019_pop_rua <- na.omit(munic_2019_pop_rua)

write_xlsx(munic_2019_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/mulheres_cidades_2019.xlsx")

### Tabela 2018 do Ministério das Cidades

brasil_munic_2018  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201812.csv", 
                               col_names = TRUE)
names(brasil_munic_2018)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test4 <- brasil_munic_2018 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test4$CO_SEXO_PESSOA[which(test4$CO_SEXO_PESSOA=="1")] <- "masculino"
test4$CO_SEXO_PESSOA[which(test4$CO_SEXO_PESSOA=="2")] <- "feminino"

test4<-subset(test4, CO_SEXO_PESSOA!="masculino")

munic_2018_pop_rua <- merge(test4, brasil, by = "codigo_ibge")
munic_2018_pop_rua$geometry <- NULL
munic_2018_pop_rua$Ano <- "2018"
munic_2018_pop_rua <- na.omit(munic_2018_pop_rua)

write_xlsx(munic_2018_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/mulheres_cidades_2018.xlsx")


### Tabela 2017 do Ministério das Cidades

brasil_munic_2017  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201712.csv", 
                               col_names = TRUE)
names(brasil_munic_2017)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test5 <- brasil_munic_2017 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test5$CO_SEXO_PESSOA[which(test5$CO_SEXO_PESSOA=="1")] <- "masculino"
test5$CO_SEXO_PESSOA[which(test5$CO_SEXO_PESSOA=="2")] <- "feminino"

test5<-subset(test5, CO_SEXO_PESSOA!="masculino")

munic_2017_pop_rua <- merge(test5, brasil, by = "codigo_ibge")
munic_2017_pop_rua$geometry <- NULL
munic_2017_pop_rua$Ano <- "2017"
munic_2017_pop_rua <- na.omit(munic_2017_pop_rua)

write_xlsx(munic_2017_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/mulheres_cidades_2017.xlsx")

### Tabela 2016 do Ministério das Cidades


brasil_munic_2016  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201612.csv", 
                               col_names = TRUE)
names(brasil_munic_2016)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test6 <- brasil_munic_2016 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test6$CO_SEXO_PESSOA[which(test6$CO_SEXO_PESSOA=="1")] <- "masculino"
test6$CO_SEXO_PESSOA[which(test6$CO_SEXO_PESSOA=="2")] <- "feminino"

test6<-subset(test6, CO_SEXO_PESSOA!="masculino")

munic_2016_pop_rua <- merge(test6, brasil, by = "codigo_ibge")
munic_2016_pop_rua$geometry <- NULL
munic_2016_pop_rua$Ano <- "2016"
munic_2016_pop_rua <- na.omit(munic_2016_pop_rua)

write_xlsx(munic_2016_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/mulheres_cidades_2016.xlsx")

### Tabela 2015 do Ministério das Cidades

brasil_munic_2015  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201512.csv", 
                               col_names = TRUE)
names(brasil_munic_2015)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test7 <- brasil_munic_2015 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test7$CO_SEXO_PESSOA[which(test7$CO_SEXO_PESSOA=="1")] <- "masculino"
test7$CO_SEXO_PESSOA[which(test7$CO_SEXO_PESSOA=="2")] <- "feminino"

test7<-subset(test7, CO_SEXO_PESSOA!="masculino")

munic_2015_pop_rua <- merge(test7, brasil, by = "codigo_ibge")
munic_2015_pop_rua$geometry <- NULL
munic_2015_pop_rua$Ano <- "2015"
munic_2015_pop_rua <- na.omit(munic_2015_pop_rua)

write_xlsx(munic_2015_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/mulheres_cidades_2015.xlsx")

### Tabela 2014 do Ministério das Cidades

brasil_munic_2014  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201412.csv", 
                               col_names = TRUE)
names(brasil_munic_2014)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test8 <- brasil_munic_2014 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test8$CO_SEXO_PESSOA[which(test8$CO_SEXO_PESSOA=="1")] <- "masculino"
test8$CO_SEXO_PESSOA[which(test8$CO_SEXO_PESSOA=="2")] <- "feminino"

test8<-subset(test8, CO_SEXO_PESSOA!="masculino")

munic_2014_pop_rua <- merge(test8, brasil, by = "codigo_ibge")
munic_2014_pop_rua$geometry <- NULL
munic_2014_pop_rua$Ano <- "2014"
munic_2014_pop_rua <- na.omit(munic_2014_pop_rua)

write_xlsx(munic_2014_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/mulheres_cidades_2014.xlsx")


### Tabela 2013 do Ministério das Cidades

brasil_munic_2013  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201312.csv", 
                               col_names = TRUE)
names(brasil_munic_2013)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test9 <- brasil_munic_2013 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test9$CO_SEXO_PESSOA[which(test9$CO_SEXO_PESSOA=="1")] <- "masculino"
test9$CO_SEXO_PESSOA[which(test9$CO_SEXO_PESSOA=="2")] <- "feminino"

test9<-subset(test9, CO_SEXO_PESSOA!="masculino")

munic_2013_pop_rua <- merge(test9, brasil, by = "codigo_ibge")
munic_2013_pop_rua$geometry <- NULL
munic_2013_pop_rua$Ano <- "2013"
munic_2013_pop_rua <- na.omit(munic_2013_pop_rua)

write_xlsx(munic_2013_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/mulheres_cidades_2013.xlsx")

### Tabela 2012 do Ministério das Cidades


brasil_munic_2012  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201212.csv", 
                               col_names = TRUE)
names(brasil_munic_2012)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test10 <- brasil_munic_2012 %>% group_by(codigo_ibge,CO_SEXO_PESSOA) %>% tally()
test10$CO_SEXO_PESSOA[which(test10$CO_SEXO_PESSOA=="1")] <- "masculino"
test10$CO_SEXO_PESSOA[which(test10$CO_SEXO_PESSOA=="2")] <- "feminino"

test10<-subset(test10, CO_SEXO_PESSOA!="masculino")

munic_2012_pop_rua <- merge(test10, brasil, by = "codigo_ibge")
munic_2012_pop_rua$geometry <- NULL
munic_2012_pop_rua$Ano <- "2012"
munic_2012_pop_rua <- na.omit(munic_2012_pop_rua)

write_xlsx(munic_2012_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/mulheres_cidades_2012.xlsx")


##########


serie_historica_mulheres_cidades <- rbind(munic_2021_pop_rua,
                                        munic_2020_pop_rua,
                                        munic_2019_pop_rua, 
                                        munic_2018_pop_rua, 
                                        munic_2017_pop_rua, 
                                        munic_2016_pop_rua, 
                                        munic_2015_pop_rua, 
                                        munic_2014_pop_rua, 
                                        munic_2013_pop_rua, 
                                        munic_2012_pop_rua)


write_xlsx(serie_historica_mulheres_cidades, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/mulheres_serie_historica_cidades.xlsx")

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



serie_historica_mulheres_cidades_10 <- rbind(munic_2012_pop_rua_10,
                                           munic_2013_pop_rua_10,
                                           munic_2014_pop_rua_10, 
                                           munic_2015_pop_rua_10, 
                                           munic_2016_pop_rua_10, 
                                           munic_2017_pop_rua_10, 
                                           munic_2018_pop_rua_10, 
                                           munic_2019_pop_rua_10, 
                                           munic_2020_pop_rua_10, 
                                           munic_2021_pop_rua_10)

write_xlsx(serie_historica_mulheres_cidades_10, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/mulheres_serie_historica_10_cidades.xlsx")
