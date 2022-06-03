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

test <- brasil_munic_2021 %>% group_by(codigo_ibge,FX_ETARIA) %>% tally()
test$FX_ETARIA[which(test$FX_ETARIA=="1")] <- "até 11 anos"
test$FX_ETARIA[which(test$FX_ETARIA=="2")] <- "de 12 a 17 anos"
test$FX_ETARIA[which(test$FX_ETARIA=="3")] <- "de 18 a 21 anos"
test$FX_ETARIA[which(test$FX_ETARIA=="4")] <- "de 22 a 29 anos"
test$FX_ETARIA[which(test$FX_ETARIA=="5")] <- "de 30 a 59 anos"
test$FX_ETARIA[which(test$FX_ETARIA=="6")] <- "de 60 anos acima"

test<-subset(test, FX_ETARIA!="de 12 a 17 anos" & FX_ETARIA!="de 18 a 21 anos"
             & FX_ETARIA!="de 22 a 29 anos" & FX_ETARIA!="de 30 a 59 anos"
             & FX_ETARIA!="de 60 anos acima")

munic_2021_pop_rua <- merge(test, brasil, by = "codigo_ibge")
munic_2021_pop_rua$geometry <- NULL
munic_2021_pop_rua$Ano <- "2021"
munic_2021_pop_rua <- na.omit(munic_2021_pop_rua)

write_xlsx(munic_2021_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/idade_criancas_cidades_2021.xlsx")



### Tabela 2020 do Ministério das Cidades

brasil_munic_2020  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_202012.csv", 
                        col_names = TRUE)
names(brasil_munic_2020)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test2 <- brasil_munic_2020 %>% group_by(codigo_ibge,FX_ETARIA) %>% tally()
test2$FX_ETARIA[which(test2$FX_ETARIA=="1")] <- "até 11 anos"
test2$FX_ETARIA[which(test2$FX_ETARIA=="2")] <- "de 12 a 17 anos"
test2$FX_ETARIA[which(test2$FX_ETARIA=="3")] <- "de 18 a 21 anos"
test2$FX_ETARIA[which(test2$FX_ETARIA=="4")] <- "de 22 a 29 anos"
test2$FX_ETARIA[which(test2$FX_ETARIA=="5")] <- "de 30 a 59 anos"
test2$FX_ETARIA[which(test2$FX_ETARIA=="6")] <- "de 60 anos acima"

test2<-subset(test2, FX_ETARIA!="de 12 a 17 anos" & FX_ETARIA!="de 18 a 21 anos"
                & FX_ETARIA!="de 22 a 29 anos" & FX_ETARIA!="de 30 a 59 anos"
                & FX_ETARIA!="de 60 anos acima")

munic_2020_pop_rua <- merge(test2, brasil, by = "codigo_ibge")
munic_2020_pop_rua$geometry <- NULL
munic_2020_pop_rua$Ano <- "2020"
munic_2020_pop_rua <- na.omit(munic_2020_pop_rua)

write_xlsx(munic_2020_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/idade_criancas_cidades_2020.xlsx")



### Tabela 2019 do Ministério das Cidades

brasil_munic_2019  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201912.csv", 
                        col_names = TRUE)
names(brasil_munic_2019)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test3 <- brasil_munic_2019 %>% group_by(codigo_ibge,FX_ETARIA) %>% tally()
test3$FX_ETARIA[which(test3$FX_ETARIA=="1")] <- "até 11 anos"
test3$FX_ETARIA[which(test3$FX_ETARIA=="2")] <- "de 12 a 17 anos"
test3$FX_ETARIA[which(test3$FX_ETARIA=="3")] <- "de 18 a 21 anos"
test3$FX_ETARIA[which(test3$FX_ETARIA=="4")] <- "de 22 a 29 anos"
test3$FX_ETARIA[which(test3$FX_ETARIA=="5")] <- "de 30 a 59 anos"
test3$FX_ETARIA[which(test3$FX_ETARIA=="6")] <- "de 60 anos acima"

test3<-subset(test3, FX_ETARIA!="de 12 a 17 anos" & FX_ETARIA!="de 18 a 21 anos"
                & FX_ETARIA!="de 22 a 29 anos" & FX_ETARIA!="de 30 a 59 anos"
                & FX_ETARIA!="de 60 anos acima")

munic_2019_pop_rua <- merge(test3, brasil, by = "codigo_ibge")
munic_2019_pop_rua$geometry <- NULL
munic_2019_pop_rua$Ano <- "2019"
munic_2019_pop_rua <- na.omit(munic_2019_pop_rua)

write_xlsx(munic_2019_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/idade_criancas_cidades_2019.xlsx")

### Tabela 2018 do Ministério das Cidades

brasil_munic_2018  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201812.csv", 
                        col_names = TRUE)
names(brasil_munic_2018)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test4 <- brasil_munic_2018 %>% group_by(codigo_ibge,FX_ETARIA) %>% tally()
test4$FX_ETARIA[which(test4$FX_ETARIA=="1")] <- "até 11 anos"
test4$FX_ETARIA[which(test4$FX_ETARIA=="2")] <- "de 12 a 17 anos"
test4$FX_ETARIA[which(test4$FX_ETARIA=="3")] <- "de 18 a 21 anos"
test4$FX_ETARIA[which(test4$FX_ETARIA=="4")] <- "de 22 a 29 anos"
test4$FX_ETARIA[which(test4$FX_ETARIA=="5")] <- "de 30 a 59 anos"
test4$FX_ETARIA[which(test4$FX_ETARIA=="6")] <- "de 60 anos acima"

test4<-subset(test4, FX_ETARIA!="de 12 a 17 anos" & FX_ETARIA!="de 18 a 21 anos"
                & FX_ETARIA!="de 22 a 29 anos" & FX_ETARIA!="de 30 a 59 anos"
                & FX_ETARIA!="de 60 anos acima")

munic_2018_pop_rua <- merge(test4, brasil, by = "codigo_ibge")
munic_2018_pop_rua$geometry <- NULL
munic_2018_pop_rua$Ano <- "2018"
munic_2018_pop_rua <- na.omit(munic_2018_pop_rua)

write_xlsx(munic_2018_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/idade_criancas_cidades_2018.xlsx")


### Tabela 2017 do Ministério das Cidades

brasil_munic_2017  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201712.csv", 
                        col_names = TRUE)
names(brasil_munic_2017)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test5 <- brasil_munic_2017 %>% group_by(codigo_ibge,FX_ETARIA) %>% tally()
test5$FX_ETARIA[which(test5$FX_ETARIA=="1")] <- "até 11 anos"
test5$FX_ETARIA[which(test5$FX_ETARIA=="2")] <- "de 12 a 17 anos"
test5$FX_ETARIA[which(test5$FX_ETARIA=="3")] <- "de 18 a 21 anos"
test5$FX_ETARIA[which(test5$FX_ETARIA=="4")] <- "de 22 a 29 anos"
test5$FX_ETARIA[which(test5$FX_ETARIA=="5")] <- "de 30 a 59 anos"
test5$FX_ETARIA[which(test5$FX_ETARIA=="6")] <- "de 60 anos acima"

test5<-subset(test5, FX_ETARIA!="de 12 a 17 anos" & FX_ETARIA!="de 18 a 21 anos"
                & FX_ETARIA!="de 22 a 29 anos" & FX_ETARIA!="de 30 a 59 anos"
                & FX_ETARIA!="de 60 anos acima")

munic_2017_pop_rua <- merge(test5, brasil, by = "codigo_ibge")
munic_2017_pop_rua$geometry <- NULL
munic_2017_pop_rua$Ano <- "2017"
munic_2017_pop_rua <- na.omit(munic_2017_pop_rua)

write_xlsx(munic_2017_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/idade_criancas_cidades_2017.xlsx")

### Tabela 2016 do Ministério das Cidades


brasil_munic_2016  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201612.csv", 
                        col_names = TRUE)
names(brasil_munic_2016)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test6 <- brasil_munic_2016 %>% group_by(codigo_ibge,FX_ETARIA) %>% tally()
test6$FX_ETARIA[which(test6$FX_ETARIA=="1")] <- "até 11 anos"
test6$FX_ETARIA[which(test6$FX_ETARIA=="2")] <- "de 12 a 17 anos"
test6$FX_ETARIA[which(test6$FX_ETARIA=="3")] <- "de 18 a 21 anos"
test6$FX_ETARIA[which(test6$FX_ETARIA=="4")] <- "de 22 a 29 anos"
test6$FX_ETARIA[which(test6$FX_ETARIA=="5")] <- "de 30 a 59 anos"
test6$FX_ETARIA[which(test6$FX_ETARIA=="6")] <- "de 60 anos acima"

test6<-subset(test6, FX_ETARIA!="de 12 a 17 anos" & FX_ETARIA!="de 18 a 21 anos"
                & FX_ETARIA!="de 22 a 29 anos" & FX_ETARIA!="de 30 a 59 anos"
                & FX_ETARIA!="de 60 anos acima")

munic_2016_pop_rua <- merge(test6, brasil, by = "codigo_ibge")
munic_2016_pop_rua$geometry <- NULL
munic_2016_pop_rua$Ano <- "2016"
munic_2016_pop_rua <- na.omit(munic_2016_pop_rua)

write_xlsx(munic_2016_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/idade_criancas_cidades_2016.xlsx")

### Tabela 2015 do Ministério das Cidades

brasil_munic_2015  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201512.csv", 
                        col_names = TRUE)
names(brasil_munic_2015)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test7 <- brasil_munic_2015 %>% group_by(codigo_ibge,FX_ETARIA) %>% tally()
test7$FX_ETARIA[which(test7$FX_ETARIA=="1")] <- "até 11 anos"
test7$FX_ETARIA[which(test7$FX_ETARIA=="2")] <- "de 12 a 17 anos"
test7$FX_ETARIA[which(test7$FX_ETARIA=="3")] <- "de 18 a 21 anos"
test7$FX_ETARIA[which(test7$FX_ETARIA=="4")] <- "de 22 a 29 anos"
test7$FX_ETARIA[which(test7$FX_ETARIA=="5")] <- "de 30 a 59 anos"
test7$FX_ETARIA[which(test7$FX_ETARIA=="6")] <- "de 60 anos acima"

test7<-subset(test7, FX_ETARIA!="de 12 a 17 anos" & FX_ETARIA!="de 18 a 21 anos"
                & FX_ETARIA!="de 22 a 29 anos" & FX_ETARIA!="de 30 a 59 anos"
                & FX_ETARIA!="de 60 anos acima")

munic_2015_pop_rua <- merge(test7, brasil, by = "codigo_ibge")
munic_2015_pop_rua$geometry <- NULL
munic_2015_pop_rua$Ano <- "2015"
munic_2015_pop_rua <- na.omit(munic_2015_pop_rua)

write_xlsx(munic_2015_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/idade_criancas_cidades_2015.xlsx")

### Tabela 2014 do Ministério das Cidades

brasil_munic_2014  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201412.csv", 
                        col_names = TRUE)
names(brasil_munic_2014)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test8 <- brasil_munic_2014 %>% group_by(codigo_ibge,FX_ETARIA) %>% tally()
test8$FX_ETARIA[which(test8$FX_ETARIA=="1")] <- "até 11 anos"
test8$FX_ETARIA[which(test8$FX_ETARIA=="2")] <- "de 12 a 17 anos"
test8$FX_ETARIA[which(test8$FX_ETARIA=="3")] <- "de 18 a 21 anos"
test8$FX_ETARIA[which(test8$FX_ETARIA=="4")] <- "de 22 a 29 anos"
test8$FX_ETARIA[which(test8$FX_ETARIA=="5")] <- "de 30 a 59 anos"
test8$FX_ETARIA[which(test8$FX_ETARIA=="6")] <- "de 60 anos acima"

test8<-subset(test8, FX_ETARIA!="de 12 a 17 anos" & FX_ETARIA!="de 18 a 21 anos"
                & FX_ETARIA!="de 22 a 29 anos" & FX_ETARIA!="de 30 a 59 anos"
                & FX_ETARIA!="de 60 anos acima")

munic_2014_pop_rua <- merge(test8, brasil, by = "codigo_ibge")
munic_2014_pop_rua$geometry <- NULL
munic_2014_pop_rua$Ano <- "2014"
munic_2014_pop_rua <- na.omit(munic_2014_pop_rua)

write_xlsx(munic_2014_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/idade_criancas_cidades_2014.xlsx")


### Tabela 2013 do Ministério das Cidades

brasil_munic_2013  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201312.csv", 
                        col_names = TRUE)
names(brasil_munic_2013)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test9 <- brasil_munic_2013 %>% group_by(codigo_ibge,FX_ETARIA) %>% tally()
test9$FX_ETARIA[which(test9$FX_ETARIA=="1")] <- "até 11 anos"
test9$FX_ETARIA[which(test9$FX_ETARIA=="2")] <- "de 12 a 17 anos"
test9$FX_ETARIA[which(test9$FX_ETARIA=="3")] <- "de 18 a 21 anos"
test9$FX_ETARIA[which(test9$FX_ETARIA=="4")] <- "de 22 a 29 anos"
test9$FX_ETARIA[which(test9$FX_ETARIA=="5")] <- "de 30 a 59 anos"
test9$FX_ETARIA[which(test9$FX_ETARIA=="6")] <- "de 60 anos acima"

test9<-subset(test9, FX_ETARIA!="de 12 a 17 anos" & FX_ETARIA!="de 18 a 21 anos"
                & FX_ETARIA!="de 22 a 29 anos" & FX_ETARIA!="de 30 a 59 anos"
                & FX_ETARIA!="de 60 anos acima")

munic_2013_pop_rua <- merge(test9, brasil, by = "codigo_ibge")
munic_2013_pop_rua$geometry <- NULL
munic_2013_pop_rua$Ano <- "2013"
munic_2013_pop_rua <- na.omit(munic_2013_pop_rua)

write_xlsx(munic_2013_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/idade_criancas_cidades_2013.xlsx")

### Tabela 2012 do Ministério das Cidades


brasil_munic_2012  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201212.csv", 
                        col_names = TRUE)
names(brasil_munic_2012)[1]<- "codigo_ibge"
###10 Seleção de Dados pa FX_ETARIA

test10 <- brasil_munic_2012 %>% group_by(codigo_ibge,FX_ETARIA) %>% tally()
test10$FX_ETARIA[which(test10$FX_ETARIA=="1")] <- "até 11 anos"
test10$FX_ETARIA[which(test10$FX_ETARIA=="2")] <- "de 12 a 17 anos"
test10$FX_ETARIA[which(test10$FX_ETARIA=="3")] <- "de 18 a 21 anos"
test10$FX_ETARIA[which(test10$FX_ETARIA=="4")] <- "de 22 a 29 anos"
test10$FX_ETARIA[which(test10$FX_ETARIA=="5")] <- "de 30 a 59 anos"
test10$FX_ETARIA[which(test10$FX_ETARIA=="6")] <- "de 60 anos acima"

test10<-subset(test10, FX_ETARIA!="de 12 a 17 anos" & FX_ETARIA!="de 18 a 21 anos"
                & FX_ETARIA!="de 22 a 29 anos" & FX_ETARIA!="de 30 a 59 anos"
                & FX_ETARIA!="de 60 anos acima")

munic_2012_pop_rua <- merge(test10, brasil, by = "codigo_ibge")
munic_2012_pop_rua$geometry <- NULL
munic_2012_pop_rua$Ano <- "2012"
munic_2012_pop_rua <- na.omit(munic_2012_pop_rua)

write_xlsx(munic_2012_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/idade_criancas_cidades_2012.xlsx")


##########


serie_historica_criancas_cidades <- rbind(munic_2021_pop_rua,
                                            munic_2020_pop_rua,
                                            munic_2019_pop_rua, 
                                            munic_2018_pop_rua, 
                                            munic_2017_pop_rua, 
                                            munic_2016_pop_rua, 
                                            munic_2015_pop_rua, 
                                            munic_2014_pop_rua, 
                                            munic_2013_pop_rua, 
                                            munic_2012_pop_rua)


write_xlsx(serie_historica_criancas_cidades, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/serie_historica_criancas_10_cidades.xlsx")

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



serie_historica_criancas_cidades_10 <- rbind(munic_2012_pop_rua_10,
                                          munic_2013_pop_rua_10,
                                          munic_2014_pop_rua_10, 
                                          munic_2015_pop_rua_10, 
                                          munic_2016_pop_rua_10, 
                                          munic_2017_pop_rua_10, 
                                          munic_2018_pop_rua_10, 
                                          munic_2019_pop_rua_10, 
                                          munic_2020_pop_rua_10, 
                                          munic_2021_pop_rua_10)

write_xlsx(serie_historica_criancas_cidades_10, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/serie_historica_criancas_10_cidades.xlsx")

