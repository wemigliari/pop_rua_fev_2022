library(readxl)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(extrafont)
library(RColorBrewer)
library(ggrepel)
library(reshape2)
library(writexl)

totais <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/serie_historica_capitais_observatorio.xlsx")
totais <- data.frame(totais)

# Plot
library(hrbrthemes)
library(ggrepel)

totais %>%
  ggplot(aes(x=Ano, y=Curitiba)) +
  geom_line( color="grey") +
  geom_point(shape=21, color="black", fill="#69b3a2", size=3) +
  theme_ipsum() +
  ggtitle("População em Situação de Rua no Município de Curitiba, 2012-2021") +
  labs(x ="", y = "Pessoas em Situação de Rua em Curitiba",
       subtitle = "Série Histórica, Ministério da Cidadania",
       caption = "Fonte: Ministério da Cidadania. Elaborado por W. Migliari, 2022.") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0))) +
  geom_label_repel(aes(label = Belo.Horizonte),
                   box.padding   = 0.45, 
                   point.padding = 0.9,
                   size= 3,
                   segment.color = 'grey50') 

###### Auxílio Brasil

curitiba_totais2021_bf <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2021.xlsx", 
                                   sheet="Bolsa Família")
curitiba_totais2021_bf  <- data.frame(curitiba_totais2021_bf)
curitiba_totais2021_bf$...1 <- NULL
curitiba_totais2021_bf$Ano <- "2021"
names(curitiba_totais2021_bf)[1] <- "MARC_PBF"

curitiba_totais2020_bf <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2020.xlsx", 
                                   sheet="Bolsa Família")
curitiba_totais2020_bf  <- data.frame(curitiba_totais2020_bf)
curitiba_totais2020_bf$...1 <- NULL
curitiba_totais2020_bf$Ano <- "2020"

curitiba_totais2019_bf <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2019.xlsx", 
                                   sheet="Bolsa Família")
curitiba_totais2019_bf  <- data.frame(curitiba_totais2019_bf)
curitiba_totais2019_bf$...1 <- NULL
curitiba_totais2019_bf$Ano <- "2019"

curitiba_totais2018_bf <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2018.xlsx", 
                                   sheet="Bolsa Família")
curitiba_totais2018_bf  <- data.frame(curitiba_totais2018_bf)
curitiba_totais2018_bf$...1 <- NULL
curitiba_totais2018_bf$Ano <- "2018"


curitiba_totais2017_bf <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2017.xlsx", 
                                   sheet="Bolsa Família")
curitiba_totais2017_bf  <- data.frame(curitiba_totais2017_bf)
curitiba_totais2017_bf$...1 <- NULL
curitiba_totais2017_bf$Ano <- "2017"

curitiba_totais2016_bf <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2016.xlsx", 
                                   sheet="Bolsa Família")
curitiba_totais2016_bf  <- data.frame(curitiba_totais2016_bf)
curitiba_totais2016_bf$...1 <- NULL
curitiba_totais2016_bf$Ano <- "2016"

curitiba_totais2015_bf <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2015.xlsx", 
                                   sheet="Bolsa Família")
curitiba_totais2015_bf  <- data.frame(curitiba_totais2015_bf)
curitiba_totais2015_bf$...1 <- NULL
curitiba_totais2015_bf$Ano <- "2015"

curitiba_totais2014_bf <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2014.xlsx", 
                                   sheet="Bolsa Família")
curitiba_totais2014_bf  <- data.frame(curitiba_totais2014_bf)
curitiba_totais2014_bf$...1 <- NULL
curitiba_totais2014_bf$Ano <- "2014"

curitiba_totais2013_bf <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2013.xlsx", 
                                   sheet="Bolsa Família")
curitiba_totais2013_bf  <- data.frame(curitiba_totais2013_bf)
curitiba_totais2013_bf$...1 <- NULL
curitiba_totais2013_bf$Ano <- "2013"

curitiba_totais2012_bf <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2012.xlsx", 
                                   sheet="Bolsa Família")
curitiba_totais2012_bf  <- data.frame(curitiba_totais2012_bf)
curitiba_totais2012_bf$...1 <- NULL
curitiba_totais2012_bf$Ano <- "2012"


serie_historica_bf <- rbind(curitiba_totais2021_bf, 
                               curitiba_totais2020_bf, 
                               curitiba_totais2019_bf,
                               curitiba_totais2018_bf,
                               curitiba_totais2017_bf,
                               curitiba_totais2016_bf,
                               curitiba_totais2015_bf,
                               curitiba_totais2014_bf,
                               curitiba_totais2013_bf,
                               curitiba_totais2012_bf)

write_xlsx(serie_historica_bf, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/serie_historica_bf.xlsx")

ggplot(serie_historica_bf, aes(x = Ano, y = freq, fill = MARC_PBF)) + 
  geom_bar(stat = "identity") +
  labs(x ="", y = "Pessoas Situação de Rua",
       subtitle = "Série Histórica 2012-2021 de Pessoas em Situação de Rua em Curitiba, Bolsa Família/Auxílio Brasil",
       caption = "Fonte: Ministério da Cidadania. Elaborado por W. Migliari, 2022.") +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(t = 5, r = 20, b = 5, l = 5))) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="gray", size = 12),
        axis.title.y = element_text(face="bold", colour="gray", size = 12),
        legend.title = element_text(face="bold", size = 10)) +
  scale_fill_manual(values = c("#DCDCDC", "#778899")) +
  theme(legend.title=element_blank())


###### Cor

curitiba_totais2021_cor <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2021.xlsx", 
                                   sheet="Cor")
curitiba_totais2021_cor  <- data.frame(curitiba_totais2021_cor)
curitiba_totais2021_cor$...1 <- NULL
curitiba_totais2021_cor$Ano <- "2021"

curitiba_totais2020_cor <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2020.xlsx", 
                                   sheet="Cor")
curitiba_totais2020_cor  <- data.frame(curitiba_totais2020_cor)
curitiba_totais2020_cor$...1 <- NULL
curitiba_totais2020_cor$Ano <- "2020"

curitiba_totais2019_cor <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2019.xlsx", 
                                   sheet="Cor")
curitiba_totais2019_cor  <- data.frame(curitiba_totais2019_cor)
curitiba_totais2019_cor$...1 <- NULL
curitiba_totais2019_cor$Ano <- "2019"

curitiba_totais2018_cor <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2018.xlsx", 
                                   sheet="Cor")
curitiba_totais2018_cor  <- data.frame(curitiba_totais2018_cor)
curitiba_totais2018_cor$...1 <- NULL
curitiba_totais2018_cor$Ano <- "2018"


curitiba_totais2017_cor <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2017.xlsx", 
                                   sheet="Cor")
curitiba_totais2017_cor  <- data.frame(curitiba_totais2017_cor)
curitiba_totais2017_cor$...1 <- NULL
curitiba_totais2017_cor$Ano <- "2017"

curitiba_totais2016_cor <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2016.xlsx", 
                                   sheet="Cor")
curitiba_totais2016_cor  <- data.frame(curitiba_totais2016_cor)
curitiba_totais2016_cor$...1 <- NULL
curitiba_totais2016_cor$Ano <- "2016"

curitiba_totais2015_cor <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2015.xlsx", 
                                   sheet="Cor")
curitiba_totais2015_cor  <- data.frame(curitiba_totais2015_cor)
curitiba_totais2015_cor$...1 <- NULL
curitiba_totais2015_cor$Ano <- "2015"

curitiba_totais2014_cor <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2014.xlsx", 
                                   sheet="Cor")
curitiba_totais2014_cor  <- data.frame(curitiba_totais2014_cor)
curitiba_totais2014_cor$...1 <- NULL
curitiba_totais2014_cor$Ano <- "2014"

curitiba_totais2013_cor <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2013.xlsx", 
                                   sheet="Cor")
curitiba_totais2013_cor  <- data.frame(curitiba_totais2013_cor)
curitiba_totais2013_cor$...1 <- NULL
curitiba_totais2013_cor$Ano <- "2013"

curitiba_totais2012_cor <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2012.xlsx", 
                                   sheet="Cor")
curitiba_totais2012_cor <- data.frame(curitiba_totais2012_cor)
curitiba_totais2012_cor$...1 <- NULL
curitiba_totais2012_cor$Ano <- "2012"


serie_historica_cor <- rbind(curitiba_totais2021_cor, 
                               curitiba_totais2020_cor, 
                               curitiba_totais2019_cor,
                               curitiba_totais2018_cor,
                               curitiba_totais2017_cor,
                               curitiba_totais2016_cor,
                               curitiba_totais2015_cor,
                               curitiba_totais2014_cor,
                               curitiba_totais2013_cor,
                               curitiba_totais2012_cor)

write_xlsx(serie_historica_cor, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/serie_historica_cor.xlsx")


ggplot(serie_historica_cor, aes(x = Ano, y = freq, fill = CO_RACA_COR_PESSOA)) + 
  geom_bar(stat = "identity") +
  labs(x ="", y = "Pessoas Situação de Rua",
       subtitle = "Série Histórica 2012-2021 de Pessoas em Situação de Rua em Curitiba por Cor",
       caption = "Fonte: Ministério da Cidadania. Elaborado por W. Migliari, 2022.") +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(t = 5, r = 20, b = 5, l = 5))) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="gray", size = 12),
        axis.title.y = element_text(face="bold", colour="gray", size = 12),
        legend.title = element_text(face="bold", size = 10)) +
  scale_fill_manual(values = c("#808080", "#778899", "#C0C0C0","#DCDCDC", "#f6f6f6", "#2F4F4F")) +
  theme(legend.title=element_blank())


###### Sexo

curitiba_totais2021_sexo <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2021.xlsx", 
                                sheet="Sexo")
curitiba_totais2021_sexo  <- data.frame(curitiba_totais2021_sexo)
curitiba_totais2021_sexo$...1 <- NULL
curitiba_totais2021_sexo$Ano <- "2021"

curitiba_totais2020_sexo <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2020.xlsx", 
                                  sheet="Sexo")
curitiba_totais2020_sexo  <- data.frame(curitiba_totais2020_sexo)
curitiba_totais2020_sexo$...1 <- NULL
curitiba_totais2020_sexo$Ano <- "2020"

curitiba_totais2019_sexo <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2019.xlsx", 
                                  sheet="Sexo")
curitiba_totais2019_sexo  <- data.frame(curitiba_totais2019_sexo)
curitiba_totais2019_sexo$...1 <- NULL
curitiba_totais2019_sexo$Ano <- "2019"

curitiba_totais2018_sexo <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2018.xlsx", 
                                  sheet="Sexo")
curitiba_totais2018_sexo  <- data.frame(curitiba_totais2018_sexo)
curitiba_totais2018_sexo$...1 <- NULL
curitiba_totais2018_sexo$Ano <- "2018"


curitiba_totais2017_sexo <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2017.xlsx", 
                                  sheet="Sexo")
curitiba_totais2017_sexo  <- data.frame(curitiba_totais2017_sexo)
curitiba_totais2017_sexo$...1 <- NULL
curitiba_totais2017_sexo$Ano <- "2017"

curitiba_totais2016_sexo <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2016.xlsx", 
                                  sheet="Sexo")
curitiba_totais2016_sexo  <- data.frame(curitiba_totais2016_sexo)
curitiba_totais2016_sexo$...1 <- NULL
curitiba_totais2016_sexo$Ano <- "2016"

curitiba_totais2015_sexo <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2015.xlsx", 
                                  sheet="Sexo")
curitiba_totais2015_sexo  <- data.frame(curitiba_totais2015_sexo)
curitiba_totais2015_sexo$...1 <- NULL
curitiba_totais2015_sexo$Ano <- "2015"

curitiba_totais2014_sexo <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2014.xlsx", 
                                  sheet="Sexo")
curitiba_totais2014_sexo  <- data.frame(curitiba_totais2014_sexo)
curitiba_totais2014_sexo$...1 <- NULL
curitiba_totais2014_sexo$Ano <- "2014"

curitiba_totais2013_sexo <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2013.xlsx", 
                                  sheet="Sexo")
curitiba_totais2013_sexo  <- data.frame(curitiba_totais2013_sexo)
curitiba_totais2013_sexo$...1 <- NULL
curitiba_totais2013_sexo$Ano <- "2013"

curitiba_totais2012_sexo <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2012.xlsx", 
                                  sheet="Sexo")
curitiba_totais2012_sexo  <- data.frame(curitiba_totais2012_sexo)
curitiba_totais2012_sexo$...1 <- NULL
curitiba_totais2012_sexo$Ano <- "2012"


serie_historica_sexo <- rbind(curitiba_totais2021_sexo, 
                              curitiba_totais2020_sexo, 
                              curitiba_totais2019_sexo,
                              curitiba_totais2018_sexo,
                              curitiba_totais2017_sexo,
                              curitiba_totais2016_sexo,
                              curitiba_totais2015_sexo,
                              curitiba_totais2014_sexo,
                              curitiba_totais2013_sexo,
                              curitiba_totais2012_sexo)

write.xlsx(serie_historica_sexo, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/serie_historica_sexo.xlsx")

# Plot

ggplot(data = serie_historica_sexo, aes(x = Ano, y = freq, fill = CO_SEXO_PESSOA)) +
  geom_bar(stat = "identity", position = position_dodge(), alpha = 0.85)  +
  geom_text(
    aes(label = freq),
    colour = c("#808080"), size = 2.3,
    vjust = 1.7, position = position_dodge(.9)
  ) +
  labs(x ="", y = "Pessoas em Situação de Rua",
       subtitle = "Sexo Série Histórica 2012-2021 de Pessoas em Situação de Rua em Curitiba por Sexo",
       caption = "Fonte: Ministério da Cidadania. Elaborado por W. Migliari, 2022.") +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(t = 5, r = 20, b = 5, l = 5))) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="gray", size = 15),
        axis.title.y = element_text(face="bold", colour="gray", size = 15),
        legend.title = element_text(face="bold", size = 10)) +
  scale_fill_manual("", values = c("Feminino" = "#DCDCDC", "Masculino" = "#2F4F4F"))

###### Renda

curitiba_totais2021_renda <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2021.xlsx", 
                                   sheet="Renda")
curitiba_totais2021_renda  <- data.frame(curitiba_totais2021_renda)
curitiba_totais2021_renda$...1 <- NULL
curitiba_totais2021_renda$Ano <- "2021"

curitiba_totais2020_renda <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2020.xlsx", 
                                   sheet="Renda")
curitiba_totais2020_renda  <- data.frame(curitiba_totais2020_renda)
curitiba_totais2020_renda$...1 <- NULL
curitiba_totais2020_renda$Ano <- "2020"

curitiba_totais2019_renda <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2019.xlsx", 
                                   sheet="Renda")
curitiba_totais2019_renda  <- data.frame(curitiba_totais2019_renda)
curitiba_totais2019_renda$...1 <- NULL
curitiba_totais2019_renda$Ano <- "2019"

curitiba_totais2018_renda <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2018.xlsx", 
                                   sheet="Renda")
curitiba_totais2018_renda  <- data.frame(curitiba_totais2018_renda)
curitiba_totais2018_renda$...1 <- NULL
curitiba_totais2018_renda$Ano <- "2018"

curitiba_totais2017_renda <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2017.xlsx", 
                                   sheet="Renda")
curitiba_totais2017_renda  <- data.frame(curitiba_totais2017_renda)
curitiba_totais2017_renda$...1 <- NULL
curitiba_totais2017_renda$Ano <- "2017"

curitiba_totais2016_renda <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2016.xlsx", 
                                   sheet="Renda")
curitiba_totais2016_renda  <- data.frame(curitiba_totais2016_renda)
curitiba_totais2016_renda$...1 <- NULL
curitiba_totais2016_renda$Ano <- "2016"

curitiba_totais2015_renda <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2015.xlsx", 
                                   sheet="Renda")
curitiba_totais2015_renda  <- data.frame(curitiba_totais2015_renda)
curitiba_totais2015_renda$...1 <- NULL
curitiba_totais2015_renda$Ano <- "2015"

curitiba_totais2014_renda <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2014.xlsx", 
                                   sheet="Renda")
curitiba_totais2014_renda  <- data.frame(curitiba_totais2014_renda)
curitiba_totais2014_renda$...1 <- NULL
curitiba_totais2014_renda$Ano <- "2014"

curitiba_totais2013_renda <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2013.xlsx", 
                                   sheet="Renda")
curitiba_totais2013_renda  <- data.frame(curitiba_totais2013_renda)
curitiba_totais2013_renda$...1 <- NULL
curitiba_totais2013_renda$Ano <- "2013"

curitiba_totais2012_renda <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2012.xlsx", 
                                   sheet="Renda")
curitiba_totais2012_renda  <- data.frame(curitiba_totais2012_renda)
curitiba_totais2012_renda$...1 <- NULL
curitiba_totais2012_renda$Ano <- "2012"

serie_historica_renda <- rbind(curitiba_totais2021_renda, 
                               curitiba_totais2020_renda, 
                               curitiba_totais2019_renda,
                               curitiba_totais2018_renda,
                               curitiba_totais2017_renda,
                               curitiba_totais2016_renda,
                               curitiba_totais2015_renda,
                               curitiba_totais2014_renda,
                               curitiba_totais2013_renda,
                               curitiba_totais2012_renda)

write.xlsx(serie_historica_renda, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/serie_historica_renda.xlsx")


#Plot


ggplot(serie_historica_renda, aes(x = Ano, y = freq, fill = FX_RENDA)) + 
  geom_bar(stat = "identity") +
  labs(x ="", y = "Pessoas Situação de Rua",
       subtitle = "Série Histórica 2012-2021 de Pessoas em Situação de Rua em Curitiba por Renda",
       caption = "Fonte: Ministério da Cidadania. Elaborado por W. Migliari, 2022.") +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(t = 5, r = 20, b = 5, l = 5))) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="gray", size = 12),
        axis.title.y = element_text(face="bold", colour="gray", size = 12),
        legend.title = element_text(face="bold", size = 10)) +
  scale_fill_manual(values = c("#808080", "#C0C0C0","#DCDCDC", "#2F4F4F")) +
  theme(legend.title=element_blank())


###### Idade

curitiba_totais2021_idade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2021.xlsx", 
                                   sheet="Idade")
curitiba_totais2021_idade  <- data.frame(curitiba_totais2021_idade)
curitiba_totais2021_idade$...1 <- NULL
curitiba_totais2021_idade$Ano <- "2021"

curitiba_totais2020_idade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2020.xlsx", 
                                   sheet="Idade")
curitiba_totais2020_idade  <- data.frame(curitiba_totais2020_idade)
curitiba_totais2020_idade$...1 <- NULL
curitiba_totais2020_idade$Ano <- "2020"

curitiba_totais2019_idade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2019.xlsx", 
                                   sheet="Idade")
curitiba_totais2019_idade  <- data.frame(curitiba_totais2019_idade)
curitiba_totais2019_idade$...1 <- NULL
curitiba_totais2019_idade$Ano <- "2019"

curitiba_totais2018_idade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2018.xlsx", 
                                   sheet="Idade")
curitiba_totais2018_idade  <- data.frame(curitiba_totais2018_idade)
curitiba_totais2018_idade$...1 <- NULL
curitiba_totais2018_idade$Ano <- "2018"

curitiba_totais2017_idade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2017.xlsx", 
                                   sheet="Idade")
curitiba_totais2017_idade  <- data.frame(curitiba_totais2017_idade)
curitiba_totais2017_idade$...1 <- NULL
curitiba_totais2017_idade$Ano <- "2017"

curitiba_totais2016_idade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2016.xlsx", 
                                   sheet="Idade")
curitiba_totais2016_idade  <- data.frame(curitiba_totais2016_idade)
curitiba_totais2016_idade$...1 <- NULL
curitiba_totais2016_idade$Ano <- "2016"

curitiba_totais2015_idade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2015.xlsx", 
                                   sheet="Idade")
curitiba_totais2015_idade  <- data.frame(curitiba_totais2015_idade)
curitiba_totais2015_idade$...1 <- NULL
curitiba_totais2015_idade$Ano <- "2015"

curitiba_totais2014_idade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2014.xlsx", 
                                   sheet="Idade")
curitiba_totais2014_idade  <- data.frame(curitiba_totais2014_idade)
curitiba_totais2014_idade$...1 <- NULL
curitiba_totais2014_idade$Ano <- "2014"

curitiba_totais2013_idade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2013.xlsx", 
                                   sheet="Idade")
curitiba_totais2013_idade  <- data.frame(curitiba_totais2013_idade)
curitiba_totais2013_idade$...1 <- NULL
curitiba_totais2013_idade$Ano <- "2013"

curitiba_totais2012_idade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2012.xlsx", 
                                   sheet="Idade")
curitiba_totais2012_idade  <- data.frame(curitiba_totais2012_idade)
curitiba_totais2012_idade$...1 <- NULL
curitiba_totais2012_idade$Ano <- "2012"

serie_historica_idade <- rbind(curitiba_totais2021_idade, 
                               curitiba_totais2020_idade, 
                               curitiba_totais2019_idade,
                               curitiba_totais2018_idade,
                               curitiba_totais2017_idade,
                               curitiba_totais2016_idade,
                               curitiba_totais2015_idade,
                               curitiba_totais2014_idade,
                               curitiba_totais2013_idade,
                               curitiba_totais2012_idade)

write_xlsx(serie_historica_idade, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/serie_historica_idade.xlsx")

#Idade

ggplot(serie_historica_idade, aes(x = Ano, y = freq, fill = FX_ETARIA)) + 
  geom_bar(stat = "identity") +
  labs(x ="", y = "Pessoas Situação de Rua",
       subtitle = "Série Histórica 2012-2021 de Pessoas em Situação de Rua em Curitiba por Idade",
       caption = "Fonte: Ministério da Cidadania. Elaborado por W. Migliari, 2022.") +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(t = 5, r = 20, b = 5, l = 5))) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="gray", size = 12),
        axis.title.y = element_text(face="bold", colour="gray", size = 12),
        legend.title = element_text(face="bold", size = 10)) +
  scale_fill_manual(values = c("#808080", "#C0C0C0","#DCDCDC", "#f6f6f6", "#778899", "#2F4F4F")) +
  theme(legend.title=element_blank())



###### Escolaridade

curitiba_totais2021_escolaridade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2021.xlsx", 
                                sheet="Instrução")
curitiba_totais2021_escolaridade <- data.frame(curitiba_totais2021_escolaridade)
curitiba_totais2021_escolaridade$...1 <- NULL
curitiba_totais2021_escolaridade$Ano <- "2021"

curitiba_totais2020_escolaridade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2020.xlsx", 
                                          sheet="Instrução")
curitiba_totais2020_escolaridade <- data.frame(curitiba_totais2020_escolaridade)
curitiba_totais2020_escolaridade$...1 <- NULL
curitiba_totais2020_escolaridade$Ano <- "2020"


curitiba_totais2019_escolaridade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2019.xlsx", 
                                          sheet="Instrução")
curitiba_totais2019_escolaridade <- data.frame(curitiba_totais2019_escolaridade)
curitiba_totais2019_escolaridade$...1 <- NULL
curitiba_totais2019_escolaridade$Ano <- "2019"

curitiba_totais2018_escolaridade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2018.xlsx", 
                                          sheet="Instrução")
curitiba_totais2018_escolaridade <- data.frame(curitiba_totais2018_escolaridade)
curitiba_totais2018_escolaridade$...1 <- NULL
curitiba_totais2018_escolaridade$Ano <- "2018"

curitiba_totais2017_escolaridade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2017.xlsx", 
                                          sheet="Instrução")
curitiba_totais2017_escolaridade <- data.frame(curitiba_totais2017_escolaridade)
curitiba_totais2017_escolaridade$...1 <- NULL
curitiba_totais2017_escolaridade$Ano <- "2017"

curitiba_totais2016_escolaridade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2016.xlsx", 
                                          sheet="Instrução")
curitiba_totais2016_escolaridade <- data.frame(curitiba_totais2016_escolaridade)
curitiba_totais2016_escolaridade$...1 <- NULL
curitiba_totais2016_escolaridade$Ano <- "2016"

curitiba_totais2015_escolaridade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2015.xlsx", 
                                          sheet="Instrução")
curitiba_totais2015_escolaridade <- data.frame(curitiba_totais2015_escolaridade)
curitiba_totais2015_escolaridade$...1 <- NULL
curitiba_totais2015_escolaridade$Ano <- "2015"

curitiba_totais2014_escolaridade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2014.xlsx", 
                                          sheet="Instrução")
curitiba_totais2014_escolaridade <- data.frame(curitiba_totais2014_escolaridade)
curitiba_totais2014_escolaridade$...1 <- NULL
curitiba_totais2014_escolaridade$Ano <- "2014"

curitiba_totais2013_escolaridade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2013.xlsx", 
                                          sheet="Instrução")
curitiba_totais2013_escolaridade <- data.frame(curitiba_totais2013_escolaridade)
curitiba_totais2013_escolaridade$...1 <- NULL
curitiba_totais2013_escolaridade$Ano <- "2013"

curitiba_totais2012_escolaridade <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2012.xlsx", 
                                          sheet="Instrução")
curitiba_totais2012_escolaridade <- data.frame(curitiba_totais2012_escolaridade)
curitiba_totais2012_escolaridade$...1 <- NULL
curitiba_totais2012_escolaridade$Ano <- "2012"

serie_historica_escolaridade <- rbind(curitiba_totais2021_escolaridade, 
                               curitiba_totais2020_escolaridade, 
                               curitiba_totais2019_escolaridade,
                               curitiba_totais2018_escolaridade,
                               curitiba_totais2017_escolaridade,
                               curitiba_totais2016_escolaridade,
                               curitiba_totais2015_escolaridade,
                               curitiba_totais2014_escolaridade,
                               curitiba_totais2013_escolaridade,
                               curitiba_totais2012_escolaridade)

write.xlsx(serie_historica_escolaridade, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/serie_historica_escolaridade.xlsx")


ggplot(serie_historica_escolaridade, aes(x = Ano, y = freq, fill = GRAU_INSTRUCAO)) + 
  geom_bar(stat = "identity") +
  labs(x ="", y = "Pessoas Situação de Rua",
       subtitle = "Série Histórica 2012-2021 de Pessoas em Situação de Rua em Curitiba por Escolaridade",
       caption = "Fonte: Ministério da Cidadania. Elaborado por W. Migliari, 2022.") +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(t = 5, r = 20, b = 5, l = 5))) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="gray", size = 12),
        axis.title.y = element_text(face="bold", colour="gray", size = 12),
        legend.title = element_text(face="bold", size = 10)) +
  scale_fill_manual(values = c("#808080", "#C0C0C0","#DCDCDC", "#f6f6f6", "black", "#778899", "#2F4F4F", "steelblue", "slategray", "gold")) +
  theme(legend.title=element_blank())



###### Saber Ler & Escrever

curitiba_totais2021_ler_escrever <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2021.xlsx", 
                                          sheet="Ler & Escrever")
curitiba_totais2021_ler_escrever <- data.frame(curitiba_totais2021_ler_escrever )
curitiba_totais2021_ler_escrever$...1 <- NULL
curitiba_totais2021_ler_escrever$Ano <- "2021"


curitiba_totais2020_ler_escrever <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2020.xlsx", 
                                          sheet="Ler & Escrever")
curitiba_totais2020_ler_escrever <- data.frame(curitiba_totais2020_ler_escrever )
curitiba_totais2020_ler_escrever$...1 <- NULL
curitiba_totais2020_ler_escrever$Ano <- "2020"


curitiba_totais2019_ler_escrever <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2019.xlsx", 
                                          sheet="Ler & Escrever")
curitiba_totais2019_ler_escrever <- data.frame(curitiba_totais2019_ler_escrever )
curitiba_totais2019_ler_escrever$...1 <- NULL
curitiba_totais2019_ler_escrever$Ano <- "2019"


curitiba_totais2018_ler_escrever <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2018.xlsx", 
                                          sheet="Ler & Escrever")
curitiba_totais2018_ler_escrever <- data.frame(curitiba_totais2018_ler_escrever )
curitiba_totais2018_ler_escrever$...1 <- NULL
curitiba_totais2018_ler_escrever$Ano <- "2018"


curitiba_totais2017_ler_escrever <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2017.xlsx", 
                                          sheet="Ler & Escrever")
curitiba_totais2017_ler_escrever <- data.frame(curitiba_totais2017_ler_escrever )
curitiba_totais2017_ler_escrever$...1 <- NULL
curitiba_totais2017_ler_escrever$Ano <- "2017"


curitiba_totais2016_ler_escrever <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2016.xlsx", 
                                          sheet="Ler & Escrever")
curitiba_totais2016_ler_escrever <- data.frame(curitiba_totais2016_ler_escrever )
curitiba_totais2016_ler_escrever$...1 <- NULL
curitiba_totais2016_ler_escrever$Ano <- "2016"


curitiba_totais2015_ler_escrever <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2015.xlsx", 
                                          sheet="Ler & Escrever")
curitiba_totais2015_ler_escrever <- data.frame(curitiba_totais2015_ler_escrever )
curitiba_totais2015_ler_escrever$...1 <- NULL
curitiba_totais2015_ler_escrever$Ano <- "2015"


curitiba_totais2014_ler_escrever <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2014.xlsx", 
                                          sheet="Ler & Escrever")
curitiba_totais2014_ler_escrever <- data.frame(curitiba_totais2014_ler_escrever )
curitiba_totais2014_ler_escrever$...1 <- NULL
curitiba_totais2014_ler_escrever$Ano <- "2014"

curitiba_totais2013_ler_escrever <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2013.xlsx", 
                                          sheet="Ler & Escrever")
curitiba_totais2013_ler_escrever <- data.frame(curitiba_totais2013_ler_escrever )
curitiba_totais2013_ler_escrever$...1 <- NULL
curitiba_totais2013_ler_escrever$Ano <- "2013"


curitiba_totais2012_ler_escrever <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2012.xlsx", 
                                          sheet="Ler & Escrever")
curitiba_totais2012_ler_escrever <- data.frame(curitiba_totais2012_ler_escrever )
curitiba_totais2012_ler_escrever$...1 <- NULL
curitiba_totais2012_ler_escrever$Ano <- "2012"



serie_historica_ler_escrever <- rbind(curitiba_totais2021_ler_escrever, 
                                      curitiba_totais2020_ler_escrever, 
                                      curitiba_totais2019_ler_escrever,
                                      curitiba_totais2018_ler_escrever,
                                      curitiba_totais2017_ler_escrever,
                                      curitiba_totais2016_ler_escrever,
                                      curitiba_totais2015_ler_escrever,
                                      curitiba_totais2014_ler_escrever,
                                      curitiba_totais2013_ler_escrever,
                                      curitiba_totais2012_ler_escrever)

write.xlsx(serie_historica_ler_escrever, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/serie_historica_ler_escrever.xlsx")


ggplot(serie_historica_ler_escrever, aes(x = Ano, y = freq, fill = CO_SABE_LER_ESCREVER_MEMB)) + 
  geom_bar(stat = "identity") +
  labs(x ="", y = "Pessoas Situação de Rua",
       subtitle = "Série Histórica 2012-2021 de Pessoas em Situação de Rua em Curitiba, Saber Ler & Escrever",
       caption = "Fonte: Ministério da Cidadania. Elaborado por W. Migliari, 2022.") +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(t = 5, r = 20, b = 5, l = 5))) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="gray", size = 12),
        axis.title.y = element_text(face="bold", colour="gray", size = 12),
        legend.title = element_text(face="bold", size = 10)) +
  scale_fill_manual(values = c("#778899", "#DCDCDC", "#C0C0C0")) +
  theme(legend.title=element_blank())



###### Atualização Cadastral

curitiba_totais2021_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2021.xlsx", 
                                sheet="Atualização")
curitiba_totais2021_atua_cadas   <- data.frame(curitiba_totais2021_atua_cadas)
curitiba_totais2021_atua_cadas$...1 <- NULL
curitiba_totais2021_atua_cadas$Ano <- "2021"
curitiba_totais2021_atua_cadas$Min <- min(curitiba_totais2021_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
curitiba_totais2021_atua_cadas$Max <- max(curitiba_totais2021_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)

curitiba_totais2020_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2020.xlsx", 
                                        sheet="Atualização")
curitiba_totais2020_atua_cadas   <- data.frame(curitiba_totais2020_atua_cadas)
curitiba_totais2020_atua_cadas$...1 <- NULL
curitiba_totais2020_atua_cadas$Ano <- "2020"
curitiba_totais2020_atua_cadas$Min <- min(curitiba_totais2020_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
curitiba_totais2020_atua_cadas$Max <- max(curitiba_totais2020_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)


curitiba_totais2019_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2019.xlsx", 
                                        sheet="Atualização")
curitiba_totais2019_atua_cadas   <- data.frame(curitiba_totais2019_atua_cadas)
curitiba_totais2019_atua_cadas$...1 <- NULL
curitiba_totais2019_atua_cadas$Ano <- "2019"
curitiba_totais2019_atua_cadas$Min <- min(curitiba_totais2019_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
curitiba_totais2019_atua_cadas$Max <- max(curitiba_totais2019_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)



curitiba_totais2018_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2018.xlsx", 
                                        sheet="Atualização")
curitiba_totais2018_atua_cadas   <- data.frame(curitiba_totais2018_atua_cadas)
curitiba_totais2018_atua_cadas$...1 <- NULL
curitiba_totais2018_atua_cadas$Ano <- "2018"
curitiba_totais2018_atua_cadas$Min <- min(curitiba_totais2018_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
curitiba_totais2018_atua_cadas$Max <- max(curitiba_totais2018_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)


curitiba_totais2017_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2017.xlsx", 
                                        sheet="Atualização")
curitiba_totais2017_atua_cadas   <- data.frame(curitiba_totais2017_atua_cadas)
curitiba_totais2017_atua_cadas$...1 <- NULL
curitiba_totais2017_atua_cadas$Ano <- "2017"
curitiba_totais2017_atua_cadas$Min <- min(curitiba_totais2017_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
curitiba_totais2017_atua_cadas$Max <- max(curitiba_totais2017_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)


curitiba_totais2016_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2016.xlsx", 
                                        sheet="Atualização")
curitiba_totais2016_atua_cadas   <- data.frame(curitiba_totais2016_atua_cadas)
curitiba_totais2016_atua_cadas$...1 <- NULL
curitiba_totais2016_atua_cadas$Ano <- "2016"
curitiba_totais2016_atua_cadas$Min <- min(curitiba_totais2016_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
curitiba_totais2016_atua_cadas$Max <- max(curitiba_totais2016_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)


curitiba_totais2015_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2015.xlsx", 
                                        sheet="Atualização")
curitiba_totais2015_atua_cadas   <- data.frame(curitiba_totais2015_atua_cadas)
curitiba_totais2015_atua_cadas$...1 <- NULL
curitiba_totais2015_atua_cadas$Ano <- "2015"
curitiba_totais2015_atua_cadas$Min <- min(curitiba_totais2015_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
curitiba_totais2015_atua_cadas$Max <- max(curitiba_totais2015_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)


curitiba_totais2014_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2014.xlsx", 
                                        sheet="Atualização")
curitiba_totais2014_atua_cadas   <- data.frame(curitiba_totais2014_atua_cadas)
curitiba_totais2014_atua_cadas$...1 <- NULL
curitiba_totais2014_atua_cadas$Ano <- "2014"
curitiba_totais2014_atua_cadas$Min <- min(curitiba_totais2014_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
curitiba_totais2014_atua_cadas$Max <- max(curitiba_totais2014_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)


curitiba_totais2013_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2013.xlsx", 
                                        sheet="Atualização")
curitiba_totais2013_atua_cadas   <- data.frame(curitiba_totais2013_atua_cadas)
curitiba_totais2013_atua_cadas$...1 <- NULL
curitiba_totais2013_atua_cadas$Ano <- "2013"
curitiba_totais2013_atua_cadas$Min <- min(curitiba_totais2013_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
curitiba_totais2013_atua_cadas$Max <- max(curitiba_totais2013_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)



curitiba_totais2012_atua_cadas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2012.xlsx", 
                                        sheet="Atualização")
curitiba_totais2012_atua_cadas   <- data.frame(curitiba_totais2012_atua_cadas)
curitiba_totais2012_atua_cadas$...1 <- NULL
curitiba_totais2012_atua_cadas$Ano <- "2012"
curitiba_totais2012_atua_cadas$Min <- min(curitiba_totais2012_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)
curitiba_totais2012_atua_cadas$Max <- max(curitiba_totais2012_atua_cadas$MESES_APOS_ULT_ATUALIZACAO)



serie_historica_cadastro <- rbind(curitiba_totais2021_atua_cadas, 
                                      curitiba_totais2020_atua_cadas, 
                                      curitiba_totais2019_atua_cadas,
                                      curitiba_totais2018_atua_cadas,
                                      curitiba_totais2017_atua_cadas,
                                      curitiba_totais2016_atua_cadas,
                                      curitiba_totais2015_atua_cadas,
                                      curitiba_totais2014_atua_cadas,
                                      curitiba_totais2013_atua_cadas,
                                      curitiba_totais2012_atua_cadas)

write.xlsx(serie_historica_cadastro, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/serie_historica_cadastro.xlsx")


serie_historica_cadastro$Ano <- as.numeric(serie_historica_cadastro$Ano)

################

library(ggthemes)

ggplot(serie_historica_cadastro, aes(Ano, MESES_APOS_ULT_ATUALIZACAO)) +
  geom_line(aes(group = Max)) +
  geom_point(aes(color = Max), size = 1.5) +
  coord_flip() +
  labs(x ="", y = "Meses Após a Útlima Atualização",
       subtitle = "Série Histórica 2012-2021 de Pessoas em Situação de Rua em Curitiba, Atualização Cadastral",
       caption = "Fonte: Ministério da Cidadania. Elaborado por W. Migliari, 2022.") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_y_continuous(limits=c(0, 120), breaks=c(0, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120))

##

ggplot(serie_historica_cadastro, aes(MESES_APOS_ULT_ATUALIZACAO, freq)) +
  geom_line(aes(group = Ano)) +
  geom_point(aes(color = Ano), size = 2.5, alpha=0.3) +
  labs(x ="", y = "Pessoas em Situação de Rua",
       subtitle = "Série Histórica 2012-2021 de Pessoas em Situação de Rua em Curitiba, Atualização Cadastral",
       caption = "Fonte: Ministério da Cidadania. Elaborado por W. Migliari, 2022.") +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(t = 5, r = 20, b = 5, l = 5))) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="gray", size = 12),
        axis.title.y = element_text(face="bold", colour="gray", size = 12),
        legend.title = element_text(face="bold", size = 10)) +
  theme(legend.position = "none") +
  scale_x_continuous(limits=c(0, 120), breaks=c(0, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120)) 

## Zoom no gráfico anterior retirando os três outliers

serie_historica_cadastro[order(-serie_historica_cadastro$freq),]

ggplot(serie_historica_cadastro, aes(MESES_APOS_ULT_ATUALIZACAO, freq)) +
  geom_line(aes(group = Ano)) +
  geom_point(aes(color = Ano), size = 2.5, alpha=0.3) +
  labs(x ="", y = "Pessoas em Situação de Rua",
       subtitle = "Série Histórica 2012-2021 de Pessoas em Situação de Rua em Curitiba, Atualização Cadastral",
       caption = "Fonte: Ministério da Cidadania. Elaborado por W. Migliari, 2022.") +
  theme_minimal() +
  theme(legend.position = "none") +
  scale_x_continuous(limits=c(0, 120), breaks=c(0, 12, 24, 36, 48, 60, 72, 84, 96, 108, 120)) +
  scale_y_continuous(limits = c(0, 3500)) +
  theme(axis.title.y = element_text(margin = margin(t = 5, r = 20, b = 5, l = 5))) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="gray", size = 12),
        axis.title.y = element_text(face="bold", colour="gray", size = 12),
        legend.title = element_text(face="bold", size = 10)) +
  annotate("rect", xmin = 0, xmax = 24, ymin = 0, ymax = 3500,
           alpha = .1) +
  annotate("rect", xmin = 24, xmax = 36, ymin = 0, ymax = 3500,
           alpha = .2) +
  annotate("rect", xmin = 36, xmax = 120, ymin = 0, ymax = 3500,
           alpha = .3) +
  annotate(geom="text", x=12, y=3000, label="+++",
           color="black") +
  annotate(geom="text", x=30, y=3000, label="++",
           color="black") +
  annotate(geom="text", x=48, y=3000, label="+",
           color="black") 

##################

###### Indígenas

curitiba_totais2021_indigenas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2021.xlsx", 
                                        sheet="Indígenas")
curitiba_totais2021_indigenas   <- data.frame(curitiba_totais2021_indigenas)
curitiba_totais2021_indigenas$...1 <- NULL
curitiba_totais2021_indigenas$Ano <- "2021"

curitiba_totais2020_indigenas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2020.xlsx", 
                                       sheet="Indígenas")
curitiba_totais2020_indigenas   <- data.frame(curitiba_totais2020_indigenas)
curitiba_totais2020_indigenas$...1 <- NULL
curitiba_totais2020_indigenas$Ano <- "2020"


curitiba_totais2019_indigenas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2019.xlsx", 
                                       sheet="Indígenas")
curitiba_totais2019_indigenas   <- data.frame(curitiba_totais2019_indigenas)
curitiba_totais2019_indigenas$...1 <- NULL
curitiba_totais2019_indigenas$Ano <- "2019"


curitiba_totais2018_indigenas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2018.xlsx", 
                                       sheet="Indígenas")
curitiba_totais2018_indigenas   <- data.frame(curitiba_totais2018_indigenas)
curitiba_totais2018_indigenas$...1 <- NULL
curitiba_totais2018_indigenas$Ano <- "2018"


curitiba_totais2017_indigenas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2017.xlsx", 
                                       sheet="Indígenas")
curitiba_totais2017_indigenas   <- data.frame(curitiba_totais2017_indigenas)
curitiba_totais2017_indigenas$...1 <- NULL
curitiba_totais2017_indigenas$Ano <- "2017"


curitiba_totais2016_indigenas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2016.xlsx", 
                                       sheet="Indígenas")
curitiba_totais2016_indigenas   <- data.frame(curitiba_totais2016_indigenas)
curitiba_totais2016_indigenas$...1 <- NULL
curitiba_totais2016_indigenas$Ano <- "2016"


curitiba_totais2015_indigenas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2015.xlsx", 
                                       sheet="Indígenas")
curitiba_totais2015_indigenas   <- data.frame(curitiba_totais2015_indigenas)
curitiba_totais2015_indigenas$...1 <- NULL
curitiba_totais2015_indigenas$Ano <- "2015"


curitiba_totais2014_indigenas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2014.xlsx", 
                                       sheet="Indígenas")
curitiba_totais2014_indigenas   <- data.frame(curitiba_totais2014_indigenas)
curitiba_totais2014_indigenas$...1 <- NULL
curitiba_totais2014_indigenas$Ano <- "2014"


curitiba_totais2013_indigenas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2013.xlsx", 
                                       sheet="Indígenas")
curitiba_totais2013_indigenas   <- data.frame(curitiba_totais2013_indigenas)
curitiba_totais2013_indigenas$...1 <- NULL
curitiba_totais2013_indigenas$Ano <- "2013"


curitiba_totais2012_indigenas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2012.xlsx", 
                                       sheet="Indígenas")
curitiba_totais2012_indigenas   <- data.frame(curitiba_totais2012_indigenas)
curitiba_totais2012_indigenas$...1 <- NULL
curitiba_totais2012_indigenas$Ano <- "2012"

serie_historica_indigenas <- rbind(curitiba_totais2021_indigenas, 
                                      curitiba_totais2020_indigenas, 
                                      curitiba_totais2019_indigenas,
                                      curitiba_totais2018_indigenas,
                                      curitiba_totais2017_indigenas,
                                      curitiba_totais2016_indigenas,
                                      curitiba_totais2015_indigenas,
                                      curitiba_totais2014_indigenas,
                                      curitiba_totais2013_indigenas,
                                      curitiba_totais2012_indigenas)


write.xlsx(serie_historica_indigenas, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/serie_historica_indigenas.xlsx")


ggplot(serie_historica_indigenas, aes(x = Ano, y = freq, fill = IN_FAMILIA_INDIGENA_FAM)) + 
  geom_bar(stat = "identity") +
  labs(x ="", y = "Pessoas Situação de Rua",
       subtitle = "Série Histórica 2012-2021 de Pessoas em Situação de Rua em Curitiba, Indígenas",
       caption = "Fonte: Ministério da Cidadania. Elaborado por W. Migliari, 2022.") +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(t = 5, r = 20, b = 5, l = 5))) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="gray", size = 12),
        axis.title.y = element_text(face="bold", colour="gray", size = 12),
        legend.title = element_text(face="bold", size = 10)) +
  scale_fill_manual(values = c("#778899", "#DCDCDC", "#C0C0C0")) +
  theme(legend.title=element_blank()) +
  geom_line(aes(x = Ano, y = freq), size = 1, color="gray", group = 1) +
  annotate(geom="text", x="2015", y=40000, label="Nenhum respondente 'Sim'",
           color="gray")


###### Quilombolas

curitiba_totais2021_quilombolas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2021.xlsx", 
                                       sheet="Quilombolas")
curitiba_totais2021_quilombolas   <- data.frame(curitiba_totais2021_quilombolas)
curitiba_totais2021_quilombolas$...1 <- NULL
curitiba_totais2021_quilombolas$Ano <- "2021"


curitiba_totais2020_quilombolas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2020.xlsx", 
                                         sheet="Quilombolas")
curitiba_totais2020_quilombolas   <- data.frame(curitiba_totais2020_quilombolas)
curitiba_totais2020_quilombolas$...1 <- NULL
curitiba_totais2020_quilombolas$Ano <- "2020"


curitiba_totais2019_quilombolas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2019.xlsx", 
                                         sheet="Quilombolas")
curitiba_totais2019_quilombolas   <- data.frame(curitiba_totais2019_quilombolas)
curitiba_totais2019_quilombolas$...1 <- NULL
curitiba_totais2019_quilombolas$Ano <- "2019"



curitiba_totais2018_quilombolas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2018.xlsx", 
                                         sheet="Quilombolas")
curitiba_totais2018_quilombolas   <- data.frame(curitiba_totais2018_quilombolas)
curitiba_totais2018_quilombolas$...1 <- NULL
curitiba_totais2018_quilombolas$Ano <- "2018"



curitiba_totais2017_quilombolas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2017.xlsx", 
                                         sheet="Quilombolas")
curitiba_totais2017_quilombolas   <- data.frame(curitiba_totais2017_quilombolas)
curitiba_totais2017_quilombolas$...1 <- NULL
curitiba_totais2017_quilombolas$Ano <- "2017"


curitiba_totais2016_quilombolas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2016.xlsx", 
                                         sheet="Quilombolas")
curitiba_totais2016_quilombolas   <- data.frame(curitiba_totais2016_quilombolas)
curitiba_totais2016_quilombolas$...1 <- NULL
curitiba_totais2016_quilombolas$Ano <- "2016"



curitiba_totais2015_quilombolas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2015.xlsx", 
                                         sheet="Quilombolas")
curitiba_totais2015_quilombolas   <- data.frame(curitiba_totais2015_quilombolas)
curitiba_totais2015_quilombolas$...1 <- NULL
curitiba_totais2015_quilombolas$Ano <- "2015"


curitiba_totais2014_quilombolas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2014.xlsx", 
                                         sheet="Quilombolas")
curitiba_totais2014_quilombolas   <- data.frame(curitiba_totais2014_quilombolas)
curitiba_totais2014_quilombolas$...1 <- NULL
curitiba_totais2014_quilombolas$Ano <- "2014"


curitiba_totais2013_quilombolas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2013.xlsx", 
                                         sheet="Quilombolas")
curitiba_totais2013_quilombolas   <- data.frame(curitiba_totais2013_quilombolas)
curitiba_totais2013_quilombolas$...1 <- NULL
curitiba_totais2013_quilombolas$Ano <- "2013"


curitiba_totais2012_quilombolas <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2012.xlsx", 
                                         sheet="Quilombolas")
curitiba_totais2012_quilombolas   <- data.frame(curitiba_totais2012_quilombolas)
curitiba_totais2012_quilombolas$...1 <- NULL
curitiba_totais2012_quilombolas$Ano <- "2012"


serie_historica_quilombolas <- rbind(curitiba_totais2021_quilombolas, 
                                   curitiba_totais2020_quilombolas, 
                                   curitiba_totais2019_quilombolas,
                                   curitiba_totais2018_quilombolas,
                                   curitiba_totais2017_quilombolas,
                                   curitiba_totais2016_quilombolas,
                                   curitiba_totais2015_quilombolas,
                                   curitiba_totais2014_quilombolas,
                                   curitiba_totais2013_quilombolas,
                                   curitiba_totais2012_quilombolas)

write.xlsx(serie_historica_quilombolas, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/serie_historica_quilombolas.xlsx")


ggplot(serie_historica_quilombolas, aes(x = Ano, y = freq, fill = IN_FAMILIA_QUILOMBOLA_FAM)) + 
  geom_bar(stat = "identity") +
  labs(x ="", y = "Pessoas Situação de Rua",
       subtitle = "Série Histórica 2012-2021 de Pessoas em Situação de Rua em Curitiba Pertencentes a Famílias Quilombolas",
       caption = "Fonte: Ministério da Cidadania. Elaborado por W. Migliari, 2022.") +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(t = 5, r = 20, b = 5, l = 5))) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="gray", size = 12),
        axis.title.y = element_text(face="bold", colour="gray", size = 12),
        legend.title = element_text(face="bold", size = 10)) +
  scale_fill_manual(values = c("#778899", "#DCDCDC", "slategray", "gold")) +
  theme(legend.title=element_blank()) +
  geom_label( 
    data=serie_historica_quilombolas %>% filter(IN_FAMILIA_QUILOMBOLA_FAM == "Sim"), # Filter data first
    aes(label=freq)
  ) +
  guides(
    fill = guide_legend(
      title = "",
      override.aes = aes(label = "")
    )
  )


###### Grupos Tradicionais Específicos

curitiba_totais2021_gte <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2021.xlsx", 
                                         sheet="Grupos Tradicionais Específicos")
curitiba_totais2021_gte <- data.frame(curitiba_totais2021_gte )
curitiba_totais2021_gte$...1 <- NULL
curitiba_totais2021_gte$Ano <- "2021"


curitiba_totais2020_gte <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2020.xlsx", 
                                 sheet="Grupos Tradicionais Específicos")
curitiba_totais2020_gte <- data.frame(curitiba_totais2020_gte )
curitiba_totais2020_gte$...1 <- NULL
curitiba_totais2020_gte$Ano <- "2020"


curitiba_totais2019_gte <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2019.xlsx", 
                                 sheet="Grupos Tradicionais Específicos")
curitiba_totais2019_gte <- data.frame(curitiba_totais2019_gte )
curitiba_totais2019_gte$...1 <- NULL
curitiba_totais2019_gte$Ano <- "2019"


curitiba_totais2018_gte <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2018.xlsx", 
                                 sheet="Grupos Tradicionais Específicos")
curitiba_totais2018_gte <- data.frame(curitiba_totais2018_gte )
curitiba_totais2018_gte$...1 <- NULL
curitiba_totais2018_gte$Ano <- "2018"


curitiba_totais2017_gte <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2017.xlsx", 
                                 sheet="Grupos Tradicionais Específicos")
curitiba_totais2017_gte <- data.frame(curitiba_totais2017_gte )
curitiba_totais2017_gte$...1 <- NULL
curitiba_totais2017_gte$Ano <- "2017"

curitiba_totais2016_gte <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2016.xlsx", 
                                 sheet="Grupos Tradicionais Específicos")
curitiba_totais2016_gte <- data.frame(curitiba_totais2016_gte )
curitiba_totais2016_gte$...1 <- NULL
curitiba_totais2016_gte$Ano <- "2016"


curitiba_totais2015_gte <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2015.xlsx", 
                                 sheet="Grupos Tradicionais Específicos")
curitiba_totais2015_gte <- data.frame(curitiba_totais2015_gte )
curitiba_totais2015_gte$...1 <- NULL
curitiba_totais2015_gte$Ano <- "2015"


curitiba_totais2014_gte <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2014.xlsx", 
                                 sheet="Grupos Tradicionais Específicos")
curitiba_totais2014_gte <- data.frame(curitiba_totais2014_gte )
curitiba_totais2014_gte$...1 <- NULL
curitiba_totais2014_gte$Ano <- "2014"


curitiba_totais2013_gte <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2013.xlsx", 
                                 sheet="Grupos Tradicionais Específicos")
curitiba_totais2013_gte <- data.frame(curitiba_totais2013_gte )
curitiba_totais2013_gte$...1 <- NULL
curitiba_totais2013_gte$Ano <- "2013"

curitiba_totais2012_gte <- read_excel("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/2012.xlsx", 
                                 sheet="Grupos Tradicionais Específicos")
curitiba_totais2012_gte <- data.frame(curitiba_totais2012_gte )
curitiba_totais2012_gte$...1 <- NULL
curitiba_totais2012_gte$Ano <- "2012"



serie_historica_gte <- rbind(curitiba_totais2021_gte, 
                                     curitiba_totais2020_gte, 
                                     curitiba_totais2019_gte,
                                     curitiba_totais2018_gte,
                                     curitiba_totais2017_gte,
                                     curitiba_totais2016_gte,
                                     curitiba_totais2015_gte,
                                     curitiba_totais2014_gte,
                                     curitiba_totais2013_gte,
                                     curitiba_totais2012_gte)

write.xlsx(serie_historica_gte, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/cur/serie_historica_gte.xlsx")

ggplot(serie_historica_gte, aes(x = Ano, y = freq, fill = IN_PARC_MDS_FAM)) + 
  geom_bar(stat = "identity") +
  labs(x ="", y = "Pessoas Situação de Rua",
       subtitle = "Série Histórica 2012-2021 de Pessoas em Situação de Rua em Curitiba, Grupos Tradicionais e Específicos",
       caption = "Fonte: Ministério da Cidadania. Elaborado por W. Migliari, 2022.") +
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(t = 5, r = 20, b = 5, l = 5))) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="gray", size = 12),
        axis.title.y = element_text(face="bold", colour="gray", size = 12),
        legend.title = element_text(face="bold", size = 10)) +
  scale_fill_manual(values = c("#dfedf3",
                               "#bfdce8",
                               "#9fcbdc", 
                               "#7cbad1", 
                               "#54a9c5", 
                               "#1c4a59",
                               "black",
                               "#808080",
                               "#A9A9A9",
                               "#D3D3D3",
                               "#ececec",
                               "#708090")) +
  theme(legend.title=element_blank()) +
  annotate(geom="text", x="2015", y=42000, label="Cores omitidas resultam de poucas ocorrências.",
           color="gray")

