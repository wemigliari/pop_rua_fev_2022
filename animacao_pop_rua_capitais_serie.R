# Get data:
library(gapminder)

# Charge libraries:
library(ggplot2)
library(gganimate)
library(readxl)
library(reshape2)
library(tidyr) 
library(dplyr)

municip_totais_serie <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/serie_historica_capitais_observatorio.xlsx")

municip_totais_serie <- melt(municip_totais_serie,
     id.vars = "Ano",
     variable.name = "Cidade",
     value.name = "Total")

# Make a ggplot, but add frame=year: one image per year
ggplot(municip_totais_serie, aes(Ano, Total, size = Total, color = Cidade)) +
  geom_point() +
  theme_bw() +
  # gganimate specific bits:
  labs(title = 'Ano: {frame_time}', x = 'Ano', y = 'Total') +
  transition_time(Ano) +
  ease_aes('linear') +
  shadow_mark(alpha = 0.3, size = 0.5)

# Save at gif:
anim_save("/Users/wemigliari/Documents/R/R_Scripts/pop_rua_2022/municipios_pop_rua_serie.gif")


#######

ggplot(municip_totais_serie, aes(Ano, Total, size = Total, color = Cidade)) +
  theme_bw() +
  geom_line() +
  scale_color_viridis_d() +
  labs(title = 'Ano: {frame_time}', x = 'Ano', y = 'Total') +
  theme(legend.position = "top") + transition_reveal(Ano)

anim_save("/Users/wemigliari/Documents/R/R_Scripts/pop_rua_2022/municipios_pop_rua_serie2.gif")



#######

municip_totais_serie_2012 <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/municipios_pop_rua_2012.xlsx")
municip_totais_serie_2012$Ano <- "2012"
municip_totais_serie_2013 <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/municipios_pop_rua_2013.xlsx")
municip_totais_serie_2013$Ano <- "2013"
municip_totais_serie_2014 <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/municipios_pop_rua_2014.xlsx")
municip_totais_serie_2014$Ano <- "2014"
municip_totais_serie_2015 <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/municipios_pop_rua_2015.xlsx")
municip_totais_serie_2015$Ano <- "2015"
municip_totais_serie_2016 <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/municipios_pop_rua_2016.xlsx")
municip_totais_serie_2016$Ano <- "2016"
municip_totais_serie_2017 <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/municipios_pop_rua_2017.xlsx")
municip_totais_serie_2017$Ano <- "2017"
municip_totais_serie_2018 <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/municipios_pop_rua_2018.xlsx")
municip_totais_serie_2018$Ano <- "2018"
municip_totais_serie_2019 <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/municipios_pop_rua_2019.xlsx")
municip_totais_serie_2019$Ano <- "2019"
municip_totais_serie_2020 <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/municipios_pop_rua_2020.xlsx")
municip_totais_serie_2020$Ano <- "2020"
municip_totais_serie_2021 <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/municipios_pop_rua_2021.xlsx")
municip_totais_serie_2021$Ano <- "2021"

serie_historica_municip_pop_rua <- rbind(municip_totais_serie_2012, 
                                        municip_totais_serie_2013,
                                        municip_totais_serie_2014,
                                        municip_totais_serie_2015,
                                        municip_totais_serie_2016,
                                        municip_totais_serie_2017,
                                        municip_totais_serie_2018,
                                        municip_totais_serie_2019,
                                        municip_totais_serie_2020)


write.xlsx(serie_historica_municip_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/serie_historica_pop_rua_cidades.xlsx",
           sheetName="Série Histórica", append=TRUE)

#######

serie_historica_municip_pop_rua$codigo_ibg <- NULL
serie_historica_municip_pop_rua$uf <- NULL


ggplot(serie_historica_municip_pop_rua, aes(Ano, freq, size = freq, color = nome)) +
  theme_bw() +
  geom_point() +
  theme(legend.position="none") +
  scale_color_viridis_d() +
  labs(x = 'Ano', y = 'Total') +
  theme(legend.position="none")
