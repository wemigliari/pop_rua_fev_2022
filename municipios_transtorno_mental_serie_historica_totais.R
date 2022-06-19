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



### Brasil

### Tabela 2021 do Ministério das Cidades

brasil_munic_2021  <- read_excel("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_202112_abril.xlsx", 
                                 col_names = TRUE)

names(brasil_munic_2021)[4]<- "codigo_ibge"

munic_2021_pop_rua  <- data.frame(count(brasil_munic_2021, "IN_DEF_TRANSTORNO_MENTAL_MEMB"))

munic_2021_pop_rua$Ano <- "2021"

### Tabela 2020 do Ministério das Cidades

brasil_munic_2020  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_202012.csv", 
                               col_names = TRUE)

names(brasil_munic_2020)[1]<- "codigo_ibge"

munic_2020_pop_rua  <- data.frame(count(brasil_munic_2020, "IN_DEF_TRANSTORNO_MENTAL_MEMB"))

munic_2020_pop_rua$Ano <- "2020"

### Tabela 2019 do Ministério das Cidades

brasil_munic_2019  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201912.csv", 
                               col_names = TRUE)

names(brasil_munic_2019)[1]<- "codigo_ibge"

munic_2019_pop_rua  <- data.frame(count(brasil_munic_2019, "IN_DEF_TRANSTORNO_MENTAL_MEMB"))

munic_2019_pop_rua$Ano <- "2019"

### Tabela 2018 do Ministério das Cidades

brasil_munic_2018  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201812.csv", 
                               col_names = TRUE)

names(brasil_munic_2018)[1]<- "codigo_ibge"

munic_2018_pop_rua  <- data.frame(count(brasil_munic_2018, "IN_DEF_TRANSTORNO_MENTAL_MEMB"))

munic_2018_pop_rua$Ano <- "2018"

### Tabela 2017 do Ministério das Cidades

brasil_munic_2017  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201712.csv", 
                               col_names = TRUE)

names(brasil_munic_2017)[1]<- "codigo_ibge"

munic_2017_pop_rua  <- data.frame(count(brasil_munic_2017, "IN_DEF_TRANSTORNO_MENTAL_MEMB"))

munic_2017_pop_rua$Ano <- "2017"

### Tabela 2016 do Ministério das Cidades

brasil_munic_2016  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201612.csv", 
                               col_names = TRUE)

names(brasil_munic_2016)[1]<- "codigo_ibge"

munic_2016_pop_rua  <- data.frame(count(brasil_munic_2016, "IN_DEF_TRANSTORNO_MENTAL_MEMB"))

munic_2016_pop_rua$Ano <- "2016"

### Tabela 2015 do Ministério das Cidades

brasil_munic_2015  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201512.csv", 
                               col_names = TRUE)

names(brasil_munic_2015)[1]<- "codigo_ibge"

munic_2015_pop_rua  <- data.frame(count(brasil_munic_2015, "IN_DEF_TRANSTORNO_MENTAL_MEMB"))

munic_2015_pop_rua$Ano <- "2015"

### Tabela 2014 do Ministério das Cidades

brasil_munic_2014  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201412.csv", 
                               col_names = TRUE)

names(brasil_munic_2014)[1]<- "codigo_ibge"

munic_2014_pop_rua  <- data.frame(count(brasil_munic_2014, "IN_DEF_TRANSTORNO_MENTAL_MEMB"))

munic_2014_pop_rua$Ano <- "2014"

### Tabela 2013 do Ministério das Cidades

brasil_munic_2013  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201312.csv", 
                               col_names = TRUE)

names(brasil_munic_2013)[1]<- "codigo_ibge"

munic_2013_pop_rua  <- data.frame(count(brasil_munic_2013, "IN_DEF_TRANSTORNO_MENTAL_MEMB"))

munic_2013_pop_rua$Ano <- "2013"

### Tabela 2012 do Ministério das Cidades

brasil_munic_2012  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201212.csv", 
                               col_names = TRUE)

names(brasil_munic_2012)[1]<- "codigo_ibge"

munic_2012_pop_rua  <- data.frame(count(brasil_munic_2012, "IN_DEF_TRANSTORNO_MENTAL_MEMB"))

munic_2012_pop_rua$Ano <- "2012"

##########


serie_historica_cidades <- rbind(munic_2021_pop_rua,
                                 munic_2020_pop_rua,
                                 munic_2019_pop_rua, 
                                 munic_2018_pop_rua, 
                                 munic_2017_pop_rua, 
                                 munic_2016_pop_rua, 
                                 munic_2015_pop_rua, 
                                 munic_2014_pop_rua, 
                                 munic_2013_pop_rua, 
                                 munic_2012_pop_rua)

serie_historica_cidades <- na.omit(serie_historica_cidades)
serie_historica_cidades <- filter(serie_historica_cidades, IN_DEF_TRANSTORNO_MENTAL_MEMB != 0)

sum(serie_historica_cidades$freq)

serie_historica_cidades$Ano <- as.numeric(serie_historica_cidades$Ano)

serie_historica_cidades <- serie_historica_cidades[order(serie_historica_cidades$Ano),]


library(xlsx)

write.xlsx(serie_historica_cidades, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/serie_historica_transtorno_pop_rua_cidades.xlsx",
           sheetName="Anos", append=TRUE)



### Transtorno Mental Tabela 2021 do Ministério das Cidades

brasil_munic_2021  <- read_excel("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_202112_abril.xlsx", 
                                 col_names = TRUE)

names(brasil_munic_2021)[4]<- "codigo_ibge"

#munic_2021_pop_rua  <- data.frame(count(brasil_munic_2021, "codigo_ibge"))

#########

brasil <- read_sf("/Users/wemigliari/Documents/R/data/BR_Municipios_2021.shp")
names(brasil)[1] <- "codigo_ibge"

brasil_munic_2021_pop_rua <- data <- merge(brasil_munic_2021, brasil, by = "codigo_ibge")

brasil_munic_2021_pop_rua <- brasil_munic_2021_pop_rua%>%
  select(codigo_ibge, IN_DEF_TRANSTORNO_MENTAL_MEMB, NM_MUN, SIGLA)

brasil_munic_2021_pop_rua <- data <- merge(brasil_munic_2021_pop_rua, brasil, by = "codigo_ibge")

brasil_munic_2021_pop_rua <- na.omit(brasil_munic_2021_pop_rua)
brasil_munic_2021_pop_rua <- data.frame(count(brasil_munic_2021_pop_rua, "codigo_ibge"))

brasil_munic_2021_pop_rua <- merge(brasil_munic_2021_pop_rua, brasil, by = "codigo_ibge")

###### Para Datawrapper

write_xlsx(brasil_munic_2021_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/transtorno_municipios_pop_rua_2021.xlsx")

######

shp_joined_2021 <- st_as_sf(data) 

# Create a color palette with handmade bins.
mybins <- c(0, 1, 5, 10, 20, 30, 40, 50, 100, 400, 500, 1000)

nb.cols <- 11
mycolors <- colorRampPalette(brewer.pal(11, "Reds"))(nb.cols)

mypalette <- colorBin( palette=mycolors, domain=data$Total, na.color="transparent", bins=mybins)

mytext <- paste(
  "Município: ", brasil_munic_2021_pop_rua$NM_MUN.x, "<br/>",
  "Total: ", brasil_munic_2021_pop_rua$IN_DEF_TRANSTORNO_MENTAL_MEMB, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)


#### Map

leaflet(shp_joined_2021) %>% 
  addPolygons( 
    fillColor = ~mypalette(freq), 
    stroke=TRUE, 
    fillOpacity = 1, 
    color="white", 
    weight=0.3,
    label = mytext,
    labelOptions = labelOptions( 
      style = list("font-weight" = "normal", padding = "3px 8px"), 
      textsize = "13px", 
      direction = "auto"
    )
  )%>% 
  addTiles()  %>%
  addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
  addLegend(pal=mypalette, values=~brasil_munic_2021_pop_rua$IN_DEF_TRANSTORNO_MENTAL_MEMB, opacity=1, title = "Municípios, Transtorno Psicológico Moradores em Situação de Rua, 2021", position = "bottomright" )%>%
  setView(-47.9392, -15.7801, zoom = 4.2)




