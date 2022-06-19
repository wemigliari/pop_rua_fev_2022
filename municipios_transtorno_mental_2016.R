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

### Transtorno Mental Tabela 2021 do Ministério das Cidades

brasil_munic_2016  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201612.csv", 
                               col_names = TRUE)

names(brasil_munic_2016)[1]<- "codigo_ibge"


#########

brasil <- read_sf("/Users/wemigliari/Documents/R/data/BR_Municipios_2021.shp")
names(brasil)[1] <- "codigo_ibge"

brasil_munic_2016_pop_rua <- merge(brasil_munic_2016, brasil, by = "codigo_ibge")

brasil_munic_2016_pop_rua <- brasil_munic_2016_pop_rua%>%
  select(codigo_ibge, IN_DEF_TRANSTORNO_MENTAL_MEMB, NM_MUN, SIGLA)

brasil_munic_2016_pop_rua <- merge(brasil_munic_2016_pop_rua, brasil, by = "codigo_ibge")

brasil_munic_2016_pop_rua <- na.omit(brasil_munic_2016_pop_rua)
brasil_munic_2016_pop_rua <- data.frame(count(brasil_munic_2016_pop_rua, "codigo_ibge"))

brasil_munic_2016_pop_rua <- merge(brasil_munic_2016_pop_rua, brasil, by = "codigo_ibge")

###### Para Datawrapper

write_xlsx(brasil_munic_2016_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/transtorno_municipios_pop_rua_2016.xlsx")

######

shp_joined_2016 <- st_as_sf(brasil_munic_2016_pop_rua) 

# Create a color palette with handmade bins.
mybins <- c(0, 1, 5, 10, 20, 30, 40, 50, 100, 400, 500, 1000)

nb.cols <- 11
mycolors <- colorRampPalette(brewer.pal(11, "Reds"))(nb.cols)

mypalette <- colorBin( palette=mycolors, domain=brasil_munic_2016_pop_rua$freq, na.color="transparent", bins=mybins)

mytext <- paste(
  "Município: ", brasil_munic_2016_pop_rua$NM_MUN, "<br/>",
  "Total: ", brasil_munic_2016_pop_rua$freq, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)


#### Map

leaflet(shp_joined_2016) %>% 
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
  addLegend(pal=mypalette, values=~brasil_munic_2016_pop_rua$freq, opacity=1, title = "Municípios, Transtorno Psicológico Moradores em Situação de Rua, 2021", position = "bottomright" )%>%
  setView(-47.9392, -15.7801, zoom = 3.5)



