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


### Tabela 2013 do Ministério das Cidades

brasil_munic_2013  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201312.csv", 
                               col_names = TRUE)

names(brasil_munic_2013)[1]<- "codigo_ibg"

munic_2013_pop_rua  <- data.frame(count(brasil_munic_2013, "codigo_ibg"))

#########

brasil <- read_sf("/Users/wemigliari/Documents/R/data/brasil_municipios.shp", 
                  options = "ENCODING=windows-1252")

brasil_munic_2013_pop_rua <- data <- merge(munic_2013_pop_rua, brasil, by = "codigo_ibg")

brasil_munic_2013_pop_rua <- brasil_munic_2013_pop_rua%>%
  select(codigo_ibg, freq, nome, uf)


###### Para Datawrapper

write_xlsx(brasil_munic_2013_pop_rua, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/municipios_pop_rua_2013.xlsx")

######

shp_joined_2013 <- st_as_sf(data) 

# Create a color palette with handmade bins.
mybins <- c(0, 10, 20, 30, 50, 100, 200, 500, 600, 800, 1000, 2000, 3000, 5000, 6000, 7000, 8000)

nb.cols <- 17
mycolors <- colorRampPalette(brewer.pal(8, "Reds"))(nb.cols)

mypalette <- colorBin( palette=mycolors, domain=data$Total, na.color="transparent", bins=mybins)

mytext <- paste(
  "Município: ", brasil_munic_2013_pop_rua$nome, "<br/>",
  "Total: ", brasil_munic_2013_pop_rua$freq, "<br/>",
  sep="") %>%
  lapply(htmltools::HTML)


#### Map

leaflet(shp_joined_2013) %>% 
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
  addLegend(pal=mypalette, values=~brasil_munic_2013_pop_rua$freq, opacity=1, title = "Municípios, Totais de Moradores em Situação de Rua, 2013", position = "bottomright" )%>%
  setView(-47.9392, -15.7801, zoom = 4.2)





