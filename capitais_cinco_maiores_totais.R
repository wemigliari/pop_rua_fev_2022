library(tidyverse)
library(readxl)
library(ggplot2)

######### Capitais Cinco Maiores Totais
######### Pessoas em situacao de rua


serie_historica_capitais <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/serie_historica_capitais_observatorio.xlsx")

serie_historica_capitais <- data.frame(serie_historica_capitais)
serie_historica_capitais2 <- serie_historica_capitais[, c(1, 2, 3, 4, 14, 24)]
serie_historica_capitais2 <- melt(serie_historica_capitais2, id.vars="Ano")
names(serie_historica_capitais2)[2] <- "Capital"
names(serie_historica_capitais2)[3] <- "Total"
serie_historica_capitais2 <- data.frame(serie_historica_capitais2)
serie_historica_capitais2$Capital <- as.character(serie_historica_capitais2$Capital)

serie_historica_capitais2[serie_historica_capitais2 == "São.Paulo"] <- "São Paulo"
serie_historica_capitais2$Capital[which(serie_historica_capitais2$Capital == "Belo.Horizonte")] <- "Belo Horizonte"
serie_historica_capitais2$Capital[which(serie_historica_capitais2$Capital == "Rio.de.Janeiro")] <- "Rio de Janeiro"
serie_historica_capitais2$Capital[which(serie_historica_capitais2$Capital == "Distrito.Federal")] <- "Distrito Federal"
serie_historica_capitais2$Capital[which(serie_historica_capitais2$Capital == "Salvador")] <- "Salvador"

ggplot(serie_historica_capitais2, aes(x=Ano, y=Total, fill=Capital)) +
  geom_bar(stat="identity")+
  geom_text(aes(y=Total, label=""), vjust=1.6, 
            color="white", size=1.5)+
  scale_fill_brewer(palette="Greens")+
  theme_minimal() +
  theme(axis.title.y = element_text(margin = margin(t = 5, r = 20, b = 5, l = 5))) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="gray", size = 12),
        axis.title.y = element_text(face="bold", colour="gray", size = 12),
        legend.title = element_text(face="bold", size = 10)) +
  coord_polar() 
