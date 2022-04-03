library(readxl)
library(dplyr)
library(ggplot2)
library(zoo)
library(hrbrthemes)
library(scales)

library(hrbrthemes)
library(ggrepel)
library(ggthemes)

igd <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/serie_historica_sp_igdm.xlsx")


ggplot(igd, aes(x=Período)) +
  geom_line(aes(y=`Valor Total de Incentivos`), linetype = "F1") + 
  xlab("") +
  theme_ipsum()+
  theme(plot.caption = element_text(size = 12)) +
  theme(text = element_text(size=12), axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  ggtitle("Índice de Gestão Descentralizada do Município (IGD-M), São Paulo") +
  labs(x ="", y = "Valores (R$)",
       subtitle = "Série Histórica 2015-2021, Repasses Mensais do Governo Federal",
       caption = "Fonte: CadÚnico. Elaborado por W. Migliari, 2022.") +
  #scale_color_manual(values = c("steelblue")) +
  theme_economist() +
  theme(axis.title.y = element_text(margin = margin(r = 0.25, unit = "in")))


igd2_acumulado <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/serie_historica_sp_igdm.xlsx",
                  sheet ="Acumulado")
#####

igd2_acumulado %>%
  ggplot(aes(x=Período, y=`Valor Total de Incentivos`)) +
  geom_line(aes(y=`Valor Total de Incentivos`), linetype = "F1") + 
  theme_ipsum() +
  ggtitle("Índice de Gestão Descentralizada do Município (IGD-M), São Paulo") +
  labs(x ="", y = "Valores (R$)",
       subtitle = "Série Histórica 2015-2021, Repasses Anuais Acumulados",
       caption = "Fonte: CadÚnico. Elaborado por W. Migliari, 2022.") +
  theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 10))) +
  geom_label_repel(aes(label = `Valor Total de Incentivos`),
                   box.padding   = 0.45, 
                   point.padding = 0.9,
                   size= 3,
                   segment.color = 'grey50') +
  theme_economist() +
  scale_colour_economist() +
  theme(axis.title.y = element_text(margin = margin(r = 0.25, unit = "in"))) +
  scale_y_continuous(labels = unit_format(unit = "Milhão", scale = 1e-6))


