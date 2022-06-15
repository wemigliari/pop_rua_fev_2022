library(ggpubr)

serie_historica_corr_cap_pop_rua <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/serie_historica_capitais_observatorio.xlsx")

pairs(serie_historica_corr_cap_pop_rua[, c(3, 2, 4, 15, 24, 28, 26, 14, 27, 25)])

library(GGally)
ggpairs(serie_historica_corr_cap_pop_rua[, c(3, 2, 4, 15, 24, 28, 26, 14, 27, 25)])

# improved correlation matrix
library(corrplot)

corrplot(cor(serie_historica_corr_cap_pop_rua[, c(3, 2, 4, 15, 24, 28, 26, 14, 27, 25)]),
         method = "color",
         addCoef.col="grey", 
         tl.col = "gray",
         order = "AOE", tl.cex = 1/par("cex"),
         cl.cex = 1/par("cex"), addCoefasPercent = TRUE,
         sig.level = 0.05,
         number.digits = 2,
         number.cex = 0.7,
         type = "upper" # show only upper side
)


colour_set <- colorRampPalette(colors = c("#f4ff4d", "#c7d123", "#acb515", "#81890b", "#656c06"))
test<- str(serie_historica_corr_cap_pop_rua[, c(3, 2, 4, 15, 24, 28, 26, 14, 27, 25)])

corrplot(test, tl.col = "blue", bg = "White", tl.srt = 35, 
         title = "\n\n Correlação de Totais São Paulo e Capitais \n",
         addCoef.col = "black", type = "lower",
         col = colour_set(100))


#######

colnames(serie_historica_corr_cap_pop_rua) <- serie_historica_corr_cap_pop_rua[, c("São Paulo", 
                                                                                   "Belo Horizonte", 
                                                                                   "Rio de Janeiro", 
                                                                                   15, 24, 28, 26, 14, 27, 25)]

# Correlation

corr_serie_historica <- cor(serie_historica_corr_cap_pop_rua)




print("Pearson Correlations")


cor(serie_historica_corr_cap_pop_rua$`São Paulo`, serie_historica_corr_cap_pop_rua$`Belo Horizonte`,  method = "pearson", use = "complete.obs")
cor(serie_historica_corr_cap_pop_rua$`São Paulo`, serie_historica_corr_cap_pop_rua$`Rio de Janeiro`, method = "pearson", use = "complete.obs")
cor(serie_historica_corr_cap_pop_rua$`São Paulo`, serie_historica_corr_cap_pop_rua$Fortaleza, method = "pearson", use = "complete.obs")
cor(serie_historica_corr_cap_pop_rua$`São Paulo`, serie_historica_corr_cap_pop_rua$`Distrito Federal`, method = "pearson", use = "complete.obs")
cor(serie_historica_corr_cap_pop_rua$`São Paulo`, serie_historica_corr_cap_pop_rua$`Porto Alegre`, method = "pearson", use = "complete.obs")
cor(serie_historica_corr_cap_pop_rua$`São Paulo`, serie_historica_corr_cap_pop_rua$Curitiba, method = "pearson", use = "complete.obs")
cor(serie_historica_corr_cap_pop_rua$`São Paulo`, serie_historica_corr_cap_pop_rua$Salvador, method = "pearson", use = "complete.obs")
cor(serie_historica_corr_cap_pop_rua$`São Paulo`, serie_historica_corr_cap_pop_rua$Florianópolis, method = "pearson", use = "complete.obs")
cor(serie_historica_corr_cap_pop_rua$`São Paulo`, serie_historica_corr_cap_pop_rua$Goiânia, method = "pearson", use = "complete.obs")


########



