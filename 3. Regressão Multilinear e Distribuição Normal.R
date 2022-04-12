library(tidyverse)
library(readxl)

######### Regressão Multilinear das dez capitais 
######### com maior número de pessoas em situacao de rua


serie_historica_capitais <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/serie_historica_capitais_observatorio.xlsx")

serie_historica_capitais <- data.frame(serie_historica_capitais)

###### 10 capitais

modelo_capitais <- lm(serie_historica_capitais$São.Paulo ~
                        serie_historica_capitais$Belo.Horizonte +
                        serie_historica_capitais$Rio.de.Janeiro +
                        serie_historica_capitais$Boa.Vista +
                        serie_historica_capitais$Salvador +
                        serie_historica_capitais$Fortaleza +
                        serie_historica_capitais$Distrito.Federal+
                        serie_historica_capitais$Curitiba +
                        serie_historica_capitais$Florianópolis +
                        serie_historica_capitais$Porto.Alegre)

summary(modelo_capitais)

plot(modelo_capitais$fitted, modelo_capitais$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(0,0)


##### 5 mais populosas https://www.statology.org/residuals/

modelo_capitais2 <- lm(serie_historica_capitais$São.Paulo ~
                         serie_historica_capitais$Belo.Horizonte +
                         serie_historica_capitais$Rio.de.Janeiro +
                         serie_historica_capitais$Distrito.Federal +
                         serie_historica_capitais$Salvador)


summary(modelo_capitais2)
plot(modelo_capitais2$fitted, modelo_capitais2$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(0,0)

par(mfrow = c(1,1), family= "Arial", cex = 0.5, oma = c(4, 1, 1, 4))
plot(modelo_capitais2$fitted, modelo_capitais2$residuals, 
     xlab = "Valores Ajustados", ylab = "Resíduos",
     col = "darkgreen", lwd = 2.5) 
abline(0,0)



## 4 capitais

modelo_capitais7 <- lm(serie_historica_capitais$São.Paulo ~
                         serie_historica_capitais$Cuiabá +
                         serie_historica_capitais$Palmas +
                         serie_historica_capitais$Boa.Vista)

summary(modelo_capitais7)
plot(modelo_capitais7$fitted, modelo_capitais7$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(0,0)


## Normal Distribution

#SP
mean(serie_historica_capitais$São.Paulo)
sd(serie_historica_capitais$São.Paulo)

x1 <- seq(0, 55000, by = 1)
y1 <- dnorm(x1, mean = 26854, sd = 15560)


#BH
mean(serie_historica_capitais$Belo.Horizonte)
sd(serie_historica_capitais$Belo.Horizonte)

x2 <- seq(0, 50000, by = 1)
y2 <- dnorm(x2, mean = 7007, sd = 3473)

#RJ
mean(serie_historica_capitais$Rio.de.Janeiro)
sd(serie_historica_capitais$Rio.de.Janeiro)

x3 <- seq(0, 50000, by = 1)
y3 <- dnorm(x2, mean = 3733, sd = 3500)

#Distrito Federal
mean(serie_historica_capitais$Distrito.Federal)
sd(serie_historica_capitais$Distrito.Federal)

x4 <- seq(0, 50000, by = 1)
y4 <- dnorm(x2, mean = 2367, sd = 2054)

#Salvador
mean(serie_historica_capitais$Salvador)
sd(serie_historica_capitais$Salvador)

x5 <- seq(0, 50000, by = 1)
y5 <- dnorm(x2, mean = 1515, sd = 1365)


library(extrafont)
library(remotes)
remotes::install_version("Rttf2pt1", version = "1.3.8")

extrafont::font_import()

par(mfrow = c(1,1), family= "Helvetica", cex = 1, oma = c(4, 1, 1, 4))
plot(x5, y5, col = "darkgreen", type = "l", lwd = 2.5,
     xlab = "Pessoas em Situação de Rua",
     ylab = "Probabilidades") 
lines(x4, y4, col = "olivedrab", type = "l", lty = 1, lwd = 2.5)
lines(x3, y3, col = "yellowgreen", type = "l", lty = 1, lwd = 2.5)
lines(x2, y2, col = "lightgray", type = "l", lty = 1, lwd = 2.5)
lines(x1, y1, col = "gold", type = "l", lty = 1, lwd = 2.5)


legend(32000, 0.00030, legend=c("São Paulo", "Belo Horizonte", "Rio de Janeiro", "Distrito Federal", "Salvador"),
       col=c("gold", "lightgray","yellowgreen", "olivedrab", "darkgreen"), 
       #title = expression("Capitais"),
       title.adj = 0.4, lty = 1, lwd = 2.5, box.lty = 0, cex = 0.6,
       y.intersp=0.4,x.intersp=0.1)

legend(32000, 0.00019,  legend = c("26854  15560", "  7007   3473", "  3733   3500", "  2367   2054", "  1515   1365"), 
       col = c("gold", "lightgray","yellowgreen", "olivedrab", "darkgreen"),
       title = expression(paste(mu, "   ", sigma)),
       title.adj = 0.4, lty = 1, lwd = 2.5, box.lty = 0, cex = 0.6,
       y.intersp=0.4,x.intersp=0.1)


## Média Sao Paulo

par(mfrow = c(1,1), family= "Helvetica", cex = 1, oma = c(4, 1, 1, 4))
plot(x1, y1, col = "darkgreen", type = "l", lwd = 2.5,
     xlab = "Pessoas em Situação de Rua",
     ylab = "Probabilidades") 
abline(v = mean(serie_historica_capitais$São.Paulo), col="gray", lwd=0.5, lty=2)
text(x=mean(serie_historica_capitais$São.Paulo), y= 1.5e-05, "Média 26.854", pos = 2, cex=0.65,col="black") 
text(x=mean(serie_historica_capitais$São.Paulo), y= 1.4e-05, "Desvio-Padrão 15.560", pos = 2, cex=0.65,col="black") 


## Distribuicao de Probabilidades
mean(serie_historica_capitais$São.Paulo)
sd(serie_historica_capitais$São.Paulo)

prob_distr_sp <- pnorm(x1, mean = 26854, sd = 15560)

par(mfrow = c(1,1), family= "Helvetica", cex = 1.0, oma = c(4, 1, 1, 4))
plot(x1, prob_distr_sp, col = "darkgreen", type="l", lty=1, lwd = 1,
     xlab = "Pessoas em Situação de Rua em São Paulo",
     ylab = "Distribuição de Probabilidades") 


## Distribuicao Acumulada
x11 <- seq(0, 1, by = 0.01)
distrib_acumul <- qnorm(x11, mean = 26854, sd = 15560)


par(mfrow = c(1,1), family= "Helvetica", cex = 1.0, oma = c(4, 1, 1, 4))
plot(x11, distrib_acumul, col = "olivedrab", type="l", lty=1, lwd = 1,
     xlab = "Distribuição Acumulada/Quantis",
     ylab = "Pessoas em Situação de Rua em São Paulo") 


## Q-Q Norm para as cidades mais populosas

library(extrafont)
par(mfrow = c(2,3), family= "Arial", cex = 0.5, oma = c(4, 1, 1, 4))

qqnorm(serie_historica_capitais$São.Paulo, pch = 1, frame = FALSE, main = "Gráfico Q-Q, Parâmetros de Dispersão, São Paulo") +
  qqline(serie_historica_capitais$São.Paulo, col = "darkgreen", lwd = 2)

qqnorm(serie_historica_capitais$Belo.Horizonte, pch = 1, frame = FALSE,  main = "Gráfico Q-Q, Parâmetros de Dispersão, Belo Horizonte") +
  qqline(serie_historica_capitais$Belo.Horizonte, col = "darkgreen", lwd = 2)

qqnorm(serie_historica_capitais$Rio.de.Janeiro, pch = 1, frame = FALSE,  main = "Gráfico Q-Q, Parâmetros de Dispersão, Rio de Janeiro") +
  qqline(serie_historica_capitais$Rio.de.Janeiro, col = "darkgreen", lwd = 2)

qqnorm(serie_historica_capitais$Distrito.Federal, pch = 1, frame = FALSE,  main = "Gráfico Q-Q, Parâmetros de Dispersão, Distrito Federal") +
  qqline(serie_historica_capitais$Distrito.Federal, col = "darkgreen", lwd = 2)

qqnorm(serie_historica_capitais$Salvador, pch = 1, frame = FALSE,  main = "Gráfico Q-Q, Parâmetros de Dispersão, Salvador") +
  qqline(serie_historica_capitais$Salvador, col = "darkgreen", lwd = 2)


## stepAIC will eliminate inappropriate terms
library(MASS)
summary(stepAIC(modelo_capitais2))

plot(modelo_capitais2$fitted, modelo_capitais2$residuals, xlab = "Valores Ajustados", ylab = "Resíduos")
abline(0,0)

##### https://www.statology.org/plot-multiple-linear-regression-in-r/
library(car)
avPlots(modelo_capitais2)

##########

# install.packages("performance")
install.packages("see")
library(performance)
library(see)

check_model(modelo_capitais2)











