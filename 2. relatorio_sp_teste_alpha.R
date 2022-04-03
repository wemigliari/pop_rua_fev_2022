library(readxl)
library(psych)

alpha_test_pop_rua <- read_xlsx("/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/serie_historica_capitais_observatorio.xlsx")
class(alpha_test_pop_rua)

alpha_test1 <- alpha_test_pop_rua$`São Paulo`
alpha_test2 <- alpha_test_pop_rua$`Belo Horizonte`
alpha_test3 <- alpha_test_pop_rua$`Rio de Janeiro`
alpha_test4 <- alpha_test_pop_rua$`Distrito Federal`
alpha_test5 <- alpha_test_pop_rua$Salvador
alpha_test6 <- alpha_test_pop_rua$Macapá
alpha_test7 <- alpha_test_pop_rua$Cuiabá

alpha_test_pop_rua <- cbind(alpha_test1,
                            alpha_test2,
                            alpha_test3,
                            alpha_test4,
                            alpha_test5,
                            alpha_test6,
                            alpha_test7)

alpha_test_pop_rua <- data.frame(alpha_test_pop_rua)

names(alpha_test_pop_rua)[1:7] <- c("São Paulo", "Belo Horizonte", "Rio de Janeiro", "Distrito Federal", "Salvador", "Macapá", "Cuiabá")

## alpha_test <- alpha(alpha_test_pop_rua, check.keys=TRUE)
alpha_test <- alpha(alpha_test_pop_rua)
class(test)

test <- data.frame(alpha_test$total, alpha_test$alpha.drop, alpha_test$item.stats)


library("writexl")
write_xlsx(test, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/test_alpha_serie_historica_capitais.xlsx")



library(ltm)

cronbach.alpha(alpha_test_pop_rua, CI = TRUE)


cor(alpha_test_pop_rua)






