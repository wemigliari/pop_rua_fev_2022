library(readxl)
library(dplyr)
library(plyr)
library(stringr)
library(readr)
library(tidyverse)
library(writexl)


### Tabela 2019 do Ministério das Cidades

brasil_munic_2019  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201912.csv", 
                               col_names = TRUE)

names(brasil_munic_2019)[1]<- "id"

### Subseccionar Dados de Teresina de 2019

munic_2019 <- subset(brasil_munic_2019, id %in% c(2211001))

###1 Seleção de Dados pa RENDA
renda_2019 <- data.frame(count(munic_2019, "FX_RENDA"))

renda_2019<- renda_2019%>% replace_na(list(FX_RENDA = "Sem Dados"))

renda_2019$FX_RENDA[which(renda_2019$FX_RENDA=="1")] <- "Até R$ 89,00"
renda_2019$FX_RENDA[which(renda_2019$FX_RENDA=="2")] <- "Entre R$ 89,01 e R$ 178,00"
renda_2019$FX_RENDA[which(renda_2019$FX_RENDA=="3")] <- "Até 1/2 Salário Mínimo"
renda_2019$FX_RENDA[which(renda_2019$FX_RENDA=="4")] <- "Acima de 1/2 Salário Mínimo"

write.xlsx(renda_2019, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/teresina/2019.xlsx",
           sheetName="Renda", append=TRUE)

###2 Seleção de Dados pa PBF
pbf_2019 <- data.frame(count(munic_2019 , "MARC_PBF"))

pbf_2019  <- pbf_2019 %>% replace_na(list(MARC_PBF = "Sem Dados"))
pbf_2019$MARC_PBF[which(pbf_2019$MARC_PBF=="0")] <- "Sem Dados"

pbf_2019$MARC_PBF[which(pbf_2019$MARC_PBF=="1")] <- "Sim"
pbf_2019$MARC_PBF[which(pbf_2019$MARC_PBF=="2")] <- "Não"

write.xlsx(pbf_2019, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/teresina/2019.xlsx",
           sheetName="Bolsa Família", append=TRUE)

###3 Seleção de Dados pa FAMÍLIA INDÍGENA
fam_ind_2019 <- data.frame(count(munic_2019 , "IN_FAMILIA_INDIGENA_FAM"))

fam_ind_2019  <- fam_ind_2019 %>% replace_na(list(IN_FAMILIA_INDIGENA_FAM = "Sem Dados"))

fam_ind_2019$IN_FAMILIA_INDIGENA_FAM[which(fam_ind_2019$IN_FAMILIA_INDIGENA_FAM=="1")] <- "Sim"
fam_ind_2019$IN_FAMILIA_INDIGENA_FAM[which(fam_ind_2019$IN_FAMILIA_INDIGENA_FAM=="2")] <- "Não"

write.xlsx(fam_ind_2019, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/teresina/2019.xlsx",
           sheetName="Indígenas", append=TRUE)

###4 Seleção de Dados pa FAMÍLIA QUILOMBOLA
quilom_2019 <- data.frame(count(munic_2019 , "IN_FAMILIA_QUILOMBOLA_FAM"))

quilom_2019  <- quilom_2019 %>% replace_na(list(IN_FAMILIA_QUILOMBOLA_FAM = "Sem Dados"))

quilom_2019$IN_FAMILIA_QUILOMBOLA_FAM[which(quilom_2019$IN_FAMILIA_QUILOMBOLA_FAM=="1")] <- "Sim"
quilom_2019$IN_FAMILIA_QUILOMBOLA_FAM[which(quilom_2019$IN_FAMILIA_QUILOMBOLA_FAM=="2")] <- "Não"


write.xlsx(quilom_2019, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/teresina/2019.xlsx",
           sheetName="Quilombolas", append=TRUE)

###5 Seleção de Dados pa IN_PARC_MDS_FAM
mds_fam_2019 <- data.frame(count(munic_2019 , "IN_PARC_MDS_FAM"))

mds_fam_2019 <- mds_fam_2019%>% replace_na(list(IN_PARC_MDS_FAM = "Sem Dados"))

mds_fam_2019$IN_PARC_MDS_FAM <- gsub(c("000"), c("Nenhuma"), mds_fam_2019$IN_PARC_MDS_FAM)  

mds_fam_2019$IN_PARC_MDS_FAM <- gsub(c("101"), c("Família Cigana"), mds_fam_2019$IN_PARC_MDS_FAM)  

mds_fam_2019$IN_PARC_MDS_FAM <- gsub(c("201"), c("Família Extrativista"), mds_fam_2019$IN_PARC_MDS_FAM)  

mds_fam_2019$IN_PARC_MDS_FAM <- gsub(c("202"), c("Pescadores Artesanais"), mds_fam_2019$IN_PARC_MDS_FAM)  

mds_fam_2019$IN_PARC_MDS_FAM <- gsub(c("203"), c("Família Pertencente a Comunidade de Terreiro"), mds_fam_2019$IN_PARC_MDS_FAM)  

mds_fam_2019$IN_PARC_MDS_FAM <- gsub(c("204"), c("Família Ribeirinha"), mds_fam_2019$IN_PARC_MDS_FAM)  

mds_fam_2019$IN_PARC_MDS_FAM <- gsub(c("205"), c("Família de Agricultores Familiares"), mds_fam_2019$IN_PARC_MDS_FAM)  

mds_fam_2019$IN_PARC_MDS_FAM <- gsub(c("301"), c("Família Assentada da Reforma Agrária"), mds_fam_2019$IN_PARC_MDS_FAM)  

mds_fam_2019$IN_PARC_MDS_FAM <- gsub(c("302"), c("Família beneficiária do Programa Nacional de Crédito Fundiário"), mds_fam_2019$IN_PARC_MDS_FAM)  

mds_fam_2019$IN_PARC_MDS_FAM <- gsub(c("303"), c("Família Acampada"), mds_fam_2019$IN_PARC_MDS_FAM)  

mds_fam_2019$IN_PARC_MDS_FAM <- gsub(c("304"), c("Família Atingida por Empreendimentos de Infraestrutura"), mds_fam_2019$IN_PARC_MDS_FAM)  

mds_fam_2019$IN_PARC_MDS_FAM <- gsub(c("305"), c("Família de Preso do Sistema Carcerário"), mds_fam_2019$IN_PARC_MDS_FAM)  

mds_fam_2019$IN_PARC_MDS_FAM <- gsub(c("306"), c("Família de Catadores de Material Reciclável"), mds_fam_2019$IN_PARC_MDS_FAM)  


write.xlsx(mds_fam_2019, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/teresina/2019.xlsx",
           sheetName="Grupos Tradicionais Epaecíficos", append=TRUE)

###6 Seleção de Dados pa MESES_APOS_ULT_ATUALIZACAO
### Já está pronto o quadro, pois não há o que identificar entre as repaostas
ult_atua_2019 <- data.frame(count(munic_2019 , "MESES_APOS_ULT_ATUALIZACAO"))

write.xlsx(ult_atua_2019, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/teresina/2019.xlsx",
           sheetName="Atualização", append=TRUE)

###7 Seleção de Dados pa CO_EST_CADASTRAL_MEMB
esta_cadas_2019 <- data.frame(count(munic_2019 , "CO_EST_CADASTRAL_MEMB"))

esta_cadas_2019 <- esta_cadas_2019%>% replace_na(list(CO_EST_CADASTRAL_MEMB = "Sem Dados"))

esta_cadas_2019$CO_EST_CADASTRAL_MEMB <- gsub(c("1"), c("Em Cadastramento"), esta_cadas_2019$CO_EST_CADASTRAL_MEMB)  

esta_cadas_2019$CO_EST_CADASTRAL_MEMB  <- gsub(c("2"), c("Sem Registro Civil"), esta_cadas_2019$CO_EST_CADASTRAL_MEMB)  

esta_cadas_2019$CO_EST_CADASTRAL_MEMB <- gsub(c("3"), c("Cadastrado"), esta_cadas_2019$CO_EST_CADASTRAL_MEMB)  

esta_cadas_2019$CO_EST_CADASTRAL_MEMB <- gsub(c("4"), c("Excluído"), esta_cadas_2019$CO_EST_CADASTRAL_MEMB)  

esta_cadas_2019$CO_EST_CADASTRAL_MEMB <- gsub(c("5"), c("Aguardando NIS"), esta_cadas_2019$CO_EST_CADASTRAL_MEMB)  

esta_cadas_2019$CO_EST_CADASTRAL_MEMB <- gsub(c("6"), c("Validando NIS"), esta_cadas_2019$CO_EST_CADASTRAL_MEMB)  

write.xlsx(esta_cadas_2019, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/teresina/2019.xlsx",
           sheetName="Estado Cadastral", append=TRUE)

###8 Seleção de Dados pa CO_SEXO_PESSOA
sexo_2019 <- data.frame(count(munic_2019 , "CO_SEXO_PESSOA"))

sexo_2019$CO_SEXO_PESSOA <- sexo_2019$CO_SEXO_PESSOA[sexo_2019$CO_SEXO_PESSOA==c(1, 2)]<-c("Masculino", "Feminino")

write.xlsx(sexo_2019, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/teresina/2019.xlsx",
           sheetName="Sexo", append=TRUE)

###9 Seleção de Dados pa CO_RACA_COR_PESSOA
cor_2019 <- data.frame(count(munic_2019 , "CO_RACA_COR_PESSOA"))

cor_2019 <- cor_2019%>% replace_na(list(CO_RACA_COR_PESSOA = "Sem Dados"))

cor_2019$CO_RACA_COR_PESSOA[which(cor_2019$CO_RACA_COR_PESSOA=="1")] <- "Branca"
cor_2019$CO_RACA_COR_PESSOA[which(cor_2019$CO_RACA_COR_PESSOA=="2")] <- "Preta"
cor_2019$CO_RACA_COR_PESSOA[which(cor_2019$CO_RACA_COR_PESSOA=="3")] <- "Amarela"
cor_2019$CO_RACA_COR_PESSOA[which(cor_2019$CO_RACA_COR_PESSOA=="4")] <- "Parda"
cor_2019$CO_RACA_COR_PESSOA[which(cor_2019$CO_RACA_COR_PESSOA=="5")] <- "Indígena"

write.xlsx(cor_2019, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/teresina/2019.xlsx",
           sheetName="Cor", append=TRUE)

###10 Seleção de Dados pa FX_ETARIA
idade_2019 <- data.frame(count(munic_2019 , "FX_ETARIA"))

idade_2019<- idade_2019%>% replace_na(list(FX_ETARIA = "Sem Dados"))

idade_2019$FX_ETARIA[which(idade_2019$FX_ETARIA=="1")] <- "Até 11 anos"
idade_2019$FX_ETARIA[which(idade_2019$FX_ETARIA=="2")] <- "De 12 a 17 anos"
idade_2019$FX_ETARIA[which(idade_2019$FX_ETARIA=="3")] <- "De 18 a 21 anos"
idade_2019$FX_ETARIA[which(idade_2019$FX_ETARIA=="4")] <- "De 22 a 29 anos"
idade_2019$FX_ETARIA[which(idade_2019$FX_ETARIA=="5")] <- "De 30 a 59 anos"
idade_2019$FX_ETARIA[which(idade_2019$FX_ETARIA=="6")] <- "De 60 anos acima"

write.xlsx(idade_2019, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/teresina/2019.xlsx",
           sheetName="Idade", append=TRUE)

###11 Seleção de Dados pa IN_DEF_TRANSTORNO_MENTAL_MEMB
transtorno_2019 <- data.frame(count(munic_2019 , "IN_DEF_TRANSTORNO_MENTAL_MEMB"))

transtorno_2019 <- transtorno_2019%>% replace_na(list(IN_DEF_TRANSTORNO_MENTAL_MEMB = "Sem Dados"))

transtorno_2019$IN_DEF_TRANSTORNO_MENTAL_MEMB[which(transtorno_2019$IN_DEF_TRANSTORNO_MENTAL_MEMB=="0")] <- "Não"

transtorno_2019$IN_DEF_TRANSTORNO_MENTAL_MEMB[which(transtorno_2019$IN_DEF_TRANSTORNO_MENTAL_MEMB=="1")] <- "Sim"


write.xlsx(transtorno_2019, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/teresina/2019.xlsx",
           sheetName="Transtorno", append=TRUE)


###12 Seleção de Dados pa CO_SABE_LER_ESCREVER_MEMB
ler_escrever_2019 <- data.frame(count(munic_2019 , "CO_SABE_LER_ESCREVER_MEMB"))

ler_escrever_2019 <- ler_escrever_2019%>% replace_na(list(CO_SABE_LER_ESCREVER_MEMB = "Sem Dados"))

ler_escrever_2019$CO_SABE_LER_ESCREVER_MEMB  <- gsub(c("1"), c("Sim"), ler_escrever_2019$CO_SABE_LER_ESCREVER_MEMB)  

ler_escrever_2019$CO_SABE_LER_ESCREVER_MEMB  <- gsub(c("2"), c("Não"), ler_escrever_2019$CO_SABE_LER_ESCREVER_MEMB)  

write.xlsx(ler_escrever_2019, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/teresina/2019.xlsx",
           sheetName="Ler & Escrever", append=TRUE)

###13 Seleção de Dados pa GRAU_INSTRUCAO
instrucao_2019 <- data.frame(count(munic_2019 , "GRAU_INSTRUCAO"))

instrucao_2019 <- instrucao_2019%>% replace_na(list(GRAU_INSTRUCAO = "Sem Dados"))

instrucao_2019$GRAU_INSTRUCAO[which(instrucao_2019$GRAU_INSTRUCAO=="0")] <- "Sem Dados"
instrucao_2019$GRAU_INSTRUCAO[which(instrucao_2019$GRAU_INSTRUCAO=="1")] <- "Sem Instrução"
instrucao_2019$GRAU_INSTRUCAO[which(instrucao_2019$GRAU_INSTRUCAO=="2")] <- "Fundamental Incompleto"
instrucao_2019$GRAU_INSTRUCAO[which(instrucao_2019$GRAU_INSTRUCAO=="3")] <- "Fundamental Completo"
instrucao_2019$GRAU_INSTRUCAO[which(instrucao_2019$GRAU_INSTRUCAO=="4")] <- "Ensino Médio Incompleto"
instrucao_2019$GRAU_INSTRUCAO[which(instrucao_2019$GRAU_INSTRUCAO=="5")] <- "Ensino Médio Completo"
instrucao_2019$GRAU_INSTRUCAO[which(instrucao_2019$GRAU_INSTRUCAO=="6")] <- "Superior"


write.xlsx(instrucao_2019, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/teresina/2019.xlsx",
           sheetName="Instrução", append=TRUE)

###14 Seleção de Dados pa CO_CURSO_FREQ_PESSOA_MEMB
curso_2019 <- data.frame(count(munic_2019, "CO_CURSO_FREQ_PESSOA_MEMB"))

curso_2019 <- curso_2019%>% replace_na(list(CO_CURSO_FREQ_PESSOA_MEMB = "Sem Dados"))

curso_2019$CO_CURSO_FREQ_PESSOA_MEMB[which(curso_2019$CO_CURSO_FREQ_PESSOA_MEMB=="16")] <- "Sem Dados"

curso_2019$CO_CURSO_FREQ_PESSOA_MEMB[which(curso_2019$CO_CURSO_FREQ_PESSOA_MEMB=="15")] <- "Nenhum"

curso_2019$CO_CURSO_FREQ_PESSOA_MEMB[which(curso_2019$CO_CURSO_FREQ_PESSOA_MEMB=="14")] <- "Alfabetização para Adultos"

curso_2019$CO_CURSO_FREQ_PESSOA_MEMB[which(curso_2019$CO_CURSO_FREQ_PESSOA_MEMB=="13")] <- "Superior"

curso_2019$CO_CURSO_FREQ_PESSOA_MEMB[which(curso_2019$CO_CURSO_FREQ_PESSOA_MEMB=="12")] <- "Ensino Médio EJA"

curso_2019$CO_CURSO_FREQ_PESSOA_MEMB[which(curso_2019$CO_CURSO_FREQ_PESSOA_MEMB=="11")] <- "Ensino Fundamental EJA, Supletivo, 5ª a 8ª séries"

curso_2019$CO_CURSO_FREQ_PESSOA_MEMB[which(curso_2019$CO_CURSO_FREQ_PESSOA_MEMB=="10")] <- "Ensino Fundamental EJA, Supletivo, 1ª a 4ª séries"

curso_2019$CO_CURSO_FREQ_PESSOA_MEMB[which(curso_2019$CO_CURSO_FREQ_PESSOA_MEMB=="9")] <- "Ensino Médio Especial"

curso_2019$CO_CURSO_FREQ_PESSOA_MEMB[which(curso_2019$CO_CURSO_FREQ_PESSOA_MEMB=="8")] <- "Ensino Médio"

curso_2019$CO_CURSO_FREQ_PESSOA_MEMB[which(curso_2019$CO_CURSO_FREQ_PESSOA_MEMB=="7")] <- "Ensino Fundamental Especial"

curso_2019$CO_CURSO_FREQ_PESSOA_MEMB[which(curso_2019$CO_CURSO_FREQ_PESSOA_MEMB=="6")] <- "Ensino Fundamental"

curso_2019$CO_CURSO_FREQ_PESSOA_MEMB[which(curso_2019$CO_CURSO_FREQ_PESSOA_MEMB=="5")] <- "Ensino Fundamental 5ª a 8ª séries"

curso_2019$CO_CURSO_FREQ_PESSOA_MEMB[which(curso_2019$CO_CURSO_FREQ_PESSOA_MEMB=="4")] <- "Ensino Fundamental 1ª a 4ª séries"

curso_2019$CO_CURSO_FREQ_PESSOA_MEMB[which(curso_2019$CO_CURSO_FREQ_PESSOA_MEMB=="3")] <- "Classe de Alfabetização"

curso_2019$CO_CURSO_FREQ_PESSOA_MEMB[which(curso_2019$CO_CURSO_FREQ_PESSOA_MEMB=="2")] <- "Pré-escola (exceto CA)"

curso_2019$CO_CURSO_FREQ_PESSOA_MEMB[which(curso_2019$CO_CURSO_FREQ_PESSOA_MEMB=="1")] <- "Creche"


write.xlsx(curso_2019, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/teresina/2019.xlsx",
           sheetName="Cursando", append=TRUE)


###15 Seleção de Dados pa CO_PRINCIPAL_TRAB_MEMB

principal_trabalho_2019 <- data.frame(count(munic_2019 , "CO_PRINCIPAL_TRAB_MEMB"))

principal_trabalho_2019 <- principal_trabalho_2019%>% replace_na(list(CO_PRINCIPAL_TRAB_MEMB = "Sem Dados"))


principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB[which(principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB=="11")] <- "Aprendiz"

principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB[which(principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB=="10")] <- "Estagiário"

principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB <- gsub(c("9"), c("Empregador"), principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB)  

principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB <- gsub(c("8"), c("Militar ou servidor público"), principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB)  

principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB <- gsub(c("7"), c("Trabalhador não-remunerado"), principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB)  

principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB <- gsub(c("6"), c("Trabalhador doméstico com carteira de trabalho assinada"), principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB)  

principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB <- gsub(c("5"), c("Trabalhador doméstico sem carteira de trabalho assinada"), principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB)  

principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB <- gsub(c("4"), c("Empregado com carteira de trabalho assinada"), principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB)  

principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB <- gsub(c("3"), c("Empregado sem carteira de trabalho assinada"), principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB)  

principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB <- gsub(c("2"), c("Trabalhador temporário em área rural"), principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB)  

principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB <- gsub(c("1"), c("Informal, Autônomo"), principal_trabalho_2019$CO_PRINCIPAL_TRAB_MEMB)  

write.xlsx(principal_trabalho_2019, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/teresina/2019.xlsx",
           sheetName="Principal Trabalho", append=TRUE)

###16 Seleção de Dados pa CO_TRABALHO_12_MESES_MEMB

trabalho_12_meses_2019 <- data.frame(count(munic_2019 , "CO_TRABALHO_12_MESES_MEMB"))

trabalho_12_meses_2019 <- trabalho_12_meses_2019%>% replace_na(list(CO_TRABALHO_12_MESES_MEMB = "Sem Dados"))

trabalho_12_meses_2019$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2019$CO_TRABALHO_12_MESES_MEMB=="1")] <- "Sim"

trabalho_12_meses_2019$CO_TRABALHO_12_MESES_MEMB[which(trabalho_12_meses_2019$CO_TRABALHO_12_MESES_MEMB=="2")] <- "Não"

write.xlsx(trabalho_12_meses_2019, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/teresina/2019.xlsx",
           sheetName="Trabalho 12 Meses", append=TRUE)

