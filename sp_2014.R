library(readxl)
library(dplyr)
library(stringr)

### Tabela 2014 do Ministério das Cidades

brasil_munic_2014  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201412.csv")

names(brasil_munic_2014)[1]<- "id"

### Subseccionar Dados de SP de 2014

munic_2014 <- subset(brasil_munic_2014, id %in% c(3550308))

###1 Seleção de Dados SP RENDA
renda_2014 <- data.frame(count(munic_2014, "FX_RENDA"))

renda_2014$FX_RENDA <- renda_2014$FX_RENDA[renda_2014$FX_RENDA==c(1, 2, 3, 4)]<-c("Até R$ 89,00", "Entre R$ 89,01 e R$ 178,00", "Até 1/2 Salário Mínimo", "Acima de 1/2 Salário Mínimo")

library(writexl)
library(xlsx)
write.xlsx(renda_2014, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2014.xlsx",
           sheetName="Renda", append=TRUE)

###2 Seleção de Dados SP PBF
pbf_2014 <- data.frame(count(munic_2014 , "MARC_PBF"))

pbf_2014$MARC_PBF <- pbf_2014$MARC_PBF[pbf_2014$MARC_PBF==c(0, 1)]<-c("Sem Dados", "Sim")

write.xlsx(pbf_2014, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2014.xlsx",
           sheetName="Bolsa Família", append=TRUE)

###3 Seleção de Dados SP FAMÍLIA INDÍGENA
fam_ind_2014 <- data.frame(count(munic_2014 , "IN_FAMILIA_INDIGENA_FAM"))

fam_ind_2014$IN_FAMILIA_INDIGENA_FAM <- fam_ind_2014$IN_FAMILIA_INDIGENA_FAM[fam_ind_2014$IN_FAMILIA_INDIGENA_FAM==c(2)]<-c("Não")

write.xlsx(fam_ind_2014, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2014.xlsx",
           sheetName="Indígenas", append=TRUE)

###4 Seleção de Dados SP FAMÍLIA QUILOMBOLA
quilom_2014 <- data.frame(count(munic_2014 , "IN_FAMILIA_QUILOMBOLA_FAM"))

quilom_2014$IN_FAMILIA_QUILOMBOLA_FAM <- quilom_2014$IN_FAMILIA_QUILOMBOLA_FAM[quilom_2014$IN_FAMILIA_QUILOMBOLA_FAM==c(1, 2)]<-c("Sim", "Não")

write.xlsx(quilom_2014, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2014.xlsx",
           sheetName="Quilombolas", append=TRUE)

###5 Seleção de Dados SP IN_PARC_MDS_FAM
mds_fam_2014 <- data.frame(count(munic_2014 , "IN_PARC_MDS_FAM"))

mds_fam_2014 <- mds_fam_2014%>% replace_na(list(IN_PARC_MDS_FAM = "Sem Dados"))

mds_fam_2014$IN_PARC_MDS_FAM <- gsub(c("000"), c("Nenhuma"), mds_fam_2014$IN_PARC_MDS_FAM)  

mds_fam_2014$IN_PARC_MDS_FAM <- gsub(c("101"), c("Família Cigana"), mds_fam_2014$IN_PARC_MDS_FAM)  

mds_fam_2014$IN_PARC_MDS_FAM <- gsub(c("205"), c("Família de Agricultores Familiares"), mds_fam_2014$IN_PARC_MDS_FAM)  

mds_fam_2014$IN_PARC_MDS_FAM <- gsub(c("305"), c("Família de Preso do Sistema Carcerário"), mds_fam_2014$IN_PARC_MDS_FAM)  

mds_fam_2014$IN_PARC_MDS_FAM <- gsub(c("306"), c("Família de Catadores de Material Reciclável"), mds_fam_2014$IN_PARC_MDS_FAM)  


write.xlsx(mds_fam_2014, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2014.xlsx",
           sheetName="Grupos Tradicionais Específicos", append=TRUE)

###6 Seleção de Dados SP MESES_APOS_ULT_ATUALIZACAO
### Já está pronto o quadro, pois não há o que identificar entre as respostas
ult_atua_2014 <- data.frame(count(munic_2014 , "MESES_APOS_ULT_ATUALIZACAO"))

write.xlsx(ult_atua_2014, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2014.xlsx",
           sheetName="Atualização", append=TRUE)

###7 Seleção de Dados SP CO_EST_CADASTRAL_MEMB
esta_cadas_2014 <- data.frame(count(munic_2014 , "CO_EST_CADASTRAL_MEMB"))

esta_cadas_2014$CO_EST_CADASTRAL_MEMB <- esta_cadas_2014$CO_EST_CADASTRAL_MEMB [esta_cadas_2014$CO_EST_CADASTRAL_MEMB ==c(1, 2, 3, 4, 5, 6)]<-c("Em Cadastramento", "Sem Registro Civil", "Cadastrado", "Excluído", "Aguardando NIS" ,"Validando NIS")

write.xlsx(esta_cadas_2014, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2014.xlsx",
           sheetName="Estado Cadastral", append=TRUE)

###8 Seleção de Dados SP CO_SEXO_PESSOA
sexo_2014 <- data.frame(count(munic_2014 , "CO_SEXO_PESSOA"))

sexo_2014$CO_SEXO_PESSOA <- sexo_2014$CO_SEXO_PESSOA[sexo_2014$CO_SEXO_PESSOA==c(1, 2)]<-c("Masculino", "Feminino")

write.xlsx(sexo_2014, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2014.xlsx",
           sheetName="Sexo", append=TRUE)

###9 Seleção de Dados SP CO_RACA_COR_PESSOA
cor_2014 <- data.frame(count(munic_2014 , "CO_RACA_COR_PESSOA"))

cor_2014 <- cor_2014%>% replace_na(list(CO_RACA_COR_PESSOA = "Sem Dados"))

cor_2014$CO_RACA_COR_PESSOA <- cor_2014$CO_RACA_COR_PESSOA[cor_2014$CO_RACA_COR_PESSOA==c(1, 2, 3, 4, 5, 6)]<-c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Sem Dados")

write.xlsx(cor_2014, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2014.xlsx",
           sheetName="Cor", append=TRUE)

###10 Seleção de Dados SP FX_ETARIA
idade_2014 <- data.frame(count(munic_2014 , "FX_ETARIA"))

idade_2014$FX_ETARIA <- idade_2014$FX_ETARIA[idade_2014$FX_ETARIA==c(1, 2, 3, 4, 5, 6)]<-c("Até 11 anos", "De 12 a 17 anos", "De 18 a 21 anos", "De 22 a 29 anos", "De 30 a 59 anos", "De 60 anos acima")

write.xlsx(idade_2014, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2014.xlsx",
           sheetName="Idade", append=TRUE)

###11 Seleção de Dados SP IN_DEF_TRANSTORNO_MENTAL_MEMB
transtorno_2014 <- data.frame(count(munic_2014 , "IN_DEF_TRANSTORNO_MENTAL_MEMB"))

transtorno_2014 <- transtorno_2014%>% replace_na(list(IN_DEF_TRANSTORNO_MENTAL_MEMB = "Sem Dados"))

transtorno_2014$IN_DEF_TRANSTORNO_MENTAL_MEMB <- transtorno_2014$IN_DEF_TRANSTORNO_MENTAL_MEMB[transtorno_2014$IN_DEF_TRANSTORNO_MENTAL_MEMB ==c(1, "Sem Dados")]<-c("Sim", "Sem Dados")

write.xlsx(transtorno_2014, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2014.xlsx",
           sheetName="Transtorno", append=TRUE)


###12 Seleção de Dados SP CO_SABE_LER_ESCREVER_MEMB
ler_escrever_2014 <- data.frame(count(munic_2014 , "CO_SABE_LER_ESCREVER_MEMB"))

ler_escrever_2014 <- ler_escrever_2014%>% replace_na(list(CO_SABE_LER_ESCREVER_MEMB = "Sem Dados"))

ler_escrever_2014$CO_SABE_LER_ESCREVER_MEMB <- ler_escrever_2014$CO_SABE_LER_ESCREVER_MEMB [ler_escrever_2014$CO_SABE_LER_ESCREVER_MEMB ==c(1, 2, "Sem Dados")]<-c("Sim", "Não", "Sem Dados")

write.xlsx(ler_escrever_2014, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2014.xlsx",
           sheetName="Ler & Escrever", append=TRUE)

###13 Seleção de Dados SP GRAU_INSTRUCAO
instrucao_2014 <- data.frame(count(munic_2014 , "GRAU_INSTRUCAO"))

instrucao_2014$GRAU_INSTRUCAO <- instrucao_2014$GRAU_INSTRUCAO[instrucao_2014$GRAU_INSTRUCAO==c(0, 1, 2, 3, 4, 5, 6)]<-c("Sem Dados", "Sem Instrução", "Fundamental Incompleto", "Fundamental Completo", "Ensino Médio Incompleto", "Ensino Médio Completo", "Superior Incompleto ou Mais")

write.xlsx(instrucao_2014, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2014.xlsx",
           sheetName="Instrução", append=TRUE)

###14 Seleção de Dados SP CO_CURSO_FREQ_PESSOA_MEMB
curso_2014 <- data.frame(count(munic_2014 , "CO_CURSO_FREQ_PESSOA_MEMB"))

curso_2014 <- curso_2014%>% replace_na(list(CO_CURSO_FREQ_PESSOA_MEMB = "Sem Dados"))

curso_2014$CO_CURSO_FREQ_PESSOA_MEMB <- curso_2014$CO_CURSO_FREQ_PESSOA_MEMB[curso_2014$CO_CURSO_FREQ_PESSOA_MEMB==c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, "Sem Dados")]<-c("Creche", "Pré-escola (exceto CA)", "Classe de Alfabetização", "Ensino Fundamental 1ª a 4ª séries", "Ensino Fundamental 5ª a 8ª séries", "Ensino Fundamental", "Ensino Fundamental Especial", "Ensino Médio", "Ensino Médio Especial", "Ensino Fundamental EJA, Supletivo, 1ª a 4ª séries", "Ensino Fundamental EJA, Supletivo, 5ª a 8ª séries", "Ensino Médio EJA", "Superior", "Alfabetização para Adultos", "Nenhum", "Sem Dados")

write.xlsx(curso_2014, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2014.xlsx",
           sheetName="Cursando", append=TRUE)


###15 Seleção de Dados SP CO_PRINCIPAL_TRAB_MEMB

principal_trabalho_2014 <- data.frame(count(munic_2014 , "CO_PRINCIPAL_TRAB_MEMB"))

principal_trabalho_2014 <- principal_trabalho_2014%>% replace_na(list(CO_PRINCIPAL_TRAB_MEMB = "Sem Dados"))

principal_trabalho_2014$CO_PRINCIPAL_TRAB_MEMB <- principal_trabalho_2014$CO_PRINCIPAL_TRAB_MEMB [principal_trabalho_2014$CO_PRINCIPAL_TRAB_MEMB ==c(1, 2, 3, 4, 5, 6, 8, "Sem Dados")]<-c("Informal, Autônomo", "Trabalhador temporário em área rural", "Empregado sem carteira de trabalho assinada", "Empregado com carteira de trabalho assinada", "Trabalhador doméstico sem carteira de trabalho assinada", "Trabalhador doméstico com carteira de trabalho assinada", "Militar ou servidor público", "Sem Dados")

write.xlsx(principal_trabalho_2014, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2014.xlsx",
           sheetName="Principal Trabalho", append=TRUE)

###16 Seleção de Dados SP CO_TRABALHO_12_MESES_MEMB

trabalho_12_meses_2014 <- data.frame(count(munic_2014 , "CO_TRABALHO_12_MESES_MEMB"))

trabalho_12_meses_2014 <- trabalho_12_meses_2014%>% replace_na(list(CO_TRABALHO_12_MESES_MEMB = "Sem Dados"))

trabalho_12_meses_2014$CO_TRABALHO_12_MESES_MEMB <- trabalho_12_meses_2014$CO_TRABALHO_12_MESES_MEMB [trabalho_12_meses_2014$CO_TRABALHO_12_MESES_MEMB ==c(1, 2, "Sem Dados")]<-c("Sim", "Não", "Sem Dados")

write.xlsx(trabalho_12_meses_2014, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2014.xlsx",
           sheetName="Trabalho 12 Meses", append=TRUE)




