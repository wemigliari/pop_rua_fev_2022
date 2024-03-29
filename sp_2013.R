library(readxl)
library(dplyr)
library(stringr)

### Tabela 2013 do Ministério das Cidades

brasil_munic_2013  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201312.csv")

names(brasil_munic_2013)[1]<- "id"

### Subseccionar Dados de SP de 2013

munic_2013 <- subset(brasil_munic_2013, id %in% c(3550308))

###1 Seleção de Dados SP RENDA
renda_2013 <- data.frame(count(munic_2013, "FX_RENDA"))

renda_2013$FX_RENDA <- renda_2013$FX_RENDA[renda_2013$FX_RENDA==c(1, 2, 3, 4)]<-c("Até R$ 89,00", "Entre R$ 89,01 e R$ 178,00", "Até 1/2 Salário Mínimo", "Acima de 1/2 Salário Mínimo")

library(writexl)
library(xlsx)
write.xlsx(renda_2013, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2013.xlsx",
           sheetName="Renda", append=TRUE)

###2 Seleção de Dados SP PBF
pbf_2013 <- data.frame(count(munic_2013 , "MARC_PBF"))

pbf_2013$MARC_PBF <- pbf_2013$MARC_PBF[pbf_2013$MARC_PBF==c(0)]<-c("Sem Dados")

write.xlsx(pbf_2013, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2013.xlsx",
           sheetName="Bolsa Família", append=TRUE)

###3 Seleção de Dados SP FAMÍLIA INDÍGENA
fam_ind_2013 <- data.frame(count(munic_2013 , "IN_FAMILIA_INDIGENA_FAM"))

fam_ind_2013$IN_FAMILIA_INDIGENA_FAM <- fam_ind_2013$IN_FAMILIA_INDIGENA_FAM[fam_ind_2013$IN_FAMILIA_INDIGENA_FAM==c(2)]<-c("Não")

write.xlsx(fam_ind_2013, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2013.xlsx",
           sheetName="Indígenas", append=TRUE)

###4 Seleção de Dados SP FAMÍLIA QUILOMBOLA
quilom_2013 <- data.frame(count(munic_2013 , "IN_FAMILIA_QUILOMBOLA_FAM"))

quilom_2013$IN_FAMILIA_QUILOMBOLA_FAM <- quilom_2013$IN_FAMILIA_QUILOMBOLA_FAM[quilom_2013$IN_FAMILIA_QUILOMBOLA_FAM==c(1, 2)]<-c("Sim", "Não")

write.xlsx(quilom_2013, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2013.xlsx",
           sheetName="Quilombolas", append=TRUE)

###5 Seleção de Dados SP IN_PARC_MDS_FAM
mds_fam_2013 <- data.frame(count(munic_2013 , "IN_PARC_MDS_FAM"))

mds_fam_2013 <- mds_fam_2013%>% replace_na(list(IN_PARC_MDS_FAM = "Sem Dados"))

mds_fam_2013$IN_PARC_MDS_FAM <- gsub(c("205"), c("Família de Agricultores Familiares"), mds_fam_2013$IN_PARC_MDS_FAM)  

mds_fam_2013$IN_PARC_MDS_FAM <- gsub(c("305"), c("Família de Preso do Sistema Carcerário"), mds_fam_2013$IN_PARC_MDS_FAM)  

mds_fam_2013$IN_PARC_MDS_FAM <- gsub(c("306"), c("Família de Catadores de Material Reciclável"), mds_fam_2013$IN_PARC_MDS_FAM)  


write.xlsx(mds_fam_2013, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2013.xlsx",
           sheetName="Grupos Tradicionais Específicos", append=TRUE)

###6 Seleção de Dados SP MESES_APOS_ULT_ATUALIZACAO
### Já está pronto o quadro, pois não há o que identificar entre as respostas
ult_atua_2013 <- data.frame(count(munic_2013 , "MESES_APOS_ULT_ATUALIZACAO"))

write.xlsx(ult_atua_2013, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2013.xlsx",
           sheetName="Atualização", append=TRUE)

###7 Seleção de Dados SP CO_EST_CADASTRAL_MEMB
esta_cadas_2013 <- data.frame(count(munic_2013 , "CO_EST_CADASTRAL_MEMB"))

esta_cadas_2013$CO_EST_CADASTRAL_MEMB <- esta_cadas_2013$CO_EST_CADASTRAL_MEMB [esta_cadas_2013$CO_EST_CADASTRAL_MEMB ==c(1, 2, 3, 4, 5, 6)]<-c("Em Cadastramento", "Sem Registro Civil", "Cadastrado", "Excluído", "Aguardando NIS" ,"Validando NIS")

write.xlsx(esta_cadas_2013, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2013.xlsx",
           sheetName="Estado Cadastral", append=TRUE)

###8 Seleção de Dados SP CO_SEXO_PESSOA
sexo_2013 <- data.frame(count(munic_2013 , "CO_SEXO_PESSOA"))

sexo_2013$CO_SEXO_PESSOA <- sexo_2013$CO_SEXO_PESSOA[sexo_2013$CO_SEXO_PESSOA==c(1, 2)]<-c("Masculino", "Feminino")

write.xlsx(sexo_2013, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2013.xlsx",
           sheetName="Sexo", append=TRUE)

###9 Seleção de Dados SP CO_RACA_COR_PESSOA
cor_2013 <- data.frame(count(munic_2013 , "CO_RACA_COR_PESSOA"))

cor_2013 <- cor_2013%>% replace_na(list(CO_RACA_COR_PESSOA = "Sem Dados"))

cor_2013$CO_RACA_COR_PESSOA <- cor_2013$CO_RACA_COR_PESSOA[cor_2013$CO_RACA_COR_PESSOA==c(1, 2, 3, 4, 5, 6)]<-c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Sem Dados")

write.xlsx(cor_2013, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2013.xlsx",
           sheetName="Cor", append=TRUE)

###10 Seleção de Dados SP FX_ETARIA
idade_2013 <- data.frame(count(munic_2013 , "FX_ETARIA"))

idade_2013$FX_ETARIA <- idade_2013$FX_ETARIA[idade_2013$FX_ETARIA==c(1, 2, 3, 4, 5, 6)]<-c("Até 11 anos", "De 12 a 17 anos", "De 18 a 21 anos", "De 22 a 29 anos", "De 30 a 59 anos", "De 60 anos acima")

write.xlsx(idade_2013, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2013.xlsx",
           sheetName="Idade", append=TRUE)

###11 Seleção de Dados SP IN_DEF_TRANSTORNO_MENTAL_MEMB
transtorno_2013 <- data.frame(count(munic_2013 , "IN_DEF_TRANSTORNO_MENTAL_MEMB"))

transtorno_2013 <- transtorno_2013%>% replace_na(list(IN_DEF_TRANSTORNO_MENTAL_MEMB = "Sem Dados"))

transtorno_2013$IN_DEF_TRANSTORNO_MENTAL_MEMB <- transtorno_2013$IN_DEF_TRANSTORNO_MENTAL_MEMB[transtorno_2013$IN_DEF_TRANSTORNO_MENTAL_MEMB ==c(0, 1, "Sem Dados")]<-c("Não", "Sim", "Sem Dados")

write.xlsx(transtorno_2013, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2013.xlsx",
           sheetName="Transtorno", append=TRUE)


###12 Seleção de Dados SP CO_SABE_LER_ESCREVER_MEMB
ler_escrever_2013 <- data.frame(count(munic_2013 , "CO_SABE_LER_ESCREVER_MEMB"))

ler_escrever_2013 <- ler_escrever_2013%>% replace_na(list(CO_SABE_LER_ESCREVER_MEMB = "Sem Dados"))

ler_escrever_2013$CO_SABE_LER_ESCREVER_MEMB <- ler_escrever_2013$CO_SABE_LER_ESCREVER_MEMB [ler_escrever_2013$CO_SABE_LER_ESCREVER_MEMB ==c(1, 2, "Sem Dados")]<-c("Sim", "Não", "Sem Dados")

write.xlsx(ler_escrever_2013, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2013.xlsx",
           sheetName="Ler & Escrever", append=TRUE)

###13 Seleção de Dados SP GRAU_INSTRUCAO
instrucao_2013 <- data.frame(count(munic_2013 , "GRAU_INSTRUCAO"))

instrucao_2013$GRAU_INSTRUCAO <- instrucao_2013$GRAU_INSTRUCAO[instrucao_2013$GRAU_INSTRUCAO==c(0, 1, 2, 3, 4, 5, 6)]<-c("Sem Dados", "Sem Instrução", "Fundamental Incompleto", "Fundamental Completo", "Ensino Médio Incompleto", "Ensino Médio Completo", "Superior Incompleto ou Mais")

write.xlsx(instrucao_2013, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2013.xlsx",
           sheetName="Instrução", append=TRUE)

###14 Seleção de Dados SP CO_CURSO_FREQ_PESSOA_MEMB
curso_2013 <- data.frame(count(munic_2013 , "CO_CURSO_FREQ_PESSOA_MEMB"))

curso_2013 <- curso_2013%>% replace_na(list(CO_CURSO_FREQ_PESSOA_MEMB = "Sem Dados"))

curso_2013$CO_CURSO_FREQ_PESSOA_MEMB <- curso_2013$CO_CURSO_FREQ_PESSOA_MEMB[curso_2013$CO_CURSO_FREQ_PESSOA_MEMB==c(1, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, "Sem Dados")]<-c("Creche", "Classe de Alfabetização", "Ensino Fundamental 1ª a 4ª séries", "Ensino Fundamental 5ª a 8ª séries", "Ensino Fundamental", "Ensino Fundamental Especial", "Ensino Médio", "Ensino Médio Especial", "Ensino Fundamental EJA, Supletivo, 1ª a 4ª séries", "Ensino Fundamental EJA, Supletivo, 5ª a 8ª séries", "Ensino Médio EJA", "Superior", "Alfabetização para Adultos", "Nenhum", "Sem Dados")

write.xlsx(curso_2013, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2013.xlsx",
           sheetName="Cursando", append=TRUE)


###15 Seleção de Dados SP CO_PRINCIPAL_TRAB_MEMB

principal_trabalho_2013 <- data.frame(count(munic_2013 , "CO_PRINCIPAL_TRAB_MEMB"))

principal_trabalho_2013 <- principal_trabalho_2013%>% replace_na(list(CO_PRINCIPAL_TRAB_MEMB = "Sem Dados"))

principal_trabalho_2013$CO_PRINCIPAL_TRAB_MEMB <- principal_trabalho_2013$CO_PRINCIPAL_TRAB_MEMB [principal_trabalho_2013$CO_PRINCIPAL_TRAB_MEMB ==c(1, 2, 3, 4, 5, 6, 8, "Sem Dados")]<-c("Informal, Autônomo", "Trabalhador temporário em área rural", "Empregado sem carteira de trabalho assinada", "Empregado com carteira de trabalho assinada", "Trabalhador doméstico sem carteira de trabalho assinada", "Trabalhador doméstico com carteira de trabalho assinada", "Militar ou servidor público", "Sem Dados")

write.xlsx(principal_trabalho_2013, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2013.xlsx",
           sheetName="Principal Trabalho", append=TRUE)

###16 Seleção de Dados SP CO_TRABALHO_12_MESES_MEMB

trabalho_12_meses_2013 <- data.frame(count(munic_2013 , "CO_TRABALHO_12_MESES_MEMB"))

trabalho_12_meses_2013 <- trabalho_12_meses_2013%>% replace_na(list(CO_TRABALHO_12_MESES_MEMB = "Sem Dados"))

trabalho_12_meses_2013$CO_TRABALHO_12_MESES_MEMB <- trabalho_12_meses_2013$CO_TRABALHO_12_MESES_MEMB [trabalho_12_meses_2013$CO_TRABALHO_12_MESES_MEMB ==c(1, 2, "Sem Dados")]<-c("Sim", "Não", "Sem Dados")

write.xlsx(trabalho_12_meses_2013, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/sp/2013.xlsx",
           sheetName="Trabalho 12 Meses", append=TRUE)




