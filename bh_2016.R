library(readxl)
library(dplyr)
library(stringr)

### Tabela 2016 do Ministério das Cidades

brasil_munic_2016  <- read_csv("/Users/wemigliari/Documents/R/tabelas/pop_rua_min_cidades/TB_POP_RUA_201612.csv")

names(brasil_munic_2016)[1]<- "id"

### Subseccionar Dados de bh de 2016

munic_2016 <- subset(brasil_munic_2016, id %in% c(3106200))

###1 Seleção de Dados bhRENDA
renda_2016 <- data.frame(count(munic_2016, "FX_RENDA"))

renda_2016$FX_RENDA <- renda_2016$FX_RENDA[renda_2016$FX_RENDA==c(1, 2, 3, 4)]<-c("Até R$ 89,00", "Entre R$ 89,01 e R$ 178,00", "Até 1/2 Salário Mínimo", "Acima de 1/2 Salário Mínimo")

library(writexl)
library(xlsx)
write.xlsx(renda_2016, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/bh/2016.xlsx",
           sheetName="Renda", append=TRUE)

###2 Seleção de Dados bhPBF
pbf_2016 <- data.frame(count(munic_2016 , "MARC_PBF"))

pbf_2016$MARC_PBF <- pbf_2016$MARC_PBF[pbf_2016$MARC_PBF==c(0, 1)]<-c("Sem Dados", "Sim")

write.xlsx(pbf_2016, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/bh/2016.xlsx",
           sheetName="Bolsa Família", append=TRUE)

###3 Seleção de Dados bhFAMÍLIA INDÍGENA
fam_ind_2016 <- data.frame(count(munic_2016 , "IN_FAMILIA_INDIGENA_FAM"))

fam_ind_2016$IN_FAMILIA_INDIGENA_FAM <- fam_ind_2016$IN_FAMILIA_INDIGENA_FAM[fam_ind_2016$IN_FAMILIA_INDIGENA_FAM==c(2)]<-c("Não")

write.xlsx(fam_ind_2016, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/bh/2016.xlsx",
           sheetName="Indígenas", append=TRUE)

###4 Seleção de Dados bhFAMÍLIA QUILOMBOLA
quilom_2016 <- data.frame(count(munic_2016 , "IN_FAMILIA_QUILOMBOLA_FAM"))

quilom_2016$IN_FAMILIA_QUILOMBOLA_FAM <- quilom_2016$IN_FAMILIA_QUILOMBOLA_FAM[quilom_2016$IN_FAMILIA_QUILOMBOLA_FAM==c(2)]<-c("Não")

write.xlsx(quilom_2016, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/bh/2016.xlsx",
           sheetName="Quilombolas", append=TRUE)

###5 Seleção de Dados bhIN_PARC_MDS_FAM
mds_fam_2016 <- data.frame(count(munic_2016 , "IN_PARC_MDS_FAM"))

mds_fam_2016 <- mds_fam_2016%>% replace_na(list(IN_PARC_MDS_FAM = "Sem Dados"))

mds_fam_2016$IN_PARC_MDS_FAM <- gsub(c("000"), c("Nenhuma"), mds_fam_2016$IN_PARC_MDS_FAM)  

mds_fam_2016$IN_PARC_MDS_FAM <- gsub(c("101"), c("Família Cigana"), mds_fam_2016$IN_PARC_MDS_FAM)  

mds_fam_2016$IN_PARC_MDS_FAM <- gsub(c("202"), c("Família de Pescadores Artesanais"), mds_fam_2016$IN_PARC_MDS_FAM)

mds_fam_2016$IN_PARC_MDS_FAM <- gsub(c("205"), c("Família de Agricultores Familiares"), mds_fam_2016$IN_PARC_MDS_FAM) 

mds_fam_2016$IN_PARC_MDS_FAM <- gsub(c("303"), c("Família Acampada"), mds_fam_2016$IN_PARC_MDS_FAM)

mds_fam_2016$IN_PARC_MDS_FAM <- gsub(c("304"), c("Família Atingida por Empreendimentos de Infraestrutura"), mds_fam_2016$IN_PARC_MDS_FAM)

mds_fam_2016$IN_PARC_MDS_FAM <- gsub(c("305"), c("Família de Preso do Sistema Carcerário"), mds_fam_2016$IN_PARC_MDS_FAM)  

mds_fam_2016$IN_PARC_MDS_FAM <- gsub(c("306"), c("Família de Catadores de Material Reciclável"), mds_fam_2016$IN_PARC_MDS_FAM)  


write.xlsx(mds_fam_2016, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/bh/2016.xlsx",
           sheetName="Grupos Tradicionais Ebhecíficos", append=TRUE)

###6 Seleção de Dados bhMESES_APOS_ULT_ATUALIZACAO
### Já está pronto o quadro, pois não há o que identificar entre as rebhostas
ult_atua_2016 <- data.frame(count(munic_2016 , "MESES_APOS_ULT_ATUALIZACAO"))

write.xlsx(ult_atua_2016, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/bh/2016.xlsx",
           sheetName="Atualização", append=TRUE)

###7 Seleção de Dados bhCO_EST_CADASTRAL_MEMB
esta_cadas_2016 <- data.frame(count(munic_2016 , "CO_EST_CADASTRAL_MEMB"))

esta_cadas_2016$CO_EST_CADASTRAL_MEMB <- esta_cadas_2016$CO_EST_CADASTRAL_MEMB [esta_cadas_2016$CO_EST_CADASTRAL_MEMB ==c(2, 3, 4)]<-c("Sem Registro Civil", "Cadastrado", "Excluído")

write.xlsx(esta_cadas_2016, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/bh/2016.xlsx",
           sheetName="Estado Cadastral", append=TRUE)

###8 Seleção de Dados bhCO_SEXO_PESSOA
sexo_2016 <- data.frame(count(munic_2016 , "CO_SEXO_PESSOA"))

sexo_2016$CO_SEXO_PESSOA <- sexo_2016$CO_SEXO_PESSOA[sexo_2016$CO_SEXO_PESSOA==c(1, 2)]<-c("Masculino", "Feminino")

write.xlsx(sexo_2016, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/bh/2016.xlsx",
           sheetName="Sexo", append=TRUE)

###9 Seleção de Dados bhCO_RACA_COR_PESSOA
cor_2016 <- data.frame(count(munic_2016 , "CO_RACA_COR_PESSOA"))

cor_2016 <- cor_2016%>% replace_na(list(CO_RACA_COR_PESSOA = "Sem Dados"))

cor_2016$CO_RACA_COR_PESSOA <- cor_2016$CO_RACA_COR_PESSOA[cor_2016$CO_RACA_COR_PESSOA==c(1, 2, 3, 4, 5, 6)]<-c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Sem Dados")

write.xlsx(cor_2016, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/bh/2016.xlsx",
           sheetName="Cor", append=TRUE)

###10 Seleção de Dados bhFX_ETARIA
idade_2016 <- data.frame(count(munic_2016 , "FX_ETARIA"))

idade_2016$FX_ETARIA <- idade_2016$FX_ETARIA[idade_2016$FX_ETARIA==c(1, 2, 3, 4, 5, 6)]<-c("Até 11 anos", "De 12 a 17 anos", "De 18 a 21 anos", "De 22 a 29 anos", "De 30 a 59 anos", "De 60 anos acima")

write.xlsx(idade_2016, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/bh/2016.xlsx",
           sheetName="Idade", append=TRUE)

###11 Seleção de Dados bhIN_DEF_TRANSTORNO_MENTAL_MEMB
transtorno_2016 <- data.frame(count(munic_2016 , "IN_DEF_TRANSTORNO_MENTAL_MEMB"))

transtorno_2016 <- transtorno_2016%>% replace_na(list(IN_DEF_TRANSTORNO_MENTAL_MEMB = "Sem Dados"))

transtorno_2016$IN_DEF_TRANSTORNO_MENTAL_MEMB <- transtorno_2016$IN_DEF_TRANSTORNO_MENTAL_MEMB[transtorno_2016$IN_DEF_TRANSTORNO_MENTAL_MEMB ==c(1, "Sem Dados")]<-c("Sim", "Sem Dados")

write.xlsx(transtorno_2016, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/bh/2016.xlsx",
           sheetName="Transtorno", append=TRUE)


###12 Seleção de Dados bhCO_SABE_LER_ESCREVER_MEMB
ler_escrever_2016 <- data.frame(count(munic_2016 , "CO_SABE_LER_ESCREVER_MEMB"))

ler_escrever_2016 <- ler_escrever_2016%>% replace_na(list(CO_SABE_LER_ESCREVER_MEMB = "Sem Dados"))

ler_escrever_2016$CO_SABE_LER_ESCREVER_MEMB <- ler_escrever_2016$CO_SABE_LER_ESCREVER_MEMB [ler_escrever_2016$CO_SABE_LER_ESCREVER_MEMB ==c(1, 2, "Sem Dados")]<-c("Sim", "Não", "Sem Dados")

write.xlsx(ler_escrever_2016, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/bh/2016.xlsx",
           sheetName="Ler & Escrever", append=TRUE)

###13 Seleção de Dados bhGRAU_INSTRUCAO
instrucao_2016 <- data.frame(count(munic_2016 , "GRAU_INSTRUCAO"))

instrucao_2016$GRAU_INSTRUCAO <- instrucao_2016$GRAU_INSTRUCAO[instrucao_2016$GRAU_INSTRUCAO==c(0, 1, 2, 3, 4, 5, 6)]<-c("Sem Dados", "Sem Instrução", "Fundamental Incompleto", "Fundamental Completo", "Médio Incompleto", "Médio Completo", "Superior Completo")

write.xlsx(instrucao_2016, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/bh/2016.xlsx",
           sheetName="Instrução", append=TRUE)

###14 Seleção de Dados bhCO_CURSO_FREQ_PESSOA_MEMB
curso_2016 <- data.frame(count(munic_2016 , "CO_CURSO_FREQ_PESSOA_MEMB"))

curso_2016 <- curso_2016%>% replace_na(list(CO_CURSO_FREQ_PESSOA_MEMB = "Sem Dados"))

curso_2016$CO_CURSO_FREQ_PESSOA_MEMB <- curso_2016$CO_CURSO_FREQ_PESSOA_MEMB[curso_2016$CO_CURSO_FREQ_PESSOA_MEMB==c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, "Sem Dados")]<-c("Creche", "Pré-escola (exceto CA)", "Classe de Alfabetização", "Ensino Fundamental 1ª a 4ª séries", "Ensino Fundamental 5ª a 8ª séries", "Ensino Fundamental", "Ensino Fundamental Ebhecial", "Ensino Médio", "Ensino Médio Ebhecial", "Ensino Fundamental EJA, Supletivo, 1ª a 4ª séries", "Ensino Fundamental EJA, Supletivo, 5ª a 8ª séries", "Ensino Médio EJA", "Superior", "Alfabetização para Adultos", "Nenhum", "Sem Dados")

write.xlsx(curso_2016, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/bh/2016.xlsx",
           sheetName="Cursando", append=TRUE)


###15 Seleção de Dados bhCO_PRINCIPAL_TRAB_MEMB

principal_trabalho_2016 <- data.frame(count(munic_2016 , "CO_PRINCIPAL_TRAB_MEMB"))

principal_trabalho_2016 <- principal_trabalho_2016%>% replace_na(list(CO_PRINCIPAL_TRAB_MEMB = "Sem Dados"))

principal_trabalho_2016$CO_PRINCIPAL_TRAB_MEMB <- principal_trabalho_2016$CO_PRINCIPAL_TRAB_MEMB [principal_trabalho_2016$CO_PRINCIPAL_TRAB_MEMB ==c(1, 2, 3, 4, 5, 6, "Sem Dados")]<-c("Informal, Autônomo", "Trabalhador temporário em área rural", "Empregado sem carteira de trabalho assinada", "Empregado com carteira de trabalho assinada", "Trabalhador doméstico sem carteira de trabalho assinada", "Trabalhador doméstico com carteira de trabalho assinada", "Sem Dados")

write.xlsx(principal_trabalho_2016, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/bh/2016.xlsx",
           sheetName="Principal Trabalho", append=TRUE)

###16 Seleção de Dados bhCO_TRABALHO_12_MESES_MEMB

trabalho_12_meses_2016 <- data.frame(count(munic_2016 , "CO_TRABALHO_12_MESES_MEMB"))

trabalho_12_meses_2016 <- trabalho_12_meses_2016%>% replace_na(list(CO_TRABALHO_12_MESES_MEMB = "Sem Dados"))

trabalho_12_meses_2016$CO_TRABALHO_12_MESES_MEMB <- trabalho_12_meses_2016$CO_TRABALHO_12_MESES_MEMB [trabalho_12_meses_2016$CO_TRABALHO_12_MESES_MEMB ==c(1, 2, "Sem Dados")]<-c("Sim", "Não", "Sem Dados")

write.xlsx(trabalho_12_meses_2016, "/Users/wemigliari/Documents/R/tabelas/min_cid_observatorio/bh/2016.xlsx",
           sheetName="Trabalho 12 Meses", append=TRUE)




