###PACOTES
install.packages("readxl")
install.packages("ggplot2")
install.packages("dplyr")
install.packages("stringr")
install.packages("AggregateR")
install.packages("dummy")
install.packages("doBy")
install.packages("sqldf")
install.packages("reshape2")
install.packages("xlsx")
library(xlsx)
library(reshape2)
library(sqldf)
library(doBy)
library("readxl")
library("ggplot2")
library("dplyr")
library("stringr")
library("AggregateR")
library("dummy")

#FUNCAO PARA GERAR MULTIPLOS PLOTS DE UMA SO VEZ
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}

#LENDO OS DADOS
dados=read_excel("Base de dados_Processo Seletivo_com_ID.xlsx",skip = 1, sheet=1)
#Coluna 1 e 2 -> dados referentes a identificações
#Coluna 3 à 9  -> dados referentes a contemplações
#Coluna 10 à 16 -> dados referentes as notas dos parâmetros
#Coluna 17 em diante -> dados referente as solicitações

#Duas colunas com o mesmo nome
colnames(dados) <- c("ID","Ano do processo seletivo","Bolsa","Vigência Início",
                     "Vigência Fim","Numero Curso", "Nível","IC no momento da contemplação",
                     "Desvinculação da bolsa","IC","RT","GF","DG","MT","TR","EP",
                     "Curso","BAS (solicitada)","Alimentação (solicitada)",
                     "Transporte (solicitada)","Moradia (solicitada)",
                     "BAS (contemplada)","Alimentação (contemplada)",
                     "Transporte  (contemplada)","Moradia  (contemplada)",
                     "Bolsa Auxílio|Moradia","PAAIS","Divergência?","Observação")


##Arrumando o PAAIS

paais_novo1= str_detect(dados$PAAIS,": SIM")
paais_novo2=dados$PAAIS=='SIM'
dados$paais_novo=paais_novo1+paais_novo2



###SEPARANDO O BOLSA AUXILIO E MORADIA
bolsa_aux=c()
moradia=c()
dados$`Bolsa Auxílio|Moradia`=str_replace(dados$`Bolsa Auxílio|Moradia`,"[|]","")
for (i in c(1:length(dados$`Bolsa Auxílio|Moradia`))){
  aux=unlist(str_split(dados$`Bolsa Auxílio|Moradia`[i],"[|]"))
  bolsa_aux=append(bolsa_aux,aux[1])
  moradia=append(moradia,aux[2])
  aux=c()
}

moradia[which(dados$`Bolsa Auxílio|Moradia`==0)]=0 # tinha um valor que era 0

dados$bolsa_aux=bolsa_aux
dados$moradia=moradia

#Criando as colunas referentes aos níveis e números dos cursos
dados$numero_curso_sol <- regmatches(dados$Curso, gregexpr("[[:digit:]]+", 
                                                           dados$Curso))
dados$nivel_curso_sol<- regmatches(dados$Curso, gregexpr("(?=\\().*?(?<=\\))",
                                                         dados$Curso, perl=T))
dados$nivel_curso_sol <- gsub("[^[:alpha:]]", "", dados$nivel_curso_sol)
dados$nivel_curso_sol <- as.character(dados$nivel_curso_sol)
dados$numero_curso_sol <- as.numeric(dados$numero_curso_sol)

#Subset para entender os dados faltantes dessas novas colunas
t <- subset(dados, dados$nivel_curso_sol == "character") 
# antes tinhamos 6731 "characters", quando confrontamos essa coluna com a dos 
#contemplados esse numero de "characteres" cai para 1795 
t2 <- subset(dados, dados$Nível == "NA")
t2.2 <- subset(t2, t2$nivel_curso_sol!= "character")
t3 <- subset(dados, dados$`Numero Curso` == "NA")
t4 <- subset(dados, is.na(dados$numero_curso_sol))
# antes tinhamos 6755 NAs, quando confrontamos essa coluna com a dos contemplados
#esse numero de NAs cai para 1803

#Preenchendo os dados "characters" da coluna nivel_curso_sol
for (i in 1:nrow(dados)){
  if (dados[i,7] != "NA"){
    if (dados[i,34] == "character"){
      dados[i,34] <- dados[i,7]
    }
  }
}
#Preenchendo os dados "NA" da coluna nivel_curso_sol
for (i in 1:nrow(dados)){
  if (dados[i,6] != "NA"){
    if (is.na(dados[i,33])){
      dados[i,33] <- dados[i,6]
    }
  }
}

#Criando colunas que acreditamos serem relevantes para o estudo
dados$tipo_curso <- NA
G_Artes<-c(26,25,64,23,22)
intersect(G_Artes, dados$numero_curso_sol)
#Cursos Ok
G_Biologicas<-c(6,100,27,45,21,63,58,46,15,107,14)
intersect(G_Biologicas, dados$numero_curso_sol)
#Cursos Ok
G_Exatas<-c(48,42,36,83,73,87,8,89,12,13,43,34,49,101,102,88,
            11,41,108,10,9,39,2,4,53,40,29,1,28,51,5,50,94,52)
intersect(G_Exatas, dados$numero_curso_sol)
#Não temos o curso 108 na coluna numero_curso_sol para esse nivel
G_Humanas<-c(109,110,17,47,16,44,75,30,54,55,19,7,57,56,18,20,38)
intersect(G_Humanas, dados$numero_curso_sol)
#Não temos os cursos 109 e 110 na coluna numero_curso_sol para esse nivel
#os cursos 59,71,70,72 não foram encontrados para inclusao nos vetores

M_Artes<-c(87,88,30,73)
intersect(M_Artes, dados$numero_curso_sol)
#Cusros ok
M_Humanas<-c(93,92,20,53,17,81,18,40,7,36,37,75,24,19,80,38,54,26,79,41)
intersect(M_Humanas, dados$numero_curso_sol)
#Cursos ok
M_Exatas<-c(91,47,56,57,55,8,89,12,11,72,10,39,9,3,4,86,2,1,29,5,82)
intersect(M_Exatas, dados$numero_curso_sol)
#Cursos ok
M_Biologicas<-c(84,94,49,58,35,42,74,51,50,44,90,48,28,59,67,23,71,70,14,69,
                83,65,61,60,63,64,62)
intersect(M_Biologicas, dados$numero_curso_sol)
#Cursos ok
#o curso 46 não foi encontrado para inclusao nos vetores

D_Artes<-c(105,106,2,22)
intersect(D_Artes, dados$numero_curso_sol)
#Cursos ok
D_Humanas<-c(107,33,70,105,106,2,22,21,67,68,93,57,28,25,30,66,94,74,69,103,27,
             26,76,24,94)
intersect(D_Humanas, dados$numero_curso_sol)
#Não temos os cursos 33 e 94 na coluna numero_curso_sol para esse nivel
D_Exatas<-c(108,81,34,6,7,5,58,32,62,98,60,92,59,35,53,54,102,1,51,79,55)
intersect(D_Exatas, dados$numero_curso_sol)
#Não temos os cursos 108 e 32 na coluna numero_curso_sol para esse nivel
D_Biologicas<-c(89,8,75,97,23,104,90,87,36,91,61,78,100,16,73,20,19,64,18,101,
                14,10,9,12,13,11)
intersect(D_Biologicas, dados$numero_curso_sol)
#Cursos ok

S_Humanas<-c(14,11)
intersect(S_Humanas, dados$numero_curso_sol)
#Cursos ok
S_Exatas<-c(4,8)
intersect(S_Exatas, dados$numero_curso_sol)
#Cursos ok
S_Biologicas<-c(12,13,9,10,7,1,15)
intersect(S_Biologicas, dados$numero_curso_sol)
#Cursos ok

n_encontrados <- c(59,71,70,72,46)
intersect(n_encontrados, dados$numero_curso_sol)
#Todos os cursos não encontrados para inclusao nos vetores existem na coluna
#Temos 7 cursos que não existem na coluna numero_curso_col e 5 cursos que nao 
#conseguimos encontrar. Talvez um dos 7 cursos seja referente a um dos 5 nao 
#encontrados.


#teste de quantos NAs deveriamos ter no fim do nosso loop
t5 <- subset(dados, dados$nivel_curso_sol != "character")
summary(as.factor(t5$numero_curso_sol))
#criando um grupo sem as observações "character" conseguimos perceber que existem
#apenas 8 NAs. Logo, o total de NAs no fim do nosso loop deverá ser 1795 + 8 = 1803 

#Realizando o loop
for (i in 1:nrow(dados)){
  if(dados[i,34]=="G"){
    if(dados[i,33]%in%G_Artes){
      dados[i,35] <- "Artes"
    }
    if(dados[i,33]%in%G_Biologicas){
      dados[i,35] <- "Biologicas"
    }
    if(dados[i,33]%in%G_Exatas){
      dados[i,35] <- "Exatas"
    }
    if(dados[i,33]%in%G_Humanas){
      dados[i,35] <- "Humanas"
    }
  }
  if(dados[i,34]=="M"){
    if(dados[i,33]%in%M_Artes){
      dados[i,35] <- "Artes"
    }
    if(dados[i,33]%in%M_Biologicas){
      dados[i,35] <- "Biologicas"
    }
    if(dados[i,33]%in%M_Exatas){
      dados[i,35] <- "Exatas"
    }
    if(dados[i,33]%in%M_Humanas){
      dados[i,35] <- "Humanas"
    }
  }
  if(dados[i,34]=="D"){
    if(dados[i,33]%in%D_Artes){
      dados[i,35] <- "Artes"
    }
    if(dados[i,33]%in%D_Biologicas){
      dados[i,35] <- "Biologicas"
    }
    if(dados[i,33]%in%D_Exatas){
      dados[i,35] <- "Exatas"
    }
    if(dados[i,33]%in%D_Humanas){
      dados[i,35] <- "Humanas"
    }
  }
  if(dados[i,34]=="S"){
    if(dados[i,33]%in%S_Biologicas){
      dados[i,35] <- "Biologicas"
    }
    if(dados[i,33]%in%S_Exatas){
      dados[i,35] <- "Exatas"
    }
    if(dados[i,33]%in%S_Humanas){
      dados[i,35] <- "Humanas"
    }
  }
  if(dados[i,6] == 200){
    dados[i,35] <- "Profis"
  }
}
#Com o loop, conseguimos 4962 NAs. Precisamos verificar qual erro está ocorrendo.


#Data Frame sem repetição
a <- data.frame()
a <- dados %>% 
  group_by(ID, `Ano do processo seletivo`) %>% 
  summarise(BAS=mean(`BAS (solicitada)`),
            Alimentacao = mean(`Alimentação (solicitada)`),
            Transporte = mean(`Transporte (solicitada)`),
            Moradia = mean(`Moradia (solicitada)`),
            Contemplacao_BAS = mean(`BAS (contemplada)`),
            Contemplacao_Alimentacao = mean(`Alimentação (contemplada)`),
            Contemplacao_Transporte = mean(`Transporte  (contemplada)`),
            Contemplacao_Moradia = mean(`Moradia  (contemplada)`),
            paais_novo = mean(paais_novo),
            tipo_curso = tipo_curso[1],
            nivel_curso_sol = nivel_curso_sol[1],
            IC = IC[1],
            RT = RT[1],
            GF = GF[1])
#TABELAS PARA OS SOLICITANTES:

#Tabela 1: numero de solicitações por tipo de curso
table(a$tipo_curso, exclude = NULL)
#Tabela 2: numero de solicitações por nivel de curso
table(a$nivel_curso_sol, exclude = NULL)
#Tabela 3: numero de solicitacoes por tipo de bolsa
num_bas <- sum(a$BAS)
num_al <- sum(a$Alimentacao)
num_tran <- sum(a$Transporte)
num_moras <- sum(a$Moradia)
tabela_numero_sol_bolsa <- data.frame(num_bas,num_al,num_tran,num_moras)
colnames(tabela_numero_sol_bolsa) <- c("BAS","Alimentação","Transporte","Moradia")
rownames(tabela_numero_sol_bolsa) <- "Solicitacoes"

#Tabela 4: renda média por morador comparada com os tipos de bolsas
srb <- tapply((a$RT/a$GF),a$BAS,mean)[2]
sra <- tapply((a$RT/a$GF),a$Alimentacao,mean)[2]
srt <- tapply((a$RT/a$GF),a$Transporte ,mean)[2]
srm <- tapply((a$RT/a$GF),a$Moradia ,mean)[2]
tabela_renda_sol_bolsa <- data.frame(srb,sra,srt,srm)
colnames(tabela_renda_sol_bolsa) <- c("BAS","Alimentação","Transporte","Moradia")
rownames(tabela_renda_sol_bolsa) <- "Renda"

#Tabela 5: renda média por morador para os tipos de cursos comparadas com as bolsas
srbtc <- tapply((a$RT/a$GF),list(a$BAS,a$tipo_curso),mean)[2,]
sratc <- tapply((a$RT/a$GF),list(a$Alimentacao,a$tipo_curso),mean)[2,]
srttc <- tapply((a$RT/a$GF),list(a$Transporte,a$tipo_curso),mean)[2,]
srmtc <- tapply((a$RT/a$GF),list(a$Moradia,a$tipo_curso),mean)[2,]
tabela_renda_sol_bolsa_tipo_curso <- data.frame(srbtc,sratc,srttc,srmtc)
colnames(tabela_renda_sol_bolsa_tipo_curso) <- c("BAS","Alimentação","Transporte",
                                                 "Moradia")

#Tabela 6: renda média por morador para os niveis de cursos comparadas com as bolsas
srbnc <- tapply((a$RT/a$GF),list(a$BAS,a$nivel_curso_sol),mean)[2,]
sranc <- tapply((a$RT/a$GF),list(a$Alimentacao,a$nivel_curso_sol),mean)[2,]
srtnc <- tapply((a$RT/a$GF),list(a$Transporte,a$nivel_curso_sol),mean)[2,]
srmnc <- tapply((a$RT/a$GF),list(a$Moradia,a$nivel_curso_sol),mean)[2,]
tabela_renda_sol_bolsa_nivel_curso <- data.frame(srbnc,sranc,srtnc,srmnc)
colnames(tabela_renda_sol_bolsa_nivel_curso) <- c("BAS","Alimentação","Transporte",
                                                  "Moradia")

#Tabela 7: PAAIS em relação a solicitação
spb <- table(a[ , c(3,11)])[2,]
spa <- table(a[ , c(4,11)])[2,]
spt <- table(a[ , c(5,11)])[2,]
spm <- table(a[ , c(6,11)])[2,]
tabela_paais_bolsa <- data.frame(spb,spa,spt,spm)
colnames(tabela_paais_bolsa) <- c("BAS","Alimentação","Transporte",
                                  "Moradia")
rownames(tabela_paais_bolsa) <- c("SEM PAAIS", "COM PAAIS")
tabela_prop_paais_bolsa <- data.frame(prop.table(as.matrix(tabela_paais_bolsa),2))

#Tabela 8: PAAIS em relação a solicitação contra o tipo dos cursos
spc_sem <- table(a[ , c(11,12)])[1,]
spc_com <- table(a[ , c(11,12)])[2,]
tabela_paais_bolsa_tipo_curso <- data.frame(spc_sem,spc_com)
tabela_paais_bolsa_tipo_curso <- t(as.matrix(tabela_paais_bolsa_tipo_curso))
tabela_paais_bolsa_tipo_curso <- as.data.frame(tabela_paais_bolsa_tipo_curso)
rownames(tabela_paais_bolsa_tipo_curso) <- c("SEM PAAIS", "COM PAAIS")
tabela_prop_paais_bolsa_tipo_curso <- data.frame(prop.table(as.matrix(tabela_paais_bolsa_tipo_curso),2))


#TABELAS PARA OS CONTEMPLADOS:

#Tabela 1: numero de contemplaçoes por tipo de bolsa
ncb <- table(a$Contemplacao_BAS, exclude = NULL)
nca <- table(a$Contemplacao_Alimentacao, exclude = NULL)
nct <- table(a$Contemplacao_Transporte, exclude = NULL)
ncm <- table(a$Contemplacao_Moradia, exclude = NULL)
tabela_numero_contemplacao_bolsa <- data.frame(ncb,nca,nct,ncm)
tabela_numero_contemplacao_bolsa <- tabela_numero_contemplacao_bolsa[,c(-1,-3,-5,-7)]
colnames(tabela_numero_contemplacao_bolsa) <- c("BAS","Alimentacao","Transporte",
                                                "Moradia")
rownames(tabela_numero_contemplacao_bolsa) <- c("tipo0","tipo1","tipo2")

#Tabela 2: numero de solicitações por tipo de curso
tabela_numero_contemplacao_bolsa_tipo_curso <- table(a$Contemplacao_BAS,a$tipo_curso) +
  table(a$Contemplacao_Alimentacao,a$tipo_curso)+
  table(a$Contemplacao_Transporte,a$tipo_curso) +
  table(a$Contemplacao_Moradia,a$tipo_curso)

#Tabela 3: numero de solicitações por nivel de curso
tabela_numero_contemplacao_bolsa_nivel_curso <- table(a$Contemplacao_BAS,a$nivel_curso_sol) +
  table(a$Contemplacao_Alimentacao,a$nivel_curso_sol)+
  table(a$Contemplacao_Transporte,a$nivel_curso_sol) +
  table(a$Contemplacao_Moradia,a$nivel_curso_sol)

#Tabela 4: renda média por morador comparada com os tipos de bolsas
crb <- tapply((a$RT/a$GF),a$Contemplacao_BAS,mean)
cra <- tapply((a$RT/a$GF),a$Contemplacao_Alimentacao,mean)
crt <- tapply((a$RT/a$GF),a$Contemplacao_Transporte ,mean)
crm <- tapply((a$RT/a$GF),a$Contemplacao_Moradia ,mean)
tabela_renda_con_bolsa <- data.frame(crb,cra,crt,crm)
colnames(tabela_renda_con_bolsa) <- c("BAS","Alimentação","Transporte","Moradia")
rownames(tabela_renda_con_bolsa) <- c("tipo0","tipo1","tipo2")

#Tabela 5: renda média por morador para os tipos de cursos comparadas com as bolsas contempladas 
#tipo1
crbt1 <- tapply((a$RT/a$GF),list(a$Contemplacao_BAS,a$tipo_curso),mean)[2,]
crat1 <- tapply((a$RT/a$GF),list(a$Contemplacao_Alimentacao,a$tipo_curso),mean)[2,]
crtt1 <- tapply((a$RT/a$GF),list(a$Contemplacao_Transporte,a$tipo_curso) ,mean)[2,]
crmt1 <- tapply((a$RT/a$GF),list(a$Contemplacao_Moradia,a$tipo_curso) ,mean)[2,]
tabela_renda_con_bolsa1_tipo_curso <- data.frame(crbt1,crat1,crtt1,crmt1)
colnames(tabela_renda_con_bolsa1_tipo_curso) <- c("BAS","Alimentação","Transporte","Moradia")

#tipo2
crbt2 <- tapply((a$RT/a$GF),list(a$Contemplacao_BAS,a$tipo_curso),mean)[3,]
crat2 <- tapply((a$RT/a$GF),list(a$Contemplacao_Alimentacao,a$tipo_curso),mean)[3,]
crtt2 <- tapply((a$RT/a$GF),list(a$Contemplacao_Transporte,a$tipo_curso) ,mean)[3,]
crmt2 <- tapply((a$RT/a$GF),list(a$Contemplacao_Moradia,a$tipo_curso) ,mean)[3,]
tabela_renda_con_bolsa2_tipo_curso <- data.frame(crbt2,crat2,crtt2,crmt2)
colnames(tabela_renda_con_bolsa2_tipo_curso) <- c("BAS","Alimentação","Transporte","Moradia")

tabela_renda_con_bolsa_tipo_curso <- cbind(tabela_renda_con_bolsa1_tipo_curso,
                                           tabela_renda_con_bolsa2_tipo_curso)
colnames(tabela_renda_con_bolsa_tipo_curso) <- c("BAS_t1",
                                                 "Al_t1",
                                                 "Tr_t1",
                                                 "Mora_t1",
                                                 "BAS_t2",
                                                 "Al_t2",
                                                 "Tr_t2",
                                                 "Mora_t2")
rownames(tabela_renda_con_bolsa_tipo_curso) <- c("Artes",
                                                 "Biologicas",
                                                 "Exatas",
                                                 "Humanas",
                                                 "Profis")

tabela_renda_con_bolsa_tipo_curso2 <- data.frame(tabela_renda_con_bolsa_tipo_curso$BAS_t1,
                                                 tabela_renda_con_bolsa_tipo_curso$BAS_t2,
                                                 tabela_renda_con_bolsa_tipo_curso$Al_t1,
                                                 tabela_renda_con_bolsa_tipo_curso$Al_t2,
                                                 tabela_renda_con_bolsa_tipo_curso$Tr_t1,
                                                 tabela_renda_con_bolsa_tipo_curso$Tr_t2,
                                                 tabela_renda_con_bolsa_tipo_curso$Mora_t1,
                                                 tabela_renda_con_bolsa_tipo_curso$Mora_t2)

colnames(tabela_renda_con_bolsa_tipo_curso2) <- c("BAS_t1","BAS_t2","Al_t1","Al_t2",
                                                  "Tr_t1","Tr_t2",
                                                  "Mora_t1","Mora_t2")
rownames(tabela_renda_con_bolsa_tipo_curso2) <- c("Artes","Biologicas","Exatas","Humanas","Profis")

#Tabela 6: renda média por morador para os niveis de cursos comparadas com as bolsas
#tipo1
cnbt1 <- tapply((a$RT/a$GF),list(a$Contemplacao_BAS,a$nivel_curso_sol),mean)[2,]
cnat1 <- tapply((a$RT/a$GF),list(a$Contemplacao_Alimentacao,a$nivel_curso_sol),mean)[2,]
cntt1 <- tapply((a$RT/a$GF),list(a$Contemplacao_Transporte,a$nivel_curso_sol) ,mean)[2,]
cnmt1 <- tapply((a$RT/a$GF),list(a$Contemplacao_Moradia,a$nivel_curso_sol) ,mean)[2,]
tabela_nivel_con_bolsa1_tipo_curso <- data.frame(cnbt1,cnat1,cntt1,cnmt1)
colnames(tabela_nivel_con_bolsa1_tipo_curso) <- c("BAS","Alimentação","Transporte","Moradia")

#tipo2
cnbt2 <- tapply((a$RT/a$GF),list(a$Contemplacao_BAS,a$nivel_curso_sol),mean)[3,]
cnat2 <- tapply((a$RT/a$GF),list(a$Contemplacao_Alimentacao,a$nivel_curso_sol),mean)[3,]
cntt2 <- tapply((a$RT/a$GF),list(a$Contemplacao_Transporte,a$nivel_curso_sol) ,mean)[3,]
cnmt2 <- tapply((a$RT/a$GF),list(a$Contemplacao_Moradia,a$nivel_curso_sol) ,mean)[3,]
tabela_nivel_con_bolsa2_tipo_curso <- data.frame(cnbt2,cnat2,cntt2,cnmt2)
colnames(tabela_nivel_con_bolsa2_tipo_curso) <- c("BAS","Alimentação","Transporte","Moradia")

tabela_nivel_con_bolsa_tipo_curso <- cbind(tabela_nivel_con_bolsa1_tipo_curso,
                                           tabela_nivel_con_bolsa2_tipo_curso)

colnames(tabela_nivel_con_bolsa_tipo_curso) <- c("BAS_t1",
                                                 "Al_t1",
                                                 "Tr_t1",
                                                 "Mora_t1",
                                                 "BAS_t2",
                                                 "Al_t2",
                                                 "Tr_t2",
                                                 "Mora_t2")
rownames(tabela_nivel_con_bolsa_tipo_curso) <-  c("character",
                                                  "D",
                                                  "G",
                                                  "M",
                                                  "S",
                                                  "T")

tabela_nivel_con_bolsa_tipo_curso2 <- data.frame(tabela_nivel_con_bolsa_tipo_curso$BAS_t1,
                                                 tabela_nivel_con_bolsa_tipo_curso$BAS_t2,
                                                 tabela_nivel_con_bolsa_tipo_curso$Al_t1,
                                                 tabela_nivel_con_bolsa_tipo_curso$Al_t2,
                                                 tabela_nivel_con_bolsa_tipo_curso$Tr_t1,
                                                 tabela_nivel_con_bolsa_tipo_curso$Tr_t2,
                                                 tabela_nivel_con_bolsa_tipo_curso$Mora_t1,
                                                 tabela_nivel_con_bolsa_tipo_curso$Mora_t2)

colnames(tabela_nivel_con_bolsa_tipo_curso2) <- c("BAS_t1","BAS_t2","Al_t1","Al_t2",
                                                  "Tr_t1","Tr_t2",
                                                  "Mora_t1","Mora_t2")
rownames(tabela_nivel_con_bolsa_tipo_curso2) <- c("character","D","G","M","S","T")
table(a$nivel_curso_sol,a$Contemplacao_BAS)


# GRAFICOS:

#Grafico 1: Boxplot para comparar as contemplações com o IC e renda percapta
g1 <- ggplot(a, aes(as.factor(Contemplacao_BAS),(RT/GF),fill=as.factor(Contemplacao_BAS))) + geom_boxplot() + ylim(0,5000) + theme_light() + labs(y="Renda por Individuo da familia",x="Situacao") + ggtitle("Renda vs Situação BAS") + scale_fill_discrete(name="Situacao",breaks=c("0","1","2"),labels=c("Indeferido","Deferido","Contemplado e Desistiu"))

g2 <- ggplot(a, aes(as.factor(Contemplacao_BAS),IC,fill=as.factor(Contemplacao_BAS))) + geom_boxplot() + ylim(0,5000) + theme_light() + labs(y="IC",x="Situacao") + ggtitle("IC vs Situação BAS") + scale_fill_discrete(name="Situacao",breaks=c("0","1","2"),labels=c("Indeferido","Deferido","Contemplado e Desistiu"))

g1.1 <- ggplot(a, aes(as.factor(Contemplacao_Alimentacao),(RT/GF),fill=as.factor(Contemplacao_Alimentacao))) + geom_boxplot() + ylim(0,5000) + theme_light() + labs(y="Renda por Individuo da familia",x="Situacao") + ggtitle("Renda vs Situação Alimentacao") + scale_fill_discrete(name="Situacao",breaks=c("0","1","2"),labels=c("Indeferido","Deferido","Contemplado e Desistiu"))

g2.1 <- ggplot(a, aes(as.factor(Contemplacao_Alimentacao),IC,fill=as.factor(Contemplacao_Alimentacao))) + geom_boxplot() + ylim(0,5000) + theme_light() + labs(y="IC",x="Situacao") + ggtitle("IC vs Situação Aliemantacao") + scale_fill_discrete(name="Situacao",breaks=c("0","1","2"),labels=c("Indeferido","Deferido","Contemplado e Desistiu"))

g1.2 <- ggplot(a, aes(as.factor(Contemplacao_Transporte),(RT/GF),fill=as.factor(Contemplacao_Transporte))) + geom_boxplot() + ylim(0,5000) + theme_light() + labs(y="Renda por Individuo da familia",x="Situacao") + ggtitle("Renda vs Situação Transporte") + scale_fill_discrete(name="Situacao",breaks=c("0","1","2"),labels=c("Indeferido","Deferido","Contemplado e Desistiu"))

g2.2 <- ggplot(a, aes(as.factor(Contemplacao_Transporte),IC,fill=as.factor(Contemplacao_Transporte))) + geom_boxplot() + ylim(0,5000) + theme_light() + labs(y="IC",x="Situacao") + ggtitle("IC vs Situação Transporte") + scale_fill_discrete(name="Situacao",breaks=c("0","1","2"),labels=c("Indeferido","Deferido","Contemplado e Desistiu"))

g1.3 <- ggplot(a, aes(as.factor(Contemplacao_Moradia),(RT/GF),fill=as.factor(Contemplacao_Moradia))) + geom_boxplot() + ylim(0,5000) + theme_light() + labs(y="Renda por Individuo da familia",x="Situacao") + ggtitle("Renda vs Situação Moradia") + scale_fill_discrete(name="Situacao",breaks=c("0","1","2"),labels=c("Indeferido","Deferido","Contemplado e Desistiu"))

g2.3 <- ggplot(a, aes(as.factor(Contemplacao_Moradia),IC,fill=as.factor(Contemplacao_Moradia))) + geom_boxplot() + ylim(0,5000) + theme_light() + labs(y="IC",x="Situacao") + ggtitle("IC vs Situação Moradia") + scale_fill_discrete(name="Situacao",breaks=c("0","1","2"),labels=c("Indeferido","Deferido","Contemplado e Desistiu"))

g1.1.2 <- ggplot(a, aes(as.factor(Contemplacao_Moradia),(RT/GF),fill=as.factor(Contemplacao_BAS))) + geom_boxplot() + ylim(0,5000) + theme_light() + labs(y="Renda por Individuo da familia",x="Situacao") + ggtitle("Renda vs Situação (Moradia & BAS)") + scale_fill_discrete(name="Situacao",breaks=c("0","1","2"),labels=c("Indeferido","Deferido","Contemplado e Desistiu"))


multiplot(g1,g2)
multiplot(g1.1,g2.1)
multiplot(g1.2,g2.2)
multiplot(g1.3,g2.3)
g1.1.2

#Grafico 2: Histograma para compara contagem de solicitações
teste <- data.frame(t(tabela_numero_sol_bolsa),Bolsas)
g3 <- ggplot(a, aes(tipo_curso)) + geom_bar()
g4 <- ggplot(a, aes(nivel_curso_sol)) + geom_bar()
Bolsas <- c("BAS","Alimentação","Transporte","Moradia")
g5 <- ggplot(teste,aes(x=Bolsas,y= Solicitacoes)) + geom_bar(stat="identity")

multiplot(g3,g4,g5)

#Grafico 3: Histograma para Renda dos solicitantes
barplot((as.matrix(tabela_renda_sol_bolsa)),main="Renda x Bolsas")
barplot(t(as.matrix(tabela_renda_sol_bolsa_tipo_curso)),main="Renda x Bolsas (Tipo Curso)",beside=TRUE)
barplot(t(as.matrix(tabela_renda_sol_bolsa_nivel_curso)),main="Renda x Bolsas (Nivel Curso)",beside=TRUE)

#Grafico 4: Histograma para os paais dos solicitantes
barplot((as.matrix(tabela_paais_bolsa)),main="Quantidade de Bolasas Solicitas (PAAIS)",beside=TRUE)
legend(2.8,-1,c("group A", "group B"), pch = c(1,2), lty = c(1,2))
barplot((as.matrix(tabela_paais_bolsa_tipo_curso)),main="Quantidade de Bolasas Solicitas por Tipo Curso (PAAIS)",beside=TRUE)

#Grafico 5: Histograma para comparar contagem de contemplações
barplot((as.matrix(tabela_numero_contemplacao_bolsa)),main="Quantidade Bolsas",beside = TRUE)
barplot((as.matrix(tabela_numero_contemplacao_bolsa_tipo_curso)),main="Quantidade Bolsas (Tipo Curso)",beside = TRUE)
barplot((as.matrix(tabela_numero_contemplacao_bolsa_nivel_curso)),main="Quantidade Bolsas (Nivel Curso)",beside = TRUE)

#Grafico 6: Histograma para comparar as rendas com as contemplações
barplot((as.matrix(tabela_renda_con_bolsa)),main="Renda x Bolsas",beside=TRUE)
barplot((as.matrix(tabela_renda_con_bolsa_tipo_curso2)),main="Renda x Bolsas (Tipo Curso)",
        beside=TRUE)
barplot((as.matrix(tabela_nivel_con_bolsa_tipo_curso2)),main="Renda x Bolsas (Nivel Curso)",beside=TRUE)

#Grafico 3: Histograma para Renda dos solicitantes
barplot((as.matrix(tabela_renda_sol_bolsa)),main="Renda x Bolsas")


barplot(t(as.matrix(tabela_renda_sol_bolsa_tipo_curso)),main="Renda x Bolsas (Tipo Curso)",beside=TRUE,legend=TRUE,args.legend = list(x="topleft"))


barplot(t(as.matrix(tabela_renda_sol_bolsa_nivel_curso)),main="Renda x Bolsas (Nivel Curso)",beside=TRUE,legend=TRUE,args.legend = list(x="top"))

#Grafico 4: Histograma para os paais dos solicitantes
barplot((as.matrix(tabela_paais_bolsa)),main="Quantidade de Bolasas Solicitas (PAAIS)",beside=TRUE,legend=TRUE,args.legend = list(x="top"))

legend(2.8,-1,c("group A", "group B"), pch = c(1,2), lty = c(1,2))

barplot((as.matrix(tabela_paais_bolsa_tipo_curso)),main="Quantidade de Bolasas Solicitas por Tipo Curso (PAAIS)",beside=TRUE,legend=TRUE,args.legend = list(x="topright"))


#Grafico 5: Histograma para comparar contagem de contemplações
barplot((as.matrix(tabela_numero_contemplacao_bolsa)),main="Quantidade Bolsas",beside = TRUE,legend=TRUE,args.legend = list(x="topright"))



barplot((as.matrix(tabela_numero_contemplacao_bolsa_tipo_curso)),main="Quantidade Bolsas (Tipo Curso)",beside = TRUE,legend=TRUE,args.legend = list(x="topright"))

barplot((as.matrix(tabela_numero_contemplacao_bolsa_nivel_curso)),main="Quantidade Bolsas (Nivel Curso)",beside = TRUE,legend=TRUE,args.legend = list(x="topright"))

#Grafico 6: Histograma para comparar as rendas com as contemplações
barplot((as.matrix(tabela_renda_con_bolsa)),main="Renda x Bolsas",beside=TRUE,legend=TRUE,args.legend = list(x="topright"))


barplot((as.matrix(tabela_renda_con_bolsa_tipo_curso2)),main="Renda x Bolsas (Tipo Curso)",
        beside=TRUE,legend=TRUE,args.legend = list(x="topright"))

barplot((as.matrix(tabela_nivel_con_bolsa_tipo_curso2)),main="Renda x Bolsas (Nivel Curso)",beside=TRUE,legend=TRUE,args.legend = list(x="topright"))


#-------------- VARIAVEIS -----------------------------------------------------------------------------------

# Indices redutores MT e TR
ggplot(a, aes("", MT)) + geom_boxplot() # 95 informaÃ§Ãµes incorretas
ggplot(a, aes("", TR)) + geom_boxplot() # 31 informaÃ§Ãµes incorretas

# Podemos notar que hÃ¡ erros no banco de dados, jÃ¡ que as variÃ¡veis MT e TR devem ser valores entre 0 e 1
# Devemos assim desconsiderar as observaÃ§Ãµes com esses erros

# Removendo essas observaÃ§Ãµes obtemos a seguinte distribuiÃ§Ã£o
ggplot(a, aes("", MT)) + geom_boxplot() + ylim(c(0,1))
ggplot(a, aes("", TR)) + geom_boxplot() + ylim(c(0,1))

# Quartis
quantile(a$MT[which(a$MT>0)])
quantile(a$TR[which(a$TR>0)])

# Iremos entÃ£o considerar essas variÃ¡veis como binÃ¡rias, ou seja, tudo que for < 1 serÃ¡ considerado como 0
# Assim como as variÃ¡vis dummys DG e EP

# Categorizando a Renda per capita
a$RPC <- a$RT/a$GF

# Para serem comparÃ¡veis, devemos utilizar a renda em relaÃ§Ã£o ao salÃ¡rio mÃ�nimo 
# do ano em que a bolsa foi solicitada

# Tabela dos salÃ¡rios mÃ�nimos de 2006 - 2012
salario_minimo <- data.frame(ano = c(2006, 2007, 2008, 2009, 2010, 2011, 2012))
salario_minimo$valor <- c(350, 380, 415, 465, 510, 545, 622)


names(salario_minimo) <- c('Ano do processo seletivo', 'salario_minimo')

a <- a %>% left_join(salario_minimo, by = 'Ano do processo seletivo')

# Numero salarios minimos per capita
a$n_sm_percapita <- a$RPC/a$salario_minimo

# Substituindo 2 por 1 na contemplacao
a <- a %>% gather(bolsa, contemplou, names(a[,7:10])) %>% 
  mutate(contemplou = if_else(contemplou<1, 0, 1)) %>% spread(bolsa, contemplou)

# Boxplot das bolsas contemplados e nao contemplados (salario minimo)
teste=a %>% gather(bolsa, contemplou, names(a[,20:23])) %>% 
  mutate(contemplou = if_else(contemplou<1, 0, 1))%>%
  mutate(bolsa1=if_else(bolsa=="Contemplacao_Alimentacao" | bolsa=="Contemplacao_Transporte", "BAT",
                        if_else(bolsa=="Contemplacao_BAS","BAS",
                                if_else(bolsa=="Contemplacao_Moradia","PME","NA"))))

### GRÃFICO BONITO ###############################################################
ggplot(teste, aes(fill=as.factor(contemplou),x=bolsa1, y=n_sm_percapita)) + 
  geom_boxplot() + 
  theme_light()+
  ylim(0,5)+
  scale_fill_discrete(name="Situacao", label=c("Nao Contemplou","Contemplou"))+
  labs(x="Bolsa",y="Salarios Minimos")
ggsave("salario_minimo.png")
quantile(a$n_sm_percapita[which(a$n_sm_percapita<6)], na.rm = TRUE)

# Ideia salario minimo TEVEZ
teste1=a %>% mutate(sm_p = salario_minimo/GF) %>% mutate( smr= RPC/sm_p)
quantile(teste1$smr[which(teste1$smr<6)], na.rm = TRUE)


#Categorizando os salÃ¡rio mÃ�nimos
a$sm_categorizado<-NA

a %>% mutate(sm_categ=if_else(n_sm_percapita==0,"0",
                              if_else(n_sm_percapita > 0 | n_sm_percapita < 0,"(0 - 1)")))

a$sm_categorizado[0<=a$n_sm_percapita]<-'n1'
a$sm_categorizado[1<=a$n_sm_percapita]<-'n2'
a$sm_categorizado[1.5<=a$n_sm_percapita]<-'n3'
a$sm_categorizado[3<=a$n_sm_percapita]<-'n4'

# Eliminando registros errados
# Anos NA
a <- a[which(!is.na(a$`Ano do processo seletivo`)),]

# Indices negativos (MT e TR)
a <- a[which(a$MT>=0),]
a <- a[which(a$TR>=0),]


#--------------DADOS POR BOLSA---------------------------------------------------------

a$ano <- as.factor(a$`Ano do processo seletivo`)
a$BAS <- as.factor(a$BAS)
a$Alimentacao <- as.factor(a$Alimentacao)
a$Transporte <- as.factor(a$Transporte)
a$Moradia <- as.factor(a$Moradia)
a$paais_novo <- as.factor(a$paais_novo)
a$tipo_curso <- as.factor(a$tipo_curso)
a$nivel_curso_sol <- as.factor(a$nivel_curso_sol)
a$DG <- as.factor(a$DG)
a$MT <- as.factor(ifelse(a$MT==1, 1, 0))
a$TR <- as.factor(ifelse(a$TR==1, 1, 0))
a$EP <- as.factor(a$EP)
a$Contemplacao_BAS <- as.factor(a$Contemplacao_BAS)
a$Contemplacao_Moradia <- as.factor(a$Contemplacao_Moradia)
a$Contemplacao_Alimentacao <- as.factor(a$Contemplacao_Alimentacao)
a$Contemplacao_Transporte <- as.factor(a$Contemplacao_Transporte)
a$sm_categorizado <- as.factor(a$sm_categorizado)
a$ja_teve_bolsa= as.factor(!is.na(a$ja_teve_bolsa))


bas_solicitados <- a %>% filter(BAS==1)
bat_solicitados <- a %>% filter(Alimentacao==1 & Transporte==1)
pme_solicitados <- a %>% filter(Moradia==1)


### MODELAAAAAAAAAAAAAA ###################################################################

set.seed(1)

bas_solicitados= bas_solicitados %>% arrange(Contemplacao_BAS)

indices_treino_bas_0 <- sample(c(1:sum(bas_solicitados$Contemplacao_BAS==0)), round(0.7*nrow(bas_solicitados)/2))
indices_treino_bas_1 <- sample(c(sum(bas_solicitados$Contemplacao_BAS==0)+1:nrow(bas_solicitados)), round(0.7*nrow(bas_solicitados)/2))


amostra_treino_bas <- bas_solicitados[c(indices_treino_bas_0,indices_treino_bas_1),]
amostra_teste_bas <-  bas_solicitados[-c(indices_treino_bas_0,indices_treino_bas_1),]


glm_bas <- step(glm(formula = Contemplacao_BAS ~ ano + GF +Contemplacao_Alimentacao+Contemplacao_Moradia+ DG + EP + MT + sm_categorizado,
                    data = amostra_treino_bas, family = binomial()))

summary(glm_bas)

predict_bas <- predict.glm(glm_bas, amostra_teste_bas, type= 'response')

predict_bas1 <- ifelse(predict_bas>0.5, 1, 0)
table(predict_bas1, amostra_teste_bas$Contemplacao_BAS)
prop.table(table(predict_bas1, amostra_teste_bas$Contemplacao_BAS))
prop.table(table(predict_bas1, amostra_teste_bas$Contemplacao_BAS),2)

plot(glm_bas)
#BAS
exp(+1.24205) # RC EP  3.462705
exp(+1.02434) #RC DG 2.785257
exp(1.204) #RC    MORADIA 3.333424
exp(0.67162) # RC 2012 1.957406
exp(3.08224) #RC n1xn2 21.8072

#bat


set.seed(1)

bat_solicitados= bat_solicitados %>% arrange(Contemplacao_Alimentacao)

indices_treino_bat_0 <- sample(c(1:sum(bat_solicitados$Contemplacao_Alimentacao==0)), round(0.7*nrow(bat_solicitados)/2))
indices_treino_bat_1 <- sample(c(sum(bat_solicitados$Contemplacao_Alimentacao==0)+1:nrow(bat_solicitados)), round(0.7*nrow(bat_solicitados)/2))


amostra_treino_bat <- bat_solicitados[c(indices_treino_bat_0,indices_treino_bat_1),]
amostra_teste_bat <-  bat_solicitados[-c(indices_treino_bat_0,indices_treino_bat_1),]


glm_bat <- step(glm(formula = Contemplacao_Alimentacao ~ ano+ Contemplacao_Moradia + GF + DG + EP + MT + sm_categorizado,
                    data = amostra_treino_bat, family = binomial()))

summary(glm_bat)

predict_bat <- predict.glm(glm_bat, amostra_teste_bat, type= 'response')

predict_bat1 <- ifelse(predict_bat>0.5, 1, 0)
table(predict_bat1, amostra_teste_bat$Contemplacao_Alimentacao)
prop.table(table(predict_bat1, amostra_teste_bat$Contemplacao_Alimentacao))
prop.table(table(predict_bat1, amostra_teste_bat$Contemplacao_Alimentacao),2)


exp(+0.4692) # RC EP  1.598715
exp(+0.8885) #RC DG 2.43148
exp(1.3785) #RC    MORADIA 3.968944
exp(0.1939) # RC 2012 1.213975
exp(+1.3066) # RC N1xN2 3.693594

#moradia


set.seed(1)

pme_solicitados= pme_solicitados %>% arrange(Contemplacao_Moradia)

indices_treino_pme_0 <- sample(c(1:sum(pme_solicitados$Contemplacao_Moradia==0)), round(0.4*nrow(pme_solicitados)/2))
indices_treino_pme_1 <- sample(c(sum(pme_solicitados$Contemplacao_Moradia==0)+1:nrow(pme_solicitados)), round(0.4*nrow(pme_solicitados)/2))


amostra_treino_pme <- pme_solicitados[c(indices_treino_pme_0,indices_treino_pme_1),]
amostra_teste_pme <-  pme_solicitados[-c(indices_treino_pme_0,indices_treino_pme_1),]


glm_pme <- step(glm(formula = Contemplacao_Moradia ~ ano +Contemplacao_BAS+ Contemplacao_Alimentacao + GF + DG + EP  + sm_categorizado,
                    data = amostra_treino_pme, family = binomial()))

summary(glm_pme)

predict_pme <- predict.glm(glm_pme, amostra_teste_pme, type= 'response')

predict_pme1 <- ifelse(predict_pme>0.5, 1, 0)
table(predict_pme1, amostra_teste_pme$Contemplacao_Moradia)
prop.table(table(predict_pme1, amostra_teste_pme$Contemplacao_Moradia))
prop.table(table(predict_pme1, amostra_teste_pme$Contemplacao_Moradia),2)

exp(+0.20853) # RC EP  1.231866
exp(+0.84950) #RC DG 2.338477
exp(1.53640) #RC    BAS 4.647828
exp(1.64599) # RC 2012 5.186142
exp(+0.16304) # RC N1xN2 1.177084


a=a%>%
  arrange(`Ano do processo seletivo`)%>%
  mutate(n_contemplou=Contemplacao_Alimentacao+Contemplacao_Transporte+Contemplacao_Moradia+Contemplacao_BAS)%>%
  mutate(contemplou_alguma=if_else(n_contemplou==0,0,1))%>%
  group_by(ID,contemplou_alguma)%>%
  mutate(ja_teve_bolsa=if_else(row_number()==1 & contemplou_alguma==1,0,1))


aux=data.frame(Bolsa=c("BAS","BAS","BAS","BAS","BAT","BAT","BAT","BAT","PME","PME","PME","PME"),RC=c("EP","DG","2006x2012","Renda: N1xN2","EP","DG","2006x2012","Renda: N1xN2","EP","DG","2006x2012","Renda: N1xN2"),Valor=c(bas,bat,pme))

ggplot(aux,aes(x=RC,y=Valor,fill=as.factor(Bolsa)))+
  geom_bar(stat="identity",position="dodge")+
  theme_light()+
  ylab("Razao de Chances")+
  xlab("Comparacoes")+
  scale_fill_discrete(name="Bolsa")
ggsave("razao_chance.png")



