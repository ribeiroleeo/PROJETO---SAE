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
install.packages("MASS")
library(reshape2)
library(sqldf)
library(doBy)
library("readxl")
library("ggplot2")
library("dplyr")
library("stringr")
library("AggregateR")
library("dummy")
library("MASS")

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


##CONVERTENDO FORMATOS
dados$RT=as.numeric(dados$RT)
dados$GF=as.numeric(dados$GF)
dados$DG=as.factor(dados$DG)
dados$MT=as.numeric(dados$MT)
dados$TR=as.numeric(dados$TR)
dados$EP=as.factor(dados$EP)

dados$`BAS (contemplada)`=as.factor(dados$`BAS (contemplada)`)


##Arrumando o PAAIS

paais_novo1= str_detect(dados$PAAIS,": SIM")
paais_novo2=dados$PAAIS=='SIM'
dados$paais_novo=paais_novo1+paais_novo2


###Ajustando modelo
bas<-subset(dados, dados$`BAS (solicitada)`==1)
alimentacao<-subset(dados, dados$`Alimentação (solicitada)` ==1)
trasnporte<-subset(dados, dados$`Transporte (solicitada)` ==1)
moradia<-subset(dados, dados$`Moradia (solicitada)` ==1)


bas<-dados%>%filter(dados$`BAS (solicitada)`==1)%>%
  group_by(ID,`Ano do processo seletivo`)%>%filter(row_number()==1)

alimentacao<-dados%>%filter(dados$`Alimentação (solicitada)`==1)%>%
  group_by(ID,`Ano do processo seletivo`)%>%filter(row_number()==1)

transporte<-dados%>%filter(dados$`Transporte (solicitada)`==1)%>%
  group_by(ID,`Ano do processo seletivo`)%>%filter(row_number()==1)

moradia<-dados%>%filter(dados$`Moradia (solicitada)`==1)%>%
  group_by(ID,`Ano do processo seletivo`)%>%filter(row_number()==1)



#model
library(MASS)
# Fit the full model 
full.model <- lm(Fertility ~., data = swiss)
# Stepwise regression model
step.model <- stepAIC(m_bas, direction = "both", 
                      trace = FALSE)
summary(step.model)


bas=bas%>%mutate(RP=RT/GF)
m_bas<-polr(`BAS (contemplada)` ~ RP+EP+as.factor(paais_novo)+Nível+as.factor(bas$`Moradia  (contemplada)`)+IC  ,data=bas, Hess = TRUE)
summary(m_bas)

floresta=randomForest(`BAS (contemplada)` ~ RP+EP+paais_novo+MT+bas$`Moradia  (contemplada)`  ,data=bas,is.na=FALSE)
varImpPlot(floresta)



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

dados$numero_curso_sol <- sub("^\\D*(\\d+).*$", "\\1", dados$Curso)
dados$nivel_curso_sol<- regmatches(dados$Curso, gregexpr("(?=\\().*?(?<=\\))", dados$Curso, perl=T))