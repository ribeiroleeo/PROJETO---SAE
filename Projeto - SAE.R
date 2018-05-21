

###PACOTES
library("readxl")
library("ggplot2")
library("dplyr")
library("stringr")
library("AggregateR")
library("dummy")

#LENDO OS DADOS

dados=read_excel("Base de dados_Processo Seletivo_com_ID.xlsx",sheet=1)



###ARRUMANDO O BANCO

names(dados)=dados[1,]#nomes estavam na primeira linha
dados=dados[-1,]

str(dados)

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


##CONVERTENDO FORMATOS
id=as.numeric(dados$ID)
ano_processo=dados$`Ano do processo seletivo`
bolsa=as.factor(dados$Bolsa)
vigencia_inicio=as.Date.character(dados$`Vigência Início`,"%Y-%m-%d")
vigencia_final=as.Date.character(dados$`Vigência Fim`,"%Y-%m-%d")
curso=as.factor(dados$Curso)
nivel=as.factor(dados$Nível)
ic_momento_contemp=as.numeric(dados$`IC no momento da contemplação`)
data_desvinc=as.Date.character(dados$`Desvinculação da bolsa`,"%Y-%m-%d")
ic_insc=as.numeric(dados$IC)
renda_total=as.numeric(dados$RT)
grupo_familiar=as.numeric(dados$GF)
doenca_grave=as.factor(dados$DG)
gastos_moradia_indice=as.numeric(dados$MT)
gastos_transp_inidce=as.numeric(dados$TR)
escola_publica=as.factor(dados$EP)
bolsa_aux_social_solicitada=as.factor(dados$`BAS (solicitada)`)
alimentacao_solicitada=as.factor(dados$`Alimentação (solicitada)`)
transp_solicitada=as.factor(dados$`Transporte (solicitada)`)
moradia_solictada=as.factor(dados$`Moradia (solicitada)`)
bolsa_aux_social_contemp=as.factor(dados$`BAS (contemplada)`)
alimetacao_contemp=as.factor(dados$`Alimentação (contemplada)`)
transp_contemp=as.factor(dados$`Transporte  (contemplada)`)
moradia_contemp=as.factor(dados$`Moradia  (contemplada)`)
paais=as.factor(dados$PAAIS)
divergencia=as.factor(dados$`Divergência?`)
obs=(dados$Observação)
bolsa_aux=as.factor(dados$bolsa_aux)
moradia=as.factor(dados$moradia)


dados1=data.frame(id,ano_processo,bolsa,vigencia_inicio,vigencia_final,curso,nivel,ic_momento_contemp,
                  data_desvinc,ic_insc,renda_total,grupo_familiar,doenca_grave,gastos_moradia_indice,
                  gastos_transp_inidce,escola_publica,bolsa_aux_social_solicitada,alimentacao_solicitada,
                  transp_solicitada,moradia_solictada,bolsa_aux_social_contemp,alimetacao_contemp,
                  transp_contemp,moradia_contemp,paais,divergencia,obs,bolsa_aux,moradia)





#AGRUPANDO AS PESSOAS PARA CONSIDERAR A HISTORIA DE CADA PESSOA


dados_agrupar=dados1[,c(3,8,11:26)]
agrupado=Aggregate(x=dados_agrupar,by=dados1$id,object=categories(dados_agrupar))



