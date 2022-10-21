## Definir área de trabalho
setwd("C:/Users/guiks/OneDrive - Unesp/UNESP/Graduação/Iniciação Científica/2021")

## Biblioteca para ler arquivos em excel
library(readxl)

## Importando a(s) base(s) de dados

zero_sem_rev = read_excel("Medição_0mm_semrev.xlsx")
zero_com_rev = read_excel("Medição_0mm_comrev.xlsx")
cem_sem_rev = read_excel("Medição_100mm_semrev.xlsx")
cem_com_rev = read_excel("Medição_100mm_comrev.xlsx")
duzentos_sem_rev = read_excel("Medição_200mm_semrev.xlsx")
duzentos_com_rev = read_excel("Medição_200mm_comrev.xlsx")
trezentos_sem_rev = read_excel("Medição_300mm_semrev.xlsx")
trezentos_com_rev = read_excel("Medição_300mm_comrev.xlsx")

## Estatísticas Descritivas com os Dados
media_e_desv <- function(config_medicao){
  ### Calculando a média entre as cinco medições para cada ponto
  ### Calculando a média - ida
  config_medicao[6] <- rowMeans(config_medicao[ ,2:5]) * 1000
  
  ### Calculando o desvio padrão
  i=1
  desv <- vector()
  while (i<=36) {
    desv[i] <- sd(config_medicao[i, 2:5]) * 1000
    i <- i+1
  }
  config_medicao[7] <- desv
  return(config_medicao)
}

zero_sem_rev <- media_e_desv(zero_sem_rev)
zero_com_rev <- media_e_desv(zero_com_rev)
cem_sem_rev <- media_e_desv(cem_sem_rev)
cem_com_rev <- media_e_desv(cem_com_rev)
duzentos_sem_rev <- media_e_desv(duzentos_sem_rev)
duzentos_com_rev <- media_e_desv(duzentos_com_rev)
trezentos_sem_rev <- media_e_desv(duzentos_sem_rev)
trezentos_com_rev <- media_e_desv(trezentos_com_rev)

# TRATAMENTO DE DADOS
## Separando as medições de ida e volta
### IDA
separa_ida <- function(config_medicao){
  ### Medições de ida
  config_medicao_ida <- config_medicao[1:18, ]
  colnames(config_medicao_ida)[6] <- "Média (um)"           ### Mudando o nome da coluna das médias
  colnames(config_medicao_ida)[7] <- "Desvio Padrão (um)"    ### Mudando o nome da coluna de desvio padrão
  return(config_medicao_ida)
}

zero_sem_rev_ida <- separa_ida(zero_sem_rev)
zero_com_rev_ida <- separa_ida(zero_com_rev)
cem_sem_rev_ida <- separa_ida(cem_sem_rev)
cem_com_rev_ida <- separa_ida(cem_com_rev)
duzentos_sem_rev_ida <- separa_ida(duzentos_sem_rev)
duzentos_com_rev_ida <- separa_ida(duzentos_com_rev)
trezentos_sem_rev_ida <- separa_ida(trezentos_sem_rev)
trezentos_com_rev_ida <- separa_ida(trezentos_com_rev)

### VOLTA
separa_volta <- function(config_medicao){
  ### Medições de volta
  config_medicao_volta <- config_medicao[19:36, ]
  colnames(config_medicao_volta)[6] <- "Média (um)"           ### Mudando o nome da coluna das médias
  colnames(config_medicao_volta)[7] <- "Desvio Padrão (um)"    ### Mudando o nome da coluna de desvio padrão
  #zero_sem_rev_volta
  ### Ordenando os dados em ordem crescente pela posição
  library(plyr)
  config_medicao_volta <- arrange(config_medicao_volta,config_medicao_volta$`Posição (mm)`)
  return(config_medicao_volta)
}

zero_sem_rev_volta <- separa_volta(zero_sem_rev)
zero_com_rev_volta <- separa_volta(zero_com_rev)
cem_sem_rev_volta <- separa_volta(cem_sem_rev)
cem_com_rev_volta <- separa_volta(cem_com_rev)
duzentos_sem_rev_volta <- separa_volta(duzentos_sem_rev)
duzentos_com_rev_volta <- separa_volta(duzentos_com_rev)
trezentos_sem_rev_volta <- separa_volta(trezentos_sem_rev)
trezentos_com_rev_volta <- separa_volta(trezentos_com_rev)

# Os dados medidos experimentalmente são grosseiros, o erro medido é composto pela junção do erro de retitude do perfil
# do artefato, com o erro de deslocamento do carro do torno mecânico e o erro de inclinação devido à configuração
# dos equipamentos no sistema de medição

# Primeiramente, é necessário corrigir o erro de desalinhamento experimental. Para isso, utilizamos
# o método dos mínimos quadrados

minimos_quadrados = function(config_medicao){
  # Passando os valores para micrometros e mudando o nome das colunas para a unidade de micrometro
  config_medicao[ ,2:5] <- config_medicao[ ,2:5] * 1000
  colnames(config_medicao)[2] <- "Med1 (um)"
  colnames(config_medicao)[3] <- "Med2 (um)"
  colnames(config_medicao)[4] <- "Med3 (um)"
  colnames(config_medicao)[5] <- "Med4 (um)"
  
  # Aplicando Mínimos Quadrados para os dados da Med1
  reta_ajuste_1 <- lm(config_medicao$`Med1 (um)`~config_medicao$`Posição (mm)`)
  coeficientes_1 <- coef(reta_ajuste_1)
  ### Nova coluna com os pontos obtidos pela reta de ajuste (yi)
  config_medicao[8] <- config_medicao$`Posição (mm)`*coeficientes_1[2]+coeficientes_1[1]
  colnames(config_medicao)[8] <- "Reta Ajuste 1 (yi)"
  ### Nova coluna com os valores subtraídos alinhados pela reta de ajuste
  config_medicao[9] <- config_medicao$`Med1 (um)` - config_medicao$`Reta Ajuste 1 (yi)`
  colnames(config_medicao)[9] <- "yi'-yi (Med1)"
  ### Zerando o ponto inicial (referência)
  config_medicao[10] <- config_medicao$`yi'-yi (Med1)` - config_medicao$`yi'-yi (Med1)`[1]
  colnames(config_medicao)[10] <- "Resultado Final 1 (um)"
  
  # Aplicando Mínimos Quadrados para os dados da Med2
  reta_ajuste_2 <- lm(config_medicao$`Med2 (um)`~config_medicao$`Posição (mm)`)
  coeficientes_2 <- coef(reta_ajuste_2)
  ### Nova coluna com os pontos obtidos pela reta de ajuste (yi)
  config_medicao[11] <- config_medicao$`Posição (mm)`*coeficientes_2[2]+coeficientes_2[1]
  colnames(config_medicao)[11] <- "Reta Ajuste 2 (yi)"
  ### Nova coluna com os valores subtraídos alinhados pela reta de ajuste
  config_medicao[12] <- config_medicao$`Med2 (um)` - config_medicao$`Reta Ajuste 2 (yi)`
  colnames(config_medicao)[12] <- "yi'-yi (Med2)"
  ### Zerando o ponto inicial (referência)
  config_medicao[13] <- config_medicao$`yi'-yi (Med2)` - config_medicao$`yi'-yi (Med2)`[1]
  colnames(config_medicao)[13] <- "Resultado Final 2 (um)"
  
  # Aplicando Mínimos Quadrados para os dados da Med3
  reta_ajuste_3 <- lm(config_medicao$`Med3 (um)`~config_medicao$`Posição (mm)`)
  coeficientes_3 <- coef(reta_ajuste_3)
  ### Nova coluna com os pontos obtidos pela reta de ajuste (yi)
  config_medicao[14] <- config_medicao$`Posição (mm)`*coeficientes_3[2]+coeficientes_3[1]
  colnames(config_medicao)[14] <- "Reta Ajuste 3 (yi)"
  ### Nova coluna com os valores subtraídos alinhados pela reta de ajuste
  config_medicao[15] <- config_medicao$`Med3 (um)` - config_medicao$`Reta Ajuste 3 (yi)`
  colnames(config_medicao)[15] <- "yi'-yi (Med3)"
  ### Zerando o ponto inicial (referência)
  config_medicao[16] <- config_medicao$`yi'-yi (Med3)` - config_medicao$`yi'-yi (Med3)`[1]
  colnames(config_medicao)[16] <- "Resultado Final 3 (um)"
  
  # Aplicando Mínimos Quadrados para os dados da Med4
  reta_ajuste_4 <- lm(config_medicao$`Med4 (um)`~config_medicao$`Posição (mm)`)
  coeficientes_4 <- coef(reta_ajuste_4)
  ### Nova coluna com os pontos obtidos pela reta de ajuste (yi)
  config_medicao[17] <- config_medicao$`Posição (mm)`*coeficientes_4[2]+coeficientes_4[1]
  colnames(config_medicao)[17] <- "Reta Ajuste 4 (yi)"
  ### Nova coluna com os valores subtraídos alinhados pela reta de ajuste
  config_medicao[18] <- config_medicao$`Med4 (um)` - config_medicao$`Reta Ajuste 4 (yi)`
  colnames(config_medicao)[18] <- "yi'-yi (Med4)"
  ### Zerando o ponto inicial (referência)
  config_medicao[19] <- config_medicao$`yi'-yi (Med4)` - config_medicao$`yi'-yi (Med4)`[1]
  colnames(config_medicao)[19] <- "Resultado Final 4 (um)"
  
  ### Reta de ajuste média (yi)
  x <- as.data.frame(config_medicao$`Reta Ajuste 1 (yi)`)
  x[2] <- config_medicao$`Reta Ajuste 2 (yi)`
  x[3] <- config_medicao$`Reta Ajuste 3 (yi)`
  x[4] <- config_medicao$`Reta Ajuste 4 (yi)`
  config_medicao[20] <- rowMeans(x)
  colnames(config_medicao)[20] <- "Reta Ajuste Media (yi)"
  
  ### yi'-yi Medio
  y <- as.data.frame(config_medicao$`yi'-yi (Med1)`)
  y[2] <- config_medicao$`yi'-yi (Med2)`
  y[3] <- config_medicao$`yi'-yi (Med3)`
  y[4] <- config_medicao$`yi'-yi (Med4)`
  config_medicao[21] <- rowMeans(y)
  colnames(config_medicao)[21] <- "yi'-yi (Medio)"
  
  ### Resultado Final Médio
  z <- as.data.frame(config_medicao$`Resultado Final 1 (um)`)
  z[2] <- config_medicao$`Resultado Final 2 (um)`
  z[3] <- config_medicao$`Resultado Final 3 (um)`
  z[4] <- config_medicao$`Resultado Final 4 (um)`
  config_medicao[22] <- rowMeans(z)
  colnames(config_medicao)[22] <- "Resultado Final Medio (um)"
  
  return(config_medicao)
}

zero_sem_rev_ida <- minimos_quadrados(zero_sem_rev_ida)
zero_sem_rev_volta <- minimos_quadrados(zero_sem_rev_volta)
zero_com_rev_ida <- minimos_quadrados(zero_com_rev_ida)
zero_com_rev_volta <- minimos_quadrados(zero_com_rev_volta)
cem_sem_rev_ida <- minimos_quadrados(cem_sem_rev_ida)
cem_sem_rev_volta <- minimos_quadrados(cem_sem_rev_volta)
cem_com_rev_ida <- minimos_quadrados(cem_com_rev_ida)
cem_com_rev_volta <- minimos_quadrados(cem_com_rev_volta)
duzentos_sem_rev_ida <- minimos_quadrados(duzentos_sem_rev_ida)
duzentos_sem_rev_volta <- minimos_quadrados(duzentos_sem_rev_volta)
duzentos_com_rev_ida <- minimos_quadrados(duzentos_com_rev_ida)
duzentos_com_rev_volta <- minimos_quadrados(duzentos_com_rev_volta)
trezentos_sem_rev_ida <- minimos_quadrados(trezentos_sem_rev_ida)
trezentos_sem_rev_volta <- minimos_quadrados(trezentos_sem_rev_volta)
trezentos_com_rev_ida <- minimos_quadrados(trezentos_com_rev_ida)
trezentos_com_rev_volta <- minimos_quadrados(trezentos_com_rev_volta)

# GRÁFICOS MÍNIMOS QUADRADOS - PERFIL E RETA DE AJUSTE

par(mfrow=c(2,2))

### Perfil e reta de ajuste - ida (sem reversão)
### 0 mm
plot(zero_sem_rev_ida$`Posição (mm)`,zero_sem_rev_ida$`Média (um)`,type="o",xaxp=c(0,680,17),
     pch=20,main = "Valores Experimentais e Reta de Ajuste (0 mm)",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)",col="blue",xlim = c(0,680),ylim = c(-800,300))
grid(nx = NULL, ny = NULL,lty = 3,lwd = 1,col = "gray")
### Plotando a reta de ajuste
lines(x=zero_sem_rev_ida$`Posição (mm)`,y=zero_sem_rev_ida$`Reta Ajuste Media (yi)`,col="blue",lty="dashed",lwd=1)
### Perfil e reta de ajuste - volta
lines(x=zero_sem_rev_volta$`Posição (mm)`,y=zero_sem_rev_volta$`Média (um)`,type = "o",col="orange",pch=20)
lines(x=zero_sem_rev_volta$`Posição (mm)`,y=zero_sem_rev_volta$`Reta Ajuste Media (yi)`,col="orange",lty="dashed",lwd=1)
### Perfil e reta de ajuste - ida (com reversão)
lines(x=zero_com_rev_ida$`Posição (mm)`,y=zero_com_rev_ida$`Média (um)`,type="o",col="red",pch=20)
lines(x=zero_com_rev_ida$`Posição (mm)`,y=zero_com_rev_ida$`Reta Ajuste Media (yi)`,col="red",lty="dashed",lwd=1)
lines(x=zero_com_rev_volta$`Posição (mm)`,y=zero_com_rev_volta$`Média (um)`,type = "o",col="green",pch=20)
lines(x=zero_com_rev_volta$`Posição (mm)`,y=zero_com_rev_volta$`Reta Ajuste Media (yi)`,col="green",lty="dashed",lwd=1)
legend(x = "bottomleft",
       legend = c("yi'-ida","yi-ida","yi'-volta",
                  "yi-volta","yi'-ida-rev","yi-ida-rev","yi'-volta-rev","yi-volta-rev"),
       col=c("blue","blue","orange","orange","red","red","green","green"),
       lty = c(1,2),lwd = 1,bty = "o",pch=c(20,NA),cex=0.6,ncol=2,text.font = 4)

### 100 mm
plot(cem_sem_rev_ida$`Posição (mm)`,cem_sem_rev_ida$`Média (um)`,type="o",xaxp=c(0,680,17),
     main = "Valores Experimentais e Reta de Ajuste (100 mm)",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)",col="blue",xlim = c(0,680),ylim = c(-800,300))
grid(nx = NULL, ny = NULL,lty = 3,lwd = 1,col = "gray")
### Plotando a reta de ajuste
lines(x=cem_sem_rev_ida$`Posição (mm)`,y=cem_sem_rev_ida$`Reta Ajuste Media (yi)`,col="blue",lty="dashed",lwd=1)
### Perfil e reta de ajuste - volta
lines(x=cem_sem_rev_volta$`Posição (mm)`,y=cem_sem_rev_volta$`Média (um)`,type = "o",col="orange")
lines(x=cem_sem_rev_volta$`Posição (mm)`,y=cem_sem_rev_volta$`Reta Ajuste Media (yi)`,col="orange",lty="dashed",lwd=1)
### Perfil e reta de ajuste - ida (com reversão)
lines(x=cem_com_rev_ida$`Posição (mm)`,y=cem_com_rev_ida$`Média (um)`,type="o",col="red")
lines(x=cem_com_rev_ida$`Posição (mm)`,y=cem_com_rev_ida$`Reta Ajuste Media (yi)`,col="red",lty="dashed",lwd=1)
lines(x=cem_com_rev_volta$`Posição (mm)`,y=cem_com_rev_volta$`Média (um)`,type = "o",col="green")
lines(x=cem_com_rev_volta$`Posição (mm)`,y=cem_com_rev_volta$`Reta Ajuste Media (yi)`,col="green",lty="dashed",lwd=1)
legend(x = "bottomleft",
       legend = c("yi'-ida","yi-ida","yi'-volta",
                  "yi-volta","yi'-ida-rev","yi-ida-rev","yi'-volta-rev","yi-volta-rev"),
       col=c("blue","blue","orange","orange","red","red","green","green"),
       lty = c(1,2),lwd = 1,bty = "o",pch=c(20,NA),cex=0.6,ncol=2,text.font = 4)

### 200 mm
plot(duzentos_sem_rev_ida$`Posição (mm)`,duzentos_sem_rev_ida$`Média (um)`,type="o",xaxp=c(0,680,17),
     main = "Valores Experimentais e Reta de Ajuste (200 mm)",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)",col="blue",xlim = c(0,680),ylim = c(-800,300))
grid(nx = NULL, ny = NULL,lty = 3,lwd = 1,col = "gray")
### Plotando a reta de ajuste
lines(x=duzentos_sem_rev_ida$`Posição (mm)`,y=duzentos_sem_rev_ida$`Reta Ajuste Media (yi)`,col="blue",lty="dashed",lwd=1)
### Perfil e reta de ajuste - volta
lines(x=duzentos_sem_rev_volta$`Posição (mm)`,y=duzentos_sem_rev_volta$`Média (um)`,type = "o",col="orange")
lines(x=duzentos_sem_rev_volta$`Posição (mm)`,y=duzentos_sem_rev_volta$`Reta Ajuste Media (yi)`,col="orange",lty="dashed",lwd=1)
### Perfil e reta de ajuste - ida (com reversão)
lines(x=duzentos_com_rev_ida$`Posição (mm)`,y=duzentos_com_rev_ida$`Média (um)`,type="o",col="red")
lines(x=duzentos_com_rev_ida$`Posição (mm)`,y=duzentos_com_rev_ida$`Reta Ajuste Media (yi)`,col="red",lty="dashed",lwd=1)
lines(x=duzentos_com_rev_volta$`Posição (mm)`,y=duzentos_com_rev_volta$`Média (um)`,type = "o",col="green")
lines(x=duzentos_com_rev_volta$`Posição (mm)`,y=duzentos_com_rev_volta$`Reta Ajuste Media (yi)`,col="green",lty="dashed",lwd=1)
legend(x = "bottomleft",
       legend = c("yi'-ida","yi-ida","yi'-volta",
                  "yi-volta","yi'-ida-rev","yi-ida-rev","yi'-volta-rev","yi-volta-rev"),
       col=c("blue","blue","orange","orange","red","red","green","green"),
       lty = c(1,2),lwd = 1,bty = "o",pch=c(20,NA),cex=0.6,ncol=2,text.font = 4)

### 300 mm
plot(trezentos_sem_rev_ida$`Posição (mm)`,trezentos_sem_rev_ida$`Média (um)`,type="o",xaxp=c(0,680,17),
     main = "Valores Experimentais e Reta de Ajuste (300 mm)",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)",col="blue",xlim = c(0,680),ylim = c(-800,300))
grid(nx = NULL, ny = NULL,lty = 3,lwd = 1,col = "gray")
### Plotando a reta de ajuste
lines(x=trezentos_sem_rev_ida$`Posição (mm)`,y=trezentos_sem_rev_ida$`Reta Ajuste Media (yi)`,col="blue",lty="dashed",lwd=1)
### Perfil e reta de ajuste - volta
lines(x=trezentos_sem_rev_volta$`Posição (mm)`,y=trezentos_sem_rev_volta$`Média (um)`,type = "o",col="orange")
lines(x=trezentos_sem_rev_volta$`Posição (mm)`,y=trezentos_sem_rev_volta$`Reta Ajuste Media (yi)`,col="orange",lty="dashed",lwd=1)
### Perfil e reta de ajuste - ida (com reversão)
lines(x=trezentos_com_rev_ida$`Posição (mm)`,y=trezentos_com_rev_ida$`Média (um)`,type="o",col="red")
lines(x=trezentos_com_rev_ida$`Posição (mm)`,y=trezentos_com_rev_ida$`Reta Ajuste Media (yi)`,col="red",lty="dashed",lwd=1)
lines(x=trezentos_com_rev_volta$`Posição (mm)`,y=trezentos_com_rev_volta$`Média (um)`,type = "o",col="green")
lines(x=trezentos_com_rev_volta$`Posição (mm)`,y=trezentos_com_rev_volta$`Reta Ajuste Media (yi)`,col="green",lty="dashed",lwd=1)
legend(x = "bottomleft",
       legend = c("yi'-ida","yi-ida","yi'-volta",
                  "yi-volta","yi'-ida-rev","yi-ida-rev","yi'-volta-rev","yi-volta-rev"),
       col=c("blue","blue","orange","orange","red","red","green","green"),
       lty = c(1,2),lwd = 1,bty = "o",pch=c(20,NA),cex=0.6,ncol=2,text.font = 4)

### Gráfido com desalinhamento corrigido pelos mínimos quadrados
par(mfrow=c(2,2))

### 0 mm
### Ida - sem rev
plot(zero_sem_rev_ida$`Posição (mm)`,zero_sem_rev_ida$`Resultado Final Medio (um)`,type="o",xaxp=c(0,680,17),
     main = "Desvio Corrigido pelos Mínimos Quadrados (0 mm)",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)",col="blue",xlim = c(0,680),ylim = c(-160,20),pch=20)
grid(nx = NULL, ny = NULL,lty = 3,lwd = 1,col = "gray")
### Volta - sem rev
lines(x=zero_sem_rev_volta$`Posição (mm)`,y=zero_sem_rev_volta$`Resultado Final Medio (um)`,type = "o",col="orange",pch=20)
### Ida - com rev
lines(x=zero_com_rev_ida$`Posição (mm)`,y=zero_com_rev_ida$`Resultado Final Medio (um)`,type="o",col="red",pch=20)
### Volta - com rev
lines(x=zero_com_rev_volta$`Posição (mm)`,y=zero_com_rev_volta$`Resultado Final Medio (um)`,type = "o",col="green",pch=20)
legend(x = "top",inset =  .02,
       legend = c("Desvio Corrigido-ida","Desvio Corrigido-volta",
                  "Desvio Corrigido-ida-rev","Desvio Corrigido-volta-rev"),
       col=c("blue","orange","red","green"),
       lty = 1,lwd = 1,bty = "o",pch=20,cex=0.6,ncol=1,text.font = 4)

### 100 mm
### Ida - sem rev
plot(cem_sem_rev_ida$`Posição (mm)`,cem_sem_rev_ida$`Resultado Final Medio (um)`,type="o",xaxp=c(0,680,17),
     main = "Desvio Corrigido pelos Mínimos Quadrados (100 mm)",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)",col="blue",xlim = c(0,680),ylim = c(-160,20),pch=20)
grid(nx = NULL, ny = NULL,lty = 3,lwd = 1,col = "gray")
### Volta - sem rev
lines(x=cem_sem_rev_volta$`Posição (mm)`,y=cem_sem_rev_volta$`Resultado Final Medio (um)`,type = "o",col="orange",pch=20)
### Ida - com rev
lines(x=cem_com_rev_ida$`Posição (mm)`,y=cem_com_rev_ida$`Resultado Final Medio (um)`,type="o",col="red",pch=20)
### Volta - com rev
lines(x=cem_com_rev_volta$`Posição (mm)`,y=cem_com_rev_volta$`Resultado Final Medio (um)`,type = "o",col="green",pch=20)
legend(x = "top",inset =  .02,
       legend = c("Desvio Corrigido-ida","Desvio Corrigido-volta",
                  "Desvio Corrigido-ida-rev","Desvio Corrigido-volta-rev"),
       col=c("blue","orange","red","green"),
       lty = 1,lwd = 1,bty = "o",pch=20,cex=0.6,ncol=1,text.font = 4)

### 200 mm
### Ida - sem rev
plot(duzentos_sem_rev_ida$`Posição (mm)`,duzentos_sem_rev_ida$`Resultado Final Medio (um)`,type="o",xaxp=c(0,680,17),
     main = "Desvio Corrigido pelos Mínimos Quadrados (200 mm)",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)",col="blue",xlim = c(0,680),ylim = c(-160,20),pch=20)
grid(nx = NULL, ny = NULL,lty = 3,lwd = 1,col = "gray")
### Volta - sem rev
lines(x=duzentos_sem_rev_volta$`Posição (mm)`,y=duzentos_sem_rev_volta$`Resultado Final Medio (um)`,type = "o",col="orange",pch=20)
### Ida - com rev
lines(x=duzentos_com_rev_ida$`Posição (mm)`,y=duzentos_com_rev_ida$`Resultado Final Medio (um)`,type="o",col="red",pch=20)
### Volta - com rev
lines(x=duzentos_com_rev_volta$`Posição (mm)`,y=duzentos_com_rev_volta$`Resultado Final Medio (um)`,type = "o",col="green",pch=20)
legend(x = "top",inset =  .02,
       legend = c("Desvio Corrigido-ida","Desvio Corrigido-volta",
                  "Desvio Corrigido-ida-rev","Desvio Corrigido-volta-rev"),
       col=c("blue","orange","red","green"),
       lty = 1,lwd = 1,bty = "o",pch=20,cex=0.6,ncol=1,text.font = 4)

### 300 mm
### Ida - sem rev
plot(trezentos_sem_rev_ida$`Posição (mm)`,trezentos_sem_rev_ida$`Resultado Final Medio (um)`,type="o",xaxp=c(0,680,17),
     main = "Desvio Corrigido pelos Mínimos Quadrados (300 mm)",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)",col="blue",xlim = c(0,680),ylim = c(-160,20),pch=20)
grid(nx = NULL, ny = NULL,lty = 3,lwd = 1,col = "gray")
### Volta - sem rev
lines(x=trezentos_sem_rev_volta$`Posição (mm)`,y=trezentos_sem_rev_volta$`Resultado Final Medio (um)`,type = "o",col="orange",pch=20)
### Ida - com rev
lines(x=trezentos_com_rev_ida$`Posição (mm)`,y=trezentos_com_rev_ida$`Resultado Final Medio (um)`,type="o",col="red",pch=20)
### Volta - com rev
lines(x=trezentos_com_rev_volta$`Posição (mm)`,y=trezentos_com_rev_volta$`Resultado Final Medio (um)`,type = "o",col="green",pch=20)
legend(x = "top",inset =  .02,
       legend = c("Desvio Corrigido-ida","Desvio Corrigido-volta",
                  "Desvio Corrigido-ida-rev","Desvio Corrigido-volta-rev"),
       col=c("blue","orange","red","green"),
       lty = 1,lwd = 1,bty = "o",pch=20,cex=0.6,ncol=1,text.font = 4)

# -------------------------------------------------------------------------------------------------------------------------
### Preparando data frame para aplicar a reversão

reversao = function(config_reversao,config_sem_rev,config_com_rev){
  ###Med1
  config_reversao[2] <- config_sem_rev$`Resultado Final 1 (um)`
  config_reversao[3] <- config_com_rev$`Resultado Final 1 (um)`
  config_reversao[4] <- (config_reversao[2]-config_reversao[3])/2
  config_reversao[5] <- (config_reversao[2]+config_reversao[3])/2
  ###Med2
  config_reversao[6] <- config_sem_rev$`Resultado Final 2 (um)`
  config_reversao[7] <- config_com_rev$`Resultado Final 2 (um)`
  config_reversao[8] <- (config_reversao[6]-config_reversao[7])/2
  config_reversao[9] <- (config_reversao[6]+config_reversao[7])/2
  ###Med3
  config_reversao[10] <- config_sem_rev$`Resultado Final 3 (um)`
  config_reversao[11] <- config_com_rev$`Resultado Final 3 (um)`
  config_reversao[12] <- (config_reversao[10]-config_reversao[11])/2
  config_reversao[13] <- (config_reversao[10]+config_reversao[11])/2
  ###Med4
  config_reversao[14] <- config_sem_rev$`Resultado Final 4 (um)`
  config_reversao[15] <- config_com_rev$`Resultado Final 4 (um)`
  config_reversao[16] <- (config_reversao[14]-config_reversao[15])/2
  config_reversao[17] <- (config_reversao[14]+config_reversao[15])/2
  ###Média
  config_reversao[18] <- config_sem_rev$`Resultado Final Medio (um)`
  config_reversao[19] <- config_com_rev$`Resultado Final Medio (um)`
  config_reversao[20] <- (config_reversao[18]-config_reversao[19])/2
  config_reversao[21] <- (config_reversao[18]+config_reversao[19])/2
  
  colnames(config_reversao) <- c("xi", "I1(x)-Med1","I2(x)-Med1","M(x)-Med1",
                                 "S(x)-Med1","I1(x)-Med2","I2(x)-Med2",
                                 "M(x)-Med2","S(x)-Med2","I1(x)-Med3",
                                 "I2(x)-Med3","M(x)-Med3","S(x)-Med3",
                                 "I1(x)-Med4","I2(x)-Med4","M(x)-Med4","S(x)-Med4",
                                 "I1(x)-Medio","I2(x)-Medio","M(x)-Medio","S(x)-Medio")
  
  return(config_reversao)
}

### 0 mm
### Ida
zero_reversao_ida <- as.data.frame(zero_sem_rev_ida$`Posição (mm)`)
zero_reversao_ida <- reversao(zero_reversao_ida,zero_sem_rev_ida,zero_com_rev_ida)
### Volta
zero_reversao_volta <- as.data.frame(zero_sem_rev_volta$`Posição (mm)`)
zero_reversao_volta <- reversao(zero_reversao_volta,zero_sem_rev_volta,zero_com_rev_volta)

### 100 mm
### Ida
cem_reversao_ida <- as.data.frame(cem_sem_rev_ida$`Posição (mm)`)
cem_reversao_ida <- reversao(cem_reversao_ida,cem_sem_rev_ida,cem_com_rev_ida)
### Volta
cem_reversao_volta <- as.data.frame(cem_sem_rev_volta$`Posição (mm)`)
cem_reversao_volta <- reversao(cem_reversao_volta,cem_sem_rev_volta,cem_com_rev_volta)

### 200 mm
### Ida
duzentos_reversao_ida <- as.data.frame(duzentos_sem_rev_ida$`Posição (mm)`)
duzentos_reversao_ida <- reversao(duzentos_reversao_ida,duzentos_sem_rev_ida,duzentos_com_rev_ida)
### Volta
duzentos_reversao_volta <- as.data.frame(duzentos_sem_rev_volta$`Posição (mm)`)
duzentos_reversao_volta <- reversao(duzentos_reversao_volta,duzentos_sem_rev_volta,duzentos_com_rev_volta)

### 300 mm
### Ida
trezentos_reversao_ida <- as.data.frame(trezentos_sem_rev_ida$`Posição (mm)`)
trezentos_reversao_ida <- reversao(trezentos_reversao_ida,trezentos_sem_rev_ida,trezentos_com_rev_ida)
### Volta
trezentos_reversao_volta <- as.data.frame(trezentos_sem_rev_volta$`Posição (mm)`)
trezentos_reversao_volta <- reversao(trezentos_reversao_volta,trezentos_sem_rev_volta,trezentos_com_rev_volta)


## GRÁFICOS - Erros do carro do torno

par(mfrow=c(2,2))

# 0 mm
plot(zero_reversao_ida$`xi`,zero_reversao_ida$`M(x)-Medio`,type="o",xaxp=c(0,680,17),
     main = "Erro do Barramento (0 mm)",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)",col="blue",xlim = c(0,680),ylim = c(0,14),pch=20)
lines(x=zero_reversao_volta$`xi`,y=zero_reversao_volta$`M(x)-Medio`,type="o",col="orange",pch=20)
grid(nx = NULL, ny = NULL,lty = 3,lwd = 1,col = "gray")
legend(x = "topright",inset =  .02,
       legend = c("M(x) - Ida","M(x) - Volta"),
       col=c("blue","orange"),
       lty = 1,lwd = 1,bty = "o",pch=20,cex=0.6,ncol=1,text.font = 4)

# 100 mm
plot(cem_reversao_ida$`xi`,cem_reversao_ida$`M(x)-Medio`,type="o",xaxp=c(0,680,17),
     main = "Erro do Barramento (100 mm)",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)",col="blue",xlim = c(0,680),ylim = c(0,14),pch=20)
lines(x=cem_reversao_volta$`xi`,y=cem_reversao_volta$`M(x)-Medio`,type="o",col="orange",pch=20)
grid(nx = NULL, ny = NULL,lty = 3,lwd = 1,col = "gray")
legend(x = "topright",inset =  .02,
       legend = c("M(x) - Ida","M(x) - Volta"),
       col=c("blue","orange"),
       lty = 1,lwd = 1,bty = "o",pch=20,cex=0.6,ncol=1,text.font = 4)

# 200 mm
plot(duzentos_reversao_ida$`xi`,duzentos_reversao_ida$`M(x)-Medio`,type="o",xaxp=c(0,680,17),
     main = "Erro do Barramento (200 mm)",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)",col="blue",xlim = c(0,680),ylim = c(0,14),pch=20)
lines(x=duzentos_reversao_volta$`xi`,y=duzentos_reversao_volta$`M(x)-Medio`,type="o",col="orange",pch=20)
grid(nx = NULL, ny = NULL,lty = 3,lwd = 1,col = "gray")
legend(x = "topright",inset =  .02,
       legend = c("M(x) - Ida","M(x) - Volta"),
       col=c("blue","orange"),
       lty = 1,lwd = 1,bty = "o",pch=20,cex=0.6,ncol=1,text.font = 4)

# 300 mm
plot(trezentos_reversao_ida$`xi`,trezentos_reversao_ida$`M(x)-Medio`,type="o",xaxp=c(0,680,17),
     main = "Erro do Barramento (300 mm)",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)",col="blue",xlim = c(0,680),ylim = c(0,14),pch=20)
lines(x=trezentos_reversao_volta$`xi`,y=trezentos_reversao_volta$`M(x)-Medio`,type="o",col="orange",pch=20)
grid(nx = NULL, ny = NULL,lty = 3,lwd = 1,col = "gray")
legend(x = "topright",inset =  .02,
       legend = c("M(x) - Ida","M(x) - Volta"),
       col=c("blue","orange"),
       lty = 1,lwd = 1,bty = "o",pch=20,cex=0.6,ncol=1,text.font = 4)

## GRÁFICOS - Erros do artefato - Barra

par(mfrow=c(2,2))

# 0 mm
plot(zero_reversao_ida$`xi`,zero_reversao_ida$`S(x)-Medio`,type="o",xaxp=c(0,680,17),
     main = "Erro do Artefato (0 mm)",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)",col="blue",xlim = c(0,680),ylim = c(-140,40),pch=20,yaxp=c(-140,40,9))
lines(x=zero_reversao_volta$`xi`,y=zero_reversao_volta$`S(x)-Medio`,type="o",col="orange",pch=20)
grid(nx = NULL, ny = NULL,lty = 3,lwd = 1,col = "gray")
legend(x = "top",inset =  .02,
       legend = c("S(x) - Ida","S(x) - Volta"),
       col=c("blue","orange"),
       lty = 1,lwd = 1,bty = "o",pch=20,cex=0.6,ncol=1,text.font = 4)

# 100 mm
plot(cem_reversao_ida$`xi`,cem_reversao_ida$`S(x)-Medio`,type="o",xaxp=c(0,680,17),
     main = "Erro do Artefato (100 mm)",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)",col="blue",xlim = c(0,680),ylim = c(-140,40),pch=20,yaxp=c(-140,40,9))
lines(x=cem_reversao_volta$`xi`,y=cem_reversao_volta$`S(x)-Medio`,type="o",col="orange",pch=20)
grid(nx = NULL, ny = NULL,lty = 3,lwd = 1,col = "gray")
legend(x = "top",inset =  .02,
       legend = c("S(x) - Ida","S(x) - Volta"),
       col=c("blue","orange"),
       lty = 1,lwd = 1,bty = "o",pch=20,cex=0.6,ncol=1,text.font = 4)

# 200 mm
plot(duzentos_reversao_ida$`xi`,duzentos_reversao_ida$`S(x)-Medio`,type="o",xaxp=c(0,680,17),
     main = "Erro do Artefato (200 mm)",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)",col="blue",xlim = c(0,680),ylim = c(-140,40),pch=20,yaxp=c(-140,40,9))
lines(x=duzentos_reversao_volta$`xi`,y=duzentos_reversao_volta$`S(x)-Medio`,type="o",col="orange",pch=20)
grid(nx = NULL, ny = NULL,lty = 3,lwd = 1,col = "gray")
legend(x = "top",inset =  .02,
       legend = c("S(x) - Ida","S(x) - Volta"),
       col=c("blue","orange"),
       lty = 1,lwd = 1,bty = "o",pch=20,cex=0.6,ncol=1,text.font = 4)

# 300 mm
plot(trezentos_reversao_ida$`xi`,trezentos_reversao_ida$`S(x)-Medio`,type="o",xaxp=c(0,680,17),
     main = "Erro do Artefato (300 mm)",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)",col="blue",xlim = c(0,680),ylim = c(-140,40),pch=20,yaxp=c(-140,40,9))
lines(x=trezentos_reversao_volta$`xi`,y=trezentos_reversao_volta$`S(x)-Medio`,type="o",col="orange",pch=20)
grid(nx = NULL, ny = NULL,lty = 3,lwd = 1,col = "gray")
legend(x = "top",inset =  .02,
       legend = c("S(x) - Ida","S(x) - Volta"),
       col=c("blue","orange"),
       lty = 1,lwd = 1,bty = "o",pch=20,cex=0.6,ncol=1,text.font = 4)

# -----------------------------------------------------------------------------------------------------------------

# COMPARAÇÃO - RESULTADOS MMC
## Importando dados medidos pela MMC
dados_MMC = read_excel("Dados_MMC_medidaperilret_artefato_2019.xlsx",sheet = 2)
dados_MMC$`y (começando do 0)` <- dados_MMC$`y (começando do 0)`*1000
par(mfrow=c(1,1))
plot(x=dados_MMC$x,dados_MMC$`y (começando do 0)`,xlim = c(0,680),
     xaxp=c(0,680,17),col="blue",type="l",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)", main = "Desvio Artefato e Reta de Ajuste - MMC")

### Aplicando Mínimos Quadrados
reta_ajuste <- lm(dados_MMC$`y (começando do 0)`~dados_MMC$x)
coeficientes <- coef(reta_ajuste)
### Nova coluna com os pontos obtidos pela reta de ajuste (yi)
dados_MMC[3] <- dados_MMC$x*coeficientes[2]+coeficientes[1]
colnames(dados_MMC)[3] <- "Reta Ajuste (yi)"
### Nova coluna com os valores subtraídos alinhados pela reta de ajuste
dados_MMC[4] <- dados_MMC$`y (começando do 0)` - dados_MMC$`Reta Ajuste (yi)`
colnames(dados_MMC)[4] <- "yi'-yi"
### Zerando o ponto inicial (referência)
dados_MMC[5] <- dados_MMC$`yi'-yi` - dados_MMC$`yi'-yi`[1]
colnames(dados_MMC)[5] <- "Resultado Final (um)"

lines(x=dados_MMC$x,y=dados_MMC$`Reta Ajuste (yi)`,lty=2,col="orange")

plot(x=dados_MMC$x,dados_MMC$`Resultado Final (um)`,xlim = c(0,680),
     xaxp=c(0,680,17),yaxp=c(-140,40,9),col="blue",type="l",xlab = "Deslocamento Longitudinal (mm)",
     ylab = "Desvio (um)", main = "Desvio do Artefato - MMC")

lines(x=zero_reversao_ida$`xi`,y=zero_reversao_ida$`S(x)-Medio`,col="orange")
lines(x=zero_reversao_volta$`xi`,y=zero_reversao_volta$`S(x)-Medio`,col="orange",lty=2)
lines(x=cem_reversao_ida$`xi`,y=cem_reversao_ida$`S(x)-Medio`,col="red")
lines(x=cem_reversao_volta$`xi`,y=cem_reversao_volta$`S(x)-Medio`,col="red",lty=2)
lines(x=duzentos_reversao_ida$`xi`,y=duzentos_reversao_ida$`S(x)-Medio`,col="green")
lines(x=duzentos_reversao_volta$`xi`,y=duzentos_reversao_volta$`S(x)-Medio`,col="green",lty=2)
lines(x=trezentos_reversao_ida$`xi`,y=trezentos_reversao_ida$`S(x)-Medio`,col="purple")
lines(x=trezentos_reversao_volta$`xi`,y=trezentos_reversao_volta$`S(x)-Medio`,col="purple",lty=2)
legend(x="top",inset=.02,legend = c("MMC","0 mm - ida","0 mm - volta","100 mm - ida","100 mm - volta",
                                    "200 mm - ida","200 mm - volta","300 mm - ida","300 mm - volta"),
       col=c("blue","orange","orange","red","red","green","green","purple","purple"),
       lty = c(1,1,2,1,2,1,2,1,2),ncol = 2)

#---------------------------------------------------------------------------------------------------------------------

# TESTE DE TUKEY E ANÁLISE DA VARIÂNCIA (ANOVA)
# Aqui teremos que tratar os dados para cada medição, e não apenas nos valores médios, como foi feito até o momento
# Para isso, serão necessárias novas análises e tratamentos separadamente

# ANÁLISE DA VARIÂNCIA (ANOVA) - Influência Longitudinal
#Criando a tabela com os valores do desvio do carro separado para cada uma das 4 medições
df_anova_0_ida <- as.data.frame(zero_reversao_ida$xi)
df_anova_0_volta <- as.data.frame(zero_reversao_volta$xi)
df_anova_100_ida <- as.data.frame(cem_reversao_ida$xi)
df_anova_100_volta <- as.data.frame(cem_reversao_volta$xi)
df_anova_200_ida <- as.data.frame(duzentos_reversao_ida$xi)
df_anova_200_volta <- as.data.frame(duzentos_reversao_volta$xi)
df_anova_300_ida <- as.data.frame(trezentos_reversao_ida$xi)
df_anova_300_volta <- as.data.frame(trezentos_reversao_volta$xi)

analise_longitudinal = function(config_anova,config_reversao){
  colnames(config_anova)[1] <- "Deslocamento Longitudinal (mm)"
  config_anova[2] <- config_reversao$`M(x)-Med1`
  config_anova[3] <- config_reversao$`M(x)-Med2`
  config_anova[4] <- config_reversao$`M(x)-Med3`
  config_anova[5] <- config_reversao$`M(x)-Med4`
  colnames(config_anova) <- c("Deslocamento Longitudinal (mm)","M(x)-Med1","M(x)-Med2","M(x)-Med3","M(x)-Med4")
  config_anova <- t(config_anova) # Transpondo a matriz
  colnames(config_anova) <- config_anova[1, ]
  config_anova <- config_anova[2:5,] #Excluindo a primeira linha
  config_anova <- as.data.frame(config_anova) # convertendo o tipo de matriz para dataframe
  config_anova <- config_anova[ ,2:18] #Excluindo coluna do ponto zero pois é a referência (valor sempre será zero)
  
  return(config_anova)
}

df_anova_0_ida <- analise_longitudinal(df_anova_0_ida,zero_reversao_ida)
df_anova_0_volta <- analise_longitudinal(df_anova_0_volta,zero_reversao_volta)
df_anova_100_ida <- analise_longitudinal(df_anova_100_ida,cem_reversao_ida)
df_anova_100_volta <- analise_longitudinal(df_anova_100_volta,cem_reversao_volta)
df_anova_200_ida <- analise_longitudinal(df_anova_200_ida,duzentos_reversao_ida)
df_anova_200_volta <- analise_longitudinal(df_anova_200_volta,duzentos_reversao_volta)
df_anova_300_ida <- analise_longitudinal(df_anova_300_ida,trezentos_reversao_ida)
df_anova_300_volta <- analise_longitudinal(df_anova_300_volta,trezentos_reversao_volta)

# Análise gráfica por boxplot - IDA
par(mfrow=c(2,2))
boxplot(df_anova_0_ida, xlab="Deslocamento Longitudinal (mm)",ylab="Erro do carro do torno (um)",
        main="0 mm - Ida")
boxplot(df_anova_100_ida, xlab="Deslocamento Longitudinal (mm)",ylab="Erro do carro do torno (um)",
        main="100 mm - Ida")
boxplot(df_anova_200_ida, xlab="Deslocamento Longitudinal (mm)",ylab="Erro do carro do torno (um)",
        main="200 mm - Ida")
boxplot(df_anova_300_ida, xlab="Deslocamento Longitudinal (mm)",ylab="Erro do carro do torno (um)",
        main="300 mm - Ida")

# Aplicando ANOVA - ida
df_anova_0_ida <- stack(df_anova_0_ida) # Cria um vetor no formato pilha para aplicar a anova
df_anova_100_ida <- stack(df_anova_100_ida)
df_anova_200_ida <- stack(df_anova_200_ida)
df_anova_300_ida <- stack(df_anova_300_ida)

anova_0_ida = aov(df_anova_0_ida$values~df_anova_0_ida$ind)
anova(anova_0_ida)
anova_100_ida = aov(df_anova_100_ida$values~df_anova_100_ida$ind)
anova(anova_100_ida)
anova_200_ida = aov(df_anova_200_ida$values~df_anova_200_ida$ind)
anova(anova_200_ida)
anova_300_ida = aov(df_anova_300_ida$values~df_anova_300_ida$ind)
anova(anova_300_ida)

# Análise gráfica por boxplot - VOLTA
par(mfrow=c(2,2))
boxplot(df_anova_0_volta, xlab="Deslocamento Longitudinal (mm)",ylab="Erro do carro do torno (um)",
        main="0 mm - Volta")
boxplot(df_anova_100_volta, xlab="Deslocamento Longitudinal (mm)",ylab="Erro do carro do torno (um)",
        main="100 mm - Volta")
boxplot(df_anova_200_volta, xlab="Deslocamento Longitudinal (mm)",ylab="Erro do carro do torno (um)",
        main="200 mm - Volta")
boxplot(df_anova_300_volta, xlab="Deslocamento Longitudinal (mm)",ylab="Erro do carro do torno (um)",
        main="300 mm - Volta")

# Aplicando ANOVA - Volta
df_anova_0_volta <- stack(df_anova_0_volta) # Cria um vetor no formato pilha para aplicar a anova
df_anova_100_volta <- stack(df_anova_100_volta)
df_anova_200_volta <- stack(df_anova_200_volta)
df_anova_300_volta <- stack(df_anova_300_volta)

anova_0_volta = aov(df_anova_0_volta$values~df_anova_0_volta$ind)
anova(anova_0_volta)
anova_100_volta = aov(df_anova_100_volta$values~df_anova_100_volta$ind)
anova(anova_100_volta)
anova_200_volta = aov(df_anova_200_volta$values~df_anova_200_volta$ind)
anova(anova_200_volta)
anova_300_volta = aov(df_anova_300_volta$values~df_anova_300_volta$ind)
anova(anova_300_volta)

# TESTE DE TUKEY
TukeyHSD(anova_0_ida,ordered = T,conf.level = 0.95)
#plot(TukeyHSD(anova_0_ida,ordered = T,conf.level = 0.95))


# ANÁLISE DA VARIÂNCIA (ANOVA) - Influência Transversal
# Ida
df_anova_40_ida <- as.data.frame(zero_reversao_ida[2,1])

df_anova_40_ida[2] <- zero_reversao_ida[2,4]
df_anova_40_ida[3] <- zero_reversao_ida[2,8]
df_anova_40_ida[4] <- zero_reversao_ida[2,12]
df_anova_40_ida[5] <- zero_reversao_ida[2,16]

df_anova_40_ida[6] <- cem_reversao_ida[2,4]
df_anova_40_ida[7] <- cem_reversao_ida[2,8]
df_anova_40_ida[8] <- cem_reversao_ida[2,12]
df_anova_40_ida[9] <- cem_reversao_ida[2,16]

df_anova_40_ida[10] <- duzentos_reversao_ida[2,4]
df_anova_40_ida[11] <- duzentos_reversao_ida[2,8]
df_anova_40_ida[12] <- duzentos_reversao_ida[2,12]
df_anova_40_ida[13] <- duzentos_reversao_ida[2,16]

df_anova_40_ida[14] <- trezentos_reversao_ida[2,4]
df_anova_40_ida[15] <- trezentos_reversao_ida[2,8]
df_anova_40_ida[16] <- trezentos_reversao_ida[2,12]
df_anova_40_ida[17] <- trezentos_reversao_ida[2,16]

colnames(df_anova_40_ida) <- c("Deslocamento Longitudinal","M(x)-Med1-0mm","M(x)-Med2-0mm",
                               "M(x)-Med3-0mm","M(x)-Med4-0mm","M(x)-Med1-100mm","M(x)-Med2-100mm",
                               "M(x)-Med3-100mm","M(x)-Med4-100mm","M(x)-Med1-200mm","M(x)-Med2-200mm",
                               "M(x)-Med3-200mm","M(x)-Med4-200mm","M(x)-Med1-300mm","M(x)-Med2-300mm",
                               "M(x)-Med3-300mm","M(x)-Med4-300mm")