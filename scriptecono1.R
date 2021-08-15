setwd('D:\\Desktop\\fgv eesp\\Trabalho Econo 3')
library(plm)
library(tidyr)
library(dplyr)
library(ggplot2)

#Lendo
emprego = read.csv('empregopainel.csv', sep = ';', header = TRUE)
educ = read.csv('educpainel.csv', sep = ';', header = TRUE, encoding = 'Latin', dec = ',')
target2000 = read.csv('IDH.csv', sep = ';', header = TRUE, dec = ',')
target2010 = read.csv('IDH-2010.csv', sep = ';', header = TRUE, dec = ',')
target2010 = data.frame(target2010$Nome, target2010$IDHM2010, target2010$Ano)
pib_ind = read.csv('pibipainel.csv', sep = ';', header = TRUE, encoding = 'Latin', dec = ',')
pib = read.csv('pibmpainel.csv', sep = ';', header = TRUE, encoding = 'Latin', dec = ',')
pop_tot = read.csv('poptotalpainel.csv', sep = ';', header = TRUE, encoding = 'Latin', dec = ',')
pop_urb = read.csv('popurbpainel.csv', sep = ';', header = TRUE, encoding = 'Latin', dec = ',')
saude = read.csv('saudepainel.csv', sep = ';', header = TRUE)

#Tratando as variáveis que vieram no IPEA para ficarem no mesmo formato que do atlas
nome <- function(x, y){
  final <- c()
  for (i in 1:length(x)){
    a <- c(x[i], y[i])
    name <- paste(a, collapse = ' (')
    b <- c(name, ')')
    final <- c(final,(paste(b, collapse = '')))}
  return(final)
}

#x = 'bira'
#y = 'dira'
#x = c('bira', 'dira')
#y = c('piri', 'biri')
#a = nome(x, y)

emprego$Nome <- nome(emprego$Município, emprego$Sigla)
educ$Nome = nome(educ$Município, educ$Sigla)
pib_ind$Nome = nome(pib_ind$Município, pib_ind$Sigla)
pib$Nome = nome(pib$Município, pib$Sigla)
pop_tot$Nome = nome(pop_tot$Município, pop_tot$Sigla)
pop_urb$Nome = nome(pop_urb$Município, pop_urb$Sigla)
saude$Nome = nome(saude$Município, saude$Sigla)

#Reordenando as colunas pra ficar mais bonito (eventualmente isso vai ser importante)
emprego <- emprego[,c(6,4,5)]
educ <- educ[,c(6,4,5)]
pib_ind <- pib_ind[,c(6,4,5)]
pib <- pib[,c(6,4,5)]
pop_tot <- pop_tot[,c(6,4,5)]
pop_urb <- pop_urb[,c(6,4,5)]
saude <- saude[,c(6,4,5)]

#Transformando as duas colunas de datas diferentes em linhas empilhadas de mesma variável
emprego <- pivot_longer(emprego, cols = -1, names_to = 'Ano', values_to = 'emprego')
educ <- pivot_longer(educ, cols = -1, names_to = 'Ano', values_to = 'educ')
pib_ind <- pivot_longer(pib_ind, cols = -1, names_to = 'Ano', values_to = 'pib_ind')
pib <- pivot_longer(pib, cols = -1, names_to = 'Ano', values_to = 'pib')
pop_tot <- pivot_longer(pop_tot, cols = -1, names_to = 'Ano', values_to = 'pop_tot')
pop_urb <- pivot_longer(pop_urb, cols = -1, names_to = 'Ano', values_to = 'pop_urb')
saude <- pivot_longer(saude, cols = -1, names_to = 'Ano', values_to = 'saude')

#Consertando a formatação doida do excel
emprego$Ano <- gsub('X', '', emprego$Ano)
educ$Ano <- gsub('X', '', educ$Ano)
pib_ind$Ano <- gsub('X', '', pib_ind$Ano)
pib$Ano <- gsub('X', '', pib$Ano)
pop_tot$Ano <- gsub('X', '', pop_tot$Ano)
pop_urb$Ano <- gsub('X', '', pop_urb$Ano)
saude$Ano <- gsub('X', '', saude$Ano)

#Juntando tudo
data <- Reduce(merge, list(emprego, educ, pib_ind, pib, pop_tot, pop_urb, saude))

#Tratamento das targets
colnames(target2000) <- c('Nome', 'IDHM', 'Ano')
colnames(target2010) <- c('Nome', 'IDHM', 'Ano')
target2010$IDHM <- as.numeric(target2010$IDHM)
target2000$IDHM <- as.numeric(target2000$IDHM)
a <- rbind(target2000, target2010)
data <- merge(data, a) #Prontinho, mas desbalanceado

#Balanceando o painel por meio de exclusão:
data <- na.omit(data)
intervalo = c(2000, 2010)
data = data[data$Ano%in%intervalo,]
ids = numeric()
for(i in unique(data$Nome)){
  ids[paste(i)] = sum(data$Nome==i)==length(intervalo)}

index = data$Nome %in% names(ids)[ids==1]
data = data[index,]

#Prontinho

data$Ano <- as.numeric(data$Ano)
data$emprego <- as.numeric(gsub(',', '.', data$emprego))
data$educ <- as.numeric(gsub(',', '.', data$educ))
data$saude <- as.numeric(gsub(',', '.', data$saude))

#Tudo numérico bonitinho

data$indust <- data$pib_ind/data$pib
data$industq <- data$indust**2
data$urb <- data$pop_urb/data$pop_tot
data$logpib <- log(data$pib)
data$logpop <- log(data$pop_tot)
data$inter <- data$indust*data$saude

#Regressão

regmain <- plm(IDHM ~ indust + industq + inter + logpib + logpop + urb + saude + educ + emprego + as.numeric(Ano), data, index = c('Nome', 'Ano'), model = 'within')
summary(regmain)

regnointer <- plm(IDHM ~ indust + industq + logpib + logpop + urb + saude + educ + emprego + as.numeric(Ano), data, index = c('Nome', 'Ano'), model = 'within')
summary(regnointer)

regrandom <- plm(IDHM ~ indust + industq + inter + logpib + logpop + urb + saude + educ + emprego + as.numeric(Ano), data, index = c('Nome', 'Ano'), model = 'random')
summary(regrandom)

regsaude <- plm(IDHM ~ indust + industq + inter + logpib + logpop + urb + educ + emprego + as.numeric(Ano), data, index = c('Nome', 'Ano'), model = 'within')
summary(regsaude)

reginverso <- plm(saude ~ indust + industq + logpib + logpop + urb + IDHM + educ + emprego + as.numeric(Ano), data, index = c('Nome', 'Ano'), model = 'within')
summary(reginverso)


phtest(regmain, regrandom)

library(lmtest)
library(sandwich)
coeftest(regmain, vcov. = vcovHC(regmain, type = "HC0"))
coeftest(regnointer, vcov. = vcovHC(regnointer, type = "HC0"))


#gráficos para a análise exploratória
library(foreign)
library(gplots)

plotmeans(indust ~ Ano, data, main = 'Evolução da industrialização com o tempo')

plotmeans(IDHM ~ Ano, data, main = "Tendência temporal")

random <- rbind(data[3953,], data[5551,], data[1756,], data[10416,], data[6344,]) #números gerados em randomnumbergenerator.org

plotmeans(IDHM ~ Nome, random, main = "Heterogeneidade entre municípios")

plotmeans(indust ~ Nome, random, main = "Heterogeneidade da indústria entre municípios")

ggplot(data, aes(x = IDHM)) + geom_histogram(alpha = 0.9, bins = 30, aes(color = 'as.factor(Ano)', fill = as.factor(Ano)), position = 'identity') + ylab('Contagem') + labs(color = "Ano", fill = "Ano")

ggplot(data, aes(x = IDHM, y = urb)) + geom_point() + geom_smooth(method = 'lm')
