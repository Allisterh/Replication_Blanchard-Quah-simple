##PRIMEIRO LENDO OS DADOS
library(haven)
dados <- read_dta('D:\\Downloads\\Data.dta') ##USAR O PACOTE FOREIGN DEU PROBLEMA COM A VERS�O DOS DADOS

##Agora precisamos criar uma dummy que indique o tratamento no munic�pio 4

dados$trat <- 0 #Isso cria uma coluna inteira de 0s, agora basta que o munic�pio 4 tenha valor 1

dados$trat[dados$munic == 4] <- 1 ## O problema disso � que pegamos �pocas em que n�o estava sendo tratado ainda, os primeiros 5 anos

dados$trat[dados$munic==4] <- c(0,0,0,0,0,1,1,1,1,1) # Agora o tratamento est� do ano 6 ao 10, como no enunciado

dados$munic = as.factor(dados$munic)


##Agora separamos um grupo de tratamento (munic�pio 4) e um grupo de controle (resto dos munic�pios), basta fazer a regress�o
library(plm)
pdados <- pdata.frame(dados, index = c('munic', 't'))
reg <- plm(Y ~ t + trat, model = 'within', data = pdados)

summary(reg)
library(lmtest)
coeftest(reg, vcov. = vcovHC(reg, type="HC0")) #Erros padr�o robustos � heteroscedasticidade

library(ggplot2)
library(dplyr)
dados_plot <- dados %>%
  group_by(t, trat) %>%
  summarise(Y = mean(Y))

dados_plot$t = as.numeric(dados_plot$t)
ggplot(dados_plot, aes(x = t, y = Y, color = factor(trat))) +   geom_line() + geom_vline(xintercept=5)

