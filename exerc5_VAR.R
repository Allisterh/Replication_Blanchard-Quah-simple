library(vars)
library(dynlm)
library(readxl)
library(ggplot2)
library(tidyr)

#Lendo os dados
dados <- read_xlsx('D:\\Desktop\\fgv eesp\\trabalhos econo aplicada\\data_ep5.xlsx')

#Formatando as datas mais bonitinho
dados$dates <- as.yearqtr(dados$dates)


#Transformando as s�ries de tempo em objetos timeseries no R
dados$dgnp <- ts(dados$dgnp, start = c(1948,2), frequency = 4)
dados$unemp <- ts(dados$unemp, start = c(1948,2), frequency = 4)

##Breve an�lise explorat�ria

plot(dados$dgnp)
plot(dados$unemp)

acf(dados$dgnp)
pacf(dados$dgnp)
acf(dados$unemp)
pacf(dados$unemp)



########################################
##Replicando Blanchard and Quah (1989)##
########################################
#O artigo usou dados de 50 a 87

inicio <- c(1950,2)
fim <- c(1987,4)

dgnp <- window(dados$dgnp, inicio, fim)
unemp <- window(dados$unemp, inicio, fim)

#No caso base do artigo estima-se controlando para uma desacelera��o da taxa de crescimento do PIB e uma tend�ncia secular no desemprego. 

#dgnp_est_reg <- dynlm(dgnp ~ 1 + trend(dgnp)) 
#dgnp_est <- dgnp_est_reg$residuals           Aqui removeria-se uma tend�ncia secular do PIB, mas n�o � isso que o artigo menciona.
#plot(dgnp_est)                               o que Blanchard e Quah fazem � retirar o efeito do choque da OPEC nos pre�os de petr�leo em 74,
#                                             subtraindo as m�dias antes e depois do choque. Em vez disso, podemos usar dummies no OLS para o mesmo efeito

library(tstools) #Esse pacote foi descontinuado no CRAN. Para utiliz�-lo tem que baixar manualmente pelo archive

OPEC_shock <- c(1974,1)
dummy <- tstools::create_dummy_ts(fim,
                                  dummy_start = OPEC_shock,
                                  dummy_end = fim,
                                  start_basic = inicio,
                                  basic_value = 0,
                                  dummy_value = 1,
                                  frequency = 4)

dgnp_est_reg <- dynlm(dgnp ~ 1 + dummy)  #Os res�duos dessa regress�o por OLS v�o ser uma s�rie temporal de todos os componentes do PNB
dgnp_est <- dgnp_est_reg$residuals#       exceto a dummy
plot(dgnp_est) #Podemos ver no plot que o choque foi removido


unemp_est_reg <- dynlm(unemp ~ 1 + trend(unemp)) #Aqui sim temos tend�ncia secular. Vamos retir�-la estimando uma linha reta de tend�ncia por OLS
unemp_est <- unemp_est_reg$residuals#             e ent�o usando os res�duos, que ser�o uma s�rie de tempo controlada pela tend�ncia.
plot(unemp_est)


#Para estimar o VAR, Blanchard e Quah usam 8 lags. Faremos o mesmo aqui para replicar o experimento.

dados_est <- data.frame(unemp = unemp_est, dgnp = dgnp_est)

var <- VAR(dados_est, p = 8, type = 'none')
bq <- BQ(var) #Estima��o com a identifica��o definida pelo artigo
summary(bq)

#IRFs
irf <- irf(bq, n.ahead = 40, boot = TRUE, ci = 0.68)
cirf <- irf(bq, n.ahead = 40, boot = TRUE, ci = 0.68, cumulative = TRUE)
plot(irf)
plot(cirf)
bq$B

#Decomposi��o de vari�ncia
decomp = fevd(bq, n.ahead = 40)
a = c("PIB", "Desemprego")
df = data.frame(decomp$dgnp, decomp$unemp, 1:40)
colnames(df) = c(paste(a[1],a[1:2]), paste(a[2],a[1:2]), "Trimestres")
df = pivot_longer(df, cols=1:4, names_sep=" ", names_to=c("wrap", "Legenda"))

ggplot(df, aes(y=value, x=Trimestres, fill=Legenda)) +
  geom_bar(stat="identity", position="fill") +
  facet_wrap(~wrap, nrow=2) +
  ylab("Porcentagem") + 
  scale_fill_brewer(palette="Set1") +
  theme_minimal()

################################
##Estimando com os dados todos##
################################

dgnpfull <- dados$dgnp
unempfull <- dados$unemp

#Tratando os dados de novo

inicio <- c(1948, 2)
fim <- c(2020, 1)
OPEC_shock <- c(1974,1)
dummy2 <- tstools::create_dummy_ts(fim,
                                  dummy_start = OPEC_shock,
                                  dummy_end = fim,
                                  start_basic = inicio,
                                  basic_value = 0,
                                  dummy_value = 1,
                                  frequency = 4)

dgnpfull_reg <- dynlm(dgnpfull ~ 1 + dummy2)
dgnpfull_est <- dgnpfull_reg$residuals

unempfull_reg <- dynlm(unempfull ~ 1 + trend(unempfull))
unempfull_est <- unempfull_reg$residuals

dadosfull <- data.frame(unemp = unempfull_est, dgnp = dgnpfull_est)

#Estimando e identificando o VAR

varfull <- VAR(dadosfull, p = 8) # Vamos manter o p = 8 por enquanto
bqfull <- BQ(varfull)
summary(bqfull)

#IRFs
irffull <- irf(bqfull, n.ahead = 40, boot = TRUE, ci = 0.68)
plot(irffull)

cirffull <- irf(bqfull, n.ahead = 40, boot = TRUE, ci = 0.68, cumulative = TRUE)
plot(cirffull)
#Decomposi��o de vari�ncia
decompfull = fevd(bqfull, n.ahead = 40)
a = c("PIB", "Desemprego")
df = data.frame(decompfull$dgnp, decompfull$unemp, 1:40)
colnames(df) = c(paste(a[1],a[1:2]), paste(a[2],a[1:2]), "Trimestres")
df = pivot_longer(df, cols=1:4, names_sep=" ", names_to=c("wrap", "Legenda"))

ggplot(df, aes(y=value, x=Trimestres, fill=Legenda)) +
  geom_bar(stat="identity", position="fill") +
  facet_wrap(~wrap, nrow=2) +
  ylab("Porcentagem") + 
  scale_fill_brewer(palette="Set1") +
  theme_minimal()

#� razo�vel que a ordem do VAR mude com a nova informa��o. Nessa se��o estimamos o VAR com crit�rio de informa��o de Akaike

vari <- VAR(dadosfull, ic = 'AIC', lag.max = 20)
summary(vari)
bqi <- BQ(vari)
summary(bqi)

irfi <- irf(bqi, n.ahead = 40, boot = TRUE, ci = 0.68)
plot(irfi)
