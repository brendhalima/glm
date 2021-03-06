#### inserir em chunk ####
library(readxl)
library(MASS)
library(ggplot2)
library(gridExtra)
library(hnp)
library(car)
library(statmod)
library(effects)

da <- read.csv('CONTAGEM.csv', sep = ';')
da$obitos <- as.numeric(da$obitos)
da$pib <- as.numeric(da$pib)
#### inserir em chunk ####


#pt1
head(da)

#pt2 analise descritiva
summary(da)

#histogramas
par(mfrow = c(2,3))
hist(da$obitos, main = '�bitos', xlab = '', ylab = '')
hist(da$pib, main = 'pib', xlab = '', ylab = '')
hist(da$popula��o, main = 'popula��o', xlab = '', ylab = '')
hist(da$frota, main = 'frota', xlab = '', ylab = '')
hist(da$emergencia, main = 'emergencia', xlab = '', ylab = '')

#boxplots
boxplot(da$obitos, main = '�bitos', xlab = '', ylab = '')
boxplot(da$pib, main = 'pib', xlab = '', ylab = '')
boxplot(da$popula��o, main = 'popula��o', xlab = '', ylab = '')
boxplot(da$frota, main = 'frota', xlab = '', ylab = '')
boxplot(da$emergencia, main = 'emergencia', xlab = '', ylab = '')

#tranforma��o das variaveis explicativas

da$pib <- log(da$pib)
da$popula��o <- log(da$popula��o)
da$frota <- log(da$frota)
da$emergencia <- log(da$emergencia)

#correla��o   N�O CONSEGUI!!!!!!!!!!!!!

cor <- cor(da[,c(3,4,5,6,7)])

plot(cor)

#PRECISA DE GRAFICO DE DISPERS�O?????

##PT3 

##### GLM com resposta Poisson
m1 <- glm(obitos ~ pib + popula��o + frota + emergencia , data = da, family = 'poisson')
summary(m1)

######GLM com resposta Binomial Negativa
m2 <- glm.nb(obitos ~ pib + popula��o + frota + emergencia, data = da)
summary(m2)


#Escolha do Modelo

#O modelo que apresentou menor AIC e maior verossimilhan�a foi o modelo Binomial Negativo (m2)
##obs: Ambos modelos s�o muito proximo e muito bem ajustado, se gente quise poderia trabalhar com a Poisoon

ajuste = c('m1', 'm2')
aic    = c(AIC(m1), AIC(m2))
verossimilhan�a = c(logLik(m1),logLik(m2))
data.frame(ajuste, aic, verossimilhan�a)



#Verificando se modelos est�o bem ajustados.
##N�O CONSEGUI TAMB�M!!!!!!!!!!
hnp(m1, xlab = 'Percentil da N(0,1)', ylab = 'Res�duos', main = 'Gr�fico Normal de Probabilidades')
hnp(m2, xlab = 'Percentil da N(0,1)', ylab = 'Res�duos', main = 'Gr�fico Normal de Probabilidades')

#Agora escolhemos o modelo para ser usado, vamos ajustar ele. 

summary(m1)
m2.1  <- glm.nb(obitos ~ popula��o + frota, data = da)
summary(m2.1)  


###Vericando se modelo novos ajustados, se s�o melhores, podemos percer que n�o, que primeiro modelo
#E melhor todos, mesmo tento variaveis n�o significativa
ajuste = c('m2', 'm2.1')
aic    = c(AIC(m2), AIC(m2.1))
verossimilhan�a = c(logLik(m2),logLik(m2.1))
data.frame(ajuste, aic, verossimilhan�a)

# Gr�ficos de Res�duos
#### Modelo est� bem ajustado e n�o aparece tem nenhum outlair

par(mfrow=c(2,2))
plot(m2, 1:4)

par(mfrow = c(1,1))

#Medidas de Influ�ncia
influenceIndexPlot(m1, vars=c("Cook", "Studentized", "hat"), main="Medidas de Influ�ncia")


#Res�duos Quant�licos Aleatorizados
par(mfrow=c(1,2))

res <- qresiduals(m1)

plot(res)

residuos <- qresiduals(m1)
qqnorm(residuos)
qqline(residuos, col = 2)


#Gr�fico Normal de Probabilidades com Envelope Simulado
par(mfrow=c(1,1))
hnp(m1, xlab = 'Percentil da N(0,1)', ylab = 'Res�duos', main = 'Gr�fico Normal de Probabilidades')

# Gr�ficos de Efeitos
plot(allEffects(m1), type = 'response', main = '')


#PREDI��O ((N�O SEI FAZE))

##AJUSTE E DIAGNOSTICO TBM NAO 


### ACHO QUE � ISTO ANJAS DA MINHA VIDA,
####### ESTA QUASE PRONTO ESTA MERDA
######## EU ESPERO 
