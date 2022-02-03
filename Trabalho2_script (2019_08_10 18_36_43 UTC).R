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
hist(da$obitos, main = 'óbitos', xlab = '', ylab = '')
hist(da$pib, main = 'pib', xlab = '', ylab = '')
hist(da$população, main = 'população', xlab = '', ylab = '')
hist(da$frota, main = 'frota', xlab = '', ylab = '')
hist(da$emergencia, main = 'emergencia', xlab = '', ylab = '')

#boxplots
boxplot(da$obitos, main = 'óbitos', xlab = '', ylab = '')
boxplot(da$pib, main = 'pib', xlab = '', ylab = '')
boxplot(da$população, main = 'população', xlab = '', ylab = '')
boxplot(da$frota, main = 'frota', xlab = '', ylab = '')
boxplot(da$emergencia, main = 'emergencia', xlab = '', ylab = '')

#tranformação das variaveis explicativas

da$pib <- log(da$pib)
da$população <- log(da$população)
da$frota <- log(da$frota)
da$emergencia <- log(da$emergencia)

#correlação   NÃO CONSEGUI!!!!!!!!!!!!!

cor <- cor(da[,c(3,4,5,6,7)])

plot(cor)

#PRECISA DE GRAFICO DE DISPERSÃO?????

##PT3 

##### GLM com resposta Poisson
m1 <- glm(obitos ~ pib + população + frota + emergencia , data = da, family = 'poisson')
summary(m1)

######GLM com resposta Binomial Negativa
m2 <- glm.nb(obitos ~ pib + população + frota + emergencia, data = da)
summary(m2)


#Escolha do Modelo

#O modelo que apresentou menor AIC e maior verossimilhança foi o modelo Binomial Negativo (m2)
##obs: Ambos modelos são muito proximo e muito bem ajustado, se gente quise poderia trabalhar com a Poisoon

ajuste = c('m1', 'm2')
aic    = c(AIC(m1), AIC(m2))
verossimilhança = c(logLik(m1),logLik(m2))
data.frame(ajuste, aic, verossimilhança)



#Verificando se modelos estão bem ajustados.
##NÃO CONSEGUI TAMBÉM!!!!!!!!!!
hnp(m1, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')
hnp(m2, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')

#Agora escolhemos o modelo para ser usado, vamos ajustar ele. 

summary(m1)
m2.1  <- glm.nb(obitos ~ população + frota, data = da)
summary(m2.1)  


###Vericando se modelo novos ajustados, se são melhores, podemos percer que não, que primeiro modelo
#E melhor todos, mesmo tento variaveis não significativa
ajuste = c('m2', 'm2.1')
aic    = c(AIC(m2), AIC(m2.1))
verossimilhança = c(logLik(m2),logLik(m2.1))
data.frame(ajuste, aic, verossimilhança)

# Gráficos de Resíduos
#### Modelo está bem ajustado e não aparece tem nenhum outlair

par(mfrow=c(2,2))
plot(m2, 1:4)

par(mfrow = c(1,1))

#Medidas de Influência
influenceIndexPlot(m1, vars=c("Cook", "Studentized", "hat"), main="Medidas de Influência")


#Resíduos Quantílicos Aleatorizados
par(mfrow=c(1,2))

res <- qresiduals(m1)

plot(res)

residuos <- qresiduals(m1)
qqnorm(residuos)
qqline(residuos, col = 2)


#Gráfico Normal de Probabilidades com Envelope Simulado
par(mfrow=c(1,1))
hnp(m1, xlab = 'Percentil da N(0,1)', ylab = 'Resíduos', main = 'Gráfico Normal de Probabilidades')

# Gráficos de Efeitos
plot(allEffects(m1), type = 'response', main = '')


#PREDIÇÃO ((NÃO SEI FAZE))

##AJUSTE E DIAGNOSTICO TBM NAO 


### ACHO QUE É ISTO ANJAS DA MINHA VIDA,
####### ESTA QUASE PRONTO ESTA MERDA
######## EU ESPERO 
