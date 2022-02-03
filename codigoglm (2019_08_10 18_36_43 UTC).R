library(readxl)
dados <- read_excel("D:/Agatha/Desktop/dados.xlsx")

head(dados)

dados$A <- factor(dados$A)
summary(dados)


boxplot(dados)

ajuste1 <- glm(A ~ .,family=binomial(link = (link='logit')),data = dados)
ajuste2 <- glm(A ~ .,family=binomial(link = (link='probit')),data = dados)
ajuste3 <- glm(A ~ .,family=binomial(link = (link='cloglog')),data = dados)
ajuste4 <- glm(A ~ .,family=binomial(link = (link='cauchit')),data = dados)
selec <- data.frame(ajuste=c('logito', 'probito', 'cloglog', 'cauchy'),
                    aic=c(AIC(ajuste1), AIC(ajuste2), AIC(ajuste3), AIC(ajuste4)),
                    logLik=c(logLik(ajuste1),logLik(ajuste2),logLik(ajuste3),logLik(ajuste4)))

selec

envelope=function(modelo){
  dados=na.omit(modelo$data)
  nsim=100
  n=modelo$df.null+1
  r1=sort(rstandard(modelo,type='deviance'))
  m1=matrix(0,nrow=n,ncol=nsim)
  a2=simulate(modelo,nsim=nsim)
  
  for (i in 1:nsim){
    dados$y=a2[,i]
    aj=update(modelo,y~.,data=dados)
    m1[,i]=sort(rstandard(aj,type='deviance'))}
  
  li=apply(m1,1,quantile,0.025)
  m=apply(m1,1,quantile,0.5)
  ls=apply(m1,1,quantile,0.975)
  
  quantis=qnorm((1:n-0.5)/n)
  
  plot(rep(quantis,2),c(li,ls),type='n',xlab='Percentil da N(0,1)',ylab='Resíduos')
  title('Gráfico Normal de Probabilidades')
  lines(quantis,li,type='l')
  lines(quantis,m,type='l',lty=2)
  lines(quantis,ls,type='l')
  points(quantis,r1,pch=16,cex=0.75)
}
par(mfrow=c(2,2))
envelope(ajuste1)
envelope(ajuste2)
envelope(ajuste3)
envelope(ajuste4)
#######Apesar de da função probito ter dado menor AIC, graficamente a melhor e logito, então vamos usar a função logito

summary(ajuste1)

#OBs: FAzer a analise que suely fazer em dados categorio
#https://docs.ufpr.br/~giolo/CE073/Material/Respostas_Ex.htm só olha exercico 5

par(mfrow=c(2,2))
plot(ajuste1, 1:4)


library(car)
influenceIndexPlot(ajuste2)
 

library(statmod)
res <- qresiduals(ajuste1)
plot(res)
residuos <- qresiduals(ajuste1)
qqnorm(residuos)
qqline(residuos, col = 2)
shapiro.test(residuos)



envelope(ajuste2)

library(effects)

plot(allEffects(ajuste2), type = 'response')


