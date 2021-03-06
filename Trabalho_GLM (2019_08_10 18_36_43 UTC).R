

### Conjunto de dados da AI for Social Good: 
### Women Coders' Bootcamp (Centro de treinamento de mulheres programadoras)
### organizado por Intelig�ncia Artificial para desenvolvimento em colabora��o com a 
### UNDP Nepal (Programa de Desenvolvimento das Na��es Unidas - Nepal)


### Descri��o -----------------------------------------------------------------------------------------
### Em torno do mundo, c�ncer de mama � o tipo mais comum de c�ncer em mulheres e � o segundo maior em 
### termos de taxas de mortalidade. O diagn�stico do c�ncer de mama � obtido quando um caro�o anormal
### � encontrado (por auto exame ou raio-x) ou quando um min�sculo gr�o de c�lcio � encontrado (raio-x).
### Depois que o caro�o suspeito � encontrado, o doutor vai conduzir um diagn�stico para determinar
### se � cancer�geno, e se for, se ele se espalhou para outras partes do corpo.
### Este conjunto de dados foi obtido da University of Wisconsin Hospitals, em Madison atrav�s do
### Dr. William H. Wolberg.


### Outras refer�ncias do problema: https://ai.google/social-good
### DESCRI��O COMPLETA https://www.rdocumentation.org/packages/TH.data/versions/1.0-9/topics/wpbc



### Vari�vel resposta da base --------------------------------------------------------------------------
### - Diagn�stico do tecido da mama (1 n�dulo maligno e 0 n�dulo benigno)

### Vari�veis explicativas da base ---------------------------------------------------------------------
### - mean_radius: raio m�dio da dist�ncia do centro ao per�metro
### - mean_texture: textura m�dia, irregulareidades (standard deviation of gray-scale values = 
### AN�LISE DE TEXTURA ESTAT�STICA
### http://www.lcad.icmc.usp.br/~jbatista/procimg/2012/textura)
### - mean_perimeter: per�metro m�dio do tumor
### - mean_area: �rea m�dia
### - mean_smoothness: regularidade m�dia (mean of local variation in radius lengths)


### In�cio da an�lise ----------------------------------------------------------------------------------

library(readxl)
dados <- read_excel("~/Desktop/UFPR/Modelos Lineares Generalizados/trabalho 1 2018/Breast_cancer_data.xlsm")

names(dados) <- c("raio","textura","perimetro","area","regularidade","diagnostico")

dados$diagnostico <- factor(dados$diagnostico)
summary(dados)

### Exerc�cio - Fazer uma an�lise descritiva (uni e bivariada), buscando avaliar, de forma preliminar 
### por meio de gr�ficos e medidas resumo, a rela��o entre as covari�veis e a resposta.

plot(dados$"raio",dados$"diagnostico")
boxplot(dados)

### Vamos separar, aleatoriamente, a base em duas: uma para o ajuste do modelo (com 4000 observa��es) e outra para valida��o (com 1000 observa��es).
sorteio=sample(1:5000) ### sorteio � um vetor com n�meros de 1 a 5000 numa sequ�ncia aleat�ria.
dadosajuste=dados[sorteio[1:4000],] ### dataframe com as quatro mil linhas para ajuste. 
dadosvalid=dados[sorteio[4001:5000],] ### dataframe com mil linhas, apenas para valida��o.
ajuste=glm(diagnostico~raio+textura+perimetro+area+regularidade,family=binomial,data=dadosajuste) ### Ajuste do modelo de regress�o log�stica.
### Antes de mais nada, � fundamental saber como a vari�vel resposta est� sendo modelada (basicamente, a qual categoria da resposta o R associa valor zero
### e a qual o R associa valor um. Para isso, vamos extrair do glm ajustado o objeto y:



#c�digos do trab da agatha
ajuste1 <- glm(diagnostico ~ .,family=binomial(link = (link='logit')),data = dados)
ajuste2 <- glm(diagnostico ~ .,family=binomial(link = (link='probit')),data = dados)
ajuste3 <- glm(diagnostico ~ .,family=binomial(link = (link='cloglog')),data = dados)
ajuste4 <- glm(diagnostico ~ .,family=binomial(link = (link='cauchit')),data = dados)
summary(ajuste1)
summary(ajuste2)
summary(ajuste3)
summary(ajuste4)


selec <- data.frame(ajuste=c('logito', 'probito', 'cloglog', 'cauchy'),
                    aic=c(AIC(ajuste1), AIC(ajuste2), AIC(ajuste3), AIC(ajuste4)),
                    logLik=c(logLik(ajuste1),logLik(ajuste2),logLik(ajuste3),logLik(ajuste4)))

selec

x11()
envelope=function(modelo){
  dados=na.omit(modelo$data)
  nsim=100
  n=modelo$df.null+1
  r1=sort(rstandard(modelo,type='deviance'))
  m1=matrix(0,nrow=n,ncol=nsim)
  a2=simulate(modelo,nsim=nsim)
  
  for (i in 1:nsim){
    dados$diagnostico=a2[,i]
    aj=update(modelo,diagnostico~.,data=dados)
    m1[,i]=sort(rstandard(aj,type='deviance'))}
  
  li=apply(m1,1,quantile,0.025)
  m=apply(m1,1,quantile,0.5)
  ls=apply(m1,1,quantile,0.975)
  
  quantis=qnorm((1:n-0.5)/n)
  
  plot(rep(quantis,2),c(li,ls),type='n',xlab='Percentil da N(0,1)',ylab='Res�duos')
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
summary(ajuste2)

#OBs: Fazer a analise que Suely fazer em dados categorio
#https://docs.ufpr.br/~giolo/CE073/Material/Respostas_Ex.htm só olha exercico 5

x11()
par(mfrow=c(2,2))
plot(ajuste1, 1:4)


library(car)
x11()
influenceIndexPlot(ajuste2)


library(statmod)
x11()
res <- qresiduals(ajuste1)
plot(res)
residuos <- qresiduals(ajuste1)
qqnorm(residuos)
qqline(residuos, col = 2)
shapiro.test(residuos)


x11()
envelope(ajuste2)

library(effects)
x11()
plot(allEffects(ajuste2), type = 'response')



