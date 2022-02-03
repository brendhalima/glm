

### Conjunto de dados da AI for Social Good: 
### Women Coders' Bootcamp (Centro de treinamento de mulheres programadoras)
### organizado por Inteligência Artificial para desenvolvimento em colaboração com a 
### UNDP Nepal (Programa de Desenvolvimento das Nações Unidas - Nepal)


### Descrição -----------------------------------------------------------------------------------------
### Em torno do mundo, câncer de mama é o tipo mais comum de câncer em mulheres e é o segundo maior em 
### termos de taxas de mortalidade. O diagnóstico do câncer de mama é obtido quando um caroço anormal
### é encontrado (por auto exame ou raio-x) ou quando um minúsculo grão de cálcio é encontrado (raio-x).
### Depois que o caroço suspeito é encontrado, o doutor vai conduzir um diagnóstico para determinar
### se é cancerígeno, e se for, se ele se espalhou para outras partes do corpo.
### Este conjunto de dados foi obtido da University of Wisconsin Hospitals, em Madison através do
### Dr. William H. Wolberg.


### Outras referências do problema: https://ai.google/social-good
### DESCRIÇÃO COMPLETA https://www.rdocumentation.org/packages/TH.data/versions/1.0-9/topics/wpbc



### Variável resposta da base --------------------------------------------------------------------------
### - Diagnóstico do tecido da mama (1 nódulo maligno e 0 nódulo benigno)

### Variáveis explicativas da base ---------------------------------------------------------------------
### - mean_radius: raio médio da distância do centro ao perímetro
### - mean_texture: textura média, irregulareidades (standard deviation of gray-scale values = 
### ANÁLISE DE TEXTURA ESTATÍSTICA
### http://www.lcad.icmc.usp.br/~jbatista/procimg/2012/textura)
### - mean_perimeter: perímetro médio do tumor
### - mean_area: área média
### - mean_smoothness: regularidade média (mean of local variation in radius lengths)


### Início da análise ----------------------------------------------------------------------------------

library(readxl)
dados <- read_excel("~/Desktop/UFPR/Modelos Lineares Generalizados/trabalho 1 2018/Breast_cancer_data.xlsm")

names(dados) <- c("raio","textura","perimetro","area","regularidade","diagnostico")

dados$diagnostico <- factor(dados$diagnostico)
summary(dados)

### Exercício - Fazer uma análise descritiva (uni e bivariada), buscando avaliar, de forma preliminar 
### por meio de gráficos e medidas resumo, a relação entre as covariáveis e a resposta.

plot(dados$"raio",dados$"diagnostico")
boxplot(dados)

### Vamos separar, aleatoriamente, a base em duas: uma para o ajuste do modelo (com 4000 observações) e outra para validação (com 1000 observações).
sorteio=sample(1:5000) ### sorteio é um vetor com números de 1 a 5000 numa sequência aleatória.
dadosajuste=dados[sorteio[1:4000],] ### dataframe com as quatro mil linhas para ajuste. 
dadosvalid=dados[sorteio[4001:5000],] ### dataframe com mil linhas, apenas para validação.
ajuste=glm(diagnostico~raio+textura+perimetro+area+regularidade,family=binomial,data=dadosajuste) ### Ajuste do modelo de regressão logística.
### Antes de mais nada, é fundamental saber como a variável resposta está sendo modelada (basicamente, a qual categoria da resposta o R associa valor zero
### e a qual o R associa valor um. Para isso, vamos extrair do glm ajustado o objeto y:



#códigos do trab da agatha
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
  
  plot(rep(quantis,2),c(li,ls),type='n',xlab='Percentil da N(0,1)',ylab='Resíduos')
  title('GrÃ¡fico Normal de Probabilidades')
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
#######Apesar de da funÃ§Ã£o probito ter dado menor AIC, graficamente a melhor e logito, entÃ£o vamos usar a funÃ§Ã£o logito

summary(ajuste1)
summary(ajuste2)

#OBs: Fazer a analise que Suely fazer em dados categorio
#https://docs.ufpr.br/~giolo/CE073/Material/Respostas_Ex.htm sÃ³ olha exercico 5

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



