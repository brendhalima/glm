library(readr)
harrypotter <- read_csv("D:/Agatha/Favorites/Downloads/harrypotter.csv")
summary(harrypotter)
names(harrypotter)
dados <- harrypotter
names(dados) <- c('Receita','Teatros','Semanas','Filme','Ano','Dias','Recdia','RecAcumada')

plot(dados,pch=20,cex=1.5)

ajuste0 <- lm(Receita~Teatros+Semanas+Filme+Ano+Dias,data = dados)
summary(ajuste0)


ajuste1 <- lm(Receita~Teatros+Ano+Filme,data=dados)
summary(ajuste1) 

ajuste2<-  lm(Receita~Teatros+Filme+Semanas,data=dados)
summary(ajuste2)

ajuste3<- lm(Receita~Teatros,data = dados)
summary(ajuste3)


ajuste3$residuals
residuos=rstandard(ajuste3)
rstudent(ajuste3) 

preditos=ajuste3$fitted.values
plot(preditos,residuos,xlab='Valores ajustados',ylab='Resíduos',pch=20,cex=1.5)

par(mfrow=c(1,2))
hist(residuos)
qqnorm(residuos)
qqline(residuos)
