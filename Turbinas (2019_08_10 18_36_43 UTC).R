### Resultados de um experimento conduzido para avaliar o desempenho de 
### cinco tipos de turb de alta velocidade para motores de avi�o. 
### Foram considerados dez motores para cada de turb e registrado o tempo 
### (em unidades de milh�es de ciclos) at� a perda da velocidade. 
he
require(labestData)
require(MASS)
data(PaulaTb2.1)
help(PaulaTb2.1)

### An�lise descritiva.

with(PaulaTb2.1, boxplot(tempo ~ turb, xlab='Tipo de turbina', 
                       ylab='tempo at� perda de velocidade (milh�es de ciclos)'))

medias <- with(PaulaTb2.1, tapply(tempo,turb,mean)); medias
variancias <- with(PaulaTb2.1, tapply(tempo,turb,var)); variancias
plot(medias, variancias, pch=20, cex=1.5)

cvs <- sqrt(variancias)/medias; cvs # Coeficientes de varia��o.


### Ajuste 0: modelo normal (com liga��o identidade):
ajuste0 <- glm(tempo ~ turb, data = PaulaTb2.1)
par(mfrow = c(2,2))
plot(ajuste0)
hnp(ajuste0)
AIC(ajuste0)

### Ajuste 1: modelo gamma (com liga��o identidade, para compara��o):
ajuste1 <- glm(tempo ~ turb, family = 'Gamma'(link = 'identity'), data = PaulaTb2.1)
x11()
par(mfrow = c(2,2))
plot(ajuste1)
hnp(ajuste1)
AIC(ajuste1)

### Vamos estimar o par�metro de dispers�o.
estimat1 <- ajuste1$deviance/ajuste1$df.residual ; estimat1 
### Baseado na deviance.

estimat2 <- sum(residuals(ajuste1,type='pearson')**2)/ajuste1$df.residual ; estimat2 
### Baseada na estat�stica X2 de Pearson.

estimat3 <- gamma.dispersion(ajuste1) 
estimat3 ### Por m�xima verossimilhan�a.

anova(ajuste1, test='F') ### An�lise de deviance.
### O resultado do teste fornece evid�ncia altamente significativa de 
### diferen�a entre os tempos m�dios de vida das turbinas. 

summary(ajuste1) 
### A turbina II tem tempo m�dio de vida inferior � turbina I. 

### Vamos considerar a hip�tese de que as turbinas podem ser divididas
### em tr�s grupos, conforme os tempos de vida: 
# Turbina II;
# Turbinas I, III e IV;
# Turbina V.

### Vamos ajustar o mesmo MLG sob essa restri��o (hip�tese):

PaulaTb2.1$turb2 <- PaulaTb2.1$turb
levels(PaulaTb2.1$turb2) <- c('T134', 'T2', 'T134', 'T134', 'T5') 
# Turb2 identifica as turbs I, III e IV da mesma forma.

ajuste2 <- glm(tempo ~ turb2, family = 'Gamma'(link = 'identity'), data = PaulaTb2.1)
anova(ajuste2, ajuste1, test='F') 
# A diferen�a das deviances dos dois modelos n�o � significativa. 
# Logo, podemos optar pelo modelo restrito, em que os tempos m�dios de vida
# das turbinas I, III e IV s�o iguais (p=0,5807).

summary(ajuste2)
# Assim, temos a turbina V como aquela com maior tempo m�dio de vida, 
# seguida pelas turbs I, III e IV e a turb II tem menor tempo m�dio de vida.


### Tentando a distribui��o normal inversa.

ajuste3 <- glm(tempo ~ turb2, family = inverse.gaussian(link = 'identity'), data = PaulaTb2.1)
hnp(ajuste3)


### Vamos comparar as tr�s distribui��es usadas (normal, gama e normal inversa)
### com base nos AICs dos respectivos ajustes.

ajuste4 <- glm(tempo ~ turb2, family = gaussian(link = 'identity'), data = PaulaTb2.1)

AIC(ajuste2, ajuste3, ajuste4)

### Os dois modelos (gama e normal inverso) produziram AIC bastante pr�ximos. 
### Ainda assim, o modelo de regress�o gama tem AIC ligeiramente menor e � prefer�vel.