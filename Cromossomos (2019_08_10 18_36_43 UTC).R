require(faraway)
require(car)
require(multcomp)

data(dicentric)
head(dicentric)
summary(dicentric)
names(dicentric) <- c('Células','Anormais','Dose','Taxalib')


round(xtabs(Anormais/Células ~ Dose+Taxalib,dicentric),2) 
### Taxa de cromossomos anormais segundo dose aplicada e taxa de liberação da dose.

x11()
with(dicentric,interaction.plot(Taxalib, Dose,Anormais/Células)) 
### Representação gráfica da tabela cruzada.


### Vamos tratar a dose administrada como um fator (compararemos as diferenças 
# nas taxas de anormalidade sob as três doses consideradas) 
# e a taxa de liberação como numérico.

dicentric$Dose <- as.factor(dicentric$Dose)


### Vamos ajustar modelos de regressão Poisson considerando a (log) contagem
### de células como termo offset.

### Inicialmente, vamos considerar modelos com efeitos aditivos de dose
### e taxa de liberação (sem interação). Vamos avaliar a melhor forma de
### inserir a taxa de liberação.

### Ajuste 1 - apenas o termo linear.
ajuste1 <- glm(Anormais ~ Dose + Taxalib + offset(log(Células)), family = poisson, data = dicentric) 
summary(ajuste1)

### Ajuste 2: incluindo o termo quadrático.
ajuste2 <- glm(Anormais ~ Dose + Taxalib + I(Taxalib^2) + offset(log(Células)), family = poisson, data = dicentric) 
summary(ajuste2)

### Ajuste 3: considerando o logaritmo da taxa de liberação.
ajuste3 <- glm(Anormais ~ Dose + log(Taxalib) + offset(log(Células)), family = poisson, data = dicentric) 
summary(ajuste3)

### A terceira opção produziu melhor ajuste (repare os valores das deviances e AICs)

### Agora, vamos avaliar se há efeito de interação.
ajuste4 <- glm(Anormais ~ log(Taxalib) * Dose + offset(log(Células)), family = poisson, data = dicentric) 
summary(ajuste4)
anova(ajuste3, ajuste4, test = 'Chisq')


dicentric$logx2 <- log(dicentric$Taxalib)^2
ajuste5 <- glm(Anormais ~ logx2 + log(Taxalib) * Dose + offset(log(Células)), family = poisson, data = dicentric) 
summary(ajuste5)


### O efeito de interação é altamente significativo. Vamos mantê-lo no modelo.

plot(allEffects(ajuste4))
plot(allEffects(ajuste4), type = 'response')

### Vamos estimar se a variação na taxa de cromossomos anormais é diferente
### sob as doses 2.5 e 5.

summary(glht(ajuste4, "log(Taxalib):Dose5 - log(Taxalib):Dose2.5 = 0"))
### A diferença é não significativa.

### Exercício - Realizar o diagnóstico do ajuste com base na análise de 
# resíduos, gráfico qqplot com envelope simulado,... 

### Uma forma de checar a adequação da distribuição Poisson é incorporar 
### o offset ao preditor como covariável, associando a ele um parâmetro a ser estimado,
# e comparar os ajustes (testando beta(células)=1). Pergunta: Por que?

ajuste5 <- glm(Anormais~Dose*log(Taxalib)+log(Células),family=poisson,data=dicentric) 
### Modelo com interação entre dose e taxa de liberação, o (log) número de células
# não entra como offset, mas sim com um parâmetro adicional a ser estimado.

anova(ajuste4, ajuste5, test='Chisq')

### O resultado não significativo para o teste indica que não há evidências 
# contra a hipótese nula (beta(log(células))=1), ou seja, é pertinente incorporar o (log) número de células
# como termo offset. Ponto para a distribuição Poisson!