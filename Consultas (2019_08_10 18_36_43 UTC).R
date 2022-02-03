########################################################################
########################################################################
### Modelos para dados com excesso de zeros.

require("faraway")
require("gamlss")
help(dvisits)

### Vamos modelar o número de visitas ao médico nas duas últimas semanas 
### (doctorco) em função de algumas das covariáveis da base.

dvisits <- dvisits[sample(1:nrow(dvisits)),]
### Apenas ordenando as linhas da base de forma aleatória (estavam dispostas
### de forma sistemática).

dvisits$sex <- as.factor(dvisits$sex)
levels(dvisits$sex) <- c('Masculino', 'Feminino')
dvisits$levyplus <- factor(dvisits$levyplus)
levels(dvisits$levyplus) <- c('Publico', 'Particular')
dvisits$chcond1 <- factor(dvisits$chcond1)
levels(dvisits$chcond1) <- c('Nao cronico', 'Cronico')
### Convertendo algumas das variáveis para fator e renomeando os níveis.

tabela <- table(dvisits$doctorco)
barplot(tabela)
### O gráfico de barras evidencia uma grande freqência de indivíduos com 
### zero consultas.

### Todos os ajustes na sequência foram realizados usando a biblioteca
### gamlss (mesmo aqueles que poderiam ser feitos usando a função glm, 
### para fins de padronização).

### Vamos avaliar os ajustes de seis modelos distintos:

### Modelo 1: Poisson (sem inflação de zeros)

modelo1 <- gamlss(doctorco ~ sex + age + agesq 
                  + income + levyplus + illness + actdays + chcond1 + hospadmi 
                  + medicine, family = PO, data = dvisits)
plot(modelo1) 
### A biblioteca gamlss tem como padrão os resíduos quantílicos aleatorizados.
### O qqplot indica a não normalidade dos resíduos e, consequentemente,
### o desajuste do modelo.

### Modelo 2: Binomial negativa (sem inflação de zeros)
modelo2 <- gamlss(doctorco ~ sex + age + agesq + income + levyplus + illness + 
                      actdays + chcond1 + hospadmi + medicine, family = NBI, 
                  data = dvisits)
plot(modelo2)
### A análise de resíduos indica melhor ajuste em relação ao modelo de Poisson.
### Mas ainda se observa algum desajuste.

### Modelo 3: Poisson com inflação de zeros (mas sem covariáveis para o componente
### do excesso de zeros)

modelo3 <- gamlss(doctorco ~ sex + age + agesq + income + levyplus + illness + 
                      actdays + chcond1 + hospadmi + medicine,
                  family = ZIP, data = dvisits) 
plot(modelo3)
### Problemas de ajuste semelhantes ao modelo de Poisson sem inflação.

### Modelo 4: Binomial negativa com inflação de zeros (mas sem covariáveis para o componente
### do excesso de zeros)

modelo4 <- gamlss(doctorco ~ sex + age + agesq + income + levyplus + illness + 
                      actdays + chcond1 + hospadmi + medicine,
                  family = ZINBI, data = dvisits) 
plot(modelo4)

### Modelo 5: Poisson com inflação de zeros (incluindo as covariáveis também na modelagem 
### do componente do excesso de zeros)

modelo5 <- gamlss(doctorco ~ sex + age + agesq + income + levyplus + illness + 
                      actdays + chcond1 + hospadmi + medicine,
                     sigma.formula = ~ sex + age + agesq + income + levyplus + 
                      illness + actdays + chcond1 + hospadmi + medicine, 
                     family = ZIP, data = dvisits) 
plot(modelo5)
### Observe que para incluir covariáveis na modelagem do parâmetro do excesso
### de zeros, tivemos que declarar uma segunda fórmula em "sigma.formula = ~ ..."

### Modelo 6: Binomial negativa com inflação de zeros (incluindo as covariáveis 
### também na modelagem do componente do excesso de zeros)

modelo6 <- gamlss(doctorco ~ sex + age + agesq + income + levyplus + illness + 
                      actdays + chcond1 + hospadmi + medicine,
                  nu.formula = ~ sex + age + agesq + income + levyplus + 
                      illness + actdays + chcond1 + hospadmi + medicine,
                  family = ZINBI, data = dvisits) 
plot(modelo6)
### No caso da binomial negativa, para incluir covariáveis na modelagem do parâmetro do excesso
### de zeros, tivemos que declarar uma segunda fórmula em "nu.formula = ~ ...".
### Isso porque, para a ZINB, o gamlss identifica sigma como o parâmetro de dispersão.

### Vamos comparar os modelos pelos respectivos AICs.

AIC(modelo1, modelo2, modelo3, modelo4, modelo5, modelo6)

### Observe que o modelo 6 (ZIBN com covariáveis para o excesso de zeros)
### foi aquele que produziu menor AIC. Pelos gráficos de resíduos, adicionalmente,
### fica claro que é o que proporciona melhor ajuste. Vamos explorar
### um pouco mais ele.

summary(modelo6)

### Observe que o resumo do modelo é separado em duas partes: a primeira para
### a média da binomial negativa e a segunda para a probabilidade de zeros excedentes.

### Podemos observar que a probabilidade associada ao excesso de zeros é
### menor para pacientes do sexo feminino; diminui conforme o número de enfermidades
### nas últimas duas semanas (illness); diminui conforme o número de dias inativos 
### nas últimas duas semanas (actdays); diminui conforme o número de medicações
### prescritas nas últimas duas semanas (medicine).

### Ao olhar para a parte da binomial negativa, verificamos que a frequência de consultas,
### em média, aumenta conforme o número de dias inativos (actdays);
### aumenta conforme o número de noites internado (hospadmi) e aumenta
### conforme o número de medicações prescritas (medicine).

### Deixo como tarefa escrever as expressões dos modelos ajustados e explorar os efeitos.

### Apenas para fins de ilustração, vamos ajustar um modelo apenas com as variáveis
### com efeito marginalmente significativo ao nível de 5% (além da idade, para o
### componente do excesso de zeros).

modelo7 <- gamlss(doctorco ~ actdays + hospadmi + medicine,
                  nu.formula = ~ sex + age + agesq +illness + actdays + medicine,
                  family = ZINBI, data = dvisits)

LR.test(modelo7, modelo6) ### Comparando os modelos via teste da razão de verossimilhanças.
### Os ajustes não diferem significativamente. Podemos adotar o modelo 7, portanto.

plot(modelo7)
summary(modelo7)
### Embora as estimativas, numericamente, tenham sido alteradas, as conclusões
### apresentadas anteriormente se mantém.

confint(modelo7, what = 'mu')
confint(modelo7, what = 'nu')
### Intervalos de confiança.

pmu <- predict(modelo7, what = 'mu', type = 'response')
### Valores ajustados para a média da binomial negativa para cada linha da base.

pnu <- predict(modelo7, what = 'nu', type = 'response')
### Valores ajustados para a probabilidade de zero excedente para cada linha da base.

pmu * (1-pnu)
### Valores ajustados para o número esperado de consultas para cada linha da base.