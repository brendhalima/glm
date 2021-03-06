########################################################################
########################################################################
########################################################################
### Esse conjunto de scripts tem por finalidade mostrar o impacto de n�o
### levar em conta o efeito da superdispers�o nas infer�ncias de um MLG.

### Vamos usar a base Ornstein, do pacote car. Vamos considerar essa amostra
### como uma popula��o de refer�ncia e simular amostras a partir dela.
### O objetivo � estimar o efeito de uma das covari�veis (escolhi, arbitrariamente,
### log (assets)). Os passos s�o os seguintes:

### 1- Ajustar o MLG com todos os dados e esyimar o par�metro de interesse
# (vamos considerar essa estimativa como o par�metro a ser estimado).

### 2 - Sele��o aleat�ria (e sem reposi��o) de uma amostra de tamanho n=70
# da popula��o de refer�ncia;

### 3 - Ajuste do MLG com distribui��o Poisson para os dados amostrados e
# obten��o do intervalo de confian�a (95%) para o efeito de log(assets);

### 4 - Verificar se o intervalo obtido no passo 3 cont�m ou n�o o par�metro
# de interesse;

### 5 - Repeti��o dos passos 2-4 por 1000 vezes;

### 6- Estima��o da taxa de cobertura do intervalo calculando a propor��o de
# ICs que cont�m o par�metro de interesse.

### O algoritmo ser� repetido ajustando, no passo 3, modelos Binomial Negativo,
# quasi poisson e poisson com IC bootstrap.


require(car) 
require(MASS)
require(lme4)
help("Ornstein")
data(Ornstein)
head(Ornstein) 
summary(Ornstein) 

ajuste1 <- glm(interlocks ~ log(assets) + nation + sector, family = poisson, data = Ornstein)
summary(ajuste1) 
parametro <- coef(ajuste1)[2]
parametro

########################################################################

### Simula��o - Regress�o poisson

set.seed(98)
ICs <- matrix(0, nrow = 1000, ncol = 2)
for (i in 1:1000){
    amostra <- sample(1:nrow(Ornstein), 70)
    Ornstein_Novo <- Ornstein[amostra,]
    ajuste_novo <- glm(interlocks ~ log(assets) + nation + sector, family = poisson, data = Ornstein_Novo)
    ICs[i,] <- confint.default(ajuste_novo)[2,]
}

indica_cobert <- function(interval) ifelse(parametro > interval[1] & parametro < interval[2], 1, 0)
### Fun��o que indica se o intervalo obtido cont�m (1) ou n�o (0) o par�metro.

cobert <- apply(ICs, 1, indica_cobert)
mean(cobert) ### Taxa de cobertura estimada para o modelo Poisson.

########################################################################

### Simula��o - Regress�o binomial negativa

ICs <- matrix(0, nrow = 1000, ncol = 2)
for (i in 1:1000){
    amostra <- sample(1:nrow(Ornstein), 70)
    Ornstein_Novo <- Ornstein[amostra,]
    ajuste_novo <- glm.nb(interlocks ~ log(assets) + nation + sector, data = Ornstein_Novo)
    ICs[i,] <- confint.default(ajuste_novo)[2,]
}

cobert <- apply(ICs, 1, indica_cobert)
mean(cobert) ### Taxa de cobertura estimada para o modelo binomial negativo.

########################################################################

### Simula��o - Regress�o quase Poisson

ICs <- matrix(0, nrow = 1000, ncol = 2)
for (i in 1:1000){
    amostra <- sample(1:nrow(Ornstein), 70)
    Ornstein_Novo <- Ornstein[amostra,]
    ajuste_novo <- glm(interlocks ~ log(assets) + nation + sector, family = quasipoisson, data = Ornstein_Novo)
    ICs[i,] <- confint.default(ajuste_novo)[2,]
}

cobert <- apply(ICs, 1, indica_cobert)
mean(cobert) ### Taxa de cobertura estimada para o modelo quase Poisson.

########################################################################

### Simula��o - Regress�o Poisson com IC Bootstrap

ICs <- matrix(0, nrow = 1000, ncol = 2)
for (i in 1:1000){
    amostra <- sample(1:nrow(Ornstein), 70)
    Ornstein_Novo <- Ornstein[amostra,]
    ajuste_novo <- glm(interlocks ~ log(assets) + nation + sector, family = poisson, data = Ornstein_Novo)
    boot_pois <- Boot(ajuste_novo)
    ICs[i,] <- confint(boot_pois, type = 'perc')[2,]
}

cobert <- apply(ICs, 1, indica_cobert)
mean(cobert) ### Taxa de cobertura = 0.98.
