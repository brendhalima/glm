########################################################################
########################################################################
########################################################################
### Esse conjunto de scripts tem por finalidade mostrar o impacto de não
### levar em conta o efeito da superdispersão nas inferências de um MLG.

### Vamos usar a base Ornstein, do pacote car. Vamos considerar essa amostra
### como uma população de referência e simular amostras a partir dela.
### O objetivo é estimar o efeito de uma das covariáveis (escolhi, arbitrariamente,
### log (assets)). Os passos são os seguintes:

### 1- Ajustar o MLG com todos os dados e esyimar o parâmetro de interesse
# (vamos considerar essa estimativa como o parâmetro a ser estimado).

### 2 - Seleção aleatória (e sem reposição) de uma amostra de tamanho n=70
# da população de referência;

### 3 - Ajuste do MLG com distribuição Poisson para os dados amostrados e
# obtenção do intervalo de confiança (95%) para o efeito de log(assets);

### 4 - Verificar se o intervalo obtido no passo 3 contém ou não o parâmetro
# de interesse;

### 5 - Repetição dos passos 2-4 por 1000 vezes;

### 6- Estimação da taxa de cobertura do intervalo calculando a proporção de
# ICs que contém o parâmetro de interesse.

### O algoritmo será repetido ajustando, no passo 3, modelos Binomial Negativo,
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

### Simulação - Regressão poisson

set.seed(98)
ICs <- matrix(0, nrow = 1000, ncol = 2)
for (i in 1:1000){
    amostra <- sample(1:nrow(Ornstein), 70)
    Ornstein_Novo <- Ornstein[amostra,]
    ajuste_novo <- glm(interlocks ~ log(assets) + nation + sector, family = poisson, data = Ornstein_Novo)
    ICs[i,] <- confint.default(ajuste_novo)[2,]
}

indica_cobert <- function(interval) ifelse(parametro > interval[1] & parametro < interval[2], 1, 0)
### Função que indica se o intervalo obtido contém (1) ou não (0) o parâmetro.

cobert <- apply(ICs, 1, indica_cobert)
mean(cobert) ### Taxa de cobertura estimada para o modelo Poisson.

########################################################################

### Simulação - Regressão binomial negativa

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

### Simulação - Regressão quase Poisson

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

### Simulação - Regressão Poisson com IC Bootstrap

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
