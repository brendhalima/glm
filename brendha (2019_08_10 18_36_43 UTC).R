library(tidyverse)
da <- read.csv('Breast_cancer_data.csv', sep = ';')

# Começo dos dados
head(da)

# Resumo dos dados
summary(da)

# Proporçao das classes
round(prop.table(table(da$diagnosis)), 2)

# Graficos -------
da %>% 
  ggplot(aes(mean_radius)) +
  geom_histogram(fill = 'orange') +
  facet_wrap(~diagnosis) +
  labs(y = 'Contagem', x = 'Covariavel: raio m�dio',
       title = 'Histogramas por diagn�stico') +
  theme_classic()

da %>% 
  ggplot(aes(y = diagnosis, x = mean_radius)) +
  geom_point(colour = 'orange') +
  labs(y = 'Contagem', x = 'Covariavel: raio médio',
       title = 'Graficos de pontos por diagnóstico') +
  theme_classic()

# Conclusao: os histogramas são diferentes para as duas classes.
# Parece que as pessoas diagnosticada com cancer possuem um
# raio media menor do que as que nao tem a doença (e etc)

# Modelo
modelo <- glm(diagnosis ~ ., data = da, 
              family = binomial(link = 'logit'))

summary(modelo)
# O intercepto nao foi significativo

# Modelo sem o intercepto
modelo_2 <- glm(diagnosis ~ -1 + ., data = da, 
              family = binomial(link = 'logit'))

summary(modelo_2)
# Conclusoes: todas as variaveis sao fortemente significativas

da$predicao <- predict(modelo_2)

da %>% 
  ggplot(aes(y = exp(predicao)/(1+exp(predicao)), x = mean_radius)) +
  geom_point(colour = 'orange') +
  geom_smooth(method="glm", 
              method.args=list(family="binomial"), 
              fullrange=TRUE, se=FALSE)  +
  labs(y = 'Contagem', x = 'Covariavel: raio médio',
       title = 'Graficos de pontos das prediçoes') +
  theme_classic()


da$residuos <- modelo_2$residuals

# Checando os residuos
da %>% 
  filter(residuos > -20) %>% 
  ggplot(aes(residuos)) +
  geom_density(fill = 'orange') + 
  theme_classic()
