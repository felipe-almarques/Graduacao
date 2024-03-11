################################################################################
###############                      Lista 1                     ###############
################################################################################


### Exercício 2
## b)
library(mvtnorm)
library(tidyverse)

mu <- c(0, 0, 0)
sigma <- matrix(c(1, .6, .4,
                  .6, 1, .2,
                  .4, .2, 1), byrow = T, nrow = 3)

dados <- data.frame(rmvnorm(100, mu, sigma))
colnames(dados) <- c("X", "Y", "Z")

dados %>% 
  ggplot() +
  geom_density(aes(X), fill="blue", alpha = .3) +
  geom_density(aes(Y), fill="red", alpha = .3) +
  geom_density(aes(Z), fill="green", alpha = .3) +
  labs(x = "", y = "Densidade") +
  theme_bw()

pyz <- cor(dados$Y, dados$Z)
pxy <- cor(dados$X, dados$Y)
pzx <- cor(dados$Z, dados$X)

pyz_x <- (pyz - (pxy*pzx)) / (sqrt(1 - pxy^2) * sqrt(1 - pzx^2)) 

pyz # 0.08362862
pyz_x # -0.2000845


### Exercício 3
library(Matching)
library(combinat)

data("lalonde")
str(lalonde)
head(lalonde)

combn(colnames(lalonde)[-c(9, 12)], 2) 

## Criando as combinações
combinacoes <- list()

for (i in 1:10) {
  message(paste0(i, "/10"))
  combinacoes <- append(combinacoes, combn(colnames(lalonde)[-c(9, 12)], i))
}

## Ajustando as regressões


lm(re78 ~ ., data=lalonde)



### Exercpicio 5






