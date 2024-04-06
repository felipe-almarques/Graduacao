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

## Construindo as combinações

variaveis <- colnames(lalonde)[-c(9,12)]

combinacoes <- lapply(1:length(variaveis), 
                      function(x) combn(variaveis, x, simplify = F))

combinacoes <- do.call(c, combinacoes)
combinacoes <- append(combinacoes, c(""))

regressao <- function(x) {
  var <- paste(x, collapse = " + ")
  reg <- paste0("re78 ~ ", var, "+ treat")
  lm(as.formula(reg), data=lalonde)
}

modelos <- map(combinacoes, function(x) regressao(x))


## Extraindo coeficientes e p-valores

extrair_coef <- function(modelo) {
  coeficiente <- as.numeric(coef(modelo)["treat"])
  
  dados <- data.frame(nome = names(summary(modelo)$coefficients[, 4]),
                      valor = summary(modelo)$coefficients[, 4])
  pvalor <- dados[dados["nome"] == "treat",2]
  
  return(coeficiente)
}

extrair_pvalor <- function(modelo) {
  dados <- data.frame(nome = names(summary(modelo)$coefficients[, 4]),
                      valor = summary(modelo)$coefficients[, 4])
  pvalor <- dados[dados["nome"] == "treat",2]
  
  return(pvalor)
}

coeficientes <- map(modelos, function(x) extrair_coef(x))
pvalores <- map(modelos, function(x) extrair_pvalor(x))

dados <- data.frame(do.call(rbind.data.frame, coeficientes),
                    do.call(rbind.data.frame, pvalores))
colnames(dados) <- c("coeficiente", "pvalor")


## a)

dados %>% 
  filter(coeficientes > 0 & pvalor < .05) %>% 
  nrow()

# 1024

## b)

dados %>% 
  filter(coeficientes < 0 & pvalor < .05) %>% 
  nrow()

# 0

## c)

dados %>% 
  filter(pvalor >= .05) %>% 
  nrow()

# 0

### Exercício 4
## b)
set.seed(236106)
Y0 <- rnorm(500)
tau <- Y0 - .5
Y1 <- Y0 + tau
Z <- ifelse(tau >= 0, 1, 0)
Y = (Z * Y1) + ((1 - Z) * Y0)

mean(Y[Z == 1]) - mean(Y[Z == 0])
# 2.2591


### Exercicio 5

set.seed(236106)
Y0 <- rnorm(500)
tau <- rnorm(500, -.5)
Y1 <- Y0 + tau

Z <- ifelse(tau >= 0, 1, 0)

Y = (Z * Y1) + ((1 - Z) * Y0)

mean(Y[Z == 1]) - mean(Y[Z == 0]) # 0.81  
sum(Z == 1) ; sum(Z == 0)

# Y(1): 151 ; Y(0) = 349
