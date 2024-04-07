############################################################################
#################                  Lista 3                 #################
############################################################################

## Pacotes
library(tidyverse)
library(car)

## Dados
dados <- read.table("Penn46_ascii.txt")
dados <- dados %>% mutate(Y = log(duration))


## a) 
x_levels <- unique(dados$quarter)
tau_L <- c()
pi <- c()
VarL <- c()

for (k in x_levels) {
  dados_k <- dados[dados$quarter == k,]
  modelo <- lm(Y ~ treatment * ., data = dados_k)
  tau_L <- append(tau_L, coef(modelo)[2])
  pi <- append(pi, nrow(dados_k) / nrow(dados))
  VarL <- append(VarL, hccm(modelo, type = "hc2")[2,2])
}

tau_SL <- sum(pi * tau_L)
tau_SL # -0.02931918

VarSL <- sum(pi^2 * VarL)
VarSL # 0.00126735