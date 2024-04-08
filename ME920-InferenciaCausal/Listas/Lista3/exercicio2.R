############################################################################
#################                  Lista 3                 #################
############################################################################

## Exercício 2
library(tidyverse)

# dados
dados <- read.csv("multicenter.csv")
glimpse(dados)


# Temos interesse em testar o efeito causal médio (para por em prática o uso do medicamento) e então aplicaremos Neynan.

#### Neyman ####
n01 <- sum(dados$n0 + dados$n1)
n05 <- sum(dados$n0 + dados$n5)

# Construindo as estatísticas
dados <- dados %>% 
  mutate(tau1_k = mean1 - mean0, tau5_k = mean5 - mean0, 
         pi1_k = (n0 + n1)/n01, pi5_k = (n0 + n5)/n05,
         V1_k = (sd1^2/n1) + (sd0^2/n0),
         V5_k = (sd5^2/n5) + (sd0^2/n0) ) %>% 
  mutate(tauS1_k = pi1_k * tau1_k, tauS5_k = pi5_k * tau5_k,
         VS1_k = pi1_k^2 * V1_k, VS5_k = pi5_k^2 * V5_k)


tauS1 <- sum(dados$tauS1_k) ; tauS5 <- sum(dados$tauS5_k)
VS1 <- sum(dados$VS1_k) ; VS5 <- sum(dados$VS5_k)

##testando as hipóteses
est1 <- tauS1 / sqrt(VS1) ; est5 <- tauS5 / sqrt(VS5)

# Caso 1: finasterida 1mg vs control
c(tauS1 - 1.96*sqrt(VS1), tauS1 + 1.96*sqrt(VS1)) # (-1.468874, 0.154194)
2*pnorm(abs(est1), lower.tail = F) # p-valor = 0.1123782

# Caso 1: finasterida 1mg vs control
c(tauS5 - 1.96*sqrt(VS5), tauS5 + 1.96*sqrt(VS5)) # (-2.42595 -0.85554)
2*pnorm(abs(est5), lower.tail = F) # p-valor = 4.211066e-05



