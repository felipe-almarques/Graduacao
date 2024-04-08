############################################################################
#################                  Lista 3                 #################
############################################################################

#### Exercício 3 ####

library(tidyverse)
library(Matching)

data(lalonde)

## Definindo funções

ee <- function(y, z, x) {
  x_levels <- unique(x)
  K <- length(x_levels)
  pi_k <- rep(0, K)
  tau_k <- rep(0, K)
  n <- length(z)
  for (k in 1:K) {
    x_k <- x_levels[k]
    z_k <- z[x == x_k]
    y_k <- y[x == x_k]
    pi_k[k] <- length(z_k)/n
    tau_k[k] <- mean(y_k[z_k == 1]) - mean(y_k[z_k == 0])
  }
  tau_S <- sum(pi_k * tau_k)
  return(tau_S)
}

z_permuta_SRE <- function(z, x) {
  x_levels <- unique(x)
  K <- length(x_levels)
  z_perm <- z
  for (k in 1:K) {
    x_k <- x_levels[k]
    z_perm[x == x_k] <- sample(z[x == x_k])
  }
  return(z_perm)
}

frt_pvalor <- function(y, z, x, ee_obs, mc=10^4){
  estatisticas <- matrix(NA, ncol = 1, nrow = mc)
  for (i in 1:mc) {
    message(paste0(i, " / ", mc))
    z_perm <- z_permuta_SRE(z, x)
    estatisticas[i] <- ee(y, z_perm, x)
  }
  
  resultado <- list(est = estatisticas, 
                    pvalor = mean(abs(ee_obs) < abs(estatisticas)))
  return(resultado)
}

neyman_sre <- function(y, z, x) {
  x_levels <- unique(x)
  K <- length(x_levels)
  pi_k <- rep(0, K)
  tau_k <- rep(0, K)
  v_k <- rep(0, K)
  n <- length(z)
  for (k in 1:K) {
    x_k <- x_levels[k]
    z_k <- z[x == x_k]
    y_k <- y[x == x_k]
    pi_k[k] <- length(z_k)/n
    tau_k[k] <- mean(y_k[z_k == 1]) - mean(y_k[z_k == 0])
    v_k[k] <- var(y_k[z_k == 1]) / sum(z_k) + var(y_k[z_k == 0]) / sum(1 - z_k)
  }
  tau_s <- sum(pi_k * tau_k)
  v_s <- sum(pi_k^2 * v_k) 
  return(c(tau_s, v_s))
}


### Caso 1: race (black, hisp, other)como estrato.
dados <- lalonde %>% 
  mutate(race = ifelse(black == 1, 1, 
                       ifelse(hisp == 1, 2, 3)))

## Fisher
tauS <- ee(dados$re78, dados$treat, dados$race)
frt <- frt_pvalor(dados$re78, dados$treat, dados$race, tauS)

hist(frt$est) # centrada em zero e aproximadamente simétrica
frt$pvalor # 0.0044, rejeitamos H0

## Neyman
neyman <- neyman_sre(dados$re78, dados$treat, dados$race)
est <- neyman[1] / sqrt(neyman[2]) # 2.65558
2*pnorm(abs(est), lower.tail = F) # p-valor: 0.007917226, rejeita H0


### Caso 2: marital status (married) como estrato.
dados <- lalonde %>% 
  mutate(married = married + 1)

## Fisher
tauS <- ee(dados$re78, dados$treat, dados$married)
frt <- frt_pvalor(dados$re78, dados$treat, dados$married, tauS)

hist(frt$est) # centrado em 0 e aprox simétrico 
frt$pvalor # p-valor: 0.0041, rejeitamos H0

## Neyman
neyman <- neyman_sre(dados$re78, dados$treat, dados$married)
est <- neyman[1] / sqrt(neyman[2]) # 2.636411
2*pnorm(abs(est), lower.tail = F) # p-valor: 0.008378817, rejeita H0

### Caso 3: high school diploma (nodegr) como estrato.
dados <- lalonde %>% 
  mutate(nodegr = nodegr + 1)

## Fisher
tauS <- ee(dados$re78, dados$treat, dados$nodegr)
frt <- frt_pvalor(dados$re78, dados$treat, dados$nodegr, tauS)

hist(frt$est) # centrado em 0 e aprox simétrico 
frt$pvalor # p-valor: 0.0113, rejeitamos H0

## Neyman
neyman <- neyman_sre(dados$re78, dados$treat, dados$nodegr)
est <- neyman[1] / sqrt(neyman[2]) # 2.396085
2*pnorm(abs(est), lower.tail = F) # p-valor: 0.01657124, rejeita H0

### Conclusão

# Mesmo estratificando em cada uma das variáveis, a conclusão não se alterou.
# Em todos os casos rejeitamos a hipótese nula de que o efeito causal individual (Fisher) ou efeito causal médio (Neyman) seja nulo.