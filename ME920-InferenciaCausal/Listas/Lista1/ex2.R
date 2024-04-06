################################################################################
###############                      Lista 1                     ###############
################################################################################

### Exercício 2
## b)
library(mvtnorm)
library(tidyverse)
set.seed(236106)

mu <- c(0, 0, 0)
sigma <- matrix(c(1, .6, .4,
                  .6, 1, .2,
                  .4, .2, 1), byrow = T, nrow = 3)

dados <- data.frame(rmvnorm(1000, mu, sigma))
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

pyz # 0.2344331
pyz_x # -0.016399
