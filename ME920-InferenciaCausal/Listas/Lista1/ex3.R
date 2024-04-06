################################################################################
###############                      Lista 1                     ###############
################################################################################

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