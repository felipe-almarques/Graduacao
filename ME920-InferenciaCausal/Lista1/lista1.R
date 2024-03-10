################################################################################
###############                      Lista 1                     ###############
################################################################################


### Exercício 2
## b)


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





