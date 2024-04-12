################################################################################
##########                           Lista 2                          ##########
################################################################################

library(vcdExtra)

### Exercício 1
dados <- matrix(c(7, 11, 7, 4, 1, 1, 5, 7, 7, 10), nrow = 2, byrow = T)
colnames(dados) <- c("0", "1", "2", "3", "4")
rownames(dados) <- c("Placebo", "Droga")

tabela <- as.table(dados)

chisq.test(tabela) # X-squared = 14.932 / p-value = 0.004845
CMHtest(tabela) # Row mean scores differ  / 14.523 / 0.00013844

### Exercício 2
## a)
dados <- data.frame(
  Centro = c(rep("1", 30), rep("2", 34), rep("3", 71)),
  Tratamento = c(rep("A", 15), rep("B", 15), rep("A", 17), rep("B", 17),
                 rep("A", 36), rep("B", 35)),
  Resposta = c(rep("Ruim", 6), rep("Razoável", 6), rep("Muito Bom", 3),
               rep("Ruim", 2), rep("Razoável", 7), rep("Muito Bom", 6),
               rep("Ruim", 4), rep("Razoável", 7), rep("Muito Bom", 6),
               rep("Ruim", 2), rep("Razoável", 4), rep("Muito Bom", 11),
               rep("Ruim", 11), rep("Razoável", 19), rep("Muito Bom", 6),
               rep("Ruim", 6), rep("Razoável", 12), rep("Muito Bom", 17)))

mantelhaen.test(dados$Tratamento, dados$Resposta, dados$Centro)
# Cochran-Mantel-Haenszel M^2 = 12.747, df = 2, p-value = 0.001706

## b)
mantelhaen.test(dados$Centro, dados$Resposta, dados$Tratamento)
# Cochran-Mantel-Haenszel M^2 = 4.0581, df = 4, p-value = 0.3982

### Exercício 3
dados <- data.frame(
  Residencia = c(rep("Rural", 300), rep("Urbana", 300)),
  Stress = c(rep("Baixo", 72), rep("Alto", 228), 
             rep("Baixo", 228), rep("Alto", 72)),
  Atitude = c(rep("Não Favorável", 14), rep("Favorável", 58),
              rep("Não Favorável", 112), rep("Favorável", 116),
              rep("Não Favorável", 162), rep("Favorável", 66),
              rep("Não Favorável", 64), rep("Favorável", 8)))
dados$Residencia <- factor(dados$Residencia)
dados$Stress <- factor(dados$Stress)
dados$Atitude <- factor(dados$Atitude)

## a)
tab <- matrix(c(176, 124, 176, 124), nrow = 2, byrow = T)
colnames(tab) <- c("Não Favorável", "Favorável") 
rownames(tab) <- c("Baixo", "Alto")
tabela <- as.table(tab)

(c(352, 248) * 300) / 600

chisq.test(tabela)

## b)
# Rural
tab <- matrix(c(14, 58, 112, 116), nrow = 2, byrow = T)
colnames(tab) <- c("Não Favorável", "Favorável") 
rownames(tab) <- c("Baixo", "Alto")
tabela <- as.table(tab)

c(126, 174) * 72 / 300
c(126, 174) * 228 / 300

chisq.test(tabela)

# Urbano
tab <- matrix(c(162, 66, 64, 8), nrow = 2, byrow = T)
colnames(tab) <- c("Não Favorável", "Favorável") 
rownames(tab) <- c("Baixo", "Alto")
tabela <- as.table(tab)

c(226, 74) * 228 / 300
c(226, 74) * 72 / 300

chisq.test(tabela)

## c)
mantelhaen.test(dados$Stress, dados$Atitude, dados$Residencia)

## d)
tab <- matrix(c(174, 74, 126, 226), nrow = 2, byrow = T)
colnames(tab) <- c("Rural", "Urbano") 
rownames(tab) <- c("Favorável", "Não Favorável")
tabela <- as.table(tab)

chisq.test(tabela)$expected
chisq.test(tabela)

tab <- matrix(c(72, 228, 228, 72), nrow = 2, byrow = T)
colnames(tab) <- c("Rural", "Urbano") 
rownames(tab) <- c("Baixo", "Alto")
tabela <- as.table(tab)

chisq.test(tabela)$expected
chisq.test(tabela)
