#########################################################################
##################          Rerandomização             ##################
#########################################################################

## Distância de Mahalanobis 

mahalanobis2 <- function(x, z) {
  x <- as.matrix(x)
  
  n <- nrow(x)
  n1 <- nrow(x[z == 1,])
  n0 <- n - n1
  
  tau_hat <- apply(x[z == 1,], 2, mean) - apply(x[z == 0,], 2, mean)
  S2x <- sum(x*t(x)) / (n - 1)
  
  M <- t(tau_hat) * solve(n*S2x/(n1*n0)) * tau_hat
  
  return(M)
}

# Teoricamente, M tem distribuição chi-quadrado com K graus de liberdade, onde K = ncol(x).

## PRIV (Percent Reduction in Variance)
  
PRIV <- function(pa, gl) {
  a <- qchisq(pa, gl)
  priv <- 1 - (pchisq(q = a, df = (gl + 2)) / pchisq(q = a, df = gl))
  
  texto <- paste0("P(M < a) = ", round(pa, 4), 
                  ", => a = ", round(a, 4), 
                  ", => priv = ", round(priv * 100, 2), "%")
  
  return(texto)
}

# PRIV(.001, 10)
# PRIV(.0001, 10)

## Análise

# Podemos usar FRT simulando Z sob a restrição de M < a.


#########################################################################
##################        Ajuste de Regressão          ##################
#########################################################################



