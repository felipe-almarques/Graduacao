#########################################################################
###############                 Estratificação            ###############
#########################################################################


#### FRT ####

tauS_hat <- function(y, z, x, estimador="ee") {
  x_levels <- unique(x)
  K <- length(x_levels)
  pi <- c()
  tau <- c()
  ck <- c()
  Vs <- c()
  
  for (k in x_levels) {
    zk <- z[x == x_levels[k]]
    yk <- y[x == x_levels[k]]
    
    # estatísticas 
    tau <- append(tau, mean(yk[zk == 1]) - mean(yk[zk == 0]))
    Wk <- wilcox.test(yk[zk == 1], yk[zk == 0])$statistic
    
    # pesos
    pi <- append(pi, length(zk) / length(z))
    ck <- append(ck, (1/(length(zk) + 1)))
    nk1 <- length(zk == 1) ; nk0 <- length(zk == 0)
    Vs <- (var(yk[zk == 1]) / nk1) + (var(yk[zk == 0]) / nk0)
  }
  
  # Variância de tau_hat
  V_tauS <- sum(pi^2 * Vs)
  tauS <- sum(pi * tau)
  
  if (estimador == "ee") {
    return(c(tauS, V_tauS))
  }
  
  if (estimador == "wilcoxon") {
    Ws <- sum(ck * Wk)
    return(Ws)
  }
  if (estimador == "std") {
    tS <- tauS / sqrt(V_tauS)
    return(tS)
  }
  
}