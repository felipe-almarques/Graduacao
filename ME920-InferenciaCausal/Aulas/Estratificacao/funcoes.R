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


#### Neyman ####

neyman_sre <- function(y, z, x) {
  x_levels <- unique(x)
  K <- length(x_levels)
  pi_k <- c()
  tau_k <- c()
  V_k <- c()
  n <- length(z)
  
  for (k in x_levels){
    zk <- z[x == k]
    yk <- y[x == k]
    
    tau_k <- append(tau_k, mean(yk[zk == 1]) - mean(yk[zk == 0]))
    pi_k <- append(pi_k, length(zk)/n)
    V_k <- append(V_k, 
                  (var(yk[zk == 1])/length(yk[zk == 1])) + 
                    (var(yk[zk == 0])/length(yk[zk == 0])))
  }
  
  tauS <- sum(pi_k * tau_k)
  VS <- sum(pi_k^2 * V_k)
  
  return(c(tauS, VS))
}

neyman_sre2 <- function(y, z, x) {
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
