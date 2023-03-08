# Quantile function

qunifLaplace <- function(u, a = estimates[1], p = estimates[2], sig = estimates[3]){
  fa1 <- (1-p)/2 * exp(-a/sig)
  fa2 <- p + (1-p)/2 *(2-exp(-a/sig))
  
  if(u >= 0 && u < fa1){
    r <- sig*log(2*u/(1-p))
  }else if(u >= fa1 && u < 0.5){
    g1 <- function(x) p*(x+a)/(2*a) + (1-p)/2 * exp(x/sig) - u
    r <- uniroot(g1, c(-a, 0))$root
    
  }else if(u >= 0.5 && u < fa2){
    g2 <- function(x) p*(x+a)/(2*a) + (1-p)/2 * (2 - exp(-x/sig)) - u
    r <- uniroot(g2, c(0, a))$root
    
  }else{
    r <- -sig*log(2*(1-u)/(1-p))
  }
  return(r)
}
