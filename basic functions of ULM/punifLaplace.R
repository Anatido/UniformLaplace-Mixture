library(rmutil)
punifLaplace <- function(x, a, sigma) p*dunif(x,-a,a) + (1-p)*plaplace(x, 0, sigma)
punifLaplace(5, 3, 4)
