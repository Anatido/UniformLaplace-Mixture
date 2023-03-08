library(rmutil)

runifLaplace <- function(n, a, p, sigma){
  
  #Sample N random bernoulli Random variables
   B = rbinom(n,1,p)
  
  #Variable to store the samples from the mixture distribution                                             
   rand.samples = rep(NA, n)
   length(rand.samples)
  
  #Sampling from the mixture
  for(i in 1:n){
    if(B[i] == 1){
      rand.samples[i] = runif(1,0,a)
    }else{
      rand.samples[i] = rlaplace(1, 0, sigma)
    }
  }
  
  rand.samples
}


