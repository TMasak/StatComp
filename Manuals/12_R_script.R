my_sim <- function(seed,verbose=F){
  set.seed(517*seed)
  ### calculation of whatever we want: here timing matrix-matrix multiplication
  K <- 10^3
  A <- array(runif(K^2),c(K,K))
  B <- array(runif(K^2),c(K,K))
  t <- Sys.time()
  Res <- A %*% B
  t <- Sys.time()-t
  return(t)
}

library(parallel)
Res <- mclapply(1:35,my_sim,mc.cores=35)
# data <- lapply(1:25,simulate) # comment the previous line an uncomment this one if not running on a cluster
save(Res, file="1_james.RData")
