#===============================================================================================================================#
# For this programming task, 
# test your implementation by invoking it for bit vectors of various sizes, 
# and plotting the bit-vector size (say N) versus the time 
# requried to do some fixed number of rank operations. 
# Also, plot the bit-vector size (say N) versus the result of calling the overhead() function.
# Does your implementation match the expected theoretical bounds?
#===============================================================================================================================#

#===============================================================================================================================#

#-----------------------------------------------------------#
# Initialization

## Build bv_list with various size of bit vectors
set.seed(001)
options(digits.secs = 12)
options(digits = 12)

numCores <- detectCores()
n_trail = 100
n_rank = 1000
bv_list = list()
rand_indice_set <- list()
result_receptor <- as.data.frame(matrix(0, nrow = n_trail, ncol = 3))
colnames(result_receptor) <- c("size", "runtime","overhead")

i = 1
for (iter in seq(100,(100*n_trail),100)) {
  bv_list[i] = list(sample(0:1,iter,replace=T))
  result_receptor[i,"size"] <- iter
  rand_indice_set[[i]] <- sample(iter, n_rank,replace=T)
  i = i+1
}


## Loading functions
source("bv_rank.R")


# Start to calculate the runtime for various bit vector size



# library(foreach)
# library(doParallel)
# 
# numCores <- detectCores()
# registerDoParallel(numCores)  # use multicore, set to the number of our cores
# foreach (trail=1:n_trail) %dopar% {
#   rnk_sup <- rank_support(bv_list[[trail]])
#   start.time <- Sys.time()
#   for (index in 1:n_rank) {
#     rand_indice = rand_indice_set[[trail]][index]
#     rank1(rank_sup = rnk_sup, indice = rand_indice)
#   }
#   end.time <- Sys.time()
# 
#   result_receptor[trail,"runtime"] <- end.time - start.time
# }

for(trail in 1:n_trail) {
  rnk_sup <- rank_support(bv_list[[trail]])
  start.time <- Sys.time()
  for (index in 1:n_rank) {
    rand_indice = rand_indice_set[[trail]][index]
    rank1(rank_sup = rnk_sup, indice = rand_indice)
  }
  end.time <- Sys.time()

  result_receptor[trail,"runtime"] <- end.time - start.time
  result_receptor[trail,"overhead"] <- overhead(rank_sup = rnk_sup)
}

plot(result_receptor[,1],result_receptor[,2],ylim = c(0,0.5),xlab = "bit vector size",ylab= "run time", main = "time for 1000 ranking operation")
plot(result_receptor[,1],result_receptor[,3],xlab = "bit vector size",ylab= "overhead", main = "bit bector size VS. overhead")

# aaa <-   system.time({ rank1(rank_sup = rnk_sup, indice = rand_indice) })
# 


# library(foreach)
# library(doParallel)
# 
# registerDoParallel(numCores)  # use multicore, set to the number of our cores
# foreach (trail=1:n_trail) %dopar% {
#   rnk_sup <- rank_support(bv_list[[trail]])
# 
#   result_receptor[trail,"runtime"] = as.numeric(system.time({
#     for (index in 1:n_rank) {
#       rand_indice = rand_indice_set[[trail]][index]
#       rank1(rank_sup = rnk_sup, indice = rand_indice)
#     }
#   })[[1]])
# }














