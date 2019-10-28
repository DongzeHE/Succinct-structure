#===============================================================================================================================#
# Create functions for bit-vector


#-----------------------------------------------------------#
# rank_B
# calculate the rank of a given position
#-----------------------------------------------------------#
rank_B <- function(bv,
                   i) {
  return(sum(bv[0:i])) 
}


#-----------------------------------------------------------#
# R_p_lookup
# 
# Finding the rank in Rp according to the pattern and position 
#-----------------------------------------------------------#

R_p_lookup <- function(R_p_rank_table,
                       pattern,
                       pos) {
  which_row = 1
  for (index in length(pattern):1) {
    which_row = which_row + 2^abs(index-length(pattern))*pattern[index]
  }
  return(R_p_rank_table[which_row,pos])
  
}

# bv = sample(0:1,16,replace=T)

rank_support <- function(bv){
  
  # Initialization
  n = length(bv)

  if (n<5) {
    bv[(n+1):5] = 0
    n = 5    

  }
  s_original = (log(n, 2)^2) / 2
  b = ceiling(log(n, 2) / 2)
  s = b*ceiling(s_original/b)

  R_S = c()
  R_b = c()
  
  # Start to build rank_support
  
  ## first for loop for R_S
  for (j in 1:ceiling(n/s)) {
    R_S[j] = rank_B(bv=bv, 
                    i=(j-1)*s
    )
    
  }
  
  
  ## second for loop for R_b
  for (k in 1:ceiling(n/b)) {
    R_b[k] = rank_B(bv=bv, i=(k-1)*b) - rank_B(bv=bv, i=(floor(((k-1)*b)/s)*s))
  }
  
  #Define R_p
  R_p <- expand.grid(replicate(b, 0:1, simplify = FALSE))
  
  for (col in ncol(R_p):1) {
    R_p = R_p[order(R_p[,col]),]
  }
  
  #Define R_p_rank_table
  R_p_rank_table = R_p
  for (row in 1:nrow(R_p)) {
    for (col in 1:ncol(R_p)) {
      R_p_rank_table[row,col] = sum(R_p[row,1:col])
    }
  }
  
  #Gathering results
  return(list('n' = n ,'b' = b ,'s' = s,'bv' =bv, "R_S" = R_S, "R_b" = R_b, "R_p_rank_table" = R_p_rank_table))
}

rank1 <- function(rank_sup, indice){
  # read in data from rank_sup
  n = rank_sup[['n']]
  b = rank_sup[['b']]
  s = rank_sup[['s']]
  bv = rank_sup[['bv']]
  R_S = rank_sup[['R_S']]
  R_b = rank_sup[['R_b']]
  R_p_rank_table = rank_sup[['R_p_rank_table']]
  
  
  # make bv up
  # calculate ranks from all blocks
  ## R_p rank
  R_p_pattern = bv[((ceiling(indice/b)-1)*b+1):(ceiling(indice/b)*b)]
  R_p_pattern[is.na(R_p_pattern)] = 0
  R_p_rank <- R_p_lookup(pattern = R_p_pattern, R_p_rank_table = R_p_rank_table, pos = (indice-(ceiling(indice/b)-1)*b))
  
  ## R_S rank
  R_S_rank <- R_S[ceiling(indice/s)]
  
  ## R_b rank
  R_b_rank <- R_b[ceiling(indice/b)]
  
  return(R_p_rank+R_S_rank+R_b_rank)
}


rank0 <- function(rank_sup, indice){
  return(indice - rank1(rank_sup, indice) )
}

# overhead <- function(rank_sup){
#   bv = rank_sup[["bv"]]
#   n = length(bv)
#   return((2*n/log(n,2))+ (2*n*log(log(n,2),2)) + ((n^(1/2))*log(n,2)*log(log(n/2, 2),2)))
# }

overhead <- function(rank_sup){
  bv = rank_sup[['bv']]
  R_S = rank_sup[['R_S']]
  R_b = rank_sup[['R_b']]
  R_p_rank_table = rank_sup[['R_p_rank_table']]
  return(length(bv)+length(R_S) + length(R_b)+prod(dim(R_p_rank_table)))
}



# 
# select1 <- function(p, r, rnk, rank_sup){
#   # initialize half position.
#   h = ceiling((p+r-1)/2)
# 
#   rank_half <- rank1(rank_sup = rank_sup, indice = h)
#   
#   if (rank_half == rnk) {
#     for (index in h:1) {
#       if (rank_sup[["bv"]][index] == 1) {
#         return(index)
#       }
#     }
#   }else if (rank_half > rnk) {
#     select1(p = p, r = h, rnk = rnk, rank_sup = rank_sup)
#   }else{
#     select1(p = h+1, r = r, rnk = rnk, rank_sup =rank_sup)
#   }
# }
# 
# select0 <- function(p, r, rnk, rank_sup){
#   # initialize half position.
#   h = ceiling((p+r-1)/2)
#   
#   
#   rank_half <- rank0(rank_sup = rank_sup, indice = h)
#   
#   if (rank_half == rnk) {
#     for (index in h:1) {
#       if (rank_sup[["bv"]][index] == 0) {
#         return(index)
#       }
#     }
#   }else if (rank_half > rnk) {
#     select0(p = p, r = h, rnk = rnk, rank_sup = rank_sup)
#   }else{
#     select0(p = h+1, r = r, rnk = rnk, rank_sup = rank_sup)
#   }
# }
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
