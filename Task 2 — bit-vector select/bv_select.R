source("bv_rank.r")

select_support <- function(bv){
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
  return(list('n' = n ,'b' = b ,'s' = s, 'p' = 1, 'r' = n, 'bv' =bv, "R_S" = R_S, "R_b" = R_b, "R_p_rank_table" = R_p_rank_table))
}

select1 <- function(rnk, slct_sup){
  # initialize half position.
  p = slct_sup[["p"]]
  r = slct_sup[["r"]]
  h = ceiling((p+r-1)/2)
  
  rank_half <- rank1(rank_sup = slct_sup, indice = h)
  
  if (rank_half == rnk) {
    for (index in h:1) {
      if (slct_sup[["bv"]][index] == 1) {
        return(index)
      }
    }
  }else if (rank_half > rnk) {
    # slct_sup[["p"]] = p
    slct_sup[["r"]] = h
    select1(rnk = rnk, slct_sup = slct_sup)
  }else{
    slct_sup[["p"]] = h+1
    # slct_sup[["r"]] = r
    select1(rnk = rnk, slct_sup =slct_sup)
  }
}

select0 <- function(rnk, slct_sup){
  # initialize half position.
  p = slct_sup[["p"]]
  r = slct_sup[["r"]]
  h = ceiling((p+r-1)/2)
  
  rank_half <- rank0(rank_sup = slct_sup, indice = h)
  
  if (rank_half == rnk) {
    for (index in h:1) {
      if (slct_sup[["bv"]][index] == 0) {
        return(index)
      }
    }
  }else if (rank_half > rnk) {
    # slct_sup[["p"]] = p
    slct_sup[["r"]] = h
    select0(rnk = rnk, slct_sup = slct_sup)
  }else{
    slct_sup[["p"]] = h+1
    # slct_sup[["r"]] = r
    select0(rnk = rnk, slct_sup =slct_sup)
  }
}








