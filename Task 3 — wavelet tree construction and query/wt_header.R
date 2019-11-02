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



dec2bin <- function(Tn, index, MSB){
  return(c(as.integer(rev(intToBits(Tn[index]))))[(32-MSB+1):32])
}

bin2dec <- function(x){
  sum(2^(which(rev(x == 1))-1))
  }

Bit <- function(l, i, Tb){
  return(Tb[l,i])
}

prefix <- function(l, i, Tb){
  bi <- Tb[1:l,i]
  dec <- 1
  i = 1
  for (index in length(bi):1 ) {
    dec = dec + 2^(index-1)*bi[i]
    i = i+1
  }
  return(dec)
}

wt_build <- function(Tc){
  #-----------------------------------------------------------#
  # initialization
  ## Transform Tc from string to vector and calculate alphabet
  # Tc = strsplit(Tc, split = "")[[1]]
  alphabet = sort(unique(Tc))
  
  ## Tn is the numeric version of Tc
  Tn = Tc
  for (index in 1:length(Tc)) {
    Tn[index] <- which(alphabet == Tc[index]) 
  }
  Tn = as.numeric(Tn) -1
  
  ## Initialize the arguments used in the process
  MSB <- ceiling(log(max(Tn),2))
  n = length(Tn)
  
  ## Tb is the binary version of Tc
  Tb <- matrix(0,nrow = MSB, ncol = n)
  for (col in 1:n) {
    Tb[,col] <-  dec2bin(Tn = Tn,index =  col, MSB = MSB)
  }
  
  ## Initialize wt to Tb, becuase they have the same structure
  wt = Tb
  
  
  #-----------------------------------------------------------#
  # Builing Wavelet Tree
  ## First calculate Histogram, this is the first loop in the psudocode.
  SPos_list <-  list(c(1, n+1))
  hst <- hist(Tn,right = FALSE,breaks = 2*length(Tn),plot = FALSE)$counts
  Hist <- hst[hst!=0]
  Hist[(length(alphabet)+1):2^MSB] = 0
  
  
  ## Second, Using initialied histogram and the first row of wt to build the rest
  
  ### Outer loop is for each level in the wavelet tree, starting from the last level, which is the LSB
  for (l in MSB:2) {
    
    #### First inner loop is for updating histogram, Hist[i] = Hist[2i] + Hist[2i + 1]
    for (i in 0:((2^(l-1)-1))) {
      Hist[i+1] = Hist[2*i+1] + Hist[(2*i+2)]
    }
    
    #### Second inner loop is for start position of the node, which is the previous position + the count of the length of the previous node.
    SPos <- c(rep(0,2^(l-1)))
    for (i in 2:(2^(l-1))) {
      SPos[i] <- SPos[i-1]+Hist[i-1]
    }
    SPos = c(SPos, n)
    SPos = SPos+1
    SPos_list[[l]] <- unique(SPos) 
    #### Third inner loop is for updating wavelet tree in the current level, from the 
    for (i in 1:n) {
      pos = SPos[prefix(l = l-1,i = i,Tb = Tb)]
      SPos[prefix(l = l-1,i = i,Tb = Tb)] = SPos[prefix(l = l-1,i = i,Tb = Tb)] + 1

      wt[l,pos] <- Bit(l,i,Tb)
      i = i+1
    }
  }
  waveletTree <- list()
  waveletTree[['Tc']] <- Tc
  waveletTree[['Tn']] <- Tn
  waveletTree[['wt']] <- wt
  waveletTree[['MSB']] <- MSB
  waveletTree[['SPos_list']] <- SPos_list
  waveletTree[['alphabet']] <- alphabet
  
  return(waveletTree)
}


wt_access <- function(waveletTree,indice){
  wt <- waveletTree[['wt']]
  MSB <- waveletTree[['MSB']]
  SPos_list <- waveletTree[['SPos_list']]
  alphabet <-  waveletTree[['alphabet']]
  letter <- c()
  current_rank = indice
  block = 1

  for (level in 1:MSB) {
    ## Initialize SPos in current level
    SPos <- SPos_list[[level]]
    p=SPos[block]
    r=SPos[block+1]-1
    ## calculate the rank support
    bv = wt[level,p:r]
    rnk_sup <- rank_support(bv = bv)
    current_bit <- bv[current_rank]
    letter <- c(letter, current_bit)
    
    if(current_bit == 1){
      current_rank <- rank1(rank_sup = rnk_sup, indice =current_rank)
    }else{
      current_rank <- rank0(rank_sup = rnk_sup, indice =current_rank)

    }
    block = 2*block - abs(current_bit-1)
    level = level + 1
  }

  return(alphabet[bin2dec(letter)+1])

  }


wt_rank <- function(waveletTree, char, indice){
  
  # Read in the data from wavelet tree building function
  Tc <- waveletTree[['Tc']] 
  wt <- waveletTree[['wt']] 
  MSB <- waveletTree[['MSB']]
  SPos_list <- waveletTree[['SPos_list']]
  alphabet <-  waveletTree[['alphabet']] 
  
  # get the binary version of char
  char_numb <- which(char == alphabet) - 1
  char_binary = c(as.integer(rev(intToBits(char_numb))))[(32-MSB+1):32]
  level_rk = indice
  block = 1
  
  # looking up wt to get the rank of query
  for (level in 1:MSB) {
    ## Initialize SPos in current level
    SPos <- SPos_list[[level]]
    p=SPos[block]
    r=SPos[block+1]-1
    ## calculate the rank support
    bv = wt[level,p:r]
    level_rs <- rank_support(bv = bv)
    
    ## get the rank for the next level using rank function
    if (char_binary[level] == 1) {
      level_rk <- rank1(rank_sup = level_rs, indice = level_rk)
    }else{
      level_rk <- rank0(rank_sup = level_rs, indice = level_rk)
    }
    
    ## get the block of query number for the next level
    block = 2*block - abs(char_binary[level]-1)
    
  }
  return(level_rk)
}


wt_select <- function(waveletTree, char, indice){
  
  # Read in the data from wavelet tree building function
  Tc <- waveletTree[['Tc']] 
  wt <- waveletTree[['wt']] 
  MSB <- waveletTree[['MSB']]
  SPos_list <- waveletTree[['SPos_list']]
  alphabet <-  waveletTree[['alphabet']] 
  
  # get the binary version of char
  char_numb <- which(char == alphabet)-1
  char_binary = c(as.integer(rev(intToBits(char_numb))))[(32-MSB+1):32]
  level_rk = indice
  block = ceiling((char_numb+1)/2)
  
  # looking up wt to get the rank of query
  for (level in MSB:1) {
    ## Initialize SPos in current level
    SPos <- SPos_list[[level]]
    
    ## calculate the rank support
    bv = wt[level,SPos[block]:(SPos[block+1]-1)]
    level_ss <- select_support(bv = bv)
    
    ## get the rank for the next level using rank function
    if (char_binary[level] == 1) {
      level_rk <- select1(rnk = level_rk, slct_sup = level_ss)
    }else{
      level_rk <- select0(rnk = level_rk, slct_sup = level_ss)
    }
    
    ## get the block of query number for the next level
    block = ceiling(block/2)
    
  }
  return(level_rk)
}



