#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)!=2) {
  stop("Please provide a wavelet tree (.RDS) file and a access indices contained file.\n", call.=FALSE)
} 

## program

### read in source file and wavelet tree object
source("wt_header.R")
waveletTree = readRDS(args[1])

# make output
cat(paste("character","rank","\n",sep = "\t"))
indices <- read.delim(args[2],sep = "\t",header = FALSE,row.names = NULL,stringsAsFactors = FALSE)
# print(indices)
for (index in 1:nrow(indices)) {

  indices[index,3] <-wt_rank(waveletTree = waveletTree,
                       char = indices[index,1],
                       indice = indices[index,2])

  cat(indices[index,1],indices[index,3],"\n",sep = "\t")
}





