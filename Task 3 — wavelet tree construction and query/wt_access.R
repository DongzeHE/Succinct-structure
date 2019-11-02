#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)!=2) {
  stop("Please provide a wavelet tree (.RDS) file and a access indices contained file.\n", call.=FALSE)
} 

## program
source("wt_header.R")
waveletTree = readRDS(args[1])
cat("character","indice", "\n",sep = "\t")
indices <- read.delim(args[2],sep = "\t",header = FALSE)[,1]
for (index in 1:length(indices)) {
  cat(wt_access(waveletTree, indices[index]),indices[index],"\n",sep = "\t")
}
