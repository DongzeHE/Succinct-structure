#!/usr/bin/env Rscript
args = commandArgs(trailingOnly=TRUE)

# test if there is at least one argument: if not, return an error
if (length(args)==0) {
  stop("Please provide a file that contains a string.\n", call.=FALSE)
} else if (length(args)==1) {
  # default output file
  args[2] = "wavelet_tree"
}

## program
source("wt_header.R")
Tc = readChar(args[1], file.info(args[1])$size)
Tc = strsplit(Tc,split = '')[[1]]
saveRDS(wt_build(Tc),file = paste0(args[2],".RDS"))
