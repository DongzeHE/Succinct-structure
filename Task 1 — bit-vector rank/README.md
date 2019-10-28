This folder is for the first task: bit-vector rank. 

`bv_rank.r` file is the header file for bv_rank operation. It contains four main functions:
1. `rank_support()`: rank_support function builds a succinct data structure containing Three parts, which are
                  super blocks `R_s`, with size (n/log2n)*logn = 2n/logn bits = o(n) bits
                  blocks `R_b`, with size 2nloglogn/logn bits = o(n) bits
                  a table `R_p`, with size 2nloglogn/logn bits = o(n) bits
                  original bit-vector `bv` with size long bits = O(n) bits
                  
2. `rank1()`: rank1 function is an O(1) time operation, which returns the counts of 1 appeared before a given position `pos` in `bv` (bv[0:pos]).

3. `rank0()`: rank0 function is an O(1) time operation, which returns the counts of 0 appeared before a given position `pos` in `bv` (bv[0:pos]).

4. `overhead()`: overhead() function returns the bits used to store the rank_support object.

`rank_evaluation` file makes up some random bit-vector in different size, and plot the relations between bit-vector size and run time or overhead.
