This folder is for the first task: bit-vector rank. 

`bv_rank.r` file is the header file for bv_rank operation. It contains four main functions:
1. `rank_support()`: rank_support function builds a succinct data structure containing four parts, which are:  
                  Super blocks `R_s`, with size (n/log2n)*logn = 2n/logn bits = o(n) bits  
                  Blocks `R_b`, with size 2nloglogn/logn bits = o(n) bits  
                  A table `R_p`, with size 2nloglogn/logn bits = o(n) bits  
                  Original bit-vector `bv` with size long bits = O(n) bits  
                  
2. `rank1()`: rank1 function is an O(1) time operation, which returns the counts of 1 appeared before a given position `pos` in `bv` (bv[0:pos]).

3. `rank0()`: rank0 function is an O(1) time operation, which returns the counts of 0 appeared before a given position `pos` in `bv` (bv[0:pos]).

4. `overhead()`: overhead() function returns the bits used to store the rank_support object.

`rank_evaluation` file evaluates the profiles of bit-vector rank operation. It first generates some random bit-vectors with different size, and plot the relations between bit-vector size and run time or overhead of rank operaion. The result shows that the runtime of rank operation is `O(1)`, and  overhead is `n+o(n)`
