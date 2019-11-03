
This folder is for the second task: bit-vector select. 

`bv_select.r` file is the header file for bv_select operation. It contains four main functions:
1. `select_support()`: select_support function builds a succinct data structure containing Three parts, which are
                  super blocks `R_s`, with size (n/log2n)*logn = 2n/logn bits = o(n) bits
                  blocks `R_b`, with size 2nloglogn/logn bits = o(n) bits
                  a table `R_p`, with size 2nloglogn/logn bits = o(n) bits
                  original bit-vector `bv` with size long bits = O(n) bits
                  
2. `select()`: select1 function is an O(logn) time operation, which returns the position of the i-th queried character in `bv`. This function requires three inputs, which are select_support object, a character and the rank of the queried character.

4. `overhead()`: overhead() function returns the bits used to store the select_support object.

`select_evaluation.R` file evaluates the profiles of the bit-vector select operation. It first generates some random bit-vectors with different size, and plot the relations between bit-vector size and run time or overhead of select operation. The result shows that the runtime of select operation is `O(logn)`, and the overhead is `n+o(n)`
