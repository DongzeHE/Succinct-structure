There are five files in this folder, which can satistisfy the four functions required for this task.
  1. `wt_header.R` is the head file including all functions used for this task. 
  This file should be placed with all other files as the source of other files in this task.
  
  
  2. `wt_build.R` is the file used to achieve wavelet tree construction. 
  To run this function, please run `$Rscript wt_build.R  <input string> <output file>`. 
  This file need two arguments to run.  
          1. <input string>: Users MUST input a string file as the string used for wavelet tree construction.  
          2. <output file>: Users can specifify the output file name, the output file should be a `.RDS` file used for further functions. This argument is optional, if not specified, a RDS file named `wavelet_tree.RDS` will be generated automatically.  
  
  
  3. `wt_access.R` is the file used for wavelet tree access function. 
  To run this function, please run `$Rscript wt_access.R <saved wt> <access indices>`. 
  The program outputs the characters (one per-line) corresponding to each index in the file <access indices>.
  This file need two arguments to run.  
            1. <saved wt> is the file saved as <output file> from the construction command above.  
            2. <access indices> is a file containing a newline-separated list of indices (0-based) to access.   
            
            
  4. `wt_rank.R` is the file used for wavelet tree rank function. 
  The program should output the answers to the rank queries (one per-line) corresponding to each query in the file <rank queries>.
  To run this function, please run `$Rscript wt_rank.R <saved wt> <access indices>`.  
          1. <saved wt> is the file saved as <output file> from the construction command above.  
          2. <rank queries> is a file containing a newline-separated list of rank queries to issue.   
          Each rank query is of the format <c>\t<i>, where <c> is some character from the alphabet of the original string, 
          <i> is some index and \t is the tab character.
          
          
  5. `wt_select.R` is the file used for wavelet tree select function. 
  The program should output the answers to the select queries (one per-line) corresponding to each query in the file <select queries>.
  To run this function, please run `$Rscript wt_select.R <saved wt> <access indices>`.  
          1. <saved wt> is the file saved as <output file> from the construction command above.  
          2. <select queries> is a file containing a newline-separated list of select queries to issue.   
          Each select query is of the format <c>\t<i>, where <c> is some character from the alphabet of the original string, 
          <i> is the occurrence of the character <c> for one wishes to know the index (again, \t is the tab character). 
          
          
  There are the example files used for each function, to run the toy example, please run:  
          1. `$Rscript wt_build.R a_string.txt` to achieve wavelet tree construction.  
          2. `Rscript wt_access.R wavelet_tree_example.RDS access_indices_example.txt`  
          3. `$Rscript wt_rank.R wavelet_tree_example.RDS rank_indices_example.txt`  
          4. `$Rscript wt_select.R wavelet_tree_example.RDS select_indices_example.txt`  
