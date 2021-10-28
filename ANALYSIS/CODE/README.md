# Content
This folder contains the code to reproduce some of the results. Since the raw data from ZIMS cannot be made publicly available the steps 00.n steps cannot be reproduced. Intermediate results can be found in the RESULTS folder. Other raw data can be found in the DATA folder. 

The functions folder contains the custom functions used in this analysis. 

The markdowns folder contains the markdown documents that knit all the results together. Start there to understand the workflow. 

# How to reproduce
To reproduce the results each script has to be sourced (with `chdir = T`). This can most easily be done from the markdown that also generates the paper. Each script can be run independently since intermediate results are saved, but to fully reproduce the results all 01 have to be run before 02, etc. Order within steps does not matter. 