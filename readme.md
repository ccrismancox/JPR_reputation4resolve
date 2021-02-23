# Replication instructions for ``Democracy, Reputation for Resolve, and Civil Conflict''
# Casey Crisman-Cox
# February 2021

## Replication instructions
All analysis was conducted using R 3.6.3.  The following files will conduct the replication:

- `replication.sh` Running this file will replicate all tables and figures in the main text. The log file `JPR_logFile_mainTxt.txt` records the output of running this file. Figures 1 & 2 are output as PDF files.
- `repicate_appendix.sh` Running this file will replicate all tables and figures in the online appendix. The log file `JPR_logFile_appendix.txt` records the output of running this file. Figures D1-4 are output as PDF files.


## Packages 
The following R packages are used in this analysis

- `DEoptim` (2.2-4)
- `matrixStats` (0.54.0)
- `doParallel` (1.0.15)
- `doRNG` (1.7.1)
- `data.table` (1.12.8)
- `stringr` (1.4.0)
- `ggplot2` (3.1.1)
- `gridExtra` (2.3)



## Additional resources and explanation
Starting values are very important for the optimization. The folder labeled `startingvalues` includes the (time and resource intensive) global optimization routines used to produce starting values. They are not run within the main replication package, but are included as additional information for replicators.


## R Session Information
    
    R version 3.6.3 (2020-02-29)
    Platform: x86_64-pc-linux-gnu (64-bit)
    Running under: Ubuntu 18.04.5 LTS

    Matrix products: default
    BLAS:   /usr/lib/x86_64-linux-gnu/openblas/libblas.so.3
    LAPACK: /usr/lib/x86_64-linux-gnu/libopenblasp-r0.2.20.so
