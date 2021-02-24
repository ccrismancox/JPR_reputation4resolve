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

## Files and contents

### Folders 

- `Data` Contains data files used in the analysis
    - `FearonLaitin.dta` Fearon and Laitin's 2003 APSR data
    - `fearonOutOfSample.csv` Case from Fearon's 2004 JPR that are not in UCDP.
    - `fullDataSet.csv` The main data set used in this paper
    - `NMC_v4_0.csv` Correlates of War NMC data
    - `woa_USE*` A series of `Rdata` files that are used to fit the various models. Each one is designed for a specific robustness check. See the appendix for more detials.
- `functions` Contains additional functions used in the project
    - `avdata.r` A function to compute "average" data profile
    - `deoptimWrap.r` A function to customize how `DEoptim` is use. Mainly used to control the parallel options
    - `estimationFunctions2018.R` loglikelihood and other functions
- `startingvalues` A folder containing code to produce and save starting values for each model
    - `model*_startvalues.R` Code to produce starting values for each model 0-11
    - `model*start_currentIT.rdata` File produced along the way the save the current output of `DEoptim`
    - `model*startvalues.rdata` File that contains the produced starting values
 
### Main files

- `analyze_MonteCarlo.r` Analyze the main Monte Carlo simulation
- `analyze_MonteCarlo_section.r` Analyze the Monte Carlo with missing 0s
- `codebook.md` description file for `fullDataSet.csv`
- `figure*.pdf` Figure in PDF format
- `figure*.R` Code to produce the figure
- `JPR_logFile_appendix.txt` Logged output from running the appendix files
- `JPR_logFile_mainText.txt` Logged output from running the main text files
- `MCresult_main.pdf` Figure A.1 in pdf format
- `model*_fit.R` Code to fit models 0-11. Consdult the bash (.sh) files for the ordering and the log files to see which files match which Tables. The main text models are 1, 2, and 7. Models 3-6 and 8-11 are in the Appendix (in order) and model 0 is used only for model fit comparisions.
- `model*_bootstrap.r` Bootstrap code for models 1-11.
- `model*_bootstrap.rdata` Saved bootstrap results
- `modelfit.r` Code for model fit exercises (Appendix F)
- `MonteCarloExperiment.r` Code to run the Monte Carlo simulation
- `MonteCarloExperiment_selection.r` Code to run the Monte Carlo simulation with missing cases
- `MonteCarloResults.rdata` Saved results of the main Monte Carlo
- `MonteCarloResults_section.rdata` Saved results of the Monte Carlo simulation with missing cases
- `nullModel.Rdata` Save results of Model 0. A null model used for model fit
- `readme.md` this file
- `replicate.sh` the bash script that runs all the files used in the main analysis.  The text output is recorded in `JPR_logFile_mainText.txt`
- `replicate_appendix.sh` the bash script that runs all the files used in the online appendix.  The text output is recorded in `JPR_logFile_appendix.txt`
- `summary.R` Produce summary statistics table in Appendix C


