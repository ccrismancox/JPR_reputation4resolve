# Codebook for "Democracy, Reputation for Resolve, and Civil Conflict"
# Casey Crisman-Cox 
# Februrary 2021

## Introduction

This codebook contains descriptions for the variables listed in the files `mainDataSet.csv` and `woaData_USEA.rdata`. This is the core data set. Other extensions are also included that are described in the paper.
These are the data used to fit the main model (Model 2).
Unless otherwise noted variables are from the  UCDP Conflict Termination Dataset version 2010-1 (Dyadic version).
Note that not all variables are used in the analysis.

## Variables and descriptions
- `ccode` The Correlates of War (COW) country code of the government side of the Uppsala Conflict Data Program (UCDP) recorded internal conflict
- `startyear` The UCDP coded start year for the conflict 
- `caseList` The rebel side of the conflict 
- `durationMons` The total duration in months from the start to the end of the conflict episode
- `VicSide3` Dummy variable that records if the outcome of the conflict was a government concession (1), rebels backed down (0), or censored (2) 
- `Const` A constant term 
- `rgdpna` Logged GDP per capita (Penn World Table)
- `polity2` Polity 2 score (Polity IV)
- `tpop` Population (COW's National Materials and Capabilities data (NMC))
- `milper` Logged Military Personnel per captia (COW NMC)
- `relfrac` Religious Fractionalization (Fearon and Laitin 2003)
- `ethfrac` Ethnic Fractionalization (Fearon and Laitin 2003)
- `lmtnest` Logged % mountainous terrain (Fearon and Laitin 2003)
- `sos` Dummy variable, is the conflict a  "Son's of Soil" conflict (Cunningham 2006)
- `coup` Dummy variable, did the conflict start as a coup (Cunningham 2006)
- `Oil` Dummy variable, is the state an oil exporter (Fearon and Laitin 2003)
- `rebInt` Dummy variable, did any external state intervene on behalf of the rebels?
- `govInt` Dummy variable, did any external state intervene on behalf of the government?
- `Incomp` Dummy variable is the conflict over government control (1) or territorial separation (0). **Note** this coding is changed when fitting the model. Within every script file that uses the data I recode this variable to make territorial separation 1.

More complete citations for these data can be found in the main article
