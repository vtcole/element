# Supplemental material for *Algorithms for Measurement Invariance Testing: Contrasts and Connections*

## Data availability

Data in this study come from the [Longitudinal Study of Australian Children (LSAC)](https://growingupinaustralia.gov.au/). This is an ongoing longitudinal study of a nationally representative study of children and families which began in 2002. If readers wish to analyze the data in these analyses, **they must submit an application to the Australian Institute of Family Studies.** Thus, what is available in this supplement is **not the data itself, but all of the scripts run on the data** so that readers may get an idea of how analyses were performed. 

## R scripts
There are three R scripts included in the **code** folder:
* **demo.R** -- This includes the code necessary to generate Figures 1-2 (i.e., the plots of uniform and non-uniform DIF). It works solely with simulated data (i.e., not the LSAC data).
* **dataprocess.R** -- This includes code to perform preliminary data management on the LSAC data, including the data visualizations shown in Figures 4-6. *It assumes the user has already read the LSAC data file in, and the file it uses corresponds with the Wave 9 data release from the Australian Institute of Family Studies.*
* **readinMplus.R** -- This includes the code to read in Mplus outputs from the likelihood ratio tests in the multiple groups testing example using the LSAC data.

## Mplus scripts
The main model fitting steps, both for the multiple-groups and regression-based approaches, took place in Mplus. There are two main folders of Mplus inputs (*.inp) and outputs (*.out).

### Multiple-groups analysis
These are in the folder entitled **multiplegroups**. For each covariate (child sex, parent gender, child age), models 1 through 5 in the file names correspond to Models 1-5 in the dataset.

### Regression-based analysis
These are in the folder entitled **regressionbased**. 
* For each item, an itemwise model was run, entitled **irtlrdif_item[[itemnumber]]**. 
* Each of these models were then combined into a model called **irtlrdif_combined**... 
* ...which was then pruned to the final model, **irtlrdif_final**.