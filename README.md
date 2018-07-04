Instructions
----------

This repository containes scripts used for the analysis in the article *The influence of Voluntary associations on fertility during the demographic transition 1880-1949*  

## Procedure

The full analysis is reproducable autmatically from the source files by running the makefile

    make all


## Source data

None of the source files are included in this package, following is a brief description of the datasets used in the analysis. 

From the `poplinkdata` package:

`eh_data`: The main data is a event history dataset which is contructed from multiple files extracted from the Demographic database. The procedure used to create the event history dataset is documented in the data package.

`person`: Containes static infromation on every individal in the sample.

`children`: Containes information on each child born to the married population in the sample.

`voroni_spdf`: Spatial dataset of boundaries of neighbourhoods in the Skellefteå region.


## Scripts


`ehd_spells.R`: Tranforms the event history dataset to a episode file, suitable for analysis of fertility.

`coxme_exp_model.R`: Runs the regression models for exposure effects.

`coxme_pre_match.R`: Runs the regression model for preventive participation effects.

`pre-psm.R`: Prepares the event history dataset for PSM analysis.

`matching.R`: Performs the PSM.

`post-psm.R`: Runs analysis of fertility outcome on matched datasets.

These scripts produce a number of regression model objects. The following scripts extracts the paramteres of interest and creates either csv files or results used for the tables in the articel or reproduces the Figures in the article.

`format_coxme_pre.R`: Produces Table X. 

`format_exp_model.R`: Produced Table X

`vis_va_decade.R`: Produces Figure X

`distribution-matched.R`: Produces table X and figure X.

`balance-mean.R`: Produces tables X, X and X.


