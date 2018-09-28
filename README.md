Instructions
----------

This repository contains scripts used for the analysis in the article Junkka, J. (2018). Membership in and Presence of Voluntary Organisations during the Swedish Fertility Transition, 1880-1949. *Historical Life Course Studies*, 5, 3-36. http://hdl.handle.net/10622/23526343-2018-00012?locatt=view:master.

## Procedure

The full analysis is reproducible automatically from the source files by running the makefile

    make all


## Source data

None of the source files are included in this package; following is a brief description of the datasets used in the analysis. 

From the `poplinkdata` package:

`eh_data`: The primary data is a event history dataset which is constructed from multiple files extracted from the Demographic database. The procedure used to create the event history dataset is documented in the data package.

`person`: Contains static information on every individual in the sample.

`children`: Contains information on each child born to the married population in the sample.

`voroni_spdf`: Spatial dataset of boundaries of neighbourhoods in the Skellefteå region.


## Scripts

`spatial-partition.R`: Performs the spatial partitioning and produces Table 16 and Figure 11.

`ehd_spells.R`: Transforms the event history dataset to a episode file, suitable for analysis of fertility.

`coxme_exp_model.R`: Runs the regression models for exposure effects.

`coxme_pre_match.R`: Runs the regression model for preventive participation effects.

`pre-psm.R`: Prepares the event history dataset for PSM analysis.

`matching.R`: Performs the PSM and produce Table 10.

`post-psm.R`: Runs analysis of fertility outcome on matched datasets. Produces Table 4, 5 and 14, and Figure 9

These scripts produce a number of regression model objects. The following scripts extract the parameters of interest and create either csv files or results used for the tables in the article or reproduce the Figures in the article.

`format_coxme_pre.R`: Produces Table 6 and 15. 

`format_exp_model.R`: Produces Table 2, 8 and 9.

`vis_va_decade.R`: Produces Figure 6.

`format_psm_reg.R`: Produces Table 10.

`distribution-matched.R`: Produces Table 3 and Figure 8.

`balance-mean.R`: Produces Tables 11, 12 and 13.


