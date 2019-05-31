# Time_series_regression_models
This is the solution to the exercises of chapter 5 of forcasting: prinsiples and practice authored by Rob J Hyndman and George Athanasopoulos. Time series is analysing a collection of random variables usually ordered in time based on the history data to forecast the future. In the following, the solution of the fifth chapter of the book along with the R implementation are provided. The solution to the exercises are provided in the R script with the output given in png format as follows:

-elecdaily is a daily time series matrix with three columns:
  -Demand:	Total electricity demand in GW for Victoria, Australia, every day during 2014.
  -WorkDay:	taking value 1 on work days, and 0 otherwise.
  -Temperature:	maximum daily temperatures for Melbourne (BOM site 086071).

![GitHub Logo](/Rplot_scatter_matrix_temp_demand.png)
![GitHub Logo](/demandVsTemp.png)
![GitHub Logo](/lin_reg_demand_temp.png)


- Times in seconds for the gold-medal winner of the men's 400m track final at each Olympics since 1896. Missing values occur in 1916, 1940 and 1944 due to the World Wars.

![GitHub Logo](/fitted_data.png)
![GitHub Logo](/rsidualerrormen400.png)
