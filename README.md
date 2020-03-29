# COVID State Comparisons

This is a simple Shiny app to facilitate comparison of the incidence and mortality trajectories
between pairs of states. All data is converted to rates per million population to facilitate 
direct comparison. Since the outbreak started at different times in different places, the data 
is plotted as event rates vs. days since the rate crossed an initial threshold you specify. 

The app can be accessed live on [shinyapps.io](https://erickort.shinyapps.io/covid_state_comp).

This is based on data curated by [The NY Times](https://www.nytimes.com/article/coronavirus-county-data-us.html) 
and US census bureau [population estimates](https://www2.census.gov/programs-surveys/popest/). As 
curating the data from diverse state level sources is not a trivial task, the data set from the 
NY Times may be a day or so behind current counts.