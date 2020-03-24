# Covid19-proj

This project provides visualizations tracking the changing status of COVID-19 cases. Visualizations will include current case trends, overlay for different countries, and comparision to previous case rates in other similar infectious diseases.

Additionally, this project will look for trends in the data and try to create a predictive model based on avaliable data. 

 #### Potential Methods
 * Linear Regression
 * Time Series Forecasting
 
 
 <hr>
 
**Collaborators**

 > ggsmith842<br> 
 > jaymie18<br>
 > chriztopherton 

**Data References**

*1. https://rapidapi.com/KishCom/api/covid-19-coronavirus-statistics?endpoint=apiendpoint_53587227-476d-4279-8f1d-4884e60d1db7*

*2. https://data.humdata.org/dataset/novel-coronavirus-2019-ncov-cases*


**Generative Approaches**
> Predicting growth by assuming new data is generated exponentially, we try to:
 - Linear Regression is used to model exponential growth, in the form of $y = a +b *x --> logx(t) = log(x_0) + log(b) +t$,
 - where x(t) is the given number of cases at any given time t, x_0 is the initial number of cases at the beginning of time, b is the growth factor, number of people infected by a single sick person
 
 
 **Findings**
 - Exponential Growth Formula: \(x(t) = 4378.537 * 1.078272^t\)


More to come.
