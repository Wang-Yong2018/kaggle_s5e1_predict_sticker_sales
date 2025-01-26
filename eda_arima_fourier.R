library(fable.prophet)

my_dcom_sold <-
  decomposition_model(
  STL(num_sold~
        trend(365*4)+
        season(period=c(365,365/2,365/4,365/52.5,31,12,7)),
      robust=T),
  ETS(season_adjust~season("N"))
  )

fit<-agged_train|>filter(date>='2012-01-01' & date<'2015-01-01')|>
  model(
    arima = ARIMA(num_sold ~ PDQ(0,0, 0) + pdq(0,0,0) +
                    fourier( K = 3, period = 7)+
                    fourier(K = 3, period = 30)+
                    fourier(K = 3, period = 12)+
                    fourier(K = 3, period = 365)+
                    fourier(K = 3, period = 365/4)),
                    
    stl=my_dcom_sold,
    #ets = ETS(num_sold),
    prophet = prophet(num_sold~  
                        season(period = "day", order = 7) +
                        season(period = "week", order = 5) +
                        season(period = "month", order = 12) +
                        season(period = "year", order = 4))
    
        ) 

fit_fc <- fit|>forecast(h=1000)

fabletools::accuracy(fit_fc,agged_train|>
                       filter(date>='2015-01-01'))
fit_fc|>autoplot(agged_train,level=NULL)
