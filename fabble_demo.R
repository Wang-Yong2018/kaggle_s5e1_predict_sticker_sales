# fable work flow it is too slow
# general tidy verse
library(dplyr)
library(timetk)
library(lubridate)
library(data.table)
library(readr)
source('etl.R')
# tidymodels
library(tidymodels)
library(workflows)
library(workflowsets)

#fpp3
library(tsibble)
library(fabletools)
# timemodels
library(modeltime)
library(timetk)

# parallel
library(tictoc)
library(future)
library(doFuture)


# others
library(lightgbm)
library(skimr)
train_file <- './input/train.csv'
test_file <- './input/test.csv'
submission_file <- './input/sample_submission.csv'

#full_train <- get_train()
full_train <- get_fillna_train()
full_test <- get_test()
full_submission <- read_csv(submission_file)

#####################################


# 1. quick forecast by fable
library(furrr)

tmp<- full_train |>  filter( country %in% c('sg','fl','ca') ,
                             #store=='disc',
                             product %in% c('goose') ,
                             date < '2016-01-01')
plan(multisession)
tic("parallel")
tmp_fct <- tmp |>
  split(tmp$store)|>
  future_map_dfr(\(x)  
                 x|>
                   model(ets = ETS(box_cox(num_sold, 0.3)),
                         arima = ARIMA(log(num_sold)),
                         snaive = SNAIVE(num_sold) ) |>
                   forecast(h = "1 years") 
  )

toc()
plan(sequential)

#tmp_fct |> autoplot(tmp|>filter(date>'2015-01-01'), level = NULL)

# 2. fpp3 eda 
full_train |> ggplot(aes(x=num_sold))+geom_density()
full_train |> ggplot(aes(x=log1p(num_sold)))+geom_density()+theme_minimal()



### 1.3 eda full test
full_train_hts <- 
  full_train |>
  mutate(store=case_match(store,
                          'Discount Stickers'~'disc',
                          'Premium Sticker Mart'~'prem',
                          'Stickers for Less'~'less'),
         product=case_match(product,
                            'Holographic Goose'~'goose',
                            'Kaggle'~'kag',
                            'Kaggle Tiers'~'kagt',
                            'Kerneler'~'kern',
                            'Kerneler Dark Mode'~'kdm'
         ),
         country=case_match(country,
                            'Canada'~'ca',
                            'Finland'~'fl',
                            'Italy'~'it',
                            'Kenya'~'ky',
                            'Norway'~'nw',
                            'Singapore'~'sg' ))|>
  as_tsibble(key=c('country','store','product'),index=date)
#### a. timeplot_per_country
full_train_hts |> 
  filter(is_aggregated(product) & is_aggregated(store) & !is_aggregated(country)) |>
  autoplot(num_sold)+
  scale_y_log10()+
  theme(legend.position = "right")
#### a. timeplot_per_store
full_train_hts |> 
  filter(is_aggregated(product) & !is_aggregated(store) & is_aggregated(country)) |>
  autoplot(num_sold)+
  scale_y_log10()+
  
  theme(legend.position = "right")
#### a. timeplot_per_product
full_train_hts |> 
  filter(!is_aggregated(product) & is_aggregated(store) & is_aggregated(country)) |>
  autoplot(num_sold)+
  scale_y_log10()+
  
  theme(legend.position = "right")
## 2. single level forecast
fcast_country <- 
  full_train_hts |>
  filter(!is_aggregated(country) & country=='ca') |>
  model(ets=fable::ETS(num_sold)) |>
  forecast()

fcast_country


# Sum bottom-level forecasts to get top-level forecasts
fcast_country <- fcast_country |>
  summarise(value = sum(num_sold), .mean = mean(value))
fcast_country
### 3. reconcile
fcast_country <- 
  full_train_hts |>
  filter(!is_aggregated(country) & country=='ca') |>
  model(ets=fable::ETS(num_sold)) |>
  reconcile(bu = bottom_up(ets)) |>
  forecast()
