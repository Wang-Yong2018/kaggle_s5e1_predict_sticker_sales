library(fpp3)
library(data.table)
library(dplyr)
library(memoise)
cach_location <- cache_filesystem("./cache")


internal_get_ts_data <- function(file_name=NULL){
  fread(file_name)|>
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
}

get_ts_data <- memoise(internal_get_ts_data, cache=cach_location)

get_train <- function(){
  file_name <- './input/train.csv'
  get_ts_data(file_name)
}
get_test <- function(){
  file_name <- './input/test.csv'
  get_ts_data(file_name)
}
