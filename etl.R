library(fpp3)
library(data.table)
library(dplyr)
library(memoise)
library(imputeTS)
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
  mutate(date = as_date(date))|>
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

get_country_ts_filled <-function(original_df,target_country_name = NULL,
                                 ref_country_name=NULL,
                                 weight=1){
  
  ref_ts <- 
    original_df|>
    filter(country==ref_country_name)|>
    rename(fill_num_sold=num_sold ,source_id=id,source_country=country)|>
    mutate(fill_weight=weight)
  if(target_country_name=='ky'){
    missing_ts <- 
      original_df |>
      filter(country==target_country_name)|>
      group_by(country,store, product)|>
      fill(num_sold,.direction='downup') |>
      mutate(num_sold=replace_na(num_sold,5))|>
      ungroup()
    fill_ts <- missing_ts |>
      mutate(
        fill_num_sold=num_sold,
        source_id = id, 
        source_country=country,
        fill_weight=weight) |>
      as_tibble()
  } else {
    fill_ts <- 
      original_df |>
      filter(country==target_country_name & is.na(num_sold))|>
      left_join(ref_ts,by=c('date','store','product'))
  
    
  }
  fill_ts <- 
    fill_ts |>
    select(id,source_id, source_country,fill_num_sold, fill_weight)
  
  return(fill_ts)
}

get_fillna_train <-function(){
  full_train <- get_train()|>as_tibble()
  # Step 2: Group by country, store, and product, and calculate missing counts
#   group_missing_summary <- full_train %>%
#     group_by_key()|>
# #    index_by(year=year(date))|>
#     index_by(date_year = ~ lubridate::floor_date(., "10 year")) %>%
#     summarise(missing_count = sum(is.na(num_sold)), .groups = 'drop')|>
#     filter(missing_count>0)
  # Key:       country, store, product [9]
  # country store product date_year  missing_count
  # <chr>   <chr> <chr>   <date>             <int>
  # 1 ca      disc  goose   2010-01-01          2557
  # OK  2 ky      disc  goose   2010-01-01          2557
  # OK  3 ky      less  goose   2010-01-01          1358
  # 4 ca      less  goose   2010-01-01          1308
  # OK 5 ky      prem  goose   2010-01-01           646
  # 6 ca      prem  goose   2010-01-01           380
  # OK 7 ky      disc  kern    2010-01-01            63
  # OK 8 ca      disc  kern    2010-01-01             1
  # OK 9 ky      disc  kdm     2010-01-01             1
  # missing observation
  ## ca/ky disc goose is fully missing 100% 
  ## --> solutioin - > use finland to replace ca disc goose by guess
  
  ## ca/ky less goose is 50% missing
  ## ca/ky prem goose is 20% missing
  ## ca/ky disc is part missing 63/1/1
  

   
 fill_ky <-get_country_ts_filled(full_train,
                                 target_country_name = 'ky',
                                 ref_country_name = 'sg')
 fill_ca <-get_country_ts_filled(full_train,
                                 target_country_name = 'ca',
                                 ref_country_name = 'fl')
 fill_ts <- bind_rows(fill_ky,fill_ca)
   
 full_train_filled <-
   full_train |>
   left_join(fill_ts,by=c('id')) |>
   mutate( num_sold = case_when(
     is.na(num_sold)~fill_num_sold,
     .default=num_sold))|>
  as_tsibble(index='date',key = c('country','store','product'))
 
 group_missing_summary <- full_train_filled %>%
      group_by_key()|>
      index_by(date_year = ~ lubridate::floor_date(., "10 year")) %>%
      summarise(missing_count = sum(is.na(num_sold)), .groups = 'drop')|>
      filter(missing_count>0)
 if (nrow(group_missing_summary)>0){
   print('missing value of num_sold fix failed!!!')
   print(group_missing_summary)
 }
return(full_train_filled)
  
  
}                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                    

internal_get_ts_features<- function(use_log1p=T){
  full_train <- get_fillna_train()
  if (use_log1p){
    full_train <- full_train|>mutate(num_sold=log1p(num_sold))
  }
  
  fab_features <- 
    full_train |>
    features(num_sold, 
             features = feature_set(tags =c("autocorrelation",'acf','count',
                                            'decomposition','intermittent','portmanteau',
                                            'rle','season','slide',
                                            'stability','spectral','unitroot')))
  fab_features<- 
    fab_features |> # replace some missing value timeseries feature NA, NAN with 0
    mutate(across(everything(), ~ ifelse(is.na(.) | is.nan(.), 0, .)))
  
  return(fab_features)
} 

get_fabts_features <- memoise(internal_get_ts_features, cache=cach_location)

get_fabts_augment_df<- function(df,use_log1p=T){
  fabts_features <- get_fabts_features(use_log1p)
  result_df <-
    df |>
    left_join(fabts_features, 
              by=c('country','store','product'))
    
  return(result_df)
}