library(testthat)
library(tsibble)
library(dplyr)
source('etl.R')


test_that("get_train returns a valid tsibble", {
  # 调用函数获取数据
  ts_data <- get_train()
  
  # 1. 检查是否为 tsibble 对象
  expect_s3_class(ts_data, "tbl_ts")
  
  # 2. 检查是否有时间间隔缺失（gap）
  # 使用 tsibble::has_gaps() 检查是否有 gap
  # 检查是否有时间间隔缺失（gap）
  gaps_info <- has_gaps(ts_data)
  expect_false(any(gaps_info$.gaps), info = "Data should not have gaps in the time index.")
  
  # 3. 检查是否有缺失值（missing values）
  # 检查是否有缺失值（missing values）
  # 检查 num_sold 列是否有缺失值
  # num_sold_missing <- sum(is.na(ts_data$num_sold))
  # expect_true(num_sold_missing ==0, info = "Column 'num_sold' should not have missing values.")
  # 
  # 4. 检查是否有重复的时间点（duplication）
  duplicates_info <- duplicates(ts_data, key=c('country','store','product'))
  expect_false(any(duplicates_info), info = "Data should not have duplicate time points.")
})


test_that("get_test returns a valid tsibble", {
  # 调用函数获取数据
  ts_data <- get_test()
  
  # 1. 检查是否为 tsibble 对象
  expect_s3_class(ts_data, "tbl_ts")
  
  # 2. 检查是否有时间间隔缺失（gap）
  # 使用 tsibble::has_gaps() 检查是否有 gap
  # 检查是否有时间间隔缺失（gap）
  gaps_info <- has_gaps(ts_data)
  expect_false(any(gaps_info$.gaps), info = "Data should not have gaps in the time index.")
  
  # 3. 检查是否有缺失值（missing values）
  # 检查是否有缺失值（missing values）
  # 检查 num_sold 列是否有缺失值
  # num_sold_missing <- sum(is.na(ts_data$num_sold))
  # expect_true(num_sold_missing ==0, info = "Column 'num_sold' should not have missing values.")
  # 
  # 4. 检查是否有重复的时间点（duplication）
  duplicates_info <- duplicates(ts_data, key=c('country','store','product'))
  expect_false(any(duplicates_info), info = "Data should not have duplicate time points.")
})