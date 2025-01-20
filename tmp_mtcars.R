library(dplyr, warn.conflicts = FALSE)

aus_retail |>
  filter( Industry=="Food retailing",
          State %in% c("Victoria", "New South Wales", "Queensland")
  ) |>
model(snaive=SNAIVE(Turnover),
      ets = ETS(log(Turnover)),
)
