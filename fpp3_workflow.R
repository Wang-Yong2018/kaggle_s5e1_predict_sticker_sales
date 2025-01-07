library(fpp3)

# data prepare

gdppc <- global_economy |>
  mutate(GDP_per_capita = GDP / Population)

# data visualization
gdppc |>
  filter(Country == "Sweden") |>
  autoplot(GDP_per_capita) +
  labs(y = "美元",x="年份", title = "瑞典人均 GDP")


# define model
# fable 中的模型是使用模型函数指定的，
# 每个模型函数都使用一个公式(y ~ x)接口。 
# 响应变量指定在公式的左侧，模型的结构写在右侧。
TSLM(GDP_per_capita ~ trend())
# 在这种情况下，模型函数是 TSLM()(时间序列线性模型)，
# 响应变量是 GDP_per_capita 并且
# 使用 trend()(一个“特殊”函数，指定线性趋势，当它是在 TSLM() 中使用)。 
# 我们将仔细研究如何在各自的部分中指定每个模型。
# 用于定义模型结构的特殊函数因模型而异(因为每个模型可以支持不同的结构)。 
# 每个模型函数文档的“特殊”部分列出了这些特殊函数及其使用方法。

# fit model
fit <- gdppc |>
  model(trend_model = SNAIVE(GDP_per_capita ~ )))
fit
# evaluate model

# forecast 
fit |>
  filter(Country == "Sweden") |>
  forecast(h = "10 years") |>
  autoplot(gdppc) +
  labs(y = "美元",x='日期', title = "瑞典人均 GDP")+
  theme(text=element_text(family="STHeiti"))+
  theme(plot.title = element_text(hjust = 0.5))

