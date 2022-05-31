library(tidyverse)
library(reshape2)
library(lubridate)

set.seed(2)

nObservations <- 10
revenue <- exp(rnorm(nObservations)) + 10
costs <- exp(rnorm(nObservations))
fhIncome <- function(inRevenue, inCosts)
  inRevenue - inCosts
unpaidAwu <- exp(rnorm(nObservations))
dataObserved <- data.frame(
  year = seq( to = year( Sys.Date() ), length.out = nObservations),
  income = fhIncome(revenue, costs),
  revenue = revenue,
  costs = costs,
  `unpaid AWU` = unpaidAwu,
  `income per unpaid AUW` = income / unpaidAwu,
  check.names = FALSE
)
dataMelted <-
  dataObserved %>%
  select(year, income, revenue, costs) %>%
  melt(id.vars = 'year') %>%
  group_by(variable) %>%
  mutate(delta = value - lag(value))
ggplot(dataMelted, aes(x = year, y = value, colour = variable)) + geom_line(size = 1) + geom_point()

changeIncomeExplainedByRevenuNotNormalised <-
  (fhIncome(revenue, costs) - fhIncome(lag(revenue), costs)) / dataMelted %>% filter(variable == 'income') %>% pull(delta)
changeIncomeExplainedByCostsNotNormalised <-
  (fhIncome(revenue, costs) - fhIncome(revenue, lag(costs))) / dataMelted %>% filter(variable == 'income') %>% pull(delta)
dataObserved$changeIncomeExplainedByRevenu <-
  abs(changeIncomeExplainedByRevenuNotNormalised) / (
    abs(changeIncomeExplainedByRevenuNotNormalised) + abs(changeIncomeExplainedByCostsNotNormalised)
  )
dataObserved$changeIncomeExplainedByCosts <-
  abs(changeIncomeExplainedByCostsNotNormalised) / (
    abs(changeIncomeExplainedByRevenuNotNormalised) + abs(changeIncomeExplainedByCostsNotNormalised)
  )
