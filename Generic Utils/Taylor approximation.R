# Get the Taylor approximation of 1 / x around x = x_0.

library(tidyverse)

x_0 <- 10
xEvaluate <- 8
nMaxDerivative <- 10
derivative <- rep(NaN, nMaxDerivative)
terms <- rep(NaN, nMaxDerivative)
functionEvaluate <- function(x) 1 / x
f_x_0 <- functionEvaluate(x_0)
functionDerivative <- function(x, n) (-1)^n * factorial(n) / x^(n  + 1)
delta <- xEvaluate - x_0
dataPlotBase <- data.frame(x = seq( 0.9 * min(x_0, xEvaluate), 1.1 * max(x_0, xEvaluate), length.out = 1e2 )) %>%
  mutate(y = functionEvaluate(x),
         type = 'function')
for(iDerivative in 1:nMaxDerivative){
  derivative[iDerivative] <- functionDerivative(x_0, iDerivative)
  terms[iDerivative] <- delta^iDerivative * derivative[iDerivative] / factorial(iDerivative)
  
  # Calculate the approximation error.
  if(iDerivative == 1){
    functionApproximation <- function(x) f_x_0 - (x - x_0) / x_0^2
  }else if(iDerivative == 2){
    functionApproximation <- function(x) f_x_0 - (x - x_0) / x_0^2 + (x - x_0)^2 / x_0^3
  }else if(iDerivative == 3){
    functionApproximation <- function(x) f_x_0 - (x - x_0) / x_0^2 + (x - x_0)^2 / x_0^3 - (x - x_0)^3 / x_0^4
  }else if(iDerivative == 4){
    functionApproximation <- function(x) f_x_0 - (x - x_0) / x_0^2 + (x - x_0)^2 / x_0^3 - (x - x_0)^3 / x_0^4 + (x - x_0)^4 / x_0^5
  }
  
  approximation <- f_x_0 + sum(terms, na.rm = TRUE)
  
  approximationError <- abs(functionEvaluate(xEvaluate) - approximation)
  
  # Plot the function values and the approximation.
  dataPlot <- rbind(dataPlotBase, 
                    dataPlotBase %>% mutate(x = dataPlotBase$x,
                                            y = functionApproximation(x),
                                            type = 'Taylor expansion'))
  print(
    ggplot(dataPlot, aes(x = x, y = y, colour = type)) + 
      geom_line() +
      geom_point(data = data.frame(x = xEvaluate, y = functionEvaluate(xEvaluate), type = 'function'), aes(x = x, y = y))
  )
  
  
  print(approximationError)
}