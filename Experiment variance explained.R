library(tidyverse)
library(MASS)
library(reshape2)
nSimulations <- 1e4
nObservations <- 1e4
X <- rnorm(nObservations) ^ 2
Y <- 1 / (1 + exp(rnorm(nObservations)))
W <- X / Y
varW <- data.frame(X = rep(NA, nSimulations),
                   Y = rep(NA, nSimulations))
data.frame(X = X, Y = Y, W = W) %>% reshape::melt() %>% ggplot(aes(x = value)) + geom_density() + facet_wrap(variable ~ .)
for (iSimulation in 1:nSimulations) {
  ixSample <- sample.int(nObservations, replace = TRUE)
  thisW <- X[ixSample] / Y
  varW[iSimulation, 'X'] <- var(thisW)
  ixSample <- sample.int(nObservations, replace = TRUE)
  thisW <- X / Y[ixSample]
  varW[iSimulation, 'Y'] <- var(thisW)
}
colMeans(varW) / sum(colMeans(varW))


nPlot <- 1e2
nObservations <- 10
varX <- exp( rnorm((nObservations)))
varY <- exp( rnorm((nObservations))) + runif(nObservations)
rho <- -0.7
dataPlot <- data.frame(year = factor(1:nObservations),
                       varX, 
                       varY) %>% mutate(
                       varXPlusY = varX + varY + 2 * rho * sqrt(varX) * sqrt(varY),
                       impactDerivativeXNotNormalised = 1 + rho * sqrt(varY) / sqrt(varX),
                       impactDerivativeYNotNormalised = 1 + rho * sqrt(varX) / sqrt(varY),
                       impactDerivativeX = abs(impactDerivativeXNotNormalised) / ( abs(impactDerivativeXNotNormalised) + abs(impactDerivativeYNotNormalised)),
                       impactDerivativeY = abs(impactDerivativeYNotNormalised) / ( abs(impactDerivativeXNotNormalised) + abs(impactDerivativeYNotNormalised)),
                       impactDecomposedNotNormalisedX = varX,
                       impactDecomposedNotNormalisedY = varY,
                       impactDecomposedNotNormalisedCorrelation = rho * sqrt(varX) * sqrt(varY),
                       impactDecomposedX = abs(impactDecomposedNotNormalisedX) / (abs(impactDecomposedNotNormalisedX) + abs(impactDecomposedNotNormalisedY) + abs(impactDecomposedNotNormalisedCorrelation)),
                       impactDecomposedY = abs(impactDecomposedNotNormalisedY) / (abs(impactDecomposedNotNormalisedX) + abs(impactDecomposedNotNormalisedY) + abs(impactDecomposedNotNormalisedCorrelation)),
                       impactDecomposedCorrelation = abs(impactDecomposedNotNormalisedCorrelation) / (abs(impactDecomposedNotNormalisedX) + abs(impactDecomposedNotNormalisedY) + abs(impactDecomposedNotNormalisedCorrelation)))
ggplot(dataPlot %>% melt(id.vars = 'year', measure.vars = c('impactDerivativeX', 'impactDerivativeY')), aes(x = year, y = value, fill = variable)) + geom_bar(position = 'fill', stat = 'identity')
ggplot(dataPlot %>% melt(id.vars = 'year', measure.vars = c('impactDecomposedX', 'impactDecomposedY', 'impactDecomposedCorrelation')), aes(x = year, y = value, fill = variable)) + geom_bar(position = 'fill', stat = 'identity')



# Visualise the change in the variance in W with respect to a change in the
# variance of X and Y.
nPlot <- 1e2
varX <- 0.1
varY <- 0.9
varXPlot <- sort( unique(c(varX, varY, seq(0, 1, length.out = nPlot))) )
varYPlot <- varXPlot
dataPlot <-
  expand.grid(varianceX = varXPlot,
              varianceY = varYPlot)
correlationXY <- -0.5
varWPlot <- rep(NA, nrow(dataPlot))
for (iPlot in 1:nrow(dataPlot)) {
  thisVarX <- dataPlot$varianceX[iPlot]
  thisVarY <- dataPlot$varianceY[iPlot]
  if(thisVarX == 0 | thisVarY == 0){
    next
  }
  covarianceXY <- correlationXY * sqrt(thisVarX) * sqrt(thisVarY)
  XYSimulated <- 
    mvrnorm(nSimulations, c(0, 0), rbind( c(thisVarX, covarianceXY), c(covarianceXY, thisVarY)))
  varWPlot[iPlot] <- var(XYSimulated[,1] + XYSimulated[,2])
}
dataPlot$varW <- varWPlot
dataPlotXyFixed <- 
  rbind(
    dataPlot %>% filter(varianceY == varY) %>% dplyr::select(x = varianceX, varW) %>% mutate(variance = 'X'),
    dataPlot %>% filter(varianceX == varX) %>% dplyr::select(x = varianceY, varW) %>% mutate(variance = 'Y')
  )
ggplot(dataPlotXyFixed, aes(x = x, y = varW, colour = variance)) +
  geom_line(size = 2) +
  geom_hline(yintercept = dataPlot %>% filter(varianceX == varX, varianceY == varY) %>% pull(varW)) +
  geom_vline(xintercept = varX) +
  geom_vline(xintercept = varY) +
  labs(x = 'variance of X and Y',
       y = 'variance of W')

ggplot(dataPlotXyFixed, aes(x = x, y = varW, colour = variance)) +
  geom_line(size = 2) +
  geom_hline(yintercept = dataPlot %>% filter(varianceX == varX, varianceY == varY) %>% pull(varW)) +
  geom_vline(xintercept = varX) +
  geom_vline(xintercept = varY) +
  labs(x = 'variance of X and Y',
       y = 'variance of W')

