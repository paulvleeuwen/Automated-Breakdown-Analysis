VisualiseImpact <- function(dataRaw, 
                            varNameOutput, 
                            varNameDimension = NULL, 
                            maxNumInputsInPlot = 4,
                            isTakeImpactOfMean = TRUE,
                            isShowConfidenceInterval = FALSE,
                            fontSize = 12){
  
  library(plotly)
  
  # WISH Allow the user to select input variables that affect the output
  # variable at several depths. For example, total accrued costs per unit is
  # dependent on total accrued costs per farm and surface. Total accrued costs
  # per farm, in turn, is dependent on reproduction material, manure, etc. The
  # user should be able to select manure as input variable and total accrued
  # costs per unit as output variable.
  
  # WISH When an input variable cannot be explained anymore by other variables
  # or no dimension variable is chosen, show just the counts.
  
  # TODO make the threshold an input argument
  thresholdOmitVariables <- 0
  
  # Validate the input arguments.
  if(!(is.numeric(maxNumInputsInPlot) & length(maxNumInputsInPlot) == 1 & maxNumInputsInPlot >= 2 & floor(maxNumInputsInPlot) == maxNumInputsInPlot))
    stop('The input argument maxNumInputsInPlot is expected to be an integer of at least 2.')
  if(!(length(isShowConfidenceInterval) == 1 & (is.logical(isShowConfidenceInterval) | ( is.numeric(isShowConfidenceInterval) & 0 <= isShowConfidenceInterval & isShowConfidenceInterval <= 1))))
    stop('The input argument isShowConfidenceInterval is expected to be a number between 0 and 1.')
  if(is.logical(isShowConfidenceInterval))
    confidenceLevel <- 0.95
  if(is.numeric(isShowConfidenceInterval)){
    confidenceLevel <- isShowConfidenceInterval
    isShowConfidenceInterval <- TRUE
  }
    
  source('C:/Data Science Team/Generic Utils/EnumerateAsText.R') # for the title of the impact plot
  
  if(isTakeImpactOfMean){
    calculateImpactResults <- CalculateImpact2(dataRaw, varNameOutput, varNameDimension, maxNumInputsInPlot)
  }else{
    calculateImpactResults <- CalculateImpact(dataRaw, varNameOutput, varNameDimension, maxNumInputsInPlot)
  }
  
  dataObserved <- calculateImpactResults$dataObserved
  dataImpact <- calculateImpactResults$dataImpact
  varNamesInputSortedImpact <- calculateImpactResults$varNamesInputSortedImpact
  varNamesInputMerged <- calculateImpactResults$varNamesInputMerged
  
  # The impact values for the first period are expected to be NaN. Replace them
  # by zero to align the plot with the observed values in the first period.
  dataImpact$mean <- replace_na(dataImpact$mean, 0)
  dataImpact$var <- replace_na(dataImpact$var, 0)
  
  # Only keep the variables that have an impact at least the threshold for one year.
  varNamesOmit <- dataImpact %>% 
    group_by(variable) %>% 
    summarise(mean = max(mean)) %>% 
    filter(mean < thresholdOmitVariables) %>% 
    pull(variable)
  dataImpact <- dataImpact %>% 
    filter(!(variable %in% varNamesOmit))
  if(nrow(dataObserved) == 0){
    ggplot() +
      theme_void() +
      geom_text(aes(0, 0, label = paste0('no data to be shown for output variable ', varNameOutput)))
    return()
  }
  dataObserved <- dataObserved %>% 
    filter(!(variable %in% varNamesOmit))
  
  # Visualise the impact plot.
  
  # Get the colours of the observed plot and apply them to the impact plot.
  fhObserved <- dataObserved %>%
    ggplot(aes(x = year, y = mean, colour = variable)) +
    geom_line(size = 1)
  coloursObserved <- ggplot_build(fhObserved)$data[[1]]$colour
  coloursImpact <-
    data.frame(
      variable = levels(dataObserved$variable),
      colour = coloursObserved[!duplicated(coloursObserved)]
    )
  
  # Combine the observed and impact data to facet the plots.
  dataPlot <-
    rbind(
      dataImpact %>% mutate(facet = 'impact'),
      dataObserved %>% mutate(facet = variable)
    ) %>% mutate(variable = factor(variable, levels = levels(dataObserved$variable)),
                 facet = factor(facet, levels = c(levels(dataObserved$variable), 'impact'), ordered = TRUE))
  
  fhObservedImpact <- dataPlot %>% 
    ggplot(aes(x = year, y = mean, colour = variable)) + 
    facet_wrap(facet ~ ., ncol = 1, scales = 'free_y',  strip.position = 'left')
  varNamesInput <- levels(dataImpact$variable)
  if(isShowConfidenceInterval){
    confidenceDistanceToMean <- qnorm( 1 - (1 - confidenceLevel) / 2 )
    fhObservedImpact <- fhObservedImpact + 
      geom_ribbon(data = dataPlot %>% filter(variable == varNameOutput, facet != 'impact'), 
                  aes(ymin = mean - confidenceDistanceToMean * sqrt(var / n), 
                      ymax = mean + confidenceDistanceToMean * sqrt(var / n)),
                  alpha = 0.1,
                  colour = 'grey')
  } 
  fhObservedImpact <- fhObservedImpact + 
    geom_line(data = dataPlot %>% filter(variable == varNameOutput, facet != 'impact'), size = 1) + 
    geom_point(data = dataPlot %>% filter(variable == varNameOutput, facet != 'impact')) +
    geom_col(data = dataPlot %>% filter(facet == 'impact') %>% mutate(mean = 100 * mean), aes(fill = variable), colour = 'white') + 
    scale_fill_manual('variable',
                      values = coloursImpact %>% filter(variable %in% varNamesInput) %>% pull(colour)) +
    scale_x_continuous(breaks = unique(dataImpact$year), minor_breaks = NULL) +
    xlab('') +
    ylab('') +
    theme_bw() + 
    theme(legend.position = 'none',
          panel.spacing=unit(0,'npc'),
          strip.background = element_blank(),
          strip.placement = 'outside',
          text = element_text(size = fontSize))
  plotTitle <- paste0('impact analysis of ', 
                      varNameOutput, 
                      ' (unweighted) with respect to the ', 
                      if_else(is.null(varNameDimension), 'input', 'dimension'), 
                      ' variables ', 
                      EnumerateAsText(varNamesInputSortedImpact))
  if(is.null(varNamesInputMerged)){
    fhObservedImpact <- fhObservedImpact + ggtitle(plotTitle)
  }else{
    fhObservedImpact <- fhObservedImpact + 
      ggtitle(plotTitle,
              subtitle = paste0(if_else(is.null(varNameDimension), 'input', 'dimension'), ' variables ', EnumerateAsText(varNamesInputMerged), ' are put in the category other because they have the least impact on average'))
  }
  
  # Each input variable requires a different line and points plot.
  for(varNameInput in varNamesInput){
    thisColour <- coloursImpact %>% filter(variable %in% varNameInput) %>% pull(colour)
    fhObservedImpact <- fhObservedImpact + 
      geom_line(data = dataPlot %>% filter(variable == varNameInput, facet != 'impact'), size = 1, colour = thisColour) + 
      geom_point(data = dataPlot %>% filter(variable == varNameInput, facet != 'impact'), colour = thisColour)
    if(isShowConfidenceInterval){
      fhObservedImpact <- fhObservedImpact + 
        geom_ribbon(data = dataPlot %>% filter(variable == varNameInput, facet != 'impact'), 
                    aes(ymin = mean - confidenceDistanceToMean * sqrt(var / n), 
                        ymax = mean + confidenceDistanceToMean * sqrt(var / n)),
                    alpha = 0.1,
                    colour = 'grey')
    } 
  }
  # TODO Make it optional to save as much space as possible, i.e. place the input variables as y-labels.
  #print(fhObservedImpact)
  # TODO make plotly have the same outpot as ggplot.
  #ggplotly(fhObservedImpact)
  return(fhObservedImpact)
} # function VisualiseImpact