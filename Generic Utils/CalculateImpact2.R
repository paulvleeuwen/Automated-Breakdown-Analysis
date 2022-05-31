CalculateImpact2 <- function(dataRaw, varNameOutput, varNameDimension = NULL, maxNumInputVariables = Inf){
  
  # WISH make the variable name for the period generic, as well as for the optional entity variable.
  
  # TODO decide on the method to calculate the not-normalised impact. The Taylor
  # approximation is theoretically speaking the best but often leads to exploding remainder values.
  
  library(tidyverse)
  library(reshape2)
  

  thresholdReproductionError <- sqrt(.Machine$double.eps)
  dataProcess <- dataRaw %>%
    select(year, entity, regime, variable, value) %>%
    arrange(entity, year)
  nMissings <- sum(is.na(dataProcess))
  if(nMissings){
    warning( paste0( nMissings, ' values are missing and are omitted.') )
  }
  dataProcess <- na.omit(dataProcess)
  
  # Either take the mean of the mapping or the mapping of the mean.
  varNamesInput <-
    functionMappings %>% filter(output == varNameOutput) %>% pull(input)
  dataProcess <- dataProcess %>% 
    filter(variable %in% c(varNameOutput, varNamesInput)) %>%
    group_by(year, variable) %>% 
    summarise(value = mean(value)) %>%
    pivot_wider(
      id_cols = year,
      names_from = variable,
      values_from = value,
      values_fill = 0,
      values_fn = sum
    )
  functionAnalysis <-
    eval(parse(text = (
      functionMappings %>% filter(output == varNameOutput) %>% pull(mapping))[1]))
  dataProcess[,varNameOutput] <- functionAnalysis(dataProcess)
  
  # Temporarily rename the output variable to output to make dplyr operations
  # more easy. Set it back to the original variable name once all dplyr
  # operations with the output variable name involved are complete.
  dataProcess <- dataProcess %>%
    mutate(output = !!sym(varNameOutput)) %>%
    select(-!!sym(varNameOutput))
  
  # Verify that the output can be reproduced. If not, inform the user.
  outputReproduced <- functionAnalysis(dataProcess[, varNamesInput])
  dataProcess[, 'output reproduced'] <- outputReproduced
  dataProcess <- dataProcess %>%
    mutate(`is output verified` = `output reproduced` == output)
  maxReproductionError <- 
    max( dataProcess %>% mutate( `reproduction error` = abs( output - `output reproduced`) ) %>% pull(`reproduction error`) )
  if (maxReproductionError > thresholdReproductionError) {
    ixMaxReproductionError <- 
      max( dataProcess %>% mutate( `reproduction error` = abs( output - `output reproduced`) ) %>% pull(`reproduction error`) )
    exampleNotReproducible <- dataProcess[ixMaxReproductionError,]
    # TODO show the maximum deviation.
    warning(
      paste0(
        'Not all values of ',
        varNameOutput,
        ' could be reproduced, taking account of a numerical rounding error of ',
        thresholdReproductionError,
        '. For example, for farm ID = ',
        exampleNotReproducible$entity,
        ' in ',
        exampleNotReproducible$year,
        ' the ',
        varNameOutput,
        ' equals ',
        exampleNotReproducible$output,
        ' while the reproduced values equals ',
        exampleNotReproducible$`output reproduced`,
        '.'
      )
    )
  }
  dataProcess <- dataProcess %>% select(-`output reproduced`, -`is output verified`)
  
  # Determine the previous value of the variable to be analysed.
  
  # TODO What to do when varNameDimension = entity? For then the order_by does not work anymore?
  # if a year is missing, the value that is available before the current year is
  # selected as the value of the previous year.
  dataProcessPrevious <- dataProcess %>% 
    mutate(year = year + 1) %>% 
    select(year, `output previous` = output)
  dataProcess <- dataProcess %>% 
    left_join(dataProcessPrevious, by = 'year')
  
  # Change the value of each input variable to its lagged value, for each entity, year, and regime.
  for (varNameInput in varNamesInput) {
    # Create a temporay copy of dataProcess to reset the values of the output
    # variable back to its original values.
    dataProcessTemp <- dataProcess
    
    # Get the value of the input variable of the previous period.
    dataProcessTempPrevious <- dataProcess %>% 
      mutate(year = year + 1) %>% 
      select(year, `input previous` = !!sym(varNameInput))
    dataProcessTemp <- dataProcessTemp %>% 
      left_join(dataProcessTempPrevious, by = 'year')
    
    dataProcessTemp[, varNameInput] <- dataProcessTemp$`input previous`
    
    # Calculate the output variable with the input variables of which one is adjusted.
    dataProcessTemp[, 'output adjusted'] <- functionAnalysis(dataProcessTemp[, varNamesInput])
    dataProcess[, paste(varNameInput, 'impact not normalised')] <-
      dataProcessTemp %>%
      mutate(
        impact = output - `output adjusted`
      ) %>% pull(impact)
  }
  
  # Normalise the impact values.
  sumOfImpacts <-
    dataProcess %>% data.frame(check.names = FALSE) %>% select(ends_with(' impact not normalised')) %>% abs() %>% rowSums()
  for (varNameInput in varNamesInput) {
    dataProcess[, paste(varNameInput, 'impact')] <-
      abs(dataProcess[, paste(varNameInput, 'impact not normalised')]) / sumOfImpacts
  }
  
  # Split up between the observed and the impact dataset.
  dataObservedLong <- dataProcess %>%
    select(year, output, all_of(varNamesInput)) 
  dataImpactLong <- dataProcess %>%
    select(year, output, all_of(paste(varNamesInput, 'impact'))) %>%
    set_names(~str_replace(.x, ' impact$', ''))
  
  # When the number of input variables to be shown exceeds the observed number
  # of input variables, take the most relevant input variables.
  varNamesInputSortedImpact <- dataProcess %>%
    select(year, ends_with(' impact')) %>%
    group_by(year) %>%
    summarise(across(.fns = mean, na.rm = TRUE)) %>%
    pivot_longer(-year, names_to = 'variable') %>%
    group_by(variable) %>%
    summarise(mean = mean(value, na.rm = TRUE)) %>%
    arrange(desc(mean)) %>% 
    mutate(variable = str_replace(variable, ' impact$', '')) %>%
    pull(variable)
  dataProcess <- as.data.frame(dataProcess)
  if(length(varNamesInput) > 2 &
     maxNumInputVariables < length(varNamesInputSortedImpact)) { # minus 1 for the period
    
    # Store the original variable names so that they can be shown in the plot title.
    varNamesInputOriginal <- varNamesInputSortedImpact
    varNamesInputImpactTop <-
      varNamesInputSortedImpact[1:(maxNumInputVariables - 1)] # minus 1 to keep one panel for the remaining input variables with the least impact
    varNamesInputToOther <-
      str_replace(setdiff(varNamesInputSortedImpact, varNamesInputImpactTop),
                  ' impact$',
                  '')
    
    # Merge the variables to be merged.
    dataObservedLong[,'other'] <- dataProcess %>% select(all_of(varNamesInputToOther)) %>% rowSums()
    dataObservedLong <- dataObservedLong %>%  
      select(-all_of(varNamesInputToOther))
    dataImpactLong[,'other'] <- dataProcess %>% select(all_of(paste(varNamesInputToOther, 'impact'))) %>% rowSums()
    dataImpactLong <- dataImpactLong %>%
      select(-all_of(varNamesInputToOther))

    # Make sure the input variable names merged in the variable other is last.
    varNamesInput <- c(varNamesInputImpactTop, 'other')
    
  } else{
    varNamesInput <- varNamesInputSortedImpact
    varNamesInputToOther <- NULL
  }
  dataObserved <- dataObservedLong %>% 
    select(year, output, all_of(varNamesInput)) %>% 
    pivot_longer(-year, names_to = 'variable') %>%
    group_by(year, variable) %>% 
    summarise(
      across(.fns = list(mean = ~mean(.x, na.rm = TRUE), var = ~var(.x, na.rm = TRUE), n = ~sum(!is.na(.x))),
             .names = '{.fn}'),
      .groups = 'drop')
  stopifnot( all(colnames(dataObserved) == c('year', 'variable', 'mean', 'var', 'n') ) )
  dataImpact <- dataImpactLong %>% 
    select(year, all_of(varNamesInput)) %>% 
    pivot_longer(-year, names_to = 'variable') %>%
    group_by(year, variable) %>% 
    summarise(
      across(.fns = list(mean = ~mean(.x, na.rm = TRUE), var = ~var(.x, na.rm = TRUE), n = ~sum(!is.na(.x))),
             .names = '{.fn}'),
      .groups = 'drop')
  stopifnot( all(colnames(dataObserved) == c('year', 'variable', 'mean', 'var', 'n') ) )
  
  # Rename the output column back to the original output column name.
  dataObserved <- dataObserved %>% 
    mutate(variable = str_replace(variable, 'output', varNameOutput))
  
  # Sanity check that the mean and variance for the first year are NaN values.
  if(dataImpact %>% filter(is.nan(mean) | is.na(var)) %>% nrow != length(varNamesInput))
    #stop('The values for the first year should be NaN.')
  dataImpact$mean[1:length(varNamesInput)] <- NA # both the mean and variance are not available for the first year
  
  # Make sure that the ordering of the variables ranges from the variables with
  # the most to the least impact.
  dataImpact <- dataImpact %>%
    mutate(variable = factor(variable, levels = varNamesInput))
  dataObserved <- dataObserved %>%
    mutate(variable = factor(variable, levels = c(varNameOutput, varNamesInput)))
  
  return(list(dataObserved = dataObserved, 
              dataImpact = dataImpact,
              varNamesInputSortedImpact = varNamesInputSortedImpact,
              varNamesInputMerged = varNamesInputToOther))
}