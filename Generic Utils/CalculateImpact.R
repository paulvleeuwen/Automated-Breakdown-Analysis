CalculateImpact <- function(dataRaw, varNameOutput, varNameDimension = NULL, maxNumInputVariables = Inf){
  
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
  
  varNamesDimensionLeft <- setdiff(varNamesDimension, varNameDimension)
  if(is.null(varNameDimension)){
    varNamesInput <-
      functionMappings %>% filter(output == varNameOutput) %>% pull(input)
    dataProcess <- dataProcess %>%
      filter(variable %in% c(varNameOutput, varNamesInput)) %>%
      pivot_wider(
        id_cols = all_of(varNamesDimension),
        names_from = variable,
        values_from = value,
        values_fill = 0,
        values_fn = sum
      )
    functionAnalysis <-
      eval(parse(text = (
        functionMappings %>% filter(output == varNameOutput) %>% pull(mapping))[1]))
    functionsDerivative <- list()
    for(varNameInput in varNamesInput){
      thisFunctionText <- functionMappings %>% filter(output == varNameOutput, input == varNameInput) %>% pull(derivative)
      functionsDerivative[[varNameInput]] <- eval(parse(text = thisFunctionText) )
    }
  }else{
    varNamesInput <- unique(dataRaw[,varNameDimension] %>% pull())
    dataProcess <- dataProcess %>%
      filter(variable == varNameOutput) %>%
      pivot_wider(
        id_cols = all_of(varNamesDimensionLeft),
        names_from = !!sym(varNameDimension),
        values_from = value,
        values_fill = 0, # it could be that not all categories of the dimension variable are present
        values_fn = sum
      )
    functionAnalysis <-
      eval(parse(text = paste( 'function(data) rowSums(data[, c(', paste0('\'', varNamesInput, '\'', collapse = ', '), ')])')))
    functionsDerivative <- list()
    for(varNameInput in varNamesInput){
      thisFunctionText <- functionMappings %>% filter(output == varNameOutput, input == varNameInput) %>% pull(derivative)
      functionsDerivative[[varNameInput]] <- eval(parse(text = 'function(data, nthDerivative)  ifelse(nthDerivative == 1, rep(1, nrow(data)), rep(0, nrow(data)))') )
    }
    dataProcess[,varNameOutput] <- functionAnalysis(dataProcess)
  }
  
  # Temporarily rename the output variable to output to make dplyr operations
  # more easy. Set it back to the original variable name once all dplyr
  # operations with the output variable name involved are complete.
  dataProcess <- dataProcess %>%
    mutate(output = !!sym(varNameOutput)) %>%
    select(-!!sym(varNameOutput))
  
  # Verify that the output can be reproduced. If not, inform the user.
  dataProcess %>% group_by(year) %>% summarise(across(all_of(varNamesInput), mean))
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
  varNamesGroupBy <- setdiff(varNamesDimensionLeft, 'year')
  # TODO What to do when varNameDimension = entity? For then the order_by does not work anymore?
  # if a year is missing, the value that is available before the current year is
  # selected as the value of the previous year.
  dataProcessPrevious <- dataProcess %>% 
    mutate(year = year + 1) %>% 
    select(year, entity, `output previous` = output)
  dataProcess <- dataProcess %>% 
    left_join(dataProcessPrevious, by = c('year', 'entity'))
  
  # Change the value of each input variable to its lagged value, for each entity, year, and regime.
  for (varNameInput in varNamesInput) {
    # Create a temporay copy of dataProcess to reset the values of the output
    # variable back to its original values.
    dataProcessTemp <- dataProcess
    
    # Get the value of the input variable of the previous period.
    dataProcessTempPrevious <- dataProcess %>% 
      mutate(year = year + 1) %>% 
      select(year, entity, `input previous` = !!sym(varNameInput))
    dataProcessTemp <- dataProcessTemp %>% 
      left_join(dataProcessTempPrevious, by = c('year', 'entity'))
    
    # By making use of the Taylor approximation.
    thisImpact <- dataProcessTemp
    thisImpact[,varNameInput] <- thisImpact$`input previous`
    thisImpact$`total accrued costs per farm` <- 1
    nMaxDerivative <- 10
    delta <- dataProcessTemp[, varNameInput] - dataProcessTemp$`input previous`
    nMaxDerivative <- min( 1, functionMappings %>% filter(output == varNameOutput, input == varNameInput) %>% pull(`maximum number of non-zero derivative`) )
    for(iDerivative in 1:nMaxDerivative){
      newVarName <- paste('term', iDerivative)
      # TODO check whether taking the exponential of the log has better numerical rounding errors.
      thisImpact[, newVarName] <- delta^iDerivative * functionsDerivative[[varNameInput]](thisImpact, iDerivative) / factorial(iDerivative)
      
      # Continue as long as the approximation error of the current Taylor
      # expansion around the previous value evaluated at the current value is
      # too high.
      thisImpact <- thisImpact %>%
        mutate(`output approximated` = rowSums( thisImpact %>% select(`output previous`, starts_with('term ') ) ),
               `approximation error` = abs(output - `output approximated`))

      if( !any(thisImpact %>% slice_max(`approximation error`) >= thresholdReproductionError))
        break
    }
    dataProcess[, paste(varNameInput, 'impact not normalised Taylor')] <- rowSums( thisImpact %>% select(starts_with('term ')) )
    
    dataProcessTemp[, varNameInput] <- dataProcessTemp$`input previous`
    
    # Calculate the output variable with the input variables of which one is adjusted.
    dataProcessTemp[, 'output adjusted'] <- functionAnalysis(dataProcessTemp[, varNamesInput])
    dataProcess[, paste(varNameInput, 'impact not normalised')] <-
      dataProcessTemp %>%
      mutate(
        `output change` = output - `output previous`,
        impact = if_else(
          `output change` == 0,
          0,
          (output - `output adjusted`) / `output change`
        )
      ) %>% pull(impact)
  }
  
  # Normalise the impact values.
  sumOfImpacts <-
    dataProcess %>% data.frame(check.names = FALSE) %>% select(ends_with(' impact not normalised')) %>% abs() %>% rowSums()
  sumOfImpactsTaylor <-
    dataProcess %>% data.frame(check.names = FALSE) %>% select(ends_with(' impact not normalised Taylor')) %>% abs() %>% rowSums()
  for (varNameInput in varNamesInput) {
    dataProcess[, paste(varNameInput, 'impact')] <-
      abs(dataProcess[, paste(varNameInput, 'impact not normalised')]) / sumOfImpacts
    dataProcess[, paste(varNameInput, 'impact Taylor')] <-
      abs(dataProcess[, paste(varNameInput, 'impact not normalised Taylor')]) / sumOfImpactsTaylor
  }
  
  # Split up between the observed and the impact dataset.
  dataObservedLong <- dataProcess %>%
    select(all_of(varNamesDimensionLeft), output, all_of(varNamesInput)) 
  dataImpactLong <- dataProcess %>%
    select(all_of(varNamesDimensionLeft), output, all_of(paste(varNamesInput, 'impact'))) %>%
    set_names(~str_replace(.x, ' impact$', ''))
  dataImpactLongTaylor <- dataProcess %>%
    select(all_of(varNamesDimensionLeft), output, all_of(paste(varNamesInput, 'impact Taylor'))) %>%
    set_names(~str_replace(.x, ' impact Taylor$', ''))
  #dataImpactLong <- dataImpactLongTaylor
  
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
    valuesOther <- dataProcess %>% select(all_of(varNamesInputToOther)) %>% rowSums()
    dataObservedLong <- dataObservedLong %>%  
      mutate(other = valuesOther) %>%
      select(-all_of(varNamesInputToOther))
    valuesOther <- dataProcess %>% select(all_of(paste(varNamesInputToOther, 'impact'))) %>% rowSums()
    dataImpactLong <- dataImpactLong %>%
      mutate(other = valuesOther) %>%
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
    stop('The values for the first year should be NaN.')
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