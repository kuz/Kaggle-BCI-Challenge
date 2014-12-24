#
#  Commonly used functions
#

# Given list of parameters and their values build a search grid for cross-validation
#
# @param parameters: list of parameters
#
# Usage example:
#   parameters <- list()
#   parameters[['n.trees']] <- c(10, 20)
#   parameters[['shrinkage']] <- c(0.01, 0.05)
#   parameters[['interaction.depth']] <- c(1, 2)
#   buildgrid(parameters)
#
buildgrid <- function(parameters) {
  
  # count number of rows in the resulting grid
  nvalues <- prod(sapply(parameters, length))
  
  # initialize resulting table with correct column names and correct number of rows
  results <- data.frame(matrix(vector(), nvalues, length(names(parameters)) + 2,
                               dimnames=list(c(), c(names(parameters), 'inscore', 'outscore', 'sd'))))
  
  # loop over parameters
  for (pidx in 1:length(parameters)) {
    pname = names(parameters)[pidx]
    
    # update counters
    if (pidx == 1) {
      plength <- nvalues / length(parameters[[pname]])
      ptimes <- 1
    } else {
      plength <- plength / length(parameters[[pname]])
      ptimes <- ptimes * length(parameters[[pidx - 1]])
    }
    
    # initialize resulting column with values for the current parameter
    column <- c()
    
    # loop over how many times to go over current parameter values
    for (t in 1:ptimes) {
      
      # loop over parameter vales
      for (pvalue in parameters[[pname]]) {
        
        # insert as many of them as counter requires
        column <- append(column, rep(pvalue, plength))
      }
    }
    
    # add column to the resulting table
    results[[pname]] <- column
    
  }
  
  return(results)
}