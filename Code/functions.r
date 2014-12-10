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
  results <- data.frame(matrix(vector(), nvalues, length(names(parameters)) + 1,
                               dimnames=list(c(), c(names(parameters), 'score'))))
  
  # counters
  plength <- ceiling(nvalues / 2)
  ptimes  <- 1
  
  # loop over parameters
  for (pname in names(parameters)) {
    
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
    
    # update counter for the next parameter
    plength <- plength / 2
    ptimes <- ptimes * 2
    
    # add column to the resulting table
    results[[pname]] <- column
  }
  
  return(results)
}