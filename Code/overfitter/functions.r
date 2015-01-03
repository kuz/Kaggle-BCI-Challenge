#
#  Helper functions for The Overfitter
#

# Convert correlation matrix into table of pairwise correlations order by correlation value
# @param correlations: correlation matrix
# @return: data.frame with one pair per row
correlationpairs <- function(correlations) {
  modelnames <- colnames(correlations)
  corpairs <- data.frame()
  for (i in 1:nrow(correlations)) {
    if (i < nrow(correlations)) {
      for (j in (i + 1):(nrow(correlations))) { 
        newrow <- data.frame(modelnames[i], modelnames[j], correlations[i, j])
        colnames(newrow) <- colnames(corpairs)
        corpairs <- rbind.data.frame(corpairs, newrow)
      }
    }
  }
  colnames(corpairs) <- c('one', 'two', 'cor')
  corpairs <- corpairs[with(corpairs, order(cor)), ]
  return (corpairs)
}
