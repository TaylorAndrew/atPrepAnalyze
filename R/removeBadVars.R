#' removeBadVars
#'
#' removeBadVars checks the variables in a dataframe, and removes those that do not meet the given parameters. To be used in conjunction with the multLogistic() function.
#'
#' @param data input data.frame
#' @param catlist vector of categorical variable names to use in multLogistic()
#' @param predlist vector of all variable names to use in multLogistic()
#' @param nLevelsCutoff Maximum number of levels a categorical variable can have and still be included in the analysis.
#' @param minCount Minimum total number of non-NA entries a variable can have and still be included in the analyses.
#'
#' @return a list of varibale name vectors to include in a later analysis.
#' @export
#'
#' @examples
#' #NULL
removeBadVars <-
  function(data, catlist, predlist, nLevelsCutoff = 5, minCount = 1) {
    e <- new.env()
    e$catlist <- catlist
    e$predlist <- predlist
    f <- function(i) {
      e$currentvar <- catlist[i]
      l <- data.frame(table(data[, e$currentvar]))
      if (min(l$Freq) == minCount |
          length(l[,1]) > nLevelsCutoff | length(l[,1]) == 1) {
        e$catlist <- e$catlist[!e$catlist %in% e$currentvar]
        e$predlist <- e$predlist[!e$predlist %in% e$currentvar]
      }
    }
    lapply(1:length(catlist), f)
    return(list(e$catlist, e$predlist))
  }
