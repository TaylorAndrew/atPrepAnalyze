#' removeLowEventCounts
#'
#' @param data data.frame containing data
#' @param event variable name of the event variable
#' @param contin vector of continuous variables
#' @param cat vector of categorical variables
#' @param minCount Minimum total number of non-NA entries a variable can have and still be included in the analyses.
#' @param maxLevels Maximum number of levels a categorical variable can have and still be included in the analysis.
#'
#' @return a list of varibale name vectors to include in a later analysis.
#' @export
#'
#' @examples
#' #NULL
removeLowEventCounts <-
  function(data, event, contin, cat, minCount, maxLevels) {
    e <- new.env()
    e$Output <- list()
    if (!is.null(contin)) {
      e$Output$ContinList <- contin
      f <- function(x) {
        j <- subset(data,!is.na(data[, x]))
        numevents <- length(j[, event][j[, event] == 1])
        if (numevents < minCount)
          e$Output$ContinList <-
          e$Output$ContinList[!e$Output$ContinList %in% x]
      }
      lapply(contin, f)
    }
    if (!is.null(cat)) {
      e$Output$CatList <- cat
      f <- function(x) {
        j <- table(df[, x], df[, event])[,2]
        numevents <- min(j)
        NLevels <- length(j)
        if (numevents < minCount)
          e$Output$CatList <- e$Output$CatList[!e$Output$CatList %in% x]
        if (NLevels > maxLevels)
          e$Output$CatList <- e$Output$CatList[!e$Output$CatList %in% x]
      }
      lapply(cat, f)
    }
    return(e$Output)
  }
