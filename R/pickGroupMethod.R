#' pickGroupMethod
#'
#' pickGroupMethod will test common assumptions for parametric and non-parametric methods for 2 + group methods (e.g. t.test, mann-whitney U, chisq, fisher, ANOVA) and will decide which method to use for each variable.
#'
#' @param data data.frame containing all data
#' @param continList continList Vector of continuous variable names
#' @param catList catList Vector of categorical variable names
#' @param grouping grouping Grouping variable to test for equal variance and other multiple group based assumptions
#' @param minCount Minimum total count
#' @param minExpectedCountProportion Minimum proportion of cells that have an expected count of 5 or more to still use Chi.square test. Default of 75%
#' @param maxLevelsCutoff  Maximum number of unique levels to a variable to still analyze. If the variable exceeds this cutoff it will be dropped from the analysis lists.
#' @param dropOneRow If TRUE, any variables with only 1 unique level will be dropped from analysis lists.
#' @param dropNLessThan Minimum number non-NA responses a variable must have before being dropped from the analysis lists. Default of 1
#' @param testNormality If TRUE, normality assumptions are checked for continuous variables
#' @param minDiscreteValues minDiscreteValues Minimum number of discrete values of a continuous variable to still be tested with parametric methods.
#' @param varEqual If TRUE, and grouping is non-NULL, equal variance assumptions will be tested
#' @param verbose If TRUE, messages about decision criteria will be printed to the console.
#'
#' @return List of vectors indicating whether variances should be tested parametrically, non-parametrically or dropped, for continuous and categorical lists separately
#' @export
#'
#' @examples
#' #NULL
pickGroupMethod <- function(data,
                          continList = NULL,
                          catList = NULL,
                          grouping = NULL,
                          minCount = NULL,
                          minExpectedCountProportion = .75,
                          maxLevelsCutoff = NULL,
                          dropOneRow = T,
                          dropNLessThan = 1,
                          testNormality = T,
                          minDiscreteValues = NULL,
                          varEqual = T,
                          verbose = T) {
  e <- new.env()
  e$output <- list()
  if (!is.null(continList)) {
    e$output$PcontinList <- continList
    e$output$NPcontinList <- continList
    e$output$continDrop <- continList
    f1 <- function(i) {
      e$UseVar <- continList[i]
      j <- new.env()
      j$Decide <- list()
      totSS <-
        length(data[,e$UseVar][!is.na(as.numeric(as.character(data[,e$UseVar])))])

      if (testNormality & totSS >= 3) {
        j$Decide$Normal <-
          shapiro.test(as.numeric(as.character(data[,e$UseVar])))$p.value > .05
        if(verbose==T & j$Decide$Normal==F) {
          print(paste0(e$UseVar, " significantly differed from the normal distribution via the shapiro test"))
        }
      }
      if (testNormality & totSS < 3) {
        j$Decide$Normal <- FALSE
      }
      if(!is.null(minDiscreteValues)) {
        j$Decide$DiscreteLevels <-
        length(levels(factor(data[,e$UseVar]))) >= minDiscreteValues
        if(verbose==T & j$Decide$DiscreteLevels==F) {
          print(paste0(e$UseVar, " did not meet the minimum levels of discrete values"))
        }
      }
      if (!is.null(grouping)) {
        get.lengths <- function(group_i) {
          xxxx <-
            unlist(subset(data, data[,grouping] == group_i, select = e$UseVar))
          nxxxx <- length(xxxx[!is.na(as.numeric(as.character(xxxx)))])
          return(nxxxx)
        }
        ns <-
          do.call(c, lapply(levels(factor(data[,grouping])), get.lengths))
      }
      if (!is.null(grouping) & varEqual) {
        j$Decide$equalVar <- bartlett.test(as.numeric(as.character(data[, e$UseVar])) ~
                                             factor(data[, grouping]))$p.value > .05
      }

      if (min(as.numeric(unlist(j$Decide[!is.na(j$Decide)]))) == 0) {
        e$output$PcontinList <-
          e$output$PcontinList[!e$output$PcontinList %in% e$UseVar]
      } else {
        e$output$NPcontinList <-
          e$output$NPcontinList[!e$output$NPcontinList %in% e$UseVar]
      }

    }
    suppressWarnings(lapply(1:length(continList), f1))

    f4 <- function(i) {
      e$UseVar <- continList[i]
      totSS <-
        length(data[,e$UseVar][!is.na(as.numeric(as.character(data[,e$UseVar])))])
      DropIfN <- totSS < dropNLessThan
      if (verbose == T & DropIfN == T) {
        print(
          paste0(
            e$UseVar,
            ": Dropped Due to total Sample Size is less than ",
            dropNLessThan
          )
        )
      }
      if (DropIfN) {
        e$output$PcontinList <-
          e$output$PcontinList[!e$output$PcontinList %in% e$UseVar]
        e$output$NPcontinList <-
          e$output$NPcontinList[!e$output$NPcontinList %in% e$UseVar]
      } else {
        e$output$continDrop <-
          e$output$continDrop[!e$output$continDrop %in% e$UseVar]
      }
    }
    suppressWarnings(lapply(1:length(continList), f4))
  }
  if (!is.null(catList)) {
    e$output$PcatList <- catList
    e$output$NPcatList <- catList
    e$output$catDrop <- catList
    f2 <- function(i) {
      g <- new.env()
      g$Decide <- list()
      e$UseVar <- catList[i]
      if (is.null(grouping)) {
        fulltab <- data.frame(table(data[,e$UseVar]))$Freq
      } else {
        fulltab <-
        data.frame(chisq.test(data[,e$UseVar], data[, grouping])$expected)
      }
      g$Decide$perAbove5 <- mean(fulltab >= 5) >= minExpectedCountProportion
      if(verbose ==T & g$Decide$perAbove5 ==F) {
        print(paste0(e$UseVar, ": Fisher's exact test will be utilized as less than ",
                     minExpectedCountProportion, "% of the cells had an expected count of 5 or more"))
      }
      if(!is.null(minCount)) g$Decide$AboveMinCount <- min(fulltab) >= minCount
      if (min(as.numeric(unlist(g$Decide)), na.rm = T) == 0) {
        e$output$PcatList <-
          e$output$PcatList[!e$output$PcatList %in% e$UseVar]
      } else {
        e$output$NPcatList <-
          e$output$NPcatList[!e$output$NPcatList %in% e$UseVar]
      }
    }
    f3 <- function(i) {
      f <- new.env()
      e$UseVar <- catList[i]
      tt <- data.frame(table(data[, e$UseVar]))$Freq
      f$DropIf <- F
      if (dropOneRow == T) {
        f$DropIf <- length(tt) == 1

        if (verbose == T & f$DropIf == T) {
          print(paste0(e$UseVar,
                       ": Dropped Due to only having one level"))
        }
      }
      if (f$DropIf == F & !is.null(maxLevelsCutoff)) {
        f$DropIf <-  length(tt) > maxLevelsCutoff
        if (verbose == T & f$DropIf == T) {
          print(
            paste0(
              e$UseVar,
              ": Dropped Due to the number of levels (",
              length(tt),
              ") being greater than the allowed maximum levels: ",
              maxLevelsCutoff
            )
          )
        }
      }

      fulltab <- data.frame(table(data[,e$UseVar]))$Freq

      f$DropIf2 <- is.na(mean(fulltab))

      if (!is.null(grouping)) {
        tab2 <- table(data[,e$UseVar], data[, grouping])
        f$DropIf3 <- min(rowSums(tab2)) == 0
        if (verbose == T & f$DropIf3 == T) {
          print(
            paste0(
              e$UseVar,
              ": Dropped Due to no observations for one ",
              e$UseVar,
              "Level"
            )
          )
        }
        f$DropIf4 <- min(colSums(tab2)) == 0
        if (verbose == T & f$DropIf4 == T) {
          print(paste0(
            e$UseVar,
            ": Dropped Due to no observations for one Group Level"
          ))
        }
      } else {
        f$DropIf3 <- F
        f$DropIf4 <- F
      }
      if (f$DropIf | f$DropIf2 | f$DropIf4 | f$DropIf4) {
        e$output$NPcatList <-
          e$output$NPcatList[!e$output$NPcatList %in% e$UseVar]
        e$output$PcatList <-
          e$output$PcatList[!e$output$PcatList %in% e$UseVar]
      } else {
        e$output$catDrop <-
          e$output$catDrop[!e$output$catDrop %in% e$UseVar]
      }
    }
    suppressWarnings(lapply(1:length(catList), f2))
    suppressWarnings(lapply(1:length(catList), f3))
  }
  if (length(e$output$PcontinList) == 0)
    e$output$PcontinList <- NULL
  if (length(e$output$NPcontinList) == 0)
    e$output$NPcontinList <- NULL
  if (length(e$output$continDrop) == 0)
    e$output$continDrop <- NULL
  if (length(e$output$PcatList) == 0)
    e$output$PcatList <- NULL
  if (length(e$output$NPcatList) == 0)
    e$output$NPcatList <- NULL
  if (length(e$output$catDrop) == 0)
    e$output$catDrop <- NULL
  return(e$output)
}
