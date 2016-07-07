#' summarySE
#'
#' summarySE provides summaries (mean, sd, ci, se, N) for a variable/variables either overall or stratified by one or more grouping variables. This is an extension of the summarySE function provided by Winston Chang in the Cookbook for R. This extension allows for getting summaries across multple outcome variables.
#'
#' @param data A data.frame
#' @param measurevars The variable name or vector of variable names that will be summarized
#' @param groupvars A vector containing names of columns that contain grouping variables
#' @param na.rm If TRUE, NA values will be ignored
#' @param conf.interval The percent range of the confidence interval (default is 95%)
#' @param .drop Default is TRUE
#' @param digits Number of decimals to round output to (default is 3)
#'
#' @return Summary data.frame
#' @export
#'
#' @examples
#' #example_df <- data.frame(a = sample(c(0, 1), 100, replace = T),
#' #                         b = sample(letters[24:26], 100, replace = T),
#' #                         c = rnorm(100),
#' #                         d = rnorm(100, 15, 3))
#' #summarySE(example_df, measurevars="c")
#' #summarySE(example_df, measurevars="c", groupvars = "a")
#' #summarySE(example_df, measurevars=c("c", "d"))
#' #summarySE(example_df, measurevars=c("c", "d"), groupvars = "a")
summarySE <-
  function(data = NULL, measurevars, groupvars = NULL, na.rm = TRUE,
           conf.interval = .95, .drop = TRUE, digits = 3) {
    detach_package <- function(pkg, character.only = FALSE)
    {
      if (!character.only)
      {
        pkg <- deparse(substitute(pkg))
      }
      search_item <- paste("package", pkg, sep = ":")
      while (search_item %in% search())
      {
        detach(search_item, unload = TRUE, character.only = TRUE)
      }
    }
    detach_package(dplyr)
    print(
      "Warning: dplyr will be detached in order to run this function. You must re-attach manually"
    )
    do_one <- function(measurevar) {
      # New version of length which can handle NA's: if na.rm==T, don't count them
      length2 <- function (x, na.rm = FALSE) {
        if (na.rm)
          sum(!is.na(x))
        else
          length(x)
      }
      # This does the summary. For each group's data frame, return a vector with
      # N, mean, and sd
      datac <- ddply(
        data, groupvars, .drop = .drop,
        .fun = function(xx, col) {
          c(
            N    = length2(as.numeric(as.character(xx[[col]])), na.rm = na.rm),
            mean = mean   (as.numeric(as.character(xx[[col]])), na.rm = na.rm),
            sd   = sd     (as.numeric(as.character(xx[[col]])), na.rm = na.rm)
          )
        },
        measurevar
      )
      datac <- data.frame(Var = measurevar,datac)
      # Rename the "mean" column
      datac$mean <- round(datac$mean, digits = digits)
      datac$sd <- round(datac$sd, digits = digits)
      # datac <- rename(datac, c("mean" = measurevar))
      datac$se <-
        round(datac$sd / sqrt(datac$N),digits = digits)  # Calculate standard error of the mean
      # Confidence interval multiplier for standard error
      # Calculate t-statistic for confidence interval:
      # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
      ciMult <- round(qt(conf.interval / 2 + .5, datac$N - 1), digits = 3)
      datac$ci <- round(datac$se * ciMult,digits = digits)
      return(datac)
    }
    detac_full <- do.call(rbind, lapply(measurevars, do_one))
    detac_full$.id <- NULL
    if(length(measurevars)==1) detac_full$Var <- NULL
    return(detac_full)
  }
