% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/summarySE.R
\name{summarySE}
\alias{summarySE}
\title{summarySE}
\usage{
summarySE(data = NULL, measurevars, groupvars = NULL, na.rm = TRUE,
  conf.interval = 0.95, .drop = TRUE, digits = 3)
}
\arguments{
\item{data}{A data.frame}

\item{measurevars}{The variable name or vector of variable names that will be summarized}

\item{groupvars}{A vector containing names of columns that contain grouping variables}

\item{na.rm}{If TRUE, NA values will be ignored}

\item{conf.interval}{The percent range of the confidence interval (default is 95%)}

\item{.drop}{Default is TRUE}

\item{digits}{Number of decimals to round output to (default is 3)}
}
\value{
Summary data.frame
}
\description{
summarySE provides summaries (mean, sd, ci, se, N) for a variable/variables either overall or stratified by one or more grouping variables. This is an extension of the summarySE function provided by Winston Chang in the Cookbook for R. This extension allows for getting summaries across multple outcome variables.
}
\examples{
#example_df <- data.frame(a = sample(c(0, 1), 100, replace = T),
#                         b = sample(letters[24:26], 100, replace = T),
#                         c = rnorm(100),
#                         d = rnorm(100, 15, 3))
#summarySE(example_df, measurevars="c")
#summarySE(example_df, measurevars="c", groupvars = "a")
#summarySE(example_df, measurevars=c("c", "d"))
#summarySE(example_df, measurevars=c("c", "d"), groupvars = "a")
}

