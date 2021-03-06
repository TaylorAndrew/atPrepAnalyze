% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/removeLowEventCounts.R
\name{removeLowEventCounts}
\alias{removeLowEventCounts}
\title{removeLowEventCounts}
\usage{
removeLowEventCounts(data, event, contin, cat, minCount, maxLevels)
}
\arguments{
\item{data}{data.frame containing data}

\item{event}{variable name of the event variable}

\item{contin}{vector of continuous variables}

\item{cat}{vector of categorical variables}

\item{minCount}{Minimum total number of non-NA entries a variable can have and still be included in the analyses.}

\item{maxLevels}{Maximum number of levels a categorical variable can have and still be included in the analysis.}
}
\value{
a list of varibale name vectors to include in a later analysis.
}
\description{
removeLowEventCounts
}
\examples{
#NULL
}

