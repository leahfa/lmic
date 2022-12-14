% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/compare_lm_loops.R
\name{compare_lm_loops}
\alias{compare_lm_loops}
\title{Summarize results of lm_loop across subcohorts}
\usage{
compare_lm_loops(keyobj, datobj, cohorts, yfv, extra.vars, ...)
}
\arguments{
\item{keyobj}{a dataframe of metadata; sequencing sample identifiers in collumn "ID"}

\item{datobj}{a matrix of taxa absolute/relative abundances; sequencing sample identifiers are in row.names. Rare taxa should be removed.}

\item{cohorts}{cahacter vector of names of subcohorts}

\item{yfv}{name of variable am inetersted in}

\item{extra.vars}{character vector of additional predictors for model (names of relevant columns in keyobj)}
}
\value{
table of y
}
\description{
Run the same lm_loop on different cohorts or subsets; combine teh ersults of all subsets/cohorts
}
