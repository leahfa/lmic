% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/prep_dat_functions.R
\name{prep.metaphlan}
\alias{prep.metaphlan}
\title{Summarize by Taxonomy}
\usage{
prep.metaphlan(pathToMeta, taxlevel = "genus")
}
\arguments{
\item{pathToMeta}{path to merged metaphlan output (txt)}

\item{taxlevel}{required level to output}
}
\value{
dob.ff numerical matrix of the required phylo level only; samples in rows
}
\description{
Summarize ASV table to required phylogenetic level (default is genus)
}
