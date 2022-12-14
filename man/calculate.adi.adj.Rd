% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculate_ADI.adj.R
\name{calculate.adi.adj}
\alias{calculate.adi.adj}
\title{Calculate Anaerobic Index (AI)}
\usage{
calculate.adi.adj(dat, depth)
}
\arguments{
\item{depth}{rarefaction depth (uniform sample depth)}

\item{datobj}{a matrix of taxa relative abundances; sequencing sample identifiers are in row.names.}
}
\value{
a numerical vector of AI values
}
\description{
Calculate an anaerobic index (log ratio of sum RAs of
facultataive anaerobes/sum RAd of obligate anaeroebes); analogous to MDI
}
