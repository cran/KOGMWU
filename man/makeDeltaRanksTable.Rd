\name{makeDeltaRanksTable}
\alias{makeDeltaRanksTable}
\title{
Make a combined delta-ranks table from several kog.mwu() results. 
}
\description{
Extracts delta ranks from several kog.mwu() result tables and combines them into a single dataframe for heat map plotting and correlation analysis.
}
\usage{
makeDeltaRanksTable(ll)
}
\arguments{
  \item{ll}{
A list of dataframes output by kog.mwu() function.
}
}
\value{
A dataframe of delta-ranks (rows - KOG classes, columns - delta-ranks in different datasets).
}
\references{
Dixon GB, Davies SW, Aglyamova GA, Meyer E, Bay LK and Matz MV (2015) Genomic determinants of coral heat tolerance across latitudes. 
}
\author{
Mikhail V. Matz
}
