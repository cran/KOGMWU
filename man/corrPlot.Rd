\name{corrPlot}
\alias{corrPlot}
\title{
Plots a pairwise correlation with loess smoothing
}
\description{
Plots Pearson's correlation between two columns in a dataframe, identified by column names. Also plots loess smoothing line and lists the correlation coefficient (r) and cor.test p-value.
}
\usage{
corrPlot(x, y, data, ...)
}
\arguments{
  \item{x}{
Name of the column to form X axis
}
  \item{y}{
Name of the column to form Y axis
}
  \item{data}{
The dataframe containing the two columns
}
  \item{\dots}{
Additional options for plot()
}
}
\references{
Dixon GB, Davies SW, Aglyamova GA, Meyer E, Bay LK and Matz MV (2015) Genomic determinants of coral heat tolerance across latitudes. 
}
