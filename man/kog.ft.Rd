\name{kog.ft}
\alias{kog.ft}
\title{
One-tailed Fisher's exact test for KOG enrichment.
}
\description{
Accessory function to kog.mwu()
}
\usage{
kog.ft(gos)
}
\arguments{
  \item{gos}{
A dataframe with three columns, 'seq' (gene id),'term' (KOG class) and 'value' (either 0 or 1, indicating significance).
}
}
\value{
A dataframe with three columns: 'term', 'nseqs', 'pval' and 'padj'
}
\references{
Dixon GB, Davies SW, Aglyamova GA, Meyer E, Bay LK and Matz MV (2015) Genomic determinants of coral heat tolerance across latitudes. 
}
\author{
Mikhail V. Matz
}
