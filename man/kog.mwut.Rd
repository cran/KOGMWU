\name{kog.mwut}
\alias{kog.mwut}
\title{
Mann-Whitney U test for KOG enrichment.
}
\description{
Accessory function to kog.mwu()
}
\usage{
kog.mwut(gos, Alternative = "t")
}
\arguments{
  \item{gos}{
A dataframe with three columns, 'seq' (gene id),'term' (KOG class) and 'value' (continuous measure, such as log fold-change).
}
  \item{Alternative}{
Tailedness of the MWU test: two-tailed ("t"), greater-than ("g"), or less-than ("l")
}
}
\value{
A dataframe with three columns: 'term', 'nseqs', 'delta.rank', 'pval' and 'padj'
}
\references{
Dixon GB, Davies SW, Aglyamova GA, Meyer E, Bay LK and Matz MV (2015) Genomic determinants of coral heat tolerance across latitudes. 
}
\author{
Mikhail V. Matz
}
