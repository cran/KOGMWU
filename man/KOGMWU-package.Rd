\name{KOGMWU-package}
\alias{KOGMWU-package}
\alias{KOGMWU}
\docType{package}
\title{
Functional summary and meta-analysis of gene expression data
}
\description{
Rank-based tests for enrichment of KOG (euKaryotic Orthologous Groups) classes with up- or down-regulated genes based on a continuous measure. The meta-analysis is based on correlation of KOG delta-ranks across datasets (delta-rank is the difference between mean rank of genes belonging to a KOG class and mean rank of all other genes). With binary measure (1 or 0 to indicate significant and non-significant genes), one-tailed Fisher's exact test for over-representation of each KOG class among significant genes will be performed. 
}
\details{
\tabular{ll}{
Package: \tab KOGMWU\cr
Type: \tab Package\cr
Version: \tab 1.2\cr
Date: \tab 2019-02-19\cr
License: \tab GPL-3\cr
}
The most important function is kog.mwu, which performs a series of Mann-Whitney U tests when given two data tables: one, containing measures of interest for each gene (for example, log fold-change), and another, listing the association of each gene with a KOG class. The KOG class annotations for a collection of genes can be obtained using eggNOG-mapper: http://eggnogdb.embl.de/#/app/emapper. To extract KOG annotations understood by this package out of the eggNOG-mapper output, see here: https://github.com/z0on/emapper_to_GOMWU_KOGMWU  
}
\author{
Mikhail V. Matz

Maintainer: Mikhail V. Matz <matz@utexas.edu>
}
\references{
Dixon, G. B., Davies, S. W., Aglyamova, G. V., Meyer, E., Bay, L. K. and Matz, M. V. Genomic determinants of coral heat tolerance across latitudes. Science 2015, 348:1460-1462. 
eggNOG-mapper to obtain KOG annotations: http://eggnogdb.embl.de/#/app/emapper
To extract KOG annotations from eggNOG-mapper output: https://github.com/z0on/emapper_to_GOMWU_KOGMWU
}
\keyword{ package }
\examples{
\dontrun{
data(adults.3dHeat.logFoldChange)
data(larvae.longTerm)
data(larvae.shortTerm)
data(gene2kog)

# Analyzing adult coral response to 3-day heat stress:
alfc.lth=kog.mwu(adults.3dHeat.logFoldChange,gene2kog) 
alfc.lth 

# coral larvae response to 5-day heat stress:
l.lth=kog.mwu(larvae.longTerm,gene2kog)
l.lth

# coral larvae response to 4-hour heat stress 
l.sth=kog.mwu(larvae.shortTerm,gene2kog)
l.sth

# compiling a table of delta-ranks to compare these results:
ktable=makeDeltaRanksTable(list("adults.long"=alfc.lth,"larvae.long"=l.lth,"larvae.short"=l.sth))

# Making a heatmap with hierarchical clustering trees: 
pheatmap(as.matrix(ktable),clustering_distance_cols="correlation") 

# exploring correlations between datasets
pairs(ktable, lower.panel = panel.smooth, upper.panel = panel.cor)
# p-values of these correlations in the upper panel:
pairs(ktable, lower.panel = panel.smooth, upper.panel = panel.cor.pval)

# plotting individual delta-rank correlations:
corrPlot(x="adults.long",y="larvae.long",ktable)
corrPlot(x="larvae.short",y="larvae.long",ktable)
}
}
