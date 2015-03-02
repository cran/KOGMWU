\name{kog.mwu}
\alias{kog.mwu}
\title{
Tests for KOG class enrichment.
}
\description{
Determines whether some KOG classes are significantly enriched with up- or down-regulated genes (Mann-Whitney U test for continuous measure), or whether some KOG classes are significantly over-represented among "significant" genes (one-tailed Fisher's exact test for binary measure, 0 or 1).
}
\usage{
kog.mwu(data, gene2kog, Alternative = "t")
}
\arguments{
  \item{data}{
Two-column dataframe: gene id, measure of significance. 
}
  \item{gene2kog}{
Two-column dataframe of gene annotations: gene id, KOG class. The gene list can be longer or shorter than the first column in the 'data' item.
}
  \item{Alternative}{
Tailedness of the Mann-Whitney U test: two-tailed ("t"), greater ("g"), or less ("l")
}
}
\details{
The measure can be continuous (such as log fold change), in which case Mann-Whitney U test will be performed, or binary (1 or 0: significant or not), in which case Fisher's exact test will be performed. The KOG class annotations for a collection of genes can be obtained using Weizhng Li's lab KOG BLAST server.
}
\value{
For continuous measure, a dataframe with three columns: 
       term : KOG class
      nseqs : Number of genes in this class
 delta.rank : Difference between the mean rank of genes belonging to this KOG class and all other genes
       pval : p-value of the Mann-Whitney U test
       padj : p-value adjusted using Benjamini-Hochberg 1995 "fdr" procedure

For binary measure, the output is similar but does not contain the delta.rank column.
}
\references{
Dixon GB, Davies SW, Aglyamova GA, Meyer E, Bay LK and Matz MV (2015) Genomic determinants of coral heat tolerance across latitudes. 
Weizhong Li's KOG BLAST server: http://weizhong-lab.ucsd.edu/metagenomic-analysis/server/kog/ 
}
\author{
Mikhail V. Matz <matz@utexas.edu>
}
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
