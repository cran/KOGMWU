\name{larvae.longTerm}
\alias{larvae.longTerm}
\docType{data}
\title{
Long-term heat stress response of coral larvae
}
\description{
Acropora millepora (larvae) response to five days of heat stress (31.5oC)
log-fold-changes inferred using DESeq package from tag-based RNA-seq data 
from Meyer et al Mol Ecol 2011,17:3599-3616
}
\usage{data("larvae.longTerm")}
\format{
  A data frame with 31844 observations on the following 2 variables.
  \describe{
    \item{\code{gene}}{gene id, a factor with 31844 levels }
    \item{\code{lfc}}{log fold-change, a numeric vector}
  }
}
