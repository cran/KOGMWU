\name{larvae.shortTerm}
\alias{larvae.shortTerm}
\docType{data}
\title{
Short-term heat stress response of coral larvae
}
\description{
Acropora millepora (larvae) response to four hours of heat stress (31.5oC)
log-fold-changes inferred using DESeq package from tag-based RNA-seq data 
from Meyer et al Mol Ecol 2011,17:3599-3616.
}
\usage{data("larvae.shortTerm")}
\format{
  A data frame with 32307 observations on the following 2 variables.
  \describe{
    \item{\code{gene}}{gene id, a factor with 32307 levels}
    \item{\code{lfc}}{log fold-change, a numeric vector}
  }
}
