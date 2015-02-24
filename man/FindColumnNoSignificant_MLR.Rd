\name{FindColumnNoSignificant_MLR}

\alias{FindColumnNoSignificant_MLR}

\title{Check significance for  all features in the set using MLR }

\description{
  This function sweeps all features in the set given by indexes in 'iFsel' that can be removed from the set without loosing significant information about the observed variable ('ophen'). These features do not add significant correlation with the independent value using multiple Linear Regression (MLR). 
  Missings are considered as NAs.
  }

\usage{
FindColumnNoSignificant_MLR(oFdata, iFsel , ophen)
}

\arguments{
  \item{oFdata}{ Input Dataset, missing values admitted (as NAs)}
  \item{iFsel}{ Vector indexes of already selected features}
  \item{ophen}{ Independent variable ( phenotype). Here, continuous variable with length the number of rows of oFdata}
  }


\value{
It returns a list with
  \item{pos }{Positions in the dataset (oFdata) of the features in the selection set}
  \item{pval} {pvalue associated to each feature in 'pos'}
}
\references{ }
\author{ Helena Brunel }


\seealso{ sffs_MLR, isNoSignif_MLR }

\examples{ 
data(MISS_data, package="MISS")
iFsel <- c(1:4)
out <- FindColumnNoSignificant_MLR(oFdata=oFdata, iFsel=iFsel, ophen=ophen)
colsign <- out$pos[out$pval>pval]
}

\keyword{SFFS}
\keyword{MLR}
