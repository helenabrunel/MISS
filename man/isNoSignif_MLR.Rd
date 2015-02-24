\name{isNoSignif_MLR}

\alias{isNoSignif_MLR}

\title{ Function to evaluate if a feature is significantly non informative about the phenotype respect to a set of features.}

\description{
  This function looks at the significance of the feature with index 'x', when it is removed from to a subset of features with indexes 'iFsel', using the multiple linear regression model (MLR).
}

\usage{
isNoSignif_MLR(x, oFdata, ophen, pos, iFsel, )
}

\arguments{
  \item{x}{ the index (in 'pos') of the feature under study }
  \item{oFdata}{ Input Dataset, missing values admitted (as NAs) }
  \item{ophen}{Vector of phenotypes (continuous) }
  \item{pos}{ indexes of candidate features to be removed from the set}
  \item{iFsel}{ Indexes of the features in the set}
  }

\value{
  Return  a p-value corresponding to the significance of the feature under study
}

\author{Helena Brunel}

\note{
In this function, missing values are removed from the total set (iFsel + {x})
}

\seealso{ FindColumnNoSignificant_MLR, RemoveMissings}

\examples{
data(MISS_data, package="MISS")
iFsel <-  c(1:4)
pos <- c(1:dim(oFdata)[2]))
x <- 1
isNoSignif_MLR(x,oFdata, ophen, pos, iFsel)
}
