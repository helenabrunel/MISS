\name{isSignif_MLR}

\alias{isSignif_MLR}

\title{ Function to evaluate if a feature is significantly informative about the phenotype respect to a set of preselected features}

\description{
  This function looks at the significance of the feature with index 'x', when it is added to a subset of features previously selected with indexes 'iFsel', using a multiple linear regression model (MLR).
}

\usage{
isSignif_MLR(x, oFdata, ophen, pos, iFsel)
}

\arguments{
  \item{x}{ the index (in 'pos') of the feature under study }
  \item{oFdata}{ Input Dataset, missing values admitted (as NAs) }
  \item{ophen}{Vector of phenotypes (continuous) }
  \item{pos}{ a vector with the indexes of available features}
  \item{iFsel}{ Indexes of the previously selected subset of features}
  }

\value{
  Return  a p-value corresponding to the significance of the feature under study
}

\author{ Alexandre Perera and Helena Brunel}

\note{
In this function, missing values are removed from the total set (iFsel + {x})
}

\seealso{ FindColumnSignificances_MLR, RemoveMissings }

\examples{
data(MISS_data, package="MISS")
pos <- c(1:dim(oFdata)[2]))
x <- 17
isSignif_MLR(x,oFdata, ophen, pos, iFsel, reps)

}
