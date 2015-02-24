\name{isSignif_MISS}

\alias{isSignif_MISS}

\title{ Function to evaluate if a feature is significantly informative about the phenotype respect to a set of preselected features.}

\description{
  This function looks at the significance of the feature with index 'x', when it is added to a subset of features previously selected with indexes 'iFsel', using the mutual information statistical significance (MISS) criterion. For doing this,  a null distribution of mutual information is computed using surrogate data technique, and a pvalue is associated to the feature under study respect to this null distribution.
}

\usage{
isSignif_MISS(x, oFdata, ophen, pos, iFsel, reps)
}

\arguments{
  \item{x}{ the index (in 'pos') of the feature under study }
  \item{oFdata}{ Input Dataset, missing values admitted (as NAs) }
  \item{ophen}{Vector of phenotypes (continuous) }
  \item{pos}{ a vector with the indexes of available features}
  \item{iFsel}{ Indexes of the previously selected subset of features}
  \item{reps}{ number of surrogate copies for generating the null distribution (by default set to 3000}
}

\value{
  Return  a p-value corresponding to the significance of the feature under study
}

\author{ Alexandre Perera and Helena Brunel}

\note{
In this function, missing values are removed from the total set (iFsel + {x})
}

\seealso{ FindColumnSignificances_MISS, RemoveMissings, MutualInformation, concat, NullMIDistSurrogate, pvalue }

\examples{
data(MISS_data, package="MISS")
pos <- c(1:dim(oFdata)[2]))
x <- 17
isSignif_MISS(x,oFdata, ophen, pos, iFsel, reps)

}
