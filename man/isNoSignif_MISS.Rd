\name{isNoSignif_MISS}

\alias{isNoSignif_MISS}

\title{ Function to evaluate if a feature is significantly non informative about the phenotype respect to a set of features.}

\description{
  This function looks at the significance of the feature with index 'x', when it is removed from to a subset of features with indexes 'iFsel', using the mutual information statistical significance criterion. For doing this, a null distribution of mutual information is computed using surrogate data technique, and a p-value is associated to the feature under study respect to this null distribution.
}

\usage{
isNoSignif_MISS(x, oFdata, ophen, pos, iFsel, reps)
}

\arguments{
  \item{x}{ the index (in 'pos') of the feature under study }
  \item{oFdata}{ Input Dataset, missing values admitted (as NAs) }
  \item{ophen}{Vector of phenotypes (continuous) }
  \item{pos}{ indexes of candidate features to be removed from the set}
  \item{iFsel}{ Indexes of the features in the set}
  \item{reps}{ number of surrogate copies for generating the null distribution (by default set to 3000}
}

\value{
  Return  a p-value corresponding to the significance of the feature under study
}

\author{Helena Brunel}

\note{
In this function, missing values are removed from the total set (iFsel + {x})
}

\seealso{ FindColumnNoSignificant_MISS, RemoveMissings, MutualInformation, concat, NullMIDistSurrogate, pvalue }

\examples{
data(MISS_data, package="MISS")
iFsel <-  c(1:4)
pos <- c(1:dim(oFdata)[2]))
x <- 1
isNoSignif_MISS(x,oFdata, ophen, pos, iFsel, reps)

}
