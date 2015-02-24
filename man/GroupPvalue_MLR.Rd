\name{GroupPvalue_MLR}

\alias{GroupPvalue_MLR}

\title{ Function to compute the pvalue of a set of features  (as a set) defined by iFsel }

\description{
  This function computes the pvalue of a set of features (as a set) against the phenotype using the MLR criterion. 
}

\usage{
GroupPvalue_MLR (iFsel, oFdata, ophen, reps)
}


\arguments{
  \item{iFsel}{ indexes of the set of features  }
  \item{oFdata}{ Input Dataset, missing values admitted (as NAs) }
  \item{ophen}{Vector of phenotypes (continuous) }
  }

\value{
  Return  a pvalue associated to the set under study
}

\author{ Helena Brunel}

\seealso{ NullDistFeno, pvalue, MutualInformation, RemoveMissings }

\examples{
data(MISS_data, package="MISS")
iFsel <- c(1:4)
GroupPvalue_MLR(iFsel, oFdata, ophen)


}
