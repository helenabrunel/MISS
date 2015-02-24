\name{GroupPvalue_MISS}

\alias{GroupPvalue_MISS}

\title{ Function to compute the pvalue of a set of features  (as a set) defined by iFsel }

\description{
  This function computes the pvalue of a set of features (as a set) against the phenotype using the MISS criterion. The null distribution is obtained by surrogating the phenotype
}

\usage{
GroupPvalue_MISS (iFsel, oFdata, ophen, reps)
}


\arguments{
  \item{iFsel}{ indexes of the set of features  }
  \item{oFdata}{ Input Dataset, missing values admitted (as NAs) }
  \item{ophen}{Vector of phenotypes (continuous) }
  \item{reps}{ number of surrogate copies of the phenotype for generating the null distribution}
}

\value{
  Return  a pvalue associated to the set under study
}

\author{ Helena Brunel}

\seealso{ NullDistFeno, pvalue, MutualInformation, RemoveMissings }

\examples{
data(MISS_data, package="MISS")
iFsel <- c(1:4)
GroupPvalue_MISS(iFsel, oFdata, ophen, reps)
}
