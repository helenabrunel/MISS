\name{concat}

\alias{concat}

\title{ Function to concatenate columns of a matrix }

\description{
  This function concatenates the columns of matrix 'a' and saves the result as a one-dimensional vector
}

\usage{
concat(a)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{a}{ matrix to concatenate (almost 2 columns)}
}

\details{
   It's supposed that 'a' matrix doesn't have missing values
}

\value{
 This function returns a vector which is the concatenation of the matrix columns
}

\author{ Helena Brunel}

\examples{
data(MISS_data, package="MISS")
iFsel <- c(1,3)
a <- oFdata[,iFsel]
concat(a)
}
\keyword{concatenation}

