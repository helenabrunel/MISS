\name{Entropy}

\alias{Entropy}

\title{ A function to calculate the entropy of a vector}

\description{
  Calculate the entropy of a vector without missing values
}

\usage{
Entropy(x, Set = as.list(unique(x)))
}


\arguments{
  \item{x}{ the vector which entropy has tobe calculated}
  \item{Set}{the levels of  'x' as a factor }
}

\value{
  An integer which is the entropy (in bits) of the vector 'x'
}


\author{Alexandre Perera}

\note{ vector 'x' must not have missing values}

\seealso{JointEntropy, MutualInformation}

\examples{

out <- Entropy(c(1:16))

}

\keyword{Entropy}