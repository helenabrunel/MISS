\name{JointEntropy}

\alias{JointEntropy}

\title{ A function to calculate the Joint Entropy between two vectors x and y}

\description{
  This function calculates the Joint Entropy between two vectors 'x' and 'y' without have missing values
}

\usage{JointEntropy(x,y)}


\value{
  An integer which is the joint entropy (in bits) of vectors 'x' and 'y'
}

\author{Alexandre Perera}

\note{ vectors 'x' and 'y' must not have missing values}


\seealso{ Entropy, MutualInformation}

\examples{
data(MISS_data, package="MISS")
x <-oFdata[,3]
y <- oFdata[,4]
out <- JointEntropy(x,y)
}

\keyword{Joint Entropy}
