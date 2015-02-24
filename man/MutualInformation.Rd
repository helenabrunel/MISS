\name{MutualInformation}

\alias{MutualInformation}

\title{ function to calculate the mutual information between two vectors  }

\description{
  This function calculates the mutual information between two vectors 'x' and 'y'  without missing values
}

\usage{
MutualInformation(x, y)
}

\arguments{
  \item{x}{ 1st vector }
  \item{y}{ 2nd vector }
}

\value{
This function returns a value that corresponds to the mutual information of vectors 'x' and 'y'
}

\author{ Alexandre Perera}

\seealso{ Entropy, JointEntropy }

\examples{

data(MISS_data, package="MISS")
iFsel <- c(17)
z <- RemoveMissings(genos=oFdata[,iFsel],phenos=ophen)
Fdata <- oFdata[z,]
binw <- dpih(ophen[z])
binvect <- seq(min(ophen[z])-0.1, max(ophen[z])+0.1+binw, by=binw)
phen <- cut (ophen[z], breaks=binvect, LABELS=FALSE)
out <- MutualInformation(Fdata[,iFsel],phen)
}

\keyword{ Mutual Information}

