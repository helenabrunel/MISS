\name{RemoveMissings}

\alias{RemoveMissings}

\title{Removes missings (as NA's) from X and an optional vector Y }

\description{
This function removes rows with missing values from an array of features X.
Missings are coded with NA's. If a vector Y is given, only common rows
free of NAs will be returned as an index vector
}

\usage{
RemoveMissings(genos, phenos = NULL)
}

\arguments{
  \item{genos}{ Matrix to remove missings from, missings coded with NA }
  \item{phenos}{Optional vector to consider, also missings coded with NA }
}

\value{
 This function returns an index of the common rows that are free of
 missing values in X and Y(optional). 
}

\author{Helena Brunel, Alexandre Perera }

\note{Missing values must be coded with NA's}
\seealso{isSignif, GroupPvalue }

\examples{
data(MISS_data, package="MISS")
iFsel <- c(6:10)
z <- RemoveMissings(genos=oFdata[,iFsel], phenos=ophen)
}

\keyword{ missing values }
