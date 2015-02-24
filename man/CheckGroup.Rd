\name{CheckGroup}

\alias{CheckGroup}

\title{ Checkgroup }

\description{
  Function to check if the set defined by iFsel is contained in the file (file)
}

\usage{
CheckGroup(iFsel, file)
}

\arguments{
  \item{iFsel}{the set to be found}
  \item{file}{the file where the search is done, for exam}
}

\details{
This function check if the group defined by iFsel is in the file (file)
}
\value{
  TRUE if the set is in the file
  FALSE if not
}

\author{Helena Brunel}

\note{ for files with the 'rs' names (i.e. result file), the last "word" of each line is the p-value and it is removed from the set.
}

\seealso{ sffs_MISS, sffs_MLR}

\examples{
iFsel <- c(1:3)
file ="test.txt"
write(iFsel, file)
CheckGroup(iFsel=iFsel, file=file)
}

\keyword{CheckGroup}

