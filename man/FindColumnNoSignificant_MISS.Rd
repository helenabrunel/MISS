\name{FindColumnNoSignificant_MISS}

\alias{FindColumnNoSignificant_MISS}

\title{Check significance for  all features in the set using MISS }

\description{
  This function sweeps all features in the set given by indexes in 'iFsel' that can be removed from the set without loosing significant information about the observed variable ('ophen'). These features do not add significant correlation with the independent value using Mutual Information Statistical Significance (MISS). The corresponding p-value is computed versus a null distribution of Mutual Information. 
  Missings are considered as NAs.
  It is a parallelized version (it needs snow and rpmi packages)
}

\usage{
FindColumnNoSignificant_MISS(oFdata, iFsel , ophen, reps,  cl)
}

\arguments{
  \item{oFdata}{ Input Dataset, missing values admitted (as NAs)}
  \item{iFsel}{ Vector indexes of already selected features}
  \item{ophen}{ Independent variable ( phenotype). Here, continuous variable with length the number of rows of oFdata}
  \item{reps}{ number of surorgate copies for the null distribution generation (by default set to 3000}
  \item{cl}{ parallelization indicator (by default set to NULL: does not parallelize)  }
}

\details{
  As it is a parallelized version it applies isNoSignif_MISS function for each of the available features 
}

\value{
It returns a list with
  \item{pos }{Positions in the dataset (oFdata) of the features in the selection set}
  \item{pval} {pvalue associated to each feature in 'pos'}
}
\references{ }
\author{ Helena Brunel }

\note{ could be Parallelized }

\seealso{ sffs_MISS, isNoSignif_MISS }

\examples{
data("MISS_data")
iFsel <- c(1:4)
out <- FindColumnNoSignificant_MISS(oFdata=oFdata, iFsel=iFsel, ophen=ophen, reps=reps, cl=cl)
colsign <- out$pos[out$pval>pval]
}


\keyword{Surrogate}
\keyword{SFFS}
\keyword{MISS}
