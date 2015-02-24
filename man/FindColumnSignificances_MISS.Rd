\name{FindColumnSignificances_MISS}

\alias{FindColumnSignificances_MISS}

\title{Check significance for each feature plus selected set using MISS  }

\description{
  This function sweeps all columns of oFdata so that jointly with the
  provided selected set in iFsel, present a significant correlation with the independent value using Mutual Information Statistical Significance (MISS)
  p-value versus a null distribution of Mutual Information criterion
  Missings are considered as NAs.
  It is a parallelized version (it needs snow and rpmi packages)
}

\usage{
FindColumnSignificances(oFdata, iFsel, ophen, reps, cl)
}

\arguments{
  \item{oFdata}{ Input Dataset, missing values admitted (as NAs)}
  \item{iFsel}{ Vector indexes of already selected features}
  \item{ophen}{ Independent variable ( phenotype). Here, continuous variable with length the number of rows of oFdata}
  \item{reps}{ number of surorgate copies for the null distribution generation (by default set to 3000)}
  \item{cl}{ parallelization indicator (by default set to NULL: does not parallelize)  }
}

\details{
  As it is a parallelized version it applies isSignif_MISS function for each of the available feature
}

\value{
It returns a list with
  \item{pos }{available features indexes in the dataset (oFdata)}
  \item{pval} {pvalue associated to each feature in 'pos'}
}
\references{ }
\author{ Helena Brunel, Alexandre Perera }

\note{ could be Parallelized 
}

\seealso{ sffs_MISS, iSignif_MISS }

\examples{
data(MISS_data, package="MISS")
out <- FindColumnSignificances_MISS(oFdata=oFdata, iFsel=iFsel, ophen=ophen, reps=reps, cl=cl)
colsign <- out$pos[out$pval<pval]

}


\keyword{Surrogate}
\keyword{SFFS}
\keyword{MISS}
