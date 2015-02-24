\name{FindColumnSignificances_MLR}

\alias{FindColumnSignificances_MLR}

\title{Check significance for each feature plus selected set using MLR  }

\description{
  This function sweeps all columns of oFdata so that jointly with the
  provided selected set in iFsel, present a significant correlation with the independent variable using a multiple linear regression (MLR)
  Missings are considered as NAs.
 }

\usage{
FindColumnSignificances(oFdata, iFsel = c(), ophen)
}

\arguments{
  \item{oFdata}{ Input Dataset, missing values admitted (as NAs)}
  \item{iFsel}{ Vector indexes of already selected features}
  \item{ophen}{ Independent variable ( phenotype). Here, continuous variable with length the number of rows of oFdata}
  }

\value{
It returns a list with
  \item{pos }{available features indexes in the dataset (oFdata)}
  \item{pval} {pvalue associated to each feature in 'pos'}
}
\references{ }
\author{ Helena Brunel, Alexandre Perera }



\seealso{ sffs_MLR, iSignif_MLR }

\examples{
data(MISS_data, package="MISS")
out <- FindColumnSignificances_MLR(oFdata=oFdata, iFsel=iFsel, ophen=ophen)
colsign <- out$pos[out$pval<pval]

}


\keyword{SFFS}
\keyword{MLR}
