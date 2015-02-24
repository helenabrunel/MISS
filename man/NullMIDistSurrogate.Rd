\name{NullMIDistSurrogate}

\alias{NullMIDistSurrogate}

\title{ This generates a NULL Mutual Information distribution between a set of features and the observed variable, by surrogating the candidate feature included in the set}

\description{
  This function computes the null distribution of the
  Mutual Information between a set of features defined by ('iFsel' + 'i') and the observed variable ('phen').  The feature to be included in the selection set ('i') is surrogated by generating the number of surrogate copies given by 'reps'. 
 }

\usage{
NullMIDistSurrogate(Fdata, iFsel, i, phen, reps)
}

\arguments{
  \item{Fdata}{Input Dataset, without missings }
  \item{iFsel}{Vector of indexes of already selected features}
  \item{i}{Index of the feature to be included in the selection set and which will be surrogated}
  \item{phen}{The observed variable (here, the phenotype) (discretized  and without missing)}
  \item{reps}{Number of surrogate copies to be generated (by default set to 3000}
}

\value{ Returns a vector of length 'reps' of Mutual Information values corresponding to each surrogate copy.}

\author{Helena Brunel, Alexandre Perera }

\seealso{MutualInformation, concat, isSignif_MISS}

\examples{

data(MISS_data, package="MISS")
iFsel <- c(3,17)
i <- 2
z <- RemoveMissings(genos=oFdata[,iFsel], phenos=ophen)
Fdata <- oFdata[z,]
binw <- dpih(ophen[z])
binvect <- seq(min(ophen[z])-0.1, max(ophen[z])+0.1+binw, by=binw)
phen <- cut (ophen[z], breaks=binvect, LABELS=FALSE)
minulldist <- NullMIDistSurrogate(Fdata, iFsel, i, phen, reps)

}

\keyword{Null Distribution}
\keyword{Mutual Information}
