\name{pvalue}

\alias{pvalue}


\title{ Function to calculate the  pvalue corresponding to a value (a) respect to a null distribution(dist)}

\description{
 This function computes the pvalue associated to a value 'a', comparing it to a null distribution 'dist' using a kernel density estimation of 'dist' and evaluating the area under this estimation from the value 'a'. 
}


\usage{
pvalue(a, dist)
}

\arguments{
  \item{a}{ The value to be compared with the null distribution}
  \item{dist}{ the null distribution}
}

\value{
 returns the pvalue, an integer between 0 and 1.
}

\references{ Kernsmooth and Hmisc packages }

\author{Helena Brunel}
\note { 
If there is no variability in dist, pvalue is set to 1 or 0 depending on 'a'
Kernsmooth and Hmisc packages needed 
}
\seealso{GroupPvalue, isSignif}

\examples{
dist <- rnorm(100)
pvalue(a=2, dist)
}
\keyword{pvalue}
\keyword{kernel density estimation}
