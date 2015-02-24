\name{NullDistFeno}

\alias{NullDistFeno}

\title{ Function to compute the null distribution of mutual information by surrogating the phenotype}

\description{
  This function computes a null distribution of mutual information betwen a set of features (SNPs) and the observed variable (phenotype) by surrogating the phenotype
}

\usage{
NullDistFeno(Fset,phen,reps)
}


\arguments{
  \item{Fset}{ The features concatenates as a onedimensional vector (without missing values)  }
  \item{phen}{ The observed variable (phenotype) (discretized and without missing values) }
  \item{reps}{ the number of surrogate copies of the phenotype to be generated (by default set to 3000}
}


\value{
  The returned vector is  the null distribution
  
}

\author{ Helena Brunel}

\note{
use NullDistFeno por surrogating the phenotype
}

\seealso{ GroupPvalue_MISS, MutualInformation }
\examples{

data(MISS_data, package="MISS")
iFsel <- c(3)
Fdata <- as.matrix(oFdata[,iFsel])
z <- RemoveMissings(genos=Fdata, phenos=ophen)
Fdata <- Fdata[z,]
Fset<-as.factor(concat(Fdata))
binw <- dpih(ophen[z])
binvect <- seq(min(ophen[z])-0.1, max(ophen[z])+0.1+binw, by=binw)
phen <- cut(ophen[z] ,breaks = binvect ,LABELS=FALSE)  
minulldist <- NullDistFeno(Fset, phen=phen, reps)

}
