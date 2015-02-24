\name{sffs_MISS}

\alias{sffs_MISS}

\title{ A function for applying a version of Sequential Forward Feature Selection to a genotype-phenotype association problem}

\description{
  This function applies an SFFS algorithm that finds several relevant solutions that are sets of SNPS significantly related to the phenotype. It is an iterative and paralelized algorithm of SFS. When a set of SNPs is found, it is written in a file joint to its set pvalue. The relevance criterion is based on the mutual information statistical significance (MISS)
}

\usage{
sffs_MISS(oFdata=oFdata, iFsel=c(), ophen=ophen, reps=reps, pvalor=0.05,  file=file, cl=cl)
}



%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{oFdata}{ Input Dataset, missing values admitted (as NAs)}
  \item{iFsel}{ initial set of features (should be c())}
  \item{ophen}{ independent variable to be predicted (here the phenotypes) (continuous) }
  \item{reps}{number of surrogate copies when generating the mutual information null distribution (by default set to 3000}
  \item{pvalor}{ significance level (by default set to 0.05)}
  \item{file1}{the file with forward tested indexes}
  \item{file2}{the file with backward tested indexes}
  \item{file3}{the file where selected SNPs sets will be written}
  \item{cl}{ parallelization indicator (by default set to NULL: does not parallelize)  
  }
}


\examples{
data(MISS_data, package="MISS")
sffs_MISS (oFdata=oFdata, iFsel=iFsel, ophen=ophen, reps=reps, pvalor=pval, file1=file1, file2=file2, file3=file3, inistate="FORWARD", cl=cl)
}


\value{
It saves the SNPs sets selected and p-values in a txt file ('file3') 
}

\author{ Helena Brunel}

\note{ Parallelized iterative version}

\seealso{ sffs_MLR, FindColumnSignificances_MISS, FindColumnNoSignificant_MISS , GroupPvalue_MISS, CheckGroup }


