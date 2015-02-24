\name{sffs_MLR}

\alias{sffs_MLR}

\title{ A function for applying a version of Sequential Forward Feature Selection to a genotype-phenotype association problem}

\description{
  This function applies an SFFS algorithm that find several relevant solutions that are sets of SNPS significantly related to the phenotype. It is an iterative algorithm of SFS. When a set of SNPs is found, it is written in a file joint to its set pvalue. The relevance criterion is based on the multiple linear regression model (MLR)
}

\usage{
sffs_MLR(oFdata, iFsel, ophen, pvalor, file1, file2, file3, inistate="FORWARD")

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{oFdata}{ Input Dataset, missing values admitted (as NAs)}
  \item{iFsel}{ initial set of features (should be c())}
  \item{ophen}{ independent variable to be predicted (here the phenotypes) (continuous) }
  \item{pvalor}{ significance level (by default set to 0.05)}
  \item{file1}{the file with forward tested indexes}
  \item{file2}{the file with backward tested indexes}
  \item{file3}{the file where selected SNPs sets will be written}
  }

\examples{
data(MISS_data, package="MISS")
sffs_MLR (oFdata=oFdata, iFsel=iFsel, ophen=ophen, pvalor=pval, file1=file1, file2=file2, file3=file3, inistate="FORWARD")
}

\value{
It saves the SNPs sets selected and its sets p-values in a txt file 
  
}

\references{ }
\author{ Helena Brunel}

\seealso{sffs_MISS, FindColumnSignificances_MLR,  FindColumnNoSignificant_MLR, GroupPvalue_MLR, CheckGroup}


