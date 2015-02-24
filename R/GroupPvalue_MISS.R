GroupPvalue_MISS <- function(iFsel, oFdata, ophen, reps=3000)
{
	MI_rand<-matrix(,1,reps)

	# extract SNPs of selection set (indexes in 'iFsel') from oFdata and remove missing values
	Fdata <- as.matrix(oFdata[,iFsel])
	z <- RemoveMissings(genos=Fdata,
                        phenos=ophen)
	Fdata <- Fdata[z,]
	Fset<-as.factor(concat(Fdata))

	#discretize the phenotype using kernel binwidth selection
	binw <- dpih(ophen[z])
	binvect <- seq(min(ophen[z])-0.1, max(ophen[z])+0.1+binw, by=binw)
	phen <- cut(ophen[z] ,breaks = binvect ,LABELS=FALSE)    

	#Mutual information of the set of SNPs and the phenoytpe
	MI <- MutualInformation(Fset, phen)

	#null distribution
	minulldist <- NullDistFeno(Fset, phen=phen, reps)
	#p-value
	pval <- pvalue(MI, dist=minulldist[1,])
	pval
}


