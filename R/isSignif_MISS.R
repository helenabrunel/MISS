isSignif_MISS <- function(x, oFdata, ophen, pos, iFsel,reps=3000 )
{
	j<-x
	i<-pos[j]
	
	# Remove missings
	z <- RemoveMissings(genos=oFdata[,c(iFsel,i)],
                        phenos=ophen)
	Fdata <- oFdata[z,]
	binw <- dpih(ophen[z])
	binvect <- seq(min(ophen[z])-0.1, max(ophen[z])+0.1+binw, by=binw)
	phen <- cut (ophen[z], breaks=binvect, LABELS=FALSE)

	minulldist <- NullMIDistSurrogate(Fdata, iFsel, i, phen, reps)
        
	# MI of the SNP under study
	Fset<-as.factor(concat(Fdata[,c(iFsel,i)]))
	MI <- MutualInformation(Fset,phen)
	pval <- pvalue(MI, minulldist) 
	pval	
#list(MI=MI,pval=pval)
}
