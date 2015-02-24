isNoSignif_MISS <- function(x, oFdata, ophen, pos, iFsel,reps=3000 )
    {	
	j<-x
	i<-pos[j]
	# Remove Missings
	z <- RemoveMissings(genos=oFdata[,iFsel],
                        phenos=ophen)
	Fdata <- oFdata[z,]
	binw <- dpih(ophen[z])
	binvect <- seq(min(ophen[z])-0.1, max(ophen[z])+0.1+binw, by=binw)
	phen <- cut(ophen[z], breaks=binvect, LABELS=FALSE)
	#new set: total set - "SNP 'i'"
	niFsel <- setdiff(iFsel, i)

	minulldist <- NullMIDistSurrogate(Fdata, niFsel, i, phen, reps)

	# MI of the total set
	Fset<-as.factor(concat(Fdata[,iFsel]))
	MI <- MutualInformation(Fset,phen)
	pval <- pvalue(MI, minulldist) 
	pval
	#list(MI=MI,pval=pval)
    }
