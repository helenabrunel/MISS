isSignif_MLR <- function(x, oFdata, ophen, pos, iFsel)
{
	j<-x
	i<-pos[j]
	# Remove missingd
	z <- RemoveMissings(genos=oFdata[,c(iFsel,i)],
                        phenos=ophen)
	Fdata <- oFdata[z,]
	binw <- dpih(ophen[z])
	binvect <- seq(min(ophen[z])-0.1, max(ophen[z])+0.1+binw, by=binw)
	phen <- cut (ophen[z], breaks=binvect, LABELS=FALSE)

	#build a set 'cist' with the previous set + the candidate SNP
	cist <- as.data.frame(Fdata[,iFsel])
	if (l <- length(iFsel))
	{
		for(i in 1:dim(cist)[2])
		{
			cist[,i]<- as.numeric(as.factor(cist[,i]))
		}
	}
	newSNP <- as.numeric(as.factor(Fdata[,pos[x]]))
	cist <- cbind(cist, newSNP)
	cist$pheno <- as.numeric(phen)
	
	#MLR model
        m  <- lm(pheno~ . ,data=cist)
	
	#p-value
	if ( !is.na( m$coefficients['newSNP'] ) )
	{	
		pval<- summary(m)$coe['newSNP','Pr(>|t|)']
	}else{
		pval <- 1
	}
	pval
}
