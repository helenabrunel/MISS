isNoSignif_MLR <- function(x, oFdata, ophen, pos, iFsel)
    {
	j<-x
	i<-pos[j]
	
	# Remove Missings
	z <- RemoveMissings(genos=oFdata[,iFsel],
                        phenos=ophen)
	Fdata <- oFdata[z,]
	binw <- dpih(ophen[z])
	binvect <- seq(min(ophen[z])-0.1, max(ophen[z])+0.1+binw, by=binw)
	phen <- cut (ophen[z], breaks=binvect, LABELS=FALSE)
	
	# 'cist' is the selection set defined by 'iFsel'
	cist <- as.data.frame(Fdata[,iFsel])
	if (l <- length(iFsel))
	{
		for(i in 1:dim(cist)[2])
		{
			cist[,i]<- as.numeric(as.factor(cist[,i]))
		}
	}
	cist$pheno <- as.numeric(phen)
	#MLR model
	m  <- lm(pheno~ . ,data=cist)
	#p-value
	if ( !is.na( m$coefficients[x+1] ) )
	{
		pval<- summary(m)$coe[names(cist)[x],'Pr(>|t|)']
	}else{
		pval <- 1
	}
	pval
}