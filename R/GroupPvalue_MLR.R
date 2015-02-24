GroupPvalue_MLR <- function(iFsel, oFdata, ophen)
{
	
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
	
	# 'cist' is the set of SNPs defined by 'iFsel'
	cist <- as.data.frame(Fdata)
	if (l <- length(iFsel))
		{
		for(i in 1:dim(Fdata)[2])
			{
			cist[,i]<- as.numeric(as.factor(cist[,i]))
			}
		}
	cist$pheno <- as.numeric(phen)

	#MLR model
	m <- lm(pheno~., data=cist)
	# p-value
	pval<- pf(summary(m)$fstatistic[1], summary(m)$fstatistic[2],summary(m)$fstatistic[3], lower.tail = FALSE) 
	pval
}


