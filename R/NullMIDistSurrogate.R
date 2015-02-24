
NullMIDistSurrogate <- function(Fdata, iFsel, i, phen,  reps=3000)
{
	# Compute the mutual information null distribution supposing: 
	#   Fdata, phen without missings
	#   phen discretized
	
	snp<- Fdata[,i]

	MI_rand<-matrix(,reps,1)
	for (kk in 1:reps)
	{
		posrand<-sample(nrow(Fdata))
		Fsetrand<-snp[posrand]
		b<-as.factor(concat(cbind(Fdata[,iFsel],Fsetrand)))
		MI_rand[kk]<-MutualInformation(b,phen)
	}
	MI_rand
}
