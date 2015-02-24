NullDistFeno <- function(Fset,phen,reps=3000)
{
	MI_rand<-matrix(,1,reps)

	#null distribution: MI_rand
	for (kk in c(1:reps))
	{  
		randphen<-sample(phen)
		MI_rand[kk]<-MutualInformation(Fset,randphen)
	}
	MI_rand
}