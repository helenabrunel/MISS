pvalue <- function (a,dist)
{
# If there is no variability in 'dist', p-value is set to 0 or 1 depending on 'a'. 
	if(sd(dist)<0.1)
	{
		if(a>mean(dist))
		{
			pvalue <- 0
		}else{
			pvalue <- 1	
		}
	}else{
		h <- dpik(as.vector(dist))
		#est is the density estimation of dist
		#est <- bkde(dist)
		est <- bkde(dist, bandwidth=h,range.x=extendrange(r=range(a,dist), f = 0.05), gridsize=1001)
		#copmute the area under the curve est from 'a' value
		pvalue <- abs(trap.rule(est$x[est$x>a], est$y[est$x>a])/trap.rule(est$x, est$y))
	}
	#pvalue <- length(dist[dist>a])/length(dist)
	pvalue
}
