JointEntropy <- function(x, y, xSet=as.list(unique(as.integer(x))), ySet=as.list
(unique(as.integer(y))))
{
	x<- as.integer(x)
	y<- as.integer(y)
	facs <- expand.grid(x=xSet, y=ySet)
	out <- array(dim = c(length(x), dim(facs)[1]))
	for (iout in c(1:dim(facs)[1]))
	{
        	out[,iout] <- (facs[iout,1] == x) & (facs[iout,2] == y)
	}
	pes <-  colSums(out)/sum(out) +0.000000000001
	entropy <- sum(-pes*log2(pes))
	entropy
}
