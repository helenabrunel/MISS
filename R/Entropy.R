Entropy <- function(x, Set=as.list(unique(x)))
{
	out <- array(dim = c(length(x), length(Set)))
	iout <- 0
	for (cSet in Set)
	{
		iout <- iout + 1
		out[,iout] <- cSet == x
	}
	pes <-  colSums(out)/sum(out)
	entropy <- sum(-pes*log2(pes))
}
