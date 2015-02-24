MutualInformation<- function(x, y, xSet=as.list(unique(x)), ySet=as.list(unique(
y)))
{
	Entropy(x, Set=xSet) +  Entropy(y, Set=ySet) - JointEntropy(x, y, xSet=xSet, ySet=ySet)

}

