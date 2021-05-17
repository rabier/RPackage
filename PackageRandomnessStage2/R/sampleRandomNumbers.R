#' Illustration of the random number generator in R. We focus here on the normal distribution
#'
#' This function first generates a sample of size sampleSize, drawn from a Normal distribution 
#' with mean 0 and variance 1
#' This sample is called Sample 1. Next, using observations from Sample 1, it generates Sample 2, 
#' containing observations that follows a normal distribution with mean 6 and variance 1.
#' Last, still using observations from Sample 1, it generates Sample 3, 
#' containing observations that follows a normal distribution with mean 6 and variance 4.
#'
#'
#' @param sampleSize size of the sample
#'
#'
#'@return a list containing: the empirical mean of sample 1, the empirical variance of sample 1, the empirical variance of sample 2, the empirical variance of sample 3
#'
#'
#' @examples
#' U<-sampleRandomNumbers(1000)
#' 
#' U[[1]] # the Empirical Mean of Sample 1
#' U[[2]] # the Empirical Variance of Sample 1
#' U[[3]] # the Empirical Variance of Sample 2
#' U[[4]] # the Empirical Variance of Sample 3

#' @export


sampleRandomNumbers<-function(sampleSize=100){
	
	##parametre d entree: 
	## sampleSize est la taille echantillonnale
	
	##cette fonction genere
	## tout d abord un echantillon X(1), ..., X(sampleSize) de taille sampleSize 
	## issu d une loi Gaussienne (ie loi normale) de moyenne 0 et de variance 1. 
	## Pour simplifier, on appelle cette echantillon X
	## A partir de l'echantillon  X, elle cree un echantillon Y 
	## issu d une loi Gaussienne de moyenne egale a 6 et de variance 1
	## Enfin, a partir de l'echantillon  X, elle cree un echantillon Z 
	## issu d une loi Gaussienne de moyenne 6 et de variance 4 
		
		
	##en sortie, cette fonction renvoie une liste contenant :
	## la moyenne empirique de X
	## la variance empirique de X
	## la variance empirique de Y
	## la variance empirique de Z
	
	
	
	##je fixe la graine avec la fonction set.seed()
	set.seed(9)
	
	
	if (sampleSize<1){

		error('La taille de l echantillon est plus petite ou egale a 0. Impossible')

	} else if (sampleSize<30) {
		
		warning('Augmentez svp la taille de l echantillon')
		
	}
	
	
	X<-rnorm(sampleSize)
	
	myMeanX<-mean(X)

	myVarX<-var(X)

	Y<-X+6
	
	myVarY<-var(Y)
	
	Z<-2*X+6
	
	myVarZ<-var(Z)

	return(list(myMeanX,myVarX,myVarY,myVarZ))
	
		
	
}

