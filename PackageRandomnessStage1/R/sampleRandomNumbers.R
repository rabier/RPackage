sampleRandomNumbers<-function(sampleSize){
	
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

