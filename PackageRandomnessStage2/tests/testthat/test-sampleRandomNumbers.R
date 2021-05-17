context("sampleRandomNumbers")



test_that("la taille echantillonnale est suffisante",  {
		
	expect_warning(sampleRandomNumbers(29))
		
	expect_error(sampleRandomNumbers(-1))
		
})



test_that("l intervalle de confiance pour la vraie valeur de la moyenne semble coherent",  {
	## on s interesse ici a la variable aleatoire X, et en particulier a sa moyenne qui est egale
	## par definition a 0
		
	## on considere par exemple une taille echantillonnale de 1000	
	mySampleSize<-1000
	U<-sampleRandomNumbers(mySampleSize)
    EmpiricalMean<-U[[1]]
    	
    ## on calcule les bornes d un intervalle de confiance a 95 pourcent	
	lowerBound<-EmpiricalMean-1.96/sqrt(mySampleSize)
	upperBound<-EmpiricalMean+1.96/sqrt(mySampleSize)
		
## la vraie valeur de la moyenne (i.e. 0) se doit d etre contenue (dans 95 pourcent des cas) dans l intervale de confiance ##si le generateur aleatoire de R marche correctement		
##donc on s attend a ce que cela soit verifie sur notre jeux de donnees simulees
	expect_gt(0,lowerBound)
	expect_lt(0,upperBound)
	### en dautres termes 0 doit appartenir a lintervalle [lowerBound,upperBound]
	

})



test_that("les operations sur la variance semblent coherentes",  {
	##pour rappel, les 2 variables aleatoires X et Y different uniquement d une translation, car Y=X+m
	##par definition Var(X)=Var(X+m)=Var(Y) avec m nombre reel
	
	mySampleSize<-1000
	U<-sampleRandomNumbers(mySampleSize)
		
	##la variance est bien la meme pour les 2 echantillons simules,
	## ie Var(X)=Var(Y) 
	VarX<-U[[2]]
	VarY<-U[[3]]
	expect_equal(VarX,VarY)
		
	## pour rappelZ=2X+6, on s attend donc a ce que var(Z) soit egale a 4*Var(X)
	VarZ<-U[[4]]
	expect_equal(VarZ,4*VarX)
		
	})



test_that("j arrive bien a regenerer les memes nombres aleatoires en fixant la graine",  {
		
	##attention a bien fixer la graine a la meme valeure que dans la fonction sampleRandomNumbers	
	set.seed(9)
		
	mySampleSize<-1000
	X<-rnorm(mySampleSize)
	U<-sampleRandomNumbers(mySampleSize)
	EmpiricalMean<-U[[1]]
	EmpiricalVariance<-U[[2]]
	 
	 
	### en toute logique la moyenne empirique et la variance doivent etre les memes que celles
	### obtenues avec la fonction sampleRandomNumbers
	expect_equal(mean(X),EmpiricalMean)
	expect_equal(var(X),EmpiricalVariance)

	
	})









