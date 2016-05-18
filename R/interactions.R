# TODO: Add comment
# 
# Author: Piotr
###############################################################################



#' Rysuje interakcje dwóch zmiennych
#' @param zm1 Pierwsza zmienna.
#' @param zm2 Druga zmienna.
#' @param target Zmienna celu. 
#' @return 
#' 
#' @author Micha³ Danaj
#' @export
interakcja<-function(zm1, zm2, target){
	target<-tapply(target, list(zm1,zm2), mean)
	logit<-log(dwa/(1-dwa))
	#jeden<-jeden[-grep('-999', rownames(jeden)),];
	#jeden<-jeden[,-grep('-999', colnames(jeden))];
	mfrow_oryg<-par('mfrow')
	par(mfrow=c(2,1))
	matplot(target, type=rep('b', ncol(target)))
	matplot(logit, type=rep('b', ncol(target)))
	par(mfrow=mfrow_oryg)
	dwa
}


