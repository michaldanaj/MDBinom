# TODO: Add comment
# 
# Author: Piotr
###############################################################################



#' Przypisuje score z definicji karty scoringowej.
#' 
#' @param scoreCard definicja karty scoringowej. Patrz \code{\link{getScoreCard}}.
#' @param x \code{data.frame} z kolumnami o nazwach takich, jak w definicji karty
#' @param sufix ci�g znak�w dodany do nazw wynikowych kolumn.
#' @seealso \code{\link{getScoreCard}}.
#' @export 
assignScore<-function(scoreCard, x, sufix='_points'){
	if (!is.data.frame(x))
		stop('"x" powinno by� typu data.frame.');
	
	czy_ok<-c('variable','value','coeff', 'points')%in%names(scoreCard);
	if(all(czy_ok)==FALSE)
		stop(paste("W scoreCard brak kolumn(y)", c('variable','value','coeff', 'points')[!czy_ok]));
	
	#if (class(scoreCard)!='scoreCard')
	#	stop('"scoreCard" powinno by� klasy scoreCard (patrz getScores).')
	wynik<-data.frame();
	nazwy<-unique(scoreCard$variable);
	nazwy2<-character(); #nazwy, kt�re s� i w x i w scoreCard
	for (i in 1:length(nazwy)){
		nazwa<-nazwy[i];
		czesc<-scoreCard[scoreCard$variable==nazwa,];
		if (nazwa %in% names(x)==FALSE){
			warning(paste("W x brak kolumny",nazwa));
			next;
		}
		nazwy2<-c(nazwy2,nazwa);
		rownames(czesc)<-czesc$value;
		if (nrow(wynik)==0)
			wynik<-data.frame(czesc[x[,nazwa],'points'])
		else
			wynik[,nazwa]<-czesc[x[,nazwa],'points'];
	}	
	
	if(any(is.na(wynik))){
		warning('W x wyst�pi�y warto�ci nie zdefiniowane w karcie scoringowej! W te miejsca przypisano NA.')
	}
	names(wynik)<-paste(nazwy2,sufix, sep='');
	wynik[,'score']=rowSums(wynik);	
	wynik
}



#' Wylicza punkty score'owe na podstawie parametr�w modelu
#' 
#' @param model model, kt�rego parametry zostan� przekszta�cone na punkty score'owe.
#' @param from od jakiej warto�ci maj� si� rozpoczyna� punkty score'owe.
#' @param to do jakiej wielko�ci maj� by� warto�ci punkt�w socre'owych.
#' @param test je�li TRUE, zwraca testy istotno�ci atrybut�w.
#' @export 
getScoreCard<-function(model, from, to, test=FALSE){
	wynik<-reshape::melt(model$xlevels);
	wynik$by<-paste(wynik$L1,wynik$value,  sep='')
	wynik$coeff<- -1*coef(model)[wynik$by]
	if (test)
		wynik$test<- (summary(model)$coefficients[,4])[wynik$by]
	wynik$coeff[is.na(wynik$coeff)]<-0;
	
	rng<-tapply(wynik$coeff, wynik$L1, function(x)max(x)-min(x))
	max_rng<-max(rng);
	
	wynik$points<-round((wynik$coeff-ave(wynik$coeff,wynik$L1, FUN=min))/max_rng*(to-from)+from)
	if (test==FALSE){
		names(wynik)<-c('value','variable','x','coeff', 'points');
		wynik<-wynik[,c('variable','value','coeff', 'points')];
	}
	else{
		names(wynik)<-c('value','variable','x','coeff', 'test', 'points');
		wynik<-wynik[,c('variable','value','coeff', 'test', 'points')];		
	}
	
	return(wynik);
}

#' Wylicza punkty score'owe na podstawie parametr�w modelu
#'
#' Wylicza punkty score'owe na podstawie parametr�w modelu 
#' @param model model, kt�rego parametry zostan� przekszta�cone na punkty score'owe.
#' @param from od jakiej warto�ci maj� si� rozpoczyna� punkty score'owe.
#' @param to do jakiej wielko�ci maj� by� warto�ci punkt�w socre'owych.
#' @param test je�li TRUE, zwraca testy istotno�ci atrybut�w.
#' @export
getScoreCard2<-function(model, from, to, test=FALSE){
	wynik<-reshape::melt(model$xlevels);
	wynik$by<-paste(wynik$L1,wynik$value,  sep='')
	wynik$coeff<- -1*coef(model)[wynik$by]
	if (test)
		wynik$test<- (summary(model)$coefficients[,4])[wynik$by]
	wynik$coeff[is.na(wynik$coeff)]<-0;
	
	sum_min_est<-sum(tapply(wynik$coeff, wynik$L1, min))
	sum_max_est<-sum(tapply(wynik$coeff, wynik$L1, max))
	
	wynik$points<-round((wynik$coeff-sum_min_est)/(sum_max_est-sum_min_est)*(to-from)+from)
	if (test==FALSE){
		names(wynik)<-c('value','variable','x','coeff', 'points');
		wynik<-wynik[,c('variable','value','coeff', 'points')];
	}
	else{
		names(wynik)<-c('value','variable','x','coeff', 'test', 'points');
		wynik<-wynik[,c('variable','value','coeff', 'test', 'points')];		
	}
	
	return(wynik);
}
