# TODO: Add comment
# 
# Author: Piotr
###############################################################################



#' Odchylenie modelu
#' 
#' @param predicted prawdopodobie�stwo.
#' @param target target.
#' @return 
#' 
#' @author Piotr
#' @export
model_dev<-function(predicted, target){
	-2*sum(target*log(predicted)+(1-target)*log(1-predicted))
}


#' Zast�puje braki danych
#' 
#' @param x x
#' @param val jak� warto�ci� zast�pi� brak danych. 
#' @return 
#' 
#' @author Piotr
#' @export
na.subst<-function(x, val){
	x[is.na(x)]<-val;
	x
}


#' Wrzuca dane do schowka, do wklejenia do Excela
#' @param table \code{data.frame} do przekazania do Excela. 
#' @param row.names  czy przekaza� r�wnie� wiersze.
#' @return 
#' 
#' @author Piotr
#' @export
to_excel<-function(table, row.names=FALSE){
	write.table(table, file='clipboard', sep='\t', row.names=row.names, dec=',')
}


#do skasowania!!! 
#' Liczy WoE
#' 
#' @param br warto�� prawdopodobie�stwa.
#' @return 
#' 
#' @author Piotr
#' @export
woe<-function(br){
	br[br==0]<-0.5/length(br)
	br[br==1]<-(length(br)-0.5)/length(br)
	log(br/(1-br))
}


#' Zwraca ostatni dzie� podanego okresu.
#'
#' @param dat Obiekt klasy \link{Date}, lub daj�cy si� skonwertowa� funkcj� \link{as.Date}.
#' @param unit Jednostka czasu, kt�ra ma by� uwzgl�dniona przy wyznaczaniu ostatniego dnia.
#'  Mo�liwe warto�ci: \code{c("month","quater", "year")}, kt�re odpowiadaj� ostatniemu dniu miesi�ca, kwarta�u, roku,
#'  do kt�rego nale�y data \code{dat}.
#' @param ... Opcje do funkcji \link{as.Date}.
#' @author Micha� Danaj
#' @export
lastDay<-function(dat, unit=c("month","quarter", "half_year", "year"),...) {
	
	# Wst�pne przygotowania
	unit<-match.arg(unit);
	
	dat<-as.Date(dat,...);
	
	# Pobieram rok i miesi�c
	rok<-as.numeric(format(dat, '%Y'));
	mies<-as.numeric(format(dat, '%m'));
	
	# Je�li ostatni dzie� kwarta�u, to wybieram ostatni miesi�c kwarta�u
	if (unit=="quarter")
		mies<-((mies-1)%/%3+1)*3
	
	if (unit=="half_year")
		mies<-((mies-1)%/%6+1)*6
	
	if (unit=="year")
		mies<-12;
	
	# W�a�ciwy algorytm
	ostatni<-mies==12;
	mies[!ostatni]<-mies[!ostatni]+1;
	mies[ostatni]<-1;
	rok[ostatni]<-  rok[ostatni]+1;
	
	# zamiana na dat�
	new_date<-as.Date(ISOdate(rok, mies, 1));
	return(new_date-1);
}