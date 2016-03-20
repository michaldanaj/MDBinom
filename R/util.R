# TODO: Add comment
# 
# Author: Micha³ Danaj
###############################################################################



#' Odchylenie modelu
#' 
#' @param predicted prawdopodobieñstwo.
#' @param target target.
#' @return 
#' 
#' @author Micha³ Danaj
#' @export
model_dev<-function(predicted, target){
	-2*sum(target*log(predicted)+(1-target)*log(1-predicted))
}


#' Zastêpuje braki danych
#' 
#' @param x x
#' @param val jak¹ wartoœci¹ zast¹piæ brak danych. 
#' @return 
#' 
#' @author Micha³ Danaj
#' @export
na.subst<-function(x, val){
	x[is.na(x)]<-val;
	x
}


#' Wrzuca dane do schowka, do wklejenia do Excela
#' @param table \code{data.frame} do przekazania do Excela. 
#' @param row.names  czy przekazaæ równie¿ wiersze.
#' @return 
#' 
#' @author Micha³ Danaj
#' @export
to_excel<-function(table, row.names=FALSE){
	write.table(table, file='clipboard', sep='\t', row.names=row.names, dec=',')
}


#do skasowania!!! 
#' Liczy WoE
#' 
#' @param br wartoœæ prawdopodobieñstwa.
#' @return 
#' 
#' @author Micha³ Danaj
#' @export
woe<-function(br){
	br[br==0]<-0.5/length(br)
	br[br==1]<-(length(br)-0.5)/length(br)
	log(br/(1-br))
}


#' Zwraca ostatni dzieñ podanego okresu.
#'
#' @param dat Obiekt klasy \link{Date}, lub daj¹cy siê skonwertowaæ funkcj¹ \link{as.Date}.
#' @param unit Jednostka czasu, która ma byæ uwzglêdniona przy wyznaczaniu ostatniego dnia.
#'  Mo¿liwe wartoœci: \code{c("month","quater", "year")}, które odpowiadaj¹ ostatniemu dniu miesi¹ca, kwarta³u, roku,
#'  do którego nale¿y data \code{dat}.
#' @param ... Opcje do funkcji \link{as.Date}.
#' @author Micha³ Danaj
#' @export
lastDay<-function(dat, unit=c("month","quarter", "half_year", "year"),...) {
	
	# Wstêpne przygotowania
	unit<-match.arg(unit);
	
	dat<-as.Date(dat,...);
	
	# Pobieram rok i miesi¹c
	rok<-as.numeric(format(dat, '%Y'));
	mies<-as.numeric(format(dat, '%m'));
	
	# Jeœli ostatni dzieñ kwarta³u, to wybieram ostatni miesi¹c kwarta³u
	if (unit=="quarter")
		mies<-((mies-1)%/%3+1)*3
	
	if (unit=="half_year")
		mies<-((mies-1)%/%6+1)*6
	
	if (unit=="year")
		mies<-12;
	
	# W³aœciwy algorytm
	ostatni<-mies==12;
	mies[!ostatni]<-mies[!ostatni]+1;
	mies[ostatni]<-1;
	rok[ostatni]<-  rok[ostatni]+1;
	
	# zamiana na datê
	new_date<-as.Date(ISOdate(rok, mies, 1));
	return(new_date-1);
}


#' Na podstawie nazwy zmiennej celu i zmiennych objaœniaj¹cych, tworzy formu³ê modelu
#' 
#' @param target zmienna celu 
#' @param vars zmienne objaœniaj¹ce
#' @return forum³a do zastosowania w modelu
#' 
#' @author Micha³ Danaj
#' @export
make_model_formula<-function(target, vars){
	
	#jeœli w nazwach zmiennych jest nazwa targetu, to j¹ usuwam
	czy_jest_target<-target==vars
	vars<-vars[!czy_jest_target]
	
	#suma zmiennych
	suma_zmiennych<-paste(vars, collapse = ' + ')
	
	#wynik
	formula(paste(target, suma_zmiennych, sep='~'))
}
