# TODO: Add comment
# 
# Author: Micha³ Danaj
###############################################################################



#' Podstawowe statystyki faktorów
#' 
#' Wiele z faktorów zaczytanych bezpoœrednio z pliku nie nadaje siê do modelowania. Funkcja umo¿liwia wstêpn¹ 
#' analizê faktorów. 
#' @param dane 
#' @return 
#' 
#' @author Micha³ Danaj
#' @export
prel_factors<-function(dane){
	faktory<-sapply(dane, is.factor)
	faktory_data<-dane[,faktory]
	lapply(faktory_data, summary)
}



#' Wyœwietla zmienne numeryczne z wariancj¹ == 0
#' 
#' Wyœwietla zmienne numeryczne z jedn¹ wartoœci¹.
#' @param dane  
#' @return 
#' 
#' @author Micha³ Danaj
#' @export
prel_numeric_sd0 <- function(dane){
	ciagle<-sapply(dane, function(x)is.numeric(x) & !is.factor(x))
	ciagle_names<-names(dane)[ciagle]
	
	sd_ciagle<-lapply(dane[,ciagle], sd, na.rm=TRUE)
	sd_ciagle<-nvl(sd_ciagle,0)
	
	do_wyrzucenia_ciagle<-names(dane)[ciagle][sd_ciagle==0]
	
	do_wyrzucenia_ciagle
}


#' Wyœwietla zmienne z wartoœci¹ o du¿ym udziale w rozk³adzie
#' 
#' Wyœwietla nazwy zmiennych, dla których istenieje taka wartoœæ, ¿e jej udzia³ przekracza \code{rate}
#' próby. 
#' @param dane Dane.
#' @param rate Graniczny udzia³ jedenej wartoœci.
#' @return 
#' 
#' @author Micha³ Danaj
#' @export
prel_numeric_one_value <- function(dane, rate=0.98){
	
	ciagle<-sapply(dane, function(x)is.numeric(x) & !is.factor(x))
	ciagle_names<-names(dane)[ciagle]
	
	ciagle_table<-lapply(dane[,ciagle_names], function(x){
				z<-nvl(x,-9999999)
				max(table(z)/length(z))
			}
	)
	
	ciagle_table<-unlist(ciagle_table)	
	ciagle_names[ciagle_table> rate]
}



#' Wyœwietla wektory logiczne
#' 
#' Po wczytaniu danych z pliku tekstowego nie powinno byæ wektorów logicznych w innych przypadkach ni¿
#' pusty wektor (tego nie jestem tak na prawdê teraz pewny). Funkcja wypisuje wszystkie wektory logiczne
#' ¿eby mo¿na by³o je obejrzeæ. 
#' @param dane 
#' @return 
#' 
#' @author Micha³ Danaj
#' @export
prel_logical<-function(dane){
	names(dane)[unlist(sapply(dane,function(x)class(x)=='logical'))]
}


