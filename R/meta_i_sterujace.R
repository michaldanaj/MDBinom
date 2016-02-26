# TODO: Add comment
# 
# Author: Piotr
###############################################################################



NA_substit=-.Machine$integer.max;
special_val=c(-99999999, -10000001, -10000000, -9999999,  -9999998,  -9999997, -9999996);


numeric_var_treatment.params<-list(
		#przy jakiej warto�ci unikalnych warto�ci zmiennej ma j� traktowa� jako dyskretn�
		discret_threshold=15,
		
		#kody r�nych warto�ci specjalnych. B�d� one traktowane osobno, jako warto�ci dyskretne
		spcial_val=-.Machine$integer.max,
		
		#warto�� do zast�pienia missing value
		NA_substit=-.Machine$integer.max,
		
		#Graniczny udzia� warto�ci, powy�ej kt�rej traktujemy j� jako warto�� specjaln� 
		#(w spos�b dyskretny, wydzielon� z pozosta�ych).
		separate_value_thr=0.1,		
		
		#maksymalna g��boko�� budowy drzewa moim algorytmem
		max_gleb=3,
		
		#minimalna liczba obserwacji - do sprawdzenia - w li�ciu/w w�le do podzia�u
		min_bucket=200,
		
		#warto�� graniczna nulli. Poni�ej robimy imputacj�, powy�ej traktujemy je jako osobn� grup�
		nulle_do_imp_thr=0.0

)



# TODO dorobi� wy�wietlanie, kt�re role zosta�y wprowadzone niepoprawnie.
#' Generuje kod do zmiany roli zmiennych lub data.frame z rolami
#' 
#' Generuje kod do zmiany roli zmiennych lub data.frame ze zmienionymi rolami.
#' @param zmienne_rola \code{data.frame} z rolami zmiennych.
#' @param pattern Wyra�enie przekazane do funkcji \code{grep} filtruj�ce zmienne
#' @param gen_code Je�li TRUE, wynikiem funkcji jest kod do zmiany warto�ci. W przeciwnym razie, zwracany
#'			jest data.frame ze zmienionymi rolami.  
#' @return 
#' 
#' @author Piotr
#' @export
editVariablesRole<-function(zmienne_rola, pattern=NULL, gen_code=TRUE){
	
	rola<-c("rejected", "explanatory", "target", "keep");
	
	if (is.null(pattern))
		wynik<-edit(zmienne_rola)
	else{
		indeksy<-grep(pattern, rownames(zmienne_rola), ignore.case = TRUE)
		wynik<-zmienne_rola
		wynik[indeksy,]<-edit(zmienne_rola[indeksy,])
		
		if (any(!(wynik$rola%in%rola))){
			blad<-"Niepoprawne warto�ci r�l!"
			#stop(rownames(zmienne_rola)[!wynik$rola%in%rola])
			stop(blad)
		}
	}
	
	#sprawdzam, kt�rych zmiennych role si� zmieni�y
	zmiany<- zmienne_rola$rola!=wynik$rola
	nazwy_zmienionych<-rownames(zmienne_rola)[zmiany]
	nowe_wartosci<-wynik$rola[zmiany]
	
	#dla tych zmiennych generuj� kod do zmiany ich r�l
	nazwy_przecinek<-paste("c('", paste(nazwy_zmienionych, collapse = "','"), "')", sep="")
	wartosci_przecinek<-paste("c('", paste(nowe_wartosci, collapse = "','"), "')", sep="")
	
	if (gen_code==TRUE)
		paste(deparse(substitute(zmienne_rola)),"[",nazwy_przecinek,",'rola']<-", wartosci_przecinek, sep="")
	else
		wynik
}


#########################################
