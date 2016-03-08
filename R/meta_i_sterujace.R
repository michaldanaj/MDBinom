# TODO: Add comment
# 
# Author: Piotr
###############################################################################



NA_substit=-.Machine$integer.max;
special_val=c(-99999999, -10000001, -10000000, -9999999,  -9999998,  -9999997, -9999996);


#' Parametry
#'
#' Parametryzacja funkcji z pakietu.
#'
#' @format Wektor o polach:
#' \describe{
#'   \item{discret_threshold}{Przy jakiej wartoœci unikalnych wartoœci zmienna numeryczna bêdzie traktowana jako dyskretna.}
#'   \item{special_val}{Kody ró¿nych wartoœci specjalnych. Bêd¹ one traktowane jako wartoœci dyskretne, mimo ¿e pozosta³a czêœæ zmiennej mo¿e byæ ci¹g³a.}
#' }
#' @export 
numeric_var_treatment.params<-list(
		#przy jakiej wartoœci unikalnych wartoœci zmiennej numerycznej ma j¹ traktowaæ jako dyskretn¹
		discrete_threshold=15,
		
		#liczba unikalnych wartoœci zmiennej kategorycznej, powy¿ej której nie generuje statystyk
		#w funkcji univariate_stats1. W przypadku przekroczenia, zwracany jest komunikat "Too many categorical levels".
		no_stats_threshold=15,
		
		#kody ró¿nych wartoœci specjalnych. Bêd¹ one traktowane osobno, jako wartoœci dyskretne
		special_val=-.Machine$integer.max,
		
		#wartoœæ do zast¹pienia missing value
		NA_substit=-.Machine$integer.max,
		
		#Graniczny udzia³ wartoœci, powy¿ej której traktujemy j¹ jako wartoœæ specjaln¹ 
		#(w sposób dyskretny, wydzielon¹ z pozosta³ych).
		separate_value_thr=0.1,		
		
		#maksymalna g³êbokoœæ budowy drzewa moim algorytmem
		max_gleb=3,
		
		#minimalna liczba obserwacji - do sprawdzenia - w liœciu/w wêŸle do podzia³u
		min_bucket=200,
		
		#wartoœæ graniczna nulli. Poni¿ej robimy imputacjê, powy¿ej traktujemy je jako osobn¹ grupê
		nulle_do_imp_thr=0.0

)



# TODO dorobiæ wyœwietlanie, które role zosta³y wprowadzone niepoprawnie.
#' Generuje kod do zmiany roli zmiennych lub data.frame z rolami
#' 
#' Generuje kod do zmiany roli zmiennych lub data.frame ze zmienionymi rolami.
#' @param zmienne_rola \code{data.frame} z rolami zmiennych.
#' @param pattern Wyra¿enie przekazane do funkcji \code{grep} filtruj¹ce zmienne
#' @param gen_code Jeœli TRUE, wynikiem funkcji jest kod do zmiany wartoœci. W przeciwnym razie, zwracany
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
			blad<-"Niepoprawne wartoœci ról!"
			#stop(rownames(zmienne_rola)[!wynik$rola%in%rola])
			stop(blad)
		}
	}
	
	#sprawdzam, których zmiennych role siê zmieni³y
	zmiany<- zmienne_rola$rola!=wynik$rola
	nazwy_zmienionych<-rownames(zmienne_rola)[zmiany]
	nowe_wartosci<-wynik$rola[zmiany]
	
	#dla tych zmiennych generujê kod do zmiany ich ról
	nazwy_przecinek<-paste("c('", paste(nazwy_zmienionych, collapse = "','"), "')", sep="")
	wartosci_przecinek<-paste("c('", paste(nowe_wartosci, collapse = "','"), "')", sep="")
	
	if (gen_code==TRUE){
		kod<-paste(deparse(substitute(zmienne_rola)),"[",nazwy_przecinek,",'rola']<-", wartosci_przecinek, sep="")
		cat(kod)
	}
	else
		wynik
}


#########################################

