# TODO: Add comment
# 
# Author: Piotr
###############################################################################


#' Grupuje zmienne i generuje dla nich listê statystyk
#' 
#' Funkcja grupuje zmienne ci¹g³e, zgodnie z domyœlnymi parametrami. Na grupach oraz na zmiennych kategorycznych 
#' wylicza statytystyki i przedstawia je w formie listy, osobno dla ka¿dej ze zmiennych. Wykorzystuje do tego funkcjê
#' \code{\link{univariate_anal_stats}}.
#' @param x_df \code{data.frame} ze zmiennymi do anliz.
#' @param y \code{vector} z wartoœciami zmiennej odpowiedzi.
#' @param vsub_bool \code{vector} logiczny okreœlaj¹cy, które zmienne z \code{df} powinny zostaæ zanalizowane.
#' @param vsub_list \code{vector} z nazwami zmiennych z \code{df}, które powinny zostaæ zanalizowane.
#' @param vsub_rola \code{vector} z rol¹ ka¿dej ze zmiennych. Zostan¹ zanalizowane zmienne, dla których rola 
#' 	ustawiona jest na \code{'explanatory'}. 
#' @param proby \code{data.frame} z wektorami logicznymi, okreœlaj¹cymi podpróby na których nale¿y przeanalizowaæ podpróby.
#' @param czas \code{vector} ze zmienn¹ czasow¹, po której zostan¹ podzielone analizy.
#' @param ... dodatkowe parametry do funkcji \code{\link{univariate_anal_stats}}. 
#' @return lista ze statstykami dla ka¿dej ze zmiennych okreœlonych przez któryœ z wektorów vsub\*. Nazwy elementów listy
#' 	s¹ nazwami zmiennych.
#' @seealso \code{\link{univariate_anal_stats}}
#' 
#' @author Michal Danaj
#' @export
univariate_loop<-function(x_df
			, y
			, vsub_bool = rep(TRUE, ncol(x_df))
			, vsub_list = NULL
			, vsub_rola = NULL
			, proby = rep(TRUE, length(y))
			, czas = rep(1, length(y))
			, ...){
	
	zmienne_names <- names(x_df)
	
	#które zmienne do analizy
	if (!is.null(vsub_rola))
		vsub_bool <- vsub_rola=='explanatory'
	
	wyniki<-list()
	
	for (zmienna in zmienne_names)
	{
		
		i<-which(zmienne_names %in% zmienna)
		if (vsub_bool[i]==FALSE)
			next;
		
		
		x<-x_df[,i]
		
		print ('------------------------------------------------------------------------------')
		print ('------------------------------------------------------------------------------')
		print ('------------------------------------------------------------------------------')
		print(i)
		print(zmienna)
		
		
		wyniki[[zmienna]]<-univariate_anal_stats(x, y,
				proby=proby,
				czas=czas,
				...
		)
		
	}
	
	wyniki
}	
