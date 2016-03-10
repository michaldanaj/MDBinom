# TODO: Add comment
# 
# Author: Piotr
###############################################################################

#TODO doda� do struktur w dyskretyzacj�, oraz do metadanych, typ danych
#Na razie robi� to r�cznie. Dlatego potrzebuj� zmeinnej \code{dane}
#aby okre�li� typy danych

#' Na podstawie dyskretyzacji i modelu generuje SQL do implementacji
#' 
#' 
#' @param wyniki wyniki dyskretyzacji.
#' @param dane dane ze zmiennymi z modelu do okre�lenia typu danych.
#' @param model model logistyczny.
#' @return Zwraca gotowego sql-a oraz kopiuje go do schowka.
#' 
#' @author Micha� Danaj
#' @export
genSQL<-function(wyniki, dane, model){
	
	#nazwy zmiennych wynikowych z case'�w
	varNames<-paste("var", 1:length(wyniki), sep="")
	sql<-c(
			genSQLCases(wyniki, dane, varNames),
			",",
			genSQLScore(model, varNames)
	)
	write.table(sql, file='clipboard', quote=FALSE, row.names=FALSE, col.names=FALSE)
	sql
}

## 
## x=3
## genSQLCase(bucket=wyniki_reczna_dyskr[[x]][[1]], 
##         nazwaZmiennej=names(wyniki_reczna_dyskr[x]), 
##         typZmiennej="numeric", 
##         nazwaZmiennejOut='xyz')


#TODO chyba trzeba doda� obs�ug� mapowania
#' Generuje kod SQL dla dyskretyzacji jednej zmiennej.
#' 
#' Generuje kod SQL dla dyskretyzacji jednej zmiennej na podstawie danych w \code{bucket},
#' przypisuj�cy woe.
#' @param bucket dyskretyzacja zmiennej.
#' @param nazwaZmiennej nazwa zmiennej w danych do score'owania.
#' @param typZmiennej je�li zmienna jest znakowa, powinna przyj�� warto�� \code{'character'}.
#' @param nazwaZmiennejOut nazwa wynikowej zmiennej.
#' @return zwraca \code{vector} znakowy z kodem SQL.
#' 
#' @author Micha� Danaj
#' @export
genSQLCase<-function(bucket, nazwaZmiennej, typZmiennej, nazwaZmiennejOut){
	
	#je�li jest wiersz z podsumowaniem, to go usuwam
	bucket<-bucket[bucket$label != 'TOTAL',] 
	
	#robi� zaokr�glenie
	bucket$woe<-round(bucket$woe, 7)
	
	kodzik_dyskr<-character()
	kodzik_ciagle<-character()
	kodzik_null<-character()
	
	#Najpierw obs�uguj� warto�ci dyskretne
	gdzie_dyskretne<-is.na(bucket$srodek)
	if (any(gdzie_dyskretne)){
		
		ciapek=''
		if (typZmiennej=='character')
			ciapek="'"
		
		dyskretne <- bucket[gdzie_dyskretne,]
		kodzik<-paste("WHEN ", nazwaZmiennej, ' = ', ciapek, dyskretne$discret, ciapek, ' THEN ', dyskretne$woe, sep='')
		
		#podmieniam dla warto�ci null
		#TODO Obs�u�y� to lepiej! Najpierw wygenerowa� woe dla null, w zale�no�ci czy jawne czy nie
		#a dopiero p�niej wygenerowa� kod w zale�no�ci od typu zmiennej
		gdzie_null<-dyskretne$discret==numeric_var_treatment.params$NA_substit | dyskretne$discret==''
		kodzik[gdzie_null]<-paste("WHEN", nazwaZmiennej, 'IS NULL', 'THEN', dyskretne$woe[gdzie_null])
		kodzik_dyskr<-kodzik
		
	}
	
	#teraz zmienne ci�g�e	
	gdzie_ciagle<- !is.na(bucket$srodek)
	
	if (any(gdzie_ciagle)){
		
		ciagle <- bucket[gdzie_ciagle,]
		kodzik<-paste("WHEN", nazwaZmiennej, '>=', ciagle$od,'AND', nazwaZmiennej, '<', ciagle$do,'THEN', ciagle$woe)
		
		#podmieniam pierwszy wiersz, aby by� od minus niesko�czono�ci
		kodzik[1]<-paste("WHEN", nazwaZmiennej, '<', ciagle$do[1],'THEN', ciagle$woe[1])
		
		#podmieniam ostatni wiersz, aby by� od minus niesko�czono�ci		
		kodzik[nrow(ciagle)]<-paste("WHEN", nazwaZmiennej, '>=', ciagle$od[nrow(ciagle)],'THEN', ciagle$woe[nrow(ciagle)])
		
		kodzik_ciagle<-kodzik
		
	}
	
	#jeszcze else na koniec
	kodzik_else<-"     ELSE NULL"
	
	#generuj� ca�y kod, dodaj�c warunek else
	kodzik_all<-c('CASE',
			paste("    ",c(kodzik_dyskr, kodzik_ciagle, kodzik_null)),
			kodzik_else,
			paste('END as ', nazwaZmiennejOut,'\n', sep=''))	
	
	kodzik_all_cat<-paste(kodzik_all, collapse='\n')
	
	
	kodzik_all_cat
	
}

#genSQLCases(wyniki_reczna_dyskr, sample0_int, varNames)

#TODO doda� do struktur w dyskretyzacj�, oraz do metadanych, typ danych
#Na razie robi� to r�cznie. Dlatego potrzebuj� zmeinnej \code{dane}
#aby okre�li� typy danych

#' Generuj� list� kod�w SQL z dyskretyzacj� wszystkich zmiennych z \code{wynik}..
#' 
#' Generuj� list� kod�w SQL z dyskretyzacj� wszystkich zmiennych z \code{wynik}.
#' @param wyniki wyniki dyskretyzacji.
#' @param dane dane ze zmiennymi z modelu do okre�lenia typu danych.
#' @param varNames \code{vector} z nazwami zmiennych wynikowych.
#' @return zwraca list� z kodami dla poszczeg�lnych zmiennych.
#' 
#' @author Piotr
#' @export
genSQLCases<-function(wynik, dane, varNames){
	
	cases<-list()
	
	#wyznaczam typ danych, p�niej trzeba by to zast�pi� metadanymi
	for (i in 1:length(wynik)){
		typ_danych<-typeof(dane[,names(wynik[i])])
		
		cases[[i]]<-genSQLCase(wynik[[i]][[1]], names(wynik[i]), typ_danych, nazwaZmiennejOut = varNames[i])
		names(cases)[i]<-names(wynik[i])
		
	}
	
	cases2<-character()
	for (i in 1:length(wynik)){
		cases2<-c(cases2, cases[[i]])
		if (i<length(wynik))
			cases2<-c(cases2,",")		
	}
	
	cat("\n\n#########   	Kod z przekszta�ceniami do wklejenia:   		############\n\n")
	cat(cases2)
	cat("\n\n#########   Koniec kodu z przekszta�ceniami do wklejenia   ############\n\n")
	
	cases2
}

#TODO doda� do struktur w dyskretyzacj�, oraz do metadanych, typ danych
#Na razie robi� to r�cznie. Dlatego potrzebuj� zmeinnej \code{dane}
#aby okre�li� typy danych
#' Generuj� list� kod�w SQL z dyskretyzacj� wszystkich zmiennych z \code{wynik}..
#' 
#' Generuj� list� kod�w SQL z dyskretyzacj� wszystkich zmiennych z \code{wynik}.
#' @param wyniki wyniki dyskretyzacji.
#' @param dane dane ze zmiennymi z modelu do okre�lenia typu danych.
#' @param mapping mapowanie nazwy �r�d�owej (przed zastosowniem dyskretyzacji z \code{wynik}
#' 			na nazw�, kt�ra jest p�niej w modelu. Je�li jedna i druga nazwa jest taka sama,
#'			to znaczy �e nie by�o robionej dyskretyzacji i case jest dla tej zmeinnej
#'			pomini�ty.
#' @return zwraca list� z kodami dla poszczeg�lnych zmiennych.
#' @author Piotr
#' @export
genSQLCases2<-function(wynik, dane, mapping){
	
	cases<-list()
	
	#ograniczam list� wynik tylko do tych zmiennych, kt�re maj�
	#zmienion� nazw� w mapping
	rozneNazwy<-mapping$sourceVarName != mapping$modelVarName
	wynik<-wynik[mapping$sourceVarName[rozneNazwy]]
	
	#wyznaczam typ danych, p�niej trzeba by to zast�pi� metadanymi
	for (i in 1:length(wynik)){
		typ_danych<-typeof(dane[,names(wynik[i])])
		
		cases[[i]]<-genSQLCase(wynik[[i]][[1]], names(wynik[i]), typ_danych, nazwaZmiennejOut = varNames[i])
		names(cases)[i]<-names(wynik[i])
		
	}
	
	
	## cases<-lapply(wynik, function(x){
	##             genSQLCase(x[[1]], names(x), attributes(x[[1]])$type)
	##         }
	## )
	
	cases2<-character()
	for (i in 1:length(wynik)){
		cases2<-c(cases2, cases[[i]])
		if (i<length(wynik))
			cases2<-c(cases2,",")		
	}
	
	cat("\n\n#########   	Kod z przekszta�ceniami do wklejenia:   		############\n\n")
	cat(cases2)
	cat("\n\n#########   Koniec kodu z przekszta�ceniami do wklejenia   ############\n\n")
	
	cases2
}


#' Kod SQL modelu logistycznego.
#' 
#' @param model model logistyczny. 
#' @param varNames alternatywne nazwy zmiennych. Je�li nie podane, to pobrane z \code{model}.
#' @return Kod SQL.
#' 
#' @author Micha� Danaj
#' @export
genSQLModel<-function(model, varNames=NULL){
	
	wspolczynniki<-round(coef(model),7)
	
	if (is.null(varNames))
		varNames=names(coef(model))[-1]
	
	#dodaj� nazw� NA na pocz�tek, bo we wsp�czynnikach mam dodatkowo itercept
	varNames<-c(NA,varNames)
	
	
	score_lin<-paste(wspolczynniki,' * ', varNames, '+')
	
	#podmieniam intercept
	score_lin[1]<-paste(wspolczynniki[1],'+')
	
	#usuwam + z ostatniego wiersza
	i<-length(score_lin)
	score_lin[i]<-paste(wspolczynniki[i],' * ', varNames[i])
	
	score_lin<-c(score_lin, 'as score_lin')
	
	score_lin<-c(score_lin,"score_pd = 1/(1+exp(-score_lin));")
	
	score_lin_cat<-paste(score_lin, collapse="\n")
	
	cat("\n\n#########   	Kod scoringowy do wklejenia:   		############\n\n")
	cat(score_lin_cat)
	cat("\n\n#########   Koniec kodu scoringowy do wklejenia   ############\n\n")
	score_lin_cat
}	

