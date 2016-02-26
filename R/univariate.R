# Funkcje zaczynaj�ce si� na univariate
# 
# Author: Piotr
###############################################################################



#' Dyskretyzuje zmienn� i wylicza dla niej statystyki
#'
#' Wylicza statystyki i zwraca je w postaci listy.
#' @param x zmienna, po kt�rej procedura b�dzie sortowa�.
#' @param y zmienna odpowiedzi.
#' @param czas podzia� na okresy czasowe.
#' @param proby \code{data.frame}, w kt�rym ka�da kolumna jest wektorem logicznym,
#'       zawieraj�cym informacje, czy obserwacja nale�y do danej pr�by.
#' @param interactive TRUE, je�li zmienna ma by� dyskretyzowana interaktywnie. W
#'                   przeciwnym razie, co jest warto�ci� domy�ln�, dyskretyzacja
#'                   jest automatyczna.
#' @param min_bucket minimalna liczba obserwacji w buckecie, przy dzieleniu drzewem.
#' @param breaks zamiast automatycznego dzielenia, mo�na poda� warto�ci przedzia��w (from,to].
#' @param mapping zamiast automatycznego dzielenia, mo�na poda� mapowanie.
#' @param forceContinous wymusza potraktowanie zmiennej jako ci�g��, mimo �e liczba
#'                      unikalnych warto�ci jest mniejsza ni� \code{discret_treshold}.
#' @param special_val Warto�ci specjalne do usuni�cia z automatycznego podzia�u. B�d� traktowane jako zmienne
#' 					  kategoryczne.
#' @param NA_subst warto�� jaka ma by� przypisana w miejsce brak�w danych. Dalsze analizy
#'		b�d� przeporwadzone w standardowy spos�b. Je�li jednak warto�� \code{NA_subst} zostanie
#' 		dodana do \code{special_val}, zostanie ona potraktowana jako warto�� dyskretna.
#' @param span Parametr wyg�adzaj�cy funkcji \code{\link[locfit]{locfit}}.  
#' @author Micha� Danaj
#' @export
univariate_anal_stats<-function(x,y,czas,proby=rep(TRUE, length(y)),
		interactive=FALSE, min_bucket=floor(0.05*length(x)), breaks=NULL,
		mapping=NULL, forceContinous=FALSE, 
		special_val=numeric_var_treatment.params$spcial_val, 
		NA_subst=numeric_var_treatment.params$NA_substit,
		span=0.9){
	
	# zamieniam braki danych na liczb�.
	if (is.numeric(x) & !is.null(NA_subst))
		x[is.na(x)]<-NA_subst;
	
	
	# dyskretyzuj� zmienn� i wyliczam pierwsze statystyki
	stat1<-univariate_anal_stats1b(x,y, special_val=special_val,
			max_gleb=3,plot=FALSE, min_bucket=min_bucket,
			interactive=interactive, breaks=breaks, mapping=mapping,
			forceContinous=forceContinous, span=span);
	
	stat2<-NULL;
	stat3<-NULL;
	#Dalsze statystyki robi� pod warunkiem, �e jest wi�cej ni� jedna warto�� dyskretna
	#(W stat1 jest te� <TOTAL>, dlatego 2)
	if (nrow(stat1)>2){
		
		# przypisuj� nazw� bucketu
		stat1$fitted<-stat1$label;
		x_discr<-przypisz2(x,stat1);
		
		# przypisuj� BR
		stat1$fitted<-stat1$br;
		BR_discr<-przypisz2(x,stat1);
		
		# wyliczam drugie statystyki
		stat2<-univariate_anal_stats2(x_discr, y, czas, BR_discr);
		
		# wyliczam trzecie statystyki (GINI) po zadanych pr�bach i czasie
		
		stat3<-univariate_anal_stats3(score=-BR_discr, y, czas, proby);
	}
	return(list(dyskretyzacja=stat1, rozklady=stat2, dyskryminacja=stat3));
}



####################




################################################################################
#                    zmienione funkcjie z pakietu binom                        #
################################################################################





#' Wylicza statystyki dla zmiennej score'owej i obja�nianej wg zadanych bucket�w
#'
#' Wylicza statystyki i zwraca je w postaci listy. Do wyliczenia \code{AR} u�ywa jako zmiennej
#' score'owej oryginalnych warto�ci BR przypisanych do bucketu. Tzn, mimo �e na nowych danych kolejno��
#' bucket�w, sortuj�c je po br mo�e by� inna ni� oryginalnie, to stosowana jest oryginalna kolejno��.
#' @param buckets \code{data.frame} z podzia�em zmiennej na buckety.
#' @param x zmienna, po kt�rej procedura b�dzie sortowa�.
#' @param y zmienna odpowiedzi.
#' @param czas podzia� na okresy czasowe.
#' @param proby \code{data.frame}, w kt�rym ka�da kolumna jest wektorem logicznym,
#'       zawieraj�cym informacje, czy obserwacja nale�y do danej pr�by.
#' @author Micha� Danaj
#' @export
univariate_stats_new_data<-function(buckets,x,y,czas,proby=rep(TRUE, length(y))){
	buckets$fitted<-buckets$label;
	nowe_wartosci<-przypisz2(x, buckets)
	buckets_new<-univariate_anal_stats(nowe_wartosci, y, czas=czas, proby=proby)
	stat1<-cbind(buckets[,c('nr','label','discret','od','srodek','do')], 
			buckets_new$dyskretyzacja[buckets$label,c('n_good','pct_good','n_bad','pct_bad','n_obs','pct_obs','br','woe','logit')],
			br_orig=buckets$br);
	
	stat2<-NULL;
	stat3<-NULL;
	#Dalsze statystyki robi� pod warunkiem, �e jest wi�cej ni� jedna warto�� dyskretna
	#(W stat1 jest te� <TOTAL>, dlatego 2)
	if (nrow(stat1)>2){
		# przypisuj� nazw� bucketu
		stat1$fitted<-stat1$label;
		x_discr<-przypisz2(x,stat1);
		
		# przypisuj� BR
		stat1$fitted<-stat1$br;
		BR_discr<-przypisz2(x,stat1);
		
		# wyliczam drugie statystyki
		stat2<-univariate_anal_stats2(x_discr, y, czas, BR_discr);
		
		# wyliczam trzecie statystyki (GINI) po zadanych pr�bach i czasie
		buckets$fitted<-buckets$br;
		stare_BR<-przypisz2(x,buckets);
		stat3<-univariate_anal_stats3(-stare_BR, y, czas, proby);
		attr(stat3,'comment')<-"AR policzony w oparciu o oryginalne warto�ci BR."
	}
	return(list(dyskretyzacja=stat1, rozklady=stat2, dyskryminacja=stat3));
}



#' Dyskretyzuje zmienn� i wylicza na niej statystyki
#'
#' W przypadku, gdy liczba unikalnych warto�ci zmiennej jest <= \code{discret_treshold}
#' lub zmienna nie jest zmienn� numeryczn�,
#' uznaje �e zmienna jest dyskretna i jedynie wylicza dla niej statystyki. W przeciwnym
#' wypadku dyskretyzuje zmienn� i wylicza statystyki.
#' @param discret_treshold je�li liczba unikalnych warto�ci zmiennej jest nie wi�ksza
#'        ta warto��, zmienna uznana jest za dyskretn� i nie jest poddawana dyskretyzacji.
#' @param interactive TRUE, je�li zmienna ma by� dyskretyzowana interaktywnie. W
#'                   przeciwnym razie, co jest warto�ci� domy�ln�, dyskretyzacja
#'                   jest automatyczna.
#' @param breaks zamiast automatycznego dzielenia, mo�na poda� warto�ci przedzia��w (from,to].
#' @param forceContinous wymusza potraktowanie zmiennej jako ci�g��, mimo �e liczba
#'                      unikalnych warto�ci jest mniejsza ni� \code{discret_treshold}.
#' @seealso \code{\link{buckety_stat}}.
#' @export
univariate_anal_stats1<-function(x,y, discret_treshold=15,
		special_val=NULL, max_gleb=3, min_bucket=200, interactive=FALSE,
		breaks=NULL, mapping=NULL, forceContinous=FALSE,...){
	
	if (length(x)!=length(y))
		stop("paramet ry 'x' i 'y' maj� r�ne d�ugo�ci!");
	
	#Mimo, �e przygotowywya�em funkcj� do obs�ugi null-i, to rezygnuj� z tego
	#ze wzgl�d�w bezpiecze�stwa.
	if (any(is.na(y)))
		stop ("W 'y' nie mo�e by� NA!");
	
	## je�li jest to ju� zmienna dyskretna;
	
	## je�li jest to zmienna dyskretna
	if ((length(unique(x))<=discret_treshold || !is.numeric(x))&&
			is.null(breaks) && !forceContinous){
		discret<-buckety_stat(x, y, total=TRUE);
		#uzupe�niam statystyki
		discret$od<-NA;
		discret$do<-NA;
		discret$srodek<-NA;
		
		nam<-rownames(discret)
		if (is.numeric(x)){
			nam[length(nam)]<-NA
			discret$discret<-as.numeric(nam)
		}
		else{
			nam[length(nam)]<-"<TOTAL>";
			discret$discret<-nam;
		}
		
		discret<-discret[,c('nr','label','discret', 'od','srodek','do','n_good','pct_good','n_bad','pct_bad','n_obs','pct_obs',
						'br','woe','logit')]
	}
	## je�li jest to zmienna ci�g�a
	else{
		discret<-discretization(x,y, special_val=special_val,
				max_gleb=max_gleb,min_bucket=min_bucket,breaks=breaks,
				interactive=interactive,...);
	}
	
	discret$label<-rownames(discret);
	return(discret);
}

#' Robi rozk�ad zmiennej po czasie (lub innym podziale)
#'
#' Wylicza liczno�ci dla ka�dego poziomu \code{x_discr} oraz �redni� warto�� \code{y} oraz \code{esitm}
#' w podziale na zadane grupy czasowe \code{czas} (lub podzia� innego typu).
#' @param x_discr zmienna obja�niaj�ca.
#' @param y zmienna odpowiedzi.
#' @param czas Czas.
#' @param estim warto�� wyestymowana przez model.
#' @export
univariate_anal_stats2<-function(x_discr, y, czas, estim){
	
	#liczno�ci po czasie i zmiennej
	total<-table(x_discr);
	total_czas<-table(czas);
	all_tbl<-cbind(table(x_discr, czas), TOTAL=total);
	all_tbl<-rbind(all_tbl, c(total_czas, length(x_discr)));
	rownames(all_tbl)[nrow(all_tbl)]<-'TOTAL';
	
	
	#rozk�ad ka�dego bucketu po czasie (agreguj�c po czasie zsumuje si� do 1)
	pct_all_tbl<-sweep(all_tbl, 2, STATS=c(total_czas,length(x_discr)), FUN="/");
	
	#�rednie LGD po czasie i zmiennej
	total<-tapply(y, list(x_discr), mean)
	avg_y<-tapply(y, list(x_discr,czas), mean);
	avg_y<-cbind(avg_y, TOTAL=total);
	total_czas<-c(tapply(y, list(czas), mean), mean(y));
	avg_y<-rbind(avg_y, total_czas);
	rownames(avg_y)[nrow(avg_y)]<-'TOTAL';
	return(list(obs_all_tbl = all_tbl, pct_all_tbl=pct_all_tbl, avg_t_tbl=avg_y,
					estim=tapply(estim, czas, mean)));
}

#' Wylicza AR po czasie i w podziale na zadane pr�by
#'
#' @param score zmienna, po kt�rej procedura b�dzie sortowa�.
#' @param y zmienna odpowiedzi.
#' @param czas podzia� na okresy czasowe.
#' @param proby \code{data.frame}, w kt�rym ka�da kolumna jest wektorem logicznym,
#'       zawieraj�cym informacje, czy obserwacja nale�y do danej pr�by.
#' @export
univariate_anal_stats3<-function (score, y, czas, proby){
	razem<-data.frame(score,y,czas);
	razem$czas<-as.factor(razem$czas);
	
	if (!is.data.frame(proby)){
		proby<-as.data.frame(proby);
	}
	
	
	for (i in 1:ncol(proby)){
		proba<-razem[proby[,i], ];
		AR_calosc<-AR(proba$score, proba$y)[[1]]['AR'];
		aery<-sapply(split(proba, proba$czas), FUN=function(pr){
					
					if (nrow(pr)==0)
						return(NA);
					wyn<-AR(pr$score, pr$y)[[1]]['AR'];
					names(wyn)<-NULL;
					return(wyn)
				}
		);
		
		if (i==1){
			wyniki_AR<-data.frame(aery);
			wyniki_AR<-cbind(AR_calosc,t(wyniki_AR));
		}
		else
			wyniki_AR<-rbind(wyniki_AR, c(AR_calosc,aery));
	}
	
	rownames(wyniki_AR)<-colnames(proby);
	colnames(wyniki_AR)<- colnames(wyniki_AR);
	return(wyniki_AR);
}



#' Dyskretyzuje zmienn� i wylicza na niej statystyki
#'
#' W przypadku, gdy liczba unikalnych warto�ci zmiennej jest <= \code{discret_treshold}
#' lub zmienna nie jest zmienn� numeryczn�,
#' uznaje �e zmienna jest dyskretna i jedynie wylicza dla niej statystyki. W przeciwnym
#' wypadku dyskretyzuje zmienn� i wylicza statystyki.
#' @param discret_treshold je�li liczba unikalnych warto�ci zmiennej jest nie wi�ksza
#'        ta warto��, zmienna uznana jest za dyskretn� i nie jest poddawana dyskretyzacji.
#' @param interactive TRUE, je�li zmienna ma by� dyskretyzowana interaktywnie. W
#'                   przeciwnym razie, co jest warto�ci� domy�ln�, dyskretyzacja
#'                   jest automatyczna.
#' @param breaks zamiast automatycznego dzielenia, mo�na poda� warto�ci przedzia��w (from,to].
#' @param forceContinous wymusza potraktowanie zmiennej jako ci�g��, mimo �e liczba
#'                      unikalnych warto�ci jest mniejsza ni� \code{discret_treshold}.
#' @seealso \code{\link{buckety_stat}}.
#' @export
univariate_anal_stats1b<-function(x,y, 
		locfit=FALSE, 
		discret_treshold=15,
		special_val=numeric_var_treatment.params$spcial_val, 
		max_gleb=3, 
		min_bucket=200, 
		interactive=FALSE,
		breaks=NULL, 
		mapping=NULL, 
		forceContinous=FALSE,
		span=0.9,...){
	
	if (length(x)!=length(y))
		stop("paramet ry 'x' i 'y' maj� r�ne d�ugo�ci!");
	
	#Mimo, �e przygotowywya�em funkcj� do obs�ugi null-i, to rezygnuj� z tego
	#ze wzgl�d�w bezpiecze�stwa.
	if (any(is.na(y)))
		stop ("W 'y' nie mo�e by� NA!");
	
	
	## je�li s� jakie� nulle w x, to odpowiednio si� nimi zajmuj�
	nulle<-is.na(x)
	if (any(nulle)){
		
		#je�li nulli jest mniej ni� za�o�ona cz�� populacji, to imputuj� je. W przeciwnym razie przypisuj� jako osobn� grup�.
		ile_nulli<-prop.table(table(nulle))
		if (ile_nulli["TRUE"]<numeric_var_treatment.params$nulle_do_imp_thr)
			#TODO zobaczy�, czy y ma dwie warto�ci i jest to 0 i 1
			x<-missing_bin_target(x, y)
		else
			x[nulle]<-numeric_var_treatment.params$NA_substit;
		
	}
	
	## patrz�, czy nie ma skupisk w jakich� warto�ciach. Je�li tak, to b�d� je traktowa� jako warto�ci specjalne
	## skupiska_freq<-prop.table(table(x))
	## skupiska <- skupiska_freq>numeric_var_treatment.params$separate_value_thr;
	## special_val<-unique(c(special_val,names(skupiska_freq)[skupiska]))
	
	
	## je�li jest to zmienna dyskretna lub mapowanie
	if (!is.null(mapping)||((length(unique(x))<=discret_treshold || !is.numeric(x))&&
				is.null(breaks) && !forceContinous)){
		
		if (!is.null(mapping))
			x<-mapuj(x, mapping)	
		
		discret<-buckety_stat(x, y, total=TRUE);
		
		
		## uzupe�niam statystyki ##
		
		# ci�g�e
		discret$od<-NA;
		discret$do<-NA;
		discret$srodek<-NA;
		
		#dyskretne
		nam<-rownames(discret)
		if (is.numeric(x)){
			nam[length(nam)]<-NA
			discret$discret<-as.numeric(nam)
		}
		else{
			nam[length(nam)]<-"<TOTAL>";
			discret$discret<-nam;
		}
		
		#mapowanie
		if(!is.null(mapping)) 
			discret$mapping_war<-mapping$war
		else
			discret$mapping_war<-NA
		
		discret<-discret[,c('nr','label','discret', 'mapping_war', 'od','srodek','do','n_good','pct_good','n_bad','pct_bad','n_obs','pct_obs',
						'br','woe','logit')]
		discret$predicted<-discret$br
	}
	## je�li jest to zmienna ci�g�a
	else{
		discret<-numeric_var_treatment(x,y, special_val=special_val,
				max_gleb=max_gleb,min_bucket=min_bucket,breaks=breaks,
				interactive=interactive, locfit=locfit, span=span, ...);
	}
	
	discret$label<-rownames(discret);
	return(discret);
}



#' Nie wiem co to robi
#' 
#' @param dane dane
#' @param mapowanie mapowanie 
#' @param czas czas
#' @param ...  dodatkowe parametry graficzne.
#' @return 
#' 
#' @author Piotr
#' @export
univariate_anal_stats4<-function(dane, mapowanie, czas=lastDay(dane$reportingdate, unit = "quater"),...){
	dyskretne<-mapuj(dane,mapowanie)
	wynik<-univariate_anal_stats(dyskretne, dane$def, czas,...)
	
	#funkcja univariate_anal_stats zmienia kolejno��, dlatego wracamy j�
	kolejnosc<-match(mapowanie$label,wynik$dyskretyzacja$label)
	#dodaj� totala
	kolejnosc<-c(kolejnosc,length(kolejnosc)+1)
	wynik$dyskretyzacja<-wynik$dyskretyzacja[kolejnosc,]
	wynik$rozklady$obs_all_tbl<-wynik$rozklady$obs_all_tbl[kolejnosc,]
	wynik$rozklady$pct_all_tbl<-wynik$rozklady$pct_all_tbl[kolejnosc,]
	wynik$rozklady$avg_t_tbl<-wynik$rozklady$avg_t_tbl[kolejnosc,]
	
	wynik$dyskretyzacja<-cbind(wynik$dyskretyzacja[,1:3], 
			mapping_war=c(as.character(mapowanie$war),""),
			wynik$dyskretyzacja[,4:length(wynik$dyskretyzacja)])
	wynik$dyskretyzacja$discret<-""
	wynik$dyskretyzacja$discret[nrow(wynik$dyskretyzacja)]="<TOTAL>"
	wynik
}


