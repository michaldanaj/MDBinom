# Z pliku "funkcje do analiz.r" przeniesione funkcje związane z budową drzewa
#
# Numeri_var_treatment i disretizetion to dwie alternatywne funkcje
# Author: Michał Danaj
###############################################################################


#TODO wyciągnąć parametry drzewa 
#TODO wyciągnąć parametry locfit
#' Dyskretyzuje zmienną ciagłą drzewkiem w oparciu o zmienną odpowiedzi
#'
#' W parametrze \code{x} ani \code{y} nie może być NULLi. Można je jakoś zakodować.
#' @param x zmienna ciągła.
#' @param y zmienna celu (PD, LGD).
#' @param NA_substit wartość specjalna wstawiona za brak danych.
#' @param special_val wartości specjalne, które powinny zostać uwzględnione jako
#'                   osobne klasy. Wartość taką jest również \code{NA}, automatycznie
#'                   uwzględniana jako osobna klasa.
#' @param min_bucket Minimalna wielkość liścia
#' @param max_gleb Maksymalna głębokośc do której budujemy drzewo
#' @param interactive TRUE, jeśli zmienna ma być dyskretyzowana interaktywnie. W
#'                   przeciwnym razie, co jest wartością domyślną, dyskretyzacja
#'                   jest automatyczna.
#' @param locfit Czy z automatu dopasować funkcję z modelu \code{locfit}.  
#' @param breaks Zamiast automatycznego dzielenia, można podać wartości przedziałów (from,to].
#' @param span Parametr wygładzający funkcji \code{locit}.
#' @param ... inne parametry do funkcji \code{\link{drzewo}}.
#' @seealso \code{drzewo}
#' @author Michał Danaj
#' @export
numeric_var_treatment<-function(x, y, 
		special_val=numeric_var_treatment.params$special_val,
		NA_substit=numeric_var_treatment.params$NA_substit,
		min_bucket=numeric_var_treatment.params$min_bucket, 
		max_gleb=numeric_var_treatment.params$max_gleb,
		interactive=FALSE, 
		locfit=FALSE, 
		breaks=NULL,
		span=0.9, ...){
	
	if (length(x)!=length(y))
		stop("discretization: parametry 'x' i 'y' mają różne długości!");
	
	#Mimo, że przygotowywyałem funkcję do obsługi null-i, to rezygnuję z tego
	#ze względów bezpieczeństwa.
	if (any(is.na(x)) | any(is.na(y)))
		stop ("discretization: W 'x' ani 'y' nie może być NA!");
	
	# Wartości specjalne
	special_val <- unique(c(special_val, NA_substit))
	special_idx<-is.na(x)|x %in% special_val;
	special_val<-unique(x[special_idx & !is.na(x)]);
	#czy są NA
	sa_na<-any(is.na(x));
	
	## Obsługuję wartości ciągłe
	#jeśli zostały podane zakresy przedziłów, to dzielimy wg nich
	if (!is.null(breaks)){
		bucket_drzewo<-buckety_stat2(breaks, x[!special_idx], y[!special_idx], total=FALSE);
	} else {
		if (locfit){
			bucket_drzewo<-try(reg_nieparam(x[!special_idx],y[!special_idx], span=span, wytnij=0.01), TRUE)
			
			#jeśli wyliczyło się z błędem, to próbuję jeszcze na dwa sposoby...
			if (class(bucket_drzewo)=="try-error"){
				bucket_drzewo<-try(reg_nieparam(x[!special_idx],y[!special_idx], span=span, wytnij=0.01, buckets=50), TRUE)
				if (class(bucket_drzewo)=="try-error"){
					bucket_drzewo<-try(reg_nieparam(x[!special_idx],y[!special_idx], span=span, wytnij=0.01, buckets=30), TRUE)
					
					#jeśli wciąż się nie powiodło, to zwracamy wartość błędu
					if (class(bucket_drzewo)=="try-error")
						return(bucket_drzewo)
				}	
			}
			
			#zmieniam nazwę, bo później ją wykorzystuję (historycznie)			
			bucket_drzewo$predicted=bucket_drzewo$fitted
		}
		else if (!interactive){
			bucket_drzewo<-drzewo(x[!special_idx],y[!special_idx], min_bucket=min_bucket, max_gleb=max_gleb, n_buckets=5, wytnij=0, ...)
			bucket_drzewo$predicted<-bucket_drzewo$br
		}else{
			bucket_drzewo<-interactive_tree(score=x[!special_idx],def=y[!special_idx],
					span=span, min_split=200, min_bucket=min_bucket,
					buckets=60, max_gleb=2)
			bucket_drzewo$predicted<-bucket_drzewo$br
		}
	}
	
	#przypisuję nr przedziałów dla wartości ciągłych
	bucket_drzewo$fitted<-bucket_drzewo$nr;
	classing<-rep(NA,length(x));
	classing[!special_idx]<-przypisz(x[!special_idx], bucket_drzewo);
	
	#nadaję indeksy wartościom specjalnym i je przypisuję
	special_map<- -(length(special_val):1);
	names(special_map)<-special_val;
	classing[special_idx]<-special_map[as.character(x[special_idx])];
	
	#i jeszcze NA
	classing[is.na(x)]<- 0;
	
	#liczę statystyki
	classing_stat<-buckety_stat(classing, y, total=TRUE);
	
	#zmieniam nazwy wierszy, żeby nie były numery a labele klas
	#mapping<-c(names(special_map), "<NA>", rownames(bucket_drzewo), 'TOTAL');
	mapping<-c(names(special_map), "<NA>", rownames(bucket_drzewo), 'TOTAL');
	names(mapping)<-c(special_map,"0",bucket_drzewo$nr, 'TOTAL');
	
	rownames(classing_stat)<-mapping[rownames(classing_stat)];
	
	#które wartości są specjalne (dyskretne)
	classing_stat$discret<-rep("",nrow(classing_stat));
	if (sa_na)
		classing_stat[c("<NA>", special_val),"discret"]<-rownames(classing_stat[c("<NA>", special_val),])
	else if (length(special_val)>0)
		classing_stat[as.character(special_val),"discret"]<-rownames(classing_stat[as.character(special_val),]);
	
	classing_stat$discret[nrow(classing_stat)]<-"<TOTAL>";
	
	classing_stat$do<-classing_stat$srodek<-classing_stat$od<-NA;
	classing_stat[classing_stat$discret=="",c("od","srodek","do")]<-bucket_drzewo[,c("od","srodek","do")];
	
	#dodaję predykcję. W przypadku drzewka był to br, w przypadku locfit był to wynik regressji
	classing_stat$predicted<-NA
	classing_stat[classing_stat$discret=="",]$predicted<-bucket_drzewo$predicted
	#dodaję predykcję do wartości specjalnych. Będzie to br
	classing_stat$predicted[classing_stat$discret!="" & classing_stat$discret!="<TOTAL>"]<-classing_stat$br[classing_stat$discret!="" & classing_stat$discret!="<TOTAL>"]
	
	classing_stat<-classing_stat[,c('nr','label','discret', 'od','srodek','do','n_good','pct_good','n_bad','pct_bad','n_obs','pct_obs',
					'br','predicted','woe','logit')];
	return(classing_stat);
}

#' Automatyczna dyskretyzacja w oparciu o algorytm drzewa
#' 
#' Dyskretyzuje zmienną \code{score}
#' @param score Zmienna do dyskretyzacji
#' @param def Zmienna celu
#' @param freq Liczność. Na razie nie obsługiwane
#' @param wytnij Jaką część końcowych wartości usunąć funkcją \code{usun_konce} 
#' @param min_split Minimalna wielkość węzła, aby móc dokonać podziału
#' @param min_bucket Minimalna wielkość liścia
#' @param max_gleb Maksymalna głębokośc do której budujemy drzewo
#' @param n_buckets Chyba nie używane?
#' @param plot Rysuje wynik dyskretyzacji 
#' @param testy Czy wyświetlać komentarze do testowania
#' @param ... Dodatkowe parametry do rysowania 
#' @return 
#' 
#' @author Michał Danaj
#' @export
drzewo<-function(score, def, freq=NULL, wytnij=0, min_split=30, min_bucket=10, max_gleb=4, n_buckets=20, plot=TRUE, testy=FALSE,...)
{
	
	#musze teraz wywolac buckety_br, bo pozniej agreguje wektory
	#do jednego scoru i wykorzystuje freq, a buckety_br musi miec
	#jeden rekord=jedna obserwacja
	#if (plot)
	#	b<-buckety_br(score , def, n_buckets, method="eq_lengt");
	if (testy==TRUE){
		print('=================   funkcja drzewo  ======================')
		print(length(score))
		print(length(def))
	}
	k<-order(score);
	score<-score[k];
	def<-def[k];
	
	if (is.null(freq))
		freq<-rep(1,length(score))
	else
	{
		stop("poki co nie obslugiwane freq inne od NULL");
		freq<-freq[k];
	}
	if (wytnij>0)
	{
		usun<-usun_konce(score, prob=wytnij);
		if (length(usun)>0){
			score<-score[-usun];
			def<-def[-usun];
			freq<-freq[-usun];
		}
	}
	
	if (testy==TRUE){
		print(length(def))
		print(length(freq))
		print(length(score))
	}
	
	def_a<-tapply(def,score,sum);
	freq_a<-tapply(freq, score, sum);
	score_a<-unique(score);
	
	if (testy==TRUE){
		print('---')
		
		print(length(def_a))
		print(length(freq_a))
		print(length(score_a))
	}
	
	#Zabezpieczam się przed zminiejszeniem precyzji liczb w wektorze score podczas konwersji
	#na zmienną znakową, wykonywaną podczas wykonania funkcji tapply
	if (length(def_a)!=length(score_a)){
		score_a<-as.numeric(names(def_a))
		warning("W funkcji 'drzewo' wykonywana jest konwersja score na ciąg znaków, która
						zmniejszyła precyzję. Dalsze działanie funkcji przebiega w oparciu o zmniejszoną
						precyzję liczb. Zaleca się zmniejszenie precyzji danych wejściowych.")
		
	}
	
	
	#vec_stats(score_a);
	w<-drzewo_podzial(score_a, def_a, 1, min(score), max(score), freq_a, 0, min_split, min_bucket, max_gleb);
	
	#wybieram liscie
	w<-w[is.na(w$poprawa),];
	w<-w[order(w$od),];
	
	#i robie dla nich statystyki
	breaks<-sort(unique(c(w$od, w$do)));
	
	# jeśli jest tylko jeden liść
	if (length(breaks)==1)
		bucket<-buckety_stat(score, def, total=FALSE)
	else
		bucket<-buckety_stat(cut(score, breaks, include.lowest=TRUE), def, total=FALSE);
	
	#uzupełniam statystyki
	bucket$fitted<-bucket$br;
	
	bucket$od<-w$od;
	bucket$do<-w$do;
	bucket$srodek<-(w$od+w$do)/2;
	#	----- rysowanie -----
	if (plot)
	{
		
		plot(bucket$srodek, bucket$br, xlab="", ylab="", xlim=c(range(breaks)),...);
		for (i in 1:length(w$od))
			lines(c(bucket$od[i], bucket$do[i]), c(bucket$fitted[i],bucket$fitted[i]), col="blue");
	}
	
	if (testy==TRUE){
		print('+++++++++++++++++++   koniec funkcja drzewo  +++++++++++++++++++++')
	}
	
	bucket
}



#' Rysuje dyskretyzację w oparciu o drzewko
#' 
#' @param liscie_drzewa Cholera wie, co jest i jak mam to tu przekazać.
#' @param ... Dodatkowe parametry graficzne.
#' @return 
#' 
#' @author Michał Danaj
#' @export
drzewo_plot<-function(liscie_drzewa,...){
	#liscie<-liscie_drzewa[liscie_drzewa$discret=="",];
	liscie<-liscie_drzewa[!is.na(liscie_drzewa$od),];
	
	# Jeśli są jakieś liście ciągłe
	if (nrow(liscie)>1){
		# Wartości nieskończone środka zamieniam krańcowymi
		liscie$srodek[liscie$srodek==-Inf]<-min(liscie$do)
		liscie$srodek[liscie$srodek==Inf]<-max(liscie$od)
		
		breaks<-sort(unique(c(liscie$od,liscie$do)));
		#usuwam nieskończoności
		niesk<-which(is.infinite(breaks))
		if (length(niesk)>0)
			breaks<-breaks[-niesk]
		
		skala <- sqrt(liscie$n_obs/(sum(liscie$n_obs)/nrow(liscie)));
		plot(liscie$srodek, liscie$br, xlim=c(range(breaks)),cex=skala,...);
		for (i in 1:length(liscie$od))
			lines(c(liscie$od[i], liscie$do[i]), c(liscie$br[i],liscie$br[i]), col="blue");
	}
	# W przeciwnym razie
	else{
		
		#usuwam totala
		liscie<-liscie_drzewa[!(is.na(liscie_drzewa$discret) | as.character(liscie_drzewa$discret)=='<TOTAL>'),];
		
		#jeśli coś tam jest
		if(nrow(liscie)>0){
			
			skala <- sqrt(liscie$n_obs/(sum(liscie$n_obs)/nrow(liscie)));
			
			plot(liscie$br, cex=skala, axes=FALSE,...);
			axis(1, at=1:nrow(liscie), labels=liscie$discret);
			axis(2);
			box();
		}
		else{
			#rysuję pusty wykres
			plot(0,pch='x');
		}
	}
}


#TODO dorobić dokumentację.
#' Podział drzewa
#' 
#' Podział drzewa.
#' @param score score
#' @param def def
#' @param nr_wezla nr_wezla 
#' @param od od
#' @param do do
#' @param freq freq
#' @param glebokosc glebokosc
#' @param min_split min_split
#' @param min_bucket min_bucket
#' @param max_gleb max_gleb
#' @param testy  testy
#' @return 
#' 
#' @author Michał Danaj
drzewo_podzial<-function(score, def, nr_wezla, od, do, freq, glebokosc,
		min_split=200, min_bucket=100, max_gleb=3, testy=FALSE)
{
	if (testy==TRUE){
		print("===============   funkcja drzewo_podzial   ====================");
		print(nr_wezla);
		print(length(score))
		print(length(def))
	}
	all_obs<-sum(freq);
	
	if (testy==TRUE){
		print(table(freq,useNA='always'))
		print(all_obs)
	}
	
	all_bad<-sum(def);
	br_akt<-all_bad/all_obs;
	gini_akt<-br_akt*(1-br_akt)*all_obs;
	wezel<-data.frame(nr_wezla, rodzic=floor(nr_wezla/2), od, do, n_obs=all_obs, n_bad=all_bad, br=br_akt, poprawa=NA, podzial=NA);
	
	wynik<-wezel;
	
	#jesli ilosc obserwacji jest wystarczajaca, aby zrobic podzial w wezle
	#i jeszcze możemy dorobić liście
	if (all_obs>min_split)
	{
		cum_bad_lewo<-cumsum(def);
		cum_obs_lewo<-cumsum(freq);
		
		cum_bad_prawo<-(all_bad-cum_bad_lewo);
		cum_obs_prawo<-(all_obs-cum_obs_lewo);
		
		br_lewo<-cum_bad_lewo/cum_obs_lewo;
		br_prawo<-cum_bad_prawo/cum_obs_prawo;
		
		gini_lewo<-br_lewo*(1-br_lewo)*cum_obs_lewo;
		gini_prawo<-br_prawo*(1-br_prawo)*cum_obs_prawo;
		
		gini_roz<-gini_akt-(gini_prawo+gini_lewo);
		#print("gini");print(gini_akt);print(gini_prawo);print(gini_lewo)
		
		#zostawiam podzialy, dla ktorych spelnione sa wymogi na ilosc
		#obserwacji w wynikowych lisciach
		zostaw<-(cum_obs_lewo>min_bucket)&(cum_obs_prawo>min_bucket);
		gini_roz[!zostaw]<-NA;
		
		#nr podzialu maksymalizujacego roznice gini
		nr<-which.max(gini_roz);
		if (length(nr)>0 & glebokosc<max_gleb)
		{
			wezel$poprawa<-gini_roz[nr];
			wezel$podzial<-(score[nr]+score[nr+1])/2;
			l<-length(score);
			
			if (testy==TRUE){
				print ('testy podzialu')
				print(length(score)) 
				print(length(def))
				print('length(freq):')
				print(length(freq))
				print('nr:') 
				print(nr)
				print('nic')
			}
			wl<-drzewo_podzial(score[1:nr], def[1:nr], nr_wezla*2, od, wezel$podzial, freq[1:nr], glebokosc+1,
					min_split, min_bucket, max_gleb);
			wr<-drzewo_podzial(score[(nr+1):l], def[(nr+1):l], nr_wezla*2+1, wezel$podzial, do, freq[(nr+1):l], glebokosc+1,
					min_split, min_bucket, max_gleb);
			wynik<-rbind(wezel,wl,wr);
		}
	}
	
	if (testy==TRUE){
		print("===============   koniec funkcja drzewo_podzial   ====================");
	}
	wynik
}



#' Interaktywny podział zmiennej ciągłej na buckety
#'
#' Interaktywny podział zmiennej ciągłej na buckety. Uwaga! W danych
#' nie może być wartości NULL. Nieopisane zmienne są to zmienne z
#' wykorzystywanej funkcji \code{\link{drzewo}} oraz \code{\link{reg_nieparam}}.
#' Po wykonaniu zmiany wyświetlane są statystyki po bucketach oraz statystyka AR.
#' @param score zmienna score'owa.
#' @param def zmienna odpowiedzi z zakresu [0,1]. Np. default, LGD.
#' @param span Parametr wygładzający funkcji \code{locit}.
#' @param min_split Minimalna wielkość węzła, aby móc dokonać podziału
#' @param min_bucket Minimalna wielkość liścia
#' @param buckets Chyba nie używane?
#' @param max_gleb Maksymalna głębokośc do której budujemy drzewo
#' @seealso \code{\link{drzewo}}, \code{\link{AR_quick}},  \code{\link{buckety_stat2}}.
#' @return \code{data.frame} ze statystykami.
#' @author Michał Danaj
#' @export 
interactive_tree<-function(score, def, span=0.80, min_split=200, min_bucket=100,
		buckets=60, max_gleb=2)
{
	
	if(any(is.na(score)|is.na(def)))
		stop('interactive_tree: Niedozwolone wartości NULL!')
	#wylicza pozycję punktów symulujących menu i je rysuje
	punkty_menu<-function(){
		
		minx<-min(axis(1))
		maxx<-max(axis(1))
		
		miny<-min(axis(2))
		maxy<-max(axis(2))
		
		deltax=(maxx-minx)/20;
		deltay=(maxy-miny)/20;
		
		x_fun=c(maxx-deltax,maxx);
		y_fun=c(maxy,maxy);
		
		points(x_fun,y_fun, col=c("green","red"), pch=19);
		
		return(identify(x_fun,y_fun,n=1));
	}
	
	#posortuj wg score
	kolej<-order(score);
	score<-score[kolej];
	def<-def[kolej];
	
	#par(mfrow=c(1,2), xpd=F);
	reg<-reg_nieparam(score=score , default=def, buckets=buckets, wytnij=0.01, span=span, degree=2);
	drz<-drzewo(score, def, wytnij=0.01, min_split=min_split, min_bucket=min_bucket, max_gleb=max_gleb);
	reg<-reg_nieparam(score , def, buckets=buckets, wytnij=0.01, span=span, degree=2);
	for (i in 1:length(drz$od))
		lines(c(drz$od[i], drz$do[i]), c(drz$br[i],drz$br[i]), col="blue");
	
	drz_pocz<-drz;
	
	#par(xpd=NA);
	#bar<-barplot(drz$br);
	#text(bar,drz$br,paste(drz$n_default, "\n", drz$n_obs, "\n", round(drz$br*100,1)), cex=0.8, adj=c(1);
	drz<-buckety_stat2(c(drz$od,drz$do), score, def, total=FALSE);
	points(drz$median, drz$br, col="red",pch="X");
	#points(drz$srodek, drz$br, col="red",pch="X");
	#points(drz$n_bad/drz$n_obs, drz$br, col="green",pch="I");
	
	print(drz[,c('od', 'do','n_bad','n_obs','pct_obs','br','woe')]);
	kolej<-order(-drz$br);
	print(paste('AR =',with(drz, AR_quick(n_bad[kolej], n_obs[kolej]))));
	
	nr<-punkty_menu();
	
	#print(data.frame(drz$od, drz$do, drz$n_default,drz$n_obs, drz$br));
	
	while (length(nr)>0)
	{
		#polacz
		if (nr==1)
		{
			#text(x_fun[1]-deltax, y_fun[1]-deltay,"Wybierz dwa przedzialy",cex=0.7);
#  		id<-identify(drz$srodek,drz$br,n=2, pos=1, col="black")$ind;
			id<-identify(drz$median,drz$br,n=2, pos=1, col="black")$ind;
			#print(id);
			if (length(id)==2)
			{
				id<-id[order(id)];
				drz[id[1],"do"]<-drz[id[2],"do"];
				drz_old<-drz;
				drz<-buckety_stat2(c(drz$od[-id[2]], drz$do[-id[2]]), score, def, total=FALSE);
				drz$od<-drz_old$od[-id[2]];
				drz$do<-drz_old$do[-id[2]];
				#drz<-buckety_stat(score, def, drz$od[-id[2]], drz$do[-id[2]]);
				#print(data.frame(drz$od, drz$do, drz$n_default,drz$n_obs, drz$br));
				drz$fitted<-drz$br;
			}
		}
		#rozlacz
		if (nr==2)
		{
			#text(x_fun[1]-deltax, y_fun[1]-deltay,"Wybierz jeden przedzial",cex=0.7);
			id<-identify(drz$median,drz$br,n=1, pos=1, col="black")$ind;
			
			if (length(id)==1)
			{
				wybr<-score>drz[id,"od"] & score<=drz[id,"do"];
				
				def_a<-tapply(def[wybr],score[wybr],sum);
				freq_a<-tapply(rep(1,length(score[wybr])), score[wybr], sum);
				score_a<-unique(score[wybr]);
				#vec_stats(score_a);
				
				wl<-drzewo_podzial(score_a, def_a, 1, drz[id,"od"], drz[id,"do"], freq_a, 0, min_split, min_bucket, 1);
				wl<-wl[is.na(wl$podzial),];
				
				drz<-drz[-id,];
				od<-c(drz$od,wl$od);
				od<-od[order(od)];
				do<-c(drz$do, wl$do);
				do<-do[order(do)];
				#i robie dla nich statystyki
				breaks<-sort(unique(c(od, do)));
				
				# jeśli jest tylko jeden liść
				if (length(breaks)==1)
				{drz<-buckety_stat2(c(od, do), score, def, total=FALSE);
				}else
					drz<-buckety_stat2(c(od, do), score, def, total=FALSE);
				
				drz$fitted<-drz$br;
				
				#print(data.frame(drz$od, drz$do, drz$n_default,drz$n_obs, drz$br));
			}
		}
		reg_nieparam(score , def, buckets=buckets, wytnij=0.01, span=span, degree=2);
		for (i in 1:length(drz$od))
			lines(c(drz$od[i], drz$do[i]), c(drz$br[i],drz$br[i]), col="blue");
		
		points(drz$median, drz$br, col="red",pch="X");
		points(drz$mean, drz$br, col="green",pch="I");
		
		print(drz[,c('od', 'do','n_bad','n_obs','pct_obs','br','woe')]);
		kolej<-order(-drz$br);
		print(paste('AR =',with(drz, AR_quick(n_bad[kolej], n_obs[kolej]))));		
		nr<-punkty_menu();
	}
	
	return(drz);
}

