
#' Ró¿ne dzia³ania na zmiennych dwumianowych.
#'
#' \tabular{ll}{
#' Package: \tab MDBinom\cr
#' Type: \tab Package\cr
#' Version: \tab 4.4\cr
#' Date: \tab 2012-08-01\cr
#' License: \tab GPL (>= 2)\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' Ró¿ne dzia³ania na zmiennych dwumianowych.
#'
#' @name MDBinom-package	
#' @aliases MDBinom
#' @docType package
#' @title Ró¿ne dzia³ania na zmiennych dwumianowych
#' @author Micha³ Danaj \email{michal.danaj@@gmail.com}
#' @keywords package


#roxygen();
NULL;
#library(roxygen2);

#' Liczy logit
#'
#' Liczy logit
#' @param p wartoœæ.
#' @author Micha³ Danaj
#' @export
logit<-function(p)
	return(log(p/(1-p)));

#' Podstawia liczbê za brak danych
#' 
#' @param x objekt zmiennych numerycznych
#' @param val wartoœæ, któr¹ ma zostaæ zast¹piony brak danych \code{NA}
#' @author Micha³ Danaj
#' @return Zwraca pierwotny obiekt z wartoœci¹ \code{val} w miejsce braku danych
#' @export
nvl<-function(x, val){
	x[is.na(x)]<-val;
	x
}

#' Zwraca jedn¹ z dwóch wartoœci w zale¿noœci czy by³ brak danych
#' 
#' @param x objekt zmiennych numerycznych
#' @param val_not_null wartoœæ, która ma zostaæ zwrócona w przypadku gdy nie ma braku danych
#' @param val_null wartoœæ, która ma zostaæ zwrócona w przypadku braku danych
#' @author Micha³ Danaj
#' @return Zwraca pierwotny obiekt z oryginalnymi wartoœciami zast¹pionymi przez \code{val_not_null} i \code{val_null} 
#' @export
nvl2<-function(x, val_not_null, val_null){
	x[!is.na(x)]<-val_not_null;
	x[is.na(x)]<-val_null;
	x
}
#' Wylicza rozk³ad AR(GINI) metod¹ bootstrap
#'
#' Algorytm dzia³a w nastêpuj¹cy sposób. Na wstêpie agregowane s¹ wartoœci
#' do poziomu score. Tak przygotowane informacje pos³u¿¹
#' do wygenerowania losowo liczby badów i goodów dla ka¿dego score, jakie zosta³yby wylosowane
#' w eksperymencie losowania ze zwracaniem z wyjœciowej próby. 
#' Jednorazowo losowanych jest tyle prób bootstrap, aby liczba wszystkich wylosowanych 
#' obserwacji
#' nie przekroczy³a \code{n_once}. Ma to zapobiec przekroczeniu dostêpnej wielkoœci pamiêci.
#' Z wylosowanych obserwacji tworzone s¹ macierze
#' do funkcji \code{\link{AR_quick}}, gdzie jedna kolumna odpowiada jedemu eksperymentowi. 
#' Jeœli liczba kolumn w macierzy jest mniejsza ni¿ \code{n_boot}, co mo¿e byæ
#' spowodowane ograniczeniem przez \code{n_once}, wykonywana jest
#' kolejna pêtla. Algorytm zosta³ zoptymalizowany pod k¹tem prób, w których
#' wystêpuje ma³a liczba unikalnych wartoœci \code{score}, w stosunku do liczby obserwacji w próbie.
#'
#' Dane wejœciowe mog¹ byæ ju¿ w jakiœ sposób zagregowane (np.do poziomu score), lub nie.
#'  
#' @param score wektor z wartoœciami score.
#' @param def wektor z liczb¹ defaultów dla danego rekordu danych.
#' @param n_boot liczba prób bootstrap do wygenerowania.
#' @param obs wektor z liczb¹ obserwacji dla danego rekordu danych.
#' @param n_once ile jednorazowo maksymalnie mo¿e byæ wylosowanych obserwacji. Parametr dodany jedynie, 
#' 		  aby nie przekroczyæ dostêpnej pamiêci. Nie ma wp³ywu na koñcowy wynik.
#' @param seed ziarno. 
#' @return Zwraca wektor d³ugoœci \code{b_boot+1}. Pierwszy element wektora
#'				 zawiera wyliczony AR na ca³ej próbie, w dalszej kolejnoœci znajduj¹
#'				 siê wartoœci wyliczone w oparciu o próby bootstrap.
#' @author Micha³ Danaj
#' @export
#' @examples
#' ## dane do przyk³adu
#' n<-100000; #liczba obserwacji w próbie
#' nb<-100; #liczba prób bootstrap
#' score<-rnorm(n); #generowanie score
#' def<-as.numeric(score+rnorm(n)<0) #generowanie defaultów
#' score<-floor(200*score) #dyskretyzacja wartoœci score
#' 
#' #liczba unikalnych wartoœci score
#' length(unique(score))
#' #[1] 1362
#' 
#' #czas wykonania
#' system.time(AR_boot(score, def, nb))
#' #user  system elapsed 
#' # .41    0.00    1.53 
#' 
#' #porównanie z czasem wykonania w przypadku pêtli i stadradowej funkcji 
#' #do wyliczania GINI
#' system.time(
#'	sapply(1:nb, function(i){
#' 			id<-sample(length(score), replace=TRUE);
#' 			AR(score[id], def[id])[[1]]['AR']
#' 	})
#' ) 
#' # user  system elapsed 
#' #83.66    1.62   92.44 
AR_boot<-function (score, def, n_boot, obs=rep(1,length(score)), n_once = 50000000, seed=NULL) 
{
	if (is.null(score)) 
		stop("AR_boot: Zmienna 'score' musi byæ ró¿na od NULL!")
	if (is.null(def)) 
		stop("AR_boot: Zmienna 'def' musi byæ ró¿na od NULL!")
	if ((min(def) < 0 || max(def) > 1) & max(obs)==1) 
		stop("AR_boot: Zmienna 'def' musi byæ z zakresu [0,1].")
	if (is.null(n_boot))
		stop("AR_boot: Zmienna 'n_boot' musi byæ ró¿na od NULL!")
	bad <- tapply(def, score, sum)
	obs <- tapply(obs, score, sum)
	good <- obs - bad
	if (length(obs) > n_once) 
		stop("AR_boot: Liczba jednorazowo wylosowanych elementów n_once musi byæ wiêksza, ni¿ \n\t\t\t\t\tliczba unikalnych wartoœci score!")
	prob <- c(bad, good)/length(score)
	ile_kolumn <- floor(n_once/length(obs))
	N <- Hmisc::ceil(n_boot/ile_kolumn)
	wynik <- AR_quick(bad, obs)
	
	
	
	#jeœli u¿ytkownik chce ustawiæ see;
	if (!is.null(seed))
	{
		#zapisujê aktualnie u¿ywany seed;
		old.seed<-.Random.seed;
		set.seed(seed);
	}
	
	for (i in 1:N) {
		n_kol <- min(ile_kolumn, n_boot)
		los <- rmultinom(n_kol, size = sum(obs), prob = prob)
		wynik <- c(wynik, AR_quick(los[1:(nrow(los)/2), ], los[1:(nrow(los)/2), 
						] + los[-c(1:(nrow(los)/2)), ]))
		n_boot <- n_boot - n_kol
	}
	
	#wracam z oryginalnym seedem
	if (!is.null(seed))		
		set.seed(old.seed);
	return(wynik)
}

#' Liczy AR(GINI) dla specyficznie przygotowanych danych
#'
#' Funkcja zak³ada, ¿e podane licznoœci badów i wszystich obserwacji
#' posortowane s¹ wg rosn¹cych wartoœci score. W przypadku gdy w próbie s¹
#' tylko bady lub tylko goody, funkcja zwraca \code{NaN}.
#' @param bad liczba badów odpowiadaj¹ca kolejnym wartoœciom score.
#' @param obs liczba obserwacji odpowiadaj¹ca kolejnym wartoœciom score.
#' @author Micha³ Danaj
#' @export
AR_quick<-function(bad, obs)
{

	if ( !is.matrix(bad) || !is.matrix(obs)){
		bad<-as.matrix(bad);
		obs<-as.matrix(obs);
	}
	
	if (any(dim(bad)!=dim(obs)))
		stop('Ró¿ne wymiary parametrów wejœciowych!');
		
	def_all<-colSums(bad);
	obs_all<-colSums(obs);
	good_all<-obs_all-def_all;

	#usuwam score'y, gdzie nie ma obserwacji ???
	#bad<-bad[obs!=0]
	#obs<-obs[obs!=0]
		
	pct_all <- sweep(obs, 2, colSums(obs), FUN='/');
	pct_bad <- sweep(bad, 2, colSums(bad), FUN='/');
	pct_good <-sweep(obs-bad, 2, colSums(obs-bad), FUN='/');
		
	cum_pct_obs<-apply(pct_all, 2, cumsum);
	cum_pct_bad<-apply(pct_bad, 2, cumsum);
	cum_pct_good<-apply(pct_good, 2, cumsum);
										 
	#biorê poprzedni element  
	cum_pct_bad_prev<-apply(cum_pct_bad, 2, FUN=function(x)c(0,x[2:length(x)-1]));
	cum_pct_obs_prev<-apply(cum_pct_obs, 2, FUN=function(x)c(0,x[2:length(x)-1]));

	#licze kawalki powierzchni
	AUC_part<-(cum_pct_obs-cum_pct_obs_prev)*(cum_pct_bad+cum_pct_bad_prev)/2;

	#sumuje cala powierzchnie
	AUC<-colSums(AUC_part);
	
	#uwzgledniam bad rate w probce
	return ( (2*AUC-1)/(1-def_all/obs_all)) ;
}	
#TODO zaktualizowaæ dokkumentacjê!
#' Liczy Accuracy Ratio (GINI)
#'
#' Wylicza Accuracy Ratio (GINI). Wynikiem jest \code{data.frame} przechowuj¹cy
#' informacje pozwalaj¹ce na wyrysowanie krzywej CAP. W ostatniej kolumnie zapiasna
#' jest wartoœæ AR.
#'
#' @param score wektor	wartoœci, wed³ug który nale¿y posortowaæ wartoœci \code{def}
#' @param def Wektor wartoœci \code{\{0,1\}} 
#' @param plot Czy narysowaæ krzyw¹ CAP. Domyœlnie \code{FALSE}. 
#' @param return.table czy zwracaæ tabelê z agregatami na poziomie pojedynczej 
#'         wartoœci score
#' @param sort.order dla wartoœci równej "br", zamiast po \code{score}, 
#'				 sortuje po wartoœci \code{br}. Mo¿e to byæ przydatne przy zmiennych dyskretnych.
#' @param label opis wyœwietlony w legendzie.
#' @param lgd_adjusted W przypadku modeli LGD pole pod krzywymi CAP i ROC dla modelu idealnego jest
#' 			mniejsze ni¿ dla modelu PD i nie da siê go wyliczyæ analogicznym wzorem. Dlatego
#' dla LGD wyliczana jest osobno powierzchnia dla modelu idealnego i jest ona u¿yta jako mianownik do
#' wyliczenia AR. 
#' @param ... dodatkowe parametry.
#'@return Zwraca listê obiektów \code{data.frame}. Elementy tej listy odpowiadaj¹ kolejnym
#'kolumnom ze \code{score_wiele} i maj¹ takie same nazwy, jak kolumny ze \code{score_wiele}.
#'Ka¿dy element listy ma postaæ:
#'  \item{do }{Górna granica przedzia³u, dla którego podane s¹ statystyki.}
#'  \item{bad}{Liczba bad-ów w przedziale.}
#'  \item{obs}{Liczba obserwacji w przedziale.}
#'  \item{pct_all}{Procentowy udzia³ obserwacji w danym przedziale.}
#'  \item{pct_bad}{Procentowy udzia³ bad-ów w danym przedziale w stosunku do 
#'									wszystkich bad-ów.}
#'  \item{pct_good}{Procentowy udzia³ good-ów w danym przedziale w stosunku do 
#'									wszystkich good-ów.}
#'  \item{br}{Stosunek bad-ów do good-ów w danym przedziale (bad rate).}
#'  \item{AR}{Wyliczone Accuracy Ratio.}							
#'
#' @author Micha³ Danaj
#' @export
#' @examples
#'	n<-1000;
#'	x<-rnorm(n);
#'	def<- as.numeric(x+rnorm(n)<0);
#'	AR(x,def,plot=TRUE);
 
AR<-function(score, def, plot=FALSE, return.table=FALSE, 
									sort.order=NULL, label="", lgd_adjusted=FALSE,...)
{
	
	if (is.null(score))
		stop("Zmienna 'score' musi byæ ró¿na od NULL!");
	if (is.null(def))
		stop("Zmienna 'def' musi byæ ró¿na od NULL!");

	#sprawdzam, czy def jest z zakresu [0,1]
	if (min(def)<0 || max(def)>1) 
		stop("Zmienna 'def' musi byæ z zakresu [0,1].");
		
	sort.order<-match.arg(sort.order);

	def_all<-sum(def);
	obs_all<-length(def);
	good_all<-obs_all-def_all;


	if (plot){
		plot(c(0,1), c(0,1), type="l", lty=1, col=1, xlab="", ylab="",...);
		lines(c(0,def_all/obs_all,1),c(0,1,1), col=1);
	}

		bad<-tapply(def,score,sum);
		obs<-as.vector(table(score));	

		#usuwam score'y, gdzie nie ma obserwacji
		bad<-bad[obs!=0]
		obs<-obs[obs!=0]

		br=bad/obs;
		
		if (!is.null(sort.order) && sort.order=="br")
		{
			k<-order(br, decreasing =TRUE);
			bad<-bad[k];
			obs<-obs[k];
			br<-br[k];
		}
		
		pct_all<-obs/obs_all;
		pct_bad<-bad/def_all;
		pct_good<-(obs-bad)/good_all;
		
		
		cum_pct_obs<-cumsum(pct_all);
		cum_pct_bad<-cumsum(pct_bad);
		cum_pct_good<-cumsum(pct_good);
										 
		#biorê poprzedni element
		cum_pct_bad_prev<-c(0,cum_pct_bad[2:length(cum_pct_bad)-1])
		cum_pct_obs_prev<-c(0,cum_pct_obs[2:length(cum_pct_obs)-1])
		
		#licze kawalki powierzchni
		AUC_part<-(cum_pct_obs-cum_pct_obs_prev)*(cum_pct_bad+cum_pct_bad_prev)/2;

		#sumuje cala powierzchnie
		AUC<-sum(AUC_part);
		
		#wyliczam AUC dla perfekcyjnego modelu LGD
		if (lgd_adjusted){
			bad_lgd<-tapply(def,-def,sum);
			obs_lgd<-as.vector(table(-def));
			
			bad_lgd<-bad_lgd[obs_lgd!=0]
			obs_lgd<-obs_lgd[obs_lgd!=0]
			
			pct_all_lgd<-obs_lgd/obs_all;
			pct_bad_lgd<-bad_lgd/def_all;
			
			cum_pct_obs_lgd<-cumsum(pct_all_lgd);
			cum_pct_bad_lgd<-cumsum(pct_bad_lgd);
			
			#biorê poprzedni element
			cum_pct_bad_prev_lgd<-c(0,cum_pct_bad_lgd[2:length(cum_pct_bad_lgd)-1])
			cum_pct_obs_prev_lgd<-c(0,cum_pct_obs_lgd[2:length(cum_pct_obs_lgd)-1])

			#licze kawalki powierzchni
			AUC_part_lgd<-(cum_pct_obs_lgd-cum_pct_obs_prev_lgd)*(cum_pct_bad_lgd+cum_pct_bad_prev_lgd)/2;
			
			#sumuje cala powierzchnie
			AUC_lgd<-sum(AUC_part_lgd);
		}
		
		#uwzgledniam bad rate w probce
		#ARt[i]<-(2*AUC-1)/(1-def_all/obs_all);
		ARt<-(2*AUC-1)/(1-def_all/obs_all);
		
		AR_lgd_perfect<-NULL;
		AR_lgd_adjusted<-NULL;
		if (lgd_adjusted){
			AR_lgd_perfect<-(2*AUC_lgd-1)/(1-def_all/obs_all);
			AR_lgd_adjusted<-(2*AUC-1)/(2*AUC_lgd-1);
		} 
		
		KS<-max(abs(cum_pct_bad-cum_pct_good));
		IV<-sum((pct_good-pct_bad)*log(pct_good/pct_bad));
		
		if (return.table==TRUE)
			wynik<-list(table=data.frame(bad, obs,pct_all, 
						 pct_bad, pct_good, cum_pct_obs, cum_pct_bad, cum_pct_good,
						 br), stats=c(bad=def_all, good=good_all, obs=obs_all, br=def_all/obs_all, AR=ARt, KS=KS, IV=IV,
						 AR_lgd_perfect=AR_lgd_perfect, AR_lgd_adjusted=AR_lgd_adjusted),
	 					 label=label)
	 	else
			wynik<-list(stats=c(bad=def_all, good=good_all, obs=obs_all, br=def_all/obs_all, AR=ARt, KS=KS, IV=IV,
							AR_lgd_perfect=AR_lgd_perfect, AR_lgd_adjusted=AR_lgd_adjusted),
							label=label);
		if (lgd_adjusted==TRUE && return.table==TRUE)
			wynik[['perfect_lgd_table']]<-data.frame(cum_pct_obs_lgd=cum_pct_obs_lgd, cum_pct_bad_lgd=cum_pct_bad_lgd);

		#names(wynik)[match("nowy",names(wynik))]=names(score)[i];
		if (plot)
			lines(c(0,cum_pct_obs), c(0,cum_pct_bad), type="l", lty=1);
	#}
	#kolej<-order(ARt);
	ARt=round(ARt*100,2);
	if (plot)
		#legend(1,0,paste(names(score)[kolej], " (", ARt[kolej], "%)", sep="" ),lty=1, 
		#	 col=kolej+1, cex=0.8, xjust=1, yjust=0 );
		 legend(1,0,paste(label, " (", ARt, "%)", sep="" ),lty=1, 
			 cex=0.8, xjust=1, yjust=0 );

	class(wynik)<-"AR";
	return(wynik);
}

#' Wyœwietla statystyki przechowywane w obiekcie \code{\link{AR}}
#' 
#' Wyœwietla statystyki przechowywane w obiekcie \code{\link{AR}}
#' @param x obiekt klasy \code{AR}.
#' @method print AR
#' @export
print.AR<-function(x){
	print(x$label);
	print(x$stat);
	if (!is.null(x$table))
		print(x$table);
}

#' Wyœwietla statystyki przechowywane w obiekcie \code{\link{AR}}
#' 
#' Wyœwietla statystyki przechowywane w obiekcie \code{\link{AR}}
#' @param x obiekt klasy \code{AR}.
#' @method HTML AR
HTML.AR<-function(x){
	R2HTML::HTML(x$label);
	R2HTML::HTML(x$stat);
	if (!is.null(x$table))
		R2HTML::HTML(x$table);	
}

#' Zwraca statystyki przechowywane w obiekcie \code{\link{AR}}
#' 
#' Zwraca statystyki przechowywane w obiekcie \code{\link{AR}}
#' @param x obiekt klasy \code{AR}.
#' @title Zwraca statystyki przechowywane w obiekcie AR
#' @export
getStats.AR<-function(x){
 s<-t(as.data.frame(x$stats))
 cbind(data.frame(label=x$label),s);
}



#' Przekszta³ca obiekt na \code{\link{data.frame}}
#' 
#' Przekszta³ca obiekt na \code{\link{data.frame}}
#' @param x obiekt klasy \code{AR}.
#' @title Przekszta³ca obiekt na data.frame 
#' @method as.data.frame AR
#' @export   
as.data.frame.AR<-function(x){
 s<-t(as.data.frame(x$stats))
 cbind(data.frame(label=x$label),s);	
}




#' Rysuje informacje o danych
#'
#'  Rysuje histogram zmiennej \code{score}, dla ka¿dego przedzia³u histogramu
#'  rysuje bad rate, oraz rysujê krzyw¹ regresji dla \code{default~score}. Pod
#'  wykresem wyœwietla informacje o Accuracy Ratios i bad rate.
#' @param score Wektor ze zmienn¹ numeryczn¹. 
#' @param default Wektor ze zmienn¹ dwumianow¹. 
#' @param buckets Sugerowana liczba bucketów. 
#' @param span Wspó³czynnik wyg³adzaj¹cy, wykorzystywany przez funkcjê \code{\link{locfit}}
#' @param main Tytu³ wykresu. 
#' @param hist_col Kolor histogramu  
#' @param ylab Label
#' @param xlab Label
#' @param ... dodatkowe parametry.
#' @author Micha³ Danaj
#' @export
#' @examples
#' x<-rnorm(100);
#' y<-(x+rnorm(100))>0;
#' informacje(x, y, buckets=15, span=0.8, main="", hist_col="blue", type="br", method_bucket="eq_length")
informacje<-function(score, default, buckets=20, span=0.8, main="", hist_col="blue",
 #method_bucket="eq_length",
				ylab=c("Frequency", "BR"),xlab="Calibrated score", ...)
{	
	def_par=par(no.readonly=TRUE);
	start_par<-par();

	par(mar=c(5,4,4,5)); # Leave space for z axis
	
	h<-hist(score, plot=FALSE);
#	w<-wygladz(score, default, buckets=buckets, span=span, type=type,  plot=FALSE, method_bucket=method_bucket);
	if (length(unique(default))==2)
		local<- locfit::locfit(default ~ locfit::lp(score, nn=span), family="binomial", link="logit")
	else
		local<- locfit::locfit(default ~ locfit::lp(score, nn=span)) 
	br2<-predict(local,newdata=h$mid, type="response");                                                                   
	cut_<-cut(score, breaks=h$breaks);
	br<-tapply(default, cut_, mean);
	XRange <- range( c(h[["break"]], h$mid) );
	print(XRange)
	plot(h, col=hist_col ,axes=FALSE, ann=FALSE, ylab="", main=main, xlim=XRange, xlab="faafa");
	print(paste("----",par(no.readonly=TRUE)$usr))
	# Now draw x axis 
	par(new=TRUE);
	axis(1, col = "black") 

	# Now draw y axis 
	par(new=TRUE);
	axis(2, col = hist_col) 

	# Now draw y axis label
	par(new=TRUE);
	mtext(side = 2, text = ylab[1], line = 3, col="black") 

	# Use axTicks() to get default tick locations
	#par(new=TRUE);
	#at = axTicks(2)
	#mtext(side = 2, text = at, at = at, col = "black", line = 1)

	par(new=TRUE)
	YRange <- range( c(br, br2) );
	print(YRange)
	plot(h$mid, br, type="p", axes=FALSE, ylab="", xlab="", bty="n", pch=16, xlim=XRange, ylim=YRange);

	par(new=TRUE)
	plot(h$mid, br2, type="l", axes=FALSE, ylab="", xlab="", bty="n", xlim=XRange, ylim=YRange);

	par(new=TRUE);
	axis(4, at=pretty(YRange))

	par(new=TRUE);
	mtext(ylab[2], 4, 3) 

	par(new=TRUE);
	#print(score[1:10])
	#print(default[1:10])
	
	#AR_value<- AR(score, default, plot=FALSE)[[1]]$stats["AR"]
	#print(AR_value);
  	mtext( xlab , 1, line=3);	
	#mtext( paste("AR = ", round(AR_value*100,2), "%;   BR = ",round(mean(default)*100,digits=2),"%" ), 1, 5, col="darkgreen");

	#wracam do wejœciowych ustawieñ parametrów, ale pozostawiam koordynaty,
	#¿eby mo¿na by³o ³atwo dodawaæ coœ do wykresu.s
	user<-par(no.readonly=TRUE)$usr;
	par(def_par);
	par(usr=user);
}

#' Rysuje jakoœæ kalibracji
#'
#' Rysuje jakoœæ kalibracji
#' @param score score.
#' @param default default.
#' @param estym wartoœci wyestymowane przez model.
#' @param buckets Sugerowana liczba bucketów. 
#' @param span Wspó³czynnik wyg³adzaj¹cy, wykorzystywany przez funkcjê \code{\link{locfit}}
#' @param hist_col Kolor histogramu 
#' @param ylab Label
#' @param xlab Label
#' @param legx po³o¿enie legendy
#' @param legy po³o¿enie legendy
#' @param legcex po³o¿enie legendy.
#' @param leg.label etykiety do legendy.
#' @param ylim2 ograniczenie y-ka.
#' @param ... dodatkowe parametry do funkcji \code{\link{plot}}.
#' @export

informacje_kal<-function (score, default, estym, buckets = 20, span = 0.8, hist_col = "blue", 
		ylab = c("Frequency", "BR"), xlab = "Calibrated score", legx = 0, 
		legy = 0, legcex = 1, leg.label=quote(c("Observations frequency", "Average LGD", 
				"Smoothed average LGD", "Expected LGD")),ylim2 = NULL, ...) 
{
	def_par = par(no.readonly = TRUE)
	start_par <- par()
	par(mar = c(5, 4, 4, 5))
	h <- hist(score, plot = FALSE, breaks=buckets)
	if (length(unique(default)) == 2) 
		local <- locfit::locfit(default ~ locfit::lp(score, nn = span), family = "binomial", 
				link = "logit")
	else local <- locfit::locfit(default ~ locfit::lp(score, nn = span))
	br2 <- predict(local, newdata = h$mid, type = "response")
	cut_ <- cut(score, breaks = h$breaks)
	br <- tapply(default, cut_, mean)
	XRange <- range(c(h[["break"]], h$mid))
	print(XRange)
	plot(h, col = hist_col, axes = FALSE, ann = FALSE, ylab = "", 
			xlim = XRange, ...)
	print(paste("----", par(no.readonly = TRUE)$usr))
	par(new = TRUE)
	axis(1, col = "black")
	par(new = TRUE)
	axis(2, col = hist_col)
	par(new = TRUE)
	mtext(side = 2, text = ylab[1], line = 3, col = "black")
	par(new = TRUE)
	if (is.null(ylim2)) {
		zakr <- c(br, br2, estym)
		zakr <- zakr[!is.na(zakr)]
		YRange <- range(zakr)
	}
	else YRange <- ylim2
	print(YRange)
	plot(h$mid, br, type = "p", axes = FALSE, ylab = "", xlab = "", 
			bty = "n", pch = 16, xlim = XRange, ylim = YRange)
	par(new = TRUE)
	plot(h$mid, br2, type = "l", axes = FALSE, ylab = "", xlab = "", 
			bty = "n", xlim = XRange, ylim = YRange)
	par(new = TRUE)
	axis(4, at = pretty(YRange))
	par(new = TRUE)
	mtext(ylab[2], 4, 3)
	par(new = TRUE)
	mtext(xlab, 1, line = 3)
	kolej <- order(score)
	par(new = TRUE)
	plot(score[kolej], estym[kolej], col = "green", lty = "dashed", 
			, type = "l", axes = FALSE, ylab = "", xlab = "", bty = "n", 
			xlim = XRange, ylim = YRange, ...)
	legend(legx, legy, legend = leg.label, col = c("blue", 
					"black", "black", "green"), lty = c("blank", "blank", 
					"solid", "dashed"), pch = c(22, 19, NA, NA), bg = c(NA, 
					"blue", "black"), cex = legcex)
	par(def_par)
}


#' Dla obiektu zwróconego przez funkcjê \code{\link{AR}} rysuje krzyw¹ ROC lub CAP.
#'
#' Dla obiektu zwróconego przez funkcjê \code{\link{AR}} rysuje krzyw¹ ROC lub CAP.
#' @title Rysuje krzywe ROC i CAP 
#' @param ar Obiekt klasy XYZ. 
#' @param plot_type Typ wykresu. 
#' @param adjusted_AR zmienna logiczna. TRUE, gdy trzeba osobno wyliczyæ pole pod
#' 		  modelem idealnym (np. w przypadku LGD).
#' @author Micha³ Danaj
#' @export 
plot_AR<-function(ar, plot_type=c("ROC", "CAP"), adjusted_AR=FALSE)
{
	plot_type<-match.arg(plot_type);

	if (class(ar)=="AR")
		ar<-list(ar=ar);

	plot(c(0,1), c(0,1), type="l", lty=1, col=1, xlab="", ylab="");
	if (plot_type=="CAP" && is.null(ar[[1]][['perfect_lgd_table']])){
		br<-ar[[1]]$stats["br"];
		lines(c(0,br,1),c(0,1,1), col=1);
	}
	else if (plot_type=="CAP" && !is.null(ar[[1]][['perfect_lgd_table']])){
		lines(c(0,ar[[1]][['perfect_lgd_table']]$cum_pct_obs_lgd), c(0,ar[[1]][['perfect_lgd_table']]$cum_pct_bad_lgd))	
	}
	
	AR<-rep(-1,length(ar));
	labels<-rep("",length(ar))
	for (i in 1:length(ar))
	{
		if (plot_type=="CAP")
			lines(c(0,ar[[i]]$table$cum_pct_obs), c(0,ar[[i]]$table$cum_pct_bad), type="l", lty=i+1, col=i+1)
		else
			lines(c(0,ar[[i]]$table$cum_pct_good), c(0,ar[[i]]$table$cum_pct_bad), type="l", lty=i+1, col=i+1);
		
		if (adjusted_AR==FALSE)
			AR[i]<-ar[[i]]$stats["AR"]
		else
			AR[i]<-ar[[i]]$stats["AR_lgd_adjusted"];
		labels[i]<-ar[[i]]$label;
	}
	
	kolej<-order(AR);
	AR<-round(AR*100,2);

	legend(1,0,paste(labels[kolej], " (", AR[kolej], "%)", sep="" ), 
			col=kolej+1, cex=0.8, xjust=1, yjust=0, lty=kolej+1 )
	
}

# TODO Dodaæ opcjê subset.
# TODO Co z b³edem out of vertex space?

#'  Rysuje lokalnie wyg³adzon¹ funckjê.
#'  
#'  Rysuje i zwraca statystyki dla bucketów.  
#' @param score Wektor zmiennych numerycznych. 
#' @param default Wektor zmiennej dwumianowej. 
#' @param buckets Liczba bucketów, na ile nele¿y podzieliæ \code{score}. 
#' @param wytnij Ile krañcowych obserwacji wyci¹æ. 
#' @param span Wspó³czynnik wyg³adzania. Szegó³y w funkcji \code{\link{locfit}}
#' @param degree Stopieñ wielomianu do lokalnego wyg³adzania. Szegó³y w funkcji \code{\link{locfit}} 
#' @param plot Czy rysowaæ wykres. 
#' @param target jeœli \code{br}, to na osi OY bêdzie BR. W przeciwnym razie bêdzie logit(BR)
#' @param new Czy rysowaæ wykres od nowa. 
#' @param col_points Kolor punktów. 
#' @param col_line Kolor lini. 
#' @param index jeœli \code{TRUE}, na osi OX bêd¹ numery kolejnych bucketów.
#'				W przeciwnym razie na osi OX bêd¹ wartoœci \code{score}.
#' @param ... dodatkowe parametry.
#' @author Micha³ Danaj 
#' @export
#' @examples
#'		n<-1000;
#'		x1<-rnorm(n);
#'		x2<-x1+rnorm(n);
#'		y<-(x1+rnorm(n))<0;
#'		
#'		reg_nieparam(x1,y, buckets=20)
#'		reg_nieparam(x2,y, buckets=20, new=FALSE, col_line="green",col_points="green")

reg_nieparam<-function (score, default, buckets = 100, wytnij = 0, span = 0.7,
		degree = 2, plot = TRUE, target = "br", new = TRUE, col_points = "black",
		col_line = "darkblue", index = FALSE, ...)
{
	dane <- data.frame(score, default)
	if (wytnij > 0){
		do_usuniecia<-usun_konce(dane$score, prob = wytnij);
		if (length(do_usuniecia)>0)
			dane <- dane[-do_usuniecia,]
	}
	bucket <- buckety_br(dane$score, dane$default, buckets, method = "eq_count")
	if (length(unique(default)) == 2)
		l <- locfit::locfit(default ~ locfit::lp(score, nn = span), family = "binomial",
				link = "logit", data = dane)
	else l <- locfit::locfit(default ~ locfit::lp(score, nn = span), data = dane)
	b2 <- predict(l, newdata = bucket$srodek)
	if (target == "br")
		bucket2 <- cbind(bucket, fitted = b2)
	else bucket2 <- cbind(bucket, fitted = log(b2/(1 - b2)))
	skala <- sqrt(bucket$n_obs/(length(score)/buckets))
	x <- bucket2$srodek
	if (index)
		x <- bucket$nr
	if (plot) {
		if (new == TRUE)
			plot(x, with(bucket2, get(target)), col = col_points,
					cex = skala, ...)
		else points(x, with(bucket2, get(target)), cex = skala,
					col = col_points, ...)
		lines(x, bucket2$fitted, col = col_line, ...)
	}
	bucket2
}

#' Usuwa krañcowe wartoœci.
#'
#' Zwraca indeksy \code{prob} najmniejszych i \code{prob} najwiêkszych wartoœci. Jeœli
#' jednak nie jest mo¿liwe ustalenie co najwy¿ej \code{prob} obserwacji, nie 
#' zostaje wybrana ¿adna. Ma to zapobiec sytuacji, gdy np. najmniejsz¹ wartoœci¹ jest 0
#' i w próbie znajduje siê 10\% takich obserwacji, a my chcemy usun¹æ tylko 1\%.
#' @param score Wektor wartoœci numerycznych. 
#' @param prob Jak¹ czêœæ obserwacj nale¿y usun¹æ z jednego krañca. 
#' @param weights Wagi.
#' @return Zwraca wektor z indeksami elementów, które powinny zostaæ usuniête.
#' @author Micha³ Danaj
#' @export
#' @examples
#' x<-sort(rnorm(10));
#' x
#' usun<-usun_konce(x, prob=0.01);
#' x[-usun]
#'
#' #usuwa tylko obserwacjê z prawego krañca
#' x2<-c(rep(min(x),5),x[5:10])
#' x2
#' usun<-usun_konce(x2, prob=0.01);
#' x2[-usun]

usun_konce<-function (score, prob = 0.01, weights=NULL)
{
	
	if (is.null(weights))
		weights<-rep(1,length(score))
	
	po_score<-tapply(weights, score, sum)
	s <- cumsum(po_score/sum(weights))
	
	new_min <- as.numeric(names(which.max(s[s <= prob])))
	
	if (length(new_min) == 0)
		new_min <- -.Machine$double.xmax
	
	#od którego elementu powinienm wycinaæ wartoœci przekraczaj¹ce 1-prob
	temp<-which(s >= 1 - prob)[1]+1;
	if (is.na(temp))
		new_max <-  .Machine$double.xmax
	else
		new_max <- as.numeric(names(s[temp]));
	
	return(which(score <= new_min | new_max <= score))
}