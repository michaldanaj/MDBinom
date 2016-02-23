# Co doda�:
# - dodawanie pojedynczych wierszy do klasy
# - po dodaniu wiersza, nale�y okre�li� stan obiektu jako nieprzeliczony
# - funkcje powinny m�c korzysta� tylko z przeliczonej klasy
# - funkcja do przeliczenia klasy na zadanych danych
# - doda� wyliczanie �rednich z dowolnych kolumn i dodawanie ich do statystyk klasy 
#   (albo rozszerzy� klas� o tak� mo�liwo��). Mi�dzy innymi by�oby to po to,
# aby m�c doda� �redni� ze score, �eby m�c wyrysowywa� j� p�niej 
#
#



#'  Dzieli na przedzia�y jedn� z dw�ch metod i wylicza na nich bad rate.
#'
#'  Jako �e jest to wersja przej�ciowa, nie ma �adnych detali.
#'  Mo�e poza tym, �e je�li nie ma obserwacji w jakim� bukcecie, to go usuwa.
#'
#'	@title Podzia� na przedzia�y. 
#'
#'  @param x  Zmienna, kt�r� b�dziemy dyskretyzowa�.  
#'  @param y  Zmienna dwumianowa.  
#'  @param n  Liczba wynikowych przedzia��w.  
#'  @param method  Spos�b podzia�u na przedzia�y. Szczeg�y w sekcji Details.  
#'  @param one.value.action Jeszcze nie dzia�a.
#'  @param weights Wagi.
#'  @return  
#'  Zwraca \code{data.frame}, w kt�rym dla \code{i}-tego wiersza podane s� statystyki
#'  dla \code{i}-tego przedzia�u.
#'  \item{nr }{Numer przedzia�u.}
#'  \item{od }{Pocz�tek przedzia�u.}
#'  \item{srodek}{�rodek przedzia�u.}
#'  \item{mean}{�rednia z obserwacji w przedziale.}
#'  \item{do}{Koniec przedzia�u.}
#'  \item{n_default}{Liczba default�w.}
#'  \item{n_obs}{Liczba obserwacji.}
#'  \item{br}{Bad rate.}
#'  \item{logit}{Logit.}
#'  \item{probit}{Probit.}
#'  \item{var}{Wariancja zmiennej \code{default}.}
#'  \item{waga}{Waga jako odwrotno�� warinacji. Gdy \code{n_default=0} to przyjmuje si�,
#'			�e \code{n_default=0.5}}
#' @examples 
#' x<-rnorm(1000);
#' y<-(x+rnorm(1000))<0;
#' buckety_br(x, y, 10);
#' @author Micha� Danaj
#' @export
buckety_br<-function(x, y, n, method=c("eq_length", "eq_count"), one.value.action=c("none","combine"),
		weights=NULL)
{                                         
#	TODO - domy�lna warto�� method
	method<-match.arg(method);
	
	if (is.null(weights))
		weights<-rep(1,length(x))
	
	#dolny kraniec bucketa (wartosc x)
	od=c(1:n);
	#gorny kranieb bucketa (wartosc x)
	do=c(1:n);
	
	if (method=="eq_length")
		granice<-seq(min(x),max(x),length.out=n+1)
	else{
		#w przypadku, gdy jedna warto�� jest dla wielu kwantyli, zdarzaj� si� problemy numeryczne
		#�e warto�� teoretycznie jest taka sama, ale r�ni si� na 15-tym miejscu po przecinku.
		#tak na szybko, brute force obej�cie: sortuj�
		granice<-sort(as.vector(unique(wtd.quantile(x, prob=0:n/n, type='quantile', weights=weights))));
	}
	#oznaczam, do ktorego przedzialu nalezy dana obserwacja
	przedzial<-findInterval(x, granice, rightmost.closed = TRUE, all.inside = TRUE);
	
	#i licze potrzebne rzeczy
	od<-granice[1:(length(granice)-1)];
	do<-granice[2:length(granice)];		
	
	srodek<-as.vector(tapply(x, przedzial, median));
	
	n_obs<- as.vector(tapply(weights, przedzial, sum));
	mean<-as.vector(tapply(x*weights, przedzial, FUN=sum))/n_obs;
	n_default<- as.vector(tapply(y*weights, przedzial, sum));
	br<- n_default/n_obs;
	logit<-log(br/(1-br));
	probit<-qnorm(br);
	
	#z tego co pami�tam, mamy dzi�ki temu wyrzuci� puste przedzia�y
	zostaw<-sort(unique(przedzial));
	
	as.data.frame(cbind(nr=zostaw,  od=od[zostaw], do=do[zostaw], 
					srodek, mean,  n_default, n_obs, br, logit, probit, var=br*(1-br)/n_obs))
}


#' Wylicza statystyki zmiennej \code{default} dla podanych grup \code{buckets}
#'
#' Wylicza statystyki zmiennej \code{default} dla podanych grup \code{buckets} 
#' @usage buckety_stat(bucket, default, total=TRUE)
#  buckety_stat_wtd(bucket, default, weights=rep(1,length(bucket)), total=TRUE)
#' @param bucket wektor z identyfikatorami bucket�w.
#' \code{as.character}, zawieraj�cy identyfikatory bucket�w.
#' @param default wektor zmiennych numerycznych.
#' @param total czy do��czy� wiersz z podsumowaniem.
#' 
#' @return
#'   Zwraca \code{data.frame}, w kt�rym dla \code{i}-tego wiersza podane s� statystyki
#'   dla \code{i}-tego przedzia�u.
#'   \item{nr }{Numer przedzia�u.}
#'   \item{od }{Pocz�tek przedzia�u.}
#'   \item{srodek}{�rodek przedzia�u.}
#'   \item{mean}{�rednia z obserwacji w przedziale.}
#'   \item{do}{Koniec przedzia�u.}
#'   \item{n_default}{Liczba default�w.}
#'   \item{n_obs}{Liczba obserwacji.}
#'   \item{br}{Bad rate.}
#'   \item{logit}{Logit.}
#'   \item{probit}{Probit.}
#'   \item{var}{Wariancja zmiennej \code{default}.}
#'   \item{waga}{Waga jako odwrotno�� warinacji. Gdy \code{n_default=0} to przyjmuje si�,
#' 			�e \code{n_default=0.5}}
#' @author Micha� Danaj 
#' @export
buckety_stat<-function (bucket, default, total = TRUE)
{
	print ('==============   buckety_stat  ==============')
	print('bucket:')
	
	print(table(is.na(bucket)))
	print('default:')
	print(table(is.na(default)))
	
	print('==============================================')
	
	
	if (any(is.na(bucket)) || any(is.na(default)))
		stop("Brakom danych w funkcji 'buckety_stat' m�wimy stanowcze NIE.")
	
	if (is.factor(bucket))
		bucket <- factor(bucket)
	obs <- table(bucket)
	obs_all <- sum(obs)
	bad <- as.vector(tapply(default, bucket, sum))
	bad_all <- sum(bad)
	br <- as.vector(tapply(default, bucket, mean))
	std_dev <- as.vector(tapply(default, bucket, sd))
	logit <- log(br/(1 - br))
	probit <- qnorm(br)
	waga <- NA
	pct_good = (obs - bad)/(obs_all - bad_all)
	pct_bad = bad/bad_all
	woe = log(pct_good/pct_bad)
	
	wynik <- as.data.frame(cbind(nr = 1:length(obs), n_good = (obs -
								bad), pct_good, n_bad = bad, pct_bad, n_obs = obs, pct_obs = obs/obs_all,
					gb_odds = (obs - bad)/bad, br, std_dev, woe, logit, probit,
					var = br * (1 - br)/obs, waga))
	
	rownames(wynik) <- sort(unique(bucket))
	if (total) {
		wynik <- rbind(wynik, TOTAL = c(length(obs) + 1, obs_all -
								bad_all, 1, bad_all, 1, obs_all, 1, (obs_all - bad_all)/bad_all,
						bad_all/obs_all, sd(default), 0, NA, NA, NA, NA))
	}
	wynik$label<-rownames(wynik);
	return(wynik)
}

#' Wylicza statystyki dla podanych przedzia��w
#'
#' Dzieli zmienn� \code{score} na przedzia�y \code{(from, to)}
#' i wylicza statystyki dla tak powsta�ych bucket�w przy pomocy funkcji.
#' 
#' Do przypisania warto�ci do przedzia�u u�ywana jest funkcja
#' 
#' \code{findInterval(score, breaks, rightmost.closed = TRUE, all.inside = TRUE)}.
#' 
#' @param breaks punktu podzia�u
#' @param score zmienna score'owa.
#' @param def zmienna odpowiedzi z zakresu [0,1]. Np. default, LGD.
#' @param total czy wylicza� wiersz z podsumowaniem.
#' @seealso \code{\link{buckety_stat}}.
#' @return \code{data.frame} ze statystykami.
#' @author Micha� Danaj
#' @export
buckety_stat2<-function(breaks, score, def, total=FALSE){
	
	print('===============  buckety_stat2')
	print('----------   breaks:')
	print(table(is.na(breaks)))
	print('score:')
	print(length(score))
	print('def:')
	print(length(def))
	print('---------')
	print('===============  koniec buckety_stat2')
	
	if (any(is.na(breaks)) || any(is.na(score)) || any(is.na(def)))
		stop("Brakom danych w funkcji 'buckety_stat2' m�wimy stanowcze NIE.");
	
	zakres_danych<-range(na.omit(score))
	zakres_breaks<-range(breaks)
	if (zakres_danych[1]<zakres_breaks[1] || 
			zakres_danych[2]>zakres_breaks[2])
		warning("buckety_stat2: Zmienna 'score' spoza zakresu 'breaks'");
	
	breaks<-sort(unique(breaks));
	
	#podzial<-cut(score, breaks, include.lowest=TRUE);
	
	przedzial_nr<-findInterval(score, breaks, rightmost.closed = TRUE, all.inside = TRUE);
	
	nawiasy_koniec<-c(rep(")", length(breaks)-2),"]")
	
	od<-breaks[-length(breaks)]
	do<-breaks[-1]
	
	labels<-paste("[",od,", ", do, nawiasy_koniec,sep="")
	#Wybieram tylko te przedzia�y, w kt�rych s� jakie� dane. Mo�e by� z tym troch�
	#kicha. Zobaczymy...
	finalne_przedzialy<-sort(unique(przedzial_nr))
	labels<-labels[finalne_przedzialy]
	
	podzial<-factor(przedzial_nr, labels=labels, ordered = TRUE)
	
	buckety<-buckety_stat(podzial, def, total=total);
	buckety$od<-od[finalne_przedzialy];
	buckety$do<-do[finalne_przedzialy];
	
	buckety$srodek<-(buckety$od+buckety$do)/2;
	buckety$median<-tapply(score, podzial,median);
	buckety$mean<-tapply(score, podzial,mean);
	
	rownames(buckety)<-labels;
	buckety$label<-labels;
	print ('+++++++++++++++++   wyj�cie z buckety_stat2  +++++++++++++++')
	return(buckety);
}
