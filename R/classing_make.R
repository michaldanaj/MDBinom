# Co dodaæ:
# - dodawanie pojedynczych wierszy do klasy
# - po dodaniu wiersza, nale¿y okreœliæ stan obiektu jako nieprzeliczony
# - funkcje powinny móc korzystaæ tylko z przeliczonej klasy
# - funkcja do przeliczenia klasy na zadanych danych
# - dodaæ wyliczanie œrednich z dowolnych kolumn i dodawanie ich do statystyk klasy 
#   (albo rozszerzyæ klasê o tak¹ mo¿liwoœæ). Miêdzy innymi by³oby to po to,
# aby móc dodaæ œredni¹ ze score, ¿eby móc wyrysowywaæ j¹ póŸniej 
#
#


# 
# N <- 1e5
# x <- rep(1:10, N/10)
# x <- rep(letters[1:10], N/10)
# y <- rep(rbinom(size=1, n=N, prob=0.4))
# ekstra <- rep(rbinom(size=1, n=N, prob=0.4))
# #breaks=c(-Inf, 1,5.5,6,7,8,910)
# weights=rep(1,length(x))
# dt<-data.table(x,y,weights)
# total=TRUE
# n=2
# method="eq_length"
# 
# for(i in 1:30){
#   # buckety_br(x,y, 5)
#   # bckt_br(x,y, n=5)
#   # 
#   # buckety_stat2(breaks,x,y)
#   # bckt_stat2(breaks, x=x,y=y)
#   
#   buckety_stat(x,y)
#   bckt_stat( x=x,y=y)
# }

#' Dzieli na przedzia³y jedn¹ z dwóch metod i wylicza na nich bad rate.
#'
#' Jako ¿e jest to wersja przejœciowa, nie ma ¿adnych detali.
#' Mo¿e poza tym, ¿e jeœli nie ma obserwacji w jakimœ bukcecie, to go usuwa.
#'
#' @param x  Zmienna, któr¹ bêdziemy dyskretyzowaæ.  
#' @param y  Zmienna dwumianowa.  
#' @param n  Liczba wynikowych przedzia³ów.  
#' @param method  Sposób podzia³u na przedzia³y. Szczegó³y w sekcji Details.  
#' @param one.value.action Jeszcze nie dzia³a.
#' @param weights Wagi.
#' 
#' @return  
#' Zwraca \code{data.frame}, w którym dla \code{i}-tego wiersza podane s¹ statystyki
#' dla \code{i}-tego przedzia³u.
#' \item{nr }{Numer przedzia³u.}
#' \item{od }{Pocz¹tek przedzia³u.}
#' \item{srodek}{Œrodek przedzia³u.}
#' \item{mean}{Œrednia z obserwacji w przedziale.}
#' \item{do}{Koniec przedzia³u.}
#' \item{n_default}{Liczba defaultów.}
#' \item{n_obs}{Liczba obserwacji.}
#' \item{br}{Bad rate.}
#' \item{logit}{Logit.}
#' \item{probit}{Probit.}
#' \item{var}{Wariancja zmiennej \code{default}.}
#' \item{waga}{Waga jako odwrotnoœæ warinacji. Gdy \code{n_default=0} to przyjmuje siê,
#'		¿e \code{n_default=0.5}}
#' @examples 
#' x<-rnorm(1000);
#' y<-(x+rnorm(1000))<0;
#' buckety_br(x, y, 10);
#' @author Micha³ Danaj
#' @export
buckety_br<-function(x, y, n, method=c("eq_length", "eq_count"), one.value.action=c("none","combine"),
		weights=NULL)
{                                         
#	TODO - domyœlna wartoœæ method
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
		#w przypadku, gdy jedna wartoœæ jest dla wielu kwantyli, zdarzaj¹ siê problemy numeryczne
		#¿e wartoœæ teoretycznie jest taka sama, ale ró¿ni siê na 15-tym miejscu po przecinku.
		#tak na szybko, brute force obejœcie: sortujê
		granice<-sort(as.vector(unique(Hmisc::wtd.quantile(x, prob=0:n/n, type='quantile', weights=weights))));
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
	
	#z tego co pamiêtam, mamy dziêki temu wyrzuciæ puste przedzia³y
	zostaw<-sort(unique(przedzial));
	
	as.data.frame(cbind(nr=zostaw,  od=od[zostaw], do=do[zostaw], 
					srodek, mean,  n_default, n_obs, br, logit, probit, var=br*(1-br)/n_obs))
}

#' Dzieli na przedzia³y jedn¹ z dwóch metod i wylicza na nich bad rate (dev). 
#'
#' Wersja w developmencie.
#' Mo¿e poza tym, ¿e jeœli nie ma obserwacji w jakimœ bukcecie, to go usuwa.
#'
#' @param x  Zmienna, któr¹ bêdziemy dyskretyzowaæ.  
#' @param y  Zmienna dwumianowa.  
#' @param n  Liczba wynikowych przedzia³ów.  
#' @param weights wektor z wagami.
#' @param method  Sposób podzia³u na przedzia³y. Szczegó³y w sekcji Details.  
#' @param one.value.action Jeszcze nie dzia³a.
#' @param avg \code{vector}, \code{data.frame}, \code{data.table} z wartoœciami dla których
#' zostan¹ wyliczone œrednie.
#' @param total czy do³¹czyæ wiersz z podsumowaniem.
#' @param sort_x jeœli \code{TRUE}, wynikowa tabela zostanie posortowana po wartoœciach 
#' @return  
#' Zwraca \code{data.frame}, w którym dla \code{i}-tego wiersza podane s¹ statystyki
#' dla \code{i}-tego przedzia³u.
#' @author Micha³ Danaj
#' @export
bckt_br<-function(x, y, n, weights=rep(1,length(x)), 
                  method=c("eq_length", "eq_count"), one.value.action=c("none","combine"),
                  avg=NULL ,total = TRUE, sort_x=TRUE)
{                                         
  #	TODO - domyœlna wartoœæ method
  method<-match.arg(method);
  
  #dolny kraniec bucketa (wartosc x)
  od=c(1:n);
  #gorny kranieb bucketa (wartosc x)
  do=c(1:n);
  
  if (method=="eq_length"){
    granice<-seq(min(x),max(x),length.out=n+1)
  }else{
    #w przypadku, gdy jedna wartoœæ jest dla wielu kwantyli, zdarzaj¹ siê problemy numeryczne
    #¿e wartoœæ teoretycznie jest taka sama, ale ró¿ni siê na 15-tym miejscu po przecinku.
    #tak na szybko, brute force obejœcie: sortujê
    granice<-sort(as.vector(unique(Hmisc::wtd.quantile(x, prob=0:n/n, type='quantile', weights=weights))));
  }
  
  dt<-data.table(x,y,weights)
  buckety <- bckt_stat2(breaks=granice, dt=dt, avg=avg, total = total, sort_x=sort_x)
  buckety
}


#' Wylicza statystyki zmiennej \code{default} dla podanych grup \code{buckets}
#'
#' Wylicza statystyki zmiennej \code{default} dla podanych grup \code{buckets} 
#' @usage buckety_stat(bucket, default, total=TRUE)
#  buckety_stat_wtd(bucket, default, weights=rep(1,length(bucket)), total=TRUE)
#' @param bucket wektor z identyfikatorami bucketów.
#' @param default wektor zmiennych numerycznych.
#' @param total czy do³¹czyæ wiersz z podsumowaniem.
#' 
#' @return
#'   Zwraca \code{data.frame}, w którym dla \code{i}-tego wiersza podane s¹ statystyki
#'   dla \code{i}-tego przedzia³u.
#'   \item{nr }{Numer przedzia³u.}
#'   \item{od }{Pocz¹tek przedzia³u.}
#'   \item{srodek}{Œrodek przedzia³u.}
#'   \item{mean}{Œrednia z obserwacji w przedziale.}
#'   \item{do}{Koniec przedzia³u.}
#'   \item{n_default}{Liczba defaultów.}
#'   \item{n_obs}{Liczba obserwacji.}
#'   \item{br}{Bad rate.}
#'   \item{logit}{Logit.}
#'   \item{probit}{Probit.}
#'   \item{var}{Wariancja zmiennej \code{default}.}
#'   \item{waga}{Waga jako odwrotnoœæ warinacji. Gdy \code{n_default=0} to przyjmuje siê,
#' 			¿e \code{n_default=0.5}}
#' @author Micha³ Danaj 
#' @export
buckety_stat<-function (bucket, default, total = TRUE)
{
	
	if (any(is.na(bucket)) || any(is.na(default)))
		stop("W argumentach funkcji pojawi³y siê braki danych.")
	
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

#TODO za³o¿yæ ¿e jak jest 0, o daæ 0.5 do wyliczenia woe i logit
#' Wylicza statystyki zmiennej \code{y} dla podanych grup \code{x} (dev)
#'
#' Wylicza statystyki zmiennej \code{y} dla podanych grup \code{x}. W za³o¿eniu
#' \code{y} jest zmienn¹ dwumianow¹ o wartoœciach {0,1}.
#' 
#' @param x wektor z identyfikatorami bucketów.Nie mo¿e byæ braków danych.
#' @param y wektor zmiennych numerycznych. Z za³o¿enia zmienna dwumianowa, ale statystyki
#' zostan¹ policzone dla ka¿dej innej zmiennej numerycznej.Nie mo¿e byæ braków danych.
#' @param weights wektor z wagami.
#' @param dt \code{data.table} z kolumnami \code{x}, \code{y}, \code{weights}.
#' @param avg \code{vector}, \code{data.frame}, \code{data.table} z wartoœciami dla których
#' zostan¹ wyliczone œrednie.
#' @param total czy do³¹czyæ wiersz z podsumowaniem.
#' @param sort_x jeœli \code{TRUE}, wynikowa tabela zostanie posortowana po wartoœciach 
#' cechy \code{x}. W przeciwnym wypadku sortowanie bêdzie zgodne z porz¹dkiem w danych.
#' 
#' @return Zwraca \code{data.table} ze statystykami.
#' @author Micha³ Danaj 
#' @export
bckt_stat<-function (x=NULL, y=NULL, weights=rep(1, length(x)), 
                     dt=NULL, avg=NULL ,total = TRUE, sort_x=TRUE)
{

  if (!is.null(x))
    dt <- data.table(x, y, weights)

  if (any(is.na(dt$x)) || any(is.na(dt$y)))
    stop("W argumentach funkcji pojawi³y siê braki danych.")
  
  if (is.factor(dt$x)){
    dt$x <- factor(dt$x)
  }
  
  #jeœli zosta³y podane kolumny 'avg', liczymy od razu dla nich œrednie
  if(!is.null(avg)){
    dt_avg <- data.table(x=dt$x, avg)
    #TODO ten lapply strasznie wolno dzia³a. Popatrzeæ, czy nie da siê czegoœ
    #z tym zrobiæ
    dt_avg <- dt_avg[,lapply(.SD, mean), x][,-1]
  }
  
  dt_wyn <- dt[, .(
    n_good = sum(weights) - sum(y*weights)
    ,n_bad = sum(y*weights)
    ,n_obs = sum(weights)
  ), x]

  totals <- dt_wyn[,.(good_all=sum(n_good), bad_all=sum(n_bad), obs_all=sum(n_obs))]  
  
  dt_wyn<- dt_wyn[,.(
    nr=NA
    ,x
    ,labels=as.character(x)
    ,n_good
    ,pct_good = n_good/totals$good_all
    ,n_bad
    ,pct_bad = n_bad/totals$bad_all
    ,n_obs
    ,pct_obs = n_obs/totals$obs_all
    ,br = n_bad/n_obs
    ,logit = log(n_bad/n_obs)
    ,woe = log(n_bad/n_good)
    )]
  
  if (sort_x==TRUE){
    setorder(dt_wyn, x)
  }
  
  rownames(dt_wyn) <- dt_wyn$labels
  
  #wyliczam wiersz z Totalem
  if (total) {
    
    #Dla dodatkowych kolumn
    if (!is.null(avg)){
      totals_avg<-as.data.frame(lapply(dt_avg, function(x)sum(x*dt_wyn$n_obs)/sum(dt_wyn$n_obs)))
      dt_avg<-rbindlist(list(dt_avg, data.table(totals_avg)))
    }
    
    #Dla zwyk³ych kolumn
    totals <- totals[,.(n_good=good_all, n_bad=bad_all, n_obs=obs_all)][,.(
      nr=NA
      ,x=NA
      ,label='TOTAL'
      ,n_good
      ,pct_good = n_good/totals$good_all
      ,n_bad
      ,pct_bad = n_bad/totals$bad_all
      ,n_obs
      ,pct_obs = n_obs/totals$obs_all
      ,br = n_bad/n_obs
      ,logit = log(n_bad/n_obs)
      ,woe = log(n_bad/n_good)
    )]
    
    dt_wyn<-rbindlist(list(dt_wyn, totals))
    
  }
  
  dt_wyn$nr=seq(nrow(dt_wyn))
  
  #Jeœli by³y dodatkowe kolumy, to je tutaj dodajê
  if (!is.null(avg)){
    dt_wyn <- cbind(dt_wyn ,dt_avg)
  }
  
  #attr(dt_wyn,'typeof_x')=typeof(x)
  #attr(dt_wyn,'typeof_x')=class(x)
  
  return(dt_wyn)
}


#' Wylicza statystyki dla podanych przedzia³ów
#'
#' Dzieli zmienn¹ \code{score} na przedzia³y okreœlone przez \code{breaks}
#' i wylicza statystyki dla tak powsta³ych bucketów przy pomocy funkcji.
#' 
#' Do przypisania wartoœci do przedzia³u u¿ywana jest funkcja
#' 
#' \code{findInterval(score, breaks, rightmost.closed = TRUE, all.inside = TRUE)}.
#' 
#' @param breaks punkty podzia³u
#' @param score zmienna score'owa.
#' @param def zmienna odpowiedzi z zakresu [0,1]. Np. default, LGD.
#' @param total czy wyliczaæ wiersz z podsumowaniem.
#' @seealso \code{\link{buckety_stat}}.
#' @return \code{data.frame} ze statystykami.
#' @author Micha³ Danaj
#' @export
buckety_stat2<-function(breaks, score, def, total=FALSE){
	
	
	if (any(is.na(breaks)) || any(is.na(score)) || any(is.na(def)))
		stop("W argumentach funkcji pojawi³y siê braki danych.");
	
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
	#Wybieram tylko te przedzia³y, w których s¹ jakieœ dane. Mo¿e byæ z tym trochê
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

	return(buckety);
}


#' Wylicza statystyki dla podanych przedzia³ów (dev)
#'
#' Dzieli zmienn¹ \code{x} na przedzia³y okreœlone przez \code{breaks}
#' i wylicza statystyki dla tak powsta³ych bucketów.
#' 
#' Do przypisania wartoœci do przedzia³u u¿ywana jest funkcja
#' 
#' \code{findInterval(x, y, rightmost.closed = TRUE, all.inside = TRUE)}.
#' 
#' @param breaks punkty podzia³u
#' @param x zmienna score'owa.
#' @param y zmienna odpowiedzi z zakresu [0,1]. Np. default, LGD.
#' @param weights wektor z wagami.
#' @param dt \code{data.table} z kolumnami \code{x}, \code{y}, \code{weights}.
#' @param avg \code{vector}, \code{data.frame}, \code{data.table} z wartoœciami dla których
#' zostan¹ wyliczone œrednie.
#' @param total czy do³¹czyæ wiersz z podsumowaniem.
#' @param sort_x jeœli \code{TRUE}, wynikowa tabela zostanie posortowana po wartoœciach 
#' 
#' @seealso \code{\link{buckety_stat}}.
#' @return \code{data.frame} ze statystykami.
#' @author Micha³ Danaj
#' @export
bckt_stat2<-function(breaks, x=NULL, y=NULL, weights=rep(1,length(x)), dt=NULL, avg=NULL,
                     total = TRUE, sort_x=TRUE){

  if (!is.null(x))
    dt <- data.table(x, y, weights)
  
  if (any(is.na(breaks)) || any(is.na(dt$x)) || any(is.na(dt$y)))
    stop("W argumentach funkcji pojawi³y siê braki danych.");
  
  zakres_danych<-range(na.omit(dt$x))
  zakres_breaks<-range(breaks)
  if (zakres_danych[1]<zakres_breaks[1] || 
      zakres_danych[2]>zakres_breaks[2])
    warning("buckety_stat2: Zmienna 'x' spoza zakresu 'breaks'");
  
  breaks<-sort(unique(breaks));
  
  #podzial<-cut(x, breaks, include.lowest=TRUE);

  #dt<-data.table(x,y, weights)
  dt$przedzial_nr<-findInterval(dt$x, breaks, rightmost.closed = TRUE, all.inside = TRUE);
  
  nawiasy_koniec<-c(rep(")", length(breaks)-2),"]")
  
  od<-breaks[-length(breaks)]
  do<-breaks[-1]
  
  labels<-paste("[",od,", ",do, nawiasy_koniec,sep="")
  
  #zastanowiæ siê, czy to zosawiæ, czy zmieniæ
  #podzial<-factor(przedzial_nr, labels=labels[finalne_przedzialy], ordered = TRUE)
  #buckety<-bckt_stat(podzial, y, total=total);
  dt$labels_assigned<-labels[dt$przedzial_nr]
  dt[,x:=przedzial_nr]
  buckety_wyn<-bckt_stat(dt=dt, avg=avg, total = total, sort_x=sort_x);
  
  #na czas przetwarzania usuwam wiersz z totalem
  if(total==TRUE) {
    buckety <- buckety_wyn[-nrow(buckety_wyn),]
  } else buckety <- buckety_wyn
  
  buckety_temp<-data.table(od=od[buckety$x])
  buckety_temp$do<-do[buckety$x];
  
  buckety_temp$srodek<-(buckety_temp$od+buckety_temp$do)/2;
  
  temp<-dt[,.(median=median(x), mean=mean(x)), przedzial_nr]
  setorder(temp, przedzial_nr)
  
  buckety_temp <- cbind(buckety_temp, temp)
  
  if(total==TRUE){
    puste<-as.data.table(matrix(rep(NA, ncol(buckety_temp)), nrow=1, ncol=ncol(buckety_temp)))
    names(puste)<-names(buckety_temp)
    buckety_temp <- rbind(buckety_temp,puste)
  }
  
  buckety_wyn <- cbind(buckety_wyn, buckety_temp)
  buckety_wyn$labels <- labels[buckety_wyn$przedzial_nr]  
  
  return(buckety_wyn);
}


#' £¹czy ze sob¹ buckety
#' 
#' £¹czy buckety. W przypadku bucketów z przedzia³ami zmiennej ci¹g³ej, mo¿liwe jest 
#' po³¹czenie tylko
#' przedzia³ów przylegaj¹cych do siebie. Dla nowo powsta³ych bucketów wylicza statystyki. 
#' W przypadku ³¹czenia przedzia³ów zmiennej ci¹g³ej, wiersze z tymi bucketami zostan¹ ze sob¹
#' po³¹czone i powstanie \code{data.frame} z liczb¹ wierszy o jeden mniejsz¹ ni¿ w \code{bucket}.
#' Przy ³¹czeniu bucketów dyskretnych lub dyskretnego i ci¹g³ego, wiersze nie zostan¹ usuniête. 
#' Zostanie im nadany wspólny label oraz wspólny numer. Jeœli liczba bucketów do po³¹czenia jest<=1,
#' zwracany jest wejœciowy podzia³. 
#' @param x zmienna score'owa.
#' @param y zmienna odpowiedzi z zakresu [0,1]. Np. default, LGD.
#' @param buckets buckety.
#' @param row_idxs numery wierszy w \code{buckets} do po³¹czenia.
#' @param new_label label jaki bêdzie nadany po³¹czonym bucketom. W przypadku braku podania, zostanie zatosowany domyœlny.
#' @return \code{data.frame}.
#' @author Micha³ Danaj
#' @export
polacz_buckety<-function(x, y, buckets, row_idxs, new_label=NULL)
{
	row_idxs<-sort(unique(row_idxs));
	#sprawdzam, czy jest co ³¹czyæ
	if (length(row_idxs)<=1)
		return(buckets)
	#sprawdzam, czy nie wyszliœmy poza zakres
	if(min(row_idxs)<1 | max(row_idxs)>nrow(buckets)-1) 
		stop("Numery wierszy s¹ poza zakresem zmiennej 'buckets'");
	#sprawdzam, czy nie ma ju¿ takiego labela w innych wierszach
	if (!is.null(new_label) & any( buckets$label[-row_idxs]==new_label))
		warning("Podana wartoœæ 'new_label' znajduje siê ju¿ w 'buckets' w wierszu innym ni¿ aktualnie ³¹czone wiersze.")
	for (i in 1:(length(row_idxs)-1)){
		nr1<-row_idxs[i];
		nr2<-row_idxs[i+1];
		#jeœli ³¹czymy dane ci¹g³e
		if (all((!is.na(buckets$od) & !is.na(buckets$do))[c(nr1,nr1)])){
			if (nr2-nr1!=1)
				stop("B³êdnie podane wiersze do po³¹czenia! Przedzia³y powinny byæ do siebie przyleg³e!")
			#jeœli label nie podany, to go tworzê
			if (is.null(new_label)){
				new_label<-strsplit(buckets$label[nr1],',')[[1]][1]
				new_label<-paste(new_label,strsplit(buckets$label[nr2],',')[[1]][2],sep=',')
			}
			#jeszcze przepisujê krañce przedzia³ów i inne wartoœci nie wyliczane w buckety_stat a
			#wyliczane w buckety_stat2
			buckets$do[nr1]<-buckets$do[nr2];
			buckets$srodek[nr1]<-c(buckets$od[nr1]+buckets$do[nr1])/2;
			buckets$label[nr1]<-new_label;
			rownames(buckets)[nr1]<-new_label;
			#usuwam drugi bucket
			buckets<-buckets[-nr2,];
			
		}else{
			if (is.null(new_label))
				new_label<-paste(buckets$label[nr1],buckets$label[nr2],sep=',');
			buckets$label[c(nr1,nr2)]<-new_label;
			buckets$nr[nr2]<-buckets$nr[nr1];
		}
		buckets$fitted<-buckets$label;
		x_buckets<-przypisz2(x,buckets);
		buckets_new<-buckety_stat(x_buckets, y)
		buckets<-cbind(buckets[,c('nr','label','discret','od','srodek','do')], 
				buckets_new[buckets$label,c('n_good','pct_good','n_bad','pct_bad','n_obs','pct_obs','br','woe','logit')]);
	}	
	return(buckets);
}
