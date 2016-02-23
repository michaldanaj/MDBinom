

#'  Znajduje przedzia� opisany w \code{bucket}, do kt�rego nale�y \code{x} i 
#'  przypisuje warto�� z \code{bucket_fitted}. Ewentualnie wykonuje interpolacj�.
#'
#'  Je�li \code{interpor=TRUE}, wykonuje interpolacj�, kt�rej w�z�ami s� 
#'  �rodki przedzia��w, pomi�dzy kt�rymi le�y \code{x}, a warto�ciami
#'   \code{bucket$fitted} odpowiadaj�ce tym �rodkom. 
#' 
#'   @title Przypisuje warto�� z przedzia�u 
#' 
#'   @param x Wektor zmiennych numerycznych. 
#'   @param bucket \code{data.frame} z kolumnami:
#'		\itemize{
#' 			\item od. Dolny kraniec przedzia�u.
#' 			\item do G�rny kreniec przedzia�u.
#' 			\item srodek�rodek przedzia�u.
#' 			\item fittedWarto�� przypisana do danego przedzia�u.
#' 		}
#'   @param interpol Czy przeprowadzi� interpolacj�?
#' 
#' @return  Wektor warto�ci.
#' 
#' @author Micha� Danaj
#' 
#' @examples
#' 		n<-1000;
#' 		x1<-rnorm(n);
#' 		y<-(x1+rnorm(n))<0;
#' 		
#' 		b<-buckety_br(x1,y,10);
#' 		b$fitted<-b$br;
#' 		
#' 		y_fitted<-przypisz(x1, b, interpol=FALSE)
#' 		plot(x1, y_fitted)
#' 		
#' 		y_fitted<-przypisz(x1, b, interpol=TRUE)
#' 		plot(x1, y_fitted) 
#' @seealso \code{\link{przypisz2}}
#' bo p�niej sortujemy i odnosimy si� tym porz�dkiem do pocz�tkowego porz�dku
#' @export
# TODO  BUG! Je�li bucket wiersze nie b�d� posortowane, to b�d� b��dne wyniki,
# TODO obs�uga jednego przedzia�u dla interpolacji
przypisz<-function (x, bucket, interpol = FALSE)
{
	n <- dim(bucket)[1]
	{
		if (interpol) {
			x[x < min(bucket$srodek)] <- min(bucket$srodek)
			x[x > max(bucket$srodek)] <- max(bucket$srodek)
			przedzial <- findInterval(x, bucket$srodek, rightmost.closed = TRUE,
					all.inside = TRUE)
			od <- bucket$srodek[przedzial]
			do <- bucket$srodek[przedzial + 1]
			wynik <- (x - od)/(do - od) * bucket$fitted[przedzial +
							1] + (do - x)/(do - od) * bucket$fitted[przedzial]
		}
		else {
			granice = sort(unique(c(bucket$od, bucket$do)))
			
			if (length(granice)==1)
				granice<-c(granice,granice);
			przedzial <- findInterval(x, granice, rightmost.closed = TRUE,
					all.inside = TRUE)
			wynik <- bucket$fitted[przedzial]
		}
	}
	wynik
}


#' Przypisuje zmienne dyskretne oraz ci�g�e w oparciu o podane przedzia�y, uwzgl�dnia warto�ci
#' specjalne
#' bo p�niej sortujemy i odnosimy si� tym porz�dkiem do pocz�tkowego porz�dku
#'   @param x Wektor zmiennych numerycznych. 
#'   @param bucket \code{data.frame} z bucketami.
#'   @param interpol Czy przeprowadzi� interpolacj�?
#'   @param NA_substit warto�� do podstawienia za brak danych
#' @seealso \code{\link{przypisz}}
#' @return  Wektor warto�ci.
#' @author Micha� Danaj
#' @export
# TODO  BUG! Je�li bucket wiersze nie b�d� posortowane, to b�d� b��dne wyniki,
# TODO Co� nie dzia�a obs�uga,  gdy w x s� NA, np. zmienna Wiek
przypisz2<-function(x, bucket, interpol=FALSE, fitted=NULL, NA_substit=-2147483647)
{
	
	if (!is.null(fitted))
		bucket$fitted<-as.vector(fitted);
	
	if (is.null(bucket$fitted))
		stop("Brak kolumny 'bucket$fitted'.")
	
	if (is.factor(bucket$fitted))
		warning("przypisz2: Uwaga! bucket$fitted jest typu factor, co prowadzi do dziwnych wynik�w!");
	
	#inicjuj� wektor z wynikami
	if (is.numeric(bucket$fitted))
		wynik<-rep(NA, length(x))
	else
		wynik<-rep("<NA>", length(x));
	#print(bucket)
	#Je�li buckety okre�lone s� przez warunki mapuj�ce
	jest_mapowanie=FALSE
	if (!is.null(bucket$mapping_war))
		if(any(!is.na(bucket$mapping_war)))
			jest_mapowanie=TRUE
	if (jest_mapowanie){
		mmm<-data.frame(war=bucket$mapping_war, label=bucket$fitted, discret=bucket$discret)
		mmm<-mmm[mmm$discret!="<TOTAL>",]
		wynik<-mapuj(x, mmm[,1:2])
	}
	else{
		#rozdzielam na warto�ci dyskretne i ci�g�e
		ciagle_buck<-bucket[!is.na(bucket$od),];
		#wybieram dyskretne warto�ci i usuwam totala
		dyskretne_buck<-bucket[is.na(bucket$od) & bucket$discret!="<TOTAL>",];
		
		#czy s� jakie� przedzia�y
		if (nrow(ciagle_buck)>0){
			if (interpol)
			{
				#obcinam zakresy do wartosci srodkow krancowych przedzialow
				x[x<min(ciagle_buck$srodek)]<-min(ciagle_buck$srodek);
				x[x>max(ciagle_buck$srodek)]<-max(ciagle_buck$srodek);
				
				przedzial<-findInterval(x, ciagle_buck$srodek, rightmost.closed = TRUE, all.inside = TRUE);
				
				od<-ciagle_buck$srodek[przedzial];
				do<-ciagle_buck$srodek[przedzial+1];
				wynik<-(x-od)/(do-od)*bucket$fitted[przedzial+1]+(do-x)/(do-od)*ciagle_buck$fitted[przedzial];
			}
			else
			{
				granice<-sort(unique(c(ciagle_buck$od, ciagle_buck$do)));
				if (length(granice)==1)
					granice<-c(granice, granice);
				przedzial<-findInterval(x, granice, rightmost.closed = TRUE, all.inside = TRUE);
				wynik<-ciagle_buck$fitted[przedzial];
			}
		}
		
		
		# Je�li bucket zdefiniowa� obs�ug� brak�w danych, to wszystkie braki podmieniam
		# warto�ci� specjaln� oznaczajac� brak danych
		if (any(rownames(bucket)== NA_substit))
			x[is.na(x)]<-NA_substit;
		
		## I jeszcze warto�ci dyskretne lub specjalne
		# gdzie s�
		spec_bool<- x %in% dyskretne_buck$discret;
		#jakie_sa
		spec_idx<- match(x, dyskretne_buck$discret);
		#nadpisuj�
		wynik[spec_bool]<- dyskretne_buck$fitted[na.omit(spec_idx)];
		
	}
	
	
	#sprawdzam, czy s� jakie� nieprzypisane warto�ci. Je�li tak, to rzucam ostrze�enie;
	if (is.numeric(bucket$fitted)){
		if (any(is.na(wynik)))
			warning("przypisz2: Nie wszystkie warto�ci zosta�y przypisane do bucketa. Pozosta�y braki danych.")	
	} 	else if(any(wynik=="<NA>"))
		warning("przypisz2: Nie wszystkie warto�ci zosta�y przypisane do bucketa. Pozosta�y braki danych,
						oznaczone jako <NA>'.")
	
	if (!is.numeric(wynik))
		wynik<-factor(wynik, levels=unique(bucket$fitted[bucket$discret!="<TOTAL>"]));
	
	wynik
}



#' Na podstawie zadanych warunk�w przypisuje etykiety
#' 
#' Na podstawie zadanych warunk�w w \code{mapping$war} przypisuje etykiet� \code{mapping$label}.
#' W \code{mapping$label} nie mo�e by� warto�ci pustych string�w. Pusty string stosowany jest jako
#' brak danych. Je�li dla jakie� elementu �aden warunek nie zachodzi, zwracany jest pusty string.    
#'
#' Mo�liwe jest okre�lenie warunku \code{else} poprzez wpisanie w \code{mapping$war} stringa "else".
#'
#' @param data \code{data.frame} do kt�rego b�dzie przypisywane mapowanie
#' @param maping   \code{data.frame} z dwoma kolumnami znakowymi (lub do przerobienia przez \code{as.character})
#'			  \code{war} oraz \code{label}
#' @author Micha� Danaj
#' @export
mapuj<-function(data, mapping){
	
	
	#je�li nie znakowe, to przerabiam na znakowe
	if (!is.character((mapping$war)))
		mapping$war<-as.character(mapping$war)
	if (!is.character((mapping$label)))
		mapping$label<-as.character(mapping$label)	
	if (!is.data.frame(data))
		stop("Funkcja mapuj: Dane musz� by� typu data.frame, z kolumnami wykorzystywanymi w warunkach mapuj�cych!")
	
	wynik<-rep("", nrow(data))
	
	gdzie_else<-which(mapping$war=='else')
	
	#je�li jest else, to go wydzielam
	if (length(gdzie_else>0)){
		lab_else<-mapping$label[gdzie_else]
		mapping<-mapping[-gdzie_else,]
	}
	
	text=sprintf("with(data,%s)",mapping$war)	
	for (i in 1:nrow(mapping)){	
		
		war<-eval(parse(text=text[i]))		
		wynik[war]<-mapping$label[i]
	}
	
	#je�li jest else, to go stosuj�
	if (length(gdzie_else)>0)
		wynik[wynik==""]<-lab_else
	
	#sprawdzam, czy co� si� nie przypisa�o
	if (any(wynik==""))
		warning("mapuj: Nie wszystkie warto�ci zosta�y przypisane.")
	
	return(wynik)
}



#' ��czy ze sob� buckety
#' 
#' ��czy buckety. W przypadku bucket�w z przedzia�ami zmiennej ci�g�ej, mo�liwe jest 
#' po��czenie tylko
#' przedzia��w przlegaj�cych do siebie. Dla nowo powsta�ych bucket�w wylicza statystyki. 
#' W przypadku ��czenia przedzia��w zmiennej ci�g�ej, wiersze z tymi bucketami zostan� ze sob�
#' po��czone i powstanie \{data.frame} z liczb� wierszy o jeden mniejsz� ni� w \code{bucket}.
#' Przy ��czeniu bucket�w dyskretnych lub dyskretnego i ci�g�ego, wiersze nie zostan� usuni�te. 
#' Zostanie im nadany wsp�lny label oraz wsp�lny numer. Je�li liczba bucket�w do po��czenia jest<=1,
#' zwracany jest wej�ciowy podzia�. 
#' @param x zmienna score'owa.
#' @param y zmienna odpowiedzi z zakresu [0,1]. Np. default, LGD.
#' @param buckets buckety.
#' @param nr1 numer wiersza w \{buckets} z pierwszym bucketem do po��czenia.
#' @param nr2 numer wiersza w \{buckets} z pierwszym bucketem do po��czenia.
#' @param new_label label jaki b�dzie nadany po��czonym bucketom. W przypadku braku podania, zostanie zatosowany domy�lny.
#' @returnType \code{data.frame}.
#' @author Micha� Danaj
#' @export
polacz_buckety<-function(x, y, buckets, row_idxs, new_label=NULL)
{
	row_idxs<-sort(unique(row_idxs));
	#sprawdzam, czy jest co ��czy�
	if (length(row_idxs)<=1)
		return(buckets)
	#sprawdzam, czy nie wyszli�my poza zakres
	if(min(row_idxs)<1 | max(row_idxs)>nrow(buckets)-1) 
		stop("Numery wierszy s� poza zakresem zmiennej 'buckets'");
	#sprawdzam, czy nie ma ju� takiego labela w innych wierszach
	if (!is.null(new_label) & any( buckets$label[-row_idxs]==new_label))
		warning("Podana warto�� 'new_label' znajduje si� ju� w 'buckets' w wierszu innym ni� aktualnie ��czone wiersze.")
	for (i in 1:(length(row_idxs)-1)){
		nr1<-row_idxs[i];
		nr2<-row_idxs[i+1];
		#je�li ��czymy dane ci�g�e
		if (all((!is.na(buckets$od) & !is.na(buckets$do))[c(nr1,nr1)])){
			if (nr2-nr1!=1)
				stop("B��dnie podane wiersze do po��czenia! Przedzia�y powinny by� do siebie przyleg�e!")
			#je�li label nie podany, to go tworz�
			if (is.null(new_label)){
				new_label<-strsplit(buckets$label[nr1],',')[[1]][1]
				new_label<-paste(new_label,strsplit(buckets$label[nr2],',')[[1]][2],sep=',')
			}
			#jeszcze przepisuj� kra�ce przedzia��w i inne warto�ci nie wyliczane w buckety_stat a
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