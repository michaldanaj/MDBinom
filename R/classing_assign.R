# Funkcje zaczynaj¹ce siê na univariate
# 
# Author: Piotr
###############################################################################


# TODO  BUG! Jeœli bucket wiersze nie bêd¹ posortowane, to bêd¹ b³êdne wyniki, bo póŸniej
# sortujemy i odnosimy siê tym porz¹dkiem do pocz¹tkowego porz¹dku
# TODO obs³uga jednego przedzia³u dla interpolacji
#' Przypisuje wartoœci zdefiowane w \code{bucket} 
#' 
#' Znajduje przedzia³ opisany w \code{bucket}, do którego nale¿y \code{x} i 
#' przypisuje wartoœæ z \code{bucket_fitted}. Ewentualnie wykonuje interpolacjê.
#'
#' Jeœli \code{interpor=TRUE}, wykonuje interpolacjê, której wêz³ami s¹ 
#' œrodki przedzia³ów, pomiêdzy którymi le¿y \code{x}, a wartoœciami
#' \code{bucket$fitted} odpowiadaj¹ce tym œrodkom.  
#' 
#' @param x Wektor zmiennych numerycznych. 
#' @param bucket \code{data.frame} z kolumnami:
#'		\itemize{
#' 			\item od. Dolny kraniec przedzia³u.
#' 			\item do Górny kreniec przedzia³u.
#' 			\item srodek Œrodek przedzia³u.
#' 			\item fitted Wartoœæ przypisana do danego przedzia³u.
#' 		}
#' @param interpol Czy przeprowadziæ interpolacjê?
#' 
#' @return  Wektor wartoœci.
#' 
#' @author Micha³ Danaj
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
#' 
#' @export

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


# TODO  BUG! Jeœli bucket wiersze nie bêd¹ posortowane, to bêd¹ b³êdne wyniki,
# TODO Coœ nie dzia³a obs³uga,  gdy w x s¹ NA, np. zmienna Wiek
#' Przypisuje dyskretyzacjê
#' 
#' Przypisuje zmienne dyskretne oraz ci¹g³e w oparciu o podane przedzia³y, uwzglêdnia wartoœci
#' specjalne
#' @param x Wektor zmiennych numerycznych. 
#' @param bucket \code{data.frame} z bucketami.
#' @param interpol Czy przeprowadziæ interpolacjê?
#' @param fitted Nadpisuje wektor \code{bucket$fitted} i na tej podstawie przypisuje wartoœci.  
#' @param NA_substit wartoœæ do podstawienia za brak danych
#' @seealso \code{\link{przypisz}}
#' @return  Wektor wartoœci.
#' @author Micha³ Danaj
#' @export
przypisz2<-function(x, bucket, interpol=FALSE, fitted=NULL, NA_substit=-2147483647)
{
	
	if (!is.null(fitted))
		bucket$fitted<-as.vector(fitted);
	
	if (is.null(bucket$fitted) & is.null(fitted))
		stop("Brak kolumny 'bucket$fitted' oraz parametru 'fitted'. Przynajmniej jeden powinien byæ uszupe³niony.")
	
	if (is.factor(bucket$fitted))
		warning("przypisz2: Uwaga! bucket$fitted jest typu factor, co prowadzi do dziwnych wyników!");
	
	#inicjujê wektor z wynikami
	if (is.numeric(bucket$fitted))
		wynik<-rep(NA, length(x))
	else
		wynik<-rep("<NA>", length(x));
	#print(bucket)
	#Jeœli buckety okreœlone s¹ przez warunki mapuj¹ce
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
		#rozdzielam na wartoœci dyskretne i ci¹g³e
		ciagle_buck<-bucket[!is.na(bucket$od),];
		#wybieram dyskretne wartoœci i usuwam totala
		dyskretne_buck<-bucket[is.na(bucket$od) & bucket$discret!="<TOTAL>",];
		
		#czy s¹ jakieœ przedzia³y
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
		
		
		# Jeœli bucket zdefiniowa³ obs³ugê braków danych, to wszystkie braki podmieniam
		# wartoœci¹ specjaln¹ oznaczajac¹ brak danych
		if (any(rownames(bucket)== NA_substit))
			x[is.na(x)]<-NA_substit;
		
		## I jeszcze wartoœci dyskretne lub specjalne
		# gdzie s¹
		spec_bool<- x %in% dyskretne_buck$discret;
		#jakie_sa
		spec_idx<- match(x, dyskretne_buck$discret);
		#nadpisujê
		wynik[spec_bool]<- dyskretne_buck$fitted[na.omit(spec_idx)];
		
	}
	
	
	#sprawdzam, czy s¹ jakieœ nieprzypisane wartoœci. Jeœli tak, to rzucam ostrze¿enie;
	if (is.numeric(bucket$fitted)){
		if (any(is.na(wynik)))
			warning("przypisz2: Nie wszystkie wartoœci zosta³y przypisane do bucketa. Pozosta³y braki danych.")	
	} 	else if(any(wynik=="<NA>"))
		warning("przypisz2: Nie wszystkie wartoœci zosta³y przypisane do bucketa. Pozosta³y braki danych,
						oznaczone jako <NA>'.")
	
	if (!is.numeric(wynik))
		wynik<-factor(wynik, levels=unique(bucket$fitted[bucket$discret!="<TOTAL>"]));
	
	wynik
}



#' Na podstawie zadanych warunków przypisuje etykiety
#' 
#' Na podstawie zadanych warunków w \code{mapping$war} przypisuje etykietê \code{mapping$label}.
#' W \code{mapping$label} nie mo¿e byæ wartoœci pustych stringów. Pusty string stosowany jest jako
#' brak danych. Jeœli dla jakieœ elementu ¿aden warunek nie zachodzi, zwracany jest pusty string.    
#'
#' Mo¿liwe jest okreœlenie warunku \code{else} poprzez wpisanie w \code{mapping$war} stringa "else".
#'
#' @param data \code{data.frame} do którego bêdzie przypisywane mapowanie
#' @param mapping   \code{data.frame} z dwoma kolumnami znakowymi (lub do przerobienia przez \code{as.character})
#'			  \code{war} oraz \code{label}
#' @author Micha³ Danaj
#' @export
mapuj<-function(data, mapping){
	
	
	#jeœli nie znakowe, to przerabiam na znakowe
	if (!is.character((mapping$war)))
		mapping$war<-as.character(mapping$war)
	if (!is.character((mapping$label)))
		mapping$label<-as.character(mapping$label)	
	if (!is.data.frame(data))
		stop("Funkcja mapuj: Dane musz¹ byæ typu data.frame, z kolumnami wykorzystywanymi w warunkach mapuj¹cych!")
	
	wynik<-rep("", nrow(data))
	
	gdzie_else<-which(mapping$war=='else')
	
	#jeœli jest else, to go wydzielam
	if (length(gdzie_else>0)){
		lab_else<-mapping$label[gdzie_else]
		mapping<-mapping[-gdzie_else,]
	}
	
	text=sprintf("with(data,%s)",mapping$war)	
	for (i in 1:nrow(mapping)){	
		
		war<-eval(parse(text=text[i]))		
		wynik[war]<-mapping$label[i]
	}
	
	#jeœli jest else, to go stosujê
	if (length(gdzie_else)>0)
		wynik[wynik==""]<-lab_else
	
	#sprawdzam, czy coœ siê nie przypisa³o
	if (any(wynik==""))
		warning("mapuj: Nie wszystkie wartoœci zosta³y przypisane.")
	
	return(wynik)
}

