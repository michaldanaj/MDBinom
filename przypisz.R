# TODO: Add comment
# 
# Author: Piotr
###############################################################################


przypisz2<-function(x, bucket, interpol=FALSE, NA_substit=-10000000)
{
	if (is.null(bucket$fitted))
		stop("Brak kolumny 'bucket$fitted'.")
	
	if (is.factor(bucket$fitted))
		warning("przypisz2: Uwaga! bucket$fitted jest typu factor, co prowadzi do dziwnych wyników!");
	
	#rozdzielam na wartoœci dyskretne i ci¹g³e
	ciagle_buck<-bucket[!is.na(bucket$od),];
	#wybieram dyskretne wartoœci i usuwam totala
	dyskretne_buck<-bucket[is.na(bucket$od) & bucket$discret!="<TOTAL>",];
	if (is.numeric(bucket$fitted))
		wynik<-rep(NA, length(x))
	else
		wynik<-rep("<NA>", length(x));
	
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
	
	# Teraz NA
	if (any(rownames(bucket)== NA_substit))
		wynik[is.na(x)]<-bucket[rownames(bucket)==NA_substit,"fitted"];
	
	## I jeszcze wartoœci dyskretne lub specjalne
	# gdzie s¹
	spec_bool<- x %in% dyskretne_buck$discret;
	#jakie_sa
	spec_idx<- match(x, dyskretne_buck$discret);
	#nadpisujê
	wynik[spec_bool]<- dyskretne_buck$fitted[na.omit(spec_idx)];
	
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
