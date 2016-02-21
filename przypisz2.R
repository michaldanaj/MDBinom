# TODO: Add comment
# 
# Author: Piotr
###############################################################################


#TODO! Coœ nie dzia³a obs³uga,  gdy w x s¹ NA
#np. zmienna Wiek
przypisz2<-function(x, bucket, interpol=FALSE, fitted=NULL, NA_substit=-2147483647)
{
	
	if (!is.null(fitted))
		bucket$fitted<-as.vector(fitted);
	
	if (is.null(bucket$fitted))
		stop("Brak kolumny 'bucket$fitted'.")
	
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
