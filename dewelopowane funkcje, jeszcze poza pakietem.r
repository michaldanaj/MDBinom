buckety_stat_wtd<-function(bucket, default, weights=rep(1,length(bucket)), total=TRUE)
{

#	dane<-as.data.frame(cbind(score, default));

	#usuwam puste levele factorów
	if (is.factor(bucket))
		bucket<-factor(bucket);

	obs<- wtd.table(bucket, weights=weights);
	obs_all<-sum(weights);
	bad<- as.vector(tapply(default, bucket, wtd.sum, weights=weights));
	bad_all<-wtd.sum(bad, weights=weights);
	br<- as.vector(tapply(default, bucket, wtd.mean, , weights=weights));
	std_dev<-sqrt(as.vector(tapply(default, bucket, wtd.var, weights=weights)));
	logit<-log(br/(1-br));
	probit<-qnorm(br);

	#do_wagi<-bad;
	#do_wagi[do_wagi==0]<-0.5;
	#do_wagi[do_wagi==obs]<-obs[do_wagi==obs]-0.5;
	#waga<-obs/((do_wagi)/(obs)*(1-(do_wagi)/(obs)));
	waga<-NA;

	pct_good=(obs-bad)/(obs_all-bad_all);
	pct_bad=bad/bad_all;
	woe=log(pct_good/pct_bad)

	wynik<-as.data.frame(cbind( nr=1:length(obs),
											 n_good=(obs-bad), pct_good,
									     n_bad=bad, pct_bad,
											 n_obs=obs, pct_obs=obs/obs_all,
											 gb_odds=(obs-bad)/bad,
										   br, std_dev, woe, logit, probit, var=br*(1-br)/obs, waga))

  rownames(wynik)<-sort(unique(bucket));

	if (total) {
			wynik<-rbind(wynik, TOTAL=c(length(obs)+1, obs_all-bad_all, 1, bad_all, 1, obs_all,1,
														(obs_all-bad_all)/bad_all, bad_all/obs_all, sd(default), 0, NA, NA, NA, NA));
  }
	return(wynik);
}


plot_AR_dev<-function(ar, plot_type=c("ROC", "CAP"),...)
{
	plot_type<-match.arg(plot_type);

	plot(c(0,1), c(0,1), type="l", lty=1, col=1, xlab="", ylab="",...);
	if (plot_type=="CAP"){
		br<-ar[[1]]$stats["br"];
		lines(c(0,br,1),c(0,1,1), col=1);
	}
	AR<-rep(-1,length(ar));
	for (i in 1:length(ar))
	{
		if (plot_type=="CAP")
			lines(c(0,ar[[i]]$table$cum_pct_obs), c(0,ar[[i]]$table$cum_pct_bad), type="l", lty=1, col=i+1)
		else
			lines(c(0,ar[[i]]$table$cum_pct_good), c(0,ar[[i]]$table$cum_pct_bad), type="l", lty=1, col=i+1);
		AR[i]<-ar[[i]]$stats["AR"];
	}
	kolej<-order(AR);
	AR<-round(AR*100,2);

	legend(1,0,paste(names(ar)[kolej], " (", AR[kolej], "%)", sep="" ),lty=1, 
		 col=kolej+1, cex=0.8, xjust=1, yjust=0 );
}



n<-1000;
x<-rnorm(n);
y<-as.numeric(x+rnorm(n)<0);

drzewo(x, y)
traceback()

drzewo<-function(score, def, freq=NULL, wytnij=0.01, min_split=30, min_bucket=10, max_gleb=4, n_buckets=20, plot=c(TRUE,FALSE))
{

	#musze teraz wywolac buckety_br, bo pozniej agreguje wektory
	#do jednego scoru i wykorzystuje freq, a buckety_br musi miec
	#jeden rekord=jedna obserwacja
	#if (plot)
	#	b<-buckety_br(score , def, n_buckets, method="eq_lengt");

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
		score<-score[-usun];
		def<-def[-usun];
		freq<-freq[-usun];
	}
	
	def_a<-tapply(def,score,sum);
	freq_a<-tapply(freq, score, sum);
	score_a<-unique(score);
	
	#vec_stats(score_a);
	w<-drzewo_podzial(score_a, def_a, 1, min(score), max(score), freq_a, 0, min_split, min_bucket, max_gleb);
	
	#wybieram liscie
	w<-w[is.na(w$poprawa),];

	#przydzielam liœcie
	temp<-c(od=w$od, do=w$do, srodek=(w$od +w$do)/2, fitted=w$nr_wezla);
  liscie<-przypisz(score, temp);
	
	#i robie dla nich statystyki
	bucket<-buckety_stat(liscie, def, total=FALSE);
	bucket$fitted<-bucket$br;

	#	----- rysowanie -----
	if (plot)
	{

		plot(bucket$srodek, bucket$br, xlab="", ylab="");
		for (i in 1:length(w$od))
			lines(c(temp$od[i], temp$do[i]), c(bucket$fitted[i],bucket$fitted[i]), col="blue");
	}

	bucket
}

drzewo_podzial(x,y,1,-3,2, rep(1,n), 1);

drzewo_podzial<-function(score, def, nr_wezla, od, do, freq, glebokosc, min_split=200, min_bucket=100, max_gleb=3)
{
	#print("#####################");
	#print(nr_wezla);print(score);print(def);print(od);print(do);print(freq);print(glebokosc);
	all_obs<-sum(freq);
	all_bad<-sum(def);
	br_akt<-all_bad/all_obs;
	gini_akt<-br_akt*(1-br_akt)*all_obs;
	wezel<-data.frame(nr_wezla, rodzic=floor(nr_wezla/2), od, do, n_obs=all_obs, n_bad=all_bad, br=br_akt, poprawa=NA, podzial=NA);
	
	wynik<-wezel;

	#jesli ilosc obserwacji jest wystarczajaca, aby zrobic podzial w wezle
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
			wl<-drzewo_podzial(score[1:nr], def[1:nr], nr_wezla*2, od, wezel$podzial, freq[1:nr], glebokosc+1, min_split, min_bucket, max_gleb);
			wr<-drzewo_podzial(score[(nr+1):l], def[(nr+1):l], nr_wezla*2+1, wezel$podzial, do, freq[(nr+1):l], glebokosc+1, min_split, min_bucket, max_gleb);
			wynik<-rbind(wezel,wl,wr);
		}
	}
	
	wynik
}


wtd.reg_nieparam<-function (score, default, buckets = 100, subset=NULL, wytnij = 0, span = 0.7,
		degree = 2, plot = TRUE, target = "br", new = TRUE, col_points = "black",
		col_line = "darkblue", index = FALSE, weights=NULL, estim=NULL, ...)
{
	dane <- data.frame(score, default)
	
	if (is.null(weights)){
		weights=rep(1,length(score))
		weighst_locfit=weights	
	}
	else{
		
		# TODO zmieniæ to!
		# na cele tego zadania przyhardkorzy³em i obchodzê b³¹d locfita w ten sposób,
		# ¿e ustalam wagê 1 dla obserwacji z target=1
		
		waga_1<-weights[which(default==1)[1]]
		weights_locfit<-weights/waga_1
	}
	
	#jeœli okreœlono subset, to ograniczam dane na których pracujemy 
	if (!is.null(subset)){
		dane<-dane[subset,]
		weights<-weigths[subset]
		estim<-estim[subset]
	}
	
	if (wytnij > 0){
		do_usuniecia<-usun_konce(dane$score, prob = wytnij, weights=weights);
		if (length(do_usuniecia)>0)
			dane <- dane[-do_usuniecia,]
	}
	bucket <- buckety_br(x=dane$score, y=dane$default, n=buckets, method = "eq_count", weights=weights)
	
	
	
	#jeœli s¹ dwie wartoœci y, to uznaje to jest to zmienna binarna, i stosuje regresjê logistyczn¹
	if (length(unique(default)) == 2)
		l <- locfit(default ~ lp(score, nn = span), family = "binomial",
				link = "logit", data = dane, weights=weights_locfit)
	#w przeciwnym razie robi regresje liniow¹
	else 
		l <- locfit(default ~ lp(score, nn = span), data = dane, weights=weights)
	
	b2 <- predict(l, newdata = bucket$srodek)
	if (target == "br")
		bucket2 <- cbind(bucket, fitted = b2)
	else bucket2 <- cbind(bucket, fitted = log(b2/(1 - b2)))
	
	#liczê wielkoœæ b¹belka
	skala <- sqrt(bucket$n_obs/(sum(weights)/buckets))
	
	#liczê wartoœci wyestymowane
	#estim_aggr<-buckety_stat(b2, default, )
	
	#rysowanie
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



usun_konce<-function(score, prob=0.01)
{
	l=quantile(score, prob=prob);
	p=quantile(score, prob=(1-prob));
	
	#sprawdzam, w taki lewy sposob, czy zmienna nie jest ograniczona z dolu
	#z bledem +-1 obserwacja

	do_usuniecia<-rep(FALSE,times=length(score));
	if ( abs(length(score[score<=l])-length(score)*prob)<=1 )
			do_usuniecia<-(score<=l);

	if ( abs(length(score[score>=p])-length(score)*prob)<=1 )
		do_usuniecia<-do_usuniecia | (score>=p);

	c(1:length(score))[do_usuniecia]
}


