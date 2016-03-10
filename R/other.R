# TODO: Add comment
# 
# Author: Piotr
###############################################################################




#' Rysuje kalibracjê modelu
#' 
#' Rysuje kalibracjê modelu
#' @param score Zmienna na osi OX.
#' @param response Zmienna celu.
#' @param estim Wyestymowana wartoœæ przez model.
#' @param target Co ma byæ na osi OY.
#' @param ylab Opis osi OY.
#' @param xlab Opis osi OX.
#' @param ... Parametry graficzne.
#' @return 
#' 
#' @author Piotr
#' @export
plotCalibr<-
		function (score, response, estim, target = c("br", "logit"), ylab=y_name,
				xlab="score",    ...)
{
	target <- match.arg(target)
	if (!is.list(estim))
		estim <- list(estim)
	y_name <- "PD"
	if (target == "logit")
		y_name <- "logit"
	buck <- reg_nieparam(score, response, target = target, ylab = ylab,
			xlab = xlab, ...)
	grupy_skala <- data.frame(od = buck$od, do = buck$do, fitted = buck$nr)
	grupy <- przypisz(score, grupy_skala)
	kolejnosc <- order(score)
	for (i in 1:length(estim)) {
		estim_grp <- tapply(estim[[i]], grupy, mean)
		buck <- cbind(buck, estim_grp)
		names(buck) <- c(names(buck)[1:(ncol(buck) - 1)], paste(names(estim[i]),
						"br", sep = "_"))
		if (target == "logit") {
			estim[[i]] <- logit(estim[[i]])
			buck <- cbind(buck, logit(estim_grp))
			names(buck) <- c(names(buck)[1:(ncol(buck) - 1)],
					paste(names(estim[i]), "logit", sep = "_"))
		}
		lines(score[kolejnosc], estim[[i]][kolejnosc], col = i +
						1, lty = i + 1, pch = i + 19)
	}
	legend(x = "topright", col = 1:(length(estim) + 1), lty = 1:(length(estim) +
						1), legend = c("Dependence from data", names(estim)),
			bty = "n", inset = 0.05)
	return(buck)
}




#' Generuje tabelê z coarse classing do dokumentacji
#' 
#' Jeœli Woe lub IV wychoch¹ +-Inf, to wstawia zamiast tego NA
#' @param wyniki lista z wynikami dyskretyzacji
#' @export 
makeCoarseClassingTables<-function(wyniki){
	lapply(wyniki, function(z){
				x<-z$dyskretyzacja;
				last_row<-nrow(x);
				x$GB_ODDS<-x$n_good/x$n_bad;
				gb_odds_total<-x$GB_ODDS[last_row];
				
				temp<-x$GB_ODDS/gb_odds_total;
				x$GB_index<-temp;
				
				print(x$GB_ODDS)
				x$GB_index[na.subst(x$GB_ODDS>gb_odds_total,FALSE)]<-paste(round(temp[na.subst(x$GB_ODDS>gb_odds_total,FALSE)]*100), 'G', sep='')
				x$GB_index[na.subst(x$GB_ODDS<=gb_odds_total, FALSE)]<-paste(round(temp[na.subst(x$GB_ODDS<=gb_odds_total, FALSE)]*100), 'B', sep='')
				
				x$IV<-(x$pct_good-x$pct_bad)*x$woe;
				
				#zamienia nieskoñczonoœci na braki danych
				x$IV[abs(x$IV)==Inf]<-NA;
				x$woe[abs(x$woe)==Inf]<-NA;
				
				x$IV[last_row]<-sum(x$IV[-last_row], na.rm=TRUE);
				x<-x[,c('label','n_obs','pct_obs','n_good','pct_good','n_bad','pct_bad','GB_ODDS','GB_index',
								'br','woe', 'IV')]
				x$discret[x$discret=='']<-x$label[x$discret==''];
				names(x)<-c('Coarse Classes','# Applicants','% Applicants',
						'# Good','% Good','# Bad','% Bad','GB Odds','GB Index',
						'LGD','WoE', 'IV')
				x<-unique(x);
				return(x);
			})
}




#' Rysuje interakcje dwóch zmiennych
#' @param zm1 Pierwsza zmienna.
#' @param zm2 Druga zmienna.
#' @param target Zmienna celu. 
#' @return 
#' 
#' @author Piotr
#' @export
interakcja<-function(zm1, zm2, target){
	dwa<-tapply(target, list(zm1,zm2), mean)
	jeden<-log(dwa/(1-dwa))
	#jeden<-jeden[-grep('-999', rownames(jeden)),];
	#jeden<-jeden[,-grep('-999', colnames(jeden))];
	mfrow_oryg<-par('mfrow')
	par(mfrow=c(2,1))
	matplot(dwa, type=rep('b', ncol(jeden)))
	matplot(jeden, type=rep('b', ncol(jeden)))
	par(mfrow=mfrow_oryg)
	dwa
}


#' Przypisuje woe na podstawie br z bucketa.
#' 
#' Przypisuje woe na podstawie br z bucketa. Korzysta z funkcji \code{woe}, która 
#' w razie niewyst¹pienia w buckecie wartoœci z klasy 0 lub 1, przyjmuje wartoœæ 0.5
#' 
#' @param bucket_list - lista z opisami dyskretyzacji. Nazwy elementów listy powinny byæ zgodne z nazwami zmiennych w \code{data}.
#' @param data - \code{data.frame} z oryginalnymi zmiennymi.
#' @param vars - wektor z nazwami zmiennych do ograniczenia.
#' @param varname_sufix - Sufix wynikowych zmiennych.
#' @export
przypisz_woe_z_listy<-function(bucket_list, data, vars=names(bucket_list), varname_sufix='woe'){
	
	data_out<-NULL
	
	for (zmienna in names(bucket_list)){
		
		if (!(zmienna %in% vars)) 
			next;
		
		####   wyliczam   woe    ######
		
		#Wyci¹gam element listy
		bucket<-bucket_list[[zmienna]]
		
		#jeœli badów lub goodów jest 0, to przyjmujê ¿e jest 0.5	
		
		pct_good = (pmax(bucket$n_good,0.5))/(max(bucket['TOTAL','n_good'],0.5))
		pct_bad = (pmax(bucket$n_bad,0.5))/(max(bucket['TOTAL','n_bad'],0.5))
		woe = log(pct_good/pct_bad)
		
		
		####   przypisujê woe    ######
		
		woe<-przypisz2(data[,zmienna],
				bucket_list[[zmienna]], 
				fitted=woe,
				NA_subst = numeric_var_treatment.params$NA_substit,
				interpol=FALSE)
		
		if (is.null(data_out))	{
			data_out <- data.frame(woe)
			names(data_out)<-zmienna
		}
		else
			data_out[,zmienna] <- c(woe)
		
	}
	
	names(data_out)<-paste(names(data_out), varname_sufix, sep="_")
	data_out
}	


przypisz_z_listy<-function(bucket_list, data, vars=names(bucket_list), colname='fitted', varname_sufix=colname){
	
	data_out<-NULL
	
	for (zmienna in names(bucket_list)){
		
		if (!(zmienna %in% vars)) 
			next;
		
		####   wyliczam   woe    ######
		
		#Wyci¹gam element listy
		bucket<-bucket_list[[zmienna]]
		#bucket<-bucket[rownames(bucket)!='TOTAL',]
		
		#jeœli badów lub goodów jest 0, to przyjmujê ¿e jest 0.5	
		
		fitted = bucket[,colname]
		
		####   przypisujê woe    ######
		
		fitted_x<-przypisz2(data[,zmienna],
				bucket_list[[zmienna]], 
				fitted=fitted,
				NA_subst = numeric_var_treatment.params$NA_substit,
				interpol=FALSE)
		
		if (is.null(data_out))	{
			data_out <- data.frame(fitted_x)
			names(data_out)<-zmienna
		}
		else
			data_out[,zmienna] <- c(fitted_x)
		
	}
	
	names(data_out)<-paste(names(data_out), varname_sufix, sep="_")
	data_out
}	




korelacje_zmiennych<-function(model, data){
	zmienne<-names(coef(model))[-1]
	edit(cor(data[,zmienne]))
}

