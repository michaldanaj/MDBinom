# TODO: Add comment
# 
# Author: Piotr
###############################################################################



#' Rysuje interakcje dwóch zmiennych
#' @param zm1 Pierwsza zmienna.
#' @param zm2 Druga zmienna.
#' @param target Zmienna celu. 
#' @return 
#' 
#' @author Micha³ Danaj
#' @export
interakcja<-function(zm1, zm2, target){
	target<-tapply(target, list(zm1,zm2), mean)
	logit<-log(dwa/(1-dwa))
	#jeden<-jeden[-grep('-999', rownames(jeden)),];
	#jeden<-jeden[,-grep('-999', colnames(jeden))];
	mfrow_oryg<-par('mfrow')
	par(mfrow=c(2,1))
	matplot(target, type=rep('b', ncol(target)))
	matplot(logit, type=rep('b', ncol(target)))
	par(mfrow=mfrow_oryg)
	dwa
}


#' Testuje interakcje miêdzy dwoma zmiennymi
#' 
#' Funkcja zak³ada, ¿e \code{var1} oraz \code{var2} s¹ zmiennymi dyskretnymi. Konwertowane s¹ one do
#' typu \code{factor} i sprawdzany jest p-value modelu z interakcjami w stosunku do modelu bez interakcji.
#' Wykorzystany jest test Chi-kwadrat odchylenia modelu.
#' 
#' @param var1 wektor z wartoœciami pierwszej zmiennej.
#' @param var2 wektor z wartoœciami drugiej zmiennej.
#' @param target wektor z wartoœciami zmiennej celu.
#' @return Zwraca listê z szeregiem statystyk:
#' \item{"p-value"}{p-value interakcji} 
#' \item{licznosc}{licznoœci obserwacji na przeciêciach zmiennych}
#' \item{licznosc_target}{licznoœci targetu na przeciêciach zmiennych}
#' \item{mean_real}{œrednia wartoœæ targetu  na przeciêciach zmiennych}
#' \item{mean_pred1}{predykcja z modelu bez interakcji na przeciêciach zmiennych} 
#' \item{mean_pred2}{precykcja z modelu z interakcjami na przeciêciach zmiennych} 
#' 
#' @author Micha³ Danaj
#' @export
interactionsTest<-function(var1, var2, target){
	
	## tworzy factory
	var1 <- factor(var1)
	var2 <- factor(var2)
	
	## buduje modele logistyczne i sprawdza p-value interakcji
	model1<-glm(target~var1+var2, family='binomial')
	model2<-glm(target~var1+var2 + var1:var2, family='binomial')
	
	#summary(model2)
	test <- anova(model2, test='Chisq')
	
	pv <- test$`Pr(>Chi)`[4]
	
	## licznoœci na przeciêciach wartoœci zmiennych
	licznosc <- table(var1, var2)
	licznosc_target <- tapply(target, list(var1,var2), sum)
	
	## œrednie wartoœci targetu
	mean_real <- tapply(target, list(var1, var2), mean)
	
	## predykcja modelu bez interakcji
	pred1 <- predict(model1, type='response')
	mean_pred1 <- tapply(pred1, list(var1,var2), mean)
	
	## predykcjam odelu z interakcjami
	pred2 <- predict(model2, type='response')
	mean_pred2 <- tapply(pred2, list(var1,var2), mean)
	
	## return
	list(	"p-value"=pv, 
			licznosc=licznosc, 
			licznosc_target=licznosc_target, 
			mean_real=mean_real, 
			mean_pred1=mean_pred1, 
			mean_pred2=mean_pred2
	)
}



#' Testuje interakcje pomiêdzy wszystkimi zmiennymi
#' 
#'  Na podstawie zmiennych Ÿród³owych oraz definicji
#' dyskretyzacji z \code{discr_list}, funkcja przypisuje zmiennym ich wartoœci dyskretne oraz
#' testuje interakcjê ka¿dej pary ze zmiennych okreœlonych w \code{vars_names}. Do testowania korzysta z funkcji \code{\link{interactionsTest}}. 
#' @param data \code{data.frame} z danymi.
#' @param discr_list lista z opisem dyskretyzacji.
#' @param target wektor z wartoœciami zmiennej celu
#' @param vars_names wektor nazw zmiennych do sprawdzenia interakcji. Domyœlnie wszystkie zmienne z \code{discr_list}. 
#' @param colname nazwa kolumny z tabeli z definicj¹ dyskretyzacji z której zostan¹ pobrane wartoœci do
#' 	przypisania dyskretyzacji zmiennej. 
#' @return Zwrócona zostanie lista z dwoma elementami:
#' \item{pvalue_table}{Zawiera \code{data.frame} z nazw¹ pierwszej zmiennej, drugiej zmiennej, oraz p-value interakcji.}
#' \item{details}{Zawiera listê ze statystykami zwróconymi przez \code{\link{interactionsTest}}.} 
#' @author Micha³ Danaj
#' @export
interactionsTestAll <- function(data, discr_list, target=data$target, vars_names=names(discr_list), colname='label'){
	
	new_vars<-przypisz_z_listy(bucket_list = discr_list
			, data = data
			, vars = vars_names
			, colname=colname
			, varname_sufix=''
			, sep_sufix='')
	
	vars_names2 <- names(new_vars)
	interact_count <- length(vars_names2)*(length(vars_names2)-1)/2
	
	interact_results <- list()
	interact_table <- data.frame(var1 = rep('',interact_count),
			var2 = rep('',interact_count),
			pvalue = rep(NA, interact_count),
			stringsAsFactors = FALSE
	)
	counter <- 0
	for (i in 1:(length(vars_names2)-1))
		for (j in (i+1):length(vars_names2)){
			
			counter <- counter+1
			var1_name <- vars_names2[i]
			var2_name <- vars_names2[j]
			
			cat('\n-----------------------------------------------\n\n')
			print(paste( counter , "/", interact_count))
			print(paste('Interakcja:', var1_name, var2_name))
			var1<-new_vars[,var1_name]
			var2<-new_vars[,var2_name]
			
			element_name<-paste(var1_name, var2_name, sep='__')
			interact_results[[element_name]] <- interactionsTest(var1, var2, target)
			
			#attributes(interact_results[[element_name]])<-list(var1=var1_name, var2=var2_name)
			
			interact_table[counter,]=c(var1_name, var2_name, interact_results[[element_name]]$'p-value')
		}
	
	list(pvalue_table=interact_table,
			details=interact_results)
}



