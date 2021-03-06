% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classing_make.R
\name{bckt_stat}
\alias{bckt_stat}
\title{Wylicza statystyki zmiennej \code{y} dla podanych grup \code{x} (dev)}
\usage{
bckt_stat(
  x = NULL,
  y = NULL,
  weights = rep(1, length(x)),
  dt = NULL,
  avg = NULL,
  total = TRUE,
  sort_x = TRUE
)
}
\arguments{
\item{x}{wektor z identyfikatorami bucket?w.Nie mo?e by? brak?w danych.}

\item{y}{wektor zmiennych numerycznych. Z za?o?enia zmienna dwumianowa, ale statystyki
zostan? policzone dla ka?dej innej zmiennej numerycznej.Nie mo?e by? brak?w danych.}

\item{weights}{wektor z wagami.}

\item{dt}{\code{data.table} z kolumnami \code{x}, \code{y}, \code{weights}.}

\item{avg}{\code{vector}, \code{data.frame}, \code{data.table} z warto?ciami dla kt?rych
zostan? wyliczone ?rednie.}

\item{total}{czy do??czy? wiersz z podsumowaniem.}

\item{sort_x}{je?li \code{TRUE}, wynikowa tabela zostanie posortowana po warto?ciach 
cechy \code{x}. W przeciwnym wypadku sortowanie b?dzie zgodne z porz?dkiem w danych.}
}
\value{
Zwraca \code{data.table} ze statystykami.
}
\description{
Wylicza statystyki zmiennej \code{y} dla podanych grup \code{x}. W za?o?eniu
\code{y} jest zmienn? dwumianow? o warto?ciach {0,1}.
}
\author{
Micha? Danaj
}
