% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/util.R
\name{lastDay}
\alias{lastDay}
\title{Zwraca ostatni dzień podanego okresu.}
\usage{
lastDay(dat, unit = c("month", "quarter", "half_year", "year"), ...)
}
\arguments{
\item{dat}{Obiekt klasy \link{Date}, lub dający się skonwertować funkcją \link{as.Date}.}

\item{unit}{Jednostka czasu, która ma być uwzględniona przy wyznaczaniu ostatniego dnia.
Możliwe wartości: \code{c("month","quater", "year")}, które odpowiadają ostatniemu dniu miesiąca, kwartału, roku,
do którego należy data \code{dat}.}

\item{...}{Opcje do funkcji \link{as.Date}.}
}
\description{
Zwraca ostatni dzień podanego okresu.
}
\author{
Michał Danaj
}
