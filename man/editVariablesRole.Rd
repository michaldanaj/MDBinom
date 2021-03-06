% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/meta_i_sterujace.R
\name{editVariablesRole}
\alias{editVariablesRole}
\title{Generuje kod do zmiany roli zmiennych lub data.frame z rolami}
\usage{
editVariablesRole(zmienne_rola, pattern = NULL, gen_code = TRUE)
}
\arguments{
\item{zmienne_rola}{\code{data.frame} z rolami zmiennych.}

\item{pattern}{Wyrażenie przekazane do funkcji \code{grep} filtrujące zmienne}

\item{gen_code}{Jeśli TRUE, wynikiem funkcji jest kod do zmiany wartości. W przeciwnym razie, zwracany
jest data.frame ze zmienionymi rolami.}
}
\value{

}
\description{
Generuje kod do zmiany roli zmiennych lub data.frame ze zmienionymi rolami.
}
\author{
Michał Danaj
}
