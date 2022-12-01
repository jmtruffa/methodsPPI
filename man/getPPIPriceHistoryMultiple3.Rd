\name{getPPIPriceHistoryMultiple3}
\alias{getPPIPriceHistoryMultiple3}
\title{Devuelve el histórico de precios}
\usage{
getPPIPriceHistoryMultiple3()
}
\description{
Devuelve el historial de precios de uno o varios activos.


Devuelve una lista con dos elementos:

1.- El dataframe con los precios
2.- Los tickers que fallaron (ya sea porque están mal o porque la API arrojó un error)

En la versión 3 se introdujo que devuelva los tickers fallados a partir de hacer un tryCatch de la pegada a la API.
}
\arguments{
  \item{token}{token que se obtiene desde getPPILogin2 (bearer token)}
  \item{ticker}{ticker a buscar. En mayúsculas}
  \item{type}{Tipo de instrumento: (siempre mayúsculas)
  "BONOS",
  "LETRAS",
  "NOBAC",
  "LEBAC",
  "ON",
  "FCI" (Solo los que comercializa PPI)
  "CAUCIONES",
  "ACCIONES",
  "ETF",
  "CEDEARS",
  "OPCIONES",
  "FUTUROS",
  "ACCIONES-USA",
  "FCI-EXTERIOR"}
\item{from}{Fecha desde la cual consultar los datos}
\item{to}{Fecha hasta cual consultar los datos. Se puede usar Sys.Date()}
\item{settlement}{Uno de los 4 tipos:
  "INMEDIATA",
  "A-24HS",
  "A-48HS",
  "A-72HS"

  Más usuales INMEDIATA y A-48HS}
}
\examples{
getPPIPriceHistoryMultiple3(PPI$token, ticker = "GD30", type = "BONOS", from = "2022-01-01", to = Sys.Date(), settlement = "A-48HS")
}






