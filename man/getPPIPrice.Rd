\name{getPPIPrice}
\alias{getPPIPrice}
\title{Devuelve el precio de un activo}
\usage{
getPPIPrice()
}
\description{
Devuelve el precio de un activo.
Ofrece el LAST

Devuelve una lista con dos elementos:

1.- date
2.- price (LAST)
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
  \item{settlement}{Uno de los 4 tipos:
  "INMEDIATA",
  "A-24HS",
  "A-48HS",
  "A-72HS"

  Más usuales INMEDIATA y A-48HS}
}
\examples{
getPPIPrice(PPI$token, ticker = "GD30", type = "BONOS", settlement = "A-48HS")
}






