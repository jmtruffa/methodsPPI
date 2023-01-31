\name{getPPIBoo2k}
\alias{getPPIBoo2k}
\title{Devuelve el book de BIDs y OFFERs del activo}
\usage{
getPPIBook(token, ticker, type, settlement)
}
\description{
Devuelve el book de BIDs y OFFERs (máximo 5) del activo en cuestión. Se le agregó control de error. Se podría, luego de pruebas, pisar getPPIBook por ésta función dado que la respuesta es la misma, incluso cuando hay error ya que cuando hay error devuelve el item en cero.
}
\arguments{
  \item{token}{Token obtenido de getPPILogin2}
  \item{ticker}{De qué ticker se van a traer los BIDs y OFFERs}
  \item{type}{Qué tipo de activo. "BONOS", "CEDEARS".}
  \item{settlement}{"INMEDIATA" o "A-48HS"}
\examples{
getPPIBook(token = PPI$token, ticker = "GD30", type = "BONOS", settlement = "INMEDIATA")
}






