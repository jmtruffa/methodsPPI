\name{getPPIBook}
\alias{getPPIBook}
\title{Devuelve el book de BIDs y OFFERs del activo}
\usage{
getPPIBook(token, ticker, type, settlement)
}
\description{
Devuelve el book de BIDs y OFFERs (máximo 5) del activo en cuestión
}
\arguments{
  \item{token}{Token obtenido de getPPILogin2}
  \item{ticker}{De qué ticker se van a traer los BIDs y OFFERs}
  \item{type}{Qué tipo de activo. "BONOS", "CEDEARS".}
  \item{settlement}{"INMEDIATA" o "A-48HS"}
\examples{
getPPIBook(token = PPI$token, ticker = "GD30", type = "BONOS", settlement = "INMEDIATA")
}






