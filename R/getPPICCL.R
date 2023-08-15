#' getPPICCL
#'
#' Devuelve el CCL a través de CEDEARs en PPI
#'
#' @param from  Fecha desde. Default hoy
#' @param to Fecha hasta. Default hoy
#' @param via Nombre CEDEAR a usar. Default AAPL, AAPLD. Primero en pesos.
#'
#' @return Un tibble con el ccl
#'
getPPICCL = function(from = Sys.Date(), to = Sys.Date(), via = c("AAPL", "AAPLD")) {
  require(methodsPPI)
  PPI = getPPILogin2()
  prices = getPPIPriceHistoryMultiple3(token = PPI$token,
                                       ticker = via,
                                       type = c(rep("CEDEARS", length(via))),
                                       from = from,
                                       to = to,
                                       settlement = "A-48HS")[[1]]
  prices = prices %>% select(date, ticker, price) %>%
    pivot_wider(names_from = ticker, values_from = price) %>%
    mutate(ccl = .[[2]] / .[[3]])
  return(prices[,c(1,4)])
}
