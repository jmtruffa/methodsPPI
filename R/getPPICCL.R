#' getPPICCL
#'
#' Devuelve el CCL a través de CEDEARs, Bonos o Acciones (y ADRs).
#' Si es Bonos, consulta a la API de PPI por el precio del bono en cuestión agregándole una C para el precio Cable.
#' Si es CEDEAR o ACCIONES, consulta las tablas de ratiosCedear o ratiosADR que está en db:test  y, con los ratio de allí
#' obtenidos, calcula el CCL:
#'
#' Ver también getRofexCCL() que trabaja sobre una serie histórica y complementa
#' con valores obtenidos de la encuesta Rofex
#'
#' @param from  Fecha desde. Default hoy
#' @param to Fecha hasta. Default hoy
#' @param via Nombre activo a usar.
#' @param type CEDEARS, ACCIONES o BONOS
#' @param settle Settlement. t+2 o t+0
#'
#' @return Un tibble con el ccl
#'
getPPICCL = function(from = Sys.Date(), to = Sys.Date() + 1, via = "AAPL", type = "CEDEARS", settle = "T+2") {
  require(methodsPPI)
  require(tidyquant)
  require(dplyr)

  df = data.frame()
  via = toupper(via)
  type = toupper(type)
  settle = ifelse(toupper(settle) == "T+2", "A-48HS", "INMEDIATA")
  ratio = 1

  if (type %in% c("CEDEARS", "ACCIONES", "BONOS")) {


    if (type == "BONOS") {

      PPI = methodsPPI::getPPILogin2()
      prices = pmap_dfr(list(ticker = c(via, paste0(via,"C")),
                             token = PPI$token,
                             type = type,
                             from = from,
                             to = to,
                             settlement = settle),
                        methodsPPI::getPPIPriceHistoryMultiple3
                       )[c(1:3)]

      df = prices %>% pivot_wider(names_from = ticker,
                             values_from = price) %>%
        mutate(CCL = .[[2]] / .[[3]]) %>%
        select(date, CCL)

    } else {

      table = switch(
        type,
        "CEDEARS" = "ratiosCedear",
        "ACCIONES" = "ratiosADR"
      )
      con = DBI::dbConnect(RSQLite::SQLite(), '~/data/test.sqlite3')
      ratio = pull(DBI::dbGetQuery(con, paste0("SELECT ratio FROM ", table ," WHERE ",ifelse(type == "CEDEARS", "ticker", "tickerExterior"),"=\'",via,"\'")))
      DBI::dbDisconnect(con)

      # Busco el de exterior

      ext = tidyquant::tq_get(via,
                              from = from,
                              to = to) %>% dplyr::select(date, ticker = symbol, price = adjusted)


      # Busco el local
      PPI = methodsPPI::getPPILogin2()

      prices = methodsPPI::getPPIPriceHistoryMultiple3(token = PPI$token,
                                                       ticker = via,
                                                       type = type,
                                                       from = from,
                                                       to = to,
                                                       settlement = settle)[[1]][c(1:3)]

      df = left_join(ext, prices, join_by(date, ticker))
      df$CCL = (df$price.y * ratio) / df$price.x
      df = df[c(1,5)]

    }


  }

  return(df)

}



