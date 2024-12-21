#' getPPILogin
#'
#' Ultima función de Login para PPI.
#' No requiere parametros.
#' Chequea si existe un objecto llamado token en el global environment.
#' Si no existe procesa uno nuevo.
#' Si existe, toma su expiration y verifica si venció.
#' Si venció solicita uno nuevo. Si no venció indica que aún está vigente.
#'
#' De esta manera se minimiza la cantidad de veces que se pide un token
#' y evitamos una sobrecarga a la API de PPI.
#'
#'
#' @return solo toca el objeto token en el global environment
#' @example getPPILogin()
getPPILogin = function() {
  require(tidyverse)
  require(jsonlite)
  require(httr2)
  # Helper function to parse API response
  parse_response = function(response) {
    body <- fromJSON(rawToChar(response$body))
    list(
      token = paste0("Bearer ", body$accessToken),
      refreshToken = body$refreshToken,
      expiration = body$expirationDate
    )
  }

  new_token = function() {
    # Request a new token
    url_base <- "https://clientapi.portfoliopersonal.com/api/1.0/"
    url_login <- paste0(url_base, "Account/LoginApi")
    r_login <- request(url_login) %>%
      req_headers(
        AuthorizedClient = "API_CLI_REST",
        #AuthorizedClient = "API-CLI",
        ClientKey = "pp19CliApp12",
        ApiKey = Sys.getenv("PPI_API_KEY"),
        ApiSecret = Sys.getenv("PPI_SECRET_KEY"),
        `User-Agent` = "http://github.com/jmtruffa"
      ) %>%
      req_body_json("") %>%
      req_method("POST") %>%
      req_perform()

    # Parse response and save the token in the global environment
    token <<- parse_response(r_login)
  }

  # Check if `token` exists in the global environment
  if (!exists("token", envir = .GlobalEnv)) {
    message("Token does not exist. Requesting a new one...")

    # Request a new token
    new_token()
    return("Token created.")
  }

  # Validate the existing token
  message("Token exists. Checking validity...")
  token_expiration_fixed <- sub("([+-]\\d{2}):(\\d{2})$", "\\1\\2", token$expiration)
  token_expiration_posix <- as.POSIXct(token_expiration_fixed, format = "%Y-%m-%dT%H:%M:%S%z")
  current_time <- Sys.time()

  if (!is.na(token_expiration_posix) && current_time < token_expiration_posix) {
    return("Token is still valid.")
  }

  # Refresh or request a new token
  if (!is.null(token$refreshToken)) {
    message("Refreshing token...")
    url_base <- "https://clientapi.portfoliopersonal.com/api/1.0/"
    url_refresh <- paste0(url_base, "Account/RefreshToken")
    tryCatch(
      expr = {
        r_refresh <- request(url_refresh) %>%
          req_headers(
            Authorization = paste0("Bearer no controlan que hay aca"),
            AuthorizedClient = "API_CLI_REST",
            ClientKey = "pp19CliApp12",
            `User-Agent` = "http://github.com/jmtruffa"
          ) %>%
          req_body_raw(
            paste0('{"refreshToken": "', token$refreshToken, '"}'),
            type = "application/json"
          ) %>%
          req_method("POST") %>%
          req_perform()

        # Si no hay error, parseamos la respuesta
        token <<- parse_response(r_refresh)
      },
      error = function(e) {
        # Si ocurre un error, se llama a new_token()
        message("Error refreshing token. Requesting a new one...")
        new_token()
      }
    )
    return("Token refreshed.")
  } else {
    message("Refresh token is not available. Requesting a new one...")

    # Request a new token
    new_token()
    return("Pedido token nuevo.")
  }
}



getPPIPrice = function(token, ticker, type, settlement = "INMEDIATA") {
  require(tidyverse)
  require(jsonlite)
  require(httr2)
  url = 'https://clientapi.portfoliopersonal.com/api/1.0/'
  urlMarketData = 'MarketData/Current'
  rPrice = request(paste0(url, urlMarketData)) %>%
    req_headers(Authorization = token,
                AuthorizedClient = 'API-CLI',
                ClientKey = 'pp19CliApp12',
                `User-Agent` = "http://github.com/jmtruffa"
    ) %>%
    req_method("GET") %>%
    req_url_query(Ticker = ticker, Type = type, Settlement = settlement) %>%
    req_perform()
  date = fromJSON(rawToChar(rPrice$body))$date
  price = fromJSON(rawToChar(rPrice$body))$price
  returnValue = list(date = date,
                     price = price)
  returnValue
}

getPPIPrice2 = function(token, ticker, type, settlement = "INMEDIATA") {
  require(tidyverse)
  require(jsonlite)
  require(httr2)
  url = 'https://clientapi.portfoliopersonal.com/api/1.0/'
  urlMarketData = 'MarketData/Current'
  result = tibble(
    ticker = character(),
    date = Date(),
    price = numeric()
  )
  fail = tibble(
    ticker = character()
  )

  for (i in 1:length(ticker)) {
    error = FALSE
    tryCatch(
      {
        rPrice = request(paste0(url, urlMarketData)) %>%
          req_headers(Authorization = token,
                      AuthorizedClient = 'API-CLI',
                      ClientKey = 'pp19CliApp12',
                      `User-Agent` = "http://github.com/jmtruffa") %>%
          req_url_query(ticker = ticker[i],
                        type = type[i],
                        settlement = settlement) %>%
          req_method("GET") %>%
          req_perform } ,
      error = function(e) { error <<- TRUE; fail <<- fail %>% add_row(ticker = ticker[i]);print(ticker[i]) }
    )

    if (!error) {
      history = fromJSON(rawToChar(rPrice$body))
      #history = do.call(rbind.data.frame, history) #Cambiaron la forma de devolver?
      history$date = as.Date(history$date)
      result = result %>% add_row(
        ticker = ticker[i],
        date = history$date,
        price = history$price
      )
      #result = rbind(result, as_tibble(cbind(ticker = rep(ticker[i], length(history$date)), history)))
    }
  }
  return(list(result, fail))


  # rPrice = request(paste0(url, urlMarketData)) %>%
  #   req_headers(Authorization = token,
  #               AuthorizedClient = 'API-CLI',
  #               ClientKey = 'pp19CliApp12',
  #               `User-Agent` = "http://github.com/jmtruffa"
  #   ) %>%
  #   req_method("GET") %>%
  #   req_url_query(Ticker = ticker, Type = type, Settlement = settlement) %>%
  #   req_perform()
  # date = fromJSON(rawToChar(rPrice$body))$date
  # price = fromJSON(rawToChar(rPrice$body))$price
  # returnValue = list(date = date,
  #                    price = price)
  # returnValue
}

getPPIPriceHistoryMultiple2 = function(token, ticker, type, from, to, settlement = "INMEDIATA") {
  url = 'https://clientapi.portfoliopersonal.com/api/1.0/'
  URLPriceHistory = "MarketData/Search"
  result = tibble(
    ticker = character(),
    date = numeric(),
    price = numeric(),
    volume = numeric(),
    openingPrice = numeric(),
    max = numeric(),
    min = numeric()
  )

  fail = tibble(
    ticker = character()
  )


  for (i in 1:length(ticker)) {
    error = FALSE
    tryCatch(
      {
    rPriceHistory = request(paste0(url, URLPriceHistory)) %>%
      req_headers(Authorization = token,
                  AuthorizedClient = 'API-CLI',
                  ClientKey = 'pp19CliApp12',
                  `User-Agent` = "http://github.com/jmtruffa") %>%
      req_url_query(ticker = ticker[i],
                    type = type[i],
                    dateFrom = from,
                    dateTo = to,
                    settlement = settlement) %>%
      req_method("GET") %>%
      req_perform } ,
    error = function(e) { error <<- TRUE; fail <<- fail %>% add_row(ticker = ticker[i]) }
    )
    print(error)
    if (!error) {
      history = fromJSON(rawToChar(rPriceHistory$body))
      #history = do.call(rbind.data.frame, history) #Cambiaron la forma de devolver?
      history$date = as.Date(history$date)
      result = rbind(result, as_tibble(cbind(ticker = rep(ticker[i], length(history$date)), history)))
    }
  }
  return(list(result, fail))
}

#' getPPIPrices
#'
#' Trae los precios de un ticker en un rango de fechas, y si PPI no lo tiene
#' lo busca en la base de datos local.
#' Esta es por ahora la más completa al 7-sep-2024 ya que es la primera que incluye
#' poder buscar en base de datos cuando PPI no tiene los precios.
#'
#' @param token Token de PPI
#' @param ticker Tickers a buscar. Acepta vector.
#' @param type Tipo de activo. Siempre usar único tipo por vez. Acepta vector.
#' @param from Fecha desde la que buscar
#' @param to Fecha hasta la que buscar
#' @param settlement Tipo de liquidación. Default es "INMEDIATA"
#' @param ... Parámetros para dbExecuteQuery
#'
#' @return Lista con dos elementos: result y fail. Result es un tibble con los precios encontrados y fail los tickers que no se encontraron.
#'
getPPIPrices = function(token, ticker, type, from, to, settlement = "INMEDIATA", ...) {
  require(tidyverse)
  require(jsonlite)
  require(httr2)
  url = 'https://clientapi.portfoliopersonal.com/api/1.0/'
  URLPriceHistory = "MarketData/Search"

  result = tibble(
    ticker = character(),
    date = structure(rep.int(NA_real_, 0), class = "Date"),
    price = numeric(),
    volume = numeric(),
    openingPrice = numeric(),
    max = numeric(),
    min = numeric()
  )

  fail = tibble(
    ticker = character()
  )

  for (i in 1:length(ticker)) {
    error = FALSE
    tryCatch(
      {
        rPriceHistory = request(paste0(url, URLPriceHistory)) %>%
          req_headers(Authorization = token,
                      AuthorizedClient = 'API_CLI_REST',
                      ClientKey = 'pp19CliApp12',
                      `User-Agent` = "http://github.com/jmtruffa") %>%
          req_url_query(ticker = ticker[i],
                        type = type[i],
                        dateFrom = from,
                        dateTo = to,
                        settlement = settlement) %>%
          req_method("GET") %>%
          req_perform()
      },
      error = function(e) { error <<- TRUE; fail <<- fail %>% add_row(ticker = ticker[i]) }
    )

    if (!error) {
      # A veces la API de PPI devuelve 200 pero el body está vacío. Vamos a controlar por body vacío
      if (rawToChar(rPriceHistory$body) == "[]") {
        error = TRUE
        fail = fail %>% add_row(ticker = ticker[i])
      } else {
        history = fromJSON(rawToChar(rPriceHistory$body))
        history$date = as.Date(history$date)
        result = rbind(result, as_tibble(cbind(ticker = rep(ticker[i], length(history$date)), history)))
      }
      # history = fromJSON(rawToChar(rPriceHistory$body))
      # history$date = as.Date(history$date)
      # print(history)
      # result = rbind(result, as_tibble(cbind(ticker = rep(ticker[i], length(history$date)), history)))
      # print(result)
    }
  }

  # Buscar en la base de datos los tickers fallidos
  if (nrow(fail) > 0) {
    query = sprintf("
      SELECT *
      FROM historical_prices
      WHERE ticker IN (%s)
        AND date BETWEEN '%s' AND '%s'
    ", paste0("'", fail$ticker, "'", collapse = ", "),
                    from, to)

    # Le dejamos la elipsis para que le pasen parámetros de server ) "local", "medina" o "aws", server, host, db y port.
    historical_data = dbExecuteQuery(query = query, ...)


    if (nrow(historical_data) > 0) {
      historical_data = rename(historical_data, price = close)
      # Agregar los datos históricos al result
      result = result %>% bind_rows(historical_data)

      # Filtrar los tickers encontrados de fail
      found_tickers = unique(historical_data$ticker)
      fail = fail %>% filter(!ticker %in% found_tickers)
    }
  }

  return(list(result, fail))
}

getPPIPriceHistoryMultiple3 = function(token, ticker, type, from, to, settlement = "INMEDIATA") {
  url = 'https://clientapi.portfoliopersonal.com/api/1.0/'
  URLPriceHistory = "MarketData/Search"
  result = tibble(
    ticker = character(),
    date = numeric(),
    price = numeric(),
    volume = numeric(),
    openingPrice = numeric(),
    max = numeric(),
    min = numeric()
  )

  fail = tibble(
    ticker = character()
  )


  for (i in 1:length(ticker)) {
    error = FALSE
    tryCatch(
      {
        rPriceHistory = request(paste0(url, URLPriceHistory)) %>%
          req_headers(Authorization = token,
                      AuthorizedClient = 'API_CLI_REST',
                      ClientKey = 'pp19CliApp12',
                      `User-Agent` = "http://github.com/jmtruffa") %>%
          req_url_query(ticker = ticker[i],
                        type = type[i],
                        dateFrom = from,
                        dateTo = to,
                        settlement = settlement) %>%
          req_method("GET") %>%
          req_perform } ,
      error = function(e) { error <<- TRUE; fail <<- fail %>% add_row(ticker = ticker[i]) }
    )

    if (!error) {
      history = fromJSON(rawToChar(rPriceHistory$body))
      #history = do.call(rbind.data.frame, history) #Cambiaron la forma de devolver?
      history$date = as.Date(history$date)
      result = rbind(result, as_tibble(cbind(ticker = rep(ticker[i], length(history$date)), history)))
    }
  }

return(list(result, fail))
}

getPPIPriceHistoryMultiple = function(token, ticker, type, from, to, settlement = "INMEDIATA") {
  url = 'https://clientapi.portfoliopersonal.com/api/1.0/'
  URLPriceHistory = "MarketData/Search"

  for (i in 1:length(ticker)) {
    rPriceHistory = request(paste0(url, URLPriceHistory)) %>%
      req_headers(Authorization = token,
                  AuthorizedClient = 'API-CLI',
                  ClientKey = 'pp19CliApp12',
                  `User-Agent` = "http://github.com/jmtruffa") %>%
      req_url_query(ticker = ticker[i],
                    type = type[i],
                    dateFrom = from,
                    dateTo = to,
                    settlement = settlement) %>%
      req_method("GET") %>%
      req_perform
    history = fromJSON(rawToChar(rPriceHistory$body))
    #history = do.call(rbind.data.frame, history) #Cambiaron la forma de devolver?
    history$date = as.Date(history$date)
    if (exists("result") == FALSE) {
      result = tibble(
        ticker = character(),
        date = numeric(),
        price = numeric(),
        volume = numeric(),
        openingPrice = numeric(),
        max = numeric(),
        min = numeric()
      )
    }
    result = rbind(result, as_tibble(cbind(ticker = rep(ticker[i], length(history$date)), history)))
  }
  result
}

#' getPPIDLR
#' Trae valores de dólar de PPI
#' @title Devuelve el histórico del precio del dolar MEP y CCL y el gráfico.
#' @description Devuelve el histórico del precio del dolar MEP y CCL y el gráfico. Lo calcula con GD30 y AL30 desde septiembre 2020. Si se pide fecha previa, lo construye con AY24. La fecha más atrás que posee es 2014-05-27
#' @param from Fecha desde la que buscar. Default es 2014-05-27
#' @param to Fecha hasta la que buscar. Default es Sys.Date()
#' @param settlement Tipo de liquidación. Default es "t+0"
#' @return Un tibble con los valores de dólar
#' @example getPPIDLR(from = "2014-05-27", to = Sys.Date())
getPPIDLR = function(from = "2014-05-27", to = Sys.Date(), settlement = 't+0') {
  require(tidyverse)
  require(scales)
  require(ggthemes)
  require(tidyquant)
  require(lubridate)
  require(methodsPPI)

  if (from >= "2014-05-27") {
      juntar = FALSE
      if (from < "2020-09-15") {
        tickersBonosOld = c('AY24', 'AY24D', 'AY24C')
        fileDirectory = '/Users/Juan/GoogleDrive/Mi unidad/analisis financieros/data/'

        for (i in seq_along(tickersBonosOld)){
          if (!file.exists(paste0(fileDirectory, tickersBonosOld[i], '.csv'))) {
            download.file(paste('http://clasico.rava.com/empresas/precioshistoricos.php?e=',tickersBonosOld[i],'&csv=1', sep=''),
                          paste0(fileDirectory,tickersBonosOld[i], '.csv'), mode = 'wb')
          }
        }
        bonosOld = tibble(
          ticker = character(),
          fecha = Date(),
          apertura = double(),
          maximo = double(),
          minimo = double(),
          cierre =double(),
          volumen = double(),
          openint = double()
        )
        for (i in seq_along(tickersBonosOld)){
          temp = read_csv(paste0(fileDirectory, tickersBonosOld[i], '.csv'))
          temp$ticker = tickersBonosOld[i]
          bonosOld = rbind(bonosOld, temp)
        }

        bonosOld = bonosOld %>%
          relocate(ticker, date = fecha, price = cierre, volume = volumen, openingPrice = apertura, max = maximo, min = minimo) %>%
          select(-openint) %>%
          filter(date >= from, date <= "2020-09-14") %>%
          #filter(date >= "2014-05-27") %>%
          mutate(
            ticker = case_when(
              ticker == "AY24" ~ "GD30",
              ticker == "AY24D" ~ "GD30D",
              ticker == "AY24C" ~ "GD30C"
            )
          )
        juntar = TRUE
      }

    getPPILogin()
    settlement = ifelse(settlement == 't+0', "INMEDIATA", "A-48HS")
    tickers = c('AL30', 'GD30C', 'AL30D', 'GD30', 'GD30D')
    type = rep('BONOS', length(tickers))

    resultMEP = getPPIPriceHistoryMultiple3(token$token,
                                            ticker = tickers,
                                           type = type,
                                           from = max(as.Date(from), as.Date("2020-09-15")),
                                           to = to,
                                           settlement = settlement)
    resultMEP = resultMEP[[1]][,-c(8:10)]

    #inicioResultMEP = max(resultMEP %>% head(n=1) %>% pull(date), "2020-09-15")

    if (juntar) {
      resultMEP = rbind(bonosOld, resultMEP)
    }


    tiposCambio = resultMEP %>%
      select(ticker, date, price) %>%
      pivot_wider(names_from =  ticker, values_from = price) %>%
      mutate(
        mepAL = AL30 / AL30D,
        mepGD = GD30 / GD30D,
        cclGD = GD30 / GD30C,
        Canje = cclGD / mepGD - 1
      ) %>%
      select(-c(AL30, GD30, GD30C, AL30D, GD30D))

    cbPalette <- c("#939599" , "#404042", "#9CD6F9", "#7ACAFA", "#4CAAE2", "#4CAAE2", "#235DBC", "#1C4993", "#14366D", "#0C1F3E", "#C6D6EE",
                   "#CAD1DC", "#ACB0B8", "#757679")

    return(tiposCambio)

  } else {
      stop("Fecha debe ser mayor a 2014-05-26")
  }
}

getPPIDLR2 = function(ticker = "GD30", type = "BONOS") {
  require(methodsPPI)
  require(lubridate)

  getPPILogin()

  tickers = c(ticker, paste0(ticker,"C"), paste0(ticker,"D"))

  # tickers = c(
  #   "GD30",
  #   "GD30D",
  #   "GD30C"
  # )

  settlement = c("INMEDIATA", "A-48HS")

  result = tibble(
    ticker = character(),
    settlement = character(),
    date = Date(),
    SIDE = character(),
    position = integer(),
    price = double(),
    quantity = numeric()
  )



  for (i in 1:length(tickers)) {
    for (k in 1:length(settlement)) {

      temp = getPPIBook(token$token, ticker = tickers[i], type = type, settlement = settlement[k])
      result = rbind(result, as_tibble(cbind(temp,
                                             settlement = rep(settlement[k], length(temp$date))
      )))
    }
  }

  result = result %>% group_by(settlement, ticker, SIDE) %>% do(head(., n=1))


}

getPPIBook = function(token, ticker, type, settlement = "INMEDIATA") {
  require(tidyverse)
  require(jsonlite)
  require(httr2)

  ##Esto es para probar la funcion de manera directa
  # token = PPI$token
  # settlement = "INMEDIATA"
  # ticker ="GD30C"
  # type = "BONOS"

  url = 'https://clientapi.portfoliopersonal.com/api/1.0/'
  urlMarketData = 'MarketData/Book'

  rBook = request(paste0(url, urlMarketData)) %>%
    req_headers(Authorization = token,
                AuthorizedClient = 'API_CLI_REST',
                ClientKey = 'pp19CliApp12',
                `User-Agent` = "http://github.com/jmtruffa"
    ) %>%
    req_method("GET") %>%
    req_url_query(Ticker = ticker, Type = type, Settlement = settlement) %>%
    req_perform()
  body = fromJSON(rawToChar(rBook$body))
  if (!length(body$offers) == 0) {
    date = body$date
    bids = tibble(cbind(SIDE = rep("BID", length(body$bids$position)),(body$bids)))
    offers = tibble(cbind(SIDE = rep("OFFER", length(body$offers$position)),(body$offers)))
  #returnValue = tibble(rbind(cbind(date = rep(date, length(bids$SIDE)), bids),
  #                           cbind(date = rep(date, length(bids$SIDE)), offers) ))
    returnValue = cbind(
      date = as.Date(rep(date, length(bids$position) + length(offers$position))),
      ticker = rep(ticker, length(bids$position) + length(offers$position)),
      rbind(bids, offers))
  } else {
    date = body$date
    bids = tibble(SIDE = rep("BID", 1),
                  position = 0,
                  price = 0,
                  quantity = 0)
    offers = tibble(SIDE = rep("OFFER", 1),
                    position = 0,
                    price = 0,
                    quantity = 0)
    returnValue = cbind(
      date = as.Date(date),
      ticker = rep(ticker, 1),
      rbind(bids, offers))
  }
}

getPPIBook2 = function(ticker, type, token,settlement = "INMEDIATA") {
  ### devuelve vacío cuando un ticker falla. Normalmente puede fallar porque consulto un ticker viejo
  require(tidyverse)
  require(jsonlite)
  require(httr2)
  require(lubridate)
  # print(ticker)
  # print(type)
  # print(token)

  ##Esto es para probar la funcion de manera directa
  # token = PPI$token
  # settlement = "INMEDIATA"
  # ticker ="S16D2"
  # type = "LETRAS"

  url = 'https://clientapi.portfoliopersonal.com/api/1.0/'
  urlMarketData = 'MarketData/Book'

  fail = tibble(
    ticker = character()
  )
  responseBody = tibble(
    date = Date(),
    bids = numeric(),
    offers = numeric()

  )
  error = FALSE
  tryCatch(
    {
      rBook = request(paste0(url, urlMarketData)) %>%
        req_headers(Authorization = token,
                    AuthorizedClient = 'API_CLI_REST',
                    ClientKey = 'pp19CliApp12',
                    `User-Agent` = "http://github.com/jmtruffa"
        ) %>%
        req_method("GET") %>%
        req_url_query(Ticker = ticker, Type = type, Settlement = settlement) %>%
        req_perform()

      responseBody = fromJSON(rawToChar(rBook$body))
    },
    error = function(e) { error <<- TRUE; fail <<- fail %>% add_row(ticker = ticker) }
  )

  if (!length(responseBody$offers) == 0) {
    date = responseBody$date
    bids = tibble(cbind(SIDE = rep("BID", length(responseBody$bids$position)),(responseBody$bids)))
    offers = tibble(cbind(SIDE = rep("OFFER", length(responseBody$offers$position)),(responseBody$offers)))
    #returnValue = tibble(rbind(cbind(date = rep(date, length(bids$SIDE)), bids),
    #                           cbind(date = rep(date, length(bids$SIDE)), offers) ))
    returnValue = cbind(
      date = as.Date(rep(date, length(bids$position) + length(offers$position))),
      ticker = rep(ticker, length(bids$position) + length(offers$position)),
      rbind(bids, offers))
  } else { ### Acá viene tanto el offer no exista o bien haya salido por error
    date = Sys.Date()
    bids = tibble(SIDE = rep("BID", 1),
                  position = 0,
                  price = 0,
                  quantity = 0)
    offers = tibble(SIDE = rep("OFFER", 1),
                    position = 0,
                    price = 0,
                    quantity = 0)
    returnValue = cbind(
      date = as.Date(date),
      ticker = rep(ticker, 1),
      rbind(bids, offers))
  }
}


#' getPPICurrentRofex
#'
#' Trae la curva de Rofex desde BYMA (via PPI) y calcula implícitas
#' Toma el spot de la web de MAE. Si se le informa uno en spt, toma ese.
#' Toma también, via getRofexPosition(), la curva del día anterior para graficarla comparada.
#'
#' @param spt Spot para usar. Overrides el de MAE. Si no se pone nada y MAE no lo tiene devuelve error.
#' @param db Base donde va a buscar los feriados. Default es test.
#' @param prevDay Fecha del día previo a tomar. Default es día anterior. Ajustar a mano cuando hay feriados raros en medio
#'
#' @return una lista con un df con los futuros y un gráfico con las implícitas
getPPICurrentRofex = function(db = "", spt = NULL, prevDay = Sys.Date() - 1) {
  #para pegarle a la API
  require(methodsPPI)
  require(dplyr)
  require(ggplot2)
  require(rofex)

  #para el grafico
  require(scales)
  require(tidyquant)
  require(ggrepel)
  require(tibble)
  require(functions)
  require(bizdays)
  require(stringr)
  ### esto podrà ser modificado en breve ya que todo apunta a ~/data
  if (db == "") {
    if (str_detect(Sys.info()['nodename'], "Air")) {
      db = "~/data/test.sqlite3"
    } else {
      db = '~/data/test.sqlite3'
    }
  }


  cal = create.calendar('tempCal', getFeriados(db), weekdays=c("saturday", "sunday"))

  secuencia = function (serie) {
    require(lubridate)
    ret = NULL
    end = Date()
    meses = c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
    for (i in seq_along(serie)) {
      ret = append(ret, paste0("DLR/",meses[lubridate::month(as.Date(serie[i]))], substr(lubridate::year(as.Date(serie[i])), 3, 4)))
      finMes = lubridate::ceiling_date(as.Date(serie[i]), unit = "month") - 1
      offset = ifelse(bizdays::is.bizday(finMes, cal = 'tempCal'), 0, -1)
      finMesAjustado = bizdays::offset(finMes, offset, cal = 'tempCal')
      end = append(end, finMesAjustado)
    }
    ret = tibble(futuro = ret, vto = end)
    ret
  }
  #serie = seq.Date(from = Sys.Date(), length.out = 12, by = "months")
  tira = secuencia(seq.Date(from = as.Date(lubridate::floor_date(Sys.Date(), unit = "month")), length.out = 12, by = "months"))

  getPPILogin()

  fut = NULL
  for (vto in tira$futuro) {
    fut = append(fut, getPPIPrice(token = token$token,
                                  ticker = vto,
                                  type = "FUTUROS",
                                  settlement = 'INMEDIATA')$price)
  }
  # llama el scraper en Go que baja de MAE el último operado si es que no hay parámetro SPOT
  # esto permite que SI SE LE PASA PARAMETRO, quiere decir que fallaba y lo fuerza calculando contra ese
  if (is.null(spt)) {
    if (str_detect(Sys.info()['nodename'], "Air")) {
      scraper = '~/Google\\ Drive/Mi\\ unidad/analisis\\ financieros/functions/data/bin/maeScraper'
      spot = as.numeric(system(scraper, intern = TRUE))
    } else {
      scraper = '~/dev/bin/maeScraper'
      spot = as.numeric(system(scraper, intern = TRUE)[4])
    }
  } else {
    spot = spt
  }


  futuros = as_tibble(cbind(cbind(tira, fut), spot))

  futuros = futuros %>%
    mutate(
      tnaImplic = (fut / spot - 1) / (as.numeric(vto - Sys.Date()))*365,
      teaImplic = (fut / spot) ^ (365 / (as.numeric(vto - Sys.Date()))) - 1,
      orden = row_number(),
      pase = ifelse(
        orden == 1, fut / spot - 1, fut / lag(fut) - 1
      ),
      futuro = sub(".*/", "", futuro)
    ) %>%
    filter(fut != 0) #limpiamos si algun precio de futuro viene en 0

  # Ahora traer los futuros del día anterior
  prevDay = bizdays::adjust.previous(Sys.Date() - 1, cal = cal)
  prevFuturos = getRofexPosition(from = prevDay, to = prevDay)[[1]]
  prevFuturos = prevFuturos[c(2,3,21,18,15,14)]
  futuros = left_join(futuros, prevFuturos, join_by(orden == pos))

  #agrego los PASES del día anterior (salvo la 1era)

  futuros = futuros %>%
    mutate(paseAnt = (settlement / lag(settlement)) - 1 )


  # establece los límites del eje Y del gráfico
  #limits = c(min(futuros$teaImplic, futuros$implie) * 0.5, max(futuros$teaImplic) * 1.3)

  coeff = mean(futuros$teaImplic) / mean(futuros$pase)*.5
  futuros = futuros %>% mutate(nombre = paste0(futuro," (", fut, ")"))

  gRofex = futuros %>%
    ggplot(aes(x=reorder(nombre, +orden), linetype = )) +

    geom_col(aes(y=pase * coeff, fill = "darkgrey")) +
    geom_col(aes(y=pase * coeff, fill = "darkgrey")) +


    geom_line(aes(y=tnaImplic, color = "cornflowerblue"), group = 1) +
    geom_line(aes(y=teaImplic, color = "#0f4114"), group = 1) +

    # esta es la anterior
    geom_line(aes(y=impliedRateTNA, color = "cornflowerblue"), group = 2, linetype = "dashed") +
    geom_line(aes(y=impliedRateTEA, color = "#0f4114"), group = 2, linetype = "dashed") +


    geom_point(aes(y=tnaImplic, color = "cornflowerblue")) +
    geom_point(aes(y=teaImplic, color = "#0f4114")) +

    scale_y_continuous(breaks = breaks_extended(10),
                       labels = scales::percent_format(),
                       #limits = limits,
                       sec.axis = sec_axis(
                         ~ . / coeff,
                         name = "PASE",
                         breaks = breaks_extended(10),
                         labels = scales::percent)) +
    scale_color_manual(name = "",
                       values = c("#0f4114", "cornflowerblue", "blue"),
                       label = c("TEA", "TNA", "Anterior")) +
    scale_linetype_manual(values = c(1,1,2)) +
    scale_fill_manual(name = "", values = "darkgrey", label = "PASE") +

    labs(title = "Curva Futuros de Dólar",
         subtitle = paste0('Línea punteada es t-1. En base a último operado: ',spot[1], '. Fecha: ', format(Sys.Date(), format = "%Y-%m-%d")),
         y = 'TNA y TEA',
         x = 'Vencimiento y valor de settlement',
         caption = "Elaboración propia en base a precios MatbaRofex")+
    theme_tq() +
    theme( title = element_text(size=12, face='bold'),
      #panel.grid.major.x = element_blank() ,
      panel.grid.major.y = element_blank()) +

    geom_text_repel(data = futuros %>% select(nombre, tnaImplic),
                    aes(x = nombre, y=tnaImplic, label = scales::percent(tnaImplic, accuracy=0.01)),
                    nudge_x = 0.25, nudge_y = -0.1) +
    geom_text_repel(data = futuros %>% select(nombre, teaImplic),
                    aes(x = nombre, y=teaImplic, label = scales::percent(teaImplic, accuracy=0.01)),
                    nudge_y = 0.25, color = "#054D41")

  return(list(futuros, gRofex))
}

getPPIFullBook = function (instrumentos, settlement = "A-48HS", endpoint = "yield", fee = tibble(
  letras = 0.003515,
  bonos = 0.01
)) {

  require(functions)
  require(tidyverse)
  require(purrr)
  require(methodsPPI)
  require(bizdays)

  tmpCalendar <- create.calendar('tmpCalendar', holidays = getFeriados(), weekdays = c('saturday','sunday'))

  getPPILogin()
  settlement = settlement

  # fee = tibble(
  #   letras = 0.003515,
  #   bonos = 0.01
  # )

  tickers = map_dfr(
    instrumentos,
    sets
  )


  book = pmap_dfr(
    list(
      tickers$ticker,
      tickers$type,
      token$token,
      settlement
    ),
    getPPIBook2
  )

  ### Le agrego el fee según tipo de instrumento
  book = book %>%
    left_join(tickers) %>%
    mutate(
      fee = ifelse(type == "LETRAS", fee$letras, fee$bonos)
    )

  ### Ahora tengo que filtrar los que tienen BID/OFFER en cero
  ### y luego pegarle a getYields para que me traiga los precios
  ### debería llenar con endpoint = apr / yield en función del tipo de instrumento
  ### pero inicialmente voy a pegar todo a yield

  book = book %>%
    group_by(ticker, SIDE) %>% ## Con esto hasta antes de filter freno los precios raros
    mutate(
      price = case_when(
        is.na(lag(price)) ~ price,
        (price / lag(price) - 1) > 0.05 ~ 0,
        TRUE ~ price,
      )
    ) %>%
    filter(price != 0)

  yields = pmap_dfr(
    list(
      book$ticker,
      as.character(bizdays::offset(Sys.Date(), ifelse(settlement == "INMEDIATA", 0, 2), cal = tmpCalendar)),
      book$price,
      book$fee
    ),
    getYields,
    endpoint = endpoint
  )

  ### ahora debería juntar los df
  fullBook = cbind(book, yields) %>% select(-endingFee, -initialFee, -letras, -precios)
}
