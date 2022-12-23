getPPILogin = function() { ### funcion deprecada cuando cambiaron la forma de Login Nov-2022

  require(tidyverse)
  require(jsonlite)
  require(httr2)

  url = 'https://clientapi.portfoliopersonal.com/api/1.0/'
  urlLogin = 'Account/Login'
  bodyLogin = list(user = Sys.getenv("PPI_API_KEY"),
                   password = Sys.getenv("PPI_SECRET_KEY"))

  rLogin = request(paste0(url, urlLogin)) %>%
    req_headers(AuthorizedClient = 'API-CLI',
                ClientKey = 'pp19CliApp12',
                `User-Agent` = "http://github.com/jmtruffa") %>%
    req_body_json(bodyLogin) %>%
    req_method("POST") %>%
    req_perform()

  token = paste0('Bearer ', fromJSON(rawToChar(rLogin$body))$accessToken)
  refreshToken = fromJSON(rawToChar(rLogin$body))$refreshToken
  returnValue = list(token = token,
                     refreshToken = refreshToken)
  returnValue
}

refreshPPIToken = function(token, refreshToken) {
  require(tidyverse)
  require(jsonlite)
  require(httr2)
  url = 'https://clientapi.portfoliopersonal.com/api/1.0/'
  urlRefreshToken = 'Account/RefreshToken'
  bodyRefreshToken = list(refreshToken = refreshToken)
  rRefreshToken = request(paste0(url, urlRefreshToken)) %>%
    req_headers(AuthorizedClient = 'API-CLI',
                ClientKey = 'pp19CliApp12',
                `User-Agent` = "http://github.com/jmtruffa",
                Authorization = token) %>%
    req_body_json(bodyRefreshToken) %>%
    req_method("POST") %>%
    req_perform()
  token = paste0('Bearer ', fromJSON(rawToChar(rRefreshToken$body))$accessToken)
  refreshToken = fromJSON(rawToChar(rRefreshToken$body))$refreshToken
  returnValue = list(token = token,
                     refreshToken = refreshToken)
  returnValue
}

getPPILogin2 = function() {

  require(tidyverse)
  require(jsonlite)
  require(httr2)

  url = 'https://clientapi.portfoliopersonal.com/api/1.0/'
  urlLogin = 'Account/LoginApi'

  rLogin = request(paste0(url, urlLogin)) %>%
    req_headers(AuthorizedClient = 'API-CLI',
                ClientKey = 'pp19CliApp12',
                ApiKey = Sys.getenv("PPI_API_KEY"),
                ApiSecret = Sys.getenv("PPI_SECRET_KEY"),
                `User-Agent` = "http://github.com/jmtruffa") %>%
    req_body_json("") %>% #No lleva nada ahora en el body. Antes llevaba user y pass
    req_method("POST") %>%
    req_perform()

  token = paste0('Bearer ', fromJSON(rawToChar(rLogin$body))$accessToken)
  refreshToken = fromJSON(rawToChar(rLogin$body))$refreshToken
  returnValue = list(token = token,
                     refreshToken = refreshToken)
  returnValue
}

refreshPPIToken = function(token, refreshToken) {
  require(tidyverse)
  require(jsonlite)
  require(httr2)
  url = 'https://clientapi.portfoliopersonal.com/api/1.0/'
  urlRefreshToken = 'Account/RefreshToken'
  bodyRefreshToken = list(refreshToken = refreshToken)
  rRefreshToken = request(paste0(url, urlRefreshToken)) %>%
    req_headers(AuthorizedClient = 'API-CLI',
                ClientKey = 'pp19CliApp12',
                `User-Agent` = "http://github.com/jmtruffa",
                Authorization = token) %>%
    req_body_json(bodyRefreshToken) %>%
    req_method("POST") %>%
    req_perform()
  token = paste0('Bearer ', fromJSON(rawToChar(rRefreshToken$body))$accessToken)
  refreshToken = fromJSON(rawToChar(rRefreshToken$body))$refreshToken
  returnValue = list(token = token,
                     refreshToken = refreshToken)
  returnValue
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

getPPIDLR = function(from = "2014-05-27", to = Sys.Date()) {
  require(tidyverse)
  require(scales)
  require(ggthemes)
  require(tidyquant)
  require(lubridate)

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

    PPI = getPPILogin2()
    settlement = 'INMEDIATA'
    tickers = c('AL30', 'GD30C', 'AL30D', 'GD30', 'GD30D')
    type = rep('BONOS', length(tickers))

    resultMEP = getPPIPriceHistoryMultiple3(PPI$token, ticker = tickers,
                                           type = type,
                                           from = max(as.Date(from), as.Date("2020-09-15")),
                                           to = to,
                                           settlement = settlement)
    resultMEP = resultMEP[[1]]

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

    g = resultMEP %>%
      select(ticker, date, price) %>%
      pivot_wider(names_from =  ticker, values_from = price) %>%
      mutate(
        mepAL = AL30 / AL30D,
        mepGD = GD30 / GD30D,
        cclGD = GD30 / GD30C
      ) %>%
      select(-AL30, -GD30, -AL30D, -GD30D, -GD30C) %>%
      #drop_na() %>%
      pivot_longer(!date) %>%
      rename(DLR = name) %>%
      ggplot(aes(x=date, y=value, color = DLR )) +
      geom_line(size = 0.6) +
      scale_y_continuous(breaks = breaks_extended(16), sec.axis = dup_axis()) +
      labs(title = "Evolución MEP y CCL via GD30 y AL30",
           subtitle = "t+0. Último operado",
           x='Fecha',
           y='Pesos',
           caption = 'Elaboración propia en base a datos de mercado (BYMA)') +
      theme_tq() +
      annotate("text", x = Sys.Date() - 15, y=255, label = paste0("CCL: ", round(last(tiposCambio$cclGD), 2)), size = 3.5) +
      annotate("text", x = Sys.Date() - 15, y=250, label = paste0("MEP AL: ", round(last(tiposCambio$mepAL), 2)), size = 3.5) +
      annotate("text", x = Sys.Date() - 15, y=245, label = paste0("MEP GD: ", round(last(tiposCambio$mepGD), 2)), size = 3.5) +
      annotate("text", x = Sys.Date() - 15, y=240, label = paste0("Canje GD: ", round(last(tiposCambio$Canje) * 100, 2), "%"), size = 3.5) +
      theme( # remove the vertical grid lines
        panel.grid.major.x = element_blank() ,
        # explicitly set the horizontal lines (or they will disappear too)
        #panel.grid.major.y = element_line( size=.1, color="black" ) ) +
        panel.grid.major.y = element_blank()) +
      scale_color_manual(name = "", values = cbPalette, labels = c("CCL GD30", "MEP AL30", "MEP GD30"))

    return(list(tiposCambio, g))

  } else {
      stop("Fecha debe ser mayor a 2014-05-26")
  }
}

getPPIDLR2 = function(ticker = "GD30", type = "BONOS") {
  require(methodsPPI)
  require(lubridate)

  PPI = getPPILogin2()

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

      temp = getPPIBook(PPI$token, ticker = tickers[i], type = type, settlement = settlement[k])
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

  # Esto es para probar la funcion de manera directa
  # token = PPI$token
  # settlement = "INMEDIATA"
  # ticker ="GD30C"
  # type = "BONOS"

  url = 'https://clientapi.portfoliopersonal.com/api/1.0/'
  urlMarketData = 'MarketData/Book'

  rBook = request(paste0(url, urlMarketData)) %>%
    req_headers(Authorization = token,
                AuthorizedClient = 'API-CLI',
                ClientKey = 'pp19CliApp12',
                `User-Agent` = "http://github.com/jmtruffa"
    ) %>%
    req_method("GET") %>%
    req_url_query(Ticker = ticker, Type = type, Settlement = settlement) %>%
    req_perform()
  body = fromJSON(rawToChar(rBook$body))
  date = body$date
  bids = tibble(cbind(SIDE = rep("BID", length(body$bids$position)),(body$bids)))
  offers = tibble(cbind(SIDE = rep("OFFER", length(body$offers$position)),(body$offers)))
  #returnValue = tibble(rbind(cbind(date = rep(date, length(bids$SIDE)), bids),
  #                           cbind(date = rep(date, length(bids$SIDE)), offers) ))
  returnValue = cbind(
    date = as.Date(rep(date, length(bids$position) + length(offers$position))),
    ticker = rep(ticker, length(bids$position) + length(offers$position)),
    rbind(bids, offers))
}

getPPICurrentRofex = function() {
  #para pegarle a la API
  require(methodsPPI)
  require(dplyr)
  require(ggplot2)

  #para el grafico
  require(scales)
  require(tidyquant)
  library(ggrepel)

  secuencia = function (serie) {
    require(lubridate)
    ret = NULL
    end = Date()
    meses = c("ENE", "FEB", "MAR", "ABR", "MAY", "JUN", "JUL", "AGO", "SEP", "OCT", "NOV", "DIC")
    for (i in seq_along(serie)) {
      ret = append(ret, paste0("DLR/",meses[lubridate::month(as.Date(serie[i]))], substr(lubridate::year(as.Date(serie[i])), 3, 4)))
      end = append(end, lubridate::ceiling_date(as.Date(serie[i]), unit = "month") - 1)
    }
    ret = tibble(futuro = ret, vto = end)
    ret
  }
  serie = seq.Date(from = Sys.Date(), length.out = 12, by = "months")
  tira = secuencia(seq.Date(from = Sys.Date(), length.out = 12, by = "months"))

  PPI = getPPILogin2()

  fut = NULL
  for (vto in tira$futuro) {
    fut = append(fut, getPPIPrice(token = PPI$token,
                                  ticker = vto,
                                  type = "FUTUROS",
                                  settlement = 'INMEDIATA')$price)
  }
  # llama el scraper en Go que baja de MAE el último operado
  spot = as.numeric(system('~/dev/bin/maeScraper', intern = TRUE)[4])

  futuros = as_tibble(cbind(cbind(tira, fut), spot))

  futuros = futuros %>%
    mutate(
      tnaImplic = (fut / spot - 1) / (as.numeric(vto - Sys.Date()))*365,
      orden = row_number(),
      futuro = sub(".*/", "", futuro)
    ) %>%
    filter(fut != 0) #limpiamos si algun precio de futuro viene en 0

  # establece los límites del eje Y del gráfico
  limits = c(min(futuros$tnaImplic) * 0.5, max(futuros$tnaImplic) * 1.3)
  gRofex = futuros %>%
    ggplot(aes(x=reorder(futuro, +orden), y=tnaImplic)) +
    geom_line(color = "cornflowerblue", group = 1) +
    geom_point(color = "cornflowerblue") +
    #scale_x_continuous(labels = scales::number_format(accuracy = 1),
    #                 breaks=seq(1, 12, 1)) +
    scale_y_continuous(breaks = breaks_extended(10),
                       labels = scales::percent_format(accuracy = 0.1),
                       limits = limits)+
    labs(title = "Curva Rofex",
         subtitle = paste0('En base a último operado: ',spot[1], '. Fecha: ', Sys.time()),
         y = 'TNA',
         x = 'Vencimiento',
         caption = "Elaboración propia en base a precios Rofex provistos por PPI")+
    theme_tq() +
    theme( # remove the vertical grid lines
      panel.grid.major.x = element_blank() ,
      panel.grid.major.y = element_blank()) +
    geom_text_repel(aes(label = scales::percent(tnaImplic, accuracy=0.01)))

  return(list(futuros, gRofex))
}
