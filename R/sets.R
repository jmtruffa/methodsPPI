sets = function(set, server = "local", port = 5432, ...) {
  if (set != '') {
    require(dplyr)
    require(functions)
    res = functions::dbGetTable(table = "sets", server = server, port = port, ...) %>%
      filter(nombre == set) %>%
      select(nombre, ticker, type)
  } else {
    stop("Set no puede ser NULL")
  }
  res
}

# sets = function(set, db = "") {
#   require(dplyr)
#   require(dbplyr)
#
#   if (db == "") {
#     if (stringr::str_detect(Sys.info()['nodename'], "Air")) {
#       db = "~/data/test.sqlite3"
#     } else {
#       db = '/home/juant/data/test.sqlite3'
#     }
#   }
#
#
#   if (set != '') {
#
#     con = DBI::dbConnect(RSQLite::SQLite(), dbname = db)
#     res = as_tibble(tbl(con, "sets") %>%
#                       filter(nombre == set) %>%
#                       select(nombre, ticker, type)
#     )
#
#   } else {
#     stop("Set no puede ser NULL")
#   }
#   DBI::dbDisconnect(con)
#   res
# }

