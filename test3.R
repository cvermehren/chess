library(bigchess)
library(httr)
library(data.table)

f <- list.files(path = "C:/Users/cve/Downloads", pattern = "Dash.pgn", full.names = T )
pgn <- read.pgn(f, stat.moves = FALSE, extract.moves = 100)

pgn <- pgn$Movetext[1]

inp <- gsub("[^[:alnum:][:space:]\\. \\-]", "", pgn)

uci <- san2lan(inp)
uci <- strsplit(uci, " ")[[1]]

endpoint <- "https://explorer.lichess.ovh/master?fen="
startingpos <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
url <- paste0(endpoint, startingpos)

w_move <- paste0(uci[1], ",", uci[2], ",", uci[3])

url_move <- URLencode(paste0(url, "&play=", w_move) )

dt <- httr::GET(url_move)
ct <- httr::content(dt)
