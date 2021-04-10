install.packages("bigchess")

library(bigchess)
library(httr)
library(data.table)

# PC: f <- list.files(path = "C:/Users/cve/Downloads", pattern = "Dash.pgn", full.names = T )
f <- list.files(path = "/Users/cvermehren/Downloads/", pattern = "games.pgn", full.names = T)
pgn <- data.table(read.pgn(f, stat.moves = FALSE, extract.moves = 100))

# Keep only alphanumeric, space, dash (0-0) - then remove trailing dash
pgn[, moves_prun := gsub("[^[:alnum:][:space:]\\. \\-]", "", Movetext)]
pgn[, moves_prun := gsub('\\-$', '', moves_prun)]

# Vectorize fun and implement
san2vec <- Vectorize(san2lan)
pgn[, uci := san2vec(moves_prun)]

uci <- pgn[, list(uci)]

move <- uci$uci[1]

chess_fun <- function(move) {
  
  endpoint <- "https://explorer.lichess.ovh/master?fen="
  startingpos <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  url <- paste0(endpoint, startingpos)
  
  url_move <- URLencode(paste0(url, "&play=", move) )
  
  dt <- httr::GET(url_move)
  ct <- httr::content(dt)
  
  
  
}

