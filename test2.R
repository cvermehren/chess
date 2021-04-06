library(bigchess)
library(httr)

f <- list.files(path = "C:/Users/cve/Downloads", pattern = "Dash.pgn", full.names = T )
dt <- read.pgn(f, stat.moves = FALSE, extract.moves = 100)

endpoint <- "https://explorer.lichess.ovh/master?fen="
startingpos <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
url <- paste0(endpoint, startingpos)

w <- "e2e4"
b <- "c7c5"

url_move <- URLencode(paste0(url, "&play=", w, ",", b) )

dt <- httr::GET(url_move)
ct <- httr::content(dt)


# Example
# https://explorer.lichess.ovh/master?fen=rnbqkbnr%2Fpppppppp%2F8%2F8%2F8%2F8%2FPPPPPPPP%2FRNBQKBNR%20w%20KQkq%20-%200%201&play=e2e4%2Cc7c5%2Cg1f3%2Cd7d6%2Cd2d4%2Cc5d4%2Cf3d4%2Cg8f6%2Cb1c3%2Ca7a6%2Cc1e3%2Ce7e5%2Cd4b3%2Cc8e6%2Cf2f3
