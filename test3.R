library(bigchess)
library(httr)
library(data.table)

f <- list.files(path = "C:/Users/cve/Downloads", pattern = "Dash.pgn", full.names = T )
pgn <- read.pgn(f, stat.moves = FALSE, extract.moves = 100)

pgn <- pgn$Movetext[1]

inp <- gsub("[^[:alnum:]. ]", "", pgn)

inp <- gsub("[^\\w\\-\\s:alnum:]|\\d", "", pgn, perl = TRUE)



uci <- san2lan(inp)
uci <- strsplit(uci, " ")[[1]]


san2lan("1.e4 e5 2.Nf3 Nc6 3.Bb5 f5 4.Qe2 fxe4 5.Bxc6 dxc6 6.Qxe4 Bd6 7.Nxe5 Nf6 8.Qe2 O-O 9.d4 Re8 10.Be3 Bxe5 11.dxe5 Rxe5")

san2lan("1.e4 e5 2.Nf3 Nc6 3.Bb5 f5 4.Qe2 fxe4 5.Bxc6 dxc6 6.Qxe4 Bd6 7.Nxe5 Nf6 8.Qe2 OO 9.d4 Re8 10.Be3 Bxe5 11.dxe5 Rxe5")

endpoint <- "https://explorer.lichess.ovh/master?fen="
startingpos <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
url <- paste0(endpoint, startingpos)

w_move <- "e2e4"

url_move <- URLencode(paste0(url, "&play=", w_move) )




dt <- httr::GET(url_move)
ct <- httr::content(dt)


san2lan("1. e4 e5 2. Nf3 Nf5 3. d5 ")


# b_move <- "c7c5"
# Example
# https://explorer.lichess.ovh/master?fen=rnbqkbnr%2Fpppppppp%2F8%2F8%2F8%2F8%2FPPPPPPPP%2FRNBQKBNR%20w%20KQkq%20-%200%201&play=e2e4%2Cc7c5%2Cg1f3%2Cd7d6%2Cd2d4%2Cc5d4%2Cf3d4%2Cg8f6%2Cb1c3%2Ca7a6%2Cc1e3%2Ce7e5%2Cd4b3%2Cc8e6%2Cf2f3
