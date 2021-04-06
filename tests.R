library(chess)
library(httr)
library(magrittr)



chess::install_chess()

file <- system.file("harmon.pgn", package = "chess")
harmon_borgov <- read_game(file)



endpoint <- "https://explorer.lichess.ovh/master?fen="
startingpos <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
url <- URLencode(paste0(endpoint, startingpos))

dt <- httr::GET(url)
ct <- httr::content(dt)



line <- list.files(path = "C:/Users/cve/Downloads", pattern = "game.pgn", full.names = T )
line <- chess::read_game(line)
line


fischer_sherwin <- game() %>%
  move(
    "e4", "c5", "Nf3", "e6", "d3", "Nc6", "g3", "Nf6", list("d5", "Nbd2", "Bd6", "Bg2",
                                                            "Nge7", "O-O", "O-O", "Nh4"), "Bg2", "Be7", "O-O", "O-O", list("d5"), "Nbd2"
  )
