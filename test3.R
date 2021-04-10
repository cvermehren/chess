# install.packages("bigchess")

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

# Extract uci moves
uci <- pgn[, list(uci)]
line <- uci$uci[8]

# Cumulative paste


chess_fun <- function(line) {
  
  line_frags <- strsplit(line, " ")[[1]]
  
  cumpaste = function(x, .sep = ",") Reduce(function(x1, x2) paste(x1, x2, sep = .sep), x, accumulate = TRUE)
  
  line_frags <- cumpaste(line_frags)
  
  endpoint <- "https://explorer.lichess.ovh/master?fen="
  startingpos <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"
  
  url <- paste0(endpoint, startingpos)
  
  # total_games <- URLencode(url)
  # total_games <- httr::GET(total_games)
  # total_games <- httr::content(total_games)
  # total_games <- total_games$white + total_games$draws + total_games$black
  
  mylist <- list()
  
  for (i in line_frags) {
    
    # <- line_frags[3]
    url_move <- URLencode(paste0(url, "&play=", i) )
    n_move <- httr::GET(url_move)
    n_move <- httr::content(n_move)
    n_move <- n_move$white + n_move$draws + n_move$black
    #p_move <- n_move/total_games
    
    mylist[[i]] <- n_move
    print(paste("Progress:", round(nchar(i)/max(nchar(line_frags))*100), "%" ))
    Sys.sleep(2)
    #print("Waking up")
    }


  stats <- lapply(mylist, data.table)
  stats <- rbindlist(stats, idcol = T)
  
  setnames(stats, names(stats), c("line", "frequency"))
  stats[, frequency := as.numeric(frequency)]
  stats[, cum_stats := round(frequency/max(frequency), 2)]
  
}

