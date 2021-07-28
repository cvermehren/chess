if (!require('bigchess')) install.packages('bigchess')

library(bigchess)
library(httr)
library(data.table)
library(stringr)


# Get PGN file ------------------------------------------------------------

# Guide: 
# 1. Export opening lines as PGN from Chessbase, 
# 2. import into Chessable, 
# 3. export from Chessable and 
# 4. upload to R working directory


# From Windows PC: 
f <- list.files(path = "C:/Users/cve/Downloads", pattern = "FourKnights.pgn", full.names = T )

# From Mac
# f <- list.files(path = "/Users/cvermehren/Downloads/", pattern = "FourKnights.pgn", full.names = T)

pgn <- data.table(read.pgn(f, stat.moves = FALSE, extract.moves = 200))



# Clean PGNs --------------------------------------------------------------

# Remove characters not within the range of x20 (SPACE) and x7E (~):
# https://stackoverflow.com/questions/38828620/how-to-remove-strange-characters-using-gsub-in-r
pgn[, moves_prun := gsub('[^\x20-\x7E]', '', Movetext)]

# Remove trailing dash
pgn[, moves_prun := gsub('\\-$', '', moves_prun)]

# Remove trailing equal sign
pgn[, moves_prun := gsub('\\=$', '', moves_prun)]

# Remove leading & trailing whitespace, then remove double space
# https://stackoverflow.com/questions/25707647/merge-multiple-spaces-to-single-space-remove-trailing-leading-spaces
pgn[, moves_prun := gsub("\\s+", " ", str_trim(moves_prun))]

# Vectorize and apply san2lan
san2vec <- Vectorize(san2lan)
pgn[, uci := san2vec(moves_prun)]

# Extract uci moves
uci <- pgn[, list(uci)]


# New ---------------------------------------------------------------------

# Set defining Four Knights line: https://en.wikipedia.org/wiki/Four_Knights_Game
fourknights <- "1.e4 e5 2.Nf3 Nc6 3.Nc3 Nf6"

# line = uci$uci[1]
# opening_line = "1.e4 e5 2.Nf3 Nc6 3.Nc3 Nf6"


line_tree <- function(line, opening_line) {
  
  opening <- san2lan(opening_line)
  opening <- strsplit(opening, " ")[[1]]
  opening <- paste(opening, collapse = ",")
  
  line_frags <- strsplit(line, " ")[[1]]
  
  cumpaste = function(x, .sep = ",") Reduce(function(x1, x2) paste(x1, x2, sep = .sep), x, accumulate = TRUE)
  
  line_frags <- cumpaste(line_frags)
  
  line_frags <- line_frags[nchar(line_frags) >= nchar(opening)]
  
  
  return(line_frags)
  
}

line <- lapply(uci$uci, line_tree, opening_line = fourknights)
dt <- lapply(line, data.table)
dt <- rbindlist(dt, idcol = T)
dt <- dt[!duplicated(V1)]

dt_list <- split(dt, by = ".id")

# line <- data.table(unlist(line))
# line <- unique(line)
# line <- line$V1

# dt$V1
# line = dt[1:10]$V1
# opening_line = fourknights

opening_line <- fourknights

opening <- san2lan(opening_line)
opening <- strsplit(opening, " ")[[1]]
opening <- paste(opening, collapse = ",")


endpoint <- "https://explorer.lichess.ovh/master?fen="
startingpos <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

url <- paste0(endpoint, startingpos)

url_total_games <- URLencode(paste0(url, "&play=", opening) )
total_games <- URLencode(url_total_games)
total_games <- httr::GET(total_games)
total_games <- httr::content(total_games)
total_games <- total_games$white + total_games$draws + total_games$black



# n_line <- length(line$V1)

mylist <- list()

#i=1
# j = frag[1]


for(i in seq_along(dt_list) ) {
  
  frag <- dt_list[[i]]$V1
  
  for (j in frag) {
    
    url_move <- URLencode(paste0(url, "&play=", j) )
    n_move <- httr::GET(url_move)
    n_move <- httr::content(n_move)
    n_move <- n_move$white + n_move$draws + n_move$black
    
    if(exists("n_move")){ if(n_move / total_games < 0.01) break } 
    
    mylist[[j]] <- n_move
    
  }


    
  print(paste("Progress:", round(i/length(dt_list) * 100), "%" ))
  
  Sys.sleep(2)
    
  }
 
res <- data.table( line = names(res), frequency = unlist(mylist))

res[, rel_freq := frequency / shift(frequency, type = "lag")]

res[is.na(rel_freq), rel_freq := 1]
res[, cumprod := round(cumprod(rel_freq)*100, 2)]

keep <- res[cumprod >= 1]
keep[ startsWith( shift(line, type = "lead"), line), keep := FALSE]
keep[is.na(keep), keep := TRUE]
keep <- keep[keep == "TRUE"]
keep[, keep_pgn := gsub(",", " ", line)]

lan2vec <- Vectorize(lan2san)
keep[, keep_pgn := lan2vec(keep_pgn)]


