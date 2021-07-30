if (!require('bigchess')) install.packages('bigchess')

library(bigchess)
library(httr)
library(data.table)
library(stringr)

# Guide: 
# 1. Export opening lines as PGN from Chessbase, 
# 2. import into Chessable, 
# 3. export from Chessable and 
# 4. upload to R working directory


# Settings ----------------------------------------------------------------

pgn_file <- "C:/Users/cve/Dropbox/Chess Repertoire/Black/queens_gambit_declined_lines.pgn"
pgn <- data.table(read.pgn(pgn_file, stat.moves = FALSE, extract.moves = 0))

opening_name <- pgn[1]$White

defining_line <- pgn$Movetext
defining_line <- substr(defining_line[1],1,which.max(apply(do.call(rbind,lapply(strsplit(defining_line,''),`length<-`,nchar(defining_line[1]))),2,function(i)!length(unique(i))==1))-1)
defining_line <- gsub('\\.$', '', defining_line)
defining_line <- gsub('\\d+$', '', defining_line)
defining_line <- gsub('\\s+$', '', defining_line)


# Cleansing ---------------------------------------------------------------

# Remove characters not within the range of x20 (SPACE) and x7E (~): https://st3ackoverflow.com/questions/38828620/how-to-remove-strange-characters-using-gsub-in-r
#pgn[, moves_prun := gsub('[^\x20-\x7E]', '', Movetext)]


pgn[, moves_prun := gsub('[^\x20|\x2B|\x2E|\x30-\x39|\x3D|N|B|K|R|Q|a-h|O-]', '', Movetext)]

# Remove trailing signs
pgn[, moves_prun := gsub('\\-$', '', moves_prun)]
pgn[, moves_prun := gsub('\\=$', '', moves_prun)]
pgn[, moves_prun := gsub('\\=\\s+', ' ', moves_prun)]
pgn[, moves_prun := gsub('\\-\\+$', '', moves_prun)]
pgn[, moves_prun := gsub('\\+\\-$', '', moves_prun)]

# Remove leading & trailing whitespace, then remove double space: https://stackoverflow.com/questions/25707647/merge-multiple-spaces-to-single-space-remove-trailing-leading-spaces
pgn[, moves_prun := gsub("\\s+", " ", str_trim(moves_prun))]

# Convert to UCI notation (engines)
san2vec <- Vectorize(san2lan)
pgn[, uci := san2vec(moves_prun)]

# pgn[23]$Movetext
# pgn[23]$moves_prun


# Functions ---------------------------------------------------------------

get_variation_levels <- function(variation, defining_line) {
  
  defining_line_uci <- san2lan(defining_line)
  defining_line_uci <- strsplit(defining_line_uci, " ")[[1]]
  defining_line_uci <- paste(defining_line_uci, collapse = ",")
  
  cumpaste_fun = function(x, .sep = ",") Reduce(function(x1, x2) paste(x1, x2, sep = .sep), x, accumulate = TRUE)
  
  variation_levels <- strsplit(variation, " ")[[1]]
  variation_levels <- cumpaste_fun(variation_levels)
  variation_levels <- variation_levels[nchar(variation_levels) >= nchar(defining_line_uci)]
  
  return(variation_levels)
  
}


# Create variation levels -------------------------------------------------

variation_levels <- lapply(pgn$uci, get_variation_levels, defining_line = defining_line)
variation_levels <- lapply(variation_levels, data.table)
variation_levels <- rbindlist(variation_levels, idcol = T)

setnames(variation_levels, c(".id", "V1"), c("line", "variation_level"))

variation_levels <- variation_levels[!duplicated(variation_level)]
variation_levels <- split(variation_levels, by = "line")


# Lichess API settings ----------------------------------------------------

endpoint <- "https://explorer.lichess.ovh/master?fen="
startingpos <- "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1"

url <- paste0(endpoint, startingpos)


# Total opening games -----------------------------------------------------

url_total_games <- san2lan(defining_line)
url_total_games <- strsplit(url_total_games, " ")[[1]]
url_total_games <- paste(url_total_games, collapse = ",")
url_total_games <- URLencode(paste0(url, "&play=", url_total_games) )

total_games <- URLencode(url_total_games)
total_games <- httr::GET(total_games)
total_games <- httr::content(total_games)
total_games <- total_games$white + total_games$draws + total_games$black


# Extract statistics ------------------------------------------------------

print("Extracting move statistics from lichess.org master games collection")

stats_list <- list()

pb <- txtProgressBar(min = 0, max = length(variation_levels), style = 3)

for(i in seq_along(variation_levels)) {
  
  var_levels <- variation_levels[[i]]$variation_level
  
  for (j in var_levels) {
    
    url_move <- URLencode(paste0(url, "&play=", j) )
    n_move <- httr::GET(url_move)
    n_move <- httr::content(n_move)
    n_move <- n_move$white + n_move$draws + n_move$black
    
    if(length(n_move) == 0) break
    
    #if(exists("n_move")){ if(n_move / total_games < 0.001) break } 
    
    stats_list[[j]] <- n_move
    
  }
  
  setTxtProgressBar(pb, i)
  
  Sys.sleep(2)
  
}

res <- data.table( line = names(stats_list), frequency = unlist(stats_list))


res[startsWith(line, shift(line, type = "lag")) , line_no := 0 ]
res[is.na(line_no) , line_no := 1 ]
res[, line_no := cumsum(line_no) ]


res[, rel_freq := frequency / shift(frequency, type = "lag"), by = line_no]
res[is.na(rel_freq), rel_freq := frequency/max(frequency)]

res[, cumprod := round(cumprod(rel_freq)*100, 2), by = line_no]

# saveRDS(res, "4Knights-White.Rds")
# res <- readRDS("4Knights-White.Rds")

keep <- res[cumprod >= 1]
keep[ startsWith( shift(line, type = "lead"), line), keep := FALSE]
keep[is.na(keep), keep := TRUE]
keep <- keep[keep == "TRUE"]
keep[, keep_pgn := gsub(",", " ", line)]

lan2vec <- Vectorize(lan2san)
keep[, keep_pgn := lan2vec(keep_pgn)]
keep$keep_pgn

keep$rank <- 1:nrow(keep)

keep[, event := ""]
keep[, site := ""]
keep[, date := ""]
keep[, round := ""]
keep[, white := "QGD"]
keep[, black := paste("Line", rank)]
keep[, result := ""]

df= data.table(keep)

file = "test.pgn"
add.tags = NULL
append = FALSE

i=1

pgn_save <- function (df, file, add.tags = NULL, append = FALSE) 
{
  tags <- c("Event", "Site", "Date", "Round", "White", "Black", "Result")
  
  write("", file = file, append = append)
  
  for (i in 1:nrow(df)) {
    
    tmp <- df[i, ]
    
    for (t in tags) { write(paste0("[", t, " \"", as.character(tmp[[t]]), "\"]"), file = file, append = TRUE) }
    
    write(paste(as.character(tmp$keep_pgn), as.character(tmp$white), as.character(tmp$black), "\n"), file = file, append = TRUE)
  }
}

