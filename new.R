m <- "1. e4 e5 2. Nf3 Nc6 3. Bb5 f5 4. Nc3 (4. Qe2 fxe4 5. Bxc6 dxc6 6. Qxe4 Bd6 7. d3 (7. Nxe5 Nf6 8. Qe2 O-O 9. d4 Re8 10. Be3 Bxe5 11. dxe5 Rxe5 $11) 7... Nf6 8. Qh4 Bg4 9. Ng5 Qd7 10. h3 Bf5 11. Nc3 O-O-O 12. Qa4 Kb8 13. Be3 b6 14. O-O-O Rhf8 $13) (4. Bxc6 dxc6 5. Nxe5 Qd4 6. Qh5+ g6 7. Nxg6 hxg6 8. Qxg6+ Kd8 9. d3 Ne7 10. Bg5 Qxb2 $17) (4. d4 fxe4 5. Nxe5 (5. Bxc6 dxc6 6. Nxe5 Nf6 7. Bg5 Be7 8. Nc3 Bf5 9. O-O O-O 10. Re1 c5 11. d5 h6 12. Bh4 Nxd5 13. Bxe7 Nxe7 14. Nxe4 Qxd1 15. Raxd1 Ng6 $11) 5... Nxe5 6. dxe5 c6 7. Nc3 cxb5 8. Nxe4 d5 9. exd6 Nf6 10. Bg5 (10. O-O Nxe4 11. Qh5+ g6 12. Qe5+ Kf7 13. Qxh8 Nf6 14. Bh6 Be6 15. Qxf8+ Qxf8 16. Bxf8 Rxf8 17. Rad1 Rd8 18. Rd4 Nd5 $11) (10. Qd4 Nxe4 11. Qxe4+ Kf7 12. Qd5+ Kg6 13. Bf4 Qe8+ 14. Be5 Qc6 $17) 10... Qa5+ 11. Nc3 b4 $13) (4. exf5 e4 5. Qe2 (5. Ng1 Qg5 6. g4 (6. g3 Qxf5 $15) 6... Nf6 7. h3 Qh4 $36) (5. Bxc6 dxc6 6. Ne5 Bxf5 7. O-O Qd4 8. Ng4 O-O-O $15) 5... Qe7 6. Bxc6 dxc6 7. Nd4 Nh6 8. Nc3 (8. g4 g6 9. Nc3 gxf5 10. gxf5 Nxf5 11. Qh5+ Qf7 12. Qxf7+ Kxf7 13. Nde2 Nh4 $44) (8. Qh5+ Qf7 9. Qxf7+ Kxf7 10. Nc3 Nxf5 11. Nxf5 Bxf5 12. O-O Bc5 $15) 8... Nxf5 9. Nxf5 Bxf5 $15) (4. d3 fxe4 5. dxe4 Nf6 6. O-O Bc5 (6... d6 $6 7. Nc3 Be7 8. Qd3) 7. Bxc6 (7. Qd3) (7. Nc3) (7. Bg5) (7. Qe2) 7... bxc6 8. Nxe5 O-O) (4. O-O fxe4 5. Bxc6 dxc6 6. Nxe5 Qd4 7. Qh5+ (7. Ng4 h5 8. Ne3 Be6 9. Nc3 O-O-O $15) 7... g6 8. Qg5 (8. Nxg6 hxg6 9. Qxg6+ Kd8 $17) 8... Bf5 $15) 4... fxe4 5. Nxe4 Nf6 (5... d5 6. Nxe5 dxe4 7. Nxc6 Qg5 8. Qe2 Nf6 (8... Qxg2 9. Qh5+ g6 10. Qe5+ Kf7 11. Bc4+) 9. f4 Qxf4 10. d4 Qh4+ 11. g3 Qh3 12. Ne5+ c6 13. Bc4 Be6 14. Bg5) 6. Qe2 (6. Nxf6+ Qxf6 7. Bxc6 dxc6 8. Qe2 Be7 9. Nxe5 (9. Qxe5 Bg4 $132) 9... Bf5 10. d3 O-O-O 11. O-O Bd6 12. f4 Rhe8 13. d4 c5 14. Be3 cxd4 15. Bxd4 g5 $5 16. Qh5 Bxe5 17. Bxe5 Qb6+ $44) 6... d5 7. Nxf6+ gxf6 8. d4 Bg7 9. dxe5 O-O 10. Bxc6 (10. e6 $6 Ne5 $1 $15) 10... bxc6 11. e6 Re8 12. O-O Bxe6 13. Bf4 *"

# m <- gsub("(\\$[0-9]+)", "", m) # Remove $ (comments)
# m <- gsub("\\.\\s+", ".", m) # Remove space after dot
# m <- gsub("1-0","", m, perl = T)
# m <- gsub("0-1","", m, perl = T)
# m <- gsub("1/2-1/2","", m, perl = T)
# m <- gsub("\\*","", m, perl = T)
# m <- gsub("{[^}]+}","", m, perl = T)
# m <- trimws(gsub("  "," ", m))

moves <- m

pgn_movetext_regex <- '([NBKRQ]?[a-h]?[1-8]?[\\-x]?[a-h][1-8](?:=?[nbrqkNBRQK])?|[PNBRQK]?@[a-h][1-8]|--|Z0|O-O(?:-O)?|0-0(?:-0)?)|(\\{.*)|(;.*)|(\\$[0-9]+)|(\\()|(\\))|(\\*|1-0|0-1|1/2-1/2)|([\\?!]{1,2})'
moves <- paste0(moves, collapse=" ")
  
moves <- stringi::stri_match_all_regex(moves, pgn_movetext_regex)[[1]][,-1]
moves <- as.data.frame(moves, stringsAsFactors = F)

open_bracket <- which(apply(moves, 2, function(x) any(grepl("\\(", x))))
close_bracket <- which(apply(moves, 2, function(x) any(grepl("\\)", x))))


moves <- moves[, c(1, open_bracket, close_bracket)]
names(moves)[1] <- "moves"
names(moves)[2] <- "open_bracket"
names(moves)[3] <- "close_bracket"

setDT(moves)

moves[open_bracket == "(", bracket_index := 1]
moves[close_bracket == ")", bracket_index := -1]
moves[is.na(bracket_index), bracket_index := 0]

moves[, level := cumsum(bracket_index)]

max_levels <- max(moves$level)

moves[level == 0, level_0 := moves]

for (i in 1:max_levels) {moves[level == i, paste0("level_", i) := moves]}

moves[open_bracket == "(" & shift( close_bracket) == ")" , variation_stop := 1]

for (i in 1:max_levels) {moves[variation_stop == 1 & level == i , paste0("level_", i) := "START"]}


level_1 <- moves$level_1
level_1 <- level_1[!is.na(level_1)]


ind <- grep("START",level_1)

level_1a <- level_1[1:23]

df=moves[ind[1]:ind[2],]
?split

moves[is.na(V1), session := 1 ]
moves[is.na(session), session := 0 ]
moves[, session := cumsum(session)]

t.first <- moves[match(min(unique(moves$V5)), moves$V5),]

min(match(")", moves$V6), na.rm = T)

moves[9:15]$V1


?which(match(")", moves$V6))




which(moves$V6 == ")")
which(moves$V5 == "(")


pgn_parse_cv <- function(moves) {
  
  # pgn_movetext_regex <- "(\\$[0-9]+)"
  pgn_movetext_regex <- "([NBKRQ]?[a-h]?[1-8]?[\\-x]?[a-h][1-8](?:=?[nbrqkNBRQK])?|[PNBRQK]?@[a-h][1-8]|--|Z0|O-O(?:-O)?|0-0(?:-0)?)"
  # 
  # pgn_movetext_regex <- "--|Z0|O-O(?:-O)?|0-0(?:-0)?"
  
  # brackets and dollar sign (comments)
  pgn_movetext_regex <- "(\\{.*)|(;.*)|(\\$[0-9]+)|(\\()|(\\))|(\\*|1-0|0-1|1/2-1/2)|([\\?!]{1,2})"
  
  
  # pgn_movetext_regex <- '([NBKRQ]?[a-h]?[1-8]?[\\-x]?[a-h][1-8](?:=?[nbrqkNBRQK])?|[PNBRQK]?@[a-h][1-8]|--|Z0|O-O(?:-O)?|0-0(?:-0)?)|(\\{.*)|(;.*)|(\\$[0-9]+)|(\\()|(\\))|(\\*|1-0|0-1|1/2-1/2)|([\\?!]{1,2})'
  
  moves <- paste0(moves, collapse=" ")
  
  
  gsub(pgn_movetext_regex, "XXXX", moves)
  
  #moves <- stringi::stri_replace_all_regex(moves, "[\\(\\)\\{\\}]", "")
  moves <- stringi::stri_match_all_regex(moves,   pgn_movetext_regex)[[1]][,-1]
  
  # moves <- moves[!is.na(moves)]
  
  moves
  
}
