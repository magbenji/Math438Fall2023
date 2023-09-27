library(chess)
library(stockfish)

#read in the game
game <- read_game("Verony64_vs_FlexoUSA_2023.09.27.pgn")

#see the position 5 steps later
game %>% forward(5)

#save the position as in FEN notation
game %>% forward(5) %>% fen() -> pos

#start up an instance of stockfish
engine <- fish$new()

#specify how many principle variants (lines) you want to see, choosing 30
#but we need to check how many legal moves are in the position
game %>% forward(5) %>% moves() %>% length() -> legalMoves
nLines <- min(30,legalMoves)
engine$setoption("MultiPV", nLines)

#set the position for the engine to analyze
engine$position(pos)

#evaluate the position at a certain depth
engine$go(depth=20)

#save the results for the n-lines you asked for
results <- tail(engine$output, nLines)

#see the best line:
results[1]

#stop the engine so that you evaluate the next position
engine$stop()

resMatrix <- vapply(results,function(x){strsplit(x," ")[[1]][c(10,20)]}, c(CP = 0, Move = "Nf6"))
attr(resMatrix, "dimnames") <- NULL
resData <- data.frame(CP = as.numeric(t(resMatrix)[,1]), Move = t(resMatrix)[,2])
