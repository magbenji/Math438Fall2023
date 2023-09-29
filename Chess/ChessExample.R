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

#let's extract the engine results into a data frame
resMatrix <- vapply(results,function(x){strsplit(x," ")[[1]][c(10,20)]}, c(CP = 0, Move = "Nf6"))
attr(resMatrix, "dimnames") <- NULL
resData <- data.frame(CP = as.numeric(t(resMatrix)[,1]), Move = t(resMatrix)[,2])
resData

resData$CPL <- resData$CP - resData$CP[[1]]

#to get the last move in UCI notation
game$uci() 

#to get the last move in SAN notation
game$san()

resData$CPL[resData$Move == game$uci()] #centipawn loss
which(resData$Move == game$uci()) #move rank


parseFish <- function(x){
  v <- strsplit(x," ")[[1]]
  l <- which(v == "cp") + 1
  m <- which(v == "pv") + 1
  if(length(l) == 0){
    l <- which(v == "mate") + 1
    v[l] <- -Inf
  }
  c("CP"  = v[l], "Move" = v[m])
}

library(dplyr)

gameEval <- data.frame()

while(! is.null(forward(game))){
  #determine the number of lines for the engine to evaluate, max 30
  game %>% moves() %>% length() -> legalMoves
  nLines <- min(30,legalMoves)
  engine$setoption("MultiPV", nLines)
  
  #set the position for the engine to analyze
  engine$position(fen(game))
  
  #evaluate the position at a certain depth
  engine$go(depth=20)
  
  #save the results for the n-lines you asked for
  results <- tail(engine$output, nLines)
  if(grepl("bestmove", results[nLines])){ 
    results <- tail(engine$output,nLines + 1)
    results <- results[-length(results)]
  }
  
  #stop the engine so that you evaluate the next position
  engine$stop()
  
  #let's extract the engine results into a data frame
  resMatrix <- vapply(results,parseFish, c(CP = 0, Move = "Nf6"))
  attr(resMatrix, "dimnames") <- NULL
  resData <- data.frame(CP = as.numeric(t(resMatrix)[,1]), Move = t(resMatrix)[,2])
  
  #calculate CPL
  resData$CPL <- resData$CP - resData$CP[[1]]
  #NaNs are produced when both the top move and considered move lead to mate
  #I will arbitrarily score them as zeroes
  resData$CPL[is.nan(resData$CPL)] <- 0
  
  game <- forward(game)
  Rank <- which(resData$Move == game$uci())
  
  #if the move wasn't in the top n principle variations...
  if(length(Rank) == 0){
    Rank <- Inf
    engine$position(fen(game))
    engine$setoption("MultiPV", 1)
    engine$go(depth = 20)
    results <- tail(engine$output, 1)
    if(grepl("bestmove", results)){ 
      results <- tail(engine$output,2)
      results <- results[-2]
    }
    engine$stop()
    CPval <- -1*as.numeric(parseFish(results)["CP"])
    moveCPL <- CPval - resData$CP[[1]]
    resData <- bind_rows(resData, data.frame(CP = CPval, Move = game$uci(), CPL = moveCPL))
  }
  
  
  data <- data.frame("Ply" = game$ply(),
            "Move" = game$san(),
            "CPL" = resData$CPL[resData$Move == game$uci()],
            "Rank" = Rank,
            "nMoves" = legalMoves
  )
  gameEval %>% bind_rows(data) -> gameEval
}


