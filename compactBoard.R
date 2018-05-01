compactBoard <- function(board) {
  m <- nrow(board)
  n <- ncol(board)
  for(i in seq(n)) {
    index <- board[, i] == 24
    board[, i] <- c(rep(0, times=sum(index)), board[!index, i])
  }
  board
}