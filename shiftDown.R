dropBoard <- function(board, cols = 1:ncol(board), amounts = NULL) {
  
  shiftDown <- function(x, by) {
    n <- length(x)
    if(by == 0)
      return(x)
    
    if(by > n)
      return(rep(0, times=n))
    
    c(rep(0, times=by), x[-(((n-by)+1):n)])
  }
  
  nr <- nrow(board)
  
  for(i in seq(length(cols))) {
    if(is.null(amounts)) {
      shift <- match(TRUE, board[nr:1, cols[i]] > 0) - 1
    } else {
      shift <- amounts[i]
    }
    
    board[, cols[i]] <- shiftDown(board[, cols[i]], shift)
  }

  board
}