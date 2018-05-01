checkBlockers <- function(board, blocker_value = 24) {
  offsets <- colSums(board == 0)
  n <- ncol(board)
  m <- nrow(board)
  
  for(i in seq(n)) {
    blockers_to_kill <- rep(FALSE, m)
    for(j in which(board[, i] == blocker_value)) {
      blocker_pos <- j - offsets[i]
      
      removable <-  TRUE
      
      if(j < m) {
        if(i > 1) {
          removable <- removable && (m - offsets[i-1] < blocker_pos)
        }
  
        if(i < n) {
          removable <- removable && (m - offsets[i+1] < blocker_pos)
        }
      }

      if(removable) {
        br <- 3
      }
      
      blockers_to_kill[j] = removable
    }
    board[, i] <- c(rep(0, times=sum(blockers_to_kill)), board[!blockers_to_kill, i])
  }
  
  board
}