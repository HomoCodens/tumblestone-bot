solveTumblestone <- function(board, blocked_cols_even, blocked_cols_odd, moves = NULL, level = 0) {
  cat(sprintf("Hi, I'm level %d...\n", level))
  
  if(all(board == 0)) {
    cat(sprintf("Solved at level %d!\n", level))
    return(list(solved = TRUE, moves = moves))
  }
  
  d <- dim(board)
  runs <- sapply(seq(d[2]), function(x){rle(board[, x])})
  board_scores <- cbind(sapply(seq(d[2]), function(x){unlist(sapply(unlist(runs["lengths", x]), seq))}))
  board_scores[board == 0] <- 0
  
  lastrow_board <- board[d[1],]
  lastrow_scores <- board_scores[d[1],]
  
  score <-  0
  color <- 0
  move <- c(0, 0, 0)
  even_move <- level %% 2 == 0
  any_blocked <- length(blocked_cols_even) > 0 || length(blocked_cols_odd) > 0
  
  for(i1 in seq(d[2])) {
    score1 <- min(3, lastrow_scores[i1])
    
    if(any_blocked) {
      score1 <- min(1, score1)
    }
    color <- lastrow_board[i1]
    
    if(color == 0 || (i1 %in% blocked_cols_even && even_move) || i1 %in% blocked_cols_odd && !even_move) {
      next
    }
    
    for(s1 in seq(score1, 1)) {
      score <- s1
      move <- c(rep(i1, s1), rep(0, 3-s1))
      

      if(score >= 3) {
        cat(sprintf("Got move %s!\n", paste(move, collapse=", ")))
        res <- solveTumblestone(dropBoard(board, i1, s1), blocked_cols_even, blocked_cols_odd, rbind(moves, move), level+1)
        
        if(res$solved) {
          cat(sprintf("Zooms (level %d)!\n", level))
          return(res)
        }
      } else {
        
        i2_range <- integer(0)
        if(any_blocked) {
          i2_range <- seq(d[2])
        } else if(i1 < d[2]) {
          i2_range <- seq(i1+1, d[2])
        }
        
        for(i2 in i2_range) {
          if(lastrow_board[i2] == color && !(i2 %in% blocked_cols_even && !even_move) && !(i2 %in% blocked_cols_odd && even_move)) {
            
            if(any_blocked) {
              if(i1 == i2 && lastrow_scores[i2] - s1 <= 0) {
                next
              } else {
                score2 <- min(1, lastrow_scores[i2])
              }
            } else {
              score2 <- min(3-s1, lastrow_scores[i2])
            }
            
            for(s2 in seq(score2, 1)) {
              score <- s1 + s2
              move[seq(s1 + 1, s1 + s2)] <- i2
              
              if(score >= 3) {
                cat(sprintf("Got move %s!\n", paste(move, collapse=", ")))
                res <- solveTumblestone(dropBoard(board, c(i1, i2), c(s1, s2)), blocked_cols_even, blocked_cols_odd, rbind(moves, move), level+1)
                
                if(res$solved) {
                  cat(sprintf("Zooms (level %d)!\n", level))
                  return(res)
                }
              } else {
                
                i3_range <- integer(0)
                if(any_blocked) {
                  i3_range <- seq(d[2])
                } else if(i2 < d[2]) {
                  i3_range <- seq(i2+1, d[2])
                }
                
                for(i3 in i3_range) {
                  if(lastrow_board[i3] == color && !(i3 %in% blocked_cols_even && even_move) && !(i3 %in% blocked_cols_odd && !even_move)) {
                    
                    if((i1 == i3 && lastrow_scores[i3] - s1 <= 0) || (i2 == i3 && lastrow_scores[i3] - s1 - s2 <= 0)) {
                      next
                    }
                    
                    move[3] <- i3
                    cat(sprintf("Got move %s!\n", paste(move, collapse=", ")))
                    res <- solveTumblestone(dropBoard(board, c(i1, i2, i3), c(s1, s2, 1)), blocked_cols_even, blocked_cols_odd, rbind(moves, move), level+1)
                    
                    if(res$solved) {
                      cat(sprintf("Zooms (level %d)!\n", level))
                      return(res)
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  
  cat(sprintf("Nothing here, moving back up to %d...\n", level-1))
  list(solved=FALSE)
}