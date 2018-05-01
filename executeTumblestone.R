executeTumblestone <- function(moves, jRobot, start_pos) {
  n_moves <- nrow(moves)
  current_pos <- start_pos
  
  key_left <- 37L
  key_right <- 39L
  key_space <- 32L
  
  pushKey <- function(key) {
    jRobot$keyPress(key)
    jRobot$keyRelease(key)
    Sys.sleep(0.05)
  }
  
  moveTo <- function(newPos) {
    dist_pos <- current_pos - newPos
    
    if(dist_pos < 0) {
      for(j in 1:-dist_pos) {
        pushKey(key_right)
      }
    } else if(dist_pos > 0) {
      for(j in 1:dist_pos) {
        pushKey(key_left)
      }
    }
    
    current_pos <<- newPos
  }
  
  for(m in 1:n_moves) {
    cat(sprintf("\n\nMove no. %d...\n", m))
    for(i in 1:3) {
      
      cat(sprintf("Grabbing stone at %d...\n", moves[m, i]))
      
      moveTo(moves[m, i])

      pushKey(key_space)
      
      Sys.sleep(0.2)
    }
  }
  
  moveTo(3)
  
  cat("Ringelingding! All done.")
}