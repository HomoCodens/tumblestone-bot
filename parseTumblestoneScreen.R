parseTumblestoneScreen <- function(screen, show=FALSE, mode = "n") {
  # w <- width(screen)
  # h <- height(screen)
  
  # screen <- crop.borders(screen, 0.425*w, 0.198*h)
  
  screen <- switch(mode,
                   "n" = imsub(screen, x > 0.425*width, x < 0.151*width, y > 0.198*height, y < 0.63*height),
                   "r1" = imsub(screen, x > 0.317*width, x < 0.151*width, y > 0.198*height, y < 0.63*height))
  
  w <- width(screen)
  h <- height(screen)
  
  # if(w %% 2 != 0) {
  #   screen <- imsub(screen, x > 1)
  #   w <- w-1
  # }
  
  n <- 10
  m <- 24
  block_dim <- floor(h/m)
  block_dim_2 <- floor(block_dim/1.5)
  sample_dim <- floor(0.3*block_dim)
  sample_range <- -sample_dim:sample_dim
  
  # beam_x <- match(0, at(screen, 1:w, h))
  # beam_y <- h - match(TRUE, at(screen, beam_x, h:1) > 0)
  # 
  # m <- ceiling(beam_y/block_dim)
  
  board_h <- matrix(0, m, n)
  board_s <- matrix(0, m, n)
  board_v <- matrix(0, m, n)
  
  for(i in seq(n)) {
    x_offset <- (i-1)*block_dim + block_dim_2
    for(j in seq(m)) {
      y_offset <- (j-1)*block_dim + block_dim_2
      board_h[j, i] <- median(at(screen, x_offset + sample_range, y_offset + sample_range))
      board_s[j, i] <- mean(at(screen, x_offset + sample_range, y_offset + sample_range, cc=2))
      board_v[j, i] <- mean(at(screen, x_offset + sample_range, y_offset + sample_range, cc=3))
    }
  }
  
  h_levels <- c(5, 24, 40, 110, 230, 263)
  tolerance <- 10
  
  # board_hist <- hist(board, plot=FALSE)
  # levels <- board_hist$mids[board_hist$counts > 0]
  # cluster_width <- board_hist$breaks[2]
  
  board_4 <- matrix(0, m, n)
  
  sapply(h_levels, function(x){board_4[abs(board_h - x) <= tolerance] <<- x})
  board_4[board_v < 0.42 & board_s > 0.42] <- 0
  
  if(show) {
    plot(HSVtoRGB(screen))
    for(i in seq(n)) {
      x_offset <- (i-1)*block_dim + block_dim_2
      for(j in seq(m)) {
        y_offset <- (j-1)*block_dim + block_dim_2
        if(board_4[j, i] > 0) {
          color = switch(as.character(board_4[j, i]),
                       "5" = "red",
                       "24" = "gray",
                       "40" = "yellow",
                       "110" = "green",
                       "230" = "blue",
                       "263" = "purple")
          rect(x_offset - sample_dim, y_offset - sample_dim, x_offset + sample_dim, y_offset + sample_dim, col=color)
        }
      }
    }
  }
  
  gray_rows <- rowSums(board_4 == 24) >= 0.6*n
  if(any(gray_rows)) {
    board_4 <- rbind(board_4[-which(gray_rows),], matrix(0, sum(gray_rows), n))
  }
  
  for(i in seq(1, n, 2)) {
    zero_rows <- rowSums(board_4[, i:(i+1)]) == 0
    if(any(zero_rows)) {
      board_4[, i:(i+1)] <- rbind(board_4[-which(zero_rows), i:(i+1)], matrix(0, sum(zero_rows), 2))
    }   
  }
        
  board <- matrix(0, m/2, n/2)
  for(i in seq(1, m, 2)) {
    for(j in seq(1, n, 2)) {
      boardvals <- board_4[i:(i+1), j:(j+1)]
      u_boardvals <- unique(c(boardvals))
      board[ceiling(i/2), ceiling(j/2)] <- u_boardvals[which.max(tabulate(match(boardvals, u_boardvals)))]
    }
  }
  
  board <- dropBoard(board)
  blocked_cols_even <- which(board[m/2, ] == 24)
  
  white_runs <- sapply(1:w, function(x){rle(at(screen, x=x, cc=1) <= 50 & at(screen, x=x, cc=3) >= 0.95)})
  white_run_lengths <- sapply(white_runs["lengths",], length)
  odd_blocker_borders <- which(white_run_lengths > 10)
  n_odd_blocker_borders <- length(odd_blocker_borders)
  if(n_odd_blocker_borders %% 2 != 0)
    stop("Whoopsie, something is too bright here!")
  dim(odd_blocker_borders) <- c(2, n_odd_blocker_borders/2)
  odd_blocker_centers <- colSums(odd_blocker_borders)/2
  blocked_cols_odd <- ceiling(findInterval(odd_blocker_centers, seq(1, w, block_dim))/2)
  if(any(is.na(blocked_cols_odd))) {
    blocked_cols_odd <- integer(0)
  }
  
  # TODO: Clean up blockers some other way!
  list(board = checkBlockers(board), blocked_cols_even = blocked_cols_even, blocked_cols_odd = blocked_cols_odd)
}