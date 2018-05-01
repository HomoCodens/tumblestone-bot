# library(rJava)
# .jinit()
# jRobot <- jRobot <- .jnew("java/awt/Robot")

handle <- getWindowsHandles("all")$Tumblestone

if(is.null(handle))
  stop("Tumblestone window not found!")

arrangeWindows("minimize", list(handle), outer=TRUE)
arrangeWindows("restore", list(handle), outer=TRUE)

Sys.sleep(0.51)

#for(i in 1:9) {
  # jRobot$keyPress(32L)
  # jRobot$keyRelease(32L)
  # Sys.sleep(3)
  
  scr <- grabTumblestoneScreen(jRobot)
  b <- parseTumblestoneScreen(scr, TRUE, "n")
  
  bt <- table(b$board)
  if(any((bt[-match(c("0", "24"), names(bt), nomatch=0)] %% 3) > 0)) {
    stop("Invalid board!")
  }
  solution <- solveTumblestone(b$board, b$blocked_cols_even, b$blocked_cols_odd)
  executeTumblestone(solution$moves, jRobot, 3)
  #Sys.sleep(5)
#}