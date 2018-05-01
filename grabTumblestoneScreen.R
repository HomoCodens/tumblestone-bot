grabTumblestoneScreen <- function(jRobot) {
  library(magick)
  library(imager)
  
  jRobot$keyPress(18L)
  jRobot$keyPress(154L)
  jRobot$keyRelease(154L)
  jRobot$keyRelease(18L)
  Sys.sleep(0.3);
  
  rawdata <- readClipboard(8, TRUE)
  magick_image <- image_read(rawdata)
  imager_image <- magick2cimg(magick_image)
  RGBtoHSV(imager_image)
}