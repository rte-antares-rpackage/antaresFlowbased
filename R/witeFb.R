# writeFb files
.writeFb <- function(nameFb, patch, data){
  #adding 24 hours (0 : bisextile)
  data <- c(data[Name == nameFb]$vect_b, rep(0,24))
  data <- cbind(data, 0, 0)
  write.table(data, file = paste0(patch, "/", nameFb, ".txt"), sep = "\t",
              row.names = FALSE, col.names = FALSE)
}
