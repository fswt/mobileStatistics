load_data <- function(filepath) {
  data <- read.table(filepath, header = TRUE, sep = "|")
  while ("data" %in% search()) {
    detach(data)
  }
  attach(data)
  return(data)
}
