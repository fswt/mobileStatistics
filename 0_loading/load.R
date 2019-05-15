load_data <- function(filepath) {
  data <- read.table(filepath, header = TRUE, sep = "|")
  return(data)
}
