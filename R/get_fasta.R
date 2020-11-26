get_fasta <- function(population){
  filename <- paste0("db/", population,".txt")
  sample <- read.csv(filename, header = FALSE, fill = TRUE)
  return(paste(unlist(sample, recursive = FALSE), collapse='\n'))
}