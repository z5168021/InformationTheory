library("stringr")

get_population_data <- function(population, user_supplied){
  
  haploids <- get_haploids(population, user_supplied)
  return(hash_data(haploids))
}

get_haploids <- function(pop, user_supplied){
  
  if (user_supplied == FALSE){
    filename <- paste("db/", pop,".txt", sep = "")
    sample <- read.table(filename, header = FALSE)
  } else {
    sample <- read.csv(pop, header = FALSE, fill = TRUE, stringsAsFactors=FALSE)
  }
  
  sample <- unlist(sample, recursive = FALSE)
  i = length(sample)
  while(i > 0){
    if (grepl("^>",sample[i])) sample <- sample[-i]
    i = i - 1
  }
  return(sample)
}

hash_data <- function(pop_data){
  hash <-list()
  hash[["size"]]<- length(pop_data)
  for(i in pop_data){
    if (i %in% names(hash)) {
      hash[[i]] = as.integer(hash[[i]] + 1) 
    } else {
      hash[[i]] <- 1
    }
  }
  return(hash)
}
