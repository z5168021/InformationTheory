library("stringr")
library(shiny)
source("./R/get_population_data.R", local = TRUE)

calculate_shannon <- function(population, ne, t, user_supplied){
  population_data <- get_population_data(population, user_supplied)
  return( get_of_set(ne, t) * 
           get_base_entropy(population_data))

}



get_base_entropy<- function(hash_data){
  base_entropy = 0
  pop_size = hash_data[["size"]]
  sequences <- names(hash_data)
  sequences <- sequences[-1]
  for(i in sequences){
    percent <- as.integer(hash_data[[i]])/pop_size
    base_entropy = base_entropy + (percent * log(percent, 2))
  }
  return(base_entropy * -1)
}


get_of_set<-function(Ne,t){
  multiple <- Ne*2
  multiple = 1/multiple
  multiple = multiple ** t
  return (multiple)
}





