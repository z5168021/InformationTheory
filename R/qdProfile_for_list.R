qdProfile_for_list <- function (population_list){
  q_profile <- data.frame(qd_0 = vector(), qd_1 = vector(), qd_2 = vector())
  for (row in 1:nrow(population_list)) {
    datdat <- get_population_data(population_list[row,]$pop_name, population_list[row,]$type) # THSI IS THE PROBLEM LINE
    datdat <- datdat[-1]
    
    sequences <- names(datdat)
    seq_count <- as.numeric(as.character(datdat))
    
    # Entropy q = 0
    entropy_0 <- length(sequences) - 1
    
    # Entropy q = 1
    entropy_1 <- 0
    
    for (count in seq_count) {
      proportion <- count/sum(seq_count)
      if (proportion != 0) {
        entropy_1 <- entropy_1 + (proportion * log(proportion, base = exp(1)))
      }
    }
    entropy_1 <- -1 * entropy_1
    
    # Entropy q = 2
    entropy_2 <- 0
    for (count in seq_count) {
      proportion <- count/sum(seq_count)
      if (proportion != 0) {
        entropy_2 <- entropy_2 + proportion^2
      }
    }
    entropy_2 <- 1 - entropy_2 
    
    
    # qD-value q = 0
    qd_0 <- length(seq_count)
    
    # qD-value q = 1
    qd_1 <- exp(entropy_1)
    
    # qD-value q = 2
    qd_2 <- 1/(1-entropy_2)
    
    
    qd_values <- data.frame(qd_0, qd_1, qd_2)
    q_profile <- rbind(q_profile, qd_values)
  }
  return (q_profile)
}