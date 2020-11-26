calculate_qdProfile <- function(a ,c, g, t){
  
  slider_values <- c(a, c, g, t)

  # calculate entropy values
  entropy_0 <- calculate_entropy_0(slider_values)
  entropy_1 <- calculate_entropy_1(slider_values)
  entropy_2 <- calculate_entropy_2(slider_values)
  
  # convert to q-profile values


  qd_0 <- entropy_0 + 1
  qd_1 <- exp(entropy_1)
  qd_2 <- 1/(1-entropy_2)

  return(c(qd_0, qd_1, qd_2))
  
}

# calculate entropy for q = 0
calculate_entropy_0 <- function(slider_values) {
  entrop_0 <- 0
  for (base in slider_values) {
    if (base != 0) {
      entrop_0 <- sum(entrop_0, 1)
    }
  }
  entrop_0 <- entrop_0 - 1
  return(entrop_0)
}

# calculate entropy for q = 1
calculate_entropy_1 <- function(slider_values) {
  entrop_1 <- 0
  for (base in slider_values) {
    proportion <- base/sum(slider_values)
    if (proportion != 0) {
      entrop_1 <- entrop_1 + (proportion * log(proportion, base = exp(1)))
    }
  }
  entrop_1 <- -1 * entrop_1
  return(entrop_1)
}

# calculate entropy for q = 2
calculate_entropy_2 <- function(slider_values) {
  entrop_2 <- 0
  for (base in slider_values) {
    proportion <- base/sum(slider_values)
    if (proportion != 0) {
      entrop_2 <- entrop_2 + proportion^2
      
    }
  }
  entrop_2 <- 1 - entrop_2 
  return(entrop_2)
}