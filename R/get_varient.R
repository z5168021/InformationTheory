
get_varient <- function (sequence, count){
  if(count >= nchar(sequence)*4 -1) return (NULL)
  mut_num = 0
  new = ''
  split_seq <- strsplit(sequence, '')[[1]]
  for(base in split_seq){
    i=0
    while(i < 4){
      if(base == 'A' || base == 'a') base = 'G'
      else if(base == 'G' || base == 'g') base = 'C'
      else if(base == 'C' || base == 'c')base = 'T'
      else base = 'A'
      if (i != 3) mut_num = mut_num + 1
      else new = paste(new, base, sep = '')
      i = i+1
      if (mut_num == count) {
        new = paste(new, base,sep = '')
        break
      }
      
    }
    
  }
  return(new)
}