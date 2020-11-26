library(stringr)

convert_to_fasta <- function(name, ori_seq, ori_count, rest){
  count = str_pad(1, 3, pad = "0")
  file_string <- paste('>', name, '.', count, '\n', ori_seq, sep = '')
  counter = 2
  num=2
  while(num <= ori_count){
    count = str_pad(num, 3, pad = "0")
    file_string <- paste(file_string,'\n>', name, '.', count, '\n', ori_seq, sep = '')
    num = num+1
  }
  for (i in seq_along(rest)){
    num = 0
    while (num < rest[[i]][3]){
      count = str_pad(counter, 3, pad = '0')
      file_string <- paste(file_string, '\n> ', name, '.', count, 
                           '\n', rest[[i]][4], sep = '')
      counter = counter+1
      num = num+1
    }
    
  }
  
  return (file_string)
}