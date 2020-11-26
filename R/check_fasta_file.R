check_fasta_file <- function(file){
  ext <- tools::file_ext(file$name)
  if(ext == "fasta" || ext =="FNA"){
    return (TRUE)
  }
    return (FALSE)
}