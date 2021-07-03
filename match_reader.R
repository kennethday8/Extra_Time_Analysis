match_reader <- function(match, team2, score){
  ## Isolate match info text
  match_start <- str_locate_all(match, pattern = paste(team2, "\n\n", score, sep = ""))
  match_start_n <- match_start[[1]][1,1]
  match_end <- str_locate_all(match, pattern = "\n\nManager: ")
  match_end_n <- match_end[[1]][1,1]
  match <- substr(match, match_start_n, match_end_n)
  
  match_start <- str_locate_all(match, pattern = "\n\ngoals\n\n")
  match_start_n <- match_start[[1]][1,2] + 1
  match_end_n <- str_length(match) - 1
  match <- substr(match, match_start_n, match_end_n)
  
  return(match)
}