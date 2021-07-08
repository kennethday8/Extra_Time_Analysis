source("match_reader.R")

## Fn creates final output table (ot)
output_table <- function(matches, rankings, country_ids){
  time_start <- Sys.time()
  ot <- matches
  match_count <- nrow(ot)
  
  ## Use match link to read info related to last goal and subs
  ot$last_goal <- 0
  ot$t1_subs <- 0
  ot$t1_sub_mins <- 0
  ot$t2_subs <- 0
  ot$t2_sub_mins <- 0
  
  for (i in 1:match_count){
    match_link <- ot[i,6]
    match_input <- gettxt(match_link)
    match_output <- match_reader(match_input, ot[i,3], ot[i,4])
    
    ot[i,12] <- match_output[1]
    ot[i,13] <- match_output[2]
    ot[i,14] <- match_output[4]
    ot[i,15] <- match_output[3]
    ot[i,16] <- match_output[5]
  }
  
  time_end <- Sys.time()
  print((time_end - time_start))
  return(ot)
}