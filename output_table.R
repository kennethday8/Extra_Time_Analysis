source("match_reader.R")

## Fn creates final output table (ot)
output_table <- function(matches, rankings, country_ids, comp_rankings){
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
  
  ## Insert columns for country id and FIFA rankings
  
  ## Remove unnecessary FIFA-country column from country_ids
  country_ids <- country_ids[,-2]
  
  ot <- merge(ot, country_ids, by.x = "Team1", by.y = "country", all.x = TRUE, sort = FALSE)
  colnames(ot)[17] <- "Team1_ID"
  
  ot <- merge(ot, country_ids, by.x = "Team2", by.y = "country", all.x = TRUE, sort = FALSE)
  colnames(ot)[18] <- "Team2_ID"
  
  ## Add column for date of FIFA rankings to be used
  ot <- merge(ot, comp_rankings, by.x = "Comp", by.y = "Comp", all.x = TRUE, sort = FALSE)
  
  ## Rearange columns
  ot <- ot[, c(4,3,2,1,5:19)]
  
  ## Add in ranking and normalized points for Team1; rename columns
  ot <- merge(ot, rankings, by.x = c("Team1_ID", "FIFA.Rankings"), by.y = c("id", "rank_date"), all.x = TRUE)[,c(1:20,26)]
  colnames(ot)[20:21] <- c("Team1_rank", "Team1_pts_norm")
  
  ## Add in ranking and normalized points for Team2; rename columns
  ot <- merge(ot, rankings, by.x = c("Team2_ID", "FIFA.Rankings"), by.y = c("id", "rank_date"), all.x = TRUE)[,c(1:22,28)]
  colnames(ot)[22:23] <- c("Team2_rank", "Team2_pts_norm")
  
  ## Reorder by match date and reset row index
  ot <- ot[order(ot$Date),]
  rownames(ot) <- NULL
  
  ot_ets <- matrix(0, nrow = match_count, ncol = 2)
  
  ## Insert columns that show stats for team1's previous matches
  
  ## Add columns to be filled in
  ot$Team1_last_date <- NA
  ot$Team1_previous_matches <- 0
  ot$Team1_subs_per_game <- 0
  ot$Team1_sub_min_per_game <- 0
  ot$Team1_subs_previous <- 0
  ot$Team1_sub_mins_previous <- 0
  ot$Team1_subs_previous_3 <- 0
  ot$Team1_sub_mins_previous_3 <- 0

  for (i in 2:match_count){
    
    ## Create table of previous matches and sort by date
    columns_use1 <- c(4,8,16,17,13)
    columns_use2 <- c(4,8,18,19,13)
    matches_i_t1 <- filter(ot[1:(i-1),], Team1_ID == ot[i,3] & Comp == ot[i,7])[,columns_use1]
    matches_i_t2 <- filter(ot[1:(i-1),], Team2_ID == ot[i,3] & Comp == ot[i,7])[,columns_use2]
    colnames(matches_i_t1) <- c("Date", "Score", "Subs", "Sub_Mins", "Extra_Time")
    colnames(matches_i_t2) <- c("Date", "Score", "Subs", "Sub_Mins", "Extra_Time")
    matches_i <- rbind(matches_i_t1, matches_i_t2)
    matches_i <- matches_i[order(matches_i$Date),]
    
    ## Count number of matches played
    previous_matches_i <- nrow(matches_i)
    ot[i,25] <- previous_matches_i
    
    ## Compute subs and sub mins per game
    subs_total <- sum(matches_i$Subs)
    sub_mins_total <- sum(matches_i$Sub_Mins)
    if (previous_matches_i != 0){
      ot[i,26] <- subs_total / previous_matches_i
      ot[i,27] <- sub_mins_total / previous_matches_i
    }
    
    ## Compute subs and sub mins in previous match
    if (previous_matches_i != 0){
      ot[i,28] <- matches_i[previous_matches_i,3]
      ot[i,29] <- matches_i[previous_matches_i,4]
    }
    
    ## Compute subs and sub mins in previous 3 matches
    if (previous_matches_i >= 3){
      ot[i,30] <- sum(matches_i[((previous_matches_i-2):previous_matches_i),3])
      ot[i,31] <- sum(matches_i[((previous_matches_i-2):previous_matches_i),4])
    }
    
    ## Determine last match played
    matches_dates_i <- matches_i[,1]
    ## Case 1: not team's first match of the competition
    if (previous_matches_i != 0){
      ot[i,24] <- matches_dates_i[previous_matches_i]
    }
    
    ## Determine if last match went to extra time
    matches_et_i <- matches_i[,5]
    ## Case 1: not team's first match of the competition
    if (previous_matches_i != 0){
      ot_ets[i,1] <- matches_et_i[previous_matches_i]
    }
  }
  ot$Team1_last_date <- as_date(ot$Team1_last_date)
  
  ## Insert columns that show stats for team2's previous matches
  
  ## Add columns to be filled in
  ot$Team2_last_date <- NA
  ot$Team2_previous_matches <- 0
  ot$Team2_subs_per_game <- 0
  ot$Team2_sub_min_per_game <- 0
  ot$Team2_subs_previous <- 0
  ot$Team2_sub_mins_previous <- 0
  ot$Team2_subs_previous_3 <- 0
  ot$Team2_sub_mins_previous_3 <- 0
  
  for (i in 2:match_count){
    
    ## Create table of previous matches and sort by date
    columns_use1 <- c(4,8,16,17,13)
    columns_use2 <- c(4,8,18,19,13)
    matches_i_t1 <- filter(ot[1:(i-1),], Team1_ID == ot[i,1] & Comp == ot[i,7])[,columns_use1]
    matches_i_t2 <- filter(ot[1:(i-1),], Team2_ID == ot[i,1] & Comp == ot[i,7])[,columns_use2]
    colnames(matches_i_t1) <- c("Date", "Score", "Subs", "Sub_Mins", "Extra_Time")
    colnames(matches_i_t2) <- c("Date", "Score", "Subs", "Sub_Mins", "Extra_Time")
    matches_i <- rbind(matches_i_t1, matches_i_t2)
    matches_i <- matches_i[order(matches_i$Date),]
    
    ## Count number of matches played
    previous_matches_i <- nrow(matches_i)
    ot[i,33] <- previous_matches_i
    
    ## Compute subs and sub mins per game
    subs_total <- sum(matches_i$Subs)
    sub_mins_total <- sum(matches_i$Sub_Mins)
    if (previous_matches_i != 0){
      ot[i,34] <- subs_total / previous_matches_i
      ot[i,35] <- sub_mins_total / previous_matches_i
    }
    
    ## Compute subs and sub mins in previous match
    if (previous_matches_i != 0){
      ot[i,36] <- matches_i[previous_matches_i,3]
      ot[i,37] <- matches_i[previous_matches_i,4]
    }
    
    ## Compute subs and sub mins in previous 3 matches
    if (previous_matches_i >= 3){
      ot[i,38] <- sum(matches_i[((previous_matches_i-2):previous_matches_i),3])
      ot[i,39] <- sum(matches_i[((previous_matches_i-2):previous_matches_i),4])
    }
    
    ## Determine last match played
    matches_dates_i <- matches_i[,1]
    ## Case 1: not team's first match of the competition
    if (previous_matches_i != 0){
      ot[i,32] <- matches_dates_i[previous_matches_i]
    }
    
    ## Determine if last match went to extra time
    matches_et_i <- matches_i[,5]
    ## Case 1: not team's first match of the competition
    if (previous_matches_i != 0){
      ot_ets[i,2] <- matches_et_i[previous_matches_i]
    }
  }
  ot$Team2_last_date <- as_date(ot$Team2_last_date)
  
  ## Clean up table by removing unnecessary columns
  columns_remove <- c(1,2,3,9,10)
  ot <- ot[,-columns_remove]
  
  ## Add in columns based on winner and loser
  ot$winner_last_goal <- 0
  ot$loser_last_goal <- 0
  
  ot$winner_subs <- 0
  ot$loser_subs <- 0
  
  ot$winner_sub_mins <- 0
  ot$loser_sub_mins <- 0
  
  ot$winner_rank <- 0
  ot$loser_rank <- 0
  
  ot$winner_pts_norm <- 0
  ot$loser_pts_norm <- 0
  
  ot$winner_last_date <- 0
  ot$loser_last_date <- 0
  
  ot$winner_subs_per_game <- 0
  ot$loser_subs_per_game <- 0
  
  ot$winner_sub_min_per_game <- 0
  ot$loser_sub_min_per_game <- 0
  
  ot$winner_subs_previous <- 0
  ot$loser_subs_previous <- 0
  
  ot$winner_sub_mins_previous <- 0
  ot$loser_sub_mins_previous <- 0
  
  ot$winner_subs_previous_3 <- 0
  ot$loser_subs_previous_3 <- 0
  
  ot$winner_sub_mins_previous_3 <- 0
  ot$loser_sub_mins_previous_3 <- 0
  
  ot$winner_previous_et <- 0
  ot$loser_previous_et <- 0
  
  for (i in 1:match_count){
  
    ## Case 1: Team 1 wins
    if(ot[i,9] == 1){
      
      ## Check if Team 1 scored last
      if(ot[i,10] == 1){
        ot[i,35] <- 1
        ot[i,36] <- 0
      }
      else if (ot[i,10] == 2){
        ot[i,35] <- 0
        ot[i,36] <- 1
      }
      
      ## subs
      ot[i,37] <- ot[i,11]
      ot[i,38] <- ot[i,13]
      
      ## sub_mins
      ot[i,39] <- ot[i,12]
      ot[i,40] <- ot[i,14]
      
      ## rank
      ot[i,41] <- ot[i,15]
      ot[i,42] <- ot[i,17]
      
      ## pts_norm
      ot[i,43] <- ot[i,16]
      ot[i,44] <- ot[i,18]
      
      ## last date
      ot[i,45] <- ot[i,19]
      ot[i,46] <- ot[i,27]
      
      ## subs per game
      ot[i,47] <- ot[i,21]
      ot[i,48] <- ot[i,29]
      
      ## sub mins per game
      ot[i,49] <- ot[i,22]
      ot[i,50] <- ot[i,30]
      
      ## subs previous
      ot[i,51] <- ot[i,23]
      ot[i,52] <- ot[i,31]
      
      ## sub mins previous
      ot[i,53] <- ot[i,24]
      ot[i,54] <- ot[i,32]
      
      ## subs previous 3
      ot[i,55] <- ot[i,25]
      ot[i,56] <- ot[i,33]
      
      ## sub mins previous 3
      ot[i,57] <- ot[i,26]
      ot[i,58] <- ot[i,34]
      
      ## previous extra time
      ot[i,59] <- ot_ets[i,1]
      ot[i,60] <- ot_ets[i,2]
    }
    
    ## Case 2: Team 2 wins
    else if (ot[i,9] == 2) {
      
      ## Check if Team 2 scored last
      if(ot[i,10] == 2){
        ot[i,35] <- 1
        ot[i,36] <- 0
      }
      else if (ot[i,10] == 1){
        ot[i,35] <- 0
        ot[i,36] <- 1
      }
      
      ## subs
      ot[i,37] <- ot[i,13]
      ot[i,38] <- ot[i,11]
      
      ## sub_mins
      ot[i,39] <- ot[i,14]
      ot[i,40] <- ot[i,12]
      
      ## rank
      ot[i,41] <- ot[i,17]
      ot[i,42] <- ot[i,15]
      
      ## pts_norm
      ot[i,43] <- ot[i,18]
      ot[i,44] <- ot[i,16]
      
      ## last date
      ot[i,45] <- ot[i,27]
      ot[i,46] <- ot[i,19]
      
      ## subs per game
      ot[i,47] <- ot[i,29]
      ot[i,48] <- ot[i,21]
      
      ## sub mins per game
      ot[i,49] <- ot[i,30]
      ot[i,50] <- ot[i,22]
      
      ## subs previous
      ot[i,51] <- ot[i,31]
      ot[i,52] <- ot[i,23]
      
      ## sub mins previous
      ot[i,53] <- ot[i,32]
      ot[i,54] <- ot[i,24]
      
      ## subs previous 3
      ot[i,55] <- ot[i,33]
      ot[i,56] <- ot[i,25]
      
      ## sub mins previous 3
      ot[i,57] <- ot[i,34]
      ot[i,58] <- ot[i,26]
      
      ## previous extra time
      ot[i,59] <- ot_ets[i,2]
      ot[i,60] <- ot_ets[i,1]
    }
  
  }
  
  time_end <- Sys.time()
  print((time_end - time_start))
  return(ot)
}