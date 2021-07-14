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
  
  ## Determine who scored last in regulation
  pso_check <- str_locate_all(match, pattern = "\n\nPenalty shootout\n\n")
  ## Case 1: Match does go to a penalty shoot-out (PSO)
  if (length(pso_check[[1]]) != 0){
    goal_list_start_n <- 1
    goal_list_end_n <- pso_check[[1]][1,1]
    goal_list <- substr(match, goal_list_start_n, goal_list_end_n)
    
    goal_count <- str_locate_all(goal_list, pattern = " [0-9]{1,3}\\.")
    goal_minute_ends <- str_locate_all(goal_list, pattern = "[0-9]\\.")
    goal_count_n <- nrow(goal_count[[1]])
    if (goal_count_n == 0){
      last_goal_team <- 0
    } else {
      last_goal_number <- 0
      for (i in 1:goal_count_n){
        goal_i_min_start_n <- goal_count[[1]][i,1] + 1
        goal_i_min_end_n <- goal_minute_ends[[1]][i,1]
        goal_i_min <- as.integer(substr(goal_list, goal_i_min_start_n, goal_i_min_end_n))
        if (goal_i_min <= 90){
          last_goal_number <- i
        }
      }
      ## Determine which team scored last in regulation
      goal_score_starts <- str_locate_all(goal_list, pattern = "[0-9]{1,2} : ")
      goal_score_seps <- str_locate_all(goal_list, pattern = " : ")
      goal_score_ends <- str_locate_all(goal_list, pattern = " [0-9]{1,2}\t")
    
      ## Determine both teams score after second to last (stl) goal
      ### Case 1: Last goal is first goal (ie. match finishes 0:1 or 1:0)
      if (goal_count_n == 1){
        team1_score_stl <- 0
        team2_score_stl <- 0
      }
      ### Case 2: Last goal is not first goal (ie. match does not finish 0:1 or 1:0)
      else {
        team1_score_stl_start_n <- goal_score_starts[[1]][(last_goal_number - 1), 1]
        team1_score_stl_end_n <- goal_score_seps[[1]][(last_goal_number - 1), 1] - 1
        team1_score_stl <- as.integer(substr(goal_list, team1_score_stl_start_n, team1_score_stl_end_n))
        
        team2_score_stl_start_n <- goal_score_seps[[1]][(last_goal_number - 1), 2] + 1
        team2_score_stl_end_n <- goal_score_ends[[1]][(last_goal_number - 1), 1] + 1
        team2_score_stl <- as.integer(substr(goal_list, team2_score_stl_start_n, team2_score_stl_end_n))
      }
      ## Determine both teams score after last goal
      team1_score_last_start_n <- goal_score_starts[[1]][last_goal_number, 1]
      team1_score_last_end_n <- goal_score_seps[[1]][last_goal_number, 1] - 1
      team1_score_last <- as.integer(substr(goal_list, team1_score_last_start_n, team1_score_last_end_n))
      
      team2_score_last_start_n <- goal_score_seps[[1]][last_goal_number, 2] + 1
      team2_score_last_end_n <- goal_score_ends[[1]][last_goal_number, 1] + 1
      team2_score_last <- as.integer(substr(goal_list, team2_score_last_start_n, team2_score_last_end_n))
      
      ## Assign which team scored last
      ### Case 1: team 1's score did not change (ie. team 2 scored)
      if (team1_score_stl == team1_score_last){
        last_goal_team <- 2
      }
      ### Case 2: team 1's score did change (ie. team 1 scored)
      else {
        last_goal_team <- 1
      }
    }
  } 
  ## Case 2: Match does not go to a penalty shoot-out (PSO)
  else {
    ## Calculate number of goals scored in match
    score_sep = str_locate_all(score, pattern = ":")
    score_sep_n <- score_sep[[1]][1,1]
    team1_goals <- as.integer(substr(score, 1, (score_sep_n - 1)))
    team2_goals <- as.integer(substr(score, (score_sep_n + 1), str_length(score)))
    goal_count_n <- team1_goals + team2_goals
    
    ## Isolate text with goals
    last_goal_team <- 0
    
    if (goal_count_n == 0){
      last_goal_team <- 0
    } else {
      goal_list_start_n <- 1
      goal_list_end <- str_locate_all(match, pattern = "\n\n")
      goal_list_end_n <- goal_list_end[[1]][goal_count_n, 1]
      goal_list <- substr(match, goal_list_start_n, goal_list_end_n)
      
      goal_count <- str_locate_all(goal_list, pattern = " [0-9]{1,3}\\.")
      goal_minute_ends <- str_locate_all(goal_list, pattern = "[0-9]\\.")
      goal_count_n <- nrow(goal_count[[1]])
      last_goal_number <- 0
      for (i in 1:goal_count_n){
        goal_i_min_start_n <- goal_count[[1]][i,1] + 1
        goal_i_min_end_n <- goal_minute_ends[[1]][i,1]
        goal_i_min <- as.integer(substr(goal_list, goal_i_min_start_n, goal_i_min_end_n))
        if (goal_i_min <= 90){
          last_goal_number <- i
        }
      }
      
      if (last_goal_number > 0){
        ## Determine which team scored last in regulation
        goal_score_starts <- str_locate_all(goal_list, pattern = "[0-9]{1,2} : ")
        goal_score_seps <- str_locate_all(goal_list, pattern = " : ")
        goal_score_ends <- str_locate_all(goal_list, pattern = " [0-9]{1,2}\t")
        
        ## Determine both teams score after second to last (stl) goal
        ### Case 1: Last goal is first goal (ie. match finishes 0:1 or 1:0)
        if (goal_count_n == 1){
          team1_score_stl <- 0
          team2_score_stl <- 0
        }
        ### Case 2: Last goal is not first goal (ie. match does not finish 0:1 or 1:0)
        else {
          team1_score_stl_start_n <- goal_score_starts[[1]][(last_goal_number - 1), 1]
          team1_score_stl_end_n <- goal_score_seps[[1]][(last_goal_number - 1), 1] - 1
          team1_score_stl <- as.integer(substr(goal_list, team1_score_stl_start_n, team1_score_stl_end_n))
          
          team2_score_stl_start_n <- goal_score_seps[[1]][(last_goal_number - 1), 2] + 1
          team2_score_stl_end_n <- goal_score_ends[[1]][(last_goal_number - 1), 1] + 1
          team2_score_stl <- as.integer(substr(goal_list, team2_score_stl_start_n, team2_score_stl_end_n))
        }
        ## Determine both teams score after last goal
        team1_score_last_start_n <- goal_score_starts[[1]][last_goal_number, 1]
        team1_score_last_end_n <- goal_score_seps[[1]][last_goal_number, 1] - 1
        team1_score_last <- as.integer(substr(goal_list, team1_score_last_start_n, team1_score_last_end_n))
        
        team2_score_last_start_n <- goal_score_seps[[1]][last_goal_number, 2] + 1
        team2_score_last_end_n <- goal_score_ends[[1]][last_goal_number, 1] + 1
        team2_score_last <- as.integer(substr(goal_list, team2_score_last_start_n, team2_score_last_end_n))
        
        ## Assign which team scored last
        ### Case 1: team 1's score did not change (ie. team 2 scored)
        if (team1_score_stl == team1_score_last){
          last_goal_team <- 2
        }
        ### Case 2: team 1's score did change (ie. team 1 scored)
        else {
          last_goal_team <- 1
        }
      }
    }
  }
  
  ## Determine how many subs each team used in regulation
  
  ## Isolate text with substitutes for both teams
  substitutes_headers <- str_locate_all(match, pattern = "\n\nSubstitutes\n\n")
  
  ## Determine if both teams have substitutes
  teams_both_subs <- nrow(substitutes_headers[[1]])
  
  ## Case 1: Neither team has subs
  if (teams_both_subs == 0){
    team1_subs_count <- 0
    team2_subs_count <- 0
    team1_subs_total_min <- 0
    team2_subs_total_min <- 0
  }
  
  ## Case 2: One team has subs
  else if (teams_both_subs == 1){
    ## Determine which team has subs
    team_subs_check <- substr(match, substitutes_headers[[1]][1,2], str_length(match))
    team_subs_check_players <- str_locate_all(team_subs_check, pattern = "\n\n[0-9]{1,2}\t")
    
    ## Case 2a: Team 1 has subs
    if (nrow(team_subs_check_players[[1]]) >= 12){
      team1_subs_text_start_n <- substitutes_headers[[1]][1,2] - 1
      team1_subs_text_alt_end_n <- str_length(match)
      team1_subs_text <- substr(match, team1_subs_text_start_n, team1_subs_text_alt_end_n)
      team1_subs_text_nns <- str_locate_all(team1_subs_text, pattern = "\n\n")
      team1_subs_available <- nrow(team1_subs_text_nns[[1]]) - 12
      team1_subs_text <- substr(team1_subs_text, 1, team1_subs_text_nns[[1]][(team1_subs_available + 1), 1] + 1)
      
      team1_subs_count <- 0
      ## Count how many sub minutes were played in regulation
      team1_subs_total_min <- 0
      for (i in 1:team1_subs_available){
        sub_i_start_n <- team1_subs_text_nns[[1]][i,2] + 1
        sub_i_end_n <- team1_subs_text_nns[[1]][(i + 1), 2]
        sub_i <- substr(team1_subs_text, sub_i_start_n, sub_i_end_n)
        sub_i_min_check <- str_locate_all(sub_i, pattern = "\t[0-9]{1,3}'\n\n")
        ### Case 1: Sub made appearance
        if (nrow(sub_i_min_check[[1]]) > 0){
          sub_i_min_start_n <- sub_i_min_check[[1]][1,1] + 1
          sub_i_min_end_n <- sub_i_min_check[[1]][1,2] - 3
          sub_i_min <- as.integer(substr(sub_i, sub_i_min_start_n, sub_i_min_end_n))
          ### Case 1: Sub made appearance in regulation
          if (sub_i_min <= 90){
            team1_subs_count <- team1_subs_count + 1
            team1_subs_total_min <- team1_subs_total_min + (90 - sub_i_min)
          }
        }
      }
      
      team2_subs_count <- 0
      team2_subs_total_min <- 0
    }
    
    ## Case 2b: Team 2 has subs
    else {
      team2_subs_text_start_n <- substitutes_headers[[1]][1,2] - 1
      team2_subs_text_end_n <- str_length(match)
      team2_subs_text <- paste(substr(match, team2_subs_text_start_n, team2_subs_text_end_n), "\n\n", sep = "")
      team2_subs_text_nns <- str_locate_all(team2_subs_text, pattern = "\n\n")
      team2_subs_available <- nrow(team2_subs_text_nns[[1]]) - 1
      
      team2_subs_count <- 0
      ## Count how many sub minutes were played in regulation
      team2_subs_total_min <- 0
      for (i in 1:team2_subs_available){
        sub_i_start_n <- team2_subs_text_nns[[1]][i,2] + 1
        sub_i_end_n <- team2_subs_text_nns[[1]][(i + 1), 2]
        sub_i <- substr(team2_subs_text, sub_i_start_n, sub_i_end_n)
        sub_i_min_check <- str_locate_all(sub_i, pattern = "\t[0-9]{1,3}'\n\n")
        ### Case 1: Sub made appearance
        if (nrow(sub_i_min_check[[1]]) > 0){
          sub_i_min_start_n <- sub_i_min_check[[1]][1,1] + 1
          sub_i_min_end_n <- sub_i_min_check[[1]][1,2] - 3
          sub_i_min <- as.integer(substr(sub_i, sub_i_min_start_n, sub_i_min_end_n))
          ### Case 1: Sub made appearance in regulation
          if (sub_i_min <= 90){
            team2_subs_count <- team2_subs_count + 1
            team2_subs_total_min <- team2_subs_total_min + (90 - sub_i_min)
          }
        }
      }
      
      team1_subs_count <- 0
      team1_subs_total_min <- 0
    }
  }
  
  ## Case 3: Both teams have subs
  else {
    team1_subs_text_start_n <- substitutes_headers[[1]][1,2] - 1
    team1_subs_text_end_n <- substitutes_headers[[1]][2,1] + 1
    team1_subs_text <- substr(match, team1_subs_text_start_n, team1_subs_text_end_n)
    team1_subs_text_nns <- str_locate_all(team1_subs_text, pattern = "\n\n")
    team1_subs_available <- nrow(team1_subs_text_nns[[1]]) - 12
    team1_subs_text <- substr(team1_subs_text, 1, team1_subs_text_nns[[1]][(team1_subs_available + 1), 1] + 1)
    
    team2_subs_text_start_n <- substitutes_headers[[1]][2,2] - 1
    team2_subs_text_end_n <- str_length(match)
    team2_subs_text <- paste(substr(match, team2_subs_text_start_n, team2_subs_text_end_n), "\n\n", sep = "")
    team2_subs_text_nns <- str_locate_all(team2_subs_text, pattern = "\n\n")
    team2_subs_available <- nrow(team2_subs_text_nns[[1]]) - 1
    
    ## Count how many subs each team used in regulation
    team1_subs_count <- 0
    ## Count how many sub minutes were played in regulation
    team1_subs_total_min <- 0
    for (i in 1:team1_subs_available){
      sub_i_start_n <- team1_subs_text_nns[[1]][i,2] + 1
      sub_i_end_n <- team1_subs_text_nns[[1]][(i + 1), 2]
      sub_i <- substr(team1_subs_text, sub_i_start_n, sub_i_end_n)
      sub_i_min_check <- str_locate_all(sub_i, pattern = "\t[0-9]{1,3}'\n\n")
      ### Case 1: Sub made appearance
      if (nrow(sub_i_min_check[[1]]) > 0){
        sub_i_min_start_n <- sub_i_min_check[[1]][1,1] + 1
        sub_i_min_end_n <- sub_i_min_check[[1]][1,2] - 3
        sub_i_min <- as.integer(substr(sub_i, sub_i_min_start_n, sub_i_min_end_n))
        ### Case 1: Sub made appearance in regulation
        if (sub_i_min <= 90){
          team1_subs_count <- team1_subs_count + 1
          team1_subs_total_min <- team1_subs_total_min + (90 - sub_i_min)
        }
      }
    }
    
    ## Count how many subs each team used in regulation
    team2_subs_count <- 0
    ## Count how many sub minutes were played in regulation
    team2_subs_total_min <- 0
    for (i in 1:team2_subs_available){
      sub_i_start_n <- team2_subs_text_nns[[1]][i,2] + 1
      sub_i_end_n <- team2_subs_text_nns[[1]][(i + 1), 2]
      sub_i <- substr(team2_subs_text, sub_i_start_n, sub_i_end_n)
      sub_i_min_check <- str_locate_all(sub_i, pattern = "\t[0-9]{1,3}'\n\n")
      ### Case 1: Sub made appearance
      if (nrow(sub_i_min_check[[1]]) > 0){
        sub_i_min_start_n <- sub_i_min_check[[1]][1,1] + 1
        sub_i_min_end_n <- sub_i_min_check[[1]][1,2] - 3
        sub_i_min <- as.integer(substr(sub_i, sub_i_min_start_n, sub_i_min_end_n))
        ### Case 1: Sub made appearance in regulation
        if (sub_i_min <= 90){
          team2_subs_count <- team2_subs_count + 1
          team2_subs_total_min <- team2_subs_total_min + (90 - sub_i_min)
        }
      }
    }
  }
  
  return(c(last_goal_team, team1_subs_count, team2_subs_count, team1_subs_total_min, team2_subs_total_min))
}