# build time lag model
game_logs <- game_logs %>% arrange(team_id, gameDate, player_id)

#minutes played
game_logs$timePlayPrevious <- NA
for (i in 2:NROW(game_logs)){
  if (game_logs$player_id[i] == game_logs$player_id[i - 1]){
    game_logs$timePlayPrevious[i] <- game_logs$time_played_total[i - 1]
  }
}

#Point
game_logs$pointsPrevious <- NA
for (i in 2:NROW(game_logs)){
  if (game_logs$player_id[i] == game_logs$player_id[i - 1]){
    game_logs$pointsPrevious[i] <- game_logs$points[i - 1]
  }
}

#three_pointers_made
game_logs$threePointersPrevious <- NA
for (i in 2:NROW(game_logs)){
  if (game_logs$player_id[i] == game_logs$player_id[i - 1]){
    game_logs$threePointersPrevious[i] <- game_logs$three_pointers_made[i - 1]
  }
}

#rebounds_total
game_logs$reboundsPrevious <- NA
for (i in 2:NROW(game_logs)){
  if (game_logs$player_id[i] == game_logs$player_id[i - 1]){
    game_logs$reboundsPrevious[i] <- game_logs$rebounds_total[i - 1]
  }
}

#assists
game_logs$assistsPrevious <- NA
for (i in 2:NROW(game_logs)){
  if (game_logs$player_id[i] == game_logs$player_id[i - 1]){
    game_logs$assistsPrevious[i] <- game_logs$assists[i - 1]
  }
}


#steals
game_logs$stealsPrevious <- NA
for (i in 2:NROW(game_logs)){
  if (game_logs$player_id[i] == game_logs$player_id[i - 1]){
    game_logs$stealsPrevious[i] <- game_logs$steals[i - 1]
  }
}


#blocks
game_logs$blocksPrevious <- NA
for (i in 2:NROW(game_logs)){
  if (game_logs$player_id[i] == game_logs$player_id[i - 1]){
    game_logs$blocksPrevious[i] <- game_logs$blocks[i - 1]
  }
}

#turnovers
game_logs$turnoversPrevious <- NA
for (i in 2:NROW(game_logs)){
  if (game_logs$player_id[i] == game_logs$player_id[i - 1]){
    game_logs$turnoversPrevious[i] <- game_logs$turnovers[i - 1]
  }
}

#double_double
game_logs$double_doublePrevious <- NA
for (i in 2:NROW(game_logs)){
  if (game_logs$player_id[i] == game_logs$player_id[i - 1]){
    game_logs$double_doublePrevious[i] <- game_logs$double_double[i - 1]
  }
}

#triple_double
game_logs$triple_doublePrevious <- NA
for (i in 2:NROW(game_logs)){
  if (game_logs$player_id[i] == game_logs$player_id[i - 1]){
    game_logs$triple_doublePrevious[i] <- game_logs$triple_double[i - 1]
  }
}




