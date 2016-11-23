# build time lag model
dat <- dat %>% arrange(player_id, gameNum)
# 
# check <- dat[c("player_id", "time_played_total", "timePlayPrevious", "gameNum")]
# 

#minutes played
dat$timePlayPrevious <- NA
for (i in 2:NROW(dat)){
  if (dat$player_id[i] == dat$player_id[i - 1]){
    dat$timePlayPrevious[i] <- dat$time_played_total[i - 1]
  }
}

#Point
dat$pointsPrevious <- NA
for (i in 2:NROW(dat)){
  if (dat$player_id[i] == dat$player_id[i - 1]){
    dat$pointsPrevious[i] <- dat$points[i - 1]
  }
}

#three_pointers_made
dat$threePointersPrevious <- NA
for (i in 2:NROW(dat)){
  if (dat$player_id[i] == dat$player_id[i - 1]){
    dat$threePointersPrevious[i] <- dat$three_pointers_made[i - 1]
  }
}

#rebounds_total
dat$reboundsPrevious <- NA
for (i in 2:NROW(dat)){
  if (dat$player_id[i] == dat$player_id[i - 1]){
    dat$reboundsPrevious[i] <- dat$rebounds_total[i - 1]
  }
}

#assists
dat$assistsPrevious <- NA
for (i in 2:NROW(dat)){
  if (dat$player_id[i] == dat$player_id[i - 1]){
    dat$assistsPrevious[i] <- dat$assists[i - 1]
  }
}


#steals
dat$stealsPrevious <- NA
for (i in 2:NROW(dat)){
  if (dat$player_id[i] == dat$player_id[i - 1]){
    dat$stealsPrevious[i] <- dat$steals[i - 1]
  }
}


#blocks
dat$blocksPrevious <- NA
for (i in 2:NROW(dat)){
  if (dat$player_id[i] == dat$player_id[i - 1]){
    dat$blocksPrevious[i] <- dat$blocks[i - 1]
  }
}

#turnovers
dat$turnoversPrevious <- NA
for (i in 2:NROW(dat)){
  if (dat$player_id[i] == dat$player_id[i - 1]){
    dat$turnoversPrevious[i] <- dat$turnovers[i - 1]
  }
}

#double_double
dat$double_doublePrevious <- NA
for (i in 2:NROW(dat)){
  if (dat$player_id[i] == dat$player_id[i - 1]){
    dat$double_doublePrevious[i] <- dat$double_double[i - 1]
  }
}

#triple_double
dat$triple_doublePrevious <- NA
for (i in 2:NROW(dat)){
  if (dat$player_id[i] == dat$player_id[i - 1]){
    dat$triple_doublePrevious[i] <- dat$triple_double[i - 1]
  }
}

#minutesPlayed
dat$minutesPlayedPrevious <- NA
for (i in 2:NROW(dat)){
  if (dat$player_id[i] == dat$player_id[i - 1]){
    dat$minutesPlayedPrevious[i] <- dat$minutesPlayed[i - 1]
  }
}

