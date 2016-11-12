library(stattleshipR)
library(dplyr)
## Get your free token from www.stattleship.com
set_token("9ffe91d47c6b7c159965a8340a4d9106")



sport <- 'basketball'
league <- 'nba'
ep <- 'game_logs'
q_body <- list(status='ended', interval_type='regularseason')
gls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)  
game_logs<-do.call('rbind', lapply(gls, function(x) x$game_logs))  

players<-do.call('rbind', lapply(gls, function(x) x$players))
colnames(players)[1] <- 'player_id'
length(unique(players$player_id))
players <- players[which(!duplicated(players$player_id)), ]
players$team_id <- NULL

game_logs <- merge(players, game_logs, by='player_id')


teams<-do.call('rbind', lapply(gls, function(x) x$teams))
colnames(teams)[1] <- 'team_id'
teams$team_id
teams$team <- paste(teams$name, teams$nickname)
teams <- teams[c("team_id", "team")]
teams <- teams[which(!duplicated(teams$team_id)), ]


game_logs <- merge(teams, game_logs, by = c("team_id"))



games<-do.call('rbind', lapply(gls, function(x) x$games))
colnames(games)[1] <- 'game_id'
games$game_id
names(games)
games <- games[which(!duplicated(games$game_id)), ]
games$on
games$gameDate <- gsub(pattern = "on ", replacement = "", x = games$on)
games$gameDate <- as.Date(games$gameDate, format = "%B %d, %Y")
hist(games$gameDate, breaks = "days", format = "%m/%d/%Y")
table(games$gameDate)



game_logs <- merge(games, game_logs, by = c("game_id"))



game_logs <- game_logs %>% arrange(team_id, gameDate, player_id)


