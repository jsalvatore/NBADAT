library(stattleshipR)

## Get your free token from www.stattleship.com
set_token("9ffe91d47c6b7c159965a8340a4d9106")



sport <- 'basketball'
league <- 'nba'
ep <- 'game_logs'
q_body <- list(team_id='nba-bos', status='ended', interval_type='regularseason')
gls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)  
game_logs<-do.call('rbind', lapply(gls, function(x) x$game_logs))  


pls <- ss_get_result(sport=sport, league=league, ep=ep, query=q_body, walk=TRUE)  
players<-do.call('rbind', lapply(pls, function(x) x$players))

colnames(players)[1] <- 'player_id'
game_logs <- merge(players, game_logs, by='player_id')