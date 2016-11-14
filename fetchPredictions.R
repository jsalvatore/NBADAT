
timePlayPrevious +
  pointsPrevious + threePointersPrevious +
  reboundsPrevious + 
  assistsPrevious + 
  stealsPrevious + 
  blocksPrevious + 
  turnoversPrevious + 
  turnoversPrevious +   

  

model <- lmer(formula = fantasyPoints ~ gameNum +   timePlayPrevious + 
                pointsPrevious + threePointersPrevious + 
                reboundsPrevious + 
                stealsPrevious + 
                assistsPrevious + 
                blocksPrevious + 
                turnoversPrevious + 
                turnoversPrevious + 
                (1 | player_id), data = game_logs
              
)


summary(model)
game_logs$predictedPoints <- predict(object = model, game_logs, allow.new.levels = TRUE)

hist(game_logs$fantasyPoints - 
       game_logs$predictedPoints)

game_logs$error <- game_logs$fantasyPoints - 
  game_logs$predictedPoints
options(scipen =999)
sqrt(mean(game_logs$error) ^ 2)

