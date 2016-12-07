library("Hmisc")
library("RMySQL")
library("dplyr")
library("lpSolve")


dat <- merge(players, game_logs, by='player_id')
dat$team_id <- dat$team_id.x
dat <- merge(teams, dat, by = c("team_id"))
dat <- merge(games, dat, by = c("game_id"))
dat$gameDate <- as.Date(dat$gameDate)
dat <- dat %>% arrange(team_id, gameDate, player_id)

dat <- dat %>% select(player_id, first_name, last_name, team_id, points, 
                      three_pointers_made, rebounds_total, 
                      assists, steals, blocks, turnovers, double_double, triple_double,
                      gameDate, time_played_total, game_started
)

dat$fantasyPoints <- dat$points * 1 + 
  dat$three_pointers_made * .5 + 
  dat$rebounds_total * 1.25 + 
  dat$assists * 1.5 + 
  dat$steals * 2 + 
  dat$blocks * 2 + 
  dat$turnovers * -.5 + 
  dat$double_double * 1.5 +
  dat$triple_double * 3
describe(dat$fantasyPoints)

dat <- dat %>% group_by(player_id) %>% arrange(gameDate) %>% mutate(gameNum = 1, 
                                                                    gameNum = cumsum(gameNum)
)
dat <- dat %>% group_by(player_id) %>% mutate(lastGameNum = max(gameNum, na.rm = T))

dat$Name <- paste(dat$first_name, dat$last_name)
dat$Name <- ifelse(dat$Name == "Moe Harkless", "Maurice Harkless", dat$Name)
dat$Name <- ifelse(dat$Name == "Larry Nance", "Larry Nance Jr.", dat$Name)
dat$minutesPlayed <- dat$time_played_total / 60
dat$pointsPerMinute <- dat$fantasyPoints / dat$minutesPlayed


########################################################
# lag
########################################################
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

#fantasy points 
dat$fantasyPointsPrevious <- NA
for (i in 2:NROW(dat)){
  if (dat$player_id[i] == dat$player_id[i - 1]){
    dat$fantasyPointsPrevious[i] <- dat$fantasyPoints[i - 1]
  }
}

#fantasy points 
dat$improve <- NA
for (i in 2:NROW(dat)) {
  if (dat$player_id[i] == dat$player_id[i - 1]) {
    dat$improve[i] <-
      ifelse(dat$fantasyPoints[i] - dat$fantasyPoints[i - 1] > 0, 1,
             ifelse(dat$fantasyPoints[i] - dat$fantasyPoints[i - 1] == 0, 0, -1))
  }
}
dat$improve <- ifelse(is.na(dat$improve), 0, dat$improve)
########################################################
# build model
########################################################
dat <- dat %>% group_by(player_id) %>% arrange(gameNum) %>% mutate(trending = cumsum(improve))
# use this to refit 
fullData <- dat
# check <- dat %>% arrange(player_id, gameNum) %>% select(Name, gameNum, fantasyPoints, improve, trending)

  

names(dat)

lastGame <- dat %>% group_by(player_id) %>% filter(gameNum == lastGameNum)

dat <- dat %>% group_by(player_id) %>% filter(gameNum < lastGameNum) %>%
  mutate(
    difference = minutesPlayedPrevious - minutesPlayed,
    avgVar = mean(difference, na.rm = T),
    fpPerMinute = mean(sum(fantasyPoints) / sum(minutesPlayed)),
    differenceFP = fantasyPointsPrevious - fantasyPoints,
    avgVarFP = mean(differenceFP, na.rm = T),
    played0 = ifelse(minutesPlayed == 0, TRUE, FALSE), 
    maxFP = max(fantasyPoints, na.rm = T), 
    avgPoints = mean(points, na.rm = T), 
    maxGameNum = max(gameNum),
    trendingCurrent = max(trending[which(gameNum == maxGameNum)])
  )

check <- dat %>% arrange(player_id, gameNum) %>% select(Name, gameNum, fantasyPoints, improve, trending, avgPoints, maxGameNum, trendingCurrent)
avgStats <- dat %>% group_by(player_id) %>% 
  select(difference, avgVar, fpPerMinute, 
         differenceFP, avgVarFP, played0, maxFP, avgPoints, maxGameNum, trendingCurrent,
         player_id)
avgStats <- avgStats[which(!duplicated(avgStats$player_id)), ]
lastGame <- merge(lastGame, avgStats, by = "player_id")
allData <- rbind.data.frame(dat, lastGame)



# hold out
set.seed(123)
smp_size <- floor(0.65 * nrow(dat))
train_cases <- sample(seq_len(nrow(dat)), size = smp_size)
train <- dat[train_cases, ]
test <- dat[-train_cases, ]

library(lme4)

model <- lmer(formula = minutesPlayed ~ minutesPlayedPrevious + I(minutesPlayedPrevious^2) + 
                game_started + game_started*minutesPlayedPrevious +  avgVar  +
                played0 + gameNum
              +  (1 | player_id)
              + (1 | game_started)
              , data = train)
summary(model)


train$predictedMinutes <- predict(object = model, train, allow.new.levels = TRUE)
# dat$predictedMinutes - dat$minutesPlayed
cor(train$predictedMinutes, train$minutesPlayed, use = "complete.obs")
test$predictedMinutes <- predict(object = model, test, allow.new.levels = TRUE)
cor(test$predictedMinutes, test$minutesPlayed, use = "complete.obs")


plot(train$predictedMinutes, train$minutesPlayed)
plot(model)
names(train)
fpmodel <-  lmer(formula = fantasyPoints ~ predictedMinutes + I(predictedMinutes^2) +
                   avgVarFP + maxFP + avgPoints + game_started + improve + trending + trendingCurrent
                 +  (1 | player_id)
                 + (1 | game_started) + (1 | improve)
                 , data = train)
summary(fpmodel)
plot(fpmodel)


test$PredictedPoints <- predict(object = fpmodel, test, allow.new.levels = TRUE)
cor(test$PredictedPoints, test$fantasyPoints, use = "complete.obs")




dat$predictedMinutes <- predict(model, dat, allow.new.levels = TRUE)
dat$PredictedPoints <- predict(fpmodel, dat,  allow.new.levels = TRUE)


# dat$PredictedPoints <- dat$predictedMinutes * dat$fpPerMinute
plot(dat$PredictedPoints, dat$fantasyPoints)
cor(dat$PredictedPoints, dat$fantasyPoints, use = "complete.obs")
cor(dat$PredictedPoints, dat$fantasyPoints, use = "complete.obs") ^ 2
sqrt(mean(dat$predictedMinutes - dat$minutesPlayed, na.rm = T)^2)
sqrt(mean(dat$PredictedPoints - dat$fantasyPoints, na.rm = T)^2)
# > sqrt(mean(test$predictedMinutes - test$minutesPlayed, na.rm = T)^2)
# [1] 1.052033
# > sqrt(mean(test$PredictedPoints - test$fantasyPoints, na.rm = T)^2)
# [1] 2.397914
# new model
# > sqrt(mean(test$predictedMinutes - test$minutesPlayed, na.rm = T)^2)
# [1] 0.4513564
# > sqrt(mean(test$PredictedPoints - test$fantasyPoints, na.rm = T)^2)
# [1] 0.7266943

# lastGameData <- dat %>% group_by(player_id) %>% filter(gameNum == lastGameNum)
lastGame$predictedMinutes <- predict(model, lastGame, allow.new.levels = T)
lastGame$PredictedPoints <- predict(fpmodel, lastGame, allow.new.levels = T)

plot(lastGame$PredictedPoints, lastGame$fantasyPoints)
cor(lastGame$PredictedPoints, lastGame$fantasyPoints, use = "complete.obs")
# > cor(lastGame$PredictedPoints, lastGame$fantasyPoints, use = "complete.obs")
# [1] 0.8763927


### The predict all the data
allData$predictedMinutes <- predict(model, allData, allow.new.levels = T)
allData$PredictedPoints <- predict(fpmodel, allData, allow.new.levels = T)




#################################################################################
#################################################################################
#################################################################################
fullData <- fullData %>% group_by(player_id) %>%
  mutate(
    difference = minutesPlayedPrevious - minutesPlayed,
    avgVar = mean(difference, na.rm = T),
    fpPerMinute = mean(sum(fantasyPoints) / sum(minutesPlayed)),
    differenceFP = fantasyPointsPrevious - fantasyPoints,
    avgVarFP = mean(differenceFP, na.rm = T),
    played0 = ifelse(minutesPlayed == 0, TRUE, FALSE), 
    maxFP = max(fantasyPoints, na.rm = T), 
    avgPoints = mean(points, na.rm = T), 
    maxGameNum = max(gameNum),
    trendingCurrent = max(trending[which(gameNum == maxGameNum)])
  )



model <- lmer(formula = minutesPlayed ~ minutesPlayedPrevious + I(minutesPlayedPrevious^2) + 
                game_started + game_started*minutesPlayedPrevious +  avgVar  +
                played0 + gameNum
              +  (1 | player_id)
              + (1 | game_started) 
              , data = fullData)
summary(model)

fullData$predictedMinutes <- predict(object = model, fullData, allow.new.levels = TRUE)
# dat$predictedMinutes - dat$minutesPlayed
cor(fullData$predictedMinutes, fullData$minutesPlayed, use = "complete.obs")

plot(fullData$predictedMinutes, fullData$minutesPlayed)
plot(model)
fpmodel <-  lmer(formula = fantasyPoints ~ predictedMinutes + I(predictedMinutes^2) +
                   avgVarFP + maxFP + avgPoints + game_started + improve + trending + trendingCurrent
                 +  (1 | player_id)
                 + (1 | game_started) + (1 | improve)
                 , data = fullData)
summary(fpmodel)
plot(fpmodel)

fullData$predictedMinutes <- predict(model, fullData, allow.new.levels = TRUE)
fullData$PredictedPoints <- predict(fpmodel, fullData,  allow.new.levels = TRUE)


# dat$PredictedPoints <- dat$predictedMinutes * dat$fpPerMinute
plot(fullData$PredictedPoints, fullData$fantasyPoints)
cor(fullData$PredictedPoints, fullData$fantasyPoints, use = "complete.obs")
cor(fullData$PredictedPoints, fullData$fantasyPoints, use = "complete.obs") ^ 2
sqrt(mean(fullData$predictedMinutes - fullData$minutesPlayed, na.rm = T)^2)
sqrt(mean(fullData$PredictedPoints - fullData$fantasyPoints, na.rm = T)^2)



lastGameFinal <- fullData %>% group_by(player_id) %>% filter(gameNum == lastGameNum)





#################################################################################

#################################################################################

#################################################################################

#################################################################################













salaries <- read.csv("~/Downloads/DKSalaries (23).csv")

salaries <- read.csv("C:/Users/joe/Downloads/DKSalaries (1).csv")
# salaries <- salaries %>% filter(!(Name %in% c('Danilo Gallinari','Dion Waiters', 'Anthony Davis')))

dat1 <- merge(lastGameFinal, salaries, by = c("Name"))



# Setup Constraints -------------------------------------------------------
dat1$PredictedPoints <- ifelse(is.nan(dat1$PredictedPoints), 0, dat1$PredictedPoints)
dat1$PredictedPoints <- ifelse(is.na(dat1$PredictedPoints), 0, dat1$PredictedPoints)

dat1$C <- ifelse(grepl(pattern = "C", x = dat1$Position), 1, 0)
dat1$G <- ifelse(grepl(pattern = "G", x = dat1$Position), 1, 0)
dat1$F <- ifelse(grepl(pattern = "F", x = dat1$Position), 1, 0)
dat1$U <- 1
solveDat <-cbind(dat1$C,dat1$G, dat1$F)
save <- solveDat
solveDat <- cbind(solveDat, 1, dat1$Salary, dat1$PredictedPoints)
cnames <- c("c",  "g", "f",
"total",  "salary", "predictedPoints")

colnames(solveDat) <- cnames
solveDat <- t(solveDat)

# character vector of direction constraints
dir <- c(
  "==", ">=",">=",
  "==", "<="
)


rhs <- c(
  # min players at each position
  1, 4, 3,
  # max players and max salary
  8, 50000
)

# setup the solver
# maximize or minimize the problem
max <- "max"

# binary objective variables
types <- T

# Objective values to maximize
obj <- solveDat[6, ]

# matrix
mat <- solveDat[1:5, ]


sol <- lp(objective.in = obj,
          const.mat = mat,
          const.dir = dir,
          const.rhs = rhs,
          all.bin = types,
          direction = max, 
          num.bin.solns = 1, 
          use.rw = T)
sol 
dat1$solution <- sol$solution
table(dat1$Position, dat1$solution)
dat1 %>% filter(solution == 1) %>% mutate(differenceMinutes = minutesPlayed - predictedMinutes, 
                                          differencePoints = fantasyPoints - PredictedPoints
                                          ) %>% select(Name, 
                                          teamAbbrev,
                                          Position, 
                                          minutesPlayed, predictedMinutes, differenceMinutes, 
                                          fantasyPoints, PredictedPoints, differencePoints,
                                          Salary
                                          ) 
playersChosen <- data.frame(fullData) %>% filter(Name == 'Bradley Beal') 

dat1 %>% filter(solution == 1) %>% summarise(sumFP = sum(PredictedPoints))
dat1 %>% filter(solution == 1) %>% summarise(sumFP = sum(fantasyPointsPrevious))
salaries$Position
