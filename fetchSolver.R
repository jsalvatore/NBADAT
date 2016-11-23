library(data.table)
library(dplyr)
library('lpSolve')

# Setup Constraints -------------------------------------------------------
dat1$Name
dat1$Salary
dat1$PredictedPoints <- ifelse(is.nan(dat1$PredictedPoints), 0, dat1$PredictedPoints)
dat1$PredictedPoints <- ifelse(is.na(dat1$PredictedPoints), 0, dat1$PredictedPoints)
dat1$C <- ifelse(grepl(pattern = "C", x = dat1$Position), 1, 0)
dat1$PF <- ifelse(grepl(pattern = "PF", x = dat1$Position), 1, 0)
dat1$PG <- ifelse(grepl(pattern = "PG", x = dat1$Position), 1, 0)
dat1$SG <- ifelse(grepl(pattern = "SG", x = dat1$Position), 1, 0)
dat1$SF <- ifelse(grepl(pattern = "SF", x = dat1$Position), 1, 0)
dat1$G <- ifelse(grepl(pattern = "G", x = dat1$Position), 1, 0)
dat1$F <- ifelse(grepl(pattern = "F", x = dat1$Position), 1, 0)
dat1$U <- 1
solveDat <-cbind(dat1$C, dat1$PF, dat1$PG, dat1$SG, dat1$SF, dat1$G, dat1$F, dat1$U)
save <- solveDat
# solveDat <- cbind(solveDat, solveDat)
solveDat <- cbind(solveDat, 1, dat1$Salary, dat1$PredictedPoints)
cnames <- c("c", "pf", "pg", "sg", "sf", "g", "f","u", 
            # "c", "pf", "pg", "sg", "sf", "g", "f", "u",
          "total",  "salary", "predictedPoints")

colnames(solveDat) <- cnames
solveDat <- t(solveDat)

# character vector of direction constraints
dir <- c(
# "<=", "<=", "<=", "<=", "<=", "<=", "<=","<=",
">=", ">=", ">=", ">=", ">=", ">=", ">=", ">=",
"==", "<="
)

# right hand side constraints
# max number of players at each position [1-5]
# 3 guards and 3 fowrads [6-7]
# total number selected 
rhs <- c(
2, 3, 3, 3, 3, 4, 4, 4,
# min players at each position
# 1, 1, 1, 1, 1, 1, 1, 1,
# max players and max salary
8, 50000
)




# setup the solver
# maximize or minimize the problem
max <- "max"

# binary objective variables
types <- T

# Objective values to maximize
obj <- solveDat[11, ]

# matrix
mat <- solveDat[1:10, ]


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
dat1 %>% filter(solution == 1) %>% select(Name, Position, predictedMinutes, PredictedPoints, Salary)
playersChosen <- dat1 %>% filter(solution == 1) %>% select(Name, Position, predictedMinutes, PredictedPoints, Salary)

dat1 %>% filter(solution == 1) %>% summarise(sumFP = sum(PredictedPoints))





# setup the solver
lineup <- data.frame()
value <- 10000
for (i in 1:100){

sol <- lp(objective.in = obj,
          const.mat = mat,
          const.dir = dir,
          const.rhs = rhs,
          all.bin = types,
          direction = max, 
          num.bin.solns = 2, 
          use.rw = T)
sol 
sol$solution
length(sol$solution[233:(233+231)])

length(sol$solution[1:length(solution) - 1]) / 2
valueIt <- sol$objval
if(valueIt < value){
value <- valueIt 
solution <- sol$solution
lineup <- rbind(lineup, solution)
}
}
str(lineup)
solution

hol

sol$solution
sol$constraints
sol
paste0("Based on players average PPG we expect the daily fantasy team to produce ", sol$objval, " points")
sol$solution
length(sol$solution)
save <- as.data.frame(save)
dat1$solution <- sol$solution
table(dat1$Position, dat1$solution)
dat1$solution2 <- sol$solution[233:(233+231)]

dat1 %>% filter(solution == 1) %>% select(Name, Position, predictedMinutes, PredictedPoints, Salary)

%>%  summarise(sumPoints = sum(PredictedPoints))
dat1 %>% filter(solution2 == 1) %>% select(Name, Position, predictedMinutes, PredictedPoints, Salary) 

colnames(dummy_vars) <- cnames
dummy_vars <- t(dummy_vars)