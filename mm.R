
df <- read.csv("~/Downloads/DKSalaries (3).csv")
#setup data
mm <- cbind(model.matrix(as.formula("AvgPointsPerGame~Position"), df)[,2:5], ifelse(df$Position == "C", 1, 0), df$Salary, df$AvgPointsPerGame)
colnames(mm) <- c("pf", "pg", "sf", "sg", "c", "salary", 'fp')

#setup solver
mm <- t(mm)
obj <- df$AvgPointsPerGame
dir <- c('=', '=', '=', '=', '=', '<=', '<=')

x <- 20000
vals <- c()
ptm <- proc.time()
for(i in 1:1000){
  rhs <- c(2, 2, 2, 2, 1, 60000, x)
  lp <- lp(direction = 'max',
           objective.in = obj,
           all.bin = T,
           const.rhs = rhs,
           const.dir = dir,
           const.mat = mm)
  vals <- c(vals, lp$objval)
  x <- lp$objval - 0.00001
}
proc.time() - ptm
lp$objval
