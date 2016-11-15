dbCon <- dbConnect(
  MySQL(), host = Credentials$host,
  port = 3306, user = Credentials$user, password = Credentials$password, dbname = "nba")



dbGetQuery(dbCon, "SELECT * FROM gamelogs limit 10;")
