# VIDEO 2

# Read in data
baseball = read.csv("baseball.csv")
str(baseball)

# Subset to only include moneyball years
moneyball = subset(baseball, Year < 2002)
str(moneyball)

# Compute Run Difference
moneyball$RD = moneyball$RS - moneyball$RA
str(moneyball)

# Scatterplot to check for linear relationship
plot(moneyball$RD, moneyball$W)

# Regression model to predict wins
WinsReg = lm(W ~ RD, data=moneyball)
summary(WinsReg)

80.881375 + 0.105766*99

# VIDEO 3

str(moneyball)

# Regression model to predict runs scored
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)

RunsReg = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsReg)

OBP = 0.311
SLG = 0.405
-804.63 + 2737.77*OBP + 1584.91*SLG

OOBP = 0.297
OSLG = 0.370
-837.38 + 2913.60*OOBP + 1514.29*OSLG

Player Name     OBP	  SLG	  Salary
Eric Chavez	    0.338	0.540	$1,400,000 - 976.5877 - 1433.563
Jeremy Giambi	  0.391	0.450	$1,065,000 - 979.0476 - 1087.792 ***
Frank Menechino	0.369	0.374	  $295,000 - 798.3635 - 369.5059
Greg Myers	    0.313	0.447	  $800,000 - 760.7468 - 1051.598
Carlos Pena	    0.361	0.500	  $300,000 - 976.16   - 307.3267 ***

teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94, 88, 95, 88, 93, 94, 98, 97, 93, 94)
wins2013 = c(97, 97, 92, 93, 92, 96, 94, 96, 92, 90)

cor(teamRank, wins2012)
cor(teamRank, wins2013)
