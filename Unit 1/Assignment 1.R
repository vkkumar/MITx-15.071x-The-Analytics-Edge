mvt <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 1/mvtWeek1.csv")

nrow(mvt)
str(mvt)
mvt$ID[which.max(mvt$ID)]
mvt$Beat[which.min(mvt$Beat)]
table(mvt$Arrest)
summary(mvt$LocationDescription == "ALLEY")

DateConvert = as.Date(strptime(mvt$Date, "%m/%d/%y %H:%M"))
summary(DateConvert)

mvt$Month = months(DateConvert)
mvt$Weekday = weekdays(DateConvert)
mvt$Date = DateConvert

arrestsByMonth <- table(mvt$Month, mvt$Arrest)
totalByMonth <- arrestsByMonth[, 1] + arrestsByMonth[, 2]
which.min(totalByMonth)

arrestsByDays <- table(mvt$Weekday, mvt$Arrest)
totalByDays <- arrestsByDays[, 1] + arrestsByDays[, 2]
which.max(totalByDays)

arrests <- subset(mvt, Arrest == TRUE)
arrestsByMonth <- table(arrests$Month, arrests$Arrest)
which.max(arrestsByMonth)
arrestsByMonth[5]

hist(mvt$Date, breaks=100)

boxplot(mvt$Date ~ mvt$Arrest)
which.min(table(mvt$Month)) 

sum(mvt$Year==2001 & mvt$Arrest=="TRUE")/sum(mvt$Year==2001)

sum(mvt$Year==2007 & mvt$Arrest=="TRUE")/sum(mvt$Year==2007)

sum(mvt$Year==2012 & mvt$Arrest=="TRUE")/sum(mvt$Year==2012)

sort(table(mvt$LocationDescription))

Top5 <- subset(mvt, LocationDescription %in% c('STREET',
                                               'PARKING LOT/GARAGE(NON.RESID.)',
                                               'ALLEY',
                                               'GAS STATION',
                                               'DRIVEWAY - RESIDENTIAL'))
View(Top5)

nrow(Top5)

Top5$LocationDescription = factor(Top5$LocationDescription)
str(Top5)

# One of the locations has a much higher arrest rate than the other locations.
# Which is it? Please enter the text in exactly the same way as how it looks in 
# the answer options for Problem 4.1.

arrestLocation <- as.data.frame(table(Top5$LocationDescription, Top5$Arrest))
View(arrestLocation)

percentage <- as.data.frame(arrestLocation[ ,3][6:10]*100/(arrestLocation[ ,3][1:5]+ arrestLocation[ ,3][6:10]))
View(percentage)
arrestLocation[1:5,  1]
arrestPercentage <- data.frame(as.data.frame(arrestLocation[1:5,  1]), percentage)
colnames(arrestPercentage)[1] = 'Location'
colnames(arrestPercentage)[2] = 'Arrest Percentage'
View(arrestPercentage)

# On which day of the week do the most motor vehicle thefts at gas stations happen?

gasStation <- subset(Top5, LocationDescription == 'GAS STATION')
which.max(table(gasStation$Weekday))

# On which day of the week do the fewest motor vehicle thefts in residential
# driveways happen?

sort(table(mvt$LocationDescription))
resDriveway <- subset(Top5, LocationDescription == 'DRIVEWAY - RESIDENTIAL')
which.min(table(resDriveway$Weekday))
