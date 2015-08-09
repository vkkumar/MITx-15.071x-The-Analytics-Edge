nrow(mvtWeek1)
str(mvtWeek1)
max(mvtWeek1$ID)
min(mvtWeek1$Beat)
table(mvtWeek1$Arrest)
table(mvtWeek1$LocationDescription == "ALLEY")

DateConvert = as.Date(strptime(mvtWeek1$Date, "%m/%d/%y %H:%M"))
median(DateConvert)

mvtWeek1$Month = months(DateConvert)
mvtWeek1$Weekday = weekdays(DateConvert)
mvtWeek1$Date = DateConvert

# In which month did the fewest motor vehicle thefts occur?
table(mvtWeek1$Month)

# On which weekday did the most motor vehicle thefts occur?
table(mvtWeek1$Weekday)
plot(table(mvtWeek1$Weekday), ylim = c(26000, 30000))

# Which month has the largest number of motor vehicle thefts for which an arrest was made?
table(mvtWeek1$Month, mvtWeek1$Arrest)[, 2]
which.max(table(mvtWeek1$Month, mvtWeek1$Arrest)[, 2])

# PROBLEM 3.1 - VISUALIZING CRIME TRENDS
hist(mvtWeek1$Date, breaks=100)

#Create a boxplot of the variable "Date", sorted by the variable "Arrest"
boxplot(mvtWeek1$Date ~ mvtWeek1$Arrest)

# PROBLEM 3.3 - VISUALIZING CRIME TRENDS
# For what proportion of motor vehicle thefts in 2001 was an arrest made?
sum(mvtWeek1$Year==2001 & mvtWeek1$Arrest=="TRUE")/sum(mvtWeek1$Year==2001)
# For what proportion of motor vehicle thefts in 2007 was an arrest made?
sum(mvtWeek1$Year==2007 & mvtWeek1$Arrest=="TRUE")/sum(mvtWeek1$Year==2007)
# For what proportion of motor vehicle thefts in 2012 was an arrest made?
sum(mvtWeek1$Year==2012 & mvtWeek1$Arrest=="TRUE")/sum(mvtWeek1$Year==2012)

# PROBLEM 4.1 - POPULAR LOCATIONS
# Which locations are the top five locations for motor vehicle thefts, excluding the "Other" category?
sort(table(mvt$LocationDescription))
