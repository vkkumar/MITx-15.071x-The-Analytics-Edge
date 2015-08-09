CPS <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 1/CPSData.csv")

str(CPS)
# How many interviewees are in the dataset?
nrow(CPS)
# What is the most common industry of employment? Please enter the name exactly how you see it.
which.max(table(CPS$Industry))

# PROBLEM 1.3 - LOADING AND SUMMARIZING THE DATASET
# Which state has the fewest interviewees?
sort(table(CPS$State))

# What proportion of interviewees are citizens of the United States?
sum(CPS$Citizenship == 'Citizen, Native' | CPS$Citizenship == 'Citizen, Naturalized')/nrow(CPS)

# For which races are there at least 250 interviewees in the CPS dataset of Hispanic ethnicity?
Hispanic <- subset(CPS, Hispanic == 1)
table(Hispanic$Race) > 250

# Which variables have at least one interviewee with a missing (NA) value? (Select all that apply.)
summary(CPS)
is.na(CPS$Married)
table(CPS$Region, is.na(CPS$Married))
table(CPS$Sex, is.na(CPS$Married))
table(CPS$Age, is.na(CPS$Married))


# How many states had all interviewees living in a non-metropolitan area (aka they have a missing MetroAreaCode value)? For this question, treat the District of Columbia as a state (even though it is not technically a state).
table(CPS$State, is.na(CPS$MetroAreaCode))
sum(table(CPS$State, is.na(CPS$MetroAreaCode))[ ,1] == 0)
# How many states had all interviewees living in a metropolitan area? Again, treat the District of Columbia as a state.
sum(table(CPS$State, is.na(CPS$MetroAreaCode))[ ,2] == 0)

# Which region of the United States has the largest proportion of interviewees living in a non-metropolitan area?
RegProp <- table(CPS$Region, is.na(CPS$MetroAreaCode))
RegProp[ ,2]/(RegProp[ ,1] + RegProp[ ,2])

# Which state has a proportion of interviewees living in a non-metropolitan area closest to 30%?
sort(tapply(is.na(CPS$MetroAreaCode), CPS$State, mean, na.rm = TRUE))

# How many observations (codes for metropolitan areas) are there in MetroAreaMap?
nrow(MetroAreaMap)
nrow(CountryMap)

CPS = merge(CPS, MetroAreaMap, by.x="MetroAreaCode", by.y="Code", all.x=TRUE)
str(CPS)
sum(is.na(CPS$MetroArea))

# Which of the following metropolitan areas has the largest number of interviewees?
sort(table(CPS$MetroArea))
which.max(table(CPS$MetroArea))

# Which metropolitan area has the highest proportion of interviewees of Hispanic ethnicity?
hispanic <- table(CPS$MetroArea, CPS$Hispanic)
hispanic[,2][1]
which.max(hispanic[ ,2]/(hispanic[ ,1] + hispanic[ ,2]))
# OR use tapply as follows
which.max(tapply(CPS$Hispanic, CPS$MetroArea, mean))

# Determine the number of metropolitan areas in the United States from which at least 20% of interviewees are Asian.
Asian <- table(CPS$MetroArea, CPS$Race == "Asian")
sum(as.numeric(Asian[ ,2]/(Asian[ ,1] + Asian[ ,2]) >= 0.20),  na.rm = TRUE)

str(CPS)
table(CPS$Education, CPS$MetroArea)
tapply(CPS$Education, CPS$MetroArea, mean, na.rm = TRUE)
tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean)
sort(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean,  na.rm = TRUE))

CPS = merge(CPS, CountryMap, by.x = "CountryOfBirthCode", by.y = "Code", all.x = TRUE)
summary(CPS)
# Determine which metropolitan area has the smallest proportion of interviewees who have received no high school diploma.
which.min(tapply(CPS$Education == "No high school diploma", CPS$MetroArea, mean, na.rm = TRUE))
str(CPS)

# How many interviewees have a missing value for the new country of birth variable?
summary(CPS)

# Among all interviewees born outside of North America, which country was the most common place of birth?
sort(table(CPS$Citizenship, CPS$Country)[3,])
sort(table(CPS$Country))

# Proportion of the interviewees from the "New York-Northern New Jersey-Long Island,
# NY-NJ-PA" metropolitan area have a country of birth that is not the United States? 
sort(table(CPS$MetroArea == "New York-Northern New Jersey-Long Island, NY-NJ-PA", CPS$Country)[2,])
tapply(CPS$Country=="United States", CPS$MetroArea=="New York-Northern New Jersey-Long Island, NY-NJ-PA", summary, rm.na=TRUE)


# Problem 4.4 - Integrating Country of Birth Data
# Which metropolitan area has the largest number (note -- not proportion) of interviewees with a country of birth in India? Hint -- remember to include na.rm=TRUE if you are using tapply() to answer this question.
tapply(CPS$MetroArea, CPS$Country=="India", summary, rm.na=TRUE)
# In Brazil?
tapply(CPS$MetroArea, CPS$Country=="Brazil", summary, rm.na=TRUE)
# In Somalia?
tapply(CPS$MetroArea, CPS$Country=="Somalia", summary, rm.na=TRUE)
