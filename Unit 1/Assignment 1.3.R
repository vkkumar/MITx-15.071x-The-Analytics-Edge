poll <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 1/AnonymityPoll.csv")

str(poll)
summary(poll)

# Breakdown of the number of people with smartphones using the table() and summary() commands on the Smartphone variable
# How many interviewees responded that they use a smartphone?
table(Poll$Smartphone)
nrow(Poll) - 487
nrow(Poll) - 487 - 472

table(poll$Sex, poll$Region)
str(poll)

# Which of the following are states in the Midwest census region?
Midwest <- subset(poll, Region == 'Midwest')
sort(table(Midwest$State))

South <- subset(poll, Region == 'South')
which.max(table(South$State))

table(poll$Internet.Use, poll$Smartphone)

table(poll$Internet.Use)
table(is.na(poll$Internet.Use))
table(is.na(poll$Smartphone))

limited <- subset(poll, Internet.Use == 1 | Smartphone == 1)
summary(limited)

table(limited$Info.On.Internet)

table(limited$Worry.About.Info, limited$Internet.Use)/
  sum(limited$Worry.About.Info == 1 | limited$Worry.About.Info == 0, na.rm = TRUE)

sum(limited$Anonymity.Possible == 1, na.rm = TRUE)/
  sum(limited$Anonymity.Possible == 1 | limited$Anonymity.Possible == 0, na.rm = TRUE)

# What proportion of interviewees who answered the Tried.Masking.Identity question
# have tried masking their identity on the Internet?
sum(limited$Tried.Masking.Identity == 1, na.rm = TRUE)/
  sum(limited$Tried.Masking.Identity == 1 | limited$Tried.Masking.Identity == 0, na.rm = TRUE)

table(limited$Tried.Masking.Identity)
128/(656+128)

str(limited)
table(limited$Privacy.Laws.Effective)
186/(541+186)

hist(limited$Age, breaks = 50)
plot(limited$Age, jitter(limited$Info.On.Internet))
abline(v = 60)
max(table(limited$Age, limited$Info.On.Internet))
which.max(table(limited$Age, limited$Info.On.Internet))

tapply(limited$Info.On.Internet, limited$Smartphone, mean, na.rm = T)
tapply(limited$Tried.Masking.Identity, limited$Smartphone, summary, na.rm = T)

