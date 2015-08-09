WHO <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 1/WHO.csv")
View(WHO)

str(WHO)
mean(WHO$Over60)
which.min(WHO$Over60)
WHO$Country[183]
WHO$Country[which.max(WHO$LiteracyRate)]

tapply(WHO$ChildMortality, WHO$Region, min, na.rm = TRUE)
