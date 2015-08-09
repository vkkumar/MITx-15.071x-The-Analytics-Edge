IBM <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 1/IBMStock.csv")
GE <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 1/GEStock.csv")
CocaCola <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 1/CocaColaStock.csv")
Boeing <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 1/BoeingStock.csv")
ProcterGamble <- read.csv("~/Dropbox/EdX/The Analytics Edge/Unit 1/ProcterGambleStock.csv")

IBM$Date = as.Date(IBM$Date, "%m/%d/%y")
GE$Date = as.Date(GE$Date, "%m/%d/%y")
CocaCola$Date = as.Date(CocaCola$Date, "%m/%d/%y")
ProcterGamble$Date = as.Date(ProcterGamble$Date, "%m/%d/%y")
Boeing$Date = as.Date(Boeing$Date, "%m/%d/%y")
IBM$Date[which.min(IBM$Date)]
IBM$Date[which.max(IBM$Date)]

companies <- c('IBM',
               'GE',
               'CocaCola',
               'Boeing',
               'ProcterGamble')

# Need to work on this!
for (c in companies){
  print (c)
}

mean(IBM$StockPrice)
min(GE$StockPrice)
max(CocaCola$StockPrice)
median(Boeing$StockPrice)
sd(ProcterGamble$StockPrice)

plot(CocaCola$StockPrice ~ CocaCola$Date, pch = 18)
CocaCola$Date[which.max(CocaCola$StockPrice)]
CocaCola$Date[which.min(CocaCola$StockPrice)]

plot(CocaCola$StockPrice ~ CocaCola$Date, pch = 18, col = 'red')
lines(ProcterGamble$Date, ProcterGamble$StockPrice, col = 'blue', lty=2)
abline(v=as.Date(c("2000-03-01")), lwd=2)
abline(v=as.Date(c("1983-01-01")), lwd=1)

plot(CocaCola$Date[301:432], CocaCola$StockPrice[301:432], type="l", col="red", ylim=c(0,210))
abline(v=as.Date(c("2000-03-01")), lwd=1)
lines(ProcterGamble$Date[301:432], ProcterGamble$StockPrice[301:432], col = 'blue')
lines(IBM$Date[301:432], IBM$StockPrice[301:432], col = 'green')
lines(GE$Date[301:432], GE$StockPrice[301:432], col = 'purple')
lines(Boeing$Date[301:432], Boeing$StockPrice[301:432], col = 'black')
abline(v=as.Date(c("1997-09-01")), lwd=1)
abline(v=as.Date(c("1997-11-01")), lwd=1)

months(IBM$Date)
tapply(IBM$StockPrice, months(IBM$Date), mean) - mean(IBM$StockPrice) > 0

which.max(tapply(GE$StockPrice, months(GE$Date), mean))
which.max(tapply(CocaCola$StockPrice, months(CocaCola$Date), mean))


