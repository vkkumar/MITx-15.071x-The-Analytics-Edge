songs = read.csv("songs.csv")
str(songs)

nrow(songs[songs$year == 2010, ])
nrow(songs[songs$artistname == "Michael Jackson", ])
songs[songs$artistname == "Michael Jackson" & songs$Top10 == 1, ]$songtitle
table(songs$timesignature)

songs[songs$tempo == max(songs$tempo), ]$songtitle
songs$songtitle[which.max(songs$tempo)]

table(songs$year)
SongsTrain = subset(songs , year != 2010)
SongsTest = subset(songs , year == 2010)

nonvars = c("year", "songtitle", "artistname", "songID", "artistID")

SongsTrain = SongsTrain[ , !(names(SongsTrain) %in% nonvars) ]
SongsTest = SongsTest[ , !(names(SongsTest) %in% nonvars) ]

SongsLog1 = glm(Top10 ~ ., data=SongsTrain, family=binomial)
summary(SongsLog1)

cor(SongsTrain$loudness, SongsTrain$energy)

SongsLog2 = glm(Top10 ~ . - loudness, data=SongsTrain, family=binomial)
summary(SongsLog2)

SongsLog3 = glm(Top10 ~ . - energy, data=SongsTrain, family=binomial)
summary(SongsLog3)

predSong = predict(SongsLog3, newdata = SongsTest, type = "response")
table(SongsTest$Top10, predSong >= 0.45)
(309+19)/sum(table(SongsTest$Top10, predSong >= 0.45))

19/(19+40)
309/(309+5)




