model1 = lm(Price ~ HarvestRain + WinterRain, data = wine)
summary(model1)

cor(wine$HarvestRain, wine$WinterRain)
