climate = read.csv("climate_change.csv")
str(climate)

climateReg = lm(Temp ~ MEI + CO2 + CH4 + N2O + CFC.11 + CFC.12 + TSI + Aerosols,
                data = climate)
summary(climateReg)
