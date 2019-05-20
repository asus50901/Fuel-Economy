library(tidyverse); library(mice); library(lattice)
library(purrr); library(VIM);


vehicle <- read_csv("vehicles.csv")
  #83 variables with 41013 obs.

str(vehicle)

variable.names(vehicle)

summary(vehicle$cityE)
electric.cars <- vehicle[vehicle$cityE != 0,] #328 observations are electrical cars


table(is.na(vehicle$barrels08)) # no explicit missing data for Y variable
                          # needs to check, some missing data is due to different types of cars
ggplot(vehicle) + geom_histogram(aes(x = vehicle$barrels08))

ggplot(data = vehicle) + geom_histogram(aes(x = vehicle$barrelsA08)) + xlim(1,10)
table(vehicle$barrelsA08 != 0) #1602 obs with dual fuel system
    #maybe we can combine barrels08 and barrelsA08


# Run through the variables to decide which to retain.
  ## cancel the variables with no meanings
    ##check if any variables need to be combined together EX barrels08 & barrelsA08


#################### missing data ##########################
summary(vehicle) #checking for missing data


missing.variables <- map_df(vehicle, function(x) sum(is.na(x)))


#drawing missing data pattern by 20 rows a time
md.pattern(vehicle[,c(1:20)]) ## clear of MDs

summary(vehicle[,c(21:40)])
md.pattern(vehicle[,c(21:40)]) # col 23, 24, 25, 27 have missings
md.pattern(vehicle[,c(23:25,27)])

summary(vehicle[,c(41:60)])
md.pattern(vehicle[,c(41:60)]) # col58 have missings

summary(vehicle[, c(61:70)])
md.pattern(vehicle[,c(61:70)], rotate.names = T) # col 66, 67, 68, 69, 70 missings

summary(vehicle[, c(71:83)])
md.pattern(vehicle[,c(71:83)], rotate.names = T) # col 71:75, 77, 80

missing1 <- vehicle[,c(23:25, 27, 58, 66:70, 71:75, 77, 80)]

# drawing aggr plot to check if any md rate > 80% and can be dropped
aggr(missing1, numbers = T, prop = T, sortVars = T,
     labels = names(missing1), cex.axis = 0.7, gap = 3,
     ylabs = c("Histogram of missing data", "Patterns"))
# fuelType2, rangeA evMotor c240Dscr, c240bDscr, startstop, sCharger, guzzler,
  #atvType, tCharger is over 80% and needs to be deleted

#trans_dscr, eng_dscr, drive, cylinders, displ, trany needs to be imputed
sum(is.na(vehicle$trans_dscr)) # trans_dscr 25966 mds

sum(is.na(vehicle$eng_dscr)) #eng_dscr 16105 mds

sum(is.na(vehicle$drive)) #drive 1189 mds

sum(is.na(vehicle$cylinders)) #cylinders 199 mds

sum(is.na(vehicle$displ)) #displ 197 mds

sum(is.na(vehicle$trany)) # trany 11 mds

################# output the list of variables #################
var <- variable.names(vehicle)
var<-as.data.frame(var)
write.csv(var, file = "variableList.csv")

################### checking variables more thoroughly ##############

  # check how many groups in drive variable
table(vehicle$drive)
# 7 types and needs tto be adjusted

  #end_dscr variable
str(vehicle$eng_dscr) # eng_dscr needs further inspection

  # feScore (-1 means not available)
table(vehicle$feScore == -1) # 32027 obs with 'not available'
prop.table(table(vehicle$feScore == -1)) #78% of variables are 'not available'
# we can probably delete the 'feScore' variable

  # ghgScore (-1 means not available)
table(vehicle$ghgScore == -1) # 32027 obs with 'not available'
prop.table(table(vehicle$ghgScore == -1)) #78% of variables are 'not available'
# we can probably delete the 'ghgScore' variable

  # ghgScoreA (-1 means not available)
table(vehicle$ghgScoreA == -1) # 40373 obs with 'not available'
prop.table(table(vehicle$ghgScoreA == -1)) #98% of variables are 'not available'
# we can delete the 'ghgAScore' variable

  #make 
factor(vehicle$make) #136 manufactuers (levels)

  #model
factor(vehicle$model) #4040 models (levels)

  #mpdData
table(vehicle$mpgData) #check what kind of data this is.

  #range 
table(vehicle$range == 0) 
prop.table(table(vehicle$range == 0)) #99.5% mds
# we can delete range variable

  #rangeCity
table(vehicle$rangeCity == 0) 
prop.table(table(vehicle$rangeCity == 0)) #99.5% mds
# we can delete rangeCity variable

  #rangeCityA
table(vehicle$rangeCityA == 0) 
prop.table(table(vehicle$rangeCityA == 0)) #99.6% mds
# we can delete rangeCityA variable

  #rangeHwy 
table(vehicle$rangeHwy == 0) 
prop.table(table(vehicle$rangeHwy == 0)) #99.5% mds
# we can delete rangeHwy variable

#rangeHwyA
table(vehicle$rangeHwyA == 0) 
prop.table(table(vehicle$rangeHwyA == 0)) #99.6% mds
# we can delete rangeHwyA variable

  #trany
factor(vehicle$trany) # 37 different transmission systems

  #Vclass
factor(vehicle$VClass) #34 levels

  #year
ggplot(vehicle) + geom_histogram(aes(x = year))
vehicle[vehicle$year > 2019, ] #109 obs with year > 2019 ??

  #trans_dscr
factor(vehicle$trans_dscr)

######################### visualizing only for the dual fuel type cars ##############3

  #barrelsA08
ggplot(vehicle) + geom_histogram(aes(x = barrelsA08)) + xlim(1, 10)

  #cityA08
ggplot(vehicle) + geom_histogram(aes(x = cityA08)) + xlim(5, 40)

  #highwayA08
ggplot(vehicle) + geom_histogram(aes(x = highwayA08)) + xlim(5, 40)

  #co2A
ggplot(vehicle) + geom_histogram(aes(x = co2A)) + xlim(300,600)

  #combA08
ggplot(vehicle) + geom_histogram(aes(x = combA08)) + xlim(1, 30)

  #fuelCostA08
ggplot(vehicle) + geom_histogram(aes(x = fuelCostA08)) + xlim(250, 4500)

  #highwayA08
ggplot(vehicle) + geom_histogram(aes(x = highwayA08)) + xlim(5, 50)

  #UCityA
ggplot(vehicle) + geom_histogram(aes(x = UCityA)) + xlim(5, 40)

  #UHighwayA
ggplot(vehicle) + geom_histogram(aes(x = UHighwayA)) + xlim(10, 50)

