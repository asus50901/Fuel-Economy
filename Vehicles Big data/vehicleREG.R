library(tidyverse); library(readr); library(gridExtra);
library(stringr); library(lubridate); # to convert time variable to useful info
library(glmnet); library(MASS); library(Hmisc) # packages for Ridge REG
library(lattice); library(Matrix) # packages for Lasso REG

# Additional packages will have to be loaded if other types of REG are to be performed

vehicle <- read_csv("vehicles.csv")
#83 variables with 41013 obs.


################### Tidying Data for the game #######################

  # Recall the results of Table of variables_REAL.csv 
    # the vars. stated will be retained, otherwise deleted.

  ## The variable "make" needs to be grouped before the actual tidying.

## First drop the obs that are electricity consuming cars.

electric.cars <- vehicle[vehicle$cityE != 0, ]

elec1 <- vehicle[vehicle$cityCD != 0,]

elec2 <- vehicle[vehicle$cityUF != 0,]

vehicle1 <- vehicle[vehicle$cityE == 0, ]
vehicle1 <- vehicle1[vehicle1$cityCD == 0, ]
vehicle1 <- vehicle1[vehicle1$cityUF == 0, ]
  # The resulting dataset is "vehicle1", 40685 obs and 83 variables.
    # variables will then be deleted based on "table of variable_REAL.csv".

######################## Deleting Unused Variables #########################

vehicle1 <- vehicle1 %>%
  dplyr::select(barrels08, barrelsA08, city08U, cityA08U, co2, co2A, comb08U, combA08U, 
                cylinders, displ, drive, fuelCost08, fuelCostA08, fuelType1, highway08U, 
                highwayA08U, hlv, hpv, lv2, lv4, make, pv2, pv4, trany, UCity, UCityA, 
                UHighway, UHighwayA, VClass, year, fuelType2, createdOn, modifiedOn, youSaveSpend)
  # The resulting dataset "vehicle1" is 40685 obs and 34 variables.

###################### Transform "make" into proper groupings #########################

  ## fuelType2 plugging the NAs as "None"
vehicle1$fuelType2[is.na(vehicle1$fuelType2)] <- "None"

  #fuelType2 is discovered to be 100% NAs and thus, resulting in 100% None.
    #fuelType2 SHOULD be discarded at once.

vehicle1 <- vehicle1 %>%
  dplyr::select(-c(fuelType2)) # "vehicle1" dataset is now 33 variables.

  # Pulling the tidied dataset for make and countries.
brand <- read_csv("variablewhat.csv")

vehicle1 <- vehicle1 %>%
  dplyr::left_join(brand)

vehicle1 %>% dplyr::select(make, Country) %>% distinct() # checking if pairings are correct

# discarding variable "make"
vehicle1 <- vehicle1 %>% dplyr::select(-c(make))

# cancel the use of variables "co2", "co2A"
vehicle1 <- vehicle1 %>% dplyr::select(-c(co2, co2A))

############################# Imputing Missing Datas ####################

vehicle_aft <- vehicle1[complete.cases(vehicle1), ]

a <- str_split(vehicle_aft$createdOn, pattern = " ")

apply(a, MARGIN = 1, FUN = "[", 2)
res <- lapply(a, '[', c(2, 3, 6)) %>% bind_cols()
res

library(tidyverse)

bind_cols(res)

a <- remake(a)








############################# EDA ##############################

for (i in 1:ncol(vehicle_aft)) {
  ind <- mean(vehicle_aft[, i] == -1)
  print(paste(colnames(vehicle_aft)[i], ":", ind))
}
# check if any unnoticed missing datas exists.

  ## splitting the data variables by con, cat
cat.vehicle <- vehicle_aft %>% 
  dplyr::select(c(drive, fuelType1, Country, trany, VClass, year, createdOn, modifiedOn))

con.vehicle <- vehicle_aft %>%
  dplyr::select(-c(drive, fuelType1, Country, trany, VClass, year, createdOn, modifiedOn))

## Functions for Drawing ~

histPlot <- function(data, col) {
  dt <- data.frame(x = data[[col]])
  p <- ggplot(dt, aes(x = factor(x)))+
    geom_bar()+
    xlab(colnames(data)[col])+
    theme_light()+
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  return(p)
}

denPlot <- function(data, col){
  dt <- data.frame(x=data[[col]], youSaveSpend = data$youSaveSpend)
  p <- ggplot(data= dt) + 
    geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
    xlab((colnames(data)[col])) + 
    theme_light() 
  return(p)
}

doPlots <- function(data, fun, ii, ncol=3) 
{
  pp <- list()
  for (i in ii) {
    p <- fun(data = data, col=i)
    pp <- c(pp, list(p))
  }
  do.call("grid.arrange", c(pp, ncol=ncol))
}

## Actually start drawing 

  ## Histogram for Categorical Variables
doPlots(cat.vehicle, fun = histPlot, ii = 1:4, ncol = 2)

doPlots(cat.vehicle, fun = histPlot, ii = 5:6, ncol = 1) 
  #createdOn and modifiedOn are time related, the drawing is messy and not meaningful.

doPlots(cat.vehicle, fun = histPlot, ii = 7, ncol = 1)
doPlots(cat.vehicle, fun = histPlot, ii = 8, ncol = 1)
  # the results of drawing is boring.

  ## Density Plot for Continuous Variables
doPlots(con.vehicle, fun = denPlot, ii = 1:4, ncol = 2) 
  #barrelsA08, city08U, cityA08U have outliers (delete and redraw!)

doPlots(con.vehicle, fun = denPlot, ii = 5:8, ncol = 2)
  # comb08U and combA08U have outliers, delete and redraw!
  # cylinders [ii = 7] is discrete and go for histogram

doPlots(con.vehicle, fun = denPlot, ii = 9:12, ncol = 2)
  # fuelCostA08, highway08U, highwayA08U have outliers, delete and redraw!

doPlots(con.vehicle, fun = denPlot, ii = 12:15, ncol = 2)
  # All have outliers delete outliers and redraw !

doPlots(con.vehicle, fun = denPlot, ii = 16:19, ncol = 2)
  # lv4, pv2, pv4 have outliers delete outliers and redraw !

doPlots(con.vehicle, fun = denPlot, ii = 20:23, ncol = 2)
  # UCityA and UHighwayA have outliers, needs to delete and redraw!
