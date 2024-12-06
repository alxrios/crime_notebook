##############################################################################################
# This script is intended for accompanying the creation of the "crime_dataset.Rmd" document. #
##############################################################################################

# Necessary libraries:
library(ggplot2)
library(lubridate)

# Setting the directory to the one where de dataset is in
getwd() # checks the current directory
setwd("..") # moves backwards in the current directory
setwd(".//datasets//crime_dataset") # sets the directory to the specified path
crimeset <- read.csv("Crime_Data_from_2020_to_Present.csv") # loads the dataset

### EXPLORATORY DATA ANALYSIS

# First let's see the dimensions of the dataset
dim(crimeset)
# The dataset counts with 955339 observations and 28 variables.
# Let's check the variable names
names(crimeset)
# There are missing values in some variable?
missing_table <- as.matrix(colSums(apply(X = crimeset, MARGIN = 2, FUN = is.na)))
missing_table
# Percentages of missing values by variable:
100*12/955339
100*630320/955339
100*11/955339
100*886873/955339
100*953045/955339
100*955275/955339

for (i in 1:dim(missing_table)[1]) {
  if (missing_table[i, 1] != 0) {
    cat("Percentage of missings of variable ", names(missing_table[i, ]), ":", round(100*missing_table[i, 1]/955339, 2), "%\n")
  }
}

# Let's explore now the content of each variable

# 1) Variable: DR_NO

head(crimeset$DR_NO)
length(unique(crimeset$DR_NO))
length(unique(crimeset$DR_NO)) == dim(crimeset)[1]

# 2) Variable: Date.Rptd

head(crimeset$Date.Rptd)
# Convert a single case
as.Date(unlist(strsplit(crimeset$Date.Rptd[1], " "))[1], format = "%m/%d/%Y")
as.Date(unlist(strsplit(crimeset$Date.Rptd[1], " ")), format = "%m/%d/%Y %h:%m:%s")
as.Date(crimeset$Date.Rptd[1], format = "%m/%d/%Y %H:%M:%S %p")
test <- as.Date(crimeset$Date.Rptd[1], format = "%m/%d/%Y %H:%M:%S %p")
year(test)
hour(test)
minutes(test)
seconds(test)
test <- as.POSIXct(crimeset$Date.Rptd[1], format = "%m/%d/%Y %H:%M:%S %p")

# Let's check how many different hours are in the variable (without the date)
# Let's see how it would be for one case
paste(unlist(strsplit(crimeset$Date.Rptd[1], " "))[2:3], collapse = " ")

# Let's create an auxiliar funciton to perform the above task
getHour <- function(value) {
  # Input a character string like "03/01/2020 12:00:00 AM"
  # Return a character string like "12:00:00 AM"
  # Input is assumed to be passed correctly by the user
  return(paste(unlist(strsplit(value, " "))[2:3], collapse = " "))
}

# Testing...
getHour(crimeset$Date.Rptd[1])
getHour(crimeset$Date.Rptd[2])
getHour(crimeset$Date.Rptd[3])

# Let's store all the hours in a vector
checkHours <- vapply(crimeset$Date.Rptd, getHour, character(1))
head(checkHours)
length(unique(checkHours))

# Since all the hours are equal, let's quit them and remain only with the date.
# Also let's transform the dates from character type into a date type.
#
# How it would be for one case...
as.Date(unlist(strsplit(crimeset$Date.Rptd[1], " "))[1], format = "%m/%d/%Y")
# Let's create an auxiliar function to do the process...
getDate <- function(value) {
  # Input: a character string with the format: "03/01/2020 12:00:00 AM"
  # Return: a date type with the format: "2020-03-01"
  # Input is assumed to be correctly passed by the user
  return(as.Date(unlist(strsplit(value, " "))[1], format = "%m/%d/%Y"))
}


# Testing...
getDate(crimeset$Date.Rptd[1])
getDate(crimeset$Date.Rptd[2])
class(getDate(crimeset$Date.Rptd[2]))
test <- vapply(crimeset$Date.Rptd, getDate, double(1))
head(test)
as.Date(test[1])

# Version 2, now it returns a character instead of a Date
getDate <- function(value) {
  # Input: a character string with the format: "03/01/2020 12:00:00 AM"
  # Return: a character type with the format: "03/01/2020"
  # Input is assumed to be correctly passed by the user
  return(unlist(strsplit(value, " "))[1])
}

# Testing...
getDate(crimeset$Date.Rptd[1])
getDate(crimeset$Date.Rptd[2])
test <- vapply(crimeset$Date.Rptd, getDate, character(1))
head(test)
head(as.Date(test, format = "%m/%d/%Y"))
testFrame <- data.frame(first = 1:6)
testFrame$second <- head(as.Date(test, format = "%m/%d/%Y"))

# Now let's create a new variable withe the dates in the dataset crimeset
crimeset$rptd_date <-  vapply(crimeset$Date.Rptd, getDate, character(1))
crimeset$rptd_date <- as.Date(crimeset$rptd_date, format = "%m/%d/%Y")
class(crimeset$rptd_date)
head(crimeset$rptd_date)
head(year(crimeset$rptd_date))
range(crimeset$rptd_date)
length(unique(crimeset$rptd_date))
head(sort(table(crimeset$rptd_date), decreasing = T), 10)
# Frequencies of the years
table(year(crimeset$rptd_date))/sum(table(year(crimeset$rptd_date)))

# 3) Variable: DATE.OCC

head(crimeset$DATE.OCC)
# Let's proceed like in the previous variable, first let's check if all the observations take
# the same hour.

# Let's store all the hours in a vector
checkHours2 <- vapply(crimeset$DATE.OCC, getHour, character(1))
head(checkHours2)
length(unique(checkHours2))
# Again only one hour for all the rows, so let's save in a new variable only the dates.
crimeset$date_occ <-  vapply(crimeset$DATE.OCC, getDate, character(1))
crimeset$date_occ <- as.Date(crimeset$date_occ, format = "%m/%d/%Y")
head(crimeset$date_occ)
range(crimeset$date_occ)

head(crimeset$rptd_date)
head(crimeset$date_occ)
# The difference with the previous variable is always zero or positive?
crimeset$rptd_date[1] - crimeset$date_occ[2]
head(crimeset$rptd_date) - head(crimeset$date_occ)

checkPositive <- crimeset$rptd_date - crimeset$date_occ
head(checkPositive, 10)
length(which(checkPositive >= 0))
# Checking for how many dates are strictly bigger 
length(which(checkPositive > 0))

# 4) Variable: TIME.OCC

head(crimeset$TIME.OCC)
summary(crimeset$TIME.OCC)
head(sort(table(crimeset$TIME.OCC), decreasing = T), 10)

# Let's check first only the observations of four digits.
checkFirstTwo <- crimeset$TIME.OCC[which(crimeset$TIME.OCC >= 1000)]%/%100
checkLastTwo <- crimeset$TIME.OCC[which(crimeset$TIME.OCC >= 1000)]%%100
range(checkFirstTwo)
range(checkLastTwo)
# Let's check now the observations with one or two digits.
checkOneTwo <- crimeset$TIME.OCC[which(crimeset$TIME.OCC < 100)]
range(checkOneTwo)
# Lastly let's check the observations with three digits.
checkFirstThree <- crimeset$TIME.OCC[which(crimeset$TIME.OCC < 1000)]%/%100
checkLastThree <- crimeset$TIME.OCC[which(crimeset$TIME.OCC < 1000)]%%100
range(checkFirstThree)
range(checkLastThree)

# 5) Variable: AREA

head(crimeset$AREA)
length(unique(crimeset$AREA))
# 21 different values
range(crimeset$AREA)
sort(unique(crimeset$AREA))
# Let's transform it into factor
crimeset$AREA <- factor(crimeset$AREA)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(crimeset$AREA)/length(crimeset$AREA), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "indianred3", fill = "indianred3") +
  labs(title = "Relative frequencies of variable area", x = "area code") + theme_classic() +
  geom_text(aes(label = Freq), hjust = 1.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + coord_flip()

# 6) Variable: AREA.NAME

head(crimeset$AREA.NAME)
length(unique(crimeset$AREA.NAME))
# Auxiliar dataframe to print 
auxSet <- data.frame(AREA = numeric(21), AREA_NAME = character(21))
for (i in 1:21) {
  auxSet[i, ] <- head(crimeset[which(crimeset$AREA == i), c("AREA", "AREA.NAME")], 1)
}

# 7) Variable: Rpt.Dist.No

head(crimeset$Rpt.Dist.No)
range(crimeset$Rpt.Dist.No)
length(unique(crimeset$Rpt.Dist.No))
head(sort(table(crimeset$Rpt.Dist.No), decreasing = T), 10)

# 8) Variable: Part.1.2

head(crimeset$Part.1.2)
unique(crimeset$Part.1.2)
table(crimeset$Part.1.2)
crimeset$Part.1.2 <- factor(crimeset$Part.1.2)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(crimeset$Part.1.2)/length(crimeset$Part.1.2), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "indianred3", fill = "indianred3") +
  labs(title = "Relative frequencies of variable Part.1.2", x = "Part.1.2") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 9) Variable: Crm.Cd

head(crimeset$Crm.Cd)
range(crimeset$Crm.Cd)
typeof(crimeset$Crm.Cd)
head(sort(table(crimeset$Crm.Cd), decreasing = T), 10)

# 10) Variable: Crm.Cd.Desc

head(crimeset$Crm.Cd.Desc)
length(unique(crimeset$Crm.Cd.Desc))

# Let's create a dataframe with the crimes codes and their name.
crimeNames <- data.frame(code = numeric(139), crime = character(139))
rowIndex <- 1 # Auxiliar variable for selecting the row
for (i in sort(unique(crimeset$Crm.Cd))) {
  crimeNames[rowIndex, ] <- head(crimeset[which(crimeset$Crm.Cd == i), c("Crm.Cd", "Crm.Cd.Desc")], 1)
  rowIndex <- rowIndex + 1
}

# Result
head(crimeNames, 20)

# Let's create a new variable with the previous one as a factor and assign it the correct 
# level names.
crimeset$crm_cd <- factor(crimeset$Crm.Cd)
head(crimeset$crm_cd)
levels(crimeset$crm_cd) <- crimeNames$crime
head(crimeset$crm_cd, 7)

head(sort(table(crimeset$crm_cd), decreasing = T), 20)
names(head(sort(table(crimeset$crm_cd), decreasing = T), 20))
data.frame(crime = names(head(sort(table(crimeset$crm_cd), decreasing = T), 20)), 
           frequency = as.vector(head(sort(table(crimeset$crm_cd), decreasing = T), 20)), 
           rel_frequency = round(100*as.vector(head(sort(table(crimeset$crm_cd),
                                                         decreasing = T), 20))/955339, 2))

# 30 rows version

data.frame(crime = names(head(sort(table(crimeset$crm_cd), decreasing = T), 30)), 
           frequency = as.vector(head(sort(table(crimeset$crm_cd), decreasing = T), 30)), 
           rel_frequency = round(100*as.vector(head(sort(table(crimeset$crm_cd),
                                                         decreasing = T), 30))/955339, 2))


# 11) Variable: Mocodes

head(crimeset$Mocodes)
length(unique(crimeset$Mocodes))
head(sort(table(crimeset$Mocodes), decreasing = T), 7)

# 12) Variable: Vict.Age

head(crimeset$Vict.Age)
range(crimeset$Vict.Age)
# How many negative values are in the variable?
length(which(crimeset$Vict.Age < 0))
table(crimeset$Vict.Age[which(crimeset$Vict.Age <= 0)])
# Let's see which type of crimes are asociated with the victim's age zero.
head(sort(table(crimeset$crm_cd[which(crimeset$Vict.Age == 0)]), decreasing = T), 20)
# Let's present the above head content in a data.frame
data.frame(crime = names(head(sort(table(crimeset$crm_cd[which(crimeset$Vict.Age == 0)]), decreasing = T), 20)), 
           frequency = as.vector(head(sort(table(crimeset$crm_cd[which(crimeset$Vict.Age == 0)]), decreasing = T), 20)))


sort(unique(crimeset$Vict.Age[which(crimeset$Vict.Age < 10)]))
head(sort(table(crimeset$crm_cd[which(crimeset$Vict.Age == 2)]), decreasing = T), 20)

length(which(crimeset$Vict.Age == 2))
100*length(which(crimeset$Vict.Age == 2))/955339
data.frame(crime = names(head(sort(table(crimeset$crm_cd[which(crimeset$Vict.Age == 2)]), decreasing = T), 20)), 
           frequency = as.vector(head(sort(table(crimeset$crm_cd[which(crimeset$Vict.Age == 2)]), decreasing = T), 20)))

# Which crimes are associated with the negative ages?

data.frame(crime = names(head(sort(table(crimeset$crm_cd[which(crimeset$Vict.Age == -1)]), decreasing = T), 20)), 
           frequency = as.vector(head(sort(table(crimeset$crm_cd[which(crimeset$Vict.Age == -1)]), decreasing = T), 20)))

data.frame(crime = names(head(sort(table(crimeset$crm_cd[which(crimeset$Vict.Age == -2)]), decreasing = T), 20)), 
           frequency = as.vector(head(sort(table(crimeset$crm_cd[which(crimeset$Vict.Age == -2)]), decreasing = T), 20)))

data.frame(crime = names(head(sort(table(crimeset$crm_cd[which(crimeset$Vict.Age == -3)]), decreasing = T), 20)), 
           frequency = as.vector(head(sort(table(crimeset$crm_cd[which(crimeset$Vict.Age == -3)]), decreasing = T), 20)))

data.frame(crime = names(head(sort(table(crimeset$crm_cd[which(crimeset$Vict.Age == -4)]), decreasing = T), 20)), 
           frequency = as.vector(head(sort(table(crimeset$crm_cd[which(crimeset$Vict.Age == -4)]), decreasing = T), 20)))

data.frame(crime = names(head(sort(table(crimeset$crm_cd[which(crimeset$Vict.Age < 0)]), decreasing = T), 26)), 
           frequency = as.vector(head(sort(table(crimeset$crm_cd[which(crimeset$Vict.Age < 0)]), decreasing = T), 26)))

# Replace all negative and zero ages with NA's.
crimeset$Vict.Age <- replace(crimeset$Vict.Age, which(crimeset$Vict.Age <= 0), NA)
range(crimeset$Vict.Age, na.rm = T)
sum(is.na(crimeset$Vict.Age))

# Which ages are near 120.
crimeset$Vict.Age[which(crimeset$Vict.Age > 100)]
table(crimeset$Vict.Age[which(crimeset$Vict.Age > 100)])
table(crimeset$Vict.Age[which(crimeset$Vict.Age > 90)])

summary(crimeset$Vict.Age)
# Upper whisker
50 + 1.5*(50 - 28)
# 120 is way above the upper whisker

# Boxplot
ggplot(crimeset, aes(x = Vict.Age)) + geom_boxplot(color = "indianred3") + coord_flip() +
  labs(title = "Boxplot of variable Vict.Age", x = "value") + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# Which crime is associated with the victim's age 120?
crimeset$crm_cd[which(crimeset$Vict.Age == 120)]

# Which crimes are associated with the other outliers?

sort(table(crimeset$crm_cd[which(crimeset$Vict.Age > 83)]), decreasing = T)

# Replace the value that takes 120 by an NA.
crimeset$Vict.Age <- replace(crimeset$Vict.Age, which(crimeset$Vict.Age == 120), NA)
range(crimeset$Vict.Age, na.rm = T)

summary(crimeset$Vict.Age)
# Histogram
ggplot(crimeset, aes(x = Vict.Age)) + geom_histogram(color = "white", fill = "indianred3", binwidth = 5) +
  theme_light() + labs(title = "Histogram of variable Vict.Age", x = "victim's age") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# 13) Variable: Vict.Sex

head(crimeset$Vict.Sex)
table(crimeset$Vict.Sex)
sum(is.na(crimeset$Vict.Sex))

crimeset$Vict.Sex <- replace(crimeset$Vict.Sex, which(crimeset$Vict.Sex == ""), NA)
crimeset$Vict.Sex <- replace(crimeset$Vict.Sex, which(crimeset$Vict.Sex == "-"), NA)
crimeset$Vict.Sex <- replace(crimeset$Vict.Sex, which(crimeset$Vict.Sex == "H"), NA)
crimeset$Vict.Sex <- replace(crimeset$Vict.Sex, which(crimeset$Vict.Sex == "X"), NA)
crimeset$Vict.Sex <- factor(crimeset$Vict.Sex)
table(crimeset$Vict.Sex)
round(100*sum(is.na(crimeset$Vict.Sex))/955339, 2)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(crimeset$Vict.Sex)/length(crimeset$Vict.Sex), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "indianred3", fill = "indianred3") +
  labs(title = "Relative frequencies of variable victim's sex", x = "victim's sex") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.5) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

# 14) Variable: Premis.Cd
