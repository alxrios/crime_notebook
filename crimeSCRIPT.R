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

head(crimeset$Premis.Cd)
range(crimeset$Premis.Cd)
sum(is.na(crimeset$Premis.Cd))
range(crimeset$Premis.Cd, na.rm = T)
length(unique(crimeset$Premis.Cd))

# 15) Variable: Premis.Desc

head(crimeset$Premis.Desc)
length(unique(crimeset$Premis.Desc))
sum(is.na(crimeset$Premis.Desc))
length(unique(crimeset$Premis.Desc)[-125])
# Let's create a dataframe with the different descriptions and their associated codes.
#
# Dataframe initialization
premisFrame <- data.frame(code = numeric(306), description = character(306))
rowIndex <- 1
for (i in unique(crimeset$Premis.Desc)[-125]) {
  premisFrame[rowIndex, ] <- head(crimeset[which(crimeset$Premis.Desc == i), c("Premis.Cd", "Premis.Desc")], 1)
  rowIndex <- rowIndex  + 1
}

# Sample result
head(premisFrame, 20)
# Most observerd ones
head(sort(table(crimeset$Premis.Desc), decreasing = T), 10)

# There are empty categories? ("")

length(which(crimeset$Premis.Desc == ""))
length(which(crimeset$Premis.Desc == " "))
length(which(premisFrame$description == ""))

length(unique(premisFrame$description))
length(unique(premisFrame$code))

# How many different codes take the description ""?
length(unique(crimeset$Premis.Cd[which(crimeset$Premis.Desc == "")]))
# Let's see what codes are:
unique(crimeset$Premis.Cd[which(crimeset$Premis.Desc == "")])
table(crimeset$Premis.Cd[which(crimeset$Premis.Desc == "")])
sum(table(crimeset$Premis.Cd[which(crimeset$Premis.Desc == "")]))
569 - 557
# Are this codes always associated with empty descriptions?
table(crimeset$Premis.Desc[which(crimeset$Premis.Cd == 256)])
# Continue checking the others...
table(crimeset$Premis.Desc[which(crimeset$Premis.Cd == 418)])
table(crimeset$Premis.Desc[which(crimeset$Premis.Cd == 972)])
table(crimeset$Premis.Desc[which(crimeset$Premis.Cd == 973)])
table(crimeset$Premis.Desc[which(crimeset$Premis.Cd == 974)])
table(crimeset$Premis.Desc[which(crimeset$Premis.Cd == 975)])
table(crimeset$Premis.Desc[which(crimeset$Premis.Cd == 976)])

for (i in unique(crimeset$Premis.Cd[which(crimeset$Premis.Desc == "")])[-4]) {
  cat("code:", i, "\n")
  print(table(crimeset$Premis.Desc[which(crimeset$Premis.Cd == i)]))
  cat("--------------------------\n")
}

# Replace all the empty descriptions with NA values.
crimeset$Premis.Desc <- replace(crimeset$Premis.Desc, which(crimeset$Premis.Desc == ""), NA)
# Checking...
sum(is.na(crimeset$Premis.Desc))

# Convert the variable into a factor
crimeset$Premis.Desc <- factor(crimeset$Premis.Desc)

# 16) Variable: Weapon.Used.Cd

head(crimeset$Weapon.Used.Cd)
sum(is.na(crimeset$Weapon.Used.Cd))
100*sum(is.na(crimeset$Weapon.Used.Cd))/dim(crimeset)[1]
# 66 % of the observation are missing values
range(crimeset$Weapon.Used.Cd, na.rm = T)
head(sort(table(crimeset$Weapon.Used.Cd), decreasing = T))

# 17) Variable: Weapon.Desc

head(crimeset$Weapon.Desc)
sum(is.na(crimeset$Weapon.Desc))
length(which(crimeset$Weapon.Desc == ""))

# Checking that the rows with NAs of the previous variable and the rows with "" of this one are
# the same.
rowsCode <- which(is.na(crimeset$Weapon.Used.Cd))
rowsDesc <- which(crimeset$Weapon.Desc == "")
length(rowsCode) == length(rowsDesc)
# Lengths are the same, ok
sum(rowsCode == rowsDesc)

# Replacing the empty strings with NAs.
crimeset$Weapon.Desc <- replace(crimeset$Weapon.Desc, which(crimeset$Weapon.Desc == ""), NA)
sum(is.na(crimeset$Weapon.Desc))
length(unique(crimeset$Weapon.Desc))

head(sort(table(crimeset$Weapon.Desc), decreasing = T), 20)
# Which values for the variable crm.cd.desc take the observations which take the category
# verbal threat for this variable?
sort(table(crimeset$Crm.Cd.Desc[which(crimeset$Weapon.Desc == "VERBAL THREAT")]), decreasing = T)
# Transform the variable into factor
crimeset$Weapon.Desc <- factor(crimeset$Weapon.Desc)

# Dataframe with the 20 most observed weapons
data.frame(weapon = names(head(sort(table(crimeset$Weapon.Desc), decreasing = T), 20)), 
           frequency = as.vector(head(sort(table(crimeset$Weapon.Desc), decreasing = T), 20)))

# Let's explore which are the most observed crimes for the weapon categories strong-arm,
# verbal threat and vehicle.
head(sort(table(crimeset$Crm.Cd.Desc[which(crimeset$Weapon.Desc == "VERBAL THREAT")]), decreasing = T), 10)

for (i in c("STRONG-ARM (HANDS, FIST, FEET OR BODILY FORCE)", "VERBAL THREAT", "VEHICLE")) {
  cat("Weapon:", i, "\n\n")
  print.data.frame(data.frame(crime = names(head(sort(table(crimeset$Crm.Cd.Desc[which(crimeset$Weapon.Desc == i)]), decreasing = T), 10)), frequency = as.vector(head(sort(table(crimeset$Crm.Cd.Desc[which(crimeset$Weapon.Desc == i)]), decreasing = T), 10))), row.names = F)
  cat("-----------------------------------------------------------------------\n")
}

# 18) Variable: Status

head(crimeset$Status)
length(unique(crimeset$Status))
table(crimeset$Status)
sum(is.na(crimeset$Status))
length(which(crimeset$Status == ""))

# Replace empty string with NA
crimeset$Status <- replace(crimeset$Status, which(crimeset$Status == ""), NA)
sum(is.na(crimeset$Status))
# Transform into factor
crimeset$Status <- factor(crimeset$Status)

# 19) Variable: Status.Desc

head(crimeset$Status.Desc)
sum(is.na(crimeset$Status.Desc))
length(which(crimeset$Status.Desc == ""))
length(unique(crimeset$Status.Desc))
# 6 Different values vs. 7 different values for the previous variable
#
# Which value for the variable Status.Desc takes the observation that had NA in the previous one?
crimeset$Status.Desc[which(is.na(crimeset$Status))]
# Answer: "UNK" which seems to mean unknown
# Let's see the other values
unique(crimeset$Status.Desc)
# Which description takes the observation that accounts for NA in the previous one?
crimeset$Status.Desc[which(is.na(crimeset$Status))]

# How many observations take the value "UNK"?
length(which(crimeset$Status.Desc == "UNK"))
# 6 observations take the value "UNK"
data.frame(status = crimeset$Status[which(crimeset$Status.Desc == "UNK")], 
           status_desc = rep("UNK", 6))

# Create a new variable as a factor
crimeset$status_desc <- replace(crimeset$Status.Desc, which(crimeset$Status.Desc == "UNK"), NA)
sum(is.na(crimeset$status_desc))
crimeset$status_desc <- factor(crimeset$status_desc)

# Relative frequencies barplot
ggplot(as.data.frame(round(table(crimeset$status_desc)/length(crimeset$status_desc), 2)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "indianred3", fill = "indianred3") +
  labs(title = "Relative frequencies of variable status", x = "status") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# Absolute frequencies barplot
ggplot(as.data.frame(table(crimeset$status_desc)),
       aes(x = Var1, y = Freq)) + geom_bar(stat = "identity", color = "indianred3", fill = "indianred3") +
  labs(title = "Relative frequencies of variable status", x = "status") + theme_classic() +
  geom_text(aes(label = Freq), vjust = -0.3) + 
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))


# 20) Variables: Crm.Cd.1 - 4

head(crimeset$Crm.Cd.1)
range(crimeset$Crm.Cd.1)
range(crimeset$Crm.Cd.1, na.rm = T)
length(unique(crimeset$Crm.Cd.1))
head(sort(table(crimeset$Crm.Cd.1), decreasing = T), 20)
sum(is.na(crimeset$Crm.Cd.1))
which(is.na(unique(crimeset$Crm.Cd.1)))

# Can codes be the same as the ones of Crm.Cd?
range(crimeset$Crm.Cd)
range(crimeset$Crm.Cd.1, na.rm = T)
# Ranges are equal
length(unique(crimeset$Crm.Cd))
length(unique(crimeset$Crm.Cd.1))
# 139 vs. 142
# Are more categories in Crm.Cd.Desc than in Crm.Cd?
length(unique(crimeset$Crm.Cd.Desc))
# Ans. NO

# Let's use a loop to find which values of Crm.Cd.1 are not in Crm.Cd
uniquesOne <- sort(unique(crimeset$Crm.Cd))
uniquesTwo <- sort(unique(crimeset$Crm.Cd.1))
tail(uniquesOne)
tail(uniquesTwo)
notFound <- numeric() # Auxiliar variable to store the not matched unique values
for (i in uniquesTwo) {
  if (length(which(uniquesOne == i)) == 0) {
    notFound <- c(notFound, i)
  }
}
# Result
print(notFound)

# How many values are equal in the variable Crm.Cd and the variable Crm.Cd.1
dim(crimeset)[1] - sum(crimeset$Crm.Cd == crimeset$Crm.Cd.1, na.rm = T)
sort(table(crimeset$Crm.Cd.1[which(crimeset$Crm.Cd != crimeset$Crm.Cd.1)]), decreasing = T)

head(crimeset$Crm.Cd)
head(crimeset$Crm.Cd.1)
tail(crimeset$Crm.Cd)
tail(crimeset$Crm.Cd.1)

# Example, let's look the value of the variable Crm.Cd when Crm.Cd.1 is 812
table(crimeset$Crm.Cd[which(crimeset$Crm.Cd.1 == 812)])
# Description for Crm.Cd = 812 and for Crm.Cd = 815
table(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 815)])
table(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 812)])
# Values for Crm.Cd.3 when Crm.
sort(table(crimeset$Crm.Cd.3[which(crimeset$Crm.Cd == 815 & crimeset$Crm.Cd.1 == 812)]))
table(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 820)])
table(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 821)])


# Create new columns with the corresponding descriptions of the crimes

# Re-start
range(crimeset$Crm.Cd)
range(crimeset$Crm.Cd.1, na.rm = T)
range(crimeset$Crm.Cd.2, na.rm = T)
range(crimeset$Crm.Cd.3, na.rm = T)
range(crimeset$Crm.Cd.4, na.rm = T)
# Now we have four variables that contains integer values, although not all the ranges are the
# same nor are included in the range of the variable Crm.Cd, let's think about this variable and
# the variable Crm.Cd.Desc as a dictionary to relate codes and their descriptions, and let's 
# take the variables Crm.Cd.1 to Crm.Cd.4 as different crimes charged to one same event.
# Let's explore some cases as an example:

# How many observations don't take NA as their value for the variable Crm.Cd.4?
sum(!is.na(crimeset$Crm.Cd.4))
# How many of this 64 have NAs for the variable Crm.Cd.3?
sum(is.na(crimeset$Crm.Cd.3[which(!is.na(crimeset$Crm.Cd.4))]))
# Ok, no NAs, as could be expected (before there are a fourth crime accounted must have been 
# accounted the three previous ones).
# Let's continue checking for Crm.Cd.2 and 1
sum(is.na(crimeset$Crm.Cd.2[which(!is.na(crimeset$Crm.Cd.4))]))
sum(is.na(crimeset$Crm.Cd.1[which(!is.na(crimeset$Crm.Cd.4))]))
# Ok, confirmed. Let's check now for three and before
sum(is.na(crimeset$Crm.Cd.3))
# 953045 missing values 
sum(!is.na(crimeset$Crm.Cd.3))
# 2294 are not missing
# Missings in Crm.Cd.2 when Crm.Cd.3 is not missing?
sum(is.na(crimeset$Crm.Cd.2[which(!is.na(crimeset$Crm.Cd.3))]))
# 0, Ok
# Missings in Crm.Cd.1 when Crm.Cd.3 is not?
sum(is.na(crimeset$Crm.Cd.1[which(!is.na(crimeset$Crm.Cd.3))]))
# 0, Ok
# Let's repeat the process for Crm.Cd.2
sum(is.na(crimeset$Crm.Cd.2))
sum(!is.na(crimeset$Crm.Cd.2))
# Missings in Crm.Cd.1 when Crm.Cd.2 is not missing?
sum(is.na(crimeset$Crm.Cd.1[which(!is.na(crimeset$Crm.Cd.2))]))
# This time is not fulfilled, there are eleven missings, which are all the missings of the 
# variable Crm.Cd.1.
# Crm.Cd.2 is missing when Crm.Cd.1 is it?
sum(is.na(crimeset$Crm.Cd.2[which(is.na(crimeset$Crm.Cd.1))]))
# No, so there are observations which have missing values for Crm.Cd.1 and not for Crm.Cd.2
# Let's observe them
sort(crimeset$Crm.Cd.2[which(is.na(crimeset$Crm.Cd.1))])
# Values are in the range of Crm.Cd

# Let's inspect some observations with values in the four variables 

indexes <- which(!is.na(crimeset$Crm.Cd.4))
crimeset$Crm.Cd.4[indexes[3]]
table(crimeset$Crm.Cd.4[indexes])
# Let's check when Crm.Cd.4 takes the value 821
crimeset[which(crimeset$Crm.Cd.4 == 821), c("Crm.Cd.1", "Crm.Cd.2", "Crm.Cd.3", "Crm.Cd.4")]
# Meaning of the codes.
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 810)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 812)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 820)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 821)])
# All are related to sexual/rape crimes
#
# Let's continue with the observations that take the value 910
crimeset[which(crimeset$Crm.Cd.4 == 910), c("Crm.Cd.1", "Crm.Cd.2", "Crm.Cd.3", "Crm.Cd.4")]
# Codes meaning:
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 210)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 230)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 761)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 910)])
# All categories seems related to a robbery.
#
# Let's continue now with 930
crimeset[which(crimeset$Crm.Cd.4 == 930), c("Crm.Cd.1", "Crm.Cd.2", "Crm.Cd.3", "Crm.Cd.4")]
# Codes meaning:
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 210)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 761)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 901)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 930)])
# Again categories related to robbery.
#
# Let's continue with 946
crimeset[which(crimeset$Crm.Cd.4 == 946), c("Crm.Cd.1", "Crm.Cd.2", "Crm.Cd.3", "Crm.Cd.4")]
# Codes meaning:
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 210)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 510)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 903)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 946)])
# All categories are related to a robbery.

# Let's explore now the categories for Crm.Cd.3
length(which(!is.na(crimeset$Crm.Cd.3)))
sort(table(crimeset$Crm.Cd.3[which(!is.na(crimeset$Crm.Cd.3))]), decreasing = T)
# Case 910:
crimeset[which(crimeset$Crm.Cd.3 == 910), c("Crm.Cd.1", "Crm.Cd.2", "Crm.Cd.3")]
# Row 16291:
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 350)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 626)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 910)])
# All seems related to kidnapping
# Row 65682
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 122)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 210)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 910)])
# All related to rape
# Row 947554:
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 236)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 761)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 910)])
# All related to kidnapping
#
# Case 434
crimeset[which(crimeset$Crm.Cd.3 == 434), c("Crm.Cd.1", "Crm.Cd.2", "Crm.Cd.3")]
# Row 275404:
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 121)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 230)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 434)])
# All related to rape
# Case 648
crimeset[which(crimeset$Crm.Cd.3 == 648), c("Crm.Cd.1", "Crm.Cd.2", "Crm.Cd.3")]
# Row 274723:
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 110)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 230)])
unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == 648)])
# All related to an homicide

# Let's create four new variables with the categories descriptions, those codes not found
# are going to be put as missing. After this is accomplished, let's transform each one of the
# four variables into a factor.

auxTransform <- function(code) {
  # Auxiliary funtion to do the transformation of the codes into their descriptions.
  # Input: a code and a column of the dataset crimeset from Crm.Cd.1 to Crm.Cd.4
  # Result: the description matching the code if found and NA otherwise
  result <- "NA" # Variable for return, NA by default
  crimeCode <- unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == code)])
  if (length(crimeCode) > 0) {
    result <- crimeCode
  }
  return(result)
}

# Testing how to apply the function to a vector:
test <- vapply(crimeset$Crm.Cd.1, auxTransform, character(1))
test <- sapply(crimeset$Crm.Cd.1, FUN = auxTransform)

# Let's try to do it in batches of 100k
test2 <- character(dim(crimeset)[1])
test2[1:100000] <- sapply(crimeset$Crm.Cd.1[1:100000], auxTransform)
test2[200001:300000] <- sapply(crimeset$Crm.Cd.1[200001:300000], auxTransform)
test2[300001:400000] <- sapply(crimeset$Crm.Cd.1[300001:400000], auxTransform)
test2[400001:500000] <- sapply(crimeset$Crm.Cd.1[400001:500000], auxTransform)
test2[500001:600000] <- sapply(crimeset$Crm.Cd.1[500001:600000], auxTransform)
test2[600001:700000] <- sapply(crimeset$Crm.Cd.1[600001:700000], auxTransform)
test2[700001:800000] <- sapply(crimeset$Crm.Cd.1[700001:800000], auxTransform)
test2[800001:900000] <- sapply(crimeset$Crm.Cd.1[800001:900000], auxTransform)
test2[900001:955339] <- sapply(crimeset$Crm.Cd.1[900001:955339], auxTransform)
# Approximately 7 minutes to execute each batch of 100k
# If we repeat this 7 times it would be...
7*7
# 49 minutes
# And for four variables...
49*4
# 196 minutes to complete the process (more than three hours)
# Test with vapply:
test3 <- character(dim(crimeset)[1])
test3[1:100000] <- vapply(crimeset$Crm.Cd.1[1:100000], auxTransform, character(1))
# Also about 7 minutes
# Make a funciton in cpp that do this work


# This is very impractical, it takes more than three hours to process create the new
# four variables. So instead of this, let's try to create four variables with only the 
# codes that have a corresponding description in the dicitonary, the other ones are going
# to be replaced by missing values (NAs). Also let's create a function like auxTransform to
# simplify the search of the description of a given code.

# First step, create four new variables with the same observations.
# Second step, using a new function replace by NAs all the codes that have no matching with
# Crm.Cd
# Third step convert the new variables into factors.
# Fourth step create a new function that match easily each code with its description.
# Obtain some results with the factor variables and tranlate them into the descriptions.

# 1st
crimeset$crime1 <- crimeset$Crm.Cd.1
crimeset$crime2 <- crimeset$Crm.Cd.2
crimeset$crime3 <- crimeset$Crm.Cd.3
crimeset$crime4 <- crimeset$Crm.Cd.4
# 2nd
# How many values in Crm.Cd.1 don't match any value in Crm.Cd?
uniquesZero <- sort(unique(crimeset$Crm.Cd))
uniquesOne <- sort(unique(crimeset$crime1))
length(uniquesZero)
length(uniquesOne)
# 139 and 141, so two observations don't match (without having into account the NA category).
dontMatch <- numeric() # Auxiliar variable to store the codes of crime1 that don't match any code
# in Crm.Cd
for (i in uniquesOne) {
  if (length(which(uniquesZero == i)) == 0) {
    dontMatch <- c(dontMatch, i)
  }
}


for (i in dontMatch) {
  crimeset$crime1 <- replace(crimeset$crime1, which(crimeset$crime1 == i), NA)
}

# How many values in Crm.Cd.2 don't match any value in Crm.Cd?
uniquesTwo <- sort(unique(crimeset$Crm.Cd.2))
length(uniquesZero)
length(uniquesTwo)
# Less values in Crm.Cd.2 than in Crm.Cd
dontMatch2 <- numeric()
for (i in uniquesTwo) {
  if (length(which(uniquesZero == i)) == 0) {
    dontMatch2 <- c(dontMatch2, i)
  }
}

for (i in dontMatch2) {
  crimeset$crime2 <- replace(crimeset$crime2, which(crimeset$crime2 == i), NA)
}

# How many values in Crm.Cd.3 don't match any value in Crm.Cd?
uniquesThree <- sort(unique(crimeset$Crm.Cd.3))
length(uniquesZero)
length(uniquesThree)
# 139 vs. 37, much less values in Crm.Cd.3

dontMatch3 <- numeric()
for (i in uniquesThree) {
  if (length(which(uniquesZero == i)) == 0) {
    dontMatch3 <- c(dontMatch3, i)
  }
}

for (i in dontMatch3) {
  crimeset$crime3 <- replace(crimeset$crime3, which(crimeset$crime3 == i), NA)
}

# How many values in Crm.Cd.4 don't match any value in Crm.Cd?
uniquesFour <- sort(unique(crimeset$Crm.Cd.4))
length(uniquesZero)
length(uniquesFour)
# 139 vs. 6

dontMatch4 <- numeric()
for (i in uniquesFour) {
  if (length(which(uniquesZero == i)) == 0) {
    dontMatch4 <- c(dontMatch4, i)
  }
}

for (i in dontMatch4) {
  crimeset$crime4 <- replace(crimeset$crime4, which(crimeset$crime4 == i), NA)
}

# Now let's create a loop to do all executing only one block of code

# Create the new variables with the same information
crimeset$crime1 <- crimeset$Crm.Cd.1
crimeset$crime2 <- crimeset$Crm.Cd.2
crimeset$crime3 <- crimeset$Crm.Cd.3
crimeset$crime4 <- crimeset$Crm.Cd.4

# Replace the non-matching codes by NAs
uniquesZero <- sort(unique(crimeset$Crm.Cd)) # Auxiliar variable with the unique observations of
                                             # Crm.Cd
for (i in c("crime1", "crime2", "crime3", "crime4")) {
  uniquesi <- sort(unique(crimeset[, i])) # Uniques of column i
  dontMatch <- numeric() # Auxiliar variable to store the codes of crime1 that don't match any code
  # in Crm.Cd
  for (j in uniquesi) {
    if (length(which(uniquesZero == j)) == 0) {
      dontMatch <- c(dontMatch, j)
    }
  }
  
  # Replace the codes that don't match with NAs
  for (k in dontMatch) {
    crimeset[, i] <- replace(crimeset[, i], which(crimeset[, i] == k), NA)
  }
}

# Transform the four variables into factor
crimeset$crime1 <- factor(crimeset$crime1)
crimeset$crime2 <- factor(crimeset$crime2)
crimeset$crime3 <- factor(crimeset$crime3)
crimeset$crime4 <- factor(crimeset$crime4)

# Create a function that return the description of a given code
getDescription <- function(code) {
  # Receives a positive integer code and if it's one of the values of the 
  # variable Crm.Cd returns the associated description.
  # Example:
  # Input: 510; Return: VEHICLE - STOLEN
  result <- NA # return variable, by default is NA
  checkCode <- unique(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == code)])
  if (length(checkCode) > 0) {
    result <- checkCode
  }
  return(result)
}

# Create a function that returns the code of a given description
getCode <- function(description) {
  # Recieves a character that must match one of the descriptions in Crm.Cd.Desc
  # Example:
  # Input: "VEHICLE - STOLEN"; Return: 510
  result <- NA # variable to be returned, NA by default
  checkDesc <- unique(crimeset$Crm.Cd[which(crimeset$Crm.Cd.Desc == description)])
  if (length(checkDesc) > 0) {
    result <- checkDesc
  }
  return(result)
}

# Let's try it
table(vapply(crimeset$crime4[which(!is.na(crimeset$crime4))], getDescription, character(1)))

# Let's create a data.frame with all the observations that have crime codes in crime4
testSet <- crimeset[which(!is.na(crimeset$crime4)),
                    c("crime1", "crime2", "crime3", "crime4")]


# ----------------------------------------------------------------------
# Question:
# ----------------------------------------------------------------------
# All the codes have one and only one description?

for (i in unique(crimeset$Crm.Cd)) {
  cat("code:", i, "\n")
  print(table(crimeset$Crm.Cd.Desc[which(crimeset$Crm.Cd == i)]))
  cat("-------------------------------------\n")
}
# Ans: yes
# ----------------------------------------------------------------------


testSetDesc <- data.frame(crime1 = character(5), crime2 = character(5), crime3 = character(5), 
                          crime4 = character(5))

for (j in 1:dim(testSetDesc)[2]) {
  testSetDesc[, j] <- vapply(testSet[, j], getDescription, character(1))
}

testSetDesc

vapply(as.numeric(names((head(sort(table(crimeset$crime1), decreasing = T), 10)))), getDescription, character(1))

# Weapon used when crime1 is "THEFT OF IDENTITY"? Values taken by the other crime variables?

table(crimeset$Weapon.Desc[which(crimeset$crime1 == getCode("THEFT OF IDENTITY"))])

sort(table(crimeset$crime2[which(crimeset$crime1 == getCode("THEFT OF IDENTITY"))]), decreasing = T)

vapply(head(as.numeric(names(sort(table(crimeset$crime2[which(crimeset$crime1 == getCode("THEFT OF IDENTITY"))]), decreasing = T))), 13), getDescription, character(1))

# crime3 takes any values?

sort(table(crimeset$crime3[which(crimeset$crime1 == getCode("THEFT OF IDENTITY"))]), 
     decreasing = T)

# Descriptions

vapply(as.numeric(head((names(sort(table(crimeset$crime3[which(crimeset$crime1 == getCode("THEFT OF IDENTITY"))]), 
           decreasing = T))), 2)), getDescription, character(1))


# 21) Variable: LOCATION

head(crimeset$LOCATION)
length(unique(crimeset$LOCATION))

# Most frequent locations
head(sort(table(crimeset$LOCATION), decreasing = T), 10)

# 22) Variable: Cross.Street

head(crimeset$Cross.Street)
tail(crimeset$Cross.Street)
head(crimeset$Cross.Street[which(crimeset$Cross.Street != "")])
length(unique(crimeset$Cross.Street))
sum(is.na(crimeset$Cross.Street))
sum(crimeset$Cross.Street == "")

# 23) Variable: LAT and LON

head(crimeset[, c("LAT", "LON")])
sum(is.na(crimeset$LAT))
sum(is.na(crimeset$LON))
head(sort(table(crimeset$LAT), decreasing = T), 10)
head(sort(table(crimeset$LON), decreasing = T), 10)


mapview(crimeset[1:1000, ], xcol = "LON", ycol = "LAT", crs = 4269, grid = FALSE)
# There are observations in the middle of the ocean, these are the ones that take
# the values (0, 0), these could be missing values
length(which(crimeset$LAT == 0))
# 2264 observations with 0 latitude
length(which(crimeset$LON == 0))
# Also 2264 observations with 0 longitude
#
# All the zero latitudes take also a zero longitude?
table(crimeset$LON[which(crimeset$LAT == 0)])
# Ans: Yes
# Let's map now a random sample of points.
mapview(crimeset[sample(1:dim(crimeset)[1], 95534), ], xcol = "LON", ycol = "LAT", crs = 4269, grid = FALSE)

# Let's plot a sample of points between the ones that are not zero
mapview(crimeset[sample(which(crimeset$LON != 0), round(0.05*dim(crimeset)[1])), ], xcol = "LON", ycol = "LAT", crs = 4269, grid = FALSE)

