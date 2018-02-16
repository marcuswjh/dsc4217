require(magrittr)
require(dplyr)
require(mice)

srxData <- read.csv("srx (Team 6).csv", stringsAsFactors = TRUE)
srxData$X <- NULL #get rid of row indices
#na_count <-sapply(srxData, function(y) sum(length(which(is.na(y)))))
na_count <-sapply(srxData, function(y) sum(is.na(y)))
na_count

md.pattern(srxData)

#srxData$Property.Name
#srxData$Property.Type   Got time do HDB dummy variable
suppressWarnings(srxData$Asking <- ifelse(srxData$Asking=="View to offer", NA, as.numeric(gsub(",", "", gsub("\\$", "", srxData$Asking)))))
srxData$AskingAvail <- is.na(srxData$Asking)
srxData$PSFLand <- as.numeric(gsub(".*[(]Built-up[)].*", "", gsub(",", "", gsub(".*\\$([0-9,]+(?=\\s+psf\\s+[(]Land[)])).*", "\\1", srxData$PSF, perl = TRUE)))) #creates a new numeric vector to store PSF by Land #should I create a factor for when it is NA?
srxData$PSFBuilt <- as.numeric(gsub(".*[(]Land[)].*", "", gsub(",", "", gsub(".*\\$([0-9,]+(?=\\s+psf\\s+[(]Built-up[)])).*", "\\1", srxData$PSF, perl = TRUE)))) #creates a new numeric vector to store PSF by Built
#srxData$Built.Year #should I create a factor for when it is NA?
srxData$Model <- factor(gsub("(?!.*[)])([(].*+)", "\\1)", srxData$Model, perl = TRUE)) #closes unclosed round brackets in the data
  
#A lot of work for Developer
  srxData$Developer <- factor(toupper(gsub("(?i)limited(?-i)", "LTD", srxData$Developer)))
  srxData$Developer <- factor(gsub("[(]*(?i)PRIVATE[)]*(?-i)", "PTE", srxData$Developer, perl = TRUE))
#manual fixing for spotted errors
  srxData$Developer <- factor(gsub("(BUKI SEMABWANG ESTATES LTD)|(BUKIT SEMABWANG ESTATES LTD)", "BUKIT SEMBAWANG ESTATES LIMITED", srxData$Developer)) #should have a dummy for every company. Not sure ASTON and ASTOR are same companies.
  srxData$Developer <- factor(gsub("CITY DEVELOPMENT LTD", "CITY DEVELOPMENTS LTD", srxData$Developer))
  srxData$Developer <- factor(gsub("ORGANISATION", "ORGANIZATION", srxData$Developer))
  srxData$Developer <- factor(gsub("FAIRVIEW DEVELOPMENT PTE LTD", "FAIRVIEW DEVELOPMENTS PTE LTD", srxData$Developer))
#srxData$Address ??
#srxData$District #can it be obtained from the address?
#srxData$Bedrooms #is 1+1 same as 2? Is Studio same as 1?
#srxData$Bathrooms #should I create a factor for NA's?
#srxData$Floor #is ground floor same as floor 1? Factor for NA's? high, low and mid floors - how to use?
srxData$AreaLand <- ifelse(grepl("sqft",srxData$Area)|is.na(srxData$Area),as.numeric(gsub(".*(([(]Built-up[)])|(sqm)).*", "", gsub(",", "", gsub("^.*(\\s|^)([0-9,]+(?=\\s+sqft\\s+[(]Land[)])).*", "\\2", srxData$Area, perl = TRUE)))), as.numeric(gsub(".*(([(]Built-up[)])|(sqft)).*", "", gsub(",", "", gsub("^.*(\\s|^)([0-9,]+(?=\\s+sqm\\s+[(]Land[)])).*", "\\2", srxData$Area, perl = TRUE)))))
srxData$AreaBuilt <- ifelse(grepl("sqft",srxData$Area)|is.na(srxData$Area),as.numeric(gsub(".*(([(]Land[)])|(sqm)).*", "", gsub(",", "", gsub("^.*(\\s|^)([0-9,]+(?=\\s+sqft\\s+[(]Built-up[)])).*", "\\2", srxData$Area, perl = TRUE)))), as.numeric(gsub(".*(([(]Land[)])|(sqft)).*", "", gsub(",", "", gsub("^.*(\\s|^)([0-9,]+(?=\\s+sqm\\s+[(]Built-up[)])).*", "\\2", srxData$Area, perl = TRUE)))))
#srxData$Tenure #can I just leave the factors as they are? Factor for NA's?
#srxData$No..of.Units #Factor for NA's?
#srxData$Facilities #split into factors?
#srxData$Agent #Looks ok?
srxData$Furnish <- addNA(srxData$Furnish)
#srxData$HDB.Town #Factor for NA's? How to identify HDBs?

