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
srxData$AskingAvail <- !is.na(srxData$Asking) #correspond to those that do not offer their prices. More expensive?. PSF also missing for same reason.
srxData$PSFLand <- as.numeric(gsub(".*[(]Built-up[)].*", "", gsub(",", "", gsub(".*\\$([0-9,]+(?=\\s+psf\\s+[(]Land[)])).*", "\\1", srxData$PSF, perl = TRUE)))) #creates a new numeric vector to store PSF by Land #should I create a factor for when it is NA?
srxData$PSFBuilt <- as.numeric(gsub(".*[(]Land[)].*", "", gsub(",", "", gsub(".*\\$([0-9,]+(?=\\s+psf\\s+[(]Built-up[)])).*", "\\1", srxData$PSF, perl = TRUE)))) #creates a new numeric vector to store PSF by Built
#srxData$Built.Year #should I create a factor for when it is NA?
srxData$Model <- factor(gsub("(?!.*[)])([(].*+)", "\\1)", srxData$Model, perl = TRUE)) #closes unclosed round brackets in the data

#treat the nearly mutually exclusive NA's in HDB Town and District
#manually treat the missing NA for both HDB Town and District
srxData[which((is.na(srxData$HDB.Town) == TRUE) & (is.na(srxData$District) == TRUE)),]$HDB.Town <- "Queenstown"

#A lot of work for Developer
  srxData$Developer <- factor(toupper(gsub("(?i)limited(?-i)", "LTD", srxData$Developer)))
  srxData$Developer <- factor(gsub("[(]*(?i)PRIVATE[)]*(?-i)", "PTE", srxData$Developer, perl = TRUE))
#manual fixing for spotted errors
  srxData$Developer <- factor(gsub("(BUKI SEMABWANG ESTATES LTD)|(BUKIT SEMABWANG ESTATES LTD)", "BUKIT SEMBAWANG ESTATES LIMITED", srxData$Developer)) #should have a dummy for every company. Not sure ASTON and ASTOR are same companies.
  srxData$Developer <- factor(gsub("CITY DEVELOPMENT LTD", "CITY DEVELOPMENTS LTD", srxData$Developer))
  srxData$Developer <- factor(gsub("ORGANISATION", "ORGANIZATION", srxData$Developer))
  srxData$Developer <- factor(gsub("FAIRVIEW DEVELOPMENT PTE LTD", "FAIRVIEW DEVELOPMENTS PTE LTD", srxData$Developer))
  srxData$Developer <- ifelse(grepl("HDB",srxData$Property.Type),
                                      "HOUSING DEVELOPMENT BOARD",srxData$Developer)
#srxData$Address ??
#srxData$District #can it be obtained from the address?
#srxData$Bedrooms #is 1+1 same as 2? Is Studio same as 1?
#srxData$Bathrooms #should I create a factor for NA's?
#srxData$Floor #is ground floor same as floor 1? Factor for NA's? high, low and mid floors - how to use?
srxData$Floor <- addNA(srxData$Floor)
levels(srxData$Floor) <- ifelse(grepl("01",levels(srxData$Floor)),"GROUND",levels(srxData$Floor))

sqmToSqftConvFactor <- 10.7639
srxData$AreaLand <- ifelse(grepl("sqft",srxData$Area)|
                             is.na(srxData$Area),
                           as.numeric(gsub(".*(([(]Built-up[)])|(sqm)).*", "", gsub(",", "", gsub("^.*(\\s|^)([0-9,]+(?=\\s+sqft\\s+[(]Land[)])).*", "\\2", srxData$Area, perl = TRUE)))), 
                           as.numeric(gsub(".*(([(]Built-up[)])|(sqft)).*", "", gsub(",", "", gsub("^.*(\\s|^)([0-9,]+(?=\\s+sqm\\s+[(]Land[)])).*", "\\2", srxData$Area, perl = TRUE))))*sqmToSqftConvFactor)
srxData$AreaBuilt <- ifelse(grepl("sqft",srxData$Area)|
                              is.na(srxData$Area),
                            as.numeric(gsub(".*(([(]Land[)])|(sqm)).*", "", gsub(",", "", gsub("^.*(\\s|^)([0-9,]+(?=\\s+sqft\\s+[(]Built-up[)])).*", "\\2", srxData$Area, perl = TRUE)))), 
                            as.numeric(gsub(".*(([(]Land[)])|(sqft)).*", "", gsub(",", "", gsub("^.*(\\s|^)([0-9,]+(?=\\s+sqm\\s+[(]Built-up[)])).*", "\\2", srxData$Area, perl = TRUE))))*sqmToSqftConvFactor)
levels(srxData$Tenure) <- ifelse(grepl("Freehold",levels(srxData$Tenure)),"FREEHOLD",levels(srxData$Tenure)) #can I just leave the factors as they are? Factor for NA's?
levels(srxData$Tenure) <- ifelse(grepl("946 years from 01/01/1938",levels(srxData$Tenure)) | grepl("946 years from 23/06/1938",levels(srxData$Tenure)) | grepl("947 years from 05/10/1934",levels(srxData$Tenure)),"999 years",levels(srxData$Tenure))
#srxData$No..of.Units #Factor for NA's?
#srxData$Facilities #split into factors?
#srxData$Agent #Looks ok?
srxData$Furnish <- addNA(srxData$Furnish)
#srxData$HDB.Town #Factor for NA's? How to identify HDBs?



