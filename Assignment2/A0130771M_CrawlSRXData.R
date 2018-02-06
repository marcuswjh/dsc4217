library(rvest)
require(dplyr)

#Note that in the example here, I use ".listingDetailTitle", not ".notranslate" as I
#wrote in class. This is because even if there is no data in a page, SRX may use ".notranslate"
#to store agent data. Thus we cannot tell whether the page is end of the search.
#please try to run Get1stProperty(100), Get1stProperty(10000), Get1stProperty(100000) repectively
#to tell the difference.
GetAllProperties <- function(n){
    url <- paste("https://www.srx.com.sg/search/sale/residential?page=", n, sep = "")
    srx <- read_html(url)
    listing <- html_nodes(srx, ".listingDetailTitle")
    lenList <- length(listing)
    if(lenList == 0)
        return("Done")
    urls <- rep("",lenList)
    for (i in 1:lenList) {
      urls[i] <- html_attr(listing[i], "href")
    }
    #a<-html_children(listing[1])
    #title <- html_text(a)
    return (urls)
}


GetPropertyData <- function(truncUrl){
  url <- paste("https://www.srx.com.sg", truncUrl, sep = "")
  srx <- read_html(url)
  listingDetails <- html_nodes(srx, "#listingInfo")
  
  propData <- listingDetails %>% 
    html_children() %>% 
    html_children() %>% 
    html_children() %>% 
    html_children() %>% 
    html_text() %>%
    trimws()
  
  df_prop <- data.frame(t(propData[seq(2,length(propData),2)]), stringsAsFactors = FALSE)
  colnames(df_prop) <- gsub("[: ]*", "", propData[seq(1,length(propData),2)])
  
  return (df_prop)
}

#Run Result(1000), Result(10000), Result(100000) and compare the results.
Result <- function(n){
   result = tryCatch(
               {
                    return (GetAllProperties(n))
               },
               warning = function(n) {
                   print(paste("Warning in crawling the ", n, "th page of SRX", sep = ""))
               },
               error = function(n) {
                 print(paste("Error in crawling the ", n, "th page of SRX", sep = ""))
                 return("Done")
               },
              finally = function(n)
             {}
          )
        return (result)
}


#To save time, I started from the 1650th page. You may change it to n<-1L to start from page 1.
n<-1654L
df <- NULL

while(n<10000){

    propertyUrls <- Result(n)
    if(propertyUrls[1] == "Done")
        break
    else
      df <- bind_rows(df, bind_rows(sapply(propertyUrls, GetPropertyData)))
    end
    n<-n+1
}


# df <- data.frame(PropertyName=character(),
#                  Type=character(),
#                  Asking=character(),
#                  PSF = character(),
#                  BuiltYr = character(),
#                  Model = character(),
#                  Developer = character(),
#                  Address = character(),
#                  District = character(),
#                  Bedrooms = character(),
#                  Bathrooms = numeric(),
#                  Floor = numeric(),
#                  Area = character(),
#                  Tenure = character(),
#                  NoUnits = numeric())