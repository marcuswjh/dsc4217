require(rvest)
require(magrittr)
require(tidyr)
require(dplyr)

url <- "http://kworb.net/cc/archive/us/"

itunes_html = read_html(url)


if (length(itunes_html %>% html_nodes("table")) > 1) {
   print("The structure of the website may have changed. Please check.")
}

## strong assumptions on structure, how to relax and check for assumptions?
itunes_link_vec <- itunes_html %>% 
  html_nodes("table") %>%
  html_nodes("tr") %>% 
  html_nodes("td") %>%
  html_nodes("a") %>% 
  html_attr("href")

itunes_link_vec <- itunes_link_vec[grepl("^20", itunes_link_vec)]
if (length(itunes_link_table) == 0) {
  print("No matches for 20yymmdd.html. The structure of the website 
        may have changed. Please check.")
}

getWeekiTunesTable <- function (link) {
  # assumes that the URL hasn't changed, 
  # and assumes a hardcoded structure of the webpage
  suburl <- paste(url, link, sep = "")
  subitunes_html <- read_html(suburl)
  subitunes_table <- subitunes_html %>% 
    html_nodes("table") %>% 
    html_table()
  if (length(subitunes_table) > 1 | length(subitunes_table) < 1){
    print("The structure of the weekly archives may have changed. Please check.")
  }
  subitunes_df <- subitunes_table[[1]]
  colnames(subitunes_df) <- subitunes_df[1, ] #assumes that second row contains the desired names
  ncol_temp <- dim(subitunes_df)[2]
  colnames(subitunes_df)[(ncol_temp-2):(ncol_temp-1)] <- c("Last week", "This week")
  subitunes_df <- subitunes_df[-1,]
  subitunes_df$date <- as.Date(gsub(".html", "", link), "%Y%m%d")
  #if (!"4d"%in%colnames(subitunes_df)) {
  #  subitunes_df$`4d` = NA
  #}
  return(list(subitunes_df))
}

itunesData <- bind_rows(sapply(itunes_link_vec, getWeekiTunesTable))

itunesData$Sales <- as.numeric(gsub("k","",itunesData$`This week`))

View(itunesData %>% group_by(date ,`Artist and Title`) %>%
       select(`Artist and Title`, date, Sales) %>%
       summarize(TotalSales = sum(Sales)) %>%
  spread(date, TotalSales))

write.csv(itunesData, file = "iTunesWeeklyData.csv")

### worthless code (failed tests)

# itunes_link_table <- itunes_html %>% 
#   html_nodes(xpath = "/html/body/table") %>%
#   html_table()
# 
# itunes_link_table %>%
#   extract2(1)

#View(itunesData[c(25164, 26093, 28190, 29134, 34286, 35461, 37088, 37860, 38513, 39562, 72432, 72554, 72555, 73948, 74110, 74122, 75442, 75638, 75662, 76902, 77189, 77257, 78544, 78916, 79152, 79988, 80353, 80536, 81674, 82323, 140391, 141349, 155903, 156479, 157335, 157987, 158744, 158945, 160178, 160419, 161613, 161889, 163057, 163366, 164493, 164698, 165922, 166228, 167380, 168502, 174598, 175885, 176030, 177339, 177436, 178730, 178848, 179801, 180273, 180985, 181993, 182740, 181663, 181855, 201700, 201858, 222186, 222234, 223224, 223302, 224285, 224387, 225310, 225468, 226333, 226615, 227366, 227643, 228531, 229036),])
#(25164, 26093), (28190, 29134), (34286, 35461), (37088, 37860), (38513, 39562), (72432, 72554, 72555), (73948, 74110, 74122), (75442, 75638, 75662), (76902, 77189, 77257), (78544, 78916, 79152), (79988, 80353, 80536), (81674, 82323), (140391, 141349), (155903, 156479), (157335, 157987), (158744, 158945), (160178, 160419), (161613, 161889), (163057, 163366), (164493, 164698), (165922, 166228), (167380, 168502), (174598, 175885), (176030, 177339), (177436, 178730), (178848, 179801), (180273, 180985), (181993, 182740), (181663, 181855), (201700, 201858), (222186, 222234), (223224, 223302), (224285, 224387), (225310, 225468), (226333, 226615), (227366, 227643), (228531, 229036)


