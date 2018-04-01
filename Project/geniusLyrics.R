iTunesWeeklyData <- read.csv("iTunesWeeklyData.csv")
require(dplyr); require(rvest); require(stringr); require(readr)

# Section 1: Data frame for "artists" and "songs" ---------------------------------------------------------------
artistsSongs <- data.frame(gsub(" -.*$", "", as.character(iTunesWeeklyData$Artist.and.Title)), gsub(".*- ", "", as.character(iTunesWeeklyData$Artist.and.Title)))
colnames(artistsSongs) <- c("Artist", "Song")
artistsSongs$Artist <- as.character(artistsSongs$Artist)
artistsSongs$Song <- as.character(artistsSongs$Song)
artistsSongsCleaned <- artistsSongs[!duplicated(artistsSongs),] # Keep only unique rows

# Manual cleaning
artistsSongsCleaned$Artist <- str_replace_all(artistsSongsCleaned$Artist, "&", "and") %>%
  str_replace_all("P!nk", "P nk") %>% str_replace_all("P!NK", "P nk")
artistsSongsCleaned$Song <- str_replace_all(artistsSongsCleaned$Song, "The Fox", "The Fox (What Does the Fox Say)") %>%
  str_replace_all("A LIGHT THAT NEVER COME...", "A LIGHT THAT NEVER COMES")
artistsSongsCleaned[21,1] <- "Lana Del Rey"; artistsSongsCleaned[21,2] <- "Summertime Sadness (Cedric Gervais Remix)"

# Section 2: Function to generate Genius.com URL for a song ---------------------------------------------------------------
gen_song_url <- function(artist = NULL, song = NULL) {
  artist <- str_replace_all(artist, "[[:punct:]]", "")
  song <- str_replace_all(song, "\\s*\\(Ft.[^\\)]+\\)", "") %>%
    str_replace_all("&", "and") %>%
    str_replace_all("-", " ") %>%
    str_replace_all("[[:blank:]]+", " ") %>%
    str_trim() %>%
    str_replace_all("[[:punct:]]", "")
  base_url <- "https://genius.com/"
  query <- paste(artist, song, "lyrics", sep = "-") %>%
    str_replace_all(" ", "-")
  url <- paste0(base_url, query)
  return(url)
}


# Section 3: Function to isolate Genius.com lyrics for a song ---------------------------------------------------------------
# Parameter "info": "simple" returns only lyrics; "title" returns only the track title; "artist" returns the lyrics and artist; "all" returns the lyrics, artist, and title.

genius_url <- function(url, info = "all") {
  
  # Start a new session
  session <- html_session(url)
  
  # Clean the song lyrics
  lyrics <- gsub(pattern = "<.*?>",
                 replacement = "\n",
                 html_node(session, ".lyrics")) %>%
    read_lines() %>%
    na.omit() %>%
    str_replace_all("’", "'")
  
  # Artist
  artist <- html_nodes(session, ".header_with_cover_art-primary_info-primary_artist") %>%
    html_text() %>%
    str_replace_all("\n", "") %>%
    str_trim()
  
  # Song title
  song_title <- html_nodes(session, ".header_with_cover_art-primary_info-title") %>%
    html_text() %>%
    str_replace_all("\n", "") %>%
    str_trim()
  
  # Convert to tibble
  lyrics <- tibble(artist = artist,
                   title = song_title,
                   text = lyrics)
  
  # Isolate only lines that contain content
  index <- which(str_detect(lyrics$text, "[[:alnum:]]") == TRUE)
  lyrics <- lyrics[index,]
  
  # Remove lines with things such as [Intro: person & so and so]
  lyrics <- lyrics[str_detect(lyrics$text, "\\[|\\]") == FALSE, ]
  
  switch(info,
         Aimple = {return(select(lyrics, -artist, -title))},
         artist = {return(select(lyrics, -title))},
         title = {return(select(lyrics, -artist))},
         all = return(lyrics)
  )
}


# Section 4: Function to retrieve Genius.com lyrics for a song ---------------------------------------------------------------
# Parameters "artist" and "song": Spelling matters, capitalisation does not.

genius_lyrics <- function(artist = NULL, song = NULL, info = "all") {
  song_url <- gen_song_url(artist, song)
  lyrics <- genius_url(song_url, info) %>%
  mutate(line = row_number())
  return(lyrics)
}


# Section 5: Compile lyrics for all songs in a CSV file ---------------------------------------------------------------
geniusLyrics <- data.frame(artist=character(),
                           title=character(),
                           text=character(),
                           line=numeric())
  
for (i in 1:nrow(artistsSongs)){
  geniusLyrics_temp <- genius_lyrics(artistsSongsCleaned[i,1], artistsSongsCleaned[i,2])
  geniusLyrics <- bind_rows(geniusLyrics, geniusLyrics_temp)
  Sys.sleep(10)
}