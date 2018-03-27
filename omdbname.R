omdbinfo <- function (title)
{
  ## Call OMDB api
  library(XML)
  #title <- gsub(" & ","%26",title)
  omdburl <- paste0("http://www.omdbapi.com/?t=", title,
                    "&type=movie&y=&plot=short&r=xml&tomatoes=true")
  moviedata <- xmlToList(xmlParse(omdburl))
  if (moviedata$.attrs == "False")
    stop (paste0("Movie", title, "not found!"))
  m <- moviedata$movie
  
  
  ## Process information from OMDB
  year <- as.integer(m["year"])
  rated <- m["rated"]
  metascore <- as.integer(m["metascore"])
  imdb <- as.numeric(m["imdbRating"])
  nimdb <- as.integer(gsub(",","",m["imdbVotes"]))
  released <- as.Date(m["released"], format = "%d %b %Y")
  genre <- unlist(strsplit(m["genre"], ", *", fixed = FALSE))
  isnom <- regexpr("nomination", m["awards"]) > 0
  iswin <- isnom && (regexpr("win", m["awards"]) > 0)
  awards_nom <- awards_win <- 0
  if (iswin) {
    awards_win <- as.integer(gsub(".*[^0-9]*?([0-9]+) +wins?.*","\\1",
                                  m["awards"]))
  }
  if (isnom) {
    awards_nom <- as.integer(gsub(".*[^0-9]*?([0-9]+) +nominations?.*","\\1",
                                  m["awards"]))
  }
  
  ## Collect results and return
  out <- list()
  out$title <- m["title"]
  out$year <- year
  out$rated <- rated
  out$released <- released
  out$genre <- genre
  out$nominations <- awards_nom
  out$awards <- awards_win
  out$metascore <- metascore
  out$imdb <- imdb
  out$nimdb <- nimdb
  out
}