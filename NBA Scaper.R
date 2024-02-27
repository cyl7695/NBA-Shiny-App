# Load necessary libraries
library(rvest)

#Merge based on player name and year and use the averages from that.

# URL pattern for basketball-reference.com
base_url <- "https://www.basketball-reference.com/players/"

# List of players and player names
players <- c("j/jamesle01", "d/duranke01", "w/westbru01", "c/curryst01", "a/anthoca01")
player_names <- c("LeBron James", "Kevin Durant", "Russell Westbrook", "Stephen Curry", "Carmelo Anthony")

# Initialize an empty data frame to store combined stats
combined_stats <- data.frame()

# Seasons of interest
seasons_of_interest <- c("2003-04", "2004-05", "2005-06", "2006-07", "2007-08",
                         "2008-09", "2009-10", "2010-11", "2011-12", "2012-13",
                         "2013-14", "2014-15", "2015-16", "2016-17", "2017-18")

# Loop through each player
for (i in seq_along(players)) {
  # Construct URL for the player
  url <- paste0(base_url, players[i], ".html#per_game")
  
  # Read HTML
  webpage <- read_html(url)
  
  # Scrape "Per Game" table
  table <- html_table(html_nodes(webpage, "#per_game"), fill = TRUE)[[1]]
  
  # Filter seasons of interest
  table <- table[table$Season %in% seasons_of_interest, ]
  
  # Add a new column with the player name
  table$Player <- player_names[i]
  
  # Append to combined stats
  combined_stats <- rbind(combined_stats, table)
}

# View the combined stats
print(head(combined_stats))