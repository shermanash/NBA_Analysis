# extract lines
library(rvest)
library(data.table)

#============================USAGE===========================================
# getodds('20151118')   [in form YYYYMMDD, can be vector of dates]
# >>> returns data frame of that days days / spreads
#============================================================================




# function teamAbb
# inputs: NBA name in long form e.g. 'Boston Celtics'
# outputs: abbreviated name e.g. 'BOS'
team_names <- read.csv("TeamNames.csv")
teamAbb <- function(longname) {
  return(team_names$ABB[team_names$TeamName == longname])
}

# function favscore-
# inputs: over/under, spread* *expects negative spread
# outputs: predicted score for favorite
favscore <- function(ou, spread) {
  return( (ou/2) - (.5*spread))
}

# MAIN function getodds
# inputs: a date OR vector of dates in format 'YYYYMMDD'
# outputs: a data frame containing scraped odds info for requested date(s)
getodds <- function(gamedate) {


        # read vegasinsider nba odds webpage
        url <- paste("http://www.donbest.com/nba/odds/",gamedate,".html",sep="")
        vegas <- read_html(url)
        
        # extract game times
        times <- html_nodes(vegas, ".alignLeft+ .alignCenter div")
        gametime <- html_text(times)
        
        # extract team names
        teams <- html_nodes(vegas, ".oddsTeamWLink")
        teams_text <- html_text(teams)
        
        # extract O/U + Spread (using pinnacle)
        lines <- html_nodes(vegas, ".bookColumn:nth-child(10) div")
        lines_text <- html_text(lines)
        
        # extract relevant info
        spread <- as.numeric(lines_text[lines_text < 0])
        overunder <- as.numeric(lines_text[lines_text >= 0])
        favorite <- teams_text[lines_text < 0]
        underdog <- teams_text[lines_text >= 0]
        # hometeam -- even numbered rows, away team odd
        hometeam <- teams_text[c(F,T)]
        awayteam <- teams_text[c(T,F)]
        
        # abbreviate all team names
        fav <- sapply(favorite, teamAbb)
        dog <- sapply(underdog, teamAbb)
        home <- sapply(hometeam, teamAbb)
        away <- sapply(awayteam, teamAbb)
                           
        # predict score of game from vegas info
        favpts <- favscore(overunder, spread)
        dogpts <- (favpts + spread)
        
        # create unique game ID of date+homeABB e.g. '20140515LAL'
        gameid <- paste(gamedate, home, sep="")
        
        # one row for each game in chronological order
        # gameid // date // time // home // away // spread  // OU // favorite // dog // favpts // dogpts
        framefinal <- data.frame(gameid, gamedate, gametime, home, away, spread, overunder, fav, favpts, dog, dogpts)
        
        # name the rows with the unique gameid
        rownames(framefinal) <- gameid
        
        return(framefinal)
}

#================================================================================================================
# this part wass for scraping the basketball-reference regular season CSV. It creates a list of dates
# that contained NBA games, which was used once to create the 2014_Odds_All.csv
# it is only needed for bulk requests and as such is commented out
# http://www.basketball-reference.com/leagues/NBA_2015_games.html <--- scraped page

# create a list of dates for input in correct format YYYYMMDD using basketball reference CSV
# CSV scraped from --> 
# bref <-read.csv("bref_2014_regseason_games.csv")

# obtain list of unique game days
# unique_dates <- unique(bref$Date)
#print(unique_dates)

# convert date info into format 'YYYYMMDD'
# dates <- as.Date(unique_dates, "%a %b %d %Y")
# datesfinal <- format(dates, "%Y%m%d")
# datesvector <- c(datesfinal)

#result <- lapply(datesvector, getodds)
#binded_data <- rbindlist(result)
#write.table(binded_data, file = "2014_Odds_121_150.csv", sep = ",", qmethod = "escape", row.names= FALSE)

#print(binded_data)

#===============================================================================================================


