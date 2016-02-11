# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'


## These are removed before the package is made the first time.
## library(DBI)
## library(stringi)
## library(dplyr)
## library(RPostgreSQL)
## library(XML)


## ## analyse.R
## # load used libraries
## library(dplyr)
#
## # Connect to database
## nfl_db <- src_postgres("nfl")
## nfl <- tbl(nfl_db, "scores")
#
#
## # create a matrix with this columns
## #1: DATE ID
## #2: AWAY ID
## #3: AWAY SCORE
## #4: HOME ID
## #5: HOME SCORE
#
# #the winner is always left unless there is an @ sign in clo6
# #
#' Takes values from a database connection string and returns a database
#' connection object to the local NFL database.
#'
#' @param host character string with hostname or IP address
#' @param port character string with the database port number
#' @param dbname character string of the name of the database to connect
#' @param user character string of the username
#' @param pwd character string of the user's password
#' @return A database connection object for the \code{nfl} local database.
#' @import DBI
#' @import RPostgreSQL
#' @export
connectDB <- function(host = "localhost", port = "5432",
                        dbname = "nfl", user = "msharp",
                        pwd = "nflpassword") {
  # Set the driver
  drv <- dbDriver("PostgreSQL")

  # Open connection to database
  conn <- dbConnect(drv,
                      host = host,
                      port = port,
                      dbname = dbname,
                      user = user,
                      password = pwd)
  conn
}

#HarvestProFootballResults.R
### Needed R packages:  XML
###                     dplyr
###
### Needed R files: connectDB.R
#' Takes start_year and last_year (defaults to current year) and returns
#' an integer vector representing a set of consecutive years.
#'
#' @param start_year integer vector of length 1 indication first year for
#' which to get NFL data.
#' @param last_year integer vector of length 1 indication last year for
#' which to get NFL data.
#' @return integer vector representing consecutive years.
#' @export
get_years <- function(start_year = 1966,
                      last_year = as.numeric(format(Sys.Date(), "%Y"))) {
  startYear:lastYear
}

## Load DB connector if not there
## if(!exists("nfl_db")){
##   source("src/connectDB.R")
## }
#
## ## Build vector of years to parse
## if (initialSetup){
##   years <- c(startYear:currentYear)
## } else {
##   years <- c(currentYear)
## }
#' Takes an integer vector representing consecutive years and returns in a
#' dataframe the NFL team statistics for the regular season games in those
#' years.
#'
#' @param years integer vector of four digit version of a years
#' @return dataframe with NFL team data for all years
#' @import stringi
#' @import XML
#' @export
get_nfl_data_from_web <- function(years) {
  df_games <- data.frame()
  for (year in years) {
    ## Get the raw HTML data
    tables <- suppressWarnings(readHTMLTable(
      stri_c("http://www.pro-football-reference.com/years/",
             year, "/games.htm"), header = TRUE, stringsAsFactors = FALSE))
    tmp_games <- tables[["games"]] # ignoring games left
    tmp_games[tmp_games[ , 6] == "", 6] <- NA
    tmp_games$Date <- as.Date(paste(tmp_games$Date, year, sep = ", "),
                              "%B %d, %Y")
    df_games <- rbind(df_games, tmp_games)
  }
  names(df_games) <- c("Week", "Day", "Date", "Col4", "Winner/tie", "Col6",
                       "Loser/tie", "PtsW", "PtsL", "YdsW", "TOW", "YdsL",
                       "TOL")
  get_regular_season_games(df_games)
}
#' Runs java garbage collection
#'
#' @import rJava
#' @return NULL
jgc <- function() {
  rJava::.jcall("java/lang/System", method = "gc")
}

#' Returns dataframe of NFL game data from local Excel file.
#'
#' @param years integer vector of four digit years.
#' @import stringi
#' @import readxl
#' @import XLConnect
#' @export
get_nfl_data_from_excel <- function(years) {
  options(java.parameters = "-Xmx4g")
  excel_file <- "inst/extdata/NFL_Reference.xlsx"
  wb <- loadWorkbook(filename = excel_file)
  nfl <- data.frame()
  for (year in as.character(years)) {
    ##jgc()
    ## Excel data
    if (existsSheet(wb, year)) {
      #tmp_nfl <- readWorksheetFromFile(file = excel_file, sheet = year,
       #                                check.names = FALSE)
      tmp_nfl <- suppressWarnings(
        read_excel(path = excel_file, sheet = as.character(year),
                   na = "", col_types = c(rep("text", 2), "date",
                                          rep("text", 10))))
      names(tmp_nfl) <- c("Week", "Day", "Date", "Col4", "Winner/tie", "Col6",
                           "Loser/tie", "PtsW", "PtsL", "YdsW", "TOW", "YdsL",
                           "TOL")
      tmp_nfl$Date <- as.Date(stri_c(year, stri_sub(tmp_nfl$Date, 5, 10)))
    } else {
      warning(stri_c("Excel file: ", excel_file, " does not have sheet: '",
                     year, "'."))
    }
    nfl <- rbind(nfl, tmp_nfl)
  }
  get_regular_season_games(nfl)
}
#' Returns dataframe of regular season games by removing extra headings,
#' playoff games, the by week, and any blank lines.
#'
#' @param df_games dataframe of NFL game data
#' @export
get_regular_season_games <- function(df_games) {
  ### Remove additional headlines, playoff games, by week, and blank lines
  df_games <- suppressWarnings(df_games[
    !is.na(as.numeric(as.character(df_games$Week))), ])
  df_games <- df_games[df_games[,4] != "", ]
  df_games
}
#' Returns dataframe with selected columns converted from character or factor
#' to numeric.
#'
#' If the column contains values that cannot be converted to a numeric value,
#' they will be coerced to NAs and a warning will be generated.
#'
#' @param my_df dataframe with columns.
#' @param cols character vector with column names of columns to be converted
#' to numeric.
#' @import stringi
#' @export
factor_to_numeric <- function(my_df, cols) {
  if (!all(suppressWarnings(sapply(cols, function(col) {
    any(stri_detect_fixed(colnames(my_df), col))}))))
    stop("All columns must be in the dataframe.")
  for (col in cols) {
    my_df[ , col] <- suppressWarnings(as.numeric(as.character(my_df[ , col])))
  }
  my_df
}
#' Changes NFL game results for Winning and Losing team format to Home and Away
#' format.
#'
#' @return dataframe of NFL game results with team names and results switched
#' according to game location. Changes NFL game results for Winning and Losing
#' team format to Home and Away format.
#' @param my_df dataframe with NFL game results
#' @param new_col character vector with the names of the new dataframe
#' columns
#' @param take_first logical vector of length equal to \code{nrow(my_df)}.
#' TRUE indicates that the loser is the Home team and columns indicated by
#' \code{replace_1} are assigned to \code{new_col} otherwise \code{replace_2}
#' is.
#' @param replace_1 character vector of the losing team results in an order
#' that corresponds to the winning team result columns in replace_2
#' @param replace_2 character vector of the losing team results in an order
#' that corresponds to the winning team result columns in replace_1
#' @examples
#' df_games <- data(df_games)
#' home_away_cols <- c("Home", "Away", "PtsH", "PtsA", "YdsH", "YdsA", "TOH", "TOA")
#'   df_games <-
#' win_lose_2_home_away(df_games,
#'                      take_first = df_games$Col6 == "@",
#'                      new_col = home_away_cols,
#'                      replace_1 = c("Loser/tie", "Winner/tie", "PtsL",
#'                                    "PtsW", "YdsL", "YdsW", "TOL",
#'                                    "TOW"),
#'                      replace_2 = c("Winner/tie", "Loser/tie", "PtsW",
#'                                    "PtsL", "YdsW", "YdsL", "TOW",
#'                                    "TOL"))

#' @export
win_lose_2_home_away <- function(my_df, take_first, new_col, replace_1,
                                 replace_2) {
  for (i in seq_along(replace_1)) {
    my_df[new_col[i]] <- NA
    my_df[new_col[i]][take_first] <-
      my_df[replace_1[i]][take_first]
    my_df[new_col[i]][!take_first] <-
      my_df[replace_2[i]][!take_first]
  }
  my_df
}

#' Returns a dataframe of NFL game data harvested from web.
#'
#' @return dataframe with NFL game data from one year of regular season games
#' @param years integer vector of years for which to collect data.
#' @export
harvest_nfl_game_stats <- function(years) {
  df_games <- get_nfl_data_from_web(years)
  ## Clean up data
  df_games <- get_regular_season_games(df_games)
  ### Add missing column names

  ### Set correct variable types
  df_games <- factor_to_numeric(df_games, c("Week", "PtsW", "PtsL", "YdsW",
                                            "YdsL", "TOW", "TOL"))
  df_games$`Winner/tie` <- as.character(df_games$`Winner/tie`)
  df_games$`Loser/tie` <- as.character(df_games$`Loser/tie`)

  ## Recode data
  home_away_cols <- c("Home", "Away", "PtsH", "PtsA", "YdsH", "YdsA", "TOH",
                      "TOA")
  df_games <-
    win_lose_2_home_away(df_games,
                         new_col = home_away_cols,
                         take_first = df_games$Col6 == "@",
                         replace_1 = c("Loser/tie", "Winner/tie", "PtsL",
                                       "PtsW", "YdsL", "YdsW", "TOL",
                                       "TOW"),
                         replace_2 = c("Winner/tie", "Loser/tie", "PtsW",
                                       "PtsL", "YdsW", "YdsL", "TOW",
                                       "TOL"))
  ### Remove unnessesary columns
  df_games[ , home_away_cols]
}

#' Returns NULL
#'
#' @param nfl_db database connection object
#' @param df_games dataframe with regular sesson NFL team results.
#' @import DBI
#' @import dplyr
update_db_with_new <- function(nfl_db, df_games) {
  ## If run in update mode, get the last db entry and only add new data
  sql <- stri_c("select Date, Home
         from scores
         WHERE Date = (select max(Date) from scores);")
  last.results <- dbFetch(dbSendQuery(nfl_db, sql))
  df_games <- df_games %>%
    filter(Date > max(last.results$Date))

  dbWriteTable(nfl_db, name = "scores", df_games, append = TRUE,
               row.names = FALSE)
}

## nfl <- get_nfl_data_from_excel(years)
## nfl <- factor_to_numeric(df_games, c("Week", "PtsW", "PtsL", "YdsW",
##                                           "YdsL", "TOW", "TOL"))
#
##   print("Recode data...")
##   ## Recode data
##   home_away_cols <- c("Home", "Away", "PtsH", "PtsA", "YdsH", "YdsA", "TOH",
##                       "TOA")
##   nfl <-
##     win_lose_2_home_away(nfl,
##                          new_col = home_away_cols,
##                          take_first = nfl$Col6 == "@",
##                          replace_1 = c("Loser/tie", "Winner/tie", "PtsL",
##                                        "PtsW", "YdsL", "YdsW", "TOL",
##                                        "TOW"),
##                          replace_2 = c("Winner/tie", "Loser/tie", "PtsW",
##                                        "PtsL", "YdsW", "YdsL", "TOW",
##                                        "TOL"))
##   #### Remove unnessesary columns
##   nfl <- nfl[ , home_away_cols]
#
##   print("Write file to DB...")
##   nfl_db <- connectDB(host = "localhost", port = "5432", dbname = "nfl",
##                       user = "msharp", pwd = "nflpassword")
##   ### Write data to db
##   dbWriteTable(nfl_db, name = "scores", nfl, append = TRUE, row.names = FALSE)
##   ## Disconnect from db
##   dbDisconnect(nfl_db)
## }
#
#
## #loadData.R
## # Load library
## library(dplyr)
#
## # Open data
## nfl_db <- src_postgres("nfl", host = "localhost", user = "dominik")
## nfl <- tbl(nfl_db, "scores")
#
## ## Get data
#
## nfl.df <- nfl %>%
##   filter(Date >= "2014-01-01",
##          Date < "2015-01-01") %>%
##   collect()
#
## nfl.years <- nfl %>%
##   select(Date) %>%
##   collect()
#
## nfl.years <- unique(year(nfl.years$Date))
