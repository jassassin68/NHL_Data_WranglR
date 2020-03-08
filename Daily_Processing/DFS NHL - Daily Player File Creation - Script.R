# Libraries
library(rvest)
library(data.table)
library(dplyr)
library(stringdist)
library(ade4)
library(clv)
library(tidyr)
library(stringi)
library(Rsymphony)

# Fields needed in order to make DKPts, PercDrafted_Cash, and PercDrafted_GPP predictions:
# DKPts:
#     - "Salary",
#     - "Implied_goals",
#     - "Line.Line1",
#     - "Line.Line2",
#     - "PP_Line.0",
#     - "PP_Line.PP1",
#     - "AvgAssistsPerGame_Lastx",
#     - "AvgShotsPerGame_Lastx",
#     - "AvgBlocksPerGame_Lastx",
#     - "AvgTimeOnIcePerGame_Lastx"
# PercDrafted_Cash:
#     - "Salary",
#     - "Implied_goals",
#     - "GamesInSlate",
#     - "Line.D1",
#     - "Line.D2",
#     - "Line.D3",
#     - "Line.Line1",
#     - "Line.Line2",
#     - "Line.Line3",
#     - "PP_Line.0",
#     - "PP_Line.PP1",
#     - "Position.C",
#     - "Position.D",
#     - "AvgAssistsPerGame_Lastx",
#     - "AvgShotsPerGame_Lastx",
#     - "AvgBlocksPerGame_Lastx",
#     - "AvgTimeOnIcePerGame_Lastx"
# PercDrafted_GPP:
#     - "Salary",
#     - "Implied_goals",
#     - "GamesInSlate",
#     - "Line.D1",
#     - "Line.D2",
#     - "Line.D3",
#     - "Line.Line1",
#     - "Line.Line2",
#     - "Line.Line3",
#     - "PP_Line.0",
#     - "PP_Line.PP1",
#     - "Position.D",
#     - "AvgAssistsPerGame_Lastx",
#     - "AvgShotsPerGame_Lastx",
#     - "AvgBlocksPerGame_Lastx",
#     - "AvgTimeOnIcePerGame_Lastx"

# Distinct Columns:
#     - "Salary",
#     - "Implied_goals",
#     - "GamesInSlate",
#     - "Line.D1",
#     - "Line.D2",
#     - "Line.D3",
#     - "Line.Line1",
#     - "Line.Line2",
#     - "Line.Line3",
#     - "PP_Line.0",
#     - "PP_Line.PP1",
#     - "Position.D",
#     - "AvgAssistsPerGame_Lastx",
#     - "AvgShotsPerGame_Lastx",
#     - "AvgBlocksPerGame_Lastx",
#     - "AvgTimeOnIcePerGame_Lastx"

# Sources:
# Daily DKSalaries file: (name = dk_file)
#     - Player Name
#     - Salary
#     - GamesInSlate
#     - Position
#     - Team

# Rotogrinders daily NHL page (name = team_goals)
#     - "Implied_goals",

# Daily Faceoff NHL team lineup pages (name = all_players)
#     - Lines and PPLines

# Hockey Reference Players Stats csv file
#     - AvgGoals, Assists, Shots, Blocks, and TimeOnIce

# Model outputs for each of the three predictions being made
#     - Model coefficients in csv format

# Final Output:
# Player Name, Position, Salary, Team, team implied goals, Line, PPLine, 
#     DKPt proj, Cash % Owned proj, GPP % Owned proj, "Cash Play Rating", "GPP Play Rating"
# Second output with best pairs, three-man plays of the slate for Cash and GPP 
# (need to figure out how to incorporate player correlations?)



# Tonight's DK Salary File - Manipulation
#write DK file to R

dk_file <- read.csv(file = 'C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Daily Datasets/DKSalaries.csv', 
                    header=TRUE, 
                    sep=",",
                    stringsAsFactors = FALSE)

#write team abbreviation table to R

abbrev_table <- read.csv(file = 'C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Daily Datasets/Team Abbreviation Table - DK.csv', 
                         header=TRUE, 
                         sep=",",
                         stringsAsFactors = FALSE)

#Update dk file team names. Can be used to confirm starters later.
#***Confirm all Vegas abbreviations are correct***
dk_file <- merge(dk_file, abbrev_table, by = 'TeamAbbrev')
dk_file <- dk_file[,c('Name', 'ID', 'Salary', 'Roster.Position', 'AvgPointsPerGame', 'Team')]
colnames(dk_file)[colnames(dk_file) == 'Team'] <- 'teamAbbrev'
colnames(dk_file)[colnames(dk_file) == 'Roster.Position'] <- 'Position'


# Rotogrinders daily NHL page
#RotoGrinders - Goalies, Full Strength lines, Vegas
rotogrinders <- read_html("https://rotogrinders.com/lineups/nhl?site=draftkings")

#ROTOGRINDERS DATA
#All teams on RG site
teams <- html_text(html_nodes(rotogrinders, ".mascot"))

# Implied Goals
implied_goals <- html_text(html_nodes(rotogrinders, ".ou a"))

# Create dataframe from teams, and include home/away info
home_away <- c("Away","Home")

home_away <- rep(home_away, (length(teams) / 2))

teams_df <- data.frame(Team = teams,
                        Home_Away = home_away,
                       Implied_goals = as.numeric(implied_goals))

#Only include teams that are also in DK file
teams_df <- merge(teams_df, dk_file, by.x = "Team", by.y = "teamAbbrev")

teams_df <- teams_df %>%
        select(Team, Home_Away, Implied_goals) %>%
        distinct()

team_goals <- data.frame(teamAbbrev = teams_df$Team,
                         Home_Away = teams_df$Home_Away,
                         Implied_goals = teams_df$Implied_goals)

# 
# implied_goals <- tail(implied_goals, length(teams_df$Team))
# 
# team_goals <- data.frame(teamAbbrev = teams_df$Team,
#                          Home_Away = teams_df$Home_Away,
#                          Implied_goals = as.numeric(implied_goals))
# 
# 




# Daily Faceoff webscraping section
#Even Strength and PP Lines

#Scraping Section

#Pull together all URLs for Line pages
ducks_page          <- 'https://www.dailyfaceoff.com/teams/anaheim-ducks/line-combinations/'
coyotes_page        <- 'https://www.dailyfaceoff.com/teams/arizona-coyotes/line-combinations/'
bruins_page         <- 'https://www.dailyfaceoff.com/teams/boston-bruins/line-combinations/'
sabres_page         <- 'https://www.dailyfaceoff.com/teams/buffalo-sabres/line-combinations/'
flames_page         <- 'https://www.dailyfaceoff.com/teams/calgary-flames/line-combinations/'
hurricanes_page     <- 'https://www.dailyfaceoff.com/teams/carolina-hurricanes/line-combinations/'
blackhawks_page     <- 'https://www.dailyfaceoff.com/teams/chicago-blackhawks/line-combinations/'
avalanche_page      <- 'https://www.dailyfaceoff.com/teams/colorado-avalanche/line-combinations/'
blue_jackets_page   <- 'https://www.dailyfaceoff.com/teams/columbus-blue-jackets/line-combinations/'
stars_page          <- 'https://www.dailyfaceoff.com/teams/dallas-stars/line-combinations/'
red_wings_page      <- 'https://www.dailyfaceoff.com/teams/detroit-red-wings/line-combinations/'
oilers_page         <- 'https://www.dailyfaceoff.com/teams/edmonton-oilers/line-combinations/'
panthers_page       <- 'https://www.dailyfaceoff.com/teams/florida-panthers/line-combinations/'
kings_page          <- 'https://www.dailyfaceoff.com/teams/los-angeles-kings/line-combinations/'
wild_page           <- 'https://www.dailyfaceoff.com/teams/minnesota-wild/line-combinations/'
canadiens_page      <- 'https://www.dailyfaceoff.com/teams/montreal-canadiens/line-combinations/'
predators_page      <- 'https://www.dailyfaceoff.com/teams/nashville-predators/line-combinations/'
devils_page         <- 'https://www.dailyfaceoff.com/teams/new-jersey-devils/line-combinations/'
islanders_page      <- 'https://www.dailyfaceoff.com/teams/new-york-islanders/line-combinations/'
rangers_page        <- 'https://www.dailyfaceoff.com/teams/new-york-rangers/line-combinations/'
senators_page       <- 'https://www.dailyfaceoff.com/teams/ottawa-senators/line-combinations/'
flyers_page         <- 'https://www.dailyfaceoff.com/teams/philadelphia-flyers/line-combinations/'
penguins_page       <- 'https://www.dailyfaceoff.com/teams/pittsburgh-penguins/line-combinations/'
sharks_page         <- 'https://www.dailyfaceoff.com/teams/san-jose-sharks/line-combinations/'
blues_page          <- 'https://www.dailyfaceoff.com/teams/st-louis-blues/line-combinations/'
lightning_page      <- 'https://www.dailyfaceoff.com/teams/tampa-bay-lightning/line-combinations/'
maple_leafs_page    <- 'https://www.dailyfaceoff.com/teams/toronto-maple-leafs/line-combinations/'
canucks_page        <- 'https://www.dailyfaceoff.com/teams/vancouver-canucks/line-combinations/'
knights_page        <- 'https://www.dailyfaceoff.com/teams/vegas-golden-knights/line-combinations/'
capitals_page       <- 'https://www.dailyfaceoff.com/teams/washington-capitals/line-combinations/'
jets_page           <- 'https://www.dailyfaceoff.com/teams/winnipeg-jets/line-combinations/'

#Read HTML for all Line pages, including sys.sleep calls (to slow down site navigation)
Sys.sleep(time = 3)
df_ducks <- read_html(ducks_page)
Sys.sleep(time = 3)
df_coyotes <- read_html(coyotes_page)
Sys.sleep(time = 3)
df_bruins <- read_html(bruins_page)
Sys.sleep(time = 3)
df_sabres <- read_html(sabres_page)
Sys.sleep(time = 3)
df_flames <- read_html(flames_page)
Sys.sleep(time = 3)
df_hurricanes <- read_html(hurricanes_page)
Sys.sleep(time = 3)
df_blackhawks <- read_html(blackhawks_page)
Sys.sleep(time = 3)
df_avalanche <- read_html(avalanche_page)
Sys.sleep(time = 3)
df_blue_jackets <- read_html(blue_jackets_page)
Sys.sleep(time = 3)
df_stars <- read_html(stars_page)
Sys.sleep(time = 15)
df_red_wings <- read_html(red_wings_page)
Sys.sleep(time = 3)
df_oilers <- read_html(oilers_page)
Sys.sleep(time = 3)
df_panthers <- read_html(panthers_page)
Sys.sleep(time = 3)
df_kings <- read_html(kings_page)
Sys.sleep(time = 3)
df_wild <- read_html(wild_page)
Sys.sleep(time = 3)
df_canadiens <- read_html(canadiens_page)
Sys.sleep(time = 3)
df_predators <- read_html(predators_page)
Sys.sleep(time = 3)
df_devils <- read_html(devils_page)
Sys.sleep(time = 3)
df_islanders <- read_html(islanders_page)
Sys.sleep(time = 30)
df_rangers <- read_html(rangers_page)
Sys.sleep(time = 3)
df_senators <- read_html(senators_page)
Sys.sleep(time = 3)
df_flyers <- read_html(flyers_page)
Sys.sleep(time = 3)
df_penguins <- read_html(penguins_page)
Sys.sleep(time = 3)
df_sharks <- read_html(sharks_page)
Sys.sleep(time = 3)
df_blues <- read_html(blues_page)
Sys.sleep(time = 3)
df_lightning <- read_html(lightning_page)
Sys.sleep(time = 15)
df_maple_leafs <- read_html(maple_leafs_page)
Sys.sleep(time = 3)
df_canucks <- read_html(canucks_page)
Sys.sleep(time = 3)
df_knights <- read_html(knights_page)
Sys.sleep(time = 3)
df_capitals <- read_html(capitals_page)
Sys.sleep(time = 3)
df_jets <- read_html(jets_page)


## TEAM LINES SETUP
#Create empty data table for inserting
all_players <- data.frame(Player = character(),
                          Position = character(),
                          Line = character(),
                          Team = character(),
                          PP_Line = character()
)

#Create lists for creating team dataframes
df_line_list <- c("Line1","Line1","Line1",
                  "Line2","Line2","Line2",
                  "Line3","Line3","Line3",
                  "Line4","Line4","Line4",
                  "D1","D1",
                  "D2","D2",
                  "D3","D3"
)

df_pos_list <- c("W","C","W",
                 "W","C","W",
                 "W","C","W",
                 "W","C","W",
                 "D","D",
                 "D","D",
                 "D","D"
)

df_pp_line_list <- c("PP1","PP1","PP1","PP1","PP1",
                     "PP2","PP2","PP2","PP2","PP2"
)

#LOOP THRU TEAMS LIST AND INSERT PLAYER DATA TO FINAL TABLE

for (t in teams) {
  
  #Check for Ducks playing tonight
  
  if (t == 'Ducks') {
    
    Sys.sleep(time = 2)
    
    df_players_ducks <- html_text(html_nodes(df_ducks, ".player-name"))
    
    df_players_ducks <- head(df_players_ducks, 28)
    
    df_ev_list_ducks <- head(df_players_ducks, 18)
    
    df_pp_list_ducks <- tail(df_players_ducks, 10)
    
    df_ev_players_ducks <- data.frame(Player = df_ev_list_ducks,
                                      Position = df_pos_list,
                                      Line = df_line_list,
                                      Team = 'Ducks'
    )
    
    df_pp_players_ducks <- data.frame(Player = df_pp_list_ducks,
                                      PP_Line = df_pp_line_list
    )
    
    df_players_ducks <- merge(df_ev_players_ducks, df_pp_players_ducks, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_ducks)
    
  }
  
  if (t == 'Coyotes') {
    
    Sys.sleep(time = 2)
    
    df_players_coyotes <- html_text(html_nodes(df_coyotes, ".player-name"))
    
    df_players_coyotes <- head(df_players_coyotes, 28)
    
    df_ev_list_coyotes <- head(df_players_coyotes, 18)
    
    df_pp_list_coyotes <- tail(df_players_coyotes, 10)
    
    df_ev_players_coyotes <- data.frame(Player = df_ev_list_coyotes,
                                        Position = df_pos_list,
                                        Line = df_line_list,
                                        Team = 'Coyotes'
    )
    
    df_pp_players_coyotes <- data.frame(Player = df_pp_list_coyotes,
                                        PP_Line = df_pp_line_list
    )
    
    df_players_coyotes <- merge(df_ev_players_coyotes, df_pp_players_coyotes, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_coyotes)
    
  }
  
  if (t == 'Bruins') {
    
    Sys.sleep(time = 2)
    
    df_players_bruins <- html_text(html_nodes(df_bruins, ".player-name"))
    
    df_players_bruins <- head(df_players_bruins, 28)
    
    df_ev_list_bruins <- head(df_players_bruins, 18)
    
    df_pp_list_bruins <- tail(df_players_bruins, 10)
    
    df_ev_players_bruins <- data.frame(Player = df_ev_list_bruins,
                                       Position = df_pos_list,
                                       Line = df_line_list,
                                       Team = 'Bruins'
    )
    
    df_pp_players_bruins <- data.frame(Player = df_pp_list_bruins,
                                       PP_Line = df_pp_line_list
    )
    
    df_players_bruins <- merge(df_ev_players_bruins, df_pp_players_bruins, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_bruins)
    
  }
  
  if (t == 'Sabres') {
    
    Sys.sleep(time = 2)
    
    df_players_sabres <- html_text(html_nodes(df_sabres, ".player-name"))
    
    df_players_sabres <- head(df_players_sabres, 28)
    
    df_ev_list_sabres <- head(df_players_sabres, 18)
    
    df_pp_list_sabres <- tail(df_players_sabres, 10)
    
    df_ev_players_sabres <- data.frame(Player = df_ev_list_sabres,
                                       Position = df_pos_list,
                                       Line = df_line_list,
                                       Team = 'Sabres'
    )
    
    df_pp_players_sabres <- data.frame(Player = df_pp_list_sabres,
                                       PP_Line = df_pp_line_list
    )
    
    df_players_sabres <- merge(df_ev_players_sabres, df_pp_players_sabres, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_sabres)
    
  }
  
  
  if (t == 'Hurricanes') {
    
    Sys.sleep(time = 2)
    
    df_players_hurricanes <- html_text(html_nodes(df_hurricanes, ".player-name"))
    
    df_players_hurricanes <- head(df_players_hurricanes, 28)
    
    df_ev_list_hurricanes <- head(df_players_hurricanes, 18)
    
    df_pp_list_hurricanes <- tail(df_players_hurricanes, 10)
    
    df_ev_players_hurricanes <- data.frame(Player = df_ev_list_hurricanes,
                                           Position = df_pos_list,
                                           Line = df_line_list,
                                           Team = 'Hurricanes'
    )
    
    df_pp_players_hurricanes <- data.frame(Player = df_pp_list_hurricanes,
                                           PP_Line = df_pp_line_list
    )
    
    df_players_hurricanes <- merge(df_ev_players_hurricanes, df_pp_players_hurricanes, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_hurricanes)
    
  }
  
  
  if (t == 'Blue Jackets') {
    
    Sys.sleep(time = 2)
    
    df_players_blue_jackets <- html_text(html_nodes(df_blue_jackets, ".player-name"))
    
    df_players_blue_jackets <- head(df_players_blue_jackets, 28)
    
    df_ev_list_blue_jackets <- head(df_players_blue_jackets, 18)
    
    df_pp_list_blue_jackets <- tail(df_players_blue_jackets, 10)
    
    df_ev_players_blue_jackets <- data.frame(Player = df_ev_list_blue_jackets,
                                             Position = df_pos_list,
                                             Line = df_line_list,
                                             Team = 'Blue Jackets'
    )
    
    df_pp_players_blue_jackets <- data.frame(Player = df_pp_list_blue_jackets,
                                             PP_Line = df_pp_line_list
    )
    
    df_players_blue_jackets <- merge(df_ev_players_blue_jackets, df_pp_players_blue_jackets, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_blue_jackets)
    
  }
  
  if (t == 'Flames') {
    
    Sys.sleep(time = 2)
    
    df_players_flames <- html_text(html_nodes(df_flames, ".player-name"))
    
    df_players_flames <- head(df_players_flames, 28)
    
    df_ev_list_flames <- head(df_players_flames, 18)
    
    df_pp_list_flames <- tail(df_players_flames, 10)
    
    df_ev_players_flames <- data.frame(Player = df_ev_list_flames,
                                       Position = df_pos_list,
                                       Line = df_line_list,
                                       Team = 'Flames'
    )
    
    df_pp_players_flames <- data.frame(Player = df_pp_list_flames,
                                       PP_Line = df_pp_line_list
    )
    
    df_players_flames <- merge(df_ev_players_flames, df_pp_players_flames, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_flames)
    
  }
  
  if (t == 'Blackhawks') {
    
    Sys.sleep(time = 2)
    
    df_players_blackhawks <- html_text(html_nodes(df_blackhawks, ".player-name"))
    
    df_players_blackhawks <- head(df_players_blackhawks, 28)
    
    df_ev_list_blackhawks <- head(df_players_blackhawks, 18)
    
    df_pp_list_blackhawks <- tail(df_players_blackhawks, 10)
    
    df_ev_players_blackhawks <- data.frame(Player = df_ev_list_blackhawks,
                                           Position = df_pos_list,
                                           Line = df_line_list,
                                           Team = 'Blackhawks'
    )
    
    df_pp_players_blackhawks <- data.frame(Player = df_pp_list_blackhawks,
                                           PP_Line = df_pp_line_list
    )
    
    df_players_blackhawks <- merge(df_ev_players_blackhawks, df_pp_players_blackhawks, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_blackhawks)
    
  }
  
  if (t == 'Avalanche') {
    
    Sys.sleep(time = 2)
    
    df_players_avalanche <- html_text(html_nodes(df_avalanche, ".player-name"))
    
    df_players_avalanche <- head(df_players_avalanche, 28)
    
    df_ev_list_avalanche <- head(df_players_avalanche, 18)
    
    df_pp_list_avalanche <- tail(df_players_avalanche, 10)
    
    df_ev_players_avalanche <- data.frame(Player = df_ev_list_avalanche,
                                          Position = df_pos_list,
                                          Line = df_line_list,
                                          Team = 'Avalanche'
    )
    
    df_pp_players_avalanche <- data.frame(Player = df_pp_list_avalanche,
                                          PP_Line = df_pp_line_list
    )
    
    df_players_avalanche <- merge(df_ev_players_avalanche, df_pp_players_avalanche, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_avalanche)
    
  }
  
  if (t == 'Stars') {
    
    Sys.sleep(time = 2)
    
    df_players_stars <- html_text(html_nodes(df_stars, ".player-name"))
    
    df_players_stars <- head(df_players_stars, 28)
    
    df_ev_list_stars <- head(df_players_stars, 18)
    
    df_pp_list_stars <- tail(df_players_stars, 10)
    
    df_ev_players_stars <- data.frame(Player = df_ev_list_stars,
                                      Position = df_pos_list,
                                      Line = df_line_list,
                                      Team = 'Stars'
    )
    
    df_pp_players_stars <- data.frame(Player = df_pp_list_stars,
                                      PP_Line = df_pp_line_list
    )
    
    df_players_stars <- merge(df_ev_players_stars, df_pp_players_stars, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_stars)
    
  }
  
  if (t == 'Red Wings') {
    
    Sys.sleep(time = 2)
    
    df_players_red_wings <- html_text(html_nodes(df_red_wings, ".player-name"))
    
    df_players_red_wings <- head(df_players_red_wings, 28)
    
    df_ev_list_red_wings <- head(df_players_red_wings, 18)
    
    df_pp_list_red_wings <- tail(df_players_red_wings, 10)
    
    df_ev_players_red_wings <- data.frame(Player = df_ev_list_red_wings,
                                          Position = df_pos_list,
                                          Line = df_line_list,
                                          Team = 'Red Wings'
    )
    
    df_pp_players_red_wings <- data.frame(Player = df_pp_list_red_wings,
                                          PP_Line = df_pp_line_list
    )
    
    df_players_red_wings <- merge(df_ev_players_red_wings, df_pp_players_red_wings, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_red_wings)
    
  }
  
  if (t == 'Oilers') {
    
    Sys.sleep(time = 2)
    
    df_players_oilers <- html_text(html_nodes(df_oilers, ".player-name"))
    
    df_players_oilers <- head(df_players_oilers, 28)
    
    df_ev_list_oilers <- head(df_players_oilers, 18)
    
    df_pp_list_oilers <- tail(df_players_oilers, 10)
    
    df_ev_players_oilers <- data.frame(Player = df_ev_list_oilers,
                                       Position = df_pos_list,
                                       Line = df_line_list,
                                       Team = 'Oilers'
    )
    
    df_pp_players_oilers <- data.frame(Player = df_pp_list_oilers,
                                       PP_Line = df_pp_line_list
    )
    
    df_players_oilers <- merge(df_ev_players_oilers, df_pp_players_oilers, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_oilers)
    
  }
  
  if (t == 'Panthers') {
    
    Sys.sleep(time = 2)
    
    df_players_panthers <- html_text(html_nodes(df_panthers, ".player-name"))
    
    df_players_panthers <- head(df_players_panthers, 28)
    
    df_ev_list_panthers <- head(df_players_panthers, 18)
    
    df_pp_list_panthers <- tail(df_players_panthers, 10)
    
    df_ev_players_panthers <- data.frame(Player = df_ev_list_panthers,
                                         Position = df_pos_list,
                                         Line = df_line_list,
                                         Team = 'Panthers'
    )
    
    df_pp_players_panthers <- data.frame(Player = df_pp_list_panthers,
                                         PP_Line = df_pp_line_list
    )
    
    df_players_panthers <- merge(df_ev_players_panthers, df_pp_players_panthers, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_panthers)
    
  }
  
  if (t == 'Kings') {
    
    Sys.sleep(time = 2)
    
    df_players_kings <- html_text(html_nodes(df_kings, ".player-name"))
    
    df_players_kings <- head(df_players_kings, 28)
    
    df_ev_list_kings <- head(df_players_kings, 18)
    
    df_pp_list_kings <- tail(df_players_kings, 10)
    
    df_ev_players_kings <- data.frame(Player = df_ev_list_kings,
                                      Position = df_pos_list,
                                      Line = df_line_list,
                                      Team = 'Kings'
    )
    
    df_pp_players_kings <- data.frame(Player = df_pp_list_kings,
                                      PP_Line = df_pp_line_list
    )
    
    df_players_kings <- merge(df_ev_players_kings, df_pp_players_kings, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_kings)
    
  }
  
  if (t == 'Wild') {
    
    Sys.sleep(time = 2)
    
    df_players_wild <- html_text(html_nodes(df_wild, ".player-name"))
    
    df_players_wild <- head(df_players_wild, 28)
    
    df_ev_list_wild <- head(df_players_wild, 18)
    
    df_pp_list_wild <- tail(df_players_wild, 10)
    
    df_ev_players_wild <- data.frame(Player = df_ev_list_wild,
                                     Position = df_pos_list,
                                     Line = df_line_list,
                                     Team = 'Wild'
    )
    
    df_pp_players_wild <- data.frame(Player = df_pp_list_wild,
                                     PP_Line = df_pp_line_list
    )
    
    df_players_wild <- merge(df_ev_players_wild, df_pp_players_wild, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_wild)
    
  }
  
  if (t == 'Canadiens') {
    
    Sys.sleep(time = 2)
    
    df_players_canadiens <- html_text(html_nodes(df_canadiens, ".player-name"))
    
    df_players_canadiens <- head(df_players_canadiens, 28)
    
    df_ev_list_canadiens <- head(df_players_canadiens, 18)
    
    df_pp_list_canadiens <- tail(df_players_canadiens, 10)
    
    df_ev_players_canadiens <- data.frame(Player = df_ev_list_canadiens,
                                          Position = df_pos_list,
                                          Line = df_line_list,
                                          Team = 'Canadiens'
    )
    
    df_pp_players_canadiens <- data.frame(Player = df_pp_list_canadiens,
                                          PP_Line = df_pp_line_list
    )
    
    df_players_canadiens <- merge(df_ev_players_canadiens, df_pp_players_canadiens, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_canadiens)
    
  }
  
  if (t == 'Predators') {
    
    Sys.sleep(time = 2)
    
    df_players_predators <- html_text(html_nodes(df_predators, ".player-name"))
    
    df_players_predators <- head(df_players_predators, 28)
    
    df_ev_list_predators <- head(df_players_predators, 18)
    
    df_pp_list_predators <- tail(df_players_predators, 10)
    
    df_ev_players_predators <- data.frame(Player = df_ev_list_predators,
                                          Position = df_pos_list,
                                          Line = df_line_list,
                                          Team = 'Predators'
    )
    
    df_pp_players_predators <- data.frame(Player = df_pp_list_predators,
                                          PP_Line = df_pp_line_list
    )
    
    df_players_predators <- merge(df_ev_players_predators, df_pp_players_predators, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_predators)
    
  }
  
  if (t == 'Devils') {
    
    Sys.sleep(time = 2)
    
    df_players_devils <- html_text(html_nodes(df_devils, ".player-name"))
    
    df_players_devils <- head(df_players_devils, 28)
    
    df_ev_list_devils <- head(df_players_devils, 18)
    
    df_pp_list_devils <- tail(df_players_devils, 10)
    
    df_ev_players_devils <- data.frame(Player = df_ev_list_devils,
                                       Position = df_pos_list,
                                       Line = df_line_list,
                                       Team = 'Devils'
    )
    
    df_pp_players_devils <- data.frame(Player = df_pp_list_devils,
                                       PP_Line = df_pp_line_list
    )
    
    df_players_devils <- merge(df_ev_players_devils, df_pp_players_devils, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_devils)
    
  }
  
  if (t == 'Islanders') {
    
    Sys.sleep(time = 2)
    
    df_players_islanders <- html_text(html_nodes(df_islanders, ".player-name"))
    
    df_players_islanders <- head(df_players_islanders, 28)
    
    df_ev_list_islanders <- head(df_players_islanders, 18)
    
    df_pp_list_islanders <- tail(df_players_islanders, 10)
    
    df_ev_players_islanders <- data.frame(Player = df_ev_list_islanders,
                                          Position = df_pos_list,
                                          Line = df_line_list,
                                          Team = 'Islanders'
    )
    
    df_pp_players_islanders <- data.frame(Player = df_pp_list_islanders,
                                          PP_Line = df_pp_line_list
    )
    
    df_players_islanders <- merge(df_ev_players_islanders, df_pp_players_islanders, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_islanders)
    
  }
  
  if (t == 'Rangers') {
    
    Sys.sleep(time = 2)
    
    df_players_rangers <- html_text(html_nodes(df_rangers, ".player-name"))
    
    df_players_rangers <- head(df_players_rangers, 28)
    
    df_ev_list_rangers <- head(df_players_rangers, 18)
    
    df_pp_list_rangers <- tail(df_players_rangers, 10)
    
    df_ev_players_rangers <- data.frame(Player = df_ev_list_rangers,
                                        Position = df_pos_list,
                                        Line = df_line_list,
                                        Team = 'Rangers'
    )
    
    df_pp_players_rangers <- data.frame(Player = df_pp_list_rangers,
                                        PP_Line = df_pp_line_list
    )
    
    df_players_rangers <- merge(df_ev_players_rangers, df_pp_players_rangers, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_rangers)
    
  }
  
  if (t == 'Senators') {
    
    Sys.sleep(time = 2)
    
    df_players_senators <- html_text(html_nodes(df_senators, ".player-name"))
    
    df_players_senators <- head(df_players_senators, 28)
    
    df_ev_list_senators <- head(df_players_senators, 18)
    
    df_pp_list_senators <- tail(df_players_senators, 10)
    
    df_ev_players_senators <- data.frame(Player = df_ev_list_senators,
                                         Position = df_pos_list,
                                         Line = df_line_list,
                                         Team = 'Senators'
    )
    
    df_pp_players_senators <- data.frame(Player = df_pp_list_senators,
                                         PP_Line = df_pp_line_list
    )
    
    df_players_senators <- merge(df_ev_players_senators, df_pp_players_senators, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_senators)
    
  }
  
  if (t == 'Flyers') {
    
    Sys.sleep(time = 2)
    
    df_players_flyers <- html_text(html_nodes(df_flyers, ".player-name"))
    
    df_players_flyers <- head(df_players_flyers, 28)
    
    df_ev_list_flyers <- head(df_players_flyers, 18)
    
    df_pp_list_flyers <- tail(df_players_flyers, 10)
    
    df_ev_players_flyers <- data.frame(Player = df_ev_list_flyers,
                                       Position = df_pos_list,
                                       Line = df_line_list,
                                       Team = 'Flyers'
    )
    
    df_pp_players_flyers <- data.frame(Player = df_pp_list_flyers,
                                       PP_Line = df_pp_line_list
    )
    
    df_players_flyers <- merge(df_ev_players_flyers, df_pp_players_flyers, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_flyers)
    
  }
  
  if (t == 'Penguins') {
    
    Sys.sleep(time = 2)
    
    df_players_penguins <- html_text(html_nodes(df_penguins, ".player-name"))
    
    df_players_penguins <- head(df_players_penguins, 28)
    
    df_ev_list_penguins <- head(df_players_penguins, 18)
    
    df_pp_list_penguins <- tail(df_players_penguins, 10)
    
    df_ev_players_penguins <- data.frame(Player = df_ev_list_penguins,
                                         Position = df_pos_list,
                                         Line = df_line_list,
                                         Team = 'Penguins'
    )
    
    df_pp_players_penguins <- data.frame(Player = df_pp_list_penguins,
                                         PP_Line = df_pp_line_list
    )
    
    df_players_penguins <- merge(df_ev_players_penguins, df_pp_players_penguins, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_penguins)
    
  }
  
  if (t == 'Sharks') {
    
    Sys.sleep(time = 2)
    
    df_players_sharks <- html_text(html_nodes(df_sharks, ".player-name"))
    
    df_players_sharks <- head(df_players_sharks, 28)
    
    df_ev_list_sharks <- head(df_players_sharks, 18)
    
    df_pp_list_sharks <- tail(df_players_sharks, 10)
    
    df_ev_players_sharks <- data.frame(Player = df_ev_list_sharks,
                                       Position = df_pos_list,
                                       Line = df_line_list,
                                       Team = 'Sharks'
    )
    
    df_pp_players_sharks <- data.frame(Player = df_pp_list_sharks,
                                       PP_Line = df_pp_line_list
    )
    
    df_players_sharks <- merge(df_ev_players_sharks, df_pp_players_sharks, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_sharks)
    
  }
  
  if (t == 'Blues') {
    
    Sys.sleep(time = 2)
    
    df_players_blues <- html_text(html_nodes(df_blues, ".player-name"))
    
    df_players_blues <- head(df_players_blues, 28)
    
    df_ev_list_blues <- head(df_players_blues, 18)
    
    df_pp_list_blues <- tail(df_players_blues, 10)
    
    df_ev_players_blues <- data.frame(Player = df_ev_list_blues,
                                      Position = df_pos_list,
                                      Line = df_line_list,
                                      Team = 'Blues'
    )
    
    df_pp_players_blues <- data.frame(Player = df_pp_list_blues,
                                      PP_Line = df_pp_line_list
    )
    
    df_players_blues <- merge(df_ev_players_blues, df_pp_players_blues, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_blues)
    
  }
  
  if (t == 'Lightning') {
    
    Sys.sleep(time = 2)
    
    df_players_lightning <- html_text(html_nodes(df_lightning, ".player-name"))
    
    df_players_lightning <- head(df_players_lightning, 28)
    
    df_ev_list_lightning <- head(df_players_lightning, 18)
    
    df_pp_list_lightning <- tail(df_players_lightning, 10)
    
    df_ev_players_lightning <- data.frame(Player = df_ev_list_lightning,
                                          Position = df_pos_list,
                                          Line = df_line_list,
                                          Team = 'Lightning'
    )
    
    df_pp_players_lightning <- data.frame(Player = df_pp_list_lightning,
                                          PP_Line = df_pp_line_list
    )
    
    df_players_lightning <- merge(df_ev_players_lightning, df_pp_players_lightning, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_lightning)
    
  }
  
  if (t == 'Maple Leafs') {
    
    Sys.sleep(time = 2)
    
    df_players_maple_leafs <- html_text(html_nodes(df_maple_leafs, ".player-name"))
    
    df_players_maple_leafs <- head(df_players_maple_leafs, 28)
    
    df_ev_list_maple_leafs <- head(df_players_maple_leafs, 18)
    
    df_pp_list_maple_leafs <- tail(df_players_maple_leafs, 10)
    
    df_ev_players_maple_leafs <- data.frame(Player = df_ev_list_maple_leafs,
                                            Position = df_pos_list,
                                            Line = df_line_list,
                                            Team = 'Maple Leafs'
    )
    
    df_pp_players_maple_leafs <- data.frame(Player = df_pp_list_maple_leafs,
                                            PP_Line = df_pp_line_list
    )
    
    df_players_maple_leafs <- merge(df_ev_players_maple_leafs, df_pp_players_maple_leafs, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_maple_leafs)
    
  }
  
  if (t == 'Canucks') {
    
    Sys.sleep(time = 2)
    
    df_players_canucks <- html_text(html_nodes(df_canucks, ".player-name"))
    
    df_players_canucks <- head(df_players_canucks, 28)
    
    df_ev_list_canucks <- head(df_players_canucks, 18)
    
    df_pp_list_canucks <- tail(df_players_canucks, 10)
    
    df_ev_players_canucks <- data.frame(Player = df_ev_list_canucks,
                                        Position = df_pos_list,
                                        Line = df_line_list,
                                        Team = 'Canucks'
    )
    
    df_pp_players_canucks <- data.frame(Player = df_pp_list_canucks,
                                        PP_Line = df_pp_line_list
    )
    
    df_players_canucks <- merge(df_ev_players_canucks, df_pp_players_canucks, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_canucks)
    
  }
  
  if (t == 'Golden Knights') {
    
    Sys.sleep(time = 2)
    
    df_players_knights <- html_text(html_nodes(df_knights, ".player-name"))
    
    df_players_knights <- head(df_players_knights, 28)
    
    df_ev_list_knights <- head(df_players_knights, 18)
    
    df_pp_list_knights <- tail(df_players_knights, 10)
    
    df_ev_players_knights <- data.frame(Player = df_ev_list_knights,
                                        Position = df_pos_list,
                                        Line = df_line_list,
                                        Team = 'Golden Knights'
    )
    
    df_pp_players_knights <- data.frame(Player = df_pp_list_knights,
                                        PP_Line = df_pp_line_list
    )
    
    df_players_knights <- merge(df_ev_players_knights, df_pp_players_knights, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_knights)
    
  }
  
  if (t == 'Jets') {
    
    Sys.sleep(time = 2)
    
    df_players_jets <- html_text(html_nodes(df_jets, ".player-name"))
    
    df_players_jets <- head(df_players_jets, 28)
    
    df_ev_list_jets <- head(df_players_jets, 18)
    
    df_pp_list_jets <- tail(df_players_jets, 10)
    
    df_ev_players_jets <- data.frame(Player = df_ev_list_jets,
                                     Position = df_pos_list,
                                     Line = df_line_list,
                                     Team = 'Jets'
    )
    
    df_pp_players_jets <- data.frame(Player = df_pp_list_jets,
                                     PP_Line = df_pp_line_list
    )
    
    df_players_jets <- merge(df_ev_players_jets, df_pp_players_jets, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_jets)
    
  }
  
  if (t == 'Capitals') {
    
    Sys.sleep(time = 2)
    
    df_players_capitals <- html_text(html_nodes(df_capitals, ".player-name"))
    
    df_players_capitals <- head(df_players_capitals, 28)
    
    df_ev_list_capitals <- head(df_players_capitals, 18)
    
    df_pp_list_capitals <- tail(df_players_capitals, 10)
    
    df_ev_players_capitals <- data.frame(Player = df_ev_list_capitals,
                                         Position = df_pos_list,
                                         Line = df_line_list,
                                         Team = 'Capitals'
    )
    
    df_pp_players_capitals <- data.frame(Player = df_pp_list_capitals,
                                         PP_Line = df_pp_line_list
    )
    
    df_players_capitals <- merge(df_ev_players_capitals, df_pp_players_capitals, by = "Player", all.x = TRUE)
    
    all_players <- rbind(all_players, df_players_capitals)
    
  }
  
}


# PLAYER STATS
#Read in most recent version of skater and goalie stats files
agg_skater_statistics <- read.csv(file = "C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Game Stats/skater_statistics.csv",
                                  stringsAsFactors = FALSE)

# Set Date as an actual date type
agg_skater_statistics['Date'] <- lapply(agg_skater_statistics['Date'], as.Date)

# ~~~~ TIME ON ICE MANIPULATION ~~~~~~ #

# ~~ SKATERS ~~ #

# Create vector(s) / column(s) so that I can format TimeOnIceMins properly
TimeOnIceMins_vector <- sapply(agg_skater_statistics["TimeOnIceMins"], strsplit, ":", "[", 2)

TimeOnIceMins_Mins <- vector()
TimeOnIceMins_Secs <- vector()

# Minutes
for (i in 1:length(TimeOnIceMins_vector)) {
  TimeOnIceMins_Mins[i] <- TimeOnIceMins_vector[[i]][1]
}

# Seconds
for (i in 1:length(TimeOnIceMins_vector)) {
  TimeOnIceMins_Secs[i] <- TimeOnIceMins_vector[[i]][2]
}

# Convert Seconds to numeric
TimeOnIceMins_Secs <- unlist(lapply(TimeOnIceMins_Secs, as.numeric))

# Convert Minutes to numeric
TimeOnIceMins_Mins <- unlist(lapply(TimeOnIceMins_Mins, as.numeric))

# Convert both to dataframes
# TimeOnIceMins_Secs <- as.data.frame(TimeOnIceMins_Secs)
# TimeOnIceMins_Mins <- as.data.frame(TimeOnIceMins_Mins)

# Add both as columns in the agg_skater_statistics df
agg_skater_statistics['TimeOnIce_Mins'] <- TimeOnIceMins_Mins
agg_skater_statistics['TimeOnIce_Secs'] <- TimeOnIceMins_Secs

# Convert Seconds column to minutes by dividing by 60
agg_skater_statistics['TimeOnIce_Secs'] <- agg_skater_statistics['TimeOnIce_Secs'] / 60

#Add new column with full TimeOnIce as a number
agg_skater_statistics['TimeOnIce_num'] <- round(agg_skater_statistics['TimeOnIce_Mins'] + agg_skater_statistics['TimeOnIce_Secs'],digits=2)

# Remove unnecessary columns
agg_skater_statistics <- agg_skater_statistics[c("Date",
                                                 "Player",
                                                 "Team",
                                                 "Opponent",
                                                 "Goals",
                                                 "Shots",
                                                 "ShotPerc",
                                                 "Assists",
                                                 "Points",
                                                 "Blocks",
                                                 "TimeOnIce_num",
                                                 "Shifts",
                                                 "PlusMinus",
                                                 "PenaltyMins",
                                                 "EvenStrengthGoals",
                                                 "EvenStrengthAssists",
                                                 "PowerPlayGoals",
                                                 "PowerPlayAssists",
                                                 "ShorthandedGoals",
                                                 "ShorthandedAssists",
                                                 "GameWinningGoals",
                                                 "Hits",
                                                 "FaceoffsWon",
                                                 "FaceoffsLost")]

#Rename TimeOnIce column
colnames(agg_skater_statistics)[colnames(agg_skater_statistics) == 'TimeOnIce_num'] <- 'TimeOnIceMins'

# ~~ DKPts Column
agg_skater_statistics <-  agg_skater_statistics %>%
  mutate(DK_Pts = (as.numeric(Goals) * 8.5) + 
           (as.numeric(Assists) * 5) + 
           (as.numeric(Shots) * 1.5) + 
           (as.numeric(Blocks) * 1.3) +
           (case_when(as.numeric(Shots) >= 5 ~ 3, TRUE ~ 0)) + 
           (case_when(as.numeric(Goals) >= 3 ~ 3, TRUE ~ 0)) +
           (case_when(as.numeric(Blocks) >= 3 ~ 3, TRUE ~ 0)) +
           (case_when((as.numeric(Goals) + as.numeric(Assists)) >= 3 ~ 3, TRUE ~ 0)),
         FaceoffsTaken = FaceoffsWon + FaceoffsLost,
         FaceoffWinPerc = FaceoffsWon / FaceoffsTaken
  )

# Replace new NaNs with 0
agg_skater_statistics[is.na(agg_skater_statistics)] <- 0


# Take the last 15 days for any skater is agg_skater_statistics
skater_ranks <-  agg_skater_statistics %>%
  group_by(Player) %>%
  mutate(my_ranks = order(Date, decreasing=TRUE))
         

skater_ranks <- subset(skater_ranks, my_ranks <= 15)

# Do two different aggregations: one at 15 days (PercDrafted) and one at 7 days (DKPts)
# Create a function to handle
# 1 game in the past function
hr_last_game_fn <- function(x_date, day_number) {
  skater_temp <- skater_ranks[skater_ranks$Date < x_date,]
  
  skater_ranking <-  skater_ranks %>%
    group_by(Player) %>%
    mutate(my_ranks = order(Date, decreasing=TRUE))
  
  skater_x_games <- subset(skater_ranking, my_ranks <= day_number)
  
  skater_x_games <- skater_x_games %>%
    group_by(Player) %>%
    summarise(AvgGoalsPerGame_Lastx = round(mean(as.numeric(Goals)),2),
              AvgAssistsPerGame_Lastx = round(mean(as.numeric(Assists)),2),
              AvgShotsPerGame_Lastx = round(mean(as.numeric(Shots)),2),
              AvgBlocksPerGame_Lastx = round(mean(as.numeric(Blocks)),2),
              AvgTimeOnIcePerGame_Lastx = round(mean(as.numeric(TimeOnIceMins)),2),
              AvgDK_PtsPerGame_Lastx = round(mean(as.numeric(DK_Pts)),2)
    )
  
  skater_x_games["Date"] <- x_date
  
  skater_x_games["Days_Aggregated"] <- day_number
  
  return(skater_x_games)
}

#PercDrafted
skater_perdrafted <- hr_last_game_fn(max(skater_ranks$Date) + 1, 15)

#DKPts
skater_dkpts <- hr_last_game_fn(max(skater_ranks$Date) + 1, 7)


# All DATA IS IN; NOW NEED TO COMBINE AND MANIPULATE TO FIT DESIRED SHAPE FOR PREDICTIONS
# Merge DK_file data and Daily Faceoff Line data
#~~~PLAYER NAME MATCH SECTION~~~~~~~~~~~~~~~~~~~

#SKATERS

#Fuzzy match between DK and Daily Faceoff player names
DK2DF.dist.name <- sapply(dk_file$Name, stringdist, all_players$Player, method = "lcs")

#Create Even Strength dataframe
DK2DF.match.name <- data.frame(Name = colnames(DK2DF.dist.name)[apply(DK2DF.dist.name,1,which.min)], 
                               RG_Position = all_players$Position,
                               Team = all_players$Team,
                               Line = all_players$Line,
                               PP_Line = all_players$PP_Line
)



#Fuzzy match between DK and Hockey-Ref Skater names
DK2HR_DK.dist.name <- sapply(dk_file$Name, stringdist, skater_dkpts$Player, method = "lcs")

#Fuzzy match between DK and Hockey-Ref Skater names
DK2HR_PD.dist.name <- sapply(dk_file$Name, stringdist, skater_perdrafted$Player, method = "lcs")


#***ADD MATCH TO DKSALARIES AND CONTEST RESULTS AGGREGATION DATAFRAMES***
#**NEXT STEPS**


#Create Hockey-Ref Skater dataframe
###############FIGURE OUT BEST COLUMNS TO INCLUDE (WHAT TO USE TO DETERMINE MOMENTUM)###############
DK2HR_DK.match.name <- data.frame(Name = colnames(DK2HR_DK.dist.name)[apply(DK2HR_DK.dist.name,1,which.min)],
                                 MinMatch = apply(DK2HR_DK.dist.name,1,min),
                                 DK_AvgAssistsPerGame = as.numeric(skater_dkpts$AvgAssistsPerGame_Lastx),
                                 DK_AvgShotsPerGame = as.numeric(skater_dkpts$AvgShotsPerGame_Lastx),
                                 DK_AvgBlocksPerGame = as.numeric(skater_dkpts$AvgBlocksPerGame_Lastx),
                                 DK_AvgTimeOnIcePerGame = as.numeric(skater_dkpts$AvgTimeOnIcePerGame_Lastx),
                                 DK_AvgDKPtsPerGame = as.numeric(skater_dkpts$AvgDK_PtsPerGame_Lastx))

#Create Hockey-Ref Skater dataframe
###############FIGURE OUT BEST COLUMNS TO INCLUDE (WHAT TO USE TO DETERMINE MOMENTUM)###############
DK2HR_PD.match.name <- data.frame(Name = colnames(DK2HR_PD.dist.name)[apply(DK2HR_PD.dist.name,1,which.min)],
                                  MinMatch = apply(DK2HR_PD.dist.name,1,min),
                                  PD_AvgAssistsPerGame = as.numeric(skater_perdrafted$AvgAssistsPerGame_Lastx),
                                  PD_AvgShotsPerGame = as.numeric(skater_perdrafted$AvgShotsPerGame_Lastx),
                                  PD_AvgBlocksPerGame = as.numeric(skater_perdrafted$AvgBlocksPerGame_Lastx),
                                  PD_AvgTimeOnIcePerGame = as.numeric(skater_perdrafted$AvgTimeOnIcePerGame_Lastx),
                                  PD_AvgDKPtsPerGame = as.numeric(skater_perdrafted$AvgDK_PtsPerGame_Lastx))



#DELETE ROWS FROM MATCH NAME DF WHERE MinMatch >= 5
DK2HR_DK.match.name <- DK2HR_DK.match.name[DK2HR_DK.match.name["MinMatch"] <= 6,]

#DELETE ROWS FROM MATCH NAME DF WHERE MinMatch >= 5
DK2HR_PD.match.name <- DK2HR_PD.match.name[DK2HR_PD.match.name["MinMatch"] <= 6,]





# PLAYER FILE START




#Merge DK and Daily Faceoff dataframes
player_file <- merge(dk_file, DK2DF.match.name, by = "Name", all.x = TRUE)


#Merge player_file and team implied goal dataframe
player_file <- merge(player_file, team_goals, by = "teamAbbrev")

#Merge player_file and Hockey Reference dataframe
player_file <- merge(player_file, DK2HR_DK.match.name, by = "Name")

#Merge player_file and Hockey Reference dataframe
player_file <- merge(player_file, DK2HR_PD.match.name, by = "Name")

# Add games in slate count
player_file["GamesInSlate"] <- length(unique(player_file$Team)) / 2

#Remove rows where Team is NA
player_file <- player_file[!is.na(player_file$Team),]


# Create two separate new dataframes; 

# Select columns that you want for prediction shape
player_file_dkpts_tmp <- player_file[c("Name",
                             "Salary",
                             "RG_Position",
                             "GamesInSlate",
                             "Line",
                             "PP_Line",
                             "Implied_goals",
                             "DK_AvgAssistsPerGame",
                             "DK_AvgShotsPerGame",
                             "DK_AvgBlocksPerGame",
                             "DK_AvgTimeOnIcePerGame",
                             "DK_AvgDKPtsPerGame")]


#Make list of variable names for which we want to create a one-hot encoding column
nhl_binary_list <- c("Line", "PP_Line", "RG_Position") #Not going to encode Position for now, may add if I think it will help

#Loop thru list to create dummy variables
for (f in nhl_binary_list){
  df_all_dummy = acm.disjonctif(player_file_dkpts_tmp[f])
  player_file_dkpts_tmp[f] = NULL
  player_file_dkpts_tmp = cbind(player_file_dkpts_tmp, df_all_dummy)
}

# Add PP_Line.0 column
player_file_dkpts_tmp["PP_Line.0"] <- 1 - player_file_dkpts_tmp["PP_Line.PP1"] - player_file_dkpts_tmp["PP_Line.PP2"]

# Select only the important columns
player_file_dkpts_tmp <- player_file_dkpts_tmp[c("Name",
                                       "Salary",
                                       "Implied_goals",
                                       "Line.Line1",
                                       "Line.Line2",
                                       "PP_Line.0",
                                       "PP_Line.PP1",
                                       "DK_AvgAssistsPerGame",
                                       "DK_AvgShotsPerGame",
                                       "DK_AvgBlocksPerGame",
                                       "DK_AvgTimeOnIcePerGame")]

# Remove duplicates
player_file_dkpts_tmp <- player_file_dkpts_tmp %>% distinct()

# Define matrix for finding the dot product
player_file_dkpts_matrix <- t(player_file_dkpts_tmp[c("Salary",
                                                      "Implied_goals",
                                                      "Line.Line1",
                                                      "Line.Line2",
                                                      "PP_Line.0",
                                                      "PP_Line.PP1",
                                                      "DK_AvgAssistsPerGame",
                                                      "DK_AvgShotsPerGame",
                                                      "DK_AvgBlocksPerGame",
                                                      "DK_AvgTimeOnIcePerGame")])

# Read in the DKPts model
dkpts_model <- read.csv(file="C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Model Results/dkpts_regression.csv",
                        stringsAsFactors = FALSE
)

# Define dkpts_model vector
dkpts_model_vector <- dkpts_model["Estimate"]
dkpts_model_vector <- dkpts_model_vector[-1,]


# Create new vector from the dot product
dk_pts_prediction_vector <- dkpts_model_vector %*% as.matrix(player_file_dkpts_matrix)
dk_pts_prediction_vector <- as.data.frame((dk_pts_prediction_vector+dkpts_model["Estimate"][1,])^2)
dk_pts_prediction_vector <- gather(dk_pts_prediction_vector)

# Add vector as a column to the tmp table
player_file_dkpts_tmp["dk_prediction"] <- round(dk_pts_prediction_vector$value,2)


# Select only the two columns you need to merge new prediction in to final table
player_file_dkpts_tmp <- player_file_dkpts_tmp[c("Name",
                                                 "dk_prediction")]

# Keep the max prediction value for each individual player entry
player_file_dkpts_tmp <- player_file_dkpts_tmp %>% 
                    group_by(Name) %>%
                    filter(dk_prediction == max(dk_prediction))

# Remove duplicates
player_file_dkpts_tmp <- player_file_dkpts_tmp %>% distinct()

# Select columns that you want for prediction shape
player_file_percdrafted_tmp <- player_file[c("Name",
                                       "Salary",
                                       "RG_Position",
                                       "GamesInSlate",
                                       "Line",
                                       "PP_Line",
                                       "Implied_goals",
                                       "PD_AvgAssistsPerGame",
                                       "PD_AvgShotsPerGame",
                                       "PD_AvgBlocksPerGame",
                                       "PD_AvgTimeOnIcePerGame",
                                       "PD_AvgDKPtsPerGame")]


#Make list of variable names for which we want to create a one-hot encoding column
nhl_binary_list <- c("Line", "PP_Line", "RG_Position") #Not going to encode Position for now, may add if I think it will help

#Loop thru list to create dummy variables
for (f in nhl_binary_list){
  df_all_dummy = acm.disjonctif(player_file_percdrafted_tmp[f])
  player_file_percdrafted_tmp[f] = NULL
  player_file_percdrafted_tmp = cbind(player_file_percdrafted_tmp, df_all_dummy)
}



# Add PP_Line.0 column
player_file_percdrafted_tmp["PP_Line.0"] <- 1 - player_file_percdrafted_tmp["PP_Line.PP1"] - player_file_percdrafted_tmp["PP_Line.PP2"]



# Select only the important columns
player_file_pd_cash_tmp <- player_file_percdrafted_tmp[c("Name",
                                                 "Salary",
                                                 "Implied_goals",
                                                 "GamesInSlate",
                                                 "Line.D1",
                                                 "Line.D2",
                                                 "Line.D3",
                                                 "Line.Line1",
                                                 "Line.Line2",
                                                 "Line.Line3",
                                                 "PP_Line.0",
                                                 "PP_Line.PP1",
                                                 "RG_Position.C",
                                                 "RG_Position.D",
                                                 "PD_AvgAssistsPerGame",
                                                 "PD_AvgShotsPerGame",
                                                 "PD_AvgBlocksPerGame",
                                                 "PD_AvgTimeOnIcePerGame")]

# Remove duplicates
player_file_pd_cash_tmp <- player_file_pd_cash_tmp %>% distinct()

# Define matrix for finding the dot product
player_file_pd_cash_matrix <- t(player_file_pd_cash_tmp[c("Salary",
                                                          "Implied_goals",
                                                          "GamesInSlate",
                                                          "Line.D1",
                                                          "Line.D2",
                                                          "Line.D3",
                                                          "Line.Line1",
                                                          "Line.Line2",
                                                          "Line.Line3",
                                                          "PP_Line.0",
                                                          "PP_Line.PP1",
                                                          "RG_Position.C",
                                                          "RG_Position.D",
                                                          "PD_AvgAssistsPerGame",
                                                          "PD_AvgShotsPerGame",
                                                          "PD_AvgBlocksPerGame",
                                                          "PD_AvgTimeOnIcePerGame")])

# Read in the DKPts model
pd_cash_model <- read.csv(file="C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Model Results/PercDraftedCash_regression.csv",
                        stringsAsFactors = FALSE
)

# Define dkpts_model vector
pd_cash_model_vector <- pd_cash_model["Estimate"]
pd_cash_model_vector <- pd_cash_model_vector[-1,]


# Create new vector from the dot product
pd_cash_prediction_vector <- pd_cash_model_vector %*% player_file_pd_cash_matrix
pd_cash_prediction_vector <- as.data.frame(exp((pd_cash_prediction_vector+pd_cash_model["Estimate"][1,])))
pd_cash_prediction_vector <- gather(pd_cash_prediction_vector)

# Add vector as a column to the tmp table
player_file_pd_cash_tmp["pd_cash_prediction"] <- round(pd_cash_prediction_vector$value,3)

# Select only the two columns you need to merge new prediction in to final table
player_file_pd_cash_tmp <- player_file_pd_cash_tmp[c("Name",
                                                 "pd_cash_prediction")]

# Keep the max prediction value for each individual player entry
player_file_pd_cash_tmp <- player_file_pd_cash_tmp %>% 
  group_by(Name) %>%
  filter(pd_cash_prediction == max(pd_cash_prediction))

# Remove duplicates
player_file_pd_cash_tmp <- player_file_pd_cash_tmp %>% distinct()

# PercDrafted_GPP:
# Select only the important columns
player_file_pd_gpp_tmp <- player_file_percdrafted_tmp[c("Name",
                                                         "Salary",
                                                         "Implied_goals",
                                                         "GamesInSlate",
                                                         "Line.D1",
                                                         "Line.D2",
                                                         "Line.D3",
                                                         "Line.Line1",
                                                         "Line.Line2",
                                                         "Line.Line3",
                                                         "PP_Line.0",
                                                         "PP_Line.PP1",
                                                         "RG_Position.D",
                                                         "PD_AvgAssistsPerGame",
                                                         "PD_AvgShotsPerGame",
                                                         "PD_AvgBlocksPerGame",
                                                         "PD_AvgTimeOnIcePerGame")]

# Remove duplicates
player_file_pd_gpp_tmp <- player_file_pd_gpp_tmp %>% distinct()

# Define matrix for finding the dot product
player_file_pd_gpp_matrix <- t(player_file_pd_gpp_tmp[c("Salary",
                                                          "Implied_goals",
                                                          "GamesInSlate",
                                                          "Line.D1",
                                                          "Line.D2",
                                                          "Line.D3",
                                                          "Line.Line1",
                                                          "Line.Line2",
                                                          "Line.Line3",
                                                          "PP_Line.0",
                                                          "PP_Line.PP1",
                                                          "RG_Position.D",
                                                          "PD_AvgAssistsPerGame",
                                                          "PD_AvgShotsPerGame",
                                                          "PD_AvgBlocksPerGame",
                                                          "PD_AvgTimeOnIcePerGame")])

# Read in the DKPts model
pd_gpp_model <- read.csv(file="C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Model Results/PercDraftedGPP_regression.csv",
                          stringsAsFactors = FALSE
)

# Define dkpts_model vector
pd_gpp_model_vector <- pd_gpp_model["Estimate"]
pd_gpp_model_vector <- pd_gpp_model_vector[-1,]


# Create new vector from the dot product
pd_gpp_prediction_vector <- pd_gpp_model_vector %*% player_file_pd_gpp_matrix
pd_gpp_prediction_vector <- as.data.frame(exp((pd_gpp_prediction_vector+pd_gpp_model["Estimate"][1,])))
pd_gpp_prediction_vector <- gather(pd_gpp_prediction_vector)

# Add vector as a column to the tmp table
player_file_pd_gpp_tmp["pd_gpp_prediction"] <- round(pd_gpp_prediction_vector$value,3)

# Select only the two columns you need to merge new prediction in to final table
player_file_pd_gpp_tmp <- player_file_pd_gpp_tmp[c("Name",
                                                     "pd_gpp_prediction")]


# Keep the max prediction value for each individual player entry
player_file_pd_gpp_tmp <- player_file_pd_gpp_tmp %>% 
  group_by(Name) %>%
  filter(pd_gpp_prediction == max(pd_gpp_prediction))

# Remove duplicates
player_file_pd_gpp_tmp <- player_file_pd_gpp_tmp %>% distinct()


# Create final table
# Final Output:
# Player Name, Position, Salary, Team, team implied goals, Line, PPLine, 
#     DKPt proj, Cash % Owned proj, GPP % Owned proj, "Cash Play Rating", "GPP Play Rating"
# Second output with best pairs, three-man plays of the slate for Cash and GPP 
# (need to figure out how to incorporate player correlations?)

# Merge predictions into player_File
player_file <- merge(player_file, player_file_dkpts_tmp, by = "Name")
player_file <- merge(player_file, player_file_pd_cash_tmp, by = "Name")
player_file <- merge(player_file, player_file_pd_gpp_tmp, by = "Name")

# Remove duplicates
player_file <- player_file %>% distinct()


# Select only limited amount of columns to be kept in player_file
player_file <- player_file[c("Name",
                             "RG_Position",
                             "Salary",
                             "Team",
                             "Home_Away",
                             "Implied_goals",
                             "Line",
                             "PP_Line",
                             "DK_AvgTimeOnIcePerGame",
                             "dk_prediction",
                             "pd_cash_prediction",
                             "pd_gpp_prediction")]


# Remove duplicates
player_file <- player_file %>% distinct()


# Order by proper columns
player_file <- player_file %>%
  arrange(Name, desc(Salary), desc(DK_AvgTimeOnIcePerGame))


# Another solution to removing duplicates
player_file <- player_file[!duplicated(player_file$Name),]


# Add player value
player_file['dk_pred_value'] <- sqrt((player_file$dk_prediction / player_file$Salary)) * 100

# Rankings such that we can develop a "cash play" rating system
# Take the last 15 days for any skater is agg_skater_statistics
player_file <-  player_file %>%
  mutate(cash_perc_rank = rank(-(as.numeric(player_file$pd_cash_prediction)), ties.method= "random"),
         value_rank = rank(-(as.numeric(player_file$dk_pred_value)), ties.method= "random"))

# Create separate table that does the same rankings as above, but segmented by Position
player_file_pos_ranks <- player_file %>%
  group_by(RG_Position) %>%
  mutate(cash_perc_rank = rank(-pd_cash_prediction, ties.method= "random"),
         raw_point_rank = rank(-dk_prediction, ties.method= "random"),
         value_rank = rank(-dk_pred_value, ties.method= "random"),
         cash_play_sum = (0.4*cash_perc_rank) + value_rank,
         cash_play_pos_rank = rank(cash_play_sum, ties.method= "random")) %>%
  select(Name, RG_Position, cash_play_pos_rank)


# Merge the by position rankings back into the table
player_file <- merge(player_file, player_file_pos_ranks, by=c("Name","RG_Position"))



#Read in aggregate "player_file" dataset, add tonight's data, and re-write to csv
player_file_write <- player_file %>%
                      mutate(Date = Sys.Date()) %>%
                      select(Date, Name, RG_Position, Team, Home_Away, Implied_goals, Line, PP_Line)


player_file_all <- read.csv(file = 'C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Game Stats/Nightly Player Data/player_file_all.csv',
                    header=TRUE,
                    stringsAsFactors = FALSE)

player_file_all <- rbind(player_file_write, player_file_all)


write.csv(player_file_all,
          file = 'C:/Users/jasselin/DataCamp_Projects_Jupyter_Notebooks/NHLData/2 - Data/Game Stats/Nightly Player Data/player_file_all.csv',
          row.names = FALSE
)



# Adjust the cash_play_sum and cash_play_rank data points to incorporate those
player_file_final <- player_file  %>%
  mutate(cash_play_sum = cash_perc_rank + value_rank + cash_play_pos_rank,
         cash_play_rank = rank(cash_play_sum, ties.method= "random")) %>%
  select(Name, RG_Position, Salary, Team, Home_Away, Implied_goals, Line, PP_Line, dk_prediction,
         pd_cash_prediction, pd_gpp_prediction, dk_pred_value, cash_perc_rank, value_rank, cash_play_pos_rank, cash_play_sum, cash_play_rank) %>%
  arrange(cash_play_rank)


# Where Salary <= x
#player_file_final <- player_file_final[player_file_final["Salary"] <= 5000,]

# Where implied goals >= x
#player_file_final <- player_file_final[player_file_final["Implied_goals"] >= 2.8,]








# Automated lineup construction

# Create subset of player_file_final with only the top 25 ranked Cs, top 30 ranked Ws, and top 25 ranked Ds 
lineup_file <- player_file_final[player_file_final['RG_Position'] == 'C' & player_file_final['cash_perc_rank'] <= 75,]
lineup_file <- rbind(lineup_file, player_file_final[player_file_final['RG_Position'] == 'W' & player_file_final['cash_perc_rank'] <= 75,])
lineup_file <- rbind(lineup_file, player_file_final[player_file_final['RG_Position'] == 'D' & player_file_final['cash_perc_rank'] <= 75,])
lineup_file <- lineup_file[lineup_file['Salary'] > 3000,]



#create one df that is position dummy variables
dummy_positions <- matrix(ifelse(stri_detect(lineup_file$RG_Position, fixed="C"),1,0))
dummy_positions <- cbind(dummy_positions,ifelse(stri_detect(lineup_file$RG_Position, fixed="W"),1,0))
dummy_positions <- cbind(dummy_positions,ifelse(stri_detect(lineup_file$RG_Position, fixed="D"),1,0))


#cbind with salary info and player projected points
dummy_positions<-data.frame(dummy_positions,lineup_file$Salary,lineup_file$cash_perc_rank,lineup_file$Salary)
cnames <- c("c","w","d","salary","rank","salary1")
colnames(dummy_positions)<-cnames

#transpose matrix to allow for inputting rhs constraints
dummy_positions <- t(dummy_positions)

#Get dummy position columns and add a row of 0s
dummy_row_count <- ncol(dummy_positions)
dummy_addl_row <- rep(0,dummy_row_count)
dummy_positions<-rbind(dummy_positions,dummy_addl_row)


x<-7
num_lineups <- 50
lineups <- matrix(0, nrow=num_lineups, ncol=11)
l <- 1
count<-1
exposed_players_list<-"dumb"


#run optimization model for number of lineups
while(l<=num_lineups){
  optim_file <- lineup_file
  rhs <- c(2,3,2,37500,x,32500,5)
  dir <- c("==","==","==","<=","<=",">=","<=")
  a<-10 
  dummy_positions <- dummy_positions[1:7,]
  sol<- Rsymphony_solve_LP(obj=dummy_positions[5,],
                           mat=dummy_positions[1:7,],
                           dir = dir,
                           rhs=rhs,
                           types = "B",
                           max = TRUE)
  
  optim_file$selected <-sol$solution
  optim_file_selected2 <- sol$solution
  optim_file <- optim_file[optim_file$selected==1,]
  x <- as.numeric(sum(optim_file$dk_prediction))
  #count<-1
  print(x)
  
  optim_file$RG_Position  <- factor(optim_file$RG_Position , levels = c("C","W","D"))
  optim_file_selected <- optim_file[order(optim_file$RG_Position),]
  dummy_positions[7,]<- optim_file_selected2
  
  lineup_row <- cbind(matrix(data=optim_file$Name, nrow = 1, ncol = 7),"",sum(optim_file$dk_prediction),"",sum(optim_file$Salary))
  lineups[l,]<- lineup_row
  l <- l+1
  count<-count+1
  print(l)
  
  #set limit for exposure
  player_count <- as.data.frame(table(lineups[,1:7]))
  
  exposure <- .25*num_lineups
  
  player_exposed_index1<-which((player_count$Freq)>=exposure)
  
  exposed_players <- as.character(player_count[c(player_exposed_index1),1])
  
  new_exposed_players <- exposed_players[!exposed_players %in% exposed_players_list]
  
  exposed_players_list<-c(new_exposed_players,exposed_players_list)
  dummy_projection_index<-which(lineup_file$Name %in% new_exposed_players)
  
  print(dummy_projection_index)
  
  dummy_positions[,dummy_projection_index]<-0
  
  #Skip lineups with duplicated players or that cannot find a solution or that have pitcher batter matchups    
  lsub<-l-1
  if(any(duplicated(lineups[lsub,1:7])) | sol$status==226){
    l<-l-1
  } 
  
  #Move to next projection if there are too many repeated lineups. Reformat the objective,rhs,dir, when it fails to remove repeated lineup filtering
  if(sol$status==226){
  }
  print(count)
  #Move to next projection if there are too many repeated lineups. Reformat the objective,rhs,dir, when it fails to remove repeated lineup filtering
  if(count==50){
    #count<-1
    l<-l-1
  }
  
  x<-x-.01
}


lineups_final <- data.frame("C1" = as.character(lineups[,1]),
                            "C2" = as.character(lineups[,2]),
                            "W1" = as.character(lineups[,3]),
                            "W2" = as.character(lineups[,4]),
                            "W3" = as.character(lineups[,5]),
                            "D1" = as.character(lineups[,6]),
                            "D2" = as.character(lineups[,7]),
                            "Prediction" = as.numeric(lineups[,9]),
                            "Salary" = as.numeric(lineups[,11]))


lineups_final <- lineups_final %>%
     filter(Salary <= 37500) %>%
    distinct()

