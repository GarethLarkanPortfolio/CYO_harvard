### Data set-up
# Load required packages
if(!require(tidyverse))
  install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if (!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
  install.packages('e1071', dependencies=TRUE)
if (!require(data.table))
  install.packages("data.table", repos = "http://cran.us.r-project.org")
if (!require(formattable))
  install.packages("formattable", repos = "http://cran.us.r-project.org")

# Read the datafiles
# Read the .csv datafiles into R
deliveries <- read.csv("deliveries.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)
matches <- read.csv("matches.csv", header = TRUE, sep = ",", stringsAsFactors = FALSE)

###################################################################################################################

### Section 1.1 ###

# inspect variable names in both datasets
names(matches)
names(deliveries)

# Inspect variables in "deliveries" datset for spelling mistakes, errors and duplicates
levels(as.factor(deliveries$batting_team))
levels(as.factor(deliveries$bowling_team))

#Correct errors
deliveries <- deliveries %>%
  mutate(batting_team = str_replace(batting_team, "Delhi Daredevils", "Delhi Capitals")
  )
deliveries <- deliveries %>%
  mutate(batting_team = str_replace(
    batting_team,
    "Rising Pune Supergiants",
    "Rising Pune Supergiant"
  )
  )

deliveries <- deliveries %>%
  mutate(bowling_team = str_replace(bowling_team, "Delhi Daredevils", "Delhi Capitals")
  )
deliveries <- deliveries %>%
  mutate(bowling_team = str_replace(
    bowling_team,
    "Rising Pune Supergiants",
    "Rising Pune Supergiant"
  )
  )

# Inspect variables in "matches" datset for spelling mistakes, errors and duplicates
levels(as.factor(matches$team1))
levels(as.factor(matches$team2))
levels(as.factor(matches$winner))
levels(as.factor(matches$toss_winner))
levels(as.factor(matches$venue))

# Preprocess data - correct errors in team names in "matches" dataset
matches <- matches %>%
  mutate(team1 = str_replace(
    team1, "Delhi Daredevils", "Delhi Capitals"
  ))
matches <- matches %>%
  mutate(team1 = str_replace(team1, "Rising Pune Supergiants", "Rising Pune Supergiant")
  )
matches <- matches %>%
  mutate(team2 = str_replace(
    team2, "Delhi Daredevils", "Delhi Capitals"
  ))
matches <- matches %>%
  mutate(team2 = str_replace(team2, "Rising Pune Supergiants", "Rising Pune Supergiant")
  )
matches <- matches %>%
  mutate(toss_winner = str_replace(
    toss_winner, "Delhi Daredevils", "Delhi Capitals"
  ))
matches <- matches %>%
  mutate(toss_winner = str_replace(toss_winner, "Rising Pune Supergiants", "Rising Pune Supergiant")
  )

matches <- matches %>%
  mutate(winner = str_replace(
    winner, "Delhi Daredevils", "Delhi Capitals"
  ))
matches <- matches %>%
  mutate(winner = str_replace(winner, "Rising Pune Supergiants", "Rising Pune Supergiant")
  )

matches <- matches %>%
  mutate(venue = str_replace(venue, "Feroz Shah Kotla Ground", "Feroz Shah Kotla")
  )

matches <- matches %>%
  mutate(venue = str_replace(venue, "M Chinnaswamy Stadium", "M. Chinnaswamy Stadium")
  )

matches <- matches %>%
  mutate(venue = str_replace(venue, "MA Chidambaram Stadium, Chepauk", "M. A. Chidambaram Stadium")
  )

matches <- matches %>%
  mutate(venue = str_replace(venue, "Punjab Cricket Association IS Bindra Stadium, Mohali",
                             "Punjab Cricket Association Stadium, Mohali")
  )

matches <- matches %>%
  mutate(venue = str_replace(venue, "Rajiv Gandhi Intl. Cricket Stadium", 
                             "Rajiv Gandhi International Stadium, Uppal")
  )

#####################################################################################################################

### Section 1.2 ###

# Check for NAs and missing values
sum(is.na(deliveries)) == 0
sum(is.na(matches)) == 0

#####################################################################################################################

### Section 2.1 ###

# Create primary datasets
matches_Data <- matches %>%
  select(
    match_id = id,
    season,
    city,
    team1,
    team2,
    toss_winner,
    toss_dec = toss_decision,
    winner,
    pom = player_of_match,
    venue
  ) 

deliveries_Data <- deliveries %>%
  select(
    inning,
    match_id,
    over,
    ball,
    batsman,
    bowler,
    runs = batsman_runs,
    bat_team = batting_team,
    bowl_team = bowling_team,
    total_runs,
    dismissal_kind
  ) %>%
  gather(role, player, batsman:bowler) %>%
  mutate(role=as.factor(role))

#####################################################################################################################

### Methods & Analysis - Data Exploration & Results:          

### Section 2.2 ###

# Total players
n_distinct(deliveries_Data$player)

# Unique teams
n_distinct(c(unique(matches_Data$team1),unique(mat_ds$team2)))

# Total venues
n_distinct(matches_Data$venue)

### Section 2.3 ###

# Total matches played
total_played <- matches_Data %>%
  summarize(tot_mat_played = n())
total_played

# Total matches without win/ loss result
total_no_results <- matches_Data %>%
  filter(winner == "") %>%
  summarize(tot_noresults = n())
total_no_results

# Major run types scored on each ball and correlation between them
runs_balls <- deliveries_Data %>%
  group_by(match_id, inning) %>%
  mutate(ball_no = 1:n()) %>% 
  ungroup() %>% 
  filter(role == "batsman") %>%
  filter(runs != "" & runs != "5" & runs != "7" & ball_no %in% 1: 120) %>%
  group_by(ball_no, runs) %>% 
  summarize(count=n()) 
runs_balls

runs_balls %>%
  ggplot(aes(ball_no,count, col=factor(ball_no))) +
  geom_col() +
  scale_y_log10() +
  facet_grid( ~ runs) +
  theme(axis.text.x = element_text(
    angle = 110,
    size = 1,
    hjust = 1
  ),
  legend.position = "none")
cor(runs_balls$runs, as.numeric(runs_balls$ball_no))

# Runs scored on each ball in 1st and 2nd innings
deliveries_Data %>%
  group_by(match_id, inning) %>%
  mutate(ball_no = 1:n()) %>% 
  ungroup() %>%
  filter(inning %in% 1:2 & role == "batsman" & runs %in% c(0,1,2,3,4,6) & ball_no %in% 1:120) %>%
  group_by(batting_turn=as.factor(inning), ball_no, batsman_runs=as.factor(runs)) %>%
  summarise (count=n()) %>%
  ggplot(aes(ball_no, count, col=batsman_runs)) +
  geom_point(size=0.5) +
  geom_jitter() +
  theme(legend.position = "top") +
  facet_grid(~ batting_turn)

# Different run types, how they trend during the innings
deliveries_Data %>%
  group_by(match_id, inning) %>%
  mutate(ball_no = 1:n()) %>%
  ungroup() %>%
  filter (inning %in% 1:2 & role == "batsman" & ball_no %in% 1:120) %>%
  group_by(ball_no=as.numeric(ball_no), runs = as.factor(runs)) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  arrange(ball_no) %>%
  top_n(4) %>%
  ggplot(aes(ball_no,count,col=runs, shape = runs)) +
  geom_point(size = 2)

# top_10 players who got maximum player of the match award
matches_Data %>%
  group_by(pom) %>%
  summarize(player_of_match= n()) %>%
  arrange(desc(player_of_match)) %>%
  head(n = 10)

# No.of matches played by each team
played1 <- matches_Data %>%
  group_by(team1) %>%
  summarize(count1 = n()) %>%
  arrange(team1) %>%
  rename(team = team1)

played2 <- matches_Data %>%
  group_by(team2) %>%
  summarize(count2 = n()) %>%
  arrange(team2) %>%
  rename(team = team2)

matches_team <- played1 %>%
  full_join(played2, by = "team") %>%
  mutate(n_matches_played = count1 + count2) %>%
  select(team, n_matches_played) %>% arrange(desc(n_matches_played))
matches_team

# No.of matches played by each player
matches_players <- deliveries_Data %>%
  select(match_id, player) %>%
  group_by(player, match_id) %>% 
  slice(1) %>% 
  ungroup() %>% 
  count(player) %>%
  rename(no_matches_played = n) %>%
  arrange(desc(no_matches_played))
matches_players %>% head(150)

# top_10 batsmen who hit maximum 6s and 4s
deliveries_Data %>%
  filter(role == "batsman" & runs == 6 | runs == 4) %>%
  group_by(player, runs) %>%
  summarize(n = n()) %>%
  spread(runs, n) %>%
  rename(sixes = `6`,fours = `4`) %>%
  select(sixes, fours) %>%
  arrange(desc(sixes)) %>%
  head(10)

# Overall runs scored in all IPL matches
oa_IPL_runs <- deliveries_Data %>%
  filter(role == "batsman") %>%
  summarize(overall_runs = sum(total_runs))
oa_IPL_runs

# Top_100 (~ 20%) batsmen with max runs
batsman_runs <- deliveries_Data %>%
  filter(role == "batsman") %>%
  group_by(player) %>%
  summarize(tot_runs = sum(runs))

t100_bat_runs <- batsman_runs%>%
  arrange(desc(tot_runs)) %>%
  head(n = 100)
t100_bat_runs

# Top_100 (~20%) batsmen contribution to overall runs scored in IPL 
percent(sum(t100_bat_runs$tot_runs)/oa_IPL_runs$overall_runs)

# Overall wicket dismissals in all IPL matches
oa_IPL_wickets <- deliveries_Data %>%
  filter(role == "bowler" & dismissal_kind != "") %>%
  summarize(overall_wickets = n())
oa_IPL_wickets

# Top_100 bowlers with max wickets
bowler_wickets <- deliveries_Data %>%
  filter(
    role == "bowler" &
      dismissal_kind != "obstructing the field" &
      dismissal_kind != "" &
      dismissal_kind != "retired hurt" & dismissal_kind != "run out"
  ) %>%
  group_by(player) %>%
  summarize(tot_wickets = n()) 

t100_bowl_wickets <- bowler_wickets %>%
  arrange(desc(tot_wickets)) %>%
  head(n = 100)
t100_bowl_wickets

# Top_100 (~20%) bowler contribution to overall wicket dismissals in IPL 
percent(sum(t100_bowl_wickets$tot_wickets)/oa_IPL_wickets$overall_wickets)

# top_10 batsmen who faced maximum balls
top_bat_max_balls <- deliveries_Data %>%
  filter(role == "batsman") %>%
  group_by(player) %>%
  summarize(tot_n_balls = n()) %>%
  arrange(desc(tot_n_balls)) 

top_bat_max_balls %>%
  head(n = 20)

# Top 10 bowlers who bowled maximum no. of balls
bowler_balls <- deliveries_Data %>%
  filter(role == "bowler") %>%
  group_by(player) %>%
  summarize(tot_n_balls = n()) %>%
  arrange(desc(tot_n_balls)) 

bowler_balls %>%
  head(n = 10)

# top_10 bowlers who conceded maximum runs
deliveries_Data %>%
  filter(role == "bowler") %>%
  group_by(player) %>%
  summarize(tot_runs = sum(runs), balls_bowled=n()) %>%
  arrange(desc(tot_runs)) %>%
  head(n = 10)

# top_10 batsmen with max strike_rate
deliveries_Data %>%
  filter(role == "batsman") %>%
  group_by(player) %>%
  summarize(runs_scored = sum(runs), balls_batted=n(), strike_rate = sum(runs) / n()) %>%
  arrange(desc(strike_rate)) %>%
  head(n = 10)

# Top 10 bowlers with best economy rate
deliveries_Data %>%
  filter(role == "bowler") %>%
  group_by(player) %>%
  summarize(runs_given = sum(runs), balls_bowled=n(), economy_rate = sum(runs) / n()) %>%
  arrange(economy_rate) %>%
  head(n = 10)

# Top 10 bowler with the best economy rate bowling over 1000 balls
data <- deliveries_Data %>%
  filter(role == "bowler") %>%
  group_by(player) %>%
  summarize(runs_given = sum(runs), balls_bowled=n(), economy_rate = sum(runs) / n()) %>%
  arrange(economy_rate) %>%
  head(n = 10)

data %>% filter(balls_bowled > 2500)

# top_10 bowlers with best wicket strike rates
top_bowl_max_balls %>%
  full_join(t_bowl_wickets, by = "player") %>%
  mutate(strike_rate = tot_n_balls/ tot_wickets) %>%
  arrange(strike_rate) %>%
  head(10)

# top_10 bowlers who conceded minimum runs
deliveries_Data %>%
  filter(role == "bowler") %>%
  group_by(player) %>%
  summarize(tot_runs = sum(runs),balls_bowled=n()) %>%
  arrange(tot_runs) %>%
  head(n = 10)

# Total runs scored by each team
deliveries %>%
  select(batting_team, total_runs) %>%
  group_by(team = batting_team) %>%
  summarize(tot_runs = sum(total_runs)) %>%
  arrange(desc(tot_runs))

# Which team won how many matches
winners <- matches_Data %>%
  group_by(winner) %>%
  filter(winner != "") %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  rename(team = winner, n_matches_won = count)
winners

# Which team lost how many matches
lost1 <- matches_Data %>%
  filter(winner != "") %>%
  filter(as.character(winner) != as.character(team1)) %>%
  group_by(team1) %>%
  summarize(count1 = n()) %>%
  arrange(team1) %>%
  rename(team = team1)

lost2 <- matches_Data %>%
  filter(winner != "") %>%
  filter(as.character(winner) != as.character(team2)) %>%
  group_by(team2) %>%
  summarize(count2 = n()) %>%
  arrange(team2) %>%
  rename(team = team2)

losers <- lost1 %>%
  full_join(lost2, by = "team") %>%
  mutate(n_matches_lost = count1 + count2) %>%
  select(-count1,-count2) %>%
  arrange(desc(n_matches_lost))
losers

# Won/ lost plots
# Which team won how many tosses
toss_winners <- matches_Data %>%
  group_by(toss_winner) %>%
  summarize(count = n()) %>%
  arrange(desc(count)) %>%
  rename(team = toss_winner, n_tosses_won = count)
toss_winners

teams <- matches_team %>%
  right_join(winners, by = "team") %>%
  right_join(losers, by = "team") %>%
  right_join(toss_winners, by = "team")

# correlation between match-wins and matches played
teams %>%
  ggplot(aes(n_matches_played, n_matches_won, fill = team)) +
  geom_point(shape=21, size=3)

cor(teams$n_matches_played, teams$n_matches_won)

# Correlation between matches played and toss-wins
teams %>%
  ggplot(aes(n_matches_played, n_tosses_won, fill = team)) +
  geom_point(shape=25, size = 3)

cor(teams$n_matches_played, teams$n_tosses_won)

# Correlation between match-wins and toss-wins
teams %>%
  ggplot(aes(n_tosses_won, n_matches_won, fill = team)) +
  geom_point(shape=24, size = 3)

cor(teams$n_matches_won, teams$n_tosses_won)

# Correlation between win% & matches played
teams %>%
  mutate(matches_won_percent = n_matches_won * 100 / n_matches_played) %>%
  ggplot(aes(matches_won_percent, n_matches_played, fill =
               team)) +
  geom_point(shape=21, size = 3) +
  xlab("% of matches won") +
  coord_flip()

cor(teams$n_matches_won * 100 / teams$n_matches_played,
    teams$n_matches_played)

# Toss wins by different teams
toss_wins <- matches_Data %>%
  filter(winner != "") %>%
  group_by(toss_winner) %>%
  summarize(t_wins = n())
toss_wins

# Toss wins Vs match wins or losses based on batted/ fielded first
toss_wins_results <- matches_Data %>%
  filter(winner != "") %>%
  mutate(match_result = ifelse(toss_winner==winner, "win", "loss")) %>%
  group_by(toss_winner, toss_dec, match_result) %>%
  summarize(m_r_count = n()) 

toss_wins_results_prcnts <- toss_wins_results %>% 
  spread(match_result, m_r_count) %>%
  full_join(toss_wins, by = "toss_winner") %>%
  mutate(win_prcnt=win*100/(t_wins),
         loss_prcnt= loss*100/(t_wins)) %>%
  select(toss_winner, t_wins, toss_dec, win, win_prcnt, loss, loss_prcnt)

toss_wins_results_prcnts

toss_wins_results %>% 
  ggplot(aes(toss_winner, m_r_count, col = toss_dec, size = match_result)) +
  geom_point()  +
  theme(axis.text.x = element_text(
    angle = 90,
    size = 6,
    hjust = 1
  ),
  legend.position = "right") +
  labs(subtitle="Comparison of toss-decision to win/loss")

# Toss losses by different teams
toss_losses <- matches_Data %>%
  filter(winner != "") %>%
  mutate(toss_loser = ifelse(toss_winner == team1, team2, team1)) %>%
  group_by(toss_loser) %>%
  summarize(t_losses = n())
toss_losses

# Toss losses Vs match wins or losses based on batted/ fielded first
toss_losses_results <- matches_Data %>%
  filter(winner != "") %>% 
  mutate(toss_loser = ifelse(toss_winner==team1, team2, team1)) %>%
  mutate(match_result = ifelse(toss_loser==winner, "win", "loss")) %>%
  group_by(toss_loser, toss_dec, match_result) %>%
  summarize(m_r_count = n())

toss_losses_results_prcnts <- toss_losses_results %>% 
  spread (match_result, m_r_count) %>%
  full_join(toss_losses, by = "toss_loser") %>%
  mutate(win_prcnt=win*100/(t_losses),
         loss_prcnt= loss*100/(t_losses))%>%
  select(toss_loser, t_losses, toss_dec, win, win_prcnt, loss, loss_prcnt)

toss_losses_results_prcnts

toss_losses_results %>% 
  ggplot(aes(toss_loser, m_r_count, col = toss_dec, size = match_result)) +
  geom_point() +
  theme(axis.text.x = element_text(
    angle = 90,
    size = 6,
    hjust = 1
  ),
  legend.position = "right") +
  labs(subtitle="toss_dec is always by toss_winner")

# No. of times teams selected to bat or field first after winning toss
matches_Data %>%
  group_by(toss_dec) %>%
  summarize(count = n())

# No. of times individual teams selected to bat or field first after winning toss
matches_Data %>%
  group_by(toss_winner, toss_dec) %>%
  summarize(count = n()) %>%
  spread(toss_dec, count) %>%
  arrange(desc(field))

################################################################################################################

### Section 2.4

# Number of matches played over seasons
matches %>%
  group_by(season) %>%
  summarize(matches_played=n()) %>%
  ggplot(aes(season, matches_played)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2007, 2019, by = 1))

# How many matches teams won batting first and batting second
# Total no. of matches won batting first across seasons
matches_Data %>%
  filter(team1==winner) %>%
  summarize(count=n())

# Total no. of matches won batting second across seasons
matches_Data %>%
  filter(team2==winner) %>%
  summarize(count=n())

# No. of matches won batting first season-wise
wins_bat_1st <- matches_Data %>%
  filter(team1==winner) %>%
  group_by(season) %>%
  summarize(wins_bat_1st=n())
wins_bat_1st
sum(wins_bat_1st$wins_bat_1st)

# No. of matches won batting second  season-wise
wins_bat_2nd <- matches_Data %>%
  filter(team2==winner) %>%
  group_by(season) %>%
  summarize(wins_bat_2nd=n())
wins_bat_2nd
sum(wins_bat_2nd$wins_bat_2nd)

wins_bat_1st %>%
  inner_join(wins_bat_2nd, by="season") %>%
  gather(batting_turn, wins, wins_bat_1st:wins_bat_2nd) %>%
  ggplot(aes(season, wins, col=batting_turn)) +
  geom_line() +
  scale_x_continuous(breaks = seq(2007, 2019, by = 1))

###################################################################################################################

### Section 2.5 ###

# Winning margin
deliveries_Data %>%
  filter(role=="batsman" & inning %in% 1:2) %>%
  group_by(inning) %>%
  summarize(total_runs_scored=sum(runs))

deliveries_Data %>%
  group_by(match_id, inning) %>%
  mutate(ball_no = 1:n()) %>% 
  ungroup() %>%
  filter(inning %in% 1:2 & runs %in% c(0,1,2,3,4,5,6,7) & ball_no %in% 1:120) %>%
  group_by(inning, batsman_runs=factor(runs)) %>%
  summarise(count=n()) %>%
  spread(inning,count)

###################################################################################################################

### Section 2.6

# No.of matches played at each venue
matches_venue <- matches_Data %>%
  group_by(venue) %>%
  summarize(n_matches_played = n()) %>%
  arrange(desc(n_matches_played))

matches_venue %>%
  ggplot(aes(venue, n_matches_played, fill = venue)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  geom_text(aes(label = n_matches_played), hjust = -1)

# % of matches played at top_10 venues
t10_n_matches_played <- matches_venue %>%
  head(10)
t10_n_matches_played

percent(sum(t10_n_matches_played$n_matches_played)/total_played)

# Venue effect on team wins
matches_Data %>%
  filter(winner != "") %>%
  group_by(venue, winner) %>%
  summarize(count = n()) %>%
  ggplot(aes(venue, count, group = winner)) +
  geom_point() +
  geom_line(aes(col = winner), size = 1) +
  scale_y_log10() +
  theme(axis.text.x = element_text(
    angle = 90,
    size = 6,
    hjust = 1
  ),
  legend.position = "top", legend.title.align=0)

# Venue effect on team runs
deliveries_Data %>%
  full_join(matches_Data, by ="match_id") %>%
  select(match_id, bat_team, role, total_runs, venue) %>%
  filter(role == "batsman") %>%
  group_by(venue, bat_team) %>%
  summarize(runs_scored = sum(total_runs)) %>%
  ggplot(aes(venue, runs_scored, group = bat_team)) +
  geom_line(aes(col = bat_team), size = 1) +
  scale_y_log10() +
  theme(axis.text.x = element_text(
    angle = 90,
    size = 6,
    hjust = 1
  ),
  legend.position = "top", legend.title.align=0)

####################################################################################################################

### Section 3.1 ###

### Methods & Analysis - Model Building & Results:          

# TOP_RATE_PLAYERS: 
# Order of players with best batting striking rates & bowling economy rates    

# Batsmen average & median number of balls and runs
batsmen_avgs <- deliveries_Data %>%
  filter(role == "batsman") %>%
  group_by(player) %>%
  summarize(tot_balls = n(), tot_runs = sum(runs)) %>%
  summarize (
    avg_balls = mean(tot_balls),
    median_balls = median(tot_balls),
    avg_runs = mean(tot_runs),
    median_runs = median(tot_runs),
    max(tot_runs),
    min(tot_runs),
    max(tot_balls),
    min(tot_balls)
  )

t(as.matrix(batsmen_avgs))

# Bowler average & median number of balls and runs
bowler_avgs <- deliveries_Data %>%
  filter(role == "bowler") %>%
  group_by(player) %>%
  summarize(tot_balls = n(), tot_runs = sum(runs)) %>%
  summarize (
    avg_balls = mean(tot_balls),
    median_balls = median(tot_balls),
    avg_runs = mean(tot_runs),
    median_runs = median(tot_runs),
    max(tot_runs),
    min(tot_runs),
    max(tot_balls),
    min(tot_balls)
  )

t(as.matrix(bowler_avgs))

# Top players with strike rates & economy rates after regularisation using median runs 
# and median balls
str_rates <- deliveries_Data %>%
  filter(role == "batsman") %>%
  group_by(player) %>%
  summarize(reg_str_rate = (sum(runs) + batsmen_avgs$median_runs) / (n() +
                                                                       batsmen_avgs$median_balls)) %>%
  arrange(desc(reg_str_rate))

str_rates %>%
  head(10)

eco_rates <- deliveries_Data %>%
  filter(role == "bowler") %>%
  group_by(player) %>%
  summarize(reg_eco_rate = (sum(runs) + bowler_avgs$median_runs) / 
              (n() + bowler_avgs$median_balls)) %>%
  arrange(reg_eco_rate)

eco_rates %>%
  head(10)

# Top rate players based on strike rates & economy rates
top_rate_players <- str_rates %>%
  full_join(eco_rates, by = "player") %>%
  mutate(reg_str_rate = replace_na(
    reg_str_rate,
    batsmen_avgs$median_runs / batsmen_avgs$avg_balls
  )) %>%
  mutate(reg_eco_rate = replace_na(reg_eco_rate, bowler_avgs$avg_runs /
                                     bowler_avgs$median_balls)) %>%
  mutate(player_value = 100 * (reg_str_rate + 1 / reg_eco_rate)) 

top_rate_players %>%
  arrange(desc(player_value)) %>%
  select(player, player_value) %>%
  mutate(rank = row_number()) %>%
  head(50) %>% 
  knitr::kable()

####################################################################################################################

### Section 3.2 ###

# TOP_CONTRI_PLAYERS: 
# Order of players with best number of highest contributions in won & lost matches    

# Which teams have won which matches and lost which matches
# Which matches which teams have won
won_t1 <- matches_Data %>%
  filter(winner != "") %>%
  filter(as.character(winner) == as.character(team1)) %>%
  select(match_id, team = team1)

won_t2 <- matches_Data %>%
  filter(winner != "") %>%
  filter(as.character(winner) == as.character(team2)) %>%
  select(match_id, team = team2)

won_matches <- won_t1 %>%
  bind_rows(won_t2)

won_matches

# Which matches which teams have lost
lost_t1 <- matches_Data %>%
  filter(winner != "") %>%
  filter(as.character(winner) != as.character(team1)) %>%
  select(match_id, team = team1, winner)

lost_t2 <- matches_Data %>%
  filter(winner != "") %>%
  filter(as.character(winner) != as.character(team2)) %>%
  select(match_id, team = team2, winner)

lost_matches <- lost_t1 %>%
  bind_rows(lost_t2)

# Batsmen score contribution in won matches
# Top scorer for winning sides
batsman_contr_w <- deliveries_Data %>%
  full_join(won_matches, by = "match_id") %>%
  filter(role == "batsman" & bat_team == team) %>%
  group_by(match_id, player) %>%
  summarize(batsman_score = sum(runs)) %>%
  top_n(1, batsman_score) %>%
  full_join(won_matches, by = "match_id")
batsman_contr_w

batsman_contr_w %>% filter(batsman_score > 120)

# Bowler wicket taking contribution in won matches
# Top wicket taker for winning sides
bowler_contr_w <- deliveries_Data %>%
  full_join(won_matches, by = "match_id") %>%
  filter(role=="bowler" & bowl_team == team) %>%
  filter (dismissal_kind %in% c("bowled", "caught", "caught and bowled", "hit wicket",     
                                "lbw", "stumped")) %>%
  select(match_id, team, bowl_team, player, dismissal_kind) %>%
  group_by(match_id, player) %>% 
  summarize(bowler_wckts = n()) %>%
  top_n(1, bowler_wckts) %>%
  full_join(won_matches, by = "match_id")
bowler_contr_w

#Bowling contribution in winning matches but with 5 or more wickets
bowler_contr_w %>% filter(bowler_wckts > 4)

# Top_batsmen on winning sides in the order of highest individual scores
winning_t_scores <- deliveries_Data %>%
  full_join(won_matches, by = "match_id") %>%
  filter(role == "batsman" & bat_team == team) %>%
  group_by(match_id) %>%
  summarize(team_score = sum(total_runs)) %>%
  full_join(batsman_contr_w, by = "match_id") %>%
  arrange(desc(batsman_score))
winning_t_scores

# Top_batsmen on winning sides in terms no.of top_scores 
win_scores <- winning_t_scores %>%
  group_by(player) %>%
  summarize(batsman_count = n()) %>%
  arrange(desc(batsman_count))
win_scores

# Top_bowlers on winning sides in terms no.of maximum wickets 
win_wickets <- bowler_contr_w %>%
  group_by(player) %>%
  summarize(bowler_count = n()) %>%
  arrange(desc(bowler_count))
win_wickets

# Batsmen score contribution in lost matches
# Top scorer for losing sides
batsman_contr_l <- deliveries_Data %>%
  full_join(lost_matches, by = "match_id") %>%
  filter(role == "batsman" & bat_team == team) %>%
  group_by(match_id, player) %>%
  summarize(batsman_score = sum(runs)) %>%
  top_n(1, batsman_score) %>%
  full_join(lost_matches, by = "match_id") %>%
  rename(losing_team=team) 
batsman_contr_l

# Bowler wicket taking contribution in lost matches
# Top wicket taker for losing sides
bowler_contr_l <- deliveries_Data %>%
  full_join(lost_matches, by = "match_id") %>%
  filter(role=="bowler" & bowl_team == team) %>%
  filter(dismissal_kind %in% c("bowled", "caught", "caught and bowled", "hit wicket",
                               "lbw", "stumped")) %>%
  select(match_id, team, bowl_team, player) %>%
  group_by(match_id, player) %>% 
  summarize(bowler_wckts = n()) %>%
  top_n(1, bowler_wckts) %>%
  full_join(lost_matches, by = "match_id") %>%
  rename(losing_team=team) 
bowler_contr_l

# Top_batsmen on losing sides in the order of highest individual scores
losing_t_scores <- deliveries_Data %>%
  full_join(lost_matches, by = "match_id") %>%
  filter(role == "batsman" & bat_team == team) %>%
  group_by(match_id) %>%
  summarize(team_score = sum(total_runs)) %>%
  full_join(batsman_contr_l, by = "match_id") %>%
  arrange(desc(batsman_score))
losing_t_scores

# Top_batsmen on losing sides in terms no.of top_scores 
loss_scores <- losing_t_scores %>%
  group_by(player) %>%
  summarize(batsman_count = n()) %>%
  arrange(desc(batsman_count))
loss_scores

# Top_bowlers on losing sides in terms no.of maximum wickets 
loss_wickets <- bowler_contr_l %>%
  group_by(player) %>%
  summarize(bowler_count = n()) %>%
  arrange(desc(bowler_count))
loss_wickets

# Top batsmen contribution in won matches & lost matches - arranged by contribution in WON matches
top_contri_batsmen <- win_scores %>%
  rename(contribution_in_WON_matches = batsman_count) %>%
  full_join(loss_scores, by = "player") %>%
  rename(contribution_in_LOST_matches = batsman_count) %>%
  arrange(desc(contribution_in_WON_matches))
top_contri_batsmen

# Top batsmen contribution in won matches & lost matches - arranged by contribution in LOST matches
top_contri_batsmen <- win_scores %>%
  rename(contribution_in_WON_matches = batsman_count) %>%
  full_join(loss_scores, by = "player") %>%
  rename(contribution_in_LOST_matches = batsman_count) %>%
  arrange(desc(contribution_in_LOST_matches))
top_contri_batsmen

# Top batsmen overall contribution in won matches & lost matches
top_contri_batsmen <- top_contri_batsmen %>%
  mutate(batsman_contribution = contribution_in_LOST_matches +
           contribution_in_WON_matches) %>%
  select(
    player,
    batsman_contribution,
    contribution_in_LOST_matches,
    contribution_in_WON_matches
  ) %>%
  arrange(desc(batsman_contribution))
top_contri_batsmen

# Top bowlers contribution in won matches and lost matches - arranged by contribution in WON matches
top_contri_bowlers <- win_wickets %>%
  rename(contribution_in_WON_matches = bowler_count) %>%
  full_join(loss_wickets, by = "player") %>%
  rename(contribution_in_LOST_matches = bowler_count) %>%
  arrange(desc(contribution_in_WON_matches))
top_contri_bowlers

# Top bowlers contribution in won matches and lost matches - arranged by contribution in LOST matches
top_contri_bowlers <- win_wickets %>%
  rename(contribution_in_WON_matches = bowler_count) %>%
  full_join(loss_wickets, by = "player") %>%
  rename(contribution_in_LOST_matches = bowler_count) %>%
  arrange(desc(contribution_in_LOST_matches))
top_contri_bowlers

# Top bowlers overall contribution in won matches and lost matches
top_contri_bowlers <- top_contri_bowlers %>%
  mutate(bowler_contribution = contribution_in_LOST_matches +
           contribution_in_WON_matches) %>%
  select(
    player,
    bowler_contribution,
    contribution_in_LOST_matches,
    contribution_in_WON_matches
  ) %>%
  arrange(desc(bowler_contribution))
top_contri_bowlers

# Top_contribution players in won/ lost matches
top_contri_players <- top_contri_batsmen %>%
  full_join(top_contri_bowlers, by="player") 

top_contri_players[is.na(top_contri_players)] <- 0

top_contri_players <- top_contri_players %>%
  mutate(player_contribution = batsman_contribution + bowler_contribution) %>%
  select(player,player_contribution, batsman_contribution, bowler_contribution)
top_contri_players %>% 
  arrange(desc(player_contribution))

# Average and median of top contribution by players
stats_contri <- top_contri_players %>%
  summarize(avg_contri_pp = mean(player_contribution), 
            med_contri_pp = median(player_contribution))

# Average and median of matches played by players
stats_matches <- matches_players %>%
  summarize(avg_mat_played = mean(no_matches_played), 
            med_mat_played = median(no_matches_played))

top_contri_players <- top_contri_players %>%
  full_join(matches_players, by ="player") %>%
  mutate(player_contri_rate=(player_contribution+stats_contri$med_contri_pp)/
           (no_matches_played+stats_matches$avg_mat_played))

top_contri_players %>%
  select(player, player_contribution, player_contri_rate) %>%
  arrange(desc(player_contri_rate)) %>%
  head(50) %>%
  knitr::kable()

####################################################################################################################

### Section 3.3 ###

# TOP_EXCEL_PLAYERS: 
# Order of players with best performance against best bowlers and best batsmen    

# Top_10 strike batsmen and top_10 economy bowlers
top_10 <- str_rates %>%
  head(10)
top_10

ggplot(data = top_10, mapping = aes(x = player, y = reg_str_rate)) + 
  geom_point()

top_10_bowlers <- eco_rates %>%
  head(10)
top_10_bowlers

ggplot(data = top_10_bowlers, mapping = aes(x = player, y = reg_eco_rate)) + 
  geom_point()

# Batsmen strike rate against top_10 bowlers
sr_vs_t20_bowlers <- deliveries %>%
  filter(bowler %in% top_10_bowlers$player) %>%
  group_by(player = batsman) %>%
  summarize(sr_t20 = (sum(batsman_runs) + batsmen_avgs$median_runs) / (n() +          
                                                                         batsmen_avgs$avg_balls)) %>%
  arrange(desc(sr_t20))
sr_vs_t20_bowlers %>%
  head(10)

# Bowlers economy rate against top_10 batsmen
er_vs_t20_batsmen <- deliveries %>%
  filter(batsman %in% top_10$player) %>%
  group_by(player = bowler) %>%
  summarize(er_t20 = (sum(batsman_runs) + bowler_avgs$avg_runs) / (n() +         
                                                                     bowler_avgs$median_balls)) %>%
  arrange(er_t20)
er_vs_t20_batsmen %>%
  head(10)

# Top excellence players
top_excel_players <- er_vs_t20_batsmen %>%
  full_join(sr_vs_t20_bowlers, by = "player") %>%
  mutate(sr_t20 = replace_na(sr_t20, batsmen_avgs$median_runs / batsmen_avgs$avg_balls)) %>%
  mutate(er_t20 = replace_na(er_t20, bowler_avgs$avg_runs / bowler_avgs$median_balls))

# Top 50 Excellence Players
top_excel_players %>%
  select(player, sr_t20, er_t20) %>%
  arrange(desc(sr_t20)) %>%
  head(10) %>% 
  knitr::kable()

####################################################################################################################

### Section 3.4 ###

# TOP_CALIBER_PLAYERS: 
# Order of players based on their summarized player values    

# Players by their calibre - strike rate + economy rate, 
# contribution in win/ loss situation and 
# player's excellence against the best in business

star_players <- top_rate_players %>%
  select(-player_value) %>%
  full_join(top_excel_players, by = "player") %>%
  mutate(sr_t20 = replace_na(sr_t20, batsmen_avgs$median_runs / batsmen_avgs$avg_balls)) %>%
  mutate(er_t20 = replace_na(er_t20, bowler_avgs$avg_runs / bowler_avgs$median_balls)) %>%
  full_join(top_contri_players, by="player") %>% 
  
  mutate(player_value = 100 * ((reg_str_rate + sr_t20) + 1 /
                                 (reg_eco_rate + er_t20) + 
                                 player_contri_rate)) %>%
  select(player) %>%
  mutate(rank = row_number())

star_players %>%
  head(10)


# TOP_10_STAR_PLAYERS: 
# Top 10 star players by player value
top_10_star_players <- star_players %>%
  head(10) 

top_10_star_players%>% 
  knitr::kable()

####################################################################################################################

## 2nd Objective - Building a model with maximum F1 Score to predict the winner of a match:

# Create the list of top 8 teams from matches_team (teams Vs matches played)
teams <- matches_team %>%
  top_n(8) %>% 
  select (team)

teams

# Do required pre-processing and data wrangling
final_data <- matches %>%
  select(first_bat_team = team1,
         second_bat_team = team2, winner, venue, toss_winner, toss_decision) %>%
  filter(winner != "" & first_bat_team %in% teams$team & second_bat_team %in% teams$team) %>%
  mutate_all(funs(str_replace_all(., "Chennai Super Kings", "CSK"))) %>% 
  mutate_all(funs(str_replace_all(., "Delhi Capitals", "DC"))) %>%
  mutate_all(funs(str_replace_all(., "Kings XI Punjab", "KP"))) %>% 
  mutate_all(funs(str_replace_all(., "Kolkata Knight Riders", "KKR"))) %>% 
  mutate_all(funs(str_replace_all(., "Mumbai Indians", "MI"))) %>% 
  mutate_all(funs(str_replace_all(., "Rajasthan Royals", "RR"))) %>% 
  mutate_all(funs(str_replace_all(., "Royal Challengers Bangalore", "RCB"))) %>% 
  mutate_all(funs(str_replace_all(., "Sunrisers Hyderabad", "SRH"))) %>%
  mutate(first_bat_team = as.factor(first_bat_team), second_bat_team = as.factor(second_bat_team), 
         winner = as.factor(winner), venue = as.factor(venue), toss_decision = as.factor(toss_decision),
         toss_winner = as.factor(toss_winner)) 

any(is.na(final_data))
summary(final_data)
dim(final_data)

# test set will be approx 10% of our data set
test_index <- createDataPartition(final_data$winner, times = 1, p = 0.1, list = FALSE)

training <- final_data[-test_index,]
temp <- final_data[test_index,]

# Make sure all variable values in test set are also in train set
testing <- temp %>%
  semi_join(training, by = "first_bat_team") %>%
  semi_join(training, by = "second_bat_team") %>%
  semi_join(training, by = "venue") %>%
  semi_join(training, by = "toss_decision") %>%
  semi_join(training, by = "toss_winner") 

# Add rows removed from temp set back into train set
removed <- anti_join(temp, testing)
training <- rbind(training, removed) 

# Limit the number of deciamal places to 4 
options(digits=4)
# if using R 3.5 or earlier, use `set.seed(1)` instead - to get same results every time
set.seed(1, sample.kind="Rounding")

# Check dimensions & variable names of train_set and test_set
dim(training)
dim(testing)
names(training)
names(testing)

###################################################################################################################

### Section 4.1 ###

#"Naive Bayes" method:
# Fit the Model based on "Naive Bayes" method, predict, test, calculate F1 score for all classes
fit_bayes <- train(winner ~ ., method = "naive_bayes", data = training)
fit_bayes
pre_bayes <- predict(fit_bayes, testing)
f1_score_bayes <- confusionMatrix(pre_bayes, testing$winner)$byClass[,"F1"]
f1_score_bayes <- as.data.frame(t(f1_score_bayes)) %>% mutate(avg_F1_score = rowMeans(.))
f1_score_bayes

# Make column names more readable
colnames(f1_score_bayes) = gsub("Class: ", "", colnames(f1_score_bayes))
# F1 table for different models
F1_table <- data.frame(Model = "Naive Bayes") %>% bind_cols(f1_score_bayes)

F1_table %>% knitr::kable()

####################################################################################################################

### Section 4.2 ###

#"rpart" method:
fir_rpart <- train(winner ~ ., method = "rpart", data = training)
pre_rpart <- predict(fir_rpart, testing)
F1_score_rpart <- confusionMatrix(pre_rpart, testing$winner)$byClass[,"F1"]
F1_score_rpart <- as.data.frame(t(F1_score_rpart)) %>% mutate(avg_F1_score = rowMeans(.))
F1_score_rpart

# Make column names more readable
colnames(F1_score_rpart) = gsub("Class: ", "", colnames(F1_score_rpart))
# Update F1 table - continued.2
F1_table <- bind_rows(F1_table,
                      data.frame(Model = "CART (rpart)") %>% bind_cols(F1_score_rpart))

F1_table %>% knitr::kable()

####################################################################################################################

### Section 4.3 ###

#"Multinom" method:
fit_multinom <- train(winner ~ ., method = "multinom", data = training, trace = FALSE)
pre_multinom <- predict(fit_multinom, testing)
F1_score_multinom <- confusionMatrix(pre_multinom, testing$winner)$byClass[,"F1"]
F1_score_multinom <- as.data.frame(t(F1_score_multinom)) %>% mutate(avg_F1_score = rowMeans(.))
F1_score_multinom

# Make column names more readable
colnames(F1_score_multinom) = gsub("Class: ", "", colnames(F1_score_multinom))
# Update F1 table - continued.4
F1_table <- bind_rows(F1_table,
                      data.frame(Model = "Multinom") %>% bind_cols(F1_score_multinom))

F1_table %>% knitr::kable()

####################################################################################################################

### Section 4.4 ###

#"LDA" method:
fit_lda <- train(winner ~ ., method = "lda", data = training)
pre_lda <- predict(fit_lda, testing)
F1_lda <- confusionMatrix(pre_lda, testing$winner)$byClass[,"F1"]
F1_lda <- as.data.frame(t(F1_lda)) %>% mutate(avg_F1_score = rowMeans(.))
F1_lda

# Make column names more readable
colnames(F1_lda) = gsub("Class: ", "", colnames(F1_lda))
# Update F1 table - continued.5
F1_table <- bind_rows(F1_table,
                      data.frame(Model = "LDA") %>% bind_cols(F1_lda))

F1_table %>% knitr::kable()

####################################################################################################################

### Section 4.5 ###

#"Random Forest" method:
trainctrl <- trainControl(method="cv")
fit_randomForest <- train(winner ~ ., method = "rf", data = training,  trControl=trainctrl)
pre_randomForest <- predict(fit_randomForest, testing)
F1_score_randomForest <- confusionMatrix(pre_randomForest, testing$winner)$byClass[,"F1"]
F1_score_randomForest <- as.data.frame(t(F1_score_randomForest)) %>% mutate(avg_F1_score = rowMeans(.))
F1_score_randomForest

# Make column names more readable
colnames(F1_score_randomForest) = gsub("Class: ", "", colnames(F1_score_randomForest))
# Update F1 table - continued.3
F1_table <- bind_rows(F1_table,
                      data.frame(Model = "Random Forest (rf)") %>% bind_cols(F1_score_randomForest))

F1_table %>% knitr::kable()

####################################################################################################################

### Section 4.6 ###

#"KNN" method:
fit_knn <- train(winner ~ ., method = "knn", data = training)
pre_knn <- predict(fit_knn, testing)
F1_knn <- confusionMatrix(pre_knn, testing$winner)$byClass[,"F1"]
F1_knn <- as.data.frame(t(F1_knn)) %>% mutate(avg_F1_score = rowMeans(.))
F1_knn

# Make column names more readable
colnames(F1_knn) = gsub("Class: ", "", colnames(F1_knn))
# Update F1 table - Final
F1_table <- bind_rows(F1_table,
                      data.frame(Model = "KNN") %>% bind_cols(F1_knn))

F1_table %>% knitr::kable()

