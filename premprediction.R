, apoisson7 = dpois(7, away_exp)
, apoisson8 = dpois(8, away_exp)
, apoisson9 = dpois(9, away_exp))
poisson_away <- prem_22_23 %>%
mutate(apoisson0 = dpois(0, away_exp)
, apoisson1 = dpois(1, away_exp)
, apoisson2 = dpois(2, away_exp)
, apoisson3 = dpois(3, away_exp)
, apoisson4 = dpois(4, away_exp)
, apoisson5 = dpois(5, away_exp)
, apoisson6 = dpois(6, away_exp)
, apoisson7 = dpois(7, away_exp)
, apoisson8 = dpois(8, away_exp)
, apoisson9 = dpois(9, away_exp))
poisson_away <- prem_22_23 %>%
mutate(apoisson0 = dpois(0, away_exp)
, apoisson1 = dpois(1, away_exp)
, apoisson2 = dpois(2, away_exp)
, apoisson3 = dpois(3, away_exp)
, apoisson4 = dpois(4, away_exp)
, apoisson5 = dpois(5, away_exp)
, apoisson6 = dpois(6, away_exp)
, apoisson7 = dpois(7, away_exp)
, apoisson8 = dpois(8, away_exp)
, apoisson9 = dpois(9, away_exp)) %>%
select(away_team, contains("apoisson"))
poisson_away <- prem_22_23 %>%
mutate(apoisson0 = dpois(0, away_exp)
, apoisson1 = dpois(1, away_exp)
, apoisson2 = dpois(2, away_exp)
, apoisson3 = dpois(3, away_exp)
, apoisson4 = dpois(4, away_exp)
, apoisson5 = dpois(5, away_exp)
, apoisson6 = dpois(6, away_exp)
, apoisson7 = dpois(7, away_exp)
, apoisson8 = dpois(8, away_exp)
, apoisson9 = dpois(9, away_exp)) %>%
select(away_team, contains("apoisson")) %>%
mutate(away_goals = strtoi(sub('.*apoisson', '', colnames(poisson_away[,c(2:11)])[max.col(poisson_away[,c(2:11)])])))
poisson_away <- prem_22_23 %>%
mutate(apoisson0 = dpois(0, away_exp)
, apoisson1 = dpois(1, away_exp)
, apoisson2 = dpois(2, away_exp)
, apoisson3 = dpois(3, away_exp)
, apoisson4 = dpois(4, away_exp)
, apoisson5 = dpois(5, away_exp)
, apoisson6 = dpois(6, away_exp)
, apoisson7 = dpois(7, away_exp)
, apoisson8 = dpois(8, away_exp)
, apoisson9 = dpois(9, away_exp)) %>%
select(away_team, contains("apoisson")) %>%
mutate(away_goals = strtoi(sub('.*apoisson', '', colnames(poisson_away[,c(2:11)])[max.col(poisson_away[,c(2:11)])]))) %>%
select(away_team, away_goals)
pred_results <- data.frame(poisson_home, poisson_away, check.names=F) %>%
mutate(home_win = ifelse(home_goals > away_goals, 1, 0), draw = ifelse(home_goals == away_goals, 1, 0), home_loss = ifelse(home_goals < away_goals, 1, 0), away_win = home_loss, away_loss = home_win,
home_points = 3 * home_win + 1 * draw + 0 * home_loss, away_points = 3 * away_win + 1 * draw + 0 * away_loss)
pred_results_home <- pred_results %>%
group_by(Team = home_team) %>%
summarize(W = sum(home_win), D = sum(draw), L = sum(home_loss), PTS = sum(home_points), GF = sum(home_goals), GA = sum(away_goals), GD = GF - GA) %>%
arrange(-PTS)
pred_results_away <- pred_results %>%
group_by(Team = away_team) %>%
summarize(W = sum(away_win), D = sum(draw), L = sum(away_loss), PTS = sum(away_points), GF = sum(away_goals), GA = sum(home_goals), GD = GF - GA) %>%
arrange(-PTS)
pred_results_home %>% gt() %>%
cols_align(
align = "center",
columns = c(Team, W, D, L, PTS, GF, GA, GD)
) %>%
cols_label(
Team = md("**Team**"),
W = md("**W**"),
D = md("**D**"),
L = md("**L**"),
PTS = md("**PTS**"),
GF = md("**GF**"),
GA = md("**GA**"),
GD = md("**GD**")
) %>%
tab_header(
title = md("**2022/23 Premier League Predicted HOME GAME Table**"),
subtitle = "Based on home attacking strength, away defending strength, and home game statistics"
)
pred_results_away %>% gt() %>%
cols_align(
align = "center",
columns = c(Team, W, D, L, PTS, GF, GA, GD)
) %>%
cols_label(
Team = md("**Team**"),
W = md("**W**"),
D = md("**D**"),
L = md("**L**"),
PTS = md("**PTS**"),
GF = md("**GF**"),
GA = md("**GA**"),
GD = md("**GD**")
) %>%
tab_header(
title = md("**2022/23 Premier League Predicted AWAY GAME Table**"),
subtitle = "Based on away attacking strength, home defending strength, and away game statistics"
)
prem_22_23 <- prem_22_23 %>%
mutate(HGF_MP = HGF/HMP, HGA_MP = HGA/HMP, AGF_MP = AGF/AMP, AGA_MP = AGA/AMP) %>%
mutate(HGF_MP_mean = mean(HGF_MP), HGA_MP_mean = mean(HGA_MP), AGF_MP_mean = mean(AGF_MP), AGA_MP_mean = mean(AGA_MP)) %>%
mutate(home_att_strength = HGF_MP/HGF_MP_mean, home_def_strength = HGA_MP/HGA_MP_mean, away_att_strength = AGF_MP/AGF_MP_mean, away_def_strength = AGA_MP/AGA_MP_mean) %>%
select(Squad, HW, HD, HL, HGF, HGA, HGD, HPts, AW, AD, AL, AGF, AGA, AGD, APts, home_att_strength, home_def_strength, away_att_strength, away_def_strength, HGF_MP_mean, HGA_MP_mean, AGF_MP_mean, AGA_MP_mean)
prem_22_23 <- read_csv("prem_22_23.csv")
prem_22_23 <- prem_22_23 %>%
mutate(HGF_MP = HGF/HMP, HGA_MP = HGA/HMP, AGF_MP = AGF/AMP, AGA_MP = AGA/AMP) %>%
mutate(HGF_MP_mean = mean(HGF_MP), HGA_MP_mean = mean(HGA_MP), AGF_MP_mean = mean(AGF_MP), AGA_MP_mean = mean(AGA_MP)) %>%
mutate(home_att_strength = HGF_MP/HGF_MP_mean, home_def_strength = HGA_MP/HGA_MP_mean, away_att_strength = AGF_MP/AGF_MP_mean, away_def_strength = AGA_MP/AGA_MP_mean) %>%
select(Squad, HW, HD, HL, HGF, HGA, HGD, HPts, AW, AD, AL, AGF, AGA, AGD, APts, home_att_strength, home_def_strength, away_att_strength, away_def_strength, HGF_MP_mean, HGA_MP_mean, AGF_MP_mean, AGA_MP_mean)
prem_22_23_home <- prem_22_23 %>%
select(home_team = Squad, home_att_strength, home_def_strength, avg = HGF_MP_mean)
prem_22_23_away <- prem_22_23 %>%
select(away_team = Squad, away_att_strength, away_def_strength)
prem_22_23 <- cross_join(prem_22_23_home, prem_22_23_away) %>%
filter(!(home_team == away_team)) %>%
mutate(home_exp = home_att_strength * away_def_strength * avg, away_exp = away_att_strength * home_def_strength * avg)
poisson_home <- prem_22_23 %>%
mutate(hpoisson0 = dpois(0, home_exp)
, hpoisson1 = dpois(1, home_exp)
, hpoisson2 = dpois(2, home_exp)
, hpoisson3 = dpois(3, home_exp)
, hpoisson4 = dpois(4, home_exp)
, hpoisson5 = dpois(5, home_exp)
, hpoisson6 = dpois(6, home_exp)
, hpoisson7 = dpois(7, home_exp)
, hpoisson8 = dpois(8, home_exp)
, hpoisson9 = dpois(9, home_exp)) %>%
select(home_team, contains("hpoisson")) %>%
mutate(home_goals = strtoi(sub('.*hpoisson', '', colnames(poisson_home[,c(2:11)])[max.col(poisson_home[,c(2:11)])]))) %>%
select(home_team, home_goals)
poisson_home <- prem_22_23 %>%
mutate(hpoisson0 = dpois(0, home_exp)
, hpoisson1 = dpois(1, home_exp)
, hpoisson2 = dpois(2, home_exp)
, hpoisson3 = dpois(3, home_exp)
, hpoisson4 = dpois(4, home_exp)
, hpoisson5 = dpois(5, home_exp)
, hpoisson6 = dpois(6, home_exp)
, hpoisson7 = dpois(7, home_exp)
, hpoisson8 = dpois(8, home_exp)
, hpoisson9 = dpois(9, home_exp)) %>%
select(home_team, contains("hpoisson"))
poisson_home <- prem_22_23 %>%
mutate(hpoisson0 = dpois(0, home_exp)
, hpoisson1 = dpois(1, home_exp)
, hpoisson2 = dpois(2, home_exp)
, hpoisson3 = dpois(3, home_exp)
, hpoisson4 = dpois(4, home_exp)
, hpoisson5 = dpois(5, home_exp)
, hpoisson6 = dpois(6, home_exp)
, hpoisson7 = dpois(7, home_exp)
, hpoisson8 = dpois(8, home_exp)
, hpoisson9 = dpois(9, home_exp)) %>%
select(home_team, contains("hpoisson")) %>%
mutate(home_goals = strtoi(sub('.*hpoisson', '', colnames(poisson_home[,c(2:11)])[max.col(poisson_home[,c(2:11)])])))
poisson_home <- prem_22_23 %>%
mutate(hpoisson0 = dpois(0, home_exp)
, hpoisson1 = dpois(1, home_exp)
, hpoisson2 = dpois(2, home_exp)
, hpoisson3 = dpois(3, home_exp)
, hpoisson4 = dpois(4, home_exp)
, hpoisson5 = dpois(5, home_exp)
, hpoisson6 = dpois(6, home_exp)
, hpoisson7 = dpois(7, home_exp)
, hpoisson8 = dpois(8, home_exp)
, hpoisson9 = dpois(9, home_exp)) %>%
select(home_team, contains("hpoisson")) %>%
mutate(home_goals = strtoi(sub('.*hpoisson', '', colnames(poisson_home[,c(2:11)])[max.col(poisson_home[,c(2:11)])]))) %>%
select(home_team, home_goals)
poisson_away <- prem_22_23 %>%
mutate(apoisson0 = dpois(0, away_exp)
, apoisson1 = dpois(1, away_exp)
, apoisson2 = dpois(2, away_exp)
, apoisson3 = dpois(3, away_exp)
, apoisson4 = dpois(4, away_exp)
, apoisson5 = dpois(5, away_exp)
, apoisson6 = dpois(6, away_exp)
, apoisson7 = dpois(7, away_exp)
, apoisson8 = dpois(8, away_exp)
, apoisson9 = dpois(9, away_exp)) %>%
select(away_team, contains("apoisson")) %>%
mutate(away_goals = strtoi(sub('.*apoisson', '', colnames(poisson_away[,c(2:11)])[max.col(poisson_away[,c(2:11)])]))) %>%
select(away_team, away_goals)
poisson_away <- prem_22_23 %>%
mutate(apoisson0 = dpois(0, away_exp)
, apoisson1 = dpois(1, away_exp)
, apoisson2 = dpois(2, away_exp)
, apoisson3 = dpois(3, away_exp)
, apoisson4 = dpois(4, away_exp)
, apoisson5 = dpois(5, away_exp)
, apoisson6 = dpois(6, away_exp)
, apoisson7 = dpois(7, away_exp)
, apoisson8 = dpois(8, away_exp)
, apoisson9 = dpois(9, away_exp)) %>%
select(away_team, contains("apoisson")) %>%
mutate(away_goals = strtoi(sub('.*apoisson', '', colnames(poisson_away[,c(2:11)])[max.col(poisson_away[,c(2:11)])])))
poisson_away <- prem_22_23 %>%
mutate(apoisson0 = dpois(0, away_exp)
, apoisson1 = dpois(1, away_exp)
, apoisson2 = dpois(2, away_exp)
, apoisson3 = dpois(3, away_exp)
, apoisson4 = dpois(4, away_exp)
, apoisson5 = dpois(5, away_exp)
, apoisson6 = dpois(6, away_exp)
, apoisson7 = dpois(7, away_exp)
, apoisson8 = dpois(8, away_exp)
, apoisson9 = dpois(9, away_exp)) %>%
select(away_team, contains("apoisson"))
poisson_away <- prem_22_23 %>%
mutate(apoisson0 = dpois(0, away_exp)
, apoisson1 = dpois(1, away_exp)
, apoisson2 = dpois(2, away_exp)
, apoisson3 = dpois(3, away_exp)
, apoisson4 = dpois(4, away_exp)
, apoisson5 = dpois(5, away_exp)
, apoisson6 = dpois(6, away_exp)
, apoisson7 = dpois(7, away_exp)
, apoisson8 = dpois(8, away_exp)
, apoisson9 = dpois(9, away_exp)) %>%
select(away_team, contains("apoisson")) %>%
mutate(away_goals = strtoi(sub('.*apoisson', '', colnames(poisson_away[,c(2:11)])[max.col(poisson_away[,c(2:11)])])))
poisson_away <- prem_22_23 %>%
mutate(apoisson0 = dpois(0, away_exp)
, apoisson1 = dpois(1, away_exp)
, apoisson2 = dpois(2, away_exp)
, apoisson3 = dpois(3, away_exp)
, apoisson4 = dpois(4, away_exp)
, apoisson5 = dpois(5, away_exp)
, apoisson6 = dpois(6, away_exp)
, apoisson7 = dpois(7, away_exp)
, apoisson8 = dpois(8, away_exp)
, apoisson9 = dpois(9, away_exp)) %>%
select(away_team, contains("apoisson")) %>%
mutate(away_goals = strtoi(sub('.*apoisson', '', colnames(poisson_away[,c(2:11)])[max.col(poisson_away[,c(2:11)])]))) %>%
select(away_team, away_goals)
pred_results <- data.frame(poisson_home, poisson_away, check.names=F) %>%
mutate(home_win = ifelse(home_goals > away_goals, 1, 0), draw = ifelse(home_goals == away_goals, 1, 0), home_loss = ifelse(home_goals < away_goals, 1, 0), away_win = home_loss, away_loss = home_win,
home_points = 3 * home_win + 1 * draw + 0 * home_loss, away_points = 3 * away_win + 1 * draw + 0 * away_loss)
pred_results_home <- pred_results %>%
group_by(Team = home_team) %>%
summarize(W = sum(home_win), D = sum(draw), L = sum(home_loss), PTS = sum(home_points), GF = sum(home_goals), GA = sum(away_goals), GD = GF - GA) %>%
arrange(-PTS)
pred_results_away <- pred_results %>%
group_by(Team = away_team) %>%
summarize(W = sum(away_win), D = sum(draw), L = sum(away_loss), PTS = sum(away_points), GF = sum(away_goals), GA = sum(home_goals), GD = GF - GA) %>%
arrange(-PTS)
pred_results_home %>% gt() %>%
cols_align(
align = "center",
columns = c(Team, W, D, L, PTS, GF, GA, GD)
) %>%
cols_label(
Team = md("**Team**"),
W = md("**W**"),
D = md("**D**"),
L = md("**L**"),
PTS = md("**PTS**"),
GF = md("**GF**"),
GA = md("**GA**"),
GD = md("**GD**")
) %>%
tab_header(
title = md("**2022/23 Premier League Predicted HOME GAME Table**"),
subtitle = "Based on home attacking strength, away defending strength, and home game statistics"
)
pred_results_away %>% gt() %>%
cols_align(
align = "center",
columns = c(Team, W, D, L, PTS, GF, GA, GD)
) %>%
cols_label(
Team = md("**Team**"),
W = md("**W**"),
D = md("**D**"),
L = md("**L**"),
PTS = md("**PTS**"),
GF = md("**GF**"),
GA = md("**GA**"),
GD = md("**GD**")
) %>%
tab_header(
title = md("**2022/23 Premier League Predicted AWAY GAME Table**"),
subtitle = "Based on away attacking strength, home defending strength, and away game statistics"
)
# compare to actual results using an online database like fbref (seems to be pretty accurate)
pred_results_home %>% gt() %>%
cols_align(
align = "center",
columns = c(Team, W, D, L, PTS, GF, GA, GD)
) %>%
cols_label(
Team = md("**Team**"),
W = md("**W**"),
D = md("**D**"),
L = md("**L**"),
PTS = md("**PTS**"),
GF = md("**GF**"),
GA = md("**GA**"),
GD = md("**GD**")
) %>%
tab_header(
title = md("**2022/23 Premier League Predicted HOME GAME Table**"),
subtitle = "Based on home attacking strength, away defending strength, and home game statistics"
)
prem_22_23 <- read_csv("prem_22_23.csv")
prem_22_23_home <- prem_22_23 %>%
select(home_team = Squad, home_att_strength, home_def_strength, home_avg = HGF_MP_mean)
prem_22_23 <- prem_22_23 %>%
mutate(HGF_MP = HGF/HMP, HGA_MP = HGA/HMP, AGF_MP = AGF/AMP, AGA_MP = AGA/AMP) %>%
mutate(HGF_MP_mean = mean(HGF_MP), HGA_MP_mean = mean(HGA_MP), AGF_MP_mean = mean(AGF_MP), AGA_MP_mean = mean(AGA_MP)) %>%
mutate(home_att_strength = HGF_MP/HGF_MP_mean, home_def_strength = HGA_MP/HGA_MP_mean, away_att_strength = AGF_MP/AGF_MP_mean, away_def_strength = AGA_MP/AGA_MP_mean) %>%
select(Squad, HW, HD, HL, HGF, HGA, HGD, HPts, AW, AD, AL, AGF, AGA, AGD, APts, home_att_strength, home_def_strength, away_att_strength, away_def_strength, HGF_MP_mean, HGA_MP_mean, AGF_MP_mean, AGA_MP_mean)
prem_22_23_home <- prem_22_23 %>%
select(home_team = Squad, home_att_strength, home_def_strength, home_avg = HGF_MP_mean)
prem_22_23_away <- prem_22_23 %>%
select(away_team = Squad, away_att_strength, away_def_strength, away_avg = AGF_MP_mean)
prem_22_23 <- cross_join(prem_22_23_home, prem_22_23_away) %>%
filter(!(home_team == away_team)) %>%
mutate(home_exp = home_att_strength * away_def_strength * home_avg, away_exp = away_att_strength * home_def_strength * away_avg)
poisson_home <- prem_22_23 %>%
mutate(hpoisson0 = dpois(0, home_exp)
, hpoisson1 = dpois(1, home_exp)
, hpoisson2 = dpois(2, home_exp)
, hpoisson3 = dpois(3, home_exp)
, hpoisson4 = dpois(4, home_exp)
, hpoisson5 = dpois(5, home_exp)
, hpoisson6 = dpois(6, home_exp)
, hpoisson7 = dpois(7, home_exp)
, hpoisson8 = dpois(8, home_exp)
, hpoisson9 = dpois(9, home_exp)) %>%
select(home_team, contains("hpoisson"))
poisson_home <- prem_22_23 %>%
mutate(hpoisson0 = dpois(0, home_exp)
, hpoisson1 = dpois(1, home_exp)
, hpoisson2 = dpois(2, home_exp)
, hpoisson3 = dpois(3, home_exp)
, hpoisson4 = dpois(4, home_exp)
, hpoisson5 = dpois(5, home_exp)
, hpoisson6 = dpois(6, home_exp)
, hpoisson7 = dpois(7, home_exp)
, hpoisson8 = dpois(8, home_exp)
, hpoisson9 = dpois(9, home_exp)) %>%
select(home_team, contains("hpoisson")) %>%
mutate(home_goals = strtoi(sub('.*hpoisson', '', colnames(poisson_home[,c(2:11)])[max.col(poisson_home[,c(2:11)])])))
poisson_home <- prem_22_23 %>%
mutate(hpoisson0 = dpois(0, home_exp)
, hpoisson1 = dpois(1, home_exp)
, hpoisson2 = dpois(2, home_exp)
, hpoisson3 = dpois(3, home_exp)
, hpoisson4 = dpois(4, home_exp)
, hpoisson5 = dpois(5, home_exp)
, hpoisson6 = dpois(6, home_exp)
, hpoisson7 = dpois(7, home_exp)
, hpoisson8 = dpois(8, home_exp)
, hpoisson9 = dpois(9, home_exp)) %>%
select(home_team, contains("hpoisson")) %>%
mutate(home_goals = strtoi(sub('.*hpoisson', '', colnames(poisson_home[,c(2:11)])[max.col(poisson_home[,c(2:11)])]))) %>%
select(home_team, home_goals)
poisson_away <- prem_22_23 %>%
mutate(apoisson0 = dpois(0, away_exp)
, apoisson1 = dpois(1, away_exp)
, apoisson2 = dpois(2, away_exp)
, apoisson3 = dpois(3, away_exp)
, apoisson4 = dpois(4, away_exp)
, apoisson5 = dpois(5, away_exp)
, apoisson6 = dpois(6, away_exp)
, apoisson7 = dpois(7, away_exp)
, apoisson8 = dpois(8, away_exp)
, apoisson9 = dpois(9, away_exp)) %>%
select(away_team, contains("apoisson"))
poisson_away <- prem_22_23 %>%
mutate(apoisson0 = dpois(0, away_exp)
, apoisson1 = dpois(1, away_exp)
, apoisson2 = dpois(2, away_exp)
, apoisson3 = dpois(3, away_exp)
, apoisson4 = dpois(4, away_exp)
, apoisson5 = dpois(5, away_exp)
, apoisson6 = dpois(6, away_exp)
, apoisson7 = dpois(7, away_exp)
, apoisson8 = dpois(8, away_exp)
, apoisson9 = dpois(9, away_exp)) %>%
select(away_team, contains("apoisson")) %>%
mutate(away_goals = strtoi(sub('.*apoisson', '', colnames(poisson_away[,c(2:11)])[max.col(poisson_away[,c(2:11)])])))
poisson_away <- prem_22_23 %>%
mutate(apoisson0 = dpois(0, away_exp)
, apoisson1 = dpois(1, away_exp)
, apoisson2 = dpois(2, away_exp)
, apoisson3 = dpois(3, away_exp)
, apoisson4 = dpois(4, away_exp)
, apoisson5 = dpois(5, away_exp)
, apoisson6 = dpois(6, away_exp)
, apoisson7 = dpois(7, away_exp)
, apoisson8 = dpois(8, away_exp)
, apoisson9 = dpois(9, away_exp)) %>%
select(away_team, contains("apoisson")) %>%
mutate(away_goals = strtoi(sub('.*apoisson', '', colnames(poisson_away[,c(2:11)])[max.col(poisson_away[,c(2:11)])]))) %>%
select(away_team, away_goals)
pred_results <- data.frame(poisson_home, poisson_away, check.names=F) %>%
mutate(home_win = ifelse(home_goals > away_goals, 1, 0), draw = ifelse(home_goals == away_goals, 1, 0), home_loss = ifelse(home_goals < away_goals, 1, 0), away_win = home_loss, away_loss = home_win,
home_points = 3 * home_win + 1 * draw + 0 * home_loss, away_points = 3 * away_win + 1 * draw + 0 * away_loss)
pred_results_home <- pred_results %>%
group_by(Team = home_team) %>%
summarize(W = sum(home_win), D = sum(draw), L = sum(home_loss), PTS = sum(home_points), GF = sum(home_goals), GA = sum(away_goals), GD = GF - GA) %>%
arrange(-PTS)
pred_results_away <- pred_results %>%
group_by(Team = away_team) %>%
summarize(W = sum(away_win), D = sum(draw), L = sum(away_loss), PTS = sum(away_points), GF = sum(away_goals), GA = sum(home_goals), GD = GF - GA) %>%
arrange(-PTS)
pred_results_home %>% gt() %>%
cols_align(
align = "center",
columns = c(Team, W, D, L, PTS, GF, GA, GD)
) %>%
cols_label(
Team = md("**Team**"),
W = md("**W**"),
D = md("**D**"),
L = md("**L**"),
PTS = md("**PTS**"),
GF = md("**GF**"),
GA = md("**GA**"),
GD = md("**GD**")
) %>%
tab_header(
title = md("**2022/23 Premier League Predicted HOME GAME Table**"),
subtitle = "Based on home attacking strength, away defending strength, and home game statistics"
)
pred_results_away %>% gt() %>%
cols_align(
align = "center",
columns = c(Team, W, D, L, PTS, GF, GA, GD)
) %>%
cols_label(
Team = md("**Team**"),
W = md("**W**"),
D = md("**D**"),
L = md("**L**"),
PTS = md("**PTS**"),
GF = md("**GF**"),
GA = md("**GA**"),
GD = md("**GD**")
) %>%
tab_header(
title = md("**2022/23 Premier League Predicted AWAY GAME Table**"),
subtitle = "Based on away attacking strength, home defending strength, and away game statistics"
)
pred_results <- inner_join(pred_results_home, pred_results_away, by = "Team")
pred_results <- pred_results %>%
mutate(W = W.x + W.y, D = D.x + D.y, L = L.x + L.y, PTS = PTS.x + PTS.y, GF = GF.x + GF.y, GA = GA.x + GA.y, GD = GD.x + GD.y) %>%
arrange(-PTS)
pred_results <- pred_results %>%
mutate(W = W.x + W.y, D = D.x + D.y, L = L.x + L.y, PTS = PTS.x + PTS.y, GF = GF.x + GF.y, GA = GA.x + GA.y, GD = GD.x + GD.y) %>%
arrange(-PTS) %>%
select(Team, W, D, L, PTS, GF, GA, GD)
pred_results_away %>% gt() %>%
cols_align(
align = "center",
columns = c(Team, W, D, L, PTS, GF, GA, GD)
) %>%
cols_label(
Team = md("**Team**"),
W = md("**W**"),
D = md("**D**"),
L = md("**L**"),
PTS = md("**PTS**"),
GF = md("**GF**"),
GA = md("**GA**"),
GD = md("**GD**")
) %>%
tab_header(
title = md("**2022/23 Premier League Predicted Table**"),
subtitle = "Based on cumulative home and away statistics"
)
pred_results %>% gt() %>%
cols_align(
align = "center",
columns = c(Team, W, D, L, PTS, GF, GA, GD)
) %>%
cols_label(
Team = md("**Team**"),
W = md("**W**"),
D = md("**D**"),
L = md("**L**"),
PTS = md("**PTS**"),
GF = md("**GF**"),
GA = md("**GA**"),
GD = md("**GD**")
) %>%
tab_header(
title = md("**2022/23 Premier League Predicted Table**"),
subtitle = "Based on cumulative home and away statistics"
)
x = pred_results$Team
actual_standings = read_csv("prem_22_23.csv")$Squad
predicted_standings = pred_results$Team
actual_standings
similarity_total = cor.test(predicted_standings, actual_standings, method = "kendall")
similarity_total = cor.test(predicted_standings, actual_standings, method = "kendall")
predicted_standings
similarity_total <- cor(rank(predicted_standings), rank(actual_standings), method = "spearman")
predicted_home = pred_results_home$Team
actual_home = (read_csv("prem_22_23.csv") %>% arrange(-HPts))$Squad
similarity_home <- cor(rank(predicted_home), rank(actual_home), method = "spearman")
actual_home
predicted_home
predicted_away = pred_results_away$Team
actual_away = (read_csv("prem_22_23.csv") %>% arrange(-APts))$Squad
similarity_away <- cor(rank(predicted_away), rank(actual_away), method = "spearman")
