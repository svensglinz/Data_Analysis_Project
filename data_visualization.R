#load libraries
#---------------------------------------------------------------------------
library(tidyverse)
library(usmap)
library(stargazer)

#import cleaned files
#---------------------------------------------------------------------------
donations <- read_csv("files/donations_cleaned.csv")
pol_ideology <- read_csv("files/pol_ideology.csv")
gender_dist <- read_csv("files/gender_distribution.csv")
us_states <- read_csv("files/us_states.csv")

#############################################################
#Create Data Frames for further analysis
#############################################################

#data frame which shows the donations grouped to the individual person (multiple donations 
#by the same person are summed up)
donations_grouped <- donations |> 
  group_by(FIRST_NAME, LAST_NAME, GENDER, CITY, STATE, EMPLOYER, ZIP_CODE, OCCUPATION, CANDIDATE) |> 
  summarize(DONATION_AMT = sum(TRANSACTION_AMT)) |> 
  ungroup() 

#produce summary table for overview 
summary_table <- donations_grouped |> group_by(GENDER, CANDIDATE) |> summarize(average_donation = mean(DONATION_AMT),
                                                              "total_donation (MIO)" = sum(DONATION_AMT)/10^6,
                                                              "total_donors (T)" = n()/10^3)

summary_table[3:5] <- data.frame(sapply(summary_table[3:5], function(x) round(x, 0)))
stargazer(summary_table, out = "plots/summary_table.html", summary = FALSE)

#data frame which shows the % of Male and Female per US State
gender_state <- gender_dist |> 
  pivot_longer(c(2:3), names_to = "GENDER", values_to= "RATIO") |> 
  mutate(GENDER = case_when(GENDER == "Male" ~ "M",
                            GENDER == "Female" ~ "F")) |> 
  left_join(us_states, by = c("Location" = "STATE")) |> 
  relocate("Location", "ABBREVIATION") |> 
  rename(LOCATION = Location) |> 
  drop_na() 

#data frame which shows the total contributions to each candidate per gender (donations and number of people who donated)
by_gender <- donations_grouped |> 
  group_by(GENDER, CANDIDATE) |> 
  summarize(DONATION = sum(DONATION = DONATION_AMT), COUNT = n()) |> 
  pivot_longer(c(DONATION, COUNT), names_to = "VARIABLE", values_to = "VALUE")

#data frame which shows the total contributions to each canidate (donations and number of people who donated)
by_variable <- by_gender |> 
  group_by(CANDIDATE, VARIABLE) |> 
  summarize(SUM = sum(VALUE))

#data frame which shows the relative contributions per gender per candidate (donations and number of people who donated)
by_gender_rel <- by_gender |>
  left_join(by_variable, by = c("CANDIDATE", "VARIABLE")) |> 
  mutate(REL_GENDER = VALUE/SUM)

#data frame with donations (count and value) per gender per candidate per state
by_gender_state <- donations_grouped |> 
  group_by(GENDER, CANDIDATE, STATE) |>  
  summarize(DONATION = sum(DONATION = DONATION_AMT), COUNT = n()) |> 
  pivot_longer(c(DONATION, COUNT), names_to = "VARIABLE", values_to = "VALUE") |> 
  left_join(us_states, by = c("STATE" = "ABBREVIATION")) |> 
  select(-STATE.y) |> 
  relocate("CANDIDATE",  "STATE", "GENDER") |>  
  ungroup()

#data frame which contains donations per state (count and value) per candidate
by_variable_state <- by_gender_state |>
  group_by(CANDIDATE, STATE, VARIABLE) |> 
  summarize(SUM = sum(VALUE))

#data frame with donations (count and value) per gender per candidate per state incl. relative values per gender
by_gender_state_rel <- by_gender_state |> 
  left_join(by_variable_state, by = c("VARIABLE", "CANDIDATE", "STATE")) |> 
  mutate(REL_GENDER = VALUE/SUM)

#plot which adds the relative occurence of male/female in each us state
by_gender_state_rel_pol <- by_gender_state_rel |>
  left_join(pol_ideology[2:5], by = c("STATE" = "Abbreviation")) |> 
  left_join(gender_state, by = c("STATE" = "ABBREVIATION", "GENDER" = "GENDER"))

#data frame which shows the donations and relative amounts grouped by month of each year
by_month <- donations %>% 
  group_by(CANDIDATE, as.yearmon(TRANSACTION_DT, format = "%Y-%m-%d"), GENDER) %>% 
  summarize(DONATION = sum(TRANSACTION_AMT), COUNT = n())
colnames(by_month)[2] <- "MONTH"

by_month <- by_month %>%
  pivot_longer(c(DONATION,COUNT), names_to = "VARIABLE", values_to = "VALUE")

by_month <- by_month %>% pivot_wider(values_from = VALUE, names_from = GENDER)
by_month <- by_month %>% mutate(REL_F = F/(F+M))


################################################################
#Plot 1:
################################################################

#plot which shows the relative count and amount women donated for trump and biden
rel_share_plot <- by_gender_rel |> filter(GENDER == "F") |> 
  ggplot(aes(x =  CANDIDATE, fill =  VARIABLE))+
  geom_bar(aes(y =REL_GENDER), stat = "identity", position = position_dodge())+
  labs(title = "% Female Donations (by Number of Donations and Donation Value)",
       fill = "",
       y = "% Female",
       x = "Candidate")

ggsave(filename = "plots/rel_share_donations.png", plot = rel_share_plot, device = "png", width = 7, height = 10)

################################################################
#Plot 2:
################################################################

#plot which shows relative amount of female donations per month and aggregate monthly donation
by_month %>% filter(VARIABLE == "DONATION" & MONTH < "Nov 2020") %>% 
  ggplot(aes(x = MONTH, y = REL_F , GROUP = CANDIDATE, color = CANDIDATE, fill = CANDIDATE))+
  geom_line()+
  geom_bar(aes(y = (F+M)/10^8.6), stat = "identity", position = position_dodge())+
  scale_y_continuous(
    # Features of the first axis
    name = "relative Share of Female Donations",
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*10^2.6, name="Total Donations in Mio.")
  )

#save plot 
ggsave(filename = "plots/rel_share_donations.png", plot = rel_share_plot, device = "png", width = 7, height = 10)

#regression model which analyzes influence of conservativeness on relative donations of women!
#run diffeent speicifications of the model

#take only female and count valriable 
temp_count <- by_gender_state_rel_pol |> 
  filter(GENDER == "F" & VARIABLE == "COUNT") |>  drop_na()

temp_donation <- by_gender_state_rel_pol |> 
  filter(GENDER == "F" & VARIABLE == "DONATION") |>  drop_na()

reg_0_0 <-  lm(REL_GENDER ~ RATIO + CANDIDATE, data = temp_count)
reg_0_1 <-  lm(REL_GENDER ~ RATIO + CANDIDATE, data = temp_donation)
#regression specifications
reg_1_0 <- lm(REL_GENDER ~ RATIO + CANDIDATE + Conservative, data = temp_count)
reg_1_1 <- lm(REL_GENDER ~ RATIO + CANDIDATE + Conservative, data = temp_donation)

#store results as HTML table
stargazer(reg_0_0, reg_1_0, reg_0_1, reg_1_1, out = "plots/regression.html", column.labels=c( "Number of Donors", "Donation Amount"),
          column.separate = c(2,2), covariate.labels = c("Female Ratio Population", "Candidate: Trump"), 
          dep.var.labels = c("FEMALE RATIO"))


data_republicans <- by_gender_state_rel |> filter(CANDIDATE == "TRUMP" & VARIABLE == "COUNT" & GENDER == "M")
data_republicans <- data_republicans |>
  select(STATE, REL_GENDER) |>
  rename(values = REL_GENDER, state = STATE)

plot_usmap(regions = c("states"), data = data_republicans)+
  scale_fill_continuous(low = "white", high = "blue", name = "% donations by men")+
  labs(title = "TITLE", subtitle = "SUBTITLE")+
  theme(legend.position = "right")

#calculate M / F Rations without taking into account NA -> such that they add up to 100%!

#plot result for democrats 
data_democrats <- by_gender_state_rel |> filter(CANDIDATE == "BIDEN" & VARIABLE == "COUNT" & GENDER == "M")
data_democrats <- data_democrats |>
  select(STATE, REL_GENDER) |>
  rename(values = REL_GENDER, state = STATE)


plot_democrats <- plot_usmap(regions = "states", data = data_democrats)+
  scale_fill_continuous(low = "white", high = "blue", name = "% donations by men")+
  labs(title = "TITLE", subtitle = "SUBTITLE")+
  theme(legend.position = "right")

ggsave(plot_democrats, device = "png", filename = "sample.png")


inhabitants_vs_female <- ggplot(by_gender_state_rel_pol |> 
                                     filter(GENDER == "F", VARIABLE == "COUNT") |> drop_na(), 
                                   aes(x = Conservative, y = REL_GENDER/RATIO , color = CANDIDATE))+
                              geom_point()+
                              #ylim((c(0.3, 0.6)))+
                              geom_smooth(method = "lm")+
                              labs(y = "%female donors / % Female inhabitants", x = "%Conservative inhabitants", 
                                   title = "%female donors per % inhabitants vs inhabitants who classify themselves as Conservative",
                                   color = "")

ggsave(filename = "plots/inhabitants_vs_female.png", plot = inhabitants_vs_female, device = "png", width = 7, height = 10)


conservative_vs_female <- ggplot(by_gender_state_rel_pol |> 
                                  filter(GENDER == "F", VARIABLE == "COUNT") |> drop_na(), 
                                aes(x = Conservative, y = REL_GENDER , color = CANDIDATE))


by_profession <- donations_grouped |> group_by(OCCUPATION, GENDER, CANDIDATE) |> summarize(count = n()) |> ungroup()
by_profession$id = seq(1, nrow(by_profession), by = 1)
profession_df <- data.frame(id = by_profession$id, text = by_profession$OCCUPATION)

professions <- classify_occupation(corpus = profession_df, isco_level = 1, lang = "en", num_leaves = 10)
professions$id <- as.numeric(professions$id)
by_profession <- by_profession |> left_join(professions, by = "id")
by_profession$OCCUPATION <- ifelse(is.na(by_profession$preferredLabel), by_profession$OCCUPATION, by_profession$preferredLabel)
by_profession <- by_profession |> select(GENDER, CANDIDATE, count, OCCUPATION)
by_profession <- by_profession |> group_by(GENDER, CANDIDATE, OCCUPATION) |> summarize(count = sum(count)) |> arrange(desc(count))

by_profession_total <- by_profession |> group_by(GENDER, CANDIDATE) |> summarize(total = sum(count)) |> ungroup()
by_profession <- by_profession |> left_join(by_profession_total, by = c("GENDER", "CANDIDATE"))
by_profession$rel_share_gender <- by_profession$count / by_profession$total

biden_profession_M <- by_profession |> filter(CANDIDATE == "BIDEN" & GENDER == "M") |> arrange(desc(rel_share_gender)) |> top_n(1)
biden_profession_F <- by_profession |> filter(CANDIDATE == "BIDEN" & GENDER == "F") |> arrange(desc(rel_share_gender)) |> top_n(1)
  
trump_profession_M <- by_profession |> filter(CANDIDATE == "TRUMP" & GENDER == "M") |> arrange(desc(rel_share_gender)) |> top_n(1)
trump_profession_F <- by_profession |> filter(CANDIDATE == "TRUMP" & GENDER == "F") |> arrange(desc(rel_share_gender)) |> top_n(1)

top_job <- bind_rows(biden_profession_F, biden_profession_M, trump_profession_F, trump_profession_M) |> 
  select(GENDER, OCCUPATION, rel_share_gender) |> pivot_wider(values_from = rel_share_gender, names_from = GENDER)

