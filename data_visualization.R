
#############################################################
#Load relevant Libraries
#############################################################

library(tidyverse)
library(usmap)
library(stargazer)
library(zoo)
library(scales)
library(ggthemes)

#############################################################
#Import cleaned files
#############################################################

donations <- read_csv("files/donations_cleaned.csv")
pol_ideology <- read_csv("files/pol_ideology.csv")
gender_dist <- read_csv("files/gender_distribution.csv")
us_states <- read_csv("files/us_states.csv")

#############################################################
#Create Data Frames which are needed for creating plots
#############################################################

#data frame which shows the donations grouped to the individual person (multiple donations 
#by the same person are summed up)
#-----------------------------------------------------------
donations_grouped <- donations |> 
  group_by(FIRST_NAME, LAST_NAME, GENDER, CITY, STATE,
           EMPLOYER, ZIP_CODE, OCCUPATION, CANDIDATE) |> 
  summarize(DONATION_AMT = sum(TRANSACTION_AMT)) |> 
  ungroup() 

#produce summary table for overview and export as HTML
summary_table <- donations_grouped |>
  group_by(GENDER, CANDIDATE) |>
  summarize("average donation" = mean(DONATION_AMT),
            "total_donation (MIO)" = sum(DONATION_AMT)/10^6,
            "total_donors (T)" = n()/10^3)

summary_table[3:5] <- data.frame(sapply(summary_table[3:5], function(x) round(x, 0)))
stargazer(summary_table, out = "plots/summary_table.html", summary = FALSE)


#data frame which shows the % of Male and Female per US State
#-----------------------------------------------------------
gender_state <- gender_dist |> 
  pivot_longer(c(2:3), names_to = "GENDER", values_to= "RATIO") |> 
  mutate(GENDER = case_when(GENDER == "Male" ~ "M",
                            GENDER == "Female" ~ "F")) |> 
  left_join(us_states, by = c("Location" = "STATE")) |> 
  relocate("Location", "ABBREVIATION") |> 
  rename(LOCATION = Location) |> 
  drop_na() 

#data frame which shows the total contributions to each candidate per gender (donations and number of people who donated)
#-----------------------------------------------------------
by_gender <- donations_grouped |> 
  group_by(GENDER, CANDIDATE) |> 
  summarize(DONATION = sum(DONATION = DONATION_AMT), COUNT = n()) |> 
  pivot_longer(c(DONATION, COUNT), names_to = "VARIABLE", values_to = "VALUE")

#data frame which shows the total contributions to each candidate (donations and number of people who donated)
#-----------------------------------------------------------
by_variable <- by_gender |> 
  group_by(CANDIDATE, VARIABLE) |> 
  summarize(SUM = sum(VALUE))

#data frame which shows the relative contributions per gender per candidate (donations and number of people who donated)
#-----------------------------------------------------------
by_gender_rel <- by_gender |>
  left_join(by_variable, by = c("CANDIDATE", "VARIABLE")) |> 
  mutate(REL_GENDER = VALUE/SUM)

#data frame with donations (count and value) per gender per candidate per state
#-----------------------------------------------------------
by_gender_state <- donations_grouped |> 
  group_by(GENDER, CANDIDATE, STATE) |>  
  summarize(DONATION = sum(DONATION = DONATION_AMT), COUNT = n()) |> 
  pivot_longer(c(DONATION, COUNT), names_to = "VARIABLE", values_to = "VALUE") |> 
  left_join(us_states, by = c("STATE" = "ABBREVIATION")) |> 
  select(-STATE.y) |> 
  relocate("CANDIDATE",  "STATE", "GENDER") |>  
  ungroup()

#data frame which contains donations per state (count and value) per candidate
#-----------------------------------------------------------
by_variable_state <- by_gender_state |>
  group_by(CANDIDATE, STATE, VARIABLE) |> 
  summarize(SUM = sum(VALUE))

#data frame with donations (count and value) per gender per candidate per state incl. relative values per gender
#-----------------------------------------------------------
by_gender_state_rel <- by_gender_state |> 
  left_join(by_variable_state, by = c("VARIABLE", "CANDIDATE", "STATE")) |> 
  mutate(REL_GENDER = VALUE/SUM)

#plot which adds the relative occurrence of male/female in each us state
#-----------------------------------------------------------
by_gender_state_rel_pol <- by_gender_state_rel |>
  left_join(pol_ideology[2:5], by = c("STATE" = "Abbreviation")) |> 
  left_join(gender_state, by = c("STATE" = "ABBREVIATION", "GENDER" = "GENDER"))

#data frame which shows the donations and relative amounts grouped by month of each year
#-----------------------------------------------------------
by_month <- donations %>% 
  group_by(CANDIDATE, as.yearmon(TRANSACTION_DT, format = "%Y-%m-%d"), GENDER) %>% 
  summarize(DONATION = sum(TRANSACTION_AMT), COUNT = n())
colnames(by_month)[2] <- "MONTH"

by_month <- by_month %>%
  pivot_longer(c(DONATION,COUNT), names_to = "VARIABLE", values_to = "VALUE")

by_month <- by_month %>% pivot_wider(values_from = VALUE, names_from = GENDER)
by_month <- by_month %>% mutate(REL_F = F/(F+M))


################################################################
#Plot 1: Relative Donations by Women (by Number of Donors and by Donation Value)
################################################################

#plot which shows the relative count and amount women donated for Trump and Biden
rel_share_plot <- by_gender_rel |> filter(GENDER == "F") |> 
  ggplot(aes(x =  CANDIDATE, fill =  VARIABLE))+
  geom_bar(aes(y =REL_GENDER), stat = "identity", position = position_dodge())+
  labs(title = "Relative Female Donations",
       fill = "",
       y = "% Female",
       x = "Candidate")+
  scale_y_continuous(labels = scales::percent)+
  theme_economist()+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
      plot.title = element_text(family = "sans", size = 16,
                                margin=margin(0,0,30,0), hjust = 0.5))+
  scale_fill_discrete(labels = c("Number of Donors", "Value of Donations"))

#save output
ggsave(filename = "plots/rel_share_donations.png", plot = rel_share_plot, device = "png")


################################################################
#Plot 2: Evolution of Relative Female Donations over Time for both Candidates
################################################################

#plot which shows relative amount of female donations per month and aggregate monthly donation
evolution_female_donations <- by_month |> 
  filter(VARIABLE == "DONATION" & MONTH < "Nov 2020") |> 
  ggplot(aes(x = as.Date(MONTH), y = REL_F , GROUP = CANDIDATE,
             color = CANDIDATE, fill = CANDIDATE))+
  geom_line(size = 0.8)+
  geom_bar(aes(y = (F+M)/10^8.6), stat = "identity", position = position_dodge())+
  scale_y_continuous(
    # Features of the first axis
    name = "Share of Female Donations (value)",
    labels = percent,
    # Add a second axis and specify its features
    sec.axis = sec_axis( trans=~.*10^2.6*10^6,
                         name="Total Donations in $Mio.",
                         labels = unit_format(unit = "M", scale = 1e-6)))+
    labs(color = "", fill = "", 
       title = "Evolution of Relative Female Donations")+
  theme_economist()+
  theme(axis.title.y.left = element_text(vjust = 4),  
        legend.position = "bottom",
        legend.text = element_text(size=10),
        legend.key.size = unit(0.2, 'cm'),
        plot.title = element_text(family = "sans", size = 16,
                                  margin=margin(0,0,15,0), hjust = 0.5),
        axis.title.y.right =  element_text(angle = 270, vjust = 4),
        axis.title.x = element_blank())+
  scale_x_date(breaks = "4 month", date_labels = "%b- %y")
  
#save plot 
ggsave(filename = "plots/evolution_female_donations.png",
       plot = evolution_female_donations, device = "png")


################################################################
#Regression: 
#model which analyzes influence of conservativeness on relative donations of women!
################################################################

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
stargazer(reg_0_0, reg_1_0, reg_0_1, reg_1_1, out = "plots/regression.html",
          column.labels=c( "Number of Donors", "Donation Amount"),
          column.separate = c(2,2),
          covariate.labels = c("Female Ratio Population", "Candidate: Trump"), 
          dep.var.labels = c("FEMALE RATIO"))

################################################################
#Regression: 
#model which analyzes influence of conservativeness on relative donations of women!
################################################################

data_republicans <- by_gender_state_rel |>
  filter(CANDIDATE == "TRUMP" & VARIABLE == "DONATION" & GENDER == "F")
data_republicans <- data_republicans |>
  select(STATE, REL_GENDER) |>
  rename(values = REL_GENDER, state = STATE)

plot_republicans <- plot_usmap(regions = c("states"), data = data_republicans)+
  scale_fill_continuous(high = "blue", low = "white",
                        name = "% donations by Female",
                        limits = c(0.25, 0.4), oob=squish,
                        guide = guide_colorbar(barwidth = 1, barheight =5,
                                               label.theme = element_text(size = 10),
                                               title = element_blank()))+
  labs(title = "rel. Share of Female Donations to Trump")+
  theme(legend.position = "right")+
  theme_economist()+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         axis.line= element_blank(),
         axis.title = element_blank(),
         legend.position = "right",
         legend.key.size = unit(1, "cm"),
         plot.title = element_text(margin=margin(0,0,15,0), size = 15, hjust = -1))+
  scale_x_discrete(labels = NULL, breaks = NULL)+
  scale_y_discrete(labels = NULL, breaks = NULL)

ggsave(plot_republicans, device = "png", filename = "plots/plot_republicans.png", width = 5)

#calculate M / F Rations without taking into account NA -> such that they add up to 100%!

#plot result for democrats 
data_democrats <- by_gender_state_rel |>
  filter(CANDIDATE == "BIDEN" & VARIABLE == "DONATION" & GENDER == "F")

data_democrats <- data_democrats |>
  select(STATE, REL_GENDER) |>
  rename(values = REL_GENDER, state = STATE)

plot_democrats <- plot_usmap(regions = c("states"), data = data_democrats)+
  scale_fill_continuous(high = "blue", low = "white",
                        name = "% donations by Female",
                         oob=squish,
                        guide = guide_colorbar(barwidth = 1, barheight =5,
                                               label.theme = element_text(size = 10),
                                               title = element_blank()))+
  labs(title = "rel. Share of Female Donations to Biden")+
  theme(legend.position = "right")+
  theme_economist()+
  theme( panel.grid.major = element_blank(),
         panel.grid.minor = element_blank(),
         panel.border = element_blank(),
         axis.line= element_blank(),
         axis.title = element_blank(),
         legend.position = "right",
         legend.key.size = unit(1, "cm"),
         plot.title = element_text(margin=margin(0,0,15,0), size = 15, hjust = -1))+
  scale_x_discrete(labels = NULL, breaks = NULL)+
  scale_y_discrete(labels = NULL, breaks = NULL)


ggsave(plot_democrats, device = "png", filename = "plots/plot_democrats.png", width = 5)

################################################################
#Regression: 
#model which analyzes influence of conservativeness on relative donations of women!
################################################################

inhabitants_vs_female <- ggplot(by_gender_state_rel_pol |> 
                                     filter(GENDER == "F", VARIABLE == "COUNT") |> drop_na(), 
                                   aes(x = Conservative, y = REL_GENDER/RATIO , color = CANDIDATE))+
                              geom_point(size = 1)+
  scale_x_continuous(labels = scales::percent)+
                              geom_smooth(method = "lm", size = 0.5)+
                              theme_economist()+
  labs(y = "%Female donors / % Female inhabitants",
       x = "%Conservative inhabitants", 
      title = "Pop. Adj. %Female Donors vs. Conservativeness",
      color = "")+
  theme(axis.text.x = element_text(vjust = 0.5),
        plot.title = element_text(family = "sans", size = 16,
                                  margin=margin(0,0,15,0)),
        axis.title.y = element_text(vjust = 3),
        legend.text = element_text(size = 10),
        legend.position = "right")


ggsave(filename = "plots/inhabitants_vs_female.png", plot = inhabitants_vs_female, device = "png", width = 6)

###############################################################################
#Top Job of Donors by Candidate and Gender
###############################################################################

by_profession <- donations_grouped |>
  group_by(OCCUPATION, GENDER, CANDIDATE) |>
  summarize(count = n()) |>
  arrange(by = desc(count)) |>
  ungroup()

by_profession_total <- by_profession |>
  group_by(GENDER, CANDIDATE) |>
  summarize(total = sum(count)) |>
  ungroup()

by_profession <- by_profession |>
  left_join(by_profession_total, by = c("GENDER", "CANDIDATE")) |>
  drop_na()

by_profession$rel_share_gender <- by_profession$count / by_profession$total 

top_job <- by_profession |>
              group_by(GENDER, CANDIDATE) |>
              summarize(SHARE = max(rel_share_gender),
                        OCCUPATION = OCCUPATION[rel_share_gender == max(rel_share_gender)])

topjob <- top_job |> 
  ggplot(aes(x = CANDIDATE, y = SHARE, fill = GENDER))+
  geom_bar(stat = "identity", position = position_dodge())+
  theme_economist()+
  labs(title = "Relative occurence of Profession Retired / Unemployed", 
       y = "hello",
       fill = "")+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        legend.position = "bottom",
        plot.title = element_text(family = "sans",
                                  size = 12,
                                  margin=margin(0,0,15,0),
                                  hjust = 1))

ggsave(filename = "plots/topjob.png", plot = topjob, device = "png")

###############################################################################
#Cumulative evolution of donations per Gender per Candidate over Time
###############################################################################

donations_trump <- donations |> 
  filter(TRANSACTION_DT < as.Date("2020-11-03", "%Y-%m-%d")) |> 
  filter(CANDIDATE == "TRUMP") |> 
  group_by(GENDER, TRANSACTION_DT) |> 
  summarize(donations = sum(TRANSACTION_AMT))

donations_biden <- donations |> 
  filter(TRANSACTION_DT < as.Date("2020-11-03", "%Y-%m-%d")) |> 
  filter(CANDIDATE == "BIDEN") |> 
  group_by(GENDER, TRANSACTION_DT) |> 
  summarize(donations = sum(TRANSACTION_AMT))

cumdonations_biden <- donations_biden |> arrange((TRANSACTION_DT)) |> 
  group_by(GENDER) |> 
  summarize(cumdonations = cumsum(donations)) |> 
  ungroup()

donations_biden <- cbind(donations_biden, cumdonations_biden["cumdonations"])

cumdonations_trump <- donations_trump |> arrange((TRANSACTION_DT)) |> 
  group_by(GENDER) |> 
  summarize(cumdonations = cumsum(donations)) |> 
  ungroup()

donations_trump <- cbind(donations_trump, cumdonations_trump["cumdonations"])

cum_donations <- ggplot()+
  geom_line(data = donations_biden, aes(x = TRANSACTION_DT, y = cumdonations, color = GENDER, linetype = "Trump"), size =0.8)+
  geom_line(data = donations_trump, aes(x = TRANSACTION_DT, y = cumdonations, color = GENDER, linetype = "Biden"), size = 0.8)+
  labs(linetype = "", color = "", 
       x = "Date", 
       y = "Cumulative Donations", 
       title = "Cumulative Donations by Gender")+
  scale_x_date(breaks = "2 month", date_labels = "%b- %y")+
  scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6))+
  annotate("rect", fill = "grey", alpha = 0.8,
           xmin = as.Date("2020-08-08", "%Y-%m-%d"), 
           xmax = as.Date("2020-08-14", "%Y-%m-%d"),
           ymin = -Inf, ymax = Inf)+
  annotate(
    geom = "curve", x = as.Date("2020-05-10", "%Y-%m-%d"), y = 170*10^6, xend = as.Date("2020-08-10", "%Y-%m-%d"), yend = 150*10^6, 
    curvature = .3, arrow = arrow(length = unit(2, "mm"))
  )+
  annotate(geom = "text", x = as.Date("2020-05-10", "%Y-%m-%d"), y = 170*10^6, label = "Kamala Harris as VP", hjust = "right")+
  theme_economist()+
  theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
        legend.position = "bottom",
        plot.title = element_text(family = "sans", size = 16, margin=margin(0,0,15,0), hjust = 0.0),
        axis.title.x = element_blank(),
        axis.title.y = element_text(vjust = 3),
        legend.text = element_text(size = 10))

ggsave(filename = "plots/cum_donations.png", plot = cum_donations, device = "png")

