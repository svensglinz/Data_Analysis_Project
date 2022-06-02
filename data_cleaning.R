
#Load needed libraries 
#---------------------------------------------------------------------------
library(tidyverse)
library(XML)

#read all file names with loop, merge and store as names_df
#only read files after 1940!
#---------------------------------------------------------------------------

#get file names 
names_path <- list.files("files/names")
years <- as.character(c(1940:2022))

relevant_years <- str_extract_all(names_path, pattern = "[0-9]+") %in% years
names_path_filtered <- names_path[relevant_years]

names_df <- data.frame()

for (i in names_path_filtered){
  
  temp <- read_delim(paste0("files/names/", i), col_names = F)
  names_df <- bind_rows(names_df, temp)
  
}

#change column names
colnames(names_df) <- c("NAME", "GENDER", "COUNT")

#summarize by name and gender and count number of occurences
names_df <- names_df |> 
  group_by(NAME, GENDER) |>
  summarize(COUNT = sum(COUNT)) |> 
  ungroup()

#pivot wider --> gender (M/F) receive individual columns
names_df <- names_df |>
  pivot_wider(names_from = GENDER, values_from = COUNT)

#set NA's to 0 and calculate percentage of each Name which was given to Male individuals
names_df[is.na(names_df)] <- 0
names_df$prob_M <- names_df$M / (names_df$M + names_df$F)

#determine a name to be Male if it is given to Male in >75% of the cases,
#as female if it given to Male with <25% cases.
#all other cases, gender == NA
names_df$GENDER <- NA
names_df$GENDER[names_df$prob_M >= 0.75] <- "M"
names_df$GENDER[names_df$prob_M <= 0.25] <- "F"

#convert names to Upper for further analysis
names_df$NAME <- toupper(names_df$NAME)


#get names and abbreviations of all US States including territories (eg. DC!) 
#---------------------------------------------------------------------------

#import HTML from website
states_link <- "http://www.50states.com/abbreviations.htm"
states_page <- readLines(states_link, encoding = "UFT-8", warn = F) |> htmlParse() 

#retreive node where table with states is stored and convert to data frame
states <- getNodeSet(states_page, "//table[@class =' table table-hover']")
states <- readHTMLTable(states[[1]])

states <- states |>
  select(c("US STATE", "POSTAL ABBREVIATION")) |>
  rename(STATE = "US STATE", ABBREVIATION = "POSTAL ABBREVIATION")

#retreive node where table with territories is stored and convert to data frame
territories <- getNodeSet(states_page, "//table[@class = 'has-fixed-layout table table-hover']")
territories <- readHTMLTable(territories[[1]])

territories <- territories |>
  select(c("Territory/Commonwealth", "Postal Abbreviation")) |>
  rename(STATE = "Territory/Commonwealth", ABBREVIATION = "Postal Abbreviation")

#combine do single Data Frame
states_df <- bind_rows(states, territories)

#write files as CSV for further usage
write.csv(states_df, "files/us_states.csv", row.names = F)


#get data on political ideology for each US State
#---------------------------------------------------------------------------
pol_ideology_link <- "https://www.pewresearch.org/religion/religious-landscape-study/compare/political-ideology/by/state/"
pol_ideology_page <- readLines(pol_ideology_link, encoding = "UFT-8", warn = F) |> htmlParse() 

pol_ideology <- getNodeSet(pol_ideology_page, "//table[@class ='ui	celled table']")
pol_ideology <- readHTMLTable(pol_ideology[[1]])
pol_ideology[,2:5] <- data.frame(sapply(pol_ideology[,2:5], function(x) gsub("%", replacement = "", x)))
pol_ideology[,2:5] <- data.frame(sapply(pol_ideology[,2:5], function(x) as.numeric(x)))
pol_ideology <- pol_ideology |> 
  select(State, Conservative, Moderate, Liberal)


#clean donation data
#---------------------------------------------------------------------------

#get paths for all donation files
donations <- data.frame()
donation_files <- list.files("files/donations")
donation_files <- donation_files[!donation_files == "header_file.csv"]

#loop through all donation files, filter out donations for Biden & Trump 
#& paste together

for (i in donation_files){
  
  temp <- read_delim(paste0("files/donations/", i), col_names = F)
  
  #filter out only donations which were made to Biden and Trump Fundraising Committee
  biden_trump <-temp |> 
    filter(X16 %in% c("C00703975", "C00580100") & ENTITY_TP == "IND")
  donations <- bind_rows(donations, biden_trump)
  
}

#import file provided by the FEC which contains the column names (were not provided in the donation files)
col_names_donations <- read_csv("files/donations/header_file.csv", col_names = F)
colnames(donations) <- col_names_donations[1,]

#Extract first and last names from NAME column 
names <- donations$NAME

#remove white-space at the beginning 
names <- gsub(pattern = "^\\s+", replacement = "", names)

#take all until the first comma or white-space (only first part of double name is taken since our file only contains one name names)
last_name <- str_extract(names, pattern = "[^,|\\s]+")
last_name <- gsub(pattern = "[[:punct:]]", replacement = "", last_name)

first_name <-  str_split(names, pattern = ",", simplify = T)[,2]

#split by space and select longer name 
first_name <- sapply(str_split(first_name, " "), function(x) x[which.max(nchar(x))])
#split by . and select longer name 
first_name <- sapply(str_split(first_name, "\\."), function(x) x[which.max(nchar(x))])
#remove possible whitespace
first_name <- gsub(pattern = "\\s", replacement = "", first_name)


###consolidated vs not consolidated...
donations$LAST_NAME <- last_name
donations$FIRST_NAME <- first_name
donations$CANDIDATE <- ifelse(donations$OTHER_ID == "C00703975", "BIDEN", "TRUMP")

donations <- donations |> 
  select(c(FIRST_NAME, LAST_NAME, TRANSACTION_AMT, CITY, STATE, ZIP_CODE, EMPLOYER, OCCUPATION, TRANSACTION_DT, CANDIDATE))

donations <- donations |>  relocate(FIRST_NAME, LAST_NAME, TRANSACTION_DT, TRANSACTION_AMT)

donations <- left_join(donations, names_df[,c("NAME", "GENDER")], by = c("FIRST_NAME" = "NAME"))
donations <- donations |>  relocate(FIRST_NAME, LAST_NAME, GENDER)
donations <- donations |>  mutate(TRANSACTION_DT = as.Date(TRANSACTION_DT, format = "%m%d%Y"))
donations <- donations |> mutate(STATE = ifelse(STATE %in% states_df$ABBREVIATION, STATE, NA))

trump_cleaned <- donations |>  filter(CANDIDATE == "TRUMP")
biden_cleaned <- donations |>  filter(CANDIDATE == "BIDEN")

#store data frames for further analysis!
write.csv(donations, "files/donations_cleaned.csv", row.names = F)
write.csv(biden_cleaned, "files/biden_cleaned.csv", row.names = F)
write.csv(trump_cleaned, "files/trump_cleaned.csv", row.names = F)
