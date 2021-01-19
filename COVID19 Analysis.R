#This is covid19 data and I will be analyzing for my first project in DataQuest. 

#Used readr to read the csv file COVID19 data from kaggle from jan 20, 2020 to june, 1, 2020 
covid_df<-read_csv("covid19.csv")
dim(covid19)
#took the names of the columns and stored in vector
vector_cols <- colnames(covid19)
#took a glimpse at data
glimpse(covid_df)
#filtered province_state by all state and got rid of column. We can keep info by deleting column because it is a column which we filtered to be uniform
covid_df_all_states <- covid_df %>% 
  filter(Province_State == "All States") %>% 
  select(-Province_State)

#separating the daily activities of countries. 
covid_df_all_states_daily <- covid_df_all_states %>% 
  select(Date, Country_Region, active, hospitalizedCurr, daily_tested, daily_positive)
#grouping by country and daily sum 
covid_df_all_states_daily_sum<-covid_df_all_states_daily %>% 
  group_by(Country_Region) %>% 
  summarise(tested = sum(daily_tested), 
            positive = sum(daily_positive), 
            active = sum(active), 
            hospitalized = sum(hospitalizedCurr)) %>% 
  arrange(-tested)
#head of sum of daily 
covid_top_10 <- head(covid_df_all_states_daily_sum , 10)

#renamed columns for top ten

countries <- covid_top_10$Country_Region
tested_cases <- covid_top_10$tested
positive_cases <- covid_top_10$positive
active_cases <- covid_top_10$active
hospitalized_cases <- covid_top_10$hospitalized

#renamed columns in countries df 
names(tested_cases) <- countries 
names(positive_cases) <-countries  
names(active_cases) <- countries
names(hospitalized_cases) <- countries

#top 10 ratio 

ratio <- positive_cases/tested_cases

#top 10 ratio sorted by decreasing 
ratio1<-sort(ratio, decreasing = TRUE)

#top 3 countries who have the most positive tested cases
positive_tested_top_3 <- head(ratio1, 3)
#reading data
positive_tested_top_3

#rewriting the top 3 countries here. 
united_kingdom <-c(0.11, 1473672, 166909, 0, 0) 

united_states <- c(0.10, 17282363, 1877179, 0, 0)

turkey <- c(0.08, 2031192, 163941, 2980960, 0)

#creating a matrix 
covid_mat <- rbind(united_kingdom, united_states, turkey)

#naming the columns in matrix 
colnames(covid_mat) <- c("Ratio", "tested", "positive", "active", "hospitalized")
#printing matrix 
covid_mat


#answering dataquest questions
question <- "Which countries have had the highest number of positive cases against the number of tests?"
answer <- c("Positive tested cases" = positive_tested_top_3)

#collecting all dataframes, vectors and matrices and putting them in a list. 
dataframes <- list(covid_df, covid_df_all_states, covid_df_all_states_daily, covid_df_all_states_daily_sum, covid_top_10)
matrices <- list(covid_mat)
vectors <- list(active_cases, countries, hospitalized_cases, positive_cases, positive_tested_top_3,united_kingdom, united_states, turkey, tested_cases, vector_cols, ratio)
#naming list 
data_structure_list <- list( dataframes = dataframes, matrices = matrices, vectors = vectors)

#creating final list

covid_analysis_list <- list(question, answer, data_structure_list)
#answering final question for Dataquest
covid_analysis_list[[2]]

