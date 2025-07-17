#title: "Lesson 3: Workflow and Statistical Test in R"
#author: Eden Burnett
#date: 7/17/2025

#load packages
library(arrow) #reading parquet files
library(tidyverse) #data manipulation & visualization


#load data
merged_data <- read_parquet(file = "merged_data.parquet", stringsAsFactors = TRUE)

#visualize with a box plot

boxplot(formula = upward_mobility_rate_2010 ~ STATE_NAME_2010SVI, data = merged_data)

# get rid of anaomolies (what state is 06?)
merged_data <- merged_data[!(merged_data$STATE_NAME_2010SVI %in% "06"), ]

#replot box plot
boxplot(formula = upward_mobility_rate_2010 ~ STATE_NAME_2010SVI, data = merged_data)

#new boxplot of population by state
boxplot(formula = M_TOTPOP_2010SVI ~ STATE_NAME_2010SVI, data = merged_data)

#install and load tidyverse
install.packages("tidyverse")
library(tidyverse)

#subset data again
subset_data_ca <- subset(merged_data, STATE_ABBR_2010SVI == "CA")
subset_data_az <- subset(merged_data, STATE_ABBR_2010SVI == "AZ")

#print means
print(upward_mean_2010_ca <- mean(subset_data_ca$upward_mobility_rate_2010))
print(upward_mean_2010_az <- mean(subset_data_az$upward_mobility_rate_2010))

#store upward mobility means
upward_mobility_means_2010 <- data.frame(State = c("CA", "AZ"),
                                         up_2010_mean = c (upward_mean_2010_ca, upward_mean_2010_az))


#group full data set by state abbrv & calculate the mean upward mobility rate for each group
state_group <- merged_data %>%
  group_by(STATE_ABBR_2010SVI)

state_mob_means <- state_group %>%
  summarise(up_mean = mean(upward_mobility_rate_2010, na.rm = TRUE))

#remove NA values for state abbreviation
state_mob_means <- state_mob_means %>%
  filter(!is.na(STATE_ABBR_2010SVI))

#use piping (%>%)
upward <- merged_data %>%
  group_by(STATE_ABBR_2010SVI) %>%
  summarise(means_2010 = mean(upward_mobility_rate_2010, na.rm = TRUE))

upward <- upward %>% filter(!is.na(STATE_ABBR_2010SVI))

#visualize means and standard errors, grouping by both state and country

upward_stats <- merged_data %>%
  group_by(STATE_ABBR_2010SVI, COUNTY_2010SVI) %>%
  summarise(up_means = mean(upward_mobility_rate_2010, na.rm = TRUE),
            up_se = sd(upward_mobility_rate_2010, na.rm = TRUE)/sqrt(n()))
#drop the NA
upward_stats <- upward_stats %>% filter (!is.na(STATE_ABBR_2010SVI))

#plot with error bars with ggplot
ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means))

#need to tell R what type of plot tho > using means as points so use geom_point()
ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point()

#add error bars
ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point() + 
  geom_errorbar(mapping = aes(ymin = up_means - up_se,
                              ymax = up_means + up_se))
#make error bars thinner
ggplot(data = upward_stats, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point() + 
  geom_errorbar(mapping = aes(ymin = up_means - up_se,
                              ymax = up_means + up_se), 
                width = 0.30)

#clean up and summarize at state level to be better visually
upward_stats_st <- merged_data %>%
  group_by(STATE_ABBR_2010SVI) %>%
  summarize(up_means = mean(upward_mobility_rate_2010),
            up_se = sd(upward_mobility_rate_2010)/sqrt(n()))
#drop the NA again
upward_stats_st <- upward_stats_st %>% filter(!is.na(STATE_ABBR_2010SVI))

#redo the graph to be prettier
ggplot(data = upward_stats_st, mapping = aes(x = STATE_ABBR_2010SVI, y = up_means)) +
  geom_point() + 
  geom_errorbar(mapping = aes(ymin = up_means - up_se,
                              ymax = up_means + up_se), 
                width = 0.30)

#critical thinking with data: who? Why? limitations? biases?

#create a working copy of your dataset. merged is the editable copy now
merged <- merged_data


#cool things you can do in tidyverse in R

#select removes columns with underscores and keeps those that start with "upward"
merged %>% 
  dplyr:: select(!contains("_"), starts_with ("upward"))

#reorder with relocate function columns to put all the columns containing "STATE" after the 2020 upward mobility coluumn
merged <- merged %>%
  dplyr:: relocate(contains("STATE"), .after = upward_mobility_rate_2020)

#now have tidy unique ID column in clean spot
unique_id <- merged %>%
  dplyr:: group_by(STATE_FIPS_2010SVI, COUNTY_2010SVI, CENSUSAREA_2010SVI) %>%
  dplyr:: mutate(uniqueid = row_number(), .before = contains("_"))

#pivot to switch between wide and long formats easily
#general syntax even tho it doesn't apply to our data rn
#Wide to long
# merged_long <- unique_id %>%
#tidyr::pivot_longer(contains("_"),
#names_to = c("upward"),
#names_sep =("_"))


#this is general synthax, but doesn't make sense for what we're doing right now

# Long to wide

# merged_wide <- _merged_long %>%
#tidyr::pivot_wider(names_from = c("upward),
#values_from = "value",
#names_sep = "_")


#again, as we didn't change, muted code


#Summarize across multiple columns
merged_stats_up <- merged %>% 
  dplyr::group_by(STATE_ABBR_2010SVI) %>% 
  dplyr::summarize(across(starts_with("upward"),
                          list(~mean(.x, na.rm = TRUE), 
                               ~sd(.x, na.rm = TRUE))))

#rename columns when summarizing
merged_stats_up <- merged %>% 
  dplyr::group_by(STATE_ABBR_2010SVI) %>% 
  dplyr::summarize(across(starts_with("upward"), 
                          list(mean = ~mean(.x, na.rm = TRUE), 
                               sd = ~sd(.x, na.rm = TRUE)),
                          .names = "{gsub('_', '', col)}_{fn}")) 

#run models within groups
upward_models <- merged %>%
  filter(!is.na(STATE_ABBR_2010SVI), 
         !is.na(upward_mobility_rate_2010),
         !is.na(POP2010)) %>%
  group_by(STATE_ABBR_2010SVI) %>% 
  summarise(model = list(lm(upward_mobility_rate_2010 ~ POP2010)))

#nest the data
merged <- nest_by(state_group)


#Running a basic Statistical Test: Students t-test
#reset merged data
merged <- merged_data

#T-Test
#are these two groups different enough that its unlikely it happened by chance?
#compare upward mobility between AZ and CA

az <- merged_data[merged_data$STATE_NAME_2010SVI == "Arizona", ]
ca <- merged_data[merged_data$STATE_NAME_2010SVI == "California",]

#see how many rows and columns are in a data.frame with the dim() command
dim(merged_data)

#return all columns for the third row in merged_data:
merged_data[3,]

#check how many observations we got in each
print(nrow(az))
print(nrow(ca))

#test is upward mobility is significantly different with t test
t.test(x=az$upward_mobility_rate_2010, y = ca$upward_mobility_rate_2010)
#fail to reject the null hypothesis

#differneces in mobility among counties?
aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = az)

#mobility rate explained by county
mobility_rate_az_aov <- aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = az)

#check out what we got
summary(object = mobility_rate_az_aov)
#big finding that ... AZ counties have significantly different mobility rates
#save this files as a sink

sink(file = "output/az_mobility_anova.txt")
summary(object = mobility_rate_az_aov)
sink()

#starting linear regression
#look at data
summary(merged_data[, 1:10], 6) 

#low key crazy so lets vizualize instead
plot(x = merged_data$upward_mobility_rate_2010, y = merged_data$M_TOTPOP_2010SVI)

#apply a log transformation
merged$logpop <- log10(merged$M_TOTPOP_2010SVI)

#plot again with labeled axis
plot(x = merged$upward_mobility_rate_2010, 
     y = merged$logpop, 
     xlab = "Upward Mobility", 
     ylab = "log10(Population)")

#lets run a regression
mobility_v_pop <- lm(upward_mobility_rate_2010 ~ logpop, data = merged)

# let’s look at the results:
summary(mobility_v_pop)

#for every 10x increase in population upward mobility goes up by about 1.05 units
#save the results to a file
sink(file = "output/mobility-pop-regression.txt")
summary(mobility_v_pop)
sink()

#add a state: compare AZ and CA, add a dummy variable (yes or no)
merged$az <- ifelse(merged$STATE_NAME_2010SVI == "Arizona", 1, 0)
merged$ca <- ifelse(merged$STATE_NAME_2010SVI == "California", 1, 0)

#add AZ and predictor and run the new model
mobility_v_pop_state <- lm(formula = upward_mobility_rate_2010 ~ logpop + az
                           , data = merged)
summary(mobility_v_pop_state)

#lets save it
sink(file = "output/mobility-pop-state-regression.txt")
summary(mobility_v_pop)
sink()


#STUDENT ACTIVITIES


#PART 1
#plot age 65 people by census track box plot
boxplot(formula = AGE65_2010SVI ~ STATE_NAME_2010SVI, data = merged_data)

#show that there are more crazy outliers of census tracts in CA with high number of 65+ age people


#PART 2
#run anova analysis of AZ but instead of county by age 65+
mobility_rate_az_aov_age65 <- aov(formula = upward_mobility_rate_2010 ~ AGE65_2010SVI, data = az)
summary(mobility_rate_az_aov_age65)
#p-value is low so it is significant??

#now county for CA
mobility_rate_ca_aov <- aov(formula = upward_mobility_rate_2010 ~ COUNTY_2010SVI, data = ca)
summary(mobility_rate_ca_aov)
#similar p value


#PART 3
mobility_v_pop_state3 <- lm(formula = upward_mobility_rate_2010 ~ logpop + az + AGE65_2010SVI
                           , data = merged)
summary(mobility_v_pop_state3)

#PART 4 
#I feel like I need more help understanding the summaries and what the results mean, how to tell if they are statistically significant, etc. 
#Need more practice with creating more pretty graphs and stuff. 
