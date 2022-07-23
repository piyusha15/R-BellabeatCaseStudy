

#Installing and loading the necessary packages 
#install.packages("tidyverse")
#install.packages("sqldf") # I will be using a bit of SQL to query data 

#install.packages("proto")   #this and the next two packages are for using sql
#install.packages("RSQLite")
#install.packages("gsubfun")

library(tidyverse)
library(readr)
library(sqldf)
library(dplyr)

#Importing the data 


activity<- read.csv("dailyActivity_merged.csv")

steps <- read.csv("dailySteps_merged.csv")

intensity <- read.csv("dailyIntensities_merged.csv")

weight <- read.csv("weightLogInfo_merged.csv")

calories <- read.csv("dailyCalories_merged.csv")

sleep <- read.csv("sleepDay_merged.csv")

#Checking the structure of imported data 
str(activity)
str(steps)
str(intensity)
str(sleep)


library(dplyr)

# n_distinct() function from dplyr package can also be used to find distinct IDs in dataframes

n_distinct(activity$Id)
n_distinct(steps$Id)
n_distinct(intensity$Id)
n_distinct(weight$Id)
n_distinct(sleep$Id)





# On viewing all imported data frames, we observe that the data frames "calories" and "intensity" may be subsets of activity. We use SQL to find out if that is true. 

#Following code performs leftjoin of calories and intensities table on activity table and returns the number of rows in the result. 

sqldf("SELECT COUNT()
      FROM activity 
      LEFT JOIN calories ON 
      activity.Id = calories.Id AND 
      activity.ActivityDate = calories.ActivityDay AND 
      activity.Calories = calories.Calories")

sqldf("SELECT COUNT()
      FROM activity 
      LEFT JOIN steps  ON 
      activity.Id = steps.Id AND 
      activity.ActivityDate = steps.ActivityDay AND 
      activity.Totalsteps = steps.StepTotal")

sqldf("SELECT COUNT()
      FROM activity 
      LEFT JOIN intensity  ON 
      activity.Id = intensity.Id AND 
      activity.ActivityDate = intensity.ActivityDay AND 
      activity.SedentaryMinutes = intensity.SedentaryMinutes AND
      activity.LightlyActiveMinutes = intensity.LightlyActiveMinutes AND
      activity.FairlyActiveMinutes = intensity.FairlyActiveMinutes AND
      activity.VeryActiveMinutes = intensity.VeryActiveMinutes AND
      activity.SedentaryActiveDistance = intensity.SedentaryActiveDistance AND
      activity.LightActiveDistance = intensity.LightActiveDistance AND
      activity.ModeratelyActiveDistance = intensity.ModeratelyActiveDistance AND
      activity.VeryActiveDistance = intensity.VeryActiveDistance")


rm(calories,intensity,steps)

# Just checking to make sure they've been removed 
colnames(activity)







#Importing the data 

activity<- read.csv("dailyActivity_merged.csv")

steps <- read.csv("dailySteps_merged.csv")

intensity <- read.csv("dailyIntensities_merged.csv")

weight <- read.csv("weightLogInfo_merged.csv")

calories <- read.csv("dailyCalories_merged.csv")

sleep <- read.csv("sleepDay_merged.csv")








# We begin by renaming columns and chaning the format of date from char to 'date' 

library(lubridate)  

activity <- activity %>% 
  rename(date= ActivityDate) %>%
  mutate(date= mdy(date)) # as_date() must be used with mutate to convert char to date format 

sleep <- sleep %>%
  rename(date= SleepDay) %>%
  mutate(date= as_date(date, format= "%m/%d/%Y  %I:%M:%S %p"))

intensity <- intensity %>% 
  rename(date= ActivityDay) %>% 
  mutate(date = mdy(date))

# Finding the number of duplicates and removing if they exist

sum(duplicated(activity))
sum(duplicated(sleep))
sum(duplicated(intensity))


sleep <- distinct(sleep)

# Checking if all duplicates are removed 

sum(duplicated(sleep))

final_activity <- merge(activity,sleep, by = c("Id","date"), all.x = TRUE)  # all.x = TRUE is used because we know that sleep has data for fewer users than total.





 
 
 
 
 
 

#Analyse Phase 




summary(final_activity)




usage_df <- sqldf("select Id, LightlyActiveMinutes,SedentaryMinutes,         FairlyActiveMinutes,VeryActiveMinutes
                  from activity ")


usage_df <- usage_df %>% 
  mutate(total_active_mins =LightlyActiveMinutes+FairlyActiveMinutes+VeryActiveMinutes, 
         total_mins= total_active_mins + SedentaryMinutes
         )

usage_df <- sqldf("select Id, 
  avg(total_active_mins) as avg_total_mins
                  from usage_df 
                  group by Id ")

 mean(usage_df$avg_total_mins) # = 225.090

 quantile(usage_df$avg_total_mins, probs = 0.9) #=312.6194

# We use quantile() function to find the 90th percentile of avg_total_mins i.e average of active minutes for data available for each user.  


# User data by active minutes. We will later use this calculation to plot a pie chart to visualise the findings for the stakeholders. 
 
user_activity_type <- sqldf("select Id, avg_total_mins, 

                    case
                    
                    when avg_total_mins < 225.090 then 'low activity'
                    
                    when avg_total_mins > 225.090 and avg_total_mins < 312.6194                                      then 'medium activity'
                    
                    when avg_total_mins > 312.6194 then 'high activity'
                    end as user_activity_type 
                    
                    from usage_df 
                    
                    group by Id
                    order by user_activity_type")


# Now we will find user data by logging activity. We will find how many entries were made by each user in the given time period . This will enable us to see how much the customers use the device - we will sort users by usage_type ( fairly active, not active, very active ).

usage_type <- activity %>% 
  select(Id) %>%
  group_by(Id) %>% 
  summarise(num_of_entries = n()) %>%
  mutate(usage_type = case_when(
    num_of_entries >= 1 & num_of_entries <= 21 ~ "low usage",
    num_of_entries >= 22 & num_of_entries <= 28 ~ "moderate usage", 
    num_of_entries >= 28    ~ "high usage")) %>% 
  arrange(usage_type)

# value of 10th percentile of num_of_entries data is 21.20 and average of values is 28.48 Hence the choice of num_of_entries to segregate users. 

# Further, we can create pie charts to illustrate the distribution of users by activity and usage. 

# Chart to show the distribution of users by usage.

## First we'll have to create a usage_dist data frame to store the usage data in a format that is appropriate for creation of pie charts. 

usage_dist <- usage_type %>% 
  group_by(usage_type) %>%
  summarise(users = n_distinct(Id)) %>% 
  mutate(percent_dist = (users/sum(users))*100) %>% 
  arrange(percent_dist)

## Now create a plot

install.packages("ggplot2")
library(ggplot2)

ypos = cumsum(usage_dist$percent_dist) - 0.5 * (usage_dist$percent_dist)


usage_pie <- ggplot( data = usage_dist, aes(x = "", y = percent_dist, fill = usage_type)) + 
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start= 0) +
  geom_text(aes(x = "", y = ypos, label = paste0(round(percent_dist), "%")),     size = 3)+
  labs(title="Distribution of users by usage", caption = " Usage is calculated according to the number of entries made by users") + 
  theme(
      panel.background = element_blank(),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks =element_blank())+
      guides(fill = guide_legend(title = "Usage level")) +
      scale_fill_manual(values=c("purple1", "purple4", "purple3"))
      
usage_pie

# Transform activity data into a form from which pie chart can be created.
activity_dist <- user_activity_type %>% 
  group_by(user_activity_type) %>%
  summarise(users = n_distinct(Id)) %>% 
  mutate(activity_dist = (users/sum(users))*100) %>% 
  arrange(activity_dist)

ypos2 = cumsum(activity_dist$activity_dist) - 0.5*(activity_dist$activity_dist)

ypos2 = 100- ypos2

activity_pie <- ggplot( data = activity_dist, aes(x = "", y = activity_dist, fill = user_activity_type)) + 
  geom_bar(stat="identity", width=1, color="black") +
  coord_polar("y", start= 0) +
  geom_text(aes(x = "", y = ypos2, label = paste0(round(activity_dist), "%")),     size = 3)+
  labs(title="Distribution of users by activity", caption = "Activity type is calculated as the average of total activity of each user each day") + 
  theme(
      panel.background = element_blank(),
      plot.caption = element_text(angle = 0, vjust = 5, size = 8),
      axis.line = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank(),
      axis.ticks =element_blank())+
      guides(fill = guide_legend(title = "Activity level")) +
      scale_fill_manual(values=c("yellow1", "yellow4", "yellow3"))
      
activity_pie



daily_data <- left_join(activity,sleep, by = c("Id", "date"))

calories_per_user <- sqldf("select Id, 
                           avg(calories) as avg_calories_pd
                           from calories 
                           group by Id")

activity_calories <- left_join(user_activity_type,calories_per_user, by = c("Id"))



# Now we will use the above data frame to plot calories burnt according to usertype. 

activity_calories_plot<- ggplot(data = activity_calories, aes(avg_calories_pd,user_activity_type, fill = user_activity_type))+
  geom_point(stat = "identity")+
  geom_boxplot(stat = "boxplot")+
  scale_fill_manual(values=c("peachpuff1", "pink3", "plum2"))
  

activity_calories_plot


#plotting steps taken by users pper weekday. 
# We will use the daily_data table to create another daily_data_weekdays table for this analysis. 

daily_data_weekdays <- daily_data %>% 
  mutate(weekday = weekdays(date)) %>%
  group_by(weekday) %>% 
  summarize(average_steps= mean(TotalSteps))

# Now we use ggplot2 to plot weekdays Vs steps 

weekdays_steps <- ggplot(daily_data_weekdays, aes(x=weekday,y=average_steps))+ geom_bar(stat = "identity", color= "green", fill = "steelblue") + theme_minimal() + geom_text(aes(label= round(average_steps)), vjust=1.6, color="white", size=3.5)+
labs(title="Steps per weekday", x="Weekday", y = "Steps")

weekdays_steps


activity_cal_usage <- left_join(activity_calories,usage_type, by = "Id")

# Now we isolate only the required columns from activity_cal_usage using sql 

activity_cal_usage <- sqldf("select id, avg_total_mins, usage_type,user_activity_type, avg_calories_pd
                            from activity_cal_usage")

# Now we can plot to find the relationship between relationship between activity, calories burned, and user type



activity_cal_usage_plot<-  ggplot(activity_cal_usage , aes(avg_total_mins  , avg_calories_pd,colour = user_activity_type))+geom_point()+
  facet_wrap(~usage_type)+ 
  theme(plot.title = element_text(size = 10),
        plot.subtitle = element_text(size = 8),
          plot.caption = element_text(size = 8),
          axis.title  = element_text(size = 8),
          axis.text.x = element_text(size = 8),
          axis.text.y  = element_text(size = 8))+
  xlab("Minutes Active")+
  ylab("Calories Burned")+
  labs(titles = "Relationship between calories burnt, usage type and   activity type ") + 
  scale_color_manual(values=c("red","yellow", "orange"))
                     
activity_cal_usage_plot











