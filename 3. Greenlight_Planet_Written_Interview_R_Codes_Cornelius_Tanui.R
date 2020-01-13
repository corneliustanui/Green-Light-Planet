# 0) DIRECTORY AND SETTINGS  ----

# if(dir.exists("C:\Users\User\Desktop\GreenLight Planet\Green_Light_Planet")){
#   setwd("C:\Users\User\Desktop\GreenLight Planet\Green_Light_Planet")
# }else{
#   dir.create("C:\Users\User\Desktop\GreenLight Planet\Green_Light_Planet")
#   setwd("C:\Users\User\Desktop\GreenLight Planet\Green_Light_Planet")
#   dir()
# }  

rm(list = ls(all.names = TRUE)) # Clear the environment
options(digits = 5)             # Use 5 significant digits for output
dev.off()                       # Shut down unnecessary graphical devices

# 1) LOAD USEFUL PACKAGES ----
library(dplyr)          # For data management
library(tidyr)          # For data management
library(stringr)        # For string handling
library(ggplot2)        # For data visualisation
library(ggpubr)         # For data visualisation
library(tseries)        # For Jarque-Bera Test of normality for correlated data
library(rstatix)        # Basic statistical tests
library(gmodels)        # Basic statistical tests
library(esquisse) 

# 2) LOAD DATA ----
Data <- read.csv("Statistician Case Study - Data.csv", header = TRUE)

View(Data)

names(Data)

# 3) DATA MANAGEMENT AND CLEANING ----
# 3.i) Recoding categorical variables

disabled_values <- c("Yes" = 1, "No" = 2)

Data <- within(Data, {
  
  # Put values
  is_disabled_w2 <- recode(is_disabled_w2, !!!disabled_values)
  is_disabled_w4 <- recode(is_disabled_w4, !!!disabled_values)
  is_disabled_w6 <- recode(is_disabled_w6, !!!disabled_values)
  is_disabled_w8 <- recode(is_disabled_w8, !!!disabled_values)
  is_disabled_w10 <- recode(is_disabled_w10, !!!disabled_values)
  
  #Put value labels
  is_disabled_w2 <- factor(is_disabled_w2, 
                           levels = c(1,2),
                           labels = c("Yes", "No"))
  is_disabled_w4 <- factor(is_disabled_w4,
                           levels = c(1,2),
                           labels = c("Yes", "No"))
  is_disabled_w6 <- factor(is_disabled_w6,
                           levels = c(1,2),
                           labels = c("Yes", "No"))
  is_disabled_w8 <- factor(is_disabled_w8,
                           levels = c(1,2),
                           labels = c("Yes", "No"))
  is_disabled_w10 <- factor(is_disabled_w10,
                            levels = c(1,2),
                            labels = c("Yes", "No"))})

# 3.ii) Confirm that:-
# a) treatments in Country 1 are 1350,
# b) non-treatments in Country 1 are 3838
# c) non-treatments in Country 2 are 45086

Data %>% select(country, is_treatment) %>% 
  group_by(country, is_treatment) %>% 
  summarise(Counts = n()) %>%  # Clearly, the data is accurate as per the descriptions. 

# 3.iii) Confirm that there is no duplicated Account ID.

any(duplicated(Data$acccunt_id)) # Again, all IDs are unique.

# 3.iv) Reorganise columns and Split data into two
Data1 <- Data %>% select("is_treatment", "country", "acccunt_id",
                "FRR_w2", "FRR_w4", "FRR_w6", "FRR_w8", "FRR_w10")
               # "is_disabled_w2", "is_disabled_w4", "is_disabled_w6", "is_disabled_w8", "is_disabled_w10")

Data2 <- Data %>% select("is_treatment", "country", "acccunt_id",
                         "is_disabled_w2", "is_disabled_w4", "is_disabled_w6", "is_disabled_w8", "is_disabled_w10")

# 3.vi) Reshape the data from wide to long format to generate "week" and end up with only 6 variables
Long <- Data1 %>% 
  gather(week, FRR, FRR_w2:FRR_w10) %>% 
  arrange(acccunt_id)

Long2 <- Data2 %>%
  gather(week_n, is_disabled, is_disabled_w2:is_disabled_w10) %>% 
  arrange(acccunt_id)

# Rejoin the "is_disabled" vriable
Long$is_disabled <- Long2$is_disabled

View(Long)

# Covert FRR to numeric
Long$FRR <- as.numeric(Long$FRR)

# Confirm that there is no missing data
View(Long[is.na(Long$FRR), ]) # There are 25 rows of missing data for FRR and is_disabled

Long$FRR <- ifelse(is.na(Long$FRR), 
                   mean(Long$FRR, na.rm = TRUE), # Impute missing FRR with the mean of availabe data
                   Long$FRR) 

any(is.na(Long$FRR)) # No more missing FRR

# Impute missing is_disabled
is.factor(Long$is_disabled)

View(Long[is.na(Long$is_disabled), ]) 

table(Long$is_disabled) # "No" is the mode

Long$is_disabled <- ifelse(is.na(Long$is_disabled), 
                           "No", # Impute missing is_disabled with the No
                           Long$is_disabled)

# Clean the "week" variable
str_sub(Long$week, 1, 4) <- ""

week_values <- c("w2" = 1, "w4" = 2, "w6" = 3, "w8"= 4, "w10" = 5)

Long <- within(Long, {
  
  # Put values
  week <- recode(week, !!!week_values)
  is_disabled <- recode(is_disabled, !!!disabled_values)

  #Put value labels
  week <- factor(week, 
                 levels = 1:5, 
                 labels = c("w2", "w4", "w6", "w8", "w10"))
  is_disabled <- factor(is_disabled,
                        levels = c(1,2),
                        labels = c("Yes", "No"))})

# 4) DESCRIPTIVE ANALYSES FOR FRR ----
# (Follow-on Revenue Realization)

FRR <- Long %>% 
  group_by(is_treatment, country, week) %>% # Group by required variables
  summarise(Sum = sum(FRR),
            Mean_FRR = mean(FRR),
            StdDev = sd(FRR))

write.csv(FRR, file = "FRR_Stats.csv")

# Country
FRR_country <- Long %>% 
  group_by(country) %>% # Group by country 
  summarise(Sum = sum(FRR),
            Mean_FRR = mean(FRR),
            StdDev = sd(FRR))

write.csv(FRR_country, file = "FRR_country.csv")

# Week
FRR_week <- Long %>% 
  group_by(week) %>% # Group by week 
  summarise(Sum = sum(FRR),
            Mean_FRR = mean(FRR),
            StdDev = sd(FRR))

write.csv(FRR_week, file = "FRR_week.csv")

# treatment
FRR_treatment <- Long %>% 
  group_by(is_treatment) %>% # Group by week 
  summarise(Sum = sum(FRR),
            Mean_FRR = mean(FRR),
            StdDev = sd(FRR))

write.csv(FRR_treatment, file = "FRR_treatment.csv")

# 5) FRR VISUALIZATION ---- 

Task_1 <- Long %>% 
  group_by(is_treatment, country, week, is_disabled) %>%  # Group by required variables
  summarise(AvrFRR = mean(FRR)) %>%                       # Record frequencies of disabled
  filter(is_disabled == "Yes") %>%                        # Keep disabled only
  select(-is_disabled)                                    # Drop unecessary variable   

# Plot the bar graph
Task_1 %>%  
  ggplot() +                      
  aes(x = week, fill = is_treatment, 
      y = AvrFRR,
      label = round(AvrFRR, 3)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  geom_text(size = 3, 
            hjust = 0.5, 
            vjust = -1, 
            position = position_dodge(width = 0.9))+
  scale_fill_manual(values=c("gold", "black")) +
  labs(x = "Week", 
       y = "FRR", 
       title = "BAR GRAPH OF FOLLOW-ON-REVENUE-REALIZATION (FRR)", 
       subtitle = "The evolution over time of the FRR for Country 1 with/without treatment and Country 2", 
       fill = "Treatment") +
  theme(plot.title = element_text(size = 12, 
                                  hjust = 0.5, 
                                  vjust = 1),
        plot.subtitle = element_text(size = 12, 
                                     hjust = 0.5, 
                                     face = "italic", 
                                     colour = "red", 
                                     vjust = -1)) +
  theme_classic() +
  facet_wrap(vars(country))


# 6) CHECKING ASSUMPTIONS FOR REPEATED MEASURES ANOVA FOR FRR----
# 6.a) DATA IS NORMALLY DISTRIBUTED (This assumption is not met)
jarque.bera.test(Long$FRR)  # Normal if p-value > 0.05 (We see that FRR is not normal)

# 6.b) THERE ARE NO OUTLIERS (This assumption is not met)
Long %>%
  group_by(is_treatment, week, country) %>%
  identify_outliers(FRR)

# 6.c) THERE IS NO SPHERICITY (Mauchly's test is employed internally and will be corrected using Greenhouse-Geisser correction)
# Due to repeatedness of the FRR 5 times in an interval of 2 weeks, we shall perform repeated measures one- and two-way ANOVA

# 7.A) ONE-WAY REPEATED MEASURES ANOVA FOR FRR ----
# FOLLOW-ON-REVENUE REALIZATION
# 1) week
OWA_FRR_Week <- anova_test(data = Long, 
                           dv = FRR, 
                           wid = acccunt_id, 
                           within = week)
get_anova_table(OWA_FRR_Week)

# POST-HOC TESTS
Pwc <- Long %>%       # Pairwise comparisons
  pairwise_t_test(FRR ~ week,
                  paired = TRUE,
                  p.adjust.method = "bonferroni")

# 2) country
# t.test(Long$FRR ~ Long$country, paired = FALSE)

OWA_FRR_Country <- anova_test(data = Long, 
                              dv = FRR, 
                              wid = acccunt_id, 
                              within = country)
get_anova_table(OWA_FRR_Country)

# POST-HOC TESTS
Long %>%       # Pairwise comparisons
  pairwise_t_test(FRR ~ country,
                  paired = TRUE,
                  p.adjust.method = "bonferroni")

# 3) is_treatment
# t.test(Long$FRR ~ Long$is_treatment, paired = FALSE)
# summary(aov(Long$FRR ~ Long$is_treatment))
# TukeyHSD(aov(Long$FRR ~ Long$is_treatment))

OWA_FRR_Treatment <- anova_test(data = Long, 
                                dv = FRR, 
                                wid = acccunt_id, 
                                within = is_treatment)
get_anova_table(OWA_FRR_Treatment)

# POST-HOC TESTS
Long %>%       # Pairwise comparisons
  pairwise_t_test(FRR ~ is_treatment,
                  paired = TRUE,
                  p.adjust.method = "bonferroni")

# 7.B) THREE-WAY REPEATED MEASURES ANOVA FOR FRR----

# with(Long, summary(aov(FRR ~ week*is_treatment*country)))

Long %>% 
  anova_test(dv = FRR,
             wid = acccunt_id,
             within = c(week, is_treatment, country)) %>% 
  get_anova_table()

# 8) DESCRIPTIVE ANALYSES FOR DR ----
# (Disabled Rate Statistics)
Task_2 <- Long %>% 
  group_by(is_treatment, country, week, is_disabled) %>% # Group by required variables
  summarise(Count = n()) %>%                             # Record frequencies of disabled
  mutate(Disabled_Rate = Count/sum(Count)) %>%           # Compute disabled rate
  filter(is_disabled == "Yes") %>%                       # Keep only disabled
  select(-is_disabled, -Count)                           # Drop unecessary variables

DR <- Task_2 %>% 
  group_by(is_treatment, country, week) %>% # Group by required variables
  summarise(Mean = mean(Disabled_Rate),
            "Standard Deviation" = sd(Disabled_Rate))

write.csv(DR, file = "DR_Stats.csv")

# Country
DR_country <- Task_2 %>% 
  group_by(country) %>% # Group by country 
  summarise(Mean = mean(Disabled_Rate),
            "Standard Deviation" = sd(Disabled_Rate))

write.csv(DR_country, file = "DR_country.csv")

# treatment
DR_treatment <- Task_2 %>% 
  group_by(is_treatment) %>% # Group by week 
  summarise(Mean = mean(Disabled_Rate),
            "Standard Deviation" = sd(Disabled_Rate))

write.csv(DR_treatment, file = "DR_treatment.csv")

# Week
DR_week <- Task_2 %>% 
  group_by(week) %>% # Group by week 
  summarise(Mean = mean(Disabled_Rate),
            "Standard Deviation" = sd(Disabled_Rate))

write.csv(DR_week, file = "DR_week.csv")

# 9) DR VISUALIZATION ----

Task_2 %>%  
  ggplot() +                      
  aes(x = week, fill = is_treatment, 
      y = Disabled_Rate,
      label = round(Disabled_Rate, 3)) +
  geom_bar(position = "dodge",
           stat = "identity") +
  geom_text(size = 3, 
            hjust = 0.5, 
            vjust = -1, 
            position = position_dodge(width = 0.9))+
  scale_fill_manual(values=c("gold", "black")) +
  labs(x = "Week", 
       y = "Disabled Rate", 
       title = "BAR GRAPH OF DISABLED RATES", 
       subtitle = "The evolution over time of the disabled rates for Country 1 with/without treatment and Country 2", 
       fill = "Treatment") +
  theme(plot.title = element_text(size = 12, 
                                  hjust = 0.5, 
                                  vjust = 1),
        plot.subtitle = element_text(size = 12, 
                                     hjust = 0.5, 
                                     face = "italic", 
                                     colour = "red", 
                                     vjust = -1)) +
  theme_classic() +
  facet_wrap(vars(country))

# 10. CHECKING ASSUMPTIONS FOR REPEATED MEASURES ANOVA FOR DISABLED RATE ----
jarque.bera.test(Task_2$Disabled_Rate)  # Normal if p-value > 0.05 (We see that DR is normal)

# 5.b) THERE ARE NO OUTLIERS (This assumption is met)
Task_2 %>%
  group_by(is_treatment, week, country) %>%
  identify_outliers(Disabled_Rate)

# 11.A) ONE-WAY REPEATED MEASURES ANOVA FOR DR ----
# country
with(Task_2, summary(aov(Disabled_Rate ~ country)))

OWA_DR_Week <- anova_test(data = Task_2, dv = Disabled_Rate, wid = acccunt_id, within = country)
get_anova_table(OWA_DR_Week)

# week
OWA_DR_Country <- anova_test(data = Task_2, dv = Disabled_Rate, wid = acccunt_id, within = week)
get_anova_table(OWA_DR_Country)

Task_2 %>%       # Pairwise comparisons
  pairwise_t_test(Disabled_Rate ~ is_treatment,
                  paired = TRUE,
                  p.adjust.method = "bonferroni")

# is_treatment 
summary(aov(Task_2$Disabled_Rate ~ Task_2$is_treatment))
t.test(Task_2$Disabled_Rate ~ Task_2$is_treatment)
TukeyHSD(aov(Task_2$Disabled_Rate ~ Task_2$is_treatment))

OWA_DR_Treatment <- anova_test(data = Task_2, dv = Disabled_Rate, wid = acccunt_id, within = is_treatment)
 get_anova_table(OWA_DR_Treatment)

# 11.B) THREE-WAY REPEATED MEASURES ANOVA FOR DR----

# with(Task_2, summary(aov(Disabled_Rate ~ is_treatment*week*country)))

 Task_2 %>% 
   anova_test(dv = Disabled_Rate,
              wid = acccunt_id,
              within = c(week, is_treatment, country)) %>% 
   get_anova_table()
 
#______________________________THE END___________________________________
