# 0) SETTINGS ----
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
library(esquisse) 
library(rstatix)

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

# 3.iii) Confirm that there is no missing data

any(is.na(Data)) # We find that no column has missing data to warrant imputation. 

# 3.iv) Confirm that there is no duplicated Account ID.

any(duplicated(Data$acccunt_id)) # Again, all IDs are unique.

# 3.v) Reorganise columns and Split data into two
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

# 4) DESCRIPTIVE ANALYSES ----

Long$is_disabled <- forcats::fct_explicit_na(Long$is_disabled) # Make missing values explicit

# TASK 1 (Disabled Rate)
Task_1 <- Long %>% 
  group_by(is_treatment, country, week, is_disabled) %>% # Group by required variables
  summarise(Count = n()) %>%                             # Record frequencies of disabled
  mutate(Disabled_Rate = Count/sum(Count)) %>%           # Compute disabled rate
  filter(is_disabled == "Yes") %>%                       # Keep only disabled
  select(-is_disabled, -Count)                           # Drop unecessary variables

# Plot the bar graph

Task_1 %>%  
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

# TASK 2 (FRR)
Task_2 <- Long %>% 
  group_by(is_treatment, country, week, is_disabled) %>%  # Group by required variables
  summarise(AvrFRR = mean(FRR)) %>%                       # Record frequencies of disabled
  filter(is_disabled == "Yes") %>%                        # Keep disabled only
  select(-is_disabled)                                    # Drop unecessary variable   

# Plot the bar graph
Task_2 %>%  
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

# 5) CHECKING NORMALITY OF DISABLED RATE AND FRR PRIOR ANOVA ----


# ANOVA FOR FRR
# ONE-WAY ANOVA
summary(aov(Long$FRR ~ Long$country))
summary(aov(Long$FRR ~ Long$is_treatment))
summary(aov(Long$FRR ~ Long$week))

TukeyHSD(aov(Long$FRR ~ Long$country))
plot(TukeyHSD(aov(aov(Long$FRR ~ Long$is_treatment))))

# TWO-WAY ANOVA
summary(aov(Long$FRR ~ Long$country*Long$is_treatment*Long$week))

# ANOVA FOR DR
summary(aov(Task_1$Disabled_Rate ~ Task_1$country))
summary(aov(Task_1$Disabled_Rate ~ Task_1$is_treatment))
summary(aov(Task_1$Disabled_Rate ~ Task_1$week))

# TWO-WAY ANOVA
summary(aov(Task_1$Disabled_Rate ~ Task_1$is_treatment*Task_1$week))


