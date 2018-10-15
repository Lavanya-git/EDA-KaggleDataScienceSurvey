# Load data
#install.packages("dplyr")
library(dplyr)
#install.packages("stringr")
library(stringr)
library(ggplot2)
#install.packages("ggthemes")
library(ggthemes)
#install.packages("tidyr")
library(tidyr)
library(scales)
in_data <- read.csv("C:/R/mult_resp.csv",header =T, stringsAsFactors = F)

attach(in_data)
in_data$Age[Age > 70] <- "Elder"
in_data$Age[Age > 40 & Age <= 70] <- "Middle Aged"
in_data$Age[Age < 40] <- "Young"

#Age Distribution
in_data %>% 
  filter(Age!='') %>% 
  group_by(Age) %>% 
  count() %>% 
  ggplot(aes(x = Age,y = (n / sum(n))*100))+
  geom_bar(stat = 'identity') + ylab('Percent') + theme_excel() +
  theme(axis.text = element_text(size = 8)) + ggtitle('Age Distribution of Kaggle Survey Respondents')


#Gender Distribution
in_data %>% 
  filter(GenderSelect!='') %>% 
  group_by(GenderSelect) %>% 
  count() %>% 
  ggplot(aes(x = GenderSelect,y = (n / sum(n))*100))+
  geom_bar(stat = 'identity') + ylab('Percent') + theme_excel() +
  theme(axis.text = element_text(size = 8)) + ggtitle('Gender Distribution of Kaggle Survey Respondents')

#country wise split
complete_data %>% filter(GenderSelect %in% c('Male','Female')) %>%
  group_by(Country,GenderSelect) %>% 
  summarise(count = n()) %>%
  arrange(desc(count)) %>% ggplot() + 
  geom_bar(aes(Country,count, fill = GenderSelect), stat = 'identity') +
  #facet_grid(.~GenderSelect)  + 
  theme_solarized() +
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 40, vjust = 0.5, hjust = 0.5)) +
  ggtitle('Country wise Survey Respondends - M/F') 

#countrywise female to male ratio
complete_data %>% filter(GenderSelect %in% c('Male','Female') & Country!="") %>%
  group_by(Country,GenderSelect) %>% 
  summarise(count = n()) %>%
  spread(GenderSelect,count) %>% 
  mutate(F2M = (Female/Male)*100) %>% 
  arrange(desc(F2M)) %>%
  #mutate(F2M = percent(F2M)) %>% 
  ggplot() +
  geom_bar(aes(Country,F2M, fill = F2M), stat = 'identity') +
  theme_solarized() +
  theme(axis.text = element_text(size = 9),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5)) +
  ggtitle('Female to Male Ratio - Country Wise') + scale_fill_continuous_tableau()

#age distribution
complete_data %>% filter(GenderSelect %in% c('Male','Female')) %>%  
  ggplot() + geom_histogram(aes(Age),binwidth = 1) + 
  theme_solarized() + facet_grid(.~GenderSelect) +
  ggtitle('Age Distribution - Male vs Female')

#language familiar with data scientists
complete_data %>% group_by(LanguageRecommendationSelect) %>% summarize(count = n())

#language that familiar with female enthusiasts
complete_data %>% filter(GenderSelect %in% c('Male','Female')) %>% group_by(MLToolNextYearSelect,GenderSelect) %>% 
  summarize(count = n()) %>%
  spread(GenderSelect,count)  %>% 
  mutate(Female2MaleRatio = Female/Male) %>% 
  mutate(Female2MaleRatio = Female2MaleRatio * 100) %>% 
  arrange(desc(Female2MaleRatio)) %>% 
  ggplot() +
  geom_bar(aes(LanguageRecommendationSelect,Female2MaleRatio, 
               fill = Female2MaleRatio),colour="black",stat = 'identity',width=0.3) +
  theme_stata() + 
  theme(axis.text = element_text(size = 12),
       axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1)) +
 scale_fill_continuous(low="green",high="blue") + ggtitle('Programming Languages Female Programmers interested to work')

#language that familiar with female enthusiasts
complete_data %>% filter(GenderSelect %in% c('Male','Female')) %>%
  filter(!MLToolNextYearSelect %in% c('Other','','Angoss','Stan','Julia','KNIME (free version)','KNIME (commercial version)','Orange','Microsoft R Server (Formerly Revolution Analytics)'))%>%
  group_by(MLToolNextYearSelect,GenderSelect) %>% 
  summarize(count = n()) %>%
  spread(GenderSelect,count)  %>% 
  mutate(Female2MaleRatio = Female/Male) %>% 
  mutate(Female2MaleRatio = Female2MaleRatio * 100) -> F2M
  F2M1 <- F2M[order(F2M$Female2MaleRatio),]
  F2M1$MLToolNextYearSelect <- factor(F2M1$MLToolNextYearSelect,
                     levels = F2M1$MLToolNextYearSelect)
  ggplot(F2M1,aes(y=Female2MaleRatio,x=factor(MLToolNextYearSelect))) +
  geom_bar(aes(MLToolNextYearSelect,Female2MaleRatio,
               fill = Female2MaleRatio),colour="black",stat = 'identity',width=0.3)+
  theme_stata() + 
  theme(axis.text = element_text(size = 14),
        axis.text.x = element_text(angle = 90, vjust = 0.2, hjust = 1)) + 
  scale_fill_continuous(low="green",high="blue") + ggtitle('Female to Male Ratio of MLTools NextYearSelect')
