
library(tidyverse)
library(ggpubr)
library(rstatix)

#####################
# read in data
#####################

# read students ranking 
df.rank = read_csv('../data/emotion_top_2_word_survey_dueling_bandits.csv')

# read frequency in the web rankig
unigram.freq= read_csv('../data/unigram_freq.csv')

head(df.rank)
head(unigram.freq)

df = merge(x = df.rank, 
           y = unigram.freq,
           by.x="word",
           by.y="word")

names(df)[7]  <- 'web.frequency'

head(df)


####################
# T-test assumptions

## check outliers 
outliers <- df %>% identify_outliers(web.frequency)
outliers # funny, well

# filter out outliers
df2 <- subset(df, photoID != 4 & photoID != 13 & photoID != 14  & photoID != 15 & photoID != 16) 

## Check normality assumption
df2 %>% shapiro_test(web.frequency)
# not normal 

ggqqplot(df2, x = "web.frequency")
# not normal


################
# basic exploration

df2 %>%
  group_by(method) %>%
  get_summary_stats(web.frequency, type = "mean_sd")


# grouped boxplot
ggplot(df2, aes(x = method, y = web.frequency)) + 
  geom_boxplot() +
  geom_point()

####################
# T test 

stat.test <- df2  %>% 
  t_test(web.frequency ~ method, paired = TRUE) %>%
  add_significance()
stat.test


###################
# Lmer because repeated measures by photoId
library(lme4)
library(car)

m1<-lmer(
  web.frequency ~ 1 + method + (1 |photoID), 
  data = df2)

summary(m1)

Anova(m1, type = "III")
