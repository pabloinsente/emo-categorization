library(tidyverse)
library(svglite)
library(equatiomatic)
library(ggforce)
library(papaja)
library(rstatix)
library(rjson)



df1 = read.csv("../clean_data_mturk/accuracy_grouped_mturk.csv")
df2 = read.csv("../clean_data/accuracy_grouped.csv")

head(df1)
head(df2)

###########
# join data

df2$participantId <- df2$participantId + 1000 

table(df1$participantId)
table(df2$participantId)

df <- rbind(df1, df2)

###########
# prep data 

## add between subjects predictor for sex
df$sex_participant.dummy <- ifelse(df$sex_participant == "Male", 1, 0)
df$sex_participant.center <- ifelse(df$sex_participant == "Male", .5, -.5)

table(df$sex_participant.dummy)
table(df$sex_participant.center)


## add between subjects predictor for ethnicity
df$ethnicity_participant.dummy <- ifelse(df$ethnicity_participant == "White or Caucasian", 1, 0)
df$ethnicity_participant.center <- ifelse(df$ethnicity_participant == "White or Caucasian", .5, -.5)

table(df$ethnicity_participant.dummy)
table(df$ethnicity_participant.center)

#######################
# plots means by groups
#######################

##############
## by sex plot

df <- df[ which(df$sex_participant!="Non-binary / third gender"), ]


correct.sex <- df %>%
  group_by(sex_participant) %>%
  get_summary_stats(correct, type = "mean_se")

correct.sex

names(correct.sex)[4] <- "correct"


correct.sex.plot <- ggplot(correct.sex, aes(x=sex_participant, y=correct)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=correct-se, ymax=correct+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x = "sex participant",
       title = "Correct responses grouped by Wordnet synonyms") + 
  theme_apa()


correct.sex.plot


####################
## by ethnicity plot

df$ethnicity_participant.dic <- ifelse(df$ethnicity_participant == "White or Caucasian", "white", "Non-white")


correct.et <- df %>%
  group_by(ethnicity_participant.dic) %>%
  get_summary_stats(correct, type = "mean_se")

correct.et

names(correct.et)[4] <- "correct"


correct.et.plot <- ggplot(correct.et, aes(x=ethnicity_participant.dic, y=correct)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=correct-se, ymax=correct+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x = "ethnicity participant",
       title = "Correct responses grouped by Wordnet synonyms") + 
  theme_apa()


correct.et.plot

#############
# by sex X condition
correct.sex.cond <- df %>%
  group_by(sex_participant, condition) %>%  
  get_summary_stats(correct, type = "mean_se")

correct.sex.cond

names(correct.sex.cond)[5] <- "correct"


correct.sex.cond.plot <- ggplot(correct.sex.cond, aes(x = reorder(sex_participant, -correct), y=correct, fill=condition)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=correct-se, ymax=correct+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x = "sex participant",
       title = "Correct responses grouped by Wordnet synonyms") + 
  theme_apa()

correct.sex.cond.plot

#############
# by ethnicity X condition
correct.et.cond <- df %>%
  group_by(ethnicity_participant.dic, condition) %>%  
  get_summary_stats(correct, type = "mean_se")

correct.et.cond

names(correct.et.cond)[5] <- "correct"


correct.et.cond.plot <- ggplot(correct.et.cond, aes(x = reorder(ethnicity_participant.dic, -correct), y=correct, fill=condition)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=correct-se, ymax=correct+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x = "ethnicity participant",
       title = "Correct responses grouped by Wordnet synonyms") + 
  theme_apa()

correct.et.cond.plot

#############
# by sex X ethnicity
correct.sex.et <- df %>%
  group_by(sex_participant, ethnicity_participant.dic) %>%  
  get_summary_stats(correct, type = "mean_se")

correct.sex.et

names(correct.sex.et)[5] <- "correct"


correct.sex.et.plot <- ggplot(correct.sex.et, aes(x = reorder(sex_participant, -correct), y=correct, fill=ethnicity_participant.dic)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymin=correct-se, ymax=correct+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9)) +
  labs(x = "sex participant",
       title = "Correct responses grouped by Wordnet synonyms") + 
  theme_apa()

correct.sex.et.plot


#################
#################
# LMER model
#################
#################

head(df)
## simple effect sex
m1 <- glmer(correct ~ 1 + sex_participant.center*condition.center  + (1 | participantIdF) +  (1 | photoIdF),
            data = df,
            family = binomial) 
summary(m1)


## simple effect ethnicity
m2 <- glmer(correct ~ 1 + ethnicity_participant.center*condition.center + (1 | participantIdF) +  (1 | photoIdF),
            data = df,
            family = binomial) 

summary(m2)


###################
# plots of effects
###################
library(sjPlot)
library(ggplot2)
library(papaja)

et.main <- plot_model(m2, type = "eff", terms = c("ethnicity_participant.center")) +
  labs(x = "participant ethnicity", color="") + 
  theme_apa()
et.main 

cond.main <- plot_model(m2, type = "eff", terms = c("condition.center")) +
  labs(x = "survey method", color="") + 
  theme_apa()
cond.main 

et.survey.int <- plot_model(m2, type = "eff", terms = c("ethnicity_participant.center", 
                                                        "condition.center")) +
  labs(x = "participant ethnicity", color="") + 
  theme_apa()
et.survey.int
