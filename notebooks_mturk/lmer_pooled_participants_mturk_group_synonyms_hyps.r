library(tidyverse)
library(svglite)
library(equatiomatic)
library(ggforce)
library(papaja)
library(rstatix)
library(rjson)
library(lme4)
library(lmerTest)
library(ggsignif)
library(margins)
library(plyr)


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

length(table(df$participantId)) # 201

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

#################
#################
# LMER models
#################
#################

head(df)

#########################
# subset by survey method

length(df$condition)
df.free <- subset(df, condition=="free")
length(df.free$condition)
length(table(df.free$participantId)) # 98



length(df$condition)
df.forced <- subset(df, condition=="forced")
length(df.forced$condition)
length(table(df.forced$participantIdF)) # 103


length(table(df.forced$participantId))


#######################

# useful tips 
# https://stats.stackexchange.com/questions/57031/interpreting-interaction-terms-in-logit-regression-with-categorical-variables

#########################
# IN GROUP ADVANTAGE TEST
#########################

########################
## Forced-choice ethnicity

df.forced$ethnicity_participant.flip = ifelse(df.forced$ethnicity_participant.dummy == "1", 0, 1)
df.forced$ethnicity.dummy = ifelse(df.forced$ethnicity == "white", 1, 0)


m1 <- glmer(correct ~ 1 + ethnicity_participant.flip*ethnicity.dummy + (1 | participantIdF) +  (1 | photoIdF),
            data = df.forced,
            family = binomial) 

summary(m1)
car::Anova(m1, type="3")

# images 1 = white ; participants 1 = POC
# b2 test whether being POC increases/decreases odds of correct for POC images

# no significant effects

tab_model(m1)


##########################################

df.forced$ethnicity.flip = ifelse(df.forced$ethnicity == "bipoc", 1, 0)

m1.1 <- glmer(correct ~ 1 + ethnicity_participant.dummy*ethnicity.flip + (1 | participantIdF) +  (1 | photoIdF),
              data = df.forced,
              family = binomial) 

summary(m1.1)
car::Anova(m1.1, type="3")

# no significant effects
# images 1 = POC; participants 1 = White
# b2 test whether being white increases/decreases odds of correct for white images

tab_model(m1.1)

##########################################

########################
## Free-response ethnicity

df.free$ethnicity_participant.flip = ifelse(df.free$ethnicity_participant.dummy == "1", 0, 1)
df.free$ethnicity.dummy = ifelse(df.free$ethnicity == "white", 1, 0)

m2 <- glmer(correct ~ 1 + ethnicity_participant.flip*ethnicity.dummy + (1 | participantIdF) +  (1 | photoIdF),
            data = df.free,
            family = binomial) 

summary(m2)
car::Anova(m2, type="3")

tab_model(m2)


# images 0 = POC; participants 1 = POC


#image/participant
# poc/poc           = -1.25*** /odds of correct for POC participants with POC images
baseline = exp(-1.42)
baseline# **0.241** odds ratio baseline

# poc/white         = 1.19** /odds of correct whites vs POC participants among POC images
b1 = exp(0.17208)
b1 # 1.19*
baseline * b1 # 0.287


# white/poc         = 1.155/odds of correct whites vs POC images among POC participants
b2 = exp(0.14439)
b2 # 1.155
baseline * b2 # 0.28 odds ratio
# Odds ratio of correct are **0.28** when white (relative to POC) for POC images 

# Odds ratio of correct decrease by a factor of 0.997 (**0.28**) when white (relative to POC) for POC images [not significant]

# image*participant = 0.15* /difference btw the odds whites/POC among POC images AND the odds whites/POC among Whites
b3 =  exp(-0.148)
b3 # 0.86
baseline * b3 # 0.20


##########################################

# POC images = 1
df.free$ethnicity.flip = ifelse(df.free$ethnicity == "bipoc", 1, 0)
  
m2.1 <- glmer(correct ~ 1 + ethnicity_participant.dummy*ethnicity.flip + (1 | participantIdF) +  (1 | photoIdF),
            data = df.free,
            family = binomial) 

summary(m2.1)
car::Anova(m2.1, type="3")

#image/participant
# white/poc           = -1.25*** /odds of correct for POC participants with POC images
baseline = exp(-1.25)
baseline# **0.286** odds ratio baseline

# white/white         = -0.024 /odds of correct whites vs POC participants among POC images
b1 = exp(-0.02)
b1 # 0.98
baseline * b1 # 0.280
# Odds ratio of correct decreases by a factor of 0.98 (**0.28**) when white (relative to POC) for white images 
# A: being white reduces the odds of correct for white images [not significant]  

# poc/white         = 0.003/odds of correct whites vs POC images among POC participants
b2 = exp(0.003)
b2 # 1.003
baseline * b2 # 0.287 odds ratio
# Odds ratio of correct increases by a factor of 1.003 (**0.28**) when white (relative to POC) for white images [not significant]
# A: when images are POC odds ratio of correct among POC people increases [not significant]

# image*participant = 0.15* /difference btw the odds whites/POC among POC images AND the odds whites/POC among Whites
b3 =  exp(0.15)
b3 # 1.16
baseline * b3 # 0.33
# A: the effect of participant-et for white-images is reliably different (larger and positive)
# than the effect of of participant-et for POC images


# Summary       OR
# image/people
# POC/POC     = 0.286 ***
# POC/White   = -0.024 
# White/POC   = 0.287 
# White/White = 0.33 *


tab_model(m2.1)


##########################################

#########################
#########################
# FEMALE ADVANTAGE TEST
#########################
#########################


df.forced$sex.dummy = ifelse(df.forced$sex == "male", 1, 0)

########################
## Forced-choice SEX

m3 <- glmer(correct ~ 1 + sex_participant.dummy + (1 | participantIdF) +  (1 | photoIdF),
            data = df.forced,
            family = binomial) 

summary(m3)
car::Anova(m3, type="3")
exp(-0.167) # 0.846 

tab_model(m3)


##########################################

########################
## Free-response SEX

m4 <- glmer(correct ~ 1 + sex_participant.dummy + (1 | participantIdF) +  (1 | photoIdF),
            data = df.free,
            family = binomial) 

summary(m4)
car::Anova(m4, type="3")
exp(0.128) #1.14

tab_model(m4)


######################
######################
# Effects charts
######################
######################

###################
# Forced survey sex

correct.sex <- df.forced %>%
  group_by(sex_participant) %>%  
  get_summary_stats(correct, type = "mean_se")

correct.sex

names(correct.sex)[4] <- "correct"

correct.sex.plot <- ggplot(correct.sex, aes(x = sex_participant, y=correct, fill=sex_participant)) + 
  geom_bar(position=position_dodge(), stat="identity", color="black") +
  labs (title="Forced-choice",
        y="correct",
        x="sex participant") +
  guides(color="none", fill="none") +   
  # geom_signif(comparisons = list(c("Female", "Male")),
  #             y_position = 0.8,
  #             tip_length = 0, 
  #             vjust = 0.2, 
  #             annotation=c("***")) +
  theme_apa()


correct.sex.plot

ggsave('accuracy-charts/sex_diff_participants_accuracy_forced_english.png', width = 4, height = 4)

###################
# Free survey SEX 

correct.sex <- df.free %>%
  group_by(sex_participant) %>%  
  get_summary_stats(correct, type = "mean_se")

correct.sex

names(correct.sex)[4] <- "correct"

correct.sex.plot <- ggplot(correct.sex, aes(x = sex_participant, y=correct, fill=sex_participant)) + 
  geom_bar(position=position_dodge(), stat="identity", color="black") +
  labs (title="Free-response",
        y="correct",
        x="sex participant") +
  guides(color="none", fill="none") +   
  # geom_signif(comparisons = list(c("Female", "Male")),
  #             y_position = 0.35,
  #             tip_length = 0, 
  #             vjust = 0.2, 
  #             annotation=c("***")) +
  theme_apa()


correct.sex.plot

ggsave('accuracy-charts/sex_diff_participants_accuracy_free_english.png', width = 4, height = 4)


##################################################

################################################
# ethnicity participant X ethnicity photo FORCED

df.forced$ethnicity_participant.dic <- ifelse(df.forced$ethnicity_participant == "White or Caucasian", "white", "poc")

correct.et.et <- df.forced %>%
  group_by(ethnicity_participant.dic, ethnicity) %>%  
  get_summary_stats(correct, type = "mean_se")

correct.et.et

correct.et.et$ethnicity  <- mapvalues(correct.et.et$ethnicity, from=c('bipoc'), to=c('poc'))
# correct.et.et$ethnicity_participant.dic  <- mapvalues(correct.et.et$ethnicity_participant.dic, from=c('Non-white'), to=c('Poc'))
# 
# correct.et.et$ethnicity  <- mapvalues(correct.et.et$ethnicity, from=c('white'), to=c('White'))
# correct.et.et$ethnicity_participant.dic  <- mapvalues(correct.et.et$ethnicity_participant.dic, from=c('white'), to=c('White'))


correct.et.et

names(correct.et.et)[5] <- "correct"


correct.et.et.plot <- ggplot(correct.et.et, aes(x = reorder(ethnicity, -correct), y=correct, fill=ethnicity_participant.dic)) + 
  geom_bar(position=position_dodge(), stat="identity", color="black") +
  labs (title= "Forced-response",
        y="correct",
        x="ethnicity participant",
        fill = "ethnicity image") +
  # geom_signif(
  #   y_position = c(0.3, 0.3),
  #   xmin = c(0.8, 1.8), 
  #   xmax = c(1.2, 2.2),
  #   annotation = c("NS", "NS"), 
  #   tip_length = 0
  # ) +
  # geom_signif(comparisons = list(c("poc", "white")),
  #             y_position = 0.4,
  #             tip_length = 0, 
  #             vjust = 0.2, 
#             annotation=c("NS")) +
theme_apa()


correct.et.et.plot

ggsave('accuracy-charts/correct-et-et-forced.png', width = 6, height = 4)


###############
# FREE survey 
###############


#########################################
# ethnicity participant X ethnicity photo

df.free$ethnicity_participant.dic <- ifelse(df.free$ethnicity_participant == "White or Caucasian", "white", "poc")

correct.et.et <- df.free %>%
  group_by(ethnicity_participant.dic, ethnicity) %>%  
  get_summary_stats(correct, type = "mean_se")

correct.et.et$ethnicity  <- mapvalues(correct.et.et$ethnicity, from=c('bipoc'), to=c('poc'))

correct.et.et

names(correct.et.et)[5] <- "correct"


correct.et.et.plot <- ggplot(correct.et.et, aes(x = reorder(ethnicity, -correct), y=correct, fill=ethnicity_participant.dic)) + 
  geom_bar(position=position_dodge(), stat="identity", color="black") +
  labs (title= "Free-response",
        y="correct",
        x="ethnicity participant",
        fill = "ethnicity image") +
  # geom_signif(
  #   y_position = c(0.3, 0.3),
  #   xmin = c(0.8, 1.8), 
  #   xmax = c(1.2, 2.2),
  #   annotation = c("NS", "NS"), 
  #   tip_length = 0
  # ) +
  # geom_signif(comparisons = list(c("poc", "white")),
  #             y_position = 0.4,
  #             tip_length = 0, 
  #             vjust = 0.2, 
  #             annotation=c("NS")) +
  theme_apa()


correct.et.et.plot

ggsave('accuracy-charts/correct-et-et-free.png', width = 6, height = 4)


###################
# plots of effects
###################
library(sjPlot)
library(ggplot2)
library(papaja)

et.main <- plot_model(m2, type = "eff", terms = c("ethnicity_participant.dummy")) +
  labs(x = "participant ethnicity", color="") + 
  theme_apa()
et.main 

###############
# Ethnicity participant = White 1 for POC images

plot_model(m2,
           show.values = TRUE, 
           vline.color = "red", 
           terms = c("ethnicity_participant.dummy", 
                     "ethnicity.dummy",
                     "ethnicity_participant.dummy:ethnicity.dummy")) +
  labs(x = "participant ethnicity", color="") + 
  theme_apa()

summary(m2.1)

###############
# Ethnicity participant = White 1 for POC images


plot_model(m2.1,
           show.values = TRUE, 
           vline.color = "red", 
           terms = c("ethnicity_participant.dummy", 
                     "ethnicity.flip",
                     "ethnicity_participant.dummy:ethnicity.flip")) +
  labs(x = "participant ethnicity", color="") + 
  theme_apa()

