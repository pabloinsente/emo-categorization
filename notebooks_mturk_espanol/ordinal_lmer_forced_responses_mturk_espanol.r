library(tidyverse)
library(ordinal)
library("RColorBrewer")


df_students = read_csv("../clean_data/forced_choice_emotion_uw_students_long_format_lmer.csv")
df_mturk_en = read_csv("../clean_data_mturk/forced_choice_emotion_mturk_long_format_lmer.csv")
df_mturk_esp = read_csv("../clean_data_mturk_espanol/forced_choice_emotion_mturk_long_format_lmer_espanol.csv")  


#####################################
#####################################

# join dataframes for lmer

#####################################
#####################################

df_students$condition <- 'english'
df_mturk_en$condition <- 'english'
df_mturk_esp$condition <- 'espanol'

# language Condition centered
df_students$conditionC <- -0.5
df_mturk_en$conditionC <- -0.5
df_mturk_esp$conditionC <- 0.5

# mturk samples comparison
df <- rbind(df_mturk_en, df_mturk_esp)

# drop "Other" to break tie with "Neutral" and keep ordinal variable
df <- subset(df, emotion!="Other")


table(df$emotion)
table(df$conditionC)

#####################################
#####################################

# prepare data for ordinal lmer

#####################################
#####################################

# filter(df, sentimentScore == "-0.4404")

df$emotionF <- factor(df$emotion,
                      order = TRUE, 
                      levels =c('Disgust', 'Anger', 'Fear', 'Sadness', 'Uncertain', 'Neutral', 'Surprise', 'Happiness'))


df$participantIdF <- as.factor(df$participantId)
df$ethnicityF <- as.factor(df$ethnicity)
df$sexF <- as.factor(df$sex)
df$conditionF <- as.factor(df$condition)


table(df$participantIdF)
table(df$ethnicityF)
table(df$sexF)
table(df$conditionF)

str(df)
df <- na.omit(df)
dim(df)

#####################################
#####################################

# minimal plots 

#####################################
#####################################
# 
# display.brewer.pal(n = 10, name = "RdBu")
# brewer.pal(n = 10, name = "RdBu")
#  "#67001F" "#B2182B" "#D6604D" "#F4A582" "#FDDBC7" "#D1E5F0" "#92C5DE" "#4393C3" "#2166AC" "#053061"

RdBu8Alter <- c("#B2182B", "#D6604D", "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")

## chart for 3-way interaction SEX x ETHNICITY X CONDITION  

df %>% mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% ggplot( aes(x = conditionF, fill = emotionF)) + 
        geom_bar(position = "fill") +
        facet_grid(sexF~ethnicityF) + 
        scale_fill_manual(values = RdBu8Alter) + 
        theme_minimal()

## chart for 2-way interaction SEX x CONDITION  

df %>% mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% ggplot( aes(x = sexF, fill = emotionF)) + 
  geom_bar(position = "fill") +
  facet_grid(. ~conditionF) + 
  scale_fill_manual(values = RdBu8Alter) + 
  theme_minimal()


## chart for effect of condition

df %>% mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% ggplot( aes(x = conditionF, fill = emotionF)) + 
  geom_bar(position = "fill") +
  scale_fill_manual(values = RdBu8Alter) + 
  theme_minimal()



aggregate(x = df$sentimentScore,                # Specify data column
          by = list(df$sex),                    # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)

# 
# 1  female -0.1245115
# 2    male -0.1442889

aggregate(x = df$sentimentScore,                # Specify data column
          by = list(df$ethnicity),              # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)
# 
# 1   bipoc -0.1338941
# 2   white -0.1348134

#####################################
#####################################

# fit lmer with ordinal approach

#####################################
#####################################

# control= clm2.control(innerCtrl="noWarn")

ord_m1 <- clmm2(
  emotionF ~ 1 + sexC*ethnicityC*conditionC + (1 + sexC*ethnicityC|participantId), 
  data=df, 
  Hess = TRUE)

ord_m2 <- clm(
  emotionF ~ 1 + sexC*ethnicityC*conditionC + (1 + sexC*ethnicityC|participantId), 
  data=df, 
  Hess = TRUE)
#######
# summary significant results
summary(ord_m1)
summary(ord_m2)


# ConditionC beta = 0.083, p-value = 0.004
# indicates spanish-speaking participants rate pictures more positively; more likely to rate in more positive categories

# sexC:conditionC beta= 0.157, p-value = 0.006
# indicates that male pictures are more likely to be rated in more positive catgories

#######
# Get odds ratio for coefficients
exp(coef(ord_m1))


#######
## The odds ratio of *emotion* being rated in category j or above (OR(Y>j)) 

######
## sex ** NOT significant 
# logit = coef(ord_m1)[9]
# 
# exp(logit) # odds ratio 
# plogis(logit) # probability

# odds ratio of *emotion* being rated in category j or above at photo=male relative to photo=female is 0.9663773 
# probability of *emotion* being rated in category j or above at photo=male relative to photo=female is 0.493809

############
## ethnicity** NOT significant

logit = coef(ord_m1)[10]

exp(logit) # odds ratio 
plogis(logit) # probability

# odds ratio of *emotion* being rated in category j or above at photo=white relative to photo=poc is 1.086546 
# probability of *emotion* being rated in category j or above at ethnicity=white relative to ethnicity=POC is 0.520739

###########
# condition

logit = coef(ord_m1)[11]

exp(logit) # odds ratio 
plogis(logit)

# odds ratio of *emotion* being rated in category j or above at condition=espanol relative to photo=english is 1.091477
# probability of *emotion* being rated in category j or above at condition=espanol relative to photo=english is 0.5234118


###########
# sex X condition: 
# whether the effect of "sex" for the Spanish-survey is reliably different from the same effect for the English-survey


logit = coef(ord_m1)[12]

exp(logit) # odds ratio 
plogis(logit)

# odds ratio of *emotion* being rated in category j or above at [] relative to [] is 1.17081
# probability of *emotion* being rated in category j or above at [] relative to [] is 0.5393425 


###############################
###############################
# checking assumptions
##############################
##############################

#######
# Multicolinearity
library(stats)

chisq.test(df$sexF, df$ethnicityF, correct=FALSE)
# X-squared = 0.005297, df = 1, p-value = 0.942
chisq.test(df$sexF, df$conditionF, correct=FALSE)
# X-squared = 0.062296, df = 1, p-value = 0.8029
chisq.test(df$ethnicityF, df$conditionF, correct=FALSE)
# X-squared = 0.036023, df = 1, p-value = 0.8495

##########
#  proportional odds assumption

nominal_test(ord_m2)

# Tests of nominal effects
# 
# formula: emotionF ~ 1 + sexC * ethnicityC * conditionC + (1 + sexC * ethnicityC | participantId)
#                                       Df logLik   AIC     LRT  Pr(>Chi)    
# <none>                                   -30907 61842                      
# sexC                                   6 -30887 61813  40.272 4.028e-07 ***
# ethnicityC                             6 -30849 61739 115.130 < 2.2e-16 ***
# conditionC                             6 -30830 61700 153.763 < 2.2e-16 ***
# sexC:ethnicityC                        6 -30876 61793  60.841 3.037e-11 ***
# sexC:conditionC                        6 -30904 61848   5.596    0.4699    
# ethnicityC:conditionC                  6 -30905 61849   4.649    0.5896    
# sexC:ethnicityC:conditionC             6 -30904 61848   5.306    0.5052  

# scale_test(ord_m2)

# assumptions are meet for the most part: all main effects + sex*ethnicity interaction
