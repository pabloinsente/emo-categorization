
library(lme4)
library(car)
library(lmerTest)
library(tidyverse)
library(ragg)
library(HLMdiag)
library(VCA)
library(hrbrthemes)
library(ggResidpanel)
library(sjPlot)
library(webshot)
library(equatiomatic)
library(svglite)
library(knitr)
library(ordinal)

df = read_csv("../clean_data/forced_choice_emotion_uw_students_long_format_lmer.csv")

table(df$emotion)

## drop "other" because it has the same sentiment score that "neutral", hence colinear

df<-subset(df, emotion!="Other")

table(df$emotion)


#####################################
#####################################

# fit lmer with ordinal approach

#####################################
#####################################

# filter(df, sentimentScore == "-0.4404")


df$emotionF <- factor(df$emotion,
                      order = TRUE, 
                      levels =c('Disgust', 'Anger', 'Fear', 'Sadness', 'Uncertain', 'Neutral', 'Surprise', 'Happiness'))


df$participantIdF <- as.factor(df$participantId)
df$ethnicityF <- as.factor(df$ethnicity)
df$sexF <- as.factor(df$sex)


str(df)


df <- na.omit(df)
df %>% mutate_if(is.character, as.factor)

dim(df)


# control= clmm2.control(innerCtrl="noWarn")

ord_m1 <- clmm2(
  emotionF ~ sexF*ethnicityF + (sexF*ethnicityF|participantId), 
  data=df, 
  Hess = TRUE)

summary(ord_m1)

## The odds ratio of *sentiment* being rated in category j or above (OR(Y ??? j))
exp(coef(ord_m1))

##############
# b_1 = (male - female) = -0.0780
# b_2 = (white - poc) = 0.1124
# b_3 = sexC:ethnicityC = 0.1542

# anova(ord_m1, type = 3)

# drop1(ord_m1, test="Chi")

## p values for variance parameters
ord_m2 <- clmm2(
  emotionF ~ sexC*ethnicityC, 
  random=participantIdF, 
  data=df, 
  Hess = TRUE, 
  control=control)

anova(ord_m2, ord_m1)

## (sexC*ethnicityC|participantId) not significant


###### The probability of a emotion-classification "j" is ??j = P (Yi ??? j) ??? P (Yi ??? j ??? 1)
###### The probability of a emotion-classification of "sadness for:
  # - the average participant
  # - for a female photo
  # - for a poc person
  # - for the sex*ethnicity at 0 

plogis(ord_m1$Theta[3] - ord_m1$beta[2]) - plogis(ord_m1$Theta[2] - ord_m1$beta[2])


plogis(ord_m1$Theta[7] - ord_m1$beta[6]) - plogis(ord_m1$Theta[6] - ord_m1$beta[6])


library(ggeffects)

ggpredictions_m1 = data.frame(ggpredict(ord_m1, terms = c("sexF", "ethnicityF"), type = "fe"))
ggpredictions_m1