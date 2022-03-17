# library(lme4)
# library(car)
# library(lmerTest)
library(tidyverse)
# library(ragg)
# library(HLMdiag)
# library(VCA)
# library(hrbrthemes)
# library(ggResidpanel)
# library(sjPlot)
# library(webshot)
# library(equatiomatic)
# library(svglite)
# library(knitr)
library(ordinal)
library("RColorBrewer")
library(ggeffects)




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

df <- subset(df, emotion!="Other")


table(df$emotion)
table(df$conditionC)

#####################################
#####################################

# prepare data for ordinal lmer

#####################################
#####################################

# filter(df, sentimentScore == "-0.4404")

# df$emotionF <- factor(df$emotion,
#                       order = TRUE, 
                      # levels =c('Disgust', 'Anger', 'Fear', 'Sadness', 'Uncertain', 'Neutral', 'Other', 'Surprise', 'Happiness'))

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
# cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7", "#000000")
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
          by = list(df$sex),              # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)


aggregate(x = df$sentimentScore,                # Specify data column
          by = list(df$ethnicity),              # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)
#####################################
#####################################

# fit lmer with ordinal approach

#####################################
#####################################

logit2prob <- function(logit){
  odds <- exp(logit)
  prob <- odds / (1 + odds)
  return(prob)
}

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


# ConditionC beta = 0.0875, p-value = 0.001 
# indicates spanish-speaking participants rate pictures more positively; more likely to rate in more positive categories

# sexC:conditionC beta= 0.1360, p-value = 0.01
# indicates that male pictures are more likely to be rated in more positive catgories

#######
# Get odds ratio for coefficients
exp(coef(ord_m1))


#######
## The odds ratio of *emotion* being rated in category j or above (OR(Y>j)) 

######
## sex ** NOT significant 
logit = coef(ord_m1)[9]

exp(logit) # odds ratio 
logit2prob(logit) # probability

# odds ratio of *emotion* being rated in category j or above at photo=male relative to photo=female is 0.9663773 

############
## ethnicity** NOT significant

logit = coef(ord_m1)[10]

exp(logit) # odds ratio 
logit2prob(logit) # probability

# odds ratio of *emotion* being rated in category j or above at photo=white relative to photo=poc is 0.982327

###########
# condition/language

logit = coef(ord_m1)[11]

exp(logit) # odds ratio 
logit2prob(logit) # probability
plogis(logit)

# odds ratio of *emotion* being rated in category j or above at condition=espanol relative to photo=english is 1.091477
# probability of *emotion* being rated in category j or above at condition=espanol relative to photo=english is 0.521869


###########
# sex X condition: 
# whether the effect of "sex" for the Spanish-survey is reliably different from the same effect for the English-survey


logit = coef(ord_m1)[13]

exp(logit) # odds ratio 
logit2prob(logit) # probability
plogis(logit)

# 

# odds ratio of *emotion* being rated in category j or above at [] relative to [] is 1.14566 
# probability of *emotion* being rated in category j or above at [] relative to [] is 0.533943 


###############################
###############################
nominal_test(ord_m2)
scale_test(ord_m2)


