library(tidyverse)
library(ordinal)
library("RColorBrewer")

df_mturk = read_csv("../clean_data_mturk/forced_choice_emotion_mturk_long_format_lmer.csv")

table(df_mturk$emotion)

# drop "Other" to break tie with "Neutral" and keep ordinal variable
df <- subset(df_mturk, emotion!="Other")

table(df$emotion)

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


table(df$participantIdF)
table(df$ethnicityF)
table(df$sexF)

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


## chart for 2-way interaction SEX x ETHNICITY  

df %>% mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% ggplot( aes(x = sexF, fill = emotionF)) + 
  geom_bar(position = "fill") +
  facet_grid(. ~ethnicityF) + 
  scale_fill_manual(values = RdBu8Alter) + 
  theme_minimal()


## chart for effect of sex

df %>% mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% ggplot( aes(x = sexF, fill = emotionF)) + 
  geom_bar(position = "fill") +
  scale_fill_manual(values = RdBu8Alter) + 
  theme_minimal()

## chart for effect of ethnicity

df %>% mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% ggplot( aes(x = ethnicityF, fill = emotionF)) + 
  geom_bar(position = "fill") +
  scale_fill_manual(values = RdBu8Alter) + 
  theme_minimal()


aggregate(x = df$sentimentScore,                # Specify data column
          by = list(df$sex),                    # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)

# 1  female -0.1242946
# 2    male -0.1574881

aggregate(x = df$sentimentScore,                # Specify data column
          by = list(df$ethnicity),              # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)

# 1   bipoc -0.1374231
# 2   white -0.1441567

#####################################
#####################################

# fit lmer with ordinal approach

#####################################
#####################################

# control= clm2.control(innerCtrl="noWarn")

ord_m1 <- clmm2(
  emotionF ~ 1 + sexC*ethnicityC + (1 + sexC*ethnicityC|participantId), 
  data=df, 
  Hess = TRUE)

ord_m2 <- clm(
  emotionF ~ 1 + sexC*ethnicityC + (1 + sexC*ethnicityC|participantId), 
  data=df, 
  Hess = TRUE)

#######
# summary significant results
summary(ord_m1)
summary(ord_m2)

# sex beta= -0.11451, p-value = 0.001
# indicates that [sex=male] pictures are less likely to be rated in more positive categories

## NOT significant
# ethnicity beta = -0.05571, p-value = 0.12

## NOT significant
# sexC:ethnicityC beta = 0.02341, p-value = 0.74538

#######
# Get odds ratio for coefficients
exp(coef(ord_m1))


#######
## The odds ratio of *emotion* being rated in category j or above (OR(Y>j)) 

######
## sex
logit = coef(ord_m1)[8]

exp(logit) # odds ratio 
plogis(logit) # probbility

# odds ratio of *emotion* being rated in category j or above at photo=male relative to photo=female is 0.8918002 
# probability of *emotion* being rated in category j or above at photo=male relative to photo=female is 0.471403

############
## ethnicity
# 
# logit = coef(ord_m1)[9]
# 
# exp(logit) # odds ratio 
# plogis(logit) # probability


###########
# sex X condition: 
# whether the effect of "sex" for the [photo=white] is reliably different from the same effect for [photo=poc]


# logit = coef(ord_m1)[10]

# exp(logit) # odds ratio 
# plogis(logit) # probability


###############################
###############################
# checking assumptions
##############################
##############################

#######
# Multicolinearity
library(stats)

chisq.test(df$sexF, df$ethnicityF, correct=FALSE)
# X-squared = 0.0052644, df = 1, p-value = 0.9422

##########
#  proportional odds assumption

nominal_test(ord_m2)

# Tests of nominal effects
# 
# formula: emotionF ~ 1 + sexC * ethnicityC + (1 + sexC * ethnicityC | participantId)
#                                        Df logLik   AIC    LRT  Pr(>Chi)    
# <none>                                   -19197 38414                     
# sexC                                   6 -19187 38405 21.009  0.001828 ** 
# ethnicityC                             6 -19165 38362 64.881 4.562e-12 ***
# sexC:ethnicityC                        6 -19180 38391 34.993 4.323e-06 ***


# assumptions are meet for sure
