library(tidyverse)
library(ordinal)
library("RColorBrewer")

df_students = read_csv("../clean_data/forced_choice_emotion_uw_students_long_format_lmer.csv")

table(df_students$emotion)

# drop "Other" to break tie with "Neutral" and keep ordinal variable
df <- subset(df_students, emotion!="Other")

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

# 1  female -0.1155575
# 2    male -0.1339802

aggregate(x = df$sentimentScore,                # Specify data column
          by = list(df$ethnicity),              # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)

# 1   bipoc -0.1407513
# 2   white -0.1088178

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

# sex beta= -0.0775, p-value = 0.03
# indicates that [sex=male] pictures are less likely to be rated in more positive categories

# ethnicity beta = 0.1129, p-value = 0.002
# indicates that [ethnicity=white] pictures are more likely to be rated in more positive categories

# sexC:ethnicityC beta = 0.1526, p-value = 0.03
# indicates the effect of ethnicity differs within each sex category (or visceversa)

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

# odds ratio of *emotion* being rated in category j or above at photo=male relative to photo=female is 0.925464 
# probability of *emotion* being rated in category j or above at photo=male relative to photo=female is 0.4806447

############
## ethnicity

logit = coef(ord_m1)[9]

exp(logit) # odds ratio 
plogis(logit) # probability

# odds ratio of *emotion* being rated in category j or above at ethnicity=white relative to ethnicity=poc is 1.119546
# probability of *emotion* being rated in category j or above at ethnicity=white relative to ethnicity=poc is 0.5282009


###########
# sex X condition: 
# whether the effect of "sex" for the [photo=white] is reliably different from the same effect for [photo=poc]


logit = coef(ord_m1)[10]

exp(logit) # odds ratio 
plogis(logit) # probability

# odds ratio of *emotion* being rated in category j or above at [] relative to [] is 1.164826
# probability of *emotion* being rated in category j or above at [] relative to [] is 0.5380692 


###############################
###############################
# checking assumptions
##############################
##############################

#######
# Multicolinearity
library(stats)

chisq.test(df$sexF, df$ethnicityF, correct=FALSE)
# X-squared = 0.073856, df = 1, p-value = 0.7858

##########
#  proportional odds assumption

nominal_test(ord_m2)

# Tests of nominal effects
# 
# formula: emotionF ~ 1 + sexC * ethnicityC + (1 + sexC * ethnicityC | participantId)
#                                        Df logLik   AIC    LRT  Pr(>Chi)    
# <none>                                   -18462 36943                     
# sexC                                   6 -18458 36947  7.838    0.2502    
# ethnicityC                             6 -18413 36857 98.081 < 2.2e-16 ***
# sexC:ethnicityC                        6 -18428 36888 67.235 1.507e-12 ***

# scale_test(ord_m2)

# assumptions are meet for the most part: only the main effect of sex fails to pass
