library(tidyverse)
library(ordinal)
library(RColorBrewer)
library(ggforce)
library(papaja)


df = read_csv("../clean_data_mturk_espanol/forced_choice_emotion_mturk_long_format_lmer_espanol.csv")  


table(df$participantId)

length(table(df$participantId)) # 52


# drop "Other" to break tie with "Neutral" and keep ordinal variable
df <- subset(df, emotion!="Other")
# drop because = to "I don't know what the person in the picture is feeling" Not "The person is feeling uncertainty"
df <- subset(df, emotion!="Uncertain")

table(df$emotion)
# 
# Anger   Disgust      Fear Happiness   Neutral   Sadness  Surprise 
# 905       483       351       909      1015       778       866 

#####################################
#####################################

# prepare data for ordinal lmer

#####################################
#####################################

# filter(df, sentimentScore == "-0.4404")

df$emotionF <- factor(df$emotion,
                      order = TRUE, 
                      levels =c('Disgust', 'Anger', 'Fear', 'Sadness', 'Neutral', 'Surprise', 'Happiness'))


df$participantIdF <- as.factor(df$participantId)
df$ethnicityF <- as.factor(df$ethnicity)
df$sexF <- as.factor(df$sex)


table(df$participantId)
table(df$ethnicityF)
table(df$sexF)

str(df)
df <- na.omit(df)
dim(df)

library(rstatix)

# sentiment scores by emotion 
df %>% group_by(emotionF) %>% get_summary_stats(sentimentScore, type = "mean_se")
# 
# 1 Disgust   sentimentScore  2125 -0.599     0
# 2 Anger     sentimentScore  2291 -0.572     0
# 3 Fear      sentimentScore  1578 -0.494     0
# 4 Sadness   sentimentScore  2657 -0.44      0
# 5 Neutral   sentimentScore  2971  0         0
# 6 Surprise  sentimentScore  2440  0.273     0
# 7 Happiness sentimentScore  3185  0.557     0

#####################################
#####################################

# fit lmer with ordinal approach

#####################################
#####################################
library(sjPlot)


df$sexC.flip <- df$sexC * -1
# female = -0.5; male = 0.5
# poc = -0.5; white = 0.5


# to get random effects correctly
ord_m2 <- clmm(
  emotionF ~ 1 + sexC.flip*ethnicityC + (1 + sexC.flip*ethnicityC|participantId), 
  data=df, 
  Hess = TRUE)

# for nominal test 
ord_m3 <- clm(
  emotionF ~ sexC.flip*ethnicityC + (sexC.flip*ethnicityC|participantId),
  data=df,
  Hess = TRUE)

#######
# summary significant results
summary(ord_m2)

# Random effects:
# Groups        Name                 Variance  Std.Dev. Corr                 
# participantId (Intercept)          0.0144922 0.12038                       
# sexC.flip            0.0001131 0.01063   1.000               
# ethnicityC           0.0002775 0.01666   1.000  1.000        
# sexC.flip:ethnicityC 0.0007034 0.02652  -1.000 -1.000 -1.000 
# Number of groups:  participantId 103 
# 
# Coefficients:
# Estimate Std. Error z value Pr(>|z|)    
# sexC.flip             0.10113    0.02672   3.784 0.000154 ***
# ethnicityC            0.01829    0.02675   0.684 0.494237    
# sexC.flip:ethnicityC -0.08480    0.05345  -1.587 0.112625   

tab_model(ord_m2)

#####################
# Nominal test effect
nominal_test(ord_m3)


# Tests of nominal effects
# 
# formula: emotionF ~ sexC.flip * ethnicityC + (sexC.flip * ethnicityC | participantId)
# Df logLik   AIC     LRT  Pr(>Chi)    
# <none>                                    -33186 66390                      
# sexC.flip                               5 -33176 66381  19.767  0.001382 ** 
# ethnicityC                              5 -33109 66245 155.350 < 2.2e-16 ***
# sexC.flip * ethnicityC | participantId                                      
# sexC.flip:ethnicityC                    5 -33144 66316  83.962 < 2.2e-16 ***
#   ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# summary(ord_m3)

exp(-0.1) # 0.9 

# sex beta= -0.1, OR =0.9 , p-value = 0.001
# indicates that [sex=male] pictures are less likely to be rated in more positive categories


######################
# PLOTS
######################

######################

RdBu8Alter <- c("#B2182B", "#D6604D", "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")


##chart effect sex

sex_chart <- df %>% 
  mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% 
  ggplot( aes(x = sexF, fill = emotionF)) + 
  geom_bar(position = "fill") +
  labs(y = "proportion", x = "sex", fill ="emotion") + 
  scale_fill_manual(values = RdBu8Alter) + 
  theme_apa()

sex_chart

ggsave('accuracy-charts/sex_ordinal_mturk_espanol.png', width = 6, height = 4)


## chart for effect of ethnicity

df$ethnicityFC <- ifelse(df$ethnicityF == "bipoc", "poc", "white")

et_chart <- df %>% 
  mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% 
  ggplot( aes(x = ethnicityFC, fill = emotionF)) + 
  geom_bar(position = "fill") +
  labs(y = "proportion", x = "ethnicity", fill ="emotion") + 
  scale_fill_manual(values = RdBu8Alter) + 
  theme_apa()

et_chart

ggsave('accuracy-charts/ethnicity_ordinal_mturk_espanol.png', width = 6, height = 4)

## chart for 2-way interaction SEX x ETHNICITY  

two_way_sex_ethnicity <- df %>% 
  mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% 
  ggplot( aes(x = sexF, fill = emotionF)) + 
  geom_bar(position = "fill") +
  facet_grid(. ~ethnicityFC) + 
  labs(y = "proportion", x = "sex", fill ="emotion") + 
  scale_fill_manual(values = RdBu8Alter) + 
  theme_apa()

two_way_sex_ethnicity

ggsave('accuracy-charts/sex_ethnicity_ordinal_mturk_espanol.png', width = 6, height = 4)

############################
############################
# descriptive results 

#### sex percentages
# female
df.sex.f <- subset(df, sexF=="female")
t.sex.f<-dplyr::count(df.sex.f, emotionF) 
t.sex.f$prop <- t.sex.f$n / nrow(df.sex.f)

t.sex.f
# positive 0.342
# negative 0.4817

# male
df.sex.m <- subset(df, sexF=="male")
t.sex.m<-dplyr::count(df.sex.m, emotionF) 
t.sex.m$prop <- t.sex.m$n / nrow(df.sex.m)

t.sex.m

# positive 0.328
# negative 0.4666

##########################
### ethnicity percentages
# poc
df.et.p <- subset(df, ethnicityFC=="poc")
t.et.p<-dplyr::count(df.et.p, emotionF) 
t.et.p$prop <- t.et.p$n / nrow(df.et.p)

t.et.p

# positive 0.324
# negative 0.4863


# white
df.et.w <- subset(df, ethnicityFC=="white")
t.et.w<-dplyr::count(df.et.w, emotionF) 
t.et.w$prop <- t.et.w$n / nrow(df.et.w)

t.et.w

# positive 0.345
# negative 0.4618


###############################
########### two-way percentages

# poc female column
df.poc.f <- subset(df, ethnicityFC=="poc" & sexF=="female")
t.poc.f<-dplyr::count(df.poc.f, emotionF) 
t.poc.f$prop <- t.poc.f$n / nrow(df.poc.f)

t.poc.f

# poc male column
df.poc.m <- subset(df, ethnicityFC=="poc" & sexF=="male")
t.poc.m<-dplyr::count(df.poc.m, emotionF) 
t.poc.m$prop <- t.poc.m$n / nrow(df.poc.m)

t.poc.m

# white female column
df.white.f <- subset(df, ethnicityFC=="white" & sexF=="female")
t.white.f<-dplyr::count(df.white.f, emotionF) 
t.white.f$prop <- t.white.f$n / nrow(df.white.f)

t.white.f

# white male column
df.white.m <- subset(df, ethnicityFC=="white" & sexF=="male")
t.white.m<-dplyr::count(df.white.m, emotionF) 
t.white.m$prop <- t.white.m$n / nrow(df.white.m)

t.white.m


