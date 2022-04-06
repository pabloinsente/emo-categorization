# install.packages('cocor')
# install.packages('trafo')

library(tidyverse)
library(plyr)
library(cocor)

#####################
# read in data
#####################

# read students ranking 
df.disgust.rank = read_csv('../clean_data/free_choice_emotion_uw_students_disgust_rank.csv')

# read frequency in the web rankig
unigram.freq= read_csv('../data/unigram_freq.csv')

# read dueling bandits ranking
borda.disgust.poc.female = read_csv('../../emo-ranking-next/disgust_bipoc_female/borda_lilucb_ranking.csv')
borda.disgust.poc.male = read_csv('../../emo-ranking-next/disgust_bipoc_male/borda_lilucb_ranking.csv')
borda.disgust.white.female = read_csv('../../emo-ranking-next/disgust_white_female/borda_lilucb_ranking.csv')
borda.disgust.white.male = read_csv('../../emo-ranking-next/disgust_white_male/borda_lilucb_ranking.csv')


#####################
# align spelling 
#####################


df.disgust.rank$emotion %>% sort()
borda.disgust.poc.female$Target %>% sort()
borda.disgust.white.female $Target %>% sort()


# # remove words not present in both df
# borda.anger.poc.female<-subset(borda.anger.poc.female, Target!="frusturated")
# df.anger.rank<-subset(df.anger.rank, emotion!="none" & emotion!="surprise")


borda.disgust.poc.female$Target <- mapvalues(borda.disgust.poc.female$Target, 
                                             from=c("angry", "happy", "disgusted", "sad", "surprised"), 
                                             to=c("anger", "happiness", "disgust", 'sadness', 'surprise'))
borda.disgust.poc.male$Target <- mapvalues(borda.disgust.poc.male$Target, 
                                           from=c("angry", "happy", "disgusted", "sad", "surprised"), 
                                           to=c("anger", "happiness", "disgust", 'sadness', 'surprise'))
borda.disgust.white.female$Target <- mapvalues(borda.disgust.white.female$Target, 
                                               from=c("angry", "happy", "disgusted", "sad", "surprised"), 
                                               to=c("anger", "happiness", "disgust", 'sadness', 'surprise'))
borda.disgust.white.male$Target <- mapvalues(borda.disgust.white.male$Target, 
                                             from=c("angry", "happy", "disgusted", "sad", "surprised"), 
                                             to=c("anger", "happiness", "disgust", 'sadness', 'surprise'))


##############################
# pooled ranking borda scores
##############################


## rename score cols
names(borda.disgust.poc.female)[3] <- 'score.apf'
names(borda.disgust.poc.male)[3] <- 'score.apm'
names(borda.disgust.white.female)[3] <- 'score.awf'
names(borda.disgust.white.male)[3] <- 'score.awm'


## merge borda dfs
df.borda = merge(x = borda.disgust.poc.female[ , c("Target", "score.apf")],
                 y = borda.disgust.poc.male[ , c("Target", "score.apm")], 
                 by = "Target", 
                 all.x=TRUE)

df.borda = merge(x = df.borda,
                 y = borda.disgust.white.female[ , c("Target", "score.awf")], 
                 by = "Target", 
                 all.x=TRUE)

df.borda = merge(x = df.borda,
                 y = borda.disgust.white.male[ , c("Target", "score.awm")], 
                 by = "Target", 
                 all.x=TRUE)

## compute row means borda scores
df.borda$score.ave <- rowMeans(df.borda[ , c("score.apf", "score.apm", "score.awf", "score.awm")])


## sort df by average borda score
df.borda.sort <- df.borda[order(-df.borda$score.ave),] 


#######################
# merge dataframes 
#######################

## add rank columns
df.disgust.rank$rank.cnt.survey <- 1:nrow(df.disgust.rank)
unigram.freq$rank.cnt.web <- 1:nrow(unigram.freq)
df.borda.sort$rank.borda <- 1:nrow(df.borda.sort)


## rename 
names(df.disgust.rank)[2] <- 'Target'
names(unigram.freq)[1] <- 'Target'
names(unigram.freq)[2] <- 'cnt.web'

## merge 
rankd.df = merge(x = df.disgust.rank[ , c("Target", "rank.cnt.survey")], 
                 y = df.borda.sort[ , c("Target", "rank.borda")], 
                 by = "Target")

rankd.df = merge(x = rankd.df, 
                 y = unigram.freq[ , c("Target", "rank.cnt.web")],
                 by = "Target")

rankd.df


#####################
# plots
#####################

library(ggplot2)

########################
# rank.web ~ rank.survey
ggplot(rankd.df, aes(x=rank.cnt.survey, y=rank.cnt.web)) + 
  geom_point() +
  geom_smooth() +
  labs(x = "rank survey",
       y = "rank web",
       title = "disgust - survey~web correlation")

ggsave("rank-cor/disgust-web-survey.png", width = 4, height = 4)


#######################
# rank.web ~ rank.borda
ggplot(rankd.df, aes(x=rank.borda, y=rank.cnt.web)) + 
  geom_point() +
  geom_smooth()  +
  labs(x = "rank survey",
       y = "rank web",
       title =  "disgust - dueling-bandit~web correlation")

ggsave("rank-cor/disgust-dueling-survey.png", width = 4, height = 4)


###################
# correlations
###################
require(cocor)
library("car")



# Shapiro-Wilk test can be performed as follow:
#   Null hypothesis: the data are normally distributed
#   Alternative hypothesis: the data are not normally distributed


shapiro.test(rankd.df$rank.cnt.web)
# W = 0.9257, p-value = 0.07815
shapiro.test(rankd.df$rank.cnt.survey)
# W = 0.95198, p-value = 0.299
shapiro.test(rankd.df$rank.borda)
# W = 0.9097, p-value = 0.03474

## shapiro-wilk test looks good for 1 and 2, not 3

qqPlot(rankd.df$rank.cnt.web)  # looks good
qqPlot(rankd.df$rank.cnt.survey)  # looks good
qqPlot(rankd.df$rank.borda)  # not great

#######################
## pearson correlation
cor.web.survey <- cor.test(rankd.df$rank.cnt.web,
                           rankd.df$rank.cnt.survey, 
                           method = "pearson")
cor.web.survey
# -0.1413045 
# t = -0.66949, df = 22, p-value = 0.5101


cor.web.borda <- cor.test(rankd.df$rank.cnt.web,
                          rankd.df$rank.borda, 
                          method = "pearson")
cor.web.borda
# -0.4900404
# t = -2.6368, df = 22, p-value = 0.01506

########################
# comparing correlations
########################

cocor(~rank.cnt.web + rank.cnt.survey | rank.cnt.web + rank.borda, rankd.df)

## null hypothesis is retained (no difference)

####################################
# linear models with transformations
####################################

library(trafo)

#####################
## survey ~ web model
linMod.survey <- lm(rank.cnt.web ~ rank.cnt.survey, data = rankd.df)
assumptions(linMod.survey)

# Test normality assumption 
#                 Skewness Kurtosis Shapiro_W Shapiro_p
# sqrtshift      -0.0537   2.0811    0.9663    0.5762
# dual           -0.0251   1.7930    0.9540    0.3296
# boxcox         -0.0217   1.7906    0.9539    0.3283

linMod.survey.trafo <- trafo_lm(linMod.survey, trafo = 'sqrtshift')

diagnostics(linMod.survey.trafo)

plot(linMod.survey.trafo)

assumptions(linMod.survey.trafo)

summary(linMod.survey.trafo)

## transformed 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       96.445     16.910   5.703 9.78e-06 ***
#   rank.cnt.survey   -0.802      1.127  -0.712    0.484  

## not transformed
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      13011.1     2970.5   4.380 0.000238 ***
#   rank.cnt.survey   -132.5      198.0  -0.669 0.510146  


########################
# borda ~ web

linMod.borda <- lm(rank.cnt.web ~ rank.borda, data = rankd.df)
assumptions(linMod.borda)

# Test normality assumption 
# Skewness Kurtosis Shapiro_W Shapiro_p
# logshiftopt     0.1862   2.1308    0.9758    0.8082
# log             0.2499   2.0857    0.9678    0.6136
# glog            0.2499   2.0857    0.9678    0.6136

linMod.borda.trafo <- trafo_lm(linMod.borda, trafo = 'logshiftopt')


diagnostics(linMod.borda.trafo)

# 
# Residual diagnostics:
#   
#   Normality:
#   Pearson residuals:
#   Skewness Kurtosis Shapiro_W   Shapiro_p
# Untransformed model 0.7637718 2.338906 0.8833400 0.009707984
# Transformed model   0.1862067 2.130793 0.9758179 0.808171293
# 
# Heteroscedasticity:
#   BreuschPagan_V BreuschPagan_p
# Untransformed model       1.021623      0.3121343
# Transformed model         1.892751      0.1688920

plot(linMod.borda.trafo)


assumptions(linMod.borda.trafo)


summary(linMod.borda.trafo)

## transformed
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  9.743830   0.257689  37.812   <2e-16 ***
#   rank.borda  -0.030142   0.009665  -3.119    0.005 ** 


## NOT transformed
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 17206.15    2587.78   6.649  1.1e-06 ***
#   rank.borda   -255.93      97.06  -2.637   0.0151 *  
# ---
