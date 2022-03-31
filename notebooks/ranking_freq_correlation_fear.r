# install.packages('cocor')
# install.packages('trafo')

library(tidyverse)
library(plyr)
library(cocor)

#####################
# read in data
#####################

# read students ranking 
df.fear.rank = read_csv('../clean_data/free_choice_emotion_uw_students_fear_rank.csv')

# read frequency in the web rankig
unigram.freq= read_csv('../data/unigram_freq.csv')

# read dueling bandits ranking
borda.fear.poc.female = read_csv('../../emo-ranking-next/fear_bipoc_female/borda_lilucb_ranking.csv')
borda.fear.poc.male = read_csv('../../emo-ranking-next/fear_bipoc_male/borda_lilucb_ranking.csv')
borda.fear.white.female = read_csv('../../emo-ranking-next/fear_white_female/borda_lilucb_ranking.csv')
borda.fear.white.male = read_csv('../../emo-ranking-next/fear_white_male/borda_lilucb_ranking.csv')


#####################
# align spelling 
#####################


df.fear.rank$emotion %>% sort()
borda.fear.poc.female$Target %>% sort()


# # remove words not present in both df
# borda.anger.poc.female<-subset(borda.anger.poc.female, Target!="frusturated")
# df.anger.rank<-subset(df.anger.rank, emotion!="none" & emotion!="surprise")


borda.fear.poc.female$Target <- mapvalues(borda.fear.poc.female$Target, 
                                           from=c("angry", "happy", "disgusted", "sad"), 
                                           to=c("anger", "happiness", "disgust", 'sadness'))
borda.fear.poc.male$Target <- mapvalues(borda.fear.poc.male$Target, 
                                         from=c("angry", "happy", "disgusted", "sad"), 
                                         to=c("anger", "happiness", "disgust", 'sadness'))
borda.fear.white.female$Target <- mapvalues(borda.fear.white.female$Target, 
                                             from=c("angry", "happy", "disgusted", "sad"), 
                                             to=c("anger", "happiness", "disgust", 'sadness'))
borda.fear.white.male$Target <- mapvalues(borda.fear.white.male$Target, 
                                           from=c("angry", "happy", "disgusted", "sad"), 
                                           to=c("anger", "happiness", "disgust", 'sadness'))


##############################
# pooled ranking borda scores
##############################


## rename score cols
names(borda.fear.poc.female)[3] <- 'score.apf'
names(borda.fear.poc.male)[3] <- 'score.apm'
names(borda.fear.white.female)[3] <- 'score.awf'
names(borda.fear.white.male)[3] <- 'score.awm'


## merge borda dfs
df.borda = merge(x = borda.fear.poc.female[ , c("Target", "score.apf")],
                 y = borda.fear.poc.male[ , c("Target", "score.apm")], 
                 by = "Target", 
                 all.x=TRUE)

df.borda = merge(x = df.borda,
                 y = borda.fear.white.female[ , c("Target", "score.awf")], 
                 by = "Target", 
                 all.x=TRUE)

df.borda = merge(x = df.borda,
                 y = borda.fear.white.male[ , c("Target", "score.awm")], 
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
df.fear.rank$rank.cnt.survey <- 1:nrow(df.fear.rank)
unigram.freq$rank.cnt.web <- 1:nrow(unigram.freq)
df.borda.sort$rank.borda <- 1:nrow(df.borda.sort)


## rename 
names(df.fear.rank)[2] <- 'Target'
names(unigram.freq)[1] <- 'Target'
names(unigram.freq)[2] <- 'cnt.web'

## merge 
rankd.df = merge(x = df.fear.rank[ , c("Target", "rank.cnt.survey")], 
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
  geom_smooth()


#######################
# rank.web ~ rank.borda
ggplot(rankd.df, aes(x=rank.borda, y=rank.cnt.web)) + 
  geom_point() +
  geom_smooth()


###################
# correlations
###################
require(cocor)
library("car")



# Shapiro-Wilk test can be performed as follow:
#   Null hypothesis: the data are normally distributed
#   Alternative hypothesis: the data are not normally distributed


shapiro.test(rankd.df$rank.cnt.web)
# W = 0.88657, p-value = 0.0113
shapiro.test(rankd.df$rank.cnt.survey)
# W = 0.94326, p-value = 0.1928
shapiro.test(rankd.df$rank.borda)
# W = 0.94749, p-value = 0.2389

## shapiro-wilk test looks good

qqPlot(rankd.df$rank.cnt.web)  # looks good
qqPlot(rankd.df$rank.cnt.survey)  # looks good
qqPlot(rankd.df$rank.borda)  # looks good

#######################
## pearson correlation
cor.web.survey <- cor.test(rankd.df$rank.cnt.web,
                           rankd.df$rank.cnt.survey, 
                           method = "pearson")
cor.web.survey
# 0.1196585
# t = 0.56531, df = 22, p-value = 0.5776


cor.web.borda <- cor.test(rankd.df$rank.cnt.web,
                          rankd.df$rank.borda, 
                          method = "pearson")
cor.web.borda
#-0.3063168 
# t = -1.5093, df = 22, p-value = 0.1454

########################
# comparing correlations
########################

cocor(~rank.cnt.web + rank.cnt.survey | rank.cnt.web + rank.borda, rankd.df)

## null hypothesis is retained (no difference)

#####################
#
#####################

library(trafo)

#####################
## survey ~ web model
linMod.survey <- lm(rank.cnt.web ~ rank.cnt.survey, data = rankd.df)
assumptions(linMod.survey)
# 
# Test normality assumption 
# Skewness Kurtosis Shapiro_W Shapiro_p
# sqrtshift       0.0032   2.5554    0.9788    0.8720
# dual           -0.0488   2.3361    0.9729    0.7393
# boxcox         -0.0474   2.3340    0.9729    0.7389

linMod.survey.trafo <- trafo_lm(linMod.survey, trafo = 'sqrtshift')

diagnostics(linMod.survey.trafo)
# 
# Residual diagnostics:
#   
#   Normality:
#   Pearson residuals:
#   Skewness Kurtosis Shapiro_W  Shapiro_p
# Untransformed model 0.902346132 2.946750 0.9087378 0.03311782
# Transformed model   0.003209655 2.555383 0.9787645 0.87204539
# 
# Heteroscedasticity:
#   BreuschPagan_V BreuschPagan_p
# Untransformed model      0.5270247      0.4678604
# Transformed model        1.2036910      0.2725852

plot(linMod.survey.trafo)

assumptions(linMod.survey.trafo)

summary(linMod.survey.trafo)

## transformed 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)      82.7935    18.2581   4.535 0.000163 ***
  # rank.cnt.survey   0.2555     1.0535   0.242 0.810642    


## not transformed
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)       9938.3     3315.9   2.997  0.00664 **
#   rank.cnt.survey    108.2      191.3   0.565  0.57758  


########################
# borda ~ web

linMod.borda <- lm(rank.cnt.web ~ rank.borda, data = rankd.df)
assumptions(linMod.borda)

# Test normality assumption 
# Skewness Kurtosis Shapiro_W Shapiro_p
# boxcox         -0.1514   2.4972    0.9669    0.5923
# bickeldoksum   -0.1514   2.4972    0.9669    0.5923
# gpower         -0.1514   2.4972    0.9669    0.5923

linMod.borda.trafo <- trafo_lm(linMod.borda, trafo = 'boxcox')

diagnostics(linMod.borda.trafo)
# 
# Residual diagnostics:
#   
#   Normality:
#   Pearson residuals:
#   Skewness Kurtosis Shapiro_W  Shapiro_p
# Untransformed model  0.7964991 2.953917 0.9230649 0.06829361
# Transformed model   -0.1514305 2.497248 0.9669384 0.59230146
# 
# Heteroscedasticity:
#   BreuschPagan_V BreuschPagan_p
# Untransformed model      1.7767869      0.1825444
# Transformed model        0.8346267      0.3609381

plot(linMod.borda.trafo)


assumptions(linMod.borda.trafo)


summary(linMod.borda.trafo)

## transformed
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 20.56521    0.98642  20.848 5.58e-16 ***
#   rank.borda  -0.06538    0.04506  -1.451    0.161    

# 
# ## NOT transformed
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)  15220.3     2861.4   5.319 2.44e-05 ***
#   rank.borda    -197.3      130.7  -1.509    0.145   