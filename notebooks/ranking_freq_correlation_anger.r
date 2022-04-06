# install.packages('cocor')
# install.packages('trafo')

library(tidyverse)
library(plyr)
library(cocor)

#####################
# read in data
#####################

# read students ranking 
df.anger.rank = read_csv('../clean_data/free_choice_emotion_uw_students_anger_rank.csv')

# read frequency in the web rankig
unigram.freq= read_csv('../data/unigram_freq.csv')

# read dueling bandits ranking
borda.anger.poc.female = read_csv('../../emo-ranking-next/anger_bipoc_female/borda_lilucb_ranking.csv')
borda.anger.poc.male = read_csv('../../emo-ranking-next/anger_bipoc_male/borda_lilucb_ranking.csv')
borda.anger.white.female = read_csv('../../emo-ranking-next/anger_white_female/borda_lilucb_ranking.csv')
borda.anger.white.male = read_csv('../../emo-ranking-next/anger_white_male/borda_lilucb_ranking.csv')


#####################
# align spelling 
#####################


df.anger.rank$emotion %>% sort()
borda.anger.poc.female$Target %>% sort()


# # remove words not present in both df
# borda.anger.poc.female<-subset(borda.anger.poc.female, Target!="frusturated")
# df.anger.rank<-subset(df.anger.rank, emotion!="none" & emotion!="surprise")


borda.anger.poc.female$Target <- mapvalues(borda.anger.poc.female$Target, 
                                           from=c("angry", "happy", "disgusted", "sad"), 
                                           to=c("anger", "happiness", "disgust", 'sadness'))
borda.anger.poc.male$Target <- mapvalues(borda.anger.poc.male$Target, 
                                           from=c("angry", "happy", "disgusted", "sad"), 
                                           to=c("anger", "happiness", "disgust", 'sadness'))
borda.anger.white.female$Target <- mapvalues(borda.anger.white.female$Target, 
                                           from=c("angry", "happy", "disgusted", "sad"), 
                                           to=c("anger", "happiness", "disgust", 'sadness'))
borda.anger.white.male$Target <- mapvalues(borda.anger.white.male$Target, 
                                           from=c("angry", "happy", "disgusted", "sad"), 
                                           to=c("anger", "happiness", "disgust", 'sadness'))


##############################
# pooled ranking borda scores
##############################


## rename score cols
names(borda.anger.poc.female)[3] <- 'score.apf'
names(borda.anger.poc.male)[3] <- 'score.apm'
names(borda.anger.white.female)[3] <- 'score.awf'
names(borda.anger.white.male)[3] <- 'score.awm'


## merge borda dfs
df.borda = merge(x = borda.anger.poc.female[ , c("Target", "score.apf")],
                 y = borda.anger.poc.male[ , c("Target", "score.apm")], 
                 by = "Target", 
                 all.x=TRUE)

df.borda = merge(x = df.borda,
                 y = borda.anger.white.female[ , c("Target", "score.awf")], 
                 by = "Target", 
                 all.x=TRUE)

df.borda = merge(x = df.borda,
                 y = borda.anger.white.male[ , c("Target", "score.awm")], 
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
df.anger.rank$rank.cnt.survey <- 1:nrow(df.anger.rank)
unigram.freq$rank.cnt.web <- 1:nrow(unigram.freq)
df.borda.sort$rank.borda <- 1:nrow(df.borda.sort)


## rename 
names(df.anger.rank)[2] <- 'Target'
names(unigram.freq)[1] <- 'Target'
names(unigram.freq)[2] <- 'cnt.web'

## merge 
rankd.df = merge(x = df.anger.rank[ , c("Target", "rank.cnt.survey")], 
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
       title = "anger - survey~web correlation")

ggsave("rank-cor/anger-web-survey.png", width = 4, height = 4)

#######################
# rank.web ~ rank.borda
ggplot(rankd.df, aes(x=rank.borda, y=rank.cnt.web)) + 
  geom_point() +
  geom_smooth() +
  labs(x = "rank survey",
       y = "rank web",
       title =  "anger - dueling-bandit~web correlation")

ggsave("rank-cor/anger-dueling-survey.png", width = 4, height = 4)

###################
# correlations
###################
require(cocor)
library("car")


# Shapiro-Wilk test can be performed as follow:
#   Null hypothesis: the data are normally distributed
#   Alternative hypothesis: the data are not normally distributed


shapiro.test(rankd.df$rank.cnt.web)
# W = 0.93828, p-value = 0.2225
shapiro.test(rankd.df$rank.cnt.survey)
# W = 0.95386, p-value = 0.4294
shapiro.test(rankd.df$rank.borda)
# W = 0.96156, p-value = 0.5754

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
# 0.2637604
# t = 1.1601, df = 18, p-value = 0.2612


cor.web.borda <- cor.test(rankd.df$rank.cnt.web,
                           rankd.df$rank.borda, 
                           method = "pearson")
cor.web.borda
# -0.01255706 
# t = -0.053279, df = 18, p-value = 0.9581

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

# Test normality assumption 
# Skewness Kurtosis Shapiro_W Shapiro_p
# sqrtshift      -0.3323   2.4740    0.9721    0.7992
# modulus        -0.1655   2.2272    0.9700    0.7544
# yeojohnson     -0.1655   2.2272    0.9700    0.7544

linMod.survey.trafo <- trafo_lm(linMod.survey, trafo = 'sqrtshift')

diagnostics(linMod.survey.trafo)

plot(linMod.survey.trafo)

assumptions(linMod.survey.trafo)

summary(linMod.survey.trafo)

## transformed 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)       71.763     18.989   3.779  0.00137 **
#   rank.cnt.survey    1.573      1.490   1.056  0.30502   


## not transformed
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)       8315.0     3330.4   2.497   0.0225 *
#   rank.cnt.survey    303.2      261.4   1.160   0.2612  


########################
# borda ~ web

linMod.borda <- lm(rank.cnt.web ~ rank.borda, data = rankd.df)
assumptions(linMod.borda)

# Test normality assumption 
# Skewness Kurtosis Shapiro_W Shapiro_p
# sqrtshift      -0.2057   2.5548    0.9850    0.9818
# dual           -0.0452   2.2080    0.9796    0.9291
# boxcox         -0.0423   2.2067    0.9796    0.9290

linMod.borda.trafo <- trafo_lm(linMod.borda, trafo = 'sqrtshift')

diagnostics(linMod.borda.trafo)

plot(linMod.borda.trafo)


assumptions(linMod.borda.trafo)


summary(linMod.borda.trafo)

## transformed
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)   
# (Intercept)       71.763     18.989   3.779  0.00137 **
#   rank.cnt.survey    1.573      1.490   1.056  0.30502   


## NOT transformed
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)  
# (Intercept)       8315.0     3330.4   2.497   0.0225 *
#   rank.cnt.survey    303.2      261.4   1.160   0.2612  
# ---
