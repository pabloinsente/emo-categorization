# install.packages('cocor')
# install.packages('trafo')

library(tidyverse)
library(plyr)
library(cocor)

#####################
# read in data
#####################

# read students ranking 
df.surprise.rank = read_csv('../clean_data/free_choice_emotion_uw_students_surprise_rank.csv')

# read frequency in the web rankig
unigram.freq= read_csv('../data/unigram_freq.csv')

# read dueling bandits ranking
borda.surprise.poc.female = read_csv('../../emo-ranking-next/surprise_bipoc_female/borda_lilucb_ranking.csv')
borda.surprise.poc.male = read_csv('../../emo-ranking-next/surprise_bipoc_male/borda_lilucb_ranking.csv')
borda.surprise.white.female = read_csv('../../emo-ranking-next/surprise_white_female/borda_lilucb_ranking.csv')
borda.surprise.white.male = read_csv('../../emo-ranking-next/surprise_white_male/borda_lilucb_ranking.csv')


#####################
# align spelling 
#####################


df.surprise.rank$emotion %>% sort()
borda.surprise.poc.female$Target %>% sort()


# # remove words not present in both df
# borda.anger.poc.female<-subset(borda.anger.poc.female, Target!="frusturated")
# df.anger.rank<-subset(df.anger.rank, emotion!="none" & emotion!="surprise")


borda.surprise.poc.female$Target <- mapvalues(borda.surprise.poc.female$Target, 
                                             from=c("angry", "happy", "disgusted", "sad", "smiling"), 
                                             to=c("anger", "happiness", "disgust", 'sadness', 'smile'))
borda.surprise.poc.male$Target <- mapvalues(borda.surprise.poc.male$Target, 
                                           from=c("angry", "happy", "disgusted", "sad", "smiling"), 
                                           to=c("anger", "happiness", "disgust", 'sadness', 'smile'))
borda.surprise.white.female$Target <- mapvalues(borda.surprise.white.female$Target, 
                                               from=c("angry", "happy", "disgusted", "sad", "smiling"), 
                                               to=c("anger", "happiness", "disgust", 'sadness', 'smile'))
borda.surprise.white.male$Target <- mapvalues(borda.surprise.white.male$Target, 
                                             from=c("angry", "happy", "disgusted", "sad", "smiling"), 
                                             to=c("anger", "happiness", "disgust", 'sadness', 'smile'))


##############################
# pooled ranking borda scores
##############################


## rename score cols
names(borda.surprise.poc.female)[3] <- 'score.apf'
names(borda.surprise.poc.male)[3] <- 'score.apm'
names(borda.surprise.white.female)[3] <- 'score.awf'
names(borda.surprise.white.male)[3] <- 'score.awm'


## merge borda dfs
df.borda = merge(x = borda.surprise.poc.female[ , c("Target", "score.apf")],
                 y = borda.surprise.poc.male[ , c("Target", "score.apm")], 
                 by = "Target", 
                 all.x=TRUE)

df.borda = merge(x = df.borda,
                 y = borda.surprise.white.female[ , c("Target", "score.awf")], 
                 by = "Target", 
                 all.x=TRUE)

df.borda = merge(x = df.borda,
                 y = borda.surprise.white.male[ , c("Target", "score.awm")], 
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
df.surprise.rank$rank.cnt.survey <- 1:nrow(df.surprise.rank)
unigram.freq$rank.cnt.web <- 1:nrow(unigram.freq)
df.borda.sort$rank.borda <- 1:nrow(df.borda.sort)


## rename 
names(df.surprise.rank)[2] <- 'Target'
names(unigram.freq)[1] <- 'Target'
names(unigram.freq)[2] <- 'cnt.web'

## merge 
rankd.df = merge(x = df.surprise.rank[ , c("Target", "rank.cnt.survey")], 
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

# remove outlier
# rankd.df<-subset(rankd.df, rank.cnt.web!="134051" & rank.cnt.web!="52242")


########################
# rank.web ~ rank.survey
ggplot(rankd.df, aes(x=rank.cnt.survey, y=rank.cnt.web)) + 
  geom_point() +
  geom_smooth() +
  labs(x = "rank survey",
       y = "rank web",
       title = "surprise - survey~web correlation")

ggsave("rank-cor/surprise-web-survey.png", width = 4, height = 4)


#######################
# rank.web ~ rank.borda
ggplot(rankd.df, aes(x=rank.borda, y=rank.cnt.web)) + 
  geom_point() +
  geom_smooth()  +
  labs(x = "rank survey",
       y = "rank web",
       title =  "surprise - dueling-bandit~web correlation")

ggsave("rank-cor/surprise-dueling-survey.png", width = 4, height = 4)


###################
# correlations
###################
require(cocor)
library("car")



# Shapiro-Wilk test can be performed as follow:
#   Null hypothesis: the data are normally distributed
#   Alternative hypothesis: the data are not normally distributed


shapiro.test(rankd.df$rank.cnt.web)
# W = 0.89054, p-value = 0.01621
shapiro.test(rankd.df$rank.cnt.survey)
#W = 0.95247, p-value = 0.329
shapiro.test(rankd.df$rank.borda)
# W = 0.97274, p-value = 0.7542

## shapiro-wilk test looks good 2 and 3, 1 bad

qqPlot(rankd.df$rank.cnt.web)  # looks good
qqPlot(rankd.df$rank.cnt.survey)  # looks good
qqPlot(rankd.df$rank.borda)  # looks good

#######################
## pearson correlation
cor.web.survey <- cor.test(rankd.df$rank.cnt.web,
                           rankd.df$rank.cnt.survey, 
                           method = "pearson")
cor.web.survey
# 0.02695644 
# t = 0.12357, df = 21, p-value = 0.9028


cor.web.borda <- cor.test(rankd.df$rank.cnt.web,
                          rankd.df$rank.borda, 
                          method = "pearson")
cor.web.borda
# -0.1839988
# t = -0.85783, df = 21, p-value = 0.4007

########################
# comparing correlations
########################

cocor(~rank.cnt.web + rank.cnt.survey | rank.cnt.web + rank.borda, rankd.df)

## null hypothesis is retained (no difference)

#####################
#
#####################
# 
# library(trafo)
# 
# #####################
# ## survey ~ web model
# linMod.survey <- lm(rank.cnt.web ~ rank.cnt.survey, data = rankd.df)
# assumptions(linMod.survey)
# # 
# # Test normality assumption 
# # # Skewness Kurtosis Shapiro_W Shapiro_p
# # logshiftopt    -0.2576   2.8675    0.9641    0.7626
# # boxcox         -0.2201   2.8178    0.9604    0.6991
# # bickeldoksum   -0.2201   2.8178    0.9604    0.6991
# 
# linMod.survey.trafo <- trafo_lm(linMod.survey, trafo = 'logshiftopt')
# 
# diagnostics(linMod.survey.trafo)
# # 
# # Residual diagnostics:
# #   
# #   Normality:
# #   Pearson residuals:
# #   Skewness Kurtosis Shapiro_W Shapiro_p
# # Untransformed model  0.2702623 3.099917 0.9581790 0.6607850
# # Transformed model   -0.2576453 2.867493 0.9640651 0.7625752
# # 
# # Heteroscedasticity:
# #   BreuschPagan_V BreuschPagan_p
# # Untransformed model      1.1182194      0.2903022
# # Transformed model        0.2162669      0.6418992
# 
# plot(linMod.survey.trafo)
# 
# assumptions(linMod.survey.trafo)
# 
# summary(linMod.survey.trafo)
# 
# ## transformed 
# # Coefficients:
# #   Estimate Std. Error t value Pr(>|t|)  
# # (Intercept)       3542.9     2823.6   1.255    0.232  
# # rank.cnt.survey    741.1      257.8   2.875    0.013 *  
# 
# 
# ## not transformed
# # 
# # Coefficients:
# #   Estimate Std. Error t value Pr(>|t|)    
# # (Intercept)      9.35739    0.15348  60.970   <2e-16 ***
# #   rank.cnt.survey  0.04194    0.01401   2.994   0.0104 * 
# 
# 
# ########################
# # borda ~ web
# 
# linMod.borda <- lm(rank.cnt.web ~ rank.borda, data = rankd.df)
# assumptions(linMod.borda)
# 
# # Test normality assumption 
# # # Skewness Kurtosis Shapiro_W Shapiro_p
# # logshiftopt     0.3702   2.8199    0.9408    0.3929
# # dual            0.3945   2.9120    0.9403    0.3862
# # boxcox          0.3953   2.9117    0.9403    0.3861
# 
# linMod.borda.trafo <- trafo_lm(linMod.borda, trafo = 'logshiftopt')
# 
# diagnostics(linMod.borda.trafo)
# # 
# # Normality:
# #   Pearson residuals:
# #   Skewness Kurtosis Shapiro_W Shapiro_p
# # Untransformed model 0.9356277 3.152598 0.9037115 0.1084609
# # Transformed model   0.3701901 2.819947 0.9408301 0.3929229
# # 
# # Heteroscedasticity:
# #   BreuschPagan_V BreuschPagan_p
# # Untransformed model      0.2007115      0.6541471
# # Transformed model        0.9270686      0.3356258
# 
# plot(linMod.borda.trafo)
# 
# 
# assumptions(linMod.borda.trafo)
# 
# 
# summary(linMod.borda.trafo)
# 
# ## transformed
# # Coefficients:
# #   Estimate Std. Error t value Pr(>|t|)    
# # (Intercept) 9.195852   0.200630  45.835 9.24e-16 ***
# #   rank.borda  0.020080   0.009756   2.058   0.0602 .  
# 
# # # 
# # # ## NOT transformed
# # Coefficients:
# #   Estimate Std. Error t value Pr(>|t|)  
# # (Intercept)   6304.2     3072.7   2.052   0.0609 .
# # rank.borda     245.4      149.4   1.643   0.1244   