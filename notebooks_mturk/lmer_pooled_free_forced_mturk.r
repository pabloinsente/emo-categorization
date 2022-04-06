library(tidyverse)

df.free = read_csv("../clean_data_mturk/free_labeling_emotion_mturk_long_format_lmer.csv")
df.forced = read_csv("../clean_data_mturk/forced_choice_emotion_mturk_long_format_lmer.csv")

## match spelling
df.forced$emotion <- tolower(df.forced$emotion)

## remove uncertain as it means "I don't know" 
df.free <- subset(df.free, label!="uncertain")
df.free <- subset(df.free, emotion!="uncertain")

df.forced <- subset(df.forced, label!="uncertain")
df.forced <- subset(df.forced, emotion!="uncertain")

##########################
# Forced-choice pre-processing
##########################

table(df.forced$emotion)
table(df.forced$label)


## add target 
df.forced$correct <- ifelse(df.forced$emotion == df.forced$label, 1, 0)

## add between subjects predictor 
df.forced$condition <- "forced"
df.forced$condition.dummy <- 0
df.forced$condition.center <- -.5


head(df.forced)

##########################
# Free-choice pre-processing
##########################


dim(table(df.free$emotion)) # 1081
table(df.free$label)

head(df.free)

## add target 
df.free$correct <- ifelse(df.free$emotion == df.free$label, 1, 0)

## add between subjects predictor 
df.free$condition <- "free"
df.free$condition.dummy <- 1
df.free$condition.center <- .5

head(df.free)


###################
# Comparison 
###################

mean(df.forced$correct)
mean(df.free$correct)


##################
# join dataframes for lmer
##################

df.free$participantId <- df.free$participantId + 100 

dim(table(df.forced$photoId)) # 168
dim(table(df.free$photoId))  # 670

df.forced$photoId <- gsub("\\..*","",df.forced$photoId)
df.free$photoId <- gsub("\\..*","", df.free$photoId)

dim(table(df.forced$photoId)) # 168
dim(table(df.free$photoId))  # 168

df <- rbind(df.forced, df.free)

# random variables as factors
df$participantIdF <- as.factor(df$participantId)
df$photoIdF <- as.factor(df$photoId)

####################
# LMER
####################

library(lme4)

# Full model:
# - repeated measures for participantId
# - repeated measures for photId

## dummy coded predictor
m1 <- glmer(correct ~ 1 + condition.dummy + (1 | participantIdF) +  (1 | photoIdF),
            data = df,
            family = binomial) 

summary(m1)

fix.effect = -2.5125
## odd ratio
exp(fix.effect) # 0.081
## probability
plogis(fix.effect) # 0.074

## centered  predictor
m2 <- glmer(correct ~ 1 + condition.center + (1 | participantIdF)  + (1 | photoIdF),
            data = df,
            family = binomial) 

summary(m2)

fix.effect = -2.51
## odd ratio
exp(fix.effect) # 0.081
## probability
plogis(fix.effect) # 0.075


## Notes about interpretation:
# https://stats.stackexchange.com/questions/365907/interpretation-of-fixed-effects-from-mixed-effect-logistic-regression
# https://stats.oarc.ucla.edu/other/mult-pkg/introduction-to-generalized-linear-mixed-models/


###################
# plots of effects
###################
library(sjPlot)
library(sjlabelled)
library(sjmisc)
library(ggplot2)

plot_model(m1)
plot_model(m1, vline.color = "red")
plot_model(m1, transform = "plogis", show.values = TRUE, value.offset = .3)
plot_model(m1, show.values = TRUE, value.offset = .3)
plot_model(m1, type = "pred", terms = "condition.dummy")
plot_model(m1, type = "emm", terms = "condition.dummy")

tab_model(m1)
tab_model(m2)


######################
# bar plots comparison
######################

## Gives count, mean, standard deviation, standard error of the mean, and confidence interval (default 95%).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   groupvars: a vector containing names of columns that contain grouping variables
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- plyr::rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

## Norms the data within specified groups in a data frame; it normalizes each
## subject (identified by idvar) so that they have the same mean, within each group
## specified by betweenvars.
##   data: a data frame.
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   na.rm: a boolean that indicates whether to ignore NA's
normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}

## Summarizes data, handling within-subjects variables by removing inter-subject variability.
## It will still work if there are no within-S variables.
## Gives count, un-normed mean, normed mean (with same between-group mean),
##   standard deviation, standard error of the mean, and confidence interval.
## If there are within-subject variables, calculate adjusted values using method from Morey (2008).
##   data: a data frame.
##   measurevar: the name of a column that contains the variable to be summariezed
##   betweenvars: a vector containing names of columns that are between-subjects variables
##   withinvars: a vector containing names of columns that are within-subjects variables
##   idvar: the name of a column that identifies each subject (or matched subjects)
##   na.rm: a boolean that indicates whether to ignore NA's
##   conf.interval: the percent range of the confidence interval (default is 95%)
summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}


###############
# correct by survey method

correct.survey <- summarySE(df, measurevar="correct", groupvars=c("condition"))

correct.survey.plot <- ggplot(correct.survey, aes(x=condition, y=correct)) + 
                              geom_bar(position=position_dodge(), stat="identity") +
                              geom_errorbar(aes(ymin=correct-se, ymax=correct+se),
                                            width=.2,                    # Width of the error bars
                                            position=position_dodge(.9))

correct.survey.plot

ggsave('accuracy-charts/correct-survey.png', width = 4, height = 4)


###############
# correct by emotion

correct.label <- summarySE(df, measurevar="correct", groupvars=c("label"))

correct.label

correct.label.plot <- ggplot(correct.label, aes(x = reorder(label, -correct), y=correct)) + 
                              geom_bar(position=position_dodge(), stat="identity") +
                              geom_errorbar(aes(ymin=correct-se, ymax=correct+se),
                                            width=.2,                    # Width of the error bars
                                            position=position_dodge(.9)) +
                              labs(x = "expected emotion label")

correct.label.plot

ggsave('accuracy-charts/correct-survey-emotion.png', width = 6, height = 4)


###############
# correct proportion by emotion and condition


correct.survey.label <-  summarySEwithin(df, measurevar="correct",
                        betweenvars="condition",
                        withinvars=c("label"), 
                        idvar="participantId")


correct.survey.label.plot <- ggplot(correct.survey.label, aes(x = reorder(label, -correct), y=correct, fill=condition)) + 
                        geom_bar(position=position_dodge(), stat="identity") +
                        geom_errorbar(aes(ymin=correct-se, ymax=correct+se),
                                      width=.2,                    # Width of the error bars
                                      position=position_dodge(.9)) +
                        labs(x = "expected emotion label")

correct.survey.label.plot

ggsave('accuracy-charts/correct-label-survey.png', width = 8, height = 4)

# ####################################
# # LMER adding ethnicity as covariate
# ####################################
# 
# ## dummy coded predictor
# m3 <- glmer(correct ~ 1 + condition.center * ethnicityC + (1 + ethnicityC| participantIdF) +(1 | photoIdF),
#             data = df,
#             family = binomial) 
# 
# summary(m3)
# 
# fix.effect = -3.376
# ## odd ratio
# exp(fix.effect) # 0.03415317
# ## probability
# plogis(fix.effect) # 0.03302525
# 
# 
# #################
# ## interpretation
# 
# # participants in the free-response format are expected to have b=-3.37626 lower log odds of "correctly" 
# # answering the expected emotion label, p < 0.001, holding constant the random-effects 
# # for participantId and photoId, and fixed-effect for ethnicity
# #
# # Or, the probability of correctly answering in the free-response condition is only 3%, holding
# # constant the random-effects for participantId and photoId, and fixed-effect for ethnicity
# 
# plot_model(m3, show.values = TRUE, value.offset = .3)
# plot_model(m3, type = "pred", terms = c("condition.center", "ethnicityC"))
# 
# 
# ####################################
# # LMER adding sex as covariate
# ####################################
# 
# ## dummy coded predictor
# m4 <- glmer(correct ~ 1 + condition.center * sexC + (1 + sexC| participantIdF) +(1 | photoIdF),
#             data = df,
#             family = binomial) 
# 
# summary(m4)
# 
# fix.effect = -3.378
# ## odd ratio
# exp(fix.effect) # 0.0341
# ## probability
# plogis(fix.effect) # 0.0330
# 
# 
# #################
# ## interpretation
# 
# # participants in the free-response format are expected to have b=-3.3782 lower log odds of "correctly" 
# # answering the expected emotion label, p < 0.001, holding constant the random-effects 
# # for participantId and photoId, and fixed-effect for sex
# #
# # Or, the probability of correctly answering in the free-response condition is only 3%, holding
# # constant the random-effects for participantId and photoId, and fixed-effect for sex
# 
# plot_model(m4, show.values = TRUE, value.offset = .3)
# plot_model(m4, type = "pred", terms = c("condition.center", "sexC"))
# 
# 
# 
# 
