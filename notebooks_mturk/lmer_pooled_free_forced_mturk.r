library(tidyverse)
library(svglite)
library(equatiomatic)
library(ggforce)
library(papaja)
library(rstatix)

df.free = read_csv("../clean_data_mturk/free_labeling_emotion_mturk_long_format_lmer.csv")
df.forced = read_csv("../clean_data_mturk/forced_choice_emotion_mturk_long_format_lmer.csv")

## match spelling
df.forced$emotion <- tolower(df.forced$emotion)
df.free$emotion <- tolower(df.free$emotion)

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

sum(df.free$emotion == 'anger') # 848
sum(df.free$emotion == 'disgust') # 536
sum(df.free$emotion == 'fear') # 356
sum(df.free$emotion == 'happiness') # 1064
sum(df.free$emotion == 'neutral') # 181
sum(df.free$emotion == 'sadness') # 1195
sum(df.free$emotion == 'surprise') # 530

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


### get mathematical formula
formula_lmer <- extract_eq(m1)

cat(formula_lmer, file = "lmer_output/formula_log_lmer_mturk.txt")
cat(formula_lmer, file = "../../emotions_dashboard/data/formula_log_lmer_mturk.txt")

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


s <- svgstring(width = 7,
               height = 5)

plot_model(m1, type = "pred", terms = "condition.dummy")

svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/predicted_prob_mturk_raw.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/predicted_prob_mturk_raw.txt")

dev.off()

tab_model(m1)
tab_model(m1,transform =  "plogis")


### get coefficient table for reporting
tab_model(m1, transform =  "plogis", file = "lmer_output/lmer_summary_free_vs_forced_mturk_raw.html")
tab_model(m1, transform =  "plogis", file = "../../emotions_dashboard/data/lmer_summary_free_vs_forced_mturk_raw.html")



######################
######################

# bar plots comparison

######################
######################

###############
# correct by survey method

correct.survey <- df %>%
  group_by(condition) %>%
  get_summary_stats(correct, type = "mean_se")

correct.survey

names(correct.survey)[4] <- "correct"


correct.survey.plot <- ggplot(correct.survey, aes(x=condition, y=correct)) + 
                              geom_bar(position=position_dodge(), stat="identity") +
                              geom_errorbar(aes(ymin=correct-se, ymax=correct+se),
                                            width=.2,                    # Width of the error bars
                                            position=position_dodge(.9)) + 
                              theme_apa()


correct.survey.plot

ggsave('accuracy-charts/correct-survey-raw.png', width = 4, height = 4)

s <- svgstring(width = 7,
               height = 5)

correct.survey.plot

svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/correct-survey_mturk_raw.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/correct-survey_mturk_raw.txt")

dev.off()



###############
# correct by emotion

correct.label <- df %>%
  group_by(label) %>%
  get_summary_stats(correct, type = "mean_se")

correct.label

names(correct.label)[4] <- "correct"

correct.label.plot <- ggplot(correct.label, aes(x = reorder(label, -correct), y=correct)) + 
                              geom_bar(position=position_dodge(), stat="identity") +
                              geom_errorbar(aes(ymin=correct-se, ymax=correct+se),
                                            width=.2,                    # Width of the error bars
                                            position=position_dodge(.9)) +
                              labs(x = "expected emotion label") + 
                              theme_apa()


correct.label.plot

s <- svgstring(width = 7,
               height = 5)

correct.label.plot

svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/correct-label_mturk_raw.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/correct-label_mturk_raw.txt")

dev.off()


ggsave('accuracy-charts/correct-survey-emotion-raw.png', width = 6, height = 4)


###############
# correct proportion by emotion and condition


correct.survey.label <- df %>%
  group_by(condition, label) %>%
  get_summary_stats(correct, type = "mean_se")

correct.survey.label

names(correct.survey.label)[5] <- "correct"


correct.survey.label.plot <- ggplot(correct.survey.label, aes(x = reorder(label, -correct), y=correct, fill=condition)) + 
                        geom_bar(position=position_dodge(), stat="identity") +
                        geom_errorbar(aes(ymin=correct-se, ymax=correct+se),
                                      width=.2,                    # Width of the error bars
                                      position=position_dodge(.9)) +
                        labs(x = "expected emotion label") + 
                        theme_apa()


correct.survey.label.plot

s <- svgstring(width = 7,
               height = 5)

correct.survey.label.plot

svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/correct-label-survey_mturk_raw.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/correct-label-survey_mturk_raw.txt")

dev.off()


ggsave('accuracy-charts/correct-label-survey-raw.png', width = 8, height = 4)


