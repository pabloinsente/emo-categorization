library(tidyverse)
library(svglite)
library(equatiomatic)
library(rjson)
library(ggforce)
library(papaja)
library(rstatix)

df.free = read_csv("../clean_data/free_labeling_emotion_uw_students_long_format_lmer.csv")
df.forced = read_csv("../clean_data/forced_choice_emotion_uw_students_long_format_lmer.csv")

syns = fromJSON(file = "../clean_data/syn_dict_emotions.json")

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

table(df.free$emotion)

dim(table(df.free$emotion)) # 1056
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

sum(df.free$emotion == 'anger') # 435
sum(df.free$emotion == 'disgust') # 294
sum(df.free$emotion == 'fear') # 31
sum(df.free$emotion == 'happiness') # 830
sum(df.free$emotion == 'neutral') # 16
sum(df.free$emotion == 'sadness') # 749
sum(df.free$emotion == 'surprise') # 262

mean(df.forced$correct) # 65%
mean(df.free$correct) # 13%


##################
# join dataframes for lmer
##################

df.free$participantId <- df.free$participantId + 100 

dim(table(df.forced$photoId)) # 168
dim(table(df.free$photoId))  # 627

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

fix.effect = -3.5225
## odd ratio
exp(fix.effect) # 0.029
## probability
plogis(fix.effect) # 0.0286

## centered  predictor
m2 <- glmer(correct ~ 1 + condition.center + (1 | participantIdF)  + (1 | photoIdF),
            data = df,
            family = binomial) 

summary(m2)

fix.effect = -3.5225
## odd ratio
exp(fix.effect) # 0.029
## probability
plogis(fix.effect) # 0.0286


## Notes about interpretation:
# https://stats.stackexchange.com/questions/365907/interpretation-of-fixed-effects-from-mixed-effect-logistic-regression
# https://stats.oarc.ucla.edu/other/mult-pkg/introduction-to-generalized-linear-mixed-models/


### get mathematical formula
formula_lmer <- extract_eq(m1)

cat(formula_lmer, file = "lmer_output/formula_log_lmer_uw_students.txt")
cat(formula_lmer, file = "../../emotions_dashboard/data/formula_log_lmer_uw_students.txt")


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


s <- svgstring(width = 7,
               height = 5)

plot_model(m1, type = "pred", terms = "condition.dummy")

svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/predicted_prob_uw_students_raw.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/predicted_prob_uw_students_raw.txt")

dev.off()

tab_model(m1)
tab_model(m1,transform =  "plogis")


### get coefficient table for reporting
tab_model(m1, transform =  "plogis", file = "lmer_output/lmer_summary_free_vs_forced_uw_students_raw.html")
tab_model(m1, transform =  "plogis", file = "../../emotions_dashboard/data/lmer_summary_free_vs_forced_uw_students_raw.html")

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


s <- svgstring(width = 7,
               height = 5)

correct.survey.plot

svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/correct-survey_uw_students_raw.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/correct-survey_uw_students_raw.txt")

dev.off()

ggsave('accuracy-charts/correct-survey-raw.png', width = 4, height = 4)

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

cat(svg.string.plot, file = "lmer_output/correct-label_uw_students_raw.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/correct-label_uw_students_raw.txt")

dev.off()


ggsave('accuracy-charts/correct-label-raw.png', width = 6, height = 4)


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

cat(svg.string.plot, file = "lmer_output/correct-label-survey_uw_students_raw.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/correct-label-survey_uw_students_raw.txt")

dev.off()


ggsave('accuracy-charts/correct-label-survey-raw.png', width = 8, height = 4)
