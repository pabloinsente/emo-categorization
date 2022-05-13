library(tidyverse)
library(svglite)
library(equatiomatic)
library(ggforce)
library(papaja)
library(rstatix)
library(rjson)

df.free = read_csv("../clean_data_mturk/free_labeling_emotion_mturk_long_format_lmer.csv")
df.forced = read_csv("../clean_data_mturk/forced_choice_emotion_mturk_long_format_lmer.csv")

syns = fromJSON(file = "../clean_data/syn_dict_emotions.json")
hyps = fromJSON(file = "../clean_data/hyp_dict_emotions.json")

## match spelling
df.forced$emotion <- tolower(df.forced$emotion)
df.free$emotion <- tolower(df.free$emotion)

## remove uncertain as it means "I don't know" 
df.free <- subset(df.free, label!="uncertain")
df.free <- subset(df.free, emotion!="uncertain")

df.forced <- subset(df.forced, label!="uncertain")
df.forced <- subset(df.forced, emotion!="uncertain")

##############################
# Forced-choice pre-processing
##############################

table(df.forced$emotion)
table(df.forced$label)


## add target 
df.forced$correct <- ifelse(df.forced$emotion == df.forced$label, 1, 0)

## add between subjects predictor 
df.forced$condition <- "forced"
df.forced$condition.dummy <- 1
df.forced$condition.center <- .5


head(df.forced)
n_distinct(df.free$emotion) # 449 distinct emotion words
length(df.free$emotion) # 12674


##########################
# Free-choice pre-processing
##########################
library(plyr)

####################
# group by synonyms


## baseline count 
sum(df.free$emotion == 'anger') # 848
sum(df.free$emotion == 'disgust') # 536
sum(df.free$emotion == 'fear') # 356
sum(df.free$emotion == 'happiness') # 1064
sum(df.free$emotion == 'neutral') # 181
sum(df.free$emotion == 'sadness') # 1195
sum(df.free$emotion == 'surprise') # 530


######################
# Replace cog synonyms


# anger
n = length(syns$anger)
from_words = syns$anger
to_word = replicate(n, 'anger')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)
# angry
n = length(syns$angry)
from_words = syns$angry
to_word = replicate(n, 'anger')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)

# disgust
n = length(syns$disgust)
from_words = syns$disgust
to_word = replicate(n, 'disgust')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)
# disgusted
n = length(syns$disgusted)
from_words = syns$disgusted
to_word = replicate(n, 'disgust')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)

# sadness
n = length(syns$sadness)
from_words = syns$sadness
to_word = replicate(n, 'sadness')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)
# sad
n = length(syns$sad)
from_words = syns$sad
to_word = replicate(n, 'sadness')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)


# fear
n = length(syns$fear)
from_words = syns$fear
to_word = replicate(n, 'fear')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)
# fearful
n = length(syns$fearful)
from_words = syns$fearful
to_word = replicate(n, 'fear')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)

# surprise
n = length(syns$surprise)
from_words = syns$surprise
to_word = replicate(n, 'surprise')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)
# surprised
n = length(syns$surprised)
from_words = syns$surprised
to_word = replicate(n, 'surprise')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)


# happiness
n = length(syns$happiness)
from_words = syns$happiness
to_word = replicate(n, 'happiness')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)
# happy
n = length(syns$happy)
from_words = syns$happy
to_word = replicate(n, 'happiness')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)

# neutral
n = length(syns$neutral)
from_words = syns$neutral
to_word = replicate(n, 'neutral')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)

sum(df.free$emotion == 'anger') # 953
sum(df.free$emotion == 'disgust') # 576
sum(df.free$emotion == 'fear') # 383
sum(df.free$emotion == 'happiness') # 1258
sum(df.free$emotion == 'neutral') # 208
sum(df.free$emotion == 'sadness') # 1366
sum(df.free$emotion == 'surprise') # 601


dim(table(df.free$emotion)) # (before 847) 434

###################
## replace hyponyms


# anger
n = length(hyps$anger)
from_words = hyps$anger
to_word = replicate(n, 'anger')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)
# neutral 
n = length(hyps$neutral)
from_words = hyps$neutral
to_word = replicate(n, 'neutral')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)
# disgust
n = length(hyps$disgust)
from_words = hyps$disgust
to_word = replicate(n, 'disgust')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)
# fear
n = length(hyps$fear)
from_words = hyps$fear
to_word = replicate(n, 'fear')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)
# happiness
n = length(hyps$happiness)
from_words = hyps$happiness
to_word = replicate(n, 'happiness')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)
# sadness
n = length(hyps$sadness)
from_words = hyps$sadness
to_word = replicate(n, 'sadness')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)
# surprise
n = length(hyps$surprise)
from_words = hyps$surprise
to_word = replicate(n, 'surprise')
df.free$emotion  <- mapvalues(df.free$emotion, from=from_words, to=to_word)


## after replacing with hyponyms too
sum(df.free$emotion == 'anger') # 980
sum(df.free$emotion == 'disgust') # 695
sum(df.free$emotion == 'fear') # 394
sum(df.free$emotion == 'happiness') # 1258
sum(df.free$emotion == 'neutral') # 208
sum(df.free$emotion == 'sadness') # 1375
sum(df.free$emotion == 'surprise') # 601

################
# scared is not among synsets or hyponyms 
df.free$emotion <- mapvalues(df.free$emotion, from="scared", to="fear")

sum(df.free$emotion == 'fear') # 639


dim(table(df.free$emotion)) # 422
table(df.free$label)

# anger   disgust      fear happiness   neutral   sadness  surprise 
# 1811      1793      1951      1837      1550      1909      1823 

## add target 
df.free$correct <- ifelse(df.free$emotion == df.free$label, 1, 0)

## add between subjects predictor 
df.free$condition <- "free"
df.free$condition.dummy <- 0
df.free$condition.center <- -.5

head(df.free)
n_distinct(df.free$emotion) # 434 distinct emotion words


###################
# Comparison 
###################

mean(df.forced$correct) # 0.59
mean(df.free$correct) # 0.26


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


# save for pooling with espanol sample
write_csv(df, "../clean_data_mturk/accuracy_grouped_mturk.csv")

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

fix.effect = 1.6254
## odd ratio
exp(fix.effect) # 5.080
## probability
plogis(fix.effect) # 0.835

car::Anova(m1, type=3)

# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: correct
# Chisq Df Pr(>Chisq)    
# (Intercept)     55.013  1  1.197e-13 ***
# condition.dummy 74.111  1  < 2.2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## centered  predictor
m2 <- glmer(correct ~ 1 + condition.center + (1 | participantIdF)  + (1 | photoIdF),
            data = df,
            family = binomial) 

summary(m2)

fix.effect = 1.6254
## odd ratio
exp(fix.effect) # 5.080
## probability
plogis(fix.effect) # 0.835

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
library(ggplot2)
library(papaja)

survey.main <- plot_model(m2, type = "eff", terms = c("condition.center")) +
  labs(x = "survey method", color="") + 
  theme_apa()
survey.main


s <- svgstring(width = 7,
               height = 5)
survey.main
svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/predicted_prob_mturk.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/predicted_prob_mturk.txt")

dev.off()


### get coefficient table for reporting

tab_model(m2, 
          pred.labels = c("Intercept",
                          "Survey condition [.5 = forced-choice]"),
          file = "../../emotions_dashboard/data/lmer_summary_odds_free_vs_forced_mturk.html")

tab_model(m2, 
          transform =  "plogis",
          pred.labels = c("Intercept",
                          "Survey condition [.5 = forced-choice]"),
          file = "../../emotions_dashboard/data/lmer_summary_free_vs_forced_mturk.html")

######################
# bar plots comparison
######################


library(rstatix)

###############
# correct by survey method


correct.survey <- df %>%
  group_by(condition) %>%
  get_summary_stats(correct, type = "mean_se")

correct.survey

names(correct.survey)[4] <- "correct"


correct.survey.plot <- ggplot(correct.survey, aes(x=condition, y=correct)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  # geom_errorbar(aes(ymin=correct-se, ymax=correct+se),
  #               width=.2,                    # Width of the error bars
  #               position=position_dodge(.9)) +
  labs(x = "survey condition",
       title = "Correct responses grouped by Wordnet synonyms") + 
  theme_apa()


correct.survey.plot


s <- svgstring(width = 7,
               height = 5)

correct.survey.plot

svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/correct-survey_mturk.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/correct-survey_mturk.txt")

dev.off()

ggsave('accuracy-charts/correct-survey.png', width = 4, height = 4)


###############
# correct by emotion

correct.label <- df %>%
  group_by(label) %>%  get_summary_stats(correct, type = "mean_se")

correct.label

names(correct.label)[4] <- "correct"

correct.label.plot <- ggplot(correct.label, aes(x = reorder(label, -correct), y=correct)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  # geom_errorbar(aes(ymin=correct-se, ymax=correct+se),
  #               width=.2,                    # Width of the error bars
  #               position=position_dodge(.9)) +
  labs(x = "expected emotion label",
       title = "Correct responses grouped by Wordnet synonyms") + 
  theme_apa()


correct.label.plot

s <- svgstring(width = 7,
               height = 5)

correct.label.plot

svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/correct-label_mturk.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/correct-label_mturk.txt")

dev.off()


ggsave('accuracy-charts/correct-survey-emotion.png', width = 6, height = 4)


###############
# correct proportion by emotion and condition


correct.survey.label <- df %>%
  group_by(condition, label) %>%  get_summary_stats(correct, type = "mean_se")

correct.survey.label

names(correct.survey.label)[5] <- "correct"


correct.survey.label.plot <- ggplot(correct.survey.label, aes(x = reorder(label, -correct), y=correct, fill=condition)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  # geom_errorbar(aes(ymin=correct-se, ymax=correct+se),
  #               width=.2,                    # Width of the error bars
  #               position=position_dodge(.9)) +
  labs(x = "expected emotion label",
       title = "Correct responses grouped by Wordnet synonyms") + 
  theme_apa()

correct.survey.label.plot

s <- svgstring(width = 7,
               height = 5)

correct.survey.label.plot

svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/correct-label-survey_mturk.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/correct-label-survey_mturk.txt")

dev.off()

ggsave('accuracy-charts/correct-label-survey.png', width = 8, height = 4)
