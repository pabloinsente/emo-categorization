library(tidyverse)
library(svglite)
library(equatiomatic)
library(ggforce)
library(papaja)
library(rstatix)
library(rjson)

df.free = read_csv("../clean_data/free_labeling_emotion_uw_students_long_format_lmer.csv")
df.forced = read_csv("../clean_data/forced_choice_emotion_uw_students_long_format_lmer.csv")

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

##########################
# Forced-choice pre-processing
##########################

table(df.forced$emotion)
table(df.forced$label)


## add target 
df.forced$correct <- ifelse(df.forced$emotion == df.forced$label, 1, 0)

## add between subjects predictor 
df.forced$condition <- "forced"
df.forced$condition.dummy <- 1
df.forced$condition.center <- .5


head(df.forced)
n_distinct(df.free$emotion) # 445 distinct emotion words
length(df.free$emotion) # 12103


##########################
# Free-choice pre-processing
##########################
library(plyr)

####################
# group by synonyms


## baseline count 
sum(df.free$emotion == 'anger') # 435
sum(df.free$emotion == 'disgust') # 294
sum(df.free$emotion == 'fear') # 30
sum(df.free$emotion == 'happiness') # 830
sum(df.free$emotion == 'neutral') # 16
sum(df.free$emotion == 'sadness') # 749
sum(df.free$emotion == 'surprise') # 262

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


## after replacing with synsets
sum(df.free$emotion == 'anger') # 716
sum(df.free$emotion == 'disgust') # 385
sum(df.free$emotion == 'fear') # 38
sum(df.free$emotion == 'happiness') # 1254
sum(df.free$emotion == 'neutral') # 57
sum(df.free$emotion == 'sadness') # 1027
sum(df.free$emotion == 'surprise') # 490


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
sum(df.free$emotion == 'anger') # 732
sum(df.free$emotion == 'disgust') # 478
sum(df.free$emotion == 'fear') # 69
sum(df.free$emotion == 'happiness') # 1256
sum(df.free$emotion == 'neutral') # 57
sum(df.free$emotion == 'sadness') # 1051
sum(df.free$emotion == 'surprise') # 490


################
# scared is not among synsets or hyponyms 
df.free$emotion <- mapvalues(df.free$emotion, from="scared", to="fear")

sum(df.free$emotion == 'fear') # 408


dim(table(df.free$emotion)) # 457
table(df.free$label)

# anger   disgust      fear happiness   neutral   sadness  surprise 
# 1717      1701      1829      1748      1479      1853      1776 

## add target 
df.free$correct <- ifelse(df.free$emotion == df.free$label, 1, 0)

## add between subjects predictor 
df.free$condition <- "free"
df.free$condition.dummy <- 0
df.free$condition.center <- -.5

head(df.free)
n_distinct(df.free$emotion) # 445 distinct emotion words


table(df.free$emotion)

###################
# Comparison 
###################

mean(df.forced$correct) # 0.6453202
mean(df.free$correct) # 0.2318433


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

# save for pooling with mturk sample
write_csv(df, "../clean_data/accuracy_grouped.csv")

####################
# LMER
####################

library(lme4)

# Full model:
# - repeated measures for participantId
# - repeated measures for photId

## dummy coded predictor


df <- as.data.frame(df)


m1 <- glmer(correct ~ 1 + condition.dummy + (1 | participantIdF) +  (1 | photoIdF),
            data = df,
            family = binomial) 

summary(m1)

fix.effect = 2.17
## odd ratio
exp(fix.effect) #  8.67
## probability
plogis(fix.effect) # 0.89

car::Anova(m1, type=3)
# 
# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: correct
# Chisq Df Pr(>Chisq)    
# (Intercept)      94.238  1  < 2.2e-16 ***
# condition.dummy 185.024  1  < 2.2e-16 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

## centered  predictor
m2 <- glmer(correct ~ 1 + condition.center + (1 | participantIdF)  + (1 | photoIdF),
            data = df,
            family = binomial) 

summary(m2)

fix.effect = 2.17
## odd ratio
exp(fix.effect) #  8.67
## probability
plogis(fix.effect) # 0.89



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

cat(svg.string.plot, file = "lmer_output/predicted_prob_uw_students.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/predicted_prob_uw_students.txt")

dev.off()


### get coefficient table for reporting

tab_model(m2, 
          pred.labels = c("Intercept",
                          "Survey condition [.5 = forced-choice]"),
          file = "../../emotions_dashboard/data/lmer_summary_odds_free_vs_forced_uw_students.html")

tab_model(m2, 
          transform =  "plogis",
          pred.labels = c("Intercept",
                          "Survey condition [.5 = forced-choice]"),
          file = "../../emotions_dashboard/data/lmer_summary_free_vs_forced_uw_students.html")


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
# 
# condition variable     n correct    se
# <chr>     <chr>    <dbl>   <dbl> <dbl>
# 1 forced    correct   8120   0.645 0.005
# 2 free      correct  12103   0.232 0.004


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

cat(svg.string.plot, file = "lmer_output/correct-survey_uw_students.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/correct-survey_uw_students.txt")

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

cat(svg.string.plot, file = "lmer_output/correct-label_uw_students.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/correct-label_uw_students.txt")

dev.off()


ggsave('accuracy-charts/correct-label.png', width = 6, height = 4)


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

cat(svg.string.plot, file = "lmer_output/correct-label-survey_uw_students.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/correct-label-survey_uw_students.txt")

dev.off()


ggsave('accuracy-charts/correct-label-survey.png', width = 8, height = 4)



