library(tidyverse)
library(svglite)
library(equatiomatic)
library(ggforce)
library(papaja)
library(rstatix)
library(rjson)

df.free = read_csv("../clean_data_mturk_espanol/free_labeling_emotion_mturk_long_format_lmer_espanol.csv")
df.forced = read_csv("../clean_data_mturk_espanol/forced_choice_emotion_mturk_long_format_lmer_espanol.csv")

syns = fromJSON(file = "../clean_data/syn_dict_emotions.json")
hyps = fromJSON(file = "../clean_data/hyp_dict_emotions.json")



df.forced$label <- plyr::mapvalues(df.forced$label, 
                                   from=c("enfado", "felicidad", "asco", "tristeza", "miedo", "sorpresa", "incertidumbre"), 
                                   to=c("anger", "happiness", "disgust", 'sadness', 'fear', 'surprise', 'uncertain'))


df.free$label <- plyr::mapvalues(df.free$label, 
                                 from=c("enfado", "felicidad", "asco", "tristeza", "miedo", "sorpresa", "incertidumbre"), 
                                 to=c("anger", "happiness", "disgust", 'sadness', 'fear', 'surprise', 'uncertain'))



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

##########################
# Free-choice pre-processing
##########################
library(plyr)

####################
# group by synonyms


## baseline count 

sum(df.free$emotion == 'anger') # 669
sum(df.free$emotion == 'disgust') # 372
sum(df.free$emotion == 'fear') # 379 (used to be 32)
sum(df.free$emotion == 'happiness') # 627
sum(df.free$emotion == 'neutral') # 146
sum(df.free$emotion == 'sadness') # 657
sum(df.free$emotion == 'surprise') # 828



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

sum(df.free$emotion == 'anger') # 717
sum(df.free$emotion == 'disgust') # 382
sum(df.free$emotion == 'fear') # 451
sum(df.free$emotion == 'happiness') # 1031
sum(df.free$emotion == 'neutral') # 148
sum(df.free$emotion == 'sadness') # 826
sum(df.free$emotion == 'surprise') # 988



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
sum(df.free$emotion == 'anger') # 717 -> 774
sum(df.free$emotion == 'disgust') # 382 -> 428
sum(df.free$emotion == 'fear') # 451 -> 497 
sum(df.free$emotion == 'happiness') # 1031 -> 1031
sum(df.free$emotion == 'neutral') # 148 -> 148
sum(df.free$emotion == 'sadness') # 826 -> 844
sum(df.free$emotion == 'surprise') # 988 -> 988


################
# scared is not among synsets or hyponyms 
df.free$emotion <- mapvalues(df.free$emotion, from="scared", to="fear")

sum(df.free$emotion == 'fear') # 497 ->


dim(table(df.free$emotion)) # 457 -> 279
table(df.free$label)

# anger   disgust   fear   happiness   neutral   sadness  surprise 
# 1125      1273    1364      1140      822      1189      1216

head(df.free)

## add target 
df.free$correct <- ifelse(df.free$emotion == df.free$label, 1, 0)

## add between subjects predictor 
df.free$condition <- "free"
df.free$condition.dummy <- 0
df.free$condition.center <- -.5

head(df.free)

n_distinct(df.free$emotion) # 290 -> 279 distinct emotion words


##################
# join dataframes for lmer
##################

df.free$participantId <- df.free$participantId + 100 

dim(table(df.forced$photoId)) # 168
dim(table(df.free$photoId))  # 504

df.forced$photoId <- gsub("\\..*","",df.forced$photoId)
df.free$photoId <- gsub("\\..*","", df.free$photoId)

dim(table(df.forced$photoId)) # 168
dim(table(df.free$photoId))  # 168

df <- rbind(df.forced, df.free)

# random variables as factors
df$participantIdF <- as.factor(df$participantId)
df$photoIdF <- as.factor(df$photoId)

######################
# join datasets
######################

# read in english sample
df.english = read_csv("../clean_data_mturk/accuracy_grouped_mturk.csv")

head(df)
head(df.english)

## add between subjects predictor 
df$language.condition <- "espanol"
df$language.condition.dummy <- 1
df$language.condition.center <- .5

## add between subjects predictor 
df.english$language.condition <- "english"
df.english$language.condition.dummy <- 0
df.english$language.condition.center <- -.5

# to avoid id duplication
df.english$participantId <- df.english$participantId + 1000 

df.english = subset(df.english, select = -c(sex_participant,ethnicity_participant) )

df.pool <- rbind(df, df.english)

df.pool



###################
# Comparison 
###################
library(crosstable)
library(apaTables)


mean(df$correct) # 0.497


df %>%  group_by(condition) %>%  get_summary_stats(correct, type = "mean_se")

# condition variable     n  mean    se
# <chr>     <chr>    <dbl> <dbl> <dbl>
# 1 forced    correct   5077 0.697 0.006
# 2 free      correct   8129 0.374 0.005

mean(df.english$correct) #0.402

df.english %>%  group_by(condition) %>%  get_summary_stats(correct, type = "mean_se")

# condition variable     n  mean    se
# <chr>     <chr>    <dbl> <dbl> <dbl>
# 1 forced    correct   8350 0.594 0.005
# 2 free      correct  12674 0.275 0.004

####################
# LMER
####################

library(lme4)
library(sjPlot)

# Full model:
# - repeated measures for participantId
# - repeated measures for photId


########################
## dummy coded predictor
#  This test SIMPLE effects
# df.pool$condition.dummy <- to_factor(df.pool$condition.dummy)
# df.pool$language.condition.dummy <- to_factor(df.pool$language.condition.dummy)

# Condition: forced = 1
# Language: espanol =  1
# b1: forced at english
# b2: espanol at free-response

m1 <- glmer(correct ~ 1 + condition.dummy*language.condition.dummy + (1 | participantIdF) +  (1 | photoIdF),
            data = df.pool,
            family = binomial) 

summary(m1)
car::Anova(m1, type="3")
tab_model(m1)

##########
## free vs forced condition
fix.effect = 1.49684
## odd ratio
exp(fix.effect) #4.467
## probability
plogis(fix.effect) # 0.81


##########
## english vs spanish condition
fix.effect = 0.45
## odd ratio
exp(fix.effect) # 1.56
## probability
plogis(fix.effect) # 0.61


##########
## survey-condition * language condition interaction
fix.effect = 0.173
## odd ratio
exp(fix.effect) # 1.18
## probability
plogis(fix.effect) # 0.54


#######################
## centered  predictor
## This test MAIN effects
# df.pool$condition.center <- to_factor(df.pool$condition.center)
# df.pool$language.condition.center <- to_factor(df.pool$language.condition.center)

length(table(df.pool$participantId))


length(table(df.pool$participantIdF))



m2 <- glmer(correct ~ 1 + condition.center*language.condition.center + (1 | participantId)  + (1 | photoIdF),
            data = df.pool,
            family = binomial) 

summary(m2)
car::Anova(m2, type="3")
tab_model(m2)

##########
## free vs forced condition
fix.effect = 1.58
## odd ratio
exp(fix.effect) #4.8
## probability
plogis(fix.effect) # 0.82


##########
## english vs spanish condition
fix.effect = 0.5396
## odd ratio
exp(fix.effect) # 1.71
## probability
plogis(fix.effect) # 0.63


##########
## survey-condition * language condition interaction
fix.effect = 0.173
## odd ratio
exp(fix.effect) # 1.18
## probability
plogis(fix.effect) # 0.54

### get mathematical formula
# formula_lmer <- extract_eq(m2)
# 
# cat(formula_lmer, file = "lmer_output/formula_log_lmer_mturk_espanol_english.txt")
# cat(formula_lmer, file = "../../emotions_dashboard/data/formula_log_lmer_mturk_espanol_english.txt")



## Notes about interpretation:
# https://stats.stackexchange.com/questions/365907/interpretation-of-fixed-effects-from-mixed-effect-logistic-regression
# https://stats.oarc.ucla.edu/other/mult-pkg/introduction-to-generalized-linear-mixed-models/


###################
# plots of effects
###################
library(sjPlot)
library(sjlabelled)
library(papaja)
library(ggplot2)


# odds ratio dots
odds.plot <- plot_model(m2, vline.color = "red", show.values = TRUE, value.offset = .3) +
  theme_apa()
odds.plot

###########################
######## Centerd predictors

# main effect of language
lang.main <- plot_model(m2, type = "eff", terms = c("language.condition.center")) +
  labs(x = "language condition") + 
  theme_apa()

lang.main

# main effect of survey method
survey.main <- plot_model(m2, type = "eff", terms = c("condition.center")) +
  labs(x = "survey method", color="") + 
  theme_apa()

survey.main

# interaction effect language X survey method
inter.effect <- plot_model(m2, type = "eff", terms = c("language.condition.center", "condition.center")) +
  labs(x = "language condition", color="survey method") + 
  theme_apa()

inter.effect

##########################
# save plots for reporting

# Odds ratio plot
s <- svgstring(width = 7,
               height = 5)
odds.plot
svg.string.plot <- s()
cat(svg.string.plot, file = "lmer_output/predicted_odds_mturk_espanol_english.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/predicted_odds_mturk_espanol_english.txt")
dev.off()


# Language main effect
s <- svgstring(width = 7,
               height = 5)
lang.main
svg.string.plot <- s()
cat(svg.string.plot, file = "lmer_output/lang_predicted_prob_mturk_espanol_english.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/lang_predicted_prob_mturk_espanol_english.txt")
dev.off()

# survey method main effect
s <- svgstring(width = 7,
               height = 5)
survey.main
svg.string.plot <- s()
cat(svg.string.plot, file = "lmer_output/survey_predicted_prob_mturk_espanol_english.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/survey_predicted_prob_mturk_espanol_english.txt")
dev.off()

# language X survey method interaction effect
s <- svgstring(width = 7,
               height = 5)
inter.effect
svg.string.plot <- s()
cat(svg.string.plot, file = "lmer_output/interaction_predicted_prob_mturk_espanol_english.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/interaction_predicted_prob_mturk_espanol_english.txt")
dev.off()



### get coefficient table for reporting
# odds ratio

tab_model(m2, 
          pred.labels = c("Intercept",
                  "Survey condition [.5 = forced-choice]",
                  "Language condition [.5 = espanol]",
                  "Interaction survey and language conditions"),
          file = "lmer_output/lmer_summary_odds_free_vs_forced_mturk_espanol_enlish.html")

tab_model(m2, 
          pred.labels = c("Intercept",
                          "Survey condition [.5 = forced-choice]",
                          "Language condition [.5 = espanol]",
                          "Interaction survey and language conditions"),
          file = "../../emotions_dashboard/data/lmer_summary_odds_free_vs_forced_mturk_espanol_enlish.html")


######################
# bar plots comparison
######################

library(rstatix)

###############
# correct by survey method


correct.survey <- df.pool %>%
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
       title = "Correct responses grouped by Wordnet synonyms (pooled surveys)") + 
  theme_apa()



correct.survey.plot

s <- svgstring(width = 7,
               height = 5)

correct.survey.plot

svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/correct_survey_mturk_espanol_english.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/correct_survey_mturk_espanol_english.txt")

dev.off()


ggsave('accuracy-charts/correct-survey-es-eng.png', width = 4, height = 4)


###############
# correct by language condition 


correct.lang<- df.pool %>%
  group_by(language.condition) %>%
  get_summary_stats(correct, type = "mean_se")

correct.lang

names(correct.lang)[4] <- "correct"

correct.lang.plot <- ggplot(correct.lang, aes(x=language.condition, y=correct)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  # geom_errorbar(aes(ymin=correct-se, ymax=correct+se),
  #               width=.2,                    # Width of the error bars
  #               position=position_dodge(.9)) +
  labs(x = "language condition",
       title = "Correct responses grouped by Wordnet synonyms (pooled surveys)") + 
  theme_apa()


correct.lang.plot

s <- svgstring(width = 7,
               height = 5)

correct.lang.plot

svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/correct_lang_mturk_espanol_english.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/correct_lang_mturk_espanol_english.txt")

dev.off()


ggsave('accuracy-charts/correct-survey-lang-es-eng.png', width = 4, height = 4)



###############
# correct proportion by language and survey condition

correct.survey.lang <- df.pool %>%
  group_by(condition, language.condition) %>%  get_summary_stats(correct, type = "mean_se")

correct.survey.lang

names(correct.survey.lang)[5] <- "correct"

correct.survey.lang.plot <- ggplot(correct.survey.lang, aes(x = reorder(language.condition, -correct), y=correct, fill=condition)) + 
  geom_bar(position=position_dodge(), stat="identity", color="black") +
  # geom_errorbar(aes(ymin=correct-se, ymax=correct+se),
  #               width=.2,                    # Width of the error bars
  #               position=position_dodge(.9))+
  labs(x = "language-origin",
       fill="response-format") + 
  theme_apa()

correct.survey.lang.plot


s <- svgstring(width = 7,
               height = 5)

correct.survey.lang.plot

svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/correct_label_survey_lang_mturk_espanol_english.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/correct_label_survey_lang_mturk_espanol_english.txt")

dev.off()

ggsave('accuracy-charts/correct-lang-survey.png', width = 6, height = 4)


