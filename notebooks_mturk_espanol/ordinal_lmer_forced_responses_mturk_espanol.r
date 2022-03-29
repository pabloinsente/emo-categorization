library(tidyverse)
library(ordinal)
library("RColorBrewer")


# df_students = read_csv("../clean_data/forced_choice_emotion_uw_students_long_format_lmer.csv")
df_mturk_en = read_csv("../clean_data_mturk/forced_choice_emotion_mturk_long_format_lmer.csv")
df_mturk_esp = read_csv("../clean_data_mturk_espanol/forced_choice_emotion_mturk_long_format_lmer_espanol.csv")  


#####################################
#####################################

# join dataframes for lmer

#####################################
#####################################

# df_students$condition <- 'english'
df_mturk_en$condition <- 'english'
df_mturk_esp$condition <- 'espanol'


table(df_mturk_en$participantId)
table(df_mturk_esp$participantId)


# add 100 to differentiate participants IDs
df_mturk_esp$participantId = df_mturk_esp$participantId+100
table(df_mturk_esp$participantId)


# language Condition centered
# df_students$conditionC <- -0.5
df_mturk_en$conditionC <- -0.5
df_mturk_esp$conditionC <- 0.5

# mturk samples comparison
df <- rbind(df_mturk_en, df_mturk_esp)

# drop "Other" to break tie with "Neutral" and keep ordinal variable
df <- subset(df, emotion!="Other")
df <- subset(df, emotion!="Uncertain")


table(df$emotion)
table(df$conditionC)

#####################################
#####################################

# prepare data for ordinal lmer

#####################################
#####################################

# filter(df, sentimentScore == "-0.4404")

df$emotionF <- factor(df$emotion,
                      order = TRUE, 
                      levels =c('Disgust', 'Anger', 'Fear', 'Sadness', 'Neutral', 'Surprise', 'Happiness'))


df$participantIdF <- as.factor(df$participantId)
df$ethnicityF <- as.factor(df$ethnicity)
df$sexF <- as.factor(df$sex)
df$conditionF <- as.factor(df$condition)


table(df$participantIdF)
table(df$ethnicityF)
table(df$sexF)
table(df$conditionF)

str(df)
df <- na.omit(df)
dim(df)


head(df)

#####################################
#####################################

# minimal plots 

#####################################
#####################################

library(svglite)
# 
# display.brewer.pal(n = 10, name = "RdBu")
# brewer.pal(n = 10, name = "RdBu")
#  "#67001F" "#B2182B" "#D6604D" "#F4A582" "#FDDBC7" "#D1E5F0" "#92C5DE" "#4393C3" "#2166AC" "#053061"

RdBu8Alter <- c("#B2182B", "#D6604D", "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")

## chart for 3-way interaction SEX x ETHNICITY X CONDITION  

three_Way <- df %>% mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% ggplot( aes(x = conditionF, fill = emotionF)) + 
                    geom_bar(position = "fill") +
                    facet_grid(sexF~ethnicityF) +
                    labs(y = "proportion", x = "survey language", fill ="emotion") + 
                    scale_fill_manual(values = RdBu8Alter) + 
                    theme_minimal()
three_Way

s <- svgstring(width = 7,
               height = 5)

three_Way

chart <- s()
cat(chart , file = "lmer_output/sex_et_cond_forced_mturk_espanol.txt")
cat(chart , file = "../../emotions_dashboard/data/sex_et_cond_forced_mturk_espanol.txt")
dev.off()

##############################
# get charts numbers as table

comp.prop <- function(df, ethnicity, sex, condition) {
  df[df$ethnicityF==ethnicity & df$sexF==sex & df$conditionF==condition,] %>%
    group_by(emotionF) %>%
    summarise(cnt = n()) %>%
    mutate(prop = round(cnt / sum(cnt), 2)) %>%
    arrange(desc(emotionF))
}

pane.1.0 <- comp.prop(df, ethnicity="bipoc", sex="female", condition="english")
pane.1.1 <- comp.prop(df, ethnicity="bipoc", sex="female", condition="espanol")
pane.2.0 <- comp.prop(df, ethnicity="white", sex="female", condition="english")
pane.2.1 <- comp.prop(df, ethnicity="white", sex="female", condition="espanol")
pane.3.0 <- comp.prop(df, ethnicity="bipoc", sex="male", condition="english")
pane.3.1 <- comp.prop(df, ethnicity="bipoc", sex="male", condition="espanol")
pane.4.0 <- comp.prop(df, ethnicity="white", sex="male", condition="english")
pane.4.1 <- comp.prop(df, ethnicity="white", sex="male", condition="espanol")

ID <- pane.1.0$emotionF
bipoc.english.female <- pane.1.0$prop
bipoc.espanol.female <- pane.1.1$prop
white.english.female <- pane.2.0$prop
white.espanol.female <- pane.2.1$prop
bipoc.english.male <- pane.3.0$prop
bipoc.espanol.male <- pane.3.1$prop
white.english.male <- pane.4.0$prop
white.espanol.male <- pane.4.1$prop

pane.table <- data.frame(ID = ID,
                         bipoc.english.female = bipoc.english.female, 
                         bipoc.espanol.female = bipoc.espanol.female,
                         white.english.female = white.english.female,
                         white.espanol.female = white.espanol.female,
                         bipoc.english.male = bipoc.english.male,
                         bipoc.espanol.male = bipoc.espanol.male,
                         white.english.male = white.english.male,
                         white.espanol.male = white.espanol.male)

pane.table <- pane.table %>%
  column_to_rownames('ID')


library(gridExtra)
library(grid)

tt1 <- ttheme_minimal() 

grid.tables <- grid.arrange(
  tableGrob(pane.table[1:2], theme=tt1),
  tableGrob(pane.table[3:4], theme=tt1),
  tableGrob(pane.table[5:6], theme=tt1),
  tableGrob(pane.table[7:8], theme=tt1),
  nrow=2)

library("ggplotify")

grid.tables.plot <- as.ggplot(grid.tables)

s <- svgstring(width = 9,
               height = 6)

grid.tables.plot

chart <- s()
cat(chart , file = "lmer_output/table_sex_et_cond_forced_mturk_espanol.txt")
cat(chart , file = "../../emotions_dashboard/data/table_sex_et_cond_forced_mturk_espanol.txt")
dev.off()


## chart for 2-way interaction SEX x CONDITION  

two_way_sex_language <- df %>% mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% ggplot( aes(x = sexF, fill = emotionF)) + 
                               geom_bar(position = "fill") +
                               facet_grid(. ~conditionF) + 
                               labs(y = "proportion", x = "sex", fill ="emotion") + 
                               scale_fill_manual(values = RdBu8Alter) + 
                               theme_minimal()


two_way_sex_language

s <- svgstring(width = 7,
               height = 5)

two_way_sex_language

chart <- s()
cat(chart , file = "lmer_output/sex_cond_forced_mturk_espanol.txt")
cat(chart , file = "../../emotions_dashboard/data/sex_cond_forced_mturk_espanol.txt")
dev.off()


## chart for 2-way interaction SEX x ETHNICITY  

two_way_sex_ethnicity <- df %>% mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% ggplot( aes(x = sexF, fill = emotionF)) + 
                            geom_bar(position = "fill") +
                            facet_grid(. ~ethnicityF) + 
                            labs(y = "proportion", x = "sex", fill ="emotion") + 
                            scale_fill_manual(values = RdBu8Alter) + 
                            theme_minimal()

two_way_sex_ethnicity

s <- svgstring(width = 7,
               height = 5)

two_way_sex_ethnicity

chart <- s()
cat(chart , file = "lmer_output/sex_ethnicity_forced_mturk_espanol.txt")
cat(chart , file = "../../emotions_dashboard/data/sex_ethnicity_forced_mturk_espanol.txt")
dev.off()


## chart for 2-way interaction  ETHNICITY X Condition

two_way_ethnicity_condition <- df %>% mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% ggplot( aes(x = ethnicityF, fill = emotionF)) + 
  geom_bar(position = "fill") +
  facet_grid(. ~conditionF) + 
  labs(y = "proportion", x = "ethnicity", fill ="emotion") + 
  scale_fill_manual(values = RdBu8Alter) + 
  theme_minimal()

two_way_ethnicity_condition

s <- svgstring(width = 7,
               height = 5)

two_way_ethnicity_condition

chart <- s()
cat(chart , file = "lmer_output/ethnicity_cond_forced_mturk_espanol.txt")
cat(chart , file = "../../emotions_dashboard/data/ethnicity_cond_forced_mturk_espanol.txt")
dev.off()


## chart for effect of condition on emotion

condition_chart <- df %>% mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% ggplot( aes(x = conditionF, fill = emotionF)) + 
                          geom_bar(position = "fill") +
                          labs(y = "proportion", x = "survey language", fill ="emotion") + 
                          scale_fill_manual(values = RdBu8Alter) + 
                          theme_minimal()

condition_chart

s <- svgstring(width = 7,
               height = 5)

condition_chart

chart <- s()
cat(chart , file = "lmer_output/cond_forced_mturk_espanol.txt")
cat(chart , file = "../../emotions_dashboard/data/cond_forced_mturk_espanol.txt")
dev.off()

## chart for effect of SEX on emotion

sex_chart <- df %>% mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% ggplot( aes(x = sexF, fill = emotionF)) + 
  geom_bar(position = "fill") +
  labs(y = "proportion", x = "sex", fill ="emotion") + 
  scale_fill_manual(values = RdBu8Alter) + 
  theme_minimal()

sex_chart

s <- svgstring(width = 7,
               height = 5)

sex_chart

chart <- s()
cat(chart , file = "lmer_output/sex_forced_mturk_espanol.txt")
cat(chart , file = "../../emotions_dashboard/data/sex_forced_mturk_espanol.txt")
dev.off()


## chart for effect of ETHNICITY on emotion

et_chart <- df %>% mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% ggplot( aes(x = ethnicityF, fill = emotionF)) + 
  geom_bar(position = "fill") +
  labs(y = "proportion", x = "ethnicity", fill ="emotion") + 
  scale_fill_manual(values = RdBu8Alter) + 
  theme_minimal()

et_chart

s <- svgstring(width = 7,
               height = 5)

et_chart

chart <- s()
cat(chart , file = "lmer_output/ethnicity_forced_mturk_espanol.txt")
cat(chart , file = "../../emotions_dashboard/data/ethnicity_forced_mturk_espanol.txt")
dev.off()


aggregate(x = df$sentimentScore,                # Specify data column
          by = list(df$sex),                    # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)

# 
# 1  female -0.1245115
# 2    male -0.1442889

aggregate(x = df$sentimentScore,                # Specify data column
          by = list(df$ethnicity),              # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)
# 
# 1   bipoc -0.1338941
# 2   white -0.1348134

#####################################
#####################################

# fit lmer with ordinal approach

#####################################
#####################################

# control= clm2.control(innerCtrl="noWarn")

ord_m1 <- clmm2(
  emotionF ~ sexC*ethnicityC*conditionC + (sexC*ethnicityC|participantId), 
  data=df, 
  Hess = TRUE)

# to get random effects correctly
ord_m2 <- clmm(
  emotionF ~ sexC*ethnicityC*conditionC + (sexC*ethnicityC|participantId), 
  data=df, 
  Hess = TRUE)

# for nominal test 
ord_m3 <- clm(
  emotionF ~ sexC*ethnicityC*conditionC + (sexC*ethnicityC|participantId), 
  data=df, 
  Hess = TRUE)

#######
# summary significant results
summary(ord_m1)
summary(ord_m2)
summary(ord_m3)


# ConditionC beta = 0.07927, p-value = 0.04
# indicates spanish-speaking participants rate pictures more positively; more likely to rate in more positive categories

# sexC:conditionC beta= 0.16502, p-value = 0.006
# indicates that male pictures are more likely to be rated in more positive categories

#######
# Get odds ratio for coefficients
exp(coef(ord_m2))


#######
## The odds ratio of *emotion* being rated in category j or above (OR(Y>j)) 

######
## sex ** NOT significant 
# logit = coef(ord_m1)[9]
# 
# exp(logit) # odds ratio 
# plogis(logit) # probability

# odds ratio of *emotion* being rated in category j or above at photo=male relative to photo=female is 0.9663773 
# probability of *emotion* being rated in category j or above at photo=male relative to photo=female is 0.493809

############
## ethnicity** NOT significant

# logit = coef(ord_m2)[11]
# 
# exp(logit) # odds ratio 
# plogis(logit) # probability

# odds ratio of *emotion* being rated in category j or above at photo=white relative to photo=poc is 1.1
# probability of *emotion* being rated in category j or above at ethnicity=white relative to ethnicity=POC is 0.524

###########
# condition

logit = coef(ord_m2)[9]

exp(logit) # odds ratio 
plogis(logit)

# odds ratio of *emotion* being rated in category j or above at condition=espanol relative to photo=english is 1.082502 
# probability of *emotion* being rated in category j or above at condition=espanol relative to photo=english is 0.5198083 


###########
# sex X condition: 
# whether the effect of "sex" for the Spanish-survey is reliably different from the same effect for the English-survey


logit = coef(ord_m2)[11]

exp(logit) # odds ratio 
plogis(logit)

# odds ratio of *emotion* being rated in category j or above at [] relative to [] is 1.17
# probability of *emotion* being rated in category j or above at [] relative to [] is 0.54


###############################
###############################
# checking assumptions
##############################
##############################

#######
# Multicolinearity
library(stats)

chisq.test(df$sexF, df$ethnicityF, correct=FALSE)
# X-squared = 0.005297, df = 1, p-value = 0.942
chisq.test(df$sexF, df$conditionF, correct=FALSE)
# X-squared = 0.062296, df = 1, p-value = 0.8029
chisq.test(df$ethnicityF, df$conditionF, correct=FALSE)
# X-squared = 0.036023, df = 1, p-value = 0.8495

##########
#  proportional odds assumption

nominal_test(ord_m3)


# Tests of nominal effects
# 
# formula: emotionF ~ sexC * ethnicityC * conditionC + (sexC * ethnicityC | participantId)
#                                    Df logLik   AIC     LRT  Pr(>Chi)    
# <none>                               -27137 54299                      
# sexC                               5 -27121 54278  30.923 9.700e-06 ***
# ethnicityC                         5 -27081 54199 110.402 < 2.2e-16 ***
# conditionC                         5 -27066 54168 140.691 < 2.2e-16 ***
# sexC:ethnicityC                    5 -27109 54254  55.525 1.018e-10 ***
# sexC:conditionC                    5 -27134 54304   5.239    0.3874    
# ethnicityC:conditionC              5 -27135 54305   4.097    0.5356    
# sexC:ethnicityC:conditionC         5 -27134 54304   5.396    0.3695    
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

# scale_test(ord_m2)

# assumptions are meet for the most part: all main effects + sex*ethnicity interaction


# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Export to  dashboard

# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#####################################
#####################################

# get mathematical formula 


library(equatiomatic)

formula_lmer <- extract_eq(ord_m3)


cat(formula_lmer, file = "lmer_output/formula_ord_lmer_summary_forced_mturk_espanol.txt")
cat(formula_lmer, file = "../../emotions_dashboard/data/formula_ord_lmer_summary_forced_mturk_espanol.txt")



#####################################
#####################################

# get model coefficients 

library(sjPlot)


tab_model(ord_m2, file = "lmer_output/ord_lmer_summary_forced_mturk_espanol.html")
tab_model(ord_m2, file = "../../emotions_dashboard/data/ord_lmer_summary_forced_mturk_espanol.html")


####################################
####################################

# get chi-square tables


## sex x ethnicity


sjt.xtab(df$sexF, 
         df$ethnicityF, 
         var.labels = c("Sex", "Ethnicity"), 
         show.exp = TRUE,
         emph.total = TRUE, 
         file = "lmer_output/chi_sex_et_forced_mturk_espanol.html")


sjt.xtab(df$sexF, 
         df$ethnicityF, 
         var.labels = c("Sex", "Ethnicity"), 
         show.exp = TRUE,
         emph.total = TRUE,
         file = "../../emotions_dashboard/data/chi_sex_et_forced_mturk_espanol.html")


## sex x condition


sjt.xtab(df$sexF, 
         df$conditionF, 
         var.labels = c("Sex", "Survey language"), 
         show.exp = TRUE,
         emph.total = TRUE,
         file = "lmer_output/chi_sex_cond_forced_mturk_espanol.html")

sjt.xtab(df$sexF, 
         df$conditionF, 
         var.labels = c("Sex", "Survey language"), 
         show.exp = TRUE,
         emph.total = TRUE,
         file = "../../emotions_dashboard/data/chi_sex_cond_forced_mturk_espanol.html")


## ethnicity x condition

sjt.xtab(df$ethnicityF, 
         df$conditionF, 
         var.labels = c("Ethnicity", "Survey language"), 
         show.exp = TRUE,
         emph.total = TRUE,
         file = "lmer_output/chi_et_cond_forced_mturk_espanol.html")

sjt.xtab(df$ethnicityF, 
         df$conditionF, 
         var.labels = c("Ethnicity", "Survey language"), 
         show.exp = TRUE,
         emph.total = TRUE,
         file = "../../emotions_dashboard/data/chi_et_cond_forced_mturk_espanol.html")



####################################
####################################

# get nominal test table

library(broom)
library(htmlTable)

nominal.test.table <- nominal_test(ord_m3) %>% 
  tidy() %>% 
  drop_na() %>% 
  addHtmlTableStyle(align = "r") %>% 
  txtRound(digits = 3) %>% 
  htmlTable()

nominal.test.table

cat(nominal.test.table, file = "lmer_output/nominal_test_forced_mturk_espanol.html")
cat(nominal.test.table, file = "../../emotions_dashboard/data/nominal_test_forced_mturk_espanol.html")
