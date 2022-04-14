library(tidyverse)
library(ordinal)
library(RColorBrewer)
library(ggforce)
library(papaja)


df_mturk = read_csv("../clean_data_mturk/forced_choice_emotion_mturk_long_format_lmer.csv")

table(df_mturk$emotion)

# drop "Other" to break tie with "Neutral" and keep ordinal variable
df <- subset(df_mturk, emotion!="Other")
# drop because = to "I don't know what the person in the picture is feeling" Not "The person is feeling uncertainty"
df <- subset(df, emotion!="Uncertain")

table(df$emotion)

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


table(df$participantIdF)
table(df$ethnicityF)
table(df$sexF)

str(df)
df <- na.omit(df)
dim(df)

#####################################
#####################################

# minimal plots 

#####################################
#####################################
library(svglite)
 
# display.brewer.pal(n = 10, name = "RdBu")
# brewer.pal(n = 10, name = "RdBu")
#  "#67001F" "#B2182B" "#D6604D" "#F4A582" "#FDDBC7" "#D1E5F0" "#92C5DE" "#4393C3" "#2166AC" "#053061"

RdBu8Alter <- c("#B2182B", "#D6604D", "#F7F7F7", "#D1E5F0", "#92C5DE", "#4393C3", "#2166AC", "#053061")


## chart for 2-way interaction SEX x ETHNICITY  

two_way_sex_ethnicity <- df %>% 
  mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% 
  ggplot( aes(x = sexF, fill = emotionF)) + 
  geom_bar(position = "fill") +
  facet_grid(. ~ethnicityF) + 
  labs(y = "proportion", x = "sex", fill ="emotion") + 
  scale_fill_manual(values = RdBu8Alter) + 
  theme_apa()

two_way_sex_ethnicity


s <- svgstring(width = 7,
               height = 5)

two_way_sex_ethnicity

chart <- s()
cat(chart , file = "lmer_output/sex_ethnicity_forced_mturk.txt")
cat(chart , file = "../../emotions_dashboard/data/sex_ethnicity_forced_mturk.txt")
dev.off()

## chart for effect of sex

sex_chart <- df %>% 
  mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% 
  ggplot( aes(x = sexF, fill = emotionF)) + 
  geom_bar(position = "fill") +
  labs(y = "proportion", x = "sex", fill ="emotion") + 
  scale_fill_manual(values = RdBu8Alter) + 
  theme_apa()

sex_chart


s <- svgstring(width = 7,
               height = 5)

sex_chart

chart <- s()
cat(chart , file = "lmer_output/sex_forced_mturk.txt")
cat(chart , file = "../../emotions_dashboard/data/sex_forced_mturk.txt")
dev.off()

## chart for effect of ethnicity

et_chart <- df %>% 
  mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% 
  ggplot( aes(x = ethnicityF, fill = emotionF)) + 
  geom_bar(position = "fill") +
  labs(y = "proportion", x = "ethnicity", fill ="emotion") + 
  scale_fill_manual(values = RdBu8Alter) + 
  theme_apa()


et_chart

s <- svgstring(width = 7,
               height = 5)

et_chart

chart <- s()
cat(chart , file = "lmer_output/ethnicity_forced_mturk.txt")
cat(chart , file = "../../emotions_dashboard/data/ethnicity_forced_mturk.txt")
dev.off()


aggregate(x = df$sentimentScore,                # Specify data column
          by = list(df$sex),                    # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)

# 1  female -0.1242946
# 2    male -0.1574881

aggregate(x = df$sentimentScore,                # Specify data column
          by = list(df$ethnicity),              # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)

# 1   bipoc -0.1374231
# 2   white -0.1441567

#####################################
#####################################

# fit lmer with ordinal approach

#####################################
#####################################

ord_m1 <- clmm2(
  emotionF ~ 1 + sexC*ethnicityC + (1 + sexC*ethnicityC|participantId), 
  data=df, 
  Hess = TRUE)

# to get random effects correctly
ord_m2 <- clmm(
  emotionF ~ 1 + sexC*ethnicityC + (1 + sexC*ethnicityC|participantId), 
  data=df, 
  Hess = TRUE)

# for nominal test 
ord_m3 <- clm(
  emotionF ~ sexC*ethnicityC + (sexC*ethnicityC|participantId), 
  data=df, 
  Hess = TRUE)

#######
# summary significant results
summary(ord_m1)
summary(ord_m2)
summary(ord_m3)

# sex beta= -0.12, p-value = 0.001
# indicates that [sex=male] pictures are less likely to be rated in more positive categories

## NOT significant
# ethnicity beta = -0.05571, p-value = 0.12

## NOT significant
# sexC:ethnicityC beta = 0.02341, p-value = 0.74538

#######
# Get odds ratio for coefficients
exp(coef(ord_m1))


#######
## The odds ratio of *emotion* being rated in category j or above (OR(Y>j)) 

######
## sex
logit = coef(ord_m2)[7]

exp(logit) # odds ratio 
plogis(logit) # probbility

# odds ratio of *emotion* being rated in category j or above at photo=male relative to photo=female is 0.89 
# probability of *emotion* being rated in category j or above at photo=male relative to photo=female is 0.47

############
## ethnicity
# 
# logit = coef(ord_m1)[9]
# 
# exp(logit) # odds ratio 
# plogis(logit) # probability


###########
# sex X condition: 
# whether the effect of "sex" for the [photo=white] is reliably different from the same effect for [photo=poc]


# logit = coef(ord_m1)[10]

# exp(logit) # odds ratio 
# plogis(logit) # probability


###############################
###############################
# checking assumptions
##############################
##############################

#######
# Multicolinearity
library(stats)

chisq.test(df$sexF, df$ethnicityF, correct=FALSE)
# X-squared = 0.0052644, df = 1, p-value = 0.9422

##########
#  proportional odds assumption

nominal_test(ord_m3)

# Tests of nominal effects
# 
# formula: emotionF ~ sexC * ethnicityC + (sexC * ethnicityC | participantId)
#                                    Df logLik   AIC    LRT  Pr(>Chi)    
# <none>                               -17010 34038                     
# sexC                               5 -17000 34029 18.795  0.002099 ** 
# ethnicityC                         5 -16978 33984 63.391 2.416e-12 ***
# sexC:ethnicityC                    5 -16993 34014 33.146 3.521e-06 ***
#   ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


# assumptions are meet for sure



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


cat(formula_lmer, file = "lmer_output/formula_ord_lmer_summary_forced_mturk.txt")
cat(formula_lmer, file = "../../emotions_dashboard/data/formula_ord_lmer_summary_forced_mturk.txt")


#####################################
#####################################

# get model coefficients 

library(sjPlot)


tab_model(ord_m2, file = "lmer_output/ord_lmer_summary_forced_mturk.html")
tab_model(ord_m2, file = "../../emotions_dashboard/data/ord_lmer_summary_forced_mturk.html")


####################################
####################################

# get chi-square tables


## sex x ethnicity


sjt.xtab(df$sexF, 
         df$ethnicityF, 
         var.labels = c("Sex", "Ethnicity"), 
         show.exp = TRUE,
         emph.total = TRUE, 
         file = "lmer_output/chi_sex_et_forced_mturk.html")


sjt.xtab(df$sexF, 
         df$ethnicityF, 
         var.labels = c("Sex", "Ethnicity"), 
         show.exp = TRUE,
         emph.total = TRUE,
         file = "../../emotions_dashboard/data/chi_sex_et_forced_mturk.html")


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

cat(nominal.test.table, file = "lmer_output/nominal_test_forced_mturk.html")
cat(nominal.test.table, file = "../../emotions_dashboard/data/nominal_test_forced_mturk.html")


