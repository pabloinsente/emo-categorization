library(tidyverse)
library(ordinal)
library("RColorBrewer")

df_students = read_csv("../clean_data/forced_choice_emotion_uw_students_long_format_lmer.csv")

table(df_students$emotion)

# drop "Other" to break tie with "Neutral" and keep ordinal variable
df <- subset(df_students, emotion!="Other")
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
  theme_minimal()

two_way_sex_ethnicity


s <- svgstring(width = 7,
               height = 5)

two_way_sex_ethnicity

chart <- s()
cat(chart , file = "lmer_output/sex_ethnicity_forced.txt")
cat(chart , file = "../../emotions_dashboard/data/sex_ethnicity_forced.txt")
dev.off()


## chart for effect of sex

sex_chart <- df %>% 
  mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% 
  ggplot( aes(x = sexF, fill = emotionF)) + 
  geom_bar(position = "fill") +
  labs(y = "proportion", x = "sex", fill ="emotion") + 
  scale_fill_manual(values = RdBu8Alter) + 
  theme_minimal()

sex_chart


s <- svgstring(width = 7,
               height = 5)

sex_chart

chart <- s()
cat(chart , file = "lmer_output/sex_forced.txt")
cat(chart , file = "../../emotions_dashboard/data/sex_forced.txt")
dev.off()

## chart for effect of ethnicity

et_chart <- df %>% 
  mutate(emotionF = ordered(emotionF, levels=rev(levels(emotionF)))) %>% 
  ggplot( aes(x = ethnicityF, fill = emotionF)) + 
  geom_bar(position = "fill") +
  labs(y = "proportion", x = "ethnicity", fill ="emotion") + 
  scale_fill_manual(values = RdBu8Alter) + 
  theme_minimal()


et_chart

s <- svgstring(width = 7,
               height = 5)

et_chart

chart <- s()
cat(chart , file = "lmer_output/ethnicity_forced.txt")
cat(chart , file = "../../emotions_dashboard/data/ethnicity_forced.txt")
dev.off()



aggregate(x = df$sentimentScore,                # Specify data column
          by = list(df$sex),                    # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)

# 1  female -0.1155575
# 2    male -0.1339802

aggregate(x = df$sentimentScore,                # Specify data column
          by = list(df$ethnicity),              # Specify group indicator
          FUN = mean)                           # Specify function (i.e. mean)

# 1   bipoc -0.1407513
# 2   white -0.1088178

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


# sex beta= -0.08, p-value = 0.03
# indicates that [sex=male] pictures are less likely to be rated in more positive categories

# ethnicity beta = 0.10, p-value = 0.007
# indicates that [ethnicity=white] pictures are more likely to be rated in more positive categories

# sexC:ethnicityC beta = 0.14, p-value = 0.057
# indicates the effect of ethnicity differs within each sex category (or visceversa)

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

# odds ratio of *emotion* being rated in category j or above at photo=male relative to photo=female is 0.92
# probability of *emotion* being rated in category j or above at photo=male relative to photo=female is 0.47 

############
## ethnicity

logit = coef(ord_m2)[8]

exp(logit) # odds ratio 
plogis(logit) # probability

# odds ratio of *emotion* being rated in category j or above at ethnicity=white relative to ethnicity=poc is 1.1
# probability of *emotion* being rated in category j or above at ethnicity=white relative to ethnicity=poc is 0.53 


###########
# sex X condition: 
# whether the effect of "sex" for the [photo=white] is reliably different from the same effect for [photo=poc]


logit = coef(ord_m2)[9]

exp(logit) # odds ratio 
plogis(logit) # probability

# odds ratio of *emotion* being rated in category j or above at [] relative to [] is 1.15
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
# X-squared = 0.073856, df = 1, p-value = 0.7858

##########
#  proportional odds assumption

nominal_test(ord_m3)

 
# Tests of nominal effects
# 
# formula: emotionF ~ sexC * ethnicityC + (sexC * ethnicityC | participantId)
#                                    Df logLik   AIC    LRT  Pr(>Chi)    
# <none>                               -16127 32273                     
# sexC                               5 -16124 32275  7.625    0.1781    
# ethnicityC                         5 -16078 32184 98.968 < 2.2e-16 ***
# sexC:ethnicityC                    5 -16096 32219 63.560 2.228e-12 ***
#   ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

# scale_test(ord_m2)

# assumptions are meet for the most part: only the main effect of sex fails to pass



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


cat(formula_lmer, file = "lmer_output/formula_ord_lmer_summary_forced.txt")
cat(formula_lmer, file = "../../emotions_dashboard/data/formula_ord_lmer_summary_forced.txt")


#####################################
#####################################

# get model coefficients 

library(sjPlot)


tab_model(ord_m2, file = "lmer_output/ord_lmer_summary_forced.html")
tab_model(ord_m2, file = "../../emotions_dashboard/data/ord_lmer_summary_forced.html")


####################################
####################################

# get chi-square tables


## sex x ethnicity


sjt.xtab(df$sexF, 
         df$ethnicityF, 
         var.labels = c("Sex", "Ethnicity"), 
         show.exp = TRUE,
         emph.total = TRUE, 
         file = "lmer_output/chi_sex_et_forced.html")


sjt.xtab(df$sexF, 
         df$ethnicityF, 
         var.labels = c("Sex", "Ethnicity"), 
         show.exp = TRUE,
         emph.total = TRUE,
         file = "../../emotions_dashboard/data/chi_sex_et_forced.html")


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

cat(nominal.test.table, file = "lmer_output/nominal_test_forced.html")
cat(nominal.test.table, file = "../../emotions_dashboard/data/nominal_test_forced.html")
