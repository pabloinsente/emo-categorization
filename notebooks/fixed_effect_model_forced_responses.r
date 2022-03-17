library(tidyverse)
library(ordinal)
library(jtools) # for summ()
library(interactions)



df = read_csv("../clean_data/forced_choice_emotion_uw_students_long_format_lmer.csv")



#####################################
#####################################

# Fit LMER 

#####################################
#####################################

# # with derivatives check
# control.check=lmerControl(optimizer ="Nelder_Mead", calc.derivs=FALSE, optCtrl=list(maxfun=2e6), check.nobs.vs.nRE = "ignore")

# m1<-lmer(
#     sentimentScore ~ 1 + sexC*ethnicityC + (1 + sexC*ethnicityC|participantId) + (1 | photoId), 
#     data = df,
#     control=control.check)
# summary(m1)

# # without derivatives check
# control=lmerControl(optimizer ="Nelder_Mead", calc.derivs=FALSE, optCtrl=list(maxfun=2e6), check.nobs.vs.nRE = "ignore")

# m1<-lmer(
#     sentimentScore ~ 1 + sexC*ethnicityC + (1 + sexC*ethnicityC|participantId) + (1 | photoId), 
#     data = df,
#     control=control)
# summary(m1)



# MAXIMAL MODEL doesn't converge at all / tried multiple optimizers 


######################

## Simplified random effects structure

######################


#  with derivatives check
# control.check=lmerControl(optimizer ="Nelder_Mead", optCtrl=list(maxfun=2e6), check.nobs.vs.nRE = "ignore")


# control=lmerControl(optimizer ="Nelder_Mead", calc.derivs=FALSE,optCtrl=list(maxfun=2e6),check.nobs.vs.nRE = "ignore")
# m2<-lmer(
#     sentimentScore ~ 1 + sexC*ethnicityC + (1 + sexC*ethnicityC|participantId), 
#     data = df,
#     control=control.check)
# summary(m2)

## doesn't converge either

## without derivatives check
control=lmerControl(optimizer ="Nelder_Mead", calc.derivs=FALSE,optCtrl=list(maxfun=2e6), check.nobs.vs.nRE = "ignore")
m2<-lmer(
  sentimentScore ~ 1 + sexC*ethnicityC + (1 + sexC*ethnicityC|participantId), 
  data = df,
  control=control)

summary(m2)


#####################################
#####################################

# Fit fixed-effect only model 

#####################################
#####################################

# df$sexD <-ifelse(df$sex=="male",1,0)
# df$ethnicityD <-ifelse(df$ethnicity=="white",1,0)


fixed.effmodel <- lm(sentimentScore ~  sexC*ethnicityC, data = df)
summary(fixed.effmodel)
summ(fixed.effmodel)


fixed.effmodel.fact <- lm(sentimentScore ~  sex*ethnicity, data = df)
summary(fixed.effmodel.fact)
summ(fixed.effmodel.fact)


cat_plot(fixed.effmodel.fact, pred = sex, modx = ethnicity)


#####################################
#####################################

# Compare models

#####################################
#####################################

anova(m2, fixed.effmodel)


#####################################
#####################################

# fit lmer with ordinal approach

#####################################
#####################################

# filter(df, sentimentScore == "-0.4404")

df$emotionF <- factor(df$emotion,
                      order = TRUE, 
                      levels =c('Disgust', 'Anger', 'Fear', 'Sadness', 'Uncertain', 'Neutral', 'Other', 'Surprise', 'Happiness'))


ord.model <- clm(emotionF~sexC * ethnicityC,data = df, link = "logit")
summary(ord.model)


#####################################
#####################################

# get mathematical formula 

#####################################
#####################################


formula_lmer <- extract_eq(fixed.effmodel)
# 
# cat(formula_lmer, file = "lmer_output/formula_lmer_summary_forced_uw_students.txt")
# cat(formula_lmer, file = "../../emotions_dashboard/data/formula_lmer_summary_forced_uw_students.txt")


#####################################
#####################################

# get coefficient table for reporting 

#####################################
#####################################

# 
# tab_model(fixed.effmodel, file = "lmer_output/lmer_summary_forced_uw_students.html")
# tab_model(fixed.effmodel, file = "../../emotions_dashboard/data/lmer_summary_forced_uw_students.html")



#####################################
#####################################

# Type III anova table with p-values for F-tests based on Satterthwaite's method

#####################################
#####################################

# 
# (aov <- anova(fixed.effmodel))
# 
# aov.apa <- kable(aov, digits = 3, format = "html", caption = "ANOVA table for LMER coefficients")
# cat(aov.apa, file = "lmer_output/anova_lmer_summary_forced_uw_students.html")
# cat(aov.apa, file = "../../emotions_dashboard/data/anova_lmer_summary_forced_uw_students.html")

#####################################
#####################################

# Testing assumptions

#####################################
#####################################

par(mfrow = c(1))

plot(fixed.effmodel.fact)


################
# normality of residuals

## hist of residuals
hist(fixed.effmodel$residuals)

## qqplot
plot(fixed.effmodel.fact, 2)

# get distribution of studentized residuals (i.e. transform residuals for test)
sresid <- MASS::studres(fixed.effmodel) #using MASS package function to transform data easily
shapiro.test(sample(sresid,5000)) # p value non-sign: normal distribution of residuals

format(2.2e-16, scientific = F, digits = 3)
