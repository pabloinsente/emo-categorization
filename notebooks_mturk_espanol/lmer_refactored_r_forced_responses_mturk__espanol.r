#install.packages(c("lme4", "car", "lmerTest", "lmerTest", 
#                   "tidyverse", "ragg", "HLMdiag", "VCA", 
#                   "hrbrthemes", "ggResidpanel", "sjPlot",
#                   "kableExtra", "knitr", "remotes", 
#                   "equatiomatic", 'textpreview', "heavy",
#                   "robustlmm", "ordinal"))


library(lme4)
library(car)
library(lmerTest)
library(tidyverse)
library(ragg)
library(HLMdiag)
library(VCA)
library(hrbrthemes)
library(ggResidpanel)
library(sjPlot)
library(webshot)
library(equatiomatic)
library(svglite)
library(knitr)
library(ordinal)

df_students = read_csv("../clean_data/forced_choice_emotion_uw_students_long_format_lmer.csv")
df_mturk_en = read_csv("../clean_data_mturk/forced_choice_emotion_mturk_long_format_lmer.csv")
df_mturk_esp = read_csv("../clean_data_mturk_espanol/forced_choice_emotion_mturk_long_format_lmer_espanol.csv")  


########################
########################

# descriptives

########################
########################


head(df_mturk_esp)

df_mturk_esp %>%  ggplot(aes(x=sentimentScore)) +
    geom_histogram(fill="#1f77b4") +
    theme_ipsum()

by.sex <- df_mturk_esp %>%
  ggplot( aes(x=sentimentScore, fill=sex)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#1f77b4", "#ff7f0e")) +
    theme_ipsum() +
    stat_bin(bins=20) +
    labs(fill="")
by.sex

by.age <- df_mturk_esp %>%
  ggplot( aes(x=sentimentScore, fill=age)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    theme_ipsum() +
    stat_bin(bins=20) +
    labs(fill="")
by.age

by.ethnicity <- df_mturk_esp %>%
  ggplot( aes(x=sentimentScore, fill=ethnicity)) +
    geom_histogram( color="#5e6162", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    theme_ipsum() +
    stat_bin(bins=20) +
    labs(fill="")
by.ethnicity

df_mturk_esp %>%
  group_by(participantId) %>%
  summarise_at(vars(sentimentScore), list(name = mean)) %>% 
  ggplot(aes(x=name)) +
    geom_histogram(color="#5e6162", fill="#66a3a3") +
    theme_ipsum()

df_mturk_esp %>%
  group_by(photoId) %>%
  summarise_at(vars(sentimentScore), list(name = mean)) %>% 
  ggplot(aes(x=name)) +
    geom_histogram(color="#5e6162", fill="#66a3a3") +
    theme_ipsum()

#####################################
#####################################

# join dataframes for lmer

#####################################
#####################################

df_students$condition <- 'english'
df_mturk_en$condition <- 'english'
df_mturk_esp$condition <- 'espanol'

# Condition centered
df_students$conditionC <- -0.5
df_mturk_en$conditionC <- -0.5
df_mturk_esp$conditionC <- 0.5

df = rbind(df_mturk_en, df_mturk_esp)

table(df$emotion)
table(df$conditionC)


#####################################
#####################################

# Fit LMER 

#####################################
#####################################

# WHIT derivatives check
# control.check=lmerControl(optimizer ="Nelder_Mead", calc.derivs=FALSE, optCtrl=list(maxfun=2e6), check.nobs.vs.nRE = "ignore")
# 
# m1<-lmer(
#    sentimentScore ~ 1 + sexC*ethnicityC + (1 + sexC*ethnicityC|participantId) + (1 | photoId),
#    data = df,
#    control=control.check)
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

################ doesn't converge either


#################################
### sex * ethnicity * condition

# without derivatives check

control=lmerControl(optimizer ="Nelder_Mead", calc.derivs=FALSE,optCtrl=list(maxfun=2e6), check.nobs.vs.nRE = "ignore")
m2<-lmer(
    sentimentScore ~ 1 + sexC*ethnicityC*conditionC + (1 + sexC*ethnicityC*conditionC|participantId),
    data = df,
    control=control)

summary(m2)

#####################################
#####################################

# model comparison

#####################################
#####################################

control=lmerControl(optimizer ="Nelder_Mead", calc.derivs=FALSE,optCtrl=list(maxfun=2e6), check.nobs.vs.nRE = "ignore")
m2.sex <-lmer(
  sentimentScore ~ 1 + sexC +  (1+ sexC*ethnicityC|participantId), 
  data = df,
  control=control)

summary(m2.sex)
anova(m2, m2.sex)

# m2 is slightly better on AIC/BIC

m2.ethnicityC <-lmer(
  sentimentScore ~ 1 + ethnicityC + (1+ sexC*ethnicityC |participantId), 
  data = df,
  control=control)

summary(m2.ethnicityC)
anova(m2, m2.ethnicityC)

# m2 is better 

m2.add <-lmer(
  sentimentScore ~ 1 + sexC+ethnicityC + (1+sexC*ethnicityC |participantId), 
  data = df,
  control=control)

summary(m2.add)
anova(m2, m2.add)

# m2 is slightly better on AIC/BIC

full.interactive.model <- m2
sex.only.model <- m2.sex
ethnicity.only.model <- m2.ethnicityC
sex.ethnicity.additive.model <- m2.add

# full.interactive.model is better

anova(sex.only.model, ethnicity.only.model, sex.ethnicity.additive.model, full.interactive.model)

# Additive model is better

(aov.comparison <- anova(sex.only.model, ethnicity.only.model, sex.ethnicity.additive.model, full.interactive.model))

aov.apa.com <- kable(aov.comparison, digits = 3, format = "html", caption = "ANOVA table for model comparison")
cat(aov.apa.com, file = "lmer_output/anova_comparison_lmer_summary_forced_mturk.html")
cat(aov.apa.com, file = "../../emotions_dashboard/data/anova_comparison_lmer_summary_forced_mturk.html")


#####################################
#####################################

# get mathematical formula 

#####################################
#####################################


formula_lmer <- extract_eq(m2)

cat(formula_lmer, file = "lmer_output/formula_lmer_summary_forced_mturk.txt")
cat(formula_lmer, file = "../../emotions_dashboard/data/formula_lmer_summary_forced_mturk.txt")


#####################################
#####################################

# get coefficient table for reporting 

#####################################
#####################################


tab_model(m2, file = "lmer_output/lmer_summary_forced_mturk.html")
tab_model(m2, file = "../../emotions_dashboard/data/lmer_summary_forced_mturk.html")


#####################################
#####################################

# Type III anova table with p-values for F-tests based on Satterthwaite's method

#####################################
#####################################


(aov <- anova(m2))

aov.apa <- kable(aov, digits = 3, format = "html", caption = "ANOVA table for LMER coefficients")
cat(aov.apa, file = "lmer_output/anova_lmer_summary_forced_mturk.html")
cat(aov.apa, file = "../../emotions_dashboard/data/anova_lmer_summary_forced_mturk.html")


#####################################
#####################################

# PENDING: UPDATE MODEL INTERPRETATION 

#####################################
#####################################


# sex-photo effect:
# The effect of sex-photo on sentiment-score was not significant, b = -0.01602, F(1,51) = 3.7, p = 0.054
# Averaging across POC and Caucasian photo-faces, and controlling for ethnicity and the ethnicity-photo by sex-photo interaction,
# sentiment-scores were -0.016 more negative in response to male faces than female faces. 


# ethnicity-photo effect:
# The effect of ethnicity-photo on sentiment-scores was significant, b =  0.028, F(1,51) = 11.9, p < .001.
# For sake of completeness:
# Averaging across female and male photos, and controlling for sex-photo and the sex-photo by ethnicity-photo interaction,
# sentiment-scores were 0.028 more positive in response to caucasian-photos than poc-photos

# Face by odor interaction:
# The face by odor interaction was significant, b = 0.04, F(1,51) = 6.03, p = 0.014.
# For sake of completeness:
# Controlling for lower-order effects, the difference in sentiment-scorees between the two sex-photo conditions 
# was 0.04 points greater in the caucasian-photos than in the poc-photos


#####################################
#####################################

# Individual participant data for sex * ethnicity conditions

#####################################
#####################################

# saving as svg string

s <- svgstring()

p = ggplot(df,aes(sex,sentimentScore,color=ethnicity,group=ethnicity))+
    geom_point()+
    geom_smooth(method="lm",se=F)+
    facet_wrap(~participantId)+
    theme_bw()+
    scale_color_manual(values=c("#1f77b4", "#ff7f0e"))

p
svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/participants_charts_lmer_forced_mturk.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/participants_charts_lmer_forced_mturk.txt")

dev.off()


#####################################
#####################################

# Check homogeneity of variance

#####################################
#####################################

# https://ademos.people.uic.edu/Chapter18.html
# ANOVA of the between subjects residuals.
# the assumption is that the variance is not going to differ, we would hope to see 
# NO STATISTICAL DIFFERENCES in the following procedure (i.e. p>0.05)

df$Model.F.Res<- residuals(m2) #extracts the residuals and places them in a new column in our original data table
df$Abs.Model.F.Res <-abs(df$Model.F.Res) #creates a new column with the absolute value of the residuals
df$Model.F.Res2 <- df$Abs.Model.F.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.Model.F <- lm(Model.F.Res2 ~ participantId, data=df) #ANOVA of the squared residuals
anova(Levene.Model.F) #displays the results
format(4.440288e-07, scientific = F)

# save to html table
aov.btw.res <- kable(anova(Levene.Model.F), digits = 3, format = "html", caption = "ANOVA table for between subjects residuals")

cat(aov.btw.res, file = "lmer_output/anova_bwt_res_summary_forced_mturk.html")
cat(aov.btw.res, file = "../../emotions_dashboard/data/anova_bwt_res_summary_forced_mturk.html")


# Since the p value < 0.05, we can say that the variance of the residuals is equal and 
# therefore the assumption of **homoscedasticity** NOT is met 

s <- svgstring(width = 7,
               height = 5)

Plot.Model.F <- plot(m2) #creates a fitted vs residual plot
Plot.Model.F
Plot.Model.F <- s()
cat(Plot.Model.F , file = "lmer_output/fitted_vs_residual_plot_forced_mturk.txt")
cat(Plot.Model.F , file = "../../emotions_dashboard/data/fitted_vs_residual_plot_forced_mturk.txt")
dev.off()

## This looks very unsystematic

resid1 <- hlm_resid(m2, level = 1, standardize = TRUE)

s <- svgstring(width = 7,
               height = 5)

ggplot(data = resid1, aes(x = participantId, y = .std.ls.resid)) + 
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(y = "Least-Squares level-1 residuals", 
       title = "Least-Squares residuals by participant ID")

l1.res <- s()
cat(l1.res , file = "lmer_output/l1_res_plot_forced_mturk.txt")
cat(l1.res , file = "../../emotions_dashboard/data/l1_res_plot_forced_mturk.txt")
dev.off()

# There are a couple of large residuals

resid2 = hlm_resid(m2, level = "participantId", standardize = TRUE, include.ls = FALSE)

s <- svgstring(width = 7,
               height = 5)

ggplot(data = resid2, aes(x = participantId, y = .std.ranef.intercept)) + 
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(y = "Random effects - intercept", 
       title = "Intercept random effects against participant ID")

l2.res.int <- s()
cat(l2.res.int , file = "lmer_output/l2_int_res_plot_forced_mturk.txt")
cat(l2.res.int , file = "../../emotions_dashboard/data/l2_int_res_plot_forced_mturk.txt")
dev.off()

# There are a couple of large residuals

#####################################
#####################################

# Check normality of error term

#####################################
#####################################

require("lattice")
s <- svgstring(width = 7,
               height = 5)
qqmath(m2, id=0.05) #id: identifies values that may be exerting undue influence on the model (i.e. outliers)
svg.qqplot <- s()
cat(svg.qqplot, file = "lmer_output/qqplot_lmer_forced_mturk.txt")
cat(svg.qqplot, file = "../../emotions_dashboard/data/qqplot_lmer_forced_mturk.txt")
dev.off()


# looks not normal...
# https://ademos.people.uic.edu/Chapter18.html

# resid_panel(m2)


#####################################
#####################################

# Check influence

#####################################
#####################################


#############################

# check datapoints influence

#############################


infl <- hlm_influence(m2, level = 1)

# IQR = as.numeric(format(IQR(infl$cooksd)*3, scientific = F))
CutOff = 4/nrow(infl)
print(CutOff)

s <- svgstring(width = 7,
               height = 5)
# dotplot_diag(infl$cooksd, name = "cooks.distance", cutoff = "internal")
dotplot_diag(infl$cooksd, name = "cooks.distance", cutoff = CutOff)
svg.string.plot <- s()
cat(svg.string.plot, file = "lmer_output/influence_datapoints_lmer_forced_mturk.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/influence_datapoints_lmer_forced_mturk.txt")
dev.off()


high_cooksd = infl[infl$cooksd > CutOff, ] %>%
  arrange(desc(cooksd))

head(high_cooksd, n=10)

high_cooksd$id

# none

#############################

# check participants influence

#############################

infl.classes <- hlm_influence(m2, level = "participantId")

CutOffGroup = 4/49
CutOffGroup

s <- svgstring(width = 7,
               height = 5)

# dotplot_diag(infl.classes$cooksd, name = "cooks.distance", cutoff = "internal", modify = "dotplot")
dotplot_diag(infl.classes$cooksd, name = "cooks.distance", cutoff = CutOffGroup, modify = "dotplot")
svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/influence_participants_lmer_forced_mturk.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/influence_participants_lmer_forced_mturk.txt")
dev.off()


high_cooksd_participants = infl.classes[infl.classes$cooksd > CutOffGroup, ] %>%
  arrange(desc(cooksd))

high_cooksd_participants

# none

#####################################
#####################################

# Check leverage

#####################################
#####################################

#############################

# check datapoints leverage

#############################


CutOffLeverage = mean(infl$leverage.overall)*3
CutOffLeverage

s <- svgstring(width = 7,
               height = 5)

# dotplot_diag(infl$leverage.overall, name = "leverage", cutoff = "internal")
dotplot_diag(infl$leverage.overall, name = "leverage", cutoff = CutOffLeverage)

svg.string.plot <- s()
cat(svg.string.plot, file = "lmer_output/leverage_datapoints_lmer_forced_mturk.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/leverage_datapoints_lmer_forced_mturk.txt")
dev.off()

high_leverage = infl[infl$leverage.overall > CutOffLeverage, ] %>%
  arrange(desc(leverage.overall))

# head(high_leverage, n=10)
high_leverage

# high leverage data points
high_leverage$id

# no high leverage datapoints

#############################

# check participants leverage

#############################

CutOffLeverageParticipants = mean(infl.classes$leverage.overall)*3
CutOffLeverageParticipants


s <- svgstring(width = 7,
               height = 5)

# dotplot_diag(infl.classes$leverage.overall, name = "leverage", cutoff = "internal")
dotplot_diag(infl.classes$leverage.overall, name = "leverage", cutoff = CutOffLeverageParticipants)

svg.string.plot <- s()
cat(svg.string.plot, file = "lmer_output/leverage_participants_lmer_forced_mturk.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/leverage_participants_lmer_forced_mturk.txt")
dev.off()


high_leverage_participants = infl.classes[infl.classes$leverage.overall > CutOffLeverageParticipants, ] %>%
  arrange(desc(leverage.overall))

# head(high_leverage, n=10)
high_leverage_participants

## No high leverage participants


###################################
###################################

# ANOVA 2x2 approach 

###################################
###################################

# 
# library(rstatix)
# library(ggpubr)
# library(ez) 
# 
# df %>%
#   group_by(sex, ethnicity) %>%
#   get_summary_stats(sentimentScore, type = "mean_sd")
# 
# bxp.sex <- ggboxplot(
#     df, 
#     x = "sex",
#     y = "sentimentScore",
#     palette = "jco"
#   )
# bxp.sex
# 
# bxp.ethnicity <- ggboxplot(
#     df, 
#     x = "ethnicity",
#     y = "sentimentScore",
#     palette = "jco"
#   )
# bxp.ethnicity
# 
# bxp <- ggboxplot(
#     df,
#     x = "sex",
#     y = "sentimentScore",
#     color = "ethnicity",
#     palette = "jco"
#   )
# bxp
# 
# df %>%
#   group_by(ethnicity, sex) %>%
#   identify_outliers(sentimentScore)
# 
# # no outliers
# 
# df %>%
#   group_by(ethnicity, sex) %>%
#   shapiro_test(sentimentScore)
# 
# # not normal
# 
# ggqqplot(df, "sentimentScore", ggtheme = theme_bw()) +
#   facet_grid(sex ~ ethnicity, labeller = "label_both")
# 
# res.aov  = ezANOVA(data=df,
#                    wid=.(participantId), 
#                    dv=.(sentimentScore), 
#                    within=.(sex, ethnicity))
# res.aov  
