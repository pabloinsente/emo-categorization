#install.packages(c("lme4", "car", "lmerTest", "lmerTest", 
#                   "tidyverse", "ragg", "HLMdiag", "VCA", 
#                   "hrbrthemes", "ggResidpanel", "sjPlot",
#                   "kableExtra", "knitr", "remotes", 
#                   "equatiomatic", 'textpreview'))

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
library(textpreview)

df = read_csv("../clean_data/free_labeling_emotion_uw_students_long_format_lmer.csv")

head(df)

df %>%  ggplot(aes(x=sentimentScore)) +
    geom_histogram(fill="#1f77b4") +
    theme_ipsum()

by.sex <- df %>%
  ggplot( aes(x=sentimentScore, fill=sex)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#1f77b4", "#ff7f0e")) +
    theme_ipsum() +
    stat_bin(bins=20) +
    labs(fill="")
by.sex

by.age <- df %>%
  ggplot( aes(x=sentimentScore, fill=age)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    theme_ipsum() +
    stat_bin(bins=20) +
    labs(fill="")
by.age

by.ethnicity <- df %>%
  ggplot( aes(x=sentimentScore, fill=ethnicity)) +
    geom_histogram( color="#5e6162", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080")) +
    theme_ipsum() +
    stat_bin(bins=20) +
    labs(fill="")
by.ethnicity

df %>%
  group_by(participantId) %>%
  summarise_at(vars(sentimentScore), list(name = mean)) %>% 
  ggplot(aes(x=name)) +
    geom_histogram(color="#5e6162", fill="#66a3a3") +
    theme_ipsum()

df %>%
  group_by(photoId) %>%
  summarise_at(vars(sentimentScore), list(name = mean)) %>% 
  ggplot(aes(x=name)) +
    geom_histogram(color="#5e6162", fill="#66a3a3") +
    theme_ipsum()

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

#  with derivatives check
# control.check=lmerControl(optimizer ="Nelder_Mead", optCtrl=list(maxfun=2e6), check.nobs.vs.nRE = "ignore")


# control=lmerControl(optimizer ="Nelder_Mead", calc.derivs=FALSE,optCtrl=list(maxfun=2e6),check.nobs.vs.nRE = "ignore")
# m2<-lmer(
#     sentimentScore ~ 1 + sexC*ethnicityC + (1 + sexC*ethnicityC|participantId), 
#     data = df,
#     control=control.check)
# summary(m2)

## doesn't converge

## without derivatives check
control=lmerControl(optimizer ="Nelder_Mead", calc.derivs=FALSE, optCtrl=list(maxfun=2e6), check.nobs.vs.nRE = "ignore")


control=lmerControl(optimizer ="Nelder_Mead", calc.derivs=FALSE,optCtrl=list(maxfun=2e6),check.nobs.vs.nRE = "ignore")
m2<-lmer(
    sentimentScore ~ 1 + sexC*ethnicityC + (1 + sexC*ethnicityC|participantId), 
    data = df,
    control=control)
summary(m2)

### get mathematical formula
formula_lmer <- extract_eq(m2)

cat(formula_lmer, file = "lmer_output/formula_lmer_summary_forced_uw_students.txt")
cat(formula_lmer, file = "../../emotions_dashboard/data/formula_lmer_summary_forced_uw_students.txt")

#extract_eq(m2, use_coefs = TRUE)


### get coefficient table for reporting
tab_model(m2, file = "lmer_output/lmer_summary_forced_uw_students.html")
tab_model(m2, file = "../../emotions_dashboard/data/lmer_summary_forced_uw_students.html")

# Anova(m2,type=3,t="F")  
# ------------------- 
# it won't coverge

## Type III anova table with p-values for F-tests based on Satterthwaite's
## method:

library(knitr)

(aov <- anova(m2))

aov.apa <- kable(aov, digits = 3, format = "html", caption = "ANOVA table for LMER coefficients")
cat(aov.apa, file = "lmer_output/anova_lmer_summary_forced_uw_students.html")
cat(aov.apa, file = "../../emotions_dashboard/data/anova_lmer_summary_forced_uw_students.html")


#format(1.007044e-04, scientific = F)

#format(1.025469e-01, scientific = F)

#format(1.418980e-07, scientific = F)

### UPDATE ###



# sex-photo effect:
# The effect of sex-photo on sentiment-score was not significant, b = -0.01602, F(1,51) = 3.7, p = 0.054
# Averaging across POC and Caucasian photo-faces, and controlling for ethnicity and the ethnicity-photo by sex-photo interaction,
# sentiment-scores were -0.016 more negative in response to male faces than female faces. 

### UPDATE ###


# ethnicity-photo effect:
# The effect of ethnicity-photo on sentiment-scores was significant, b =  0.028, F(1,51) = 11.9, p < .001.
# For sake of completeness:
# Averaging across female and male photos, and controlling for sex-photo and the sex-photo by ethnicity-photo interaction,
# sentiment-scores were 0.028 more positive in response to caucasian-photos than poc-photos

### UPDATE ###


# Face by odor interaction:
# The face by odor interaction was significant, b = 0.04, F(1,51) = 6.03, p = 0.014.
# For sake of completeness:
# Controlling for lower-order effects, the difference in sentiment-scorees between the two sex-photo conditions 
# was 0.04 points greater in the caucasian-photos than in the poc-photos


### Individual participant data for sex * ethnicity conditions

p = ggplot(df,aes(sex,sentimentScore,color=ethnicity,group=ethnicity))+
    geom_point()+
    geom_smooth(method="lm",se=F)+
    facet_wrap(~participantId)+
    theme_bw()+
    scale_color_manual(values=c("#1f77b4", "#ff7f0e"))

p

ggsave("lmer_output/participants_charts_lmer_forced_uw_students.png", width = 800, height = 800, units = "px", scale=5, dpi=400)
ggsave("../../emotions_dashboard/data/participants_charts_lmer_forced_uw_students.png", width = 800, height = 800, units = "px", scale=5, dpi=400)


# ANOVA of the between subjects residuals.
# the assumption is that the variance is not going to differ, we would hope to see 
# NO STATISTICAL DIFFERENCES in the following procedure (i.e. p>0.05)

df$Model.F.Res<- residuals(m2) #extracts the residuals and places them in a new column in our original data table
df$Abs.Model.F.Res <-abs(df$Model.F.Res) #creates a new column with the absolute value of the residuals
df$Model.F.Res2 <- df$Abs.Model.F.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.Model.F <- lm(Model.F.Res2 ~ participantId, data=df) #ANOVA of the squared residuals
anova(Levene.Model.F) #displays the results

format(4.440288e-07, scientific = F)

# Since the p value < 0.05, we can say that the variance of the residuals is equal and 
# therefore the assumption of **homoscedasticity** NOT is met 

Plot.Model.F <- plot(m2) #creates a fitted vs residual plot
Plot.Model.F

## This looks very unsystematic

resid1 <- hlm_resid(m2, level = 1, standardize = TRUE)

ggplot(data = resid1, aes(x = participantId, y = .std.ls.resid)) + 
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(y = "LS level-1 residuals", 
       title = "LS residuals by participant ID")

## There are quite a couple of large residuals 

resid2 = hlm_resid(m2, level = "participantId", standardize = TRUE, include.ls = FALSE)

ggplot(data = resid2, aes(x = participantId, y = .std.ranef.intercept)) + 
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(y = "Level-2 residuals", 
       title = "L2 residuals by participant ID")

# 43, 9, 16

ggplot(data = resid2, aes(x = participantId, y = .std.ranef.sex_c)) + 
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(y = "Level-2 residuals", 
       title = "L2 residuals by participant ID")

# 9, 16

    ggplot(data = resid2, aes(x = participantId, y = .std.ranef.ethnicity_c)) + 
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(y = "Level-2 residuals", 
       title = "L2 residuals by participant ID")

# 43

ggplot(data = resid2, aes(x = participantId, y = .std.ranef.sex_c_ethnicity_c)) + 
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(y = "Level-2 residuals", 
       title = "L2 residuals by participant ID")

# 43, 9

require("lattice")

qqmath(m2, id=0.05) #id: identifies values that may be exerting undue influence on the model (i.e. outliers)

# looks not normal...
# https://ademos.people.uic.edu/Chapter18.html

resid_panel(m2)

infl <- hlm_influence(m2, level = 1)

# IQR = as.numeric(format(IQR(infl$cooksd)*3, scientific = F))
CutOff = 4/nrow(infl)
print(CutOff)

# dotplot_diag(infl$cooksd, name = "cooks.distance", cutoff = "internal")
dotplot_diag(infl$cooksd, name = "cooks.distance", cutoff = CutOff)

# filter(infl, cookd > IQR)

high_cooksd = infl[infl$cooksd > CutOff, ] %>%
  arrange(desc(cooksd))

head(high_cooksd, n=10)

# high influence data points
high_cooksd$id

infl.classes <- hlm_influence(m2, level = "participantId")

CutOffGroup = 4/49
CutOffGroup

# dotplot_diag(infl.classes$cooksd, name = "cooks.distance", cutoff = "internal", modify = "dotplot")
dotplot_diag(infl.classes$cooksd, name = "cooks.distance", cutoff = CutOffGroup, modify = "dotplot")

high_cooksd_participants = infl.classes[infl.classes$cooksd > CutOffGroup, ] %>%
  arrange(desc(cooksd))

high_cooksd_participants

# participant 43

CutOffLeverage = mean(infl$leverage.overall)*3
CutOffLeverage

# dotplot_diag(infl$leverage.overall, name = "leverage", cutoff = "internal")
dotplot_diag(infl$leverage.overall, name = "leverage", cutoff = CutOffLeverage)

high_leverage = infl[infl$leverage.overall > CutOffLeverage, ] %>%
  arrange(desc(leverage.overall))

# head(high_leverage, n=10)
high_leverage

# high leverage data points
high_leverage$id

CutOffLeverageParticipants = mean(infl.classes$leverage.overall)*3
CutOffLeverageParticipants

# dotplot_diag(infl.classes$leverage.overall, name = "leverage", cutoff = "internal")
dotplot_diag(infl.classes$leverage.overall, name = "leverage", cutoff = CutOffLeverageParticipants)

high_leverage_participants = infl.classes[infl.classes$leverage.overall > CutOffLeverageParticipants, ] %>%
  arrange(desc(leverage.overall))

# head(high_leverage, n=10)
high_leverage_participants

## No high leverage participants

# infl2 <- hlm_influence(m2, level = 1, leverage = c("overall", "fixef", "ranef", "ranef.uc"))

# dotplot_diag(infl2$leverage.fixef, name = "leverage", cutoff = "internal", modify = "dotplot")

# aug <- hlm_augment(m2, level = 1)

# aug2 <- aug %>%
#   arrange(desc(cooksd))

# head(aug2)

#add index column to data frame
df$id <- 1:nrow(df)

high_cooksd$id

high_leverage$id

high_cooksd_participants$participantId

nrow(df)

`%ni%` <- Negate(`%in%`)
df.filtered <- filter(df, id %ni% high_cooksd$id)
df.filtered <- filter(df.filtered, id %ni% high_leverage$id)
df.filtered <- filter(df.filtered, participantId != 43)

nrow(df.filtered)

## without derivatives check
control=lmerControl(optimizer ="Nelder_Mead", calc.derivs=FALSE, optCtrl=list(maxfun=2e6), check.nobs.vs.nRE = "ignore")

m3<-lmer(
    sentimentScore ~ 1 + sexC*ethnicityC + (1 + sexC*ethnicityC|participantId), 
    data = df.filtered,
    control=control)

summary(m3)

## Type III anova table with p-values for F-tests based on Satterthwaite's
## method:
(aov <- anova(m3))

#### UPDATE #####

p = ggplot(df.filtered,aes(sex,sentimentScore,color=ethnicity,group=ethnicity))+
    geom_point()+
    geom_smooth(method="lm", se=F)+
    facet_wrap(~participantId)+
    theme_bw()

pngfile <- fs::path(knitr::fig_path(),  "scaling_2.png")
agg_png(pngfile, width = 60, height = 60, units = "cm", res = 300, scaling = 2.5)
plot(p)
invisible(dev.off())
knitr::include_graphics(pngfile)

df.filtered$Model.F.Res<- residuals(m3) #extracts the residuals and places them in a new column in our original data table
df.filtered$Abs.Model.F.Res <-abs(df.filtered$Model.F.Res) #creates a new column with the absolute value of the residuals
df.filtered$Model.F.Res2 <- df.filtered$Abs.Model.F.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.Model.F <- lm(Model.F.Res2 ~ participantId, data=df.filtered) #ANOVA of the squared residuals
anova(Levene.Model.F) #displays the results

Plot.Model.F <- plot(m3) #creates a fitted vs residual plot
Plot.Model.F

resid1.filtered <- hlm_resid(m3, level = 1, standardize = TRUE)

head(resid1.filtered)

ggplot(data = resid1.filtered, aes(x = participantId, y = .std.ls.resid)) + 
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(y = "LS level-1 residuals", 
       title = "LS residuals by participant ID")

resid2.filtered = hlm_resid(m3, level = "participantId", standardize = TRUE, include.ls = FALSE)

ggplot(data = resid2.filtered, aes(x = participantId, y = .std.ranef.intercept)) + 
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(y = "Level-2 residuals", 
       title = "L2 residuals by participant ID")

ggplot(data = resid2.filtered, aes(x = participantId, y = .std.ranef.sex_c)) + 
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(y = "Level-2 residuals", 
       title = "L2 residuals by participant ID")

ggplot(data = resid2.filtered, aes(x = participantId, y = .std.ranef.ethnicity_c)) + 
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(y = "Level-2 residuals", 
       title = "L2 residuals by participant ID")

ggplot(data = resid2.filtered, aes(x = participantId, y = .std.ranef.sex_c_ethnicity_c)) + 
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(y = "Level-2 residuals", 
       title = "L2 residuals by participant ID")

qqmath(m3, id=0.05) #id: identifies values that may be exerting undue influence on the model (i.e. outliers)

resid_panel(m3)

infl.filtered <- hlm_influence(m3, level = 1)

CutOff = 4/nrow(infl.filtered)
print(CutOff)


dotplot_diag(infl.filtered$cooksd, name = "cooks.distance", cutoff = CutOff)

high_cooksd_filtered = infl.filtered[infl.filtered$cooksd > CutOff, ] %>%
  arrange(desc(cooksd))
high_cooksd_filtered

infl.classes.filtered <- hlm_influence(m3, level = "participantId")

CutOffGroupFiltered = 4/48
CutOffGroupFiltered

dotplot_diag(infl.classes.filtered$cooksd, name = "cooks.distance", cutoff = CutOffGroupFiltered, modify = "dotplot")

library(rstatix)
library(ggpubr)
library(ez) 

df %>%
  group_by(sex, ethnicity) %>%
  get_summary_stats(sentimentScore, type = "mean_sd")

bxp.sex <- ggboxplot(
    df, 
    x = "sex",
    y = "sentimentScore",
    palette = "jco"
  )
bxp.sex

bxp.ethnicity <- ggboxplot(
    df, 
    x = "ethnicity",
    y = "sentimentScore",
    palette = "jco"
  )
bxp.ethnicity

bxp <- ggboxplot(
    df,
    x = "sex",
    y = "sentimentScore",
    color = "ethnicity",
    palette = "jco"
  )
bxp

df %>%
  group_by(ethnicity, sex) %>%
  identify_outliers(sentimentScore)

# no outliers

df %>%
  group_by(ethnicity, sex) %>%
  shapiro_test(sentimentScore)

# not normal

ggqqplot(df, "sentimentScore", ggtheme = theme_bw()) +
  facet_grid(sex ~ ethnicity, labeller = "label_both")

res.aov  = ezANOVA(data=df,
                   wid=.(participantId), 
                   dv=.(sentimentScore), 
                   within=.(sex, ethnicity))
res.aov  
