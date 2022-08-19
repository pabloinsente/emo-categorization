
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
library(ggforce)
library(papaja)

df = read_csv("../clean_data_mturk_espanol/free_labeling_emotion_mturk_long_format_lmer_espanol.csv") 


####################################
# Descriptives
library(rstatix)

df %>%
  group_by(sex) %>%
  get_summary_stats(sentimentScore, type = "mean_se")
# 
# sex    variable           n   mean    se
# <chr>  <chr>          <dbl>  <dbl> <dbl>
# 1 female sentimentScore  4594 -0.077 0.006
# 2 male   sentimentScore  4536 -0.075 0.006


df %>%
  group_by(ethnicity) %>%
  get_summary_stats(sentimentScore, type = "mean_se")

# 
# ethnicity variable           n   mean    se
# <chr>     <chr>          <dbl>  <dbl> <dbl>
# 1 bipoc     sentimentScore  4613 -0.08  0.006
# 2 white     sentimentScore  4517 -0.072 0.006


df %>%
  group_by(sex, ethnicity) %>%
  get_summary_stats(sentimentScore, type = "mean_se")
# 
# ethnicity sex    variable           n   mean    se
# <chr>     <chr>  <chr>          <dbl>  <dbl> <dbl>
# 1 bipoc     female sentimentScore  2294 -0.065 0.009
# 2 white     female sentimentScore  2300 -0.089 0.008
# 3 bipoc     male   sentimentScore  2319 -0.095 0.008
# 4 white     male   sentimentScore  2217 -0.055 0.009

#####################################
#####################################

# Fit LMER 

#####################################
#####################################

m2<-lmer(
  sentimentScore ~ 1 + sexC*ethnicityC + (1 + sexC*ethnicityC|participantId), 
  data = df)

summary(m2)
tab_model(m2)
car::Anova(m2, type="3")

# Random effects:
#   Groups        Name            Variance  Std.Dev. Corr          
# participantId (Intercept)     2.700e-03 0.051959               
# sexC            2.042e-06 0.001429 1.00          
# ethnicityC      1.124e-04 0.010601 1.00 1.00     
# sexC:ethnicityC 1.446e-04 0.012025 1.00 1.00 1.00
# Residual                      1.661e-01 0.407590               
# Number of obs: 9130, groups:  participantId, 33
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)     -8.179e-02  1.009e-02  3.295e+01  -8.103 2.39e-09 ***
# sexC             7.002e-04  8.540e-03  6.119e+03   0.082  0.93465    
# ethnicityC       6.313e-03  8.737e-03  3.270e+02   0.723  0.47050    
# sexC:ethnicityC  6.185e-02  1.720e-02  9.648e+02   3.596  0.00034 ***
# ---
# Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) sexC  ethncC
# sexC        0.029              
# ethnicityC  0.198  0.018       
# sxC:thnctyC 0.117  0.014 0.033 

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

# Since the p value < 0.05, we can say that the variance of the residuals is equal and 
# therefore the assumption of **homoscedasticity** NOT is met 


Plot.Model.F <- plot(m2) #creates a fitted vs residual plot
Plot.Model.F



resid1 <- hlm_resid(m2, level = 1, standardize = TRUE)

ggplot(data = resid1, aes(x = participantId, y = .std.ls.resid)) + 
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(y = "Least-Squares level-1 residuals", 
       title = "Least-Squares residuals by participant ID") + theme_apa()

## residuals look kinda ok

resid2 = hlm_resid(m2, level = "participantId", standardize = TRUE, include.ls = FALSE)


ggplot(data = resid2, aes(x = participantId, y = .std.ranef.intercept)) + 
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(y = "Random effects - intercept", 
       title = "Intercept random effects against participant ID") + theme_apa()

## residuals look kinda ok

#####################################
#####################################

# Check normality of error term

#####################################
#####################################
library(lattice)

qqmath(m2, id=0.05) 
#id: identifies values that may be exerting undue influence on the model (i.e. outliers)


#####################################
#####################################

# Check influence

#####################################
#####################################
invisible(utils::memory.limit(64000))


#############################

# check datapoints influence

#############################

infl <- hlm_influence(m2, level = 1)

# IQR = as.numeric(format(IQR(infl$cooksd)*3, scientific = F))
CutOff = 4/nrow(infl)
print(CutOff)


dotplot_diag(infl$cooksd, name = "cooks.distance", cutoff = CutOff) + theme_apa()


high_cooksd = infl[infl$cooksd > CutOff, ] %>%
  arrange(desc(cooksd))

head(high_cooksd, n=10)

high_cooksd$id

### NONE

#############################

# check participants influence

#############################

infl.classes <- hlm_influence(m2, level = "participantId")


CutOffGroup = 4/length(table(df$participantId))

CutOffGroup

# dotplot_diag(infl.classes$cooksd, name = "cooks.distance", cutoff = "internal", modify = "dotplot")
dotplot_diag(infl.classes$cooksd, name = "cooks.distance", cutoff = CutOffGroup, modify = "dotplot") + theme_apa()


high_cooksd_participants = infl.classes[infl.classes$cooksd > CutOffGroup, ] %>%
  arrange(desc(cooksd))

high_cooksd_participants

### NONE


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

# dotplot_diag(infl$leverage.overall, name = "leverage", cutoff = "internal")
dotplot_diag(infl$leverage.overall, name = "leverage", cutoff = CutOffLeverage) + theme_apa()


high_leverage = infl[infl$leverage.overall > CutOffLeverage, ] %>%
  arrange(desc(leverage.overall))

# head(high_leverage, n=10)
high_leverage

# high leverage data points
high_leverage$id

#### NONE

#############################

# check participants leverage

#############################

CutOffLeverageParticipants = mean(infl.classes$leverage.overall)*3
CutOffLeverageParticipants

# dotplot_diag(infl.classes$leverage.overall, name = "leverage", cutoff = "internal")
dotplot_diag(infl.classes$leverage.overall, name = "leverage", cutoff = CutOffLeverageParticipants) + theme_apa()

high_leverage_participants = infl.classes[infl.classes$leverage.overall > CutOffLeverageParticipants, ] %>%
  arrange(desc(leverage.overall))

# head(high_leverage, n=10)
high_leverage_participants

## No high leverage participants

#############################
# NO NEED TO REFIT MODEL 
############################


###########################
## charts article

library(papaja)


###########
# sex chart

sex.score <- df %>%
  group_by(sex) %>%
  get_summary_stats(sentimentScore, type = "mean_se")

sex.score


sex.score.plot <- ggplot(sex.score, aes(x=sex, y=mean, fill=sex)) + 
  geom_bar(position=position_dodge(), stat="identity",  color="black") +
  labs(x = "image sex",
       y = "sentiment score") + 
  guides(fill="none") +
  theme_apa()

sex.score.plot

ggsave('accuracy-charts/sex_linear_mturk_espanol.png', width = 4, height = 4)


#################
# ethnicity chart

df$ethnicity <- ifelse(df$ethnicity == "bipoc", "poc", "white")

et.score <- df %>%
  group_by(ethnicity) %>%
  get_summary_stats(sentimentScore, type = "mean_se")

et.score

et.score.plot <- ggplot(et.score, aes(x=ethnicity, y=mean, fill=ethnicity)) + 
  geom_bar(position=position_dodge(), stat="identity", color="black") +
  labs(x = "image ethnicity",
       y = "sentiment score") + 
  guides(fill="none") +
  theme_apa()

et.score.plot

ggsave('accuracy-charts/et_linear_mturk_espanol.png', width = 4, height = 4)


#################
# ethnicity x sex chart

sex.et.score <- df %>%
  group_by(sex, ethnicity) %>%
  get_summary_stats(sentimentScore, type = "mean_se")

sex.et.score

sex.et.score.plot <- ggplot(sex.et.score, aes(x=sex, y=mean, fill=ethnicity)) + 
  geom_bar(position=position_dodge(), stat="identity", color="black") +
  labs(x = "image sex",
       y = "sentiment score") + 
  theme_apa()

sex.et.score.plot

ggsave('accuracy-charts/sex_et_linear_mturk_espanol.png', width = 6, height = 4)
