
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

df_mturk = read_csv("../clean_data_mturk/free_labeling_emotion_mturk_long_format_lmer.csv")
df_uw = read_csv("../clean_data/free_labeling_emotion_uw_students_long_format_lmer.csv")


colnames(df_mturk) 
colnames(df_uw) 

df_mturk$participantId <- df_mturk$participantId + 100 

dim(table(df_mturk$participantId)) # 50
dim(table(df_uw$participantId))  # 49


df <- rbind(df_mturk, df_uw)

####################################
# Descriptives
library(rstatix)

df %>%
  group_by(sex) %>%
  get_summary_stats(sentimentScore, type = "mean_se")
# 
# sex    variable           n   mean    se
# <chr>  <chr>          <dbl>  <dbl> <dbl>
# 1 female sentimentScore 14038 -0.087 0.003
# 2 male   sentimentScore 14066 -0.116 0.003


df %>%
  group_by(ethnicity) %>%
  get_summary_stats(sentimentScore, type = "mean_se")

# 
# ethnicity variable           n   mean    se
# <chr>     <chr>          <dbl>  <dbl> <dbl>
# 1 bipoc     sentimentScore 14107 -0.103 0.003
# 2 white     sentimentScore 13997 -0.1   0.003

#####################################
#####################################

# Fit LMER 

#####################################
#####################################


## without derivatives check
# control=lmerControl(optimizer ="Nelder_Mead", calc.derivs=FALSE,optCtrl=list(maxfun=2e6), check.nobs.vs.nRE = "ignore")
m2<-lmer(
  sentimentScore ~ 1 + sexC*ethnicityC + (1 + sexC*ethnicityC|participantId), 
  data = df)

summary(m2)
tab_model(m2)
car::Anova(m2, type="3")

# 
# Random effects:
#   Groups        Name            Variance  Std.Dev. Corr             
# participantId (Intercept)     0.0045435 0.06741                   
# sexC            0.0001045 0.01022  -0.73            
# ethnicityC      0.0002298 0.01516   0.57 -0.98      
# sexC:ethnicityC 0.0003132 0.01770  -0.89  0.96 -0.89
# Residual                      0.1436166 0.37897                   
# Number of obs: 28104, groups:  participantId, 99
# 
# Fixed effects:
#   Estimate Std. Error         df t value Pr(>|t|)    
# (Intercept)      -0.103316   0.007215  99.349933 -14.320  < 2e-16 ***
#   sexC             -0.028279   0.004650 331.936886  -6.081 3.28e-09 ***
#   ethnicityC        0.002365   0.004807 113.936433   0.492    0.624    
# sexC:ethnicityC   0.064156   0.009229 913.293754   6.952 6.86e-12 ***
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
# 
# Correlation of Fixed Effects:
#   (Intr) sexC   ethncC
# sexC        -0.155              
# ethnicityC   0.176 -0.072       
# sxC:thnctyC -0.162  0.048 -0.060

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

## There are quite a couple of large residuals 

resid2 = hlm_resid(m2, level = "participantId", standardize = TRUE, include.ls = FALSE)


ggplot(data = resid2, aes(x = participantId, y = .std.ranef.intercept)) + 
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(y = "Random effects - intercept", 
       title = "Intercept random effects against participant ID") + theme_apa()

# probably two outliers

#####################################
#####################################

# Check normality of error term

#####################################
#####################################
library(lattice)

qqmath(m2, id=0.05) #id: identifies values that may be exerting undue influence on the model (i.e. outliers)


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

#### [1] 25909 25598 27520 19120 26036 25922 26051

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

# 43 and 146


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

length(high_leverage$id) # 67

# [1] 27524 27525 27533 27534 27538 27526 27514 27523 27520 27522 27516 27498 14833 14841 14843
# [16] 14852 14837 14838 14858 14848 14883 14860 14880 14871 14878 14920 14882 14907 14869 14895
# [31] 14866 14919 14859 14861 14894 14893 14908 14910 14911 14913 27492 27493 27496 27505 27518
# [46] 27527 27528 27519 27521 27513 27517 27529 27530 27542 27544 27531 27532 27503 27497 27499
# [61] 27502 27507 27508 27515 27501 27506 27500

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


#################
# remove outliers
#add index column to data frame
df$id <- 1:nrow(df)

high_cooksd$id
high_leverage$id
high_cooksd_participants # 43 and 146


nrow(df) # 28104


`%ni%` <- Negate(`%in%`)
df.filtered <- filter(df, id %ni% high_cooksd$id) # remove high cook obs
df.filtered <- filter(df.filtered, id %ni% high_leverage$id) # remove high leverage obs
df.filtered <- filter(df.filtered, participantId != 43) # remove high cook participants
df.filtered <- filter(df.filtered, participantId != 146)

nrow(df.filtered) # 26998


#####################################
#####################################

# Refitted LMER 
# - Excluding outliers; high influence; high leverage observations and participants 

#####################################
#####################################

### traditional lmer approach ####

m3 <-lmer(
  sentimentScore ~ 1 + sexC*ethnicityC + (1 + sexC*ethnicityC|participantId), 
  data = df.filtered)

summary(m3)


###########################
## charts article

library(papaja)


###########
# sex chart

sex.score <- df.filtered %>%
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

ggsave('accuracy-charts/sex_linear_mturk_uw.png', width = 4, height = 4)


#################
# ethnicity chart

df.filtered$ethnicity <- ifelse(df.filtered$ethnicity == "bipoc", "poc", "white")

et.score <- df.filtered %>%
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

ggsave('accuracy-charts/et_linear_mturk_uw.png', width = 4, height = 4)


#################
# ethnicity x sex chart

sex.et.score <- df.filtered %>%
  group_by(sex, ethnicity) %>%
  get_summary_stats(sentimentScore, type = "mean_se")

sex.et.score

sex.et.score.plot <- ggplot(sex.et.score, aes(x=sex, y=mean, fill=ethnicity)) + 
  geom_bar(position=position_dodge(), stat="identity", color="black") +
  labs(x = "image sex",
       y = "sentiment score") + 
  theme_apa()

sex.et.score.plot

ggsave('accuracy-charts/sex_et_linear_mturk_uw.png', width = 6, height = 4)
