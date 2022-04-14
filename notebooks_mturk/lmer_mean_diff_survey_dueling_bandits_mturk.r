library(svglite)
library(tidyverse)
library(ggpubr)
library(rstatix)
library(sjPlot)
library(knitr)
library(equatiomatic)
library(ggforce)
library(papaja)

#####################
# read in data
#####################

# read students ranking 
df.rank = read_csv('../data_mturk/emotion_top_2_word_survey_dueling_bandits_mturk.csv')

# read frequency in the web ranking
unigram.freq= read_csv('../data/unigram_freq.csv')

head(df.rank)
head(unigram.freq)


# merge datasets
df = merge(x = df.rank, 
           y = unigram.freq,
           by.x="word",
           by.y="word")

names(df)[7]  <- 'web.frequency'

head(df)


####################
# T-test assumptions

## check outliers 
outliers <- df %>% identify_outliers(web.frequency)
outliers # content, friendly, interested, serious


# filter out extreme outliers
df2 <- subset(df, photoID != 15 & photoID != 13 & photoID != 20)

## Check normality assumption
df2 %>% shapiro_test(web.frequency)
# not normal

ggqqplot(df2, x = "web.frequency")
# not normal

################
# basic exploration

df2 %>%
  group_by(method) %>%
  get_summary_stats(web.frequency, type = "mean_se")


# grouped boxplot
boxplot <- ggplot(df2, aes(x = method, y = web.frequency, color=method)) + 
  geom_boxplot() +
  geom_point() +
  guides(color="none") + 
  theme_apa()

boxplot

s <- svgstring(width = 7,
               height = 5)

boxplot

svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/web_freq_method_boxplot_mturk.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/web_freq_method_boxplot_mturk.txt")

dev.off()


ggsave('accuracy-charts/web_freq_method_boxplot_mturk.png', width = 8, height = 4)

####################
# T test 

stat.test <- df2  %>% 
  t_test(web.frequency ~ method, paired = TRUE) %>%
  add_significance()
stat.test

########################
########################
# Lmer because repeated 
# measures by photoId
########################
########################

library(lme4)
library(car)

## format predictors
df2$method.dummy <- ifelse(df2$method == "survey", 1, 0)
df2$method.center <- ifelse(df2$method == "survey", .5, -.5)


m1<-lmer(
  web.frequency ~ 1 + method.dummy + (1 |photoID), 
  data = df2)

summary(m1)

tab_model(m1)

tab_model(m1, file = "lmer_output/lmer_summary_method_ranking_mturk.html")
tab_model(m1, file = "../../emotions_dashboard/data/lmer_summary_method_ranking_mturk.html")


Anova(m1, type = "III")
# 
# Analysis of Deviance Table (Type III Wald chisquare tests)
# 
# Response: web.frequency
# Chisq Df Pr(>Chisq)    
# (Intercept)  18.6964  1  1.533e-05 ***
#   method.dummy  4.8155  1    0.02821 *  
#   ---
#   Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1


## Type III anova table with p-values for F-tests based on Satterthwaite's method

aov <- anova(m1)

aov.apa <- kable(aov, digits = 3, format = "html", caption = "ANOVA table for LMER coefficients")
cat(aov.apa, file = "lmer_output/anova_lmer_method_ranking_free_mturk.html")
cat(aov.apa, file = "../../emotions_dashboard/data/anova_lmer_method_ranking_free_mturk.html")


m2<-lmer(
  web.frequency ~ 1 + method.center + (1 |photoID), 
  data = df2)

summary(m2)

Anova(m1, type = "III")

# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)    4691069     460704  10.182
# method.center  2697361     845246   3.191


### get mathematical formula
formula_lmer <- extract_eq(m1)

cat(formula_lmer, file = "lmer_output/formula_method_lmer_mturk.txt")
cat(formula_lmer, file = "../../emotions_dashboard/data/formula_method_lmer_mturk.txt")


#####################################
#####################################

# significant effects charts

#####################################
#####################################

method.table <- df2 %>%
                group_by(method) %>%
                get_summary_stats(web.frequency, type = "mean_se")

names(method.table)[4] <- "web.frequency.mean"


# # Mean + std error of the mean 
freq.method <- ggplot(method.table, aes(x=method, y=web.frequency.mean, color=method)) +
                      geom_errorbar(aes(ymin=web.frequency.mean-se, ymax=web.frequency.mean+se), width=.1) +
                      geom_point() +
                      labs (title= "Mean and SEM web frequency by survey method") +
                      guides(color="none") + 
                      theme_apa()

freq.method

s <- svgstring(width = 7,
               height = 5)

freq.method

svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/web_freq_method_mturk.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/web_freq_method_mturk.txt")

dev.off()


ggsave('accuracy-charts/web_freq_method_mturk.png', width = 8, height = 4)


#%%%%%%%%%%%%%%%%%%%%%%%%
#########################
#########################

# Diagnostics


library(lmerTest)
library(ragg)
library(HLMdiag)
library(VCA)

#####################################
#####################################

# Check homogeneity of variance

#####################################
#####################################

# https://ademos.people.uic.edu/Chapter18.html
# ANOVA of the between subjects residuals.
# the assumption is that the variance is not going to differ, we would hope to see 
# NO STATISTICAL DIFFERENCES in the following procedure (i.e. p>0.05)

df2$Model.F.Res<- residuals(m1) #extracts the residuals and places them in a new column in our original data table
df2$Abs.Model.F.Res <-abs(df2$Model.F.Res) #creates a new column with the absolute value of the residuals
df2$Model.F.Res2 <- df2$Abs.Model.F.Res^2 #squares the absolute values of the residuals to provide the more robust estimate
Levene.Model.F <- lm(Model.F.Res2 ~ photoID, data=df2) #ANOVA of the squared residuals
anova(Levene.Model.F) #displays the results

# Since the p value > 0.05, all good

# save to html table
aov.btw.res <- kable(anova(Levene.Model.F), digits = 3, format = "html", caption = "ANOVA table for between subjects residuals")

cat(aov.btw.res, file = "lmer_output/anova_bwt_lmer_method_mturk.html")
cat(aov.btw.res, file = "../../emotions_dashboard/data/anova_bwt_lmer_method_mturk.html")

Plot.Model.F <- plot(m1) #creates a fitted vs residual plot
Plot.Model.F

# looks fine

s <- svgstring(width = 7,
               height = 5)

Plot.Model.F

Plot.Model.F <- s()
cat(Plot.Model.F , file = "lmer_output/fitted_vs_residual_plot_method_mturk.txt")
cat(Plot.Model.F , file = "../../emotions_dashboard/data/fitted_vs_residual_plot_method_mturk.txt")
dev.off()

#####################################
#####################################

# Check normality of error term

#####################################
#####################################

require("lattice")

qqmath(m1, id=0.05) #id: identifies values that may be exerting undue influence on the model (i.e. outliers)


s <- svgstring(width = 7,
               height = 5)

qqmath(m1, id=0.05) #id: identifies values that may be exerting undue influence on the model (i.e. outliers)
svg.qqplot <- s()
cat(svg.qqplot, file = "lmer_output/qqplot_lmer_method_mturk.txt")
cat(svg.qqplot, file = "../../emotions_dashboard/data/qqplot_lmer_method_mturk.txt")
dev.off()

#####################################
#####################################

# Check influence

#####################################
#####################################

##################
# datapoints level

infl <- hlm_influence(m1, level = 1)
# IQR = as.numeric(format(IQR(infl$cooksd)*3, scientific = F))
CutOff = 4/nrow(infl)
print(CutOff)
dotplot_diag(infl$cooksd, name = "cooks.distance", cutoff = CutOff) + theme_apa()


s <- svgstring(width = 7,
               height = 5)

# dotplot_diag(infl$cooksd, name = "cooks.distance", cutoff = "internal")
dotplot_diag(infl$cooksd, name = "cooks.distance", cutoff = CutOff) + theme_apa()
svg.string.plot <- s()
cat(svg.string.plot, file = "lmer_output/influence_datapoints_lmer_method_mturk .txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/influence_datapoints_lmer_method_mturk.txt")
dev.off()


high_cooksd = infl[infl$cooksd > CutOff, ] %>%
  arrange(desc(cooksd))

head(high_cooksd, n=10)

#### high influence data points

high_cooksd$id # 26 30 27 29 28 25


##############
# images level
infl.classes <- hlm_influence(m1, level = "photoID")


CutOffGroup = 4/21


dotplot_diag(infl.classes$cooksd, name = "cooks.distance", cutoff = "internal", modify = "dotplot") + theme_apa()


s <- svgstring(width = 7,
               height = 5)

dotplot_diag(infl.classes$cooksd, name = "cooks.distance", cutoff = "internal", modify = "dotplot") + theme_apa()

svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/influence_participants_lmer_method_mturk.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/influence_participants_lmer_method_mturk.txt")
dev.off()



high_cooksd_participants = infl.classes[infl.classes$cooksd > CutOffGroup, ] %>%
  arrange(desc(cooksd))

high_cooksd_participants # none


#####################################
#####################################

# Check leverage

#####################################
#####################################

####################
## observation level 
CutOffLeverage = mean(infl$leverage.overall)*3
CutOffLeverage

dotplot_diag(infl$leverage.overall, name = "leverage", cutoff = CutOffLeverage) + theme_apa()


s <- svgstring(width = 7,
               height = 5)

# dotplot_diag(infl$leverage.overall, name = "leverage", cutoff = "internal")
dotplot_diag(infl$leverage.overall, name = "leverage", cutoff = CutOffLeverage) + theme_apa()

svg.string.plot <- s()
cat(svg.string.plot, file = "lmer_output/leverage_datapoints_lmer_method_mturk.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/leverage_datapoints_lmer_method_mturk.txt")
dev.off()


# None

high_leverage = infl[infl$leverage.overall > CutOffLeverage, ] %>%
  arrange(desc(leverage.overall))

# head(high_leverage, n=10)
high_leverage

# high leverage data points
high_leverage$id

################
## photo level

CutOffLeverageParticipants = mean(infl.classes$leverage.overall)*3
CutOffLeverageParticipants

dotplot_diag(infl.classes$leverage.overall, name = "leverage", cutoff = CutOffLeverageParticipants) + theme_apa()


s <- svgstring(width = 7,
               height = 5)

dotplot_diag(infl.classes$leverage.overall, name = "leverage", cutoff = CutOffLeverageParticipants) + theme_apa()

svg.string.plot <- s()
cat(svg.string.plot, file = "lmer_output/leverage_participants_lmer_method_mturk.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/leverage_participants_lmer_method_mturk.txt")
dev.off()

# none

###########################
##########################
# filter out high cook ids

nrow(df2)

#add index column to data frame
df2$id <- 1:nrow(df2)

`%ni%` <- Negate(`%in%`)
df.filtered <- filter(df2, id %ni% high_cooksd$id)


nrow(df.filtered)

#####################################
#####################################

# Refitted LMER 

######################################
######################################

################
# basic exploration

df.filtered %>%
  group_by(method) %>%
  get_summary_stats(web.frequency, type = "mean_se")


# grouped boxplot
boxplot.re <- ggplot(df.filtered, aes(x = method, y = web.frequency, color=method)) + 
  geom_boxplot() +
  geom_point() +
  guides(color="none") + 
  theme_apa()

boxplot.re

#############
# rerun
m3<-lmer(
  web.frequency ~ 1 + method.dummy + (1 |photoID), 
  data = df.filtered)

summary(m3)
Anova(m3, type = "III")
tab_model(m3)

#####################################
#####################################

# significant effects charts

#####################################
#####################################

method.table.re <- df.filtered %>%
  group_by(method) %>%
  get_summary_stats(web.frequency, type = "mean_se")

names(method.table.re)[4] <- "web.frequency.mean"
method.table.re

# # Mean + std error of the mean 
freq.method.re <- ggplot(method.table.re, aes(x=method, y=web.frequency.mean, color=method)) +
  geom_errorbar(aes(ymin=web.frequency.mean-se, ymax=web.frequency.mean+se), width=.1) +
  geom_point() +
  labs (title= "Mean and SEM web frequency by survey method") +
  guides(color="none") + 
  theme_apa()

freq.method.re



##################################
##################################

# reporting reffited model

#################################
#################################

#########
# boxplot 
s <- svgstring(width = 7,
               height = 5)

boxplot.re

svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/web_freq_method_boxplot_mturk_re.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/web_freq_method_boxplot_mturk_re.txt")

dev.off()


ggsave('accuracy-charts/web_freq_method_boxplot_mturk_re.png', width = 8, height = 4)

################
#  model summary

tab_model(m3, file = "lmer_output/lmer_summary_method_ranking_mturk_re.html")
tab_model(m3, file = "../../emotions_dashboard/data/lmer_summary_method_ranking_mturk_re.html")

#######
# Anova
## Type III anova table with p-values for F-tests based on Satterthwaite's method

aov <- anova(m3)

aov.apa.re <- kable(aov, digits = 3, format = "html", caption = "ANOVA table for LMER coefficients")
cat(aov.apa.re, file = "lmer_output/anova_lmer_method_ranking_mturk_re.html")
cat(aov.apa.re, file = "../../emotions_dashboard/data/anova_lmer_method_ranking_mturk_re.html")


###################
# main effect chart

# # Mean + std error of the mean 


s <- svgstring(width = 7,
               height = 5)

freq.method.re

svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/web_freq_method_mturk_re.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/web_freq_method_mturk_re.txt")

dev.off()


ggsave('accuracy-charts/web_freq_method_mturk_re.png', width = 8, height = 4)


qqmath(m1, id=0.05) #id: identifies values that may be exerting undue influence on the model (i.e. outliers)

qqmath(m3, id=0.05) #id: identifies values that may be exerting undue influence on the model (i.e. outliers)


