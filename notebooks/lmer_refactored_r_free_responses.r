#install.packages(c("lme4", "car", "lmerTest", "lmerTest", 
#                   "tidyverse", "ragg", "HLMdiag", "VCA", 
#                   "hrbrthemes", "ggResidpanel", "sjPlot",
#                   "kableExtra", "knitr", "remotes", 
#                   "equatiomatic", 'textpreview', "heavy",
#                   "robustlmm"))

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


df = read_csv("../clean_data/free_labeling_emotion_uw_students_long_format_lmer.csv")


########################
########################

# descriptives

########################
########################


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


##
# MAXIMAL MODEL doesn't converge at all / tried multiple optimizers 
##

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

### model comparison ####

m2.sex <-lmer(
  sentimentScore ~ 1 + sexC +  (1+ sexC*ethnicityC|participantId), 
  data = df,
  control=control)

summary(m2.sex)
anova(m2, m2.sex)

m2.ethnicityC <-lmer(
  sentimentScore ~ 1 + ethnicityC + (1+ sexC*ethnicityC |participantId), 
  data = df,
  control=control)

summary(m2.ethnicityC)
anova(m2, m2.ethnicityC)

m2.add <-lmer(
  sentimentScore ~ 1 + sexC+ethnicityC + (1+sexC*ethnicityC |participantId), 
  data = df,
  control=control)

summary(m2.add)
anova(m2, m2.add)

full.interactive.model <- m2
sex.only.model <- m2.sex
ethnicity.only.model <- m2.ethnicityC
sex.ethnicity.additive.model <- m2.add

anova(sex.only.model, ethnicity.only.model, sex.ethnicity.additive.model, full.interactive.model)

(aov.comparison <- anova(sex.only.model, ethnicity.only.model, sex.ethnicity.additive.model, full.interactive.model))

aov.apa.com <- kable(aov.comparison, digits = 3, format = "html", caption = "ANOVA table for model comparison")
cat(aov.apa.com, file = "lmer_output/anova_comparison_lmer_summary_free_uw_students.html")
cat(aov.apa.com, file = "../../emotions_dashboard/data/anova_comparison_lmer_summary_free_uw_students.html")


### get mathematical formula
formula_lmer <- extract_eq(m2)

cat(formula_lmer, file = "lmer_output/formula_lmer_summary_free_uw_students.txt")
cat(formula_lmer, file = "../../emotions_dashboard/data/formula_lmer_summary_free_uw_students.txt")

#extract_eq(m2, use_coefs = TRUE)


### get coefficient table for reporting
tab_model(m2, file = "lmer_output/lmer_summary_free_uw_students.html")
tab_model(m2, file = "../../emotions_dashboard/data/lmer_summary_free_uw_students.html")


## Type III anova table with p-values for F-tests based on Satterthwaite's method

(aov <- anova(m2))

aov.apa <- kable(aov, digits = 3, format = "html", caption = "ANOVA table for LMER coefficients")
cat(aov.apa, file = "lmer_output/anova_lmer_summary_free_uw_students.html")
cat(aov.apa, file = "../../emotions_dashboard/data/anova_lmer_summary_free_uw_students.html")


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

s <- svgstring()

p = ggplot(df,aes(sex,sentimentScore,color=ethnicity,group=ethnicity))+
    geom_point()+
    geom_smooth(method="lm",se=F)+
    facet_wrap(~participantId)+
    theme_bw()+
    scale_color_manual(values=c("#1f77b4", "#ff7f0e")) + theme_apa()

p
svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/participants_charts_lmer_free_uw_students.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/participants_charts_lmer_free_uw_students.txt")

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

cat(aov.btw.res, file = "lmer_output/anova_bwt_res_summary_free_uw_students.html")
cat(aov.btw.res, file = "../../emotions_dashboard/data/anova_bwt_res_summary_free_uw_students.html")


# Since the p value < 0.05, we can say that the variance of the residuals is equal and 
# therefore the assumption of **homoscedasticity** NOT is met 

s <- svgstring(width = 7,
               height = 5)

Plot.Model.F <- plot(m2) #creates a fitted vs residual plot
Plot.Model.F
Plot.Model.F <- s()
cat(Plot.Model.F , file = "lmer_output/fitted_vs_residual_plot_free_uw_students.txt")
cat(Plot.Model.F , file = "../../emotions_dashboard/data/fitted_vs_residual_plot_free_uw_students.txt")
dev.off()

## This looks very unsystematic

resid1 <- hlm_resid(m2, level = 1, standardize = TRUE)

s <- svgstring(width = 7,
               height = 5)

ggplot(data = resid1, aes(x = participantId, y = .std.ls.resid)) + 
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(y = "Least-Squares level-1 residuals", 
       title = "Least-Squares residuals by participant ID") + theme_apa()

l1.res <- s()
cat(l1.res , file = "lmer_output/l1_res_plot_free_uw_students.txt")
cat(l1.res , file = "../../emotions_dashboard/data/l1_res_plot_free_uw_students.txt")
dev.off()

## There are quite a couple of large residuals 

resid2 = hlm_resid(m2, level = "participantId", standardize = TRUE, include.ls = FALSE)

s <- svgstring(width = 7,
               height = 5)

ggplot(data = resid2, aes(x = participantId, y = .std.ranef.intercept)) + 
  geom_point(alpha = 0.4) +
  geom_smooth(method = "loess", se = FALSE) + 
  labs(y = "Random effects - intercept", 
       title = "Intercept random effects against participant ID") + theme_apa()

l2.res.int <- s()
cat(l2.res.int , file = "lmer_output/l2_int_res_plot_free_uw_students.txt")
cat(l2.res.int , file = "../../emotions_dashboard/data/l2_int_res_plot_free_uw_students.txt")
dev.off()

# 43, 9, 16
# 
# ggplot(data = resid2, aes(x = participantId, y = .std.ranef.sex_c)) + 
#   geom_point(alpha = 0.4) +
#   geom_smooth(method = "loess", se = FALSE) + 
#   labs(y = "Level-2 residuals", 
#        title = "L2 residuals by participant ID")

# 9, 16
# 
#     ggplot(data = resid2, aes(x = participantId, y = .std.ranef.ethnicity_c)) + 
#   geom_point(alpha = 0.4) +
#   geom_smooth(method = "loess", se = FALSE) + 
#   labs(y = "Level-2 residuals", 
#        title = "L2 residuals by participant ID")

# 43
# 
# ggplot(data = resid2, aes(x = participantId, y = .std.ranef.sex_c_ethnicity_c)) + 
#   geom_point(alpha = 0.4) +
#   geom_smooth(method = "loess", se = FALSE) + 
#   labs(y = "Level-2 residuals", 
#        title = "L2 residuals by participant ID")

# 43, 9


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
cat(svg.qqplot, file = "lmer_output/qqplot_lmer_free_uw_students.txt")
cat(svg.qqplot, file = "../../emotions_dashboard/data/qqplot_lmer_free_uw_students.txt")
dev.off()


# looks not normal...
# https://ademos.people.uic.edu/Chapter18.html

# resid_panel(m2)


#####################################
#####################################

# Check influence

#####################################
#####################################

invisible(utils::memory.limit(64000))


infl <- hlm_influence(m2, level = 1)

# IQR = as.numeric(format(IQR(infl$cooksd)*3, scientific = F))
CutOff = 4/nrow(infl)
print(CutOff)

s <- svgstring(width = 7,
               height = 5)

# dotplot_diag(infl$cooksd, name = "cooks.distance", cutoff = "internal")
dotplot_diag(infl$cooksd, name = "cooks.distance", cutoff = CutOff) + theme_apa()
svg.string.plot <- s()
cat(svg.string.plot, file = "lmer_output/influence_datapoints_lmer_free_uw_students.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/influence_datapoints_lmer_free_uw_students.txt")
dev.off()


high_cooksd = infl[infl$cooksd > CutOff, ] %>%
  arrange(desc(cooksd))

head(high_cooksd, n=10)

#### high influence data points

high_cooksd$id

### 12300  5180 14007  1623 11980 12446 14001 13997

infl.classes <- hlm_influence(m2, level = "participantId")

CutOffGroup = 4/49
CutOffGroup

s <- svgstring(width = 7,
               height = 5)

dotplot_diag(infl.classes$cooksd, name = "cooks.distance", cutoff = CutOffGroup, modify = "dotplot") + theme_apa()
svg.string.plot <- s()

cat(svg.string.plot, file = "lmer_output/influence_participants_lmer_free_uw_students.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/influence_participants_lmer_free_uw_students.txt")
dev.off()


high_cooksd_participants = infl.classes[infl.classes$cooksd > CutOffGroup, ] %>%
  arrange(desc(cooksd))

high_cooksd_participants

# participant 43

#####################################
#####################################

# Check leverage

#####################################
#####################################

CutOffLeverage = mean(infl$leverage.overall)*3
CutOffLeverage

s <- svgstring(width = 7,
               height = 5)

dotplot_diag(infl$leverage.overall, name = "leverage", cutoff = CutOffLeverage) + theme_apa()

svg.string.plot <- s()
cat(svg.string.plot, file = "lmer_output/leverage_datapoints_lmer_free_uw_students.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/leverage_datapoints_lmer_free_uw_students.txt")
dev.off()

high_leverage = infl[infl$leverage.overall > CutOffLeverage, ] %>%
  arrange(desc(leverage.overall))

# head(high_leverage, n=10)
high_leverage

# high leverage data points
high_leverage$id

CutOffLeverageParticipants = mean(infl.classes$leverage.overall)*3
CutOffLeverageParticipants


s <- svgstring(width = 7,
               height = 5)

dotplot_diag(infl.classes$leverage.overall, name = "leverage", cutoff = CutOffLeverageParticipants) + theme_apa()

svg.string.plot <- s()
cat(svg.string.plot, file = "lmer_output/leverage_participants_lmer_free_uw_students.txt")
cat(svg.string.plot, file = "../../emotions_dashboard/data/leverage_participants_lmer_free_uw_students.txt")
dev.off()


high_leverage_participants = infl.classes[infl.classes$leverage.overall > CutOffLeverageParticipants, ] %>%
  arrange(desc(leverage.overall))

# head(high_leverage, n=10)
high_leverage_participants

## No high leverage participants

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


#####################################
#####################################

# Refitted LMER 
# - Excluding outliers; high influence; high leverage observations and participants 
# - Heavylmer: Lmer for heavy-tailed residuals

#####################################
#####################################

### traditional lmer approach ####


## without derivatives check
control <- lmerControl(optimizer ="Nelder_Mead", calc.derivs=FALSE, optCtrl=list(maxfun=2e6), check.nobs.vs.nRE = "ignore")

m3 <-lmer(
    sentimentScore ~ 1 + sexC*ethnicityC + (1 + sexC*ethnicityC|participantId), 
    data = df.filtered,
    control=control)

summary(m3)


### get coefficient table for reporting
tab_model(m3, file = "lmer_output/lmer_refit_summary_free_uw_students.html")
tab_model(m3, file = "../../emotions_dashboard/data/lmer_refit_summary_free_uw_students.html")

## Type III anova table with p-values for F-tests based on Satterthwaite's
## method:
(aov.m3 <- anova(m3))

aov.apa.m3 <- kable(aov.m3, digits = 3, format = "html", caption = "ANOVA table for refitted LMER coefficients")
cat(aov.apa.m3, file = "lmer_output/anova_lmer_refit_summary_free_uw_students.html")
cat(aov.apa.m3, file = "../../emotions_dashboard/data/anova_lmer_refit_summary_free_uw_students.html")



#####################################
#####################################

# significant effects charts

#####################################
#####################################

####################
### Helper functions

summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}



normDataWithin <- function(data=NULL, idvar, measurevar, betweenvars=NULL,
                           na.rm=FALSE, .drop=TRUE) {
  library(plyr)
  
  # Measure var on left, idvar + between vars on right of formula.
  data.subjMean <- ddply(data, c(idvar, betweenvars), .drop=.drop,
                         .fun = function(xx, col, na.rm) {
                           c(subjMean = mean(xx[,col], na.rm=na.rm))
                         },
                         measurevar,
                         na.rm
  )
  
  # Put the subject means with original data
  data <- merge(data, data.subjMean)
  
  # Get the normalized data in a new column
  measureNormedVar <- paste(measurevar, "_norm", sep="")
  data[,measureNormedVar] <- data[,measurevar] - data[,"subjMean"] +
    mean(data[,measurevar], na.rm=na.rm)
  
  # Remove this subject mean column
  data$subjMean <- NULL
  
  return(data)
}

summarySEwithin <- function(data=NULL, measurevar, betweenvars=NULL, withinvars=NULL,
                            idvar=NULL, na.rm=FALSE, conf.interval=.95, .drop=TRUE) {
  
  # Ensure that the betweenvars and withinvars are factors
  factorvars <- vapply(data[, c(betweenvars, withinvars), drop=FALSE],
                       FUN=is.factor, FUN.VALUE=logical(1))
  
  if (!all(factorvars)) {
    nonfactorvars <- names(factorvars)[!factorvars]
    message("Automatically converting the following non-factors to factors: ",
            paste(nonfactorvars, collapse = ", "))
    data[nonfactorvars] <- lapply(data[nonfactorvars], factor)
  }
  
  # Get the means from the un-normed data
  datac <- summarySE(data, measurevar, groupvars=c(betweenvars, withinvars),
                     na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Drop all the unused columns (these will be calculated with normed data)
  datac$sd <- NULL
  datac$se <- NULL
  datac$ci <- NULL
  
  # Norm each subject's data
  ndata <- normDataWithin(data, idvar, measurevar, betweenvars, na.rm, .drop=.drop)
  
  # This is the name of the new column
  measurevar_n <- paste(measurevar, "_norm", sep="")
  
  # Collapse the normed data - now we can treat between and within vars the same
  ndatac <- summarySE(ndata, measurevar_n, groupvars=c(betweenvars, withinvars),
                      na.rm=na.rm, conf.interval=conf.interval, .drop=.drop)
  
  # Apply correction from Morey (2008) to the standard error and confidence interval
  #  Get the product of the number of conditions of within-S variables
  nWithinGroups    <- prod(vapply(ndatac[,withinvars, drop=FALSE], FUN=nlevels,
                                  FUN.VALUE=numeric(1)))
  correctionFactor <- sqrt( nWithinGroups / (nWithinGroups-1) )
  
  # Apply the correction factor
  ndatac$sd <- ndatac$sd * correctionFactor
  ndatac$se <- ndatac$se * correctionFactor
  ndatac$ci <- ndatac$ci * correctionFactor
  
  # Combine the un-normed means with the normed results
  merge(datac, ndatac)
}


############################
#### tables for plotting ###

# summarySE provides the standard deviation, standard error of the mean, and a (default 95%) confidence interval
sex.sum.table <- summarySEwithin(df.filtered, measurevar="sentimentScore", withinvars=c("sex"),  idvar="participantId")
sex.sum.table
  
ethnicity.sum.table <- summarySEwithin(df.filtered, measurevar="sentimentScore", withinvars=c("ethnicity"), idvar="participantId")
ethnicity.sum.table

sex.ethnicity.sum.table <- summarySEwithin(df.filtered, measurevar="sentimentScore", withinvars=c("sex","ethnicity"), idvar="participantId")
sex.ethnicity.sum.table

############################
### Effect of sex ####

# Mean + Standard error of the mean
ggplot(sex.sum.table, aes(y=sentimentScore, x=sex, colour=sex)) + 
  geom_errorbar(aes(ymin=sentimentScore-se, ymax=sentimentScore+se), width=.1) +
  geom_point() + 
  labs (title= "Mean and SEM sentiment score by sex") + theme_apa()

s <- svgstring(width = 7,
               height = 5)

ggplot(sex.sum.table, aes(y=sentimentScore, x=sex, colour=sex)) + 
  geom_errorbar(aes(ymin=sentimentScore-se, ymax=sentimentScore+se), width=.1) +
  geom_point() + 
  labs (title= "Mean and SEM sentiment score by sex") + theme_apa()

chart <- s()
cat(chart , file = "lmer_output/sex_effect_free.txt")
cat(chart , file = "../../emotions_dashboard/data/sex_effect_free.txt")
dev.off()

############################
### Effect of ethnicity ####

# Mean + Standard error of the mean
ggplot(ethnicity.sum.table, aes(y=sentimentScore, x=ethnicity, colour=ethnicity)) + 
  geom_errorbar(aes(ymin=sentimentScore-se, ymax=sentimentScore+se), width=.1) +
  geom_point() + 
  labs (title= "Mean and SEM sentiment score by ethnicity") + theme_apa()

s <- svgstring(width = 7,
               height = 5)

ggplot(ethnicity.sum.table, aes(y=sentimentScore, x=ethnicity, colour=ethnicity)) + 
  geom_errorbar(aes(ymin=sentimentScore-se, ymax=sentimentScore+se), width=.1) +
  geom_point() + 
  labs (title= "Mean and SEM sentiment score by ethnicity") + theme_apa()

chart <- s()
cat(chart , file = "lmer_output/ethnicity_effect_free.txt")
cat(chart , file = "../../emotions_dashboard/data/ethnicity_effect_free.txt")
dev.off()


###############################
### Sex * Ethnicity effect ####

# Mean + std error of the mean 
ggplot(sex.ethnicity.sum.table, aes(x=ethnicity, y=sentimentScore, fill=sex, color=sex)) +
  geom_errorbar(aes(ymin=sentimentScore-se, ymax=sentimentScore+se), width=.1) +
  geom_point() + 
  labs (title= "Mean and SEM sentiment score by sex and ethnicity ") + theme_apa()


s <- svgstring(width = 7,
               height = 5)

ggplot(sex.ethnicity.sum.table, aes(x=ethnicity, y=sentimentScore, fill=sex, color=sex)) +
  geom_errorbar(aes(ymin=sentimentScore-se, ymax=sentimentScore+se), width=.1) +
  geom_point() + 
  labs (title= "Mean and SEM sentiment score by sex and ethnicity ") + theme_apa()

chart <- s()
cat(chart , file = "lmer_output/sex_et_effect_free.txt")
cat(chart , file = "../../emotions_dashboard/data/sex_et_effect_free.txt")
dev.off()

