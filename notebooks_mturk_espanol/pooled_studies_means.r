library(tidyverse)
library(svglite)
library(ggforce)
library(papaja)
library(rstatix)

df = read_csv("../clean_data/pooled_means_categorization_emotions.csv")
df

correct.plot <- ggplot(df, aes(x =method  , y=mean.correct, fill=study)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  labs(x = "survey method",
       title = "Correct responses") + 
  theme_apa()

ggsave('accuracy-charts/pooled-studies-correct-method.png',  width = 5, height = 4)


correct.plot


correct.plot.facet <- ggplot(df, aes(x =method  , y=mean.correct, fill=study)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  labs(x = "survey method",
       title = "Correct responses") + 
  facet_wrap(~label) +
  theme_apa()


correct.plot.facet

ggsave('accuracy-charts/pooled-studies-correct-method-faceted.png',  width = 8, height = 6)

