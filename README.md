# Emotion categorization from images of faces 

***Last update***: 3/10/2022

Repository containing code and materials for my Ph.D. dissertation. 

Go to [this link](https://share.streamlit.io/pabloinsente/emotions_dashboard/main/emotions_dashboard_2.py) to see a Dashboard with all my data and analysis. It takes 10-15 seconds to run.

## Summary 

My dissertation investigates three questions at the intersection of cognitive science and the psychology of emotion:

(1) how humans categorize and conceptualize facial expressions of emotion  
(2) whether different methods for asking people to categorize facial expressions alters such behavior  
(3) whether perceived social characteristics of faces (like race and gender) impacts humans categorization behavior   

## Populations under study

As this research project was done during the COVID-19 pandemic, all the data collection was completed on-line. Data was collected from three groups:

(1) Undergraduates from a large public university in the United States  
(2) English-speaking adults workers from the United States in Amazon MTurk  
(3) Spanish-speaking adults workers from South America in Amazon MTurk (ongoing...)

## Directories

**affectnet-split-images/**: images depicting one of the six so-called basic emotions (Happiness, Fear, Anger, Disgust(aversion), Surprise, Sadness), plus "neutral" faces, and faces with uncategorized emotions. Images are split by gender (Male/Female), and age (Adult/Minor). The images come from the [AffectNet](http://mohammadmahoor.com/affectnet/) dataset. 

**data**, **data_mturk**, **data_mturk_espanol**: data was obtained via an online survey asking participants to either (1) select one of 7 predetermined response choices ("forced-choice condition/survey");  or (2) freely naming images with up to three distinct words ("free-choice condition/survey")

**notebooks**, **notebooks_mturk**, **notebooks_mturk_espanol**: 
    - Exploratory data analysis
    - Clustering and dimensionality reducction analysis
    - Sentiment analysis
    - Linear Mixed-Models from sentiment analysis
    - Ranking (borda-score) comparison
