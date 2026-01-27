source("5-Functions.r")

library(lme4)
library(lmerTest)
library(tidyverse)
library(dplyr)
library(here)
library(readxl)
library(readr)
library(here)

#Run "1-Data_plots.r" first to obtain "individual_ratings_all.csv"

df <- read.csv(file="individual_ratings_all.csv")
df$Model <- factor(df$Model, levels = c("GPT35", "GPT4", "Llama"))
df$ID <- factor(df$ID)
df$Rater <- factor(df$Rater)

#fit LMM with randon task and rater effects.
model <- lmer(Score ~ Model + Criterion + (1 | ID) + (1 | Rater), data = df)

#Table 4 results
#estimated variance
summary(model)
#bootstrap results
results <- bootstrap_LMM_task_rater(model, df, B = 1000)
results