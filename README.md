# Data Analysis for LLM-generated SAS Code Rating

This repository contains R and Python code used for the data analysis in the paper "Performance Evaluation of Large Language Models in Statistical Programming with SAS" by Xinyi Song, Kexin Xie, Lina Lee, Ruizhe Chen, Jared Clark, Hao He, Haoran He, Jie Min, Xinlei Zhang, Simin Zheng, Zhiyang Zhang, Xinwei Deng, and Yili Hong.

This repository also provides the original individual-level rating scores. The StatLLM dataset is available in a separate repository: https://github.com/yili-hong/StatLLM.

This repository includes a README.md, data files and code files.  

## Data

The original individual-level rating scores are stored in three .csv files: group1_rating.csv, group2_rating.csv, and group3_rating.csv. The meaning of the columns are:
  
  ID: the task ID.
  Model: the LLM label.
  Rater: the rater ID.

The labels C1_1, C1_2, C1_3, C1_4, C1_5, C2_1, C2_2, C3_1, C3_2, and C3_3 correspond to criteria Q1 to Q10, respectively.



## Code 

There are five files in total for the R and Python code.

1. 1-Data_plots.r: R script for data processing and generating all figures in the paper, except for Figures 5 and 6.

2. 2-Radar_plot.py: Python script to generate the radar plots used in Figures 5 and 6 to visualize and compare model performance across multiple evaluation criteria.

3. 3-Bootstrap_lmm_task_random_effects.py: Python script for the bootstrap procedure for linear mixed-effects models with task-level random effects. Generates the results in Tables 1, 2, and 3.

4. 4-Bootstrap_lmm_task_random_and_rater_effects.r: R script for the bootstrap procedure for linear mixed-effects models with both task-level and rater-level random effects. Generates the results in Table 4.

5. 5-Functions.r: Contains R functions used in the other R scripts.

 
  
 
