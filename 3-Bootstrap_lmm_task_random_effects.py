#!/usr/bin/env python
# coding: utf-8

#This Python script generate the results in Tables 1, 2, and 3 of the paper. 

import os
import math
import warnings
import textwrap
from pathlib import Path
from functools import reduce
import numpy as np
import pandas as pd
import scipy.stats as stats
from scipy.stats import t, boxcox, bootstrap
import statsmodels.api as sm
import statsmodels.formula.api as smf
import statsmodels.stats.api as sms
from statsmodels.stats.multitest import multipletests
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score
import matplotlib.pyplot as plt
import seaborn as sns
from IPython.display import display
from fpdf import FPDF
import bambi as bmb
warnings.filterwarnings("ignore")



if "__file__" in globals():
    BASE_DIR = Path(__file__).resolve().parent.parent
else:
    BASE_DIR = Path.cwd().parent
DATA_DIR = BASE_DIR/"Perf-Eval-LLM-SAS"



# provide llama model version
def rename_model(df):
    df['Model'] = df['Model'].replace('Llama', 'Llama-3.1 70B')
    return df

# Calculate average score per task per LLm
def calculate_average_scores(df, dimension):
    return df.groupby(['ID', 'Model'])[dimension].mean().reset_index().rename(columns={dimension: 'Average_Score'})

# Merge group1, group2 and group 2 rating scores on ['ID', 'Model']

def merge_by_one_column(dfs, aspect_names, dim_names):
    len_df = len(aspect_names)
    key_columns = ['ID', 'Model']
    renamed_dfs = []
    for i, df in enumerate(dfs):
        aspect_name = aspect_names[i]  
        score_columns = [col for col in df.columns if col not in key_columns]  
        rename_mapping = {col: f"{dim_names[i]}" for col in score_columns}
        renamed_df = df.rename(columns=rename_mapping)
        renamed_dfs.append(renamed_df)
    df_combined = reduce(lambda left, right: pd.merge(left, right, on=key_columns, how='left'), renamed_dfs)
    return df_combined

# Long-format df with one row per ID–Model–Criterion combination,
def data_long(df, aspect_name):
    data_long = df.melt(id_vars = ["ID", "Model"], 
                      value_vars = aspect_name, 
                      var_name = "Aspect", value_name = "Average_Score")
    return data_long

# Row-wise average scores across selected columns
def compute_average_with_columns(df, selected_cols, keep_cols, new_col):
    if isinstance(keep_cols, str):  
        keep_cols = [keep_cols]  
    valid_selected_cols = [col for col in selected_cols if col in df.columns]
    missing_selected = set(selected_cols) - set(valid_selected_cols)
    valid_keep_cols = [col for col in keep_cols if col in df.columns]
    missing_keep = set(keep_cols) - set(valid_keep_cols)
    new_df = df[valid_keep_cols].copy()  
    new_df[new_col] = df[valid_selected_cols].mean(axis=1)
    return new_df

def bootstrap_pvalue(samples):
    p_right = np.mean(samples >= 0)
    p_left  = np.mean(samples <= 0)
    return 2 * min(p_right, p_left)

def ci(x):
    return np.percentile(x, [2.5, 97.5])

# Bootstrap algorithms 
def bootstrap_LMM(m_ri, df, B = 1000, seed= 12345):
    np.random.seed(seed)
    epa = m_ri.random_effects
    epa_flat = np.array([float(v.iloc[0]) for v in epa.values()])
    MS_re = np.mean(epa_flat ** 2)
    sigma_a2 = float(m_ri.cov_re.iloc[0, 0]) 
    # Adjusted Random Effects
    adjust_ai = math.sqrt(sigma_a2 / MS_re) * epa_flat
    # Adjusted Residuals
    eps = m_ri.resid
    MS_eps = np.sum(eps ** 2) / len(df)
    sigma_e2 = m_ri.scale 
    adjust_epsilon = math.sqrt(sigma_e2 / MS_eps) * eps
    eps_groups = np.asarray(adjust_epsilon).reshape(207, 3)

    beta0 = m_ri.fe_params["Intercept"]
    beta1 = m_ri.fe_params.get("Model[T.GPT4]", 0.0)
    beta2 = m_ri.fe_params.get("Model[T.Llama-3.1 70B]", 0.0)

    df_modified = (
        df.sort_values("ID")
        .reset_index(drop=True)
        .copy()
    )
    df_modified["z1"] = (df_modified["Model"] == "GPT4").astype(float)
    df_modified["z2"] = (df_modified["Model"] == "Llama-3.1 70B").astype(float)
    contrasts = {
        "GPT4_vs_GPT35":   np.array([0.0, 1.0, 0.0]),
        "llama_vs_GPT35":  np.array([0.0, 0.0, 1.0]),
        "llama_vs_GPT4":   np.array([0.0, -1.0, 1.0]),
    }
    contrast_GPT4_vs_GPT35 = []
    contrast_llama_vs_GPT35 = []
    contrast_llama_vs_GPT4 = []
    boot_beta0, boot_beta1, boot_beta2 = [], [], []
    boot_var_u, boot_var_eps = [], []
    for _ in range(B):
        a_hat = np.random.choice(adjust_ai, size=207, replace=True)
        eps_hat = np.array(
            [np.random.choice(eps_groups[i], size=3, replace=True)
             for i in range(207)]
        )
        df_modified["a_hat"] = df_modified["ID"].map(
            {i: a_hat[i-1] for i in range(1,208)}
        )
        df_modified["eps_hat"] = eps_hat.reshape(-1)

        df_modified["x_M"] = (
            beta0
            + df_modified["a_hat"]
            + beta1 * df_modified["z1"]
            + beta2 * df_modified["z2"]
            + df_modified["eps_hat"]
        )

        m_sim = smf.mixedlm(
            "x_M ~ Model",
            data = df_modified,
            groups = "ID",
            re_formula = "1",
        ).fit(reml=True, method="lbfgs", disp=False)

        boot_beta0.append(m_sim.fe_params["Intercept"])
        boot_beta1.append(m_sim.fe_params.get("Model[T.GPT4]", 0.0))
        boot_beta2.append(m_sim.fe_params.get("Model[T.Llama-3.1 70B]", 0.0))

        boot_var_u.append(float(m_sim.cov_re.iloc[0, 0]))
        boot_var_eps.append(float(m_sim.scale))

        params = m_sim.fe_params.values.tolist()

        contrast_GPT4_vs_GPT35.append(
            np.dot(contrasts["GPT4_vs_GPT35"], params)
        )
        contrast_llama_vs_GPT35.append(
            np.dot(contrasts["llama_vs_GPT35"], params)
        )
        contrast_llama_vs_GPT4.append(
            np.dot(contrasts["llama_vs_GPT4"], params)
        )

    contrast_GPT4_vs_GPT35 = np.asarray(contrast_GPT4_vs_GPT35)
    contrast_llama_vs_GPT35 = np.asarray(contrast_llama_vs_GPT35)
    contrast_llama_vs_GPT4  = np.asarray(contrast_llama_vs_GPT4)
    # Bootstrap Contrast Estimates
    mean_GPT4_vs_GPT35 = np.mean(contrast_GPT4_vs_GPT35)
    mean_llama_vs_GPT35 = np.mean(contrast_llama_vs_GPT35)
    mean_llama_vs_GPT4  = np.mean(contrast_llama_vs_GPT4)
    
    boot_beta0 = np.asarray(boot_beta0)
    boot_beta1 = np.asarray(boot_beta1)
    boot_beta2 = np.asarray(boot_beta2)
    boot_var_u = np.asarray(boot_var_u)
    boot_var_eps = np.asarray(boot_var_eps)

    return {
        "ci_GPT4_vs_GPT35": ci(contrast_GPT4_vs_GPT35),
        "ci_llama_vs_GPT35": ci(contrast_llama_vs_GPT35),
        "ci_llama_vs_GPT4": ci(contrast_llama_vs_GPT4),
        "p_GPT4_vs_GPT35": bootstrap_pvalue(contrast_GPT4_vs_GPT35),
        "p_llama_vs_GPT35": bootstrap_pvalue(contrast_llama_vs_GPT35),
        "p_llama_vs_GPT4": bootstrap_pvalue(contrast_llama_vs_GPT4),
        "mean_GPT4_vs_GPT35": mean_GPT4_vs_GPT35,
        "mean_llama_vs_GPT35": mean_llama_vs_GPT35,
        "mean_llama_vs_GPT4": mean_llama_vs_GPT4
    }

# Summarize results, contrast estimation, 95% confidence interval, p-value and adjusted p-value
def compute_results_table(results_dict):
    pval_keys = [k for k in results_dict.keys() if k.startswith("p_")]
    pvals = [results_dict[k] for k in pval_keys]
    pvals_adj = multipletests(pvals, method="fdr_bh")[1]
    result_final_score = results_dict.copy()
    for key, adj_val in zip(pval_keys, pvals_adj):
        result_final_score[key + "_adj"] = adj_val
    rows = []
    rows.append({
        "contrast": "GPT4_vs_GPT35",
        "ci_low": result_final_score["ci_GPT4_vs_GPT35"][0],
        "ci_high": result_final_score["ci_GPT4_vs_GPT35"][1],
        "p_value": result_final_score["p_GPT4_vs_GPT35"],
        "p_value_adj": result_final_score["p_GPT4_vs_GPT35_adj"]
    })
    rows.append({
        "contrast": "llama_vs_GPT35",
        "ci_low": result_final_score["ci_llama_vs_GPT35"][0],
        "ci_high": result_final_score["ci_llama_vs_GPT35"][1],
        "p_value": result_final_score["p_llama_vs_GPT35"],
        "p_value_adj": result_final_score["p_llama_vs_GPT35_adj"]
    })
    rows.append({
        "contrast": "llama_vs_GPT4",
        "ci_low": result_final_score["ci_llama_vs_GPT4"][0],
        "ci_high": result_final_score["ci_llama_vs_GPT4"][1],
        "p_value": result_final_score["p_llama_vs_GPT4"],
        "p_value_adj": result_final_score["p_llama_vs_GPT4_adj"]
    })
    df_table = pd.DataFrame(rows)
    return result_final_score, df_table

def fit_mixed_model(response_col, df, group_col):
    formula = f"{response_col} ~ Model"
    model = smf.mixedlm(
        formula,
        data=df,
        groups=df[group_col], # Grouping variable for random effects
        re_formula="1" # Random intercept only
    )
    return model.fit(reml=True, method="lbfgs", disp=False)


# In[451]:


data_g1 = pd.read_csv(DATA_DIR/"group1_rating_blinded.csv")
data_g2 = pd.read_csv(DATA_DIR /"group2_rating_blinded.csv")
data_g3 = pd.read_csv(DATA_DIR /"group3_rating_blinded.csv")



# This block preprocesses evaluation data from three scoring groups (Group1, Group2, Group3)
# For each group, it:
# (1) standardizes model names,
# (2) computes average scores across predefined sub-dimensions,
# (3) merges these averages into wide-format DataFrames,
# (4) reshapes them into long format for downstream analysis if needed.
# Finally, it merges all group-level scores by (ID, Model),
# converts scores to numeric, and computes a final aggregated score
# by summing across all evaluation criterias


data_g1_new = rename_model(df = data_g1)
data_g2_new = rename_model(df = data_g2)
data_g3_new = rename_model(df = data_g3)
dimensions = ['C1_1', 'C1_2', 'C1_3', 'C1_4', 'C1_5']
df_list_g1 = [calculate_average_scores(data_g1, dim) for dim in dimensions]
dim_names_g1 = ['Data step/Model proc and model structure', 'Correctness of dataset and variable', 'Model and output options',
               'Code Readability and structure', 'Code conciseness']
df_list_g1_new = [calculate_average_scores(data_g1_new, dim) for dim in dimensions]
df_c1 = merge_by_one_column(dfs = df_list_g1_new, aspect_names = 'Average_Score', dim_names = ['Data step/Model proc and model structure', 'Correctness of dataset and variable', 'Model and output options',
               'Code Readability and structure', 'Code conciseness'])
data_long_c1 = data_long(df = df_c1, aspect_name = ['Data step/Model proc and model structure', 'Correctness of dataset and variable', 'Model and output options',
               'Code Readability and structure', 'Code conciseness'])

dimensions = ['C2_1', 'C2_2']
df_list_g2_new = [calculate_average_scores(data_g2_new, dim) for dim in dimensions]
df_c2 = merge_by_one_column(dfs = df_list_g2_new, aspect_names = 'Average_Score', dim_names = ['Error due to variable and dataset name', 'Other error and warning message'])
data_long_c2 = data_long(df = df_c2, aspect_name = ['Error due to variable and dataset name', 'Other error and warning message'])
df_c2
dimensions = ['C3_1', 'C3_2', 'C3_3']
df_list_g3_new = [calculate_average_scores(data_g3_new, dim) for dim in dimensions]
df_c3 = merge_by_one_column(dfs = df_list_g3_new, aspect_names = 'Average_Score', dim_names = ['Correctness', 'Relevance and conciseness', 'Redundancy'])
data_long_c3 = data_long(df = df_c3, aspect_name = ['Correctness', 'Relevance and conciseness', 'Redundancy'])
common_columns = ["ID", "Model"]
merged_df = df_c1.merge(df_c2, on=common_columns, how="outer").merge(df_c3, on=common_columns, how="outer")
score_columns = [col for col in merged_df.columns if col not in common_columns]
merged_df[score_columns] = merged_df[score_columns].apply(pd.to_numeric, errors='coerce')
merged_df["final_score"] = merged_df[score_columns].sum(axis=1)
merged_df = merged_df.reset_index(drop=True)
df = merged_df



# Rescaled final scores from total 50 to 5
df['scaled_final_score'] = df['final_score'] / 10



m_ri = smf.mixedlm(
    "scaled_final_score ~ Model",
    data=df,
    groups="ID",
    re_formula="1"
).fit(reml=True, method="lbfgs", disp=False)
m_ri.summary()



epa = m_ri.random_effects
epa_flat = np.array([float(v.iloc[0]) for v in epa.values()])
epa_flat
MS_re = np.mean(epa_flat ** 2)
print(MS_re)
sigma_a2 = float(m_ri.cov_re.iloc[0, 0]) 
print(sigma_a2)


#Results of Table 1 
results = bootstrap_LMM(m_ri, df)
print(results)
result_final_score, df_result_table = compute_results_table(results_dict = results)
df_result_table


# df of average scores for each group
g1_average = compute_average_with_columns(df = df, 
                                          selected_cols = ['Data step/Model proc and model structure',
       'Correctness of dataset and variable', 'Model and output options',
       'Code Readability and structure', 'Code conciseness'], 
                                          keep_cols = ['Model', 'ID'], new_col = 'Code Quality Average') 
g1_average = g1_average.rename(columns={"Code Quality Average": "codequalityscore"})

g2_average = compute_average_with_columns(df = df, 
                                          selected_cols = ['Error due to variable and dataset name',
      'Other error and warning message'], 
                                          keep_cols = ['Model', 'ID'], new_col = 'Code Executability Average') 
g2_average = g2_average.rename(columns={"Code Executability Average": "codeexecutabilityaverage"})

g3_average = compute_average_with_columns(df = df, 
                                          selected_cols = ['Correctness',
       'Relevance and conciseness', 'Redundancy'], 
                                          keep_cols = ['Model', 'ID'], new_col = 'Code Output Average') 
g3_average = g3_average.rename(columns={"Code Output Average": "codeoutputaverage"})



# Table 2 Parameter estimates, bootstrap 95% confidence intervals, and p-values for model contrasts based on group-level scores

#group 1
g1_codequalityscore = fit_mixed_model("codequalityscore", df = g1_average, group_col =  "ID")
g1_codequalityscore_result = bootstrap_LMM(m_ri = g1_codequalityscore, df = g1_average, B = 1000, seed= 12345)
print(g1_codequalityscore_result)
result_g1_codequalityscore, df_g1_codequalityscore_table= compute_results_table(results_dict = g1_codequalityscore_result)
df_g1_codequalityscore_table


#group 2
g2_codeexecutability = fit_mixed_model("codeexecutabilityaverage", df = g2_average, group_col =  "ID")
g2_codeexecutability_result = bootstrap_LMM(m_ri = g2_codeexecutability, df = g2_average, B = 1000, seed= 12345)
print(g2_codeexecutability_result)
result_g2_codeexecutability, df_g2_codeexecutability_table= compute_results_table(results_dict = g2_codeexecutability_result)
df_g2_codeexecutability_table



#group 3
g3_codeoutputaverage = fit_mixed_model("codeoutputaverage", df = g3_average, group_col =  "ID")
g3_codeoutputscore_result = bootstrap_LMM(m_ri = g3_codeoutputaverage, df = g3_average, B = 1000, seed= 12345)
print(g3_codeoutputscore_result)
result_g3_codeoutputscore, df_g3_codeoutputscore_table= compute_results_table(results_dict = g3_codeoutputscore_result)
df_g3_codeoutputscore_table

# Table 3 Parameter estimates, bootstrap 95% confidence intervals, and p-values for model contrasts based on individual criterion scores

individual_criteria_df = df[[
    'Model',
    'Data step/Model proc and model structure',
    'Correctness of dataset and variable',
    'Model and output options',
    'Code Readability and structure',
    'Code conciseness',
    'Error due to variable and dataset name',
    'Other error and warning message',
    'Correctness',
    'Relevance and conciseness',
    'Redundancy',
    'ID'
]].copy()

individual_criteria_df.columns = (
    individual_criteria_df.columns
    .str.replace(' ', '_', regex=False)   
    .str.replace('/', '_', regex=False)   
)
individual_criteria = ['Data_step_Model_proc_and_model_structure',
       'Correctness_of_dataset_and_variable', 'Model_and_output_options',
       'Code_Readability_and_structure', 'Code_conciseness',
       'Error_due_to_variable_and_dataset_name',
       'Other_error_and_warning_message', 'Correctness',
       'Relevance_and_conciseness', 'Redundancy'] 

for i in individual_criteria: 
    model_df = fit_mixed_model(i, df = individual_criteria_df, group_col =  "ID")
    bmm_result = bootstrap_LMM(m_ri = model_df, df = individual_criteria_df, B = 1000, seed= 12345)
    print(bmm_result)
    result_bmm, df_result_bmm_table = compute_results_table(results_dict = bmm_result)
    print(i)
    display(df_result_bmm_table)
    



