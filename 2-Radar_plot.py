#!/usr/bin/env python
# coding: utf-8


#This Python script generate Figures 5 and 6 of the paper. 
#Run 1-Data_plots.r to obtain "ind_mean.csv" and "ind_sd.csv". 

import pandas as pd
import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import seaborn as sns
from fpdf import FPDF
# Read CSV file into a DataFrame


df = pd.read_csv("ind_mean.csv")   
# Display the first few rows
df.set_index("Model", inplace=True)
print(df.head())
print(df.shape)


print('matplotlib: {}'.format(matplotlib.__version__))


df1 = pd.read_csv("ind_sd.csv")  
# Display the first few rows
df1.set_index("Model", inplace=True)
print(df1.head())
print(df1.shape)



df.columns.tolist()


def radar_plot(df_pd, save_path):
    categories = df_pd.columns.tolist()
    num_vars = len(categories)
    angles = np.linspace(0, 2 * np.pi, num_vars, endpoint=False)
    angles = np.concatenate((angles, [angles[0]]))  # Complete the circle
    marker_styles = ['o', 's', 'D', '^', 'P']
    fig, ax = plt.subplots(figsize=(7, 7), subplot_kw=dict(polar=True))
    ax.set_theta_offset(np.pi / 2)
    ax.set_theta_direction(-1)
    colors = sns.color_palette('Set2', n_colors=len(df_pd))
    for i, (model_name, row) in enumerate(df_pd.iterrows()):
        values = row.values
        values = np.concatenate((values, [values[0]]))  
        ax.plot(angles, values, color=colors[i], linewidth=2, 
                marker=marker_styles[i % len(marker_styles)], markersize=6)
        ax.fill(angles, values, color=colors[i], alpha=0.2)
    
    ax.set_xticks(angles[:-1])
    ax.set_xticklabels(categories, fontsize=16, fontname='Arial')
    
    for label,i in zip(ax.get_xticklabels(),range(0,len(angles))):
        angle_rad=angles[i]
        
        if angle_rad==0.0:
            ha='center'
            va="bottom"
            angle_text=90
        elif 0.0<angle_rad <= np.pi/2:
            ha= 'left'
            va= "bottom"
            angle_text=angle_rad*(-180/np.pi)+90
        elif np.pi/2 < angle_rad < np.pi:
            ha= 'left'
            va= "top"
            angle_text=angle_rad*(-180/np.pi)+90
        elif angle_rad == np.pi:
            ha= 'center'
            va= "top"
            angle_text=90            
        elif np.pi < angle_rad <= (3*np.pi/2):
            ha= 'right'
            va= "top"  
            angle_text=angle_rad*(-180/np.pi)-90
        else:
            ha= 'right'
            va= "bottom"
            angle_text=angle_rad*(-180/np.pi)-90
        label.set_fontname('Arial')
        label.set_verticalalignment(va)
        label.set_horizontalalignment(ha)
        label.set_rotation(angle_text)    
    
    max_val = df_pd.max().max()
    ax.set_ylim(0, max_val * 1.2)  
    handles = [
        plt.scatter([], [], marker=marker_styles[i % len(marker_styles)], 
                    color=colors[i], s=80, label=model_name)
        for i, model_name in enumerate(df_pd.index)
    ]
    plt.legend(handles=handles, loc='upper center', bbox_to_anchor=(0.5, 1.2), ncol=3, 
               fontsize=20, prop={'family': 'Arial', 'size': 18})
    plt.savefig(save_path, bbox_inches='tight')
    plt.show()
    return df_pd


#Figure 6: Radar plot of the means of individual criterion scores among three LLMs.


radar_plot(df_pd = df, save_path = 'mean_radar_ind.pdf')


#Figure 7: Radar plot of the SD of individual criterion scores among three LLMs. 


radar_plot(df_pd = df1, save_path = 'sd_radar_ind.pdf')






