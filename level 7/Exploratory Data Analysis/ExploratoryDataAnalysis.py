#!/usr/bin/env python
# coding: utf-8

# In[1]:


#########EDA Questions#########
#1 Which country has the highest score? Why?
#2 How many observations are there?
#3 Are there any null values? How does this dataset need to be cleaned?
#4 Is there any correlation between the features?
#5 Are the minimum and maximum happiness scores reasonable? Are there any outliers?
#6 What is the mean happiness score?


# In[2]:


#### Main areas ####
# Data wrangling
# Data analysis
# Data visualization
# Model prediction and evaluation


# In[3]:


# imports
import seaborn as sns
import pandas as pd
import numpy as np


# In[9]:


# loading into a dataframe
df = pd.read_csv('data/hapiness2019.csv')
df


# In[10]:


df.columns


# In[12]:


#Number of rows and columns
df.shape


# In[13]:


#Check for NaN values in this dataset
df.isnull().values.any()


# In[14]:


#data types in this dataset
df.dtypes


# In[16]:


#Minimum and maximum happiness scores


# In[18]:


df['Score'].max()


# In[19]:


df['Score'].min()


# In[20]:


#Average Socre
df['Score'].mean()


# In[ ]:




