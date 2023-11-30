import pandas as pd
import numpy as np
import datetime as dt

import matplotlib.pyplot as plt
import seaborn as sns

from lifelines import CoxPHFitter

from sklearn.model_selection import GridSearchCV, KFold, RepeatedKFold, train_test_split, ParameterGrid
import sksurv.util
import warnings
from lifelines.utils import k_fold_cross_validation
from sklearn.linear_model import ElasticNet

from sksurv.preprocessing import OneHotEncoder
import warnings
warnings.filterwarnings('ignore')


df=pd.read_csv("../1_data/private/vitalant_preprocessed_data.csv")
#split by pre-pandemic and intra pandmeic

# the date to divide the DataFrame
split_date = pd.to_datetime('2020-01-1')
df["Visit_Date"] = df["Visit_Date"].astype("datetime64[ns]")

# Create two subsets based on the split date
pre = df.loc[df["Visit_Date"] <= split_date]
intra = df.loc[df["Visit_Date"] > split_date]

#split by fixed mobile
pre_fixed= pre.query("Fixed_mobile== 1")
pre_mobile= pre.query("Fixed_mobile == 0")

intra_fixed= intra.query("Fixed_mobile== 1")
intra_mobile= intra.query("Fixed_mobile == 0")

ref_fix=['OUTCOME_TYPE_completed', 'RACE_ETHNICITY_White', 'DONOR_GENDER_M','DONOR_ABORH_Opos', 'DONOR_EDU_Post_Secondary']
ref_mob=['OUTCOME_TYPE_completed', 'RACE_ETHNICITY_White', 'DONOR_GENDER_M', 'DONOR_ABORH_Opos', 'DONOR_EDU_Post_Secondary', 'Opp_to_donate_Annual']


#pre COVID
Xpre_fix= pre_fixed.loc[:,['time_to_return','CENSORED','DONOR_AGE_AT_DONATION', 'first_time','DONOR_GENDER', 'RACE_ETHNICITY',  'OUTCOME_TYPE', 'DONOR_ABORH', 'cum_lifetime_donations',  'rbc_loss_last_12_months', 'rbc_loss_last_24_months', 'DONOR_WEIGHT', 'DONOR_HEIGHT', 'DONOR_BORN_IN_US', 'DONOR_EDU']]
                        
#['time_to_return','CENSORED','DONOR_AGE_AT_DONATION', 'first_time','DONOR_GENDER', 'RACE_ETHNICITY',  'OUTCOME_TYPE', 'cum_lifetime_donations',  'rbc_loss_last_12_months', 'rbc_loss_last_24_months', 'DONOR_ABORH', 'DONOR_BORN_IN_US', 'DONOR_EDU', 'DONOR_WEIGHT', 'DONOR_HEIGHT']]

Xpre_fix= pd.get_dummies(Xpre_fix)  
Xpre_fix = Xpre_fix.drop(ref_fix, axis=1)  

Xpre_fix['first_time_hgb']=Xpre_fix["first_time"]*Xpre_fix['OUTCOME_TYPE_low hgb']
Xpre_fix.fillna(0, inplace=True)
ypre_fix= Xpre_fix[['time_to_return']]

Xpre_mob= pre_mobile.loc[:,['time_to_return','CENSORED','DONOR_AGE_AT_DONATION', 'first_time','DONOR_GENDER', 'RACE_ETHNICITY',  'OUTCOME_TYPE', 'DONOR_ABORH', 'cum_lifetime_donations',  'rbc_loss_last_12_months', 'rbc_loss_last_24_months', 'DONOR_WEIGHT', 'DONOR_HEIGHT', 'DONOR_BORN_IN_US', 'DONOR_EDU', 'Opp_to_donate']]

Xpre_mob= pd.get_dummies(Xpre_mob)  
Xpre_mob = Xpre_mob.drop(ref_mob, axis=1)  

Xpre_mob['first_time_hgb']=Xpre_mob["first_time"]*Xpre_mob['OUTCOME_TYPE_low hgb']
Xpre_mob.fillna(0, inplace=True)
ypre_mob= Xpre_mob[['time_to_return']]

Xpre_fix.columns = Xpre_fix.columns.str.replace(' ', '_')
Xpre_mob.columns = Xpre_mob.columns.str.replace(' ', '_')


#intra covid

Xintra_fix= intra_fixed.loc[:,['time_to_return','CENSORED','DONOR_AGE_AT_DONATION', 'first_time','DONOR_GENDER', 'RACE_ETHNICITY',  'OUTCOME_TYPE', 'DONOR_ABORH', 'cum_lifetime_donations',  'rbc_loss_last_12_months', 'rbc_loss_last_24_months', 'DONOR_WEIGHT', 'DONOR_HEIGHT', 'DONOR_BORN_IN_US', 'DONOR_EDU']]

Xintra_fix= pd.get_dummies(Xintra_fix)  
Xintra_fix = Xintra_fix.drop(ref_fix, axis=1)  


Xintra_fix['first_time_hgb']=Xintra_fix["first_time"]*Xintra_fix['OUTCOME_TYPE_low hgb']
Xintra_fix.fillna(0, inplace=True)
yintra_fix= Xintra_fix[[ 'time_to_return']]

Xintra_mob= intra_mobile.loc[:,['time_to_return','CENSORED','DONOR_AGE_AT_DONATION', 'first_time','DONOR_GENDER', 'RACE_ETHNICITY',  'OUTCOME_TYPE', 'DONOR_ABORH', 'cum_lifetime_donations',  'rbc_loss_last_12_months', 'rbc_loss_last_24_months', 'DONOR_WEIGHT', 'DONOR_HEIGHT', 'DONOR_BORN_IN_US', 'DONOR_EDU', 'Opp_to_donate']]


Xintra_mob= pd.get_dummies(Xintra_mob)  
Xintra_mob = Xintra_mob.drop(ref_mob, axis=1)  

Xintra_mob['first_time_hgb']=Xintra_mob["first_time"]*Xintra_mob['OUTCOME_TYPE_low hgb']
Xintra_mob.fillna(0, inplace=True)
yintra_mob= Xintra_mob[[ 'time_to_return']]


Xintra_fix.columns = Xintra_fix.columns.str.replace(' ', '_')
Xintra_mob.columns = Xintra_mob.columns.str.replace(' ', '_')

#drop unknowns
Xpre_fix.drop(['RACE_ETHNICITY_UNKNOWN', 'DONOR_ABORH_UNK', 'DONOR_EDU_UNAVAILABLE'], axis=1, inplace=True)
Xintra_fix.drop(['RACE_ETHNICITY_UNKNOWN', 'DONOR_ABORH_UNK', 'DONOR_EDU_UNAVAILABLE','DONOR_GENDER_UNKNOWN'], axis=1, inplace=True)
Xpre_mob.drop(['RACE_ETHNICITY_UNKNOWN', 'DONOR_ABORH_UNK', 'DONOR_EDU_UNAVAILABLE','DONOR_GENDER_UNKNOWN'], axis=1, inplace=True)
Xintra_mob.drop(['RACE_ETHNICITY_UNKNOWN', 'DONOR_ABORH_UNK', 'DONOR_EDU_UNAVAILABLE'], axis=1, inplace=True)

"""
#univariate regression

#pre-fixed
results_pre_fix = pd.DataFrame(columns=['Covariate', 'Hazard Ratio', 'Lower', 'Upper'])

covariates = Xpre_fix.columns.drop(['time_to_return', 'CENSORED'])
# Loop through covariates and fit univariate models
for covariate in covariates:
    cph_univariate = CoxPHFitter()
    cph_univariate.fit(Xpre_fix, duration_col='time_to_return', event_col='CENSORED', formula=covariate)
    hazard_ratio = cph_univariate.summary.loc[covariate, 'exp(coef)']
    lower = cph_univariate.summary.loc[covariate, 'exp(coef) lower 95%']
    upper = cph_univariate.summary.loc[covariate, 'exp(coef) upper 95%']
    results_pre_fix = results_pre_fix.append({'Covariate': covariate, 'Hazard Ratio': hazard_ratio, 'Lower': lower, 'Upper': upper}, ignore_index=True)
    
print(results_pre_fix)

#pre mobile
results_pre_mob = pd.DataFrame(columns=['Covariate', 'Hazard Ratio', 'Lower', 'Upper'])

covariates = Xpre_mob.columns.drop(['time_to_return', 'CENSORED'])
# Loop through covariates and fit univariate models
for covariate in covariates:
    cph_univariate = CoxPHFitter()
    cph_univariate.fit(Xpre_mob, duration_col='time_to_return', event_col='CENSORED', formula=covariate)
    hazard_ratio = cph_univariate.summary.loc[covariate, 'exp(coef)']
    lower = cph_univariate.summary.loc[covariate, 'exp(coef) lower 95%']
    upper = cph_univariate.summary.loc[covariate, 'exp(coef) upper 95%']
    results_pre_mob = results_pre_mob.append({'Covariate': covariate, 'Hazard Ratio': hazard_ratio, 'Lower': lower, 'Upper': upper}, ignore_index=True)
    
print(results_pre_mob)

#intra fixed
results_intra_fix = pd.DataFrame(columns=['Covariate', 'Hazard Ratio', 'Lower', 'Upper'])
covariates = Xintra_fix.columns.drop(['time_to_return', 'CENSORED'])
# Loop through covariates and fit univariate models
for covariate in covariates:
    cph_univariate = CoxPHFitter()
    cph_univariate.fit(Xintra_fix, duration_col='time_to_return', event_col='CENSORED', formula=covariate)
    hazard_ratio = cph_univariate.summary.loc[covariate, 'exp(coef)']
    lower = cph_univariate.summary.loc[covariate, 'exp(coef) lower 95%']
    upper = cph_univariate.summary.loc[covariate, 'exp(coef) upper 95%']
    results_intra_fix = results_intra_fix.append({'Covariate': covariate, 'Hazard Ratio': hazard_ratio, 'Lower': lower, 'Upper': upper}, ignore_index=True)
    
print(results_intra_fix)

#intra mobile
results_intra_mob = pd.DataFrame(columns=['Covariate', 'Hazard Ratio', 'Lower', 'Upper'])
covariates = Xintra_mob.columns.drop(['time_to_return', 'CENSORED'])
# Loop through covariates and fit univariate models
for covariate in covariates:
    cph_univariate = CoxPHFitter()
    cph_univariate.fit(Xintra_mob, duration_col='time_to_return', event_col='CENSORED', formula=covariate)
    hazard_ratio = cph_univariate.summary.loc[covariate, 'exp(coef)']
    lower = cph_univariate.summary.loc[covariate, 'exp(coef) lower 95%']
    upper = cph_univariate.summary.loc[covariate, 'exp(coef) upper 95%']
    results_intra_mob = results_intra_mob.append({'Covariate': covariate, 'Hazard Ratio': hazard_ratio, 'Lower': lower, 'Upper': upper}, ignore_index=True)
    
print(results_intra_mob)

Xpre_fix.drop(['time_to_return'], axis=1, inplace=True)
Xintra_fix.drop(['time_to_return'], axis=1, inplace=True)
Xpre_mob.drop(['time_to_return'], axis=1, inplace=True)
Xintra_mob.drop(['time_to_return'], axis=1, inplace=True)

from lifelines.utils.sklearn_adapter import sklearn_adapter
param_grid = {
    'penalizer': [0.001, 0.01],  # Range of alpha values 
    'l1_ratio':[1]#np.arange(0, 1, 0.0) # Choose lasso - for variable selection
}

base_class = sklearn_adapter(CoxPHFitter, event_col='CENSORED')
cph = base_class(solver='newton-cg', tol=1e-4)

gcv = GridSearchCV(cph,
    param_grid=param_grid,
    cv=3,
    error_score='raise')

gcv.fit(Xpre_fix, ypre_fix)

best_params = gcv.best_params_
best_model = gcv.best_estimator_

# Print the best hyperparameters
print("Best Hyperparameters:")
print(best_params)

# Print the summary of the best model
print("\nBest Model Summary:")
print(best_model.lifelines_model.print_summary())

gcv.fit(Xpre_mob, ypre_mob)

best_params = gcv.best_params_
best_model = gcv.best_estimator_

# Print the best hyperparameters
print("Best Hyperparameters:")
print(best_params)

# Print the summary of the best model
print("\nBest Model Summary:")
print(best_model.lifelines_model.print_summary())

gcv.fit(Xintra_fix, yintra_fix)

best_params = gcv.best_params_
best_model = gcv.best_estimator_

# Print the best hyperparameters
print("Best Hyperparameters:")
print(best_params)

# Print the summary of the best model
print("\nBest Model Summary:")
print(best_model.lifelines_model.print_summary())

gcv.fit(Xintra_mob, yintra_mob)

best_params = gcv.best_params_
best_model = gcv.best_estimator_

# Print the best hyperparameters
print("Best Hyperparameters:")
print(best_params)

# Print the summary of the best model
print("\nBest Model Summary:")
print(best_model.lifelines_model.print_summary())
"""
cph = CoxPHFitter()
cph.fit(Xpre_fix, 'time_to_return', 'CENSORED')
cph.print_summary()

cph = CoxPHFitter()
cph.fit(Xpre_mob, 'time_to_return', 'CENSORED')
cph.print_summary()

cph = CoxPHFitter()
cph.fit(Xintra_fix, 'time_to_return', 'CENSORED')
cph.print_summary()

cph = CoxPHFitter()
cph.fit(Xintra_mob, 'time_to_return', 'CENSORED')
cph.print_summary()
