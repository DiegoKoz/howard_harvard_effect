import pandas as pd
from tqdm.notebook import tqdm
import numpy as np
import statsmodels.api as sm
from libs.LastNamesInference import LastNamesInference
import statsmodels.formula.api as smf


class OLSSubsetModel:
    
    def __init__(self,df_ols):
        self.df_ols = df_ols
        self.race_gender_groups = ["white_M", "hispanic_M", "black_M", "asian_M", "white_F", "hispanic_F", "black_F", "asian_F"]
        self.diversity_university_groups = ['HBCU','HSI','Womens']
        self.categorizations_university_groups = ['usnr_rank_cat','avg_citations_Q','avg_citations_Q10','selindex']
    
    def run_ols(self,df,covars,dep='cit_all_IAC',logvars=None,vars_100=None, catvars=None, removing_threshold = None):

        df_ols = df[[dep]+covars].copy()

        # df_ols = pd.get_dummies(df_ols, drop_first=True)
        df_ols = df_ols.dropna()

        if removing_threshold is not None:
            df_ols=df_ols[df_ols[dep]<=removing_threshold]

        if dep is 'Cit_2_iac':
            df_ols=df_ols[df_ols.Annee_Bibliographique<=df_ols.Annee_Bibliographique.max()-2]

        if 'white_M' in covars:
            covars.remove('white_M')
        if 'topic_1' in covars:
            covars.remove('topic_1')
        if logvars is not None:
            covars = [x for x in covars if x not in logvars]
        if catvars is not None:
            covars = [x for x in covars if x not in catvars]

        if vars_100 is not None:
            for var_100 in vars_100:
                df_ols[var_100] = df_ols[var_100]*100

        form = "{} ~ {}".format(dep, " + ".join(covars))

        if logvars is not None:
            logvars_str = '+'.join(['np.log('+x+')' for x in logvars])
            form = form + " + "+ logvars_str

        if catvars is not None:
            catvars_str = '+'.join(['C('+x+')' for x in logvars])
            form = form + " + "+ catvars_str

        ols_model = smf.ols(form, df_ols)

        ols_model_res = ols_model.fit()
        return ols_model_res

    def write_summary_results(self,path, results):
        with open(path, 'w') as f:
            f.write(results.summary().as_text())

    def results_summary_to_dataframe(self, results, subset = 'all'):
        '''take the result of an statsmodel results table and transforms it into a dataframe'''
        pvals = results.pvalues
        coeff = results.params
        conf_lower = results.conf_int()[0]
        conf_higher = results.conf_int()[1]

        results_df = pd.DataFrame({"pvals":pvals,
                                   "coeff":coeff,
                                   "conf_lower":conf_lower,
                                   "conf_higher":conf_higher
                                    })
        results_df = results_df[["coeff","pvals","conf_lower","conf_higher"]]
        results_df = results_df.reset_index().rename(columns={'index':'variable'})
        results_df['subset'] = subset
        return results_df

    def save_results(self, results,path='../../results/linear_models/subsets/', subset = 'all'):
        path_summary = path + 'log_summary/ols_{}.txt'.format(subset)

        self.write_summary_results(path_summary, results)

        # res_df = self.results_summary_to_dataframe(results, subset)
        # res_df.to_csv(path + 'ols_{}.csv'.format(subset), index=False)
    
    def get_params_table(self,fitted_models):

        results = pd.DataFrame()
        for g in fitted_models.keys():
            models_group = fitted_models[g]
            res_group = pd.DataFrame()
            for c in models_group.keys():
                _ = self.results_summary_to_dataframe(models_group[c],c)
                _['n'] = models_group[c].nobs
                res_group = res_group.append(_)
            res_group['university_group'] = g
            results = results.append(res_group)
        return results
        
    def main(self,subset = 'all', dep = 'norm_Cit_2_iac'):
        
        covars= ['nb_auteur','career_age'] + self.race_gender_groups #, 'Annee_Bibliographique'
        logvars = None
        vars_100 = None
        
        # if subset in (['usnr_rank_cat','selindex','avg_citations_Q']):
        #     catvars = [subset]
        # else:
        #     catvars = None
        
        df_ols = self.df_ols
        # if dep is 'norm_Cit_2_iac':
        #     df_ols=df_ols[df_ols.Annee_Bibliographique<=df_ols.Annee_Bibliographique.max()-2]

        df_subset = df_ols[~df_ols[subset].isnull()]
        cats = df_subset[subset].unique()
        models = {}
        for c in cats:
            df_tmp = df_subset[df_subset[subset]==c]
            model_subset = self.run_ols(df_tmp,covars,dep,logvars=logvars,vars_100=vars_100)
            models[c] = model_subset
            self.save_results(model_subset,'../../results/linear_models/subsets/',subset=dep+'_'+subset+':'+c)
        return models