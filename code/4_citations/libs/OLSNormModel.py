import pandas as pd
from tqdm.notebook import tqdm
import numpy as np
import statsmodels.api as sm
from libs.LastNamesInference import LastNamesInference
import statsmodels.formula.api as smf


class OLSNormModel:
    
    def __init__(self,df_ols):
        race_gender_groups= ["white_M", "hispanic_M", "black_M", "asian_M", "white_F", "hispanic_F", "black_F", "asian_F"]
        self.race_gender_groups = race_gender_groups
        self.df_ols = df_ols
    
    def run_ols(self,df,covars,dep='cit_all_IAC',logvars=None,vars_100=None, catvars=None, removing_threshold = None):

        df_ols = df[[dep]+covars].copy()

        # df_ols = pd.get_dummies(df_ols, drop_first=True)
        if removing_threshold is not None:
            df_ols=df_ols[df_ols[dep]<=removing_threshold]

        covars.remove('white_M')

        if 'topic_1' in covars:
            covars.remove('topic_1')
        if logvars is not None:
            covars = [x for x in covars if x not in logvars]
        # if catvars is not None:
        #     covars = [x for x in covars if x not in catvars]

        if vars_100 is not None:
            for var_100 in vars_100:
                df_ols[var_100] = df_ols[var_100]*100

        form = "{} ~ {}".format(dep, " + ".join(covars))

        if logvars is not None:
            logvars_str = '+'.join(['np.log('+x+')' for x in logvars])
            form = form + " + "+ logvars_str

        # if catvars is not None:
        #     catvars_str = '+'.join(['C('+x+')' for x in catvars])
        #     form = form + " + "+ catvars_str
        
        df_ols = df_ols.dropna()

        ols_model = smf.ols(form, df_ols)

        ols_model_res = ols_model.fit()
        return ols_model_res
    def write_summary_results(self,path, results):
        with open(path, 'w') as f:
            f.write(results.summary().as_text())

    def results_summary_to_dataframe(self, results, model = 'all'):
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
        results_df['model'] = model
        return results_df

    def save_results(self, results,path='../../results/linear_models/agg_model/', model = 'all'):
        path_summary = path + 'log_summary/ols_{}.txt'.format(model)

        self.write_summary_results(path_summary, results)
    def get_params_table(self,fitted_models):
        results = pd.DataFrame()
        for g in fitted_models.keys():
            model_result = fitted_models[g]
            _ = self.results_summary_to_dataframe(model_result,g)
            _[['institution_cov','dep']]=_.model.str.split('+', expand=True)
            _['n'] = model_result.nobs
            _.drop(columns='model', inplace=True)
            results = results.append(_)
        return results
        
    def main(self,model='usnr_rank_cat', dep = 'norm_Cit_2_iac'):
        # model: ['usnr_rank_cat','selindex','avg_citations_Q','avg_citations']
        df_ols = self.df_ols
        df_ols = pd.get_dummies(df_ols,columns=['selindex','avg_citations_Q','usnr_rank_cat'])
        uni_cat_to_remove = ['avg_citations_Q_low','selindex_inclusive', 'usnr_rank_cat_not_top']
        df_ols = df_ols.drop(columns= uni_cat_to_remove)

        # university_groups = [model]
        covars= ['nb_auteur','career_age'] + self.race_gender_groups #+ university_groups #, 'Annee_Bibliographique'
        logvars = None
        vars_100 = None
        if model in (['usnr_rank_cat','selindex','avg_citations_Q']):            
            added_dummy_cols = [item for item in list(df_ols.columns.values) if item.startswith(model)]
            covars = covars + added_dummy_cols
        else:
            covars = covars + [model]
        # if 'Cit_2_iac' in dep:
        #     df_ols=df_ols[df_ols.Annee_Bibliographique<=df_ols.Annee_Bibliographique.max()-2]
        #normalize citations/JIF by topic


        model_fit = self.run_ols(df_ols,covars,dep,logvars=logvars,vars_100=vars_100,catvars=None)
        self.save_results(model_fit,'../../results/linear_models/agg_model/',model=dep+'_'+model)
        return model_fit