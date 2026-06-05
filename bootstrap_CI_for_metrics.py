import os

import numpy as np
import pandas as pd
from sklearn.metrics import precision_score, recall_score, f1_score

metrics = ['binary_accuracy', 'precision', 'specificity', 'recall', 'f1']

cnn_folder = os.path.join('Outputs', 'CNNs')
xgb_folder = os.path.join('Outputs', 'XGBoost')

species = ['all_species', 'Alnus_glutinosa', 'Betula_pendula', 'Betula_pubescens', 'Pinus_sylvestris', 'Sorbus_aucuparia']
cases = ['colour', 'xray']


def do_CNN_bootstraps():
    for sp in species:
        for case in cases:
            bin_results = pd.read_csv(os.path.join(cnn_folder, f'bin_pred_{sp}_{case}.csv'))


            cnn_df = pd.DataFrame(bootstrap_CI(bin_results), index=['Mean', 'Lower', 'Upper'])

            cnn_df.to_csv(os.path.join(cnn_folder, 'CIs', f'bootstrap_CI_for_{sp}_{case}.csv'))

def do_XGB_bootstraps():
    # The output predictions haven't been stored very nicely
    # Make sure to check outputs against earlier calculations
    existing_eval_metrics = pd.read_csv(os.path.join(xgb_folder, 'evaluation_results.csv'))


    species = ["Alnus_glutinosa", "Betula_pendula", "Betula_pubescens", "Pinus_sylvestris", "Sorbus_aucuparia", "all_species"]
    folders = ["names_all_features", "names_colour_features", "names_xray_features"]

    # Get y_test values
    seed_data = ("data/ml_paper_seeds_all.csv")
    seeds_all_data = pd.read_csv(seed_data)


    for case in folders:
        predictions = pd.read_csv(os.path.join(xgb_folder, f'predictions_{case}.csv'))
        for sp in species:
            # Get y_test values
            seeds_sp = seeds_all_data[seeds_all_data.Species == sp]
            holdout = seeds_sp[seeds_sp.Set == "Hold out"]
            y_test = holdout['Bin_germ']
            y_test = y_test.to_numpy()

            sp_predictions = predictions.head(len(y_test))
            # remove sp_predictions rows from predictions
            predictions = predictions.iloc[len(y_test):]

            bin_results = pd.DataFrame({'prediction': sp_predictions['Prediction'].values, 'real_class': y_test})
            xgb_df = pd.DataFrame(bootstrap_CI(bin_results), index=['Mean', 'Lower', 'Upper'])

            ## Checks
            this_case = existing_eval_metrics[existing_eval_metrics.model == case]
            this_case = this_case[this_case.Species == sp]

            assert round(this_case['Accuracy'].iloc[0],2) == xgb_df['binary_accuracy'].iloc[0]
            assert round(this_case['f1'].iloc[0],2) == xgb_df['f1'].iloc[0]
            assert round(this_case['Specificity'].iloc[0],2) == xgb_df['specificity'].iloc[0]

            xgb_df.to_csv(os.path.join(xgb_folder, 'CIs', f'bootstrap_CI_for_{sp}_{case}.csv'))



def bootstrap_CI(bin_results):
    bin_results['correct'] = bin_results['prediction'] == bin_results['real_class']
    bin_results['correct'] = bin_results['correct'].astype(int)

    total_binary_accuracy = bin_results['correct'].mean()
    total_precision = precision_score(bin_results['real_class'], bin_results['prediction'])
    total_recall = recall_score(bin_results['real_class'], bin_results['prediction'])
    total_f1 = f1_score(bin_results['real_class'], bin_results['prediction'])
    total_specifity = recall_score(bin_results['real_class'], bin_results['prediction'], pos_label=0)

    total_stats = {'binary_accuracy': total_binary_accuracy, 'precision': total_precision, 'recall': total_recall, 'f1': total_f1,
                   'specificity': total_specifity}

    stats = {}
    for metric in metrics:
        stats[metric] = []
    for i in range(1000):
        boot_sample = bin_results.sample(frac=1, replace=True).reset_index(drop=True)
        stats['binary_accuracy'].append(boot_sample['correct'].mean())
        stats['precision'].append(precision_score(boot_sample['real_class'], boot_sample['prediction']))
        stats['recall'].append(recall_score(boot_sample['real_class'], boot_sample['prediction']))
        stats['f1'].append(f1_score(boot_sample['real_class'], boot_sample['prediction']))
        stats['specificity'].append(recall_score(boot_sample['real_class'], boot_sample['prediction'], pos_label=0)
                                    # specificity is the recall of the negative class.
                                    )

    metric_ci_stats = {}
    alpha = 0.95
    for metric in metrics:
        # confidence intervals
        p = ((1.0 - alpha) / 2.0) * 100
        lower = np.percentile(stats[metric], p)
        p = (alpha + ((1.0 - alpha) / 2.0)) * 100
        upper = np.percentile(stats[metric], p)

        mean = total_stats[metric]
        metric_ci_stats[metric] = [round(mean, 2), round(lower, 2), round(upper, 2)]

    return metric_ci_stats


def main():
    do_XGB_bootstraps()
    do_CNN_bootstraps()


if __name__ == '__main__':
    main()
