import os
import pathlib

import numpy as np
import pandas as pd
from sklearn.metrics import confusion_matrix

metrics = ['binary_accuracy', 'precision', 'specificity', 'recall', 'f1']

cnn_folder = os.path.join('Outputs', 'CNNs')
xgb_folder = os.path.join('Outputs', 'XGBoost')

species = ['all_species', 'Alnus_glutinosa', 'Betula_pendula', 'Betula_pubescens', 'Pinus_sylvestris', 'Sorbus_aucuparia']
cases = ['colour', 'xray']


def do_CNN_confusion_matrices():
    pathlib.Path(os.path.join(cnn_folder, 'confusion_matrices')).mkdir(parents=True, exist_ok=True)
    for sp in species:
        for case in cases:
            bin_results = pd.read_csv(os.path.join(cnn_folder, f'bin_pred_{sp}_{case}.csv'))


            cnn_df = make_confusion_matrix(bin_results)

            cnn_df.to_csv(os.path.join(cnn_folder, 'confusion_matrices', f'CM_CI_for_{sp}_{case}.csv'))

def do_XGB_confusion_matrices():
    pathlib.Path(os.path.join(xgb_folder, 'confusion_matrices')).mkdir(parents=True, exist_ok=True)


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
            xgb_df = make_confusion_matrix(bin_results)


            xgb_df.to_csv(os.path.join(xgb_folder, 'confusion_matrices', f'CM_for_{sp}_{case}.csv'))



def make_confusion_matrix(bin_results):
    predictions = bin_results['prediction']
    real_class = bin_results['real_class']
    _confusion_matrix = pd.DataFrame(confusion_matrix(real_class, predictions))
    new_index = [f'Actual {c}' for c in _confusion_matrix.index]
    _confusion_matrix.index = new_index

    new_cols = [f'Predicted {c}' for c in _confusion_matrix.columns]
    _confusion_matrix.columns = new_cols
    return _confusion_matrix


def main():
    do_XGB_confusion_matrices()
    do_CNN_confusion_matrices()


if __name__ == '__main__':
    main()
