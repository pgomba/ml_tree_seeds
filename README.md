# Overview

![](images/project_cover.png)

This file provides an overview of the supplementary data and code associated with the study titled **"Enhancing Tree Seed Germination with Image-Driven Machine Learning Models."** The contents are designed to support the reproducibility of the study and facilitate further analysis. Running the analysis will create an `Outputs` folder, which will be populated as the scripts are executed. The `Outputs` folder is pre-populated to save time by avoiding the need to process all data from scratch. However, its contents can be recreated by running the provided scripts in order. The approximate time to run each script is provided next to the corresponding file names.

## Contents

1.  Data files

    `data/ml_paper_seeds_all.csv:` Morphological (shape, size, colour) traits obtained using ImageJ/Fiji directly on original images and texture on individual images using Scikit-image (Van Der Walt et al., 2014) library functions, *`graycoprops`* and *`graycomatrix`*.

    `data/individual_seed_images.rar:` This compressed file contains all individual seed images used to train and evaluate the Convolutional Neural Networks (CNNs). The images were extracted from grid-based seed images using ImageJ/Fiji. To simplify the supplementary data package and reduce file size, the original grid images are not included. However, these can be requested by contacting the corresponding author. For convenience, the individual seed images are already organized into `xray/` and `colour/` groups, stratified by `training/` and `testing/` data sets and sorted in two folders: `0/`: Non-germinated seeds and `1/`: Germinated seeds.

    This compressed folder also includes the script `all_species.R` \| :hourglass: *5 min*. \| Run this script with the `individual_seed_images` folder set as the working directory. The script will create a new folder that combines images from all species, enabling the training and evaluation of CNNs using a data set that includes all images regardless of species.

    `data/maternal_predict.csv:` Data containing recorded germination for all seeds and the predicted germination from all models

    1.  Code files

        R and Jupyter Notebook files are included in the root directory and are numbered in the order they should be run. Note the working directory for these files to run is the root directory. These include:

        **`01_summary_tables.R`**: \| :hourglass: \< *10 sec. \|* Creates an `Outputs` folder and generates the main (except Table 2) and supplementary tables for the Materials and Methods section in .html format.

        **`02a_Xgboost.ipynb`**: \| :hourglass: \~9 h. *\|* XGBoost hyperparameters tuning. Outputs generated in `Outputs/XGBoost`

        **`02b_Xgboost.ipynb`**: \| :hourglass: \~5 min. *\|* Finalization of XGBoost analysis. Outputs generated in `Outputs/XGBoost`

        **`03_CNNs.ipynb:`** \|hourglass: \~50-80 min. \| CNN training and testing. Ouptuts generated in `Outputs/CNNs`.

        **`04_models_summary.R`** \|hourglass: \< 10 sec. \| Combines evaluation metrics from XGBoost and CNNs and produce Figure 2.

        **`05_compile_maternal_predictions`**: Creates file `maternal_predictions.csv` in `Outputs` folder. A compilation of all predictions in the testing dataset to be used in later figures.

        **`06_germination_fractions.R`**: \| :hourglass: \<*10 sec \|* Creates Figure 3 (Germination fractions) and Figure 4 (Collection quality improvements using models).

        **`07_traits_vs_germination.R`**: \| :hourglass: \<*10 sec. \|* Pairwise comparison between morphological traits and relevant figures. Outputs Fig S1A
