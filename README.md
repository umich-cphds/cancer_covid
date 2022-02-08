# Cancer-COVID analysis in Michigan Medicine

* Files in `src/` folder are functions required to run the analysis pipeline.
    * Large, complex functions get their own script
    * Small, helper functions are listed in the `functions.r` script

* The `lists/` folder contains list objects that specify codes or variables for analysis. For example, the `cancer_phecodes` sublist of the `cancer_codes` list contains all the phecodes that we use to specify "any cancer".

* The `objects/` folder contains the analysis outputs including `main_data`, the dataset which has been prepared for analysis.
