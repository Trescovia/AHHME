# Model-Code-and-Inputs
Code :computer: and necessary inputs :file_folder: for a holistic macroeconomic :chart: model for evaluating the cost-effectiveness :money_mouth_face: of AMR :microbe: interventions in agriculture :pig: :hatching_chick: across countries :earth_asia:, and with a case study in Viet Nam :vietnam:

# Paper Information
Title: A Holistic Macroeconomic Model for Evaluating the Cost-Effectiveness of AMR Interventions in Agriculture across Countries <br> <br>
Authors (alphabetical order): <br>
Tresco Emes, Gwenan M Knight, Nichola Naylor, Jeff Waage <br> <br>
Other collaborators (alphabetical order) <br> Dang Sinh, Fred Unger, Le Trang <br>


# Contents of Repo

## code - general model.R
R code for the main model. It is not parameterised to a single country but instead has scenarios which aim to be broadly reflective of countries at different levels of income per person

## inputs - general model.csv
A spreadsheet containing all of the inputs needed to run the main model

## code - case study.R
R code for the case study version of the model, which simulates a named intervention in present-day Viet Nam

## inputs - case study.csv
A spreadsheet containing the main inputs for the case study version of the model

## Viet Nam dependency ratio.csv
An additional input for the case study version of the model, used in the demographic module of the model

## Vietnam Population.csv
The final input for the case study version of the model, used in the demographic module of the model

# Required packages
data.table / readxl / stargazer / tidyverse / tseries / forecast / dynlm / seastests / forecast / TSA / epiR / extraDistr / MonoInc / pksensi / sensitivity / xlsx / gridExtra / ggplot2 / reshape2 / here / multisensi

# Using this model
To run either the main model or the case study, download the six files named above and store them in the same folder on your computer. Ensure that the required packages are installed. The models can be run straight away <br> A major aim of these models is that they be used to simulate One Health AMR interventions in agriculture in other real-life contexts. In order to do this, researchers and policymakers can alter the inputs to reflect the context of interest. Because it may be difficult to determine all of the necessary parameters for the context of interest, we recommend the following:<br><br>1) choose the income category scenario that is most appropriate (low-income (LIC), middle income centred on smallholder agricultre (MIC-s), middle income centred on industrial agriculture (MIC-i) or high income);<br>2) in the input file for the general version of the model, 'inputs - general model.csv', edit the parameters in the column of the chosen income category where they are known, and leave the remaining parameters unchanged;<br>3) open the general version of the model 'General Model.R' and select the relevant income category by editing the line <br>`scenario_income <- "..... `;<br>4) run the model and obtain results! <br><br>the more accurately the user is able to parameterise the model to the context of the interest, the more accurate the results

# Data
All sources can be found in the bibliography of the paper. The sources used to determine each parameter, and the methods used for calculating them, can be found in the appendix thereof

