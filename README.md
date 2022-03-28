# Agriculture-Human-Health-MicroEconomic (AHHME) Tool
Code :computer: and necessary inputs :file_folder: for a holistic economic :chart: model for evaluating the cost-effectiveness :money_mouth_face: of AMR :microbe: interventions in agriculture :pig: :hatching_chick: across countries :earth_asia:, and with a case study in Viet Nam :vietnam:. The interactive online modelling tool version of this model is availble at (https://trescovius.shinyapps.io/Model-Code-and-Inputs/)

# Paper Information
Title: A Holistic Macroeconomic Model for Evaluating the Cost-Effectiveness of AMR Interventions in Agriculture across Countries <br> <br>
Authors (alphabetical order): <br>
Eve Tresco Emes, Gwenan M Knight, Nichola Naylor, Dang Sinh, Le Trang, Jeff Waage <br> <br>


# Contents of Repo

## Functions.R
An R script which creates all of the functions used in the model

## Results Generation.R
An R script which uses the functions created in Functions.R to generate all of the figures and tables referenced in the paper

## Example.R
An R script which demonstrates how a researcher or policymaker could adapt this model to simulate a real intervention in their context of interest

## inputs - general model.csv
A spreadsheet containing all of the inputs needed to run the main model

## inputs - case study.csv
A spreadsheet containing the main inputs for the case study version of the model

## Viet Nam dependency ratio.csv
An additional input for the case study version of the model, used in the demographic module of the model

## Vietnam Population.csv
The final input for the case study version of the model, used in the demographic module of the model

## Model Parameters and Calculations.pdf
This spreadsheet contains sources for our model parameters for the general model; and displays the values used, their distributions (if applicable), and how they were calculated

# Required packages
data.table / readxl / stargazer / tidyverse / tseries / forecast / dynlm / seastests / forecast / TSA / epiR / extraDistr / MonoInc / pksensi / sensitivity / xlsx / gridExtra / ggplot2 / reshape2 / here / multisensi / rsq / forcats

# Using this model
The easiest way to reproduce the results of our paper is to clone this repository to your computer and then run the "Results Generation.R" file. <br><br> A major aim of these models is that they be used to simulate One Health AMR interventions in agriculture in other real-life contexts. In order to do this, researchers and policymakers can alter the inputs to reflect the context of interest. Because it may be difficult to determine all of the necessary parameters for the context of interest, we recommend the following:<br><br>1) choose the income category scenario that is most appropriate (low-income (LIC), middle income (MIC), or high income(HIC));<br>2) in the input file for the general version of the model, 'inputs - general model.csv', edit the parameters in the column of the chosen income category where they are known, and leave the remaining parameters unchanged;<br>3) go to the general version of the model and select the relevant income category by editing the line <br>`scenario_income <- "..... `;<br>4) run the model and obtain results! <br><br>the more accurately the user is able to parameterise the model to the context of the interest, the more accurate the results. An example of how to adapt the model to a country-specific context is included in the file "Example.R".

# Data
All sources can be found in the bibliography of the paper. The sources used to determine each parameter, and the methods used for calculating them, can be found in the appendix thereof

