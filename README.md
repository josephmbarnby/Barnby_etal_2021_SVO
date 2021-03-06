# Introduction
Models and data describing participant social learning about different partner Social-Value Orientations (SVO) given their own SVO preferences in Barnby et al., 2021.

![Abstract](GraphicalAbstract.png)

# Computational Models

Data structure required for models are like so

ID | Trial | Option1 Self | Option1 Other | Option2 Self | Option2 Other | PPT Choice/Prediction | Partner Answer | PPT Choice Categorical | Correct
------------ | ------------- | ------------- | ------------- | ------------- | ------------- | ------------- | ------------- | ------------- | -------------
1 | 1 | 8 | 8 | 8 | 5 | 1 | - | 1 | -
... | ... | ... | ... | ... | ... | ... | ... | ... | ...
1 | 18 | 10 | 6 | 8 | 2 | 1 | - | 2 | -
1 | 19 | 7 | 7 | 10 | 6 | 1 | 1 | 1 | 1
... | ... | ... | ... | ... | ... | ... | ... | ... | ...
1 | 54 | 6 | 2 | 8 | 5 | 2 | 1 | 2 | 0



Matlab files of models are constructed to enable use of Concurrent Bayesian Modelling (CBM; Piray et al., 2018) functions - for a guide on how to use these series of useful functions see Payam's blog online here https://payampiray.github.io/cbm.

Each computational model is contained in a single .m file.

Each computational model is first fitted using Laplace approximation (cbm_lap) and concurrently compared (cbm_hbi) in the Master '.m' file.

Speedier modelling requires the installation of the 'parfor' function in the parralell toolbox in Matlab. I have already written the master .m file to include parralell processing, although the cbm_lap function does not require it (it will however take _significantly_ longer without it).

<b> NB: Model fitting for the Bayesian models (even using ParFor) will take a long time. </b> Adjust your expectations accordingly. 

Approximate time commitment for fitting 697 participants using 8 cores:

<i> machine specs:  MacBook Pro 2.6 GhZ 6-Core (12 virtual cores) Intel Core i7, 16GB DDR4 RAM </i>

 - heuristic model fitting (4-6 parameters) ~ 1 hour
 - 4 parameter Bayes model laplace approximation ~ 6 hours
 - 5 parameter Bayes model laplace approximation ~ 9 hours
 - 6 parameter Bayes model laplace approximation ~ 12-14 hours
 - Concurrent Bayesian fitting for all model ~ 96+ hours
 - Concurrent Bayesian fitting & null model fitting for the final 3 selected models ~ 12 hours

If you figure out a script to connect to a cluster to run these analyses this would be probably optimal for your time and your sanity, and please share with me if possible so I can add it! :)

# Analysis

The winning model is imported into the .R script to test and assess.
All visualisations and regression models are completed with this script.
Required packages for each analyses are loaded and listed at the start.

<i>NB: Graphical abstract and Figure 1 made using Adobe Illustrator </i>

If you want to simply recheck the models used for the main regression models reported in the paper, all you need to do is open 'Analysis_Reproduction_Workbook.Rmd' in your favorite IDE and click each code chunk. Alternatively, you can check out the knitted html of the output for a quick check. All code chunks in this script are listed in the same order as that in the manuscript and model numbers correspond to those in the manuscript text.
