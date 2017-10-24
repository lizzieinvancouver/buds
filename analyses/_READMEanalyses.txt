README
budexperiments/analyses
Started 6 March 2017
Updated 21 May 2017
By Lizzie

This folder contains R code written by Dan F. B. Flynn for his growth chamber experiment conducted in winter-spring 2015 and for the chilling experiment winter-spring 2016.

Since it’s by Lizzie and not Dan, these are generally educated guesses!

<><><><><><><>><><><><>><><>
Main files I have been using:
<><><><><><><>><><><><>><><>

Pheno Budburst prep.R - preps the data, good use for methods and I used it to figure out who to acknowledge

Pheno Budburst analysis.R — this is the MAIN analysis file and is sourced in the main text and supp (incl. figures: Sens_vs_day)

Additional Plots and Processing.R 

chill unit calcs.R — is used in the Supp file

climatesummary.R — results of this are referenced in supp file (Lizzie built this file in late June 2017 in response to queries by Yann Vitasse)

<><><><><><><>><><><><>><><>
Other files I suspect are important:
<><><><><><><>><><><><>><><>
Analyzing non-leafouts.R



<><><><><><><>><><><><>><><>
Other important files:
<><><><><><><>><><><><>><><>

FakeBudburst_Generate.R — for generating fake data for models

FakeBudburst_Generate_ind.R — for generating fake data for models, including tree ID (not currently used in main experiment ms, but may be useful for traits analysis)

FakeBudburst_Analysis.R — for analysis of fake data for models


<><><><><><><>><><><><>><><>
Files I hypothesize are not that useful:
<><><><><><><>><><><><>><><>

eliminatingdivergingerrors.R - I don’t think this does what the name says, see my notes in the stan folder. 

<><><><><><><>><><><><>><><>
supp folder
<><><><><><><>><><><><>><><>
Copied files (from elsewhere in analyses folder) that will be submitted with manuscript as supplemental materials. See also notes in _READMEstan.txt in the Stan folder. 