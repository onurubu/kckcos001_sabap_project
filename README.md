# kckcos001_sabap_project
Coşkun Onur Küçükkaragöz's repository for the SABAP project for BIO3019S

This code is for analysing the changes of reporting rates of endangered raptors by comparing SABAP 1 and 2, and then using SABAP 2 only using time series analysis.

The code is split into 2 sections each representing the different analyses.

The code to read the data, process the data, and create the models have been fully automated so that these scripts can be used to run the analyses again for any set of species. The only alteration required will be to put the appropriate species number and species name (according to the species name convention found in the Merged SABAP 1 and 2 dataset) into the section where the SPP data frame is created. 

For the time series analysis the appropriate SABAP 2 data set with null counts for the species in question is required. These data sets can be searched for and found at http://sabap2.birdmap.africa/species . After downloading the data sets the spp data frame needs to be changed jsut like for the comparison analysis.

Also obviously the file directories must be changed to match your system.

The data to display the model summaries and plot is not automated and they must be written by the user. The current existing code can be used as an example as the naming conventions will be consistent.

My raw data sets are available in this repository for anyone to try out the code with.