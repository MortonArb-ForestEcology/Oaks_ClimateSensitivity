# Oaks_ClimateSensitivity
Looking at climate sensitivity in our Quercus Collection

Currently to run all that is needed is an input of occurence points (retrieved using the 0_BIEN_Download script or a script by Emily Beckman) and you can start with script 1.

Scripts
0_BIEN_Download -- A Script that can download BIEN data for traits and occurence points.

0_Oak_Ordination -- A Script for performing an ordination on BIEN traits for select species. Currently unused/leftover.

0_traits_organizing -- A Script for organizing trait lists downloaded from TRY or BIEN.

1_Oak_Occurence -- A Script for taking a csv of lat and long occurence points, downloading daily day met data for those points, and then     summarizing that data into desirable metrics. Input: csv of species occurence points. Output: csv of summarizing daymet data for every year.

2_Oak_Mapping -- A script for Mapping the occurence points of our species. Currently just a playground for me learning. Not fully functional.

