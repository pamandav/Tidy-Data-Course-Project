## This is created to address Course Project for Getting and cleaning data.

###About the project.

This project attempts to clean the data fetched from a URL.

###Input Data

The input data collected from the accelerometers from the Samsung Galaxy S smartphone. It is collected from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

###Output Data

The output consists of mean of all variables related to mean and standard deviation for a particular subject and activity.

###Brief Description of files included.

1. run_analysis.R - the R script that cleans the data.
2. CookBook.md -  Describes the variables, the data, and any transformations that are performed.
			   -  It has step by step description of run_analysis.R
3. README.md - Gives the list of all the scripts.
4. tidydata.txt - Has the output of the R script.		

##Explanation of the analysis file.

1. run_analysis.R  is the script that does transformations to get the tidy data.
2. First the data is downloaded from the URL mentioned.
3. Next the data is read from the files downloaded.
4. The transformations mentioned in the CookBook.md are performed.
5. The tdy data is then written to a file(tidydata.txt)
