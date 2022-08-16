# salmon-migration-timing
# explore inter-annual changes in run timing for juvenile CCV Chinook 

# The code as written assumes there's a "data" folder and a "figures" folder in your 
# working directory. The former should include the DJFMP data, accessible via
# the edi.244.9.5 script as well as a couple other smaller data files (eg, 
# yearassignments.csv, which includes the year type).The figures folder just 
# serves as a repository for figures that you may want quick access to.

# To re-run all of the code here (as of Aug 16, 2022), you'd want to start by
# running "edi.244.9.r", followed by "data acquistion.R", "data QAQR.R", and then
# "start your day.R". I think the others (eg "chipps_all_runs.R") should run fine,
# once those code files have been run and the appropriate objects are in your 
# working space. Thereafter, you should just be able to start with "start your
# day.R"; that'll load all the data files. 