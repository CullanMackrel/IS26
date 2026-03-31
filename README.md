# Identifying The Role Of Wildfires On Hydrogen Sulfide Measurements In Alberta’s Oil Sand Regions
## This repository contains the R scripts needed to perform the analysis and comparison of wildfire data and Hydrogen Sulfide (H2S) Concentrations
Before the use of these scripts, both VIIRS Satellite data and Alberta Oil Sands Air Monitoring Station data must be downloaded, which can be done from the links provided below:
+ VIIRS Satellite Data can be downloaded [here](https://firms.modaps.eosdis.nasa.gov/download/)
+ Oil Sands Data can be downloaded [here](https://wbea.org/data/continuous-monitoring-data/)

To avoid downloading unnecessary VIIRS data, open the map tool and highlight the specific region to receive data for that region only. For the oil sands data, select "One Parameter/Multiple Stations" with H2S as the parameter, and select the desired data time frame (ensure this falls within the VIIRS data time frame).

### Importing The Data Into R
Firstly, use NecessaryLibraries.R to ensure all libraries are installed or loaded, and set the working directory to whatever is being used. Secondly, using BaseFunctions.R, store each function in the environment. Navigating back to NecessaryLibraries.R, run Import_FIRMS (this usually takes a minute or two). Finally, set the TFIM parameters to the desired latitude, longitude, date range, etc.

Running the code in DataCleaning.R (Up until the plotting section) will import and organize the Oil Sands data into a cleaner form that is optimal for plotting.



