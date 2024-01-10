This directory contains R scripts for the generation
of the Nature4SDGs dataset archived at 
https://reshare.ukdataservice.ac.uk/856560/.

The 'r_workflow_diagram.jpg' contains an overview figure of the
entire Nature4SDGs workflow. However, not all code can 
be made publicly available because they include 
confidential information that can be used to identify the 
names, demographics, opinions and locations of individual 
respondents. The same applies to many of the source datasets.

Thus, this directory contains scripts that do
not contain sensitive information. This code base
is therefore not functional for those outside the Nature4SDGs 
project, but serves as a reference for other data users 
to see (and if desired, replicate) the details of the 
data generation process.

The non-sensitive scripts are denoted in the figure in
'r_workflow_diagram.jpg' by boxes with dotted lines.
All other scripts and data sources contain sensitive
information.

This directory thus contains the following scripts:
- hh_atttributes_data_gen: the generation of household social demographic variables
- hwb_data_gen: the generation of household wellbeing variables
- spatial_data_gen: the generation of settlement level population density, travel time and gdp variables
- landscape_data_gen: the generation of settle-level resource tenure system variables



