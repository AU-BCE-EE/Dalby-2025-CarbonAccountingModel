# Carbon Accounting Model 
This repository contains code and files for running the Carbon Accounting Model, which was developed as part of the Udvidet Normtal Project. The model is a tool for accounting the carbon loss and emission of methane and carbon dioxide from livestock animals and manure at farm scale. These projects that have contributed to the model development and optimization are listed below. 

Udvidet Normtal, PIGMET, MILK, klimagylle, metemis

## Background
The model is based on 1) A static calculation of enteric emission and digestiblity of organic matter and 2) a dynamic simulation of organic matter transformation in manure storages with the Anerobic Biodegradation Model (ABM). A thourough description of the governing equations for enteric emission, digestibility and exretion of organic matter is described for pigs in Lashkari et al. 2025 and for cattle it is described in XXX et al. 2025.
The predicted excretion of organic matter to the manure pits is used as input for the ABM model, which simulates organic matter transformation both in-house and outdoor storage. The model is available as an R-packge here: https://github.com/AU-BCE-EE/ABM and can be installed form the R-command: | `devtools::install_github('AU-BCE-EE/ABM', build_vignettes = T)`. Details on the ABM model equations and structure is described in several papers: Dalby et al. 2021, 2023a, 2023b, 2024. and in the R-packges documentation. This repositroy also contains code the models and give estimates for emission at the farm scale level and aggregated on the different sources of emission (enteric, in-house manure and outdoor manure), based on a single input spreadsheet file that defines the whole farm management. 

## Input file to setup farm scale management
A single spreadsheet file "main_sheet.xlsm" is found under /inputs. Under the sheets 'in-barn' and 'out-of-barn' the user can define the farm. The name of the input file can be changed to anything as long as the format remains 'xlsm'. Under 'in-barn', multiple sections can be choosen by changing the number under 'sections'. Under each section the manure management, mitigation technology, feeding ingredients, and connected outdoor tanks can be defined. The spreadsheet contains default values for all the input parameters as soon as the animal category and barn type and flooring has been selected. Under 'out-of-barn' sheet, the number and settings of outdoor storages can be defined. Under each storage, dimensions, mitigation technology, and manure emptying schedules can be set. Default manure removal patterns from the storages are available for pig and cattle and the numbers represent the fraction (%) of the yearly manure production that is removed during a particular month. The storage ID must match with the IDs defined in the 'in-barn' sheet in order to pass the manure from the correct section to the correct storage. 

## Running the model 
Once the input file is setup correctly and saved, the model can be called by running the "./run/main.R" script in R or by clicking the './run/run_ABM.bat' file. The './run/run_ABM.bat' file might require some editing depending on the directory of you repository and your R installation.

## Output from farm scale simulation
The output is contained in a spreadsheet file located here "./outputs/". The output file name will be identical to the input file and contains different sheets. The input variables given to the model are shown under the 'input_barn' and 'input_storage' sheets, emission of CH4, CO2, NH3 and N2O are given under 'emission_dat'. The sheets 'nutrient_dat' contains information about C and N loss, 'manure' contains information about the composition of the excreted manure, and 'feed' holds information about the composition of the feed. The sheet 'feed_spill' shows how much feed was spilled into the pits inside the barn. 

## Maintainer information
Frederik Rask Dalby. Contact information here: https://au.dk/fd@bce.au.dk

## References
Dalby, F.R., Hafner, S.D., Petersen, S.O., Vanderzaag, A., Habtewold, H., Dunfield, K., Chantigny, M.H., Sommer, S.G., 2021. A mechanistic model of methane emission from animal slurry with a focus on microbial groups. PLOS ONE. https://doi.org/10.1371/journal.pone.0252881

Dalby, F.R., Hansen, M.J., Guldberg, L.B., Hafner, S.D., Feilberg, A., 2023a. Simple Management Changes Drastically Reduce Pig House Methane Emission in Combined Experimental and Modeling Study. Environ. Sci. Technol. https://doi.org/10.1021/acs.est.2c08891

Dalby, F.R., Ambrose, H.W., Poulsen, J.S., Nielsen, J.L., Adamsen, A.P.S., 2023b. Pig slurry organic matter transformation and methanogenesis at ambient storage temperatures. JEQ. https://doi.org/10.1002/jeq2.20512

Dalby, F.R., Hafner, S.D., Ambrose, H.W., Adamsen, A.P.S., 2024. Pig manure degradation and carbon emission: Measuring and modeling combined aerobic–anaerobic transformations. JEQ. https://doi.org/10.1002/jeq2.20603






