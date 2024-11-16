# ABM_carbon_accounting_git
## Carbon accounting model developed in the Udvidet Normtal Project.
This repository contains code and files for runing the farm scale carbon accounting model developed in the "Udvidet normtal" project. 

# Background
The model is based on 1) A static calculation of enteric emission and digestiblity of organic matter and 2) a dynamic simulation of organic matter transformation in manure storages with the Anerobic Biodegradation Model (ABM). A thourough description of the governing equations for enteric emission, digestibility and exretion of organic matter is described for pigs in Lashkari et al. 2025 and for cattle it is described in XXX et al. 2025.
The predicted excretion of organic matter to the manure pits is used as input for the ABM model, which simulates organic matter transformation both in-house and outdoor storage. The model is available as an R-packge here: https://github.com/AU-BCE-EE/ABM and can be installed form the R-command: devtools::install_github('AU-BCE-EE/ABM', build_vignettes = T). Details on the ABM model equations and structure is described in several papers: Dalby et al. 2021, 2023, 2024, 2025. and in the R-packges documentation. This repositroy also contains code the models and give estimates for emission at the farm scale level and aggregated on the different sources of emission (enteric, in-house manure and outdoor manure), based on a single input spreadsheet file that defines the whole farm management. 

# Input file to setup farm scale management
A single spreadsheet file "main_sheet.xlsm" is found under /inputs. Under the sheets "in-barn" and "out-of-barn" the user can define the farm. Under 'in-barn', multiple sections can be choosen by changing the number under 'sections'. Under each section the manure management, mitigation technology, feeding ingredients, and connected outdoor tanks can be defined. The spreedsheet contains default values for all the input parameters as soon as the animal category and barn type and flooring has been selected. Under 'out-of-barn' the dimensions and number of outdoor storages can be defined. Under each storage, dimensions, mititgation technology, and emptying schedules can be defined. Default manure removal patterns from the storage are available for pig and cattle and the numbers represent the fraction (%) of the yearly manure production that is removed during a particular month. The storage ID must match with the IDs defined in the 'in-barn' sheet in order to pass the slurry from the correct section to the correct storage. 

# Running the model 
Once the "main_sheet.xlsx" is setup correctly and saved. The model can be called by running the "./run/main.R" script in R or by clicking the './run/run_ABM.bat' file. The './run/run_ABM.bat' file might require some editing depending on the directory of the R file.

# Output from farm scale simulation
The output file is a spreedsheet located here "./outputs/main_sheet.xlsx". The files contains different sheets with first inputs given to the model under " 




