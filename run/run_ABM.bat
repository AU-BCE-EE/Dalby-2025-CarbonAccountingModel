:: Example Windows batch file for proposed ABM approach
:: some_R_script.R would take input data from Excel file, run ABM, and produce an output file
:: If the user needs to specify a file name, we should be able to add input/output file names as parameters to this batch file

:: Get the directory of the current batch file
set SCRIPT_DIR=%~dp0

:: Define the relative path to the R script
set R_SCRIPT=%SCRIPT_DIR%..\ABM_carbon_accounting_git\run\main.R

:: Prepare input data and run ABM
"C:\Program Files\R\R-4.2.2\bin\Rscript.exe" 

