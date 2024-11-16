:: Example Windows batch file for proposed ABM approach
:: some_R_script.R would take input data from Excel file, run ABM, and produce an output file
:: If the user needs to specify a file name, we should be able to add input/output file names as parameters to this batch file

@echo off
:: Batch file to run main.R using Rscript

:: Get the directory of the batch file
set SCRIPT_DIR=%~dp0

:: Define the relative path to main.R
set R_SCRIPT=%SCRIPT_DIR%..\ABM_carbon_accounting_git\run\main.R

:: Run the R script using the discovered Rscript.exe
"C:\Program Files\R\R-4.3.1\bin\Rscript.exe" "%R_SCRIPT%"