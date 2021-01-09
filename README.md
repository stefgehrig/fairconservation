# fairconservation
R Code and data for reproducing the Appendix, analysis and figures for the article:  
 
Loft, L., Gehrig, S., Salk, C., & Rommel, J. (2020). Fair payments for effective environmental conservation. *Proceedings of the National Academy of Sciences, 117*(25), 14094–14101. https://doi.org/10.1073/pnas.1919783117
  
*May 2020*

## Overview of repository

This directory contains **two major R scripts:**  
  

- `supplementary_information.Rmd`
- `figures.R`  
  
The first produces the full Supplementary Information file (Appendix) with all text, tables and figures, knitted as PDF in R Markdown. It also contains a (non-printed) section with the few analyses that are exclusively presented in the main text. The second script recreates all data-based figures of the main text.  
  
All other R scripts in this directory are **helper scripts** which get loaded into the major scripts upon execution. Apart from the helper R files, **other helper files** that get loaded into the script "supplementary_information.Rmd" concern references and their formatting:  
  
- `cite_style.csl`
- `references.bib`

...as well as four image files that contain no data (pictures of field work and experimental materials).  
  
Three **data files** are contained in this repository:

- `exp_data.csv` (all individual-level experimental and survey data collected in the field)  
- `vil_data.csv` (all administrative village-level information)  
- `wvs_data.csv` (a country ranking on the World Values Survey question on expectation to be treated fair "A168A")  
  
The script `supplementary_information.Rmd` uses all three data files, whereas the script `figures.R` uses only the first one.  
  
A **data dictionary** for the major data sheet `exp_data.csv` is contained in the file
  
- `exp_data_dictionary.xlsx`
  
Further details on all questionnaire-based variables can be found in the Supplementary Information where the original questionnaire is shown.  
  
For remarks, errors or questions, please contact *stefan-gehrig[at]t-online.de*.
