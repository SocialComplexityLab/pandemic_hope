This folder contains reproduction materials for 
"Communicate “Hope” to Motivate Action Against Infectious COVID-19 Variants"

maintained by Alexander Bor alexander.bor@ps.au.dk 

File name structure:

- `A-original-data` These are not shared on Github due to containing potentially sensitive, and definitely irrelevant data
  - `B117_experiment.dta` Raw experimental data from YouGov
  - `B117_observational.rds` Raw observational data from Epinion
- `B-analysis-data` 
  - `B117_experiment_clean.rds` Cleaned experimental data
  - `B117_observational_clean.rds` Cleaned and subsetted observational data
- `C-code` 
  - `analysis.R` Code to reproduce all analyses and figures
- `D-documents` 
  - `figures`
    - `fig1_a.jpg` Illustration for Hope condition
    - `fig1_b.jpg` Illustration for Threat condition
    - `fig1_c-h.jpg` Experimental effects reported in Fig 1 
    - `fig1_combine.pxm` Pixelmator file to combine panels a + b + c-h
    - `fig2.jpg` Observational effects reported in Fig 2
  - `tables`        
    - `experiment_results.html` Regression table Marginal effects of experiment
    - `experiment_contrasts.html` Regression table with contrast between hope and threat
    - `hope-vs-fear.html` Regression table contrasting hope vs fear against other outcomes  
- `E-metadata` 
    - `B117 experiment questionnaire.docx` Questionnaire of experiment
    - `B117 experiment weighting scheme` Sample characteristics
- `Paper Mutation.Rproj` an R project to effectively navigate this folder` 
