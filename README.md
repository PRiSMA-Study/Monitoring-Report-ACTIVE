## PRiSMA & ReMAPP Monitoring Report


### Overview

Data monitoring is conducted on a regularly basis to ensure the data collected and processed by sites meets quality expectation, 
and to have an ontime look at the accrual of maternal and neonatal outcomes of interest. 

In the ultimate output report, the following contents are included (tentative plan):
- Pre-screening numbers for PRiSMA MNH study for most recent one week and for all (cumulative)
- Enrollment numbers for PRiSMA MNH Study for the most recent one week and for all (cumulative)
- Study Status for PRiSMA Participants
- ANC study visit & procedure completion metrics
- Birth characteristics
- PNC study visit & procedure completion
- Maternal and neonatal outcomes
- ReMAPP healthy cohort eligibility
- Proportion of women eligible for ReMAPP healthy cohort by eligibility criteria
- Hemoglobin measurements for participants in ReMAPP per visit
- Hemoglobin descriptive statistics for participants in ReMAPP cohort
- Cumulative Enrollment by Site and Week
- Hemoglobin measures by gestational age for participants enrolled in ReMAPP
- ANC hemoglobin values by trimester for participants enrolled in ReMAPP
- Potential risk factors for anemia among women enrolled in PRiSMA MNH study



### Data Input and Output

Data input: sites data will be uploaded and stored in synapse for now and in AWS later. 
   - notes: :heavy_exclamation_mark: data not included in github


Data output: As of 2023-01-12, 1 merged data and 3 derived data sets are created to generate the final report.

- matData.rda

- matOutcome.rda
- healthyOutcome.rda
- statusOutcome.rda

- Monitor_Report.Rmd

### File Structure

1. `data_import.R` imports data from synapse.
   - input: data uploaded by sites in synapse.
   - output: `mnh01.csv-mnh26.csv` stored in directory `/cleaned_data/`.
   
2. `data_merge.R` extracts key variables from each of the 25 MNH forms, merge extracted variables across the forms.
   - input: `mnh01.csv-mnh26.csv` stored in directory `/cleaned_data/`.
   - output: `derived_data/matData.rda`, `derived_data/neoData.rda`, `derived_data/dataMerged.rda` (a combined maternal data and a combined neonatal data). 
   - notes: :heavy_exclamation_mark: we only have `matData.rda` so far;
   `varNames_sheet.xlsx` lists all the key variable names, only update the names here.
   
3. `data_materanl.R` generates the core maternal outcomes.
   - input: `derived_data/matData.rda` 
   - output: `derived_data/matOutcome.rda`
   - notes: :heavy_exclamation_mark: double check the algorithm is consistent with the definition; 

4. `data_healthy.R` generates the criteria variables of healthy cohort.
   - input: `derived_data/dataMerged.rda`, `derived_data/matOutcome.rda`
   - output: `derived_data/healthyOutcome.rda`
   - notes: :heavy_exclamation_mark: current code only considers data collected at the enrollment/first ANC visit.
   
5. `data_dashboard.R` generates the monitoring variables (pre-screening, enrollment, study status, etc.).
   - input: `derived_data/dataMerged.rda`, `derived_data/matOutcome.rda`
   - output: `derived_data/statusOutcome.rda`.
   
6. `Monitor_Report.Rmd` is the final report in R markdown and is in .html output format. `report_helpers.R` includes helper functions of the report.
