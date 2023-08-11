## PRISMA & ReMAPP Monitoring Report
#### Last Updated: 8 August 2023

### Overview

Data monitoring is conducted on a weekly basis to ensure the data collected and processed by sites meets quality expectation, 
and to have an on-time look at the accrual of maternal and neonatal outcomes of interest. 

In the ultimate output report, the following contents are included:
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
   - notes: :heavy_exclamation_mark: data not included; ask for permission for synapse access and data use.

Data output: will update based on monitoring report updates and uploaed data by site

### File Structure

1. `1. Data_Import.R` imports raw data from network drive/synapse.
   - input: raw data uploaded by sites
   - output: `mnh00_merged.csv-mnh26_merged.csv` stored in directory `/merged_data/[folder named with upload date]`.
   
2. `2. Maternal_Data_Merge_Wide.R` merges all maternal forms and transforms them into wide format. 
   - input: `mnh00.csv-mnh26.csv` stored in directory `/cleaned_data/[folder named with upload date]`.
   - output: `MatData_Wide.Rdata` & `MatData_Wide_Visit.Rdata`
   - notes: MatData_Wide is a wide dataset with one row for each woman. MatData_Wide_Visit is a wide data with one row for each woman at each visit.
   
3. `Infant_Data_Merge_Wide.R` merges all infant forms and transforms them into wide format. 
   - input: `mnh00.csv-mnh26.csv` stored in directory `/cleaned_data/[folder named with upload date]`.
   - output: `InfData_Wide.Rdata` & `InfData_Wide_Visit.Rdata`
   - notes: InfData_Wide is a wide dataset with one row for each infant. InfData_Wide_Visit is a wide data with one row for each infant at each visit.

4. `3. Report_Setup.R` generates all the variables needed for the monitoring report. 
   - input: `Maternal_Data_Merge_Wide`, `Infant_Data_Merge_Wide`
   - output:
     - `MatData_Report.RData`
         - Only includes a subset of variables from the maternal wide dataset to be used for report purposes.
     - `InfData_Report.Rdata`: 
         - only includes a subset of variables from the infant wide dataset to be used for report purposes. 
     - `MatData_Screen_Enroll.RData`: 
         - Includes all women screen and enrolled. Includes constructed variables on enrollment and screening status.
     - `MatData_Anc_Visit.RData`: 
         - Includes all women who are enrolled. Includes constructed variables relevant to ANC. 
     - `MatData_Pnc_Visits.RData`:
         - Includes all women who are enrolled who have entered the PNC period. Includes constructed variables relevant to PNC.
     - `InfData_Pnc_Visits.RData`: 
         - Includes all infants. Includes constructed variables relevant to PNC period.
     - `MatData_Hb_Visit.RData`: 
         - Includes relevant constructed variables for ReMAPP tables looking at testing completion for Hb at each visit. (ReMAPP Table 3) 
     - `MatData_Hb_GA_Visit.RData`: 
         - Includes relevant constructed variables for ReMAPP tables looking at Hb test outcomes by gestational age. (ReMAPP Figure 2)
     - `healthyOutcome.RData`: 
         - Includes relevant constructed variables for ReMAPP healthy criteria.

6. `Monitoring_Report.Rmd` is the final report in R markdown and is in .html output format.;
`varNames_sheet.xlsx` lists all the key variable names, only update the names here.

