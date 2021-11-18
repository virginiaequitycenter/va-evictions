_Repo last updated: 11-18-2021_

This repository contains code for cleaning and summarizing Virginia eviction data. The data are filings scraped from the [Virginia court system](www.courts.state.va.us) by [Ben Schoenfeld](https://github.com/bschoenfeld). Anonymized data are available through [virginiacourtdata.org](https://virginiacourtdata.org/). In our work, we use de-anonymized data, and accordingly, we do not currently include the data in this repository.

Contents:

- `clean-eviction-data.R` cleans and aggregates data from court data folders containing child tables (Cases.csv, Defendants.csv, etc.), and it exports two cleaned CSVs where 1 row = 1 case
    - cases.csv: a data file containing all cleaned cases from all years (with a `year_filed` variable delineating year)
    - cases_residential_only.csv: a data containing cleaned cases from all years excluding those cases tagged as having a non-residential primary defendant (`year_filed` again delineates year)
    - `clean-eviction-data.R` also exports a small text file, `cleaning-notes.txt`, containing information on the cleaning process (e.g., how many duplicate case records were identified and removed for each year, etc.)

- `non-residential-regex.R` contains the regex pattern used for identifying non-residential defendants in the case data; it is called as part of `clean-eviction-data.R` (see `non_person_regex` [here](https://github.com/jacob-gg/manager) for more information and tests of the pattern)

- `summarize-eviction-data.R` summarizes the cleaned data by VA court and by VA ZIP code; it exports:
    - Four _long_ CSVs where 1 row = 1 court/ZIP code for _one year_
        - by_court.csv contains court-wise summary data based on cases.csv (all cases), with a `year` variable delineating year
        - by_zip.csv contains ZIP-wise summary data based on cases.csv (all cases), with a `year` variable delineating year
        - by_court_residential_only.csv contains court-wise summary data based on cases_residential_only.csv (cases with residential defendants), with a `year` variable delineating year
        - by_zip_residential_only.csv contains ZIP-wise summary data based on cases_residential_only.csv (cases with residential defendants), with a `year` variable delineating year
    - Two _wide_ CSVs where 1 row = 1 court/ZIP code for _multiple years_; these CSVs contain additional showing *percent change* year to year in filings, evictions, and defaults
        - by_court_wide.csv
        - by_court_residential_only.csv

Team:  
Barbara Wilson &#9677;  
Benjamin Teresa &#9708;  
Connor White &#9708;  
Hannah Woehrle &#9708;  
Jacob Goldstein-Greenwood &#9677; (code author)  
Kathryn Howell &#9708;  
Michael Salgueiro &#9677;  
Michele Claibourn &#9677;  

&#9708; - [RVA Eviction Lab](https://rampages.us/rvaevictionlab/)  
&#9677; - [UVa Equity Center](https://virginiaequitycenter.org/)
