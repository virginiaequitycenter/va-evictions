_Repo last updated: 2022-01-28_

This repository contains code for cleaning and summarizing Virginia eviction data. The data are filings scraped from the [Virginia court system](www.courts.state.va.us) by [Ben Schoenfeld](https://github.com/bschoenfeld). Anonymized data are available through [virginiacourtdata.org](https://virginiacourtdata.org/). In our work, we use de-anonymized data, and accordingly, we do not currently include the data in this repository.

Contents:

- clean-eviction-data.R cleans and aggregates data from court data folders containing child tables (Cases.csv, Defendants.csv, etc.), and it exports two cleaned CSVs to a code-generated processed-data folder:
    - cases.txt: a comma-separated data file containing all cleaned cases from all years (with a `filing_year` variable delineating year)
    - cases_residential_only.txt: a comma-separated data file containing cleaned cases from all years, excluding cases tagged as having a non-residential primary defendant (`filing_year` again delineates year)
    - clean-eviction-data.R also exports a small text file, cleaning-notes.txt, containing information on the cleaning process (e.g., how many duplicate case records were identified and removed for each year; how many serial cases were identified and tagged for each year; etc.)

- non-residential-defendant-regex.R contains the regex pattern used for identifying non-residential defendants in the case data; it is called as part of clean-eviction-data.R (see `non_person_regex` [here](https://github.com/jacob-gg/manager) for more information and tests of the pattern)

- defuzz-locality-plaintiff-names.R contains code for "defuzzing" plaintiff names for a user-specified locality, thereby reducing the number of alternative spellings of the same name, misspellings, etc.; this process is done by fuzzy matching each plaintiff names to others listed across cases and picking from possible fuzzy matches
  - defuzz-bespoke-corrections.csv is a simple, two-column CSV in which a user can specific typo corrections that they want to be automatically applied in advance of the defuzzing (e.g., if "SQUARE" is repeatedly misspelled "SQUERE" in a locality's case data, it can be easier to correct that by using the CSV instead of relying on the defuzzing process)
  - non-residential-plaintiff-regex.R contains a regex pattern that used for separating non-residential and residential plaintiffs, as we apply a more-conservative defuzzing process to non-residential names than to residential ones

- summarize-eviction-data.R summarizes the cleaned data by VA court and by VA ZIP code; it exports:
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
