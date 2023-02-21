_Last updated: 2023-02-21_  

The [Virginia Evictors Catalog](https://virginiaequitycenter.shinyapps.io/va-evictors-catalog/) provides data on plaintiffs filing unlawful detainers (evictions) in Virginia's General District Courts.

This repository contains code for cleaning, standardizing, and aggregating case data and for building the app. Raw case data are provided by the non-profit [Legal Services Corporation](https://www.lsc.gov/). (An earlier version of this app used data gathered by [Ben Schoenfeld](https://github.com/bschoenfeld).)

Details about the data cleaning and standardization process are on the app's [_Data Notes_ page](https://virginiaequitycenter.shinyapps.io/va-evictors-catalog/) and are duplicated in the expandable section below. The process draws heavily on the [ECtools](https://github.com/virginiaequitycenter/ECtools) package, developed by UVA Library Research Data Services and The Equity Center.

<details><summary>Data cleaning and standardization process notes</summary><br/>
Case data are provided periodically by the Legal Services Corporation. Data on plaintiffs, defendants, hearings, etc. are provided separately; we aggregate all the data for a given case and identify a "primary" plaintiff name, plaintiff address, defendant name, and defendant address for each case based on the _first-listed_ plaintiff/defendant in each court record. We perform this step because many cases have multiple plaintiffs and/or defendants listed.<br><br>

Names in the case data have both formatting inconsistencies and errors. If left unaddressed, these would radically hamper our ability to identify multiple cases filed by the same defendant (e.g., "ABC REAL ESTATE CO" and "ABC REAL-ESTATE COMPANY" would be treated as separate plaintiffs). We apply several cleaning steps to standardize the data format and address common errors:

- We correct punctuation-spacing errors in names by ensuring that spaces do not precede but do follow commas, semicolons, and colons (e.g., "SMITH ,MARY" &#8594; "SMITH, MARY")

- We standardizing name formatting by:
  - Removing leading and trailing spaces in names
  - Converting dashes and forward slashes to single spaces
  - Eliminating the following characters: . ; ( ) [ ] { } # : _
  - Removing trailing commas at the ends of names
  - Converting "@"" signs and ampersands ("&") to "at" and "and" (and ensuring that spaces surround those strings)
  - Converting all instances of more than one space ("   ") to single spaces (" ")
  - (At the extreme, the name-standardization process means that both ` _MAGNOLIA-&-FIR_ #COMPANY#     L.L.C.,` and `{MAGNOLIA} /AND/ (F)(I)(R) [COMPANY]... LLC, ` can be identified as the same name.)


- We expand common housing related shorthands when identified in plaintiff and defendant names, which you can view [here](https://github.com/virginiaequitycenter/ECtools/blob/main/inst/extdata/housing.csv).

After the cleaning and standardization processes above, we then remove duplicate records by identifying cases that have the same filing date, plaintiff name, defendant name, defendant ZIP Code, judgment (outcome), judgment costs, attorney fees, and principal/other amounts. (We retain one record for each set of duplicate case.)

We then identify "serial cases," which we consider to be repeated cases filed by a given plaintiff against a given defendant in a given ZIP Code within a 12-month period.

We then identify and filter out _non-residential_ defendants by using a custom-developed regex pattern, as we display results in the app for cases against residential defendants only. You can view full regex pattern [here](https://github.com/jacob-gg/non-person-regex).

Cleaned data are then exported and aggregated up to the level of plaintiff, plaintiff/year, and plaintiff/month, which are the levels of summarization available for viewing in the app.

Code for the data cleaning and standardization process is in `clean.R`; code for aggregating cleaned data is in `summarize.R`; code for the app is in the `va-evictors-catalog` directory (see `app.R`).

</details>

The case records comprising the data reflected in the app are public; however, to protect defendants from being named against their will or wishes, we do not currently include the raw case data in this repository.

---

Full contributor acknowledgments are on the app's [_About the Project_ page](https://virginiaequitycenter.shinyapps.io/va-evictors-catalog/).

- Code: Jacob Goldstein-Greenwood,<sup>&#963;</sup> Michele Claibourn,<sup>&#9677;</sup> and Elizabeth Mitchell<sup>&#9677;</sup>
- Subject-matter expertise and conceptual guidance: Kate Howell,<sup>&#9708;</sup> Ben Teresa,<sup>&#9708;</sup> Barbara Brown Wilson,<sup>&#9677;</sup> Michele Claibourn,<sup>&#9677;</sup> Hannah Woehrle,<sup>&#9708;</sup> and Michael Salguiero<sup>&#9677; (formerly)</sup>
- Additional project support, coordination, and communications assistance: Hannah Woehrle,<sup>&#9708;</sup> Connor White,<sup>&#9708;</sup> Michael Salguerio,<sup>&#9677; (formerly)</sup> and Atticus Johnson<sup>&#9708;</sup>


&#9677; - [The Equity Center at UVA](https://virginiaequitycenter.org/)  
&#963; - [UVA Library Research Data Services](https://data.library.virginia.edu/)  
&#9708; - [RVA Eviction Lab at VCU](https://rampages.us/rvaevictionlab/)  
