# Housing Affordability Index Code 

Production code to create housing affordability index data for California counties (and cities).

## Index Basic

The index incorporates county/city income distribution, mortgage rates, and housing prices to determine what percentage of people in a particular place can afford to purchase a home based on their income and the maximum monthly mortgage that can be carried. 

Price data comes from internally ETLed and calculated MLS transaction data but can be used from any publicly aggregated source. 

The income distribution (and forecast) data is interpolated from the 5 year trend forecast, into yearly and quarterly data, but only gets updated once per year. 

The Index is calculated for regular buyers and first-time buyers who face a slightly different calculation of mortgage rates. As housing price data is revised from quarter to quarter, the process reads in previous quarter and previous year data checks for revisions and applies revision tags.

Data is ingested through a variety of excel sheets based on ETL processes and commercial output formats, and requires some cross-walks to harmonize data names for calculation, then re-translated and reformatted again into naming formats for news releases.

The data is exported in clean csvs, outputted for dashboards with specific ingestion requirements and placed into templated xlsx files.  

## Automating Reporting for News Releases 

For regions and counties, impacts and magnitudes of changes in affordability is automated and written out for each region and county along with some tricks to make the language natural for the changes. 

Some news rooms have experimented with this type of automated event reporting, but it has seen the most use in sports (in particular fantasy sports) event recaps. 

Automating numeric reporting reduces the tediousness of looking at basic point changes for many areas, while allowing some   

### Readme Todos
* add links to examples 
* add pictures / screenshots to readme
* add example to "glue" reporting trick which should have value for others

## Code Improvements TBD

Incorporate some thesaursus packages to draw from a list of randomized key words for language reasons.

Re-write calculation in a callable function: given that it gets re-run several times. Requires some work with non-standard evaluation to properly pass some dplyr and global environment names into a function environment. Not super tricky but haven't had a chance to implement yet.
