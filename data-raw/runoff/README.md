The runoff files here are direct from flumes. 

These files must have the first 7 rows as metadata with 
the 7th row being a header row with the following column names: 
date_time, level, flow, bottle, and sampleID. 
These columns should be in the follow units.

  - date_time: in MM/DD/YYYY HH:MM:SS format
  - level: in meters 
  - flow: (gallons per minute)
  - bottle: indicator of sample taken (0 indicates yes and NA indicates no)
  - sampleID: (relates to water_quality data)

*MAKE SURE UNITS OF MEASUREMENT MATCH THE DESCRIPTIONS ABOVE