---
---
---

# ANSR

An R package to query ANSIS and massage the responses into various useful formats.

The [Australian National Soil Information System](https://ansis.net/) (ANSIS) brings together a range of publicly available soil profile data sets from across Australia. It delivers data in the form of a standardised JSON Schema preserving complex relationships within the data. This JSON data can be daunting to untangle.

ANSR does all the hard work of querying ANSIS and parsing the JSON response data into simpler formats - mainly CSV files.

![](images/clipboard-1441072419.png)

## How to Install the package

library(devtools)\
devtools::install_github("RossDSearle/ANSR", auth_token = "YourGitHubAuthToken")  
  
  

## About ANSR

The Australian National Soil Information System (ANSIS) aggregates and delivers soil data from governments, research and natural resource management organisations, industry, farmers, and the community. To achieve this, data managed by multiple providers in different ways and for different reasons must be harmonised. A common information model to which the provider data can be mapped is therefore required. This model must be consistent with soil scientists’ understanding of soils and their nationally agreed practices for sampling and describing them.

The ANSIS information model was developed to deliver the soil data concepts described in the [Australian Soil and Land Survey Field Handbook](https://www.publishing.csiro.au/book/8129/). The ANSIS information model is described in detail [here](https://github.com/ANZSoilData/def-au-schema-json).

ANSIS data can be queried and downloaded from the ANSIS [Data Portal](https://portal.ansis.net/). Data from the ANSIS Portal is delivered to the user as JavaScript Object Notation ([JSON](https://en.wikipedia.org/wiki/JSON)) which allows it to maintain the complex relational data objects inherent in the Schema.

While the information model is a powerful tool, it can be tricky for us mere mortals to access end use the required bits of soil data we are chasing from with the Schema.

This is where ANSR comes in. [R](https://www.r-project.org/) is a data centric programming language widely used by scientists, especially in the soil science domain. The ANSR package allows R users to query the ANSIS data directly from R, download the JSON query responses and reformat the JSON data into simpler data analysis ready formats.

Before you can use the ANSR package you will need to create an [ANSIS](https://portal.ansis.net/) user account.

## Disclaimer

ANSR may or may not be supported into the future and no guarantees are given as to data quality, so use with caution. All the disclaimers of ANSIS also apply to the ANSR package.

Data and other content made accessible via ANSIS is provided by multiple providers, harmonised and made available on an ‘as is’ basis. If you use or download any data or other material made accessible by ANSIS you do so at your own risk and you acknowledge that such data may be incomplete or not applicable to all situations.

## Contacts

Contact the [ANSIS help desk](https://ansis.net/contact/) with any questions about ANSIS or ANSIS data

Contact [Ross Searle](href="ross.searle@csiro.au") for any issues with the ANSR package.
