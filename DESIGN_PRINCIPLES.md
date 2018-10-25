# Design principles

In the development of a version 1.0 pxweb package we will have the following principles:

1. The package should work for pxweb api 2017 v1 and later.

2. The package should follow the tidyverse best practice
https://cran.r-project.org/web/packages/httr/vignettes/api-packages.html

3. Minimize the dependencies. 
Only use httr (for API connections), jsonlite (for handling JSON objects) and dplyr (for processing data efficiently). All other dependencies should try to be removed or set as suggested.

4. Objects
To modularize and have low maintenance the package should be built upon the following objects. These are defined using S3 to minimize dependencies. Based on these objects we can all add the structures we need and make specific wrappers.

### The pxweb_s3_api object 
The pxwebapi object contain all information to do calls to the pxweb api and count the calls. It will not make to big calls (so big queries need to be split up before). All calls should be done using the object. This corresponds to the "base url" of the api. It already exista a pxweb_api object that should be deprecated.

As an example the api_catalogue() function just list different api urls, the configs are set by the constructor, unlike today.

### The pxweb_s3_table_metadata object 
The metadata object return by the api in a json-list format.

### The pxweb_s3_content object 
The levels object return by the api in a json-list format and the database object returned (but stored in the same way as a levels object).

### The pxweb_s3_query object 
This is the object that is send as a query to get an answer back. This is essentially a json-list object with the query.

### The pxweb_s3_table object 
This is the object (in JSON format) that is returned by the API (response table).






