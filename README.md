# agglomUA
A set of R code blocks to approximate US Census Bureau "larger urban agglomerations"
## setup
1. An urban area [shapefile](https://www.census.gov/cgi-bin/geo/shapefiles/)
1. The sf package
```r
#if sf is not already installed:
install.packages("sf")
```
3. A distance in meters (urban area features are agglomerated if separated by this distance or less)
## run
In the [/demo](/demo/) directory, I have an example R script
