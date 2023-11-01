#' US Census Bureau 2000 Census Urban Area Shapefile sf
#'
#' A sf containing the 2000 US Census urban areas. Obtained from
#'  https://www.census.gov/geographies/mapping-files/time-series/geo/carto-boundary-file.2000.html#list-tab-1556094155
#'  on Aug 20, 2023. The variables are as follows:
#'
#' @format A sf with 11880 rows and 9 variables:
#' \describe{
#'   \item{AREA}{AREA}
#'   \item{PERIMETER}{PERIMETER}
#'   \item{UA99_D00_}{UA99_D00_}
#'   \item{UA99_D00_I}{UA99_D00_I}
#'   \item{UA}{the urban area UACE code}
#'   \item{NAME}{the urban area name}
#'   \item{LSAD}{LSAD}
#'   \item{LSAD_TRANS}{LSAD_TRANS}
#'   \item{geometry}{the urban area feature geometry}
#' }
#' @usage data("ua2000")
"ua2000"
#' US Census Bureau 2000 Census Urban Area Metadata data frame
#'
#' A data frame containing the 2000 US Census urban area metadata. Obtained from
#'  https://www.census.gov/programs-surveys/geography/guidance/geo-areas/urban-rural/2000-urban-rural.html
#'  on Aug 20, 2023. August 23, 2002 Changes Alphabetically-sorted corrected National list of UAs was
#'  combined with November 20, 2002 Changes Alphabetically-sorted corrected National list of UCs.
#'  The variables are as follows:
#'
#' @format A data frame with 3637 rows and 5 variables:
#' \describe{
#'   \item{Code}{the urban area UACE code}
#'   \item{Name}{the urban area name}
#'   \item{Population}{the urban area population}
#'   \item{Area(sq meters)}{the area of the urban area}
#'   \item{PopulationDensity}{the urban area population density per square mile}
#' }
#' @usage data("meta2000")
"meta2000"