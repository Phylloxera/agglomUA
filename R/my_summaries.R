#' Read an urban area shapefile
#'
#' @param wd a directory containing the shapefile
#' @param file the file name
#'
#' @return a sf
#' @export
#'
#' @examples
#' #not run: `shapeUA("~", "ua99_d00.shp")`
shapeUA <- function(wd, file) {cd <- getwd(); setwd(wd); ua <- sf::read_sf(file); setwd(cd); return(ua)}
#' Read an urban area metadata file
#'
#' @param wd a directory containing the excel metadata file
#' @param mfile the file name
#'
#' @return a data frame
#' @export
#'
#' @examples
#' #not run: `metaUA("~", "ua_comb00_natl_corr.xlsx")`
metaUA <- function(wd, mfile) {cd <- getwd(); setwd(wd); ua_meta <- readxl::read_excel(mfile); setwd(cd); return(ua_meta)}
#' Merge a compatible urban area sf with its matching metadata data frame
#'
#' @param wd a directory containing the shapefile and excel metadata file
#' @param file the shapefile name with extension shp
#' @param mfile the metadata file with an excel extension (e.g. xlsx or xls)
#'
#' @return a sf
#' @export
#'
#' @examples
#' #not run: `mergeUA("~", "sf_2000_reconcile.shp", "sf_2000_reconcile.xlsx")`
mergeUA <- function(wd, file, mfile){cd <- getwd(); setwd(wd); ua <- sf::read_sf(file); checksf(ua); ua_meta <- readxl::read_excel(mfile)
  uauacol <- which(colnames(ua) %in% c("UACE", "UA", "UACE10", "UACE20", "UACE30", "Code"))
  uanamecol <- which(colnames(ua) %in% c("NAME", "Name", "NAME10", "NAME20", "NAME30"))
  metuacol <- which(colnames(ua_meta) %in% c("UACE", "UA", "UACE10", "UACE20", "UACE30", "Code"))
  metnamecol <- which(colnames(ua_meta) %in% c("NAME", "Name", "NAME10", "NAME20", "NAME30"))
  popcol <- which(colnames(ua_meta) %in% c("POP", "Population")); areacol <- which(colnames(ua_meta) %in% c("AREALANDSQMI", "Area(sq meters)"))
  if(colnames(ua_meta)[areacol] == "Area(sq meters)") {ua_meta[[areacol]] <- round(ua_meta[[areacol]] / 2589988, 7)}
  popdencol <- which(colnames(ua_meta) %in% c("POPDEN", "PopulationDensity")); metothercols <- setdiff(1:ncol(ua_meta), c(metuacol, metnamecol, popcol, areacol, popdencol))
  ua <- ua[, c(uauacol, uanamecol)]; ua_meta <- ua_meta[, c(metuacol, metnamecol, popcol, areacol, popdencol, metothercols)]; names(ua)[1:2] <- c("UACE", "NAME")
  names(ua_meta)[1:5] <- c("UACE", "NAME", "POP", "AREALANDSQMI", "POPDEN"); ua3 <- dplyr::left_join(ua, ua_meta); ua3$EABCOMBINED <- character(nrow(ua3))
  ua3$UACES <- character(nrow(ua3)); ua3$NEARNEIGHBOR <- character(nrow(ua3)); ua3$NEIGHBORDIST <- double(nrow(ua3)); ua2 <- ua3[, c(1:2, 4:ncol(ua3), 3), ][, 1:ncol(ua3)]
  setwd(cd); return(ua2)}
#' check the coordinate reference system of a sf
#'
#' @param ua a sf
#'
#' @return an error if the sf lacks a coordinate reference system
#' @export
#'
#' @examples
#' data("ua2000"); class(ua2000)
#' #will produce error (not run) `checksf(ua2000)`
checksf <- function(ua) {if(is.na(sf::st_crs(ua))) {stop(paste0("Input file has no coordinate reference system. You must set one with the st_set_crs() command before combining",
  " urban areas. The crs might be 4269 like in 2020 and 2010 US Censuses."))}}
#' check the format of an urban area sf and an urban area metadata data frame
#'
#' @param ua a sf
#' @param ua_meta a data frame
#'
#' @return an error if a formatting error is found
#' @export
#'
#' @examples
#' data("ua2000"); data("meta2000"); class(ua2000); class(meta2000)
#' #will produce error (not run) `checkuaformat(ua2000, meta2000)`
checkuaformat <- function(ua, ua_meta) {if (ncol(ua) != 3) {stop(paste0("Error in input shapefile, it should have urban area name and code columns."))}
  if (ncol(ua_meta) < 5) {stop(paste0("Error in input mfile. Ensure character columns for urban area code and name are present as well as numeric columns for population, ",
                                      ", and population density."))}
  if (all(unlist(sapply(ua_meta, class))[1:5] != c("character", "character", "numeric", "numeric", "numeric"))) {
    stop(paste0("Error in input mfile. Ensure urban area code and name columns are like <chr> 05275 <chr> Barrow, AK and population area, and population density columns are ",
                "like <dbl> 17348 <dbl> 27222610 <dbl> 1650."))}; if (all(unlist(sapply(ua, class))[1:2] != c("character", "character"))) {
    stop(paste0("Error in input shapefile. Ensure urban area code and name columns are like <chr> 05275 <chr> Barrow, AK."))}}
#' check for urban area name and uace code matching between urban area sf and an urban area metadata data frame
#'
#' @param ua a sf
#' @param ua_meta a data frame
#'
#' @return an error if a synonymy error is found
#' @export
#'
#' @examples
#' data("ua2000"); data("meta2000"); class(ua2000); class(meta2000)
#' #will produce error (not run) `checkuasynonymy(ua2000, meta2000)`
checkuasynonymy <- function (ua, ua_meta) {if(all(unique(ua_meta$UACE) %in% unique(ua$UACE)) == FALSE || all(unique(ua$UACE) %in% unique(ua_meta$UACE)) == FALSE) {
    stop(paste0("Detected mismatch between input file & mfile. Run: setdiff(unique(ua$UACE), unique(ua_meta$UACE)) to find urban area ID values unique to your input file and",
                " setdiff(unique(ua_meta$UACE), unique(ua$UACE)) to find urban area ID values unique to your input mfile. You will need compatible input files before ",
                "combining urban areas"))}}
#' check if an urban area sf is multipolygon formatted
#'
#' @param ua a sf
#'
#' @return an error if not a multipolygon formatted sf
#' @export
#'
#' @examples
#' data("ua2000"); class(ua2000)
#' #will produce error (not run) `checkuapoly(ua2000)`
checkuapoly <- function(ua) {if(length(unique(ua$UACE)) != nrow(ua)) {
    stop(paste0("Too many features file. You will need a multi polygon shapefile with a single row per urban area ID value before combining urban areas"))}}
#' return the number of decimal places
#'
#' @param x a number
#'
#' @return an integer
#' @export
#'
#' @examples
#' decimalplaces(5.678)
decimalplaces <- function(x) {if (abs(x - round(x)) > .Machine$double.eps^0.5) {nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed = T)[[1]][[2]])} else {return(0)}}
#' Union a merged urban area sf (see ?mergeUA) when neighbor features are separated by less than a combine distance in miles
#'
#' @param ua a merged sf
#' @param cdist a combine distance (in miles)
#'
#' @return a sf
#' @export
#'
#' @examples
#' #none
unionUA <- function(ua, cdist) {if (!('NEARNEIGHBOR' %in% colnames(ua))) {stop("Error: run mergeUA prior to running unionUA")}; if (all(unique(ua$NEARNEIGHBOR) != "")) {
  stop("Error: run mergeUA prior to running unionUA")}; cdist <- cdist * 1609.344; gcol <- ncol(ua); mcol <- gcol - 1; xcol <- c(gcol + 1, gcol + 2)
  ua$DIST <- double(nrow(ua)); ua$UACE2 <- character(nrow(ua)); ua2 <- ua[, c(1:mcol, xcol, gcol), ][, 1:ncol(ua)]; ua <- ua2; union_vector <- character(); estprog <- 1
  dist_df <- head(data.frame(ua[, c("UACE", "DIST", "UACE2")]), n = 0)[1:3]; st <- Sys.time(); while (!(ua$UACE[1] %in% dist_df$UACE2)) {
  if (length(unique(dist_df$UACE2)) > (0.01 * nrow(ua)) && estprog == 1) {et <- Sys.time(); pt1 <- round(as.double(difftime(et, st, units = "mins")), 2)
  pt2 <- round((pt1 * 100 - pt1) / 60, 2); print(paste0("unionUA 1% complete after ", pt1, " minutes. unionUA projected to complete in another ", pt2, " hours."))
  estprog <- 2}; ua$DIST <- as.vector(sf::st_distance(ua[ua$UACE == ua$UACE[1], ], ua)); ua$UACE2 <- ua$UACE[1]; distrows1 <- nrow(dist_df)
  dist_df <- rbind(data.frame(ua[ua$DIST <= cdist, c("UACE", "DIST", "UACE2")])[, 1:3], dist_df); distrows2 <- nrow(dist_df)
  nn <- data.frame(ua[ua$DIST > cdist, c("UACE", "DIST", "UACE2")])[1:3]; nn <- nn[which.min(nn$DIST), ]; names(nn) <- c("nn_UACE","DIST","UACE")
  distrows <- distrows2 - distrows1; if (distrows > 1) {ids2check <- setdiff(dist_df$UACE[1:distrows], ua$UACE[1]); union_vector <- c(ua$UACE[1], ids2check[1])
  union_vector <- union_vector[!is.na(union_vector)]; while(length(ids2check) > 0) {ua$DIST <- as.vector(sf::st_distance(ua[ua$UACE == ids2check[1], ], ua))
  ua$UACE2 <- ids2check[1]; dist_df <- rbind(data.frame(ua[ua$DIST <= cdist, c("UACE", "DIST", "UACE2")])[, 1:3], dist_df)
  nn2 <- data.frame(ua[ua$DIST > cdist, c("UACE", "DIST", "UACE2")])[1:3]; nn2 <- nn2[which(!(nn2$UACE %in% union_vector)), ]; nn2 <- nn2[which.min(nn2$DIST), ]
  names(nn2) <- c("nn_UACE", "DIST", "UACE"); nn <- rbind(nn, nn2); nn <- nn[which(!(nn$nn_UACE %in% union_vector)), ]; distrows2 <- nrow(dist_df)
  distrows <- distrows2 - distrows1; ids2check <- unique(setdiff(dist_df$UACE[1:distrows], union_vector)); union_vector <- c(union_vector, ids2check[1])
  union_vector <- union_vector[!is.na(union_vector)]}; u <- sf::st_sf(sf::st_union(ua[ua$UACE %in% union_vector, ])); nn <- nn[which(!(nn$nn_UACE %in% union_vector)), ]
  nn <- nn[which.min(nn$DIST), ]; sf_remain <- ua[!(ua$UACE %in% union_vector), ]; df <- ua[ua$UACE %in% union_vector, ]; df <- df[order(df$POP), ]
  df$EABCOMBINED <- paste(df$NAME[1:(nrow(df) - 1)], collapse = "; "); df$UACES <- paste(df$UACE[1:(nrow(df) - 1)], collapse = ", "); df$NEARNEIGHBOR <- nn$nn_UACE[1]
  df$NEIGHBORDIST <- round(nn$DIST[1] / 1609.344, 2); if (which(colnames(df) == "EABCOMBINED") == 6) {othercols <- double()} else {
  othercols <- 6:(which(colnames(df) == "EABCOMBINED") - 1)}; if (length(othercols) > 0) {df2 <- data.frame(head(df, n=0)[, c("POP", "AREALANDSQMI", "POPDEN")])[,1:3]
  for (i in 1:3) {df2[1, i] <- sum(df[[colnames(df2[i])]])}; df3 <- data.frame(head(df, n = 0)[, othercols])[,1:length(othercols)]; for (i in 1:ncol(df3)) {
  if (is.numeric(df[[othercols[i]]])) {df3[1, i] <- sum(df[[othercols[i]]])} else {df3[1, i] <- df[[othercols[i]]][nrow(df)]}}; df4 <- head(ua, n = 0)
  df4[1, ] <- c(data.frame(df[nrow(df), c("UACE", "NAME")])[, 1:2], df2, df3, data.frame(df[nrow(df), which(colnames(ua) == "EABCOMBINED"):ncol(df)])[, 1:6], u)
  ua <- rbind(sf_remain, df4)} else {df2 <- data.frame(head(df, n = 0)[, c("POP", "AREALANDSQMI", "POPDEN")])[,1:3]; for (i in 1:3) {df2[1, i] <- sum(df[[colnames(df2[i])]])}
  df4 <- head(ua, n = 0); df4[1, ] <- c(data.frame(df[nrow(df), c("UACE", "NAME")])[, 1:2], df2, data.frame(df[nrow(df), 6:ncol(df)])[, 1:6], u); ua <- rbind(sf_remain, df4)}
  union_vector <- character()} else {ua$NEARNEIGHBOR[1] <-  nn$nn_UACE[1]; ua$NEIGHBORDIST[1] <- round(nn$DIST[1] / 1609.344, 2); ua$EABCOMBINED[1] <- NA; ua$UACES[1] <- NA
  ua <- ua[c(2:nrow(ua), 1), ]}; }; ua$POPDEN <- round(ua$POP / ua$AREALANDSQMI, 2); ua$UACE2 <- NULL; ua$DIST <- NULL; ua$POPDEN <- round(ua$POP / ua$AREALANDSQMI, 2)
  ua$UACE2 <- NULL; ua$DIST <- NULL; for (j in 4:(ncol(ua) - 1)) {if (is.numeric(ua[[j]])) {dec_vec <- integer(); for (i in 1:nrow(ua)) {
  dec_vec[i] <- decimalplaces(ua[[j]][i])}; rlev <- max(dec_vec); if (rlev > 2) {ua[[j]] <- round(ua[[j]], 2)}}}
  for (i in which(ua$NEARNEIGHBOR %in% setdiff(ua$NEARNEIGHBOR, ua$UACE))) {rep <- ua$UACE[grep(ua$NEARNEIGHBOR[i], ua$UACES)]; ua$NEARNEIGHBOR[i] <- rep}
  for (i in 1:nrow(ua)) {rep <- ua$NAME[grep(ua$NEARNEIGHBOR[i], ua$UACE)]; ua$NEARNEIGHBOR[i] <- rep}; return(ua)}
#' apply a gravitational model to a unioned (see ?unionUA) urban area sf
#'
#' @param ua a unioned sf
#'
#' @return a sf
#' @export
#'
#' @examples
#' #none
gmUA <- function(ua) {if (!('NEARNEIGHBOR' %in% colnames(ua))) {stop("Error: run mergeUA then unionUA prior to running gmUA")}; if (all(unique(ua$NEARNEIGHBOR) == "")) {
  stop("Error: run unionUA prior to running gmUA")}; ua$GRAVPULL <- double(nrow(ua)); for (i in 1:nrow(ua)) {
  ua$GRAVPULL[i] <- (as.numeric(ua[which(ua$NAME %in% ua$NEARNEIGHBOR[i]), 3])[1] * as.numeric(ua[i, 3])[1]) / (as.numeric(ua[1, "NEIGHBORDIST"])[1] ^ 2)}
  ua <- ua[!duplicated(ua$GRAVPULL), ][, c("NAME", "POP", "NEARNEIGHBOR", "NEIGHBORDIST", "GRAVPULL")]; return(ua)}
