#variables
fp <- <path to shapefile>
file <- <name of file>
cdist <- <threshold distance for combining neighbor features>

#for example, on windows
fp <- "GeoSpatial\\Urban"; file <- "tl_2020_us_uac20"; cdist <- 100

#code block 1 (runs in seconds)
{library(sf); ua <- read_sf(dsn = fp, layer = file); union_vector <- character()
  i <- 1; ua$dist <- double(nrow(ua)); ua$id2 <- character(nrow(ua))
  dist_df <- head(ua[, c(1, 14, 15)], n = 0); agglom_list <- list()
  runtimes <- data.frame(iter = character(), runtime = double())}
  
#code block 2 ( > 1 day on a laptop)
for (iter in nrow(ua)) {d1 <- Sys.time(); if (ua$UACE20[1] %in% dist_df[, 3]) {
  break}; ua$dist <- as.vector(st_distance(ua[ua$UACE20 == ua$UACE20[1], ], ua))
  ua$id2 <- ua$UACE20[1]; distrows1 <- nrow(dist_df)
  dist_df <- rbind(data.frame(ua[ua$dist <= cdist, c(1, 14, 15)]), dist_df)
  distrows2 <- nrow(dist_df); distrows <- distrows2 - distrows1
  if (distrows > 1) {ids2check <- setdiff(dist_df$UACE20[1:distrows],
                                          ua$UACE20[1])
  union_vector <- c(ua$UACE20[1], ids2check[1]); while(length(ids2check) > 0) {
    ua$dist <- as.vector(st_distance(ua[ua$UACE20 == ids2check[1], ], ua))
    ua$id2 <- ids2check[1]
    dist_df <- rbind(data.frame(ua[ua$dist <= cdist, c(1, 14, 15)]), dist_df)
    distrows2 <- nrow(dist_df); distrows <- distrows2 - distrows1
    ids2check <- unique(setdiff(dist_df$UACE20[1:distrows], union_vector))
    union_vector <- c(union_vector, ids2check[1])}
  u <- st_sf(st_union(ua[ua$UACE20 %in% union_vector, ]))} else {
    u <- st_sf(st_union(ua[ua$UACE20 == ua$UACE20[1], ]))}
  if (length(union_vector) == 0) {ua <- ua[c(2:nrow(ua), 1), ]} else {
    sf_remain <- ua[!(ua$UACE20 %in% union_vector), ]; df <- head(ua, n = 0)
    df[1, ] <- c(paste0("agglom", i), character(1L), character(1L),
                 character(1L), character(1L), character(1L), character(1L),
                 character(1L), double(1L), double(1L), character(1L),
                 character(1L), u, double(1L), character(1L))
    ua <- rbind(sf_remain, df); union_vector <- union_vector[!is.na(union_vector)]
    agglom_list[[i]] <- union_vector; names(agglom_list)[[i]] <- paste0("agglom",
                                                                        i)
    union_vector <- character(0); i <- i + 1}; d2 <- Sys.time()
  runtimes[iter, ] <- c(iter, difftime(d2, d1))}
