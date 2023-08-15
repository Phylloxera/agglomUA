#variables
fp <- <path to shapefile>
file <- <name of file>
cdist <- <threshold distance for combining neighbor features>

#for example, on windows
fp <- "GeoSpatial\\Urban"; file <- "tl_2020_us_uac20"; cdist <- 100

#setup code block (runs in seconds)
{library(sf); ua <- read_sf(dsn = fp, layer = file); union_vector <- character()
  i <- 1; ua$dist <- double(nrow(ua)); ua$id2 <- character(nrow(ua))
  dist_df <- head(ua[, c(1, 14, 15)], n = 0)[, 1:3]; agglom_list <- list()
  runtimes <- data.frame(iter = character(), runtime = double())
  nn_df <- data.frame(nn_UACE = character(), dist = double(),
                      UACE = character())}

#loop code block (runtime inverse proportional to cdist, ~19h on laptop)
for (iter in 1:nrow(ua)) {if (ua[[1]][1] %in% dist_df[, 3]) {break}
ua$dist <- as.vector(st_distance(ua[ua[[1]] == ua[[1]][1], ], ua))
ua$id2 <- ua[[1]][1]; distrows1 <- nrow(dist_df)
dist_df <- rbind(data.frame(ua[ua$dist <= cdist, c(1, 14, 15)])[, 1:3], dist_df)
distrows2 <- nrow(dist_df)
nn <- data.frame(ua[ua$dist > cdist, c(1, 14, 15)])[1:3]
nn <- nn[which.min(nn$dist), ]; names(nn) <- c("nn_UACE","dist","UACE")
distrows <- distrows2 - distrows1
if (distrows > 1) {ids2check <- setdiff(dist_df[[1]][1:distrows], ua[[1]][1])
union_vector <- c(ua[[1]][1], ids2check[1])
union_vector <- union_vector[!is.na(union_vector)]
while(length(ids2check) > 0) {
  ua$dist <- as.vector(st_distance(ua[ua[[1]] == ids2check[1], ], ua))
  ua$id2 <- ids2check[1]
  dist_df <- rbind(data.frame(ua[ua$dist <= cdist, c(1, 14, 15)])[, 1:3],
                   dist_df)
  nn2 <- data.frame(ua[ua$dist > cdist, c(1, 14, 15)])[1:3]
  nn2 <- nn2[which(!(nn2[[3]] %in% union_vector)), ]
  nn2 <- nn2[which.min(nn2$dist), ]
  names(nn2) <- c("nn_UACE", "dist", "UACE"); nn <- rbind(nn, nn2)
  nn <- nn[which(!(nn[[1]] %in% union_vector)), ]
  distrows2 <- nrow(dist_df); distrows <- distrows2 - distrows1
  ids2check <- unique(setdiff(dist_df[[1]][1:distrows], union_vector))
  union_vector <- c(union_vector, ids2check[1])
  union_vector <- union_vector[!is.na(union_vector)]}
u <- st_sf(st_union(ua[ua[[1]] %in% union_vector, ]))
nn <- nn[which(!(nn[[1]] %in% union_vector)), ]
nn <- nn[which.min(nn$dist), ]} else {
  u <- st_sf(st_union(ua[ua[[1]] == ua[[1]][1], ]))}
if (length(union_vector) == 0) {ua <- ua[c(2:nrow(ua), 1), ]} else {
  sf_remain <- ua[!(ua[[1]] %in% union_vector), ]; df <- head(ua, n = 0)
  df[1, ] <- c(paste0("agglom", i), character(1L), character(1L), character(1L),
               character(1L), character(1L), character(1L), character(1L),
               double(1L), double(1L), character(1L), character(1L), u,
               double(1L), character(1L)); ua <- rbind(sf_remain, df)
  union_vector <- union_vector[!is.na(union_vector)]
  agglom_list[[i]] <- union_vector
  names(agglom_list)[[i]] <- paste0("agglom", i); union_vector <- character(0)
  i <- i + 1}; nn_df <- rbind(nn_df, nn)}

#clean up code block
{dist_df <- dist_df[which(dist_df[[1]] != dist_df$id2), ]
remove(df, nn, nn2, sf_remain, u, d1, d2, distrows, distrows1, distrows2, file,
       i, ids2check, iter, union_vector)}

#metadata file. user inputs. load readxl package
file <- "\\2020_Census_ua_list_all.xlsx"; library(readxl)

#process agglom_list to metadata. results in a table of urban agglomerations
#corresponding to the user provided distance
{ua_meta <- read_excel(paste0(fp, file))
 if ("LSADC" %in% names(ua_meta)) {
    ua_meta2 <- ua_meta[, -which(names(ua_meta) == "LSADC")]
    ua_meta <- ua_meta2; remove(ua_meta2)}
for (i in 1:length(agglom_list)) {
  df <- ua_meta[ua_meta[[1]] %in% agglom_list[[i]], ]
  df <- df[order(df[[3]]), ]
  df$EABCOMBINED <- paste(df$NAME[1:(nrow(df) - 1)], collapse = "; ")
  df$UACES <- paste(df$UACE[1:(nrow(df) - 1)], collapse = ", ")
  if (exists("agglom_meta")) {
    agglom_meta <- rbind(agglom_meta, data.frame(c(df[nrow(df), 1:2],
                                                   colSums(df[, 3:9]),
                                                   df[nrow(df), 10:11])))
  } else {agglom_meta <- data.frame(c(df[nrow(df), 1:2], colSums(df[, 3:9]),
                                      df[nrow(df), 10:11]))}}
ua_meta_ss <- ua_meta[ua_meta[[1]] %in% ua[[1]], ]
ua_meta_ss$EABCOMBINED <- NA; ua_meta_ss$UACES <- NA
agglom_meta <- rbind(agglom_meta, ua_meta_ss)
agglom_meta[["POPDEN"]] <- agglom_meta[["POP"]]/agglom_meta[["AREALANDSQMI"]]
agglom_meta$AREALANDSQMI <- round(agglom_meta$AREALANDSQMI, 0)
agglom_meta$AREAWATERSQMI <- round(agglom_meta$AREAWATERSQMI, 0)
agglom_meta$POPDEN <- round(agglom_meta$POPDEN, 0); nn_df2 <- nn_df
for (i in 1:nrow(nn_df)) {for (j in c(1, 3)) {
  if (nn_df[i, j] %in% names(agglom_list)) {
    nn_df2[i, j] <- agglom_meta[which(agglom_meta[[1]] %in%
                                        agglom_list[[nn_df[i, j]]]), 2]
  } else if (nn_df[i, j] %in% agglom_meta[[1]]) {
    nn_df2[i, j] <- agglom_meta[which(agglom_meta[[1]] %in% nn_df[i, j]), 2]
  } else {nn_df2[i, j] <- agglom_meta[grep(nn_df[i, j], agglom_meta[[11]]), 2]}}}
names(nn_df2) <- c("NEARNEIGHBOR", "NEIGHBORDIST", "NAME")
agglom_meta <- merge(agglom_meta, nn_df2)}

#write output urban agglomerations to a table
write.table(agglom_meta, file = "agglom_meta.txt", row.names = F, quote = F,
            sep = "|")

#user input (number of top nearest neighbor pairs desired by gravitational pull)
n <- 50

#define new table and column
gm <- agglom_meta; gm$GRAVPULL <- NULL

#calculate Gravitational Pull
for (i in 1:nrow(gm)) {gm$GRAVPULL[i] <- (gm[which(gm[[1]] %in% gm[[12]][i]),
                                             3] * gm[i, 3]) / (gm[i, 13] ^ 2)}

#remove duplicates (bidirectional nearest neighbors) and subset out columns
gm <- gm[!duplicated(gm[ , "GRAVPULL"]), ][, c(1, 3, 12:14)]

#sort decending by Gravitational Pull and subset top n rows (user input)
gm <- gm[order(-gm[[5]]), ][1:n, ]

#write output table
write.table(gm, file = "gm.txt", row.names = F, quote = F, sep = "|")
