#set up r environment
intall.packages('devtools'); devtools::install_github("Phylloxera/agglomUA"); library(agglomUA)
#run the mergeUA function on Census 2020 data. in this example, the working directory is your home directory
m2020 <- mergeUA("~", "tl_2020_us_uac20.shp", "2020_Census_ua_list_all.xlsx")
#run the unionUA function on the merged UA sf object with a combine distance of 0.25 (will combine features separated by less than 0.25 miles)
u2020 <- unionUA(m2020, 0.25)
#run the gravitational model on the unioned UA sf object
g2020 <- gmUA(u2020)
