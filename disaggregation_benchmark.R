# Code to compare different methods to disaggregate spatial points and to 
# find an equivalent of the function sp::remove.duplicates(x,zero =dist)
# with the package sf

# load libraries and data -------------------------------------------------
set.seed(131)
library(sf)
library(sp)

df = data.frame(x = runif(10000), y = runif(10000), id = 1:10000) # generate points data
s_sf<-st_as_sf(df,coords = c("x","y")) #transform into an sf object
s_sp<-as(s_sf,'Spatial') #transform into an sp object
min_dist<-0.2 # minimal distance between points

# disaggregate with sp ----------------------------------------------------
ini_time<-Sys.time()
s_sp_dsg<-sp::remove.duplicates(s_sp, zero = min_dist)
end_time<-Sys.time()
sp_time<-end_time-ini_time
n_points_sp<-nrow(s_sp_dsg)

# disaggregate with sf ----------------------------------------------------
# code proposed by Edzer Pebesma https://github.com/r-spatial/sf/issues/669#issuecomment-830054441
ini_time<-Sys.time()
d = st_is_within_distance(s_sf, ,min_dist)
dupl = unlist(mapply(function(x,y) x[x<y] , d, seq_along(d)))
end_time<-Sys.time()
sf_time<-end_time-ini_time
n_points_sf<-nrow(s_sf[-dupl,])
# This does not work because :
# - the dataset should be dynamically disaggregated to be more conservative
# - st_is_within_distance calculation time grows exponentially with the number of points

# Here is an alternative that works, but still much slower than the sp::remove.duplicates function
ini_time<-Sys.time()
d<-st_equals_exact(s_sf,par = min_dist, remove_self = TRUE)
a<-which(lengths(d)>0)[1]
s_sf_dsg<-s_sf
while(!is.na(a)){
  s_sf_dsg<-s_sf_dsg[-d[[a]],]
  d<-st_equals_exact(s_sf_dsg,par = min_dist, remove_self = TRUE)
  a<-which(lengths(d)>0)[1]
}
end_time<-Sys.time()
sf_iterative_time<-end_time-ini_time
n_points_sf_iterative<-nrow(s_sf_dsg)

# Comparisons -------------------------------------------------------------

# Calculation time  
cat(paste('time with sp package', round(sp_time,2),"sec."))
cat(paste('time with sf::st_is_within_distance', round(sf_time,2),"sec."))
cat(paste('time with iterative sf::st_is_within_distance', round(sf_iterative_time,2),"sec."))

# number of points
cat (paste(n_points_sp), "points after disaggregation with sp::remove.duplicates")
cat (paste(n_points_sf), "points after disaggregation with sf st_is_within_distance and without iteratively removing points")
cat (paste(n_points_sf_iterative), "points after disaggregation with sf::st_equals_exact and iteratively removing points")

# plot
par(mfrow = c(1,3))
plot(s_sp, main = 'sp::remove.duplicates')
plot(s_sp_dsg, col = 'red', pch = 19, add = T)
plot(st_geometry(s_sf),main = 'sf::st_is_within_distance')
plot(st_geometry(s_sf[-dupl, ]),col = 'red', pch = 19, add = T)
plot(st_geometry(s_sf),main = 'sf::st_equals_exact and iterative' )
plot(st_geometry(s_sf_dsg),col = 'red', pch = 19, add = T)


# Create a sample dataset
set.seed(131)
df = data.frame(x = runif(10000), y = runif(10000), id = 1:10000)

# Convert to sf object
s_sf <- st_as_sf(df, coords = c("x", "y"))

# Set minimal distance between points
min_dist <- 0.2

# Buffer the points
s_buffer <- st_buffer(s_sf, dist = min_dist / 2)

# Find overlapping buffer areas
s_buffer<-st_set_precision(s_buffer,1)
overlaps <- st_intersection(s_buffer)

# Identify unique pairs of overlapping points
overlap_pairs <- overlaps %>%
  dplyr::filter(id.1 < id.2) %>%
  dplyr::select(id.1, id.2)

# Remove points that are too close
s_sf_dsg <- s_sf[!(s_sf$id %in% overlap_pairs$id.2),]

# Number of points after removing duplicates
n_points_sf <- nrow(s_sf_dsg)



