# This script illustrates the partitioning procedure for one parish in the Skellefteå region.
# 
# 
# The procedure: 
#   
# 1. First run vornoni within parish
# 2. cluster points
# 3. combine clustered voronis


library(sp)
library(maptools)
library(rgeos)
library(poplinkdata)
library(broom)
library(deldir)
library(survival)
library(histmaps)
library(gridExtra)
library(tidyverse)

data("ehd_loc")

# Extract base parish map
crs <- proj4string(histmaps::hist_parish)
nofrs <-  c(82980,83010,83000,82983,82990,82988,82981, 82984, 82986)
prs <- subset(hist_parish, dedik %in% c(nofrs) & from <= 1935 & tom >= 1935)

if (interactive()){
  ggplot(prs, aes(long, lat, group = group)) +
    geom_path() + 
    coord_equal() +
    theme_void()
}


# Base background map
data(sweden)
swe <- sp_to_ggplot(sweden)



# Definition of functions used in the process.

cluster_points <- function(xy, ratio=0.15){
  set.seed(12)
  
  av_obs <- xy %>% group_by(year) %>% summarise(pop = sum(pop)) %>% ungroup() %>% summarise(av = mean(pop))%>% pluck("av")
  xy <- xy %>% select(x, y)
  clusts <- as.data.frame(xy) %>% kmeans(round(av_obs * ratio), 500)
  augment(clusts, as.data.frame(xy))
}



extract_tiles <- function(voronoi, crs){
  tiles <- tile.list(voronoi)
  
  PPS <- list()
  for ( i in 1:length(tiles)){
    t1 <- tiles[[i]]
    pp <- Polygon(coordinates(cbind(t1$x, t1$y)))
    PPS <- c(PPS, Polygons(list(pp), as.character(i)))
  }
  
  SpatialPolygons(PPS, proj4string = CRS(crs))
  
}

cut_to_region <- function(voro, polys){
  gIntersection(voro, polys, byid =T, drop_lower_td = T)
}

# The procedure is exemplified using location within the Bureå parish. 
# I start by extracting each observation for each year and location. Then I 
# extract all points within the boundaries of Bureå parish.

xy1 <- ehd_loc %>% 
  filter(
    !is.na(x)
  ) %>% 
  distinct(mid, date, x, y) %>% 
  filter(date >= "1880-01-01", date < "1950-01-01") %>% 
  arrange(mid, date)


xy2 <- xy1 %>% 
  group_by(mid) %>% 
  mutate(end_date = lead(date)) %>% 
  ungroup() %>% 
  filter(!is.na(end_date)) %>% 
  mutate(date = lubridate::decimal_date(date),end_date = lubridate::decimal_date(end_date)) %>% 
  filter(date < end_date)

xy3 <- survSplit(Surv(date, end_date, rep(1, nrow(xy2)))~., cut = 1880:1950, episode = "theid", data= xy2) %>% as_tibble()

xy <- xy3 %>% 
  mutate(durr = end_date - date, year = theid + 1878) %>% 
  group_by(x, y, year) %>% 
  summarise(pop = sum(durr))


subset_points <- function(points, polys){
  crs <- proj4string(polys)
  xy_coo <- SpatialPointsDataFrame(coordinates(as.data.frame(points[ ,c("x", "y")])), data= as.data.frame(points[ ,c("year", "pop")]), proj4string = CRS(crs))
  containes <- gContains(polys, xy_coo, byid = TRUE)
  xy_coo[as.vector(containes), ]
}

sub_reg <- subset(prs, geomid == 142)
xy_subset <- subset_points(xy, sub_reg)
xy_sub <- as.data.frame(xy_subset)

clip_spdf <- function (shp, bb, byid = T) {
  if (all(c("raster", "rgeos") %in% rownames(installed.packages())) == 
      FALSE) 
    stop("Requires raster and rgeos packages")
  if (class(bb) == "matrix") 
    b_poly <- as(raster::extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(raster::extent(bb), "SpatialPolygons")
  sp::proj4string(b_poly) <- sp::proj4string(shp)
  # Workaround for assignment bug
  shp_ids <- rownames(shp@data)
  shp_ids_in <- rlang::duplicate(shp_ids)
  res <- rgeos::gIntersection(shp, b_poly, id = shp_ids_in, 
                              byid = byid)
  ids <- sapply(slot(res, "polygons"), slot, "ID")
  d <- slot(shp, "data") %>% add_rownames() %>% filter(rowname %in% 
                                                         ids)
  rownames(d) <- ids
  SpatialPolygonsDataFrame(res, data = as.data.frame(d))
}
cut_spbox <- function (big, small, dist, ...) 
{
  bb <- sp::bbox(small)
  bb[, 1] <- bb[, 1] - dist
  bb[, 2] <- bb[, 2] + dist
  clip_spdf(big, bb, ...)
}

# Cut background maps for plotting
backg <- cut_spbox(sweden, sub_reg, 2000)
back_prs <- cut_spbox(prs, sub_reg, 2000)



# I use an implementation of the Voronoi algorithm in the `deldir` library
# xy_subset
xy_sub <- as.data.frame(xy_sub)

xy_und <- distinct(xy_sub)
# bounds of the parish
bounds <-  as.vector(t(bbox(sub_reg))) 
# calculate tesselation
voronoi <- deldir(xy_und$x, xy_und$y, rw = bounds)
# extract tiles
spp <- extract_tiles(voronoi, proj4string(sub_reg))

# plot tiles
spp_df <- tidy(spp)
sub_df <- sp_to_ggplot(sub_reg)
p1 <- ggplot() +
  geom_polygon(data = sub_df, aes(long, lat, group = group), fill = "gray")  +
  geom_point(data = xy_sub, aes(x, y), color = "red", size = .5) +
  geom_path(data = spp_df, aes(long, lat, group = group)) +
  coord_equal() +
  theme_void()

if (interactive()) p1

# As seen the Voronoi tesselation asumes that the space is rectangular, thus I use 
# the bounding box of the parish as the space to be partition. The tesselation tiles 
# is then cut to the boundaries of the parish.

spp_c <- cut_to_region(spp, sub_reg)

p2 <- ggplot() +
  geom_polygon(data= backg, aes(long, lat, group = group), fill = "gray80") +
  geom_path(data= back_prs, aes(long, lat, group = group), color = "gray40") +
  # geom_point(data = clusts_t, aes(x1, x2), color = "blue") +
  geom_polygon(data = spp_c, aes(long, lat, group = group), fill = "white", color = "black") +
  geom_point(data = xy_sub, aes(x, y), color = "red", size = .5) +
  coord_equal() +
  theme_void()
if (interactive()) p2




## Cluster points 
#
# The next step is to cluster all points into groups.

r <- 0.015
clust <- cluster_points(xy_sub, r)

if (interactive()) count(clust, .cluster) %>% knitr::kable(booktabs = T)

# Each point is assigned into a group through k-means clustering.  The number of goups are set to 1/1000 observations, in this case 11 clusters. 

clust_u <- distinct(clust, x, y, .cluster)
temp <- clust_u
ids <- sapply(spp_c@polygons, function(x) x@ID)
row.names(temp) <- ids
d <- SpatialPolygonsDataFrame(spp_c, temp)
d <- sp_to_ggplot(d)
p3 <- ggplot() +
  geom_polygon(data= backg, aes(long, lat, group = group), fill = "gray80") +
  geom_path(data= back_prs, aes(long, lat, group = group), color = "gray50") +
  geom_polygon(data = d, aes(long, lat, group = group, fill = factor(.cluster))) + 
  geom_path(data= d, aes(long, lat, group = group)) +
  geom_point(data = clust, aes(x, y), size = .5) +
  coord_equal() + 
  theme_void() +
  theme(legend.position = "none")
if (interactive()) p3

# Then the voronoi tiles are combined into sub-regions, according to their cluster 
# assignment.


clust_u <- distinct(clust, x, y, .cluster)

spp_u <- unionSpatialPolygons(spp_c, clust_u$.cluster)

p4 <- ggplot() +
  geom_polygon(data= backg, aes(long, lat, group = group), fill = "gray80") +
  geom_path(data= back_prs, aes(long, lat, group = group), color = "gray40") +
  # geom_polygon(data = d, aes(long, lat, group = group, fill = factor(.cluster))) + 
  geom_polygon(data= spp_u, aes(long, lat, group = group), fill = "white", color = "black") +
  geom_point(data = clust, aes(x, y), size = .5) +
  coord_equal() + 
  theme_void()
if (interactive()) p4

# Overview map
nadkods <- c(248201001, 241771000, 248203000, 248204006, 248209000, 248207001)

prs3 <- subset(hist_parish, nadkod %in% nadkods)

back2 <- cut_spbox(sweden, prs, 2000)
p5 <- ggplot() +
  geom_polygon(data= back2, aes(long, lat, group = group), fill = "gray80") +
  geom_path(data= prs3, aes(long, lat, group = group), color = "gray40") +
  geom_polygon(data= sub_reg, aes(long, lat, group = group), fill = "white", color = "black") +
  coord_equal() + 
  theme_void()

if (interactive()) p5


p2 <- p2 + labs(title = "A")
p3 <- p3 + labs(title = "B")
p4 <- p4 + labs(title = "C")
p5 <- p5 + labs(title = "Overview")
gr2 <- marrangeGrob(list(p5, p2, p3, p4), nrow = 1, ncol = 4, top = NULL)

ggsave("figures/voro_cluster.png", gr2, height = 2.5, width = 12)





# Exposure map

library(movement)

data("voroni_spdf")

skell_m <- tbl_df(movement) %>% 
  filter(forkod %in% c(unique(prs@data$forkod), 241702), year < 1951) %>% 
  distinct(orgtypn, lon, lat)

vsp <- gBuffer(voroni_spdf, TRUE, width = 10)

sp_rel <- gOverlaps(vsp, byid = TRUE)
sp_rel_d <- sp_rel %>% as_data_frame() %>% 
  mutate(voro = rownames(sp_rel)) %>% 
  gather(group, ind, -voro) %>% 
  filter(ind)


voroni_df <- tidy(voroni_spdf)

ms <- skell_m %>% filter(!is.na(lon))

ms_sp <- SpatialPointsDataFrame(ms[ ,c("lon", "lat")], data = ms, proj4string = CRS(crs))

x <- over(voroni_spdf, ms_sp, returnList = TRUE)

a <- lapply(1:length(x), function(y){
  z <- x[[y]]
  if (nrow(z) < 1) return(NULL)
  z$group = names(x)[y]
  z
}) %>% bind_rows() 


skell_moves <- movement %>% 
  tbl_df() %>% 
  left_join(a) %>% 
  filter(!is.na(group))

period_l <- 1

voro_move <- skell_moves %>% 
  mutate(year = (year %/% period_l) * period_l) %>% 
  group_by(group, year, orgtypn) %>% 
  summarise(membs = sum(medl, na.rm=T)/period_l) %>% 
  ungroup() %>% 
  filter(!is.na(membs)) %>% 
  mutate(
    memb_g = cut(membs, quantile(membs))
  )
voro_move


voro_t <- full_join(sp_rel_d, voro_move, by = "group") %>% 
  filter(!is.na(year)) %>% 
  replace_na(list(membs = 0)) %>% 
  group_by(voro, year, orgtypn) %>% 
  summarise(neighbour = sum(membs)) %>% 
  ungroup() %>% 
  rename(group = voro)

voro_move2 <- full_join(voro_move, voro_t) %>% 
  replace_na(list(membs = 0, neighbour = 0)) %>% 
  filter(membs > 0 | neighbour > 0)


library(forcats)
tmp1 <- voro_move2 %>% 
  mutate(year = (year %/% 1)*1) %>% 
  group_by(year, group, orgtypn) %>% 
  summarise(membs = mean(membs)) %>% 
  ungroup() 

cutp <- c(0, quantile(tmp1$membs[tmp1$membs > 0], probs = seq(0,1,1/3)))
cut_l <- round(cutp,2)[-length(cutp)]

voro2 <- tmp1 %>% 
  mutate(
    memb_g = cut(membs, cutp, include.lowest = TRUE, labels = cut_l),
    memb_o = as.character(memb_g) %>% as.numeric()
  ) %>% 
  arrange(memb_o) %>% 
  mutate(memb_g = fct_reorder(memb_g, memb_o)) %>% 
  rename(id = group) %>% 
  left_join(voroni_df, .) %>% 
  replace_na(list(memb_g = "0"))



library(ggthemes)
voro2 %>%
  filter(!is.na(memb_g)) %>% 
  mutate(
    year2 = as.character(year)
  ) %>% 
  filter(orgtypn %in% c("nykt", "fackf"), !is.na(orgtypn), year %in% c(1890, 1900, 1910, 1920,1930)) %>%
  mutate(
    orgtypn = fct_recode(orgtypn, Temperance = 'nykt', Union = 'fackf'), 
    memb_g = ifelse(memb_g == "0", "Adjecent", "Present") %>% fct_rev()
  ) %>% 
  ggplot() +
  geom_path(data = voroni_df, aes(long, lat, group=group), color = "lightgray", size = .2) +
  geom_path(data = prs3, aes(long, lat, group=group), color = "gray80") +
  geom_polygon(aes(long, lat, group=group, fill = memb_g), color ="black", size = .1) +
  # scale_fill_brewer(palette = "Blues") +
  scale_fill_tableau("bluered12")+
  coord_equal() +
  labs(fill = NULL) +
  facet_grid(orgtypn~year2) +
  theme_void() +
  theme(legend.position = "bottom")

ggsave(filename = "figures/va_map.png", height = 3.2, width = 8)


areas <- gArea(voroni_spdf, TRUE)/1000000
p_areas <- gArea(prs3, TRUE)/1000000
n <- length(voroni_spdf)

sum_area <- function(x){
  areas <- gArea(x, TRUE)/1000000
  data_frame(
    n = length(x),
    mean = mean(areas),
    median = median(areas),
    min = min(areas),
    max = max(areas),
    sd  = sd(areas)
  )
}

places <- nrow(distinct(xy, x, y))

d <- plyr::ldply(list(voroni_spdf, prs3), sum_area) %>% mutate(area = c("Neigbourhood", "Parish"))

d <- bind_rows(d, data_frame(area= "Place", n = places))

write_csv(d, "data/nei_desc.csv")

