library(histmaps)
library(rgeos)
library(stringr)
library(tidyverse)

# Create bse plot ------------
load("../histmaps/data/e1900bounds.rda")
load("../histmaps/data/e1900.rda")
bounds <- sp_to_ggplot(e1900bounds)

data(sweden)

bkgr <- cut_spbox(e1900, sweden, 40000)
bkgr <- sp_to_ggplot(bkgr)
d6 <- bkgr %>%
  filter(COUNTRY != 20)


bounds2 <- bounds %>%
  filter(COUNTRY %in% unique(d6$COUNTRY))

library(rgeos)
# Create swe boundaries by buffering
swe2 <- gBuffer(sweden, byid = TRUE, width = 3500)
swe2 <- SpatialPolygonsDataFrame(swe2, sweden@data)
swe <- sp_to_ggplot(sweden)
swe_g <- sp_to_ggplot(swe2)

# vb_back <- cut_spbox(sweden, skell, 2000)
# vb_back_df <- sp_to_ggplot(vb_back)



nadkods <- c(248201001, 241771000, 248203000, 248204006, 248209000, 248207001)

prs2 <- subset(hist_parish, nadkod %in% nadkods)

p0 <- ggplot() + 
  geom_polygon(data = d6, aes(long, lat, group = group), fill = "gray80") + 
  geom_polygon(data = swe, aes(long, lat, group = group), fill = "gray60") +
  geom_path(data = bounds2, aes(long, lat, group = group), color = "gray40", size = 0.5) +
  geom_polygon(data = prs2, aes(long, lat, group=group), fill = "black") +
  scale_x_continuous(expand = c(0,0), limits = c(min(d6$long), max(d6$long))) + 
  scale_y_continuous(expand = c(0,0), limits = c(min(d6$lat), max(d6$lat))) +
  coord_equal() +
  theme_void()


prs22 <- sp_to_ggplot(prs2)

lbls <- gCentroid(prs2, byid = TRUE) %>% as_data_frame() %>% rownames_to_column("geomid") %>% 
  mutate(geomid = as.numeric(geomid) +1)
lbls <- prs2@data %>% 
  left_join(lbls) %>% 
  mutate(socken = tolower(socken) %>% Hmisc::capitalize() %>% str_replace(" ", "\n"))

lbls$x[2] <- lbls$x[2] + 20000

dist <- 5000
bb <- sp::bbox(prs2)
bb[, 1] <- bb[, 1] - dist
bb[, 2] <- bb[, 2] + c(dist + 30000, dist)
backg2 <- clip_spdf(sweden, bb)

p1 <- ggplot() +
  geom_polygon(data = backg2, aes(long, lat, group = group), fill = "gray80") + 
  geom_polygon(data = prs22, aes(long, lat, group=group), fill = "white", color = "black")+
  geom_label(data =lbls, aes(x, y, label = socken)) +
  # scale_x_continuous(expand = c(0,0), limits = c(min(prs22$long), max(prs22$long))) + 
  coord_equal() +
  theme_void()

pl <- arrangeGrob(grobs = list(p0, p1), nrow = 1, ncol = 2, widths = c(0.3,1))

ggsave(plot = pl, file="figures/parish-map.png", width =8, height = 4)
