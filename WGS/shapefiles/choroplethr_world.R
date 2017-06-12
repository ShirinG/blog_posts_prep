download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/physical/ne_110m_land.zip", destfile="ne_110m_land.zip")
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_admin_0_countries.zip", destfile="ne_110m_admin_0_countries.zip")
download.file("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/110m/cultural/ne_110m_populated_places.zip", destfile="ne_110m_populated_places.zip")
unzip("ne_110m_land.zip")
unzip("ne_110m_admin_0_countries.zip")
unzip("ne_110m_populated_places.zip")

#https://www.mirosa.org/blog/post/2015/01/maps-in-r/

################################################

setwd("U:/Github_blog/blog_posts_prep/wip")

library(rgdal)
library(ggplot2)
library(plyr)
library(dplyr)
library(scales)

library(maptools)
gpclibPermit()


wmap <- readOGR(dsn=".", layer="ne_110m_land")

wmap_df <- fortify(wmap)
head(wmap_df)

theme_opts <- list(theme(panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         panel.background = element_blank(),
                         plot.background = element_rect(fill="white"),
                         panel.border = element_blank(),
                         axis.line = element_blank(),
                         axis.text.x = element_blank(),
                         axis.text.y = element_blank(),
                         axis.ticks = element_blank(),
                         axis.title.x = element_blank(),
                         axis.title.y = element_blank(),
                         plot.title = element_text(size=22)))

ggplot(wmap_df, aes(long, lat, group=group)) +
  geom_polygon() +
  coord_equal() +
  theme_opts


wmap_robin <- spTransform(wmap, CRS("+proj=robin"))
wmap_robin_df <- fortify(wmap_robin)

ggplot(wmap_robin_df) +
  geom_polygon(aes(long, lat, group=group)) +
  coord_equal() +
  theme_opts


wmap_countries <- readOGR(dsn=".", layer="ne_110m_admin_0_countries")
wmap_countries_robin <- spTransform(wmap_countries, CRS("+proj=robin"))
wmap_countries_robin_df <- fortify(wmap_countries_robin)

head(wmap_countries_robin_df)

ggplot(wmap_robin_df) +
  geom_polygon(aes(long, lat, group=group, fill=hole)) +
  geom_path(data=wmap_countries_robin_df, aes(long, lat, group=group), color="white", size=0.3) +
  coord_equal() +
  theme_opts +
  scale_fill_manual(values=c("black", "white"), guide="none")


ggplot(wmap_countries_robin_df) +
  geom_polygon(aes(long, lat, group=group, fill=id)) +
  geom_path(aes(long, lat, group=group), color="white", size=0.3) +
  coord_equal() +
  theme_opts +
  scale_fill_manual(values=1:177, guide="none")


wmap_countries_robin@data$id <- rownames(wmap_countries_robin@data)

head(wmap_countries_robin@data)

wmap_countries_robin_df_final <- join(wmap_countries_robin_df, wmap_countries_robin@data, by="id")

ggplot(wmap_countries_robin_df_final) +
  geom_path(aes(long, lat, group=group), color="white", size=0.7) +
  coord_equal() +
  theme_opts +
  geom_polygon(aes(long, lat, group=group, fill=continent)) +
  scale_fill_manual(values=1:8, guide="none")
