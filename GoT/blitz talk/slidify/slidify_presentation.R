library(slidify)
#author("Presentation")

setwd("~/Documents/Github/blog_posts_prep/GoT/blitz talk/slidify/GoT_presentation")
slidify("index.Rmd")
#browseURL("index.html")

publish(host = "github", user = "github.com/ShirinG", repo = "GoT_slidify")
