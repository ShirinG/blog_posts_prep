# Library
library(fmsb)

data=as.data.frame(matrix( sample( 2:20 , 10 , replace=T) , ncol=10))
colnames(data)=c("math" , "english" , "biology" , "music" , "R-coding", "data-viz" , "french" , "physic", "statistic", "sport" )

# To use the fmsb package, I have to add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data=rbind(rep(20,10) , rep(0,10) , data)

# The default radar chart proposed by the library:
radarchart(data)

# Custom the radarChart !
radarchart( data  , axistype=1 ,

            #custom polygon
            pcol=rgb(0.2,0.5,0.5,0.9) , pfcol=rgb(0.2,0.5,0.5,0.5) , plwd=4 ,

            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,

            #custom labels
            vlcex=0.8
)


mydata <- data.frame(Data_Analytics = as.numeric(as.character(c("10", "0", "8"))),
                     Machine_Learning = as.numeric(as.character(c("10", "0", "8"))),
                     Statistics = as.numeric(as.character(c("10", "0", "7"))),
                     Data_Visualization = as.numeric(as.character(c("10", "0", "8"))),
                     #NGS_Analysis = as.numeric(as.character(c("10", "0", "9"))),
                     R = as.numeric(as.character(c("10", "0", "9"))),
                     Python = as.numeric(as.character(c("10", "0", "4"))),
                     JS_CSS_HTML = as.numeric(as.character(c("10", "0", "3"))),
                     Unix = as.numeric(as.character(c("10", "0", "8"))),
                     #Algorithms = as.numeric(as.character(c("10", "0", "5"))),
                     Hadoop = as.numeric(as.character(c("10", "0", "2"))),
                     SQL = as.numeric(as.character(c("10", "0", "1"))),
                     #Data_Mining = as.numeric(as.character(c("10", "0", "4"))),
                     LaTeX = as.numeric(as.character(c("10", "0", "8"))),
                     Communication = as.numeric(as.character(c("10", "0", "8"))),
                     Teamwork = as.numeric(as.character(c("10", "0", "7"))),
                     Teaching = as.numeric(as.character(c("10", "0", "8"))),
                     Biology = as.numeric(as.character(c("10", "0", "9"))),
                     #Evolution = as.numeric(as.character(c("10", "0", "9"))),
                     #Scientific_Research = as.numeric(as.character(c("10", "0", "9"))),
                     English =  as.numeric(as.character(c("10", "0", "9"))))

colnames(mydata) <- gsub("_", " ", colnames(mydata))
colnames(mydata)[7] <- "JS/CSS/HTML"

library(RColorBrewer)
cols <- brewer.pal(n = 4, "Set1")

library(scales)

pdf("spiderchart_2017_04.pdf", width = 6, height = 6)
radarchart(mydata,
           axistype = 1,
           seg = 5,
           pty = 16,
           pcol = cols[1],
           plty = 1,
           plwd = 2,
           pfcol = alpha(cols[1], alpha = 0.5),
           cglwd = 0.8,
           cglty = 1,
           cglcol = alpha(cols[2], alpha = 0.5),
           axislabcol = cols[2],
           vlcex = 0.7,
           calcex = 0.8)
dev.off()
