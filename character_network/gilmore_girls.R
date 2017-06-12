

for(i in 1:7){ # there are 7 seasons

  if(i == 1){

    for(j in 1:21){ # all seasons except the first have 22 episodes (the first has 21)

      cat("\nSeason", i, ", Episode", j, "\n")

      thepage <- readLines(paste0("http://www.crazy-internet-people.com/site/gilmoregirls/pages/s", i, "/s", i, "s/", j, ".html"))

      thepage <- thepage[grep("[[:upper:]]+:", thepage)]
      thepage <- gsub("\t", "", thepage)
      thepage <- gsub("<.*>", "", thepage)

      thepage <- as.data.frame(thepage)
      thepage$season <- i
      thepage$episode <- paste(i, j, sep = "_")
      thepage$episode_running_nr <- j

      if(i == 1 & j == 1){
        transcripts <- thepage
      } else {
        transcripts <- rbind(transcripts, thepage)
      }
    }

    } else {

      for(j in 1:22){ # all seasons except the first have 22 episodes (the first has 21)

        cat("\nSeason", i, ", Episode", j, "\n")

        if(i == 2){
          n <- j+21
        }

        if(i == 3){
          n <- j+21+22
        }

        if(i == 4){
          n <- j+21+22+22
        }

        if(i == 5){
          n <- j+21+22+22+22
        }

        if(i == 6){
          n <- j+21+22+22+22+22
        }

        if(i == 7){
          n <- j+21+22+22+22+22+22
        }

        thepage <- readLines(paste0("http://www.crazy-internet-people.com/site/gilmoregirls/pages/s", i, "/s", i, "s/", n, ".html"))

        thepage <- thepage[grep("[[:upper:]]{2,}:", thepage)]
        thepage <- gsub("\t", "", thepage)
        thepage <- gsub("<.*>", "", thepage)

        thepage <- as.data.frame(thepage)
        thepage$season <- i
        thepage$episode <- paste(i, j, sep = "_")
        thepage$episode_running_nr <- n

        if(i == 1 & j == 1){
          transcripts <- thepage
        } else {
          transcripts <- rbind(transcripts, thepage)
        }
        }
  }
}

setwd("U:/Github_blog/blog_posts_prep/character_network")
write.table(transcripts, "gilmore_girls_transcripts.txt", row.names = F, col.names = T, sep = "\t")


transcripts$thepage <- as.character(transcripts$thepage)

transcripts <- transcripts[!transcripts$thepage == "", ]

head(transcripts)
nrow(transcripts)


library(tidyr)
transcripts_2 <- separate(transcripts, "thepage", into = c("character", "dialogue"), sep = ":", extra = "merge", fill = "right")

transcripts[650, ]
transcripts[92582, ]

transcripts_2[650, ]
transcripts_2[92582, ]

# remove leading and trailing whitespace
transcripts_2$character <- gsub("^\\s+|\\s+$", "", transcripts_2$character)

head(transcripts_2)
nrow(transcripts_2)

characters <- as.data.frame(table(transcripts_2$character))
head(characters)

characters_with_most_lines <- characters[order(characters$Freq, decreasing = TRUE), ]
head(characters_with_most_lines)

transcripts_3 <- transcripts_2[which(transcripts_2$character %in% as.character(characters_with_most_lines$Var1[1:10])), ]

head(transcripts_3)
nrow(transcripts_3)

characters <- as.data.frame(table(transcripts_3$character))
head(characters)

characters_by_season <- as.data.frame(with(transcripts_3, table(character, season)))
head(characters_by_season)

characters_by_episode <- as.data.frame(with(transcripts_3, table(character, episode)))
head(characters_by_episode)


library(reshape2)
speaker_scene_matrix <- transcripts_3 %>%
  acast(character ~ episode, fun.aggregate = length)

tail(speaker_scene_matrix)


norm <- speaker_scene_matrix / rowSums(speaker_scene_matrix)

h <- hclust(dist(norm, method = "manhattan"))

plot(h)

ordering <- h$labels[h$order]
ordering

library(ggplot2)
ggplot(transcripts_3, aes(episode, character)) +
  geom_point() +
  geom_path(aes(group = episode))

head(transcripts_3)
head(characters_by_episode)
head(speaker_scene_matrix)

heatmap(speaker_scene_matrix)

library(WGCNA)
#speaker_scene_matrix_2 <- ifelse(speaker_scene_matrix > 0, 1, 0)

a <- adjacency(t(speaker_scene_matrix), type="distance")
head(a)



library('network')
library('sna')
library('ndtv')
library('visNetwork')


data_matrix <- as.matrix(t(speaker_scene_matrix))
total_occurrences <- colSums(t(speaker_scene_matrix))

co_occurrence <- t(data_matrix) %*% data_matrix

library(igraph)
g <- graph.adjacency(co_occurrence,
                         weighted=TRUE,
                         #mode="undirected",
                         diag=FALSE,
                         mode="upper")

g <- simplify(g, remove.multiple = F, remove.loops = T, edge.attr.comb=c(weight="sum", type="ignore"))

V(g)$name
V(g)$gender <- c("female", "male", "female", "male", "female", "male", "female", "male", "female", "female")

plot(g,
     vertex.label=colnames(t(speaker_scene_matrix)),
     vertex.label.family = "Helvetica",
     vertex.label.font = 1,
     vertex.shape = "sphere",
     vertex.size=total_occurrences/500,
     vertex.label.cex=0.8,
     vertex.color=c( "pink", "skyblue")[1+(V(g)$gender=="male")],
     vertex.label.color="black",
     vertex.frame.color = NA,
     edge.width=E(graph)$weight/500000,
     edge.curved=.1,
     layout=layout_in_circle)

cfg <- cluster_fast_greedy(as.undirected(g))

plot(cfg, as.undirected(g))
