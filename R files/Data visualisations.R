#----0 ~ Packages ----

#Installation of packages 
install.packages("tidyverse") #GGplot for graphing and visualization, dplyr for data manipulation, and tidyr for data tidying
install.packages("dplyr") #allows for data manipulation
install.packages(("RColorBrewer"))#Allows me to customise graph colourings
install.packages("ggalt")
install.packages("scales")
install.packages("GGally")
install.packages("ggtext") #allows me to manipulate text using markdown-like formatting
install.packages("gridExtra")
install.packages("extrafont")

#GitHub packages require slightly different installation
install.packages("devtools")
devtools::install_github("ricardo-bion/ggradar",
                         dependencies = TRUE, force=TRUE)


#accessing libraries
library(tidyverse)
library(dplyr)
library(RColorBrewer)
library(ggalt)
library(ggradar)
library(scales)
library(GGally)
library(ggtext)
library(gridExtra)
library(forcats)
library(extrafont)

#----1.0 ~ Data formatting and manipulation ----

music_data<-read.csv("Music_Data.csv", header = TRUE) #Function allows me to import the Spotify songs data set from my folders

##----1.1 ~ formatting ----
#to perform analysis, all unnecesary categorical variables are excluded
clean_music_data <- music_data %>% 
  select (track_genre, 6:19) %>% 
  select(-duration_ms,-explicit, -mode, -key)


#Not all columns are consistently scaled, so we must change popularity and loudness to be within 0-1
#To change scales into 0-1, min-max values are imposed on the dataset
min_max_scale <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

music_data.adj <- clean_music_data %>% 
  mutate(
    popularity = min_max_scale(popularity), #applies f(x) to popularity
    loudness = min_max_scale(loudness), #applies f(x) to loudness
    tempo = min_max_scale(tempo) # applied f(x) to tempo
  )



#Creates categories for the full dataset based on popularity
adj.ms_cat <- music_data.adj %>%
  mutate(
    pop_category = case_when(
      popularity <= 0.1700 ~ "Low popularity",
      popularity > 0.1700 & popularity <= 0.3324 ~ "Low/Med popularity",
      popularity > 0.3324 & popularity <= 0.5 ~ "Med/High popularity",
      popularity > 0.50 ~ "High popularity"
    )
  )
#prints the n. of tracks in each category
table(adj.ms_cat$pop_category)


#grouping tracks by genre, and calculating means for all numeric values
genres_music_data.adj <- music_data.adj %>% 
  group_by(track_genre) %>%
  summarise_if(is.numeric,mean) #get the average across all tracks in each genre

##----1.2 ~Grouping by genre for all genres ----

#In order to categorise all the genres, we classify based on popularity IQR
summary(music_data.adj$popularity)

#grouped data based on IQRs of popularity to determine categories
#Absolute Values
grouped_music_data.categorised <- genres_music_data.adj %>%
  mutate(
    pop_category = case_when(
      popularity <= 0.1700 ~ "Low popularity",
      popularity > 0.1700 & popularity <= 0.3324 ~ "Low/Med popularity",
      popularity > 0.3324 & popularity <= 0.5 ~ "Med/High popularity",
      popularity > 0.50 ~ "High popularity"
    )
  )

#Quantile values
grouped_music_data.Qcat <- genres_music_data.adj
grouped_music_data.Qcat <- genres_music_data.adj %>%
  mutate(
    pop_category = case_when(
      popularity <= quantile(popularity, 0.3333) ~ "Low popularity",
      popularity > quantile(popularity, 0.3333) & popularity <= quantile(popularity, 0.6667) ~ "medium popularity",
      popularity > quantile(popularity,0.6667) ~ "High popularity"
      )
  )


#Proportionally selects 4 entries from each popularity category
set.seed(100)
prop.categorised_grouped_music_data <-grouped_music_data.categorised %>%
  group_by(pop_category) %>%
  slice_sample(n = 4) %>% #n = 4 as the smallest subset contained a maximum of 4 entries
  ungroup()



##----1.3 ~ Grouping by genre for filtered genres ----

#filters for genres, but does not group, resulting in indiv tracks persent for each genre
fullset_filtered.genres <- music_data.adj %>%
  filter(track_genre %in% c("pop-film", "k-pop", "chill",
                           "cantopop", "children", "edm", "rock-n-roll",
                           "latin", "romance", "iranian")
  )

fullset_filtered.genres <- music_data.adj %>%
  mutate(
    pop_category = case_when(
      popularity <= 0.1700 ~ "Low popularity",
      popularity > 0.1700 & popularity <= 0.3324 ~ "Low/Med popularity",
      popularity > 0.3324 & popularity <= 0.5 ~ "Med/High popularity",
      popularity > 0.50 ~ "High popularity"
    )
  )


##Quantile values of fullset filtered genres, but for all datapoints
full.cat_music_data <- fullset_filtered.genres %>%
  mutate(
    pop_category = case_when(
      popularity <= quantile(popularity, 0.3333) ~ "Low popularity",
      popularity > quantile(popularity, 0.3333) & popularity <= quantile(popularity, 0.6667) ~ "medium popularity",
      popularity > quantile(popularity,0.6667) ~ "High popularity"
    )
  )



#Grouped colours based on popularity category
grouped_pop.col <- c(
  "Low popularity" = '#33a02c',
  "Low/Med popularity" = '#b2df8a',
  "Medium popularity" = '#a6cee3',
  "High popularity" = '#1f78b4'
)



#Alternatively, we can filter for genres with added grouping by genre, and calculate means
#Required for radar chart to display top/bottom 3 popularity, and 4 median genres
filtered_genres <- genres_music_data.adj %>%
  filter(track_genre %in% c("pop-film", "k-pop", "chill",
                            "cantopop", "children", "edm", "rock-n-roll",
                            "latin", "romance", "iranian"))

#Arranging the genres according to popularity
filtered_genres <- filtered_genres %>% arrange(desc(popularity))


#Additionally, we create categories outlining popularity based on IQR ranges
summary(filtered_genres$popularity)

#We create filters to organise genres based on popularity ranges

#To do this, we assign colours to categories
popularity_colours <- c(
  "Low popularity" = "red",
  "Medium popularity" = "blue",
  "High popularity" = "green"
)

#Then we create a filtered df, selecting entries according to quantile datapoints.
filtered_genres.categorised <- filtered_genres %>%
  mutate(
    pop_category = case_when(
      popularity <= quantile(popularity, 0.25)~ "Low popularity",
      popularity > quantile(popularity, 0.25) & popularity <= quantile(popularity, 0.5) ~ "Medium popularity",
      popularity > quantile(popularity, 0.51) ~ "High popularity"
    ),
    #EDM and Children are causing issues, therefore they are manually changed to the correct category
    pop_category = ifelse(
      track_genre %in% c("edm", "children") & pop_category == "High popularity", #searches for High pop categories in edm and children
      "Medium popularity", # Replaces the found entries for medium popularity in pop_category
      pop_category
    ),
    #We also ensure it's a factor to ensure correct plotting
    pop_category = factor(pop_category, levels = c("Low popularity", "Medium popularity", "High popularity"))
  )


#outputs the table data for the filtered genres, categorised by popularity range
write.table(filtered_genres.categorised, "categories for filtered genres.csv", row.names = T, sep = ",")



#----2.0 ~ Data Visualisations ----
##----2.1 ~ Radar chart ----

#Radar Chart can allow for high dimensional data to be plotted 
#->Usage of ggradar() required

#to differentiate colours, i assign each genre a colour, matching their popularity category
preset_colours <- c('#89c167','#01ef48','#8f0000','#7ef137','#033e68', '#d98217', '#92c6f7', '#eb4752','#035c00', '#35719c')

#Reds are high popularity
#Greens are medium popularity
#Blues are low popularity
#Brighter colours are more popular within their category

#simple plot of all genres and ranging popularity across features
ggradar (filtered_genres,
         values.radar = c("0", "0.5", "1"),
         grid.label.size = 5,
         gridline.label.offset = -0.175,
         axis.label.size = 4,
         legend.text.size = 8,
         group.line.width = 0.7,
         group.point.size = 1.7,
         legend.title = "Filtered Genres",
         legend.position = "right",
         plot.title = "Feature intensity across genres",
         background.circle.colour = "#aeada9",
         group.colours = preset_colours,
         fill = TRUE, fill.alpha = 0.07,
         gridline.min.colour = "black",
         gridline.mid.colour = "blue",
         gridline.max.colour = "grey",
)

#.csv file containing all plot data for Radar chart
write.table(filtered_genres, "data for Radar chart.csv", row.names = T, sep = ",")

##----2.2 ~ Parallel coordinate plot ----
#parallel coordinate plot can be used to visualise genre features.
#-> x = music features, y = value, color = genre (separated by category (high-pop, low-pop, mid-pop))

#parallel coordinate plot

ggparcoord(grouped_music_data.Qcat, 
           columns = c(2:11), #Selects my music feature columns 
           groupColumn = 12, #Selects my colour variable to determine line grouping
           alphaLines = 0.4) + #Improves Readability
  geom_vline(xintercept = c(2:11), alpha = 0.3) + #Adds lines to identify feature points
  theme_bw() + #changes theme
  theme(axis.text.x = element_text(angle = 45, vjust = 0.575), #adjusts text position and orientation
        axis.ticks.length.x = unit(.2, "cm"), #adds ticks to connect feature name to line
        plot.caption = element_markdown(size = 8),
        legend.text = element_text(size =10),
        legend.title = element_text(size =14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = element_text(size = 16),
        plot.subtitle = element_text(size = 10)
  ) + 
  scale_x_discrete(expand = expansion(add = c(0,0))) + #Aligns category 1 (popularity) to start of plot
  scale_y_continuous(
    breaks = scales::rescale(seq(0, 1, by = 0.25), from = c(0, 1), to = c(-5, 10)), #reformats breakpoints for Y-axis ticks
    labels = seq(0, 1, by = 0.25)#labels to match the breakpoints
  ) +
  labs(
    title = "Parallel coordinate plot of genre features\nand their intensity",
    subtitle = "Lines represent unique genres, with colour determined by popularity category",
    caption = "popularity categories based on adjusted track popularity IQR",
    colour = 'Track popularity', x = 'Features', y = 'Feature Intensity'
  )




#.csv output containing all parallel coordinate plot data
write.table(grouped_music_data.categorised, "Plot data for parallel coordinate plot.csv", row.names = T, sep = ",")


##----2.3 ~ Violin plot data ----

#Create dataset containing the 2 comparison categories
HiLo.pop<- adj.ms_cat%>%
  group_by(pop_category) %>%
  filter(pop_category %in% 
           c("Low popularity", "High popularity")
  )

#convert to long format for faceted plot
HiLo.pop_long <- HiLo.pop %>% 
  pivot_longer(cols = 3:11, 
               names_to = "feature",
               values_to = "value")

#.csv of Violin plot data utilised
write.table(HiLo.pop_long, "Violin plot data.csv", row.names = T, sep = ",")

###----2.31 ~ Violin plot ----

#Violin plot of high and low popularity
ggplot(HiLo.pop_long, aes(x = popularity, y = value, fill = pop_category )) + 
  geom_violin(trim = F, scale = "area") +
  facet_wrap(~feature, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = 0.575), #adjusts text position and orientation
        axis.ticks.length.x = unit(.2, "cm"), #adds ticks to connect feature name to line
        plot.caption = element_markdown(size = 8),
        legend.text = element_text(size =10),
        legend.title = element_text(size =14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = element_text(size = 16),) + 
  coord_flip() +
  labs(
    y = "Feature intensity",
    title = "Distribution of feature intensity by popularity category*",
    caption = "*Plot does <b>not</b> include popularity scores ranging from 0.17 - 0.3324",
    fill = "Popularity category"
  )

##----2.4 ~ PCA plot data ----

#we only utilise lowpop and highpop entries
HiLo.pop_fullFilter <- HiLo.pop %>% 
  filter(track_genre %in% c("pop-film", "k-pop", "chill",
                            "latin", "romance", "iranian")
  )


#creation of pca dataframe
pca_fullset_genres <- prcomp(HiLo.pop_fullFilter[2:11], scale = F)
pca_HiLo.pop_fullFilter <- data.frame(
  track_genre = HiLo.pop_fullFilter$track_genre,
  popularity = HiLo.pop_fullFilter$popularity,
  pop_category = HiLo.pop_fullFilter$pop_category,
  danceability = HiLo.pop_fullFilter$danceability,
  energy = HiLo.pop_fullFilter$energy,
  loudness = HiLo.pop_fullFilter$loudness,
  speechiness = HiLo.pop_fullFilter$speechiness,
  acousticness = HiLo.pop_fullFilter$acousticness,
  instrumentalness = HiLo.pop_fullFilter$instrumentalness,
  liveness = HiLo.pop_fullFilter$liveness, 
  valence = HiLo.pop_fullFilter$valence,
  tempo = HiLo.pop_fullFilter$tempo,
  PC1 = pca_fullset_genres$x[, 1],
  PC2 = pca_fullset_genres$x[, 2],
  PC4 = pca_fullset_genres$x[, 4]
)


#Exporting PCA rotation data
pca.rotation_df <- as.data.frame(pca_fullset_genres$rotation) # Creates dataframe
write.table(pca.rotation_df, "PCA results for High-Low popularity tracks.csv", row.names = T, sep = ",")


###----2.41 ~ PCA plot ----

#Plot of PCA data 
ggplot(pca_HiLo.pop_fullFilter, aes(x = PC1, y = PC2, color = pop_category)) +
  geom_point(alpha = 0.6) +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = 0.575), #adjusts text position and orientation
        axis.ticks.length.x = unit(.2, "cm"), #adds ticks to connect feature name to line
        plot.caption = element_markdown(size = 8), #allows for markdown editing; size readjustment
        legend.text = element_text(size =10), #size manipulation
        legend.title = element_text(size =14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), #aligns the y-axis title
        title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
  ) +
  scale_x_continuous(
    limits = c(-1.5,1.5),
    breaks = seq(-1,1, by = 0.5), #Sets breaks at -1, -0.5, 0, 0.5, and 1
    labels = c("More acoustic (-1)", "Less\nAcoustic (-0.5)", "mixed Energy &\nAcousticness (0)", "less Energetic\n(0.5)", "more Energetic (1)") #Custom labels
  ) +
  scale_y_continuous(
    limits = c(-1.1,0.8),
    breaks = seq(-1.1,0.9, by = 0.45),
    labels = c("Moody &\nInstrumental (-1)", "less Moody &\nInstrumental (-0.5)", "mixed Mood &\nInstrumentalness (0)", "less Upbeat &\nVocal (0.5)", "Upbeat &\nVocal (1)" )
  ) +
  coord_cartesian(xlim = c(-0.9, 1.25), ylim = c(-1.1,0.75 )) + #zooms the plot in
  labs(
    title = "Categorised music popularity and \ntheir musical traits*",
    subtitle = "Principal Component Analysis (PCA)",
    x = "Acoustic vs. Energetic",
    y = "Mood and Instrumentation",
    color = "Popularity Category",
    caption = "*Plot <b>excludes</b> popularity scores ranging from 0.17 - 0.3324"
  )

#----3.0 ~ Composite visualisation ----
##----3.1 ~ Violin Plot ----

plotV <- ggplot(HiLo.pop_long, aes(x = popularity, y = value, fill = pop_category )) + 
  geom_violin(trim = F, scale = "area") +
  facet_wrap(~feature, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = 0.575), #adjusts text position and orientation
        axis.ticks.length.x = unit(.2, "cm"), #adds ticks to connect feature name to line
        plot.caption = element_markdown(size = 8, vjust = 6,hjust = 1),
        legend.position = "none",
        axis.title.x = element_text(size = 14, vjust = 1),
        axis.title.y = element_text(size = 14, vjust = 2),
        title = element_text(size = 16),
        plot.subtitle = element_markdown(size = 10)) + 
  coord_flip() +
  labs(
    y = "Feature intensity", x = "Popularity",
    title = "Distribution of feature intensity by popularity category*",
    subtitle = "<span style='color:red;'>Red</span> plots represent high popularity tracks, 
    while <span style='color:blue;'>blue</span> plots represent low popularity tracks",
    caption = "*Plot does <b>not</b> include popularity scores ranging from 0.17 - 0.3324",
    fill = "Popularity category"
  )


##----3.2 ~ Parallel Coordinate plot ----
plotPCoord <- ggparcoord(grouped_music_data.categorised, 
                         columns = c(2:11), #Selects my music feature columns 
                         groupColumn = 12, #Selects my colour variable to determine line grouping
                         alphaLines = 0.6) + #Improves Readability
  geom_vline(xintercept = c(2:11), alpha = 0.2) + #Adds lines to identify feature points
  theme_bw() + #changes theme
  theme(axis.text.x = element_text(angle = 45, vjust = 0.575), #adjusts text position and orientation
        axis.ticks.length.x = unit(.2, "cm"), #adds ticks to connect feature name to line
        plot.caption = element_markdown(size = 8),
        legend.text = element_text(size =10),
        legend.title = element_text(size =14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = element_text(size = 16),
        plot.subtitle = element_text(size = 10)
  ) + 
  scale_x_discrete(expand = expansion(add = c(0,0))) + #Aligns category 1 (popularity) to start of plot
  scale_y_continuous(
    breaks = scales::rescale(seq(0, 1, by = 0.25), from = c(0, 1), to = c(-5, 10)), #reformats breakpoints for Y-axis ticks
    labels = seq(0, 1, by = 0.25) #labels to match the breakpoints
  ) +
  labs(
    title = "Parallel coordinate plot of genre features and their intensity",
    subtitle = "Lines represent unique genres, with colour determined by popularity category",
    caption = "popularity categories based on adjusted track popularity IQR",
    colour = 'Track popularity', x = 'Features', y = 'Feature Intensity'
  )




##----3.3 ~ Radar Chart ----
plotR <- ggradar (filtered_genres,
                  values.radar = c("0", "0.5", "1"),
                  grid.label.size = 5,
                  gridline.label.offset = -0.175,
                  axis.label.size = 4,
                  legend.text.size = 8,
                  group.line.width = 0.7,
                  group.point.size = 1.7,
                  legend.title = "Filtered Genres",
                  legend.position = "right",
                  plot.title = "Feature intensity\nacross genres",
                  background.circle.colour = "#aeada9",
                  group.colours = preset_colours,
                  fill = TRUE, fill.alpha = 0.07,
                  gridline.min.colour = "black",
                  gridline.mid.colour = "blue",
                  gridline.max.colour = "grey",
)

plotR <- plotR + theme(
  legend.title = element_text(size = 12),
  plot.title = element_text(hjust = 0.3)
)


##----3.4 ~ PCA plot ----

PlotPCA <- ggplot(pca_HiLo.pop_fullFilter, aes(x = PC1, y = PC2, color = pop_category)) +
  geom_point(alpha = 0.6) +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = 0.575), #adjusts text position and orientation
        axis.ticks.length.x = unit(.2, "cm"), #adds ticks to connect feature name to line
        plot.caption = element_markdown(size = 8), #allows for markdown editing; size readjustment
        legend.text = element_text(size =10), #size manipulation
        legend.title = element_text(size =14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14), #aligns the y-axis title
        title = element_text(size = 16),
        plot.subtitle = element_text(size = 12),
  ) +
  scale_x_continuous(
    limits = c(-1.5,1.5),
    breaks = seq(-1,1, by = 0.5), #Sets breaks at -1, -0.5, 0, 0.5, and 1
    labels = c("More acoustic (-1)", "Less\nAcoustic (-0.5)", "mixed Energy &\nAcousticness (0)", "less Energetic\n(0.5)", "more Energetic (1)") #Custom labels
  ) +
  scale_y_continuous(
    limits = c(-1.1,0.8),
    breaks = seq(-1.1,0.9, by = 0.45),
    labels = c("Moody &\nInstrumental (-1)", "less Moody &\nInstrumental (-0.5)", "mixed Mood &\nInstrumentalness (0)", "less Upbeat &\nVocal (0.5)", "Upbeat &\nVocal (1)" )
  ) +
  coord_cartesian(xlim = c(-0.9, 1.25), ylim = c(-1.1,0.75 )) + #zooms the plot in
  labs(
    title = "Categorised music popularity and \ntheir musical traits*",
    subtitle = "Principal Component Analysis (PCA)",
    x = "Acoustic vs. Energetic",
    y = "Mood and Instrumentation",
    color = "Popularity Category",
    caption = "*Plot <b>excludes</b> popularity scores ranging from 0.17 - 0.3324"
  )

##----3.5 ~ composite plot ----

#Gridextras used to arrange data in a structured manner
grid.arrange(plotV, plotPCoord, plotR, PlotPCA, ncol= 2)






##---- TEST PLOTS ----

ggparcoord(grouped_music_data.Qcat, 
           columns = c(2:11), #Selects my music feature columns 
           groupColumn = 12, #Selects my colour variable to determine line grouping
           alphaLines = 0.4) + #Improves Readability
  geom_vline(xintercept = c(2:11), alpha = 0.3) + #Adds lines to identify feature points
  theme_bw() + #changes theme
  theme(axis.text.x = element_text(angle = 45, vjust = 0.575), #adjusts text position and orientation
        axis.ticks.length.x = unit(.2, "cm"), #adds ticks to connect feature name to line
        plot.caption = element_markdown(size = 8),
        legend.text = element_text(size =10),
        legend.title = element_text(size =14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        title = element_text(size = 16),
        plot.subtitle = element_text(size = 10)
  ) + 
  scale_x_discrete(expand = expansion(add = c(0,0))) + #Aligns category 1 (popularity) to start of plot
  scale_y_continuous(
    breaks = scales::rescale(seq(0, 1, by = 0.25), from = c(0, 1), to = c(-5, 10)), #reformats breakpoints for Y-axis ticks
    labels = seq(0, 1, by = 0.25) #labels to match the breakpoints
  ) +
  labs(
    title = "Parallel coordinate plot of genre features\nand their intensity",
    subtitle = "Lines represent unique genres, with colour determined by popularity category",
    caption = "popularity categories based on adjusted track popularity IQR",
    colour = 'Track popularity', x = 'Features', y = 'Feature Intensity'
  )