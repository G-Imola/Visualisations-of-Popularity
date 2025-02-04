#Box plot comparison for violin plot

full.cat_music_data_long <- full.cat_music_data %>% 
  pivot_longer(cols = 3:11, 
               names_to = "feature",
               values_to = "value")


ggplot(HiLo.pop_long, aes(x = popularity, y = value, fill = pop_category )) + 
  geom_boxplot() +
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


#Violin plot using 3 categories
ggplot(full.cat_music_data_long, aes(x = popularity, y = value, fill = pop_category )) + 
  geom_violin(trim = F, scale = "area") +
  facet_wrap(~feature, scales = "free_y") +
  theme_bw() +
  theme(axis.text.x = element_text(vjust = 0.575), #adjusts text position and orientation
        axis.ticks.length.x = unit(.2, "cm"), #adds ticks to connect feature name to line
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
     <span style='color:green;'>green</span> plots represent mid-range popularity tracks, while <span style='color:blue;'>blue</span> plots represent low popularity tracks",
    fill = "Popularity category"
  )

#Hi-lo pop separated by quantiles
HiLo.Quantpop <- music_data.adj %>%
  mutate(
    pop_category = case_when(
      popularity <= quantile(popularity, 0.3333) ~ "Low popularity",
      popularity > quantile(popularity, 0.3333) & popularity <= quantile(popularity, 0.6667) ~ "medium popularity",
      popularity > quantile(popularity,0.6667) ~ "High popularity"
    )
  )
HiLo.Quantpop <- HiLo.Quantpop %>%
  filter(pop_category == c("Low popularity", "High popularity")) %>%
  pivot_longer(cols = 3:11, 
               names_to = "feature",
               values_to = "value")


ggplot(HiLo.Quantpop, aes(x = popularity, y = value, fill = pop_category )) + 
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

HiLogrouped_music_data.adj <- grouped_music_data.Qcat


#PCA analysis 

Filter.adj.music_data <- adj.ms_cat %>% 
  filter(track_genre %in% c("pop-film", "k-pop", "chill",
                            "cantopop", "children", "edm", "rock-n-roll",
                            "latin", "romance", "iranian")
  )
rm(pcaFilter.adj.music_data)

#creation of pca dataframe
better.pca <- prcomp(Filter.adj.music_data[2:11], scale = F)
better.pca_DF <- data.frame(
  track_genre = Filter.adj.music_data$track_genre,
  popularity = Filter.adj.music_data$popularity,
  pop_category = Filter.adj.music_data$pop_category,
  danceability = Filter.adj.music_data$danceability,
  energy = Filter.adj.music_data$energy,
  loudness = Filter.adj.music_data$loudness,
  speechiness = Filter.adj.music_data$speechiness,
  acousticness = Filter.adj.music_data$acousticness,
  instrumentalness = Filter.adj.music_data$instrumentalness,
  liveness = Filter.adj.music_data$liveness, 
  valence = Filter.adj.music_data$valence,
  tempo = Filter.adj.music_data$tempo,
  PC1 = better.pca$x[, 1],
  PC2 = better.pca$x[, 2]
)


#imports fonts (courier)
font_import(prompt = F)
loadfonts(device = "win")

ggplot(better.pca_DF, aes(x = PC1, y = PC2, color = pop_category)) +
  geom_point(alpha = 0.6) +
  facet_wrap(~pop_category, scales = "free_y") +
  theme_bw() +
  theme(text = element_text(family = "Courier New"),
        axis.text.x = element_text(angle = 45, vjust = 0.575), #adjusts text position and orientation
        axis.ticks.length.x = unit(.2, "cm"), #adds ticks to connect feature name to line
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
    title = "Categorised music popularity and \ntheir musical traits",
    subtitle = "Principal Component Analysis of top/bottom 3\nand median 4 popularity genres",
    x = "Acoustic vs. Energetic",
    y = "Mood and Instrumentation",
    color = "Popularity Category",
  )
  

#radar plot alternatives

#Faceted clustered bar chart

filtered_genres.long <- filtered_genres %>%
  pivot_longer(cols = 3:11,
               names_to = "feature",
               values_to = 'value')

ggplot(filtered_genres.long, aes(x = reorder(track_genre, popularity, FUN = mean), y = value, fill = popularity)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ feature, scales = "free_y") +
  scale_fill_gradient(low = "lightblue", high = "darkblue") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, vjust = 0.575)) +
  labs(title = "Relationship between music feature intensity and genres",
       x = "Genre (ordered by increasing popularity)",
       y = "Feature intensity",
       fill = "Popularity")


#----Heatmap ----

#creates long column of radarplot data, and adds additional popularity column
filtered_genres.longPop <- filtered_genres %>%
  mutate(popcol = popularity) %>%
  mutate(track_genre = fct_reorder(track_genre,popcol, .fun = mean)) %>% #reorders the genres in descending mean popularity
  pivot_longer(cols = 2:11,
               names_to = "feature",
               values_to = 'value')

#heatmap
ggplot(filtered_genres.longPop,aes(x = feature, y = track_genre, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(
    colors = brewer.pal(9, "PRGn"),
    name = "intensity of feature") +
  theme_minimal() + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  labs(
    x = "Musical Features within tracks", y = "Track Genre",
    title = "Heatmap of genre and musical feature intensity",
    subtitle = "The relationship between genres and musical features",
  )





