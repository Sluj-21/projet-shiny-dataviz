library(tidyverse)

weather_path <- "weatherAUS.csv/weatherAUS.csv"

weather <- read.csv(weather_path)

weather$Location <- factor(weather$Location)

# Visualisation du jeu de données
head(weather)
summary(weather)

# Visualisation et répartition des NAs
colSums(is.na(weather))
colMeans(is.na(weather)) * 100

na_df <- data.frame(
  variable = names(weather),
  nb_NA = colSums(is.na(weather))
)

ggplot(na_df, aes(x = reorder(variable, nb_NA), y = nb_NA)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Variable",
    y = "Nombre de NA",
    title = "Nombre de valeurs manquantes par variable"
  ) +
  theme_minimal()

# Visualisation de la variable Sunshine
summary(df$Sunshine)

df <- na.omit(weather)
dim(df)
