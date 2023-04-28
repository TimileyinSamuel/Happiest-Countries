### Loading libraries
library(tidyverse)
library(tidylog)
library(ggbump)
library(ggimage)
library(showtext)


### Adding Fonts
font_add_google(name = "Open Sans", family = "open_sans")
font_add_google(name = "Roboto", family = "Roboto")
font_add_google(name = "Montserrat", family = "Montserrat")
font_add_google(name = "Cinzel", family = "Cinzel")
font_add_google(name = "Carter One", family = "Carter One")

showtext_auto()

### Loading Data sets
df <- read_csv2("./hapiness_data.csv") |> 
  janitor::clean_names()
df

### Converting data set to long format
df <- pivot_longer(data = df,
             cols = x2016:x2023,
             names_to = "year",
             values_to = "rank",
             names_prefix = "x")
df

### filtering data
df <- df |>
filter(rank < 11)

### adding flag
df <- df |> 
  mutate(flag = case_when(country == "Switzerland" ~ "https://raw.githubusercontent.com/TimileyinSamuel/Happiest-Countries/main/Flags/Switzerland.png",
                            country == "Iceland" ~ "https://raw.githubusercontent.com/TimileyinSamuel/Happiest-Countries/main/Flags/Iceland.png",
                            country == "Denmark" ~ "https://raw.githubusercontent.com/TimileyinSamuel/Happiest-Countries/main/Flags/Denmark.png",
                            country == "Norway" ~ "https://raw.githubusercontent.com/TimileyinSamuel/Happiest-Countries/main/Flags/Norway.png",
                            country == "Canada" ~ "https://raw.githubusercontent.com/TimileyinSamuel/Happiest-Countries/main/Flags/Canada.png",
                            country == "Finland" ~ "https://raw.githubusercontent.com/TimileyinSamuel/Happiest-Countries/main/Flags/Finland.png",
                            country == "Netherlands" ~ "https://raw.githubusercontent.com/TimileyinSamuel/Happiest-Countries/main/Flags/Netherlands.png",
                            country == "Sweden" ~ "https://raw.githubusercontent.com/TimileyinSamuel/Happiest-Countries/main/Flags/Sweden.png",
                            country == "New Zealand" ~ "https://raw.githubusercontent.com/TimileyinSamuel/Happiest-Countries/main/Flags/New%20Zealand.png",
                            country =="Australia" ~ "https://raw.githubusercontent.com/TimileyinSamuel/Happiest-Countries/main/Flags/Australia.png",
                            country == "Israel" ~ "https://raw.githubusercontent.com/TimileyinSamuel/Happiest-Countries/main/Flags/Israel.png",
                            country == "Austria" ~ "https://raw.githubusercontent.com/TimileyinSamuel/Happiest-Countries/main/Flags/Austria.png",
                            country == "Luxembourg" ~ "https://raw.githubusercontent.com/TimileyinSamuel/Happiest-Countries/main/Flags/Luxembourg.png"))

### adding position
df <- df |> 
  mutate(position = paste0("#", rank))

### Adding title
title <- "Tracking Happiness: The World's Top 10 Happiest Countries Over the Years"
caption <- "Data: The World Hapiness Report | Graphic: @Timmy1Tesla"

#### Plotting
ggplot(data = df, aes(x = year, y = desc(rank), group = country)) +
  geom_bump(linewidth = 1.4,smooth = 10, colour = "grey75", lineend = 'round') +
  geom_bump(data = df |> filter(country == "Denmark"),
    linewidth = 1.4,smooth = 10, colour = "#C8102E", lineend = 'round') +
  geom_bump(data = df |> filter(country == "Sweden"),
            linewidth = 1.4,smooth = 10, colour = "#006AA7", lineend = 'round') +
  geom_bump(data = df |> filter(country == "Finland"),
            linewidth = 1.4,smooth = 10, colour = "#002F6C", lineend = 'round') +
  geom_bump(data = df |> filter(country == "New Zealand"),
            linewidth = 1.4,smooth = 10, colour = "#000000", lineend = 'round') +
  geom_image(data = df |> filter(year == min(year)), mapping = aes(x = year, image = flag),
             size=0.028, asp=12/7.5) +
  geom_image(data = df |> filter(year == max(year)), mapping = aes(x = year, image = flag),
             size=0.028, asp=12/7.5) +
   geom_image(data = df |> filter(year == 2018, country == "Australia"), mapping = aes(x = year, image = flag),
             size=0.028, asp=12/7.5) +
  geom_image(data = df |> filter(year == 2019, country == "Canada"), mapping = aes(x = year, image = flag),
             size=0.028, asp=12/7.5) +
  geom_image(data = df |> filter(year == 2020, country == "Luxembourg"), mapping = aes(x = year, image = flag),
             size=0.028, asp=12/7.5) +
  geom_image(data = df |> filter(year == 2019, country == "Austria"), mapping = aes(x = year, image = flag),
             size=0.028, asp=12/7.5) +
  geom_image(data = df |> filter(year == 2021, country == "Austria"), mapping = aes(x = year, image = flag),
             size=0.028, asp=12/7.5) +
  geom_image(data = df |> filter(year == 2022, country == "Israel"), mapping = aes(x = year, image = flag),
             size=0.028, asp=12/7.5) +
  geom_text(data = df |> filter(year == max(year)),
            mapping = aes(x = year, label = paste(position, country)),hjust = 0.40,
            vjust = -2.1, family = "Montserrat") +
  geom_text(data = df |> filter(year == min(year)),
            mapping = aes(x = year, label = paste(position, country)), hjust = 0.65,
            vjust = -2.1, family = "Montserrat") +
  geom_text(data = df |> filter(year == 2019, country == "Austria"), 
            mapping = aes(x = year, label = country,hjust = 0.45,
            vjust = -2.1, family = "Montserrat")) +
  scale_x_discrete(guide = guide_axis(position = "bottom")) +
  labs(title = title,
       caption = caption) +
    theme(
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
    axis.text.x = element_text(size = 10.5, family = "Montserrat", face = "bold"),
    panel.background = element_rect(fill = "#FEFCFF", colour = "#FEFCFF"),
    plot.background = element_rect(fill = "#FEFCFF", colour = "#FEFCFF"),
    plot.title = element_text(size = 20, family = "Carter One", hjust = 0.5, margin = 
                                margin(b = 16.5, t = 18.5)),
    plot.caption = element_text(size = 10.0, family = "Montserrat", hjust = 0.99,
                                margin = margin(t = 15)),
    legend.position = "none")

### Saving the plot
ggsave(filename ="hapinessplot.png", height = 8.8, width = 12.5, units = "in")
