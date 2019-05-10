#Loading the Packages
library("dplyr")
library("ggplot2")
library("tidyr")
library("plotly")
library("lubridate")

#Read the data set
shooting_data <- read.csv("data/shootings-2018.csv", stringsAsFactors = FALSE)

#How many shootings occurred? 
num_shootings <- nrow(shooting_data)

#How many lives were lost?
lives_lost <- shooting_data %>%
  select(num_killed) %>%
  summarize(total_killed = sum(num_killed)) %>%
pull()
#Which city was most impacted by shootings (make sure to clarify  
#how you are measuring "impact")?

#Most impacted by shooting is the city with the most number of people killed in
#2018
most_impacted_city <- shooting_data %>%
  group_by(city) %>%
  summarize(most_people_killed = sum(num_killed)) %>%
  filter(most_people_killed == max(most_people_killed)) %>%
pull(city)
  
#Two other insights of your choice

#Which City had the most injuries in 2018 and how many?
most_injured_city <- shooting_data %>%
  group_by(city) %>%
  summarize(most_people_injured = sum(num_injured)) %>%
  filter(most_people_injured == max(most_people_injured)) %>%
pull(city)

#How many people were injured in 2018?
num_people_injured <- shooting_data %>%
  select(num_injured) %>%
  summarize(total_injured = sum(num_injured)) %>%
pull()

#Summary Table
most_impacted_state <- shooting_data %>%
  group_by(state) %>%
  summarize(num_killed_by_state = sum(num_killed)) %>%
  mutate(years_lost = num_killed_by_state * 49) %>%
  arrange(desc(num_killed_by_state)) %>%
  top_n(5)


## Description of Single Incident Information
data_of_single_incident <- shooting_data %>%
  filter(address == "12603 Renton Ave S")

date_of_incident <- data_of_single_incident %>%
  select(date) %>%
pull()

city_of_incident <- data_of_single_incident %>%
  select(city) %>%
pull()

state_of_incident <- data_of_single_incident %>%
  select(state) %>%
  pull()

num_injured_incident <- data_of_single_incident %>%
  select(num_injured) %>%
  pull()

num_killed_incident <- data_of_single_incident %>%
  select(num_killed) %>%
  pull()

num_incident_in_state <- shooting_data %>%
  filter(state == "Washington") %>%
  nrow(.)

num_incident_in_ds <- shooting_data %>%
  nrow()

map_shape <- list(
  scope = "usa",
  projection = list(type = "usa"),
  showland = TRUE,
  landcolor = toRGB("grey95"),
  countrywidth = 1,
  subunitwidth = 1
)

map_i <- plot_geo(
  shooting_data,
  lon = ~long,
  lat = ~lat) %>%
  add_markers(
    text = ~paste(paste("City: ", shooting_data$city),
           paste("Number Killed: ", shooting_data$num_killed),
           paste("Number Injured: ", shooting_data$num_injured),
           sep = "<br />"),
    marker = list(color = "blue", size = (~shooting_data$num_killed * 2) ),
    hoverinfo = "text"
  ) %>%
  layout(
    title = "Shootings in the United States (2018)",
    geo = map_shape
  )

top_10_cities <- shooting_data %>%
  group_by(city) %>%
  summarize(total_killed = sum(num_killed)) %>%
  arrange(desc(total_killed)) %>%
  top_n(10)

top_10_cities_plot <- ggplot(top_10_cities) +
  geom_col(mapping = aes(x = city, y = total_killed), fill = "maroon") +
  geom_hline(yintercept = mean(top_10_cities$total_killed), color = "blue") +
  ggtitle("Cities with Highest Fatalities") +
  coord_flip()
