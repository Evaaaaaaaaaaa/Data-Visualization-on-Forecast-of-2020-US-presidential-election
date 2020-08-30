# install and load library
# for plot
install.packages("ggplot2")
library(ggplot2)
# for join/merge
install.packages("dplyr")
library(dplyr)
# for drawing scatter pie
install.packages("scatterpie")
library(scatterpie)

# original data retrieved from https://data.fivethirtyeight.com/
# at election-forecasts-2020	2020 Election Forecast from this page

# data has been pre-processed in excel, only the newest relevent data from 
# 17.08.2020 will be used , for the visualization.
elections_map <- read.csv(
  "/Users/eva/Desktop/asm1/presidential_state_toplines_2020.csv", 
  stringsAsFactors=FALSE)


# select data we need
elections_map <- elections_map[,c(8,14,15,17)]


# get data used to draw the map
states_map <- map_data("state")
head(states_map)



# get the state center, this is more accurate compare to using the average
# of the long and lat in states_map for each state, from experiments.
states_centers <- as.data.frame(state.center)
# get state name for merge later
states_centers$name <- state.name

# get abbreviation which will be present on the map (pie chart) to specify 
# state name and remove rows that is not included in the election set.
states_centers$abb <- state.abb

# replace column name state with name, ready to join the the 2 sets together
elections_map$name <- elections_map$state
elections_map$state = NULL

# join, this will be used to plot the pie chart and the state abbreviation
elections_map <- inner_join(states_centers, elections_map,by = "name")


# This section will move the pie chart some states out of the map to 
# avoid overlapping

# move pie chart for state Massachusetts
elections_map$x[elections_map$name == "Massachusetts"] <- 
  elections_map$x[elections_map$name == "Massachusetts"] +4
# get the arrow data ready
arrowA <- data.frame(x1 =  elections_map$x[elections_map$name == 
                                             "Massachusetts"]-4, 
                     x2 =  elections_map$x[elections_map$name == 
                                             "Massachusetts"]-1.2, 
                     y1 = elections_map$y[elections_map$name == 
                                            "Massachusetts"], 
                     y2 = elections_map$y[elections_map$name == 
                                            "Massachusetts"])

# move pie chart for state Rhode Island
elections_map$x[elections_map$name == "Rhode Island"] <- 
  elections_map$x[elections_map$name == "Rhode Island"] +3
elections_map$y[elections_map$name == "Rhode Island"] <- 
  elections_map$y[elections_map$name == "Rhode Island"] -3
# get the arrow data ready
arrowB <- data.frame(x1 =  elections_map$x[elections_map$name == 
                                             "Rhode Island"]-3, 
                     x2 =  elections_map$x[elections_map$name == 
                                             "Rhode Island"]-1, 
                     y1 = elections_map$y[elections_map$name == 
                                            "Rhode Island"]+3, 
                     y2 = elections_map$y[elections_map$name == 
                                            "Rhode Island"]+1)

# move pie chart for state Delaware
elections_map$x[elections_map$name == "Delaware"] <- 
  elections_map$x[elections_map$name == "Delaware"] +3
elections_map$y[elections_map$name == "Delaware"] <- 
  elections_map$y[elections_map$name == "Delaware"] -3
# get the arrow data ready
arrowC <- data.frame(x1 =  elections_map$x[elections_map$name == "Delaware"]-3, 
                     x2 =  elections_map$x[elections_map$name == "Delaware"]-1, 
                     y1 = elections_map$y[elections_map$name == "Delaware"]+3, 
                     y2 = elections_map$y[elections_map$name == "Delaware"]+1)

# move pie chart for state Vermont
elections_map$y[elections_map$name == "Vermont"] <- 
  elections_map$y[elections_map$name == "Vermont"] + 3
# get the arrow data ready
arrowD <- data.frame(x1 =  elections_map$x[elections_map$name == "Vermont"], 
                     x2 =  elections_map$x[elections_map$name == "Vermont"], 
                     y1 = elections_map$y[elections_map$name == "Vermont"]-3, 
                     y2 = elections_map$y[elections_map$name == "Vermont"]-1.2)


#finally, we can plot everything out!
ggplot() +
  # draw map
  geom_polygon(data = states_map , aes(x = long, y = lat, group = region), 
               colour = "white", fill = "black")+
  # draw the 4 arrows that points to the pie charts that are be moved by a 
  # small distance to prevent overlapping
  geom_segment(data = arrowA,aes(x = x1, y = y1, xend = x2, yend = y2), 
               color = "white", arrow = arrow(length = unit(0.2, "cm"),
               type = "closed"),lineend = "round")+
  geom_segment(data = arrowB,aes(x = x1, y = y1, xend = x2, yend = y2), 
               color = "white", arrow = arrow(length = unit(0.2, "cm"),
               type = "closed"),lineend ="round")+
  geom_segment(data = arrowC,aes(x = x1, y = y1, xend = x2, yend = y2), 
               color = "white", arrow = arrow(length = unit(0.2, "cm"),
               type = "closed"),lineend = "round")+
  geom_segment(data = arrowD,aes(x = x1, y = y1, xend = x2, yend = y2), 
               color = "white", arrow = arrow(length = unit(0.2, "cm"),
               type = "closed"),lineend = "round")+
  
  # draw the pie chart, according to the data, distribution of people supporting 
  # Trump, Bidden, and others.
  geom_scatterpie(data = elections_map, aes(x=x, y=y, group=name), 
                  cols=c("voteshare_inc","voteshare_chal","voteshare_other"), 
                  color=NA) + coord_equal()+
  # add the abbreviation on top of the pie, use light color to make it
  # less stand out
  geom_text(data=elections_map, aes(x, y, label = abb),
            color = "#c2c2c2", size=4) +
  # a completely empty theme
  theme_void()+
  # add legend title, title, and subtitle
  labs(fill = "Candidate",
           title = "Forecast of the 2020 United States Presidential Election",
            subtitle = 
                  "Vote share distribution between candidates of each state")+
  # add theme, legend position at the right
  theme(legend.position = "bottom",
        # add legend text and legend title color
        legend.text = element_text(colour = "white"),
        legend.title = element_text(colour = "white",face = "bold"),
        # add margin of the graph
        plot.margin = unit(c(2,2,2,2), "cm"),
        # background color, 
        plot.background = element_rect("black"),
        # add subtitle and title adjustment
        plot.subtitle = element_text(color = "white", hjust = 0.5, size = 18), 
        plot.title = element_text(colour = "white", hjust = 0.5, size = 30))+
  # add color for the pie chart and legend
  scale_fill_manual(labels = c("Joe Biden", "Donald Trump","Others"),
                        values = c("#275b96","#bf4343","white"))