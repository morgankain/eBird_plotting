########################
## Explore ebird data ##
########################

####
## Needed packages -- INSTALL THEM IF THEY ARE NOT INSTALLED ALREADY !!!
####

needed_packages <- c(
  "dplyr"
, "ggplot2"
, "sf"
, "rnaturalearth"
, "rnaturalearthdata"
, "ggspatial"
, "mapdata"
)

if (length(setdiff(needed_packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(needed_packages, rownames(installed.packages())))  
}

lapply(needed_packages, require, character.only = TRUE)

source("ggplot_theme.R")

ebird <- read.csv("MyEBirdData.csv")

ebird$year <- apply(matrix(ebird$Date), 1
  , FUN = function(x) as.numeric(strsplit(as.character(x), split = "-")[[1]][1]))

ebird$month <- apply(matrix(ebird$Date), 1
  , FUN = function(x) as.numeric(strsplit(as.character(x), split = "-")[[1]][2]))

ebird <- ebird %>% mutate(day = as.numeric(format(as.Date(Date), "%j")))

all.years <- sort(unique(ebird$year))

ebird.spec.accum <- data.frame(year = 0, day = 0, spec = 0); ebird.spec.accum <- ebird.spec.accum[-1, ]

for (i in seq_along(all.years)) {
  
  temp_ebird  <- ebird %>% filter(year == all.years[i]) %>% droplevels()
  unique_spec <- unique(temp_ebird$Common.Name)
  
  all.days <- sort(unique(temp_ebird$day))
  
  for (j in seq_along(all.days)) {
    
    j = j + 1
  spec.day        <- temp_ebird %>% filter(day == all.days[j]) %>% 
    summarize(uni_spec = unique(Common.Name)) %>% unlist() %>% unname()
  
  spec.day.unique <- match(spec.day, unique_spec)
  spec.day.unique <- spec.day.unique[which(!is.na(spec.day.unique))]
    
  ebird.spec.accum <- rbind(ebird.spec.accum, 
    data.frame(
      year  = all.years[i]
    , day   = all.days[j]
    , spec  = ifelse(length(spec.day.unique) > 0, length(spec.day.unique), 0)
    ))
  
  if (length(spec.day.unique) > 0) {
    unique_spec <- unique_spec[-spec.day.unique]
  }
      
  }
}

ebird.spec.accum <- ebird.spec.accum %>% group_by(year) %>% mutate(cumspec = cumsum(spec))
ebird.spec.accum <- ebird.spec.accum %>% mutate(date = as.Date(day, origin = paste(year, "01-01", sep = "-")))

date_axis_breaks <- c(1, 32, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335)
date_axis_ticks  <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")

  ggplot(ebird.spec.accum, aes(day, cumspec)) +
    geom_line(aes(colour = as.factor(year)), lwd = 1) +
  xlab("Date") +
  ylab("Cummulative Species") +
 scale_x_continuous(breaks = date_axis_breaks, labels = date_axis_ticks) +
  scale_color_discrete(name = "Year") +
    theme(
    legend.key.size = unit(1.0, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 12)
  , legend.background = element_blank()
  , axis.text.y = element_text(size = 14) 
  , axis.title.x = element_text(size = 16) 
  , axis.title.y = element_text(size = 16)
  )

ebird %>% arrange(desc(Distance.Traveled..km.)) %>% head()

ebird %>% group_by(Submission.ID, month, year) %>%
  summarize(
    num.spec = length(unique(Common.Name))
  , duration = mean(Duration..Min.)
  , distance = mean(Distance.Traveled..km.)) %>%
  filter(!is.na(duration), !is.na(distance)) %>% {
 
ggplot(., aes(duration, num.spec)) + 
  geom_point(aes(colour = log10(distance)), lwd = 3) +
  xlab("Minutes") +
  ylab("Species") +
  scale_color_continuous(name = "Distance Traveled (Km)") +
    theme(
    legend.key.size = unit(1.0, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 12)
  , legend.background = element_blank()
  , axis.text.y = element_text(size = 14) 
  , axis.title.x = element_text(size = 16) 
  , axis.title.y = element_text(size = 16)
  ) + facet_wrap(~year)
       
  }

ebird %>% group_by(Common.Name) %>% summarize(num_lists = n()) %>% {
  
  ggplot(., aes(x = num_lists)) +
    geom_histogram(bins = 100) +
    xlab("Number of Lists") +
    ylab("Species Entries")
  
}

world      <- ne_countries(scale = "medium", returnclass = "sf")
MainStates <- map_data("state")

{
ggplot(data = world) +
    geom_sf() +
   geom_polygon( data=MainStates, aes(x=long, y=lat, group=group),
                color="black", fill="grey90" ) +
    annotation_scale(location = "bl", width_hint = 0.5) +
    annotation_north_arrow(location = "bl", which_north = "true", 
        pad_x = unit(0.75, "in"), pad_y = unit(0.5, "in"),
        style = north_arrow_fancy_orienteering) +
    coord_sf(xlim = c(-123, -55), ylim = c(18, 55)) +
  geom_point(
    data = (ebird %>% group_by(Location.ID, year) %>% summarize(
      num_spec  = length(unique(Common.Name))
    , num_list  = length(unique(Submission.ID))
    , Latitude  = Latitude[num_spec]
    , Longitude = Longitude[num_spec]
    ))
  , aes(x = Longitude, y = Latitude, size = num_spec
    , colour = year
    )
  , alpha = 0.75
  ) +
  scale_size_continuous(name = "Number
of Species", breaks = seq(10, 100, by = 10)) +
#  scale_colour_gradient(name = "Number
#of Lists", low = "blue2", high = "red2") +
  scale_colour_gradient(name = "Year", low = "blue2", high = "red2") +
  xlab("Latitude") +
  ylab("Longitude") +
  theme(
    legend.key.size = unit(0.75, "cm")
  , legend.title = element_text(size = 16)
  , legend.text = element_text(size = 12)
  , legend.background = element_blank()
  , axis.text.y = element_text(size = 14) 
  , axis.title.x = element_text(size = 16) 
  , axis.title.y = element_text(size = 16)
  )
}
