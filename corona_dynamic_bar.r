#Corona Data - https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide
#Country Code and Coordinate JSON - https://gist.github.com/sindresorhus/1341699

#Load the Libraries
library(tidyverse)
library(readxl)
library(jsonlite)
library(ggthemes)
library(lubridate)
library(gganimate)
library(gifski)

#Read the Corona data from
#European Centre for Disease Prevention and Control (ECDC)
corona_df <- read_excel('C:/Corona/COVID-19-2020-03-27.xlsx')
#Verify the data
head(corona_df)

#Read the Country Code, latitude and longitude
cjson <- fromJSON('C:/Corona/countrycode-latlong-array.json')

#Make a dataframe from the JSON

#Custom Fucntion
convertList <- function(x){
  lat <<- append(lat, unlist(x[1]))
  long <<- append(long, unlist(x[2]))
}

#Get the country names
countryNames <- unlist(attributes(cjson))
names(countryNames) <- ""

#Extract latitude and longitude from the list
lat <<- c()
long <<- c()

lapply(cjson, convertList)

#Generate the Dataframe
djson <- data.frame(as.character(countryNames), as.numeric(lat), as.numeric(long))
colnames(djson) <- c('GeoId', 'lat', 'long')
djson$GeoId <- toupper(as.character(djson$GeoId))

#Join both the data sets
corona_df <- corona_df %>% 
  rename("GeoId" = "geoId", "DateRep" = "dateRep", "Cases" = "cases") %>%
  inner_join(djson)

#Generate Cumulative dataframe for each date.
cumsum_corona_df <- corona_df %>% 
  #rename("GeoId" = "geoId", "DateRep" = "dateRep", "Cases" = "cases") %>%
  arrange(GeoId, DateRep) %>% 
  group_by(GeoId) %>%
  mutate("cumsum_cases" = cumsum(Cases)) %>% 
  arrange(GeoId, DateRep)

#Select the top Countries Combining till date
top10_df <- corona_df %>% 
  #rename("Cases" = "cases") %>%
  group_by(countriesAndTerritories) %>%
  summarise(total_cases = sum(Cases)) %>% 
  arrange(desc(total_cases)) %>% 
  head(n=10)


#plotting the top 10 countries -Static Plot
ggplot(top10_df,
       aes(reorder(countriesAndTerritories,total_cases),
           total_cases,fill=as.factor(countriesAndTerritories)))+
  geom_col()+
  coord_flip(clip = "off", expand = FALSE)+
  guides( fill = FALSE) +
  labs(title="TOTAL CASES PER COUNTRY TILL DATE - THE TOP 10", 
       y="Total Cases Per Country", x = "Country")+
  scale_y_continuous(labels = scales::comma) +
  geom_text(aes(label = paste(total_cases,"")), hjust = 1)

###############Code for Dynamic Bar Plot ##############################

#Generate the dataset which will have top 10 countries
#for cumulative cases for each date
top10_df_cumsum <- ungroup(cumsum_corona_df) %>% 
  group_by(DateRep) %>%
  top_n(n=10, wt=cumsum_cases)

#Validate data for couple of fields
top10_df_cumsum[top10_df_cumsum$DateRep == 
                  as.Date("2020-01-21"),] %>% 
  select(GeoId,countriesAndTerritories, DateRep, cumsum_cases, rank) %>% 
  arrange(desc(cumsum_cases))

top10_df_cumsum[top10_df_cumsum$DateRep == 
                  as.Date("2020-03-27"),] %>% 
  select(GeoId,countriesAndTerritories, DateRep, cumsum_cases, rank) %>% 
  arrange(desc(cumsum_cases))

#* 1 ensures we have non-integer ranks while sliding
top10_df_cumsum<-top10_df_cumsum %>% 
  group_by(DateRep) %>% 
  mutate(rank = rank(-cumsum_cases, 
                     ties.method="random" ) * 1) %>%
  ungroup()

#Drop the values with Zero Cumulative cases.
top10_df_cumsum <-  top10_df_cumsum[top10_df_cumsum$cumsum_cases != 0, ]

#Generate static plot
static_plot<-ggplot(top10_df_cumsum,
                    aes(rank,group=countriesAndTerritories,
                        fill=as.factor(countriesAndTerritories),
                        color=as.factor(countriesAndTerritories))) +
  geom_tile(aes(y = cumsum_cases/2,
                height = cumsum_cases,
                width = 0.9), 
            alpha = 0.8, color = NA) +
  geom_text(aes(y = 0, label = paste(countriesAndTerritories, " ")), vjust = 0.2, hjust = 1) +
  geom_text(aes(y=cumsum_cases,label = paste(" ", cumsum_cases)), hjust=0)+
  coord_flip(clip = "off", expand = TRUE) +
  scale_y_continuous(labels = scales::comma) +
  scale_x_reverse() +
  guides(color = FALSE, fill = FALSE) +
  theme_minimal() +
  theme( plot.title = element_text(size = 30, hjust=0.5, face="bold", colour="grey", vjust= -1), 
         plot.subtitle = element_text(size = 20, hjust=0.5, face="italic", color="grey"), 
         plot.caption = element_text(size = 20, hjust = 0.5, face="italic", color="blue"), 
         axis.ticks.y = element_blank(), 
         axis.text.y = element_blank(),
         plot.margin = margin(1,1,1,4, "cm")
         )

#Creating the final animation
dynamic_plt<-static_plot + 
  transition_states(states = DateRep, transition_length = 4, state_length = 1) + 
  ease_aes('cubic-in-out') +
  labs(title = 'Cumulative Cases Till Date : {closest_state}', 
       subtitle = "Top 10 Countries",
       caption = "Data Source: European Centre for Disease Prevention and Control\n Author:Avisek Choudhury",
       x="Countries", y="Total Cases Per Country")

#Rendering the animation for gif
final_animation<-animate(dynamic_plt,100,
                         fps = 20, duration = 60, 
                         width = 1200, 
                         height = 800, 
                         renderer = gifski_renderer())

#Rendering the animation for mp4
animate(dynamic_plt, 100,
        fps = 20,duration = 60, 
        width = 1200, height = 800, 
        renderer = ffmpeg_renderer())

#Saving the animation
anim_save('CoronaDynamic_Avisek.gif',
          animation = final_animation, 
          path = 'C:/Corona/')
