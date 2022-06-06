#Scrape data on house price and availability from Persimmon, then make summary statistics on it and produce graphs 

#load libraries
library(rvest)
library(tidyverse)
library(jsonlite)
library(httr)
library(sf)
library(rgdal)
library(rnaturalearthdata)
library(rnaturalearth)
library(viridis)


# goes to the webstie and gets a list of how many named regions they have, this is = a
URL1 <- "https://www.persimmonhomes.com/find-your-new-home"
URL2 <- "https://www.persimmonhomes.com"
page1 = read_html(URL1)
name_of_regions = page1 %>% html_nodes("#developmentsAccordion .futura-b") %>% html_text() 
name_of_regionsdf <- data.frame(name_of_regions)
a = count(name_of_regionsdf)
vec2 <- name_of_regionsdf$name_of_regions


y <- 0
long_list_of_urls = data.frame()
datalist1 <- list()

for (item in vec2) {i
  
 
  Z = paste0("#collapse-",y," a")
  page1 = read_html(URL1) 
  scraped_urls = page1 %>% html_nodes(Z) %>% html_attr("href") %>% paste("https://www.persimmonhomes.com", ., sep ="")
  y <- y+1
  short_list_of_urls <- data.frame(scraped_urls, stringsAsFactors = FALSE)
  datalist1[[item]] <- short_list_of_urls
}
long_list_of_urls = do.call(rbind, datalist1)

vec1 <- long_list_of_urls$scraped_urls

# this does the webscraping, there is an issue with reading the names of houses
# The Rufford appears twice though it is not displayed, it might be used for testing or be a previous development

daily_data <- data.frame()
datalist = list()

for(i in vec1){
  
  
    page = read_html(i) 
    price_count <- data.frame()
    name_intermediate_count <- data.frame()
       
      # it looks like "The Rufford" really is there twice at the start of each instance, drop the first two values and not use its name
       
    name_old = page %>% html_nodes(".mb-0.t-g") %>% html_text() 
    name_intermediate <- tail(name_old, -2)
    name_intermediate_count = data.frame(name_intermediate)
   
   
    
    
    
       
       # There is an issue with price where early bird offers have a bird icon instead of a price, however, these are always first displayed so the difference between the role lengths can be calculated
       # then make a new variable with where you drop that number of rows from that start of price
    price = page %>% html_nodes(".t24.fw600.t-g") %>% html_text()
    price_count = data.frame(price)
   
    #This skips pages with no house prices listed
    if(count(price_count)==0) next 
       
    x <- count(name_intermediate_count) - count(price_count)  
       
    if (x == 0) 
      {name_new <- name_intermediate} 
    else 
      {name_new <- tail(name_intermediate, - x)}
       
    availability_and_house_type = page %>% html_nodes(".available-house .fw600.t16") %>% html_text()
       
    if (x == 0) 
      {availability_and_house_type_new <- availability_and_house_type} 
    else 
    {availability_and_house_type_new <- tail(availability_and_house_type, -x)}
    
    #gets the address and postcode
    address_and_postcode = page %>% html_nodes(".t-g-grey") %>% html_text()
    postcode <- sub('.*\\,', '', address_and_postcode)
    postcode <- sub(" ", "", postcode)  
    postcode <- sub(" ", "", postcode)  
    
    #old code
    #postcode <- substr(address_and_postcode, count_of_char-7, count_of_char)
   
    #'duplicates code to number of rows required  
    
   
    ### this is an api for looking up uk postcodes, it's free
    # ping the website, returns long and lat data 
    #if there is an error it stores it as not_avaliable 
    #https://api.postcodes.io
  
    base_api_url <- "https://api.postcodes.io/postcodes/"
    
    full_url <- base::paste0(base_api_url,postcode)
    
    api_call <- httr::GET(full_url)
    
    if (api_call$status_code == 404)
    {longitude <- "not_available" 
    latitude <- "not_available"
    rep(longitude,count(price_count))
    rep(latitude,count(price_count))
    }
    else {
      api_char <- base::rawToChar(api_call$content)
      
      api_json <- jsonlite::fromJSON(api_char, flatten = TRUE)
      
      longitude <- api_json$result$longitude
      latitude  <- api_json$result$latitude
      
      
      rep(longitude,count(price_count))
      rep(latitude,count(price_count))
      
    }
 
    rep(postcode,count(price_count))
    
    ### this is an api for looking up uk postcodes, it's free so not the best but alrighti
    #https://api.postcodes.io
    
    
  #get the geodata
  ## #mapDiv > div > div > div:nth-child(5) > div > div > div > div > div.place-desc-large > div.place-name
  #<div jstcache="24" class="place-name" jsan="7.place-name">52Â°35'15.0"N 1Â°08'18.7"E</div>
  #<div class="place-name">Test</div>
    
  #somehwo pass 7 place name
    
  ## = page %>% html_nodes("[class='place-name']") %>% html_text()
  
       
  dat <- data.frame(name_new, price, availability_and_house_type_new, address_and_postcode, postcode, longitude, latitude, stringsAsFactors = FALSE)
  dat$url <- i
  datalist[[i]] <- dat
       
 
}

daily_data = do.call(rbind, datalist) 


#some data cleaning
daily_data$price <- gsub(",", "", daily_data$price)
daily_data$price <- gsub("£", "", daily_data$price)
daily_data$price <- as.numeric(daily_data$price)

region <- "UK"
daily_data$region <- rep(region,nrow(daily_data))

#getting avalability as a numberic
daily_data <- daily_data %>% separate(availability_and_house_type_new, into = c("availability", "house_type"), sep = "\\|" )
#this is a troubled line below
daily_data$availability <- gsub("[a-zA-Z]", "", daily_data$availability)
daily_data$availability <- str_trim(daily_data$availability, "right")

daily_data$availability  <- as.numeric(daily_data$availability)

#filtering out where there is no long and lat 
daily_data_geolocation_available = filter(daily_data, latitude!="not_available")

#adding new coloum and grouping
daily_data_geolocation_available$price_times_availability <- daily_data_geolocation_available$price * daily_data_geolocation_available$availability

daily_data_geolocation_available$longitude <- as.numeric(daily_data_geolocation_available$longitude)
daily_data_geolocation_available$latitude <- as.numeric(daily_data_geolocation_available$latitude)





# creates the data to be mapped
df1 <- daily_data_geolocation_available %>% group_by(postcode) %>% dplyr::summarize(average_price = sum(price_times_availability)/sum(availability),
                                sum_of_availability = sum(availability),
                                long =   first(longitude),
                                lat = first(latitude),
                                region = first(region))




library(maps)
UK <- map_data("world") %>% filter(region=="UK")
 
final_map <- ggplot() +
  geom_polygon(data = UK, aes(x=long, y = lat, group = group), fill="#A6A6A6", alpha=.7) +
  scale_color_viridis() +
  scale_size_continuous(range=c(1,6)) +
  ##scale_color_manual(breaks = c("100", "200", "300"), values=c("red", "blue", "green")) +
  geom_point(data=df1, aes(x=long, y=lat, size=sum_of_availability, color = average_price/1000, alpha=.4)) +
  theme_light() + 
  ylim(50,58.5) +
  coord_map() +
  guides(color=guide_legend("Average House Price (£000)"),size=guide_legend("Availability"), alpha = FALSE) +
  labs(title ="Persimmon: Homes for Sale", 
       subtitle = "Average Price and Number Available per Development", 
       caption = "Source: Gathered from www.persimmonhomes.com on 06/06/2022", 
       x = "Longitude", 
       y = "Latitude") 
  
final_map



