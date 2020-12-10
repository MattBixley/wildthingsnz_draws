#setup
library(tidyverse)

# read data
data <- read.csv("data/test_data.csv") %>% 
  janitor::clean_names()

# tidy data
data <- data %>%
  # make names have upper case 1st letter
  mutate_if(is.character, str_to_title) %>% 
  
  # strip white space
  mutate(surname =  str_replace_all(surname," ", "")) %>% 
  mutate(first_name =  str_replace_all(first_name," ", "")) %>% 
  
  # 1name
  mutate(name = paste0(first_name,"_",surname)) %>% 
  
  # round number
  mutate(points = floor(points))
  
# Make tickets
# take names and multiple by the number of points, score, kms

tickets <- data %>% 
  select(name, points) %>%
  map_df(., rep, .$points) %>% 
  select(name)

# draw
draws <- 96
for(i in 1:draws){
  winner <- sample(tickets$name, 1)
  print(paste0("Draw ",i," Winner: ",winner))
  tickets <- subset(tickets, name != winner)
}
  

# simulate 1000 draws
iter <- 6905764

winners <- character()

for(i in 1:iter){
  winners[i] <- sample(tickets$name, 1)
}

table(winners) %>% 
  sort(decreasing = T) %>% 
  head()
