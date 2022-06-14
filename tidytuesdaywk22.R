## Librarys ####

library(tidyverse)
library(ggbump)

## read data ##
poll <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/poll.csv')
reputation <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-05-31/reputation.csv')


### Transform data -- filter only automotive industry###

automotive <- 
  poll %>%
  mutate(year = 2022) %>%
  distinct( company, industry, year, rank = `2022_rank`, rq = `2022_rq`) %>%
  bind_rows( select( poll, company, industry, year, rank, rq)) %>%
  arrange( company, year)  %>%
  filter(industry == "Tech") %>%
  mutate(n = is.na(rank)) %>%
  group_by( company ) %>%
  mutate(n = sum(n)) %>%
  ungroup() %>%
  filter( n == 0)

### Plot ###
plot <- ggplot( automotive, aes( x = year, y = rank, group = company, color = company ))+
  geom_bump(size = 1.4)+
  geom_point(size = 3)+
  theme_void()+
  geom_text( data = automotive %>% filter(year == min(year)),
             aes( x = year -.7, label = paste0(company, " rank: ", rank )), size = 4, hjust = .1) +
  geom_text( data = automotive %>% filter(year == max(year)),
             aes( x = year+.1, label = paste0(company, " rank: ", rank )), size = 4, hjust = 0)+
  scale_y_reverse()+
  theme(legend.position = "none",
        axis.text.x = element_text(color = "gray"),
        text = element_text(family = "Times-Italic"),
        plot.title = element_text( color = "#546E7A", size = rel(2)),
        plot.subtitle = element_text( color = "#546E7A", size = rel(1.3)),
        plot.caption =  element_text( color = "#546E7A", size = rel(1)))+
  scale_color_grey()+
  scale_x_continuous(limits = c(2016,2023), n.breaks = 7)+
  labs(title = "2022 Axios-Harris Poll",
       subtitle = "Change in ranking from 2017 to 2022 Tech Industry",
       caption = "#Tidytuesday wk22 | source: Axios-Harris poll | viz: Reyes Esparza; @ReyesEsparza10")
  
  


