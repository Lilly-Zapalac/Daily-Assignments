# Daily-Assignments

#Lilly Zapalac, 02/17/25, #learning to code tables
library(dplyr)
library(ggplot2)
url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid = read.csv(url)
head(covid, 6)



library(gapminder) 
library(ggplot2)
library(dplyr)

covid %>% 
  group_by(state) %>% 
  summarize(total_cases = sum(cases, na.rm = TRUE)) %>%
  slice_max(order_by = total_cases, n= 6) %>%
  pull(state)
 
top_6_states <- slice_max(covid, date) %>% 
  group_by(state) %>% 
  summarise(totCases = sum(cases)) %>% 
  slice_max(totCases, n = 6) %>% 
  pull(state)

  filter_covid <- covid %>%
  
  filter(state %in% top_6_states) %>%
  group_by(state, date) %>%
  summarize(total_cases = sum(cases, na.rm = TRUE), .groups = "drop") %>%
  arrange(state, date)


  ggplot(filter_covid, aes(x = date, y = total_cases) -> Maps) +
  labs(title = "Cumulative Case Counts COVID-19 Pandemic",
       x = "Date",
       y = "Number of cases per state",
       color = "State",
       group = "State") +
    geom_smooth(aes(linewidth = cases)) +
    facet_wrap(~ state, scales = "free_y") +
    scale_y_continuous() +
    theme_minimal() +
    
  
  print(Maps)
  ggsave("covid_cases_top_states.png")
  
  library(dplyr)
  library(ggplot2)
  library(readr)
  
  url = 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties-recent.csv'
   covid_data = read_csv(url)
  
   daily_cases <- covid %>%
     group_by(state) %>%
     summarize(total_cases = sum(cases, na.rm = TRUE))
  
   
daily_cases <- covid_data %>%
  group_by(date) %>%
  summarize(total_cases = sum(cases, na.rm = TRUE), .groups = "drop")
  
  
plot <- ggplot(data = covid,
    aes(x = date, y = cases)) +
  geom_col(aes(linewidth = cases)) +
  labs(title = "National Cumulative Case Counts: COVID-19 Pandemic",
       x = "Date",
       y = "Cases") +
    geom_hline(yintercept = mean(covid_data$cases), colors = "grey")
    geom_vline(xintercept = mean(covid_data$date), colors = "grey")
    
  
print(plot)
ggsave("daily_covid_US.png")
