
#Project1: Use of Tidyverse for data analysis
#Q1

#Read in the gapminder_clean.csv data as a tibble using read_csv
#Filter the data to include only rows where Year is 1962 and then make a scatter plot comparing 'CO2 emissions (metric tons per capita)' and gdpPercap for the filtered data.

library(tidyverse)
data <- read_csv("gapminder_clean.csv")
data_1 <- data %>% 
  filter(Year == 1962) %>% 
  ggplot(aes(x =`CO2 emissions (metric tons per capita)`, y = gdpPercap)) +
  geom_point(color= "orange" ) +
  coord_cartesian(xlim = c(0, 20), ylim = c(100, 25000)) +
  labs(x = "CO2 Emissions (metric tons per capita)", y = "GDP per Capita") +
  ggtitle("Scatter Plot of CO2 Emissions vs. GDP per Capita (Year 1962)")+
  theme_classic()
data_1


#Q2.

#On the filtered data, calculate the correlation of 'CO2 emissions (metric tons per capita)' and gdpPercap. What is the correlation and associated p value?
data_2<-data %>% 
  filter(Year==1962) %>%
  summarise(correlation=cor(`CO2 emissions (metric tons per capita)`,gdpPercap, use= "complete.obs")) %>% 
  mutate(p_value = cor.test(data$`CO2 emissions (metric tons per capita)`,data$gdpPercap)$p.value)

cat("Correlation:", data_2$correlation, "\n")
cat("p-value:", data_2$p_value, "\n")

#Q3.
names(data)
#On the unfiltered data, answer "In what year is the correlation between 'CO2 emissions (metric tons per capita)' and gdpPercap the strongest?" Filter the dataset to that year for the next step..
  data_3 <- data %>%
  group_by(Year) %>%
  mutate(correlation = cor(`CO2 emissions (metric tons per capita)`, gdpPercap, use = "complete.obs")) %>% 
  summarise(value = max(correlation)) %>% 
  filter(value == max(value))
cat( data_3$Year, " was the year when correlation was the highest")

#Q4.Using plotly, create an interactive scatter plot comparing 'CO2 emissions (metric tons per capita)' and gdpPercap, where the point size is determined by pop (population) and the color is determined by the continent. You can easily convert any ggplot plot to a plotly plot using the ggplotly() command

install.packages("plotly")
library(plotly)
 
plot <- data %>%
  ggplot(aes(x = gdpPercap, y = `CO2 emissions (metric tons per capita)`, 
             size = pop, color = continent)) +
  geom_point(alpha = 0.7) +
  labs(
    title = "CO2 Emissions vs. GDP per Capita",
    x = "GDP per Capita",
    y = "CO2 Emissions (metric tons per capita)") +
  theme_minimal()

 ggplotly(plot)


  