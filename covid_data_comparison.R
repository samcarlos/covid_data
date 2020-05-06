library(dplyr)
library(ggplot2)


co_data = read.csv('/users/samweiss/src/covid_data/covid19_case_summary_2020-05-02.csv')
nyt_data = read.csv('/users/samweiss/src/covid_data/downloads/us-states.txt')

co_covid_deaths = subset(co_data, description == 'Number of Deaths From COVID-19 in Colorado by Date of Death - By Day'  ) %>% 
  select('attribute','value')
colnames(co_covid_deaths) = c('date', 'deaths')

nyt_covid_deaths = subset(nyt_data, state == 'Colorado') %>% select('date', 'deaths')
nyt_covid_deaths = nyt_covid_deaths[order(nyt_covid_deaths[,'date']), ]
nyt_covid_deaths_diff = data.frame(date = nyt_covid_deaths[-1,'date'], deaths = diff(nyt_covid_deaths[,'deaths']))

co_covid_deaths[,'source'] = 'Colorado_Website'
nyt_covid_deaths_diff[,'source'] = 'NYT_Webiste'

long_data = rbind(co_covid_deaths, nyt_covid_deaths_diff)
wide_data = merge(co_covid_deaths, nyt_covid_deaths_diff, by = 'date')[,c(1,2,4)]
colnames(wide_data) = c('date','deaths_co','deaths_nyt')

 
ggplot(long_data, aes(x = as.Date(date), y = deaths, colour = source, group = source))+geom_line(size = 1.5) +
  theme_minimal()+ xlab('Date')+ ylab('Number of Covid Deaths') + ggtitle('Colorado Covid Deaths By Time') +
  theme(text = element_text(size=20), legend.position="bottom")
  

ggplot(wide_data, aes(x = deaths_nyt, y = deaths_co))+geom_point()+
  theme_minimal()+ xlab('NY Times Reported Change in Cumulative Deaths')+ ylab('Colorado Reported Number of Covid Deaths') + 
  ggtitle('Colorado vs NY Times Covid Daily Deaths') +
  theme(text = element_text(size=20), legend.position="bottom")


sd(merged_data[,'deaths_co'])
sd(merged_data[,'deaths_co'] - merged_data[,'deaths_nyt'])
sd(merged_data[-47,'deaths_co'] - merged_data[-47,'deaths_nyt'])


reg_1 = lm(deaths_co ~ deaths_nyt, data = merged_data)
summary(reg_1)

reg_2 = lm(deaths_co ~ deaths_nyt, data = subset(merged_data, deaths_nyt<100))
summary(reg_2)
