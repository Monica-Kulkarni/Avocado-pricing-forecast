setwd("C:/Users/Monica Kulkarni/Downloads/avocado.csv")
df <- read.csv("avocado.csv")
original_df <- df
levels(df$type)


suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(skimr))
suppressPackageStartupMessages(library(GGally))
suppressPackageStartupMessages(library(viridis))
suppressPackageStartupMessages(library(caret))
suppressPackageStartupMessages(library(e1071))
suppressPackageStartupMessages(library(rpart))
suppressPackageStartupMessages(library(xgboost))
suppressPackageStartupMessages(library(corrplot))
suppressPackageStartupMessages(library(corrgram))
suppressPackageStartupMessages(library(ggplot2))
suppressPackageStartupMessages(library(ggthemes))
suppressPackageStartupMessages(library(psych))
suppressPackageStartupMessages(library(scales))
suppressPackageStartupMessages(library(treemap))
suppressPackageStartupMessages(library(repr))
suppressPackageStartupMessages(library(cowplot))
suppressPackageStartupMessages(library(magrittr))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(RColorBrewer))
suppressPackageStartupMessages(library(plotrix))
suppressPackageStartupMessages(library(ggrepel))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(lubridate))
suppressPackageStartupMessages(library(tibbletime))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(tidyr))
suppressPackageStartupMessages(library(ggpubr))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(smooth))
suppressPackageStartupMessages(library(forecast))
suppressPackageStartupMessages(library(fpp2))

options(repr.plot.width=8, repr.plot.height=4)
ggplot(df, aes(x=AveragePrice, fill=type)) + geom_density() + facet_wrap(~type) + theme_gray() + 
theme(plot.title=element_text(hjust=0.5), legend.position="bottom") + labs(title="Avocado Price by Type") + scale_fill_brewer(palette="Dark2")

vol_type <- df %>% group_by(type) %>% summarise(avg.vol=mean(Total.Volume))  %>% mutate(pct=prop.table(avg.vol) * 100) 
vol_type

# Change the date column from factor to date
df$Date <- as.Date(df$Date, "%Y-%m-%d")
df[ order(df$Date , decreasing =  ),]
class(df$Date)

seasonal_df <- original_df

seasonal_df$month_year <- format(as.Date(original_df$Date), "%Y-%m")
seasonal_df$month <- format(as.Date(original_df$Date), "%m")
seasonal_df$year <- format(as.Date(original_df$Date), "%Y")


seasonal_df$monthabb <- sapply(seasonal_df$month, function(x) month.abb[as.numeric(x)])
seasonal_df$monthabb = factor(seasonal_df$monthabb, levels = month.abb)


# # Let's see if there are seasonal patterns with conventional avocadoes
ggplot(seasonal_df, aes(x = AveragePrice, fill = as.factor(year))) + 
  geom_density(alpha = .5) + 
  theme_economist() +
  facet_wrap(~ year) + theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F9E79F")) + 
  guides(fill = FALSE) + labs(title="Distribution of Prices by year", x = 'Average Price', y = 'Density') + 
  scale_fill_manual(values=c("#2E64FE", "#40FF00", "#FE642E", "#FE2E2E"))
# Detecting seasonality patterns
conv_patterns <- seasonal_df %>% select(monthabb, AveragePrice, type) %>% filter(type == "conventional") %>%
  group_by(monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) + geom_point(color="#F35D5D", aes(size=avg)) + geom_line(group=2, color="#000000") + 
  theme_economist() + theme(legend.position="none", plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#9aeafe")) + 
  labs(title="Conventional Avocados", x="Month", y="Average Price")


org_patterns <- seasonal_df %>% select(monthabb, AveragePrice, type) %>% filter(type == "organic") %>%
  group_by(monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) + geom_point(color="#F35D5D", aes(size=avg)) + geom_line(group=2, color="#000000") + 
  theme_economist() + theme(legend.position="none", plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#9aeafe")) + 
  labs(title="Organic Avocados", x="Month", y="Average Price")

plot_grid(conv_patterns, org_patterns, nrow=2)

# Hmm let's see if the Seasonality pattern is maintained each year.
options(repr.plot.width=8, repr.plot.height=6) 
conv_pat_yearly <- seasonal_df %>% select(year, monthabb, AveragePrice, type) %>% filter(type == "conventional", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) + geom_point(color="#5D6D7E") + geom_line(group=1, color="#1b1efe") + facet_wrap(~as.factor(year)) + 
  theme_cowplot() + theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#fed19a"), axis.text.x = element_text(angle = 90)) + 
  labs(title="Seasonal Fluctuations \n Convenctional Avocados", x="Month", y="Average Price")

org_pat_yearly <- seasonal_df %>% select(year, monthabb, AveragePrice, type) %>% filter(type == "organic", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% summarize(avg=mean(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=avg)) + geom_point(color="#5D6D7E") + geom_line(group=1, color="#E74C3C") + facet_wrap(~as.factor(year)) + 
  theme_cowplot() + theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#fed19a"), axis.text.x = element_text(angle = 90)) + 
  labs(title="Seasonal Fluctuations \n Organic Avocados", x="Month", y="Average Price")

plot_grid(conv_pat_yearly, org_pat_yearly, nrow=2)






# Measuring standard deviation per month through each year by type of avocado.
std_conv <- seasonal_df %>% select(year, monthabb, AveragePrice, type) %>% filter(type == "conventional", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% summarize(std=sd(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=std)) + 
  geom_point(aes(size=std), col="#5A96C6") +   
  geom_segment(aes(x=monthabb, 
                   xend=monthabb, 
                   y=min(std), 
                   yend=max(std)), 
               linetype="dashed", 
               size=0.1) + 
  coord_flip() + 
  facet_wrap(~year) + 
  theme_tufte() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#ffd6f1"), legend.position="none") + 
  labs(title="Conventional Avocados \n Price Volatility",x="Months", y="Standard Deviation")


std_org <- seasonal_df %>% select(year, monthabb, AveragePrice, type) %>% filter(type == "organic", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% summarize(std=sd(AveragePrice)) %>%
  ggplot(aes(x=monthabb, y=std)) + 
  geom_point(aes(size=std), col="#5AC67C") +   
  geom_segment(aes(x=monthabb, 
                   xend=monthabb, 
                   y=min(std), 
                   yend=max(std)), 
               linetype="dashed", 
               size=0.1) + 
  coord_flip() + 
  facet_wrap(~year) + 
  theme_tufte() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#ffd6f1"), legend.position="none") + 
  labs(title="Organic Avocados \n Price Volatility",x="Months", y="Standard Deviation")

plot_grid(std_conv, std_org, nrow=2)





# Let's have a closer look how the price changes per month.
# filter by type and year

options(repr.plot.width=10, repr.plot.height=8) 

se <- function(x) sqrt(var(x)/length(x)) 

conv <- seasonal_df %>% select(year, monthabb, AveragePrice, type) %>% filter(type == "conventional", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% 
  ggplot(aes(x=monthabb, y=AveragePrice, fill=monthabb), color="white") + geom_bar(width=1, stat='identity') + 
  geom_errorbar(aes(ymin = AveragePrice - se(AveragePrice), 
                    ymax = AveragePrice + se(AveragePrice), 
                    color = monthabb), 
                width = .2) + scale_y_continuous(breaks = 0:nlevels(seasonal_df$monthabb)) +
  facet_wrap(~year) + theme_minimal() + 
  theme(axis.ticks = element_blank(),
        axis.text.y=element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(), 
        plot.background=element_rect(fill="#FFF1E0"),
        legend.position="none", plot.title=element_text(hjust=0.5)) + 
  coord_polar() + labs(title="Seasonal cycle \n Conventional Avocados") + 
  scale_fill_manual(values=c('#57FCE0', '#57A6FC', '#3C546E', '#4AFA76', '#95CFA4', '#C0E436', '#F2A42D', '#F25F2D', '#F2442D',
                             '#AB4949', '#4950AB', '#4974AB'))


org <- seasonal_df %>% select(year, monthabb, AveragePrice, type) %>% filter(type == "organic", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>% 
  ggplot(aes(x=monthabb, y=AveragePrice, fill=monthabb), color="white") + geom_bar(width=1, stat='identity') + 
  geom_errorbar(aes(ymin = AveragePrice - se(AveragePrice), 
                    ymax = AveragePrice + se(AveragePrice), 
                    color = monthabb), 
                width = .2) + scale_y_continuous(breaks = 0:nlevels(seasonal_df$monthabb)) +
  facet_wrap(~year) + theme_minimal() + 
  theme(axis.ticks = element_blank(),
        axis.text.y=element_blank(),
        axis.title = element_blank(),
        axis.line = element_blank(), 
        plot.background=element_rect(fill="#FFF1E0"),
        legend.position="none", plot.title=element_text(hjust=0.5)) + 
  coord_polar() + labs(title="Seasonal cycle \n Conventional Avocados") + 
  scale_fill_manual(values=c('#57FCE0', '#57A6FC', '#3C546E', '#4AFA76', '#95CFA4', '#C0E436', '#F2A42D', '#F25F2D', '#F2442D',
                             '#AB4949', '#4950AB', '#4974AB'))


grid.arrange(conv, org, nrow = 2)


########################################################################################


options(repr.plot.width=10, repr.plot.height=7) 
r_avg <- seasonal_df %>% group_by(year, monthabb) %>%  select(type, year, monthabb, AveragePrice) %>% 
  filter(type == "conventional", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>%
  summarize(avg=mean(AveragePrice))



structured_data <- spread_(r_avg, key="year", value="avg")


colnames(structured_data) <- c("Months", "First_year", "Second_year", "Third_year")


structured_data$first_pct <- NA
structured_data$second_pct <- NA

structured_data$first_pct <- (structured_data$Second_year - structured_data$First_year)/structured_data$First_year
structured_data$second_pct <- (structured_data$Third_year - structured_data$Second_year)/structured_data$Second_year


structured_data<- structured_data %>% 
  mutate(first_cond=ifelse(first_pct > 0, "Positive", "Negative"),
         second_cond=ifelse(second_pct > 0, "Positive", "Negative"))


firstp_change <- ggplot(structured_data) +
  geom_segment( aes(x=Months, xend=Months, y=First_year, yend=Second_year), color="#6E6A6A") +
  geom_point( aes(x=Months, y=First_year), color="#F74B4B", size=3 ) +
  geom_point( aes(x=Months, y=Second_year),color="#36ACD7", size=3 ) +
  coord_flip()+ 
  theme_economist() +
  theme(
    legend.position = "top",
    plot.title=element_text(hjust=0.5),
    plot.background=element_rect(fill="#F4F6F7")
  ) +
  labs(title="Conventional Avocado Price changes \n (2015 - 2016)", x="Months", y="Price",
       caption="Red: Year of 2015, Blue: Year of 2016")


secondp_change <- ggplot(structured_data) +
  geom_segment( aes(x=Months, xend=Months, y=Second_year, yend=Third_year), color="#6E6A6A") +
  geom_point( aes(x=Months, y=Second_year), color="#36ACD7", size=3 ) +
  geom_point( aes(x=Months, y=Third_year), color="#58FA58", size=3 ) +
  coord_flip()+ 
  theme_economist() +
  theme(
    legend.position = "top",
    plot.title=element_text(hjust=0.5),
    plot.background=element_rect(fill="#F4F6F7")
  ) +
  labs(title="Conventional Avocado Price changes \n (2016 - 2017)", x="Months", y="Price",
       caption="Blue: Year of 2016, Green: Year of 2017" )

# plot_grid(firstp_change, secondp_change, ncol=2)

first_pct_dif <- structured_data %>% select(Months, first_pct, first_cond) %>%
  ggplot(aes(fill=first_cond)) + geom_bar(stat='identity', aes(x=Months, y=round(first_pct,2) * 100), color="black") + 
  theme_economist() + theme(axis.text.x=element_text(angle=90), plot.background=element_rect(fill="#F4F6F7"), legend.position="bottom") + 
  labs(x="Month", y="% Difference") + 
  guides(fill=guide_legend(title="Diff Status")) + scale_fill_manual(values=c("#FB4D42", "#ADE175"))

second_pct_dif <- structured_data %>% select(Months, second_pct, second_cond) %>%
  ggplot(aes(fill=second_cond)) + geom_bar(stat='identity', aes(x=Months, y=round(second_pct,2) * 100), color="black") + 
  theme_economist() + 
  theme(axis.text.x=element_text(angle=90), plot.background=element_rect(fill="#F4F6F7"), legend.position="bottom") + labs(x="Month", y="% Difference") + 
  guides(fill=guide_legend(title="Diff Status")) + scale_fill_manual(values=c("#FB4D42", "#ADE175"))



plot_grid(firstp_change, secondp_change, first_pct_dif, second_pct_dif,  nrow=2, ncol=2)


##########################################################################################


# Organic avvocados

r_avg_org <- seasonal_df %>% group_by(year, monthabb) %>%  select(type, year, monthabb, AveragePrice) %>% 
  filter(type == "organic", year == c("2015", "2016", "2017")) %>%
  group_by(year, monthabb) %>%
  summarize(avg=mean(AveragePrice))



structured_data_org <- spread_(r_avg_org, key="year", value="avg")


colnames(structured_data_org) <- c("Months", "First_year", "Second_year", "Third_year")




structured_data_org$first_pct <- NA
structured_data_org$second_pct <- NA

structured_data_org$first_pct <- (structured_data_org$Second_year - structured_data_org$First_year)/structured_data$First_year
structured_data_org$second_pct <- (structured_data_org$Third_year - structured_data_org$Second_year)/structured_data$Second_year


structured_data_org<- structured_data_org %>% 
  mutate(first_cond=ifelse(first_pct > 0, "Positive", "Negative"),
         second_cond=ifelse(second_pct > 0, "Positive", "Negative"))


firstp_change_org <- ggplot(structured_data_org) +
  geom_segment( aes(x=Months, xend=Months, y=First_year, yend=Second_year), color="#6E6A6A") +
  geom_point( aes(x=Months, y=First_year), color="#F74B4B", size=3 ) +
  geom_point( aes(x=Months, y=Second_year),color="#36ACD7", size=3 ) +
  coord_flip()+ 
  theme_economist() +
  theme(
    legend.position = "top",
    plot.title=element_text(hjust=0.5),
    plot.background=element_rect(fill="#e5d6ff")
  ) +
  labs(title="Organic Avocado Price changes \n (2015 - 2016)", x="Months", y="Price",
       caption="Red: Year of 2015, Blue: Year of 2016")


secondp_change_org <- ggplot(structured_data_org) +
  geom_segment( aes(x=Months, xend=Months, y=Second_year, yend=Third_year), color="#6E6A6A") +
  geom_point( aes(x=Months, y=Second_year), color="#36ACD7", size=3 ) +
  geom_point( aes(x=Months, y=Third_year), color="#58FA58", size=3 ) +
  coord_flip()+ 
  theme_economist() +
  theme(
    legend.position = "top",
    plot.title=element_text(hjust=0.5),
    plot.background=element_rect(fill="#e5d6ff")
  ) +
  labs(title="Organic Avocado Price changes \n (2016 - 2017)", x="Months", y="Price",
       caption="Blue: Year of 2016, Green: Year of 2017" )

# plot_grid(firstp_change, secondp_change, ncol=2)

first_pct_dif_org <- structured_data_org %>% select(Months, first_pct, first_cond) %>%
  ggplot(aes(fill=first_cond)) + geom_bar(stat='identity', aes(x=Months, y=round(first_pct,2) * 100), color="black") + 
  theme_economist() + theme(axis.text.x=element_text(angle=90), plot.background=element_rect(fill="#e5d6ff"), legend.position="bottom") + 
  labs(x="Month", y="% Difference") + 
  guides(fill=guide_legend(title="Diff Status")) + scale_fill_manual(values=c("#FB4D42", "#ADE175"))

second_pct_dif_org <- structured_data_org %>% select(Months, second_pct, second_cond) %>%
  ggplot(aes(fill=second_cond)) + geom_bar(stat='identity', aes(x=Months, y=round(second_pct,2) * 100), color="black") + 
  theme_economist() + 
  theme(axis.text.x=element_text(angle=90), plot.background=element_rect(fill="#e5d6ff"), legend.position="bottom") + labs(x="Month", y="% Difference") + 
  guides(fill=guide_legend(title="Diff Status")) + scale_fill_manual(values=c("#FB4D42", "#ADE175"))



plot_grid(firstp_change_org, secondp_change_org, first_pct_dif_org, second_pct_dif_org,  nrow=2, ncol=2)


########################################################################################

options(repr.plot.width=8, repr.plot.height=6) 

# Let's create a seasonal column and plot a point line chart by each year.
seasonal_df$season <- ifelse(seasonal_df$month %in% c("03", "04","05"), "Spring",
                             ifelse(seasonal_df$month %in% c("06","07" ,"08"), "Summer",
                                    ifelse(seasonal_df$month %in% c("09","10","11"), "Fall", "Winter")))


seasonality.plot.conventional <- seasonal_df %>% select(season, year, AveragePrice, type) %>% 
  filter(type == "conventional", year == c("2015", "2016", "2017")) %>%
  group_by(season, year) %>%
  summarize(avg=mean(AveragePrice)) %>% ggplot(aes(x=season, y=avg, color=season)) + geom_point(size=3) + 
  geom_segment(aes(x=season, 
                   xend=season, 
                   y=0, 
                   yend=avg)) + 
  coord_flip() + facet_wrap(~as.factor(year)) + theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7")) + 
  scale_color_manual(values=c("#a06a31", "#9bd16b", "#d1706b", "#3bbf9e")) + 
  labs(title="Conventional Avocados by Season", x="Season", y="Average Price") + 
  geom_text(aes(x=season, y=0.01, label= paste0("$ ", round(avg,2))),
            hjust=-0.5, vjust=-0.5, size=4, 
            colour="black", fontface="italic",
            angle=360)

seasonality.plot.organic <- seasonal_df %>% select(season, year, AveragePrice, type) %>% 
  filter(type == "organic", year == c("2015", "2016", "2017")) %>%
  group_by(season, year) %>%
  summarize(avg=mean(AveragePrice)) %>% ggplot(aes(x=season, y=avg, color=season)) + geom_point(size=3) + 
  geom_segment(aes(x=season, 
                   xend=season, 
                   y=0, 
                   yend=avg)) + 
  coord_flip() + facet_wrap(~as.factor(year)) + theme_minimal() + 
  theme(plot.title=element_text(hjust=0.5), plot.background=element_rect(fill="#F4F6F7")) + 
  scale_color_manual(values=c("#a06a31", "#9bd16b", "#d1706b", "#3bbf9e")) + 
  labs(title="Organic Avocados by Season", x="Season", y="Average Price") + 
  geom_text(aes(x=season, y=0.01, label= paste0("$ ", round(avg,2))),
            hjust=-0.5, vjust=-0.5, size=4, 
            colour="black", fontface="italic",
            angle=360)

plot_grid(seasonality.plot.conventional, seasonality.plot.organic, nrow=2)

##########################################################################################
