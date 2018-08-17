source('dependencies.R')

# Event data for eventing ###########
events <- read.csv('data/events.csv',stringsAsFactors = FALSE)%>%
  mutate(description = row_number())
cf_data <- read.csv('data/cf_data.csv')
cf_data$start_date <- date(cf_data$start_date)
cf_data$end_date <- date(cf_data$end_date)

tlPlot <- events%>%
  mutate(id = row_number(),
         start_year = date(paste(start_year,'01','01',sep = '-')),
         end_year = date(paste(end_year,'12','31',sep = '-')))%>%
  ggplot()+
  
  geom_histogram(data = cf_data%>%
                   mutate(id = (row_number())/max(row_number())),
                 aes(x = year(start_date),stat(count),fill = dyadName),
                 binwidth = 1)+
  
  geom_rect(aes(xmin = year(start_year)-0.5,
                xmax = year(end_year)-0.5,
                ymin = 0,
                ymax = 5),
            alpha = 0,
            color = 'black',
            inherit.aes = FALSE)+  
  
  geom_text(aes(x = year(start_year + (end_year - start_year) / 2)-0.5,
                y = 5,
                label = description),
            angle = 0,
            size = 3,
            hjust = 0.5,
            vjust = 1.4,
            family = 'GillSans')+ 
  
  theme_classic()+
  theme(text = element_text('GillSans'),
        axis.line.y = element_blank(),
        axis.title.y = element_text(size = 13),
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        panel.grid.major.x = element_line(color = 'gray'),
        panel.grid.minor.x = element_line(color = 'light gray'),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.key.size = unit(0.5,units = 'cm'),
        legend.spacing.x = unit(0.1,units = 'cm'),
        legend.margin = margin(t = 0.1,r = 0.1,b = 0.1,l = 0.1, unit = 'cm'),
        
        plot.margin = margin(t = 0.3,l = 0.1,r = 0.3,unit = 'cm'))+
  guides(fill = guide_legend(ncol = 3))+
  
  labs(y = 'Ceasefires per year')+
  scale_x_continuous(breaks = seq(1987,2019,1),
                     limits = c(1988,2019))+
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = c(PRIOolive,
                               PRIOpeach,
                               PRIOgray,
                               PRIOblue,
                               PRIOlightblue),
                    breaks = unique(cf_data$dyadName),
                    name = 'Conflict dyad')

ggsave('./plots/timeline.png',tlPlot,height = 6,width = 20,units = 'cm',device = 'png',dpi = 'print')
