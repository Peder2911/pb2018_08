source('dependencies.R')

expl <- read.csv('data/casualties.csv',stringsAsFactors = FALSE)
expl$date <- date(expl$date)

expl_farc <- expl[str_detect(expl$dyad,'FARC'),]
cf_data <- read.csv('data/cf_data.csv',stringsAsFactors = FALSE)
cf_data$start_date <- date(cf_data$start_date)
cf_data$end_date <- date(cf_data$end_date)
cf_farc <- cf_data[str_detect(cf_data$dyadName,'FARC'),]%>%
  mutate(end_date = if_else(is.na(end_date),
                            start_date + 10,
                            end_date))%>%
  .[complete.cases(.),]%>%
  unique()%>%
  arrange(start_date)

vizDat <- expl_farc%>%
  mutate(monthstart = paste(year,month,'01',sep = '-')%>%
           date()+31)%>%
  group_by(year,month,monthstart)%>%
  summarise(deaths = n())%>%
  filter(year > 2011)
#vizDat2 <- vizDat%>%
#  group_by(month(monthstart %% 2))

farcPPplot <- ggplot(vizDat)+
  
  geom_rect(data = cf_farc%>%
              filter(year(start_date) > 2011)%>%
              mutate(end_date = if_else(end_date > date('2016-12-01'),
                                        date('2016-12-01'),
                                        end_date)),
            aes(
              x = NULL,
              xmin = start_date,
              xmax = end_date+1,
              ymin = 0,
              ymax = 100,
              fill = part_of_process,
              color = NULL
            ))+
  
#  geom_segment(data = cf_farc%>%
#                 filter(start_date > date('2015-03-01'))%>%
#                 mutate(end_date = if_else(end_date > date('2016-12-01'),
#                                           date('2016-12-01'),
#                                           end_date)),
#               aes(
#                 x = start_date,
#                 xend = start_date,
#                 y = 0,
#                 yend = 100
#               ),color = PRIOgray)+
  
  geom_line(aes(monthstart,deaths),
            color = PRIOblue,
            size = 1.2)+
    
  scale_x_date(date_breaks = '6 months',
               date_minor_breaks = 'months',
               date_labels = '%b %Y',
               expand = c(0.01,0.01))+
  
  scale_y_continuous(expand = c(0,0),
                     name = 'Fatalities (monthly)')+
  
  scale_fill_manual(values = c(
    PRIOorange,
    PRIOlightblue
  ),name = 'Part of peace process')+
  
  theme_classic()+
  theme(text = element_text('GillSans'),
        axis.line.y = element_blank(),
        
        axis.title.x = element_blank(),
        axis.text.x = element_text(angle = 45,
                                   hjust = 1),
        axis.title.y = element_text(margin = margin(r = 0.2,unit = 'cm')),
        panel.grid.major.x = element_line(color = 'gray'),
        panel.grid.minor.x = element_line(color = 'light gray'),
        legend.position = 'bottom',
        legend.key.width = unit(2,units = 'cm'),
        legend.spacing.x = unit(0.5,units = 'cm'),
        plot.margin = margin(t = 0.3,l = 0.2,r = 0.2,unit = 'cm'))  

farcPPplot
ggsave('./plots/farcPP.png',farcPPplot,width = 13,height = 8.5,units = 'cm',dpi = 'print',device = 'png')
