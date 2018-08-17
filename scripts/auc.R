source('dependencies.R')

# AUC plot ##########################

expl <- read.csv('data/casualties.csv',stringsAsFactors = FALSE)
expl$date <- date(expl$date)

expl_auc <- expl[str_detect(expl$dyad,'AUC'),]

auc_events <- tibble(
  start_date = c(date('1998-01-01'),date('2002-01-01')),
  end_date = c(date('2002-01-01'),date('2008-01-01')),
  description = c('','Disarmament')
)%>%
  mutate(id = factor(row_number()))

aucPlot <- expl_auc%>%
  ggplot()+
  geom_rect(data = auc_events,
            aes(
              xmin = start_date,
              xmax = end_date,
              ymin = 0,
              ymax = 450,
              fill = id
            ),alpha = 0.4,
            show.legend = FALSE)+
  geom_freqpoly(aes(date,color = dyad),
                binwidth = 182.5,
                size = 1.2)+
  geom_text(data = auc_events,
            aes(
              x = start_date + (end_date - start_date)/2,
              y = 450,
              label = description
            ),
#            size = 8,
            vjust = 1.4,
            family = 'GillSans')+
  scale_x_date(date_breaks = 'years',
               limits = c(date('1997-09-15'),date('2008-01-01')),
               expand = c(0,0),
               date_labels = '%Y')+
  scale_y_continuous(expand = c(0,0),
                     name = 'Fatalities (half year)')+
  scale_fill_manual(values = c('white',PRIOgray))+
  scale_color_manual(values = c(PRIOpeach,PRIOgray,PRIOblue),
                     name = 'Conflict dyad')+
  theme_classic()+
  theme(text = element_text('GillSans'),
        axis.line.y = element_blank(),
        
        axis.title.x = element_blank(),
        axis.title.y = element_text(margin = margin(r = 0.5,unit = 'cm')),
        panel.grid.major.x = element_line(color = 'gray'),
        panel.grid.minor.x = element_line(color = 'light gray'),
        legend.position = 'bottom',
        legend.title = element_blank(),
        legend.key.width = unit(1,units = 'cm'),
#        legend.spacing.x = unit(0.5,units = 'cm'),
        plot.margin = margin(l = 0.7,r = 1.5,t = 0.3,unit = 'cm'))

ggsave('./plots/aucPlot.png',aucPlot,height = 8.5,width = 13,units = 'cm',device = 'png',dpi = 'print')
