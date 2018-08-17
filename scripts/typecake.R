source('dependencies.R')

cf_types <- read.csv('data/types.csv',stringsAsFactors = FALSE)
cf_types$type_name <- ordered(cf_types$type_name)
cf_types$type_name <- reorder(cf_types$type_name,cf_types$type_name,FUN = function(x){
  -length(x)}
)

cake <- ggplot()+
  geom_bar(data = cf_types,
           aes(x = factor(1),fill = type_name),
           width = 1,color = 'black')+
  coord_polar(theta = 'y')+
  scale_fill_manual(values = c(PRIOlightblue,
                               PRIOpeach,
                               PRIOolive,
                               PRIOblue,
                               PRIOlightblue),
                    name = 'Ceasefire type')+
  theme_void()+
  theme(text = element_text('GillSans',size = 20),
        plot.margin = margin(r = 1,unit = 'cm'))

ggsave('./plots/cake.png',cake,width = 13,height = 8.5,dpi = 'print',units = 'cm',device = 'png')