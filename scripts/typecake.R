cf_types <- cf_tee%>%
  select(start_date,
         end_date,
         type = Type_2,
         comment = Comments,
         dyad = UCDP_Dyad_1,
         id = CF_ID)%>%
  mutate(type = as.numeric(str_extract(type,'[0-9]')),
         dyad = as.numeric(dyad))%>%
  unique()
cf_types$dyad_name <- dyadvec[as.character(cf_types$dyad)]
cf_types <- cf_types[-5,] # drop ambiguous case

cf_types$type_name <- if_else(cf_types$type == 1,
                              'Humanitarian',
                              if_else(cf_types$type == 2,
                                      'Peace process',
                                      if_else(str_detect(cf_types$comment,'[Ee]lection'),
                                              'Election',
                                              if_else(str_detect(cf_types$comment,'[Hh]ostage'),
                                                      'Hostage release',
                                                      'Religious / other'))))%>%
  ordered()
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

ggsave('./plots/pb2018_08/cake.png',cake,width = 13,height = 8.5,dpi = 'print',units = 'cm',device = 'png')