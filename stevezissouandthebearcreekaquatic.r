sites <- levels(bear$site)
first_sites <- sites[1:6]



bear_first_sites <- bear %>% filter(site %in% first_sites)

p <- ggplot(bear_first_sites, aes(x=as.Date(date, 
                                format= "%m/%d/%Y"),
                      y=log10(E.coli_CFUper100ml)
                      , color=site))
p <- p + geom_area(alpha=.9, aes(fill=site))
p <- p + scale_color_brewer(palette = 1)
p <- p + scale_fill_manual(values=
                             c(wes_palette(n=3,"GrandBudapest2"),
                               wes_palette(n=3,"GrandBudapest")))
p <- p + theme_bw()
p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
p
library(plotly)
ggplotly(p)
