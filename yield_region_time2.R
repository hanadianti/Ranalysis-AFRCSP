# Fig 2

rg <- rgdx.set("data_prep.gdx", symName = 'MAP_RG') %>%
  mutate(R=as.character(R), G=as.numeric(as.character(G))) %>%
  mutate(RG=ifelse(R %in% c('USA','CAN','JPN','XE25','XOC', 'TUR', 'XER'), 'OECD', R))


yield2010 <- rgdx.param("base/2010.gdx", symName = 'YIELDAFR') %>%
  mutate(G=as.numeric(as.character(G))) %>%
  merge(rg %>% dplyr::select(G, RG), by = 'G', all.x = T)

yield2020 <- rgdx.param("base/2020.gdx", symName = 'YIELDAFR_real')%>%
  mutate(G=as.numeric(as.character(G))) %>%
  merge(rg %>% dplyr::select(G, RG), by = 'G', all.x = T)

yield2030 <- rgdx.param("base/2030.gdx", symName = 'YIELDAFR_real')%>%
  mutate(G=as.numeric(as.character(G))) %>%
  merge(rg %>% dplyr::select(G, RG), by = 'G', all.x = T)

yield2040 <- rgdx.param("base/2040.gdx", symName = 'YIELDAFR_real')%>%
  mutate(G=as.numeric(as.character(G))) %>%
  merge(rg %>% dplyr::select(G, RG), by = 'G', all.x = T)

yield2050 <- rgdx.param("base/2050.gdx", symName = 'YIELDAFR_real')%>%
  mutate(G=as.numeric(as.character(G))) %>%
  merge(rg %>% dplyr::select(G, RG), by = 'G', all.x = T)

yield2060 <- rgdx.param("base/2060.gdx", symName = 'YIELDAFR_real')%>%
  mutate(G=as.numeric(as.character(G))) %>%
  merge(rg %>% dplyr::select(G, RG), by = 'G', all.x = T)

yield2070 <- rgdx.param("base/2070.gdx", symName = 'YIELDAFR_real')%>%
  mutate(G=as.numeric(as.character(G))) %>%
  merge(rg %>% dplyr::select(G, RG), by = 'G', all.x = T)

yield2080 <- rgdx.param("base/2080.gdx", symName = 'YIELDAFR_real')%>%
  mutate(G=as.numeric(as.character(G))) %>%
  merge(rg %>% dplyr::select(G, RG), by = 'G', all.x = T)

yield2090 <- rgdx.param("base/2090.gdx", symName = 'YIELDAFR_real')%>%
  mutate(G=as.numeric(as.character(G))) %>%
  merge(rg %>% dplyr::select(G, RG), by = 'G', all.x = T)

yield2100 <- rgdx.param("base/2100.gdx", symName = 'YIELDAFR_real')%>%
  mutate(G=as.numeric(as.character(G))) %>%
  merge(rg %>% dplyr::select(G, RG), by = 'G', all.x = T)


base<- rgdx.param("Paper4/base.gdx", "PAFRSUP")

pca_all = bind_rows(y2010=yield2010, y2020=yield2020, y2030=yield2030, y2040=yield2040, y2050=yield2050, 
                      y2060=yield2060, y2070=yield2070, y2080=yield2080, y2090=yield2090, y2100=yield2100, .id='Y') %>%
  mutate(Y=as.numeric(substr(Y,2,5))) %>%
  group_by(RG, Y) %>%
  summarise(pca_avg = mean(YIELDAFR_real) * 2) ## should check if needs to multiply by 2


reg <- (c("BRA","XLM", "XAF", "OECD", "XSE",
          "IND", "CHN", "XME", "XSA",
          "CIS","XNF"))

reg_lab_yield <- c('Brazil', 'Rest of South America', 'Rest of Africa','OECD',
                   'Southeast Asia', 'India', 'China', 'Middle East', 'Rest of Asia',
                   'Former Soviet Union', 'North Africa')


library(directlabels)

vv=brewer.pal(11, "Paired")

colvecadj = c(vv[1],vv[10],vv[8],vv[7],vv[11],vv[4],
              vv[2],vv[5],vv[8],vv[3],vv[6])

pca_all <- pca_all %>%
  #filter(Y <= 2100) %>%
  #mutate(tree_age = 2100 - Y) %>%  # Create tree age column
  merge(data.frame(RG = reg, RGfull = reg_lab_yield), by = 'RG', all.x = TRUE)

# Create the plot
pyd <- ggplot(yield_all, 
              aes(x = tree_age, y = yield_avg / 3.67, linetype = RGfull, color = RGfull, label = RGfull)) +
  geom_line() +
  theme_minimal() +
  scale_linetype_discrete(breaks = reg_lab_yield, name = 'Region') +
  scale_color_manual(breaks = reg_lab_yield, name = 'Region', values = colvecadj) +
  scale_x_continuous(breaks = seq(0, 90, 10), labels = seq(0, 90, 10), limits = c(0, 90)) +  # Adjust x-axis
  xlab('Tree Age (years)') + ylab('Biomass (tonne/ha/yr)') +
  theme(axis.text.y = element_text(color = 'grey30', hjust = 0),
        legend.key.width = unit(1, "cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = margin(1, 0.2, 0, 0, "cm")) +
  theme(axis.ticks = element_blank()) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 0.5))

# Display the plot
pyd

install.packages("ggplot2")
p<- ggplot(pca_all %>% 
             # filter(Y<=2050) %>%
             merge(data.frame(RG=reg, RGfull = reg_lab_yield), by='RG', all.x=T) %>%
             mutate(pca_avg=pca_avg), 
           aes(x=Y, y=pca_avg, linetype=RGfull,color=RGfull, label=RGfull)) +
  geom_line() +
  theme_minimal()+
  scale_linetype_discrete(breaks= reg_lab_yield,
                          name='Region')+
  scale_color_manual(breaks = reg_lab_yield,
                     name='Region',
                     values = colvecadj)+
  scale_x_continuous(breaks = seq(2010,2100,10), labels = seq(2010,2100,10), limits = c(2010,2100))+
  xlab('Year') + ylab('Afforestation Carbon Sequestration Cost (US$/ha/yr)')+
  theme(axis.text.y=element_text(color = 'grey30', hjust = 0),
        legend.key.width = unit(1,"cm"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "right",
        plot.margin = margin(1, 0.2, 0, 0, "cm")) +
  theme(axis.ticks=element_blank())+
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth=0.5))

p

costyield_cb <-ggarrange(pyd, p, nrow = 1, ncol = 2, common.legend = T, labels = c('(a)', '(b)'),
                         label.x = 0.5, label.y = 1, widths= c(1,1.035),
                         legend='bottom')

install.packages("ggpubr")

ggsave("Figure 2. yield_cost.jpeg",
       device = 'jpeg', width = 12*0.6,height = 7*0.6, dpi = 600)

