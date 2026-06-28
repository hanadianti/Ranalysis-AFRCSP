#Afforestation Study To generate Table S3, Fig$a, Fig$b, Fig5, Fig6, Fig S3

library(gdxrrw)
library(dplyr)
library(reshape2)
library(qdap)
library(stacomirtools)
library(dplyr)
library(reshape2)
library(tibble)
library(directlabels)
library(ggplot2)
library(ggpubr)
library(gganimate)
library(forcats)
library(tidyverse)
library(data.table)
library(dplyr)

base<- rgdx.param("afrcs_2025_04/bau.gdx", "PAFRSUP")
biod1<- rgdx.param("afrcs_2025_04/biod.gdx", "PAFRSUP")
soil1<- rgdx.param("afrcs_2025_04/soil.gdx", "PAFRSUP")
all<- rgdx.param("afrcs_2025_04/soilbiod.gdx", "PAFRSUP")
demand<- rgdx.param("afrcs_2025_04/demfwr.gdx", "PAFRSUP")
supply<- rgdx.param("afrcs_2025_04/sup.gdx", "PAFRSUP")
supdem<- rgdx.param("afrcs_2025_04/supdem.gdx", "PAFRSUP")


RG <- rgdx.set("data_prep.gdx", symName = 'MAP_RG') %>% 
  mutate(R=as.character(R), G=as.numeric(as.character(G)))

bioe_in <- function(scenario){
  dfin <- NULL
    for (i in seq(2010, 2100, 10)) {
      dir <- paste('..\\..\\..\\output\\gdx\\', scenario, '_BaU_NoCC\\bio\\',i,'.gdx', sep='')
      temp <- rgdx.param(dir, symName = 'PBIOSUP') %>%
        dcast(G+LB~.k) %>%
        mutate(Y=i, G=as.numeric(as.character(G)), LB = as.character(LB))
      dfin <- rbind(dfin, temp)
    }
  dfout <- merge(dfin, RG, by='G', all.x=T)
  return(dfout)
}

bioe_pric <-function(df){
  df_m <- df %>%
    group_by(Y,R) %>%
    arrange(Y, R, price) %>%
    do(mutate(.,r_sup = cumsum(.$quantity))) %>%
    ungroup()%>%
    group_by(Y)%>%
    arrange(Y,price)%>%
    do(mutate(.,w_sup = cumsum(.$quantity))) %>%
    ungroup()%>%
    group_by(Y) %>%
    mutate(perc = w_sup/max(w_sup)) %>%
    ungroup() %>%
    group_by(Y, R) %>%
    mutate(perc_r = r_sup/max(r_sup)) %>%
    ungroup()%>%
    group_by(Y)%>%
    arrange(desc(yield))%>%
    do(mutate(.,area_cum = cumsum(.$area))) %>%
    ungroup()
  return(df_m)
}

SSP_ALL <- bind_rows(base = base, soil1 = soil1, biod1 = biod1,  all = all,
                     demand = demand, supply = supply, supdem = supdem,
                     .id = 'Scenario') %>%
  mutate(Scenario = factor(Scenario, levels = c('base', 'soil1',  'biod1', 'all', 
                                                'demand', 'supply', 'supdem')))
filter( j%in% c("WLD"))%>%
filter( i%in% c("2100", "2010", "2050"))
  rename(RG=j)%>%
  #rename(Y=i, RG=j, value=value)
  #mutate(L = ifelse(i == "WLD", "AFR", i))
  #filter (i %in% c("2100"))%>%
  mutate (L="AFR")%>%
  rename("amount"="value")

SSP_ALL <- bind_rows(base = base, supdem = supdem,
                     .id = 'Scenario') %>%
  mutate(Scenario = factor(Scenario, levels = c('base', 'supdem')))
  filter( j%in% c("WLD")) 

SSP_ALL <- SSP_ALL %>%
  group_by(Scenario, j) %>%
  mutate(i= as.numeric(as.character(i)),  # Convert 'year' to numeric
         years_remaining = 2105 - i,           # Calculate years remaining
         result = value * years_remaining)        # Perform multiplication to get resultgroup_by(Scenario, rg) %>%



SSP_ALL2 <- SSP_ALL %>%
  group_by(Scenario,j) %>%
  mutate(value_cum=(cumsum(result))) 

SSP_ALL2 <- SSP_ALL2 %>%
  filter(i %in% c(2100))

print(SSP_ALL)

SSP_ALL<- spread(SSP_ALL, k, value)

supdem<- spread(supdem, k, value)


g <- base %>% 
  ggplot() +
  geom_bar(aes(x=i1, y=area, fill=i3),
           stat="identity",position="dodge")
plot(g)

SSP_POT_W <- SSP_ALL %>%
  group_by(Scenario,i) %>%
  summarise(pot_w = max(w_sup))%>%
  ungroup()%>%
  group_by(i)%>%
  mutate(perc = pot_w/max(pot_w))%>%
  ungroup()

SSP_POT_R_OECD <- SSP_ALL2 %>%
  mutate(RG=ifelse(j %in% c('USA','CAN','JPN','XE25','XOC', 'TUR', 'XER'), 'OECD', j))

SSP_POT_R_OECD <- SSP_ALL %>%
  mutate(RG = ifelse(j %in% c('USA', 'CAN', 'JPN', 'XE25', 'XOC', 'TUR', 'XER'), 
                     'OECD', as.character(j)))

SSP_POT_R_OECD <- SSP_POT_R_OECD %>%
  filter(i %in% c(2100)) %>%
  group_by(Scenario,RG) %>%
  filter(row_number()==1)
  
library(dplyr)

# Example mutate operation
SSP_POT_R_OECD <- SSP_ALL %>%
  mutate(RG = ifelse(j %in% c('USA', 'CAN', 'JPN', 'XE25', 'XOC', 'TUR', 'XER'), 'OECD', as.character(j)))

head(SSP_POT_R_OECD)

SSP_ARE_R_OECD <- SSP_ALL %>%
  mutate(RG = ifelse(j %in% c('USA', 'CAN', 'JPN', 'XE25', 'XOC', 'TUR', 'XER'), 'OECD', as.character(j))) %>%
  rename(Y = i) %>%
  group_by(Scenario, Y, RG) %>%
  summarise(are_r = sum(value)) %>%
  ungroup() %>%
  group_by(Y, RG) %>%
  mutate(perc = are_r / max(are_r)) %>%
  ungroup()



SSP_value_OECD <- SSP_ALL %>% 
  #filter(j != "WLD")%>%
  mutate(RG = ifelse(j %in% c('USA', 'CAN', 'JPN', 'XE25', 'XOC', 'TUR', 'XER'), 'OECD', as.character(j))) %>%
  group_by(Scenario, i, RG) %>%
  mutate(value = value) %>%
  ungroup() %>%
  group_by(i, RG) %>%
  mutate(perc = value / max(value)) %>%
  ungroup() %>%
  rename(Y = i)


head (SSP_ALL)
class(SSP_ALL$area)



lbs <- c("Baseline", 
         "Soil Quality Enhancement",
         "Biodiversity Protection", 
         "Full Environmental Consideration",
         "Dietary Shift and Food Waste Reduction", 
         "Food Trade and Technological Advancement", 
         "Full Environmental Consideration and 
Full Sustainable Food System")



tabs <- c(" ",
          "Environmental Consideration",
          "Environmental Consideration",
          "Environmental Consideration",
          'Sustainable Food System',
          'Sustainable Food System',
          'Sustainable Food System')

bks <- c('base', 'soil1',  'biod1', 'all', 
         'demand', 'supply', 'supdem')

length(bks)

########################################
##                                    ##
##       Global potential plot        ##
##                                    ##
########################################

install.packages("RColorBrewer")  # Install RColorBrewer package
library(RColorBrewer)  # Load RColorBrewer package

color_vector <- get_palette('Dark2', 7)
col <- get_palette("jco", 7)

# 2050
color_vector <- get_palette('Dark2', 9)
col <- get_palette("jco", 5)

SSP_ALL2 <- SSP_ALL %>%
filter( i%in% c(2100))%>%
  filter (j=="WLD")
  rename (i=Y)

# 2100
glo_comp1 <- ggplot(SSP_ALL2 %>% 
                      filter( i%in% c(2100)) %>%
                      # filter( j%in% c("WLD")) %>%
                      # mutate(Y=factor(i, levels = c(2100)))%>%
                      mutate(Scenario=factor(Scenario, levels = rev(bks)))%>%
                      ungroup() %>%
                      merge(data.frame(bks, tabs), by.x = 'Scenario', by.y='bks')%>%
                      filter(tabs==" "),
                    aes(x=Scenario, y = value))+
  geom_segment(size = 2, aes(x = Scenario,y = value,xend =Scenario,yend = 0,color=Scenario)) +
  coord_flip()+
  ylim(0,7)+
  scale_color_manual(values = rev(c(col[4])),
                     breaks = bks[1],
                     labels = lbs[1],
                     name = ' ')+
  scale_x_discrete(breaks = bks[1],
                   labels = lbs[1])+
  geom_text(aes(label=paste(round(value,2), "GtCO2", sep=' ')),
            position = position_dodge(0.8),hjust=-0.2,vjust=0.4,color='gray30', size=3)+
  theme_minimal() +
  ylab('Afforestation Carbon Sequestration Potential in 2100 (GtCO2)') +
  xlab('')+
  theme(axis.text.y=element_text(color = 'grey30', hjust = 0),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0, 0, "cm")) +
  theme(axis.ticks=element_blank())+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  theme(axis.text.x = element_blank())+
  theme(axis.title.x = element_blank())


glo_comp2 <- ggplot(SSP_ALL2 %>% 
                      filter( i%in% c(2100)) %>%
                      #filter( j%in% c("WLD")) %>%
                      #mutate(Y=factor(i, levels = c(2100)))%>%
                      mutate(Scenario=factor(Scenario, levels = rev(bks)))%>%
                      ungroup() %>%
                      merge(data.frame(bks, tabs), by.x = 'Scenario', by.y='bks')%>%
                      filter(tabs=="Environmental Consideration"),
                    aes(x=Scenario, y = value))+
  geom_segment(size = 2, aes(x = Scenario,y = value,xend =Scenario,yend = 0,color=Scenario)) +
  coord_flip()+
  ylim(0,7)+
  scale_color_manual(values = rev(rep(col[1],3)),
                     breaks = bks[2:4],
                     labels = lbs[2:4],
                     name = 'Environmental Consideration')+
  scale_x_discrete(breaks = bks[2:4],
                   labels = lbs[2:4])+
  geom_text(aes(label=paste(round(value,2), "GtCO2", sep=' ')),
            position = position_dodge(0.8),hjust=-0.2,vjust=0.4,color='gray30', size=3)+
  theme_minimal() +
  ylab('Afforestation Carbon Sequestration Potential (GtCO2)') +
  xlab('')+
  theme(axis.text.y=element_text(color = 'grey30', hjust = 0),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0, 0, "cm")) +
  theme(axis.ticks=element_blank())+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  ggtitle('')+
  theme(axis.text.x = element_blank())+
  theme(axis.title.x = element_blank())+
  theme(title = element_text(size = 8))


glo_comp3 <- ggplot(SSP_ALL2 %>% 
                      #filter( i%in% c(2100)) %>%
                      #filter( j%in% c("WLD")) %>%
                      #mutate(Y=factor(i, levels = c(2100)))%>%
                      mutate(Scenario=factor(Scenario, levels = rev(bks)))%>%
                      ungroup() %>%
                      merge(data.frame(bks, tabs), by.x = 'Scenario', by.y='bks') %>%
                      filter(tabs=="Sustainable Food System"),
                    aes(x=Scenario, y = value))+
  geom_segment(size = 2, aes(x = Scenario,y = value,xend =Scenario,yend = 0,color=Scenario)) +
  coord_flip()+
  ylim(0,7)+
  scale_color_manual(values = rev(rep(col[2],3)),
                     breaks = bks[5:7],
                     labels = lbs[5:7],
                     name = 'Environmental Consideration')+
  scale_x_discrete(breaks = bks[5:7],
                   labels = lbs[5:7])+
  geom_text(aes(label=paste(round(value,2), "GtCO2", sep=' ')),
            position = position_dodge(0.8),hjust=-0.2,vjust=0.4,color='gray30', size=3)+
  theme_minimal() +
  ylab('Global Technical Afforestation Carbon Sequestration Potential in 2100 (GtCO2)') +
  xlab('')+
  theme(axis.text.y=element_text(color = 'grey30', hjust = 0),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "none",
        plot.margin = margin(0.2, 0.2, 0, 0, "cm")) +
  theme(axis.ticks=element_blank())+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))+
  ggtitle('')+
  theme(title = element_text(size = 8))

glo_comp2

cb <- ggarrange(glo_comp1, glo_comp2, glo_comp3, ncol = 1, nrow=3,
                heights = c(1,2.7,3), align = 'v')
cb



ggsave("Fig 3.  Global Cumulative Afforestation Carbon   Sequestration Potential in 2100 under each scenario.eps", cb,device = 'jpeg', width = 20*0.5,height = 12*0.35, dpi = 800)

ggsave("Fig 3.   Global Cumulative Afforestation Carbon   Sequestration Potential in 2100 under each scenario.jpeg", cb,device = 'jpeg', width = 20*0.5,height = 12*0.35, dpi = 600)

########################################
##                                    ##
##      Regional potential plot  OECD ##
##                                    ##
########################################

reg <- factor(c("CIS", "XAF", "XLM", "CHN", "OECD",
                "BRA", "XSE", "XNF", "XME",
                "XSA","IND"))

reg_lab <- c( 'Former\nSoviet\nUnion', 'Rest of\nAfrica','Rest of\nSouth\nAmerica', 
             'China', 'OECD', 'Brazil', 'Southeast\nAsia', 'North\nAfrica', 
             'Middle\nEast', 'Rest of\nAsia', 'India')

merged_data2 <- SSP_ALL %>%
  rename(Y = i)%>%
  filter(Y %in% c(2100)) %>%
  rename(RG = j)%>%
  group_by(Scenario,RG) %>%
  filter(row_number()==1) 

reg_area_OECD <- ggplot(merged_data2 %>% 
                          filter(Y %in% c(2100)) %>%
                          mutate(value=value/1000) %>% # convert to million ha
                          #mutate(Y=factor(Y, levels = c(2050)))%>%
                          mutate(R=factor(RG, levels = c("CIS","XAF", "BRA", "XLM", "OECD", "XSE",
                                                         "IND", "CHN", "XME", "XSA",
                                                         "XNF")))%>%
                          merge(data.frame(RG=reg, reg_lab), by='RG')%>%
                          mutate(reg_lab=factor(reg_lab, levels = c('Former\nSoviet\nUnion','Rest of\nAfrica','Brazil', 'Rest of\nSouth\nAmerica', 'OECD',
                                                                    'Southeast\nAsia', 'India', 'China', 'Middle\nEast', 'Rest of\nAsia',
                                                                    'North\nAfrica')))%>% 
                          mutate(Scenario=factor(Scenario, levels = bks))%>%
                          ungroup(),
                        aes(x=Scenario, y = value))+
  #  geom_bar(stat = "identity", position = "dodge", aes(fill=Scenario)) +
  geom_segment(size = 1.2, aes(x = Scenario,y = value,xend =Scenario,yend = 0, color=Scenario)) +
  geom_point(size=2,aes(color=Scenario,shape=Scenario))+
  facet_wrap(~reg_lab, nrow = 1,strip.position = "bottom")+
  scale_color_manual(values = c(col[4], rep(col[1],3), rep(col[2],3)),
                     breaks = bks,
                     labels = lbs,
                     name = 'Scenario')+
  scale_shape_manual(values = 0:8,
                     breaks = bks,
                     labels = lbs,
                     name = 'Scenario')+
  scale_y_continuous(breaks = seq(0,300,100),labels = seq(0,300,100), limits = c(0,300))+
  theme_minimal() +
  xlab('') +
  ylab(expression(paste("Afforestation Land (Million ha)", sep = "")))+
  theme(axis.text.y=element_text(color = 'black'),
        axis.text.x=element_blank(),
        axis.title.x = element_text(colour = "black", size= 10, vjust = 2)) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.spacing.x = unit(0.5, "lines"))+
  theme(legend.position = 'top', legend.box.background = element_blank(),
        strip.text.x = element_text(size=9),
        plot.margin = margin(0.5, 0.5, 0, 0, "cm"))+
  guides(col = guide_legend(byrow=T))+
  geom_hline(yintercept = 0) +
  theme(legend.position = c(0.62, 0.88))+
  theme(strip.text.x = element_text(size=9,angle = 0,vjust=1),
        axis.title.x = element_blank())+
  guides(fill=guide_legend(ncol =2),
         shape=guide_legend(ncol =2),
         color=guide_legend(ncol =2))

reg_area_OECD


ggsave("Fig 5. Regional Afforestation carbon sequestration potential in 2100 under each scenario.jpeg", reg_area_OECD,device = 'jpeg', 
       width = 22*0.35,height = 16*0.35, dpi = 600)

ggsave("Fig 5. Regional Afforestation carbon sequestration potential in 2100 under each scenario.eps", reg_area_OECD,device = 'jpeg', 
       width = 22*0.35,height = 16*0.35, dpi = 600)



########################################
##                                    ##
##      Regional area plot  OECD      ##
##                                    ##
########################################

head (SSP_value_OECD)

SSP_value_OECD2 <- SSP_value_OECD %>%
  filter(Y %in% c(2100)) %>%
  group_by(Scenario, RG) %>%
  filter(row_number()==1) %>%
  group_by(Scenario, RG, Y)
  mutate (sum = value-(21.1/100*value) )


library(dplyr)
library(ggplot2)

# Rename scenario labels and plot
g <- SSP_value_OECD %>%
  filter(j %in% c("WLD")) %>%
  filter(Scenario %in% c("base", "biod1", "soil1", "all", "demand")) %>%
  mutate(Scenario = recode(Scenario,
                           "biod1" = "Cropland",
                           "soil1" = "Potential area for Afforestation",
                           "base" = "Afforested Land",
                           "all"= "Pasture Land", 
                           "demand"="Forest & Grassland")) %>%
  ggplot() +
  geom_area(aes(x = Y, y = value / 1000, group = Scenario, fill = Scenario)) +
  labs(
    x = "Year",
    y = "Area (Million Ha)",
    color = "-"
  ) +
  theme_minimal()

# Plot the graph
plot(g)

head(SSP_value_OECD)
reg_area_OECD <- ggplot(SSP_value_OECD %>% 
                          filter(Y %in% c(2100)) %>%
                          #filter(Scenario %in% c("base", "biod1", "soil1")) %>%
                          #mutate(=value/1000) %>% # convert to million ha
                          #mutate(Y=factor(Y, levels = c(2050)))%>%
                          #mutate(RG=factor(RG, levels = c("XAF","CIS", "XLM", "CHN", "OECD",
                                                          #"BRA", "XSE", "XNF", "XME",
                                                          #"XSA","IND")))%>%
                          #merge(data.frame(RG=reg, reg_lab), by='RG')%>%
                          mutate(reg_lab=factor(reg_lab, levels = c('Rest of\nAfrica', 'Former\nSoviet\nUnion', 'Rest of\nSouth\nAmerica', 
                                                                    'China', 'OECD', 'Brazil', 'Southeast\nAsia', 'North\nAfrica', 
                                                                    'Middle\nEast', 'Rest of\nAsia', 'India')))%>%   
                          mutate(Scenario=factor(Scenario, levels = bks))%>%
                          ungroup(),
                        aes(x=Scenario, y = value))+
#  geom_bar(stat = "identity", position = "dodge", aes(fill=Scenario)) +
  geom_segment(size = 1.2, aes(x = Scenario,y = value,xend =Scenario,yend = 0, color=Scenario)) +
  geom_point(size=2,aes(color=Scenario,shape=Scenario))+
  facet_wrap(~reg_lab, nrow = 1,strip.position = "bottom")+
  scale_color_manual(values = c(col[4], rep(col[1],5), rep(col[2],3)),
                     breaks = bks,
                     labels = lbs,
                     name = 'Scenario')+
  scale_shape_manual(values = 0:8,
                     breaks = bks,
                     labels = lbs,
                     name = 'Scenario')+
  #scale_y_continuous(breaks = seq(0,300,100),labels = seq(0,300,100), limits = c(0,300))+
  theme_minimal() +
  xlab('Region') +
  ylab(expression(paste("Afforestation land (Mha)", sep = "")))+
  theme(axis.text.y=element_text(color = 'black'),
        axis.text.x=element_blank(),
        axis.title.x = element_text(colour = "black", size= 10, vjust = 2)) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.spacing.x = unit(0.5, "lines"))+
  theme(legend.position = 'top', legend.box.background = element_blank(),
        strip.text.x = element_text(size=9),
        plot.margin = margin(0.5, 0.5, 0, 0, "cm"))+
  guides(col = guide_legend(byrow=T))+
  geom_hline(yintercept = 0) +
  theme(legend.position = c(0.62, 0.72))+
  theme(strip.text.x = element_text(size=9,angle = 0,vjust=1),
        axis.title.x = element_blank())+
  guides(fill=guide_legend(ncol =2),
         shape=guide_legend(ncol =2),
           color=guide_legend(ncol =2))

reg_area_OECD

reg_area_OECD_border <- ggarrange(reg_area_OECD)+
  geom_segment(aes(x = 0.06, y = 0.92, xend = 0.98, yend = 0.92))+
  geom_segment(aes(x = 0.06, y = 0.12, xend = 0.06, yend = 0.92))+
  geom_segment(aes(x = 0.06, y = 0.12, xend = 0.98, yend = 0.12))+
  geom_segment(aes(x = 0.98, y = 0.12, xend = 0.98, yend = 0.92))

ggsave("FIG S3. ", g,device = 'jpeg', 
       width = 22*0.35,height = 15*0.35, dpi = 600)

########################################
##                                    ##
## Price percentage and supply curve  ##
##                                    ##
########################################


policy_df <- data.frame(Scenario = bks,
                        Policy= lbs)

supply_curve_df <- SSP_ALL %>% 
  group_by(Scenario) %>%
  #filter(Scenario=="base")%>% 
  #filter(quantity>0,area>0,fraction>0,yield>0)%>%
  mutate(area_cum=(cumsum(area))/100000) %>%
  merge(policy_df, by= 'Scenario', all.x=T)%>%
  filter(price<300)%>%
  #filter(area_cum)%>%
  group_by(Scenario) %>%
  arrange(price)%>%
  do(mutate(.,w_sup = cumsum(.$quantity))) %>%
  mutate(Policy=factor(Policy, levels = lbs))

supply_point_df <- supply_curve_df %>%
  mutate(rg = ifelse(price>0.9 & price<50,50, NA)) %>%
  mutate(rg = ifelse(price>50 & price<100,100, rg)) %>%
  mutate(rg = ifelse(price>100 & price<120,120, rg)) %>%
  mutate(rg = ifelse(price>120 & price<150,150, rg)) %>%
  mutate(rg = ifelse(price>150 & price<180,180, rg)) %>%
  mutate(rg = ifelse(price>150 & price<200,200, rg)) %>%
  mutate(rg = ifelse(price>200 & price<220,220, rg)) %>%
  mutate(rg = ifelse(price>220 & price<250,250, rg)) %>%
  mutate(rg = ifelse(price>250 & price<280,280, rg)) %>%
  mutate(rg = ifelse(price>280 & price<300,300, rg)) %>%
  #mutate(rg = ifelse(price>300 & price<350,350, rg)) %>%
  #mutate(rg = ifelse(price>350 & price<400,400, rg)) %>%
  filter(!is.na(rg)) %>%
  group_by( Scenario,rg) %>%
filter(row_number()==1)

supplycurve <- ggplot(supply_curve_df %>% 
                             filter(Scenario %in% bks)) +
  geom_line(aes(x=w_sup/1000000000, y=price, linetype = Policy, color=Policy))+
  xlim(0,6)+
  xlab('Afforestation Carbon Sequestration Potential 
       (GtCO2/year) in 2100') +
  ylab('Carbon Sequestration Cost ($/tCO2)')+
  theme_minimal() +
  scale_y_continuous(minor_breaks = seq(0, 300, 100), breaks = seq(0, 300,100), limits = c(0,300))+
  geom_point(data=supply_point_df%>%
               filter(Scenario %in% bks), aes(x=w_sup/1000000000, y=price, shape=Policy, color=Policy))+
  theme(#axis.line = element_line(size = 0.4, colour = "black"),
    panel.spacing = unit(1, "lines"),
    #        axis.ticks.x = element_line(size = 0.5, colour = "black"),
    #        axis.ticks.y = element_line(size = 0.5, colour = "black"),
    panel.grid.major.y= element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank())+
  theme(panel.border = element_rect(colour = "black", fill=NA, size =0.5))+
  theme(legend.key.width=unit(2,"line"),
        legend.box.background = element_rect()) +
  scale_linetype_manual(values = rep("solid",9),
                        breaks = lbs,
                        labels = lbs,
                        name='Scenario')+
  scale_shape_manual(values = 0:8,
                     breaks = lbs,
                     labels = lbs,
                     name = 'Scenario')+
  scale_color_manual(values = c(col[4], rep(col[1],3), rep(col[2],3)),
                     breaks = lbs,
                     labels = lbs,
                     name = 'Scenario')+
  theme(legend.position = 'right', legend.box.background = element_blank(),
        strip.text.x = element_text(size=9),
        plot.margin = margin(0.5, 0.5, 0, 0, "cm"))+
  guides(col = guide_legend(byrow=T))

#c("solid", "dotted", "dotdash", "longdash", "dashed", "twodash", "F1", "1F", "12345678")


supplycurve

max_value <- max(supply_curve_df$w_sup, na.rm = TRUE)
print(max_value)

ggsave("Fig 6. Afforestation Carbon Sequestration Potential Supply curve in 2100 under each scenario .eps", supplycurve,device = 'jpeg', width = 14*0.55,height = 6*0.55, dpi = 800)

ggsave("Fig 6. Afforestation Carbon Sequestration Potential Supply curve in 2100 under each scenario .jpeg", supplycurve,device = 'jpeg', width = 12*0.55,height = 6*0.55, dpi = 300)

########################################
##                                    ##
##           Area yield curve         ##
##                                    ##
########################################

area_curve_df <- SSP_ALL %>% 
  group_by(Scenario,j)%>%
  mutate(y_cum=cumsum(yield))%>% # convert to million ha
  group_by(Scenario)%>%
  arrange(desc(y_cum))%>%
  do(mutate(.,area_cum = cumsum(.$area))) %>%
  mutate(area_cum=area_cum/100000*100) %>% 
  merge(policy_df, by= 'Scenario', all.x=T)%>%
  mutate(yield2=y_cum) %>% 
  mutate(Policy=factor(Policy, levels = lbs))
  mutate(area_cum=area_cum-600)

area_point_df <- area_curve_df %>%
  mutate(rgrd = round(area_cum/100)*100) %>%
  filter(rgrd > 0) %>%
  mutate(rg = ifelse(yield >rgrd-0.01 |yield <rgrd + 0.01, rgrd, NA)) %>%
  filter(!is.na(rg)) %>%
  group_by(Scenario, rg) %>%
  filter(row_number()==1)


areayield <- ggplot(area_curve_df%>%
                           filter(Scenario %in% bks)) +
  geom_line(aes(x=area_cum, y=yield2, linetype = Policy, color=Policy))+
  geom_point(data=area_point_df%>%
               filter(Scenario %in% bks), aes(x = area_cum, y = yield2, shape=Policy, color=Policy))+
  #xlim(0,2000)+ylim(0,40) +
  ylab("Carbon Sequester (tC/ha/yr)") +
  xlab(expression(paste("Cumulative area (million ", ha,")", sep = ""))) + 
  scale_linetype_manual(values = rep("solid",9),
                        breaks = lbs,
                        labels = lbs,
                        name='Scenario')+
  scale_shape_manual(values = 0:8,
                     breaks = lbs,
                     labels = lbs,
                     name = 'Scenario')+
  scale_color_manual(values = c(col[4], rep(col[1],3), rep(col[2],3)),
                     breaks = lbs,
                     labels = lbs,
                     name = 'Scenario')+
  theme_minimal() +
  theme(legend.key.width=unit(2,"line"),legend.box.background = element_rect()) +
  theme(#axis.line = element_line(size = 0.4, colour = "grey80"),
    #        legend.position = 'none',
    panel.spacing = unit(1, "lines"),
    #        axis.ticks.x = element_line(size = 0.4, colour = "grey80"),
    #        axis.ticks.y = element_line(size = 0.4, colour = "grey80"),
    panel.grid.major.y= element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank()) +
  theme(legend.position = 'right', legend.box.background = element_blank(),
        strip.text.x = element_text(size=9),
        plot.margin = margin(0.5, 0.5, 0, 0, "cm"))+
  #  guides(colour = guide_legend(byrow=T, nrow = 2),
  #         shape = guide_legend(byrow=T, nrow = 2),
  #         linetype = guide_legend(byrow=T, nrow = 2))+
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))


areayield

ggsave("Fig 6. Global afforestation area-yield curve in 2100 under each scenario   .eps", areayield,device = 'jpeg', width = 12*0.55,height = 6*0.55, dpi = 800)

ggsave("Fig 6. Global afforestation area-yield curve in 2100 under each scenario   .jpg", areayield,device = 'jpeg', width = 12*0.55,height = 6*0.55, dpi = 800)



#################################################################################################

# Table s3. Regional quantile prices

# contain OECD region
SSP_ALL<- rename(SSP_ALL,"G"=j)
SSP_ALL <- SSP_ALL %>%
  left_join(RG, by = "G")
SSP_ALL$G <- as.numeric(as.character(SSP_ALL$G))

SSP_ALL %>%
  filter( Scenario=='supdem')%>%
  filter( price>0, fraction>0, area>0, quantity>0)%>%
  mutate(RG=ifelse(R %in% c('USA','CAN','JPN','XE25','XOC', 'TUR', 'XER'), 'OECD', R)) %>%
  #filter(Y==2050)%>%
  dplyr::select(Scenario, RG, price, quantity,R) %>%
  arrange(Scenario, RG, price) %>%
  group_by(Scenario, RG)%>%
  summarize(price_wm=weighted.mean(price, quantity)) %>%
  filter(Scenario=='supdem') %>% arrange(price_wm)

# all region
SSP_ALL %>% 
  filter( Scenario=='supdem')%>%
  filter( price>0, fraction>0, area>0, quantity>0)%>%
  mutate(RG=ifelse(R %in% c('USA','CAN','JPN','XE25','XOC', 'TUR', 'XER'), 'OECD', R)) %>%
  #filter(Y==2050)%>%
  dplyr::select(Scenario, price, quantity,R) %>%
  arrange(Scenario, R, price) %>%
  group_by(Scenario, R)%>%
  summarize(price_wm=weighted.mean(price, quantity)) %>%
  filter(Scenario=='supdem') %>% arrange(price_wm)




reg <- 'BRA'


price_quantile_reg_17 <- SSP_ALL %>% 
  filter( price>0, fraction>0, area>0, quantity>0)%>%
  mutate(RG=ifelse(R %in% c('USA','CAN','JPN','XE25','XOC', 'TUR', 'XER'), 'OECD', R)) %>%
  dplyr::select(Scenario, price, quantity,R) %>%
  group_by(Scenario, R)%>%
  arrange(Scenario, R, price) %>%
  mutate(quant_cum = cumsum(quantity), perc_reg = quant_cum/max(quant_cum))

price_quantile_reg_oecd <- SSP_ALL %>% 
  filter( price>0, fraction>0, area>0, quantity>0)%>%
  mutate(R=ifelse(R %in% c('USA','CAN','JPN','XE25','XOC', 'TUR', 'XER'), 'OECD', R)) %>%
  filter( Scenario=='supdem')%>%
  dplyr::select(Scenario,R, price, quantity) %>%
  group_by(Scenario, R)%>%
  arrange(Scenario, R, price) %>%
  mutate(quant_cum = cumsum(quantity), perc_reg = quant_cum/max(quant_cum))


quantfunc <- function(dataframe,reg,quant){
  
  temp <- filter(dataframe, R==reg) %>%
    mutate(diff = perc_reg - quant/100)%>%
    mutate(pos = ifelse(diff>0, diff, NA),
           neg = ifelse(diff<0, diff, NA)) %>%
    mutate(close_up = min(pos, na.rm = T)) %>%
    mutate(close_dw = max(neg, na.rm = T)) %>%
    mutate(up_pin = pos==close_up,
           dw_pin = neg==close_dw)   %>%
    filter(up_pin|dw_pin) %>%
    mutate(sign=ifelse(pos>0, 'positive', NA)) %>%
    mutate(sign=ifelse(is.na(sign), 'negative', sign)) %>%
    group_by(sign) %>%
    summarise(price = mean(price),
              perc_reg= mean(perc_reg))
    
  pricee =as.numeric((quant/100-temp[2,3])/(temp[1,3] - temp[2,3]) * (temp[1,2] - temp[2,2]) + temp[2,2])
  
  rt <- data.frame(reg=reg, quant=quant, price=pricee, stringsAsFactors = F)
    
  return(rt)
}
  
REGs <- c("BRA","XLM", "XAF", 'USA','CAN','JPN','XE25','XOC', 'TUR', 'XER', "XSE",
                "IND", "CHN", "XME", "XSA","CIS","XNF")

quants <- c(10,20,30,40,50,60,70,80,90)
quants <- c(100,200,300,400,500,600,700,800,900)


df <- NULL

for (reg in REGs) {
  for (i in quants) {
    df <- rbind(df, quantfunc(price_quantile_reg_17,reg,i))
    
  }
  
}


dfoecd <- NULL

for (reg in 'OECD') {
  for (i in quants) {
    dfoecd <- rbind(dfoecd, quantfunc(price_quantile_reg_oecd,reg,i))
    
  }
  
}


exportdf <- rbind(df, dfoecd) %>%
  mutate(price=round(price, 1)) %>%
  dcast(reg~quant) %>%
  arrange(`500`)

write.csv(exportdf, "Table S3. price_quantile.csv", row.names = F)


