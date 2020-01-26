library("ggplot2")
library("plotly")
library("ggthemes")
library("dplyr")
library("reshape2")
library("hrbrthemes")
library("viridis")
library("tidyr")




# Armenia Regions

am <- read.csv("am_density.csv")

am_density <- ggplotly(ggplot(data=am, aes(x=am$ï..Province, y=am$Density, color =am$Capital, size=as.numeric(am$Population))) +
           geom_point(alpha=0.8)+
           scale_y_log10()+
           scale_size(range = c(1, 11))+
           labs(x="Region", y="Density")+
           theme_ipsum()+
           theme(axis.text.x  = element_blank(),axis.ticks.x  =element_blank(),
                 legend.title = element_blank(),legend.position = "bottom",
                 axis.title.x = element_text(size = 16, face = "bold"),
                 axis.title.y = element_text(size = 16, face = "bold")))

# Georgia Regions

ge <- read.csv("ge_density.csv")

ge_density <- ggplotly(ggplot(data=ge, aes(x=ge$ï..Region, y=ge$Density, color= ge$Centre, size=as.numeric(ge$Population))) +
           geom_point(alpha=0.8)+ 
           scale_y_log10()+
           scale_size(range = c(1, 12))+
           labs(x="Region", y="Density")+
           theme_ipsum()+
             theme(axis.text.x  = element_blank(),axis.ticks.x  =element_blank(),
                   legend.title = element_blank(),legend.position = "bottom",
                   axis.title.x = element_text(size = 16, face = "bold"),
                   axis.title.y = element_text(size = 16, face = "bold")))

# Azerbaijan Regions

az <- read.csv("az_density.csv")

az_density <- ggplotly(ggplot(data=az, aes(x=az$ï..Region, y=az$Density,
                  color =az$ï..Region, size=as.numeric(az$Population))) +
              geom_point(alpha=0.8)+ 
              scale_y_log10()+
              scale_size(range = c(1, 12))+
              labs(x="Region", y="Density")+
              theme_ipsum()+
              theme(axis.text.x  = element_blank(),axis.ticks.x  =element_blank(),
                      legend.title = element_blank(),legend.position = "bottom",
                      axis.title.x = element_text(size = 16, face = "bold"),
                      axis.title.y = element_text(size = 16, face = "bold")))
              

############

chess_df <- read.csv(file = "chess.csv",
                     header = TRUE,sep = ",")

chess_df <- select(chess_df,-2)
chess_df.long <- melt(chess_df,id.vars="ï..Country")
chess_df.long

chess_plot <- ggplot(chess_df.long,aes(x=variable,y=value,fill=variable))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Country",breaks=c(1,2,3),
                      labels=c("Armenia", "Azerbaijan","Georgia"))+
  xlab("Title")+ylab("Count")+facet_wrap(~ï..Country)+
  scale_fill_manual(values= c("#868686FF", "#EFC000FF", "#CD534CFF"))+ theme_ipsum()+
  theme(axis.text.x  = element_blank(),axis.ticks.x  =element_blank(),
        legend.title = element_blank())


###########

tour_df <- read.csv(file = "tourism.csv",
                    header = TRUE,sep = ",")

tour_df.long <- melt(tour_df,id.vars="ï..Year")
tour_df.long

tourism_plot <-ggplot(tour_df.long,
                               aes(x=tour_df.long$ï..Year,color=variable))+
  geom_line(aes(y=tour_df.long$value/1000),size = 1.3)+ 
  geom_line(aes(y=tour_df.long$value/1000),size = 1.3) +
  geom_line(aes(y=tour_df.long$value/1000),size = 1.3) + 
    scale_color_manual(name="Country",
                     values =  c("#0073C2FF", "#EFC000FF", "#CD534CFF"),
                     labels=c("Armenia", "Azerbaijan","Georgia")) + 
  theme_ipsum() + labs(x="Year",y="Number of Visitors(x 1000)") +
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold")) 

##########

revenue_df <- read.csv(file = "tourism_revenue.csv",
                    header = TRUE,sep = ",")

revenue_df.long <- melt(revenue_df,id.vars="ï..Year")
revenue_df.long


revenue_plot <-ggplot(revenue_df.long,
                      aes(x=revenue_df.long$ï..Year,color=variable))+
  geom_line(aes(y=revenue_df.long$value/1000000),size = 1.3)+ 
  geom_line(aes(y=revenue_df.long$value/1000000),size = 1.3) +
  geom_line(aes(y=revenue_df.long$value/1000000),size = 1.3) + 
  scale_color_manual(name="Country",
                     values = c("#0073C2FF", "#EFC000FF", "#CD534CFF"),
                     labels=c("Armenia", "Azerbaijan","Georgia")) + 
  theme_ipsum() + labs(x="Year",y="Revenue from Tourism(x 1mln)") +
  theme(axis.title.x = element_text(size = 14, face = "bold"),
        axis.title.y = element_text(size = 14, face = "bold")) 

##########

gdp_df <- read.csv(file = "gdp.csv",
                   header = TRUE,sep = ",")

gdp_df <- select(gdp_df,-1,-4)


gdp_df <- gdp_df %>%
  arrange(desc(gdp_df$Country)) %>%
  mutate(lab.ypos = cumsum(GDP_Nominal) - 0.5*GDP_Nominal)



mycols <- c("#0073C2FF", "#CD534CFF", "#868686FF")


nominal_gdp <- ggplot(gdp_df, aes(x = "", y =gdp_df$GDP_Nominal, fill = gdp_df$Country)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0)+
  geom_text(aes(y = lab.ypos, label = GDP_Nominal), color = "white")+
  scale_fill_manual(values = mycols,name="Country") +
  theme_void() + labs(title = ("Nominal GDP(x 1mln)"))

##########


gdp_df2 <- read.csv(file = "gdp_change.csv",
                   header = TRUE,sep = ",")


gdp_df2.long<-melt(gdp_df2,id.vars="ï..Year")
gdp_df2.long

gdp_df2 <- gdp_df2.long %>% 
  mutate(mycolor = ifelse(gdp_df2.long$value>0, "type2", "type1"))


change_plot <- ggplot(gdp_df2, aes(x=gdp_df2$ï..Year, y=gdp_df2$value)) +
  geom_segment( aes(x=gdp_df2$ï..Year, xend=gdp_df2$ï..Year, y=0, yend=gdp_df2$value, color=mycolor), size=1.3, alpha=0.9) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("Years") +
  ylab("Percentage")+
  facet_wrap(~variable)+theme(strip.background = element_rect(fill="slategray"),
                              strip.text = element_text(size = 18, colour = "white") )


#########

gdp_df1 <- read.csv(file = "gdp.csv",
                   header = TRUE,sep = ",")

gdp_df1 <- select(gdp_df1,-1,-3)

gdp_df1 <- gdp_df1 %>%
  arrange(desc(gdp_df1$Country)) %>%
  mutate(lab.ypos = cumsum(GDP_Per_Capita) - 0.5*GDP_Per_Capita)

mycols1 <- c("#868686FF", "#EFC000FF", "#CD534CFF")


pc_plot <- ggplot(gdp_df1, aes(x = 2, y = gdp_df1$GDP_Per_Capita, fill = gdp_df1$Country)) +
  geom_bar(stat = "identity", color = "white") +
  coord_polar(theta = "y", start = 0)+
  geom_text(aes(y = lab.ypos, label = GDP_Per_Capita), color = "white")+
  scale_fill_manual(values = mycols1,name="Country") +
  theme_void()+
  xlim(0.5, 2.5)

#########

crime <- read.csv(file = "crime.csv",
                  header = TRUE,sep = ",")


crime.long<-melt(crime,id.vars ="ï..Year" )

am <- data.frame(
  x=crime.long$ï..Year[1:7], 
  amvalue1=crime.long$value[1:7], 
  amvalue2=crime.long$value[8:14] 
) %>%
  rowwise() %>% 
  mutate( mymean = mean(c(amvalue1,amvalue2) )) %>% 
  arrange(mymean) %>% 
  mutate(x=factor(x, x))

az <- data.frame(
  x=crime.long$ï..Year[15:21], 
  azvalue1=crime.long$value[15:21],
  azvalue2=crime.long$value[22:28]) %>%
  rowwise() %>% 
  mutate( mymean = mean(c(azvalue1,azvalue2) )) %>% 
  arrange(mymean) %>% 
  mutate(x=factor(x, x))

ge <- data.frame(
  x=crime.long$ï..Year[29:35], 
  gevalue1=crime.long$value[29:35], 
  gevalue2=crime.long$value[36:42] 
) %>%
  rowwise() %>% 
  mutate( mymean = mean(c(gevalue1,gevalue2) )) %>% 
  arrange(mymean) %>% 
  mutate(x=factor(x, x))


crime_plot <- ggplot(data = fortify(am,az,ge)) +
  geom_segment(data = am, aes(x=x, xend=x, y=amvalue1, yend=amvalue2), color="grey") +
  geom_point( aes(x=x, y=amvalue1), color=rgb(227/255,74/255,51/255), size=5,shape=15  ) +
  geom_point( aes(x=x, y=amvalue2), color=rgb(227/255,74/255,51/255), size=5, shape=16) +
  geom_segment(data = az, aes(x=x, xend=x, y= azvalue1, yend= azvalue2), color="grey") +
  geom_point( data=az,aes(x=x, y=azvalue1), color=rgb(161/255,217/255,155/255), size=5,shape=15 ) +
  geom_point(data=az, aes(x=x, y=azvalue2), color=rgb(161/255,217/255,155/255), size=5, shape=16) +
  geom_segment(data = ge, aes(x=x, xend=x, y=gevalue1, yend=gevalue2), color="grey") +
  geom_point(data=ge, aes(x=x, y=gevalue1), color=rgb(252/255,146/255,114/255), size=5, shape=15) +
  geom_point(data=ge, aes(x=x, y=gevalue2), color=rgb(252/255,146/255,114/255), size=5, shape=16) +
  coord_flip()+
  theme_ipsum() +
  theme(panel.border = element_blank()) + xlab("") +  ylab("") +  labs(title = "Crime index vs Safety index")

########


medal <- read.csv(file = "olympic_medals.csv",
                  header = TRUE,sep = ",")

medal.long<-melt(medal,id.vars ="ï..Country")

medal_plot <- ggplot(medal.long,aes(x=medal.long$ï..Country,y=medal.long$value,
                      col=medal.long$variable)) + geom_jitter(size=5)+
  scale_color_manual(name="Type",
                     values =  c("#0073C2FF", "#EFC000FF", "#CD534CFF"),
                     labels=c("Silver","Gold","Bronze")) + 
  theme_ipsum() + labs(x="",y="Number of Medals") +
  theme(axis.title.y = element_text(size = 14, face = "bold"),
        text = element_text(size=14),
        axis.text.x = element_blank())+
  facet_wrap(~medal.long$ï..Country)

##################

military <-read.csv(file = "military.csv",
               header = TRUE,sep = ",")

military_plot <- ggplot(military,aes(x=ï..country,y=military_budget/1000000,fill=ï..country))+
  geom_bar(stat="identity",position="dodge")+
  scale_fill_discrete(name="Country",breaks=c(1,2,3),
                      labels=c("Armenia", "Azerbaijan","Georgia"))+
  xlab("Countries")+ylab("Amount")+
  scale_fill_manual(values=c("#868686FF", "#EFC000FF", "#CD534CFF"))+
  theme_ipsum()+
  theme(axis.text.x  = element_blank(),axis.ticks.x  =element_blank(),
        legend.title = element_blank(),legend.position = "bottom")

#########

urban <-read.csv(file = "pop.csv",
               header = TRUE,sep = ",")

urban.long<-melt(urban,id.vars ="ï..Year")


urban_plot <-ggplot(urban.long,
                      aes(x=urban.long$ï..Year,fill=variable))+
  geom_area(aes(y=urban.long$value/1000),size = 1.3,alpha=0.6)+ 
  geom_area(aes(y=urban.long$value/1000),size = 1.3,alpha=0.6) +
  geom_area(aes(y=urban.long$value/1000),size = 1.3,alpha=0.6) + 
  scale_fill_manual(name="Country",
                     values = alpha(c("#868686FF", "#EFC000FF", "#CD534CFF")),
                     labels=c("Azerbaijan","Georgia","Armenia")) + 
  theme_classic() + labs(x="Year",y="Urban Population(x 1000)") +
  theme(axis.title.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold")) 

#######
df_migration <- read.csv(file = "migration.csv",
                         header = TRUE,sep = ",")

df_migration<- df_migration %>% 
  mutate(mycolor = ifelse(df_migration$ï..Count>0, "type2", "type1"))


migration_plot <-ggplot(df_migration, aes(x=df_migration$Year, y=df_migration$ï..Count)) +
  geom_segment( aes(x=df_migration$Year, xend=df_migration$Year, y=0, yend=df_migration$ï..Count, color=mycolor), size=1.3, alpha=0.9) +
  theme_light() +
  theme(
    legend.position = "none",
    panel.border = element_blank(),
  ) +
  xlab("Years") +
  ylab("Percentage")+
  theme(strip.background = element_rect(fill="slategray"))+facet_wrap(~Country)

########

population <-read.csv(file = "population.csv",
                 header = TRUE,sep = ",")


population_plot <-ggplot(population,
                    aes(x=population$ï..Year,y=population$Population/1000,fill=Country))+
  geom_bar(stat="identity",position="dodge",width = 2)+ 
  scale_fill_manual(name="Country",
                    values = alpha(c("#868686FF", "#EFC000FF", "#CD534CFF")),
                    labels=c("Armenia","Azerbaijan","Georgia")) + 
  theme_ipsum() + labs(x="Year",y="Population(x 1000)") +
  theme(axis.title.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold")) 


