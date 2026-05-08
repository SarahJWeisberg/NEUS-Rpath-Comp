## 4.1.1 Simplified map for talk

bts<-NEFSCspatial::BTS_Strata

MAB_strata <- c(1010:1080, 1100:1120, 1600:1750, 3010:3450, 3470, 3500, 3510)
GB_strata  <- c(1090, 1130:1210, 1230, 1250, 3460, 3480, 3490, 3520:3550)
GOM_strata <- c(1220, 1240, 1260:1290, 1360:1400, 3560:3830)

bts_area<- bts %>%
  mutate(EPU = ifelse(STRATA %in% MAB_strata,"MAB",ifelse(STRATA %in% GB_strata,"GB",ifelse(STRATA %in% GOM_strata,"GOM","other"))))

theme_set(theme_bw())

#load in countries for plotting
world <- ne_countries(scale = "large", returnclass = "sf")

#generate region outline polygons
Region <- bts_area %>% filter(!EPU == "other") %>%
  st_make_valid() %>% st_buffer(0) %>% group_by(EPU) %>% summarize()
Region <- ms_filter_islands(Region, min_area=100000000000000) #get rid of that weird one in the gulf of maine
#Region <- ms_simplify(Region,keep = 0.003, weighting = 11)

# call in bathymetric data
# convert  bathymetric contours into dataframe
bathy <- getNOAA.bathy(-80, -64.5, 34, 46, resolution=1); bathydf <- as.xyz(bathy)
#ignore positive depth (land)
bathy_sea <- bathydf; bathy_sea$V3[bathy_sea$V3 > 1] <- 0
#ignore depths greater than 500m
#bathy_shelf<-bathy_sea %>% filter(V3 > -500)

usa <- st_as_sf(maps::map("state", fill=TRUE, plot =FALSE))
#plot
NEUS <- ggplot() +
  geom_sf()+
  geom_raster(data = bathy_sea, aes(x = V1, y = V2, fill = V3)) +
  scale_fill_gradient(low="#586575", high="#daedf7",
                      breaks = c(-5000,-3000,-1000),labels=c("5k","3k","1k"))+
  #geom_sf(data = world)+
  geom_sf(data = usa)+
 #geom_sf(data = Region, fill = NA, linewidth = .75)+
  #scale_color_manual(values = c("#FF7F00","#377EB8","#E41A1C"))+
  #geom_sf(data=eez_canada)+
  coord_sf(xlim =c(-78, -65.5), ylim = c(35, 45)) + #zoomed to Hatteras and N
  labs(x = NULL, y = NULL,fill= "Depth (m)") +
  guides(color="none",fill=guide_colorbar(reverse = T))+
  annotate("text",x=-74,y=39,label="Mid-Atlantic Bight",size=6,angle=50)+
  annotate("text", x = -69, y = 43, label = "Gulf of Maine", size = 6)+
  annotate("text", x = -68.75, y = 40.75, label = "Georges Bank", size = 6,angle=10)+
  #annotation_custom(tableGrob(summary_stats),xmin=-69, xmax=-65.5, ymin=36.25, ymax=38)+
  #ggspatial::annotation_scale(location = "br",bar_cols = c("grey60", "white")) +
  theme(plot.margin = margin(0, 0, 0, 0, "cm"),
        panel.grid.major = element_blank(),
        #panel.border = element_blank(),
        legend.position = "bottom",
        #legend.position.inside = c(0.85,0.1),
        legend.title = element_text(face="italic",size=18),
        legend.direction = "horizontal",
        legend.text = element_text(size=12, color="black"),
        axis.text=element_text(size=12, color="black"))

# NEUS
