setwd("C:/Users/Joseph/Desktop/University_18_19/Classes/STAT133/workout01/code")
library(jpeg)
library(grid)
library(gridExtra)
library(dplyr)
library(ggplot2)

# Shot Charts 

# Download images and save as a Raster Object 
court_file <- "../images/nba-court.jpg"
court_image <- rasterGrob(
  readJPEG(court_file),
  width=unit(1,"npc"),
  height = unit(1,"npc")
)

# EXAMPLE chart:
klay_shot_chart <- ggplot(data = shots) + 
  annotation_custom(court_image,-250,250,-50,420) +
  geom_point(aes(x=x,y=y,color=shot_made_flag)) +
  ylim(-50,420)+
  theme_minimal()
klay_shot_chart


# ----------------
# Make shot charts 
#   Thompson
thompson_shotchart <- ggplot(data = thompson) + 
  annotation_custom(court_image,-250,250,-50,420) +
  geom_point(aes(x=x,y=y,color=shot_made_flag)) +
  ylim(-50,420)+
  ggtitle("Shot Chart: Klay Thompson (2016 Season)")+
  theme_minimal()

curry_shotchart <- ggplot(data = curry) + 
  annotation_custom(court_image,-250,250,-50,420) +
  geom_point(aes(x=x,y=y,color=shot_made_flag)) +
  ylim(-50,420)+
  ggtitle("Shot Chart: Steph Curry (2016 Season)")+
  theme_minimal()

durant_shotchart <- ggplot(data = durant) + 
  annotation_custom(court_image,-250,250,-50,420) +
  geom_point(aes(x=x,y=y,color=shot_made_flag)) +
  ylim(-50,420)+
  ggtitle("Shot Chart: Kevin Durant (2016 Season)")+
  theme_minimal()

green_shotchart <- ggplot(data = green) + 
  annotation_custom(court_image,-250,250,-50,420) +
  geom_point(aes(x=x,y=y,color=shot_made_flag)) +
  ylim(-50,420)+
  ggtitle("Shot Chart: Draymond Green (2016 Season)")+
  theme_minimal()

iguodala_shotchart <- ggplot(data = iguodala) + 
  annotation_custom(court_image,-250,250,-50,420) +
  geom_point(aes(x=x,y=y,color=shot_made_flag)) +
  ylim(-50,420)+
  ggtitle("Shot Chart: Andre Iguodala (2016 Season)")+
  theme_minimal()

# Save as pdfs, width = 6.5 and height = 5 in folder images 
pdf(file = "../images/klay-thompson-shot-chart.pdf",width = 6.5,height = 5)
thompson_shotchart
dev.off()

pdf(file = "../images/steph-curry-shot-chart.pdf",width = 6.5,height = 5)
curry_shotchart
dev.off()

pdf(file = "../images/kevin-durant-shot-chart.pdf",width = 6.5,height = 5)
durant_shotchart
dev.off()

pdf(file = "../images/draymond-green-shot-chart.pdf",width = 6.5,height = 5)
green_shotchart
dev.off()

pdf(file = "../images/andre-iguodala-shot-chart.pdf",width = 6.5,height = 5)
iguodala_shotchart
dev.off()

# Faceted Shot Chart 
pdf(file = "../images/gsw-shot-charts.pdf",width = 8,height = 7)
gsw_shotcharts <- grid.arrange(thompson_shotchart,
                               curry_shotchart,
                               durant_shotchart,
                               green_shotchart,
                               iguodala_shotchart,
                               nrow=2,
                               ncol=3, 
                               top=textGrob("Shot Charts: GSW 2016 Season",
                                            gp=gpar(fontsize=20)))
# save it 
dev.off()

#As PNG
ggsave("gsw-shot-charts.png", plot=gsw_shotcharts,
       path='../images', width = 8,height = 7,units = "in",scale = 3)
ggsave("gsw-shot-charts2.png", plot=gsw_shotcharts,
       path='../images', width = 10.5,height = 7,units = "in",scale = 1.9)


