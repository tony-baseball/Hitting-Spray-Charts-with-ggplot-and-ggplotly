library(ggplot2)
library(plotly)
library(plyr)
library(tidyverse)
library(stringr)
library(grDevices)
library(GeomMLBStadiums)
library(MASS)

# We're going to use ggplot and ggplotly to create these spray charts. ggplotly allows us to turn ggplot graphics into interactive graphics. We can zoom, filter, select, hover, and more.  If you do not want to use plotly, that code is also provided

df <- read.csv('sample.csv')

# this small chunk creates a polygon that we will later use to shade LCF and RCF
{
l_ang <- c(-163.43658,  -58.50649,    0.00000)
l_dist <- c(309, 373, -11)
lcf<- data.frame(l_ang, l_dist) %>%
  slice(chull(l_ang, l_dist))%>%
  rename(hc_x = 1,
         hc_y = 2)
r_ang <- c(164.79855, 130.65169, 114.02496, 102.49234,  76.51441,  63.19952,   0.00000)
r_dist <- c(323.4354, 358.9626, 372.9589, 382.5066, 393.6325, 399.0261, -11.0000)
rcf<- data.frame(r_ang, r_dist) %>% 
  slice(chull(r_ang, r_dist)) %>%
  rename(hc_x = 1,
         hc_y = 2)
}


# make a condensed dataframe keeping relevant columns 
batted_ball <- df %>% filter( PitchCall == 'InPlay', !HitType %in% c('Bunt',''), !is.na(HitType)) %>%
      dplyr::select(Date, HomeTeam,AwayTeam, `Top.Bottom`, PitchNo, Batter,ExitSpeed,TaggedPitchType, PitchCall ,PlayResult, HitType, Angle,
                    HitSpinRate, Direction, PlateLocSide,  Distance, Bearing, pfxx, pfxz  )%>%
  # Yakkertech data provides Bearing, Distance, and Angle. To get the hit coordinates (landing point) of the batted ball, we create new columns called hc_x, and hc_y 
  mutate(hc_x = sin(Bearing * pi/180)*Distance ,
         hc_y = cos(Bearing * pi/180)*Distance ,
         # recode the play results for a cleaner look
         PlayResult = recode(PlayResult, Double = '2B', HomeRun = 'HR', Single = '1B', Triple = '3B', Sacrifice = 'SAC', Error = 'E',
                             FieldersChoice = 'FC'),
         # recode the play hit types for a cleaner look
         HitType = recode(HitType, FlyBall = 'FB', LineDrive = 'LD', GroundBall = 'GB', PopUp = 'PU'),
         Matchup = ifelse(`Top.Bottom` == 'Bottom', 
                              paste(HomeTeam, 'vs.', AwayTeam), 
                              paste(AwayTeam, 'at', HomeTeam) )  )

#'m' creates custom margins for the plotly output
  m <- list(
    l = 0,
    r = 0,
    b = 0,
    t = 50,
    pad = 4
  )
  # key creates the data you want displayed when you hover over the point
  key<-paste0(batted_ball$HitType ," - ",batted_ball$PlayResult ," \n", 
              round(batted_ball$ExitSpeed,1), " mph,\n ",
              round(batted_ball$Angle,0), "°,\n ", 
              round(batted_ball$Distance,1), " ft.\n",
              batted_ball$Date, "\n",
              "Pitch #: ", batted_ball$PitchNo )
  
### GGPLOTLY SPRAY CHART ----
  ggplotly(
    ggplot(batted_ball , aes(x = hc_x, y = hc_y ) )+ 
      # 
      geom_mlb_stadium(stadium_ids = 'cubs',
                       stadium_transform_coords = TRUE, 
                       stadium_segments = c('outfield_outer', 'home_plate', 'foul_lines', 'infield_outer', 'infield_inner') , 
                       size =.5, 
                       color = 'black') + 
      theme_void() + 
      geom_polygon(data = lcf, alpha = .2) +
      geom_polygon(data = rcf, alpha = .2) +
      geom_point(aes(color = HitType, text=key), size = 3)+ 
      # WALL DIMENSIONS
      annotate("text", 20, 380, label = "400",size = 3, color = 'black', fontface = 2) +
      annotate("text", -220, 250, label = "355", size = 3, color = 'black', fontface = 2) +
      annotate("text", 220, 250, label = "353", size = 3, color = 'black', fontface = 2) +
      annotate("text", -100, 325, label = "368", size = 3, color = 'black', fontface = 2) +
      annotate("text", 130, 325, label = "368", size = 3, color = 'black', fontface = 2) +
      annotate("text", 0, 450, label = "*Distances are only estimates", size = 3, fontface = 2) +
      # SPRAY %
      annotate("text", 20, 380, label = "400",size = 3, color = 'black', fontface = 2) +
      annotate("text", -220, 250, label = "355", size = 3, color = 'black', fontface = 2) +
      annotate("text", 220, 250, label = "353", size = 3, color = 'black', fontface = 2) +
      annotate("text", -100, 325, label = "368", size = 3, color = 'black', fontface = 2) +
      annotate("text", 130, 325, label = "368", size = 3, color = 'black', fontface = 2) +
      coord_fixed()  +
      labs(title = paste("Spray Chart") ) +
      #  scale_color_gradient2(midpoint=90, low="lightblue", mid="grey",high="red")+
      theme(legend.position = "none",
            panel.background = element_rect(fill = 'white') ) ) %>%
    layout(legend = list(orientation = "h",   # show entries horizontally
                         xanchor = "center",  # use center of legend as anchor
                         x = 0.5,
                         y = 0),
           autosize = T,
           margin = m,
           plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)",
           fig_bgcolor   = "rgba(0, 0, 0, 0)",
           showlegend = TRUE,
           xaxis = list(autorange = TRUE, showgrid = FALSE, visible = FALSE, range=c(-400,400)),
           yaxis = list(autorange = TRUE, showgrid = FALSE, visible = FALSE))%>%
    layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))

# GGPLOT SPRAY CHART ----
  ggplot(batted_ball , aes(x = hc_x, y = hc_y ) )+ 
   geom_mlb_stadium(stadium_ids = 'cubs',
                     stadium_transform_coords = TRUE, 
                     stadium_segments = c('outfield_outer', 'home_plate', 'foul_lines', 'infield_outer', 'infield_inner') , 
                     size =.5, 
                     color = 'black') + 
    theme_void() + 
    geom_polygon(data = lcf, alpha = .2) +
    geom_polygon(data = rcf, alpha = .2) +
    geom_point(aes(colour = HitType), size = 3)+ 
    # WALL DIMENSIONS
    annotate("text", 20, 380, label = "400",size = 3, color = 'black', fontface = 2) +
    annotate("text", -220, 250, label = "355", size = 3, color = 'black', fontface = 2) +
    annotate("text", 220, 250, label = "353", size = 3, color = 'black', fontface = 2) +
    annotate("text", -100, 325, label = "368", size = 3, color = 'black', fontface = 2) +
    annotate("text", 130, 325, label = "368", size = 3, color = 'black', fontface = 2) +
    annotate("text", 0, 450, label = "*Distances are only estimates", size = 3, fontface = 2) +
    # if you use ExitSpeed as the color, you can manually color using the code below
    #  scale_color_gradient2(midpoint=90, low="lightblue", mid="grey",high="red")+
    theme(panel.background = element_rect(fill = 'white') ) +
    coord_fixed()  +
    labs(title = paste("Spray Chart"),
      subtitle = "BatterName",
      colour = 'HitType')
  
### GGPLOTLY SPRAY DENSITY CHART----
  
  ggplotly(
    ggplot(batted_ball, aes(x = hc_x, y = hc_y ))+ 
      # 
      geom_mlb_stadium(stadium_ids = 'cubs',
                       stadium_transform_coords = TRUE, 
                       stadium_segments = c('outfield_outer', 'home_plate', 'foul_lines', 'infield_outer', 'infield_inner') , 
                       size =.5, 
                       color = 'black') + 
      theme_void() + 
      geom_polygon(data = lcf, alpha = .07) +
      geom_polygon(data = rcf, alpha = .07) +
      stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
      # facet_wrap(vars(BatterSide)) +
      scale_fill_distiller(palette = "Paired", direction = 1) +
      # geom_point(color = boomers$ExitSpeed, text='key', size = 3)+ 
      # WALL DIMENSIONS
      annotate("text", 20, 380, label = "400",size = 3, color = 'black', fontface = 2) +
      annotate("text", -220, 250, label = "355", size = 3, color = 'black', fontface = 2) +
      annotate("text", 220, 250, label = "353", size = 3, color = 'black', fontface = 2) +
      annotate("text", -100, 325, label = "368", size = 3, color = 'black', fontface = 2) +
      annotate("text", 130, 325, label = "368", size = 3, color = 'black', fontface = 2) +
      #  annotate("text", 0, 700, label = "*Distances are only estimates", size = 3, fontface = 2) +
      annotate("", 0, 450, label = "*Distances are only estimates", size = 3, fontface = 2) +
      
      # SPRAY %
      annotate("text", 20, 380, label = "400",size = 3, color = 'black', fontface = 2) +
      annotate("text", -220, 250, label = "355", size = 3, color = 'black', fontface = 2) +
      annotate("text", 220, 250, label = "353", size = 3, color = 'black', fontface = 2) +
      annotate("text", -100, 325, label = "368", size = 3, color = 'black', fontface = 2) +
      annotate("text", 130, 325, label = "368", size = 3, color = 'black', fontface = 2) +
      coord_fixed()  +
      
      #  scale_color_gradient2(midpoint=90, low="lightblue", mid="grey",high="red")+
      theme(legend.position = "none",
            panel.background = element_rect(fill = 'white') ) ) %>%
    layout(legend = list(orientation = "h",   # show entries horizontally
                         xanchor = "center",  # use center of legend as anchor
                         x = 0.5,
                         y = 15),
           autosize = T,
           margin = m,
           plot_bgcolor  = "rgba(0, 0, 0, 0)",
           paper_bgcolor = "rgba(0, 0, 0, 0)",
           fig_bgcolor   = "rgba(0, 0, 0, 0)",
           showlegend = TRUE,
           #  key = key,
           # tooltip="ExitSpeed",
           # plot_bgcolor='#f47b20',
           xaxis = list(autorange = TRUE, showgrid = FALSE, visible = FALSE, range=c(-400,400)),
           yaxis = list(autorange = TRUE, showgrid = FALSE, visible = FALSE))%>%
    layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))

# GGPLOT SPRAY DESNITY CHART ----
  ggplot(batted_ball, aes(x = hc_x, y = hc_y )) +
    theme_void() + 
    geom_polygon(data = lcf, alpha = .07) +
    geom_polygon(data = rcf, alpha = .07) +
    stat_density_2d(aes(fill = ..density..), geom = "raster", contour = FALSE) +
    # facet_wrap(vars(BatterSide)) +
    scale_fill_distiller(palette = "Paired", direction = 1) +
    # geom_point(color = boomers$ExitSpeed, text='key', size = 3)+ 
    # WALL DIMENSIONS
    # annotate("text", 20, 380, label = "400",size = 3, color = 'black', fontface = 2) +
    # annotate("text", -220, 250, label = "355", size = 3, color = 'black', fontface = 2) +
    # annotate("text", 220, 250, label = "353", size = 3, color = 'black', fontface = 2) +
    # annotate("text", -100, 325, label = "368", size = 3, color = 'black', fontface = 2) +
    # annotate("text", 130, 325, label = "368", size = 3, color = 'black', fontface = 2) +
    # annotate("text", 0, 700, label = "*Distances are only estimates", size = 3, fontface = 2) +
    # annotate("", 0, 450, label = "*Distances are only estimates", size = 3, fontface = 2) +
    # annotate("text", 130, 325, label = "368", size = 3, color = 'black', fontface = 2) +
    coord_fixed()  +
    
    #  scale_color_gradient2(midpoint=90, low="lightblue", mid="grey",high="red")+
    theme(legend.position = "none") +
    geom_mlb_stadium(stadium_ids = 'cubs',
                     stadium_transform_coords = TRUE, 
                     stadium_segments = c('outfield_outer', 'home_plate', 'foul_lines', 'infield_outer', 'infield_inner') , 
                     size =.5, 
                     color = 'black')  + 
    labs(title = paste("Spray Chart"),
         subtitle = "BatterName",
         colour = 'HitType')
