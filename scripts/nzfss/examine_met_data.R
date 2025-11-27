# look at wind direction during the 2024 bloom
library(openair)
library(magick)
library(av)
library(tidyverse)

met <- read.csv('./data/buoy/rotorua_meteorology_latest.csv')

met <- met %>% 
  filter(DateTime > as.POSIXct('2024-11-18') & DateTime < as.POSIXct('2025-01-13'))

windRose(met, ws = 'WndSpd', wd = 'WndDir',
         main = 'Wind direction from Nov 18 2024 to Jan 13 2025')

dates <-  c("2024-11-25", "2024-12-02", "2024-12-09", "2024-12-16", "2024-12-23",
            "2025-01-06", "2025-01-13", "2025-01-20", "2025-01-27", "2025-02-03",
            "2025-02-10") 
dates <- as.Date(dates)

for(i in 1:length(dates)){
  
  # subset to the seven days before this date
  met_sub <- met %>% 
    filter(as.Date(DateTime) >= (dates[i] - 7) & as.Date(DateTime) <= dates[i])
  
  # Save PNG for this frame
  png_filename <- sprintf(paste0("./figures/nzfss/windrose_frames/windrose_",dates[i], ".png"))
  png(png_filename, width = 800, height = 800)
  
  print(
    windRose(met_sub, ws = "WndSpd", wd = "WndDir",
             main = paste0("Wind direction on ", dates[i]),
             paddle = FALSE)
  )
  
  dev.off()

  
}

# Combine into an animated GIF
frames <- list.files("./figures/nzfss/windrose_frames", full.names = TRUE, pattern = "*.png")
frames <- frames[order(frames)]  # Ensure correct order
img_list <- image_read(frames)
img_gif <- image_animate(image_join(img_list), fps = 1)  # adjust fps for speed
image_write(img_gif, "./figures/nzfss/windrose_animation.gif")
image_write_video(img_list, path = "./figures/nzfss/windrose_animation.mp4",
                  framerate = 1)


# analytically look for a signal?
out2 <- out %>% 
  filter(as.Date(date)<=dates[i]) %>% 
  group_by(date, Site) %>% 
  mutate(diff_btw_buoy_shore = observed_bv - pred_bv_native)

met2 <- NULL
for(i in 1:length(dates)){
  
  # subset to the seven days before this date
  met_sub <- met %>% 
    filter(as.Date(DateTime) >= (dates[i] - 7) & as.Date(DateTime) <= dates[i]) %>% 
    summarise(avg_wndspd = mean(WndSpd, na.rm = TRUE),
              avg_wnddir = mean(WndDir, na.rm = TRUE)) %>% 
    mutate(date = dates[i]) %>% 
    select(date, everything())
  
  met2 <- rbind(met2, met_sub)
  
  
}


######



met_long <- met %>% 
  pivot_longer(RadSWD:RadClr, names_to = 'variable', values_to = 'value') %>% 
  filter(!is.na(value))

ggplot(met_long, aes(x = as.POSIXct(DateTime), y = value)) +
  geom_point() +
  facet_wrap(~variable, scales = 'free')


