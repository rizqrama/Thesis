# 0. clear environment and console ----------------------------------------
# Clear environment
rm(list = ls()) 

# Clear console
cat("\014")  # ctrl+L

# 1. Installing and Loading Packages --------------------------------------------------
pacman::p_load(pacman, here, readr, dplyr, stringr, tidyr, lubridate, tidyverse, fpp3, fs, purrr, data.table)


# 2. Load Detector Data

.fn <- "D:/2. My Research/Disaster Related MFD/police_data/Data/DET_16039"
.from <- lubridate::dmy("01/07/2016")
.to <- lubridate::dmy("31/07/2019")
load_dates <- function(fn, from, to) {
  `%|%` <- rlang::`%|%`
  if (!inherits(c(from,to), c("Date", "POSIXct", "POSIXlt"))) {
    .dates <- suppressWarnings(purrr::map(list(from, to), ~{
      lubridate::mdy(.x) %|% lubridate::ymd(.x) %|% lubridate::dmy(.x)
    }))
  } else {
    .dates <- c(from, to)
  }
  message(paste0("Loading From: ", .dates[[1]], " To: ", .dates[[2]]))
  .folders <- purrr::map_chr(setNames(seq(from, to, by = "month"), seq(from, to, by = "month")), ~{
    .m <- lubridate::month(.x)
    .fn <- paste0(substr(lubridate::year(.x),3,4), ifelse(.m < 10, paste0("0",.m), as.character(.m)))
  })
  browser()
  .folders <- .folders[.folders %in% list.files(fn)]
  .out <- dplyr::bind_rows(purrr::imap(.folders, ~{
    .fns <- list.files(fs::path(fn,.x), recursive = TRUE, full.names = TRUE)
    .csvs <- purrr::map(setNames(.fns, basename(.fns)), data.table::fread)
    .out <- dplyr::bind_rows(.csvs, .id = "filename")
  }), .id = "Month")
  return(.out)
}
test4 <- load_dates(.fn, .from, .to)



# 3. Data cleansing ----

traffic_data <- test4 %>% 
  mutate(time_date = as_datetime(V1),
         flow = V4,
         occ = V5/100) %>% # mutating columns
  select(time_date, flow, occ) %>% # select the necessary variables
  as_tsibble(index = time_date) %>% # make it a time series object
  fill_gaps(.full = FALSE) %>% # fill the missing time
  as.data.frame() # convert back to data frame

traffic_data %>% filter(flow > 4096 | occ > 100) # find the outliers

# data cleansing
traffic_data  <-  traffic_data %>% 
  mutate(flow = as.double(flow),
         occ = as.double(occ)) %>% 
  mutate(flow = case_when(flow > 4096 ~ NA_real_,
                          TRUE ~ flow),
         occ = case_when(occ > 100 ~ NA_real_,
                         TRUE ~ occ)
         ) %>% 
  fill(flow, .direction = "down") %>% 
  fill(occ, .direction = "down")

# adding year, month, day, and hour columns
traffic_data <- traffic_data %>% 
  mutate(year = year(time_date),
         month = month(time_date),
         day = day(time_date),
         hour = hour(time_date),
         minute = minute(time_date))



# hourly flow
flow_hr <- traffic_data %>% 
  group_by(year, month, day) %>% 
  count(hour, wt = flow, name = "flow")


# hourly occupancy
# occ_hr <- twoyearsdata %>% 
#   group_by(year, month, day) %>% 
#   count(hour, wt = occ)

# 4. Making Time Series tibble --------------------------



# 4. making plot ----

fl_plot <- flow_hr %>% 
  ggplot(aes(x = day, y = hour, fill = flow)) + # basic
  geom_tile(color = "white", size = 0.05) + # adding border for the tiles
  scale_fill_viridis(option = "C", name = "Hourly Flow (veh/hr)") + # adding color palette
  facet_grid(year~month) +
  scale_y_continuous(trans = "reverse", breaks = unique(traffic_data$hour)) +
  scale_x_continuous(breaks =c(1,10,20,31)) +
  labs(title= "Traffic Flow: City of Kure, Hiroshima", x="Day", y="Hour", subtitle = "Detector 16039. July 2016 - July 2019") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title=element_text(size = 22, hjust = 0, face = "bold"),
        plot.subtitle = element_text(size = 12, hjust = 0, face = "italic"),
        axis.text.y=element_text(size=8),
        strip.background = element_rect(colour="white"),
        axis.ticks=element_blank(),
        axis.text=element_text(size=8),
        legend.title=element_text(size=10),
        legend.text=element_text(size=8)) +
  removeGrid()

ggsave("flow_det_16039.png", dpi = 400, width = 16, height = 9)
