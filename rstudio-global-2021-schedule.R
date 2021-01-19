# loading libraries --
library(tidyverse)
library(calendar)
library(lubridate)
library(glue)



schedule <- read_csv(paste0(getwd(), "/schedule-from-organizers-fixed-lg.csv"))

schedule



# Lucky cos I'm GMT!
clean_schedule <- schedule %>%
  mutate(event_name = case_when(
    is.na(name) ~  glue("{title_text}"),
    TRUE ~ glue("{title_text} by {name}")
    )
  ) %>%
  select(event_name, time_gmt, duration) %>% #, url, abstract_text
  mutate(
    start_date_time = ymd_hms(time_gmt),
    end_date_time = start_date_time + duration
  ) %>%
  select(-time_gmt, -duration)

clean_schedule


make_calendar <- function(event) {
  event_subset <- clean_schedule[event, ]

  calendar_event <-
  calendar::ic_event(start_time = event_subset$start_date_time,
                     end_time = event_subset$end_date_time,
                     summary = event_subset$event_name)

  return(calendar_event)
}

number_events <- length(clean_schedule$event_name)

# creating ics objects for all events --
events_all <- map(1:number_events, make_calendar) %>%
  bind_rows()

# writing to .ics file --
# calendar::ic_write(events_all, "lg_events_localtime.ics")

