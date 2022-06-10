# Notes -------------------------------------------------
# Unique User: defined as a pairing of the Principal Invesitagor (PI) and User
# Unique users are not Singh Staff, GSFs or belong to related projects therein
# Tools: Have full names and are not designated with the ID NF-0x

# 2019-08-14
# Leading and trailing whitespaces must be removed upon 
# import; this fixes issues with tool IDs.
# QNF entries with QNFSL tool charges has been fixed.
# Legend must be enabled to use color scales

# 2020-03-13
# Created path definitions for better code management 

library(tidyverse)
library(ggplot2)
library(ggpubr)
library(ggrepel)
library(lubridate)
library(leaflet)
library(scales)
library(markdown)
library(leaflet)
library(DT)

Sys.setenv(TZ='EST')

# Palette Definitions  -----------------------------------------

# Loading the Penn Branding Color Scheme
s <- read_delim("_palettes/penn-theme-colors", delim=":", trim_ws = TRUE) %>% 
  distinct() %>%
  select(-Colors) 

# Loading the Penn Branding Color Scheme
s7 <- read_delim("_palettes/penn-theme-colors-7", delim=":", trim_ws = TRUE) %>% 
  distinct() %>%
  select(-Colors) 


# Loading the Penn Branding Color Scheme
sd <- read_delim("_palettes/penn-theme-colors-spectral-dark", delim=":", trim_ws = TRUE) %>% 
  distinct() %>%
  select(-Colors) 

# Loading the Penn Branding Color Scheme
sl <- read_delim("_palettes/penn-theme-colors-spectral", delim=":", trim_ws = TRUE) %>% 
  distinct() %>%
  select(-Colors) 


# Theme Definitions -----------------------------------------
gtheme = theme_light() + theme(
  title = element_text(size = 20),
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 14),
  strip.background =element_rect(fill="#011f5b"),
  strip.text = element_text(size = 12)) 

gtheme_void = theme_void() + theme(
  title = element_text(size = 20),
  axis.title.x = element_text(size = 16),
  axis.text.x = element_text(size = 14),
  axis.title.y = element_text(size = 16),
  axis.text.y = element_text(size = 14),
  strip.background =element_rect(fill="#011f5b"),
  strip.text = element_text(size = 12)) 


# Level Definitions -----------------------------------------

monthLevel <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")
monthLevel2 <- c("July", "August", "September", "October", "November", "December","January", "February", "March", "April", "May", "June")
monthLevel3 <- c("October", "November", "December","January", "February", "March", "April", "May", "June", "July", "August", "September")

dayLevel <- c("Sunday","Monday","Tuesday","Wednesday","Thursday","Friday","Saturday")
dayLevel2 <- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")

affliationLevel <- c("Foreign", "State and Federal Government", "Large Company (> 500)",  "Small Company (< 500)", "4 Year College", "Other University", "Educational Lab Use",  "Local Site Academic")
affliationLevel2 <- c("Foreign", "State and Federal Gov", "Large Company",  "Small Company", "4 Year College", "2 Year College", "Other University", "Pre-College", "Local Site Academic")



# Function Defintions --------------------------------------------------------------------
calcYear <- function(Date){
  ifelse(month(Date) <= 6, year(Date), year(Date)+1)
}

isWeekend <- function(day){
  ifelse(grepl("Sunday|Saturday",day),"Saturday and Sunday","Monday-Friday")
}

lookup <- function(phrases, teststr) {
  grepl(paste(phrases, collapse = "|"), teststr)
}

