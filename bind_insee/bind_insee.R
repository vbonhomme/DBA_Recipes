#
# What: This scripts binds a data.frame with
# city names (case 1) or insee code (case 2)
# to the insee reference database. This helps ensures codes are right,
# and if so, adds various administrative and geopgraphical informations
#
# Please read the comments!!!
# Also change what needs to be changed, ie your path to your database
# and the name of your coluùn containing insee code or city names
# Author(s):
#   - Vincent Bonhomme <bonhomme.vincent@gmail.com>
# Date:      30/06/2020
#

# 0. Dependencies --------------------------------------------
library(tidyverse) # general data handling
library(xlsx)      # to read straight from MS Office (berk)



# 1. Insee database ------------------------------------------
# available there (among others). This one is under Etalab license
insee_url <- "https://public.opendatasoft.com/explore/dataset/correspondance-code-insee-code-postal/download/?format=csv&timezone=Europe/Berlin&lang=fr&use_labels_for_header=true&csv_separator=%3B"
# download it locally (do it only once!)
download.file(insee_url, destfile = "insee.csv")
# load it as a tibble
insee <- read.csv("insee.csv", sep=";") %>% as_tibble()

# drop some and rename columns
# if you want more columns, select them !
insee <- insee %>%
  select(insee_code=Code.INSEE,
         insee_city=Commune,
         insee_departement=Département,
         insee_region=Région,
         geo_altitude=Altitude.Moyenne,
         geo_latlon=geo_point_2d,
         geo_shape=geo_shape) %>%
  # better formatting of places names
  mutate(insee_city=str_to_title(insee_city),
         insee_departement=str_to_title(insee_departement),
         insee_region=str_to_title(insee_region)) %>%
  # separate latlon into lat and lon
  separate(geo_latlon, into=c("lat", "lon"), sep=",", remove=TRUE)
# to have a look at this this beauty
head(insee)


# 2. Your database -------------------------------------------
# It should have some "insee" code already, otherwise, you can try
# Levenshtein distance between your city column and
# "insee_city" created in the tibble "insee" below

your_file <- "Synth_ArchBot_Vitis_17juin20.xlsx"
# if information is in another sheet than the first one, then change it below
local <- read.xlsx(your_file, sheetIndex = 1) %>% as_tibble()

# now if you already have correct insee code, go to last section
# if you don't we will have bind first the city names to the correct one
# to retrieve them

# CASE 1: You do NOT have insee codes ---------------------

# correct insee city names
insee$insee_city

# your city names (change your column name accordingly)
city_column <- "com"

# polish a bit your city_column
local[[city_column]] <- local[[city_column]] %>%
  # titlecase, just like we did for insee before
  str_to_title %>%
  # trim spaces on both ends
  str_trim(side="both")

# approximate string matching -----------------------------
# we use Levenshtein distance
# see https://en.wikipedia.org/wiki/Levenshtein_distance
# it's implemented in utils::adist, we only wrap around it
# there is possibly a less circonvoluted way to do it
# yet this one works fine

# this maps the closest city name to each of yours
# this may be a bit long (due to adist)
# on my machine it does ~ 700 lines / min
# so in cas of problems below,
# you'd better purge all of them, not one by one...
binding_df <- map_df(local[[city_column]],
                     ~ {
                       d <- adist(.x, insee$insee_city)
                       tibble(actual_city = .x,
                              matched_d = min(d),
                              matched_id = which.min(d),
                              matched_city = insee$insee_city[matched_id]) %>%
                         # rearrange columns
                         select(ends_with("city"), everything())
                     })

# inspect this now and purge all problems
# by correcting them in your spreadsheet
# they likely are only in the first rows below
# if we arrange them by decreasing distance
# ideally you only have 0s (or 1 or a very small number)
binding_df %>% arrange(desc(matched_d)) %>% distinct() %>% View

binding_df %>% arrange(desc(matched_d)) %>% distinct() %>% write.table("problems.csv", sep=";", row.names = FALSE)
# once everything is fine, it becomes a piece of cake
local2 <- bind_cols(local, insee[binding_df$matched_id, ])

# check it again
local2 %>% View

# then rename it and save it eg:
# my_df <- local2
# save(my_df, file="my_clean_df.rda")
# write.table(my_df, sep=";", row.names = FALSE)

# CASE 2 : You have insee codes ---------------------------
# This case is much easier,
# it is just a left_join between your table and 'insee' table

local2 <- local %>%
  # remove leading zeros
  mutate(insee_code = str_remove(insee_code, "^0+")) %>%
  # now join with insee
  left_join(insee, by="insee_code")

# inspect it for the non matched ones, and purge problem in your spreadsheet
local2 %>%
  select(insee_code, insee_city, geo_city) %>%
  arrange(insee_code) %>%
  filter(is.na(insee_code)) %>%
  distinct() %>% View

# Note that this supposes your insee code is in "insee_code" column
# if that's not the case, (say in "blabla" column), then you can:
# 1. rename it (preferred) with local <- rename(local, insee_code="blabla")
# 2. pass the 'by'  argument in left_join with 'c("blabla"="insee_code") ; see ?left_join

# once you're happy with it rename it and save it eg:
# my_df <- local2
# save(my_df, file="my_clean_df.rda")
# write.table(my_df, sep=";", row.names = FALSE)

# farewell -----
# In case of bugs, desperate or funny situations the hotline is there:
# <bonhomme.vincent@gmail.com>
# be nice, patient and bring chocolate/beers :~)
