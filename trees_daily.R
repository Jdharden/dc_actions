# -------------------------------
# Project Description: How many trees are dying
#
#
#
# Creation Date:   2025-08-23
# -------------------------------

library(tidyverse)
library(googlesheets4)

# Read Current Trees -----------------------------------------------------------------------------------------
current_trees <- read_csv("https://hub.arcgis.com/api/v3/datasets/f6c3c04113944f23a7993f2e603abaf2_23/downloads/data?format=csv&spatialRefId=26985&where=1%3D1") |> 
  select(FACILITYID, CONDITION, TREE_NOTES)
# Read Previously Inserted Tress
google_trees <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WhoSzwz0inT8KXefOOoEPj-pTLMQn-LPFulfE8tqblc/edit?gid=1995103054#gid=1995103054", sheet = "trees_01")
# Read Current Dead Tree Upload
dead_google_trees <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1WhoSzwz0inT8KXefOOoEPj-pTLMQn-LPFulfE8tqblc/edit?gid=337317843#gid=337317843", sheet = "dead_trees") |> 
                                                 mutate(
                                                   FACILITYID = as.character(FACILITYID), 
                                                   CONDITION = as.character(CONDITION),
                                                   TREE_NOTES = as.character(TREE_NOTES), 
                                                   added = as.Date(added)
                                                 )

dead_trees <- google_trees |> 
  anti_join(current_trees, by = c("FACILITYID","CONDITION")) |> 
  filter(CONDITION == "Dead") |> 
  mutate(
    added = Sys.Date()
  )

google_trees <- bind_rows(dead_trees, dead_google_trees)


# Adding new list
current_trees |> 
  googlesheets4::write_sheet("https://docs.google.com/spreadsheets/d/1WhoSzwz0inT8KXefOOoEPj-pTLMQn-LPFulfE8tqblc/edit?gid=1194887215#gid=1194887215", sheet = "trees_01")

# Adding new trees
google_trees |> 
  googlesheets4::write_sheet("https://docs.google.com/spreadsheets/d/1WhoSzwz0inT8KXefOOoEPj-pTLMQn-LPFulfE8tqblc/edit?gid=1194887215#gid=1194887215", sheet = "dead_trees")

