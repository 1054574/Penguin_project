# ---- Cleaning --------------
#Clean column names, remove empty rows and remove columns called comment and delta and rename the culmen columns to bill columns 
cleaning <- function(data_raw){
  data_raw %>%
    clean_names() %>%
    remove_empty(c("rows", "cols")) %>%
    select(-starts_with("delta")) %>%
    select(-comments) %>%
    rename(bill_length_mm = culmen_length_mm,
           bill_depth_mm = culmen_depth_mm)
}

# Subset the data to only include the penguins that are not NA for flipper length, bill length  
remove_empty_rows<- function(data_clean){
  data_clean %>%
    filter(species == ("Gentoo penguin (Pygoscelis papua)")) %>%
    filter(!is.na(body_mass_g), !is.na(species), !is.na(sex)) %>%
  select(species, sex, body_mass_g) %>%
    droplevels() 
}

