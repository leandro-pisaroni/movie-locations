
# PACKAGES
library(readr)
library(dplyr)
library(stringr)

# LOADING THE DATASETS
locations <- read_csv('datasets/locations.csv')
imdb_movies <- read_csv('datasets/imdb_movies.csv')

# FINDING THE MOST POPULAR LOCATIONS IN SF
popular_locations <- locations %>%
    filter(!is.na(Locations)) %>% #Drop the rows with no location
    group_by(Locations) %>%
    count() %>%
    arrange(desc(n)) %>%
    head(10)

# FILTERING THE MOVIES FILMED ON THE MOST POPULAR LOCATIONS
location_filter <- popular_locations$Locations

popular_movies <- locations %>%
    select(-`Production Company`, -Distributor) %>% #Not interested in the Production Company or Distributor information
    filter(Locations %in% location_filter)

# ADDING THE GENRE AND GROSS INCOME FOR EACH MOVIE
imdb_movies_selected <- imdb_movies %>%
    select(title, year, genre, avg_vote, worldwide_gross_income)

movies <- inner_join(popular_movies,
                    imdb_movies_selected,
                    by = c("Title" = "title", "Release Year" = "year")) #Some films may share the same title and are only differentiated by year of release

movies_filtered <- movies %>%
    filter(avg_vote > 6 & str_detect(tolower(genre), c("action", "drama", "diography"))) %>%
    mutate(gross_income_numeric = as.numeric(str_replace(worldwide_gross_income, "\\$", "")))

# FINDING THE HIGHEST GROSSING FILM FOR EACH LOCATION
sf_hits <- movies_filtered %>%
    filter(!is.na(gross_income_numeric)) %>%
    group_by(Locations) %>%
    filter(gross_income_numeric == max(gross_income_numeric)) %>%
    arrange(match(Locations, location_filter)) %>%
    mutate(`Release Year` = as.integer(`Release Year`)) %>% #Year from numeric to integer
    select(Location = Locations, Title, Year =`Release Year`)

sf_hits

movies_filtered %>%
    arrange(match(Locations, location_filter))

head(imdb_movies_selected)

# These packages need to be loaded in the first @tests cell
library(testthat) 
library(IRkernel.testthat)
library(dplyr)

# Define test solution
locations <- c('Golden Gate Bridge', 'City Hall', 'Fairmont Hotel (950 Mason Street, Nob Hill)', 
             'Treasure Island', 'Coit Tower','Palace of Fine Arts (3301 Lyon Street)', 'Chinatown',
             'Bay Bridge', 'Grace Cathedral Episcopal Church (1100 California Street)',
             'Hall of Justice (850 Bryant Street)')

films <- c('Superman', 'Dawn of the Planet of the Apes', 'The Rock', 'Patch Adams',
     'San Andreas', 'Forrest Gump', 'Basic Instinct', 'The Game', 'The Towering Inferno', 'Basic Instinct')

years <- c(1978, 2014, 1996, 1998, 2015, 1994, 1992, 1997, 1974, 1992)

test_solution <- data.frame ('location' = locations,
                             'title' = films,
                             'year' = years)

# Convert column names for easier comparisons
colnames(sf_hits) <- tolower(make.names(colnames(sf_hits), unique = TRUE))

run_tests({    
    test_that("The solution has been provided.", {
        expect_true(exists('sf_hits'), 
            info = "Have you assigned your answer to a data frame named `sf_hits`?")
        expect_s3_class(sf_hits, "data.frame")
    })
    test_that("The correct columns exist.", {
        expect_true(all(sort(colnames(test_solution)) == sort(colnames(sf_hits))),
            info = "Your data frame is missing the required columns!")
    })
    test_that("The location columns are equal", {
    expect_true(isTRUE(all.equal(as.character(tolower(sf_hits$location)),
                                 as.character(tolower(test_solution$location)))),
        info = "Your submitted data frame does not contain the correct values!")
    })
    test_that("The title columns are equal", {
    expect_true(isTRUE(all.equal(as.character(tolower(sf_hits$title)),
                                 as.character(tolower(test_solution$title)))),
        info = "Your submitted data frame does not contain the correct values!")
    })
    test_that("The year columns are equal", {
        expect_true(isTRUE(all.equal(round(as.numeric(sf_hits$year), 0),
                                     round(as.numeric(test_solution$year)), 0)),
            info = "Your submitted data frame does not contain the correct values!")
    })
})
