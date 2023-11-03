# =============================================================================
# Import the data that will be used to create the tables.
#
# Use this code to import the data at the top of every .qmd file. That way, 
# the information in Administrative Information table on the home page is 
# correct for all tables.
#
# L2C Specific Notes:
# ---------------------------------------------------------------------------
# Import `combined_data_03.rds`. This data set was created in the final
# steps of the variable map processing pipeline:
# `link2care_public/data_survey_05_post_processing.qmd`
#
# This code assumes that the repositories for `link2care_public` and 
# `link2care_master_table` are stored in the same parent folder, accessible
# through the 'here' package.
#
# Other Notes:
# ---------------------------------------------------------------------------
# It looks like the Quarto team is looking for a way to pass data between qmd
# files inside of Quarto projects, but they don't have it ready yet. Maybe
# just keep an eye on it.
# Using a .Rprofile file?
# SO Post: https://stackoverflow.com/questions/72544775/passing-data-from-one-qmd-file-to-another-in-a-quarto-book-template?rq=1
# GH Issue: https://github.com/quarto-dev/quarto-cli/discussions/1045
# GH Issue: https://github.com/quarto-dev/quarto-cli/discussions/431
#
# =============================================================================

library(haven)

# Import the data that will be used for all of the descriptive tables.
# Some of these objects are also used in the Administrative Information table
# on the homepage.

df_nm  <- "combined_data_03.rds"
folder <- 'link2care_public/data/Combined Participant Data/'
path <- paste0(
    substring(
        here::here(), 
        1, 
        nchar(here::here())-nchar('link2care_master_table')
      ),
    folder,
    df_nm
  )

full_data <- readr::read_rds(path)

# Calculate and save other values that will be used across multiple Quarto files
# =============================================================================
# This should be used sparingly. Only variables or values which are 
# specifically utilized by these Quarto Master Table files should be modified.
# Major modifications should instead be performed on the actual data file
# before import to these Quarto Master Table files, whenever reasonable. 

# Rename levels in `group`, for easier filtering/indexing in these files.

levels(full_data$group)[levels(full_data$group) == 
    "Usual Case Management (UCM)"] <- "UCM"
levels(full_data$group)[levels(full_data$group) == 
    "Usual Care plus Smartphone (UCM+SP)"] <- "UCM+SP"
levels(full_data$group)[levels(full_data$group) == 
    "Usual Care plus Smartphone based Case Management (SPCM)"] <- "L2C"

# Create a `drop_flag` variable, to allow comparison between included and
# excluded subjects in the data set (from multiple drop/exclusion conditions)

full_data <- full_data |>
  dplyr::mutate(
    drop_flag = ifelse(
      ((!is.na(group) & is.na(dropped_status)) &
         !subj_randomized_status == "Do Not Include"),
      FALSE,
      TRUE
    )
  )

# Filtered Data Sets
# ===========================================================================
# These data sets should be limited in use. However, if the same selection
# criteria may be used across multiple files, it may be beneficial to
# keep the selection criteria in a singular location.


# Baseline Data
# ---------------------------------------------------------------------------
# We created a set of Baseline data. Since baseline data collection was
# split between Visit 1 and Visit 2 (with Visit 1 considered the Baseline Date)
# we consolidated the visits into a single "Baseline" set without overlap

baseline_data <- dplyr::left_join( 
    # Visit 1 Data
    full_data |>
      dplyr::filter(visit == "Visit 1: Baseline") |>
      dplyr::select(where(~ any(!is.na(.)))),
    # Visit 2 Data
    full_data |>
      dplyr::filter(visit == "Visit 2: Randomization") |>
      dplyr::select(where(~ any(!is.na(.)))) |>
      dplyr::select(-any_of(
        full_data |>
          dplyr::filter(visit == "Visit 1: Baseline") |>
          dplyr::select(where(~ any(!is.na(.)))) |>
          dplyr::select(-id) |>
          names()
      )),
    by = 'id')

# Randomized Data
# ---------------------------------------------------------------------------
# We created a data set that only contained the included subjects

randomized_data <- full_data |>
  dplyr::filter(!drop_flag)

# Data management
# =============================================================================
# 2023-10-27: this was moved to each individual file


# Calculate and save information about the size of the full data frame that 
# will be used to create the descriptive tables.
# =============================================================================
df_size <- full_data |> 
  utils::object.size() |> 
  format(units = "auto")
df_dim <- dim(full_data) |> 
  format(big.mark = ",", trim=TRUE)


# Calculate and save other values that will be used across multiple Quarto files
# =============================================================================

# Total number of participants
n_participants <- length(unique(full_data$id))

# Total number of included participants
n_participants_incl <- length(unique(randomized_data$id))
  
# Total number of excluded participants
n_participants_excl <- length(setdiff(
  unique(full_data$id), unique(randomized_data$id)
  ))

# Total number of participants per L2C group

n_per_group <- randomized_data |>
  dplyr::filter(visit == "Visit 1: Baseline") |> 
  dplyr::count(group) |> 
  dplyr::pull(n) |> 
  rlang::set_names(levels(randomized_data$group))