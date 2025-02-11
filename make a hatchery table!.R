# Load necessary libraries
library(tidyverse)
library(writexl)

# Read and preprocess the data
Hatcherydata <- read.csv(file.choose())

# Set column names and clean up the dataset
colnames(Hatcherydata) <- Hatcherydata[4, ]  # Set column names from the 4th row
Hatcherydata <- Hatcherydata[-c(1:4), ]      # Remove the first 4 rows
Hatcherydata <- Hatcherydata[-nrow(Hatcherydata), ]  # Remove the last row (totals)
colnames(Hatcherydata) <- gsub(" ", ".", colnames(Hatcherydata))  # Replace spaces in column names
Hatcherydata$Date <- as.Date(Hatcherydata$Date, format = "%m/%d/%Y")  # Convert dates

# Filter out rows where Mark.Info is missing or empty
Hatcherydata <- Hatcherydata[!is.na(Hatcherydata$Mark.Info) & Hatcherydata$Mark.Info != "", ]

# Create the summary table
Hatcherydata_summary <- Hatcherydata %>%
  mutate(
    # Extract year and updated Brood_prefix
    Year = sapply(strsplit(Brood, ":"), function(x) x[4]),  # Extract brood year
    Brood_prefix = sapply(strsplit(Brood, ":"), function(x) paste(c(x[1:3], x[5]), collapse = ":")),  # Include "H" or "W"
    # Calculate total spawned
    Total.Spawned = as.numeric(Lethal.Jack.Spawn) + 
      as.numeric(Lethal.Male.Spawn) + 
      as.numeric(Lethal.Female.Spawn),
    # Add a column to check for mis-labeled wild fish
    Potential.Wild.Mislabeled = ifelse(
      sapply(strsplit(Brood, ":"), function(x) x[5]) == "H" &
        Checked.for.Wire == "Y" &
        Wire.Present == "N" &
        Wire.Collected == "N" &
        Mark.Info == "UM", Total.Spawned, 0  # Use Total.Spawned now that it is defined
    ),
    # Add a column to check for non-AD/UM mark statuses
    Other.Mark.Status = ifelse(
      !(Mark.Info %in% c("AD", "UM")), Total.Spawned, 0  # Include fish not marked as AD or UM
    )
  ) %>%
  # Filter out rows with "M" in the fifth category
  filter(
    sapply(strsplit(Brood, ":"), function(x) x[5]) %in% c("H", "W", "U")  # Keep only "H", "W", or "U"
  ) %>%
  group_by(Brood_prefix, Year, Mark.Info) %>%  # Group by Brood_prefix and Year
  summarize(
    Total.Male.Spawn = sum(as.numeric(Lethal.Male.Spawn), na.rm = TRUE),
    Total.Female.Spawn = sum(as.numeric(Lethal.Female.Spawn), na.rm = TRUE),
    Total.Jack.Spawn = sum(as.numeric(Lethal.Jack.Spawn), na.rm = TRUE),
    Total.Spawned = sum(as.numeric(Lethal.Jack.Spawn), na.rm = TRUE) + 
      sum(as.numeric(Lethal.Male.Spawn), na.rm = TRUE) + 
      sum(as.numeric(Lethal.Female.Spawn), na.rm = TRUE),
    Total.NVF.Spawn = sum(as.numeric(Lethal.NVF.Spawn), na.rm = TRUE),
    Male.Morts = sum(as.numeric(Male.Morts), na.rm = TRUE),
    Female.Morts = sum(as.numeric(Female.Morts), na.rm = TRUE),
    Jack.Morts = sum(as.numeric(Jack.Morts), na.rm = TRUE),
    Total.Morts = sum(as.numeric(Male.Morts), na.rm = TRUE) + 
      sum(as.numeric(Female.Morts), na.rm = TRUE) + 
      sum(as.numeric(Jack.Morts), na.rm = TRUE),
    Male.Surplus = sum(as.numeric(Male.Surplus), na.rm = TRUE),
    Female.Surplus = sum(as.numeric(Female.Surplus), na.rm = TRUE),
    Jack.Surplus = sum(as.numeric(Jack.Surplus), na.rm = TRUE),
    Total.Surplus = sum(as.numeric(Male.Surplus), na.rm = TRUE) + 
      sum(as.numeric(Female.Surplus), na.rm = TRUE) + 
      sum(as.numeric(Jack.Surplus), na.rm = TRUE),
    Live.Male.Spawn = sum(as.numeric(Live.Male.Spawn), na.rm = TRUE), 
    Live.Female.Spawn = sum(as.numeric(Live.Female.Spawn), na.rm = TRUE),
    Live.Jack.Spawn = sum(as.numeric(Live.Jack.Spawn), na.rm = TRUE),
    Total.Live.Spawn = sum(as.numeric(Live.Jack.Spawn), na.rm = TRUE) + 
      sum(as.numeric(Live.Male.Spawn), na.rm = TRUE) + 
      sum(as.numeric(Live.Female.Spawn), na.rm = TRUE),
    Total.Potential.Wild.Mislabeled = sum(Potential.Wild.Mislabeled, na.rm = TRUE),  # Sum mis-labeled wild fish
    Total.Other.Mark.Status = sum(Other.Mark.Status, na.rm = TRUE),  # Sum fish with non-AD/UM statuses
    .groups = "drop"
  )

# Display the summary table
print(Hatcherydata_summary)

# Save the summary table to a file
write_xlsx(Hatcherydata_summary, path = "C://Users//brut1477//OneDrive - Washington State Executive Branch Agencies//Desktop//Nasellesummary.xlsx")

#Now we need to do the same thing but for the plant data!
# Set column names and clean up the dataset
# Read and preprocess the data
Plantdata <- read.csv(file.choose())

colnames(Plantdata) <- Plantdata[5, ]  # Set column names from the 5th row
Plantdata <- Plantdata[-c(1:5), ]      # Remove the first 5 rows
colnames(Plantdata) <- gsub(" ", ".", colnames(Plantdata))  # Replace spaces in column names
Plantdata$`Activity Date` <- as.Date(Plantdata$`Activity.Date`, format = "%m/%d/%Y")  # Convert dates

# Create the summary table
Plantdata_summary <- Plantdata %>%
  mutate(
    # Extract year and updated Brood_prefix
    Year = sapply(strsplit(Brood, ":"), function(x) x[4]),  # Extract brood year
    Brood_prefix = sapply(strsplit(Brood, ":"), function(x) paste(c(x[1:3], x[5]), collapse = ":")),  # Include "H" or "W"
    Total.plant = as.numeric(`Estimated.Males`) + 
      as.numeric(`Estimated.Females`) + 
      as.numeric(`Estimated.Jacks`)
  ) %>%
  # Filter out rows with "M" in the fifth category
  filter(
    sapply(strsplit(Brood, ":"), function(x) x[5]) %in% c("H", "W", "U")  # Keep only "H", "W", or "U"
  ) %>%
  group_by(Brood_prefix, Year,Mark.Info) %>%  # Group by Brood_prefix and Year
  summarize(
    Estimated.Males = sum(as.numeric(`Estimated.Males`), na.rm = TRUE),
    Estimated.Females = sum(as.numeric(`Estimated.Females`), na.rm = TRUE),
    Estimated.Jacks = sum(as.numeric(`Estimated.Jacks`), na.rm = TRUE),
    Total.plant = sum(as.numeric(`Estimated.Males`), na.rm = TRUE) + 
      sum(as.numeric(`Estimated.Females`), na.rm = TRUE) + 
      sum(as.numeric(`Estimated.Jacks`), na.rm = TRUE),
    .groups = "drop"
  )

# Save the summary table to a file
write_xlsx(Plantdata_summary, path = "C://Users//brut1477//OneDrive - Washington State Executive Branch Agencies//Desktop/humptulipsplantsummary.xlsx")
