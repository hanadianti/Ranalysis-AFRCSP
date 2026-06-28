# Install and load required packages
install.packages("rasterVis")
install.packages("pbkrtest")  # Corrected typo from 'pbkrtes'
install.packages("terra", dependencies = TRUE)
install.packages("sf")
install.packages("sp")
install.packages("data.table")  # for fread
install.packages("RColorBrewer")

# Load required libraries
library(sf)
library(sp)
library(raster)
library(rasterVis)  # Load rasterVis for rasterTheme and levelplot
library(RColorBrewer)
library(data.table)

# Define shapefile path and read the shapefile
coast_shapefile <- "ne_110m_coastline/ne_110m_coastline.shp"  # Adjust to .shp file
coast_lines <- terra::vect(coast_shapefile)  # Read shapefile using terra

# Step 1: Read CSV data using fread
merged_data <- fread("regional_area5.csv", header = TRUE)

# Step 2: Convert to matrix if needed
quantity_mat <- as.matrix(quantity_dt)

# Step 3: Create a raster object from matrix data
quantity_raster <- raster(quantity_mat,
                          xmn = -180, xmx = 180, ymn = -90, ymx = 90,
                          crs = '+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0')

# Set up color palette and plot theme (using rasterTheme)
potentialTheme <- rasterTheme(region = brewer.pal(9, "Reds"))

# Plot the raster data with levelplot
plt_potential <- levelplot(quantity_raster,
                           margin = FALSE,
                           maxpixels = 1e6,
                           xlab = NULL,
                           ylab = NULL,
                           colorkey = list(space = 'right', width = 2, height = 1/2, at = seq(0, 0.16, 0.002)),
                           par.settings = potentialTheme)

# Save plot as JPEG
jpeg("Figure_8_potential_map_dpi600.jpeg", width = 12, height = 6, units = 'in', res = 600)
print(plt_potential)
dev.off()

# Plot the raster with coastlines added
plot(quantity_raster, col = brewer.pal(9, "Reds")(100))
lines(coast_lines, col = "black", lwd = 0.5)

# Save the plot with coastlines
jpeg("Figure_8_with_coastlines.jpeg", width = 12, height = 6, units = 'in', res = 600)
plot(quantity_raster, col = brewer.pal(9, "Reds")(100))
lines(coast_lines, col = "black", lwd = 0.5)
dev.off()

library(RColorBrewer)

# Create a color palette with 100 colors based on the Reds scheme
reds_palette <- colorRampPalette(brewer.pal(9, "Greens"))

# Plot the raster using the generated palette
plot(quantity_raster, col = reds_palette(100))



# Save using ggsave for better control over the plot output
ggsave("Figure_8_intensity3.jpeg", plot = plt_potential, device = 'jpeg', width = 20 * 0.35, height = 10 * 0.35, dpi = 600)
ggsave("Figure_8_Global_Cumulative_Afforestation_Carbon_Sequestration_Potential_in_2100.jpeg", plot = plt_potential, device = 'jpeg', width = 17 * 0.5, height = 10 * 0.35, dpi = 600)
