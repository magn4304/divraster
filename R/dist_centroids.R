#' Function to calculate distance and direction of change between centroids
#'
#' @param raster1 A binary spatraster.
#' @param raster2 A binary spatraster.
#'
#' @return A data frame with distance and direction.
#' @export
#'
#' @examples
#' \donttest{
#' library(terra)
#' r1 <- terra::rast(system.file("extdata", "ref.tif",
#' package = "divraster"))
#' r2 <- terra::rast(system.file("extdata", "fut.tif",
#' package = "divraster"))
#' dd.calc(r1, r2)
#' }
dd.calc <- function(raster1, raster2) {
  # Check if the SpatRasters have the same number of layers
  if (terra::nlyr(raster1) != terra::nlyr(raster2)) {
    stop("The SpatRasters must have the same number of layers.")
  }

  # Initialize a list to store the results
  results <- list()

  # Iterate over the layers
  for (i in 1:terra::nlyr(raster1)) {
    # Select the current layer
    layer1 <- raster1[[i]]
    layer2 <- raster2[[i]]

    # Convert values equal to 1 into SpatVector polygons
    terra::values(layer1)[terra::values(layer1) != 1] <- NA
    terra::values(layer2)[terra::values(layer2) != 1] <- NA

    # Convert to polygons
    poly1 <- terra::as.polygons(layer1)
    poly2 <- terra::as.polygons(layer2)

    # Check the presence of valid geometries
    has_poly1 <- length(poly1) > 0
    has_poly2 <- length(poly2) > 0

    if (!has_poly1 & !has_poly2) {
      warning(paste("No data in both layers for", names(raster1)[i], "- marking as Absent species."))
      result <- data.frame(
        Layer = names(raster1)[i],
        Distance_meters = NA,
        Direction = "Absent species",
        Compass_angle = NA
      )
      results[[i]] <- result
      next
    }

    if (!has_poly1 & has_poly2) {
      warning(paste("No data in the first layer for", names(raster1)[i], "- marking as Novel species."))
      result <- data.frame(
        Layer = names(raster1)[i],
        Distance_meters = NA,
        Direction = "Novel species",
        Compass_angle = NA
      )
      results[[i]] <- result
      next
    }

    if (has_poly1 & !has_poly2) {
      warning(paste("No data in the second layer for", names(raster1)[i], "- marking as Locally extinct."))
      result <- data.frame(
        Layer = names(raster1)[i],
        Distance_meters = NA,
        Direction = "Locally extinct",
        Compass_angle = NA
      )
      results[[i]] <- result
      next
    }

    # Calculate the centroids of the polygons
    cent1 <- terra::centroids(poly1)
    cent2 <- terra::centroids(poly2)

    # Get the coordinates of the centroids
    coords1 <- terra::crds(cent1)
    coords2 <- terra::crds(cent2)

    # Calculate the distance in meters (assuming the projection is appropriate)
    dist_meters <- terra::distance(coords1, coords2, lonlat = FALSE)[1, 1]

    # Function to determine the relative direction
    determine_direction <- function(coord1, coord2) {
    # Compute differences
    dx <- coord2[1] - coord1[1]
    dy <- coord2[2] - coord1[2]
  
    # If there is no movement
    if (dx == 0 && dy == 0) {
      return(list(direction = "No change", angle = NA))
    }
  
    # Calculate the angle in degrees
    angle <- atan2(dy, dx) * (180 / pi)

    # Get compass direction (angle)
    compass_angle <- 90 - angle
  
    # Normalize angle to match 8 compass directions (each covering 45 degrees)
    if (angle >= -22.5 && angle < 22.5) {
      return(list(direction = "East", angle = compass_angle))
    } else if (angle >= 22.5 && angle < 67.5) {
      return(list(direction = "Northeast", angle = compass_angle))
    } else if (angle >= 67.5 && angle < 112.5) {
      return(list(direction = "North", angle = compass_angle))
    } else if (angle >= 112.5 && angle < 157.5) {
    return(list(direction = "Northwest", angle = compass_angle))
    } else if (angle >= -67.5 && angle < -22.5) {
      return(list(direction = "Southeast", angle = compass_angle))
    } else if (angle >= -112.5 && angle < -67.5) {
      return(list(direction = "South", angle = compass_angle))
    } else if (angle >= -157.5 && angle < -112.5) {
      return(list(direction = "Southwest", angle = compass_angle))
    } else {
      return(list(direction = "West", angle = compass_angle))
      }
    }

    # Determine the relative direction
    direction <- determine_direction(coords1, coords2)

    # Create a data frame with the information for the current layer
    result <- data.frame(
      Layer = names(raster1)[i],
      Distance_meters = dist_meters,
      Direction = direction$direction,
      Compass_angle = direction$angle
    )

    # Add the result to the list
    results[[i]] <- result
  }

  # Combine all results into a single data frame
  results_df <- do.call(rbind, results)

  return(results_df)
}
