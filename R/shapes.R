#' Create geometric shapes with optional text

#' @description
#' This function creates various geometric shapes using base R graphics.
#' It supports multiple shape types including circle, square, rectangle,
#' triangle, hexagon, star, ellipse, and regular polygons with custom sides.
#'
#'
#' @param shape Character string specifying the shape to create. Must be one of:
#'   "circle", "square", "rectangle", "triangle", "hexagon", "star", "ellipse", "polygon".
#' @param x_center Numeric, x-coordinate of the shape's center. Default is 0.
#' @param y_center Numeric, y-coordinate of the shape's center. Default is 0.
#' @param size Numeric, the primary size parameter (radius for circle, side length for square, etc.). Default is 1.
#' @param width Numeric, width for rectangle and ellipse. Default is size.
#' @param height Numeric, height for rectangle and ellipse. Default is size.
#' @param n_sides Integer, number of sides for polygon. Default is 5.
#' @param rotation Numeric, rotation angle in degrees. Default is 0.
#' @param fill_color Character string or NA, the fill color of the shape. Default is NA (transparent).
#' @param border_color Character string, the border color of the shape. Default is "black".
#' @param line_width Numeric, the width of the border line. Default is 1.
#' @param title Character string, the title of the plot. Default is "Geometric Shape".
#' @param text Character string, optional text to display inside the shape. Default is NULL (no text).
#' @param text_color Character string, color of the text. Default is "black".
#' @param text_size Numeric, size of the text. Default is 5.
#' @param text_font Character string, font family for the text. Default is "".
#' @param text_angle Numeric, rotation angle for the text in degrees. Default is 0.
#' @param show_axes Logical. Whether to display the axes. Default is FALSE.
#' @param show_ticks Logical. Whether to display axis ticks. Default is FALSE.
#' @param show_axis_labels Logical. Whether to display axis labels (X and Y). Default is FALSE.
#'
#' @return A list containing:
#'   \itemize{
#'     \item plot: The ggplot object
#'     \item coordinates: Data frame with x and y coordinates of the shape's vertices
#'     \item center: Vector with x and y coordinates of the shape's center
#'     \item size: Primary size parameter used
#'     \item shape: Shape type
#'     \item text: Text displayed inside the shape (if any)
#'   }
#'
#' @examples
#' \dontrun{
#' # Example 1: Basic circle
#' create_shape(shape = "circle", size = 2, fill_color = "lightblue")
#'
#' # Example 2: Square with rotation and text
#' create_shape(
#'   shape = "square",
#'   size = 3,
#'   rotation = 45,
#'   fill_color = "salmon",
#'   border_color = "darkred",
#'   line_width = 2,
#'   text = "Rotated Square",
#'   text_color = "darkblue",
#'   text_size = 3
#' )
#'
#' # Example 3: Hexagon with custom title
#' create_shape(
#'   shape = "hexagon",
#'   size = 2.5,
#'   fill_color = "lightgreen",
#'   border_color = "darkgreen",
#'   title = "My Hexagon"
#' )
#'
#' # Example 4: Star with custom styling
#' create_shape(
#'   shape = "star",
#'   size = 3,
#'   rotation = 15,
#'   fill_color = "gold",
#'   border_color = "orange",
#'   line_width = 2,
#'   title = "Gold Star"
#' )
#'
#' # Example 5: Rectangle with custom dimensions
#' create_shape(
#'   shape = "rectangle",
#'   width = 4,
#'   height = 2,
#'   fill_color = "lavender",
#'   text = "Rectangle",
#'   text_size = 2.5
#' )
#'
#' # Example 6: Triangle with rotation and styling
#' create_shape(
#'   shape = "triangle",
#'   size = 3,
#'   rotation = 180,
#'   fill_color = "pink",
#'   border_color = "purple",
#'   line_width = 2,
#'   text = "rpkg",
#'   text_color = "black",
#'   text_size = 4
#' )
#'
#' # Example 7: Ellipse with custom dimensions
#' create_shape(
#'   shape = "ellipse",
#'   width = 5,
#'   height = 2,
#'   rotation = 30,
#'   fill_color = "lightcyan",
#'   border_color = "steelblue",
#'   title = "Rotated Ellipse"
#' )
#'
#' # Example 8: Custom polygon (octagon)
#' create_shape(
#'   shape = "polygon",
#'   n_sides = 8,
#'   size = 2,
#'   fill_color = "thistle",
#'   border_color = "purple",
#'   title = "Octagon",
#'   text = "8",
#'   text_color = "darkblue",
#'   text_size = 3
#' )
#'
#' # Example 9: Overlapping shapes (need to call par() between shapes)
#' create_shape(
#'   shape = "circle",
#'   x_center = 0,
#'   y_center = 0,
#'   size = 3,
#'   fill_color = rgb(1, 0, 0, 0.5),
#'   title = "Overlapping Shapes"
#' )
#'
#' par(new = TRUE)  # Allow adding to the existing plot
#' create_shape(
#'   shape = "circle",
#'   x_center = 1,
#'   y_center = 1,
#'   size = 3,
#'   fill_color = rgb(0, 0, 1, 0.5),
#'   title = ""  # Empty title to avoid overwriting the previous title
#' )
#' # Create a star with text
#' create_shape("star", size = 2,
#'                        fill_color = "gold",
#'                        text = "*",
#'                        text_size = 8)
#' }
#'
#' @export
create_shape <- function(shape = c("circle", "square", "rectangle", "triangle", "hexagon", "star", "ellipse", "polygon"), x_center = 0, y_center = 0, size = 1,
                         width = size, height = size, n_sides = 5, rotation = 0,
                         fill_color = NA, border_color = "black", line_width = 1,
                         title = "Geometric Shape", text = NULL, text_color = "black",
                         text_size = 1, text_font = 1, text_angle = 0,
                         show_axes = FALSE, show_ticks = FALSE, show_axis_labels = FALSE) {
  # Define valid shapes
  shape = match.arg(shape)

  # Check if the provided shape is valid
  if (shape %nin% valid_shapes)
    stop(paste0("Invalid shape: '", shape, "'. Please choose one of: ",paste(valid_shapes, collapse = ", "), "."))

  # Generate points based on the specified shape
  coords <- data.frame(x = numeric(), y = numeric())

  # Convert rotation to radians
  rotation_rad <- rotation * pi / 180

  if (shape == "circle") {
    theta <- seq(0, 2 * pi, length.out = 100)
    x <- x_center + size * cos(theta)
    y <- y_center + size * sin(theta)
    coords <- data.frame(x = x, y = y)
    if (title == "Geometric Shape") title <- "Circle"

  } else if (shape == "square") {
    # Create square points (centered)
    points <- matrix(c(
      -size/2, -size/2,
      size/2, -size/2,
      size/2, size/2,
      -size/2, size/2
    ), ncol = 2, byrow = TRUE)

    # Apply rotation
    if (rotation != 0) {
      rot_matrix <- matrix(c(cos(rotation_rad), -sin(rotation_rad),
                             sin(rotation_rad), cos(rotation_rad)), 2, 2)
      points <- points %*% rot_matrix
    }

    # Translate to center position
    x <- x_center + points[,1]
    y <- y_center + points[,2]
    coords <- data.frame(x = c(x, x[1]), y = c(y, y[1]))
    if (title == "Geometric Shape") title <- "Square"

  } else if (shape == "rectangle") {
    # Create rectangle points (centered)
    points <- matrix(c(
      -width/2, -height/2,
      width/2, -height/2,
      width/2, height/2,
      -width/2, height/2
    ), ncol = 2, byrow = TRUE)

    # Apply rotation
    if (rotation != 0) {
      rot_matrix <- matrix(c(cos(rotation_rad), -sin(rotation_rad),
                             sin(rotation_rad), cos(rotation_rad)), 2, 2)
      points <- points %*% rot_matrix
    }

    # Translate to center position
    x <- x_center + points[,1]
    y <- y_center + points[,2]
    coords <- data.frame(x = c(x, x[1]), y = c(y, y[1]))
    if (title == "Geometric Shape") title <- "Rectangle"

  } else if (shape == "triangle") {
    # Create equilateral triangle points (centered)
    height_tri <- size * sqrt(3) / 2
    points <- matrix(c(
      0, height_tri * 2/3,
      -size/2, -height_tri/3,
      size/2, -height_tri/3
    ), ncol = 2, byrow = TRUE)

    # Apply rotation
    if (rotation != 0) {
      rot_matrix <- matrix(c(cos(rotation_rad), -sin(rotation_rad),
                             sin(rotation_rad), cos(rotation_rad)), 2, 2)
      points <- points %*% rot_matrix
    }

    # Translate to center position
    x <- x_center + points[,1]
    y <- y_center + points[,2]
    coords <- data.frame(x = c(x, x[1]), y = c(y, y[1]))
    if (title == "Geometric Shape") title <- "Triangle"

  } else if (shape == "hexagon") {
    # Create regular hexagon
    theta <- seq(0, 2 * pi, length.out = 7)[1:6]
    theta <- theta + rotation_rad  # Apply rotation
    x <- x_center + size * cos(theta)
    y <- y_center + size * sin(theta)
    coords <- data.frame(x = c(x, x[1]), y = c(y, y[1]))
    if (title == "Geometric Shape") title <- "Hexagon"

  } else if (shape == "star") {
    # Create a 5-pointed star
    outer_points <- 5
    theta_outer <- seq(0, 2 * pi, length.out = outer_points + 1)[1:outer_points]
    theta_outer <- theta_outer + rotation_rad - pi/2  # Start from top, apply rotation

    # Create interleaved points (outer and inner)
    theta <- numeric(outer_points * 2)
    theta[seq(1, outer_points * 2, 2)] <- theta_outer
    theta[seq(2, outer_points * 2, 2)] <- theta_outer + pi/outer_points

    # Radii for outer and inner points
    radii <- numeric(outer_points * 2)
    radii[seq(1, outer_points * 2, 2)] <- size  # Outer points
    radii[seq(2, outer_points * 2, 2)] <- size * 0.4  # Inner points

    # Calculate coordinates
    x <- x_center + radii * cos(theta)
    y <- y_center + radii * sin(theta)
    coords <- data.frame(x = c(x, x[1]), y = c(y, y[1]))
    if (title == "Geometric Shape") title <- "Star"

  } else if (shape == "ellipse") {
    theta <- seq(0, 2 * pi, length.out = 100)

    # Create ellipse points
    x_unrotated <- width/2 * cos(theta)
    y_unrotated <- height/2 * sin(theta)

    # Apply rotation if needed
    if (rotation != 0) {
      x_rot <- x_unrotated * cos(rotation_rad) - y_unrotated * sin(rotation_rad)
      y_rot <- x_unrotated * sin(rotation_rad) + y_unrotated * cos(rotation_rad)
      x <- x_center + x_rot
      y <- y_center + y_rot
    } else {
      x <- x_center + x_unrotated
      y <- y_center + y_unrotated
    }

    coords <- data.frame(x = x, y = y)
    if (title == "Geometric Shape") title <- "Ellipse"

  } else if (shape == "polygon") {
    # Create regular polygon with n_sides
    theta <- seq(0, 2 * pi, length.out = n_sides + 1)[1:n_sides]
    theta <- theta + rotation_rad  # Apply rotation
    x <- x_center + size * cos(theta)
    y <- y_center + size * sin(theta)
    coords <- data.frame(x = c(x, x[1]), y = c(y, y[1]))
    if (title == "Geometric Shape") title <- paste0(n_sides, "-gon")
  }

  # Calculate margin for plot based on shape dimensions
  x_range <- max(coords$x) - min(coords$x)
  y_range <- max(coords$y) - min(coords$y)
  margin <- max(x_range, y_range) * 0.2

  # Handle axis display settings
  xlab <- ifelse(show_axis_labels, "X", "")
  ylab <- ifelse(show_axis_labels, "Y", "")

  # Set up axis parameters based on user preferences
  axis_params <- list()
  if (!show_axes) {
    axis_params$axes <- FALSE
  }

  # Set title to NULL if it's empty string to properly remove it
  if (is.null(title) || title == "") {
    title_param <- NULL
  } else {
    title_param <- title
  }

  # Create new plot with adjusted parameters
  do.call(plot, c(
    list(
      x = coords$x, y = coords$y,
      type = "n",  # Don't plot points
      xlim = c(min(coords$x) - margin, max(coords$x) + margin),
      ylim = c(min(coords$y) - margin, max(coords$y) + margin),
      xlab = xlab, ylab = ylab,
      main = title_param,
      asp = 1  # Force aspect ratio 1:1
    ),
    axis_params
  ))

  # Handle ticks display if axes are shown but ticks should be hidden
  if (show_axes && !show_ticks) {
    axis(1, labels = FALSE, tick = FALSE)
    axis(2, labels = FALSE, tick = FALSE)
  }

  # Draw the shape
  polygon(
    x = coords$x, y = coords$y,
    col = fill_color,
    border = border_color,
    lwd = line_width
  )

  # Add text if provided
  if (!is.null(text)) {
    # Convert text_size from ggplot size to cex parameter (approx)
    cex_value <- text_size * 0.25

    # In base R, graphics family parameter (font family) is device-dependent
    # text_font is used as font parameter: 1=plain, 2=bold, 3=italic, 4=bold italic, 5=symbol
    text(
      x = x_center, y = y_center,
      labels = text,
      col = text_color,
      cex = cex_value,
      font = text_font,
      srt = text_angle
    )
  }

  # Return the shape properties
  invisible(list(
    coordinates = coords,
    center = c(x_center = x_center, y_center = y_center),
    size = size,
    shape = shape,
    text = text
  ))
}

# ggplot version of the function
# create_shape_ggplot <- function(shape = "circle", x_center = 0, y_center = 0, size = 1,
#                          width = size, height = size, n_sides = 5, rotation = 0,
#                          fill_color = NA, border_color = "black", line_width = 1,
#                          title = "Geometric Shape", text = NULL, text_color = "black",
#                          text_size = 5, text_font = "", text_angle = 0) {
#
#   # Define valid shapes
#   valid_shapes <- c("circle", "square", "rectangle", "triangle", "hexagon", "star", "ellipse", "polygon")
#
#   # Check if the provided shape is valid
#   if (!shape %in% valid_shapes) {
#     stop(paste0("Invalid shape: '", shape, "'. Please choose one of: ",
#                 paste(valid_shapes, collapse = ", "), "."))
#   }
#
#   # Generate points based on the specified shape
#   coords <- data.frame(x = numeric(), y = numeric())
#
#   # Convert rotation to radians
#   rotation_rad <- rotation * pi / 180
#
#   if (shape == "circle") {
#     theta <- seq(0, 2 * pi, length.out = 100)
#     x <- x_center + size * cos(theta)
#     y <- y_center + size * sin(theta)
#     coords <- data.frame(x = x, y = y)
#     title <- ifelse(title == "Geometric Shape", "Circle", title)
#
#   } else if (shape == "square") {
#     # Create square points (centered)
#     points <- matrix(c(
#       -size/2, -size/2,
#       size/2, -size/2,
#       size/2, size/2,
#       -size/2, size/2
#     ), ncol = 2, byrow = TRUE)
#
#     # Apply rotation
#     if (rotation != 0) {
#       rot_matrix <- matrix(c(cos(rotation_rad), -sin(rotation_rad),
#                              sin(rotation_rad), cos(rotation_rad)), 2, 2)
#       points <- points %*% rot_matrix
#     }
#
#     # Translate to center position
#     x <- x_center + points[,1]
#     y <- y_center + points[,2]
#     coords <- data.frame(x = c(x, x[1]), y = c(y, y[1]))
#     title <- ifelse(title == "Geometric Shape", "Square", title)
#
#   } else if (shape == "rectangle") {
#     # Create rectangle points (centered)
#     points <- matrix(c(
#       -width/2, -height/2,
#       width/2, -height/2,
#       width/2, height/2,
#       -width/2, height/2
#     ), ncol = 2, byrow = TRUE)
#
#     # Apply rotation
#     if (rotation != 0) {
#       rot_matrix <- matrix(c(cos(rotation_rad), -sin(rotation_rad),
#                              sin(rotation_rad), cos(rotation_rad)), 2, 2)
#       points <- points %*% rot_matrix
#     }
#
#     # Translate to center position
#     x <- x_center + points[,1]
#     y <- y_center + points[,2]
#     coords <- data.frame(x = c(x, x[1]), y = c(y, y[1]))
#     title <- ifelse(title == "Geometric Shape", "Rectangle", title)
#
#   } else if (shape == "triangle") {
#     # Create equilateral triangle points (centered)
#     height_tri <- size * sqrt(3) / 2
#     points <- matrix(c(
#       0, height_tri * 2/3,
#       -size/2, -height_tri/3,
#       size/2, -height_tri/3
#     ), ncol = 2, byrow = TRUE)
#
#     # Apply rotation
#     if (rotation != 0) {
#       rot_matrix <- matrix(c(cos(rotation_rad), -sin(rotation_rad),
#                              sin(rotation_rad), cos(rotation_rad)), 2, 2)
#       points <- points %*% rot_matrix
#     }
#
#     # Translate to center position
#     x <- x_center + points[,1]
#     y <- y_center + points[,2]
#     coords <- data.frame(x = c(x, x[1]), y = c(y, y[1]))
#     title <- ifelse(title == "Geometric Shape", "Triangle", title)
#
#   } else if (shape == "hexagon") {
#     # Create regular hexagon
#     theta <- seq(0, 2 * pi, length.out = 7)[1:6]
#     theta <- theta + rotation_rad  # Apply rotation
#     x <- x_center + size * cos(theta)
#     y <- y_center + size * sin(theta)
#     coords <- data.frame(x = c(x, x[1]), y = c(y, y[1]))
#     title <- ifelse(title == "Geometric Shape", "Hexagon", title)
#
#   } else if (shape == "star") {
#     # Create a 5-pointed star
#     outer_points <- 5
#     theta_outer <- seq(0, 2 * pi, length.out = outer_points + 1)[1:outer_points]
#     theta_outer <- theta_outer + rotation_rad - pi/2  # Start from top, apply rotation
#
#     # Create interleaved points (outer and inner)
#     theta <- numeric(outer_points * 2)
#     theta[seq(1, outer_points * 2, 2)] <- theta_outer
#     theta[seq(2, outer_points * 2, 2)] <- theta_outer + pi/outer_points
#
#     # Radii for outer and inner points
#     radii <- numeric(outer_points * 2)
#     radii[seq(1, outer_points * 2, 2)] <- size  # Outer points
#     radii[seq(2, outer_points * 2, 2)] <- size * 0.4  # Inner points
#
#     # Calculate coordinates
#     x <- x_center + radii * cos(theta)
#     y <- y_center + radii * sin(theta)
#     coords <- data.frame(x = c(x, x[1]), y = c(y, y[1]))
#     title <- ifelse(title == "Geometric Shape", "Star", title)
#
#   } else if (shape == "ellipse") {
#     theta <- seq(0, 2 * pi, length.out = 100)
#
#     # Create ellipse points
#     x_unrotated <- width/2 * cos(theta)
#     y_unrotated <- height/2 * sin(theta)
#
#     # Apply rotation if needed
#     if (rotation != 0) {
#       x_rot <- x_unrotated * cos(rotation_rad) - y_unrotated * sin(rotation_rad)
#       y_rot <- x_unrotated * sin(rotation_rad) + y_unrotated * cos(rotation_rad)
#       x <- x_center + x_rot
#       y <- y_center + y_rot
#     } else {
#       x <- x_center + x_unrotated
#       y <- y_center + y_unrotated
#     }
#
#     coords <- data.frame(x = x, y = y)
#     title <- ifelse(title == "Geometric Shape", "Ellipse", title)
#
#   } else if (shape == "polygon") {
#     # Create regular polygon with n_sides
#     theta <- seq(0, 2 * pi, length.out = n_sides + 1)[1:n_sides]
#     theta <- theta + rotation_rad  # Apply rotation
#     x <- x_center + size * cos(theta)
#     y <- y_center + size * sin(theta)
#     coords <- data.frame(x = c(x, x[1]), y = c(y, y[1]))
#     title <- ifelse(title == "Geometric Shape", paste0(n_sides, "-gon"), title)
#   }
#
#   # Create the plot with ggplot2
#   p <- ggplot(coords, aes(x, y)) +
#     geom_polygon(fill = fill_color, color = border_color, size = line_width) +
#     coord_fixed(ratio = 1) +  # Ensure the shape isn't distorted
#     labs(title = title, x = "X", y = "Y") +
#     theme_minimal()
#
#   # Add text if provided
#   if (!is.null(text)) {
#     text_df <- data.frame(
#       x = x_center,
#       y = y_center,
#       label = text
#     )
#
#     p <- p + geom_text(
#       data = text_df,
#       aes(x, y, label = label),
#       color = text_color,
#       size = text_size,
#       family = text_font,
#       angle = text_angle
#     )
#   }
#
#   # Display the plot
#   print(p)
#
#   # Return the plot and shape properties
#   return(list(
#     plot = p,
#     coordinates = coords,
#     center = c(x_center = x_center, y_center = y_center),
#     size = size,
#     shape = shape,
#     text = text
#   ))
# }

# Define valid shapes
valid_shapes <- c("circle", "square", "rectangle", "triangle", "hexagon", "star", "ellipse", "polygon")
