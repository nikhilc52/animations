# Plot the original data
plot(average_positions$average_x, type = "l", main = "Original Data (X)", ylab = "X", xlab = "Time")
plot(average_positions$average_y, type = "l", main = "Original Data (Y)", ylab = "Y", xlab = "Time")
plot(average_positions$average_z, type = "l", main = "Original Data (Z)", ylab = "Z", xlab = "Time")

# Smoothing Splines
spline_fit <- smooth.spline(average_positions$average_x, spar = .8) # Adjust spar for more/less smoothness
average_positions$x_spline <- predict(spline_fit)$y
spline_fit <- smooth.spline(average_positions$average_y, spar = .8) # Adjust spar for more/less smoothness
average_positions$y_spline <- predict(spline_fit)$y
spline_fit <- smooth.spline(average_positions$average_z, spar = .8) # Adjust spar for more/less smoothness
average_positions$z_spline <- predict(spline_fit)$y

# Plot all smoothed data
par(mfrow = c(3, 3))
plot(average_positions$average_x, type = "l", main = "Smoothing Spline", ylab = "X", xlab = "Time")
lines(average_positions$x_spline, col = "red")
plot(average_positions$average_y, type = "l", main = "Smoothing Spline", ylab = "Y", xlab = "Time")
lines(average_positions$y_spline, col = "red")
plot(average_positions$average_z, type = "l", main = "Smoothing Spline", ylab = "Z", xlab = "Time")
lines(average_positions$z_spline, col = "red")
