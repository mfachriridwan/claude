# 3D Spring Pendulum - INTERACTIVE VIEWER (HTML/Plotly)
# Bisa dibuka di RStudio Viewer atau Browser

library(deSolve)
library(plotly)

cat("=== 3D Spring Pendulum Interactive Viewer ===\n")

# ===== PARAMETER SISTEM =====
m <- 1.0
k <- 10.0
g <- 9.8
L0 <- 1.0
b <- 0.1

# ===== FUNGSI ODE =====
spring_pendulum_3d <- function(time, state, params) {
  x <- state[1]
  y <- state[2]
  z <- state[3]
  vx <- state[4]
  vy <- state[5]
  vz <- state[6]

  m_mass <- params["m"]
  k_spring <- params["k"]
  g_grav <- params["g"]
  L0_natural <- params["L0"]
  b_damp <- params["b"]

  L <- sqrt(x^2 + y^2 + z^2)

  if (L > 0.01) {
    F_spring_x <- -k_spring * (L - L0_natural) * (x / L)
    F_spring_y <- -k_spring * (L - L0_natural) * (y / L)
    F_spring_z <- -k_spring * (L - L0_natural) * (z / L)
  } else {
    F_spring_x <- 0
    F_spring_y <- 0
    F_spring_z <- 0
  }

  F_gravity_z <- -m_mass * g_grav
  F_damp_x <- -b_damp * vx
  F_damp_y <- -b_damp * vy
  F_damp_z <- -b_damp * vz

  ax <- (F_spring_x + F_damp_x) / m_mass
  ay <- (F_spring_y + F_damp_y) / m_mass
  az <- (F_spring_z + F_gravity_z + F_damp_z) / m_mass

  list(c(vx, vy, vz, ax, ay, az))
}

# ===== SOLVE ODE =====
cat("Menjalankan simulasi...\n")
initial_state <- c(x = 0.3, y = 0.2, z = -1.0, vx = 0.2, vy = 0.3, vz = 0)
params <- c(m = m, k = k, g = g, L0 = L0, b = b)

times <- seq(0, 12, by = 0.02)
solution <- ode(y = initial_state, times = times, func = spring_pendulum_3d,
                parms = params, method = "rk4")

df <- as.data.frame(solution)
colnames(df) <- c("time", "x", "y", "z", "vx", "vy", "vz")
cat("✓ Simulasi selesai. Total frames:", nrow(df), "\n")

# ===== PREPARASI DATA =====
df$L <- sqrt(df$x^2 + df$y^2 + df$z^2)
df$speed <- sqrt(df$vx^2 + df$vy^2 + df$vz^2)
df$KE <- 0.5 * m * df$speed^2
df$PE_spring <- 0.5 * k * (df$L - L0)^2
df$PE_grav <- m * g * df$z
df$E_total <- df$KE + df$PE_spring + df$PE_grav

cat("Membuat visualisasi interaktif...\n")

# ===== 3D TRAJECTORY PLOT (PLOTLY) =====
fig1 <- plot_ly() %>%
  # Trajectory
  add_trace(x = df$x, y = df$y, z = df$z,
            type = "scatter3d", mode = "lines",
            line = list(color = "blue", width = 2),
            name = "Trajectory",
            hovertemplate = "Time: %{customdata:.2f}s<br>X: %{x:.3f}<br>Y: %{y:.3f}<br>Z: %{z:.3f}<extra></extra>",
            customdata = df$time) %>%

  # Starting point (hijau)
  add_trace(x = df$x[1], y = df$y[1], z = df$z[1],
            type = "scatter3d", mode = "markers",
            marker = list(size = 8, color = "green"),
            name = "Start",
            hovertemplate = "START<extra></extra>") %>%

  # Ending point (merah)
  add_trace(x = df$x[nrow(df)], y = df$y[nrow(df)], z = df$z[nrow(df)],
            type = "scatter3d", mode = "markers",
            marker = list(size = 8, color = "red"),
            name = "End",
            hovertemplate = "END<extra></extra>") %>%

  # Anchor point (orange)
  add_trace(x = 0, y = 0, z = 0,
            type = "scatter3d", mode = "markers+text",
            marker = list(size = 10, color = "orange"),
            text = "Anchor",
            textposition = "top center",
            name = "Anchor",
            hovertemplate = "ANCHOR POINT<extra></extra>") %>%

  layout(
    title = list(text = "3D Spring Pendulum - Complete Trajectory",
                 font = list(size = 20)),
    scene = list(
      xaxis = list(title = "X (m)", backgroundcolor = "rgb(240,240,240)"),
      yaxis = list(title = "Y (m)", backgroundcolor = "rgb(240,240,240)"),
      zaxis = list(title = "Z (m)", backgroundcolor = "rgb(240,240,240)"),
      camera = list(
        eye = list(x = 1.5, y = 1.5, z = 1.3)
      )
    ),
    width = 1000,
    height = 800,
    hovermode = "closest",
    showlegend = TRUE
  )

# ===== TIME SERIES PLOTS =====
fig2 <- subplot(
  # X vs Time
  plot_ly(df, x = ~time, y = ~x, type = "scatter", mode = "lines",
          name = "X", line = list(color = "red", width = 2)) %>%
    add_trace(y = ~y, name = "Y", line = list(color = "green", width = 2)) %>%
    add_trace(y = ~z, name = "Z", line = list(color = "blue", width = 2)) %>%
    layout(title = "Position vs Time",
           xaxis = list(title = "Time (s)"),
           yaxis = list(title = "Position (m)"),
           hovermode = "x unified"),

  # Spring Length vs Time
  plot_ly(df, x = ~time, y = ~L, type = "scatter", mode = "lines",
          name = "Spring Length", line = list(color = "purple", width = 2)) %>%
    add_hline(y = L0, line = list(dash = "dash", color = "orange"),
              annotation_text = "L₀", annotation_position = "right") %>%
    layout(title = "Spring Length vs Time",
           xaxis = list(title = "Time (s)"),
           yaxis = list(title = "Length (m)"),
           hovermode = "x unified"),

  nrows = 2, margin = 0.1
) %>%
  layout(
    title = list(text = "Time Series Analysis", font = list(size = 18)),
    height = 800,
    showlegend = TRUE
  )

# ===== ENERGY PLOT =====
fig3 <- plot_ly() %>%
  add_trace(x = df$time, y = df$KE, type = "scatter", mode = "lines",
            name = "Kinetic Energy", line = list(color = "red", width = 2)) %>%
  add_trace(x = df$time, y = df$PE_spring, type = "scatter", mode = "lines",
            name = "PE (Spring)", line = list(color = "orange", width = 2)) %>%
  add_trace(x = df$time, y = df$PE_grav, type = "scatter", mode = "lines",
            name = "PE (Gravity)", line = list(color = "blue", width = 2)) %>%
  add_trace(x = df$time, y = df$E_total, type = "scatter", mode = "lines",
            name = "Total Energy", line = list(color = "black", width = 3, dash = "dash")) %>%
  layout(
    title = list(text = "Energy Analysis", font = list(size = 18)),
    xaxis = list(title = "Time (s)"),
    yaxis = list(title = "Energy (J)"),
    hovermode = "x unified",
    width = 1000,
    height = 600,
    legend = list(orientation = "v", x = 0.02, y = 0.98)
  )

# ===== PHASE SPACE PLOTS =====
fig4 <- subplot(
  plot_ly(df, x = ~x, y = ~vx, type = "scatter", mode = "lines",
          name = "X", line = list(color = "red", width = 1.5)) %>%
    layout(title = "Phase Space: X",
           xaxis = list(title = "Position (m)"),
           yaxis = list(title = "Velocity (m/s)"),
           hovermode = "closest"),

  plot_ly(df, x = ~y, y = ~vy, type = "scatter", mode = "lines",
          name = "Y", line = list(color = "green", width = 1.5)) %>%
    layout(title = "Phase Space: Y",
           xaxis = list(title = "Position (m)"),
           yaxis = list(title = "Velocity (m/s)"),
           hovermode = "closest"),

  plot_ly(df, x = ~z, y = ~vz, type = "scatter", mode = "lines",
          name = "Z", line = list(color = "blue", width = 1.5)) %>%
    layout(title = "Phase Space: Z",
           xaxis = list(title = "Position (m)"),
           yaxis = list(title = "Velocity (m/s)"),
           hovermode = "closest"),

  nrows = 2, margin = 0.1
) %>%
  layout(
    title = list(text = "Phase Space Analysis", font = list(size = 18)),
    height = 800
  )

# ===== VELOCITY & SPRING LENGTH PLOT =====
fig5 <- subplot(
  plot_ly(df, x = ~time, y = ~speed, type = "scatter", mode = "lines",
          name = "Speed", fill = "tozeroy",
          line = list(color = "darkgreen", width = 2)) %>%
    layout(title = "Velocity Magnitude",
           xaxis = list(title = "Time (s)"),
           yaxis = list(title = "Speed (m/s)"),
           hovermode = "x unified"),

  plot_ly(df, x = ~time, y = ~L, type = "scatter", mode = "lines",
          name = "Spring Length", fill = "tozeroy",
          line = list(color = "darkblue", width = 2)) %>%
    add_hline(y = L0, line = list(dash = "dash", color = "red", width = 1),
              annotation_text = "Natural Length") %>%
    layout(title = "Spring Extension",
           xaxis = list(title = "Time (s)"),
           yaxis = list(title = "Length (m)"),
           hovermode = "x unified"),

  nrows = 2, margin = 0.1
) %>%
  layout(
    title = list(text = "Dynamics Analysis", font = list(size = 18)),
    height = 800
  )

# ===== SAVE AS HTML FILES =====
cat("\nMenyimpan file HTML...\n")

output_dir <- "/Users/scarpiy/Desktop"

# Simpan setiap plot
htmlwidgets::saveWidget(fig1,
  file = file.path(output_dir, "01_trajectory_3d.html"))
htmlwidgets::saveWidget(fig2,
  file = file.path(output_dir, "02_time_series.html"))
htmlwidgets::saveWidget(fig3,
  file = file.path(output_dir, "03_energy.html"))
htmlwidgets::saveWidget(fig4,
  file = file.path(output_dir, "04_phase_space.html"))
htmlwidgets::saveWidget(fig5,
  file = file.path(output_dir, "05_dynamics.html"))

cat("✓ File HTML berhasil dibuat:\n")
cat("  1. 01_trajectory_3d.html\n")
cat("  2. 02_time_series.html\n")
cat("  3. 03_energy.html\n")
cat("  4. 04_phase_space.html\n")
cat("  5. 05_dynamics.html\n\n")

# ===== TAMPILKAN DI RSTUDIO VIEWER =====
cat("Menampilkan di RStudio Viewer...\n")

# Buat combined view (summary)
fig_summary <- subplot(
  plot_ly(df, x = ~time, y = ~x, type = "scatter", mode = "lines",
          name = "X") %>% layout(title = "Position X"),

  plot_ly(df, x = ~time, y = ~L, type = "scatter", mode = "lines",
          name = "Spring L") %>% layout(title = "Spring Length"),

  plot_ly(df, x = ~time, y = ~E_total, type = "scatter", mode = "lines",
          name = "Energy") %>% layout(title = "Total Energy"),

  plot_ly(df, x = ~x, y = ~vx, type = "scatter", mode = "lines") %>%
    layout(title = "Phase Space X"),

  nrows = 2, ncols = 2
) %>%
  layout(
    title = "3D Spring Pendulum - Dashboard",
    height = 900,
    showlegend = FALSE
  )

print(fig_summary)

cat("\n")
cat("=== STATISTIK SIMULASI ===\n")
cat("Durasi: ", max(df$time), " detik\n")
cat("Total steps: ", nrow(df), "\n")
cat("X range: [", round(min(df$x), 3), ", ", round(max(df$x), 3), "]\n", sep = "")
cat("Y range: [", round(min(df$y), 3), ", ", round(max(df$y), 3), "]\n", sep = "")
cat("Z range: [", round(min(df$z), 3), ", ", round(max(df$z), 3), "]\n", sep = "")
cat("Spring length range: [", round(min(df$L), 3), ", ", round(max(df$L), 3), "]\n", sep = "")
cat("Max speed: ", round(max(df$speed), 3), " m/s\n")
cat("Initial energy: ", round(df$E_total[1], 4), " J\n")
cat("Final energy: ", round(df$E_total[nrow(df)], 4), " J\n")
cat("Energy loss: ", round(abs(df$E_total[nrow(df)] - df$E_total[1]), 6), " J\n")
cat("\n✓ Selesai!\n")
