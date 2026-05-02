# 3D Spring Pendulum - ANIMASI
# Simulasi dan visualisasi gerakan massa dalam 3D secara real-time

library(deSolve)
library(rgl)
library(gifski)

# ===== PARAMETER SISTEM =====
m <- 1.0          # Massa (kg)
k <- 10.0         # Konstanta pegas (N/m)
g <- 9.8          # Gravitasi (m/s²)
L0 <- 1.0         # Panjang pegas alami (m)
b <- 0.1          # Koefisien redaman

# ===== FUNGSI ODE =====
spring_pendulum_3d <- function(time, state, params) {
  x  <- state[1]
  y  <- state[2]
  z  <- state[3]
  vx <- state[4]
  vy <- state[5]
  vz <- state[6]

  m_mass <- params["m"]
  k_spring <- params["k"]
  g_grav <- params["g"]
  L0_natural <- params["L0"]
  b_damp <- params["b"]

  L <- sqrt(x^2 + y^2 + z^2)

  if (L > 0) {
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

# ===== KONDISI AWAL =====
initial_state <- c(x = 0.3, y = 0.2, z = -1.0, vx = 0, vy = 0, vz = 0)
params <- c(m = m, k = k, g = g, L0 = L0, b = b)

# ===== SOLVE ODE =====
cat("Menjalankan simulasi...\n")
times <- seq(0, 15, by = 0.02)  # 15 detik, step 0.02s
solution <- ode(y = initial_state, times = times, func = spring_pendulum_3d,
                parms = params, method = "rk4")

df_solution <- as.data.frame(solution)
colnames(df_solution) <- c("time", "x", "y", "z", "vx", "vy", "vz")

cat("Simulasi selesai. Total steps:", nrow(df_solution), "\n")

# ===== ANIMASI 3D REAL-TIME =====
cat("\nMemulai animasi 3D (tekan ESC untuk berhenti)...\n")
cat("Tunggu hingga animasi selesai...\n")

open3d()
bg3d("white")

# Setup viewing angle
view3d(theta = 45, phi = 30, zoom = 1.2)

# Frame interval (animasi lebih smooth)
frame_interval <- 1  # Tampilkan setiap 1 frame (bisa diubah ke 2 atau 3 untuk lebih cepat)
frame_indices <- seq(1, nrow(df_solution), by = frame_interval)

# Vector untuk menyimpan trajectory
trajectory_x <- c()
trajectory_y <- c()
trajectory_z <- c()

# Loop animasi
for (i in frame_indices) {
  # Hapus semua objek kecuali axis
  clear3d(type = "shapes")

  # Data saat ini
  x <- df_solution$x[i]
  y <- df_solution$y[i]
  z <- df_solution$z[i]
  time_current <- df_solution$time[i]

  # Update trajectory
  trajectory_x <- c(trajectory_x, x)
  trajectory_y <- c(trajectory_y, y)
  trajectory_z <- c(trajectory_z, z)

  # Plot anchor point (titik atas, tempat pegas menggantung)
  spheres3d(0, 0, 0, radius = 0.08, col = "red")

  # Plot pegas (garis dari anchor ke massa)
  lines3d(c(0, x), c(0, y), c(0, z), lwd = 3, col = "orange")

  # Plot massa (bola)
  spheres3d(x, y, z, radius = 0.12, col = "blue", alpha = 0.8)

  # Plot trajectory (garis jejak)
  if (length(trajectory_x) > 1) {
    lines3d(trajectory_x, trajectory_y, trajectory_z,
            lwd = 1, col = rgb(0, 0, 0.7, 0.3))
  }

  # Setup axes
  axes3d(labels = TRUE, tick = TRUE)
  xlim3d(-1, 1)
  ylim3d(-1, 1)
  zlim3d(-2.5, 0.5)

  # Judul dan info
  title3d(main = "3D Spring Pendulum Animation",
          xlab = "X (m)", ylab = "Y (m)", zlab = "Z (m)",
          sub = sprintf("Time: %.2f s | Pos: (%.2f, %.2f, %.2f)",
                        time_current, x, y, z))

  # Render frame
  rgl.snapshot(filename = tempfile(fileext = ".png"), fmt = "png")

  # Pause sebentar untuk visualisasi
  Sys.sleep(0.01)
}

cat("Animasi selesai!\n")

# ===== MEMBUAT ANIMATED GIF =====
cat("\nMembuat animated GIF (ini memakan waktu)...\n")

# Buat temp directory untuk menyimpan frames
temp_dir <- tempdir()
frame_files <- c()

# Generate frames untuk GIF
for (i in frame_indices) {
  # Bersihkan
  clear3d(type = "shapes")

  # Data
  x <- df_solution$x[i]
  y <- df_solution$y[i]
  z <- df_solution$z[i]
  time_current <- df_solution$time[i]

  trajectory_x <- df_solution$x[1:i]
  trajectory_y <- df_solution$y[1:i]
  trajectory_z <- df_solution$z[1:i]

  # Plot elements
  spheres3d(0, 0, 0, radius = 0.08, col = "red")
  lines3d(c(0, x), c(0, y), c(0, z), lwd = 3, col = "orange")
  spheres3d(x, y, z, radius = 0.12, col = "blue", alpha = 0.8)

  if (i > 1) {
    lines3d(trajectory_x, trajectory_y, trajectory_z,
            lwd = 1, col = rgb(0, 0, 0.7, 0.3))
  }

  axes3d(labels = TRUE, tick = TRUE)
  xlim3d(-1, 1)
  ylim3d(-1, 1)
  zlim3d(-2.5, 0.5)

  title3d(main = "3D Spring Pendulum",
          xlab = "X (m)", ylab = "Y (m)", zlab = "Z (m)",
          sub = sprintf("Time: %.2f s", time_current))

  # Save frame
  frame_path <- file.path(temp_dir, sprintf("frame_%04d.png", which(frame_indices == i)))
  rgl.snapshot(filename = frame_path, fmt = "png")
  frame_files <- c(frame_files, frame_path)
}

# Buat GIF
output_gif <- "/Users/scarpiy/Desktop/spring_pendulum_animation.gif"
tryCatch({
  gifski(frame_files,
         gif_file = output_gif,
         width = 800,
         height = 600,
         delay = 0.05)  # 50ms per frame
  cat("\n✓ GIF berhasil dibuat di:", output_gif, "\n")
}, error = function(e) {
  cat("\nWarning: GIF tidak bisa dibuat (gifski mungkin tidak terinstall)\n")
  cat("Install dengan: install.packages('gifski')\n")
})

# ===== PLOT TAMBAHAN =====
par(mfrow = c(2, 2))

# Phase space (x vs vx)
plot(df_solution$x, df_solution$vx, type = "l", col = "red",
     xlab = "Posisi X (m)", ylab = "Kecepatan X (m/s)",
     main = "Phase Space: X", lwd = 1.5)
grid()

# Phase space (y vs vy)
plot(df_solution$y, df_solution$vy, type = "l", col = "green",
     xlab = "Posisi Y (m)", ylab = "Kecepatan Y (m/s)",
     main = "Phase Space: Y", lwd = 1.5)
grid()

# Panjang pegas vs waktu
L <- sqrt(df_solution$x^2 + df_solution$y^2 + df_solution$z^2)
plot(df_solution$time, L, type = "l", col = "purple",
     xlab = "Waktu (s)", ylab = "Panjang Pegas (m)",
     main = "Panjang Pegas vs Waktu", lwd = 1.5)
abline(h = L0, col = "red", lty = 2, label = "L0")
grid()
legend("topright", c("L(t)", "L₀"), col = c("purple", "red"), lty = c(1, 2))

# Jarak dari anchor (r)
r <- sqrt(df_solution$x^2 + df_solution$y^2 + df_solution$z^2)
plot(df_solution$time, r, type = "l", col = "darkblue",
     xlab = "Waktu (s)", ylab = "Jarak dari Anchor (m)",
     main = "Distance from Anchor", lwd = 1.5)
grid()

par(mfrow = c(1, 1))

cat("\n===== SIMULASI SELESAI =====\n")
