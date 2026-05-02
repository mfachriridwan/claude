# 3D Spring Pendulum - ANIMASI (Fixed Version)
# Versi sederhana dan stabil

library(deSolve)
library(rgl)

cat("=== 3D Spring Pendulum Animation ===\n")

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
cat("✓ Total frames:", nrow(df), "\n")

# ===== ANIMASI 3D =====
cat("Membuka window 3D...\n\n")
open3d(windowRect = c(50, 50, 1000, 800))
bg3d("white")

# Trajectory storage
traj_x <- numeric(0)
traj_y <- numeric(0)
traj_z <- numeric(0)

# Animation loop
frame_skip <- 2
frame_indices <- seq(1, nrow(df), by = frame_skip)

for (i in frame_indices) {
  clear3d(type = "shapes")

  x <- df$x[i]
  y <- df$y[i]
  z <- df$z[i]
  vx <- df$vx[i]
  vy <- df$vy[i]
  vz <- df$vz[i]
  t <- df$time[i]

  traj_x <- c(traj_x, x)
  traj_y <- c(traj_y, y)
  traj_z <- c(traj_z, z)

  # Anchor point (merah)
  spheres3d(0, 0, 0, radius = 0.1, col = "red")

  # Spring (kuning/orange)
  L <- sqrt(x^2 + y^2 + z^2)
  spring_col <- if (L > L0) "orange" else "gold"
  lines3d(c(0, x), c(0, y), c(0, z), lwd = 4, col = spring_col)

  # Mass (bola biru)
  spheres3d(x, y, z, radius = 0.12, col = "blue", alpha = 0.8)

  # Trajectory trail
  if (length(traj_x) > 5) {
    lines3d(traj_x, traj_y, traj_z, lwd = 1.5, col = rgb(0.3, 0.3, 0.8, 0.4))
  }

  # Velocity vector (garis hijau)
  v_mag <- sqrt(vx^2 + vy^2 + vz^2)
  if (v_mag > 0.05) {
    scale_v <- 0.25
    lines3d(c(x, x + vx*scale_v), c(y, y + vy*scale_v), c(z, z + vz*scale_v),
            col = "green", lwd = 3)
  }

  # Axes
  axes3d(col = "black")

  # Title
  title3d(
    main = "3D Spring Pendulum",
    xlab = "X (m)", ylab = "Y (m)", zlab = "Z (m)",
    sub = sprintf("t = %.2f s | (%.3f, %.3f, %.3f) | L = %.3f m",
                  t, x, y, z, L)
  )

  Sys.sleep(0.01)
}

cat("\n✓ Animasi selesai!\n\n")

# ===== PLOTS =====
cat("Membuat plot analisis...\n")

par(mfrow = c(2, 3), mar = c(4, 4, 3, 1))

# Plot 1: X vs time
plot(df$time, df$x, type = "l", col = "red", lwd = 2,
     xlab = "Waktu (s)", ylab = "X (m)", main = "Posisi X")
grid()

# Plot 2: Y vs time
plot(df$time, df$y, type = "l", col = "green", lwd = 2,
     xlab = "Waktu (s)", ylab = "Y (m)", main = "Posisi Y")
grid()

# Plot 3: Z vs time
plot(df$time, df$z, type = "l", col = "blue", lwd = 2,
     xlab = "Waktu (s)", ylab = "Z (m)", main = "Posisi Z")
grid()

# Plot 4: Phase space X
plot(df$x, df$vx, type = "l", col = "red", lwd = 1.5,
     xlab = "X (m)", ylab = "Vx (m/s)", main = "Phase Space X")
grid()
points(df$x[1], df$vx[1], col = "green", cex = 2.5, pch = 19)

# Plot 5: Phase space Y
plot(df$y, df$vy, type = "l", col = "green", lwd = 1.5,
     xlab = "Y (m)", ylab = "Vy (m/s)", main = "Phase Space Y")
grid()

# Plot 6: Spring length
L_all <- sqrt(df$x^2 + df$y^2 + df$z^2)
plot(df$time, L_all, type = "l", col = "purple", lwd = 2,
     xlab = "Waktu (s)", ylab = "L (m)", main = "Panjang Pegas")
abline(h = L0, col = "red", lty = 2, lwd = 1.5)
grid()

par(mfrow = c(1, 1))

# ===== ENERGI =====
cat("\n=== ANALISIS ENERGI ===\n")

KE <- 0.5 * m * (df$vx^2 + df$vy^2 + df$vz^2)
PE_spring <- 0.5 * k * (L_all - L0)^2
PE_grav <- m * g * df$z
E_total <- KE + PE_spring + PE_grav

par(mfrow = c(1, 2), mar = c(4, 4, 3, 1))

# Energy plot
plot(df$time, KE, type = "l", col = "red", lwd = 2.5,
     xlab = "Waktu (s)", ylab = "Energi (J)", main = "Energi Sistem",
     ylim = c(min(E_total)-0.5, max(KE)+0.5))
lines(df$time, PE_spring, col = "orange", lwd = 2.5)
lines(df$time, PE_grav, col = "blue", lwd = 2.5)
lines(df$time, E_total, col = "black", lwd = 3)
legend("topright", c("KE", "PE_spring", "PE_grav", "Total"),
       col = c("red", "orange", "blue", "black"), lwd = 2.5)
grid()

# Energy deviation
plot(df$time, E_total - E_total[1], type = "l", col = "darkred", lwd = 2,
     xlab = "Waktu (s)", ylab = "ΔE (J)", main = "Energy Loss (Damping)")
abline(h = 0, col = "gray", lty = 2)
grid()

par(mfrow = c(1, 1))

# ===== STATISTIK =====
cat("\nStatistik Posisi:\n")
cat("  X: [", round(min(df$x), 3), ", ", round(max(df$x), 3), "]\n", sep = "")
cat("  Y: [", round(min(df$y), 3), ", ", round(max(df$y), 3), "]\n", sep = "")
cat("  Z: [", round(min(df$z), 3), ", ", round(max(df$z), 3), "]\n", sep = "")
cat("  L: [", round(min(L_all), 3), ", ", round(max(L_all), 3), "]\n\n", sep = "")

cat("Energi:\n")
cat("  E_initial:", round(E_total[1], 4), "J\n")
cat("  E_final:  ", round(E_total[length(E_total)], 4), "J\n")
cat("  Loss:     ", round(abs(E_total[length(E_total)] - E_total[1]), 6), "J\n\n")

cat("✓ Selesai!\n")
