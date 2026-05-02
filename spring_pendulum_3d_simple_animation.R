# 3D Spring Pendulum - ANIMASI SEDERHANA (Tanpa GIF)
# Fokus pada visualisasi 3D real-time yang smooth

library(deSolve)
library(rgl)

cat("=== 3D Spring Pendulum Animation ===\n\n")

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
initial_state <- c(x = 0.3, y = 0.2, z = -1.0, vx = 0.2, vy = 0.3, vz = 0)
params <- c(m = m, k = k, g = g, L0 = L0, b = b)

# ===== SOLVE ODE =====
cat("Menjalankan simulasi...\n")
times <- seq(0, 12, by = 0.02)
solution <- ode(y = initial_state, times = times, func = spring_pendulum_3d,
                parms = params, method = "rk4")

df_solution <- as.data.frame(solution)
colnames(df_solution) <- c("time", "x", "y", "z", "vx", "vy", "vz")

cat("✓ Simulasi selesai.\n")
cat("✓ Total frames:", nrow(df_solution), "\n\n")

# ===== ANIMASI 3D =====
cat("Membuka jendela 3D...\n")
cat("Anda bisa rotate dengan mouse, zoom dengan scroll\n")
cat("Tunggu animasi berjalan...\n\n")

open3d(windowRect = c(0, 0, 1200, 800))
bg3d("white")

# Setup viewing
view3d(theta = 45, phi = 25, zoom = 1)

# Vektor trajectory
traj_x <- c()
traj_y <- c()
traj_z <- c()

# Animasi utama
frame_skip <- 2  # Skip frame untuk animasi lebih smooth
frame_indices <- seq(1, nrow(df_solution), by = frame_skip)

for (idx in frame_indices) {
  # Clear shapes (tapi keep axis)
  clear3d(type = "shapes")

  # Data saat ini
  x <- df_solution$x[idx]
  y <- df_solution$y[idx]
  z <- df_solution$z[idx]
  vx <- df_solution$vx[idx]
  vy <- df_solution$vy[idx]
  vz <- df_solution$vz[idx]
  time_now <- df_solution$time[idx]

  # Update trajectory
  traj_x <- c(traj_x, x)
  traj_y <- c(traj_y, y)
  traj_z <- c(traj_z, z)

  # ANCHOR POINT (merah, tempat pegas menggantung)
  spheres3d(0, 0, 0, radius = 0.1, col = "red", alpha = 0.9)
  texts3d(0, 0.15, 0, "Anchor", color = "red", cex = 1.2)

  # PEGAS (garis kuning)
  L_current <- sqrt(x^2 + y^2 + z^2)
  pegas_color <- if (L_current > L0) "orange" else "yellowgreen"
  lines3d(c(0, x), c(0, y), c(0, z),
          lwd = 4, col = pegas_color, alpha = 0.8)

  # MASSA (bola biru, lebih besar)
  spheres3d(x, y, z, radius = 0.15, col = "blue", alpha = 0.85)

  # TRAJECTORY (garis jejak, transparan)
  if (length(traj_x) > 10) {
    lines3d(traj_x, traj_y, traj_z,
            lwd = 1.5, col = rgb(0.2, 0.2, 0.8, 0.4))
  }

  # VELOCITY VECTOR (garis hijau)
  scale_v <- 0.3
  if (sqrt(vx^2 + vy^2 + vz^2) > 0.01) {
    lines3d(c(x, x + vx*scale_v), c(y, y + vy*scale_v), c(z, z + vz*scale_v),
            color = "green", lwd = 3, alpha = 0.7)
  }

  # AXES
  axes3d(labels = TRUE, tick = TRUE, col = "black")
  xlim3d(-1, 1)
  ylim3d(-1, 1)
  zlim3d(-2.5, 0.5)

  # TITLE & INFO
  speed <- sqrt(vx^2 + vy^2 + vz^2)
  spring_length <- sqrt(x^2 + y^2 + z^2)

  title3d(
    main = "3D Spring Pendulum Animation",
    xlab = "X (m)", ylab = "Y (m)", zlab = "Z (m)",
    sub = sprintf(
      "t=%.2fs | Pos:(%.3f, %.3f, %.3f) | Speed:%.3f m/s | Spring:%.3f m",
      time_now, x, y, z, speed, spring_length
    )
  )

  # Pause untuk smoothness
  Sys.sleep(0.01)
}

cat("\n✓ Animasi selesai!\n\n")

# ===== PLOT PHASE SPACE DAN ENERGI =====
cat("Membuat plot analisis...\n")

par(mfrow = c(2, 3), mar = c(4, 4, 3, 2))

# 1. Posisi X vs Waktu
plot(df_solution$time, df_solution$x, type = "l", col = "red", lwd = 2,
     xlab = "Waktu (s)", ylab = "X (m)", main = "Posisi X")
grid()

# 2. Posisi Y vs Waktu
plot(df_solution$time, df_solution$y, type = "l", col = "green", lwd = 2,
     xlab = "Waktu (s)", ylab = "Y (m)", main = "Posisi Y")
grid()

# 3. Posisi Z vs Waktu
plot(df_solution$time, df_solution$z, type = "l", col = "blue", lwd = 2,
     xlab = "Waktu (s)", ylab = "Z (m)", main = "Posisi Z")
grid()

# 4. Phase Space X (Posisi vs Kecepatan)
plot(df_solution$x, df_solution$vx, type = "l", col = "red", lwd = 1.5,
     xlab = "X (m)", ylab = "Vx (m/s)", main = "Phase Space X")
grid()
points(df_solution$x[1], df_solution$vx[1], col = "green", cex = 2, pch = 19)
text(df_solution$x[1], df_solution$vx[1], " START", pos = 4, col = "green")

# 5. Phase Space Y
plot(df_solution$y, df_solution$vy, type = "l", col = "green", lwd = 1.5,
     xlab = "Y (m)", ylab = "Vy (m/s)", main = "Phase Space Y")
grid()

# 6. Panjang Pegas vs Waktu
L_all <- sqrt(df_solution$x^2 + df_solution$y^2 + df_solution$z^2)
plot(df_solution$time, L_all, type = "l", col = "purple", lwd = 2,
     xlab = "Waktu (s)", ylab = "Panjang Pegas (m)", main = "Spring Length")
abline(h = L0, col = "red", lty = 2, lwd = 2)
legend("topright", c("L(t)", "L₀ (natural)"), col = c("purple", "red"), lty = c(1, 2))
grid()

par(mfrow = c(1, 1))

# ===== ENERGI ANALYSIS =====
cat("\n=== ENERGY ANALYSIS ===\n")

KE <- 0.5 * m * (df_solution$vx^2 + df_solution$vy^2 + df_solution$vz^2)
PE_spring <- 0.5 * k * (L_all - L0)^2
PE_grav <- m * g * df_solution$z
E_total <- KE + PE_spring + PE_grav

# Plot Energi
par(mfrow = c(1, 2), mar = c(4, 4, 3, 2))

plot(df_solution$time, KE, type = "l", col = "red", lwd = 2.5,
     xlab = "Waktu (s)", ylab = "Energi (J)", main = "Energy Components",
     ylim = c(min(c(KE, PE_spring, PE_grav, E_total)) - 0.5,
              max(c(KE, PE_spring, PE_grav, E_total)) + 0.5))
lines(df_solution$time, PE_spring, col = "orange", lwd = 2.5)
lines(df_solution$time, PE_grav, col = "blue", lwd = 2.5)
lines(df_solution$time, E_total, col = "black", lwd = 3)
legend("topright", c("KE", "PE (Spring)", "PE (Gravity)", "Total E"),
       col = c("red", "orange", "blue", "black"), lwd = c(2.5, 2.5, 2.5, 3))
grid()

# Energy conservation
energy_loss <- abs(E_total[length(E_total)] - E_total[1])
plot(df_solution$time, E_total - E_total[1], type = "l", col = "darkred", lwd = 2,
     xlab = "Waktu (s)", ylab = "ΔE (J)", main = "Energy Deviation from Initial")
abline(h = 0, col = "gray", lty = 2)
grid()

par(mfrow = c(1, 1))

# ===== STATISTIK FINAL =====
cat("\nPosisi X   : min =", round(min(df_solution$x), 3),
    ", max =", round(max(df_solution$x), 3), "\n")
cat("Posisi Y   : min =", round(min(df_solution$y), 3),
    ", max =", round(max(df_solution$y), 3), "\n")
cat("Posisi Z   : min =", round(min(df_solution$z), 3),
    ", max =", round(max(df_solution$z), 3), "\n")
cat("Spring L   : min =", round(min(L_all), 3),
    ", max =", round(max(L_all), 3), "\n")
cat("Energy Loss:", round(energy_loss, 6), "J (damping effect)\n")
cat("\n✓ Semua plot selesai!\n")
