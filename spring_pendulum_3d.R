# 3D Spring Pendulum - Pemrograman R
# Sistem: Massa terhubung ke pegas di atas, bergerak dalam 3D dengan pengaruh gravitasi

library(deSolve)
library(rgl)

# ===== Parameter Sistem =====
m <- 1.0          # Massa (kg)
k <- 10.0         # Konstanta pegas (N/m)
g <- 9.8          # Gravitasi (m/s²)
L0 <- 1.0         # Panjang pegas alami (m)
b <- 0.1          # Koefisien redaman (damping)

# ===== Fungsi ODE untuk 3D Spring Pendulum =====
spring_pendulum_3d <- function(time, state, params) {
  # state = [x, y, z, vx, vy, vz]
  x  <- state[1]
  y  <- state[2]
  z  <- state[3]
  vx <- state[4]
  vy <- state[5]
  vz <- state[6]

  # Ekstrak parameter
  m_mass <- params["m"]
  k_spring <- params["k"]
  g_grav <- params["g"]
  L0_natural <- params["L0"]
  b_damp <- params["b"]

  # Panjang pegas saat ini
  L <- sqrt(x^2 + y^2 + z^2)

  # Gaya pegas (menuju anchor point di 0,0,0)
  if (L > 0) {
    F_spring_x <- -k_spring * (L - L0_natural) * (x / L)
    F_spring_y <- -k_spring * (L - L0_natural) * (y / L)
    F_spring_z <- -k_spring * (L - L0_natural) * (z / L)
  } else {
    F_spring_x <- 0
    F_spring_y <- 0
    F_spring_z <- 0
  }

  # Gaya gravitasi (hanya di sumbu z)
  F_gravity_z <- -m_mass * g_grav

  # Gaya redaman (friction)
  F_damp_x <- -b_damp * vx
  F_damp_y <- -b_damp * vy
  F_damp_z <- -b_damp * vz

  # Percepatan (Newton's 2nd Law: F = ma)
  ax <- (F_spring_x + F_damp_x) / m_mass
  ay <- (F_spring_y + F_damp_y) / m_mass
  az <- (F_spring_z + F_gravity_z + F_damp_z) / m_mass

  # Return: [dx/dt, dy/dt, dz/dt, dvx/dt, dvy/dt, dvz/dt]
  list(c(vx, vy, vz, ax, ay, az))
}

# ===== Kondisi Awal =====
# Posisi awal: sedikit offset dari equilibrium
x0 <- 0.3
y0 <- 0.2
z0 <- -1.0  # Negatif karena hanging (gravitasi ke bawah)
vx0 <- 0.0
vy0 <- 0.0
vz0 <- 0.0

initial_state <- c(x = x0, y = y0, z = z0, vx = vx0, vy = vy0, vz = vz0)

params <- c(m = m, k = k, g = g, L0 = L0, b = b)

# ===== Solve ODE =====
times <- seq(0, 10, by = 0.01)  # Simulasi 10 detik
solution <- ode(y = initial_state,
                times = times,
                func = spring_pendulum_3d,
                parms = params,
                method = "rk4")

# Konversi ke data frame
df_solution <- as.data.frame(solution)
colnames(df_solution) <- c("time", "x", "y", "z", "vx", "vy", "vz")

# ===== Visualisasi 3D Trajectory =====
cat("Membuat visualisasi 3D trajectory...\n")

open3d()
plot3d(0, 0, 0, type = "s", size = 1.5, col = "red",
       xlim = c(-1, 1), ylim = c(-1, 1), zlim = c(-2.5, 0.5),
       xlab = "X (m)", ylab = "Y (m)", zlab = "Z (m)",
       main = "3D Spring Pendulum Trajectory")

# Plot trajectory
lines3d(df_solution$x, df_solution$y, df_solution$z, color = "blue", lwd = 2)

# Plot posisi awal
points3d(x0, y0, z0, col = "green", size = 8)

# Plot anchor point (0, 0, 0)
points3d(0, 0, 0, col = "orange", size = 10)
text3d(0.1, 0.1, 0.1, "Anchor", color = "orange")

# ===== Plot Time Series =====
par(mfrow = c(3, 2))

# Posisi x
plot(df_solution$time, df_solution$x, type = "l",
     xlab = "Waktu (s)", ylab = "x (m)",
     main = "Posisi X vs Waktu", col = "red", lwd = 1.5)
grid()

# Posisi y
plot(df_solution$time, df_solution$y, type = "l",
     xlab = "Waktu (s)", ylab = "y (m)",
     main = "Posisi Y vs Waktu", col = "green", lwd = 1.5)
grid()

# Posisi z
plot(df_solution$time, df_solution$z, type = "l",
     xlab = "Waktu (s)", ylab = "z (m)",
     main = "Posisi Z vs Waktu", col = "blue", lwd = 1.5)
grid()

# Kecepatan x
plot(df_solution$time, df_solution$vx, type = "l",
     xlab = "Waktu (s)", ylab = "vx (m/s)",
     main = "Kecepatan X vs Waktu", col = "red", lwd = 1.5)
grid()

# Kecepatan y
plot(df_solution$time, df_solution$vy, type = "l",
     xlab = "Waktu (s)", ylab = "vy (m/s)",
     main = "Kecepatan Y vs Waktu", col = "green", lwd = 1.5)
grid()

# Kecepatan z
plot(df_solution$time, df_solution$vz, type = "l",
     xlab = "Waktu (s)", ylab = "vz (m/s)",
     main = "Kecepatan Z vs Waktu", col = "blue", lwd = 1.5)
grid()

par(mfrow = c(1, 1))

# ===== Analisis Energi =====
cat("\n===== ANALISIS ENERGI =====\n")

# Energi Kinetik: KE = 0.5 * m * v²
df_solution$KE <- 0.5 * m * (df_solution$vx^2 + df_solution$vy^2 + df_solution$vz^2)

# Energi Potensial Pegas: PE_spring = 0.5 * k * (L - L0)²
df_solution$L <- sqrt(df_solution$x^2 + df_solution$y^2 + df_solution$z^2)
df_solution$PE_spring <- 0.5 * k * (df_solution$L - L0)^2

# Energi Potensial Gravitasi: PE_grav = m * g * z (dengan referensi di z=0)
df_solution$PE_grav <- m * g * df_solution$z

# Total Energi
df_solution$E_total <- df_solution$KE + df_solution$PE_spring + df_solution$PE_grav

# Plot Energi
plot(df_solution$time, df_solution$KE, type = "l", col = "red",
     xlab = "Waktu (s)", ylab = "Energi (J)",
     main = "Energi vs Waktu", lwd = 2, ylim = c(-5, 5))
lines(df_solution$time, df_solution$PE_spring, col = "green", lwd = 2)
lines(df_solution$time, df_solution$PE_grav, col = "blue", lwd = 2)
lines(df_solution$time, df_solution$E_total, col = "black", lwd = 2.5)
legend("topright", c("KE", "PE (Spring)", "PE (Gravity)", "Total E"),
       col = c("red", "green", "blue", "black"), lwd = 2)
grid()

cat("Energi Total Awal:", df_solution$E_total[1], "J\n")
cat("Energi Total Akhir:", df_solution$E_total[nrow(df_solution)], "J\n")
cat("Selisih Energi:", abs(df_solution$E_total[nrow(df_solution)] - df_solution$E_total[1]), "J\n")

# ===== Statistik =====
cat("\n===== STATISTIK POSISI =====\n")
cat("X: min =", min(df_solution$x), ", max =", max(df_solution$x), "\n")
cat("Y: min =", min(df_solution$y), ", max =", max(df_solution$y), "\n")
cat("Z: min =", min(df_solution$z), ", max =", max(df_solution$z), "\n")

cat("\n===== PERIODE OSILASI =====\n")
# Temukan local maxima untuk estimasi periode
x_signal <- df_solution$x
times_signal <- df_solution$time
peaks <- which(diff(sign(diff(x_signal))) == -2) + 1
if (length(peaks) > 1) {
  periods <- diff(times_signal[peaks])
  avg_period <- mean(periods)
  cat("Periode rata-rata (X):", avg_period, "s\n")
  cat("Frekuensi rata-rata (X):", 1/avg_period, "Hz\n")
}
