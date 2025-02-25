# Cargar librerías necesarias
library(ggplot2)
library(dplyr)

# Parámetros de la simulación
alpha <- 0.05  # Nivel de significancia
n_values <- c(20, 30, 40, 50, 60, 70, 80, 90, 100, 200, 300, 400, 500, 600, 700, 800)  # Tamaños muestrales
effect_sizes <- c(0.2, 0.5, 0.8)  # Tamaños del efecto (pequeño, mediano, grande)

# Crear un data frame vacío para almacenar los resultados
power_results <- expand.grid(n = n_values, d = effect_sizes)
power_results$power <- NA  # Inicializar la columna de potencia

# Calcular la potencia para cada combinación de tamaño muestral y tamaño del efecto
for (i in 1:nrow(power_results)) {
  power_results$power[i] <- power.t.test(n = power_results$n[i], 
                                         delta = power_results$d[i], 
                                         sd = 1, 
                                         sig.level = alpha, 
                                         type = "two.sample", 
                                         alternative = "two.sided")$power
}

# Convertir el tamaño de muestra en factor para etiquetar curvas
power_results$n <- factor(power_results$n, levels = n_values, 
                          labels = paste0("n=", n_values))

# Generar el gráfico con ggplot2
plot_potencia <- ggplot(power_results, aes(x = d, y = power, color = n, group = n)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  geom_hline(yintercept = 0.8, linetype = "dashed", color = "black") + 
  labs(title = "Potencia Estadística en función del Tamaño del Efecto",
       subtitle = expression(paste("Niveles de significancia ", alpha, " = 0.05")),
       x = "Tamaño del Efecto (d)",
       y = "Potencia",
       color = "Tamaño Muestral") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
        plot.subtitle = element_text(hjust = 0.5, size = 12),
        legend.position = "bottom")

# Mostrar el gráfico
print(plot_potencia)

