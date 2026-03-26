import numpy as np
import matplotlib.pyplot as plt

# --- 4 assets ---
mu = np.array([0.05, 0.09, 0.07, 0.16])
Sigma = np.array([
    [0.04, 0.01, 0.012, 0.02],
    [0.01, 0.09, 0.018, 0.025],
    [0.012, 0.018, 0.09, 0.028],
    [0.02, 0.025, 0.028, 0.25]
])

N = len(mu)
ones = np.ones(N)
Sigma_inv = np.linalg.inv(Sigma)

# --- A, B, C, Delta ---
A = ones @ Sigma_inv @ ones
B = ones @ Sigma_inv @ mu
C = mu @ Sigma_inv @ mu
Delta = A*C - B**2

# --- range af expected returns ---
mu_vals = np.linspace(min(mu)*0.5, max(mu)*1.5, 500)

# --- analytisk MV-kurve ---
var_min = (A*mu_vals**2 - 2*B*mu_vals + C)/Delta
sigma_min = np.sqrt(var_min)

# --- GMV punkt ---
mu_gmv = B/A
sigma_gmv = np.sqrt(1/A)

# --- Efficient frontier mask ---
ef_mask = mu_vals >= mu_gmv

# --- Asymptote for std-dev plot ---
slope = np.sqrt(Delta / A)
sigma_asym = np.linspace(0, sigma_min.max(), 200)
mu_asym_pos = mu_gmv + slope * sigma_asym

# --- PLOT 1: Std. Dev version ---
plt.figure(figsize=(9,6))

plt.plot(sigma_min[ef_mask], mu_vals[ef_mask], 'r', linewidth=5, label="Efficient frontier")
plt.plot(sigma_min, mu_vals, 'b--', linewidth=2, label="Min. Variance set")
plt.scatter(sigma_gmv, mu_gmv, c='black', zorder=5, label=r'$x^{MIN}$ Portfolio')  # tilføjet forklaring

# Stiplede linjer til GMV-punkt
plt.vlines(x=sigma_gmv, ymin=0, ymax=mu_gmv, linestyles=':', colors='black', linewidth=1.5)
plt.hlines(y=mu_gmv, xmin=0, xmax=sigma_gmv, linestyles=':', colors='black', linewidth=1.5)

# Tekst på x-aksen lige under grafen
plt.text(sigma_gmv, -0.02, r'$\sqrt{\frac{1}{C}}$', 
         ha='center', va='top', fontsize=18, transform=plt.gca().get_xaxis_transform())

# Tekst på y-aksen lidt til venstre
plt.text(-0.005, mu_gmv, r'$\frac{A}{C}$', ha='right', va='center', fontsize=18)

# Asymptote
plt.plot(sigma_asym, mu_asym_pos, 'g--', linewidth=2, label="Asymptote")

# Akseskalering
plt.xlim(0, sigma_min.max()*1.2)
plt.ylim(mu_vals.min()*0.95, mu_vals.max()*1.05)

# Aksetitler og ticks større
plt.xlabel("Risk (Std. Dev.)", fontsize=16)
plt.ylabel("Expected Return", fontsize=16)
plt.xticks([])
plt.yticks([])

# Legend med forklaring på GMV punkt
plt.legend(fontsize=12)
plt.grid(True)
plt.show()


# --- PLOT 2: Variance version ---
plt.figure(figsize=(9,6))

plt.plot(var_min[ef_mask], mu_vals[ef_mask], 'r', linewidth=5, label="Efficient frontier")
plt.plot(var_min, mu_vals, 'b--', linewidth=2, label="Min. Variance set")

var_gmv = 1/A
plt.scatter(var_gmv, mu_gmv, c='black', zorder=5, label=r'$x^{MIN}$ Portfolio')  # tilføjet forklaring

# Stiplede linjer til GMV-punkt
plt.vlines(x=var_gmv, ymin=0, ymax=mu_gmv, linestyles=':', colors='black', linewidth=1.5)
plt.hlines(y=mu_gmv, xmin=0, xmax=var_gmv, linestyles=':', colors='black', linewidth=1.5)

# Tekst på x-aksen lige under grafen
plt.text(var_gmv, -0.02, r'$\frac{1}{C}$', 
         ha='center', va='top', fontsize=18, transform=plt.gca().get_xaxis_transform())

# Tekst på y-aksen lidt til venstre
plt.text(-0.005, mu_gmv, r'$\frac{A}{C}$', ha='right', va='center', fontsize=18)

# Akseskalering
plt.xlim(0, var_min.max()*1.2)
plt.ylim(mu_vals.min()*0.95, mu_vals.max()*1.05)

# Aksetitler og ticks større
plt.xlabel("Risk (Variance)", fontsize=16)
plt.ylabel("Expected Return", fontsize=16)
plt.xticks([])
plt.yticks([])

# Legend med forklaring på GMV punkt
plt.legend(fontsize=12)
plt.show()