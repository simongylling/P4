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

rf = 0.04  # risikofri rente

# --- Tangency portfolio ---
excess_mu = mu - rf
w_tan_unnormalized = Sigma_inv @ excess_mu
w_tan = w_tan_unnormalized / (ones @ w_tan_unnormalized)

mu_tan = w_tan @ mu
sigma_tan = np.sqrt(w_tan @ Sigma @ w_tan)

# --- range af expected returns ---
mu_vals = np.linspace(min(mu)*0.5, max(mu)*1.5, 500)

# --- analytisk MV-kurve ---
var_min = (A*mu_vals**2 - 2*B*mu_vals + C)/Delta
sigma_min = np.sqrt(var_min)

# --- CML ---
sigma_cml = np.linspace(0, sigma_min.max(), 200)
slope_cml = (mu_tan - rf) / sigma_tan
mu_cml = rf + slope_cml * sigma_cml

# --- GMV punkt ---
mu_gmv = B/A
sigma_gmv = np.sqrt(1/A)

# --- Efficient frontier ---
ef_mask = mu_vals >= mu_gmv
# Også ineff til pænere plot
inef_mask = mu_vals < mu_gmv

# --- PLOT ---
plt.figure(figsize=(9,6))

# Efficient frontier (øverste del)
plt.plot(sigma_min[ef_mask], mu_vals[ef_mask], 'r', c='red', linewidth=3, label="Efficient frontier")

# Inefficient del (kun den nederste del!)
plt.plot(sigma_min[inef_mask], mu_vals[inef_mask], 'b--', linewidth=2, label="Min. Variance set")

# GMV punkt
plt.scatter(sigma_gmv, mu_gmv, c='black', s=80, zorder=5, label = r'$x^{MIN}$ Portfolio')

# CML
plt.plot(sigma_cml, mu_cml, 'g', linewidth=1, label="CML")

# Risk-free
plt.scatter(0, rf, c='purple', s=80, zorder=5, label="Risk-free rate")

# Tangency
plt.scatter(sigma_tan, mu_tan, c='green', s=80, zorder=6, label="Tangency portfolio")

# Akser
plt.xlim(-0.008, sigma_min.max()*1.2)
plt.ylim(mu_vals.min()*0.95, mu_vals.max()*1.05)

plt.xlabel("Risk (Std. Dev.)", fontsize=16)
plt.ylabel("Expected Return", fontsize=16)
plt.xticks([])
plt.yticks([])


# Stiplede linjer til GMV-punkt
plt.vlines(x=sigma_gmv, ymin=0, ymax=mu_gmv, linestyles=':', colors='black', linewidth=1.5)
plt.hlines(y=mu_gmv, xmin=0, xmax=sigma_gmv, linestyles=':', colors='black', linewidth=1.5)

#Tekst til punkterne
plt.text(0, rf, r'$r$', 
         ha='right', va='bottom', fontsize=14)
# Tekst på x-aksen lige under grafen
plt.text(sigma_gmv, -0.02, r'$\sqrt{\frac{1}{C}}$', 
         ha='center', va='top', fontsize=18, transform=plt.gca().get_xaxis_transform())

# Tekst på y-aksen lidt til venstre
plt.text(-0.005, mu_gmv, r'$\frac{A}{C}$', ha='right', va='center', fontsize=18)

plt.text(sigma_tan + 0.01, mu_tan - 0.01, "Tangency portfolio",
         ha='left', va='bottom', fontsize=12)


plt.legend(fontsize=12)
plt.grid(True)

plt.show()