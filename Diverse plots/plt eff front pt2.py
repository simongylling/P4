import numpy as np
import matplotlib.pyplot as plt

# --- 4 assets ---
mu = np.array([0.05, 0.09, 0.13, 0.17])
Sigma = np.array([
    [0.04, 0.01, 0.015, 0.02],
    [0.01, 0.09, 0.02, 0.025],
    [0.015, 0.02, 0.16, 0.03],
    [0.02, 0.025, 0.03, 0.25]
])
N = len(mu)

# --- generer long-only simplex grid ---
grid_size = 200
grid = np.linspace(0, 1, grid_size)
weights = []

for w1 in grid:
    for w2 in grid:
        for w3 in grid:
            w4 = 1 - w1 - w2 - w3
            if w4 >= 0:
                weights.append([w1, w2, w3, w4])
weights = np.array(weights)

# --- beregn return og VARIANS ---
returns = weights @ mu
variances = np.einsum('ij,jk,ik->i', weights, Sigma, weights)

# --- find constrained minimum-variance frontier ---
mu_targets = np.linspace(min(mu), max(mu), 200)
mv_variances = []

for r in mu_targets:
    mask = np.isclose(returns, r, atol=0.001)
    if np.any(mask):
        mv_variances.append(np.min(variances[mask]))
    else:
        mv_variances.append(np.nan)

mv_variances = np.array(mv_variances)

# --- GMV (long-only) ---
min_idx = np.nanargmin(mv_variances)
var_gmv = mv_variances[min_idx]
mu_gmv = mu_targets[min_idx]

# --- Efficient frontier = øvre del ---
ef_mask = mu_targets >= mu_gmv

# --- Plot ---
plt.figure(figsize=(9,6))

# Feasible set
plt.scatter(variances, returns, s=3, alpha=0.3, label="Feasible set")

# Minimum variance set
plt.plot(mv_variances, mu_targets, 'b--', linewidth=1, label="Minimum Variance set")

# Efficient frontier
plt.plot(mv_variances[ef_mask], mu_targets[ef_mask], 'r', linewidth=3, label="Efficient frontier")

# GMV punkt
plt.scatter(var_gmv, mu_gmv, c='black', label=r'$x^{MIN}$', zorder=5)

# Single asset points (varians på x-akse)
plt.scatter(np.diag(Sigma), mu, c='orange', s=50, label="Single assets")

plt.xlabel("Risk (Variance)")
plt.ylabel("Expected Return")
plt.title("Feasible Set and Efficient Frontier (Variance)")
plt.legend()
plt.grid(True)
plt.show()