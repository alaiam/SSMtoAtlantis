import matplotlib.pyplot as plt
import numpy as np

# Meshgrid for the regular grid


 mx, my = np.meshgrid(r.lon,r.lat, indexing='ij') 
def plot_var(mx,my, new_regular_temp, variable, layer = 1, ts = 350):
  
  import matplotlib.pyplot as plt
  plt.figure(figsize=(10, 10))
  plt.pcolormesh(mx, my, new_regular_temp[ts][layer], cmap='viridis')
  plt.colorbar(label= variable)
  plt.title('Interpolated Variable')
  plt.xlabel('Longitude')
  plt.ylabel('Latitude')
  plt.show()

# Done
plot_var(mx= mx,my = my, new_regular_temp = r.var, variable = "Temperature", layer = 1, ts = 20)

