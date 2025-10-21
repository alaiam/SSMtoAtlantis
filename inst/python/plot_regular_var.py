import matplotlib.pyplot as plt


def plot_var(mx,my, new_regular_temp, variable, layer = 1, ts = 350):
  import matplotlib.pyplot as plt
  plt.figure(figsize=(10, 10))
  plt.pcolormesh(mx, my, new_regular_temp[ts][layer], cmap='viridis')
  plt.colorbar(label= variable)
  plt.title('Interpolated Variable')
  plt.xlabel('Longitude')
  plt.ylabel('Latitude')
  plt.show()
