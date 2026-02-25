# lakecast_rotorua

This is a working repository for data storage and forecast production related to the MBIE-funded SmartIdeas project titled LakeCast: Enhancing lake ecosystem management with multisource integrated data and ecological forecasts.

Primary data sources at Lake Rotorua include
- **Central buoy data**: Automated high-frequency profiles of water quality data from the central monitoring buoy managed by Limnotrack
  - data include water temperature, dissolved oxygen, pH, turbidity, chlorophyll-a fluorescence, and phycocyanin fluorescence
      -profiles occur ~every two hours and taking readings from the surface to ~20m
  - data cover the period from February 21, 2022 to present
  - data are owned by the Bay of Plenty Regional Council and managed by Limnotrack. With the appropriate API key, data can be downloaded using the function in this repo at `scripts/functions/get_lake_wqprofilers_data.R` by specifying the site name ('rotorua').
- **Nearshore buoy data**: High frequency sensor data measured near-surface at two nearshore sites (Wharekura Jetty and Mataikotare Marae)
  - data at both sites include water temperature, dissolved oxygen, pH, oxidation reduction potential, specific conductance, turbidity, chlorophyll-a fluorescence, and phycocyanin fluorescence
      - measurements are taken at the surface (~0.3m) every 15 minutes
  - data cover the period from ~December 2025 to present
  - data are owned by the LakeCast project and managed by Limnotrack. With the appropriate API key, data can be downloaded using the function in this repo at `scripts/functions/get_lake_wqprofilers_data.R` by specifying the site name ('mataikotare' or 'wharekura').
- **TALT FluoroQuik data**: Tri-weekly manual data collected by Te Arawa Lakes Trust at 10 shoreline sites
  - data include FluoroQuik measurements of chlorophyll-a and phycocyanin, as well as ProDSS measurements of water temperature, dissolved oxygen, turbidity and chlorophyll-a.
  - data cover the period from January 20, 2025 to present
  - data are available in this repository at `data/talt_cyano` under the most recent file name. Data are periodically updated.
- **BOPRC Cyano data**: Weekly Cyanoacterial Monitoring data collected by the Bay of Plenty Regional Council from ~November to May
  - data include information on biovolume of cyanobacteria at four shoreline sites, including whether species are potentially toxin-producing
  - data cover the period from 2015 to present
  - data are available in this repository at `data/boprc_cyano/boprc_cyano_latest.csv`. Data are manually added as they become available and automatically integrated into the file.


