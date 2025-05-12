#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Wed Apr 23 14:42:41 2025

@author: krein21
"""
# %% Import modules

import requests
import numpy as np
import pandas as pd
from netCDF4 import Dataset
import CCMMF_Irrigation_CalcVis
import os
import ee

ee.Initialize()


# %% Download GEE OPEN ET Data

def GEEOpenET(START_DATE, END_DATE, LAT, LON):
    '''THIS DOES NOT RUN AT THE MOMENT'''
    
    # Access OpenET dataset
    collection = ee.ImageCollection("OpenET/ENSEMBLE/CONUS/GRIDMET/MONTHLY/v2_0") \
    .filterDate(START_DATE, END_DATE) \
    .filterBounds(ee.Geometry.Point([LON, LAT]))

    # Extract et time series
    def extract_et(img):
        date = img.date().format()
        et = img.reduceRegion(ee.Reducer.first(), ee.Geometry.Point([LON, LAT]), 1000).get('et_ensemble_mad')
        return ee.Feature(None, {'date': date, 'et': et})

    et_series = collection.map(extract_et)
    
    # Convert data to df
    et_series = et_series.getInfo()  # Convert from ee.List to Python list
    print(et_series)
    print(type(et_series))
    open_et_df = pd.DataFrame(et_series)
    open_et_df['date'] = pd.to_datetime(open_et_df['date'])
    
    print(open_et_df)
    
    return open_et_df

# %% Request OPEN ET Data (from website)

def OpenETData(START_DATE, END_DATE, LAT, LON):
    
    # Set directory
    working_dir = '/projectnb/dietzelab/ccmmf/management/irrigation/'
    os.chdir(working_dir)
    
    # Read in API Key
    with open('OpenETAPIKey.txt', 'r') as file:
        api_key = file.readline()

    header = {"Authorization": api_key}
    
    # endpoint arguments
    args = {
      "date_range": [START_DATE, END_DATE],
      "interval": "daily",
      "geometry": [LON,LAT],
      "model": "Ensemble",
      "variable": "ET",
      "reference_et": "gridMET",
      "units": "mm",
      "file_format": "JSON"
    }
    
    # query the api 
    resp = requests.post(
        headers=header,
        json=args,
        url="https://openet-api.org/raster/timeseries/point"
    )
    
    # Parse the JSON response
    et_data = resp.json()
    
    open_et_df = pd.DataFrame(et_data)
    open_et_df['time'] = pd.to_datetime(open_et_df['time'])
    
    return open_et_df

# %% Download CHIRPS Data

def CHIRPSData(YEAR, LAT, LON):
    
    # Set URL and file name
    url = f'https://data.chc.ucsb.edu/products/CHIRPS-2.0/global_daily/netcdf/p05/chirps-v2.0.{YEAR}.days_p05.nc'
    destfile = f'/projectnb/dietzelab/ccmmf/management/irrigation/chirps-v2.0.{YEAR}.days_p05.nc'
    
    # Check if the file already exists before downloading
    if not os.path.exists(destfile):
        print(f"{destfile} not found. Downloading now...")
        response = requests.get(url, timeout=600)
        
        with open(destfile, 'wb') as f:
            f.write(response.content)
    
    # Open the NetCDF file
    nc_data = Dataset(destfile, 'r')
    
    # Print metadata for precipitation
    #precip_variable = nc_data.variables['precip']
    #print(precip_variable)
    
    # Extract coordinate variables
    lon = nc_data.variables['longitude'][:]
    lat = nc_data.variables['latitude'][:]
    
    # Find the nearest lat/lon index
    lon_idx = np.abs(lon - LON).argmin()
    lat_idx = np.abs(lat - LAT).argmin()
    
    # Extract the data just for that lat lon
    precip_data = nc_data.variables['precip'][:, lat_idx, lon_idx]
    
    # Close the NetCDF file when done
    nc_data.close()
    
    # Clean data
    precip_data = precip_data.filled(np.nan)
    precip_data_df = pd.DataFrame(precip_data)
    
    return precip_data_df

# %% Calculate and visualize new data

def new_data_entry_API(LAT, LON, years, csv_folder, START_DATE = None, END_DATE = None):
    print(f'{LAT} {LON} {years}')
    
    # Define start and end date
    if START_DATE == None or END_DATE == None:
        START_DATE = f'{years[0]}-01-01'
        END_DATE = f'{years[-1]}-12-31'
    
    # Download open et data
    et_df = OpenETData(START_DATE, END_DATE, LAT, LON)
    
    # Download CHIRPS data year by year and concatenate
    precip_data = pd.DataFrame()
    for year in years:
        precip_data_year = CHIRPSData(year, LAT, LON)
        precip_data = pd.concat([precip_data, precip_data_year], ignore_index=True)
    
    # Organize and water balance
    df_water_balance = CCMMF_Irrigation_CalcVis.water_balance(et_df, precip_data, LAT, LON)
    
    # Graph
    df_water_balance['time'] = pd.to_datetime(df_water_balance['time'])
    for year in years:
        CCMMF_Irrigation_CalcVis.timeseries_graphs(df_water_balance[df_water_balance['time'].dt.year == year], LAT, LON, year)
    
    # Save to csv to ensure data is stored
    filename = f'{csv_folder}CCMMR_Water_Balance_{LAT}_{LON}.csv'
    df_water_balance.to_csv(filename, index=False)
    return df_water_balance