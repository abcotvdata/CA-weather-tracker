import requests
from bs4 import BeautifulSoup
import time
import urllib
import re
from selenium import webdriver
from selenium.webdriver.common.by import By
from selenium.webdriver.support.ui import WebDriverWait
from selenium.webdriver.support import expected_conditions as EC
import selenium.webdriver.common.keys
from selenium.webdriver.common.keys import Keys
from selenium.webdriver.common.action_chains import ActionChains
from webdriver_manager.chrome import ChromeDriverManager
from webdriver_manager.core.utils import ChromeType
#from webdriver_manager.core.os_manager import ChromeType
from selenium.webdriver import Chrome
from selenium.webdriver.chrome.options import Options
from selenium.webdriver.chrome.service import Service

chrome_service = Service(ChromeDriverManager(chrome_type=ChromeType.CHROMIUM).install())

chrome_options = Options()
options = [
    "--headless",
    "--disable-gpu",
    "--window-size=1920,1200",
    "--ignore-certificate-errors",
    "--disable-extensions",
    "--no-sandbox",
    "--disable-dev-shm-usage"
]
for option in options:
    chrome_options.add_argument(option)

import pandas as pd
import plotly.express as px
import pandas as pd
from datetime import datetime, timedelta
import plotly.graph_objects as go
import plotly.io as pio
import numpy as np
from datetime import date
import os

today = date.today()
today_str = str(today)
print(today_str)

month = int(today.strftime("%m"))
year = int(today.strftime("%Y"))

today_month_str = str(today.strftime("%m"))
today_year_str = str(today.strftime("%Y"))

today_first_of_month = today_year_str + "-" + today_month_str + "-" + "01"

print(today_first_of_month)

this_month_str = str(today.strftime("%Y-%m"))

print(this_month_str)

#print(month)
#print(year)


if month==1:
    last_month_year = str(year-1)
else:
    last_month_year = str(year)


last_month = str(month-1).zfill(2)


#print(last_month_year)
#print(last_month)

last_month_year_str = last_month_year + "-" + last_month
print(last_month_year_str)

#scrape last month
url = "https://xmacis.rcc-acis.org/"
driver = webdriver.Chrome(service=chrome_service, options=chrome_options)
driver.get(url)

#page = driver.execute_script("return document.documentElement.outerHTML")

multistation = driver.find_element(By.XPATH, '//*[@id="product_select"]/div[2]')
multistation.click()

time.sleep(1)

monthlydata = driver.find_element(By.XPATH, '//*[@id="ui-id-18"]')
monthlydata.click()

time.sleep(1)

csv = driver.find_element(By.XPATH, '//*[@id="outformat_csv"]')
csv.click()

time.sleep(1)


date = driver.find_element(By.ID, "tDatepicker")
date.click()
date.clear()
date.send_keys(Keys.RETURN)
time.sleep(1)
date.send_keys(last_month_year_str)
time.sleep(1)
date.send_keys(Keys.RETURN)

time.sleep(1)

variable = driver.find_element(By.XPATH, '//*[@id="element_area"]/div[1]/fieldset[1]/select')
variable.click()

time.sleep(1)

precip = driver.find_element(By.XPATH, '//*[@id="element_area"]/div[1]/fieldset[1]/select/option[4]')
precip.click()

time.sleep(1)

moreoptions = driver.find_element(By.XPATH, '//*[@id="option_select"]/div[4]/span[2]')
moreoptions.click()

time.sleep(1)

coords = driver.find_element(By.XPATH, '//*[@id="stacrds"]')
coords.click()

time.sleep(1)

countybox = driver.find_element(By.XPATH, '//*[@id="stacnty"]')
countybox.click()

time.sleep(1)

stationselect = driver.find_element(By.XPATH, '//*[@id="station_acc"]')
stationselect.click()

time.sleep(1)

stateselect = driver.find_element(By.XPATH, '//*[@id="acis-state_select_type"]/label[1]')
stateselect.click()

time.sleep(1)

statedrop = driver.find_element(By.XPATH, '//*[@id="state"]')
statedrop.click()

time.sleep(1)

california = driver.find_element(By.XPATH, '//*[@id="state"]/option[5]')
california.click()

time.sleep(1)

go = driver.find_element(By.XPATH, '//*[@id="go"]/span')
go.click()

time.sleep(1)

wait = WebDriverWait(driver, 60)
wait.until(EC.visibility_of_element_located((By.XPATH, '//*[@id="results_area"]/pre')))

time.sleep(1)

page = driver.execute_script("return document.documentElement.outerHTML")

driver.quit()
soup = BeautifulSoup(page)

results = soup.find('div', {'id':'results_area'}).find('pre')

results = str(results)

results = results.replace("<pre>", "")
results = results.replace("<br/>", "\n")

print(results)

filename = "CSVs/precip_sum_" + last_month_year_str + ".csv"

with open(filename,'w') as f:
    f.write('')

with open(filename,'a') as f:
    f.write(results)
    
url = "https://xmacis.rcc-acis.org/"
driver = webdriver.Chrome(service=chrome_service, options=chrome_options)
driver.get(url)

#page = driver.execute_script("return document.documentElement.outerHTML")

multistation = driver.find_element(By.XPATH, '//*[@id="product_select"]/div[2]')
multistation.click()

time.sleep(1)

daterange = driver.find_element(By.XPATH, '//*[@id="ui-id-19"]')
daterange.click()

time.sleep(1)

csv = driver.find_element(By.XPATH, '//*[@id="outformat_csv"]')
csv.click()

time.sleep(1)

#scrape this month

startdate = driver.find_element(By.ID, "sDatepicker")
startdate.click()
startdate.clear()
startdate.send_keys(Keys.RETURN)
time.sleep(1)
startdate.send_keys(today_first_of_month)
time.sleep(1)
startdate.send_keys(Keys.RETURN)

enddate = driver.find_element(By.ID, "eDatepicker")
enddate.click()
enddate.clear()
enddate.send_keys(Keys.RETURN)
time.sleep(1)
enddate.send_keys(today_str)
time.sleep(1)
enddate.send_keys(Keys.RETURN)

time.sleep(1)

variable = driver.find_element(By.XPATH, '//*[@id="element_area"]/div[1]/fieldset[1]/select')
variable.click()

time.sleep(1)

precip = driver.find_element(By.XPATH, '//*[@id="element_area"]/div[1]/fieldset[1]/select/option[4]')
precip.click()

time.sleep(1)

moreoptions = driver.find_element(By.XPATH, '//*[@id="option_select"]/div[4]/span[2]')
moreoptions.click()

time.sleep(1)

coords = driver.find_element(By.XPATH, '//*[@id="stacrds"]')
coords.click()

time.sleep(1)

countybox = driver.find_element(By.XPATH, '//*[@id="stacnty"]')
countybox.click()

time.sleep(1)

stationselect = driver.find_element(By.XPATH, '//*[@id="station_acc"]')
stationselect.click()

time.sleep(1)

stateselect = driver.find_element(By.XPATH, '//*[@id="acis-state_select_type"]/label[1]')
stateselect.click()

time.sleep(1)

statedrop = driver.find_element(By.XPATH, '//*[@id="state"]')
statedrop.click()

time.sleep(1)

california = driver.find_element(By.XPATH, '//*[@id="state"]/option[5]')
california.click()

time.sleep(1)

go = driver.find_element(By.XPATH, '//*[@id="go"]/span')
go.click()

time.sleep(1)

wait = WebDriverWait(driver, 60)
wait.until(EC.visibility_of_element_located((By.XPATH, '//*[@id="results_area"]/pre')))

time.sleep(1)

page = driver.execute_script("return document.documentElement.outerHTML")

driver.quit()
soup = BeautifulSoup(page)

results = soup.find('div', {'id':'results_area'}).find('pre')

results = str(results)

results = results.replace("<pre>", "")
results = results.replace("<br/>", "\n")

print(results)

filename = "CSVs/precip_sum_" + this_month_str + ".csv"

with open(filename,'w') as f:
    f.write('')

with open(filename,'a') as f:
    f.write(results)
