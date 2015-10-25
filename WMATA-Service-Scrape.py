import pandas as pd
import numpy as np
from bs4 import BeautifulSoup as Soup, Tag
import requests

response = requests.get('http://www.wmata.com/rail/service_reports/viewReportArchive.cfm')
soup = Soup(response.content)

wmata = soup.find("ul", {"class": "links2"})
category_links = ["http://www.wmata.com/rail/service_reports/" + li.a["href"] for li in wmata.findAll("li")]

Incidents=[]
Incident=[]
Date=[]
DateN=[]
links=category_links[1:5]
d=[]

for link in links:
	result = requests.get(link)
	c = result.content
	soup = Soup(c)
	Incident.append(soup.find("div", {"class":"internal-box2-inner"}).findAll('p'))
	for element in Incident:
 		Incidents.append(element.get_text())
	Date.append(soup.find("h1").get_text())
	length=len(Incidents)

DF=pd.DataFrame({'Date':DateN, 'Incident':Incidents})
print DF