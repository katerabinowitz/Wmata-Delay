import pandas as pd
import numpy as np
from bs4 import BeautifulSoup as Soup, Tag
import requests

response = requests.get('http://www.wmata.com/rail/service_reports/viewReportArchive.cfm')
soup = Soup(response.content)

wmata = soup.find("ul", {"class": "links2"})
date_links = ["http://www.wmata.com/rail/service_reports/" + li.a["href"] for li in wmata.findAll("li")]

Incidents=[]
Incident=[]
Date=[]
DateN=[]
d=pd.DataFrame()

for link in date_links:
	result = requests.get(link)
	c = result.content
	soup = Soup(c)
	Incident=soup.find("div", {"class":"internal-box2-inner"}).findAll('p')
	for element in Incident:
 		Incidents.append(element.get_text().encode('utf8'))
	Date=soup.find("h1").get_text().encode('utf8')
	length=len(Incident)
	DateN=Date*length
	data=pd.DataFrame({'Date':DateN, 'Incident':Incidents})
	d=d.append(data)

d.to_csv('WMATAService.csv')
