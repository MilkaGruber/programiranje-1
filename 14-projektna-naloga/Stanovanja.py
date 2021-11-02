import csv
import os
import requests
import re

lj_center_frontpage_url = 'https://www.nepremicnine.net/oglasi-prodaja/ljubljana-mesto/stanovanje/'
lj_okolica_frontpage_url = 'https://www.nepremicnine.net/oglasi-prodaja/ljubljana-okolica/stanovanje/'
gorenjska_frontpage_url = 'https://www.nepremicnine.net/oglasi-prodaja/gorenjska/stanovanje/'
juzna_primorska_frontpage_url = 'https://www.nepremicnine.net/oglasi-prodaja/juzna-primorska/stanovanje/'
severna_primorska_frontpage_url = 'https://www.nepremicnine.net/oglasi-prodaja/severna-primorska/stanovanje/'
notranjska_frontpage_url = 'https://www.nepremicnine.net/oglasi-prodaja/notranjska/stanovanje/'
savinjska_frontpage_url = 'https://www.nepremicnine.net/oglasi-prodaja/savinjska/stanovanje/'
podravska_frontpage_url = 'https://www.nepremicnine.net/oglasi-prodaja/podravska/stanovanje/'
koroska_frontpage_url = 'https://www.nepremicnine.net/oglasi-prodaja/koroska/stanovanje/'
dolenjska_frontpage_url = 'https://www.nepremicnine.net/oglasi-prodaja/dolenjska/stanovanje/'
posavska_frontpage_url = 'https://www.nepremicnine.net/oglasi-prodaja/posavska/stanovanje/'
zasavska_frontpage_url = 'https://www.nepremicnine.net/oglasi-prodaja/zasavska/stanovanje/'
pomurska_frontpage_url = 'https://www.nepremicnine.net/oglasi-prodaja/pomurska/stanovanje/'

stanovanja_directory = 'podatki_stanovanja'
frontpage_filename = 'stanovanja.html'
csv_filename = 'stanovanja'

def download_url_to_string(url):
    try:
        page_content = requests.get(url)
    except requests.exceptions.ConnectionError:
        print('Verjetno imaš težave z internetom.')
        return None
    return page_content.text

def save_string_to_file(text, directory, filename):
    os.makedirs(directory, exist_ok=True)
    path = os.path.join(directory, filename)
    with open(path, 'w', encoding='utf-8') as file_out:
        file_out.write(text)
    return None

def read_file_to_string(directory, filename):
    with open(os.path.join(directory, filename), encoding='UTF-8') as input_file:
        return input_file.read()

#lj_center = download_url_to_string(lj_center_frontpage_url)
#lj_okolica = download_url_to_string(lj_okolica_frontpage_url)
#gorenjska = download_url_to_string(gorenjska_frontpage_url)
#juzna_primorska = download_url_to_string(juzna_primorska_frontpage_url)
#severna_primorska = download_url_to_string(severna_primorska_frontpage_url)
#notranjska = download_url_to_string(notranjska_frontpage_url)
#savinjska = download_url_to_string(savinjska_frontpage_url)
#podravska = download_url_to_string(podravska_frontpage_url)
#koroska = download_url_to_string(koroska_frontpage_url)
#doljenska = download_url_to_string(dolenjska_frontpage_url)
#posavska = download_url_to_string(posavska_frontpage_url)
#zasavska = download_url_to_string(zasavska_frontpage_url)
#pomurska = download_url_to_string(pomurska_frontpage_url)
#
#save_string_to_file(lj_center, stanovanja_directory, frontpage_filename)
#save_string_to_file(lj_okolica, stanovanja_directory, frontpage_filename)
#save_string_to_file(gorenjska, stanovanja_directory, frontpage_filename)
#save_string_to_file(juzna_primorska, stanovanja_directory, frontpage_filename)
#save_string_to_file(severna_primorska, stanovanja_directory, frontpage_filename)
#save_string_to_file(notranjska, stanovanja_directory, frontpage_filename)
#save_string_to_file(savinjska, stanovanja_directory, frontpage_filename)
#save_string_to_file(podravska, stanovanja_directory, frontpage_filename)
#save_string_to_file(koroska, stanovanja_directory, frontpage_filename)
#save_string_to_file(doljenska, stanovanja_directory, frontpage_filename)
#save_string_to_file(posavska, stanovanja_directory, frontpage_filename)
#save_string_to_file(zasavska, stanovanja_directory, frontpage_filename)
#save_string_to_file(pomurska, stanovanja_directory, frontpage_filename)
