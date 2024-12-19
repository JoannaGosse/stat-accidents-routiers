import pandas as pd
import os
from pathlib import Path
import requests

        
def collect_data():
    
    #Création du fichier de stockage des données si besoin
    data_folder_path = Path(os.getcwd()) / "data"
    
    if data_folder_path.exists() is False:
        os.makedirs(data_folder_path)
        
    links=pd.read_csv("./download_links.csv", index_col="annee",delimiter=";")
    
            
    for index, row in links.iterrows():  # Parcourt chaque ligne
        for col in links.columns:  # Parcourt chaque colonne
            data_file_path = data_folder_path / f"{str(index)}_{str(col)}.csv"
            
            if data_file_path.exists() is False:
                # Download file
                print(f"Downloading {str(index)}_{str(col)}.csv from data.gouv.fr")
                url = row[col]
                print("https://www.data.gouv.fr/fr/datasets/r/"+url)
                r = requests.get(
                    url="https://www.data.gouv.fr/fr/datasets/r/"+url)
                    
                with open(data_file_path, "wb") as file:
                    file.write(r.content)
            else :  print(f"{str(index)}_{str(col)}.csv from data.gouv.fr is already download.")

collect_data()




















