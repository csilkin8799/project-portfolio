import re
import numpy as np
import pandas as pd
from pdfminer.high_level import extract_pages, extract_text

pdf_text = "C:/Users/Student/Desktop/Personal Stuff/NFL Draft Project/The Beast/beast_2019.pdf"
#for page_layout in extract_pages(pdf_text, password="draftguide2019"):
 #   for element in page_layout:
  #      print(element)

text = extract_text(pdf_text, password = "draftguide2019")

pattern = re.compile(r"[A-Z]+.*[A-Z]+\.*\s{1}\|{1}") # Correct pattern

matches = pattern.findall(text)

print(matches)

player_data = pd.DataFrame(matches, columns=["Name"])
player_data["Initial Info"] = ""

for i in range(len(matches)):
    start_string = player_data["Name"][i]
    if i == len(matches) - 1:
        player_data['Initial Info'][i] = text.split(start_string)[1]
    else:
        end_string = player_data["Name"][i+1]
        init_text = text.split(start_string)[1]
        
        player_data['Initial Info'][i] = init_text.split(end_string)[0]

player_data["Name"] = player_data["Name"].str[:-2]

def extract_text_between_words(input_text, start_word, end_word):
    pattern = re.compile(fr'{re.escape(start_word)}(.*?){re.escape(end_word)}', re.DOTALL)
    match = pattern.search(input_text)
    
    if match:
        return match.group(1)
    else:
        return None

player_data["Strengths"] = ""

player_data["Strengths"] = player_data["Initial Info"].apply\
    (lambda x: extract_text_between_words(x, "STRENGTHS:", "WEAKNESSES:"))

player_data["Weaknesses"] = player_data["Initial Info"].apply\
    (lambda x: extract_text_between_words(x, "WEAKNESSES:", "SUMMARY:"))

player_data["Summary"] = player_data["Initial Info"].apply\
    (lambda x: extract_text_between_words(x, "SUMMARY:", "GRADE:"))
    
player_data["Brugler Grade"] = player_data["Initial Info"].apply\
    (lambda x: extract_text_between_words(x, "GRADE:", "\n\n"))