#!/usr/bin/python3
'''
Run as ./translation_extract.py <input file path>
Output: A text file - translation_text.txt - with text inside the i18n$t() function - requiring translation.
'''
import re, sys

def returnMatch(text):
  return re.findall(r'i18n\(\)\$t\((".*?")\)', text)

if __name__ == '__main__':
  try:
    filename = sys.argv[1]
  except:
    print ("Missing Filename")
    sys.exit(-1)
    
  with open(filename, 'r', encoding = 'latin-1') as f:
    filetext = f.read()
    
  outtext = returnMatch(filetext)
  
  with open('translation_text.txt', 'w', encoding = 'latin-1') as f:
    for item in list(set(outtext)):
        f.write("%s\n" % item)
    
