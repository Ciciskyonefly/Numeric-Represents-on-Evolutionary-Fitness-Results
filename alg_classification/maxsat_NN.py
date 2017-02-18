# -*- coding: utf-8 -*-
"""
Created on Mon Nov 21 09:03:01 2016

@author: Cici
"""

from os import listdir
import pandas as pd

path = "./maxsat_dataset/"
files = os.listdir(path)

use_to_cols = ["a", "b", "c", "d", "residuals", "instance_file"]

data = []
for file in files:
    tempdata = pd.read_csv(os.path.join(path, file), usecols = use_to_cols) 
    data.append(tempdata)

print pd.concat(data)


#frames = [pd.read_csv(os.path.join(path, file), usecols = use_to_cols) for file in files]
#pd.concat(frames)