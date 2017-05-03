#!/usr/bin/python 
import math
import itertools
import json
import sys
import pprint
import numpy as np
from sklearn.preprocessing import scale, RobustScaler


f = open(sys.argv[1],'rb');
data = json.load(f)
#pprint.pprint( data)

meta = map(lambda ent: [ent["addr"],ent["name"]],data)
values = map(lambda ent: float(ent["fkt"]["size"]),data)
values = np.reshape(values,(-1, 1))
if int(sys.argv[3]) > 0:
    scaler = RobustScaler()
    values = scaler.fit_transform(values)
    #values = scale(values, with_mean = False)

data = map(lambda ent: [ent[0][0],ent[0][1],ent[1][0]],zip(meta,values))

with open(sys.argv[2],'wb') as g:
    json.dump(data,g)

