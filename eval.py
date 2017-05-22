#!/usr/bin/python

import os
import sys
import random


smallscale = 100
medscale = 200
crossscale = 300

doom_files = [
"games/doom/00_doom02.exe"
,"games/doom/01_doom03.exe" 
,"games/doom/02_doom04.exe"
,"games/doom/03_doom05.exe" 
,"games/doom/04_doomp.exe"
,"games/doom/05_doom10s.exe"
,"games/doom/06_doom11s.exe" 
,"games/doom/07_doom12s.exe"
,"games/doom/08_doom125ssybex.exe" 
,"games/doom/09_doom14bl.exe"
,"games/doom/10_doom14bs.exe" 
,"games/doom/11_doom15bs.exe"
,"games/doom/12_doom16b.exe" 
,"games/doom/13_doom1666.exe"
,"games/doom/14_doom18s.exe" 
,"games/doom/15_doom19s.exe" 
]

allalignments = ["nflen","flen","anflen","aflen","bnflen","bflen","cnflen","cflen","dnflen","dflen","truth"] 
chosenone = "dflen"
def stdeval(chosenone=chosenone, allalignments=allalignments, crossscale=crossscale, medscale=medscale, smallscale=smallscale, overallbest = "dflen"):
	return {
        "F1":    [ 
            ([chosenone],("cross",crossscale)) , # inspection F1 
            (["truth"],("cross",crossscale)) , # inspection F1 
            ([overallbest],("cross",crossscale)) , # inspection F1 
            (allalignments,("cross",smallscale)) ]      # selection F1 boxplot
        ,"recall" : [ 
(["truth"],("cross",crossscale))
]
        ,"precision" : [ 
([chosenone],("cross",crossscale))
,([overallbest],("cross",crossscale))
,(allalignments,("cross",smallscale)) 
 ]
        ,"similarity" : [ (["truth"],("cross",crossscale)) ]
        ,"potential" : [ (["truth"],("cross",crossscale)) ]
        ,"F1a":    [ 
            (allalignments,("cross",smallscale)) ]      # selection F1 boxplot
        #,"corD": [ (allalignments,("cross",smallscale)) ] 
        ,"cor":  [ 
            ([overallbest],("cross",crossscale)) ,
            ([chosenone],("cross",crossscale)) ,
            (["truth"],("cross",crossscale)) 
            ] # analysis selected similarities and measures
        }

testsuite = [
{"name": "pokemongo", "files":[
"pokemongo/classes_0290",
"pokemongo/classes_0292",
"pokemongo/classes_0293",
    ], "types":{
    "align_page":[ 
        ("flen","ref",("cross",None),"verb"), ("flen","ref",("line",None),"verb") 
        ,("nflen","ref",("cross",None),"verb"), ("nflen","ref",("line",None),"verb") 
        ]
    ,"pairs":[ 
        ("flen","ref",("cross",None),"mis") 
        ,("nflen","ref",("cross",None),"mis")
        ,("dflen","ref",("cross",None),"mis")
        ,("truth","ref",("cross",None),"mis")
        ]
        }},
        
        {"name": "pokemongo", "files":[
"pokemongo/classes_0290",
"pokemongo/classes_029x",
"pokemongo/classes_0292",
"pokemongo/classes_0293",
    ], "types":{
    "page":[("ref","verb")]
    ,"plot":[ ("ref","verb")]
    ,"align_page":[ 
        ("flen","ref",("cross",None),"verb"), ("flen","ref",("line",None),"verb") 
        ,("nflen","ref",("cross",None),"verb"), ("nflen","ref",("line",None),"verb") 
        ]
    ,"pairs":[ 
        ("flen","ref",("cross",None),"mis") ,("nflen","ref",("cross",None),"mis")
        ,("dflen","ref",("cross",None),"mis")
        ,("truth","ref",("cross",None),"mis")
        ]
        }},
{"name": "putty", "files":[
"malputty/putty064.exe",
"malputty/MalPutty.exe",
"malputty/putty063.exe"
    ], "types":{
    "page":[("ref","verb")]
    ,"plot":[ ("ref","verb") ,("ref","a") ,("ref","b") ,("ref","c") ,("ref","d") ,("ref","e") ,("ref","f") ]
    ,"align_page":[ 
        ("flen","ref",("cross",None),"verb"), ("flen","ref",("line",None),"verb") 
        ,("nflen","ref",("cross",None),"verb"), ("nflen","ref",("line",None),"verb") 
        ,("flen","ref",("cross",None),"mis"), ("flen","ref",("line",None),"mis") 
        ,("nflen","ref",("cross",None),"mis"), ("nflen","ref",("line",None),"mis") 
        ]
    ,"pairs":[ 
        ("flen","ref",("line",None),"verb") ,("nflen","ref",("line",None),"verb"),
        ("flen","ref",("line",None),"inspect") ,("nflen","ref",("line",None),"inspect"),
        ("flen","ref",("line",None),"mis") ,("nflen","ref",("line",None),"mis"),
        ("flen","ref",("line",None),"sim") ,("nflen","ref",("line",None),"sim"),
        ("dflen","ref",("line",None),"mis"),
        ("truth","ref",("line",None),"mis")
        ]
        }},
{"name": "doom_early", "files":[ 
"games/doom/00_doom02.exe"
,"games/doom/01_doom03.exe" 
,"games/doom/02_doom04.exe"
,"games/doom/03_doom05.exe" 
,"games/doom/04_doomp.exe"
,"games/doom/05_doom10s.exe"
], "types":{
    "align_page":[ ("dflen","ref",("line",None),"norm")
        ]
}},
{"name": "doom_beta", "files":[ 
"games/doom/05_doom10s.exe"
,"games/doom/06_doom11s.exe" 
,"games/doom/07_doom12s.exe"
,"games/doom/08_doom125ssybex.exe" 
,"games/doom/09_doom14bl.exe"
,"games/doom/10_doom14bs.exe" 
], "types":{
    "align_page":[ ("dflen","ref",("line",None),"norm") ]
}},
{"name": "doom_final", "files":[ 
"games/doom/10_doom14bs.exe" 
,"games/doom/11_doom15bs.exe"
,"games/doom/12_doom16b.exe" 
,"games/doom/13_doom1666.exe"
,"games/doom/14_doom18s.exe" 
,"games/doom/15_doom19s.exe" 
], "types":{
    "align_page":[ ("dflen","ref",("line",None),"norm") ]
}},
{"name": "doom", "files": doom_files, "types":{ "page":[("ref","verb")]}},
{"name": "doom", "files": doom_files, "types":{ "vsA": [([chosenone],("cross",None))] }},
{"name": "doom", "files": doom_files, "types": stdeval() },
]

def line_gen(files):
    for i in range(0,len(files)):
        for j in range(i-1,i):
            if i>=0 and j>=0 and files[i] != files[j]:
                yield (files[i], files[j])

def cross_gen(files):
    for i in range(0,len(files)):
        for j in range(0,i):
            if i>=0 and j>=0: #and files[i] != files[j]:
                yield (files[i], files[j])

def crossA_gen(files):
    for i in range(0,len(files)):
        for j in range(0,i):
            suiteA = os.path.basename(files[i]).split("_")[0]
            suiteB = os.path.basename(files[j]).split("_")[0]
            if suiteA != suiteB and suiteA < suiteB:
                yield (files[i], files[j])

def crossB_gen(files):
    for i in range(0,len(files)):
        for j in range(0,i):
            if i>=0 and j>=0 and os.path.basename(files[i]).split("_")[0] == os.path.basename(files[j]).split("_")[0]:
                #sys.stderr.write("%s %s\n" % (files[i], files[j]))
                yield (files[i], files[j])

gen = {"cross": cross_gen, "line": line_gen, "crossA": crossA_gen, "crossB": crossB_gen}

def get_gen(files,perm):
    sel, sample = perm
    data = list(gen[sel](files))
    if sample is None or len(data) <= sample:
        return data
    else:
        random.seed(0)
        rs = random.sample(range(0,len(data)),sample)
        return [data[i] for i in sorted(rs)]
    
compression=".brotli"

def alignment_template(a,b,t,n): 
    target = "eval/{0}_{1}_{2}.align".format(a, os.path.basename(b),t)
    im1 = "seq/{0}_{1}.json".format(a, t)
    im2 = "seq/{0}_{1}.json".format(b, t)
    src1 = "data/{0}.json".format(a)
    src2 = "data/{0}.json".format(b)
    print(target + ": " + im1 + " " + im2)
    print(im1+ ": " + src1)
    print(im2+ ": " + src2)
    return [target]

def evaluation_template(target, name,typ, prefix):
    print(""+prefix+":: " + target)
    print(""+prefix+"-"+typ+":: " + target)
    print(""+prefix+"-"+name+":: " + target)
    print(""+prefix+"-"+name+"-"+typ+":: " + target)

def foo_template(typ,name,prefix):
        num = []
        for align,perm in case["types"][typ]:
            for (b,a) in get_gen(case["files"],perm):
                for al in align:
                    num += alignment_template(a,b,al,name)
                    path = os.path.dirname(a)
                    alignment = "eval/{0}_{1}_{2}.align{3}".format(a, os.path.basename(b),al,compression)
                    print("alignments: " + alignment)
                    target= "img/{1}_{2}_{3}_{4}.png".format(path, name, "-".join(align), perm[0],typ)
                    print(target + ": {0}".format(alignment))
                    evaluation_template(target, name,typ,prefix)
        return set(num)

for prefix, cases in [("evaluation", testsuite)]:
    for case in cases:
        types = case["types"].keys()
        name = case["name"]
        alignments = []
        if "plot" in types:
            print("# plot")
            for typ,mode in case["types"]["plot"]:
                for i in case["files"]:
                    target = "img/{0}_{1}_{2}.png".format(i,typ,mode)
                    print(target + ": data/{0}.json{1}".format(i,""))
                    if typ == "mod":
                        print(target + ": data/{0}.names".format(i))
                    evaluation_template(target, name,"plot",prefix)
        if "pairs" in types:
            print("# pairs")
            #alignments.extend([ (x,perm) for (x,_,perm,_) in case["types"]["pairs"] ] )
            for align,typ,perm,mode in case["types"]["pairs"]:
                for (b,a) in get_gen(case["files"],perm):
                    alignments+= alignment_template(a,b,align,name)
                    alignment = "eval/{0}_{1}_{2}.align{3}".format(a, os.path.basename(b),align,compression)
                    target = "img/{0}_{1}_{2}_{3}_pairs_{4}.png".format(a, os.path.basename(b),align,typ,mode)
                    print(target + ": {0} data/{1}.json{3} data/{2}.json{3}".format(alignment,a,b,""))
                    if typ == "mod":
                        print(target + ": data/{0}.names data/{1}.names".format(a,b))
                    evaluation_template(target, name,"pairs",prefix)
        if "align_page" in types:
            print("# align_page")
            #alignments.extend([ (x,perm) for (x,_,perm,_) in case["types"]["align_page"] ] )
            for align,typ,perm,mode in case["types"]["align_page"]:
                for (b,a) in get_gen(case["files"],perm):
                    alignments+= alignment_template(a,b,align,name)
                    path = os.path.dirname(a)
                    alignment = "eval/{0}_{1}_{2}.align{3}".format(a, os.path.basename(b),align,compression)
                    target= "img/{1}_{2}_{3}_{4}_alignpage_{5}.png".format(path, name, align, typ, perm[0], mode)
                    print(target + ": {0} data/{1}.json{3} data/{2}.json{3}".format(alignment,a,b,""))
                    if typ == "mod":
                        print(target + ": data/{0}.names data/{1}.names".format(a,b))
                    evaluation_template(target, name,"align_page",prefix)
        if "page" in types:
            print("# page")
            for typ,mode in case["types"]["page"]:
                for a in case["files"]:
                    path = os.path.dirname(a)
                    target = "img/{1}_{2}_page_{3}.png".format(path,name,typ,mode)
                    print(target + ": data/{0}.json{1}".format(a,""))
                    if typ == "mod":
                        print(target + ": data/{0}.names".format(a))
                    evaluation_template(target, name,"page",prefix)
        for t in ["pr","F1","F1a","F2","precision","recall","similarity", "potential","simA","simB","simC","corA","corB","corC","corD","cor","vsA","vsB", "vsC"]:
            if t in types:
                alignments+= foo_template(t,name,prefix)
        sys.stderr.write("%s %d\n" % (name, len(set(alignments))))
        #for align, perm in alignments: 
            #for (b,a) in get_gen(case["files"],perm):
                #alignment_template(a,b,align,name)
