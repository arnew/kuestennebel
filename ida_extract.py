import idaapi
import idascript
import idautils
import idc
import math
import itertools
import json
import csv
import datetime
import cProfile

def eatostart(ea):
    """Map an address from within a function to the function start address."""
    f = idaapi.get_func(ea)
    return None if f is None else f.startEA

def sum_xrefs(ea):
    """Count the number of control references from a given address."""
    return len([x for x in XrefsFrom(ea,0)])
def sum_drefs(ea):
    """Count the number of data references from a given address."""
    return len([x for x in DataRefsFrom(ea)])

def bb_info(bb):
    """Count the number of control and data references from a basic block."""
    data = [[1,sum_xrefs(ea),sum_drefs(ea)] for ea in range(bb.startEA, bb.endEA)]
    return map(sum,zip(*data))

def fkt_info(f):
    """Return the start and end addresses of a function, as well as its control coupling to other functions."""
    calls = list(set(map(eatostart,CodeRefsTo(f.startEA,0))))
    callees = list(set([(x,y) for x in range(f.startEA, f.endEA) for y in CodeRefsFrom(x,1) if y < f.startEA or y >= f.endEA]))
    return {'start': f.startEA, 'end': f.endEA, 'size':f.size(), 'calls': calls, 'callees': callees}

def flowchart_info(idaflow):
    """Return a control flow graph of a function as well as count summaries of the code and data references of the code."""
    nodes = list(set([x for x in idaflow]))
    edges = list(set([(r,y.startEA) for x in nodes for y in x.succs() for r in CodeRefsTo(y.startEA,1) if r >= x.startEA and r <=x.endEA ]))
    bbinfo = map(sum,zip(*map(bb_info, [n for n in nodes if n.endEA-n.startEA > 0])))
    graph = [{"start": x.startEA, "len": x.endEA-x.startEA, "nxt": [y.startEA for y in x.succs()]} for x in idaflow]
    return {'nodes' : len(nodes),'edges': len(edges),'bbsize': bbinfo[0], 'Xrefs': bbinfo[1], 'Drefs': bbinfo[2], 'graph': graph, 'edges' : edges}
    
def function_extract(x):
    """Return information about a function starting at the given address."""
    name = GetFunctionName(x)
    idafkt = idaapi.get_func(x)
    idaflow = idaapi.FlowChart(idafkt)
    return {'addr': x, 'name': name, 'fkt': fkt_info(idafkt), 'chart': flowchart_info(idaflow)}


# find set of functions
functions = sorted(set(idautils.Functions()))

# extract information about functions
data = map(function_extract, functions)

# write extraced information to file
with open(idc.ARGV[1],'wb') as f:
    json.dump(data,f)

idc.Exit(0)
