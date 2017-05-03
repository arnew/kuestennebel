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

def createitems(basicblock):
        xrefs = lambda ea: len([x for x in XrefsFrom(ea,0)])
        drefs = lambda ea: len([x for x in DataRefsFrom(ea)])
        consts = lambda ea: len([GetOperandValue(ea,y) for y in range(idaapi.UA_MAXOP) if (GetOpType(ea,y) in [o_mem,o_phrase,o_displ,o_imm,o_far,o_near])])
        data = [[1,xrefs(ea),drefs(ea),consts(ea)] for ea in range(basicblock.startEA, basicblock.endEA)]
        return map(sum,zip(*data))

def cc(fkt):
    nodes = set([x for x in idaapi.FlowChart(idaapi.get_func(fkt))])
    edges = set([(x,y) for x in nodes for y in x.succs()])
    return [len(nodes),len(edges),len(edges) - len(nodes) + 2]

def eatostart(ea):
    f = idaapi.get_func(ea)
    return None if f is None else f.startEA

def calc_inout(functions):
    calls = [(callee,caller,1) for caller in map(eatostart,functions) for callee in map(eatostart,CodeRefsTo(caller,0))]
    calls = sorted(calls ,key=lambda x: x[0])
    callees = {c[0]:sum(map(lambda x:x[2],c[1])) for c in itertools.groupby(calls, lambda x: x[0])}
    calls = sorted(calls ,key=lambda x: x[1])
    callers = {c[0]: sum(map(lambda x:x[2],c[1])) for c in itertools.groupby(calls, lambda x: x[1])}
    return (callers, callees)

def lookup_calls(callers, callees, x):
    return [ callers[x] if x in callers and callers[x] < x else 0 ,callees[x] if x in callees and callees[x] < x else 0 ,callers[x] if x in callers and callers[x] > x else 0 ,callees[x] if x in callees and callees[x] > x else 0 ]


def sum_xrefs(ea):
    return len([x for x in XrefsFrom(ea,0)])
def sum_drefs(ea):
    return len([x for x in DataRefsFrom(ea)])
def sum_consts(ea):
    return len([GetOperandValue(ea,y) for y in range(idaapi.UA_MAXOP) if (GetOpType(ea,y) in [o_mem,o_phrase,o_displ,o_imm,o_far,o_near])])

def bb_info(bb):
    data = [[1,sum_xrefs(ea),sum_drefs(ea)] for ea in range(bb.startEA, bb.endEA)]
    return map(sum,zip(*data))

def fkt_info(f):
    calls = list(set(map(eatostart,CodeRefsTo(f.startEA,0))))
    callees = list(set([(x,y) for x in range(f.startEA, f.endEA) for y in CodeRefsFrom(x,1) if y < f.startEA or y >= f.endEA]))
    return {'start': f.startEA, 'end': f.endEA, 'size':f.size(), 'calls': calls, 'callees': callees}

def flowchart_info(idaflow):
    nodes = list(set([x for x in idaflow]))
    edges = list(set([(r,y.startEA) for x in nodes for y in x.succs() for r in CodeRefsTo(y.startEA,1) if r >= x.startEA and r <=x.endEA ]))
    bbinfo = map(sum,zip(*map(bb_info, [n for n in nodes if n.endEA-n.startEA > 0])))
    graph = [{"start": x.startEA, "len": x.endEA-x.startEA, "nxt": [y.startEA for y in x.succs()]} for x in idaflow]
    return {'nodes' : len(nodes),'edges': len(edges),'bbsize': bbinfo[0], 'Xrefs': bbinfo[1], 'Drefs': bbinfo[2], 'graph': graph, 'edges' : edges}
    
def function_extract(x):
    name = GetFunctionName(x)
    idafkt = idaapi.get_func(x)
    idaflow = idaapi.FlowChart(idafkt)
    return {'addr': x, 'name': name, 'fkt': fkt_info(idafkt), 'chart': flowchart_info(idaflow)}



#for i in xrange(1, len(idc.ARGV)):
    #print "ARGV[%d]=%s" % (i, idc.ARGV[i])


functions = []
sta=idc.SelStart()
end=idc.SelEnd()
#print sta, end
if sta!=end:
    functions = idautils.Functions(sta,end)
else:
    functions = idautils.Functions()
functions = sorted(set(functions))


#(callers, callees) = calc_inout(functions)

#print "aggregating data", datetime.datetime.now()
#cProfile.run('data = map(function_extract, functions)')
data = map(function_extract, functions)



#[
#        #per function
#        (
#        # start, end, name
#        x, , GetFunctionName(x), 
#        # cyclomatic complexity
#        cc(x)
#        # sum of bb lengths
#	+ [sum([ bb.endEA-bb.startEA for bb in idaapi.FlowChart(idaapi.get_func(x))])]
#        # ( list of caller addrs, list of callee addrs )
#        + lookup_calls(callers,callees,x) 
#        # [ per bb ( start, end, list of xrefs, list of drefs, list of consts ) ]
#	+ map(sum,zip(*[createitems(bb) 
#            for bb in idaapi.FlowChart(idaapi.get_func(x)) 
#            if (bb.endEA-bb.startEA >0) ])) 
#        ) for x in functions]


#print "writing output", datetime.datetime.now()
with open(idc.ARGV[1],'wb') as f:
    json.dump(data,f)

#print "done", datetime.datetime.now()

idc.Exit(0)
