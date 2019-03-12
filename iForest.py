
# Building Isolation tree in Python


import pandas as pd
import numpy as np
import random as rd



def iTree(df, l, e = 0):
    """Recursivley builds an iTree from a pandas data frame
    df = the data
    e = current depth
    l = max depth"""
    if df.shape[0] <= 1 or e >= l:
        return {'NodeType' : 'External',
               'Left' : None,
               'Right' :  None,
               'SplitAtt' : None,
               'SplitVal' : None,
               'Size' : df.shape[0],
               'Depth' : e}
    else:
        q = rd.choice(df.columns.values)
        p = np.random.uniform(low = df[q].min(), high = df[q].max())
        return {'NodeType': 'Internal',
               'Left' : iTree(df[df[q] < p], l, e + 1),
               'Right' : iTree(df[df[q] >= p], l, e + 1), 
               'SplitAtt' : q,
               'SplitVal' : p,
               'Size': df.shape[0],
                'Depth' : e
               }
    
def USS(n):
    """average path length of unsuccessful search in BST"""
    if n < 2:
        return 0
    elif n == 2:
        return 1
    else:
        return (2.0 * (np.log(n - 1.0) + 0.5772156649) + 2.0 * (n - 1.0)/n)


def PathLength(pnt, Tree, e = 0):
    """ Get Path length of a data point (pnt) from a tree (Tree) """
    if Tree['NodeType'] == 'External':
        return e + USS(Tree['Size']) 
    
    else:
        q = Tree['SplitAtt']
        p = Tree['SplitVal']
        if pnt[q] < p:
            return PathLength(pnt, Tree['Left'], e + 1)
        else:
            return PathLength(pnt, Tree['Right'], e + 1)
        

        
def iForest(df, nt , phi):
    """df - the data set, nt - the number of trees to build, phi - subsample size"""
    Forest = [None] * nt
    depth = np.ceil(np.log(phi))
    for i in xrange(nt):
        sub_sample = rd.sample(df.index, phi)
        Tree = iTree(df.iloc[sub_sample,:], l = depth)
        Forest[i] = Tree
    return Forest


def predict_iForest(df, Forest, Phi):
    """Takes an iForest and then predicts the values of each point, and then converts it into an anomoly score"""
    PL = pd.Series(0, index = range(len(df)))
    for i in range(len(Forest)):
        Tree = Forest[i]
        PL = PL + df.apply(lambda x: PathLength(x, Tree) , axis = 1)
    PL = PL/len(Forest)
    PL = np.power(2, -1 * PL/USS(Phi))
    return PL    

    
def Print_iTree(Tree, Direction = None):
    "pretty prints an iTree"
    #getting number of indents
    Prnt_str = ' ' * Tree['Depth'] + 'Depth: ' + str(Tree['Depth'])
    if Direction != None:
        Prnt_str = Prnt_str + ' (' + Direction + ')'
    if Tree['NodeType'] == 'External':
        Prnt_str = Prnt_str + ' - External Node - (' + str(Tree['Size']) + ')'
        print(Prnt_str)
    else:
        Prnt_str = Prnt_str + ' - Split Var: ' +  str(Tree['SplitAtt']) + ' at ' + str(round(Tree['SplitVal'],4))
        Prnt_str = Prnt_str + ' True: ' + str(round(Tree['Left']['Size']/(Tree['Size'] * 1.000),2) * 100) + '%'
        Prnt_str = Prnt_str + ' (' + str(Tree['Size']) + ')'
        print(Prnt_str)
        Print_iTree(Tree['Left'], 'Left')
        Print_iTree(Tree['Right'], 'Right')
