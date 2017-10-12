class Start: # start of a sentence
    def __init__(self, left):
        self.type = "start"
        self.left = left
        self.right = None
        self.op = "start"

class NegateOp:
    def __init__(self, left, parent=None):
        self.type = "not"
        self.op = "~"
        self.parent = parent
        self.left = left
        self.right = None

class BinOp:
    def __init__(self,left,op,right, parent=None):
        self.type = "binop"
        self.op = op
        self.parent = parent
        self.left = left
        self.right = right

class Predicate():
    def __init__(self, name, children=None):
         self.type = "pred"
         self.name = name
         if children: self.children = children
         else: self.children = [ ]
         self.op = "pred"

class Variable():
    def __init__(self, value):
        self.type = "var"
        self.value = value

class List():
    def __init__(self, first_child):
        self.type = "list"
        self.children = []
        self.children.append(first_child)

class Constant():
    def __init__(self, value):
        self.type = "const"
        self.value = value