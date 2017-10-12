import ply.lex as lex
import ply.yacc as yacc
import parsertree as pt

tokens = (
    'PREDICATE','CONSTANT', 'VARIABLE', 
    'COMMA',
    'LPAREN', 'RPAREN',
    'NOT', 'AND', 'OR', 'IMPLIES',
)

t_IMPLIES = r'\=>'
t_NOT = r'\~'
t_AND = r'\&'
t_OR = r'\|'
t_COMMA = r','
t_LPAREN  = r'\('
t_RPAREN  = r'\)'

def t_PREDICATE(t):
    r'[A-Z][a-zA-Z]*\('
    return t

def t_CONSTANT(t):
    r'[A-Z_][a-zA-Z0-9_]*'
    return t

def t_VARIABLE(t):
    r'[a-z]'
    return t

def t_error(t):
    print("Illegal character '%s'" % t.value[0])
    t.lexer.skip(1)

def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.count("\n")

t_ignore = " \t"

lexer = lex.lex()

# Parsing rules
precedence = (
    ('left', 'IMPLIES'),
    ('left', 'OR'),
    ('left', 'AND'),
    ('right', 'NOT'),
)

# grammar rules #

def p_sentence_not(p):
    "sentence : NOT sentence"
    if p[2].op == '~':
        p[0] = p[2].left
    else:
        p[0] = pt.NegateOp(p[2])

def p_sentence_op(p):
    '''sentence : sentence AND sentence
                | sentence OR sentence
                | sentence IMPLIES sentence
    '''
    if p[2] == '=>':
        p[0] = pt.BinOp(pt.NegateOp(p[1]), '|', p[3])
    else :
        p[0] = pt.BinOp(p[1], p[2], p[3])

def p_sentence_nested(p):
    "sentence : LPAREN sentence RPAREN"
    p[0] = p[2]

def p_literal(p):
    "literal : PREDICATE term_list RPAREN"
    p[0] = pt.Predicate(p[1][0:-1])
    p[0].children = p[2].children

def p_sentence_literal(p):
    "sentence : literal"
    p[0] = p[1]

def p_term_list(p):
    '''term_list : term
                 | term_list COMMA term'''
    if len(p) == 2:
        p[0] = pt.List(p[1])
    elif len(p) == 4:
        p[1].children.append(p[3])
        p[0] = p[1]

def p_term_variable(p):
    "term : VARIABLE"
    p[0] = pt.Variable(p[1])

def p_term_constant(p):
    "term : CONSTANT"
    p[0] = pt.Constant(p[1])

def p_error(p):
    if p:
        print("Syntax error at '%s'" % p.value)
    else:
        print("Syntax error at EOF")

def populate_parent(root):
    if root.type == "pred":
        return
    right, left  = root.right, root.left
    if right:
        right.parent = root
        populate_parent(right)
    if left:
        left.parent = root
        populate_parent(left)

def push_negation_inward(root):
    if root.type == "pred":
        return
    if root.op != '~':
        if root.right:
            push_negation_inward(root.right)
        if root.left:
            push_negation_inward(root.left)

    else:
        parent = root.parent
        child = root.left

        if child.op == '&' or child.op == '|':
            child.left = pt.NegateOp(child.left, child)
            child.right = pt.NegateOp(child.right, child)
            if child.op == '|':
                child.op = '&'
            elif child.op == '&':
                child.op = '|'
            child.parent = parent
            if parent.left != root:
                parent.right = child
            else:
                parent.left = child
            push_negation_inward(child.left), push_negation_inward(child.right)

        if child.op == '~':
            if parent.left != root:
                parent.right = child.left
            else:
                parent.left = child.left
            child.left.parent = parent
            push_negation_inward(child.left)

def distribute_or(root):
    if root.type == "pred":
        return
    if root.right:
        distribute_or(root.right)
    if root.left:
        distribute_or(root.left)
    if not (root.op == '|' and (root.left.op == '&' or root.right.op == '&')):
        return
    else:
        parent = root.parent
        left, right = root.left, root.right
        if right.op == '&':
            l_and, r_and = pt.BinOp(right.left, '|', left), pt.BinOp(right.right, '|', left)
        elif left.op == '&':
            l_and, r_and = pt.BinOp(left.left, '|', right), pt.BinOp(left.right, '|', right)
        new_and = pt.BinOp(l_and, '&', r_and, parent)
        l_and.parent = r_and.parent = new_and
        if parent.left != root:
            parent.right = new_and
        else:
            parent.left = new_and
        distribute_or(l_and), distribute_or(r_and)

def parse_sentence(line):
    res = pt.Start(yacc.parse(line))
    populate_parent(res)
    push_negation_inward(res)
    distribute_or(res)
    return res

yacc.yacc()
