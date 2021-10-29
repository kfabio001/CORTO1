import ply.lex as lex
import ply.yacc as yacc

cont = 0

tokens = (

    'ParA',
    'ParC',
    'SUMA',
    'RESTA',
    'MULTIPLICACION',
    'DIVISION',
    'ID',
)
# -----------------------------------------------------------------------------
# Calculadora
# 
# Tokens


t_SUMA = r'\+'  
t_RESTA = r'-'
t_MULTIPLICACION = r'\*'
t_DIVISION = r'/'
t_ParA = r'\('
t_ParC = r'\)'


def t_ID(t):
    r'[a-zA-Z_0-9][a-zA-Z_0-9]*'
    return t
# Ignored characters
t_ignore = " \t"


def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.cont("\n")


def t_error(t):
    print("Ilegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()
#lexico
# Precedence
precedence = (
	('left', 'SUMA', 'RESTA'),
    ('left', 'MULTIPLICACION', 'DIVISION'),
)

names = { }
def new_temp():
    global cont
    cont = cont +1
    etiqueta = 't'+str(cont)
    return etiqueta
        

class Temporal():
    def __init__(self,tmp,c3d):
        self.tmp=tmp
        self.c3d=c3d

def p_S(t):
    ' s	: e'
    t[0] = t[1]
    print("Salida \n"+str(t[0].c3d))
def p_Exp(t):
    ''' e	: e SUMA t 
            | e RESTA t  '''

    if t[2] == '+':
        Etemp = new_temp()
        Ec3d = str(t[1].c3d)+str(t[3].c3d)+ str(Etemp)+"="+str(t[1].tmp)+"+"+str(t[3].tmp+"\n")
        t[0] = Temporal(str(Etemp),str(Ec3d))
    
    elif t[2] == '-':
        Etemp = new_temp()
        Ec3d = str(t[1].c3d)+str(t[3].c3d)+str(Etemp)+"="+str(t[1].tmp)+"-"+str(t[3].tmp+"\n")
        t[0] = Temporal(str(Etemp),str(Ec3d))
        
def p_etiqueta(t):
    'e : t'
    t[0] = t[1]
    
def p_T(t):
    ''' e	: e MULTIPLICACION t 
            | e DIVISION t  ''' 
    if t[2] == '*':
        Etemp = new_temp()  
        Ec3d = str(t[1].c3d)+str(t[3].c3d)+str(Etemp)+"="+str(t[1].tmp)+"*"+str(t[3].tmp+"\n")
        t[0] = Temporal(str(Etemp),str(Ec3d))
	    
    
    elif t[2] == '/':
        Etemp = new_temp()
        Ec3d = str(t[1].c3d)+str(t[3].c3d)+str(Etemp)+"="+str(t[1].tmp)+"/"+str(t[3].tmp+"\n")
        t[0] = Temporal(str(Etemp),str(Ec3d))


        
def p_tf(t):
    't : f'
    t[0] = t[1]
    
def p_F_ID(t):
    ' f	: ID '
    t[0] = Temporal(str(t[1]),"")
    
def p_F_PAR(t):
    ' f	: ParA e ParC '
    t[0] = t[2]

def p_error(t):
    print("Error Sintáctico en '%s'" % t.value)
parser = yacc.yacc()
while True:
    try:
        entrada = input('ingrese entrada  \n ejemplo 3*3+4  >')   # input python
    except EOFError:
        break
    parser.parse(entrada)

parser.parse(entrada)
