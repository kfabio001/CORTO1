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
    def __init__(selayerF,tmp,concat):
        selayerF.tmp=tmp
        selayerF.concat=concat

def p_S(t):
    ' s	: e'
    t[0] = t[1]
    print("Salida \n"+str(t[0].concat))
def p_Exp(t):
    ''' e	: e SUMA t 
            | e RESTA t  '''

    if t[2] == '+':
        expr = new_temp()
        contenido = str(t[1].concat)+str(t[3].concat)+ str(expr)+"="+str(t[1].tmp)+"+"+str(t[3].tmp+"\n")
        t[0] = Temporal(str(expr),str(contenido))
    
    elif t[2] == '-':
        expr = new_temp()
        contenido = str(t[1].concat)+str(t[3].concat)+str(expr)+"="+str(t[1].tmp)+"-"+str(t[3].tmp+"\n")
        t[0] = Temporal(str(expr),str(contenido))
        
def p_etiqueta(t):
    'e : t'
    t[0] = t[1]
    
def p_T(t):
    ''' e	: e MULTIPLICACION t 
            | e DIVISION t  ''' 
    if t[2] == '*':
        expr = new_temp()  
        contenido = str(t[1].concat)+str(t[3].concat)+str(expr)+"="+str(t[1].tmp)+"*"+str(t[3].tmp+"\n")
        t[0] = Temporal(str(expr),str(contenido))
	    
    
    elif t[2] == '/':
        expr = new_temp()
        contenido = str(t[1].concat)+str(t[3].concat)+str(expr)+"="+str(t[1].tmp)+"/"+str(t[3].tmp+"\n")
        t[0] = Temporal(str(expr),str(contenido))


        
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
