
import ply.yacc as yacc
import ply.lex as lex

cont = 0
etiquetass=0
error=[]
palabras={
    'or' : 'OR',
    'and' : 'AND',
    'not' : 'NOT'
}

tokens = [
    'ParA',
    'ParC',
    'SUMA',
    'RESTA',
    'MULTIPLICACION',
    'DIVISION',
    'ID',
    'ENTERO',
    'DECIMAL',
    'MAYOR',
    'MENOR',
    'MAYORIGUAL',
    'MENORIGUAL',
    'IGUAL',
    'DIFERENTE',
    'VERDADERO',
    'FALSO',
]+ list(palabras.values())



t_ParA = r'\('
t_ParC = r'\)'
t_SUMA = r'\+'  
t_RESTA = r'-'
t_MULTIPLICACION = r'\*'
t_DIVISION = r'/'
t_AND = r'and'
t_OR = r'or'
t_IGUAL = r'=='
t_DIFERENTE = r'!='
t_MAYOR = r'>'
t_MENOR = r'<'
t_MAYORIGUAL = r'>='
t_MENORIGUAL = r'<='
t_VERDADERO = r'true'
t_FALSO =r'false'


def t_ID(t):
    r'[a-zA-Z_0-9][a-zA-Z_0-9]*'
    t.type = palabras.get(t.value.lower(),'ID')  
    return t

def t_DECIMAL(t):
    r'\d+\.\d+'
    try:
        t.value = float(t.value)
    except ValueError:
        print("Float value too large %d", t.value)
        t.value = 0
    return t

def t_ENTERO(t):
    r'\d+'
    try:
        t.value = int(t.value)
    except ValueError:
        print("Integer value too large %d", t.value)
        t.value = 0
    return t
	
t_ignore = " \t\r"


def t_newline(t):
    r'\n+'
    t.lexer.lineno += t.value.cont("\n")


def t_error(t):
    print("Ilegal character '%s'" % t.value[0])
    error.append("Ilegal character '%s'" % t.value[0])
    t.lexer.skip(1)

lexer = lex.lex()

precedence = (
    ('left', 'OR'),
    ('left', 'AND'),
    ('left', 'IGUAL','DIFERENTE'),
    ('nonassoc','MAYOR','MENOR' ),
    ('left', 'SUMA', 'RESTA'),
    ('left', 'MULTIPLICACION', 'DIVISION'),
    ('right', 'URESTA'),
    ('right', 'NOT'),
    ('left','ParA','ParC'),
)

def new_temp():
    global cont
    cont = cont +1
    etiqueta = 't'+str(cont)
    return etiqueta

def new_Etiqueta():
    global etiquetass
    etiquetass = etiquetass+1
    etiquetaa='L'+str(etiquetass)
    return etiquetaa
        

class Temporales():
    def __init__(selayerF,id,tmp,concat,layerT,layerF):
        selayerF.id=id
        selayerF.tmp=tmp
        selayerF.concat=concat
        selayerF.layerT = layerT
        selayerF.layerF = layerF

def p_S(t):
    ' s	: p'
    t[0] = t[1]
    print(str(t[0].concat))
    print("etiquetas verdad: "+str(t[1].layerT))
    print("etiquetas falsas: "+str(t[1].layerF))
    

def p_p_OR_l(t):
    'p  :   p OR l'
    layerT=str(t[1].layerT)+ ' , '+str(t[3].layerT)
    layerF=str(t[3].layerF)
    concat=str(t[1].concat)+t[1].layerF+" : \n"+str(t[3].concat)
    t[0]=Temporales("","",concat,layerT,layerF)

def p_p_l(t):
    'p  : l'
    t[0] = t[1]
    
def p_l_AND_K(t):
    'l  :   l AND n'
    layerT=str(t[3].layerT)
    layerF=str(t[1].layerF)+' , '+str(t[3].layerF)
    concat=str(t[1].concat)+t[1].layerT+" : \n"+str(t[3].concat)
    t[0]=Temporales("","",concat,layerT,layerF)

def p_l_k(t):
    'l  : n'
    t[0]=t[1]
    
def p_r_NOT(t):
    'n  :   NOT k'
    layerT=str(t[2].layerF)
    layerF=str(t[2].layerT)
    concat=str(t[2].concat)
    t[0]=Temporales("","",concat,layerT,layerF) 
    
def p_r_NOT1(t):
    'n  :   k'
    t[0]=t[1]
    
def p_k_ma(t):
    'k  : e MAYOR e'
    layerT = new_Etiqueta()
    layerF = new_Etiqueta()
    concat=str(t[1].concat)+str(t[3].concat)+'if '+str(t[1].tmp)+str(t[1].id)+' > '+str(t[3].tmp)+str(t[3].id)+' goto '+layerT+'\r\n'+'goto '+layerF+'\r\n'
    t[0]=Temporales("","",concat,layerT,layerF)
def p_k_me(t):
    'k  : e MENOR e'
    layerT = new_Etiqueta()
    layerF = new_Etiqueta()
    concat=str(t[1].concat)+str(t[3].concat)+'if '+str(t[1].tmp)+str(t[1].id)+' < '+str(t[3].tmp)+str(t[3].id)+' goto '+layerT+'\r\n'+'goto '+layerF+'\r\n'
    t[0]=Temporales("","",concat,layerT,layerF)

def p_k_maigual(t):
    'k  : e MAYORIGUAL e'
    layerT = new_Etiqueta()
    layerF = new_Etiqueta()
    concat=str(t[1].concat)+str(t[3].concat)+'if '+str(t[1].tmp)+str(t[1].id)+' >= '+str(t[3].tmp)+str(t[3].id)+' goto '+layerT+'\r\n'+'goto '+layerF+'\r\n'
    t[0]=Temporales("","",concat,layerT,layerF)
def p_k_meigual(t):
    'k  : e MENORIGUAL e'
    layerT = new_Etiqueta()
    layerF = new_Etiqueta()
    concat=str(t[1].concat)+str(t[3].concat)+'if '+str(t[1].tmp)+str(t[1].id)+' <= '+str(t[3].tmp)+str(t[3].id)+' goto '+layerT+'\r\n'+'goto '+layerF+'\r\n'
    t[0]=Temporales("","",concat,layerT,layerF)

def p_k_ig(t):
    'k  : e IGUAL e'
    layerT = new_Etiqueta()
    layerF = new_Etiqueta()
    concat=str(t[1].concat)+str(t[3].concat)+'if '+str(t[1].tmp)+str(t[1].id)+' == '+str(t[3].tmp)+str(t[3].id)+' goto '+layerT+'\r\n'+'goto '+layerF+'\r\n'
    t[0]=Temporales("","",concat,layerT,layerF)

def p_k_dif(t):
    'k  : e DIFERENTE e'
    layerT = new_Etiqueta()
    layerF = new_Etiqueta()
    concat=str(t[1].concat)+str(t[3].concat)+'if '+str(t[1].tmp)+str(t[1].id)+' != '+str(t[3].tmp)+str(t[3].id)+' goto '+layerT+'\r\n'+'goto '+layerF+'\r\n'
    t[0]=Temporales("","",concat,layerT,layerF)

def p_k_ee(t):
    'k  : e'    
    t[0]=t[1]


def p_E(t):
    ''' e	: e SUMA e 
            | e RESTA e
            | e MULTIPLICACION e
            | e DIVISION e
            '''

    if t[2] == '+':
        expr = new_temp()
        contenido = str(t[1].concat)+str(t[3].concat)+str(expr)+"="+str(t[1].id)+str(t[1].tmp)+"+"+str(t[3].id)+str(t[3].tmp)+"\n"
        t[0] = Temporales("",str(expr),str(contenido),"","")

    elif t[2] == '-':
        expr = new_temp()
        contenido = str(t[1].concat)+str(t[3].concat)+str(expr)+"="+str(t[1].id)+str(t[1].tmp)+"-"+str(t[3].id)+str(t[3].tmp)+"\n"
        t[0] = Temporales("",str(expr),str(contenido),"","")
    elif t[2] == '*':
        expr = new_temp()
        contenido = str(t[1].concat)+str(t[3].concat)+str(expr)+"="+str(t[1].id)+str(t[1].tmp)+"*"+str(t[3].id)+str(t[3].tmp)+"\n"
        t[0] = Temporales("",str(expr),str(contenido),"","")
    
    elif t[2] == '/':
        expr = new_temp()
        contenido = str(t[1].concat)+str(t[3].concat)+str(expr)+"="+str(t[1].id)+str(t[1].tmp)+"/"+str(t[3].id)+str(t[3].tmp)+"\n"
        t[0] = Temporales("",str(expr),str(contenido),"","")
 
def p_e_min(t):
    'e     :   RESTA e %prec URESTA ' 
    expr = new_temp() 
    contenido = str(expr)+' = 0 -'+str(t[0].id)
    t[0] = Temporales("",str(expr),contenido,"","")



def p_e_ID(t):
    ''' e	: ID 
            | ENTERO
            | DECIMAL
            '''
      
    t[0] = Temporales(str(t[1]),"","","","")

def p_e_PAR(t):
    ' e	: ParA p ParC '
    t[0] = t[2]


def p_error(t):
    print("SyntaxError '%s'" % t.value)
    error.append("SyntaxError en '%s'" % t.value)


parser = yacc.yacc()
while True:
    try:
        entrada = input('Ingrese entrada  \n (ejemplo1: 3*3+4) \n (ejemplo2: 3>3) \n >>  ')   # input python
    except EOFError:
        break
    parser.parse(entrada)
    errores=""
    res3d=""
    for err in error:
        errores+=str(err)+"\n"
    

    print(res3d)
