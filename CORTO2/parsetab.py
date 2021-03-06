
# parsetab.py
# This file is automatically generated. Do not edit.
# pylint: disable=W,C,R
_tabversion = '3.10'

_lr_method = 'LALR'

_lr_signature = 'leftORleftANDleftIGUALDIFERENTEnonassocMAYORMENORleftSUMARESTAleftMULTIPLICACIONDIVISIONrightURESTArightNOTleftParAParCAND DECIMAL DIFERENTE DIVISION ENTERO FALSO ID IGUAL MAYOR MAYORIGUAL MENOR MENORIGUAL MULTIPLICACION NOT OR ParA ParC RESTA SUMA VERDADERO s\t: pp  :   p OR lp  : ll  :   l AND nl  : nn  :   NOT kn  :   kk  : e MAYOR ek  : e MENOR ek  : e MAYORIGUAL ek  : e MENORIGUAL ek  : e IGUAL ek  : e DIFERENTE ek  : e e\t: e SUMA e \n            | e RESTA e\n            | e MULTIPLICACION e\n            | e DIVISION e\n            e     :   RESTA e %prec URESTA  e\t: ID \n            | ENTERO\n            | DECIMAL\n             e\t: ParA p ParC '
    
_lr_action_items = {'NOT':([0,12,13,14,],[5,5,5,5,]),'RESTA':([0,5,7,8,9,10,11,12,13,14,16,17,18,19,20,21,22,23,24,25,26,30,31,32,33,34,35,36,37,38,39,40,],[8,8,23,8,-20,-21,-22,8,8,8,8,8,8,8,8,8,8,8,8,8,-19,23,23,23,23,23,23,-15,-16,-17,-18,-23,]),'ID':([0,5,8,12,13,14,16,17,18,19,20,21,22,23,24,25,],[9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,9,]),'ENTERO':([0,5,8,12,13,14,16,17,18,19,20,21,22,23,24,25,],[10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,]),'DECIMAL':([0,5,8,12,13,14,16,17,18,19,20,21,22,23,24,25,],[11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,11,]),'ParA':([0,5,8,12,13,14,16,17,18,19,20,21,22,23,24,25,],[12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,12,]),'$end':([1,2,3,4,6,7,9,10,11,15,26,28,29,30,31,32,33,34,35,36,37,38,39,40,],[0,-1,-3,-5,-7,-14,-20,-21,-22,-6,-19,-2,-4,-8,-9,-10,-11,-12,-13,-15,-16,-17,-18,-23,]),'OR':([2,3,4,6,7,9,10,11,15,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,],[13,-3,-5,-7,-14,-20,-21,-22,-6,-19,13,-2,-4,-8,-9,-10,-11,-12,-13,-15,-16,-17,-18,-23,]),'ParC':([3,4,6,7,9,10,11,15,26,27,28,29,30,31,32,33,34,35,36,37,38,39,40,],[-3,-5,-7,-14,-20,-21,-22,-6,-19,40,-2,-4,-8,-9,-10,-11,-12,-13,-15,-16,-17,-18,-23,]),'AND':([3,4,6,7,9,10,11,15,26,28,29,30,31,32,33,34,35,36,37,38,39,40,],[14,-5,-7,-14,-20,-21,-22,-6,-19,14,-4,-8,-9,-10,-11,-12,-13,-15,-16,-17,-18,-23,]),'MAYOR':([7,9,10,11,26,36,37,38,39,40,],[16,-20,-21,-22,-19,-15,-16,-17,-18,-23,]),'MENOR':([7,9,10,11,26,36,37,38,39,40,],[17,-20,-21,-22,-19,-15,-16,-17,-18,-23,]),'MAYORIGUAL':([7,9,10,11,26,36,37,38,39,40,],[18,-20,-21,-22,-19,-15,-16,-17,-18,-23,]),'MENORIGUAL':([7,9,10,11,26,36,37,38,39,40,],[19,-20,-21,-22,-19,-15,-16,-17,-18,-23,]),'IGUAL':([7,9,10,11,26,36,37,38,39,40,],[20,-20,-21,-22,-19,-15,-16,-17,-18,-23,]),'DIFERENTE':([7,9,10,11,26,36,37,38,39,40,],[21,-20,-21,-22,-19,-15,-16,-17,-18,-23,]),'SUMA':([7,9,10,11,26,30,31,32,33,34,35,36,37,38,39,40,],[22,-20,-21,-22,-19,22,22,22,22,22,22,-15,-16,-17,-18,-23,]),'MULTIPLICACION':([7,9,10,11,26,30,31,32,33,34,35,36,37,38,39,40,],[24,-20,-21,-22,-19,24,24,24,24,24,24,24,24,-17,-18,-23,]),'DIVISION':([7,9,10,11,26,30,31,32,33,34,35,36,37,38,39,40,],[25,-20,-21,-22,-19,25,25,25,25,25,25,25,25,-17,-18,-23,]),}

_lr_action = {}
for _k, _v in _lr_action_items.items():
   for _x,_y in zip(_v[0],_v[1]):
      if not _x in _lr_action:  _lr_action[_x] = {}
      _lr_action[_x][_k] = _y
del _lr_action_items

_lr_goto_items = {'s':([0,],[1,]),'p':([0,12,],[2,27,]),'l':([0,12,13,],[3,3,28,]),'n':([0,12,13,14,],[4,4,4,29,]),'k':([0,5,12,13,14,],[6,15,6,6,6,]),'e':([0,5,8,12,13,14,16,17,18,19,20,21,22,23,24,25,],[7,7,26,7,7,7,30,31,32,33,34,35,36,37,38,39,]),}

_lr_goto = {}
for _k, _v in _lr_goto_items.items():
   for _x, _y in zip(_v[0], _v[1]):
       if not _x in _lr_goto: _lr_goto[_x] = {}
       _lr_goto[_x][_k] = _y
del _lr_goto_items
_lr_productions = [
  ("S' -> s","S'",1,None,None,None),
  ('s -> p','s',1,'p_S','grammar.py',127),
  ('p -> p OR l','p',3,'p_p_OR_l','grammar.py',135),
  ('p -> l','p',1,'p_p_l','grammar.py',142),
  ('l -> l AND n','l',3,'p_l_AND_K','grammar.py',146),
  ('l -> n','l',1,'p_l_k','grammar.py',153),
  ('n -> NOT k','n',2,'p_r_NOT','grammar.py',157),
  ('n -> k','n',1,'p_r_NOT1','grammar.py',164),
  ('k -> e MAYOR e','k',3,'p_k_ma','grammar.py',168),
  ('k -> e MENOR e','k',3,'p_k_me','grammar.py',174),
  ('k -> e MAYORIGUAL e','k',3,'p_k_maigual','grammar.py',181),
  ('k -> e MENORIGUAL e','k',3,'p_k_meigual','grammar.py',187),
  ('k -> e IGUAL e','k',3,'p_k_ig','grammar.py',194),
  ('k -> e DIFERENTE e','k',3,'p_k_dif','grammar.py',201),
  ('k -> e','k',1,'p_k_ee','grammar.py',208),
  ('e -> e SUMA e','e',3,'p_E','grammar.py',213),
  ('e -> e RESTA e','e',3,'p_E','grammar.py',214),
  ('e -> e MULTIPLICACION e','e',3,'p_E','grammar.py',215),
  ('e -> e DIVISION e','e',3,'p_E','grammar.py',216),
  ('e -> RESTA e','e',2,'p_e_min','grammar.py',239),
  ('e -> ID','e',1,'p_e_ID','grammar.py',247),
  ('e -> ENTERO','e',1,'p_e_ID','grammar.py',248),
  ('e -> DECIMAL','e',1,'p_e_ID','grammar.py',249),
  ('e -> ParA p ParC','e',3,'p_e_PAR','grammar.py',255),
]
