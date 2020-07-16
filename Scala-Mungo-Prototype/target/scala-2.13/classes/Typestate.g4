grammar Typestate;

// Info: https://github.com/antlr/antlr4/blob/master/doc/parser-rules.md

start :
  ( p=package_statement  )? ( i+=import_statement )* t=typestate_declaration

;

ref :
  id  |
  r=ref '.' id
;

package_statement :
  t='package' ref
;

import_statement :
  t='import' s='static'? ref ( '.' star='*' )?
;

typestate_declaration:
  t='typestate' ID '{' typestate_body '}' EOF
;

typestate_body  :
  ( s+=state_declaration )*
;

state_declaration:
  name=ID '=' state
;

state:
  t='{' ( m+=method ( ',' m+=method )* ( ',' DROP ':' END )? )? '}'
;

method:
  name=ID'(' ( args+=ref ( ',' args+=ref )* )? ')' ':' return_type=ref '=>' (
    ref  |
    state  |
    decision_state
  )
;

decision_state  :
  t='<' decisions+=decision ( ',' decisions+=decision )* '>'
;

decision :
  label=ID '=>' (
    ref  |
    state
  )
;

id  :
  DROP  |
  END  |
  ID
;

// keywords
DROP : 'drop' ;
END : 'end' ;

// match identifiers
ID : [$_a-zA-Z]+[$_a-zA-Z0-9]* ;

// skip spaces, tabs, newlines
WS : [ \t\r\n]+ -> skip ;

// skip comments
BlockComment : '/*' .*? '*/' -> skip ;
LineComment : '//' ~[\r\n]* -> skip ;