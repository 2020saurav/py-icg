#!/usr/bin/python
import yacc
import lexer # our lexer
tokens = lexer.tokens
from subprocess import call
import sys
from tac import *
from symbolTable import *
# file_input: (NEWLINE | stmt)* ENDMARKER
def p_file_input(p):
	"""file_input :	single_stmt ENDMARKER
	"""
	p[0] = p[1]
	# noop(getCurrentScope(), p[1]['beginlist'])
	# noop(getCurrentScope(), p[1]['endlist'])
	emit(getCurrentScope(), '','', -1, 'HALT')
	addAttributeToCurrentScope('numParam', 0)
	removeCurrentScope()
	printCode()

# Our temporary symbol
def p_single_stmt(p):
	"""single_stmt	:	single_stmt NEWLINE
					|
	"""
	if len(p) == 3:
		p[0] = p[1]
	else:
		p[0] = []

def p_single_stmt1(p):
	"""single_stmt	:	single_stmt stmt
	"""
	p[0] = p[1] + [p[2]]


# funcdef: [decorators] 'def' NAME parameters ':' suite
def p_funcdef(p):
    """funcdef : DEF NAME MarkerScope parameters MarkerArg COLON suite
    """
    noop(getCurrentScope(), p[7]['beginlist'])
    noop(getCurrentScope(), p[7]['endlist'])
    emit(getCurrentScope(), '', '', '', 'JUMP_RETURN')
    # print getCurrentScope()
    removeCurrentScope()
    p[0] = dict()
    p[0]['type'] = 'FUNCTION'
    p[0]['name'] = p[3]['name']


def p_MarkerScope(p):
	"""MarkerScope 	:
	"""
	p[0] = dict()
	p[0]['name'] = p[-1]
	if existsInCurrentScope(p[0]['name']):
		redefinitionError(p[0]['name'])
	else:
		addIdentifier(p[0]['name'], 'FUNCTION')
		place = getNewTempVar()
		addAttribute(p[0]['name'], getCurrentScope(), place)
		addAttribute(p[0]['name'], 'name', p[0]['name'])
		# emit(getCurrentScope(), place, p[0]['name'], '', 'REF')
		addScope(p[0]['name'])
		createNewFucntionCode(p[0]['name'])

def p_MarkerArg(p):
	"""MarkerArg 	:
	"""
	for arg in p[-1]:
		if existsInCurrentScope(arg):
			redefinitionError(arg)
		else:
			addIdentifier(arg, 'UNDEFINED')
			place = getNewTempVar()
			addAttribute(arg, getCurrentScope(), place)
	addAttributeToCurrentScope('numParam', len(p[-1]))

# parameters: '(' [varargslist] ')'
def p_parameters(p):
	"""parameters 	: LPAREN RPAREN 
					| LPAREN varargslist RPAREN"""
	if len(p) == 3:
		p[0] = []
	else:
		p[0] = p[2]

def p_function_call(p):
	"""function_call 	: NAME LPAREN RPAREN 
						| NAME LPAREN testlist RPAREN 
	"""
	p[0] = dict()
	place = ''
	if not exists(p[1]):
		referenceError(p[1])
	else :
		identifierType = getAttribute(p[1], 'type')
		if identifierType == 'FUNCTION':
			if len(p)==4:
				pass
			else:
				for param in p[3]:
					emit(getCurrentScope(), param['place'], '', '', 'PARAM')

			emit(getCurrentScope(), '', '', p[1], 'JUMPLABEL')
			# fname = getAttribute(p[1], 'name')
			fname = p[1]
			# print fname
			p[0]['type'] = getAttributeFromFunctionList(fname, 'returnType')
			returnPlace = getNewTempVar()
			emit(getCurrentScope(), returnPlace, '', '', 'FUNCTION_RETURN')
			p[0]['place'] = returnPlace
		else :
			referenceError(p[1])
	# p[0]['type'] = 'UNDEFINED'
#varargslist: fpdef ['=' test] (',' fpdef ['=' test])* 
def p_varargslist(p):
	"""varargslist 	: fpdef
					| fpdef EQUAL test
	"""
	if len(p) == 2:
		p[0] = [p[1]]
	else:
		pass

def p_varargslistext(p):
	"""varargslist 	: fpdef COMMA varargslist
					| fpdef EQUAL test COMMA varargslist
	"""
	if len(p) == 4:
		p[0] = [p[1]] + p[3]
	else:
		pass

# fpdef: NAME | '(' fplist ')'
def p_fpdef(p):
	"""fpdef 	: NAME 
				| LPAREN fplist RPAREN
	"""
	if len(p) == 2:
		p[0] = p[1]
	else:
		pass

# fplist: fpdef (',' fpdef)* [',']
def p_fplist(p):
	"""fplist 	: fpdef
				| fpdef COMMA fplist	
	"""
	if len(p) == 2:
		p[0] = [p[1]]
	else:
		pass


# stmt: simple_stmt | compound_stmt
def p_stmt(p):
	"""stmt 	: simple_stmt
				| compound_stmt
	"""
	p[0] = p[1]

# simple_stmt: small_stmt  NEWLINE

def p_simple_stmt(p):
	"""simple_stmt 	: small_stmts NEWLINE
	"""
	p[0] = p[1]

def p_small_stmts(p):
	"""small_stmts 	: small_stmt
	"""
	p[0] = p[1]

# small_stmt: 	expr_stmt 	| print_stmt   	| 
#			  	pass_stmt 	| flow_stmt 	|assert_stmt|
#    			import_stmt | global_stmt 	

# def p_small_stmt(p):
# 	"""small_stmt 	: flow_stmt
# 					| expr_stmt
# 					| print_stmt
# 					| pass_stmt
# 					| import_stmt
# 					| global_stmt
# 					| assert_stmt
# 					"""
## CHANGING GRAMMAR : pass, import, global, assert : not urgent
def p_small_stmt(p):
	"""small_stmt 	: flow_stmt Marker
					| expr_stmt Marker
					| print_stmt Marker
	"""
	p[0] = p[1]
	backpatch(getCurrentScope(), p[1].get('nextlist', []), p[2]['quad'])


# expr_stmt: testlist (augassign testlist | ('=' testlist)*)
# def p_expr_stmt(p):
# 	"""expr_stmt 	: testlist augassign testlist
# 					| testlist eqtestlist
# 	"""
## CHANGING GRAMMAR : Removing fancy operations
## CHANGING GRAMMAR : Removing list assignment multiple
def p_expr_stmt(p):
	"""expr_stmt 	: test EQUAL test
					| test EQUAL function_call
	"""
	p[0] = dict()
	place = ''
	if exists(p[1]['name']):
		addAttribute(p[1]['name'], 'type', p[3]['type'])
		if existsInCurrentScope(p[1]['name']):
			place = getAttribute(p[1]['name'], getCurrentScope())
		else:
			place = getNewTempVar()
			addAttribute(p[1]['name'], getCurrentScope(), place)
	else:
		addIdentifier(p[1]['name'], p[3]['type'])
		place = getNewTempVar()
		addAttribute(p[1]['name'], getCurrentScope(), place)
	p[0]['nextlist'] = []
	try:
		emit(getCurrentScope(),place, p[3]['place'], '', '=')
	except:		
		referenceError(p)
	# printST()


	# TODO Add functions for identifier declaration and assignment
	# How about expr_stmt -> NAME EQUAL test ?
# our new symbol
## CHANGING GRAMMAR : No longer required
# def p_eqtestlist(p):
# 	"""eqtestlist 	:
# 					| eqtestlist EQUAL testlist
# 	"""

# augassign: ('+=' | '-=' | '*=' | '/=' | '%=' | '**=' | '//=')
## CHANGING GRAMMAR : No longer required
# def p_augassign(p):
# 	"""augassign 	: PLUSEQUAL 
# 					| MINEQUAL 
# 					| STAREQUAL 
# 					| SLASHEQUAL 
# 					| PERCENTEQUAL 
# 	"""

# print_stmt: 'print' [ test (',' test)* [','] ]
def p_print_stmt(p):
	"""print_stmt 	:	PRINT
					|	PRINT testlist
	"""
	p[0] = dict()
	if len(p)==2:
		emit(getCurrentScope(), '"\n"', '', 'STRING', 'PRINT')
	else:
		for item in p[2]:
			itemType = item.get('type')
			if itemType not in ['STRING', 'NUMBER', 'BOOLEAN', 'UNDEFINED']:
				printError(p)
			emit(getCurrentScope(), item['place'], '', itemType, 'PRINT')
		emit(getCurrentScope(), '"\n"', '', 'STRING', 'PRINT')



# pass_stmt: 'pass'
# def p_pass_stmt(p):
# 	"pass_stmt : PASS"

# flow_stmt: break_stmt | continue_stmt | return_stmt 
def p_flow_stmt(p):
	"""flow_stmt 	: break_stmt Marker
					| continue_stmt Marker
					| return_stmt Marker
	"""
	p[0] = p[1]
	backpatch(getCurrentScope(), p[1].get('nextlist', []), p[2]['quad'])

# break_stmt: 'break'
def p_break_stmt(p):
	"""break_stmt 	: BREAK
	"""
	p[0] = dict()
	p[0]['endlist'] = [getNextQuad(getCurrentScope())]
	emit(getCurrentScope(), '', '', -1, 'GOTO')

# continue_stmt: 'continue'
def p_continue_stmt(p):
	"""continue_stmt 	: CONTINUE
	"""
	p[0] = dict()
	p[0]['beginlist'] = [getNextQuad(getCurrentScope())]
	emit(getCurrentScope(), '', '', -1, 'GOTO')

# return_stmt: 'return' [testlist]
def p_return_stmt(p):
	"""return_stmt 	:	RETURN 
					|	RETURN test
	"""
	p[0] = dict()
	if len(p) == 2:
		addAttributeToCurrentScope('returnType', 'UNDEFINED')
		emit(getCurrentScope(), '', '', '', 'RETURN')				
	else:
		returnType = getAttributeFromCurrentScope('returnType')
		if returnType == 'UNDEFINED':
			if p[2]['type'] == 'FUNCTION':
				addAttributeToCurrentScope('returnType', 'UNDEFINED')
			else:
				addAttributeToCurrentScope('returnType', p[2]['type'])
		elif p[2]['type'] != returnType:
			typeError(p)
		else:
			pass
		emit(getCurrentScope(), p[2]['place'], '', '', 'RETURN')

# TODO:
# import_stmt: 'import' NAME
# def p_import_stmt(p): 
# 	"""import_stmt 	:	IMPORT NAME
# 	"""

# global_stmt: 'global' NAME (',' NAME)*
# def p_global_stmt(p):
# 	"""global_stmt 	: GLOBAL NAME namelist
# 	"""

## CHANGING GRAMMAR : No longer needed
# def p_namelist(p):
# 	"""namelist 	: 
# 					| COMMA NAME namelist
# 	"""
# assert_stmt: 'assert' test [',' test]
## CHANGING GRAMMAR : No longer needed
# def p_assert_stmt(p):
# 	"""assert_stmt 	: ASSERT testlist
# 	"""

# compound_stmt: if_stmt | while_stmt | for_stmt | funcdef | classdef 
def p_compound_stmt(p):
	"""compound_stmt 	: if_stmt Marker
						| for_stmt Marker
						| while_stmt Marker
						| funcdef Marker
						| classdef Marker
						| function_call Marker
	"""
	p[0] = p[1]
	nextlist = p[1].get('nextlist',[])
	backpatch(getCurrentScope(), nextlist, p[2]['quad'])


# if_stmt: 'if' test ':' suite ('elif' test ':' suite)* ['else' ':' suite]
# def p_if_stmt(p):
# 	"""if_stmt 	:	IF test COLON suite elif_list
# 				|	IF test COLON suite elif_list ELSE COLON suite
# 	"""
def p_if_stmt(p):
	"""if_stmt 	:	IF test COLON MarkerIf suite
				|	IF test COLON MarkerIf suite ELSE COLON MarkerElse suite
	"""
	p[0] = dict()
	if p[2]['type'] != 'BOOLEAN':
		typeError(p)
		
	if len(p) == 6:
		p[0]['nextlist'] = merge(p[4].get('falselist', []), p[5].get('nextlist', []))
		p[0]['beginlist'] = p[5].get('beginlist', [])
		p[0]['endlist'] = p[5].get('endlist', [])
	else:
		backpatch(getCurrentScope(), p[4]['falselist'], p[8]['quad'])
		p[0]['nextlist'] = p[8]['nextlist']
		p[0]['beginlist'] = merge(p[9].get('beginlist', []), p[5].get('beginlist', []))
		p[0]['endlist'] = merge(p[9].get('endlist', []), p[5].get('endlist', []))


# while_stmt: 'while' test ':' suite
def p_while_stmt(p):
	"""while_stmt 	:	WHILE Marker test COLON MarkerWhile suite 
	"""
	if p[3]['type'] != 'BOOLEAN':
		typeError(p)
	
	p[0] = dict()
	p[0]['type'] = 'VOID'
	p[0]['nextlist'] = []
	backpatch(getCurrentScope(), p[6]['beginlist'], p[2]['quad'])
	p[0]['nextlist'] = merge(p[6].get('endlist', []), p[6].get('nextlist', []))
	p[0]['nextlist'] = merge(p[5].get('falselist', []), p[0].get('nextlist', []))
	emit(getCurrentScope(),'', '', p[2]['quad'], 'GOTO')
 
def p_Marker(p):
	"""Marker 		:	
	"""
	p[0] = dict()
	p[0]['quad'] = getNextQuad(getCurrentScope())

def p_MarkerWhile(p):
	"""MarkerWhile 	:
	"""
	p[0] = dict()
	p[0]['falselist'] = [getNextQuad(getCurrentScope())]
	emit(getCurrentScope(), p[-2]['place'], 0, -1,'COND_GOTO') 

def p_MarkerIf(p):
	"""MarkerIf 	:
	"""
	p[0] = dict()
	p[0]['falselist'] = [getNextQuad(getCurrentScope())]
	emit(getCurrentScope(), p[-2]['place'], 0, -1, 'COND_GOTO')

def p_MarkerElse(p):
	"""MarkerElse 	:
	"""
	p[0] = dict()
	p[0]['nextlist'] = [getNextQuad(getCurrentScope())]
	emit(getCurrentScope(), '', '', -1, 'GOTO')
	p[0]['quad'] = getNextQuad(getCurrentScope())


# for_stmt: 'for' exprlist 'in' testlist ':' suite ['else' ':' suite]
# def p_for_stmt(p): 
# 	"""for_stmt 	:	FOR exprlist IN testlist COLON suite
# 					|	FOR exprlist IN testlist COLON suite ELSE COLON suite
# 	"""
## CHANGING GRAMMAR : Removing ELSE from FOR statement
## CHANGING GRAMMAR : Simplifying for loop for single iterator
def p_for_stmt(p): 
	"""for_stmt 	:	FOR expr IN test COLON suite
	"""
# suite: simple_stmt | NEWLINE INDENT stmt+ DEDENT
def p_suite(p):
	"""suite 	: simple_stmt
				| NEWLINE INDENT stmts DEDENT"""
	if len(p) == 2:
		p[0] = p[1]
	else:
		p[0] = p[3]

# test: or_test
def p_test(p):
	"""test 	: or_test
	"""
	p[0] = p[1]


# or_test: and_test ('or' and_test)*
def p_or_test(p):
	"""or_test 	: and_test
				| and_test OR or_test
	"""
	if len(p)==2:
		p[0] = p[1]
	else:
		if p[1]['type'] == p[3]['type'] == 'BOOLEAN':
			p[0] = dict()
			p[0]['place'] = getNewTempVar()
			p[0]['type'] = 'BOOLEAN'
			emit(getCurrentScope(),p[0]['place'], p[1]['place'], p[3]['place'], p[2])
		else:
			if(p[1]['type']=='REFERENCE_ERROR' or p[3]['type']=='REFERENCE_ERROR'):
				referenceError(p)
			typeError(p)		

# and_test: not_test ('and' not_test)*
def p_and_test(p):
	"""and_test 	: not_test
					| not_test AND and_test
	"""
	if len(p)==2:
		p[0] = p[1]
	else:
		if p[1]['type'] == p[3]['type'] == 'BOOLEAN':
			p[0] = dict()
			p[0]['place'] = getNewTempVar()
			p[0]['type'] = 'BOOLEAN'
			emit(getCurrentScope(),p[0]['place'], p[1]['place'], p[3]['place'], p[2])
		else:
			if(p[1]['type']=='REFERENCE_ERROR' or p[3]['type']=='REFERENCE_ERROR'):
				referenceError(p)
			typeError(p)

# not_test: 'not' not_test | comparison
def p_not_test(p):
	"""not_test 	: NOT not_test
					| comparison
	"""
	if len(p)==2:
		p[0] = p[1]
	else:
		if p[2]['type'] == 'BOOLEAN':
			p[0] = dict()
			p[0]['place'] = getNewTempVar()
			p[0]['type'] = 'BOOLEAN'
			emit(getCurrentScope(),p[0]['place'], p[2]['place'],'', p[1])
		else:
			if(p[2]['type']=='REFERENCE_ERROR'):
				referenceError(p)
			typeError(p)

# comparison: expr (comp_op expr)*
# def p_comparision(p):
# 	"""comparison 	: expr compexprlist
# 	"""
## CHANGING GRAMMAR : comparision expression shortened, one at a time
def p_comparision(p):
	"""comparison 	: 	expr
					|	expr comp_op expr
	"""
	if len(p)==2:
		p[0] = p[1]
	elif len(p)==4:
		if p[1]['type'] == p[3]['type']=='NUMBER':
			pass
			# okay : Nothing to do here
		else:
			if(p[1]['type']=='REFERENCE_ERROR' or p[3]['type']=='REFERENCE_ERROR'):
				referenceError(p)
			typeError(p)
		p[0] = dict()
		p[0]['type'] = 'BOOLEAN'
		p[0]['place'] = getNewTempVar()
		emit(getCurrentScope(),p[0]['place'], p[1]['place'], p[3]['place'], p[2])


## CHANGING GRAMMAR : No longer needed
# our new symbol
# def p_compexprlist(p):
# 	"""compexprlist 	:
# 						| comp_op expr compexprlist
# 	"""

# comp_op: '<'|'>'|'=='|'>='|'<='|'!='|'in'|'not' 'in'|'is'|'is' 'not'
# def p_comp_op(p):
# 	"""comp_op 	: LESS
# 				| GREATER
# 				| EQEQUAL
# 				| GREATEREQUAL
# 				| LESSEQUAL
# 				| NOTEQUAL
# 				| IN
# 				| NOT IN
# 				| IS
# 				| IS NOT
# 	"""
## CHANGING GRAMMAR : Withdrawing support of IN and IS
def p_comp_op(p):
	"""comp_op 	: LESS
				| GREATER
				| EQEQUAL
				| GREATEREQUAL
				| LESSEQUAL
				| NOTEQUAL
	"""
	p[0] = p[1]

# expr: xor_expr ('|' xor_expr)*
def p_expr(p):
	"""expr 	: xor_expr
				| xor_expr VBAR expr
	"""
	if len(p)==2:
		p[0] = p[1]
	else:
		if p[1]['type'] == p[3]['type'] == 'NUMBER':
			p[0] = dict()
			p[0]['place'] = getNewTempVar()
			p[0]['type'] = 'NUMBER'
			emit(getCurrentScope(),p[0]['place'], p[1]['place'], p[3]['place'], p[2])
		else:
			if(p[1]['type']=='REFERENCE_ERROR' or p[3]['type']=='REFERENCE_ERROR'):
				referenceError(p)
			typeError(p)

# xor_expr: and_expr ('^' and_expr)*
def p_xor_expr(p):
	"""xor_expr 	: and_expr
					| and_expr CIRCUMFLEX xor_expr
	"""
	if len(p)==2:
		p[0] = p[1]
	else:
		if p[1]['type'] == p[3]['type'] == 'NUMBER':
			p[0] = dict()
			p[0]['place'] = getNewTempVar()
			p[0]['type'] = 'NUMBER'
			emit(getCurrentScope(),p[0]['place'], p[1]['place'], p[3]['place'], p[2])
		else:
			if(p[1]['type']=='REFERENCE_ERROR' or p[3]['type']=='REFERENCE_ERROR'):
				referenceError(p)
			typeError(p)

# and_expr: shift_expr ('&' shift_expr)*
def p_and_expr(p):
	"""and_expr 	: shift_expr
					| shift_expr AMPER and_expr
	"""
	if len(p)==2:
		p[0] = p[1]
	else:
		if p[1]['type'] == p[3]['type'] == 'NUMBER':
			p[0] = dict()
			p[0]['place'] = getNewTempVar()
			p[0]['type'] = 'NUMBER'
			emit(getCurrentScope(),p[0]['place'], p[1]['place'], p[3]['place'], p[2])
		else:
			if(p[1]['type']=='REFERENCE_ERROR' or p[3]['type']=='REFERENCE_ERROR'):
				referenceError(p)
			typeError(p)

# shift_expr: arith_expr (('<<'|'>>') arith_expr)*
def p_shift_expr(p):
	"""shift_expr 	: arith_expr
					| arith_expr LEFTSHIFT shift_expr
					| arith_expr RIGHTSHIFT shift_expr
	"""
	if len(p)==2:
		p[0] = p[1]
	else:
		if p[1]['type'] == p[3]['type'] == 'NUMBER':
			p[0] = dict()
			p[0]['place'] = getNewTempVar()
			p[0]['type'] = 'NUMBER'
			emit(getCurrentScope(),p[0]['place'], p[1]['place'], p[3]['place'], p[2])
		else:
			if(p[1]['type']=='REFERENCE_ERROR' or p[3]['type']=='REFERENCE_ERROR'):
				referenceError(p)
			typeError(p)

# arith_expr: term (('+'|'-') term)*
def p_arith_expr(p):
	"""arith_expr 	:	term
					|	term PLUS arith_expr
					|	term MINUS arith_expr
	"""
	if len(p)==2:
		p[0] = p[1]
	else:
		if p[1]['type'] == p[3]['type'] == 'NUMBER':
			p[0] = dict()
			p[0]['place'] = getNewTempVar()
			p[0]['type'] = 'NUMBER'
			emit(getCurrentScope(),p[0]['place'], p[1]['place'], p[3]['place'], p[2])
		else:
			if(p[1]['type']=='REFERENCE_ERROR' or p[3]['type']=='REFERENCE_ERROR'):
				referenceError(p)
			typeError(p)


# term: factor (('*'|'/'|'%'|'//') factor)*
def p_term(p):
	"""term :	factor
			|	factor STAR term
			|	factor SLASH term
			|	factor PERCENT term
	"""
	if len(p)==2:
		p[0] = p[1]
	else:
		if p[1]['type'] == p[3]['type'] == 'NUMBER':
			p[0] = dict()
			p[0]['place'] = getNewTempVar()
			p[0]['type'] = 'NUMBER'
			emit(getCurrentScope(), p[0]['place'], p[1]['place'], p[3]['place'], p[2])
		else:
			if(p[1]['type']=='REFERENCE_ERROR' or p[3]['type']=='REFERENCE_ERROR'):
				referenceError(p)
			typeError(p)

# factor: ('+'|'-') factor | power
def p_factor(p):
	"""factor 	: power
				| PLUS factor
				| MINUS factor
	"""
	if len(p) == 2:
		p[0] = p[1]
	else:
		if p[2]['type'] != 'NUMBER':
			typeError(p)
		p[0] = dict()
		p[0]['place'] = getNewTempVar()
		p[0]['type'] = 'NUMBER'
		emit(getCurrentScope(), p[0]['place'], 0, p[2]['place'], '-')
# power: atom trailer*
def p_power(p):
	"""power 	: atom
	"""
	p[0] = p[1]
# our new symbol
# CHANGING GRAMMAR : not needed, unless above is reverted
# def p_trailerlist(p):
# 	"""trailerlist 	: 
# 					| trailer trailerlist
# 	"""

# atom: ('(' [testlist_comp] ')' |
#       '[' [listmaker] ']' |
#       '{' [dictorsetmaker] '}' |
#       '`' testlist1 '`' |
#       NAME | NUMBER | STRING+)
# def p_atom(p):
# 	"""atom 	: LPAREN RPAREN
# 				| LPAREN testlist_comp RPAREN
# 				| LSQB RSQB
# 				| LSQB listmaker RSQB
# 				| LBRACE RBRACE
# 				| LBRACE dictorsetmaker RBRACE
# 				| BACKQUOTE testlist1 BACKQUOTE
# 				| NAME
# 				| NUMBER
# 				| FNUMBER
# 				| stringlist
# 	"""
## CHANGING GRAMMAR: separating out to give types
def p_atom(p):
	"""atom 	: LBRACE RBRACE
				| LBRACE dictorsetmaker RBRACE
				| BACKQUOTE testlist1 BACKQUOTE
	"""
def p_atom1(p):
	'''atom :	NAME
	'''
	p[0] = dict()
	# p[0]['place'] = p[1]
	if exists(p[1]):
		p[0]['name'] = p[1]
		p[0]['type'] = getAttribute(p[1], 'type')
		p[0]['place'] = getAttribute(p[1], getCurrentScope())
		# TODO may need to add more keys like offset and scopename
	else:
		p[0]['name'] = p[1]
		p[0]['type'] = 'REFERENCE_ERROR'


	# need to do symbol table thing, type attribution
def p_atom2(p):
	'''atom :	NUMBER
	'''
	p[0] = dict()
	p[0]['type'] = 'NUMBER'
	p[0]['place'] = p[1]
	# need to do symbol table thing, type attribution
# our new symbol
# def p_stringlist(p):
# 	"""stringlist 	: STRING 
# 					| STRING stringlist
# 					| TRIPLESTRING
# 					| TRIPLESTRING stringlist
# 	"""

def p_atom3(p):
	"""atom		:	STRING
				|	TRIPLESTRING
	"""
	p[0] = dict()
	p[0]['type'] = 'STRING'
	p[0]['place'] = p[1]

def p_atom4(p):
	'''atom :	FNUMBER
	'''
	p[0] = dict()
	p[0]['type'] = 'NUMBER'
	p[0]['place'] = p[1]

def p_atom5(p):
	'''atom :	LSQB RSQB
			| 	LSQB listmaker RSQB
	'''
	p[0] = dict()
	if len(p)==3:
		p[0]['place'] = []
		p[0]['type'] = 'UNDEFINED'
	else:
		p[0] = p[2]

def p_atom6(p): 
	"""atom	:	LPAREN RPAREN
			| 	LPAREN testlist_comp RPAREN
	"""
	if len(p) == 3:
		p[0] = dict()
	else:
		p[0] = p[2]
# listmaker: test (',' test)* [','] 
def p_listmaker(p):
	"""listmaker 	: test
					| test COMMA listmaker
	"""
	p[0] = dict()
	if len(p) == 2:
		try:
			p[0]['place'] = [p[1]['place']]
			p[0]['type'] = p[1]['type']
		except:
			referenceError(p)
	else:
		if p[3]['type'] != p[1]['type']:
			typeError(p)
		else:
			try:
				p[0]['place'] = [p[1]['place']] + p[3]['place']
				p[0]['type'] = p[1]['type']
			except:
				referenceError(p)

# testlist_comp: test (',' test)* [','] 
def p_testlist_comp(p):
	"""testlist_comp 	: test
						| test COMMA testlist_comp
	"""
	if len(p)==2:
		p[0] = p[1]
	else:
		p[0] = [p[1]] + p[2]
		# Not very sure.

# CHANGING GRAMMAR : May be dangerous. Need to revert above trailerlist too
# trailer: '(' [arglist] ')' | '[' subscriptlist ']' | '.' NAME
# def p_trailer(p):
# 	"""trailer 	: LPAREN RPAREN
# 				| LPAREN arglist RPAREN
# 				| LSQB subscriptlist RSQB
# 				| DOT NAME
# 	"""

# CHANGING GRAMMAR : May be dangerous. Need to revert above trailerlist too
# subscriptlist: subscript (',' subscript)* [',']
# def p_subscriptlist(p):
# 	"""subscriptlist 	: subscript
# 						| subscript COMMA
# 						| subscript COMMA subscriptlist
# 	"""

# subscript: '.' '.' '.' | test | [test] ':' [test] [sliceop]
# CHANGING GRAMMAR : May be dangerous. Need to revert above trailerlist too
# def p_subscript(p):
# 	"""subscript 	: DOT DOT DOT
# 					| test
# 					| test COLON test sliceop
# 					| COLON test sliceop
# 					| test COLON sliceop
# 					| test COLON test
# 					| test COLON
# 					| COLON test
# 					| COLON sliceop
# 					| COLON
# 	"""

# sliceop: ':' [test]
# CHANGING GRAMMAR : May be dangerous. Need to revert above trailerlist too
# def p_sliceop(p):
# 	"""sliceop 	: COLON
# 				| COLON test
# 	"""

# exprlist: expr (',' expr)* [',']
## CHANGING GRAMMAR : MAY MAY MAY not be needed.
# def p_exprlist(p):
# 	"""exprlist 	: expr
# 					| expr COMMA
# 					| expr COMMA exprlist
# 	"""

# testlist: test (',' test)* [',']
def p_testlist(p):
	"""testlist 	: test
					| test COMMA testlist
	"""
	if len(p) == 2:
		p[0] = [p[1]]
	else:
		p[0] = [p[1]] + p[3]

# dictorsetmaker:  (test ':' test  (',' test ':' test)* [',']) 
#					| (test  (',' test)* [',']) 
def p_dictorsetmaker(p):
	"""dictorsetmaker 	: testcolonlist
						| testlist
	"""

# our new symbol
def p_testcolonlist(p):
	"""testcolonlist 	: test COLON test
						| test COLON test COMMA
						| test COLON test COMMA testcolonlist
	"""

# classdef: 'class' NAME ['(' [testlist] ')'] ':' suite
def p_classdef(p):
	"""classdef 	: CLASS NAME COLON suite
					| CLASS NAME LPAREN RPAREN COLON suite
					| CLASS NAME LPAREN testlist RPAREN COLON suite
	"""

# CHANGING GRAMMAR : Definitely dangerous. Need to revert. Probably need by fn call
# arglist: (argument ',')* argument [',']
# def p_arglist(p):
# 	"""arglist 	: argument
# 				| argument COMMA
# 				| argument COMMA arglist
# 	"""
# argument: test | test '=' test
# CHANGING GRAMMAR : Definitely dangerous. Need to revert. Probably need by fn call
# def p_argument(p):
# 	"""argument 	: test
# 					| test EQUAL test
# 	"""
# testlist1: test (',' test)*
def p_testlist1(p):
	"""testlist1 	: test
					| test COMMA testlist1
	"""

def p_stmts(p):
	"""stmts 	: stmt stmts
				| stmt Marker"""
	p[0] = dict()
	p[0]['beginlist'] = merge(p[1].get('beginlist', []), p[2].get('beginlist', []))
	p[0]['endlist'] = merge(p[1].get('endlist', []), p[2].get('endlist', []))

def p_error(p):
	try:
		print "Syntax Error near '"+str(p.value)+ "' in line "+str(p.lineno)
	except:
		try:
			print "Syntax Error in line "+str(p.lineno)
		except:
			print "Syntax Error"
	sys.exit()

def typeError(p):
	try:
		print "Type Error near '"+str(p.value)+"' in line "+str(p.lineno)
	except:
		try:
			print "Type Error in line "+str(p.lineno)
		except:
			print "Type Error"
	sys.exit()

def referenceError(p):
	try:
		print "Reference Error near '"+str(p.value)+"' in line "+str(p.lineno)
	except:
		try:
			print "Reference Error in line "+str(p.lineno)
		except:
			print "Reference Error"
	sys.exit()

def redefinitionError(p):
	try:
		print "Redefinition Error near '"+str(p.value)+"' in line "+str(p.lineno)
	except:
		try:
			print "Redefinition Error in line "+str(p.lineno)
		except:
			print "Redefinition Error"
	sys.exit()

def printError(p):
	try:
		print "Print Error near '"+str(p.value)+"' in line "+str(p.lineno)
	except:
		try:
			print "Print Error in line "+str(p.lineno)
		except:
			print "Print Error"
	# sys.exit()


class G1Parser(object):
	def __init__(self, mlexer=None):
		if mlexer is None:
			mlexer = lexer.G1Lexer()
		self.mlexer = mlexer
		self.parser = yacc.yacc(start="file_input", debug=True)
	def parse(self, code):
		self.mlexer.input(code)
		result = self.parser.parse(lexer = self.mlexer, debug=True)
		return result

if __name__=="__main__":
	z = G1Parser()
	# filename = sys.argv[1]
	filename = "../test/test1.py"
	sourcefile = open(filename)
	data = sourcefile.read()
	sys.stderr = open('dump','w')
	root =  z.parse(data)
	sys.stderr.close()
	# call(["python","converter.py", filename])
	# s = filename
	# fname = "../"+s[s.find("/")+1:s.find(".py")]
	# call(["dot","-Tpng",fname+".dot","-o",fname+".png"])
	# call(["gnome-open",fname+".png"])
