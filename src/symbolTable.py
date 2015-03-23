from pprint import pprint

symbolTable = {
	"program" : {
		"scopeName"		: "program",
		"type"			: "function",
		"returnType"	: "none",
	}
}

offsetStack	= [0]
scopeStack	= [symbolTable["program"]]

def printSymbolTable():
	pprint(symbolTable)

def lookup(identifier):
	global scopeStack
	currentScope = len(scopeStack)
	return lookupScopeStack(identifier, currentScope - 1)

def lookupScopeStack(identifier, position):
	if position == -1:
		return None
	global scopeStack
	currentScope = scopeStack[len(scopeStack) - 1]
	# if sought identifier is not in current scope, it may be in parent
	if identifier in currentScope:
		return currentScope[identifier]
	else:
		return lookupScopeStack(identifier, position - 1)

def getCurrentScope():
	global scopeStack
	return scopeStack[len(scopeStack) - 1]["scopeName"]
	# ensure every scope has this key





# print getCurrentScope()