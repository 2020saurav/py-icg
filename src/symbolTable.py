import pprint
'''
DOCUMENTATION goes here
'''

symbolTable = {
	"program" : {
		"scopeName"		: "program",
		"type"			: "FUNCTION",
		"returnType"	: "UNDEFINED",
	}
}

offsetStack	= [0]
scopeStack	= [symbolTable["program"]]

def lookup(identifier):
	global scopeStack
	currentScope = len(scopeStack)
	return lookupScopeStack(identifier, currentScope - 1)

def lookupScopeStack(identifier, position):
	if position == -1:
		return None
	global scopeStack
	currentScope = scopeStack[position]
	# if sought identifier is not in current scope, it may be in parent
	if identifier in currentScope:
		return currentScope[identifier]
	else:
		return lookupScopeStack(identifier, position - 1)

def getCurrentScope():
	global scopeStack
	return scopeStack[len(scopeStack) - 1]["scopeName"]
	# ensure every scope has this key

def addScope(scopeName):
	global scopeStack
	currentScope = scopeStack[len(scopeStack) - 1]
	currentScope[scopeName] = {
		"scopeName"		: scopeName,
		"parentName"	: currentScope["scopeName"],
		"type"			: "FUNCTION",
		"returnType"	: "UNDEFINED"
	}
	scopeStack.append(currentScope[scopeName])

	# start new relative addressing
	offsetStack.append(0)

def addIdentifier(identifier, identifierType):
	global scopeStack
	currentScope = scopeStack[len(scopeStack) - 1]
	if identifierType == 'NUMBER':
		width = 4
	elif identifierType == 'STRING':
		width = 256
	elif identifierType == 'UNDEFINED':
		width = 0
	# TODO Add other types

	if not identifier in currentScope:
		currentScope[identifier] = dict()
	currentScope[identifier]["offset"] = width
	currentScope[identifier]["type"] = identifierType

	currentOffset = offsetStack.pop() + width
	offsetStack.append(currentOffset)

def addAttribute(identifier, key, value):
	entry = lookup(identifier)
	entry[key] = value

def getAttribute(identifier, key):
	entry = lookup(identifier)
	if key in entry:
		return entry[key]
	else:
		return None

def getAttributeFromCurrentScope(key):
	currentScope = scopeStack[len(scopeStack) - 1]
	return currentScope[key]

def addAttributeToCurrentScope(key, value):
	currentScope = scopeStack[len(scopeStack) - 1]
	currentScope[key] = value

def exists(identifier):
	if lookup(identifier) != None:
		return True
	return False

def existsInCurrentScope(identifier):
	return scopeStack[len(scopeStack)-1].get(identifier, False) != False

def removeCurrentScope():
	global scopeStack
	currentScope = scopeStack.pop()
	currentScope["width"] = offsetStack.pop()

# print scopeStack
def printST():
	print scopeStack