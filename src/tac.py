#Code stores array of all three address codes
code = {'program':[]}
# quad = {'program':-1}
# nextQuad = {"program":0}

tempVarBaseName = "var"
varCount = 0

def getNewTempVar():
	global varCount
	varCount += 1
	return tempVarBaseName + str(varCount)


# def incrementQuad(functionName):
# 	global quad
# 	quad[functionName] = quad[functionName] + 1
# 	return quad[functionName]

def getNextQuad(functionName):
	# return quad[functionName] + 1
	return len(code[functionName])

def getCodeLength(functionName):
	return len(code[functionName]) - 1

def emitCode(functionName, regDest, regSrc1, regSrc2, op):
	global code
	code[functionName].append([regDest, regSrc1, regSrc2, op])
	# incrementQuad(functionName)

def createNewFucntionCode(functionName):
	global code , quad
	# quad[functionName] = -1
	code[functionName] = []

def printCode():
	for functionName in code.keys():
		print functionName,":"
		for i in range(len(code[functionName])):
			print  "%5d: \t" %i, code[functionName][i]

def merge(list1, list2):
	return list1+list2

def backPatch(functionName, locationList, location):
	global code
	for position in locationList:
		code[functionName][position][2] = location

def noop(functionName, locationList):
	global code
	for position in locationList:
		code[functionName][position][3] = 'NOOP'




	


