minic.exe:
	dune build minic.exe

tests: minic.exe
	./_build/default/minic.exe Tests/testInit.mnc
	./_build/default/minic.exe Tests/testSup.mnc
	./_build/default/minic.exe Tests/testSupEqual.mnc
	./_build/default/minic.exe Tests/testInf.mnc
	./_build/default/minic.exe Tests/testInfEqual.mnc
	./_build/default/minic.exe Tests/testEqual.mnc
	./_build/default/minic.exe Tests/testAdd.mnc
	./_build/default/minic.exe Tests/testMul.mnc
	./_build/default/minic.exe Tests/testMinus.mnc
	./_build/default/minic.exe Tests/testSub.mnc
	./_build/default/minic.exe Tests/testDiv.mnc
	./_build/default/minic.exe Tests/testAnd.mnc
	./_build/default/minic.exe Tests/testOr.mnc
	./_build/default/minic.exe Tests/testDiv.mnc
	./_build/default/minic.exe Tests/testGet.mnc
	./_build/default/minic.exe Tests/testCall.mnc
	./_build/default/minic.exe Tests/testWhile.mnc
	./_build/default/minic.exe Tests/testPutchar.mnc
	./_build/default/minic.exe Tests/testIf.mnc
	./_build/default/minic.exe Tests/testVarLoc.mnc
	./_build/default/minic.exe Tests/testVarNonInit.mnc
	./_build/default/minic.exe Tests/testMiniC.mnc


