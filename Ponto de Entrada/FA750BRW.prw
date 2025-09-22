#INCLUDE "PROTHEUS.CH"

//Ponto de entrada adiciona botão no contas a pagar

User Function FA750BRW()
	private aRet := {}

	AADD( aRet, { "Transf. Pagar"		    , "U_TRFCTAPAGAR()",	      0 , 6})
	AADD( aRet, { "Transf. Receber"		    , "U_TRFCTASRECEBER()",	      0 , 6})

return(aRet)
