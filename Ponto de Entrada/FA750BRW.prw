#INCLUDE "PROTHEUS.CH"

//Ponto de entrada adiciona bot�o no contas a pagar

User Function FA750BRW()
	private aRet := {}

	AADD( aRet, { "Transf. Filial"		    , "U_TRFCTAPAGAR()",	      0 , 6})

return(aRet)
