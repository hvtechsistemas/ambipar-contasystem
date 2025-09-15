#INCLUDE "PROTHEUS.CH"

//Ponto de entrada adiciona botão no contas a pagar

User Function FA750BRW()
	private aRet := {}

	AADD( aRet, { "Transf. Titulos"		    , "U_AMBFIN01(1)",	      0 , 6})

return(aRet)
