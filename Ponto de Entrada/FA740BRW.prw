#INCLUDE "PROTHEUS.CH"

//Ponto de entrada adiciona botão no contas a receber

User Function FA740BRW()
	private aRet := {}

	AADD( aRet, { "Transf. Filial"		    , "U_TRFCTASRECEBER()",	      0 , 6})

return(aRet)
