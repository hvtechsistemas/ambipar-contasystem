#INCLUDE "PROTHEUS.CH"

//Ponto de entrada adiciona bot�o no contas a receber

User Function FA740BRW()
	private aRet := {}

	AADD( aRet, { "Transf. Filial"		    , "U_TRFCTASRECEBER()",	      0 , 6})

return(aRet)
