#include "TOTVS.CH"
#INCLUDE "PROTHEUS.CH"
#INCLUDE "TBICONN.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "RPTDEF.CH"
#INCLUDE 'FWMVCDEF.CH'
#INCLUDE "FWEditPanel.CH"

#DEFINE CONFIRMED			.T.
#DEFINE CANCELED			.F.

Static _lCpoEnt05 //Entidade 05
Static _lCpoEnt06 //Entidade 06
Static _lCpoEnt07 //Entidade 07
Static _lCpoEnt08 //Entidade 08
Static _lCpoEnt09 //Entidade 09
Static _cFilLog   //Filial a qual a tela foi aberta
Static __lFA631APV := ExistBlock( "FA631APV" )
Static __lMunic    := .F.
Static __lDedIns   := .F.
Static __lReinf As Logical

/*/{Protheus.doc} AMBFIN02
Função para Aprovar Transferencia de Titulos a Pagar entre filiais

@author Henrique Vieira
@since 14/09/2025
@version 1.0
/*/


User Function AMBFIN02(cAlias,nReg,nOpc)

    DbSelectArea("SED")

    Private cCodNat     := SPACE(LEN(SED->ED_CODIGO))
    Private cNomNat     := SPACE(LEN(SED->ED_DESCRIC))
    Private aRotAuto    := Nil
    Private lImp    	:= (SuperGetMv("MV_IMPTRAN",.F.,"1") == "2" )

    SE6->(dbGoto(nReg))

    XFa631Apv(cAlias,nReg,nOpc)

Return

/*
    Função para aprovar transferencias do contas a pagar
*/

Static Function XFa631Apv(cAlias,nReg,nOpc)

Local lPanelFin 	:= IsPanelFin()
Local nOpcA                                                                
Local aTit 			:= {}
Local nX
Local nSaldo
Local aAcho			:= {}
Local cFilSe2 		:= xFilial("SE2")
Local cFilBkpSE2	:= xFilial("SE2") 
Local cFilOld 		:= cFilAnt // Guarda filial atual
Local cEmpOld		:= cEmpAnt		
Local cChave
Local aRecno 		:= {}
Local aDadosFKF		As Array
Local nY 			:= 0
Local cParcela		:= Space(TamSx3("E2_PARCELA")[1])
Local lSolicAb		:= .T. //(mv_par01==1)	// Apenas solicitacoes em aberto
Local lDtMovFin 	:= .T.
Local lRet			:= .F.
Local cTipo 		:= ""
Local nValBx		:= 0
Local cParcRec		:= Space(TamSx3("E2_PARCELA")[1])
Local aBkpSM0		:= {}
Local lTrocaParc 	:= .F.
Local cParcAux   	:= cParcela
Local cChaveTit		As Character
Local cIdFK7		As Character
Local lExistFKF		As Logical
Local lRetImp 		:= .F.
Local lPccBaixa		:= SuperGetMV("MV_BX10925",.F.,"2") == "1"
Local lIssBaixa		:= SuperGetMV("MV_MRETISS",.F.,"1") == "2"
Local lIRRFBaixa    := .F.
Local nOpca         := 1  

Private lF631Auto := .T.

cChaveTit	:= ''
cIdFK7		:= ''
lExistFKF	:= .F.
aDadosFKF	:= {}

if lF631Auto
	DbSelectArea("SE6")
	DbSetorder(3)
	If (nT := ascan(aRotAuto,{|x| x[1]='E6_NUMSOL'}) ) > 0
		DbSeek(xFilial("SE6")+ Padr(aRotAuto[nT,2],TamSx3('E6_NUMSOL')[1 ]) )
	EndIf
Endif

// Apenas solicitacoes em aberto podem ser aprovadas.
If SE6->E6_SITSOL != "1"
	Return .F.
Endif

// Nao é permitido aprovar transferencias de titulos solicitados para outra filial.
If cFilAnt <> SE6->E6_FILDEB
	Return .F.
Endif

If __lFA631APV
	lDtMovFin := Execblock( "FA631APV", .f., .f. )
    If !lDtMovFin
		Return .F.
    EndIf
EndIf

aAcho := F631Campos(.T.)

If lPanelFin
	RegToMemory(cAlias,.T.,,,FunName())                                       
	oPanelDados := FinWindow:GetVisPanel()
	oPanelDados:FreeChildren()		
	aDim := DLGinPANEL(oPanelDados)		
	nOpca := AxVisual(cAlias,nReg,nOpc,aAcho,,,,,,,.T.,,.T.,.T.,aDim)
Else
	If !lF631Auto
		nOpca := AxVisual(cAlias,nReg,nOpc,aAcho,,,,,,,,,.T.)
	Else
		If ascan(aRotAuto,{|x| x[1]='E6_NUMSOL'})>0 .and. SE6->(DbSeek(xFilial("SE6")+ Padr(aRotAuto[nT,2],TamSx3('E6_NUMSOL')[1]) ))		
			RegToMemory(cAlias,.T.)
			nOpca := 1
		EndIf
	EndIf
Endif	

If nOpcA == 1
	// Filial a ser utilizada para localizar o titulo, FILIAL ORIGEM do SE6 caso o SE2
	// seja exclusivo ou xFilial se o SE2 for compartilhado
	cFilSe2 := If(!Empty(cFilSe2),xFilial("SE2",SE6->E6_FILORIG),cFilSe2)
	cChave := cFilSe2 + SE6->(E6_PREFIXO+E6_NUM+E6_PARCELA+E6_TIPO)
	SE2->(MsSeek(cChave))
	While SE2->(!Eof()) .And. SE2->(E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO) == cChave
		If SE2->E2_NUMSOL <> SE6->E6_NUMSOL	
			SE2->(DbSkip())
			Loop
		Else
			Exit
		EndIf
	EndDo
			
	nRecNoSe2 := SE2->(Recno())
	lMsErroAuto := .F. // variavel interna da rotina automatica	   	

	If SE2->(!Eof())
		BEGIN TRANSACTION
			If RecLock("SE6",.F.)
				
				If SE2->E2_VRETISS > 0 .and. lImp
					//Verifica se as filiais estão localizadas no mesmo municipio
					__lMunic := FMunicFil()
				EndiF	

				SE6->E6_SITSOL := "2" // Solicitacao aprovada
				SE6->E6_USRAPV := RetCodUsr() //Aprovador
				SE6->(MsUnlock())
			   
				// Baixa o titulo na filial de origem
				aTit := {}
				nAbatim := SomaAbat(SE2->E2_PREFIXO,SE2->E2_NUM,SE2->E2_PARCELA,"P",SE2->E2_MOEDA,dDataBase,SE2->E2_FORNECE,SE2->E2_LOJA, cFilSe2)
				nSaldo := (SE2->E2_SALDO - nAbatim)
				nSaldo += SE2->E2_SDACRES
				nSaldo -= SE2->E2_SDDECRE
				SE2->(MsGoTo(nRecNoSe2))
				// Primeiro adiciona os registros a serem transferidos, pois os primeiro
				// deve-se transferir o titulo principal, depois os agragados. Como os agragados podem
				// vir primeiro, entao utiliza-se uma matriz de recnos para transferir os titulos na ordem
				// em que foram incluidos, pois um abatimento nunca eh incluido primeiro que um titulo
				// principal
				// Os titulos de impostos nao se incluem nesta lista visto que os mesmos serao incluidos junto com
				// o titulo principal
				While SE2->(!Eof()) .And. SE2->(E2_FILIAL+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO) == cChave
					If SE2->E2_NUMSOL <> SE6->E6_NUMSOL	
						SE2->(DbSkip())
						Loop
					EndIf

				  	If !(SE2->E2_TIPO $ MVABATIM) .OR. ;
				  		((SE2->E2_TIPO $ MVABATIM) .AND. !(SE2->E2_TIPO $ MVIRABT+"/"+MVINABT+"/"+MVCSABT+"/"+MVCFABT+"/"+MVPIABT+"/"+MVISABT+"/"+MVFUABT+"/"+MVI2ABT))
						Aadd(aRecno, SE2->(Recno()))
					Endif

					SE2->(DbSkip())
				Enddo	
				aSort(aRecno)

				//Zera valor da baixa
				nValBx := 0

				// Transfere todos os titulos agregados
				nY := 1
				
				While !lMsErroAuto .And. nY <= Len(aRecno)
					SE2->(MsGoto(aRecno[nY]))

					aTit       := {}
					lTrocaParc := .F.
					cParcela   := cParcAux

					For nX := 1 To SE2->(fCount())
						If !Empty(SE2->(FieldGet(nX))) .Or. SE2->(FieldName(nX)) == "E2_PARCELA"//A parcela faz parte da chave e precisa estar presente para que seja incrementada no caso de título já existente no destino
							aAdd(aTit, { SE2->(FieldName(nX)), SE2->(FieldGet(nX)), NIL } )
						EndIf
					Next nX

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_FILIAL"})
					If nX > 0
						aTit[nX][2] := SE6->E6_FILDEB // Codigo da filial que recebera o titulo
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_TIPO"})
					If nX > 0
						cTipo := aTit[nX][2]  //Tipo do titulo que sera incluido para verificar a parcela
					Endif 				

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_ORIGEM"})
					If nX > 0
						aTit[nX][2] := "FINA631"
					Endif
					
					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_FORNECE"})
					If nX > 0
						aTit[nX][2] := SE6->E6_CLIENTE // Codigo do cliente que recebera o titulo
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_LOJA"})
					If nX > 0
						aTit[nX][2] := SE6->E6_LOJA // Codigo da loja do cliente que recebera o titulo
					Endif

					__lDedIns := F631DedIns(SE2->E2_NATUREZ)

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_CODRET"})
					If nX > 0
						aTit[nX][2] := SE2->E2_CODRET
					Endif

					If lImp
						F631RecVl(@aTit)		
					Endif	

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_PARCIR"})
					If nX > 0
						aTit[nX][2] := ''
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_PARCINS"})
					If nX > 0
						aTit[nX][2] := ''
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_PARCCOF"})
					If nX > 0
						aTit[nX][2] := ''
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_PARCPIS"})
					If nX > 0
						aTit[nX][2] := ''
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_PARCSLL"})
					If nX > 0
						aTit[nX][2] := ''
					Endif
					
					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_MDRTISS"})
					If nX > 0
						IF Empty(aTit[nX][2])
							aTit[nX][2] := '1'
						EndIf
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_TEMDOCS"})
					If nX > 0
						If(Empty(aTit[nX][2]),aTit[nX][2] := "2",aTit[nX][2]) // Forca o dirf se estiver em branco
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_DIRF"})
					If nX > 0
						If(Empty(aTit[nX][2]),aTit[nX][2] := "2",aTit[nX][2]) // Forca o dirf se estiver em branco
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_DESDOBR"})
					If nX > 0
						aTit[nX][2] := "N" // Forca o desdobramento como nao
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_NUMSOL"})
					If nX > 0
						aTit[nX][2] := "" // Limpa numero da solicitacao na filial destino (debito)
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_VALOR"})
					If cTipo == SE6->E6_TIPO
						If nX > 0
							nValBx += aTit[nX][2]
						Endif
					EndIf

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_DECRESC"})
					If nX > 0
						nValBx -= aTit[nX][2]
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_ACRESC"})
					If nX > 0
						nValBx += aTit[nX][2]
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_MULTNAT"})
					If nX > 0 
						aTit[nX][2] := "2"
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_RATEIO"})
					If nX > 0 
						aTit[nX][2] := "N"
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_ARQRAT"})
					If nX > 0
						aTit[nX][2] := ""						
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_MODSPB"})
					If nX > 0
						If(Empty(aTit[nX][2]),aTit[nX][2]:="1",.T.)						
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_APLVLMN"})
					If nX > 0
						If(Empty(aTit[nX][2]),aTit[nX][2]:="1",.T.)
					Endif
					
					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_PARCELA"})
					If nX > 0
						// Incrementa a parcela para que nao haja registro duplicado na filial Debito
						lTrocaParc := .T.
						If cTipo $ MVABATIM
							aTit[nX][2] := cParcela
						Else
							aTit[nX][2] := SE6->E6_PARCELA
							cFilSe2 := If(!Empty(cFilSe2),xFilial("SE2",SE6->E6_FILDEB),cFilSe2)
							cChave := cFilSe2 + SE6->(E6_PREFIXO+E6_NUM+aTit[nX][2]+cTipo)
							While SE2->(MsSeek(cChave))
								aTit[nX][2] := Soma1(aTit[nX][2])
								cChave := cFilSe2 + SE6->(E6_PREFIXO+E6_NUM+aTit[nX][2]+cTipo)
							Enddo
							cParcela := aTit[nX][2]  //Parcela do titulo Principal
						Endif
					Endif
   					SE2->(MsGoto(aRecno[nY]))//ponteiro pode ter sido desposicionado ao buscar pelas parcelas do Titulo	

					cFilBkpSE2 := SE6->E6_FILORIG
					aTitBx := {}
					// Altera para filial do titulo de origem para fazer a baixa
 					
					aBkpSM0 := SM0->(GetArea())
					cFilAnt := cFilBkpSE2
					cFilSe2 := If(!Empty(cFilSE2),xFilial("SE2",SE6->E6_FILORIG),cFilSE2)

					If !Empty( cFilAnt )
 						SM0->(DbSetOrder(1))
						SM0->(DbSeek(cEmpAnt+cFilAnt))
					Else
 						SM0->(DbSetOrder(1))
						SM0->(DbSeek(cEmpAnt+SE6->E6_FILORIG))
					EndIf

					AADD(aTitBx , {"E2_PREFIXO"	, SE6->E6_PREFIXO	, NIL})
					AADD(aTitBx , {"E2_NUM"		, SE6->E6_NUM		, NIL})
					AADD(aTitBx , {"E2_PARCELA"	, SE6->E6_PARCELA	, NIL})
					AADD(aTitBx , {"E2_TIPO"	, SE6->E6_TIPO		, NIL})
					AADD(aTitBx , {"E2_FORNECE"	, SE2->E2_FORNECE	, NIL})
					AADD(aTitBx , {"E2_LOJA"	, SE2->E2_LOJA		, NIL})
					AADD(aTitBx , {"AUTMOTBX"	, "TRF"				, NIL})
					AADD(aTitBx , {"AUTDTBAIXA"	, dDataBase			, NIL})
					AADD(aTitBx , {"AUTHIST"	, "Transferencia de Filial",NIL}) //"Bx. transf. da "###" p/ " 

					//Verificar titulos de impostos já gerados na filial de origem
					If lImp
						F631Impos()	
					EndIf

					//Executa a Baixa do Titulo                                  
					MSExecAuto({|x, y| FINA080(x, y)}, aTitBx, 3)

					If lMsErroAuto
						MostraErro()
						DisarmTransaction()
						Break
					Else
						//Transfere SK1 tambem.
						If SK1->(MsSeek(xFilial("SK1")+SE6->(E6_PREFIXO+E6_NUM+E6_PARCELA+E6_TIPO)))
							RecLock("SK1")
							SK1->K1_FILIAL		:= SE6->E6_FILDEB
							SK1->K1_CLIENTE	:= SE6->E6_CLIENTE
							SK1->K1_LOJA		:= SE6->E6_LOJA
							MsUnlock()
						Endif

						cFilAnt := cFilOld //Restaura filial atual	
						cEmpAnt	:= cEmpOld
						
						If Len(aBkpSM0) > 0
							RestArea(aBkpSM0)
						EndIf
						lRet	:= .T.
					Endif
				
   					SE2->(MsGoto(aRecno[nY]))//ponteiro pode ter sido desposicionado ao buscar pelas parcelas do Titulo	

					lRetImp :=  cPaisLoc == "BRA" .And. (SE2->E2_VRETPIS + SE2->E2_VRETCOF + SE2->E2_VRETCSLL + SE2->E2_VRETISS + SE2->E2_VRETIRF) > 0
					
					If lRetImp .And. !lImp .And. ( nX:= Ascan(aTit, {|e| AllTrim(e[1]) == "E2_VALOR"}) ) > 0
						SA2->(dbSetOrder(1))
						lIRRFBaixa := SA2->(MsSeek(xFilial("SA2", SE2->E2_FILORIG)+SE2->E2_FORNECE+SE2->E2_LOJA)) .And. SA2->A2_CALCIRF == "2"

					    If lPccBaixa 
							aTit[nX][2] -= SE2->E2_VRETPIS + SE2->E2_VRETCOF + SE2->E2_VRETCSLL
						EndIf
						If lIRRFBaixa 
							aTit[nX][2] -= SE2->E2_VRETIRF 
						EndIf 
						If lIssBaixa 
							aTit[nX][2] -= SE2->E2_VRETISS
						EndIf		
					EndIf					

					//Altera a FILORIG para a filial de destino
					cFilAnt := SE6->E6_FILDEB
					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_FILORIG"})
					If nX > 0
						aTit[nX][2] := cFilAnt // Codigo da filial que recebera o titulo
					Endif

					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_NATUREZ"})
					If nX > 0
						
						If !lImp //MV_IMPTRAN = 1
							If !(AllTrim(cTipo) $ MVTAXA+"/"+MVINSS+"/"+MVISS+"/"+MVTXA+"/"+"SES"+"/"+"INA")
								F631SemImp(@aTit) 
								aTit[nX][2] := fNatTrf(SE6->E6_FILDEB)
								If Empty(aTit[nX][2])
									aTit[nX][2] := GetNewPar("ZZ_NATTRF","10907")
								Endif	
							Else
								aTit[nX][2] := SE2->E2_NATUREZ
							EndIf
						Else //MV_IMPTRAN = 2
							If !Empty(FWFilial("SED"))
								aTit[nX][2] := GetNewPar("ZZ_NATTRF","10907")
							Else
								aTit[nX][2] := SE2->E2_NATUREZ
							Endif

							//Exibe mensagem ao usuário indicando que o título possui vinculo com o REINF na filial de origem
							If __lReinf .And. !IsBlind()
								cChaveTit := SE2->E2_FILIAL + "|" + SE2->E2_PREFIXO + "|" + SE2->E2_NUM     + "|" + SE2->E2_PARCELA + "|" + SE2->E2_TIPO    + "|" + SE2->E2_FORNECE + "|" + SE2->E2_LOJA
								cIdFK7	  := FINBuscaFK7(cChaveTit, "SE2")
								aDadosFKF := GetAdvFVal("FKF",{"FKF_TPREPA", "FKF_TPSERV", "FKF_NATREN"}, FWxFilial("FK7", SE6->E6_FILORIG) + cIdFK7,1,"")
								lExistFKF := !Empty(aDadosFKF[1]) .Or. !Empty(aDadosFKF[2]) .Or. !Empty(aDadosFKF[3]) .Or. (FindFunction("FTemFKW") .And. FTemFKW(cIdFK7))
								If lExistFKF
									//MsgInfo(STR0047, STR0046) // ''O título foi classificado para o REINF na filial de origem e esses dados não serão migrados para a filial de destino. Classifique o título na filial de destino ou insira os dados diretamente no TAF'
								EndIf
							EndIf
						EndIf
					EndIf
					
					//Grava titulo na filial de debito destino
					MSExecAuto({|x, y, z| FINA050(x, y, z)}, aTit, 3, 3)
					
					cFilAnt := cFilOld	// Restaura filial atual

					SE2->(MsGoto(aRecno[nY]))

					If lTrocaParc
						cParcRec := cParcela
					Else
						cParcRec := SE6->E6_PARCELA
					EndIf
	
					nY ++
				End	
				
				If lMsErroAuto
					if !IsBlind()
						MostraErro()
					EndIf
					DisarmTransaction()
					Break
				Else	
					SE6->(RecLock("SE6"))
					SE6->E6_PARCDES := cParcRec						
					SE6->(MSUnlock())
				Endif	
			Endif
		END TRANSACTION	
	Endif	
Endif

__lMunic := .F.
cFilAnt	 := cFilOld	// Restaura filial atual
cEmpAnt	 := cEmpOld
If Len(aBkpSM0) > 0
	RestArea(aBkpSM0)
EndIf

If lSolicAb // Apenas solicitacoes em aberto
	SE6->(DbSetOrder(2))
	SE6->(MsSeek(xFilial("SE6")+"1")) // Posiciona na primeira solicitacao em aberto
EndIf

Return lRet

Static Function FnatTudOk(oDlg As Object) As Logical
	Local lRet As Logical

	Default oDlg := NIL

	lRet := .T.

	If Empty(Alltrim(cCodNat))
		//MSGINFO(STR0041) //"Atenção! Natureza não pode ter o conteúdo em branco!"
		lRet := .F.
	Endif

Return lRet


Static Function F631DedIns(cNaturez As Character) As Logical
	Local lDeduz   As Logical
	Local aAreaSED As Array
	Local cFilSED  As Character

	Default cNaturez := ""

	lDeduz 		:= .F.
	cFilSED     := xFilial("SED")
	aAreaSED	:= SED->(GetArea())

	If !Empty(cFilSED)
		cFilSED := xFilial("SED",SE2->E2_FILORIG)
	Endif

	SED->(dbSetOrder(1))
	If SED->(DbSeek(cFilSED+cNaturez))
		lDeduz	:= SED->ED_DEDINSS == "1"
	EndIf

	RestArea(aAreaSED)
	FwFreeArray(aAreaSED)

Return lDeduz


Static Function F631Campos(lAprova As Logical) As Array
	Local aCampos 	As Array
	Local aArea   	As Array
	Local aAreaSx3	As Array

	Default lAprova := .F.

	aCampos  := {}
	aArea	 := GetArea()
	aAreaSx3 := SX3->(GetArea())

	SX3->(dbSetOrder(1))
	SX3->(MsSeek("SE6"))
	While !SX3->(EOF()) .And. (SX3->X3_Arquivo == "SE6")
		If X3USO(SX3->X3_Usado) .And. cNivel >= SX3->X3_NIVEL
			if lAprova
				// Na aprovacao, o historico da rejeicao nao deve ser apresentado
				If AllTrim(SX3->X3_CAMPO) != "E6_HISREJ"
					Aadd(aCampos, SX3->X3_CAMPO)
				Endif
			Else
				// Na rejeicao, o historico da solicitacao nao deve ser apresentado
				If AllTrim(SX3->X3_CAMPO) != "E6_HISTSOL"
					Aadd(aCampos, SX3->X3_CAMPO)
				Endif
			Endif				
		Endif
		SX3->(dbSkip())
	EndDo

	RestArea(aAreaSx3)
	RestArea(aArea)

	FwFreeArray(aArea)
	FwFreeArray(aAreaSx3)

Return aCampos


Static Function F631OriIss(aTit As Array) As Numeric
	Local nZ 	  As Numeric
	Local nRetIss As Numeric

	Default aTit := {}

	nZ      := 0
	nRetIss := 0

	If __lMunic
		If	(SuperGetMv('MV_MRETISS',.F.,'1',SE6->E6_FILORIG) == '1')						
			nZ := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_ISS"})
			If nZ > 0
				nRetIss	:= aTit[nZ][2]
			EndIf
		EndIf
	Else
		// Se os municipios forem diferentes nao deve realizar a transferencia do imposto	
		nZ := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_VRETISS"})
		If nZ > 0
			aTit[nZ][2] := 0
		Endif

		nZ := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_ISS"})
		If nZ > 0
			aTit[nZ][2] := 0
		Endif

		nZ := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_VBASISS"})
		If nZ > 0
			aTit[nZ][2] := 0
		Endif

		nZ := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_BASEISS"})
		If nZ > 0
			aTit[nZ][2] := 0
		Endif
	Endif		

Return nRetIss


Static Function FMunicFil() As Logical
	Local lRet     As Logical
	Local aFilorig As Array
	Local aFilDeb  As Array

	lRet := .F.

	aFilorig := FWSM0Util():GetSM0Data(,SE6->E6_FILORIG, { "M0_CODMUN" } )
	aFilDeb  := FWSM0Util():GetSM0Data(, SE6->E6_FILDEB, { "M0_CODMUN" } )

	If Alltrim(aFilorig[1][2]) == Alltrim(aFilDeb[1][2])
		lRet := .T.
	Elseif FindFunction("FinHelp")
		FIN_ISSMUNIC()
	Endif		

Return lRet

static function FIN_ISSMUNIC()
	Local cMsg 			 AS Character
	Local cTitulo 		 AS Character
	Local aBtLinks 		 AS Array
	Local cColorTitle 	 AS Character
	Local cColorSubTitle AS Character
	Local cColorText 	 AS Character

	setHelpColors(@cColorTitle,@cColorSubTitle,@cColorText)

	cTitulo  := "Aprovação e rejeição de transferência"
	aBtLinks := ARRAY(1,2)

	cMsg := "<font size='5' color='" + cColorTitle + "'><b>HELP - ISSMUNIC</b></font><br/><br/>"
	cMsg += "<font size='3' color='" + cColorSubTitle + "'><b>Ocorrência</b></font><br/>"
	cMsg += "<font size='3' color='" + cColorText + "'>Verificamos no processo de transferencia a retenção do imposto ISS (Imposto sobre serviços) "
	cMsg += "por se tratar de filiais com o municipio diferente o imposto não <br/>será transferido.</font><br/>"
	cMsg += "</font><br/><br/>"
	

	cMsg += "<font size='3' color='" + cColorSubTitle + "'><b>Solução</b></font><br/>"
	cMsg += "<font size='3' color='" + cColorText + "'>Para a correta transferencia do imposto de ISS, as filiais devem estar dentro do mesmo municipio (M0_CODMUN).</font><br/><br/>"

	cMsg += "<font size='3' color='" + cColorSubTitle + "'><b>Para maiores informações acesse:</b></font>"
	cMsg += "<br/><br/><br/><tr/>"
	
	cMsg += "</div>"

	aBtLinks[1,1] := "Transferencia de titulos com ISS"
	aBtLinks[1,2] := "https://tdn.totvs.com/x/xRw9I"

	FinHelp(cTitulo, cMsg, aBtLinks, 350, 600)

Return

Static Function F631Impos()
	Local lBxImp	 As Logical
	Local cNatIss	 As Character
	Local aAreaSE2	 As Array
	Local aImpos     As Array
	Local cQry       As Character

	lBxImp	 	:= .T.
	cNatIss	 	:= AllTrim(&(GetMv("MV_ISS")))
	aAreaSE2	:= SE2->(GetArea())
	aImpos     	:= {}
	cQry       	:= ""

	cQry := " SELECT E2_PREFIXO, E2_NUM, E2_PARCELA, E2_TIPO, E2_FORNECE, E2_LOJA, E2_VALOR, E2_NATUREZ "
	cQry += " FROM "+RetSqlName("SE2")+" SE2 "
	cQry += " WHERE E2_TITPAI = '"+SE2->(E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO+E2_FORNECE+E2_LOJA)+"' "
	cQry += " AND E2_FILIAL = '"+SE2->E2_FILIAL+"' AND D_E_L_E_T_ = ' '  AND E2_SALDO > 0"

	dbUseArea(.T.,"TOPCONN",TCGENQRY(,,cQry),"TIMP",.F.,.T.)
	TCSetField("TIMP", "E2_VALOR" ,"N",14,2)
	TIMP->(dbGotop())

	Do While TIMP->(!Eof())
		lBxImp := .T.

		If AllTrim(TIMP->E2_NATUREZ) == cNatIss .and. !__lMunic
			lBxImp := .F.
		Endif

		If lBxImp	
			aImpos := {}		
			AADD(aImpos , {"E2_PREFIXO"	, TIMP->E2_PREFIXO, NIL})
			AADD(aImpos , {"E2_NUM"			, TIMP->E2_NUM	, NIL})
			AADD(aImpos , {"E2_PARCELA"	, TIMP->E2_PARCELA, NIL})
			AADD(aImpos , {"E2_TIPO"		, TIMP->E2_TIPO	, NIL})
			AADD(aImpos , {"E2_FORNECE"	, TIMP->E2_FORNECE, NIL})
			AADD(aImpos , {"E2_LOJA"		, TIMP->E2_LOJA	, NIL})
			AADD(aImpos , {"AUTMOTBX"		, "TRF"			, NIL})
			AADD(aImpos , {"AUTDTBAIXA"	, dDataBase		, NIL})
			AADD(aImpos , {"AUTHIST"		, SE6->E6_FILORIG + " p/" + SE6->E6_FILDEB,NIL}) 	
		
			MSExecAuto({|x, y| FINA080(x, y)}, aImpos, 3)

			If lMsErroAuto
				MostraErro()
				DisarmTransaction()
				Break
			EndIf

		Endif

		TIMP->(dbSkip())			
	EndDo

	TIMP->(DbCloseArea())
	RestArea(aAreaSE2)	

Return 

Static Function F631RecVl(aTit As Array)
	Local nImp 		 As Numeric
	Local nVlImp     As Numeric
	Local nAux1		 As Numeric
	Local nAux2		 As Numeric
	
	nVlImp	:= 0
	nImp	:= 0
	nAux1	:= 0
	nAux2	:= 0

	//ISS
	nVlImp += F631OriIss(@aTit)

	//PCC
	If	(SuperGetMv('MV_BX10925',.F.,'1',SE6->E6_FILORIG) == '2')						
		nImp		:= Ascan(aTit, {|e| AllTrim(e[1]) == "E2_PIS"})
		If nImp > 0
			nVlImp	+= aTit[nImp][2]
		Endif
		nImp		:= Ascan(aTit, {|e| AllTrim(e[1]) == "E2_COFINS"})
		If nImp > 0
			nVlImp	+= aTit[nImp][2]
		Endif
		nImp		:= Ascan(aTit, {|e| AllTrim(e[1]) == "E2_CSLL"})
		If nImp > 0
			nVlImp	+= aTit[nImp][2]
		Endif
	EndIf

	//INSS
	If	(SuperGetMv('MV_INSBXCP',.F.,'1',SE6->E6_FILORIG) == '2')						
		nImp		:= Ascan(aTit, {|e| AllTrim(e[1]) == "E2_INSS"})
		If nImp > 0 .And. __lDedIns
			nVlImp	+= aTit[nImp][2]
		Endif
	EndIf

	//IRRF
	dbSelectArea("SA2")
	SA2->(dbSetOrder(1))
	If SA2->(dbSeek(xFilial("SA2")+SE6->E6_CLIENTE+SE6->E6_LOJA))
		If SA2->A2_CALCIRF == "1"				
			nImp		:= Ascan(aTit, {|e| AllTrim(e[1]) == "E2_IRRF"})
			If nImp > 0
				nVlImp	+= aTit[nImp][2]
			Endif
		EndIf
	EndIf

	nAux1	:= Ascan(aTit, {|e| AllTrim(e[1]) == "E2_VALOR"})
	nAux2	:= Ascan(aTit, {|e| AllTrim(e[1]) == "E2_SALDO"})
		
	If (nVlImp > 0) .and. (nAux1 > 0) .and. (nAux2 > 0) 
		aTit[nAux1][2] += nVlImp
		aTit[nAux2][2] := aTit[nAux1][2] // Atualiza o Saldo
	Endif

Return 

Static Function ValNat(aTit)

Local lRet:= .F.  
Local lIrrf := .F.
Local lPis := .F.
Local lCsll := .F.
Local lCofins := .F.
Local lIss		:= .F.
Local lInss	:= .F.
Local lDedInsDes	:= .F. 
Local aAreaSED	:= {}

Local aTaxas	:=  {}

aAdd(aTaxas, {if(lImp,SE2->E2_IRRF,0)   , 	SED->ED_PERCIRF })
aAdd(aTaxas, {if(lImp,SE2->E2_PIS,0)    ,  	SED->ED_PERCPIS })
aAdd(aTaxas, {If(lImp,SE2->E2_CSLL,0)   , 	SED->ED_PERCCSL })
aAdd(aTaxas, {If(lImp,SE2->E2_COFINS,0) , 	SED->ED_PERCCOF })
aAdd(aTaxas, {If(lImp,SE2->E2_ISS,0)	, 	SuperGetMv("MV_ALIQISS",.F.,5, SE6->E6_FILORIG) })
aAdd(aTaxas, {If(lImp,SE2->E2_INSS,0)	, 	SED->ED_PERCINS })

If Empty(Alltrim(cCodNat))
	//MSGINFO(STR0041) //"Atenção! Natureza não pode ter o conteúdo em branco!"
	Return lRet
Endif	

aAreaSED := SED->(GetArea())
SED->(dbSetOrder(1))
IF !SED->(dbSeek(xFilial("SED")+cCodNat))
	//MSGINFO(STR0027) //"Atenção! Natureza não localizada no cadastro!"
	lRet := .F.
Else 
	If aTaxas[1,1] > 0 .And. SED->ED_CALCIRF = 'S' .And. SED->ED_PERCIRF > 0 .And. SED->ED_PERCIRF == aTaxas[1,2] 
		lRet := .T.	
	ElseIf aTaxas[1,1] == 0 .And. SED->ED_CALCIRF = 'N' 
		lRet := .T.
	ElseIf aTaxas[1,1] > 0 .And. SED->ED_CALCIRF = 'N' 
		lRet := .F.
		//MSGINFO(STR0028)//"Atenção! A Natureza não calcula IRRF, selecione outra natureza que calcule. "
	Else 
		//MSGINFO(STR0029)//"Atenção! A Natureza calcula IRRF, selecione outra natureza que não calcule. "
		lRet :=.F.
	EndIf
	lIrrf := lRet
	If lRet
		If aTaxas[2,1] > 0 .And. SED->ED_CALCPIS = 'S' .And. SED->ED_PERCPIS > 0 .And. SED->ED_PERCPIS == aTaxas[2,2]
			lRet := .T.
		ElseIf aTaxas[2,1] == 0 .And. SED->ED_CALCPIS = 'N' 
			lRet := .T.
		ElseIf aTaxas[2,1] > 0 .And. SED->ED_CALCPIS = 'N' 
			lRet := .F.
			//MSGINFO(STR0030)//"Atenção! A Natureza não calcula PIS, selecione outra natureza que calcule. "
		Else
			lRet := .F.
			//MSGINFO(STR0031)//"Atenção! A Natureza calcula PIS, selecione outra natureza que não calcule. "
		EndIf
		lPis := lRet
		If lRet 
			If aTaxas[3,1] > 0 .And. SED->ED_CALCCSL = 'S' .And. SED->ED_PERCCSL > 0 .And. SED->ED_PERCCSL == aTaxas[3,2]
				lRet := .T.
			ElseIf aTaxas[3,1] == 0 .And. SED->ED_CALCCSL = 'N' 
				lRet := .T.
			ElseIf aTaxas[3,1] > 0 .And. SED->ED_CALCCSL = 'N' 
				lRet := .F.
				//MSGINFO(STR0032)//"Atenção! A Natureza não calcula CSLL, selecione outra natureza que calcule. "
			Else
				lRet := .F.
				//MSGINFO(STR0033)//"Atenção! A Natureza calcula CSLL, selecione outra natureza que não calcule. "
			EndIf
			lCsll := lRet
			If lRet
				If aTaxas[4,1] > 0 .And. SED->ED_CALCCOF = 'S' .And. SED->ED_PERCCOF > 0 .And. SED->ED_PERCCOF == aTaxas[4,2]
					lRet := .T.
				ElseIf aTaxas[4,1] == 0 .And. SED->ED_CALCCOF = 'N' 
					lRet := .T.
				ElseIf aTaxas[4,1] > 0 .And. SED->ED_CALCCOF = 'N' 
					lRet := .F.
					//MSGINFO(STR0034)//"Atenção! A Natureza não calcula COFINS, selecione outra natureza que calcule. "
				Else
					lRet := .F.
					//MSGINFO(STR0035)//"Atenção! A Natureza calcula COFINS, selecione outra natureza que não calcule. "
				EndIf
				lCofins := lRet
				If lRet
					If aTaxas[6,1] > 0 .And. SED->ED_CALCINS = 'S' .And. SED->ED_PERCINS > 0 .And. SED->ED_PERCINS == aTaxas[6,2]
						lRet := .T.
						lDedInsDes :=  ( SED->ED_DEDINSS == "1" .And. __lDedIns ) .Or. ( SED->ED_DEDINSS <> "1" .And. !__lDedIns )  
						If lRet .And. !lDedInsDes 
							lRet	:= .F.
							//MSGINFO(STR0040)//"Atenção! "O campo 'Ded. Inss' está diferente da Natureza origem."	
						EndIf
					ElseIf aTaxas[6,1] == 0 .And. SED->ED_CALCINS = 'N' 
						lRet := .T.
					ElseIf aTaxas[6,1] > 0 .And. SED->ED_CALCINS = 'N' 
						lRet := .F.
						//MSGINFO(STR0038)//"Atenção! A Natureza não calcula INSS, selecione outra natureza que calcule. "
					Else
						lRet := .F.
						//MSGINFO(STR0039)//"Atenção! A Natureza calcula INSS, selecione outra natureza que não calcule. "
					EndIf
					lInss := lRet
					If lRet
						If !__lMunic
							If SED->ED_CALCISS = 'S' .And. SuperGetMv("MV_ALIQISS",.F.,5, SE6->E6_FILDEB) > 0 
								lRet := .F.
								//MSGINFO(STR0037)//"Atenção! A Natureza calcula ISS, selecione outra natureza que não calcule. "
							EndIf	

						Else
							
							If aTaxas[5,1] > 0 .And. SED->ED_CALCISS = 'S' .And. SuperGetMv("MV_ALIQISS",.F.,5, SE6->E6_FILDEB) > 0
								lRet := .T.
							ElseIf aTaxas[5,1] == 0 .And. SED->ED_CALCISS = 'N' 
								lRet := .T.
							ElseIf aTaxas[5,1] > 0 .And. SED->ED_CALCISS = 'N' 
								lRet := .F.
								//MSGINFO(STR0036)//"Atenção! A Natureza não calcula ISS, selecione outra natureza que calcule. "
							Else
								lRet := .F.
								//MSGINFO(STR0037)//"Atenção! A Natureza calcula ISS, selecione outra natureza que não calcule. "
							EndIf
						EndIf 
						lIss := lRet
					EndIf
				Endif	
			EndIf
		EndIf
	EndIf
	
	If lRet 
		cNomNat := SED->ED_DESCRIC
	EndIF
ENDIF 

RestArea(aAreaSED)                  

Return lRet

Static Function fNatTrf(cFilTransf)

Local aArea     := GetArea()
Local cVar		  := ""

Default cFilTransf := cFilAnt

dbSelectArea("SED")
cVar := Alltrim(GetMV("MV_NATTRFF"))

If !Empty(Alltrim(cVar))
	cVar := cVar + Space(Len(SED->ED_CODIGO)-Len(cVar))
	If !(DbSeek(xFilial("SED",cFilTransf)+cVar))
		RecLock("SED",.T.)
		SED->ED_FILIAL  := xFilial( "SED" , cFilTransf )
		SED->ED_CODIGO  := cVar
		SED->ED_CALCIRF := "N"
		SED->ED_CALCISS := "N"
		SED->ED_CALCINS := "N"
		SED->ED_CALCCSL := "N"
		SED->ED_CALCCOF := "N"
		SED->ED_CALCPIS := "N"
		SED->ED_DESCRIC := "TITULOS TRANSFERIDOS"
		Msunlock()
		FKCommit()
	EndIf
Endif

RestArea(aArea)

Return(cVar)

Static Function F631SemImp(aTit As Array)
	Local nLimpa As Numeric

	nLimpa := 0

	nLimpa := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_DIRF"})
	If nLimpa > 0
		aTit[nLimpa][2] := "2" 
	Endif
	nLimpa := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_IRRF"})
	If nLimpa > 0
		aTit[nLimpa][2] := 0
	Endif
	nLimpa := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_INSS"})
	If nLimpa > 0
		aTit[nLimpa][2] := 0
	Endif
	nLimpa := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_COFINS"})
	If nLimpa > 0
		aTit[nLimpa][2] := 0
	Endif
	nLimpa := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_PIS"})
	If nLimpa > 0
		aTit[nLimpa][2] := 0
	Endif
	nLimpa := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_CSLL"})
	If nLimpa > 0
		aTit[nLimpa][2] := 0
	Endif
	nLimpa := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_VRETPIS"})
	If nLimpa > 0
		aTit[nLimpa][2] := 0
	Endif
	nLimpa := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_VRETCOF"})
	If nLimpa > 0
		aTit[nLimpa][2] := 0
	Endif
	nLimpa := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_VRETCSL"})
	If nLimpa > 0
		aTit[nLimpa][2] := 0
	Endif
	nLimpa := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_VRETINS"})
	If nLimpa > 0
		aTit[nLimpa][2] := 0
	Endif
	nLimpa := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_VRETIRF"})
	If nLimpa > 0
		aTit[nLimpa][2] := 0
	Endif
	nLimpa := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_PRETPIS"})
	If nLimpa > 0
		aTit[nLimpa][2] := ''
	Endif
	nLimpa := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_PRETCOF"})
	If nLimpa > 0
		aTit[nLimpa][2] := ''
	Endif
	nLimpa := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_PRETCSL"})
	If nLimpa > 0
		aTit[nLimpa][2] := ''
	Endif
	nZ := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_VRETISS"})
	If nZ > 0
		aTit[nZ][2] := 0
	Endif
	nZ := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_ISS"})
	If nZ > 0
		aTit[nZ][2] := 0
	Endif
	nZ := Ascan(aTit, {|e| AllTrim(e[1]) == "E2_VBASISS"})
	If nZ > 0
		aTit[nZ][2] := 0
	Endif

Return 

