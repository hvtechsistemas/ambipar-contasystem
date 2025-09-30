#include "TOTVS.CH"
#INCLUDE "PROTHEUS.CH"
#INCLUDE "TBICONN.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "RPTDEF.CH"
#INCLUDE 'FWMVCDEF.CH'
#INCLUDE "FWEditPanel.CH"

Static _cFilOld   //Filial

/*/{Protheus.doc} AMBFIN03
Função para Aprovar Transferencia de Titulos a Receber entre filiais

@author Henrique Vieira
@since 14/09/2025
@version 1.0
/*/


User Function AMBFIN03(cAlias,nReg,nOpc)

	Private aIndexFil	:= {}
	Private bFiltraBrw
	Private cFilPad := ""
	Private aRotAuto    := Nil
	Private lExcSX5	:= FWModeAccess("SX5",3) == "E"

	SE6->(dbGoto(nReg))

	lRet := XFa630Apv(cAlias,nReg,nOpc)
	If !lRet
		SE6->(dbGoto(nReg))
		RecLock("SE6",.F.)
		SE6->E6_SITSOL := "1"
		SE6->(MsUnlock())
	Endif

Return

Static Function XFa630Apv(cAlias,nReg,nOpc)
	Local lPanelFin := IsPanelFin()
	Local nOpcA
	Local aTit := {}
	Local nX
	Local nSaldo
	Local aAcho := {}
	Local cFilSe1
	Local cFilOld := cFilAnt // Guarda filial atual
	Local cChave
	Local aRecno := {}
	//Controle de abatimento
	Local lTitpaiSE1 := .T.
	Local cTitPai := ""
	Local nY := 0
	Local cParcela	:= Space(TamSx3("E1_PARCELA")[1])
	Local lSolicAb	:= .T.
	Local lDtMovFin := .T.
	Local lRet		:= .T.
	Local IsAuto	:= Type("lMsErroAuto")<>"U"
	Local cHist
	Local aAreaSE1	:={}
	Local aAlt      :={}
	Local lExcSF2 	:= FWModeAccess("SF2",3) == "E"
	Local lExcSD9 	:= FWModeAccess("SD9",3) == "E"
	Local cTipo 	:= ""
	Local cFilSe2	:= xFilial("SE2")
	//.T. - Gera o titulo de ISS (se houver) na filial de destino juntamente com o titulo NF, baixando o titulo ISS na origem
	//.F. - Nã gera o titulo de ISS na filial de destino, permanecendo o titulo ISS na origem em aberto. 
	Local lTrfISSf	:= GetNewPar("MV_TRFISSF",.T.)
	Local cPrefixo	:= ""
	Local lAltPref	:= .F.
	Local aMsg		:= {}
	Local aChaves 	:= {"E1_FILIAL", "E1_PREFIXO", "E1_NUM", "E1_PARCELA", "E1_TIPO"}
	Local nPos 		:= 0
	Local lCompSED	:= FwModeAccess("SED", 3) == "E"
	Local nPosParc	:= 0
	Local cGetMVTPN := ""
	Local nOpcA     := 1

	Private lF630Auto := .T.

	_cFilOld := cFilAnt

	if lF630Auto
		DbSelectArea("SE6")
		DbSetorder(3)
		If (nT := ascan(aRotAuto,{|x| x[1]='E6_NUMSOL'}) ) > 0
			DbSeek(xFilial("SE6")+ Padr(aRotAuto[nT,2],TamSx3('E6_NUMSOL')[1 ]) )
		EndIf
	Endif

	// Apenas solicitacoes em aberto podem ser aprovadas.
	If SE6->E6_SITSOL != "1"
		//Help(" ",1,"FIN63004",,STR0021,1,0)
		Return .F.
	Endif

	// Nao eh permitido aprovar transferencias de titulos solicitados para
	// outra filial.
	If _cFilOld != SE6->E6_FILDEB
		//Help(" ",1,"FIN63006",,	STR0008 + SE6->E6_FILDEB + CHR(13)+; //"Transferência solicitada para filial: "
		//STR0009 + _cFilOld, 4 , 0 ) //"Filial atual: "
		Return .F.
	Endif

	//Ponto de Entrada

	If ExistBlock( "FA630APV" )

		lDtMovFin := Execblock( "FA630APV", .f., .f. )

		If !lDtMovFin
			Return .F.
		EndIf

	EndIf

	DbSelectArea("SX3")
	dbSetOrder(1)
	MsSeek("SE6")
	While ! SX3->(EOF()) .And. (SX3->X3_Arquivo == "SE6")
		If X3USO(SX3->X3_Usado) .And. cNivel >= SX3->X3_NIVEL
			// Na aprovacao, o historico da rejeicao nao deve ser apresentado
			If AllTrim(SX3->X3_CAMPO) != "E6_HISREJ"
				Aadd(aAcho, X3_CAMPO)
			Endif
		Endif
		SX3->(dbSkip())
	End

	If lPanelFin
		RegToMemory(cAlias,.T.,,,FunName())
		oPanelDados := FinWindow:GetVisPanel()
		oPanelDados:FreeChildren()
		aDim := DLGinPANEL(oPanelDados)
		nOpca := AxVisual(cAlias,nReg,nOpc,aAcho,,,,,,,.T.,,.T.,.T.,aDim)
	Else
		If !lF630Auto
			nOpca := AxVisual(cAlias,nReg,nOpc,aAcho,,,,,,,,,.T.)
		Else
			If ascan(aRotAuto,{|x| x[1]='E6_NUMSOL'})>0 .and. SE6->(DbSeek(xFilial("SE6")+ Padr(aRotAuto[nT,2],TamSx3('E6_NUMSOL')[1]) ))
				INCLUI:= .F.
				RegToMemory(cAlias,.T.,,,)
				nOpca := 1
			EndIf
		EndIf
	Endif

	If nOpcA == 1
		cFilAnt 	:= SE6->E6_FILORIG
		cFilSe1 	:= xFilial("SE1")
		Posicione("SE1",1,cFilSe1+SE6->(E6_PREFIXO+E6_NUM+E6_PARCELA+E6_TIPO),"E1_CLIENTE")

		cGetMVTPN := GetMV("MV_TPNRNFS") 

		lAltPref 	:= AllTrim(SE1->E1_ORIGEM) $ "MATA460" .And. ((lExcSX5 .Or. ExistBlock("CHGX5FIL")) .And. cGetMVTPN == "1") .Or.;
			(lExcSF2 .And. cGetMVTPN == "2") .Or. (lExcSD9 .And. cGetMVTPN == "3") .And.;
			AllTrim(SuperGetMV("MV_1DUPREF")) == "SF2->F2_SERIE" //Conteudo padrao do parametro
		

		If lAltPref .And. !lF630Auto
			//aAdd(aMsg,STR0023)
			//aAdd(aMsg,STR0024)
			//aAdd(aMsg,STR0025)
			//aAdd(aMsg,STR0026)
			//aAdd(aMsg,STR0027)
			//aAdd(aMsg,STR0028)

			//FormBatch(STR0029,aMsg,{{1,.T.,{|| lExeFun := .T., FechaBatch()}}})
		EndIf
			

		If lAltPref
			cOrigem := SE1->E1_ORIGEM
		EndIf

		If	!IsAuto
			lMsErroAuto := .F. // variavel interna da rotina automatica
		Endif
		If SE1->(!Eof())
			BEGIN TRANSACTION
			If RecLock("SE6",.F.)
				SE6->E6_SITSOL := "2" // Solicitacao aprovada
				SE6->E6_USRAPV :=	RetCodUsr() // Aprovador
				SE6->(MsUnlock())
				// Baixa o titulo na filial de origem
				aTit := {}
				aAreaSE1:=SE1->(GetArea())
				nAbatim := SomaAbat(SE1->E1_PREFIXO,SE1->E1_NUM,SE1->E1_PARCELA,"R",SE1->E1_MOEDA,dDataBase,SE1->E1_CLIENTE,SE1->E1_LOJA, cFilSe1,,SE1->E1_TIPO)
				RestArea(aAreaSE1)
				nSaldo := (SE1->E1_SALDO - nAbatim)
				nSaldo += SE1->E1_SDACRES
				nSaldo -= SE1->E1_SDDECRE
				cChave := cFilSe1 + SE6->(E6_PREFIXO+E6_NUM+E6_PARCELA)
				cTitPai := SE6->(E6_PREFIXO+E6_NUM+E6_PARCELA+E6_TIPO+E6_CLIENTE+E6_LOJA)
				nRecNoSe1 := SE1->(Recno())
				SE1->(MsSeek(cChave))
				// Primeiro adiciona os registros a serem transferidos, pois os primeiro
				// deve-se transferir o titulo principal, depois os agragados. Como os agragados podem
				// vir primeiro, entao utiliza-se uma matriz de recnos para transferir os titulos na ordem
				// em que foram incluidos, pois um abatimento nunca eh incluido primeiro que um titulo
				// principal
				// Os titulos de impostos nao se incluem nesta lista visto que os mesmos serao incluidos junto com
				// o titulo principal
				While ! lMsErroAuto .and. SE1->(!Eof()) .And. SE1->(E1_FILIAL+E1_PREFIXO+E1_NUM+E1_PARCELA) == cChave
					// Nao eh abatimento
					If !(SE1->E1_TIPO $ MVABATIM)
						If SE1->E1_TIPO == SE6->E6_TIPO // Titulo Principal
							Aadd(aRecno, SE1->(Recno()))
						EndIf
						// Abatimento (menos impostos) sem baixas
					ElseIf	!(SE1->E1_TIPO $ MVIRABT+"/"+MVINABT+"/"+MVCSABT+"/"+MVCFABT+"/"+MVPIABT+"/"+MVISABT+"/"+MVFUABT+"/"+MVI2ABT)
						// Se nao houve baixa, adiciona para transferencia
						If Empty( SE1->E1_BAIXA )
							If lTitpaiSE1
								If !Empty(SE1->E1_TITPAI) .AND. Alltrim(SE1->E1_TITPAI) <> AllTrim(cTitPai)
									SE1->(DbSkip())
									Loop
								Endif
							Endif
							Aadd(aRecno, SE1->(Recno()))
						Else	// Se jah houver baixa, volta o saldo do titulo principal
							nSaldo += nAbatim
						EndIf
					Endif
					SE1->(DbSkip())
				Enddo
				aSort(aRecno)
				// Transfere todos os titulos agregados
				nY := 1
				While ! lMsErroAuto .And. nY <= Len(aRecno)
					SE1->(MsGoto(aRecno[nY]))
					aTit := {}
					//garante que os campos chaves serao os primeiros itens do array
					//para validacao se chave existe
					For nX := 1 To Len(aChaves)
						nPos := SE1->(FieldPos(aChaves[nX]))
						If nPos > 0
							AADD(aTit, { SE1->(FieldName(nPos)), SE1->(FieldGet(nPos)), NIL } )
						EndIf
					Next nX

					For nX := 1 To SE1->(fCount())
						If aScan(aChaves,{|x| x == FieldName(nX) }) == 0
							AADD(aTit, { SE1->(FieldName(nX)), SE1->(FieldGet(nX)), NIL } )
							If aTit[nX][1] == "E1_NATUREZ" .And. lCompSED .And. !lF630Auto
								aTit[nX][2] := SE1->E1_NATUREZ //ConfigSED(aTit[nX][2])
							EndIf
						EndIf
					Next
					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_FILIAL"})
					If nX > 0
						cFilAnt := SE6->E6_FILDEB
						aTit[nX][2] := xFilial("SE1") // Codigo da filial que recebera o titulo
						cFilAnt := _cFilOld
					Endif
					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_FILORIG"})
					If nX > 0
						aTit[nX][2] := SE6->E6_FILDEB	// Codigo da filial de origem do titulo, lembrando que o E6_FILDEB posicionado é aonde está o título no momento
					Endif
					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_ORIGEM"})
					If nX > 0
						aTit[nX][2] := IIf(lAltPref,cOrigem,"FINA630") // Codigo da Rotina
					Endif
					cPrefixo := SE6->E6_PREFIXO
					If lAltPref
						nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_PREFIXO"})
						If nX > 0
							cPrefixo    := "TRS"
							aTit[nX][2] := cPrefixo // Prefixo do título
						Endif
						nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_TITPAI"})
						If nX > 0
							aTit[nX][2] := SE1->(E1_PREFIXO+E1_NUM+E1_PARCELA+E1_TIPO+E1_CLIENTE+E1_LOJA) // Prefixo do título
						Endif
					EndIf
					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_FATURA"})
					If nX > 0
						aTit[nX][2] := ""
					Endif
					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_CLIENTE"})
					If nX > 0
						aTit[nX][2] := SE6->E6_CLIENTE // Codigo do cliente que recebera o titulo
					Endif
					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_LOJA"})
					If nX > 0
						aTit[nX][2] := SE6->E6_LOJA // Codigo da loja do cliente que recebera o titulo
					Endif
					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_DESDOBR"})
					If nX > 0
						aTit[nX][2] := "2" // Forca o desdobramento como nao
					Endif
					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_NUMSOL"})
					If nX > 0
						aTit[nX][2] := "" // Limpa numero da solicitacao na filial destino (debito)
					Endif
					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_TIPO"})
					If nX > 0
						cTipo := aTit[nX][2]  //Tipo do titulo que sera incluido para verificar a parcela
					Endif
					nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_MODSPB"})
					If nX > 0
						If(Empty(aTit[nX][2]),aTit[nX][2]:="1",.T.)
						Endif
						nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_APLVLMN"})
						If nX > 0
							If(Empty(aTit[nX][2]),aTit[nX][2]:="1",.T.)
							Endif
							nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_VALOR"})
							If nX > 0
								aTit[nX][2] := SE6->E6_VALOR
							Endif
							nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_RATFIN"})
							If nX > 0
								If Empty(aTit[nX][2])
									aTit[nX][2] := "2"
								Endif
							Endif
							nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_TCONHTL"})
							If nX > 0
								If Empty(aTit[nX][2])
									aTit[nX][2] := "3"
								Endif
							Endif
							nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_PARCELA"})
							If nX > 0
								nPosParc := nX
								// Incrementa a parcela para que nao haja registro duplicado na filial Debito
								If cTipo $ MVABATIM
									aTit[nX][2] := cParcela
								Else
									aTit[nX][2] := SE6->E6_PARCELA
									cFilAnt := SE6->E6_FILDEB
									cFilSe1 := xFilial("SE1")
									cFilAnt := _cFilOld
									cChave := cFilSe1 + SE6->(cPrefixo+E6_NUM+aTit[nX][2]+cTipo)
									While SE1->(MsSeek(cChave))
										aTit[nX][2] := Soma1(aTit[nX][2])
										cChave := cFilSe1 + SE6->(cPrefixo+E6_NUM+aTit[nX][2]+cTipo)
									Enddo
									cParcela := aTit[nX][2]  //Parcela do titulo Principal
								Endif
							Endif
							nX := Ascan(aTit, {|e| AllTrim(e[1]) == "E1_TPDESC"})
							If nX > 0
								If Empty(aTit[nX][2])
									aTit[nX][2] := "C"
								Endif
							Endif
							// Grava titulo na filial de debito destino
							MSExecAuto({|x, y| FINA040(x, y)}, aTit, 3)

							nY ++
						End
						If lMsErroAuto
							if !IsBlind()
								MostraErro()
							EndIf
							DisarmTransaction()
							Break
						Else
							If nPosParc > 0
								SE6->(RecLock("SE6"))
								SE6->E6_PARCDES := aTit[nPosParc][2]
								SE6->(MSUnlock())
							EndIf
							SE1->(MsGoto(nRecNoSe1))
							cFilSe1 := SE6->E6_FILORIG
							// Altera para filial do titulo de origem para fazer a baixa
							cFilAnt := cFilSe1
							aTit := {}
							AADD(aTit , {"E1_PREFIXO"	, SE6->E6_PREFIXO	, NIL})
							AADD(aTit , {"E1_NUM"		, SE6->E6_NUM		, NIL})
							AADD(aTit , {"E1_PARCELA"	, SE6->E6_PARCELA	, NIL})
							AADD(aTit , {"E1_TIPO"		, SE6->E6_TIPO		, NIL})
							AADD(aTit , {"E1_CLIENTE"	, SE1->E1_CLIENTE	, NIL})
							AADD(aTit , {"E1_LOJA"		, SE1->E1_LOJA		, NIL})
							AADD(aTit , {"AUTMOTBX"		, "TRF"				, NIL})
							AADD(aTit , {"AUTDTBAIXA"	, dDataBase			, NIL})
							AADD(aTit , {"AUTDTCREDITO", dDataBase			, NIL})
							//Ponto de entrada criado para alterar a gravação do histórico na SE5
							IF ExistBlock("F630HIST")
								cHist:=ExecBlock("F630HIST",.f.,.f.)
								AADD(aTit , {"AUTHIST"		, cHist			,NIL})
							Else
								AADD(aTit , {"AUTHIST"		, "Transferencia entre filial",NIL}) //"Bx. p/transf. da filial "###" p/"
							EndIf
							AADD(aTit , {"AUTVALREC"	, nSaldo				, NIL })
							//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
							//³Executa a Baixa do Titulo                                         ³
							//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
							lRet := .T.
							If !IsAuto
								MSExecAuto({|x, y| FINA070(x, y)}, aTit, 3)
							Else
								lRet	:= FINA070(aTit, 3)
							Endif
							If lMsErroAuto //.or. !lRet
								If lMsErroAuto
									MostraErro()
								EndIf
								DisarmTransaction()
								Break
							Else
								// Transfere SK1 tambem.
								If SK1->(MsSeek(xFilial("SK1")+SE6->(E6_PREFIXO+E6_NUM+E6_PARCELA+E6_TIPO)))
									RecLock("SK1")
									SK1->K1_FILIAL		:= SE6->E6_FILDEB
									SK1->K1_CLIENTE		:= SE6->E6_CLIENTE
									SK1->K1_LOJA		:= SE6->E6_LOJA
									SK1->K1_FILORIG		:= SE6->E6_FILDEB
									MsUnlock()
								Endif
								lRet	:= .T.
							EndIf
							If lRet
								If SE1->E1_ISS != 0 .And. lTrfISSf
									//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
									//³ Baixa tambem os registro de impostos-ISS	  ³
									//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
									dbSelectArea("SE2")
									dbSetOrder(1)
									If dbSeek(xFilial("SE2")+SE1->(E1_PREFIXO+E1_NUM+E1_PARCELA))
										IF AllTrim(E2_NATUREZ) = Alltrim(&(GetMv("MV_ISS"))) .and. ;
												STR(SE2->E2_SALDO,17,2) == STR(SE2->E2_VALOR,17,2)

											cFilSe2 := If(!Empty(cFilSe2), SE6->E6_FILORIG,cFilSe2)
											aTit := {}
											// Altera para filial do titulo de origem para fazer a baixa
											cFilAnt := cFilSe2
											AADD(aTit , {"E2_FILIAL"	, SE2->E2_FILIAL	, NIL})
											AADD(aTit , {"E2_PREFIXO"	, SE2->E2_PREFIXO	, NIL})
											AADD(aTit , {"E2_NUM"		, SE2->E2_NUM		, NIL})
											AADD(aTit , {"E2_PARCELA"	, SE2->E2_PARCELA	, NIL})
											AADD(aTit , {"E2_TIPO"		, SE2->E2_TIPO		, NIL})
											AADD(aTit , {"E2_NATUREZ"	, SE2->E2_NATUREZ	, NIL})
											AADD(aTit , {"E2_FORNECE"	, SE2->E2_FORNECE	, NIL})
											AADD(aTit , {"E2_LOJA"		, SE2->E2_LOJA		, NIL})
											AADD(aTit , {"AUTMOTBX"		, "TRF"				, NIL})
											AADD(aTit , {"AUTDTBAIXA"	, dDataBase			, NIL})
											//Ponto de entrada criado para alterar a gravação do histórico na SE5

											IF ExistBlock("F630HIST")
												cHist:=ExecBlock("F630HIST",.f.,.f.)
												AADD(aTit , {"AUTHIST"		, cHist			,NIL})
											Else
												AADD(aTit , {"AUTHIST"		, "Transferencia entre filial",NIL}) //"Bx. p/transf. da filial "###" p/"
											EndIf
											AADD(aTit , {"AUTVLRPG"	, SE2->E2_SALDO	, NIL })
											//ÚÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¿
											//³Executa a Baixa do Titulo                                         ³
											//ÀÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÙ
											If !IsAuto
												MSExecAuto({|x, y| FINA080(x, y)}, aTit, 3)
											Else
												lRet	:= FINA080(aTit, 3)
											Endif

											If lMsErroAuto
												MostraErro()
												DisarmTransaction()
												Break
											Else
												cFilAnt := cFilOld // Restaura filial atual
												lRet	:= .T.
											Endif
										EndIf
									Endif
								EndIf
							EndIf
						Endif
					Endif
				END TRANSACTION
			Endif
		Endif

	//numbor			
	aadd( aAlt,{ '','','','',Alltrim(cFilOld) })
	//chamada da Função que cria o Histórico de Cobrança
	FinaCONC(aAlt)

	cFilAnt := cFilOld	// Restaura filial atual
	If lSolicAb 		// Apenas solicitacoes em aberto
		SE6->(DbSetOrder(2))
		SE6->(MsSeek(xFilial("SE6")+"1")) // Posiciona na primeira solicitacao em aberto
	EndIf
Return lRet

Static Function FindISSBx(cChaveReg)

	Local aArea := GetArea()
	Local aAreaSE5 := SE5->(GetArea())
	Local lRet 	:= .T.
	Local _aTit	:=	{}

	Local cCompleteKey	:= ""
	Local cISSNatur 		:= AllTrim(&(GetMv("MV_ISS")))
	Local cMunicip 		:= Padr(GetMV("MV_MUNIC"),TamSx3("E5_CLIFOR")[1])

	Default cChaveReg := ""

	cCompleteKey := cChaveReg+'TX '+cMunicip

	dbSelectArea("SE5")
	dbSetOrder(7) //	E5_FILIAL+E5_PREFIXO+E5_NUMERO+E5_PARCELA+E5_TIPO+E5_CLIFOR+E5_LOJA
	If dbSeek(cCompleteKey)
		While SE5->(!Eof()) .and. (cCompleteKey == E5_FILIAL+E5_PREFIXO+E5_NUMERO+E5_PARCELA+E5_TIPO+E5_CLIFOR)

			If (Alltrim(SE5->E5_NATUREZ) == cISSNatur) .and. (SE5->E5_RECPAG == 'P')

				//Cancelo Baixa Titulos a Pagar de ISS
				_aTit := {}
				AADD(_aTit , {"E2_PREFIXO"	,SE5->E5_PREFIXO	,NIL})
				AADD(_aTit , {"E2_NUM"		,SE5->E5_NUMERO	,NIL})
				AADD(_aTit , {"E2_PARCELA"	,SE5->E5_PARCELA	,NIL})
				AADD(_aTit , {"E2_TIPO"  	,SE5->E5_TIPO		,NIL})
				AADD(_aTit , {"E2_FORNECE"	,SE5->E5_CLIFOR	,NIL})
				AADD(_aTit , {"E2_LOJA"  	,SE5->E5_LOJA		,NIL})

				MSExecAuto({|x, y| FINA080(x, y)}, _aTit, 5)

				If  lMsErroAuto
					lRet	:= .F.
					If !IsBlind()
						MOSTRAERRO()
					EndIf
					DisarmTransaction()
					Break
				Else
					Exit
				Endif
			Endif

			SE5->(dbSkip())
		EndDo
	Endif

	RestARea(aAreaSE5)
	RestARea(aArea)

Return lRet
/*
ÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜÜ
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
±±ÉÍÍÍÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍËÍÍÍÍÍÍÑÍÍÍÍÍÍÍÍÍÍÍÍÍ»±±
±±ºPrograma  ³FindIrfBx  ºAutor  ³Jose.Gavetti       º Data ³  19/11/14   º±±
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÊÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºDesc.     ³ Cancela a baixa do titulo IRF que foi baixado na transferencia
±±ºDesc.     ³ ao realizar o estorno.
±±ÌÍÍÍÍÍÍÍÍÍÍØÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹±±
±±ºUso       ³ AP                                                         º±±
±±ÈÍÍÍÍÍÍÍÍÍÍÏÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼±±
±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±±
ßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßßß
*/
Static Function FindIrfBx()

	Local aArea := GetArea()
	Local aAreaSE5 := SE5->(GetArea())
	Local lRet 	:= .T.
	Local _aTit	:=	{}
	Local cForUNIAO	:= ""
	Local cLojaIrrf	:= ""

	cForUNIAO := PADR(GetMV("MV_UNIAO"),TAMSX3("E2_FORNECE")[1])
	cLojaIrrf  := Padr( "00", Len( SE2->E2_LOJA ), "0" )

	dbSelectArea("SE5")
	dbSetOrder(7) // E2_FILIAL+E2_FORNECE+E2_LOJA+E2_PREFIXO+E2_NUM+E2_PARCELA+E2_TIPO
	If dbSeek(xFilial("SE5")+SE5->(E5_PREFIXO+E5_NUMERO))
		While !(EOF()) .And. (xFilial("SE5")== SE5->E5_FILIAL) .AND. SE5->(E5_PREFIXO+E5_NUMERO) == SE2->(E2_PREFIXO+E2_NUM)
			If cForUNIAO+cLojaIrrf == (SE5->(E5_FORNECE+E5_LOJA)) .AND. cForUNIAO == SE5->E5_CLIFOR .AND. SE5->E5_TIPO = 'TX'

				//Cancelo Baixa Titulos a Pagar de IRF
				_aTit := {}
				AADD(_aTit , {"E2_PREFIXO"	,SE5->E5_PREFIXO	,NIL})
				AADD(_aTit , {"E2_NUM"		,SE5->E5_NUMERO	,NIL})
				AADD(_aTit , {"E2_PARCELA"	,SE5->E5_PARCELA	,NIL})
				AADD(_aTit , {"E2_TIPO"  	,SE5->E5_TIPO		,NIL})
				AADD(_aTit , {"E2_FORNECE"	,SE5->E5_CLIFOR	,NIL})
				AADD(_aTit , {"E2_LOJA"  	,SE5->E5_LOJA		,NIL})

				MSExecAuto({|x, y| FINA080(x, y)}, _aTit, 5)

				If  lMsErroAuto
					lRet	:= .F.
					If !IsBlind()
						MOSTRAERRO()
					EndIf
					DisarmTransaction()
					Break
				Else
					Exit
				Endif
			Endif

			SE5->(dbSkip())
		EndDo
	Endif

	RestARea(aAreaSE5)
	RestARea(aArea)

Return


//-------------------------------------------------------------------
/*/{Protheus.doc} ConfigSED
Verifica se a configuração da natureza destino é similar a da de 
origem.

@param cNat = código da natureza de origem

@author rodrigo.oliveira
@since 03/07/2018
@version 12.1.17
/*/
//-------------------------------------------------------------------
Static Function ConfigSED(cNat)

	Local aConfSED	:= {}
	Local nX		:= 0
	Local nPos		:= 0
	Local aAreaSED	:= GetArea()
	Local cFilOld	:= cFilAnt
	Local cCpo		:= ""
	Local lSEDDif	:= .F.
	Local cNatAux	:= cNat


	DbSelectArea("SED")
	DbSetOrder(1)
	DbSeek(xFilial("SED") + cNat)
	For nX := 1 To SED->(fCount())
		If "_REC" $ (cCpo := SED->(FieldName(nX))) .Or. "BASE" $ cCpo .Or. "CALC" $ cCpo .Or. "PER" $ cCpo
			AADD(aConfSED, { SED->(FieldName(nX)), SED->(FieldGet(nX)) } )
		EndIf
	Next

	cFilAnt := SE6->E6_FILDEB // Codigo da filial que recebera o titulo
	If DbSeek(xFilial("SED") + cNat)
		For nX := 1 to Len(aConfSED)
			nPos := SED->(FieldPos(aConfSED[nX][1]))
			If nPos > 0
				If ValType(aConfSED[nX][2]) == "C"
					If Empty(SED->(FieldGet(nPos))) .And. aConfSED[nX][2] $ "S|1|"
						lSEDDif := .T.
					EndIf
					If Empty(aConfSED[nX][2]) .And. SED->(FieldGet(nPos)) $ "S|1|"
						lSEDDif := .T.
					EndIf
					If !Empty(SED->(FieldGet(nPos))) .And. !Empty(aConfSED[nX][2])
						If SED->(FieldGet(nPos)) != aConfSED[nX][2]
							lSEDDif := .T.
						EndIf
					EndIf
				Else
					If SED->(FieldGet(nPos)) != aConfSED[nX][2]
						lSEDDif := .T.
					EndIf
				EndIf
				If lSEDDif == .T.
					Exit
				EndIf
			EndIf
		Next
		If lSEDDif
			//MsgInfo(STR0036, STR0035)
			//If !MsgYesNo(STR0037, STR0035)
				DEFINE MSDIALOG oDlg FROM	22,9 TO 130,310 TITLE "Natureza filial de destino" PIXEL  // "Natureza filial de destino"

				@ 010, 050 MSGET cNatAux	F3 "SED" Valid Fa630Nat(cNatAux)      		SIZE 55, 11 OF oDlg PIXEL Hasbutton
				@ 010, 020 SAY "Natureza"	OF oDlg PIXEL //"Natureza"
				@ 004, 007 TO 036, 150 OF oDlg PIXEL

				DEFINE SBUTTON FROM 07, 120 TYPE 1 ACTION (nOpca:=1,cNat := cNatAux, oDlg:End()) ENABLE OF oDlg

				ACTIVATE MSDIALOG oDlg CENTERED
			//EndIf
		EndIf
	EndIf

	cFilAnt := cFilOld

	RestArea(aAreaSED)

Return cNat
