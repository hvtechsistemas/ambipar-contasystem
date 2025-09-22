#include "TOTVS.CH"
#INCLUDE "PROTHEUS.CH"
#INCLUDE "TBICONN.CH"
#INCLUDE "TOPCONN.CH"
#INCLUDE "RPTDEF.CH"
#INCLUDE 'FWMVCDEF.CH'
#INCLUDE "FWEditPanel.CH"

#DEFINE CONFIRMED			.T.
#DEFINE CANCELED			.F.

/*/{Protheus.doc} AMBFIN01
Função para Transferencia de Titulos entre filiais

- nOpc 1 - Contas a Pagar 
- nOpc 2 - Contas a Receber

@author Henrique Vieira
@since 14/09/2025
@version 1.0
/*/


User Function AMBFIN01(nOpc)
    Local aPergs := {}

    Private aTitulos := {}

    aAdd(aPergs ,{1,"Filial De: ", Space(TamSX3("E1_FILIAL")[1]) , PesqPict("SE1","E1_FILIAL"), '', '', '.T.', 60, .T.})
    aAdd(aPergs ,{1,"Filial Para: ", Space(TamSX3("E1_FILIAL")[1]) , PesqPict("SE1","E1_FILIAL"), '', '', '.T.', 60, .T.})
    aAdd(aPergs ,{1,"Titulo De: ", Space(TamSX3("E1_NUM")[1]) , PesqPict("SE1","E1_NUM"), '', '', '.T.', 60, .T.})
    aAdd(aPergs ,{1,"Titulo Ate: ", Space(TamSX3("E1_NUM")[1]) , PesqPict("SE1","E1_NUM"), '', '', '.T.', 60, .T.})
    aAdd(aPergs, {1,"Data De:", Ctod(Space(8)), "", ".T.", "", ".T.", 50, .T.})
    aAdd(aPergs, {1,"Data Até:", Ctod(Space(8)), "", ".T.", "", ".T.", 50, .T.})

    lCanSave := .F.
    lUserSave := .T.
    If ParamBox(aPergs, "Filtros",,,,,,,,, lCanSave, lUserSave)
        GetDadosTitulos(nOpc)
        If Len(aTitulos) > 0
            BrowseSelecTitulo(nOpc)
        Else 
            MsgAlert("Dados não encontrados com os filtros informados!!!")
        Endif
    Endif

Return

/*
    Função para montar tela markbrowse com dados do contas a pagar 
*/

Static Function BrowseSelecTitulo(nOpc)

    DEFINE MsDIALOG o3Dlg TITLE 'Seleção de Titulos a Transferir' From 0, 4 To 550, 980 Pixel

	oPnMaster := tPanel():New(0,0,,o3Dlg,,,,,,0,0)
	oPnMaster:Align := CONTROL_ALIGN_ALLCLIENT

	oPedBrw := fwBrowse():New()
	oPedBrw:setOwner( oPnMaster )

	oPedBrw:setDataArray()
	oPedBrw:setArray( aTitulos )
	oPedBrw:disableConfig()
	oPedBrw:disableReport()

	oPedBrw:SetLocate() // Habilita a Localização de registros

	//Create Mark Column
	oPedBrw:AddMarkColumns({|| IIf(aTitulos[oPedBrw:nAt,01], "LBOK", "LBNO")},; //Code-Block image
	{|| SelectOne(oPedBrw, aTitulos)},; //Code-Block Double Click
	{|| SelectAll(oPedBrw, 01, aTitulos) }) //Code-Block Header Click

	oPedBrw:addColumn({"Filial"              , {||aTitulos[oPedBrw:nAt,02]}, "C", "@!"    , 0,  20    ,                            , .T. , , .F.,, "aTitulos[oPedBrw:nAt,02]",, .F., .T.,                                    , "ETDESPES1"    })
	oPedBrw:addColumn({"Num. Doc"              , {||aTitulos[oPedBrw:nAt,03]}, "C", "@!"    , 0,  20    ,                            , .T. , , .F.,, "aTitulos[oPedBrw:nAt,03]",, .F., .T.,                                    , "ETDESPES1"    })
	oPedBrw:addColumn({"Prefixo"              , {||aTitulos[oPedBrw:nAt,04]}, "C", "@!"    , 0,  20    ,                            , .T. , , .F.,, "aTitulos[oPedBrw:nAt,04]",, .F., .T.,                                    , "ETDESPES1"    })
    oPedBrw:addColumn({"Parcela"              , {||aTitulos[oPedBrw:nAt,09]}, "C", "@!"    , 0,  20    ,                            , .T. , , .F.,, "aTitulos[oPedBrw:nAt,09]",, .F., .T.,                                    , "ETDESPES1"    })
    oPedBrw:addColumn({"Cli/For"              , {||aTitulos[oPedBrw:nAt,05]}, "C", "@!"    , 0,  20    ,                            , .T. , , .F.,, "aTitulos[oPedBrw:nAt,05]",, .F., .T.,                                    , "ETDESPES1"    })
    oPedBrw:addColumn({"Loja"              , {||aTitulos[oPedBrw:nAt,06]}, "C", "@!"    , 0,  20    ,                            , .T. , , .F.,, "aTitulos[oPedBrw:nAt,06]",, .F., .T.,                                    , "ETDESPES1"    })
    oPedBrw:addColumn({"Valor"              , {||aTitulos[oPedBrw:nAt,07]}, "C", "@!"    , 0,  20    ,                            , .T. , , .F.,, "aTitulos[oPedBrw:nAt,07]",, .F., .T.,                                    , "ETDESPES1"    })
    oPedBrw:addColumn({"Recno"              , {||aTitulos[oPedBrw:nAt,08]}, "C", "@!"    , 0,  20    ,                            , .T. , , .F.,, "aTitulos[oPedBrw:nAt,08]",, .F., .T.,                                    , "ETDESPES1"    })

	oPedBrw:setEditCell( .T. , { || .T. } ) //activa edit and code block for validation

	oPedBrw:Activate(.T.)

	ACTIVATE MSDIALOG o3Dlg CENTERED  ON INIT (EnchoiceBar(o3Dlg,{|| (lConfirmed := CONFIRMED , FWMsgRun(, {|| ProcTransf(aTitulos,nOpc)}, "Aguarde", "Processando Transferencia")) },{||(lConfirmed := CANCELED , o3Dlg:End() )},,))

Return

/*
    Função para consultar os pedidos que o usuario marcou
*/

Static Function ProcTransf(aDados,nOpc)
	Local _ni := 1

	For _ni := 1 to len(aDados)
		If aDados[_ni,1]
            //Marcados
            If nOpc == 1 //Cta a Pagar
                nRecTitulo := aDados[_ni,8]
                SE2->(dbGoto(nRecTitulo))

                cFilDest := MV_PAR02
                cCliFor := SE2->E2_FORNECE
                cLojCliFor := SE2->E2_LOJA

                nRecTransf := TranfSE2(nRecTitulo,cFilDest,cCliFor,cLojCliFor)
                If nRecTransf > 0
                    If cFilDest <> cFilAnt
                        cCNPJ := Alltrim(POSICIONE("SM0",1,cEmpAnt+cFilDest,"M0_CGC" ))
                        TrocaFilial(cCNPJ)
                    Endif
                    If cFilDest == cFilAnt
                        //Aprova a Transf
                        U_AMBFIN02("SE6",nRecTransf,4)
                    Else 
                        MsgAlert("Erro ao abrir ambiente na filial de destino: "+Alltrim(cFilDest))
                    Endif
                Else 
                    MsgAlert("Erro ao transferir titulo "+Alltrim(SE2->E2_NUM))
                Endif
            Else 
                //Cta a Receber
                nRecTitulo := aDados[_ni,8]
                SE1->(dbGoto(nRecTitulo))

                cFilDest := MV_PAR02
                cCliFor := SE1->E1_CLIENTE
                cLojCliFor := SE1->E1_LOJA

                nRecTransf := TranfSE1(nRecTitulo,cFilDest,cCliFor,cLojCliFor)
                If nRecTransf > 0
                    If cFilDest <> cFilAnt
                        cCNPJ := Alltrim(POSICIONE("SM0",1,cEmpAnt+cFilDest,"M0_CGC" ))
                        TrocaFilial(cCNPJ)
                    Endif
                    If cFilDest == cFilAnt
                        SE6->(dbGoto(nRecTransf))
                        //Aprova a Transf
                        Fa630Apv()
                    Else 
                        MsgAlert("Erro ao abrir ambiente na filial de destino: "+Alltrim(cFilDest))
                    Endif
                Else 
                    MsgAlert("Erro ao transferir titulo "+Alltrim(SE1->E1_NUM))
                Endif
            Endif

		Endif
	Next

    MsgInfo("Processo concluido !")

	o3Dlg:End()

Return .T.

/*
 Transfere titulo contas a Receber
*/

Static function TranfSE1(nRecTitulo,cFilDest,cCliFor,cLojCliFor)
    Local aCab := {}
    Local nRet := 0
 
    Private lMsErroAuto := .F.
 
    SE2->(dbGoto(nRecTitulo))
 
    aadd(aCab, {"E6_FILDEB",cFilDest})
    aadd(aCab, {"E6_CLIENTE",cCliFor})
    aadd(aCab, {"E6_LOJA",cLojCliFor})
    aadd(aCab, {"AUTHISTDEB","solicitado pelo FINA621 via execauto."})

    //Inclusao de Solicitação de transferencia
    MSExecAuto({|a, b| FINA620(a,b)}, aCab,3)

    If lMsErroAuto
        mostraerro() 
    Else 
        nRet := SE6->(Recno())
    EndIf

Return nRet

/*
 Transfere titulo contas a pagar
*/

Static function TranfSE2(nRecTitulo,cFilDest,cCliFor,cLojCliFor)
    Local aCab := {}
    Local nRet := 0
 
    Private lMsErroAuto := .F.
 
    SE2->(dbGoto(nRecTitulo))
 
    aadd(aCab, {"E6_FILDEB",cFilDest})
    aadd(aCab, {"E6_CLIENTE",cCliFor})
    aadd(aCab, {"E6_LOJA",cLojCliFor})
    aadd(aCab, {"AUTHISTDEB","solicitado pelo FINA621 via execauto."})

    //Inclusao de Solicitação de transferencia
    MSExecAuto({|a, b| FINA621(a,b)}, aCab,3)

    If lMsErroAuto
        mostraerro() 
    Else 
        nRet := SE6->(Recno())
    EndIf

Return nRet

Static Function SelectOne(oBrowse, aArquivo)
aArquivo[oPedBrw:nAt,1] := !aArquivo[oPedBrw:nAt,1]
oBrowse:Refresh()
Return .T.

Static Function SelectAll(oBrowse, nCol, aArquivo)
Local _ni := 1
For _ni := 1 to len(aArquivo)
    aArquivo[_ni,1] := lMarker
Next
oBrowse:Refresh()
lMarker:=!lMarker
Return .T.

/*
    Função para consultar dados para popular browse
*/

Static Function GetDadosTitulos(nOpc)
    Local cAlias := GetNextAlias()

    If nOpc == 1
        //SE2
        cQry := " SELECT SE2.E2_FILIAL,SE2.R_E_C_N_O_ AS RECSE2,SE2.E2_NUM,SE2.E2_PREFIXO,SE2.E2_FORNECE,SE2.E2_LOJA,SE2.E2_VALOR,SE2.E2_PARCELA "
        cQry += " FROM "+RetSqlName("SE2")+" SE2"
        cQry += " WHERE SE2.D_E_L_E_T_ = ' ' "
        cQry += " AND SE2.E2_FILIAL='"+MV_PAR01+"'" 
        cQry += " AND SE2.E2_NUM>='"+MV_PAR03+"'" 
        cQry += " AND SE2.E2_NUM<='"+MV_PAR04+"'" 
        cQry += " AND SE2.E2_EMISSAO>='"+DTOS(MV_PAR05)+"'" 
        cQry += " AND SE2.E2_EMISSAO<='"+DTOS(MV_PAR06)+"'"
        cQry += " AND SE2.E2_BAIXA=' '" 

        TcQuery cQry New Alias &cAlias

        DBSelectArea(cAlias)
        (cAlias)->(DBGoTop())
        If !(cAlias)->(EOF()) .AND. !(cAlias)->(BOF())
            While   !(cAlias)->(EoF())

                cValor := 'R$ '+ TRANSFORM(Val(Alltrim(Str((cAlias)->E2_VALOR))),"@E 999,999,999.99")

                AADD(aTitulos,{.F.,(cAlias)->E2_FILIAL,(cAlias)->E2_NUM,(cAlias)->E2_PREFIXO,(cAlias)->E2_FORNECE,(cAlias)->E2_LOJA,cValor,(cAlias)->RECSE2,(cAlias)->E2_PARCELA})
            
                (cAlias)->(DBSkip())
            EndDo
        Endif  
        (cAlias)->(DBCloseArea())

    Else
        //SE1
        cQry := " SELECT SE1.E1_FILIAL,SE1.R_E_C_N_O_ AS RECSE1,SE1.E1_NUM,SE1.E1_PREFIXO,SE1.E1_CLIENTE,SE1.E1_LOJA,SE1.E1_VALOR,SE1.E1_PARCELA "
        cQry += " FROM "+RetSqlName("SE1")+" SE1"
        cQry += " WHERE SE1.D_E_L_E_T_ = ' ' "
        cQry += " AND SE1.E1_FILIAL='"+MV_PAR01+"'" 
        cQry += " AND SE1.E1_NUM>='"+MV_PAR03+"'" 
        cQry += " AND SE1.E1_NUM<='"+MV_PAR04+"'" 
        cQry += " AND SE1.E1_EMISSAO>='"+DTOS(MV_PAR05)+"'" 
        cQry += " AND SE1.E1_EMISSAO<='"+DTOS(MV_PAR06)+"'"
        cQry += " AND SE1.E1_BAIXA=' '" 

        TcQuery cQry New Alias &cAlias

        DBSelectArea(cAlias)
        (cAlias)->(DBGoTop())
        If !(cAlias)->(EOF()) .AND. !(cAlias)->(BOF())
            While   !(cAlias)->(EoF())

                cValor := 'R$ '+ TRANSFORM(Val(Alltrim(Str((cAlias)->E1_VALOR))),"@E 999,999,999.99")

                AADD(aTitulos,{.F.,(cAlias)->E1_FILIAL,(cAlias)->E1_NUM,(cAlias)->E1_PREFIXO,(cAlias)->E1_CLIENTE,(cAlias)->E1_LOJA,cValor,(cAlias)->RECSE1,(cAlias)->E1_PARCELA})
            
                (cAlias)->(DBSkip())
            EndDo
        Endif  
        (cAlias)->(DBCloseArea())
    Endif 

Return

/*
	Função para conectar em filiais em tempode execução
*/

Static Function TrocaFilial(cCGC)

    Local aSM0 := FWLoadSM0()
    Local nPos := AScan(aSM0, {|aEmpresa| aEmpresa[18] == cCGC })
    
    cFilBck := cFilAnt
    
    If nPos > 0
        cFilAnt := aSM0[nPos][2]
    EndIf
    
Return
