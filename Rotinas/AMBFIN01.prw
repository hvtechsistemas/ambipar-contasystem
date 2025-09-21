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
            If nOpc == 1
                TelaCtaPagar()
            Else 
                TelaCtaReceber()
            Endif
        Else 
            MsgAlert("Dados não encontrados com os filtros informados!!!")
        Endif
    Endif

Return 

/*
    Função para consultar dados para popular browse
*/

Static Function GetDadosTitulos(nOpc)
    Local cAlias := GetNextAlias()

    If nOpc == 1
        //SE2
        cQry := " SELECT SE2.E2_FILIAL,SE2.R_E_C_N_O_ AS RECSE2,SE2.E2_NUM,SE2.E2_PREFIXO,SE2.E2_FORNECE,SE2.E2_LOJA,SE2.E2_VALOR "
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

                AADD(aTitulos,{.F.,(cAlias)->E2_FILIAL,(cAlias)->E2_NUM,(cAlias)->E2_PREFIXO,(cAlias)->E2_FORNECE,(cAlias)->E2_LOJA,(cAlias)->E2_VALOR,(cAlias)->RECSE2})
            
                (cAlias)->(DBSkip())
            EndDo
        Endif  
        (cAlias)->(DBCloseArea())

    Else
        //SE1
        cQry := " SELECT SE1.E1_FILIAL,SE1.R_E_C_N_O_ AS RECSE1,SE1.E1_NUM,SE1.E1_PREFIXO,SE1.E1_CLIENTE,SE1.E1_LOJA,SE1.E1_VALOR "
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

                AADD(aTitulos,{.F.,(cAlias)->E1_FILIAL,(cAlias)->E1_NUM,(cAlias)->E1_PREFIXO,(cAlias)->E1_CLIENTE,(cAlias)->E1_LOJA,(cAlias)->E1_VALOR,(cAlias)->RECSE1})
            
                (cAlias)->(DBSkip())
            EndDo
        Endif  
        (cAlias)->(DBCloseArea())
    Endif 

Return

/*
    Função para montar tela markbrowse com dados do contas a pagar 
*/

Static Function TelaCtaPagar()

    DEFINE MsDIALOG o3Dlg TITLE 'Seleção de Titulos' From 0, 4 To 550, 980 Pixel

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
    oPedBrw:addColumn({"Recno"              , {||aTitulos[oPedBrw:nAt,08]}, "C", "@!"    , 0,  20    ,                            , .T. , , .F.,, "aTitulos[oPedBrw:nAt,08]",, .F., .T.,                                    , "ETDESPES1"    })

	oPedBrw:setEditCell( .T. , { || .T. } ) //activa edit and code block for validation

	oPedBrw:Activate(.T.)

	ACTIVATE MSDIALOG o3Dlg CENTERED  ON INIT (EnchoiceBar(o3Dlg,{|| (lConfirmed := CONFIRMED , ConsultaItens(aTitulos)) },{||(lConfirmed := CANCELED , o3Dlg:End() )},,))


Return


//Função para consultar os pedidos que o usuario marcou

Static Function ConsultaItens(aDados)
	Local _ni := 1

	For _ni := 1 to len(aDados)
		If aDados[_ni,1]
            //Marcados

            nRecTitulo := aDados[_ni,8]
            SE2->(dbGoto(nRecTitulo))

            cFilDest := MV_PAR02
            cCodForn := SE2->E2_FORNECE
            cLOjaFor := SE2->E2_LOJA

            TranfSE2(nRecTitulo,cFilDest,cCodForn,cLOjaFor)

		Endif
	Next

	o3Dlg:End()

Return .T.

Static function TranfSE2(nRecTitulo,cFilDest,cCodForn,cLOjaFor)
    Local aCab        := {}
 
    Private lMsErroAuto := .F.
 
    SE2->(dbGoto(nRecTitulo))
 
    aadd(aCab, {"E6_FILDEB",cFilDest})
    aadd(aCab, {"E6_CLIENTE",cCodForn})
    aadd(aCab, {"E6_LOJA",cLOjaFor})
    aadd(aCab, {"AUTHISTDEB","solicitado pelo FINA621 via execauto."})

    //Inclusao de Solicitação de transferencia
    MSExecAuto({|a, b| FINA621(a,b)}, aCab,3)

    If lMsErroAuto
        //mostraerro() //Se for usado em interface
        conout('erro na inclusao')
    Else
        //MsgStop("Processo realizado com sucesso.") // Se for usado em interface
        conout('Processo realizado com sucesso.')
    EndIf

Return

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
    Função para montar tela markbrowse com dados do contas a receber 
*/

Static Function TelaCtaReceber()

    

Return


