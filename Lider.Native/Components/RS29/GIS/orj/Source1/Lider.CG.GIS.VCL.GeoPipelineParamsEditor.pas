//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DK11.85.0.33382-Unstable2
// (c)2000-2023 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// 
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  Form for editing pipeline operations' parameters.
}
unit Lider.CG.GIS.VCL.GeoPipelineParamsEditor;
{$HPPEMIT '#pragma link "VCL.Lider.CG.GIS.GeoPipelineParamsEditor"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  Winapi.Windows,
  Winapi.Messages,

  System.Classes,
  Vcl.StdCtrls,
  Vcl.Controls,
  Vcl.Grids,
  Vcl.Graphics,
  Lider.CG.GIS.VCL.GeoModalForm,

  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoPipeline ;

type
  /// <summary>
  ///   Form for editing pipeline operations' parameters.
  /// </summary>
  TGIS_PipelineParamsEditor = class( TGIS_ModalForm )
    private
      iCellCol  : Integer ;
      iCellRow  : Integer ;
      bComboBox : Boolean ;
    private
      lblNoParams : TLabel ;
      grdParams   : TStringGrid ;
      cmbbxList   : TComboBox ;
    private
      FOperation : TGIS_PipelineOperationAbstract ;
    private
      function  fget_Operation : TGIS_PipelineOperationAbstract ;
      procedure fset_Operation ( _operation : TGIS_PipelineOperationAbstract
                               ) ;
    private
      procedure doSelectCell   (     _sender : TObject ;
                                     _col    : Integer ;
                                     _row    : Integer ;
                                 var _select : Boolean
                               ) ;
      procedure doComboChange  (     _sender : TObject
                               ) ;
      procedure doGridScroll   (     _sender : TObject
                               ) ;
    private
      procedure fillGrid ;
    protected
      /// <inheritdoc/>
      procedure initForm     ; override ;
      /// <inheritdoc/>
      procedure initControls ; override ;
      /// <inheritdoc/>
      procedure showForm     ; override ;
      /// <inheritdoc/>
      procedure btnOKClick   ( _sender : TObject
                             ) ; override ;
      /// <inheritdoc/>
      procedure btnHelpClick ( _sender : TObject
                             ) ; override ;
    public

      /// <summary>
      ///   Execute dialog with a given pipeline operation.
      /// </summary>
      /// <param name="_operation">
      ///   pipeline operation
      /// </param>
      /// <param name="_onhelp">
      ///   help notification function; if assigned the help button will be
      ///   visible
      /// </param>
      /// <returns>
      ///    Modal result.
      /// </returns>
      function Execute ( const _operation : TGIS_PipelineOperationAbstract ;
                         const _onhelp    : TGIS_HelpEvent
                       ) : Integer ; overload ;

      /// <summary>
      ///   Execute dialog with a given pipeline operation.
      /// </summary>
      /// <param name="_operation">
      ///   pipeline operation
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function Execute ( const _operation : TGIS_PipelineOperationAbstract
                       ) : Integer ; overload ;

      {#gendoc:hide:GENXDK}
      /// <summary>
      ///   Show parameters editor for a given pipeline operation.
      /// </summary>
      /// <param name="_owner">
      ///   owner component
      /// </param>
      /// <param name="_operation">
      ///   pipeline operation
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      class function ShowPipelineOperationParams(
                         const _owner     : TComponent ;
                         const _operation : TGIS_PipelineOperationAbstract
                       ) : Integer ; overload ;

      {#gendoc:hide:GENXDK}
      /// <summary>
      ///   Show parameters editor for a given pipeline operation.
      /// </summary>
      /// <param name="_owner">
      ///   owner component
      /// </param>
      /// <param name="_operation">
      ///   pipeline operation
      /// </param>
      /// <param name="_onhelp">
      ///   help notification function; if assigned the help button will be
      ///   visible
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      class function ShowPipelineOperationParams(
                         const _owner     : TComponent ;
                         const _operation : TGIS_PipelineOperationAbstract ;
                         const _onhelp    : TGIS_HelpEvent
                       ) : Integer ; overload ;

      /// <summary>
      ///   Show parameters editor for a given pipeline operation.
      /// </summary>
      /// <param name="_operation">
      ///   pipeline operation
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      class function ShowPipelineOperationParams(
                         const _operation : TGIS_PipelineOperationAbstract
                       ) : Integer ; overload ;

      /// <summary>
      ///   Show parameters editor for a given pipeline operation.
      /// </summary>
      /// <param name="_operation">
      ///   pipeline operation
      /// </param>
      /// <param name="_onhelp">
      ///   help notification function; if assigned the help button will be
      ///   visible
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      class function ShowPipelineOperationParams(
                         const _operation : TGIS_PipelineOperationAbstract ;
                         const _onhelp    : TGIS_HelpEvent
                       ) : Integer ; overload ;
    public
      /// <summary>
      ///   Pipeline operation to be edited.
      /// </summary>
      property Operation : TGIS_PipelineOperationAbstract
                           read  fget_Operation
                           write fset_Operation ;
  end ;


//##############################################################################
implementation

uses
  System.SysUtils,
  Vcl.Dialogs,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoResource ;


const
  GIS_PLNPRMEDT_NAME_COL     : Integer = 0 ;
  GIS_PLNPRMEDT_REQUIRED_COL : Integer = 1 ;
  GIS_PLNPRMEDT_VALUE_COL    : Integer = 2 ;


type
  T_stringGrid = class( TStringGrid )
    private
      FOnScroll : TNotifyEvent ;
    private
      procedure msgVScroll ( var _msg : TWMVScroll
                           ) ; message WM_VSCROLL ;
    public
      property OnVScroll : TNotifyEvent
                           read  FOnScroll
                           write FOnScroll ;
  end ;


//==============================================================================
// T_stringGrid
//==============================================================================

  procedure T_stringGrid.msgVScroll(
    var _msg : TWMVScroll
  ) ;
  begin
    inherited ;

    if Assigned( FOnScroll ) then
      FOnScroll( Self ) ;
  end ;


//==============================================================================
// TGIS_PipelineParamsEditor
//==============================================================================

  procedure TGIS_PipelineParamsEditor.initForm ;
  begin
    Self.Caption := 'TGIS_PipelineParamsEditor' ;
    Self.ClientWidth  := 400 ;
    Self.ClientHeight := 300 ;
    Self.Name := 'TGIS_PipelineParamsEditor' ;
    Self.ScalingFlags := Self.ScalingFlags - [sfFont] ;
  end ;


  procedure TGIS_PipelineParamsEditor.initControls ;
  var
    w : Integer ;
    i : Integer ;
  begin
    lblNoParams := TLabel.Create( Self ) ;
    lblNoParams.Parent := Self ;
    lblNoParams.AutoSize := False ;
    lblNoParams.Left := 0 ;
    lblNoParams.ClientWidth := 400 ;
    lblNoParams.Alignment := TAlignment.taCenter ;
    lblNoParams.Top := Self.Height div 2 ;
    lblNoParams.Caption := 'empty' ;
    lblNoParams.Visible := False ;

    grdParams := T_stringGrid.Create( Self ) ;
    grdParams.Parent := Self ;
    grdParams.Left := 8 ;
    grdParams.Top := 8 ;
    grdParams.Width := Self.ClientWidth - 16 ;
    grdParams.Height := Self.ClientHeight - grdParams.Top - 40 ;
    grdParams.Anchors := [ TAnchorKind.akLeft , TAnchorKind.akTop,
                           TAnchorKind.akRight, TAnchorKind.akBottom ] ;
    grdParams.Options := [
      TGridOption.goFixedVertLine, TGridOption.goFixedHorzLine,
      TGridOption.goVertLine, TGridOption.goHorzLine,
      TGridOption.goEditing
    ] ;
    T_stringGrid( grdParams ).OnVScroll := doGridScroll ;

    grdParams.ColCount := 3 ;
    grdParams.ColWidths[GIS_PLNPRMEDT_NAME_COL] := 96 ;
    grdParams.ColWidths[GIS_PLNPRMEDT_REQUIRED_COL] := 64 ;
    w := 0 ;
    for i := 0 to GIS_PLNPRMEDT_VALUE_COL - 1 do
      Inc( w, grdParams.ColWidths[i] + 2 ) ;
    grdParams.ColWidths[GIS_PLNPRMEDT_VALUE_COL] :=
      grdParams.Width - w div 2 - 16 ;

    grdParams.RowCount := 2 ;
    grdParams.Cells[GIS_PLNPRMEDT_NAME_COL    , 0] :=
      _rsrc( GIS_RS_PLNPRMEDT_NAME_COL_TITLE ) ;
    grdParams.Cells[GIS_PLNPRMEDT_REQUIRED_COL, 0] :=
      _rsrc( GIS_RS_PLNPRMEDT_REQUIRED_COL_TITLE ) ;
    grdParams.Cells[GIS_PLNPRMEDT_VALUE_COL   , 0] :=
      _rsrc( GIS_RS_PLNPRMEDT_VALUE_COL_TITLE ) ;

    grdParams.FixedCols := 2 ;
    grdParams.FixedRows := 1 ;

    grdParams.OnSelectCell := doSelectCell ;
    grdParams.OnTopLeftChanged := doGridScroll ;

    cmbbxList := TComboBox.Create( Self ) ;
    cmbbxList.Parent := Self ;
    cmbbxList.Left := grdParams.Left + w ;
    cmbbxList.Top := 0 ;
    cmbbxList.ClientWidth := grdParams.ColWidths[GIS_PLNPRMEDT_VALUE_COL];
    cmbbxList.OnChange := doComboChange ;
    cmbbxList.OnSelect := doComboChange ;

    cmbbxList.Visible := False ;
    bComboBox := False ;
  end ;


  function TGIS_PipelineParamsEditor.fget_Operation
    : TGIS_PipelineOperationAbstract ;
  begin
    Result := FOperation ;
  end ;


  procedure TGIS_PipelineParamsEditor.fset_Operation(
    _operation : TGIS_PipelineOperationAbstract
  ) ;
  begin
    if not Assigned( _operation ) then
      exit ;

    FOperation := _operation ;

    Self.Caption := Format( _rsrc( GIS_RS_PLNPRMEDT_FORM_CAPTION ),
                            [ FOperation.Name ] ) ;
    fillGrid ;
  end ;


  procedure TGIS_PipelineParamsEditor.doGridScroll(
    _sender : TObject
  ) ;
  var
    rct : TRect ;
    b   : Boolean ;
  begin
    if not bComboBox then
      exit ;

    rct := grdParams.CellRect( iCellCol, iCellRow ) ;
    cmbbxList.Top := grdParams.Top + rct.Top + 2 ;
    cmbbxList.Left := grdParams.Left + rct.Left + 2 ;

    b := ( rct.Left = 0 ) and ( rct.Top = 0 ) ;

    if b then
      cmbbxList.Visible := False
    else
      cmbbxList.Visible := True ;
  end ;


  procedure TGIS_PipelineParamsEditor.doSelectCell(
        _sender : TObject ;
        _col    : Integer ;
        _row    : Integer ;
    var _select : Boolean
  ) ;
  var
    tkn  : TGIS_Tokenizer ;
    only : Boolean ;
    val  : String ;
    rct  : TRect ;
    i    : Integer ;
    k    : Integer ;
  begin
    cmbbxList.Visible := False ;

    if _col <> GIS_PLNPRMEDT_VALUE_COL then
      exit ;

    iCellCol := _col ;
    iCellRow := _row ;

    bComboBox := False ;

    val := FOperation.Params[_row-1].Predefined ;
    if IsStringEmpty( val ) then
      exit ;

    bComboBox := True ;

    cmbbxList.Clear ;
    cmbbxList.BringToFront ;

    rct := grdParams.CellRect( _col, _row ) ;
    cmbbxList.Top := grdParams.Top + rct.Top + 2 ;
    cmbbxList.Left := grdParams.Left + rct.Left + 2 ;

    only := val[StringFirst] = '!' ;
    if only then
      cmbbxList.Style := TComboBoxStyle.csDropDownList
    else
      cmbbxList.Style := TComboBoxStyle.csDropDown ;

    if only then
      val := Copy( val, StringFirst + 1, Length( val ) - 1 ) ;

    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.Execute( val, [ '|' ] ) ;
      for i := 0 to tkn.Result.Count - 1 do
        cmbbxList.AddItem( tkn.Result[i], nil ) ;
    finally
      FreeObject( tkn ) ;
    end ;

    if only then begin
      val := grdParams.Cells[GIS_PLNPRMEDT_VALUE_COL,_row] ;
      if IsStringEmpty( val ) then
        val := FOperation.Params[_row-1].Default ;

      k := -1 ;
      for i := 0 to cmbbxList.Items.Count - 1 do begin
        if CompareText( val, cmbbxList.Items[i] ) = 0 then begin
          k := i ;
          break ;
        end ;
      end ;

      if k = -1 then
        cmbbxList.ItemIndex := 0
      else
        cmbbxList.ItemIndex := k ;
    end
    else begin
      val := grdParams.Cells[GIS_PLNPRMEDT_VALUE_COL,_row] ;
      if IsStringEmpty( val ) then
        val := FOperation.Params[_row-1].Default ;

      cmbbxList.Text := val ;
    end ;

    cmbbxList.Visible := True ;
  end ;


  procedure TGIS_PipelineParamsEditor.doComboChange(
    _sender : TObject
  ) ;
  begin
    grdParams.Cells[GIS_PLNPRMEDT_VALUE_COL,iCellRow] := cmbbxList.Text ;
  end ;


  procedure TGIS_PipelineParamsEditor.showForm ;
  begin
    inherited ;

    btnHelp.Visible := Assigned( pOnHelp ) ;

    if not Assigned( FOperation ) then begin
      Self.Caption := _rsrc( GIS_RS_PLNPRMEDT_FORM_CAPTION ) ;
      lblNoParams.Caption := _rsrc( GIS_RS_PLNPRMEDT_NO_OPERATION ) ;
      lblNoParams.Visible := True ;
      grdParams.Visible := False ;
    end
    else
    if FOperation.Params.Count = 0 then begin
      lblNoParams.Caption := Format( _rsrc( GIS_RS_PLNPRMEDT_NO_PARAMS ),
                                     [ FOperation.Name ] ) ;
      lblNoParams.Visible := True ;
      grdParams.Visible := False ;
    end ;
  end ;


  procedure TGIS_PipelineParamsEditor.btnOKClick(
    _sender : TObject
  ) ;
  var
    val : String ;
    i   : Integer ;
  begin
    for i := 0 to FOperation.Params.Count - 1 do begin
      if FOperation.Params[i].Required and
         IsStringEmpty( grdParams.Cells[GIS_PLNPRMEDT_VALUE_COL,i+1] ) then
      begin
        MessageDlg(
          Format( _rsrc( GIS_RS_PLNPRMEDT_REQUIRED_PARAM ),
                  [ FOperation.Params[i].Name ] ),
          TMsgDlgType.mtWarning, [TMsgDlgBtn.mbOK], 0, TMsgDlgBtn.mbOK
        ) ;
        exit ;
      end ;
    end ;

    for i := 0 to FOperation.Params.Count - 1 do begin
      val := grdParams.Cells[GIS_PLNPRMEDT_VALUE_COL,i+1] ;
      if IsStringEmpty( val ) then
        FOperation.Params[i].Value := FOperation.Params[i].Default
      else
        FOperation.Params[i].Value := val ;
    end ;

    inherited ;
  end ;


  procedure TGIS_PipelineParamsEditor.btnHelpClick(
    _sender : TObject
  ) ;
  begin
    if Assigned( pOnHelp ) then
      pOnHelp( Self, Name + '?detail=' + FOperation.Name ) ;
  end ;


  procedure TGIS_PipelineParamsEditor.fillGrid ;
  var
    str : String ;
    i   : Integer ;
  begin
    str := _rsrc( GIS_RS_PLNPRMEDT_REQUIRED_YES ) ;
    grdParams.RowCount := FOperation.Params.Count + 1 ;

    for i := 0 to FOperation.Params.Count - 1 do begin

      grdParams.RowHeights[i +1] := cmbbxList.Height ;
      grdParams.Cells[0,i+1] := FOperation.Params[i].Name ;

      if FOperation.Params[i].Required then
        grdParams.Cells[GIS_PLNPRMEDT_REQUIRED_COL,i+1] := str ;

      grdParams.Cells[GIS_PLNPRMEDT_VALUE_COL,i+1] :=
        FOperation.Params[i].Value ;
    end ;
  end ;


  function TGIS_PipelineParamsEditor.Execute(
    const _operation : TGIS_PipelineOperationAbstract ;
    const _onhelp    : TGIS_HelpEvent
  ) : Integer ;
  begin
    pOnHelp := _onhelp ;
    Operation := _operation ;

    grdParams.DefaultRowHeight := cmbbxList.ClientHeight ;

    Result := ShowModal ;
  end ;


  function TGIS_PipelineParamsEditor.Execute(
    const _operation : TGIS_PipelineOperationAbstract
  ) : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( _operation, hlp ) ;
  end ;


  class function TGIS_PipelineParamsEditor.ShowPipelineOperationParams(
    const _owner     : TComponent ;
    const _operation : TGIS_PipelineOperationAbstract
  ) : Integer ;
  begin
    Result := ShowPipelineOperationParams( _owner, _operation,
                TGIS_HelpEvent( nil ) ) ;
  end ;


  class function TGIS_PipelineParamsEditor.ShowPipelineOperationParams(
    const _owner     : TComponent ;
    const _operation : TGIS_PipelineOperationAbstract ;
    const _onhelp    : TGIS_HelpEvent
  ) : Integer ;
  var
    frm : TGIS_PipelineParamsEditor ;
  begin
    if not assigned( _operation ) then begin
      Result := mrNone ;
      exit ;
    end ;

    frm := TGIS_PipelineParamsEditor.Create( _owner ) ;
    try
      Result := frm.Execute( _operation, _onhelp ) ;

      _operation.Result := Result = mrOk ;
    finally
      FreeObject( frm ) ;
    end ;
  end ;


  class function TGIS_PipelineParamsEditor.ShowPipelineOperationParams(
    const _operation : TGIS_PipelineOperationAbstract
  ) : Integer ;
  begin
    Result := ShowPipelineOperationParams( _operation,
                TGIS_HelpEvent( nil ) ) ;
  end ;


  class function TGIS_PipelineParamsEditor.ShowPipelineOperationParams(
    const _operation : TGIS_PipelineOperationAbstract ;
    const _onhelp    : TGIS_HelpEvent
  ) : Integer ;
  var
    frm : TGIS_PipelineParamsEditor ;
  begin
    if not assigned( _operation ) then begin
      Result := mrNone ;
      exit ;
    end ;

    frm := TGIS_PipelineParamsEditor.Create( nil ) ;
    try
      Result := frm.Execute( _operation, _onhelp ) ;

      _operation.Result := Result = mrOk ;
    finally
      FreeObject( frm ) ;
    end ;
  end ;


//==================================== END =====================================
end.

