//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DKv100.1.37476
// (c)2000-2025 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// ILKER#LIDERYAZILIM.COM-481078-KSVX7UYN-1D12B8B5
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  Extended Line Symbology Editor form.
}

unit VCL.GisLineSymbolEditor ;
{$HPPEMIT '#pragma link "VCL.GisLineSymbolEditor"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Classes,
  Winapi.Windows,
  Vcl.Forms,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,

  {$IFDEF GEN_XDK}
    XDK.Splitter,
  {$ENDIF}

  GisTypes,
  GisTypesUI,
  GisSymbol,
  GisLayerVector,
  VCL.GisModalForm,
  VCL.GisViewerWnd ;


type

  /// <summary>
  ///   A form which allows to edit a symbol defining string and preview
  ///   the result.
  /// </summary>
  TGIS_LineSymbolEditor = class( TGIS_ModalForm )
    private
      pnlMain       : TPanel ;
      bvlMain       : TBevel ;
      pnlLeft       : TPanel ;
      vspltrMain    : TSplitter ;
      pnlRight      : TPanel ;
      btnOpen       : TButton ;
      btnSave       : TButton ;
      lblCommands   : TLabel ;
      btnComGOTO    : TButton ;
      btnComMOVE    : TButton ;
      btnComFOREND  : TButton ;
      btnComDRAW    : TButton ;
      btnComLINE    : TButton ;
      btnComOUTLINE : TButton ;
      btnComFILL    : TButton ;
      btnComWIDTH   : TButton ;
      btnComCOLOR   : TButton ;
      lblUnits      : TLabel ;
      lblUnitsList  : TLabel ;
      lblPreview    : TLabel ;
      gisPreview    : TGIS_ViewerWnd ;
      hspltrRight   : TSplitter ;
      lblWizard     : TLabel ;
      pnlWizard     : TPanel ;
      cmbbxWizStart : TComboBox ;
      cmbbxWizLine  : TComboBox ;
      cmbbxWizEnd   : TComboBox ;
      btnWizard     : TButton ;
      lblCode       : TLabel ;
      mmCode        : TMemo ;
      lblError      : TLabel ;
      tmrUpdate     : TTimer ;
      tmrResize     : TTimer ;
    private
      skeleton      : TGIS_LayerVector ;
      layer         : TGIS_LayerVector ;
      previous      : String ;
      is_err        : Boolean ;
      err_s         : Integer ;
      err_l         : Integer ;
    private // property access routines
      function  fget_Symbol : String ;
      procedure fset_Symbol ( const _symbol : String
                            ) ;
    private // event handlers
      procedure doGOTOClick       (  _sender : TObject
                                  ) ;
      procedure doMOVEClick       (  _sender : TObject
                                  ) ;
      procedure doFORENDClick     (  _sender : TObject
                                  ) ;
      procedure doDRAWClick       (  _sender : TObject
                                  ) ;
      procedure doLINEClick       (  _sender : TObject
                                  ) ;
      procedure doOUTLINEClick    (  _sender : TObject
                                  ) ;
      procedure doFILLClick       (  _sender : TObject
                                  ) ;
      procedure doWIDTHClick      (  _sender : TObject
                                  ) ;
      procedure doCOLORClick      (  _sender : TObject
                                  ) ;
      procedure doOpenClick       (  _sender : TObject
                                  ) ;
      procedure doSaveClick       (  _sender : TObject
                                  ) ;
      procedure doWizardClick     (  _sender : TObject
                                  ) ;
      procedure doCodeKeyPress    (  _sender : TObject ;
                                    var _key : Char
                                  ) ;
      procedure doCodeChange      (  _sender : TObject
                                  ) ;
      procedure doUpdateTimer     (  _sender : TObject
                                  ) ;
      procedure doErrorClick      (  _sender : TObject
                                  ) ;
      procedure doErrorMouseEnter (  _sender : TObject
                                  ) ;
      procedure doErrorMouseLeave (  _sender : TObject
                                  ) ;
      procedure doResize          (  _sender : TObject
                                  ) ;
      procedure doResizeTimer     (  _sender : TObject
                                  ) ;
      procedure doSplitterMoved   (  _sender : TObject
                                  ) ;
    private
      procedure createLayout   ;
      procedure createLayers   ;
      procedure updatePreview  ;
      procedure comClick       ( const _com : String ;
                                 const _len : Integer
                               ) ;
    protected

      /// <inheritdoc/>
      procedure initForm        ; override;

      /// <inheritdoc/>
      procedure initControls    ; override;

    public // destructors
      /// <inheritdoc/>
      destructor Destroy ; override;

    public

      /// <summary>
      ///   Execute dialog on a bitmap given by path.
      /// </summary>
      /// <param name="_path">
      ///   path to the symbol.
      /// </param>
      /// <param name="_onhelp">
      ///   help notification function; if assigned the help button will be
      ///   visible and help support will be enabled;
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function Execute( const _path   : String        ;
                        const _onhelp : TGIS_HelpEvent
                      ) : Integer ; overload;

      /// <summary>
      ///   Execute dialog on a bitmap given by path.
      /// </summary>
      /// <param name="_path">
      ///   path to the symbol.
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function Execute( const _path   : String
                      ) : Integer ; overload;

    public
      /// <summary>
      ///   Symbol defining string.
      /// </summary>
      property Symbol : String
                        read  fget_Symbol
                        write fset_Symbol ;
  end ;


//##############################################################################
implementation

uses
  System.Types,
  System.SysUtils,

  VCL.Graphics,
  VCL.Dialogs,

  GisResource,
  GisRtl,
  GisFunctions,
  GisParams,
  GisLineSymbolEditorUtils,
  VCL.GisViewerBmp,
  VCL.GisRendererGdiPlus,
  VCL.GisComboBoxHelper,
  VCL.GisControlHelper ;

const
  LOCAL_DLG_FILTER    : String = 'Symbol file (*.sym)|*.sym' ;
  LOCAL_DLG_DEFEXT    : String = 'sym' ;

type
  T_LineSymbolElement = ( SymbolStart, SymbolLine, SymbolEnd ) ;

  T_LineSymbolComboBox = class( TComboBox )
    private
      helper   : TGIS_ComboBoxHelper ;
      symList  : TGIS_PredefinedLineSymbolList ;
      viewer   : TGIS_ViewerBmp ;
      vWidth   : Integer ;
      vHeight  : Integer ;
      layer    : TGIS_LayerVector ;
    private
      FElement : T_LineSymbolElement ;
    private
      procedure prepareLayer ;
    private
      function  doValue  ( _sender  : TObject ;
                           _value   : String
                         ) : String ;
      procedure doRender ( _control : TComboBox       ;
                           _rect    : TRect           ;
                           _state   : TOwnerDrawState ;
                           _class   : Char            ;
                           _caption : String          ;
                           _value   : String
                         ) ;
    public
      constructor Create ( _owner : TComponent
                         ) ;
    public
      destructor Destroy ; override;
    public
      procedure UpdateList ;
      function  GetSymbol  ( const _start  : Integer ;
                             const _line   : Integer ;
                             const _end    : Integer
                           ) : String ; overload;
    public
      property SymbolElement : T_LineSymbolElement
                               read  FElement
                               write FElement ;
  end ;


//==============================================================================
// T_LineSymbolComboBox
//==============================================================================

  constructor T_LineSymbolComboBox.Create(
    _owner : TComponent
  ) ;
  begin
    inherited ;

    helper := TGIS_ComboBoxHelper.Create( Self, '', 0 ) ;
    helper.ValueEvent  := doValue ;
    helper.RenderEvent := doRender ;

    Style := TComboBoxStyle.csOwnerDrawFixed ;

    symList := TGIS_PredefinedLineSymbolList.Create ;
    viewer := TGIS_ViewerBmp.Create ;
    viewer.Renderer := TGIS_RendererVclGdiPlus.Create ;
    viewer.Color := TGIS_Color.None ;

    FElement := T_LineSymbolElement.SymbolLine ;

    vWidth  := 0 ;
    vHeight := 0 ;

    prepareLayer ;
  end ;


  destructor T_LineSymbolComboBox.Destroy ;
  begin
    FreeObject( helper  ) ;
    FreeObject( symList ) ;
    FreeObject( viewer  ) ;

    inherited ;
  end ;


  procedure T_LineSymbolComboBox.prepareLayer ;
  var
    shp : TGIS_Shape ;
  begin
    viewer.RestrictedDrag := False ;

    layer := TGIS_LayerVector.Create ;
    layer.Name := 'Symbol' ;
    layer.Open ;

    shp := layer.CreateShape( TGIS_ShapeType.Arc, TGIS_DimensionType.XY ) ;
    shp.AddPart ;
    shp.AddPoint( GisPoint( -5.0, 0.0 ) ) ;
    shp.AddPoint( GisPoint(  5.0, 0.0 ) ) ;

    viewer.Add( layer ) ;
    viewer.RecalcExtent ;
    viewer.FullExtent ;
  end ;


  function T_LineSymbolComboBox.doValue(
   _sender : TObject ;
   _value  : String
  ) : String ;
  begin
    Result := TGIS_ComboBoxHelper.PrepareItem(
      False,
      'C',
      TGIS_ComboBoxHelperPosition.Bottom,
      _value,
      _value
    ) ;
  end ;


  procedure T_LineSymbolComboBox.doRender(
    _control : TComboBox       ;
    _rect    : TRect           ;
    _state   : TOwnerDrawState ;
    _class   : Char            ;
    _caption : String          ;
    _value   : String
  ) ;
  var
    sym : String ;
    ext : TGIS_Extent ;
    i   : Integer ;
  begin
    i := StrToInt( _value ) ;

    case SymbolElement of
      T_LineSymbolElement.SymbolStart :
        begin
          if i = 0 then
            sym := '&'
          else
            sym := '&' + symList.PrepareSymbol( i, 1, 0 ) ;

          layer.Params.Line.Symbol := SymbolList.Prepare( sym ) ;
          ext := GisExtent( -6.0, -0.001, -2.5,  0.001 ) ;
        end ;
      T_LineSymbolElement.SymbolLine      :
        begin
          if i = 0 then
            sym := '&'
          else
            sym := '&' + symList.PrepareSymbol( 0, i, 0 ) ;

          layer.Params.Line.Symbol := SymbolList.Prepare( sym ) ;
          ext := GisExtent( -5.5, -0.001,  5.5,  0.001 ) ;
        end ;
      T_LineSymbolElement.SymbolEnd   :
        begin
          if i = 0 then
            sym := '&'
          else
            sym := '&' + symList.PrepareSymbol( 0, 1, i ) ;

          layer.Params.Line.Symbol := SymbolList.Prepare( sym ) ;
          ext := GisExtent(  2.5, -0.001,  8.0,  0.001 ) ;
        end ;
    end ;

    viewer.VisibleExtent := ext ;
    if ( odSelected in _state ) then
      viewer.Color := TGIS_Color.FromBGR( ColorToRGB( clHighlight ) )
    else
      viewer.Color := TGIS_Color.FromBGR( ColorToRGB( Color ) ) ;
    viewer.Draw ;

    _control.Canvas.Draw( _rect.Left, _rect.Top, TBitmap( viewer.Bitmap ) ) ;
  end ;


  procedure T_LineSymbolComboBox.UpdateList ;
  var
    idx : Integer ;
    cnt : Integer ;
    i   : Integer ;
  begin
    if Width < 0 then
      exit ;
    if ( vWidth = Width ) and ( vHeight = ItemHeight ) then
      exit ;

    cnt := 0 ;
    vWidth  := Width ;
    vHeight := ItemHeight ;
    viewer.SetSize( vWidth, vHeight ) ;

    idx := ItemIndex ;
    if idx < 0 then
      idx := 0 ;

    case SymbolElement of
      T_LineSymbolElement.SymbolStart : cnt := symList.StartCount ;
      T_LineSymbolElement.SymbolLine  : cnt := symList.LineCount  ;
      T_LineSymbolElement.SymbolEnd   : cnt := symList.EndCount   ;
    end ;

    Items.BeginUpdate ;
    Clear ;
    for i := 0 to cnt - 1 do begin
      helper.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False,
          'C',
          TGIS_ComboBoxHelperPosition.Bottom,
          IntToStr( ItemCount ),
          IntToStr( ItemCount )
        )
      ) ;
    end ;
    Items.EndUpdate ;

    ItemIndex := idx ;
  end ;


  function T_LineSymbolComboBox.GetSymbol(
    const _start : Integer ;
    const _line  : Integer ;
    const _end   : Integer
  ) : String ;
  begin
    Result := symList.PrepareSymbol( _start, _line, _end ) ;
  end ;


//==============================================================================
// TGIS_LineSymbolEditor
//==============================================================================

  destructor TGIS_LineSymbolEditor.Destroy ;
  begin
    inherited ;
  end ;

  procedure TGIS_LineSymbolEditor.initForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_LS_FRM_CAPTION ) ;
    Self.ClientWidth := 640 ;
    Self.ClientHeight := 480 ;
    Self.Name := 'TGIS_LineSymbolEditor' ;
  end ;

  procedure TGIS_LineSymbolEditor.initControls ;
  begin
    createLayout ;
    createLayers ;
    is_err := False ;
  end ;

  function TGIS_LineSymbolEditor.fget_Symbol : String ;
  var
    sb   : TStringBuilder ;
    prsr : TGIS_LineSymbolParser ;
    str  : String ;
    i    : Integer ;
  begin
    sb := TStringBuilder.Create ;
    prsr := TGIS_LineSymbolParser.Create ;
    try
      for i := 0 to mmCode.Lines.Count - 1 do begin
        str := mmCode.Lines[i] ;
        str := prsr.Compactify( str ) ;
        sb.Append( str ) ;
      end ;

      str := sb.ToString ;
    finally
      FreeObject( prsr ) ;
      FreeObject( sb ) ;
    end ;

    Result := str ;
  end ;


  procedure TGIS_LineSymbolEditor.fset_Symbol(
    const _symbol : String
  ) ;
  var
    prsr : TGIS_LineSymbolParser ;
  begin
    if Length( _symbol ) = 0 then begin
      mmCode.Text := GIS_LS_DEF_SYMBOL ;
      exit ;
    end ;

    prsr := TGIS_LineSymbolParser.Create ;
    try
      mmCode.Text := prsr.Expand( _symbol ) ;
    finally
      FreeObject( prsr ) ;
    end ;
  end ;


  procedure TGIS_LineSymbolEditor.doGOTOClick(
    _sender : TObject
  ) ;
  begin
    comClick( GIS_LS_CMD_GOTO + '(?)', 6 ) ;
  end ;


  procedure TGIS_LineSymbolEditor.doMOVEClick(
    _sender : TObject
  ) ;
  begin
    comClick( GIS_LS_CMD_MOVE + '(? ?)', 6 ) ;
  end ;


  procedure TGIS_LineSymbolEditor.doFORENDClick(
    _sender : TObject
  ) ;
  begin
    comClick( GIS_LS_CMD_FOR + '(?)' + GIS_LS_CMD_END + '()', 5 ) ;
  end ;


  procedure TGIS_LineSymbolEditor.doDRAWClick(
    _sender : TObject
  ) ;
  begin
    comClick( GIS_LS_CMD_DRAW + '(? ?)', 6 ) ;
  end ;


  procedure TGIS_LineSymbolEditor.doLINEClick(
    _sender : TObject
  ) ;
  begin
    comClick( GIS_LS_CMD_LINE + '(?)', 6 ) ;
  end ;


  procedure TGIS_LineSymbolEditor.doOUTLINEClick(
    _sender : TObject
  ) ;
  begin
    comClick( GIS_LS_CMD_OUTLINE + '(? ? ? ?)', 9 ) ;
  end ;


  procedure TGIS_LineSymbolEditor.doFILLClick(
    _sender : TObject
  ) ;
  begin
    comClick( GIS_LS_CMD_FILL + '(? ? ? ? ? ?)', 6 ) ;
  end ;


  procedure TGIS_LineSymbolEditor.doWIDTHClick(
    _sender : TObject
  ) ;
  begin
    comClick( GIS_LS_CMD_WIDTH + '(?)', 7 ) ;
  end ;


  procedure TGIS_LineSymbolEditor.doCOLORClick(
    _sender : TObject
  ) ;
  begin
    comClick( GIS_LS_CMD_COLOR + '(?,?,?)', 7 ) ;
  end ;


  procedure TGIS_LineSymbolEditor.doOpenClick(
    _sender : TObject
  ) ;
  var
    dlg : TOpenDialog ;
  begin
    dlg := TOpenDialog.Create( Self ) ;
    try
      dlg.Filter := LOCAL_DLG_FILTER ;
      dlg.DefaultExt := LOCAL_DLG_DEFEXT ;
      if dlg.Execute then
        mmCode.Lines.LoadFromFile( dlg.FileName ) ;
    finally
      FreeObject( dlg ) ;
    end ;
  end ;


  procedure TGIS_LineSymbolEditor.doSaveClick(
    _sender : TObject
  ) ;
  var
    dlg : TSaveDialog ;
  begin
    dlg := TSaveDialog.Create( Self ) ;
    try
      dlg.Filter := LOCAL_DLG_FILTER ;
      dlg.DefaultExt := LOCAL_DLG_DEFEXT ;
      if dlg.Execute then
        mmCode.Lines.SaveToFile( dlg.FileName ) ;
    finally
      FreeObject( dlg ) ;
    end ;
  end ;


  procedure TGIS_LineSymbolEditor.doWizardClick(
    _sender : TObject
  ) ;
  var
    prsr : TGIS_LineSymbolParser ;
    syms : Integer ;
    syml : Integer ;
    syme : Integer ;
    sym  : String ;
  begin
    syms := T_LineSymbolComboBox( cmbbxWizStart ).ItemIndex ;
    syml := T_LineSymbolComboBox( cmbbxWizLine  ).ItemIndex ;
    syme := T_LineSymbolComboBox( cmbbxWizEnd   ).ItemIndex ;

    sym := T_LineSymbolComboBox( cmbbxWizLine ).GetSymbol( syms, syml, syme ) ;

    prsr := TGIS_LineSymbolParser.Create ;
    try
      mmCode.Text := prsr.Expand( sym ) ;
    finally
      FreeObject( prsr ) ;
    end ;
  end ;


  procedure TGIS_LineSymbolEditor.doCodeKeyPress(
    _sender  : TObject ;
    var _key : Char
  ) ;
  var
    c : Integer ;
  begin
    c := Ord( _key ) ;
    if ( c >= 97 ) and ( c <= 122 ) then
      _key := Char( c - 32 ) ;
  end ;


  procedure TGIS_LineSymbolEditor.doCodeChange(
    _sender : TObject
  ) ;
  begin
    tmrUpdate.Enabled := False ;
    tmrUpdate.Enabled := True ;
  end ;


  procedure TGIS_LineSymbolEditor.doUpdateTimer(
    _sender : TObject
  ) ;
  begin
    updatePreview ;
    tmrUpdate.Enabled := False ;
  end ;


  procedure TGIS_LineSymbolEditor.doErrorClick(
    _sender : TObject
  ) ;
  begin
    if not is_err then
      exit ;

    mmCode.SelStart  := err_s ;
    mmCode.SelLength := err_l ;
  end ;


  procedure TGIS_LineSymbolEditor.doErrorMouseEnter(
    _sender : TObject
  ) ;
  begin
    if not is_err then
      exit ;

    lblError.Cursor := crHandPoint ;
    lblError.Font.Style := [ TFontStyle.fsUnderline ] ;
  end ;


  procedure TGIS_LineSymbolEditor.doErrorMouseLeave(
    _sender : TObject
  ) ;
  begin
    if not is_err then
      exit ;

    lblError.Cursor := crDefault ;
    lblError.Font.Style := [ ] ;
  end ;


  procedure TGIS_LineSymbolEditor.doResize(
    _sender : TObject
  ) ;
  begin
    tmrResize.Enabled := False ;
    tmrResize.Enabled := True ;
  end ;


  procedure TGIS_LineSymbolEditor.doResizeTimer(
    _sender : TObject
  ) ;
  var
    idx : Integer ;
  begin
    idx := cmbbxWizLine.ItemIndex ;
    T_LineSymbolComboBox( cmbbxWizLine ).UpdateList ;
    T_LineSymbolComboBox( cmbbxWizStart ).UpdateList ;
    T_LineSymbolComboBox( cmbbxWizEnd ).UpdateList ;

    if idx = -1 then begin
      if IsStringEmpty( Symbol ) then
        cmbbxWizLine.ItemIndex := 22
      else
        cmbbxWizLine.ItemIndex := 0 ;
    end ;

    gisPreview.FullExtent ;

    tmrResize.Enabled := False ;
  end ;


  procedure TGIS_LineSymbolEditor.doSplitterMoved(
    _sender : TObject
  ) ;
  begin
    gisPreview.FullExtent ;
  end ;


  procedure TGIS_LineSymbolEditor.createLayout ;
  var
    ltrb : TAnchors ;
    ltr  : TAnchors ;
    tr   : TAnchors ;
    lb   : TAnchors ;
    lrb  : TAnchors ;
    t    : Integer ;
    i    : Integer ;
  begin
    ltrb := [ TAnchorKind.akLeft, TAnchorKind.akTop,
              TAnchorKind.akRight, TAnchorKind.akBottom ] ;
    lrb := [ TAnchorKind.akLeft, TAnchorKind.akRight, TAnchorKind.akBottom ] ;
    ltr := [ TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight ] ;
    if BiDiMode = bdRightToLeft then begin
      tr := [ TAnchorKind.akTop, TAnchorKind.akLeft ] ;
      lb := [ TAnchorKind.akRight, TAnchorKind.akBottom ] ;
    end else begin
      tr := [ TAnchorKind.akTop, TAnchorKind.akRight ] ;
      lb := [ TAnchorKind.akLeft, TAnchorKind.akBottom ] ;
    end;

    tmrUpdate := TTimer.Create( Self ) ;
    tmrUpdate.Interval := 500 ;
    tmrUpdate.Enabled := False ;
    tmrUpdate.OnTimer := doUpdateTimer ;

    tmrResize := TTimer.Create( Self ) ;
    tmrResize.Interval := 500 ;
    tmrResize.Enabled := False ;
    tmrResize.OnTimer := doResizeTimer ;

    btnOpen := TButton.Create( Self ) ;
    btnOpen.Parent := Self ;
    btnOpen.Height := 25 ;
    btnOpen.Top := Self.ClientHeight - btnOpen.Height - 8 ;
    PlaceControl( BiDiMode, nil, btnOpen, 8, 75 ) ;
    btnOpen.Anchors := lb ;
    btnOpen.Caption := _rsrc( GIS_RS_LS_BTN_OPEN ) ;
    btnOpen.TabOrder := 0 ;
    btnOpen.OnClick := doOpenClick ;

    btnSave := TButton.Create( Self ) ;
    btnSave.Parent := Self ;
    btnSave.Height := 25 ;
    btnSave.Top := Self.ClientHeight - btnSave.Height - 8 ;
    PlaceControl( BiDiMode, btnOpen, btnSave, 8, 75 ) ;
    btnSave.Anchors := lb ;
    btnSave.Caption := _rsrc( GIS_RS_LS_BTN_SAVE_AS ) ;
    btnSave.TabOrder := 1 ;
    btnSave.OnClick := doSaveClick ;

    btnHelp.Visible := assigned( pOnHelp ) ;
    PlaceControl( BiDiMode, btnSave, btnHelp, 8, 75 ) ;
    btnHelp.TabOrder := 2 ;

    btnOK.Top := Self.ClientHeight - btnOK.Height - 8 ;
    PlaceControl( BiDiMode, btnCancel, btnOK, -8, 75 ) ;
    btnOK.Enabled := False ;
    btnOK.TabOrder := 3 ;

    btnCancel.Top := Self.ClientHeight - btnCancel.Height - 8 ;
    PlaceControl( BiDiMode, nil, btnCancel, -8, 75 ) ;
    btnCancel.TabOrder := 4 ;

    bvlMain := TBevel.Create( Self ) ;
    bvlMain.Parent := Self ;
    bvlMain.Shape := TBevelShape.bsTopLine ;
    bvlMain.Top := btnCancel.Top - 8 ;
    bvlMain.Width := Self.ClientWidth ;
    bvlMain.Height := 4 ;
    bvlMain.Anchors := lrb ;

    pnlMain := TPanel.Create( Self ) ;
    pnlMain.Parent := Self ;
    pnlMain.Left := 0 ;
    pnlMain.Top := 0 ;
    pnlMain.Width := Self.ClientWidth ;
    pnlMain.Height := Self.ClientHeight - btnCancel.Height - 16 ;
    pnlMain.BorderStyle := bsNone ;
    pnlMain.BevelOuter := TBevelCut.bvNone ;
    pnlMain.Anchors := ltrb ;
    pnlMain.TabOrder := 5 ;

    pnlLeft := TPanel.Create( pnlMain ) ;
    pnlLeft.Parent := pnlMain ;
    pnlLeft.Top := 0 ;
    PlaceControl( BiDiMode, nil, pnlLeft, 0, 112 ) ;
    if BiDiMode = bdRightToLeft then
      pnlLeft.Align := TAlign.alRight
    else
      pnlLeft.Align := TAlign.alLeft ;
    pnlLeft.BorderStyle := bsNone ;
    pnlLeft.BevelOuter := TBevelCut.bvNone ;
    pnlLeft.TabOrder := 0 ;

    vspltrMain := TSplitter.Create( pnlMain ) ;
    vspltrMain.Parent := pnlMain ;
    PlaceControl( BiDiMode, pnlLeft, vspltrMain, 8, vspltrMain.Width ) ;
    vspltrMain.Align := TAlign.alLeft ;
    vspltrMain.OnMoved := doSplitterMoved ;

    pnlRight := TPanel.Create( pnlMain ) ;
    pnlRight.Parent := pnlMain ;
    pnlRight.Top := 0 ;
    PlaceControl( BiDiMode, pnlLeft, pnlRight, 122, 132 ) ;
    pnlRight.Align := TAlign.alClient ;
    pnlRight.BorderStyle := bsNone ;
    pnlRight.BevelOuter := TBevelCut.bvNone ;
    pnlRight.TabOrder := 1 ;

    t := 0 ;

    lblCommands := TLabel.Create( pnlLeft ) ;
    lblCommands.Parent := pnlLeft ;
    lblCommands.AutoSize := False ;
    lblCommands.Top := t ;
    lblCommands.Height := 20 ;
    PlaceControl( BiDiMode, nil, lblCommands, 0, pnlLeft.Width ) ;
    lblCommands.Layout := TTextLayout.tlCenter ;
    lblCommands.Caption := '  ' + _rsrc( GIS_RS_LS_LBL_COMMANDS ) ;

    t := lblCommands.Top + lblCommands.Height ;

    btnComGOTO := TButton.Create( pnlLeft ) ;
    btnComGOTO.Parent := pnlLeft ;
    btnComGOTO.Top := t ;
    btnComGOTO.Height := 25 ;
    PlaceControl( BiDiMode, nil, btnComGOTO, 4, pnlLeft.Width - 8 ) ;
    btnComGOTO.Anchors := ltr ;
    btnComGOTO.Font.Name := GIS_LS_FONT_FAMILY ;
    btnComGOTO.Font.Size := btnComGOTO.Font.Size  ;
    btnComGOTO.Caption := GIS_LS_CMD_GOTO + '      ' ;
    btnComGOTO.OnClick := doGOTOClick ;
    btnComGOTO.ShowHint := True ;
    btnComGOTO.Hint := GIS_RS_LS_CMD_GOTO_HINT ;
    btnComGOTO.TabOrder := 0 ;

    t := btnComGOTO.Top + btnComGOTO.Height ;

    btnComMOVE := TButton.Create( pnlLeft ) ;
    btnComMOVE.Parent := pnlLeft ;
    btnComMOVE.Top := t ;
    btnComMOVE.Height := 25 ;
    PlaceControl( BiDiMode, nil, btnComMOVE, 4, pnlLeft.Width - 8 ) ;
    btnComMOVE.Anchors := ltr ;
    btnComMOVE.Font.Name := GIS_LS_FONT_FAMILY ;
    btnComMOVE.Font.Size := btnComMOVE.Font.Size ;
    btnComMOVE.Caption := GIS_LS_CMD_MOVE + '      ' ;
    btnComMOVE.OnClick := doMOVEClick ;
    btnComMOVE.ShowHint := True ;
    btnComMOVE.Hint := GIS_RS_LS_CMD_MOVE_HINT ;
    btnComMOVE.TabOrder := 1 ;

    t := btnComMOVE.Top + btnComMOVE.Height ;

    btnComFOREND := TButton.Create( pnlLeft ) ;
    btnComFOREND.Parent := pnlLeft ;
    btnComFOREND.Top := t ;
    btnComFOREND.Height := 25 ;
    PlaceControl( BiDiMode, nil, btnComFOREND, 4, pnlLeft.Width - 8 ) ;
    btnComFOREND.Anchors := ltr ;
    btnComFOREND.Font.Name := GIS_LS_FONT_FAMILY ;
    btnComFOREND.Font.Size := btnComFOREND.Font.Size ;
    btnComFOREND.Caption := GIS_LS_CMD_FOR + '-' + GIS_LS_CMD_END + '   ' ;
    btnComFOREND.OnClick := doFORENDClick ;
    btnComFOREND.ShowHint := True ;
    btnComFOREND.Hint := GIS_RS_LS_CMD_FOR_HINT + #10#13 + GIS_RS_LS_CMD_END_HINT;
    btnComFOREND.TabOrder := 2 ;

    t := btnComFOREND.Top + btnComFOREND.Height ;

    btnComDRAW := TButton.Create( pnlLeft ) ;
    btnComDRAW.Parent := pnlLeft ;
    btnComDRAW.Top := t ;
    btnComDRAW.Height := 25 ;
    PlaceControl( BiDiMode, nil, btnComDRAW, 4, pnlLeft.Width - 8 ) ;
    btnComDRAW.Anchors := ltr ;
    btnComDRAW.Font.Name := GIS_LS_FONT_FAMILY ;
    btnComDRAW.Font.Size := btnComDRAW.Font.Size ;
    btnComDRAW.Caption := GIS_LS_CMD_DRAW + '      ' ;
    btnComDRAW.OnClick := doDRAWClick ;
    btnComDRAW.ShowHint := True ;
    btnComDRAW.Hint := GIS_RS_LS_CMD_DRAW_HINT ;
    btnComDRAW.TabOrder := 3 ;

    t := btnComDRAW.Top + btnComDRAW.Height ;

    btnComLINE := TButton.Create( pnlLeft ) ;
    btnComLINE.Parent := pnlLeft ;
    btnComLINE.Top := t ;
    btnComLINE.Height := 25 ;
    PlaceControl( BiDiMode, nil, btnComLINE, 4, pnlLeft.Width - 8 ) ;
    btnComLINE.Anchors := ltr ;
    btnComLINE.Font.Name := GIS_LS_FONT_FAMILY ;
    btnComLINE.Font.Size := btnComLINE.Font.Size ;
    btnComLINE.Caption := GIS_LS_CMD_LINE + '      ' ;
    btnComLINE.OnClick := doLINEClick ;
    btnComLINE.ShowHint := True ;
    btnComLINE.Hint := GIS_RS_LS_CMD_LINE_HINT ;
    btnComLINE.TabOrder := 4 ;

    t := btnComLINE.Top + btnComLINE.Height ;

    btnComOUTLINE := TButton.Create( pnlLeft ) ;
    btnComOUTLINE.Parent := pnlLeft ;
    btnComOUTLINE.Top := t ;
    btnComOUTLINE.Height := 25 ;
    PlaceControl( BiDiMode, nil, btnComOUTLINE, 4, pnlLeft.Width - 8 ) ;
    btnComOUTLINE.Anchors := ltr ;
    btnComOUTLINE.Font.Name := GIS_LS_FONT_FAMILY ;
    btnComOUTLINE.Font.Size := btnComOUTLINE.Font.Size ;
    btnComOUTLINE.Caption := GIS_LS_CMD_OUTLINE + '   ' ;
    btnComOUTLINE.OnClick := doOUTLINEClick ;
    btnComOUTLINE.ShowHint := True ;
    btnComOUTLINE.Hint := GIS_RS_LS_CMD_OUTLINE_HINT ;
    btnComOUTLINE.TabOrder := 5 ;

    t := btnComOUTLINE.Top + btnComOUTLINE.Height ;

    btnComFILL := TButton.Create( pnlLeft ) ;
    btnComFILL.Parent := pnlLeft ;
    btnComFILL.Top := t ;
    btnComFILL.Height := 25 ;
    PlaceControl( BiDiMode, nil, btnComFILL, 4, pnlLeft.Width - 8 ) ;
    btnComFILL.Anchors := ltr ;
    btnComFILL.Font.Name := GIS_LS_FONT_FAMILY ;
    btnComFILL.Font.Size := btnComFILL.Font.Size ;
    btnComFILL.Caption := GIS_LS_CMD_FILL + '      ' ;
    btnComFILL.OnClick := doFILLClick ;
    btnComFILL.ShowHint := True ;
    btnComFILL.Hint := GIS_RS_LS_CMD_FILL_HINT ;
    btnComFILL.TabOrder := 6 ;

    t := btnComFILL.Top + btnComFILL.Height ;

    btnComWIDTH := TButton.Create( pnlLeft ) ;
    btnComWIDTH.Parent := pnlLeft ;
    btnComWIDTH.Top := t ;
    btnComWIDTH.Height := 25 ;
    PlaceControl( BiDiMode, nil, btnComWIDTH, 4, pnlLeft.Width - 8 ) ;
    btnComWIDTH.Anchors := ltr ;
    btnComWIDTH.Font.Name := GIS_LS_FONT_FAMILY ;
    btnComWIDTH.Font.Size := btnComWIDTH.Font.Size ;
    btnComWIDTH.Caption := GIS_LS_CMD_WIDTH + '     ' ;
    btnComWIDTH.OnClick := doWIDTHClick ;
    btnComWIDTH.ShowHint := True ;
    btnComWIDTH.Hint := GIS_RS_LS_CMD_WIDTH_HINT ;
    btnComWIDTH.TabOrder := 7 ;

    t := btnComWIDTH.Top + btnComWIDTH.Height ;

    btnComCOLOR := TButton.Create( pnlLeft ) ;
    btnComCOLOR.Parent := pnlLeft ;
    btnComCOLOR.Top := t ;
    btnComCOLOR.Height := 25 ;
    PlaceControl( BiDiMode, nil, btnComCOLOR, 4, pnlLeft.Width - 8 ) ;
    btnComCOLOR.Anchors := ltr ;
    btnComCOLOR.Font.Name := GIS_LS_FONT_FAMILY ;
    btnComCOLOR.Font.Size := btnComCOLOR.Font.Size ;
    btnComCOLOR.Caption := GIS_LS_CMD_COLOR + '     ' ;
    btnComCOLOR.OnClick := doCOLORClick ;
    btnComCOLOR.ShowHint := True ;
    btnComCOLOR.Hint := GIS_RS_LS_CMD_COLOR_HINT ;
    btnComCOLOR.TabOrder := 8 ;

    t := btnComCOLOR.Top + btnComCOLOR.Height ;

    lblUnits := TLabel.Create( pnlLeft ) ;
    lblUnits.Parent := pnlLeft ;
    lblUnits.Top := t ;
    lblUnits.Height := 20 ;
    PlaceControl( BiDiMode, nil, lblUnits, 0, pnlLeft.Width ) ;
    lblUnits.AutoSize := False ;
    lblUnits.Layout := TTextLayout.tlCenter ;
    lblUnits.Anchors := ltr ;
    lblUnits.Caption := '  ' + _rsrc( GIS_RS_LS_LBL_UNITSYMBOLS ) ;

    t := lblUnits.Top + lblUnits.Height ;

    lblUnitsList := TLabel.Create( pnlLeft ) ;
    lblUnitsList.Parent := pnlLeft ;
    lblUnitsList.Top := t ;
    lblUnitsList.Height := 17 ;
    PlaceControl( BiDiMode, nil, lblUnitsList, 0, pnlLeft.Width ) ;
    lblUnitsList.AutoSize := False ;
    lblUnitsList.Anchors := ltr ;
    lblUnitsList.Font.Name := GIS_LS_FONT_FAMILY ;
    lblUnitsList.Font.Size := lblUnitsList.Font.Size ;
    lblUnitsList.Alignment := TAlignment.taCenter ;
    lblUnitsList.Caption := GIS_LS_UNITS ;
    lblUnitsList.ShowHint := True ;
    lblUnitsList.Hint := GIS_RS_LS_UNITS_HINT ;

    t := lblUnitsList.Top + lblUnitsList.Height ;

    lblPreview := TLabel.Create( pnlRight ) ;
    lblPreview.Parent := pnlRight ;
    lblPreview.AutoSize := False ;
    lblPreview.Align := TAlign.alTop ;
    lblPreview.Height := 20 ;
    lblPreview.Layout := TTextLayout.tlCenter ;
    lblPreview.Caption := _rsrc( GIS_RS_LS_LBL_PREVIEW ) ;

    gisPreview := TGIS_ViewerWnd.Create( pnlRight ) ;
    gisPreview.Parent := pnlRight ;
    gisPreview.Top := lblPreview.Top + lblPreview.Height + 8 ;
    gisPreview.Height := 192 ;
    gisPreview.Align := TAlign.alTop ;
    gisPreview.Mode := TGIS_ViewerMode.UserDefined ;
    gisPreview.Renderer := TGIS_RendererVclGdiPlus.Create ;

    hspltrRight := TSplitter.Create( pnlRight ) ;
    hspltrRight.Parent := pnlRight ;
    hspltrRight.Top := gisPreview.Top + gisPreview.Height + 8 ;
    hspltrRight.Align := TAlign.alTop ;
    hspltrRight.OnMoved := doSplitterMoved ;

    // Wizard begin

    lblWizard := TLabel.Create( pnlRight ) ;
    lblWizard.Parent := pnlRight ;
    lblWizard.AutoSize := False ;
    lblWizard.Top := gisPreview.Top + gisPreview.Height + 16 ;
    lblWizard.Align := TAlign.alTop ;
    lblWizard.Height := 20 ;
    lblWizard.Layout := TTextLayout.tlCenter ;
    lblWizard.Caption := _rsrc( GIS_RS_LS_LBL_WIZARD ) ;

    pnlWizard := TPanel.Create( pnlRight ) ;
    pnlWizard.Parent := pnlRight ;
    pnlWizard.Top := lblWizard.Top + lblWizard.Height + 16 ;
    pnlWizard.Align := TAlign.alTop ;
    pnlWizard.BevelOuter := TBevelCut.bvNone ;
    pnlWizard.TabOrder := 0 ;

    btnWizard := TButton.Create( pnlWizard ) ;
    btnWizard.Parent := pnlWizard ;
    btnWizard.Top := 0 ;
    btnWizard.Height := 25 ;
    PlaceControl( BiDiMode, nil, btnWizard, -8, 75 ) ;
    btnWizard.Anchors := tr ;
    btnWizard.Caption := _rsrc( GIS_RS_LS_LBL_WIZARD_APPLY ) ;
    btnWizard.OnClick := doWizardClick ;

    cmbbxWizStart := T_LineSymbolComboBox.Create( pnlWizard ) ;
    cmbbxWizStart.Parent := pnlWizard ;
    cmbbxWizStart.Top := 2 ;
    cmbbxWizStart.Height := 25 ;
    if BiDiMode = bdRightToLeft then
      PlaceControl( BiDiMode, btnWizard, cmbbxWizStart, -4, 64 )
    else
      PlaceControl( BiDiMode, nil, cmbbxWizStart, 0, 64 ) ;
    T_LineSymbolComboBox( cmbbxWizStart ).SymbolElement :=
      T_LineSymbolElement.SymbolStart ;
    T_LineSymbolComboBox( cmbbxWizStart ).UpdateList ;

    cmbbxWizEnd := T_LineSymbolComboBox.Create( pnlWizard ) ;
    cmbbxWizEnd.Parent := pnlWizard ;
    cmbbxWizEnd.Top := 2 ;
    cmbbxWizEnd.Height := 25 ;
    if BiDiMode = bdRightToLeft then
      PlaceControl( BiDiMode, nil, cmbbxWizEnd, 0, 64 )
    else
      PlaceControl( BiDiMode, btnWizard, cmbbxWizEnd, -4, 64 ) ;
    cmbbxWizEnd.Anchors := [ TAnchorKind.akTop, TAnchorKind.akRight ] ;
    T_LineSymbolComboBox( cmbbxWizEnd ).SymbolElement :=
      T_LineSymbolElement.SymbolEnd ;
    T_LineSymbolComboBox( cmbbxWizEnd ).UpdateList ;

    cmbbxWizLine := T_LineSymbolComboBox.Create( pnlWizard ) ;
    cmbbxWizLine.Parent := pnlWizard ;
    cmbbxWizLine.Top := 2 ;
    cmbbxWizLine.Height := 25 ;
    cmbbxWizLine.Left := cmbbxWizStart.Left + cmbbxWizStart.Width + 4 ;
    cmbbxWizLine.Width := cmbbxWizEnd.Left - cmbbxWizLine.Left - 4 ;
    cmbbxWizLine.Anchors := ltr ;
    T_LineSymbolComboBox( cmbbxWizLine ).SymbolElement :=
      T_LineSymbolElement.SymbolLine ;
    T_LineSymbolComboBox( cmbbxWizLine ).UpdateList ;

    cmbbxWizStart.TabOrder := 0 ;
    cmbbxWizLine.TabOrder := 1 ;
    cmbbxWizEnd.TabOrder := 2 ;
    btnWizard.TabOrder := 3 ;

    pnlWizard.Height := btnWizard.Top + btnWizard.Height ;

    Self.OnResize := doResize ;

    // Wizard end

    lblCode := TLabel.Create( pnlRight ) ;
    lblCode.Parent := pnlRight ;
    lblCode.AutoSize := False ;
    lblCode.Top := pnlWizard.Top + pnlWizard.Height + 8 ;
    lblCode.Height := 20 ;
    PlaceControl( BiDiMode, nil, lblCode, 0, 65 ) ;
    lblCode.Align := TAlign.alTop ;
    lblCode.Layout := TTextLayout.tlCenter ;
    lblCode.Caption := _rsrc( GIS_RS_LS_LBL_CODE ) ; ;

    mmCode := TMemo.Create( pnlRight ) ;
    mmCode.Parent := pnlRight ;
    mmCode.Top := lblCode.Top + lblCode.Height + 8 ;
    mmCode.BorderStyle := TFormBorderStyle.bsNone ;
    mmCode.Font.Name := GIS_LS_FONT_FAMILY ;
    mmCode.Font.Size := mmCode.Font.Size ;
    mmCode.Align := TAlign.alClient ;
    mmCode.TabOrder := 1 ;
    mmCode.WordWrap := False ;
    mmCode.ScrollBars := TScrollStyle.ssBoth ;
    mmCode.Text := GIS_LS_DEF_SYMBOL ;
    mmCode.OnChange := doCodeChange ;
    mmCode.OnKeyPress := doCodeKeyPress ;

    lblError := TLabel.Create( pnlRight ) ;
    lblError.Parent := pnlRight ;
    lblError.AutoSize := False ;
    lblError.Align := TAlign.alBottom ;
    lblError.Height := 20 ;
    lblError.Layout := TTextLayout.tlCenter ;
    lblError.Caption := '' ;
    lblError.OnClick := doErrorClick ;
    lblError.OnMouseEnter := doErrorMouseEnter ;
    lblError.OnMouseLeave := doErrorMouseLeave ;
  end ;

  procedure TGIS_LineSymbolEditor.createLayers ;
  const
    LOCAL_CURVE_STEPS : Integer = 32 ;
  var
    shp : TGIS_Shape ;
    tmp : TGIS_Shape ;
    i   : Integer ;
  begin
    skeleton := TGIS_LayerVector.Create ;
    skeleton.Name := 'SKELETON' ;
    skeleton.Open ;

    layer := TGIS_LayerVector.Create ;
    layer.Name := 'LAYER' ;
    layer.Open ;

    for i := 0 to 2 do begin

      shp := TGIS_ShapeArc.Create( TGIS_DimensionType.XY ) ;
      try
        shp.AddPart ;

        shp.AddPoint( GisPoint( -6.0,  0.0 - 1.6*i ) ) ;
        shp.AddPoint( GisPoint( -4.0,  0.0 - 1.6*i ) ) ;
        shp.AddPoint( GisPoint( -3.0,  0.5 - 1.6*i ) ) ;
        shp.AddPoint( GisPoint( -1.0, -0.5 - 1.6*i ) ) ;
        shp.AddPoint( GisPoint(  1.0,  0.5 - 1.6*i ) ) ;
        shp.AddPoint( GisPoint(  3.0, -0.5 - 1.6*i ) ) ;
        shp.AddPoint( GisPoint(  4.0,  0.0 - 1.6*i ) ) ;
        shp.AddPoint( GisPoint(  6.0,  0.0 - 1.6*i ) ) ;

        tmp := skeleton.AddShape( shp ) ;
        if i <> 0 then
          skeleton.ParamsList.Add ;
        skeleton.Params.Query := '( GIS_UID = ' + IntToStr( tmp.Uid ) + ' )' ;
        skeleton.Params.Line.Color := TGIS_Color.LightGray ;
        //skeleton.Params.Line.Width := -1-i ;
        skeleton.Params.Line.WidthAsText := Format( 'SIZE:%ddip', [1+i] ) ;

        tmp := layer.AddShape( shp ) ;
        if i <> 0 then
          layer.ParamsList.Add ;
        layer.Params.Query := '( GIS_UID = ' + IntToStr( tmp.Uid ) + ' )' ;
        layer.Params.Line.Symbol :=  SymbolList.Prepare( '&' + GIS_LS_DEF_SYMBOL ) ;
        //layer.Params.Line.Width := -1-i ;
        layer.Params.Line.WidthAsText := Format( 'SIZE:%ddip', [1+i] ) ;

      finally
        FreeObject( shp ) ;
      end ;

    end ;

    gisPreview.Add( skeleton ) ;
    gisPreview.Add( layer ) ;
    gisPreview.RecalcExtent ;
    gisPreview.FullExtent ;
  end ;


  procedure TGIS_LineSymbolEditor.updatePreview ;
  var
    prsr  : TGIS_LineSymbolParser ;
    estr  : String ;
    sb    : TStringBuilder ;
    str   : String ;
    i     : Integer ;
    k     : Integer ;
    len   : Integer ;
  begin
    estr := '' ;
    is_err := False ;

    prsr := TGIS_LineSymbolParser.Create ;
    try

      for i := 0 to mmCode.Lines.Count - 1 do begin
        str := mmCode.Lines[i] ;
        if not prsr.Parse( str, err_s, err_l ) then begin
          len := -1+i ;
          for k := 0 to i - 1 do
            Inc( len, Length( mmCode.Lines[k] )+1 ) ;

          estr := Copy( str, err_s, err_l ) ;
          estr := Format( _rsrc( GIS_RS_LS_ERR_GENERIC ), [estr] ) ;
          err_s := err_s + len ;

          is_err := True ;
          exit ;
        end ;
      end ;

      if not prsr.IsClosed then begin
        tmrUpdate.Enabled := False ;
        i := mmCode.SelStart ;
        k := mmCode.SelLength ;
        mmCode.Lines[mmCode.Lines.Count-1] :=
          mmCode.Lines[mmCode.Lines.Count-1] + ')' ;
        mmCode.SelStart := i ;
        mmCode.SelLength := k ;
        exit ;
      end ;

    finally
      if is_err then
        lblError.Font.Color := clRed
      else
        lblError.Font.Color := clWindowText ;
      lblError.Caption := estr ;
      btnOK.Enabled := not ( is_err or ( Length( Trim( mmCode.Text ) ) = 0 ) ) ;
      FreeObject( prsr ) ;
    end ;

    sb := TStringBuilder.Create ;
    try
      for i := 0 to mmCode.Lines.Count - 1 do
        sb.Append( Trim( mmCode.Lines[i] ) ) ;
      str := sb.ToString ;
    finally
      FreeObject( sb ) ;
    end ;

    previous := TGIS_SymbolLineEx( layer.Params.Line.Symbol ).Name ;
    try
      for i := 0 to 2 do
        TGIS_ParamsSectionVector( layer.ParamsList.Items[i] ).Line.Symbol :=
          SymbolList.Prepare( '&' + str ) ;
    except
      for i := 0 to 2 do
        TGIS_ParamsSectionVector( layer.ParamsList.Items[i] ).Line.Symbol :=
          SymbolList.Prepare( '&' + str ) ;
    end ;

    gisPreview.FullExtent ;
  end ;


  procedure TGIS_LineSymbolEditor.comClick(
    const _com : String ;
    const _len : Integer
  ) ;
  var
    i : Integer ;
    k : Integer ;
  begin
    i := mmCode.SelStart ;

    mmCode.SelText := _com ;
    k := _len ;

    Self.FocusControl( mmCode ) ;
    mmCode.SelStart := i + k ;
  end ;


  function TGIS_LineSymbolEditor.Execute(
    const _path   : String ;
    const _onhelp : TGIS_HelpEvent
  ) : Integer ;
  begin
    pOnHelp := _onhelp ;
    btnHelp.Visible := assigned( pOnHelp ) ;

    Symbol := _path ;
    Result := ShowModal ;
  end ;

  function TGIS_LineSymbolEditor.Execute(
    const _path : String
  ) : Integer ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Result := Execute( _path, hlp ) ;
  end;

//==================================== END =====================================
end.

