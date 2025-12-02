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

unit FMX.GisLineSymbolEditor ;
{$HPPEMIT '#pragma link "FMX.GisLineSymbolEditor"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.SysUtils,
  System.UITypes,
  System.Classes,
  FMX.Types,
  FMX.Forms,
  FMX.Layouts,
  FMX.Memo,
  FMX.StdCtrls,
  FMX.ListBox,

  GisTypes,
  GisTypesUI,
  GisSymbol,
  GisLayerVector,
  FMX.GisViewerWnd,
  FMX.GisModalForm,
  FMX.GisComboBox ;


type

  /// <summary>
  ///   A form which allows to edit a symbol defining string and preview
  ///   the result.
  /// </summary>
  TGIS_LineSymbolEditor = class( TGIS_ModalForm )
    private
      pnlMain       : TLayout ;
      pnlLeft       : TLayout ;
      vspltrMain    : TSplitter ;
      pnlRight      : TLayout ;
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
      lblPreview    : TLabel ;
      lblUnits      : TLabel ;
      lblUnitsList  : TLabel ;
      gisPreview    : TGIS_ViewerWnd ;
      hspltrRight   : TSplitter ;
      lblWizard     : TLabel ;
      pnlWizard     : TLayout ;
      cmbbxWizStart : TGIS_ComboBox ;
      cmbbxWizLine  : TGIS_ComboBox ;
      cmbbxWizEnd   : TGIS_ComboBox ;
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
      procedure doGOTOClick          (  _sender : TObject
                                     ) ;
      procedure doMOVEClick          (  _sender : TObject
                                     ) ;
      procedure doFORENDClick        (  _sender : TObject
                                     ) ;
      procedure doDRAWClick          (  _sender : TObject
                                     ) ;
      procedure doLINEClick          (  _sender : TObject
                                     ) ;
      procedure doOUTLINEClick       (  _sender : TObject
                                     ) ;
      procedure doFILLClick          (  _sender : TObject
                                     ) ;
      procedure doWIDTHClick         (  _sender : TObject
                                     ) ;
      procedure doCOLORClick         (  _sender : TObject
                                     ) ;
      procedure doWizardClick        (  _sender : TObject
                                     ) ;
      procedure doCodeKeyDown        (   _sender : TObject ;
                                       var  _key : Word ;
                                       var _char : Char ;
                                          _shift : TShiftState
                                     ) ;
      procedure doCodeChangeTracking (  _sender : TObject
                                     ) ;
      procedure doUpdateTimer        (  _sender : TObject
                                     ) ;
      procedure doErrorClick         (  _sender : TObject
                                     ) ;
      procedure doErrorMouseEnter    (  _sender : TObject
                                     ) ;
      procedure doErrorMouseLeave    (  _sender : TObject
                                     ) ;
      procedure doResize             (  _sender : TObject
                                     ) ;
      procedure doResizeTimer        (  _sender : TObject
                                     ) ;
      procedure doOpenClick          (  _sender : TObject
                                     ) ;
      procedure doSaveClick          (  _sender : TObject
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
      procedure initForm     ; override;

      /// <inheritdoc/>
      procedure initControls ; override;

    public // destructors
      /// <inheritdoc/>
      destructor Destroy ; override;
    public
      /// <summary>
      ///   Shows the form.
      /// </summary>
      /// <param name="_onhelp">
      ///   help notification function; if assigned the help button
      ///   will be visible and help support will be enabled;
      /// </param>
      /// <param name="_proc">
      ///   to be executed after the form was closed
      /// </param>
      procedure Execute     ( const _onhelp : TGIS_HelpEvent ;
                              const _proc  : TProc<TModalResult>
                            ) ; overload;

      /// <summary>
      ///   Shows the form.
      /// </summary>
      /// <param name="_proc">
      ///   to be executed after the form was closed
      /// </param>
      procedure Execute     ( const _proc  : TProc<TModalResult>
                            ) ; overload;

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

  FMX.Objects,
  FMX.Graphics,
  FMX.Dialogs,
  {$IFDEF LEVEL_XE8_FMX}
    FMX.Memo.Types,
  {$ENDIF}

  GisResource,
  GisRtl,
  GisFunctions,
  GisParams,
  GisLineSymbolEditorUtils,
  FMX.GisViewerBmp,
  FMX.GisControlHelper,
  FMX.GisComboBoxHelper ;

const
  LOCAL_DLG_FILTER    : String = 'Symbol file (*.sym)|*.sym' ;
  LOCAL_DLG_DEFEXT    : String = 'sym' ;

type
  T_LineSymbolElement = ( SymbolStart, SymbolLine, SymbolEnd ) ;

  T_LineSymbolComboBox = class( TGIS_ComboBox )
    private
      helper   : TGIS_ComboBoxHelper ;
      symList  : TGIS_PredefinedLineSymbolList ;
      viewer   : TGIS_ViewerBmp ;
      layer    : TGIS_LayerVector ;
    private
      FElement : T_LineSymbolElement ;
    private
      procedure prepareLayer ;
    private
      function  doValue  ( _sender    : TObject      ;
                           _value     : String
                         ) : String ;
      procedure doRender ( _item      : TListBoxItem ;
                           _canvas    : TCanvas      ;
                           _rect      : TRectF       ;
                           _font      : TFont        ;
                           _color     : TAlphaColor  ;
                           _class     : Char         ;
                           _caption   : String       ;
                           _value     : String
                         ) ;
    public
      constructor Create ( _owner : TComponent
                         ) ;
    public
      destructor Destroy ; override;
    protected
      /// <inheritdoc/>
      function GetDefaultStyleLookupName : String ; override;
    public
      procedure UpdateList ;
      function  GetSymbol  ( const _start  : Integer ;
                             const _line   : Integer ;
                             const _end    : Integer
                           ) : String ; overload;
    public
      property  SymbolElement : T_LineSymbolElement
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
    helper.ValueEvent := doValue ;
    helper.RenderEvent := doRender ;

    symList := TGIS_PredefinedLineSymbolList.Create ;
    viewer := TGIS_ViewerBmp.Create ;
    viewer.Color := TGIS_Color.None ;

    FElement := T_LineSymbolElement.SymbolLine ;

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
    _item    : TListBoxItem ;
    _canvas  : TCanvas      ;
    _rect    : TRectF       ;
    _font    : TFont        ;
    _color   : TAlphaColor  ;
    _class   : Char         ;
    _caption : String       ;
    _value   : String
  ) ;
  var
    sym : String ;
    ext : TGIS_Extent ;
    rct : TRectF ;
    i   : Integer ;

    procedure prep_bitmap ;
    begin
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
        T_LineSymbolElement.SymbolLine :
          begin
          if i = 0 then
            sym := '&'
          else
            sym := '&' + symList.PrepareSymbol( 0, i, 0 ) ;

            layer.Params.Line.Symbol := SymbolList.Prepare( sym ) ;
            ext := GisExtent( -5.5, -0.001,  5.5,  0.001 ) ;
          end ;
        T_LineSymbolElement.SymbolEnd :
          begin
            if i = 0 then
              sym := '&'
            else
              sym := '&' + symList.PrepareSymbol( 0, 1, i ) ;

            layer.Params.Line.Symbol := SymbolList.Prepare( sym ) ;
            ext := GisExtent(  2.5, -0.001,  8.0,  0.001 ) ;
          end ;
      end ;
      viewer.SetSize( RoundS( _rect.Width ), RoundS( _rect.Height ) ) ;
      viewer.VisibleExtent := ext ;
      viewer.Draw ;
    end ;

  begin
    i := StrToInt( _value ) ;

    prep_bitmap ;

    _canvas.DrawBitmap(
       TBitmap( viewer.Bitmap ),
       RectF(0,0,TBitmap( viewer.Bitmap ).Width, TBitmap( viewer.Bitmap ).Height),
       _rect, _item.AbsoluteOpacity
    ) ;
  end ;

  function T_LineSymbolComboBox.GetDefaultStyleLookupName : string ;
  begin
    Result := 'comboboxstyle' ;
  end ;

  procedure T_LineSymbolComboBox.UpdateList ;
  var
    idx : Integer ;
    cnt : Integer ;
    i   : Integer ;
  begin
    idx := ItemIndex ;
    if idx < 0 then
      idx := 0 ;

    case SymbolElement of
      T_LineSymbolElement.SymbolStart : cnt := symList.StartCount ;
      T_LineSymbolElement.SymbolLine : cnt := symList.LineCount ;
      T_LineSymbolElement.SymbolEnd : cnt := symList.EndCount ;
    end ;

    Clear ;
    for i := 0 to cnt - 1 do begin
      helper.SetItem(
        TGIS_ComboBoxHelper.PrepareItem(
          False,
          'C',
          TGIS_ComboBoxHelperPosition.Bottom,
          IntToStr( Items.Count ),
          IntToStr( Items.Count )
        )
      ) ;
    end ;

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

  procedure TGIS_LineSymbolEditor.initForm ;
  begin
    Self.Caption := _rsrcna( GIS_RS_LS_FRM_CAPTION ) ;
    Self.ClientWidth := 640 ;
    Self.ClientHeight := 480 ;
    Self.Name := 'TGIS_LineSymbolEditor' ;
  end;

  procedure TGIS_LineSymbolEditor.initControls ;
  begin
    {$IFNDEF GIS_MOBILE_DIALOGS}
      ResizeHandle ;
    {$ENDIF}
    createLayout ;
    createLayers ;
    is_err := False ;
  end ;

  destructor TGIS_LineSymbolEditor.Destroy ;
  begin
    tmrUpdate.Enabled := False ;
    tmrResize.Enabled := False ;

    inherited ;
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


  procedure TGIS_LineSymbolEditor.doCodeKeyDown(
      _sender : TObject ;
    var  _key : Word ;
    var _char : Char ;
       _shift : TShiftState
  ) ;
  var
    c : Integer ;
  begin
    c := Ord( _char ) ;
    if ( c >= 97 ) and ( c <= 122 ) then
      _char := Char( c - 32 ) ;
  end ;


  procedure TGIS_LineSymbolEditor.doCodeChangeTracking(
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
    tmrUpdate.Enabled := False ;
    updatePreview ;
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
  var
    ss : TStyledSettings ;
  begin
    if not is_err then
      exit ;

    ss := lblError.StyledSettings ;
    Exclude( ss, TStyledSetting.Style ) ;
    lblError.StyledSettings := ss ;

    lblError.Cursor := crHandPoint ;
    lblError.Font.Style := [ TFontStyle.fsUnderline ] ;
  end ;


  procedure TGIS_LineSymbolEditor.doErrorMouseLeave(
    _sender : TObject
  ) ;
  var
    ss : TStyledSettings ;
  begin
    if not is_err then
      exit ;

    ss := lblError.StyledSettings ;
    Include( ss, TStyledSetting.Style ) ;
    lblError.StyledSettings := ss ;

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
  begin
    tmrResize.Enabled := False ;

    T_LineSymbolComboBox( cmbbxWizLine ).UpdateList ;

    gisPreview.FullExtent ;
  end ;


  procedure TGIS_LineSymbolEditor.doOpenClick(
    _sender : TObject
  ) ;
  var
    dlg : TOpenDialog ;
  begin
    dlg := TOpenDialog.Create( oMainForm ) ;
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
    dlg := TSaveDialog.Create( oMainForm ) ;
    try
      dlg.Filter := LOCAL_DLG_FILTER ;
      dlg.DefaultExt := LOCAL_DLG_DEFEXT ;
      if dlg.Execute then
        mmCode.Lines.SaveToFile( dlg.FileName ) ;
    finally
      FreeObject( dlg ) ;
    end ;
  end ;


  procedure TGIS_LineSymbolEditor.createLayout ;
  var
    pntf : TPointF ;
    ltrb : TAnchors ;
    ltr  : TAnchors ;
    tr   : TAnchors ;
    lb   : TAnchors ;
    rb   : TAnchors ;
    t    : Single ;
  begin
    ltrb := [ TAnchorKind.akLeft, TAnchorKind.akTop,
              TAnchorKind.akRight, TAnchorKind.akBottom ] ;
    ltr  := [ TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight ] ;
    tr   := [ TAnchorKind.akTop, TAnchorKind.akRight ] ;
    lb   := [ TAnchorKind.akLeft, TAnchorKind.akBottom ] ;
    rb   := [ TAnchorKind.akRight, TAnchorKind.akBottom ] ;

    tmrUpdate := TTimer.Create( Self ) ;
    tmrUpdate.Interval := 500 ;
    tmrUpdate.Enabled := False ;
    tmrUpdate.OnTimer := doUpdateTimer ;

    tmrResize := TTimer.Create( Self ) ;
    tmrResize.Interval := 500 ;
    tmrResize.Enabled := False ;
    tmrResize.OnTimer := doResizeTimer ;

    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnOpen := TButton.Create( oMainForm ) ;
      btnOpen.Parent := oMainForm ;
      btnOpen.Height := btnCancel.Height ;
      pntf := TPointF.Create( 8, Self.ClientHeight - btnOpen.Height - 8 ) ;
      btnOpen.Position.Point := pntf ;
      btnOpen.Anchors := lb ;
      btnOpen.Text := _rsrcna( GIS_RS_LS_BTN_OPEN ) ;
      btnOpen.OnClick := doOpenClick ;
      btnOpen.TabOrder := 0 ;

      btnSave := TButton.Create( oMainForm ) ;
      btnSave.Parent := oMainForm ;
      btnSave.Height := btnCancel.Height ;
      pntf := TPointF.Create(
        btnOpen.Position.Point.X + btnOpen.Width + 8,
        Self.ClientHeight - btnSave.Height - 8
      ) ;
      btnSave.Position.Point := pntf ;
      btnSave.Anchors := lb ;
      btnSave.Text := _rsrcna( GIS_RS_LS_BTN_SAVE_AS ) ;
      btnSave.TabOrder := 1 ;
      btnSave.OnClick := doSaveClick ;

      btnHelp.Visible := assigned( pOnHelp ) ;
      btnHelp.TabOrder := 2 ;
      pntf := TPointF.Create(
        btnSave.Position.Point.X + btnSave.Width + 8,
        Self.ClientHeight - btnHelp.Height - 8
      ) ;
      btnHelp.Position.Point := pntf ;

      pntf := TPointF.Create(
        Self.ClientWidth - btnCancel.Width - 8,
        Self.ClientHeight - btnCancel.Height - 8
      ) ;
      btnCancel.Position.Point := pntf ;
      btnCancel.Anchors := [TAnchorKind.akRight, TAnchorKind.akBottom ] ;
      btnCancel.TabOrder := 4 ;

      pntf := TPointF.Create(
        btnCancel.Position.Point.X - btnOK.Width - 8,
        Self.ClientHeight - btnOK.Height - 8
      ) ;
      btnOK.Position.Point := pntf ;
      btnOK.Anchors := [TAnchorKind.akRight, TAnchorKind.akBottom ] ;
      btnOK.TabOrder := 3 ;
      btnOK.Enabled := False ;
    {$ENDIF}

    pnlMain := TLayout.Create( oMainForm ) ;
    pnlMain.Parent := oMainForm ;
    pntf := TPointF.Create( 0, 0 ) ;
    pnlMain.Position.Point := pntf ;
    pnlMain.Width := Self.ClientWidth ;
    {$IFDEF GIS_MOBILE_DIALOGS}
      pnlMain.Height := Self.ClientHeight - 8 ;
    {$ELSE}
      pnlMain.Height := Self.ClientHeight - btnCancel.Height - 14 ;
    {$ENDIF}
    pnlMain.TabOrder := 5 ;

    pnlLeft := TLayout.Create( pnlMain ) ;
    pnlLeft.Parent := pnlMain ;
    pntf := TPointF.Create( 0, 0 ) ;
    pnlLeft.Position.Point := pntf ;
    pnlLeft.Width := 120 ;
    pnlLeft.Align := TAlignLayout.Left ;
    pnlLeft.TabOrder := 0 ;

    vspltrMain := TSplitter.Create( pnlMain ) ;
    vspltrMain.Parent := pnlMain ;
    pntf := TPointF.Create(
      pnlLeft.Position.Point.X + pnlLeft.Width + 8,
      vspltrMain.Position.Point.Y
    ) ;
    vspltrMain.Position.Point := pntf ;
    vspltrMain.Align := TAlignLayout.Left ;

    pnlRight := TLayout.Create( pnlMain ) ;
    pnlRight.Parent := pnlMain ;
    pntf := TPointF.Create ( 122, 0 ) ;
    pnlRight.Position.Point := pntf ;
    pnlRight.Align := TAlignLayout.Client ;
    pnlRight.TabOrder := 1 ;

    t := 0 ;

    lblCommands := TLabel.Create( pnlLeft ) ;
    lblCommands.Parent := pnlLeft ;
    lblCommands.AutoSize := False ;
    pntf := TPointF.Create( 0, t ) ;
    lblCommands.Position.Point := pntf ;
    lblCommands.Height := 20 ;
    lblCommands.Text := '  ' + _rsrc( GIS_RS_LS_LBL_COMMANDS ) ;
    lblCommands.FixSize ;

    t := lblCommands.Position.Point.Y + lblCommands.Height ;

    btnComGOTO := TButton.Create( pnlLeft ) ;
    btnComGOTO.Parent := pnlLeft ;
    btnComGOTO.StyledSettings := [ TStyledSetting.FontColor ] ;
    pntf := TPointF.Create( 4, t ) ;
    btnComGOTO.Position.Point := pntf ;
    btnComGOTO.Width := pnlLeft.Width - 8 ;
    btnComGOTO.Anchors := ltr ;
    btnComGOTO.Font.Family := GIS_LS_FONT_FAMILY ;
    btnComGOTO.Font.Size := 14 ;
    btnComGOTO.TextSettings.HorzAlign := TTextAlign.Leading ;
    btnComGOTO.Text := GIS_LS_INDENT + GIS_LS_CMD_GOTO ;
    btnComGOTO.OnClick := doGOTOClick ;
    {$IFDEF LEVEL_RX10_FMX}
      btnComGOTO.Hint := GIS_RS_LS_CMD_GOTO_HINT ;
    {$ENDIF}

    t := btnComGOTO.Position.Point.Y + btnComGOTO.Height + 2 ;

    btnComMOVE := TButton.Create( pnlLeft ) ;
    btnComMOVE.Parent := pnlLeft ;
    btnComMOVE.StyledSettings := [ TStyledSetting.FontColor ] ;
    pntf := TPointF.Create( 4, t ) ;
    btnComMOVE.Position.Point := pntf ;
    btnComMOVE.Width := pnlLeft.Width - 8 ;
    btnComMOVE.Anchors := ltr ;
    btnComMOVE.Font.Family := GIS_LS_FONT_FAMILY ;
    btnComMOVE.Font.Size := 14 ;
    btnComMOVE.TextSettings.HorzAlign := TTextAlign.Leading ;
    btnComMOVE.Text := GIS_LS_INDENT + GIS_LS_CMD_MOVE ;
    btnComMOVE.OnClick := doMOVEClick ;
    {$IFDEF LEVEL_RX10_FMX}
      btnComMOVE.Hint := GIS_RS_LS_CMD_MOVE_HINT ;
    {$ENDIF}

    t := btnComMOVE.Position.Point.Y + btnComMOVE.Height + 2 ;

    btnComFOREND := TButton.Create( pnlLeft ) ;
    btnComFOREND.Parent := pnlLeft ;
    btnComFOREND.StyledSettings := [ TStyledSetting.FontColor ] ;
    pntf := TPointF.Create( 4, t ) ;
    btnComFOREND.Position.Point := pntf ;
    btnComFOREND.Width := pnlLeft.Width - 8 ;
    btnComFOREND.Anchors := ltr ;
    btnComFOREND.Font.Family := GIS_LS_FONT_FAMILY ;
    btnComFOREND.Font.Size := 14 ;
    btnComFOREND.TextSettings.HorzAlign := TTextAlign.Leading ;
    btnComFOREND.Text := GIS_LS_INDENT + GIS_LS_CMD_FOR + '-' + GIS_LS_CMD_END ;
    btnComFOREND.OnClick := doFORENDClick ;
    {$IFDEF LEVEL_RX10_FMX}
      btnComFOREND.Hint := GIS_RS_LS_CMD_FOR_HINT + #10#13 + GIS_RS_LS_CMD_END_HINT;
    {$ENDIF}

    t := btnComFOREND.Position.Point.Y + btnComFOREND.Height + 2 ;

    btnComDRAW := TButton.Create( pnlLeft ) ;
    btnComDRAW.Parent := pnlLeft ;
    btnComDRAW.StyledSettings := [ TStyledSetting.FontColor ] ;
    pntf := TPointF.Create( 4, t ) ;
    btnComDRAW.Position.Point := pntf ;
    btnComDRAW.Width := pnlLeft.Width - 8 ;
    btnComDRAW.Anchors := ltr ;
    btnComDRAW.Font.Family := GIS_LS_FONT_FAMILY ;
    btnComDRAW.Font.Size := 14 ;
    btnComDRAW.TextSettings.HorzAlign := TTextAlign.Leading ;
    btnComDRAW.Text := GIS_LS_INDENT + GIS_LS_CMD_DRAW ;
    btnComDRAW.OnClick := doDRAWClick ;
    {$IFDEF LEVEL_RX10_FMX}
      btnComDRAW.Hint := GIS_RS_LS_CMD_DRAW_HINT ;
    {$ENDIF}

    t := btnComDRAW.Position.Point.Y + btnComDRAW.Height + 2 ;

    btnComLINE := TButton.Create( pnlLeft ) ;
    btnComLINE.Parent := pnlLeft ;
    btnComLINE.StyledSettings := [ TStyledSetting.FontColor ] ;
    pntf := TPointF.Create( 4, t ) ;
    btnComLINE.Position.Point := pntf ;
    btnComLINE.Width := pnlLeft.Width - 8 ;
    btnComLINE.Anchors := ltr ;
    btnComLINE.Font.Family := GIS_LS_FONT_FAMILY ;
    btnComLINE.Font.Size := 14 ;
    btnComLINE.TextSettings.HorzAlign := TTextAlign.Leading ;
    btnComLINE.Text := GIS_LS_INDENT + GIS_LS_CMD_LINE ;
    btnComLINE.OnClick := doLINEClick ;
    {$IFDEF LEVEL_RX10_FMX}
      btnComLINE.Hint := GIS_RS_LS_CMD_LINE_HINT ;
    {$ENDIF}

    t := btnComLINE.Position.Point.Y + btnComLINE.Height + 2 ;

    btnComOUTLINE := TButton.Create( pnlLeft ) ;
    btnComOUTLINE.Parent := pnlLeft ;
    btnComOUTLINE.StyledSettings := [ TStyledSetting.FontColor ] ;
    pntf := TPointF.Create( 4, t ) ;
    btnComOUTLINE.Position.Point := pntf ;
    btnComOUTLINE.Width := pnlLeft.Width - 8 ;
    btnComOUTLINE.Anchors := ltr ;
    btnComOUTLINE.Font.Family := GIS_LS_FONT_FAMILY ;
    btnComOUTLINE.Font.Size := 14 ;
    btnComOUTLINE.TextSettings.HorzAlign := TTextAlign.Leading ;
    btnComOUTLINE.Text := GIS_LS_INDENT + GIS_LS_CMD_OUTLINE ;
    btnComOUTLINE.OnClick := doOUTLINEClick ;
    {$IFDEF LEVEL_RX10_FMX}
      btnComOUTLINE.Hint := GIS_RS_LS_CMD_OUTLINE_HINT ;
    {$ENDIF}

    t := btnComOUTLINE.Position.Point.Y + btnComOUTLINE.Height + 2 ;

    btnComFILL := TButton.Create( pnlLeft ) ;
    btnComFILL.Parent := pnlLeft ;
    btnComFILL.StyledSettings := [ TStyledSetting.FontColor ] ;
    pntf := TPointF.Create( 4, t ) ;
    btnComFILL.Position.Point := pntf ;
    btnComFILL.Width := pnlLeft.Width - 8 ;
    btnComFILL.Anchors := ltr ;
    btnComFILL.Font.Family := GIS_LS_FONT_FAMILY ;
    btnComFILL.Font.Size := 14 ;
    btnComFILL.TextSettings.HorzAlign := TTextAlign.Leading ;
    btnComFILL.Text := GIS_LS_INDENT + GIS_LS_CMD_FILL ;
    btnComFILL.OnClick := doFILLClick ;
    {$IFDEF LEVEL_RX10_FMX}
      btnComFILL.Hint := GIS_RS_LS_CMD_FILL_HINT ;
    {$ENDIF}

    t := btnComFILL.Position.Point.Y + btnComFILL.Height + 2 ;

    btnComWIDTH := TButton.Create( pnlLeft ) ;
    btnComWIDTH.Parent := pnlLeft ;
    btnComWIDTH.StyledSettings := [ TStyledSetting.FontColor ] ;
    pntf := TPointF.Create( 4, t ) ;
    btnComWIDTH.Position.Point := pntf ;
    btnComWIDTH.Width := pnlLeft.Width - 8 ;
    btnComWIDTH.Anchors := ltr ;
    btnComWIDTH.Font.Family := GIS_LS_FONT_FAMILY ;
    btnComWIDTH.Font.Size := 14 ;
    btnComWIDTH.TextSettings.HorzAlign := TTextAlign.Leading ;
    btnComWIDTH.Text := GIS_LS_INDENT + GIS_LS_CMD_WIDTH ;
    btnComWIDTH.OnClick := doWIDTHClick ;
    {$IFDEF LEVEL_RX10_FMX}
      btnComWIDTH.Hint := GIS_RS_LS_CMD_WIDTH_HINT ;
    {$ENDIF}

    t := btnComWIDTH.Position.Point.Y + btnComWIDTH.Height + 2 ;

    btnComCOLOR := TButton.Create( pnlLeft ) ;
    btnComCOLOR.Parent := pnlLeft ;
    btnComCOLOR.StyledSettings := [ TStyledSetting.FontColor ] ;
    pntf := TPointF.Create( 4, t ) ;
    btnComCOLOR.Position.Point := pntf ;
    btnComCOLOR.Width := pnlLeft.Width - 8 ;
    btnComCOLOR.Anchors := ltr ;
    btnComCOLOR.Font.Family := GIS_LS_FONT_FAMILY ;
    btnComCOLOR.Font.Size := 14 ;
    btnComCOLOR.TextSettings.HorzAlign := TTextAlign.Leading ;
    btnComCOLOR.Text := GIS_LS_INDENT + GIS_LS_CMD_COLOR ;
    btnComCOLOR.OnClick := doCOLORClick ;
    {$IFDEF LEVEL_RX10_FMX}
      btnComCOLOR.Hint := GIS_RS_LS_CMD_COLOR_HINT ;
    {$ENDIF}

    t := btnComCOLOR.Position.Point.Y + btnComCOLOR.Height + 2 ;

    lblUnits := TLabel.Create( pnlLeft ) ;
    lblUnits.Parent := pnlLeft ;
    pntf := TPointF.Create( 0, t ) ;
    lblUnits.Position.Point := pntf ;
    lblUnits.Width := pnlLeft.Width ;
    lblUnits.Height := 20 ;
    lblUnits.AutoSize := False ;
    lblUnits.Anchors := ltr ;
    lblUnits.Text := '  ' + _rsrc( GIS_RS_LS_LBL_UNITSYMBOLS ) ;
    lblUnits.FixSize ;

    t := lblUnits.Position.Point.Y + lblUnits.Height ;

    lblUnitsList := TLabel.Create( pnlLeft ) ;
    lblUnitsList.Parent := pnlLeft ;
    pntf := TPointF.Create( 0, t ) ;
    lblUnitsList.Position.Point := pntf ;
    lblUnitsList.Width := pnlLeft.Width ;
    lblUnitsList.AutoSize := False ;
    lblUnitsList.Anchors := ltr ;
    lblUnitsList.Font.Family := GIS_LS_FONT_FAMILY ;
    lblUnitsList.Font.Size := 10 ;
    lblUnitsList.TextSettings.HorzAlign := TTextAlign.Center ;
    lblUnitsList.Text := GIS_LS_UNITS ;
    {$IFDEF LEVEL_RX10_FMX}
      lblUnitsList.Hint := GIS_RS_LS_UNITS_HINT ;
    {$ENDIF}
    lblUnitsList.FixSize ;

    t := lblUnitsList.Position.Point.Y + lblUnitsList.Height ;

    lblPreview := TLabel.Create( pnlRight ) ;
    lblPreview.Parent := pnlRight ;
    lblPreview.AutoSize := False ;
    lblPreview.Align := TAlignLayout.Top ;
    lblPreview.Height := 20 ;
    lblPreview.Text := _rsrcna( GIS_RS_LS_LBL_PREVIEW ) ;
    lblPreview.FixSize ;

    gisPreview := TGIS_ViewerWnd.Create( pnlRight ) ;
    gisPreview.Parent := pnlRight ;
    pntf := TPointF.Create(
      gisPreview.Position.Point.X,
      lblPreview.Position.Point.Y + lblPreview.Height + 8
    ) ;
    gisPreview.Position.Point := pntf ;
    gisPreview.Height := 192 ;
    gisPreview.Align := TAlignLayout.Top ;
    gisPreview.Mode := TGIS_ViewerMode.UserDefined ;

    hspltrRight := TSplitter.Create( pnlRight ) ;
    hspltrRight.Parent := pnlRight ;
    pntf := TPointF.Create(
      hspltrRight.Position.Point.X,
      gisPreview.Position.Point.Y + gisPreview.Height + 8
    ) ;
    hspltrRight.Position.Point := pntf ;
    hspltrRight.Align := TAlignLayout.Top ;

    // Wizard begin

    lblWizard := TLabel.Create( pnlRight ) ;
    lblWizard.Parent := pnlRight ;
    lblWizard.AutoSize := False ;
    pntf := TPointF.Create(
      lblWizard.Position.Point.X,
      gisPreview.Position.Point.Y + gisPreview.Height + 16
    ) ;
    lblWizard.Position.Point := pntf ;
    lblWizard.Height := 20 ;
    lblWizard.Align := TAlignLayout.Top ;
    lblWizard.Text := _rsrcna( GIS_RS_LS_LBL_WIZARD ) ;
    lblWizard.FixSize ;

    pnlWizard := TLayout.Create( pnlRight ) ;
    pnlWizard.Parent := pnlRight ;
    pntf := TPointF.Create(
      pnlWizard.Position.Point.X,
      lblWizard.Position.Point.Y + gisPreview.Height + 16
    ) ;
    pnlWizard.Position.Point := pntf ;
    pnlWizard.Align := TAlignLayout.Top ;
    pnlWizard.TabOrder := 0 ;

    cmbbxWizStart := T_LineSymbolComboBox.Create( pnlWizard ) ;
    cmbbxWizStart.Parent := pnlWizard ;
    pntf := TPointF.Create( 0, 0 ) ;
    cmbbxWizStart.Position.Point := pntf ;
    cmbbxWizStart.Width := 64 ;
    T_LineSymbolComboBox( cmbbxWizStart ).SymbolElement :=
      T_LineSymbolElement.SymbolStart ;
    T_LineSymbolComboBox( cmbbxWizStart ).UpdateList ;
    cmbbxWizStart.TabOrder := 0 ;

    btnWizard := TButton.Create( pnlWizard ) ;
    btnWizard.Parent := pnlWizard ;
    pntf := TPointF.Create(
      pnlWizard.Width - btnWizard.Width - 8,
      0
    ) ;
    btnWizard.Position.Point := pntf ;
    btnWizard.Anchors := tr ;
    btnWizard.Text := _rsrcna( GIS_RS_LS_LBL_WIZARD_APPLY ) ;
    btnWizard.OnClick := doWizardClick ;
    btnWizard.TabOrder := 3 ;

    cmbbxWizEnd := T_LineSymbolComboBox.Create( pnlWizard ) ;
    cmbbxWizEnd.Parent := pnlWizard ;
    cmbbxWizEnd.Width := 64 ;
    pntf := TPointF.Create(
      btnWizard.Position.Point.X - cmbbxWizEnd.Width - 4,
      0
    ) ;
    cmbbxWizEnd.Position.Point := pntf ;
    cmbbxWizEnd.Anchors := tr ;
    T_LineSymbolComboBox( cmbbxWizEnd ).SymbolElement :=
      T_LineSymbolElement.SymbolEnd ;
    T_LineSymbolComboBox( cmbbxWizEnd ).UpdateList ;
    cmbbxWizEnd.TabOrder := 2 ;

    cmbbxWizLine := T_LineSymbolComboBox.Create( pnlWizard ) ;
    cmbbxWizLine.Parent := pnlWizard ;
    pntf := TPointF.Create(
      cmbbxWizStart.Position.Point.X + cmbbxWizStart.Width + 4,
      0
    ) ;
    cmbbxWizLine.Position.Point := pntf ;
    cmbbxWizLine.Width := cmbbxWizEnd.Position.Point.X -
                          cmbbxWizLine.Position.Point.X - 4 ;
    cmbbxWizLine.Anchors := ltr ;
    T_LineSymbolComboBox( cmbbxWizLine ).SymbolElement :=
      T_LineSymbolElement.SymbolLine ;
    T_LineSymbolComboBox( cmbbxWizLine ).UpdateList ;
    cmbbxWizLine.TabOrder := 1 ;

    pnlWizard.Height := btnWizard.Position.Point.Y + btnWizard.Height ;

    oMainForm.OnResize := doResize ;

    // Wizard end

    lblCode := TLabel.Create( pnlRight ) ;
    lblCode.Parent := pnlRight ;
    pntf := TPointF.Create(
      lblCode.Position.Point.X,
      pnlWizard.Position.Point.Y + pnlWizard.Height + 16
    ) ;
    lblCode.Position.Point := pntf ;
    lblCode.AutoSize := False ;
    lblCode.Align := TAlignLayout.Top ;
    lblCode.Height := 20 ;
    lblCode.Text := _rsrcna( GIS_RS_LS_LBL_CODE ) ;
    lblCode.FixSize ;

    mmCode := TMemo.Create( pnlRight ) ;
    mmCode.Parent := pnlRight ;
    pntf := TPointF.Create(
      mmCode.Position.Point.X,
      lblCode.Position.Point.Y + lblCode.Height + 8
    ) ;
    mmCode.Position.Point := pntf ;
    mmCode.StyledSettings := [ TStyledSetting.FontColor ] ;
    mmCode.Font.Family := GIS_LS_FONT_FAMILY ;
    mmCode.Font.Size := 14 ;
    mmCode.Align := TAlignLayout.Client ;
    mmCode.TabOrder := 1 ;
    mmCode.WordWrap := False ;
    mmCode.Text := GIS_LS_DEF_SYMBOL ;
    mmCode.OnChangeTracking := doCodeChangeTracking ;
    mmCode.OnKeyDown := doCodeKeyDown ;

    lblError := TLabel.Create( pnlRight ) ;
    lblError.Parent := pnlRight ;
    lblError.StyledSettings := [ TStyledSetting.Family, TStyledSetting.Size ] ;
    lblError.AutoSize := False ;
    lblError.Align := TAlignLayout.Bottom ;
    lblError.Height := 20 ;
    lblError.Text := '' ;
    lblError.HitTest := True ;
    lblError.OnClick := doErrorClick ;
    lblError.OnMouseEnter := doErrorMouseEnter ;
    lblError.OnMouseLeave := doErrorMouseLeave ;

    {$IFDEF GIS_MOBILE_DIALOGS}
      Self.ClientHeight := Round( pnlMain.Position.Y + pnlMain.Height ) ;
    {$ENDIF}

    pnlMain.Anchors := ltrb ;
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
    ss    : TStyledSettings ;
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
      ss := lblError.StyledSettings ;
      if is_err then begin
        Exclude( ss, TStyledSetting.FontColor ) ;
        lblError.FontColor := TAlphaColors.Red ;
      end
      else begin
        Include( ss, TStyledSetting.FontColor ) ;
        lblError.FontColor := TAlphaColors.Black ;
      end ;
      lblError.StyledSettings := ss ;
      lblError.Text := estr ;
      btnOK.Enabled := not ( is_err or ( Length( Trim( mmCode.Text ) ) = 0 ) ) ;
      {$IFDEF GIS_MOBILE_DIALOGS}
        ShowOk := not ( is_err or ( Length( Trim( mmCode.Text ) ) = 0 ) ) ;
      {$ENDIF}
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
          SymbolList.Prepare( '&' + previous ) ;
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

    mmCode.DeleteSelection ;
    mmCode.InsertAfter( mmCode.CaretPosition, _com, [TInsertOption.CanUndo] ) ;
    k := _len ;

    ActiveControl := mmCode ;
    mmCode.SelStart := i + k ;
  end ;


  procedure TGIS_LineSymbolEditor.Execute(
    const _onhelp : TGIS_HelpEvent ;
    const _proc   : TProc<TModalResult>
  ) ;
  begin
    pOnHelp := _onhelp ;
    {$IFNDEF GIS_MOBILE_DIALOGS}
      btnHelp.Visible := Assigned( pOnHelp ) ;
    {$ENDIF}

    if IsStringEmpty( Symbol ) then
      cmbbxWizLine.ItemIndex := 22
    else
      cmbbxWizLine.ItemIndex := 0 ;
    ShowModalEx( _proc ) ;
  end ;

  procedure TGIS_LineSymbolEditor.Execute(
    const _proc  : TProc<TModalResult>
  ) ;
  var
    hlp : TGIS_HelpEvent ;
  begin
    hlp := nil ;
    Execute( hlp, _proc ) ;
  end;

//==================================== END =====================================
end.

