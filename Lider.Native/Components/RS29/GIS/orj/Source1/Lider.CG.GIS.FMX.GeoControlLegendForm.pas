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
  VCL renderer.
}

unit Lider.CG.GIS.FMX.GeoControlLegendForm ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.FMX.GeoControlLegendForm"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
    Winapi.Messages,
  {$ENDIF}
  System.Types,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  System.UITypes,

  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Objects,
  FMX.ListBox,
  FMX.TreeView,
  FMX.Types,
  FMX.Layouts,
  FMX.Edit,
  FMX.Memo,
  {$IFDEF LEVEL_XE7_FMX}
    FMX.ComboEdit,
  {$ENDIF}

  Lider.CG.GIS.GeoInterfaces,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoLayer,
  Lider.CG.GIS.GeoCsSystems,
  Lider.CG.GIS.GeoControlLegendFormControler,
  Lider.CG.GIS.FMX.GeoRenderer,
  Lider.CG.GIS.FMX.GeoFramework,
  Lider.CG.GIS.FMX.GeoViewerBmp,
  Lider.CG.GIS.FMX.GeoViewerWnd,
  Lider.CG.GIS.FMX.GeoControlLegend,
  Lider.CG.GIS.FMX.GeoLineSymbolEditor,
  Lider.CG.GIS.FMX.GeoModalForm ;

type

{$REGION 'TGIS_ControlLegendForm'}

  /// <summary>
  ///   Visual form for managing various layer properties.
  ///   Use class TGIS_ControlLegendForm.ShowLayerProperties() to call this form directly.
  /// </summary>
  TGIS_ControlLegendForm = class( TGIS_ModalForm )
    private
      MVC         : TGIS_ControlLegendFormMVC ;
      bLock       : Boolean ;
      iBusy       : Integer ;
    private
      pnlLeft     : TControl ;
      pnlRight    : TControl ;
      layRight    : TControl ;
      tvPages     : TTreeView ;
      gisPreview  : TGIS_ViewerBmp;
      gisPreviewR : TRectangle ;
      gisPreviewI : TImage ;
      btnAdd      : TSpeedButton ;
      btnRemove   : TSpeedButton ;
      btnClear    : TSpeedButton ;
      btnMoveUp   : TSpeedButton ;
      btnMoveDown : TSpeedButton ;
      btnOpen     : TSpeedButton ;
      btnSave     : TSpeedButton ;
      btnWizard   : TButton ;
      btnApply    : TButton ;
      layBottom   : TLayout ;
      pnlTop      : TLayout ;
      lblTop      : TLabel ;
      toolButtons : TToolBar ;
      tmrShowForm : TTimer ;
      tmrUpdate   : TTimer ;
      globalProc  : TProc<TModalResult> ;
      dlgOpen     : TOpenDialog;
      dlgSave     : TSaveDialog;
    private
      pnlCurrent  : TControl ;
      pnlGeneral  : TControl ;
      pnlSections : TControl ;
      lstFeatures : TObject ;
      lastSection : String ;
      lastNodeIdx : Integer ;
    private
      oLegend     : TGIS_ControlLegend ;
      oViewer     : IGIS_Viewer ;
      oViewerWnd  : TGIS_ViewerWnd ;
    private
      {$IFDEF IOS}
        {$IFNDEF LEVEL_RX101_FMX}
          BiDiMode : TBiDiMode ;
        {$ENDIF}
      {$ENDIF}
    private
      procedure initSelf ;
      procedure initLeft ;
      procedure initRight          ( const _full : Boolean
                                   ) ;
      procedure readLayer          ( const _full : Boolean
                                   ) ;
      procedure setSection         ( const _idx  : Integer ;
                                     const _fea  : Boolean
                                   ) ;
      procedure fillComboBoxWithFields
                                   ( const _cmb  : TComboBox
                                   ) ; overload;
      procedure fillComboBoxWithFields
                                   ( const _cmb  : TComboEdit
                                   ) ; overload;
      procedure showPreview        ( const _show : Boolean
                                   ) ;
      procedure lock               ;
      procedure unlock             ;
      function  isLocked           : Boolean ;
      procedure waitNoBusy         ;
      procedure updateStatistics   ( const _layer  : TGIS_Layer
                                   ) ;
      procedure addResBmp          ( const _name   : String ;
                                     const _parent : TCustomButton
                                   ) ;
      procedure doGlobalProc       ( _modal_result : TModalResult
                                   ) ;
    private
      procedure doShow             ( _sender : TObject
                                   ) ;
      procedure doPageChange       ( _node   : TTreeViewItem
                                   ) ;
      procedure doTvPagesOnChange  ( _sender : TObject
                                   ) ;
      procedure doApplySettings    ( _sender : TObject
                                   ) ;
      procedure btnOKClick         ( _sender : TObject
                                   ) ; override;
      procedure doAddClick         ( _sender : TObject
                                   ) ;
      procedure doRemoveClick      ( _sender : TObject
                                   ) ;
      procedure doClearClick       ( _sender : TObject
                                   ) ;
      procedure doMoveUpClick      ( _sender : TObject
                                   ) ;
      procedure doMoveDownClick    ( _sender : TObject
                                   ) ;
      procedure doOpenClick        ( _sender : TObject
                                   ) ;
      procedure doSaveClick        ( _sender : TObject
                                   ) ;
      procedure doWizardClick      ( _sender : TObject
                                   ) ;
      procedure doUpdateTimer      ( _sender : TObject
                                   ) ;
      procedure doSectionsRebuild ;
    protected

      /// <inheritdoc/>
      procedure initForm     ; override;

      /// <inheritdoc/>
      procedure initControls ; override;

    public

      /// <summary>
      ///   See documentation for TCustomForm in Delphi help.
      /// </summary>
      destructor Destroy ; override;

    public
      /// <summary>
      ///   Execute dialog on a given layer.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be attached
      /// </param>
      /// <param name="_legend">
      ///   legend control to be used; can be nil
      /// </param>
      /// <param name="_onhelp">
      ///   help notification function; if assigned the help button
      ///   will be visible and help support will be enabled;
      /// </param>
      /// <param name="_proc">
      ///   procedure to be executed upon modal result
      /// </param>
      procedure Execute     ( const _layer  : TGIS_Layer ;
                              const _legend : TGIS_ControlLegend ;
                              const _onhelp : TGIS_HelpEvent ;
                              const _proc   : TProc<TModalResult>
                            ) ; overload;

      /// <summary>
      ///   Execute dialog on a given layer.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be attached
      /// </param>
      /// <param name="_legend">
      ///   legend control to be used; can be nil
      /// </param>
      /// <param name="_proc">
      ///   procedure to be executed upon modal result
      /// </param>
      procedure Execute     ( const _layer  : TGIS_Layer ;
                              const _legend : TGIS_ControlLegend ;
                              const _proc   : TProc<TModalResult>
                            ) ; overload;

      /// <summary>
      ///   Show layer properties dialog for a given layer.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be attached
      /// </param>
      /// <param name="_legend">
      ///   legend control to be used; can be nil
      /// </param>
      /// <param name="_onhelp">
      ///   help notification function; if assigned the help button
      ///   will be visible and help support will be enabled;
      /// </param>
      /// <param name="_proc">
      ///   procedure to be executed upon modal result
      /// </param>
      class procedure ShowLayerProperties( const _layer  : TGIS_Layer         ;
                                           const _legend : TGIS_ControlLegend ;
                                           const _onhelp : TGIS_HelpEvent     ;
                                           const _proc   : TProc<TModalResult>
                                         ) ; overload;

      /// <summary>
      ///   Show layer properties dialog for a given layer.
      /// </summary>
      /// <param name="_layer">
      ///   layer to be attached
      /// </param>
      /// <param name="_legend">
      ///   legend control to be used; can be nil
      /// </param>
      /// <returns>
      ///   value returned by ShowModal function
      /// </returns>
      /// <param name="_proc">
      ///   procedure to be executed upon modal result
      /// </param>
      class procedure ShowLayerProperties( const _layer  : TGIS_Layer         ;
                                           const _legend : TGIS_ControlLegend ;
                                           const _proc   : TProc<TModalResult>
                                         ) ; overload;
  end ;

{$ENDREGION}

{$REGION 'GisShowLayerProperties'}

  /// <summary>
  ///   Show layer properties dialog for a given layer.
  /// </summary>
  /// <param name="_layer">
  ///   layer to be attached
  /// </param>
  /// <param name="_legend">
  ///   legend control to be used; can be nil
  /// </param>
  /// <param name="_onhelp">
  ///   help notification function; if assigned the help button
  ///   will be visible and help support will be enabled;
  /// </param>
  /// <param name="_proc">
  ///   procedure to be executed upon modal result
  /// </param>
  procedure GisShowLayerProperties ( const _layer  : TGIS_Layer         ;
                                     const _legend : TGIS_ControlLegend ;
                                     const _onhelp : TGIS_HelpEvent     ;
                                     const _proc   : TProc<TModalResult>
                                   ) ; overload;
                                   {$IFNDEF GENDOC} deprecated ; {$ENDIF}

  /// <summary>
  ///   Show layer properties dialog for a given layer.
  /// </summary>
  /// <param name="_layer">
  ///   layer to be attached
  /// </param>
  /// <param name="_legend">
  ///   legend control to be used; can be nil
  /// </param>
  /// <param name="_proc">
  ///   procedure to be executed upon modal result
  /// </param>
  procedure GisShowLayerProperties ( const _layer  : TGIS_Layer         ;
                                     const _legend : TGIS_ControlLegend ;
                                     const _proc   : TProc<TModalResult>
                                   ) ; overload;
                                   {$IFNDEF GENDOC} deprecated ; {$ENDIF}
{$ENDREGION}

//##############################################################################
implementation

{$R Lider.CG.GIS.FMX.GeoControlLegendForm_16x16.RES}

{$IFDEF DCC}
  uses
    System.Math,
    System.UIConsts,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoSymbol,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoCsBase,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoLayerPixel,

    Lider.CG.GIS.FMX.GeoControlHelper,
    Lider.CG.GIS.FMX.GeoControlVarious,
    Lider.CG.GIS.FMX.GeoValueValidatorHelper,
    Lider.CG.GIS.FMX.GeoControlColor,
    Lider.CG.GIS.PVL.GeoControlSizeForm,
    {$IFDEF GIS_MOBILE_DIALOGS}
      Lider.CG.GIS.FMX.Mobile.GeoControlCsSystem,
    {$ELSE}
      Lider.CG.GIS.FMX.GeoControlCsSystem,
    {$ENDIF}
    Lider.CG.GIS.FMX.GeoPvl,
    Lider.CG.GIS.PVL.GeoPvl,
    Lider.CG.GIS.PVL.GeoControlBitmap,
    Lider.CG.GIS.PVL.GeoControlSymbologyLibrary,
    Lider.CG.GIS.PVL.GeoControlSymbology,
    Lider.CG.GIS.PVL.GeoControlFieldFactor,
    Lider.CG.GIS.PVL.GeoControlColor,
    Lider.CG.GIS.PVL.GeoControlLegendVectorWiz,
    Lider.CG.GIS.PVL.GeoControlLegendGridWiz,
    Lider.CG.GIS.FMX.GeoControlStatistics ;
{$ENDIF}

{$REGION 'Const'}
const

  {$IFDEF GIS_MOBILE}
    WIDTH_WIDE   : Integer = 414 ;
    WIDTH_NARROW : Integer = 304 ;
    WIDTH_2COL   : Integer = 182 + 30 ;
    WIDTH_3COL   : Integer = 80 + 27 + 20 ;
    WIDTH_NORMAL : Integer = 440 ;
    LEFT_2COL_1  : Integer = 182 ;
    LEFT_2COL_2  : Integer = 216 ;
    LEFT_3COL_1  : Integer = 16  ;
    LEFT_3COL_2  : Integer = 112 + 24 + 20 ;
    LEFT_3COL_3  : Integer = 208 + 48 + 40 ;
    LEFT_3COL_4  : Integer = 326 + 60 ;
  {$ELSE}
    WIDTH_WIDE   : Integer = 414 ;
    WIDTH_NARROW : Integer = 304 ;
    WIDTH_2COL   : Integer = 182 ;
    WIDTH_3COL   : Integer = 80 + 27 ;
    WIDTH_NORMAL : Integer = 380 ;
    LEFT_2COL_1  : Integer = 182 ;
    LEFT_2COL_2  : Integer = 216 ;
    LEFT_3COL_1  : Integer = 16  ;
    LEFT_3COL_2  : Integer = 112 + 24 ;
    LEFT_3COL_3  : Integer = 208 + 48 ;
    LEFT_3COL_4  : Integer = 326 ;
  {$ENDIF}

  LABEL_GAP : Integer = 2 ;

  GROUPBOX_TOP : Integer = 4 ;
  GROUPBOX_LEFT : Integer = 8 ;
  GROUPBOX_TOP_GAP : Integer = 24 ;
  GROUPBOX_BOTTOM_GAP : Integer = 16 ;
  GROUPBOX_GAP : Integer = 8 ;

  DIP_TO_TWIPS = 15;

  MAX_PREVIEW_SIZE_LINE          = 16 * DIP_TO_TWIPS ;
  MAX_PREVIEW_SIZE_OUTLINE       = 12 * DIP_TO_TWIPS ;
  MAX_PREVIEW_SIZE_MAKER         = 32 * DIP_TO_TWIPS ;
  MAX_PREVIEW_SIZE_FONT          = 32 * DIP_TO_TWIPS ;
  MAX_PREVIEW_SIZE_PATTERNSYMBOL = 24 * DIP_TO_TWIPS ;
  MAX_PREVIEW_SIZE_RENDERER      = 12 * DIP_TO_TWIPS ;

  GIS_CONTAINER_INDEX : Integer = 1 ;
{$ENDREGION}

{$REGION 'Const'}
var
  oComboBox : TGIS_ComboBoxAbstract ;

{$ENDREGION}

{$REGION 'local methods'}
  procedure local_SetEnabledOnControl(
    const _control : TControl ;
    const _enable  : Boolean
  ) ;
  var
    c : TControl ;
    i : Integer ;
  begin
    if _enable then
      _control.Enabled := _enable ;

    for i := 0 to _control.ControlsCount - 1 do begin
      c := _control.Controls[i] ;
      c.Enabled := _enable ;
    end ;

    if not _enable then
      _control.Enabled := _enable ;
  end ;
{$ENDREGION}

{$REGION 'T_fieldPanel'}

type

  T_fieldPanel = class( TPanel )
    private
      lblField : TLabel ;
    private
      function  fget_Field : String ;
      procedure fset_Field ( const _name : String
                           ) ;
    protected
      procedure SetEnabled ( const Value : Boolean
                           ) ; override;
    public
      constructor Create ( _owner : TComponent
                         ) ; override;
    public
      property Field : String
                       read  fget_Field
                       write fset_Field ;
  end ;
{$ENDREGION}

{$REGION 'T_scrollablePanel'}
  T_scrollablePanel = class( TVertScrollBox )
    private
      pnlPanel      : TVertScrollBox ;
      vShift        : Integer ;
      hShift        : Integer ;
      iBlockUpdates : Integer ;
      lineSymbol    : String ;
    private
      FItemText     : String ;
    protected
      oParentWindow : TGIS_ControlLegendForm ;
    protected
      function  fget_HasPreview : Boolean ; virtual;
    protected
      procedure init  ; virtual;
      procedure lockUpdates     ;
      procedure unlockUpdates   ;
      function  blockedUpdates  : Boolean ;
    protected
      function GetDefaultStyleLookupName: string; override;
      function doCustomSize           (       _sender : TObject ;
                                              _value  : String
                                      ) : String ;
      function doCustomRWUnits        (       _sender : TObject ;
                                              _value  : String
                                      ) : String ;
      function doCustomRotation       (       _sender : TObject ;
                                              _value  : String
                                      ) : String ;
      function doCustomPattern        (       _sender : TObject ;
                                              _value  : String
                                      ) : String ;
      function doCustomShield         ( _sender : TObject ;
                                        _value  : String
                                      ) : String ;
      function doCustomColor          (       _sender : TObject ;
                                              _value  : String
                                      ) : String ;

      function doGetBitmapAreaFill    (       _sender : TObject ;
                                        const _value  : String  ;
                                        const _color  : TAlphaColor ;
                                        const _font   : TFont   ;
                                        const _width  : Integer ;
                                        const _height : Integer
                                       ) : TGIS_Bitmap ;
      function doGetBitmapMarkerStyle (       _sender : TObject ;
                                        const _value  : String  ;
                                        const _color  : TAlphaColor ;
                                        const _font   : TFont   ;
                                        const _width  : Integer ;
                                        const _height : Integer
                                      ) : TGIS_Bitmap ;

      function doGetBitmapAreaOutline (       _sender : TObject ;
                                        const _value  : String  ;
                                        const _color  : TAlphaColor ;
                                        const _font   : TFont   ;
                                        const _width  : Integer ;
                                        const _height : Integer
                                      ) : TGIS_Bitmap ;
      function  doGetBitmapShield     (       _sender : TObject ;
                                        const _value  : String  ;
                                        const _color  : TAlphaColor ;
                                        const _font   : TFont   ;
                                        const _width  : Integer ;
                                        const _height : Integer
                                      ) : TGIS_Bitmap ;

    public
      procedure Read  ; virtual;
      procedure Write ; virtual;
      procedure PreparePreview( const _viewer : IGIS_Viewer
                              ) ; virtual;
      procedure UpdatePreview   ; virtual;
    public
      constructor Create (       _owner : TFmxObject ;
                           const _pwnd  : TGIS_ControlLegendForm
                         ) ; reintroduce ;
    public
      property Panel      : TVertScrollBox
                            read pnlPanel ;
      property ItemText   : String
                            read  FItemText
                            write FItemText ;
      property HasPreview : Boolean
                            read  fget_HasPreview ;
  end ;
{$ENDREGION}

{$REGION 'T_sectionContainer'}
  T_sectionContainer = class
    private
      oParentWindow : TGIS_ControlLegendForm ;
      lstFeatures   : TObjectList<T_scrollablePanel> ;
    private
      function  fget_Count : Integer ;
      function  fget_Panel ( const _i : Integer
                           ) : T_scrollablePanel ;
    public
      procedure initVector ;
      procedure initPixel  ;
      procedure initGrid   ;
      procedure initDefault;
    public
      constructor Create   ( const _pwnd : TGIS_ControlLegendForm
                           ) ;
    public
      destructor Destroy   ; override;
    public
      procedure ReadSection;
      procedure Read       ;
      procedure Write      ;
      function  FindPanel  ( const _text : String
                           ) : T_scrollablePanel ;
    public
      property Count : Integer
               read  fget_Count ;
      property Panel[const _i : Integer] : T_scrollablePanel
               read  fget_Panel ;
  end ;
{$ENDREGION}

{$REGION 'T_panelGeneral'}
  T_panelGeneral = class( T_scrollablePanel )
    private
      mvc : TGIS_ControlLegendFormMVC_General ;
    private // group boxes
      gpbParameters : TGroupBox ;
      gpbInfo : TGroupBox ;
      gpbPainting : TGroupBox ;
      gpbThreshold : TGroupBox ;
      gpbAggregation : TGroupBox ;
    private // Parameters
      lblPath : TLabel ;
      edtPath : TEdit ;
      lblName : TLabel;
      lblCaption : TLabel ;
      edtName : TEdit ;
      edtCaption : TEdit ;
      lblCS : TLabel ;
      edtCS : TEdit ;
      btnCS : TButton ;
      ckbBasemap : TCheckBox ;
      ckbUseConfig : TCheckBox ;
      ckbCachedPaint : TCheckBox ;
      ckbIgnoreShapeParams : TCheckBox ;
      ckbMultipassRendering : TCheckBox ;
      lblTransparency : TLabel ;
      lblInterpretation : TLabel ;
      cmbTransparency : TComboEdit ;
      cmbInterpretation : TComboBox ;
      vvcmbTransparency : IGIS_ValueValidator ;
      lblScope : TLabel ;
      cmbScope : TComboEdit ;
    private // Info
      lblFileInformation : TLabel ;
      txbFileInformation : TMemo ;
      lblUserComments : TLabel ;
      txbUserComments : TMemo ;
      lblCodePage : TLabel ;
      edtCodePage : TEdit ;
      vvedtCodePage : IGIS_ValueValidator ;
    private // Aggregation
      lblAggMethod : TLabel ;
      cmbAggMethod : TComboBox ;
      lblAggRadius : TLabel ;
      cmbAggRadius : TGIS_SizeComboBox ;
      lblAggThreshold : TLabel ;
      edtAggThreshold : TEdit ;
      vvedtAggThreshold : IGIS_ValueValidator ;
    protected
      function  fget_HasPreview : Boolean ; override;
    private
      procedure initParameters ;
      procedure initInfo ;
    protected
      procedure init  ; override;
    public
      procedure Read  ; override;
      procedure Write ; override;
    protected
      procedure doCallback   ( _sender : TObject ;
                               _code   : Integer
                             ) ;
      procedure doCSClick    ( _sender : TObject
                             ) ;
      procedure doControlChange ( _sender : TObject
                                ) ;
      procedure doAggregateChange( _sender : TObject
                                 ) ;
  end ;
{$ENDREGION}

{$REGION 'T_panel3D'}
  T_panel3D = class( T_scrollablePanel )
    private
      mvc : TGIS_ControlLegendFormMVC_3D ;
    private
      rbnAs2D  : TRadioButton ;
      rbnAsDEM : TRadioButton ;
      rbnAs3D  : TRadioButton ;
    private
      lblNormalizedZ : TLabel ;
      lblNormalizedM : TLabel ;
      cmbNormalizedZ : TComboBox ;
      cmbNormalizedM : TComboBox ;
      lblScaleZ : TLabel ;
      lblScaleM : TLabel ;
      speScaleZ : TEdit ;
      speScaleM : TEdit ;
      vvspeScaleZ : IGIS_ValueValidator ;
      vvspeScaleM : IGIS_ValueValidator ;
      lblFalseZ : TLabel ;
      lblFalseM : TLabel ;
      cmbFalseZ : TGIS_SizeComboBox ;
      cmbFalseM : TGIS_SizeComboBox ;
      lblAdjustZ : TLabel ;
      lblAdjustBasement : TLabel ;
      cmbAdjustZ : TComboBox ;
      cmbAdjustBasement : TComboBox ;
      hasDEM : Boolean ;
    protected
      function  fget_HasPreview : Boolean ; override;
    protected
      procedure init  ; override;
    public
      procedure Read  ; override;
      procedure Write ; override;
    protected
      procedure doCallback   ( _sender : TObject ;
                               _code   : Integer
                             ) ;
      procedure doAs2DClick  ( _sender : TObject
                             ) ;
      procedure doAsDEMClick ( _sender : TObject
                             ) ;
      procedure doAs3DClick  ( _sender : TObject
                             ) ;
  end ;
{$ENDREGION}

{$REGION 'T_panelSections'}
  T_panelSections = class( T_scrollablePanel )
    protected
      gpbSection : TGroupBox ;
      lvSections : TListBox ;
    protected
      function  fget_HasPreview : Boolean ; override;

      procedure lvSectionsDblClick( _sender : TObject) ;
    protected
      procedure init  ; override;
    public
      procedure Prepare ;
      procedure PreparePreview( const _viewer : IGIS_Viewer
                              ) ; override;
      procedure UpdatePreview   ; override;

    public
      procedure Read  ; override;
      procedure Write ; override;
  end ;
{$ENDREGION}

{$REGION 'T_panelSection'}
  T_panelSection = class( T_scrollablePanel )
    protected
      mvc : TGIS_ControlLegendFormMVC_Section ;
    protected
      gpbSection : TGroupBox ;
      ckbVisible : TCheckBox ;
      lblMinScale : TLabel ;
      lblMaxScale : TLabel ;
      btnMinScaleCur : TButton ;
      btnMinScaleClr : TButton ;
      cmbMinScale : TComboEdit ;
      btnMaxScaleCur : TButton ;
      btnMaxScaleClr : TButton ;
      cmbMaxScale : TComboEdit ;
    protected
      function  fget_HasPreview : Boolean ; override;
    protected
      procedure init  ; override;
      procedure updateNode ; virtual;
    public
      procedure Read  ; override;
      procedure Write ; override;
    protected
      procedure doCallback       ( _sender : TObject ;
                                   _code   : Integer
                                 ) ; virtual;
      procedure doVisibleClick   ( _sender : TObject
                                 ) ;
      procedure doMinScaleClick  ( _sender : TObject
                                 ) ;
      procedure doMinScaleClear  ( _sender : TObject
                                 ) ;
      procedure doMinScaleChange ( _sender : TObject
                                 ) ;
      procedure doMaxScaleClick  ( _sender : TObject
                                 ) ;
      procedure doMaxScaleClear  ( _sender : TObject
                                 ) ;
      procedure doMaxScaleChange ( _sender : TObject
                                 ) ;
  end ;
{$ENDREGION}

{$REGION 'T_panelSectionVector'}
  T_panelSectionVector = class( T_panelSection )
    protected
      lblQuery : TLabel ;
      cmbQuery : TComboEdit ;
      lblLegend : TLabel ;
      edtLegend : TEdit ;
      wasLegendEdited : Boolean ;
    protected
      procedure init  ; override;
      procedure updateNode ; override;
      procedure updateSectionNode ;
    public
      procedure Read  ; override;
      procedure Write ; override;
    protected
      procedure doCallback     ( _sender : TObject ;
                                 _code   : Integer
                               ) ; override;
      procedure doQueryChange  ( _sender : TObject
                               ) ;
      procedure doLegendChange ( _sender : TObject
                               ) ;
  end ;
{$ENDREGION}

{$REGION 'T_panelRenderer'}
  T_panelRenderer = class( T_scrollablePanel )
    private
      mvc : TGIS_ControlLegendFormMVC_Renderer ;
    private // group boxes
      gpbRender : TGroupBox ;
      gpbFirst : TGroupBox ;
      gpbSecond : TGroupBox ;
      lblExpression : TLabel ;
      lblRounding : TLabel ;
      cmbExpression : TComboEdit ;
      speRounding : TEdit ;
      vvspeRounding : IGIS_ValueValidator ;
    private // First
      lblNumOfZones1 : TLabel ;
      lblMinVal1 : TLabel ;
      lblMaxVal1 : TLabel ;
      speNumOfZones1 : TEdit ;
      edtMinVal1 : TEdit ;
      edtMaxVal1 : TEdit ;
      vvspeNumOfZones1 : IGIS_ValueValidator ;
      vvedtMinVal1 : IGIS_ValueValidator ;
      vvedtMaxVal1 : IGIS_ValueValidator ;
      lblStartColor1 : TLabel ;
      lblEndColor1 : TLabel ;
      lblDefaultColor1 : TLabel ;
      cmbStartColor1 : TGIS_ColorComboBox ;
      cmbEndColor1 : TGIS_ColorComboBox ;
      cmbDefaultColor1 : TGIS_ColorComboBox ;
      lblStartSize1 : TLabel ;
      lblEndSize1 : TLabel ;
      lblDefaultSize1 : TLabel ;
      cmbStartSize1 : TGIS_SizeComboBox ;
      cmbEndSize1 : TGIS_SizeComboBox ;
      cmbDefaultSize1 : TGIS_SizeComboBox ;
    private // Second
      lblNumOfZones2 : TLabel ;
      lblMinVal2 : TLabel ;
      lblMaxVal2 : TLabel ;
      speNumOfZones2 : TEdit ;
      edtMinVal2 : TEdit ;
      edtMaxVal2 : TEdit ;
      vvspeNumOfZones2 : IGIS_ValueValidator ;
      vvedtMinVal2 : IGIS_ValueValidator ;
      vvedtMaxVal2 : IGIS_ValueValidator ;
      lblStartColor2 : TLabel ;
      lblEndColor2 : TLabel ;
      lblDefaultColor2 : TLabel ;
      cmbStartColor2 : TGIS_ColorComboBox ;
      cmbEndColor2 : TGIS_ColorComboBox ;
      cmbDefaultColor2 : TGIS_ColorComboBox ;
      lblStartSize2 : TLabel ;
      lblEndSize2 : TLabel ;
      lblDefaultSize2 : TLabel ;
      cmbStartSize2 : TGIS_SizeComboBox ;
      cmbEndSize2 : TGIS_SizeComboBox ;
      cmbDefaultSize2 : TGIS_SizeComboBox ;
    protected
      function  fget_HasPreview : Boolean ; override;
    private
      procedure initSelf ;
      procedure initFirst ;
      procedure initSecond ;
    private
      procedure enableFirst  ( _enable : Boolean
                             ) ;
      procedure enableSecond ( _enable : Boolean
                             ) ;
    protected
      procedure init  ; override;
    public
      procedure Read  ; override;
      procedure Write ; override;
    protected
      procedure doCallback     ( _sender : TObject ;
                                 _code   : Integer
                               ) ;
      procedure doExpressionChange
                               ( _sender : TObject
                               ) ;
      procedure doNumberOfZones1Change
                               ( _sender : TObject
                               ) ;
      procedure doNumberOfZones2Change
                               ( _sender : TObject
                               ) ;
      procedure doControlChange( _sender : TObject
                               ) ;
  end ;
{$ENDREGION}

{$REGION 'T_panelMarker'}
  T_panelMarker = class( T_scrollablePanel )
    private
      mvc : TGIS_ControlLegendFormMVC_Marker ;
    private
      ckbLegend : TCheckBox ;
      gpbMarker : TGroupBox ;
      gpbOutline : TGroupBox ;
    private // Marker
      lblStyle : TLabel ;
      cmbStyle : TGIS_SymbolComboBox ;
      lblColor : TLabel ;
      cmbColor : TGIS_ColorComboBox ;
      lblSize  : TLabel ;
      cmbSize  : TGIS_SizeComboBox ;
      lblPattern : TLabel ;
      cmbPattern : TGIS_PatternComboBox ;
      lblSymbolRotate : TLabel ;
      cmbSymbolRotate : TGIS_RotationComboBox ;
    private // Outline
      lblOStyle : TLabel ;
      cmbOStyle : TGIS_PenStyleComboBox ;
      lblOColor : TLabel ;
      cmbOColor : TGIS_ColorComboBox ;
      lblOWidth : TLabel ;
      cmbOWidth : TGIS_SizeComboBox ;
      lblOPattern : TLabel ;
      cmbOPattern : TGIS_PatternComboBox ;
    private // Smart size
      lblSSSize : TLabel ;
      cmbSSSize : TGIS_SizeComboBox ;
    protected
      function  fget_HasPreview : Boolean ; override;
    private
      procedure initSelf ;
      procedure initMarker ;
      procedure initOutline ;

    protected
      procedure init  ; override;
    public
      procedure Read  ; override;
      procedure Write ; override;
      procedure PreparePreview( const _viewer : IGIS_Viewer
                              ) ; override;
      procedure UpdatePreview   ; override;
    protected
      procedure doCallback     ( _sender : TObject ;
                                 _code   : Integer
                               ) ;
      procedure doSmartSizeFieldChange
                               ( _sender : TObject
                               ) ;
      procedure doControlChange( _sender : TObject
                               ) ;
      procedure doPatternChange( _sender : TObject
                               ) ;
      procedure doOPatternChange( _sender : TObject
                               ) ;
  end ;
{$ENDREGION}

{$REGION 'T_panelLine'}
  T_panelLine = class( T_scrollablePanel )
    private
      mvc : TGIS_ControlLegendFormMVC_Line ;
    private
      ckbLegend : TCheckBox ;
      gpbLine : TGroupBox ;
      gpbOutline : TGroupBox ;
    private // Line
      lblStyle : TLabel ;
      cmbStyle : TGIS_PenStyleComboBox ;
      lblColor : TLabel ;
      cmbColor : TGIS_ColorComboBox ;
      lblWidth : TLabel ;
      cmbWidth : TGIS_SizeComboBox ;
      lblPattern : TLabel ;
      cmbPattern : TGIS_PatternComboBox ;
      lblSymbolGap : TLabel ;
      cmbSymbolGap : TGIS_SizeComboBox ;
      lblSymbolRotate : TLabel ;
      cmbSymbolRotate : TGIS_RotationComboBox ;
    private // Outline
      lblOStyle : TLabel ;
      cmbOStyle : TGIS_PenStyleComboBox ;
      lblOColor : TLabel ;
      cmbOColor : TGIS_ColorComboBox ;
      lblOWidth : TLabel ;
      cmbOWidth : TGIS_SizeComboBox ;
      lblOPattern : TLabel ;
      cmbOPattern : TGIS_PatternComboBox ;
    private // Smart size
      lblSSSize : TLabel ;
      cmbSSSize : TGIS_SizeComboBox ;
    protected
      function  fget_HasPreview : Boolean ; override;
    private
      procedure initSelf ;
      procedure initLine ;
      procedure initOutline ;
    protected
      procedure init  ; override;
    public
      procedure Read  ; override;
      procedure Write ; override;
      procedure PreparePreview( const _viewer : IGIS_Viewer
                              ) ; override;
      procedure UpdatePreview   ; override;
    protected
      procedure doCallback     ( _sender : TObject ;
                                 _code   : Integer
                               ) ;
      procedure doSmartSizeFieldChange
                               ( _sender : TObject
                               ) ;
      procedure doControlChange( _sender : TObject
                               ) ;
      procedure doPatternChange( _sender : TObject
                               ) ;
      procedure doOPatternChange( _sender : TObject
                               ) ;
  end ;
{$ENDREGION}

{$REGION 'T_panelArea'}
  T_panelArea = class( T_scrollablePanel )
    private
      mvc : TGIS_ControlLegendFormMVC_Area ;
    private
      ckbLegend : TCheckBox ;
      gpbArea : TGroupBox ;
      gpbOutline : TGroupBox ;
    private // Area
      lblPattern : TLabel ;
      lblColor : TLabel ;
      cmbPattern : TGIS_PatternComboBox ;
      cmbColor : TGIS_ColorComboBox ;
      lblSymbolGap : TLabel ;
      lblSymbolRotate : TLabel ;
      cmbSymbolGap : TGIS_SizeComboBox ;
      cmbSymbolRotate : TGIS_RotationComboBox ;
      lblSymbolSize : TLabel ;
      cmbSymbolSize : TGIS_SizeComboBox ;
    private // Outline
      lblOStyle : TLabel ;
      cmbOStyle : TGIS_PenStyleComboBox ;
      lblOColor : TLabel ;
      cmbOColor : TGIS_ColorComboBox ;
      lblOWidth : TLabel ;
      cmbOWidth : TGIS_SizeComboBox ;
      lblOPattern : TLabel ;
      cmbOPattern : TGIS_PatternComboBox ;
      lblOSymbolGap : TLabel ;
      cmbOSymbolGap : TGIS_SizeComboBox ;
      lblOSymbolRotate : TLabel ;
      cmbOSymbolRotate : TGIS_RotationComboBox ;
    private // Smart size
      lblSSSize : TLabel ;
      cmbSSSize : TGIS_SizeComboBox ;
    protected
      function  fget_HasPreview : Boolean ; override;
    private
      procedure initSelf ;
      procedure initArea ;
      procedure initOutline ;
    protected
      procedure init  ; override;
    public
      procedure Read  ; override;
      procedure Write ; override;
      procedure PreparePreview( const _viewer : IGIS_Viewer
                              ) ; override;
      procedure UpdatePreview   ; override;
    protected
      procedure doCallback     ( _sender : TObject ;
                                 _code   : Integer
                               ) ;
      procedure doSmartSizeFieldChange
                               ( _sender : TObject
                               ) ;
      procedure doControlChange( _sender : TObject
                               ) ;
      procedure doPatternChange( _sender : TObject
                               ) ;
      procedure doOPatternChange( _sender : TObject
                               ) ;
  end ;
{$ENDREGION}

{$REGION 'T_panelLabel'}
  T_panelLabel = class( T_scrollablePanel )
    private
      mvc : TGIS_ControlLegendFormMVC_Label ;
    private
      ckbVisible : TCheckBox ;
      ckbLegend : TCheckBox ;
      ckbPAvoidOverlapping : TCheckBox ;
      ckbPAvoidDuplicates : TCheckBox ;
      gpbField : TGroupBox ;
      gpbLabel : TGroupBox ;
      gpbFont : TGroupBox ;
      gpbOutline : TGroupBox ;
      gpbPosition : TGroupBox ;
    private // Label
      lblFontName : TLabel ;
      cmbFontName : TGIS_FontNameComboBox ;
      lblFontSize : TLabel ;
      cmbFontSize : TGIS_SizeComboBox ;
      lblFontColor : TLabel ;
      cmbFontColor : TGIS_ColorComboBox ;
      lblWidth : TLabel ;
      lblHeight : TLabel ;
      lblColor : TLabel ;
      cmbWidth : TGIS_SizeComboBox ;
      cmbHeight : TGIS_SizeComboBox ;
      cmbColor : TGIS_ColorComboBox ;
      lblPattern : TLabel ;
      cmbPattern : TGIS_PatternComboBox ;
      lblShield : TLabel ;
      cmbShield : TGIS_ShieldComboBox ;
      btnFont : TGIS_FontButton ;
      cmbValue : TGIS_FieldValueComboBox ;
      ckbFontStyleBold : TCheckBox ;
      ckbFontStyleItalic : TCheckBox ;
      ckbFontStyleUnderline : TCheckBox ;
      ckbFontStyleStrikeout : TCheckBox ;
      ckbFontStyleShadow : TCheckBox ;
    private // Outline
      lblOStyle : TLabel ;
      cmbOStyle : TGIS_PenStyleComboBox ;
      lblOColor : TLabel ;
      cmbOColor : TGIS_ColorComboBox ;
      lblOWidth : TLabel ;
      cmbOWidth : TGIS_SizeComboBox ;
      lblOPattern : TLabel ;
      cmbOPattern : TGIS_PatternComboBox ;
    private // Smart size
      lblSSSize : TLabel ;
      cmbSSSize : TGIS_SizeComboBox ;
    private // Position
      lblPPosition : TLabel ;
      fldPPosition : T_fieldPanel ;
      fbnPPosition : TGIS_FieldButton ;
      btnPTopLeft : TButton ;
      btnPTopCenter : TButton ;
      btnPTopRight : TButton ;
      btnPCenterLeft : TButton ;
      btnPCenterCenter : TButton ;
      btnPCenterRight : TButton ;
      btnPBottomLeft : TButton ;
      btnPBottomCenter : TButton ;
      btnPBottomRight : TButton ;
      ckbPFlow : TCheckBox ;
      lblPAlignment : TLabel ;
      lblPRotation : TLabel ;
      cmbPAlignment : TComboBox ;
      cmbPRotation : TGIS_RotationComboBox ;
    protected
      function  fget_HasPreview : Boolean ; override;
    private
      procedure initSelf ;
      procedure initLabel ;
      procedure initFont ;
      procedure initOutline ;
      procedure initPosition ;
    protected
      procedure init  ; override;
    public
      procedure Read  ; override;
      procedure Write ; override;
      procedure PreparePreview( const _viewer : IGIS_Viewer
                              ) ; override;
      procedure UpdatePreview   ; override;
    private
      procedure actBtnPositionClick ( _sender : TObject
                                    ) ;
    protected
      procedure doCallback     ( _sender : TObject ;
                                 _code   : Integer
                               ) ;
      procedure doFieldChange  ( _sender : TObject
                               ) ;
      procedure doSmartSizeFieldChange
                               ( _sender : TObject
                               ) ;
      procedure doPositionExNotify
                               ( _sender : TObject
                               ) ;
      procedure doAlignmentChange
                               ( _sender : TObject
                               ) ;
      procedure doControlChange( _sender : TObject
                               ) ;
      procedure doShadowChange( _sender : TObject
                               ) ;
      procedure doOPatternChange( _sender : TObject
                               ) ;
      procedure doPatternChange( _sender : TObject
                               ) ;
      procedure doShieldChange ( _sender : TObject
                               ) ;
  end ;
{$ENDREGION}

{$REGION 'T_panelChart'}
  T_panelChart = class( T_scrollablePanel )
    private
      mvc : TGIS_ControlLegendFormMVC_Chart ;
    private
      ckbLegend : TCheckBox ;
      gpbChart : TGroupBox ;
      gpbValues : TGroupBox ;
    private // Chart
      lblStyle : TLabel ;
      lblSize : TLabel ;
      cmbStyle : TComboBox ;
      cmbSize : TGIS_SizeComboBox ;
      lblMinVal : TLabel ;
      lblMaxVal : TLabel ;
      edtMinVal : TEdit ;
      edtMaxVal : TEdit ;
    private // Values
      lblValue : TLabel ;
      lblLegend : TLabel ;
      lblVal1 : TLabel ;
      cmbVal1 : TComboEdit ;
      edtVal1 : TEdit ;
      pnlVal1 : TGIS_ColorPreview ;
      lblVal2 : TLabel ;
      cmbVal2 : TComboEdit ;
      edtVal2 : TEdit ;
      pnlVal2 : TGIS_ColorPreview ;
      lblVal3 : TLabel ;
      cmbVal3 : TComboEdit ;
      edtVal3 : TEdit ;
      pnlVal3 : TGIS_ColorPreview ;
      lblVal4 : TLabel ;
      cmbVal4 : TComboEdit ;
      edtVal4 : TEdit ;
      pnlVal4 : TGIS_ColorPreview ;
      lblVal5 : TLabel ;
      cmbVal5 : TComboEdit ;
      edtVal5 : TEdit ;
      pnlVal5 : TGIS_ColorPreview ;
      lblVal6 : TLabel ;
      cmbVal6 : TComboEdit ;
      edtVal6 : TEdit ;
      pnlVal6 : TGIS_ColorPreview ;
      lblVal7 : TLabel ;
      cmbVal7 : TComboEdit ;
      edtVal7 : TEdit ;
      pnlVal7 : TGIS_ColorPreview ;
      lblVal8 : TLabel ;
      cmbVal8 : TComboEdit ;
      edtVal8 : TEdit ;
      pnlVal8 : TGIS_ColorPreview ;
    private
      wasLegend1Edited : Boolean ;
      wasLegend2Edited : Boolean ;
      wasLegend3Edited : Boolean ;
      wasLegend4Edited : Boolean ;
      wasLegend5Edited : Boolean ;
      wasLegend6Edited : Boolean ;
      wasLegend7Edited : Boolean ;
      wasLegend8Edited : Boolean ;
    protected
      function  fget_HasPreview : Boolean ; override;
    private
      procedure initSelf ;
      procedure initChart ;
      procedure initValues ;
    protected
      procedure init  ; override;
    public
      procedure Read  ; override;
      procedure Write ; override;
      procedure PreparePreview( const _viewer : IGIS_Viewer
                              ) ; override;
      procedure UpdatePreview   ; override;
    protected
      procedure doCallback     ( _sender : TObject ;
                                 _code   : Integer
                               ) ;
      procedure doStyleChange  ( _sender : TObject
                               ) ;
      procedure doSizeUseRenderer
                               ( _sender : TObject
                               ) ;
      procedure doValue1Change ( _sender : TObject
                               ) ;
      procedure doLegend1Change( _sender : TObject
                               ) ;
      procedure doValue2Change ( _sender : TObject
                               ) ;
      procedure doLegend2Change( _sender : TObject
                               ) ;
      procedure doValue3Change ( _sender : TObject
                               ) ;
      procedure doLegend3Change( _sender : TObject
                               ) ;
      procedure doValue4Change ( _sender : TObject
                               ) ;
      procedure doLegend4Change( _sender : TObject
                               ) ;
      procedure doValue5Change ( _sender : TObject
                               ) ;
      procedure doLegend5Change( _sender : TObject
                               ) ;
      procedure doValue6Change ( _sender : TObject
                               ) ;
      procedure doLegend6Change( _sender : TObject
                               ) ;
      procedure doValue7Change ( _sender : TObject
                               ) ;
      procedure doLegend7Change( _sender : TObject
                               ) ;
      procedure doValue8Change ( _sender : TObject
                               ) ;
      procedure doLegend8Change( _sender : TObject
                               ) ;
      procedure doControlChange( _sender : TObject
                               ) ;
  end ;
{$ENDREGION}

{$REGION 'T_panelPixel'}
  T_panelPixel = class( T_scrollablePanel )
    private
      mvc : TGIS_ControlLegendFormMVC_Pixel ;
    private
      ckbLegend : TCheckBox ;
      gpbBands : TGroupBox ;
      gpbColors : TGroupBox ;
      lblRed : TLabel ;
      lblGreen : TLabel ;
      lblBlue : TLabel ;
      speRed : TEdit ;
      speGreen : TEdit ;
      speBlue : TEdit ;
      vvspeRed : IGIS_ValueValidator ;
      vvspeGreen : IGIS_ValueValidator ;
      vvspeBlue : IGIS_ValueValidator ;
      lblBrightness : TLabel ;
      lblContrast : TLabel ;
      speBrightness : TEdit ;
      speContrast : TEdit ;
      vvspeBrightness : IGIS_ValueValidator ;
      vvspeContrast : IGIS_ValueValidator ;
      lblRedBand : TLabel ;
      lblGreenBand : TLabel ;
      lblBlueBand : TLabel ;
      cmbRedBand : TComboBox ;
      cmbGreenBand : TComboBox ;
      cmbBlueBand : TComboBox ;
      lblAlphaBand : TLabel ;
      cmbAlphaBand : TComboBox ;
      lblPage : TLabel ;
      cmbPage : TComboBox ;
      ckbInversion : TCheckBox ;
      ckbGrayscale : TCheckBox ;
      ckbHistogram : TCheckBox ;
      ckbContrastEnhanced : TCheckBox ;
      btnReset : TButton ;
      gpbTransparency : TGroupBox ;
      pnlColorFrom : TGIS_ColorPreview ;
      pnlColorTo : TGIS_ColorPreview ;
      lstZones : TListBox ;
      btnAdd : TButton ;
      btnRemove : TButton ;
      btnClear : TButton ;
    protected
      function  fget_HasPreview : Boolean ; override;
    private
      procedure initSelf ;
    protected
      procedure init  ; override;
    public
      procedure Read  ; override;
      procedure Write ; override;
      procedure PreparePreview( const _viewer : IGIS_Viewer
                              ) ; override;
      procedure UpdatePreview   ; override;
    protected
      procedure doCallback     ( _sender : TObject ;
                                 _code   : Integer
                               ) ;
      procedure doReset        ( _sender : TObject
                               ) ;
      procedure doControlChange( _sender : TObject
                               ) ;
      procedure doZonesChange  ( _sender : TObject
                               ) ;
      procedure lstZoneDrawItem ( _sender : TObject ;
                                  _canvas : TCanvas ;
                                  const _rect   : TRectF
                                ) ;
      procedure lstZoneClick    ( _sender : TObject
                                ) ;
      procedure doAddClick      ( _sender : TObject
                                ) ;
      procedure doDeleteClick   ( _sender : TObject
                                ) ;
      procedure doClearClick    ( _sender : TObject
                                ) ;
      procedure doUpdate        ( _sender : TObject
                                ) ;
  end ;
{$ENDREGION}

{$REGION 'T_panelGrid'}
  T_panelGrid = class( T_scrollablePanel )
    private
      mvc : TGIS_ControlLegendFormMVC_Pixel ;
    private
      ckbLegend : TCheckBox ;
      gpbGridd : TGroupBox ;
      gpbThreshold : TGroupBox ;
      gpbRamp : TGroupBox ;
      lblMin : TLabel ;
      lblMax : TLabel ;
      lblLegend : TLabel ;
      pnlColor : TGIS_ColorPreview ;
      edtMin : TEdit ;
      edtMax : TEdit ;
      vvedtMin : IGIS_ValueValidator ;
      vvedtMax : IGIS_ValueValidator ;
      edtLegend : TEdit ;
      btnAdd : TButton ;
      btnRemove : TButton ;
      btnClear : TButton ;
      lstGrid : TListBox ;
      ckbShadow : TCheckBox ;
      lblGridBand : TLabel ;
      cmbGridBand : TComboBox ;
      updateIndex : Integer ;
      lblHeightMin : TLabel ;
      lblHeightMax : TLabel ;
      edtHeightMin : TEdit ;
      edtHeightMax : TEdit ;
      vvedtHeightMin : IGIS_ValueValidator ;
      vvedtHeightMax : IGIS_ValueValidator ;
       lblThresholdMin : TLabel ;
      lblThresholdMax : TLabel ;
      edtThresholdMin : TEdit ;
      edtThresholdMax : TEdit ;
      vvedtThresholdMin : IGIS_ValueValidator ;
      vvedtThresholdMax : IGIS_ValueValidator ;
      ckbAntialias : TCheckBox ;
      lockChange : Boolean ;
      ckbGridSmoothColors : TCheckBox ;
    protected
      function  fget_HasPreview : Boolean ; override;
    private
      procedure initSelf ;
    protected
      procedure init  ; override;
    public
      procedure Read  ; override;
      procedure Write ; override;
      procedure PreparePreview( const _viewer : IGIS_Viewer
                              ) ; override;
      procedure UpdatePreview   ; override;

    protected
      procedure doCallback      ( _sender : TObject ;
                                  _code   : Integer
                                ) ;
      procedure lstGridDrawItem ( _sender : TObject ;
                                  _canvas : TCanvas ;
                                  const _rect   : TRectF
                                ) ;
      procedure lstGridClick    ( _sender : TObject
                                ) ;
      procedure doControlChange ( _sender : TObject
                                ) ;
      procedure doAddClick      ( _sender : TObject
                                ) ;
      procedure doDeleteClick   ( _sender : TObject
                                ) ;
      procedure doClearClick    ( _sender : TObject
                                ) ;
      procedure doUpdate        ( _sender : TObject
                                ) ;
  end ;
{$ENDREGION}

{$REGION 'Utilities'}
function convert_color(
  const _color : String
) : TGIS_Color ;
var
  s1, s2, s3 : String ;
begin
  SplitParamAsText( _color, s1, s2, s3 );

  if s1 = GIS_PARAMTXT_TYPE_ARGB then
    Result := TGIS_Color.FromARGB( Cardinal(StrToInt64( '$'+s2)) )
  else
    Result := TGIS_Color.Gray ;
end ;

function normalize_color(
  _color       : TGIS_Color ;
  _colorAsText : String
) : TGIS_Color ;
var
  s1, s2, s3 : String ;
begin
  SplitParamAsText( _colorAsText, s1, s2, s3 ) ;
  if s1 = GIS_PARAMTXT_TYPE_FIELD then
    Result := TGIS_Color.DimGray
  else
  if s1 = GIS_PARAMTXT_TYPE_RENDERER then
    Result := TGIS_Color.DimGray
  else
    Result := _color ;
end ;

function normalize_angle(
  _angle       : Double ;
  _angleAsText : String
) : Double ;
var
  s1, s2, s3, s4 : String ;
begin
  SplitNumberAsText( _angleAsText, s1, s2, s3, s4 ) ;
  if s1 = GIS_PARAMTXT_TYPE_FIELD then
    Result := DegToRad( 45 )
  else
    Result := _angle ;
end ;

function normalize_size(
  _vwr        : IGIS_Viewer ;
  _size       : Integer;
  _sizeAsText : String ;
  _max_points : Integer
) : Integer ;
var
  s1, s2, s3, s4 : String ;
begin
  if not IsStringEmpty( _sizeAsText ) then
    SplitNumberAsText( _sizeAsText, s1, s2, s3, s4 )
  else
    s1 := '' ;
  if s1 = GIS_PARAMTXT_TYPE_FIELD then begin
    Result := _vwr.TwipsToPixels(  MAX_PREVIEW_SIZE_RENDERER ) ;
  end
  else
  if s1 = GIS_PARAMTXT_TYPE_RENDERER then begin
    Result := _vwr.TwipsToPixels(  MAX_PREVIEW_SIZE_RENDERER ) ;
  end
  else begin
    if Abs( _size ) > GIS_AUTOSIZE_SIZE then
      Result := _vwr.TwipsToPixels( MAX_PREVIEW_SIZE_RENDERER )
    else
      Result := _vwr.TwipsToPixels( _size ) ;
  end;

  if Result > _vwr.TwipsToPixels( _max_points ) then
    Result := _vwr.TwipsToPixels( _max_points ) ;

  Result := - Result ;
end;
{$ENDREGION}

{$REGION 'T_fieldPanel'}

constructor T_fieldPanel.Create(
  _owner : TComponent
) ;
begin
  inherited ;

  lblField := TLabel.Create( Self ) ;
  lblField.Parent := Self ;
  lblField.Align := TAlignLayout.Client ;
  lblField.Text := '' ;
end ;

function T_fieldPanel.fget_Field : String ;
begin
  Result := lblField.Text ;
end ;

procedure T_fieldPanel.fset_Field(
  const _name : String
) ;
begin
  lblField.Text := _name ;
end ;

procedure T_fieldPanel.setEnabled(
  const Value : Boolean
) ;
begin
  inherited ;

  lblField.Enabled := Value ;
end ;
{$ENDREGION}

{$REGION 'T_scrollablePanel'}
constructor T_scrollablePanel.Create(
        _owner : TFmxObject ;
  const _pwnd  : TGIS_ControlLegendForm
) ;
begin
  inherited Create( _owner ) ;

  Parent := _owner ;
  oParentWindow := _pwnd ;

  Self.Width := 256 ;
  Self.Height := 256 ;

  vShift := 0 ;
  hShift := 0 ;

  pnlPanel := Self ;

  lineSymbol := '' ;

  init ;
  read ;
end ;

function T_scrollablePanel.fget_HasPreview : Boolean ;
begin
  // to be implemented in descendant classes
  Result := False ;
end ;

procedure T_scrollablePanel.init ;
begin
  // to be implemented in descendant classes
end ;

procedure T_scrollablePanel.lockUpdates ;
begin
  Inc( iBlockUpdates ) ;
end ;

procedure T_scrollablePanel.unlockUpdates ;
begin
  Dec( iBlockUpdates ) ;
end ;

function T_scrollablePanel.blockedUpdates : Boolean ;
begin
  Result := ( iBlockUpdates > 0 ) ;
end ;

function T_scrollablePanel.GetDefaultStyleLookupName: string;
begin
  Result := 'scrollboxstyle';
end ;

function T_scrollablePanel.doCustomSize(
  _sender : TObject ;
  _value  : String
) : String ;
var
  dlg  : TGIS_ControlSizeForm ;
  frm  : TGIS_ControlFieldFactor ;
  val  : String ;
  proc : TGIS_Proc ;
begin
  Result := '' ;

  oComboBox := TGIS_ComboBoxAbstract( _sender ) ;

  if _value = GIS_PARAMTXT_TYPE_FIELD then begin
    frm := TGIS_ControlFieldFactor.Create( Self ) ;
    frm.FillFields( oParentWindow.MVC.FieldNames ) ;
    frm.FillUnits( TGIS_FieldFactorUnitsType.Size ) ;

    proc := procedure( _modal_result : TGIS_PvlModalResult )
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit ;

        val := ConstructNumberAsText(
                 frm.cmbFields.Text,
                 frm.spnFactor.Text,
                 frm.cmbUnits.Text
               ) ;
        oComboBox.DelayedUpdate( val ) ;
      end ;

    frm.Execute(
      oParentWindow.pOnHelp,
      proc
    );
    Result := val ;
  end
  else
  if _value = GIS_PARAMTXT_TYPE_CUSTOM then begin
    dlg := TGIS_ControlSizeForm.Create( Self ) ;
    dlg.FillUnits( False ) ;
    dlg.FillUnits( False ) ;
    proc := procedure( _modal_result : TGIS_PvlModalResult )
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit ;
        if dlg.isRotation then
          val := GIS_PARAMTXT_TYPE_ANGLE + ':' +
                    dlg.spnFactor.Text + ' ' + dlg.cmbUnits.Text
        else
          val := GIS_PARAMTXT_TYPE_SIZE + ':' +
                    dlg.spnFactor.Text + ' ' + dlg.cmbUnits.Text ;
        oComboBox.DelayedUpdate( val ) ;
      end ;

    dlg.Execute( oParentWindow.pOnHelp, proc ) ;
    Result := val ;
  end ;
end ;

function T_scrollablePanel.doCustomRWUnits(
  _sender : TObject ;
  _value  : String
) : String ;
var
  dlg  : TGIS_ControlSizeForm ;
  frm  : TGIS_ControlFieldFactor ;
  val  : String ;
  proc : TGIS_Proc ;
begin
  Result := '' ;

  oComboBox := TGIS_ComboBoxAbstract( _sender ) ;

  if _value = GIS_PARAMTXT_TYPE_FIELD then begin
    frm := TGIS_ControlFieldFactor.Create( Self ) ;
    frm.FillFields( oParentWindow.MVC.FieldNames ) ;
    frm.FillUnits( TGIS_FieldFactorUnitsType.Measure ) ;

    proc := procedure( _modal_result : TGIS_PvlModalResult )
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit ;

       val := ConstructNumberAsText(
                    frm.cmbFields.Text,
                    frm.spnFactor.Text,
                    frm.cmbUnits.Text
                  ) ;
        oComboBox.DelayedUpdate( val ) ;
      end;
    frm.Execute( oParentWindow.pOnHelp, proc ) ;
    Result := val ;
  end
  else
  if _value = GIS_PARAMTXT_TYPE_CUSTOM then begin
    dlg := TGIS_ControlSizeForm.Create( Self ) ;
    dlg.FillRealWorldUnits ;
    proc := procedure( _modal_result : TGIS_PvlModalResult )
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit ;
        if dlg.isRotation then
          val := GIS_PARAMTXT_TYPE_ANGLE + ':' +
                    dlg.spnFactor.Text + ' ' + dlg.cmbUnits.Text
        else
          val := GIS_PARAMTXT_TYPE_SIZE + ':' +
                    dlg.spnFactor.Text + ' ' + dlg.cmbUnits.Text

//        oComboBox.DelayedUpdate( val ) ;
      end ;

    dlg.Execute( oParentWindow.pOnHelp, proc ) ;
    Result := val ;
  end ;
end ;

function T_scrollablePanel.doCustomRotation(
  _sender : TObject ;
  _value  : String
) : String ;
var
  dlg : TGIS_ControlSizeForm ;
  frm : TGIS_ControlFieldFactor ;
  val : String ;
  proc: TGIS_Proc ;
begin
  Result := '' ;

  oComboBox := TGIS_ComboBoxAbstract( _sender ) ;

  if _value = GIS_PARAMTXT_TYPE_FIELD then begin
    frm := TGIS_ControlFieldFactor.Create( Self ) ;
    frm.FillFields( oParentWindow.MVC.FieldNames ) ;
    frm.FillUnits( TGIS_FieldFactorUnitsType.Angular ) ;

    proc := procedure( _modal_result : TGIS_PvlModalResult )
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit ;

       val := ConstructNumberAsText(
                    frm.cmbFields.Text,
                    frm.spnFactor.Text,
                    frm.cmbUnits.Text
                  ) ;
        oComboBox.DelayedUpdate( val ) ;
      end;

      frm.Execute( oParentWindow.pOnHelp, proc ) ;
      Result := val ;
  end
  else
  if _value = GIS_PARAMTXT_TYPE_CUSTOM then begin
    dlg := TGIS_ControlSizeForm.Create( Self ) ;
    dlg.FillUnits( True ) ;
    proc := procedure( _modal_result : TGIS_PvlModalResult )
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit ;
        if dlg.isRotation then
          val := GIS_PARAMTXT_TYPE_ANGLE + ':' +
                    dlg.spnFactor.Text + ' ' + dlg.cmbUnits.Text
        else
          val := GIS_PARAMTXT_TYPE_SIZE + ':' +
                    dlg.spnFactor.Text + ' ' + dlg.cmbUnits.Text ;

        oComboBox.DelayedUpdate( val ) ;
      end ;

    dlg.Execute( oParentWindow.pOnHelp, proc ) ;
    Result := val ;
  end ;
end;

function T_scrollablePanel.doCustomPattern(
  _sender : TObject ;
  _value  : String
) : String ;
var
  frms : TGIS_ControlSymbology ;
  {$IFNDEF GIS_MOBILE_DIALOGS}
    frmb : TGIS_ControlBitmap ;
  {$ENDIF}
  frme : TGIS_LineSymbolEditor ;
  s1, s2, s3 : String ;
  val : String ;
  frm : TGIS_ControlFieldFactor ;
  proc : TGIS_Proc ;
begin
  Result := '' ;

  oComboBox := TGIS_ComboBoxAbstract( _sender ) ;

  if _value = GIS_PARAMTXT_TYPE_SYMBOL then begin
    frms := TGIS_ControlSymbology.Create( Self ) ;

    proc := procedure( _modal_result : TGIS_PvlModalResult )
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit ;


        if assigned( frms.Symbol ) then begin
          val := ConstructParamAsText(
                      GIS_PARAMTXT_TYPE_SYMBOL,
                      frms.Symbol.Name,
                      ''
                    ) ;
         oComboBox.DelayedUpdate( val ) ;
        end;
      end;
    frms.Execute( '', oParentWindow.pOnHelp, proc ) ;
    Result := val ;
  end
  else
  if _value = GIS_PARAMTXT_TYPE_TEXTURE then begin
    {$IFNDEF GIS_MOBILE_DIALOGS}
      frmb := TGIS_ControlBitmap.Create( Self ) ;

      proc := procedure( _modal_result : TGIS_PvlModalResult )
        begin
          if _modal_result <> TGIS_PvlModalResult.OK then
            exit ;


          if assigned( frmb.Bitmap ) then begin
            val := ConstructParamAsText(
                        GIS_PARAMTXT_TYPE_TEXTURE,
                        frmb.Bitmap.Path,
                        ''
                      ) ;
            oComboBox.DelayedUpdate( val ) ;
          end;
        end;
      frmb.Execute( '', oParentWindow.pOnHelp, proc ) ;
      Result := val ;
    {$ENDIF}
  end
  else
  if _value = GIS_PARAMTXT_TYPE_CODE then begin
    frme := TGIS_LineSymbolEditor.Create( Self, True ) ;
    SplitParamAsText( TGIS_PenStyleComboBox( _sender ).Value, s1, s2, s3 );
    if s1 = GIS_PARAMTXT_TYPE_CODE then
      frme.Symbol := s2
    else
      frme.Symbol := '' ;

    frme.Execute(
      oParentWindow.pOnHelp,
      procedure( _modal_result : TModalResult )
      begin
        if _modal_result <> mrOK then
          exit ;

        val := ConstructParamAsText(
                    GIS_PARAMTXT_TYPE_CODE,
                    '' + frme.Symbol,
                    ''
                  ) ;
        oComboBox.DelayedUpdate( val ) ;
      end
    ) ;
  end
  else
  if _value = GIS_PARAMTXT_TYPE_FIELD then begin
    frm := TGIS_ControlFieldFactor.Create( Self ) ;
    frm.FillFields( oParentWindow.MVC.FieldNames ) ;
    frm.FillUnits( TGIS_FieldFactorUnitsType.NoScale ) ;

    proc := procedure( _modal_result : TGIS_PvlModalResult )
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit ;

        val := ConstructParamAsText(
                 GIS_PARAMTXT_TYPE_FIELD,
                 frm.cmbFields.Text,
                 ''
               ) ;
          oComboBox.DelayedUpdate( val ) ;
      end;

      frm.Execute( oParentWindow.pOnHelp, proc ) ;
      Result := val ;
  end

end ;

function T_scrollablePanel.doCustomShield(
  _sender : TObject ;
  _value  : String
) : String ;
var
  frm : TGIS_ControlSymbology ;
  s1, s2, s3 : String ;
  s : String ;
  val : String ;
  proc : TGIS_Proc ;
begin
  Result := '' ;

  oComboBox := TGIS_ComboBoxAbstract( _sender ) ;

  if _value = GIS_PARAMTXT_TYPE_SYMBOL then begin

    frm := TGIS_ControlSymbology.Create( Self ) ;

    proc := procedure( _modal_result : TGIS_PvlModalResult )
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit ;


        if assigned( frm.Symbol ) then begin
          val := ConstructParamAsText(
                      GIS_PARAMTXT_TYPE_SYMBOL,
                      frm.Symbol.Name,
                      ''
                    ) ;
          oComboBox.DelayedUpdate( val ) ;
        end;
      end;
    frm.OnlySVG := True ;
    frm.OnlyCategory := GIS_SHIELD_CATEGORY ;
    frm.Execute( '', oParentWindow.pOnHelp, proc ) ;
    Result := val ;
  end
  else
  if _value = 'nil' then begin
    Result := 'nil' ;
  end
  else
    Assert( False )
end ;


function T_scrollablePanel.doCustomColor(
  _sender : TObject ;
  _value  : String
) : String ;
var
  frmc : TGIS_ControlColor ;
  frmf : TGIS_ControlFieldFactor ;
  val  : String ;
  clr  : TGIS_Color ;
  proc : TGIS_Proc ;
begin
  Result := '' ;

  oComboBox := TGIS_ComboBoxAbstract( _sender ) ;

  if _value = GIS_PARAMTXT_TYPE_FIELD then begin
    frmf := TGIS_ControlFieldFactor.Create( Self ) ;
    frmf.FillFields( oParentWindow.MVC.FieldNames ) ;
    frmf.FillUnits( TGIS_FieldFactorUnitsType.Color ) ;

      proc := procedure( _modal_result : TGIS_PvlModalResult )
        begin
          if _modal_result <> TGIS_PvlModalResult.OK then
            exit ;

          val := ConstructParamAsText(
                   GIS_PARAMTXT_TYPE_FIELD,
                   frmf.cmbFields.Text,
                   frmf.cmbFormat.Text
                 ) ;
          oComboBox.DelayedUpdate( val ) ;
        end;
      frmf.Execute( oParentWindow.pOnHelp, proc ) ;
      Result := val ;
  end
  else
  if _value = GIS_PARAMTXT_TYPE_CUSTOM then begin
    frmc := TGIS_ControlColor.Create( Self ) ;

    if _sender is TGIS_ColorComboBox then
      clr := convert_color( TGIS_ColorComboBox( _sender ).Value )
    else
      clr := TGIS_Color.White;

      proc := procedure( _modal_result : TGIS_PvlModalResult )
        begin
          if _modal_result <> TGIS_PvlModalResult.OK then
            exit ;

          val := ConstructParamAsText(
                 GIS_PARAMTXT_TYPE_ARGB,
                 IntToHex( frmc.Color.ARGB, 2 ),
                   ''
                 ) ;
          oComboBox.DelayedUpdate( val ) ;
        end;
      frmc.Execute( clr, oParentWindow.pOnHelp, proc ) ;
      Result := val ;
  end ;
end ;


function T_scrollablePanel.doGetBitmapAreaFill(
        _sender : TObject ;
  const _value  : String  ;
  const _color  : TAlphaColor ;
  const _font   : TFont   ;
  const _width  : Integer ;
  const _height : Integer
) : TGIS_Bitmap ;
var
  vbmp   : TGIS_ViewerBmp   ;
  ll     : TGIS_LayerVector ;
  shp    : TGIS_Shape       ;
  shpl   : TGIS_Shape       ;
  w,h    : Integer          ;
  cx,cy  : Single           ;
  sizmul : Double           ;
begin
  sizmul := GUIScale * oParentWindow.PPI/oParentWindow.SystemPPI ;

  w := TruncS( _width  * sizmul ) ;
  h := TruncS( _height * sizmul ) ;

  if w mod 2 = 0 then
   cx := -0.5
  else
   cx := 0 ;

  if h mod 2 =0 then
   cy := -0.5
  else
   cy := 0 ;

  vbmp := TGIS_ViewerBmp.Create( w, h ) ;
  try
    vbmp.CustomPPI := oParentWindow.PPI ;
    vbmp.FontScale := RoundS( 96/oParentWindow.SystemPPI * GUIScale * 100
                              {$IFDEF MSWINDOWS}
                                * 72/96
                              {$ENDIF}
                             ) ;
    vbmp.Color := TGIS_Color.None ;

    ll := TGIS_LayerVector.Create ;
    ll.Params.Labels.Allocator := False;
    vbmp.Add( ll ) ;
    vbmp.BigExtentMargin := 0 ;

    shp := ll.CreateShape( TGIS_ShapeType.Polygon ) ;
    shp.Lock( TGIS_Lock.Extent );
    shp.AddPart ;
    shp.AddPoint( GisPoint( cx-w/2, cy-h/2 ) ) ;
    shp.AddPoint( GisPoint( cx-w/2, cy+h/2 ) ) ;
    shp.AddPoint( GisPoint( cx+w/2, cy+h/2 ) ) ;
    shp.AddPoint( GisPoint( cx+w/2, cy-h/2 ) ) ;
    shp.Unlock ;

    shp.Params.Area.OutlineWidth  := 0 ;
    shp.Params.Area.PatternAsText := _value ;

    shp.Params.Area.SymbolGap     :=  0 ;
    shp.Params.Area.SymbolSize    := -h ;

    if Assigned( shp.Params.Area.Symbol ) then begin
      if shp.Params.Area.Symbol is TGIS_SymbolFont then
        shp.Params.Area.SymbolSize  := -h * 3  div 4 // bit more space for fonts
      else
        shp.Params.Area.SymbolSize  := RoundS( -h *
                                               shp.Params.Area.Symbol.Width /
                                               shp.Params.Area.Symbol.Height
                                             ) ;
    end;

    shp.Params.Area.Color  := GISColor( _color ) ;

    if ( (shp.Params.Area.Pattern = TGIS_BrushStyle.Clear) or
         (shp.Params.Area.Pattern = TGIS_BrushStyle.Solid)
       )
       and
       TGIS_Bitmap.IsNilOrEmpty( shp.Params.Area.Bitmap )
       and
       ( not Assigned ( shp.Params.Area.Symbol ) )
    then begin
      shpl := ll.CreateShape( TGIS_ShapeType.Point ) ;
      shpl.Lock( TGIS_Lock.Extent );
      shpl.AddPart ;
      shpl.AddPoint( GisPoint( cx, cy ) ) ;
      shpl.Unlock ;
      if shp.Params.Area.Pattern = TGIS_BrushStyle.Clear then
        shpl.Params.Labels.Value := _rsrc( GIS_RS_LEGEND_PRM_BRUSH_TRANSPARENT )
      else begin
        shp.Params.Area.OutlineWidth := 1 ;
//        shp.Params.Area.Pattern  := TGIS_BrushStyle.Clear ;
        shp.Params.Area.Color := TGIS_Color.LightGray ;
        shpl.Params.Labels.Value := _rsrc( GIS_RS_LEGEND_PRM_BRUSH_SOLID ) ;
      end ;
      shpl.Params.Labels.Position  := [TGIS_LabelPosition.MiddleCenter] ;
      shpl.Params.Labels.FontName  := _font.Family ;
      shpl.Params.Labels.FontSize  := TGIS_ParamsLabelFont.PtToFontSize(
                                        TruncS( _font.Size )
                                      );
      shpl.Params.Labels.Color     := GISColor( _color ) ;
      shpl.Params.Labels.FontColor := GISCOlor( _color ) ;
      shpl.Params.Marker.Size := 0 ;
    end ;

    vbmp.FullExtent ;
    vbmp.Draw ;

    Result := TGIS_Bitmap.Create ;
    Result.LoadFromBitmap( vbmp.Bitmap, _value ) ;
  finally
    FreeObject( vbmp ) ;
  end ;
end ;

function T_scrollablePanel.doGetBitmapAreaOutline(
        _sender : TObject ;
  const _value  : String  ;
  const _color  : TAlphaColor ;
  const _font   : TFont   ;
  const _width  : Integer ;
  const _height : Integer
) : TGIS_Bitmap ;
var
  vbmp   : TGIS_ViewerBmp   ;
  ll     : TGIS_LayerVector ;
  shp    : TGIS_Shape       ;
  shpl   : TGIS_Shape       ;
  w,h    : Integer          ;
  cx,cy  : Single           ;
  bline  : Boolean          ;
  sizmul : Double           ;
begin
  sizmul := GUIScale * oParentWindow.PPI/oParentWindow.SystemPPI ;

  w := TruncS( _width  * sizmul ) ;
  h := TruncS( _height * sizmul ) ;

  if w mod 2 = 0 then
   cx := -0.5
  else
   cx := 0 ;

  if h mod 2 =0 then
   cy := -0.5
  else
   cy := 0 ;

  vbmp := TGIS_ViewerBmp.Create( w, h ) ;
  try
    vbmp.CustomPPI := oParentWindow.PPI ;
    vbmp.FontScale := RoundS( 96/oParentWindow.SystemPPI * GUIScale * 100
                              {$IFDEF MSWINDOWS}
                                * 72/96
                              {$ENDIF}
                             ) ;
    vbmp.Color := TGIS_Color.None ;

    ll := TGIS_LayerVector.Create ;
    ll.Extent := GisExtent ( cx-w/2, cy-h/2, cx+w/2, cy+h/2 ) ;
    vbmp.Add( ll ) ;

    shp := ll.CreateShape( TGIS_ShapeType.Arc ) ;

    shp.Params.Line.StyleAsText := _value ;

    bline := False ;
    if shp.Params.Line.Symbol is TGIS_SymbolLineEx then
      bline := True
    else
    if shp.Params.Line.Symbol is TGIS_SymbolLine then
      bline := True  ;

    shp.Lock( TGIS_Lock.Extent );
    shp.AddPart ;

    if Assigned( shp.Params.Line.Symbol ) and ( not bline ) then begin
      shp.AddPoint( GisPoint( -w/2 * 0.7, cy ) ) ;
      shp.AddPoint( GisPoint(  w/2,       cy ) ) ;
    end
    else begin
      shp.AddPoint( GisPoint( -w/2 * 0.7, cy ) ) ;
      shp.AddPoint( GisPoint(  w/2 * 0.7, cy ) ) ;
    end;
    shp.Unlock ;

    shp.Params.Line.Color         := GISColor( _color ) ;
    shp.Params.Line.OutlineColor  := GISColor( _color ) ;

    if Assigned( shp.Params.Line.Symbol ) then begin
      if not bline then // symbol character
        shp.Params.Line.Width       := -h * 4 div 5
      else
        shp.Params.Line.WidthAsText := 'SIZE:2dip' ;
    end
    else begin
      shp.Params.Line.WidthAsText   := 'SIZE:3dip' ;
    end;

    if shp.Params.Line.Style = TGIS_PenStyle.Clear then begin
      shpl := ll.CreateShape( TGIS_ShapeType.Point ) ;
      shpl.Lock( TGIS_Lock.Extent );
      shpl.AddPart ;
      shpl.AddPoint( GisPoint( 0, 0 ) ) ;
      shpl.Unlock ;
      shpl.Params.Labels.Value := _rsrc( GIS_RS_LEGEND_PRM_BRUSH_CLEAR ) ;
      shpl.Params.Labels.Position := [TGIS_LabelPosition.MiddleCenter] ;
      shpl.Params.Labels.FontName  := _font.Family ;
      shpl.Params.Labels.FontSize  := TGIS_ParamsLabelFont.PtToFontSize(
                                        TruncS( _font.Size )
                                      );
      shpl.Params.Labels.Color     := GISColor( _color ) ;
      shpl.Params.Labels.FontColor := GISColor( _color ) ;
      shpl.Params.Marker.Size := 0 ;
    end ;

    vbmp.VisibleExtent := GisExtent ( -w/2, -h/2, w/2, h/2 ) ;

    vbmp.Draw ;

    Result := TGIS_Bitmap.Create ;
    Result.LoadFromBitmap( vbmp.Bitmap, _value ) ;
  finally
    FreeObject( vbmp ) ;
  end ;
end ;

function T_scrollablePanel.doGetBitmapMarkerStyle(
        _sender : TObject ;
  const _value  : String  ;
  const _color  : TAlphaColor ;
  const _font   : TFont   ;
  const _width  : Integer ;
  const _height : Integer
) : TGIS_Bitmap ;
var
  vbmp    : TGIS_ViewerBmp   ;
  ll      : TGIS_LayerVector ;
  shp     : TGIS_Shape       ;
  w,h     : Integer          ;
  cx,cy   : Single           ;
  sh      : Single           ;
  sw      : Single           ;
  sizmul  : Double           ;
  old_cnt : Boolean          ;
begin
  sizmul := GUIScale * oParentWindow.PPI/oParentWindow.SystemPPI ;

  w := TruncS( _width  * sizmul ) ;
  h := TruncS( _height * sizmul ) ;

  if w mod 2 = 0 then
   cx := -0.5
  else
   cx := 0 ;

  if h mod 2 =0 then
   cy := -0.5
  else
   cy := 0 ;

  vbmp := TGIS_ViewerBmp.Create( w, h ) ;
  try
    vbmp.CustomPPI := oParentWindow.PPI ;
    vbmp.FontScale := RoundS( 96/oParentWindow.SystemPPI * GUIScale * 100
                              {$IFDEF MSWINDOWS}
                                * 72/96
                              {$ENDIF}
                             ) ;
    vbmp.Color := TGIS_Color.None ;

    ll := TGIS_LayerVector.Create ;
    vbmp.Add( ll ) ;

    ll.Extent := GisExtent( cx-w/2, cy-h/2, cx+w/2, cx+h/2 ) ;

    shp := ll.CreateShape( TGIS_ShapeType.Point ) ;
    shp.Lock( TGIS_Lock.Extent );
    shp.AddPart ;
    shp.AddPoint( GisPoint( cx, cy ) ) ;
    shp.Unlock ;
    vbmp.FullExtent ;

    shp.Params.Marker.StyleAsText := _value ;
    shp.Params.Marker.Color := GISColor( _color ) ;
    shp.Params.Marker.OutlineColor := GISColor( _color ) ;
    shp.Params.Marker.Size  := -h * 3 div 4 ;

    if Assigned( shp.Params.Marker.Symbol ) then begin
      old_cnt := shp.Params.Marker.Symbol.AutoCenter ;
      shp.Params.Marker.Symbol.AutoCenter := True ;

      sh := shp.Params.Marker.Symbol.NativeHeight ;
      if sh = 0 then
        sh := 1 ;
      sw := shp.Params.Marker.Symbol.NativeWidth ;
      if sw = 0 then
        sw := 1 ;

      if shp.Params.Marker.Symbol is TGIS_SymbolFont then begin
        shp.Params.Marker.Size  := -h * 3  div 4 ;// bit more space for fonts
        shp.Params.Marker.OutlineWidth := 0 ;
      end
      else begin
        if sh >= sw then
          shp.Params.Marker.Size := RoundS( -h )
        else begin
          if sw/sh * h > 4/5 * _width then
            shp.Params.Marker.Size := RoundS( - 4/5 * _width * sh/sw )
          else
            shp.Params.Marker.Size := RoundS( -h ) ;
        end;
      end;
    end;

    vbmp.Draw ;

    if assigned(shp.Params.Marker.Symbol) then
      shp.Params.Marker.Symbol.AutoCenter := old_cnt ;

    Result := TGIS_Bitmap.Create ;
    Result.LoadFromBitmap( vbmp.Bitmap, _value ) ;
  finally
    FreeObject( vbmp ) ;
  end ;
end ;

function T_scrollablePanel.doGetBitmapShield(
        _sender : TObject ;
  const _value  : String  ;
  const _color  : TAlphaColor ;
  const _font   : TFont   ;
  const _width  : Integer ;
  const _height : Integer
) : TGIS_Bitmap ;
var
  vbmp : TGIS_ViewerBmp   ;
  ll   : TGIS_LayerVector ;
  shp  : TGIS_Shape       ;
  w,h  : Integer          ;
  sh   : Single           ;
  sw   : Single           ;
  sizmul : Double         ;
begin
  sizmul := GUIScale * oParentWindow.PPI/oParentWindow.SystemPPI ;

  w := TruncS( _width  * sizmul ) ;
  h := TruncS( _height * sizmul ) ;

  vbmp := TGIS_ViewerBmp.Create( w, h ) ;
  try
    vbmp.CustomPPI := oParentWindow.PPI ;
    vbmp.FontScale := RoundS( 96/oParentWindow.SystemPPI * GUIScale * 100
                              {$IFDEF MSWINDOWS}
                                * 72/96
                              {$ENDIF}
                             ) ;
    vbmp.Color := TGIS_Color.None ;

    ll := TGIS_LayerVector.Create ;
    vbmp.Add( ll ) ;

    ll.Extent := GisExtent( -w/2, -h/2, w/2, h/2 ) ;

    shp := ll.CreateShape( TGIS_ShapeType.Point ) ;
    shp.Lock( TGIS_Lock.Extent );
    shp.AddPart ;
    shp.AddPoint( GisPoint( 0, 0 ) ) ;
    shp.Unlock ;
    vbmp.FullExtent ;

    shp.Params.Marker.StyleAsText := _value ;
    shp.Params.Marker.Color := GISColor( _color ) ;
    shp.Params.Marker.OutlineColor := GISColor( _color ) ;
    shp.Params.Marker.Size  := -h * 3 div 4 ;

    if Assigned( shp.Params.Marker.Symbol ) then begin
      sh := shp.Params.Marker.Symbol.NativeHeight ;
      if sh = 0 then
        sh := 1 ;
      sw := shp.Params.Marker.Symbol.NativeWidth ;
      if sw = 0 then
        sw := 1 ;
      if sh >= sw then
        shp.Params.Marker.Size := RoundS( -h )
      else begin
        if sw/sh * h > 4/5 * _width then
          shp.Params.Marker.Size := RoundS( - 4/5 * _width * sh/sw )
        else
          shp.Params.Marker.Size := RoundS( -h ) ;
      end;
    end
    else begin
      shp.Params.Labels.Value := _rsrc( GIS_RS_LEGEND_PRM_BRUSH_CLEAR ) ;
      shp.Params.Labels.Position  := [TGIS_LabelPosition.MiddleCenter] ;
      shp.Params.Labels.FontName  := _font.Family ;
      shp.Params.Labels.FontSize  := TGIS_ParamsLabelFont.PtToFontSize(
                                        TruncS( _font.Size )
                                      );
      shp.Params.Labels.Color     := GISColor( _color ) ;
      shp.Params.Labels.FontColor := GISColor( _color ) ;
      shp.Params.Marker.Size := 0 ;
    end ;

 //   vbmp.VisibleExtent := GisExtent ( -w/2, -h/2, w/2, h/2 ) ;

    vbmp.Draw ;

    Result := TGIS_Bitmap.Create ;
    Result.LoadFromBitmap( vbmp.Bitmap, _value ) ;
  finally
    FreeObject( vbmp ) ;
  end ;
end ;

procedure T_scrollablePanel.Read ;
begin
  // to be implemented in descendant classes
end ;

procedure T_scrollablePanel.Write ;
begin
  // to be implemented in descendant classes
end ;

procedure T_scrollablePanel.PreparePreview(
  const _viewer : IGIS_Viewer
) ;
begin

end ;

procedure T_scrollablePanel.UpdatePreview ;
begin

end ;



{$ENDREGION}

{$REGION 'T_sectionContainer'}
constructor T_sectionContainer.Create(
  const _pwnd : TGIS_ControlLegendForm
) ;
begin
  inherited Create ;

  oParentWindow := _pwnd ;

  lstFeatures := TObjectList<T_scrollablePanel>.Create( True ) ;

  oParentWindow.btnWizard.Enabled := False ;

  if oParentWindow.MVC.IsVector then
    initVector
  else if oParentWindow.MVC.IsGrid then
    initGrid
  else if oParentWindow.MVC.IsPixel then
    initPixel
  else
    initDefault ;
end ;

destructor T_sectionContainer.Destroy ;
begin
  FreeObject( lstFeatures ) ;

  inherited ;
end ;

procedure T_sectionContainer.initDefault ;
var
  pnl  : T_scrollablePanel ;
  prnt : TControl ;
  w     : Single ;
begin
  prnt := oParentWindow.pnlRight ;
  w := oParentWindow.pnlGeneral.Width ;

  pnl := T_panelSection.Create( prnt, oParentWindow ) ;
  pnl.Visible := False ;
  pnl.Position.X    := 0 ;
  pnl.Position.Y     := 0 ;
  pnl.Width   := w ;
  pnl.Height  := prnt.Height ;
  pnl.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom ] ;
  pnl.Align   := TAlignLayout.Client ;
  pnl.Name := 'pnlSection' ;

  lstFeatures.Add( pnl ) ;
end ;

procedure T_sectionContainer.initVector ;
var
  pnl   : T_scrollablePanel ;
  prnt  : TControl ;
  w     : Single ;
begin
  oParentWindow.btnWizard.Enabled := True ;

  prnt := oParentWindow.pnlRight ;
  w := oParentWindow.pnlGeneral.Width ;

  pnl := T_panelSectionVector.Create( prnt, oParentWindow ) ;
  pnl.Visible := False ;
  pnl.Position.X    := 0 ;
  pnl.Position.Y     := 0 ;
  pnl.Width   := w ;
  pnl.Height  := prnt.Height ;
  pnl.Align   := TAlignLayout.Client ;
  pnl.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom ] ;
  pnl.Name := 'pnlSection' ;

  lstFeatures.Add( pnl ) ;

  pnl := T_panelRenderer.Create( prnt, oParentWindow ) ;
  pnl.Visible := False ;
  pnl.Position.X    := 0 ;
  pnl.Position.Y     := 0 ;
  pnl.Width   := w ;
  pnl.Height  := prnt.Height ;
  pnl.Align   := TAlignLayout.Client ;
  pnl.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom ] ;
  pnl.Name := 'pnlRenderer' ;

  lstFeatures.Add( pnl ) ;

  if oParentWindow.MVC.HasMarker then begin
    pnl := T_panelMarker.Create( prnt, oParentWindow ) ;
    pnl.Visible := False ;
    pnl.Position.X    := 0 ;
    pnl.Position.Y     := 0 ;
    pnl.Width   := w ;
    pnl.Height  := prnt.Height ;
    pnl.Align   := TAlignLayout.Client ;
    pnl.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom ] ;
    pnl.Name := 'pnlMarker' ;

    lstFeatures.Add( pnl ) ;
  end ;

  if oParentWindow.MVC.HasLine then begin
    pnl := T_panelLine.Create( prnt, oParentWindow ) ;
    pnl.Visible := False ;
    pnl.Position.X    := 0 ;
    pnl.Position.Y     := 0 ;
    pnl.Width   := w ;
    pnl.Height  := prnt.Height ;
    pnl.Align   := TAlignLayout.Client ;
    pnl.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom ] ;
    pnl.Name := 'pnlLine' ;

    lstFeatures.Add( pnl ) ;
  end ;

  if oParentWindow.MVC.HasArea then begin
    pnl := T_panelArea.Create( prnt, oParentWindow ) ;
    pnl.Visible := False ;
    pnl.Position.X    := 0 ;
    pnl.Position.Y     := 0 ;
    pnl.Width   := w ;
    pnl.Height  := prnt.Height ;
    pnl.Align   := TAlignLayout.Client ;
    pnl.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom ] ;
    pnl.Name := 'pnlArea' ;

    lstFeatures.Add( pnl ) ;
  end ;

  pnl := T_panelLabel.Create( prnt, oParentWindow ) ;
  pnl.Visible := False ;
  pnl.Position.X    := 0 ;
  pnl.Position.Y     := 0 ;
  pnl.Width   := w ;
  pnl.Height  := prnt.Height ;
  pnl.Align   := TAlignLayout.Client ;
  pnl.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom ] ;
  pnl.Name := 'pnlLabel' ;

  lstFeatures.Add( pnl ) ;

  pnl := T_panelChart.Create( prnt, oParentWindow ) ;
  pnl.Visible := False ;
  pnl.Position.X    := 0 ;
  pnl.Position.Y     := 0 ;
  pnl.Width   := w ;
  pnl.Height  := prnt.Height ;
  pnl.Align   := TAlignLayout.Client ;
  pnl.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom ] ;
  pnl.Name := 'pnlChart' ;

  lstFeatures.Add( pnl ) ;

  pnl := T_panel3D.Create( prnt, oParentWindow ) ;
  pnl.Visible := False ;
  pnl.Position.X    := 0 ;
  pnl.Position.Y     := 0 ;
  pnl.Width   := w ;
  pnl.Height  := prnt.Height ;
  pnl.Align   := TAlignLayout.Client ;
  pnl.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom ] ;
  pnl.Name := 'pnl3D' ;

  lstFeatures.Add( pnl ) ;
end ;

procedure T_sectionContainer.initPixel ;
var
  pnl  : T_scrollablePanel ;
  prnt : TControl ;
  w     : Single ;
begin
  prnt := oParentWindow.pnlRight ;
  w := oParentWindow.pnlGeneral.Width ;

  pnl := T_panelSection.Create( prnt, oParentWindow ) ;
  pnl.Visible := False ;
  pnl.Position.X := 0 ;
  pnl.Position.Y := 0 ;
  pnl.Width   := w ;
  pnl.Height  := prnt.Height ;
  pnl.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom ] ;
  pnl.Align   := TAlignLayout.Client ;
  pnl.Name := 'pnlSection' ;

  lstFeatures.Add( pnl ) ;

  pnl := T_panelPixel.Create( prnt, oParentWindow ) ;
  pnl.Visible := False ;
  pnl.Position.X := 0 ;
  pnl.Position.Y := 0 ;
  pnl.Width   := w ;
  pnl.Height  := prnt.Height ;
  pnl.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom ] ;
  pnl.Align   := TAlignLayout.Client ;
  pnl.Name := 'pnlPixel' ;

  lstFeatures.Add( pnl ) ;


  pnl := T_panel3D.Create( prnt, oParentWindow ) ;
  pnl.Visible := False ;
  pnl.Position.X    := 0 ;
  pnl.Position.Y     := 0 ;
  pnl.Width   := w ;
  pnl.Height  := prnt.Height ;
  pnl.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom ] ;
  pnl.Name := 'pnl3D' ;

  lstFeatures.Add( pnl ) ;

end ;

procedure T_sectionContainer.initGrid ;
var
  pnl  : T_scrollablePanel ;
  prnt : TControl ;
begin
    if ( TGIS_LayerPixel( oParentWindow.MVC.Layer ).MinHeight >=
       TGIS_LayerPixel( oParentWindow.MVC.Layer ).MaxHeight )
  then
    oParentWindow.btnWizard.Enabled := False
  else
    oParentWindow.btnWizard.Enabled := True ;

  prnt := oParentWindow.pnlRight ;

  pnl := T_panelSection.Create( prnt, oParentWindow ) ;
  pnl.Visible := False ;
  pnl.Position.X    := 0 ;
  pnl.Position.Y     := 0 ;
  pnl.Width   := prnt.Width ;
  pnl.Height  := prnt.Height ;
  pnl.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom ] ;
  pnl.Align   := TAlignLayout.Client ;
  pnl.Name := 'pnlSection' ;


  lstFeatures.Add( pnl ) ;

  pnl := T_panelGrid.Create( prnt, oParentWindow ) ;
  pnl.Visible := False ;
  pnl.Position.X    := 0 ;
  pnl.Position.Y     := 0 ;
  pnl.Width   := prnt.Width ;
  pnl.Height  := prnt.Height ;
  pnl.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom ] ;
  pnl.Align   := TAlignLayout.Client ;
  pnl.Name := 'pnlGrid' ;


  lstFeatures.Add( pnl ) ;

  pnl := T_panel3D.Create( prnt, oParentWindow ) ;
  pnl.Visible := False ;
  pnl.Position.X    := 0 ;
  pnl.Position.Y     := 0 ;
  pnl.Width   := prnt.Width ;
  pnl.Height  := prnt.Height ;
  pnl.Align   := TAlignLayout.Client ;
  pnl.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight, TAnchorKind.akBottom ] ;
  pnl.Name := 'pnl3D' ;


  lstFeatures.Add( pnl ) ;
end ;

procedure T_sectionContainer.ReadSection;
begin
  lstFeatures.Items[0].Read ;
end;

procedure T_sectionContainer.Read ;
var
  i : Integer ;
begin
  for i := 0 to Count - 1 do
    lstFeatures.Items[i].Read ;
end ;

procedure T_sectionContainer.Write ;
var
  i : Integer ;
begin
  for i := 0 to Count - 1 do
    lstFeatures.Items[i].Write ;
end ;

function T_sectionContainer.FindPanel(
  const _text : String
) : T_scrollablePanel ;
var
  i : Integer ;
begin
  Result := nil ;

  for i := 0 to Count - 1 do begin
    if lstFeatures.Items[i].ItemText = _text then begin
      Result := lstFeatures.Items[i] ;
      break ;
    end ;
  end ;
end ;

function T_sectionContainer.fget_Count : Integer ;
begin
  Result := lstFeatures.Count ;
end ;

function T_sectionContainer.fget_Panel(
  const _i : Integer
) : T_scrollablePanel ;
begin
  Result := lstFeatures.Items[_i] ;
end ;
{$ENDREGION}

{$REGION 'T_panelGeneral'}
function T_panelGeneral.fget_HasPreview : Boolean ;
begin
  Result := mvc.HasPreview ;
end ;

procedure T_panelGeneral.init ;
begin
  inherited ;

  ItemText := _rsrcna( GIS_RS_LEGEND_PAG_GENERAL ) ;

  mvc := oParentWindow.MVC.General ;
  mvc.Callback := doCallback ;

  initParameters ;
  initInfo ;
end ;

procedure T_panelGeneral.initParameters ;
var
  t : Single ;
  lst : TStringList ;
  i : Integer ;
  anchors : TAnchors ;
  anchorsR : TAnchors ;
  bd : TBiDiMode ;
  str : String ;
  agg : TGIS_DynamicAggregatorAbstract ;
begin
  bd := oParentWindow.BiDiMode ;
  if bd = bdRightToLeft then begin
    anchors := [TAnchorKind.akRight, TAnchorKind.akTop] ;
    anchorsR := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  end else begin
    anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
    anchorsR := [TAnchorKind.akRight, TAnchorKind.akTop] ;
  end;

  gpbParameters := TGroupBox.Create( Self.Panel ) ;
  gpbParameters.Parent := Self.Panel ;
  gpbParameters.Position.Y := GROUPBOX_TOP ;
  gpbParameters.Height := 512 ;
  PlaceControl( bd, nil, gpbParameters, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  gpbParameters.Anchors := anchors ;
  gpbParameters.Text := _rsrcna( GIS_RS_LEGEND_TAB_PARAMS ) ;

  t := GROUPBOX_TOP_GAP ;

  lblPath := TLabel.Create( gpbParameters ) ;
  lblPath.Parent := gpbParameters ;
  lblPath.Position.Y := t ;
  PlaceControl( bd, nil, lblPath, LEFT_3COL_1, -1 ) ;
  lblPath.Text := _rsrcna( GIS_RS_LEGEND_PRM_PATH ) ;
  lblPath.FixSize ;

  t := t + lblPath.Height ;

  edtPath := TEdit.Create( gpbParameters ) ;
  edtPath.Parent := gpbParameters ;
  edtPath.Position.Y := t ;
  edtPath.FixSize ;
  PlaceControl( bd, nil, edtPath, LEFT_3COL_1, -1 ) ;
  edtPath.Text := '' ;
  edtPath.ReadOnly := True ;
  edtPath.KillFocusByReturn := True ;

  t := t + edtPath.Height + 8 ;

  lblName := TLabel.Create( gpbParameters ) ;
  lblName.Parent := gpbParameters ;
  lblName.Position.Y := t ;
  PlaceControl( bd, nil, lblName, LEFT_3COL_1, -1 ) ;
  lblName.Text := _rsrcna( GIS_RS_LEGEND_PRM_NAME ) ;
  lblName.FixSize ;

  t := t + lblName.Height ;

  edtName := TEdit.Create( gpbParameters ) ;
  edtName.Parent := gpbParameters ;
  edtName.Position.Y := t ;
  edtName.FixSize ;
  PlaceControl( bd, nil, edtName, LEFT_3COL_1, -1 ) ;
  edtName.Text := 'test' ;
  edtName.ReadOnly := True ;
  edtName.KillFocusByReturn := True ;

  t := t + edtName.Height + 8 ;

  lblCaption := TLabel.Create( gpbParameters ) ;
  lblCaption.Parent := gpbParameters ;
  lblCaption.Position.Y := t ;
  PlaceControl( bd, nil, lblCaption, LEFT_3COL_1, -1 ) ;
  lblCaption.Text := _rsrcna( GIS_RS_LEGEND_PRM_CAPTION ) ;
  lblCaption.FixSize ;

  t := t + lblName.Height ;

  edtCaption := TEdit.Create( gpbParameters ) ;
  edtCaption.Parent := gpbParameters ;
  edtCaption.Position.Y := t ;
  edtCaption.FixSize ;
  PlaceControl( bd, nil, edtCaption, LEFT_3COL_1, -1 ) ;
  edtCaption.Text := 'test' ;
  edtCaption.KillFocusByReturn := True ;

  t := t + edtCaption.Height + 8 ;

  lblCS := TLabel.Create( gpbParameters ) ;
  lblCS.Parent := gpbParameters ;
  lblCS.Position.Y := t ;
  PlaceControl( bd, nil, lblCS, LEFT_3COL_1,
                gpbParameters.Width - 2*LEFT_3COL_1 - WIDTH_3COL ) ;
  lblCS.Text := _rsrcna( GIS_RS_LEGEND_PRM_CS ) ;
  lblCS.FixSize ;

  t := t + lblCS.Height ;

  btnCS := TButton.Create( gpbParameters ) ;
  btnCS.Parent := gpbParameters ;
  btnCS.Anchors := anchorsR ;
  btnCS.Position.Y := t  ;
  PlaceControl( bd, nil, btnCS, -LEFT_3COL_1, WIDTH_3COL - 8 ) ;
  btnCS.Text := _rsrcna( GIS_RS_LEGEND_PRM_SELECT ) ;
  btnCS.OnClick := doCSClick ;
  {$IFDEF NEXTGEN}
  btnCS.Scale.X := 0.8 ;
  btnCS.Scale.Y := 0.8 ;
  {$ENDIF}

  edtCS := TEdit.Create( gpbParameters ) ;
  edtCS.Parent := gpbParameters ;
  edtCS.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akTop] ;
  edtCS.Position.Y := t ;
  edtCS.FixSize ;
  PlaceControl( bd, nil, edtCS, LEFT_3COL_1,
                gpbParameters.Width - 2*LEFT_3COL_1 - WIDTH_3COL ) ;
  edtCS.Text := 'WGS 84 (epsg:4326)' ;
  edtCS.ReadOnly := True ;
  edtCS.KillFocusByReturn := True ;

  t := t + edtCS.Height + GROUPBOX_BOTTOM_GAP ;

  gpbParameters.Height := t ;

  gpbPainting := TGroupBox.Create( Self.Panel ) ;
  gpbPainting.Parent := Self.Panel ;
  gpbPainting.Position.Y  := gpbParameters.Position.Y + gpbParameters.Height + GROUPBOX_GAP ; ;
  gpbPainting.Height := 512 ;
  PlaceControl( bd, nil, gpbPainting, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  gpbPainting.Anchors := anchors ;
  gpbPainting.Text := _rsrcna( GIS_RS_BUSY_PAINT ) ;

  t := GROUPBOX_TOP_GAP ;

  ckbBasemap := TCheckBox.Create( gpbPainting ) ;
  ckbBasemap.Parent := gpbPainting ;
  ckbBasemap.Position.Y := t ;
  PlaceControl( bd, nil, ckbBasemap, LEFT_3COL_1, LEFT_3COL_3 - LEFT_3COL_1 - 12 ) ;
  ckbBasemap.Text := _rsrcna( GIS_RS_LEGEND_PRM_BASEMAP ) ;

  t := t + ckbBasemap.Height + 8 ;

  ckbCachedPaint := TCheckBox.Create( gpbPainting ) ;
  ckbCachedPaint.Parent := gpbPainting ;
  ckbCachedPaint.Position.Y := t ;
  PlaceControl( bd, nil, ckbCachedPaint, LEFT_3COL_1, LEFT_3COL_3 - LEFT_3COL_1 - 12 ) ;
  ckbCachedPaint.Text := _rsrcna( GIS_RS_LEGEND_PRM_CACHEDPAINT ) ;

  t := t + ckbCachedPaint.Height + 8 ;

  ckbIgnoreShapeParams := TCheckBox.Create( gpbPainting ) ;
  ckbIgnoreShapeParams.Parent := gpbPainting ;
  ckbIgnoreShapeParams.Position.Y := t ;
  PlaceControl( bd, nil, ckbIgnoreShapeParams, LEFT_3COL_1, LEFT_3COL_3 - LEFT_3COL_1 - 12 ) ;
  ckbIgnoreShapeParams.Text := _rsrcna( GIS_RS_LEGEND_PRM_IGNORESHAPEPARAMS ) ;
  ckbIgnoreShapeParams.Enabled := oParentWindow.MVC.Layer is TGIS_LayerVector ;

  t := t + ckbIgnoreShapeParams.Height + 8 ;

  ckbMultipassRendering := TCheckBox.Create( gpbPainting ) ;
  ckbMultipassRendering.Parent := gpbPainting ;
  ckbMultipassRendering.Position.Y := t ;
  PlaceControl( bd, nil, ckbMultipassRendering, LEFT_3COL_1, LEFT_3COL_3 - LEFT_3COL_1 - 12 ) ;
  ckbMultipassRendering.Text := _rsrcna( GIS_RS_LEGEND_PRM_MULTIPASSRENDERING ) ;
  ckbMultipassRendering.Enabled := oParentWindow.MVC.Layer is TGIS_LayerVector ;

  t := GROUPBOX_TOP_GAP ;

  lblTransparency := TLabel.Create( gpbPainting ) ;
  lblTransparency.Parent := gpbPainting ;
  lblTransparency.Position.Y := t ;
  PlaceControl( bd, nil, lblTransparency, LEFT_3COL_3, WIDTH_3COL - 24 ) ;
  lblTransparency.Text := _rsrcna( GIS_RS_LEGEND_PRM_TRANSPARENCY ) ;
  lblTransparency.FixSize ;

  t := t + lblTransparency.Height ;

  lst := TStringList.Create ;
  try
    i := 0 ;
    while i <= 100 do begin
      lst.Add( IntToStr(i) ) ;
      inc( i, 10 ) ;
    end ;
    cmbTransparency := TComboEdit.Create( gpbPainting ) ;
    cmbTransparency.Parent := gpbPainting ;
    cmbTransparency.Position.Y := t ;
    PlaceControl( bd, nil, cmbTransparency, LEFT_3COL_3, WIDTH_3COL - 24 ) ;
    cmbTransparency.Items.BeginUpdate ;
    cmbTransparency.ItemIndex := -1 ;
    cmbTransparency.Items.Assign( lst ) ;
    cmbTransparency.Items.EndUpdate ;
    cmbTransparency.KillFocusByReturn := True ;

    vvcmbTransparency := TGIS_ValueValidatorComboBoxHelper.Create( cmbTransparency ) ;
    vvcmbTransparency.MinVal := 0 ;
    vvcmbTransparency.MaxVal := 100 ;

    t := t + cmbTransparency.Height + 8 ;

    if oParentWindow.MVC.IsPixel then begin
      lblInterpretation := TLabel.Create( gpbPainting ) ;
      lblInterpretation.Parent := gpbPainting ;
      lblInterpretation.Position.Y := t ;
      PlaceControl( bd, nil, lblInterpretation, LEFT_3COL_3, WIDTH_3COL - 24 ) ;
      lblInterpretation.Text := _rsrcna( GIS_RS_LEGEND_PRM_INTERPRETATION ) ;
      lblInterpretation.FixSize ;
      lblInterpretation.Visible := True ;

      t := t + lblInterpretation.Height ;

      cmbInterpretation := TComboBox.Create( gpbPainting ) ;
      cmbInterpretation.Parent := gpbPainting ;
      cmbInterpretation.Position.Y := t ;
      PlaceControl( bd, nil, cmbInterpretation, LEFT_3COL_3, WIDTH_3COL - 24 ) ;
      cmbInterpretation.Items.BeginUpdate ;
      cmbInterpretation.ItemIndex := -1 ;
      cmbInterpretation.Items.Add( _rsrc( GIS_RS_LEGEND_PRM_DEFAULT ) ) ;
      cmbInterpretation.Items.Add( _rsrc( GIS_RS_LEGEND_PAG_PIXEL   ) ) ;
      cmbInterpretation.Items.Add( _rsrc( GIS_RS_LEGEND_PAG_GRID    ) ) ;
      cmbInterpretation.Items.EndUpdate ;
      cmbInterpretation.Visible := True ;
    end ;

    t := ckbMultipassRendering.Position.Y + ckbMultipassRendering.Height ;

    if oParentWindow.MVC.Layer is TGIS_LayerVector then begin
      t := t + 8 ;

      lblScope := TLabel.Create( gpbPainting ) ;
      lblScope.Parent := gpbPainting ;
      lblScope.Position.Y := t ;
      PlaceControl( bd, nil, lblScope, LEFT_3COL_1, -1 ) ;
      lblScope.Text := _rsrcna( GIS_RS_LEGEND_PRM_SCOPE ) ;
      lblScope.TextSettings.WordWrap := False ;
      lblScope.TextSettings.Trimming := TTextTrimming.None ;
      lblScope.Width := WIDTH_2COL ;
      lblScope.FixSize ;

      t := t + lblScope.Height ;

      cmbScope := TComboEdit.Create( gpbPainting ) ;
      cmbScope.Parent := gpbPainting ;
      cmbScope.Position.Y := t ;
      PlaceControl( bd, nil, cmbScope, LEFT_3COL_1, -1 ) ;
      cmbScope.ItemIndex := -1 ;
      oParentWindow.fillComboBoxWithFields( cmbScope ) ;
      cmbScope.KillFocusByReturn := True ;

      t := cmbScope.Position.Y + cmbScope.Height ;
    end
    else
      t := ckbMultipassRendering.Position.Y + ckbMultipassRendering.Height ;

    t := t + 8 ;

    ckbUseConfig := TCheckBox.Create( gpbPainting ) ;
    ckbUseConfig.Parent := gpbPainting ;
    ckbUseConfig.Position.Y := t ;
    PlaceControl( bd, nil, ckbUseConfig, LEFT_3COL_1, WIDTH_NARROW ) ;
    ckbUseConfig.Text := _rsrc( GIS_RS_LEGEND_PRM_USECONFIG ) ;

    t := t + ckbUseConfig.Height ;

    gpbPainting.Height := t + GROUPBOX_BOTTOM_GAP ;
  finally
    FreeObject( lst ) ;
  end ;

  if oParentWindow.MVC.IsVector then begin
    gpbAggregation := TGroupBox.Create( Self.Panel ) ;
    gpbAggregation.Parent := Self.Panel ;
    gpbAggregation.Position.Y  := gpbPainting.Position.Y + gpbPainting.Height + GROUPBOX_GAP ;
    gpbAggregation.Height := 512 ;
    PlaceControl( bd, nil, gpbAggregation, 8, WIDTH_NORMAL ) ;
    gpbAggregation.Anchors := anchors ;
    gpbAggregation.Text := _rsrc( GIS_RS_LEGEND_PRM_AGGREGATION ) ;

    t := GROUPBOX_TOP_GAP ;

    lblAggMethod := TLabel.Create( gpbAggregation ) ;
    lblAggMethod.Parent := gpbAggregation ;
    lblAggMethod.Position.Y := t ;
    PlaceControl( bd, nil, lblAggMethod, LEFT_3COL_1, lblAggMethod.Width ) ;
    lblAggMethod.Text := _rsrc( GIS_RS_LEGEND_PRM_AGGR_METHOD ) ;
    lblAggMethod.TextSettings.WordWrap := False ;
    lblAggMethod.TextSettings.Trimming := TTextTrimming.None ;
    lblAggMethod.FixSize ;

    t := t + lblAggMethod.Height ;

    cmbAggMethod := TComboBox.Create( gpbAggregation ) ;
    cmbAggMethod.Parent := gpbAggregation ;
    cmbAggMethod.Position.Y := t ;
    PlaceControl( bd, nil, cmbAggMethod, LEFT_3COL_1, -1 ) ;
    cmbAggMethod.Items.BeginUpdate ;
    cmbAggMethod.ItemIndex := -1 ;
    cmbAggMethod.Items.Add( _rsrc( GIS_RS_LEGEND_PRM_OFF ) ) ;

    for str in TGIS_DynamicAggregatorFactory.Names do begin
      agg := TGIS_DynamicAggregatorFactory.CreateInstance(
                str, TGIS_LayerVector( oParentWindow.MVC.Layer )
             ) ;
      try
        cmbAggMethod.Items.Add( agg.Caption ) ;
      finally
        FreeObject( agg ) ;
      end;
    end;

    cmbAggMethod.Items.EndUpdate ;
    cmbAggMethod.Visible := True ;
    cmbAggMethod.ItemIndex := 0 ;
    cmbAggMethod.OnChange := doAggregateChange ;

    t := t + cmbAggMethod.Height + 8 ;

    lblAggRadius := TLabel.Create( gpbAggregation ) ;
    lblAggRadius.Parent := gpbAggregation ;
    lblAggRadius.Position.Y := t ;
    PlaceControl( bd, nil, lblAggRadius, LEFT_3COL_1, lblAggRadius.Width ) ;
    lblAggRadius.Text := _rsrc( GIS_RS_LEGEND_PRM_AGGR_RADIUS ) ;
    lblAggRadius.TextSettings.WordWrap := False ;
    lblAggRadius.TextSettings.Trimming := TTextTrimming.None ;
    lblAggRadius.FixSize ;

    lblAggThreshold := TLabel.Create( gpbAggregation ) ;
    lblAggThreshold.Parent := gpbAggregation ;
    lblAggThreshold.Position.Y := t ;
    PlaceControl( bd, nil, lblAggThreshold, LEFT_3COL_2, lblAggThreshold.Width ) ;
    lblAggThreshold.Text := _rsrc( GIS_RS_LEGEND_PRM_AGGR_THRESHOLD ) ;
    lblAggThreshold.TextSettings.WordWrap := False ;
    lblAggThreshold.TextSettings.Trimming := TTextTrimming.None ;
    lblAggThreshold.FixSize ;

    t := t + lblAggThreshold.Height ;

    cmbAggRadius := TGIS_SizeComboBox.Create( gpbAggregation ) ;
    cmbAggRadius.Parent := gpbAggregation ;
    cmbAggRadius.Position.Y := t ;
    PlaceControl( bd, nil, cmbAggRadius, LEFT_3COL_1, WIDTH_3COL ) ;
    cmbAggRadius.Anchors := anchors ;
    cmbAggRadius.FillAggregation ;
    cmbAggRadius.CustomEvent := doCustomSize ;

    edtAggThreshold := TEdit.Create( gpbAggregation ) ;
    edtAggThreshold.Parent := gpbAggregation ;
    edtAggThreshold.Position.Y := t ;
    PlaceControl( bd, nil, edtAggThreshold, LEFT_3COL_2, WIDTH_3COL ) ;
    edtAggThreshold.OnChange := doControlChange ;

    vvedtAggThreshold := TGIS_ValueValidatorEditHelper.Create( edtAggThreshold ) ;
    vvedtAggThreshold.MinVal := 0 ;
    vvedtAggThreshold.MaxVal := GIS_MAX_INTEGER ;
    vvedtAggThreshold.Precision := 0 ;

    t := t + edtAggThreshold.Height + 16 ;

    gpbAggregation.Height := t ;

  end ;
end ;

procedure T_panelGeneral.initInfo ;
var
  t : Single ;
  anchors : TAnchors ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;
  if bd = bdRightToLeft then
    anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;

  gpbInfo := TGroupBox.Create( Self.Panel ) ;
  gpbInfo.Parent := Self.Panel ;
  if oParentWindow.MVC.IsVector then
    gpbInfo.Position.Y := gpbAggregation.Position.Y + gpbAggregation.Height + GROUPBOX_GAP
  else
    gpbInfo.Position.Y  := gpbPainting.Position.Y + gpbPainting.Height + GROUPBOX_GAP ;

  gpbInfo.Height := 512 ;
  PlaceControl( bd, nil, gpbInfo, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  gpbInfo.Anchors := anchors ;
  gpbInfo.Text := _rsrcna( GIS_RS_LEGEND_TAB_INFO ) ;

  t := GROUPBOX_TOP_GAP ;

  lblFileInformation := TLabel.Create( gpbInfo ) ;
  lblFileInformation.Parent := gpbInfo ;
  lblFileInformation.Position.Y := t ;
  PlaceControl( bd, nil, lblFileInformation, LEFT_3COL_1, -1 ) ;
  lblFileInformation.Text := _rsrcna( GIS_RS_LEGEND_PRM_INFO ) ;
  lblFileInformation.FixSize ;

  t := t + lblFileInformation.Height ;

  txbFileInformation := TMemo.Create( gpbInfo ) ;
  txbFileInformation.Parent := gpbInfo ;
  txbFileInformation.Position.Y := t ;
  txbFileInformation.Height := 64 ;
  {$IFDEF GIS_MOBILE}
  txbFileInformation.Scale.X := 0.8 ;
  txbFileInformation.Scale.Y := 0.8 ;
  {$ENDIF}
  PlaceControl( bd, nil, txbFileInformation, LEFT_3COL_1, -1 ) ;
  {$IFDEF GIS_MOBILE}
  txbFileInformation.Width := txbFileInformation.Width / 0.8 ;
  {$ENDIF}
  txbFileInformation.Text := '' ;
  txbFileInformation.ReadOnly := True ;
  txbFileInformation.WordWrap := True ;
  txbFileInformation.ShowScrollBars := True ;

  t := t + txbFileInformation.Height + 8 ;

  lblUserComments := TLabel.Create( gpbInfo ) ;
  lblUserComments.Parent := gpbInfo ;
  lblUserComments.Position.Y := t ;
  PlaceControl( bd, nil, lblUserComments, LEFT_3COL_1, -1 ) ;
  lblUserComments.Text := _rsrcna( GIS_RS_LEGEND_PRM_COMMENTS ) ;
  lblUserComments.TextSettings.WordWrap := False ;
  lblUserComments.TextSettings.Trimming := TTextTrimming.None ;
  lblUserComments.Width := WIDTH_2COL ;
  lblUserComments.FixSize ;

  t := t + lblUserComments.Height ;

  txbUserComments := TMemo.Create( gpbInfo ) ;
  txbUserComments.Parent := gpbInfo ;
  txbUserComments.Position.Y := t ;
  txbUserComments.Height := 64 ;
  {$IFDEF GIS_MOBILE}
  txbUserComments.Scale.X := 0.8 ;
  txbUserComments.Scale.Y := 0.8 ;
  {$ENDIF}
  PlaceControl( bd, nil, txbUserComments, LEFT_3COL_1, -1 ) ;
  {$IFDEF GIS_MOBILE}
  txbUserComments.Width := txbUserComments.Width / 0.8 ;
  {$ENDIF}
  txbUserComments.WordWrap := True ;
  txbUserComments.ShowScrollBars := True ;

  t := t + txbUserComments.Height + 8 ;

  if oParentWindow.MVC.Layer is TGIS_LayerVector then begin
    lblCodePage := TLabel.Create( gpbInfo ) ;
    lblCodePage.Parent := gpbInfo ;
    lblCodePage.Position.Y := t ;
    PlaceControl( bd, nil, lblCodePage, LEFT_3COL_1, WIDTH_3COL ) ;
    lblCodePage.Text := _rsrcna( GIS_RS_LEGEND_PRM_CODEPAGE ) ;
    lblCodePage.FixSize ;

    t := t + lblName.Height ;

    edtCodePage := TEdit.Create( gpbInfo ) ;
    edtCodePage.Parent := gpbInfo ;
    edtCodePage.Position.Y := t ;
    edtCodePage.FixSize ;
    PlaceControl( bd, nil, edtCodePage, LEFT_3COL_1, WIDTH_3COL ) ;
    edtCodePage.KillFocusByReturn := True ;

    vvedtCodePage := TGIS_ValueValidatorEditHelper.Create( edtCodePage ) ;
    vvedtCodePage.MinVal := 0 ;
    vvedtCodePage.MaxVal := 65535 ;

    t := t + edtCodePage.Height + GROUPBOX_BOTTOM_GAP ;
  end ;

  gpbInfo.Height := t ;

end ;

procedure T_panelGeneral.Read ;
var
  idx : Integer ;
begin
  lockUpdates ;
  try
    edtPath.Text := mvc.Path ;
    edtName.Text := mvc.Name ;
    edtCaption.Text := mvc.Caption ;
    edtCS.Text := mvc.CS.FriendlyName ;
    ckbBasemap.IsChecked := mvc.Basemap ;
    ckbCachedPaint.IsChecked := mvc.CachedPaint ;
    ckbIgnoreShapeParams.IsChecked := mvc.IgnoreShapeParams ;
    ckbMultipassrendering.IsChecked := mvc.MultipassRendering ;
    vvcmbTransparency.Value := mvc.Transparency ;
    txbFileInformation.Text := mvc.FileInformation ;
    txbUserComments.Text := mvc.Comments ;

    if oParentWindow.MVC.Layer is TGIS_LayerVector then begin
      cmbScope.Text := mvc.Scope ;
      vvedtCodePage.Value := mvc.CodePage ;


      idx := TGIS_DynamicAggregatorFactory.Names.IndexOf( mvc.AggregationMethod ) ;
      if idx > -1 then
        cmbAggMethod.ItemIndex := idx + 1
      else
        cmbAggMethod.ItemIndex := 0 ;
      cmbAggRadius.Value       := mvc.AggregationRadius ;
      vvedtAggThreshold.Value  := mvc.AggregationThreshold ;
    end
    else if oParentWindow.MVC.IsGrid then begin
      cmbInterpretation.ItemIndex := mvc.Interpretation ;
    end
    else if oParentWindow.MVC.IsPixel then
      cmbInterpretation.ItemIndex := mvc.Interpretation ;

    ckbUseConfig.IsChecked := mvc.UseConfig ;
  finally
    unlockUpdates ;
  end;
  if oParentWindow.MVC.Layer is TGIS_LayerVector then begin
    if cmbAggMethod.ItemIndex = 0 then
      mvc.DoAggregateChange ;
  end ;
end ;

procedure T_panelGeneral.Write ;
begin
  mvc.Caption := edtCaption.Text ;
  mvc.Basemap := ckbBasemap.IsChecked ;
  mvc.CachedPaint := ckbCachedPaint.IsChecked ;
  mvc.IgnoreShapeParams := ckbIgnoreShapeParams.IsChecked ;
  mvc.Multipassrendering := ckbMultipassRendering.IsChecked ;
  mvc.Transparency := FloorS( vvcmbTransparency.Value ) ;
  mvc.Comments := txbUserComments.Text ;
  if oParentWindow.MVC.Layer is TGIS_LayerVector then begin
    mvc.Scope := cmbScope.Text ;
    mvc.CodePage := FloorS( vvedtCodePage.Value ) ;

    if cmbAggMethod.ItemIndex > 0 then
      mvc.AggregationMethod := TGIS_DynamicAggregatorFactory.Names[ cmbAggMethod.ItemIndex-1 ]
    else
      mvc.AggregationMethod  := cmbAggMethod.Items[cmbAggMethod.ItemIndex] ;
    mvc.AggregationRadius    := cmbAggRadius.Value ;
    mvc.AggregationThreshold := FloorS( vvedtAggThreshold.Value ) ;
  end
  else if oParentWindow.MVC.IsGrid then begin
    mvc.Interpretation := cmbInterpretation.ItemIndex ;
  end
  else if oParentWindow.MVC.IsPixel then
    mvc.Interpretation := cmbInterpretation.ItemIndex ;

  mvc.UseConfig := ckbUseConfig.IsChecked ;
end ;

procedure T_panelGeneral.doCallback(
  _sender : TObject ;
  _code   : Integer
) ;

  procedure do_cs ;
  var
    dlg  : TGIS_PvlControlCSSystem ;
    ctx  : TGIS_PvlContext ;
    proc : TGIS_Proc ;
    cs   : TGIS_CSCoordinateSystem ;
  begin
    ctx := TGIS_PvlContext.Create( Self );
    dlg := TGIS_PvlControlCSSystem.Create( ctx ) ;

    cs := mvc.CS ;

    proc := {$IFDEF OXYGENE}TGIS_Proc.create({$ENDIF}
      procedure( _modal_result : TGIS_PvlModalResult )
      begin
        if _modal_result = TGIS_PvlModalResult.OK then begin
          cs := dlg.CS ;
        end;
      end
    {$IFDEF OXYGENE}){$ENDIF};
    dlg.Execute( mvc.CS,
                  oParentWindow.pOnHelp,
                  proc
                ) ;


    mvc.CS := cs ;
    edtCS.Text := cs.FriendlyName ;
  end ;

  procedure do_agg ;
  var
    agg   : TGIS_DynamicAggregatorAbstract ;
    aname : String ;
  begin
    if cmbAggMethod.ItemIndex > 0 then begin
      lblAggThreshold.Enabled  := True ;
      edtAggThreshold.Enabled  := True ;
      lblAggRadius.Enabled      := True ;
      cmbAggRadius.Enabled      := True ;

      aname := TGIS_DynamicAggregatorFactory.Names[ cmbAggMethod.ItemIndex-1 ] ;
      if cmbAggMethod.Tag <> cmbAggMethod.ItemIndex then begin
        agg := TGIS_DynamicAggregatorFactory.CreateInstance(
                aname,
                TGIS_LayerVector( oParentWindow.MVC.Layer )
               ) ;
        try
          if assigned( agg ) then begin
            cmbAggRadius.Value      := agg.RadiusAsText ;
            vvedtAggThreshold.Value := agg.Threshold ;
          end ;
        finally
          FreeObject( agg ) ;
        end ;
      end ;
    end
    else begin
      lblAggThreshold.Enabled := False ;
      edtAggThreshold.Enabled := False ;
      lblAggRadius.Enabled    := False ;
      cmbAggRadius.Enabled    := False ;
    end;
    cmbAggMethod.Tag := cmbAggMethod.ItemIndex ;
  end;

begin
  case _code of
    1 : do_cs ;
    2 :  ;
    3 : do_agg ;
  end ;
end ;

procedure T_panelGeneral.doCSClick(
  _sender : TObject
) ;
begin
  mvc.DoCSClick ;
end ;

procedure T_panelGeneral.doControlChange(
  _sender: TObject
) ;
begin
  mvc.DoControlChange ;
end ;

procedure T_panelGeneral.doAggregateChange(
  _sender: TObject
) ;
begin
  mvc.DoAggregateChange ;
end ;


{$ENDREGION}

{$REGION 'T_Panel3D'}
function T_Panel3D.fget_HasPreview : Boolean ;
begin
  Result := mvc.HasPreview ;
end ;


procedure T_Panel3D.init ;
var
  t : Single ;
  bd : TBiDiMode ;
begin
  inherited ;

  ItemText := _rsrcna( GIS_RS_LEGEND_PAG_3D ) ;

  mvc := oParentWindow.MVC.View3D ;
  mvc.Callback := doCallback ;

  hasDEM := oParentWindow.MVC.IsVector or oParentWindow.MVC.IsGrid ;

  bd := oParentWindow.BiDiMode ;

  t := 16 ;

  rbnAs2D := TRadioButton.Create( Self.Panel ) ;
  rbnAs2D.GroupName := Self.ClassName ;
  rbnAs2D.Parent := Self.Panel ;
  rbnAs2D.Position.Y := t ;
  PlaceControl( bd, nil, rbnAs2D, LEFT_3COL_1, 256 ) ;
  if bd = bdRightToLeft then
    rbnAs2D.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    rbnAs2D.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  rbnAs2D.Text := _rsrcna( GIS_RS_LEGEND_PRM_TREAT_AS_2D ) ;
  rbnAs2D.IsChecked := True ;
  rbnAs2D.OnClick := doAs2DClick ;
  rbnAs2D.FixSize ;

  t := t + rbnAs2D.Height + 8 ;

  if hasDEM then begin
    rbnAsDEM := TRadioButton.Create( Self.Panel ) ;
    rbnAsDEM.GroupName := Self.ClassName ;
    rbnAsDEM.Parent := Self.Panel ;
    rbnAsDEM.Position.Y := t ;
    PlaceControl( bd, nil, rbnAsDEM, LEFT_3COL_1, 286 ) ;
    if bd = bdRightToLeft then
      rbnAsDEM.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
    else
      rbnAsDEM.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
    rbnAsDEM.Text := _rsrcna( GIS_RS_LEGEND_PRM_TREAT_AS_DEM ) ;
    rbnAsDEM.OnClick := doAsDEMClick ;
    rbnAsDEM.TextSettings.WordWrap := False ;
    rbnAsDEM.TextSettings.Trimming := TTextTrimming.None ;
    rbnAsDEM.FixSize ;

    t := t + rbnAsDEM.Height + 8 ;
  end;

  rbnAs3D := TRadioButton.Create( Self.Panel ) ;
  rbnAs3D.GroupName := Self.ClassName ;
  rbnAs3D.Parent := Self.Panel ;
  rbnAs3D.Position.Y := t ;
  PlaceControl( bd, nil, rbnAs3D, LEFT_3COL_1, 256 ) ;
  if bd = bdRightToLeft then
    rbnAs3D.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    rbnAs3D.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  rbnAs3D.Text := _rsrcna( GIS_RS_LEGEND_PRM_TREAT_AS_3DSHAPES ) ;
  rbnAs3D.OnClick := doAs3DClick ;
  rbnAs3D.FixSize ;

  t := t + rbnAs3D.Height + 16 ;

  lblNormalizedZ := TLabel.Create( Self.Panel ) ;
  lblNormalizedZ.Parent := Self.Panel ;
  lblNormalizedZ.Position.Y := t ;
  PlaceControl( bd, nil, lblNormalizedZ, 2*LEFT_3COL_1, lblNormalizedZ.Width ) ;
  if bd = bdRightToLeft then
    lblNormalizedZ.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    lblNormalizedZ.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  lblNormalizedZ.Text := _rsrc( GIS_RS_LEGEND_PRM_NORMALIZED_Z_LBL ) ;

  if oParentWindow.MVC.IsVector then begin
    lblNormalizedM := TLabel.Create( Self.Panel ) ;
    lblNormalizedM.Parent := Self.Panel ;
    lblNormalizedM.Position.Y := t ;
    PlaceControl( bd, nil, lblNormalizedM, LEFT_3COL_1 + LEFT_2COL_2, lblNormalizedM.Width ) ;
  if bd = bdRightToLeft then
    lblNormalizedM.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    lblNormalizedM.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
    lblNormalizedM.Text := _rsrc( GIS_RS_LEGEND_PRM_NORMALIZED_M_LBL ) ;
  end;

  t := t + lblNormalizedZ.Height + 8 ;

  cmbNormalizedZ := TComboBox.Create( Self.Panel ) ;
  cmbNormalizedZ.Parent := Self.Panel ;
  cmbNormalizedZ.Position.Y := t ;
  PlaceControl( bd, nil, cmbNormalizedZ, 2*LEFT_3COL_1, WIDTH_3COL ) ;
  if bd = bdRightToLeft then
    cmbNormalizedZ.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    cmbNormalizedZ.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  cmbNormalizedZ.Items.BeginUpdate ;
  cmbNormalizedZ.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_NORMALIZED_Z_OFF ) ) ;
  cmbNormalizedZ.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_NORMALIZED_Z_MAX ) ) ;
  cmbNormalizedZ.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_NORMALIZED_Z_RANGE ) ) ;
  cmbNormalizedZ.ItemIndex := 0 ;
  cmbNormalizedZ.Items.EndUpdate ;

  if oParentWindow.MVC.IsVector then begin
    cmbNormalizedM := TComboBox.Create( Self.Panel ) ;
    cmbNormalizedM.Parent := Self.Panel ;
    cmbNormalizedM.Position.Y := t ;
    PlaceControl( bd, nil, cmbNormalizedM, LEFT_3COL_1 + LEFT_2COL_2,
                  WIDTH_3COL ) ;
    if bd = bdRightToLeft then
      cmbNormalizedM.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
    else
      cmbNormalizedM.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
    cmbNormalizedM.Items.BeginUpdate ;
    cmbNormalizedM.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_NORMALIZED_M_OFF ) ) ;
    cmbNormalizedM.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_NORMALIZED_M_MAX ) ) ;
    cmbNormalizedM.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_NORMALIZED_M_RANGE ) ) ;
    cmbNormalizedM.ItemIndex := 0 ;
    cmbNormalizedM.Items.EndUpdate ;
  end ;

  t := t + cmbNormalizedZ.Height + 8 ;

  lblScaleZ := TLabel.Create( Self.Panel ) ;
  lblScaleZ.Parent := Self.Panel ;
  lblScaleZ.Position.Y := t ;
  PlaceControl( bd, nil, lblScaleZ, 2*LEFT_3COL_1, WIDTH_3COL ) ;
  if bd = bdRightToLeft then
    lblScaleZ.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    lblScaleZ.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  lblScaleZ.Text := _rsrcna( GIS_RS_LEGEND_PRM_SCALE_Z ) ;
  lblScaleZ.FixSize ;

  if oParentWindow.MVC.IsVector then begin
    lblScaleM := TLabel.Create( Self.Panel ) ;
    lblScaleM.Parent := Self.Panel ;
    lblScaleM.Position.Y := t ;
    PlaceControl( bd, nil, lblScaleM, LEFT_3COL_1 + LEFT_2COL_2, WIDTH_3COL ) ;
    if bd = bdRightToLeft then
      lblScaleM.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
    else
      lblScaleM.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
    lblScaleM.Text := _rsrcna( GIS_RS_LEGEND_PRM_SCALE_M ) ;
    lblScaleM.FixSize ;
  end ;

  t := t + lblScaleZ.Height ;

  speScaleZ := TEdit.Create( Self.Panel ) ;
  speScaleZ.Parent := Self.Panel ;
  speScaleZ.Position.Y := t ;
  PlaceControl( bd, nil, speScaleZ, 2*LEFT_3COL_1, WIDTH_3COL ) ;
  if bd = bdRightToLeft then
    speScaleZ.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    speScaleZ.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;

  vvspeScaleZ := TGIS_ValueValidatorEditHelper.Create( speScaleZ ) ;
  vvspeScaleZ.MinVal := -999.0 ;
  vvspeScaleZ.MaxVal :=  999.0 ;

  if oParentWindow.MVC.IsVector then begin
    speScaleM := TEdit.Create( Self.Panel ) ;
    speScaleM.Parent := Self.Panel ;
    speScaleM.Position.Y := t ;
    PlaceControl( bd, nil, speScaleM, LEFT_3COL_1 + LEFT_2COL_2, WIDTH_3COL ) ;
    if bd = bdRightToLeft then
      speScaleM.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
    else
      speScaleM.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;

    vvspeScaleM := TGIS_ValueValidatorEditHelper.Create( speScaleM ) ;
    vvspeScaleM.MinVal := -999.0 ;
    vvspeScaleM.MaxVal :=  999.0 ;
  end ;

  t := t + speScaleZ.Height + 16 ;

  lblFalseZ := TLabel.Create( Self.Panel ) ;
  lblFalseZ.Parent := Self.Panel ;
  lblFalseZ.Position.Y := t ;
  PlaceControl( bd, nil, lblFalseZ, 2*LEFT_3COL_1, WIDTH_3COL ) ;
  if bd = bdRightToLeft then
    lblFalseZ.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    lblFalseZ.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  lblFalseZ.Text := _rsrcna( GIS_RS_LEGEND_PRM_FALSE_Z ) ;
  lblFalseZ.FixSize;

  if oParentWindow.MVC.IsVector then begin
    lblFalseM := TLabel.Create( Self.Panel ) ;
    lblFalseM.Parent := Self.Panel ;
    lblFalseM.Position.X := LEFT_3COL_1 + LEFT_2COL_2 ;
    lblFalseM.Position.Y := t ;
    PlaceControl( bd, nil, lblFalseM, LEFT_3COL_1 + LEFT_2COL_2, WIDTH_3COL ) ;
    if bd = bdRightToLeft then
      lblFalseM.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
    else
      lblFalseM.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
    lblFalseM.Text := _rsrcna( GIS_RS_LEGEND_PRM_FALSE_M ) ;
    lblFalseM.FixSize ;
  end ;

  t := t + lblFalseZ.Height ;

  cmbFalseZ := TGIS_SizeComboBox.Create( Self.Panel ) ;
  cmbFalseZ.Parent := Self.Panel ;
  cmbFalseZ.Position.Y := t ;
  PlaceControl( bd, nil, cmbFalseZ, 2*LEFT_3COL_1, WIDTH_3COL ) ;
  if bd = bdRightToLeft then
    cmbFalseZ.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    cmbFalseZ.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  cmbFalseZ.FillRealWorldUnits( oParentWindow.MVC.IsVector ) ;
  cmbFalseZ.CustomEvent := doCustomRWUnits ;

  if not oParentWindow.MVC.IsVector then
    exit ;

  cmbFalseM := TGIS_SizeComboBox.Create( Self.Panel ) ;
  cmbFalseM.Parent := Self.Panel ;
  cmbFalseM.Position.Y := t ;
  PlaceControl( bd, nil, cmbFalseM, LEFT_3COL_1 + LEFT_2COL_2, WIDTH_3COL ) ;
  if bd = bdRightToLeft then
    cmbFalseM.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    cmbFalseM.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  cmbFalseM.FillRealWorldUnits( oParentWindow.MVC.IsVector ) ;
  cmbFalseM.CustomEvent := doCustomRWUnits ;

  t := t + cmbFalseZ.Height + 16 ;

  lblAdjustZ := TLabel.Create( Self.Panel ) ;
  lblAdjustZ.Parent := Self.Panel ;
  lblAdjustZ.Position.Y := t ;
  PlaceControl( bd, nil, lblAdjustZ, 2*LEFT_3COL_1, WIDTH_3COL ) ;
  if bd = bdRightToLeft then
    lblAdjustZ.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    lblAdjustZ.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  lblAdjustZ.Text := _rsrcna( GIS_RS_LEGEND_PRM_ADJUST_Z ) ;
  lblAdjustZ.FixSize ;

  lblAdjustBasement := TLabel.Create( Self.Panel ) ;
  lblAdjustBasement.Parent := Self.Panel ;
  lblAdjustBasement.Position.Y := t ;
  PlaceControl( bd, nil, lblAdjustBasement, LEFT_3COL_1 + LEFT_2COL_2, WIDTH_3COL ) ;
  if bd = bdRightToLeft then
    lblAdjustBasement.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    lblAdjustBasement.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  lblAdjustBasement.Text := _rsrcna( GIS_RS_LEGEND_PRM_ADJUST_BASEMENT ) ;
  lblAdjustBasement.FixSize ;

  t := t + lblAdjustZ.Height ;

  cmbAdjustZ := TComboBox.Create( Self.Panel ) ;
  cmbAdjustZ.Parent := Self.Panel ;
  cmbAdjustZ.Position.Y := t ;
  PlaceControl( bd, nil, cmbAdjustZ, 2*LEFT_3COL_1, speScaleZ.Width ) ;
  if bd = bdRightToLeft then
    cmbAdjustZ.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    cmbAdjustZ.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  cmbAdjustZ.Items.BeginUpdate ;
  cmbAdjustZ.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_RELATIVE_0 ) ) ;
  cmbAdjustZ.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_RELATIVE_DEM ) ) ;
  cmbAdjustZ.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_EQUAL_DEM ) ) ;
  cmbAdjustZ.ItemIndex := 2 ;
  cmbAdjustZ.Items.EndUpdate ;

  cmbAdjustBasement := TComboBox.Create( Self.Panel ) ;
  cmbAdjustBasement.Parent := Self.Panel ;
  cmbAdjustBasement.Position.Y := t ;
  PlaceControl( bd, nil, cmbAdjustBasement, LEFT_3COL_1 + LEFT_2COL_2, speScaleM.Width ) ;
  if bd = bdRightToLeft then
    cmbAdjustBasement.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    cmbAdjustBasement.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  cmbAdjustBasement.Items.BeginUpdate ;
  cmbAdjustBasement.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_OFF ) ) ;
  cmbAdjustBasement.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_LOWEST ) ) ;
  cmbAdjustBasement.ItemIndex := 0 ;
  cmbAdjustBasement.Items.EndUpdate ;

end ;

procedure T_panel3D.Read ;
var
  prt : TFmxObject ;
begin
  lockUpdates ;
  try
    case mvc.TreatLayerAs of
      TGIS_3DLayerType.Off    :
        begin
          rbnAs2D.IsChecked := True ;
          mvc.DoAs2DClick ;
        end ;
      TGIS_3DLayerType.Dem    :
        begin
          if hasDEM then begin
            rbnAsDEM.IsChecked := True ;
            mvc.DoAsDEMClick ;
          end ;
        end ;
      TGIS_3DLayerType.Shapes :
        begin
          rbnAs3D.IsChecked := True ;
          mvc.DoAs3DClick ;
        end ;
    end ;

    cmbNormalizedZ.ItemIndex := Integer( mvc.NormalizedZ ) ;

    vvspeScaleZ.Value := 100 * mvc.ScaleZ ;
    cmbFalseZ.Value   := mvc.FalseZ ;

    if not oParentWindow.MVC.IsVector then
      exit ;

    cmbNormalizedM.ItemIndex := Integer( mvc.NormalizedM ) ;
    vvspeScaleM.Value := 100 * mvc.ScaleM ;
    cmbFalseM.Value := mvc.FalseM ;

    cmbAdjustZ.Items.BeginUpdate ;
    case mvc.AdjustZ of
      TGIS_3DGroundType.AboveZero : cmbAdjustZ.ItemIndex := 0 ;
      TGIS_3DGroundType.AboveDem  : cmbAdjustZ.ItemIndex := 1 ;
      TGIS_3DGroundType.OnDem     : cmbAdjustZ.ItemIndex := 2 ;
    end ;
    cmbAdjustZ.Items.EndUpdate ;

    cmbAdjustBasement.Items.BeginUpdate ;
    case mvc.AdjustBasement of
      TGIS_3DBasementType.Off   : cmbAdjustBasement.ItemIndex := 0 ;
      TGIS_3DBasementType.Lowest: cmbAdjustBasement.ItemIndex := 1 ;
    end ;
    cmbAdjustBasement.Items.EndUpdate ;
  finally
    unlockUpdates ;
  end;
end ;

procedure T_panel3D.Write;
begin
  if rbnAs2D.IsChecked then
    mvc.TreatLayerAs := TGIS_3DLayerType.Off
  else
  if hasDEM and rbnAsDEM.IsChecked then
    mvc.TreatLayerAs := TGIS_3DLayerType.Dem
  else
  if Assigned( rbnAs3D ) then
    if rbnAs3D.IsChecked then
      mvc.TreatLayerAs := TGIS_3DLayerType.Shapes ;

  mvc.NormalizedZ := TGIS_3DNormalizationType( cmbNormalizedZ.ItemIndex ) ;
  mvc.ScaleZ      := vvspeScaleZ.Value / 100 ;
  mvc.FalseZ      := cmbFalseZ.Value ;

  if not oParentWindow.MVC.IsVector then
    exit ;

  mvc.NormalizedM := TGIS_3DNormalizationType( cmbNormalizedM.ItemIndex ) ;
  mvc.ScaleM      := vvspeScaleM.Value / 100 ;
  mvc.FalseM      := cmbFalseM.Value ;

  case cmbAdjustZ.ItemIndex of
    0 : mvc.AdjustZ := TGIS_3DGroundType.AboveZero ;
    1 : mvc.AdjustZ := TGIS_3DGroundType.AboveDem  ;
    2 : mvc.AdjustZ := TGIS_3DGroundType.OnDem     ;
  end ;

  case cmbAdjustBasement.ItemIndex of
    0 : mvc.AdjustBasement := TGIS_3DBasementType.Off ;
    1 : mvc.AdjustBasement := TGIS_3DBasementType.Lowest ;
  end ;
end ;

procedure T_panel3D.doCallback(
  _sender : TObject ;
  _code   : Integer
) ;

  procedure do_2d ;
  begin
    lblNormalizedZ.Visible := False ;
    cmbNormalizedZ.Visible := False ;
    lblScaleZ.Visible := False ;
    speScaleZ.Visible := False ;
    lblFalseZ.Visible := False ;
    cmbFalseZ.Visible := False ;

    if not oParentWindow.MVC.IsVector then
      exit ;

    lblNormalizedM.Visible := False ;
    cmbNormalizedM.Visible := False ;
    lblScaleM.Visible := False ;
    speScaleM.Visible := False ;
    lblFalseM.Visible := False ;
    cmbFalseM.Visible := False ;
    lblAdjustZ.Visible := False ;
    lblAdjustBasement.Visible := False ;
    cmbAdjustZ.Visible := False ;
    cmbAdjustBasement.Visible := False ;
  end ;

  procedure do_dem ;
  begin
    lblNormalizedZ.Visible := True ;
    cmbNormalizedZ.Visible := True ;
    lblScaleZ.Visible := True ;
    speScaleZ.Visible := True ;
    lblFalseZ.Visible := True ;
    cmbFalseZ.Visible := True ;

    if not oParentWindow.MVC.IsVector then
      exit ;

    lblNormalizedM.Visible := False ;
    cmbNormalizedM.Visible := False ;
    lblScaleM.Visible := False ;
    speScaleM.Visible := False ;
    lblFalseM.Visible := False ;
    cmbFalseM.Visible := False ;
    lblAdjustZ.Visible := False ;
    lblAdjustBasement.Visible := False ;
    cmbAdjustZ.Visible := False ;
    cmbAdjustBasement.Visible := False ;
  end ;

  procedure do_3d ;
  begin
    lblNormalizedZ.Visible := True ;
    cmbNormalizedZ.Visible := True ;
    lblScaleZ.Visible := True ;
    speScaleZ.Visible := True ;
    lblFalseZ.Visible := True ;
    cmbFalseZ.Visible := True ;

    if not oParentWindow.MVC.IsVector then
      exit ;

    lblNormalizedM.Visible := True ;
    cmbNormalizedM.Visible := True ;
    lblScaleM.Visible := True ;
    speScaleM.Visible := True ;
    lblFalseM.Visible := True ;
    cmbFalseM.Visible := True ;
    lblAdjustZ.Visible := True ;
    lblAdjustBasement.Visible := True ;
    cmbAdjustZ.Visible := True ;
    cmbAdjustBasement.Visible := True ;

  end ;

begin
  case _code of
    1 : do_2d  ;
    2 : do_dem ;
    3 : do_3d  ;
  end ;
end ;

procedure T_panel3D.doAs2DClick(
  _sender : TObject
) ;
begin
  mvc.DoAs2DClick ;
end ;

procedure T_panel3D.doAsDEMClick(
  _sender : TObject
) ;
begin
  mvc.DoAsDEMClick ;
end ;

procedure T_panel3D.doAs3DClick(
  _sender : TObject
) ;
begin
  mvc.DoAs3DClick ;
end ;

{$ENDREGION}

{$REGION 'T_panelSections'}
function  lstParamsPrepare( const _visible  : Boolean ;
                            const _minscale : String  ;
                            const _maxscale : String  ;
                            const _query    : String  ;
                            const _renderer : String  ;
                            const _legend   : String
                          ) : String ;
begin
  if _visible then Result := _rsrc( GIS_RS_LEGEND_SEC_VISIBLE )
              else Result := _rsrc( GIS_RS_LEGEND_SEC_HIDDEN ) ;

  if ( not IsStringEmpty( _minscale ) ) or
     ( not IsStringEmpty( _maxscale ) ) then
    Result := Result +
              Format( ' %s[ %s..%s ]', [ _rsrc( GIS_RS_LEGEND_SEC_SCALE ),
                                         _minscale,_maxscale
                                       ]
              ) ;
  if not IsStringEmpty( _query ) then
    Result := Result +
              Format( ' %s[ %s ]',     [ _rsrc( GIS_RS_LEGEND_SEC_QUERY ),
                                         _query
                                       ]
              ) ;
  if not IsStringEmpty( _renderer ) then
    Result := Result +
              Format( ' %s[ %s ]',    [ _rsrc( GIS_RS_LEGEND_SEC_RENDERER ),
                                        _renderer
                                      ]
              ) ;
  if not IsStringEmpty( _legend ) then
    Result := Format( ' %s',    [ _legend ] ) ;

end ;


function prepareScale( const oval : Double ) : String ; overload;
begin
  if      oval =  0     then Result := ''
  else if oval >  1e300 then Result := ''
  else if oval <  1     then Result := Format( '1:%.0f'  , [ 1/oval ] )
  else if oval >= 1     then Result := Format( '%.0f:1'  , [ oval/1 ] )
end ;

function split( const _str : String ; const _separator : array of Char ) : TArray<String> ;
var
  res : TStringDynArray ;
  i   : Integer ;
begin
  {$IFDEF LEVEL_XE3_RTL}
    Result := _str.Split( _separator )
  {$ELSE}
    res := System.StrUtils.SplitString( _str, _separator ) ;
    SetLength( Result, length( res ) ) ;
    for i := 0 to length( res )-1 do
      Result[i] := res[i] ;
  {$ENDIF}
end;

function prepareScale( const oval : String ; const def : Double ) : Double ; overload;
var
  arr : TArray<String> ;
begin
  if IsStringEmpty( oval ) then
    Result := def
  else begin
    arr := split(oval,[':']) ;
    if length(arr) > 1 then
      Result := DotStrToFloat( arr[1] )
    else
      Result := DotStrToFloat( arr[0] ) ;
    if Result <> 0 then
      Result := 1 / Result
    else
      Result := def ;
  end ;
end ;
function T_panelSections.fget_HasPreview : Boolean ;
begin
  Result := False ;
end ;

procedure T_panelSections.lvSectionsDblClick( _sender : TObject) ;
var
  nd : TTreeViewItem ;
  i  : Integer ;
begin
  nd := nil ;
  for i := 0 to lvSections.ItemIndex do
    nd := oParentWindow.tvPages.Items[1].Items[i] ;

  if assigned( nd ) then
    oParentWindow.tvPages.Selected := nd ;
end ;

procedure T_panelSections.PreparePreview(
  const _viewer : IGIS_Viewer
) ;
begin
  inherited;

end ;

procedure T_panelSections.init ;
begin
  inherited ;

  ItemText := _rsrcna( GIS_RS_LEGEND_PAG_SECTIONS ) ;

  gpbSection := TGroupBox.Create( Self.Panel ) ;
  gpbSection.Parent     := Self.Panel ;
  gpbSection.Position.Y := 4 ;
  gpbSection.Position.X := 8 ;
  gpbSection.Height     := 200 ;
  gpbSection.Width      := WIDTH_NORMAL ;
  gpbSection.Text       := _rsrcna( GIS_RS_LEGEND_PAG_SECTIONS ) ;

  lvSections := TListBox.Create( gpbSection ) ;
  lvSections.Parent     := gpbSection ;
  lvSections.Position.X := LEFT_3COL_1 ;
  lvSections.Position.Y := 24 ;
  lvSections.Height     := 200-24-16 ;
  lvSections.Width      := WIDTH_NORMAL-32 ;

  lvSections.OnDblClick := lvSectionsDblClick ;
end ;

procedure T_panelSections.Read ;
begin
  // nothing to read
end ;

procedure T_panelSections.Prepare ;
var
  i     : Integer ;
  mvc   : TGIS_ControlLegendFormMVC_Section ;
  idx   : Integer ;
  qstr  : String ;
begin
  lvSections.Items.BeginUpdate ;
  try
    lvSections.Items.Clear ;
    idx := oParentWindow.MVC.SectionIndex ;
    for i := 0 to oParentWindow.MVC.SectionCount - 1 do begin
      oParentWindow.MVC.SectionIndex := i ;
      mvc := oParentWindow.MVC.Section ;

      if oParentWindow.MVC.IsVector then
        qstr := mvc.Query
      else
        qstr := '' ;

      lvSections.Items.Add( lstParamsPrepare(
                              mvc.Visible,
                              prepareScale( mvc.MinScale ),
                              prepareScale( mvc.MaxScale ),
                              qstr,
                              '',
                              ''
                            )
                          ) ;
    end ;
  finally
    lvSections.Items.EndUpdate ;
    oParentWindow.MVC.SectionIndex := idx ;
  end ;
end ;

procedure T_panelSections.UpdatePreview ;
begin
  inherited ;

end ;

procedure T_panelSections.Write ;
begin
  // nothing to write
end ;
{$ENDREGION}

{$REGION 'T_panelSection'}

function T_panelSection.fget_HasPreview : Boolean ;
begin
  Result := mvc.HasPreview ;
end ;

procedure T_panelSection.init ;
var
  t   : Single ;
  anchors : TAnchors ;
  bd : TBiDiMode ;
begin
  inherited ;

  bd := oParentWindow.BiDiMode ;
  if bd = bdRightToLeft then
    anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;

  ItemText := _rsrcna( GIS_RS_LEGEND_SEC_VISIBLE ) ;

  mvc := oParentWindow.MVC.Section ;
  mvc.Callback := doCallback ;

  gpbSection := TGroupBox.Create( Self.Panel ) ;
  gpbSection.Parent := Self.Panel ;
  gpbSection.Position.Y := GROUPBOX_TOP ;
  gpbSection.Height := 200 ;
  PlaceControl( bd, nil, gpbSection, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  gpbSection.Anchors := anchors ;
  gpbSection.Text := _rsrcna( GIS_RS_LEGEND_PAG_SECTION ) ;

  t := GROUPBOX_TOP_GAP ;

  ckbVisible := TCheckBox.Create( gpbSection ) ;
  ckbVisible.Parent := gpbSection ;
  ckbVisible.Position.Y := t ;
  PlaceControl( bd, nil, ckbVisible, LEFT_3COL_1, WIDTH_2COL ) ;
  ckbVisible.Text := _rsrcna( GIS_RS_LEGEND_PRM_VISIBLE ) ;
  ckbVisible.IsChecked := True ;
  ckbVisible.OnChange := doVisibleClick ;

  t := t + ckbVisible.Height + 8 ;

  lblMinScale := TLabel.Create( gpbSection ) ;
  lblMinScale.Parent := gpbSection ;
  lblMinScale.Position.Y := t ;
  PlaceControl( bd, nil, lblMinScale, LEFT_3COL_1, WIDTH_3COL ) ;
  lblMinScale.Text := _rsrcna( GIS_RS_LEGEND_PRM_MINSCALE ) ;
  lblMinScale.FixSize ;

  t := t + lblMinScale.Height ;

  cmbMinScale := TComboEdit.Create( gpbSection ) ;
  cmbMinScale.Parent := gpbSection ;
  cmbMinScale.Position.Y := t + 1 ;
  PlaceControl( bd, nil, cmbMinScale, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbMinScale.Items.BeginUpdate ;
  cmbMinScale.Items.Add('1:500') ;
  cmbMinScale.Items.Add('1:1000') ;
  cmbMinScale.Items.Add('1:2500') ;
  cmbMinScale.Items.Add('1:5000') ;
  cmbMinScale.Items.Add('1:10 000') ;
  cmbMinScale.Items.Add('1:12 000') ;
  cmbMinScale.Items.Add('1:25 000') ;
  cmbMinScale.Items.Add('1:50 000') ;
  cmbMinScale.Items.Add('1:100 000') ;
  cmbMinScale.Items.Add('1:250 000') ;
  cmbMinScale.Items.Add('1:500 000') ;
  cmbMinScale.Items.Add('1:1 000 000') ;
  cmbMinScale.Items.Add('1:2 000 000') ;
  cmbMinScale.Items.Add('1:5 000 000') ;
  cmbMinScale.Items.Add('1:10 000 000') ;
  cmbMinScale.Items.Add('1:25 000 000') ;
  cmbMinScale.Items.Add('1:50 000 000') ;
  cmbMinScale.Items.Add('1:100 000 000') ;
  cmbMinScale.Items.EndUpdate ;
  cmbMinScale.OnChange := doMinScaleChange ;
  cmbMinScale.KillFocusByReturn := True ;

  btnMinScaleCur := TButton.Create( gpbSection ) ;
  btnMinScaleCur.Parent := gpbSection ;
  btnMinScaleCur.Position.Y := t ;
  {$IFDEF GIS_MOBILE}
  btnMinScaleCur.Scale.X := 0.8 ;
  btnMinScaleCur.Scale.Y := 0.8 ;
  {$ENDIF}
  {$IFNDEF GIS_MOBILE}
    btnMinScaleCur.Height := 23 ;
    PlaceControl( bd, cmbMinScale, btnMinScaleCur, 2, 70 ) ;
  {$ELSE}
    PlaceControl( bd, cmbMinScale, btnMinScaleCur, 2, btnMinScaleCur.Width ) ;
  {$ENDIF}
  btnMinScaleCur.Text := _rsrcna( GIS_RS_LEGEND_PRM_SCALE_CURRENT ) ;
  btnMinScaleCur.OnClick := doMinScaleClick ;

  btnMinScaleClr := TButton.Create( gpbSection ) ;
  btnMinScaleClr.Parent := gpbSection ;
  btnMinScaleClr.Position.Y := t ;
  {$IFDEF GIS_MOBILE}
  btnMinScaleClr.Scale.X := 0.8 ;
  btnMinScaleClr.Scale.Y := 0.8 ;
  {$ENDIF}
  btnMinScaleClr.Height := btnMinScaleCur.Height ;
  PlaceControl( bd, btnMinScaleCur, btnMinScaleClr, 2, btnMinScaleClr.Height ) ;
  btnMinScaleClr.OnClick := doMinScaleClear ;
  oParentWindow.addResBmp( 'TGIS_LEGENDFORMIMAGE_CLEAR', btnMinScaleClr ) ;

  t := t + lblMinScale.Height + 16 ;

  lblMaxScale := TLabel.Create( gpbSection ) ;
  lblMaxScale.Parent := gpbSection ;
  lblMaxScale.Position.Y := t ;
  PlaceControl( bd, nil, lblMaxScale, LEFT_3COL_1, WIDTH_3COL ) ;
  lblMaxScale.Text := _rsrcna( GIS_RS_LEGEND_PRM_MAXSCALE ) ;
  lblMaxScale.FixSize ;

  t := t + lblMinScale.Height ;

  cmbMaxScale := TComboEdit.Create( gpbSection ) ;
  cmbMaxScale.Parent := gpbSection ;
  cmbMaxScale.Position.Y := t + 1 ;
  PlaceControl( bd, nil, cmbMaxScale, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbMaxScale.Items.BeginUpdate ;
  cmbMaxScale.Items.Add('1:500') ;
  cmbMaxScale.Items.Add('1:1000') ;
  cmbMaxScale.Items.Add('1:2500') ;
  cmbMaxScale.Items.Add('1:5000') ;
  cmbMaxScale.Items.Add('1:10 000') ;
  cmbMaxScale.Items.Add('1:12 000') ;
  cmbMaxScale.Items.Add('1:25 000') ;
  cmbMaxScale.Items.Add('1:50 000') ;
  cmbMaxScale.Items.Add('1:100 000') ;
  cmbMaxScale.Items.Add('1:250 000') ;
  cmbMaxScale.Items.Add('1:500 000') ;
  cmbMaxScale.Items.Add('1:1 000 000') ;
  cmbMaxScale.Items.Add('1:2 000 000') ;
  cmbMaxScale.Items.Add('1:5 000 000') ;
  cmbMaxScale.Items.Add('1:10 000 000') ;
  cmbMaxScale.Items.Add('1:25 000 000') ;
  cmbMaxScale.Items.Add('1:50 000 000') ;
  cmbMaxScale.Items.Add('1:100 000 000') ;
  cmbMaxScale.Items.EndUpdate ;
  cmbMaxScale.OnChange := doMaxScaleChange ;
  cmbMaxScale.KillFocusByReturn := True ;

  btnMaxScaleCur := TButton.Create( gpbSection ) ;
  btnMaxScaleCur.Parent := gpbSection ;
  btnMaxScaleCur.Position.Y := t ;
  {$IFNDEF GIS_MOBILE}
    btnMaxScaleCur.Height := 23 ;
    PlaceControl( bd, cmbMaxScale, btnMaxScaleCur, 2, 70 ) ;
  {$ELSE}
    PlaceControl( bd, cmbMaxScale, btnMaxScaleCur, 2, btnMaxScaleCur.Width ) ;
  {$ENDIF}
  {$IFDEF GIS_MOBILE}
  btnMaxScaleCur.Scale.X := 0.8 ;
  btnMaxScaleCur.Scale.Y := 0.8 ;
  {$ENDIF}
  btnMaxScaleCur.Text := _rsrcna( GIS_RS_LEGEND_PRM_SCALE_CURRENT ) ;
  btnMaxScaleCur.OnClick := doMaxScaleClick ;

  btnMaxScaleClr := TButton.Create( gpbSection ) ;
  btnMaxScaleClr.Parent := gpbSection ;
  btnMaxScaleClr.Position.Y := t ;
  {$IFDEF GIS_MOBILE}
  btnMaxScaleClr.Scale.X := 0.8 ;
  btnMaxScaleClr.Scale.Y := 0.8 ;
  {$ENDIF}
  btnMaxScaleClr.Height := btnMaxScaleCur.Height ;
  PlaceControl( bd, btnMaxScaleCur, btnMaxScaleClr, 2, btnMaxScaleClr.Height ) ;
  btnMaxScaleClr.OnClick := doMaxScaleClear ;
  oParentWindow.addResBmp( 'TGIS_LEGENDFORMIMAGE_CLEAR', btnMaxScaleClr ) ;

  gpbSection.Height := t + cmbMaxScale.Height + GROUPBOX_BOTTOM_GAP ;

end ;

procedure T_panelSection.Read ;
begin
  lockUpdates ;
  try
    ckbVisible.IsChecked := mvc.Visible ;
    cmbMinScale.Text := prepareScale(mvc.MinScale) ;
    cmbMaxScale.Text := prepareScale(mvc.MaxScale) ;

    mvc.DoMinScaleChange ;
    mvc.DoMaxScaleChange ;
  finally
    unlockUpdates ;
  end;
end ;

procedure T_panelSection.updateNode ;
begin
  oParentWindow.lblTop.Text := lstParamsPrepare(
                                    ckbVisible.IsChecked,
                                    cmbMinScale.Text,
                                    cmbMaxScale.Text,
                                    '',
                                    '',
                                    ''
                                ) ;
end;

procedure T_panelSection.Write ;


begin
  mvc.Visible := ckbVisible.IsChecked ;
  mvc.MinScale := prepareScale( cmbMinScale.Text, 0 ) ;
  mvc.MaxScale := prepareScale( cmbMaxScale.Text, GIS_MAX_DOUBLE ) ;
end ;

procedure T_panelSection.doCallback(
  _sender : TObject ;
  _code   : Integer
) ;

  procedure do_visible_click ;
  begin
    updateNode ;
  end ;

  procedure do_min_click ;
  begin
    cmbMinScale.Text := prepareScale( oParentWindow.oViewer.Scale * 0.99 ) ;
    btnMinScaleClr.Enabled := True ;
    updateNode ;
  end ;

  procedure do_min_clear ;
  begin
    cmbMinScale.Text := '' ;
    btnMinScaleClr.Enabled := False ;
    updateNode ;
  end ;

  procedure do_min_change ;
  begin
    btnMinScaleClr.Enabled := not IsStringEmpty( cmbMinScale.Text ) ;
    updateNode ;
  end ;

  procedure do_max_click ;
  begin
    cmbMaxScale.Text := prepareScale( oParentWindow.oViewer.Scale *1.01 ) ;
    btnMaxScaleClr.Enabled := True ;
    updateNode ;
  end ;

  procedure do_max_clear ;
  begin
    cmbMaxScale.Text := '' ;
    btnMaxScaleClr.Enabled := False ;
    updateNode ;
  end ;

  procedure do_max_change ;
  begin
    btnMaxScaleClr.Enabled := not IsStringEmpty( cmbMaxScale.Text ) ;
    updateNode ;
  end ;

begin
  case _code of
    0 : do_visible_click  ;
    1 : do_min_click  ;
    2 : do_min_clear  ;
    3 : do_min_change ;
    4 : do_max_click  ;
    5 : do_max_clear  ;
    6 : do_max_change ;
  end ;
end ;

procedure T_panelSection.doVisibleClick(
  _sender : TObject
) ;
begin
  mvc.DoVisibleClick ;
end ;

procedure T_panelSection.doMinScaleClick(
  _sender : TObject
) ;
begin
  mvc.DoMinScaleClick ;
end ;

procedure T_panelSection.doMinScaleClear(
  _sender : TObject
) ;
begin
  mvc.DoMinScaleClear ;
end ;

procedure T_panelSection.doMinScaleChange(
  _sender : TObject
) ;
begin
  mvc.DoMinScaleChange ;
end ;

procedure T_panelSection.doMaxScaleClick(
  _sender : TObject
) ;
begin
  mvc.DoMaxScaleClick ;
end ;

procedure T_panelSection.doMaxScaleClear(
  _sender : TObject
) ;
begin
  mvc.DoMaxScaleClear ;
end ;

procedure T_panelSection.doMaxScaleChange(
  _sender : TObject
) ;
begin
  mvc.DoMaxScaleChange ;
end ;
{$ENDREGION}

{$REGION 'T_panelSectionVector'}
procedure T_panelSectionVector.init ;
var
  t : Single ;
  bd : TBiDiMode ;
begin
  inherited ;

  bd := oParentWindow.BiDiMode ;

  ItemText := _rsrcna( GIS_RS_LEGEND_SEC_VISIBLE ) ;

  t := btnMaxScaleCur.Position.Y + btnMaxScaleCur.Height + 16 ;

  lblQuery := TLabel.Create( gpbSection) ;
  lblQuery.Parent := gpbSection ;
  lblQuery.Position.Y := t ;
  PlaceControl( bd, nil, lblQuery, LEFT_3COL_1, -1 ) ;
  lblQuery.Text := _rsrcna( GIS_RS_LEGEND_PRM_QUERY ) ;
  lblQuery.FixSize ;

  t := t + lblQuery.Height ;

  cmbQuery := TComboEdit.Create( gpbSection ) ;
  cmbQuery.Parent := gpbSection ;
  cmbQuery.Position.Y := t ;
  PlaceControl( bd, nil, cmbQuery, LEFT_3COL_1, -1 ) ;
  cmbQuery.ItemIndex := -1 ;
  cmbQuery.OnChange := doQueryChange ;
  oParentWindow.fillComboBoxWithFields( cmbQuery ) ;
  cmbQuery.KillFocusByReturn := True ;

  t := t + cmbQuery.Height + 8 ;

  lblLegend := TLabel.Create( gpbSection ) ;
  lblLegend.Parent := gpbSection ;
  lblLegend.Position.Y := t ;
  PlaceControl( bd, nil, lblLegend, LEFT_3COL_1, -1 ) ;
  lblLegend.Text := _rsrcna( GIS_RS_LEGEND_PRM_LEGEND ) ;
  lblLegend.FixSize ;

  t := t + lblLegend.Height ;

  edtLegend := TEdit.Create( gpbSection ) ;
  edtLegend.Parent := gpbSection ;
  edtLegend.Position.Y := t ;
  edtLegend.FixSize ;
  PlaceControl( bd, nil, edtLegend, LEFT_3COL_1, -1 ) ;
  edtLegend.Text := '' ;
  edtLegend.OnChange := doLegendChange ;
  edtLegend.KillFocusByReturn := True ;
  wasLegendEdited := False ;

  gpbSection.Height := t + edtLegend.Height + GROUPBOX_BOTTOM_GAP ;

end ;

procedure T_panelSectionVector.Read ;
begin
  inherited ;

  lockUpdates ;
  try
    cmbQuery.Text := mvc.Query ;
    edtLegend.Text := mvc.Legend ;

    mvc.DoVisibleClick ;
  finally
    unlockUpdates ;
  end;
end ;

procedure T_panelSectionVector.updateNode ;
begin
  if IsStringEmpty( edtLegend.Text ) then
    ItemText := Format( '%s %d',
                        [ _rsrcna( GIS_RS_LEGEND_PAG_SECTION ),
                          oParentWindow.MVC.SectionIndex+1 ]
                      )
  else
    ItemText := edtLegend.Text ;

  oParentWindow.lblTop.Text := lstParamsPrepare(
                                    ckbVisible.IsChecked,
                                    cmbMinScale.Text,
                                    cmbMaxScale.Text,
                                    cmbQuery.Text,
                                    '',
                                    ''
                                  ) ;
end;

procedure T_panelSectionVector.updateSectionNode ;
var
  nd   : TTreeViewItem ;
  prnt : TTreeViewItem ;
begin
  nd := oParentWindow.tvPages.Selected ;
  if not assigned( nd ) then exit ;

  prnt := nd.ParentItem ;
  if not assigned( prnt ) then exit ;

  if ( prnt.Level = 1 ) and ( nd.Level = 2 ) then begin
    if IsStringEmpty( edtLegend.Text ) then
      nd.Text := Format( '%s %d',
                          [ _rsrc( GIS_RS_LEGEND_PAG_SECTION ),
                            oParentWindow.MVC.SectionIndex+1 ]
                        )
    else
      nd.Text := edtLegend.Text ;

    oParentWindow.tvPages.Repaint ;
  end ;
end ;


procedure T_panelSectionVector.Write ;
begin
  inherited ;

  mvc.Query := cmbQuery.Text ;
  mvc.Legend := edtLegend.Text ;
end ;

procedure T_panelSectionVector.doCallback(
  _sender : TObject ;
  _code   : Integer
) ;

  procedure do_query_change ;
  begin
    if not wasLegendEdited then
      edtLegend.Text := cmbQuery.Text ;

    updateNode ;
  end ;

  procedure do_legend_change ;
  begin
    updateSectionNode ;

    if wasLegendEdited then
      exit ;

    if edtLegend.Text <> cmbQuery.Text then
      wasLegendEdited := True ;

    updateNode ;
  end ;

  procedure do_visible_click ;
  begin
    updateNode ;
  end ;

begin
  inherited ;

  case _code of
    0 : do_visible_click  ;
    7 : do_query_change ;
    8 : do_legend_change ;
  end ;
end ;

procedure T_panelSectionVector.doQueryChange(
  _sender : TObject
) ;
begin
  mvc.DoQueryChange ;
end ;

procedure T_panelSectionVector.doLegendChange(
  _sender : TObject
) ;
begin
  mvc.DoLegendChange ;
end ;
{$ENDREGION}

{$REGION 'T_panelRenderer'}
function T_panelRenderer.fget_HasPreview : Boolean ;
begin
  Result := mvc.HasPreview ;
end ;

procedure T_panelRenderer.init ;
begin
  inherited ;

  ItemText := _rsrcna( GIS_RS_LEGEND_PAG_RENDERER ) ;

  mvc := oParentWindow.MVC.Renderer ;
  mvc.Callback := doCallback ;

  initSelf ;
  initFirst ;
  initSecond ;
end ;

procedure T_panelRenderer.initSelf ;
var
  t : Single ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbRender := TGroupBox.Create( Self.Panel ) ;
  gpbRender.Parent := Self.Panel ;
  gpbRender.Position.Y  := GROUPBOX_TOP ;
  gpbRender.Height := 512 ;
  PlaceControl( bd, nil, gpbRender, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbRender.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbRender.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbRender.Text := _rsrcna( GIS_RS_LEGEND_PAG_RENDERER ) ;

  t := GROUPBOX_TOP_GAP ;

  lblExpression := TLabel.Create( gpbRender ) ;
  lblExpression.Parent := gpbRender ;
  lblExpression.Position.Y := t ;
  PlaceControl( bd, nil, lblExpression, LEFT_3COL_1, LEFT_3COL_3 ) ;
  lblExpression.Text := _rsrcna( GIS_RS_LEGEND_PRM_EXPRESSION ) ;
  lblExpression.FixSize ;

  lblRounding := TLabel.Create( gpbRender ) ;
  lblRounding.Parent := gpbRender ;
  lblRounding.Position.Y := t ;
  lblRounding.Text := _rsrcna( GIS_RS_LEGEND_PRM_ROUND ) ;
  lblRounding.FixSize ;

  t := t + lblExpression.Height ;

  cmbExpression := TComboEdit.Create( gpbRender ) ;
  cmbExpression.Parent := gpbRender ;
  cmbExpression.Position.Y := t ;
  PlaceControl( bd, nil, cmbExpression, LEFT_3COL_1, LEFT_3COL_3 ) ;
  cmbExpression.ItemIndex := -1 ;
  cmbExpression.OnChange := doExpressionChange ;
  oParentWindow.fillComboBoxWithFields( cmbExpression ) ;
  cmbExpression.KillFocusByReturn := True ;

  PlaceControl( bd, cmbExpression, lblRounding, 16, 75 ) ;

  speRounding := TEdit.Create( gpbRender ) ;
  speRounding.Parent := gpbRender ;
  speRounding.Position.Y := t ;
  PlaceControl( bd, cmbExpression, speRounding, 16, 75 ) ;
  speRounding.KillFocusByReturn := True ;

  vvspeRounding := TGIS_ValueValidatorEditHelper.Create( speRounding ) ;
  vvspeRounding.MinVal := -10.0 ;
  vvspeRounding.MaxVal :=  10.0 ;

  gpbRender.Height := t + speRounding.Height + GROUPBOX_BOTTOM_GAP ;
end ;

procedure T_panelRenderer.initFirst ;
var
  t : Single ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbFirst := TGroupBox.Create( Self.Panel ) ;
  gpbFirst.Parent := Self.Panel ;
  gpbFirst.Position.Y  := gpbRender.Position.Y + gpbRender.Height + GROUPBOX_GAP ;
  gpbFirst.Height := 512 ;
  PlaceControl( bd, nil, gpbFirst, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbFirst.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbFirst.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbFirst.Text := _rsrcna( GIS_RS_LEGEND_TAB_FIRST ) ;

  t := GROUPBOX_TOP_GAP ;

  lblNumOfZones1 := TLabel.Create( gpbFirst ) ;
  lblNumOfZones1.Parent := gpbFirst ;
  lblNumOfZones1.Position.Y := t ;
  PlaceControl( bd, nil, lblNumOfZones1, LEFT_3COL_1, WIDTH_3COL ) ;
  lblNumOfZones1.Text := _rsrcna( GIS_RS_LEGEND_PRM_ZONES ) ;
  lblNumOfZones1.FixSize ;


  lblMinVal1 := TLabel.Create( gpbFirst ) ;
  lblMinVal1.Parent := gpbFirst ;
  lblMinVal1.Position.Y := t ;
  PlaceControl( bd, nil, lblMinVal1, LEFT_3COL_2, WIDTH_3COL ) ;
  lblMinVal1.Text := _rsrcna( GIS_RS_LEGEND_PRM_MINVAL ) ;
  lblMinVal1.FixSize ;

  lblMaxVal1 := TLabel.Create( gpbFirst ) ;
  lblMaxVal1.Parent := gpbFirst ;
  lblMaxVal1.Position.Y := t ;
  PlaceControl( bd, nil, lblMaxVal1, LEFT_3COL_3, WIDTH_3COL ) ;
  lblMaxVal1.Text := _rsrcna( GIS_RS_LEGEND_PRM_MAXVAL ) ;
  lblMaxVal1.FixSize ;

  t := t + lblNumOfZones1.Height ;

  speNumOfZones1 := TEdit.Create( gpbFirst ) ;
  speNumOfZones1.Parent := gpbFirst ;
  speNumOfZones1.Position.Y := t ;
  PlaceControl( bd, nil, speNumOfZones1, LEFT_3COL_1, WIDTH_3COL ) ;
  speNumOfZones1.OnChange := doNumberOfZones1Change ;
  speNumOfZones1.KillFocusByReturn := True ;

  vvspeNumOfZones1 := TGIS_ValueValidatorEditHelper.Create( speNumOfZones1 ) ;
  vvspeNumOfZones1.MinVal := -100.0 ;
  vvspeNumOfZones1.MaxVal :=  100.0 ;

  edtMinVal1 := TEdit.Create( gpbFirst ) ;
  edtMinVal1.Parent := gpbFirst ;
  edtMinVal1.Position.Y := t ;
  PlaceControl( bd, nil, edtMinVal1, LEFT_3COL_2, WIDTH_3COL ) ;
  edtMinVal1.Text := '0' ;
  edtMinVal1.FixSize ;
  edtMinVal1.KillFocusByReturn := True ;

  vvedtMinVal1 := TGIS_ValueValidatorEditHelper.Create( edtMinVal1 ) ;
  vvedtMinVal1.MinVal := -1E307 ;
  vvedtMinVal1.MaxVal := 1E307 ;
  vvedtMinVal1.Precision := 99 ;

  edtMaxVal1 := TEdit.Create( gpbFirst ) ;
  edtMaxVal1.Parent := gpbFirst ;
  edtMaxVal1.Position.Y := t ;
  PlaceControl( bd, nil, edtMaxVal1, LEFT_3COL_3, WIDTH_3COL ) ;
  edtMaxVal1.Text := '0' ;
  edtMaxVal1.FixSize ;
  edtMaxVal1.KillFocusByReturn := True ;

  vvedtMaxVal1 := TGIS_ValueValidatorEditHelper.Create( edtMaxVal1 ) ;
  vvedtMaxVal1.MinVal := -1E307 ;
  vvedtMaxVal1.MaxVal := 1E307 ;
  vvedtMaxVal1.Precision := 99 ;

  t := t + speNumOfZones1.Height + 8 ;

  lblStartColor1 := TLabel.Create( gpbFirst ) ;
  lblStartColor1.Parent := gpbFirst ;
  lblStartColor1.Position.Y := t ;
  PlaceControl( bd, nil, lblStartColor1, LEFT_3COL_1, WIDTH_3COL ) ;
  lblStartColor1.Text := _rsrcna( GIS_RS_LEGEND_PRM_STARTCOLOR ) ;
  lblStartColor1.FixSize ;

  lblEndColor1 := TLabel.Create( gpbFirst ) ;
  lblEndColor1.Parent := gpbFirst ;
  lblEndColor1.Position.Y := t ;
  PlaceControl( bd, nil, lblEndColor1, LEFT_3COL_2, WIDTH_3COL ) ;
  lblEndColor1.Text := _rsrcna( GIS_RS_LEGEND_PRM_ENDCOLOR ) ;
  lblEndColor1.FixSize ;

  lblDefaultColor1 := TLabel.Create( gpbFirst ) ;
  lblDefaultColor1.Parent := gpbFirst ;
  lblDefaultColor1.Position.Y := t ;
  PlaceControl( bd, nil, lblDefaultColor1, LEFT_3COL_3, WIDTH_3COL ) ;
  lblDefaultColor1.Text := _rsrcna( GIS_RS_LEGEND_PRM_DEFAULTCOLOR ) ;
  lblDefaultColor1.FixSize ;

  t := t + lblStartColor1.Height ;

  cmbStartColor1 := TGIS_ColorComboBox.Create( gpbFirst ) ;
  cmbStartColor1.Parent := gpbFirst ;
  cmbStartColor1.Position.Y := t ;
  cmbStartColor1.Height := 28 ;
  PlaceControl( bd, nil, cmbStartColor1, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbStartColor1.Fill( True, False ) ;
  cmbStartColor1.OnChange    := doControlChange ;
  cmbStartColor1.CustomEvent := doCustomColor ;

  cmbEndColor1 := TGIS_ColorComboBox.Create( gpbFirst ) ;
  cmbEndColor1.Parent := gpbFirst ;
  cmbEndColor1.Position.Y := t ;
  cmbEndColor1.Height := 28 ;
  PlaceControl( bd, nil, cmbEndColor1, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbEndColor1.Fill( True, False ) ;
  cmbEndColor1.OnChange    := doControlChange ;
  cmbEndColor1.CustomEvent := doCustomColor ;

  cmbDefaultColor1 := TGIS_ColorComboBox.Create( gpbFirst ) ;
  cmbDefaultColor1.Parent := gpbFirst ;
  cmbDefaultColor1.Position.Y := t ;
  cmbDefaultColor1.Height := 28 ;
  PlaceControl( bd, nil, cmbDefaultColor1, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbDefaultColor1.Fill( True, False ) ;
  cmbDefaultColor1.OnChange    := doControlChange ;
  cmbDefaultColor1.CustomEvent := doCustomColor ;

  t := t + cmbStartColor1.Height + 8 ;

  lblStartSize1 := TLabel.Create( gpbFirst ) ;
  lblStartSize1.Parent := gpbFirst ;
  lblStartSize1.Position.Y := t ;
  PlaceControl( bd, nil, lblStartSize1, LEFT_3COL_1, WIDTH_3COL ) ;
  lblStartSize1.Text := _rsrcna( GIS_RS_LEGEND_PRM_STARTSIZE ) ;
  lblStartSize1.FixSize ;

  lblEndSize1 := TLabel.Create( gpbFirst ) ;
  lblEndSize1.Parent := gpbFirst ;
  lblEndSize1.Position.Y := t ;
  PlaceControl( bd, nil, lblEndSize1, LEFT_3COL_2, WIDTH_3COL ) ;
  lblEndSize1.Text := _rsrcna( GIS_RS_LEGEND_PRM_ENDSIZE ) ;
  lblEndSize1.FixSize ;

  lblDefaultSize1 := TLabel.Create( gpbFirst ) ;
  lblDefaultSize1.Parent := gpbFirst ;
  lblDefaultSize1.Position.Y := t ;
  PlaceControl( bd, nil, lblDefaultSize1, LEFT_3COL_3, WIDTH_3COL ) ;
  lblDefaultSize1.Text := _rsrcna( GIS_RS_LEGEND_PRM_DEFAULTSIZE ) ;
  lblDefaultSize1.FixSize ;

  t := t + lblStartSize1.Height ;

  cmbStartSize1 := TGIS_SizeComboBox.Create( gpbFirst ) ;
  cmbStartSize1.Parent := gpbFirst ;
  cmbStartSize1.Position.Y := t ;
  PlaceControl( bd, nil, cmbStartSize1, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbStartSize1.Fill( True, False, True, False ) ;
  cmbStartSize1.OnChange := doControlChange ;
  cmbStartSize1.CustomEvent := doCustomSize ;

  cmbEndSize1 := TGIS_SizeComboBox.Create( gpbFirst ) ;
  cmbEndSize1.Parent := gpbFirst ;
  cmbEndSize1.Position.Y := t ;
  PlaceControl( bd, nil, cmbEndSize1, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbEndSize1.Fill( True, False, True, False ) ;
  cmbEndSize1.OnChange := doControlChange ;
  cmbEndSize1.CustomEvent := doCustomSize ;

  cmbDefaultSize1 := TGIS_SizeComboBox.Create( gpbFirst ) ;
  cmbDefaultSize1.Parent := gpbFirst ;
  cmbDefaultSize1.Position.Y := t ;
  PlaceControl( bd, nil, cmbDefaultSize1, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbDefaultSize1.Fill( True, False, True, False ) ;
  cmbDefaultSize1.OnChange := doControlChange ;
  cmbDefaultSize1.CustomEvent := doCustomSize ;

  t := t + cmbStartSize1.Height + GROUPBOX_BOTTOM_GAP ;

  gpbFirst.Height := t ;
end ;

procedure T_panelRenderer.initSecond ;
var
  t : Single ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbSecond := TGroupBox.Create( Self.Panel ) ;
  gpbSecond.Parent := Self.Panel ;
  gpbSecond.Position.Y  := gpbFirst.Position.Y + gpbFirst.Height + GROUPBOX_GAP ;
  gpbSecond.Height := 512 ;
  PlaceControl( bd, nil, gpbSecond, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbSecond.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbSecond.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbSecond.Text := _rsrcna( GIS_RS_LEGEND_TAB_SECOND ) ;

  t := GROUPBOX_TOP_GAP ;

  lblNumOfZones2 := TLabel.Create( gpbSecond ) ;
  lblNumOfZones2.Parent := gpbSecond ;
  lblNumOfZones2.Position.Y := t ;
  PlaceControl( bd, nil, lblNumOfZones2, LEFT_3COL_1, WIDTH_3COL ) ;
  lblNumOfZones2.Text := _rsrcna( GIS_RS_LEGEND_PRM_ZONES ) ;
  lblNumOfZones2.FixSize ;

  lblMinVal2 := TLabel.Create( gpbSecond ) ;
  lblMinVal2.Parent := gpbSecond ;
  lblMinVal2.Position.Y := t ;
  PlaceControl( bd, nil, lblMinVal2, LEFT_3COL_2, WIDTH_3COL ) ;
  lblMinVal2.Text := _rsrcna( GIS_RS_LEGEND_PRM_MINVAL ) ;
  lblMinVal2.FixSize ;

  lblMaxVal2 := TLabel.Create( gpbSecond ) ;
  lblMaxVal2.Parent := gpbSecond ;
  lblMaxVal2.Position.Y := t ;
  PlaceControl( bd, nil, lblMaxVal2, LEFT_3COL_3, WIDTH_3COL ) ;
  lblMaxVal2.Text := _rsrcna( GIS_RS_LEGEND_PRM_MAXVAL ) ;
  lblMaxVal2.FixSize ;

  t := t + lblNumOfZones2.Height ;

  speNumOfZones2 := TEdit.Create( gpbSecond ) ;
  speNumOfZones2.Parent := gpbSecond ;
  speNumOfZones2.Position.Y := t ;
  PlaceControl( bd, nil, speNumOfZones2, LEFT_3COL_1, WIDTH_3COL ) ;
  speNumOfZones2.OnChange := doNumberOfZones2Change ;
  speNumOfZones2.KillFocusByReturn := True ;

  vvspeNumOfZones2 := TGIS_ValueValidatorEditHelper.Create( speNumOfZones2 ) ;
  vvspeNumOfZones2.MinVal := -100.0 ;
  vvspeNumOfZones2.MaxVal :=  100.0 ;

  edtMinVal2 := TEdit.Create( gpbSecond ) ;
  edtMinVal2.Parent := gpbSecond ;
  edtMinVal2.Position.Y := t ;
  PlaceControl( bd, nil, edtMinVal2, LEFT_3COL_2, WIDTH_3COL ) ;
  edtMinVal2.Width := WIDTH_3COL ;
  edtMinVal2.Text := '0' ;
  edtMinVal2.FixSize ;
  edtMinVal2.KillFocusByReturn := True ;

  vvedtMinVal2 := TGIS_ValueValidatorEditHelper.Create( edtMinVal2 ) ;
  vvedtMinVal2.MinVal := -1E307 ;
  vvedtMinVal2.MaxVal := 1E307 ;
  vvedtMinVal2.Precision := 99 ;

  edtMaxVal2 := TEdit.Create( gpbSecond ) ;
  edtMaxVal2.Parent := gpbSecond ;
  edtMaxVal2.Position.Y := t ;
  PlaceControl( bd, nil, edtMaxVal2, LEFT_3COL_3, WIDTH_3COL ) ;
  edtMaxVal2.Text := '0' ;
  edtMaxVal2.FixSize ;
  edtMaxVal2.KillFocusByReturn := True ;

  vvedtMaxVal2 := TGIS_ValueValidatorEditHelper.Create( edtMaxVal2 ) ;
  vvedtMaxVal2.MinVal := -1E307 ;
  vvedtMaxVal2.MaxVal := 1E307 ;
  vvedtMaxVal2.Precision := 99 ;

  t := t + speNumOfZones2.Height + 8 ;

  lblStartColor2 := TLabel.Create( gpbSecond ) ;
  lblStartColor2.Parent := gpbSecond ;
  lblStartColor2.Position.Y := t ;
  PlaceControl( bd, nil, lblStartColor2, LEFT_3COL_1, WIDTH_3COL ) ;
  lblStartColor2.Text := _rsrcna( GIS_RS_LEGEND_PRM_STARTCOLOR ) ;
  lblStartColor2.FixSize ;

  lblEndColor2 := TLabel.Create( gpbSecond ) ;
  lblEndColor2.Parent := gpbSecond ;
  lblEndColor2.Position.Y := t ;
  PlaceControl( bd, nil, lblEndColor2, LEFT_3COL_2, WIDTH_3COL ) ;
  lblEndColor2.Text := _rsrcna( GIS_RS_LEGEND_PRM_ENDCOLOR ) ;
  lblEndColor2.FixSize ;

  lblDefaultColor2 := TLabel.Create( gpbSecond ) ;
  lblDefaultColor2.Parent := gpbSecond ;
  lblDefaultColor2.Position.Y := t ;
  PlaceControl( bd, nil, lblDefaultColor2, LEFT_3COL_3, WIDTH_3COL ) ;
  lblDefaultColor2.Text := _rsrcna( GIS_RS_LEGEND_PRM_DEFAULTCOLOR ) ;
  lblDefaultColor2.FixSize ;

  t := t + lblStartColor2.Height ;

  cmbStartColor2 := TGIS_ColorComboBox.Create( gpbSecond ) ;
  cmbStartColor2.Parent := gpbSecond ;
  cmbStartColor2.Position.Y := t ;
  cmbStartColor2.Height := 28 ;
  PlaceControl( bd, nil, cmbStartColor2, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbStartColor2.Fill( True, False ) ;
  cmbStartColor2.OnChange    := doControlChange ;
  cmbStartColor2.CustomEvent := doCustomColor ;

  cmbEndColor2 := TGIS_ColorComboBox.Create( gpbSecond ) ;
  cmbEndColor2.Parent := gpbSecond ;
  cmbEndColor2.Position.Y := t ;
  cmbEndColor2.Height := 28 ;
  PlaceControl( bd, nil, cmbEndColor2, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbEndColor2.Fill( True, False ) ;
  cmbEndColor2.OnChange    := doControlChange ;
  cmbEndColor2.CustomEvent := doCustomColor ;

  cmbDefaultColor2 := TGIS_ColorComboBox.Create( gpbSecond ) ;
  cmbDefaultColor2.Parent := gpbSecond ;
  cmbDefaultColor2.Position.Y := t ;
  cmbDefaultColor2.Height := 28 ;
  PlaceControl( bd, nil, cmbDefaultColor2, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbDefaultColor2.Fill( True, False ) ;
  cmbDefaultColor2.OnChange    := doControlChange ;
  cmbDefaultColor2.CustomEvent := doCustomColor ;

  t := t + cmbStartColor2.Height + 8 ;

  lblStartSize2 := TLabel.Create( gpbSecond ) ;
  lblStartSize2.Parent := gpbSecond ;
  lblStartSize2.Position.Y := t ;
  PlaceControl( bd, nil, lblStartSize2, LEFT_3COL_1, WIDTH_3COL ) ;
  lblStartSize2.Text := _rsrcna( GIS_RS_LEGEND_PRM_STARTSIZE ) ;
  lblStartSize2.FixSize;

  lblEndSize2 := TLabel.Create( gpbSecond ) ;
  lblEndSize2.Parent := gpbSecond ;
  lblEndSize2.Position.Y := t ;
  PlaceControl( bd, nil, lblEndSize2, LEFT_3COL_2, WIDTH_3COL ) ;
  lblEndSize2.Text := _rsrcna( GIS_RS_LEGEND_PRM_ENDSIZE ) ;
  lblEndSize2.FixSize;

  lblDefaultSize2 := TLabel.Create( gpbSecond ) ;
  lblDefaultSize2.Parent := gpbSecond ;
  lblDefaultSize2.Position.Y := t ;
  PlaceControl( bd, nil, lblDefaultSize2, LEFT_3COL_3, WIDTH_3COL ) ;
  lblDefaultSize2.Text := _rsrcna( GIS_RS_LEGEND_PRM_DEFAULTSIZE ) ;
  lblDefaultSize2.FixSize ;

  t := t + lblStartSize2.Height ;

  cmbStartSize2 := TGIS_SizeComboBox.Create( gpbSecond ) ;
  cmbStartSize2.Parent := gpbSecond ;
  cmbStartSize2.Position.Y := t ;
  PlaceControl( bd, nil, cmbStartSize2, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbStartSize2.Fill( True, False, True, False ) ;
  cmbStartSize2.OnChange := doControlChange ;
  cmbStartSize2.CustomEvent := doCustomSize ;

  cmbEndSize2 := TGIS_SizeComboBox.Create( gpbSecond ) ;
  cmbEndSize2.Parent := gpbSecond ;
  cmbEndSize2.Position.Y := t ;
  PlaceControl( bd, nil, cmbEndSize2, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbEndSize2.Fill( True, False, True, False ) ;
  cmbEndSize2.OnChange := doControlChange ;
  cmbEndSize2.CustomEvent := doCustomSize ;

  cmbDefaultSize2 := TGIS_SizeComboBox.Create( gpbSecond ) ;
  cmbDefaultSize2.Parent := gpbSecond ;
  cmbDefaultSize2.Position.Y := t ;
  PlaceControl( bd, nil, cmbDefaultSize2, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbDefaultSize2.Fill( True, False, True, False ) ;
  cmbDefaultSize2.OnChange := doControlChange ;
  cmbDefaultSize2.CustomEvent := doCustomSize ;

  t := t + cmbStartSize2.Height + GROUPBOX_BOTTOM_GAP ;

  gpbSecond.Height := t ;
end ;

procedure T_panelRenderer.enableFirst(
  _enable : Boolean
) ;
begin
  lblMinVal1.Enabled := _enable ;
  lblMaxVal1.Enabled := _enable ;
  edtMinVal1.Enabled := _enable ;
  edtMaxVal1.Enabled := _enable ;
  lblStartColor1.Enabled := _enable ;
  lblEndColor1.Enabled := _enable ;
  lblDefaultColor1.Enabled := _enable ;
  cmbStartColor1.Enabled := _enable ;
  cmbEndColor1.Enabled := _enable ;
  cmbDefaultColor1.Enabled := _enable ;
  lblStartSize1.Enabled := _enable ;
  lblEndSize1.Enabled := _enable ;
  lblDefaultSize1.Enabled := _enable ;
  cmbStartSize1.Enabled := _enable ;
  cmbEndSize1.Enabled := _enable ;
  cmbDefaultSize1.Enabled := _enable ;
end ;

procedure T_panelRenderer.enableSecond(
  _enable : Boolean
) ;
begin
  lblMinVal2.Enabled := _enable ;
  lblMaxVal2.Enabled := _enable ;
  edtMinVal2.Enabled := _enable ;
  edtMaxVal2.Enabled := _enable ;
  lblStartColor2.Enabled := _enable ;
  lblEndColor2.Enabled := _enable ;
  lblDefaultColor2.Enabled := _enable ;
  cmbStartColor2.Enabled := _enable ;
  cmbEndColor2.Enabled := _enable ;
  cmbDefaultColor2.Enabled := _enable ;
  lblStartSize2.Enabled := _enable ;
  lblEndSize2.Enabled := _enable ;
  lblDefaultSize2.Enabled := _enable ;
  cmbStartSize2.Enabled := _enable ;
  cmbEndSize2.Enabled := _enable ;
  cmbDefaultSize2.Enabled := _enable ;
end ;

procedure T_panelRenderer.Read ;
begin
  lockUpdates ;
  try
    cmbExpression.Text      := mvc.ParamsRender.Expression ;
    vvspeRounding.Value     := mvc.ParamsRender.Round ;

    // First
    vvspeNumOfZones1.Value  := mvc.ParamsRender.Zones ;
    vvedtMinVal1.Value      := mvc.ParamsRender.MinVal ;
    vvedtMaxVal1.Value      := mvc.ParamsRender.MaxVal ;

    cmbStartColor1.Value    := mvc.ParamsRender.StartColorAsText ;
    cmbEndColor1.Value      := mvc.ParamsRender.EndColorAsText ;
    cmbDefaultColor1.Value  := mvc.ParamsRender.ColorDefaultAsText ;

    cmbStartSize1.Value     := mvc.ParamsRender.StartSizeAsText ;
    cmbEndSize1.Value       := mvc.ParamsRender.EndSizeAsText ;
    cmbDefaultSize1.Value   := mvc.ParamsRender.SizeDefaultAsText ;

    // Second
    vvspeNumOfZones2.Value  := mvc.ParamsRender.ZonesEx ;
    vvedtMinVal2.Value      := mvc.ParamsRender.MinValEx ;
    vvedtMaxVal2.Value      := mvc.ParamsRender.MaxValEx ;

    cmbStartColor2.Value    := mvc.ParamsRender.StartColorExAsText ;
    cmbEndColor2.Value      := mvc.ParamsRender.EndColorExAsText ;
    cmbDefaultColor2.Value  := mvc.ParamsRender.ColorDefaultAsText ;

    cmbStartSize2.Value     := mvc.ParamsRender.StartSizeExAsText ;
    cmbEndSize2.Value       := mvc.ParamsRender.EndSizeExAsText ;
    cmbDefaultSize2.Value   := mvc.ParamsRender.SizeDefaultAsText ;

    mvc.DoExpressionChange ;
  finally
    unlockUpdates ;
  end;
end ;

procedure T_panelRenderer.Write ;
begin
  mvc.ParamsRender.Expression         := cmbExpression.Text ;
  mvc.ParamsRender.Round              := FloorS( vvspeRounding.Value ) ;

  // First
  mvc.ParamsRender.Zones              := FloorS( vvspeNumOfZones1.Value ) ;
  mvc.ParamsRender.MinVal             := vvedtMinVal1.Value ;
  mvc.ParamsRender.MaxVal             := vvedtMaxVal1.Value ;
  mvc.ParamsRender.StartColorAsText   := cmbStartColor1.Value ;
  mvc.ParamsRender.EndColorAsText     := cmbEndColor1.Value ;
  mvc.ParamsRender.ColorDefaultAsText := cmbDefaultColor1.Value ;
  mvc.ParamsRender.StartSizeAsText    := cmbStartSize1.Value  ;
  mvc.ParamsRender.EndSizeAsText      := cmbEndSize1.Value  ;
  mvc.ParamsRender.SizeDefaultAsText  := cmbDefaultSize1.Value  ;

  // Second
  mvc.ParamsRender.ZonesEx            := FloorS( vvspeNumOfZones2.Value ) ;
  mvc.ParamsRender.MinValEx           := vvedtMinVal2.Value ;
  mvc.ParamsRender.MaxValEx           := vvedtMaxVal2.Value ;
  mvc.ParamsRender.StartColorExAsText := cmbStartColor2.Value ;
  mvc.ParamsRender.EndColorExAsText   := cmbEndColor2.Value ;
  mvc.ParamsRender.StartSizeExAsText  := cmbStartSize2.Value  ;
  mvc.ParamsRender.EndSizeExAsText    := cmbEndSize2.Value  ;
end ;

procedure T_panelRenderer.doCallback(
  _sender : TObject ;
  _code   : Integer
) ;

  procedure do_expression_change ;
  begin
    if cmbExpression.Text <> '' then begin
      lblNumOfZones1.Enabled := True ;
      speNumOfZones1.Enabled := True ;
      mvc.DoZonesChange ;
      lblNumOfZones2.Enabled := True ;
      speNumOfZones2.Enabled := True ;
      mvc.DoZonesExChange ;
    end
    else begin
      lblNumOfZones1.Enabled := False ;
      speNumOfZones1.Enabled := False ;
      enableFirst( False ) ;
      lblNumOfZones2.Enabled := False ;
      speNumOfZones2.Enabled := False ;
      enableSecond( False ) ;
    end ;
  end ;

  procedure do_noz1_change ;
  begin
    enableFirst( vvspeNumOfZones1.Value <> 0 ) ;
  end ;

  procedure do_noz2_change ;
  begin
    enableSecond( vvspeNumOfZones2.Value <> 0 ) ;
  end ;

begin
  case _code of
    1 : do_expression_change ;
    2 : do_noz1_change ;
    3 : do_noz2_change ;
  end ;
end ;

procedure T_panelRenderer.doExpressionChange(
  _sender : TObject
) ;
begin
  mvc.DoExpressionChange ;
end ;

procedure T_panelRenderer.doNumberOfZones1Change(
  _sender : TObject
) ;
begin
  mvc.DoZonesChange ;
end ;

procedure T_panelRenderer.doNumberOfZones2Change(
  _sender : TObject
) ;
begin
  mvc.DoZonesExChange ;
end ;

procedure T_panelRenderer.doControlChange(
  _sender: TObject
) ;
begin
  mvc.DoControlChange ;
end ;

{$ENDREGION}

{$REGION 'T_panelMarker'}
function T_panelMarker.fget_HasPreview : Boolean ;
begin
  Result := mvc.HasPreview ;
end ;

procedure T_panelMarker.init ;
begin
  inherited ;

  ItemText := _rsrcna( GIS_RS_LEGEND_PAG_MARKER ) ;

  mvc := oParentWindow.MVC.Marker ;
  mvc.Callback := doCallback ;

  initMarker ;
  initOutline ;
  initSelf ;
end ;

procedure T_panelMarker.initSelf ;
var
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  ckbLegend := TCheckBox.Create( Self.Panel ) ;
  ckbLegend.Parent := Self.Panel ;
  ckbLegend.Position.Y := gpbOutline.Position.Y + gpbOutline.Height + GROUPBOX_GAP ;
  PlaceControl( bd, nil, ckbLegend, LEFT_3COL_1, WIDTH_2COL ) ;
  if bd = bdRightToLeft then
    ckbLegend.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    ckbLegend.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  ckbLegend.Text := _rsrcna( GIS_RS_LEGEND_PRM_INCLUDEINLEGEND ) ;
end ;

procedure T_panelMarker.initMarker ;
var
  t : Single ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbMarker := TGroupBox.Create( Self.Panel ) ;
  gpbMarker.Parent := Self.Panel ;
  gpbMarker.Position.Y := GROUPBOX_TOP ;
  gpbMarker.Height := 512 ;
  PlaceControl( bd, nil, gpbMarker, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbMarker.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbMarker.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbMarker.Text := _rsrcna( GIS_RS_LEGEND_TAB_MARKER ) ;

  t := GROUPBOX_TOP_GAP ;

  lblStyle := TLabel.Create( gpbMarker ) ;
  lblStyle.Parent := gpbMarker ;
  lblStyle.Position.Y := t ;
  PlaceControl( bd, nil, lblStyle, LEFT_3COL_1, WIDTH_3COL ) ;
  lblStyle.Text := _rsrcna( GIS_RS_LEGEND_PRM_STYLE ) ;
  lblStyle.FixSize ;

  lblColor := TLabel.Create( gpbMarker ) ;
  lblColor.Parent := gpbMarker ;
  lblColor.Position.Y := t ;
  PlaceControl( bd, nil, lblColor, LEFT_3COL_2, WIDTH_3COL ) ;
  lblColor.Text := _rsrcna( GIS_RS_LEGEND_PRM_COLOR ) ;
  lblColor.FixSize ;

  lblPattern := TLabel.Create( gpbMarker ) ;
  lblPattern.Parent := gpbMarker ;
  lblPattern.Position.Y := t ;
  PlaceControl( bd, nil, lblPattern, LEFT_3COL_3, WIDTH_3COL ) ;
  lblPattern.Text := _rsrcna( GIS_RS_LEGEND_PRM_PATTERN ) ;
  lblPattern.FixSize ;

  t := t + lblStyle.Height ;

  cmbStyle := TGIS_SymbolComboBox.Create( gpbMarker ) ;
  cmbStyle.Parent := gpbMarker ;
  cmbStyle.Position.Y := t ;
  PlaceControl( bd, nil, cmbStyle, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbStyle.Fill ;
  cmbStyle.OnChange       := doPatternChange ;
  cmbStyle.CustomEvent    := doCustomPattern ;
  cmbStyle.GetBitmapEvent := doGetBitmapMarkerStyle ;

  cmbColor := TGIS_ColorComboBox.Create( gpbMarker ) ;
  cmbColor.Parent := gpbMarker ;
  cmbColor.Position.Y    := t ;
  cmbColor.Height := 28 ;
  PlaceControl( bd, nil, cmbColor, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbColor.Fill( True, True ) ;
  cmbColor.OnChange    := doControlChange ;
  cmbColor.CustomEvent := doCustomColor ;

  cmbPattern := TGIS_PatternComboBox.Create( gpbMarker ) ;
  cmbPattern.Parent := gpbMarker ;
  cmbPattern.Position.Y := t ;
  PlaceControl( bd, nil, cmbPattern, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbPattern.Fill( False ) ;
  cmbPattern.OnChange       := doPatternChange ;
  cmbPattern.CustomEvent    := doCustomPattern ;
  cmbPattern.GetBitmapEvent := doGetBitmapAreaFill ;

  t := t + cmbPattern.Height + 16 ;

  lblSize := TLabel.Create( gpbMarker ) ;
  lblSize.Parent := gpbMarker ;
  lblSize.Position.Y := t ;
  PlaceControl( bd, nil, lblSize, LEFT_3COL_1, WIDTH_3COL ) ;
  lblSize.Text := _rsrcna( GIS_RS_LEGEND_PRM_SIZE ) ;
  lblSize.FixSize ;

  lblSymbolRotate := TLabel.Create( gpbMarker ) ;
  lblSymbolRotate.Parent := gpbMarker ;
  lblSymbolRotate.Position.Y := t ;
  PlaceControl( bd, nil, lblSymbolRotate, LEFT_3COL_2, WIDTH_3COL ) ;
  lblSymbolRotate.Text := _rsrcna( GIS_RS_LEGEND_PRM_SYMBOLROTATE ) ;
  lblSymbolRotate.FixSize ;

  lblSSSize := TLabel.Create( gpbMarker ) ;
  lblSSSize.Parent := gpbMarker ;
  lblSSSize.Position.Y := t ;
  PlaceControl( bd, nil, lblSSSize, LEFT_3COL_3, WIDTH_3COL ) ;
  lblSSSize.Text := _rsrcna( GIS_RS_LEGEND_PRM_SMART_SIZE ) ;
  lblSSSize.FixSize ;

  t := t + lblSymbolRotate.Height ;

  cmbSize := TGIS_SizeComboBox.Create( gpbMarker ) ;
  cmbSize.Parent := gpbMarker ;
  cmbSize.Position.Y := t ;
  PlaceControl( bd, nil, cmbSize, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbSize.Fill( True, False, True, True ) ;
  cmbSize.OnChange := doControlChange ;
  cmbSize.CustomEvent := doCustomSize ;

  cmbSymbolRotate := TGIS_RotationComboBox.Create( gpbMarker ) ;
  cmbSymbolRotate.Parent := gpbMarker ;
  cmbSymbolRotate.Position.Y := t ;
  PlaceControl( bd, nil, cmbSymbolRotate, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbSymbolRotate.Fill( True ) ;
  cmbSymbolRotate.OnChange := doControlChange ;
  cmbSymbolRotate.CustomEvent := doCustomRotation ;

  cmbSSSize := TGIS_SizeComboBox.Create( gpbMarker ) ;
  cmbSSSize.Parent := gpbMarker ;
  cmbSSSize.Position.Y := t ;
  PlaceControl( bd, nil, cmbSSSize, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbSSSize.Fill( False, False, True, False ) ;
  cmbSSSize.OnChange := doControlChange ;
  cmbSSSize.CustomEvent := doCustomSize ;

  t := t + cmbSymbolRotate.Height + GROUPBOX_BOTTOM_GAP ;

  gpbMarker.Height := t ;
end ;

procedure T_panelMarker.initOutline ;
var
  t : Single ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbOutline := TGroupBox.Create( Self.Panel ) ;
  gpbOutline.Parent := Self.Panel ;
  gpbOutline.Position.Y := gpbMarker.Position.Y + gpbMarker.Height + GROUPBOX_GAP ;
  gpbOutline.Height := 512 ;
  PlaceControl( bd, nil, gpbOutline, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbOutline.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbOutline.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbOutline.Text := _rsrcna( GIS_RS_LEGEND_TAB_OUTLINE ) ;

  t := GROUPBOX_TOP_GAP ;

  lblOStyle := TLabel.Create( gpbOutline ) ;
  lblOStyle.Parent := gpbOutline ;
  lblOStyle.Position.Y := t ;
  PlaceControl( bd, nil, lblOStyle, LEFT_3COL_1, WIDTH_3COL ) ;
  lblOStyle.Text := _rsrcna( GIS_RS_LEGEND_PRM_STYLE ) ;
  lblOStyle.FixSize ;

  lblOColor := TLabel.Create( gpbOutline ) ;
  lblOColor.Parent := gpbOutline ;
  lblOColor.Position.Y := t ;
  PlaceControl( bd, nil, lblOColor, LEFT_3COL_2, WIDTH_3COL ) ;
  lblOColor.Text := _rsrcna( GIS_RS_LEGEND_PRM_COLOR ) ;
  lblOColor.Parent := gpbOutline ;
  lblOColor.FixSize ;

  lblOPattern := TLabel.Create( gpbOutline ) ;
  lblOPattern.Parent := gpbOutline ;
  lblOPattern.Position.Y := t ;
  PlaceControl( bd, nil, lblOPattern, LEFT_3COL_3, WIDTH_3COL ) ;
  lblOPattern.Text := _rsrcna( GIS_RS_LEGEND_PRM_PATTERN ) ;
  lblOPattern.FixSize ;

  t := t + lblOStyle.Height ;

  cmbOStyle := TGIS_PenStyleComboBox.Create( gpbOutline ) ;
  cmbOStyle.Parent := gpbOutline ;
  cmbOStyle.Position.Y := t ;
  PlaceControl( bd, nil, cmbOStyle, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbOStyle.Fill( False ) ;
  cmbOStyle.OnChange := doOPatternChange ;
  cmbOStyle.CustomEvent := doCustomPattern ;
  cmbOStyle.GetBitmapEvent := doGetBitmapAreaOutline ;

  cmbOColor := TGIS_ColorComboBox.Create( gpbOutline ) ;
  cmbOColor.Parent := gpbOutline ;
  cmbOColor.Position.Y    := t ;
  cmbOColor.Height := 28 ;
  PlaceControl( bd, nil, cmbOColor, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbOColor.Fill( True, True ) ;
  cmbOColor.OnChange    := doControlChange ;
  cmbOColor.CustomEvent := doCustomColor ;

  cmbOPattern := TGIS_PatternComboBox.Create( gpbOutline ) ;
  cmbOPattern.Parent := gpbOutline ;
  cmbOPattern.Position.X := LEFT_3COL_3 ;
  cmbOPattern.Position.Y := t ;
  cmbOPattern.Width := WIDTH_3COL ;
  PlaceControl( bd, nil, cmbOPattern, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbOPattern.Fill( False ) ;
  cmbOPattern.OnChange       := doOPatternChange ;
  cmbOPattern.CustomEvent    := doCustomPattern ;
  cmbOPattern.GetBitmapEvent := doGetBitmapAreaFill ;

  t := t + cmbOPattern.Height + 16 ;

  lblOWidth := TLabel.Create( gpbOutline ) ;
  lblOWidth.Parent := gpbOutline ;
  lblOWidth.Position.Y := t ;
  PlaceControl( bd, nil, lblOWidth, LEFT_3COL_1, WIDTH_3COL ) ;
  lblOWidth.Text := _rsrcna( GIS_RS_LEGEND_PRM_WIDTH ) ;
  lblOWidth.FixSize ;

  t := t + lblOWidth.Height ;

  cmbOWidth := TGIS_SizeComboBox.Create( gpbOutline ) ;
  cmbOWidth.Parent := gpbOutline ;
  cmbOWidth.Position.Y := t ;
  PlaceControl( bd, nil, cmbOWidth, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbOWidth.Fill( False, True, True, True ) ;
  cmbOWidth.OnChange := doControlChange ;
  cmbOWidth.CustomEvent := doCustomSize ;

  t := t + cmbOWidth.Height + GROUPBOX_BOTTOM_GAP ;

  gpbOutline.Height := t ;
end ;

procedure T_panelMarker.Read ;
begin
  lockUpdates ;
  try
    ckbLegend.IsChecked     := mvc.ParamsMarker.ShowLegend ;

    cmbStyle.Value        := mvc.ParamsMarker.StyleAsText ;
    cmbColor.Value        := mvc.ParamsMarker.ColorAsText ;
    cmbPattern.Value      := mvc.ParamsMarker.PatternAsText ;
    cmbSymbolRotate.Value := mvc.ParamsMarker.SymbolRotateAsText ;
    cmbSize.Value         := mvc.ParamsMarker.SizeAsText ;

    // Outline
    cmbOStyle.Value       := mvc.ParamsMarker.OutlineStyleAsText ;
    cmbOWidth.Value       := mvc.ParamsMarker.OutlineWidthAsText ;
    cmbOColor.Value       := mvc.ParamsMarker.OutlineColorAsText ;
    cmbOPattern.Value     := mvc.ParamsMarker.OutlinePatternAsText ;

    // Smart size
    cmbSSSize.Value       := mvc.ParamsMarker.SmartSizeAsText ;

    mvc.DoPatternChange ;
    mvc.DoOPatternChange ;
    mvc.DoSmartSizeFieldChange ;
  finally
    unlockUpdates ;
  end ;
end ;

procedure T_panelMarker.Write ;
begin
  mvc.ParamsMarker.ShowLegend           := ckbLegend.IsChecked ;

  mvc.ParamsMarker.StyleAsText          := cmbStyle.Value ;
  mvc.ParamsMarker.ColorAsText          := cmbColor.Value ;
  mvc.ParamsMarker.PatternAsText        := cmbPattern.Value ;
  mvc.ParamsMarker.SymbolRotateAsText   := cmbSymbolRotate.Value ;
  mvc.ParamsMarker.SizeAsText           := cmbSize.Value ;

  // Outline
  mvc.ParamsMarker.OutlineStyleAsText   := cmbOStyle.Value ;
  mvc.ParamsMarker.OutlineWidthAsText   := cmbOWidth.Value ;
  mvc.ParamsMarker.OutlineColorAsText   := cmbOColor.Value ;
  mvc.ParamsMarker.OutlinePatternAsText := cmbOPattern.Value ;

  // Smart size
  mvc.ParamsMarker.SmartSizeAsText      := cmbSSSize.Value ;
end ;

procedure T_panelMarker.PreparePreview(
  const _viewer : IGIS_Viewer
) ;
begin
  mvc.PreparePreview( _viewer ) ;
end ;

procedure T_panelMarker.UpdatePreview ;
var
  ll        : TGIS_Layer ;
  paramsvec : TGIS_ParamsSectionVector ;

begin
  oParentWindow.tmrUpdate.Enabled := False ;
  if oParentWindow.gisPreview.IsEmpty then exit ;
  oParentWindow.tmrUpdate.Enabled := True  ;

  ll := TGIS_Layer( oParentWindow.gisPreview.Items[0] ) ;

  paramsvec := TGIS_ParamsSectionVector( ll.Params ) ;

  mvc.SectionWrite( paramsvec ) ;

  paramsvec.Labels.Value := '' ;
  paramsvec.Labels.Field := '' ;

  paramsvec.Render.Chart := '' ;
  paramsvec.Chart.Values := '' ;

  paramsvec.Query        := '' ;

  with TGIS_ParamsSection( ll.Params ) do begin
    MinZoom  := 0              ;
    MaxZoom  := GIS_MAX_DOUBLE ;
    MinScale := 0              ;
    MaxScale := GIS_MAX_DOUBLE ;
  end ;

  // avoid unreasonable preview
  paramsvec.Marker.Color        := normalize_color(
                                     paramsvec.Marker.Color,
                                     paramsvec.Marker.ColorAsText
                                   ) ;
  paramsvec.Marker.OutlineColor := normalize_color(
                                     paramsvec.Marker.OutlineColor,
                                     paramsvec.Marker.OutlineColorAsText
                                   ) ;
  paramsvec.Marker.Size         := normalize_size(
                                     oParentWindow.gisPreview,
                                     paramsvec.Marker.Size,
                                     paramsvec.Marker.SizeAsText,
                                     MAX_PREVIEW_SIZE_MAKER
                                   ) ;
  paramsvec.Marker.OutlineWidth := normalize_size(
                                     oParentWindow.gisPreview,
                                     paramsvec.Marker.OutlineWidth,
                                     paramsvec.Marker.OutlineWidthAsText,
                                     MAX_PREVIEW_SIZE_OUTLINE
                                   ) ;
  paramsvec.Marker.SymbolRotate := normalize_angle(
                                     paramsvec.Marker.SymbolRotate,
                                     paramsvec.Marker.SymbolRotateAsText
                                   ) ;
end ;

procedure T_panelMarker.doCallback(
  _sender : TObject ;
  _code   : Integer
) ;

  procedure do_ssfield_change ;
  begin

  end ;

  procedure do_update_symbol_bitmap ;
  var
    bsymfnt : Boolean     ;
    bsymcgm : Boolean     ;
    bsymsvg : Boolean     ;
    bsympic : Boolean     ;
    bsymlin : Boolean     ;
    sym     : TGIS_SymbolAbstract ;
    bbmp    : Boolean     ;
  begin
    sym := mvc.ParamsMarker.Symbol ;

    bsymfnt := Assigned( sym ) and ( sym is TGIS_SymbolFont    ) ;
    bsymcgm := Assigned( sym ) and ( sym is TGIS_SymbolCGM     ) ;
    bsymsvg := Assigned( sym ) and ( sym is TGIS_SymbolSVG     ) ;
    bsympic := Assigned( sym ) and ( sym is TGIS_SymbolPicture ) ;
    bsymlin := Assigned( sym ) and ( ( sym is TGIS_SymbolLine   ) or
                                     ( sym is TGIS_SymbolLineEx )
                                    ) ;
    bbmp := not TGIS_Bitmap.IsNilOrEmpty( mvc.ParamsMarker.Bitmap ) ;

    if bsymfnt or bsymcgm or bsymsvg or bsymlin then begin
      lblColor.Enabled           := True ;
      cmbColor.Enabled           := True ;
      lblPattern.Enabled         := False ;
      cmbPattern.Enabled         := False ;
      lblSymbolRotate.Enabled    := True ;
      cmbSymbolRotate.Enabled    := True ;
    end
    else if bsympic then begin
      lblColor.Enabled           := False ;
      cmbColor.Enabled           := False ;
      lblPattern.Enabled         := False ;
      cmbPattern.Enabled         := False ;
      lblSymbolRotate.Enabled    := True ;
      cmbSymbolRotate.Enabled    := True ;
    end
    else if bbmp then begin
      lblColor.Enabled           := False ;
      cmbColor.Enabled           := False ;
      lblPattern.Enabled         := True ;
      cmbPattern.Enabled         := True ;
      lblSymbolRotate.Enabled    := False ;
      cmbSymbolRotate.Enabled    := False ;
    end
    else begin
      lblColor.Enabled           := True ;
      cmbColor.Enabled           := True ;
      lblPattern.Enabled         := True  ;
      cmbPattern.Enabled         := True  ;
      lblSymbolRotate.Enabled    := False ;
      cmbSymbolRotate.Enabled    := False ;
    end ;
  end ;

  procedure do_update_osymbol_obitmap ;
  var
    bsymfnt : Boolean     ;
    bsymcgm : Boolean     ;
    bsymsvg : Boolean     ;
    bsympic : Boolean     ;
    bsymlin : Boolean     ;
    sym     : TGIS_SymbolAbstract ;
    bbmp    : Boolean     ;
  begin
    sym := mvc.ParamsMarker.Symbol ;

    bsymfnt := Assigned( sym ) and ( sym is TGIS_SymbolFont    ) ;
    bsymcgm := Assigned( sym ) and ( sym is TGIS_SymbolCGM     ) ;
    bsymsvg := Assigned( sym ) and ( sym is TGIS_SymbolSVG     ) ;
    bsympic := Assigned( sym ) and ( sym is TGIS_SymbolPicture ) ;
    bsymlin := Assigned( sym ) and ( ( sym is TGIS_SymbolLine   ) or
                                     ( sym is TGIS_SymbolLineEx )
                                    ) ;
    bbmp := not TGIS_Bitmap.IsNilOrEmpty( mvc.ParamsMarker.OutlineBitmap ) ;

    if bsymfnt or bsymcgm or bsymsvg or bsymlin then begin
      lblOColor.Enabled           := True ;
      cmbOColor.Enabled           := True ;
      lblOPattern.Enabled         := False ;
      cmbOPattern.Enabled         := False ;
    end
    else if bsympic then begin
      lblOColor.Enabled           := False ;
      cmbOColor.Enabled           := False ;
      lblOPattern.Enabled         := False ;
      cmbOPattern.Enabled         := False ;
    end
    else if bbmp then begin
      lblOColor.Enabled           := False ;
      cmbOColor.Enabled           := False ;
      lblOPattern.Enabled         := True ;
      cmbOPattern.Enabled         := True ;
    end
    else begin
      lblOColor.Enabled           := True ;
      cmbOColor.Enabled           := True ;
      lblOPattern.Enabled         := True  ;
      cmbOPattern.Enabled         := True  ;
    end ;
  end ;

begin
  if not blockedUpdates then begin
    Write ;
    UpdatePreview ;
  end ;

  case _code of
    60  : do_ssfield_change ;
    100 : do_update_symbol_bitmap ;
    130 : do_update_osymbol_obitmap
  end ;

end ;

procedure T_panelMarker.doOPatternChange(
  _sender : TObject
) ;
begin
  mvc.DoOPatternChange ;
end;

procedure T_panelMarker.doPatternChange(
  _sender : TObject
) ;
begin
  mvc.DoPatternChange ;
end;

procedure T_panelMarker.doSmartSizeFieldChange(
  _sender : TObject
) ;
begin
  mvc.DoSmartSizeFieldChange ;
end ;

procedure T_panelMarker.doControlChange(
  _sender: TObject
) ;
begin
  mvc.DoControlChange ;
end ;
{$ENDREGION}

{$REGION 'T_panelLine'}
function T_panelLine.fget_HasPreview : Boolean ;
begin
  Result := mvc.HasPreview ;
end ;

procedure T_panelLine.init ;
begin
  inherited ;

  ItemText := _rsrcna( GIS_RS_LEGEND_PAG_LINE ) ;

  mvc := oParentWindow.MVC.Line ;
  mvc.Callback := doCallback ;

  initLine ;
  initOutline ;
  initSelf ;
end ;

procedure T_panelLine.initSelf ;
var
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;
  ckbLegend := TCheckBox.Create( Self.Panel ) ;
  ckbLegend.Parent := Self.Panel ;
  ckbLegend.Position.Y := gpbOutline.Position.Y + gpbOutline.Height + GROUPBOX_GAP ;
  PlaceControl( bd, nil, ckbLegend, LEFT_3COL_1, WIDTH_2COL ) ;
  if bd = bdRightToLeft then
    ckbLegend.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    ckbLegend.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  ckbLegend.Text := _rsrcna( GIS_RS_LEGEND_PRM_INCLUDEINLEGEND ) ;
end ;

procedure T_panelLine.initLine ;
var
  t : Single ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbLine := TGroupBox.Create( Self.Panel ) ;
  gpbLine.Parent := Self.Panel ;
  gpbLine.Position.Y  := GROUPBOX_TOP ;
  gpbLine.Height := 512 ;
  PlaceControl( bd, nil, gpbLine, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbLine.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbLine.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbLine.Text := _rsrcna( GIS_RS_LEGEND_TAB_LINE ) ;

  t := GROUPBOX_TOP_GAP ;

  lblStyle := TLabel.Create( gpbLine ) ;
  lblStyle.Parent := gpbLine ;
  lblStyle.Position.Y := t ;
  PlaceControl( bd, nil, lblStyle, LEFT_3COL_1, WIDTH_3COL ) ;
  lblStyle.Text := _rsrcna( GIS_RS_LEGEND_PRM_STYLE ) ;
  lblStyle.FixSize ;

  lblColor := TLabel.Create( gpbLine ) ;
  lblColor.Parent := gpbLine ;
  lblColor.Position.Y := t ;
  PlaceControl( bd, nil, lblColor, LEFT_3COL_2, WIDTH_3COL ) ;
  lblColor.Text := _rsrcna( GIS_RS_LEGEND_PRM_COLOR ) ;
  lblColor.FixSize ;

  lblPattern := TLabel.Create( gpbLine ) ;
  lblPattern.Parent := gpbLine ;
  lblPattern.Position.Y := t ;
  PlaceControl( bd, nil, lblPattern, LEFT_3COL_3, WIDTH_3COL ) ;
  lblPattern.Text := _rsrcna( GIS_RS_LEGEND_PRM_PATTERN ) ;
  lblPattern.FixSize ;

  t := t + lblStyle.Height ;

  cmbStyle := TGIS_PenStyleComboBox.Create( gpbLine ) ;
  cmbStyle.Parent := gpbLine ;
  cmbStyle.Position.Y := t ;
  PlaceControl( bd, nil, cmbStyle, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbStyle.Fill( True ) ;
  cmbStyle.OnChange := doPatternChange ;
  cmbStyle.CustomEvent := doCustomPattern ;
  cmbStyle.GetBitmapEvent := doGetBitmapAreaOutline ;

  cmbColor := TGIS_ColorComboBox.Create( gpbLine ) ;
  cmbColor.Parent := gpbLine ;
  cmbColor.Position.Y    := t ;
  cmbColor.Height := 28 ;
  PlaceControl( bd, nil, cmbColor, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbColor.Fill( True, True ) ;
  cmbColor.OnChange    := doControlChange ;
  cmbColor.CustomEvent := doCustomColor ;

  cmbPattern := TGIS_PatternComboBox.Create( gpbLine ) ;
  cmbPattern.Parent := gpbLine ;
  cmbPattern.Position.Y := t ;
  PlaceControl( bd, nil, cmbPattern, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbPattern.Fill( True ) ;
  cmbPattern.OnChange       := doPatternChange ;
  cmbPattern.CustomEvent    := doCustomPattern ;
  cmbPattern.GetBitmapEvent := doGetBitmapAreaFill ;

  t := t + cmbPattern.Height + 16 ;

  lblWidth := TLabel.Create( gpbLine ) ;
  lblWidth.Parent := gpbLine ;
  lblWidth.Position.Y := t ;
  PlaceControl( bd, nil, lblWidth, LEFT_3COL_1, WIDTH_3COL ) ;
  lblWidth.Text := _rsrcna( GIS_RS_LEGEND_PRM_WIDTH ) ;
  lblWidth.FixSize ;

  lblSymbolGap := TLabel.Create( gpbLine ) ;
  lblSymbolGap.Parent := gpbLine ;
  lblSymbolGap.Position.Y := t ;
  PlaceControl( bd, nil, lblSymbolGap, LEFT_3COL_2, WIDTH_3COL ) ;
  lblSymbolGap.Text := _rsrcna( GIS_RS_LEGEND_PRM_SYMBOLGAP ) ;
  lblSymbolGap.FixSize ;

  lblSymbolRotate := TLabel.Create( gpbLine ) ;
  lblSymbolRotate.Parent := gpbLine ;
  lblSymbolRotate.Position.Y := t ;
  PlaceControl( bd, nil, lblSymbolRotate, LEFT_3COL_3, WIDTH_3COL ) ;
  lblSymbolRotate.Text := _rsrcna( GIS_RS_LEGEND_PRM_SYMBOLROTATE ) ;
  lblSymbolRotate.FixSize ;

  t := t + lblSymbolRotate.Height ;

  cmbWidth := TGIS_SizeComboBox.Create( gpbLine ) ;
  cmbWidth.Parent := gpbLine ;
  cmbWidth.Position.Y := t ;
  PlaceControl( bd, nil, cmbWidth, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbWidth.Fill( False, True, True, True ) ;
  cmbWidth.OnChange := doControlChange ;
  cmbWidth.CustomEvent := doCustomSize ;

  cmbSymbolGap := TGIS_SizeComboBox.Create( gpbLine ) ;
  cmbSymbolGap.Parent := gpbLine ;
  cmbSymbolGap.Position.Y := t ;
  PlaceControl( bd, nil, cmbSymbolGap, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbSymbolGap.Fill( False, False, False, False ) ;
  cmbSymbolGap.OnChange := doControlChange ;
  cmbSymbolGap.CustomEvent := doCustomSize ;

  cmbSymbolRotate := TGIS_RotationComboBox.Create( gpbLine ) ;
  cmbSymbolRotate.Parent := gpbLine ;
  cmbSymbolRotate.Position.Y := t ;
  PlaceControl( bd, nil, cmbSymbolRotate, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbSymbolRotate.Fill( True ) ;
  cmbSymbolRotate.OnChange := doControlChange ;
  cmbSymbolRotate.CustomEvent := doCustomRotation ;

  t := t + cmbSymbolRotate.Height + 16 ;

  lblSSSize := TLabel.Create( gpbLine ) ;
  lblSSSize.Parent := gpbLine ;
  lblSSSize.Position.Y := t ;
  PlaceControl( bd, nil, lblSSSize, LEFT_3COL_1, WIDTH_3COL ) ;
  lblSSSize.Text := _rsrcna( GIS_RS_LEGEND_PRM_SMART_SIZE ) ;
  lblSSSize.FixSize ;

  t := t + lblSSSize.Height ;

  cmbSSSize := TGIS_SizeComboBox.Create( gpbLine ) ;
  cmbSSSize.Parent := gpbLine ;
  cmbSSSize.Position.Y := t ;
  PlaceControl( bd, nil, cmbSSSize, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbSSSize.Fill( False, False, True, False ) ;
  cmbSSSize.OnChange := doControlChange ;
  cmbSSSize.CustomEvent := doCustomSize ;

  t := t + cmbSSSize.Height + GROUPBOX_BOTTOM_GAP ;

  gpbLine.Height := t ;
end ;

procedure T_panelLine.initOutline ;
var
  t : Single ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbOutline := TGroupBox.Create( Self.Panel ) ;
  gpbOutline.Parent := Self.Panel ;
  gpbOutline.Position.Y  := gpbLine.Position.Y + gpbLine.Height + GROUPBOX_GAP ;
  gpbOutline.Height := 512 ;
  PlaceControl( bd, nil, gpbOutline, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbOutline.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbOutline.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbOutline.Text := _rsrcna( GIS_RS_LEGEND_TAB_OUTLINE ) ;

  t := GROUPBOX_TOP_GAP ;

  lblOStyle := TLabel.Create( gpbOutline ) ;
  lblOStyle.Parent := gpbOutline ;
  lblOStyle.Position.X := LEFT_3COL_1 ;
  lblOStyle.Position.Y := t ;
  PlaceControl( bd, nil, lblOStyle, LEFT_3COL_1, WIDTH_3COL ) ;
  lblOStyle.Text := _rsrcna( GIS_RS_LEGEND_PRM_STYLE ) ;
  lblOStyle.FixSize ;

  lblOColor := TLabel.Create( gpbOutline ) ;
  lblOColor.Parent := gpbOutline ;
  lblOColor.Position.Y := t ;
  PlaceControl( bd, nil, lblOColor, LEFT_3COL_2, WIDTH_3COL ) ;
  lblOColor.Text := _rsrcna( GIS_RS_LEGEND_PRM_COLOR ) ;
  lblOColor.Parent := gpbOutline ;
  lblOColor.FixSize ;

  lblOPattern := TLabel.Create( gpbOutline ) ;
  lblOPattern.Parent := gpbOutline ;
  lblOPattern.Position.Y := t ;
  PlaceControl( bd, nil, lblOPattern, LEFT_3COL_3, WIDTH_3COL ) ;
  lblOPattern.Text := _rsrcna( GIS_RS_LEGEND_PRM_PATTERN ) ;
  lblOPattern.FixSize ;

  t := t + lblOStyle.Height ;

  cmbOStyle := TGIS_PenStyleComboBox.Create( gpbOutline ) ;
  cmbOStyle.Parent := gpbOutline ;
  cmbOStyle.Position.Y := t ;
  PlaceControl( bd, nil, cmbOStyle, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbOStyle.Fill( False ) ;
  cmbOStyle.OnChange := doOPatternChange ;
  cmbOStyle.CustomEvent := doCustomPattern ;
  cmbOStyle.GetBitmapEvent := doGetBitmapAreaOutline ;

  cmbOColor := TGIS_ColorComboBox.Create( gpbOutline ) ;
  cmbOColor.Parent := gpbOutline ;
  cmbOColor.Position.Y    := t ;
  cmbOColor.Height := 28 ;
  PlaceControl( bd, nil, cmbOColor, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbOColor.Fill( True, True ) ;
  cmbOColor.OnChange    := doControlChange ;
  cmbOColor.CustomEvent := doCustomColor ;

  cmbOPattern := TGIS_PatternComboBox.Create( gpbOutline ) ;
  cmbOPattern.Parent := gpbOutline ;
  cmbOPattern.Position.Y := t ;
  PlaceControl( bd, nil, cmbOPattern, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbOPattern.Fill( False ) ;
  cmbOPattern.OnChange       := doOPatternChange ;
  cmbOPattern.CustomEvent    := doCustomPattern ;
  cmbOPattern.GetBitmapEvent := doGetBitmapAreaFill ;

  t := t + cmbOPattern.Height + 16 ;

  lblOWidth := TLabel.Create( gpbOutline ) ;
  lblOWidth.Parent := gpbOutline ;
  lblOWidth.Position.Y := t ;
  PlaceControl( bd, nil, lblOWidth, LEFT_3COL_1, WIDTH_3COL ) ;
  lblOWidth.Text := _rsrcna( GIS_RS_LEGEND_PRM_WIDTH ) ;
  lblOWidth.FixSize ;

  t := t + lblOWidth.Height ;

  cmbOWidth := TGIS_SizeComboBox.Create( gpbOutline ) ;
  cmbOWidth.Parent := gpbOutline ;
  cmbOWidth.Position.Y := t ;
  PlaceControl( bd, nil, cmbOWidth, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbOWidth.Fill( False, True, True, True ) ;
  cmbOWidth.OnChange := doControlChange ;
  cmbOWidth.CustomEvent := doCustomSize ;

  t := t + cmbOWidth.Height + GROUPBOX_BOTTOM_GAP ;

  gpbOutline.Height := t ;
end ;


procedure T_panelLine.Read ;
var
  str : String ;
begin
  lockUpdates ;
  try
    ckbLegend.IsChecked     := mvc.ParamsLine.ShowLegend ;

    cmbStyle.Value          := mvc.ParamsLine.StyleAsText ;
    cmbWidth.Value          := mvc.ParamsLine.WidthAsText ;
    cmbColor.Value          := mvc.ParamsLine.ColorAsText ;
    cmbPattern.Value        := mvc.ParamsLine.PatternAsText ;
    cmbSymbolGap.Value      := mvc.ParamsLine.SymbolGapAsText ;
    cmbSymbolRotate.Value   := mvc.ParamsLine.SymbolRotateAsText ;

    // Outline
    cmbOStyle.Value         := mvc.ParamsLine.OutlineStyleAsText ;
    cmbOWidth.Value         := mvc.ParamsLine.OutlineWidthAsText ;
    cmbOColor.Value         := mvc.ParamsLine.OutlineColorAsText ;
    cmbOPattern.Value       := mvc.ParamsLine.OutlinePatternAsText ;

    // Smart size
    cmbSSSize.Value         := mvc.ParamsLine.SmartSizeAsText ;

    mvc.DoPatternChange ;
    mvc.DoOPatternChange ;
    mvc.DoSmartSizeFieldChange ;

    str := Copy( cmbStyle.Value, StringFirst, 4 ) ;
    if str = GIS_PARAMTXT_TYPE_CODE then
      lineSymbol :=
        Copy( cmbStyle.Value, StringFirst+6, Length( cmbStyle.Value ) ) ;
  finally
    unlockUpdates ;
  end ;
end ;

procedure T_panelLine.Write ;
begin
  mvc.ParamsLine.ShowLegend           := ckbLegend.IsChecked ;

  mvc.ParamsLine.StyleAsText          := cmbStyle.Value ;
  mvc.ParamsLine.WidthAsText          := cmbWidth.Value ;
  mvc.ParamsLine.ColorAsText          := cmbColor.Value ;
  mvc.ParamsLine.PatternAsText        := cmbPattern.Value ;
  mvc.ParamsLine.SymbolGapAsText      := cmbSymbolGap.Value ;
  mvc.ParamsLine.SymbolRotateAsText   := cmbSymbolRotate.Value ;

  // Outline
  mvc.ParamsLine.OutlineStyleAsText   := cmbOStyle.Value ;
  mvc.ParamsLine.OutlineWidthAsText   := cmbOWidth.Value ;
  mvc.ParamsLine.OutlineColorAsText   := cmbOColor.Value ;
  mvc.ParamsLine.OutlinePatternAsText := cmbOPattern.Value ;

  // Smart size
  mvc.ParamsLine.SmartSizeAsText      := cmbSSSize.Value ;
end ;

procedure T_panelLine.PreparePreview(
  const _viewer : IGIS_Viewer
) ;
begin
  mvc.PreparePreview( _viewer ) ;
end ;

procedure T_panelLine.UpdatePreview ;
var
  ll        : TGIS_Layer ;
  paramsvec : TGIS_ParamsSectionVector ;
begin
  oParentWindow.tmrUpdate.Enabled := False ;
  if oParentWindow.gisPreview.IsEmpty then exit ;
  oParentWindow.tmrUpdate.Enabled := True  ;

  ll := TGIS_Layer( oParentWindow.gisPreview.Items[0] ) ;

  paramsvec := TGIS_ParamsSectionVector( ll.Params ) ;

  mvc.SectionWrite( paramsvec ) ;

  paramsvec.Labels.Value := '' ;
  paramsvec.Labels.Field := '' ;

  paramsvec.Render.Chart := '' ;
  paramsvec.Chart.Values := '' ;
  paramsvec.Query        := '' ;

  with TGIS_ParamsSection( ll.Params ) do begin
    MinZoom  := 0              ;
    MaxZoom  := GIS_MAX_DOUBLE ;
    MinScale := 0              ;
    MaxScale := GIS_MAX_DOUBLE ;
  end ;

  // avoid unreasonable preview
  paramsvec.Line.Color          := normalize_color(
                                     paramsvec.Line.Color,
                                     paramsvec.Line.ColorAsText
                                   ) ;
  paramsvec.Line.OutlineColor   := normalize_color(
                                     paramsvec.Line.OutlineColor,
                                     paramsvec.Line.OutlineColorAsText
                                   ) ;
  paramsvec.Line.Width          := normalize_size(
                                     oParentWindow.gisPreview,
                                     paramsvec.Line.Width,
                                     paramsvec.Line.WidthAsText,
                                     MAX_PREVIEW_SIZE_LINE
                                   ) ;
  paramsvec.Line.OutlineWidth   := normalize_size(
                                     oParentWindow.gisPreview,
                                     paramsvec.Line.OutlineWidth,
                                     paramsvec.Line.OutlineWidthAsText,
                                     MAX_PREVIEW_SIZE_OUTLINE
                                   ) ;
  paramsvec.Line.SymbolRotate   := normalize_angle(
                                     paramsvec.Line.SymbolRotate,
                                     paramsvec.Line.SymbolRotateAsText
                                   ) ;
end ;

procedure T_panelLine.doCallback(
  _sender : TObject ;
  _code   : Integer
) ;

  procedure do_ssfield_change ;
  begin

  end ;

  procedure do_update_symbol_bitmap ;
  var
    bsymfnt : Boolean     ;
    bsymcgm : Boolean     ;
    bsymsvg : Boolean     ;
    bsympic : Boolean     ;
    bsymlin : Boolean     ;
    sym     : TGIS_SymbolAbstract ;
    bbmp    : Boolean     ;
  begin
    sym := mvc.ParamsLine.Symbol ;

    bsymfnt := Assigned( sym ) and ( sym is TGIS_SymbolFont    ) ;
    bsymcgm := Assigned( sym ) and ( sym is TGIS_SymbolCGM     ) ;
    bsymsvg := Assigned( sym ) and ( sym is TGIS_SymbolSVG     ) ;
    bsympic := Assigned( sym ) and ( sym is TGIS_SymbolPicture ) ;
    bsymlin := Assigned( sym ) and ( ( sym is TGIS_SymbolLine   ) or
                                     ( sym is TGIS_SymbolLineEx )
                                    ) ;
    bbmp := not TGIS_Bitmap.IsNilOrEmpty( mvc.ParamsLine.Bitmap ) ;

    if bsymfnt or bsymcgm or bsymsvg or bsymlin then begin
      lblColor.Enabled           := True ;
      cmbColor.Enabled           := True ;
      lblPattern.Enabled         := False ;
      cmbPattern.Enabled         := False ;
      lblSymbolGap.Enabled       := True ;
      cmbSymbolGap.Enabled       := True ;
      lblSymbolRotate.Enabled    := True ;
      cmbSymbolRotate.Enabled    := True ;
    end
    else if bsympic then begin
      lblColor.Enabled           := False ;
      cmbColor.Enabled           := False ;
      lblPattern.Enabled         := False ;
      cmbPattern.Enabled         := False ;
      lblSymbolGap.Enabled       := True ;
      cmbSymbolGap.Enabled       := True ;
      lblSymbolRotate.Enabled    := True ;
      cmbSymbolRotate.Enabled    := True ;
    end
    else if bbmp then begin
      lblColor.Enabled           := False ;
      cmbColor.Enabled           := False ;
      lblPattern.Enabled         := True ;
      cmbPattern.Enabled         := True ;
      lblSymbolGap.Enabled       := False ;
      cmbSymbolGap.Enabled       := False ;
      lblSymbolRotate.Enabled    := False ;
      cmbSymbolRotate.Enabled    := False ;
    end
    else begin
      lblColor.Enabled           := True ;
      cmbColor.Enabled           := True ;
      lblPattern.Enabled         := True  ;
      cmbPattern.Enabled         := True  ;
      lblSymbolGap.Enabled       := False ;
      cmbSymbolGap.Enabled       := False ;
      lblSymbolRotate.Enabled    := False ;
      cmbSymbolRotate.Enabled    := False ;
    end ;
  end ;

  procedure do_update_osymbol_obitmap ;
  var
    bsymfnt : Boolean     ;
    bsymcgm : Boolean     ;
    bsymsvg : Boolean     ;
    bsympic : Boolean     ;
    bsymlin : Boolean     ;
    sym     : TGIS_SymbolAbstract ;
    bbmp    : Boolean     ;
  begin
    sym := mvc.ParamsLine.Symbol ;

    bsymfnt := Assigned( sym ) and ( sym is TGIS_SymbolFont    ) ;
    bsymcgm := Assigned( sym ) and ( sym is TGIS_SymbolCGM     ) ;
    bsymsvg := Assigned( sym ) and ( sym is TGIS_SymbolSVG     ) ;
    bsympic := Assigned( sym ) and ( sym is TGIS_SymbolPicture ) ;
    bsymlin := Assigned( sym ) and ( ( sym is TGIS_SymbolLine   ) or
                                     ( sym is TGIS_SymbolLineEx )
                                    ) ;
    bbmp := not TGIS_Bitmap.IsNilOrEmpty( mvc.ParamsLine.OutlineBitmap );

    if bsymfnt or bsymcgm or bsymsvg or bsymlin then begin
      lblOColor.Enabled           := True ;
      cmbOColor.Enabled           := True ;
      lblOPattern.Enabled         := False ;
      cmbOPattern.Enabled         := False ;
    end
    else if bsympic then begin
      lblOColor.Enabled           := False ;
      cmbOColor.Enabled           := False ;
      lblOPattern.Enabled         := False ;
      cmbOPattern.Enabled         := False ;
    end
    else if bbmp then begin
      lblOColor.Enabled           := False ;
      cmbOColor.Enabled           := False ;
      lblOPattern.Enabled         := False ;
      cmbOPattern.Enabled         := False ;
    end
    else begin
      lblOColor.Enabled           := True ;
      cmbOColor.Enabled           := True ;
      lblOPattern.Enabled         := True  ;
      cmbOPattern.Enabled         := True  ;
    end ;
  end ;

begin
  if not blockedUpdates then begin
    Write ;
    UpdatePreview ;
  end ;

  case _code of
    60  : do_ssfield_change ;
    100 : do_update_symbol_bitmap ;
    130 : do_update_osymbol_obitmap
  end ;

end ;


procedure T_panelLine.doOPatternChange(
  _sender : TObject
) ;
begin
  mvc.DoOPatternChange ;
end;

procedure T_panelLine.doPatternChange(
  _sender : TObject
) ;
begin
  mvc.DoPatternChange ;
end;

procedure T_panelLine.doSmartSizeFieldChange(
  _sender : TObject
) ;
begin
  mvc.DoSmartSizeFieldChange ;
end ;

procedure T_panelLine.doControlChange(
  _sender: TObject
) ;
begin
  mvc.DoControlChange ;
end ;

{$ENDREGION}

{$REGION 'T_panelArea'}
function T_panelArea.fget_HasPreview : Boolean ;
begin
  Result := mvc.HasPreview ;
end ;

procedure T_panelArea.init ;
begin
  inherited ;

  ItemText := _rsrcna( GIS_RS_LEGEND_PAG_AREA ) ;

  mvc := oParentWindow.MVC.Area ;
  mvc.Callback := doCallback ;

  initArea ;
  initOutline ;
  initSelf ;
end ;

procedure T_panelArea.initSelf ;
var
  t : Single ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  ckbLegend := TCheckBox.Create( Self.Panel ) ;
  ckbLegend.Parent := Self.Panel ;
  ckbLegend.Position.Y := gpbOutline.Position.Y + gpbOutline.Height + GROUPBOX_GAP ;
  PlaceControl( bd, nil, ckbLegend, LEFT_3COL_1, WIDTH_2COL ) ;
  if bd = bdRightToLeft then
    ckbLegend.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    ckbLegend.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  ckbLegend.Text := _rsrcna( GIS_RS_LEGEND_PRM_INCLUDEINLEGEND ) ;
end ;

procedure T_panelArea.initArea ;
var
  t : Single ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbArea := TGroupBox.Create( Self.Panel ) ;
  gpbArea.Parent := Self.Panel ;
  gpbArea.Position.Y := GROUPBOX_TOP ;
  gpbArea.Height := 512 ;
  PlaceControl( bd, nil, gpbArea, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbArea.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbArea.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbArea.Text := _rsrcna( GIS_RS_LEGEND_TAB_AREA ) ;

  t := GROUPBOX_TOP_GAP ;

  lblPattern := TLabel.Create( gpbArea ) ;
  lblPattern.Parent := gpbArea ;
  lblPattern.Position.Y := t ;
  PlaceControl( bd, nil, lblPattern, LEFT_3COL_1, WIDTH_3COL ) ;
  lblPattern.Text := _rsrcna( GIS_RS_LEGEND_PRM_PATTERN ) ;
  lblPattern.FixSize ;

  lblColor := TLabel.Create( gpbArea ) ;
  lblColor.Parent := gpbArea ;
  lblColor.Position.Y := t ;
  PlaceControl( bd, nil, lblColor, LEFT_3COL_2, WIDTH_3COL ) ;
  lblColor.Text := _rsrcna( GIS_RS_LEGEND_PRM_COLOR ) ;
  lblColor.FixSize ;

  t := t + lblColor.Height ;

  cmbPattern := TGIS_PatternComboBox.Create( gpbArea ) ;
  cmbPattern.Parent := gpbArea ;
  cmbPattern.Position.Y := t ;
  PlaceControl( bd, nil, cmbPattern, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbPattern.Fill( True ) ;
  cmbPattern.OnChange       := doPatternChange ;
  cmbPattern.CustomEvent    := doCustomPattern ;
  cmbPattern.GetBitmapEvent := doGetBitmapAreaFill ;

  cmbColor := TGIS_ColorComboBox.Create( gpbArea ) ;
  cmbColor.Parent := gpbArea ;
  cmbColor.Position.Y    := t ;
  cmbColor.Height := 28 ;
  PlaceControl( bd, nil, cmbColor, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbColor.Fill( True, True ) ;
  cmbColor.OnChange    := doControlChange ;
  cmbColor.CustomEvent := doCustomColor ;

  t := t + cmbColor.Height + 16 ;

  lblSymbolSize := TLabel.Create( gpbArea ) ;
  lblSymbolSize.Parent := gpbArea ;
  lblSymbolSize.Position.Y := t ;
  PlaceControl( bd, nil, lblSymbolSize, LEFT_3COL_1, WIDTH_3COL ) ;
  lblSymbolSize.Text := _rsrcna( GIS_RS_LEGEND_PRM_SYMBOLSIZE ) ;
  lblSymbolSize.FixSize ;

  lblSymbolGap := TLabel.Create( gpbArea ) ;
  lblSymbolGap.Parent := gpbArea ;
  lblSymbolGap.Position.Y := t ;
  PlaceControl( bd, nil, lblSymbolGap, LEFT_3COL_2, WIDTH_3COL ) ;
  lblSymbolGap.Text := _rsrcna( GIS_RS_LEGEND_PRM_SYMBOLGAP ) ;
  lblSymbolGap.FixSize ;

  lblSymbolRotate := TLabel.Create( gpbArea ) ;
  lblSymbolRotate.Parent := gpbArea ;
  lblSymbolRotate.Position.Y := t ;
  PlaceControl( bd, nil, lblSymbolRotate, LEFT_3COL_3, WIDTH_3COL ) ;
  lblSymbolRotate.Text := _rsrcna( GIS_RS_LEGEND_PRM_SYMBOLROTATE ) ;
  lblSymbolRotate.FixSize ;

  t := t + lblSymbolGap.Height ;

  cmbSymbolSize := TGIS_SizeComboBox.Create( gpbArea ) ;
  cmbSymbolSize.Parent := gpbArea ;
  cmbSymbolSize.Position.Y := t ;
  PlaceControl( bd, nil, cmbSymbolSize, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbSymbolSize.Fill( True, False, False, False ) ;
  cmbSymbolSize.OnChange := doControlChange ;
  cmbSymbolSize.CustomEvent := doCustomSize ;

  cmbSymbolGap := TGIS_SizeComboBox.Create( gpbArea ) ;
  cmbSymbolGap.Parent := gpbArea ;
  cmbSymbolGap.Position.Y := t ;
  PlaceControl( bd, nil, cmbSymbolGap, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbSymbolGap.Fill( False, False, False, False ) ;
  cmbSymbolGap.OnChange := doControlChange ;
  cmbSymbolGap.CustomEvent := doCustomSize ;

  cmbSymbolRotate := TGIS_RotationComboBox.Create( gpbArea ) ;
  cmbSymbolRotate.Parent := gpbArea ;
  cmbSymbolRotate.Position.Y := t ;
  PlaceControl( bd, nil, cmbSymbolRotate, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbSymbolRotate.Fill( True ) ;
  cmbSymbolRotate.OnChange := doControlChange ;
  cmbSymbolRotate.CustomEvent := doCustomRotation ;

  t := t + cmbSymbolRotate.Height + 16 ;

  lblSSSize := TLabel.Create( gpbArea ) ;
  lblSSSize.Parent := gpbArea ;
  lblSSSize.Position.Y := t ;
  PlaceControl( bd, nil, lblSSSize, LEFT_3COL_1, WIDTH_3COL ) ;
  lblSSSize.Text := _rsrcna( GIS_RS_LEGEND_PRM_SMART_SIZE ) ;
  lblSSSize.FixSize ;

  t := t + lblSymbolGap.Height ;

  cmbSSSize := TGIS_SizeComboBox.Create( gpbArea ) ;
  cmbSSSize.Parent := gpbArea ;
  cmbSSSize.Position.Y := t ;
  PlaceControl( bd, nil, cmbSSSize, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbSSSize.Fill( False, False, True, False ) ;
  cmbSSSize.OnChange := doControlChange ;
  cmbSSSize.CustomEvent := doCustomSize ;

  t := t + cmbSSSize.Height + GROUPBOX_BOTTOM_GAP ;

  gpbArea.Height := t ;
end ;

procedure T_panelArea.initOutline ;
var
  t : Single ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbOutline := TGroupBox.Create( Self.Panel ) ;
  gpbOutline.Parent := Self.Panel ;
  gpbOutline.Position.Y  := gpbArea.Position.Y + gpbArea.Height + GROUPBOX_GAP;
  gpbOutline.Height := 512 ;
  PlaceControl( bd, nil, gpbOutline, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbOutline.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbOutline.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbOutline.Text := _rsrcna( GIS_RS_LEGEND_TAB_OUTLINE ) ;

  t := GROUPBOX_TOP_GAP ;

  lblOStyle := TLabel.Create( gpbOutline ) ;
  lblOStyle.Parent := gpbOutline ;
  lblOStyle.Position.Y := t ;
  PlaceControl( bd, nil, lblOStyle, LEFT_3COL_1, WIDTH_3COL ) ;
  lblOStyle.Text := _rsrcna( GIS_RS_LEGEND_PRM_STYLE ) ;
  lblOStyle.FixSize ;

  lblOColor := TLabel.Create( gpbOutline ) ;
  lblOColor.Parent := gpbOutline ;
  lblOColor.Position.Y := t ;
  PlaceControl( bd, nil, lblOColor, LEFT_3COL_2, WIDTH_3COL ) ;
  lblOColor.Text := _rsrcna( GIS_RS_LEGEND_PRM_COLOR ) ;
  lblOColor.Parent := gpbOutline ;
  lblOColor.FixSize ;

  lblOPattern := TLabel.Create( gpbOutline ) ;
  lblOPattern.Parent := gpbOutline ;
  lblOPattern.Position.Y := t ;
  PlaceControl( bd, nil, lblOPattern, LEFT_3COL_3, WIDTH_3COL ) ;
  lblOPattern.Text := _rsrcna( GIS_RS_LEGEND_PRM_PATTERN ) ;
  lblOPattern.FixSize;

  t := t + lblOStyle.Height ;

  cmbOStyle := TGIS_PenStyleComboBox.Create( gpbOutline ) ;
  cmbOStyle.Parent := gpbOutline ;
  cmbOStyle.Position.Y := t ;
  PlaceControl( bd, nil, cmbOStyle, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbOStyle.Fill( True ) ;
  cmbOStyle.OnChange := doOPatternChange ;
  cmbOStyle.CustomEvent := doCustomPattern ;
  cmbOStyle.GetBitmapEvent := doGetBitmapAreaOutline ;

  cmbOColor := TGIS_ColorComboBox.Create( gpbOutline ) ;
  cmbOColor.Parent := gpbOutline ;
  cmbOColor.Position.Y    := t ;
  cmbOColor.Height := 28 ;
  PlaceControl( bd, nil, cmbOColor, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbOColor.Fill( True, True ) ;
  cmbOColor.OnChange    := doControlChange ;
  cmbOColor.CustomEvent := doCustomColor ;

  cmbOPattern := TGIS_PatternComboBox.Create( gpbOutline ) ;
  cmbOPattern.Parent := gpbOutline ;
  cmbOPattern.Position.Y := t ;
  PlaceControl( bd, nil, cmbOPattern, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbOPattern.Fill( False ) ;
  cmbOPattern.OnChange       := doOPatternChange ;
  cmbOPattern.CustomEvent    := doCustomPattern ;
  cmbOPattern.GetBitmapEvent := doGetBitmapAreaFill ;

  t := t + cmbOPattern.Height + 16 ;

  lblOWidth := TLabel.Create( gpbOutline ) ;
  lblOWidth.Parent := gpbOutline ;
  lblOWidth.Position.Y := t ;
  PlaceControl( bd, nil, lblOWidth, LEFT_3COL_1, WIDTH_3COL ) ;
  lblOWidth.Text := _rsrcna( GIS_RS_LEGEND_PRM_WIDTH ) ;
  lblOWidth.FixSize ;

  lblOSymbolGap := TLabel.Create( gpbOutline ) ;
  lblOSymbolGap.Parent := gpbOutline ;
  lblOSymbolGap.Position.Y := t ;
  PlaceControl( bd, nil, lblOSymbolGap, LEFT_3COL_2, WIDTH_3COL ) ;
  lblOSymbolGap.Text := _rsrcna( GIS_RS_LEGEND_PRM_SYMBOLGAP ) ;
  lblOSymbolGap.FixSize ;

  lblOSymbolRotate := TLabel.Create( gpbOutline ) ;
  lblOSymbolRotate.Parent := gpbOutline ;
  lblOSymbolRotate.Position.Y := t ;
  PlaceControl( bd, nil, lblOSymbolRotate, LEFT_3COL_3, WIDTH_3COL ) ;
  lblOSymbolRotate.Text := _rsrcna( GIS_RS_LEGEND_PRM_SYMBOLROTATE ) ;
  lblOSymbolRotate.FixSize ;

  t := t + lblOSymbolRotate.Height ;

  cmbOWidth := TGIS_SizeComboBox.Create( gpbOutline ) ;
  cmbOWidth.Parent := gpbOutline ;
  cmbOWidth.Position.Y := t ;
  PlaceControl( bd, nil, cmbOWidth, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbOWidth.Fill( False, True, True, True ) ;
  cmbOWidth.OnChange := doControlChange ;
  cmbOWidth.CustomEvent := doCustomSize ;

  cmbOSymbolGap := TGIS_SizeComboBox.Create( gpbOutline ) ;
  cmbOSymbolGap.Parent := gpbOutline ;
  cmbOSymbolGap.Position.Y := t ;
  PlaceControl( bd, nil, cmbOSymbolGap, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbOSymbolGap.Fill( False, False, False, False ) ;
  cmbOSymbolGap.OnChange := doControlChange ;
  cmbOSymbolGap.CustomEvent := doCustomSize ;

  cmbOSymbolRotate := TGIS_RotationComboBox.Create( gpbOutline ) ;
  cmbOSymbolRotate.Parent := gpbOutline ;
  cmbOSymbolRotate.Position.Y := t ;
  PlaceControl( bd, nil, cmbOSymbolRotate, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbOSymbolRotate.Fill( False ) ;
  cmbOSymbolRotate.OnChange := doControlChange ;
  cmbOSymbolRotate.CustomEvent := doCustomRotation ;

  t := t + cmbOSymbolRotate.Height + GROUPBOX_BOTTOM_GAP ;

  gpbOutline.Height := t ;
end ;

procedure T_panelArea.Read ;
var
  str : string ;
begin
  lockUpdates ;
  try
    ckbLegend.IsChecked       := mvc.ParamsArea.ShowLegend ;

    cmbPattern.Value        := mvc.ParamsArea.PatternAsText ;
    cmbColor.Value          := mvc.ParamsArea.ColorAsText ;
    cmbSymbolGap.Value      := mvc.ParamsArea.SymbolGapAsText ;
    cmbSymbolRotate.Value   := mvc.ParamsArea.SymbolRotateAsText ;
    cmbSymbolSize.Value     := mvc.ParamsArea.SymbolSizeAsText ;

    // Outline
    cmbOStyle.Value         := mvc.ParamsArea.OutlineStyleAsText ;
    cmbOWidth.Value         := mvc.ParamsArea.OutlineWidthAsText ;
    cmbOColor.Value         := mvc.ParamsArea.OutlineColorAsText ;
    cmbOPattern.Value       := mvc.ParamsArea.OutlinePatternAsText ;
    cmbOSymbolGap.Value     := mvc.ParamsArea.OutlineSymbolGapAsText ;
    cmbOSymbolRotate.Value  := mvc.ParamsArea.OutlineSymbolRotateAsText ;

    // Smart size
    cmbSSSize.Value         := mvc.ParamsArea.SmartSizeAsText ;

    mvc.DoPatternChange ;
    mvc.DoOPatternChange ;
    mvc.DoSmartSizeFieldChange ;

    str := Copy( cmbOStyle.Value, StringFirst, 4 ) ;
    if str = GIS_PARAMTXT_TYPE_CODE then
      lineSymbol :=
        Copy( cmbOStyle.Value, StringFirst+6, Length( cmbOStyle.Value ) ) ;
  finally
    unlockUpdates ;
  end ;
end ;

procedure T_panelArea.Write ;
begin
  mvc.ParamsArea.ShowLegend                 := ckbLegend.IsChecked ;

  mvc.ParamsArea.PatternAsText              := cmbPattern.Value ;
  mvc.ParamsArea.ColorAsText                := cmbColor.Value ;
  mvc.ParamsArea.SymbolGapAsText            := cmbSymbolGap.Value ;
  mvc.ParamsArea.SymbolRotateAsText         := cmbSymbolRotate.Value ;
  mvc.ParamsArea.SymbolSizeAsText           := cmbSymbolSize.Value ;

  // Outline
  mvc.ParamsArea.OutlineStyleAsText         := cmbOStyle.Value ;
  mvc.ParamsArea.OutlineWidthAsText         := cmbOWidth.Value ;
  mvc.ParamsArea.OutlineColorAsText         := cmbOColor.Value ;
  mvc.ParamsArea.OutlinePatternAsText       := cmbOPattern.Value ;
  mvc.ParamsArea.OutlineSymbolGapAsText     := cmbOSymbolGap.Value ;
  mvc.ParamsArea.OutlineSymbolRotateAsText  := cmbOSymbolRotate.Value ;

  // Smart size
  mvc.ParamsArea.SmartSizeAsText            := cmbSSSize.Value ;
end ;

procedure T_panelArea.PreparePreview(
  const _viewer : IGIS_Viewer
) ;
begin
  mvc.PreparePreview( _viewer ) ;
end ;

procedure T_panelArea.UpdatePreview ;
var
  ll        : TGIS_Layer ;
  paramsvec : TGIS_ParamsSectionVector ;
begin
  oParentWindow.tmrUpdate.Enabled := False ;
  if oParentWindow.gisPreview.IsEmpty then exit ;
  oParentWindow.tmrUpdate.Enabled := True  ;

  ll := TGIS_Layer( oParentWindow.gisPreview.Items[0] ) ;

  paramsvec := TGIS_ParamsSectionVector( ll.Params ) ;

  mvc.SectionWrite( paramsvec ) ;

  paramsvec.Labels.Value := '' ;
  paramsvec.Labels.Field := '' ;

  paramsvec.Render.Chart := '' ;
  paramsvec.Chart.Values := '' ;
  paramsvec.Query        := '' ;

  with TGIS_ParamsSection( ll.Params ) do begin
    MinZoom  := 0              ;
    MaxZoom  := GIS_MAX_DOUBLE ;
    MinScale := 0              ;
    MaxScale := GIS_MAX_DOUBLE ;
  end ;

  // avoid unreasonable preview
  paramsvec.Area.Color          := normalize_color(
                                     paramsvec.Area.Color,
                                     paramsvec.Area.ColorAsText
                                   ) ;
  paramsvec.Area.OutlineColor   := normalize_color(
                                     paramsvec.Area.OutlineColor,
                                     paramsvec.Area.OutlineColorAsText
                                   ) ;
  paramsvec.Area.SymbolSize     := normalize_size(
                                     oParentWindow.gisPreview,
                                     paramsvec.Area.SymbolSize,
                                     paramsvec.Area.SymbolSizeAsText,
                                     MAX_PREVIEW_SIZE_PATTERNSYMBOL
                                   ) ;
  paramsvec.Area.OutlineWidth   := normalize_size(
                                     oParentWindow.gisPreview,
                                     paramsvec.Area.OutlineWidth,
                                     paramsvec.Area.OutlineWidthAsText,
                                     MAX_PREVIEW_SIZE_OUTLINE
                                   ) ;
  paramsvec.Area.SymbolRotate   := normalize_angle(
                                     paramsvec.Area.SymbolRotate,
                                     paramsvec.Area.SymbolRotateAsText
                                   ) ;
end ;

procedure T_panelArea.doCallback(
  _sender : TObject ;
  _code   : Integer
) ;

  procedure do_ssfield_change ;
  begin
  end ;

  procedure do_update_symbol_bitmap ;
  var
    bsymfnt : Boolean     ;
    bsymcgm : Boolean     ;
    bsymsvg : Boolean     ;
    bsympic : Boolean     ;
    bsymlin : Boolean     ;
    sym     : TGIS_SymbolAbstract ;
    bbmp    : Boolean     ;
  begin
    sym := mvc.ParamsArea.Symbol ;

    bsymfnt := Assigned( sym ) and ( sym is TGIS_SymbolFont    ) ;
    bsymcgm := Assigned( sym ) and ( sym is TGIS_SymbolCGM     ) ;
    bsymsvg := Assigned( sym ) and ( sym is TGIS_SymbolSVG     ) ;
    bsympic := Assigned( sym ) and ( sym is TGIS_SymbolPicture ) ;
    bsymlin := Assigned( sym ) and ( ( sym is TGIS_SymbolLine   ) or
                                     ( sym is TGIS_SymbolLineEx )
                                    ) ;
    bbmp := not TGIS_Bitmap.IsNilOrEmpty( mvc.ParamsArea.Bitmap ) ;

    if bsymfnt or bsymcgm or bsymsvg or bsymlin then begin
      lblColor.Enabled        := True ;
      cmbColor.Enabled        := True ;
      lblSymbolGap.Enabled    := True ;
      cmbSymbolGap.Enabled    := True ;
      lblSymbolRotate.Enabled := True ;
      cmbSymbolRotate.Enabled := True ;
      lblSymbolSize.Enabled   := True ;
      cmbSymbolSize.Enabled   := True ;
    end
    else if bsympic then begin
      lblColor.Enabled        := False ;
      cmbColor.Enabled        := False ;
      lblSymbolGap.Enabled    := True ;
      cmbSymbolGap.Enabled    := True ;
      lblSymbolRotate.Enabled := True ;
      cmbSymbolRotate.Enabled := True ;
      lblSymbolSize.Enabled   := True ;
      cmbSymbolSize.Enabled   := True ;
    end
    else if bbmp then begin
      lblColor.Enabled        := False ;
      cmbColor.Enabled        := False ;
      lblSymbolGap.Enabled    := False ;
      cmbSymbolGap.Enabled    := False ;
      lblSymbolRotate.Enabled := False ;
      cmbSymbolRotate.Enabled := False ;
      lblSymbolSize.Enabled   := False ;
      cmbSymbolSize.Enabled   := False ;
    end
    else begin
      lblColor.Enabled        := True ;
      cmbColor.Enabled        := True ;
      lblSymbolGap.Enabled    := False ;
      cmbSymbolGap.Enabled    := False ;
      lblSymbolRotate.Enabled := False ;
      cmbSymbolRotate.Enabled := False ;
      lblSymbolSize.Enabled   := False ;
      cmbSymbolSize.Enabled   := False ;
    end ;
  end ;

  procedure do_update_osymbol_obitmap ;
  var
    bsymfnt : Boolean     ;
    bsymcgm : Boolean     ;
    bsymsvg : Boolean     ;
    bsympic : Boolean     ;
    bsymlin : Boolean     ;
    sym     : TGIS_SymbolAbstract ;
    bbmp    : Boolean     ;
  begin
    sym := mvc.ParamsArea.OutlineSymbol ;

    bsymfnt := Assigned( sym ) and ( sym is TGIS_SymbolFont    ) ;
    bsymcgm := Assigned( sym ) and ( sym is TGIS_SymbolCGM     ) ;
    bsymsvg := Assigned( sym ) and ( sym is TGIS_SymbolSVG     ) ;
    bsympic := Assigned( sym ) and ( sym is TGIS_SymbolPicture ) ;
    bsymlin := Assigned( sym ) and ( ( sym is TGIS_SymbolLine   ) or
                                     ( sym is TGIS_SymbolLineEx )
                                    ) ;
    bbmp := not TGIS_Bitmap.IsNilOrEmpty( mvc.ParamsArea.OutlineBitmap ) ;

    if bsymfnt or bsymcgm or bsymsvg or bsymlin then begin
      lblOColor.Enabled           := True ;
      cmbOColor.Enabled           := True ;
      lblOPattern.Enabled         := False ;
      cmbOPattern.Enabled         := False ;
      lblOSymbolGap.Enabled       := True ;
      cmbOSymbolGap.Enabled       := True ;
      lblOSymbolRotate.Enabled    := True ;
      cmbOSymbolRotate.Enabled    := True ;
    end
    else if bsympic then begin
      lblOColor.Enabled           := False ;
      cmbOColor.Enabled           := False ;
      lblOPattern.Enabled         := False ;
      cmbOPattern.Enabled         := False ;
      lblOSymbolGap.Enabled       := True ;
      cmbOSymbolGap.Enabled       := True ;
      lblOSymbolRotate.Enabled    := True ;
      cmbOSymbolRotate.Enabled    := True ;
    end
    else if bbmp then begin
      lblOColor.Enabled           := False ;
      cmbOColor.Enabled           := False ;
      lblOPattern.Enabled         := True ;
      cmbOPattern.Enabled         := True ;
      lblOSymbolGap.Enabled       := False ;
      cmbOSymbolGap.Enabled       := False ;
      lblOSymbolRotate.Enabled    := False ;
      cmbOSymbolRotate.Enabled    := False ;
    end
    else begin
      lblOColor.Enabled           := True ;
      cmbOColor.Enabled           := True ;
      lblOPattern.Enabled         := True  ;
      cmbOPattern.Enabled         := True  ;
      lblOSymbolGap.Enabled       := False ;
      cmbOSymbolGap.Enabled       := False ;
      lblOSymbolRotate.Enabled    := False ;
      cmbOSymbolRotate.Enabled    := False ;
    end ;
  end ;

begin
  if not blockedUpdates then begin
    Write ;
    UpdatePreview ;
  end ;

  case _code of
    60  : do_ssfield_change ;
    100 : do_update_symbol_bitmap ;
    130 : do_update_osymbol_obitmap
  end ;

end ;

procedure T_panelArea.doOPatternChange(
  _sender : TObject
) ;
begin
  mvc.DoOPatternChange ;
end;

procedure T_panelArea.doPatternChange(
  _sender : TObject
) ;
begin
  mvc.DoPatternChange ;
end;

procedure T_panelArea.doSmartSizeFieldChange(
  _sender : TObject
) ;
begin
  mvc.DoSmartSizeFieldChange ;
end ;

procedure T_panelArea.doControlChange(
  _sender: TObject
) ;
begin
  mvc.DoControlChange ;
end ;

{$ENDREGION}

{$REGION 'T_panelLabel'}
function T_panelLabel.fget_HasPreview : Boolean ;
begin
  Result := mvc.HasPreview ;
end ;

procedure T_panelLabel.init ;
var
  bd : TBiDiMode ;
begin
  inherited ;

  ItemText := _rsrcna( GIS_RS_LEGEND_PAG_LABEL ) ;

  mvc := oParentWindow.MVC.&Label ;
  mvc.Callback := doCallback ;

  initSelf ;
  initFont ;
  initLabel ;
  initOutline ;
  initPosition ;

  bd := oParentWindow.BiDiMode ;
  ckbLegend := TCheckBox.Create( Self.Panel ) ;
  ckbLegend.Parent := Self.Panel ;
  ckbLegend.Position.Y := gpbPosition.Position.Y + gpbPosition.Height + 8 ;
  PlaceControl( bd, nil, ckbLegend, LEFT_3COL_1, WIDTH_2COL ) ;
  if bd = bdRightToLeft then
    ckbLegend.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    ckbLegend.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  ckbLegend.Text := _rsrcna( GIS_RS_LEGEND_PRM_INCLUDEINLEGEND ) ;
end ;

procedure T_panelLabel.initSelf ;
var
  t : Single ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbField := TGroupBox.Create( Self.Panel ) ;
  gpbField.Parent := Self.Panel ;
  gpbField.Position.Y  := GROUPBOX_TOP ;
  gpbField.Height := 256 ;
  PlaceControl( bd, nil, gpbField, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbField.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbField.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbField.Text := _rsrcna( GIS_RS_LEGEND_PRM_VALUE ) ;

  t := GROUPBOX_TOP_GAP ;

  cmbValue := TGIS_FieldValueComboBox.Create( gpbField ) ;
  cmbValue.Parent := gpbField ;
  cmbValue.Position.Y := t ;
  PlaceControl( bd, nil, cmbValue, LEFT_3COL_1, -1 ) ;
  cmbValue.OnChange := doFieldChange ;
  cmbValue.Fill( oParentWindow.MVC.FieldNames ) ;
  cmbValue.ItemIndex := -1 ;

  t := t + cmbValue.Height + 8 ;

  ckbVisible := TCheckBox.Create( gpbField ) ;
  ckbVisible.Parent := gpbField ;
  ckbVisible.Position.Y := t ;
  PlaceControl( bd, nil, ckbVisible, LEFT_3COL_1, WIDTH_3COL ) ;
  ckbVisible.Text := _rsrcna( GIS_RS_LEGEND_PRM_VISIBLE ) ;
  ckbVisible.IsChecked := True ;

  ckbPAvoidOverlapping := TCheckBox.Create( gpbField ) ;
  ckbPAvoidOverlapping.Parent := gpbField ;
  ckbPAvoidOverlapping.Position.Y := t ;
  PlaceControl( bd, nil, ckbPAvoidOverlapping, LEFT_3COL_2, WIDTH_3COL ) ;
  ckbPAvoidOverlapping.Text := _rsrcna( GIS_RS_LEGEND_PRM_AVOIDOVERLAP ) ;
  ckbPAvoidOverlapping.IsChecked := True ;

  ckbPAvoidDuplicates := TCheckBox.Create( gpbField ) ;
  ckbPAvoidDuplicates.Parent := gpbField ;
  ckbPAvoidDuplicates.Position.Y := t ;
  PlaceControl( bd, nil, ckbPAvoidDuplicates, LEFT_3COL_3, WIDTH_3COL ) ;
  ckbPAvoidDuplicates.Text := _rsrcna( GIS_RS_LEGEND_PRM_AVOIDDUPLICATES ) ;
  ckbPAvoidDuplicates.IsChecked := True ;

  gpbField.Height := t + ckbVisible.Height + GROUPBOX_BOTTOM_GAP ;

end ;

procedure T_panelLabel.initFont ;
var
  t   : Single ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbFont := TGroupBox.Create( Self.Panel ) ;
  gpbFont.Parent := Self.Panel ;
  gpbFont.Position.Y := gpbField.Position.Y + gpbField.Height + GROUPBOX_GAP ;
  gpbFont.Height := 512 ;
  PlaceControl( bd, nil, gpbFont, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbFont.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbFont.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbFont.Text := _rsrcna( GIS_RS_LEGEND_PRM_FONT ) ;

  t := GROUPBOX_TOP_GAP ;

  lblFontName := TLabel.Create( gpbFont )  ;
  lblFontName.Parent := gpbFont ;
  lblFontName.Position.Y := t ;
  PlaceControl( bd, nil, lblFontName, LEFT_3COL_1, WIDTH_3COL ) ;
  lblFontName.Text := _rsrcna( GIS_RS_LEGEND_PRM_NAME ) ;
  lblFontName.FixSize ;

  lblFontSize := TLabel.Create( gpbFont )  ;
  lblFontSize.Parent := gpbFont ;
  lblFontSize.Position.Y := t ;
  PlaceControl( bd, nil, lblFontSize, LEFT_3COL_2, WIDTH_3COL ) ;
  lblFontSize.Text := _rsrcna( GIS_RS_LEGEND_PRM_SIZE ) ;
  lblFontSize.FixSize ;

  lblFontColor := TLabel.Create( gpbFont ) ;
  lblFontColor.Parent := gpbFont ;
  lblFontColor.Position.Y := t ;
  PlaceControl( bd, nil, lblFontColor, LEFT_3COL_3, WIDTH_3COL ) ;
  lblFontColor.Text := _rsrcna( GIS_RS_LEGEND_PRM_COLOR ) ;
  lblFontColor.FixSize ;

  t := t + lblFontColor.Height ;

  cmbFontName := TGIS_FontNameComboBox.Create( gpbFont ) ;
  cmbFontName.Parent := gpbFont ;
  cmbFontName.Position.Y := t ;
  PlaceControl( bd, nil, cmbFontName, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbFontName.Fill ;
  cmbFontName.OnChange := doControlChange ;

  cmbFontSize := TGIS_SizeComboBox.Create( gpbFont ) ;
  cmbFontSize.Parent := gpbFont ;
  cmbFontSize.Position.Y := t ;
  PlaceControl( bd, nil, cmbFontSize, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbFontSize.Fill( True, False, True, True ) ;
  cmbFontSize.OnChange    := doControlChange ;
  cmbFontSize.CustomEvent := doCustomSize ;

  cmbFontColor := TGIS_ColorComboBox.Create( gpbFont ) ;
  cmbFontColor.Parent := gpbFont ;
  cmbFontColor.Position.Y    := t ;
  cmbFontColor.Height := 28 ;
  PlaceControl( bd, nil, cmbFontColor, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbFontColor.Fill( True, True ) ;
  cmbFontColor.OnChange    := doControlChange ;
  cmbFontColor.CustomEvent := doCustomColor ;

  t := t + cmbFontColor.Height + 8 ;

  ckbFontStyleBold := TCheckBox.Create( gpbFont ) ;
  ckbFontStyleBold.Parent := gpbFont ;
  ckbFontStyleBold.Position.Y := t ;
  PlaceControl( bd, nil, ckbFontStyleBold, LEFT_3COL_1, WIDTH_3COL ) ;
  ckbFontStyleBold.Text:= _rsrcna( GIS_RS_LEGEND_DLGFONT_CHKBOLD ) ;
  ckbFontStyleBold.OnChange := doControlChange ;

  ckbFontStyleUnderline := TCheckBox.Create( gpbFont ) ;
  ckbFontStyleUnderline.Parent := gpbFont ;
  ckbFontStyleUnderline.Position.Y    := t ;
  PlaceControl( bd, nil, ckbFontStyleUnderline, LEFT_3COL_2, WIDTH_3COL ) ;
  ckbFontStyleUnderline.Text:= _rsrcna( GIS_RS_LEGEND_DLGFONT_CHKUNDERLINE ) ;
  ckbFontStyleUnderline.OnChange := doControlChange ;

  ckbFontStyleShadow := TCheckBox.Create( gpbFont ) ;
  ckbFontStyleShadow.Parent := gpbFont ;
  ckbFontStyleShadow.Position.Y    := t ;
  PlaceControl( bd, nil, ckbFontStyleShadow, LEFT_3COL_3, WIDTH_3COL ) ;
  ckbFontStyleShadow.Text:= _rsrcna( GIS_RS_LEGEND_PRM_SHADOW ) ;
  ckbFontStyleShadow.OnChange := doShadowChange ;

  t := t + ckbFontStyleShadow.Height ;

  ckbFontStyleItalic := TCheckBox.Create( gpbFont ) ;
  ckbFontStyleItalic.Parent := gpbFont ;
  ckbFontStyleItalic.Position.Y    := t ;
  PlaceControl( bd, nil, ckbFontStyleItalic, LEFT_3COL_1, WIDTH_3COL ) ;
  ckbFontStyleItalic.Text:= _rsrcna( GIS_RS_LEGEND_DLGFONT_CHKITALIC ) ;
  ckbFontStyleItalic.OnChange := doControlChange ;

  ckbFontStyleStrikeout := TCheckBox.Create( gpbFont ) ;
  ckbFontStyleStrikeout.Parent := gpbFont ;
  ckbFontStyleStrikeout.Position.Y    := t ;
  PlaceControl( bd, nil, ckbFontStyleStrikeout, LEFT_3COL_2, WIDTH_3COL ) ;
  ckbFontStyleStrikeout.Text := _rsrcna( GIS_RS_LEGEND_DLGFONT_STRIKEOUT ) ;
  ckbFontStyleStrikeout.OnChange := doControlChange ;

  gpbFont.Height := t + ckbFontStyleStrikeout.Height + GROUPBOX_BOTTOM_GAP ;
end ;

procedure T_panelLabel.initLabel ;
var
  t : Single ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbLabel := TGroupBox.Create( Self.Panel ) ;
  gpbLabel.Parent := Self.Panel ;
  gpbLabel.Position.Y := gpbFont.Position.Y + gpbFont.Height + GROUPBOX_GAP ;
  gpbLabel.Height := 512 ;
  PlaceControl( bd, nil, gpbLabel, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbLabel.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbLabel.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbLabel.Text := _rsrcna( GIS_RS_LEGEND_TAB_LABEL ) ;

  t := GROUPBOX_TOP_GAP ;

  lblColor := TLabel.Create( gpbLabel ) ;
  lblColor.Parent := gpbLabel ;
  lblColor.Position.Y := t ;
  PlaceControl( bd, nil, lblColor, LEFT_3COL_1, WIDTH_3COL ) ;
  lblColor.Text := _rsrcna( GIS_RS_LEGEND_PRM_COLOR ) ;
  lblColor.FixSize;

  lblPattern := TLabel.Create( gpbLabel ) ;
  lblPattern.Parent := gpbLabel ;
  lblPattern.Position.Y := t ;
  PlaceControl( bd, nil, lblPattern, LEFT_3COL_2, WIDTH_3COL ) ;
  lblPattern.Text := _rsrcna( GIS_RS_LEGEND_PRM_PATTERN ) ;
  lblPattern.FixSize ;

  lblShield := TLabel.Create( gpbLabel ) ;
  lblShield.Parent := gpbLabel ;
  lblShield.Position.Y := t ;
  PlaceControl( bd, nil, lblShield, LEFT_3COL_3, WIDTH_3COL ) ;
  lblShield.Text := _rsrcna( GIS_RS_LEGEND_PRM_SHIELD ) ;
  lblColor.FixSize;

  t := t + lblShield.Height ;

  cmbColor := TGIS_ColorComboBox.Create( gpbLabel ) ;
  cmbColor.Parent := gpbLabel ;
  cmbColor.Position.Y    := t ;
  cmbColor.Height := 28 ;
  PlaceControl( bd, nil, cmbColor, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbColor.Fill( True, True ) ;
  cmbColor.OnChange    := doControlChange ;
  cmbColor.CustomEvent := doCustomColor ;

  cmbPattern := TGIS_PatternComboBox.Create( gpbLabel ) ;
  cmbPattern.Parent := gpbLabel ;
  cmbPattern.Position.Y := t ;
  PlaceControl( bd, nil, cmbPattern, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbPattern.Fill( True ) ;
  cmbPattern.OnChange       := doPatternChange ;
  cmbPattern.CustomEvent    := doCustomPattern ;
  cmbPattern.GetBitmapEvent := doGetBitmapAreaFill ;

  cmbShield := TGIS_ShieldComboBox.Create( gpbLabel ) ;
  cmbShield.Parent := gpbLabel ;
  cmbShield.Position.Y := t ;
  PlaceControl( bd, nil, cmbShield, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbShield.Fill ;
  cmbShield.OnChange       := doShieldChange ;
  cmbShield.CustomEvent    := doCustomShield ;
  cmbShield.GetBitmapEvent := doGetBitmapShield ;
//  lblShield.FocusControl   := cmbShield ;

  t := t + cmbShield.Height + 8 ;

  lblWidth := TLabel.Create( gpbLabel ) ;
  lblWidth.Parent := gpbLabel ;
  lblWidth.Position.Y := t ;
  PlaceControl( bd, nil, lblWidth, LEFT_3COL_1, WIDTH_3COL ) ;
  lblWidth.Text := _rsrcna( GIS_RS_LEGEND_PRM_WIDTH ) ;
  lblWidth.FixSize ;

  lblHeight := TLabel.Create( gpbLabel ) ;
  lblHeight.Parent := gpbLabel ;
  lblHeight.Position.Y := t ;
  PlaceControl( bd, nil, lblHeight, LEFT_3COL_2, WIDTH_3COL ) ;
  lblHeight.Text := _rsrcna( GIS_RS_LEGEND_PRM_HEIGHT ) ;
  lblHeight.FixSize ;

  lblSSSize := TLabel.Create( gpbLabel ) ;
  lblSSSize.Parent := gpbLabel ;
  lblSSSize.Position.Y := t ;
  PlaceControl( bd, nil, lblSSSize, LEFT_3COL_3, WIDTH_3COL ) ;
  lblSSSize.Text := _rsrcna( GIS_RS_LEGEND_PRM_SMART_SIZE ) ;
  lblSSSize.FixSize ;

  t := t + lblWidth.Height ;

  cmbWidth := TGIS_SizeComboBox.Create( gpbLabel ) ;
  cmbWidth.Parent := gpbLabel ;
  cmbWidth.Position.Y := t ;
  PlaceControl( bd, nil, cmbWidth, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbWidth.Fill( True, False, False, False ) ;

  cmbWidth.OnChange := doControlChange ;
  cmbWidth.CustomEvent := doCustomSize ;

  cmbHeight := TGIS_SizeComboBox.Create( gpbLabel ) ;
  cmbHeight.Parent := gpbLabel ;
  cmbHeight.Position.Y := t ;
  PlaceControl( bd, nil, cmbHeight, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbHeight.Fill( True, False, False, False ) ;
  cmbHeight.OnChange := doControlChange ;
  cmbHeight.CustomEvent := doCustomSize ;

  cmbSSSize := TGIS_SizeComboBox.Create( gpbLabel ) ;
  cmbSSSize.Parent := gpbLabel ;
  cmbSSSize.Position.Y := t ;
  PlaceControl( bd, nil, cmbSSSize, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbSSSize.Fill( False, False, True, False ) ;
  cmbSSSize.OnChange := doControlChange ;
  cmbSSSize.CustomEvent := doCustomSize ;

  t := t + cmbHeight.Height + GROUPBOX_BOTTOM_GAP ;

  gpbLabel.Height := t ;
end ;

procedure T_panelLabel.initOutline ;
var
  t : Single ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbOutline := TGroupBox.Create( Self.Panel ) ;
  gpbOutline.Parent := Self.Panel ;
  gpbOutline.Position.Y := gpbLabel.Position.Y + gpbLabel.Height + GROUPBOX_GAP ;
  gpbOutline.Height := 512 ;
  PlaceControl( bd, nil, gpbOutline, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbOutline.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbOutline.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbOutline.Text := _rsrcna( GIS_RS_LEGEND_TAB_OUTLINE ) ;

  t := GROUPBOX_TOP_GAP ;

  lblOStyle := TLabel.Create( gpbOutline ) ;
  lblOStyle.Parent := gpbOutline ;
  lblOStyle.Position.Y := t ;
  PlaceControl( bd, nil, lblOStyle, LEFT_3COL_1, WIDTH_3COL ) ;
  lblOStyle.Text := _rsrcna( GIS_RS_LEGEND_PRM_STYLE ) ;
  lblOStyle.FixSize ;

  lblOColor := TLabel.Create( gpbOutline ) ;
  lblOColor.Parent := gpbOutline ;
  lblOColor.Position.Y := t ;
  PlaceControl( bd, nil, lblOColor, LEFT_3COL_2, WIDTH_3COL ) ;
  lblOColor.Text := _rsrcna( GIS_RS_LEGEND_PRM_COLOR ) ;
  lblOColor.FixSize ;

  lblOPattern := TLabel.Create( gpbOutline ) ;
  lblOPattern.Parent := gpbOutline ;
  lblOPattern.Position.Y := t ;
  PlaceControl( bd, nil, lblOPattern, LEFT_3COL_3, WIDTH_3COL ) ;
  lblOPattern.Text := _rsrcna( GIS_RS_LEGEND_PRM_PATTERN ) ;
  lblOPattern.FixSize ;

  t := t + lblOStyle.Height ;

  cmbOStyle := TGIS_PenStyleComboBox.Create( gpbOutline ) ;
  cmbOStyle.Parent := gpbOutline ;
  cmbOStyle.Position.Y := t ;
  PlaceControl( bd, nil, cmbOStyle, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbOStyle.Fill( False ) ;
  cmbOStyle.OnChange := doOPatternChange ;
  cmbOStyle.CustomEvent := doCustomPattern ;
  cmbOStyle.GetBitmapEvent := doGetBitmapAreaOutline ;

  cmbOColor := TGIS_ColorComboBox.Create( gpbOutline ) ;
  cmbOColor.Parent := gpbOutline ;
  cmbOColor.Position.Y := t ;
  cmbOColor.Height := 28 ;
  PlaceControl( bd, nil, cmbOColor, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbOColor.Fill( True, True ) ;
  cmbOColor.OnChange    := doControlChange ;
  cmbOColor.CustomEvent := doCustomColor ;

  cmbOPattern := TGIS_PatternComboBox.Create( gpbOutline ) ;
  cmbOPattern.Parent := gpbOutline ;
  cmbOPattern.Position.Y := t ;
  PlaceControl( bd, nil, cmbOPattern, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbOPattern.Fill( False ) ;
  cmbOPattern.OnChange       := doOPatternChange ;
  cmbOPattern.CustomEvent    := doCustomPattern ;
  cmbOPattern.GetBitmapEvent := doGetBitmapAreaFill ;

  t := t + cmbOPattern.Height + 8 ;

  lblOWidth := TLabel.Create( gpbOutline ) ;
  lblOWidth.Parent := gpbOutline ;
  lblOWidth.Position.Y := t ;
  PlaceControl( bd, nil, lblOWidth, LEFT_3COL_1, WIDTH_3COL ) ;
  lblOWidth.Text := _rsrcna( GIS_RS_LEGEND_PRM_WIDTH ) ;
  lblOWidth.FixSize ;

  t := t + lblOWidth.Height ;

  cmbOWidth := TGIS_SizeComboBox.Create( gpbOutline ) ;
  cmbOWidth.Parent := gpbOutline ;
  cmbOWidth.Position.Y := t ;
  PlaceControl( bd, nil, cmbOWidth, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbOWidth.Fill( False, True, True, True ) ;
  cmbOWidth.OnChange := doControlChange ;
  cmbOWidth.CustomEvent := doCustomSize ;

  t := t + cmbOWidth.Height + 16 ;

  gpbOutline.Height := t ;
end ;


procedure T_panelLabel.initPosition ;
var
  t : Single ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbPosition := TGroupBox.Create( Self.Panel ) ;
  gpbPosition.Parent := Self.Panel ;
  gpbPosition.Position.Y := gpbOutline.Position.Y + gpbOutline.Height + GROUPBOX_GAP ;
  gpbPosition.Height := 512 ;
  PlaceControl( bd, nil, gpbPosition, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbPosition.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbPosition.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbPosition.Text := _rsrcna( GIS_RS_LEGEND_TAB_POSITION ) ;

  t := GROUPBOX_TOP_GAP ;

  lblPAlignment := TLabel.Create( gpbPosition ) ;
  lblPAlignment.Parent := gpbPosition ;
  lblPAlignment.Position.Y := t ;
  PlaceControl( bd, nil, lblPAlignment, LEFT_3COL_1, WIDTH_3COL ) ;
  lblPAlignment.Text := _rsrcna( GIS_RS_LEGEND_PRM_ALIGNMENT ) ;
  lblPAlignment.FixSize ;

  lblPRotation := TLabel.Create( gpbPosition ) ;
  lblPRotation.Parent := gpbPosition ;
  lblPRotation.Position.Y := t ;
  PlaceControl( bd, nil, lblPRotation, LEFT_3COL_2, WIDTH_3COL ) ;
  lblPRotation.Text := _rsrcna( GIS_RS_LEGEND_PRM_LABELROTATE ) ;
  lblPRotation.FixSize ;

  lblPPosition := TLabel.Create( gpbPosition ) ;
  lblPPosition.Parent := gpbPosition ;
  lblPPosition.Position.Y := t ;
  PlaceControl( bd, nil, lblPPosition, LEFT_3COL_3, WIDTH_3COL ) ;
  lblPPosition.Text := _rsrcna( GIS_RS_LEGEND_PRM_POSITION ) ;
  lblPPosition.FixSize ;

  t := t + lblPRotation.Height ;

  cmbPAlignment := TComboBox.Create( gpbPosition ) ;
  cmbPAlignment.Parent := gpbPosition ;
  cmbPAlignment.Position.Y := t ;
  PlaceControl( bd, nil, cmbPAlignment, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbPAlignment.Items.BeginUpdate ;
  cmbPAlignment.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_SINGLELINE ) ) ;
  cmbPAlignment.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_LEFTJUSTIFY ) ) ;
  cmbPAlignment.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_CENTER ) ) ;
  cmbPAlignment.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_RIGHTJUSTIFY ) ) ;
  cmbPAlignment.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_FOLLOW ) ) ;
  cmbPAlignment.ItemIndex := 1 ;
  cmbPAlignment.Items.EndUpdate ;
  cmbPAlignment.OnChange := doAlignmentChange ;

  cmbPRotation := TGIS_RotationComboBox.Create( gpbPosition ) ;
  cmbPRotation.Parent := gpbPosition ;
  cmbPRotation.Position.Y := t ;
  PlaceControl( bd, nil, cmbPRotation, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbPRotation.Fill( True ) ;
  cmbPRotation.OnChange := doControlChange ;
  cmbPRotation.CustomEvent := doCustomRotation ;

  btnPTopLeft := TButton.Create( gpbPosition ) ;
  btnPTopLeft.Parent := gpbPosition ;
  if bd = bdRightToLeft then
    btnPTopLeft.Position.X := LEFT_3COL_1 + WIDTH_3COL - 3 * 23
  else
    btnPTopLeft.Position.X := LEFT_3COL_3 ;
  btnPTopLeft.Position.Y := t ;
  btnPTopLeft.Width := 23 ;
  btnPTopLeft.Height := 23 ;
  btnPTopLeft.OnClick := actBtnPositionClick ;

  btnPTopCenter := TButton.Create( gpbPosition ) ;
  btnPTopCenter.Parent := gpbPosition ;
  btnPTopCenter.Position.X := btnPTopLeft.Position.X + btnPTopLeft.Width ;
  btnPTopCenter.Position.Y := t ;
  btnPTopCenter.Width := 23 ;
  btnPTopCenter.Height := 23 ;
  btnPTopCenter.OnClick := actBtnPositionClick ;

  btnPTopRight := TButton.Create( gpbPosition ) ;
  btnPTopRight.Parent := gpbPosition ;
  btnPTopRight.Position.X := btnPTopCenter.Position.X + btnPTopCenter.Width ;
  btnPTopRight.Position.Y := t ;
  btnPTopRight.Width := 23 ;
  btnPTopRight.Height := 23 ;
  btnPTopRight.OnClick := actBtnPositionClick ;

  fbnPPosition := TGIS_FieldButton.Create( gpbPosition ) ;
  fbnPPosition.Parent := gpbPosition ;
  if bd = bdRightToLeft then
    fbnPPosition.Position.X := btnPTopLeft.Position.X - fbnPPosition.Width
  else
    fbnPPosition.Position.X := btnPTopRight.Position.X + btnPTopRight.Width ;
  fbnPPosition.Position.Y := t ;
  fbnPPosition.FieldsList.Assign( oParentWindow.MVC.FieldNamesEx ) ;
  fbnPPosition.FieldsList.Insert( 0, '' ) ;
  fbnPPosition.OnCommitClick := doPositionExNotify ;

  t := t + btnPTopLeft.Height ;

  ckbPFlow := TCheckBox.Create( gpbPosition ) ;
  ckbPFlow.Parent := gpbPosition ;
  ckbPFlow.Position.Y := t + 8 ;
  PlaceControl( bd, nil, ckbPFlow, LEFT_3COL_1, WIDTH_3COL ) ;
  ckbPFlow.Text := _rsrcna( GIS_RS_LEGEND_PRM_FLOW ) ;

  btnPCenterLeft := TButton.Create( gpbPosition ) ;
  btnPCenterLeft.Parent := gpbPosition ;
  btnPCenterLeft.Position.X := btnPTopLeft.Position.X ;
  btnPCenterLeft.Position.Y := t ;
  btnPCenterLeft.Width := 23 ;
  btnPCenterLeft.Height := 23 ;
  btnPCenterLeft.OnClick := actBtnPositionClick ;

  btnPCenterCenter := TButton.Create( gpbPosition ) ;
  btnPCenterCenter.Parent := gpbPosition ;
  btnPCenterCenter.Position.X := btnPCenterLeft.Position.X + btnPCenterLeft.Width ;
  btnPCenterCenter.Position.Y := t ;
  btnPCenterCenter.Width := 23 ;
  btnPCenterCenter.Height := 23 ;
  btnPCenterCenter.OnClick := actBtnPositionClick ;

  btnPCenterRight := TButton.Create( gpbPosition ) ;
  btnPCenterRight.Parent := gpbPosition ;
  btnPCenterRight.Position.X := btnPCenterCenter.Position.X + btnPCenterCenter.Width ;
  btnPCenterRight.Position.Y := t ;
  btnPCenterRight.Width := 23 ;
  btnPCenterRight.Height := 23 ;
  btnPCenterRight.OnClick := actBtnPositionClick ;

  t := t + btnPCenterLeft.Height ;

  btnPBottomLeft := TButton.Create( gpbPosition ) ;
  btnPBottomLeft.Parent := gpbPosition ;
  btnPBottomLeft.Position.X := btnPTopLeft.Position.X ;
  btnPBottomLeft.Position.Y := t ;
  btnPBottomLeft.Width := 23 ;
  btnPBottomLeft.Height := 23 ;
  btnPBottomLeft.OnClick := actBtnPositionClick ;

  btnPBottomCenter := TButton.Create( gpbPosition ) ;
  btnPBottomCenter.Parent := gpbPosition ;
  btnPBottomCenter.Position.X := btnPBottomLeft.Position.X + btnPBottomLeft.Width ;
  btnPBottomCenter.Position.Y := t ;
  btnPBottomCenter.Width := 23 ;
  btnPBottomCenter.Height := 23 ;
  btnPBottomCenter.OnClick := actBtnPositionClick ;

  btnPBottomRight := TButton.Create( gpbPosition ) ;
  btnPBottomRight.Parent := gpbPosition ;
  btnPBottomRight.Position.X := btnPBottomCenter.Position.X + btnPBottomCenter.Width ;
  btnPBottomRight.Position.Y := t ;
  btnPBottomRight.Width := 23 ;
  btnPBottomRight.Height := 23 ;
  btnPBottomRight.OnClick := actBtnPositionClick ;

  cmbPAlignment.TabOrder := 0 ;
  cmbPRotation.TabOrder := 1 ;
  btnPTopLeft.TabOrder := 2 ;
  btnPTopCenter.TabOrder := 3 ;
  btnPTopRight.TabOrder := 4 ;
  btnPCenterLeft.TabOrder := 5 ;
  btnPCenterCenter.TabOrder := 6 ;
  btnPCenterRight.TabOrder := 7 ;
  btnPBottomLeft.TabOrder := 8 ;
  btnPBottomCenter.TabOrder := 9 ;
  btnPBottomRight.TabOrder := 10 ;
  fbnPPosition.TabOrder := 11 ;
  ckbPFlow.TabOrder := 12 ;

  fldPPosition := T_fieldPanel.Create( gpbPosition ) ;
  fldPPosition.Parent := gpbPosition ;
  fldPPosition.Position.X := btnPTopLeft.Position.X ;
  fldPPosition.Position.Y := btnPTopLeft.Position.Y ;
  fldPPosition.Width := btnPBottomRight.Position.X + btnPBottomRight.Width -
                        btnPTopLeft.Position.X ;
  fldPPosition.Height := btnPBottomRight.Position.Y + btnPBottomRight.Height -
                         btnPTopLeft.Position.Y ;
  fldPPosition.Visible := False ;

  gpbPosition.Height := t + btnPBottomRight.Height + GROUPBOX_BOTTOM_GAP ;

end ;

procedure T_panelLabel.Read ;
var
  s1,s2,s3 : String ;
begin
  lockUpdates ;
  try
    cmbValue.Value := mvc.ParamsLabel.Value ;  // use Value
    ckbVisible.IsChecked := mvc.ParamsLabel.Visible ;
    ckbLegend.IsChecked := mvc.ParamsLabel.ShowLegend ;
    ckbPAvoidOverlapping.IsChecked := mvc.ParamsLabel.Allocator ;
    ckbPAvoidDuplicates.IsChecked  := not mvc.ParamsLabel.Duplicates ;

    // Font
    cmbFontName.Value   := mvc.ParamsLabel.FontName ;
    cmbFontSize.Value   := mvc.ParamsLabel.FontSizeAsText ;
    cmbFontColor.Value  := mvc.ParamsLabel.FontColorAsText ;

    ckbFontStyleBold.IsChecked      := TGIS_FontStyle.Bold in mvc.ParamsLabel.FontStyle ;
    ckbFontStyleItalic.IsChecked    := TGIS_FontStyle.Italic in mvc.ParamsLabel.FontStyle ;
    ckbFontStyleUnderline.IsChecked := TGIS_FontStyle.Underline in mvc.ParamsLabel.FontStyle ;
    ckbFontStyleStrikeout.IsChecked := TGIS_FontStyle.StrikeOut in mvc.ParamsLabel.FontStyle ;

    ckbFontStyleShadow.IsChecked    := mvc.ParamsLabel.Pattern = TGIS_BrushStyle.Clear ;

    // Label
    cmbColor.Value                := mvc.ParamsLabel.ColorAsText ;
    cmbPattern.Value              := mvc.ParamsLabel.PatternAsText ;
    cmbShield.Value               := mvc.ParamsLabel.ShieldAsText ;

    cmbWidth.Value                := mvc.ParamsLabel.WidthAsText ;
    cmbHeight.Value               := mvc.ParamsLabel.HeightAsText ;

    // Outline
    cmbOStyle.Value         := mvc.ParamsLabel.OutlineStyleAsText ;
    cmbOWidth.Value         := mvc.ParamsLabel.OutlineWidthAsText ;
    cmbOColor.Value         := mvc.ParamsLabel.OutlineColorAsText ;
    cmbOPattern.Value       := mvc.ParamsLabel.OutlinePatternAsText ;

    // Smart size
    cmbSSSize.Value := mvc.ParamsLabel.SmartSizeAsText ;

    // Position
    cmbPAlignment.Items.BeginUpdate ;
    case mvc.ParamsLabel.Alignment of
      TGIS_LabelAlignment.Single       : cmbPAlignment.ItemIndex := 0 ;
      TGIS_LabelAlignment.LeftJustify  : cmbPAlignment.ItemIndex := 1 ;
      TGIS_LabelAlignment.Center       : cmbPAlignment.ItemIndex := 2 ;
      TGIS_LabelAlignment.RightJustify : cmbPAlignment.ItemIndex := 3 ;
      TGIS_LabelAlignment.Follow       : cmbPAlignment.ItemIndex := 4 ;
    end ;
    cmbPAlignment.Items.EndUpdate ;

    cmbPRotation.Value := mvc.ParamsLabel.RotateAsText ;
    ckbPFlow.IsChecked := TGIS_LabelPosition.Flow in mvc.ParamsLabel.Position ;

    if TGIS_LabelPosition.UpLeft in mvc.ParamsLabel.Position then
      btnPTopLeft.Text      := 'X' ;
    if TGIS_LabelPosition.UpCenter in mvc.ParamsLabel.Position then
      btnPTopCenter.Text    := 'X' ;
    if TGIS_LabelPosition.UpRight in mvc.ParamsLabel.Position then
      btnPTopRight.Text     := 'X' ;
    if TGIS_LabelPosition.MiddleLeft in mvc.ParamsLabel.Position then
      btnPCenterLeft.Text   := 'X' ;
    if TGIS_LabelPosition.MiddleCenter in mvc.ParamsLabel.Position then
      btnPCenterCenter.Text := 'X' ;
    if TGIS_LabelPosition.MiddleRight in mvc.ParamsLabel.Position then
      btnPCenterRight.Text  := 'X' ;
    if TGIS_LabelPosition.DownLeft in mvc.ParamsLabel.Position then
      btnPBottomLeft.Text   := 'X' ;
    if TGIS_LabelPosition.DownCenter in mvc.ParamsLabel.Position then
      btnPBottomCenter.Text := 'X' ;
    if TGIS_LabelPosition.DownRight in mvc.ParamsLabel.Position then
      btnPBottomRight.Text  := 'X' ;

    SplitParamAsText( mvc.ParamsLabel.PositionAsText, s1, s2, s3 ) ;

    if s1 = GIS_PARAMTXT_TYPE_FIELD then begin
      fldPPosition.Field := s2 ;
      fbnPPosition.Field := s2 ;
    end ;

    mvc.DoUpdateBitmap ;
    mvc.DoUpdateOBitmap ;
    mvc.DoFieldChange ;
  finally
    unlockUpdates ;
  end ;

end ;

procedure T_panelLabel.Write ;
var
  lps : TGIS_LabelPositions ;
  fs  : TGIS_FontStyles ;
begin
  mvc.ParamsLabel.Value      := cmbValue.Value ;
  mvc.ParamsLabel.Visible    := ckbVisible.IsChecked ;
  mvc.ParamsLabel.ShowLegend := ckbLegend.IsChecked ;
  mvc.ParamsLabel.Allocator  := ckbPAvoidOverlapping.IsChecked ;
  mvc.ParamsLabel.Duplicates := not ckbPAvoidDuplicates.IsChecked ;

  mvc.ParamsLabel.FontColorAsText := cmbFontColor.Value ;
  mvc.ParamsLabel.FontName        := cmbFontName.Value ;
  mvc.ParamsLabel.FontSizeAsText  := cmbFontSize.Value ;

  fs := [] ;
  if ckbFontStyleBold.IsChecked then
    fs := fs + [TGIS_FontStyle.Bold] ;
  if ckbFontStyleItalic.IsChecked then
    fs := fs + [TGIS_FontStyle.Italic] ;
  if ckbFontStyleUnderline.IsChecked then
    fs := fs + [TGIS_FontStyle.Underline] ;
  if ckbFontStyleStrikeout.IsChecked then
    fs := fs + [TGIS_FontStyle.StrikeOut] ;

  mvc.ParamsLabel.FontStyle := fs ;

  // Label
  mvc.ParamsLabel.ColorAsText      := cmbColor.Value ;
  mvc.ParamsLabel.PatternAsText    := cmbPattern.Value ;
  mvc.ParamsLabel.ShieldAsText     := cmbShield.Value ;
  mvc.ParamsLabel.WidthAsText      := cmbWidth.Value  ;
  mvc.ParamsLabel.HeightAsText     := cmbHeight.Value  ;

  // Outline
  mvc.ParamsLabel.OutlineStyleAsText         := cmbOStyle.Value ;
  mvc.ParamsLabel.OutlineWidthAsText         := cmbOWidth.Value ;
  mvc.ParamsLabel.OutlineColorAsText         := cmbOColor.Value ;
  mvc.ParamsLabel.OutlinePatternAsText       := cmbOPattern.Value ;

  // Smart size
  mvc.ParamsLabel.SmartSizeAsText := cmbSSSize.Value  ;

  // Position
  lps := [] ;
  if btnPTopLeft.Text      = 'X' then
    lps := lps + [TGIS_LabelPosition.UpLeft] ;
  if btnPTopCenter.Text    = 'X' then
    lps := lps + [TGIS_LabelPosition.UpCenter] ;
  if btnPTopRight.Text     = 'X' then
    lps := lps + [TGIS_LabelPosition.UpRight] ;
  if btnPCenterLeft.Text   = 'X' then
    lps := lps + [TGIS_LabelPosition.MiddleLeft] ;
  if btnPCenterCenter.Text = 'X' then
    lps := lps + [TGIS_LabelPosition.MiddleCenter] ;
  if btnPCenterRight.Text  = 'X' then
    lps := lps + [TGIS_LabelPosition.MiddleRight] ;
  if btnPBottomLeft.Text   = 'X' then
    lps := lps + [TGIS_LabelPosition.DownLeft] ;
  if btnPBottomCenter.Text = 'X' then
    lps := lps + [TGIS_LabelPosition.DownCenter] ;
  if btnPBottomRight.Text  = 'X' then
    lps := lps + [TGIS_LabelPosition.DownRight] ;
  if ckbPFlow.IsChecked then
    lps := lps + [TGIS_LabelPosition.Flow] ;

  if not IsStringEmpty( fbnPPosition.Field ) then
    mvc.ParamsLabel.PositionAsText := ConstructParamAsText(
                                       GIS_PARAMTXT_TYPE_FIELD,
                                       fbnPPosition.Field,
                                       ''
                                      )
  else
    mvc.ParamsLabel.PositionAsText := ConstructParamAsText(
                                        GIS_PARAMTXT_TYPE_STOCK,
                                        ConstructParamPosition( lps ) ,
                                        ''
                                      ) ;

  case cmbPAlignment.ItemIndex of
    0 : mvc.ParamsLabel.Alignment := TGIS_LabelAlignment.Single       ;
    1 : mvc.ParamsLabel.Alignment := TGIS_LabelAlignment.LeftJustify  ;
    2 : mvc.ParamsLabel.Alignment := TGIS_LabelAlignment.Center       ;
    3 : mvc.ParamsLabel.Alignment := TGIS_LabelAlignment.RightJustify ;
    4 : mvc.ParamsLabel.Alignment := TGIS_LabelAlignment.Follow       ;
  end ;

  mvc.ParamsLabel.RotateAsText  := cmbPRotation.Value ;
end ;

procedure T_panelLabel.PreparePreview(
  const _viewer : IGIS_Viewer
) ;
begin
  mvc.PreparePreview( _viewer ) ;
end ;

procedure T_panelLabel.UpdatePreview ;
var
  ll        : TGIS_Layer ;
  paramsvec : TGIS_ParamsSectionVector ;
begin
  oParentWindow.tmrUpdate.Enabled := False ;
  if oParentWindow.gisPreview.IsEmpty then exit ;
  oParentWindow.tmrUpdate.Enabled := True  ;

  ll := TGIS_Layer( oParentWindow.gisPreview.Items[0] ) ;

  paramsvec := TGIS_ParamsSectionVector( ll.Params ) ;

  mvc.SectionWrite( paramsvec ) ;

  paramsvec.Marker.Size := 0 ;

  paramsvec.Labels.Position := [ TGIS_LabelPosition.MiddleCenter ] ;
  paramsvec.Labels.Allocator := False ;

  paramsvec.Labels.Value := paramsvec.Labels.Value ;
  if ( not IsStringEmpty( paramsvec.Labels.Value ) ) or
     ( not IsStringEmpty( paramsvec.Labels.Field ) )
  then begin
    if Assigned( paramsvec.Labels.Shield ) then
      paramsvec.Labels.Value := '66'
    else
      paramsvec.Labels.Value := 'Lorem<BR>ipsum dolor' ;
  end ;

  if paramsvec.Labels.Alignment = TGIS_LabelAlignment.Follow then
     paramsvec.Labels.Rotate := DegToRad( 45 ) ;

  paramsvec.Render.Chart := '' ;
  paramsvec.Chart.Values := '' ;
  paramsvec.Query        := '' ;

  with TGIS_ParamsSection( ll.Params ) do begin
    MinZoom  := 0              ;
    MaxZoom  := GIS_MAX_DOUBLE ;
    MinScale := 0              ;
    MaxScale := GIS_MAX_DOUBLE ;
  end ;

  // avoid unreasonable preview
  paramsvec.Labels.FontColor    := normalize_color(
                                     paramsvec.Labels.FontColor,
                                     paramsvec.Labels.FontColorAsText
                                   ) ;
  paramsvec.Labels.Color        := normalize_color(
                                     paramsvec.Labels.Color,
                                     paramsvec.Labels.ColorAsText
                                   ) ;
  paramsvec.Labels.OutlineColor := normalize_color(
                                     paramsvec.Labels.OutlineColor,
                                     paramsvec.Labels.OutlineColorAsText
                                   ) ;
  paramsvec.Labels.OutlineWidth := normalize_size(
                                     oParentWindow.gisPreview,
                                     paramsvec.Labels.OutlineWidth,
                                     paramsvec.Labels.OutlineWidthAsText,
                                     MAX_PREVIEW_SIZE_OUTLINE
                                   ) ;
  paramsvec.Labels.FontSize     := normalize_size(
                                     oParentWindow.gisPreview,
                                     paramsvec.Labels.FontSize,
                                     paramsvec.Labels.FontSizeAsText,
                                     MAX_PREVIEW_SIZE_FONT
                                   ) ;
  paramsvec.Labels.Rotate       := normalize_angle(
                                     paramsvec.Labels.Rotate,
                                     paramsvec.Labels.RotateAsText
                                   ) ;
end ;

procedure T_panelLabel.actBtnPositionClick(
  _sender : TObject
) ;
var
  btn : TButton ;
begin
  btn := TButton( _sender ) ;

  if Length( btn.Text ) = 0 then
    btn.Text := 'X'
  else
    btn.Text := '' ;
end ;

procedure T_panelLabel.doCallback(
  _sender : TObject ;
  _code   : Integer
) ;

  procedure call_dependant_events ;
  begin
    mvc.DoSmartSizeFieldChange ;
    mvc.DoPositionExNotify ;
    mvc.DoAlignmentChange ;
  end ;

  procedure do_field_change ;
  var
    b : Boolean ;
  begin
    b := Length( cmbValue.Value ) <> 0 ;

    ckbVisible.Enabled := b ;
    ckbLegend.Enabled := b ;
    ckbPAvoidOverlapping.Enabled := b ;
    ckbPAvoidDuplicates.Enabled := b ;

    local_SetEnabledOnControl( gpbFont     , b ) ;
    local_SetEnabledOnControl( gpbLabel    , b ) ;
    local_SetEnabledOnControl( gpbOutline  , b ) ;
    local_SetEnabledOnControl( gpbPosition , b ) ;

    call_dependant_events ;
  end ;

  procedure do_ssfield_change ;
  begin

  end ;

  procedure do_posex ;
  begin
    if IsStringEmpty( fbnPPosition.Field ) then begin
      fldPPosition.Visible := False ;
      btnPTopLeft.Visible := True ;
      btnPTopCenter.Visible := True ;
      btnPTopRight.Visible := True ;
      btnPCenterLeft.Visible := True ;
      btnPCenterCenter.Visible := True ;
      btnPCenterRight.Visible := True ;
      btnPBottomLeft.Visible := True ;
      btnPBottomCenter.Visible := True ;
      btnPBottomRight.Visible := True ;
    end
    else begin
      btnPTopLeft.Visible := False ;
      btnPTopCenter.Visible := False ;
      btnPTopRight.Visible := False ;
      btnPCenterLeft.Visible := False ;
      btnPCenterCenter.Visible := False ;
      btnPCenterRight.Visible := False ;
      btnPBottomLeft.Visible := False ;
      btnPBottomCenter.Visible := False ;
      btnPBottomRight.Visible := False ;
      fldPPosition.Field := 'Field:' + fbnPPosition.Field ;
      fldPPosition.Visible := True ;
    end ;
  end ;

  procedure do_alignment_change ;
  var
    b : Boolean ;
  begin
    if not gpbPosition.Enabled then
      exit ;

    b := cmbPAlignment.ItemIndex <> 4 ;

    fbnPPosition.Enabled     := b ;
    btnPTopLeft.Enabled      := b ;
    btnPTopCenter.Enabled    := b ;
    btnPTopRight.Enabled     := b ;
    btnPCenterLeft.Enabled   := b ;
    btnPCenterRight.Enabled  := b ;
    btnPBottomLeft.Enabled   := b ;
    btnPBottomCenter.Enabled := b ;
    btnPBottomRight.Enabled  := b ;
  end ;

  procedure do_update_bitmap ;
  var
    bbmp : Boolean ;
  begin
    bbmp := not TGIS_Bitmap.IsNilOrEmpty( mvc.ParamsLabel.Bitmap ) ;

    if bbmp then begin
      lblColor.Enabled            := False ;
      cmbColor.Enabled            := False ;
      lblPattern.Enabled          := False ;
      cmbPattern.Enabled          := False ;
    end
    else begin
      lblColor.Enabled            := True ;
      cmbColor.Enabled            := True ;
      lblPattern.Enabled          := True ;
      cmbPattern.Enabled          := True ;
    end ;
  end ;

  procedure do_update_obitmap ;
  var
    bbmp : Boolean ;
  begin
    bbmp := not TGIS_Bitmap.IsNilOrEmpty( mvc.ParamsLabel.OutlineBitmap ) ;

    if bbmp then begin
      lblOColor.Enabled           := False ;
      cmbOColor.Enabled           := False ;
      lblOPattern.Enabled         := False ;
      cmbOPattern.Enabled         := False ;
    end
    else begin
      lblOColor.Enabled           := True ;
      cmbOColor.Enabled           := True ;
      lblOPattern.Enabled         := True  ;
      cmbOPattern.Enabled         := True  ;
    end ;
  end ;

  procedure do_shadow_change ;
  begin
    if ckbFontStyleShadow.IsChecked then
      cmbPattern.Value := ConstructParamAsText(
                            GIS_PARAMTXT_TYPE_STOCK,
                            GIS_INI_PARAM_PATTERN_TRANSPARENT,
                            ''
                          )
    else
      cmbPattern.Value := ConstructParamAsText(
                            GIS_PARAMTXT_TYPE_STOCK,
                            GIS_INI_PARAM_PATTERN_SOLID,
                            ''
                          ) ;

    doControlChange(cmbPattern);
  end;

begin

  if not blockedUpdates then begin
    Write ;
    UpdatePreview ;
  end ;

  case _code of
    10  : do_field_change ;
    70  : do_ssfield_change ;
    80  : do_posex ;
    82  : do_alignment_change ;
    100 : do_update_bitmap ;
    120 : do_update_obitmap ;

    150 : do_update_obitmap ;
    160 : do_update_bitmap ;
  end ;

end ;

procedure T_panelLabel.doFieldChange(
  _sender : TObject
) ;
begin
  mvc.DoFieldChange ;
end;

procedure T_panelLabel.doSmartSizeFieldChange(
  _sender : TObject
) ;
begin
  mvc.DoSmartSizeFieldChange ;
end ;

procedure T_panelLabel.doPositionExNotify(
  _sender : TObject
) ;
begin
  mvc.DoPositionExNotify ;
end ;

procedure T_panelLabel.doAlignmentChange(
  _sender : TObject
) ;
begin
  mvc.DoAlignmentChange ;
end ;

procedure T_panelLabel.doControlChange(
  _sender: TObject
) ;
begin
  mvc.DoControlChange ;
end ;

procedure T_panelLabel.doOPatternChange(
  _sender : TObject
) ;
begin
  mvc.DoOPatternChange ;
end;

procedure T_panelLabel.doPatternChange(
  _sender : TObject
) ;
begin
  mvc.DoPatternChange ;
end;

procedure T_panelLabel.doShieldChange(
  _sender : TObject
) ;
begin
  mvc.DoShieldChange ;
end;

procedure T_panelLabel.doShadowChange(
  _sender : TObject
) ;
begin
  mvc.DoShadowChange ;
end;
{$ENDREGION}

{$REGION 'T_panelChart'}
function T_panelChart.fget_HasPreview : Boolean ;
begin
  Result := mvc.HasPreview ;
end ;

procedure T_panelChart.init ;
begin
  inherited ;

  ItemText := _rsrcna( GIS_RS_LEGEND_PAG_CHART ) ;

  mvc := oParentWindow.MVC.Chart ;
  mvc.Callback := doCallback ;

  initChart ;
  initValues ;
  initSelf ;
end ;

procedure T_panelChart.initSelf ;
var
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  ckbLegend := TCheckBox.Create( Self.Panel ) ;
  ckbLegend.Parent := Self.Panel ;
  ckbLegend.Position.Y := gpbValues.Position.Y + gpbValues.Height + GROUPBOX_GAP;
  PlaceControl( bd, nil, ckbLegend, LEFT_3COL_1, WIDTH_2COL ) ;
  if bd = bdRightToLeft then
    ckbLegend.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    ckbLegend.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  ckbLegend.Text := _rsrcna( GIS_RS_LEGEND_PRM_INCLUDEINLEGEND ) ;
end ;

procedure T_panelChart.initChart ;
var
  t : Single ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbChart := TGroupBox.Create( Self.Panel ) ;
  gpbChart.Parent := Self.Panel ;
  gpbChart.Position.Y := GROUPBOX_TOP ;
  gpbChart.Height := 512 ;
  PlaceControl( bd, nil, gpbChart, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbChart.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbChart.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbChart.Text := _rsrcna( GIS_RS_LEGEND_TAB_CHART ) ;

  t := GROUPBOX_TOP_GAP ;

  lblStyle := TLabel.Create( gpbChart ) ;
  lblStyle.Parent := gpbChart ;
  lblStyle.Position.Y := t ;
  PlaceControl( bd, nil, lblStyle, LEFT_3COL_1, WIDTH_3COL ) ;
  lblStyle.Text := _rsrcna( GIS_RS_LEGEND_PRM_STYLE ) ;
  lblStyle.FixSize ;

  lblMinVal := TLabel.Create( gpbChart ) ;
  lblMinVal.Parent := gpbChart ;
  lblMinVal.Position.Y := t ;
  PlaceControl( bd, nil, lblMinVal, LEFT_3COL_2, WIDTH_3COL ) ;
  lblMinVal.Text := _rsrcna( GIS_RS_LEGEND_PRM_MINIMUM ) ;
  lblMinVal.FixSize ;


  lblMaxVal := TLabel.Create( gpbChart ) ;
  lblMaxVal.Parent := gpbChart ;
  lblMaxVal.Position.Y := t ;
  PlaceControl( bd, nil, lblMaxVal, LEFT_3COL_3, WIDTH_3COL ) ;
  lblMaxVal.Text := _rsrcna( GIS_RS_LEGEND_PRM_MAXIMUM ) ;
  lblMaxVal.FixSize ;

  t := t + lblStyle.Height ;

  cmbStyle := TComboBox.Create( gpbChart ) ;
  cmbStyle.Parent := gpbChart ;
  cmbStyle.Position.Y := t ;
  PlaceControl( bd, nil, cmbStyle, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbStyle.Items.BeginUpdate ;
  cmbStyle.Items.Add( _rsrc( GIS_RS_LEGEND_PRM_CHART_PIE ) ) ;
  cmbStyle.Items.Add( _rsrc( GIS_RS_LEGEND_PRM_CHART_BAR ) ) ;
  cmbStyle.ItemIndex := 0 ;
  cmbStyle.Items.EndUpdate ;
  cmbStyle.OnChange := doStyleChange ;

  edtMinVal := TEdit.Create( gpbChart ) ;
  edtMinVal.Parent := gpbChart ;
  edtMinVal.Position.Y := t ;
  PlaceControl( bd, nil, edtMinVal, LEFT_3COL_2, WIDTH_3COL ) ;
  edtMinVal.FixSize ;
  edtMinVal.KillFocusByReturn := True ;

  edtMaxVal := TEdit.Create( gpbChart ) ;
  edtMaxVal.Parent := gpbChart ;
  edtMaxVal.Position.Y := t ;
  PlaceControl( bd, nil, edtMaxVal, LEFT_3COL_3, WIDTH_3COL ) ;
  edtMaxVal.FixSize ;
  edtMaxVal.KillFocusByReturn := True ;

  t := t + cmbStyle.Height + 8 ;

  lblSize := TLabel.Create( gpbChart ) ;
  lblSize.Parent := gpbChart ;
  lblSize.Position.Y := t ;
  PlaceControl( bd, nil, lblSize, LEFT_3COL_1, WIDTH_3COL ) ;
  lblSize.Text := _rsrcna( GIS_RS_LEGEND_PRM_SIZE ) ;
  lblSize.FixSize ;

  t := t + lblMinVal.Height ;

  cmbSize := TGIS_SizeComboBox.Create( gpbChart ) ;
  cmbSize.Parent := gpbChart ;
  cmbSize.Position.Y := t ;
  PlaceControl( bd, nil, cmbSize, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbSize.Fill( True, False, False, True ) ;
  cmbSize.OnChange := doControlChange ;
  cmbSize.CustomEvent := doCustomSize ;

  t := t + edtMinVal.Height + GROUPBOX_BOTTOM_GAP ;

  gpbChart.Height := t ;
end ;

procedure T_panelChart.initValues ;
var
  t : Single ;
  w : Integer ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbValues := TGroupBox.Create( Self.Panel ) ;
  gpbValues.Parent := Self.Panel ;
  gpbValues.Position.Y  := gpbChart.Position.Y + gpbChart.Height + GROUPBOX_GAP ;
  gpbValues.Height := 512 ;
  PlaceControl( bd, nil, gpbValues, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbValues.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbValues.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbValues.Text := _rsrcna( GIS_RS_LEGEND_PRM_VALUES ) ;

  t := GROUPBOX_TOP_GAP ;

  w := WIDTH_2COL - LEFT_3COL_1 ;

  lblValue := TLabel.Create( gpbValues ) ;
  lblValue.Parent := gpbValues ;
  lblValue.Position.Y := t ;
  PlaceControl( bd, nil, lblValue, 2 * LEFT_3COL_1, w ) ;
  lblValue.Text := _rsrcna( GIS_RS_LEGEND_PRM_VALUES ) ;
  lblValue.FixSize;

  lblLegend := TLabel.Create( gpbValues ) ;
  lblLegend.Parent := gpbValues ;
  lblLegend.Position.Y := t ;
  PlaceControl( bd, nil, lblLegend, LEFT_2COL_1 + 20, WIDTH_3COL + LEFT_3COL_1 ) ;
  lblLegend.Text := _rsrcna( GIS_RS_LEGEND_PRM_LEGENDS ) ;
  lblLegend.FixSize ;

  t := t + lblValue.Height ;

  lblVal1 := TLabel.Create( gpbValues ) ;
  lblVal1.Parent := gpbValues ;
  lblVal1.Position.Y := t + 3 ;
  PlaceControl( bd, nil, lblVal1, LEFT_3COL_1, 10 ) ;
  lblVal1.Text := '1' ;
  lblVal1.FixSize ;

  cmbVal1 := TComboEdit.Create( gpbValues ) ;
  cmbVal1.Parent := gpbValues ;
  cmbVal1.Position.Y := t ;
  PlaceControl( bd, nil, cmbVal1, 2 * LEFT_3COL_1, w ) ;
  cmbVal1.ItemIndex := -1 ;
  cmbVal1.KillFocusByReturn := True ;
  cmbVal1.OnChange := doValue1Change ;
  oParentWindow.fillComboBoxWithFields( cmbVal1 ) ;

  edtVal1 := TEdit.Create( gpbValues ) ;
  edtVal1.Parent := gpbValues ;
  edtVal1.Position.Y := t ;
  PlaceControl( bd, cmbVal1, edtVal1, 4, WIDTH_3COL + LEFT_3COL_1 ) ;
  edtVal1.OnChange := doLegend1Change ;
  edtVal1.FixSize ;
  edtVal1.KillFocusByReturn := True ;

  pnlVal1 := TGIS_ColorPreview.Create( gpbValues ) ;
  pnlVal1.Parent := gpbValues ;
  pnlVal1.Position.Y := t ;
  pnlVal1.Height := 21 ;
  PlaceControl( bd, edtVal1, pnlVal1, 4, 21 ) ;
  pnlVal1.Color := TGIS_Color.Red ;

  t := t + cmbVal1.Height ;

  lblVal2 := TLabel.Create( gpbValues ) ;
  lblVal2.Parent := gpbValues ;
  lblVal2.Position.Y := t + 3 ;
  PlaceControl( bd, nil, lblVal2, LEFT_3COL_1, 10 ) ;
  lblVal2.Text := '2' ;
  lblVal2.FixSize ;

  cmbVal2 := TComboEdit.Create( gpbValues ) ;
  cmbVal2.Parent := gpbValues ;
  cmbVal2.Position.Y := t ;
  PlaceControl( bd, nil, cmbVal2, 2 * LEFT_3COL_1, w ) ;
  cmbVal2.ItemIndex := -1 ;
  cmbVal2.KillFocusByReturn := True ;
  cmbVal2.OnChange := doValue2Change ;
  oParentWindow.fillComboBoxWithFields( cmbVal2 ) ;

  edtVal2 := TEdit.Create( gpbValues ) ;
  edtVal2.Parent := gpbValues ;
  edtVal2.Position.Y := t ;
  PlaceControl( bd, cmbVal2, edtVal2, 4, WIDTH_3COL + LEFT_3COL_1 ) ;
  edtVal2.OnChange := doLegend2Change ;
  edtVal2.FixSize ;
  edtVal2.KillFocusByReturn := True ;

  pnlVal2 := TGIS_ColorPreview.Create( gpbValues ) ;
  pnlVal2.Parent := gpbValues ;
  pnlVal2.Position.Y := t ;
  pnlVal2.Height := 21 ;
  PlaceControl( bd, edtVal2, pnlVal2, 4, 21 ) ;
  pnlVal2.Color := TGIS_Color.Lime ;

  t := t + cmbVal2.Height ;

  lblVal3 := TLabel.Create( gpbValues ) ;
  lblVal3.Parent := gpbValues ;
  lblVal3.Position.Y := t + 3 ;
  PlaceControl( bd, nil, lblVal3, LEFT_3COL_1, 10 ) ;
  lblVal3.Text := '3' ;
  lblVal3.FixSize ;

  cmbVal3 := TComboEdit.Create( gpbValues ) ;
  cmbVal3.Parent := gpbValues ;
  cmbVal3.Position.Y := t ;
  PlaceControl( bd, nil, cmbVal3, 2 * LEFT_3COL_1, w ) ;
  cmbVal3.ItemIndex := -1 ;
  cmbVal3.KillFocusByReturn := True ;
  cmbVal3.OnChange := doValue3Change ;
  oParentWindow.fillComboBoxWithFields( cmbVal3 ) ;

  edtVal3 := TEdit.Create( gpbValues ) ;
  edtVal3.Parent := gpbValues ;
  edtVal3.Position.Y := t ;
  PlaceControl( bd, cmbVal3, edtVal3, 4, WIDTH_3COL + LEFT_3COL_1 ) ;
  edtVal3.OnChange := doLegend3Change ;
  edtVal3.FixSize ;
  edtVal3.KillFocusByReturn := True ;

  pnlVal3 := TGIS_ColorPreview.Create( gpbValues ) ;
  pnlVal3.Parent := gpbValues ;
  pnlVal3.Position.Y := t ;
  pnlVal3.Height := 21 ;
  PlaceControl( bd, edtVal3, pnlVal3, 4, 21 ) ;
  pnlVal3.Color := TGIS_Color.Blue ;

  t := t + cmbVal3.Height ;

  lblVal4 := TLabel.Create( gpbValues ) ;
  lblVal4.Parent := gpbValues ;
  lblVal4.Position.Y := t + 3 ;
  PlaceControl( bd, nil, lblVal4, LEFT_3COL_1, 10 ) ;
  lblVal4.Text := '4' ;
  lblVal4.FixSize ;

  cmbVal4 := TComboEdit.Create( gpbValues ) ;
  cmbVal4.Parent := gpbValues ;
  cmbVal4.Position.Y := t ;
  PlaceControl( bd, nil, cmbVal4, 2 * LEFT_3COL_1, w ) ;
  cmbVal4.ItemIndex := -1 ;
  cmbVal4.KillFocusByReturn := True ;
  cmbVal4.OnChange := doValue4Change ;
  oParentWindow.fillComboBoxWithFields( cmbVal4 ) ;

  edtVal4 := TEdit.Create( gpbValues ) ;
  edtVal4.Parent := gpbValues ;
  edtVal4.Position.Y := t ;
  PlaceControl( bd, cmbVal4, edtVal4, 4, WIDTH_3COL + LEFT_3COL_1 ) ;
  edtVal4.OnChange := doLegend4Change ;
  edtVal4.FixSize ;
  edtVal4.KillFocusByReturn := True ;

  pnlVal4 := TGIS_ColorPreview.Create( gpbValues ) ;
  pnlVal4.Parent := gpbValues ;
  pnlVal4.Position.Y := t ;
  pnlVal4.Height := 21 ;
  PlaceControl( bd, edtVal4, pnlVal4, 4, 21 ) ;
  pnlVal4.Color := TGIS_Color.Fuchsia ;

  t := t + cmbVal4.Height ;

  lblVal5 := TLabel.Create( gpbValues ) ;
  lblVal5.Parent := gpbValues ;
  lblVal5.Position.Y := t + 3 ;
  PlaceControl( bd, nil, lblVal5, LEFT_3COL_1, 10 ) ;
  lblVal5.Text := '5' ;
  lblVal5.FixSize ;

  cmbVal5 := TComboEdit.Create( gpbValues ) ;
  cmbVal5.Parent := gpbValues ;
  cmbVal5.Position.Y := t ;
  PlaceControl( bd, nil, cmbVal5, 2 * LEFT_3COL_1, w ) ;
  cmbVal5.ItemIndex := -1 ;
  cmbVal5.KillFocusByReturn := True ;
  cmbVal5.OnChange := doValue5Change ;
  oParentWindow.fillComboBoxWithFields( cmbVal5 ) ;

  edtVal5 := TEdit.Create( gpbValues ) ;
  edtVal5.Parent := gpbValues ;
  edtVal5.Position.Y := t ;
  PlaceControl( bd, cmbVal5, edtVal5, 4, WIDTH_3COL + LEFT_3COL_1 ) ;
  edtVal5.OnChange := doLegend5Change ;
  edtVal5.FixSize ;
  edtVal5.KillFocusByReturn := True ;

  pnlVal5 := TGIS_ColorPreview.Create( gpbValues ) ;
  pnlVal5.Parent := gpbValues ;
  pnlVal5.Position.Y := t ;
  pnlVal5.Height := 21 ;
  PlaceControl( bd, edtVal5, pnlVal5, 4, 21 ) ;
  pnlVal5.Color := TGIS_Color.Aqua ;

  t := t + cmbVal5.Height ;

  lblVal6 := TLabel.Create( gpbValues ) ;
  lblVal6.Parent := gpbValues ;
  lblVal6.Position.Y := t + 3 ;
  PlaceControl( bd, nil, lblVal6, LEFT_3COL_1, 10 ) ;
  lblVal6.Text := '6' ;
  lblVal6.FixSize ;

  cmbVal6 := TComboEdit.Create( gpbValues ) ;
  cmbVal6.Parent := gpbValues ;
  cmbVal6.Position.Y := t ;
  PlaceControl( bd, nil, cmbVal6, 2 * LEFT_3COL_1, w ) ;
  cmbVal6.ItemIndex := -1 ;
  cmbVal6.KillFocusByReturn := True ;
  cmbVal6.OnChange := doValue6Change ;
  oParentWindow.fillComboBoxWithFields( cmbVal6 ) ;

  edtVal6 := TEdit.Create( gpbValues ) ;
  edtVal6.Parent := gpbValues ;
  edtVal6.Position.Y := t ;
  PlaceControl( bd, cmbVal6, edtVal6, 4, WIDTH_3COL + LEFT_3COL_1 ) ;
  edtVal6.OnChange := doLegend6Change ;
  edtVal6.FixSize ;
  edtVal6.KillFocusByReturn := True ;

  pnlVal6 := TGIS_ColorPreview.Create( gpbValues ) ;
  pnlVal6.Parent := gpbValues ;
  pnlVal6.Position.Y := t ;
  pnlVal6.Height := 21 ;
  PlaceControl( bd, edtVal6, pnlVal6, 4, 21 ) ;
  pnlVal6.Color := TGIS_Color.Green ;

  t := t + cmbVal6.Height ;

  lblVal7 := TLabel.Create( gpbValues ) ;
  lblVal7.Parent := gpbValues ;
  lblVal7.Position.Y := t + 3 ;
  PlaceControl( bd, nil, lblVal7, LEFT_3COL_1, 10 ) ;
  lblVal7.Text := '7' ;
  lblVal7.FixSize ;

  cmbVal7 := TComboEdit.Create( gpbValues ) ;
  cmbVal7.Parent := gpbValues ;
  cmbVal7.Position.Y := t ;
  PlaceControl( bd, nil, cmbVal7, 2 * LEFT_3COL_1, w ) ;
  cmbVal7.ItemIndex := -1 ;
  cmbVal7.KillFocusByReturn := True ;
  cmbVal7.OnChange := doValue7Change ;
  oParentWindow.fillComboBoxWithFields( cmbVal7 ) ;

  edtVal7 := TEdit.Create( gpbValues ) ;
  edtVal7.Parent := gpbValues ;
  edtVal7.Position.Y := t ;
  PlaceControl( bd, cmbVal7, edtVal7, 4, WIDTH_3COL + LEFT_3COL_1 ) ;
  edtVal7.OnChange := doLegend7Change ;
  edtVal7.FixSize ;
  edtVal7.KillFocusByReturn := True ;

  pnlVal7 := TGIS_ColorPreview.Create( gpbValues ) ;
  pnlVal7.Parent := gpbValues ;
  pnlVal7.Position.Y := t ;
  pnlVal7.Height := 21 ;
  PlaceControl( bd, edtVal7, pnlVal7, 4, 21 ) ;
  pnlVal7.Color := TGIS_Color.White ;

  t := t + cmbVal7.Height ;

  lblVal8 := TLabel.Create( gpbValues ) ;
  lblVal8.Parent := gpbValues ;
  lblVal8.Position.Y := t + 3 ;
  PlaceControl( bd, nil, lblVal8, LEFT_3COL_1, 10 ) ;
  lblVal8.Text := '8' ;
  lblVal8.FixSize ;

  cmbVal8 := TComboEdit.Create( gpbValues ) ;
  cmbVal8.Parent := gpbValues ;
  cmbVal8.Position.Y := t ;
  PlaceControl( bd, nil, cmbVal8, 2 * LEFT_3COL_1, w ) ;
  cmbVal8.ItemIndex := -1 ;
  cmbVal8.KillFocusByReturn := True ;
  cmbVal8.OnChange := doValue8Change ;
  oParentWindow.fillComboBoxWithFields( cmbVal8 ) ;

  edtVal8 := TEdit.Create( gpbValues ) ;
  edtVal8.Parent := gpbValues ;
  edtVal8.Position.Y := t ;
  PlaceControl( bd, cmbVal8, edtVal8, 4, WIDTH_3COL + LEFT_3COL_1 ) ;
  edtVal8.OnChange := doLegend8Change ;
  edtVal8.FixSize ;
  edtVal8.KillFocusByReturn := True ;

  pnlVal8 := TGIS_ColorPreview.Create( gpbValues ) ;
  pnlVal8.Parent := gpbValues ;
  pnlVal8.Position.Y := t ;
  pnlVal8.Height := 21 ;
  PlaceControl( bd, edtVal8, pnlVal8, 4, 21 ) ;
  pnlVal8.Color := TGIS_Color.Black ;

  t := t + cmbVal8.Height + GROUPBOX_BOTTOM_GAP ;

  gpbValues.Height := t ;
end ;

procedure T_panelChart.Read ;

    function get_params( const _txt : String; const _idx : Integer ) : String ;
    var
      i,j : Integer ;
    begin
      j := 0 ;
      Result := '' ;
      for i:= 1 to Length( _txt ) do begin
        if _txt[i] = ':' then begin
          if j = _idx then break ;
          Result := '' ;
          Inc( j ) ;
        end
        else
          Result := Result + _txt[i] ;
      end ;
      if j <> _idx then
        Result := '' ;
    end ;

begin
  lockUpdates ;
  try
    ckbLegend.IsChecked := mvc.ParamsChart.ShowLegend ;

    // Chart
    case mvc.ParamsChart.Style of
      TGIS_ChartStyle.Pie : cmbStyle.ItemIndex := 0 ;
      TGIS_ChartStyle.Bar : cmbStyle.ItemIndex := 1 ;
    end ;

    cmbSize.Value   := mvc.ParamsChart.SizeAsText ;

    edtMinVal.Text  := get_params( mvc.RenderChart, 0 ) ;
    edtMaxVal.Text  := get_params( mvc.RenderChart, 1 ) ;

    // Values
    cmbVal1.Text  := get_params( mvc.RenderChart, 2 ) ;
    edtVal1.Text  := get_params( mvc.ParamsChart.Legend, 2 ) ;
    pnlVal1.Color := mvc.ParamsChart.ColorsInternal[0];
    cmbVal2.Text  := get_params( mvc.RenderChart, 3 ) ;
    edtVal2.Text  := get_params( mvc.ParamsChart.Legend, 3 ) ;
    pnlVal2.Color := mvc.ParamsChart.ColorsInternal[1];
    cmbVal3.Text  := get_params( mvc.RenderChart, 4 ) ;
    edtVal3.Text  := get_params( mvc.ParamsChart.Legend, 4 ) ;
    pnlVal3.Color := mvc.ParamsChart.ColorsInternal[2];
    cmbVal4.Text  := get_params( mvc.RenderChart, 5 ) ;
    edtVal4.Text  := get_params( mvc.ParamsChart.Legend, 5 ) ;
    pnlVal4.Color := mvc.ParamsChart.ColorsInternal[3];
    cmbVal5.Text  := get_params( mvc.RenderChart, 6 ) ;
    edtVal5.Text  := get_params( mvc.ParamsChart.Legend, 6 ) ;
    pnlVal5.Color := mvc.ParamsChart.ColorsInternal[4];
    cmbVal6.Text  := get_params( mvc.RenderChart, 7 ) ;
    edtVal6.Text  := get_params( mvc.ParamsChart.Legend, 7 ) ;
    pnlVal6.Color := mvc.ParamsChart.ColorsInternal[5];
    cmbVal7.Text  := get_params( mvc.RenderChart, 8 ) ;
    edtVal7.Text  := get_params( mvc.ParamsChart.Legend, 8 ) ;
    pnlVal7.Color := mvc.ParamsChart.ColorsInternal[6];
    cmbVal8.Text  := get_params( mvc.RenderChart, 9 ) ;
    edtVal8.Text  := get_params( mvc.ParamsChart.Legend, 9 ) ;
    pnlVal8.Color := mvc.ParamsChart.ColorsInternal[7];

    wasLegend1Edited := False ;
    wasLegend2Edited := False ;
    wasLegend3Edited := False ;
    wasLegend4Edited := False ;
    wasLegend5Edited := False ;
    wasLegend6Edited := False ;
    wasLegend7Edited := False ;
    wasLegend8Edited := False ;

    mvc.DoStyleChange ;
    mvc.DoSizeUseRenderer ;
  finally
    unlockUpdates ;
  end ;
end ;

procedure T_panelChart.Write ;

    function add_val( const _val : String ) : String ;
    begin
      Result := _val + ':'
    end ;

    function add_color_val( const _val : TGIS_Color ) : String ;
    begin
      Result := _val.R.ToString + ':' + _val.G.ToString + ':' +
                _val.B.ToString + ':' + _val.A.ToString + ',' ;
    end;

begin
  mvc.ParamsChart.ShowLegend := ckbLegend.IsChecked ;

  // Chart
  case cmbStyle.ItemIndex of
    0 : mvc.ParamsChart.Style := TGIS_ChartStyle.Pie ;
    1 : mvc.ParamsChart.Style := TGIS_ChartStyle.Bar ;
  end ;

  mvc.ParamsChart.SizeAsText  := cmbSize.Value ;

  mvc.RenderChart := add_val( edtMinVal.Text ) +
                     add_val( edtMaxVal.Text ) +
                     add_val( cmbVal1.Text   ) +
                     add_val( cmbVal2.Text   ) +
                     add_val( cmbVal3.Text   ) +
                     add_val( cmbVal4.Text   ) +
                     add_val( cmbVal5.Text   ) +
                     add_val( cmbVal6.Text   ) +
                     add_val( cmbVal7.Text   ) +
                     add_val( cmbVal8.Text   ) ;
  mvc.ParamsChart.Values := mvc.RenderChart ;
  mvc.ParamsChart.Legend := add_val( '' ) +
                            add_val( '' ) +
                            add_val( edtVal1.Text   ) +
                            add_val( edtVal2.Text   ) +
                            add_val( edtVal3.Text   ) +
                            add_val( edtVal4.Text   ) +
                            add_val( edtVal5.Text   ) +
                            add_val( edtVal6.Text   ) +
                            add_val( edtVal7.Text   ) +
                            add_val( edtVal8.Text   ) ;
  mvc.ParamsChart.Colors := add_color_val( pnlVal1.Color ) +
                            add_color_val( pnlVal2.Color ) +
                            add_color_val( pnlVal3.Color ) +
                            add_color_val( pnlVal4.Color ) +
                            add_color_val( pnlVal5.Color ) +
                            add_color_val( pnlVal6.Color ) +
                            add_color_val( pnlVal7.Color ) +
                            add_color_val( pnlVal8.Color ) ;
end ;

procedure T_panelChart.PreparePreview(
  const _viewer : IGIS_Viewer
) ;
begin
  mvc.PreparePreview( _viewer ) ;
end ;

procedure T_panelChart.UpdatePreview ;
var
  ll        : TGIS_Layer ;
  paramsvec : TGIS_ParamsSectionVector ;
  txt       : String ;
  i         : Integer ;
begin
  oParentWindow.tmrUpdate.Enabled := False ;
  if oParentWindow.gisPreview.IsEmpty then exit ;
  oParentWindow.tmrUpdate.Enabled := True  ;

  ll := TGIS_Layer( oParentWindow.gisPreview.Items[0] ) ;

  paramsvec := TGIS_ParamsSectionVector( ll.Params ) ;

  mvc.SectionWrite( paramsvec ) ;

  paramsvec.Marker.Size := 0 ;

  txt := '0:0' ;
  if Assigned( paramsvec.Render.ChartObj ) then begin
    for i:=2 to paramsvec.Render.ChartObj.Count - 1 do begin
      if Assigned(paramsvec.Render.ChartObj[i]) then
        txt := txt + ':1'
      else
        txt := txt + ':'  ;
    end ;
  end ;
  paramsvec.Render.Chart := '' ;
  paramsvec.Chart.Values := txt ;

  if paramsvec.Chart.Size = GIS_RENDER_SIZE then
    paramsvec.Chart.Size := - TruncS( oParentWindow.gisPreview.ControlCanvasWidth/2 ) ;

  paramsvec.Query := '' ;

  with TGIS_ParamsSection( ll.Params ) do begin
    MinZoom  := 0              ;
    MaxZoom  := GIS_MAX_DOUBLE ;
    MinScale := 0              ;
    MaxScale := GIS_MAX_DOUBLE ;
  end ;

  // avoid unreasonable preview
  paramsvec.Chart.Size := normalize_size(
                            oParentWindow.gisPreview,
                            paramsvec.Chart.Size,
                            paramsvec.Chart.SizeAsText,
                            MAX_PREVIEW_SIZE_MAKER
                          ) ;
end ;

procedure T_panelChart.doCallback(
  _sender : TObject ;
  _code   : Integer
) ;

  procedure do_style_change ;
  var
    b : Boolean ;
  begin
    b := cmbStyle.ItemIndex = 1 ;

    lblMinVal.Enabled := b ;
    edtMinVal.Enabled := b ;
    lblMaxVal.Enabled := b ;
    edtMaxVal.Enabled := b ;
  end ;

  procedure do_size_ur ;
  begin
  end ;

  procedure do_value1_change ;
  begin
    if not wasLegend1Edited then
      edtVal1.Text := cmbVal1.Text ;
  end ;

  procedure do_legend1_change ;
  begin
    if wasLegend1Edited then
      exit ;

    if edtVal1.Text <> cmbVal1.Text then
      wasLegend1Edited := True ;
  end ;

  procedure do_value2_change ;
  begin
    if not wasLegend2Edited then
      edtVal2.Text := cmbVal2.Text ;
  end ;

  procedure do_legend2_change ;
  begin
    if wasLegend2Edited then
      exit ;

    if edtVal2.Text <> cmbVal2.Text then
      wasLegend2Edited := True ;
  end ;

  procedure do_value3_change ;
  begin
    if not wasLegend3Edited then
      edtVal3.Text := cmbVal3.Text ;
  end ;

  procedure do_legend3_change ;
  begin
    if wasLegend3Edited then
      exit ;

    if edtVal3.Text <> cmbVal3.Text then
      wasLegend3Edited := True ;
  end ;

  procedure do_value4_change ;
  begin
    if not wasLegend4Edited then
      edtVal4.Text := cmbVal4.Text ;
  end ;

  procedure do_legend4_change ;
  begin
    if wasLegend4Edited then
      exit ;

    if edtVal4.Text <> cmbVal4.Text then
      wasLegend4Edited := True ;
  end ;

  procedure do_value5_change ;
  begin
    if not wasLegend5Edited then
      edtVal5.Text := cmbVal5.Text ;
  end ;

  procedure do_legend5_change ;
  begin
    if wasLegend5Edited then
      exit ;

    if edtVal5.Text <> cmbVal5.Text then
      wasLegend5Edited := True ;
  end ;

  procedure do_value6_change ;
  begin
    if not wasLegend6Edited then
      edtVal6.Text := cmbVal6.Text ;
  end ;

  procedure do_legend6_change ;
  begin
    if wasLegend6Edited then
      exit ;

    if edtVal6.Text <> cmbVal6.Text then
      wasLegend6Edited := True ;
  end ;

  procedure do_value7_change ;
  begin
    if not wasLegend7Edited then
      edtVal7.Text := cmbVal7.Text ;
  end ;

  procedure do_legend7_change ;
  begin
    if wasLegend7Edited then
      exit ;

    if edtVal7.Text <> cmbVal7.Text then
      wasLegend7Edited := True ;
  end ;

  procedure do_value8_change ;
  begin
    if not wasLegend8Edited then
      edtVal8.Text := cmbVal8.Text ;
  end ;

  procedure do_legend8_change ;
  begin
    if wasLegend8Edited then
      exit ;

    if edtVal8.Text <> cmbVal8.Text then
      wasLegend8Edited := True ;
  end ;

begin
  case _code of
    1  : do_style_change ;
    2  : do_size_ur ;
    10 : do_value1_change ;
    11 : do_legend1_change ;
    20 : do_value2_change ;
    21 : do_legend2_change ;
    30 : do_value3_change ;
    31 : do_legend3_change ;
    40 : do_value4_change ;
    41 : do_legend4_change ;
    50 : do_value5_change ;
    51 : do_legend5_change ;
    60 : do_value6_change ;
    61 : do_legend6_change ;
    70 : do_value7_change ;
    71 : do_legend7_change ;
    80 : do_value8_change ;
    81 : do_legend8_change ;
  end ;

  if not blockedUpdates then begin
    Write ;
    UpdatePreview ;
  end ;

end ;

procedure T_panelChart.doStyleChange(
  _sender : TObject
) ;
begin
  mvc.DoStyleChange ;
end ;

procedure T_panelChart.doSizeUseRenderer(
  _sender : TObject
) ;
begin
  mvc.DoSizeUseRenderer ;
end ;

procedure T_panelChart.doValue1Change(
  _sender : TObject
) ;
begin
  mvc.DoValue1Change ;
end ;

procedure T_panelChart.doLegend1Change(
  _sender : TObject
) ;
begin
  mvc.DoLegend1Change ;
end ;

procedure T_panelChart.doValue2Change(
  _sender : TObject
) ;
begin
  mvc.DoValue2Change ;
end ;

procedure T_panelChart.doLegend2Change(
  _sender : TObject
) ;
begin
  mvc.DoLegend2Change ;
end ;

procedure T_panelChart.doValue3Change(
  _sender : TObject
) ;
begin
  mvc.DoValue3Change ;
end ;

procedure T_panelChart.doLegend3Change(
  _sender : TObject
) ;
begin
  mvc.DoLegend3Change ;
end ;

procedure T_panelChart.doValue4Change(
  _sender : TObject
) ;
begin
  mvc.DoValue4Change ;
end ;

procedure T_panelChart.doLegend4Change(
  _sender : TObject
) ;
begin
  mvc.DoLegend4Change ;
end ;

procedure T_panelChart.doValue5Change(
  _sender : TObject
) ;
begin
  mvc.DoValue5Change ;
end ;

procedure T_panelChart.doLegend5Change(
  _sender : TObject
) ;
begin
  mvc.DoLegend5Change ;
end ;

procedure T_panelChart.doValue6Change(
  _sender : TObject
) ;
begin
  mvc.DoValue6Change ;
end ;

procedure T_panelChart.doLegend6Change(
  _sender : TObject
) ;
begin
  mvc.DoLegend6Change ;
end ;

procedure T_panelChart.doValue7Change(
  _sender : TObject
) ;
begin
  mvc.DoValue7Change ;
end ;

procedure T_panelChart.doLegend7Change(
  _sender : TObject
) ;
begin
  mvc.DoLegend7Change ;
end ;

procedure T_panelChart.doValue8Change(
  _sender : TObject
) ;
begin
  mvc.DoValue8Change ;
end ;

procedure T_panelChart.doLegend8Change(
  _sender : TObject
) ;
begin
  mvc.DoLegend8Change ;
end ;

procedure T_panelChart.doControlChange(
  _sender: TObject
) ;
begin
  mvc.DoControlChange ;
end ;
{$ENDREGION}

{$REGION 'T_panelPixel'}
function T_panelPixel.fget_HasPreview : Boolean ;
begin
  Result := mvc.HasPreview ;
end ;

procedure T_panelPixel.init ;
begin
  inherited ;

  ItemText := _rsrcna( GIS_RS_LEGEND_PAG_PIXEL ) ;

  mvc := oParentWindow.MVC.Pixel ;
  mvc.Callback := doCallback ;

  initSelf ;
end ;

procedure T_panelPixel.initSelf ;
var
  i : Integer ;
  t : Single ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbColors := TGroupBox.Create( Self.Panel ) ;
  gpbColors.Parent := Self.Panel ;
  gpbColors.Position.Y := GROUPBOX_TOP ;
  gpbColors.Height := 200 ;
  PlaceControl( bd, nil, gpbColors, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbColors.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbColors.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbColors.Text := 'Colors' ;

  t := GROUPBOX_TOP_GAP ;

  lblRed := TLabel.Create( gpbColors ) ;
  lblRed.Parent := gpbColors ;
  lblRed.Position.Y := t ;
  PlaceControl( bd, nil, lblRed, LEFT_3COL_1, WIDTH_3COL ) ;
  lblRed.Text := _rsrcna( GIS_RS_LEGEND_PRM_RED ) ;
  lblRed.FixSize ;

  lblGreen := TLabel.Create( gpbColors ) ;
  lblGreen.Parent := gpbColors ;
  lblGreen.Position.Y := t ;
  PlaceControl( bd, nil, lblGreen, LEFT_3COL_2, WIDTH_3COL ) ;
  lblGreen.Text := _rsrcna( GIS_RS_LEGEND_PRM_GREEN ) ;
  lblGreen.FixSize ;

  lblBlue := TLabel.Create( gpbColors ) ;
  lblBlue.Parent := gpbColors ;
  lblBlue.Position.Y := t ;
  PlaceControl( bd, nil, lblBlue, LEFT_3COL_3, WIDTH_3COL ) ;
  lblBlue.Text := _rsrcna( GIS_RS_LEGEND_PRM_BLUE ) ;
  lblBlue.FixSize ;

  t := t + lblRed.Height + 4 ;

  speRed := TEdit.Create( gpbColors ) ;
  speRed.Parent := gpbColors ;
  speRed.Position.Y := t ;
  PlaceControl( bd, nil, speRed, LEFT_3COL_1, WIDTH_3COL ) ;
  speRed.OnChange := doControlChange ;
  speRed.KillFocusByReturn := True ;

  vvspeRed := TGIS_ValueValidatorEditHelper.Create( speRed ) ;
  vvspeRed.MinVal := -100.0 ;
  vvspeRed.MaxVal :=  100.0 ;

  speGreen := TEdit.Create( gpbColors ) ;
  speGreen.Parent := gpbColors ;
  speGreen.Position.Y := t ;
  PlaceControl( bd, nil, speGreen, LEFT_3COL_2, WIDTH_3COL ) ;
  speGreen.OnChange := doControlChange ;
  speGreen.KillFocusByReturn := True ;

  vvspeGreen := TGIS_ValueValidatorEditHelper.Create( speGreen ) ;
  vvspeGreen.MinVal := -100.0 ;
  vvspeGreen.MaxVal :=  100.0 ;

  speBlue := TEdit.Create( gpbColors ) ;
  speBlue.Parent := gpbColors ;
  speBlue.Position.Y := t ;
  PlaceControl( bd, nil, speBlue, LEFT_3COL_3, WIDTH_3COL ) ;
  speBlue.OnChange := doControlChange ;
  speBlue.KillFocusByReturn := True ;

  vvspeBlue := TGIS_ValueValidatorEditHelper.Create( speBlue ) ;
  vvspeBlue.MinVal := -100.0 ;
  vvspeBlue.MaxVal :=  100.0 ;

  t := t + speRed.Height + 16 ;

  lblBrightness := TLabel.Create( gpbColors ) ;
  lblBrightness.Parent := gpbColors ;
  lblBrightness.Position.Y := t ;
  PlaceControl( bd, nil, lblBrightness, LEFT_3COL_1, WIDTH_3COL ) ;
  lblBrightness.Text := _rsrcna( GIS_RS_LEGEND_PRM_BRIGHTNESS ) ;
  lblBrightness.FixSize ;

  lblContrast := TLabel.Create( gpbColors ) ;
  lblContrast.Parent := gpbColors ;
  lblContrast.Position.Y := t ;
  PlaceControl( bd, nil, lblContrast, LEFT_3COL_2, WIDTH_3COL ) ;
  lblContrast.Text := _rsrcna( GIS_RS_LEGEND_PRM_CONTRAST ) ;
  lblContrast.FixSize ;

  t := t + lblBrightness.Height + 4 ;

  speBrightness := TEdit.Create( gpbColors ) ;
  speBrightness.Parent := gpbColors ;
  speBrightness.Position.Y := t ;
  PlaceControl( bd, nil, speBrightness, LEFT_3COL_1, WIDTH_3COL ) ;
  speBrightness.OnChange := doControlChange ;
  speBrightness.KillFocusByReturn := True ;

  vvspeBrightness := TGIS_ValueValidatorEditHelper.Create( speBrightness ) ;
  vvspeBrightness.MinVal := -100.0 ;
  vvspeBrightness.MaxVal :=  100.0 ;

  speContrast := TEdit.Create( gpbColors ) ;
  speContrast.Parent := gpbColors ;
  speContrast.Position.Y := t ;
  PlaceControl( bd, nil, speContrast, LEFT_3COL_2, WIDTH_3COL ) ;
  speContrast.OnChange := doControlChange ;
  speContrast.KillFocusByReturn := True ;

  vvspeContrast := TGIS_ValueValidatorEditHelper.Create( speContrast ) ;
  vvspeContrast.MinVal := -100.0 ;
  vvspeContrast.MaxVal :=  100.0 ;

  t := t + speContrast.Height + 8 ;

  ckbInversion := TCheckBox.Create( gpbColors ) ;
  ckbInversion.Parent := gpbColors ;
  ckbInversion.Position.Y := t ;
  PlaceControl( bd, nil, ckbInversion, LEFT_3COL_1, WIDTH_3COL ) ;
  ckbInversion.Text := _rsrcna( GIS_RS_LEGEND_PRM_INVERSION ) ;
  ckbInversion.OnChange := doControlChange ;

  ckbGrayscale := TCheckBox.Create( gpbColors ) ;
  ckbGrayscale.Parent := gpbColors ;
  ckbGrayscale.Position.Y := t ;
  PlaceControl( bd, nil, ckbGrayscale, LEFT_3COL_2, WIDTH_3COL ) ;
  ckbGrayscale.Text := _rsrcna( GIS_RS_LEGEND_PRM_GRAYSCALE ) ;
  ckbGrayscale.OnChange := doControlChange ;

  ckbHistogram := TCheckBox.Create( gpbColors ) ;
  ckbHistogram.Parent := gpbColors ;
  ckbHistogram.Position.Y := t ;
  PlaceControl( bd, nil, ckbHistogram, LEFT_3COL_3, WIDTH_3COL ) ;
  ckbHistogram.Text := _rsrcna( GIS_RS_LEGEND_PRM_HISTOGRAM ) ;
  ckbHistogram.OnChange := doControlChange ;

  t := t + ckbHistogram.Height + 8 ;

  ckbContrastEnhanced := TCheckBox.Create( gpbColors ) ;
  ckbContrastEnhanced.Parent := gpbColors ;
  ckbContrastEnhanced.Position.Y := t ;
  PlaceControl( bd, nil, ckbContrastEnhanced, LEFT_3COL_1, WIDTH_3COL ) ;
  ckbContrastEnhanced.Text := _rsrcna( GIS_RS_LEGEND_PRM_CONTRASTENHANCED ) ;
  ckbContrastEnhanced.OnChange := doControlChange ;

  gpbColors.Height := t + ckbContrastEnhanced.Height + GROUPBOX_BOTTOM_GAP ;

  t := gpbColors.Position.Y + gpbColors.Height + GROUPBOX_GAP ;

  gpbBands := TGroupBox.Create( Self.Panel ) ;
  gpbBands.Parent := Self.Panel ;
  gpbBands.Position.Y  := t ;
  gpbBands.Height := 200 ;
  PlaceControl( bd, nil, gpbBands, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbBands.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbBands.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbBands.Text := 'Bands' ;

  t := GROUPBOX_TOP_GAP ;

  lblRedBand := TLabel.Create( gpbBands ) ;
  lblRedBand.Parent := gpbBands ;
  lblRedBand.Position.Y := t ;
  PlaceControl( bd, nil, lblRedBand, LEFT_3COL_1, WIDTH_3COL ) ;
  lblRedBand.Text := _rsrcna( GIS_RS_LEGEND_PRM_REDBAND ) ;
  lblRedBand.FixSize ;

  lblGreenBand := TLabel.Create( gpbBands ) ;
  lblGreenBand.Parent := gpbBands ;
  lblGreenBand.Position.Y := t ;
  PlaceControl( bd, nil, lblGreenBand, LEFT_3COL_2, WIDTH_3COL ) ;
  lblGreenBand.Text := _rsrcna( GIS_RS_LEGEND_PRM_GREENBAND ) ;
  lblGreenBand.FixSize ;

  lblBlueBand := TLabel.Create( gpbBands ) ;
  lblBlueBand.Parent := gpbBands ;
  lblBlueBand.Position.Y := t ;
  PlaceControl( bd, nil, lblBlueBand, LEFT_3COL_3, WIDTH_3COL ) ;
  lblBlueBand.Text := _rsrcna( GIS_RS_LEGEND_PRM_BLUEBAND ) ;
  lblBlueBand.FixSize ;

  t := t + lblRedBand.Height + 4 ;

  cmbRedBand := TComboBox.Create( gpbBands ) ;
  cmbRedBand.Parent := gpbBands ;
  cmbRedBand.Position.Y := t ;
  PlaceControl( bd, nil, cmbRedBand, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbRedBand.Items.BeginUpdate ;
  cmbRedBand.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_OFF ) ) ;
  cmbRedBand.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_DEFAULT ) ) ;
  for i := 0 to TGIS_LayerPixel(oParentWindow.MVC.Layer).BandsCount -1 do
    cmbRedBand.Items.Add( IntToStr( i+1 ) ) ;

  cmbRedBand.ItemIndex := 1 ;
  cmbRedBand.Items.EndUpdate ;
  cmbRedBand.OnChange := doControlChange ;

  cmbGreenBand := TComboBox.Create( gpbBands ) ;
  cmbGreenBand.Parent := gpbBands ;
  cmbGreenBand.Position.Y := t ;
  PlaceControl( bd, nil, cmbGreenBand, LEFT_3COL_2, WIDTH_3COL ) ;
  cmbGreenBand.Items.BeginUpdate ;
  cmbGreenBand.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_OFF ) ) ;
  cmbGreenBand.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_DEFAULT ) ) ;
  for i := 0 to TGIS_LayerPixel(oParentWindow.MVC.Layer).BandsCount -1 do
    cmbGreenBand.Items.Add( IntToStr( i+1 ) ) ;

  cmbGreenBand.ItemIndex := 1 ;
  cmbGreenBand.Items.EndUpdate ;
  cmbGreenBand.OnChange := doControlChange ;

  cmbBlueBand := TComboBox.Create( gpbBands ) ;
  cmbBlueBand.Parent := gpbBands ;
  cmbBlueBand.Position.Y := t ;
  PlaceControl( bd, nil, cmbBlueBand, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbBlueBand.Items.BeginUpdate ;
  cmbBlueBand.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_OFF ) ) ;
  cmbBlueBand.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_DEFAULT ) ) ;
  for i := 0 to TGIS_LayerPixel(oParentWindow.MVC.Layer).BandsCount -1 do
    cmbBlueBand.Items.Add( IntToStr( i+1 ) ) ;

  cmbBlueBand.ItemIndex := 1 ;
  cmbBlueBand.Items.EndUpdate ;
  cmbBlueBand.OnChange := doControlChange ;

  t := t + cmbRedBand.Height + 16 ;

  lblAlphaBand := TLabel.Create( gpbBands ) ;
  lblAlphaBand.Parent := gpbBands ;
  lblAlphaBand.Position.Y := t ;
  PlaceControl( bd, nil, lblAlphaBand, LEFT_3COL_1, WIDTH_3COL ) ;
  lblAlphaBand.Text := _rsrcna( GIS_RS_LEGEND_PRM_ALPHABAND ) ;
  lblAlphaBand.FixSize ;

  lblPage := TLabel.Create( gpbBands ) ;
  lblPage.Parent := gpbBands ;
  lblPage.Position.Y := t ;
  PlaceControl( bd, nil, lblPage, LEFT_3COL_3, WIDTH_3COL ) ;
  lblPage.Text := _rsrcna( GIS_RS_LEGEND_PRM_PAGE ) ;
  lblPage.FixSize ;

  t := t + lblPage.Height + 4 ;

  cmbAlphaBand := TComboBox.Create( gpbBands ) ;
  cmbAlphaBand.Parent := gpbBands ;
  cmbAlphaBand.Position.Y := t ;
  PlaceControl( bd, nil, cmbAlphaBand, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbAlphaBand.Items.BeginUpdate ;
  cmbAlphaBand.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_OFF ) ) ;
  cmbAlphaBand.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_DEFAULT ) ) ;
  for i := 0 to TGIS_LayerPixel(oParentWindow.MVC.Layer).BandsCount -1 do
    cmbAlphaBand.Items.Add( IntToStr( i+1 ) ) ;
  cmbAlphaBand.ItemIndex := 1 ;
  cmbAlphaBand.Items.EndUpdate ;
  cmbAlphaBand.OnChange := doControlChange ;

  cmbPage := TComboBox.Create( gpbBands ) ;
  cmbPage.Parent := gpbBands ;
  cmbPage.Position.Y := t ;
  PlaceControl( bd, nil, cmbPage, LEFT_3COL_3, WIDTH_3COL ) ;
  cmbPage.Items.BeginUpdate ;
  for i := 0 to TGIS_LayerPixel(oParentWindow.MVC.Layer).PageCount -1 do
    cmbPage.Items.Add( IntToStr( i+1 ) ) ;
  cmbPage.ItemIndex := 0 ;
  cmbPage.Items.EndUpdate ;
  cmbPage.OnChange := doControlChange ;

  gpbBands.Height := t + cmbPage.Height + GROUPBOX_BOTTOM_GAP ;

  gpbTransparency := TGroupBox.Create( Self.Panel ) ;
  gpbTransparency.Parent := Self.Panel ;
  gpbTransparency.Position.Y  := gpbBands.Position.Y + gpbBands.Height + GROUPBOX_GAP ;
  gpbTransparency.Height := 200 ;
  PlaceControl( bd, nil, gpbTransparency, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbTransparency.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbTransparency.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbTransparency.Text := _rsrcna( GIS_RS_LEGEND_PRM_TRANSPARENCY ) ;

  t := GROUPBOX_TOP_GAP ;

  pnlColorFrom := TGIS_ColorPreview.Create( gpbTransparency ) ;
  pnlColorFrom.Parent := gpbTransparency ;
  pnlColorFrom.Position.Y := t ;
  pnlColorFrom.Height := 21 ;
  PlaceControl( bd, nil, pnlColorFrom, LEFT_3COL_1 + 23 + 8 - 2, WIDTH_3COL ) ;
  pnlColorFrom.CellSize := 5 ;
  pnlColorFrom.OnDialogChange := doZonesChange ;

  pnlColorTo := TGIS_ColorPreview.Create( gpbTransparency ) ;
  pnlColorTo.Parent := gpbTransparency ;
  pnlColorTo.Position.Y    := t ;
  pnlColorTo.Height := 21 ;
  PlaceControl( bd, pnlColorFrom, pnlColorTo, 4, WIDTH_3COL ) ;
  pnlColorTo.CellSize := 5 ;
  pnlColorTo.OnDialogChange := doZonesChange ;

  t := t + pnlColorTo.Height + 4 ;

  btnAdd := TButton.Create( gpbTransparency ) ;
  btnAdd.Parent := gpbTransparency ;
  btnAdd.Position.Y  := t ;
  btnAdd.Height := 25 ;
  PlaceControl( bd, nil, btnAdd, LEFT_3COL_1, 25 ) ;
  btnAdd.TabOrder := 0 ;
  btnAdd.OnClick := doAddClick ;
  oParentWindow.addResBmp( 'TGIS_LEGENDFORMIMAGE_ADD', btnAdd ) ;

  lstZones := TListBox.Create( gpbTransparency ) ;
  lstZones.Parent := gpbTransparency ;
  lstZones.Position.Y    := t ;
  lstZones.Height := 100 ;
  PlaceControl( bd, btnAdd, lstZones, 4, WIDTH_3COL * 3 ) ;
  lstZones.TabOrder := 3 ;
  lstZones.OnClick := lstZoneClick ;

  t := t + btnAdd.Height + 8 ;

  btnRemove := TButton.Create( gpbTransparency ) ;
  btnRemove.Parent := gpbTransparency ;
  btnRemove.Position.Y  := t ;
  btnRemove.Height := 25 ;
  PlaceControl( bd, nil, btnRemove, LEFT_3COL_1, 25 ) ;
  btnRemove.TabOrder := 1 ;
  btnRemove.OnClick := doDeleteClick ;
  oParentWindow.addResBmp( 'TGIS_LEGENDFORMIMAGE_DELETE', btnRemove ) ;

  t := t + btnRemove.Height + 8 ;

  btnClear := TButton.Create( gpbTransparency ) ;
  btnClear.Parent := gpbTransparency ;
  btnClear.Position.Y  := t ;
  btnClear.Height := 25 ;
  PlaceControl( bd, nil, btnClear, LEFT_3COL_1, 25 ) ;
  btnClear.TabOrder := 2 ;
  btnClear.OnClick := doClearClick ;
  oParentWindow.addResBmp( 'TGIS_LEGENDFORMIMAGE_CLEAR', btnClear ) ;

  gpbTransparency.Height := lstZones.Position.Y + lstZones.Height + GROUPBOX_BOTTOM_GAP ;

  t := gpbTransparency.Position.Y + gpbTransparency.Height + GROUPBOX_GAP ;

  btnReset := TButton.Create( Self ) ;
  btnReset.Parent := Self ;
  btnReset.Position.Y := t ;
  PlaceControl( bd, nil, btnReset, WIDTH_NORMAL - 88, 80 ) ;
  if bd = bdRightToLeft then
    btnReset.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    btnReset.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  btnReset.Text := _rsrcna( GIS_RS_BTN_RESET ) ;
  btnReset.OnClick := doReset ;

  ckbLegend := TCheckBox.Create( Self.Panel ) ;
  ckbLegend.Parent := Self.Panel ;
  ckbLegend.Position.Y := t ;
  PlaceControl( bd, nil, ckbLegend, LEFT_3COL_1, WIDTH_2COL ) ;
  if bd = bdRightToLeft then
    ckbLegend.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    ckbLegend.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  ckbLegend.Text := _rsrc( GIS_RS_LEGEND_PRM_INCLUDEINLEGEND ) ;

end ;

procedure T_panelPixel.PreparePreview(
  const _viewer : IGIS_Viewer
) ;
begin
  mvc.PreparePreview( _viewer ) ;
end;

procedure T_panelPixel.Read ;
var
  i   : Integer;
  itm : TListBoxItem ;
begin
  lockUpdates ;
  try
    ckbLegend.IsChecked := mvc.ParamsPixel.ShowLegend ;
    vvspeRed.Value := mvc.ParamsPixel.Red ;
    vvspeGreen.Value := mvc.ParamsPixel.Green ;
    vvspeBlue.Value := mvc.ParamsPixel.Blue ;
    vvspeBrightness.Value := mvc.ParamsPixel.Brightness ;
    vvspeContrast.Value := mvc.ParamsPixel.Contrast ;
    cmbRedBand.Items.BeginUpdate ;
    cmbRedBand.ItemIndex := mvc.ParamsPixel.RedBand + 1 ;
    cmbRedBand.Items.EndUpdate ;

    cmbGreenBand.Items.BeginUpdate ;
    cmbGreenBand.ItemIndex := mvc.ParamsPixel.GreenBand + 1 ;
    cmbGreenBand.Items.EndUpdate ;

    cmbBlueBand.Items.BeginUpdate ;
    cmbBlueBand.ItemIndex := mvc.ParamsPixel.BlueBand + 1 ;
    cmbBlueBand.Items.EndUpdate ;

    cmbAlphaBand.Items.BeginUpdate ;
    cmbAlphaBand.ItemIndex := mvc.ParamsPixel.AlphaBand + 1 ;
    cmbAlphaBand.Items.EndUpdate ;

    cmbPage.Items.BeginUpdate ;
    cmbPage.ItemIndex := mvc.ParamsPixel.Page - 1 ;
    cmbPage.Items.EndUpdate ;

    ckbInversion.IsChecked := mvc.ParamsPixel.Inversion ;
    ckbGrayscale.IsChecked := mvc.ParamsPixel.Grayscale ;
    ckbHistogram.IsChecked := mvc.ParamsPixel.Histogram ;
    ckbContrastEnhanced.IsChecked := mvc.ParamsPixel.ContrastEnhanced ;

    lstZones.Items.BeginUpdate ;
    lstZones.Items.Assign( mvc.ParamsPixel.TransparentZones ) ;

    for i := 0 to lstZones.Count-1 do begin
      itm := lstZones.ItemByIndex(i) ;
      itm.OnPaint := lstZoneDrawItem ;
      itm.StyleLookup := 'colorlistboxitemstyle' ;
    end ;

    if lstZones.Items.Count > 0 then
      lstZones.ItemIndex := 0 ;
    lstZones.Items.EndUpdate ;


    lstZoneClick( Self ) ;
  finally
    unlockUpdates ;
  end ;
end ;

procedure T_panelPixel.UpdatePreview;
var
  lp        : TGIS_LayerPixel ;
  paramspix : TGIS_ParamsSectionPixel ;
begin
  oParentWindow.tmrUpdate.Enabled := False ;
  if oParentWindow.gisPreview.IsEmpty then exit ;
  oParentWindow.tmrUpdate.Enabled := True  ;

  lp := TGIS_LayerPixel( oParentWindow.gisPreview.Items[0] ) ;

  if lp.Params.Pixel.Page <> mvc.ParamsPixel.Page then begin
    // recreate preview
    mvc.PreparePreview( oParentWindow.gisPreview ) ;
    lp := TGIS_LayerPixel( oParentWindow.gisPreview.Items[0] ) ;
  end;

  paramspix := lp.Params ;
  mvc.SectionWrite( paramspix ) ;

  with lp.Params do begin
    MinZoom  := 0              ;
    MaxZoom  := GIS_MAX_DOUBLE ;
    MinScale := 0              ;
    MaxScale := GIS_MAX_DOUBLE ;
  end ;
end;

procedure T_panelPixel.Write ;
begin
  mvc.ParamsPixel.ShowLegend := ckbLegend.IsChecked ;
  mvc.ParamsPixel.Red        := FloorS( vvspeRed.Value ) ;
  mvc.ParamsPixel.Green      := FloorS( vvspeGreen.Value ) ;
  mvc.ParamsPixel.Blue       := FloorS( vvspeBlue.Value ) ;
  mvc.ParamsPixel.Brightness := FloorS( vvspeBrightness.Value ) ;
  mvc.ParamsPixel.Contrast   := FloorS( vvspeContrast.Value ) ;
  mvc.ParamsPixel.RedBand    := cmbRedBand.ItemIndex - 1 ;
  mvc.ParamsPixel.GreenBand  := cmbGreenBand.ItemIndex - 1 ;
  mvc.ParamsPixel.BlueBand   := cmbBlueBand.ItemIndex - 1 ;
  mvc.ParamsPixel.AlphaBand  := cmbAlphaBand.ItemIndex - 1 ;
  mvc.ParamsPixel.Page       := cmbPage.ItemIndex + 1 ;
  mvc.ParamsPixel.Inversion  := ckbInversion.IsChecked ;
  mvc.ParamsPixel.Grayscale  := ckbGrayscale.IsChecked ;
  mvc.ParamsPixel.Histogram  := ckbHistogram.IsChecked ;
  mvc.ParamsPixel.ContrastEnhanced  := ckbContrastEnhanced.IsChecked ;
  mvc.ParamsPixel.TransparentZones.Assign( lstZones.Items ) ;
end ;

procedure T_panelPixel.doCallback(
  _sender : TObject ;
  _code   : Integer
) ;

  procedure do_reset ;
  var
    idx : Integer ;
  begin
    idx := oParentWindow.MVC.SectionIndex ;
    oParentWindow.MVC.SectionIndex := -1 ;
    oParentWindow.MVC.Pixel.SectionRead( False ) ;
    Read ;

    oParentWindow.MVC.SectionIndex := idx ;
  end ;

begin
  case _code of
    1 : do_reset ;
    2 : doUpdate( _sender ) ;
  end ;

  if not blockedUpdates then begin
    Write ;
    UpdatePreview ;
  end ;

end ;

procedure T_panelPixel.doReset(
  _sender : TObject
) ;
begin
  mvc.DoReset ;
end ;

procedure T_panelPixel.doControlChange(
  _sender: TObject
) ;
begin
  mvc.DoControlChange ;
end ;

procedure T_panelPixel.lstZoneDrawItem(
  _sender : TObject ;
  _canvas : TCanvas ;
  const _rect   : TRectF
) ;
var
  r     : TRect ;
  tkn   : TGIS_Tokenizer ;
  sfrom : String ;
  sto   : String ;
  o     : TFmxObject ;
  orect : TRectangle ;
  itm   : TListBoxItem ;
  otext : TText      ;
begin
  sfrom := '' ;
  sto   := '' ;

  itm := TListBoxItem(_sender) ;

  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.ExecuteEx( itm.Text ) ;

    if tkn.Result.Count > 0 then
      sfrom := tkn.Result[0] ;

    if tkn.Result.Count > 1 then
      sto := tkn.Result[1] ;
  finally
    FreeObject( tkn ) ;
  end ;

  o := itm.FindStyleResource( 'text' ) ;

  if not ( o is TText ) then exit ;

  otext := TText( o ) ;
  otext.Text := sfrom + '                   ' + sto ;

  o := itm.FindStyleResource( 'color' ) ;
  Assert( o is TRectangle ) ;
  orect := TRectangle( o  ) ;
  orect.Visible := False ;
end ;

procedure T_panelPixel.lstZoneClick(
  _sender : TObject
) ;
var
  tkn : TGIS_Tokenizer ;
  cl  : TGIS_Color ;

  function fixColor( const _strColor : String ) : TGIS_Color ;
  begin
    Result := ParamColor( _strColor, TGIS_Color.DimGray ) ;
  end ;

begin
  if lstZones.ItemIndex < 0 then exit ;

  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.ExecuteEx( lstZones.Items[ lstZones.ItemIndex ] ) ;

    if tkn.Result.Count > 0 then
       pnlColorFrom.Color := fixColor( tkn.Result[0] )
    else
       pnlColorFrom.Color := TGIS_Color.White ;

    if tkn.Result.Count > 1 then
       pnlColorTo.Color := fixColor( tkn.Result[1] )
    else
       pnlColorTo.Color := TGIS_Color.White ;
  finally
    FreeObject( tkn ) ;
  end ;
end ;

procedure T_panelPixel.doAddClick(
  _sender : TObject
) ;
begin
  lstZones.Items.BeginUpdate ;
  if lstZones.ItemIndex >= 0 then begin
    lstZones.Items.Insert( lstZones.ItemIndex, lstZones.Items[ lstZones.ItemIndex ] ) ;
  end
  else begin
    lstZones.Items.Add( '' ) ;
    lstZones.ItemIndex := 0 ;
  end ;
  lstZones.Items.EndUpdate ;

  doUpdate( _sender ) ;
  lstZoneClick( _sender ) ;
  mvc.DoTransparentZonesChange ;
end ;

procedure T_panelPixel.doDeleteClick(
  _sender : TObject
) ;
var
  idx : Integer ;
begin
  idx := lstZones.ItemIndex ;

  if idx >= 0 then
    lstZones.Items.Delete( idx ) ;

  doUpdate( _sender ) ;
  lstZones.ItemIndex := Min( idx, lstZones.Items.Count - 1 ) ;
  lstZoneClick( _sender ) ;
  mvc.DoTransparentZonesChange ;
end ;

procedure T_panelPixel.doClearClick(
  _sender : TObject
) ;
begin
  lstZones.Clear ;
  doUpdate( _sender ) ;
  mvc.DoTransparentZonesChange ;
end ;

procedure T_panelPixel.doUpdate(
  _sender : TObject
) ;
var
  stmp  : String ;
  i     : Integer ;
  itm   : TListBoxItem ;
begin
  if lstZones.ItemIndex >= 0 then begin
      stmp := '$'+IntToHex( pnlColorFrom.Color.ToABGR, 2 ) + ',' +
              '$'+IntToHex( pnlColorTo.Color.ToABGR, 2 )  ;

    lstZones.Items[ lstZones.ItemIndex ] := stmp ;

    for i := 0 to lstZones.Count-1 do begin
      itm := lstZones.ItemByIndex(i) ;
      itm.OnPaint := lstZoneDrawItem ;
      itm.StyleLookup := 'colorlistboxitemstyle' ;
    end ;
  end ;

end ;

procedure T_panelPixel.doZonesChange(
  _sender : TObject
) ;
begin
  mvc.DoTransparentZonesChange ;
end;

{$ENDREGION}

{$REGION 'T_panelGrid'}
function T_panelGrid.fget_HasPreview : Boolean ;
begin
  Result := mvc.HasPreview ;
end ;

procedure T_panelGrid.init ;
begin
  inherited ;

  ItemText := _rsrcna( GIS_RS_LEGEND_PAG_GRID ) ;

  mvc := oParentWindow.MVC.Pixel ;
  mvc.Callback := doCallback ;

  updateIndex := -1 ;
  initSelf ;
end ;

procedure T_panelGrid.initSelf ;
var
  t   : Single ;
  i : Integer ;
  bd : TBiDiMode ;
begin
  bd := oParentWindow.BiDiMode ;

  gpbGridd := TGroupBox.Create( Self.Panel ) ;
  gpbGridd.Parent := Self.Panel ;
  gpbGridd.Position.Y := GROUPBOX_TOP ;
  gpbGridd.Height := 512 ;
  PlaceControl( bd, nil, gpbGridd, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbGridd.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbGridd.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbGridd.Text := _rsrcna( GIS_RS_LEGEND_PAG_GRID ) ;

  t := GROUPBOX_TOP_GAP ;

  lblGridBand := TLabel.Create( gpbGridd ) ;
  lblGridBand.Parent := gpbGridd ;
  lblGridBand.Position.Y := t ;
  PlaceControl( bd, nil, lblGridBand, LEFT_3COL_1, WIDTH_3COL ) ;
  lblGridBand.Text := _rsrcna( GIS_RS_LEGEND_PRM_GRIDBAND ) ;
  lblGridBand.FixSize;

  t := t + lblGridBand.Height + 4 ;

  cmbGridBand := TComboBox.Create( gpbGridd ) ;
  cmbGridBand.Parent := gpbGridd ;
  cmbGridBand.Position.Y := t ;
  PlaceControl( bd, nil, cmbGridBand, LEFT_3COL_1, WIDTH_3COL ) ;
  cmbGridBand.Items.BeginUpdate ;
  cmbGridBand.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_DEFAULT ) ) ;
  for i := 0 to TGIS_LayerPixel(oParentWindow.MVC.Layer).BandsCount -1 do
    cmbGridBand.Items.Add( IntToStr( i+1 ) ) ;

  cmbGridBand.ItemIndex := 0 ;
  cmbGridBand.Items.EndUpdate ;
  cmbGridBand.OnChange := doControlChange;

  ckbShadow := TCheckBox.Create( gpbGridd ) ;
  ckbShadow.Parent := gpbGridd ;
  ckbShadow.Position.Y := t ;
  PlaceControl( bd, nil, ckbShadow, LEFT_3COL_2, WIDTH_3COL ) ;
  ckbShadow.Text := _rsrcna( GIS_RS_LEGEND_PRM_SHADOW ) ;
  ckbShadow.OnChange := doControlChange;

  ckbAntialias := TCheckBox.Create( gpbGridd ) ;
  ckbAntialias.Parent := gpbGridd ;
  ckbAntialias.Position.Y := t ;
  PlaceControl( bd, nil, ckbAntialias, LEFT_3COL_3-20, WIDTH_2COL ) ;
  ckbAntialias.Text := _rsrcna( GIS_RS_LEGEND_PRM_ALIASING ) ;
  ckbAntialias.OnChange := doControlChange;

  t := t + ckbAntialias.Height + 4 ;

  lblHeightMin := TLabel.Create( gpbGridd ) ;
  lblHeightMin.Parent := gpbGridd ;
  lblHeightMin.Position.Y := t ;
  PlaceControl( bd, nil, lblHeightMin, LEFT_3COL_1, lblHeightMin.Width ) ;
  lblHeightMin.Text := _rsrc( GIS_RS_LEGEND_PRM_MINVAL ) ;

  lblHeightMax := TLabel.Create( gpbGridd ) ;
  lblHeightMax.Parent := gpbGridd ;
  lblHeightMax.Position.Y := t ;
  PlaceControl( bd, nil, lblHeightMax, LEFT_3COL_2, lblHeightMax.Width ) ;
  lblHeightMax.Text := _rsrc( GIS_RS_LEGEND_PRM_MAXVAL ) ;

  t := t + lblHeightMax.Height + 4 ;

  edtHeightMin := TEdit.Create( gpbGridd ) ;
  edtHeightMin.Parent := gpbGridd ;
  edtHeightMin.Position.Y := t ;
  PlaceControl( bd, nil, edtHeightMin, LEFT_3COL_1, WIDTH_3COL ) ;
  edtHeightMin.OnChange := doControlChange ;

  vvedtHeightMin := TGIS_ValueValidatorEditHelper.Create( edtHeightMin ) ;
  vvedtHeightMin.MinVal := -GIS_MAX_SINGLE ;
  vvedtHeightMin.MaxVal := GIS_MAX_SINGLE ;
  vvedtHeightMin.Precision := 15 ;

  edtHeightMax := TEdit.Create( gpbGridd ) ;
  edtHeightMax.Parent := gpbGridd ;
  edtHeightMax.Position.Y := t ;
  PlaceControl( bd, nil, edtHeightMax, LEFT_3COL_2, WIDTH_3COL ) ;
  edtHeightMax.OnChange := doControlChange ;

  vvedtHeightMax := TGIS_ValueValidatorEditHelper.Create( edtHeightMax ) ;
  vvedtHeightMax.MinVal := -GIS_MAX_SINGLE ;
  vvedtHeightMax.MaxVal := GIS_MAX_SINGLE ;
  vvedtHeightMax.Precision := 15 ;

  t := t + edtHeightMax.Height + 16 ;

  gpbGridd.Height := t ;

  gpbThreshold := TGroupBox.Create( Self.Panel ) ;
  gpbThreshold.Parent := Self.Panel ;
  gpbThreshold.Position.Y  := t + GROUPBOX_GAP ;
  gpbThreshold.Height := 512 ;
  PlaceControl( bd, nil, gpbThreshold, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbThreshold.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbThreshold.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbThreshold.Text := _rsrcna( GIS_RS_LEGEND_PRM_HEIGHT_THRESHOLD ) ;

  t := GROUPBOX_TOP_GAP ;

  lblThresholdMin := TLabel.Create( gpbThreshold ) ;
  lblThresholdMin.Parent := gpbThreshold ;
  lblThresholdMin.Position.Y := t ;
  PlaceControl( bd, nil, lblThresholdMin, LEFT_3COL_1, WIDTH_3COL ) ;
  lblThresholdMin.Text := _rsrcna( GIS_RS_LEGEND_PRM_MINVAL ) ;
  lblThresholdMin.FixSize ;

  lblThresholdMax := TLabel.Create( gpbThreshold ) ;
  lblThresholdMax.Parent := gpbThreshold ;
  lblThresholdMax.Position.Y := t ;
  PlaceControl( bd, nil, lblThresholdMax, LEFT_3COL_2, WIDTH_3COL ) ;
  lblThresholdMax.Text := _rsrcna( GIS_RS_LEGEND_PRM_MAXVAL ) ;
  lblThresholdMax.FixSize ;

  t := t + lblThresholdMax.Height + 4 ;

  edtThresholdMin := TEdit.Create( gpbThreshold ) ;
  edtThresholdMin.Parent := gpbThreshold ;
  edtThresholdMin.Position.Y := t ;
  PlaceControl( bd, nil, edtThresholdMin, LEFT_3COL_1, WIDTH_3COL ) ;
  edtThresholdMin.OnChange := doControlChange ;
  edtThresholdMin.FixSize ;
  edtThresholdMin.KillFocusByReturn := True ;

  vvedtThresholdMin := TGIS_ValueValidatorEditHelper.Create( edtThresholdMin ) ;
  vvedtThresholdMin.MinVal := -GIS_MAX_SINGLE ;
  vvedtThresholdMin.MaxVal := GIS_MAX_SINGLE ;
  vvedtThresholdMin.Precision := 15 ;

  edtThresholdMax := TEdit.Create( gpbThreshold ) ;
  edtThresholdMax.Parent := gpbThreshold ;
  edtThresholdMax.Position.Y := t ;
  PlaceControl( bd, nil, edtThresholdMax, LEFT_3COL_2, WIDTH_3COL ) ;
  edtThresholdMax.OnChange := doControlChange ;
  edtThresholdMax.KillFocusByReturn := True ;

  vvedtThresholdMax := TGIS_ValueValidatorEditHelper.Create( edtThresholdMax ) ;
  vvedtThresholdMax.MinVal := -GIS_MAX_SINGLE ;
  vvedtThresholdMax.MaxVal := GIS_MAX_SINGLE ;
  vvedtThresholdMax.Precision := 15 ;

  gpbThreshold.Height := t + edtThresholdMax.Height + 16 ;

  gpbRamp := TGroupBox.Create( Self.Panel ) ;
  gpbRamp.Parent := Self.Panel ;
  gpbRamp.Position.Y := gpbThreshold.Position.Y + gpbThreshold.Height + GROUPBOX_GAP ;
  gpbRamp.Height := 512 ;
  PlaceControl( bd, nil, gpbRamp, GROUPBOX_LEFT, WIDTH_NORMAL ) ;
  if bd = bdRightToLeft then
    gpbRamp.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    gpbRamp.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  gpbRamp.Text := _rsrcna( GIS_RS_LEGEND_PAG_RAMP ) ;

  t := GROUPBOX_TOP_GAP ;

  lblMin := TLabel.Create( gpbRamp ) ;
  lblMin.Parent := gpbRamp ;
  lblMin.Position.Y := t ;
  lblMin.Position.X := LEFT_3COL_1 + 23 + 8 + 32 ;
  lblMin.Width := 64 + 16 ;
  lblMin.Text := _rsrcna( GIS_RS_LEGEND_PRM_MIN ) ;
  lblMin.FixSize ;

  lblMax := TLabel.Create( gpbRamp ) ;
  lblMax.Parent := gpbRamp ;
  lblMax.Position.Y := t ;
  lblMax.Position.X := LEFT_3COL_1 + 23 + 8 + 32 + 64 + 16 ;
  lblMax.Width := 64 + 16 ;
  lblMax.Text := _rsrcna( GIS_RS_LEGEND_PRM_MAX ) ;
  lblMax.FixSize ;

  lblLegend := TLabel.Create( gpbRamp ) ;
  lblLegend.Parent := gpbRamp ;
  lblLegend.Position.Y := t ;
  lblLegend.Position.X := LEFT_3COL_1 + 23 + 8 + 32 + 64 + 64 + 16 + 16 ;
  lblLegend.Width := 128 ;
  lblLegend.Text := _rsrcna( GIS_RS_LEGEND_PRM_LEGEND ) ;
  lblLegend.FixSize ;

  t := t + lblMin.Height + 4 ;

  pnlColor := TGIS_ColorPreview.Create( gpbRamp ) ;
  pnlColor.Parent := gpbRamp ;
  pnlColor.Position.Y := t ;
  pnlColor.Height := 21 ;
  pnlColor.Position.X := LEFT_3COL_1 + 23 + 8 - 2 ;
  pnlColor.Width := 32 ;
  pnlColor.CellSize := 5 ;
  pnlColor.OnDialogChange := doControlChange ;

  edtMin := TEdit.Create( gpbRamp ) ;
  edtMin.Parent := gpbRamp ;
  edtMin.Position.Y := t ;
  edtMin.Position.X := pnlColor.Position.X + pnlColor.Width + 1 ;
  edtMin.Width := 64 + 16 ;
  edtMin.OnChange := doControlChange ;
  edtMin.FixSize ;
  edtMin.TabOrder := 0 ;
  edtMin.KillFocusByReturn := True ;

  vvedtMin := TGIS_ValueValidatorEditHelper.Create( edtMin ) ;
  vvedtMin.MinVal := -GIS_MAX_SINGLE ;
  vvedtMin.MaxVal := GIS_MAX_SINGLE ;
  vvedtMin.Precision := 15 ;

  edtMax := TEdit.Create( gpbRamp ) ;
  edtMax.Parent := gpbRamp ;
  edtMax.Position.Y := t ;
  edtMax.Position.X := edtMin.Position.X + edtMin.Width + 1 ;
  edtMax.Width := 64 + 16 ;
  edtMax.OnChange := doControlChange ;
  edtMax.FixSize ;
  edtMax.TabOrder := 1 ;
  edtMax.KillFocusByReturn := True ;

  vvedtMax := TGIS_ValueValidatorEditHelper.Create( edtMax ) ;
  vvedtMax.MinVal := -GIS_MAX_SINGLE ;
  vvedtMax.MaxVal := GIS_MAX_SINGLE ;
  vvedtMax.Precision := 15 ;

  edtLegend := TEdit.Create( gpbRamp ) ;
  edtLegend.Parent := gpbRamp ;
  edtLegend.Position.Y := t ;
  edtLegend.Position.X := edtMax.Position.X + edtMax.Width + 1 ;
  edtLegend.Width := 128 ;
  edtLegend.OnChange := doControlChange ;
  edtLegend.FixSize ;
  edtLegend.TabOrder := 2 ;
  edtLegend.KillFocusByReturn := True ;

  t := t + pnlColor.Height + 4 ;

  btnAdd := TButton.Create( gpbRamp ) ;
  btnAdd.Parent := gpbRamp ;
  btnAdd.Position.Y := t ;
  btnAdd.Height := 25 ;
  PlaceControl( bd, nil, btnAdd, LEFT_3COL_1, 25 ) ;
  btnAdd.TabOrder := 3 ;
  btnAdd.OnClick := doAddClick ;
  oParentWindow.addResBmp( 'TGIS_LEGENDFORMIMAGE_ADD', btnAdd ) ;

  lstGrid := TListBox.Create( gpbRamp ) ;
  lstGrid.Parent := gpbRamp ;
  lstGrid.Position.Y := t ;
  lstGrid.Height := 110 ;
  PlaceControl( bd, btnAdd, lstGrid, 4,
                pnlColor.Width + edtMin.Width +
                edtMax.Width + edtLegend.Width + 3 ) ;
  lstGrid.ItemHeight := 24 ;
  lstGrid.TabOrder := 6 ;
  lstGrid.OnClick := lstGridClick ;

  t := t + btnAdd.Height + 8 ;

  btnRemove := TButton.Create( gpbRamp ) ;
  btnRemove.Parent := gpbRamp ;
  btnRemove.Position.Y  := t ;
  btnRemove.Height := 25 ;
  PlaceControl( bd, nil, btnRemove, LEFT_3COL_1, 25 ) ;
  btnRemove.TabOrder := 4 ;
  btnRemove.OnClick := doDeleteClick ;
  oParentWindow.addResBmp( 'TGIS_LEGENDFORMIMAGE_DELETE', btnRemove ) ;

  t := t + btnRemove.Height + 8 ;

  btnClear := TButton.Create( gpbRamp ) ;
  btnClear.Parent := gpbRamp ;
  btnClear.Position.Y  := t ;
  btnClear.Height := 25 ;
  PlaceControl( bd, nil, btnClear, LEFT_3COL_1, 25 ) ;
  btnClear.TabOrder := 5 ;
  btnClear.OnClick := doClearClick ;
  oParentWindow.addResBmp( 'TGIS_LEGENDFORMIMAGE_CLEAR', btnClear ) ;

  t := lstGrid.Position.Y + lstGrid.Height + 8 ;

  ckbGridSmoothColors := TCheckBox.Create( gpbRamp ) ;
  ckbGridSmoothColors.Parent := gpbRamp ;
  ckbGridSmoothColors.Position.Y := t ;
  PlaceControl( bd, nil, ckbGridSmoothColors, lstGrid.Position.X, WIDTH_2COL ) ;
  ckbGridSmoothColors.Text := _rsrc( GIS_RS_LEGEND_PRM_GRIDSMOOTHCOLOR ) ;
  ckbGridSmoothColors.OnChange := doControlChange;

  gpbRamp.Height := t + ckbGridSmoothColors.Height + GROUPBOX_BOTTOM_GAP ;

  if bd = bdRightToLeft then begin
    pnlColor.Position.X := lstGrid.Position.X ;
    edtMin.Position.X := pnlColor.Position.X + pnlColor.Width + 1 ;
    edtMax.Position.X := edtMin.Position.X + edtMin.Width + 1 ;
    edtLegend.Position.X := edtMax.Position.X + edtMax.Width + 1 ;
    lblMin.Position.X := edtMin.Position.X ;
    lblMax.Position.X := edtMax.Position.X ;
    lblLegend.Position.X := edtLegend.Position.X ;
  end ;

  ckbLegend := TCheckBox.Create( Self.Panel ) ;
  ckbLegend.Parent := Self.Panel ;
  ckbLegend.Position.Y := gpbRamp.Position.Y + gpbRamp.Height + 4 ;
  PlaceControl( bd, nil, ckbLegend, LEFT_3COL_1, WIDTH_2COL ) ;
  if bd = bdRightToLeft then
    ckbLegend.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
  else
    ckbLegend.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
  ckbLegend.Text := _rsrc( GIS_RS_LEGEND_PRM_INCLUDEINLEGEND ) ;

end ;

procedure T_panelGrid.PreparePreview(
  const _viewer : IGIS_Viewer
) ;
begin
  mvc.PreparePreview( _viewer ) ;
end;

procedure T_panelGrid.UpdatePreview;
var
  lp        : TGIS_LayerPixel ;
  paramspix : TGIS_ParamsSectionPixel ;
begin
  oParentWindow.tmrUpdate.Enabled := False ;
  if oParentWindow.gisPreview.IsEmpty then exit ;
  oParentWindow.tmrUpdate.Enabled := True  ;

  lp := TGIS_LayerPixel( oParentWindow.gisPreview.Items[0] ) ;

  if ( lp.Antialias         <> mvc.ParamsPixel.Antialias    ) or
     ( lp.Params.Pixel.Page <> mvc.ParamsPixel.Page         ) or
     ( lp.Params.Pixel.GridBand <> mvc.ParamsPixel.GridBand )
  then begin
    // recreate preview
    mvc.PreparePreview( oParentWindow.gisPreview ) ;
    lp := TGIS_LayerPixel( oParentWindow.gisPreview.Items[0] ) ;
  end;

  paramspix := lp.Params ;
  mvc.SectionWrite( paramspix ) ;
  lp.Params.Pixel.Antialias := mvc.ParamsPixel.Antialias ;

  with lp.Params do begin
    MinZoom  := 0              ;
    MaxZoom  := GIS_MAX_DOUBLE ;
    MinScale := 0              ;
    MaxScale := GIS_MAX_DOUBLE ;
  end ;
end;

procedure T_panelGrid.Read ;
var
  i   : Integer;
  itm : TListBoxItem ;
  function _sameValue(
    const _a, _b : Single
  ) : Boolean ;
  var
    epsilon : Single ;
  begin
    epsilon := Max( Min( Abs(_a), Abs(_b) ) * 1e-7, 1e-7 ) ;
    if _a > _b then
      Result := (_a - _b) <= epsilon
    else
      Result := (_b - _a) <= epsilon ;
  end ;
begin
  lockUpdates ;
  try
    lstGrid.Items.BeginUpdate ;
    lstGrid.Items.Assign( mvc.ParamsPixel.AltitudeMapZones ) ;

    for i := 0 to lstGrid.Count-1 do begin
      itm := lstGrid.ItemByIndex(i) ;
      itm.OnPaint := lstGridDrawItem ;
      itm.StyleLookup := 'colorlistboxitemstyle' ;
    end ;

    if lstGrid.Items.Count > 0 then
      lstGrid.ItemIndex := 0 ;

    lstGrid.Items.EndUpdate ;
    lstGridClick( Self ) ;

    ckbShadow.IsChecked := mvc.ParamsPixel.GridShadow ;
    ckbAntialias.IsChecked := mvc.ParamsPixel.Antialias ;
    ckbLegend.IsChecked := mvc.ParamsPixel.ShowLegend ;
    ckbGridSmoothColors.IsChecked := mvc.ParamsPixel.GridSmoothColors ;

    if _sameValue( mvc.ParamsPixel.MinHeightThreshold, -GIS_MAX_SINGLE ) and
       _sameValue( mvc.ParamsPixel.MaxHeightThreshold, GIS_MAX_SINGLE  ) then
    begin
      vvedtThresholdMin.Value := mvc.MinHeight ;
      vvedtThresholdMax.Value := mvc.MaxHeight ;
      vvedtHeightMin.Value    := mvc.MinHeight ;
      vvedtHeightMax.Value    := mvc.MaxHeight ;
    end
    else begin
      vvedtThresholdMin.Value := mvc.ParamsPixel.MinHeightThreshold ;
      vvedtThresholdMax.Value := mvc.ParamsPixel.MaxHeightThreshold ;
      vvedtHeightMin.Value    := oParentWindow.MVC.General.HeightMin ;
      vvedtHeightMax.Value    := oParentWindow.MVC.General.HeightMax ;
    end ;

    cmbGridBand.Items.BeginUpdate ;
    cmbGridBand.ItemIndex := mvc.ParamsPixel.GridBand ;
    cmbGridBand.Items.EndUpdate ;
  finally
    unlockUpdates ;
  end ;
end ;

procedure T_panelGrid.Write ;
begin
  mvc.ParamsPixel.AltitudeMapZones.Assign( lstGrid.Items ) ;
  mvc.ParamsPixel.ShowLegend         := ckbLegend.IsChecked ;
  mvc.ParamsPixel.GridShadow         := ckbShadow.IsChecked ;
  mvc.ParamsPixel.GridBand           := cmbGridBand.ItemIndex   ;
  mvc.ParamsPixel.MinHeightThreshold := vvedtThresholdMin.Value ;
  mvc.ParamsPixel.MaxHeightThreshold := vvedtThresholdMax.Value ;
  mvc.ParamsPixel.Antialias          := ckbAntialias.IsChecked  ;
  mvc.ParamsPixel.GridSmoothColors   := ckbGridSmoothColors.IsChecked ;

  oParentWindow.MVC.General.HeightMin := vvedtHeightMin.Value ;
  oParentWindow.MVC.General.HeightMax := vvedtHeightMax.Value ;
end ;

procedure T_panelGrid.lstGridDrawItem(
  _sender : TObject ;
  _canvas : TCanvas ;
  const _rect   : TRectF
) ;

var
  r    : TRectF ;
  tkn  : TGIS_Tokenizer ;
  cl   : Cardinal ;
  dmin : Double ;
  dmax : Double ;
  sleg : String ;
  o : TFmxObject ;
  orect  : TRectangle ;
  itm : TListBoxItem ;
  otext  : TText      ;
begin
  cl   := TAlphaColorRec.White ;
  dmin := 0       ;
  dmax := 0       ;
  sleg := ''      ;

  itm := TListBoxItem(_sender) ;

  tkn := TGIS_Tokenizer.Create ;
  try
    tkn.ExecuteEx( itm.Text ) ;

    if tkn.Result.Count > 0 then
      dmin := DotStrToFloat( tkn.Result[0] ) ;
    if tkn.Result.Count > 1 then
      dmax := DotStrToFloat( tkn.Result[1] ) ;
    if tkn.Result.Count > 2 then
      cl := ParamColor( tkn.Result[2], TGIS_Color.White ).ARGB ;
    if tkn.Result.Count > 3 then
      sleg := tkn.Result[3] ;
  finally
    FreeObject( tkn ) ;
  end ;

  o := itm.FindStyleResource( 'text' ) ;

  if not ( o is TText ) then exit ;

  otext := TText( o ) ;
  otext.Text := DotFloatToStr( dmin ) + '  ' + DotFloatToStr( dmax ) + '  ' + sleg ;

  o := itm.FindStyleResource( 'color' ) ;
  Assert( o is TRectangle ) ;
  orect := TRectangle( o  ) ;
  orect.Width := 24 ;
  orect.Fill.Kind := TBrushKind.Solid ;
  orect.Fill.Color := cl ;
  orect.Stroke.Color := cl ;
end ;

procedure T_panelGrid.lstGridClick(
  _sender : TObject
) ;
var
  tkn : TGIS_Tokenizer ;
begin
  if lstGrid.ItemIndex < 0 then begin
    vvedtMin.Value := 0 ;
    vvedtMax.Value := 0 ;
    edtMin.Text := '' ;
    edtMax.Text := '' ;
    pnlColor.Color := TGIS_Color.White ;
    edtLegend.Text := '' ;
    exit ;
  end;

  updateIndex := lstGrid.ItemIndex ;
  lockUpdates ;
  try
    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.ExecuteEx( lstGrid.Items[ lstGrid.ItemIndex ] ) ;

      if tkn.Result.Count > 0 then
         edtMin.Text := tkn.Result[0]
      else
         edtMin.Text := '0' ;

      if tkn.Result.Count > 1 then
         edtMax.Text := tkn.Result[1]
      else
         edtMax.Text := '0' ;

      if tkn.Result.Count > 2 then
        pnlColor.Color :=  ParamColor( tkn.Result[2], TGIS_Color.White )
      else
        pnlColor.Color := TGIS_Color.White ;

      if tkn.Result.Count > 3 then
         edtLegend.Text := tkn.Result[3]
      else
         edtLegend.Text := '' ;
    finally
      FreeObject( tkn ) ;
    end ;
  finally
    unlockUpdates ;
  end ;
end ;

procedure T_panelGrid.doAddClick(
  _sender : TObject
) ;
begin
  lstGrid.Items.BeginUpdate ;
  if lstGrid.ItemIndex >= 0 then begin
    updateIndex := lstGrid.ItemIndex ;
    lstGrid.Items.Insert( lstGrid.ItemIndex, lstGrid.Items[ lstGrid.ItemIndex ] ) ;
  end
  else begin
    lstGrid.Items.Add( '' ) ;
    lstGrid.ItemIndex := 0 ;
    updateIndex := lstGrid.ItemIndex ;
  end ;
  lstGrid.Items.EndUpdate ;
  doUpdate( _sender ) ;
  lstGridClick( _sender ) ;
  doControlChange( _sender ) ;
end ;

procedure T_panelGrid.doDeleteClick(
  _sender : TObject
) ;
var
  idx : Integer ;
begin
  idx := lstGrid.ItemIndex ;

  if idx >= 0 then
    lstGrid.Items.Delete( idx ) ;

  doUpdate( _sender ) ;
  lstGrid.ItemIndex := Min( idx, lstGrid.Items.Count - 1 ) ;
  lstGridClick( _sender ) ;
  doControlChange( _sender ) ;
end ;

procedure T_panelGrid.doClearClick(
  _sender : TObject
) ;
begin
  lstGrid.Clear ;
  doUpdate( _sender ) ;
  doControlChange( _sender ) ;
end ;

procedure T_panelGrid.doControlChange(
  _sender: TObject
) ;
begin
  if not blockedUpdates then
    mvc.DoControlChange ;
end ;

procedure T_panelGrid.doUpdate(
  _sender : TObject
) ;
var
  stmp : String ;
  itm  : TListBoxItem ;
begin
  if lstGrid.ItemIndex >= 0 then begin
      stmp := DotFloatToStr( vvedtMin.Value )+ ',' +
              DotFloatToStr( vvedtMax.Value ) + ',' +
              ConstructParamColor( pnlColor.Color ) ;

    if not IsStringEmpty( Trim( edtLegend.Text ) ) then
      stmp := stmp + ',' + Trim( edtLegend.Text ) ;
    lstGrid.Items[ lstGrid.ItemIndex ] := stmp ;

    if updateIndex>-1 then begin
      itm := lstGrid.ItemByIndex(updateIndex) ;
      if not assigned( itm.OnPaint ) then begin
        itm.OnPaint := lstGridDrawItem ;
        itm.StyleLookup := 'colorlistboxitemstyle' ;
      end ;
      itm.Repaint ;
    end ;
  end ;

end ;

procedure T_panelGrid.doCallback(
  _sender : TObject ;
  _code   : Integer
) ;
begin
  case _code of
    0 : doUpdate( _sender ) ;
  end;

  if not blockedUpdates then begin
    Write ;
    UpdatePreview ;
  end ;
end ;

{$ENDREGION}

{$REGION 'TGIS_ControlLegendForm'}
destructor TGIS_ControlLegendForm.Destroy ;
begin
  FreeObject( gisPreview  ) ;
  FreeObject( lstFeatures ) ;
  FreeObject( MVC         ) ;

  inherited ;
end ;

procedure TGIS_ControlLegendForm.doGlobalProc(
  _modal_result : TModalResult
) ;
begin
  {$IFDEF GIS_MOBILE_DIALOGS}
    if _modal_result = mrOK then begin
      waitNoBusy ;
      T_panelGeneral( pnlGeneral ).Write ;
      T_sectionContainer( lstFeatures ).Write ;
      MVC.Write ;

      if Assigned( oViewerWnd ) then
        oViewerWnd.InvalidateWholeMap ;
    end ;
  {$ENDIF}

  if Assigned( globalProc ) then
    globalProc( _modal_result ) ;
end ;

procedure TGIS_ControlLegendForm.Execute(
  const _layer  : TGIS_Layer         ;
  const _legend : TGIS_ControlLegend ;
  const _onhelp : TGIS_HelpEvent     ;
  const _proc   : TProc<TModalResult>
) ;
begin
  if not Assigned( _layer ) then begin
    _proc( mrNone ) ;
    exit ;
  end;

  Assert( Assigned( _layer.Viewer ) ) ;

  oLegend    := _legend ;
  oViewer    := _layer.Viewer.Ref ;
  oViewerWnd := TGIS_ViewerWnd( _layer.Viewer.Ref.ViewerParent ) ;

  pOnHelp := _onhelp ;
  {$IFNDEF GIS_MOBILE_DIALOGS}
  btnHelp.Visible := assigned( pOnHelp ) ;
  {$ENDIF}

  globalProc := _proc ;

  MVC := TGIS_ControlLegendFormMVC.Create( _layer ) ;
  if MVC.IsVector then
    Self.Caption := Format( _rsrcna( GIS_RS_LEGEND_DLGVECTOR ),
                           [ MVC.Layer.Caption ]
                          )
  else if MVC.IsPixel then
    Self.Caption := Format( _rsrcna( GIS_RS_LEGEND_DLGPIXEL ),
                           [ MVC.Layer.Caption ]
                          )
  else
    Self.Caption := Format( _rsrcna( GIS_RS_LEGEND_OTHER ),
                           [ MVC.Layer.Caption ]
                          ) ;

  initLeft ;
  initRight( False ) ;
  tmrShowForm.Enabled := True ;

  ActiveControl := btnCancel ;

  ShowModalEx( doGlobalProc ) ;
end ;

procedure TGIS_ControlLegendForm.Execute(
  const _layer  : TGIS_Layer         ;
  const _legend : TGIS_ControlLegend ;
  const _proc   : TProc<TModalResult>
) ;
var
  hlp : TGIS_HelpEvent ;
begin
  if assigned( _layer ) and assigned (_layer.Viewer ) then
    hlp := _layer.Viewer.Ref.AssignedHelpEvent()
  else
    hlp := nil ;
  Execute( _layer, _legend, hlp, _proc ) ;
end ;

procedure TGIS_ControlLegendForm.readLayer(
  const _full : Boolean
)  ;
var
  root  : TTreeViewItem ;
  gnode : TTreeViewItem ;
  snode : TTreeViewItem ;
  cnode : TTreeViewItem ;
  i     : Integer ;
  k     : Integer ;
begin
  tvPages.BeginUpdate ;
  try
    if not _full then begin
      root := TTreeViewItem.Create(tvPages) ;
      root.TextAlign := TTextAlign.Leading ;
      root.WordWrap := False ;
      root.Text := T_scrollablePanel( pnlGeneral ).ItemText ;
      root.Parent := tvPages ;
      exit ;
    end ;

    gnode := TTreeViewItem.Create(tvPages) ;
    gnode.TextAlign := TTextAlign.Leading ;
    gnode.WordWrap := False ;

    gnode.Text := T_scrollablePanel( pnlSections ).ItemText ;
    gnode.Parent := tvPages ;
    snode := nil ;

    lstFeatures := T_sectionContainer.Create( Self ) ;

    for k := 0 to MVC.SectionCount - 1 do begin

      MVC.SectionIndex := k ;
      T_sectionContainer( lstFeatures ).ReadSection ;

      for i := 0 to T_sectionContainer( lstFeatures ).Count - 1 do begin
        if i = 0 then begin
          snode := TTreeViewItem.Create(tvPages) ;
          snode.TextAlign := TTextAlign.Leading ;
          snode.WordWrap := False ;
          snode.Text := T_sectionContainer( lstFeatures ).Panel[i].ItemText ;
          snode.Parent := gnode ;
        end
        else begin
          cnode := TTreeViewItem.Create(tvPages) ;
          cnode.TextAlign := TTextAlign.Leading ;
          cnode.WordWrap := False ;
          cnode.Text := T_sectionContainer( lstFeatures ).Panel[i].ItemText ;
          cnode.Parent := snode ;
        end ;
      end ;
    end ;

    if gnode.Count > 0 then
      gnode.Items[0].Expand ;

    gnode.Expand ;
  finally
    tvPages.EndUpdate ;
  end;
end ;

procedure TGIS_ControlLegendForm.setSection(
  const _idx : Integer ;
  const _fea : Boolean
) ;
var
  node  : TTreeViewItem ;
  i     : Integer ;
begin
  T_sectionContainer( lstFeatures ).Write ;
  MVC.SectionIndex := _idx ;
  T_sectionContainer( lstFeatures ).Read ;

  if _fea then exit ;

  tvPages.BeginUpdate ;
  try
    for i := 0 to MVC.SectionCount - 1 do begin
      node := tvPages.Items[GIS_CONTAINER_INDEX].Items[i] ;

      if node.Index <> _idx then
        node.Collapse
      else
        node.IsExpanded := True ;
    end ;
  finally
    tvPages.EndUpdate ;
  end;
end ;

procedure TGIS_ControlLegendForm.fillComboBoxWithFields(
  const _cmb : TComboBox
) ;
begin
  _cmb.Items.BeginUpdate ;
  _cmb.Items.Assign( MVC.FieldNames ) ;
  _cmb.Items.EndUpdate ;
end ;

procedure TGIS_ControlLegendForm.fillComboBoxWithFields(
  const _cmb : TComboEdit
) ;
begin
  _cmb.Items.BeginUpdate ;
  _cmb.Items.Assign( MVC.FieldNames ) ;
  _cmb.Items.EndUpdate ;
end ;

procedure TGIS_ControlLegendForm.showPreview(
  const _show : Boolean
) ;
begin
  if _show then begin
    if gisPreviewR.Visible then exit ;
    tvPages.Height := tvPages.Height - tvPages.Width
  end
  else begin
    if not gisPreviewR.Visible then exit ;
    tvPages.Height := tvPages.Height + tvPages.Width ;
  end ;

  gisPreviewR.Visible := _show ;
end ;

procedure TGIS_ControlLegendForm.lock ;
begin
  bLock := True ;
end ;

procedure TGIS_ControlLegendForm.unlock ;
begin
  bLock := False ;
end ;

function TGIS_ControlLegendForm.isLocked : Boolean ;
begin
  Result := bLock ;
end ;

procedure TGIS_ControlLegendForm.waitNoBusy ;
begin
  // what until all pending updates will be finished
  repeat
    Application.ProcessMessages ;
  until not tmrUpdate.Enabled ;

  repeat
    Application.ProcessMessages ;
  until iBusy = 0 ;
end ;

procedure TGIS_ControlLegendForm.updateStatistics(
  const _layer : TGIS_Layer
) ;
var
  frm_stats : TGIS_ControlStatistics ;
begin
  if _layer.MustCalculateStatistics then begin
    frm_stats := TGIS_ControlStatistics.Create( oMainForm ) ;
    frm_stats.Execute(
      _layer,
      pOnHelp,
      procedure( _modal_result : TModalResult )
      begin
        if ( _modal_result = mrCancel ) then
          _layer.Statistics.ResetModified ;
      end
    ) ;
  end ;
end;

procedure TGIS_ControlLegendForm.initForm ;
begin
  {$IFDEF GIS_MOBILE}
    Self.ClientWidth  := 660 ;
  {$ELSE}
    Self.ClientWidth  := 560 ;
  {$ENDIF}
  Self.ClientHeight := 530 ;
  Self.Name := 'TGIS_ControlLegendForm' ;
end ;

procedure TGIS_ControlLegendForm.initControls ;
begin
  initSelf ;
end ;

procedure TGIS_ControlLegendForm.initSelf ;
var
  anchorsB  : TAnchors ;
  anchorsRB : TAnchors ;
begin
  layBottom := TLayout.Create( oMainForm ) ;
  layBottom.Parent := oMainForm ;
  layBottom.Align  := TAlignLayout.Bottom ;
  layBottom.Height := 40 ;
  layBottom.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akBottom ] ;

  if BiDiMode = bdRightToLeft then begin
    anchorsB  := [TAnchorKind.akRight, TAnchorKind.akBottom] ;
    anchorsRB := [TAnchorKind.akLeft,  TAnchorKind.akBottom] ;
  end else begin
    anchorsB  := [TAnchorKind.akLeft,  TAnchorKind.akBottom] ;
    anchorsRB := [TAnchorKind.akRight, TAnchorKind.akBottom] ;
  end ;

  btnWizard := TButton.Create( layBottom ) ;
  btnWizard.Parent := layBottom ;
  {$IFNDEF GIS_MOBILE}
    btnWizard.Height := 24 ;
  {$ENDIF}
  {$IFDEF GIS_MOBILE}
    btnWizard.Scale.X := 0.8 ;
    btnWizard.Scale.Y := 0.8 ;
  {$ENDIF}
  btnWizard.Position.Y  := layBottom.Height - btnWizard.Height - 8 ;
  PlaceControl( BiDiMode, nil, btnWizard, 8, 80 ) ;
  btnWizard.Anchors := anchorsB ;
  btnWizard.Text := _rsrcna( GIS_RS_BTN_WIZARD ) ;
  btnWizard.TextSettings.Trimming := TTextTrimming.None ;
  btnWizard.TextSettings.WordWrap := False ;
  btnWizard.TabOrder := 0 ;
  btnWizard.OnClick := doWizardClick ;

  {$IFNDEF GIS_MOBILE_DIALOGS}
    if Assigned( btnHelp ) then begin
      btnHelp.Parent := nil ;
      FreeObject( btnHelp ) ;
    end;
    btnHelp := TButton.Create( layBottom ) ;
    btnHelp.Parent := layBottom ;
    {$IFNDEF GIS_MOBILE}
      btnHelp.Height := 24 ;
    {$ENDIF}
    btnHelp.Position.Y  := layBottom.Height - btnHelp.Height - 8 ;
    PlaceControl( BiDiMode, btnWizard, btnHelp, 8, 80 ) ;
    btnHelp.Text := _rsrcna( GIS_RS_BTN_HELP ) ;
    btnHelp.TabOrder := 1 ;
    btnHelp.Visible := Assigned( pOnHelp ) ;
    btnHelp.OnClick := btnHelpClick ;

    btnApply := TButton.Create( layBottom ) ;
    btnApply.Parent := layBottom ;
    {$IFNDEF GIS_MOBILE}
      btnApply.Height := 24 ;
    {$ENDIF}
    btnApply.Position.Y  := layBottom.Height - btnApply.Height - 8 ;
    PlaceControl( BiDiMode, nil, btnApply, -8, 80 ) ;
    btnApply.Anchors := anchorsRB ;
    btnApply.Text := _rsrcna( GIS_RS_BTN_APPLY ) ;
    btnApply.TabOrder := 4 ;
    btnApply.OnClick := doApplySettings ;

    if Assigned( btnCancel ) then begin
      btnCancel.Parent := nil ;
      FreeObject( btnCancel ) ;
    end ;
    btnCancel := TButton.Create( layBottom ) ;
    btnCancel.Parent := layBottom ;
    {$IFNDEF GIS_MOBILE}
      btnCancel.Height := 24 ;
    {$ENDIF}
    btnCancel.Position.Y  := layBottom.Height - btnCancel.Height - 8 ;
    PlaceControl( BiDiMode, btnApply, btnCancel, -8, 80 ) ;
    btnCancel.Anchors := anchorsRB ;
    btnCancel.Text := _rsrcna( GIS_RS_BTN_CANCEL ) ;
    btnCancel.TabOrder := 3 ;
    btnCancel.OnClick := btnCancelClick ;

    if Assigned( btnOK ) then begin
      btnOK.Parent := nil ;
      FreeObject( btnOK ) ;
    end;
    btnOK := TButton.Create( layBottom ) ;
    btnOK.Parent := layBottom ;
    {$IFNDEF GIS_MOBILE}
      btnOK.Height := 24 ;
    {$ENDIF}
    btnOK.Position.Y  := layBottom.Height - btnOK.Height - 8 ;
    PlaceControl( BiDiMode, btnCancel, btnOK, -8, 80 ) ;
    btnOK.Anchors := anchorsRB ;
    btnOK.Text := _rsrcna( GIS_RS_BTN_OK ) ;
    btnOK.TabOrder := 2 ;
    btnOK.OnClick := btnOKClick ;
  {$ENDIF}

  // enfore proper size
  layBottom.Height := btnWizard.Height + 8 +8 ;
end ;

procedure TGIS_ControlLegendForm.addResBmp(
  const _name   : String ;
  const _parent : TCustomButton
) ;
var
  rstm  : TResourceStream ;
  bmp   : TBitmap ;
  img   : TImage ;
  dat   : TBitmapData ;
  x     : Integer ;
  y     : Integer ;
  w1    : Integer ;
  btn   : TCustomButton ;
  obj   : TFMXObject ;
  clr   : TAlphaColor ;
  blck  : TAlphaColor ;

begin
  rstm := TResourceStream.Create(hInstance, _name, RT_RCDATA) ;
  try
    rstm.Position := 0 ;

    try
      if _parent is TSpeedButton then
        btn := TSpeedButton.Create( nil )
      else
        btn := TButton.Create( nil ) ;

      btn.Visible := False;
      btn.Parent := Self ;
      btn.ApplyStyleLookup ;

      obj := btn.FindStyleResource( 'text' ) ;
      if obj is TText then
        clr := TText(obj).TextSettings.FontColor
      else
        clr := TAlphaColorRec.Null ;
    finally
      FreeObject( btn ) ;
    end;

    bmp := TBitmap.Create ;
    try
      bmp.LoadFromStream( rstm ) ;

      if clr <> TAlphaColorRec.Null  then begin
        bmp.Map( TMapAccess.ReadWrite, dat );
        try
          if dat.PixelFormat = TPixelFormat.RGBA then begin
            blck := RGBtoBGR( TAlphaColorRec.Black  ) ;
            clr  := RGBtoBGR( clr ) ;
          end
          else
            blck := RGBtoBGR( TAlphaColorRec.Black ) ;

          for y := 0 to bmp.Height - 1 do begin
            w1 := y * dat.Pitch ;
            for x := 0 to bmp.Width - 1 do begin
              if PCardinal( IntPtr(dat.Data) + w1+x*4 )^ = blck
              then
                PCardinal( IntPtr( dat.Data ) + w1+x*4 )^ := clr ;
            end;
          end;
        finally
          bmp.Unmap(dat);
        end;
      end;

      img := TImage.Create( _parent ) ;
      img.Parent := _parent ;
      img.Name := 'img' ;
      img.Margins.Left   := 5 ;
      img.Margins.Top    := 5 ;
      img.Margins.Right  := 5 ;
      img.Margins.Bottom := 5 ;
      img.Align := TAlignLayout.Client ;
      img.Bitmap.Assign( bmp );
      img.HitTest := False ;
      img.WrapMode := TImageWrapMode.Fit ;
    finally
      FreeObject( bmp ) ;
    end ;
  finally
    FreeObject( rstm ) ;
  end ;
end ;

procedure TGIS_ControlLegendForm.initLeft ;
var
  t      : Single  ;
  sizmul : Double  ;
  ppi    : Integer ;
begin
  pnlLeft := TPanel.Create( oMainForm ) ;
  pnlLeft.Parent  := oMainForm ;
  {$IFNDEF GIS_MOBILE}
    pnlLeft.Width    := 130  ;
  {$ELSE}
    pnlLeft.Width    := 200  ;
  {$ENDIF}
  if BiDiMode = bdRightToLeft then begin
    pnlLeft.Position.X := oMainForm.Width - pnlLeft.Width ;
    pnlLeft.Align := TAlignLayout.Right ;
  end else begin
    pnlLeft.Position.X := 0 ;
    pnlLeft.Align := TAlignLayout.Left ;
  end;
  pnlLeft.Position.Y := 0 ;
  pnlLeft.Height  := oMainForm.Height - 32 ;
  pnlLeft.Name := 'pnlLeft' ;

  with TSplitter.Create( oMainForm ) do begin
    Parent  := oMainForm ;
    if BiDiMode = bdRightToLeft then begin
      Position.X := oMainForm.Width - pnlLeft.Width - Width ;
      Align := TAlignLayout.Right ;
    end else begin
      Position.X := pnlLeft.Width ;
      Align := TAlignLayout.Left ;
    end;
  end;

  t := 0 ;

  tvPages := TTreeView.Create( pnlLeft ) ;
  tvPages.Parent   := pnlLeft ;
  tvPages.Position.X := 0 ;
  tvPages.Position.Y := 0 ;
  tvPages.Align   := TAlignLayout.Client ;
  tvPages.Width   := pnlLeft.Width ;
  tvPages.Height  := pnlLeft.Height - 24 ;
  tvPages.Anchors := [ TAnchorKind.akLeft, TAnchorKind.akTop,
                       TAnchorKind.akRight, TAnchorKind.akBottom
                     ] ;

  t := t + tvPages.Height ;

  ppi := RoundS( oViewer.PPI ) ;

  sizmul := GUIScale * ppi/SystemPPI ;

  gisPreview := TGIS_ViewerBmp.Create( TruncS(tvPages.Width*sizmul),
                                       TruncS(tvPages.Width*sizmul)
                                     ) ;
  gisPreview.BigExtentMargin := -10;
  gisPreview.IncrementalPaint := False;
  gisPreview.Name := 'gisPreview';
  gisPreview.RestrictedDrag := True;
  gisPreview.SelectionOutlineOnly := False;
  gisPreview.SelectionTransparency := 100;
  gisPreview.SelectionWidth := 100;
  gisPreview.UseRTree := False;
  gisPreview.Zoom := 1;
  gisPreview.ZoomEx := 15;

  gisPreviewR := TRectangle.Create( pnlLeft ) ;
  gisPreviewR.Parent  := pnlLeft ;
  gisPreviewR.Position.X := 0 ;
  gisPreviewR.Position.Y := t - tvPages.Width ;
  gisPreviewR.Width  := tvPages.Width ;
  gisPreviewR.Height := tvPages.Width ;
  gisPreviewR.Anchors := [ TAnchorKind.akLeft,
                           TAnchorKind.akRight,
                           TAnchorKind.akBottom
                         ] ;
  gisPreviewR.Align := TAlignLayout.Bottom ;
  gisPreviewR.Visible := False ;
  gisPreviewR.Name := 'gisPreviewR';
  gisPreviewR.Stroke.Kind := TBrushKind.None ;

  if assigned( oViewerWnd ) then begin
    gisPreviewR.Fill.Color := oViewerWnd.BackgroundColor ;
  end;

  gisPreviewI := TImage.Create( gisPreviewR ) ;
  gisPreviewI.Parent := gisPreviewR ;
  gisPreviewI.Align := TAlignLayout.Client ;
  gisPreviewI.Visible := True ;
  gisPreviewI.Name := 'gisPreviewI';

  gisPreview.CustomPPI := oViewer.PPI ;

  tmrShowForm := TTimer.Create( Self ) ;
  tmrShowForm.Interval := 1 ;
  tmrShowForm.OnTimer := doShow ;
  tmrShowForm.Enabled := False ;

  tmrUpdate := TTimer.Create( Self ) ;
  tmrUpdate.Interval := 500 ;
  tmrUpdate.OnTimer := doUpdateTimer ;
  tmrUpdate.Enabled := False ;

  toolButtons := TToolBar.Create( pnlLeft ) ;
  toolButtons.Parent  := pnlLeft ;
  toolButtons.Align := TAlignLayout.Top ;
  toolButtons.Name := 'toolButtons' ;
  toolButtons.Height := 24 ;

  btnAdd := TSpeedButton.Create( toolButtons ) ;
  btnAdd.Parent := toolButtons ;
  btnAdd.OnClick := doAddClick ;
  {$IFNDEF GIS_MOBILE}
    btnAdd.Width  := 24 ;
    btnAdd.Height := 24 ;
  {$ELSE}
    btnAdd.Width  := 32 ;
    btnAdd.Height := 32 ;
  {$ENDIF}

  addResBmp( 'TGIS_LEGENDFORMIMAGE_ADD', btnAdd ) ;
  btnAdd.Align := TAlignLayout.Left ;

  btnRemove := TSpeedButton.Create( toolButtons ) ;
  btnRemove.Parent := toolButtons ;
  btnRemove.OnClick := doRemoveClick ;
  {$IFNDEF GIS_MOBILE}
    btnRemove.Width  := 24 ;
    btnRemove.Height := 24 ;
  {$ELSE}
    btnRemove.Width  := 32 ;
    btnRemove.Height := 32 ;
  {$ENDIF}

  addResBmp( 'TGIS_LEGENDFORMIMAGE_DELETE', btnRemove ) ;
  btnRemove.Align := TAlignLayout.Left ;

  btnClear := TSpeedButton.Create( toolButtons ) ;
  btnClear.Parent := toolButtons ;
  btnClear.OnClick := doClearClick ;
  {$IFNDEF GIS_MOBILE}
    btnClear.Width  := 24 ;
    btnClear.Height := 24 ;
  {$ELSE}
    btnClear.Width  := 32 ;
    btnClear.Height := 32 ;
  {$ENDIF}

  addResBmp( 'TGIS_LEGENDFORMIMAGE_CLEAR', btnClear ) ;
  btnClear.Align := TAlignLayout.Left ;

  btnMoveDown := TSpeedButton.Create( toolButtons ) ;
  btnMoveDown.Parent := toolButtons ;
  btnMoveDown.OnClick := doMoveDownClick ;
  {$IFNDEF GIS_MOBILE}
    btnMoveDown.Width  := 24 ;
    btnMoveDown.Height := 24 ;
  {$ELSE}
    btnMoveDown.Width  := 32 ;
    btnMoveDown.Height := 32 ;
  {$ENDIF}

  addResBmp( 'TGIS_LEGENDFORMIMAGE_BTNDOWN', btnMoveDown ) ;
  btnMoveDown.Align := TAlignLayout.Left ;

  btnMoveUp := TSpeedButton.Create( toolButtons ) ;
  btnMoveUp.Parent := toolButtons ;
  btnMoveUp.OnClick := doMoveUpClick ;
  {$IFNDEF GIS_MOBILE}
    btnMoveUp.Width  := 24 ;
    btnMoveUp.Height := 24 ;
  {$ELSE}
    btnMoveUp.Width  := 32 ;
    btnMoveUp.Height := 32 ;
  {$ENDIF}

  addResBmp( 'TGIS_LEGENDFORMIMAGE_BTNUP', btnMoveUp ) ;
  btnMoveUp.Align := TAlignLayout.Left ;

  toolButtons := TToolBar.Create( pnlLeft ) ;
  toolButtons.Parent  := pnlLeft ;
  toolButtons.Align := TAlignLayout.Bottom ;
  toolButtons.Name := 'toolButtons2' ;
  toolButtons.Height := 24 ;

  {$IFDEF GIS_MOBILE}
    toolButtons.Visible := False ;
  {$ENDIF}

  btnOpen := TSpeedButton.Create( toolButtons ) ;
  btnOpen.Parent := toolButtons ;
  btnOpen.OnClick := doOpenClick ;
  {$IFNDEF GIS_MOBILE}
    btnOpen.Width  := 24 ;
    btnOpen.Height := 24 ;
  {$ELSE}
    btnOpen.Width  := 32 ;
    btnOpen.Height := 32 ;
  {$ENDIF}
  addResBmp( 'TGIS_LEGENDFORMIMAGE_OPEN', btnOpen ) ;
  btnOpen.Align := TAlignLayout.Left ;

  btnSave := TSpeedButton.Create( toolButtons ) ;
  btnSave.Parent := toolButtons ;
  btnSave.OnClick := doSaveClick ;
  {$IFNDEF GIS_MOBILE}
    btnSave.Width  := 24 ;
    btnSave.Height := 24 ;
  {$ELSE}
    btnSave.Width  := 32 ;
    btnSave.Height := 32 ;
  {$ENDIF}
  addResBmp( 'TGIS_LEGENDFORMIMAGE_SAVE', btnSave ) ;
  btnSave.Align := TAlignLayout.Left ;

  dlgOpen := TOpenDialog.Create( Self ) ;
  dlgOpen.Filter := _rsrc( GIS_RS_LEGEND_CONFIG_FILTER_OPEN ) ;

  dlgSave := TSaveDialog.Create( Self ) ;
  dlgSave.Filter := _rsrc( GIS_RS_LEGEND_CONFIG_FILTER_SAVE ) ;
end ;

procedure TGIS_ControlLegendForm.initRight(
  const _full : Boolean
) ;
begin
  if not _full then begin

    layRight := T_scrollablePanel.Create( oMainForm, self ) ;
    layRight.Parent  := oMainForm ;
    if BiDiMode = bdRightToLeft then
      layRight.Position.X := 0
    else
      layRight.Position.X := pnlLeft.Width ;
    layRight.Position.Y   := 0 ;
    layRight.Align   := TAlignLayout.Client ;
    layRight.Name := 'layRight' ;

    pnlRight := TLayout.Create( layRight ) ;
    pnlRight.Parent := layRight ;
    pnlRight.Position.X := 0 ;
    pnlRight.Position.Y := 0 ;
    pnlRight.Align := TAlignLayout.Client ;
    pnlRight.Name := 'pnlRight' ;

    pnlTop := TLayout.Create( layRight ) ;
    pnlTop.Parent := layRight ;
    pnlTop.Align := TAlignLayout.Top ;
    pnlTop.Visible := False ;
    pnlTop.Height := 24 ;

    lblTop := TLabel.Create( pnlTop ) ;
    lblTop.Parent := pnlTop ;
    lblTop.Align := TAlignLayout.Client ;
    lblTop.Visible := False ;
    lblTop.TextAlign := TTextAlign.Center ;

    pnlGeneral := T_panelGeneral.Create( pnlRight, Self ) ;
    pnlGeneral.Position.X    := 0 ;
    pnlGeneral.Position.Y     := 0 ;
    pnlGeneral.Visible := False ;
    pnlGeneral.Name := 'pnlGeneral' ;
    pnlGeneral.Align := TAlignLayout.Client ;

    pnlSections := T_panelSections.Create( pnlRight, Self ) ;
    pnlSections.Position.X    := 0 ;
    pnlSections.Position.Y     := 0 ;
    pnlSections.Visible := False ;
    pnlSections.Align := TAlignLayout.Client ;
    pnlSections.Name := 'pnlSections' ;

  end ;

  readLayer( _full )  ;

  pnlCurrent := pnlGeneral ;
  pnlCurrent.Visible := True ;

  doPageChange( tvPages.Items[0] ) ;
  tvPages.OnChange := doTvPagesOnChange ;
end ;

procedure TGIS_ControlLegendForm.doShow ;
begin
  tmrShowForm.Enabled := False ;
  BeginUpdate ;
  initRight( True ) ;
  EndUpdate ;
end;

procedure TGIS_ControlLegendForm.doPageChange(
  _node   : TTreeViewItem
) ;
var
  prnt2 : TTreeViewItem ;
  prnt  : TTreeViewItem ;
  i     : Integer ;
begin
  if isLocked then exit ;

  tvPages.BeginUpdate ;
  try

    prnt := _node.ParentItem;

    pnlCurrent.Visible := False ;
    pnlTop.Visible := False ;
    lblTop.Visible := False ;

    //--- General, Sections ---//

    if not Assigned( prnt ) then begin

      btnRemove.Enabled   := False ;
      btnMoveUp.Enabled   := False ;
      btnMoveDown.Enabled := False ;

      case _node.Index of
        0 : pnlCurrent := pnlGeneral ;
        1 : pnlCurrent := pnlSections ;
        2 : begin
              lastSection := '' ;
              lastNodeIdx := 0 ;
              doPageChange( _node.Items[0] ) ;
              exit ;
            end ;
      end ;

      pnlCurrent.Visible := True ;

      // prepare list
      if pnlCurrent = pnlSections then
        T_panelSections( pnlSections ).Prepare ;

      if T_scrollablePanel( pnlCurrent ).HasPreview then begin
        T_scrollablePanel( pnlCurrent ).PreparePreview( gisPreview ) ;
        T_scrollablePanel( pnlCurrent ).UpdatePreview ;
      end ;

      showPreview( T_scrollablePanel( pnlCurrent ).HasPreview ) ;

      tvPages.Selected := _node ;

      exit ;
    end ;

    //--- sections ---//
    prnt2 := prnt.ParentItem ;
    if ( not Assigned( prnt2 ) ) and
       ( prnt.Index = GIS_CONTAINER_INDEX ) then begin

      btnRemove.Enabled := MVC.SectionCount <> 1 ;
      btnMoveUp.Enabled := _node.Index <> 0 ;
      btnMoveDown.Enabled := _node.Index <> prnt.Count - 1 ;

      setSection( _node.Index, False ) ;

      if not IsStringEmpty( lastSection ) and
         ( _node.Index <> lastNodeIdx ) then begin
        for i := 0 to _node.Count-1 do
          if _node[i].Text = lastSection then begin
            lastNodeIdx := _node.Index ;
            doPageChange( _node[i] ) ;
            exit ;
          end ;
      end;

      pnlCurrent := T_sectionContainer( lstFeatures ).Panel[0] ;
      pnlTop.Visible := True ;
      lblTop.Visible := True ;
      pnlCurrent.Visible := True ;

      if T_scrollablePanel( pnlCurrent ).HasPreview then begin
        T_scrollablePanel( pnlCurrent ).PreparePreview( gisPreview ) ;
        T_scrollablePanel( pnlCurrent ).UpdatePreview ;
      end ;

      showPreview( T_scrollablePanel( pnlCurrent ).HasPreview ) ;
      lastNodeIdx := _node.Index ;
      lastSection := '' ;

      exit ;
    end ;

    //--- sections' internals ---//

    if prnt.Index <> MVC.SectionIndex then
      setSection( prnt.Index, True ) ;

    btnRemove.Enabled   := False ;
    btnMoveUp.Enabled   := False ;
    btnMoveDown.Enabled := False ;

    pnlCurrent := T_sectionContainer( lstFeatures ).FindPanel( _node.Text ) ;
    pnlTop.Visible := True ;
    lblTop.Visible := True ;
    pnlCurrent.Visible := True ;

    lastSection := _node.Text ;

    if T_scrollablePanel( pnlCurrent ).HasPreview then begin
      T_scrollablePanel( pnlCurrent ).PreparePreview( gisPreview ) ;
      T_scrollablePanel( pnlCurrent ).UpdatePreview ;
    end ;

    showPreview( T_scrollablePanel( pnlCurrent ).HasPreview ) ;


  finally
    tvPages.Selected := _node ;
    tvPages.EndUpdate ;
  end;

end ;

procedure TGIS_ControlLegendForm.doTvPagesOnChange(
  _sender : TObject
) ;
begin
  if _sender is TTreeViewItem then
    doPageChange( _sender as TTreeViewItem )
  else
    doPageChange( tvPages.Selected ) ;
end ;

procedure TGIS_ControlLegendForm.doUpdateTimer(
  _sender : TObject
) ;
begin
  Inc( iBusy ) ;
  try
    tmrUpdate.Enabled := False ;
    gisPreviewR.Visible := True ;

    gisPreview.ControlUpdateWholeMap ;
    gisPreviewI.Bitmap.Assign( TBitmap( gisPreview.Bitmap ) ) ;
  finally
    Dec( iBusy ) ;
  end ;
end ;

procedure TGIS_ControlLegendForm.doApplySettings(
  _sender : TObject
) ;
var
  last_index : Integer ;
  old_agg    : Boolean ;
  new_agg    : Boolean ;
begin
  waitNoBusy ;
  last_index := MVC.SectionIndex ;
  if MVC.Layer.IsVector then
    old_agg := assigned( TGIS_LayerVector(MVC.Layer).DynamicAggregator )
  else
    old_agg := False ;

  T_panelGeneral( pnlGeneral ).Write ;
  T_sectionContainer( lstFeatures ).Write ;
  MVC.Write ;
  MVC.SectionIndex := last_index ;

  updateStatistics( MVC.Layer );

  if MVC.Layer.IsVector then begin
    new_agg := assigned( TGIS_LayerVector(MVC.Layer).DynamicAggregator ) ;
    if new_agg or ( new_agg <> old_agg ) then
      doSectionsRebuild ;
  end ;

  if Assigned( oViewerWnd ) then
    oViewerWnd.InvalidateWholeMap ;
end ;

procedure TGIS_ControlLegendForm.btnOKClick(
  _sender : TObject
) ;
begin
  {$IFNDEF GIS_MOBILE_DIALOGS}
    waitNoBusy ;
    T_panelGeneral( pnlGeneral ).Write ;
    T_sectionContainer( lstFeatures ).Write ;
    MVC.Write ;

    updateStatistics( MVC.Layer );
  {$ENDIF}

  ModalResult := mrOK ;
end ;

procedure TGIS_ControlLegendForm.doAddClick(
  _sender : TObject
) ;
var
  node  : TTreeViewItem ;
  root  : TTreeViewItem ;
  cnode : TTreeViewItem ;
  i     : Integer ;
  ntext : String ;
begin
  MVC.SectionAdd( True ) ;

  ntext := Format( '%s %d', [ _rsrc( GIS_RS_LEGEND_PAG_SECTION ),
                              MVC.SectionCount ]
                  ) ;

  tvPages.BeginUpdate ;
  try
    root := tvPages.Items[GIS_CONTAINER_INDEX] ;
    node := root ;
    for i := 0 to T_sectionContainer( lstFeatures ).Count - 1 do begin
      if i = 0 then begin
        node := TTreeViewItem.Create(tvPages) ;
        node.TextAlign := TTextAlign.Leading ;
        node.WordWrap := False ;
        node.Text := ntext ;
        node.Parent := root ;
      end
      else begin
        cnode := TTreeViewItem.Create(tvPages) ;
        cnode.TextAlign := TTextAlign.Leading ;
        cnode.WordWrap := False ;
        cnode.Text := T_sectionContainer( lstFeatures ).Panel[i].ItemText ;
        cnode.Parent := node ;
      end ;
    end ;
  finally
    tvPages.EndUpdate ;
    tvPages.Repaint ;
  end;

  doPageChange( node ) ;
end ;

procedure TGIS_ControlLegendForm.doRemoveClick(
  _sender : TObject
) ;
var
  node : TTreeViewItem ;
begin
  lock ;
  try
    tvPages.BeginUpdate ;
    try
      if MVC.SectionCount <> 1 then begin
        node := tvPages.Items[GIS_CONTAINER_INDEX].Items[MVC.SectionIndex] ;
        node.DeleteChildren ;
        tvPages.Items[GIS_CONTAINER_INDEX].RemoveObject( node ) ;
      end ;

      MVC.SectionDelete ;
      T_sectionContainer( lstFeatures ).Read ;
    finally
      tvPages.EndUpdate ;
      tvPages.Repaint ;
    end;
  finally
    unlock ;
  end;

  doPageChange( tvPages.Items[GIS_CONTAINER_INDEX].Items[MVC.SectionIndex] ) ;
end ;


procedure TGIS_ControlLegendForm.doClearClick(
  _sender : TObject
) ;
var
  node  : TTreeViewItem ;
  i     : Integer ;
  ntext : String ;
begin
  lock ;
  try
    MVC.SectionClear ;
    node := tvPages.Items[GIS_CONTAINER_INDEX] ;
    tvPages.BeginUpdate ;
    try
    for i := node.Count - 1 downto 1 do
      node.RemoveObject( node.Items[i] ) ;
    finally
      tvPages.EndUpdate ;
      tvPages.Repaint ;
    end;

    ntext := Format( '%s %d', [ _rsrc( GIS_RS_LEGEND_PAG_SECTION ),
                                MVC.SectionCount ]
                    ) ;
    if T_sectionContainer( lstFeatures ).Count > 0 then begin
      T_sectionContainer( lstFeatures ).Panel[0].ItemText := ntext ;
      node.Items[0].Text := ntext ;
    end ;

    T_sectionContainer( lstFeatures ).Read ;
  finally
    unlock ;
  end;
  doPageChange( node.Items[0] ) ;
end ;

procedure TGIS_ControlLegendForm.doMoveUpClick(
  _sender : TObject
) ;
var
  node : TTreeViewItem ;
begin
  if MVC.SectionIndex = 0 then
    exit ;

  MVC.SectionMove( True ) ;
  MVC.SectionIndex := MVC.SectionIndex - 1 ;

  tvPages.BeginUpdate ;
  try
    node := tvPages.Items[GIS_CONTAINER_INDEX].Items[MVC.SectionIndex+1] ;
    tvPages.Items[GIS_CONTAINER_INDEX].RemoveObject( node ) ;
    tvPages.Items[GIS_CONTAINER_INDEX].InsertObject( MVC.SectionIndex, node ) ;
    node.IsSelected := True ;
  finally
    tvPages.EndUpdate ;
    tvPages.Repaint ;
  end;

  doPageChange( tvPages.Items[GIS_CONTAINER_INDEX].Items[MVC.SectionIndex] ) ;
end ;

procedure TGIS_ControlLegendForm.doMoveDownClick(
  _sender : TObject
) ;
var
  node : TTreeViewItem ;
begin
  if MVC.SectionIndex = MVC.SectionCount - 1 then
    exit ;

  MVC.SectionMove( False ) ;
  MVC.SectionIndex := MVC.SectionIndex + 1 ;

  tvPages.BeginUpdate ;
  try
    node := tvPages.Items[GIS_CONTAINER_INDEX].Items[MVC.SectionIndex-1] ;
    tvPages.Items[GIS_CONTAINER_INDEX].RemoveObject( node ) ;
    tvPages.Items[GIS_CONTAINER_INDEX].InsertObject( MVC.SectionIndex, node ) ;
    node.IsSelected := True ;
  finally
    tvPages.EndUpdate ;
    tvPages.Repaint ;
  end;

  doPageChange( tvPages.Items[GIS_CONTAINER_INDEX].Items[MVC.SectionIndex] ) ;
end ;

procedure TGIS_ControlLegendForm.doOpenClick(
  _sender : TObject
) ;
var
  desc : String ;
  i,k  : Integer ;
  node : TTreeViewItem ;
  snode: TTreeViewItem ;
  cnode: TTreeViewItem ;
begin
  if dlgOpen.Execute then begin
    MVC.PrepareLayerEx ;
    try
      lock ;
      try
        for i :=  MVC.SectionCount - 1 downto 0 do
          MVC.SectionDelete ;

        tvPages.BeginUpdate ;
        try
          node := tvPages.Items[GIS_CONTAINER_INDEX] ;
          for i := node.Count - 1 downto 0 do
            node.RemoveObject( node.Items[i] ) ;

          MVC.SwitchToLayerEx ;
          MVC.LoadConfig( dlgOpen.FileName ) ;

          for k := 0 to MVC.LayerEx.ParamsList.Count - 1 do begin
            MVC.SectionAdd( False ) ;
            for i := 0 to T_sectionContainer( lstFeatures ).Count - 1 do begin
              if i = 0 then begin
                snode := TTreeViewItem.Create(tvPages) ;
                snode.TextAlign := TTextAlign.Leading ;
                snode.WordWrap := False ;
                // set to the section to get the section description
                MVC.SectionIndex := k ;
                if not IsStringEmpty( MVC.Section.Legend ) then
                  desc := MVC.Section.Legend
                else
                  desc := Format( '%s %d', [ _rsrc( GIS_RS_LEGEND_PAG_SECTION ),
                                            MVC.SectionCount ]
                                ) ;
                snode.Text := desc ;
                snode.Parent := node ;
              end
              else begin
                cnode := TTreeViewItem.Create(tvPages) ;
                cnode.TextAlign := TTextAlign.Leading ;
                cnode.WordWrap := False ;
                cnode.Text := T_sectionContainer( lstFeatures ).Panel[i].ItemText ;
                cnode.Parent := snode ;
              end ;
            end ;
          end ;
        finally
          MVC.SectionIndex := 0 ;
          tvPages.EndUpdate ;
          tvPages.Repaint ;
        end;
      finally
        unlock ;
        MVC.SwitchToLayer ;
      end ;
      T_panelGeneral( pnlGeneral ).mvc.Read ;
      T_panelGeneral( pnlGeneral ).Read ;
      T_sectionContainer( lstFeatures ).Read ;
      doPageChange( tvPages.Items[GIS_CONTAINER_INDEX] ) ;
    finally
      MVC.UnPrepareLayerEx ;
    end;
  end ;
end ;

procedure TGIS_ControlLegendForm.doSaveClick(
  _sender : TObject
) ;
begin
  if IsServerPath( MVC.Layer.Path ) then
    dlgSave.FileName := MVC.Layer.Name + GIS_TTKSTYLE_EXT
  else
    dlgSave.FileName := MVC.Layer.Path + GIS_TTKSTYLE_EXT ;

  dlgSave.DefaultExt := GIS_TTKSTYLE_EXT ;
  if dlgSave.Execute then begin
    MVC.PrepareLayerEx ;
    try
      MVC.SwitchToLayerEx ;
      try
        MVC.SaveConfig( dlgSave.FileName ) ;
      finally
        MVC.SwitchToLayer ;
      end ;
    finally
      MVC.UnPrepareLayerEx ;
    end ;
  end ;
end ;

procedure TGIS_ControlLegendForm.doSectionsRebuild ;
var
  i, k  : Integer ;
  desc  : String ;
  node  : TTreeViewItem ;
  snode : TTreeViewItem ;
  cnode : TTreeViewItem ;
begin
  lock ;
  try
    for i := MVC.SectionCount - 1 downto 0 do
      MVC.SectionDelete ;

    tvPages.BeginUpdate ;
    try
      node := tvPages.Items[GIS_CONTAINER_INDEX] ;
      for i := node.Count - 1 downto 0 do
        node.RemoveObject( node.Items[i] ) ;

      for k := 0 to MVC.Layer.ParamsList.Count - 1 do begin
        MVC.SectionAdd( False ) ;
        for i := 0 to T_sectionContainer( lstFeatures ).Count - 1 do begin
          if i = 0 then begin
            snode := TTreeViewItem.Create(tvPages) ;
            snode.TextAlign := TTextAlign.Leading ;
            snode.WordWrap := False ;
            // set to the section to get the section description
            MVC.SectionIndex := k ;
            if not IsStringEmpty( MVC.Section.Legend ) then
              desc := MVC.Section.Legend
            else
              desc := Format( '%s %d',
                [_rsrc(GIS_RS_LEGEND_PAG_SECTION), MVC.SectionIndex+1]
              ) ;

            snode.Text := desc ;
            snode.Parent := node ;
          end
          else begin
            cnode := TTreeViewItem.Create(tvPages) ;
            cnode.TextAlign := TTextAlign.Leading ;
            cnode.WordWrap := False ;
            cnode.Text := T_sectionContainer( lstFeatures ).Panel[i].ItemText ;
            cnode.Parent := snode ;
          end ;
        end ;
      end ;
    finally
      MVC.SectionIndex := 0 ;
      tvPages.EndUpdate ;
    end;
  finally
    unlock ;
  end ;
  T_sectionContainer( lstFeatures ).Read ;
  doPageChange( tvPages.Items[0] ) ;
end ;

procedure TGIS_ControlLegendForm.doWizardClick(
  _sender : TObject
) ;
var
  vfrm : TGIS_PvlControlLegendVectorWiz ;
  gfrm : TGIS_PvlControlLegendGridWiz   ;
  typ  : TGIS_ShapeType                 ;
  proc : TGIS_Proc                      ;
begin

  if MVC.IsVector then begin
    vfrm := TGIS_PvlControlLegendVectorWiz.Create( Self ) ;
    MVC.PrepareLayerEx ;

    if Assigned( oLegend ) then begin
      vfrm.UniqueLimit       := oLegend.DialogOptions.VectorWizardUniqueLimit ;
      vfrm.UniqueSearchLimit := oLegend.DialogOptions.VectorWizardUniqueSearchLimit ;
    end
    else begin
      vfrm.UniqueLimit       := 256 ;
      vfrm.UniqueSearchLimit := 16384 ;
    end ;

    if T_scrollablePanel( pnlCurrent ).ItemText =
         _rsrcna( GIS_RS_LEGEND_PAG_MARKER ) then
      typ := TGIS_ShapeType.Point
    else
    if T_scrollablePanel( pnlCurrent ).ItemText =
        _rsrcna( GIS_RS_LEGEND_PAG_LINE   ) then
      typ := TGIS_ShapeType.Arc
    else
    if T_scrollablePanel( pnlCurrent ).ItemText =
        _rsrcna( GIS_RS_LEGEND_PAG_AREA   ) then
      typ := TGIS_ShapeType.Polygon
    else
      typ := TGIS_ShapeType.Unknown ;

    proc := {$IFDEF OXYGENE}TGIS_Proc.create({$ENDIF}
      procedure( _modal_result : TGIS_PvlModalResult )
      var
        i, k  : Integer ;
        node  : TTreeViewItem ;
        snode : TTreeViewItem ;
        cnode : TTreeViewItem ;
      begin
        try
          if _modal_result = TGIS_PvlModalResult.OK then begin
            Lock ;
            try
              for i :=  MVC.SectionCount - 1 downto 0 do
                MVC.SectionDelete ;

              tvPages.BeginUpdate ;
              try
                node := tvPages.Items[GIS_CONTAINER_INDEX] ;
                for i := node.Count - 1 downto 0 do
                  node.RemoveObject( node.Items[i] ) ;

                MVC.SwitchToLayerEx ;

                for k := 0 to MVC.LayerEx.ParamsList.Count - 1 do begin
                  MVC.SectionAdd( False ) ;
                  for i := 0 to T_sectionContainer( lstFeatures ).Count - 1 do begin
                    if i = 0 then begin
                      snode := TTreeViewItem.Create(tvPages) ;
                      snode.TextAlign := TTextAlign.Leading ;
                      snode.WordWrap := False ;
                      // set to the section to get the section description
                      MVC.SectionIndex := k ;
                      if not IsStringEmpty( MVC.Section.Legend ) then
                        snode.Text := MVC.Section.Legend
                      else
                        snode.Text := T_sectionContainer( lstFeatures ).Panel[i].ItemText ;
                      snode.Parent := node ;
                    end
                    else begin
                      cnode := TTreeViewItem.Create(tvPages) ;
                      cnode.TextAlign := TTextAlign.Leading ;
                      cnode.WordWrap := False ;
                      cnode.Text := T_sectionContainer( lstFeatures ).Panel[I].ItemText ;
                      cnode.Parent := snode ;
                    end ;
                  end ;
                end ;
              finally
                MVC.SectionIndex := 0 ;
                tvPages.EndUpdate ;
                tvPages.Repaint ;
              end;
            finally
              unlock ;
              MVC.SwitchToLayer ;
            end ;
            T_sectionContainer( lstFeatures ).Read ;
            doPageChange( tvPages.Items[GIS_CONTAINER_INDEX] ) ;
          end ;
        finally
          MVC.UnPrepareLayerEx ;
        end;
      end
    {$IFDEF OXYGENE}){$ENDIF};

    vfrm.Execute(
      MVC.Layer as TGIS_LayerVector,
      typ,
      MVC.LayerEx.ParamsList,
      pOnHelp,
      proc
    );
  end
  else if MVC.IsGrid then begin

    gfrm := TGIS_PvlControlLegendGridWiz.Create( Self ) ;
    MVC.PrepareLayerEx ;

    proc := {$IFDEF OXYGENE}TGIS_Proc.create({$ENDIF}
      procedure( _modal_result : TGIS_PvlModalResult )
      var
        i, k     : Integer ;
        node     : TTreeViewItem ;
        snode    : TTreeViewItem ;
        cnode    : TTreeViewItem ;
        cur_node : TTreeViewItem ;
      begin
        try
          if _modal_result = TGIS_PvlModalResult.OK then begin
            cur_node := nil ;
            lock ;
            try
              for i :=  MVC.SectionCount - 1 downto 0 do
                MVC.SectionDelete ;

              tvPages.BeginUpdate ;
              try
                node := tvPages.Items[GIS_CONTAINER_INDEX] ;
                for i := node.Count - 1 downto 0 do
                  node.RemoveObject( node.Items[i] ) ;

                MVC.SwitchToLayerEx ;

                for k := 0 to MVC.LayerEx.ParamsList.Count - 1 do begin
                  MVC.SectionAdd( False ) ;
                  for i := 0 to T_sectionContainer( lstFeatures ).Count - 1 do begin
                    if i = 0 then begin
                      snode := TTreeViewItem.Create(tvPages) ;
                      snode.TextAlign := TTextAlign.Leading ;
                      snode.WordWrap := False ;
                      snode.Text := T_sectionContainer( lstFeatures ).Panel[i].ItemText ;
                      snode.Parent := node ;
                    end
                    else begin
                      cnode := TTreeViewItem.Create(tvPages) ;
                      cnode.TextAlign := TTextAlign.Leading ;
                      cnode.WordWrap := False ;
                      cnode.Text := T_sectionContainer( lstFeatures ).Panel[i].ItemText ;
                      cnode.Parent := snode ;

                      if i = 1 then
                        cur_node := cnode ;
                    end ;
                  end ;
                end ;
              finally
                tvPages.EndUpdate ;
                tvPages.Repaint ;
              end;
            finally
              unlock ;
              MVC.SwitchToLayer ;
            end ;
            T_sectionContainer( lstFeatures ).Read ;
            lastNodeIdx := -1 ;
            doPageChange( cur_node ) ;
          end ;
        finally
          MVC.UnPrepareLayerEx ;
        end;
      end
    {$IFDEF OXYGENE}){$ENDIF};
    gfrm.Execute(
      MVC.Layer as TGIS_LayerPixel,
      MVC.LayerEx.Params as TGIS_ParamsSectionPixel,
      pOnHelp,
      proc
    ) ;
  end ;
end ;

class procedure TGIS_ControlLegendForm.ShowLayerProperties(
  const _layer  : TGIS_Layer         ;
  const _legend : TGIS_ControlLegend ;
  const _onhelp : TGIS_HelpEvent     ;
  const _proc   : TProc<TModalResult>
) ;
var
  frm : TGIS_ControlLegendForm ;
  oparent : TComponent ;
begin
  if not Assigned( _layer ) then begin
    _proc( mrNone ) ;
    exit ;
  end ;

  oparent := nil ;
  if Assigned( _legend ) then
    oparent := _legend
  else
  if Assigned( _layer.Viewer ) then
    oparent := TGIS_ViewerWnd( _layer.Viewer.Ref.ViewerParent ) ;

  if not Assigned( oparent ) then
    oparent := Application.MainForm ;

  frm := TGIS_ControlLegendForm.Create( oparent ) ;
  _layer.Open ;
  frm.Execute( _layer, _legend, _onhelp, _proc ) ;
end ;

class procedure TGIS_ControlLegendForm.ShowLayerProperties(
  const _layer  : TGIS_Layer         ;
  const _legend : TGIS_ControlLegend ;
  const _proc   : TProc<TModalResult>
) ;
var
  hlp : TGIS_HelpEvent ;
begin
  hlp := nil ;
  ShowLayerProperties( _layer, _legend, hlp, _proc ) ;
end ;
{$ENDREGION}

{$REGION 'GisShowLayerProperties'}

procedure GisShowLayerProperties(
  const _layer  : TGIS_Layer         ;
  const _legend : TGIS_ControlLegend ;
  const _onhelp : TGIS_HelpEvent     ;
  const _proc   : TProc<TModalResult>
) ;
begin
  TGIS_ControlLegendForm.ShowLayerProperties( _layer, _legend, _onhelp, _proc ) ;
end ;

procedure GisShowLayerProperties(
  const _layer  : TGIS_Layer         ;
  const _legend : TGIS_ControlLegend ;
  const _proc   : TProc<TModalResult>
) ;
begin
  TGIS_ControlLegendForm.ShowLayerProperties( _layer, _legend, _proc ) ;
end ;

{$ENDREGION}

//==================================== END =====================================
end.

