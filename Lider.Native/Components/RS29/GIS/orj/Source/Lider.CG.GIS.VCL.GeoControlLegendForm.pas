// =============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DKv100.1.37476
// (c)2000-2025 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
// ILKER#LIDERYAZILIM.COM-481078-KSVX7UYN-1D12B8B5
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
// =============================================================================
{
  Layer properties legend form control.
}

unit VCL.GisControlLegendForm;
{$HPPEMIT '#pragma link "VCL.GisControlLegendForm"'}

interface

{$INCLUDE GisInclude.inc}

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Types,
  System.Generics.Collections,
  System.UITypes,
  VCL.Graphics,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.Buttons,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.ExtDlgs,
  VCL.ComCtrls,
  VCL.Themes,
  VCL.ImgList,

{$IFDEF GEN_XDK}
  XDK.Splitter,
{$ENDIF}
  GisInterfaces,
  GisTypes,
  GisLayer,
  GisRendererAbstract,
  GisControlLegendFormControler,
  VCL.GisFramework,
  VCL.GisModalForm,
  VCL.GisViewerBmp,
  VCL.GisViewerWnd,
  VCL.GisControlLegend;

type

{$REGION 'TGIS_ControlLegendForm'}
  /// <summary>
  /// Visual form for managing various layer properties.
  /// Use class TGIS_ControlLegendForm.ShowLayerProperties() to call this form directly.
  /// </summary>
  TGIS_ControlLegendForm = class(TGIS_ModalForm)
  private
    MVC: TGIS_ControlLegendFormMVC;
    ImgList: TImageList;
    imgListSclN: TImageList;
    imgListSclD: TImageList;
    bLock: Boolean;
    iBusy: Integer;
    iLastPPI: Integer;
  private
    pnlLeft: TWinControl;
    pnlRight: TWinControl;
    tvPages: TTreeView;
    gisPreview: TGIS_ViewerBmp;
    gisPreviewI: TImage;
    btnAdd: TToolButton;
    btnRemove: TToolButton;
    btnClear: TToolButton;
    btnMoveUp: TToolButton;
    btnMoveDown: TToolButton;
    btnOpen: TToolButton;
    btnSave: TToolButton;
    btnWizard: TButton;
    btnApply: TButton;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    toolButtonsU: TToolBar;
    toolButtonsD: TToolBar;
    tmrShowForm: TTimer;
    tmrUpdate: TTimer;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
  private
    pnlCurrent: TWinControl;
    pnlGeneral: TWinControl;
    pnlSections: TWinControl;
    lstFeatures: TObject;
    lastSection: String;
    lastNodeIdx: Integer;

    lockLevel: Integer;
  private
    oLegend: TGIS_ControlLegend;
    oViewer: IGIS_Viewer;
    oViewerWnd: TGIS_ViewerWnd;
  private
    function ppiFix(const _value: Integer): Integer;
    procedure initSelf;
    procedure initLeft;
    procedure initRight(const _full: Boolean);
    procedure readLayer(const _full: Boolean);
    procedure setSection(const _idx: Integer; const _fea: Boolean);
    procedure fillComboBoxWithFields(const _cmb: TComboBox);
    procedure showPreview(const _show: Boolean);
    procedure lock;
    procedure unlock;
    function isLocked: Boolean;
    procedure waitNoBusy;
    procedure updateStatistics(const _layer: TGIS_Layer);
  private
    procedure btnOKClick(_sender: TObject); override;
    procedure doShowTimer(_sender: TObject);
    procedure doPageChange(_node: TTreeNode);
    procedure doTvPagesOnChange(_sender: TObject; _node: TTreeNode);
    procedure doApplySettings(_sender: TObject);
    procedure doAddClick(_sender: TObject);
    procedure doRemoveClick(_sender: TObject);
    procedure doClearClick(_sender: TObject);
    procedure doMoveUpClick(_sender: TObject);
    procedure doMoveDownClick(_sender: TObject);
    procedure doOpenClick(_sender: TObject);
    procedure doSaveClick(_sender: TObject);
    procedure doWizardClick(_sender: TObject);
    procedure doUpdateTimer(_sender: TObject);
    procedure doSectionsRebuild;

    procedure doChangeDPI;

{$IFDEF LEVEL_RX101_VCL}
    procedure WMDpiChanged(var _message: TWMDpi); message WM_DPICHANGED;
{$ENDIF}
  protected

    /// <inheritdoc/>
    procedure initForm; override;

    /// <inheritdoc/>
    procedure initControls; override;

  protected

    /// <inheritdoc/>
    procedure DoShow; override;

  public

    /// <summary>
    /// See documentation for TCustomForm in Delphi help.
    /// </summary>
    destructor Destroy; override;

  protected

    /// <summary>
    /// See documentation for TCustomForm in Delphi help.
    /// </summary>
    procedure Paint; override;

  public

    /// <summary>
    /// Execute dialog on a given layer.
    /// </summary>
    /// <param name="_layer">
    /// layer to be attached
    /// </param>
    /// <param name="_legend">
    /// legend control to be used; can be nil
    /// </param>
    /// <param name="_onhelp">
    /// help notification function; if assigned the help button
    /// will be visible and help support will be enabled;
    /// </param>
    /// <returns>
    /// Modal result.
    /// </returns>
    function Execute(const _layer: TGIS_Layer;
      const _legend: TGIS_ControlLegend; const _onhelp: TGIS_HelpEvent)
      : Integer; overload;

    /// <summary>
    /// Execute dialog on a given layer.
    /// </summary>
    /// <param name="_layer">
    /// layer to be attached
    /// </param>
    /// <param name="_legend">
    /// legend control to be used; can be nil
    /// </param>
    /// <returns>
    /// Modal result.
    /// </returns>
    function Execute(const _layer: TGIS_Layer;
      const _legend: TGIS_ControlLegend): Integer; overload;

    /// <summary>
    /// Show layer properties dialog for a given layer.
    /// </summary>
    /// <param name="_layer">
    /// layer to be attached
    /// </param>
    /// <param name="_legend">
    /// legend control to be used; can be nil
    /// </param>
    /// <param name="_onhelp">
    /// help notification function; if assigned the help button
    /// will be visible and help support will be enabled;
    /// </param>
    /// <returns>
    /// Value returned by ShowModal function.
    /// </returns>
    class function ShowLayerProperties(const _layer: TGIS_Layer;
      const _legend: TGIS_ControlLegend; const _onhelp: TGIS_HelpEvent)
      : Integer; overload;

    /// <summary>
    /// Show layer properties dialog for a given layer.
    /// </summary>
    /// <param name="_layer">
    /// layer to be attached
    /// </param>
    /// <param name="_legend">
    /// legend control to be used; can be nil
    /// </param>
    /// <returns>
    /// Value returned by ShowModal function.
    /// </returns>
    class function ShowLayerProperties(const _layer: TGIS_Layer;
      const _legend: TGIS_ControlLegend): Integer; overload;

  end;

{$ENDREGION}
{$REGION 'GisShowLayerProperties'}

  /// <summary>
  /// Show layer properties dialog for a given layer.
  /// </summary>
  /// <param name="_layer">
  /// layer to be attached
  /// </param>
  /// <param name="_legend">
  /// legend control to be used; can be nil
  /// </param>
  /// <param name="_onhelp">
  /// help notification function; if assigned the help button
  /// will be visible and help support will be enabled;
  /// </param>
  /// <returns>
  /// Value returned by ShowModal function.
  /// </returns>
function GisShowLayerProperties(const _layer: TGIS_Layer;
  const _legend: TGIS_ControlLegend; const _onhelp: TGIS_HelpEvent)
  : Integer; overload;
{$IFNDEF GENDOC} deprecated; {$ENDIF}
/// <summary>
/// Show layer properties dialog for a given layer.
/// </summary>
/// <param name="_layer">
/// layer to be attached
/// </param>
/// <param name="_legend">
/// legend control to be used; can be nil
/// </param>
/// <returns>
/// Value returned by ShowModal function.
/// </returns>
function GisShowLayerProperties(const _layer: TGIS_Layer;
  const _legend: TGIS_ControlLegend): Integer; overload;
{$IFNDEF GENDOC} deprecated; {$ENDIF}
{$ENDREGION}

// ##############################################################################
implementation

{$R GisControlLegendForm_16x16.RES}
{$IFDEF DCC}

uses
  System.Math,

  GisRtl,
  GisTypesUI,
  GisClasses,
  GisSymbol,
  GisFunctions,
  GisCsBase,
  GisCSSystems,
  GisParams,
  GisResource,
  GisSqlQuery,
  GisInternals,
  GisLayerVector,
  GisLayerPixel,

  VCL.GisRendererGdiPlus,
  VCL.GisControlVarious,
  VCL.GisValueValidatorHelper,
  VCL.GisControlHelper,
  VCL.GisControlColor,
  VCL.GisControlCsSystem,
  PVL.GisControlLegendVectorWiz,
  PVL.GisControlLegendGridWiz,
  VCL.GisControlStatistics,
  VCL.GisLineSymbolEditor,

  VCL.GisPvl,
  PVL.GisPvl,
  PVL.GisControlSymbology,
  PVL.GisControlBitmap,
  PVL.GisControlSymbologyLibrary,
  PVL.GisControlFieldFactor,
  PVL.GisControlColor,
  PVL.GisControlSizeForm;
{$ENDIF}
{$REGION 'Const'}

const

  WIDTH_NARROW: Integer = 304;
  WIDTH_2COL: Integer = 182;
  WIDTH_3COL: Integer = 80 + 27;
  WIDTH_NORMAL: Integer = 380;
  LEFT_2COL_1: Integer = 182;
  LEFT_2COL_2: Integer = 216;
  LEFT_3COL_1: Integer = 16;
  LEFT_3COL_2: Integer = 112 + 24;
  LEFT_3COL_3: Integer = 208 + 48;
  LEFT_3COL_4: Integer = 326;

  DIP_TO_TWIPS = 15;

  MAX_PREVIEW_SIZE_LINE = 16 * DIP_TO_TWIPS;
  MAX_PREVIEW_SIZE_OUTLINE = 12 * DIP_TO_TWIPS;
  MAX_PREVIEW_SIZE_MAKER = 32 * DIP_TO_TWIPS;
  MAX_PREVIEW_SIZE_FONT = 32 * DIP_TO_TWIPS;
  MAX_PREVIEW_SIZE_PATTERNSYMBOL = 24 * DIP_TO_TWIPS;
  MAX_PREVIEW_SIZE_RENDERER = 12 * DIP_TO_TWIPS;

  GIS_CONTAINER_INDEX: Integer = 1;
{$ENDREGION}
{$REGION 'local methods'}

procedure local_SetEnabledOnControl(const _control: TWinControl;
  const _enable: Boolean);
var
  c: TControl;
  i: Integer;
begin
  if _enable then
    _control.Enabled := _enable;

  for i := 0 to _control.ControlCount - 1 do
  begin
    c := _control.Controls[i];
    c.Enabled := _enable;
  end;

  if not _enable then
    _control.Enabled := _enable;
end;
{$ENDREGION}
{$REGION 'T_speedButton'}

type
  T_speedButton = class(TSpeedButton)
  protected
    procedure Paint; override;
  public
    Images: TImageList;
    DisabledImages: TImageList;
    ImageIndex: Integer;
  end;
{$ENDREGION}
{$REGION 'T_fieldPanel'}

type

  T_fieldPanel = class(TPanel)
  private
    lblField: TLabel;
  private
    function fget_Field: String;
    procedure fset_Field(const _name: String);
  protected
    procedure SetEnabled(Value: Boolean); override;
  public
    constructor Create(_owner: TComponent); override;
  public
    property Field: String read fget_Field write fset_Field;
  end;
{$ENDREGION}
{$REGION 'T_scrollablePanel'}

  T_scrollablePanel = class(TScrollBox)
  private
    pnlPanel: TScrollBox;
    vShift: Integer;
    hShift: Integer;
    iBlockUpdates: Integer;
  private
    FItemText: String;
  protected
    oParentWindow: TGIS_ControlLegendForm;
  protected
    function fget_HasPreview: Boolean; virtual;
  protected
    procedure init; virtual;
    procedure lockUpdates;
    procedure unlockUpdates;
    function blockedUpdates: Boolean;
  protected
    function ppiFix(const _value: Integer): Integer;
    function doCustomSize(_sender: TObject; _value: String): String;
    function doCustomRWUnits(_sender: TObject; _value: String): String;
    function doCustomRotation(_sender: TObject; _value: String): String;
    function doCustomPattern(_sender: TObject; _value: String): String;
    function doCustomShield(_sender: TObject; _value: String): String;
    function doCustomColor(_sender: TObject; _value: String): String;

    function doGetBitmapAreaFill(_sender: TObject; _value: String;
      _width: Integer; _height: Integer): TGIS_Bitmap;
    function doGetBitmapMarkerStyle(_sender: TObject; _value: String;
      _width: Integer; _height: Integer): TGIS_Bitmap;

    function doGetBitmapAreaOutline(_sender: TObject; _value: String;
      _width: Integer; _height: Integer): TGIS_Bitmap;
    function doGetBitmapShield(_sender: TObject; _value: String;
      _width: Integer; _height: Integer): TGIS_Bitmap;

    procedure doMouseWheel(_sender: TObject; _shift: TShiftState;
      _wheelDelta: Integer; _mousePos: TPoint; var _handled: Boolean);
  public
    procedure Read; virtual;
    procedure Write; virtual;
    procedure PreparePreview(const _viewer: IGIS_Viewer); virtual;
    procedure UpdatePreview; virtual;
  public
    constructor Create(_owner: TWinControl;
      const _pwnd: TGIS_ControlLegendForm); reintroduce;
  public
    property Panel: TScrollBox read pnlPanel;
    property ItemText: String read FItemText write FItemText;
    property HasPreview: Boolean read fget_HasPreview;
  end;
{$ENDREGION}
{$REGION 'T_sectionContainer'}

  T_sectionContainer = class
  private
    oParentWindow: TGIS_ControlLegendForm;
    lstFeatures: TObjectList<T_scrollablePanel>;
  private
    function fget_Count: Integer;
    function fget_Panel(const _i: Integer): T_scrollablePanel;
  public
    procedure initVector;
    procedure initPixel;
    procedure initGrid;
    procedure initDefault;
  public
    constructor Create(const _pwnd: TGIS_ControlLegendForm);
  public
    destructor Destroy; override;
  public
    procedure ReadSection;
    procedure Read;
    procedure Write;
    function FindPanel(const _text: String): T_scrollablePanel;
  public
    property Count: Integer read fget_Count;
    property Panel[const _i: Integer]: T_scrollablePanel read fget_Panel;
  end;
{$ENDREGION}
{$REGION 'T_panelGeneral'}

  T_panelGeneral = class(T_scrollablePanel)
  private
    MVC: TGIS_ControlLegendFormMVC_General;
  private // group boxes
    gpbParameters: TGroupBox;
    gpbInfo: TGroupBox;
    gpbPainting: TGroupBox;
    gpbThreshold: TGroupBox;
    gpbAggregation: TGroupBox;
  private // Parameters
    lblPath: TLabel;
    edtPath: TEdit;
    lblName: TLabel;
    lblCaption: TLabel;
    edtName: TEdit;
    edtCaption: TEdit;
    lblCS: TLabel;
    edtCS: TEdit;
    btnCS: TButton;
    ckbUseConfig: TCheckBox;
    ckbBasemap: TCheckBox;
    ckbCachedPaint: TCheckBox;
    ckbIgnoreShapeParams: TCheckBox;
    ckbMultipassRendering: TCheckBox;
    lblTransparency: TLabel;
    lblInterpretation: TLabel;
    cmbTransparency: TComboBox;
    vvcmbTransparency: IGIS_ValueValidator;
    cmbInterpretation: TComboBox;
    lblScope: TLabel;
    cmbScope: TComboBox;
  private // Info
    lblFileInformation: TLabel;
    txbFileInformation: TMemo;
    lblUserComments: TLabel;
    txbUserComments: TMemo;
    lblCodePage: TLabel;
    edtCodePage: TEdit;
    vvedtCodePage: IGIS_ValueValidator;
  private // Aggregation
    lblAggMethod: TLabel;
    cmbAggMethod: TComboBox;
    lblAggRadius: TLabel;
    cmbAggRadius: TGIS_SizeComboBox;
    lblAggThreshold: TLabel;
    edtAggThreshold: TEdit;
    vvedtAggThreshold: IGIS_ValueValidator;
  protected
    function fget_HasPreview: Boolean; override;
  private
    procedure initParameters;
    procedure initInfo;
  protected
    procedure init; override;
  public
    procedure Read; override;
    procedure Write; override;
  protected
    procedure doCallback(_sender: TObject; _code: Integer);
    procedure doCSClick(_sender: TObject);
    procedure doControlChange(_sender: TObject);
    procedure doAggregateChange(_sender: TObject);
  end;
{$ENDREGION}
{$REGION 'T_panel3D'}

  T_panel3D = class(T_scrollablePanel)
  private
    MVC: TGIS_ControlLegendFormMVC_3D;
  private
    rbnAs2D: TRadioButton;
    rbnAsDEM: TRadioButton;
    rbnAs3D: TRadioButton;
  private
    lblNormalizedZ: TLabel;
    lblNormalizedM: TLabel;
    cmbNormalizedZ: TComboBox;
    cmbNormalizedM: TComboBox;
    lblScaleZ: TLabel;
    lblScaleM: TLabel;
    speScaleZ: TEdit;
    speScaleM: TEdit;
    vvspeScaleZ: IGIS_ValueValidator;
    vvspeScaleM: IGIS_ValueValidator;
    lblFalseZ: TLabel;
    lblFalseM: TLabel;
    cmbFalseZ: TGIS_SizeComboBox;
    cmbFalseM: TGIS_SizeComboBox;
    lblAdjustZ: TLabel;
    lblAdjustBasement: TLabel;
    cmbAdjustZ: TComboBox;
    cmbAdjustBasement: TComboBox;
    hasDEM: Boolean;
  protected
    function fget_HasPreview: Boolean; override;
  protected
    procedure init; override;
  public
    procedure Read; override;
    procedure Write; override;
  protected
    procedure doCallback(_sender: TObject; _code: Integer);
    procedure doAs2DClick(_sender: TObject);
    procedure doAsDEMClick(_sender: TObject);
    procedure doAs3DClick(_sender: TObject);
  end;
{$ENDREGION}
{$REGION 'T_panelSections'}

  T_panelSections = class(T_scrollablePanel)
  protected
    gpbSection: TGroupBox;
    lvSections: TListBox;
  protected
    function fget_HasPreview: Boolean; override;

    procedure lvSectionsDblClick(_sender: TObject);
  protected
    procedure init; override;
  public
    procedure Prepare;
    procedure PreparePreview(const _viewer: IGIS_Viewer); override;
    procedure UpdatePreview; override;

  public
    procedure Read; override;
    procedure Write; override;
  end;
{$ENDREGION}
{$REGION 'T_panelSection'}

  T_panelSection = class(T_scrollablePanel)
  protected
    MVC: TGIS_ControlLegendFormMVC_Section;
  protected
    gpbSection: TGroupBox;
    ckbVisible: TCheckBox;
    lblMinScale: TLabel;
    lblMaxScale: TLabel;
    btnMinScaleCur: TButton;
    btnMinScaleClr: T_speedButton;
    cmbMinScale: TComboBox;
    btnMaxScaleCur: TButton;
    btnMaxScaleClr: T_speedButton;
    cmbMaxScale: TComboBox;
  protected
    function fget_HasPreview: Boolean; override;
  protected
    procedure init; override;
    procedure updateNode; virtual;
  public
    procedure Read; override;
    procedure Write; override;
  protected
    procedure doCallback(_sender: TObject; _code: Integer); virtual;
    procedure doVisibleClick(_sender: TObject);
    procedure doMinScaleClick(_sender: TObject);
    procedure doMinScaleClear(_sender: TObject);
    procedure doMinScaleChange(_sender: TObject);
    procedure doMaxScaleClick(_sender: TObject);
    procedure doMaxScaleClear(_sender: TObject);
    procedure doMaxScaleChange(_sender: TObject);
  end;
{$ENDREGION}
{$REGION 'T_panelSectionVector'}

  T_panelSectionVector = class(T_panelSection)
  protected
    lblQuery: TLabel;
    cmbQuery: TComboBox;
    lblLegend: TLabel;
    edtLegend: TEdit;
    wasLegendEdited: Boolean;
  protected
    procedure init; override;
    procedure updateNode; override;
    procedure updateSectionNode;
  public
    procedure Read; override;
    procedure Write; override;
  protected
    procedure doCallback(_sender: TObject; _code: Integer); override;
    procedure doQueryChange(_sender: TObject);
    procedure doLegendChange(_sender: TObject);
  end;
{$ENDREGION}
{$REGION 'T_panelRenderer'}

  T_panelRenderer = class(T_scrollablePanel)
  private
    MVC: TGIS_ControlLegendFormMVC_Renderer;
  private // group boxes
    gpbRender: TGroupBox;
    gpbFirst: TGroupBox;
    gpbSecond: TGroupBox;
    lblExpression: TLabel;
    lblRounding: TLabel;
    cmbExpression: TComboBox;
    speRounding: TEdit;
    vvspeRounding: IGIS_ValueValidator;
  private // First
    lblNumOfZones1: TLabel;
    lblMinVal1: TLabel;
    lblMaxVal1: TLabel;
    speNumOfZones1: TEdit;
    edtMinVal1: TEdit;
    edtMaxVal1: TEdit;
    vvspeNumOfZones1: IGIS_ValueValidator;
    vvedtMinVal1: IGIS_ValueValidator;
    vvedtMaxVal1: IGIS_ValueValidator;
    lblStartColor1: TLabel;
    lblEndColor1: TLabel;
    lblDefaultColor1: TLabel;
    cmbStartColor1: TGIS_ColorComboBox;
    cmbEndColor1: TGIS_ColorComboBox;
    cmbDefaultColor1: TGIS_ColorComboBox;
    lblStartSize1: TLabel;
    lblEndSize1: TLabel;
    lblDefaultSize1: TLabel;
    cmbStartSize1: TGIS_SizeComboBox;
    cmbEndSize1: TGIS_SizeComboBox;
    cmbDefaultSize1: TGIS_SizeComboBox;
  private // Second
    lblNumOfZones2: TLabel;
    lblMinVal2: TLabel;
    lblMaxVal2: TLabel;
    speNumOfZones2: TEdit;
    edtMinVal2: TEdit;
    edtMaxVal2: TEdit;
    vvspeNumOfZones2: IGIS_ValueValidator;
    vvedtMinVal2: IGIS_ValueValidator;
    vvedtMaxVal2: IGIS_ValueValidator;
    lblStartColor2: TLabel;
    lblEndColor2: TLabel;
    lblDefaultColor2: TLabel;
    cmbStartColor2: TGIS_ColorComboBox;
    cmbEndColor2: TGIS_ColorComboBox;
    cmbDefaultColor2: TGIS_ColorComboBox;
    lblStartSize2: TLabel;
    lblEndSize2: TLabel;
    lblDefaultSize2: TLabel;
    cmbStartSize2: TGIS_SizeComboBox;
    cmbEndSize2: TGIS_SizeComboBox;
    cmbDefaultSize2: TGIS_SizeComboBox;
  protected
    function fget_HasPreview: Boolean; override;
  private
    procedure initSelf;
    procedure initFirst;
    procedure initSecond;
  private
    procedure enableFirst(_enable: Boolean);
    procedure enableSecond(_enable: Boolean);
  protected
    procedure init; override;
  public
    procedure Read; override;
    procedure Write; override;
  protected
    procedure doCallback(_sender: TObject; _code: Integer);
    procedure doExpressionChange(_sender: TObject);
    procedure doNumberOfZones1Change(_sender: TObject);
    procedure doNumberOfZones2Change(_sender: TObject);
    procedure doControlChange(_sender: TObject);
  end;
{$ENDREGION}
{$REGION 'T_panelMarker'}

  T_panelMarker = class(T_scrollablePanel)
  private
    MVC: TGIS_ControlLegendFormMVC_Marker;
  private
    ckbLegend: TCheckBox;
    gpbMarker: TGroupBox;
    gpbOutline: TGroupBox;
  private // Marker
    lblStyle: TLabel;
    cmbStyle: TGIS_SymbolComboBox;
    lblColor: TLabel;
    cmbColor: TGIS_ColorComboBox;
    lblSize: TLabel;
    cmbSize: TGIS_SizeComboBox;
    lblPattern: TLabel;
    cmbPattern: TGIS_PatternComboBox;
    lblSymbolRotate: TLabel;
    cmbSymbolRotate: TGIS_RotationComboBox;
  private // Outline
    lblOStyle: TLabel;
    cmbOStyle: TGIS_PenStyleComboBox;
    lblOColor: TLabel;
    cmbOColor: TGIS_ColorComboBox;
    lblOWidth: TLabel;
    cmbOWidth: TGIS_SizeComboBox;
    lblOPattern: TLabel;
    cmbOPattern: TGIS_PatternComboBox;
  private // Smart size
    lblSSSize: TLabel;
    cmbSSSize: TGIS_SizeComboBox;
  protected
    function fget_HasPreview: Boolean; override;
  private
    procedure initSelf;
    procedure initMarker;
    procedure initOutline;

  protected
    procedure init; override;
  public
    procedure Read; override;
    procedure Write; override;
    procedure PreparePreview(const _viewer: IGIS_Viewer); override;
    procedure UpdatePreview; override;
  protected
    procedure doCallback(_sender: TObject; _code: Integer);
    procedure doSmartSizeFieldChange(_sender: TObject);
    procedure doControlChange(_sender: TObject);
    procedure doPatternChange(_sender: TObject);
    procedure doOPatternChange(_sender: TObject);

  end;
{$ENDREGION}
{$REGION 'T_panelLine'}

  T_panelLine = class(T_scrollablePanel)
  private
    MVC: TGIS_ControlLegendFormMVC_Line;
  private
    ckbLegend: TCheckBox;
    gpbLine: TGroupBox;
    gpbOutline: TGroupBox;
  private // Line
    lblStyle: TLabel;
    cmbStyle: TGIS_PenStyleComboBox;
    lblColor: TLabel;
    cmbColor: TGIS_ColorComboBox;
    lblWidth: TLabel;
    cmbWidth: TGIS_SizeComboBox;
    lblPattern: TLabel;
    cmbPattern: TGIS_PatternComboBox;
    lblSymbolGap: TLabel;
    cmbSymbolGap: TGIS_SizeComboBox;
    lblSymbolRotate: TLabel;
    cmbSymbolRotate: TGIS_RotationComboBox;
  private // Outline
    lblOStyle: TLabel;
    cmbOStyle: TGIS_PenStyleComboBox;
    lblOColor: TLabel;
    cmbOColor: TGIS_ColorComboBox;
    lblOWidth: TLabel;
    cmbOWidth: TGIS_SizeComboBox;
    lblOPattern: TLabel;
    cmbOPattern: TGIS_PatternComboBox;
  private // Smart size
    lblSSSize: TLabel;
    cmbSSSize: TGIS_SizeComboBox;
  protected
    function fget_HasPreview: Boolean; override;
  private
    procedure initSelf;
    procedure initLine;
    procedure initOutline;
  protected
    procedure init; override;
  public
    procedure Read; override;
    procedure Write; override;
    procedure PreparePreview(const _viewer: IGIS_Viewer); override;
    procedure UpdatePreview; override;

  protected
    procedure doCallback(_sender: TObject; _code: Integer);
    procedure doSmartSizeFieldChange(_sender: TObject);
    procedure doControlChange(_sender: TObject);
    procedure doPatternChange(_sender: TObject);
    procedure doOPatternChange(_sender: TObject);
  end;
{$ENDREGION}
{$REGION 'T_panelArea'}

  T_panelArea = class(T_scrollablePanel)
  private
    MVC: TGIS_ControlLegendFormMVC_Area;
  private
    ckbLegend: TCheckBox;
    gpbArea: TGroupBox;
    gpbOutline: TGroupBox;
  private // Area
    lblPattern: TLabel;
    lblColor: TLabel;
    cmbPattern: TGIS_PatternComboBox;
    cmbColor: TGIS_ColorComboBox;
    lblSymbolGap: TLabel;
    lblSymbolRotate: TLabel;
    cmbSymbolGap: TGIS_SizeComboBox;
    cmbSymbolRotate: TGIS_RotationComboBox;
    lblSymbolSize: TLabel;
    cmbSymbolSize: TGIS_SizeComboBox;
  private // Outline
    lblOStyle: TLabel;
    cmbOStyle: TGIS_PenStyleComboBox;
    lblOColor: TLabel;
    cmbOColor: TGIS_ColorComboBox;
    lblOWidth: TLabel;
    cmbOWidth: TGIS_SizeComboBox;
    lblOPattern: TLabel;
    cmbOPattern: TGIS_PatternComboBox;
    lblOSymbolGap: TLabel;
    cmbOSymbolGap: TGIS_SizeComboBox;
    lblOSymbolRotate: TLabel;
    cmbOSymbolRotate: TGIS_RotationComboBox;
  private // Smart size
    lblSSSize: TLabel;
    cmbSSSize: TGIS_SizeComboBox;
  protected
    function fget_HasPreview: Boolean; override;
  private
    procedure initSelf;
    procedure initArea;
    procedure initOutline;
  protected
    procedure init; override;
  public
    procedure Read; override;
    procedure Write; override;
    procedure PreparePreview(const _viewer: IGIS_Viewer); override;
    procedure UpdatePreview; override;
  protected
    procedure doCallback(_sender: TObject; _code: Integer);
    procedure doSmartSizeFieldChange(_sender: TObject);
    procedure doControlChange(_sender: TObject);
    procedure doPatternChange(_sender: TObject);
    procedure doOPatternChange(_sender: TObject);
  end;
{$ENDREGION}
{$REGION 'T_panelLabel'}

  T_panelLabel = class(T_scrollablePanel)
  private
    MVC: TGIS_ControlLegendFormMVC_Label;
  private
    ckbVisible: TCheckBox;
    ckbLegend: TCheckBox;
    ckbPAvoidOverlapping: TCheckBox;
    ckbPAvoidDuplicates: TCheckBox;
    gpbField: TGroupBox;
    gpbLabel: TGroupBox;
    gpbFont: TGroupBox;
    gpbOutline: TGroupBox;
    gpbPosition: TGroupBox;
  private // Label
    lblFontName: TLabel;
    cmbFontName: TGIS_FontNameComboBox;
    lblFontSize: TLabel;
    cmbFontSize: TGIS_SizeComboBox;
    lblFontColor: TLabel;
    cmbFontColor: TGIS_ColorComboBox;
    lblWidth: TLabel;
    lblHeight: TLabel;
    lblColor: TLabel;
    cmbWidth: TGIS_SizeComboBox;
    cmbHeight: TGIS_SizeComboBox;
    cmbColor: TGIS_ColorComboBox;
    lblPattern: TLabel;
    cmbPattern: TGIS_PatternComboBox;
    lblShield: TLabel;
    cmbShield: TGIS_ShieldComboBox;
    btnFont: TGIS_FontButton;
    cmbValue: TGIS_FieldValueComboBox;
    dlgFont: TFontDialog;
    ckbFontStyleBold: TCheckBox;
    ckbFontStyleItalic: TCheckBox;
    ckbFontStyleUnderline: TCheckBox;
    ckbFontStyleStrikeout: TCheckBox;
    ckbFontStyleShadow: TCheckBox;
  private // Outline
    lblOStyle: TLabel;
    cmbOStyle: TGIS_PenStyleComboBox;
    lblOColor: TLabel;
    cmbOColor: TGIS_ColorComboBox;
    lblOWidth: TLabel;
    cmbOWidth: TGIS_SizeComboBox;
    lblOPattern: TLabel;
    cmbOPattern: TGIS_PatternComboBox;
  private // Smart size
    lblSSSize: TLabel;
    cmbSSSize: TGIS_SizeComboBox;
  private // Position
    lblPPosition: TLabel;
    fldPPosition: T_fieldPanel;
    fbnPPosition: TGIS_FieldButton;
    btnPTopLeft: T_speedButton;
    btnPTopCenter: T_speedButton;
    btnPTopRight: T_speedButton;
    btnPCenterLeft: T_speedButton;
    btnPCenterCenter: T_speedButton;
    btnPCenterRight: T_speedButton;
    btnPBottomLeft: T_speedButton;
    btnPBottomCenter: T_speedButton;
    btnPBottomRight: T_speedButton;
    ckbPFlow: TCheckBox;
    lblPAlignment: TLabel;
    lblPRotation: TLabel;
    cmbPAlignment: TComboBox;
    cmbPRotation: TGIS_RotationComboBox;
  protected
    function fget_HasPreview: Boolean; override;
  private
    procedure initSelf;
    procedure initLabel;
    procedure initFont;
    procedure initOutline;
    procedure initPosition;
  protected
    procedure init; override;
  public
    procedure Read; override;
    procedure Write; override;
    procedure PreparePreview(const _viewer: IGIS_Viewer); override;
    procedure UpdatePreview; override;
  private
    procedure actBtnPositionClick(_sender: TObject);
  protected
    procedure doCallback(_sender: TObject; _code: Integer);
    procedure doFieldChange(_sender: TObject);
    procedure doSmartSizeFieldChange(_sender: TObject);
    procedure doPositionExNotify(_sender: TObject);
    procedure doAlignmentChange(_sender: TObject);
    procedure doControlChange(_sender: TObject);
    procedure doShadowChange(_sender: TObject);
    procedure doOPatternChange(_sender: TObject);
    procedure doPatternChange(_sender: TObject);
    procedure doShieldChange(_sender: TObject);
  end;
{$ENDREGION}
{$REGION 'T_panelChart'}

  T_panelChart = class(T_scrollablePanel)
  private
    MVC: TGIS_ControlLegendFormMVC_Chart;
  private
    ckbLegend: TCheckBox;
    gpbChart: TGroupBox;
    gpbValues: TGroupBox;
  private // Chart
    lblStyle: TLabel;
    lblSize: TLabel;
    cmbStyle: TComboBox;
    cmbSize: TGIS_SizeComboBox;
    lblMinVal: TLabel;
    lblMaxVal: TLabel;
    edtMinVal: TEdit;
    edtMaxVal: TEdit;
  private // Values
    lblValue: TLabel;
    lblLegend: TLabel;
    lblVal1: TLabel;
    cmbVal1: TComboBox;
    edtVal1: TEdit;
    pnlVal1: TGIS_ColorPreview;
    lblVal2: TLabel;
    cmbVal2: TComboBox;
    edtVal2: TEdit;
    pnlVal2: TGIS_ColorPreview;
    lblVal3: TLabel;
    cmbVal3: TComboBox;
    edtVal3: TEdit;
    pnlVal3: TGIS_ColorPreview;
    lblVal4: TLabel;
    cmbVal4: TComboBox;
    edtVal4: TEdit;
    pnlVal4: TGIS_ColorPreview;
    lblVal5: TLabel;
    cmbVal5: TComboBox;
    edtVal5: TEdit;
    pnlVal5: TGIS_ColorPreview;
    lblVal6: TLabel;
    cmbVal6: TComboBox;
    edtVal6: TEdit;
    pnlVal6: TGIS_ColorPreview;
    lblVal7: TLabel;
    cmbVal7: TComboBox;
    edtVal7: TEdit;
    pnlVal7: TGIS_ColorPreview;
    lblVal8: TLabel;
    cmbVal8: TComboBox;
    edtVal8: TEdit;
    pnlVal8: TGIS_ColorPreview;
  private
    wasLegend1Edited: Boolean;
    wasLegend2Edited: Boolean;
    wasLegend3Edited: Boolean;
    wasLegend4Edited: Boolean;
    wasLegend5Edited: Boolean;
    wasLegend6Edited: Boolean;
    wasLegend7Edited: Boolean;
    wasLegend8Edited: Boolean;
  protected
    function fget_HasPreview: Boolean; override;
  private
    procedure initSelf;
    procedure initChart;
    procedure initValues;
  protected
    procedure init; override;
  public
    procedure Read; override;
    procedure Write; override;
    procedure PreparePreview(const _viewer: IGIS_Viewer); override;
    procedure UpdatePreview; override;
  protected
    procedure doCallback(_sender: TObject; _code: Integer);
    procedure doStyleChange(_sender: TObject);
    procedure doSizeUseRenderer(_sender: TObject);
    procedure doValue1Change(_sender: TObject);
    procedure doLegend1Change(_sender: TObject);
    procedure doValue2Change(_sender: TObject);
    procedure doLegend2Change(_sender: TObject);
    procedure doValue3Change(_sender: TObject);
    procedure doLegend3Change(_sender: TObject);
    procedure doValue4Change(_sender: TObject);
    procedure doLegend4Change(_sender: TObject);
    procedure doValue5Change(_sender: TObject);
    procedure doLegend5Change(_sender: TObject);
    procedure doValue6Change(_sender: TObject);
    procedure doLegend6Change(_sender: TObject);
    procedure doValue7Change(_sender: TObject);
    procedure doLegend7Change(_sender: TObject);
    procedure doValue8Change(_sender: TObject);
    procedure doLegend8Change(_sender: TObject);
    procedure doControlChange(_sender: TObject);
  end;
{$ENDREGION}
{$REGION 'T_panelPixel'}

  T_panelPixel = class(T_scrollablePanel)
  private
    MVC: TGIS_ControlLegendFormMVC_Pixel;
  private
    ckbLegend: TCheckBox;
    gpbBands: TGroupBox;
    gpbColors: TGroupBox;
    lblRed: TLabel;
    lblGreen: TLabel;
    lblBlue: TLabel;
    speRed: TEdit;
    speGreen: TEdit;
    speBlue: TEdit;
    vvspeRed: IGIS_ValueValidator;
    vvspeGreen: IGIS_ValueValidator;
    vvspeBlue: IGIS_ValueValidator;
    lblBrightness: TLabel;
    lblContrast: TLabel;
    speBrightness: TEdit;
    speContrast: TEdit;
    vvspeBrightness: IGIS_ValueValidator;
    vvspeContrast: IGIS_ValueValidator;
    lblRedBand: TLabel;
    lblGreenBand: TLabel;
    lblBlueBand: TLabel;
    cmbRedBand: TComboBox;
    cmbGreenBand: TComboBox;
    cmbBlueBand: TComboBox;
    lblAlphaBand: TLabel;
    cmbAlphaBand: TComboBox;
    lblPage: TLabel;
    cmbPage: TComboBox;
    ckbInversion: TCheckBox;
    ckbGrayscale: TCheckBox;
    ckbHistogram: TCheckBox;
    ckbContrastEnhanced: TCheckBox;
    btnReset: TButton;
    gpbTransparency: TGroupBox;
    pnlColorFrom: TGIS_ColorPreview;
    pnlColorTo: TGIS_ColorPreview;
    lstZones: TListBox;
    btnAdd: T_speedButton;
    btnRemove: T_speedButton;
    btnClear: T_speedButton;
  protected
    function fget_HasPreview: Boolean; override;
  private
    procedure initSelf;
  protected
    procedure init; override;
  public
    procedure Read; override;
    procedure Write; override;
    procedure PreparePreview(const _viewer: IGIS_Viewer); override;
    procedure UpdatePreview; override;
  protected
    procedure doCallback(_sender: TObject; _code: Integer);
    procedure doReset(_sender: TObject);
    procedure doControlChange(_sender: TObject);
    procedure doZonesChange(_sender: TObject);
    procedure lstZoneDrawItem(_ctrl: TWinControl; _idx: Integer; _rect: TRect;
      _state: TOwnerDrawState);
    procedure lstZoneClick(_sender: TObject);
    procedure doAddClick(_sender: TObject);
    procedure doDeleteClick(_sender: TObject);
    procedure doClearClick(_sender: TObject);
    procedure doUpdate(_sender: TObject);
  end;
{$ENDREGION}
{$REGION 'T_panelGrid'}

  T_panelGrid = class(T_scrollablePanel)
  private
    MVC: TGIS_ControlLegendFormMVC_Pixel;
  private
    ckbLegend: TCheckBox;
    gpbGridd: TGroupBox;
    gpbThreshold: TGroupBox;
    gpbRamp: TGroupBox;
    lblMin: TLabel;
    lblMax: TLabel;
    lblLegend: TLabel;
    pnlColor: TGIS_ColorPreview;
    edtMin: TEdit;
    edtMax: TEdit;
    vvedtMin: IGIS_ValueValidator;
    vvedtMax: IGIS_ValueValidator;
    edtLegend: TEdit;
    btnAdd: T_speedButton;
    btnRemove: T_speedButton;
    btnClear: T_speedButton;
    lstGrid: TListBox;
    ckbShadow: TCheckBox;
    lblGridBand: TLabel;
    cmbGridBand: TComboBox;
    lblHeightMin: TLabel;
    lblHeightMax: TLabel;
    edtHeightMin: TEdit;
    edtHeightMax: TEdit;
    vvedtHeightMin: IGIS_ValueValidator;
    vvedtHeightMax: IGIS_ValueValidator;
    lblThresholdMin: TLabel;
    lblThresholdMax: TLabel;
    edtThresholdMin: TEdit;
    edtThresholdMax: TEdit;
    vvedtThresholdMin: IGIS_ValueValidator;
    vvedtThresholdMax: IGIS_ValueValidator;
    ckbAntialias: TCheckBox;
    lockChange: Boolean;
    ckbGridSmoothColors: TCheckBox;
  protected
    function fget_HasPreview: Boolean; override;
  private
    procedure initSelf;
  protected
    procedure init; override;
  public
    procedure Read; override;
    procedure Write; override;
    procedure PreparePreview(const _viewer: IGIS_Viewer); override;
    procedure UpdatePreview; override;

  protected
    procedure doCallback(_sender: TObject; _code: Integer);
    procedure lstGridDrawItem(_ctrl: TWinControl; _idx: Integer; _rect: TRect;
      _state: TOwnerDrawState);
    procedure lstGridClick(_sender: TObject);
    procedure doControlChange(_sender: TObject);
    procedure doAddClick(_sender: TObject);
    procedure doDeleteClick(_sender: TObject);
    procedure doClearClick(_sender: TObject);
    procedure doUpdate(_sender: TObject);
  end;
{$ENDREGION}
{$REGION 'Utilities'}

function convert_color(const _color: String): TGIS_Color;
var
  s1, s2, s3: String;
begin
  SplitParamAsText(_color, s1, s2, s3);

  if s1 = GIS_PARAMTXT_TYPE_ARGB then
    Result := TGIS_Color.FromARGB(Cardinal(StrToInt64('$' + s2)))
  else
    Result := TGIS_Color.Gray;
end;

function normalize_color(_color: TGIS_Color; _colorAsText: String): TGIS_Color;
var
  s1, s2, s3: String;
begin
  SplitParamAsText(_colorAsText, s1, s2, s3);
  if s1 = GIS_PARAMTXT_TYPE_FIELD then
    Result := TGIS_Color.DimGray
  else if s1 = GIS_PARAMTXT_TYPE_RENDERER then
    Result := TGIS_Color.DimGray
  else
    Result := _color;
end;

function normalize_angle(_angle: Double; _angleAsText: String): Double;
var
  s1, s2, s3, s4: String;
begin
  SplitNumberAsText(_angleAsText, s1, s2, s3, s4);
  if s1 = GIS_PARAMTXT_TYPE_FIELD then
    Result := DegToRad(45)
  else
    Result := _angle;
end;

function normalize_size(_vwr: IGIS_Viewer; _size: Integer; _sizeAsText: String;
  _max_points: Integer): Integer;
var
  s1, s2, s3, s4: String;
begin
  if not IsStringEmpty(_sizeAsText) then
    SplitNumberAsText(_sizeAsText, s1, s2, s3, s4)
  else
    s1 := '';
  if s1 = GIS_PARAMTXT_TYPE_FIELD then
  begin
    Result := MAX_PREVIEW_SIZE_RENDERER;
  end
  else if s1 = GIS_PARAMTXT_TYPE_RENDERER then
  begin
    Result := MAX_PREVIEW_SIZE_RENDERER;
  end
  else
  begin
    if Abs(_size) > GIS_AUTOSIZE_SIZE then
      Result := MAX_PREVIEW_SIZE_RENDERER
    else
      Result := _size;
  end;

  if Result > _max_points then
    Result := _max_points;

  if Result < 0 then
    Result := _vwr.PixelsToTwips(Result);
end;

{$ENDREGION}
{$REGION 'T_SpeedButton'}

procedure T_speedButton.Paint;
begin
  inherited;

  if not assigned(Images) then
    exit;

  if Enabled then
    Images.Draw(Canvas, (Width - Images.Width) div 2, (Height - Images.Height)
      div 2, ImageIndex, True)
  else
    DisabledImages.Draw(Canvas, (Width - Images.Width) div 2,
      (Height - Images.Height) div 2, ImageIndex, True);
end;
{$ENDREGION}
{$REGION 'T_fieldPanel'}

constructor T_fieldPanel.Create(_owner: TComponent);
begin
  inherited;

  Self.Caption := '';
  Self.BevelInner := TBevelCut.bvNone;
  Self.BevelOuter := TBevelCut.bvNone;
  Self.BevelKind := TBevelKind.bkFlat;

  lblField := TLabel.Create(Self);
  lblField.Parent := Self;
  lblField.Align := TAlign.alClient;
  lblField.Alignment := TAlignment.taCenter;
  lblField.Layout := tTextLayout.tlCenter;
  lblField.Caption := '';
end;

function T_fieldPanel.fget_Field: String;
begin
  Result := lblField.Caption;
end;

procedure T_fieldPanel.fset_Field(const _name: String);
begin
  lblField.Caption := _name;
end;

procedure T_fieldPanel.SetEnabled(Value: Boolean);
begin
  inherited;

  lblField.Enabled := Value;
end;
{$ENDREGION}
{$REGION 'T_scrollablePanel'}

constructor T_scrollablePanel.Create(_owner: TWinControl;
  const _pwnd: TGIS_ControlLegendForm);
begin
  inherited Create(_owner);

  OnMouseWheel := doMouseWheel;
  Parent := _owner;
  oParentWindow := _pwnd;

  Visible := False;
  Self.Width := 256;
  Self.Height := 256;
  Self.BorderStyle := bsNone;

  vShift := 0;
  hShift := 0;

  pnlPanel := Self;

  init;
  read;
  Visible := True;
end;

function T_scrollablePanel.fget_HasPreview: Boolean;
begin
  // to be implemented in descendant classes
  Result := False;
end;

procedure T_scrollablePanel.init;
begin
  // to be implemented in descendant classes
end;

procedure T_scrollablePanel.lockUpdates;
begin
  Inc(iBlockUpdates);
end;

procedure T_scrollablePanel.unlockUpdates;
begin
  Dec(iBlockUpdates);
end;

function T_scrollablePanel.blockedUpdates: Boolean;
begin
  Result := (iBlockUpdates > 0);
end;

function T_scrollablePanel.ppiFix(const _value: Integer): Integer;
begin
  Result := oParentWindow.ppiFix(_value);
end;

function T_scrollablePanel.doCustomSize(_sender: TObject;
  _value: String): String;
var
  dlg: TGIS_ControlSizeForm;
  frm: TGIS_ControlFieldFactor;
  val: String;
  proc: TGIS_Proc;
begin
  Result := '';

  if _value = GIS_PARAMTXT_TYPE_FIELD then
  begin
    frm := TGIS_ControlFieldFactor.Create(Self);
    frm.FillFields(oParentWindow.MVC.FieldNames);
    frm.FillUnits(TGIS_FieldFactorUnitsType.Size);

    proc := procedure(_modal_result: TGIS_PvlModalResult)
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit;

        val := ConstructNumberAsText(frm.cmbFields.Text, frm.spnFactor.Text,
          frm.cmbUnits.Text);
        // oComboBox.DelayedUpdate( val ) ;
      end;

    frm.Execute(oParentWindow.pOnHelp, proc);
    Result := val;
  end
  else if _value = GIS_PARAMTXT_TYPE_CUSTOM then
  begin
    dlg := TGIS_ControlSizeForm.Create(Self);
    dlg.FillUnits(False);
    proc := procedure(_modal_result: TGIS_PvlModalResult)
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit;
        if dlg.isRotation then
          val := GIS_PARAMTXT_TYPE_ANGLE + ':' + dlg.spnFactor.Text + ' ' +
            dlg.cmbUnits.Text
        else
          val := GIS_PARAMTXT_TYPE_SIZE + ':' + dlg.spnFactor.Text + ' ' +
            dlg.cmbUnits.Text

          // oComboBox.DelayedUpdate( val ) ;
      end;

    dlg.Execute(oParentWindow.pOnHelp, proc);
    Result := val;
    // dlg.Free ;
  end;
end;

function T_scrollablePanel.doCustomRWUnits(_sender: TObject;
  _value: String): String;
var
  dlg: TGIS_ControlSizeForm;
  frm: TGIS_ControlFieldFactor;
  val: String;
  proc: TGIS_Proc;
begin
  Result := '';

  if _value = GIS_PARAMTXT_TYPE_FIELD then
  begin
    frm := TGIS_ControlFieldFactor.Create(Self);
    frm.FillFields(oParentWindow.MVC.FieldNames);
    frm.FillUnits(TGIS_FieldFactorUnitsType.Measure);

    proc := procedure(_modal_result: TGIS_PvlModalResult)
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit;

        val := ConstructNumberAsText(frm.cmbFields.Text, frm.spnFactor.Text,
          frm.cmbUnits.Text);
        // oComboBox.DelayedUpdate( val ) ;
      end;
    frm.Execute(oParentWindow.pOnHelp, proc);
    Result := val;
  end
  else if _value = GIS_PARAMTXT_TYPE_CUSTOM then
  begin
    dlg := TGIS_ControlSizeForm.Create(Self);
    dlg.FillRealWorldUnits;
    proc := procedure(_modal_result: TGIS_PvlModalResult)
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit;
        if dlg.isRotation then
          val := GIS_PARAMTXT_TYPE_ANGLE + ':' + dlg.spnFactor.Text + ' ' +
            dlg.cmbUnits.Text
        else
          val := GIS_PARAMTXT_TYPE_SIZE + ':' + dlg.spnFactor.Text + ' ' +
            dlg.cmbUnits.Text

          // oComboBox.DelayedUpdate( val ) ;
      end;

    dlg.Execute(oParentWindow.pOnHelp, proc);
    Result := val;
  end;
end;

function T_scrollablePanel.doCustomRotation(_sender: TObject;
  _value: String): String;
var
  dlg: TGIS_ControlSizeForm;
  frm: TGIS_ControlFieldFactor;
  val: String;
  proc: TGIS_Proc;
begin
  Result := '';

  if _value = GIS_PARAMTXT_TYPE_FIELD then
  begin
    frm := TGIS_ControlFieldFactor.Create(Self);
    frm.FillFields(oParentWindow.MVC.FieldNames);
    frm.FillUnits(TGIS_FieldFactorUnitsType.Angular);

    proc := procedure(_modal_result: TGIS_PvlModalResult)
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit;

        val := ConstructNumberAsText(frm.cmbFields.Text, frm.spnFactor.Text,
          frm.cmbUnits.Text);
        // oComboBox.DelayedUpdate( val ) ;
      end;

    frm.Execute(oParentWindow.pOnHelp, proc);
    Result := val;
  end
  else if _value = GIS_PARAMTXT_TYPE_CUSTOM then
  begin
    dlg := TGIS_ControlSizeForm.Create(Self);
    dlg.FillUnits(True);
    proc := procedure(_modal_result: TGIS_PvlModalResult)
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit;
        if dlg.isRotation then
          val := GIS_PARAMTXT_TYPE_ANGLE + ':' + dlg.spnFactor.Text + ' ' +
            dlg.cmbUnits.Text
        else
          val := GIS_PARAMTXT_TYPE_SIZE + ':' + dlg.spnFactor.Text + ' ' +
            dlg.cmbUnits.Text

          // oComboBox.DelayedUpdate( val ) ;
      end;

    dlg.Execute(oParentWindow.pOnHelp, proc);
    Result := val;
  end;
end;

function T_scrollablePanel.doCustomPattern(_sender: TObject;
  _value: String): String;
var
  frms: TGIS_ControlSymbology;
  frmb: TGIS_ControlBitmap;
  frme: TGIS_LineSymbolEditor;
  s1, s2, s3: String;
  s: String;
  frm: TGIS_ControlFieldFactor;
  val: String;
  proc: TGIS_Proc;
begin
  Result := '';

  if _value = GIS_PARAMTXT_TYPE_SYMBOL then
  begin
    frms := TGIS_ControlSymbology.Create(Self);

    proc := procedure(_modal_result: TGIS_PvlModalResult)
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit;

        if assigned(frms.Symbol) then
        begin
          val := ConstructParamAsText(GIS_PARAMTXT_TYPE_SYMBOL,
            frms.Symbol.Name, '');
          // oComboBox.DelayedUpdate( val ) ;
        end;
      end;
    frms.Execute('', oParentWindow.pOnHelp, proc);
    Result := val;
  end
  else if _value = GIS_PARAMTXT_TYPE_TEXTURE then
  begin
    frmb := TGIS_ControlBitmap.Create(Self);

    proc := procedure(_modal_result: TGIS_PvlModalResult)
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit;

        if assigned(frmb.Bitmap) then
        begin
          val := ConstructParamAsText(GIS_PARAMTXT_TYPE_TEXTURE,
            frmb.Bitmap.Path, '');
          // oComboBox.DelayedUpdate( val ) ;
        end;
      end;
    frmb.Execute('', oParentWindow.pOnHelp, proc);
    Result := val;
  end
  else if _value = GIS_PARAMTXT_TYPE_CODE then
  begin
    frme := TGIS_LineSymbolEditor.Create(Self, True);
    try
      SplitParamAsText(TGIS_PenStyleComboBox(_sender).Value, s1, s2, s3);
      if s1 = GIS_PARAMTXT_TYPE_CODE then
        s := s2
      else
        s := '';
      if frme.Execute(s, oParentWindow.pOnHelp) = mrOK then
      begin
        Result := ConstructParamAsText(GIS_PARAMTXT_TYPE_CODE,
          '' + frme.Symbol, '');
      end;
    finally
      frme.Free;
    end;
  end
  else if _value = GIS_PARAMTXT_TYPE_FIELD then
  begin
    frm := TGIS_ControlFieldFactor.Create(Self);
    frm.FillFields(oParentWindow.MVC.FieldNames);
    frm.FillUnits(TGIS_FieldFactorUnitsType.NoScale);

    proc := procedure(_modal_result: TGIS_PvlModalResult)
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit;

        val := ConstructParamAsText(GIS_PARAMTXT_TYPE_FIELD,
          frm.cmbFields.Text, '');
        // oComboBox.DelayedUpdate( val ) ;
      end;

    frm.Execute(oParentWindow.pOnHelp, proc);
    Result := val;
  end
end;

function T_scrollablePanel.doCustomShield(_sender: TObject;
  _value: String): String;
var
  frm: TGIS_ControlSymbology;
  s1, s2, s3: String;
  s: String;
  val: String;
  proc: TGIS_Proc;
begin
  Result := '';

  if _value = GIS_PARAMTXT_TYPE_SYMBOL then
  begin
    frm := TGIS_ControlSymbology.Create(Self);

    proc := procedure(_modal_result: TGIS_PvlModalResult)
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit;

        if assigned(frm.Symbol) then
        begin
          val := ConstructParamAsText(GIS_PARAMTXT_TYPE_SYMBOL,
            frm.Symbol.Name, '');
          // oComboBox.DelayedUpdate( val ) ;
        end;
      end;
    frm.OnlySVG := True;
    frm.OnlyCategory := GIS_SHIELD_CATEGORY;
    frm.Execute('', oParentWindow.pOnHelp, proc);
    Result := val;
  end
  else if _value = 'nil' then
  begin
    Result := 'nil';
  end
  else
    Assert(False)
end;

function T_scrollablePanel.doCustomColor(
  _sender : TObject ;
  _value  : String
) : String ;
var
  frmc : TGIS_ControlColor ;
  frmf : TGIS_ControlFieldFactor ;
  clr  : TGIS_Color ;
  val  : String ;
  proc : TGIS_Proc ;
begin
  Result := '' ;

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
//          oComboBox.DelayedUpdate( val ) ;
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
//          oComboBox.DelayedUpdate( val ) ;
      end;

    frmc.Execute( clr, oParentWindow.pOnHelp, proc ) ;
    Result := val ;
  end ;
end ;

function T_scrollablePanel.doGetBitmapAreaFill(_sender: TObject; _value: String;
  _width: Integer; _height: Integer): TGIS_Bitmap;
var
  vbmp: TGIS_ViewerBmp;
  ll: TGIS_LayerVector;
  shp: TGIS_Shape;
  shpl: TGIS_Shape;
  w, h: Integer;
begin
  w := _width;
  h := _height;

  vbmp := TGIS_ViewerBmp.Create(w, h);
  try
    vbmp.Renderer  := oParentWindow.gisPreview.Renderer.CreateInstance;
    vbmp.CustomPPI := oParentWindow.gisPreview.PPI;
    vbmp.Color := TGIS_Color.None;

    ll := TGIS_LayerVector.Create;
    vbmp.Add(ll);

    shp := ll.CreateShape(TGIS_ShapeType.Polygon);
    shp.lock(TGIS_Lock.Extent);
    shp.AddPart;
    shp.AddPoint(GisPoint(-w / 2, -h / 2));
    shp.AddPoint(GisPoint(-w / 2, h / 2));
    shp.AddPoint(GisPoint(w / 2, h / 2));
    shp.AddPoint(GisPoint(w / 2, -h / 2));
    shp.unlock;

    shp.Params.Area.OutlineWidth := 0;
    shp.Params.Area.PatternAsText := _value;

    shp.Params.Area.SymbolGap := 0;
    shp.Params.Area.SymbolSize := -h;

    if assigned(shp.Params.Area.Symbol) then
    begin
      if shp.Params.Area.Symbol is TGIS_SymbolFont then
        shp.Params.Area.SymbolSize := -h * 3 div 4 // bit more space for fonts
      else
      if shp.Params.Area.Symbol is TGIS_SymbolHatch then begin
        shp.Params.Area.SymbolSize := -2 ;
        shp.Params.Area.SymbolGap := 1 ;
        vbmp.Renderer := TGIS_RendererVclGdiPlus.Create ;
      end
      else
        shp.Params.Area.SymbolSize :=
          RoundS(-h * shp.Params.Area.Symbol.Width /
          shp.Params.Area.Symbol.Height);
    end;

    if shp.Params.Area.Pattern = TGIS_BrushStyle.Clear then
      shp.Params.Area.Color := TGIS_Color.None
    else
      shp.Params.Area.Color := GISColor(TComboBox(_sender).Canvas.Font.Color);

    if ((shp.Params.Area.Pattern = TGIS_BrushStyle.Clear) or
      (shp.Params.Area.Pattern = TGIS_BrushStyle.Solid)) and
      TGIS_Bitmap.IsNilOrEmpty(shp.Params.Area.Bitmap) and
      (not assigned(shp.Params.Area.Symbol)) then
    begin
      shpl := ll.CreateShape(TGIS_ShapeType.Point);
      shpl.lock(TGIS_Lock.Extent);
      shpl.AddPart;
      shpl.AddPoint(GisPoint(0, 0));
      shpl.unlock;
      if shp.Params.Area.Pattern = TGIS_BrushStyle.Clear then
      begin
        shpl.Params.Labels.Value := _rsrc(GIS_RS_LEGEND_PRM_BRUSH_TRANSPARENT);
        shpl.Params.Labels.Color :=
          GISColor(TComboBox(_sender).Canvas.Font.Color);
        shpl.Params.Labels.FontColor :=
          GISColor(TComboBox(_sender).Canvas.Font.Color);
      end
      else
      begin
        shp.Params.Area.OutlineWidth := 1;
        shp.Params.Area.Pattern := TGIS_BrushStyle.Clear;
        shpl.Params.Labels.Value := _rsrc(GIS_RS_LEGEND_PRM_BRUSH_SOLID);
        shpl.Params.Labels.Color :=
          GISColor(TComboBox(_sender).Canvas.Font.Color);
        shpl.Params.Labels.FontColor :=
          GISColor(TComboBox(_sender).Canvas.Font.Color);
      end;
      shpl.Params.Labels.Position := [TGIS_LabelPosition.MiddleCenter];
      shpl.Params.Labels.FontSize := TGIS_ParamsLabelFont.PtToFontSize
        (Font.Size);
      shpl.Params.Marker.Size := 0;
      vbmp.Renderer := TGIS_RendererVclGdiPlus.Create ;
    end;

    vbmp.FullExtent;
    vbmp.Draw;

    Result := TGIS_Bitmap.Create;
    Result.LoadFromBitmap(vbmp.GIS_Bitmap.NativeBitmap, _value);
  finally
    FreeObject(vbmp);
  end;
end;

function T_scrollablePanel.doGetBitmapAreaOutline(_sender: TObject;
  _value: String; _width: Integer; _height: Integer): TGIS_Bitmap;
var
  vbmp: TGIS_ViewerBmp;
  ll: TGIS_LayerVector;
  shp: TGIS_Shape;
  shpl: TGIS_Shape;
  w, h: Integer;
  bline: Boolean;
begin
  w := _width;
  h := _height;

  vbmp := TGIS_ViewerBmp.Create(w, h);
  try
    vbmp.Renderer  := oParentWindow.gisPreview.Renderer.CreateInstance;
    vbmp.CustomPPI := oParentWindow.gisPreview.PPI;
    vbmp.Color := TGIS_Color.None;

    ll := TGIS_LayerVector.Create;
    vbmp.Add(ll);

    shp := ll.CreateShape(TGIS_ShapeType.Arc);

    shp.Params.Line.StyleAsText := _value;

    bline := False;
    if shp.Params.Line.Symbol is TGIS_SymbolLineEx then
      bline := True
    else if shp.Params.Line.Symbol is TGIS_SymbolLine then
      bline := True;

    shp.lock(TGIS_Lock.Extent);
    shp.AddPart;
    if assigned(shp.Params.Line.Symbol) and (not bline) then
    begin
      shp.AddPoint(GisPoint(-w / 2 * 0.7, 0));
      shp.AddPoint(GisPoint(w / 2, 0));
    end
    else
    begin
      shp.AddPoint(GisPoint(-w / 2 * 0.7, 0));
      shp.AddPoint(GisPoint(w / 2 * 0.7, 0));
    end;
    shp.unlock;

    shp.Params.Line.Color := GISColor(TComboBox(_sender).Canvas.Font.Color);
    shp.Params.Line.OutlineColor :=
      GISColor(TComboBox(_sender).Canvas.Font.Color);

    if assigned(shp.Params.Line.Symbol) then
    begin
      if not bline then // symbol character
        shp.Params.Line.Width := -h * 4 div 5
      else
        shp.Params.Line.WidthAsText := 'SIZE:2dip';
    end
    else
    begin
      shp.Params.Line.WidthAsText := 'SIZE:3dip';
    end;

    if shp.Params.Line.Style = TGIS_PenStyle.Clear then
    begin
      shpl := ll.CreateShape(TGIS_ShapeType.Point);
      shpl.lock(TGIS_Lock.Extent);
      shpl.AddPart;
      shpl.AddPoint(GisPoint(0, 0));
      shpl.unlock;
      shpl.Params.Labels.Value := _rsrc(GIS_RS_LEGEND_PRM_BRUSH_CLEAR);
      shpl.Params.Labels.Position := [TGIS_LabelPosition.MiddleCenter];
      shpl.Params.Labels.FontSize := TGIS_ParamsLabelFont.PtToFontSize
        (Font.Size);
      shpl.Params.Labels.Color :=
        GISColor(TComboBox(_sender).Canvas.Font.Color);
      shpl.Params.Labels.FontColor :=
        GISColor(TComboBox(_sender).Canvas.Font.Color);
      shpl.Params.Marker.Size := 0;
    end;

    vbmp.VisibleExtent := GisExtent(-w / 2, -h / 4, w / 2, h / 4);

    vbmp.Draw;

    Result := TGIS_Bitmap.Create;
    Result.LoadFromBitmap(vbmp.GIS_Bitmap.NativeBitmap, _value);
  finally
    FreeObject(vbmp);
  end;
end;

procedure T_scrollablePanel.doMouseWheel(_sender: TObject; _shift: TShiftState;
  _wheelDelta: Integer; _mousePos: TPoint; var _handled: Boolean);
var
  new_val: Integer;
  old_val: Integer;
  ctl: TControl;
begin
  _handled := False;

  ctl := (FindVCLWindow(_mousePos));
  if (ctl is TWinControl) and (TWinControl(ctl).Focused) then
    exit;

  old_val := VertScrollBar.Position;
  new_val := old_val - _wheelDelta;

  // scroll window
  if new_val > VertScrollBar.Range then
    new_val := VertScrollBar.Range;
  if new_val < 0 then
    new_val := 0;

  VertScrollBar.Position := new_val;
  _handled := True;
end;

function T_scrollablePanel.doGetBitmapMarkerStyle(_sender: TObject;
  _value: String; _width: Integer; _height: Integer): TGIS_Bitmap;
var
  vbmp: TGIS_ViewerBmp;
  ll: TGIS_LayerVector;
  shp: TGIS_Shape;
  w, h: Integer;
  sh: Single;
  sw: Single;
  old_cnt : Boolean ;
begin
  w := _width;
  h := _height;

  vbmp := TGIS_ViewerBmp.Create(w, h);
  try
    vbmp.Renderer  := oParentWindow.gisPreview.Renderer.CreateInstance;
    vbmp.CustomPPI := oParentWindow.gisPreview.PPI;
    vbmp.Color := TGIS_Color.None;

    ll := TGIS_LayerVector.Create;
    vbmp.Add(ll);

    ll.Extent := GisExtent(-w / 2, -h / 2, w / 2, h / 2);

    shp := ll.CreateShape(TGIS_ShapeType.Point);
    shp.lock(TGIS_Lock.Extent);
    shp.AddPart;
    shp.AddPoint(GisPoint(0, 0));
    shp.unlock;
    vbmp.FullExtent;

    shp.Params.Marker.StyleAsText := _value;
    shp.Params.Marker.Color := GISColor(TComboBox(_sender).Canvas.Font.Color);
    shp.Params.Marker.OutlineColor :=
      GISColor(TComboBox(_sender).Canvas.Font.Color);
    shp.Params.Marker.Size := -h * 3 div 4;

    if assigned(shp.Params.Marker.Symbol) then
    begin
      old_cnt := shp.Params.Marker.Symbol.AutoCenter ;
      shp.Params.Marker.Symbol.AutoCenter := True ;

      sh := shp.Params.Marker.Symbol.NativeHeight;
      if sh = 0 then
        sh := 1;
      sw := shp.Params.Marker.Symbol.NativeWidth;
      if sw = 0 then
        sw := 1;
      if shp.Params.Marker.Symbol is TGIS_SymbolFont then
      begin
        shp.Params.Marker.Size := -h * 3 div 4; // bit more space for fonts
        shp.Params.Marker.OutlineWidth := 0;
      end
      else
      begin
        if sh >= sw then
          shp.Params.Marker.Size := RoundS(-h)
        else
        begin
          if sw / sh * h > 4 / 5 * _width then
            shp.Params.Marker.Size := RoundS(-4 / 5 * _width * sh / sw)
          else
            shp.Params.Marker.Size := RoundS(-h);
        end;
      end;
    end;

    vbmp.Draw;

    if assigned(shp.Params.Marker.Symbol) then
      shp.Params.Marker.Symbol.AutoCenter := old_cnt ;

    Result := TGIS_Bitmap.Create;
    Result.LoadFromBitmap(vbmp.GIS_Bitmap.NativeBitmap, _value);
  finally
    FreeObject(vbmp);
  end;
end;

function T_scrollablePanel.doGetBitmapShield(_sender: TObject; _value: String;
  _width: Integer; _height: Integer): TGIS_Bitmap;
var
  vbmp: TGIS_ViewerBmp;
  ll: TGIS_LayerVector;
  shp: TGIS_Shape;
  w, h: Integer;
  sh: Single;
  sw: Single;
begin
  w := _width;
  h := _height;

  vbmp := TGIS_ViewerBmp.Create(w, h);
  try
    vbmp.Renderer  := oParentWindow.gisPreview.Renderer.CreateInstance;
    vbmp.CustomPPI := oParentWindow.gisPreview.PPI;
    vbmp.Color := TGIS_Color.None;

    ll := TGIS_LayerVector.Create;
    vbmp.Add(ll);

    ll.Extent := GisExtent(-w / 2, -h / 2, w / 2, h / 2);

    shp := ll.CreateShape(TGIS_ShapeType.Point);
    shp.lock(TGIS_Lock.Extent);
    shp.AddPart;
    shp.AddPoint(GisPoint(0, 0));
    shp.unlock;
    vbmp.FullExtent;

    shp.Params.Marker.StyleAsText := _value;
    shp.Params.Marker.Color := GISColor(TComboBox(_sender).Canvas.Font.Color);
    shp.Params.Marker.OutlineColor :=
      GISColor(TComboBox(_sender).Canvas.Font.Color);
    shp.Params.Marker.Size := -h * 3 div 4;

    if assigned(shp.Params.Marker.Symbol) then
    begin
      sh := shp.Params.Marker.Symbol.NativeHeight;
      if sh = 0 then
        sh := 1;
      sw := shp.Params.Marker.Symbol.NativeWidth;
      if sw = 0 then
        sw := 1;
      if sh >= sw then
        shp.Params.Marker.Size := RoundS(-h)
      else
      begin
        if sw / sh * h > 4 / 5 * _width then
          shp.Params.Marker.Size := RoundS(-4 / 5 * _width * sh / sw)
        else
          shp.Params.Marker.Size := RoundS(-h);
      end;
    end
    else
    begin
      shp.lock(TGIS_Lock.Extent);
      shp.AddPart;
      shp.AddPoint(GisPoint(0, 0));
      shp.unlock;
      shp.Params.Labels.Value := _rsrc(GIS_RS_LEGEND_PRM_BRUSH_CLEAR);
      shp.Params.Labels.Position := [TGIS_LabelPosition.MiddleCenter];
      shp.Params.Labels.FontSize := TGIS_ParamsLabelFont.PtToFontSize
        (Font.Size);
      shp.Params.Labels.Color := GISColor(TComboBox(_sender).Canvas.Font.Color);
      shp.Params.Labels.FontColor :=
        GISColor(TComboBox(_sender).Canvas.Font.Color);
      shp.Params.Marker.Size := 0;
    end;

    vbmp.Draw;

    Result := TGIS_Bitmap.Create;
    Result.LoadFromBitmap(vbmp.GIS_Bitmap.NativeBitmap, _value);
  finally
    FreeObject(vbmp);
  end;
end;

procedure T_scrollablePanel.Read;
begin
  // to be implemented in descendant classes
end;

procedure T_scrollablePanel.Write;
begin
  // to be implemented in descendant classes
end;

procedure T_scrollablePanel.PreparePreview(const _viewer: IGIS_Viewer);
begin

end;

procedure T_scrollablePanel.UpdatePreview;
begin
end;

{$ENDREGION}
{$REGION 'T_sectionContainer'}

constructor T_sectionContainer.Create(const _pwnd: TGIS_ControlLegendForm);
begin
  inherited Create;

  oParentWindow := _pwnd;

  lstFeatures := TObjectList<T_scrollablePanel>.Create(True);

  oParentWindow.btnWizard.Enabled := False;

  if oParentWindow.MVC.IsVector then
    initVector
  else if oParentWindow.MVC.IsGrid then
    initGrid
  else if oParentWindow.MVC.IsPixel then
    initPixel
  else
    initDefault;
end;

destructor T_sectionContainer.Destroy;
begin
  FreeObject(lstFeatures);

  inherited;
end;

procedure T_sectionContainer.initDefault;
var
  pnl: T_scrollablePanel;
  prnt: TWinControl;
begin
  prnt := oParentWindow.pnlRight;

  pnl := T_panelSection.Create(prnt, oParentWindow);
  pnl.Visible := False;
  pnl.Left := 0;
  pnl.Top := 0;
  pnl.Width := prnt.ClientWidth;
  pnl.Height := prnt.ClientHeight;
  pnl.Anchors := [akLeft, akTop, akRight, akBottom];
  pnl.Name := 'pnlSection';
  pnl.Caption := '';

  lstFeatures.Add(pnl);
end;

procedure T_sectionContainer.initVector;
var
  pnl: T_scrollablePanel;
  prnt: TWinControl;
begin
  oParentWindow.btnWizard.Enabled := True;

  prnt := oParentWindow.pnlRight;

  pnl := T_panelSectionVector.Create(prnt, oParentWindow);
  pnl.Visible := False;
  pnl.Left := 0;
  pnl.Top := 0;
  pnl.Width := prnt.ClientWidth;
  pnl.Height := prnt.ClientHeight;
  pnl.Align := TAlign.alClient;
  pnl.Anchors := [akLeft, akTop, akRight, akBottom];
  pnl.Name := 'pnlSection';
  pnl.Caption := '';

  lstFeatures.Add(pnl);

  pnl := T_panelRenderer.Create(prnt, oParentWindow);
  pnl.Visible := False;
  pnl.Left := 0;
  pnl.Top := 0;
  pnl.Width := prnt.ClientWidth;
  pnl.Height := prnt.ClientHeight;
  pnl.Align := TAlign.alClient;
  pnl.Anchors := [akLeft, akTop, akRight, akBottom];
  pnl.Name := 'pnlRenderer';
  pnl.Caption := '';

  lstFeatures.Add(pnl);

  if oParentWindow.MVC.HasMarker then
  begin
    pnl := T_panelMarker.Create(prnt, oParentWindow);
    pnl.Visible := False;
    pnl.Left := 0;
    pnl.Top := 0;
    pnl.Width := prnt.ClientWidth;
    pnl.Height := prnt.ClientHeight;
    pnl.Align := TAlign.alClient;
    pnl.Anchors := [akLeft, akTop, akRight, akBottom];
    pnl.Name := 'pnlMarker';
    pnl.Caption := '';

    lstFeatures.Add(pnl);
  end;

  if oParentWindow.MVC.HasLine then
  begin
    pnl := T_panelLine.Create(prnt, oParentWindow);
    pnl.Visible := False;
    pnl.Left := 0;
    pnl.Top := 0;
    pnl.Width := prnt.ClientWidth;
    pnl.Height := prnt.ClientHeight;
    pnl.Align := TAlign.alClient;
    pnl.Anchors := [akLeft, akTop, akRight, akBottom];
    pnl.Name := 'pnlLine';
    pnl.Caption := '';

    lstFeatures.Add(pnl);
  end;

  if oParentWindow.MVC.HasArea then
  begin
    pnl := T_panelArea.Create(prnt, oParentWindow);
    pnl.Visible := False;
    pnl.Left := 0;
    pnl.Top := 0;
    pnl.Width := prnt.ClientWidth;
    pnl.Height := prnt.ClientHeight;
    pnl.Align := TAlign.alClient;
    pnl.Anchors := [akLeft, akTop, akRight, akBottom];
    pnl.Name := 'pnlArea';
    pnl.Caption := '';

    lstFeatures.Add(pnl);
  end;

  pnl := T_panelLabel.Create(prnt, oParentWindow);
  pnl.Visible := False;
  pnl.Left := 0;
  pnl.Top := 0;
  pnl.Width := prnt.ClientWidth;
  pnl.Height := prnt.ClientHeight;
  pnl.Align := TAlign.alClient;
  pnl.Anchors := [akLeft, akTop, akRight, akBottom];
  pnl.Name := 'pnlLabel';
  pnl.Caption := '';

  lstFeatures.Add(pnl);

  pnl := T_panelChart.Create(prnt, oParentWindow);
  pnl.Visible := False;
  pnl.Left := 0;
  pnl.Top := 0;
  pnl.Width := prnt.ClientWidth;
  pnl.Height := prnt.ClientHeight;
  pnl.Align := TAlign.alClient;
  pnl.Anchors := [akLeft, akTop, akRight, akBottom];
  pnl.Name := 'pnlChart';
  pnl.Caption := '';

  lstFeatures.Add(pnl);

  pnl := T_panel3D.Create(prnt, oParentWindow);
  pnl.Visible := False;
  pnl.Left := 0;
  pnl.Top := 0;
  pnl.Width := prnt.ClientWidth;
  pnl.Height := prnt.ClientHeight;
  pnl.Align := TAlign.alClient;
  pnl.Anchors := [akLeft, akTop, akRight, akBottom];
  pnl.Name := 'pnl3D';
  pnl.Caption := '';

  lstFeatures.Add(pnl);
end;

procedure T_sectionContainer.initPixel;
var
  pnl: T_scrollablePanel;
  prnt: TWinControl;
begin
  prnt := oParentWindow.pnlRight;

  pnl := T_panelSection.Create(prnt, oParentWindow);
  pnl.Visible := False;
  pnl.Left := 0;
  pnl.Top := 0;
  pnl.Width := prnt.ClientWidth;
  pnl.Height := prnt.ClientHeight;
  pnl.Anchors := [akLeft, akTop, akRight, akBottom];
  pnl.Name := 'pnlSection';
  pnl.Caption := '';

  lstFeatures.Add(pnl);

  pnl := T_panelPixel.Create(prnt, oParentWindow);
  pnl.Visible := False;
  pnl.Left := 0;
  pnl.Top := 0;
  pnl.Width := prnt.ClientWidth;
  pnl.Height := prnt.ClientHeight;
  pnl.Anchors := [akLeft, akTop, akRight, akBottom];
  pnl.Name := 'pnlPixel';
  pnl.Caption := '';

  lstFeatures.Add(pnl);

  pnl := T_panel3D.Create(prnt, oParentWindow);
  pnl.Visible := False;
  pnl.Left := 0;
  pnl.Top := 0;
  pnl.Width := prnt.ClientWidth;
  pnl.Height := prnt.ClientHeight;
  pnl.Anchors := [akLeft, akTop, akRight, akBottom];
  pnl.Name := 'pnl3D';
  pnl.Caption := '';

  lstFeatures.Add(pnl);
end;

procedure T_sectionContainer.initGrid;
var
  pnl: T_scrollablePanel;
  prnt: TWinControl;
begin
  if (TGIS_LayerPixel(oParentWindow.MVC.Layer).MinHeight >=
    TGIS_LayerPixel(oParentWindow.MVC.Layer).MaxHeight) then
    oParentWindow.btnWizard.Enabled := False
  else
    oParentWindow.btnWizard.Enabled := True;

  prnt := oParentWindow.pnlRight;

  pnl := T_panelSection.Create(prnt, oParentWindow);
  pnl.Visible := False;
  pnl.Left := 0;
  pnl.Top := 0;
  pnl.Width := prnt.ClientWidth;
  pnl.Height := prnt.ClientHeight;
  pnl.Anchors := [akLeft, akTop, akRight, akBottom];
  pnl.Name := 'pnlSection';
  pnl.Caption := '';

  lstFeatures.Add(pnl);

  pnl := T_panelGrid.Create(prnt, oParentWindow);
  pnl.Visible := False;
  pnl.Left := 0;
  pnl.Top := 0;
  pnl.Width := prnt.ClientWidth;
  pnl.Height := prnt.ClientHeight;
  pnl.Anchors := [akLeft, akTop, akRight, akBottom];
  pnl.Name := 'pnlGrid';
  pnl.Caption := '';

  lstFeatures.Add(pnl);

  pnl := T_panel3D.Create(prnt, oParentWindow);
  pnl.Visible := False;
  pnl.Left := 0;
  pnl.Top := 0;
  pnl.Width := prnt.ClientWidth;
  pnl.Height := prnt.ClientHeight;
  pnl.Anchors := [akLeft, akTop, akRight, akBottom];
  pnl.Name := 'pnl3D';
  pnl.Caption := '';

  lstFeatures.Add(pnl);
end;

procedure T_sectionContainer.ReadSection;
begin
  lstFeatures.Items[0].Read;
end;

procedure T_sectionContainer.Read;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    lstFeatures.Items[i].Read;
end;

procedure T_sectionContainer.Write;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    lstFeatures.Items[i].Write;
end;

function T_sectionContainer.FindPanel(const _text: String): T_scrollablePanel;
var
  i: Integer;
begin
  Result := nil;

  for i := 0 to Count - 1 do
  begin
    if lstFeatures.Items[i].ItemText = _text then
    begin
      Result := lstFeatures.Items[i];
      break;
    end;
  end;
end;

function T_sectionContainer.fget_Count: Integer;
begin
  Result := lstFeatures.Count;
end;

function T_sectionContainer.fget_Panel(const _i: Integer): T_scrollablePanel;
begin
  Result := lstFeatures.Items[_i];
end;
{$ENDREGION}
{$REGION 'T_panelGeneral'}

function T_panelGeneral.fget_HasPreview: Boolean;
begin
  Result := MVC.HasPreview;
end;

procedure T_panelGeneral.init;
begin
  inherited;

  ItemText := _rsrc(GIS_RS_LEGEND_PAG_GENERAL);

  MVC := oParentWindow.MVC.General;
  MVC.Callback := doCallback;

  initParameters;
  initInfo;
end;

procedure T_panelGeneral.initParameters;
var
  t, i: Integer;
  lst: TStringList;
  bd: TBiDiMode;
  Anchors: TAnchors;
  str: String;
  agg: TGIS_DynamicAggregatorAbstract;
begin
  bd := oParentWindow.BiDiMode;
  if bd = bdRightToLeft then
    Anchors := [akRight, akTop]
  else
    Anchors := [akLeft, akTop];

  gpbParameters := TGroupBox.Create(Self.Panel);
  gpbParameters.Parent := Self.Panel;
  gpbParameters.Top := ppiFix(4);
  gpbParameters.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbParameters, ppiFix(8), ppiFix(WIDTH_NORMAL));
  gpbParameters.Anchors := Anchors;
  gpbParameters.Caption := _rsrc(GIS_RS_LEGEND_TAB_PARAMS);

  t := ppiFix(24);

  lblPath := TLabel.Create(gpbParameters);
  lblPath.Parent := gpbParameters;
  lblPath.Top := t;
  PlaceControl(bd, nil, lblPath, ppiFix(LEFT_3COL_1), lblPath.Width);
  lblPath.Caption := _rsrc(GIS_RS_LEGEND_PRM_PATH);

  t := t + lblPath.Height + ppiFix(4);

  edtPath := TEdit.Create(gpbParameters);
  edtPath.Parent := gpbParameters;
  edtPath.Top := t;
  PlaceControl(bd, nil, edtPath, ppiFix(LEFT_3COL_1), -1);
  edtPath.Text := '';
  edtPath.Color := clBtnFace;
  edtPath.ReadOnly := True;

  t := t + edtPath.Height + ppiFix(8);

  lblName := TLabel.Create(gpbParameters);
  lblName.Parent := gpbParameters;
  lblName.Top := t;
  PlaceControl(bd, nil, lblName, ppiFix(LEFT_3COL_1), lblName.Width);
  lblName.Caption := _rsrc(GIS_RS_LEGEND_PRM_NAME);

  t := t + lblName.Height + ppiFix(4);

  edtName := TEdit.Create(gpbParameters);
  edtName.Parent := gpbParameters;
  edtName.Top := t;
  PlaceControl(bd, nil, edtName, ppiFix(LEFT_3COL_1), -1);
  edtName.Text := 'test';
  edtName.Color := clBtnFace;
  edtName.ReadOnly := True;

  t := t + edtName.Height + ppiFix(8);

  lblCaption := TLabel.Create(gpbParameters);
  lblCaption.Parent := gpbParameters;
  lblCaption.Top := t;
  PlaceControl(bd, nil, lblCaption, ppiFix(LEFT_3COL_1), lblCaption.Width);
  lblCaption.Caption := _rsrc(GIS_RS_LEGEND_PRM_CAPTION);

  t := t + lblName.Height + ppiFix(4);

  edtCaption := TEdit.Create(gpbParameters);
  edtCaption.Parent := gpbParameters;
  edtCaption.Top := t;
  PlaceControl(bd, nil, edtCaption, ppiFix(LEFT_3COL_1), -1);
  edtCaption.Text := 'test';

  t := t + edtCaption.Height + ppiFix(8);

  lblCS := TLabel.Create(gpbParameters);
  lblCS.Parent := gpbParameters;
  lblCS.Top := t;
  PlaceControl(bd, nil, lblCS, ppiFix(LEFT_3COL_1), lblCS.Width);
  lblCS.Caption := _rsrc(GIS_RS_LEGEND_PRM_CS);

  t := t + lblCS.Height + ppiFix(4);

  edtCS := TEdit.Create(gpbParameters);
  edtCS.Parent := gpbParameters;
  edtCS.Top := t;
  PlaceControl(bd, nil, edtCS, ppiFix(LEFT_3COL_1), gpbParameters.Width - 2 *
    ppiFix(LEFT_3COL_1) - ppiFix(WIDTH_3COL));
  edtCS.Text := 'WGS 84 (epsg:4326)';
  edtCS.Color := clBtnFace;
  edtCS.ReadOnly := True;

  btnCS := TButton.Create(gpbParameters);
  btnCS.Parent := gpbParameters;
  btnCS.Top := t - ppiFix(2);
  PlaceControl(bd, edtCS, btnCS, ppiFix(8), ppiFix(WIDTH_3COL) - ppiFix(8));
  btnCS.Caption := _rsrc(GIS_RS_LEGEND_PRM_SELECT);
  btnCS.OnClick := doCSClick;

  t := t + edtCS.Height + ppiFix(16);

  gpbParameters.Width := edtPath.Left + edtPath.Width + ppiFix(LEFT_3COL_1);
  gpbParameters.Height := t;

  t := t + ppiFix(8);

  gpbPainting := TGroupBox.Create(Self.Panel);
  gpbPainting.Parent := Self.Panel;
  gpbPainting.Top := t;
  gpbPainting.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbPainting, ppiFix(8), ppiFix(WIDTH_NORMAL));
  gpbPainting.Anchors := Anchors;
  gpbPainting.Caption := _rsrc(GIS_RS_BUSY_PAINT);

  t := ppiFix(24);

  ckbBasemap := TCheckBox.Create(gpbPainting);
  ckbBasemap.Parent := gpbPainting;
  ckbBasemap.Top := t;
  PlaceControl(bd, nil, ckbBasemap, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_2COL));
  ckbBasemap.Caption := _rsrc(GIS_RS_LEGEND_PRM_BASEMAP);

  t := t + ckbBasemap.Height + ppiFix(8);

  ckbCachedPaint := TCheckBox.Create(gpbPainting);
  ckbCachedPaint.Parent := gpbPainting;
  ckbCachedPaint.Top := t;
  PlaceControl(bd, nil, ckbCachedPaint, ppiFix(LEFT_3COL_1),
    ppiFix(WIDTH_2COL));
  ckbCachedPaint.Caption := _rsrc(GIS_RS_LEGEND_PRM_CACHEDPAINT);

  t := t + ckbCachedPaint.Height + ppiFix(8);

  ckbIgnoreShapeParams := TCheckBox.Create(gpbPainting);
  ckbIgnoreShapeParams.Parent := gpbPainting;
  ckbIgnoreShapeParams.Top := t;
  PlaceControl(bd, nil, ckbIgnoreShapeParams, ppiFix(LEFT_3COL_1),
    ppiFix(WIDTH_2COL));
  ckbIgnoreShapeParams.Caption := _rsrc(GIS_RS_LEGEND_PRM_IGNORESHAPEPARAMS);
  ckbIgnoreShapeParams.Enabled := oParentWindow.MVC.Layer is TGIS_LayerVector;

  t := t + ckbIgnoreShapeParams.Height + ppiFix(8);

  ckbMultipassRendering := TCheckBox.Create(gpbPainting);
  ckbMultipassRendering.Parent := gpbPainting;
  ckbMultipassRendering.Top := t;
  PlaceControl(bd, nil, ckbMultipassRendering, ppiFix(LEFT_3COL_1),
    ppiFix(WIDTH_2COL));
  ckbMultipassRendering.Caption := _rsrc(GIS_RS_LEGEND_PRM_MULTIPASSRENDERING);
  ckbMultipassRendering.Enabled := oParentWindow.MVC.Layer is TGIS_LayerVector;

  t := ppiFix(24);

  lblTransparency := TLabel.Create(gpbPainting);
  lblTransparency.Parent := gpbPainting;
  lblTransparency.Top := t;
  PlaceControl(bd, nil, lblTransparency, ppiFix(LEFT_3COL_3),
    lblTransparency.Width);
  lblTransparency.Caption := _rsrc(GIS_RS_LEGEND_PRM_TRANSPARENCY);

  t := t + lblTransparency.Height + ppiFix(4);

  lst := TStringList.Create;
  try
    i := 0;
    while i <= 100 do
    begin
      lst.Add(IntToStr(i));
      Inc(i, 10);
    end;
    cmbTransparency := TComboBox.Create(gpbPainting);
    cmbTransparency.AutoComplete := False;
    cmbTransparency.Parent := gpbPainting;
    cmbTransparency.Top := t;
    PlaceControl(bd, nil, cmbTransparency, ppiFix(LEFT_3COL_3),
      ppiFix(WIDTH_3COL) - ppiFix(24));
    cmbTransparency.Items.BeginUpdate;
    cmbTransparency.ItemIndex := -1;
    cmbTransparency.Items.Assign(lst);
    cmbTransparency.Items.EndUpdate;

    vvcmbTransparency := TGIS_ValueValidatorComboBoxHelper.Create
      (cmbTransparency);
    vvcmbTransparency.MinVal := 0;
    vvcmbTransparency.MaxVal := 100;

    t := t + cmbTransparency.Height + ppiFix(8);

    if oParentWindow.MVC.IsPixel then
    begin
      lblInterpretation := TLabel.Create(gpbPainting);
      lblInterpretation.Parent := gpbPainting;
      lblInterpretation.Top := t;
      PlaceControl(bd, nil, lblInterpretation, ppiFix(LEFT_3COL_3),
        lblInterpretation.Width);
      lblInterpretation.Caption := _rsrc(GIS_RS_LEGEND_PRM_INTERPRETATION);
      lblInterpretation.Visible := True;

      t := t + lblInterpretation.Height + ppiFix(4);

      cmbInterpretation := TComboBox.Create(gpbPainting);
      cmbInterpretation.AutoComplete := False;
      cmbInterpretation.Parent := gpbPainting;
      cmbInterpretation.Top := t;
      cmbInterpretation.Style := TComboBoxStyle.csDropDownList;
      PlaceControl(bd, nil, cmbInterpretation, ppiFix(LEFT_3COL_3),
        ppiFix(WIDTH_3COL) - ppiFix(24));
      cmbInterpretation.Items.BeginUpdate;
      cmbInterpretation.ItemIndex := -1;
      cmbInterpretation.Items.Add(_rsrc(GIS_RS_LEGEND_PRM_DEFAULT));
      cmbInterpretation.Items.Add(_rsrc(GIS_RS_LEGEND_PAG_PIXEL));
      cmbInterpretation.Items.Add(_rsrc(GIS_RS_LEGEND_PAG_GRID));
      cmbInterpretation.Items.EndUpdate;
      cmbInterpretation.Visible := True;
      cmbInterpretation.OnChange := doControlChange;
    end;

    t := ckbMultipassRendering.Top + ckbMultipassRendering.Height;

    if oParentWindow.MVC.Layer is TGIS_LayerVector then
    begin
      t := t + ppiFix(8);

      lblScope := TLabel.Create(gpbPainting);
      lblScope.Parent := gpbPainting;
      lblScope.Top := t;
      PlaceControl(bd, nil, lblScope, ppiFix(LEFT_3COL_1), lblScope.Width);
      lblScope.Caption := _rsrc(GIS_RS_LEGEND_PRM_SCOPE);

      t := t + lblScope.Height + ppiFix(4);

      cmbScope := TComboBox.Create(gpbPainting);
      cmbScope.Parent := gpbPainting;
      cmbScope.Top := t;
      PlaceControl(bd, nil, cmbScope, ppiFix(LEFT_3COL_1), -1);
      cmbScope.ItemIndex := -1;
      oParentWindow.fillComboBoxWithFields(cmbScope);

      t := cmbScope.Top + cmbScope.Height;
    end
    else
      t := ckbMultipassRendering.Top + ckbMultipassRendering.Height;

    t := t + ppiFix(8);

    ckbUseConfig := TCheckBox.Create(gpbPainting);
    ckbUseConfig.Parent := gpbPainting;
    ckbUseConfig.Top := t;
    PlaceControl(bd, nil, ckbUseConfig, ppiFix(LEFT_3COL_1),
      ppiFix(WIDTH_NARROW));
    ckbUseConfig.Caption := _rsrc(GIS_RS_LEGEND_PRM_USECONFIG);

    t := t + ckbUseConfig.Height;

    gpbPainting.Height := t + ppiFix(16);
  finally
    FreeObject(lst);
  end;

  if oParentWindow.MVC.IsVector then
  begin
    gpbAggregation := TGroupBox.Create(Self.Panel);
    gpbAggregation.Parent := Self.Panel;
    gpbAggregation.Top := gpbPainting.Top + gpbPainting.Height + ppiFix(4);
    gpbAggregation.Height := ppiFix(512);
    PlaceControl(bd, nil, gpbAggregation, ppiFix(8), ppiFix(WIDTH_NORMAL));
    gpbAggregation.Anchors := Anchors;
    gpbAggregation.Caption := _rsrc(GIS_RS_LEGEND_PRM_AGGREGATION);

    t := ppiFix(24);

    lblAggMethod := TLabel.Create(gpbAggregation);
    lblAggMethod.Parent := gpbAggregation;
    lblAggMethod.Top := t;
    PlaceControl(bd, nil, lblAggMethod, ppiFix(LEFT_3COL_1),
      lblAggMethod.Width);
    lblAggMethod.Caption := _rsrc(GIS_RS_LEGEND_PRM_AGGR_METHOD);

    t := t + lblAggMethod.Height + ppiFix(4);

    cmbAggMethod := TComboBox.Create(gpbAggregation);
    cmbAggMethod.AutoComplete := False;
    cmbAggMethod.Parent := gpbAggregation;
    cmbAggMethod.Top := t;
    cmbAggMethod.Style := TComboBoxStyle.csDropDownList;
    PlaceControl(bd, nil, cmbAggMethod, ppiFix(LEFT_3COL_1), -1);
    cmbAggMethod.Items.BeginUpdate;
    cmbAggMethod.ItemIndex := -1;
    cmbAggMethod.Items.Add(_rsrc(GIS_RS_LEGEND_PRM_OFF));

    for str in TGIS_DynamicAggregatorFactory.Names do
    begin
      agg := TGIS_DynamicAggregatorFactory.CreateInstance(str,
        TGIS_LayerVector(oParentWindow.MVC.Layer));
      try
        cmbAggMethod.Items.Add(agg.Caption);
      finally
        FreeObject(agg);
      end;
    end;

    cmbAggMethod.Items.EndUpdate;
    cmbAggMethod.Visible := True;
    cmbAggMethod.ItemIndex := 0;
    cmbAggMethod.OnChange := doAggregateChange;

    t := t + cmbAggMethod.Height + ppiFix(8);

    lblAggRadius := TLabel.Create(gpbAggregation);
    lblAggRadius.Parent := gpbAggregation;
    lblAggRadius.Top := t;
    PlaceControl(bd, nil, lblAggRadius, ppiFix(LEFT_3COL_1),
      lblAggRadius.Width);
    lblAggRadius.Caption := _rsrc(GIS_RS_LEGEND_PRM_AGGR_RADIUS);

    lblAggThreshold := TLabel.Create(gpbAggregation);
    lblAggThreshold.Parent := gpbAggregation;
    lblAggThreshold.Top := t;
    PlaceControl(bd, nil, lblAggThreshold, ppiFix(LEFT_3COL_2),
      lblAggThreshold.Width);
    lblAggThreshold.Caption := _rsrc(GIS_RS_LEGEND_PRM_AGGR_THRESHOLD);

    t := t + lblAggThreshold.Height + ppiFix(4);

    cmbAggRadius := TGIS_SizeComboBox.Create(gpbAggregation);
    cmbAggRadius.Parent := gpbAggregation;
    cmbAggRadius.Top := t;
    PlaceControl(bd, nil, cmbAggRadius, ppiFix(LEFT_3COL_1),
      ppiFix(WIDTH_3COL));
    cmbAggRadius.Anchors := Anchors;
    cmbAggRadius.FillAggregation;
    cmbAggRadius.CustomEvent := doCustomSize;

    edtAggThreshold := TEdit.Create(gpbAggregation);
    edtAggThreshold.Parent := gpbAggregation;
    edtAggThreshold.Top := t;
    PlaceControl(bd, nil, edtAggThreshold, ppiFix(LEFT_3COL_2),
      ppiFix(WIDTH_3COL));
    edtAggThreshold.OnChange := doControlChange;

    vvedtAggThreshold := TGIS_ValueValidatorEditHelper.Create(edtAggThreshold);
    vvedtAggThreshold.MinVal := 0;
    vvedtAggThreshold.MaxVal := GIS_MAX_INTEGER;
    vvedtAggThreshold.Precision := 0;

    t := t + edtAggThreshold.Height + ppiFix(16);

    gpbAggregation.Height := t;

  end;
end;

procedure T_panelGeneral.initInfo;
var
  t: Integer;
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  gpbInfo := TGroupBox.Create(Self.Panel);
  gpbInfo.Parent := Self.Panel;
  if oParentWindow.MVC.IsVector then
    gpbInfo.Top := gpbAggregation.Top + gpbAggregation.Height + ppiFix(4)
  else
    gpbInfo.Top := gpbPainting.Top + gpbPainting.Height + ppiFix(4);

  gpbInfo.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbInfo, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbInfo.Anchors := [akRight, akTop]
  else
    gpbInfo.Anchors := [akLeft, akTop];
  gpbInfo.Caption := _rsrc(GIS_RS_LEGEND_TAB_INFO);

  t := ppiFix(24);

  lblFileInformation := TLabel.Create(gpbInfo);
  lblFileInformation.Parent := gpbInfo;
  lblFileInformation.Top := t;
  PlaceControl(bd, nil, lblFileInformation, ppiFix(LEFT_3COL_1),
    lblFileInformation.Width);
  lblFileInformation.Caption := _rsrc(GIS_RS_LEGEND_PRM_INFO);

  t := t + lblFileInformation.Height + ppiFix(4);

  txbFileInformation := TMemo.Create(gpbInfo);
  txbFileInformation.Parent := gpbInfo;
  txbFileInformation.Top := t;
  txbFileInformation.Height := ppiFix(64);
  PlaceControl(bd, nil, txbFileInformation, ppiFix(LEFT_3COL_1), -1);
  txbFileInformation.Text := '';
  txbFileInformation.Color := clBtnFace;
  txbFileInformation.ReadOnly := True;
  txbFileInformation.WordWrap := True;
  txbFileInformation.ScrollBars := TScrollStyle.ssVertical;

  t := t + txbFileInformation.Height + ppiFix(8);

  lblUserComments := TLabel.Create(gpbInfo);
  lblUserComments.Parent := gpbInfo;
  lblUserComments.Top := t;
  PlaceControl(bd, nil, lblUserComments, ppiFix(LEFT_3COL_1),
    lblUserComments.Width);
  lblUserComments.Caption := _rsrc(GIS_RS_LEGEND_PRM_COMMENTS);

  t := t + lblUserComments.Height + ppiFix(4);

  txbUserComments := TMemo.Create(gpbInfo);
  txbUserComments.Parent := gpbInfo;
  txbUserComments.Top := t;
  txbUserComments.Height := ppiFix(32);
  PlaceControl(bd, nil, txbUserComments, ppiFix(LEFT_3COL_1), -1);
  txbUserComments.WordWrap := True;
  txbUserComments.ScrollBars := TScrollStyle.ssVertical;

  t := t + txbUserComments.Height + ppiFix(8);

  if oParentWindow.MVC.Layer is TGIS_LayerVector then
  begin
    lblCodePage := TLabel.Create(gpbInfo);
    lblCodePage.Parent := gpbInfo;
    lblCodePage.Top := t;
    PlaceControl(bd, nil, lblCodePage, ppiFix(LEFT_3COL_1), lblCodePage.Width);
    lblCodePage.Caption := _rsrc(GIS_RS_LEGEND_PRM_CODEPAGE);

    t := t + lblName.Height + ppiFix(4);

    edtCodePage := TEdit.Create(gpbInfo);
    edtCodePage.Parent := gpbInfo;
    edtCodePage.Top := t;
    PlaceControl(bd, nil, edtCodePage, ppiFix(LEFT_3COL_1), ppiFix(72));

    vvedtCodePage := TGIS_ValueValidatorEditHelper.Create(edtCodePage);
    vvedtCodePage.MinVal := 0;
    vvedtCodePage.MaxVal := 65535;

    t := t + edtCodePage.Height + ppiFix(16);
  end;

  gpbInfo.Height := t;

end;

procedure T_panelGeneral.Read;
var
  idx: Integer;
begin
  lockUpdates;
  try
    edtPath.Text := MVC.Path;
    edtName.Text := MVC.Name;
    edtCaption.Text := MVC.Caption;
    edtCS.Text := MVC.CS.FriendlyName;
    ckbBasemap.Checked := MVC.Basemap;
    ckbCachedPaint.Checked := MVC.CachedPaint;
    ckbIgnoreShapeParams.Checked := MVC.IgnoreShapeParams;
    ckbMultipassRendering.Checked := MVC.MultipassRendering;
    vvcmbTransparency.Value := MVC.Transparency;
    txbFileInformation.Text := MVC.FileInformation;
    txbUserComments.Text := MVC.Comments;

    if oParentWindow.MVC.Layer is TGIS_LayerVector then
    begin
      cmbScope.Text := MVC.Scope;
      vvedtCodePage.Value := MVC.CodePage;

      idx := TGIS_DynamicAggregatorFactory.Names.IndexOf(MVC.AggregationMethod);
      if idx > -1 then
        cmbAggMethod.ItemIndex := idx + 1
      else
        cmbAggMethod.ItemIndex := 0;
      cmbAggRadius.Value := MVC.AggregationRadius;
      vvedtAggThreshold.Value := MVC.AggregationThreshold;
    end
    else if oParentWindow.MVC.IsGrid then
    begin
      cmbInterpretation.ItemIndex := MVC.Interpretation;
    end
    else if oParentWindow.MVC.IsPixel then
      cmbInterpretation.ItemIndex := MVC.Interpretation;

    ckbUseConfig.Checked := MVC.UseConfig;
  finally
    unlockUpdates;
  end;
  if oParentWindow.MVC.Layer is TGIS_LayerVector then
  begin
    if cmbAggMethod.ItemIndex = 0 then
      MVC.doAggregateChange;
  end;
end;

procedure T_panelGeneral.Write;
begin
  MVC.Caption := edtCaption.Text;
  MVC.Basemap := ckbBasemap.Checked;
  MVC.CachedPaint := ckbCachedPaint.Checked;
  MVC.IgnoreShapeParams := ckbIgnoreShapeParams.Checked;
  MVC.MultipassRendering := ckbMultipassRendering.Checked;
  MVC.Transparency := FloorS(vvcmbTransparency.Value);
  MVC.Comments := txbUserComments.Text;

  if oParentWindow.MVC.Layer is TGIS_LayerVector then
  begin
    MVC.Scope := cmbScope.Text;
    MVC.CodePage := FloorS(vvedtCodePage.Value);

    if cmbAggMethod.ItemIndex > 0 then
      MVC.AggregationMethod := TGIS_DynamicAggregatorFactory.Names
        [cmbAggMethod.ItemIndex - 1]
    else
      MVC.AggregationMethod := cmbAggMethod.Text;
    MVC.AggregationRadius := cmbAggRadius.Value;
    MVC.AggregationThreshold := FloorS(vvedtAggThreshold.Value);

  end
  else if oParentWindow.MVC.IsGrid then
  begin
    MVC.Interpretation := cmbInterpretation.ItemIndex;
  end
  else if oParentWindow.MVC.IsPixel then
    MVC.Interpretation := cmbInterpretation.ItemIndex;

  MVC.UseConfig := ckbUseConfig.Checked;
end;

procedure T_panelGeneral.doCallback(_sender: TObject; _code: Integer);

  procedure do_cs;
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

  procedure do_agg;
  var
    agg: TGIS_DynamicAggregatorAbstract;
    aname: String;
  begin
    if cmbAggMethod.ItemIndex > 0 then
    begin
      lblAggThreshold.Enabled := True;
      edtAggThreshold.Enabled := True;
      lblAggRadius.Enabled := True;
      cmbAggRadius.Enabled := True;

      aname := TGIS_DynamicAggregatorFactory.Names[cmbAggMethod.ItemIndex - 1];
      if cmbAggMethod.Tag <> cmbAggMethod.ItemIndex then
      begin
        agg := TGIS_DynamicAggregatorFactory.CreateInstance(aname,
          TGIS_LayerVector(oParentWindow.MVC.Layer));
        try
          if assigned(agg) then
          begin
            cmbAggRadius.Value := agg.RadiusAsText;
            vvedtAggThreshold.Value := agg.Threshold;
          end;
        finally
          FreeObject(agg);
        end;
      end;
    end
    else
    begin
      lblAggThreshold.Enabled := False;
      edtAggThreshold.Enabled := False;
      lblAggRadius.Enabled := False;
      cmbAggRadius.Enabled := False;
    end;
    cmbAggMethod.Tag := cmbAggMethod.ItemIndex;
  end;

begin
  case _code of
    1:
      do_cs;
    2:
      ;
    3:
      do_agg;
  end;
end;

procedure T_panelGeneral.doCSClick(_sender: TObject);
begin
  MVC.doCSClick;
end;

procedure T_panelGeneral.doControlChange(_sender: TObject);
begin
  MVC.doControlChange;
end;

procedure T_panelGeneral.doAggregateChange(_sender: TObject);
begin
  MVC.doAggregateChange;
end;

{$ENDREGION}
{$REGION 'T_Panel3D'}

function T_panel3D.fget_HasPreview: Boolean;
begin
  Result := MVC.HasPreview;
end;

procedure T_panel3D.init;
var
  t: Integer;
  bd: TBiDiMode;
  Anchors: TAnchors;
begin
  inherited;

  bd := oParentWindow.BiDiMode;
  if bd = bdRightToLeft then
    Anchors := [akRight, akTop]
  else
    Anchors := [akLeft, akTop];

  ItemText := _rsrc(GIS_RS_LEGEND_PAG_3D);

  MVC := oParentWindow.MVC.View3D;
  MVC.Callback := doCallback;

  hasDEM := oParentWindow.MVC.IsVector or oParentWindow.MVC.IsGrid;

  t := ppiFix(16);

  rbnAs2D := TRadioButton.Create(Self.Panel);
  rbnAs2D.Parent := Self.Panel;
  rbnAs2D.Top := t;
  PlaceControl(bd, nil, rbnAs2D, ppiFix(LEFT_3COL_1), ppiFix(256));
  rbnAs2D.Caption := _rsrc(GIS_RS_LEGEND_PRM_TREAT_AS_2D);
  rbnAs2D.Checked := True;
  rbnAs2D.Anchors := Anchors;
  rbnAs2D.OnClick := doAs2DClick;

  t := t + rbnAs2D.Height + ppiFix(8);

  if hasDEM then
  begin
    rbnAsDEM := TRadioButton.Create(Self.Panel);
    rbnAsDEM.Parent := Self.Panel;
    rbnAsDEM.Top := t;
    PlaceControl(bd, nil, rbnAsDEM, ppiFix(LEFT_3COL_1), ppiFix(256));
    rbnAsDEM.Caption := _rsrc(GIS_RS_LEGEND_PRM_TREAT_AS_DEM);
    rbnAsDEM.Anchors := Anchors;
    rbnAsDEM.OnClick := doAsDEMClick;

    t := t + rbnAsDEM.Height + ppiFix(8);
  end;

  rbnAs3D := TRadioButton.Create(Self.Panel);
  rbnAs3D.Parent := Self.Panel;
  rbnAs3D.Top := t;
  PlaceControl(bd, nil, rbnAs3D, ppiFix(LEFT_3COL_1), ppiFix(256));
  rbnAs3D.Caption := _rsrc(GIS_RS_LEGEND_PRM_TREAT_AS_3DSHAPES);
  rbnAs3D.Anchors := Anchors;
  rbnAs3D.OnClick := doAs3DClick;

  t := t + rbnAs3D.Height + ppiFix(16);

  lblNormalizedZ := TLabel.Create(Self.Panel);
  lblNormalizedZ.Parent := Self.Panel;
  lblNormalizedZ.Top := t;
  PlaceControl(bd, nil, lblNormalizedZ, 2 * ppiFix(LEFT_3COL_1),
    lblNormalizedZ.Width);
  lblNormalizedZ.Anchors := Anchors;
  lblNormalizedZ.Caption := _rsrc(GIS_RS_LEGEND_PRM_NORMALIZED_Z_LBL);

  if oParentWindow.MVC.IsVector then
  begin
    lblNormalizedM := TLabel.Create(Self.Panel);
    lblNormalizedM.Parent := Self.Panel;
    lblNormalizedM.Top := t;
    PlaceControl(bd, nil, lblNormalizedM, ppiFix(LEFT_3COL_1) +
      ppiFix(LEFT_2COL_2), lblNormalizedM.Width);
    lblNormalizedM.Anchors := Anchors;
    lblNormalizedM.Caption := _rsrc(GIS_RS_LEGEND_PRM_NORMALIZED_M_LBL);
  end;

  t := t + lblNormalizedZ.Height + ppiFix(4);

  cmbNormalizedZ := TComboBox.Create(Self.Panel);
  cmbNormalizedZ.Parent := Self.Panel;
  cmbNormalizedZ.Top := t;
  PlaceControl(bd, nil, cmbNormalizedZ, 2 * ppiFix(LEFT_3COL_1),
    cmbNormalizedZ.Width);
  cmbNormalizedZ.Anchors := Anchors;
  cmbNormalizedZ.Style := csDropDownList;
  cmbNormalizedZ.Items.BeginUpdate;
  cmbNormalizedZ.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_NORMALIZED_Z_OFF));
  cmbNormalizedZ.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_NORMALIZED_Z_MAX));
  cmbNormalizedZ.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_NORMALIZED_Z_RANGE));
  cmbNormalizedZ.ItemIndex := 0;
  cmbNormalizedZ.Items.EndUpdate;

  if oParentWindow.MVC.IsVector then
  begin
    cmbNormalizedM := TComboBox.Create(Self.Panel);
    cmbNormalizedM.Parent := Self.Panel;
    cmbNormalizedM.Top := t;
    PlaceControl(bd, nil, cmbNormalizedM, ppiFix(LEFT_3COL_1) +
      ppiFix(LEFT_2COL_2), cmbNormalizedM.Width);
    cmbNormalizedM.Anchors := Anchors;
    cmbNormalizedM.Style := csDropDownList;
    cmbNormalizedM.Items.BeginUpdate;
    cmbNormalizedM.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_NORMALIZED_M_OFF));
    cmbNormalizedM.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_NORMALIZED_M_MAX));
    cmbNormalizedM.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_NORMALIZED_M_RANGE));
    cmbNormalizedM.ItemIndex := 0;
    cmbNormalizedM.Items.EndUpdate;
  end;

  t := t + cmbNormalizedZ.Height + ppiFix(8);

  lblScaleZ := TLabel.Create(Self.Panel);
  lblScaleZ.Parent := Self.Panel;
  lblScaleZ.Top := t;
  PlaceControl(bd, nil, lblScaleZ, 2 * ppiFix(LEFT_3COL_1), lblScaleZ.Width);
  lblScaleZ.Anchors := Anchors;
  lblScaleZ.Caption := _rsrc(GIS_RS_LEGEND_PRM_SCALE_Z);

  if oParentWindow.MVC.IsVector then
  begin
    lblScaleM := TLabel.Create(Self.Panel);
    lblScaleM.Parent := Self.Panel;
    lblScaleM.Top := t;
    PlaceControl(bd, nil, lblScaleM, ppiFix(LEFT_3COL_1) + ppiFix(LEFT_2COL_2),
      lblScaleM.Width);
    lblScaleM.Anchors := Anchors;
    lblScaleM.Caption := _rsrc(GIS_RS_LEGEND_PRM_SCALE_M);
  end;

  t := t + lblScaleZ.Height + ppiFix(4);

  speScaleZ := TEdit.Create(Self.Panel);
  speScaleZ.Parent := Self.Panel;
  speScaleZ.Top := t;
  PlaceControl(bd, nil, speScaleZ, 2 * ppiFix(LEFT_3COL_1), speScaleZ.Width);
  speScaleZ.Anchors := Anchors;

  vvspeScaleZ := TGIS_ValueValidatorEditHelper.Create(speScaleZ);
  vvspeScaleZ.MinVal := -999.0;
  vvspeScaleZ.MaxVal := 999.0;

  if oParentWindow.MVC.IsVector then
  begin
    speScaleM := TEdit.Create(Self.Panel);
    speScaleM.Parent := Self.Panel;
    speScaleM.Top := t;
    PlaceControl(bd, nil, speScaleM, ppiFix(LEFT_3COL_1) + ppiFix(LEFT_2COL_2),
      speScaleM.Width);
    speScaleM.Anchors := Anchors;

    vvspeScaleM := TGIS_ValueValidatorEditHelper.Create(speScaleM);
    vvspeScaleM.MinVal := -999.0;
    vvspeScaleM.MaxVal := 999.0;
  end;

  t := t + speScaleZ.Height + ppiFix(16);

  lblFalseZ := TLabel.Create(Self.Panel);
  lblFalseZ.Parent := Self.Panel;
  lblFalseZ.Top := t;
  PlaceControl(bd, nil, lblFalseZ, 2 * ppiFix(LEFT_3COL_1), lblFalseZ.Width);
  lblFalseZ.Anchors := Anchors;
  lblFalseZ.Caption := _rsrc(GIS_RS_LEGEND_PRM_FALSE_Z);

  if oParentWindow.MVC.IsVector then
  begin
    lblFalseM := TLabel.Create(Self.Panel);
    lblFalseM.Parent := Self.Panel;
    lblFalseM.Top := t;
    PlaceControl(bd, nil, lblFalseM, ppiFix(LEFT_3COL_1) + ppiFix(LEFT_2COL_2),
      lblFalseM.Width);
    lblFalseM.Anchors := Anchors;
    lblFalseM.Caption := _rsrc(GIS_RS_LEGEND_PRM_FALSE_M);
  end;

  t := t + lblFalseZ.Height + ppiFix(4);

  cmbFalseZ := TGIS_SizeComboBox.Create(Self.Panel);
  cmbFalseZ.Parent := Self.Panel;
  cmbFalseZ.Top := t;
  PlaceControl(bd, nil, cmbFalseZ, 2 * ppiFix(LEFT_3COL_1), speScaleZ.Width);
  cmbFalseZ.Anchors := Anchors;
  cmbFalseZ.FillRealWorldUnits(oParentWindow.MVC.IsVector);
  cmbFalseZ.CustomEvent := doCustomRWUnits;

  if not oParentWindow.MVC.IsVector then
    exit;

  cmbFalseM := TGIS_SizeComboBox.Create(Self.Panel);
  cmbFalseM.Parent := Self.Panel;
  cmbFalseM.Top := t;
  PlaceControl(bd, nil, cmbFalseM, ppiFix(LEFT_3COL_1) + ppiFix(LEFT_2COL_2),
    speScaleM.Width);
  cmbFalseM.Anchors := Anchors;
  cmbFalseM.FillRealWorldUnits(oParentWindow.MVC.IsVector);
  cmbFalseM.CustomEvent := doCustomRWUnits;

  t := t + cmbFalseZ.Height + ppiFix(16);

  lblAdjustZ := TLabel.Create(Self.Panel);
  lblAdjustZ.Parent := Self.Panel;
  lblAdjustZ.Top := t;
  PlaceControl(bd, nil, lblAdjustZ, 2 * ppiFix(LEFT_3COL_1), lblAdjustZ.Width);
  lblAdjustZ.Anchors := Anchors;
  lblAdjustZ.Caption := _rsrc(GIS_RS_LEGEND_PRM_ADJUST_Z);

  lblAdjustBasement := TLabel.Create(Self.Panel);
  lblAdjustBasement.Parent := Self.Panel;
  lblAdjustBasement.Top := t;
  lblAdjustBasement.Left := ppiFix(LEFT_3COL_1) + ppiFix(LEFT_2COL_2);
  PlaceControl(bd, nil, lblAdjustBasement, ppiFix(LEFT_3COL_1) +
    ppiFix(LEFT_2COL_2), lblAdjustBasement.Width);
  lblAdjustBasement.Anchors := Anchors;
  lblAdjustBasement.Caption := _rsrc(GIS_RS_LEGEND_PRM_ADJUST_BASEMENT);

  t := t + lblAdjustZ.Height + ppiFix(4);

  cmbAdjustZ := TComboBox.Create(Self.Panel);
  cmbAdjustZ.Parent := Self.Panel;
  cmbAdjustZ.Top := t;
  PlaceControl(bd, nil, cmbAdjustZ, 2 * ppiFix(LEFT_3COL_1), speScaleZ.Width);
  cmbAdjustZ.Anchors := Anchors;
  cmbAdjustZ.Style := csDropDownList;
  cmbAdjustZ.Items.BeginUpdate;
  cmbAdjustZ.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_RELATIVE_0));
  cmbAdjustZ.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_RELATIVE_DEM));
  cmbAdjustZ.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_EQUAL_DEM));
  cmbAdjustZ.ItemIndex := 2;
  cmbAdjustZ.Items.EndUpdate;

  cmbAdjustBasement := TComboBox.Create(Self.Panel);
  cmbAdjustBasement.Parent := Self.Panel;
  cmbAdjustBasement.Top := t;
  PlaceControl(bd, nil, cmbAdjustBasement, ppiFix(LEFT_3COL_1) +
    ppiFix(LEFT_2COL_2), speScaleM.Width);
  cmbAdjustBasement.Anchors := Anchors;
  cmbAdjustBasement.Style := csDropDownList;
  cmbAdjustBasement.Items.BeginUpdate;
  cmbAdjustBasement.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_OFF));
  cmbAdjustBasement.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_LOWEST));
  cmbAdjustBasement.ItemIndex := 0;
  cmbAdjustBasement.Items.EndUpdate;

  if bd = bdRightToLeft then
  begin
    cmbNormalizedZ.Left := cmbNormalizedZ.Left + cmbNormalizedZ.Width -
      speScaleZ.Width;
    cmbNormalizedM.Left := cmbNormalizedM.Left + cmbNormalizedM.Width -
      speScaleM.Width;
  end;
  cmbNormalizedZ.Width := speScaleZ.Width;
  cmbNormalizedM.Width := speScaleM.Width;
end;

procedure T_panel3D.Read;
var
  prt: TWinControl;
begin
  lockUpdates;
  try
    case MVC.TreatLayerAs of
      TGIS_3DLayerType.Off:
        begin
          rbnAs2D.Checked := True;
          MVC.doAs2DClick;
        end;
      TGIS_3DLayerType.Dem:
        begin
          if hasDEM then
          begin
            rbnAsDEM.Checked := True;
            MVC.doAsDEMClick;
          end;
        end;
      TGIS_3DLayerType.Shapes:
        begin
          rbnAs3D.Checked := True;
          MVC.doAs3DClick;
        end;
    end;

    cmbNormalizedZ.Items.BeginUpdate;
    cmbNormalizedZ.ItemIndex := Integer(MVC.NormalizedZ);
    cmbNormalizedZ.Items.EndUpdate;

    vvspeScaleZ.Value := 100 * MVC.ScaleZ;
    cmbFalseZ.Value := MVC.FalseZ;

    if not oParentWindow.MVC.IsVector then
      exit;

    cmbNormalizedM.Items.BeginUpdate;
    cmbNormalizedM.ItemIndex := Integer(MVC.NormalizedM);
    cmbNormalizedM.Items.EndUpdate;
    vvspeScaleM.Value := 100 * MVC.ScaleM;
    cmbFalseM.Value := MVC.FalseM;

    cmbAdjustZ.Items.BeginUpdate;
    case MVC.AdjustZ of
      TGIS_3DGroundType.AboveZero:
        cmbAdjustZ.ItemIndex := 0;
      TGIS_3DGroundType.AboveDem:
        cmbAdjustZ.ItemIndex := 1;
      TGIS_3DGroundType.OnDem:
        cmbAdjustZ.ItemIndex := 2;
    end;
    cmbAdjustZ.Items.EndUpdate;

    cmbAdjustBasement.Items.BeginUpdate;
    case MVC.AdjustBasement of
      TGIS_3DBasementType.Off:
        cmbAdjustBasement.ItemIndex := 0;
      TGIS_3DBasementType.Lowest:
        cmbAdjustBasement.ItemIndex := 1;
    end;
    cmbAdjustBasement.Items.EndUpdate;
  finally
    unlockUpdates;
  end;
end;

procedure T_panel3D.Write;
begin
  if rbnAs2D.Checked then
    MVC.TreatLayerAs := TGIS_3DLayerType.Off
  else if hasDEM and rbnAsDEM.Checked then
    MVC.TreatLayerAs := TGIS_3DLayerType.Dem
  else if rbnAs3D.Checked then
    MVC.TreatLayerAs := TGIS_3DLayerType.Shapes;

  MVC.NormalizedZ := TGIS_3DNormalizationType(cmbNormalizedZ.ItemIndex);
  MVC.ScaleZ := vvspeScaleZ.Value / 100;
  MVC.FalseZ := cmbFalseZ.Value;

  if not oParentWindow.MVC.IsVector then
    exit;

  MVC.NormalizedM := TGIS_3DNormalizationType(cmbNormalizedM.ItemIndex);
  MVC.ScaleM := vvspeScaleM.Value / 100;
  MVC.FalseM := cmbFalseM.Value;

  case cmbAdjustZ.ItemIndex of
    0:
      MVC.AdjustZ := TGIS_3DGroundType.AboveZero;
    1:
      MVC.AdjustZ := TGIS_3DGroundType.AboveDem;
    2:
      MVC.AdjustZ := TGIS_3DGroundType.OnDem;
  end;

  case cmbAdjustBasement.ItemIndex of
    0:
      MVC.AdjustBasement := TGIS_3DBasementType.Off;
    1:
      MVC.AdjustBasement := TGIS_3DBasementType.Lowest;
  end;
end;

procedure T_panel3D.doCallback(_sender: TObject; _code: Integer);

  procedure do_2d;
  begin
    lblNormalizedZ.Visible := False;
    cmbNormalizedZ.Visible := False;
    lblScaleZ.Visible := False;
    speScaleZ.Visible := False;
    lblFalseZ.Visible := False;
    cmbFalseZ.Visible := False;

    if not oParentWindow.MVC.IsVector then
      exit;

    lblNormalizedM.Visible := False;
    cmbNormalizedM.Visible := False;
    lblScaleM.Visible := False;
    speScaleM.Visible := False;
    lblFalseM.Visible := False;
    cmbFalseM.Visible := False;
    lblAdjustZ.Visible := False;
    lblAdjustBasement.Visible := False;
    cmbAdjustZ.Visible := False;
    cmbAdjustBasement.Visible := False;
  end;

  procedure do_dem;
  begin
    lblNormalizedZ.Visible := True;
    cmbNormalizedZ.Visible := True;
    lblScaleZ.Visible := True;
    speScaleZ.Visible := True;
    lblFalseZ.Visible := True;
    cmbFalseZ.Visible := True;

    if not oParentWindow.MVC.IsVector then
      exit;

    lblNormalizedM.Visible := False;
    cmbNormalizedM.Visible := False;
    lblScaleM.Visible := False;
    speScaleM.Visible := False;
    lblFalseM.Visible := False;
    cmbFalseM.Visible := False;
    lblAdjustZ.Visible := False;
    lblAdjustBasement.Visible := False;
    cmbAdjustZ.Visible := False;
    cmbAdjustBasement.Visible := False;
  end;

  procedure do_3d;
  begin
    lblNormalizedZ.Visible := True;
    cmbNormalizedZ.Visible := True;
    lblScaleZ.Visible := True;
    speScaleZ.Visible := True;
    lblFalseZ.Visible := True;
    cmbFalseZ.Visible := True;

    if not oParentWindow.MVC.IsVector then
      exit;

    lblNormalizedM.Visible := True;
    cmbNormalizedM.Visible := True;
    lblScaleM.Visible := True;
    speScaleM.Visible := True;
    lblFalseM.Visible := True;
    cmbFalseM.Visible := True;
    lblAdjustZ.Visible := True;
    lblAdjustBasement.Visible := True;
    cmbAdjustZ.Visible := True;
    cmbAdjustBasement.Visible := True;
  end;

begin
  case _code of
    1:
      do_2d;
    2:
      do_dem;
    3:
      do_3d;
  end;
end;

procedure T_panel3D.doAs2DClick(_sender: TObject);
begin
  MVC.doAs2DClick;
end;

procedure T_panel3D.doAsDEMClick(_sender: TObject);
begin
  MVC.doAsDEMClick;
end;

procedure T_panel3D.doAs3DClick(_sender: TObject);
begin
  MVC.doAs3DClick;
end;

{$ENDREGION}
{$REGION 'T_panelSections'}

function lstParamsPrepare(const _visible: Boolean; const _minscale: String;
  const _maxscale: String; const _query: String; const _renderer: String;
  const _legend: String): String;
begin
  if _visible then
    Result := _rsrc(GIS_RS_LEGEND_SEC_VISIBLE)
  else
    Result := _rsrc(GIS_RS_LEGEND_SEC_HIDDEN);

  if (not IsStringEmpty(_minscale)) or (not IsStringEmpty(_maxscale)) then
    Result := Result + Format(' %s[ %s..%s ]', [_rsrc(GIS_RS_LEGEND_SEC_SCALE),
      _minscale, _maxscale]);
  if not IsStringEmpty(_query) then
    Result := Result + Format(' %s[ %s ]', [_rsrc(GIS_RS_LEGEND_SEC_QUERY),
      _query]);
  if not IsStringEmpty(_renderer) then
    Result := Result + Format(' %s[ %s ]', [_rsrc(GIS_RS_LEGEND_SEC_RENDERER),
      _renderer]);
  if not IsStringEmpty(_legend) then
    Result := Format(' %s', [_legend]);

end;

function prepareScale(const oval: Double): String; overload;
begin
  if oval = 0 then
    Result := ''
  else if oval > 1E300 then
    Result := ''
  else if oval < 1 then
    Result := Format('1:%.0f', [1 / oval])
  else if oval >= 1 then
    Result := Format('%.0f:1', [oval / 1])
end;

function split(const _str: String; const _separator: array of Char)
  : TArray<String>;
var
  res: TStringDynArray;
  i: Integer;
begin
{$IFDEF LEVEL_XE3_RTL}
  Result := _str.split(_separator)
{$ELSE}
  res := System.StrUtils.SplitString(_str, _separator);
  SetLength(Result, length(res));
  for i := 0 to length(res) - 1 do
    Result[i] := res[i];
{$ENDIF}
end;

function prepareScale(const oval: String; const def: Double): Double; overload;
var
  arr: TArray<String>;
begin
  if IsStringEmpty(oval) then
    Result := def
  else
  begin
    arr := split(oval, [':']);
    if length(arr) > 1 then
      Result := DotStrToFloat(arr[1])
    else
      Result := DotStrToFloat(arr[0]);
    if Result <> 0 then
      Result := 1 / Result
    else
      Result := def ;
  end;
end;

function T_panelSections.fget_HasPreview: Boolean;
begin
  Result := False;
end;

procedure T_panelSections.lvSectionsDblClick(_sender: TObject);
var
  nd: TTreeNode;
  i: Integer;
begin
  nd := oParentWindow.tvPages.Items.Item[1].getFirstChild;
  for i := 0 to lvSections.ItemIndex - 1 do
    nd := nd.getNextSibling;

  if assigned(nd) then
    oParentWindow.tvPages.Selected := nd;
end;

procedure T_panelSections.PreparePreview(const _viewer: IGIS_Viewer);
begin
  inherited;

end;

procedure T_panelSections.init;
var
  bd: TBiDiMode;
begin
  inherited;

  bd := oParentWindow.BiDiMode;
  ItemText := _rsrc(GIS_RS_LEGEND_PAG_SECTIONS);

  gpbSection := TGroupBox.Create(Self.Panel);
  gpbSection.Parent := Self.Panel;
  gpbSection.Top := ppiFix(4);
  gpbSection.Height := ppiFix(200);
  PlaceControl(bd, nil, gpbSection, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbSection.Anchors := [akRight, akTop]
  else
    gpbSection.Anchors := [akLeft, akTop];
  gpbSection.Caption := _rsrc(GIS_RS_LEGEND_PAG_SECTIONS);

  lvSections := TListBox.Create(gpbSection);
  lvSections.Parent := gpbSection;
  lvSections.Top := ppiFix(24);
  lvSections.Height := ppiFix(200 - 24 - 16);

  PlaceControl(bd, nil, lvSections, ppiFix(LEFT_3COL_1),
    ppiFix(WIDTH_NORMAL - 32));
  if bd = bdRightToLeft then
    lvSections.Anchors := [akRight, akTop]
  else
    lvSections.Anchors := [akLeft, akTop];

  lvSections.OnDblClick := lvSectionsDblClick;

end;

procedure T_panelSections.Read;
begin
  // nothing to read
end;

procedure T_panelSections.Prepare;
var
  i: Integer;
  MVC: TGIS_ControlLegendFormMVC_Section;
  idx: Integer;
  qstr: String;
begin
  idx := 0;
  lvSections.Items.BeginUpdate;
  try
    lvSections.Items.Clear;
    idx := oParentWindow.MVC.SectionIndex;
    for i := 0 to oParentWindow.MVC.SectionCount - 1 do
    begin
      oParentWindow.MVC.SectionIndex := i;
      MVC := oParentWindow.MVC.Section;

      if oParentWindow.MVC.IsVector then
        qstr := MVC.Query
      else
        qstr := '';

      lvSections.Items.Add(lstParamsPrepare(MVC.Visible,
        prepareScale(MVC.MinScale), prepareScale(MVC.MaxScale), qstr, '', ''));
    end;
  finally
    lvSections.Items.EndUpdate;
    oParentWindow.MVC.SectionIndex := idx;
  end;
end;

procedure T_panelSections.UpdatePreview;
begin
  inherited;

end;

procedure T_panelSections.Write;
begin
  // nothing to write
end;
{$ENDREGION}
{$REGION 'T_panelSection'}

function T_panelSection.fget_HasPreview: Boolean;
begin
  Result := MVC.HasPreview;
end;

procedure T_panelSection.init;
var
  t: Integer;
  bd: TBiDiMode;
begin
  inherited;

  bd := oParentWindow.BiDiMode;

  ItemText := _rsrc(GIS_RS_LEGEND_SEC_VISIBLE);

  MVC := oParentWindow.MVC.Section;
  MVC.Callback := doCallback;

  gpbSection := TGroupBox.Create(Self.Panel);
  gpbSection.Parent := Self.Panel;
  gpbSection.Top := ppiFix(4);
  gpbSection.Height := ppiFix(200);
  PlaceControl(bd, nil, gpbSection, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbSection.Anchors := [akRight, akTop]
  else
    gpbSection.Anchors := [akLeft, akTop];
  gpbSection.Caption := _rsrc(GIS_RS_LEGEND_PAG_SECTION);

  t := ppiFix(24);

  ckbVisible := TCheckBox.Create(gpbSection);
  ckbVisible.Parent := gpbSection;
  ckbVisible.Top := t;
  PlaceControl(bd, nil, ckbVisible, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_2COL));
  ckbVisible.Caption := _rsrc(GIS_RS_LEGEND_PRM_VISIBLE);
  ckbVisible.Checked := True;
  ckbVisible.OnClick := doVisibleClick;

  t := t + ckbVisible.Height + ppiFix(8);

  lblMinScale := TLabel.Create(gpbSection);
  lblMinScale.Parent := gpbSection;
  lblMinScale.Top := t;
  PlaceControl(bd, nil, lblMinScale, ppiFix(LEFT_3COL_1), lblMinScale.Width);
  lblMinScale.Caption := _rsrc(GIS_RS_LEGEND_PRM_MINSCALE);

  t := t + lblMinScale.Height + ppiFix(4);

  cmbMinScale := TComboBox.Create(gpbSection);
  cmbMinScale.Parent := gpbSection;
  cmbMinScale.Top := t + ppiFix(1);
  PlaceControl(bd, nil, cmbMinScale, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbMinScale.Text := '';
  cmbMinScale.Items.BeginUpdate;
  cmbMinScale.Items.Add('1:500');
  cmbMinScale.Items.Add('1:1000');
  cmbMinScale.Items.Add('1:2500');
  cmbMinScale.Items.Add('1:5000');
  cmbMinScale.Items.Add('1:10 000');
  cmbMinScale.Items.Add('1:12 000');
  cmbMinScale.Items.Add('1:25 000');
  cmbMinScale.Items.Add('1:50 000');
  cmbMinScale.Items.Add('1:100 000');
  cmbMinScale.Items.Add('1:250 000');
  cmbMinScale.Items.Add('1:500 000');
  cmbMinScale.Items.Add('1:1 000 000');
  cmbMinScale.Items.Add('1:2 000 000');
  cmbMinScale.Items.Add('1:5 000 000');
  cmbMinScale.Items.Add('1:10 000 000');
  cmbMinScale.Items.Add('1:25 000 000');
  cmbMinScale.Items.Add('1:50 000 000');
  cmbMinScale.Items.Add('1:100 000 000');
  cmbMinScale.Items.EndUpdate;
  cmbMinScale.OnChange := doMinScaleChange;

  btnMinScaleCur := TButton.Create(gpbSection);
  btnMinScaleCur.Parent := gpbSection;
  btnMinScaleCur.Top := t;
  btnMinScaleCur.Height := ppiFix(23);
  PlaceControl(bd, cmbMinScale, btnMinScaleCur, ppiFix(2), ppiFix(70));
  btnMinScaleCur.Caption := _rsrc(GIS_RS_LEGEND_PRM_SCALE_CURRENT);
  btnMinScaleCur.OnClick := doMinScaleClick;

  btnMinScaleClr := T_speedButton.Create(gpbSection);
  btnMinScaleClr.Parent := gpbSection;
  btnMinScaleClr.Top := t;
  btnMinScaleClr.Height := ppiFix(23);
  PlaceControl(bd, btnMinScaleCur, btnMinScaleClr, ppiFix(2), ppiFix(23));
  btnMinScaleClr.OnClick := doMinScaleClear;
  T_speedButton(btnMinScaleClr).Images := oParentWindow.imgListSclN;
  T_speedButton(btnMinScaleClr).DisabledImages := oParentWindow.imgListSclD;
  T_speedButton(btnMinScaleClr).ImageIndex := 1;

  t := t + lblMinScale.Height + ppiFix(16);

  lblMaxScale := TLabel.Create(gpbSection);
  lblMaxScale.Parent := gpbSection;
  lblMaxScale.Top := t;
  PlaceControl(bd, nil, lblMaxScale, ppiFix(LEFT_3COL_1), lblMaxScale.Width);
  lblMaxScale.Caption := _rsrc(GIS_RS_LEGEND_PRM_MAXSCALE);

  t := t + lblMinScale.Height + ppiFix(4);

  cmbMaxScale := TComboBox.Create(gpbSection);
  cmbMaxScale.Parent := gpbSection;
  cmbMaxScale.Top := t + 1;
  PlaceControl(bd, nil, cmbMaxScale, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbMaxScale.Text := '';
  cmbMaxScale.Items.BeginUpdate;
  cmbMaxScale.Items.Add('1:500');
  cmbMaxScale.Items.Add('1:1000');
  cmbMaxScale.Items.Add('1:2500');
  cmbMaxScale.Items.Add('1:5000');
  cmbMaxScale.Items.Add('1:10 000');
  cmbMaxScale.Items.Add('1:12 000');
  cmbMaxScale.Items.Add('1:25 000');
  cmbMaxScale.Items.Add('1:50 000');
  cmbMaxScale.Items.Add('1:100 000');
  cmbMaxScale.Items.Add('1:250 000');
  cmbMaxScale.Items.Add('1:500 000');
  cmbMaxScale.Items.Add('1:1 000 000');
  cmbMaxScale.Items.Add('1:2 000 000');
  cmbMaxScale.Items.Add('1:5 000 000');
  cmbMaxScale.Items.Add('1:10 000 000');
  cmbMaxScale.Items.Add('1:25 000 000');
  cmbMaxScale.Items.Add('1:50 000 000');
  cmbMaxScale.Items.Add('1:100 000 000');
  cmbMaxScale.Items.EndUpdate;
  cmbMaxScale.OnChange := doMaxScaleChange;

  btnMaxScaleCur := TButton.Create(gpbSection);
  btnMaxScaleCur.Parent := gpbSection;
  btnMaxScaleCur.Top := t;
  btnMaxScaleCur.Height := ppiFix(23);
  PlaceControl(bd, cmbMaxScale, btnMaxScaleCur, ppiFix(2), ppiFix(70));
  btnMaxScaleCur.Caption := _rsrc(GIS_RS_LEGEND_PRM_SCALE_CURRENT);
  btnMaxScaleCur.OnClick := doMaxScaleClick;

  btnMaxScaleClr := T_speedButton.Create(gpbSection);
  btnMaxScaleClr.Parent := gpbSection;
  btnMaxScaleClr.Top := t;
  btnMaxScaleClr.Height := ppiFix(23);
  PlaceControl(bd, btnMaxScaleCur, btnMaxScaleClr, ppiFix(2), ppiFix(23));
  btnMaxScaleClr.OnClick := doMaxScaleClear;
  T_speedButton(btnMaxScaleClr).Images := oParentWindow.imgListSclN;
  T_speedButton(btnMaxScaleClr).DisabledImages := oParentWindow.imgListSclD;
  T_speedButton(btnMaxScaleClr).ImageIndex := 1;

  gpbSection.Height := t + cmbMaxScale.Height + ppiFix(16);
end;

procedure T_panelSection.Read;
begin
  lockUpdates;
  try
    ckbVisible.Checked := MVC.Visible;
    cmbMinScale.Text := prepareScale(MVC.MinScale);
    cmbMaxScale.Text := prepareScale(MVC.MaxScale);

    MVC.doMinScaleChange;
    MVC.doMaxScaleChange;
  finally
    unlockUpdates;
  end;
end;

procedure T_panelSection.updateNode;
begin
  oParentWindow.pnlTop.Caption := lstParamsPrepare(ckbVisible.Checked,
    cmbMinScale.Text, cmbMaxScale.Text, '', '', '');
end;

procedure T_panelSection.Write;

begin
  MVC.Visible := ckbVisible.Checked;
  MVC.MinScale := prepareScale(cmbMinScale.Text, 0);
  MVC.MaxScale := prepareScale(cmbMaxScale.Text, GIS_MAX_DOUBLE);
end;

procedure T_panelSection.doCallback(_sender: TObject; _code: Integer);

  procedure do_visible_click;
  begin
    updateNode;
  end;

  procedure do_min_click;
  begin
    cmbMinScale.Text := prepareScale(oParentWindow.oViewer.Scale * 0.99);
    btnMinScaleClr.Enabled := True;
    updateNode;
  end;

  procedure do_min_clear;
  begin
    cmbMinScale.Text := '';
    btnMinScaleClr.Enabled := False;
    updateNode;
  end;

  procedure do_min_change;
  begin
    btnMinScaleClr.Enabled := not IsStringEmpty(cmbMinScale.Text);
    updateNode;
  end;

  procedure do_max_click;
  begin
    cmbMaxScale.Text := prepareScale(oParentWindow.oViewer.Scale * 1.01);
    btnMaxScaleClr.Enabled := True;
    updateNode;
  end;

  procedure do_max_clear;
  begin
    cmbMaxScale.Text := '';
    btnMaxScaleClr.Enabled := False;
    updateNode;
  end;

  procedure do_max_change;
  begin
    btnMaxScaleClr.Enabled := not IsStringEmpty(cmbMaxScale.Text);
    updateNode;
  end;

begin
  case _code of
    0:
      do_visible_click;
    1:
      do_min_click;
    2:
      do_min_clear;
    3:
      do_min_change;
    4:
      do_max_click;
    5:
      do_max_clear;
    6:
      do_max_change;
  end;
end;

procedure T_panelSection.doVisibleClick(_sender: TObject);
begin
  MVC.doVisibleClick;
end;

procedure T_panelSection.doMinScaleClick(_sender: TObject);
begin
  MVC.doMinScaleClick;
end;

procedure T_panelSection.doMinScaleClear(_sender: TObject);
begin
  MVC.doMinScaleClear;
end;

procedure T_panelSection.doMinScaleChange(_sender: TObject);
begin
  MVC.doMinScaleChange;
end;

procedure T_panelSection.doMaxScaleClick(_sender: TObject);
begin
  MVC.doMaxScaleClick;
end;

procedure T_panelSection.doMaxScaleClear(_sender: TObject);
begin
  MVC.doMaxScaleClear;
end;

procedure T_panelSection.doMaxScaleChange(_sender: TObject);
begin
  MVC.doMaxScaleChange;
end;
{$ENDREGION}
{$REGION 'T_panelSectionVector'}

procedure T_panelSectionVector.init;
var
  t: Integer;
  bd: TBiDiMode;
begin
  inherited;

  bd := oParentWindow.BiDiMode;

  ItemText := _rsrc(GIS_RS_LEGEND_SEC_VISIBLE);

  t := btnMaxScaleCur.Top + btnMaxScaleCur.Height + ppiFix(16);

  lblQuery := TLabel.Create(gpbSection);
  lblQuery.Parent := gpbSection;
  lblQuery.Top := t;
  PlaceControl(bd, nil, lblQuery, ppiFix(LEFT_3COL_1), lblQuery.Width);
  lblQuery.Caption := _rsrc(GIS_RS_LEGEND_PRM_QUERY);

  t := t + lblQuery.Height + ppiFix(4);

  cmbQuery := TComboBox.Create(gpbSection);
  cmbQuery.Parent := gpbSection;
  cmbQuery.Top := t;
  PlaceControl(bd, nil, cmbQuery, ppiFix(LEFT_3COL_1), -1);
  cmbQuery.ItemIndex := -1;
  cmbQuery.OnChange := doQueryChange;
  oParentWindow.fillComboBoxWithFields(cmbQuery);

  t := t + cmbQuery.Height + ppiFix(8);

  lblLegend := TLabel.Create(gpbSection);
  lblLegend.Parent := gpbSection;
  lblLegend.Top := t;
  PlaceControl(bd, nil, lblLegend, ppiFix(LEFT_3COL_1), lblLegend.Width);
  lblLegend.Caption := _rsrc(GIS_RS_LEGEND_PRM_LEGEND);

  t := t + lblLegend.Height + ppiFix(4);

  edtLegend := TEdit.Create(gpbSection);
  edtLegend.Parent := gpbSection;
  edtLegend.Top := t;
  PlaceControl(bd, nil, edtLegend, ppiFix(LEFT_3COL_1), -1);
  edtLegend.Text := '';
  edtLegend.OnChange := doLegendChange;
  wasLegendEdited := False;

  gpbSection.Height := t + edtLegend.Height + ppiFix(24);
end;

procedure T_panelSectionVector.Read;
begin
  inherited;

  lockUpdates;
  try
    cmbQuery.Text := MVC.Query;
    edtLegend.Text := MVC.Legend;

    MVC.doVisibleClick;
  finally
    unlockUpdates;
  end;
end;

procedure T_panelSectionVector.updateNode;
begin
  if IsStringEmpty(edtLegend.Text) then
    ItemText := Format('%s %d', [_rsrc(GIS_RS_LEGEND_PAG_SECTION),
      oParentWindow.MVC.SectionIndex + 1])
  else
    ItemText := edtLegend.Text;

  oParentWindow.pnlTop.Caption := lstParamsPrepare(ckbVisible.Checked,
    cmbMinScale.Text, cmbMaxScale.Text, cmbQuery.Text, '', '');
end;

procedure T_panelSectionVector.updateSectionNode;
var
  nd: TTreeNode;
  prnt: TTreeNode;
begin
  nd := oParentWindow.tvPages.Selected;
  if not assigned(nd) then
    exit;

  prnt := nd.Parent;
  if not assigned(prnt) then
    exit;

  if (prnt.Level = 0) and (nd.Level = 1) then
  begin
    if IsStringEmpty(edtLegend.Text) then
      nd.Text := Format('%s %d', [_rsrc(GIS_RS_LEGEND_PAG_SECTION),
        oParentWindow.MVC.SectionIndex + 1])
    else
      nd.Text := edtLegend.Text;
  end;
end;

procedure T_panelSectionVector.Write;
begin
  inherited;

  MVC.Query := cmbQuery.Text;
  MVC.Legend := edtLegend.Text;
end;

procedure T_panelSectionVector.doCallback(_sender: TObject; _code: Integer);

  procedure do_query_change;
  begin
    if not wasLegendEdited then
      edtLegend.Text := cmbQuery.Text;

    updateNode;
  end;

  procedure do_legend_change;
  begin
    updateSectionNode;

    if wasLegendEdited then
      exit;

    if edtLegend.Text <> cmbQuery.Text then
      wasLegendEdited := True;

    updateNode;
  end;

  procedure do_visible_click;
  begin
    updateNode;
  end;

begin
  inherited;

  case _code of
    0:
      do_visible_click;
    7:
      do_query_change;
    8:
      do_legend_change;
  end;
end;

procedure T_panelSectionVector.doQueryChange(_sender: TObject);
begin
  MVC.doQueryChange;
end;

procedure T_panelSectionVector.doLegendChange(_sender: TObject);
begin
  MVC.doLegendChange;
end;
{$ENDREGION}
{$REGION 'T_panelRenderer'}

function T_panelRenderer.fget_HasPreview: Boolean;
begin
  Result := MVC.HasPreview;
end;

procedure T_panelRenderer.init;
begin
  inherited;

  ItemText := _rsrc(GIS_RS_LEGEND_PAG_RENDERER);

  MVC := oParentWindow.MVC.Renderer;
  MVC.Callback := doCallback;

  initSelf;
  initFirst;
  initSecond;
end;

procedure T_panelRenderer.initSelf;
var
  t: Integer;
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  gpbRender := TGroupBox.Create(Self.Panel);
  gpbRender.Parent := Self.Panel;
  gpbRender.Top := ppiFix(8);
  gpbRender.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbRender, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbRender.Anchors := [akRight, akTop]
  else
    gpbRender.Anchors := [akLeft, akTop];
  gpbRender.Caption := _rsrc(GIS_RS_LEGEND_PAG_RENDERER);

  t := ppiFix(24);

  lblExpression := TLabel.Create(gpbRender);
  lblExpression.Parent := gpbRender;
  lblExpression.Top := t;
  PlaceControl(bd, nil, lblExpression, ppiFix(LEFT_3COL_1),
    lblExpression.Width);
  lblExpression.Caption := _rsrc(GIS_RS_LEGEND_PRM_EXPRESSION);

  lblRounding := TLabel.Create(gpbRender);
  lblRounding.Parent := gpbRender;
  lblRounding.Top := t;
  lblRounding.Caption := _rsrc(GIS_RS_LEGEND_PRM_ROUND);

  t := t + lblExpression.Height + ppiFix(4);

  cmbExpression := TComboBox.Create(gpbRender);
  cmbExpression.Parent := gpbRender;
  cmbExpression.Top := t;
  PlaceControl(bd, nil, cmbExpression, ppiFix(LEFT_3COL_1),
    ppiFix(LEFT_3COL_3));
  cmbExpression.ItemIndex := -1;
  cmbExpression.OnChange := doExpressionChange;
  oParentWindow.fillComboBoxWithFields(cmbExpression);

  PlaceControl(bd, cmbExpression, lblRounding, ppiFix(16), lblRounding.Width);

  speRounding := TEdit.Create(gpbRender);
  speRounding.Parent := gpbRender;
  speRounding.Top := t;
  PlaceControl(bd, cmbExpression, speRounding, ppiFix(16), ppiFix(75));

  vvspeRounding := TGIS_ValueValidatorEditHelper.Create(speRounding);
  vvspeRounding.MinVal := -10.0;
  vvspeRounding.MaxVal := 10.0;

  gpbRender.Height := t + speRounding.Height + ppiFix(16);
end;

procedure T_panelRenderer.initFirst;
var
  t: Integer;
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  gpbFirst := TGroupBox.Create(Self.Panel);
  gpbFirst.Parent := Self.Panel;
  gpbFirst.Top := gpbRender.Top + gpbRender.Height + ppiFix(8);
  gpbFirst.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbFirst, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbFirst.Anchors := [akRight, akTop]
  else
    gpbFirst.Anchors := [akLeft, akTop];
  gpbFirst.Caption := _rsrc(GIS_RS_LEGEND_TAB_FIRST);

  t := ppiFix(24);

  lblNumOfZones1 := TLabel.Create(gpbFirst);
  lblNumOfZones1.Parent := gpbFirst;
  lblNumOfZones1.Top := t;
  PlaceControl(bd, nil, lblNumOfZones1, ppiFix(LEFT_3COL_1),
    lblNumOfZones1.Width);
  lblNumOfZones1.Caption := _rsrc(GIS_RS_LEGEND_PRM_ZONES);

  lblMinVal1 := TLabel.Create(gpbFirst);
  lblMinVal1.Parent := gpbFirst;
  lblMinVal1.Top := t;
  PlaceControl(bd, nil, lblMinVal1, ppiFix(LEFT_3COL_2), lblMinVal1.Width);
  lblMinVal1.Caption := _rsrc(GIS_RS_LEGEND_PRM_MINVAL);

  lblMaxVal1 := TLabel.Create(gpbFirst);
  lblMaxVal1.Parent := gpbFirst;
  lblMaxVal1.Top := t;
  PlaceControl(bd, nil, lblMaxVal1, ppiFix(LEFT_3COL_3), lblMaxVal1.Width);
  lblMaxVal1.Caption := _rsrc(GIS_RS_LEGEND_PRM_MAXVAL);

  t := t + lblNumOfZones1.Height + ppiFix(4);

  speNumOfZones1 := TEdit.Create(gpbFirst);
  speNumOfZones1.Parent := gpbFirst;
  speNumOfZones1.Top := t;
  PlaceControl(bd, nil, speNumOfZones1, ppiFix(LEFT_3COL_1),
    ppiFix(WIDTH_3COL));
  speNumOfZones1.OnChange := doNumberOfZones1Change;

  vvspeNumOfZones1 := TGIS_ValueValidatorEditHelper.Create(speNumOfZones1);
  vvspeNumOfZones1.MinVal := -100.0;
  vvspeNumOfZones1.MaxVal := 100.0;

  edtMinVal1 := TEdit.Create(gpbFirst);
  edtMinVal1.Parent := gpbFirst;
  edtMinVal1.Top := t;
  PlaceControl(bd, nil, edtMinVal1, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  edtMinVal1.Text := '0';

  vvedtMinVal1 := TGIS_ValueValidatorEditHelper.Create(edtMinVal1);
  vvedtMinVal1.MinVal := -1E307;
  vvedtMinVal1.MaxVal := 1E307;
  vvedtMinVal1.Precision := 99;

  edtMaxVal1 := TEdit.Create(gpbFirst);
  edtMaxVal1.Parent := gpbFirst;
  edtMaxVal1.Top := t;
  PlaceControl(bd, nil, edtMaxVal1, ppiFix(LEFT_3COL_3), ppiFix(WIDTH_3COL));
  edtMaxVal1.Text := '0';

  vvedtMaxVal1 := TGIS_ValueValidatorEditHelper.Create(edtMaxVal1);
  vvedtMaxVal1.MinVal := -1E307;
  vvedtMaxVal1.MaxVal := 1E307;
  vvedtMaxVal1.Precision := 99;

  t := t + speNumOfZones1.Height + ppiFix(8);

  lblStartColor1 := TLabel.Create(gpbFirst);
  lblStartColor1.Parent := gpbFirst;
  lblStartColor1.Top := t;
  PlaceControl(bd, nil, lblStartColor1, ppiFix(LEFT_3COL_1),
    lblStartColor1.Width);
  lblStartColor1.Caption := _rsrc(GIS_RS_LEGEND_PRM_STARTCOLOR);

  lblEndColor1 := TLabel.Create(gpbFirst);
  lblEndColor1.Parent := gpbFirst;
  lblEndColor1.Top := t;
  PlaceControl(bd, nil, lblEndColor1, ppiFix(LEFT_3COL_2), lblEndColor1.Width);
  lblEndColor1.Caption := _rsrc(GIS_RS_LEGEND_PRM_ENDCOLOR);

  lblDefaultColor1 := TLabel.Create(gpbFirst);
  lblDefaultColor1.Parent := gpbFirst;
  lblDefaultColor1.Top := t;
  PlaceControl(bd, nil, lblDefaultColor1, ppiFix(LEFT_3COL_3),
    lblDefaultColor1.Width);
  lblDefaultColor1.Caption := _rsrc(GIS_RS_LEGEND_PRM_DEFAULTCOLOR);

  t := t + lblStartColor1.Height + ppiFix(4);

  cmbStartColor1 := TGIS_ColorComboBox.Create(gpbFirst);
  cmbStartColor1.Parent := gpbFirst;
  cmbStartColor1.Top := t;
  cmbStartColor1.Height := ppiFix(22);
  PlaceControl(bd, nil, cmbStartColor1, ppiFix(LEFT_3COL_1),
    ppiFix(WIDTH_3COL));
  cmbStartColor1.Fill(True, False);
  cmbStartColor1.OnChange := doControlChange;
  cmbStartColor1.CustomEvent := doCustomColor;

  cmbEndColor1 := TGIS_ColorComboBox.Create(gpbFirst);
  cmbEndColor1.Parent := gpbFirst;
  cmbEndColor1.Top := t;
  cmbEndColor1.Height := ppiFix(22);
  PlaceControl(bd, nil, cmbEndColor1, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbEndColor1.Fill(True, False);
  cmbEndColor1.OnChange := doControlChange;
  cmbEndColor1.CustomEvent := doCustomColor;

  cmbDefaultColor1 := TGIS_ColorComboBox.Create(gpbFirst);
  cmbDefaultColor1.Parent := gpbFirst;
  cmbDefaultColor1.Top := t;
  cmbDefaultColor1.Height := ppiFix(22);
  PlaceControl(bd, nil, cmbDefaultColor1, ppiFix(LEFT_3COL_3),
    ppiFix(WIDTH_3COL));
  cmbDefaultColor1.Fill(True, False);
  cmbDefaultColor1.OnChange := doControlChange;
  cmbDefaultColor1.CustomEvent := doCustomColor;

  t := t + cmbStartColor1.Height + ppiFix(8);

  lblStartSize1 := TLabel.Create(gpbFirst);
  lblStartSize1.Parent := gpbFirst;
  lblStartSize1.Top := t;
  PlaceControl(bd, nil, lblStartSize1, ppiFix(LEFT_3COL_1),
    lblStartSize1.Width);
  lblStartSize1.Caption := _rsrc(GIS_RS_LEGEND_PRM_STARTSIZE);

  lblEndSize1 := TLabel.Create(gpbFirst);
  lblEndSize1.Parent := gpbFirst;
  lblEndSize1.Top := t;
  PlaceControl(bd, nil, lblEndSize1, ppiFix(LEFT_3COL_2), lblEndSize1.Width);
  lblEndSize1.Caption := _rsrc(GIS_RS_LEGEND_PRM_ENDSIZE);

  lblDefaultSize1 := TLabel.Create(gpbFirst);
  lblDefaultSize1.Parent := gpbFirst;
  lblDefaultSize1.Top := t;
  PlaceControl(bd, nil, lblDefaultSize1, ppiFix(LEFT_3COL_3),
    lblDefaultSize1.Width);
  lblDefaultSize1.Caption := _rsrc(GIS_RS_LEGEND_PRM_DEFAULTSIZE);

  t := t + lblStartSize1.Height + ppiFix(4);

  cmbStartSize1 := TGIS_SizeComboBox.Create(gpbFirst);
  cmbStartSize1.Parent := gpbFirst;
  cmbStartSize1.Top := t;
  PlaceControl(bd, nil, cmbStartSize1, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbStartSize1.Fill(True, False, True, False);
  cmbStartSize1.OnChange := doControlChange;
  cmbStartSize1.CustomEvent := doCustomSize;

  cmbEndSize1 := TGIS_SizeComboBox.Create(gpbFirst);
  cmbEndSize1.Parent := gpbFirst;
  cmbEndSize1.Top := t;
  PlaceControl(bd, nil, cmbEndSize1, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbEndSize1.Fill(True, False, True, False);
  cmbEndSize1.OnChange := doControlChange;
  cmbEndSize1.CustomEvent := doCustomSize;

  cmbDefaultSize1 := TGIS_SizeComboBox.Create(gpbFirst);
  cmbDefaultSize1.Parent := gpbFirst;
  cmbDefaultSize1.Top := t;
  PlaceControl(bd, nil, cmbDefaultSize1, ppiFix(LEFT_3COL_3),
    ppiFix(WIDTH_3COL));
  cmbDefaultSize1.Fill(True, False, True, False);
  cmbDefaultSize1.OnChange := doControlChange;
  cmbDefaultSize1.CustomEvent := doCustomSize;

  t := t + cmbStartSize1.Height + ppiFix(16);

  gpbFirst.Height := t;
end;

procedure T_panelRenderer.initSecond;
var
  t: Integer;
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  gpbSecond := TGroupBox.Create(Self.Panel);
  gpbSecond.Parent := Self.Panel;
  gpbSecond.Top := gpbFirst.Top + gpbFirst.Height + ppiFix(8);
  gpbSecond.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbSecond, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbSecond.Anchors := [akRight, akTop]
  else
    gpbSecond.Anchors := [akLeft, akTop];
  gpbSecond.Caption := _rsrc(GIS_RS_LEGEND_TAB_SECOND);

  t := ppiFix(24);

  lblNumOfZones2 := TLabel.Create(gpbSecond);
  lblNumOfZones2.Parent := gpbSecond;
  lblNumOfZones2.Top := t;
  PlaceControl(bd, nil, lblNumOfZones2, ppiFix(LEFT_3COL_1),
    lblNumOfZones2.Width);
  lblNumOfZones2.Caption := _rsrc(GIS_RS_LEGEND_PRM_ZONES);

  lblMinVal2 := TLabel.Create(gpbSecond);
  lblMinVal2.Parent := gpbSecond;
  lblMinVal2.Top := t;
  PlaceControl(bd, nil, lblMinVal2, ppiFix(LEFT_3COL_2), lblMinVal2.Width);
  lblMinVal2.Caption := _rsrc(GIS_RS_LEGEND_PRM_MINVAL);

  lblMaxVal2 := TLabel.Create(gpbSecond);
  lblMaxVal2.Parent := gpbSecond;
  lblMaxVal2.Top := t;
  PlaceControl(bd, nil, lblMaxVal2, ppiFix(LEFT_3COL_3), lblMaxVal2.Width);
  lblMaxVal2.Caption := _rsrc(GIS_RS_LEGEND_PRM_MAXVAL);

  t := t + lblNumOfZones2.Height + ppiFix(4);

  speNumOfZones2 := TEdit.Create(gpbSecond);
  speNumOfZones2.Parent := gpbSecond;
  speNumOfZones2.Top := t;
  PlaceControl(bd, nil, speNumOfZones2, ppiFix(LEFT_3COL_1),
    ppiFix(WIDTH_3COL));
  speNumOfZones2.OnChange := doNumberOfZones2Change;

  vvspeNumOfZones2 := TGIS_ValueValidatorEditHelper.Create(speNumOfZones2);
  vvspeNumOfZones2.MinVal := -100.0;
  vvspeNumOfZones2.MaxVal := 100.0;

  edtMinVal2 := TEdit.Create(gpbSecond);
  edtMinVal2.Parent := gpbSecond;
  edtMinVal2.Top := t;
  PlaceControl(bd, nil, edtMinVal2, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  edtMinVal2.Text := '0';

  vvedtMinVal2 := TGIS_ValueValidatorEditHelper.Create(edtMinVal2);
  vvedtMinVal2.MinVal := -1E307;
  vvedtMinVal2.MaxVal := 1E307;
  vvedtMinVal2.Precision := 99;

  edtMaxVal2 := TEdit.Create(gpbSecond);
  edtMaxVal2.Parent := gpbSecond;
  edtMaxVal2.Top := t;
  PlaceControl(bd, nil, edtMaxVal2, ppiFix(LEFT_3COL_3), ppiFix(WIDTH_3COL));
  edtMaxVal2.Text := '0';

  vvedtMaxVal2 := TGIS_ValueValidatorEditHelper.Create(edtMaxVal2);
  vvedtMaxVal2.MinVal := -1E307;
  vvedtMaxVal2.MaxVal := 1E307;
  vvedtMaxVal2.Precision := 99;

  t := t + speNumOfZones2.Height + ppiFix(8);

  lblStartColor2 := TLabel.Create(gpbSecond);
  lblStartColor2.Parent := gpbSecond;
  lblStartColor2.Top := t;
  PlaceControl(bd, nil, lblStartColor2, ppiFix(LEFT_3COL_1),
    lblStartColor2.Width);
  lblStartColor2.Caption := _rsrc(GIS_RS_LEGEND_PRM_STARTCOLOR);

  lblEndColor2 := TLabel.Create(gpbSecond);
  lblEndColor2.Parent := gpbSecond;
  lblEndColor2.Top := t;
  PlaceControl(bd, nil, lblEndColor2, ppiFix(LEFT_3COL_2), lblEndColor2.Width);
  lblEndColor2.Caption := _rsrc(GIS_RS_LEGEND_PRM_ENDCOLOR);

  lblDefaultColor2 := TLabel.Create(gpbSecond);
  lblDefaultColor2.Parent := gpbSecond;
  lblDefaultColor2.Top := t;
  PlaceControl(bd, nil, lblDefaultColor2, ppiFix(LEFT_3COL_3),
    lblDefaultColor2.Width);
  lblDefaultColor2.Caption := _rsrc(GIS_RS_LEGEND_PRM_DEFAULTCOLOR);

  t := t + lblStartColor2.Height + ppiFix(4);

  cmbStartColor2 := TGIS_ColorComboBox.Create(gpbSecond);
  cmbStartColor2.Parent := gpbSecond;
  cmbStartColor2.Top := t;
  cmbStartColor2.Height := ppiFix(22);
  PlaceControl(bd, nil, cmbStartColor2, ppiFix(LEFT_3COL_1),
    ppiFix(WIDTH_3COL));
  cmbStartColor2.Fill(True, False);
  cmbStartColor2.OnChange := doControlChange;
  cmbStartColor2.CustomEvent := doCustomColor;

  cmbEndColor2 := TGIS_ColorComboBox.Create(gpbSecond);
  cmbEndColor2.Parent := gpbSecond;
  cmbEndColor2.Top := t;
  cmbEndColor2.Height := ppiFix(22);
  PlaceControl(bd, nil, cmbEndColor2, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbEndColor2.Fill(True, False);
  cmbEndColor2.OnChange := doControlChange;
  cmbEndColor2.CustomEvent := doCustomColor;

  cmbDefaultColor2 := TGIS_ColorComboBox.Create(gpbSecond);
  cmbDefaultColor2.Parent := gpbSecond;
  cmbDefaultColor2.Top := t;
  cmbDefaultColor2.Height := ppiFix(22);
  PlaceControl(bd, nil, cmbDefaultColor2, ppiFix(LEFT_3COL_3),
    ppiFix(WIDTH_3COL));
  cmbDefaultColor2.Fill(True, False);
  cmbDefaultColor2.OnChange := doControlChange;
  cmbDefaultColor2.CustomEvent := doCustomColor;

  t := t + cmbStartColor2.Height + ppiFix(8);

  lblStartSize2 := TLabel.Create(gpbSecond);
  lblStartSize2.Parent := gpbSecond;
  lblStartSize2.Top := t;
  PlaceControl(bd, nil, lblStartSize2, ppiFix(LEFT_3COL_1),
    lblStartSize2.Width);
  lblStartSize2.Caption := _rsrc(GIS_RS_LEGEND_PRM_STARTSIZE);

  lblEndSize2 := TLabel.Create(gpbSecond);
  lblEndSize2.Parent := gpbSecond;
  lblEndSize2.Top := t;
  PlaceControl(bd, nil, lblEndSize2, ppiFix(LEFT_3COL_2), lblEndSize2.Width);
  lblEndSize2.Caption := _rsrc(GIS_RS_LEGEND_PRM_ENDSIZE);

  lblDefaultSize2 := TLabel.Create(gpbSecond);
  lblDefaultSize2.Parent := gpbSecond;
  lblDefaultSize2.Top := t;
  PlaceControl(bd, nil, lblDefaultSize2, ppiFix(LEFT_3COL_3),
    lblDefaultSize2.Width);
  lblDefaultSize2.Caption := _rsrc(GIS_RS_LEGEND_PRM_DEFAULTSIZE);

  t := t + lblStartSize2.Height + ppiFix(4);

  cmbStartSize2 := TGIS_SizeComboBox.Create(gpbSecond);
  cmbStartSize2.Parent := gpbSecond;
  cmbStartSize2.Top := t;
  PlaceControl(bd, nil, cmbStartSize2, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbStartSize2.Fill(True, False, True, False);
  cmbStartSize2.OnChange := doControlChange;
  cmbStartSize2.CustomEvent := doCustomSize;

  cmbEndSize2 := TGIS_SizeComboBox.Create(gpbSecond);
  cmbEndSize2.Parent := gpbSecond;
  cmbEndSize2.Top := t;
  PlaceControl(bd, nil, cmbEndSize2, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbEndSize2.Fill(True, False, True, False);
  cmbEndSize2.OnChange := doControlChange;
  cmbEndSize2.CustomEvent := doCustomSize;

  cmbDefaultSize2 := TGIS_SizeComboBox.Create(gpbSecond);
  cmbDefaultSize2.Parent := gpbSecond;
  cmbDefaultSize2.Top := t;
  PlaceControl(bd, nil, cmbDefaultSize2, ppiFix(LEFT_3COL_3),
    ppiFix(WIDTH_3COL));
  cmbDefaultSize2.Fill(True, False, True, False);
  cmbDefaultSize2.OnChange := doControlChange;
  cmbDefaultSize2.CustomEvent := doCustomSize;

  t := t + cmbStartSize2.Height + ppiFix(16);

  gpbSecond.Height := t;
end;

procedure T_panelRenderer.enableFirst(_enable: Boolean);
begin
  lblMinVal1.Enabled := _enable;
  lblMaxVal1.Enabled := _enable;
  edtMinVal1.Enabled := _enable;
  edtMaxVal1.Enabled := _enable;
  lblStartColor1.Enabled := _enable;
  lblEndColor1.Enabled := _enable;
  lblDefaultColor1.Enabled := _enable;
  cmbStartColor1.Enabled := _enable;
  cmbEndColor1.Enabled := _enable;
  cmbDefaultColor1.Enabled := _enable;
  lblStartSize1.Enabled := _enable;
  lblEndSize1.Enabled := _enable;
  lblDefaultSize1.Enabled := _enable;
  cmbStartSize1.Enabled := _enable;
  cmbEndSize1.Enabled := _enable;
  cmbDefaultSize1.Enabled := _enable;
end;

procedure T_panelRenderer.enableSecond(_enable: Boolean);
begin
  lblMinVal2.Enabled := _enable;
  lblMaxVal2.Enabled := _enable;
  edtMinVal2.Enabled := _enable;
  edtMaxVal2.Enabled := _enable;
  lblStartColor2.Enabled := _enable;
  lblEndColor2.Enabled := _enable;
  lblDefaultColor2.Enabled := _enable;
  cmbStartColor2.Enabled := _enable;
  cmbEndColor2.Enabled := _enable;
  cmbDefaultColor2.Enabled := _enable;
  lblStartSize2.Enabled := _enable;
  lblEndSize2.Enabled := _enable;
  lblDefaultSize2.Enabled := _enable;
  cmbStartSize2.Enabled := _enable;
  cmbEndSize2.Enabled := _enable;
  cmbDefaultSize2.Enabled := _enable;
end;

procedure T_panelRenderer.Read;
begin
  lockUpdates;
  try
    cmbExpression.Text := MVC.ParamsRender.Expression;
    vvspeRounding.Value := MVC.ParamsRender.Round;

    // First
    vvspeNumOfZones1.Value := MVC.ParamsRender.Zones;
    vvedtMinVal1.Value := MVC.ParamsRender.MinVal;
    vvedtMaxVal1.Value := MVC.ParamsRender.MaxVal;

    cmbStartColor1.Value := MVC.ParamsRender.StartColorAsText;
    cmbEndColor1.Value := MVC.ParamsRender.EndColorAsText;
    cmbDefaultColor1.Value := MVC.ParamsRender.ColorDefaultAsText;

    cmbStartSize1.Value := MVC.ParamsRender.StartSizeAsText;
    cmbEndSize1.Value := MVC.ParamsRender.EndSizeAsText;
    cmbDefaultSize1.Value := MVC.ParamsRender.SizeDefaultAsText;

    // Second
    vvspeNumOfZones2.Value := MVC.ParamsRender.ZonesEx;
    vvedtMinVal2.Value := MVC.ParamsRender.MinValEx;
    vvedtMaxVal2.Value := MVC.ParamsRender.MaxValEx;

    cmbStartColor2.Value := MVC.ParamsRender.StartColorExAsText;
    cmbEndColor2.Value := MVC.ParamsRender.EndColorExAsText;
    cmbDefaultColor2.Value := MVC.ParamsRender.ColorDefaultAsText;

    cmbStartSize2.Value := MVC.ParamsRender.StartSizeExAsText;
    cmbEndSize2.Value := MVC.ParamsRender.EndSizeExAsText;
    cmbDefaultSize2.Value := MVC.ParamsRender.SizeDefaultAsText;

    MVC.doExpressionChange;
  finally
    unlockUpdates;
  end;
end;

procedure T_panelRenderer.Write;
begin
  MVC.ParamsRender.Expression := cmbExpression.Text;
  MVC.ParamsRender.Round := FloorS(vvspeRounding.Value);

  // First
  MVC.ParamsRender.Zones := FloorS(vvspeNumOfZones1.Value);
  MVC.ParamsRender.MinVal := vvedtMinVal1.Value;
  MVC.ParamsRender.MaxVal := vvedtMaxVal1.Value;
  MVC.ParamsRender.StartColorAsText := cmbStartColor1.Value;
  MVC.ParamsRender.EndColorAsText := cmbEndColor1.Value;
  MVC.ParamsRender.ColorDefaultAsText := cmbDefaultColor1.Value;
  MVC.ParamsRender.StartSizeAsText := cmbStartSize1.Value;
  MVC.ParamsRender.EndSizeAsText := cmbEndSize1.Value;
  MVC.ParamsRender.SizeDefaultAsText := cmbDefaultSize1.Value;

  // Second
  MVC.ParamsRender.ZonesEx := FloorS(vvspeNumOfZones2.Value);
  MVC.ParamsRender.MinValEx := vvedtMinVal2.Value;
  MVC.ParamsRender.MaxValEx := vvedtMaxVal2.Value;
  MVC.ParamsRender.StartColorExAsText := cmbStartColor2.Value;
  MVC.ParamsRender.EndColorExAsText := cmbEndColor2.Value;
  MVC.ParamsRender.StartSizeExAsText := cmbStartSize2.Value;
  MVC.ParamsRender.EndSizeExAsText := cmbEndSize2.Value;
end;

procedure T_panelRenderer.doCallback(_sender: TObject; _code: Integer);

  procedure do_expression_change;
  begin
    if cmbExpression.Text <> '' then
    begin
      lblNumOfZones1.Enabled := True;
      speNumOfZones1.Enabled := True;
      MVC.doZonesChange;
      lblNumOfZones2.Enabled := True;
      speNumOfZones2.Enabled := True;
      MVC.DoZonesExChange;
    end
    else
    begin
      lblNumOfZones1.Enabled := False;
      speNumOfZones1.Enabled := False;
      enableFirst(False);
      lblNumOfZones2.Enabled := False;
      speNumOfZones2.Enabled := False;
      enableSecond(False);
    end;
  end;

  procedure do_noz1_change;
  begin
    enableFirst(vvspeNumOfZones1.Value <> 0);
  end;

  procedure do_noz2_change;
  begin
    enableSecond(vvspeNumOfZones2.Value <> 0);
  end;

begin
  case _code of
    1:
      do_expression_change;
    2:
      do_noz1_change;
    3:
      do_noz2_change;
  end;
end;

procedure T_panelRenderer.doExpressionChange(_sender: TObject);
begin
  MVC.doExpressionChange;
end;

procedure T_panelRenderer.doNumberOfZones1Change(_sender: TObject);
begin
  MVC.doZonesChange;
end;

procedure T_panelRenderer.doNumberOfZones2Change(_sender: TObject);
begin
  MVC.DoZonesExChange;
end;

procedure T_panelRenderer.doControlChange(_sender: TObject);
begin
  MVC.doControlChange;
end;

{$ENDREGION}
{$REGION 'T_panelMarker'}

function T_panelMarker.fget_HasPreview: Boolean;
begin
  Result := MVC.HasPreview;
end;

procedure T_panelMarker.init;
begin
  inherited;

  ItemText := _rsrc(GIS_RS_LEGEND_PAG_MARKER);

  MVC := oParentWindow.MVC.Marker;
  MVC.Callback := doCallback;

  initMarker;
  initOutline;
  initSelf;
end;

procedure T_panelMarker.initSelf;
var
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  ckbLegend := TCheckBox.Create(Self.Panel);
  ckbLegend.Parent := Self.Panel;
  ckbLegend.Top := gpbOutline.Top + gpbOutline.Height + ppiFix(8);
  PlaceControl(bd, nil, ckbLegend, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_2COL));
  if bd = bdRightToLeft then
    ckbLegend.Anchors := [akRight, akTop]
  else
    ckbLegend.Anchors := [akLeft, akTop];
  ckbLegend.Caption := _rsrc(GIS_RS_LEGEND_PRM_INCLUDEINLEGEND);
end;

procedure T_panelMarker.initMarker;
var
  t: Integer;
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  gpbMarker := TGroupBox.Create(Self.Panel);
  gpbMarker.Parent := Self.Panel;
  gpbMarker.Top := ppiFix(8);
  gpbMarker.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbMarker, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbMarker.Anchors := [akRight, akTop]
  else
    gpbMarker.Anchors := [akLeft, akTop];
  gpbMarker.Caption := _rsrc(GIS_RS_LEGEND_TAB_MARKER);

  t := ppiFix(24);

  lblStyle := TLabel.Create(gpbMarker);
  lblStyle.Parent := gpbMarker;
  lblStyle.Top := t;
  PlaceControl(bd, nil, lblStyle, ppiFix(LEFT_3COL_1), lblStyle.Width);
  lblStyle.Caption := _rsrc(GIS_RS_LEGEND_PRM_STYLE);

  lblColor := TLabel.Create(gpbMarker);
  lblColor.Parent := gpbMarker;
  lblColor.Top := t;
  PlaceControl(bd, nil, lblColor, ppiFix(LEFT_3COL_2), lblColor.Width);
  lblColor.Caption := _rsrc(GIS_RS_LEGEND_PRM_COLOR);

  lblPattern := TLabel.Create(gpbMarker);
  lblPattern.Parent := gpbMarker;
  lblPattern.Top := t;
  PlaceControl(bd, nil, lblPattern, ppiFix(LEFT_3COL_3), lblPattern.Width);
  lblPattern.Caption := _rsrc(GIS_RS_LEGEND_PRM_PATTERN);

  t := t + lblStyle.Height + ppiFix(4);

  cmbStyle := TGIS_SymbolComboBox.Create(gpbMarker);
  cmbStyle.Parent := gpbMarker;
  cmbStyle.Top := t;
  PlaceControl(bd, nil, cmbStyle, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbStyle.Fill;
  cmbStyle.OnChange := doPatternChange;
  cmbStyle.CustomEvent := doCustomPattern;
  cmbStyle.GetBitmapEvent := doGetBitmapMarkerStyle;

  cmbColor := TGIS_ColorComboBox.Create(gpbMarker);
  cmbColor.Parent := gpbMarker;
  cmbColor.Top := t;
  cmbColor.Height := ppiFix(22);
  PlaceControl(bd, nil, cmbColor, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbColor.Fill(True, True);
  cmbColor.OnChange := doControlChange;
  cmbColor.CustomEvent := doCustomColor;

  cmbPattern := TGIS_PatternComboBox.Create(gpbMarker);
  cmbPattern.Parent := gpbMarker;
  cmbPattern.Top := t;
  PlaceControl(bd, nil, cmbPattern, ppiFix(LEFT_3COL_3), ppiFix(WIDTH_3COL));
  cmbPattern.Fill(False);
  cmbPattern.OnChange := doPatternChange;
  cmbPattern.CustomEvent := doCustomPattern;
  cmbPattern.GetBitmapEvent := doGetBitmapAreaFill;

  t := t + cmbPattern.Height + ppiFix(16);

  lblSize := TLabel.Create(gpbMarker);
  lblSize.Parent := gpbMarker;
  lblSize.Top := t;
  PlaceControl(bd, nil, lblSize, ppiFix(LEFT_3COL_1), lblSize.Width);
  lblSize.Caption := _rsrc(GIS_RS_LEGEND_PRM_SIZE);

  lblSymbolRotate := TLabel.Create(gpbMarker);
  lblSymbolRotate.Parent := gpbMarker;
  lblSymbolRotate.Top := t;
  PlaceControl(bd, nil, lblSymbolRotate, ppiFix(LEFT_3COL_2),
    lblSymbolRotate.Width);
  lblSymbolRotate.Caption := _rsrc(GIS_RS_LEGEND_PRM_SYMBOLROTATE);

  lblSSSize := TLabel.Create(gpbMarker);
  lblSSSize.Parent := gpbMarker;
  lblSSSize.Top := t;
  PlaceControl(bd, nil, lblSSSize, ppiFix(LEFT_3COL_3), lblSSSize.Width);
  lblSSSize.Caption := _rsrc(GIS_RS_LEGEND_PRM_SMART_SIZE);

  t := t + lblSymbolRotate.Height + ppiFix(4);

  cmbSize := TGIS_SizeComboBox.Create(gpbMarker);
  cmbSize.Parent := gpbMarker;
  cmbSize.Top := t;
  PlaceControl(bd, nil, cmbSize, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbSize.Fill(True, False, True, True);
  cmbSize.OnChange := doControlChange;
  cmbSize.CustomEvent := doCustomSize;

  cmbSymbolRotate := TGIS_RotationComboBox.Create(gpbMarker);
  cmbSymbolRotate.Parent := gpbMarker;
  cmbSymbolRotate.Top := t;
  PlaceControl(bd, nil, cmbSymbolRotate, ppiFix(LEFT_3COL_2),
    ppiFix(WIDTH_3COL));
  cmbSymbolRotate.Fill(True);
  cmbSymbolRotate.OnChange := doControlChange;
  cmbSymbolRotate.CustomEvent := doCustomRotation;

  cmbSSSize := TGIS_SizeComboBox.Create(gpbMarker);
  cmbSSSize.Parent := gpbMarker;
  cmbSSSize.Top := t;
  PlaceControl(bd, nil, cmbSSSize, ppiFix(LEFT_3COL_3), ppiFix(WIDTH_3COL));
  cmbSSSize.Fill(False, False, True, False);
  cmbSSSize.OnChange := doControlChange;
  cmbSSSize.CustomEvent := doCustomSize;

  t := t + cmbSymbolRotate.Height + ppiFix(16);

  gpbMarker.Height := t;
end;

procedure T_panelMarker.initOutline;
var
  t: Integer;
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  gpbOutline := TGroupBox.Create(Self.Panel);
  gpbOutline.Parent := Self.Panel;
  gpbOutline.Top := gpbMarker.Top + gpbMarker.Height + ppiFix(4);
  gpbOutline.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbOutline, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbOutline.Anchors := [akRight, akTop]
  else
    gpbOutline.Anchors := [akLeft, akTop];
  gpbOutline.Caption := _rsrc(GIS_RS_LEGEND_TAB_OUTLINE);

  t := ppiFix(24);

  lblOStyle := TLabel.Create(gpbOutline);
  lblOStyle.Parent := gpbOutline;
  lblOStyle.Top := t;
  PlaceControl(bd, nil, lblOStyle, ppiFix(LEFT_3COL_1), lblOStyle.Width);
  lblOStyle.Caption := _rsrc(GIS_RS_LEGEND_PRM_STYLE);

  lblOColor := TLabel.Create(gpbOutline);
  lblOColor.Parent := gpbOutline;
  lblOColor.Top := t;
  PlaceControl(bd, nil, lblOColor, ppiFix(LEFT_3COL_2), lblOColor.Width);
  lblOColor.Caption := _rsrc(GIS_RS_LEGEND_PRM_COLOR);

  lblOPattern := TLabel.Create(gpbOutline);
  lblOPattern.Parent := gpbOutline;
  lblOPattern.Top := t;
  PlaceControl(bd, nil, lblOPattern, ppiFix(LEFT_3COL_3), lblOPattern.Width);
  lblOPattern.Caption := _rsrc(GIS_RS_LEGEND_PRM_PATTERN);

  t := t + lblOStyle.Height + ppiFix(4);

  cmbOStyle := TGIS_PenStyleComboBox.Create(gpbOutline);
  cmbOStyle.Parent := gpbOutline;
  cmbOStyle.Top := t;
  PlaceControl(bd, nil, cmbOStyle, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbOStyle.Fill(False);
  cmbOStyle.OnChange := doOPatternChange;
  cmbOStyle.CustomEvent := doCustomPattern;
  cmbOStyle.GetBitmapEvent := doGetBitmapAreaOutline;

  cmbOColor := TGIS_ColorComboBox.Create(gpbOutline);
  cmbOColor.Parent := gpbOutline;
  cmbOColor.Top := t;
  cmbOColor.Height := ppiFix(22);
  PlaceControl(bd, nil, cmbOColor, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbOColor.Fill(True, True);
  cmbOColor.OnChange := doControlChange;
  cmbOColor.CustomEvent := doCustomColor;

  cmbOPattern := TGIS_PatternComboBox.Create(gpbOutline);
  cmbOPattern.Parent := gpbOutline;
  cmbOPattern.Top := t;
  PlaceControl(bd, nil, cmbOPattern, ppiFix(LEFT_3COL_3), ppiFix(WIDTH_3COL));
  cmbOPattern.Fill(False);
  cmbOPattern.OnChange := doOPatternChange;
  cmbOPattern.CustomEvent := doCustomPattern;
  cmbOPattern.GetBitmapEvent := doGetBitmapAreaFill;

  t := t + cmbOPattern.Height + ppiFix(16);

  lblOWidth := TLabel.Create(gpbOutline);
  lblOWidth.Parent := gpbOutline;
  lblOWidth.Top := t;
  PlaceControl(bd, nil, lblOWidth, ppiFix(LEFT_3COL_1), lblOWidth.Width);
  lblOWidth.Caption := _rsrc(GIS_RS_LEGEND_PRM_WIDTH);

  t := t + lblOWidth.Height + ppiFix(4);

  cmbOWidth := TGIS_SizeComboBox.Create(gpbOutline);
  cmbOWidth.Parent := gpbOutline;
  cmbOWidth.Top := t;
  PlaceControl(bd, nil, cmbOWidth, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbOWidth.Fill(False, True, True, True);
  cmbOWidth.OnChange := doControlChange;
  cmbOWidth.CustomEvent := doCustomSize;

  t := t + cmbOWidth.Height + ppiFix(16);

  gpbOutline.Height := t;
end;

procedure T_panelMarker.Read;
begin
  lockUpdates;
  try
    ckbLegend.Checked := MVC.ParamsMarker.ShowLegend;

    cmbStyle.Value := MVC.ParamsMarker.StyleAsText;
    cmbColor.Value := MVC.ParamsMarker.ColorAsText;
    cmbPattern.Value := MVC.ParamsMarker.PatternAsText;
    cmbSymbolRotate.Value := MVC.ParamsMarker.SymbolRotateAsText;
    cmbSize.Value := MVC.ParamsMarker.SizeAsText;

    // Outline
    cmbOStyle.Value := MVC.ParamsMarker.OutlineStyleAsText;
    cmbOWidth.Value := MVC.ParamsMarker.OutlineWidthAsText;
    cmbOColor.Value := MVC.ParamsMarker.OutlineColorAsText;
    cmbOPattern.Value := MVC.ParamsMarker.OutlinePatternAsText;

    // Smart size
    cmbSSSize.Value := MVC.ParamsMarker.SmartSizeAsText;

    MVC.doPatternChange;
    MVC.doOPatternChange;
    MVC.doSmartSizeFieldChange;
  finally
    unlockUpdates;
  end;
end;

procedure T_panelMarker.Write;
begin
  MVC.ParamsMarker.ShowLegend := ckbLegend.Checked;

  MVC.ParamsMarker.StyleAsText := cmbStyle.Value;
  MVC.ParamsMarker.ColorAsText := cmbColor.Value;
  MVC.ParamsMarker.PatternAsText := cmbPattern.Value;
  MVC.ParamsMarker.SymbolRotateAsText := cmbSymbolRotate.Value;
  MVC.ParamsMarker.SizeAsText := cmbSize.Value;

  // Outline
  MVC.ParamsMarker.OutlineStyleAsText := cmbOStyle.Value;
  MVC.ParamsMarker.OutlineWidthAsText := cmbOWidth.Value;
  MVC.ParamsMarker.OutlineColorAsText := cmbOColor.Value;
  MVC.ParamsMarker.OutlinePatternAsText := cmbOPattern.Value;

  // Smart size
  MVC.ParamsMarker.SmartSizeAsText := cmbSSSize.Value;
end;

procedure T_panelMarker.PreparePreview(const _viewer: IGIS_Viewer);
begin
  MVC.PreparePreview(_viewer);
end;

procedure T_panelMarker.UpdatePreview;
var
  ll: TGIS_Layer;
  paramsvec: TGIS_ParamsSectionVector;
begin
  oParentWindow.tmrUpdate.Enabled := False;
  if oParentWindow.gisPreview.IsEmpty then
    exit;
  oParentWindow.tmrUpdate.Enabled := True;

  ll := TGIS_Layer(oParentWindow.gisPreview.Items[0]);

  paramsvec := TGIS_ParamsSectionVector(ll.Params);

  MVC.SectionWrite(paramsvec);

  paramsvec.Labels.Value := '';
  paramsvec.Labels.Field := '';

  paramsvec.Render.Chart := '';
  paramsvec.Chart.Values := '';

  paramsvec.Query := '';

  with TGIS_ParamsSection(ll.Params) do
  begin
    MinZoom := 0;
    MaxZoom := GIS_MAX_DOUBLE;
    MinScale := 0;
    MaxScale := GIS_MAX_DOUBLE;
  end;

  // avoid unreasonable preview
  paramsvec.Marker.Color := normalize_color(paramsvec.Marker.Color,
    paramsvec.Marker.ColorAsText);
  paramsvec.Marker.OutlineColor :=
    normalize_color(paramsvec.Marker.OutlineColor,
    paramsvec.Marker.OutlineColorAsText);
  paramsvec.Marker.Size := normalize_size(oParentWindow.gisPreview,
    paramsvec.Marker.Size, paramsvec.Marker.SizeAsText, MAX_PREVIEW_SIZE_MAKER);
  paramsvec.Marker.OutlineWidth := normalize_size(oParentWindow.gisPreview,
    paramsvec.Marker.OutlineWidth, paramsvec.Marker.OutlineWidthAsText,
    MAX_PREVIEW_SIZE_OUTLINE);
  paramsvec.Marker.SymbolRotate :=
    normalize_angle(paramsvec.Marker.SymbolRotate,
    paramsvec.Marker.SymbolRotateAsText);
end;

procedure T_panelMarker.doCallback(_sender: TObject; _code: Integer);

  procedure do_ssfield_change;
  begin

  end;

  procedure do_update_symbol_bitmap;
  var
    bsymfnt: Boolean;
    bsymcgm: Boolean;
    bsymsvg: Boolean;
    bsympic: Boolean;
    bsymlin: Boolean;
    sym: TGIS_SymbolAbstract;
    bbmp: Boolean;
  begin
    sym := MVC.ParamsMarker.Symbol;

    bsymfnt := assigned(sym) and (sym is TGIS_SymbolFont);
    bsymcgm := assigned(sym) and (sym is TGIS_SymbolCGM);
    bsymsvg := assigned(sym) and (sym is TGIS_SymbolSVG);
    bsympic := assigned(sym) and (sym is TGIS_SymbolPicture);
    bsymlin := assigned(sym) and
      ((sym is TGIS_SymbolLine) or (sym is TGIS_SymbolLineEx));
    bbmp := not TGIS_Bitmap.IsNilOrEmpty(MVC.ParamsMarker.Bitmap);

    if bsymfnt or bsymcgm or bsymsvg or bsymlin then
    begin
      lblColor.Enabled := True;
      cmbColor.Enabled := True;
      lblPattern.Enabled := False;
      cmbPattern.Enabled := False;
      lblSymbolRotate.Enabled := True;
      cmbSymbolRotate.Enabled := True;
    end
    else if bsympic then
    begin
      lblColor.Enabled := False;
      cmbColor.Enabled := False;
      lblPattern.Enabled := False;
      cmbPattern.Enabled := False;
      lblSymbolRotate.Enabled := True;
      cmbSymbolRotate.Enabled := True;
    end
    else if bbmp then
    begin
      lblColor.Enabled := False;
      cmbColor.Enabled := False;
      lblPattern.Enabled := True;
      cmbPattern.Enabled := True;
      lblSymbolRotate.Enabled := False;
      cmbSymbolRotate.Enabled := False;
    end
    else
    begin
      lblColor.Enabled := True;
      cmbColor.Enabled := True;
      lblPattern.Enabled := True;
      cmbPattern.Enabled := True;
      lblSymbolRotate.Enabled := False;
      cmbSymbolRotate.Enabled := False;
    end;
  end;

  procedure do_update_osymbol_obitmap;
  var
    bsymfnt: Boolean;
    bsymcgm: Boolean;
    bsymsvg: Boolean;
    bsympic: Boolean;
    bsymlin: Boolean;
    sym: TGIS_SymbolAbstract;
    bbmp: Boolean;
  begin
    sym := MVC.ParamsMarker.Symbol;

    bsymfnt := assigned(sym) and (sym is TGIS_SymbolFont);
    bsymcgm := assigned(sym) and (sym is TGIS_SymbolCGM);
    bsymsvg := assigned(sym) and (sym is TGIS_SymbolSVG);
    bsympic := assigned(sym) and (sym is TGIS_SymbolPicture);
    bsymlin := assigned(sym) and
      ((sym is TGIS_SymbolLine) or (sym is TGIS_SymbolLineEx));
    bbmp := not TGIS_Bitmap.IsNilOrEmpty(MVC.ParamsMarker.OutlineBitmap);

    if bsymfnt or bsymcgm or bsymsvg or bsymlin then
    begin
      lblOStyle.Enabled := False;
      cmbOStyle.Enabled := False;
      lblOColor.Enabled := True;
      cmbOColor.Enabled := True;
      lblOPattern.Enabled := False;
      cmbOPattern.Enabled := False;
      lblOWidth.Enabled := False;
      cmbOWidth.Enabled := False;
    end
    else if bsympic then
    begin
      lblOStyle.Enabled := False;
      cmbOStyle.Enabled := False;
      lblOColor.Enabled := False;
      cmbOColor.Enabled := False;
      lblOPattern.Enabled := False;
      cmbOPattern.Enabled := False;
      lblOWidth.Enabled := False;
      cmbOWidth.Enabled := False;
    end
    else if bbmp then
    begin
      lblOStyle.Enabled := True;
      cmbOStyle.Enabled := True;
      lblOColor.Enabled := False;
      cmbOColor.Enabled := False;
      lblOPattern.Enabled := True;
      cmbOPattern.Enabled := True;
      lblOWidth.Enabled := True;
      cmbOWidth.Enabled := True;
    end
    else
    begin
      lblOStyle.Enabled := True;
      cmbOStyle.Enabled := True;
      lblOColor.Enabled := True;
      cmbOColor.Enabled := True;
      lblOPattern.Enabled := True;
      cmbOPattern.Enabled := True;
      lblOWidth.Enabled := True;
      cmbOWidth.Enabled := True;
    end;
  end;

begin
  if not blockedUpdates then
  begin
    Write;
    UpdatePreview;
  end;

  case _code of
    60:
      do_ssfield_change;
    100:
      begin
        do_update_symbol_bitmap;
        do_update_osymbol_obitmap;
      end;
    130:
      do_update_osymbol_obitmap
  end;

end;

procedure T_panelMarker.doOPatternChange(_sender: TObject);
begin
  MVC.doOPatternChange;
end;

procedure T_panelMarker.doPatternChange(_sender: TObject);
begin
  MVC.doPatternChange;
end;

procedure T_panelMarker.doSmartSizeFieldChange(_sender: TObject);
begin
  MVC.doSmartSizeFieldChange;
end;

procedure T_panelMarker.doControlChange(_sender: TObject);
begin
  MVC.doControlChange;
end;
{$ENDREGION}
{$REGION 'T_panelLine'}

function T_panelLine.fget_HasPreview: Boolean;
begin
  Result := MVC.HasPreview;
end;

procedure T_panelLine.init;
begin
  inherited;

  ItemText := _rsrc(GIS_RS_LEGEND_PAG_LINE);

  MVC := oParentWindow.MVC.Line;
  MVC.Callback := doCallback;

  initLine;
  initOutline;
  initSelf;
end;

procedure T_panelLine.initSelf;
var
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  ckbLegend := TCheckBox.Create(Self.Panel);
  ckbLegend.Parent := Self.Panel;
  ckbLegend.Top := gpbOutline.Top + gpbOutline.Height + ppiFix(8);
  PlaceControl(bd, nil, ckbLegend, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_2COL));
  if bd = bdRightToLeft then
    ckbLegend.Anchors := [akRight, akTop]
  else
    ckbLegend.Anchors := [akLeft, akTop];
  ckbLegend.Caption := _rsrc(GIS_RS_LEGEND_PRM_INCLUDEINLEGEND);
end;

procedure T_panelLine.initLine;
var
  t: Integer;
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  gpbLine := TGroupBox.Create(Self.Panel);
  gpbLine.Parent := Self.Panel;
  gpbLine.Top := ppiFix(8);
  gpbLine.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbLine, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbLine.Anchors := [akRight, akTop]
  else
    gpbLine.Anchors := [akLeft, akTop];
  gpbLine.Caption := _rsrc(GIS_RS_LEGEND_TAB_LINE);

  t := ppiFix(24);

  lblStyle := TLabel.Create(gpbLine);
  lblStyle.Parent := gpbLine;
  lblStyle.Top := t;
  PlaceControl(bd, nil, lblStyle, ppiFix(LEFT_3COL_1), lblStyle.Width);
  lblStyle.Caption := _rsrc(GIS_RS_LEGEND_PRM_STYLE);

  lblColor := TLabel.Create(gpbLine);
  lblColor.Parent := gpbLine;
  lblColor.Top := t;
  PlaceControl(bd, nil, lblColor, ppiFix(LEFT_3COL_2), lblColor.Width);
  lblColor.Caption := _rsrc(GIS_RS_LEGEND_PRM_COLOR);

  lblPattern := TLabel.Create(gpbLine);
  lblPattern.Parent := gpbLine;
  lblPattern.Top := t;
  PlaceControl(bd, nil, lblPattern, ppiFix(LEFT_3COL_3), lblPattern.Width);
  lblPattern.Caption := _rsrc(GIS_RS_LEGEND_PRM_PATTERN);

  t := t + lblStyle.Height + ppiFix(4);

  cmbStyle := TGIS_PenStyleComboBox.Create(gpbLine);
  cmbStyle.Parent := gpbLine;
  cmbStyle.Top := t;
  PlaceControl(bd, nil, cmbStyle, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbStyle.Fill(True);
  cmbStyle.OnChange := doPatternChange;
  cmbStyle.CustomEvent := doCustomPattern;
  cmbStyle.GetBitmapEvent := doGetBitmapAreaOutline;

  cmbColor := TGIS_ColorComboBox.Create(gpbLine);
  cmbColor.Parent := gpbLine;
  cmbColor.Top := t;
  cmbColor.Height := ppiFix(22);
  PlaceControl(bd, nil, cmbColor, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbColor.Fill(True, True);
  cmbColor.OnChange := doControlChange;
  cmbColor.CustomEvent := doCustomColor;

  cmbPattern := TGIS_PatternComboBox.Create(gpbLine);
  cmbPattern.Parent := gpbLine;
  cmbPattern.Top := t;
  PlaceControl(bd, nil, cmbPattern, ppiFix(LEFT_3COL_3), ppiFix(WIDTH_3COL));
  cmbPattern.Fill(True);
  cmbPattern.OnChange := doPatternChange;
  cmbPattern.CustomEvent := doCustomPattern;
  cmbPattern.GetBitmapEvent := doGetBitmapAreaFill;

  t := t + cmbPattern.Height + ppiFix(16);

  lblWidth := TLabel.Create(gpbLine);
  lblWidth.Parent := gpbLine;
  lblWidth.Top := t;
  PlaceControl(bd, nil, lblWidth, ppiFix(LEFT_3COL_1), lblWidth.Width);
  lblWidth.Caption := _rsrc(GIS_RS_LEGEND_PRM_WIDTH);

  lblSymbolGap := TLabel.Create(gpbLine);
  lblSymbolGap.Parent := gpbLine;
  lblSymbolGap.Top := t;
  PlaceControl(bd, nil, lblSymbolGap, ppiFix(LEFT_3COL_2), lblSymbolGap.Width);
  lblSymbolGap.Caption := _rsrc(GIS_RS_LEGEND_PRM_SYMBOLGAP);

  lblSymbolRotate := TLabel.Create(gpbLine);
  lblSymbolRotate.Parent := gpbLine;
  lblSymbolRotate.Top := t;
  PlaceControl(bd, nil, lblSymbolRotate, ppiFix(LEFT_3COL_3),
    lblSymbolRotate.Width);
  lblSymbolRotate.Caption := _rsrc(GIS_RS_LEGEND_PRM_SYMBOLROTATE);

  t := t + lblSymbolRotate.Height + ppiFix(4);

  cmbWidth := TGIS_SizeComboBox.Create(gpbLine);
  cmbWidth.Parent := gpbLine;
  cmbWidth.Top := t;
  PlaceControl(bd, nil, cmbWidth, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbWidth.Fill(False, True, True, True);
  cmbWidth.OnChange := doControlChange;
  cmbWidth.CustomEvent := doCustomSize;

  cmbSymbolGap := TGIS_SizeComboBox.Create(gpbLine);
  cmbSymbolGap.Parent := gpbLine;
  cmbSymbolGap.Top := t;
  PlaceControl(bd, nil, cmbSymbolGap, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbSymbolGap.Fill(False, False, False, False);
  cmbSymbolGap.OnChange := doControlChange;
  cmbSymbolGap.CustomEvent := doCustomSize;

  cmbSymbolRotate := TGIS_RotationComboBox.Create(gpbLine);
  cmbSymbolRotate.Parent := gpbLine;
  cmbSymbolRotate.Top := t;
  PlaceControl(bd, nil, cmbSymbolRotate, ppiFix(LEFT_3COL_3),
    ppiFix(WIDTH_3COL));
  cmbSymbolRotate.Fill(True);
  cmbSymbolRotate.OnChange := doControlChange;
  cmbSymbolRotate.CustomEvent := doCustomRotation;

  t := t + cmbSymbolRotate.Height + ppiFix(16);

  lblSSSize := TLabel.Create(gpbLine);
  lblSSSize.Parent := gpbLine;
  lblSSSize.Top := t;
  PlaceControl(bd, nil, lblSSSize, ppiFix(LEFT_3COL_1), lblSSSize.Width);
  lblSSSize.Caption := _rsrc(GIS_RS_LEGEND_PRM_SMART_SIZE);

  t := t + lblSSSize.Height + ppiFix(4);

  cmbSSSize := TGIS_SizeComboBox.Create(gpbLine);
  cmbSSSize.Parent := gpbLine;
  cmbSSSize.Top := t;
  PlaceControl(bd, nil, cmbSSSize, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbSSSize.Fill(False, False, True, False);
  cmbSSSize.OnChange := doControlChange;
  cmbSSSize.CustomEvent := doCustomSize;

  t := t + cmbSSSize.Height + ppiFix(16);

  gpbLine.Height := t;
end;

procedure T_panelLine.initOutline;
var
  t: Integer;
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  gpbOutline := TGroupBox.Create(Self.Panel);
  gpbOutline.Parent := Self.Panel;
  gpbOutline.Top := gpbLine.Top + gpbLine.Height + ppiFix(4);
  gpbOutline.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbOutline, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbOutline.Anchors := [akRight, akTop]
  else
    gpbOutline.Anchors := [akLeft, akTop];
  gpbOutline.Caption := _rsrc(GIS_RS_LEGEND_TAB_OUTLINE);

  t := ppiFix(24);

  lblOStyle := TLabel.Create(gpbOutline);
  lblOStyle.Parent := gpbOutline;
  lblOStyle.Top := t;
  PlaceControl(bd, nil, lblOStyle, ppiFix(LEFT_3COL_1), lblOStyle.Width);
  lblOStyle.Caption := _rsrc(GIS_RS_LEGEND_PRM_STYLE);

  lblOColor := TLabel.Create(gpbOutline);
  lblOColor.Parent := gpbOutline;
  lblOColor.Top := t;
  PlaceControl(bd, nil, lblOColor, ppiFix(LEFT_3COL_2), lblOColor.Width);
  lblOColor.Caption := _rsrc(GIS_RS_LEGEND_PRM_COLOR);

  lblOPattern := TLabel.Create(gpbOutline);
  lblOPattern.Parent := gpbOutline;
  lblOPattern.Top := t;
  PlaceControl(bd, nil, lblOPattern, ppiFix(LEFT_3COL_3), lblOPattern.Width);
  lblOPattern.Caption := _rsrc(GIS_RS_LEGEND_PRM_PATTERN);

  t := t + lblOStyle.Height + ppiFix(4);

  cmbOStyle := TGIS_PenStyleComboBox.Create(gpbOutline);
  cmbOStyle.Parent := gpbOutline;
  cmbOStyle.Top := t;
  PlaceControl(bd, nil, cmbOStyle, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbOStyle.Fill(False);
  cmbOStyle.OnChange := doOPatternChange;
  cmbOStyle.CustomEvent := doCustomPattern;
  cmbOStyle.GetBitmapEvent := doGetBitmapAreaOutline;

  cmbOColor := TGIS_ColorComboBox.Create(gpbOutline);
  cmbOColor.Parent := gpbOutline;
  cmbOColor.Top := t;
  cmbOColor.Height := ppiFix(22);
  PlaceControl(bd, nil, cmbOColor, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbOColor.Fill(True, True);
  cmbOColor.OnChange := doControlChange;
  cmbOColor.CustomEvent := doCustomColor;

  cmbOPattern := TGIS_PatternComboBox.Create(gpbOutline);
  cmbOPattern.Parent := gpbOutline;
  cmbOPattern.Top := t;
  PlaceControl(bd, nil, cmbOPattern, ppiFix(LEFT_3COL_3), ppiFix(WIDTH_3COL));
  cmbOPattern.Fill(False);
  cmbOPattern.OnChange := doOPatternChange;
  cmbOPattern.CustomEvent := doCustomPattern;
  cmbOPattern.GetBitmapEvent := doGetBitmapAreaFill;

  t := t + cmbOPattern.Height + ppiFix(16);

  lblOWidth := TLabel.Create(gpbOutline);
  lblOWidth.Parent := gpbOutline;
  lblOWidth.Top := t;
  PlaceControl(bd, nil, lblOWidth, ppiFix(LEFT_3COL_1), lblOWidth.Width);
  lblOWidth.Caption := _rsrc(GIS_RS_LEGEND_PRM_WIDTH);

  t := t + lblOWidth.Height + ppiFix(4);

  cmbOWidth := TGIS_SizeComboBox.Create(gpbOutline);
  cmbOWidth.Parent := gpbOutline;
  cmbOWidth.Top := t;
  PlaceControl(bd, nil, cmbOWidth, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbOWidth.Fill(False, True, True, True);
  cmbOWidth.OnChange := doControlChange;
  cmbOWidth.CustomEvent := doCustomSize;

  t := t + cmbOWidth.Height + ppiFix(16);

  gpbOutline.Height := t;
end;

procedure T_panelLine.Read;
begin
  lockUpdates;
  try
    ckbLegend.Checked := MVC.ParamsLine.ShowLegend;

    cmbStyle.Value := MVC.ParamsLine.StyleAsText;
    cmbWidth.Value := MVC.ParamsLine.WidthAsText;
    cmbColor.Value := MVC.ParamsLine.ColorAsText;
    cmbPattern.Value := MVC.ParamsLine.PatternAsText;
    cmbSymbolGap.Value := MVC.ParamsLine.SymbolGapAsText;
    cmbSymbolRotate.Value := MVC.ParamsLine.SymbolRotateAsText;

    // Outline
    cmbOStyle.Value := MVC.ParamsLine.OutlineStyleAsText;
    cmbOWidth.Value := MVC.ParamsLine.OutlineWidthAsText;
    cmbOColor.Value := MVC.ParamsLine.OutlineColorAsText;
    cmbOPattern.Value := MVC.ParamsLine.OutlinePatternAsText;

    // Smart size
    cmbSSSize.Value := MVC.ParamsLine.SmartSizeAsText;

    MVC.doPatternChange;
    MVC.doOPatternChange;
    MVC.doSmartSizeFieldChange;
  finally
    unlockUpdates;
  end;
end;

procedure T_panelLine.Write;
begin
  MVC.ParamsLine.ShowLegend := ckbLegend.Checked;

  MVC.ParamsLine.StyleAsText := cmbStyle.Value;
  MVC.ParamsLine.WidthAsText := cmbWidth.Value;
  MVC.ParamsLine.ColorAsText := cmbColor.Value;
  MVC.ParamsLine.PatternAsText := cmbPattern.Value;
  MVC.ParamsLine.SymbolGapAsText := cmbSymbolGap.Value;
  MVC.ParamsLine.SymbolRotateAsText := cmbSymbolRotate.Value;

  // Outline
  MVC.ParamsLine.OutlineStyleAsText := cmbOStyle.Value;
  MVC.ParamsLine.OutlineWidthAsText := cmbOWidth.Value;
  MVC.ParamsLine.OutlineColorAsText := cmbOColor.Value;
  MVC.ParamsLine.OutlinePatternAsText := cmbOPattern.Value;

  // Smart size
  MVC.ParamsLine.SmartSizeAsText := cmbSSSize.Value;
end;

procedure T_panelLine.PreparePreview(const _viewer: IGIS_Viewer);
begin
  MVC.PreparePreview(_viewer);
end;

procedure T_panelLine.UpdatePreview;
var
  ll: TGIS_Layer;
  paramsvec: TGIS_ParamsSectionVector;
begin
  oParentWindow.tmrUpdate.Enabled := False;
  if oParentWindow.gisPreview.IsEmpty then
    exit;
  oParentWindow.tmrUpdate.Enabled := True;

  ll := TGIS_Layer(oParentWindow.gisPreview.Items[0]);

  paramsvec := TGIS_ParamsSectionVector(ll.Params);

  MVC.SectionWrite(paramsvec);

  paramsvec.Labels.Value := '';
  paramsvec.Labels.Field := '';

  paramsvec.Render.Chart := '';
  paramsvec.Chart.Values := '';
  paramsvec.Query := '';

  with TGIS_ParamsSection(ll.Params) do
  begin
    MinZoom := 0;
    MaxZoom := GIS_MAX_DOUBLE;
    MinScale := 0;
    MaxScale := GIS_MAX_DOUBLE;
  end;

  // avoid unreasonable preview
  paramsvec.Line.Color := normalize_color(paramsvec.Line.Color,
    paramsvec.Line.ColorAsText);
  paramsvec.Line.OutlineColor := normalize_color(paramsvec.Line.OutlineColor,
    paramsvec.Line.OutlineColorAsText);
  paramsvec.Line.Width := normalize_size(oParentWindow.gisPreview,
    paramsvec.Line.Width, paramsvec.Line.WidthAsText, MAX_PREVIEW_SIZE_LINE);
  paramsvec.Line.OutlineWidth := normalize_size(oParentWindow.gisPreview,
    paramsvec.Line.OutlineWidth, paramsvec.Line.OutlineWidthAsText,
    MAX_PREVIEW_SIZE_OUTLINE);
  paramsvec.Line.SymbolRotate := normalize_angle(paramsvec.Line.SymbolRotate,
    paramsvec.Line.SymbolRotateAsText);
end;

procedure T_panelLine.doCallback(_sender: TObject; _code: Integer);

  procedure do_ssfield_change;
  begin

  end;

  procedure do_update_symbol_bitmap;
  var
    bsymfnt: Boolean;
    bsymcgm: Boolean;
    bsymsvg: Boolean;
    bsympic: Boolean;
    bsymlin: Boolean;
    sym: TGIS_SymbolAbstract;
    bbmp: Boolean;
  begin
    sym := MVC.ParamsLine.Symbol;

    bsymfnt := assigned(sym) and (sym is TGIS_SymbolFont);
    bsymcgm := assigned(sym) and (sym is TGIS_SymbolCGM);
    bsymsvg := assigned(sym) and (sym is TGIS_SymbolSVG);
    bsympic := assigned(sym) and (sym is TGIS_SymbolPicture);
    bsymlin := assigned(sym) and
      ((sym is TGIS_SymbolLine) or (sym is TGIS_SymbolLineEx));
    bbmp := not TGIS_Bitmap.IsNilOrEmpty(MVC.ParamsLine.Bitmap);

    if bsymfnt or bsymcgm or bsymsvg or bsymlin then
    begin
      lblColor.Enabled := True;
      cmbColor.Enabled := True;
      lblPattern.Enabled := False;
      cmbPattern.Enabled := False;
      lblSymbolGap.Enabled := True;
      cmbSymbolGap.Enabled := True;
      lblSymbolRotate.Enabled := True;
      cmbSymbolRotate.Enabled := True;
    end
    else if bsympic then
    begin
      lblColor.Enabled := False;
      cmbColor.Enabled := False;
      lblPattern.Enabled := False;
      cmbPattern.Enabled := False;
      lblSymbolGap.Enabled := True;
      cmbSymbolGap.Enabled := True;
      lblSymbolRotate.Enabled := True;
      cmbSymbolRotate.Enabled := True;
    end
    else if bbmp then
    begin
      lblColor.Enabled := False;
      cmbColor.Enabled := False;
      lblPattern.Enabled := True;
      cmbPattern.Enabled := True;
      lblSymbolGap.Enabled := False;
      cmbSymbolGap.Enabled := False;
      lblSymbolRotate.Enabled := False;
      cmbSymbolRotate.Enabled := False;
    end
    else
    begin
      lblColor.Enabled := True;
      cmbColor.Enabled := True;
      lblPattern.Enabled := True;
      cmbPattern.Enabled := True;
      lblSymbolGap.Enabled := False;
      cmbSymbolGap.Enabled := False;
      lblSymbolRotate.Enabled := False;
      cmbSymbolRotate.Enabled := False;
    end;
  end;

  procedure do_update_osymbol_obitmap;
  var
    bsymfnt: Boolean;
    bsymcgm: Boolean;
    bsymsvg: Boolean;
    bsympic: Boolean;
    bsymlin: Boolean;
    sym: TGIS_SymbolAbstract;
    bbmp: Boolean;
  begin
    sym := MVC.ParamsLine.Symbol;

    bsymfnt := assigned(sym) and (sym is TGIS_SymbolFont);
    bsymcgm := assigned(sym) and (sym is TGIS_SymbolCGM);
    bsymsvg := assigned(sym) and (sym is TGIS_SymbolSVG);
    bsympic := assigned(sym) and (sym is TGIS_SymbolPicture);
    bsymlin := assigned(sym) and
      ((sym is TGIS_SymbolLine) or (sym is TGIS_SymbolLineEx));
    bbmp := not TGIS_Bitmap.IsNilOrEmpty(MVC.ParamsLine.OutlineBitmap);

    if bsymfnt or bsymcgm or bsymsvg or bsymlin then
    begin
      lblOStyle.Enabled := False;
      cmbOStyle.Enabled := False;
      lblOColor.Enabled := True;
      cmbOColor.Enabled := True;
      lblOPattern.Enabled := False;
      cmbOPattern.Enabled := False;
      lblOWidth.Enabled := False;
      cmbOWidth.Enabled := False;
    end
    else if bsympic then
    begin
      lblOStyle.Enabled := False;
      cmbOStyle.Enabled := False;
      lblOColor.Enabled := False;
      cmbOColor.Enabled := False;
      lblOPattern.Enabled := False;
      cmbOPattern.Enabled := False;
      lblOWidth.Enabled := False;
      cmbOWidth.Enabled := False;
    end
    else if bbmp then
    begin
      lblOStyle.Enabled := True;
      cmbOStyle.Enabled := True;
      lblOColor.Enabled := False;
      cmbOColor.Enabled := False;
      lblOPattern.Enabled := True;
      cmbOPattern.Enabled := True;
      lblOWidth.Enabled := True;
      cmbOWidth.Enabled := True;
    end
    else
    begin
      lblOStyle.Enabled := True;
      cmbOStyle.Enabled := True;
      lblOColor.Enabled := True;
      cmbOColor.Enabled := True;
      lblOPattern.Enabled := True;
      cmbOPattern.Enabled := True;
      lblOWidth.Enabled := True;
      cmbOWidth.Enabled := True;
    end;
  end;

begin
  if not blockedUpdates then
  begin
    Write;
    UpdatePreview;
  end;

  case _code of
    60:
      do_ssfield_change;
    100:
      begin
        do_update_symbol_bitmap;
        do_update_osymbol_obitmap;
      end;
    130:
      do_update_osymbol_obitmap
  end;

end;

procedure T_panelLine.doOPatternChange(_sender: TObject);
begin
  MVC.doOPatternChange;
end;

procedure T_panelLine.doPatternChange(_sender: TObject);
begin
  MVC.doPatternChange;
end;

procedure T_panelLine.doSmartSizeFieldChange(_sender: TObject);
begin
  MVC.doSmartSizeFieldChange;
end;

procedure T_panelLine.doControlChange(_sender: TObject);
begin
  MVC.doControlChange;
end;

{$ENDREGION}
{$REGION 'T_panelArea'}

function T_panelArea.fget_HasPreview: Boolean;
begin
  Result := MVC.HasPreview;
end;

procedure T_panelArea.init;
begin
  inherited;

  ItemText := _rsrc(GIS_RS_LEGEND_PAG_AREA);

  MVC := oParentWindow.MVC.Area;
  MVC.Callback := doCallback;

  initArea;
  initOutline;
  initSelf;
end;

procedure T_panelArea.initSelf;
var
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  ckbLegend := TCheckBox.Create(Self.Panel);
  ckbLegend.Parent := Self.Panel;
  ckbLegend.Top := gpbOutline.Top + gpbOutline.Height + ppiFix(8);
  PlaceControl(bd, nil, ckbLegend, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_2COL));
  if bd = bdRightToLeft then
    ckbLegend.Anchors := [akRight, akTop]
  else
    ckbLegend.Anchors := [akLeft, akTop];
  ckbLegend.Caption := _rsrc(GIS_RS_LEGEND_PRM_INCLUDEINLEGEND);
end;

procedure T_panelArea.initArea;
var
  t: Integer;
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  gpbArea := TGroupBox.Create(Self.Panel);
  gpbArea.Parent := Self.Panel;
  gpbArea.Top := ppiFix(8);
  gpbArea.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbArea, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbArea.Anchors := [akRight, akTop]
  else
    gpbArea.Anchors := [akLeft, akTop];
  gpbArea.Caption := _rsrc(GIS_RS_LEGEND_TAB_AREA);

  t := ppiFix(24);

  lblPattern := TLabel.Create(gpbArea);
  lblPattern.Parent := gpbArea;
  lblPattern.Top := t;
  PlaceControl(bd, nil, lblPattern, ppiFix(LEFT_3COL_1), lblPattern.Width);
  lblPattern.Caption := _rsrc(GIS_RS_LEGEND_PRM_PATTERN);

  lblColor := TLabel.Create(gpbArea);
  lblColor.Parent := gpbArea;
  lblColor.Top := t;
  PlaceControl(bd, nil, lblColor, ppiFix(LEFT_3COL_2), lblColor.Width);
  lblColor.Caption := _rsrc(GIS_RS_LEGEND_PRM_COLOR);

  t := t + lblColor.Height + ppiFix(4);

  cmbPattern := TGIS_PatternComboBox.Create(gpbArea);
  cmbPattern.Parent := gpbArea;
  cmbPattern.Top := t;
  PlaceControl(bd, nil, cmbPattern, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbPattern.Fill(True);
  cmbPattern.OnChange := doPatternChange;
  cmbPattern.CustomEvent := doCustomPattern;
  cmbPattern.GetBitmapEvent := doGetBitmapAreaFill;

  cmbColor := TGIS_ColorComboBox.Create(gpbArea);
  cmbColor.Parent := gpbArea;
  cmbColor.Top := t;
  cmbColor.Height := ppiFix(22);
  PlaceControl(bd, nil, cmbColor, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbColor.Fill(True, True);
  cmbColor.OnChange := doControlChange;
  cmbColor.CustomEvent := doCustomColor;

  t := t + cmbColor.Height + ppiFix(16);

  lblSymbolSize := TLabel.Create(gpbArea);
  lblSymbolSize.Parent := gpbArea;
  lblSymbolSize.Top := t;
  PlaceControl(bd, nil, lblSymbolSize, ppiFix(LEFT_3COL_1),
    lblSymbolSize.Width);
  lblSymbolSize.Caption := _rsrc(GIS_RS_LEGEND_PRM_SYMBOLSIZE);

  lblSymbolGap := TLabel.Create(gpbArea);
  lblSymbolGap.Parent := gpbArea;
  lblSymbolGap.Top := t;
  PlaceControl(bd, nil, lblSymbolGap, ppiFix(LEFT_3COL_2), lblSymbolGap.Width);
  lblSymbolGap.Caption := _rsrc(GIS_RS_LEGEND_PRM_SYMBOLGAP);

  lblSymbolRotate := TLabel.Create(gpbArea);
  lblSymbolRotate.Parent := gpbArea;
  lblSymbolRotate.Top := t;
  PlaceControl(bd, nil, lblSymbolRotate, ppiFix(LEFT_3COL_3),
    lblSymbolRotate.Width);
  lblSymbolRotate.Caption := _rsrc(GIS_RS_LEGEND_PRM_SYMBOLROTATE);

  t := t + lblSymbolGap.Height + ppiFix(4);

  cmbSymbolSize := TGIS_SizeComboBox.Create(gpbArea);
  cmbSymbolSize.Parent := gpbArea;
  cmbSymbolSize.Top := t;
  PlaceControl(bd, nil, cmbSymbolSize, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbSymbolSize.Fill(True, False, False, False);
  cmbSymbolSize.OnChange := doControlChange;
  cmbSymbolSize.CustomEvent := doCustomSize;

  cmbSymbolGap := TGIS_SizeComboBox.Create(gpbArea);
  cmbSymbolGap.Parent := gpbArea;
  cmbSymbolGap.Top := t;
  PlaceControl(bd, nil, cmbSymbolGap, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbSymbolGap.Fill(False, False, False, False);
  cmbSymbolGap.OnChange := doControlChange;
  cmbSymbolGap.CustomEvent := doCustomSize;

  cmbSymbolRotate := TGIS_RotationComboBox.Create(gpbArea);
  cmbSymbolRotate.Parent := gpbArea;
  cmbSymbolRotate.Top := t;
  PlaceControl(bd, nil, cmbSymbolRotate, ppiFix(LEFT_3COL_3),
    ppiFix(WIDTH_3COL));
  cmbSymbolRotate.Fill(True);
  cmbSymbolRotate.OnChange := doControlChange;
  cmbSymbolRotate.CustomEvent := doCustomRotation;

  t := t + cmbSymbolRotate.Height + ppiFix(16);

  lblSSSize := TLabel.Create(gpbArea);
  lblSSSize.Parent := gpbArea;
  lblSSSize.Top := t;
  PlaceControl(bd, nil, lblSSSize, ppiFix(LEFT_3COL_1), lblSSSize.Width);
  lblSSSize.Caption := _rsrc(GIS_RS_LEGEND_PRM_SMART_SIZE);

  t := t + lblSymbolGap.Height + ppiFix(4);

  cmbSSSize := TGIS_SizeComboBox.Create(gpbArea);
  cmbSSSize.Parent := gpbArea;
  cmbSSSize.Top := t;
  PlaceControl(bd, nil, cmbSSSize, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbSSSize.Fill(False, False, True, False);
  cmbSSSize.OnChange := doControlChange;
  cmbSSSize.CustomEvent := doCustomSize;

  t := t + cmbSSSize.Height + ppiFix(16);

  gpbArea.Height := t;
end;

procedure T_panelArea.initOutline;
var
  t: Integer;
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  gpbOutline := TGroupBox.Create(Self.Panel);
  gpbOutline.Parent := Self.Panel;
  gpbOutline.Top := gpbArea.Top + gpbArea.Height + ppiFix(4);
  gpbOutline.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbOutline, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbOutline.Anchors := [akRight, akTop]
  else
    gpbOutline.Anchors := [akLeft, akTop];
  gpbOutline.Caption := _rsrc(GIS_RS_LEGEND_TAB_OUTLINE);

  t := ppiFix(24);

  lblOStyle := TLabel.Create(gpbOutline);
  lblOStyle.Parent := gpbOutline;
  lblOStyle.Top := t;
  PlaceControl(bd, nil, lblOStyle, ppiFix(LEFT_3COL_1), lblOStyle.Width);
  lblOStyle.Caption := _rsrc(GIS_RS_LEGEND_PRM_STYLE);

  lblOColor := TLabel.Create(gpbOutline);
  lblOColor.Parent := gpbOutline;
  lblOColor.Top := t;
  PlaceControl(bd, nil, lblOColor, ppiFix(LEFT_3COL_2), lblOColor.Width);
  lblOColor.Caption := _rsrc(GIS_RS_LEGEND_PRM_COLOR);

  lblOPattern := TLabel.Create(gpbOutline);
  lblOPattern.Parent := gpbOutline;
  lblOPattern.Top := t;
  PlaceControl(bd, nil, lblOPattern, ppiFix(LEFT_3COL_3), lblOPattern.Width);
  lblOPattern.Caption := _rsrc(GIS_RS_LEGEND_PRM_PATTERN);

  t := t + lblOStyle.Height + ppiFix(4);

  cmbOStyle := TGIS_PenStyleComboBox.Create(gpbOutline);
  cmbOStyle.Parent := gpbOutline;
  cmbOStyle.Top := t;
  PlaceControl(bd, nil, cmbOStyle, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbOStyle.Fill(True);
  cmbOStyle.OnChange := doOPatternChange;
  cmbOStyle.CustomEvent := doCustomPattern;
  cmbOStyle.GetBitmapEvent := doGetBitmapAreaOutline;

  cmbOColor := TGIS_ColorComboBox.Create(gpbOutline);
  cmbOColor.Parent := gpbOutline;
  cmbOColor.Top := t;
  cmbOColor.Height := ppiFix(22);
  PlaceControl(bd, nil, cmbOColor, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbOColor.Fill(True, True);
  cmbOColor.OnChange := doControlChange;
  cmbOColor.CustomEvent := doCustomColor;

  cmbOPattern := TGIS_PatternComboBox.Create(gpbOutline);
  cmbOPattern.Parent := gpbOutline;
  cmbOPattern.Top := t;
  PlaceControl(bd, nil, cmbOPattern, ppiFix(LEFT_3COL_3), ppiFix(WIDTH_3COL));
  cmbOPattern.Fill(False);
  cmbOPattern.OnChange := doOPatternChange;
  cmbOPattern.CustomEvent := doCustomPattern;
  cmbOPattern.GetBitmapEvent := doGetBitmapAreaFill;

  t := t + cmbOPattern.Height + ppiFix(16);

  lblOWidth := TLabel.Create(gpbOutline);
  lblOWidth.Parent := gpbOutline;
  lblOWidth.Top := t;
  PlaceControl(bd, nil, lblOWidth, ppiFix(LEFT_3COL_1), lblOWidth.Width);
  lblOWidth.Caption := _rsrc(GIS_RS_LEGEND_PRM_WIDTH);

  lblOSymbolGap := TLabel.Create(gpbOutline);
  lblOSymbolGap.Parent := gpbOutline;
  lblOSymbolGap.Top := t;
  PlaceControl(bd, nil, lblOSymbolGap, ppiFix(LEFT_3COL_2),
    lblOSymbolGap.Width);
  lblOSymbolGap.Caption := _rsrc(GIS_RS_LEGEND_PRM_SYMBOLGAP);

  lblOSymbolRotate := TLabel.Create(gpbOutline);
  lblOSymbolRotate.Parent := gpbOutline;
  lblOSymbolRotate.Top := t;
  PlaceControl(bd, nil, lblOSymbolRotate, ppiFix(LEFT_3COL_3),
    lblOSymbolRotate.Width);
  lblOSymbolRotate.Caption := _rsrc(GIS_RS_LEGEND_PRM_SYMBOLROTATE);

  t := t + lblOSymbolRotate.Height + ppiFix(4);

  cmbOWidth := TGIS_SizeComboBox.Create(gpbOutline);
  cmbOWidth.Parent := gpbOutline;
  cmbOWidth.Top := t;
  PlaceControl(bd, nil, cmbOWidth, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbOWidth.Fill(False, True, True, True);
  cmbOWidth.OnChange := doControlChange;
  cmbOWidth.CustomEvent := doCustomSize;

  cmbOSymbolGap := TGIS_SizeComboBox.Create(gpbOutline);
  cmbOSymbolGap.Parent := gpbOutline;
  cmbOSymbolGap.Top := t;
  PlaceControl(bd, nil, cmbOSymbolGap, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbOSymbolGap.Fill(False, False, False, False);
  cmbOSymbolGap.OnChange := doControlChange;
  cmbOSymbolGap.CustomEvent := doCustomSize;

  cmbOSymbolRotate := TGIS_RotationComboBox.Create(gpbOutline);
  cmbOSymbolRotate.Parent := gpbOutline;
  cmbOSymbolRotate.Top := t;
  PlaceControl(bd, nil, cmbOSymbolRotate, ppiFix(LEFT_3COL_3),
    ppiFix(WIDTH_3COL));
  cmbOSymbolRotate.Fill(False);
  cmbOSymbolRotate.OnChange := doControlChange;
  cmbOSymbolRotate.CustomEvent := doCustomRotation;

  t := t + cmbOSymbolRotate.Height + ppiFix(16);

  gpbOutline.Height := t;
end;

procedure T_panelArea.Read;
begin
  lockUpdates;
  try
    ckbLegend.Checked := MVC.ParamsArea.ShowLegend;

    cmbPattern.Value := MVC.ParamsArea.PatternAsText;
    cmbColor.Value := MVC.ParamsArea.ColorAsText;
    cmbSymbolGap.Value := MVC.ParamsArea.SymbolGapAsText;
    cmbSymbolRotate.Value := MVC.ParamsArea.SymbolRotateAsText;
    cmbSymbolSize.Value := MVC.ParamsArea.SymbolSizeAsText;

    // Outline
    cmbOStyle.Value := MVC.ParamsArea.OutlineStyleAsText;
    cmbOWidth.Value := MVC.ParamsArea.OutlineWidthAsText;
    cmbOColor.Value := MVC.ParamsArea.OutlineColorAsText;
    cmbOPattern.Value := MVC.ParamsArea.OutlinePatternAsText;
    cmbOSymbolGap.Value := MVC.ParamsArea.OutlineSymbolGapAsText;
    cmbOSymbolRotate.Value := MVC.ParamsArea.OutlineSymbolRotateAsText;

    // Smart size
    cmbSSSize.Value := MVC.ParamsArea.SmartSizeAsText;

    MVC.doPatternChange;
    MVC.doOPatternChange;
    MVC.doSmartSizeFieldChange;

  finally
    unlockUpdates;
  end;
end;

procedure T_panelArea.Write;
begin
  MVC.ParamsArea.ShowLegend := ckbLegend.Checked;

  MVC.ParamsArea.PatternAsText := cmbPattern.Value;
  MVC.ParamsArea.ColorAsText := cmbColor.Value;
  MVC.ParamsArea.SymbolGapAsText := cmbSymbolGap.Value;
  MVC.ParamsArea.SymbolRotateAsText := cmbSymbolRotate.Value;
  MVC.ParamsArea.SymbolSizeAsText := cmbSymbolSize.Value;

  // Outline
  MVC.ParamsArea.OutlineStyleAsText := cmbOStyle.Value;
  MVC.ParamsArea.OutlineWidthAsText := cmbOWidth.Value;
  MVC.ParamsArea.OutlineColorAsText := cmbOColor.Value;
  MVC.ParamsArea.OutlinePatternAsText := cmbOPattern.Value;
  MVC.ParamsArea.OutlineSymbolGapAsText := cmbOSymbolGap.Value;
  MVC.ParamsArea.OutlineSymbolRotateAsText := cmbOSymbolRotate.Value;

  // Smart size
  MVC.ParamsArea.SmartSizeAsText := cmbSSSize.Value;
end;

procedure T_panelArea.PreparePreview(const _viewer: IGIS_Viewer);
begin
  MVC.PreparePreview(_viewer);
end;

procedure T_panelArea.UpdatePreview;
var
  ll: TGIS_Layer;
  paramsvec: TGIS_ParamsSectionVector;
begin
  oParentWindow.tmrUpdate.Enabled := False;
  if oParentWindow.gisPreview.IsEmpty then
    exit;
  oParentWindow.tmrUpdate.Enabled := True;

  ll := TGIS_Layer(oParentWindow.gisPreview.Items[0]);

  paramsvec := TGIS_ParamsSectionVector(ll.Params);

  MVC.SectionWrite(paramsvec);

  paramsvec.Labels.Value := '';
  paramsvec.Labels.Field := '';

  paramsvec.Render.Chart := '';
  paramsvec.Chart.Values := '';
  paramsvec.Query := '';

  with TGIS_ParamsSection(ll.Params) do
  begin
    MinZoom := 0;
    MaxZoom := GIS_MAX_DOUBLE;
    MinScale := 0;
    MaxScale := GIS_MAX_DOUBLE;
  end;

  // avoid unreasonable preview
  paramsvec.Area.Color := normalize_color(paramsvec.Area.Color,
    paramsvec.Area.ColorAsText);
  paramsvec.Area.OutlineColor := normalize_color(paramsvec.Area.OutlineColor,
    paramsvec.Area.OutlineColorAsText);
  paramsvec.Area.SymbolSize := normalize_size(oParentWindow.gisPreview,
    paramsvec.Area.SymbolSize, paramsvec.Area.SymbolSizeAsText,
    MAX_PREVIEW_SIZE_PATTERNSYMBOL);
  paramsvec.Area.OutlineWidth := normalize_size(oParentWindow.gisPreview,
    paramsvec.Area.OutlineWidth, paramsvec.Area.OutlineWidthAsText,
    MAX_PREVIEW_SIZE_OUTLINE);
  paramsvec.Area.SymbolRotate := normalize_angle(paramsvec.Area.SymbolRotate,
    paramsvec.Area.SymbolRotateAsText);
end;

procedure T_panelArea.doCallback(_sender: TObject; _code: Integer);

  procedure do_ssfield_change;
  begin
  end;

  procedure do_update_symbol_bitmap;
  var
    bsymfnt: Boolean;
    bsymcgm: Boolean;
    bsymsvg: Boolean;
    bsympic: Boolean;
    bsymlin: Boolean;
    sym: TGIS_SymbolAbstract;
    bbmp: Boolean;
  begin
    sym := MVC.ParamsArea.Symbol;

    bsymfnt := assigned(sym) and (sym is TGIS_SymbolFont);
    bsymcgm := assigned(sym) and (sym is TGIS_SymbolCGM);
    bsymsvg := assigned(sym) and (sym is TGIS_SymbolSVG);
    bsympic := assigned(sym) and (sym is TGIS_SymbolPicture);
    bsymlin := assigned(sym) and
      ((sym is TGIS_SymbolLine) or (sym is TGIS_SymbolLineEx) or (sym is TGIS_SymbolHatch));
    bbmp := not TGIS_Bitmap.IsNilOrEmpty(MVC.ParamsArea.Bitmap);

    if bsymfnt or bsymcgm or bsymsvg or bsymlin then
    begin
      lblColor.Enabled := True;
      cmbColor.Enabled := True;
      lblSymbolGap.Enabled := True;
      cmbSymbolGap.Enabled := True;
      lblSymbolRotate.Enabled := True;
      cmbSymbolRotate.Enabled := True;
      lblSymbolSize.Enabled := True;
      cmbSymbolSize.Enabled := True;
    end
    else if bsympic then
    begin
      lblColor.Enabled := False;
      cmbColor.Enabled := False;
      lblSymbolGap.Enabled := True;
      cmbSymbolGap.Enabled := True;
      lblSymbolRotate.Enabled := True;
      cmbSymbolRotate.Enabled := True;
      lblSymbolSize.Enabled := True;
      cmbSymbolSize.Enabled := True;
    end
    else if bbmp then
    begin
      lblColor.Enabled := False;
      cmbColor.Enabled := False;
      lblSymbolGap.Enabled := False;
      cmbSymbolGap.Enabled := False;
      lblSymbolRotate.Enabled := False;
      cmbSymbolRotate.Enabled := False;
      lblSymbolSize.Enabled := False;
      cmbSymbolSize.Enabled := False;
    end
    else
    begin
      lblColor.Enabled := True;
      cmbColor.Enabled := True;
      lblSymbolGap.Enabled := False;
      cmbSymbolGap.Enabled := False;
      lblSymbolRotate.Enabled := False;
      cmbSymbolRotate.Enabled := False;
      lblSymbolSize.Enabled := False;
      cmbSymbolSize.Enabled := False;
    end;
  end;

  procedure do_update_osymbol_obitmap;
  var
    bsymfnt: Boolean;
    bsymcgm: Boolean;
    bsymsvg: Boolean;
    bsympic: Boolean;
    bsymlin: Boolean;
    sym: TGIS_SymbolAbstract;
    bbmp: Boolean;
  begin
    sym := MVC.ParamsArea.OutlineSymbol;

    bsymfnt := assigned(sym) and (sym is TGIS_SymbolFont);
    bsymcgm := assigned(sym) and (sym is TGIS_SymbolCGM);
    bsymsvg := assigned(sym) and (sym is TGIS_SymbolSVG);
    bsympic := assigned(sym) and (sym is TGIS_SymbolPicture);
    bsymlin := assigned(sym) and
      ((sym is TGIS_SymbolLine) or (sym is TGIS_SymbolLineEx));
    bbmp := not TGIS_Bitmap.IsNilOrEmpty(MVC.ParamsArea.OutlineBitmap);

    if bsymfnt or bsymcgm or bsymsvg or bsymlin then
    begin
      lblOColor.Enabled := True;
      cmbOColor.Enabled := True;
      lblOPattern.Enabled := False;
      cmbOPattern.Enabled := False;
      lblOSymbolGap.Enabled := True;
      cmbOSymbolGap.Enabled := True;
      lblOSymbolRotate.Enabled := True;
      cmbOSymbolRotate.Enabled := True;
    end
    else if bsympic then
    begin
      lblOColor.Enabled := False;
      cmbOColor.Enabled := False;
      lblOPattern.Enabled := False;
      cmbOPattern.Enabled := False;
      lblOSymbolGap.Enabled := True;
      cmbOSymbolGap.Enabled := True;
      lblOSymbolRotate.Enabled := True;
      cmbOSymbolRotate.Enabled := True;
    end
    else if bbmp then
    begin
      lblOColor.Enabled := False;
      cmbOColor.Enabled := False;
      lblOPattern.Enabled := True;
      cmbOPattern.Enabled := True;
      lblOSymbolGap.Enabled := False;
      cmbOSymbolGap.Enabled := False;
      lblOSymbolRotate.Enabled := False;
      cmbOSymbolRotate.Enabled := False;
    end
    else
    begin
      lblOColor.Enabled := True;
      cmbOColor.Enabled := True;
      lblOPattern.Enabled := True;
      cmbOPattern.Enabled := True;
      lblOSymbolGap.Enabled := False;
      cmbOSymbolGap.Enabled := False;
      lblOSymbolRotate.Enabled := False;
      cmbOSymbolRotate.Enabled := False;
    end;
  end;

begin
  if not blockedUpdates then
  begin
    Write;
    UpdatePreview;
  end;

  case _code of
    60:
      do_ssfield_change;
    100:
      do_update_symbol_bitmap;
    130:
      do_update_osymbol_obitmap
  end;

end;

procedure T_panelArea.doOPatternChange(_sender: TObject);
begin
  MVC.doOPatternChange;
end;

procedure T_panelArea.doPatternChange(_sender: TObject);
begin
  MVC.doPatternChange;
end;

procedure T_panelArea.doSmartSizeFieldChange(_sender: TObject);
begin
  MVC.doSmartSizeFieldChange;
end;

procedure T_panelArea.doControlChange(_sender: TObject);
begin
  MVC.doControlChange;
end;

{$ENDREGION}
{$REGION 'T_panelLabel'}

function T_panelLabel.fget_HasPreview: Boolean;
begin
  Result := MVC.HasPreview;
end;

procedure T_panelLabel.init;
var
  bd: TBiDiMode;
begin
  inherited;

  ItemText := _rsrc(GIS_RS_LEGEND_PAG_LABEL);

  MVC := oParentWindow.MVC.&Label;
  MVC.Callback := doCallback;

  initSelf;
  initFont;
  initLabel;
  initOutline;
  initPosition;

  bd := oParentWindow.BiDiMode;
  ckbLegend := TCheckBox.Create(Self.Panel);
  ckbLegend.Parent := Self.Panel;
  ckbLegend.Top := gpbPosition.Top + gpbPosition.Height + ppiFix(8);
  PlaceControl(bd, nil, ckbLegend, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_2COL));
  if bd = bdRightToLeft then
    ckbLegend.Anchors := [akRight, akTop]
  else
    ckbLegend.Anchors := [akLeft, akTop];
  ckbLegend.Caption := _rsrc(GIS_RS_LEGEND_PRM_INCLUDEINLEGEND);
end;

procedure T_panelLabel.initSelf;
var
  t: Integer;
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  gpbField := TGroupBox.Create(Self.Panel);
  gpbField.Parent := Self.Panel;
  gpbField.Top := ppiFix(8);
  gpbField.Height := ppiFix(256);
  PlaceControl(bd, nil, gpbField, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbField.Anchors := [akRight, akTop]
  else
    gpbField.Anchors := [akLeft, akTop];
  gpbField.Caption := _rsrc(GIS_RS_LEGEND_PRM_VALUE);

  t := ppiFix(24);

  cmbValue := TGIS_FieldValueComboBox.Create(gpbField);
  cmbValue.AutoComplete := False;
  cmbValue.Parent := gpbField;
  cmbValue.Top := t;
  PlaceControl(bd, nil, cmbValue, ppiFix(LEFT_3COL_1), -1);
  cmbValue.OnChange := doFieldChange;
  cmbValue.Fill(oParentWindow.MVC.FieldNames);
  cmbValue.ItemIndex := -1;

  t := t + cmbValue.Height + ppiFix(8);

  ckbVisible := TCheckBox.Create(gpbField);
  ckbVisible.Parent := gpbField;
  ckbVisible.Top := t;
  PlaceControl(bd, nil, ckbVisible, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_2COL));
  ckbVisible.Caption := _rsrc(GIS_RS_LEGEND_PRM_VISIBLE);
  ckbVisible.Checked := True;

  ckbPAvoidOverlapping := TCheckBox.Create(gpbField);
  ckbPAvoidOverlapping.Parent := gpbField;
  ckbPAvoidOverlapping.Top := t;
  PlaceControl(bd, nil, ckbPAvoidOverlapping, ppiFix(LEFT_3COL_2),
    ppiFix(WIDTH_2COL));
  ckbPAvoidOverlapping.Caption := _rsrc(GIS_RS_LEGEND_PRM_AVOIDOVERLAP);
  ckbPAvoidOverlapping.Checked := True;

  ckbPAvoidDuplicates := TCheckBox.Create(gpbField);
  ckbPAvoidDuplicates.Parent := gpbField;
  ckbPAvoidDuplicates.Top := t;
  PlaceControl(bd, nil, ckbPAvoidDuplicates, ppiFix(LEFT_3COL_3),
    ppiFix(WIDTH_3COL));
  ckbPAvoidDuplicates.Caption := _rsrc(GIS_RS_LEGEND_PRM_AVOIDDUPLICATES);
  ckbPAvoidDuplicates.Checked := True;

  gpbField.Height := t + ckbVisible.Height + ppiFix(16);

  dlgFont := TFontDialog.Create(Self.Panel);
end;

procedure T_panelLabel.initFont;
var
  t: Integer;
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  gpbFont := TGroupBox.Create(Self.Panel);
  gpbFont.Parent := Self.Panel;
  gpbFont.Top := gpbField.Top + gpbField.Height + ppiFix(8);
  gpbFont.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbFont, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbFont.Anchors := [akRight, akTop]
  else
    gpbFont.Anchors := [akLeft, akTop];
  gpbFont.Caption := _rsrc(GIS_RS_LEGEND_PRM_FONT);

  t := ppiFix(24);

  lblFontName := TLabel.Create(gpbFont);
  lblFontName.Parent := gpbFont;
  lblFontName.Top := t;
  PlaceControl(bd, nil, lblFontName, ppiFix(LEFT_3COL_1), lblFontName.Width);
  lblFontName.Caption := _rsrc(GIS_RS_LEGEND_PRM_NAME);

  lblFontSize := TLabel.Create(gpbFont);
  lblFontSize.Parent := gpbFont;
  lblFontSize.Top := t;
  PlaceControl(bd, nil, lblFontSize, ppiFix(LEFT_3COL_2), lblFontSize.Width);
  lblFontSize.Caption := _rsrc(GIS_RS_LEGEND_PRM_SIZE);

  lblFontColor := TLabel.Create(gpbFont);
  lblFontColor.Parent := gpbFont;
  lblFontColor.Top := t;
  PlaceControl(bd, nil, lblFontColor, ppiFix(LEFT_3COL_3), lblFontColor.Width);
  lblFontColor.Caption := _rsrc(GIS_RS_LEGEND_PRM_COLOR);

  t := t + lblFontColor.Height + ppiFix(4);

  cmbFontName := TGIS_FontNameComboBox.Create(gpbFont);
  cmbFontName.Parent := gpbFont;
  cmbFontName.Top := t;
  PlaceControl(bd, nil, cmbFontName, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbFontName.Fill;
  cmbFontName.OnChange := doControlChange;

  cmbFontSize := TGIS_SizeComboBox.Create(gpbFont);
  cmbFontSize.Parent := gpbFont;
  cmbFontSize.Top := t;
  PlaceControl(bd, nil, cmbFontSize, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbFontSize.Fill(True, False, True, True);
  cmbFontSize.OnChange := doControlChange;
  cmbFontSize.CustomEvent := doCustomSize;

  cmbFontColor := TGIS_ColorComboBox.Create(gpbFont);
  cmbFontColor.Parent := gpbFont;
  cmbFontColor.Top := t;
  PlaceControl(bd, nil, cmbFontColor, ppiFix(LEFT_3COL_3), ppiFix(WIDTH_3COL));
  cmbFontColor.Height := ppiFix(22);
  cmbFontColor.Fill(True, True);
  cmbFontColor.OnChange := doControlChange;
  cmbFontColor.CustomEvent := doCustomColor;

  t := t + cmbFontColor.Height + ppiFix(4);

  ckbFontStyleBold := TCheckBox.Create(gpbFont);
  ckbFontStyleBold.Parent := gpbFont;
  ckbFontStyleBold.Top := t;
  PlaceControl(bd, nil, ckbFontStyleBold, ppiFix(LEFT_3COL_1),
    ckbFontStyleBold.Width);
  ckbFontStyleBold.Caption := _rsrc(GIS_RS_LEGEND_DLGFONT_CHKBOLD);
  ckbFontStyleBold.OnClick := doControlChange;

  ckbFontStyleUnderline := TCheckBox.Create(gpbFont);
  ckbFontStyleUnderline.Parent := gpbFont;
  ckbFontStyleUnderline.Top := t;
  PlaceControl(bd, nil, ckbFontStyleUnderline, ppiFix(LEFT_3COL_2),
    ckbFontStyleUnderline.Width);
  ckbFontStyleUnderline.Caption := _rsrc(GIS_RS_LEGEND_DLGFONT_CHKUNDERLINE);
  ckbFontStyleUnderline.OnClick := doControlChange;

  ckbFontStyleShadow := TCheckBox.Create(gpbFont);
  ckbFontStyleShadow.Parent := gpbFont;
  ckbFontStyleShadow.Top := t;
  PlaceControl(bd, nil, ckbFontStyleShadow, ppiFix(LEFT_3COL_3),
    ckbFontStyleShadow.Width);
  ckbFontStyleShadow.Caption := _rsrc(GIS_RS_LEGEND_PRM_SHADOW);
  ckbFontStyleShadow.OnClick := doShadowChange;

  t := t + ckbFontStyleShadow.Height + ppiFix(4);

  ckbFontStyleItalic := TCheckBox.Create(gpbFont);
  ckbFontStyleItalic.Parent := gpbFont;
  ckbFontStyleItalic.Top := t;
  PlaceControl(bd, nil, ckbFontStyleItalic, ppiFix(LEFT_3COL_1),
    ckbFontStyleItalic.Width);
  ckbFontStyleItalic.Caption := _rsrc(GIS_RS_LEGEND_DLGFONT_CHKITALIC);
  ckbFontStyleItalic.OnClick := doControlChange;

  ckbFontStyleStrikeout := TCheckBox.Create(gpbFont);
  ckbFontStyleStrikeout.Parent := gpbFont;
  ckbFontStyleStrikeout.Top := t;
  PlaceControl(bd, nil, ckbFontStyleStrikeout, ppiFix(LEFT_3COL_2),
    ckbFontStyleStrikeout.Width);
  ckbFontStyleStrikeout.Caption := _rsrc(GIS_RS_LEGEND_DLGFONT_STRIKEOUT);
  ckbFontStyleStrikeout.OnClick := doControlChange;

  gpbFont.Height := t + ckbFontStyleStrikeout.Height + ppiFix(16);
end;

procedure T_panelLabel.initLabel;
var
  t: Integer;
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  gpbLabel := TGroupBox.Create(Self.Panel);
  gpbLabel.Parent := Self.Panel;
  gpbLabel.Top := gpbFont.Top + gpbFont.Height + ppiFix(8);
  gpbLabel.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbLabel, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbLabel.Anchors := [akRight, akTop]
  else
    gpbLabel.Anchors := [akLeft, akTop];
  gpbLabel.Caption := _rsrc(GIS_RS_LEGEND_TAB_LABEL);

  t := ppiFix(24);

  lblColor := TLabel.Create(gpbLabel);
  lblColor.Parent := gpbLabel;
  lblColor.Top := t;
  PlaceControl(bd, nil, lblColor, ppiFix(LEFT_3COL_1), lblColor.Width);
  lblColor.Caption := _rsrc(GIS_RS_LEGEND_PRM_COLOR);

  lblPattern := TLabel.Create(gpbLabel);
  lblPattern.Parent := gpbLabel;
  lblPattern.Top := t;
  PlaceControl(bd, nil, lblPattern, ppiFix(LEFT_3COL_2), lblPattern.Width);
  lblPattern.Caption := _rsrc(GIS_RS_LEGEND_PRM_PATTERN);

  lblShield := TLabel.Create(gpbLabel);
  lblShield.Parent := gpbLabel;
  lblShield.Top := t;
  PlaceControl(bd, nil, lblShield, ppiFix(LEFT_3COL_3), lblShield.Width);
  lblShield.Caption := _rsrc(GIS_RS_LEGEND_PRM_SHIELD);

  t := t + lblShield.Height + ppiFix(4);

  cmbColor := TGIS_ColorComboBox.Create(gpbLabel);
  cmbColor.Parent := gpbLabel;
  cmbColor.Top := t;
  cmbColor.Height := ppiFix(22);
  PlaceControl(bd, nil, cmbColor, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbColor.Fill(True, True);
  cmbColor.OnChange := doControlChange;
  cmbColor.CustomEvent := doCustomColor;

  cmbPattern := TGIS_PatternComboBox.Create(gpbLabel);
  cmbPattern.Parent := gpbLabel;
  cmbPattern.Top := t;
  PlaceControl(bd, nil, cmbPattern, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbPattern.Fill(True);
  cmbPattern.OnChange := doPatternChange;
  cmbPattern.CustomEvent := doCustomPattern;
  cmbPattern.GetBitmapEvent := doGetBitmapAreaFill;

  cmbShield := TGIS_ShieldComboBox.Create(gpbLabel);
  cmbShield.Parent := gpbLabel;
  cmbShield.Top := t;
  PlaceControl(bd, nil, cmbShield, ppiFix(LEFT_3COL_3), ppiFix(WIDTH_3COL));
  cmbShield.Fill;
  cmbShield.OnChange := doShieldChange;
  cmbShield.CustomEvent := doCustomShield;
  cmbShield.GetBitmapEvent := doGetBitmapShield;
  lblShield.FocusControl := cmbShield;

  t := t + cmbShield.Height + ppiFix(8);

  lblWidth := TLabel.Create(gpbLabel);
  lblWidth.Parent := gpbLabel;
  lblWidth.Top := t;
  PlaceControl(bd, nil, lblWidth, ppiFix(LEFT_3COL_1), lblWidth.Width);
  lblWidth.Caption := _rsrc(GIS_RS_LEGEND_PRM_WIDTH);

  lblHeight := TLabel.Create(gpbLabel);
  lblHeight.Parent := gpbLabel;
  lblHeight.Top := t;
  PlaceControl(bd, nil, lblHeight, ppiFix(LEFT_3COL_2), lblHeight.Width);
  lblHeight.Caption := _rsrc(GIS_RS_LEGEND_PRM_HEIGHT);

  lblSSSize := TLabel.Create(gpbLabel);
  lblSSSize.Parent := gpbLabel;
  lblSSSize.Top := t;
  PlaceControl(bd, nil, lblSSSize, ppiFix(LEFT_3COL_3), lblSSSize.Width);
  lblSSSize.Caption := _rsrc(GIS_RS_LEGEND_PRM_SMART_SIZE);

  t := t + lblWidth.Height + ppiFix(4);

  cmbWidth := TGIS_SizeComboBox.Create(gpbLabel);
  cmbWidth.Parent := gpbLabel;
  cmbWidth.Top := t;
  PlaceControl(bd, nil, cmbWidth, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbWidth.Fill(True, False, False, False);
  cmbWidth.OnChange := doControlChange;
  cmbWidth.CustomEvent := doCustomSize;

  cmbHeight := TGIS_SizeComboBox.Create(gpbLabel);
  cmbHeight.Parent := gpbLabel;
  cmbHeight.Top := t;
  PlaceControl(bd, nil, cmbHeight, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbHeight.Fill(True, False, False, False);
  cmbHeight.OnChange := doControlChange;
  cmbHeight.CustomEvent := doCustomSize;

  cmbSSSize := TGIS_SizeComboBox.Create(gpbLabel);
  cmbSSSize.Parent := gpbLabel;
  cmbSSSize.Top := t;
  PlaceControl(bd, nil, cmbSSSize, ppiFix(LEFT_3COL_3), ppiFix(WIDTH_3COL));
  cmbSSSize.Fill(False, False, True, False);
  cmbSSSize.OnChange := doControlChange;
  cmbSSSize.CustomEvent := doCustomSize;

  t := t + cmbHeight.Height + ppiFix(16);

  gpbLabel.Height := t;
end;

procedure T_panelLabel.initOutline;
var
  t: Integer;
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  gpbOutline := TGroupBox.Create(Self.Panel);
  gpbOutline.Parent := Self.Panel;
  gpbOutline.Top := gpbLabel.Top + gpbLabel.Height + ppiFix(4);
  gpbOutline.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbOutline, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbOutline.Anchors := [akRight, akTop]
  else
    gpbOutline.Anchors := [akLeft, akTop];
  gpbOutline.Caption := _rsrc(GIS_RS_LEGEND_TAB_OUTLINE);

  t := ppiFix(24);

  lblOStyle := TLabel.Create(gpbOutline);
  lblOStyle.Parent := gpbOutline;
  lblOStyle.Top := t;
  PlaceControl(bd, nil, lblOStyle, ppiFix(LEFT_3COL_1), lblOStyle.Width);
  lblOStyle.Caption := _rsrc(GIS_RS_LEGEND_PRM_STYLE);

  lblOColor := TLabel.Create(gpbOutline);
  lblOColor.Parent := gpbOutline;
  lblOColor.Top := t;
  PlaceControl(bd, nil, lblOColor, ppiFix(LEFT_3COL_2), lblOColor.Width);
  lblOColor.Caption := _rsrc(GIS_RS_LEGEND_PRM_COLOR);

  lblOPattern := TLabel.Create(gpbOutline);
  lblOPattern.Parent := gpbOutline;
  lblOPattern.Top := t;
  PlaceControl(bd, nil, lblOPattern, ppiFix(LEFT_3COL_3), lblOPattern.Width);
  lblOPattern.Caption := _rsrc(GIS_RS_LEGEND_PRM_PATTERN);

  t := t + lblOStyle.Height + ppiFix(4);

  cmbOStyle := TGIS_PenStyleComboBox.Create(gpbOutline);
  cmbOStyle.Parent := gpbOutline;
  cmbOStyle.Top := t;
  PlaceControl(bd, nil, cmbOStyle, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbOStyle.Fill(False);
  cmbOStyle.OnChange := doOPatternChange;
  cmbOStyle.CustomEvent := doCustomPattern;
  cmbOStyle.GetBitmapEvent := doGetBitmapAreaOutline;

  cmbOColor := TGIS_ColorComboBox.Create(gpbOutline);
  cmbOColor.Parent := gpbOutline;
  cmbOColor.Top := t;
  cmbOColor.Height := ppiFix(22);
  PlaceControl(bd, nil, cmbOColor, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbOColor.Fill(True, True);
  cmbOColor.OnChange := doControlChange;
  cmbOColor.CustomEvent := doCustomColor;

  cmbOPattern := TGIS_PatternComboBox.Create(gpbOutline);
  cmbOPattern.Parent := gpbOutline;
  cmbOPattern.Top := t;
  PlaceControl(bd, nil, cmbOPattern, ppiFix(LEFT_3COL_3), ppiFix(WIDTH_3COL));
  cmbOPattern.Fill(False);
  cmbOPattern.OnChange := doOPatternChange;
  cmbOPattern.CustomEvent := doCustomPattern;
  cmbOPattern.GetBitmapEvent := doGetBitmapAreaFill;

  t := t + cmbOPattern.Height + ppiFix(8);

  lblOWidth := TLabel.Create(gpbOutline);
  lblOWidth.Parent := gpbOutline;
  lblOWidth.Top := t;
  PlaceControl(bd, nil, lblOWidth, ppiFix(LEFT_3COL_1), lblOWidth.Width);
  lblOWidth.Caption := _rsrc(GIS_RS_LEGEND_PRM_WIDTH);

  t := t + lblOWidth.Height + ppiFix(4);

  cmbOWidth := TGIS_SizeComboBox.Create(gpbOutline);
  cmbOWidth.Parent := gpbOutline;
  cmbOWidth.Top := t;
  PlaceControl(bd, nil, cmbOWidth, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbOWidth.Fill(False, True, True, True);
  cmbOWidth.OnChange := doControlChange;
  cmbOWidth.CustomEvent := doCustomSize;

  t := t + cmbOWidth.Height + ppiFix(16);

  gpbOutline.Height := t;
end;

procedure T_panelLabel.initPosition;
var
  t: Integer;
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  gpbPosition := TGroupBox.Create(Self.Panel);
  gpbPosition.Parent := Self.Panel;
  gpbPosition.Top := gpbOutline.Top + gpbOutline.Height + ppiFix(4);
  gpbPosition.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbPosition, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbPosition.Anchors := [akRight, akTop]
  else
    gpbPosition.Anchors := [akLeft, akTop];
  gpbPosition.Caption := _rsrc(GIS_RS_LEGEND_TAB_POSITION);

  t := ppiFix(24);

  lblPAlignment := TLabel.Create(gpbPosition);
  lblPAlignment.Parent := gpbPosition;
  lblPAlignment.Top := t;
  PlaceControl(bd, nil, lblPAlignment, ppiFix(LEFT_3COL_1),
    lblPAlignment.Width);
  lblPAlignment.Caption := _rsrc(GIS_RS_LEGEND_PRM_ALIGNMENT);

  lblPRotation := TLabel.Create(gpbPosition);
  lblPRotation.Parent := gpbPosition;
  lblPRotation.Top := t;
  PlaceControl(bd, nil, lblPRotation, ppiFix(LEFT_3COL_2), lblPRotation.Width);
  lblPRotation.Caption := _rsrc(GIS_RS_LEGEND_PRM_LABELROTATE);

  lblPPosition := TLabel.Create(gpbPosition);
  lblPPosition.Parent := gpbPosition;
  lblPPosition.Top := t;
  PlaceControl(bd, nil, lblPPosition, ppiFix(LEFT_3COL_3), lblPPosition.Width);
  lblPPosition.Caption := _rsrc(GIS_RS_LEGEND_PRM_POSITION);

  t := t + lblPRotation.Height + ppiFix(4);

  cmbPAlignment := TComboBox.Create(gpbPosition);
  cmbPAlignment.Parent := gpbPosition;
  cmbPAlignment.Top := t;
  PlaceControl(bd, nil, cmbPAlignment, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbPAlignment.Style := csDropDownList;
  cmbPAlignment.Items.BeginUpdate;
  cmbPAlignment.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_SINGLELINE));
  cmbPAlignment.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_LEFTJUSTIFY));
  cmbPAlignment.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_CENTER));
  cmbPAlignment.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_RIGHTJUSTIFY));
  cmbPAlignment.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_FOLLOW));
  cmbPAlignment.ItemIndex := 1;
  cmbPAlignment.Items.EndUpdate;
  cmbPAlignment.OnChange := doAlignmentChange;

  cmbPRotation := TGIS_RotationComboBox.Create(gpbPosition);
  cmbPRotation.Parent := gpbPosition;
  cmbPRotation.Top := t;
  PlaceControl(bd, nil, cmbPRotation, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbPRotation.Fill(True);
  cmbPRotation.OnChange := doControlChange;
  cmbPRotation.CustomEvent := doCustomRotation;

  btnPTopLeft := T_speedButton.Create(gpbPosition);
  btnPTopLeft.Parent := gpbPosition;
  if bd = bdRightToLeft then
    btnPTopLeft.Left := ppiFix(LEFT_3COL_1) + ppiFix(WIDTH_3COL) - 3 *
      ppiFix(23)
  else
    btnPTopLeft.Left := ppiFix(LEFT_3COL_3);
  btnPTopLeft.Top := t;
  btnPTopLeft.Width := ppiFix(23);
  btnPTopLeft.Height := ppiFix(23);
  btnPTopLeft.OnClick := actBtnPositionClick;

  btnPTopCenter := T_speedButton.Create(gpbPosition);
  btnPTopCenter.Parent := gpbPosition;
  btnPTopCenter.Left := btnPTopLeft.Left + btnPTopLeft.Width;
  btnPTopCenter.Top := t;
  btnPTopCenter.Width := ppiFix(23);
  btnPTopCenter.Height := ppiFix(23);
  btnPTopCenter.OnClick := actBtnPositionClick;

  btnPTopRight := T_speedButton.Create(gpbPosition);
  btnPTopRight.Parent := gpbPosition;
  btnPTopRight.Left := btnPTopCenter.Left + btnPTopCenter.Width;
  btnPTopRight.Top := t;
  btnPTopRight.Width := ppiFix(23);
  btnPTopRight.Height := ppiFix(23);
  btnPTopRight.OnClick := actBtnPositionClick;

  fbnPPosition := TGIS_FieldButton.Create(gpbPosition);
  fbnPPosition.Parent := gpbPosition;
  if bd = bdRightToLeft then
    fbnPPosition.Left := btnPTopLeft.Left - fbnPPosition.Width
  else
    fbnPPosition.Left := btnPTopRight.Left + btnPTopRight.Width;
  fbnPPosition.Top := t;
  fbnPPosition.FieldsList.Assign(oParentWindow.MVC.FieldNamesEx);
  fbnPPosition.FieldsList.Insert(0, '');
  fbnPPosition.OnCommitClick := doPositionExNotify;

  t := t + btnPTopLeft.Height;

  ckbPFlow := TCheckBox.Create(gpbPosition);
  ckbPFlow.Parent := gpbPosition;
  ckbPFlow.Top := t;
  PlaceControl(bd, nil, ckbPFlow, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_2COL));
  ckbPFlow.Caption := _rsrc(GIS_RS_LEGEND_PRM_FLOW);

  btnPCenterLeft := T_speedButton.Create(gpbPosition);
  btnPCenterLeft.Parent := gpbPosition;
  btnPCenterLeft.Left := btnPTopLeft.Left;
  btnPCenterLeft.Top := t;
  btnPCenterLeft.Width := ppiFix(23);
  btnPCenterLeft.Height := ppiFix(23);
  btnPCenterLeft.OnClick := actBtnPositionClick;

  btnPCenterCenter := T_speedButton.Create(gpbPosition);
  btnPCenterCenter.Parent := gpbPosition;
  btnPCenterCenter.Left := btnPCenterLeft.Left + btnPCenterLeft.Width;
  btnPCenterCenter.Top := t;
  btnPCenterCenter.Width := ppiFix(23);
  btnPCenterCenter.Height := ppiFix(23);
  btnPCenterCenter.OnClick := actBtnPositionClick;

  btnPCenterRight := T_speedButton.Create(gpbPosition);
  btnPCenterRight.Parent := gpbPosition;
  btnPCenterRight.Left := btnPCenterCenter.Left + btnPCenterCenter.Width;
  btnPCenterRight.Top := t;
  btnPCenterRight.Width := ppiFix(23);
  btnPCenterRight.Height := ppiFix(23);
  btnPCenterRight.OnClick := actBtnPositionClick;

  t := t + btnPCenterLeft.Height;

  btnPBottomLeft := T_speedButton.Create(gpbPosition);
  btnPBottomLeft.Parent := gpbPosition;
  btnPBottomLeft.Left := btnPTopLeft.Left;
  btnPBottomLeft.Top := t;
  btnPBottomLeft.Width := ppiFix(23);
  btnPBottomLeft.Height := ppiFix(23);
  btnPBottomLeft.OnClick := actBtnPositionClick;

  btnPBottomCenter := T_speedButton.Create(gpbPosition);
  btnPBottomCenter.Parent := gpbPosition;
  btnPBottomCenter.Left := btnPBottomLeft.Left + btnPBottomLeft.Width;
  btnPBottomCenter.Top := t;
  btnPBottomCenter.Width := ppiFix(23);
  btnPBottomCenter.Height := ppiFix(23);
  btnPBottomCenter.OnClick := actBtnPositionClick;

  btnPBottomRight := T_speedButton.Create(gpbPosition);
  btnPBottomRight.Parent := gpbPosition;
  btnPBottomRight.Left := btnPBottomCenter.Left + btnPBottomCenter.Width;
  btnPBottomRight.Top := t;
  btnPBottomRight.Width := ppiFix(23);
  btnPBottomRight.Height := ppiFix(23);
  btnPBottomRight.OnClick := actBtnPositionClick;

  fldPPosition := T_fieldPanel.Create(gpbPosition);
  fldPPosition.Parent := gpbPosition;
  fldPPosition.Left := btnPTopLeft.Left;
  fldPPosition.Top := btnPTopLeft.Top;
  fldPPosition.Width := btnPBottomRight.Left + btnPBottomRight.Width -
    btnPTopLeft.Left;
  fldPPosition.Height := btnPBottomRight.Top + btnPBottomRight.Height -
    btnPTopLeft.Top;
  fldPPosition.Visible := False;

  gpbPosition.Height := t + btnPBottomRight.Height + ppiFix(16);

end;

procedure T_panelLabel.Read;
var
  s1, s2, s3: String;
begin
  lockUpdates;
  try
    cmbValue.Value := MVC.ParamsLabel.Value; // use Value
    ckbVisible.Checked := MVC.ParamsLabel.Visible;
    ckbLegend.Checked := MVC.ParamsLabel.ShowLegend;
    ckbPAvoidOverlapping.Checked := MVC.ParamsLabel.Allocator;
    ckbPAvoidDuplicates.Checked := not MVC.ParamsLabel.Duplicates;

    // Font
    cmbFontName.Value := MVC.ParamsLabel.FontName;
    cmbFontSize.Value := MVC.ParamsLabel.FontSizeAsText;
    cmbFontColor.Value := MVC.ParamsLabel.FontColorAsText;

    ckbFontStyleBold.Checked := TGIS_FontStyle.Bold
      in MVC.ParamsLabel.FontStyle;
    ckbFontStyleItalic.Checked := TGIS_FontStyle.Italic
      in MVC.ParamsLabel.FontStyle;
    ckbFontStyleUnderline.Checked := TGIS_FontStyle.Underline
      in MVC.ParamsLabel.FontStyle;
    ckbFontStyleStrikeout.Checked := TGIS_FontStyle.StrikeOut
      in MVC.ParamsLabel.FontStyle;

    ckbFontStyleShadow.Checked :=
      MVC.ParamsLabel.Pattern = TGIS_BrushStyle.Clear;

    // Label
    cmbColor.Value := MVC.ParamsLabel.ColorAsText;
    cmbPattern.Value := MVC.ParamsLabel.PatternAsText;
    cmbShield.Value := MVC.ParamsLabel.ShieldAsText;

    cmbWidth.Value := MVC.ParamsLabel.WidthAsText;
    cmbHeight.Value := MVC.ParamsLabel.HeightAsText;

    // Outline
    cmbOStyle.Value := MVC.ParamsLabel.OutlineStyleAsText;
    cmbOWidth.Value := MVC.ParamsLabel.OutlineWidthAsText;
    cmbOColor.Value := MVC.ParamsLabel.OutlineColorAsText;
    cmbOPattern.Value := MVC.ParamsLabel.OutlinePatternAsText;

    // Smart size
    cmbSSSize.Value := MVC.ParamsLabel.SmartSizeAsText;

    // Position
    cmbPAlignment.Items.BeginUpdate;
    case MVC.ParamsLabel.Alignment of
      TGIS_LabelAlignment.Single:
        cmbPAlignment.ItemIndex := 0;
      TGIS_LabelAlignment.LeftJustify:
        cmbPAlignment.ItemIndex := 1;
      TGIS_LabelAlignment.Center:
        cmbPAlignment.ItemIndex := 2;
      TGIS_LabelAlignment.RightJustify:
        cmbPAlignment.ItemIndex := 3;
      TGIS_LabelAlignment.Follow:
        cmbPAlignment.ItemIndex := 4;
    end;
    cmbPAlignment.Items.EndUpdate;

    cmbPRotation.Value := MVC.ParamsLabel.RotateAsText;
    ckbPFlow.Checked := TGIS_LabelPosition.Flow in MVC.ParamsLabel.Position;

    if TGIS_LabelPosition.UpLeft in MVC.ParamsLabel.Position then
      btnPTopLeft.Caption := 'X';
    if TGIS_LabelPosition.UpCenter in MVC.ParamsLabel.Position then
      btnPTopCenter.Caption := 'X';
    if TGIS_LabelPosition.UpRight in MVC.ParamsLabel.Position then
      btnPTopRight.Caption := 'X';
    if TGIS_LabelPosition.MiddleLeft in MVC.ParamsLabel.Position then
      btnPCenterLeft.Caption := 'X';
    if TGIS_LabelPosition.MiddleCenter in MVC.ParamsLabel.Position then
      btnPCenterCenter.Caption := 'X';
    if TGIS_LabelPosition.MiddleRight in MVC.ParamsLabel.Position then
      btnPCenterRight.Caption := 'X';
    if TGIS_LabelPosition.DownLeft in MVC.ParamsLabel.Position then
      btnPBottomLeft.Caption := 'X';
    if TGIS_LabelPosition.DownCenter in MVC.ParamsLabel.Position then
      btnPBottomCenter.Caption := 'X';
    if TGIS_LabelPosition.DownRight in MVC.ParamsLabel.Position then
      btnPBottomRight.Caption := 'X';

    SplitParamAsText(MVC.ParamsLabel.PositionAsText, s1, s2, s3);

    if s1 = GIS_PARAMTXT_TYPE_FIELD then
    begin
      fldPPosition.Field := s2;
      fbnPPosition.Field := s2;
    end;

    MVC.DoUpdateBitmap;
    MVC.DoUpdateOBitmap;
    MVC.doFieldChange;
  finally
    unlockUpdates;
  end;

end;

procedure T_panelLabel.Write;
var
  lps: TGIS_LabelPositions;
  fs: TGIS_FontStyles;
begin
  MVC.ParamsLabel.Value := cmbValue.Value;
  MVC.ParamsLabel.Visible := ckbVisible.Checked;
  MVC.ParamsLabel.ShowLegend := ckbLegend.Checked;
  MVC.ParamsLabel.Allocator := ckbPAvoidOverlapping.Checked;
  MVC.ParamsLabel.Duplicates := not ckbPAvoidDuplicates.Checked;

  MVC.ParamsLabel.FontColorAsText := cmbFontColor.Value;
  MVC.ParamsLabel.FontName := cmbFontName.Value;
  MVC.ParamsLabel.FontSizeAsText := cmbFontSize.Value;

  fs := [];
  if ckbFontStyleBold.Checked then
    fs := fs + [TGIS_FontStyle.Bold];
  if ckbFontStyleItalic.Checked then
    fs := fs + [TGIS_FontStyle.Italic];
  if ckbFontStyleUnderline.Checked then
    fs := fs + [TGIS_FontStyle.Underline];
  if ckbFontStyleStrikeout.Checked then
    fs := fs + [TGIS_FontStyle.StrikeOut];

  MVC.ParamsLabel.FontStyle := fs;

  // Label
  MVC.ParamsLabel.ColorAsText := cmbColor.Value;
  MVC.ParamsLabel.PatternAsText := cmbPattern.Value;
  MVC.ParamsLabel.ShieldAsText := cmbShield.Value;
  MVC.ParamsLabel.WidthAsText := cmbWidth.Value;
  MVC.ParamsLabel.HeightAsText := cmbHeight.Value;

  // Outline
  MVC.ParamsLabel.OutlineStyleAsText := cmbOStyle.Value;
  MVC.ParamsLabel.OutlineWidthAsText := cmbOWidth.Value;
  MVC.ParamsLabel.OutlineColorAsText := cmbOColor.Value;
  MVC.ParamsLabel.OutlinePatternAsText := cmbOPattern.Value;

  // Smart size
  MVC.ParamsLabel.SmartSizeAsText := cmbSSSize.Value;

  // Position
  lps := [];
  if btnPTopLeft.Caption = 'X' then
    lps := lps + [TGIS_LabelPosition.UpLeft];
  if btnPTopCenter.Caption = 'X' then
    lps := lps + [TGIS_LabelPosition.UpCenter];
  if btnPTopRight.Caption = 'X' then
    lps := lps + [TGIS_LabelPosition.UpRight];
  if btnPCenterLeft.Caption = 'X' then
    lps := lps + [TGIS_LabelPosition.MiddleLeft];
  if btnPCenterCenter.Caption = 'X' then
    lps := lps + [TGIS_LabelPosition.MiddleCenter];
  if btnPCenterRight.Caption = 'X' then
    lps := lps + [TGIS_LabelPosition.MiddleRight];
  if btnPBottomLeft.Caption = 'X' then
    lps := lps + [TGIS_LabelPosition.DownLeft];
  if btnPBottomCenter.Caption = 'X' then
    lps := lps + [TGIS_LabelPosition.DownCenter];
  if btnPBottomRight.Caption = 'X' then
    lps := lps + [TGIS_LabelPosition.DownRight];
  if ckbPFlow.Checked then
    lps := lps + [TGIS_LabelPosition.Flow];

  if not IsStringEmpty(fbnPPosition.Field) then
    MVC.ParamsLabel.PositionAsText :=
      ConstructParamAsText(GIS_PARAMTXT_TYPE_FIELD, fbnPPosition.Field, '')
  else
    MVC.ParamsLabel.PositionAsText :=
      ConstructParamAsText(GIS_PARAMTXT_TYPE_STOCK,
      ConstructParamPosition(lps), '');

  case cmbPAlignment.ItemIndex of
    0:
      MVC.ParamsLabel.Alignment := TGIS_LabelAlignment.Single;
    1:
      MVC.ParamsLabel.Alignment := TGIS_LabelAlignment.LeftJustify;
    2:
      MVC.ParamsLabel.Alignment := TGIS_LabelAlignment.Center;
    3:
      MVC.ParamsLabel.Alignment := TGIS_LabelAlignment.RightJustify;
    4:
      MVC.ParamsLabel.Alignment := TGIS_LabelAlignment.Follow;
  end;

  MVC.ParamsLabel.RotateAsText := cmbPRotation.Value;
end;

procedure T_panelLabel.PreparePreview(const _viewer: IGIS_Viewer);
begin
  MVC.PreparePreview(_viewer);
end;

procedure T_panelLabel.UpdatePreview;
var
  ll: TGIS_Layer;
  paramsvec: TGIS_ParamsSectionVector;
begin
  oParentWindow.tmrUpdate.Enabled := False;
  if oParentWindow.gisPreview.IsEmpty then
    exit;
  oParentWindow.tmrUpdate.Enabled := True;

  ll := TGIS_Layer(oParentWindow.gisPreview.Items[0]);

  paramsvec := TGIS_ParamsSectionVector(ll.Params);

  MVC.SectionWrite(paramsvec);

  paramsvec.Marker.Size := 0;

  paramsvec.Labels.Position := [TGIS_LabelPosition.MiddleCenter];
  paramsvec.Labels.Allocator := False;

  paramsvec.Labels.Value := paramsvec.Labels.Value;
  if (not IsStringEmpty(paramsvec.Labels.Value)) or
    (not IsStringEmpty(paramsvec.Labels.Field)) then
  begin
    if assigned(paramsvec.Labels.Shield) then
      paramsvec.Labels.Value := '66'
    else
      paramsvec.Labels.Value := 'Lorem<BR>ipsum dolor';
  end;

  if paramsvec.Labels.Alignment = TGIS_LabelAlignment.Follow then
    paramsvec.Labels.Rotate := DegToRad(45);

  paramsvec.Render.Chart := '';
  paramsvec.Chart.Values := '';
  paramsvec.Query := '';

  with TGIS_ParamsSection(ll.Params) do
  begin
    MinZoom := 0;
    MaxZoom := GIS_MAX_DOUBLE;
    MinScale := 0;
    MaxScale := GIS_MAX_DOUBLE;
  end;

  // avoid unreasonable preview
  paramsvec.Labels.FontColor := normalize_color(paramsvec.Labels.FontColor,
    paramsvec.Labels.FontColorAsText);
  paramsvec.Labels.Color := normalize_color(paramsvec.Labels.Color,
    paramsvec.Labels.ColorAsText);
  paramsvec.Labels.OutlineColor :=
    normalize_color(paramsvec.Labels.OutlineColor,
    paramsvec.Labels.OutlineColorAsText);
  paramsvec.Labels.OutlineWidth := normalize_size(oParentWindow.gisPreview,
    paramsvec.Labels.OutlineWidth, paramsvec.Labels.OutlineWidthAsText,
    MAX_PREVIEW_SIZE_OUTLINE);
  paramsvec.Labels.FontSize := normalize_size(oParentWindow.gisPreview,
    paramsvec.Labels.FontSize, paramsvec.Labels.FontSizeAsText,
    MAX_PREVIEW_SIZE_FONT);
  paramsvec.Labels.Rotate := normalize_angle(paramsvec.Labels.Rotate,
    paramsvec.Labels.RotateAsText);
end;

procedure T_panelLabel.actBtnPositionClick(_sender: TObject);
var
  btn: T_speedButton;
begin
  btn := T_speedButton(_sender);

  if length(btn.Caption) = 0 then
    btn.Caption := 'X'
  else
    btn.Caption := '';
end;

procedure T_panelLabel.doCallback(_sender: TObject; _code: Integer);

  procedure call_dependant_events;
  begin
    MVC.doSmartSizeFieldChange;
    MVC.doPositionExNotify;
    MVC.doAlignmentChange;
  end;

  procedure do_field_change;
  var
    b: Boolean;
  begin
    b := length(cmbValue.Value) <> 0;

    ckbVisible.Enabled := b;
    ckbLegend.Enabled := b;
    ckbPAvoidOverlapping.Enabled := b;
    ckbPAvoidDuplicates.Enabled := b;

    local_SetEnabledOnControl(gpbFont, b);
    local_SetEnabledOnControl(gpbLabel, b);
    local_SetEnabledOnControl(gpbOutline, b);
    local_SetEnabledOnControl(gpbPosition, b);

    call_dependant_events;
  end;

  procedure do_ssfield_change;
  begin

  end;

  procedure do_posex;
  begin
    if IsStringEmpty(fbnPPosition.Field) then
    begin
      fldPPosition.Visible := False;
      btnPTopLeft.Visible := True;
      btnPTopCenter.Visible := True;
      btnPTopRight.Visible := True;
      btnPCenterLeft.Visible := True;
      btnPCenterCenter.Visible := True;
      btnPCenterRight.Visible := True;
      btnPBottomLeft.Visible := True;
      btnPBottomCenter.Visible := True;
      btnPBottomRight.Visible := True;
    end
    else
    begin
      btnPTopLeft.Visible := False;
      btnPTopCenter.Visible := False;
      btnPTopRight.Visible := False;
      btnPCenterLeft.Visible := False;
      btnPCenterCenter.Visible := False;
      btnPCenterRight.Visible := False;
      btnPBottomLeft.Visible := False;
      btnPBottomCenter.Visible := False;
      btnPBottomRight.Visible := False;
      fldPPosition.Field := 'Field:' + fbnPPosition.Field;
      fldPPosition.Visible := True;
    end;
  end;

  procedure do_alignment_change;
  var
    b: Boolean;
  begin
    if not gpbPosition.Enabled then
      exit;

    b := cmbPAlignment.ItemIndex <> 4;

    fbnPPosition.Enabled := b;
    btnPTopLeft.Enabled := b;
    btnPTopCenter.Enabled := b;
    btnPTopRight.Enabled := b;
    btnPCenterLeft.Enabled := b;
    btnPCenterRight.Enabled := b;
    btnPBottomLeft.Enabled := b;
    btnPBottomCenter.Enabled := b;
    btnPBottomRight.Enabled := b;
  end;

  procedure do_update_bitmap;
  var
    bbmp: Boolean;
  begin
    bbmp := not TGIS_Bitmap.IsNilOrEmpty(MVC.ParamsLabel.Bitmap);

    if bbmp then
    begin
      lblColor.Enabled := False;
      cmbColor.Enabled := False;
    end
    else
    begin
      lblColor.Enabled := True;
      cmbColor.Enabled := True;
    end;
  end;

  procedure do_update_obitmap;
  var
    bbmp: Boolean;
  begin
    bbmp := not TGIS_Bitmap.IsNilOrEmpty(MVC.ParamsLabel.OutlineBitmap);

    if bbmp then
    begin
      lblOColor.Enabled := False;
      cmbOColor.Enabled := False;
      lblOPattern.Enabled := False;
      cmbOPattern.Enabled := False;
    end
    else
    begin
      lblOColor.Enabled := True;
      cmbOColor.Enabled := True;
      lblOPattern.Enabled := True;
      cmbOPattern.Enabled := True;
    end;
  end;

  procedure do_shadow_change;
  begin
    if ckbFontStyleShadow.Checked then
      cmbPattern.Value := ConstructParamAsText(GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_PATTERN_TRANSPARENT, '')
    else
      cmbPattern.Value := ConstructParamAsText(GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_PATTERN_SOLID, '');

    doControlChange(cmbPattern);
  end;

  procedure do_shield_change;
  begin
    doControlChange(cmbShield);
  end;

begin

  if not blockedUpdates then
  begin
    Write;
    UpdatePreview;
  end;

  case _code of
    10:
      do_field_change;
    70:
      do_ssfield_change;
    80:
      do_posex;
    82:
      do_alignment_change;
    100:
      do_update_bitmap;
    120:
      do_update_obitmap;
    150:
      do_update_obitmap;
    160:
      do_update_bitmap;
    170:
      do_shadow_change;
    190:
      do_shield_change;
  end;

end;

procedure T_panelLabel.doFieldChange(_sender: TObject);
begin
  MVC.doFieldChange;
end;

procedure T_panelLabel.doSmartSizeFieldChange(_sender: TObject);
begin
  MVC.doSmartSizeFieldChange;
end;

procedure T_panelLabel.doPositionExNotify(_sender: TObject);
begin
  MVC.doPositionExNotify;
end;

procedure T_panelLabel.doAlignmentChange(_sender: TObject);
begin
  MVC.doAlignmentChange;
end;

procedure T_panelLabel.doControlChange(_sender: TObject);
begin
  MVC.doControlChange;
end;

procedure T_panelLabel.doOPatternChange(_sender: TObject);
begin
  MVC.doOPatternChange;
end;

procedure T_panelLabel.doPatternChange(_sender: TObject);
begin
  MVC.doPatternChange;
end;

procedure T_panelLabel.doShieldChange(_sender: TObject);
begin
  MVC.doShieldChange;
end;

procedure T_panelLabel.doShadowChange(_sender: TObject);
begin
  MVC.doShadowChange;
end;
{$ENDREGION}
{$REGION 'T_panelChart'}

function T_panelChart.fget_HasPreview: Boolean;
begin
  Result := MVC.HasPreview;
end;

procedure T_panelChart.init;
begin
  inherited;

  ItemText := _rsrc(GIS_RS_LEGEND_PAG_CHART);

  MVC := oParentWindow.MVC.Chart;
  MVC.Callback := doCallback;

  initChart;
  initValues;
  initSelf;
end;

procedure T_panelChart.initSelf;
var
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  ckbLegend := TCheckBox.Create(Self.Panel);
  ckbLegend.Parent := Self.Panel;
  ckbLegend.Top := gpbValues.Top + gpbValues.Height + ppiFix(8);
  PlaceControl(bd, nil, ckbLegend, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_2COL));
  if bd = bdRightToLeft then
    ckbLegend.Anchors := [akRight, akTop]
  else
    ckbLegend.Anchors := [akLeft, akTop];
  ckbLegend.Caption := _rsrc(GIS_RS_LEGEND_PRM_INCLUDEINLEGEND);
end;

procedure T_panelChart.initChart;
var
  t: Integer;
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  gpbChart := TGroupBox.Create(Self.Panel);
  gpbChart.Parent := Self.Panel;
  gpbChart.Top := ppiFix(8);
  gpbChart.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbChart, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbChart.Anchors := [akRight, akTop]
  else
    gpbChart.Anchors := [akLeft, akTop];
  gpbChart.Caption := _rsrc(GIS_RS_LEGEND_TAB_CHART);

  t := ppiFix(24);

  lblStyle := TLabel.Create(gpbChart);
  lblStyle.Parent := gpbChart;
  lblStyle.Top := t;
  PlaceControl(bd, nil, lblStyle, ppiFix(LEFT_3COL_1), lblStyle.Width);
  lblStyle.Caption := _rsrc(GIS_RS_LEGEND_PRM_STYLE);

  lblMinVal := TLabel.Create(gpbChart);
  lblMinVal.Parent := gpbChart;
  lblMinVal.Top := t;
  PlaceControl(bd, nil, lblMinVal, ppiFix(LEFT_3COL_2), lblMinVal.Width);
  lblMinVal.Caption := _rsrc(GIS_RS_LEGEND_PRM_MINIMUM);

  lblMaxVal := TLabel.Create(gpbChart);
  lblMaxVal.Parent := gpbChart;
  lblMaxVal.Top := t;
  PlaceControl(bd, nil, lblMaxVal, ppiFix(LEFT_3COL_3), lblMaxVal.Width);
  lblMaxVal.Caption := _rsrc(GIS_RS_LEGEND_PRM_MAXIMUM);

  t := t + lblStyle.Height + ppiFix(4);

  cmbStyle := TComboBox.Create(gpbChart);
  cmbStyle.Parent := gpbChart;
  cmbStyle.Top := t;
  PlaceControl(bd, nil, cmbStyle, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbStyle.Style := csDropDownList;
  cmbStyle.Items.BeginUpdate;
  cmbStyle.Items.Add(_rsrc(GIS_RS_LEGEND_PRM_CHART_PIE));
  cmbStyle.Items.Add(_rsrc(GIS_RS_LEGEND_PRM_CHART_BAR));
  cmbStyle.ItemIndex := 0;
  cmbStyle.Items.EndUpdate;
  cmbStyle.OnChange := doStyleChange;

  edtMinVal := TEdit.Create(gpbChart);
  edtMinVal.Parent := gpbChart;
  edtMinVal.Top := t;
  PlaceControl(bd, nil, edtMinVal, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));

  edtMaxVal := TEdit.Create(gpbChart);
  edtMaxVal.Parent := gpbChart;
  edtMaxVal.Top := t;
  PlaceControl(bd, nil, edtMaxVal, ppiFix(LEFT_3COL_3), ppiFix(WIDTH_3COL));

  t := t + cmbStyle.Height + ppiFix(8);

  lblSize := TLabel.Create(gpbChart);
  lblSize.Parent := gpbChart;
  lblSize.Top := t;
  PlaceControl(bd, nil, lblSize, ppiFix(LEFT_3COL_1), lblSize.Width);
  lblSize.Caption := _rsrc(GIS_RS_LEGEND_PRM_SIZE);

  t := t + lblMinVal.Height + ppiFix(4);

  cmbSize := TGIS_SizeComboBox.Create(gpbChart);
  cmbSize.Parent := gpbChart;
  cmbSize.Top := t;
  PlaceControl(bd, nil, cmbSize, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbSize.Fill(True, False, False, True);
  cmbSize.OnChange := doControlChange;
  cmbSize.CustomEvent := doCustomSize;

  t := t + edtMinVal.Height + ppiFix(16);

  gpbChart.Height := t;
end;

procedure T_panelChart.initValues;
var
  t: Integer;
  w: Integer;
  bd: TBiDiMode;
begin
  bd := oParentWindow.BiDiMode;

  gpbValues := TGroupBox.Create(Self.Panel);
  gpbValues.Parent := Self.Panel;
  gpbValues.Top := gpbChart.Top + gpbChart.Height + ppiFix(4);
  gpbValues.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbValues, ppiFix(8), ppiFix(WIDTH_NORMAL));
  if bd = bdRightToLeft then
    gpbValues.Anchors := [akRight, akTop]
  else
    gpbValues.Anchors := [akLeft, akTop];
  gpbValues.Caption := _rsrc(GIS_RS_LEGEND_PRM_VALUES);

  t := ppiFix(24);

  lblValue := TLabel.Create(gpbValues);
  lblValue.Parent := gpbValues;
  lblValue.Top := t;
  PlaceControl(bd, nil, lblValue, 2 * ppiFix(LEFT_3COL_1), lblValue.Width);
  lblValue.Caption := _rsrc(GIS_RS_LEGEND_PRM_VALUES);

  lblLegend := TLabel.Create(gpbValues);
  lblLegend.Parent := gpbValues;
  lblLegend.Top := t;
  PlaceControl(bd, nil, lblLegend, ppiFix(LEFT_2COL_1) + ppiFix(20),
    lblLegend.Width);
  lblLegend.Caption := _rsrc(GIS_RS_LEGEND_PRM_LEGENDS);

  t := t + lblValue.Height + ppiFix(4);

  lblVal1 := TLabel.Create(gpbValues);
  lblVal1.Parent := gpbValues;
  lblVal1.Top := t + ppiFix(3);
  PlaceControl(bd, nil, lblVal1, ppiFix(LEFT_3COL_1), lblVal1.Width);
  lblVal1.Caption := '1';

  w := ppiFix(WIDTH_2COL) - ppiFix(LEFT_3COL_1);

  cmbVal1 := TComboBox.Create(gpbValues);
  cmbVal1.Parent := gpbValues;
  cmbVal1.Top := t;
  PlaceControl(bd, nil, cmbVal1, 2 * ppiFix(LEFT_3COL_1), w);
  cmbVal1.ItemIndex := -1;
  cmbVal1.OnChange := doValue1Change;
  oParentWindow.fillComboBoxWithFields(cmbVal1);

  edtVal1 := TEdit.Create(gpbValues);
  edtVal1.Parent := gpbValues;
  edtVal1.Top := t;
  PlaceControl(bd, cmbVal1, edtVal1, ppiFix(4), ppiFix(WIDTH_3COL) +
    ppiFix(LEFT_3COL_1));
  edtVal1.OnChange := doLegend1Change;

  pnlVal1 := TGIS_ColorPreview.Create(gpbValues);
  pnlVal1.Parent := gpbValues;
  pnlVal1.Top := t;
  pnlVal1.Height := edtVal1.Height;
  PlaceControl(bd, edtVal1, pnlVal1, ppiFix(4), edtVal1.Height);
  pnlVal1.Color := TGIS_Color.Red;

  t := t + cmbVal1.Height + ppiFix(4);

  lblVal2 := TLabel.Create(gpbValues);
  lblVal2.Parent := gpbValues;
  lblVal2.Top := t + ppiFix(3);
  PlaceControl(bd, nil, lblVal2, ppiFix(LEFT_3COL_1), lblVal2.Width);
  lblVal2.Caption := '2';

  cmbVal2 := TComboBox.Create(gpbValues);
  cmbVal2.Parent := gpbValues;
  cmbVal2.Top := t;
  PlaceControl(bd, nil, cmbVal2, 2 * ppiFix(LEFT_3COL_1), w);
  cmbVal2.ItemIndex := -1;
  cmbVal2.OnChange := doValue2Change;
  oParentWindow.fillComboBoxWithFields(cmbVal2);

  edtVal2 := TEdit.Create(gpbValues);
  edtVal2.Parent := gpbValues;
  edtVal2.Top := t;
  PlaceControl(bd, cmbVal2, edtVal2, ppiFix(4), ppiFix(WIDTH_3COL) +
    ppiFix(LEFT_3COL_1));
  edtVal2.OnChange := doLegend2Change;

  pnlVal2 := TGIS_ColorPreview.Create(gpbValues);
  pnlVal2.Parent := gpbValues;
  pnlVal2.Top := t;
  pnlVal2.Height := edtVal2.Height;
  PlaceControl(bd, edtVal2, pnlVal2, ppiFix(4), edtVal2.Height);
  pnlVal2.Color := TGIS_Color.Lime;

  t := t + cmbVal2.Height + ppiFix(4);

  lblVal3 := TLabel.Create(gpbValues);
  lblVal3.Parent := gpbValues;
  lblVal3.Top := t + ppiFix(3);
  PlaceControl(bd, nil, lblVal3, ppiFix(LEFT_3COL_1), lblVal3.Width);
  lblVal3.Caption := '3';

  cmbVal3 := TComboBox.Create(gpbValues);
  cmbVal3.Parent := gpbValues;
  cmbVal3.Top := t;
  PlaceControl(bd, nil, cmbVal3, 2 * ppiFix(LEFT_3COL_1), w);
  cmbVal3.ItemIndex := -1;
  cmbVal3.OnChange := doValue3Change;
  oParentWindow.fillComboBoxWithFields(cmbVal3);

  edtVal3 := TEdit.Create(gpbValues);
  edtVal3.Parent := gpbValues;
  edtVal3.Top := t;
  PlaceControl(bd, cmbVal3, edtVal3, ppiFix(4), ppiFix(WIDTH_3COL) +
    ppiFix(LEFT_3COL_1));
  edtVal3.OnChange := doLegend3Change;

  pnlVal3 := TGIS_ColorPreview.Create(gpbValues);
  pnlVal3.Parent := gpbValues;
  pnlVal3.Top := t;
  pnlVal3.Height := edtVal3.Height;
  PlaceControl(bd, edtVal3, pnlVal3, ppiFix(4), edtVal3.Height);
  pnlVal3.Color := TGIS_Color.Blue;

  t := t + cmbVal3.Height + ppiFix(4);

  lblVal4 := TLabel.Create(gpbValues);
  lblVal4.Parent := gpbValues;
  lblVal4.Top := t + ppiFix(3);
  PlaceControl(bd, nil, lblVal4, ppiFix(LEFT_3COL_1), lblVal4.Width);
  lblVal4.Caption := '4';

  cmbVal4 := TComboBox.Create(gpbValues);
  cmbVal4.Parent := gpbValues;
  cmbVal4.Top := t;
  PlaceControl(bd, nil, cmbVal4, 2 * ppiFix(LEFT_3COL_1), w);
  cmbVal4.ItemIndex := -1;
  cmbVal4.OnChange := doValue4Change;
  oParentWindow.fillComboBoxWithFields(cmbVal4);

  edtVal4 := TEdit.Create(gpbValues);
  edtVal4.Parent := gpbValues;
  edtVal4.Top := t;
  PlaceControl(bd, cmbVal4, edtVal4, ppiFix(4), ppiFix(WIDTH_3COL) +
    ppiFix(LEFT_3COL_1));
  edtVal4.OnChange := doLegend4Change;

  pnlVal4 := TGIS_ColorPreview.Create(gpbValues);
  pnlVal4.Parent := gpbValues;
  pnlVal4.Top := t;
  pnlVal4.Height := edtVal4.Height;
  PlaceControl(bd, edtVal4, pnlVal4, ppiFix(4), edtVal4.Height);
  pnlVal4.Color := TGIS_Color.Fuchsia;

  t := t + cmbVal4.Height + ppiFix(4);

  lblVal5 := TLabel.Create(gpbValues);
  lblVal5.Parent := gpbValues;
  lblVal5.Top := t + ppiFix(3);
  PlaceControl(bd, nil, lblVal5, ppiFix(LEFT_3COL_1), lblVal5.Width);
  lblVal5.Caption := '5';

  cmbVal5 := TComboBox.Create(gpbValues);
  cmbVal5.Parent := gpbValues;
  cmbVal5.Top := t;
  PlaceControl(bd, nil, cmbVal5, 2 * ppiFix(LEFT_3COL_1), w);
  cmbVal5.ItemIndex := -1;
  cmbVal5.OnChange := doValue5Change;
  oParentWindow.fillComboBoxWithFields(cmbVal5);

  edtVal5 := TEdit.Create(gpbValues);
  edtVal5.Parent := gpbValues;
  edtVal5.Top := t;
  PlaceControl(bd, cmbVal5, edtVal5, ppiFix(4), ppiFix(WIDTH_3COL) +
    ppiFix(LEFT_3COL_1));
  edtVal5.OnChange := doLegend5Change;

  pnlVal5 := TGIS_ColorPreview.Create(gpbValues);
  pnlVal5.Parent := gpbValues;
  pnlVal5.Top := t;
  pnlVal5.Height := edtVal5.Height;
  PlaceControl(bd, edtVal5, pnlVal5, ppiFix(4), edtVal5.Height);
  pnlVal5.Color := TGIS_Color.Aqua;

  t := t + cmbVal5.Height + ppiFix(4);

  lblVal6 := TLabel.Create(gpbValues);
  lblVal6.Parent := gpbValues;
  lblVal6.Top := t + ppiFix(3);
  PlaceControl(bd, nil, lblVal6, ppiFix(LEFT_3COL_1), lblVal6.Width);
  lblVal6.Caption := '6';

  cmbVal6 := TComboBox.Create(gpbValues);
  cmbVal6.Parent := gpbValues;
  cmbVal6.Top := t;
  PlaceControl(bd, nil, cmbVal6, 2 * ppiFix(LEFT_3COL_1), w);
  cmbVal6.ItemIndex := -1;
  cmbVal6.OnChange := doValue6Change;
  oParentWindow.fillComboBoxWithFields(cmbVal6);

  edtVal6 := TEdit.Create(gpbValues);
  edtVal6.Parent := gpbValues;
  edtVal6.Top := t;
  PlaceControl(bd, cmbVal6, edtVal6, ppiFix(4), ppiFix(WIDTH_3COL) +
    ppiFix(LEFT_3COL_1));
  edtVal6.OnChange := doLegend6Change;

  pnlVal6 := TGIS_ColorPreview.Create(gpbValues);
  pnlVal6.Parent := gpbValues;
  pnlVal6.Top := t;
  pnlVal6.Height := edtVal6.Height;
  PlaceControl(bd, edtVal6, pnlVal6, ppiFix(4), edtVal6.Height);
  pnlVal6.Color := TGIS_Color.Green;

  t := t + cmbVal6.Height + ppiFix(4);

  lblVal7 := TLabel.Create(gpbValues);
  lblVal7.Parent := gpbValues;
  lblVal7.Top := t + ppiFix(3);
  PlaceControl(bd, nil, lblVal7, ppiFix(LEFT_3COL_1), lblVal7.Width);
  lblVal7.Caption := '7';

  cmbVal7 := TComboBox.Create(gpbValues);
  cmbVal7.Parent := gpbValues;
  cmbVal7.Top := t;
  PlaceControl(bd, nil, cmbVal7, 2 * ppiFix(LEFT_3COL_1), w);
  cmbVal7.ItemIndex := -1;
  cmbVal7.OnChange := doValue7Change;
  oParentWindow.fillComboBoxWithFields(cmbVal7);

  edtVal7 := TEdit.Create(gpbValues);
  edtVal7.Parent := gpbValues;
  edtVal7.Top := t;
  PlaceControl(bd, cmbVal7, edtVal7, ppiFix(4), ppiFix(WIDTH_3COL) +
    ppiFix(LEFT_3COL_1));
  edtVal7.OnChange := doLegend7Change;

  pnlVal7 := TGIS_ColorPreview.Create(gpbValues);
  pnlVal7.Parent := gpbValues;
  pnlVal7.Top := t;
  pnlVal7.Height := edtVal7.Height;
  PlaceControl(bd, edtVal7, pnlVal7, ppiFix(4), edtVal7.Height);
  pnlVal7.Color := TGIS_Color.White;

  t := t + cmbVal7.Height + ppiFix(4);

  lblVal8 := TLabel.Create(gpbValues);
  lblVal8.Parent := gpbValues;
  lblVal8.Top := t + ppiFix(3);
  PlaceControl(bd, nil, lblVal8, ppiFix(LEFT_3COL_1), lblVal8.Width);
  lblVal8.Caption := '8';

  cmbVal8 := TComboBox.Create(gpbValues);
  cmbVal8.Parent := gpbValues;
  cmbVal8.Top := t;
  PlaceControl(bd, nil, cmbVal8, 2 * ppiFix(LEFT_3COL_1), w);
  cmbVal8.ItemIndex := -1;
  cmbVal8.OnChange := doValue8Change;
  oParentWindow.fillComboBoxWithFields(cmbVal8);

  edtVal8 := TEdit.Create(gpbValues);
  edtVal8.Parent := gpbValues;
  edtVal8.Top := t;
  PlaceControl(bd, cmbVal8, edtVal8, ppiFix(4), ppiFix(WIDTH_3COL) +
    ppiFix(LEFT_3COL_1));
  edtVal8.OnChange := doLegend8Change;

  pnlVal8 := TGIS_ColorPreview.Create(gpbValues);
  pnlVal8.Parent := gpbValues;
  pnlVal8.Top := t;
  pnlVal8.Height := edtVal8.Height;;
  PlaceControl(bd, edtVal8, pnlVal8, ppiFix(4), edtVal8.Height);
  pnlVal8.Color := TGIS_Color.Black;

  t := t + cmbVal8.Height + ppiFix(16);

  gpbValues.Height := t;
end;

procedure T_panelChart.Read;

  function get_params(const _txt: String; const _idx: Integer): String;
  var
    i, j: Integer;
  begin
    j := 0;
    Result := '';
    for i := 1 to length(_txt) do
    begin
      if _txt[i] = ':' then
      begin
        if j = _idx then
          break;
        Result := '';
        Inc(j);
      end
      else
        Result := Result + _txt[i];
    end;
    if j <> _idx then
      Result := '';
  end;

begin
  lockUpdates;
  try
    ckbLegend.Checked := MVC.ParamsChart.ShowLegend;

    // Chart
    case MVC.ParamsChart.Style of
      TGIS_ChartStyle.Pie:
        cmbStyle.ItemIndex := 0;
      TGIS_ChartStyle.Bar:
        cmbStyle.ItemIndex := 1;
    end;

    cmbSize.Value := MVC.ParamsChart.SizeAsText;

    edtMinVal.Text := get_params(MVC.RenderChart, 0);
    edtMaxVal.Text := get_params(MVC.RenderChart, 1);

    // Values
    cmbVal1.Text := get_params(MVC.RenderChart, 2);
    edtVal1.Text := get_params(MVC.ParamsChart.Legend, 2);
    pnlVal1.Color := MVC.ParamsChart.ColorsInternal[0];
    cmbVal2.Text := get_params(MVC.RenderChart, 3);
    edtVal2.Text := get_params(MVC.ParamsChart.Legend, 3);
    pnlVal2.Color := MVC.ParamsChart.ColorsInternal[1];
    cmbVal3.Text := get_params(MVC.RenderChart, 4);
    edtVal3.Text := get_params(MVC.ParamsChart.Legend, 4);
    pnlVal3.Color := MVC.ParamsChart.ColorsInternal[2];
    cmbVal4.Text := get_params(MVC.RenderChart, 5);
    edtVal4.Text := get_params(MVC.ParamsChart.Legend, 5);
    pnlVal4.Color := MVC.ParamsChart.ColorsInternal[3];
    cmbVal5.Text := get_params(MVC.RenderChart, 6);
    edtVal5.Text := get_params(MVC.ParamsChart.Legend, 6);
    pnlVal5.Color := MVC.ParamsChart.ColorsInternal[4];
    cmbVal6.Text := get_params(MVC.RenderChart, 7);
    edtVal6.Text := get_params(MVC.ParamsChart.Legend, 7);
    pnlVal6.Color := MVC.ParamsChart.ColorsInternal[5];
    cmbVal7.Text := get_params(MVC.RenderChart, 8);
    edtVal7.Text := get_params(MVC.ParamsChart.Legend, 8);
    pnlVal7.Color := MVC.ParamsChart.ColorsInternal[6];
    cmbVal8.Text := get_params(MVC.RenderChart, 9);
    edtVal8.Text := get_params(MVC.ParamsChart.Legend, 9);
    pnlVal8.Color := MVC.ParamsChart.ColorsInternal[7];

    wasLegend1Edited := False;
    wasLegend2Edited := False;
    wasLegend3Edited := False;
    wasLegend4Edited := False;
    wasLegend5Edited := False;
    wasLegend6Edited := False;
    wasLegend7Edited := False;
    wasLegend8Edited := False;

    MVC.doStyleChange;
    MVC.doSizeUseRenderer;
  finally
    unlockUpdates;
  end;
end;

procedure T_panelChart.Write;

  function add_val(const _val: String): String;
  begin
    Result := _val + ':'
  end;

  function add_color_val(const _val: TGIS_Color): String;
  begin
    Result := IntToStr(_val.R) + ':' + IntToStr(_val.G) + ':' + IntToStr(_val.b)
      + ':' + IntToStr(_val.A) + ',';
  end;

begin
  MVC.ParamsChart.ShowLegend := ckbLegend.Checked;

  // Chart
  case cmbStyle.ItemIndex of
    0:
      MVC.ParamsChart.Style := TGIS_ChartStyle.Pie;
    1:
      MVC.ParamsChart.Style := TGIS_ChartStyle.Bar;
  end;

  MVC.ParamsChart.SizeAsText := cmbSize.Value;

  MVC.RenderChart := add_val(edtMinVal.Text) + add_val(edtMaxVal.Text) +
    add_val(cmbVal1.Text) + add_val(cmbVal2.Text) + add_val(cmbVal3.Text) +
    add_val(cmbVal4.Text) + add_val(cmbVal5.Text) + add_val(cmbVal6.Text) +
    add_val(cmbVal7.Text) + add_val(cmbVal8.Text);
  MVC.ParamsChart.Values := MVC.RenderChart;
  MVC.ParamsChart.Legend := add_val('') + add_val('') + add_val(edtVal1.Text) +
    add_val(edtVal2.Text) + add_val(edtVal3.Text) + add_val(edtVal4.Text) +
    add_val(edtVal5.Text) + add_val(edtVal6.Text) + add_val(edtVal7.Text) +
    add_val(edtVal8.Text);
  MVC.ParamsChart.Colors := add_color_val(pnlVal1.Color) +
    add_color_val(pnlVal2.Color) + add_color_val(pnlVal3.Color) +
    add_color_val(pnlVal4.Color) + add_color_val(pnlVal5.Color) +
    add_color_val(pnlVal6.Color) + add_color_val(pnlVal7.Color) +
    add_color_val(pnlVal8.Color);

end;

procedure T_panelChart.PreparePreview(const _viewer: IGIS_Viewer);
begin
  MVC.PreparePreview(_viewer);
end;

procedure T_panelChart.UpdatePreview;
var
  ll: TGIS_Layer;
  paramsvec: TGIS_ParamsSectionVector;
  txt: String;
  i: Integer;
begin
  oParentWindow.tmrUpdate.Enabled := False;
  if oParentWindow.gisPreview.IsEmpty then
    exit;
  oParentWindow.tmrUpdate.Enabled := True;

  ll := TGIS_Layer(oParentWindow.gisPreview.Items[0]);

  paramsvec := TGIS_ParamsSectionVector(ll.Params);

  MVC.SectionWrite(paramsvec);

  paramsvec.Marker.Size := 0;

  txt := '0:0';
  if assigned(paramsvec.Render.ChartObj) then
  begin
    for i := 2 to paramsvec.Render.ChartObj.Count - 1 do
    begin
      if assigned(paramsvec.Render.ChartObj[i]) then
        txt := txt + ':1'
      else
        txt := txt + ':';
    end;
  end;
  paramsvec.Render.Chart := '';
  paramsvec.Chart.Values := txt;

  if paramsvec.Chart.Size = GIS_RENDER_SIZE then
    paramsvec.Chart.Size :=
      -(oParentWindow.gisPreview.ControlCanvasWidth div 2);

  paramsvec.Query := '';

  with TGIS_ParamsSection(ll.Params) do
  begin
    MinZoom := 0;
    MaxZoom := GIS_MAX_DOUBLE;
    MinScale := 0;
    MaxScale := GIS_MAX_DOUBLE;
  end;

  // avoid unreasonable preview
  paramsvec.Chart.Size := normalize_size(oParentWindow.gisPreview,
    paramsvec.Chart.Size, paramsvec.Chart.SizeAsText, MAX_PREVIEW_SIZE_MAKER);
end;

procedure T_panelChart.doCallback(_sender: TObject; _code: Integer);

  procedure do_style_change;
  var
    b: Boolean;
  begin
    b := cmbStyle.ItemIndex = 1;

    lblMinVal.Enabled := b;
    edtMinVal.Enabled := b;
    lblMaxVal.Enabled := b;
    edtMaxVal.Enabled := b;
  end;

  procedure do_size_ur;
  begin
  end;

  procedure do_value1_change;
  begin
    if not wasLegend1Edited then
      edtVal1.Text := cmbVal1.Text;
  end;

  procedure do_legend1_change;
  begin
    if wasLegend1Edited then
      exit;

    if edtVal1.Text <> cmbVal1.Text then
      wasLegend1Edited := True;
  end;

  procedure do_value2_change;
  begin
    if not wasLegend2Edited then
      edtVal2.Text := cmbVal2.Text;
  end;

  procedure do_legend2_change;
  begin
    if wasLegend2Edited then
      exit;

    if edtVal2.Text <> cmbVal2.Text then
      wasLegend2Edited := True;
  end;

  procedure do_value3_change;
  begin
    if not wasLegend3Edited then
      edtVal3.Text := cmbVal3.Text;
  end;

  procedure do_legend3_change;
  begin
    if wasLegend3Edited then
      exit;

    if edtVal3.Text <> cmbVal3.Text then
      wasLegend3Edited := True;
  end;

  procedure do_value4_change;
  begin
    if not wasLegend4Edited then
      edtVal4.Text := cmbVal4.Text;
  end;

  procedure do_legend4_change;
  begin
    if wasLegend4Edited then
      exit;

    if edtVal4.Text <> cmbVal4.Text then
      wasLegend4Edited := True;
  end;

  procedure do_value5_change;
  begin
    if not wasLegend5Edited then
      edtVal5.Text := cmbVal5.Text;
  end;

  procedure do_legend5_change;
  begin
    if wasLegend5Edited then
      exit;

    if edtVal5.Text <> cmbVal5.Text then
      wasLegend5Edited := True;
  end;

  procedure do_value6_change;
  begin
    if not wasLegend6Edited then
      edtVal6.Text := cmbVal6.Text;
  end;

  procedure do_legend6_change;
  begin
    if wasLegend6Edited then
      exit;

    if edtVal6.Text <> cmbVal6.Text then
      wasLegend6Edited := True;
  end;

  procedure do_value7_change;
  begin
    if not wasLegend7Edited then
      edtVal7.Text := cmbVal7.Text;
  end;

  procedure do_legend7_change;
  begin
    if wasLegend7Edited then
      exit;

    if edtVal7.Text <> cmbVal7.Text then
      wasLegend7Edited := True;
  end;

  procedure do_value8_change;
  begin
    if not wasLegend8Edited then
      edtVal8.Text := cmbVal8.Text;
  end;

  procedure do_legend8_change;
  begin
    if wasLegend8Edited then
      exit;

    if edtVal8.Text <> cmbVal8.Text then
      wasLegend8Edited := True;
  end;

begin
  case _code of
    1:
      do_style_change;
    2:
      do_size_ur;
    10:
      do_value1_change;
    11:
      do_legend1_change;
    20:
      do_value2_change;
    21:
      do_legend2_change;
    30:
      do_value3_change;
    31:
      do_legend3_change;
    40:
      do_value4_change;
    41:
      do_legend4_change;
    50:
      do_value5_change;
    51:
      do_legend5_change;
    60:
      do_value6_change;
    61:
      do_legend6_change;
    70:
      do_value7_change;
    71:
      do_legend7_change;
    80:
      do_value8_change;
    81:
      do_legend8_change;
  end;

  if not blockedUpdates then
  begin
    Write;
    UpdatePreview;
  end;

end;

procedure T_panelChart.doStyleChange(_sender: TObject);
begin
  MVC.doStyleChange;
end;

procedure T_panelChart.doSizeUseRenderer(_sender: TObject);
begin
  MVC.doSizeUseRenderer;
end;

procedure T_panelChart.doValue1Change(_sender: TObject);
begin
  MVC.doValue1Change;
end;

procedure T_panelChart.doLegend1Change(_sender: TObject);
begin
  MVC.doLegend1Change;
end;

procedure T_panelChart.doValue2Change(_sender: TObject);
begin
  MVC.doValue2Change;
end;

procedure T_panelChart.doLegend2Change(_sender: TObject);
begin
  MVC.doLegend2Change;
end;

procedure T_panelChart.doValue3Change(_sender: TObject);
begin
  MVC.doValue3Change;
end;

procedure T_panelChart.doLegend3Change(_sender: TObject);
begin
  MVC.doLegend3Change;
end;

procedure T_panelChart.doValue4Change(_sender: TObject);
begin
  MVC.doValue4Change;
end;

procedure T_panelChart.doLegend4Change(_sender: TObject);
begin
  MVC.doLegend4Change;
end;

procedure T_panelChart.doValue5Change(_sender: TObject);
begin
  MVC.doValue5Change;
end;

procedure T_panelChart.doLegend5Change(_sender: TObject);
begin
  MVC.doLegend5Change;
end;

procedure T_panelChart.doValue6Change(_sender: TObject);
begin
  MVC.doValue6Change;
end;

procedure T_panelChart.doLegend6Change(_sender: TObject);
begin
  MVC.doLegend6Change;
end;

procedure T_panelChart.doValue7Change(_sender: TObject);
begin
  MVC.doValue7Change;
end;

procedure T_panelChart.doLegend7Change(_sender: TObject);
begin
  MVC.doLegend7Change;
end;

procedure T_panelChart.doValue8Change(_sender: TObject);
begin
  MVC.doValue8Change;
end;

procedure T_panelChart.doLegend8Change(_sender: TObject);
begin
  MVC.doLegend8Change;
end;

procedure T_panelChart.doControlChange(_sender: TObject);
begin
  MVC.doControlChange;
end;
{$ENDREGION}
{$REGION 'T_panelPixel'}

function T_panelPixel.fget_HasPreview: Boolean;
begin
  Result := MVC.HasPreview;
end;

procedure T_panelPixel.init;
begin
  inherited;

  ItemText := _rsrc(GIS_RS_LEGEND_PAG_PIXEL);

  MVC := oParentWindow.MVC.Pixel;
  MVC.Callback := doCallback;

  initSelf;
end;

procedure T_panelPixel.initSelf;
var
  t, i: Integer;
  bd: TBiDiMode;
  Anchors: TAnchors;
begin
  bd := oParentWindow.BiDiMode;
  if bd = bdRightToLeft then
    Anchors := [akRight, akTop]
  else
    Anchors := [akLeft, akTop];

  gpbColors := TGroupBox.Create(Self.Panel);
  gpbColors.Parent := Self.Panel;
  gpbColors.Top := ppiFix(4);
  gpbColors.Height := ppiFix(200);
  PlaceControl(bd, nil, gpbColors, ppiFix(8), ppiFix(WIDTH_NORMAL));
  gpbColors.Anchors := Anchors;
  gpbColors.Caption := 'Colors';

  t := ppiFix(24);

  lblRed := TLabel.Create(gpbColors);
  lblRed.Parent := gpbColors;
  lblRed.Top := t;
  PlaceControl(bd, nil, lblRed, ppiFix(LEFT_3COL_1), lblRed.Width);
  lblRed.Caption := _rsrc(GIS_RS_LEGEND_PRM_RED);

  lblGreen := TLabel.Create(gpbColors);
  lblGreen.Parent := gpbColors;
  lblGreen.Top := t;
  PlaceControl(bd, nil, lblGreen, ppiFix(LEFT_3COL_2), lblGreen.Width);
  lblGreen.Caption := _rsrc(GIS_RS_LEGEND_PRM_GREEN);

  lblBlue := TLabel.Create(gpbColors);
  lblBlue.Parent := gpbColors;
  lblBlue.Top := t;
  PlaceControl(bd, nil, lblBlue, ppiFix(LEFT_3COL_3), lblBlue.Width);
  lblBlue.Caption := _rsrc(GIS_RS_LEGEND_PRM_BLUE);

  t := t + lblRed.Height + ppiFix(4);

  speRed := TEdit.Create(gpbColors);
  speRed.Parent := gpbColors;
  speRed.Top := t;
  PlaceControl(bd, nil, speRed, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  speRed.OnChange := doControlChange;

  vvspeRed := TGIS_ValueValidatorEditHelper.Create(speRed);
  vvspeRed.MinVal := -100.0;
  vvspeRed.MaxVal := 100.0;

  speGreen := TEdit.Create(gpbColors);
  speGreen.Parent := gpbColors;
  speGreen.Top := t;
  PlaceControl(bd, nil, speGreen, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  speGreen.OnChange := doControlChange;

  vvspeGreen := TGIS_ValueValidatorEditHelper.Create(speGreen);
  vvspeGreen.MinVal := -100.0;
  vvspeGreen.MaxVal := 100.0;

  speBlue := TEdit.Create(gpbColors);
  speBlue.Parent := gpbColors;
  speBlue.Top := t;
  PlaceControl(bd, nil, speBlue, ppiFix(LEFT_3COL_3), ppiFix(WIDTH_3COL));
  speBlue.OnChange := doControlChange;

  vvspeBlue := TGIS_ValueValidatorEditHelper.Create(speBlue);
  vvspeBlue.MinVal := -100.0;
  vvspeBlue.MaxVal := 100.0;

  t := t + speRed.Height + ppiFix(16);

  lblBrightness := TLabel.Create(gpbColors);
  lblBrightness.Parent := gpbColors;
  lblBrightness.Left := ppiFix(LEFT_3COL_1);
  lblBrightness.Top := t;
  PlaceControl(bd, nil, lblBrightness, ppiFix(LEFT_3COL_1),
    lblBrightness.Width);
  lblBrightness.Caption := _rsrc(GIS_RS_LEGEND_PRM_BRIGHTNESS);

  lblContrast := TLabel.Create(gpbColors);
  lblContrast.Parent := gpbColors;
  lblContrast.Top := t;
  PlaceControl(bd, nil, lblContrast, ppiFix(LEFT_3COL_2), lblContrast.Width);
  lblContrast.Caption := _rsrc(GIS_RS_LEGEND_PRM_CONTRAST);

  t := t + lblBrightness.Height + ppiFix(4);

  speBrightness := TEdit.Create(gpbColors);
  speBrightness.Parent := gpbColors;
  speBrightness.Top := t;
  PlaceControl(bd, nil, speBrightness, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  speBrightness.OnChange := doControlChange;

  vvspeBrightness := TGIS_ValueValidatorEditHelper.Create(speBrightness);
  vvspeBrightness.MinVal := -100.0;
  vvspeBrightness.MaxVal := 100.0;

  speContrast := TEdit.Create(gpbColors);
  speContrast.Parent := gpbColors;
  speContrast.Top := t;
  PlaceControl(bd, nil, speContrast, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  speContrast.OnChange := doControlChange;

  vvspeContrast := TGIS_ValueValidatorEditHelper.Create(speContrast);
  vvspeContrast.MinVal := -100.0;
  vvspeContrast.MaxVal := 100.0;

  t := t + speContrast.Height + ppiFix(8);

  ckbInversion := TCheckBox.Create(gpbColors);
  ckbInversion.Parent := gpbColors;
  ckbInversion.Top := t;
  PlaceControl(bd, nil, ckbInversion, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  ckbInversion.Caption := _rsrc(GIS_RS_LEGEND_PRM_INVERSION);
  ckbInversion.OnClick := doControlChange;

  ckbGrayscale := TCheckBox.Create(gpbColors);
  ckbGrayscale.Parent := gpbColors;
  ckbGrayscale.Top := t;
  PlaceControl(bd, nil, ckbGrayscale, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  ckbGrayscale.Caption := _rsrc(GIS_RS_LEGEND_PRM_GRAYSCALE);
  ckbGrayscale.OnClick := doControlChange;

  ckbHistogram := TCheckBox.Create(gpbColors);
  ckbHistogram.Parent := gpbColors;
  ckbHistogram.Top := t;
  PlaceControl(bd, nil, ckbHistogram, ppiFix(LEFT_3COL_3), ppiFix(WIDTH_3COL));
  ckbHistogram.Caption := _rsrc(GIS_RS_LEGEND_PRM_HISTOGRAM);
  ckbHistogram.OnClick := doControlChange;

  t := t + ckbHistogram.Height + ppiFix(8);

  ckbContrastEnhanced := TCheckBox.Create(gpbColors);
  ckbContrastEnhanced.Parent := gpbColors;
  ckbContrastEnhanced.Top := t;
  PlaceControl(bd, nil, ckbContrastEnhanced, ppiFix(LEFT_3COL_1),
    ppiFix(WIDTH_3COL) + ppiFix(LEFT_3COL_1));
  ckbContrastEnhanced.Caption := _rsrc(GIS_RS_LEGEND_PRM_CONTRASTENHANCED);
  ckbContrastEnhanced.OnClick := doControlChange;

  gpbColors.Height := t + ckbContrastEnhanced.Height + ppiFix(16);

  t := gpbColors.Top + gpbColors.Height + ppiFix(8);

  gpbBands := TGroupBox.Create(Self.Panel);
  gpbBands.Parent := Self.Panel;
  gpbBands.Top := t;
  gpbBands.Height := ppiFix(200);
  PlaceControl(bd, nil, gpbBands, ppiFix(8), ppiFix(WIDTH_NORMAL));
  gpbBands.Anchors := Anchors;
  gpbBands.Caption := 'Bands';

  t := ppiFix(24);

  lblRedBand := TLabel.Create(gpbBands);
  lblRedBand.Parent := gpbBands;
  lblRedBand.Top := t;
  PlaceControl(bd, nil, lblRedBand, ppiFix(LEFT_3COL_1), lblRedBand.Width);
  lblRedBand.Caption := _rsrc(GIS_RS_LEGEND_PRM_REDBAND);

  lblGreenBand := TLabel.Create(gpbBands);
  lblGreenBand.Parent := gpbBands;
  lblGreenBand.Top := t;
  PlaceControl(bd, nil, lblGreenBand, ppiFix(LEFT_3COL_2), lblGreenBand.Width);
  lblGreenBand.Caption := _rsrc(GIS_RS_LEGEND_PRM_GREENBAND);

  lblBlueBand := TLabel.Create(gpbBands);
  lblBlueBand.Parent := gpbBands;
  lblBlueBand.Top := t;
  PlaceControl(bd, nil, lblBlueBand, ppiFix(LEFT_3COL_3), lblBlueBand.Width);
  lblBlueBand.Caption := _rsrc(GIS_RS_LEGEND_PRM_BLUEBAND);

  t := t + lblRedBand.Height + ppiFix(4);

  cmbRedBand := TComboBox.Create(gpbBands);
  cmbRedBand.Parent := gpbBands;
  cmbRedBand.Style := csDropDownList;
  cmbRedBand.Top := t;
  PlaceControl(bd, nil, cmbRedBand, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbRedBand.Items.BeginUpdate;
  cmbRedBand.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_OFF));
  cmbRedBand.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_DEFAULT));
  for i := 0 to TGIS_LayerPixel(oParentWindow.MVC.Layer).BandsCount - 1 do
    cmbRedBand.Items.Add(IntToStr(i + 1));

  cmbRedBand.ItemIndex := 1;
  cmbRedBand.Items.EndUpdate;
  cmbRedBand.OnChange := doControlChange;

  cmbGreenBand := TComboBox.Create(gpbBands);
  cmbGreenBand.Parent := gpbBands;
  cmbGreenBand.Style := csDropDownList;
  cmbGreenBand.Top := t;
  PlaceControl(bd, nil, cmbGreenBand, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  cmbGreenBand.Items.BeginUpdate;
  cmbGreenBand.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_OFF));
  cmbGreenBand.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_DEFAULT));
  for i := 0 to TGIS_LayerPixel(oParentWindow.MVC.Layer).BandsCount - 1 do
    cmbGreenBand.Items.Add(IntToStr(i + 1));
  cmbGreenBand.ItemIndex := 1;
  cmbGreenBand.Items.EndUpdate;
  cmbGreenBand.OnChange := doControlChange;

  cmbBlueBand := TComboBox.Create(gpbBands);
  cmbBlueBand.Parent := gpbBands;
  cmbBlueBand.Style := csDropDownList;
  cmbBlueBand.Top := t;
  PlaceControl(bd, nil, cmbBlueBand, ppiFix(LEFT_3COL_3), ppiFix(WIDTH_3COL));
  cmbBlueBand.Items.BeginUpdate;
  cmbBlueBand.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_OFF));
  cmbBlueBand.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_DEFAULT));
  for i := 0 to TGIS_LayerPixel(oParentWindow.MVC.Layer).BandsCount - 1 do
    cmbBlueBand.Items.Add(IntToStr(i + 1));
  cmbBlueBand.ItemIndex := 1;
  cmbBlueBand.Items.EndUpdate;
  cmbBlueBand.OnChange := doControlChange;

  t := t + cmbRedBand.Height + ppiFix(16);

  lblAlphaBand := TLabel.Create(gpbBands);
  lblAlphaBand.Parent := gpbBands;
  lblAlphaBand.Top := t;
  lblAlphaBand.Left := ppiFix(LEFT_3COL_1);
  PlaceControl(bd, nil, lblAlphaBand, ppiFix(LEFT_3COL_1), lblAlphaBand.Width);
  lblAlphaBand.Caption := _rsrc(GIS_RS_LEGEND_PRM_ALPHABAND);

  lblPage := TLabel.Create(gpbBands);
  lblPage.Parent := gpbBands;
  lblPage.Top := t;
  PlaceControl(bd, nil, lblPage, ppiFix(LEFT_3COL_3), lblPage.Width);
  lblPage.Caption := _rsrc(GIS_RS_LEGEND_PRM_PAGE);

  t := t + lblPage.Height + ppiFix(4);

  cmbAlphaBand := TComboBox.Create(gpbBands);
  cmbAlphaBand.Parent := gpbBands;
  cmbAlphaBand.Style := csDropDownList;
  cmbAlphaBand.Top := t;
  PlaceControl(bd, nil, cmbAlphaBand, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbAlphaBand.Items.BeginUpdate;
  cmbAlphaBand.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_OFF));
  cmbAlphaBand.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_DEFAULT));
  for i := 0 to TGIS_LayerPixel(oParentWindow.MVC.Layer).BandsCount - 1 do
    cmbAlphaBand.Items.Add(IntToStr(i + 1));
  cmbAlphaBand.ItemIndex := 1;
  cmbAlphaBand.Items.EndUpdate;
  cmbAlphaBand.OnChange := doControlChange;

  cmbPage := TComboBox.Create(gpbBands);
  cmbPage.Parent := gpbBands;
  cmbPage.Style := csDropDownList;
  cmbPage.Top := t;
  PlaceControl(bd, nil, cmbPage, ppiFix(LEFT_3COL_3), ppiFix(WIDTH_3COL));
  cmbPage.Items.BeginUpdate;
  for i := 0 to TGIS_LayerPixel(oParentWindow.MVC.Layer).PageCount - 1 do
    cmbPage.Items.Add(IntToStr(i + 1));
  cmbPage.ItemIndex := 0;
  cmbPage.Items.EndUpdate;
  cmbPage.OnChange := doControlChange;

  gpbBands.Height := t + cmbPage.Height + ppiFix(16);

  t := t + ckbGrayscale.Height + ppiFix(8);

  gpbTransparency := TGroupBox.Create(Self.Panel);
  gpbTransparency.Parent := Self.Panel;
  gpbTransparency.Top := gpbBands.Top + gpbBands.Height + ppiFix(4);
  gpbTransparency.Height := ppiFix(200);
  PlaceControl(bd, nil, gpbTransparency, ppiFix(8), ppiFix(WIDTH_NORMAL));
  gpbTransparency.Anchors := Anchors;
  gpbTransparency.Caption := _rsrc(GIS_RS_LEGEND_PRM_TRANSPARENCY);

  t := ppiFix(24);

  pnlColorFrom := TGIS_ColorPreview.Create(gpbTransparency);
  pnlColorFrom.Parent := gpbTransparency;
  pnlColorFrom.Top := t;
  pnlColorFrom.Height := ppiFix(21);
  PlaceControl(bd, nil, pnlColorFrom, ppiFix(LEFT_3COL_1) + ppiFix(23 + 8 - 2),
    ppiFix(WIDTH_3COL));
  pnlColorFrom.CellSize := ppiFix(5);
  pnlColorFrom.OnDialogChange := doZonesChange;

  pnlColorTo := TGIS_ColorPreview.Create(gpbTransparency);
  pnlColorTo.Parent := gpbTransparency;
  pnlColorTo.Top := t;
  pnlColorTo.Height := ppiFix(21);
  PlaceControl(bd, pnlColorFrom, pnlColorTo, ppiFix(4), ppiFix(WIDTH_3COL));
  pnlColorTo.CellSize := ppiFix(5);
  pnlColorTo.OnDialogChange := doZonesChange;

  t := t + pnlColorTo.Height + ppiFix(4);

  btnAdd := T_speedButton.Create(gpbTransparency);
  btnAdd.Parent := gpbTransparency;
  btnAdd.Top := t;
  btnAdd.Height := ppiFix(25);
  PlaceControl(bd, nil, btnAdd, ppiFix(LEFT_3COL_1), ppiFix(25));
  btnAdd.OnClick := doAddClick;
  T_speedButton(btnAdd).Images := oParentWindow.imgListSclN;
  T_speedButton(btnAdd).DisabledImages := oParentWindow.imgListSclD;
  T_speedButton(btnAdd).ImageIndex := 0;

  lstZones := TListBox.Create(gpbTransparency);
  lstZones.Parent := gpbTransparency;
  lstZones.Top := t;
  lstZones.Height := ppiFix(100);
  PlaceControl(bd, btnAdd, lstZones, ppiFix(4), ppiFix(WIDTH_3COL) * 3);
  lstZones.ParentColor := False;
  lstZones.Color := clWhite;

  lstZones.OnDrawItem := lstZoneDrawItem;
  lstZones.OnClick := lstZoneClick;
  lstZones.Style := lbOwnerDrawFixed;

  t := t + btnAdd.Height + ppiFix(8);

  btnRemove := T_speedButton.Create(gpbTransparency);
  btnRemove.Parent := gpbTransparency;
  btnRemove.Top := t;
  btnRemove.Height := ppiFix(25);
  PlaceControl(bd, nil, btnRemove, ppiFix(LEFT_3COL_1), ppiFix(25));
  btnRemove.OnClick := doDeleteClick;
  T_speedButton(btnRemove).Images := oParentWindow.imgListSclN;
  T_speedButton(btnRemove).DisabledImages := oParentWindow.imgListSclD;
  T_speedButton(btnRemove).ImageIndex := 2;

  t := t + btnRemove.Height + ppiFix(8);

  btnClear := T_speedButton.Create(gpbTransparency);
  btnClear.Parent := gpbTransparency;
  btnClear.Top := t;
  btnClear.Height := ppiFix(25);
  PlaceControl(bd, nil, btnClear, ppiFix(LEFT_3COL_1), ppiFix(25));
  btnClear.OnClick := doClearClick;
  T_speedButton(btnClear).Images := oParentWindow.imgListSclN;
  T_speedButton(btnClear).DisabledImages := oParentWindow.imgListSclD;
  T_speedButton(btnClear).ImageIndex := 1;

  gpbTransparency.Height := lstZones.Top + lstZones.Height + ppiFix(16);

  t := gpbTransparency.Top + gpbTransparency.Height + ppiFix(8);

  btnReset := TButton.Create(Self);
  btnReset.Parent := Self;
  btnReset.Top := t;
  PlaceControl(bd, nil, btnReset, ppiFix(WIDTH_NORMAL) - ppiFix(88),
    ppiFix(80));
  btnReset.Caption := _rsrc(GIS_RS_BTN_RESET);
  btnReset.Anchors := Anchors;
  btnReset.OnClick := doReset;

  ckbLegend := TCheckBox.Create(Self.Panel);
  ckbLegend.Parent := Self.Panel;
  ckbLegend.Top := t;
  PlaceControl(bd, nil, ckbLegend, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_2COL));
  if bd = bdRightToLeft then
    ckbLegend.Anchors := [akRight, akTop]
  else
    ckbLegend.Anchors := [akLeft, akTop];
  ckbLegend.Caption := _rsrc(GIS_RS_LEGEND_PRM_INCLUDEINLEGEND);

end;

procedure T_panelPixel.PreparePreview(const _viewer: IGIS_Viewer);
begin
  MVC.PreparePreview(_viewer);
end;

procedure T_panelPixel.Read;
begin
  lockUpdates;
  try
    ckbLegend.Checked := MVC.ParamsPixel.ShowLegend;
    vvspeRed.Value := MVC.ParamsPixel.Red;
    vvspeGreen.Value := MVC.ParamsPixel.Green;
    vvspeBlue.Value := MVC.ParamsPixel.Blue;
    vvspeBrightness.Value := MVC.ParamsPixel.Brightness;
    vvspeContrast.Value := MVC.ParamsPixel.Contrast;

    cmbRedBand.Items.BeginUpdate;
    cmbRedBand.ItemIndex := MVC.ParamsPixel.RedBand + 1;
    cmbRedBand.Items.EndUpdate;

    cmbGreenBand.Items.BeginUpdate;
    cmbGreenBand.ItemIndex := MVC.ParamsPixel.GreenBand + 1;
    cmbGreenBand.Items.EndUpdate;

    cmbBlueBand.Items.BeginUpdate;
    cmbBlueBand.ItemIndex := MVC.ParamsPixel.BlueBand + 1;
    cmbBlueBand.Items.EndUpdate;

    cmbAlphaBand.Items.BeginUpdate;
    cmbAlphaBand.ItemIndex := MVC.ParamsPixel.AlphaBand + 1;
    cmbAlphaBand.Items.EndUpdate;

    cmbPage.Items.BeginUpdate;
    cmbPage.ItemIndex := MVC.ParamsPixel.Page - 1;
    cmbPage.Items.EndUpdate;

    ckbInversion.Checked := MVC.ParamsPixel.Inversion;
    ckbGrayscale.Checked := MVC.ParamsPixel.Grayscale;
    ckbHistogram.Checked := MVC.ParamsPixel.Histogram;
    ckbContrastEnhanced.Checked := MVC.ParamsPixel.ContrastEnhanced;

    lstZones.Items.BeginUpdate;
    lstZones.Items.Assign(MVC.ParamsPixel.TransparentZones);
    if lstZones.Items.Count > 0 then
      lstZones.ItemIndex := 0;
    lstZones.Items.EndUpdate;

    lstZoneClick(Self);
  finally
    unlockUpdates;
  end;
end;

procedure T_panelPixel.UpdatePreview;
var
  lp: TGIS_LayerPixel;
  paramspix: TGIS_ParamsSectionPixel;
begin
  oParentWindow.tmrUpdate.Enabled := False;
  if oParentWindow.gisPreview.IsEmpty then
    exit;
  oParentWindow.tmrUpdate.Enabled := True;

  lp := TGIS_LayerPixel(oParentWindow.gisPreview.Items[0]);

  if (lp.Params.Pixel.Page <> MVC.ParamsPixel.Page) or
    (lp.Params.Pixel.AlphaBand <> MVC.ParamsPixel.AlphaBand) or
    (lp.Params.Pixel.RedBand <> MVC.ParamsPixel.RedBand) or
    (lp.Params.Pixel.GreenBand <> MVC.ParamsPixel.GreenBand) or
    (lp.Params.Pixel.BlueBand <> MVC.ParamsPixel.BlueBand) then
  begin
    // recreate preview
    MVC.PreparePreview(oParentWindow.gisPreview);
    lp := TGIS_LayerPixel(oParentWindow.gisPreview.Items[0]);
  end;

  paramspix := lp.Params;
  MVC.SectionWrite(paramspix);

  with lp.Params do
  begin
    MinZoom := 0;
    MaxZoom := GIS_MAX_DOUBLE;
    MinScale := 0;
    MaxScale := GIS_MAX_DOUBLE;
  end;
end;

procedure T_panelPixel.Write;
begin
  MVC.ParamsPixel.ShowLegend := ckbLegend.Checked;
  MVC.ParamsPixel.Red := FloorS(vvspeRed.Value);
  MVC.ParamsPixel.Green := FloorS(vvspeGreen.Value);
  MVC.ParamsPixel.Blue := FloorS(vvspeBlue.Value);
  MVC.ParamsPixel.Brightness := FloorS(vvspeBrightness.Value);
  MVC.ParamsPixel.Contrast := FloorS(vvspeContrast.Value);
  MVC.ParamsPixel.RedBand := cmbRedBand.ItemIndex - 1;
  MVC.ParamsPixel.GreenBand := cmbGreenBand.ItemIndex - 1;
  MVC.ParamsPixel.BlueBand := cmbBlueBand.ItemIndex - 1;
  MVC.ParamsPixel.AlphaBand := cmbAlphaBand.ItemIndex - 1;
  MVC.ParamsPixel.Page := cmbPage.ItemIndex + 1;
  MVC.ParamsPixel.Inversion := ckbInversion.Checked;
  MVC.ParamsPixel.Grayscale := ckbGrayscale.Checked;
  MVC.ParamsPixel.Histogram := ckbHistogram.Checked;
  MVC.ParamsPixel.ContrastEnhanced := ckbContrastEnhanced.Checked;
  MVC.ParamsPixel.TransparentZones.Assign(lstZones.Items);
end;

procedure T_panelPixel.doCallback(_sender: TObject; _code: Integer);

  procedure do_reset;
  var
    idx: Integer;
  begin
    idx := oParentWindow.MVC.SectionIndex;
    oParentWindow.MVC.SectionIndex := -1;
    oParentWindow.MVC.Pixel.SectionRead(False);
    Read;

    oParentWindow.MVC.SectionIndex := idx;
  end;

begin
  case _code of
    1:
      do_reset;
    2:
      doUpdate(_sender);
  end;

  if not blockedUpdates then
  begin
    Write;
    UpdatePreview;
  end;

end;

procedure T_panelPixel.doReset(_sender: TObject);
begin
  MVC.doReset;
end;

procedure T_panelPixel.doControlChange(_sender: TObject);
begin
  MVC.doControlChange;
end;

procedure T_panelPixel.lstZoneDrawItem(_ctrl: TWinControl; _idx: Integer;
  _rect: TRect; _state: TOwnerDrawState);
var
  R: TRect;
  tkn: TGIS_Tokenizer;
  sfrom: String;
  sto: String;
  tf: TTextFormat;
begin
  sfrom := '';
  sto := '';

  tkn := TGIS_Tokenizer.Create;
  try
    tkn.ExecuteEx(lstZones.Items[_idx]);

    if tkn.Result.Count > 0 then
      sfrom := tkn.Result[0];

    if tkn.Result.Count > 1 then
      sto := tkn.Result[1];
  finally
    FreeObject(tkn);
  end;

  lstZones.Canvas.FillRect(_rect);

  if BiDiMode = bdRightToLeft then
    tf := [tfRight]
  else
    tf := [];

  R := _rect;
  R.Left := pnlColorFrom.Left - lstZones.Left;
  R.Right := pnlColorFrom.Left - lstZones.Left + pnlColorFrom.Width - 5;
  R.Top := R.Top + 1;
  lstZones.Canvas.TextRect(R, sfrom, tf);

  R := _rect;
  R.Left := pnlColorTo.Left - lstZones.Left;
  R.Right := pnlColorTo.Left + pnlColorTo.Width - lstZones.Left - 5;
  R.Top := R.Top + 1;
  lstZones.Canvas.TextRect(R, sto, tf);
end;

procedure T_panelPixel.lstZoneClick(_sender: TObject);
var
  tkn: TGIS_Tokenizer;
  cl: TGIS_Color;

  function fixColor(const _strColor: String): TGIS_Color;
  begin
    Result := ParamColor(_strColor, TGIS_Color.DimGray);
  end;

begin
  if lstZones.ItemIndex < 0 then
    exit;

  tkn := TGIS_Tokenizer.Create;
  try
    tkn.ExecuteEx(lstZones.Items[lstZones.ItemIndex]);

    if tkn.Result.Count > 0 then
      pnlColorFrom.Color := fixColor(tkn.Result[0])
    else
      pnlColorFrom.Color := TGIS_Color.White;

    if tkn.Result.Count > 1 then
      pnlColorTo.Color := fixColor(tkn.Result[1])
    else
      pnlColorTo.Color := TGIS_Color.White;
  finally
    FreeObject(tkn);
  end;
end;

procedure T_panelPixel.doAddClick(_sender: TObject);
begin
  lstZones.Items.BeginUpdate;
  if lstZones.ItemIndex >= 0 then
  begin
    lstZones.Items.Insert(lstZones.ItemIndex,
      lstZones.Items[lstZones.ItemIndex]);
    lstZones.ItemIndex;
  end
  else
  begin
    lstZones.Items.Add('');
    lstZones.ItemIndex := 0;
  end;
  lstZones.Items.EndUpdate;
  doUpdate(_sender);
  lstZoneClick(_sender);
  MVC.DoTransparentZonesChange;
end;

procedure T_panelPixel.doDeleteClick(_sender: TObject);
var
  idx: Integer;
begin
  idx := lstZones.ItemIndex;

  if idx >= 0 then
    lstZones.Items.Delete(idx);

  doUpdate(_sender);
  lstZones.ItemIndex := Min(idx, lstZones.Items.Count - 1);
  lstZoneClick(_sender);
  MVC.DoTransparentZonesChange;
end;

procedure T_panelPixel.doClearClick(_sender: TObject);
begin
  lstZones.Clear;
  doUpdate(_sender);
  MVC.DoTransparentZonesChange;
end;

procedure T_panelPixel.doUpdate(_sender: TObject);
var
  stmp: String;
begin
  if lstZones.ItemIndex >= 0 then
  begin
    stmp := '$' + IntToHex(pnlColorFrom.Color.ToABGR, 2) + ',' + '$' +
      IntToHex(pnlColorTo.Color.ToABGR, 2);

    lstZones.Items[lstZones.ItemIndex] := stmp;
  end;

end;

procedure T_panelPixel.doZonesChange(_sender: TObject);
begin
  MVC.DoTransparentZonesChange;
end;

{$ENDREGION}
{$REGION 'T_panelGrid'}

function T_panelGrid.fget_HasPreview: Boolean;
begin
  Result := MVC.HasPreview;
end;

procedure T_panelGrid.init;
begin
  inherited;

  ItemText := _rsrc(GIS_RS_LEGEND_PAG_GRID);

  MVC := oParentWindow.MVC.Pixel;
  MVC.Callback := doCallback;

  lockChange := False;
  initSelf;
end;

procedure T_panelGrid.initSelf;
var
  t, i: Integer;
  bd: TBiDiMode;
  Anchors: TAnchors;
begin
  bd := oParentWindow.BiDiMode;
  if bd = bdRightToLeft then
    Anchors := [akRight, akTop]
  else
    Anchors := [akLeft, akTop];

  gpbGridd := TGroupBox.Create(Self.Panel);
  gpbGridd.Parent := Self.Panel;
  gpbGridd.Top := ppiFix(8);
  gpbGridd.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbGridd, ppiFix(8), ppiFix(WIDTH_NORMAL));
  gpbGridd.Anchors := Anchors;
  gpbGridd.Caption := _rsrc(GIS_RS_LEGEND_PAG_GRID);

  t := ppiFix(24);

  lblGridBand := TLabel.Create(gpbGridd);
  lblGridBand.Parent := gpbGridd;
  lblGridBand.Top := t;
  PlaceControl(bd, nil, lblGridBand, ppiFix(LEFT_3COL_1), lblGridBand.Width);
  lblGridBand.Caption := _rsrc(GIS_RS_LEGEND_PRM_GRIDBAND);

  t := t + lblGridBand.Height + ppiFix(4);

  cmbGridBand := TComboBox.Create(gpbGridd);
  cmbGridBand.Parent := gpbGridd;
  cmbGridBand.Style := csDropDownList;
  cmbGridBand.Top := t;
  PlaceControl(bd, nil, cmbGridBand, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  cmbGridBand.Items.BeginUpdate;
  cmbGridBand.Items.Add(_rsrcna(GIS_RS_LEGEND_PRM_DEFAULT));

  for i := 0 to TGIS_LayerPixel(oParentWindow.MVC.Layer).BandsCount - 1 do
    cmbGridBand.Items.Add(IntToStr(i + 1));

  cmbGridBand.ItemIndex := 0;
  cmbGridBand.Items.EndUpdate;
  cmbGridBand.OnChange := doControlChange;

  ckbShadow := TCheckBox.Create(gpbGridd);
  ckbShadow.Parent := gpbGridd;
  ckbShadow.Top := t;
  PlaceControl(bd, nil, ckbShadow, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  ckbShadow.Caption := _rsrc(GIS_RS_LEGEND_PRM_SHADOW);
  ckbShadow.OnClick := doControlChange;

  ckbAntialias := TCheckBox.Create(gpbGridd);
  ckbAntialias.Parent := gpbGridd;
  ckbAntialias.Top := t;
  PlaceControl(bd, nil, ckbAntialias, ppiFix(LEFT_3COL_3 - 20),
    ppiFix(WIDTH_3COL + 20));
  ckbAntialias.Caption := _rsrc(GIS_RS_LEGEND_PRM_ALIASING);
  ckbAntialias.OnClick := doControlChange;

  t := t + ckbAntialias.Height + ppiFix(16);

  lblHeightMin := TLabel.Create(gpbGridd);
  lblHeightMin.Parent := gpbGridd;
  lblHeightMin.Top := t;
  PlaceControl(bd, nil, lblHeightMin, ppiFix(LEFT_3COL_1), lblHeightMin.Width);
  lblHeightMin.Caption := _rsrc(GIS_RS_LEGEND_PRM_MINVAL);

  lblHeightMax := TLabel.Create(gpbGridd);
  lblHeightMax.Parent := gpbGridd;
  lblHeightMax.Top := t;
  PlaceControl(bd, nil, lblHeightMax, ppiFix(LEFT_3COL_2), lblHeightMax.Width);
  lblHeightMax.Caption := _rsrc(GIS_RS_LEGEND_PRM_MAXVAL);

  t := t + lblHeightMax.Height + ppiFix(4);

  edtHeightMin := TEdit.Create(gpbGridd);
  edtHeightMin.Parent := gpbGridd;
  edtHeightMin.Top := t;
  PlaceControl(bd, nil, edtHeightMin, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_3COL));
  edtHeightMin.OnChange := doControlChange;

  vvedtHeightMin := TGIS_ValueValidatorEditHelper.Create(edtHeightMin);
  vvedtHeightMin.MinVal := -GIS_MAX_SINGLE;
  vvedtHeightMin.MaxVal := GIS_MAX_SINGLE;
  vvedtHeightMin.Precision := 15;

  edtHeightMax := TEdit.Create(gpbGridd);
  edtHeightMax.Parent := gpbGridd;
  edtHeightMax.Top := t;
  PlaceControl(bd, nil, edtHeightMax, ppiFix(LEFT_3COL_2), ppiFix(WIDTH_3COL));
  edtHeightMax.OnChange := doControlChange;

  vvedtHeightMax := TGIS_ValueValidatorEditHelper.Create(edtHeightMax);
  vvedtHeightMax.MinVal := -GIS_MAX_SINGLE;
  vvedtHeightMax.MaxVal := GIS_MAX_SINGLE;
  vvedtHeightMax.Precision := 15;

  t := t + edtHeightMax.Height + ppiFix(16);

  gpbGridd.Height := t;

  gpbThreshold := TGroupBox.Create(Self.Panel);
  gpbThreshold.Parent := Self.Panel;
  gpbThreshold.Top := t + ppiFix(4);
  gpbThreshold.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbThreshold, ppiFix(8), ppiFix(WIDTH_NORMAL));
  gpbThreshold.Anchors := Anchors;
  gpbThreshold.Caption := _rsrc(GIS_RS_LEGEND_PRM_HEIGHT_THRESHOLD);

  t := ppiFix(24);

  lblThresholdMin := TLabel.Create(gpbThreshold);
  lblThresholdMin.Parent := gpbThreshold;
  lblThresholdMin.Top := t;
  PlaceControl(bd, nil, lblThresholdMin, ppiFix(LEFT_3COL_1),
    lblThresholdMin.Width);
  lblThresholdMin.Caption := _rsrc(GIS_RS_LEGEND_PRM_MINVAL);

  lblThresholdMax := TLabel.Create(gpbThreshold);
  lblThresholdMax.Parent := gpbThreshold;
  lblThresholdMax.Top := t;
  PlaceControl(bd, nil, lblThresholdMax, ppiFix(LEFT_3COL_2),
    lblThresholdMax.Width);
  lblThresholdMax.Caption := _rsrc(GIS_RS_LEGEND_PRM_MAXVAL);

  t := t + lblThresholdMax.Height + ppiFix(4);

  edtThresholdMin := TEdit.Create(gpbThreshold);
  edtThresholdMin.Parent := gpbThreshold;
  edtThresholdMin.Top := t;
  PlaceControl(bd, nil, edtThresholdMin, ppiFix(LEFT_3COL_1),
    ppiFix(WIDTH_3COL));
  edtThresholdMin.OnChange := doControlChange;

  vvedtThresholdMin := TGIS_ValueValidatorEditHelper.Create(edtThresholdMin);
  vvedtThresholdMin.MinVal := -GIS_MAX_SINGLE;
  vvedtThresholdMin.MaxVal := GIS_MAX_SINGLE;
  vvedtThresholdMin.Precision := 15;

  edtThresholdMax := TEdit.Create(gpbThreshold);
  edtThresholdMax.Parent := gpbThreshold;
  edtThresholdMax.Top := t;
  PlaceControl(bd, nil, edtThresholdMax, ppiFix(LEFT_3COL_2),
    ppiFix(WIDTH_3COL));
  edtThresholdMax.OnChange := doControlChange;

  vvedtThresholdMax := TGIS_ValueValidatorEditHelper.Create(edtThresholdMax);
  vvedtThresholdMax.MinVal := -GIS_MAX_SINGLE;
  vvedtThresholdMax.MaxVal := GIS_MAX_SINGLE;
  vvedtThresholdMax.Precision := 15;

  gpbThreshold.Height := t + edtThresholdMax.Height + ppiFix(16);

  gpbRamp := TGroupBox.Create(Self.Panel);
  gpbRamp.Parent := Self.Panel;
  gpbRamp.Top := gpbThreshold.Top + gpbThreshold.Height + ppiFix(4);
  gpbRamp.Height := ppiFix(512);
  PlaceControl(bd, nil, gpbRamp, ppiFix(8), ppiFix(WIDTH_NORMAL));
  gpbRamp.Anchors := Anchors;
  gpbRamp.Caption := _rsrc(GIS_RS_LEGEND_PAG_RAMP);

  t := ppiFix(24);

  lblMin := TLabel.Create(gpbRamp);
  lblMin.Parent := gpbRamp;
  lblMin.Top := t;
  PlaceControl(bd, nil, lblMin, ppiFix(LEFT_3COL_1) + ppiFix(23 + 8 + 32),
    lblMin.Width);
  lblMin.Caption := _rsrc(GIS_RS_LEGEND_PRM_MIN);

  lblMax := TLabel.Create(gpbRamp);
  lblMax.Parent := gpbRamp;
  lblMax.Top := t;
  PlaceControl(bd, nil, lblMax, ppiFix(LEFT_3COL_1) +
    ppiFix(23 + 8 + 32 + 64 + 16), lblMax.Width);
  lblMax.Caption := _rsrc(GIS_RS_LEGEND_PRM_MAX);

  lblLegend := TLabel.Create(gpbRamp);
  lblLegend.Parent := gpbRamp;
  lblLegend.Top := t;
  PlaceControl(bd, nil, lblLegend, ppiFix(LEFT_3COL_1) +
    ppiFix(23 + 8 + 32 + 64 + 64 + 16 + 16), lblLegend.Width);
  lblLegend.Caption := _rsrc(GIS_RS_LEGEND_PRM_LEGEND);

  t := t + lblMin.Height + ppiFix(4);

  pnlColor := TGIS_ColorPreview.Create(gpbRamp);
  pnlColor.Parent := gpbRamp;
  pnlColor.Top := t;
  pnlColor.Height := ppiFix(21);
  PlaceControl(bd, nil, pnlColor, ppiFix(LEFT_3COL_1) + ppiFix(23 + 8 - 2),
    ppiFix(32));
  pnlColor.CellSize := ppiFix(5);
  pnlColor.OnDialogChange := doControlChange;

  edtMin := TEdit.Create(gpbRamp);
  edtMin.Parent := gpbRamp;
  edtMin.Top := t;
  PlaceControl(bd, pnlColor, edtMin, ppiFix(1), ppiFix(64 + 16));
  edtMin.OnChange := doControlChange;

  vvedtMin := TGIS_ValueValidatorEditHelper.Create(edtMin);
  vvedtMin.MinVal := -GIS_MAX_SINGLE;
  vvedtMin.MaxVal := GIS_MAX_SINGLE;
  vvedtMin.Precision := 15;

  edtMax := TEdit.Create(gpbRamp);
  edtMax.Parent := gpbRamp;
  edtMax.Top := t;
  PlaceControl(bd, edtMin, edtMax, ppiFix(1), ppiFix(64 + 16));
  edtMax.OnChange := doControlChange;

  vvedtMax := TGIS_ValueValidatorEditHelper.Create(edtMax);
  vvedtMax.MinVal := -GIS_MAX_SINGLE;
  vvedtMax.MaxVal := GIS_MAX_SINGLE;
  vvedtMax.Precision := 15;

  edtLegend := TEdit.Create(gpbRamp);
  edtLegend.Parent := gpbRamp;
  edtLegend.Top := t;
  PlaceControl(bd, edtMax, edtLegend, ppiFix(1), ppiFix(128));
  edtLegend.OnChange := doControlChange;

  t := t + pnlColor.Height + ppiFix(4);

  btnAdd := T_speedButton.Create(gpbRamp);
  btnAdd.Parent := gpbRamp;
  btnAdd.Top := t;
  btnAdd.Height := ppiFix(25);
  PlaceControl(bd, nil, btnAdd, ppiFix(LEFT_3COL_1), ppiFix(25));
  btnAdd.OnClick := doAddClick;
  T_speedButton(btnAdd).Images := oParentWindow.imgListSclN;
  T_speedButton(btnAdd).DisabledImages := oParentWindow.imgListSclD;
  T_speedButton(btnAdd).ImageIndex := 0;

  lstGrid := TListBox.Create(gpbRamp);
  lstGrid.Parent := gpbRamp;
  lstGrid.Top := t;
  lstGrid.Height := ppiFix(110);
  PlaceControl(bd, btnAdd, lstGrid, ppiFix(4), pnlColor.Width + edtMin.Width +
    edtMax.Width + edtLegend.Width + 3);
  lstGrid.ParentColor := False;
  lstGrid.Color := clWhite;

  lstGrid.OnDrawItem := lstGridDrawItem;
  lstGrid.OnClick := lstGridClick;
  lstGrid.Style := lbOwnerDrawFixed;

  t := t + btnAdd.Height + ppiFix(8);

  btnRemove := T_speedButton.Create(gpbRamp);
  btnRemove.Parent := gpbRamp;
  btnRemove.Top := t;
  btnRemove.Height := ppiFix(25);
  PlaceControl(bd, nil, btnRemove, ppiFix(LEFT_3COL_1), ppiFix(25));
  btnRemove.OnClick := doDeleteClick;
  T_speedButton(btnRemove).Images := oParentWindow.imgListSclN;
  T_speedButton(btnRemove).DisabledImages := oParentWindow.imgListSclD;
  T_speedButton(btnRemove).ImageIndex := 2;

  t := t + btnRemove.Height + ppiFix(8);

  btnClear := T_speedButton.Create(gpbRamp);
  btnClear.Parent := gpbRamp;
  btnClear.Top := t;
  btnClear.Height := ppiFix(25);
  PlaceControl(bd, nil, btnClear, ppiFix(LEFT_3COL_1), ppiFix(25));
  btnClear.OnClick := doClearClick;
  T_speedButton(btnClear).Images := oParentWindow.imgListSclN;
  T_speedButton(btnClear).DisabledImages := oParentWindow.imgListSclD;
  T_speedButton(btnClear).ImageIndex := 1;

  t := lstGrid.Top + lstGrid.Height + ppiFix(8);

  ckbGridSmoothColors := TCheckBox.Create(gpbRamp);
  ckbGridSmoothColors.Parent := gpbRamp;
  ckbGridSmoothColors.Top := t;
  PlaceControl(bd, nil, ckbGridSmoothColors, lstGrid.Left, ppiFix(WIDTH_2COL));
  ckbGridSmoothColors.Caption := _rsrc(GIS_RS_LEGEND_PRM_GRIDSMOOTHCOLOR);
  ckbGridSmoothColors.OnClick := doControlChange;

  gpbRamp.Height := t + ckbGridSmoothColors.Height + ppiFix(8);

  ckbLegend := TCheckBox.Create(Self.Panel);
  ckbLegend.Parent := Self.Panel;
  ckbLegend.Top := gpbRamp.Top + gpbRamp.Height + ppiFix(4);
  PlaceControl(bd, nil, ckbLegend, ppiFix(LEFT_3COL_1), ppiFix(WIDTH_2COL));
  if bd = bdRightToLeft then
    ckbLegend.Anchors := [akRight, akTop]
  else
    ckbLegend.Anchors := [akLeft, akTop];
  ckbLegend.Caption := _rsrc(GIS_RS_LEGEND_PRM_INCLUDEINLEGEND);
end;

procedure T_panelGrid.PreparePreview(const _viewer: IGIS_Viewer);
begin
  MVC.PreparePreview(_viewer);
end;

procedure T_panelGrid.UpdatePreview;
var
  lp: TGIS_LayerPixel;
  bandchan: Boolean;
begin
  oParentWindow.tmrUpdate.Enabled := False;
  if oParentWindow.gisPreview.IsEmpty then
    exit;
  oParentWindow.tmrUpdate.Enabled := True;

  lp := TGIS_LayerPixel(oParentWindow.gisPreview.Items[0]);
  bandchan := lp.Params.Pixel.GridBand <> MVC.ParamsPixel.GridBand;

  if (lp.Antialias <> MVC.ParamsPixel.Antialias) or
    (lp.Params.Pixel.Page <> MVC.ParamsPixel.Page) or bandchan then
  begin
    // recreate preview
    MVC.PreparePreview(oParentWindow.gisPreview);
    lp := TGIS_LayerPixel(oParentWindow.gisPreview.Items[0]);
  end;

  if bandchan then
  begin
    MVC.ParamsPixel.MinHeightThreshold := -GIS_MAX_SINGLE;
    MVC.ParamsPixel.MaxHeightThreshold := GIS_MAX_SINGLE;
    Read;
  end;

  MVC.SectionWrite(lp.Params);
  lp.Params.Pixel.Antialias := MVC.ParamsPixel.Antialias;

  with lp.Params do
  begin
    MinZoom := 0;
    MaxZoom := GIS_MAX_DOUBLE;
    MinScale := 0;
    MaxScale := GIS_MAX_DOUBLE;
  end;
end;

procedure T_panelGrid.Read;

  function _sameValue(const _a, _b: Single): Boolean;
  var
    epsilon: Single;
  begin
    epsilon := Max(Min(Abs(_a), Abs(_b)) * 1E-7, 1E-7);
    if _a > _b then
      Result := (_a - _b) <= epsilon
    else
      Result := (_b - _a) <= epsilon;
  end;

begin
  lockUpdates;
  try
    lstGrid.Items.BeginUpdate;
    lstGrid.Items.Assign(MVC.ParamsPixel.AltitudeMapZones);
    if lstGrid.Items.Count > 0 then
      lstGrid.ItemIndex := 0;
    lstGrid.Items.EndUpdate;
    lstGridClick(Self);

    ckbLegend.Checked := MVC.ParamsPixel.ShowLegend;
    ckbShadow.Checked := MVC.ParamsPixel.GridShadow;
    ckbAntialias.Checked := MVC.ParamsPixel.Antialias;
    ckbGridSmoothColors.Checked := MVC.ParamsPixel.GridSmoothColors;

    if _sameValue(MVC.ParamsPixel.MinHeightThreshold, -GIS_MAX_SINGLE) and
      _sameValue(MVC.ParamsPixel.MaxHeightThreshold, GIS_MAX_SINGLE) then
    begin
      vvedtThresholdMin.Value := MVC.MinHeight;
      vvedtThresholdMax.Value := MVC.MaxHeight;
      vvedtHeightMin.Value := MVC.MinHeight;
      vvedtHeightMax.Value := MVC.MaxHeight;
    end
    else
    begin
      vvedtThresholdMin.Value := MVC.ParamsPixel.MinHeightThreshold;
      vvedtThresholdMax.Value := MVC.ParamsPixel.MaxHeightThreshold;
      vvedtHeightMin.Value := oParentWindow.MVC.General.HeightMin;
      vvedtHeightMax.Value := oParentWindow.MVC.General.HeightMax;
    end;

    cmbGridBand.Items.BeginUpdate;
    cmbGridBand.ItemIndex := MVC.ParamsPixel.GridBand;
    cmbGridBand.Items.EndUpdate;
  finally
    unlockUpdates;
  end;
end;

procedure T_panelGrid.Write;
begin
  MVC.ParamsPixel.AltitudeMapZones.Assign(lstGrid.Items);
  MVC.ParamsPixel.ShowLegend := ckbLegend.Checked;
  MVC.ParamsPixel.GridShadow := ckbShadow.Checked;
  MVC.ParamsPixel.GridBand := cmbGridBand.ItemIndex;
  MVC.ParamsPixel.MinHeightThreshold := vvedtThresholdMin.Value;
  MVC.ParamsPixel.MaxHeightThreshold := vvedtThresholdMax.Value;
  MVC.ParamsPixel.Antialias := ckbAntialias.Checked;
  MVC.ParamsPixel.GridSmoothColors := ckbGridSmoothColors.Checked;

  oParentWindow.MVC.General.HeightMin := vvedtHeightMin.Value;
  oParentWindow.MVC.General.HeightMax := vvedtHeightMax.Value;
end;

procedure T_panelGrid.lstGridDrawItem(_ctrl: TWinControl; _idx: Integer;
  _rect: TRect; _state: TOwnerDrawState);
var
  R: TRect;
  tkn: TGIS_Tokenizer;
  cl: TColor;
  dmin: Double;
  dmax: Double;
  sleg: String;
  tf: TTextFormat;
  s: String;
begin
  cl := clWhite;
  dmin := 0;
  dmax := 0;
  sleg := '';

  tkn := TGIS_Tokenizer.Create;
  try
    tkn.ExecuteEx(lstGrid.Items[_idx]);

    if tkn.Result.Count > 0 then
      dmin := DotStrToFloat(tkn.Result[0]);
    if tkn.Result.Count > 1 then
      dmax := DotStrToFloat(tkn.Result[1]);
    if tkn.Result.Count > 2 then
      cl := ImitationColor(ParamColor(tkn.Result[2], TGIS_Color.White)).ToBGR;
    if tkn.Result.Count > 3 then
      sleg := tkn.Result[3];
  finally
    FreeObject(tkn);
  end;

  lstGrid.Canvas.FillRect(_rect);

  if BiDiMode = bdRightToLeft then
    tf := [tfRtlReading, tfRight]
  else
    tf := [];

  R := _rect;
  if BiDiMode = bdRightToLeft then
  begin
    R.Right := R.Right - pnlColor.Width - ppiFix(2);
    R.Left := R.Right - edtMin.Width + ppiFix(6);
  end
  else
  begin
    R.Left := edtMin.Left - lstGrid.Left + 1;
    R.Right := edtMax.Left - lstGrid.Left - 4;
  end;
  R.Top := R.Top + 1;
  s := DotFloatToStr(dmin);
  lstGrid.Canvas.TextRect(R, s, tf);

  R := _rect;
  if BiDiMode = bdRightToLeft then
  begin
    R.Right := R.Right - pnlColor.Width - edtMin.Width - ppiFix(3);
    R.Left := R.Right - edtMax.Width + ppiFix(6);
  end
  else
  begin
    R.Left := edtMax.Left - lstGrid.Left + 1;
    R.Right := edtLegend.Left - lstGrid.Left - 4;
  end;
  R.Top := R.Top + 1;
  s := DotFloatToStr(dmax);
  lstGrid.Canvas.TextRect(R, s, tf);

  R := _rect;
  if BiDiMode = bdRightToLeft then
  begin
    R.Right := R.Right - pnlColor.Width - edtMin.Width - edtMax.Width -
      ppiFix(4);
    R.Left := R.Left + 1;
  end
  else
  begin
    R.Left := edtLegend.Left - lstGrid.Left + 1;
  end;
  R.Top := R.Top + 1;
  lstGrid.Canvas.TextRect(R, sleg, tf);

  R := _rect;
  if BiDiMode = bdRightToLeft then
  begin
    R.Right := R.Right - ppiFix(2);
    R.Left := R.Right - pnlColor.Width + ppiFix(4);
  end
  else
  begin
    R.Left := R.Left + ppiFix(2);
    R.Right := edtMin.Left - lstGrid.Left - ppiFix(4);
  end;
  R.Top := R.Top + ppiFix(1);
  R.Bottom := R.Bottom - ppiFix(1);
  lstGrid.Canvas.Brush.Color := cl;
  lstGrid.Canvas.FillRect(R);
end;

procedure T_panelGrid.lstGridClick(_sender: TObject);
var
  tkn: TGIS_Tokenizer;
  pos: Integer;
  d: Double;
begin
  if lstGrid.ItemIndex < 0 then
  begin
    vvedtMin.Value := 0;
    vvedtMax.Value := 0;
    edtMin.Text := '';
    edtMax.Text := '';
    pnlColor.Color := TGIS_Color.White;
    edtLegend.Text := '';
    exit;
  end;

  tkn := TGIS_Tokenizer.Create;
  try
    lockChange := True;
    tkn.ExecuteEx(lstGrid.Items[lstGrid.ItemIndex]);

    if tkn.Result.Count > 0 then
    begin
      d := DotStrToFloat(tkn.Result[0]);
      edtMin.Text := DotFloatToStr(d);
    end
    else
      edtMin.Text := '0';

    if tkn.Result.Count > 1 then
    begin
      d := DotStrToFloat(tkn.Result[1]);
      edtMax.Text := DotFloatToStr(d);
    end
    else
      edtMax.Text := '0';

    if tkn.Result.Count > 2 then
      pnlColor.Color := ParamColor(tkn.Result[2], TGIS_Color.White)
    else
      pnlColor.Color := TGIS_Color.White;

    if tkn.Result.Count > 3 then
      edtLegend.Text := tkn.Result[3]
    else
      edtLegend.Text := '';
  finally
    lockChange := False;
    FreeObject(tkn);
  end;
end;

procedure T_panelGrid.doAddClick(_sender: TObject);
begin
  lstGrid.Items.BeginUpdate;
  if lstGrid.ItemIndex >= 0 then
  begin
    lstGrid.Items.Insert(lstGrid.ItemIndex, lstGrid.Items[lstGrid.ItemIndex]);
    lstGrid.ItemIndex;
  end
  else
  begin
    lstGrid.Items.Add('');
    lstGrid.ItemIndex := 0;
  end;
  lstGrid.Items.EndUpdate;
  doUpdate(_sender);
  lstGridClick(_sender);
  doControlChange(_sender);
end;

procedure T_panelGrid.doDeleteClick(_sender: TObject);
var
  idx: Integer;
begin
  idx := lstGrid.ItemIndex;

  if idx >= 0 then
    lstGrid.Items.Delete(idx);

  doUpdate(_sender);
  lstGrid.ItemIndex := Min(idx, lstGrid.Items.Count - 1);
  lstGridClick(_sender);
  doControlChange(_sender);
end;

procedure T_panelGrid.doClearClick(_sender: TObject);
begin
  lstGrid.Clear;
  doUpdate(_sender);
  doControlChange(_sender);
end;

procedure T_panelGrid.doControlChange(_sender: TObject);
begin
  if not lockChange then
    MVC.doControlChange;
end;

procedure T_panelGrid.doUpdate(_sender: TObject);
var
  stmp: String;
begin
  if lstGrid.ItemIndex >= 0 then
  begin
    stmp := DotFloatToStr(vvedtMin.Value) + ',' + DotFloatToStr(vvedtMax.Value)
      + ',' + ConstructParamColor(pnlColor.Color);

    if not IsStringEmpty(Trim(edtLegend.Text)) then
      stmp := stmp + ',' + Trim(edtLegend.Text);
    lstGrid.Items[lstGrid.ItemIndex] := stmp;
  end;
end;

procedure T_panelGrid.doCallback(_sender: TObject; _code: Integer);
begin
  case _code of
    0:
      doUpdate(_sender);
  end;

  if not blockedUpdates then
  begin
    Write;
    UpdatePreview;
  end;
end;

{$ENDREGION}
{$REGION 'TGIS_ControlLegendForm'}

destructor TGIS_ControlLegendForm.Destroy;
begin
  FreeObject(gisPreview);
  FreeObject(lstFeatures);
  FreeObject(ImgList);
  FreeObject(imgListSclN);
  FreeObject(imgListSclD);
  FreeObject(MVC);

  inherited;
end;

procedure TGIS_ControlLegendForm.initForm;
begin
  Self.ClientWidth := 550;
  Self.ClientHeight := 490;
  Self.Name := 'TGIS_ControlLegendForm';
  Self.Caption := Self.Name;

  lockLevel := 0;
end;

procedure TGIS_ControlLegendForm.initControls;
begin
  initSelf;
  initLeft;
end;

procedure TGIS_ControlLegendForm.DoShow;
begin
  iLastPPI := 0;

  doChangeDPI;

  inherited;
end;

procedure TGIS_ControlLegendForm.doChangeDPI;
var
  rnd: TGIS_RendererAbstract;
begin
{$IFDEF LEVEL_RX101_VCL}
  if not assigned(imgListSclN) then
    exit;
  if not assigned(MVC) then
    exit;
{$ENDIF}
  // resize image list only if required
  if (imgListSclN.Count <= 0) or (iLastPPI <> CurrentPPI) then
  begin
    GisScaleImageList(ImgList, imgListSclN, CurrentPPI, 96);
    GisScaleImageList(ImgList, imgListSclD, CurrentPPI, 96, True);
    toolButtonsU.ButtonHeight := imgListSclN.Height;
    toolButtonsU.ButtonWidth := imgListSclN.Width;
    toolButtonsD.ButtonHeight := imgListSclN.Height;
    toolButtonsD.ButtonWidth := imgListSclN.Width;
  end;

  // resize preview only if required
  if (gisPreviewI.Width <> gisPreview.GIS_Bitmap.Width) or
    (gisPreviewI.Height <> gisPreview.GIS_Bitmap.Height) or
    (iLastPPI <> CurrentPPI) then
  begin
    gisPreview.SetSize(gisPreviewI.Width, gisPreviewI.Height);
    gisPreview.CustomPPI := MVC.Layer.Viewer.Ref.PPI;
    rnd := gisPreview.Renderer.CreateInstance;
    gisPreview.Renderer := rnd;
    gisPreviewI.Picture.Assign(nil);
    gisPreview.FullExtent;
    tmrUpdate.Enabled := True;
  end;

  iLastPPI := CurrentPPI;
end;

{$IFDEF LEVEL_RX101_VCL}

procedure TGIS_ControlLegendForm.WMDpiChanged(var _message: TWMDpi);
begin
  inherited;
  doChangeDPI;
end;
{$ENDIF}

procedure TGIS_ControlLegendForm.Paint;
begin
{$IFNDEF LEVEL_RX101_VCL}
  doChangeDPI;
{$ENDIF}
  inherited;
end;

function TGIS_ControlLegendForm.Execute(const _layer: TGIS_Layer;
  const _legend: TGIS_ControlLegend; const _onhelp: TGIS_HelpEvent): Integer;
begin
  if not assigned(_layer) then
  begin
    Result := mrNone;
    exit;
  end;

  Assert(assigned(_layer.Viewer));

  oLegend := _legend;
  oViewer := _layer.Viewer.Ref;

  oViewerWnd := TGIS_ViewerWnd( oViewer.ViewerParentRoot );

  pOnHelp := _onhelp;
  btnHelp.Visible := assigned(pOnHelp);

  MVC := TGIS_ControlLegendFormMVC.Create(_layer);
  if MVC.IsVector then
    Self.Caption := Format(_rsrc(GIS_RS_LEGEND_DLGVECTOR), [MVC.Layer.Caption])
  else if MVC.IsPixel then
    Self.Caption := Format(_rsrc(GIS_RS_LEGEND_DLGPIXEL), [MVC.Layer.Caption])
  else
    Self.Caption := Format(_rsrc(GIS_RS_LEGEND_OTHER), [MVC.Layer.Caption]);

  initRight(False);
  tmrShowForm.Enabled := True;

  ActiveControl := btnCancel;

  Result := ShowModal;
end;

function TGIS_ControlLegendForm.Execute(const _layer: TGIS_Layer;
  const _legend: TGIS_ControlLegend): Integer;
var
  hlp: TGIS_HelpEvent;
begin
  hlp := nil;
  Result := Execute(_layer, _legend, hlp);
end;

procedure TGIS_ControlLegendForm.readLayer(const _full: Boolean);
var
  nodes: TTreeNodes;
  node: TTreeNode;
  i: Integer;
  k: Integer;
begin
  tvPages.Items.BeginUpdate;
  try
    nodes := tvPages.Items;
    if not _full then
    begin
      nodes.Add(nil, T_scrollablePanel(pnlGeneral).ItemText);
      node := nodes.Add(nil, T_scrollablePanel(pnlSections).ItemText);

      exit;
    end;

    node := nodes[nodes.Count - 1];

    lstFeatures := T_sectionContainer.Create(Self);

    for k := 0 to MVC.SectionCount - 1 do
    begin

      MVC.SectionIndex := k;
      T_sectionContainer(lstFeatures).ReadSection;

      for i := 0 to T_sectionContainer(lstFeatures).Count - 1 do
      begin
        if i = 0 then
          node := nodes.AddChild(node, T_sectionContainer(lstFeatures)
            .Panel[i].ItemText)
        else
          nodes.AddChild(node, T_sectionContainer(lstFeatures)
            .Panel[i].ItemText);
      end;

      node := node.Parent;
    end;

    node.Expand(False);
    node.Item[0].Expand(False);
  finally
    tvPages.Items.EndUpdate;
  end;
end;

procedure TGIS_ControlLegendForm.setSection(const _idx: Integer;
  const _fea: Boolean);
var
  node: TTreeNode;
  i: Integer;
begin
  T_sectionContainer(lstFeatures).Write;
  MVC.SectionIndex := _idx;
  T_sectionContainer(lstFeatures).Read;

  if _fea then
    exit;

  tvPages.Items.BeginUpdate;
  try
    i := 0;
    node := tvPages.Items.Item[GIS_CONTAINER_INDEX].Item[0];
    while node <> nil do
    begin
      if i <> _idx then
        node.Collapse(False)
      else
        node.Expand(False);
      node := node.getNextSibling;
      Inc(i)
    end;
  finally
    tvPages.Items.EndUpdate;
  end;
end;

procedure TGIS_ControlLegendForm.fillComboBoxWithFields(const _cmb: TComboBox);
var
  prnt: TWinControl;
begin
  _cmb.Items.BeginUpdate;
  _cmb.Items.Assign(MVC.FieldNames);
  _cmb.Items.EndUpdate;
end;

procedure TGIS_ControlLegendForm.showPreview(const _show: Boolean);
begin
  if _show then
  begin
    if gisPreviewI.Visible then
      exit;
    tvPages.Height := tvPages.Height - tvPages.Width
  end
  else
  begin
    if not gisPreviewI.Visible then
      exit;
    tvPages.Height := tvPages.Height + tvPages.Width;
  end;

  gisPreviewI.Visible := _show;
end;

procedure TGIS_ControlLegendForm.lock;
begin
  bLock := True;
end;

procedure TGIS_ControlLegendForm.unlock;
begin
  bLock := False;
end;

function TGIS_ControlLegendForm.isLocked: Boolean;
begin
  Result := bLock;
end;

procedure TGIS_ControlLegendForm.waitNoBusy;
begin
  // what until all pending updates will be finished
  repeat
    Application.ProcessMessages;
  until not tmrUpdate.Enabled;

  repeat
    Application.ProcessMessages;
  until iBusy = 0;
end;

procedure TGIS_ControlLegendForm.updateStatistics(const _layer: TGIS_Layer);
var
  frm_stats: TGIS_ControlStatistics;
begin
  if not _layer.MustCalculateStatistics then
    exit;

  frm_stats := TGIS_ControlStatistics.Create(Self);
  try
    frm_stats.Execute(_layer, pOnHelp);
  finally
    FreeObject(frm_stats);
  end;
end;

function TGIS_ControlLegendForm.ppiFix(const _value: Integer): Integer;
begin
  Result := MulDiv(_value, CurrentPPI, 96);
end;

procedure TGIS_ControlLegendForm.initSelf;
var
  anchorsB: TAnchors;
  anchorsRB: TAnchors;
  bmp: TBitmap;

  procedure loadRes(const _name: String);
  var
    rstm: TResourceStream;
  begin
    rstm := TResourceStream.Create(hInstance, _name, RT_RCDATA);
    try
      bmp.LoadFromStream(rstm);
      ImgList.AddMasked(bmp, bmp.TransparentColor);
      ImgList.DrawingStyle := TDrawingStyle.dsTransparent;
    finally
      FreeObject(rstm);
    end;
  end;

begin
  ImgList := TImageList.Create(Self);
  imgListSclN := TImageList.Create(Self);
  imgListSclD := TImageList.Create(Self);

  bmp := TBitmap.Create;
  try
    loadRes('TGIS_LEGENDFORMIMAGE_ADD');
    loadRes('TGIS_LEGENDFORMIMAGE_CLEAR');
    loadRes('TGIS_LEGENDFORMIMAGE_DELETE');
    loadRes('TGIS_LEGENDFORMIMAGE_BTNDOWN');
    loadRes('TGIS_LEGENDFORMIMAGE_BTNUP');
    loadRes('TGIS_LEGENDFORMIMAGE_OPEN');
    loadRes('TGIS_LEGENDFORMIMAGE_SAVE');
  finally
    FreeObject(bmp);
  end;

  pnlBottom := TPanel.Create(Self);
  pnlBottom.Parent := Self;
  pnlBottom.BevelOuter := bvNone;
  pnlBottom.Align := alBottom;
  pnlBottom.Height := 40;
  pnlBottom.Anchors := [akLeft, akBottom, akRight];

  if BiDiMode = bdRightToLeft then
  begin
    anchorsB := [akRight, akBottom];
    anchorsRB := [akLeft, akBottom];
  end
  else
  begin
    anchorsB := [akLeft, akBottom];
    anchorsRB := [akRight, akBottom];
  end;

  btnWizard := TButton.Create(pnlBottom);
  btnWizard.Parent := pnlBottom;
  btnWizard.Height := btnHelp.Height;
  btnWizard.Top := pnlBottom.Height - btnWizard.Height - 8;
  PlaceControl(BiDiMode, nil, btnWizard, 8, 80);
  btnWizard.Anchors := anchorsB;
  btnWizard.Caption := _rsrc(GIS_RS_BTN_WIZARD);
  btnWizard.OnClick := doWizardClick;
  btnWizard.ImageIndex := 5;

  btnHelp.Parent := pnlBottom;
  btnHelp.Top := pnlBottom.Height - btnHelp.Height - 8;
  PlaceControl(BiDiMode, btnWizard, btnHelp, 8, 80);

  btnApply := TButton.Create(pnlBottom);
  btnApply.Parent := pnlBottom;
  btnApply.Height := btnHelp.Height;
  btnApply.Top := pnlBottom.Height - btnApply.Height - 8;
  PlaceControl(BiDiMode, nil, btnApply, -8, 80);
  btnApply.Anchors := anchorsRB;
  btnApply.Caption := _rsrc(GIS_RS_BTN_APPLY);
  btnApply.OnClick := doApplySettings;

  btnCancel.Parent := pnlBottom;
  btnCancel.Top := pnlBottom.Height - btnCancel.Height - 8;
  PlaceControl(BiDiMode, btnApply, btnCancel, -8, 80);

  btnOK.Parent := pnlBottom;
  btnOK.Top := pnlBottom.Height - btnOK.Height - 8;
  PlaceControl(BiDiMode, btnCancel, btnOK, -8, 80);

  btnWizard.TabOrder := 0;
  btnHelp.TabOrder := 1;
  btnOK.TabOrder := 2;
  btnCancel.TabOrder := 3;
  btnApply.TabOrder := 4;
end;

procedure TGIS_ControlLegendForm.initLeft;
var
  t: Integer;
begin
  pnlLeft := TPanel.Create(Self);
  pnlLeft.Parent := Self;
  if BiDiMode = bdRightToLeft then
    pnlLeft.Align := alRight
  else
    pnlLeft.Align := alLeft;
  pnlLeft.Top := 0;
  pnlLeft.Width := 130;
  pnlLeft.Height := Self.ClientHeight - 32;
  if BiDiMode = bdRightToLeft then
    pnlLeft.Anchors := [akRight, akTop, akBottom]
  else
    pnlLeft.Anchors := [akLeft, akTop, akBottom];
  pnlLeft.Name := 'pnlLeft';
  TPanel(pnlLeft).BevelOuter := bvNone;

  with TSplitter.Create(Self) do
  begin
    Parent := Self;
  end;

  if BiDiMode = bdRightToLeft then
    pnlLeft.Left := Self.ClientWidth - 130
  else
    pnlLeft.Left := 0;

  t := 0;

  tvPages := TTreeView.Create(pnlLeft);
  tvPages.Parent := pnlLeft;
  tvPages.Left := 0;
  tvPages.Top := 0;
  tvPages.Align := alClient;
  tvPages.Width := pnlLeft.ClientWidth;
  tvPages.Height := pnlLeft.ClientHeight - 24;
  tvPages.Anchors := [akLeft, akTop, akRight, akBottom];
  tvPages.ReadOnly := True;
  tvPages.Font.Name := 'Tahoma';
  tvPages.ShowButtons := True;
  tvPages.ShowRoot := False;
  tvPages.ShowLines := False;
  tvPages.Indent := 12;
  tvPages.BorderStyle := bsNone;
  tvPages.HideSelection := False;
  tvPages.RowSelect := True;

  t := t + tvPages.Height;

  gisPreview := TGIS_ViewerBmp.Create(tvPages.Width, tvPages.Width);

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

  gisPreviewI := TImage.Create(pnlLeft);
  gisPreviewI.Parent := pnlLeft;
  gisPreviewI.Left := 0;
  gisPreviewI.Top := t - tvPages.Width;
  gisPreviewI.Width := tvPages.Width;
  gisPreviewI.Height := tvPages.Width;
  gisPreviewI.Anchors := [akLeft, akRight, akBottom];
  gisPreviewI.Visible := False;
  gisPreviewI.Align := alBottom;
  gisPreviewI.Name := 'gisPreviewI';

  tmrUpdate := TTimer.Create(Self);
  tmrUpdate.Interval := 500;
  tmrUpdate.OnTimer := doUpdateTimer;
  tmrUpdate.Enabled := False;

  tmrShowForm := TTimer.Create(Self);
  tmrShowForm.Interval := 1;
  tmrShowForm.OnTimer := doShowTimer;
  tmrShowForm.Enabled := False;

  toolButtonsU := TToolBar.Create(pnlLeft);
  toolButtonsU.Parent := pnlLeft;
  toolButtonsU.Align := alTop;
  toolButtonsU.Name := 'toolButtons';
  toolButtonsU.AutoSize := True;
  toolButtonsU.Images := imgListSclN;
  toolButtonsU.DisabledImages := imgListSclD;
  toolButtonsU.DrawingStyle := TTBDrawingStyle.dsNormal;
  toolButtonsU.ShowHint := True;

  btnMoveUp := TToolButton.Create(toolButtonsU);
  btnMoveUp.Parent := toolButtonsU;
  btnMoveUp.OnClick := doMoveUpClick;
  btnMoveUp.ImageIndex := 4;

  btnMoveDown := TToolButton.Create(toolButtonsU);
  btnMoveDown.Parent := toolButtonsU;
  btnMoveDown.OnClick := doMoveDownClick;
  btnMoveDown.ImageIndex := 3;

  btnClear := TToolButton.Create(toolButtonsU);
  btnClear.Parent := toolButtonsU;
  btnClear.OnClick := doClearClick;
  btnClear.ImageIndex := 1;

  btnRemove := TToolButton.Create(toolButtonsU);
  btnRemove.Parent := toolButtonsU;
  btnRemove.OnClick := doRemoveClick;
  btnRemove.ImageIndex := 2;

  btnAdd := TToolButton.Create(toolButtonsU);
  btnAdd.Parent := toolButtonsU;
  btnAdd.OnClick := doAddClick;
  btnAdd.ImageIndex := 0;

  toolButtonsD := TToolBar.Create(pnlLeft);
  toolButtonsD.Parent := pnlLeft;
  toolButtonsD.Align := alBottom;
  toolButtonsD.Name := 'toolButtons2';
  toolButtonsD.AutoSize := True;
  toolButtonsD.Images := imgListSclN;
  toolButtonsD.DisabledImages := imgListSclD;
  toolButtonsD.DrawingStyle := TTBDrawingStyle.dsNormal;
  toolButtonsD.ShowHint := True;

  btnSave := TToolButton.Create(toolButtonsD);
  btnSave.Parent := toolButtonsD;
  btnSave.OnClick := doSaveClick;
  btnSave.ImageIndex := 6;

  btnOpen := TToolButton.Create(toolButtonsD);
  btnOpen.Parent := toolButtonsD;
  btnOpen.OnClick := doOpenClick;
  btnOpen.ImageIndex := 5;

  dlgOpen := TOpenDialog.Create(Self);
  dlgOpen.Filter := _rsrc(GIS_RS_LEGEND_CONFIG_FILTER_OPEN);

  dlgSave := TSaveDialog.Create(Self);
  dlgSave.Filter := _rsrc(GIS_RS_LEGEND_CONFIG_FILTER_SAVE);
end;

procedure TGIS_ControlLegendForm.initRight(const _full: Boolean);
begin
  if not _full then
  begin

    pnlRight := T_scrollablePanel.Create(Self, Self);
    pnlRight.Parent := Self;
    if BiDiMode = bdRightToLeft then
      pnlRight.Left := 0
    else
      pnlRight.Left := pnlLeft.Width;
    pnlRight.Top := 0;
    pnlRight.Width := Self.ClientWidth - pnlLeft.Width;
    pnlRight.Height := Self.ClientHeight - 32;
    pnlRight.Anchors := [akLeft, akTop, akRight, akBottom];
    pnlRight.Align := alClient;
    pnlRight.Name := 'pnlRight';

    pnlGeneral := T_panelGeneral.Create(pnlRight, Self);
    pnlGeneral.Left := 0;
    pnlGeneral.Top := 0;
    pnlGeneral.Width := pnlRight.ClientWidth;
    pnlGeneral.Height := pnlRight.ClientHeight;
    pnlGeneral.Anchors := [akLeft, akTop, akRight, akBottom];
    pnlGeneral.Visible := False;
    pnlGeneral.Name := 'pnlGeneral';
    TPanel(pnlGeneral).Align := TAlign.alClient;
    TPanel(pnlGeneral).Caption := '';

    pnlSections := T_panelSections.Create(pnlRight, Self);
    pnlSections.Left := 0;
    pnlSections.Top := 0;
    pnlSections.Width := pnlRight.ClientWidth;
    pnlSections.Height := pnlRight.ClientHeight;
    pnlSections.Anchors := [akLeft, akTop, akRight, akBottom];
    pnlSections.Visible := False;
    TPanel(pnlSections).Align := TAlign.alClient;
    pnlSections.Name := 'pnlSections';
    TPanel(pnlSections).Caption := '';

    pnlTop := TPanel.Create(pnlRight);
    pnlTop.Parent := pnlRight;
    pnlTop.Height := 18;
    pnlTop.Visible := True;
    pnlTop.Align := TAlign.alTop;
    pnlTop.BevelOuter := bvNone;

  end;

  readLayer(_full);

  pnlCurrent := pnlGeneral;
  pnlCurrent.Visible := True;

  tvPages.OnChange := doTvPagesOnChange;
end;

procedure TGIS_ControlLegendForm.doShowTimer;
begin
  tmrShowForm.Enabled := False;
  lockWindow;
  try
    initRight(True);
  finally
    unlockWindow;
  end;
end;

procedure TGIS_ControlLegendForm.doPageChange(_node: TTreeNode);
var
  prnt: TTreeNode;
  i: Integer;
begin
  if isLocked then
    exit;

  if assigned(oViewerWnd) then
  begin
    gisPreview.Renderer := oViewerWnd.Renderer.CreateInstance;
    gisPreview.CustomPPI := oViewerWnd.PPI;
    if oViewerWnd.Color = clWindow then
      gisPreview.Color := GISColor(StyleServices.GetSystemColor(clWindow))
    else
      gisPreview.Color := GISColor(oViewerWnd.Color);
  end;

  lockWindow;
  try
    prnt := _node.Parent;

    pnlCurrent.Visible := False;
    pnlTop.Visible := False;

    // --- General, Sections ---//

    if not assigned(prnt) then
    begin

      btnRemove.Enabled := False;
      btnMoveUp.Enabled := False;
      btnMoveDown.Enabled := False;

      case _node.Index of
        0:
          pnlCurrent := pnlGeneral;
        1:
          pnlCurrent := pnlSections;
        2:
          begin
            lastSection := '';
            lastNodeIdx := 0;
            doPageChange(_node.Item[0]);
            exit;
          end;
      end;

      pnlCurrent.Visible := True;

      // prepare list
      if pnlCurrent = pnlSections then
        T_panelSections(pnlSections).Prepare;

      if T_scrollablePanel(pnlCurrent).HasPreview then
      begin
        T_scrollablePanel(pnlCurrent).PreparePreview(gisPreview);
        T_scrollablePanel(pnlCurrent).UpdatePreview;
      end;

      showPreview(T_scrollablePanel(pnlCurrent).HasPreview);

      tvPages.Selected := _node;

      exit;
    end;

    // --- sections ---//

    if (not assigned(prnt.Parent)) and (prnt.Index = GIS_CONTAINER_INDEX) then
    begin

      btnRemove.Enabled := MVC.SectionCount <> 1;
      btnMoveUp.Enabled := _node.Index <> 0;
      btnMoveDown.Enabled := _node.Index <> prnt.Count - 1;

      setSection(_node.Index, False);

      if not IsStringEmpty(lastSection) and (_node.Index <> lastNodeIdx) then
      begin
        for i := 0 to _node.Count - 1 do
          if _node[i].Text = lastSection then
          begin
            lastNodeIdx := _node.Index;
            doPageChange(_node[i]);
            exit;
          end;
      end
      else
        tvPages.Selected := _node;

      pnlCurrent := T_sectionContainer(lstFeatures).Panel[0];
      pnlTop.Visible := True;
      pnlCurrent.Visible := True;

      if T_scrollablePanel(pnlCurrent).HasPreview then
      begin
        T_scrollablePanel(pnlCurrent).PreparePreview(gisPreview);
        T_scrollablePanel(pnlCurrent).UpdatePreview;
      end;

      showPreview(T_scrollablePanel(pnlCurrent).HasPreview);
      lastNodeIdx := _node.Index;
      lastSection := '';

      exit;
    end;

    // --- sections' internals ---//

    if prnt.Index <> MVC.SectionIndex then
      setSection(prnt.Index, True);

    btnRemove.Enabled := False;
    btnMoveUp.Enabled := False;
    btnMoveDown.Enabled := False;

    pnlCurrent := T_sectionContainer(lstFeatures).FindPanel(_node.Text);
    pnlCurrent.Visible := True;
    pnlTop.Visible := True;

    lastSection := _node.Text;

    if T_scrollablePanel(pnlCurrent).HasPreview then
    begin
      T_scrollablePanel(pnlCurrent).PreparePreview(gisPreview);
      T_scrollablePanel(pnlCurrent).UpdatePreview;
    end;

    showPreview(T_scrollablePanel(pnlCurrent).HasPreview);

    tvPages.Selected := _node;
  finally
    unlockWindow;
  end;
end;

procedure TGIS_ControlLegendForm.doTvPagesOnChange(_sender: TObject;
  _node: TTreeNode);
begin
  doPageChange(_node);
end;

procedure TGIS_ControlLegendForm.doUpdateTimer(_sender: TObject);
begin
  Inc(iBusy);
  try
    tmrUpdate.Enabled := False;
    gisPreviewI.Visible := True;

    gisPreview.ControlUpdateWholeMap;

    gisPreviewI.Picture.Assign( TPersistent(gisPreview.GIS_Bitmap.NativeBitmap) ) ;
  finally
    Dec(iBusy);
  end;
end;

procedure TGIS_ControlLegendForm.doApplySettings(_sender: TObject);
var
  last_index: Integer;
  old_agg: Boolean;
  new_agg: Boolean;
begin
  waitNoBusy;
  last_index := MVC.SectionIndex;

  if MVC.Layer.IsVector then
    old_agg := assigned(TGIS_LayerVector(MVC.Layer).DynamicAggregator)
  else
    old_agg := False;

  T_panelGeneral(pnlGeneral).Write;
  T_sectionContainer(lstFeatures).Write;
  MVC.Write;
  MVC.SectionIndex := last_index;

  updateStatistics(MVC.Layer);

  if MVC.Layer.IsVector then
  begin
    new_agg := assigned(TGIS_LayerVector(MVC.Layer).DynamicAggregator);
    if new_agg or (new_agg <> old_agg) then
      doSectionsRebuild;
  end;

  if assigned(oLegend) then
  begin
    if assigned(oLegend.LayerParamsChangeEvent) then
      oLegend.LayerParamsChangeEvent(oLegend, MVC.Layer);

    if assigned(oViewerWnd) then
      oViewerWnd.InvalidateWholeMap;
  end;
end;

procedure TGIS_ControlLegendForm.btnOKClick(_sender: TObject);
begin
  inherited;
  waitNoBusy;
  T_panelGeneral(pnlGeneral).Write;
  T_sectionContainer(lstFeatures).Write;
  MVC.Write;

  updateStatistics(MVC.Layer);
end;

procedure TGIS_ControlLegendForm.doAddClick(_sender: TObject);
var
  node: TTreeNode;
  i: Integer;
  ntext: String;
begin
  MVC.SectionAdd(True);

  ntext := Format('%s %d', [_rsrc(GIS_RS_LEGEND_PAG_SECTION),
    MVC.SectionCount]);

  tvPages.Items.BeginUpdate;
  try
    node := tvPages.Items.Item[GIS_CONTAINER_INDEX];
    for i := 0 to T_sectionContainer(lstFeatures).Count - 1 do
    begin
      if i = 0 then
        node := tvPages.Items.AddChild(node, ntext)
      else
        tvPages.Items.AddChild(node, T_sectionContainer(lstFeatures)
          .Panel[i].ItemText);
    end;
  finally
    tvPages.Items.EndUpdate;
  end;

  doPageChange(node);
end;

procedure TGIS_ControlLegendForm.doRemoveClick(_sender: TObject);
var
  node: TTreeNode;
begin
  lock;
  try
    tvPages.Items.BeginUpdate;
    try
      if MVC.SectionCount <> 1 then
      begin
        node := tvPages.Items[GIS_CONTAINER_INDEX].Item[MVC.SectionIndex];
        node.Delete;
      end;

      MVC.SectionDelete;
      T_sectionContainer(lstFeatures).Read;
    finally
      tvPages.Items.EndUpdate;
    end;
  finally
    unlock;
  end;

  doPageChange(tvPages.Items[GIS_CONTAINER_INDEX].Item[MVC.SectionIndex]);
end;

procedure TGIS_ControlLegendForm.doClearClick(_sender: TObject);
var
  node: TTreeNode;
  i: Integer;
  ntext: String;
begin
  lock;
  try
    MVC.SectionClear;
    node := tvPages.Items.Item[GIS_CONTAINER_INDEX];
    tvPages.Items.BeginUpdate;
    try
      for i := node.Count - 1 downto 1 do
        node.Item[i].Delete;
    finally
      tvPages.Items.EndUpdate;
    end;

    ntext := Format('%s %d', [_rsrc(GIS_RS_LEGEND_PAG_SECTION),
      MVC.SectionCount]);
    if T_sectionContainer(lstFeatures).Count > 0 then
    begin
      T_sectionContainer(lstFeatures).Panel[0].ItemText := ntext;
      node.Item[0].Text := ntext;
    end;
    T_sectionContainer(lstFeatures).Read;
  finally
    unlock;
  end;
  doPageChange(node.Item[0]);
end;

procedure TGIS_ControlLegendForm.doMoveUpClick(_sender: TObject);
var
  node: TTreeNode;
begin
  if MVC.SectionIndex = 0 then
    exit;

  MVC.SectionMove(True);
  MVC.SectionIndex := MVC.SectionIndex - 1;

  tvPages.Items.BeginUpdate;
  try
    node := tvPages.Items[GIS_CONTAINER_INDEX].Item[MVC.SectionIndex + 1];
    node.MoveTo(node.getPrevSibling, TNodeAttachMode.naInsert);
  finally
    tvPages.Items.EndUpdate;
  end;

  doPageChange(tvPages.Items[GIS_CONTAINER_INDEX].Item[MVC.SectionIndex]);
end;

procedure TGIS_ControlLegendForm.doMoveDownClick(_sender: TObject);
var
  node: TTreeNode;
begin
  if MVC.SectionIndex = MVC.SectionCount - 1 then
    exit;

  MVC.SectionMove(False);
  MVC.SectionIndex := MVC.SectionIndex + 1;

  tvPages.Items.BeginUpdate;
  try
    node := tvPages.Items[GIS_CONTAINER_INDEX].Item[MVC.SectionIndex];
    node.MoveTo(node.getPrevSibling, TNodeAttachMode.naInsert);
  finally
    tvPages.Items.EndUpdate;
  end;

  doPageChange(tvPages.Items[GIS_CONTAINER_INDEX].Item[MVC.SectionIndex]);
end;

procedure TGIS_ControlLegendForm.doOpenClick(_sender: TObject);
var
  desc: String;
  i, k: Integer;
  node: TTreeNode;
begin
  if dlgOpen.Execute then
  begin
    MVC.PrepareLayerEx;
    try
      lock;
      try
        for i := MVC.SectionCount - 1 downto 0 do
          MVC.SectionDelete;
        tvPages.Items.BeginUpdate;
        try
          node := tvPages.Items.Item[GIS_CONTAINER_INDEX];
          for i := node.Count - 1 downto 0 do
            node.Item[i].Delete;

          MVC.SwitchToLayerEx;
          MVC.LoadConfig(dlgOpen.FileName);

          for k := 0 to MVC.LayerEx.ParamsList.Count - 1 do
          begin
            MVC.SectionAdd(False);
            for i := 0 to T_sectionContainer(lstFeatures).Count - 1 do
            begin
              if i = 0 then
              begin
                // set to the section to get the section description
                MVC.SectionIndex := k;
                if not IsStringEmpty(MVC.Section.Legend) then
                  desc := MVC.Section.Legend
                else
                  desc := Format('%s %d', [_rsrc(GIS_RS_LEGEND_PAG_SECTION),
                    MVC.SectionCount]);
                node := tvPages.Items.AddChild(node, desc);
              end
              else
                tvPages.Items.AddChild(node, T_sectionContainer(lstFeatures)
                  .Panel[i].ItemText);
            end;
            node := node.Parent;
          end;
        finally
          MVC.SectionIndex := 0;
          tvPages.Items.EndUpdate;
        end;
        T_panelGeneral(pnlGeneral).MVC.Read;
        T_panelGeneral(pnlGeneral).Read;
        T_sectionContainer(lstFeatures).Read;
      finally
        unlock;
        MVC.SwitchToLayer;
      end;
      doPageChange(tvPages.Items[GIS_CONTAINER_INDEX]);
    finally
      MVC.UnPrepareLayerEx;
    end;
  end;
end;

procedure TGIS_ControlLegendForm.doSaveClick(_sender: TObject);
begin
  if IsServerPath(MVC.Layer.Path) then
    dlgSave.FileName := MVC.Layer.Name + GIS_TTKSTYLE_EXT
  else
    dlgSave.FileName := MVC.Layer.Path + GIS_TTKSTYLE_EXT;

  dlgSave.DefaultExt := GIS_TTKSTYLE_EXT;
  if dlgSave.Execute then
  begin
    MVC.PrepareLayerEx;
    try
      MVC.SwitchToLayerEx;
      try
        MVC.SaveConfig(dlgSave.FileName);
      finally
        MVC.SwitchToLayer;
      end;
    finally
      MVC.UnPrepareLayerEx;
    end;
  end;
end;

procedure TGIS_ControlLegendForm.doSectionsRebuild;
var
  i, k: Integer;
  desc: String;
  node: TTreeNode;
begin
  lock;
  try
    for i := MVC.SectionCount - 1 downto 0 do
      MVC.SectionDelete;

    tvPages.Items.BeginUpdate;
    try
      node := tvPages.Items.Item[GIS_CONTAINER_INDEX];
      for i := node.Count - 1 downto 0 do
        node.Item[i].Delete;

      for k := 0 to MVC.Layer.ParamsList.Count - 1 do
      begin
        MVC.SectionAdd(False);
        for i := 0 to T_sectionContainer(lstFeatures).Count - 1 do
        begin
          if i = 0 then
          begin
            // set to the section to get the section description
            MVC.SectionIndex := k;

            if not IsStringEmpty(MVC.Section.Legend) then
              desc := MVC.Section.Legend
            else
              desc := Format('%s %d', [_rsrc(GIS_RS_LEGEND_PAG_SECTION),
                MVC.SectionIndex + 1]);
            node := tvPages.Items.AddChild(node, desc);
          end
          else
            tvPages.Items.AddChild(node, T_sectionContainer(lstFeatures)
              .Panel[i].ItemText);
        end;
        node := node.Parent;
      end;
    finally
      MVC.SectionIndex := 0;
      tvPages.Items.EndUpdate;
    end;
  finally
    unlock;
  end;
  T_sectionContainer(lstFeatures).Read;
  doPageChange(tvPages.Items[0]);
end;

procedure TGIS_ControlLegendForm.doWizardClick(_sender: TObject);
var
  vfrm: TGIS_PvlControlLegendVectorWiz;
  gfrm: TGIS_PvlControlLegendGridWiz;
  res: Integer;
  typ: TGIS_ShapeType;
  node: TTreeNode;
  cur_node: TTreeNode;
  desc: String;
  proc: TGIS_Proc;
begin
  btnWizard.Enabled := False ;
  MVC.PrepareLayerEx;
  try
    if MVC.IsVector then
    begin
      vfrm := TGIS_PvlControlLegendVectorWiz.Create( Self );

      if assigned(Self.oLegend) then
      begin
        vfrm.UniqueLimit := Self.oLegend.DialogOptions.VectorWizardUniqueLimit;
        vfrm.UniqueSearchLimit :=
          Self.oLegend.DialogOptions.VectorWizardUniqueSearchLimit;
      end
      else
      begin
        vfrm.UniqueLimit := 256;
        vfrm.UniqueSearchLimit := 16384;
      end;

      if T_scrollablePanel(pnlCurrent).ItemText = _rsrc
        (GIS_RS_LEGEND_PAG_MARKER) then
        typ := TGIS_ShapeType.Point
      else if T_scrollablePanel(pnlCurrent)
        .ItemText = _rsrc(GIS_RS_LEGEND_PAG_LINE) then
        typ := TGIS_ShapeType.Arc
      else if T_scrollablePanel(pnlCurrent)
        .ItemText = _rsrc(GIS_RS_LEGEND_PAG_AREA) then
        typ := TGIS_ShapeType.Polygon
      else
        typ := TGIS_ShapeType.Unknown;


      proc := {$IFDEF OXYGENE}TGIS_Proc.create({$ENDIF}
        procedure( _modal_result : TGIS_PvlModalResult )
        var
          i,k : Integer ;
        begin
          if _modal_result = TGIS_PvlModalResult.OK then
          begin
            lock;
            try
              for i := MVC.SectionCount - 1 downto 0 do
                MVC.SectionDelete;
              tvPages.Items.BeginUpdate;
              try
                node := tvPages.Items.Item[GIS_CONTAINER_INDEX];
                for i := node.Count - 1 downto 0 do
                  node.Item[i].Delete;

                MVC.SwitchToLayerEx;

                for k := 0 to MVC.LayerEx.ParamsList.Count - 1 do
                begin
                  MVC.SectionAdd(False);
                  for i := 0 to T_sectionContainer(lstFeatures).Count - 1 do
                  begin
                    if i = 0 then
                    begin
                      // set to the section to get the section description
                      MVC.SectionIndex := k;
                      if not IsStringEmpty(MVC.Section.Legend) then
                        desc := MVC.Section.Legend
                      else
                        desc := T_sectionContainer(lstFeatures).Panel[i].ItemText;
                      node := tvPages.Items.AddChild(node, desc);
                    end
                    else
                      tvPages.Items.AddChild(node, T_sectionContainer(lstFeatures)
                        .Panel[i].ItemText);
                  end;
                  node := node.Parent;
                end;
              finally
                MVC.SectionIndex := 0;
                tvPages.Items.EndUpdate;
              end;
            finally
              unlock;
              MVC.SwitchToLayer;
            end;
            T_sectionContainer(lstFeatures).Read;
            doPageChange(tvPages.Items[GIS_CONTAINER_INDEX]);
          end;
        end
      {$IFDEF OXYGENE}){$ENDIF};

      vfrm.Execute(MVC.Layer as TGIS_LayerVector, typ,
        MVC.LayerEx.ParamsList, pOnHelp, proc);
    end
    else if MVC.IsGrid then
    begin
      gfrm := TGIS_PvlControlLegendGridWiz.Create( Self );
      proc := {$IFDEF OXYGENE}TGIS_Proc.create({$ENDIF}
        procedure( _modal_result : TGIS_PvlModalResult )
        var
          i, k : Integer ;
        begin
          if _modal_result = TGIS_PvlModalResult.OK then
          begin
            cur_node := nil;
            lock;
            try
              for i := MVC.SectionCount - 1 downto 0 do
                MVC.SectionDelete;
              tvPages.Items.BeginUpdate;
              try
                node := tvPages.Items.Item[GIS_CONTAINER_INDEX];
                for i := node.Count - 1 downto 0 do
                  node.Item[i].Delete;

                MVC.SwitchToLayerEx;

                for k := 0 to MVC.LayerEx.ParamsList.Count - 1 do
                begin
                  MVC.SectionAdd(False);
                  for i := 0 to T_sectionContainer(lstFeatures).Count - 1 do
                  begin
                    if i = 0 then
                      node := tvPages.Items.AddChild(node,
                        T_sectionContainer(lstFeatures).Panel[i].ItemText)
                    else
                    begin
                      if i = 1 then
                        cur_node := tvPages.Items.AddChild(node,
                          T_sectionContainer(lstFeatures).Panel[i].ItemText)
                      else
                        tvPages.Items.AddChild(node, T_sectionContainer(lstFeatures)
                          .Panel[i].ItemText);
                    end;
                  end;
                  node := node.Parent;
                end;
              finally
                tvPages.Items.EndUpdate;
              end;
            finally
              unlock;
              MVC.SwitchToLayer;
            end;
            T_sectionContainer(lstFeatures).Read;
            lastNodeIdx := -1;
            doPageChange(cur_node);
          end;
        end
      {$IFDEF OXYGENE}){$ENDIF};
      gfrm.Execute(MVC.Layer as TGIS_LayerPixel,
        MVC.LayerEx.Params as TGIS_ParamsSectionPixel, pOnHelp, proc);
    end;
  finally
    MVC.UnPrepareLayerEx;
    btnWizard.Enabled := True ;
  end;
end;

class function TGIS_ControlLegendForm.ShowLayerProperties
  (const _layer: TGIS_Layer; const _legend: TGIS_ControlLegend;
  const _onhelp: TGIS_HelpEvent): Integer;
var
  frm: TGIS_ControlLegendForm;
  prnt: TComponent;
begin
  if not assigned(_layer) then
  begin
    Result := mrNone;
    exit;
  end;

  prnt := nil;
  if assigned(_legend) then
    prnt := _legend
  else if assigned(_layer.Viewer) then
    prnt := TGIS_ViewerWnd(_layer.Viewer.Ref.ViewerParentRoot);

  if not assigned(prnt) then
    prnt := Application.MainForm;

  frm := TGIS_ControlLegendForm.Create(prnt);
  try
    _layer.Open;
    Result := frm.Execute(_layer, _legend, _onhelp);
    if Result = mrOK then
    begin
      frm.Hide;
      if assigned(_legend) then
      begin
        if assigned(_legend.LayerParamsChangeEvent) then
          _legend.LayerParamsChangeEvent(_legend, _layer);
      end;
      if assigned(_layer.Viewer) then
        _layer.Viewer.Ref.InvalidateWholeMap;
    end;
  finally
    frm.Free;
  end;
end;

class function TGIS_ControlLegendForm.ShowLayerProperties
  (const _layer: TGIS_Layer; const _legend: TGIS_ControlLegend): Integer;
var
  hlp: TGIS_HelpEvent;
begin
  if assigned(_layer) and assigned(_layer.Viewer) then
    hlp := _layer.Viewer.Ref.AssignedHelpEvent()
  else
    hlp := nil;
  Result := ShowLayerProperties(_layer, _legend, hlp);
end;

{$ENDREGION}
{$REGION 'GisShowLayerProperties'}

function GisShowLayerProperties(const _layer: TGIS_Layer;
  const _legend: TGIS_ControlLegend; const _onhelp: TGIS_HelpEvent): Integer;
begin
  Result := TGIS_ControlLegendForm.ShowLayerProperties(_layer, _legend,
    _onhelp);
end;

function GisShowLayerProperties(const _layer: TGIS_Layer;
  const _legend: TGIS_ControlLegend): Integer;
begin
  Result := TGIS_ControlLegendForm.ShowLayerProperties(_layer, _legend);
end;

{$ENDREGION}

// ==================================== END =====================================
end.

