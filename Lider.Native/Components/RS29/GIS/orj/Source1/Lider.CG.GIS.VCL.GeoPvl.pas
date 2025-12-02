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
  Implementation of PVL controls for WinForms
}

unit Lider.CG.GIS.VCL.GeoPvl;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoPvl"'}

interface

uses
  Lider.CG.GIS.GeoInterfaces,
  Lider.CG.GIS.PVL.GeoPvl;

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

type
  {#gendoc:hide}
  // Initialization section handler
  Unit_VCL_GisPvl = class
    public
      class procedure SelfRegisterPVL();
  end;

  {#gendoc:hide}
  // register PVL control
  procedure RegisterPVLPlatformControl(
              const _name  : String;
              const _class : TClass
            );

  {#gendoc:hide}
  // Apply DPI on all constext
  procedure ApplyPPI(
    const _form   : TObject;
    const _ppi    : Integer;
    const _ppiFix : Double ;
    const _redraw : Boolean
  ) ;

  type

    {#gendoc:hide}
    TGIS_PvlBaseVCL = class( TGIS_PvlBase, IGIS_PvlBase )
      protected
        oParent  : IGIS_PvlBase;
        oContext : TGIS_PvlContext;
        oControl : TObject;

        function Parent : TGIS_PvlBase ;
      private
        function  fget_NativeControl: TObject ;
                                    override ;
      public
        constructor Create          ( const _context    : TGIS_PvlContext;
                                      const _parent     : IGIS_PvlBase
                                    );
                                    virtual;
    end;

//##############################################################################
implementation

uses
  Winapi.Windows,
  System.Generics.Collections,
  System.SysUtils,
  System.Classes,
  System.UITypes,
  System.Types,
  System.TypInfo,
  System.Math,
  StrUtils,
  VCL.Themes,
  VCL.Forms,
  VCL.Dialogs,
  VCL.ExtDlgs,
  VCL.Buttons,
  VCL.Controls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.ComCtrls,
  VCL.Graphics,
  VCL.ImgList,

  Lider.CG.GIS.PVL.GeoPvlForms,
  Lider.CG.GIS.PVL.GeoControlColor,
  Lider.CG.GIS.PVL.GeoControlSizeForm,
  Lider.CG.GIS.PVL.GeoControlFieldFactor,
  Lider.CG.GIS.PVL.GeoControlSymbology,
  Lider.CG.GIS.PVL.GeoControlBitmap,
  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoSymbol,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoInternals,
  Lider.CG.GIS.GeoLibrarySVG,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoCsSystems,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.GeoViewer,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoControlPrintPreviewSimple,
  Lider.CG.GIS.GeoPrintManagerAbstract,
  Lider.CG.GIS.VCL.GeoFramework,
  Lider.CG.GIS.VCL.GeoComboBoxHelper,
  Lider.CG.GIS.VCL.GeoViewerBmp,
  Lider.CG.GIS.VCL.GeoLineSymbolEditor,
  Lider.CG.GIS.VCL.GeoControlCsSystem,
  Lider.CG.GIS.VCL.GeoViewerWnd,
  Lider.CG.GIS.VCL.GeoSystemForms,
  Lider.CG.GIS.VCL.GeoPvlForms;

const
  GIS_COLOR_HUE_SIZE         : Integer = 1530;

  GIS_COLOR_WHEEL_MARGIN     : Integer = 14;
  GIS_COLOR_WHEEL_MARGIN2    : Integer = 7;

  GIS_COLOR_GRADIENT_MARGIN  : Integer = 8;
  GIS_COLOR_GRADIENT_MARGIN2 : Integer = 4;

  DPI_MANAGER                = 'nibalofreb';


var
  oItemCache   : TDictionary<String,TGIS_Bitmap>; // global bitmap item cache
  oControlDict : TDictionary< String, TClass>;

// draw checkerboard color
procedure draw_color(
  _canvas   : TCanvas;
  _color    : TGIS_Color;
  _rect     : TRect;
  _cellsize : Integer;
  _enabled  : Boolean
);
var
  clr  : TGIS_Color;
  i    : Integer;
  imax : Integer;
  k    : Integer;
  kmax : Integer;
  blst : Boolean;
  bkgm : Double;
  fggm : Double;
  r    : Byte;
  g    : Byte;
  b    : Byte;
  rb   : Byte;
  gb   : Byte;
  bb   : Byte;
  rw   : Byte;
  gw   : Byte;
  bw   : Byte;
  bmix : Cardinal;
  wmix : Cardinal;
begin
  if _enabled then begin
    clr := _color;
    rb := $FF;
    gb := $FF;
    bb := $FF;
    rw := $C0;
    gw := $C0;
    bw := $C0;
  end
  else begin
    clr := TGIS_Color.FromARGB( $00000000 );
    rb := $C0;
    gb := $C0;
    bb := $C0;
    rw := $88;
    gw := $88;
    bw := $88;
  end;

  imax := RoundS( _rect.Width  / _cellsize ) + 1;
  kmax := RoundS( _rect.Height / _cellsize ) + 1;

  fggm := clr.A / 255;
  bkgm := 1.0 - fggm;

  r := clr.R;
  g := clr.G;
  b := clr.B;

  bmix := ( RoundS( bkgm * rb + fggm * b ) shl 16 ) +
          ( RoundS( bkgm * gb + fggm * g ) shl 8  ) +
          ( RoundS( bkgm * bb + fggm * r )        );
  wmix := ( RoundS( bkgm * rw + fggm * b ) shl 16 ) +
          ( RoundS( bkgm * gw + fggm * g ) shl 8  ) +
          ( RoundS( bkgm * bw + fggm * r )        );

  _canvas.Brush.Style := TBrushStyle.bsSolid;
  for k := 0 to kmax do begin

    blst := ( k mod 2 ) = 0;

    for i := 0 to imax do begin

      if blst then begin
        _canvas.Pen.Color   := TColor( bmix );
        _canvas.Brush.Color := TColor( bmix );
      end
      else begin
        _canvas.Pen.Color   := TColor( wmix );
        _canvas.Brush.Color := TColor( wmix );
      end;

      _canvas.Rectangle(
        Min( _rect.Right , _rect.Left + i * _cellsize + 1 ),
        Min( _rect.Bottom, _rect.Top  + k * _cellsize + 1 ),
        Min( _rect.Right , _rect.Left + ( i + 1 ) * _cellsize + 1 ),
        Min( _rect.Bottom, _rect.Top  + ( k + 1 ) * _cellsize + 1 )
      );

      blst := not blst;
    end;

  end;
end;

type
  TGIS_PvlBaseVCLClass = class of TGIS_PvlBaseVCL;
  T_PvlControl = class;
  T_PvlContext = class;
  T_PvlTreeNode = class;

{$REGION 'T_speedButton'}
  T_SpeedButton = class(TSpeedButton)
    protected
      procedure Paint;            override;
    public
      Images                      : TImageList;
      DisabledImages              : TImageList;
      ImageIndex                  : Integer;
    end;
{$ENDREGION}
  /// <summary>
  ///   Panel with image rendered in the middle.
  /// </summary>
  TGIS_ImagePanel = class( TCustomControl )
    public
      constructor Create          (       _owner       : TComponent
                                  );
                                  override;
    protected
      /// <inheritdoc/>
      procedure Paint;            override;
    public
      /// <summary>
      ///   Image to be rendered.
      /// </summary>
      Image                       : TBitmap;
      /// <summary>
      ///   Flag to stretch image.
      /// </summary>
      Stretch                     : Boolean;

      OnPaintEvent                : TGIS_PvlEvent;
  end;

  T_PvlColorPreviewPanel = class ( TCustomControl )
    public
      constructor Create          (      _owner       : TComponent
                                  );
                                  override;
    public
      Color                       : TGIS_Color;
      CellSize                    : Integer;
      Border                      : Boolean;
    protected
      procedure Paint;            override;
  end;

  // component to be added as singleton a proxy to a form to maneger DPI events
  T_dpiManager = class( TComponent )
    public
      constructor Create          (       _owner      : TComponent
                                  );
                                  override;
    private
      oForm                       : TForm;
      iPPI                        : Integer;
      dPPIFix                     : Single;

    {$IFDEF LEVEL_RX101_VCL}
      oldBeforeMonitorDpiChanged  : TMonitorDpiChangedEvent;
      oldAfterMonitorDpiChanged   : TMonitorDpiChangedEvent;
      procedure doBeforeMonitorDpiChanged(  _sender     : TObject;
                                            _oldDPI     : Integer;
                                            _newDPI     : Integer
                                    );
      procedure doAfterMonitorDpiChanged(   _sender     : TObject;
                                            _oldDPI     : Integer;
                                            _newDPI     : Integer
                                        );
    {$ENDIF}
  end;

  // component to be added as a proxy to a form to allow automatic disposal
  T_component = class( TComponent )
    public
      constructor Create          (       _owner      : TComponent
                                  );
                                  override;
      destructor  Destroy;        override;
    private
      oOwner                      : T_PvlContext;
      oMaster                     : TGIS_PvlContext;
      lstControls                 : TObjectList<T_PvlControl>;
    public
      procedure   Add             ( const _control    : T_PvlControl
                                  );

  end;


  T_PvlContext = class( TGIS_PvlContextBase, IGIS_PvlContext )
    public
      constructor Create          ( const _parent     : TObject;
                                    const _ppi        : Integer;
                                    const _ppifix     : Single;
                                    const _canvasscale: Single;
                                    const _righttoleft: Boolean;
                                    const _context    : TGIS_PvlContext
                                  );
                                  override;
      constructor Create          ( const _parent     : TObject;
                                    const _context    : TGIS_PvlContext
                                  );
                                  override;
      constructor Create          ( const _parent     : TObject;
                                    const _context    : TGIS_PvlContext;
                                    const _refcontext : TGIS_PvlContext
                                  );
                                  override;
    protected
      oMaster                     : TGIS_PvlContext;
      oParent                     : TObject;
      iHMargin                    : Integer;
      iVMargin                    : Integer;
      bBiDi                       : Boolean;
      oOwner                      : T_component; // component to be owned by
                                                 // form to control for a proper
                                                 // disposal
    protected
      function  fget_HMargin      : Integer;
      procedure fset_HMargin      ( const _value : Integer
                                  ) ;
      function  fget_VMargin      : Integer;
      procedure fset_VMargin      ( const _value : Integer
                                  ) ;
      function  fget_HSpace       : Integer;
      function  fget_VSpace       : Integer;
      function  fget_LSpace       : Integer;
      function  fget_CanvasScale  : Single;
      function  fget_PPIFix       : Single;
      function  fget_PPI          : Integer;
    protected
      function  CreateObject      ( const _parent     : TGIS_PvlBase;
                                    const _name       : String
                                  ) : IGIS_PvlBase;
                                  overload;
    public
      class function CreateOpenDialog(
                                    const _parent     : TObject;
                                    const _self       : TGIS_PvlOpenDialog
                                  ) : TGIS_PvlOpenDialog;
                                  override;
      class function CreateSaveDialog(
                                    const _parent     : TObject;
                                    const _self       : TGIS_PvlSaveDialog
                                  ) : TGIS_PvlSaveDialog;
                                  override;
      class function CreateSelectFolderDialog(
                                    const _parent     : TObject;
                                    const _self       : TGIS_PvlSelectFolderDialog
                                  ) : TGIS_PvlSelectFolderDialog;
                                  override;
      class function CreateOptionDialog(
                                    const _parent     : TObject;
                                    const _self       : TGIS_PvlOptionDialog
                                  ) : TGIS_PvlOptionDialog;
                                  override;
      class function  CreateInfoDialog (
                                    const _parent     : TObject;
                                    const _self       : TGIS_PvlInfoDialog
                                  ) : TGIS_PvlInfoDialog;
                                  override ;
      class function  CreateWarningDialog(
                                    const _parent     : TObject;
                                    const _self       : TGIS_PvlWarningDialog
                                  ) : TGIS_PvlWarningDialog;
                                  override ;
      class function  CreateErrorDialog(
                                    const _parent     : TObject;
                                    const _self       : TGIS_PvlErrorDialog
                                  ) : TGIS_PvlErrorDialog;
                                  override ;
      class function  CreateCSDialog(
                                    const _parent     : TObject;
                                    const _self       : TGIS_PvlControlCSSystem
                                  ) : TGIS_PvlControlCSSystem;
                                  override ;
      class function  CreateLineSymbologyDialog(
                                    const _parent     : TObject;
                                    const _self       : TGIS_PvlLineSymbolEditor
                                  ) : TGIS_PvlLineSymbolEditor;
                                  override ;
      class function  CreatePrintPreviewSimpleDialog(
                                    const _parent     : TObject;
                                    const _self       : TGIS_PvlControlPrintPreviewSimple
                                  ) : TGIS_PvlControlPrintPreviewSimple;
                                  override ;
      class function Support      ( const _parent     : TObject
                                  ) : Boolean;
                                  override;
    public
      procedure FreeContext       ;
      function  GetAllFonts       : TStringList;
      function  ClientWidth       : Integer;
  end;

  T_PvlControl = class( TGIS_PvlBaseVCL, IGIS_PvlControl )
    public
      constructor Create          ( const _context    : TGIS_PvlContext ;
                                    const _parent     : IGIS_PvlBase
                                  );
                                  override;

    protected
      /// <summary>
      ///   Perform actual low-velel construction.
      /// </summary>
      /// <param name="_context">
      ///   Context
      /// </param>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  virtual; abstract;
      procedure doDestroy        ; override ;

    protected
      procedure DoRedraw;         virtual;

    protected
      procedure &add              ( const _control    : TObject
                                  );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_NativeControl: TObject;
                                  override;

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_IsStyled     : Boolean;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Anchors     : TGIS_PvlAnchors;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Anchors      ( const _value      : TGIS_PvlAnchors
                                  );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Align        : TGIS_PvlAlign;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Align        ( const _value      : TGIS_PvlAlign
                                  );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Enabled      : Boolean;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Enabled      ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Width        : Integer;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Width        ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Height       : Integer;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Height       ( const _value      : Integer
                                  );
                                  virtual;

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Left         : Integer;

      /// <inheritdoc from="IGIS_PvlLabel"/>
      procedure fset_Left         ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Top          : Integer;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Top          ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_TabOrder     : Integer;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_TabOrder     ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Visible      : Boolean;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Visible      ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Hint         : String;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Hint         ( const _value      : String
                                  );
    public
      procedure SetFocus;         virtual;
  end;

  T_PvlLabel = class( T_PvlControl, IGIS_PvlLabel )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
    private
      oFocusControl               : TGIS_PvlControl;

    private
      /// <inheritdoc from="IGIS_PvlLabel"/>
      function  fget_Caption      : String;

      /// <inheritdoc from="IGIS_PvlLabel"/>
      procedure fset_Caption      ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlLabel"/>
      function  fget_FocusControl : TGIS_PvlControl;

      /// <inheritdoc from="IGIS_PvlLabel"/>
      procedure fset_FocusControl (
                                    const _value      : TGIS_PvlControl
                                  );
      /// <inheritdoc from="IGIS_PvlLabel"/>
      function  fget_FontSize     : Integer;

      /// <inheritdoc from="IGIS_PvlLabel"/>
      procedure fset_FontSize     ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlLabel"/>
      function  fget_FontStyle    : TGIS_FontStyles;

      /// <inheritdoc from="IGIS_PvlLabel"/>
      procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                  );

      /// <inheritdoc from="IGIS_PvlLabel"/>
      function  fget_FontFamily   : String;

      /// <inheritdoc from="IGIS_PvlLabel"/>
      procedure fset_FontFamily   ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlLabel"/>
      function  fget_Alignment    : TGIS_PvlLabelTextAlignment;

      /// <inheritdoc from="IGIS_PvlLabel"/>
      procedure fset_Alignment    ( const _value      : TGIS_PvlLabelTextAlignment
                                  );
    public
      /// <inheritdoc from="IGIS_PvlLabel"/>
      procedure SetFocus;         override;
  end;

  T_PvlIconButton = class ( T_PvlControl, IGIS_PvlIconButton )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
    public
      procedure DoRedraw;         override;

    private
      FOnClick                    : TGIS_PvlEvent;

    private
      procedure doOnClick         (      _sender      : TObject
                                  );

    private
      /// <inheritdoc from="IGIS_PvlIconButton"/>
      function  fget_Pushed       : Boolean;

      /// <inheritdoc from="IGIS_PvlIconButton"/>
      procedure fset_Pushed       ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IconButton"/>
      function  fget_OnClick      : TGIS_PvlEvent;

      /// <inheritdoc from="IconButton"/>
      procedure fset_OnCLick      ( const _value      : TGIS_PvlEvent
                                  );

    public
      procedure SetFocus;         override;
  end;

  T_PvlButton = class( T_PvlControl, IGIS_PvlButton )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
    private
      FOnClick                    : TGIS_PvlEvent;

    private
      procedure doOnClick         (      _sender      : TObject
                                  );

    private
      /// <inheritdoc from="IGIS_PvlButton"/>
      function  fget_Caption      : String;

      /// <inheritdoc from="IGIS_PvlButton"/>
      procedure fset_Caption      ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlButton"/>
      function  fget_Default      : Boolean;

      /// <inheritdoc from="IGIS_PvlButton"/>
      procedure fset_Default      ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlButton"/>
      function  fget_OnClick      : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlButton"/>
      procedure fset_OnClick      ( const _value      : TGIS_PvlEvent
                                  );

    public
      procedure SetFocus;         override;
  end;

  T_PvlModalButton = class( T_PvlControl, IGIS_PvlModalButton )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;

    private
      FOnClick                    : TGIS_PvlEvent;

    private
      procedure doOnClick         (      _sender      : TObject
                                  );
    protected
      /// <inheritdoc from="IGIS_PvlModalButton"/>
      function  fget_Caption      : String;

      /// <inheritdoc from="IGIS_PvlModalButton"/>
      procedure fset_Caption      ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlModalButton"/>
      function  fget_Default      : Boolean;

      /// <inheritdoc from="IGIS_PvlModalButton"/>
      procedure fset_Default      ( const _value      : Boolean
                                  );
      /// <inheritdoc from="IGIS_PvlButton"/>
      function  fget_OnClick      : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlButton"/>
      procedure fset_OnClick      ( const _value      : TGIS_PvlEvent
                                  );

      /// <inheritdoc from="IGIS_PvlModalButton"/>
      function  fget_Parent       : TObject;

      /// <inheritdoc from="IGIS_PvlModalButton"/>
      procedure fset_Parent       ( const _value      : TObject
                                  );

    public
      procedure SetFocus;         override;
  end;

  T_PvlEdit = class( T_PvlControl, IGIS_PvlEdit )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
    protected
      FOnChange                   : TGIS_PvlEvent;
      FOnClick                    : TGIS_PvlEvent;
      FOnKeyDown                  : TGIS_PvlKeyEvent;
      FOnKeyPress                 : TGIS_PvlKeyPressEvent;

    private
      procedure doOnClick         (       _sender     : TObject
                                  );
      procedure doOnChange        (       _sender     : TObject
                                  );
      procedure doOnKeyDown       (       _sender     : TObject;
                                    var   _key        : Word;
                                          _shift      : TShiftState
                                  );
      procedure doOnKeyPress      (       _sender     : TObject;
                                     var  _char       : Char
                                  );
    protected
      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_Text         : String;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_Text         ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_SelectionLength
                                  : Integer;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_SelectionLength(
                                    const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_SelectionStart
                                  : Integer;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_SelectionStart(
                                    const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_SelectedText : String;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  );

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_OnClick      : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_OnClick      ( const _value      : TGIS_PvlEvent
                                  );

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_OnKeyDown    : TGIS_PvlKeyEvent;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_OnKeyDown    ( const _value      : TGIS_PvlKeyEvent
                                  );

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_OnKeyPress   : TGIS_PvlKeyPressEvent;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_OnKeyPress   ( const _value      : TGIS_PvlKeyPressEvent
                                  );

    public
      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure SetFocus;         override;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure SetFontAlarm;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure SetFontDefault;
  end;

  T_PvlMemo = class( T_PvlControl, IGIS_PvlMemo )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;

    protected
      FOnChange                   : TGIS_PvlEvent;
      FOnClick                    : TGIS_PvlEvent;
      FOnKeyDown                  : TGIS_PvlKeyEvent;
      FOnKeyPress                 : TGIS_PvlKeyPressEvent;

    private
      procedure doOnClick         (       _sender     : TObject
                                  );
      procedure doOnChange        (       _sender     : TObject
                                  );
      procedure doOnKeyDown       (        _sender    : TObject;
                                    var   _key        : Word;
                                          _shift      : TShiftState
                                  );
      procedure doOnKeyPress      (       _sender     : TObject;
                                     var  _char       : Char
                                  );

    protected
      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_CursorPos    : TPoint;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_CursorPos    ( const _value      : TPoint
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_Text         : String;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_Text         ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_SelectionLength
                                  : Integer;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_SelectionLength(
                                    const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_SelectionStart
                                  : Integer;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_SelectionStart(
                                    const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_SelectedText : String;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_WordWrap     : Boolean;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_WordWrap     ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_OnClick      : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_OnClick      ( const _value      : TGIS_PvlEvent
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_OnKeyDown    : TGIS_PvlKeyEvent;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_OnKeyDown    ( const _value      : TGIS_PvlKeyEvent
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      function  fget_OnKeyPress   : TGIS_PvlKeyPressEvent;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure fset_OnKeyPress   ( const _value      : TGIS_PvlKeyPressEvent
                                  );

    public
      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure SetFocus;         override;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure SetFontAlarm;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure SetFontDefault;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure Clear;

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure AppendText        ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlMemo"/>
      procedure AppendLine        ( const _value      : String
                                  );
  end;

  T_PvlColorPreview = class ( T_PvlControl, IGIS_PvlColorPreview )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
    protected
      FOnClick                    : TNotifyEvent;

    private
      procedure doOnClick         (       _sender     : TObject
                                  );
    protected
      /// <inheritdoc from="IGIS_PvlColorPreview"/>
      function  fget_Color        : TGIS_Color;

      /// <inheritdoc from="IGIS_PvlColorPreview"/>
      procedure fset_Color        (  const _value     : TGIS_Color
                                  );

      /// <inheritdoc from="IGIS_PvlColorPreview"/>
      function  fget_Border       : Boolean;

      /// <inheritdoc from="IGIS_PvlColorPreview"/>
      procedure fset_Border       ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlColorPreview"/>
      function  fget_OnClick      : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlColorPreview"/>
      procedure fset_OnClick      ( const _value      : TGIS_PvlEvent
                                  );
    public
      /// <inheritdoc from="IGIS_PvlColorPreview"/>
      procedure SetFocus;         override;
  end;

  T_PvlColorWheel = class( T_PvlControl, IGIS_PvlColorWheel )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
      /// <inheritdoc"/>
      procedure doDestroy;        override;

    private
      bInit                       : Boolean;
      colorWheel                  : TBitmap;
      currState                   : TBitmap;
      currX                       : Integer;
      currY                       : Integer;
      bMouseDown                  : Boolean;
      oldPPI                      : Single;
      arrHue                      : array of TGIS_PvlRGBVal;

      FOnChange                   : TNotifyEvent;

    protected
      /// <summary>
      ///   Draw color wheel
      /// </summary>
      procedure makeColorWheel;

      /// <summary>
      ///   Draw crosshair
      /// </summary>
      procedure drawCrosshair;

      /// <summary>
      ///   Prepares Hue factor.
      /// </summary>
      procedure prepareHue;

      /// <summary>
      ///   Converts angle from picker to RGB value
      /// </summary>
      /// <param name="_phi">
      ///   Given angle
      /// </param>
      /// <returns>
      ///   RGB value
      /// </returns>
      function  angleToRGB        ( const _phi        : Double
                                  ) : TGIS_PvlRGBVal;


      /// <summary>
      ///   Checks if coordinates are within the color picker
      /// </summary>
      /// <param name="_x">
      ///   X coordinate
      /// </param>
      /// <param name="_y">
      ///   Y coordinate
      /// </param>
      /// <returns>
      ///   True if inside
      /// </returns>
      function  checkMouse        ( const _x          : Integer;
                                    const _y          : Integer
                                  ) : Boolean;

      procedure doOnPaint         (       _e          : TObject
                                  );
      procedure doMouseDown       (       _sender     : TObject;
                                          _button     : TMouseButton;
                                          _shift      : TShiftState;
                                          _x          : Integer;
                                          _y          : Integer
                                  );
      procedure doMouseMove       (       _sender     : TObject;
                                          _shift      : TShiftState;
                                          _x          : Integer;
                                          _y          : Integer
                                  );
      procedure doMouseUp         (       _sender     : TObject;
                                          _button     : TMouseButton;
                                          _shift      : TShiftState;
                                          _x          : Integer;
                                          _y          : Integer
                                  );
    protected
      /// <inheritdoc from="IGIS_PvlColorWheel"/>
      function  fget_Color        : TGIS_Color;

      /// <inheritdoc from="IGIS_PvlColorWheel"/>
      procedure fset_Color        ( const _value      : TGIS_Color
                                  );
      /// <inheritdoc from="IGIS_PvlColorWheel"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlColorWheel"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  );
    protected
      procedure DoRedraw;         override;

    public
      /// <inheritdoc from="IGIS_PvlColorWheel"/>
      function  HueToRGB          ( const _hue        : Double
                                  ) : TGIS_PvlRGBVal;

      procedure SetFocus;         override;
  end;

  T_PvlColorBar = class( T_PvlControl, IGIS_PvlColorBar )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
      /// <inheritdoc"/>
      procedure doDestroy;        override;

    protected
      bInit                       : Boolean;
      gradient                    : TBitmap;
      currState                   : TBitmap;
      currX                       : Integer;
      bMouseDown                  : Boolean;
      oldPPI                      : Single;

      FValue                      : Double;
      FColor                      : TGIS_Color;
      FAlpha                      : Boolean;
      FOnChange                   : TNotifyEvent;

    private
      /// <summary>
      ///   Draw gradient on the slider
      /// </summary>
      procedure makeGradient;

      /// <summary>
      ///   Draw Color on the slider
      /// </summary>
      procedure makeColor;

      /// <summary>
      ///   Draw alpha version of the slider
      /// </summary>
      procedure makeAlpha;

      procedure drawArrows;

      /// <summary>
      ///   Checks if coordinates are within the color picker
      /// </summary>
      /// <param name="_x">
      ///   X coordinate
      /// </param>
      /// <param name="_y">
      ///   Y coordinate
      /// </param>
      /// <returns>
      ///   True if inside
      /// </returns>
      function  checkMouse        ( const _x          : Integer;
                                    const _y          : Integer
                                  ) : Boolean;

      procedure doOnPaint         (       _e          : TObject
                                  );
      procedure doMouseDown       (       _sender     : TObject;
                                          _button     : TMouseButton;
                                          _shift      : TShiftState;
                                          _x          : Integer;
                                          _y          : Integer
                                  );
      procedure doMouseMove       (      _sender      : TObject;
                                         _shift       : TShiftState;
                                         _x           : Integer;
                                         _y           : Integer
                                  );
      procedure doMouseUp         (      _sender      : TObject;
                                         _button      : TMouseButton;
                                         _shift       : TShiftState;
                                         _x           : Integer;
                                         _y           : Integer
                                  );
    protected
      /// <inheritdoc from="IGIS_PvlColorBar"/>
      function  fget_Color        : TGIS_Color;

      /// <inheritdoc from="IGIS_PvlColorBar"/>
      procedure fset_Color        ( const _value      : TGIS_Color
                                  );

      /// <inheritdoc from="IGIS_PvlColorBar"/>
      function  fget_Value        : Double;

      /// <inheritdoc from="IGIS_PvlColorBar"/>
      procedure fset_Value        ( const _value      : Double
                                  );

      /// <inheritdoc from="IGIS_PvlColorBar"/>
      function  fget_Alpha        : Boolean;

      /// <inheritdoc from="IGIS_PvlColorBar"/>
      procedure fset_Alpha        ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlColorBar"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlColorBar"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  );
    protected
      procedure DoRedraw;         override;

    public
      procedure SetFocus;         override;
  end;

  T_PvlCustomComboBox = class( T_PvlControl, IGIS_PvlCustomComboBox )

    public
      /// <inheritdoc"/>
      constructor Create          ( const _context    : TGIS_PvlContext ;
                                    const _parent     : IGIS_PvlBase
                                  );
                                  override;
    protected
      /// <inheritdoc"/>
      procedure doDestroy;        override;

    private
      /// <summary>
      ///   Count of the custom fields generated
      ///   through Fill method.
      /// </summary>
      iCustomFieldsCount          : Integer;

      ///  <summary>
      ///   Fields to be used if theres FieldFactor control involved.
      ///  </summary>
      FFields                     : TStringList;

      FOnChange                   : TGIS_PvlEvent;

    protected
      procedure doOnChange        (        _sender    : TObject
                                  );

    protected
       /// <inheritdoc from="IGIS_PvlCustomComboBox"/>
      function  fget_Value        : String;
                                  virtual;

      /// <inheritdoc from="IGIS_PvlCustomComboBox"/>
      procedure fset_Value        ( const _value      : String
                                  );
                                  virtual;

      /// <inheritdoc from="IGIS_PvlCustomComboBox"/>
      function  fget_Fields       : TStringList;

      /// <inheritdoc from="IGIS_PvlCustomComboBox"/>
      procedure fset_Fields       ( const _value      : TStringList
                                  );

      /// <inheritdoc from="IGIS_PvlCustomComboBox"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlCustomComboBox"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  );
  end;

  T_PvlSizeComboBox = class ( T_PvlCustomComboBox, IGIS_PvlSizeComboBox )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;

      procedure doOnChange        (       _sender     : TObject
                                  );
      procedure doColorListDrawItem(
                                          Control     : TWinControl;
                                          Index       : Integer;
                                          Rect        : TRect;
                                          State       : TOwnerDrawState
                                   );
    protected
      /// <inheritdoc from="IGIS_PvlSizeComboBox"/>
      procedure fset_Value        ( const _value      : String
                                  );
                                  override;

      /// <inheritdoc from="IGIS_PvlSizeComboBox"/>
      procedure fset_Height       ( const _value      : Integer
                                  );
                                  override;

    public
      /// <inheritdoc from="IGIS_PvlSizeComboBox"/>
      procedure Fill              ( const _forSymbol  : Boolean;
                                    const _forLine    : Boolean;
                                    const _field      : Boolean;
                                    const _renderer   : Boolean
                                  );
      /// <inheritdoc from="IGIS_PvlSizeComboBox"/>
      procedure FillAggregation;

      /// <inheritdoc from="IGIS_PvlSizeComboBox"/>
      procedure FillRealWorldUnits( const _field      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlSizeComboBox"/>
      procedure SetFocus;         override;
  end;

  T_PvlColorComboBox = class ( T_PvlCustomComboBox, IGIS_PvlColorComboBox )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;

    private
      prevColor                   : String;

    private
      procedure doOnChange        (       _sender     : TObject
                                  );
      procedure doColorListDrawItem(      _control    : TWinControl;
                                          _index      : Integer;
                                          _rect       : TRect;
                                          _state      : TOwnerDrawState
                                  );
    protected
      /// <inheritdoc from="TGIS_PvlContext"/>
      procedure fset_Value        ( const _value      : String
                                  );
                                  override;
    public
      /// <inheritdoc from="TGIS_PvlContext"/>
      procedure Fill              ( const _field      : Boolean;
                                    const _renderer   : Boolean
                                  );
                                  overload;

      /// <inheritdoc from="TGIS_PvlContext"/>
      procedure Fill              ( const _field      : Boolean;
                                    const _renderer   : Boolean;
                                    const _colors     : TStringList
                                  );
                                  overload;

      /// <inheritdoc from="TGIS_PvlContext"/>
      procedure SetFocus;         override;
  end;

  T_PvlCustomBitmapComboBox = class ( T_PvlCustomComboBox, IGIS_PvlCustomBitmapComboBox )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;

    protected

      ///  <summary>
      ///   Fields to be used if theres FieldFactor control involved.
      ///  </summary>
      FFields                     : TStringList;

      /// <summary>
      ///   Type of the combobox to determine items type.
      /// </summary>
      FType                       : TGIS_PvlCustomBitmapType;


      /// <summary>
      ///   Bitmap event
      /// </summary>
      FGetBitmapEvent : TGIS_PvlComboBoxHelperGetBitmapEvent;

    private
      procedure doCloseUp         (        _sender    : TObject
                                  );
      procedure doColorListDrawItem(       _control   : TWinControl;
                                           _index     : Integer;
                                           _rect      : TRect;
                                           _state     : TOwnerDrawState
                                  );
    protected
      /// <inheritdoc from="IGIS_CustomBitmapComboBox"/>
      procedure fset_Value        ( const _value      : String
                                  );
                                  override;

      /// <inheritdoc from="IGIS_CustomBitmapComboBox"/>
      function  fget_Type         : TGIS_PvlCustomBitmapType;

      /// <inheritdoc from="IGIS_CustomBitmapComboBox"/>
      function  fget_GetBitmapEvent
                                  : TGIS_PvlComboBoxHelperGetBitmapEvent;

      /// <inheritdoc from="IGIS_CustomBitmapComboBox"/>
      procedure fset_GetBitmapEvent(
                                    const _value      : TGIS_PvlComboBoxHelperGetBitmapEvent
                                  );
    public
      /// <inheritdoc from="IGIS_CustomBitmapComboBox"/>
      procedure FillPattern       ( const _hasSymbol  : Boolean
                                  );

      /// <inheritdoc from="IGIS_CustomBitmapComboBox"/>
      procedure FillStyle         ( const _hasSymbol  : Boolean
                                  );

      /// <inheritdoc from="IGIS_CustomBitmapComboBox"/>
      procedure FillMarker        ( const _hasSymbol  : Boolean
                                  );

      /// <inheritdoc from="IGIS_CustomBitmapComboBox"/>
      procedure FillShield;

      /// <inheritdoc from="IGIS_CustomBitmapComboBox"/>
      procedure SetFocus;         override;
  end;

  T_PvlColorRampComboBox = class ( T_PvlControl, IGIS_PvlColorRampComboBox )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
      /// <inheritdoc"/>
      procedure doDestroy;        override;

    private
      iTextWidth                  : Integer;
      iTextGap                    : Integer;

    protected
      /// <summary>
      ///   Flag for locking combobox from updates.
      /// </summary>
      bLock : Boolean;

    protected
      /// <summary>
      ///   OnChange Event.
      /// </summary>
      FOnChange                   : TGIS_PvlEvent;

      /// <summary>
      ///   Bitmaps of ramps.
      /// </summary>
      bmpMap                      : TGIS_ObjectList;

      /// <summary>
      ///   Underlaying Color Schemas of the color ramp combobox.
      /// </summary>
      FColorSchemas               : TGIS_ColorSchemas;

      /// <summary>
      ///   Underlying Mode of the color ramp combobox.
      /// </summary>
      FMode                       : TGIS_ColorMapMode;

      /// <summary>
      ///   If True then reverse ramp list in the combobox.
      /// </summary>
      FReverse                    : Boolean;

      /// <summary>
      ///   If False then draw ramp on the whole combobox width.
      ///   without a caption
      /// </summary>
      FShowNames                  : Boolean;

      /// <summary>
      ///   Array of indexes.
      /// </summary>
      AIdx                        : TGIS_IntegerArray;

    private
      procedure gradHorizontal    ( const _canvas     : TCanvas;
                                    const _rect       : TRect;
                                    const _fromColor  : TGIS_Color;
                                    const _toColor    : TGIS_Color;
                                    const _mode       : TGIS_ColorMapMode
                                  );
      procedure doRampListDrawItem(       Control     : TWinControl;
                                          Index       : Integer;
                                          Rect        : TRect;
                                          State       : TOwnerDrawState
                                  );
      procedure doOnChange        (       _sender     : TObject
                                  );

    protected
      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      function  fget_Mode         : TGIS_ColorMapMode;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      procedure fset_Mode         ( const _value      : TGIS_ColorMapMode
                                  );

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      function  fget_Index        : Integer;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      procedure fset_Index        ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      function  fget_Item         ( const _idx        : Integer
                                  ) : String;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      function  fget_ItemCount    : Integer;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>

      function  fget_ColorSchemas : TGIS_ColorSchemas;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      procedure fset_ColorSchemas ( const _value      : TGIS_ColorSchemas
                                  );

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      function  fget_Reverse      : Boolean;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      procedure fset_Reverse      ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      function  fget_ShowNames    : Boolean;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      procedure fset_ShowNames    ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  );
      /// <inheritdoc from="IGIS_PvlColorRampComboBox"/>
      procedure fset_Height       ( const _value      : Integer
                                  );
                                  override;
    public
      function  Value             ( const _subClass   : Integer = -1
                                  ) : TGIS_ColorMapArray;
      procedure Fill;
      procedure Lock;
      procedure Unlock;
      procedure SetFocus;         override;
  end;

  T_PvlComboBox = class( T_PvlControl, IGIS_PvlComboBox )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;

    protected
      /// <summary>
      ///   OnChange Event.
      /// </summary>
      FOnChange                   : TGIS_PvlEvent;

      /// <summary>
      ///   Stores a NativeInt integral value as a part of a component.
      /// </summary>
      FTag                        : NativeInt;

    private
      procedure doOnChange        (       _sender     : TObject
                                  );
                                  virtual;
    protected

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_ItemsCount   : Integer;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_Item         ( const _idx        : Integer
                                  ) : String;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_ItemIndex    : Integer;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_ItemIndex    ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_Text         : String;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_Text         ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_Tag          : NativeInt;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_Tag          ( const _value      : NativeInt
                                  );

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_Sorted       : Boolean;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_Sorted       ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_DropDownCount: Integer;


      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_DropDownCount( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  );

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure fset_Height       ( const _value      : Integer
                                 );
                                 override;

    public
      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure BeginUpdate;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure EndUpdate;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure ItemsClear;       virtual;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure ItemsAdd          ( const _item       : String
                                  );
                                  virtual;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  IndexOf           ( const _item       : String
                                  ) : Integer;

      procedure SetFocus;         override;
  end;

  T_PvlComboEdit = class ( T_PvlComboBox, IGIS_PvlComboEdit )
    private
      lstItems                    : TGIS_ListOfStrings ;
      bFilteredSearch             : Boolean ;
      bUpDown                     : Boolean ;
      bLeftRight                  : Boolean ;
      iSel                        : Integer ;
      prevIdx                     : Integer ;
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;

      /// <inheritdoc from="IGIS_PvlComboEdit"/>
      function  fget_FilteredSearch
                                  : Boolean;

      /// <inheritdoc from="IGIS_PvlComboEdit"/>
      procedure fset_FilteredSearch
                                  ( const _value      : Boolean
                                  );
      /// <inheritdoc"/>
      procedure doDestroy;        override;
    private
      procedure doOnChange        (       _sender     : TObject
                                  );
                                  override;
      procedure doOnKeyUp         (       _sender     : TObject;
                                    var   _key        : Word;
                                          _shift      : TShiftState
                                  );
      procedure doOnKeyDown       (       _sender     : TObject;
                                    var   _key        : Word;
                                          _shift      : TShiftState
                                  );
    public
      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure ItemsAdd          ( const _item       : String
                                  );
                                  override;
      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure ItemsClear;       override;
  end;


  T_PvlRadioButton = class ( T_PvlControl, IGIS_PvlRadioButton )
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
    protected
      /// <summary>
      ///   OnClick Event.
      /// </summary>
      FOnClick                    : TGIS_PvlEvent;

   private
      procedure doOnClick         (       _sender     : TObject
                                  );
    protected
      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      function  fget_Checked      : Boolean;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure fset_Checked      ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      function  fget_Group        : String;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure fset_Group        ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      function  fget_Caption      : String;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure fset_Caption      ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      function  fget_FontSize     : Integer;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure fset_FontSize     ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      function  fget_FontStyle    : TGIS_FontStyles;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                  );

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      function  fget_FontFamily   : String;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure fset_FontFamily   ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      function  fget_OnClick      : TGIS_PvlEvent;


      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure fset_OnClick      ( const _value      : TGIS_PvlEvent
                                  );
    public
      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure SetFocus;         override;

      /// <inheritdoc from="IGIS_PvlRadioButton"/>
      procedure UncheckGroup      ( const _button     : TGIS_PvlRadioButton
                                  );
  end;

  T_PvlCheckBox = class ( T_PvlControl, IGIS_PvlCheckBox )
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
    protected
      /// <summary>
      ///   OnClick Event.
      /// </summary>
      FOnClick                    : TGIS_PvlEvent;

    private
      procedure doOnClick         (       _sender     : TObject
                                  );
    protected
      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      function  fget_Checked      : Boolean;

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      procedure fset_Checked      ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      function  fget_Caption      : String;

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      procedure fset_Caption      ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      function  fget_FontSize     : Integer;

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      procedure fset_FontSize     ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      function  fget_FontStyle    : TGIS_FontStyles;

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                  );

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      function  fget_FontFamily   : String;

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      procedure fset_FontFamily   ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      function  fget_OnClick      : TGIS_PvlEvent;


      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      procedure fset_OnClick      ( const _value      : TGIS_PvlEvent
                                  );
    public
      /// <inheritdoc from="IGIS_PvlCheckBox"/>
      procedure SetFocus;         override;
  end;

  T_PvlPanel = class ( T_PvlControl, IGIS_PvlPanel )
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
      /// <inheritdoc"/>
      procedure doDestroy;        override;
    private

      /// <summary>
      ///   Map of controls with their id as a key
      /// </summary>
      itmMap                      : TDictionary<String, TGIS_PvlControl>;

    protected
      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Scrollable   : Boolean;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Scrollable   ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  fget_Border       : Boolean;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure fset_Border       ( const _value      : Boolean
                                  );

    public

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure AddComponent      ( const _component  : TGIS_PvlControl
                                  );
                                  overload;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure AddComponent      ( const _component  : TGIS_PvlControl;
                                    const _id         : String
                                  );
                                  overload;

      /// <inheritdoc from="IGIS_PvlControl"/>
      procedure RemoveAllComponents;

      /// <inheritdoc from="IGIS_PvlControl"/>
      function  GetById           ( const _id         : String
                                  ) : TGIS_PvlControl;
  end;

  T_PvlPreviewPanel = class( T_PvlControl, IGIS_PvlPreviewPanel  )
     protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
    private
      /// <summary>
      ///   Styled symbol color
      /// </summary>
      cStyledAreaColor            : TGIS_Color;

      bBitmap                     : Boolean;

    protected
      oLabel                      : TLabel;
      oImage                      : TImage;

    private
      procedure applyStyle;

    protected
      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_IsStyled     : Boolean;
                                  override;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_Caption      : String;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      procedure fset_Caption      ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_FontSize     : Integer;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      procedure fset_FontSize     ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_FontStyle    : TGIS_FontStyles;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      procedure fset_FontStyle    ( const _value      : TGIS_FontStyles
                                  );

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_FontFamily   : String;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      procedure fset_FontFamily   ( const _value      : String
                                  );

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_Border       : Boolean;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      procedure fset_Border       ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_Bitmap       : TGIS_Bitmap;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      procedure fset_Bitmap       ( const _value      : TGIS_Bitmap
                                  );

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_Color        : TGIS_Color;

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      procedure fset_Color        ( const _value      : TGIS_Color
                                  );

      /// <inheritdoc from="IGIS_PvlPreviewPanel"/>
      function  fget_StyledAreaColor
                                  : TGIS_Color;
    public
      procedure Invalidate;
      procedure DoRedraw;         override;
      procedure SetFocus;         override;
  end;

  T_PvlGroupBox = class ( T_PvlControl, IGIS_PvlGroupBox )
    public
      /// <inheritdoc/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
    protected
      function  fget_Caption      : String;
      procedure fset_Caption      ( const _value      : String
                                  );
  end;

  T_PvlTrackBar = class( T_PvlControl, IGIS_PvlTrackBar )
    protected
      FOnChange                   : TGIS_PvlEvent;
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
    protected

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      function  fget_Minimum      : Integer;

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      procedure fset_Minimum      ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      function  fget_Maximum      : Integer;

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      procedure fset_Maximum      ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      function  fget_Position     : Integer;

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      procedure fset_Position     ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  );
    private
      procedure doOnChange        (       _sender     : TObject
                                  );
    public
      procedure SetFocus;         override;
  end;

  T_PvlTree = class ( T_PvlControl, IGIS_PvlTree )
    protected
      FOnClick                    : TGIS_PvlEvent;
      FOnSelectChange             : TGIS_PvlEvent;
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
      /// <inheritdoc"/>
      procedure doDestroy;        override;

    private
      oRoot : TGIS_PvlTreeNode;
    protected
      /// <inheritdoc from="IGIS_PvlTree"/>
      function  fget_Root         : TGIS_PvlTreeNode;

      /// <inheritdoc from="IGIS_PvlTree"/>
      function  fget_Selected     : TGIS_PvlTreeNode;

      /// <inheritdoc from="IGIS_PvlTree"/>
      procedure fset_Selected     ( const _value      : TGIS_PvlTreeNode
                                  );

      /// <inheritdoc from="IGIS_PvlTree"/>
      procedure fset_OnClick      ( const _value      : TGIS_PvlEvent
                                  );

      /// <inheritdoc from="IGIS_PvlTree"/>
      function  fget_OnClick      : TGIS_PvlEvent ;

      /// <inheritdoc from="IGIS_PvlTree"/>
      procedure fset_OnSelectChange
                                  ( const _value      : TGIS_PvlEvent
                                  );

      /// <inheritdoc from="IGIS_PvlTree"/>
      function  fget_OnSelectChange
                                  : TGIS_PvlEvent ;
    public
      /// <inheritdoc from="IGIS_PvlTree"/>
      procedure CreateRoot;

    public
      procedure doOnClick         (       _sender     : TObject
                                  );
      procedure doOnSelectChange  (       _sender     : TObject;
                                           _node      : TTreeNode
                                  );
      procedure doOnDeletion      (       _sender     : TObject;
                                          _node       : TTreeNode
                                  );
    public
      procedure SetFocus;         override;
  end;

  T_PvlTreeNode = class ( TGIS_PvlBaseVCL, IGIS_PvlTreeNode )
    public
      constructor Create          ( const _context    : TGIS_PvlContext   ;
                                    const _parent     : IGIS_PvlBase
                                  );
                                  overload; override;
    protected
      procedure doDestroy;        override;

    private
      sCaption                    : String;
    protected
      function  fget_Caption      : String;
      procedure fset_Caption      ( const _value      : String
                                  );
      function  fget_NativeControl: TObject;
                                  override;
      procedure fset_NativeControl( const _value        : TObject
                                  );
      function  fget_Parent       : TGIS_PvlTreeNode;

      function  fget_Node         ( const _index      : Integer
                                  ) : TGIS_PvlTreeNode;
    public
      procedure  doCreateNode     ( const _node       : TGIS_PvlTreeNode;
                                    const _caption    : String;
                                    const _index      : Integer
                                  );
      procedure  doRemoveNode     ( const _node       : TGIS_PvlTreeNode
                                  );
      procedure  doDeleteNode     ( const _node       : TGIS_PvlTreeNode
                                  );
      procedure  doMoveNode       ( const _node       : TGIS_PvlTreeNode;
                                    const _newParent  : TGIS_PvlTreeNode;
                                    const _index      : Integer
                                  );

    protected
      function  fget_Expanded     : Boolean;
      procedure fset_Expanded     ( const _value      : Boolean
                                  );
  end;

  T_PvlListBox = class( T_PvlControl, IGIS_PvlListBox )
    private
      lstItm : TGIS_ListOfStrings ;
      lstSel : TGIS_ListOfStrings ;
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
      /// <inheritdoc"/>
      procedure doDestroy;        override;

    private
      FOnClick                    : TGIS_PvlEvent;

    private
      procedure doOnClick         (       _sender     : TObject
                                  );

    private
      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_ItemList     : TGIS_ListOfStrings;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_ItemsCount   : Integer;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_Item         ( const _idx        : Integer
                                  ) : String;
      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_ItemIndex    : Integer;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure fset_ItemIndex    ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_SelectedItems: TGIS_ListOfStrings;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_Multiselect  : Boolean;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure fset_Multiselect  ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_Selected     ( const _index      : Integer
                                  ) : Boolean ;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure fset_Selected     ( const _index      : Integer ;
                                    const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlListBox"/>
      function  fget_OnClick      : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure fset_OnClick      ( const _value      : TGIS_PvlEvent
                                  );
    public
      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure BeginUpdate;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure EndUpdate;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure ItemsClear;

      /// <inheritdoc from="IGIS_PvlListBox"/>
      procedure ItemsAdd          ( const _item       : String
                                  );
      procedure SetFocus;         override;
  end;

  T_PvlSVGList = class( T_PvlControl, IGIS_PvlSVGList )
    private
      lstItm : TGIS_ListOfStrings ;
     protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
      /// <inheritdoc"/>
      procedure doDestroy;        override;

    private
      oLib                        : TGIS_SymbolLibrarySVG;
      lstNames                    : TStringList;
      svgList                     : TImageList;
      cForeground                 : TGIS_Color;
      cBackground                 : TGIS_Color;

    private
      procedure applyStyle;
      procedure doOnClick         (       _sender     : TObject
                                  );
    private
      FOnClick                    : TGIS_PvlEvent;
    protected
      /// <inheritdoc from="IGIS_PvlSVGList"/>
      function  fget_ItemList     : TGIS_ListOfStrings;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      function  fget_ItemsCount   : Integer;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      function  fget_Item         ( const _idx        : Integer
                                  ) : String;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      function  fget_SelectedItems: TGIS_ListOfStrings;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      function  fget_Multiselect  : Boolean;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      procedure fset_Multiselect  ( const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      function  fget_Selected     ( const _index      : Integer
                                  ) : Boolean ;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      procedure fset_Selected     ( const _index      : Integer ;
                                    const _value      : Boolean
                                  );

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      function  fget_ItemIndex    : Integer;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      procedure fset_ItemIndex    ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      function  fget_OnClick      : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      procedure fset_OnClick      ( const _value      : TGIS_PvlEvent
                                  );

    public
      /// <inheritdoc from="IGIS_PvlSVGList"/>
      procedure BeginUpdate;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      procedure EndUpdate;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      procedure ItemsClear;

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      procedure ItemsAdd          ( const _item       : String
                                  );

      /// <inheritdoc from="IGIS_PvlSVGList"/>
      procedure SetFocus;         override;

    public
      procedure DoRedraw;         override;
  end;

const
  SVG_SIZE = 64;

{$REGION 'TGIS_ImagePanel'}
constructor TGIS_ImagePanel.Create(
    _owner : TComponent
  );
begin
  inherited;
  DoubleBuffered := true;
end;

procedure TGIS_ImagePanel.Paint;
var
  w, h   : Integer;
  rw, rh : Integer;
begin
  inherited;

  if Assigned( OnPaintEvent ) then
    OnPaintEvent( Self )
  else begin
    if assigned(Image) then begin
      w := Image.Width;
      h := Image.Height;
      if Stretch then begin
        if ( w <= Width ) and ( h <= Height ) then begin
          rw := w;
          rh := h;
        end else begin
          if ( w / Width ) > ( h / Height ) then begin
            rw := Width;
            rh := RoundS(Width / w * h);
          end else begin
            rw := RoundS(Height / h * w);
            rh := Height;
          end;
        end;

        Canvas.FillRect( TRect.Create( 0, 0, Width, Height ) );
        Canvas.Draw( (Width - rw) div 2, (Height - rh) div 2, Image );
      end else begin
        Canvas.Brush.Color := Color;
        Canvas.FillRect( TRect.Create( 0, 0, w, h ) );
        Canvas.Draw( 0, 0, Image); //( Image, 0, 0, w, h );
      end;
    end;
  end;
end;
{$ENDREGION}

{$REGION 'T_SpeedButton'}

  procedure T_SpeedButton.Paint;
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

{$REGION 'T_PvlColorPreviewPanel'}

constructor T_PvlColorPreviewPanel.Create(
  _owner : TComponent
);
begin
  inherited;
  DoubleBuffered := True;
end;

procedure T_PvlColorPreviewPanel.Paint;
begin
  if Border and ( Color = TGIS_Color.None ) then
    Border := False;

  if Color = TGIS_Color.None then
    exit;

  draw_color( Canvas, Color, TRect.Create( 0, 0, Width, Height ), CellSize, Enabled );
//  Border

  if Enabled then
    Canvas.Pen.Color := TColor( $C9CED2 )
  else
    Canvas.Pen.Color := TColor( $888888 );
  Canvas.Brush.Style := TBrushStyle.bsClear;

  Canvas.Rectangle( 0, 0, Width, Height );
end;

{$ENDREGION 'T_PvlColorPreviewPanel'}

{$REGION 'T_dpiManager'}

constructor T_dpiManager.Create(
  _owner : TComponent
);
begin
  inherited;

  oForm := TForm( _owner );

  Name := DPI_MANAGER;

  {$IFDEF LEVEL_RX101_VCL}
    oldBeforeMonitorDpiChanged := oForm.OnBeforeMonitorDpiChanged;
    oldAfterMonitorDpiChanged := oForm.OnAfterMonitorDpiChanged;
    oForm.OnBeforeMonitorDpiChanged := doBeforeMonitorDpiChanged;
    oForm.OnAfterMonitorDpiChanged := doAfterMonitorDpiChanged;
  {$ENDIF}
end;

{$IFDEF LEVEL_RX101_VCL}
  procedure T_dpiManager.doBeforeMonitorDpiChanged(
    _sender : TObject;
    _oldDPI : Integer;
    _newDPI: Integer
  );
  begin
    if Assigned( oldBeforeMonitorDpiChanged ) then
      oldBeforeMonitorDpiChanged( _sender, _oldDPI, _newDPI );

    iPPI := _newDPI;
  end;

  procedure T_dpiManager.doAfterMonitorDpiChanged(
    _sender : TObject;
    _oldDPI : Integer;
    _newDPI: Integer
  );
  begin
    if Assigned( oldAfterMonitorDpiChanged ) then
      oldAfterMonitorDpiChanged( _sender, _oldDPI, _newDPI );

    iPPI := _newDPI;
    dPPIFix := iPPI/96;

    ApplyPPI( self, iPPI, dPPIFix, True ) ;
  end;
{$ENDIF}

{$ENDREGION 'T_xyz'}

{$REGION 'T_component'}

constructor T_component.Create(
  _owner : TComponent
);
begin
  inherited;
  lstControls := TObjectList<T_PvlControl>.Create;
end;

destructor T_component.Destroy;
begin
  FreeObject( lstControls );
  FreeObject( oOwner );
  FreeObject( oMaster );
  inherited;
end;

procedure T_component.Add(
  const _control : T_PvlControl
);
begin
  lstControls.Add( _control );
end;

{$ENDREGION 'T_component'}

{$REGION 'TGIS_PvlBaseVCL'}

constructor TGIS_PvlBaseVCL.Create(
  const _context : TGIS_PvlContext;
  const _parent  : IGIS_PvlBase
);
begin
  oContext := _context ;
  oParent  := _parent ;
end;

function TGIS_PvlBaseVCL.Parent
  : TGIS_PvlBase ;
begin
  Result := oParent as TGIS_PvlBase ;
end;

function TGIS_PvlBaseVCL.fget_NativeControl: TObject ;
begin
  Result := oControl ;
end;

{$ENDREGION 'TGIS_PvlBaseFmx'}

{$REGION 'T_PvlControl'}
constructor T_PvlControl.Create(
  const _context : TGIS_PvlContext ;
  const _parent  : IGIS_PvlBase
);
begin
  inherited Create( _context, _parent );

  doCreate( _context );;

  T_PvlContext( _context.Platform ).oOwner.Add( self );
end;

procedure T_PvlControl.doDestroy;
begin
  TGIS_PvlControl( oParent ).Platform := nil ;
  inherited;
end;

procedure T_PvlControl.DoRedraw;
begin
  // only for safe inheritance
end;

procedure T_PvlControl.add(
  const _control: TObject
);
begin
  TControl( _control ).Parent := TWinControl( oControl );
end;

function T_PvlControl.fget_NativeControl
  : TObject;
begin
  Result := oControl;
end;

function T_PvlControl.fget_IsStyled
  : Boolean;
begin
  Result := True;
end;

function  T_PvlControl.fget_Anchors
  : TGIS_PvlAnchors;
begin
  Result := [];
  if TAnchorKind.akLeft in TControl( oControl ).Anchors then
    Result := Result + [TGIS_PvlAnchor.Left];
  if TAnchorKind.akTop in TControl( oControl ).Anchors then
    Result := Result + [TGIS_PvlAnchor.Top];
  if TAnchorKind.akRight in TControl( oControl ).Anchors then
    Result := Result + [TGIS_PvlAnchor.Right];
  if TAnchorKind.akBottom in TControl( oControl ).Anchors then
    Result := Result + [TGIS_PvlAnchor.Bottom];
end;

procedure T_PvlControl.fset_Anchors(
  const _value : TGIS_PvlAnchors
);
var
  anchors : TAnchors;
begin
  anchors := [];

  if TGIS_PvlAnchor.Left in _value then
    anchors := anchors + [TAnchorKind.akLeft];
  if TGIS_PvlAnchor.Top in _value then
    anchors := anchors + [TAnchorKind.akTop];
  if TGIS_PvlAnchor.Right in _value then
    anchors := anchors + [TAnchorKind.akRight];
  if TGIS_PvlAnchor.Bottom in _value then
    anchors := anchors + [TAnchorKind.akBottom];
  TControl( oControl ).Anchors := anchors;
end;

function  T_PvlControl.fget_Align
  : TGIS_PvlAlign;
begin
  case TControl( oControl ).Align of
    TAlign.alNone   : Result := TGIS_PvlAlign.None;
    TAlign.alClient : Result := TGIS_PvlAlign.Client;
    TAlign.alTop    : Result := TGIS_PvlAlign.Top;
    TAlign.alLeft   : Result := TGIS_PvlAlign.Left;
    TAlign.alRight  : Result := TGIS_PvlAlign.Right;
    TAlign.alBottom : Result := TGIS_PvlAlign.Bottom;
    else              begin
                        fset_Align( TGIS_PvlAlign.None );
                        Result := TGIS_PvlAlign.None;
                      end;
  end;

end;

procedure T_PvlControl.fset_Align(
  const _value : TGIS_PvlAlign
);
begin
  case _value of
    TGIS_PvlAlign.None    : TControl( oControl ).Align := TAlign.alNone;
    TGIS_PvlAlign.Client  : TControl( oControl ).Align := TAlign.alClient;
    TGIS_PvlAlign.Top     : TControl( oControl ).Align := TAlign.alTop;
    TGIS_PvlAlign.Left    : TControl( oControl ).Align := TAlign.alLeft;
    TGIS_PvlAlign.Bottom  : TControl( oControl ).Align := TAlign.alBottom;
    TGIS_PvlAlign.Right   : TControl( oControl ).Align := TAlign.alRight;
    else                    begin
                              assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) );
                            end;
  end;
end;

function T_PvlControl.fget_Enabled
  : Boolean;
begin
  Result := TControl( oControl ).Enabled;
end;

procedure T_PvlControl.fset_Enabled(
  const _value : Boolean
);
begin
  TControl( oControl ).Enabled := _value;
end;

function T_PvlControl.fget_Width
  : Integer;
begin
  Result := Round( TControl( oControl ).Width / oContext.PPIFix );
end;

procedure T_PvlControl.fset_Width(
  const _value : Integer
);
begin
  TControl( oControl ).Width := Round( _value * oContext.PPIFix );
end;

function T_PvlControl.fget_Height
  : Integer;
begin
  Result := Round( TControl( oControl ).Height / oContext.PPIFix );
end;

procedure T_PvlControl.fset_Height(
  const _value : Integer
);
begin
  TControl( oControl ).Height := Round( _value * oContext.PPIFix );
end;

function T_PvlControl.fget_Left
  : Integer;
begin
  Result := Round( TControl( oControl ).Left / oContext.PPIFix );
end;

procedure T_PvlControl.fset_Left(
  const _value : Integer
);
begin
  TControl( oControl ).Left := Round( _value * oContext.PPIFix );
end;

function T_PvlControl.fget_Top
  : Integer;
begin
  Result := Round( TControl( oControl ).Top / oContext.PPIFix );
end;

procedure T_PvlControl.fset_Top(
  const _value : Integer
);
begin
  TControl( oControl ).Top := Round( _value * oContext.PPIFix );
end;

function T_PvlControl.fget_TabOrder
  : Integer;
begin
  Result := 0;
end;

procedure T_PvlControl.fset_TabOrder(
  const _value : Integer
);
begin
//  do nothing no tab order for TControl type
end;

function T_PvlControl.fget_Visible
  : Boolean;
begin
  Result := TControl( oControl ).Visible;
end;

procedure T_PvlControl.fset_Visible(
  const _value : Boolean
);
begin
  TControl( oControl ).Visible := _value;
end;

function T_PvlControl.fget_Hint
  : String;
begin
  Result := TControl( oControl ).Hint;
end;

procedure T_PvlControl.fset_Hint(
  const _value : String
);
begin
  TControl( oControl ).Hint := _value;
end;

procedure T_PvlControl.SetFocus;
begin
//  No set focus for control
//  TControl( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlControl'}

{$REGION 'T_PvlLabel'}

procedure T_PvlLabel.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TLabel.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );
  TLabel( oControl ).AutoSize := False;
  TLabel( oControl ).Layout := tlCenter;

  oFocusControl := nil;
end;

function T_PvlLabel.fget_Caption
  : String;
begin
  Result := TLabel( oControl ).Caption;
end;

procedure T_PvlLabel.fset_Caption(
  const _value : String
);
begin
  TLabel( oControl ).Caption := _value;
end;

function T_PvlLabel.fget_FocusControl
 : TGIS_PvlControl;
begin
  Result := oFocusControl;
end;

procedure T_PvlLabel.fset_FocusControl(
  const _value : TGIS_PvlControl
);
begin
  oFocusControl := _value;
  TLabel( oControl ).FocusControl := TWinControl( oFocusControl.NativeControl );
end;

function T_PvlLabel.fget_FontSize
  : Integer;
begin
  Result := TLabel( oControl ).Font.Size;
end;

procedure T_PvlLabel.fset_FontSize(
  const _value : Integer
);
begin
  TLabel( oControl ).Font.Size := _value;
end;

function T_PvlLabel.fget_FontStyle
  : TGIS_FontStyles;
var
  iStyle : TGIS_FontStyles;
begin
  iStyle := GisGetEmptyFontStyle;
  if TFontStyle.fsBold in TLabel( oControl ).Font.Style then
    iStyle := GisAddFontStyle( iStyle, TGIS_FontStyle.Bold );
  if TFontStyle.fsItalic in TLabel( oControl ).Font.Style then
    iStyle := GisAddFontStyle( iStyle, TGIS_FontStyle.Italic );
  if TFontStyle.fsUnderline in TLabel( oControl ).Font.Style then
    iStyle := GisAddFontStyle( iStyle, TGIS_FontStyle.Underline );
  if TFontStyle.fsStrikeOut in TLabel( oControl ).Font.Style then
    iStyle := GisAddFontStyle( iStyle, TGIS_FontStyle.StrikeOut );
  Result := iStyle;
end;

procedure T_PvlLabel.fset_FontStyle(
  const _value : TGIS_FontStyles
);
begin
  if TGIS_FontStyle.Bold in _value then
    TLabel( oControl ).Font.Style := TLabel( oControl ).Font.Style + [ TFontStyle.fsBold ];
  if TGIS_FontStyle.Italic in _value then
    TLabel( oControl ).Font.Style := TLabel( oControl ).Font.Style + [ TFontStyle.fsItalic ];
  if TGIS_FontStyle.Underline in _value then
    TLabel( oControl ).Font.Style := TLabel( oControl ).Font.Style + [ TFontStyle.fsUnderline ];
  if TGIS_FontStyle.StrikeOut in _value then
    TLabel( oControl ).Font.Style := TLabel( oControl ).Font.Style + [ TFontStyle.fsStrikeOut ];
end;

function T_PvlLabel.fget_FontFamily
  : string;
begin
  Result := TLabel( oControl ).Font.Name;
end;

procedure T_PvlLabel.fset_FontFamily(
  const _value : string
);
begin
  TLabel( oControl ).Font.Name := _value;
end;

function T_PvlLabel.fget_Alignment
  : TGIS_PvlLabelTextAlignment;
begin
  if TLabel( oControl ).Alignment = taLeftJustify then
    Result := TGIS_PvlLabelTextAlignment.Left
  else if TLabel( oControl ).Alignment = taCenter then
    Result := TGIS_PvlLabelTextAlignment.Center
  else if TLabel( oControl ).Alignment = taRightJustify then
    Result := TGIS_PvlLabelTextAlignment.Right
  else
    Result := TGIS_PvlLabelTextAlignment.Left
end;

procedure T_PvlLabel.fset_Alignment(
  const _value : TGIS_PvlLabelTextAlignment
);
begin
  if _value = TGIS_PvlLabelTextAlignment.Left then
    TLabel( oControl ).Alignment := taLeftJustify
  else if _value = TGIS_PvlLabelTextAlignment.Center then
    TLabel( oControl ).Alignment := taCenter
  else if _value = TGIS_PvlLabelTextAlignment.Right then
    TLabel( oControl ).Alignment := taRightJustify;
end;

procedure T_PvlLabel.SetFocus;
begin
//  No set focus for labels
//  TLabel( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlLabel'}

{$REGION 'T_PvlIconButton'}

procedure T_PvlIconButton.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := T_SpeedButton.Create( TWinControl( _context.NativeParent ) );

  TControl( oControl ).Parent := TWinControl( _context.NativeParent );

  T_SpeedButton( oControl ).OnClick := doOnClick;
end;

procedure T_PvlIconButton.DoRedraw;
var
  tmp: TGIS_Bitmap;
  bmp: TBitmap;
  smb: TGIS_SymbolAbstract;
  clr: TGIS_Color;
begin
  if not Assigned( TGIS_PvlIconButton( oParent ).IconsList) then
    exit;

  smb := TGIS_PvlIconButton( oParent ).IconsList.Icon[TGIS_PvlIconButton( oParent ).IconIndex];
  if not Assigned(smb) then
    exit;
  tmp := TGIS_Bitmap.Create(RoundS(TGIS_PvlIconButton( oParent ).IconSize * oContext.PPIFix),
    RoundS(TGIS_PvlIconButton( oParent ).IconSize * oContext.PPIFix));
  try
    if fget_Enabled then
      clr := GISColor( StyleServices.GetStyleFontColor(sfButtonTextNormal) )
    else
      clr := GISColor( StyleServices.GetStyleFontColor(sfButtonTextDisabled) );
    tmp.DrawGlyph(smb, oContext.PPI, clr, fget_Enabled);

    bmp := TBitmap(tmp.NativeBitmap);

    T_SpeedButton(oControl).Images := TImageList.Create(TWinControl(oControl));
    T_SpeedButton(oControl).Images.Height := tmp.Height;
    T_SpeedButton(oControl).Images.Width := tmp.Width;
    T_SpeedButton(oControl).Images.ColorDepth := TColorDepth.cd32Bit;
    T_SpeedButton(oControl).Images.AddMasked(bmp, bmp.TransparentColor);
    T_SpeedButton(oControl).ImageIndex := 0;
  finally
    FreeObject(tmp)
  end;
end;

procedure T_PvlIconButton.doOnClick(
  _sender : TObject
);
begin
  if not TGIS_PvlIconButton( oParent ).StayPressed then
    T_SpeedButton( oControl ).Down := False;

  if Assigned( FOnClick ) then
    FOnClick( Parent );
end;

function T_PvlIconButton.fget_Pushed
  : Boolean;
begin
  Result := T_SpeedButton( oControl ).Down;
end;

procedure T_PvlIconButton.fset_Pushed(
  const _value : Boolean
);
begin
  T_SpeedButton( oControl ).Down := _value;
end;

function T_PvlIconButton.fget_OnClick
  : TGIS_PvlEvent;
begin
  Result := FOnClick;
end;

procedure T_PvlIconButton.fset_OnCLick(
  const _value : TGIS_PvlEvent
);
begin
  FOnClick := _value;
end;

procedure T_PvlIconButton.SetFocus;
begin
//  No SetFocus on TSpeedButton
//  TSpeedButton( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlIconButton'}

{$REGION 'T_PvlButton'}

procedure T_PvlButton.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TButton.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );

  TButton( oControl ).OnClick := doOnClick;
end;

procedure T_PvlButton.doOnClick(
  _sender : TObject
);
begin
  if Assigned( FOnClick ) then
    FOnClick( Parent );
end;

function T_PvlButton.fget_Caption
  : String;
begin
  Result := TButton( oControl ).Caption;
end;

procedure T_PvlButton.fset_Caption(
  const _value : String
);
begin
  TButton( oControl ).Caption := _value;
end;

function T_PvlButton.fget_Default
  : Boolean;
begin
  Result := TButton( oControl ).Default;
end;

procedure T_PvlButton.fset_Default(
  const _value : Boolean
);
begin
  TButton( oControl ).Default := _value;
end;

function T_PvlButton.fget_OnClick
  : TGIS_PvlEvent;
begin
  Result := FOnClick;
end;

procedure T_PvlButton.fset_OnClick(
  const _value : TGIS_PvlEvent
);
begin
  FOnClick := _value;
end;

procedure T_PvlButton.SetFocus;
begin
  TButton( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlButton'}

{$REGION 'T_PvlModalButton'}

procedure T_PvlModalButton.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TButton.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );

  TButton( oControl ).OnClick := doOnClick;
end;

procedure T_PvlModalButton.doOnClick(
  _sender : TObject
);
begin
  if Assigned( FOnClick ) then
    FOnClick( Parent );
end;

function T_PvlModalButton.fget_Caption
  : String;
begin
  Result := TButton( oControl ).Caption;
end;

procedure T_PvlModalButton.fset_Caption(
  const _value : String
);
begin
  TButton( oControl ).Caption := _value;
end;

function T_PvlModalButton.fget_Default
  : Boolean;
begin
  Result := TButton( oControl ).Default;
end;

procedure T_PvlModalButton.fset_Default(
  const _value : Boolean
);
begin
  TButton( oControl ).Default := _value;
end;

function T_PvlModalButton.fget_OnClick
  : TGIS_PvlEvent;
begin
  Result := FOnClick;
end;

procedure T_PvlModalButton.fset_OnClick(
  const _value : TGIS_PvlEvent
);
begin
  FOnClick := _value;
end;

function T_PvlModalButton.fget_Parent
  : TObject;
begin
  Result := TButton( oControl ).Parent;
end;

procedure T_PvlModalButton.fset_Parent(
  const _value : TObject
);
begin
  TButton( oControl ).Parent := TWinControl( _value );
end;

procedure T_PvlModalButton.SetFocus;
begin
  TButton( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlModalButton'}

{$REGION 'T_PvlEdit'}

procedure T_PvlEdit.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TEdit.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );

  TEdit( oControl ).OnClick    := doOnClick;
  TEdit( oControl ).OnChange   := doOnChange;
  TEdit( oControl ).OnKeyDown  := doOnKeyDown;
  TEdit( oControl ).OnKeyPress := doOnKeyPress;
end;

procedure T_PvlEdit.doOnClick(
       _sender : TObject
);
begin
  if Assigned( FOnClick ) then
    FOnClick( Parent );
end;

procedure T_PvlEdit.doOnChange(
       _sender : TObject
);
begin
  if Assigned( FOnChange ) then
    FOnChange( Parent );
end;

procedure T_PvlEdit.doOnKeyDown(
       _sender : TObject;
   var _key    : Word;
       _shift  : TShiftState
);
begin
  if Assigned( FOnKeyDown ) then
    FOnKeyDown( Parent, _key );
end;

procedure T_PvlEdit.doOnKeyPress(
       _sender : TObject;
   var _char   : Char
);
begin
  if Assigned( FOnKeyPress ) then
    FOnKeyPress( Parent, _char );
end;

function T_PvlEdit.fget_Text
  : String;
begin
  Result := TEdit( oControl ).Text;
end;

procedure T_PvlEdit.fset_Text(
  const _value : String
);
begin
  TEdit( oControl ).Text := _value;
end;

function T_PvlEdit.fget_SelectionLength
  : Integer;
begin
  Result := TEdit( oControl ).SelLength;
end;

procedure T_PvlEdit.fset_SelectionLength(
  const _value : Integer
);
begin
  TEdit( oControl ).SelLength := _value;
end;

function T_PvlEdit.fget_SelectionStart
  : Integer;
begin
  Result := TEdit( oControl ).SelStart;
end;

procedure T_PvlEdit.fset_SelectionStart(
  const _value : Integer
);
begin
  TEdit( oControl ).SelStart := _value;
end;

function T_PvlEdit.fget_SelectedText
  : String;
begin
  Result := TEdit( oControl ).SelText;
end;

function T_PvlEdit.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := FOnChange;
end;

procedure T_PvlEdit.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  FOnChange := _value;
end;

function T_PvlEdit.fget_OnClick
  : TGIS_PvlEvent;
begin
  Result := FOnClick;
end;

procedure T_PvlEdit.fset_OnClick(
  const _value : TGIS_PvlEvent
);
begin
  FOnClick := _value;
end;

function T_PvlEdit.fget_OnKeyDown
  : TGIS_PvlKeyEvent;
begin
  Result := FOnKeyDown;
end;

procedure T_PvlEdit.fset_OnKeyDown(
  const _value : TGIS_PvlKeyEvent
);
begin
  FOnKeyDown := _value;
end;

function T_PvlEdit.fget_OnKeyPress
  : TGIS_PvlKeyPressEvent;
begin
  Result := FOnKeyPress;
end;

procedure T_PvlEdit.fset_OnKeyPress(
  const _value : TGIS_PvlKeyPressEvent
);
begin
  FOnKeyPress := _value;
end;

procedure T_PvlEdit.SetFocus;
begin
  TEdit( oControl ).SetFocus;
end;

procedure T_PvlEdit.SetFontAlarm;
begin
  TEdit( oControl ).Font.Color := clRed;
end;

procedure T_PvlEdit.SetFontDefault;
begin
  TEdit( oControl ).Font.Color := clWindowText;
end;

{$ENDREGION 'T_PvlEdit'}

{$REGION 'T_PvlMemo'}

procedure T_PvlMemo.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TMemo.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );

  TMemo( oControl ).ScrollBars := TScrollStyle.ssVertical ;

  TMemo( oControl ).OnClick    := doOnClick;
  TMemo( oControl ).OnChange   := doOnChange;
  TMemo( oControl ).OnKeyDown  := doOnKeyDown;
  TMemo( oControl ).OnKeyPress := doOnKeyPress;
end;

procedure T_PvlMemo.doOnClick(
       _sender : TObject
);
begin
  if Assigned( FOnClick ) then
    FOnClick( Parent );
end;

procedure T_PvlMemo.doOnChange(
       _sender : TObject
);
begin
  if Assigned( FOnChange ) then
    FOnChange( Parent );
end;

procedure T_PvlMemo.doOnKeyDown(
       _sender : TObject;
   var _key    : Word;
       _shift  : TShiftState
);
begin
  if Assigned( FOnKeyDown ) then
    FOnKeyDown( Parent, _key );
end;

procedure T_PvlMemo.doOnKeyPress(
       _sender : TObject;
   var _char   : Char
);
begin
  if Assigned( FOnKeyPress ) then
    FOnKeyPress( Parent, _char );
end;

function T_PvlMemo.fget_CursorPos
  : TPoint;
begin
  Result := TMemo( oControl ).CaretPos
end;

procedure T_PvlMemo.fset_CursorPos(
  const _value : TPoint
);
begin
  TMemo( oControl ).CaretPos := _value;
end;

function T_PvlMemo.fget_Text
  : String;
begin
  Result := TMemo( oControl ).Text;
end;

procedure T_PvlMemo.fset_Text(
  const _value : String
);
begin
  TMemo( oControl ).Text := _value;
end;

function T_PvlMemo.fget_SelectionLength
  : Integer;
begin
  Result := TMemo( oControl ).SelLength;
end;

procedure T_PvlMemo.fset_SelectionLength(
  const _value : Integer
);
begin
  TMemo( oControl ).SelLength := _value;
end;

function T_PvlMemo.fget_SelectionStart
  : Integer;
begin
  Result := TMemo( oControl ).SelStart;
end;

procedure T_PvlMemo.fset_SelectionStart(
  const _value : Integer
);
begin
  TMemo( oControl ).SelStart := _value;
end;

function T_PvlMemo.fget_SelectedText
  : String;
begin
  Result := TMemo( oControl ).SelText;
end;

function T_PvlMemo.fget_WordWrap
  : Boolean;
begin
  Result := TMemo( oControl ).WordWrap;
end;

procedure T_PvlMemo.fset_WordWrap(
  const _value : Boolean
);
begin
  TMemo( oControl ).WordWrap := _value;
end;

function T_PvlMemo.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := FOnChange;
end;

procedure T_PvlMemo.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  FOnChange := _value;
end;

function T_PvlMemo.fget_OnClick
  : TGIS_PvlEvent;
begin
  Result := FOnClick;
end;

procedure T_PvlMemo.fset_OnClick(
  const _value : TGIS_PvlEvent
);
begin
  FOnClick := _value;
end;

function T_PvlMemo.fget_OnKeyDown
  : TGIS_PvlKeyEvent;
begin
  Result := FOnKeyDown;
end;

procedure T_PvlMemo.fset_OnKeyDown(
  const _value : TGIS_PvlKeyEvent
);
begin
  FOnKeyDown := _value;
end;

function T_PvlMemo.fget_OnKeyPress
  : TGIS_PvlKeyPressEvent;
begin
  Result := FOnKeyPress;
end;

procedure T_PvlMemo.fset_OnKeyPress(
  const _value : TGIS_PvlKeyPressEvent
);
begin
  FOnKeyPress := _value;
end;

procedure T_PvlMemo.SetFocus;
begin
  TMemo( oControl ).SetFocus;
end;

procedure T_PvlMemo.SetFontAlarm;
begin
  TEdit( oControl ).Font.Color := clRed;
end;

procedure T_PvlMemo.SetFontDefault;
begin
  TEdit( oControl ).Font.Color := clWindowText;
end;

procedure T_PvlMemo.Clear;
begin
  TMemo( oControl ).Lines.Clear;
end;

procedure T_PvlMemo.AppendText(
  const _value : String
);
begin
  TMemo( oControl ).Text := TMemo( oControl ).Text + _value;
end;

procedure T_PvlMemo.AppendLine(
  const _value      : String
);
begin
  TMemo( oControl ).Lines.Add( _value );
end;

{$ENDREGION 'T_PvlMemo'}

{$REGION 'T_PvlColorPreview'}

procedure T_PvlColorPreview.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := T_PvlColorPreviewPanel.Create( TWinControl( _context.NativeParent ) );

  TControl( oControl ).Parent := TWinControl( _context.NativeParent );

  T_PvlColorPreviewPanel( oControl ).DoubleBuffered := True;
  T_PvlColorPreviewPanel( oControl ).CellSize := 8;
  T_PvlColorPreviewPanel( oControl ).Color := TGIS_Color.Black;

  T_PvlColorPreviewPanel( oControl ).OnClick := doOnClick;
end;

procedure T_PvlColorPreview.doOnClick(
  _sender: TObject
);
begin
  if Assigned( FOnClick ) then
    FOnClick( Parent );
end;

procedure T_PvlColorPreview.fset_Color(
  const _value : TGIS_Color
);
begin
  T_PvlColorPreviewPanel( oControl ).Color := _value;
  T_PvlColorPreviewPanel( oControl ).Repaint;
end;

function T_PvlColorPreview.fget_Color
  : TGIS_Color;
begin
  Result := T_PvlColorPreviewPanel( oControl ).Color;
end;

function T_PvlColorPreview.fget_Border
  : Boolean;
begin
  Result := T_PvlColorPreviewPanel( oControl ).Border
end;

procedure T_PvlColorPreview.fset_Border(
  const _value : Boolean
);
begin
  T_PvlColorPreviewPanel( oControl ).Border := _value;
end;

function T_PvlColorPreview.fget_OnClick
  : TGIS_PvlEvent;
begin
  Result := FOnClick;
end;

procedure T_PvlColorPreview.fset_OnClick(
  const _value : TGIS_PvlEvent
);
begin
  FOnClick := _value;
end;

procedure T_PvlColorPreview.SetFocus;
begin
  T_PvlColorPreviewPanel( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlColorPreview'}

{$REGION 'T_PvlColorWheel'}

procedure T_PvlColorWheel.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TGIS_ImagePanel.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );

  currX := RoundS(  fget_Width  / 2 );
  currY := RoundS(  fget_Height / 2 );

  prepareHue;

  bInit := False;
  bMouseDown := False;

  TGIS_ImagePanel( oControl ).OnPaintEvent := doOnPaint;
  TGIS_ImagePanel( oControl ).OnMouseDown  := doMouseDown;
  TGIS_ImagePanel( oControl ).OnMouseMove  := doMouseMove;
  TGIS_ImagePanel( oControl ).OnMouseUp    := doMouseUp;
end;

procedure T_PvlColorWheel.doDestroy;
begin
  FreeObject( colorWheel );
  FreeObject( currState );

  inherited;
end;

procedure T_PvlColorWheel.makeColorWheel;
var
  w2  : Integer;
  h2  : Integer;
  x   : Integer;
  y   : Integer;
  r   : Double;
  a   : Double;
  hue : TGIS_PvlRGBVal;
  i   : Integer;
  k   : Integer;
  ptr : pByteArray;
begin
  if not assigned( colorWheel ) then
    colorWheel := TBitmap.Create;
  colorWheel.PixelFormat := TPixelFormat.pf24bit;
  colorWheel.SetSize( RoundS( TGIS_ImagePanel( oControl ).Width ),
                      RoundS( TGIS_ImagePanel( oControl ).Height )
  );

  colorWheel.Canvas.Pen.Color := StyleServices.GetSystemColor( clBtnFace );
  colorWheel.Canvas.Pen.Style := TPenStyle.psSolid;
  colorWheel.Canvas.Brush.Color := StyleServices.GetSystemColor( clBtnFace );
  colorWheel.Canvas.Brush.Style := TBrushStyle.bsSolid;
  colorWheel.Canvas.Rectangle( 0, 0, TGIS_ImagePanel( oControl ).Width, TGIS_ImagePanel( oControl ).Height );

  w2 := RoundS( ( TGIS_ImagePanel( oControl ).Width  - GIS_COLOR_WHEEL_MARGIN )/2 );
  h2 := RoundS( ( TGIS_ImagePanel( oControl ).Height - GIS_COLOR_WHEEL_MARGIN )/2 );

  for i := GIS_COLOR_WHEEL_MARGIN2
      to colorWheel.Height - GIS_COLOR_WHEEL_MARGIN2 - 1 do begin

    ptr := colorWheel.ScanLine[i];
    y := h2 - i + GIS_COLOR_WHEEL_MARGIN2;

    for k := GIS_COLOR_WHEEL_MARGIN2
        to colorWheel.Width - GIS_COLOR_WHEEL_MARGIN2 - 1 do begin

      x := k - w2 - GIS_COLOR_WHEEL_MARGIN2;

      r := Sqrt( x*x + y*y )/w2;
      if r > 1.0 then
        continue;

      a := ArcTan2( y, x );

      hue := angleToRGB( a );

      ptr[3*k  ] := 255 - RoundS( r*hue.B );
      ptr[3*k+1] := 255 - RoundS( r*hue.G );
      ptr[3*k+2] := 255 - RoundS( r*hue.R );
    end;

  end;

  if not assigned( currState ) then
    currState := TBitmap.Create;
  currState.PixelFormat := TPixelFormat.pf24bit;
  currState.SetSize( TGIS_ImagePanel( oControl ).Width, TGIS_ImagePanel( oControl ).Height );

  bInit := True;
end;

procedure T_PvlColorWheel.drawCrosshair;
begin
  currState.Canvas.Draw( 0, 0, colorWheel );

  currState.Canvas.Pen.Style := TPenStyle.psSolid;
  currState.Canvas.Pen.Width := (1);

  currState.Canvas.Pen.Color := TColor( $000000 );
  currState.Canvas.MoveTo( currX - (1), currY - (2) );
  currState.Canvas.LineTo( currX - (1), currY - (7) );
  currState.Canvas.MoveTo( currX      , currY - (2) );
  currState.Canvas.LineTo( currX      , currY - (7) );
  currState.Canvas.Pen.Color := TColor( $FFFFFF );
  currState.Canvas.MoveTo( currX      , currY - (3) );
  currState.Canvas.LineTo( currX      , currY - (6) );
  currState.Canvas.Pen.Color := TColor( $000000 );
  currState.Canvas.MoveTo( currX + (1), currY - (2) );
  currState.Canvas.LineTo( currX + (1), currY - (7) );

  currState.Canvas.Pen.Color := TColor( $000000 );
  currState.Canvas.MoveTo( currX - (1), currY + (2) );
  currState.Canvas.LineTo( currX - (1), currY + (7) );
  currState.Canvas.MoveTo( currX      , currY + (2) );
  currState.Canvas.LineTo( currX      , currY + (7) );
  currState.Canvas.Pen.Color := TColor( $FFFFFF );
  currState.Canvas.MoveTo( currX      , currY + (3) );
  currState.Canvas.LineTo( currX      , currY + (6) );
  currState.Canvas.Pen.Color := TColor( $000000 );
  currState.Canvas.MoveTo( currX + (1), currY + (2) );
  currState.Canvas.LineTo( currX + (1), currY + (7) );

  currState.Canvas.Pen.Color := TColor( $000000 );
  currState.Canvas.MoveTo( currX - (2), currY - (1) );
  currState.Canvas.LineTo( currX - (7), currY - (1) );
  currState.Canvas.MoveTo( currX - (2), currY             );
  currState.Canvas.LineTo( currX - (7), currY             );
  currState.Canvas.Pen.Color := TColor( $FFFFFF );
  currState.Canvas.MoveTo( currX - (3), currY             );
  currState.Canvas.LineTo( currX - (6), currY             );
  currState.Canvas.Pen.Color := TColor( $000000 );
  currState.Canvas.MoveTo( currX - (2), currY + (1) );
  currState.Canvas.LineTo( currX - (7), currY + (1) );

  currState.Canvas.Pen.Color := TColor( $000000 );
  currState.Canvas.MoveTo( currX + (2), currY - (1) );
  currState.Canvas.LineTo( currX + (7), currY - (1) );
  currState.Canvas.MoveTo( currX + (2), currY             );
  currState.Canvas.LineTo( currX + (7), currY             );
  currState.Canvas.Pen.Color := TColor( $FFFFFF );
  currState.Canvas.MoveTo( currX + (3), currY             );
  currState.Canvas.LineTo( currX + (6), currY             );
  currState.Canvas.Pen.Color := TColor( $000000 );
  currState.Canvas.MoveTo( currX + (2), currY + (1) );
  currState.Canvas.LineTo( currX + (7), currY + (1) );
end;

procedure T_PvlColorWheel.prepareHue;
var
  seq : Integer;
  r   : Byte;
  g   : Byte;
  b   : Byte;
  i   : Integer;

  procedure do_w_0;
  begin
    inc( g );
    if g = 255 then
      inc( seq );
  end;

  procedure do_w_1;
  begin
    dec( r );
    if r = 0 then
      inc( seq );
  end;

  procedure do_w_2;
  begin
    inc( b );
    if b = 255 then
      inc( seq );
  end;

  procedure do_w_3;
  begin
    dec( g );
    if g = 0 then
      inc( seq );
  end;

  procedure do_w_4;
  begin
    inc( r );
    if r = 255 then
      inc( seq );
  end;

  procedure do_w_5;
  begin
    dec( b );
    if b = 0 then
      inc( seq );
  end;

begin
  SetLength( arrHue, GIS_COLOR_HUE_SIZE );


  {$IFDEF GIS_NORECORDS}
    for i := 0 to GIS_COLOR_HUE_SIZE - 1 do
      arrHue[i] := TGIS_PvlRGBVal.create;
  {$ENDIF}

  seq := 0;

  seq := 0;
  r := 255;
  g := 0;
  b := 0;

  arrHue[0].R := r;
  arrHue[0].G := g;
  arrHue[0].B := b;

  for i := 1 to GIS_COLOR_HUE_SIZE - 1 do begin

    case seq of
      0 : do_w_0;
      1 : do_w_1;
      2 : do_w_2;
      3 : do_w_3;
      4 : do_w_4;
      5 : do_w_5;
    end;

    arrHue[i].R := r;
    arrHue[i].G := g;
    arrHue[i].B := b;

  end;
end;

function T_PvlColorWheel.angleToRGB(
  const _phi : Double
) : TGIS_PvlRGBVal;
var
  i : Integer;
begin
  i := FloorS( ( ( Pi + _phi )/( 2 * Pi ) ) * GIS_COLOR_HUE_SIZE );

  if i < GIS_COLOR_HUE_SIZE then
    Result := arrHue[i]
  else
    Result := arrHue[0];
end;

function T_PvlColorWheel.checkMouse(
  const _x : Integer;
  const _y : Integer
) : Boolean;
var
  w2 : Integer;
  h2 : Integer;
  x  : Integer;
  y  : Integer;
  r  : Double;
begin
  Result := True;

  w2 := RoundS( ( fget_Width * oContext.PPIFix * oContext.CanvasScale  - GIS_COLOR_WHEEL_MARGIN )/2 );
  h2 := RoundS( ( fget_Height * oContext.PPIFix * oContext.CanvasScale - GIS_COLOR_WHEEL_MARGIN )/2 );

  y := h2 - RoundS( _y * oContext.CanvasScale ) + GIS_COLOR_WHEEL_MARGIN2;
  x := RoundS( _x * oContext.CanvasScale ) - w2 - GIS_COLOR_WHEEL_MARGIN2;

  r := Sqrt( x*x + y*y )/w2;
  if r > 1.0 then
    Result := False;
end;

procedure T_PvlColorWheel.doOnPaint(
  _e : TObject
);
begin

  if ( not bInit ) or ( oContext.PPIFix <> oldPPI ) then begin
    oldPPI := oContext.PPI;
    makeColorWheel;
  end;

  drawCrosshair;

  TGIS_ImagePanel( oControl ).Canvas.Draw( 0, 0, currState );
end;

procedure T_PvlColorWheel.doMouseDown(
  _sender : TObject;
  _button : TMouseButton;
  _shift  : TShiftState;
  _x      : Integer;
  _y      : Integer
);
begin
  if not checkMouse( _x, _y ) then
    exit;

  currX := _x;
  currY := _y;

  TGIS_ImagePanel( oControl ).Repaint;

  if assigned( FOnChange ) then
    FOnChange( Parent );

  bMouseDown := True;
end;

procedure T_PvlColorWheel.doMouseMove(
  _sender : TObject;
  _shift  : TShiftState;
  _x      : Integer;
  _y      : Integer
);
begin
  if not bMouseDown then
    exit;

  if not checkMouse( _x, _y ) then
    exit;

  currX := _x;
  currY := _y;

  if assigned( FOnChange ) then
    FOnChange( Parent );

  TGIS_ImagePanel( oControl ).Repaint;
end;


procedure T_PvlColorWheel.doMouseUp(
  _sender : TObject;
  _button : TMouseButton;
  _shift  : TShiftState;
  _x      : Integer;
  _y      : Integer
);
begin
  if not bMouseDown then
    exit;

  if not checkMouse( _x, _y ) then begin
    bMouseDown := False;
    exit;
  end;

  currX := _x;
  currY := _y;

  TGIS_ImagePanel( oControl ).Repaint;

  bMouseDown := False;

  if assigned( FOnChange ) then
    FOnChange( Parent );
end;

function T_PvlColorWheel.fget_Color
  : TGIS_Color;
var
  px  : TGIS_PvlRGBVal;
  w2  : Double;
  h2  : Double;
  x   : Double;
  y   : Double;
  s   : Double;
begin
  w2 := ( fget_Width * oContext.PPIFix  - GIS_COLOR_WHEEL_MARGIN )/2;
  h2 := ( fget_Height * oContext.PPIFix - GIS_COLOR_WHEEL_MARGIN )/2;

  y := h2 - currY + GIS_COLOR_WHEEL_MARGIN2;
  x := currX - w2 - GIS_COLOR_WHEEL_MARGIN2;

  px := angleToRGB( ArcTan2( y, x ) );

  s := Sqrt( x*x + y*y )/w2;
  if s > 1.0 then
    s := 1.0;

  Result := TGIS_Color.FromRGB(
              RoundS( 255 - s*px.R ),
              RoundS( 255 - s*px.G ),
              RoundS( 255 - s*px.B )
            );
end;


procedure T_PvlColorWheel.fset_Color(
  const _value : TGIS_Color
);
var
  h   : Double;
  s   : Double;
  v   : Double;
  rad : Double;
  w2  : Double;
  x   : Double;
  y   : Double;
begin
  _value.ToHSV( h, s, v );

  rad := 2*Pi*h;

  y := s*Sin( rad );
  x := s*Cos( rad );
  w2 := ( fget_Width * oContext.PPIFix - GIS_COLOR_WHEEL_MARGIN )/2;

  currX := RoundS( x*w2 + fget_Width * oContext.PPIFix /2 );
  currY := RoundS( fget_Height * oContext.PPIFix /2 - y*w2 );

  TGIS_ImagePanel( oControl ).repaint;
end;

function T_PvlColorWheel.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := FOnChange;
end;

procedure T_PvlColorWheel.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  FOnChange := _value;
end;

function T_PvlColorWheel.HueToRGB(
  const _hue : Double
) : TGIS_PvlRGBVal;
var
  hue : Double;
begin
  if _hue = 360.0 then
    hue := 0.0
  else
    hue := _hue;

  Result := angleToRGB( 2*Pi*hue/360 - Pi );
end;

procedure T_PvlColorWheel.DoRedraw;
var
  factor : Double;
begin
  oContext.Refresh;

  if oldppi = 0 then
    oldppi := 96;

  factor := oContext.PPI / oldppi;

  currX := RoundS( currX * factor );
  currY := RoundS( currY * factor );
end;

procedure T_PvlColorWheel.SetFocus;
begin
  TCustomControl( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlColorWheel'}

{$REGION 'T_PvlColorBar'}

procedure T_PvlColorBar.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TGIS_ImagePanel.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );

  bInit := False;
  bMouseDown := False;

  FColor := TGIS_Color.White;
  FAlpha := False;

  TGIS_ImagePanel( oControl ).OnPaintEvent := doOnPaint;
  TGIS_ImagePanel( oControl ).OnMouseDown  := doMouseDown;
  TGIS_ImagePanel( oControl ).OnMouseMove  := doMouseMove;
  TGIS_ImagePanel( oControl ).OnMouseUp    := doMouseUp;
end;

procedure T_PvlColorBar.doDestroy;
begin
  FreeObject( gradient );
  FreeObject( currState );

  inherited;
end;

procedure T_PvlColorBar.makeGradient;
begin
  FreeObject( gradient );

  gradient := TBitmap.Create;
  gradient.PixelFormat := TPixelFormat.pf24bit;
  gradient.SetSize( TGIS_ImagePanel( oControl ).Width, TGIS_ImagePanel( oControl ).Height );

  gradient.Canvas.Pen.Color := StyleServices.GetSystemColor( clBtnFace );
  gradient.Canvas.Pen.Style := TPenStyle.psSolid;
  gradient.Canvas.Brush.Color := StyleServices.GetSystemColor( clBtnFace );
  gradient.Canvas.Brush.Style := TBrushStyle.bsSolid;
  gradient.Canvas.Rectangle( 0, 0, TGIS_ImagePanel( oControl ).Width, TGIS_ImagePanel( oControl ).Height );

  if fget_Alpha then
    makeAlpha
  else
    makeColor;


  gradient.Canvas.Pen.Color := TColor( $C9CED2 );
  gradient.Canvas.Brush.Style := TBrushStyle.bsClear;
  gradient.Canvas.Rectangle(
    GIS_COLOR_GRADIENT_MARGIN2 - 1 ,
    GIS_COLOR_GRADIENT_MARGIN2 - 1,
    TGIS_ImagePanel( oControl ).Width  - GIS_COLOR_GRADIENT_MARGIN2 + 1,
    TGIS_ImagePanel( oControl ).Height - GIS_COLOR_GRADIENT_MARGIN2 + 1
  );

  FreeObject( currState );

  currState := TBitmap.Create;
  currState.PixelFormat := TPixelFormat.pf24bit;
  currState.SetSize( TGIS_ImagePanel( oControl ).Width, TGIS_ImagePanel( oControl ).Height );

  bInit := True;
end;

procedure T_PvlColorBar.makeColor;
var
  ptr : pByteArray;
  i   : Integer;
  k   : Integer;
begin
  inherited;

  for i := GIS_COLOR_GRADIENT_MARGIN2
      to TGIS_ImagePanel( oControl ).Height - GIS_COLOR_GRADIENT_MARGIN2 - 1 do begin

    ptr := gradient.ScanLine[i];

    for k := GIS_COLOR_GRADIENT_MARGIN2
        to TGIS_ImagePanel( oControl ).Width - GIS_COLOR_GRADIENT_MARGIN2 - 1 do begin

      ptr[3*k  ] :=
        RoundS(
          (
            ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
            ( TGIS_ImagePanel( oControl ).Width - GIS_COLOR_GRADIENT_MARGIN )
          ) * FColor.B
        );
      ptr[3*k+1] :=
        RoundS(
          (
            ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
            ( TGIS_ImagePanel( oControl ).Width - GIS_COLOR_GRADIENT_MARGIN )
          ) * FColor.G
        );
      ptr[3*k+2] :=
        RoundS(
          (
            ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
            ( TGIS_ImagePanel( oControl ).Width - GIS_COLOR_GRADIENT_MARGIN )
          ) * FColor.R
        );

    end;
  end;
end;

procedure T_PvlColorBar.makeAlpha;
var
  ptr : pByteArray;
  mul : Double;
  i   : Integer;
  k   : Integer;
  rb  : Boolean;
  cb  : Boolean;

  r   : Byte;
  g   : Byte;
  b   : Byte;
  sz  : Integer;
begin
  inherited;

  r := FColor.R;
  g := FColor.G;
  b := FColor.B;

  sz := MulDiv( 8, oContext.PPI, 96 );
  cb := False;

  for i := GIS_COLOR_GRADIENT_MARGIN2
      to TGIS_ImagePanel( oControl ).Height - GIS_COLOR_GRADIENT_MARGIN2 - 1 do begin

    if ( ( i - GIS_COLOR_GRADIENT_MARGIN2 ) mod sz ) = 0 then
      cb := not cb;
    rb := cb;

    ptr := gradient.ScanLine[i];

    for k := GIS_COLOR_GRADIENT_MARGIN2
        to TGIS_ImagePanel( oControl ).Width - GIS_COLOR_GRADIENT_MARGIN2 - 1 do begin

      if ( ( k - GIS_COLOR_GRADIENT_MARGIN2 ) mod sz ) = 0 then
        rb := not rb;

      mul := ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
             ( TGIS_ImagePanel( oControl ).Width - GIS_COLOR_GRADIENT_MARGIN );

      if rb then begin
        ptr[3*k  ] := RoundS( mul * b + ( 1 - mul ) * $C0 );
        ptr[3*k+1] := RoundS( mul * g + ( 1 - mul ) * $C0 );
        ptr[3*k+2] := RoundS( mul * r + ( 1 - mul ) * $C0 );
      end
      else begin
        ptr[3*k  ] := RoundS( mul * b + ( 1 - mul ) * $FF );
        ptr[3*k+1] := RoundS( mul * g + ( 1 - mul ) * $FF );
        ptr[3*k+2] := RoundS( mul * r + ( 1 - mul ) * $FF );
      end;

    end;

  end;

end;

procedure T_PvlColorBar.drawArrows;
begin
  currState.Canvas.Draw( 0, 0, gradient );

  currState.Canvas.Brush.Color := clWhite;
  currState.Canvas.Pen.Color   := clBlack;
  currState.Canvas.Polygon( [
    Point( currX - 1 , 0          ),
    Point( currX - 2 , 1          ),
    Point( currX - 2 , TGIS_ImagePanel( oControl ).Height - 2 ),
    Point( currX - 1 , TGIS_ImagePanel( oControl ).Height - 1 ),
    Point( currX + 1 , TGIS_ImagePanel( oControl ).Height - 1 ),
    Point( currX + 2 , TGIS_ImagePanel( oControl ).Height - 2 ),
    Point( currX + 2 , 1          ),
    Point( currX + 1 , 0          ),
    Point( currX - 1 , 0          )
  ] );
end;

function T_PvlColorBar.checkMouse(
  const _x : Integer;
  const _y : Integer
) : Boolean;
begin
  Result := True;

  if ( _x < GIS_COLOR_GRADIENT_MARGIN2             ) or
      ( _x > fget_Width * oContext.PPIFix - GIS_COLOR_GRADIENT_MARGIN2 - 1 ) then
    Result := False;
end;

function T_PvlColorBar.fget_Value : Double;
begin
  Result := FValue;
end;

procedure T_PvlColorBar.doMouseDown(
  _sender : TObject;
  _button : TMouseButton;
  _shift  : TShiftState;
  _x      : Integer;
  _y      : Integer
);
begin
  if not checkMouse( _x, _y ) then
    exit;

  currX := _x;

  FValue := ( currX - GIS_COLOR_GRADIENT_MARGIN2 )/
            ( fget_Width * oContext.PPIFix - GIS_COLOR_GRADIENT_MARGIN - 1 );

  if Assigned( FOnChange ) then
    FOnChange( Parent );

  bMouseDown := True;
end;


procedure T_PvlColorBar.doMouseMove(
  _sender : TObject;
  _shift  : TShiftState;
  _x      : Integer;
  _y      : Integer
);
begin
  if not bMouseDown then
    exit;

  if not checkMouse( _x, _y ) then
    exit;

  currX := _x;

  FValue := ( currX - GIS_COLOR_GRADIENT_MARGIN2 )/
            ( fget_Width * oContext.PPIFix - GIS_COLOR_GRADIENT_MARGIN - 1 );

  TGIS_ImagePanel( oControl ).Repaint;

  if Assigned( FOnChange ) then
    FOnChange( Parent );
end;

procedure T_PvlColorBar.doMouseUp(
  _sender : TObject;
  _button : TMouseButton;
  _shift  : TShiftState;
  _x      : Integer;
  _y      : Integer
);
begin
  if not bMouseDown then
    exit;

  if not checkMouse( _x, _y ) then begin
    bMouseDown := False;
    exit;
  end;

  currX := _x;

  FValue := ( currX - GIS_COLOR_GRADIENT_MARGIN2 )/
            ( fget_Width * oContext.PPIFix - GIS_COLOR_GRADIENT_MARGIN - 1 );

  TGIS_ImagePanel( oControl ).Repaint;

  if Assigned( FOnChange ) then
    FOnChange( Parent );

  bMouseDown := False;
end;

procedure T_PvlColorBar.doOnPaint(
  _e: TObject
);
var
  ppi : Integer;
begin
  ppi := oContext.PPI;
  if ( not bInit ) or ( ppi <> oldPPI ) then begin
    oldPPI := ppi;
    makeGradient;
  end;

  drawArrows;

  TGIS_ImagePanel( oControl ).Canvas.Draw( 0, 0, currState );
end;

procedure T_PvlColorBar.fset_Value(
  const _value : Double
);
begin
  if _value > 1.0 then
    FValue := 1.0
  else
  if FValue < 0.0 then
    FValue := 0
  else
    FValue := _value;

  currX := RoundS(
             FValue * ( fget_Width * oContext.PPIFix - GIS_COLOR_GRADIENT_MARGIN - 1 ) +
             GIS_COLOR_GRADIENT_MARGIN2
           );

 TGIS_ImagePanel( oControl ).Invalidate;
end;

function T_PvlColorBar.fget_Color : TGIS_Color;
begin
  Result := FColor;
end;

procedure T_PvlColorBar.fset_Color(
  const _value : TGIS_Color
);
begin
  FColor := _value;

  bInit := False;

  TGIS_ImagePanel( oControl ).Invalidate;
end;

function T_PvlColorBar.fget_Alpha : Boolean;
begin
  Result := FAlpha;
end;

procedure T_PvlColorBar.fset_Alpha(
  const _value : Boolean
);
begin
  FAlpha := _value;
end;

function T_PvlColorBar.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := FOnChange;
end;

procedure T_PvlColorBar.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  FOnChange := _value;
end;

procedure T_PvlColorBar.DoRedraw;
var
  factor : Double;
begin
  oContext.Refresh;

  if oldppi = 0 then
    oldppi := 96;

  factor := oContext.PPI / oldppi;

  currX := RoundS( currX * factor );
end;

procedure T_PvlColorBar.SetFocus;
begin
  TCustomControl( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlColorBar'}

{$REGION 'T_PvlCustomComboBox'}

constructor T_PvlCustomComboBox.Create(
  const _context : TGIS_PvlContext ;
  const _parent  : IGIS_PvlBase
);
begin
  inherited Create( _context, _parent );

  iCustomFieldsCount := 0;
  FFields := TStringList.Create;
end;

procedure T_PvlCustomComboBox.doDestroy;
begin
  FreeObject( FFields );
  inherited;
end;

procedure T_PvlCustomComboBox.doOnChange(
  _sender: TObject
);
begin
  if Assigned( FOnChange ) then
    FOnChange( Parent );
end;

function T_PvlCustomComboBox.fget_Value
  : String;
begin
  if TComboBox( oControl ).ItemIndex > -1 then
    Result := TComboBox( oControl ).Items[ TComboBox( oControl ).ItemIndex ].split( ['|'] )[2];
end;

procedure T_PvlCustomComboBox.fset_Value(
  const _value : String
);
begin

end;

function T_PvlCustomComboBox.fget_Fields
  : TStringList;
begin
  Result := FFields;
end;

procedure T_PvlCustomComboBox.fset_Fields(
  const _value : TStringList
);
begin
  FFields := _value;
end;

function T_PvlCustomComboBox.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := FOnChange;
end;

procedure T_PvlCustomComboBox.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  FOnChange := _value;
end;

{$ENDREGION 'T_PvlSizeCustomComboBox'}

{$REGION 'T_PvlSizeComboBox'}

procedure T_PvlSizeComboBox.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TComboBox.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );
  TComboBox( oControl ).Style := csOwnerDrawFixed;
  TComboBox( oControl ).DropDownCount := 5;
  TComboBox( oControl ).OnDrawItem := doColorListDrawItem;
  TComboBox( oControl ).OnChange := doOnChange;
end;

procedure T_PvlSizeComboBox.Fill(
  const _forSymbol  : Boolean;
  const _forLine    : Boolean;
  const _field      : Boolean;
  const _renderer   : Boolean
);
begin
  TComboBox( oControl ).Items.Clear;
  TComboBox( oControl ).Items.BeginUpdate;

  if _forSymbol then begin
    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '8 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '8pt',
          ''
        )
      )
    );

    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '10 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '10pt',
          ''
        )
      )
    );

    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '12 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '12pt',
          ''
        )
      )
    );

    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '16 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '16pt',
          ''
        )
      )
    );

    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '24 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '24pt',
          ''
        )
      )
    );

    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '32 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '32pt',
          ''
        )
      )
    );
  end
  else if _forLine then begin
    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, 'HAIR',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          'HAIR',
          ''
        )
      )
    );
    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '1 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '1pt',
          ''
        )
      )
    );
    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '2 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '2pt',
          ''
        )
      )
    );
    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '4 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '4pt',
          ''
        )
      )
    );
    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '6 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '6pt',
          ''
        )
      )
    );
    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '10 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '10pt',
          ''
        )
      )
    );
  end
  else begin
    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '0 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '0pt',
          ''
        )
      )
    );

    if TComboBox( oControl ).Items.Count = 1 then begin
      TComboBox( oControl ).Items.Add(
        PrepareComboBoxItem(
          False, 's', TGIS_PvlComboBoxHelperPosition.Top, '1 pt',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_SIZE,
            '1pt',
            ''
          )
        )
      );
      TComboBox( oControl ).Items.Add(
        PrepareComboBoxItem(
          False, 's', TGIS_PvlComboBoxHelperPosition.Top, '2 pt',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_SIZE,
            '2pt',
            ''
          )
        )
      );
      TComboBox( oControl ).Items.Add(
        PrepareComboBoxItem(
          False, 's', TGIS_PvlComboBoxHelperPosition.Top, '3 pt',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_SIZE,
            '3pt',
            ''
          )
        )
      );
      TComboBox( oControl ).Items.Add(
        PrepareComboBoxItem(
          False, 's', TGIS_PvlComboBoxHelperPosition.Top, '4 pt',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_SIZE,
            '4pt',
            ''
          )
        )
      );
      TComboBox( oControl ).Items.Add(
        PrepareComboBoxItem(
          False, 's', TGIS_PvlComboBoxHelperPosition.Top, '5 pt',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_SIZE,
            '5pt',
            ''
          )
        )
      );
    end;

  end;

  if _renderer then begin
    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        False, 'r', TGIS_PvlComboBoxHelperPosition.Bottom,
        GIS_RS_GENERAL_BYRENDERER, GIS_PARAMTXT_TYPE_RENDERER
      )
    );
    inc( iCustomFieldsCount );
  end;

    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
      True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
      GIS_RS_GENERAL_CUSTOM + '...', GIS_PARAMTXT_TYPE_CUSTOM
    )
  );
    inc( iCustomFieldsCount );

  if _field then begin
    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
        GIS_RS_GENERAL_BYFIELD + '...', GIS_PARAMTXT_TYPE_FIELD
      )
    );
    inc( iCustomFieldsCount );
  end;

  TComboBox( oControl ).ItemIndex := 0;
  TComboBox( oControl ).Items.EndUpdate;
end;

procedure T_PvlSizeComboBox.FillRealWorldUnits(
  const _field : Boolean
);
begin
  TComboBox( oControl ).Items.Clear;
  TComboBox( oControl ).Items.BeginUpdate;

  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top, '0 m',
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_SIZE,
        '0m',
        ''
      )
    )
  );

  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
      GIS_RS_GENERAL_CUSTOM + '...', GIS_PARAMTXT_TYPE_CUSTOM
    )
  );
  inc( iCustomFieldsCount );

  if _field then begin
    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
        GIS_RS_GENERAL_BYFIELD + '...', GIS_PARAMTXT_TYPE_FIELD
      )
    );
    inc( iCustomFieldsCount );
  end;

  TComboBox( oControl ).ItemIndex := 0;
  TComboBox( oControl ).Items.EndUpdate;
end;

procedure T_PvlSizeComboBox.FillAggregation;
begin
  TComboBox( oControl ).Items.Clear;
  TComboBox( oControl ).Items.BeginUpdate;

  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top, '20 pt',
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_SIZE,
        '20 pt',
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top, '40 pt',
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_SIZE,
        '40 pt',
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top, '60 pt',
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_SIZE,
        '60 pt',
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top, '80 pt',
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_SIZE,
        '80 pt',
        ''
      )
    )
  );

  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
      GIS_RS_GENERAL_CUSTOM + '...', GIS_PARAMTXT_TYPE_CUSTOM
    )
  );
  inc( iCustomFieldsCount );

  TComboBox( oControl ).ItemIndex := 0;
  TComboBox( oControl ).Items.EndUpdate;
end;

procedure T_PvlSizeComboBox.fset_Value(
  const _value : String
);
var
  s1, s2, s3 : String;
  res : String;
begin
  try
    SplitParamAsText( _value, s1, s2, s3 );


    if s1 = GIS_PARAMTXT_TYPE_RENDERER then
      res := PrepareComboBoxItem(
                  False ,
                  'r',
                  TGIS_PvlComboBoxHelperPosition.Top,
                  s2,
                  _value
                )
    else
    if s1 = GIS_PARAMTXT_TYPE_FIELD then
      res := PrepareComboBoxItem(
                  False ,
                  'f',
                  TGIS_PvlComboBoxHelperPosition.Lru,
                  s2,
                  _value
                )
    else
    if s1 = GIS_PARAMTXT_TYPE_SIZE then
      res := PrepareComboBoxItem(
                  False ,
                  's',
                  TGIS_PvlComboBoxHelperPosition.Lru,
                  s2,
                  _value
                )

  except
    res := '';
  end;

  if not res.isEmpty then begin
    TComboBox( oControl ).Items.Insert(
      TComboBox( oControl ).Items.Count - iCustomFieldsCount, res
    );
    TComboBox( oControl ).ItemIndex
      := TComboBox( oControl ).Items.Count - 1 - iCustomFieldsCount;
  end;
end;

procedure T_PvlSizeComboBox.fset_Height(
  const _value : Integer
);
begin
  inherited;
  if _value > 0 then
    TComboBox( oControl ).ItemHeight := _value;
end;

procedure T_PvlSizeComboBox.doColorListDrawItem(
  Control : TWinControl;
  Index   : Integer;
  Rect    : TRect;
  State   : TOwnerDrawState
);
var
  s1,s2,s3 : String;
  tf       : TTextFormat;
  s       : string;
  ar : TArray<String>;
  &type, caption, key : String;
begin
    if TComboBox( oControl ).BiDiMode = bdRightToLeft then
      tf := [tfRtlReading,tfRight]
    else
      tf := [];

    if IsStringEmpty( TComboBox( oControl ).Items[ Index ] ) then
      exit;

    SplitParamAsText( TComboBox( oControl ).Items[ Index ], s1, s2, s3 );

    ar := TComboBox( oControl ).Items[ Index ].split( ['|'] );
    &type := ar[0].substring( 1 );
    caption := ar[1];
    key := ar[2];

    with TComboBox( oControl ).Canvas do begin
      FillRect( Rect );

      SplitParamAsText( caption, s1, s2, s3  );

      case &type.Chars[0] of
        'f' : begin
                s := s2 + ' * ' + s3;
              end;
        'r' : begin
                s := caption;
              end;
        'C' : begin
                s := caption;
              end;
        else begin
          if not IsStringEmpty( s2) then
            s := s2
          else
            s := caption
        end;
      end;


      Rect.Left  := Rect.Left  + 2;
      Rect.Right := Rect.Right - 2;
      Rect.Top   := Rect.Top   + 1;

      TextRect( Rect, s, tf );
    end;
end;

procedure T_PvlSizeComboBox.doOnChange(
  _sender: TObject
);
var
  res : String;
  s1,s2, s3 : String;
  frms : TGIS_ControlSizeForm;
  frmf : TGIS_ControlFieldFactor;
  val  : String;
  proc : TGIS_Proc;
begin

  if fget_Value = GIS_PARAMTXT_TYPE_FIELD then begin
    frmf := TGIS_ControlFieldFactor.Create( oContext.NativeParent );
    if ( not assigned( FFields ) ) or ( FFields.Count = 0 ) then
      raise EGIS_Exception.create( _rsrcna( GIS_RS_ERR_FIELDFACTOR_NOFIELDS ), '', 0 );

    frmf.FillFields( FFields );
    frmf.FillUnits( TGIS_FieldFactorUnitsType.Size );

    proc := procedure( _modal_result : TGIS_PvlModalResult )
    begin
      if _modal_result <> TGIS_PvlModalResult.OK then
        exit;

      val := ConstructNumberAsText(
               frmf.cmbFields.Text,
               frmf.spnFactor.Text,
               frmf.cmbUnits.Text
             );
    end;

    frmf.Execute(
      proc
    );
    res := val;
  end
  else
  if fget_Value = GIS_PARAMTXT_TYPE_CUSTOM then begin
      frms := TGIS_ControlSizeForm.Create( oContext.NativeParent );
      frms.FillUnits( False );

    proc := procedure( _modal_result : TGIS_PvlModalResult )
    begin
      if _modal_result <> TGIS_PvlModalResult.OK then
        exit;

      if frms.isRotation then
        val := GIS_PARAMTXT_TYPE_ANGLE + ':' +
                  frms.spnFactor.Text + ' ' + frms.cmbUnits.Text
      else
        val := GIS_PARAMTXT_TYPE_SIZE + ':' +
                  frms.spnFactor.Text + ' ' + frms.cmbUnits.Text;
    end;

    frms.Execute( proc );

    res := val;

  end;


  SplitParamAsText( res, s1, s2, s3 );

  if s1 = GIS_PARAMTXT_TYPE_SIZE then
     res := PrepareComboBoxItem(
                 False ,
                 's',
                 TGIS_PvlComboBoxHelperPosition.Lru,
                 s2,
                 res
               )
   else
   if s1 = GIS_PARAMTXT_TYPE_RENDERER then
     res := PrepareComboBoxItem(
                 False ,
                 'r',
                 TGIS_PvlComboBoxHelperPosition.Top,
                 s2,
                 res
               )
   else
   if s1 = GIS_PARAMTXT_TYPE_FIELD then
     res := PrepareComboBoxItem(
                 False ,
                 'f',
                 TGIS_PvlComboBoxHelperPosition.Lru,
                 s2,
                 res
               )
   else
     res := '';

  if not res.isEmpty then begin
    TComboBox( oControl ).Items.Insert(
      TComboBox( oControl ).Items.Count - iCustomFieldsCount, res
    );
    TComboBox( oControl ).ItemIndex
      := TComboBox( oControl ).Items.Count - 1 - iCustomFieldsCount;
  end;
end;

procedure T_PvlSizeComboBox.SetFocus;
begin
  TComboBox( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlSizeComboBox'}

{$REGION 'T_PvlColorComboBox'}

procedure T_PvlColorComboBox.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TComboBox.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );
  TComboBox( oControl ).Style := csOwnerDrawFixed;
  TComboBox( oControl ).DropDownCount := 5;
  TComboBox( oControl ).OnDrawItem := doColorListDrawItem;
  TComboBox( oControl ).OnChange := doOnChange;
end;

procedure T_PvlColorComboBox.Fill(
  const _field    : Boolean;
  const _renderer : Boolean
);
var
  lst : TStringList;
begin
  lst := PrepareComboBoxColorsList;
  try
    Fill( _field, _renderer, lst );
  finally
    FreeObject( lst );
  end;
end;

procedure T_PvlColorComboBox.Fill(
  const _field    : Boolean;
  const _renderer : Boolean;
  const _colors   : TStringList
);
var
  i : Integer;

  procedure addItem( const _value : String );
  begin
    TComboBox( oControl ).addItem(
      PrepareComboBoxItem(
        False, 'c', TGIS_PvlComboBoxHelperPosition.Top,
        _value, ConstructParamAsText( GIS_PARAMTXT_TYPE_ARGB, _value, '' )
      ),
      nil
    );
  end;

begin

  for i := 0 to _colors.Count - 1 do begin
    addItem( _colors.Strings[i] );
  end;

  if _renderer then begin
    TComboBox( oControl ).addItem(
      PrepareComboBoxItem(
        False, 'r', TGIS_PvlComboBoxHelperPosition.Bottom,
        GIS_RS_GENERAL_BYRENDERER, GIS_PARAMTXT_TYPE_RENDERER
      ), nil
    );
    inc( iCustomFieldsCount );
  end;

  TComboBox( oControl ).addItem(
    PrepareComboBoxItem(
      True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
      GIS_RS_GENERAL_CUSTOM + '...', GIS_PARAMTXT_TYPE_CUSTOM
    ), nil
  );
  inc( iCustomFieldsCount );

  if _field then begin
    TComboBox( oControl ).addItem(
      PrepareComboBoxItem(
        True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
        GIS_RS_GENERAL_BYFIELD + '...', GIS_PARAMTXT_TYPE_FIELD
      ), nil
    );
    inc( iCustomFieldsCount );
  end;
  TComboBox( oControl ).ItemIndex := 0;
end;

procedure T_PvlColorComboBox.fset_Value(
  const _value : String
);
var
  s1, s2, s3 : String;
  res : String;
begin
  try
    SplitParamAsText( _value, s1, s2, s3 );

    if s1 = GIS_PARAMTXT_TYPE_RENDERER then
      res := PrepareComboBoxItem(
                  False ,
                  'r',
                  TGIS_PvlComboBoxHelperPosition.Top,
                  s2,
                  _value
                )
    else
    if s1 = GIS_PARAMTXT_TYPE_ARGB then
      res := PrepareComboBoxItem(
                  False ,
                  'c',
                  TGIS_PvlComboBoxHelperPosition.Top,
                  s2,
                  _value
                )
    else
    if s1 = GIS_PARAMTXT_TYPE_FIELD then
      res := PrepareComboBoxItem(
                  False ,
                  'f',
                  TGIS_PvlComboBoxHelperPosition.Lru,
                  s2,
                  _value
                );

  except
    res := '';
  end;

  if not res.isEmpty then begin
    TComboBox( oControl ).Items.Insert(
      TComboBox( oControl ).Items.Count - iCustomFieldsCount, res
    );
    TComboBox( oControl ).ItemIndex
      := TComboBox( oControl ).Items.Count - 1 - iCustomFieldsCount;
  end;

  prevColor := fget_Value;
end;

procedure T_PvlColorComboBox.doColorListDrawItem(
  _control : TWinControl;
  _index   : Integer;
  _rect    : TRect;
  _state   : TOwnerDrawState
);
var
  rect     : TRect;
  a,r,g,b  : Byte;
  bckg     : TColor;
  s1,s2,s3 : String;
  tf       : TTextFormat;
  ar : TArray<String>;
  &type, caption, key : String;

  procedure convert_color(
    const _color : String;
    out   _a     : Byte;
    out   _r     : Byte;
    out   _g     : Byte;
    out   _b     : Byte
  );
  var
    cl  : TGIS_Color;
    s1, s2, s3 : String;
  begin
    SplitParamAsText( _color, s1, s2, s3 );

    Assert( s1 = GIS_PARAMTXT_TYPE_ARGB );

    cl := TGIS_Color.FromARGB( Cardinal(StrToInt64( '$'+s2)) );
    _a := cl.A;
    _r := cl.R;
    _g := cl.G;
    _b := cl.B;
  end;

begin
    if TComboBox( oControl ).BiDiMode = bdRightToLeft then
      tf := [tfRtlReading,tfRight]
    else
      tf := [];

    if IsStringEmpty( TComboBox( oControl ).Items[ _index ] ) then
      exit;

    SplitParamAsText( TComboBox( oControl ).Items[ _index ], s1, s2, s3 );

    ar := TComboBox( oControl ).Items[ _index ].split( ['|'] );
    &type := ar[0].substring( 1 );
    caption := ar[1];
    key := ar[2];

    with TComboBox( oControl ).Canvas do begin
      FillRect( _rect );

      bckg := Brush.Color;

      rect := _rect;

      case &type.Chars[0] of
        'c' : begin
                InflateRect( rect, -1, -1 );
                InflateRect( rect, -1, -1 );
                if TComboBox( oControl ).BiDiMode = bdRightToLeft then
                  rect.Left := rect.Right - ( rect.Bottom - rect.Top )
                else
                  rect.Right := rect.Bottom - rect.Top + rect.Left;
                convert_color( key + ':' + caption, a, r, g ,b );

                if ( odDisabled in _state ) or not _control.Enabled then begin
                  Brush.Color := clGray;
                  FillRect( rect );
                  Brush.Color := clGray;
                  FrameRect( rect );
                end
                else begin
                  draw_color( TComboBox( oControl ).Canvas, TGIS_Color.FromARGB( a, r, g, b ), rect, 4, True );
                  Brush.Color := TComboBox( oControl ).Canvas.Font.Color;
                  FrameRect( rect );
                end;

                Brush.Color := bckg;
                if TComboBox( oControl ).BiDiMode = bdRightToLeft then begin
                  Rect.Right := Rect.Left - 9;
                  Rect.Left  := Rect.Right - TextWidth( caption );
                end
                else
                begin
                  Rect.Left := rect.Right + 9;
                  Rect.Width := TextWidth( caption );
                end;
                Rect.Top := Rect.Top + (Rect.Bottom - Rect.Top - TextHeight('$')) div 2;

                TextRect( Rect, caption, tf );
              end;
        'f' : begin
                SplitParamAsText( Caption, s1, s2, s3 );
                if IsStringEmpty(s2) then
                  s2 := s1;
                Rect.Right := Rect.Right - 4;
                Rect.Left := Rect.Left + 4;
                Rect.Top := Rect.Top + (Rect.Bottom - Rect.Top - TextHeight(s2)) div 2;
                TextRect( Rect, s2, tf );
              end;
        else begin
          Rect.Right := Rect.Right - 4;
          Rect.Left := Rect.Left + 4;
          Rect.Top := Rect.Top + (Rect.Bottom - Rect.Top - TextHeight(caption)) div 2;
          TextRect( Rect, Caption, tf );
        end;
      end;
    end;
end;

procedure T_PvlColorComboBox.doOnChange(
  _sender: TObject
);
var
  res : String;
  s1,s2, s3 : String;
  frmc : TGIS_ControlColor;
  frmf : TGIS_ControlFieldFactor;
  clr  : TGIS_Color;
  val  : String;
  proc : TGIS_Proc;

  function convert_color(
    const _color : String
  ) : TGIS_Color;
  var
    s1, s2, s3 : String;
  begin
    SplitParamAsText( _color, s1, s2, s3 );

    if s1 = GIS_PARAMTXT_TYPE_ARGB then
      Result := TGIS_Color.FromARGB( Cardinal(StrToInt64( '$'+s2)) )
    else
      Result := TGIS_Color.Gray;
  end;

begin

  if fget_Value = GIS_PARAMTXT_TYPE_FIELD then begin
    frmf := TGIS_ControlFieldFactor.Create( oContext.NativeParent );
    if ( not assigned( FFields ) ) or ( FFields.Count = 0 ) then
      raise EGIS_Exception.create( _rsrcna( GIS_RS_ERR_FIELDFACTOR_NOFIELDS ), '', 0 );
    frmf.FillFields( FFields );
    frmf.FillUnits( TGIS_FieldFactorUnitsType.NoScale );

    frmf.FillFields( FFields );
    frmf.FillUnits( TGIS_FieldFactorUnitsType.NoScale );

    proc := procedure( _modal_result : TGIS_PvlModalResult )
    begin
      if _modal_result <> TGIS_PvlModalResult.OK then
        exit;

      val := ConstructParamAsText(
               GIS_PARAMTXT_TYPE_FIELD,
               frmf.cmbFields.Text,
               ''
             );
      //oComboBox.DelayedUpdate( val );
    end;

    frmf.Execute(
      proc
    );
    res := val;
  end
  else
  if fget_Value = GIS_PARAMTXT_TYPE_CUSTOM then begin
    frmc := TGIS_ControlColor.Create( oContext.NativeParent );

    clr := convert_color( prevColor );

    proc := procedure( _modal_result : TGIS_PvlModalResult )
    begin
      if _modal_result <> TGIS_PvlModalResult.OK then
        exit;

      val := ConstructParamAsText(
                  GIS_PARAMTXT_TYPE_ARGB,
                  IntToHex( frmc.Color.ARGB, 8 ),
                  ''
                );
      //oComboBox.DelayedUpdate( val );
    end;

    frmc.Execute( clr, proc );

    res := val;

  end;

  SplitParamAsText( res, s1, s2, s3 );

  if s1 = GIS_PARAMTXT_TYPE_ARGB then
    res := PrepareComboBoxItem(
                False ,
                'c',
                TGIS_PvlComboBoxHelperPosition.Lru,
                s2,
                res
              )
  else
  if s1 = GIS_PARAMTXT_TYPE_FIELD then
    res := PrepareComboBoxItem(
                False ,
                'f',
                TGIS_PvlComboBoxHelperPosition.Lru,
                s2,
                res
              )
  else
    res := '';

  if not res.isEmpty then begin
    TComboBox( oControl ).Items.Insert(
      TComboBox( oControl ).Items.Count - iCustomFieldsCount, res );
    TComboBox( oControl ).ItemIndex
      := TComboBox( oControl ).Items.Count - 1 - iCustomFieldsCount;
  end;

  prevColor := fget_Value;
end;

procedure T_PvlColorComboBox.SetFocus;
begin
  TComboBox( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlColorComboBox'}

{$REGION 'T_PvlCustomBitmapComboBox'}

procedure T_PvlCustomBitmapComboBox.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TComboBox.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );
  TComboBox( oControl ).Style := csOwnerDrawFixed;
  TComboBox( oControl ).DropDownCount := 5;
  TComboBox( oControl ).OnDrawItem := doColorListDrawItem;
  TComboBox( oControl ).OnChange := doOnChange;
  TComboBox( oControl ).OnCloseUp := doCloseUp;
end;

procedure T_PvlCustomBitmapComboBox.FillPattern(
  const _hasSymbol : Boolean
);
begin
  TComboBox( oControl ).Clear;

  FType := TGIS_PvlCustomBitmapType.Fill;

  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_PATTERN_SOLID,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_PATTERN_SOLID,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_PATTERN_TRANSPARENT,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_PATTERN_TRANSPARENT,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_PATTERN_HORIZONTAL,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_PATTERN_HORIZONTAL,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_PATTERN_VERTICAL,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_PATTERN_VERTICAL,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_PATTERN_FDIAGONAL,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_PATTERN_FDIAGONAL,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_PATTERN_BDIAGONAL,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_PATTERN_BDIAGONAL,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_PATTERN_CROSS,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_PATTERN_CROSS,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_PATTERN_DIAGCROSS,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_PATTERN_DIAGCROSS,
        ''
      )
    )
  );

  inc( iCustomFieldsCount );

  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
      _rsrcna( GIS_RS_LEGEND_PRM_BITMAP ) + '...', GIS_PARAMTXT_TYPE_TEXTURE
    )
  );
  if _hasSymbol then begin
    inc( iCustomFieldsCount );
    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
        _rsrcna( GIS_RS_LEGEND_PRM_SYMBOL ) + '...', GIS_PARAMTXT_TYPE_SYMBOL
      )
    );
  end;

  TComboBox( oControl ).ItemIndex := 0;
end;

procedure T_PvlCustomBitmapComboBox.FillStyle(
  const _hasSymbol : Boolean
);
begin
  TComboBox( oControl ).Clear;

  FType := TGIS_PvlCustomBitmapType.Outline;

  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_PEN_SOLID,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_PEN_SOLID,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_PEN_DASH,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_PEN_DASH,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_PEN_DOT,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_PEN_DOT,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_PEN_DASHDOT,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_PEN_DASHDOT,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_PEN_DASHDOTDOT,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_PEN_DASHDOTDOT,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_PEN_CLEAR,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_PEN_CLEAR,
        ''
      )
    )
  );
  if _hasSymbol then begin
    inc( iCustomFieldsCount );
    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
        'Editor...', GIS_PARAMTXT_TYPE_CODE
      )
    );
  end;

  if _hasSymbol then begin
    inc( iCustomFieldsCount );
    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
        _rsrcna( GIS_RS_LEGEND_PRM_SYMBOL ) + '...', GIS_PARAMTXT_TYPE_SYMBOL
      )
    );
  end;

  inc( iCustomFieldsCount );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
      GIS_RS_GENERAL_BYFIELD + '...', GIS_PARAMTXT_TYPE_FIELD
    )
  );

  TComboBox( oControl ).ItemIndex := 0;
end;

procedure T_PvlCustomBitmapComboBox.FillMarker(
  const _hasSymbol : Boolean
);
begin
  TComboBox( oControl ).Clear;

  FType := TGIS_PvlCustomBitmapType.Marker;

  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_MARKER_BOX,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_MARKER_BOX,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_MARKER_CIRCLE,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_MARKER_CIRCLE,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_MARKER_CROSS,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_MARKER_CROSS,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_MARKER_DIAGCROSS,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_MARKER_DIAGCROSS,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_MARKER_TRIANGLEUP,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_MARKER_TRIANGLEUP,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_MARKER_TRIANGLEDOWN,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_MARKER_TRIANGLEDOWN,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_MARKER_TRIANGLELEFT,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_MARKER_TRIANGLELEFT,
        ''
      )
    )
  );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top,
      GIS_INI_PARAM_MARKER_TRIANGLERIGHT,
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_STOCK,
        GIS_INI_PARAM_MARKER_TRIANGLERIGHT,
        ''
      )
    )
  );

  if _hasSymbol then begin
    inc( iCustomFieldsCount );
    TComboBox( oControl ).Items.Add(
      PrepareComboBoxItem(
        True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
        _rsrcna( GIS_RS_LEGEND_PRM_SYMBOL ) + '...', GIS_PARAMTXT_TYPE_SYMBOL
      )
    );
  end;

  TComboBox( oControl ).ItemIndex := 0;
end;

procedure T_PvlCustomBitmapComboBox.FillShield;
begin
  TComboBox( oControl ).Clear;

  FType := TGIS_PvlCustomBitmapType.Shield;

  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      True, 'B', TGIS_PvlComboBoxHelperPosition.Bottom,
      GIS_RS_LEGEND_PRM_BRUSH_CLEAR, 'nil'
    )
  );

  inc( iCustomFieldsCount );
  TComboBox( oControl ).Items.Add(
    PrepareComboBoxItem(
      True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
      _rsrcna( GIS_RS_LEGEND_PRM_SYMBOL ) + '...', GIS_PARAMTXT_TYPE_SYMBOL
    )
  );

  TComboBox( oControl ).ItemIndex := 0;
end;

procedure T_PvlCustomBitmapComboBox.fset_Value(
  const _value : String
);
var
  s1 : String;
  s2 : String;
  s3 : String;
  res : String;
begin
  try
    SplitParamAsText( _value, s1, s2, s3 );

    if s1 = GIS_PARAMTXT_TYPE_STOCK then
      res := PrepareComboBoxItem(
                  False ,
                  's',
                  TGIS_PvlComboBoxHelperPosition.Top,
                  s2,
                  _value
                )
    else if not IsStringEmpty(_value) then
      res := PrepareComboBoxItem(
                  False ,
                  'B',
                  TGIS_PvlComboBoxHelperPosition.Lru,
                  s2,
                  _value
                )
    else
      res := '';
  except
    res := '';
  end;

  if not res.isEmpty then begin
    TComboBox( oControl ).Items.Insert(
      TComboBox( oControl ).Items.Count - iCustomFieldsCount, res
    );
    TComboBox( oControl ).ItemIndex
      := TComboBox( oControl ).Items.Count - 1 - iCustomFieldsCount;
  end;
end;

function T_PvlCustomBitmapComboBox.fget_Type
  : TGIS_PvlCustomBitmapType;
begin
  Result := FType;
end;

function T_PvlCustomBitmapComboBox.fget_GetBitmapEvent
  : TGIS_PvlComboBoxHelperGetBitmapEvent;
begin
  Result := FGetBitmapEvent;
end;

procedure T_PvlCustomBitmapComboBox.fset_GetBitmapEvent(
  const _value : TGIS_PvlComboBoxHelperGetBitmapEvent
);
begin
  FGetBitmapEvent := _value;
end;

procedure T_PvlCustomBitmapComboBox.doColorListDrawItem(
  _control : TWinControl;
  _index   : Integer;
  _rect    : TRect;
  _state   : TOwnerDrawState
);
var
  ar : TArray<String>;
  itmType : String;
  caption : String;
  key : String;
  s1 : String;
  s2 : String;
  s3 : String;
  r    : TRect;
  sitm : String;
  bmp  : TGIS_Bitmap;
  tf   : TTextFormat;
  str  : String;
  fnt  : TGIS_Font;
  args : TGIS_PvlComboBoxHelperGetBitmapEventArgs;
begin
  args := nil;
  TComboBox( oControl ).Canvas.Pen.Width   := 1;
  TComboBox( oControl ).Canvas.Brush.Style := bsSolid;
  TComboBox( oControl ).Canvas.Pen.Color   := TComboBox( _control ).Canvas.Font.Color;

  str := TComboBox( oControl ).Items[ _index ];

  s1 := str.Split(['|'])[0];
  s2 := str.Split(['|'])[1];
  s3 := str.Split(['|'])[2];

  ar := s1.Split( ['|'] );
  itmType := s1.Substring( 1 );
  caption := s2;
  key := s3;

  r := _rect;
  TComboBox( oControl ).Canvas.FillRect(r);
  InflateRect(r, -1, -1);

  case itmType.Chars[0] of
    'C' : begin
            if TComboBox( oControl ).BiDiMode = bdRightToLeft then
              tf := [tfRtlReading,tfRight]
            else
              tf := [];
            r.Left  := r.Left  + 4;
            r.Right := r.Right - 4;
            r.Top   := r.Top   + 4;
            TComboBox( oControl ).Canvas.TextRect( r, caption, tf );
          end;
    'B', 's' :
          begin
            if assigned( FGetBitmapEvent ) then begin
              sitm := ClassName + '_' +
                      GetEnumName( TypeInfo(TGIS_PvlCustomBitmapType) , Integer( Self.&FType ) ) + '_'+
                      s3 + '_' +
                      IntToStr( r.Width  ) + '_' +
                      IntToStr( r.Height ) + '_' +
                      IntToStr( TComboBox( _control ).Canvas.Font.Color );

              fnt := TGIS_Font.Create;
              try
              fnt.LoadFromFont( TComboBox( oControl ).Font );

              if not oItemCache.TryGetValue( sitm, bmp ) then begin
                args := TGIS_PvlComboBoxHelperGetBitmapEventArgs.Create(
                                                s3,
                                                GISColor( TComboBox( _control ).Canvas.Font.Color ),  fnt,
                                                r.Right-r.Left+1, r.Bottom-r.Top+1
                                            );

                bmp := FGetBitmapEvent( _control,
                                        args
                                      );

                oItemCache.Add( sitm, bmp );
              end;

              if not TGIS_Bitmap.IsNilOrEmpty( bmp ) then begin
                if ( odDisabled in _state ) or not _control.Enabled then begin
                  TComboBox( oControl ).Canvas.Draw(
                    r.Left,
                    r.Top,
                    TBitmap(bmp.NativeBitmap),
                    128
                  );
                end
                else begin
                  TComboBox( oControl ).Canvas.Draw(
                    r.Left,
                    r.Top,
                    TBitmap(bmp.NativeBitmap),
                    255
                  );
                end
              end;
              finally
                FreeObject( fnt );
                FreeObject( args );
              end;
            end;
          end;
  end;
end;

procedure T_PvlCustomBitmapComboBox.doCloseUp(
  _sender: TObject
);
var
  frms : TGIS_ControlSymbology;
  frmb : TGIS_ControlBitmap;
  frme : Lider.CG.GIS.VCL.GeoLineSymbolEditor.TGIS_LineSymbolEditor;
  frm  : TGIS_ControlFieldFactor;
  proc : TGIS_Proc;
  s    : String;
  s1   : String;
  s2   : String;
  s3   : String;
  res  : String;
  ar   : TArray<String>;
begin
  res := '';

  if TComboBox( oControl ).ItemIndex = -1 then exit;

  ar := TComboBox( oControl ).Items[ TComboBox( oControl ).ItemIndex ].Split( ['|'] );

  if length( ar ) < 1 then
    exit;

  assert( length( ar ) = 3 );
  assert( length( ar[0] ) = 3 );

  if ar[2] = GIS_PARAMTXT_TYPE_SYMBOL then begin
    frms := TGIS_ControlSymbology.Create( oContext.NativeParent );

    proc := procedure( _modal_result : TGIS_PvlModalResult )
    begin
      if _modal_result <> TGIS_PvlModalResult.OK then
        exit;
      if assigned( frms.Symbol ) then
        res := ConstructParamAsText(
                      GIS_PARAMTXT_TYPE_SYMBOL,
                      frms.Symbol.Name,
                      ''
                    );
    end;

    if FType = TGIS_PvlCustomBitmapType.Shield then begin
      frms.OnlySVG := True;
      frms.OnlyCategory := GIS_SHIELD_CATEGORY;
    end;

    frms.Execute(
      '',
      proc
    );
  end
  else
  if ar[2] = GIS_PARAMTXT_TYPE_TEXTURE then begin
    frmb := TGIS_ControlBitmap.Create( oContext.NativeParent );

    proc := procedure( _modal_result : TGIS_PvlModalResult )
    begin
      if _modal_result <> TGIS_PvlModalResult.OK then
        exit;

      res := ConstructParamAsText(
               GIS_PARAMTXT_TYPE_TEXTURE,
               frmb.Bitmap.Path,
               ''
             );
    end;


    frmb.Execute(
      '',
      proc
    );
  end
  else
  if ar[2] = GIS_PARAMTXT_TYPE_CODE then begin
    frme := Lider.CG.GIS.VCL.GeoLineSymbolEditor.TGIS_LineSymbolEditor.create( TComponent( oContext.NativeParent ), True );
    try
      SplitParamAsText( TGIS_PvlCustomBitmapComboBox( _sender ).Value, s1, s2, s3 );
      if s1 = GIS_PARAMTXT_TYPE_CODE then
        s := s2
      else
        s := '';

      if frme.Execute( s ) = mrOk then begin
        res := ConstructParamAsText(
                    GIS_PARAMTXT_TYPE_CODE,
                    '' + frme.Symbol,
                    ''
                  );
      end;
    finally
      FreeObject( frme );
    end;
  end
  else
    if ar[2] = GIS_PARAMTXT_TYPE_FIELD then begin
      frm := TGIS_ControlFieldFactor.Create( oContext.NativeParent );
      if ( not assigned( FFields ) ) or ( FFields.Count = 0 ) then
        raise EGIS_Exception.create( _rsrcna( GIS_RS_ERR_FIELDFACTOR_NOFIELDS ), '', 0 );
      frm.FillFields( FFields );
      frm.FillUnits( TGIS_FieldFactorUnitsType.NoScale );
      proc := procedure( _modal_result : TGIS_PvlModalResult )
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit;

        res := ConstructParamAsText(
                  GIS_PARAMTXT_TYPE_FIELD,
                  frm.cmbFields.Text,
                  ''
                );
      end;

      frm.Execute(
        proc
      );
    end;


    if not String.IsNullOrEmpty( res ) then begin
      res := PrepareComboBoxItem(
                  False,
                  'B',
                  TGIS_PvlComboBoxHelperPosition.Lru,
                  res,
                  res
                );
     TComboBox( oControl ).Items.Insert(
       TComboBox( oControl ).Items.Count - iCustomFieldsCount, res
     );
     TComboBox( oControl ).ItemIndex
       := TComboBox( oControl ).Items.IndexOf( res );
    end;
end;

procedure T_PvlCustomBitmapComboBox.SetFocus;
begin
  TComboBox( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlCustomBitmapComboBox'}

{$REGION 'T_PvlColorRampComboBox'}

procedure T_PvlColorRampComboBox.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TComboBox.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );
  TComboBox( oControl ).Style := csOwnerDrawFixed;
  TComboBox( oControl ).DropDownCount := 5;
  TComboBox( oControl ).OnDrawItem := doRampListDrawItem;
  TComboBox( oControl ).OnChange := doOnChange;

  bmpMap   := nil;
  AIdx     := nil;
  FMode    := TGIS_ColorMapMode.Continuous;
  FColorSchemas := [ TGIS_ColorSchema.Diverging,
                     TGIS_ColorSchema.Miscellaneous,
                     TGIS_ColorSchema.Qualitative,
                     TGIS_ColorSchema.Sequential   ];
  FShowNames := True;
  iTextWidth := 0;
  iTextGap   := 0;
end;

procedure T_PvlColorRampComboBox.doDestroy;
begin
  FreeObject( bmpMap );
  inherited;
end;

function T_PvlColorRampComboBox.fget_Mode
  : TGIS_ColorMapMode;
begin
  Result := FMode;
end;

procedure T_PvlColorRampComboBox.fset_Mode(
  const _value : TGIS_ColorMapMode
);
begin
  if _value <> FMode then begin
    FMode := _value;
  end;
end;

function T_PvlColorRampComboBox.fget_ColorSchemas
  : TGIS_ColorSchemas;
begin
  Result := FColorSchemas;
end;

procedure T_PvlColorRampComboBox.fset_ColorSchemas(
  const _value : TGIS_ColorSchemas
);
begin
  if _value <> FColorSchemas then begin
    FColorSchemas := _value;
  end;
end;

function T_PvlColorRampComboBox.fget_ShowNames
  : Boolean;
begin
  Result := FShowNames;
end;

procedure T_PvlColorRampComboBox.fset_ShowNames(
  const _value : Boolean
);
begin
  if _value <> FShowNames then begin
    FShowNames := _value;
  end;
end;

function T_PvlColorRampComboBox.fget_Index
  : Integer;
begin
  Result := TComboBox( oControl ).ItemIndex;
end;

procedure T_PvlColorRampComboBox.fset_Index(
  const _value : Integer
);
begin
  TComboBox( oControl ).ItemIndex := _value;
end;

function T_PvlColorRampComboBox.fget_Reverse
  : Boolean;
begin
  Result := FReverse;
end;

procedure T_PvlColorRampComboBox.fset_Reverse(
  const _value : Boolean
);
begin
  if _value <> FReverse then begin
    FReverse := _value;
    Fill;
  end;
end;

function T_PvlColorRampComboBox.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := FOnChange;
end;

procedure T_PvlColorRampComboBox.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  FOnChange := _value;
end;

procedure T_PvlColorRampComboBox.doRampListDrawItem(
  Control : TWinControl;
  Index   : Integer;
  Rect    : TRect;
  State   : TOwnerDrawState
);

function convertBitmapToGreyScale( const _bmp : TBitmap ) : TBitmap;
type
  TRGBArray = array[0..32767] of TRGBTriple;
  PRGBArray = ^TRGBArray;
var
  x, y, gray : Integer;
  row : PRGBArray;
begin
  Result := TBitmap.Create;
  Result.Width  := _bmp.Width;
  Result.Height := _bmp.Height;
  Result.Assign( _bmp );
  Result.PixelFormat := pf24bit;

  for y := 0 to Result.Height - 1 do begin

    row := Result.ScanLine[ y ];

    for x := 0 to Result.Width - 1 do begin
      gray := ( row[ x ].rgbtRed + row[ x ].rgbtGreen + row[ x ].rgbtBlue ) div 3;
      row[x].rgbtRed    := gray;
      row[x].rgbtGreen  := gray;
      row[x].rgbtBlue   := gray;
    end;
  end;
end;

var
  cnt  : Integer;
  r    : TRect;
  bmp  : TBitmap;
  idx  : Integer;
  str  : String;
  tf   : TTextFormat;
  i    : Integer;
begin
  idx := AIdx[Index];
  cnt := length( GisColorRampList[idx].Map );

  if ( cnt = 0 ) then exit;

  with (Control as TComboBox) do begin
    Canvas.FillRect( Rect );

    if iTextGap = 0 then
      iTextGap := Canvas.TextWidth( ' ' );

    if iTextWidth = 0 then begin
      for i := 0 to Items.Count -1 do
        iTextWidth := Max( iTextWidth,
                           Canvas.TextWidth( GisColorRampList[i].Name )
                         );
    end;

    if not FShowNames then begin
      iTextWidth := 0;
      iTextGap := 0;
    end;

    r := Rect;
    r.Bottom := r.Bottom - 2;
    r.Top    := r.Top + 2;
    r.Left:= r.Left + 2;
    r.Right  := r.Right - iTextWidth - 3*iTextGap;

    if not Control.Enabled then begin
      bmp := convertBitmapToGreyScale( TBitmap( bmpMap[Index] ) );
      try
        Canvas.StretchDraw( r, bmp )
      finally
        FreeObject( bmp );
      end;
    end else begin
      bmp := TBitmap( bmpMap[Index] );
      Canvas.StretchDraw( r, bmp );
    end;

    Canvas.Brush.Style := bsClear;

    if FShowNames then begin
      if BIDiMode = bdRightToLeft then begin
        tf := [tfRtlReading,tfRight];
      end
      else begin
        tf := [];
      end;

      r := Rect;
      r.Left  := r.Right - iTextWidth - iTextGap;
      r.Top   := r.Top + ( Rect.Height - Canvas.TextHeight( 'Ay' ) ) div 2;
      r.Right := r.Right - iTextGap;
      str := GisColorRampList[idx].Name;
      Canvas.TextRect( r, str, tf );
    end;

    if odFocused in State then
      Canvas.DrawFocusRect( Rect );
  end;
end;

procedure T_PvlColorRampComboBox.gradHorizontal(
  const _canvas    : TCanvas;
  const _rect      : TRect;
  const _fromColor : TGIS_Color;
  const _toColor   : TGIS_Color;
  const _mode      : TGIS_ColorMapMode
);
 var
   x        : Integer;
   dr,dg,db : Double;
   c1,c2    : TColor;
   r1,r2,
   g1,g2,
   b1,b2    : Byte;
   r,g,b    : Byte;
   xr,xg,xb : Integer;
   cnt      : Integer;
   width    : Integer;
 begin
   c1 := VCLColor( _fromColor );
   r1 := GetRValue(c1);
   g1 := GetGValue(c1);
   b1 := GetBValue(c1);

   c2 := VCLColor( _toColor );
   r2 := GetRValue(c2);
   g2 := GetGValue(c2);
   b2 := GetBValue(c2);

   width  := _rect.Right - _rect.Left;
   if width = 0 then
     width := 1;
   dr := ( r2 - r1 ) / width;
   dg := ( g2 - g1 ) / width;
   db := ( b2 - b1 ) / width;

   if _mode = TGIS_ColorMapMode.Continuous then begin
     cnt := 0;
     for x := _rect.Left to _rect.Right-1 do begin
       xr := r1 + Ceil( dr * cnt );
       xg := g1 + Ceil( dg * cnt );
       xb := b1 + Ceil( db * cnt );

       r := Max( 0, Min( 255, xr ) );
       g := Max( 0, Min( 255, xg ) );
       b := Max( 0, Min( 255, xb ) );

       _canvas.Pen.Color := RGB( r, g, b );
       _canvas.MoveTo( x, _rect.Top );
       _canvas.LineTo( x, _rect.Bottom );
       inc( cnt );
     end;
   end
   else begin
     _canvas.Brush.Color := VCLColor( _fromColor );
     _canvas.FillRect( _rect );
   end;
 end;

procedure T_PvlColorRampComboBox.doOnChange(
  _sender: TObject
);
begin
  if Assigned( FOnChange ) then
    FOnChange( Parent );
end;

procedure T_PvlColorRampComboBox.Fill;
var
  i_ramp         : Integer;
  i_color        : Integer;
  bmp            : TBitmap;
  rect           : TRect;
  ramps_count    : Integer;
  col_map_arr    : TGIS_ColorMapArray;
  left_colormap  : TGIS_ColorMap;
  right_colormap : TGIS_ColorMap;
begin
  if bLock then
    exit;

  FreeObject( bmpMap );

  TComboBox( oControl ).ItemIndex := -1;

  ramps_count := GisColorRampList.Count;
  if ramps_count = 0 then
    GisColorRampList.Init;

  bmpMap := TGIS_ObjectList.Create( True );
  rect.Top    := 0;
  rect.Bottom := 1;

  for i_ramp := 0 to ramps_count - 1 do begin
    if not ( GisColorRampList[i_ramp].MapType in FColorSchemas ) then
      continue;

    bmp := TBitmap.Create;
    bmp.Width  := fget_Width;
    bmp.Height := 1;

    col_map_arr := GisColorRampList[i_ramp].RealizeColorMap( fget_Mode, 0, fget_Reverse );

    for i_color := 0 to high( col_map_arr ) - 1 do begin
      left_colormap := col_map_arr[i_color];
      right_colormap := col_map_arr[i_color+1];

      rect.Left  := RoundS( fget_Width * left_colormap.Index / 100 );
      rect.Right := RoundS( fget_Width * right_colormap.Index / 100 );

      gradHorizontal(
        bmp.Canvas,
        rect,
        left_colormap.RGB,
        right_colormap.RGB,
        FMode
      );
    end;

    bmpMap.Add( bmp );

    if ( i_ramp mod 20 ) = 0 then
      Application.ProcessMessages;
  end;

  SetLength( AIdx, bmpMap.Count );
  TComboBox( oControl ).Items.BeginUpdate;
  try
    TComboBox( oControl ).Items.Clear;
    for i_ramp := 0 to ramps_count - 1 do begin
      if not (GisColorRampList[i_ramp].MapType in FColorSchemas) then continue;
      i_color := TComboBox( oControl ).Items.Add( GisColorRampList[i_ramp].Name );
      AIdx[i_color] := i_ramp;
    end;
  finally
    TComboBox( oControl ).Items.EndUpdate;
    TCOmboBox( oControl ).ItemIndex := 0;
  end;
end;

procedure T_PvlColorRampComboBox.Lock;
begin
  bLock := True;
end;

procedure T_PvlColorRampComboBox.Unlock;
begin
  bLock := False;
  Fill;
end;

function T_PvlColorRampComboBox.Value(
  const _subClass : Integer
) : TGIS_ColorMapArray;
var
  idx : Integer;
begin
  if (TComboBox( oControl ).ItemIndex >=0) and (TComboBox( oControl ).ItemIndex < GisColorRampList.Count) then begin
    idx := AIdx[TComboBox( oControl ).ItemIndex];
    Result := GisColorRampList[idx].RealizeColorMap( fget_Mode, _subClass, fget_Reverse );
  end
  else
    Result := nil;
end;

procedure T_PvlColorRampComboBox.fset_Height(
  const _value : Integer
);
begin
  inherited;
  if _value > 0 then
    TComboBox( oControl ).ItemHeight := _value;
end;

function T_PvlColorRampComboBox.fget_ItemCount
  : Integer;
begin
  Result := TComboBox( oControl ).Items.Count;
end;

function T_PvlColorRampComboBox.fget_Item(
  const _idx: Integer
): string;
begin
  Result := TComboBox( oControl ).Items[_idx];
end;

procedure T_PvlColorRampComboBox.SetFocus;
begin
  TComboBox( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlColorRampComboBox'}

{$REGION 'T_PvlComboBox'}

procedure T_PvlComboBox.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TComboBox.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );
  TComboBox( oControl ).Style := csDropDownList;
  TComboBox( oControl ).DropDownCount := 5;
  TComboBox( oControl ).OnChange := doOnChange;
end;

procedure T_PvlComboBox.doOnChange(
       _sender : TObject
);
begin
  if Assigned( FOnChange ) then
    FOnChange( Parent );
end;

function T_PvlComboBox.fget_ItemsCount
  : Integer;
begin
  Result := TComboBox( oControl ).Items.Count;
end;

function T_PvlComboBox.fget_Item(
  const _idx   : Integer
) : String;
begin
  Result := TComboBox( oControl ).Items[ _idx ];
end;

function  T_PvlComboBox.fget_ItemIndex
  : Integer;
begin
  Result := TComboBox( oControl ).ItemIndex;
end;

procedure T_PvlComboBox.fset_ItemIndex(
  const _value : Integer
);
begin
  TComboBox( oControl ).ItemIndex := _value;
  doOnChange( Parent );
end;

function  T_PvlComboBox.fget_Text
  : String;
begin
  Result := TComboBox( oControl ).Text;
end;

procedure T_PvlComboBox.fset_Tag(
  const _value : NativeInt
);
begin
  FTag := _value;
end;

function  T_PvlComboBox.fget_Tag
  : NativeInt;
begin
  Result := FTag;
end;

procedure T_PvlComboBox.fset_Text(
  const _value : String
);
begin
  TComboBox( oControl ).Text := _value;
end;

function T_PvlComboBox.fget_Sorted
  : Boolean;
begin
  Result := TComboBox( oControl ).Sorted;
end;

procedure T_PvlComboBox.fset_Sorted(
  const _value : Boolean
);
begin
  TComboBox( oControl ).Sorted := _value;
end;

function T_PvlComboBox.fget_DropDownCount
  : Integer;
begin
  Result := TComboBox( oControl ).DropDownCount;
end;

procedure T_PvlComboBox.fset_DropDownCount(
  const _value : Integer
);
begin
  TComboBox( oControl ).DropDownCount := _value;
end;

function T_PvlComboBox.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := FOnChange;
end;

procedure T_PvlComboBox.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  FOnChange := _value;
end;

procedure T_PvlComboBox.fset_Height(
  const _value : Integer
);
begin
  inherited;
  if _value > 0 then
    TComboBox( oControl ).Font.Height := RoundS( _value * oContext.PPIFix / 1.5 );
end;

procedure T_PvlComboBox.BeginUpdate;
begin
  TComboBox( oControl ).Items.BeginUpdate;
end;

procedure T_PvlComboBox.EndUpdate;
begin
  TComboBox( oControl ).Items.EndUpdate;
end;

procedure T_PvlComboBox.ItemsClear;
begin
  TComboBox( oControl ).Items.Clear;
end;

procedure T_PvlComboBox.ItemsAdd(
  const _item  : String
);
begin
  TComboBox( oControl ).Items.Add( _item );
end;

function T_PvlComboBox.IndexOf(
  const _item: string
): Integer;
begin
  Result := TComboBox( oControl ).Items.IndexOf( _item );
end;

procedure T_PvlComboBox.SetFocus;
begin
  TComboBox( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlComboBox'}

{$REGION 'T_PvlComboEdit'}

procedure T_PvlComboEdit.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TComboBox.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );
  TComboBox( oControl ).Style := csDropDown;
  TComboBox( oControl ).DropDownCount := 5;
  TComboBox( oControl ).AutoComplete := True ;
  TComboBox( oControl ).OnChange := doOnChange;
  TComboBox( oControl ).OnKeyUp := doOnKeyUp ;
  TComboBox( oControl ).OnKeyDown := doOnKeyDown ;

  bFilteredSearch := False ;
  bUpDown := False ;
  bLeftRight := False ;
end;

procedure T_PvlComboEdit.fset_FilteredSearch(
  const _value: Boolean
) ;
begin
  if not _value then begin
    TComboBox( oControl ).AutoComplete := True ;
    exit ;
  end else begin
    TComboBox( oControl ).AutoComplete := False ;
  end;

  bFilteredSearch := _value ;
end;

function T_PvlComboEdit.fget_FilteredSearch
  : Boolean ;
begin
  Result := bFilteredSearch ;
end;

procedure T_PvlComboEdit.doDestroy;
begin
  FreeObject( lstItems ) ;
  inherited ;
end;

procedure T_PvlComboEdit.ItemsClear;
begin
  TComboBox( oControl ).Items.Clear;
  FreeObject( lstItems ) ;
end;

procedure T_PvlComboEdit.ItemsAdd(
  const _item  : String
);
begin
  inherited ;

  if not assigned( lstItems ) then
    lstItems := TGIS_ListOfStrings.Create ;

  lstItems.Add( _item ) ;
end;

procedure T_PvlComboEdit.doOnKeyUp(
       _sender : TObject;
   var _key    : Word;
       _shift  : TShiftState
);
var
  curr : String ;

  function IsCtrlPressed: Boolean;
  var
    State: TKeyboardState;
  begin
    GetKeyboardState(State);
    Result := ((State[VK_CONTROL] and 128) <> 0);
  end;
begin
  inherited ;

  try

    if not TGIS_PvlComboEdit( oParent ).FilteredSearch then begin
      exit ;
    end;

    if bUpDown then begin
      TComboBox( oControl ).DroppedDown := True ;
      exit ;
    end;

    if ( _key = VK_ESCAPE ) then begin
      TComboBox( oControl ).DroppedDown := False ;
      TComboBox( oControl ).Text := curr ;
      exit ;
    end ;

    if ( _key = VK_RETURN ) then begin
      TComboBox( oControl ).DroppedDown := False ;
      exit ;
    end ;

    if _key in [VK_LEFT..VK_RIGHT] then begin
      exit ;
    end;

    curr :=  TComboBox( oControl ).Text ;

    if ( _key = vkA ) and IsCtrlPressed then begin
      TComboBox( oControl ).SelStart := 0 ;
      TComboBox( oControl ).SelLength := Length( curr ) ;
      exit ;
    end;

    if ( _key = VK_DELETE ) then begin
      TComboBox( oControl ).SelStart := isel ;
      TComboBox( oControl ).DroppedDown := True ;
      exit;
    end;

    if not ( ( _key in [vkA..vkZ] ) or ( _key in [vk0..vk9] ) ) then begin
      TComboBox( oControl ).DroppedDown := True ;
      TComboBox( oControl ).Text := curr ;
      if not ( _key = VK_CONTROL ) then
        TComboBox( oControl ).SelStart := Length( curr ) ;
      exit ;
    end;

    if ( TComboBox( oControl ).Items.Count = 1 ) then begin
      TComboBox( oControl ).DroppedDown := False ;
      TComboBox( oControl ).Text := TComboBox( oControl ).Items.Strings[0] ;
      TComboBox( oControl ).SelStart := Length( curr ) ;
      iSel := TComboBox( oControl ).SelStart ;
      TComboBox( oControl ).SelLength := TComboBox( oControl ).Items.Strings[0].Length - Length( curr ) ;
      exit ;
    end
    else begin
      TComboBox( oControl ).DroppedDown := True ;
      TComboBox( oControl ).Text := curr ;
      TComboBox( oControl ).SelStart := Length( curr ) ;
    end;

    doOnChange( Parent ) ;
  finally
    isel := TComboBox( oControl ).SelStart ;
  end;
end;

procedure T_PvlComboEdit.doOnKeyDown(
       _sender : TObject;
   var _key    : Word;
       _shift  : TShiftState
);
begin
  inherited ;

  prevIdx := TComboBox( oControl ).ItemIndex ;

  if ( _key in [VK_UP..VK_DOWN] ) then begin
    bUpDown := True ;
  end else begin
    bUpDown := False ;
  end;

  if ( _key in [VK_LEFT..VK_RIGHT] ) then begin
    bLeftRight := True ;
  end else begin
    bLeftRight := False ;
  end;
end;

procedure T_PvlComboEdit.doOnChange(
  _sender: TObject
) ;
var
  filter  : string  ;
  i       : Integer ;
  idx     : Integer ;
begin
  inherited ;

  if TGIS_PvlComboEdit( oParent ).FilteredSearch then begin

    if bLeftRight then
    exit ;

    if bUpDown then
    begin
      if prevIdx = -1 then
        TComboBox( oControl ).ItemIndex := 0 ;
      exit ;
    end;

    filter := TComboBox( oControl ).Text;
    TComboBox( oControl ).SelStart := Length(filter);

    for i := 0 to lstItems.Count - 1 do begin
      if SameText(LeftStr(lstItems[i], Length(TComboBox( oControl ).Text)), TComboBox( oControl ).Text) then
      begin
        if TComboBox( oControl ).Items.IndexOf(lstItems[i]) < 0 then
          TComboBox( oControl ).Items.Add(lstItems[i]);
      end
      else
      begin
        idx := TComboBox( oControl ).Items.IndexOf(lstItems[i]);
        if idx >= 0 then
          TComboBox( oControl ).Items.Delete(idx);
      end;
    end;
  end;
end;

{$ENDREGION 'T_PvlComboEdit'}

{$REGION 'T_PvlCheckBox'}

procedure T_PvlCheckBox.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TCheckBox.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );
  TCheckBox( oControl ).OnClick := doOnClick;
end;

procedure T_PvlCheckBox.doOnClick(
  _sender: TObject
);
begin
  if Assigned( FOnClick ) then
    FOnClick( Parent );
end;

function T_PvlCheckBox.fget_Caption
  : string;
begin
  Result := TCheckBox( oControl ).Caption;
end;

procedure T_PvlCheckBox.fset_Caption(
  const _value : string
);
begin
  TCheckBox( oControl ).Caption := _value;
end;

function T_PvlCheckBox.fget_Checked
  : Boolean;
begin
  Result := TCheckBox( oControl ).Checked;
end;

procedure T_PvlCheckBox.fset_Checked(
  const _value : Boolean
);
begin
  TCheckBox( oControl ).Checked := _value;
end;

function T_PvlCheckBox.fget_FontSize
  : Integer;
begin
  Result := TCheckBox( oControl ).Font.Size;
end;

procedure T_PvlCheckBox.fset_FontSize(
  const _value : Integer
);
begin
  TCheckBox( oControl ).Font.Size := _value;
end;

function T_PvlCheckBox.fget_FontStyle
  : TGIS_FontStyles;
var
  iStyle : TGIS_FontStyles;
begin
  iStyle := GisGetEmptyFontStyle;
  if TFontStyle.fsBold in TCheckBox( oControl ).Font.Style then
    iStyle := GisAddFontStyle( iStyle, TGIS_FontStyle.Bold );
  if TFontStyle.fsItalic in TCheckBox( oControl ).Font.Style then
    iStyle := GisAddFontStyle( iStyle, TGIS_FontStyle.Italic );
  if TFontStyle.fsUnderline in TCheckBox( oControl ).Font.Style then
    iStyle := GisAddFontStyle( iStyle, TGIS_FontStyle.Underline );
  if TFontStyle.fsStrikeOut in TCheckBox( oControl ).Font.Style then
    iStyle := GisAddFontStyle( iStyle, TGIS_FontStyle.StrikeOut );
  Result := iStyle;
end;

procedure T_PvlCheckBox.fset_FontStyle(
  const _value : TGIS_FontStyles
);
begin
  if TGIS_FontStyle.Bold in _value then
    TCheckBox( oControl ).Font.Style := TCheckBox( oControl ).Font.Style + [ TFontStyle.fsBold ];
  if TGIS_FontStyle.Italic in _value then
    TCheckBox( oControl ).Font.Style := TCheckBox( oControl ).Font.Style + [ TFontStyle.fsItalic ];
  if TGIS_FontStyle.Underline in _value then
    TCheckBox( oControl ).Font.Style := TCheckBox( oControl ).Font.Style + [ TFontStyle.fsUnderline ];
  if TGIS_FontStyle.StrikeOut in _value then
    TCheckBox( oControl ).Font.Style := TCheckBox( oControl ).Font.Style + [ TFontStyle.fsStrikeOut ];
end;

function T_PvlCheckBox.fget_FontFamily
  : string;
begin
  Result := TCheckBox( oControl ).Font.Name;
end;

procedure T_PvlCheckBox.fset_FontFamily(
  const _value : string
);
begin
  TCheckBox( oControl ).Font.Name := _value;
end;

function T_PvlCheckBox.fget_OnClick
  : TGIS_PvlEvent;
begin
  Result := FOnClick;
end;

procedure T_PvlCheckBox.fset_OnClick(
  const _value : TGIS_PvlEvent
);
begin
  FOnClick := _value;
end;

procedure T_PvlCheckBox.SetFocus;
begin
  TCheckBox( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlCheckBox'}

{$REGION 'T_PvlRadioButton'}

procedure T_PvlRadioButton.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TRadioButton.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );
  TCheckBox( oControl ).OnClick := doOnClick;
end;

procedure T_PvlRadioButton.doOnClick(
  _sender: TObject
);
begin
  try
    TCheckBox( oControl ).OnClick := nil;
    TGIS_PvlRadioButton( Self.oParent ).UncheckGroup(
      TGIS_PvlRadioButton( Self.oParent )
    );

    if Assigned( FOnClick ) then
      FOnClick( Parent );
  finally
    TCheckBox( oControl ).OnClick := doOnClick;
  end;
end;

function T_PvlRadioButton.fget_Caption
  : string;
begin
  Result := TRadioButton( oControl ).Caption;
end;

procedure T_PvlRadioButton.fset_Caption(
  const _value : string
);
begin
  TRadioButton( oControl ).Caption := _value;
end;

function T_PvlRadioButton.fget_Group
  : String;
begin
  // do nothing
end;

procedure T_PvlRadioButton.fset_Group(
  const _value : String
);
begin
  // do nothing
end;

function T_PvlRadioButton.fget_Checked
  : Boolean;
begin
  Result := TRadioButton( oControl ).Checked;
end;

procedure T_PvlRadioButton.fset_Checked(
  const _value : Boolean
);
begin
  TRadioButton( oControl ).Checked := _value;
end;

function T_PvlRadioButton.fget_FontSize
  : Integer;
begin
  Result := TRadioButton( oControl ).Font.Size;
end;

procedure T_PvlRadioButton.fset_FontSize(
  const _value : Integer
);
begin
  TRadioButton( oControl ).Font.Size := _value;
end;

function T_PvlRadioButton.fget_FontStyle
  : TGIS_FontStyles;
var
  iStyle : TGIS_FontStyles;
begin
  iStyle := GisGetEmptyFontStyle;
  if TFontStyle.fsBold in TRadioButton( oControl ).Font.Style then
    iStyle := GisAddFontStyle( iStyle, TGIS_FontStyle.Bold );
  if TFontStyle.fsItalic in TRadioButton( oControl ).Font.Style then
    iStyle := GisAddFontStyle( iStyle, TGIS_FontStyle.Italic );
  if TFontStyle.fsUnderline in TRadioButton( oControl ).Font.Style then
    iStyle := GisAddFontStyle( iStyle, TGIS_FontStyle.Underline );
  if TFontStyle.fsStrikeOut in TRadioButton( oControl ).Font.Style then
    iStyle := GisAddFontStyle( iStyle, TGIS_FontStyle.StrikeOut );
  Result := iStyle;
end;

procedure T_PvlRadioButton.fset_FontStyle(
  const _value : TGIS_FontStyles
);
begin
  if TGIS_FontStyle.Bold in _value then
    TCheckBox( oControl ).Font.Style := TRadioButton( oControl ).Font.Style + [ TFontStyle.fsBold ];
  if TGIS_FontStyle.Italic in _value then
    TCheckBox( oControl ).Font.Style := TRadioButton( oControl ).Font.Style + [ TFontStyle.fsItalic ];
  if TGIS_FontStyle.Underline in _value then
    TCheckBox( oControl ).Font.Style := TRadioButton( oControl ).Font.Style + [ TFontStyle.fsUnderline ];
  if TGIS_FontStyle.StrikeOut in _value then
    TCheckBox( oControl ).Font.Style := TRadioButton( oControl ).Font.Style + [ TFontStyle.fsStrikeOut ];
end;

function T_PvlRadioButton.fget_FontFamily
  : string;
begin
  Result := TRadioButton( oControl ).Font.Name;
end;

procedure T_PvlRadioButton.fset_FontFamily(
  const _value : string
);
begin
  TRadioButton( oControl ).Font.Name := _value;
end;

function T_PvlRadioButton.fget_OnClick
  : TGIS_PvlEvent;
begin
  Result := FOnClick;
end;

procedure T_PvlRadioButton.fset_OnClick(
  const _value : TGIS_PvlEvent
);
begin
  FOnClick := _value;
end;

procedure T_PvlRadioButton.SetFocus;
begin
  TRadioButton( oControl ).SetFocus;
end;

procedure T_PvlRadioButton.UncheckGroup(
  const _button : TGIS_PvlRadioButton
);
begin
  // do nothing
end;

{$ENDREGION 'T_PvlRadioButton'}

{$REGION 'T_PvlPanel'}

procedure T_PvlPanel.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TScrollBox.Create( TWinControl( _context.NativeParent ) );
  TScrollBox( oControl ).BorderStyle := bsNone;
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );

  itmMap := TDictionary<String,TGIS_PvlControl>.Create;

  inherited;
end;

procedure T_PvlPanel.doDestroy;
begin
  FreeObject( itmMap );
  inherited ;
end;

function T_PvlPanel.fget_Scrollable
  : Boolean;
begin
  Result := TScrollBox( oControl ).AutoScroll;
end;

procedure T_PvlPanel.fset_Scrollable(
  const _value : Boolean
);
begin
  TScrollBox( oControl ).AutoScroll := _value;
  TScrollBox( oControl ).HorzScrollBar.Visible := _value;
  TScrollBox( oControl ).VertScrollBar.Visible := _value;
end;

function T_PvlPanel.fget_Border
  : Boolean;
begin
  Result := TScrollBox( oControl ).BorderStyle = bsSingle;
end;

procedure T_PvlPanel.fset_Border(
  const _value : Boolean
);
begin
  if _value then begin
    TScrollBox( oControl ).BorderStyle := bsSingle;
  end else begin
    TScrollBox( oControl ).BorderStyle := bsNone;
  end;
end;

procedure T_PvlPanel.AddComponent(
  const _component: TGIS_PvlControl
);
begin
  TControl( _component.NativeControl ).Parent := TWinControl( oControl );
  if ( _component.NativeControl is TListView ) or
     ( _component.NativeControl is TListBox  )
  then begin
    TControl( _component.NativeControl ).Width := TScrollBox( oControl ).Width;
    TControl( _component.NativeControl ).Height := TScrollBox( oControl ).Height;
    TScrollBox( oControl ).HorzScrollBar.Visible := False;
    TScrollBox( oControl ).VertScrollBar.Visible := False;
  end;
end;

procedure T_PvlPanel.AddComponent(
  const _component  : TGIS_PvlControl;
  const _id         : String
);
begin
  if not assigned( itmMap ) then
    itmMap := TDictionary<String,TGIS_PvlControl>.create;

  if itmMap.ContainsKey( _id ) then begin
      raise EGIS_Exception.Create(
        _rsrc( GIS_RS_ERR_FIELDEXIST ), '', 0
      );
  end;

  itmMap.Add( _id, _component );
  AddComponent( _component );
end;

procedure T_PvlPanel.RemoveAllComponents;
var
  i : Integer;
  itm : TComponent;
begin
  for i := ( TScrollBox( oControl ).ControlCount - 1) downto 0 do
  begin
    itm := TScrollBox( oControl ).Controls[i];
    itm.Free;
  end;

  itmMap.Clear ;
end;

function T_PvlPanel.GetById(
  const _id: String
) : TGIS_PvlControl;
begin
  itmMap.TryGetValue( _id, Result );
end;

{$ENDREGION 'T_PvlPanel'}

{$REGION 'T_PvlPreviewPanel'}
procedure T_PvlPreviewPanel.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TPanel.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );
  TPanel( oControl ).Ctl3D := False;
  TPanel( oControl ).BevelInner := bvNone;
  TPanel( oControl ).BevelOuter := bvNone;
  TPanel( oControl ).BevelKind  := bkNone;

  oLabel := TLabel.Create( TPanel( oControl ) );
  oLabel.Parent := TPanel( oControl );
  oLabel.Visible := False;
  oLabel.AutoSize := False;
  oLabel.Alignment := taCenter;
  oLabel.Align := TAlign.alClient;
  oLabel.Layout := tlCenter;

  oImage := TImage.Create( TPanel( oControl ) );
  oImage.Parent := TPanel( oControl );
  oImage.Visible := False;
  oImage.AutoSize := False;
  oImage.Align := TAlign.alClient;
  oImage.Stretch := True;

  cStyledAreaColor := TGIS_Color.Red;

  applyStyle;
end;

procedure T_PvlPreviewPanel.applyStyle;
begin
  cStyledAreaColor := GISColor( StyleServices.GetSystemColor( clWindowText ) );
end;

function T_PvlPreviewPanel.fget_IsStyled
  : Boolean;
begin
  Result := True;
end;

function T_PvlPreviewPanel.fget_Caption
  : string;
begin
  Result := oLabel.Caption;
end;

procedure T_PvlPreviewPanel.fset_Caption(
  const _value : string
);
begin
  oLabel.Caption := _value;
  Invalidate;
end;

function T_PvlPreviewPanel.fget_Border
  : Boolean;
begin
  Result := TPanel( oControl ).BorderWidth > 0
end;

procedure T_PvlPreviewPanel.fset_Border(
  const _value : Boolean
);
begin
  if _value then begin
    TPanel( oControl ).BorderWidth := 1;
    TPanel( oControl ).BorderStyle := bsSingle;
  end else begin
    TPanel( oControl ).BorderWidth := 0;
    TPanel( oControl ).BorderStyle := bsNone;
  end;
end;

function T_PvlPreviewPanel.fget_FontSize
  : Integer;
begin
  Result := oLabel.Font.Size;
end;

procedure T_PvlPreviewPanel.fset_FontSize(
  const _value : Integer
);
begin
  oLabel.Font.Size := _value;
end;

function T_PvlPreviewPanel.fget_FontStyle
  : TGIS_FontStyles;
var
  iStyle : TGIS_FontStyles;
begin
  iStyle := GisGetEmptyFontStyle;
  if TFontStyle.fsBold in oLabel.Font.Style then
    iStyle := GisAddFontStyle( iStyle, TGIS_FontStyle.Bold );
  if TFontStyle.fsItalic in oLabel.Font.Style then
    iStyle := GisAddFontStyle( iStyle, TGIS_FontStyle.Italic );
  if TFontStyle.fsUnderline in oLabel.Font.Style then
    iStyle := GisAddFontStyle( iStyle, TGIS_FontStyle.Underline );
  if TFontStyle.fsStrikeOut in oLabel.Font.Style then
    iStyle := GisAddFontStyle( iStyle, TGIS_FontStyle.StrikeOut );
  Result := iStyle;
end;

procedure T_PvlPreviewPanel.fset_FontStyle(
  const _value : TGIS_FontStyles
);
begin
  oLabel.Font.Style := [];
  if TGIS_FontStyle.Bold in _value then
    oLabel.Font.Style := oLabel.Font.Style + [ TFontStyle.fsBold ];
  if TGIS_FontStyle.Italic in _value then
    oLabel.Font.Style := oLabel.Font.Style + [ TFontStyle.fsItalic ];
  if TGIS_FontStyle.Underline in _value then
    oLabel.Font.Style := oLabel.Font.Style + [ TFontStyle.fsUnderline ];
  if TGIS_FontStyle.StrikeOut in _value then
    oLabel.Font.Style := oLabel.Font.Style + [ TFontStyle.fsStrikeOut ];
end;

function T_PvlPreviewPanel.fget_FontFamily
  : string;
begin
  Result := oLabel.Font.Name;
end;

procedure T_PvlPreviewPanel.fset_FontFamily(
  const _value : string
);
begin
  oLabel.Font.Name := _value;
end;

function T_PvlPreviewPanel.fget_Bitmap
  : TGIS_Bitmap;
var
  bmp : TGIS_Bitmap;
begin
  bmp := TGIS_Bitmap.Create;
  bmp.LoadFromBitmap( oImage.Picture.Bitmap, '' );
  Result := bmp;
end;

procedure T_PvlPreviewPanel.fset_Bitmap(
  const _value : TGIS_Bitmap
);
begin
  if Assigned( _value ) then begin
    oImage.Width := self.fget_Width;
    oImage.Height := self.fget_Height;
    bBitmap := False;
    if _value.NativeBitmap is TBitmap then begin
      oImage.Picture.Assign( TBitmap( _value.NativeBitmap ) );
      bBitmap := True;
    end;
    Invalidate;
  end;
end;

function T_PvlPreviewPanel.fget_Color
  : TGIS_Color;
begin
  Result := GISColor( TPanel( oControl ).Color );
end;

procedure T_PvlPreviewPanel.fset_Color(
  const _value : TGIS_Color
);
begin
  TPanel( oControl ).Color := VCLColor( _value );
end;

function T_PvlPreviewPanel.fget_StyledAreaColor
  : TGIS_Color;
begin
  Result := cStyledAreaColor;
end;


procedure T_PvlPreviewPanel.SetFocus;
begin
  TPanel( oControl ).SetFocus;
end;

procedure T_PvlPreviewPanel.Invalidate;
begin
  if bBitmap then begin
    oLabel.Visible := False;
    oImage.Visible := True;
  end else begin
    oLabel.Visible := True;
    oImage.Visible := False;
  end;
end;

procedure T_PvlPreviewPanel.DoRedraw;
begin
  applyStyle;
end;
{$ENDREGION 'T_PvlPreviewPanel'}

{$REGION 'T_PvlGroupBox'}

procedure T_PvlGroupBox.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TGroupBox.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );
end;

function T_PvlGroupBox.fget_Caption
  : String;
begin
  Result := TGroupBox( oControl ).Caption;
end;

procedure T_PvlGroupBox.fset_Caption(
  const _value : String
);
begin
  TGroupBox( oControl ).Caption := _value;
end;

{$ENDREGION 'T_PvlGroupBox'}

{$REGION 'T_PvlTrackBar'}

procedure T_PvlTrackBar.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TTrackBar.Create( TWinControl( _context.NativeParent ) ) ;
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );
  TTrackBar( oControl ).TickStyle := tsNone ;
  TTrackBar( oControl ).OnChange := doOnChange ;
end;

function T_PvlTrackBar.fget_Minimum
  : Integer ;
begin
  Result := TTrackBar( oControl ).Min
end;

procedure T_PvlTrackBar.fset_Minimum(
  const _value: Integer
) ;
begin
  TTrackBar( oControl ).Min := _value ;
end;

function T_PvlTrackBar.fget_Maximum
  : Integer ;
begin
  Result := TTrackBar( oControl ).Max
end;

procedure T_PvlTrackBar.fset_Maximum(
  const _value: Integer
) ;
begin
  TTrackBar( oControl ).Max := _value ;
end;

function T_PvlTrackBar.fget_Position
  : Integer ;
begin
  Result := TTrackBar( oControl ).Position
end;

procedure T_PvlTrackBar.fset_Position(
  const _value: Integer
) ;
begin
  TTrackBar( oControl ).Position := _value ;
end;

function T_PvlTrackBar.fget_OnChange
  : TGIS_PvlEvent ;
begin
  Result := FOnChange ;
end;

procedure T_PvlTrackBar.fset_OnChange(
  const _value: TGIS_PvlEvent
) ;
begin
  FOnChange := _value ;
end;

procedure T_PvlTrackBar.doOnChange(
  _sender: TObject
) ;
begin
  if assigned( FOnChange ) then
    FOnChange( Parent ) ;
end;

procedure T_PvlTrackBar.SetFocus;
begin
  TTrackBar( oControl ).SetFocus ;
end;

{$ENDREGION 'T_PvlTrackBar'}

{$REGION 'T_PvlTree'}

procedure T_PvlTree.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TTreeView.Create( TWinControl( _context.NativeParent ) );
  TControl( oControl ).Parent := TWinControl( _context.NativeParent );

  TTreeView( oControl ).ShowLines := True;
  TTreeView( oControl ).OnClick := doOnClick;
  TTreeView( oControl ).OnChange := doOnSelectChange;
  TTreeView( oControl ).OnDeletion := doOnDeletion;
end;

procedure T_PvlTree.doDestroy;
begin
  FreeObject( oRoot );
  inherited;
end;

procedure T_PvlTree.doOnClick(
  _sender: TObject
);
begin
  if assigned( FOnClick ) then
    FOnClick( Parent );
end;

procedure T_PvlTree.doOnSelectChange(
  _sender: TObject;
  _node: TTreeNode
);
begin
  if assigned( FOnSelectChange ) then
    FOnSelectChange( Parent  );
end;

procedure T_PvlTree.doOnDeletion(
  _sender : TObject;
  _node   : TTreeNode
);
begin
  TGIS_PvlTreeNode( _node.Data ).RemoveNode ;

  FreeObjectNotNil( _node.Data );
  _node.Data := nil ;
  inherited;
end;

function T_PvlTree.fget_Root
  : TGIS_PvlTreeNode;
begin
  Result := oRoot;
end;

function T_PvlTree.fget_Selected
  : TGIS_PvlTreeNode;
begin
  if Assigned( TTreeView( oControl ).Selected ) then
    Result := TTreeNode( TTreeView( oControl ).Selected ).Data
  else
    Result := nil;
end;

procedure T_PvlTree.fset_Selected(
  const _value : TGIS_PvlTreeNode
);
begin
  TTreeView( oControl ).ClearSelection( False );
  TTreeNode( _value.NativeControl ).Selected := True;
end;

procedure T_PvlTree.fset_OnClick(
  const _value: TGIS_PvlEvent
) ;
begin
  FOnClick := _value ;
end;

function T_PvlTree.fget_OnClick
  : TGIS_PvlEvent ;
begin
  Result := FOnClick ;
end;

procedure T_PvlTree.fset_OnSelectChange(
  const _value: TGIS_PvlEvent
) ;
begin
  FOnSelectChange := _value ;
end;

function T_PvlTree.fget_OnSelectChange
  : TGIS_PvlEvent ;
begin
  Result := FOnSelectChange ;
end;

procedure T_PvlTree.CreateRoot;
begin
  if not Assigned( oRoot ) then begin
    oRoot := TGIS_PvlTreeNode.Create( TGIS_PvlTree( oParent ) );
    oRoot.Caption := 'ROOT';
  end;
end;


procedure T_PvlTree.SetFocus;
begin
  inherited;
end;

{$ENDREGION 'T_PvlTree'}

{$REGION 'T_PvlTreeNode'}

constructor T_PvlTreeNode.Create(
  const _context : TGIS_PvlContext ;
  const _parent  : IGIS_PvlBase
);
begin

  inherited;
  oControl := nil;
end;

procedure T_PvlTreeNode.doDestroy;
var
  evnt : TTVExpandedEvent;
begin
  if Assigned( oControl ) then begin
    evnt := TTreeView( TTreeNode( oControl ).TreeView ).OnDeletion;
    TTreeView( TTreeNode( oControl ).TreeView ).OnDeletion := nil ;
    TTreeNode( oControl ).Delete;
    TTreeView( TTreeNode( oControl ).TreeView ).OnDeletion := evnt ;
    oControl := nil;
  end;

  inherited;
end;

procedure T_PvlTreeNode.doCreateNode(
  const _node    : TGIS_PvlTreeNode;
  const _caption : String;
  const _index   : Integer
);
var
  tv : TTreeView;
begin
  tv := TTreeView( _node.TreeControl.NativeControl );

  //  Count - 1 because its already added to Parent in object list.
  if _index >= _node.Count - 1 then begin
    if Assigned( _node.Parent ) then // nil only for Root node!
      _node.NativeControl := tv.Items.AddChild(
                                TTreeNode( _node.Parent.NativeControl ),
                                _node.Caption
                              )
    else
      _node.NativeControl := tv.Items.Add(
                                nil,
                                _caption
                              );
  end else begin
    if Assigned( _node.Parent ) then // nil only for Root node!
      _node.NativeControl := tv.Items.Insert(
                                TTreeNode( _node.Parent.NativeControl ).Item[_index],
                                _caption
                              )
    else
      _node.NativeControl := tv.Items.Insert(
                                tv.Items[_index],
                                _caption
                              );
  end;

  TTreeNode( _node.NativeControl ).Data := _node;

end;

procedure T_PvlTreeNode.doRemoveNode(
  const _node : TGIS_PvlTreeNode
);
var
  tv   : TTreeView;
  evnt : TTVExpandedEvent;
begin
  if Assigned( _node.Parent ) then begin
    tv := TTreeView( _node.TreeControl.NativeControl );

    evnt := tv.OnDeletion;
    tv.OnDeletion := nil;
    TTreeNode( _node.NativeControl  ).Delete;
    tv.OnDeletion := evnt;
  end;
end;

procedure T_PvlTreeNode.doDeleteNode(
  const _node : TGIS_PvlTreeNode
);
begin
  if Assigned( _node.Parent ) then begin
    TTreeNode( _node.NativeControl  ).Delete;
  end;
end;

procedure T_PvlTreeNode.doMoveNode(
  const _node       : TGIS_PvlTreeNode;
  const _newParent  : TGIS_PvlTreeNode;
  const _index      : Integer
);
var
  tv : TTreeView;
begin
  tv := TTreeView( _node.TreeControl.NativeControl );

  //  Count - 1 because its already added to Parent in object list.
  if _index >= _newParent.Count - 1 then begin
    if _newParent.NativeControl <> nil then // nil only for Root node!
      _node.NativeControl := tv.Items.AddChild(
                                TTreeNode( _newParent.NativeControl ),
                                _node.Caption
                              )
    else
      _node.NativeControl := tv.Items.Add(
                                TTreeNode( _newParent.NativeControl ),
                                _node.Caption
                              );
  end else begin
    if _newParent.NativeControl <> nil then // nil only for Root node!
      _node.NativeControl := tv.Items.Insert(
                                TTreeNode( _newParent.NativeControl ).Item[_index],
                                _node.Caption
                              )
    else
      _node.NativeControl := tv.Items.Insert(
                                tv.Items[_index],
                                _node.Caption
                              );
  end;

end;

function T_PvlTreeNode.fget_Caption: String;
begin
  Result := sCaption;
end;

procedure T_PvlTreeNode.fset_Caption(
  const _value : String
);
begin
  sCaption := _value;
  if Assigned( oControl ) then
    TTreeNode( oControl ).Text := _value;
end;

function T_PvlTreeNode.fget_NativeControl : TObject;
begin
  Result := oControl;
end;

procedure T_PvlTreeNode.fset_NativeControl(
  const _value: TObject
);
begin
  oControl := _value;
end;

function T_PvlTreeNode.fget_Expanded
  : Boolean;
begin
  Result := TTreeNode( oControl ).Expanded;
end;

procedure T_PvlTreeNode.fset_Expanded(
  const _value : Boolean
);
begin
  TTreeNode( oControl ).Expanded := _value;
end;

function T_PvlTreeNode.fget_Node(
  const _index: Integer
): TGIS_PvlTreeNode;
begin
  //For safe inheritance only
  Result := nil ;
end;

function T_PvlTreeNode.fget_Parent
  : TGIS_PvlTreeNode;
begin
  //For safe inheritance only
  Result := nil ;
end;

{$ENDREGION 'T_PvlTreeNode'}

{$REGION 'T_PvlListBox'}
procedure T_PvlListBox.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TListBox.Create( TWinControl( _context.NativeParent ) );
  TListBox( oControl ).Parent := TWinControl( _context.NativeParent );
  TListBox( oControl ).OnClick := doOnClick;
  TListBox( oControl ).Ctl3D := False;
  TListBox( oControl ).BevelInner := bvNone;
  TListBox( oControl ).BevelOuter := bvNone;
  TListBox( oControl ).BevelKind  := bkNone;
  TListBox( oControl ).BorderStyle := bsNone;
end;

procedure T_PvlListBox.doDestroy ;
begin
  FreeObject( lstItm ) ;
  FreeObject( lstSel ) ;

  inherited ;
end;

function T_PvlListBox.fget_ItemList
  : TGIS_ListOfStrings;
var
  i : Integer;
begin
  if not assigned( lstItm ) then
    lstItm := TGIS_ListOfStrings.Create ;

  lstItm.Clear ;

  for i := 0 to TListBox( oControl ).Count - 1 do
  begin
    lstItm.Add( TListBox( oControl ).Items[ i ] ) ;
  end;

  Result := lstItm ;
end;

function T_PvlListBox.fget_Item(
  const _idx: Integer
) : string;
begin
  if _idx >= 0 then
    Result := TListBox( oControl ).Items[_idx];
end;

function T_PvlListBox.fget_ItemIndex
  : Integer;
begin
  Result := TListBox( oControl ).ItemIndex;
end;

procedure T_PvlListBox.fset_ItemIndex(
  const _value : Integer
);
begin
  TListBox( oControl ).ItemIndex := _value;
end;

function T_PvlListBox.fget_SelectedItems
  : TGIS_ListOfStrings ;
var
  i: Integer;
begin
  if not assigned( lstSel ) then
    lstSel := TGIS_ListOfStrings.Create ;

  lstSel.Clear ;

  for i := 0 to TListBox( oControl ).Count - 1 do
  begin
    if TListBox( oControl ).Selected[ i ] then
      lstSel.Add( TListBox( oControl ).Items[ i ] ) ;
  end;

  Result := lstSel ;
end;

function T_PvlListBox.fget_Multiselect
  : Boolean ;
begin
  Result := TListBox( oControl ).MultiSelect ;
end;

procedure T_PvlListBox.fset_Multiselect(
  const _value: Boolean
) ;
begin
  TListBox( oControl ).MultiSelect := _value ;
end;

function T_PvlListBox.fget_Selected(
  const _index: Integer
) : Boolean ;
begin
  Result := TListBox( oControl ).Selected[ _index ] ;
end;

procedure T_PvlListBox.fset_Selected(
  const _index: Integer;
  const _value: Boolean
) ;
begin
  TListBox( oControl ).Selected[ _index ] := _value ;
end;

function T_PvlListBox.fget_ItemsCount
  : Integer;
begin
  Result := TListBox( oControl ).Items.Count;
end;

function T_PvlListBox.fget_OnClick
  : TGIS_PvlEvent;
begin
  Result := FOnClick;
end;

procedure T_PvlListBox.fset_OnClick(
  const _value : TGIS_PvlEvent
);
begin
  FOnClick := _value;
end;

procedure T_PvlListBox.BeginUpdate;
begin
  TListBox( oControl ).Items.BeginUpdate;
end;

procedure T_PvlListBox.EndUpdate;
begin
  TListBox( oControl ).Items.EndUpdate;
end;

procedure T_PvlListBox.ItemsClear;
begin
  TListBox( oControl ).Items.Clear;
end;

procedure T_PvlListBox.ItemsAdd(
  const _item: string
);
begin
  TListBox( oControl ).Items.Add( _item );
end;

procedure T_PvlListBox.SetFocus;
begin
  TListBox( oControl ).SetFocus;
end;

procedure T_PvlListBox.doOnClick(
  _sender: TObject
);
begin
  if Assigned( FOnClick ) then
    FOnClick( Parent );
end;

{$ENDREGION 'T_PvlListBox'}

{$REGION 'T_PvlSVGList'}
procedure T_PvlSVGList.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TListView.Create( TWinControl( _context.NativeParent ) );
  TListView( oControl ).Parent := TWinControl( _context.NativeParent );
  TListView( oControl ).OnClick := doOnClick;
  TListView( oControl ).Ctl3D := False;
  TListView( oControl ).BevelInner := bvNone;
  TListView( oControl ).BevelOuter := bvNone;
  TListView( oControl ).BevelKind  := bkNone;
  TListView( oControl ).BorderStyle := bsNone;

  oLib := TGIS_SymbolLibrarySVG.Handle;
  oLib.OpenForBrowsing;

  lstNames := TStringList.Create;

  svgList := TImageList.Create( TWinControl( _context.NativeParent ) );
  svgList.ColorDepth := cd32Bit;
  svgList.DrawingStyle := dsTransparent;
  svgList.Height := RoundS( SVG_SIZE * oContext.CanvasScale * GUIScale );
  svgList.Width  := RoundS( SVG_SIZE * oContext.CanvasScale * GUIScale );

  TListView( oControl ).ViewStyle := vsIcon;
  TListView( oControl ).LargeImages := svgList;

  TListView( oControl ).Width  := fget_Width;
  TListView( oControl ).Height := fget_Height;

  applyStyle;
end;

procedure T_PvlSVGList.doDestroy;
begin
  FreeObject( lstNames );
  FreeObject( lstItm ) ;
  inherited;
end;

procedure T_PvlSVGList.applyStyle;
begin
  cForeground := GISColor( StyleServices.GetSystemColor( clWindowText ) );
  cBackground := GISColor( StyleServices.GetSystemColor( clWindow ) );
end;

function T_PvlSVGList.fget_ItemList
  : TGIS_ListOfStrings;
var
  idx : Integer;
begin
  if not assigned( lstItm ) then
    lstItm := TGIS_ListOfStrings.Create;

  lstItm.Clear ;
  for idx := 0 to TListView( oControl ).Items.Count - 1 do begin
    lstItm.Add( TListView( oControl ).Items.Item[ idx ].Caption );
  end;

  Result := lstItm ;
end;

function T_PvlSVGList.fget_Item(
  const _idx: Integer
) : string;
begin
  if _idx >= 0 then
    Result := lstNames[_idx];
end;

function T_PvlSVGList.fget_SelectedItems
  : TGIS_ListOfStrings ;
begin
  //For safe inheritance only
  Result := nil ;
end;

function T_PvlSVGList.fget_Multiselect
  : Boolean ;
begin
  // Inheritance only
  Result := False ;
end;

procedure T_PvlSVGList.fset_Multiselect(
  const _value: Boolean
) ;
begin
  // Inheritance only
end;

function T_PvlSVGList.fget_Selected(
  const _index: Integer
) : Boolean ;
begin
  //For safe inheritance only
  Result := False ;
end;

procedure T_PvlSVGList.fset_Selected(
  const _index: Integer;
  const _value: Boolean
) ;
begin
  // Inheritance only
end;

function T_PvlSVGList.fget_ItemIndex
  : Integer;
begin
  Result := TListView( oControl ).ItemIndex;
end;

procedure T_PvlSVGList.fset_ItemIndex(
  const _value : Integer
);
begin
  TListView( oControl ).ItemIndex := _value;
end;

function T_PvlSVGList.fget_ItemsCount
  : Integer;
begin
  Result := TListView( oControl ).Items.Count;
end;

function T_PvlSVGList.fget_OnClick
  : TGIS_PvlEvent;
begin
  Result := FOnClick;
end;

procedure T_PvlSVGList.fset_OnClick(
  const _value : TGIS_PvlEvent
);
begin
  FOnClick := _value;
end;

procedure T_PvlSVGList.BeginUpdate;
begin
  TListView( oControl ).Items.BeginUpdate;
end;

procedure T_PvlSVGList.EndUpdate;
begin
  TListView( oControl ).Items.EndUpdate;
end;

procedure T_PvlSVGList.ItemsClear;
var
  isize : Integer;
begin
  TListView( oControl ).Items.Clear;
  svgList.Clear;
  lstNames.Clear;

  oContext.Refresh;

  isize := RoundS( SVG_SIZE * oContext.CanvasScale * oContext.PPIFix * GUIScale );
  svgList.Height := isize;
  svgList.Width  := isize;
end;

procedure T_PvlSVGList.ItemsAdd(
  const _item: string
);
var
  itm   : TListItem;
  bmp   : TBitmap;
  tbmp  : TGIS_Bitmap;
begin
  itm := TListView( oControl ).Items.Add;
  itm.Caption := oLib.GetCaption( _item );

  bmp := TBitmap.Create;
  try
    tbmp := oLib.GetPreview( _item, oContext.PPI, svgList.Height,
                             cForeground, TGIS_Color.Silver
                            );
    tbmp.MakeGlowing( cBackground, 1 * Max( 1, RoundS( oContext.PPI / 96 ) ) );

    bmp.Assign( TBitmap( tbmp.NativeBitmap ) );

    svgList.Add( bmp, nil );
    lstNames.Add( _item );
    itm.ImageIndex := svgList.Count - 1;
  finally
    FreeObject( bmp );
  end;

end;

procedure T_PvlSVGList.SetFocus;
begin
  TListView( oControl ).SetFocus;
end;

procedure T_PvlSVGList.doOnClick(
  _sender: TObject
);
begin
  if Assigned( FOnClick ) then
    FOnClick( Parent );
end;

procedure T_PvlSVGList.DoRedraw;
var
  isize : Integer;
  bmp   : TBitmap;
  tbmp  : TGIS_Bitmap;
  sitem : String;
begin
  applyStyle;

  oContext.Refresh;
  isize := RoundS( SVG_SIZE * oContext.CanvasScale * oContext.PPIFix * GUIScale );

  svgList.Clear;
  svgList.Height := isize;
  svgList.Width  := isize;

  for sitem in lstNames do begin
    bmp := TBitmap.Create;
    try
      tbmp := oLib.GetPreview( sitem, oContext.PPI, svgList.Height,
                               cForeground, TGIS_Color.Silver
                             );
      tbmp.MakeGlowing( cBackground, 1 * Max( 1, RoundS( oContext.PPI / 96 ) ) );
      bmp.Assign( TBitmap( tbmp.NativeBitmap ) );

      svgList.Add( bmp, nil );
    finally
      FreeObject( bmp );
    end;
  end;
end;

{$ENDREGION 'T_PvlSVGList'}

{$REGION 'T_PvlContext'}

constructor T_PvlContext.Create(
  const _parent     : TObject;
  const _ppi        : Integer;
  const _ppifix     : Single;
  const _canvasscale: Single;
  const _righttoleft: Boolean;
  const _context    : TGIS_PvlContext
);
begin
  oMaster := _context;
  oParent := _parent;

  bBiDi := _righttoleft;
  iHMargin := 8 ;
  iVMargin := 8 ;

  oOwner := T_component.Create(TComponent(oParent));
  oOwner.oMaster := oMaster; // trick to destroyself TGIS_Pvl*
  oOwner.oOwner  := self;    // trick to destroyself T_Pvl*

  // create singleton component
  if _parent is TCustomForm then
    if not Assigned( TForm( _parent ).FindComponent( DPI_MANAGER ) ) then
      T_dpiManager.Create(TCustomForm(_parent));
end;

constructor T_PvlContext.Create(
  const _parent: TObject;
  const _context : TGIS_PvlContext
);
begin
  Create( _parent, _context, nil  );
end;

constructor T_PvlContext.Create(
  const _parent     : TObject;
  const _context    : TGIS_PvlContext ;
  const _refcontext : TGIS_PvlContext
);
var
  frm : TControl;
  prn : TObject;
  px  : Integer;
begin
  if _parent is TGIS_PvlBase then begin
    prn := TControl(  TGIS_PvlBase( _parent ).NativeControl );
  end
  else
    prn := _parent;

  frm := TControl(prn);
  while frm <> nil do begin
    if frm is TCustomForm then
      break;
    frm := frm.Parent;
  end;

  if Assigned( _refcontext ) then
    Create( prn,
            _refcontext.PPI,
            _refcontext.PPIfIX,
            _refcontext.CanvasScale,
            _refcontext.RightToLeft,
            _context
          )
  else begin
    px := TForm(frm).Canvas.Font.PixelsPerInch;
    Create( prn,
            px,
            px / 96,
            1,
            frm.BiDiMode = bdRightToLeft,
            _context
          );
  end ;

  // create singleton component
  if frm is TForm then
    if not Assigned( TForm( frm ).FindComponent( DPI_MANAGER ) ) then
      T_dpiManager.Create(TCustomForm(frm));
end;

procedure T_PvlContext.FreeContext ;
begin
  // force freeing of context
  oOwner.Free ;
end;

function T_PvlContext.GetAllFonts
  : TStringList;
begin
  Result := TStringList.Create;
  GetFontList( Result );
end;

class function T_PvlContext.CreateOpenDialog(
  const _parent : TObject;
  const _self   : TGIS_PvlOpenDialog
) : TGIS_PvlOpenDialog;
begin
  Result := TGIS_OpenDialog.Create( _parent, _self );
end;

class function T_PvlContext.CreateSaveDialog(
  const _parent : TObject;
  const _self   : TGIS_PvlSaveDialog
) : TGIS_PvlSaveDialog;
begin
  Result := TGIS_SaveDialog.Create( _parent, _self );
end;

class function T_PvlContext.CreateSelectFolderDialog(
  const _parent : TObject;
  const _self   : TGIS_PvlSelectFolderDialog
) : TGIS_PvlSelectFolderDialog;
begin
  Result := TGIS_SelectFolderDialog.Create( _parent, _self );
end;

class function T_PvlContext.CreateOptionDialog(
  const _parent : TObject;
  const _self   : TGIS_PvlOptionDialog
) : TGIS_PvlOptionDialog;
begin
  Result := TGIS_OptionDialog.Create( _parent, _self );
end;

class function T_PvlContext.CreateInfoDialog(
  const _parent : TObject;
  const _self   : TGIS_PvlInfoDialog
) : TGIS_PvlInfoDialog ;
begin
  Result := TGIS_InfoDialog.Create( _parent, _self ) ;
end;

class function T_PvlContext.CreateWarningDialog(
  const _parent : TObject;
  const _self   : TGIS_PvlWarningDialog
) : TGIS_PvlWarningDialog ;
begin
  Result := TGIS_WarningDialog.Create( _parent, _self ) ;
end;

class function T_PvlContext.CreateErrorDialog(
  const _parent : TObject;
  const _self   : TGIS_PvlErrorDialog
) : TGIS_PvlErrorDialog ;
begin
  Result := TGIS_ErrorDialog.Create( _parent, _self ) ;
end;

class function T_PvlContext.CreateCSDialog(
  const _parent : TObject;
  const _self   : TGIS_PvlControlCSSystem
) : TGIS_PvlControlCSSystem ;
begin
  Result := Lider.CG.GIS.VCL.GeoSystemForms.TGIS_ControlCSSystem.Create( _parent, _self ) ;
end;

class function T_PvlContext.CreateLineSymbologyDialog(
  const _parent : TObject;
  const _self   : TGIS_PvlLineSymbolEditor
) : TGIS_PvlLineSymbolEditor ;
begin
  Result := Lider.CG.GIS.VCL.GeoSystemForms.TGIS_LineSymbolEditor.Create( _parent, _self ) ;
end;

class function T_PvlContext.CreatePrintPreviewSimpleDialog(
  const _parent : TObject;
  const _self   : TGIS_PvlControlPrintPreviewSimple
) : TGIS_PvlControlPrintPreviewSimple ;
begin
  Result := Lider.CG.GIS.VCL.GeoSystemForms.TGIS_ControlPrintPreviewSimple.Create( _parent, _self ) ;
end;

class function T_PvlContext.Support(
  const _parent  : TObject
) : Boolean;
var
  o : TObject;
begin
  if _parent is TGIS_PvlBase then
    o := TGIS_PvlBase( _parent ).NativeControl
  else if _parent is TGIS_PvlContext then
    o := TGIS_PvlContext( _parent ).NativeParent
  else
    o := _parent;

  Result := o is TWinControl ;
end;

function T_PvlContext.CreateObject(
  const _parent : TGIS_PvlBase;
  const _name   : String
) : IGIS_PvlBase;
var
  ctmp : TClass;
  otmp : TGIS_PvlBaseVCL;
begin
  if oControlDict.TryGetValue( _name, ctmp ) then begin
    otmp := TGIS_PvlBaseVCLClass( ctmp ).Create( self.oMaster, _parent );
    otmp.oParent := _parent;

    Result := otmp;
  end
  else
    Assert( False );
end;

function T_PvlContext.ClientWidth
  : Integer;
begin
  Result := RoundS( TWinControl( oParent ).ClientWidth / fget_PPIFix );
end;

function T_PvlContext.fget_HMargin
  : Integer;
begin
  Result := iHMargin ;
end;

procedure T_PvlContext.fset_HMargin(
  const _value: Integer
) ;
begin
  iHMargin := _value ;
end;

function T_PvlContext.fget_VMargin
  : Integer;
begin
  Result := iVMargin ;
end;

procedure T_PvlContext.fset_VMargin(
  const _value: Integer
) ;
begin
  iVMargin := _value ;
end;

function T_PvlContext.fget_HSpace
  : Integer;
begin
  Result := 4 ;
end;

function T_PvlContext.fget_VSpace
  : Integer;
begin
  Result := 4 ;
end;

function T_PvlContext.fget_LSpace
  : Integer;
begin
  Result := 2 ;
end;

function T_PvlContext.fget_CanvasScale
  : Single;
begin
  Result := oMaster.CanvasScale; //?dCanvasScale;
end;

function T_PvlContext.fget_PPIFix
  : Single;
begin
  Result := oMaster.PPIFix;//?dPPIFix;
end;

function T_PvlContext.fget_PPI
  : Integer;
begin
  Result := oMaster.PPI//?iPPI;
end;

{$ENDREGION 'T_PvlContext'}

procedure RegisterPVLPlatformControl(
  const _name : String;
  const  _class : TClass
);
begin
  if not Assigned( oControlDict ) then
    oControlDict := TDictionary< String, TClass>.Create;

  oControlDict.Add( _name, TGIS_PvlBaseVCLClass( _class ) );
end;

procedure ApplyPPI(
  const _form   : TObject;
  const _ppi    : Integer;
  const _ppiFix : Double ;
  const _redraw : Boolean
) ;
var
  i   : Integer;
  cmp : T_component;
  itm : T_PvlControl;
begin
  for i := 0 to TForm(_form).ComponentCount -1 do begin
    if TForm(_form).Components[i] is T_Component then begin
      cmp := T_Component( TForm(_form).Components[i] );
//?      cmp.oOwner.iPPI    := _ppi;
//?      cmp.oOwner.dPPIFix := _ppiFix;

      if _redraw then
        for itm in cmp.lstControls do
          itm.DoRedraw;
    end;
  end;
end;

{$REGION 'Unit_VCL_GisPvl'}

class procedure Unit_VCL_GisPvl.SelfRegisterPVL;
begin
  RegisterPVLPlatformControl( 'Label'               , T_PvlLabel               );
  RegisterPVLPlatformControl( 'IconButton'          , T_PvlIconButton          );
  RegisterPVLPlatformControl( 'Button'              , T_PvlButton              );
  RegisterPVLPlatformControl( 'ModalButton'         , T_PvlModalButton         );
  RegisterPVLPlatformControl( 'Edit'                , T_PvlEdit                );
  RegisterPVLPlatformControl( 'Memo'                , T_PvlMemo                );
  RegisterPVLPlatformControl( 'ListBox'             , T_PvlListBox             );
  RegisterPVLPlatformControl( 'SVGList'             , T_PvlSVGList             );
  RegisterPVLPlatformControl( 'ColorPreview'        , T_PvlColorPreview        );
  RegisterPVLPlatformControl( 'ColorWheel'          , T_PvlColorWheel          );
  RegisterPVLPlatformControl( 'ColorBar'            , T_PvlColorBar            );
  RegisterPVLPlatformControl( 'SizeComboBox'        , T_PvlSizeComboBox        );
  RegisterPVLPlatformControl( 'ColorComboBox'       , T_PvlColorComboBox       );
  RegisterPVLPlatformControl( 'CustomBitmapComboBox', T_PvlCustomBitmapComboBox);
  RegisterPVLPlatformControl( 'ColorRampComboBox'   , T_PvlColorRampComboBox   );
  RegisterPVLPlatformControl( 'ComboBox'            , T_PvlComboBox            );
  RegisterPVLPlatformControl( 'ComboEdit'           , T_PvlComboEdit           );
  RegisterPVLPlatformControl( 'RadioButton'         , T_PvlRadioButton         );
  RegisterPVLPlatformControl( 'CheckBox'            , T_PvlCheckBox            );
  RegisterPVLPlatformControl( 'Panel'               , T_PvlPanel               );
  RegisterPVLPlatformControl( 'PreviewPanel'        , T_PvlPreviewPanel        );
  RegisterPVLPlatformControl( 'GroupBox'            , T_PvlGroupBox            );
  RegisterPVLPlatformControl( 'Tree'                , T_PvlTree                );
  RegisterPVLPlatformControl( 'TreeNode'            , T_PvlTreeNode            );
  RegisterPVLPlatformControl( 'TrackBar'            , T_PvlTrackBar            );
end;

{$ENDREGION 'Unit_VCL_GisPvl'}

procedure clear_oItemCache;
var
  tmp : String;
begin
  for tmp in oItemCache.Keys.ToArray do
    oItemCache.Items[tmp].Free;
end;

initialization
  TGIS_PvlContext.&Register( T_PvlContext );
  oItemCache := TDictionary<String,TGIS_Bitmap>.Create;

  Unit_VCL_GisPvl.SelfRegisterPVL; // call simillar method in every PVL
                                    // platform impoemenation unit

finalization
  clear_oItemCache;
  FreeObject(oItemCache);
  FreeObject(oControlDict);

//==================================== END =====================================
end.

