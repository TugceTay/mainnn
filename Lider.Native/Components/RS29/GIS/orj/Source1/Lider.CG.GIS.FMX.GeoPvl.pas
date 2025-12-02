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
  Implementation of PVL controls for FMX
}

unit Lider.CG.GIS.FMX.GeoPvl;
{$HPPEMIT '#pragma link "Lider.CG.GIS.FMX.GeoPvl"'}

interface

uses
  Lider.CG.GIS.GeoInterfaces,
  Lider.CG.GIS.PVL.GeoPvl;

type
  {#gendoc:hide}
  // Initialization section handler
  Unit_FMX_GisPvl = class
    public
      class procedure SelfRegisterPVL();
  end;

  {#gendoc:hide}
  // register PVL control
  procedure RegisterPVLPlatformControl(
              const _name  : String;
              const _class : TClass
            );

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

type

  {#gendoc:hide}
  TGIS_PvlBaseFmx = class( TGIS_PvlBase, IGIS_PvlBase )
    protected
      oParent  : IGIS_PvlBase;
      oContext : TGIS_PvlContext;
      oControl : TObject;

      function Parent : TGIS_PvlBase ;
    protected
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
  System.Generics.Collections,
  System.SysUtils,
  System.Classes,
  System.UIConsts,
  System.UITypes,
  System.Types,
  System.TypInfo,
  System.Math,
  System.StrUtils,
  System.DateUtils,
  FMX.Forms,
  FMX.Text,
  FMX.BehaviorManager,
  FMX.Styles,
  FMX.Dialogs,
  FMX.Types,
  FMX.Platform,
  FMX.Controls,
  FMX.Controls.Presentation,
  FMX.TreeView,
  FMX.Layouts,
  FMX.Edit,
  FMX.Memo,
  {$IFDEF LEVEL_XE8_FMX}
    FMX.Memo.Types,
  {$ENDIF}
  FMX.ListBox,
  FMX.ListView,
  {$IFDEF LEVEL_RX10_FMX}
    FMX.ListView.Appearances,
  {$ENDIF}
  FMX.ComboEdit,
  FMX.StdCtrls,
  FMX.ExtCtrls,
  FMX.Objects,
  FMX.Graphics,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoInternals,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoCsSystems,
  Lider.CG.GIS.GeoControlPrintPreviewSimple,
  Lider.CG.GIS.GeoPrintManagerAbstract,
  Lider.CG.GIS.GeoLibrarySVG,
  Lider.CG.GIS.GeoParams,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoSymbol,
  Lider.CG.GIS.FMX.GeoFramework,
  Lider.CG.GIS.FMX.GeoViewerBmp,
  Lider.CG.GIS.FMX.GeoComboBox,
  Lider.CG.GIS.FMX.GeoComboBoxHelper,
  Lider.CG.GIS.FMX.GeoPvlForms,
  Lider.CG.GIS.FMX.GeoSystemForms,
  Lider.CG.GIS.FMX.GeoViewerWnd,
  {$IFDEF GIS_MOBILE}
    Lider.CG.GIS.FMX.Mobile.GeoControlCsSystem,
  {$ELSE}
    Lider.CG.GIS.FMX.GeoControlCsSystem,
  {$ENDIF}
  Lider.CG.GIS.FMX.GeoLineSymbolEditor,
  Lider.CG.GIS.FMX.GeoControlColor,
  Lider.CG.GIS.PVL.GeoControlFieldFactor,
  Lider.CG.GIS.PVL.GeoControlSymbology,
  Lider.CG.GIS.PVL.GeoControlBitmap,
  Lider.CG.GIS.PVL.GeoControlSizeForm,
  Lider.CG.GIS.PVL.GeoControlColor ;

  const
    DPI_MANAGER  = 'nibalogreb' ;
  var
    oItemCache : TDictionary<String,TGIS_Bitmap> ; // global bitmap item cache
    oControlDict : TDictionary< String, TClass>;

  // draw checkerboard color
  procedure draw_color(
    _canvas   : TCanvas    ;
    _color    : TGIS_Color ;
    _rect     : TRectF     ;
    _cellsize : Integer    ;
    _enabled  : Boolean
  ) ;
  var
    clr  : TGIS_Color ;
    i    : Integer ;
    imax : Integer ;
    k    : Integer ;
    kmax : Integer ;
    blst : Boolean ;
    bkgm : Double ;
    fggm : Double ;
    r    : Byte ;
    g    : Byte ;
    b    : Byte ;
    rb   : Byte ;
    gb   : Byte ;
    bb   : Byte ;
    rw   : Byte ;
    gw   : Byte ;
    bw   : Byte ;
    bmix : Cardinal ;
    wmix : Cardinal ;
  begin
    if _enabled then begin
      clr := _color ;
      rb := $FF ;
      gb := $FF ;
      bb := $FF ;
      rw := $C0 ;
      gw := $C0 ;
      bw := $C0 ;
    end
    else begin
      clr := TGIS_Color.FromARGB( $00000000 ) ;
      rb := $C0 ;
      gb := $C0 ;
      bb := $C0 ;
      rw := $88 ;
      gw := $88 ;
      bw := $88 ;
    end ;

    imax := RoundS( _rect.Width  / _cellsize ) ;
    kmax := RoundS( _rect.Height / _cellsize ) ;

    fggm := clr.A / 255 ;
    bkgm := 1.0 - fggm ;

    r := clr.R ;
    g := clr.G ;
    b := clr.B ;

    bmix := ( RoundS( bkgm * rb + fggm * r ) shl 16 ) +
            ( RoundS( bkgm * gb + fggm * g ) shl 8  ) +
            ( RoundS( bkgm * bb + fggm * b )        ) ;
    wmix := ( RoundS( bkgm * rw + fggm * r ) shl 16 ) +
            ( RoundS( bkgm * gw + fggm * g ) shl 8  ) +
            ( RoundS( bkgm * bw + fggm * b )        ) ;

    _canvas.Fill.Kind := TBrushKind.Solid ;
    _canvas.Stroke.Thickness := 1 ;

    for k := 0 to kmax do begin

      blst := ( k mod 2 ) = 0 ;

      for i := 0 to imax do begin

        if blst then begin
          _canvas.Stroke.Color := bmix or $FF000000 ;
          _canvas.Fill.Color   := bmix or $FF000000;
        end
        else begin
          _canvas.Stroke.Color := wmix or $FF000000;
          _canvas.Fill.Color   := wmix or $FF000000;
        end ;

        // additional 1 added to Right
        // because of problems with bdRightToLeft mode
        _canvas.FillRect(
           RectF( Min( _rect.Right , _rect.Left + i * _cellsize - 0.5 ),
                  Min( _rect.Bottom, _rect.Top  + k * _cellsize - 0.5 ),
                  Min( _rect.Right ,
                    _rect.Left + ( i + 1 ) * _cellsize + 1 + 1 - 0.5 ),
                  Min( _rect.Bottom,
                    _rect.Top  + ( k + 1 ) * _cellsize + 1 + 1 - 0.5 )
                ),
           0, 0, [], 1
        ) ;

        blst := not blst ;
      end ;

    end ;
  end ;

const
  GIS_COLOR_HUE_SIZE       : Integer = 1530 ;

  GIS_COLOR_WHEEL_MARGIN   : Integer = 14 ;
  GIS_COLOR_WHEEL_MARGIN2  : Integer = 7 ;

  GIS_COLOR_GRADIENT_MARGIN  : Integer = 8 ;
  GIS_COLOR_GRADIENT_MARGIN2 : Integer = 4 ;

type

  TGIS_PvlBaseFmxClass = class of TGIS_PvlBaseFmx;
  T_PvlControl = class;
  T_PvlContext = class;
  T_PvlTreeNode = class;

  T_PvlColorPreviewPanel = class ( TControl )
    public
      Color     : TGIS_Color ;
      CellSize  : Integer ;
      Border   : Boolean ;
    protected
      procedure Paint ; override;
  end;

  T_SVGListItem = class( TLayout )
    private
      oRect     : TRectangle ;
      oIdx      : Integer ;
      oImage    : TImage ;
      oCaption  : TLabel ;
      oName     : String ;
      bSelected : Boolean ;
    public
      constructor Create( _owner: TComponent) ; override;
    private
      procedure fset_Selected( const _value : Boolean ) ;
    public
      property Selected : Boolean read bSelected write fset_Selected ;
  end;

  // component to be added as singleton a proxy to a form to maneger DPI events
  T_dpiManager = class( TComponent )
    public
      constructor Create                ( const _owner      : TComponent ;
                                          const _ppi        : Integer    ;
                                          const _ppifix     : Single     ;
                                          const _canvasscale: Single
                                        );
    private
      oForm   : TForm ;
      iPPI    : Integer ;
      dPPIFix : Single ;
      dScale  : Single ;

      oldPaint: TOnPaintEvent ;
      procedure doPaint   (
                                  _sender  : TObject ;
                                  _canvas  : TCanvas ;
                            const _arect   : TRectF
                          );
  end;

  // component to be addes a proxe to a form to allow automatic disposal
  T_component = class( TComponent )
    public
      constructor Create  (       _owner   : TComponent
                          ); override ;
      destructor  Destroy ;  override ;
    public
      procedure   Add     ( const _control : T_PvlControl
                          ) ;

    private
      oOwner      : T_PvlContext ;
      oMaster     : TGIS_PvlContext;
      lstControls : TObjectList<T_PvlControl> ;
  end;

  T_PvlContext = class( TGIS_PvlContextBase, IGIS_PvlContext )
    protected
      oMaster                           : TGIS_PvlContext;
      oParent                           : TObject;
      iHMargin                          : Integer;
      iVMargin                          : Integer;
      bBiDi                             : Boolean;
      oOwner                            : T_component; // component to be owned by
                                                 // form to control for a proper
                                                 // disposal
    public
      constructor Create                ( const _parent     : TObject;
                                          const _ppi        : Integer;
                                          const _ppifix     : Single;
                                          const _canvasscale: Single;
                                          const _righttoleft: Boolean;
                                          const _context    : TGIS_PvlContext
                                        ) ; override ;
      constructor Create                ( const _parent     : TObject;
                                          const _context    : TGIS_PvlContext
                                        ) ; override ;
      constructor Create                ( const _parent     : TObject;
                                          const _context    : TGIS_PvlContext;
                                          const _refcontext : TGIS_PvlContext
                                        ); override;
    public
      class function CreateOpenDialog   (
                                          const _parent     : TObject;
                                          const _self       : TGIS_PvlOpenDialog
                                        ) : TGIS_PvlOpenDialog;
                                        override;
      class function CreateSaveDialog   (
                                          const _parent     : TObject;
                                          const _self       : TGIS_PvlSaveDialog
                                        ) : TGIS_PvlSaveDialog;
                                        override;
      class function CreateSelectFolderDialog(
                                          const _parent     : TObject;
                                          const _self       : TGIS_PvlSelectFolderDialog
                                        ) : TGIS_PvlSelectFolderDialog;
                                        override;
      class function CreateOptionDialog (
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
      class function Support            ( const _parent     : TObject
                                        ) : Boolean ;
                                        override;
    protected
      function  CreateObject            ( const _parent     : TGIS_PvlBase;
                                          const _name       : String
                                        ) : IGIS_PvlBase;
                                        overload;
    public
      procedure FreeContext ;
    public
      function  GetAllFonts             : TStringList;
      function  ClientWidth             : Integer ;
    protected
      function  fget_HMargin            : Integer;
      procedure fset_HMargin            ( const _value : Integer
                                        ) ;
      function  fget_VMargin            : Integer;
      procedure fset_VMargin            ( const _value : Integer
                                        ) ;
      function  fget_HSpace             : Integer;
      function  fget_VSpace             : Integer;
      function  fget_LSpace             : Integer;
      function  fget_CanvasScale        : Single;
      function  fget_PPIFix             : Single;
      function  fget_PPI                : Integer;
  end ;

  T_PvlControl = class( TGIS_PvlBaseFmx, IGIS_PvlControl )
    public
      constructor Create          ( const _context    : TGIS_PvlContext;
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
    private
      icon_bitmap                 : TBitmap ;
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
                                    var   _keyChar   : WideChar;
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
                                    var   _keyChar    : Char;
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
    private
      vScaleFactor                : Single ;
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

      procedure doOnPaint         (       _sender     : TObject  ;
                                          _canvas     : TCanvas ;
                                    const _rect       : TRectF
                                  );
      procedure doMouseDown       (       _sender     : TObject;
                                          _button     : TMouseButton;
                                          _shift      : TShiftState;
                                          _x          : Single;
                                          _y          : Single
                                  );
      procedure doMouseMove       (       _sender     : TObject;
                                          _shift      : TShiftState;
                                          _x          : Single;
                                          _y          : Single
                                  );
      procedure doMouseUp         (       _sender     : TObject;
                                          _button     : TMouseButton;
                                          _shift      : TShiftState;
                                          _x          : Single;
                                          _y          : Single
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
    private
      vScaleFactor                : Single ;
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

      procedure doOnPaint         (       _sender     : TObject  ;
                                          _canvas     : TCanvas ;
                                    const _rect       : TRectF
                                  );
      procedure doMouseDown       (       _sender     : TObject;
                                          _button     : TMouseButton;
                                          _shift      : TShiftState;
                                          _x          : Single;
                                          _y          : Single
                                  );
      procedure doMouseMove       (      _sender      : TObject;
                                         _shift       : TShiftState;
                                         _x           : Single;
                                         _y           : Single
                                  );
      procedure doMouseUp         (      _sender      : TObject;
                                         _button      : TMouseButton;
                                         _shift       : TShiftState;
                                         _x           : Single;
                                         _y           : Single
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
      constructor Create          ( const _context    : TGIS_PvlContext;
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

      /// <summary>
      ///   Extension to the FMX comboboxes.
      /// </summary>
      comboExt                    : TGIS_ComboBoxHelper ;

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

      /// <inheritdoc from="IGIS_PvlCustomComboBox"/>
      procedure fset_Value        ( const _value      : String
                                  );

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
    private
      function  doValueSize       (       _sender    : TObject      ;
                                          _value     : String
                                  ) : String ;
      function  doCustomSize      (       _sender    : TObject      ;
                                          _value     : String
                                  ) : String ;
      procedure doRenderSize      (       _item      : TListBoxItem ;
                                          _canvas    : TCanvas      ;
                                          _rect      : TRectF       ;
                                          _font      : TFont        ;
                                          _color     : TAlphaColor  ;
                                          _class     : Char         ;
                                          _caption   : String       ;
                                          _value     : String
                                  ) ;
      procedure DelayedUpdate     ( const _val       : String
                                  ) ;
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
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
    private
      function  doValueColor      (       _sender    : TObject      ;
                                          _value     : String
                                  ) : String ;
      function  doCustomColor     (       _sender    : TObject ;
                                          _value     : String
                                  ) : String ;
      procedure doRenderColor     (       _item      : TListBoxItem ;
                                          _canvas    : TCanvas      ;
                                          _rect      : TRectF       ;
                                          _font      : TFont        ;
                                          _color     : TAlphaColor  ;
                                          _class     : Char         ;
                                          _caption   : String       ;
                                          _value     : String
                                  ) ;
      procedure DelayedUpdate     ( const _val : String
                                  ) ;
      function  convert_color     ( const _col      : String
                                  ) : TGIS_Color ;
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
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
    private
      function  doValueBrush      (       _sender    : TObject      ;
                                          _value     : String
                                  ) : String ;
      function  doCustomBrush     (       _sender    : TObject ;
                                          _value     : String
                                  ) : String ;
      procedure doRenderBrush     (       _item      : TListBoxItem ;
                                          _canvas    : TCanvas      ;
                                          _rect      : TRectF       ;
                                          _font      : TFont        ;
                                          _color     : TAlphaColor  ;
                                          _class     : Char         ;
                                          _caption   : String       ;
                                          _value     : String
                                  ) ;
      procedure DelayedUpdate     ( const _val : String
                                  ) ;
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

    protected

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
      comboExt                    : TGIS_ComboBoxHelper ;
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
      procedure gradHorizontal    ( const _canvas    : TCanvas ;
                                    const _rect      : TRect ;
                                    const _fromColor : TColor ;
                                    const _toColor   : TColor ;
                                    const _mode      : TGIS_ColorMapMode
                                  );
      function  doValueBitmap     (       _sender    : TObject      ;
                                          _value     : String
                                  ) : String ;
      procedure doRenderBitmap    (       _item      : TListBoxItem ;
                                          _canvas    : TCanvas      ;
                                          _rect      : TRectF       ;
                                          _font      : TFont        ;
                                          _color     : TAlphaColor  ;
                                          _class     : Char         ;
                                          _caption   : String       ;
                                          _value     : String
                                  ) ;
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
    private
      bSorted                     : Boolean ;
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

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  );

    public
      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure BeginUpdate;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure EndUpdate;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure ItemsClear;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure ItemsAdd          ( const _item       : String
                                  );

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  IndexOf           ( const _item       : String
                                  ) : Integer;

      procedure SetFocus;         override;
  end;

  T_PvlComboEdit = class ( T_PvlControl, IGIS_PvlComboEdit )
    private
      lstItems                    : TGIS_ListOfStrings ;
      bSorted                     : Boolean ;
      bFilteredSearch             : Boolean ;
      bUpDown                     : Boolean ;
      bLeftRight                  : Boolean ;
      iSel                        : Integer ;
      bReturn                     : Boolean ;
    protected
      /// <inheritdoc"/>
      procedure doCreate          ( const _context    : TGIS_PvlContext
                                  );
                                  override;
      /// <inheritdoc"/>
      procedure doDestroy;        override;

      /// <inheritdoc from="IGIS_PvlComboEdit"/>
      function  fget_FilteredSearch
                                  : Boolean;

      /// <inheritdoc from="IGIS_PvlComboEdit"/>
      procedure fset_FilteredSearch
                                  ( const _value      : Boolean
                                  );

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
      procedure doOnKeyUp         (       _sender     : TObject;
                                    var   _key        : Word;
                                    var   _keychar    : WideChar;
                                          _shift      : TShiftState
                                  );
      procedure doOnKeyDown       (       _sender     : TObject;
                                    var   _key        : Word;
                                    var   _keychar    : WideChar;
                                          _shift      : TShiftState
                                  );
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

      /// <inheritdoc from="IGIS_PvlEdit"/>
      function  fget_OnChange     : TGIS_PvlEvent;

      /// <inheritdoc from="IGIS_PvlEdit"/>
      procedure fset_OnChange     ( const _value      : TGIS_PvlEvent
                                  );

    public
      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure BeginUpdate;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure EndUpdate;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure ItemsClear;

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      procedure ItemsAdd          ( const _item       : String
                                  );

      /// <inheritdoc from="IGIS_PvlComboBox"/>
      function  IndexOf           ( const _item       : String
                                  ) : Integer;

      procedure SetFocus;         override;
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
    private
      oPanel                      : TVertScrollBox ;
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
      function  fget_Tick         : Integer;

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      procedure fset_Tick         ( const _value      : Integer
                                  );

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      function  fget_DrawTicks    : Boolean;

      /// <inheritdoc from="IGIS_PvlTrackBar"/>
      procedure fset_DrawTicks    ( const _value      : Boolean
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

  T_TreeViewItemEx = class( TTreeViewItem )
    public
      Node : TGIS_PvlTreeNode ;
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
      procedure doOnSelectChange  (       _sender     : TObject
                                  );
      procedure doOnDeletion      (       _sender     : TObject;
                                          _node       : T_TreeViewItemEx
                                  );
    public
      procedure SetFocus;         override;
  end;

  T_PvlTreeNode = class ( TGIS_PvlBaseFmx, IGIS_PvlTreeNode )
    public
      constructor Create          ( const _context    : TGIS_PvlContext;
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
      function  fget_NativeControl  : TObject; override;
      procedure fset_NativeControl  ( const _value        : TObject
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
      lstItm   : TGIS_ListOfStrings ;
      lstSel   : TGIS_ListOfStrings ;
      itmCount : Integer            ;
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
      oLib          : TGIS_SymbolLibrarySVG ;
      iCount        : Integer ;
      iIdx          : Integer ;
      row           : Integer ;
      col           : Integer ;
      itmList       : TList<T_SVGListItem> ;
      cForeground   : TGIS_Color ;
      cBackground   : TGIS_Color ;

    private
      procedure applyStyle;
      procedure doOnClick         (       _sender     : TObject
                                  );
      procedure doOnMouseDown     (       _sender    : TObject ;
                                          _button    : TMouseButton ;
                                          _shift     : TShiftState ;
                                          _x         : Single ;
                                          _y         : Single
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
  SVG_SIZE = 64 ;

{$REGION 'utils'}

procedure get_style_colors(
  const _context    : TGIS_PvlContext ;
  const _control    : TObject    ;
  var   _foreground : TGIS_Color ;
  var   _background : TGIS_Color
) ;
var
  isok    : Boolean     ;
  oparent : TCustomForm ;
  otmp    : TFmxObject  ;
  obj     : TFmxObject  ;
  ctrl    : TControl    ;
  comp    : TControl    ;
  bmp     : TBitmap     ;
  bmpdat  : TBitmapData ;

  function doStyleObject(
    const _style_name : String
  ) : TControl ;
  var
    o : TFmxObject ;
  begin
    {$IFNDEF LEVEL_RX10_FMX}
      if Assigned(TStyleManager.ActiveStyleForScene(oparent)) then
        o := TStyleManager.ActiveStyleForScene(
               oparent
             ).FindStyleResource(_style_name);
    {$ELSE}
      o:= TStyledControl.LookupStyleObject(
            TFmxObject( _control ), TFmxObject( _control ),
            oparent, _style_name, '', '', True
          );
    {$ENDIF}

    if o is TControl then
      Result := TControl( o )
    else
     Result := nil ;
  end;

begin
  isok := False ;

  try
    oparent := nil ;
    otmp := TFmxObject( _context.NativeParent ) ;
    while Assigned( otmp ) do begin
      if otmp is TCustomForm then
        break ;
      otmp := otmp.Parent ;
    end;
    oparent := TCustomForm(otmp) ;

    if Assigned( oparent ) then
      isok := True ;
    if not isok then
      exit ;

    isok := False ;
    ctrl := doStyleObject( 'listboxstyle' ) ;
    try
      if Assigned( ctrl ) then begin
        obj := ctrl.FindStyleResource('background') ;

        if obj is TControl then begin
          comp := TControl( obj );
          comp.Width  := 8 ;
          comp.Height := 8 ;
          bmp := TBitmap.Create( Round(comp.Width) , Round(comp.Height) ) ;
          try
            bmp.Canvas.BeginScene();
            comp.PaintTo( bmp.Canvas, RectF( 0, 0, bmp.Width, bmp.Height ) ) ;
            bmp.Canvas.EndScene();
            bmp.Map( TMapAccess.Read, bmpdat );
            try
              _background := GISColor( bmpdat.GetPixel(3, 3) );
            finally
              bmp.Unmap( bmpdat);
            end;
            isok := True ;
          finally
            bmp.Free ;
          end;
        end;
      end;
    finally
      FreeObject( ctrl ) ;
    end ;
    if not isok then
      exit ;

    isok := False ;
    ctrl := doStyleObject( 'labelstyle' ) ;
    try
      if Assigned( ctrl ) then begin
        obj := ctrl.FindStyleResource('text') ;
        if obj is TText then begin
          _foreground := GISColor( TText(obj).TextSettings.FontColor ) ;
          isok := True ;
        end;
      end;
    finally
      FreeObject( ctrl ) ;
    end ;
    if not isok then
      exit ;


  finally
    if not isok then begin
      _background := TGIS_Color.White   ;
      _foreground := TGIS_Color.DimGray ;
    end ;
  end;
end ;

{$ENDREGION 'utils'}

{$REGION 'TGIS_PvlBaseFmx'}

constructor TGIS_PvlBaseFmx.Create(
  const _context : TGIS_PvlContext;
  const _parent  : IGIS_PvlBase
);
begin
  oContext := _context ;
  oParent  := _parent ;
end;

function TGIS_PvlBaseFmx.Parent
  : TGIS_PvlBase ;
begin
  Result := oParent as TGIS_PvlBase ;
end;

function TGIS_PvlBaseFmx.fget_NativeControl: TObject ;
begin
  Result := oControl ;
end;

{$ENDREGION 'TGIS_PvlBaseFmx'}

{$REGION 'T_PvlColorPreviewPanel'}

procedure T_PvlColorPreviewPanel.Paint ;
begin

  if Border and ( Color = TGIS_Color.None ) then begin
    Border := False ;
    exit ;
  end;

  if Color = TGIS_Color.None then begin
    draw_color( Canvas, Color, RectF( 0.5, 0.5, Width - 0.5, Height - 0.5 ), CellSize, Enabled ) ;
    exit ;
  end;

  draw_color( Canvas, Color, RectF( 0.5, 0.5, Width - 0.5, Height - 0.5 ), CellSize, Enabled ) ;

//  Border
  Canvas.BeginScene ;

  draw_color( Canvas,
              Color,
              RectF( 0.5, 0.5, Width - 0.5, Height - 0.5 ),
              CellSize,
              Enabled
            ) ;

  Canvas.Stroke.Kind := TBrushKind.Solid ;
  if Enabled then
    Canvas.Stroke.Color := $FF7A7A7A
  else
    Canvas.Stroke.Color :=  $FF888888 ;
  Canvas.Fill.Kind := TBrushKind.None ;

  Canvas.DrawRect( RectF( 0.5, 0.5, Width - 0.5, Height - 0.5 ), 0, 0, [], 1 ) ;
  Canvas.EndScene ;
end;

{$ENDREGION 'T_PvlColorPreviewPanel'}

{$REGION 'T_SVGListItem'}

constructor T_SVGListItem.Create( _owner: TComponent) ;
begin
  inherited Create( _owner );

  Width          := 110 ;
  Padding.Left   := 5 ;
  Padding.Top    := 5 ;
  Padding.Bottom := 5 ;
  Padding.Right  := 5 ;

  oRect := TRectangle.Create( Self );
  oRect.Parent := Self ;

  oImage := TImage.Create( Self );
  oImage.Parent := Self ;
  oImage.Width  := SVG_SIZE ;
  oImage.Height := SVG_SIZE ;
  oImage.Align := TAlignLayout.Top ;
  oImage.HitTest := False ;

  oCaption := TLabel.Create( Self );
  oCaption.Parent := Self ;
  oCaption.TextSettings.HorzAlign := TTextAlign.Center ;
  oCaption.TextSettings.VertAlign := TTextAlign.Leading ;
  oCaption.Align := TAlignLayout.Bottom ;
  oCaption.HitTest := False ;
  oCaption.StyledSettings := oCaption.StyledSettings - [TStyledSetting.Size] ;
  oCaption.Font.Size := 11 ;
  oCaption.Height := 18 ;

  Height := oImage.Height + oCaption.Height + 10 ;

  // oRect continuation
  oRect.Width := Width - 2 ;
  oRect.Height := Height - 2 ;
  oRect.Position.X := 1 ;
  oRect.Position.Y := 1 ;
  oRect.Stroke.Thickness := 0 ;
  oRect.Fill.Color := $FFCDE8FF ;
  oRect.Visible := False ;
  oRect.HitTest := False ;

  HitTest := True ;
end;

procedure T_SVGListItem.fset_Selected( const _value : Boolean ) ;
begin
  if _value = bSelected then Exit ;

  oRect.Visible := _value ;
  bSelected := _value ;
end;
{$ENDREGION 'T_SVGListItem'}

{$REGION 'T_dpiManager'}

constructor T_dpiManager.Create(
  const _owner       : TComponent ;
  const _ppi         : Integer    ;
  const _ppifix      : Single     ;
  const _canvasscale : Single
) ;
begin
  inherited Create( _owner );

  oForm := TForm( _owner ) ;
  iPPI    := _ppi ;
  dPPIFix := _ppifix ;
  dScale  := _canvasscale ;

  Name := DPI_MANAGER ;

  oldPaint := oForm.OnPaint ;
  oForm.OnPaint := doPaint ;
end;

procedure T_dpiManager.doPaint(
        _sender : TObject ;
        _canvas : TCanvas ;
  const _arect  : TRectF
);
var
  i   : Integer ;
  cmp : T_component ;
  itm : T_PvlControl ;
begin
  if dScale <> _canvas.Scale then begin
    dScale := _canvas.Scale ;

    for i := 0 to oForm.ComponentCount -1 do begin
      if oForm.Components[i] is T_Component then begin
        cmp := T_Component( oForm.Components[i] ) ;
//?        cmp.oOwner.iPPI    := iPPI ;
//?        cmp.oOwner.dCanvasScale := dScale ;

        for itm in cmp.lstControls do
          itm.DoRedraw ;
      end;
    end;
  end;

  if Assigned( oldPaint ) then
     oldPaint( _sender, _canvas, _arect ) ;
end;

{$ENDREGION 'T_dpiManager'}

{$REGION 'T_component'}

constructor T_component.Create(
  _owner : TComponent
) ;
begin
  inherited ;
  lstControls := TObjectList<T_PvlControl>.Create ;
end;

destructor T_component.Destroy ;
begin
  FreeObject( lstControls ) ;
  FreeObject( oOwner ) ;
  FreeObject( oMaster );
  inherited ;
end ;

procedure T_component.Add(
  const _control : T_PvlControl
) ;
begin
  lstControls.Add( _control ) ;
end;

{$ENDREGION 'T_component'}

{$REGION 'T_PvlControl'}
constructor T_PvlControl.Create(
  const _context : TGIS_PvlContext;
  const _parent  : IGIS_PvlBase
);
begin
  inherited Create( _context, _parent );

  doCreate( _context );

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
  TControl( _control ).Parent := TFmxObject( oControl );
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
    TAlignLayout.None   : Result := TGIS_PvlAlign.None   ;
    TAlignLayout.Client : Result := TGIS_PvlAlign.Client ;
    TAlignLayout.Top    : Result := TGIS_PvlAlign.Top    ;
    TAlignLayout.Left   : Result := TGIS_PvlAlign.Left   ;
    TAlignLayout.Right  : Result := TGIS_PvlAlign.Right  ;
    TAlignLayout.Bottom : Result := TGIS_PvlAlign.Bottom ;
    else                  begin
                            fset_Align( TGIS_PvlAlign.None ) ;
                            Result := TGIS_PvlAlign.None   ;
                          end ;
  end;
end;

procedure T_PvlControl.fset_Align(
  const _value : TGIS_PvlAlign
);
begin
  case _value of
    TGIS_PvlAlign.None    : TControl( oControl ).Align := TAlignLayout.None   ;
    TGIS_PvlAlign.Client  : TControl( oControl ).Align := TAlignLayout.Client ;
    TGIS_PvlAlign.Top     : TControl( oControl ).Align := TAlignLayout.Top    ;
    TGIS_PvlAlign.Left    : TControl( oControl ).Align := TAlignLayout.Left   ;
    TGIS_PvlAlign.Bottom  : TControl( oControl ).Align := TAlignLayout.Bottom ;
    TGIS_PvlAlign.Right   : TControl( oControl ).Align := TAlignLayout.Right  ;
    else                    begin
                              assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                            end ;
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
  Result := Round( TControl( oControl ).Position.X / oContext.PPIFix );
end;

procedure T_PvlControl.fset_Left(
  const _value : Integer
);
begin
  TControl( oControl ).Position.X := Round( _value * oContext.PPIFix );
end;

function T_PvlControl.fget_Top
  : Integer;
begin
  Result := Round( TControl( oControl ).Position.Y / oContext.PPIFix );
end;

procedure T_PvlControl.fset_Top(
  const _value : Integer
);
begin
  TControl( oControl ).Position.Y := Round( _value * oContext.PPIFix );
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
  {$IFNDEF LEVEL_RX101_FMX}
    Result := '' ;
  {$ELSE}
    Result := TControl( oControl ).Hint ;
  {$ENDIF}
end;

procedure T_PvlControl.fset_Hint(
  const _value : String
);
begin
  {$IFNDEF LEVEL_RX101_FMX}
    // not supported
  {$ELSE}
    TControl( oControl ).Hint := _value ;
  {$ENDIF}
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
  oControl := TLabel.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );
  TLabel( oControl ).AutoSize := False ;

  {$IFDEF IOS}
    TLabel( oControl ).TextSettings.WordWrap := False ;
    TLabel( oControl ).Scale.X := 0.8 ;
    TLabel( oControl ).Scale.Y := 0.8 ;
  {$ENDIF}
  {$IFDEF ANDROID}
    TLabel( oControl ).TextSettings.WordWrap := False ;
    TLabel( oControl ).TextSettings.Trimming := TTextTrimming.None ;
  {$ENDIF}

  oFocusControl := nil ;
end;

function T_PvlLabel.fget_Caption
  : String;
begin
  Result := TLabel( oControl ).Text;
end;

procedure T_PvlLabel.fset_Caption(
  const _value : String
);
begin
  TLabel( oControl ).Text := _value.Replace( '&', '' ) ;
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

  {$IFDEF LEVEL_RX101_FMX}
    TLabel( oControl ).FocusControl := TControl( oFocusControl.NativeControl ) ;
  {$ENDIF}
end;

function T_PvlLabel.fget_FontSize
  : Integer;
begin
  Result := RoundS( TLabel( oControl ).Font.Size );
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
  Result := TLabel( oControl ).Font.Family;
end;

procedure T_PvlLabel.fset_FontFamily(
  const _value : string
);
begin
  TLabel( oControl ).Font.Family := _value;
end;

function T_PvlLabel.fget_Alignment
  : TGIS_PvlLabelTextAlignment;
begin
  Result := TGIS_PvlLabelTextAlignment.Left ;

  if TLabel( oControl ).TextAlign = TTextAlign.Center then
    Result := TGIS_PvlLabelTextAlignment.Center
  else if TLabel( oControl ).TextAlign = TTextAlign.Leading then
    Result := TGIS_PvlLabelTextAlignment.Left
  else if TLabel( oControl ).TextAlign = TTextAlign.Trailing then
    Result := TGIS_PvlLabelTextAlignment.Right ;
end;

procedure T_PvlLabel.fset_Alignment(
  const _value : TGIS_PvlLabelTextAlignment
);
begin
  if _value = TGIS_PvlLabelTextAlignment.Left then
    TLabel( oControl ).TextAlign := TTextAlign.Leading
  else if _value = TGIS_PvlLabelTextAlignment.Center then
    TLabel( oControl ).TextAlign := TTextAlign.Center
  else if _value = TGIS_PvlLabelTextAlignment.Right then
    TLabel( oControl ).TextAlign := TTextAlign.Trailing ;
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
  oControl := TSpeedButton.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );

  TSpeedButton( oControl ).OnClick := doOnClick ;
end;

procedure T_PvlIconButton.DoRedraw;
var
  tmp : TGIS_Bitmap ;
  obj : TFmxObject  ;
  bmp : TBitmap ;
  smb : TGIS_SymbolAbstract ;
  clr : TGIS_Color ;

  procedure addResBmp ;
  var
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
    try
      if oControl is TSpeedButton then
        btn := TSpeedButton.Create( nil )
      else
        btn := TButton.Create( nil ) ;

      btn.Visible := False;
      btn.Parent := TFmxObject( oContext.Parent ) ;
      btn.ApplyStyleLookup ;

      obj := btn.FindStyleResource( 'text' ) ;
      if obj is TText then
        clr := TText(obj).TextSettings.FontColor
      else
        clr := TAlphaColorRec.Null ;
    finally
      FreeObject( btn ) ;
    end;

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

    img := TImage( TFmxObject( oControl ).FindComponent( 'img') ) ;

    if not Assigned( img ) then begin
      img := TImage.Create( TFmxObject( oControl ) ) ;
      img.Parent := TFmxObject( oControl ) ;
      img.Name := 'img' ;
        img.Align := TAlignLayout.Center ;
    end
    else
      img.Bitmap.Assign( bmp );
    img.HitTest := False ;
    img.Width := bmp.Width  / oContext.CanvasScale ;
    img.Height:= bmp.Height / oContext.CanvasScale ;
    img.WrapMode := TImageWrapMode.Fit ;
  end ;

begin
  if not Assigned( TGIS_PvlIconButton( oParent ).IconsList ) then
    exit ;

  smb := TGIS_PvlIconButton( oParent ).IconsList.Icon[ TGIS_PvlIconButton( oParent ).IconIndex ] ;

  if not Assigned( smb ) then
    exit ;

  tmp := TGIS_Bitmap.Create(
          RoundS( TGIS_PvlIconButton( oParent ).IconSize * oContext.CanvasScale ),
          RoundS( TGIS_PvlIconButton( oParent ).IconSize * oContext.CanvasScale )
         ) ;
  try
    TSpeedButton( oControl ).ApplyStyleLookup ;
    obj := TButton( oControl ).FindStyleResource('text') ;
    if obj is TText then
      clr := GISColor( TText(obj).TextSettings.FontColor )
    else begin
      if TGIS_PvlIconButton( oParent ).Enabled then
        clr := TGIS_Color.Black
      else
        clr := TGIS_Color.Silver;
    end;

    tmp.DrawGlyph( smb, oContext.PPI, clr, TGIS_PvlIconButton( oParent ).Enabled  ) ;

    bmp := TBitmap( tmp.NativeBitmap ) ;

  addResBmp ;
  icon_bitmap := bmp ;
  finally
    FreeObject( tmp ) ;
  end;
end;

procedure T_PvlIconButton.doOnClick(
  _sender : TObject
);
begin
  if not TGIS_PvlIconButton( oParent ).StayPressed then
    TSpeedButton( oControl ).IsPressed := False;

  if Assigned( FOnClick ) then
    FOnClick( oContext.Parent );
end;

function T_PvlIconButton.fget_Pushed
  : Boolean;
begin
  Result := TSpeedButton( oControl ).IsPressed;
end;

procedure T_PvlIconButton.fset_Pushed(
  const _value : Boolean
);
begin
  TSpeedButton( oControl ).IsPressed := _value;
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
  oControl := TButton.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );

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
  Result := TButton( oControl ).Text;
end;

procedure T_PvlButton.fset_Caption(
  const _value : String
);
begin
  TButton( oControl ).Text := _value;
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
  oControl := TButton.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );

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
  Result := TButton( oControl ).Text;
end;

procedure T_PvlModalButton.fset_Caption(
  const _value : String
);
begin
  TButton( oControl ).Text := _value;
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
  TButton( oControl ).Parent := TFmxObject( _value );
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
  oControl := TEdit.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );

  TEdit( oControl ).StyledSettings := [TStyledSetting.Family, TStyledSetting.Size, TStyledSetting.Style] ;

  TEdit( oControl ).OnClick           := doOnClick    ;
  TEdit( oControl ).OnKeyDown         := doOnKeyDown  ;
  TEdit( oControl ).OnChangeTracking  := doOnChange   ;
  TEdit( oControl ).OnTyping          := doOnChange   ;
  TEdit( oControl ).OnKeyDown         := doOnKeyDown  ;
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
      _sender  : TObject;
  var _key     : Word;
  var _keyChar : Char;
      _shift   : TShiftState
) ;
begin
  if Assigned( FOnKeyDown ) then
    FOnKeyDown( Parent, _key );
  doOnKeyPress( _sender, _keyChar ) ;
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
  TEdit( oControl ).FontColor := TAlphaColors.Red ;
end;

procedure T_PvlEdit.SetFontDefault;
begin
  TEdit( oControl ).FontColor := TAlphaColors.Black ;
end;

{$ENDREGION 'T_PvlEdit'}

{$REGION 'T_PvlMemo'}

procedure T_PvlMemo.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TMemo.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );

  TMemo( oControl ).StyledSettings := [TStyledSetting.Family, TStyledSetting.Size, TStyledSetting.Style] ;
  TMemo( oControl ).ShowScrollBars := True ;
  TMemo( oControl ).WordWrap := True ;

  TMemo( oControl ).OnClick           := doOnClick    ;
  TMemo( oControl ).OnKeyDown         := doOnKeyDown  ;
  TMemo( oControl ).OnChangeTracking  := doOnChange   ;
  TMemo( oControl ).OnKeyDown         := doOnKeyDown  ;
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
   var _keyChar: Char;
       _shift  : TShiftState
);
begin
  if Assigned( FOnKeyDown ) then
    FOnKeyDown( Parent, _key );
  doOnKeyPress( _sender, _keyChar ) ;
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
  Result := TPoint.Create( TMemo( oControl ).CaretPosition.Pos, TMemo( oControl ).CaretPosition.Line );
end;

procedure T_PvlMemo.fset_CursorPos(
  const _value : TPoint
);
begin
  TMemo( oControl ).CaretPosition := TCaretPosition.Create( _value.y, _value.x ) ;
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
  TEdit( oControl ).FontColor := TALphaColors.Red;
end;

procedure T_PvlMemo.SetFontDefault;
begin
  TEdit( oControl ).FontColor := TAlphaColors.Black;
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
  oControl := T_PvlColorPreviewPanel.Create( TFmxObject( _context.NativeParent ) );

  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );


  T_PvlColorPreviewPanel( oControl ).CellSize := 8 ;
  T_PvlColorPreviewPanel( oControl ).Color := TGIS_Color.Black ;
  T_PvlColorPreviewPanel( oControl ).OnClick := doOnClick ;
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
  oControl := TControl.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );

  vScaleFactor := oContext.CanvasScale ;

  currX := RoundS(  fget_Width  / 2 );
  currY := RoundS(  fget_Height / 2 );

  prepareHue;

  bInit := False;
  bMouseDown := False;

  TControl( oControl ).OnPaint      := doOnPaint;
  TControl( oControl ).OnMouseDown  := doMouseDown;
  TControl( oControl ).OnMouseMove  := doMouseMove;
  TControl( oControl ).OnMouseUp    := doMouseUp;
end;

procedure T_PvlColorWheel.doDestroy;
begin
  FreeObject( colorWheel );
  FreeObject( currState );

  inherited;
end;

procedure T_PvlColorWheel.makeColorWheel;
var
  w2  : Integer ;
  h2  : Integer ;
  x   : Integer ;
  y   : Integer ;
  r   : Double ;
  a   : Double ;
  hue : TGIS_PvlRGBVal ;
  i   : Integer ;
  k   : Integer ;
  bmpd: TBitmapData ;
  ptr : Pointer ;
begin
  colorWheel := TBitmap.Create ;
  colorWheel.SetSize(
    RoundS( vScaleFactor*fget_Width  ),
    RoundS( vScaleFactor*fget_Height )
  ) ;
  colorWheel.Clear( 0 ) ;

  colorWheel.Canvas.BeginScene ;

  w2 := RoundS( ( vScaleFactor*fget_Width  - GIS_COLOR_WHEEL_MARGIN )/2.0 ) ;
  h2 := RoundS( ( vScaleFactor*fget_Height - GIS_COLOR_WHEEL_MARGIN )/2.0 ) ;

  colorWheel.Map( TMapAccess.Write, bmpd ) ;
  for i := GIS_COLOR_WHEEL_MARGIN2
      to colorWheel.Height - GIS_COLOR_WHEEL_MARGIN2 - 1 do begin

    ptr := bmpd.GetScanline(i) ;
    y := h2 - i + GIS_COLOR_WHEEL_MARGIN2 ;

    for k := GIS_COLOR_WHEEL_MARGIN2
        to colorWheel.Width - GIS_COLOR_WHEEL_MARGIN2 - 1 do begin

      x := k - w2 - GIS_COLOR_WHEEL_MARGIN2 ;

      r := Sqrt( x*x + y*y )/w2 ;
      if r > 1.0 then
        continue ;

      a := ArcTan2( y, x ) ;

      hue := angleToRGB( a ) ;

      {$IFNDEF ANDROID}
      PCardinal( NativeInt(ptr) + 4*k)^ := Cardinal(
               255 shl 24 +
              (255 - RoundS( r*hue.R )) shl 16 +
              (255 - RoundS( r*hue.G )) shl 8 +
              (255 - RoundS( r*hue.B )) shl 0 ) ;
      {$ELSE}
      PCardinal( NativeInt(ptr) + 4*k)^ := Cardinal(
               255 shl 24 +
              (255 - RoundS( r*hue.B )) shl 16 +
              (255 - RoundS( r*hue.G )) shl 8 +
              (255 - RoundS( r*hue.R )) shl 0 ) ;
      {$ENDIF}
    end ;

  end ;
  colorWheel.UnMap( bmpd ) ;

  colorWheel.Canvas.EndScene ;

  currState := TBitmap.Create ;
  currState.SetSize(
    RoundS( vScaleFactor*fget_Width  ),
    RoundS( vScaleFactor*fget_Height )
  ) ;
  currState.Clear( 0 ) ;

  bInit := True ;
end;

procedure T_PvlColorWheel.drawCrosshair;
var
  path : TPathData ;
  scl  : Single ;
  cx   : Integer ;
  cy   : Integer ;
  c1, c2, c3, c6 ,c7 : Single ;
begin
  currState.Canvas.BeginScene ;
  currState.Canvas.Clear(0);
  currState.Canvas.DrawBitmap(
    colorWheel,
    RectF(0, 0, colorWheel.Width, colorWheel.Height),
    RectF(0, 0, colorWheel.Width, colorWheel.Height),
    1
  ) ;

  currState.Canvas.Stroke.Kind := TBrushKind.Solid ;
  currState.Canvas.Stroke.Thickness := vScaleFactor ;

  path := TPathData.Create();

  cx := currX ;
  cy := currY ;

  currX := RoundS( vScaleFactor * currX ) ;
  currY := RoundS( vScaleFactor * currY ) ;

  c1 := vScaleFactor*1.0 ;
  c2 := vScaleFactor*2.0 ;
  c3 := vScaleFactor*3.0 ;
  c6 := vScaleFactor*6.0 ;
  c7 := vScaleFactor*7.0 ;

  currState.Canvas.Stroke.Color := $FF000000 ;
  path.MoveTo( PointF( currX - c1 - 0.5, currY - c2 - 0.5 ) ) ;
  path.LineTo( PointF( currX - c1 - 0.5, currY - c7 - 0.5 ) ) ;
  path.MoveTo( PointF( currX      - 0.5, currY - c2 - 0.5 ) ) ;
  path.LineTo( PointF( currX      - 0.5, currY - c7 - 0.5 ) ) ;
  currState.Canvas.DrawPath( path, 1 ) ;
  path.Clear ;
  currState.Canvas.Stroke.Color := $FFFFFFFF  ;
  path.MoveTo( PointF( currX      - 0.5, currY - c3 - 0.5 ) ) ;
  path.LineTo( PointF( currX      - 0.5, currY - c6 - 0.5 ) ) ;
  currState.Canvas.DrawPath( path, 1 ) ;
  path.Clear ;
  currState.Canvas.Stroke.Color := $FF000000  ;
  path.MoveTo( PointF( currX + c1 - 0.5, currY - c2 - 0.5 ) ) ;
  path.LineTo( PointF( currX + c1 - 0.5, currY - c7 - 0.5 ) ) ;
  currState.Canvas.DrawPath( path, 1 ) ;

  path.Clear ;
  currState.Canvas.Stroke.Color := ( $FF000000 ) ;
  path.MoveTo( PointF( currX - c1 - 0.5, currY + c2 - 0.5 ) ) ;
  path.LineTo( PointF( currX - c1 - 0.5, currY + c7 - 0.5 ) ) ;
  path.MoveTo( PointF( currX      - 0.5, currY + c2 - 0.5 ) ) ;
  path.LineTo( PointF( currX      - 0.5, currY + c7 - 0.5 ) ) ;
  currState.Canvas.DrawPath( path, 1 ) ;
  path.Clear ;
  currState.Canvas.Stroke.Color := ( $FFFFFFFF ) ;
  path.MoveTo( PointF( currX      - 0.5, currY + c3 - 0.5 ) ) ;
  path.LineTo( PointF( currX      - 0.5, currY + c6 - 0.5 ) ) ;
  currState.Canvas.DrawPath( path, 1 ) ;
  path.Clear ;
  currState.Canvas.Stroke.Color := ( $FF000000 ) ;
  path.MoveTo( PointF( currX + c1 - 0.5, currY + c2 - 0.5 ) ) ;
  path.LineTo( PointF( currX + c1 - 0.5, currY + c7 - 0.5 ) ) ;
  currState.Canvas.DrawPath( path, 1 ) ;

  path.Clear ;
  currState.Canvas.Stroke.Color := ( $FF000000 ) ;
  path.MoveTo( PointF( currX - c2 - 0.5, currY - c1 - 0.5 ) ) ;
  path.LineTo( PointF( currX - c7 - 0.5, currY - c1 - 0.5 ) ) ;
  path.MoveTo( PointF( currX - c2 - 0.5, currY      - 0.5 ) ) ;
  path.LineTo( PointF( currX - c7 - 0.5, currY      - 0.5 ) ) ;
  currState.Canvas.DrawPath( path, 1 ) ;
  path.Clear ;
  currState.Canvas.Stroke.Color := ( $FFFFFFFF ) ;
  path.MoveTo( PointF( currX - c3 - 0.5, currY      - 0.5 ) ) ;
  path.LineTo( PointF( currX - c6 - 0.5, currY      - 0.5 ) ) ;
  currState.Canvas.DrawPath( path, 1 ) ;
  path.Clear ;
  currState.Canvas.Stroke.Color := ( $FF000000 ) ;
  path.MoveTo( PointF( currX - c2 - 0.5, currY + c1 - 0.5 ) ) ;
  path.LineTo( PointF( currX - c7 - 0.5, currY + c1 - 0.5 ) ) ;
  currState.Canvas.DrawPath( path, 1 ) ;

  path.Clear ;
  currState.Canvas.Stroke.Color := ( $FF000000 ) ;
  path.MoveTo( PointF( currX + c2 - 0.5, currY - c1 - 0.5 ) ) ;
  path.LineTo( PointF( currX + c7 - 0.5, currY - c1 - 0.5 ) ) ;
  path.MoveTo( PointF( currX + c2 - 0.5, currY      - 0.5 ) ) ;
  path.LineTo( PointF( currX + c7 - 0.5, currY      - 0.5 ) ) ;
  currState.Canvas.DrawPath( path, 1 ) ;
  path.Clear ;
  currState.Canvas.Stroke.Color := ( $FFFFFFFF ) ;
  path.MoveTo( PointF( currX + c3 - 0.5, currY      - 0.5 ) ) ;
  path.LineTo( PointF( currX + c6 - 0.5, currY      - 0.5 ) ) ;
  currState.Canvas.DrawPath( path, 1 ) ;
  path.Clear ;
  currState.Canvas.Stroke.Color := ( $FF000000 ) ;
  path.MoveTo( PointF( currX + c2 - 0.5, currY + c1 - 0.5 ) ) ;
  path.LineTo( PointF( currX + c7 - 0.5, currY + c1 - 0.5 ) ) ;
  currState.Canvas.DrawPath( path, 1 ) ;
  FreeObject( path ) ;

  currX := cx ;
  currY := cy ;

  currState.Canvas.EndScene ;
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
        _sender  : TObject ;
        _canvas  : TCanvas ;
  const _rect    : TRectF
);
begin
  oContext.Refresh ;

  if ( not bInit ) or ( oContext.PPIFix <> oldPPI ) then begin
    oldPPI := oContext.PPI;
    makeColorWheel;
  end;

  drawCrosshair ;

  TControl( oControl ).Canvas.DrawBitmap(
    currState,
    RectF(0, 0, currState.Width, currState.Height ),
    RectF(0, 0, currState.Width/vScaleFactor, currState.Height/vScaleFactor ),
    1
  ) ;
end;

procedure T_PvlColorWheel.doMouseDown(
  _sender : TObject;
  _button : TMouseButton;
  _shift  : TShiftState;
  _x      : Single;
  _y      : Single
);
begin
  if not checkMouse( RoundS( _x ), RoundS( _y ) ) then
    exit ;

  currX := RoundS( _x ) ;
  currY := RoundS( _y ) ;

  TControl( oControl ).Repaint ;

  bMouseDown := True ;
end;

procedure T_PvlColorWheel.doMouseMove(
  _sender : TObject;
  _shift  : TShiftState;
  _x      : Single;
  _y      : Single
);
begin
  if not bMouseDown then
    exit ;

  if not checkMouse( RoundS( _x ), RoundS( _y ) ) then
    exit ;

  currX := RoundS( _x  ) ;
  currY := RoundS( _y  ) ;

  if assigned( FOnChange ) then
    FOnChange( Self ) ;

  TControl( oControl ).Repaint ;
end;


procedure T_PvlColorWheel.doMouseUp(
  _sender : TObject;
  _button : TMouseButton;
  _shift  : TShiftState;
  _x      : Single;
  _y      : Single
);
begin
  if not bMouseDown then
    exit ;

  if not checkMouse( RoundS( _x ), RoundS( _y ) ) then begin
    bMouseDown := False ;
    exit ;
  end ;

  currX := RoundS( _x ) ;
  currY := RoundS( _y ) ;

  TControl( oControl ).Repaint ;

  bMouseDown := False ;

  if assigned( FOnChange ) then
    FOnChange( Self ) ;
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
  w2 := ( fget_Width - GIS_COLOR_WHEEL_MARGIN )/2;

  currX := RoundS( x*w2 + fget_Width/2 );
  currY := RoundS( fget_Height/2 - y*w2 );

  TControl( oControl ).repaint;
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
  TControl( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlColorWheel'}

{$REGION 'T_PvlColorBar'}

procedure T_PvlColorBar.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TControl.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );

  vScaleFactor := oContext.CanvasScale ;

  bInit := False;
  bMouseDown := False;

  FColor := TGIS_Color.White;
  FAlpha := False;

  TControl( oControl ).OnPaint      := doOnPaint;
  TControl( oControl ).OnMouseDown  := doMouseDown;
  TControl( oControl ).OnMouseMove  := doMouseMove;
  TControl( oControl ).OnMouseUp    := doMouseUp;
end;

procedure T_PvlColorBar.doDestroy;
begin
  FreeObject( gradient );
  FreeObject( currState );

  inherited;
end;

procedure T_PvlColorBar.makeGradient;
begin
  FreeObject( gradient ) ;

  gradient := TBitmap.Create ;
  gradient.SetSize(
    RoundS( vScaleFactor*fget_Width  ),
    RoundS( vScaleFactor*fget_Height )
  ) ;
  gradient.Clear( 0 ) ;
  gradient.Canvas.BeginScene ;
  gradient.Canvas.Stroke.Color := TAlphaColorRec.Black ;
  gradient.Canvas.Stroke.Kind := TBrushKind.Solid ;
  gradient.Canvas.Fill.Color := 0 ;
  gradient.Canvas.Fill.Kind := TBrushKind.Solid ;
  gradient.Canvas.FillRect(
    RectF( 0, 0, vScaleFactor*fget_Width, vScaleFactor*fget_Height ), 0, 0, [], 1
  ) ;

  if fget_Alpha then
    makeAlpha
  else
    makeColor ;

  gradient.Canvas.Stroke.Color := $FF7A7A7A  ;
  gradient.Canvas.Fill.Kind := TBrushKind.None ;
  gradient.Canvas.Stroke.Thickness := vScaleFactor ;
  gradient.Canvas.DrawRect(
    RectF(
      GIS_COLOR_GRADIENT_MARGIN2 - 0.5 ,
      GIS_COLOR_GRADIENT_MARGIN2 - 0.5,
      vScaleFactor*fget_Width  - GIS_COLOR_GRADIENT_MARGIN2 + 0.5,
      vScaleFactor*fget_Height - GIS_COLOR_GRADIENT_MARGIN2 + 0.5
    ),
    0, 0, [], 1
  ) ;

  gradient.Canvas.EndScene ;

  FreeObject( currState ) ;

  currState := TBitmap.Create ;
  currState.SetSize(
    RoundS( vScaleFactor*fget_Width  ),
    RoundS( vScaleFactor*fget_Height )
  ) ;
  currState.Clear( 0 ) ;

  bInit := True ;
end;

procedure T_PvlColorBar.makeColor;
var
  bmpd: TBitmapData ;
  ptr : PByteArray ;
  i   : Integer ;
  k   : Integer ;
begin
  inherited ;

  gradient.Map( TMapAccess.Write, bmpd ) ;

  for i := GIS_COLOR_GRADIENT_MARGIN2
    to RoundS( vScaleFactor*fget_Height - GIS_COLOR_GRADIENT_MARGIN2 - 1) do begin

    ptr := bmpd.GetScanline(i) ;

    for k := GIS_COLOR_GRADIENT_MARGIN2
      to RoundS( vScaleFactor*fget_Width - GIS_COLOR_GRADIENT_MARGIN2 - 1) do begin

      {$IFNDEF ANDROID}
        ptr[4*k  ] :=
          RoundS(
            (
              ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
              ( vScaleFactor*fget_Width - GIS_COLOR_GRADIENT_MARGIN )
            ) * FColor.B
          ) ;
        ptr[4*k+1] :=
          RoundS(
            (
              ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
              ( vScaleFactor*fget_Width - GIS_COLOR_GRADIENT_MARGIN )
            ) * FColor.G
          ) ;
        ptr[4*k+2] :=
          RoundS(
            (
              ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
              ( vScaleFactor*fget_Width - GIS_COLOR_GRADIENT_MARGIN )
            ) * FColor.R
          ) ;
        ptr[4*k+3  ] := 255 ;
      {$ELSE}
        ptr[4*k  ] :=
          RoundS(
            (
              ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
              ( vScaleFactor*fget_Width - GIS_COLOR_GRADIENT_MARGIN )
            ) * FColor.R
          ) ;
        ptr[4*k+1] :=
          RoundS(
            (
              ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
              ( vScaleFactor*fget_Width - GIS_COLOR_GRADIENT_MARGIN )
            ) * FColor.G
          ) ;
        ptr[4*k+2] :=
          RoundS(
            (
              ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
              ( vScaleFactor*fget_Width - GIS_COLOR_GRADIENT_MARGIN )
            ) * FColor.B
          ) ;
        ptr[4*k+3  ] := 255 ;

      {$ENDIF}

    end ;

  end ;

  gradient.Unmap(bmpd);
end;

procedure T_PvlColorBar.makeAlpha;
var
  bmpd: TBitmapData ;
  ptr : PByteArray ;
  mul : Double ;
  i   : Integer ;
  k   : Integer ;
  rb  : Boolean ;
  cb  : Boolean ;

  r   : Byte ;
  g   : Byte ;
  b   : Byte ;
begin
  inherited ;

  r := FColor.R ;
  g := FColor.G ;
  b := FColor.B ;

  cb := False ;
  gradient.Map( TMapAccess.Write, bmpd ) ;

  for i := GIS_COLOR_GRADIENT_MARGIN2
    to RoundS( vScaleFactor*fget_Height - GIS_COLOR_GRADIENT_MARGIN2 - 1 ) do begin

    if ( ( i - GIS_COLOR_GRADIENT_MARGIN2 ) mod RoundS( vScaleFactor*8 )  ) = 0 then
      cb := not cb ;
    rb := cb ;

    ptr := bmpd.GetScanline(i) ;

    for k := GIS_COLOR_GRADIENT_MARGIN2
      to RoundS( vScaleFactor*fget_Width - GIS_COLOR_GRADIENT_MARGIN2 - 1 ) do begin

      if ( ( k - GIS_COLOR_GRADIENT_MARGIN2 ) mod RoundS( vScaleFactor*8 ) ) = 0 then
        rb := not rb ;

      mul := ( k - GIS_COLOR_GRADIENT_MARGIN2 )/
             ( vScaleFactor*fget_Width - GIS_COLOR_GRADIENT_MARGIN ) ;

      if rb then begin
        {$IFNDEF ANDROID}
          ptr[4*k  ] := RoundS( mul * b + ( 1 - mul ) * $C0 ) ;
          ptr[4*k+1] := RoundS( mul * g + ( 1 - mul ) * $C0 ) ;
          ptr[4*k+2] := RoundS( mul * r + ( 1 - mul ) * $C0 ) ;
          ptr[4*k+3] := 255 ;
        {$ELSE}
          ptr[4*k  ] := RoundS( mul * r + ( 1 - mul ) * $C0 ) ;
          ptr[4*k+1] := RoundS( mul * g + ( 1 - mul ) * $C0 ) ;
          ptr[4*k+2] := RoundS( mul * b + ( 1 - mul ) * $C0 ) ;
          ptr[4*k+3] := 255 ;
        {$ENDIF}
      end
      else begin
        {$IFNDEF ANDROID}
          ptr[4*k  ] := RoundS( mul * b + ( 1 - mul ) * $FF ) ;
          ptr[4*k+1] := RoundS( mul * g + ( 1 - mul ) * $FF ) ;
          ptr[4*k+2] := RoundS( mul * r + ( 1 - mul ) * $FF ) ;
          ptr[4*k+3] := 255 ;
        {$ELSE}
          ptr[4*k  ] := RoundS( mul * r + ( 1 - mul ) * $FF ) ;
          ptr[4*k+1] := RoundS( mul * g + ( 1 - mul ) * $FF ) ;
          ptr[4*k+2] := RoundS( mul * b + ( 1 - mul ) * $FF ) ;
          ptr[4*k+3] := 255 ;
        {$ENDIF}
      end ;

    end ;

  end ;
  gradient.Unmap(bmpd);
end;

procedure T_PvlColorBar.drawArrows ;
var
  cx   : Single ;
begin
  currState.Canvas.BeginScene ;
  currState.Canvas.Clear(0);
  currState.Canvas.DrawBitmap( gradient,
    RectF(0, 0, gradient.Width, gradient.Height),
    RectF(0, 0, gradient.Width, gradient.Height),
    1
  ) ;

  cx := currX ;
  currX := RoundS( vScaleFactor*currX ) ;

  currState.Canvas.Stroke.Color := TAlphaColorRec.Black ;
  currState.Canvas.Stroke.Thickness := vScaleFactor ;
  currState.Canvas.Fill.Color := TAlphaColorRec.White ;
  currState.Canvas.Fill.Kind := TBrushKind.Solid ;
  currState.Canvas.FillRect(
    RectF( currX - vScaleFactor*2.5, 0, currX + vScaleFactor*2.5, vScaleFactor*fget_Height ),
    2, 2, [TCorner.TopLeft, TCorner.TopRight,
      TCorner.BottomLeft, TCorner.BottomRight ], 1
  ) ;
  currState.Canvas.DrawRect(
    RectF( currX - vScaleFactor*2.5, 0, currX + vScaleFactor*2.5, vScaleFactor*fget_Height ),
    2, 2, [TCorner.TopLeft, TCorner.TopRight,
      TCorner.BottomLeft, TCorner.BottomRight ], 1
  ) ;

  currX := RoundS( cx ) ;

  currState.Canvas.EndScene ;
end ;

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
  _x      : Single;
  _y      : Single
);
begin
  if not checkMouse( RoundS( _x ), RoundS( _y ) ) then
    exit ;

  currX := RoundS( _x ) ;

  FValue := ( currX - GIS_COLOR_GRADIENT_MARGIN2 )/
            ( fget_Width - GIS_COLOR_GRADIENT_MARGIN - 1 ) ;

  if Assigned( FOnChange ) then
    FOnChange( Self ) ;

  bMouseDown := True ;
end;


procedure T_PvlColorBar.doMouseMove(
  _sender : TObject;
  _shift  : TShiftState;
  _x      : Single;
  _y      : Single
);
begin
  if not bMouseDown then
    exit ;

  if not checkMouse( RoundS( _x ), RoundS( _y ) ) then
    exit ;

  currX := RoundS( _x ) ;

  FValue := ( currX - GIS_COLOR_GRADIENT_MARGIN2 )/
            ( fget_Width - GIS_COLOR_GRADIENT_MARGIN - 1 ) ;

  TControl( oControl ).Repaint ;

  if Assigned( FOnChange ) then
    FOnChange( Self );
end;

procedure T_PvlColorBar.doMouseUp(
  _sender : TObject;
  _button : TMouseButton;
  _shift  : TShiftState;
  _x      : Single;
  _y      : Single
);
begin
  if not bMouseDown then
    exit ;

  if not checkMouse( RoundS( _x ), RoundS( _y ) ) then begin
    bMouseDown := False ;
    exit ;
  end ;

  currX := RoundS( _x ) ;

  FValue := ( currX - GIS_COLOR_GRADIENT_MARGIN2 )/
            ( fget_Width - GIS_COLOR_GRADIENT_MARGIN - 1 ) ;

  TControl( oControl ).Repaint ;

  if Assigned( FOnChange ) then
    FOnChange( Self ) ;

  bMouseDown := False ;
end;

procedure T_PvlColorBar.doOnPaint(
        _sender : TObject  ;
        _canvas : TCanvas ;
  const _rect   : TRectF
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

  _canvas.BeginScene ;
  _canvas.DrawBitmap( currState,
    RectF(0, 0, currState.Width, currState.Height),
    RectF(0, 0, currState.Width/vScaleFactor, currState.Height/vScaleFactor),
    1
  ) ;
  _canvas.EndScene ;
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

 TControl( oControl ).Repaint;
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

  TControl( oControl ).Repaint;
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
  TControl( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlColorBar'}

{$REGION 'T_PvlCustomComboBox'}

constructor T_PvlCustomComboBox.Create(
  const _context : TGIS_PvlContext;
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
  FreeObject( comboExt );

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
  Result := comboExt.Value ;
end;

procedure T_PvlCustomComboBox.fset_Value(
  const _value : String
);
begin
  comboExt.Value := _value ;
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
  oControl := TGIS_ComboBox.Create( TFmxObject( _context.NativeParent ) );

  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );

  comboExt := TGIS_ComboBoxHelper.Create( TGIS_ComboBox( oControl ), TGIS_ComboBox( oControl ).Name + '_Color', 3 ) ;
  comboExt.ValueEvent  := doValueSize ;
  comboExt.RenderEvent := doRenderSize ;
  comboExt.CustomEvent := doCustomSize ;
  comboExt.SetOnChange( doOnChange ) ;
end;

procedure T_PvlSizeComboBox.Fill(
  const _forSymbol  : Boolean;
  const _forLine    : Boolean;
  const _field      : Boolean;
  const _renderer   : Boolean
);
begin
  TGIS_ComboBox( oControl ).Items.Clear;

  comboExt.BeginFill ;

  if _forSymbol then begin
    comboExt.SetItem(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '8 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '8pt',
          ''
        )
      )
    );

    comboExt.SetItem(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '10 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '10pt',
          ''
        )
      )
    );

    comboExt.SetItem(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '12 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '12pt',
          ''
        )
      )
    );

    comboExt.SetItem(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '16 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '16pt',
          ''
        )
      )
    );

    comboExt.SetItem(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '24 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '24pt',
          ''
        )
      )
    );

    comboExt.SetItem(
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
    comboExt.SetItem(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, 'HAIR',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          'HAIR',
          ''
        )
      )
    );
    comboExt.SetItem(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '1 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '1pt',
          ''
        )
      )
    );
    comboExt.SetItem(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '2 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '2pt',
          ''
        )
      )
    );
    comboExt.SetItem(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '4 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '4pt',
          ''
        )
      )
    );
    comboExt.SetItem(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '6 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '6pt',
          ''
        )
      )
    );
    comboExt.SetItem(
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
    comboExt.SetItem(
      PrepareComboBoxItem(
        False, 's', TGIS_PvlComboBoxHelperPosition.Top, '0 pt',
        ConstructParamAsText(
          GIS_PARAMTXT_TYPE_SIZE,
          '0pt',
          ''
        )
      )
    );

    if TGIS_ComboBox( oControl ).Items.Count = 1 then begin
      comboExt.SetItem(
        PrepareComboBoxItem(
          False, 's', TGIS_PvlComboBoxHelperPosition.Top, '1 pt',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_SIZE,
            '1pt',
            ''
          )
        )
      );
      comboExt.SetItem(
        PrepareComboBoxItem(
          False, 's', TGIS_PvlComboBoxHelperPosition.Top, '2 pt',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_SIZE,
            '2pt',
            ''
          )
        )
      );
      comboExt.SetItem(
        PrepareComboBoxItem(
          False, 's', TGIS_PvlComboBoxHelperPosition.Top, '3 pt',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_SIZE,
            '3pt',
            ''
          )
        )
      );
      comboExt.SetItem(
        PrepareComboBoxItem(
          False, 's', TGIS_PvlComboBoxHelperPosition.Top, '4 pt',
          ConstructParamAsText(
            GIS_PARAMTXT_TYPE_SIZE,
            '4pt',
            ''
          )
        )
      );
      comboExt.SetItem(
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
    comboExt.SetItem(
      PrepareComboBoxItem(
        False, 'r', TGIS_PvlComboBoxHelperPosition.Bottom,
        GIS_RS_GENERAL_BYRENDERER, GIS_PARAMTXT_TYPE_RENDERER
      )
    );
    inc( iCustomFieldsCount );
  end;

    comboExt.SetItem(
      PrepareComboBoxItem(
      True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
      GIS_RS_GENERAL_CUSTOM + '...', GIS_PARAMTXT_TYPE_CUSTOM
    )
  );
    inc( iCustomFieldsCount );

  if _field then begin
    comboExt.SetItem(
      PrepareComboBoxItem(
        True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
        GIS_RS_GENERAL_BYFIELD + '...', GIS_PARAMTXT_TYPE_FIELD
      )
    );
    inc( iCustomFieldsCount );
  end;

  comboExt.EndFill ;
  TGIS_ComboBox( oControl ).ItemIndex := 0;
end;

procedure T_PvlSizeComboBox.FillRealWorldUnits(
  const _field : Boolean
);
begin
  TGIS_ComboBox( oControl ).Items.Clear;

  comboExt.BeginFill ;

  comboExt.SetItem(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top, '0 m',
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_SIZE,
        '0m',
        ''
      )
    )
  );

  comboExt.SetItem(
    PrepareComboBoxItem(
      True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
      GIS_RS_GENERAL_CUSTOM + '...', GIS_PARAMTXT_TYPE_CUSTOM
    )
  );
  inc( iCustomFieldsCount );

  if _field then begin
    comboExt.SetItem(
      PrepareComboBoxItem(
        True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
        GIS_RS_GENERAL_BYFIELD + '...', GIS_PARAMTXT_TYPE_FIELD
      )
    );
    inc( iCustomFieldsCount );
  end;

  comboExt.EndFill ;
  TGIS_ComboBox( oControl ).ItemIndex := 0;
end;

procedure T_PvlSizeComboBox.FillAggregation;
begin
  TGIS_ComboBox( oControl ).Items.Clear;

  comboExt.BeginFill ;

  comboExt.SetItem(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top, '20 pt',
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_SIZE,
        '20 pt',
        ''
      )
    )
  );
  comboExt.SetItem(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top, '40 pt',
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_SIZE,
        '40 pt',
        ''
      )
    )
  );
  comboExt.SetItem(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top, '60 pt',
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_SIZE,
        '60 pt',
        ''
      )
    )
  );
  comboExt.SetItem(
    PrepareComboBoxItem(
      False, 's', TGIS_PvlComboBoxHelperPosition.Top, '80 pt',
      ConstructParamAsText(
        GIS_PARAMTXT_TYPE_SIZE,
        '80 pt',
        ''
      )
    )
  );

  comboExt.SetItem(
    PrepareComboBoxItem(
      True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
      GIS_RS_GENERAL_CUSTOM + '...', GIS_PARAMTXT_TYPE_CUSTOM
    )
  );
  inc( iCustomFieldsCount );

  comboExt.EndFill ;
  TGIS_ComboBox( oControl ).ItemIndex := 0;
end;

procedure T_PvlSizeComboBox.fset_Height(
  const _value : Integer
);
begin
  inherited;
  if _value > 0 then
    TGIS_ComboBox( oControl ).ItemHeight := _value;
end;

function T_PvlSizeComboBox.doValueSize(
  _sender : TObject ;
  _value  : String
) : String ;
var
  s1, s2, s3 : String ;
begin
  try
    SplitParamAsText( _value, s1, s2, s3 );
    if s1 = GIS_PARAMTXT_TYPE_SIZE then
      Result := TGIS_ComboBoxHelper.PrepareItem(
                  False ,
                  's',
                  TGIS_ComboBoxHelperPosition.Lru,
                  _value,
                  _value
                )
    else
    if s1 = GIS_PARAMTXT_TYPE_RENDERER then
      Result := TGIS_ComboBoxHelper.PrepareItem(
                  False ,
                  'r',
                  TGIS_ComboBoxHelperPosition.Top,
                  _value,
                  _value
                )
    else
    if s1 = GIS_PARAMTXT_TYPE_FIELD then
      Result := TGIS_ComboBoxHelper.PrepareItem(
                  False ,
                  'f',
                  TGIS_ComboBoxHelperPosition.Lru,
                  _value,
                  _value
                )
    else
      Result := '' ;
  except
    Result := '' ;
  end;
end;


function T_PvlSizeComboBox.doCustomSize(
  _sender : TObject ;
  _value  : String
) : String ;
var
  res : String ;
  s1, s2, s3 : String ;
  dlg  : TGIS_ControlSizeForm ;
  frm  : TGIS_ControlFieldFactor ;
  val  : String ;
  proc : TGIS_Proc ;
begin

  Result := '' ;

  if _value = GIS_PARAMTXT_TYPE_FIELD then begin
    frm := TGIS_ControlFieldFactor.Create( oContext.NativeParent ) ;
    if ( not assigned( FFields ) ) or ( FFields.Count = 0 ) then
      raise EGIS_Exception.create( _rsrcna( GIS_RS_ERR_FIELDFACTOR_NOFIELDS ), '', 0 ) ;
    frm.FillFields( FFields ) ;
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
        DelayedUpdate( val ) ;
      end ;

    frm.Execute(
      proc
    );
    res := val ;
  end
  else
  if _value = GIS_PARAMTXT_TYPE_CUSTOM then begin
    dlg := TGIS_ControlSizeForm.Create( oContext.NativeParent ) ;
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
        DelayedUpdate( val ) ;
      end ;

    dlg.Execute( proc ) ;
    res := val ;
  end ;

  SplitParamAsText( res, s1, s2, s3 ) ;

  if s1 = GIS_PARAMTXT_TYPE_SIZE then
    Result := TGIS_ComboBoxHelper.PrepareItem(
                False ,
                's',
                TGIS_ComboBoxHelperPosition.Lru,
                res,
                res
              )
  else
  if s1 = GIS_PARAMTXT_TYPE_FIELD then
    Result := TGIS_ComboBoxHelper.PrepareItem(
                False ,
                'f',
                TGIS_ComboBoxHelperPosition.Lru,
                res,
                res
              )
  else
    Result := '' ;
end ;

procedure T_PvlSizeComboBox.DelayedUpdate(
  const _val : String
) ;
var
  val : String ;
  s1, s2, s3 : String ;
begin
  if IsStringEmpty( _val ) then
    exit ;

  SplitParamAsText( _val, s1, s2, s3 ) ;

  if s1 = GIS_PARAMTXT_TYPE_SIZE then
    val := TGIS_ComboBoxHelper.PrepareItem(
             False ,
             's',
             TGIS_ComboBoxHelperPosition.Lru,
             _val,
             _val
           )
  else
  if s1 = GIS_PARAMTXT_TYPE_FIELD then
    val := TGIS_ComboBoxHelper.PrepareItem(
             False ,
             'f',
             TGIS_ComboBoxHelperPosition.Lru,
             _val,
             _val
           ) ;

  comboExt.SetItem( val, True ) ;
end ;

procedure T_PvlSizeComboBox.doRenderSize(
  _item      : TListBoxItem ;
  _canvas    : TCanvas      ;
  _rect      : TRectF       ;
  _font      : TFont        ;
  _color     : TAlphaColor  ;
  _class     : Char         ;
  _caption   : String       ;
  _value     : String
);
var
  s1, s2, s3 : String ;
  str : String ;
begin
  str := '' ;

  SplitParamAsText( _value, s1, s2, s3  );

  case _class of
    'f' : begin
            str := s2 + ' * ' + s3;
          end ;
    'r' : begin
            str := _caption ;
          end ;
    'C' : begin
            str := _caption ;
          end ;
    else begin
      if not IsStringEmpty( s2) then
        str  := s2
      else
        str  := s1
    end;
  end;

  _canvas.Font.Assign( _font ) ;
  _canvas.Fill.Color := _color ;
  _canvas.FillText( _rect, str, False,
                    _item.AbsoluteOpacity, [], TTextAlign.Leading
                  );
end ;

procedure T_PvlSizeComboBox.SetFocus;
begin
  TGIS_ComboBox( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlSizeComboBox'}

{$REGION 'T_PvlColorComboBox'}

procedure T_PvlColorComboBox.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TGIS_ComboBox.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );

  comboExt := TGIS_ComboBoxHelper.Create( TGIS_ComboBox( oControl ), TGIS_ComboBox( oControl ).Name + '_Color', 3 ) ;
  comboExt.ValueEvent  := doValueColor ;
  comboExt.RenderEvent := doRenderColor ;
  comboExt.CustomEvent := doCustomColor ;
  comboExt.SetOnChange( doOnChange ) ;
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
    comboExt.SetItem(
      PrepareComboBoxItem(
        False, 'c', TGIS_PvlComboBoxHelperPosition.Top,
        _value, ConstructParamAsText( GIS_PARAMTXT_TYPE_ARGB, _value, '' )
      )
    );
  end;

begin

  for i := 0 to _colors.Count - 1 do begin
    addItem( _colors.Strings[i] );
  end;

  if _renderer then begin
    comboExt.SetItem(
      PrepareComboBoxItem(
        False, 'r', TGIS_PvlComboBoxHelperPosition.Bottom,
        GIS_RS_GENERAL_BYRENDERER, GIS_PARAMTXT_TYPE_RENDERER
      )
    );
    inc( iCustomFieldsCount );
  end;

  comboExt.SetItem(
    PrepareComboBoxItem(
      True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
      GIS_RS_GENERAL_CUSTOM + '...', GIS_PARAMTXT_TYPE_CUSTOM
    )
  );
  inc( iCustomFieldsCount );

  if _field then begin
    comboExt.SetItem(
      PrepareComboBoxItem(
        True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
        GIS_RS_GENERAL_BYFIELD + '...', GIS_PARAMTXT_TYPE_FIELD
      )
    );
    inc( iCustomFieldsCount );
  end;
  TGIS_ComboBox( oControl ).ItemIndex := 0;
end;

function T_PvlColorComboBox.doValueColor(
  _sender : TObject ;
  _value  : String
) : String ;
var
  s1, s2, s3 : String ;
begin
  try
    SplitParamAsText( _value, s1, s2, s3 );

    if s1 = GIS_PARAMTXT_TYPE_RENDERER then
      Result := TGIS_ComboBoxHelper.PrepareItem(
                  False ,
                  'r',
                  TGIS_ComboBoxHelperPosition.Top,
                  s2,
                  _value
                )
    else
    if s1 = GIS_PARAMTXT_TYPE_ARGB then
      Result := TGIS_ComboBoxHelper.PrepareItem(
                  False ,
                  'c',
                  TGIS_ComboBoxHelperPosition.Top,
                  s2,
                  _value
                )
    else
    if s1 = GIS_PARAMTXT_TYPE_FIELD then
      Result := TGIS_ComboBoxHelper.PrepareItem(
                  False ,
                  'f',
                  TGIS_ComboBoxHelperPosition.Lru,
                  s2,
                  _value
                ) ;
  except
    Result := '' ;
  end;
end ;


function T_PvlColorComboBox.doCustomColor(
  _sender : TObject ;
  _value  : String
) : String ;
var
  res : String ;
  s1, s2, s3 : String ;
  dlg : TGIS_ControlColor ;
  frm : TGIS_ControlFieldFactor ;
  val : String ;
  clr : TGIS_Color ;
  proc : TGIS_Proc ;
begin

  res := '' ;

  if _value = GIS_PARAMTXT_TYPE_FIELD then begin
    if ( not assigned( FFields ) ) or ( FFields.Count = 0 ) then
      raise EGIS_Exception.create( _rsrc( GIS_RS_ERR_FIELDFACTOR_NOFIELDS ), '', 0 ) ;
    frm := TGIS_ControlFieldFactor.Create( TComponent( oContext.Parent ) ) ;
    frm.FillFields( fget_Fields ) ;
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
          DelayedUpdate( val ) ;
        end;
      frm.Execute( proc ) ;
      res := val ;
  end
  else
  if _value = GIS_PARAMTXT_TYPE_CUSTOM then begin

    clr := convert_color( fget_Value ) ;

    dlg := TGIS_ControlColor.Create( TComponent( oContext.Parent ) ) ;
    proc := procedure( _modal_result : TGIS_PvlModalResult )
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit ;

        val := ConstructParamAsText(
                 GIS_PARAMTXT_TYPE_ARGB,
                 IntToHex( dlg.Color.ARGB, 2 ),
                 ''
               ) ;
        DelayedUpdate( val ) ;
      end;
    dlg.Execute(
      clr,
      proc
    ) ;
    res := val ;
  end ;

  SplitParamAsText( res, s1, s2, s3 );

  if s1 = GIS_PARAMTXT_TYPE_ARGB then
    Result := TGIS_ComboBoxHelper.PrepareItem(
                False ,
                'c',
                TGIS_ComboBoxHelperPosition.Lru,
                s2,
                res
              )
  else
  if s1 = GIS_PARAMTXT_TYPE_FIELD then
    Result := TGIS_ComboBoxHelper.PrepareItem(
                False ,
                'f',
                TGIS_ComboBoxHelperPosition.Lru,
                s2,
                res
              )
  else
    Result := '' ;
end ;

//
procedure T_PvlColorComboBox.DelayedUpdate(
  const _val : String
) ;
var
  val : String ;
  s1, s2, s3 : String ;
begin
  if IsStringEmpty( _val ) then
    exit ;

  SplitParamAsText( _val, s1, s2, s3 );

  if s1 = GIS_PARAMTXT_TYPE_ARGB then
    val := TGIS_ComboBoxHelper.PrepareItem(
             False ,
             'c',
             TGIS_ComboBoxHelperPosition.Lru,
             s2,
             _val
           )
  else
  if s1 = GIS_PARAMTXT_TYPE_FIELD then
    val := TGIS_ComboBoxHelper.PrepareItem(
             False ,
             'f',
             TGIS_ComboBoxHelperPosition.Lru,
             s2,
             _val
           ) ;

  comboExt.SetItem( val, True ) ;
end ;

function T_PvlColorComboBox.convert_color(
  const _col: string
) : TGIS_Color ;
var
  s1, s2, s3 : String ;
begin
  SplitParamAsText( _col, s1, s2, s3 );

  Assert( s1 = GIS_PARAMTXT_TYPE_ARGB ) ;

  Result := TGIS_Color.FromARGB( StrToUInt64( '$'+s2 ) ) ;
end ;

procedure T_PvlColorComboBox.doRenderColor(
  _item      : TListBoxItem ;
  _canvas    : TCanvas      ;
  _rect      : TRectF       ;
  _font      : TFont        ;
  _color     : TAlphaColor  ;
  _class     : Char         ;
  _caption   : String       ;
  _value     : String
);
var
  r  : TRectF ;
  r1 : TRectF ;
  str : String ;
  s1, s2, s3 : String ;
begin
  str := '' ;

  r := _rect ;

  case _class of
    'c' : begin
            r1 := _rect ;
            r1.Width  := r1.Height ;
            r1.Left   := r1.Left   + 1.5  ;
            r1.Top    := r1.Top    + 1.5  ;
            r1.Bottom := r1.Bottom - 1.5  ;
            r1.Right  := r1.Right  - 1.5  ;

            _canvas.Fill.Kind := TBrushKind.Solid ;
            _canvas.Fill.Color := TAlphaColorRec.White ;

            _canvas.FillRect( r1, 0, 0, [], _item.AbsoluteOpacity ) ;

            if fget_Enabled then
              draw_color( _canvas, convert_color(_value), r1, 4, True )
            else
              draw_color( _canvas, TGIS_Color.Gray, r1, 4, True ) ;


            _canvas.Stroke.Color := _color ;
            _canvas.Stroke.Kind := TBrushKind.Solid ;
            _canvas.Stroke.Thickness := 1 ;
            _canvas.DrawRect( r1, 0, 0, [], _item.AbsoluteOpacity ) ;

            r.Left := r1.Right + r1.Left ;
            str := _caption;
          end ;
    'f' : begin
            SplitParamAsText(_caption, s1, s2, s3 ) ;
            if IsStringEmpty(s2) then
              s2 := s1 ;
            str  := s2 ;
          end ;
    else  begin
            str  := _caption ;
          end ;
  end;

  _canvas.Font.Assign( _font ) ;
  _canvas.Fill.Color := _color ;
  _canvas.FillText( r, str, False,
                    _item.AbsoluteOpacity, [], TTextAlign.Leading
                  );
end ;

procedure T_PvlColorComboBox.SetFocus;
begin
  TGIS_ComboBox( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlColorComboBox'}

{$REGION 'T_PvlCustomBitmapComboBox'}

procedure T_PvlCustomBitmapComboBox.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TGIS_ComboBox.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );

  comboExt := TGIS_ComboBoxHelper.Create( TGIS_ComboBox( oControl ), TGIS_ComboBox( oControl ).Name + '_Bitmap', 3 ) ;
  comboExt.ValueEvent  := doValueBrush ;
  comboExt.RenderEvent := doRenderBrush ;
  comboExt.CustomEvent := doCustomBrush ;
  comboExt.SetOnChange( doOnChange ) ;
end;

function T_PvlCustomBitmapComboBox.doValueBrush(
  _sender : TObject ;
  _value  : String
) : String ;
var
  s1, s2, s3 : String ;
begin
  try
    SplitParamAsText( _value, s1, s2, s3 ) ;

    if s1 = GIS_PARAMTXT_TYPE_STOCK then
      Result := TGIS_ComboBoxHelper.PrepareItem(
                  False ,
                  's',
                  TGIS_ComboBoxHelperPosition.Top,
                  _value,
                  _value
                )
    else
    if not IsStringEmpty(_value) then
      Result := TGIS_ComboBoxHelper.PrepareItem(
                  False ,
                  'B',
                  TGIS_ComboBoxHelperPosition.Lru,
                  _value,
                  _value
                )
    else
      Result := '' ;
  except
    Result := '' ;
  end;
end ;


function T_PvlCustomBitmapComboBox.doCustomBrush(
  _sender : TObject ;
  _value  : String
) : String ;
var
  frms : TGIS_ControlSymbology ;
  frmb : TGIS_ControlBitmap ;
  frme : TGIS_LineSymbolEditor ;
  frm  : TGIS_ControlFieldFactor ;
  proc : TGIS_Proc ;
  s    : String ;
  s1   : String ;
  s2   : String ;
  s3   : String ;
  res  : String ;
begin
  res := '' ;

  if _value = GIS_PARAMTXT_TYPE_SYMBOL then begin
    frms := TGIS_ControlSymbology.Create( oContext.NativeParent ) ;

    proc := procedure( _modal_result : TGIS_PvlModalResult )
    begin
      if _modal_result <> TGIS_PvlModalResult.OK then
        exit ;
      if assigned( frms.Symbol ) then
        res := ConstructParamAsText(
                      GIS_PARAMTXT_TYPE_SYMBOL,
                      frms.Symbol.Name,
                      ''
                    ) ;

      DelayedUpdate( res ) ;
    end;

    if FType = TGIS_PvlCustomBitmapType.Shield then begin
      frms.OnlySVG := True ;
      frms.OnlyCategory := GIS_SHIELD_CATEGORY ;
    end;

    frms.Execute(
      '',
      proc
    ) ;
  end
  else
  if _value = GIS_PARAMTXT_TYPE_TEXTURE then begin
    frmb := TGIS_ControlBitmap.Create( oContext.NativeParent ) ;

    proc := procedure( _modal_result : TGIS_PvlModalResult )
    begin
      if _modal_result <> TGIS_PvlModalResult.OK then
        exit ;

      res := ConstructParamAsText(
               GIS_PARAMTXT_TYPE_TEXTURE,
               frmb.Bitmap.Path,
               ''
             ) ;

      DelayedUpdate( res ) ;
    end ;


    frmb.Execute(
      '',
      proc
    ) ;
  end
  else
  if _value = GIS_PARAMTXT_TYPE_CODE then begin
    frme := TGIS_LineSymbolEditor.create( TComponent( oContext.NativeParent ), True ) ;
    try
      SplitParamAsText( TGIS_PvlCustomBitmapComboBox( _sender ).Value, s1, s2, s3 );
      if s1 = GIS_PARAMTXT_TYPE_CODE then
        s := s2
      else
        s := '' ;

      frme.Execute(
        procedure( _modal_result : TModalResult )
        begin
          if _modal_result <> mrOk then
            exit ;

          res := ConstructParamAsText(
                      GIS_PARAMTXT_TYPE_CODE,
                      '' + frme.Symbol,
                      ''
                    ) ;
          DelayedUpdate( res ) ;
        end
      ) ;
    finally
      FreeObject( frme ) ;
    end ;
  end
  else
    if _value = GIS_PARAMTXT_TYPE_FIELD then begin
      frm := TGIS_ControlFieldFactor.Create( oContext.NativeParent ) ;
      if ( not assigned( FFields ) ) or ( FFields.Count = 0 ) then
        raise EGIS_Exception.create( _rsrcna( GIS_RS_ERR_FIELDFACTOR_NOFIELDS ), '', 0 ) ;
      frm.FillFields( FFields ) ;
      frm.FillUnits( TGIS_FieldFactorUnitsType.NoScale ) ;
      proc := procedure( _modal_result : TGIS_PvlModalResult )
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit ;

        res := ConstructParamAsText(
                  GIS_PARAMTXT_TYPE_FIELD,
                  frm.cmbFields.Text,
                  ''
                ) ;
        DelayedUpdate( res ) ;
      end ;

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
      Result := res ;
    end;
end ;


procedure T_PvlCustomBitmapComboBox.DelayedUpdate(
  const _val : String
) ;
var
  val : String ;
  s1, s2, s3 : String ;
begin
  if IsStringEmpty( _val ) then
    exit ;

  SplitParamAsText( _val, s1, s2, s3 );

  val := PrepareComboBoxItem(
              False,
              'B',
              TGIS_PvlComboBoxHelperPosition.Lru,
              _val,
              _val
            );
  comboExt.SetItem( val, True ) ;
end ;


procedure T_PvlCustomBitmapComboBox.doRenderBrush(
  _item      : TListBoxItem ;
  _canvas    : TCanvas      ;
  _rect      : TRectF       ;
  _font      : TFont        ;
  _color     : TAlphaColor  ;
  _class     : Char         ;
  _caption   : String       ;
  _value     : String
);
var
  sitm : STring ;
  bmp  : TGIS_Bitmap ;
  str  : String ;
  r    : TRectF ;
  fnt  : TGIS_Font ;
  args : TGIS_PvlComboBoxHelperGetBitmapEventArgs ;
begin
  str := '' ;

  case _class of
    'C' : begin
            str := _caption ;
          end ;
    'B','s' : begin
            if Assigned( _canvas ) then begin
              if assigned( FGetBitmapEvent ) then begin
                r := _rect ;
                r.Inflate(-2,-2);
                sitm := ClassName + '_' +
                        GetEnumName( TypeInfo(TGIS_PvlCustomBitmapType) , Integer( Self.&FType ) ) + '_'+
                        _value + '_' +
                        IntToStr( TruncS(r.Width) ) + '_' +
                        IntToStr( TruncS(r.Height) ) ;

                fnt := TGIS_Font.Create ;
                try
                  fnt.LoadFromFont( _font ) ;

                  if not oItemCache.TryGetValue( sitm, bmp ) then begin
                    args := TGIS_PvlComboBoxHelperGetBitmapEventArgs.Create(
                                                    _value,
                                                    GISColor( _color ),  fnt,
                                                    TruncS( r.Right-r.Left+1), TruncS( r.Bottom-r.Top+1 )
                                                ) ;
                    bmp := FGetBitmapEvent(
                              _canvas,
                              args
                           ) ;
                    oItemCache.Add( sitm, bmp );
                  end;

                  _canvas.DrawBitmap(
                    TBitmap(bmp.NativeBitmap),
                    RectF(0,0,bmp.Width,bmp.Height),
                    r, _item.AbsoluteOpacity
                  ) ;
                finally
//                  FreeObject( fnt ) ;
//                  FreeObject( args ) ;
                end;
              end ;
            end
          end ;
  end ;

  _canvas.Font.Assign( _font ) ;
  _canvas.Fill.Color := _color ;
  _canvas.FillText( _rect, str, False,
                    _item.AbsoluteOpacity, [], TTextAlign.Leading
                  );
end ;

procedure T_PvlCustomBitmapComboBox.FillPattern(
  const _hasSymbol : Boolean
);
begin
  TGIS_ComboBox( oControl ).Clear;

  FType := TGIS_PvlCustomBitmapType.Fill;

  comboExt.BeginFill ;

  comboExt.SetItem(
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
  comboExt.SetItem(
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
  comboExt.SetItem(
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
  comboExt.SetItem(
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
  comboExt.SetItem(
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
  comboExt.SetItem(
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
  comboExt.SetItem(
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
  comboExt.SetItem(
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

  comboExt.SetItem(
    PrepareComboBoxItem(
      True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
      _rsrcna( GIS_RS_LEGEND_PRM_BITMAP ) + '...', GIS_PARAMTXT_TYPE_TEXTURE
    )
  );
  if _hasSymbol then begin
    inc( iCustomFieldsCount );
    comboExt.SetItem(
      PrepareComboBoxItem(
        True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
        _rsrcna( GIS_RS_LEGEND_PRM_SYMBOL ) + '...', GIS_PARAMTXT_TYPE_SYMBOL
      )
    );
  end;

  comboExt.EndFill ;

  TGIS_ComboBox( oControl ).ItemIndex := 0;
end;

procedure T_PvlCustomBitmapComboBox.FillStyle(
  const _hasSymbol : Boolean
);
begin
  TGIS_ComboBox( oControl ).Clear;

  FType := TGIS_PvlCustomBitmapType.Outline;

  comboExt.BeginFill ;

  comboExt.SetItem(
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
  comboExt.SetItem(
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
  comboExt.SetItem(
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
  comboExt.SetItem(
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
  comboExt.SetItem(
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
  comboExt.SetItem(
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
    comboExt.SetItem(
      PrepareComboBoxItem(
        True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
        'Editor...', GIS_PARAMTXT_TYPE_CODE
      )
    );
  end;

  if _hasSymbol then begin
    inc( iCustomFieldsCount );
    comboExt.SetItem(
      PrepareComboBoxItem(
        True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
        _rsrcna( GIS_RS_LEGEND_PRM_SYMBOL ) + '...', GIS_PARAMTXT_TYPE_SYMBOL
      )
    );
  end;

  inc( iCustomFieldsCount );
  comboExt.SetItem(
    PrepareComboBoxItem(
      True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
      GIS_RS_GENERAL_BYFIELD + '...', GIS_PARAMTXT_TYPE_FIELD
    )
  );

  comboExt.EndFill ;

  TGIS_ComboBox( oControl ).ItemIndex := 0;
end;

procedure T_PvlCustomBitmapComboBox.FillMarker(
  const _hasSymbol : Boolean
);
begin
  TGIS_ComboBox( oControl ).Clear;

  FType := TGIS_PvlCustomBitmapType.Marker;

  comboExt.BeginFill ;

  comboExt.SetItem(
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
  comboExt.SetItem(
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
  comboExt.SetItem(
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
  comboExt.SetItem(
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
  comboExt.SetItem(
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
  comboExt.SetItem(
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
  comboExt.SetItem(
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
  comboExt.SetItem(
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
    comboExt.SetItem(
      PrepareComboBoxItem(
        True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
        _rsrcna( GIS_RS_LEGEND_PRM_SYMBOL ) + '...', GIS_PARAMTXT_TYPE_SYMBOL
      )
    );
  end;

  comboExt.EndFIll ;

  TGIS_ComboBox( oControl ).ItemIndex := 0;
end;

procedure T_PvlCustomBitmapComboBox.FillShield;
begin
  TGIS_ComboBox( oControl ).Clear;

  FType := TGIS_PvlCustomBitmapType.Shield;

  comboExt.BeginFill ;

  comboExt.SetItem(
    PrepareComboBoxItem(
      True, 'B', TGIS_PvlComboBoxHelperPosition.Bottom,
      GIS_RS_LEGEND_PRM_BRUSH_CLEAR, 'nil'
    )
  );

  inc( iCustomFieldsCount );
  comboExt.SetItem(
    PrepareComboBoxItem(
      True, 'C', TGIS_PvlComboBoxHelperPosition.Bottom,
      _rsrcna( GIS_RS_LEGEND_PRM_SYMBOL ) + '...', GIS_PARAMTXT_TYPE_SYMBOL
    )
  );

  comboExt.EndFill ;

  TGIS_ComboBox( oControl ).ItemIndex := 0;
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

procedure T_PvlCustomBitmapComboBox.SetFocus;
begin
  TGIS_ComboBox( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlCustomBitmapComboBox'}

{$REGION 'T_PvlColorRampComboBox'}

procedure T_PvlColorRampComboBox.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TGIS_ComboBox.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );

  comboExt := TGIS_ComboBoxHelper.Create( TGIS_ComboBox( oControl ), TGIS_ComboBox( oControl ).Name + '_Bitmap', 3 ) ;
  comboExt.ValueEvent  := doValueBitmap ;
  comboExt.RenderEvent := doRenderBitmap ;
  comboExt.SetOnChange( doOnChange ) ;

  AIdx     := nil ;
  FMode    := TGIS_ColorMapMode.Continuous ;
  FColorSchemas := [ TGIS_ColorSchema.Diverging,
                     TGIS_ColorSchema.Miscellaneous,
                     TGIS_ColorSchema.Qualitative,
                     TGIS_ColorSchema.Sequential   ] ;
  FShowNames := True ;

  iTextWidth := 0 ;
  iTextGap   := 0 ;
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
  Result := TGIS_ComboBox( oControl ).ItemIndex;
end;

procedure T_PvlColorRampComboBox.fset_Index(
  const _value : Integer
);
begin
  TGIS_ComboBox( oControl ).ItemIndex := _value;
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

function T_PvlColorRampComboBox.doValueBitmap(
  _sender : TObject ;
  _value  : String
) : String ;
var
  s1, s2, s3 : String ;
begin
  try
    SplitParamAsText( _value, s1, s2, s3 );

    if s1 = GIS_PARAMTXT_TYPE_TEXTURE then
      Result := TGIS_ComboBoxHelper.PrepareItem(
                  False ,
                  'c',
                  TGIS_ComboBoxHelperPosition.Top,
                  _value,
                  _value
                )
  except
    Result := '' ;
  end;
end ;

procedure T_PvlColorRampComboBox.doRenderBitmap(
  _item      : TListBoxItem ;
  _canvas    : TCanvas      ;
  _rect      : TRectF       ;
  _font      : TFont        ;
  _color     : TAlphaColor  ;
  _class     : Char         ;
  _caption   : String       ;
  _value     : String
);

function ConvertBitmapToGrayscale( bmp : TBitmap ) : TBitmap ;
var
  bd: TBitmapData;
  i: integer;
  j: integer;
  c: TAlphaColor;
  cl   : TGIS_Color ;
  gray : Cardinal ;
begin
  Result := TBitmap.Create( bmp.Width, bmp.Height ) ;
  Result.Assign( bmp ) ;
  if (Result.Map(TMapAccess.ReadWrite, bd)) then
    try
      for i := 0 to bmp.Width - 1 do
        for j := 0 to bmp.Height - 1 do
        begin
          c := bd.GetPixel( i, j ) ;
          cl := GISColor( c ) ;
          gray := ( cl.R + cl.G + cl.B ) div 3 ;
          cl := TGIS_Color.FromRGB(gray, gray, gray) ;
          c  := cl.ToARGB ;
          bd.SetPixel( i, j, c ) ;
        end ;
    finally
      Result.Unmap(bd);
    end;
end;

var
  r   : TRectF ;
  r1  : TRectF ;
  str : String ;
  s1,
  s2,
  s3  : String ;
  bmp : TBitmap ;
  idx : Integer ;
  i   : Integer ;

begin
  str := '' ;

  SplitParamAsText( _value, s1, s2, s3 );

  idx := AIdx[_item.Index] ;
  if length( GisColorRampList[idx].Map ) = 0 then exit ;


  if iTextGap = 0 then
    iTextGap := RounDS( _canvas.TextWidth( ' ' ) ) ;

  if iTextWidth = 0 then
    for i := 0 to TGIS_ComboBox( oControl ).Items.Count -1 do
      iTextWidth := Max( iTextWidth,
                         RoundS( _canvas.TextWidth( GisColorRampList[i].Name ) )
                       ) ;

  if not FShowNames then begin
    iTextWidth := 0 ;
    iTextGap := 0 ;
  end;


  case _class of
    'c' : begin
            r1 := _rect ;
            r1.Left   := r1.Left   + 0.5  ;
            r1.Top    := r1.Top    + 0.5  ;
            r1.Bottom := r1.Bottom - 0.5  ;
            r1.Right  := r1.Right  - 0.5 - iTextWidth - 2*iTextGap ;

            bmp := TBitmap(bmpMap[_item.Index]) ;

            if not fget_Enabled then begin
              bmp := ConvertBitmapToGrayscale( bmp ) ;
              try
                _canvas.DrawBitmap( bmp, RectF(0, 0, bmp.Width, bmp.Height),
                                    r1, 1
                                   ) ;
              finally
                FreeObject( bmp ) ;
              end;
            end else
              _canvas.DrawBitmap( bmp, RectF(0, 0, bmp.Width, bmp.Height),
                                  r1, 1
                                 ) ;

            str := s2;

            r := _rect ;
            r.Left    := _rect.Right - iTextWidth - iTextGap;
            r.Right   := _rect.Right - iTextGap;
          end ;
    else  begin
            r := _rect ;
            r.Left    := iTextGap;
            r.Right   := _rect.Right - iTextGap ;
            str  := _caption ;
          end ;
  end;

  if FShowNames then begin
    _canvas.Font.Assign( _font ) ;
    _canvas.Fill.Color := _color ;
    _canvas.FillText( r, str, False,
                      1, [], TTextAlign.Leading
                    );
  end;
end ;

procedure T_PvlColorRampComboBox.gradHorizontal(
  const _canvas    : TCanvas ;
  const _rect      : TRect ;
  const _fromColor : TColor ;
  const _toColor   : TColor ;
  const _mode      : TGIS_ColorMapMode
) ;
 var
   x        : Integer ;
   dr,dg,db : Double ;
   c1,c2    : TColor ;
   r1,r2,
   g1,g2,
   b1,b2    : Byte ;
   r,g,b    : Byte ;
   ir,ig,ib : Integer ;
   cnt      : Integer ;
   w        : Integer ;

    function GetRValue( const _color : Integer ) : Byte ;
    begin
      Result := Byte(( _color and $ff0000 ) shr 16) ;
    end;

    function GetGValue( const _color : Integer ) : Byte ;
    begin
      Result := Byte(( _color and $0000ff00 ) shr 8) ;
    end;

    function GetBValue( const _color : Integer ) : Byte ;
    begin
      Result := Byte( _color and $000000ff ) ;
    end;

 begin
   c1 := _fromColor;
   r1 := GetRValue(c1) ;
   g1 := GetGValue(c1) ;
   b1 := GetBValue(c1) ;

   c2 := _toColor;
   r2 := GetRValue(c2) ;
   g2 := GetGValue(c2) ;
   b2 := GetBValue(c2) ;

   w  := _rect.Right - _rect.Left ;
   if w = 0 then
    w := 1 ;
   dr := (r2-r1) / w ;
   dg := (g2-g1) / w ;
   db := (b2-b1) / w ;

   if _mode = TGIS_ColorMapMode.Continuous then begin
     cnt := 0 ;
     for X := _rect.Left to _rect.Right-1 do begin
       ir := r1 + Ceil(dr*cnt) ;
       if ir > 255 then
         r := 255
       else if ir < 0 then
         r := 0
       else
        r := ir ;
       ig := g1 + Ceil(dg*cnt) ;
       if ig > 255 then
         g := 255
       else if ig < 0 then
         g := 0
       else
        g := ig ;
       ib := b1 + Ceil(db*cnt) ;
       if ib > 255 then
         b := 255
       else if ib < 0 then
         b := 0
       else
        b := ib ;

       _canvas.Stroke.Thickness := 1 ;
       _canvas.Stroke.Color := TGIS_Color.FromRGB(r,g,b).ARGB ;
       _canvas.DrawLine( PointF(X, _rect.Top), PointF(X, _rect.Bottom),1 ) ;
       inc( cnt ) ;
     end ;
   end
   else begin
     _canvas.Fill.Color := TGIS_Color.FromRGB(r1,g1,b1).ARGB ;
     _canvas.FillRect( RectF( _rect.Left, _rect.Top, _rect.Right, _rect.Bottom ),
                       0, 0, [], 1
                     ) ;
   end;
 end;

procedure T_PvlColorRampComboBox.doOnChange(
  _sender: TObject
);
begin
  if Assigned( FOnChange ) then
    FOnChange( Self );
end;

procedure T_PvlColorRampComboBox.Fill;
var
  i_ramp         : Integer ;
  i_color        : Integer ;
  bmp            : TBitmap ;
  rect           : TRect ;
  ramps_count    : Integer ;
  col_map_arr    : TGIS_ColorMapArray ;
  left_colormap  : TGIS_ColorMap ;
  right_colormap : TGIS_ColorMap ;

  function VCLColor( const _color : TGIS_Color ) : TColor ;
  begin
    Result := _color.ARGB and $00FFFFFF ;
    if Result = $00000000 then
      Result := $00010101 ; // to avoid bad interpretation of NULL
  end ;

  function addItem( const _value : String ) : Integer ;
  begin
    Result := comboExt.SetItem(
      TGIS_ComboBoxHelper.PrepareItem(
        False, 'c', TGIS_ComboBoxHelperPosition.Top,
        _value, ConstructParamAsText( GIS_PARAMTXT_TYPE_TEXTURE, _value, '' )
      )
    ) ;
  end ;

begin
  if bLock then
    exit;

  FreeObject( bmpMap ) ;

  TGIS_ComboBox( oControl ).ItemIndex := -1 ;

  ramps_count := GisColorRampList.Count ;
  if ramps_count = 0 then
    GisColorRampList.Init ;

  bmpMap := TGIS_ObjectList.Create( True ) ;
  rect.Top    := 0 ;
  rect.Bottom := 1 ;

  for i_ramp := 0 to ramps_count - 1 do begin
    if not ( GisColorRampList[i_ramp].MapType in FColorSchemas ) then
      continue ;

    bmp := TBitmap.Create ;
    bmp.Width  := RoundS(fget_Width) ;
    bmp.Height := 1 ;
    bmp.Canvas.BeginScene ;

    col_map_arr := GisColorRampList[i_ramp].RealizeColorMap( fget_Mode, 0, fget_Reverse ) ;

    for i_color := 0 to high( col_map_arr ) - 1 do begin
      left_colormap := col_map_arr[i_color] ;
      right_colormap := col_map_arr[i_color+1] ;

      rect.Left  := RoundS( fget_Width * left_colormap.Index / 100 ) ;
      rect.Right := RoundS( fget_Width * right_colormap.Index / 100 ) ;

      gradHorizontal(
        bmp.Canvas,
        rect,
        VCLColor( left_colormap.RGB ),
        VCLColor( right_colormap.RGB),
        FMode
      ) ;
    end ;

    bmp.Canvas.EndScene ;
    bmpMap.Add( bmp ) ;

    if ( i_ramp mod 20 ) = 0 then
      Application.ProcessMessages ;
  end ;

  SetLength( AIdx, bmpMap.Count ) ;
  comboExt.BeginFill ;
  try
    TGIS_ComboBox( oControl ).Items.Clear ;
    for i_ramp := 0 to ramps_count - 1 do begin
      if not (GisColorRampList[i_ramp].MapType in FColorSchemas) then continue ;
      i_color := addItem( GisColorRampList[i_ramp].Name ) ;
      AIdx[i_color] := i_ramp ;
    end ;
  finally
    comboExt.EndFill ;
  end ;
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
  if (TGIS_ComboBox( oControl ).ItemIndex >=0) and (TGIS_ComboBox( oControl ).ItemIndex < GisColorRampList.Count) then begin
    idx := AIdx[TGIS_ComboBox( oControl ).ItemIndex];
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
    TGIS_ComboBox( oControl ).ItemHeight := _value;
end;

function T_PvlColorRampComboBox.fget_ItemCount
  : Integer;
begin
  Result := TGIS_ComboBox( oControl ).Items.Count;
end;

function T_PvlColorRampComboBox.fget_Item(
  const _idx: Integer
): string;
begin
  Result := TGIS_ComboBox( oControl ).Items[_idx];
end;

procedure T_PvlColorRampComboBox.SetFocus;
begin
  TGIS_ComboBox( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlColorRampComboBox'}

{$REGION 'T_PvlComboBox'}

procedure T_PvlComboBox.doCreate(
  const _context : TGIS_PvlContext
);
begin
  oControl := TComboBox.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );
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
  Result := TComboBox( oControl ).Selected.Text;
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
  if TComboBox( oControl ).Items.IndexOf( _value ) >= 0 then
    TComboBox( oControl ).ItemIndex := TComboBox( oControl ).Items.IndexOf( _value ) ;
end;

function ComboBoxCompare(Item1, Item2: TFmxObject): Integer ;
begin
  Result := CompareText(TListBoxItem( Item1 ).Text, TListBoxItem( Item2 ).Text );
end;

function T_PvlComboBox.fget_Sorted
  : Boolean;
begin
  Result := bSorted ;
end;

procedure T_PvlComboBox.fset_Sorted(
  const _value : Boolean
);
begin
  bSorted := _value ;
  if bSorted then begin
    TComboBox( oControl ).Sort( ComboBoxCompare ) ;
  end;
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
  oControl := TComboEdit.Create( TFmxObject( _context.NativeParent ) );
  lstItems := TGIS_ListOFStrings.Create ;
  TComboEdit( oControl ).OnChange := doOnChange ;
  TComboEdit( oControl ).OnKeyUp  := doOnKeyUp ;
  TComboEdit( oControl ).OnKeyDown:= doOnKeyDown ;
  TComboEdit( oControl ).DropDownCount := 5 ;
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );
end;

procedure T_PvlComboEdit.doDestroy;
begin
  FreeObject( lstItems ) ;
  inherited ;
end;

procedure T_PvlComboEdit.fset_FilteredSearch(
  const _value: Boolean
) ;
begin
  bFilteredSearch := _value ;
end;

function T_PvlComboEdit.fget_FilteredSearch
  : Boolean ;
begin
  Result := bFilteredSearch ;
end;

procedure T_PvlComboEdit.doOnKeyUp(
       _sender : TObject;
   var _key    : Word;
   var _keychar: WideChar;
       _shift  : TShiftState
);
var
  curr : String ;
  idx  : Integer ;

  procedure refreshDropDown ;
  var
    i    : Integer ;
  begin
    TComboEdit( oControl ).BeginUpdate ;
    for i := 0 to lstItems.Count - 1 do begin
      if SameText( LeftStr( lstItems[i], Length( curr ) ), curr ) then
      begin
        if TComboEdit( oControl ).Items.IndexOf(lstItems[i]) < 0 then
          TComboEdit( oControl ).Items.Add(lstItems[i]);
      end
      else
      begin
        idx := TComboEdit( oControl ).Items.IndexOf(lstItems[i]);
        if idx >= 0 then
          TComboEdit( oControl ).Items.Delete(idx);
      end;
    end;
    TComboEdit( oControl ).EndUpdate ;
    TComboEdit( oControl ).ItemIndex := 0  ;

    // Trick to ensure reset of the selection upon removing text
    TComboEdit( oControl ).ItemIndex := -1 ;
    TComboEdit( oControl ).ItemIndex := 0  ;
  end;

begin
   inherited ;

  if not TGIS_PvlComboEdit( oParent ).FilteredSearch then begin
    exit ;
  end;

  try
    if bReturn then
      exit ;

    if bUpDown then begin
      if not TComboEdit( oControl ).DroppedDown then
        TComboEdit( oControl ).DropDown ;
      exit ;
    end;

    if _key in [VKLEFT..VKRIGHT] then begin
      exit ;
    end;

    if _key = vkControl then
      exit ;

    curr :=  TComboEdit( oControl ).Text ;

    {$IFDEF LEVEL_RX10_FMX}
      if TComboEdit( oControl ).DroppedDown then
        TComboEdit( oControl ).CloseDropDown ;
    {$ENDIF}

    refreshDropDown ;

    if ( _key = vkA ) and ( ssCtrl in _shift ) then begin
      TComboEdit( oControl ).SelStart := 0 ;
      TComboEdit( oControl ).SelLength := Length( curr ) ;
      exit ;
    end;

    if ( _key = VKDELETE ) then begin
      TComboEdit( oControl ).SelStart := isel ;
      exit;
    end;

    if not ( CharInSet( _keychar, ['a'..'z'] ) or  CharInSet( _keychar, ['A'..'Z'] ) or CharInSet( _keychar, ['0'..'9'] ) ) then begin
      if not TComboEdit( oControl ).DroppedDown then
        TComboEdit( oControl ).DropDown ;
      TComboEdit( oControl ).Text := curr ;
      if not ( _key = VKCONTROL ) then
        TComboEdit( oControl ).SelStart := Length( curr ) ;
      exit ;
    end;

    if ( _key = VKESCAPE ) then begin
      {$IFDEF LEVEL_RX10_FMX}
        if TComboEdit( oControl ).DroppedDown then
          TComboEdit( oControl ).CloseDropDown ;
      {$ENDIF}
      TComboEdit( oControl ).Text := curr ;
    end else if ( _key = vkReturn ) then begin
      {$IFDEF LEVEL_RX10_FMX}
        if TComboEdit( oControl ).DroppedDown then
          TComboEdit( oControl ).CloseDropDown ;
      {$ENDIF}
      TComboEdit( oControl ).Text := TComboEdit( oControl ).Items.Strings[ TComboEdit( oControl ).ItemIndex ] ;
    end else
    if ( TComboEdit( oControl ).Items.Count = 1 ) then begin
      {$IFDEF LEVEL_RX10_FMX}
        if TComboEdit( oControl ).DroppedDown then
          TComboEdit( oControl ).CloseDropDown ;
      {$ENDIF}
      TComboEdit( oControl ).Text := TComboEdit( oControl ).Items.Strings[0] ;
      TComboEdit( oControl ).ItemIndex := 0 ;
      TComboEdit( oControl ).SelStart := Length( curr ) ;
      iSel := TComboEdit( oControl ).SelStart ;
      TComboEdit( oControl ).SelLength := TComboEdit( oControl ).Items.Strings[0].Length - Length( curr ) ;
    end
    else begin
      if not TComboEdit( oControl ).DroppedDown then
        TComboEdit( oControl ).DropDown ;
      TComboEdit( oControl ).Text := curr ;
      TComboEdit( oControl ).SelStart := Length( curr ) ;
    end;

    doOnChange( Parent ) ;
  finally
    isel := TComboEdit( oControl ).SelStart ;
  end;

  inherited;
end;


procedure T_PvlComboEdit.doOnKeyDown(
       _sender : TObject;
   var _key    : Word;
   var _keychar: WideChar;
       _shift  : TShiftState
);
begin
  inherited ;

  if _key = VKRETURN then
    bReturn := True
  else
    bReturn := False ;

  if ( _key in [VKUP..VKDOWN] ) or ( _key = VKESCAPE ) then begin
    bUpDown := True ;
  end else begin
    bUpDown := False ;
  end;

  if ( _key in [VKLEFT..VKRIGHT] ) then begin
    bLeftRight := True ;
  end else begin
    bLeftRight := False ;
  end;
end;

procedure T_PvlComboEdit.doOnChange(
  _sender: TObject
) ;
begin
  if bReturn then
  begin
    {$IFDEF LEVEL_RX10_FMX}
      if TComboEdit( oControl ).DroppedDown then
        TComboEdit( oControl ).CloseDropDown ;
    {$ENDIF}
  end;

  if Assigned( FOnChange ) then
    FOnChange( Parent );
end;

function T_PvlComboEdit.fget_ItemsCount
  : Integer;
begin
  Result := TComboEdit( oControl ).Items.Count;
end;

function T_PvlComboEdit.fget_Item(
  const _idx   : Integer
) : String;
begin
  Result := TComboEdit( oControl ).Items[ _idx ];
end;

function  T_PvlComboEdit.fget_ItemIndex
  : Integer;
begin
  Result := TComboEdit( oControl ).ItemIndex;
end;

procedure T_PvlComboEdit.fset_ItemIndex(
  const _value : Integer
);
begin
  TComboEdit( oControl ).ItemIndex := _value;
  doOnChange( Parent );
end;

function  T_PvlComboEdit.fget_Text
  : String;
begin
  Result := TComboEdit( oControl ).Text;
end;

procedure T_PvlComboEdit.fset_Tag(
  const _value : NativeInt
);
begin
  FTag := _value;
end;

function  T_PvlComboEdit.fget_Tag
  : NativeInt;
begin
  Result := FTag;
end;

procedure T_PvlComboEdit.fset_Text(
  const _value : String
);
begin
  if TComboEdit( oControl ).Items.IndexOf( _value ) >= 0 then
    TComboEdit( oControl ).ItemIndex := TComboEdit( oControl ).Items.IndexOf( _value )
  else
    TComboEdit( oControl ).Text := _value ;
end;

function T_PvlComboEdit.fget_Sorted
  : Boolean;
begin
  Result := bSorted ;
end;

procedure T_PvlComboEdit.fset_Sorted(
  const _value : Boolean
);
begin
  bSorted := _value ;
  if bSorted then begin
    TComboEdit( oControl ).Sort( ComboBoxCompare ) ;
  end;
end;

function T_PvlComboEdit.fget_DropDownCount
  : Integer;
begin
  Result := TComboEdit( oControl ).DropDownCount;
end;

procedure T_PvlComboEdit.fset_DropDownCount(
  const _value : Integer
);
begin
  TComboEdit( oControl ).DropDownCount := _value;
end;

function T_PvlComboEdit.fget_OnChange
  : TGIS_PvlEvent;
begin
  Result := FOnChange;
end;

procedure T_PvlComboEdit.fset_OnChange(
  const _value : TGIS_PvlEvent
);
begin
  FOnChange := _value;
end;

procedure T_PvlComboEdit.BeginUpdate;
begin
  TComboEdit( oControl ).Items.BeginUpdate;
end;

procedure T_PvlComboEdit.EndUpdate;
begin
  TComboEdit( oControl ).Items.EndUpdate;
end;

procedure T_PvlComboEdit.ItemsClear;
begin
  TComboEdit( oControl ).Items.Clear;
  lstItems.Clear ;
end;

procedure T_PvlComboEdit.ItemsAdd(
  const _item  : String
);
begin
  TComboEdit( oControl ).Items.Add( _item );
  lstItems.Add( _item ) ;
end;

function T_PvlComboEdit.IndexOf(
  const _item: string
): Integer;
begin
  Result := TComboEdit( oControl ).Items.IndexOf( _item );
end;

procedure T_PvlComboEdit.SetFocus;
begin
  TComboEdit( oControl ).SetFocus;
end;

{$ENDREGION 'T_PvlComboEdit'}

{$REGION 'T_PvlCheckBox'}

procedure T_PvlCheckBox.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TCheckBox.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );
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
  Result := TCheckBox( oControl ).Text;
end;

procedure T_PvlCheckBox.fset_Caption(
  const _value : string
);
begin
  TCheckBox( oControl ).Text := _value;
end;

function T_PvlCheckBox.fget_Checked
  : Boolean;
begin
  Result := TCheckBox( oControl ).IsChecked;
end;

procedure T_PvlCheckBox.fset_Checked(
  const _value : Boolean
);
begin
  TCheckBox( oControl ).IsChecked := _value;
end;

function T_PvlCheckBox.fget_FontSize
  : Integer;
begin
  Result := RoundS( TCheckBox( oControl ).Font.Size );
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
  Result := TCheckBox( oControl ).Font.Family;
end;

procedure T_PvlCheckBox.fset_FontFamily(
  const _value : string
);
begin
  TCheckBox( oControl ).Font.Family := _value;
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
  oControl := TRadioButton.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );
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
  Result := TRadioButton( oControl ).Text;
end;

procedure T_PvlRadioButton.fset_Caption(
  const _value : string
);
begin
  TRadioButton( oControl ).Text := _value;
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
  TRadioButton( oControl ).GroupName := TGIS_PvlRadioButton( oParent ).Group ;
  // do nothing
end;

function T_PvlRadioButton.fget_Checked
  : Boolean;
begin
  Result := TRadioButton( oControl ).IsChecked;
end;

procedure T_PvlRadioButton.fset_Checked(
  const _value : Boolean
);
begin
  TRadioButton( oControl ).IsChecked := _value;
end;

function T_PvlRadioButton.fget_FontSize
  : Integer;
begin
  Result := RoundS( TRadioButton( oControl ).Font.Size );
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
  Result := TRadioButton( oControl ).Font.Family;
end;

procedure T_PvlRadioButton.fset_FontFamily(
  const _value : string
);
begin
  TRadioButton( oControl ).Font.Family := _value;
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
  oControl := TListView.Create( TFmxObject( _context.NativeParent ) );
  oPanel := TVertScrollBox.Create( TFmxObject( oControl ) ) ;

  TListView( oControl ).Transparent := True ;

  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );
  oPanel.Parent := TFmxObject( oControl ) ;
  oPanel.Align := TAlignLayout.Client ;

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
  Result := oPanel.ShowScrollBars ;
end;

procedure T_PvlPanel.fset_Scrollable(
  const _value : Boolean
);
begin
  oPanel.ShowScrollBars := _value ;
end;

function T_PvlPanel.fget_Border
  : Boolean;
begin
  Result := False ; // No borders in FMX on previewpanel?
end;

procedure T_PvlPanel.fset_Border(
  const _value : Boolean
);
begin
// No borders in FMX on previewpanel?
end;

procedure T_PvlPanel.AddComponent(
  const _component: TGIS_PvlControl
);
begin
  oPanel.AddObject( TControl( _component.NativeControl ) ) ;
  if _component is TGIS_PvlSVGList then begin
    oPanel.ShowScrollBars := True ;
    TListView( oControl ).Transparent := False ;
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
begin
  oPanel.Content.DeleteChildren ;
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
  oControl := TRectangle.Create( TFmxObject( _context.NativeParent ) );
  TRectangle( oControl ).Parent := TFmxObject( _context.NativeParent ) ;
  TRectangle( oControl ).Fill.Color := $F0F0F0   ;

  oLabel := TLabel.Create( TRectangle( oControl ) ) ;
  oLabel.Parent := TRectangle( oControl ) ;
  oLabel.Visible := False ;
  oLabel.AutoSize := False ;
  oLabel.Align := TAlignLayout.Client ;
  oLabel.TextAlign := TTextAlign.Center ;
  oLabel.StyledSettings := oLabel.StyledSettings -
                           [ TStyledSetting.Style,
                             TStyledSetting.Family,
                             TStyledSetting.Size
                           ] ;

  oImage := TImage.Create( TRectangle( oControl ) ) ;
  oImage.Parent := TRectangle( oControl ) ;
  oImage.WrapMode := TImageWrapMode.Stretch ;
  oImage.Align := TAlignLayout.Client ;
  oImage.Visible := False ;


  oLabel.Width := fget_Width ;
  oLabel.Height := fget_Height ;

  oImage.Width := fget_Width * oContext.CanvasScale ;
  oImage.Height := fget_Height * oContext.CanvasScale ;

  applyStyle ;
end;

procedure T_PvlPreviewPanel.applyStyle;
var
  tmp : TGIS_Color ;
begin
  get_style_colors( oContext, oControl, cStyledAreaColor, tmp ) ;
end;

function T_PvlPreviewPanel.fget_IsStyled
  : Boolean;
begin
  Result := True;
end;

function T_PvlPreviewPanel.fget_Caption
  : string;
begin
  Result := oLabel.Text;
end;

procedure T_PvlPreviewPanel.fset_Caption(
  const _value : string
);
begin
  oLabel.Text := _value;
  Invalidate;
end;

function T_PvlPreviewPanel.fget_Border
  : Boolean;
begin
  Result := False ; // No borders in FMX on previewpanel?
end;

procedure T_PvlPreviewPanel.fset_Border(
  const _value : Boolean
);
begin
// No borders in FMX on previewpanel?
end;

function T_PvlPreviewPanel.fget_FontSize
  : Integer;
begin
  Result := RoundS( oLabel.Font.Size );
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
  Result := oLabel.Font.Family;
end;

procedure T_PvlPreviewPanel.fset_FontFamily(
  const _value : string
);
begin
  oLabel.Font.Family := _value;
end;

function T_PvlPreviewPanel.fget_Bitmap
  : TGIS_Bitmap;
var
  bmp : TGIS_Bitmap;
begin
  bmp := TGIS_Bitmap.Create;
  bmp.LoadFromBitmap( oImage.Bitmap, '' );
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
      oImage.Bitmap.Assign( TBitmap( _value.NativeBitmap ) );
      bBitmap := True;
    end;
    Invalidate;
  end;
end;

function T_PvlPreviewPanel.fget_Color
  : TGIS_Color;
begin
  Result := GISColor( TRectangle( oControl ).Fill.Color ) ;
end;

procedure T_PvlPreviewPanel.fset_Color(
  const _value : TGIS_Color
);
begin
  TRectangle( oControl ).Fill.Color := FMXColor( _value ) ;
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
  oControl := TGroupBox.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );
end;

function T_PvlGroupBox.fget_Caption
  : String;
begin
  Result := TGroupBox( oControl ).Text;
end;

procedure T_PvlGroupBox.fset_Caption(
  const _value : String
);
begin
  TGroupBox( oControl ).Text := _value;
end;

{$ENDREGION 'T_PvlGroupBox'}


{$REGION 'T_PvlTrackBar'}

procedure T_PvlTrackBar.doCreate(
  const _context: TGIS_PvlContext
);
begin
  oControl := TTrackBar.Create( TFmxObject( _context.NativeParent ) ) ;
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );
  TTrackBar( oControl ).OnChange := doOnChange ;
end;

function T_PvlTrackBar.fget_Minimum
  : Integer ;
begin
  Result := RoundS( TTrackBar( oControl ).Min ) ;
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
  Result := RoundS( TTrackBar( oControl ).Max ) ;
end;

procedure T_PvlTrackBar.fset_Maximum(
  const _value: Integer
) ;
begin
  TTrackBar( oControl ).Max := _value ;
end;

function T_PvlTrackBar.fget_Tick
  : Integer ;
begin
  Result := RoundS( TTrackBar( oControl ).Frequency ) ;
end;

procedure T_PvlTrackBar.fset_Tick(
  const _value: Integer
) ;
begin
  TTrackBar( oControl ).Frequency := _value ;
end;

function T_PvlTrackBar.fget_DrawTicks
  : Boolean ;
begin
//  Result := TTrackBar( oControl ).TickStyle = tsAuto ;
end;

procedure T_PvlTrackBar.fset_DrawTicks(
  const _value: Boolean
) ;
begin
//  if _value then
//    TTrackBar( oControl ).TickStyle := tsAuto
//  else
//    TTrackBar( oControl ).TickStyle := tsNone ;
end;

function T_PvlTrackBar.fget_Position
  : Integer ;
begin
  Result := RoundS( TTrackBar( oControl ).Value )
end;

procedure T_PvlTrackBar.fset_Position(
  const _value: Integer
) ;
begin
  TTrackBar( oControl ).Value := _value ;
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
  oControl := TTreeView.Create( TFmxObject( _context.NativeParent ) );
  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );

  TTreeView( oControl ).OnClick := doOnClick;
  TTreeView( oControl ).OnChange := doOnSelectChange;
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
  _sender: TObject
);
begin
  if assigned( FOnSelectChange ) then
    FOnSelectChange( Parent );
end;

procedure T_PvlTree.doOnDeletion(
  _sender : TObject;
  _node   : T_TreeViewItemEx
);
begin
  FreeObjectNotNil( _node.Node );
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
    Result := T_TreeViewItemEx( TTreeView( oControl ).Selected ).Node
  else
    Result := nil;
end;

procedure T_PvlTree.fset_Selected(
  const _value : TGIS_PvlTreeNode
);
begin
  T_TreeViewItemEx( _value.NativeControl ).Select
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
  const _context : TGIS_PvlContext;
  const _parent  : IGIS_PvlBase
);
begin
  inherited Create( _context, _parent );
  oControl := nil ;
end;

procedure T_PvlTreeNode.doDestroy;
begin
  FreeObject( oControl ) ;

  inherited;
end;

procedure T_PvlTreeNode.doCreateNode(
  const _node    : TGIS_PvlTreeNode;
  const _caption : String;
  const _index   : Integer
);
var
  tv : TTreeView;
  nd : T_TreeViewItemEx ;
begin
  tv := TTreeView( _node.TreeControl.NativeControl ) ;

  nd := T_TreeViewItemEx.Create(tv);
  nd.Text := _caption ;

  //  Count - 1 because its already added to Parent in object list.
  if _index >= _node.Count - 1 then begin
    if oControl <> nil then // nil only for Root node!
      TTreeView( _node.Parent.NativeControl ).AddObject( nd )
    else
      tv.AddObject( nd ) ;
  end else begin
    if oControl <> nil then // nil only for Root node!
      TTreeView( _node.Parent.NativeControl ).InsertObject( _index, nd )
    else
      tv.InsertObject( _index, nd ) ;
  end;

  _node.NativeControl := nd ;

  T_TreeViewItemEx( _node.NativeControl ).Node := _node ;
end;

procedure T_PvlTreeNode.doRemoveNode(
  const _node : TGIS_PvlTreeNode
);
begin
  T_TreeViewItemEx( _node.Parent.NativeControl ).RemoveObject( T_TreeViewItemEx( _node.NativeControl ) ) ;
  T_TreeViewItemEx( _node.NativeControl  ).Free;
end;

procedure T_PvlTreeNode.doDeleteNode(
  const _node : TGIS_PvlTreeNode
);
begin
  T_TreeViewItemEx( _node.Parent.NativeControl ).RemoveObject( T_TreeViewItemEx( _node.NativeControl ) ) ;
  T_TreeViewItemEx( _node.NativeControl  ).Free;
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
      T_TreeViewItemEx( _newParent.NativeControl ).AddObject( T_TreeViewItemEx( oControl ) )
    else
      tv.AddObject( T_TreeViewItemEx( _node.NativeControl ) ) ;
  end else begin
    if _newParent.NativeControl <> nil then // nil only for Root node!
      T_TreeViewItemEx( _newParent.NativeControl ).InsertObject( _index, T_TreeViewItemEx( _node.NativeControl ) )
    else
      tv.InsertObject( _index, T_TreeViewItemEx( _node.NativeControl ) ) ;
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
    T_TreeViewItemEx( oControl ).Text := _value;
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
  Result := T_TreeViewItemEx( oControl ).IsExpanded;
end;

procedure T_PvlTreeNode.fset_Expanded(
  const _value : Boolean
);
begin
  T_TreeViewItemEx( oControl ).IsExpanded := _value;
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
  oControl := TListBox.Create( TFmxObject( _context.NativeParent ) );
  TListBox( oControl ).Parent := TFmxObject( _context.NativeParent );
  TListBox( oControl ).ItemHeight := 14 ;
  TListBox( oControl ).OnClick := doOnClick;

  itmCount := 0 ;
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
    if TListBox( oControl ).ListItems[ i ].IsSelected then
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
  Result := TListBox( oControl ).ListItems[ _index ].IsSelected ;
end;

procedure T_PvlListBox.fset_Selected(
  const _index: Integer;
  const _value: Boolean
) ;
begin
  TListBox( oControl ).ListItems[ _index ].IsSelected := _value ;
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
  itmCount := 0 ;
end;

procedure T_PvlListBox.ItemsAdd(
  const _item: string
);
var
  h : Single ;
begin
  TListBox( oControl ).Items.Add( _item );
  inc( itmCount ) ;

  h := TListBox( oControl ).ItemHeight * itmCount ;
  if h > TListBox( oControl ).Height then
    TListBox( oControl ).Height := h + 7 ;
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
  iCount := 0 ;
  iIdx := -1 ;

  itmList := TList<T_SVGListItem>.Create ;

  oControl := TLayout.Create( TFmxObject( _context.NativeParent ) );
  TLayout( oControl ).Parent := TFmxObject( _context.NativeParent ) ;

  oLib := TGIS_SymbolLibrarySVG.Handle ;
  oLib.OpenForBrowsing ;

  TControl( oControl ).Parent := TFmxObject( _context.NativeParent );


  TLayout( oControl ).Width := fget_Width ;
  TLayout( oControl ).Height := fget_Height ;

  applyStyle ;
end;

procedure T_PvlSVGList.doDestroy;
begin
  FreeObject( lstItm ) ;

  inherited;
end;

procedure T_PvlSVGList.applyStyle;
var
  isize : Integer       ;
begin
  get_style_colors( oContext, oControl, cForeground, cBackground ) ;

  {$IFNDEF GIS_MOBILE_DIALOGS}
    isize := RoundS(  SVG_SIZE * oContext.CanvasScale * GUIScale ) ;
  {$ELSE}
    isize := RoundS(  SVG_SIZE * oContext.CanvasScale ) ;
  {$ENDIF}

  BeginUpdate;

  {$IFNDEF LEVEL_RX10_FMX}
    // not supported
  {$ELSE}
    TLayout( oControl ).EnumObjects(
      function( _obj : TFmxObject ) : TEnumProcResult
      var
        itm   : T_SVGListItem ;
        bmp   : TGIS_Bitmap   ;
      begin
        if _obj is T_SVGListItem then begin
          itm := T_SVGListItem(_obj) ;
          bmp := oLib.GetPreview( itm.oName, oContext.PPI, isize,
                                  cForeground, TGIS_Color.Silver
                                ) ;
          bmp.Premultiplied := True ;
          bmp.MakeGlowing( cBackground,
                           2 * Max( 1, RoundS( oContext.PPI / 96 ) )
                         ) ;

          itm.oImage.Bitmap.Assign( TBitmap( bmp.NativeBitmap ) ) ;
        end;
        Result := TEnumProcResult.Continue ;
      end
    ) ;
  {$ENDIF}

  EndUpdate;
  TLayout( oControl ).Repaint ;
end;

function T_PvlSVGList.fget_ItemList
  : TGIS_ListOfStrings;
var
  idx : Integer;
begin
  if not assigned( lstItm ) then
    lstItm := TGIS_ListOfStrings.Create;

  lstItm.Clear ;
  for idx := 0 to itmList.Count - 1 do begin
    lstItm.Add( itmList.Items[ idx ].ToString );
  end;

  Result := lstItm ;
end;

function T_PvlSVGList.fget_Item(
  const _idx: Integer
) : string;
begin
  if _idx >= 0 then
    Result := itmList.Items[_idx].oName ;
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
  Result := iIdx ;
end;

procedure T_PvlSVGList.fset_ItemIndex(
  const _value : Integer
);
begin
  iIdx := _value ;
end;

function T_PvlSVGList.fget_ItemsCount
  : Integer;
begin
  Result := iCount ;
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
  TLayout( oControl ).BeginUpdate;
end;

procedure T_PvlSVGList.EndUpdate;
begin
  TLayout( oControl ).EndUpdate;
end;

procedure T_PvlSVGList.ItemsClear;
var
  i : Integer ;
begin
  iCount := 0 ;
  if Assigned( TLayout( oControl ).Children ) then begin
    for i:= TLayout( oControl ).Children.Count -1 downto 0 do
      TLayout( oControl ).RemoveObject( TLayout( oControl ).Children.Items[i] );
  end;
  col := 0 ;
  row := 0 ;
  itmList.Clear ;

  oContext.Refresh ;
end;

procedure T_PvlSVGList.ItemsAdd(
  const _item: string
);
var
  svgItem  : T_SVGListItem ;
  isize    : Integer       ;
  bmp      : TGIS_Bitmap   ;
begin
  svgItem := T_SVGListItem.Create( TListView( oControl ) ) ;

  {$IFNDEF GIS_MOBILE_DIALOGS}
    svgItem.OnClick := doOnClick ;
  {$ENDIF}
  svgItem.OnMouseDown := doOnMouseDown ;

  svgItem.oName := _item ;
  svgItem.oCaption.Text := oLib.GetCaption( _item ) ;
  svgItem.oIdx := iCount ;

  {$IFNDEF GIS_MOBILE_DIALOGS}
    isize := RoundS(  SVG_SIZE * oContext.CanvasScale * GUIScale ) ;
  {$ELSE}
    isize := RoundS(  SVG_SIZE * oContext.CanvasScale ) ;
  {$ENDIF}

  bmp := oLib.GetPreview( _item, oContext.PPI, isize,
                          cForeground, TGIS_Color.Silver
                        ) ;
  bmp.Premultiplied := True ;
  bmp.MakeGlowing( cBackground,
                   2 * Max( 1, RoundS( oContext.PPI / 96 ) )
                 ) ;

  svgItem.oImage.Bitmap.Assign( TBitmap( bmp.NativeBitmap ) ) ;

  svgItem.Position.X := col * svgItem.Width ;
  svgItem.Position.Y := row * svgItem.Height ;

  itmList.Add( svgItem ) ;
  TLayout( oControl ).AddObject( svgItem ) ;

  inc( iCount ) ;
  inc( col ) ;
  if col >= 3 then begin
    inc( row ) ;
    col := 0 ;
  end ;

  fset_Width( 3 * Integer.Parse( svgItem.Width.ToString ) ) ;
  fset_Height( row * Integer.Parse( svgItem.Height.ToString ) ) ;
  TLayout( oControl ).Width := fget_Width ;
  TLayout( oControl ).Height := fget_Height ;
end;

procedure T_PvlSVGList.SetFocus;
begin
  TLayout( oControl ).SetFocus;
end;

procedure T_PvlSVGList.doOnClick(
  _sender: TObject
);
var
  i : Integer ;
begin
  if _sender is T_SVGListItem then begin
    fset_ItemIndex( T_SVGListItem( _sender ).oIdx ) ;
    if Assigned( itmList ) then begin
      for i:= itmList.Count - 1 downto 0 do
        T_SVGListItem( itmList.Items[i] ).Selected := False ;
    end;
    T_SVGListItem( _sender ).Selected := True ;

    if Assigned( FOnClick ) then
      FOnClick( Parent );
  end;
end;

procedure T_PvlSVGList.doOnMouseDown(
  _sender : TObject ;
  _button : TMouseButton ;
  _shift  : TShiftState ;
  _x      : Single ;
  _y      : Single
) ;
begin
  doOnClick( _sender ) ;
end;

procedure T_PvlSVGList.DoRedraw;
begin
  oContext.Refresh ;
  applyStyle ;
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
) ;
var
  frm : TFmxObject ;
begin
  oMaster := _context;
  oParent := _parent;

  bBiDi := _righttoleft;

  iHMargin := 8 ;
  iVMargin := 8 ;

  oOwner := T_component.Create(TComponent(oParent));
  oOwner.oMaster := oMaster; // trick to destroyself TGIS_Pvl*
  oOwner.oOwner  := self;    // trick to destroyself T_Pvl*

  frm := TControl(_parent) ;
  while frm <> nil do begin
    if frm is TForm then
      break ;
    frm := frm.Parent ;
  end;

  // create singleton component
  if frm is TForm then
    if not Assigned( TForm( _parent ).FindComponent( DPI_MANAGER ) ) then
      T_dpiManager.Create(TCustomForm(_parent),  _ppi, _ppifix, _canvasscale );
end;

constructor T_PvlContext.Create(
  const _parent     : TObject;
  const _context    : TGIS_PvlContext
) ;
begin
  Create( _parent, _context, nil ) ;
end;

constructor T_PvlContext.Create(
  const _parent     : TObject;
  const _context    : TGIS_PvlContext ;
  const _refcontext : TGIS_PvlContext
) ;
var
  ppi          : Integer ;
  systemppi    : Integer ;
  obehaviorsvc : IDeviceBehavior;
  met          : TDeviceDisplayMetrics;
  owidowscvs   : IFMXWindowService;
  prnt         : TFmxObject ;
  frm          : TFmxObject ;
begin
  frm := nil ;
  prnt := nil ;
  if _parent is TFmxObject then
    prnt := TFmxObject( _parent )
  else
  if _parent is TGIS_PvlBase then
    prnt := TFmxObject( TGIS_PvlBase( _parent ).NativeControl ) ;

  frm := prnt ;
  while Assigned( frm ) do begin
    if frm is TCustomForm then
      break ;
    frm := frm.Parent
  end;

  if Assigned( _refcontext ) then
    Create( prnt,
      _refcontext.PPI,
      _refcontext.PPIFix,
      _refcontext.CanvasScale,
      _refcontext.RightToLeft,
      _context
    )
  else begin
    if TBehaviorServices.Current.SupportsBehaviorService(
         IDeviceBehavior,
         obehaviorsvc, TFmxObject(_parent)
       )
    then begin
      met := obehaviorsvc.GetDisplayMetrics( frm ) ;
      ppi       :=  met.PixelsPerInch ;
      systemppi :=  RoundS( met.PixelsPerInch / met.ScreenScale ) ;
    end
    else begin
      ppi      := 96 ;
      systemppi:= 96 ;
    end;

    if TPlatformServices.Current.SupportsPlatformService(
         IFMXWindowService,
         owidowscvs
       )
    then begin
      //ppi := RoundS( owidowscvs.GetWindowScale( TCustomForm(frm) ) * systemppi ) ; //Deprecated
      ppi := RoundS( TCustomForm(frm).Handle.Scale * systemppi ) ;
    end ;

    Create( prnt,
      ppi,
       1,
      TCustomForm( frm ).Canvas.Scale,
      TCustomForm( frm ).BiDiMode = bdRightToLeft,
      _context
    );
  end;
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
  Result := Lider.CG.GIS.FMX.GeoSystemForms.TGIS_ControlCSSystem.Create( _parent, _self ) ;
end;

class function T_PvlContext.CreateLineSymbologyDialog(
  const _parent : TObject;
  const _self   : TGIS_PvlLineSymbolEditor
) : TGIS_PvlLineSymbolEditor ;
begin
  Result := Lider.CG.GIS.FMX.GeoSystemForms.TGIS_LineSymbolEditor.Create( _parent, _self ) ;
end;

class function T_PvlContext.CreatePrintPreviewSimpleDialog(
  const _parent : TObject;
  const _self   : TGIS_PvlControlPrintPreviewSimple
) : TGIS_PvlControlPrintPreviewSimple ;
begin
  Result := Lider.CG.GIS.FMX.GeoSystemForms.TGIS_ControlPrintPreviewSimple.Create( _parent, _self ) ;
end;

class function T_PvlContext.Support(
  const _parent  : TObject
) : Boolean ;
var
  o : TObject;
begin
  if _parent is TGIS_PvlBase then
    o := TGIS_PvlBase( _parent ).NativeControl
  else if _parent is TGIS_PvlContext then
    o := TGIS_PvlContext( _parent ).NativeParent
  else
    o := _parent;

  Result := o is TFmxObject;
end;

function T_PvlContext.getAllFonts
  : TStringList ;
begin
  Result := TStringList.Create ;
  GetFontList( Result ) ;
end;

function T_PvlContext.CreateObject(
  const _parent : TGIS_PvlBase;
  const _name   : String
) : IGIS_PvlBase;
var
  ctmp : TClass;
begin
  if oControlDict.TryGetValue( _name, ctmp ) then begin
    Result := TGIS_PvlBaseFmxClass( ctmp ).Create( self.oMaster, _parent );
  end
  else
    Assert( False );
end;

procedure T_PvlContext.FreeContext ;
begin
  // force freeing of context
  oOwner.Free ;
end;

function T_PvlContext.ClientWidth
  : Integer;
begin
  Result := 0 ;
  if oParent is TLayout then
    Result := RoundS( TLayout( oParent ).Width )
  else
  if oParent is TFrame then
    Result := RoundS( TFrame( oParent ).Width )
  else
  if oParent is TCustomForm then
    Result := RoundS( TCustomForm( oParent ).ClientWidth )
  else
    Assert( False ) ;
end;

function T_PvlContext.fget_HMargin
  : Integer;
begin
  Result := RoundS( iHMargin );
end;

procedure T_PvlContext.fset_HMargin(
  const _value: Integer
) ;
begin
  iHMargin := RoundS( _value ) ;
end;

function T_PvlContext.fget_VMargin
  : Integer;
begin
  Result := RoundS( iVMargin );
end;

procedure T_PvlContext.fset_VMargin(
  const _value: Integer
) ;
begin
  iVMargin := RoundS( _value  ) ;
end;

function T_PvlContext.fget_HSpace
  : Integer ;
begin
  Result := 4 {$IFDEF ANDROID}* 2{$ENDIF} ;
end;

function T_PvlContext.fget_VSpace
  : Integer ;
begin
  Result := 4 {$IFDEF ANDROID}* 2{$ENDIF} ;
end;

function T_PvlContext.fget_LSpace
  : Integer ;
begin
  Result := 2 ;
end;

function T_PvlContext.fget_CanvasScale
  : Single;
begin
  Result := oMaster.CanvasScale ; //?dCanvasScale;
end;

function T_PvlContext.fget_PPIFix
  : Single;
begin
  Result := oMaster.PPIFix ; //?dPPIFix;
end;

function T_PvlContext.fget_PPI
  : Integer;
begin
  Result := oMaster.PPI ; //?iPPI;
end;

{$ENDREGION 'T_PvlContext'}

procedure RegisterPVLPlatformControl(
  const _name : String;
  const  _class : TClass
);
begin
  if not Assigned( oControlDict ) then
    oControlDict := TDictionary< String, TClass>.Create;

  oControlDict.Add( _name, TGIS_PvlBaseFmxClass( _class ) );
end;

{$REGION 'Unit_FMX_GisPvl'}

class procedure Unit_FMX_GisPvl.SelfRegisterPVL;
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

{$ENDREGION 'Unit_FMX_GisPvl'}

var tmpKey : String ;

initialization
  TGIS_PvlContext.&Register( T_PvlContext ) ;
  oItemCache := TDictionary<String,TGIS_Bitmap>.Create  ;

  Unit_FMX_GisPvl.SelfRegisterPVL; // call simillar method in every PVL
                                    // platform impoemenation unit

finalization
  for tmpKey in oItemCache.Keys.ToArray do
    oItemCache.Items[tmpKey].Free ;
  FreeObject( oItemCache ) ;
  FreeObject(oControlDict);

//==================================== END =====================================
end.

