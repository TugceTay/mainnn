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
  Print template designer form.
}

unit VCL.GisPrintTemplateDesigner ;
{$HPPEMIT '#pragma link "VCL.GisPrintTemplateDesigner"'}

{$INCLUDE GisInclude.inc}

interface

uses
  Winapi.Windows,
  System.Classes,
  System.Generics.Collections,
  VCL.Controls,
  VCL.Graphics,
  VCL.Forms,
  VCL.Dialogs,
  VCL.StdCtrls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.Buttons,

  GisTypes,
  GisTypesUI,
  GisTemplatePrint,
  GisPrintUtils,
  GisPrintBuilder,
  VCL.GisModalForm,
  VCL.GisControlPrintPreview,
  VCL.GisPrintManager,
  VCL.GisValueValidatorHelper,
  VCL.GisControlPrintPositionForm,
  VCL.GisControlVarious,
  VCL.GisComboBoxHelper,
  VCL.GisViewerBmp ;

type

{$REGION 'T_ResizeType'}
  {#gendoc:hide}
  T_ResizeType = (
    rtTopLeft, rtTopMiddle, rtTopRight,
    rtMiddleLeft, rtCenter, rtMiddleRight,
    rtBottomLeft, rtBottomMiddle, rtBottomRight
  );
{$ENDREGION}

{$REGION 'T_ResizeElement'}
  T_ControlPrintTemplate = class ;

  {#gendoc:hide}
  /// <summary>
  ///   Event for changing element's location.
  /// </summary>
  /// <param name="_sender">
  ///   element to change
  /// </param>
  /// <param name="_left">
  ///   if change the left coordinate
  /// </param>
  /// <param name="_top">
  ///   if change the top coordinate
  /// </param>
  /// <param name="_right">
  ///   if change the right coordinate
  /// </param>
  /// <param name="_bottom">
  ///   if change the bottom coordinate
  /// </param>
  T_BoundsChangeEvent = procedure(
    _sender : TObject ;
    _left   : Boolean ;
    _top    : Boolean ;
    _right  : Boolean ;
    _bottom : Boolean
  ) of object ;

  {#gendoc:hide}
  /// <summary>
  ///   Event for changing element's location.
  /// </summary>
  /// <param name="_sender">
  ///   element to change
  /// </param>
  /// <param name="_left">
  ///   change of the left coordinate
  /// </param>
  /// <param name="_top">
  ///   change of the top coordinate
  /// </param>
  /// <param name="_right">
  ///   change of the right coordinate
  /// </param>
  /// <param name="_bottom">
  ///   change of the bottom coordinate
  /// </param>
  T_BoundsChangeExEvent = procedure(
    _sender : TObject ;
    _left   : Double ;
    _top    : Double ;
    _right  : Double ;
    _bottom : Double
  ) of object ;

  {#gendoc:hide}
  /// <summary>
  ///   Event for preparing map bitmaps to draw.
  /// </summary>
  /// <param name="_sender">
  ///   who raises the event
  /// </param>
  /// <param name="_element">
  ///   element to draw
  /// </param>
  /// <param name="_width">
  ///   width of the bitmap
  /// </param>
  /// <param name="_height">
  ///   height of the bitmap
  /// </param>
  /// <param name="_bitmap">
  ///   bitmap to draw
  /// </param>
  T_DrawMapEvent = procedure(
        _sender  : TObject ;
        _element : TGIS_PrintLayoutElement ;
        _width   : Integer ;
        _height  : Integer ;
    var _bitmap  : TBitmap
  ) of object ;

  {#gendoc:hide}
  /// <summary>
  ///   Event for preparing control bitmaps to draw.
  /// </summary>
  /// <param name="_sender">
  ///   who raises the event
  /// </param>
  /// <param name="_element">
  ///   element to draw
  /// </param>
  /// <param name="_bitmap">
  ///   bitmap to be drawn
  /// </param>
  T_DrawControlEvent = procedure(
    _sender  : TObject ;
    _element : TGIS_PrintLayoutElement ;
    _bitmap  : TGIS_Bitmap
  ) of object ;

  {#gendoc:hide}
  /// <summary>
  ///   Event for drawing graphics.
  /// </summary>
  /// <param name="_sender">
  ///   who raises the event
  /// </param>
  /// <param name="_element">
  ///   element to draw
  /// </param>
  /// <param name="_canvas">
  ///   canvas to draw on
  /// </param>
  /// <param name="_rect">
  ///   rectangle to draw in
  /// </param>
  /// <param name="_drawn">
  ///   True if a graphic has been drawn
  /// </param>
  T_DrawGraphicEvent = procedure(
        _sender  : TObject ;
        _element : TGIS_PrintLayoutElement ;
        _canvas  : TCanvas ;
        _rect    : TRect ;
    var _drawn   : Boolean
  ) of object ;

  {#gendoc:hide}
  /// <summary>
  ///   Encapsulate all graphical objects connected with the canvas.
  ///   The objects represent elements of print template.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  T_ResizeElement = class( TGraphicControl )
    private
      FCacheIndex         : Integer ;
      FCacheBitmap        : TBitmap ;
      FRealBoundsChange   : T_BoundsChangeEvent ;
      FRealBoundsChangeEx : T_BoundsChangeExEvent ;
      FClickEvent         : TNotifyEvent ;
      FDrawMapEvent       : T_DrawMapEvent ;
      FDrawControlEvent   : T_DrawControlEvent ;
      FDrawGraphicEvent   : T_DrawGraphicEvent ;
      FMouseClick         : TPoint ;
      FTemplate           : TGIS_TemplatePrint ;
      FParent             : T_ControlPrintTemplate ;
      FObject             : TObject ;

    private
      procedure setElementAtPos( const _x : Integer ;
                                 const _y : Integer
                               ) ;

    protected
      procedure Paint          ; override ;
      procedure MouseDown      ( _button : TMouseButton ;
                                 _shift  : TShiftState ;
                                 _x, _y  : Integer
                               ) ; override ;

    public
      UserData : TGIS_PrintLayoutElement ;

      constructor Create        ( _owner : TComponent
                                ) ; override ;
      destructor  Destroy       ; override;
      procedure ResetMouseClickPos ;
      procedure SetMouseClickPos( _x : Integer ;
                                  _y : Integer
                                ) ;
      procedure InvalidateCache ;

    public
      property ControlParent  : T_ControlPrintTemplate
                                         read FParent      write FParent ;
      property Template       : TGIS_TemplatePrint
                                         read FTemplate    write FTemplate ;
      property MouseClickPos  : TPoint   read FMouseClick ;
      property ElementObject  : TObject  read FObject      write FObject ;

    public
      /// <event/>
      property RealBoundsChangeEvent : T_BoundsChangeEvent
                                                      read  FRealBoundsChange
                                                      write FRealBoundsChange ;
      /// <event/>
      property RealBoundsChangeExEvent
                                     : T_BoundsChangeExEvent
                                                      read  FRealBoundsChangeEx
                                                      write FRealBoundsChangeEx ;
      /// <event/>
      property DrawMapEvent          : T_DrawMapEvent read  FDrawMapEvent
                                                      write FDrawMapEvent ;
      /// <event/>
      property DrawControlEvent      : T_DrawControlEvent
                                                      read  FDrawControlEvent
                                                      write FDrawControlEvent ;
      /// <event/>
      property DrawGraphicEvent      : T_DrawGraphicEvent
                                                      read  FDrawGraphicEvent
                                                      write FDrawGraphicEvent ;
      /// <event/>
      property ClickEvent            : TNotifyEvent   read  FClickEvent
                                                      write FClickEvent ;
  end ;
{$ENDREGION}

{$REGION 'T_FocusControl'}
  {#gendoc:hide}
  /// <summary>
  ///   Encapsulate a graphical object representing focus.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  T_FocusControl = class ( TGraphicControl )
    private
      FResizeElement : T_ResizeElement ;
      FOrigin        : TPoint ;
      FIsResizing    : Boolean ;
      FResizeType    : T_ResizeType ;

    private
      procedure fset_ResizeElement ( const _value : T_ResizeElement ) ;
      procedure fset_ResizeType    ( const _value : T_ResizeType    ) ;

    private
      procedure resetResizeType    ;
      function  setNextElementAtPos( const _x : Integer ;
                                     const _y : Integer
                                   ) : Boolean ;
      procedure doMouseLeave       ( _sender : TObject
                                   ) ;

    private
      mouseMoved          : Boolean ;
      mouseMovedOldCursor : TCursor ;
      last_pos            : TPoint ;

    protected
      procedure MouseDown ( _button : TMouseButton ;
                            _shift  : TShiftState ;
                            _x, _y  : Integer
                          ) ; override ;
      procedure MouseMove ( _shift : TShiftState ;
                            _x, _y : Integer
                          ) ; override ;
      procedure MouseUp   ( _button : TMouseButton ;
                            _shift : TShiftState ;
                            _x, _y : Integer
                          ) ; override ;
      procedure Paint     ; override;

    public

      constructor Create  ( _owner : TComponent
                          ) ; override ;
      /// <summary>
      ///   Move focus element one pixel left left.
      /// </summary>
      procedure MoveLeft ;

      /// <summary>
      ///   Move focus element one pixel left right.
      /// </summary>
      procedure MoveRight ;

      /// <summary>
      ///   Move focus element one pixel left up.
      /// </summary>
      procedure MoveUp ;

      /// <summary>
      ///   Move focus element one pixel left down.
      /// </summary>
      procedure MoveDown ;

    public
      property ResizeElement : T_ResizeElement read  FResizeElement
                                               write fset_ResizeElement ;
  end ;
{$ENDREGION}

{$REGION 'T_TemplatePanel'}
  {#gendoc:hide}
  /// <summary>
  ///   Panel for drawing a print template.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  T_TemplatePanel = class ( TPanel )
    private
      FParent : T_ControlPrintTemplate ;
    public
      property ControlParent : T_ControlPrintTemplate  read FParent write FParent ;
  end ;
{$ENDREGION}

{$REGION 'T_ControlPrintTemplate'}
  {#gendoc:hide}
  /// <summary>
  ///   Control that represents a print template.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  T_ControlPrintTemplate = class( TCustomPanel )
    private  // control components
      pnlPage      : T_TemplatePanel ;
      imgGrid      : TImage ;
      pnlShadow    : TPanel ;

    private
      FElementList     : TList<T_ResizeElement> ;
      FFocusControl    : T_FocusControl ;
      FPageSize        : TPoint  ;
      FPrintArea       : TRect   ;
      FPPI             : Integer ;
      FCreatingElement : Boolean ;
      FCreatingElementHandling : Boolean ;
      FSizeRatio       : Double  ;
      FCurrentElement  : T_ResizeElement ;
      FOnCreateShape   : TNotifyEvent ;
      FOnAddShape      : TNotifyEvent ;
      FOnCancelShape   : TNotifyEvent ;

    private
      procedure fset_CreatingElement
                              ( const _value : Boolean
                              ) ;
      function  fget_Element  (       _index : Integer
                              ) : T_ResizeElement ;
      procedure fset_SizeRatio( const _value : Double
                              ) ;

    private
      procedure doMouseDown ( _sender : TObject ;
                              _button : TMouseButton ;
                              _shift  : TShiftState ;
                              _x, _y  : Integer
                            ) ;
      procedure doMouseUp   ( _sender : TObject ;
                              _button : TMouseButton ;
                              _shift  : TShiftState ;
                              _x, _y  : Integer
                            ) ;
      procedure doMouseMove ( _sender : TObject ;
                              _shift  : TShiftState ;
                              _x, _y  : Integer
                            ) ;
      procedure updateGrid  ( _left_margin   : Integer ;
                              _top_margin    : Integer ;
                              _right_margin  : Integer ;
                              _bottom_margin : Integer
                            ) ;

    protected
      procedure Resize      ; override;
      procedure MouseDown   ( _button : TMouseButton ;
                              _shift  : TShiftState ;
                              _x, _y  : Integer
                            ) ; override ;

    public

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_owner">
      ///   parent element
      /// </param>
      constructor Create    ( _owner : TComponent
                            ) ; override;

      /// <summary>
      ///   Destroy dataset.
      /// </summary>
      destructor  Destroy   ; override;

      /// <summary>
      ///   Set printer parameters for the model.
      /// </summary>
      /// <param name="_pageSize">
      ///   page size
      /// </param>
      /// <param name="_printArea">
      ///   print area
      /// </param>
      /// <param name="_ppi">
      ///   resolution
      /// </param>
      procedure SetPrinter       ( const _pageSize  : TPoint ;
                                   const _printArea : TRect ;
                                   const _ppi       : Integer
                                 ) ;

      /// <summary>
      ///   Set focused element.
      /// </summary>
      /// <param name="_idx">
      ///   index of the element to be focused
      /// </param>
      procedure SetFocused       ( const _idx : Integer
                                 ) ;

      /// <summary>
      ///   Check if element is focused.
      /// </summary>
      /// <param name="_idx">
      ///   index of the element to be checked
      /// </param>
      /// <returns>
      ///   True if focused
      /// </returns>
      function  IsFocused        ( const _idx : Integer
                                 ) : Boolean ;

      /// <summary>
      ///   Create a new element and add it to the internal list.
      /// </summary>
      /// <returns>
      ///   new element
      /// </returns>
      function  AddElement       : T_ResizeElement ;

      /// <summary>
      ///   Delete element and remove it from the internal list.
      /// </summary>
      /// <param name="_idx">
      ///   index of the element to be deleted
      /// </param>
      procedure DeleteElement    ( const _idx : Integer ) ;

      /// <summary>
      ///   Move element one position up in the internal list.
      /// </summary>
      /// <param name="_idx">
      ///   index of the element to be moved
      /// </param>
      procedure MoveElementUp    ( const _idx : Integer ) ;

      /// <summary>
      ///   Move element one position down in the internal list.
      /// </summary>
      /// <param name="_idx">
      ///   index of the element to be moved
      /// </param>
      procedure MoveElementDown  ( const _idx : Integer ) ;

      /// <summary>
      ///   Move element to the back in the internal list.
      /// </summary>
      /// <param name="_idx">
      ///   index of the element to be moved
      /// </param>
      procedure MoveElementBack   ( const _idx : Integer ) ;

      /// <summary>
      ///   Move element to the front in the internal list.
      /// </summary>
      /// <param name="_idx">
      ///   index of the element to be moved
      /// </param>
      procedure MoveElementFront   ( const _idx : Integer ) ;

      /// <summary>
      ///   Delete all elements.
      /// </summary>
      procedure ClearElements    ;

      /// <summary>
      ///   Redraw all elements.
      /// </summary>
      procedure InvalidateElements ;

      /// <summary>
      ///   Set Enable = True for all elements.
      /// </summary>
      procedure EnableElements   ;

      /// <summary>
      ///   Set Enable = False for all elements.
      /// </summary>
      procedure DisableElements  ;

      /// <summary>
      ///   Return element's position in the internal list.
      /// </summary>
      /// <param name="_element">
      ///   element to be searched
      /// </param>
      /// <returns>
      ///   index of the element
      /// </returns>
      function  IndexOf          ( const _element : T_ResizeElement
                                 ) : Integer ;

      /// <summary>
      ///   Return an element at given position.
      /// </summary>
      /// <param name="_x">
      ///   x
      /// </param>
      /// <param name="_y">
      ///   y
      /// </param>
      function  ElementAtPos     ( const _x       : Integer ;
                                   const _y       : Integer
                                 ) : T_ResizeElement ;

      /// <summary>
      ///   Apply new bounds to element.
      /// </summary>
      /// <param name="_element">
      ///   element to be affected
      /// </param>
      /// <param name="_rect">
      ///   new bounds
      /// </param>
      procedure ApplyBounds      ( const _element : T_ResizeElement ;
                                   const _rect    : TRect
                                 ) ;

      /// <summary>
      ///   Move focus element one pixel left left.
      /// </summary>
      procedure MoveFocusElementLeft ;

      /// <summary>
      ///   Move focus element one pixel left right.
      /// </summary>
      procedure MoveFocusElementRight ;

      /// <summary>
      ///   Move focus element one pixel left up.
      /// </summary>
      procedure MoveFocusElementUp ;

      /// <summary>
      ///   Move focus element one pixel left down.
      /// </summary>
      procedure MoveFocusElementDown ;

      /// <summary>
      ///   Ratio: control size / page size .
      /// </summary>
      property SizeRatio         : Double  read  FSizeRatio
                                           write fset_SizeRatio;

      /// <summary>
      ///   Flag indicating that a new element is just created.
      /// </summary>
      property CreatingElement   : Boolean read  FCreatingElement
                                           write fset_CreatingElement ;

      /// <summary>
      ///   List of elements.
      /// </summary>
      property Elements[_index : Integer]
                                 : T_ResizeElement
                                           read  fget_Element ;

      /// <summary>
      ///   Focus control.
      /// </summary>
      property FocusControl      : T_FocusControl
                                           read  FFocusControl ;

      /// <event/>
      /// <summary>
      ///   CreateShape event. Will be fired when a new element just has been created.
      /// </summary>
      property CreateShapeEvent  : TNotifyEvent
                                           read  FOnCreateShape
                                           write FOnCreateShape ;

      /// <event/>
      /// <summary>
      ///   AddShape event. Will be fired when a new element just has been added.
      /// </summary>
      property AddShapeEvent     : TNotifyEvent
                                           read  FOnAddShape
                                           write FOnAddShape ;

      /// <event/>
      /// <summary>
      ///   CancelShape event. Will be fired when adding a new element has been canceled.
      /// </summary>
      property CancelShapeEvent  : TNotifyEvent
                                           read  FOnCancelShape
                                           write FOnCancelShape ;
  end;
{$ENDREGION}

{$REGION 'T_TemplateObjectInspector'}
  {#gendoc:hide}
  /// <summary>
  ///   Element event.
  /// </summary>
  /// <param name="_sender">
  ///   who raises the event
  /// </param>
  /// <param name="_element">
  ///   element to process
  /// </param>
  T_ElementEvent = procedure(
    _sender  : TObject ;
    _element : TGIS_PrintLayoutElement
  ) of object ;

  {#gendoc:hide}
  T_PathButton = class( TButton )
    Edit : TEdit ;
  end;

  {#gendoc:hide}
  /// <summary>
  ///   Control that represents properties of an element.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  T_TemplateObjectInspector = class( TScrollBox )
    private
      FOnInit     : T_ElementEvent ;
      FOnChange   : TNotifyEvent ;
      FOnClick    : TNotifyEvent ;
      FOnKeyPress : TKeyPressEvent ;
      FOnEnter    : TNotifyEvent ;
      FOnExit     : TNotifyEvent ;
      FOnCustom   : TGIS_ComboBoxHelperCustomEvent ;

    private
      anchorCache : TGIS_AnchorCache ;

    private
      procedure clearFields    ;
      procedure doMouseMove    ( _sender  : TObject ;
                                 _shift   : TShiftState ;
                                 _x, _y   : Integer
                               ) ;
      procedure doResize       ( _sender  : TObject
                               ) ; dynamic ;
      procedure cmbDrawItem    ( _control : TWinControl ;
                                 _idx     : Integer ;
                                 _rect    : TRect   ;
                                 _state   : TOwnerDrawState
                               ) ;
    public
      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create       ( _owner   : TComponent
                               ) ; override ;

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      destructor Destroy       ; override ;

      /// <summary>
      ///   Clear the control.
      /// </summary>
      procedure Clear      ;

      /// <summary>
      ///   Show an element; display its properties.
      /// </summary>
      /// <param name="_element">
      ///   element to be showned
      /// </param>
      procedure ShowElement( _element : TGIS_PrintLayoutElement ) ;

      /// <summary>
      ///   find a control in object inspector
      /// </summary>
      /// <param name="_attribute">
      ///   control name
      /// </param>
      /// <returns>
      ///   Found control or nil
      /// </returns>
      function  FindAttribute( _attribute : String
                             ) : TControl ;

      /// <summary>
      ///   Set visibility of the attribute.
      /// </summary>
      /// <param name="_attribute">
      ///   attribute to set
      /// </param>
      /// <param name="_visible">
      ///   what to do
      /// </param>
      procedure VisibleAttribute( _attribute : String ;
                                  _visible   : Boolean
                                ) ;

      /// <summary>
      ///   Enable/Disable attribute.
      /// </summary>
      /// <param name="_attribute">
      ///   attribute to set
      /// </param>
      /// <param name="_enable">
      ///   what to do
      /// </param>
      procedure EnableAttribute( _attribute : String ;
                                 _enable    : Boolean
                               ) ;

      /// <summary>
      ///   Check if the control or any of child controls is ActiveControl.
      /// </summary>
      /// <param name="_activeControl">
      ///   active control of the form
      /// </param>
      /// <returns>
      ///   True if any child control is active control
      /// </returns>
      function HasActiveControl( _activeControl : TControl
                               ) : Boolean ;

    public
      /// <event/>
      /// <summary>
      ///   Init event. Will be fired when a field is created.
      /// </summary>
      property InitEvent : T_ElementEvent   read  FOnInit
                                            write FOnInit ;

      /// <event/>
      /// <summary>
      ///   Change event. Will be fired when any of properties is changed.
      /// </summary>
      property ChangeEvent : TNotifyEvent   read  FOnChange
                                            write FOnChange ;

      /// <event/>
      /// <summary>
      ///   Click event. Will be fired when some of edit fields is clicked.
      /// </summary>
      property ClickEvent : TNotifyEvent    read  FOnClick
                                            write FOnClick ;

      /// <event/>
      /// <summary>
      ///   Key Press event.
      /// </summary>
      property KeyPressEvent : TKeyPressEvent
                                            read  FOnKeyPress
                                            write FOnKeyPress ;

      /// <event/>
      /// <summary>
      ///   Enter event.
      /// </summary>
      property EnterEvent : TNotifyEvent    read  FOnEnter
                                            write FOnEnter ;

      /// <event/>
      /// <summary>
      ///   Exit event.
      /// </summary>
      property ExitEvent : TNotifyEvent     read  FOnExit
                                            write FOnExit ;

      /// <event/>
      /// <summary>
      ///   Custom event. Will be attached to CustomEvent of TGIS_ColorComboBox
      ///   and TGIS_SizeComboBox fields.
      /// </summary>
      property CustomEvent : TGIS_ComboBoxHelperCustomEvent
                                            read  FOnCustom
                                            write FOnCustom ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_ControlPrintTemplateDesignerForm'}
  {#gendoc:hide}
  /// <summary>
  ///   Printer parameters.
  /// </summary>
  T_CurrentPrinter = record
    Name          : String ;
    PageSize      : TPoint ;
    PrintArea     : TRect  ;
    PPI           : Integer ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Default values for legend.
  /// </summary>
  T_DefaultLegend = record
    CompactView   : Boolean ;
    DrawIconStyle : TGIS_LegendIconStyle ;
    ReverseOrder  : Boolean ;
    Font          : String ;
    FontSize      : Integer ;
    FontColor     : TGIS_Color ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Default values for scale.
  /// </summary>
  T_DefaultScale = record
    Dividers      : Integer;
    DividerColor1 : TGIS_Color ;
    DividerColor2 : TGIS_Color ;
    Font          : String ;
    FontSize      : Integer ;
    FontColor     : TGIS_Color ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   Default values for north arrow.
  /// </summary>
  T_DefaultNorthArrow = record
    Style  : TGIS_ControlNorthArrowStyle ;
    Color1 : TGIS_Color ;
    Color2 : TGIS_Color ;
    Path   : String ;
  end ;

  {#gendoc:hide}
  /// <summary>
  ///   New element.
  /// </summary>
  T_NewElement = record
    EType   : TGIS_PrintLayoutElementType ;
    Created : Boolean ;
  end ;

  /// <summary>
  ///   Print template designer for VCL.
  ///   Use class TGIS_ControlPrintTemplateDesignerForm.ShowPrintTemplateDesigner()
  ///   to call this form directly.
  /// </summary>
  /// <remarks>
  ///   <para>
  ///     If creating an inheriting class is needed,
  ///     take a look at our TemplatePrint sample.
  ///     There is an equivalent call there.
  ///   </para>
  /// </remarks>
  TGIS_ControlPrintTemplateDesignerForm = class( TGIS_ModalForm )
    private
      imgList        : TImageList ;
      imgListScld    : TImageList ;

      pnlTop         : TPanel ;
      cmbPrinters    : TComboBox ;
      chkLandscape   : TCheckBox ;
      chkWireframe   : TCheckBox ;
      btnPreview     : TButton ;

      pnlBottom      : TPanel ;
      btnNew         : TButton ;
      btnOpen        : TButton ;
      btnSaveAs      : TButton ;
      lblProgress    : TLabel ;

      dlgOpen        : TOpenDialog ;
      dlgSave        : TSaveDialog ;

      pnlPageControl : TPanel ;
      pageControl    : TPageControl ;
      tabModel       : TTabSheet ;
      gisModel       : T_ControlPrintTemplate ;
      tabPreview     : TTabSheet ;
      gisPreview     : TGIS_ControlPrintPreviewTemplateDesigner ;

      splitter       : TSplitter ;
      pnlRight       : TPanel ;
      pnlRightRight  : TPanel ;
      pnlRightClient : TPanel ;

      pnlStructure     : TPanel ;
      pnlStructureTop  : TPanel ;
      lblStructure     : TLabel ;
      btnStructureUp   : TSpeedButton ;
      btnStructureDown : TSpeedButton ;
      btnStructureBack : TSpeedButton ;
      btnStructureFront: TSpeedButton ;
      btnStructureDel  : TSpeedButton ;
      btnStructureWnd  : TSpeedButton ;
      tvStructure      : TTreeView ;

      pnlObjectInspector     : TPanel ;
      pnlObjectInspectorBg   : TPanel ;
      lblObjectInspector     : TLabel ;
      lblObjectInspectorSnap : TLabel ;
      cmbObjectInspectorSnap : TGIS_SizeComboBox ;
      cmbObjectInspector     : TComboBox ;
      tplObjectInspector     : T_TemplateObjectInspector ;

      pnlToolPalette       : TPanel ;
      pnlToolPaletteTop    : TPanel ;
      lblToolPalette       : TLabel ;
      tvToolPalette        : TTreeView ;
      pnlToolPaletteBottom : TPanel ;

      tmrShowForm     : TTimer ;
      tmrLandscape    : TTimer ;
      tmrPaint        : TTimer ;

    private
      FWorkingFolder  : String ;
      FTemplateName   : String ;
      FTemplate       : TGIS_TemplatePrint ;
      FTemplateOrig   : TGIS_TemplatePrint ;
      FViewerTiledPaints : array of Boolean ;
      FViewers        : array of TGIS_ViewerBmp ;
      FViewersEx      : array of TGIS_ViewerBmp ;
      FBuilder        : TGIS_TemplatePrintBuilder ;
      FPrintManager   : TGIS_PrintManager ;
      FPrinter        : T_CurrentPrinter ;
      FCustomPrinterSettings : TGIS_CustomPrinterSettings ;
      FNewElement     : T_NewElement ;
      FDefaultLegend     : T_DefaultLegend ;
      FDefaultScale      : T_DefaultScale ;
      FDefaultNorthArrow : T_DefaultNorthArrow ;

    private
      formResult      : Integer ;
      viewersCreated  : Boolean ;
      extentChanged   : Boolean ;
      oldViewerExtent : TGIS_Extent ;

    protected
      function  fget_Snap      : String ;
      procedure fset_Snap      ( const _value   : String
                               ) ;

    private
      procedure setWorkingFolder
                               ( _folder        : String ) ;
      procedure finishChangingExtent ;
      procedure setTemplateName( _name          : String ) ;
      function  resolvePrinter ( _custom        : String ) : String ;
      procedure setPrinter     ( _printer       : String ;
                                 _setLandscape  : Boolean
                               ) ;
      procedure initDesigner   ( _template      : TGIS_TemplatePrint ;
                                 _workingFolder : String
                               ) ;
      procedure updateInputTemplate
                               ( _template      : TGIS_TemplatePrint
                               ) ;
      procedure cleanupDesigner ;
      procedure prepare_default_values ;
      procedure openTemplate   ;
      procedure update_values  ( _element      : TGIS_PrintLayoutElement
                               ) ;
      procedure doShowForm     ( _sender       : TObject
                               ) ;
      procedure doLandscape    ( _sender       : TObject
                               ) ;
      procedure doPaint        ( _sender       : TObject
                               ) ;
      procedure initElement    ( const _type   : TGIS_PrintLayoutElementType
                               ) ;
      procedure updateElements ;
      procedure updateElementsEx;
      procedure updateElementsWithDelay ;
      procedure updateElementsLandscape ;
      procedure updateElementsWireFrame ;
      function  getElementRectangle
                               ( const _shp    : T_ResizeElement
                               ) : TRect ; overload ;
      function  createUniqueName
                               ( _name         : String
                               ) : String ;
      procedure doCreateShape  ( _sender       : TObject
                               ) ;
      procedure doAddShape     ( _sender       : TObject
                               ) ;
      procedure doCancelShape  ( _sender       : TObject
                               ) ;
      procedure doClickShape   ( _sender       : TObject
                               ) ;
      procedure doDrawMap      ( _sender       : TObject ;
                                 _element      : TGIS_PrintLayoutElement ;
                                 _width        : Integer ;
                                 _height       : Integer ;
                                 var _bitmap   : TBitmap
                               ) ;
      procedure doDrawControl  ( _sender       : TObject ;
                                 _element      : TGIS_PrintLayoutElement ;
                                 _bitmap       : TGIS_Bitmap
                               ) ;
      procedure doDrawGraphic  ( _sender       : TObject ;
                                 _element      : TGIS_PrintLayoutElement ;
                                 _canvas       : TCanvas ;
                                 _rect         : TRect   ;
                                 var _drawn    : Boolean
                               ) ;
      procedure doRealBoundsChange
                               ( _sender       : TObject ;
                                 _left         : Boolean ;
                                 _top          : Boolean ;
                                 _right        : Boolean ;
                                 _bottom       : Boolean
                               ) ;
      procedure doRealBoundsChangeEx
                               ( _sender       : TObject ;
                                 _left         : Double  ;
                                 _top          : Double  ;
                                 _right        : Double  ;
                                 _bottom       : Double
                               ) ;
      procedure addElementToObjectInspector
                               ( _element      : TGIS_PrintLayoutElement
                               ) ;
      procedure setElementInObjectInspector
                               ( _idx          : Integer
                               ) ;
      procedure deleteElementFromObjectInspector
                               ( _idx          : Integer
                               ) ;
      procedure updateGraphicsPaths
                               ( _path         : String ;
                                 _update       : Boolean
                               ) ;
      procedure prepare_template_bmp
                               ( _idx          : Integer ;
                                 _mode         : Boolean
                               ) ;
      procedure recoverValues  ;
      procedure showPreview    ;
      function  saveTemplate   : Boolean ;
      procedure doBusy         (_sender: TObject; _pos, _end: Integer; var _abort: Boolean);
    private
      procedure btnNewClick        ( _sender       : TObject
                                   ) ;
      procedure btnOpenClick       ( _sender       : TObject
                                   ) ;
      procedure btnSaveAsClick     ( _sender       : TObject
                                   ) ;

      procedure cmbPrintersInit    ( _selected     : String ) ;
      procedure cmbPrintersUpdate  ( _printer      : String ) ;
      procedure cmbPrintersChange  ( _sender       : TObject
                                   ) ;
      procedure chkLandscapeClick  ( _sender       : TObject
                                   ) ;
      procedure chkWireframeClick  ( _sender       : TObject
                                   ) ;
      procedure btnPreviewMouseDown( _sender       : TObject ;
                                     _button       : TMouseButton ;
                                     _shift        : TShiftState ;
                                     _x, _y        : Integer
                                   ) ;
      procedure btnPreviewMouseUp  ( _sender       : TObject ;
                                     _button       : TMouseButton ;
                                     _shift        : TShiftState ;
                                     _x, _y        : Integer
                                   ) ;

      procedure FormCloseQuery     ( _sender       : TObject ;
                                     var _canClose : Boolean
                                   ) ;
      procedure FormDestroy        ( _sender       : TObject
                                   ) ;

      procedure splitterMoved      ( _sender       : TObject
                                   ) ;
      procedure pageControlChange  ( _sender       : TObject
                                   ) ;
      procedure btnStructureDownClick
                                   ( _sender       : TObject
                                   ) ;
      procedure btnStructureUpClick( _sender       : TObject
                                   ) ;
      procedure btnStructureBackClick
                                   ( _sender       : TObject
                                   ) ;
      procedure btnStructureFrontClick
                                   ( _sender       : TObject
                                   ) ;
      procedure btnStructureDelClick
                                   ( _sender       : TObject
                                   ) ;
      procedure btnStructureWndClick
                                   ( _sender       : TObject
                                   ) ;
      procedure gisExtentChanged   ( _sender       : TObject
                                   ) ;
      procedure gisMouseWheelUp    (     _sender   : TObject ;
                                         _shift    : TShiftState ;
                                         _mousePos : TPoint ;
                                     var _handled  : Boolean
                                   ) ;
      procedure gisMouseWheelDown  (     _sender   : TObject ;
                                         _shift    : TShiftState ;
                                         _mousePos : TPoint ;
                                     var _handled  : Boolean
                                   ) ;
      procedure tvStructureChange  ( _sender       : TObject ;
                                     _node         : TTreeNode
                                   ) ;
      procedure cmbObjectInspectorSnapChange
                                   ( _sender       : TObject
                                   ) ;
      function  cmbObjectInspectorSnapCustom
                                   ( _sender       : TObject ;
                                     _value        : String
                                   ) : String ;
      procedure cmbObjectInspectorChange
                                   ( _sender       : TObject
                                   ) ;
      procedure tplObjectInspectorInit
                                   ( _sender       : TObject ;
                                     _element      : TGIS_PrintLayoutElement
                                   ) ;
      procedure tplObjectInspectorChange
                                   ( _sender       : TObject
                                   ) ;
      procedure tplObjectInspectorClick
                                   ( _sender       : TObject
                                   ) ;
      procedure tplObjectInspectorKeyPress
                                   ( _sender       : TObject ;
                                     var _key      : Char
                                   ) ;
      procedure tplObjectInspectorEnter
                                   ( _sender       : TObject
                                   ) ;
      procedure tplObjectInspectorExit
                                   ( _sender       : TObject
                                   ) ;
      function  tplObjectInspectorCustom
                                   ( _sender       : TObject ;
                                     _value        : String
                                   ) : String ;
      procedure tvToolPaletteMouseDown
                                   ( _sender       : TObject ;
                                     _button       : TMouseButton ;
                                     _shift        : TShiftState ;
                                     _x, _y        : Integer
                                   ) ;

    protected

      /// <inheritdoc/>
      procedure btnOKClick         ( _sender       : TObject
                                   ) ; override ;
      /// <inheritdoc/>
      procedure btnCancelClick     ( _sender       : TObject
                                   ) ; override ;
      /// <inheritdoc/>
      procedure initForm           ; override ;

      /// <inheritdoc/>
      procedure initControls       ; override ;

    protected

      /// <inheritdoc/>
      procedure Paint     ; override ;

      /// <inheritdoc/>
      procedure KeyDown   ( var _key   : Word ;
                                _shift : TShiftState
                          ) ; override ;

    public

      /// <inheritdoc/>
      constructor Create  ( _owner     : TComponent
                          ) ; overload ; override ;

      /// <inheritdoc/>
      constructor Create  ( _owner     : TComponent ;
                            _sizeable  : Boolean
                          ) ; overload ; override ;

      /// <summary>
      ///   Execute dialog.
      /// </summary>
      /// <param name="_template">
      ///   print template to be processed
      /// </param>
      /// <param name="_workingFolder">
      ///   default folder where templates are saved;
      ///   if empty string then current directory is taken into account
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function Execute    ( _template      : TGIS_TemplatePrint ;
                            _workingFolder : String
                          ) : Integer ; overload ;

      /// <summary>
      ///   Execute dialog.
      /// </summary>
      /// <param name="_template">
      ///   print template to be processed
      /// </param>
      /// <param name="_workingFolder">
      ///   default folder where templates are saved;
      ///   if empty string then current directory is taken into account
      /// </param>
      /// <param name="_customPage">
      ///   default page format set when the designer starts
      ///   examples: 'Landscape', 'A0', 'A3;Landscape',
      ///             'Microsoft Print to PDF' ,
      ///             'Microsoft Print to PDF;Landscape',
      ///             '5000px;7000px'.
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      function Execute    ( _template      : TGIS_TemplatePrint ;
                            _workingFolder : String ;
                            _customPage    : String
                          ) : Integer ; overload ;

      /// <summary>
      ///   Show print template designer dialog.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     It is not recommended to use the method
      ///     for inheriting classes.
      ///     Then take a look at our TemplatePrint sample.
      ///     There is an equivalent call there.
      ///   </para>
      /// </remarks>
      /// <param name="_template">
      ///   print template to be processed
      /// </param>
      /// <param name="_workingFolder">
      ///   default folder where templates are saved;
      ///   if empty string then current directory is taken into account
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      class function ShowPrintTemplateDesigner
                          ( _template      : TGIS_TemplatePrint ;
                            _workingFolder : String
                          ) : Integer ; overload ;

      /// <summary>
      ///   Show print template designer dialog.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     It is not recommended to use the method
      ///     for inheriting classes.
      ///     Then take a look at our TemplatePrint sample.
      ///     There is an equivalent call there.
      ///   </para>
      /// </remarks>
      /// <param name="_template">
      ///   print template to be processed
      /// </param>
      /// <param name="_workingFolder">
      ///   default folder where templates are saved;
      ///   if empty string then current directory is taken into account
      /// </param>
      /// <param name="_customPage">
      ///   default page format set when the designer starts
      ///   examples: 'Landscape', 'A0', 'A3;Landscape',
      ///             'Microsoft Print to PDF' ,
      ///             'Microsoft Print to PDF;Landscape',
      ///             '5000px;7000px'.
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      class function ShowPrintTemplateDesigner
                          ( _template      : TGIS_TemplatePrint ;
                            _workingFolder : String ;
                            _customPage    : String
                          ) : Integer ; overload ;

      /// <summary>
      ///   Show print template designer dialog.
      /// </summary>
      /// <remarks>
      ///   <para>
      ///     It is not recommended to use the method
      ///     for inheriting classes.
      ///     Then take a look at our TemplatePrint sample.
      ///     There is an equivalent call there.
      ///   </para>
      /// </remarks>
      /// <param name="_template">
      ///   print template to be processed
      /// </param>
      /// <param name="_workingFolder">
      ///   default folder where templates are saved;
      ///   if empty string then current directory is taken into account
      /// </param>
      /// <param name="_customPage">
      ///   default page format set when the designer starts
      ///   examples: 'Landscape', 'A0', 'A3;Landscape',
      ///             'Microsoft Print to PDF' ,
      ///             'Microsoft Print to PDF;Landscape',
      ///             '5000px;7000px'.
      /// </param>
      /// <param name="_snap">
      ///   the value applies when adding and moving elements
      ///   0-value or empty string means no snapping
      ///   examples: '1mm', '0.05cm', '2px', '0.01in', '1pt'
      /// </param>
      /// <returns>
      ///   Modal result.
      /// </returns>
      class function ShowPrintTemplateDesigner
                          ( _template      : TGIS_TemplatePrint ;
                            _workingFolder : String ;
                            _customPage    : String ;
                            _snap          : String
                          ) : Integer ; overload ;

    public

      /// <summary>
      ///   Applies when adding and moving elements
      ///   0-value or empty string means no snapping
      ///   examples: '1mm', '0.05cm', '2px', '0.01in', '1pt'
      /// </summary>
      property Snap : String read  fget_Snap
                             write fset_Snap ;

  end ;
{$ENDREGION}

{$REGION 'GisShowPrintTemplateDesigner'}
  /// <summary>
  ///   Show print template designer dialog.
  /// </summary>
  /// <param name="_template">
  ///   print template to be processed
  /// </param>
  /// <param name="_workingFolder">
  ///   default folder where templates are saved;
  ///   if empty string then current directory is taken into account
  /// </param>
  /// <returns>
  ///   Value returned by ShowModal function.
  /// </returns>
  function GisShowPrintTemplateDesigner( _template      : TGIS_TemplatePrint ;
                                         _workingFolder : String
                                       ) : Integer ; overload ;
                                       {$IFNDEF GENDOC} deprecated ; {$ENDIF}

  /// <summary>
  ///   Show print template designer dialog.
  /// </summary>
  /// <param name="_template">
  ///   print template to be processed
  /// </param>
  /// <param name="_workingFolder">
  ///   default folder where templates are saved;
  ///   if empty string then current directory is taken into account
  /// </param>
  /// <param name="_customPage">
  ///   default page format set when the designer starts
  ///   examples: 'Landscape', 'A0', 'A3;Landscape',
  ///             'Microsoft Print to PDF' ,
  ///             'Microsoft Print to PDF;Landscape',
  ///             '5000px;7000px'.
  /// </param>
  /// <returns>
  ///   Value returned by ShowModal function.
  /// </returns>
  function GisShowPrintTemplateDesigner( _template      : TGIS_TemplatePrint ;
                                         _workingFolder : String ;
                                         _customPage    : String
                                       ) : Integer ; overload ;
                                       {$IFNDEF GENDOC} deprecated ; {$ENDIF}
{$ENDREGION}

//##############################################################################
implementation

{$R GisControlPrintTemplateDesigner_16x16.RES}

uses
  Winapi.Messages,
  System.SysUtils,
  System.UITypes,
  System.Types,
  Math,
  VCL.Printers,
  VCL.ImgList,

  GisRtl,
  GisParams,
  GisResource,
  GisInternals,
  GisFunctions,
  GisInterfaces,
  GisLayer,
  GisLayerVector,
  GisLayerBmp,
  GisUtils,
  GisPixelExportManager,
  GisSymbol,
  GisRendererAbstract,
  VCL.GisControlHelper,
  VCL.GisFramework,
  VCL.GisControlColor,
  PVL.GisControlSizeForm,
  PVL.GisPvl,
  VCL.GisControlFontStyleForm,
  VCL.GisControlLegend,
  VCL.GisControlScale,
  VCL.GisControlNorthArrow,
  VCL.GisPrinters,
  VCL.GisViewerWnd,
  VCL.GISRendererGdiPlus;

{$REGION 'T_CloseForm'}
type
  {#gendoc:hide}
  /// <summary>
  ///   Form to ask if save changes.
  /// </summary>
  /// <remarks>
  ///   <note type="caution">
  ///     Only for internal use of TatukGIS.
  ///   </note>
  /// </remarks>
  T_CloseForm = class( TGIS_ModalForm )
    private
      lblQuery   : TLabel ;
      btnSave    : TButton ;
      btnNotSave : TButton ;
      FSave      : Boolean ;

    protected
      procedure btnSaveClick    ( _sender       : TObject
                                ) ;
      procedure btnNotSaveClick ( _sender       : TObject
                                ) ;

    protected

      /// <inheritdoc/>
      procedure initForm     ; override;

      /// <inheritdoc/>
      procedure initControls ; override;

    public
      property Save : Boolean read FSave ;
  end;
{$ENDREGION}

{$REGION 'T_ScaleablePanel'}
type
  T_ScaleablePanel = class( TPanel )
    private
      FPanelTop : TPanel ;
      FPanelBottom : TPanel ;
    protected
      procedure Resize     ; override ;
    public
      property PanelTop    : TPanel read FPanelTop    write FPanelTop ;
      property PanelBottom : TPanel read FPanelBottom write FPanelBottom ;
  end ;
{$ENDREGION}

{$REGION 'T_SpeedButton'}
type
  T_SpeedButton = class( TSpeedButton )
  protected
    procedure Paint ; override ;
  public
    Images     : TImageList ;
    ImageIndex : Integer ;
    DisabledImageIndex : Integer ;
  end;
{$ENDREGION}

{$REGION 'global settings'}
type
  T_DesignerGlobalSettings = record
    Snap          : TGIS_PrintLayoutSnap ;
    WireframeMode : Boolean ;
    DrawingPaused : Boolean ;
    Designer      : TGIS_ControlPrintTemplateDesignerForm ;
    PageControl   : TWinControl ;
    AppStarting   : Boolean ;
    ChangeExtent  : Boolean ;
    MapElement    : Integer ;
  end ;

var
  Settings : T_DesignerGlobalSettings ;
{$ENDREGION}

{$REGION 'T_Utilities'}
type
  T_Utilities = class
    public
      class function  CalculateDistance
                                  ( _x     : Integer ;
                                    _y     : Integer ;
                                    _cntrl : TControl
                                  ) : Integer ;
      class procedure SetActiveControl ;
  end;

  class function T_Utilities.CalculateDistance(
    _x     : Integer ;
    _y     : Integer ;
    _cntrl : TControl
  ) : Integer ;
  begin
    Result := Min( Min( Abs( _x - _cntrl.Left ),
                        Abs( _y - _cntrl.Top ) ),
                   Min( Abs( ( _cntrl.Left + _cntrl.Width ) - _x ),
                        Abs( ( _cntrl.Top + _cntrl.Height ) - _y ) ) ) ;
  end ;

  class procedure T_Utilities.SetActiveControl ;
  begin
    if Settings.Designer.ActiveControl <> Settings.PageControl then
      Settings.Designer.ActiveControl := Settings.PageControl ;
  end ;
{$ENDREGION}

{$REGION 'T_CloseForm'}
  procedure T_CloseForm.initForm ;
  begin
    Self.Caption := _rsrc( GIS_RS_TPL_DESIGNER ) ;
    Self.ClientHeight := 88 ;
    Self.ClientWidth := 293 ;
    Self.Name := 'T_CloseForm' ;
  end ;

  procedure T_CloseForm.initControls ;
  var
    anchors   : TAnchors ;
    anchorsRB : TAnchors ;
  begin
    if BiDiMode = bdRightToLeft then begin
      anchors   := [akRight, akTop] ;
      anchorsRB := [akLeft,  akBottom] ;
    end else begin
      anchors   := [akLeft,  akTop] ;
      anchorsRB := [akRight, akBottom] ;
    end ;

    lblQuery := TLabel.Create( Self );
    lblQuery.Parent := Self ;
    lblQuery.Anchors := anchors ;
    lblQuery.Top  := 18 ;
    PlaceControl( BiDiMode, nil, lblQuery, 25, 100 ) ;
    lblQuery.AutoSize := False ;
    lblQuery.Caption := _rsrc( GIS_RS_TPL_DESIGNER_CLOSE_SAVEQUERY ) ;

    btnNotSave := TButton.Create( Self ) ;
    btnNotSave.Parent := Self ;
    btnNotSave.Top := btnCancel.Top ;
    btnNotSave.Height := btnCancel.Height ;
    PlaceControl( BiDiMode, btnCancel, btnNotSave, -8, btnCancel.Width ) ;
    btnNotSave.Anchors := anchorsRB ;
    btnNotSave.Caption := _rsrc( GIS_RS_TPL_DESIGNER_CLOSE_DONTSAVE ) ;
    btnNotSave.OnClick := btnNotSaveClick ;

    btnSave := TButton.Create( Self ) ;
    btnSave.Parent := Self ;
    btnSave.Top := btnCancel.Top ;
    btnSave.Height := btnCancel.Height ;
    PlaceControl( BiDiMode, btnNotSave, btnSave, -8, btnCancel.Width ) ;
    btnSave.Anchors := anchorsRB ;
    btnSave.Caption := _rsrc( GIS_RS_TPL_DESIGNER_CLOSE_SAVE ) ;
    btnSave.OnClick := btnSaveClick ;
    btnSave.Default := True ;
    btnSave.TabOrder := 0 ;

    btnOK.Visible := False ;
    btnHelp.Visible := False ;
    FSave := False ;
    ActiveControl := btnSave ;
  end ;

  procedure T_CloseForm.btnSaveClick(
    _sender : TObject
  ) ;
  begin
    FSave := True ;
    ModalResult := mrOK ;
  end ;

  procedure T_CloseForm.btnNotSaveClick(
    _sender : TObject
  ) ;
  begin
    FSave := False ;
    ModalResult := mrOK ;
  end ;
{$ENDREGION}

{$REGION 'T_SpeedButton'}
procedure T_SpeedButton.Paint;
begin
  inherited;

  if not assigned( Images ) then exit ;

  if Enabled then
    Images.Draw( Canvas,
                 ( Width  - Images.Width  ) div 2,
                 ( Height - Images.Height ) div 2 ,
                 ImageIndex,
                 True
               )
  else
    Images.Draw( Canvas,
                         ( Width  - Images.Width  ) div 2,
                         ( Height - Images.Height ) div 2,
                         DisabledImageIndex,
                         True ) ;
end;
{$ENDREGION}

{$REGION 'T_ScalablePanel'}
  procedure T_ScaleablePanel.Resize ;
  // empirically taken scale values
  const
    TOP_SCALE = 185 / 900 ;
    BOTTOM_SCALE = 271 / 900 ;
  begin
    if assigned( FPanelTop ) and assigned( FPanelBottom ) then begin
      FPanelTop.Height    := RoundS( TOP_SCALE * Self.Height ) ;
      FPanelBottom.Height := RoundS( BOTTOM_SCALE * Self.Height ) ;
    end ;
  end ;
{$ENDREGION}

{$REGION 'T_FocusControl'}
  procedure T_FocusControl.fset_ResizeElement(
    const _value : T_ResizeElement
  ) ;
  begin
    if assigned( FResizeElement ) then
      FResizeElement.ResetMouseClickPos ;
    FResizeElement := _value ;
    SetBounds( FResizeElement.Left,
               FResizeElement.Top,
               FResizeElement.Width,
               FResizeElement.Height
             ) ;
    FResizeElement.ResetMouseClickPos ;
    ResetResizeType ;
  end ;

  procedure T_FocusControl.fset_ResizeType(
    const _value : T_ResizeType
  ) ;
  begin
    FResizeType := _value;
    case FResizeType of
      T_ResizeType.rtTopLeft       : Cursor := crSizeNWSE ;
      T_ResizeType.rtTopMiddle     : Cursor := crSizeNS ;
      T_ResizeType.rtTopRight      : Cursor := crSizeNESW ;
      T_ResizeType.rtMiddleLeft    : Cursor := crSizeWE ;
      T_ResizeType.rtCenter        : Cursor := crDefault ;
      T_ResizeType.rtMiddleRight   : Cursor := crSizeWE ;
      T_ResizeType.rtBottomLeft    : Cursor := crSizeNESW ;
      T_ResizeType.rtBottomMiddle  : Cursor := crSizeNS ;
      T_ResizeType.rtBottomRight   : Cursor := crSizeNWSE ;
    end ;
  end ;

  procedure T_FocusControl.resetResizeType ;
  begin
    FResizeType := T_ResizeType.rtCenter ;
  end ;

  function T_FocusControl.setNextElementAtPos(
    const _x : Integer ;
    const _y : Integer
  ) : Boolean ;
  var
    cntrl : TControl ;
    x, y  : Integer ;

    function get_back_control : TControl ;
    var
      cntrl : TControl ;
    begin
      cntrl := Parent.ControlAtPos( Point( x, y ), False ) ;
      if cntrl <> FResizeElement then begin
        cntrl.Enabled := False ;
        try
          Result := get_back_control ;
        finally
          cntrl.Enabled := True ;
        end;
      end else begin
        cntrl.Enabled := False ;
        try
          Result := Parent.ControlAtPos( Point( x, y ), False ) ;
        finally
          cntrl.Enabled := True ;
        end;
      end ;
    end ;

  begin
    Result := False ;
    x := _x ;
    y := _y ;
    Self.Enabled := False ;
    try
      cntrl := get_back_control ;
      if not assigned( cntrl ) then
        cntrl := Parent.ControlAtPos( Point( x, y ), False ) ;
      if assigned( cntrl ) and
         ( cntrl is T_ResizeElement ) and
         ( cntrl <> FResizeElement ) then begin
        if assigned( T_ResizeElement(cntrl).FClickEvent ) then
          T_ResizeElement(cntrl).FClickEvent( cntrl ) ;
        Result := True ;
      end ;
    finally
      Self.Enabled := True ;
    end ;
  end ;

  procedure T_FocusControl.doMouseLeave ;
  begin
    ResetResizeType ;
  end ;

  procedure T_FocusControl.MouseDown(
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x, _y  : Integer
  ) ;
  begin
    if _button <> mbLeft then exit ;
    FOrigin     := Point( _x, _y ) ;
    FIsResizing := True ;
    mouseMoved  := False ;
    last_pos    := FOrigin ;
    T_Utilities.SetActiveControl ;
  end ;

  procedure T_FocusControl.MouseMove(
    _shift : TShiftState ;
    _x, _y : Integer
  ) ;
  var
    x, y : Integer ;
  const
    Threshold = 10;

    function distance( const _x1, _y1, _x2, _y2 : Extended ) : Extended ;
    begin
      Result:= Sqrt(Sqr( _x1 - _x2 ) + Sqr( _y1 - _y2 ) ) ;
    end ;

  begin
    inherited MouseMove( _shift, _x, _y ) ;

    x := _x ;
    y := _y ;

    if not FIsResizing then begin
      if distance(0,0,x,y) < Threshold                 then
        fset_ResizeType(T_ResizeType.rtTopLeft)
      else if distance(0,Height/2,x,y) < Threshold     then
        fset_ResizeType(T_ResizeType.rtMiddleLeft)
      else if distance(0,Height,x,y) < Threshold       then
        fset_ResizeType(T_ResizeType.rtBottomLeft)
      else if distance(Width/2,Height,x,y) < Threshold then
        fset_ResizeType(T_ResizeType.rtBottomMiddle)
      else if distance(Width,Height,x,y) < Threshold   then
        fset_ResizeType(T_ResizeType.rtBottomRight)
      else if distance(Width,Height/2,x,y) < Threshold then
        fset_ResizeType(T_ResizeType.rtMiddleRight)
      else if distance(Width,0,x,y) < Threshold        then
        fset_ResizeType(T_ResizeType.rtTopRight)
      else if distance(Width/2,0,x,y) < Threshold      then
        fset_ResizeType(T_ResizeType.rtTopMiddle)
      else
        fset_ResizeType(T_ResizeType.rtCenter);
    end else begin

      if ( last_pos.X = x ) and ( last_pos.Y = y ) then exit ;
      last_pos := Point( x, y ) ;
      if not mouseMoved then begin
        mouseMoved := True ;
        mouseMovedOldCursor := Screen.Cursor ;
      end ;

      case FResizeType of
        T_ResizeType.rtTopLeft: begin
          Left   := Left   + (x-FOrigin.x);
          Top    := Top    + (y-FOrigin.y);
          Width  := Width  - (x-FOrigin.x);
          Height := Height - (y-FOrigin.y);
        end ;
        T_ResizeType.rtTopMiddle: begin
          Top    := Top    + (y-FOrigin.y);
          Height := Height - (y-FOrigin.y);
        end ;
        T_ResizeType.rtTopRight: begin
          Top    := Top    + (y-FOrigin.y);
          Height := Height - (y-FOrigin.y);
          Width  := x;
        end ;
        T_ResizeType.rtMiddleLeft: begin
          Left  := Left  + (x-FOrigin.x);
          Width := Width - (x-FOrigin.x);
        end ;
        T_ResizeType.rtCenter: begin
          Screen.Cursor := 15 ;
          Left := Left + (x-FOrigin.x);
          Top  := Top  + (y-FOrigin.y);
        end ;
        T_ResizeType.rtMiddleRight: begin
          Width := x;
        end ;
        T_ResizeType.rtBottomLeft: begin
          Height := y;
          Left   := Left  + (x-FOrigin.x);
          Width  := Width - (x-FOrigin.x);
        end ;
        T_ResizeType.rtBottomMiddle: begin
          Height := y;
        end ;
        T_ResizeType.rtBottomRight: begin
          Height := y;
          Width  := x;
        end ;
      end ;
      FResizeElement.SetBounds( Left, Top, Width, Height ) ;
    end ;
  end;

  procedure T_FocusControl.MouseUp(
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x, _y  : Integer
  ) ;
  const
    CLICK_MARGIN = 10 ;
  var
    l, t : Integer ;
    x, y : Integer ;
    bl : Boolean ;
    bt : Boolean ;
    br : Boolean ;
    bb : Boolean ;
  begin
    inherited MouseUp( _button, _shift, _x, _y ) ;

    x := _x ;
    y := _y ;

    if FIsResizing then begin
      Cursor := crDefault ;
      FIsResizing := False ;

      if not mouseMoved then begin
        if ( FResizeType = T_ResizeType.rtCenter ) then begin
          if ( Abs( x - FResizeElement.MouseClickPos.X ) < CLICK_MARGIN ) and
             ( Abs( y - FResizeElement.MouseClickPos.Y ) < CLICK_MARGIN ) then begin
            l := Left + x ;
            t := Top  + y ;
            if setNextElementAtPos( l, t ) then
              FResizeElement.SetMouseClickPos( l - Left, t - Top ) ;
          end else
            FResizeElement.SetMouseClickPos( x, y ) ;
        end ;
        exit ;
      end;

      FResizeElement.ResetMouseClickPos ;
      Screen.Cursor := mouseMovedOldCursor ;

      if Width < 0 then begin
        Width := Abs(Width);
        Left  := Left-Width;
      end ;
      if Height < 0 then begin
        Height := Abs(Height);
        Top    := Top-Height;
      end ;

      FResizeElement.SetBounds( Left, Top, Width, Height ) ;

      if ( FResizeType = T_ResizeType.rtCenter ) or
         ( FResizeType = T_ResizeType.rtTopLeft ) or
         ( FResizeType = T_ResizeType.rtMiddleLeft ) or
         ( FResizeType = T_ResizeType.rtBottomLeft ) then
        bl := True
      else
        bl := False ;
      if ( FResizeType = T_ResizeType.rtCenter ) or
         ( FResizeType = T_ResizeType.rtTopLeft ) or
         ( FResizeType = T_ResizeType.rtTopMiddle ) or
         ( FResizeType = T_ResizeType.rtTopRight ) then
        bt := True
      else
        bt := False ;
      if ( FResizeType = T_ResizeType.rtCenter ) or
         ( FResizeType = T_ResizeType.rtTopRight ) or
         ( FResizeType = T_ResizeType.rtMiddleRight ) or
         ( FResizeType = T_ResizeType.rtBottomRight ) then
        br := True
      else
        br := False ;
      if ( FResizeType = T_ResizeType.rtCenter ) or
         ( FResizeType = T_ResizeType.rtBottomLeft ) or
         ( FResizeType = T_ResizeType.rtBottomMiddle ) or
         ( FResizeType = T_ResizeType.rtBottomRight ) then
        bb := True
      else
        bb := False ;
      if assigned( FResizeElement.RealBoundsChangeEvent ) then
        // update Builder and ObjectInspector
        FResizeElement.RealBoundsChangeEvent( FResizeElement, bl, bt, br, bb ) ;
    end ;
  end ;

  procedure T_FocusControl.Paint ;
  const DOTSIZE = 8 ;
  var
    l, t : Integer ;

    procedure fill( _x1, _y1, _x2, _y2 : Integer ) ;
    begin
      Canvas.Brush.Color := clActiveCaption ;
      Canvas.FillRect( Rect( _x1, _y1, _x2, _y2 ) ) ;
      Canvas.Brush.Color := clHotLight ;
      Canvas.FrameRect( Rect( _x1, _y1, _x2, _y2 ) ) ;
    end ;

  begin
    if Settings.DrawingPaused then exit ;
    if Width > 0 then
      l := 0
    else
      l := Width ;
    if Height > 0 then
      t := 0
    else
      t := Height ;

    Canvas.Brush.Style := bsSolid ;
    fill( l, t,
          DOTSIZE+l, DOTSIZE+t ) ;
    fill( l, Ceil(Height/2-DOTSIZE/2),
          DOTSIZE+l, Ceil(Height/2+DOTSIZE/2) ) ;
    fill( l, Height-t-DOTSIZE,
          DOTSIZE+l, Height-t ) ;
    fill( Ceil(Width/2-DOTSIZE/2), t,
          Ceil(Width/2+DOTSIZE/2), DOTSIZE+t ) ;
    fill( Ceil(Width/2-DOTSIZE/2), Height-t-DOTSIZE,
          Ceil(Width/2+DOTSIZE/2), Height-t ) ;
    fill( Width-l-DOTSIZE, t,
          Width-l, DOTSIZE+t ) ;
    fill( Width-l-DOTSIZE, Ceil(Height/2-DOTSIZE/2),
          Width-l, Ceil(Height/2+DOTSIZE/2) ) ;
    fill( Width-l-DOTSIZE, Height-t-DOTSIZE,
          Width-l, Height-t ) ;
    Canvas.Brush.Style := bsClear ;
  end;

  constructor T_FocusControl.Create(
    _owner : TComponent
  ) ;
  begin
    inherited ;
    FIsResizing := false ;
    OnMouseLeave := doMouseLeave ;
  end ;

  procedure T_FocusControl.MoveLeft ;
  var
    l, r, w : Integer ;
  begin
    l := 0 ;
    r := 0 ;
    w := 0 ;
    case FResizeType of
      T_ResizeType.rtTopLeft,
      T_ResizeType.rtMiddleLeft,
      T_ResizeType.rtBottomLeft :
        begin
          l := -1 ;
          w := 1 ;
        end;
      T_ResizeType.rtTopRight,
      T_ResizeType.rtMiddleRight,
      T_ResizeType.rtBottomRight :
        begin
          r := -1 ;
          w := -1 ;
        end;
      T_ResizeType.rtTopMiddle,
      T_ResizeType.rtBottomMiddle : ;
    else
        begin
          l := -1 ;
          r := -1 ;
        end
    end;
    if ( l <> 0 ) or ( r <> 0 ) then begin
      if Settings.Snap.Value = 0 then begin
        Left := Left + l ;
        Width := Width + w ;
        FResizeElement.SetBounds( Left, Top, Width, Height ) ;
        if assigned( FResizeElement.RealBoundsChangeEvent ) then
          // update Builder and ObjectInspector
          FResizeElement.RealBoundsChangeEvent( FResizeElement,
                                                ( l <> 0 ), False,
                                                ( r <> 0 ), False ) ;
      end
      else
        // take snap into account
        if assigned( FResizeElement.RealBoundsChangeExEvent ) then
          // update Builder and ObjectInspector
          FResizeElement.RealBoundsChangeExEvent( FResizeElement, l, 0, r, 0 ) ;
    end ;
    FResizeElement.ResetMouseClickPos ;
  end ;

  procedure T_FocusControl.MoveRight ;
  var
    l, r, w : Integer ;
  begin
    l := 0 ;
    r := 0 ;
    w := 0 ;
    case FResizeType of
      T_ResizeType.rtTopLeft,
      T_ResizeType.rtMiddleLeft,
      T_ResizeType.rtBottomLeft :
        begin
          l := 1 ;
          w := -1 ;
        end;
      T_ResizeType.rtTopRight,
      T_ResizeType.rtMiddleRight,
      T_ResizeType.rtBottomRight :
        begin
          r := 1 ;
          w := 1 ;
        end;
      T_ResizeType.rtTopMiddle,
      T_ResizeType.rtBottomMiddle : ;
    else
        begin
          l := 1 ;
          r := 1 ;
        end
    end;
    if ( l <> 0 ) or ( r <> 0 ) then begin
      if Settings.Snap.Value = 0 then begin
        Left := Left + l ;
        Width := Width + w ;
        FResizeElement.SetBounds( Left, Top, Width, Height ) ;
        if assigned( FResizeElement.RealBoundsChangeEvent ) then
          // update Builder and ObjectInspector
          FResizeElement.RealBoundsChangeEvent( FResizeElement,
                                                ( l <> 0 ), False,
                                                ( r <> 0 ), False ) ;
      end
      else
        // take snap into account
        if assigned( FResizeElement.RealBoundsChangeExEvent ) then
          // update Builder and ObjectInspector
          FResizeElement.RealBoundsChangeExEvent( FResizeElement, l, 0, r, 0 ) ;
    end ;
    FResizeElement.ResetMouseClickPos ;
  end ;

  procedure T_FocusControl.MoveUp ;
  var
    t, b, h : Integer ;
  begin
    t := 0 ;
    b := 0 ;
    h := 0 ;
    case FResizeType of
      T_ResizeType.rtTopLeft,
      T_ResizeType.rtTopMiddle,
      T_ResizeType.rtTopRight :
        begin
          t := -1 ;
          h := 1 ;
        end;
      T_ResizeType.rtBottomLeft,
      T_ResizeType.rtBottomMiddle,
      T_ResizeType.rtBottomRight :
        begin
          b := -1 ;
          h := -1 ;
        end;
      T_ResizeType.rtMiddleLeft,
      T_ResizeType.rtMiddleRight : ;
    else
        begin
          t := -1 ;
          b := -1 ;
        end
    end;
    if ( t <> 0 ) or ( b <> 0 ) then begin
      if Settings.Snap.Value = 0 then begin
        Top := Top + t ;
        Height := Height + h ;
        FResizeElement.SetBounds( Left, Top, Width, Height ) ;
        if assigned( FResizeElement.RealBoundsChangeEvent ) then
          // update Builder and ObjectInspector
          FResizeElement.RealBoundsChangeEvent( FResizeElement,
                                                False, ( t <> 0 ),
                                                False, ( b <> 0 ) ) ;
      end
      else
        // take snap into account
        if assigned( FResizeElement.RealBoundsChangeExEvent ) then
          // update Builder and ObjectInspector
          FResizeElement.RealBoundsChangeExEvent( FResizeElement, 0, t, 0, b ) ;
    end ;
    FResizeElement.ResetMouseClickPos ;
  end ;

  procedure T_FocusControl.MoveDown ;
  var
    t, b, h : Integer ;
  begin
    t := 0 ;
    b := 0 ;
    h := 0 ;
    case FResizeType of
      T_ResizeType.rtTopLeft,
      T_ResizeType.rtTopMiddle,
      T_ResizeType.rtTopRight :
        begin
          t := 1 ;
          h := -1 ;
        end;
      T_ResizeType.rtBottomLeft,
      T_ResizeType.rtBottomMiddle,
      T_ResizeType.rtBottomRight :
        begin
          h := 1 ;
          b := 1 ;
        end;
      T_ResizeType.rtMiddleLeft,
      T_ResizeType.rtMiddleRight : ;
    else
        begin
          t := 1 ;
          b := 1 ;
        end
    end;
    if ( t <> 0 ) or ( b <> 0 ) then begin
      if Settings.Snap.Value = 0 then begin
        Top := Top + t ;
        Height := Height + h ;
        FResizeElement.SetBounds( Left, Top, Width, Height ) ;
        if assigned( FResizeElement.RealBoundsChangeEvent ) then
          // update Builder and ObjectInspector
          FResizeElement.RealBoundsChangeEvent( FResizeElement,
                                                False, ( t <> 0 ),
                                                False, ( b <> 0 ) ) ;
      end
      else
        // take snap into account
        if assigned( FResizeElement.RealBoundsChangeExEvent ) then begin
          // update Builder and ObjectInspector
          FResizeElement.RealBoundsChangeExEvent( FResizeElement, 0, t, 0, b ) ;
        end ;
    end ;
    FResizeElement.ResetMouseClickPos ;
  end ;
{$ENDREGION}

{$REGION 'T_ResizeElement'}
//==============================================================================
// T_ResizeElement
//==============================================================================

  procedure T_ResizeElement.setElementAtPos(
    const _x : Integer ;
    const _y : Integer
  ) ;
  var
    cntrl : TControl ;
    distance : Integer ;
    x, y : Integer ;

    function get_active_control(
          _cntrl    : TControl ;
      var _distance : Integer
    ) : TControl ;
    var
      cntrl : TControl ;
      distance1 : Integer ;
      distance2 : Integer ;
    begin
      Result := _cntrl ;
      _distance := -1 ;
      if assigned( Result ) and ( Result is T_ResizeElement ) then begin
        distance1 := T_Utilities.CalculateDistance( x, y, _cntrl ) ;
        _cntrl.Enabled := False ;
        try
          cntrl := Parent.ControlAtPos( Point( x, y ), False ) ;
          if assigned( cntrl ) then begin
            cntrl := get_active_control( cntrl, distance2 ) ;
            if distance2 < distance1 then begin
              Result := cntrl ;
              _distance := distance2 ;
            end else begin
              _distance := distance1 ;
            end ;
          end else
            _distance := distance1 ;
        finally
          _cntrl.Enabled := True ;
        end;
      end ;
    end ;

  begin
    if assigned( UserData ) then begin
      x := _x ;
      y := _y ;
      cntrl := get_active_control( Self, distance ) ;
      if assigned( cntrl ) and ( cntrl is T_ResizeElement ) then begin
        if assigned( T_ResizeElement(cntrl).FClickEvent ) then
          T_ResizeElement(cntrl).FClickEvent( cntrl ) ;
        T_ResizeElement(cntrl).SetMouseClickPos( _x - cntrl.Left, _y - cntrl.Top ) ;
      end ;
    end;
  end ;

  procedure T_ResizeElement.ResetMouseClickPos ;
  begin
    FMouseClick := Point( -1, -1 ) ;
  end ;

  procedure T_ResizeElement.SetMouseClickPos(
    _x : Integer ;
    _y : Integer
  ) ;
  begin
    FMouseClick := Point( _x, _y ) ;
  end ;

  procedure T_ResizeElement.InvalidateCache ;
  begin
    FreeObject( FCacheBitmap ) ;
    Invalidate ;
  end;

  procedure T_ResizeElement.Paint;
  var
    elm   : TGIS_PrintLayoutElement ;
    ratio : Double ;
    x     : Integer ;
    y     : Integer ;
    w     : Integer ;
    h     : Integer ;

    procedure draw_outline ;
    begin
      Canvas.Pen.Style := psDot ;
      Canvas.Pen.Color := clBlack ;
      Canvas.Brush.Style := bsClear ;
      Canvas.Rectangle( x, y, x + w + 1, y + h + 1 ) ;
    end ;

    procedure draw_caption(
      _caption : String
    ) ;
    var
      ts : TSize ;
    begin
      Canvas.Font.Name := 'Arial' ;
      Canvas.Font.Color := clWindowText ;
      Canvas.Font.Size := 8 ;
      Canvas.Font.Style := [] ;
      ts := Canvas.TextExtent( _caption ) ;
      Canvas.TextOut( Ceil(x + (w - ts.cx)/2), Ceil(y + (h - ts.cy)/2),
                      _caption );
    end ;

    procedure draw_wireframe ;
    begin
      Canvas.Brush.Style := bsClear ;
      Canvas.Pen.Style := psSolid ;
      Canvas.Pen.Width := 1 ;
      Canvas.Pen.Color := clGray ;
      Canvas.Rectangle( x, y, x + w + 1, y + h + 1 ) ;
      draw_caption( Caption ) ;
    end ;

    procedure draw_empty_element ;
    begin
      Canvas.Brush.Style := bsSolid ;
      Canvas.Brush.Color := clWhite ;
      Canvas.Pen.Style := psSolid ;
      Canvas.Pen.Width := 1 ;
      Canvas.Pen.Color := clWhite ;
      Canvas.Rectangle( x, y, x + w, y + h ) ;
    end ;

    procedure draw_map ;
    var
      vbmp : TBitmap ;
      rnd  : TGIS_RendererVclGdiPlus ;
      bmp  : TBitmap ;
    begin
      if not assigned( Self.Template.GIS_Viewer[elm.Index] ) or
         not assigned( FDrawMapEvent ) then begin
        draw_empty_element ;
        exit ;
      end ;
      if ( w < 5 ) or ( h < 5 ) then begin
        draw_empty_element ;
        exit ;
      end ;
      if assigned( FCacheBitmap ) and ( FCacheIndex = elm.Index ) and
         ( w = FCacheBitmap.Width ) and ( h = FCacheBitmap.Height ) then begin
        Canvas.Draw( x, y, FCacheBitmap, 255 ) ;
      end else begin
        draw_empty_element ;
        FDrawMapEvent( Self, elm,
                       RoundS( w * Canvas.Font.PixelsPerInch / 96 / ratio ),
                       RoundS( h * Canvas.Font.PixelsPerInch / 96 / ratio ),
                       vbmp ) ;
        if assigned( vbmp ) then begin
          try
            rnd := TGIS_RendererVclGdiPlus.Create ;
            bmp := TBitmap.Create ;
            bmp.Width := w ;
            bmp.Height := h ;
            bmp.PixelFormat := pf32bit ;
            bmp.Canvas.Brush.Style := bsSolid ;
            bmp.Canvas.Brush.Color := 0 ;
            bmp.Canvas.FillRect( Rect( 0, 0, bmp.Width, bmp.Height ) );
            rnd.stretchBitmap( vbmp, bmp, Rect(0, 0, bmp.Width, bmp.Height), 100 );
            Canvas.Draw( x, y, bmp, 255 ) ;
            FreeObject( FCacheBitmap ) ;
            FCacheBitmap := bmp ;
            FCacheIndex := elm.Index ;
          finally
            FreeObject( rnd ) ;
          end;
        end else
          draw_empty_element ;
      end;
      if not Self.Template.GIS_UseViewerColor[elm.Index] then
        draw_outline ;
    end ;

    procedure draw_legend ;
    var
      gbmp : TGIS_Bitmap ;
      bmp  : TBitmap ;
      rnd  : TGIS_RendererVclGdiPlus ;
    begin
      if not assigned( Self.Template.GIS_Legend[elm.Index] ) or
         not ( Self.Template.GIS_Legend[elm.Index] is TGIS_ControlLegend ) or
         not assigned( TGIS_ControlLegend(Self.Template.GIS_Legend[elm.Index]).GIS_Viewer ) or
         not assigned( FDrawControlEvent ) then begin
        draw_empty_element ;
        exit ;
      end;
      if ( w < 5 ) or ( h < 5 ) then begin
        draw_empty_element ;
        exit ;
      end ;
      if assigned( FCacheBitmap ) and ( FCacheIndex = elm.Index ) and
         ( w = FCacheBitmap.Width ) and ( h = FCacheBitmap.Height ) then begin
        Canvas.Draw( x, y, FCacheBitmap ) ;
      end else begin
        gbmp := TGIS_Bitmap.Create( RoundS( w * Canvas.Font.PixelsPerInch / 96 / ratio ),
                                    RoundS( h * Canvas.Font.PixelsPerInch / 96 / ratio ) ) ;
        try
          FDrawControlEvent( Self, elm, gbmp ) ;
          try
            rnd := TGIS_RendererVclGdiPlus.Create ;
            bmp := TBitmap.Create ;
            bmp.Width := w ;
            bmp.Height := h ;
            bmp.PixelFormat := pf32bit ;
            bmp.Canvas.Brush.Style := bsSolid ;
            bmp.Canvas.Brush.Color := 0 ;
            bmp.Canvas.FillRect( Rect( 0, 0, bmp.Width, bmp.Height ) );
            rnd.stretchBitmap( TBitmap(gbmp.NativeBitmap), bmp, Rect(0, 0, bmp.Width, bmp.Height), 100 ) ;
            Canvas.Draw( x, y, bmp ) ;
            FreeObject( FCacheBitmap ) ;
            FCacheBitmap := bmp ;
            FCacheIndex := elm.Index ;
          finally
            FreeObject( rnd ) ;
          end;
        finally
          FreeObject( gbmp ) ;
        end;
      end;
    end ;

    procedure draw_scale ;
    var
      gbmp : TGIS_Bitmap ;
      bmp  : TBitmap ;
      rnd  : TGIS_RendererVclGdiPlus ;
    begin
      if not assigned( Self.Template.GIS_Scale[elm.Index] ) or
         not ( Self.Template.GIS_Scale[elm.Index] is TGIS_ControlScale ) or
         not assigned( TGIS_ControlScale(Self.Template.GIS_Scale[elm.Index]).GIS_Viewer ) or
         not assigned( FDrawControlEvent ) then begin
        draw_empty_element ;
        exit ;
      end;
      if ( w < 5 ) or ( h < 5 ) then begin
        draw_empty_element ;
        exit ;
      end ;
      gbmp := TGIS_Bitmap.Create( RoundS( w * Canvas.Font.PixelsPerInch / 96 / ratio ),
                                  RoundS( h * Canvas.Font.PixelsPerInch / 96 / ratio )
                                ) ;
      try
        FDrawControlEvent( Self, elm, gbmp ) ;
        try
          rnd := TGIS_RendererVclGdiPlus.Create ;
          bmp := TBitmap.Create ;
          try
            bmp.Width := w ;
            bmp.Height := h ;
            bmp.PixelFormat := pf32bit ;
            bmp.Canvas.Brush.Style := bsSolid ;
            bmp.Canvas.Brush.Color := 0 ;
            bmp.Canvas.FillRect( Rect( 0, 0, bmp.Width, bmp.Height ) );
            rnd.stretchBitmap( TBitmap(gbmp.NativeBitmap), bmp, Rect(0, 0, bmp.Width, bmp.Height), 100 ) ;
            if TGIS_ControlScale( Self.Template.GIS_Scale[elm.Index] ).Transparent then
              Canvas.Draw( x, y, bmp, 255 )
            else
              Canvas.Draw( x, y, bmp ) ;
          finally
            FreeObject( bmp ) ;
          end;
        finally
          FreeObject( rnd ) ;
        end;
      finally
        FreeObject( gbmp ) ;
      end;
      if TGIS_ControlScale( Self.Template.GIS_Scale[elm.Index] ).Transparent then
        draw_outline ;
    end ;

    procedure draw_northarrow ;
    var
      gbmp : TGIS_Bitmap ;
      rnd  : TGIS_RendererVclGdiPlus ;
      bmp  : TBitmap ;
    begin
      if not assigned( Self.Template.GIS_NorthArrow[elm.Index] ) or
         not ( Self.Template.GIS_NorthArrow[elm.Index] is TGIS_ControlNorthArrow ) or
         not assigned( TGIS_ControlNorthArrow(Self.Template.GIS_NorthArrow[elm.Index]).GIS_Viewer ) or
         not assigned( FDrawControlEvent ) then begin
        draw_empty_element ;
        exit ;
      end;
      if ( w < 5 ) or ( h < 5 ) then begin
        draw_empty_element ;
        exit ;
      end ;
      if assigned( FCacheBitmap ) and ( FCacheIndex = elm.Index ) and
         ( w = FCacheBitmap.Width ) and ( h = FCacheBitmap.Height ) then begin
        if TGIS_ControlNorthArrow( Self.Template.GIS_NorthArrow[elm.Index] ).Transparent then
          Canvas.Draw( x, y, FCacheBitmap, 255 )
        else
          Canvas.Draw( x, y, FCacheBitmap ) ;
      end else begin
        gbmp := TGIS_Bitmap.Create( RoundS( w / ratio ), RoundS( h / ratio ) ) ;
        try
          FDrawControlEvent( Self, elm, gbmp ) ;
          try
            rnd := TGIS_RendererVclGdiPlus.Create ;
            bmp := TBitmap.Create ;
            bmp.Width := w ;
            bmp.Height := h ;
            bmp.PixelFormat := pf32bit ;
            bmp.Canvas.Brush.Style := bsSolid ;
            bmp.Canvas.Brush.Color := 0 ;
            bmp.Canvas.FillRect( Rect( 0, 0, bmp.Width, bmp.Height ) );
            rnd.stretchBitmap( TBitmap(gbmp.NativeBitmap), bmp, Rect(0, 0, bmp.Width, bmp.Height), 100 ) ;
            if TGIS_ControlNorthArrow( Self.Template.GIS_NorthArrow[elm.Index] ).Transparent then
              Canvas.Draw( x, y, bmp, 255 )
            else
              Canvas.Draw( x, y, bmp ) ;
            FreeObject( FCacheBitmap ) ;
            FCacheBitmap := bmp ;
            FCacheIndex := elm.Index ;
          finally
            FreeObject( rnd ) ;
          end;
        finally
          FreeObject( gbmp ) ;
        end;
      end;
      if TGIS_ControlNorthArrow( Self.Template.GIS_NorthArrow[elm.Index] ).Transparent then
        draw_outline ;
    end ;

    procedure draw_graphic ;
    var
      drawn : Boolean ;
    begin
      if not assigned( FDrawGraphicEvent ) or
         ( w < 5 ) or ( h < 5 ) then begin
        draw_empty_element ;
        exit ;
      end ;
      drawn := True ;
      FDrawGraphicEvent( Self, elm, Canvas,
                         Rect( x, y, x + w, y + h ),
                         drawn ) ;
      draw_outline ;
      if not drawn then
        draw_caption( '[' + _rsrc( GIS_RS_TPL_DESIGNER_NOTDEFINED ) + ']' ) ;
    end ;

    procedure draw_box ;
    var
      drawn : Boolean ;
      wi : Integer ;
    begin
      drawn := False ;
      if TGIS_PrintLayoutBox(elm).Color <> TGIS_Color.None then begin
        Canvas.Brush.Style := bsSolid ;
        Canvas.Brush.Color := VCLColor( TGIS_PrintLayoutBox(elm).Color ) ;
        Canvas.Pen.Style := psSolid ;
        Canvas.Pen.Width := 1 ;
        Canvas.Pen.Color := VCLColor( TGIS_PrintLayoutBox(elm).Color ) ;
        Canvas.Rectangle( x, y, x + w, y + h ) ;
        drawn := True ;
      end ;
      // frame
      if TGIS_PrintLayoutBox(elm).FrameColor <> TGIS_Color.None then begin
        wi := TGIS_PrintUtils.ToPixels(
                TGIS_PrintLayoutBox(elm).FrameWidth.Value,
                TGIS_PrintLayoutBox(elm).FrameWidth.Units,
                96 ) ;
        if wi > 0 then begin
          wi := Max ( 1, RoundS( wi * ratio ) ) ;
          Canvas.Brush.Style := bsClear ;
          Canvas.Pen.Style := psSolid ;
          Canvas.Pen.Width := 2 * wi - 1 ;
          Canvas.Pen.Color := VCLColor( TGIS_PrintLayoutBox(elm).FrameColor ) ;
          Canvas.Rectangle( x, y, x + w + 1, y + h + 1 ) ;
          drawn := True ;
        end ;
      end ;
      if not drawn then
        draw_outline ;
    end ;

    procedure draw_text ;
    var
      element : TGIS_PrintLayoutText ;
      rct     : TRect ;
      pt      : TPoint ;
      txt     : String ;
      old_bkmode : Integer ;
    begin
      element := TGIS_PrintLayoutText(elm) ;

      rct := Rect( x, y, x + w, y + h ) ;
      Canvas.Brush.Style := bsClear ;

      Canvas.Font.Color := VCLColor( element.Color ) ;
      Canvas.Font.Name  := element.Font ;
      Canvas.Font.Size  := RoundS( element.Size * ratio * 96 /
                                   Canvas.Font.PixelsPerInch ) ;
      Canvas.Font.Style := VCLFontStyle( element.Style ) ;

      pt.Y := rct.Top ;

      case element.Align of
        TGIS_LabelAlignment.Center :
          begin
            pt.X := rct.Left + ( rct.Right - rct.Left ) div 2 ;
            Winapi.Windows.SetTextAlign( Canvas.Handle, TA_TOP or TA_CENTER ) ;
          end ;
        TGIS_LabelAlignment.RightJustify :
          begin
            pt.X := rct.Right ;
            Winapi.Windows.SetTextAlign( Canvas.Handle, TA_TOP or TA_RIGHT ) ;
          end ;
        else
          begin
            pt.X := rct.Left ;
            Winapi.Windows.SetTextAlign( Canvas.Handle, TA_TOP or TA_LEFT ) ;
          end ;
      end ;

      if element.BackgroundColor <> TGIS_Color.None then begin
        Canvas.Brush.Style := bsSolid ;
        Canvas.Brush.Color := VCLColor( element.BackgroundColor ) ;
        Canvas.Pen.Width := 1 ;
        Canvas.Pen.Style := psSolid ;
        Canvas.Pen.Color := VCLColor( element.BackgroundColor ) ;
        Canvas.Rectangle( rct ) ;
      end ;

      txt := '' ;
      if element.Index > 0 then begin
        if not IsStringEmpty( FTemplate.Text[ element.Index ] ) then
          txt := FTemplate.Text[ element.Index ]
        else
          element.Index := 0 ;
      end ;
      if element.Index = 0 then
        txt := element.Text ;
      if not IsStringEmpty( txt ) then begin
        txt := GisExpandLabel( txt, FTemplate.GetField ) ;
        old_bkmode := Winapi.Windows.SetBkMode( Canvas.Handle, TRANSPARENT ) ;
        try
          Winapi.Windows.ExtTextOut( Canvas.Handle, pt.X, pt.Y, 0, @rct,
                                     PChar(txt), Length( txt ), nil ) ;
        finally
          Winapi.Windows.SetBkMode( Canvas.Handle, old_bkmode ) ;
        end;
      end ;
      if ( element.BackgroundColor = TGIS_Color.None ) then
        draw_outline ;
    end ;

  begin
    if Settings.DrawingPaused then exit ;

    ratio := ControlParent.SizeRatio ;

    x := Min( 0, 0 + Width ) ;
    y := Min( 0, 0 + Height ) ;
    if Width > 0 then
      w := Abs( Width - 1 )
    else
      w := Abs( Width + 1 ) ;
    if Height > 0 then
      h := Abs( Height - 1 )
    else
      h := Abs( Height + 1 ) ;

    if Settings.WireframeMode then begin
      draw_wireframe ;
      exit ;
    end ;
    elm := Self.UserData ;
    if elm.ElementType = TGIS_PrintLayoutElementType.Map then
      draw_map
    else if elm.ElementType = TGIS_PrintLayoutElementType.Box then
      draw_box
    else if elm.ElementType = TGIS_PrintLayoutElementType.Legend then
      draw_legend
    else if elm.ElementType = TGIS_PrintLayoutElementType.Scale then
      draw_scale
    else if elm.ElementType = TGIS_PrintLayoutElementType.NorthArrow then
      draw_northarrow
    else if elm.ElementType = TGIS_PrintLayoutElementType.Graphic then
      draw_graphic
    else if elm.ElementType = TGIS_PrintLayoutElementType.Text then
      draw_text ;
  end ;

  procedure T_ResizeElement.MouseDown(
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x, _y  : Integer
  ) ;
    procedure finishChangingExtent ;
    var
      frm : TCustomForm ;
    begin
      frm := GetParentForm( Self ) ;
      if assigned( frm ) and ( frm is TGIS_ControlPrintTemplateDesignerForm ) then
        TGIS_ControlPrintTemplateDesignerForm(frm).finishChangingExtent ;
    end ;

  begin
    finishChangingExtent ;
    if _button <> mbLeft then exit ;
    setElementAtPos( Left + _x, Top + _y ) ;
    T_Utilities.SetActiveControl ;
  end ;

  constructor T_ResizeElement.Create(
    _owner: TComponent
  );
  begin
    inherited Create( _owner ) ;
    UserData     := nil ;
    FCacheIndex  := 0 ;
    FCacheBitmap := nil ;
    FMouseClick  := Point( -1, -1 ) ;
    FClickEvent  := nil ;
    FRealBoundsChange := nil ;
    FRealBoundsChangeEx := nil ;
    FTemplate    := nil ;
    FObject      := nil ;
  end ;

  destructor T_ResizeElement.Destroy ;
  begin
    FreeObject( FCacheBitmap ) ;
    FreeObject( FObject ) ;
    inherited ;
  end ;
{$ENDREGION}

{$REGION 'T_ControlPrintTemplate'}
//==============================================================================
// T_ControlPrintTemplate
//==============================================================================

  procedure T_ControlPrintTemplate.doMouseDown(
    _sender : TObject ;
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x, _y  : Integer
  ) ;
    procedure finishChangingExtent ;
    var
      frm : TCustomForm ;
    begin
      frm := GetParentForm( Self ) ;
      if assigned( frm ) and ( frm is TGIS_ControlPrintTemplateDesignerForm ) then
        TGIS_ControlPrintTemplateDesignerForm(frm).finishChangingExtent ;
    end ;

  begin
    finishChangingExtent ;
    if Settings.AppStarting then exit ;
    if _button = mbLeft then begin
      if FCreatingElement then begin
        FCreatingElementHandling := True ;
        FCurrentElement := AddElement ;
        FCurrentElement.Left   := _x ;
        FCurrentElement.Top    := _y ;
        FCurrentElement.Width  := 0 ;
        FCurrentElement.Height := 0 ;
        if assigned( FOnCreateShape ) then
          FOnCreateShape( FCurrentElement ) ;
      end ;
    end ;
    T_Utilities.SetActiveControl ;
  end ;

  procedure T_ControlPrintTemplate.doMouseUp(
    _sender : TObject ;
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x, _y  : Integer
  ) ;
  begin
    if Settings.AppStarting then exit ;
    if FCreatingElementHandling then begin
      FCreatingElementHandling := False;
      pnlPage.Cursor := crDefault ;

      if FCurrentElement.Width < 0 then begin
        FCurrentElement.Width := Abs(FCurrentElement.Width);
        FCurrentElement.Left  := FCurrentElement.Left-FCurrentElement.Width;
      end ;
      if FCurrentElement.Height < 0 then begin
        FCurrentElement.Height := Abs(FCurrentElement.Height);
        FCurrentElement.Top    := FCurrentElement.Top-FCurrentElement.Height;
      end ;

      if (FCurrentElement.Height < 2) or (FCurrentElement.Width < 2) then begin
        FCurrentElement.Width  := 100 ;
        FCurrentElement.height := 100 ;
      end ;

      FCreatingElement := False ;

      if assigned( FOnAddShape ) then
        FOnAddShape( FCurrentElement ) ;

      FCurrentElement := nil;
    end ;
  end ;

  procedure T_ControlPrintTemplate.doMouseMove(
    _sender : TObject ;
    _shift  : TShiftState ;
    _x, _y  : Integer
  ) ;
  begin
    if Settings.AppStarting then exit ;
    if FCreatingElement then begin
      if FCreatingElementHandling then begin
        if ( _x - FCurrentElement.Left ) > 10 then
          FCurrentElement.Width  := _x - FCurrentElement.Left
        else
          FCurrentElement.Width  := 10 ;
        if ( _y - FCurrentElement.Top ) > 10 then
          FCurrentElement.Height := _y - FCurrentElement.Top
        else
          FCurrentElement.Height := 10 ;
        Application.ProcessMessages;
      end
      else
        pnlPage.Cursor := crDrag ;
    end;
  end ;

  procedure T_ControlPrintTemplate.fset_CreatingElement(
    const _value : Boolean
  ) ;
  begin
    FCreatingElement := _value ;
    if FCreatingElement then
      DisableElements
    else
      EnableElements ;
  end ;

  function T_ControlPrintTemplate.fget_Element(
    _index : Integer
  ) : T_ResizeElement ;
  begin
    Result := nil;
    if ( _index > -1 ) and ( _index < FElementList.Count ) then
      Result := FElementList.Items[_index] ;
  end ;

  procedure T_ControlPrintTemplate.fset_SizeRatio(
    const _value : Double
  ) ;
  var
    old : Double ;
    i : Integer ;
    m : Integer ;
  begin
    old := FSizeRatio ;
    FSizeRatio := _value ;

    m := -1 ;
    for i := 0 to Pred( FElementList.Count ) do begin
      ApplyBounds( FElementList[i], FElementList[i].UserData.Location.Rectangle ) ;
      if assigned( FElementList[i].ElementObject ) and
         ( FElementList[i].UserData.ElementType = TGIS_PrintLayoutElementType.Map ) then
        m := i ;
    end ;
    if assigned( FFocusControl ) then begin
      // needed ?
      FFocusControl.ResizeElement.ResetMouseClickPos ;
      FFocusControl.ResetResizeType ;
    end ;
    if m >= 0 then begin
      if FElementList[m].ElementObject is TControl then
        TControl( FElementList[m].ElementObject ).SetBounds(
          FElementList[m].Left,
          FElementList[m].Top,
          FElementList[m].Width,
          FElementList[m].Height
        ) ;
      if FElementList[m].ElementObject is TGIS_ViewerWnd then
        TGIS_ViewerWnd( FElementList[m].ElementObject ).CustomPPI :=
          RoundS( TGIS_ViewerWnd( FElementList[m].ElementObject ).CustomPPI * FSizeRatio / old ) ;
    end ;
  end ;

  constructor T_ControlPrintTemplate.Create(
    _owner : TComponent
  ) ;
  begin
    inherited ;
    ControlStyle := ControlStyle + [csNeedsBorderPaint];

    Caption          := ''     ;
    Color            := clGray ;
    ParentBackground := False  ;
    Width            := 100    ;
    Height           := 100    ;
    BevelOuter       := bvNone ;
    BevelInner       := bvNone ;
    BorderStyle      := bsNone ;

    pnlShadow := TPanel.Create( self ) ;
    with pnlShadow do begin
      Parent             := Self     ;
      Caption            := ''       ;
      Color              := clBlack  ;
      ParentBackground   := False    ;
      BevelOuter         := bvNone   ;
      BevelInner         := bvNone   ;
      BorderStyle        := bsNone   ;
      Width              := 1        ;
      Height             := 1        ;
      Left               := -10      ;
    end ;

    pnlPage := T_TemplatePanel.Create( Self ) ;
    with pnlPage do begin
      Parent           := Self     ;
      ControlStyle     := ControlStyle - [csAcceptsControls];
      Caption          := ''       ;
      Color            := clWhite  ;
      ParentBackground := False    ;
      BevelOuter       := bvNone   ;
      BevelInner       := bvNone   ;
      BorderStyle      := bsNone   ;
      Width            := 1        ;
      Height           := 1        ;
      Left             := -10      ;
      ControlParent    := Self     ;
      OnMouseDown := doMouseDown ;
      OnMouseMove := doMouseMove ;
      OnMouseUp   := doMouseUp ;
    end ;

    imgGrid := TImage.Create( pnlPage ) ;
    with imgGrid do begin
      Parent  := pnlPage ;
      Name    := 'imgGrid' ;
      Enabled := False ;
    end ;

    FPageSize := Point( 850, 1100 ) ;

    FElementList  := TList<T_ResizeElement>.Create;
    FFocusControl := nil ;
    FCreatingElement := False ;
    FCreatingElementHandling := False ;
    FSizeRatio := 1 ;
  end;

  destructor T_ControlPrintTemplate.Destroy ;
  begin
    FreeObject( FElementList ) ;
    inherited ;
  end;

  procedure T_ControlPrintTemplate.Resize ;
  const
    SHADOW = 5          ;
    MARGIN = SHADOW + 8 ;
  var
    page_size_scaled : TPoint ;
    print_area_scaled : TRect ;
    scale : Double ;
  begin
    if ( FPageSize.X = 0 ) or ( FPageSize.Y = 0 ) then exit ;

    // like ControlPrintPreview does ; do not optimize the calculations
    page_size_scaled.X := RoundS( FPageSize.X * 96 / FPPI ) ;
    page_size_scaled.Y := RoundS( FPageSize.Y * 96 / FPPI ) ;
    print_area_scaled.Left   := RoundS( FPrintArea.Left   * 96 / FPPI ) ;
    print_area_scaled.Top    := RoundS( FPrintArea.Top    * 96 / FPPI ) ;
    print_area_scaled.Right  := RoundS( FPrintArea.Right  * 96 / FPPI ) ;
    print_area_scaled.Bottom := RoundS( FPrintArea.Bottom * 96 / FPPI ) ;

    scale := Min( ( Width  - 2* MARGIN ) / page_size_scaled.X ,
                  ( Height - 2* MARGIN ) / page_size_scaled.Y
                ) ;

    pnlPage.Width  := RoundS( page_size_scaled.X * scale ) ;
    pnlPage.Height := RoundS( page_size_scaled.Y * scale ) ;

    pnlPage.Left := ( Width  - pnlPage.Width  ) div 2 ;
    pnlPage.Top  := ( Height - pnlPage.Height ) div 2 ;

    pnlShadow.Width   := pnlPage.Width  ;
    pnlShadow.Height  := pnlPage.Height ;
    pnlShadow.Top     := pnlPage.Top    + SHADOW ;
    pnlShadow.Left    := pnlPage.Left   + SHADOW ;

    imgGrid.Width  := pnlPage.Width ;
    imgGrid.Height := pnlPage.Height ;
    imgGrid.Left := 0 ;
    imgGrid.Top  := 0 ;

    updateGrid(
      RoundS( print_area_scaled.Left   * scale ),
      RoundS( print_area_scaled.Top    * scale ),
      RoundS( print_area_scaled.Right  * scale ),
      RoundS( print_area_scaled.Bottom * scale )
    ) ;
    SizeRatio := scale ;
  end;

  procedure T_ControlPrintTemplate.MouseDown (
    _button : TMouseButton ;
    _shift : TShiftState ;
    _x, _y : Integer
  ) ;
    procedure finishChangingExtent ;
    var
      frm : TCustomForm ;
    begin
      frm := GetParentForm( Self ) ;
      if assigned( frm ) and ( frm is TGIS_ControlPrintTemplateDesignerForm ) then
        TGIS_ControlPrintTemplateDesignerForm(frm).finishChangingExtent ;
    end ;

  begin
    finishChangingExtent ;
    inherited MouseDown( _button, _shift, _x, _y ) ;
    if FCreatingElement then begin
      pnlPage.Cursor := crDefault ;
      if assigned( FOnCancelShape ) then
        FOnCancelShape( Self ) ;
    end ;
  end ;

  procedure T_ControlPrintTemplate.updateGrid(
    _left_margin   : Integer ;
    _top_margin    : Integer ;
    _right_margin  : Integer ;
    _bottom_margin : Integer
  ) ;
  var
    rct  : TRect ;
    i, j : Integer ;
    skip : Integer ;
  begin
    imgGrid.Canvas.Lock ;
    try
      imgGrid.Picture.Graphic.Width := imgGrid.Width ;
      imgGrid.Picture.Graphic.Height := imgGrid.Height ;

      imgGrid.Canvas.Brush.Color := clWindow ;
      imgGrid.Canvas.FillRect( imgGrid.BoundsRect ) ;
      imgGrid.Canvas.FrameRect( imgGrid.BoundsRect ) ;

      imgGrid.Canvas.Brush.Color := clBtnFace ;
      rct := Rect( 0, 0, _left_margin, imgGrid.Height ) ;
      imgGrid.Canvas.FillRect( rct ) ;
      imgGrid.Canvas.FrameRect( rct ) ;
      rct := Rect( 0, 0, imgGrid.Width, _top_margin ) ;
      imgGrid.Canvas.FillRect( rct ) ;
      imgGrid.Canvas.FrameRect( rct ) ;
      rct := Rect( _right_margin, 0, imgGrid.Width, imgGrid.Height ) ;
      imgGrid.Canvas.FillRect( rct ) ;
      imgGrid.Canvas.FrameRect( rct ) ;
      rct := Rect( 0, _bottom_margin, imgGrid.Width, imgGrid.Height ) ;
      imgGrid.Canvas.FillRect( rct ) ;
      imgGrid.Canvas.FrameRect( rct ) ;

      skip := 10 ;

      i := 0 ;
      while i < imgGrid.Width do begin
        j := 0 ;
        while j < imgGrid.Height do begin
          imgGrid.Canvas.Pixels[i,j] := $000000 ;
          inc( j, skip ) ;
        end;
        inc( i, skip ) ;
      end;
    finally
      imgGrid.Canvas.Unlock ;
    end;
  end;

  procedure T_ControlPrintTemplate.SetPrinter(
    const _pageSize  : TPoint ;
    const _printArea : TRect ;
    const _ppi       : Integer
  ) ;
  begin
    FPageSize  := _pageSize ;
    FPrintArea := _printArea ;
    FPPI       := _ppi ;
  end;

  function T_ControlPrintTemplate.AddElement
    : T_ResizeElement ;
  begin
    Result := T_ResizeElement.Create( pnlPage );
    Result.Parent := pnlPage ;
    Result.Left   := 0 ;
    Result.Top    := 0 ;
    Result.Width  := 0 ;
    Result.Height := 0 ;
    Result.ControlParent := Self ;
    FElementList.Add( Result ) ;
  end ;

  procedure T_ControlPrintTemplate.DeleteElement(
    const _idx : Integer
  ) ;
  var
    elm : T_ResizeElement ;
  begin
    if ( _idx > -1 ) and ( _idx < FElementList.Count ) then begin
      if assigned( FFocusControl ) and
         ( FFocusControl.ResizeElement = FElementList.Items[_idx] ) then begin
        pnlPage.RemoveControl( FFocusControl ) ;
        FreeObject( FFocusControl ) ;
      end ;
      elm := FElementList[_idx] ;
      pnlPage.RemoveControl( elm );
      FElementList.Delete( _idx ) ;
      FreeObject( elm ) ;
    end ;
  end ;

  procedure T_ControlPrintTemplate.MoveElementUp(
    const _idx : Integer
  ) ;
  var
    cntrl : TControl ;
    i : Integer ;
  begin
    if ( _idx > 0 ) and ( _idx < FElementList.Count ) then begin
      FElementList.Move( _idx, _idx - 1 ) ;
      for i := pnlPage.ControlCount - 1 downto ( _idx - 1 ) + 1 do begin
        cntrl := pnlPage.Controls[i] ;
        pnlPage.RemoveControl( cntrl ) ;
      end ;
      for i := _idx - 1 to FElementList.Count-1 do
        pnlPage.InsertControl( FElementList.Items[i] ) ;
      if assigned( FFocusControl ) then
        pnlPage.InsertControl( FFocusControl ) ;
    end ;
  end ;

  procedure T_ControlPrintTemplate.MoveElementDown(
    const _idx : Integer
  ) ;
  var
    cntrl : TControl ;
    i : Integer ;
  begin
    if ( _idx >= 0 ) and ( _idx < FElementList.Count - 1 ) then begin
      FElementList.Move( _idx, _idx + 1 ) ;
      for i := pnlPage.ControlCount - 1 downto _idx + 1 do begin
        cntrl := pnlPage.Controls[i] ;
        pnlPage.RemoveControl( cntrl ) ;
      end ;
      for i := _idx to FElementList.Count-1 do
        pnlPage.InsertControl( FElementList.Items[i] ) ;
      if assigned( FFocusControl ) then
        pnlPage.InsertControl( FFocusControl ) ;
    end;
  end ;

  procedure T_ControlPrintTemplate.MoveElementBack(
    const _idx : Integer
  ) ;
  var
    cntrl : TControl ;
    i : Integer ;
  begin
    if ( _idx > 0 ) and ( _idx < FElementList.Count ) then begin
      FElementList.Move( _idx, 0 ) ;
      for i := pnlPage.ControlCount - 1 downto 1 do begin
        cntrl := pnlPage.Controls[i] ;
        pnlPage.RemoveControl( cntrl ) ;
      end ;
      for i := 0 to FElementList.Count-1 do
        pnlPage.InsertControl( FElementList.Items[i] ) ;
      if assigned( FFocusControl ) then
        pnlPage.InsertControl( FFocusControl ) ;
    end ;
  end ;

  procedure T_ControlPrintTemplate.MoveElementFront(
    const _idx : Integer
  ) ;
  var
    cntrl : TControl ;
    i : Integer ;
  begin
    if ( _idx >= 0 ) and ( _idx < FElementList.Count - 1 ) then begin
      FElementList.Move( _idx, FElementList.Count - 1 ) ;
      for i := pnlPage.ControlCount - 1 downto _idx + 1 do begin
        cntrl := pnlPage.Controls[i] ;
        pnlPage.RemoveControl( cntrl ) ;
      end ;
      for i := _idx to FElementList.Count-1 do
        pnlPage.InsertControl( FElementList.Items[i] ) ;
      if assigned( FFocusControl ) then
        pnlPage.InsertControl( FFocusControl ) ;
    end;
  end ;

  procedure T_ControlPrintTemplate.ClearElements ;
  var
    ctrl : TControl ;
  begin
    while FElementList.Count > 0 do begin
      ctrl := FElementList.Items[0] ;
      pnlPage.RemoveControl( ctrl );
      FElementList.Delete( 0 ) ;
      FreeObject( ctrl ) ;
    end ;
    if assigned( FFocusControl ) then begin
      pnlPage.RemoveControl( FFocusControl ) ;
      FreeObject( FFocusControl ) ;
    end;
  end ;

  procedure T_ControlPrintTemplate.InvalidateElements ;
  var
    i : Integer ;
  begin
    if not Visible then exit ;
    for i := 0 to Pred( FElementList.Count ) do
      FElementList[i].Invalidate ;
    if assigned( FFocusControl ) then
      FFocusControl.Invalidate ;
  end;

  procedure T_ControlPrintTemplate.SetFocused(
    const _idx : Integer
  ) ;
  begin
    if assigned( FFocusControl ) then begin
      pnlPage.RemoveControl( FFocusControl ) ;
      FreeObject( FFocusControl ) ;
    end;
    if ( _idx > -1 ) and ( _idx < FElementList.Count ) then begin
      FFocusControl := T_FocusControl.Create( pnlPage ) ;
      FFocusControl.Parent := pnlPage ;
      FFocusControl.ResizeElement := FElementList[_idx] ;
    end;
  end ;

  function T_ControlPrintTemplate.IsFocused(
    const _idx : Integer
  ) : Boolean ;
  begin
    Result := False ;
    if assigned( FFocusControl ) and
       ( _idx > -1 ) and ( _idx < FElementList.Count ) and
       ( FFocusControl.ResizeElement = FElementList[_idx] ) then
      Result := True ;
  end ;

  procedure T_ControlPrintTemplate.EnableElements;
  var
    i : Integer ;
  begin
    for i := 0 to Pred( FElementList.Count ) do
      FElementList[i].Enabled := True ;
    if assigned( FFocusControl ) then
      FFocusControl.Enabled := True ;
  end;

  procedure T_ControlPrintTemplate.DisableElements ;
  var
    i : Integer ;
  begin
    for i := 0 to Pred( FElementList.Count ) do
      FElementList[i].Enabled := False ;
    if assigned( FFocusControl ) then
      FFocusControl.Enabled := False ;
  end;

  function T_ControlPrintTemplate.IndexOf(
    const _element : T_ResizeElement
  ) : Integer ;
  begin
    Result := FElementList.IndexOf( _element ) ;
  end;

  function T_ControlPrintTemplate.ElementAtPos(
    const _x : Integer ;
    const _y : Integer
  ) : T_ResizeElement ;
  var
    i : Integer ;
    e : T_ResizeElement ;
  begin
    result := nil ;
    for i := FElementList.Count - 1 downto 0 do begin
      e := FElementList[i] ;
      if ( e.Enabled = True ) and
         ( ( _x >= e.Left ) and ( _x <= e.Left + e.Width  ) and
           ( _y >= e.Top  ) and ( _y <= e.Top + e.Height ) ) then begin
        result := e ;
        break ;
      end ;
    end ;
  end ;

  procedure T_ControlPrintTemplate.ApplyBounds(
    const _element : T_ResizeElement ;
    const _rect    : TRect
  ) ;
  var
    area_left : Integer ;
    area_top  : Integer ;

    function get_size ( _value : Integer ) : Integer ;
    begin
      Result := RoundS( _value * 96 * FSizeRatio / FPPI ) ;
    end;

  begin
    // scaling print area first to keep compatibility with preview and printing
    area_left := get_size( FPrintArea.Left ) ;
    area_top  := get_size( FPrintArea.Top  ) ;

    _element.BoundsRect := Rect( area_left + get_size( _rect.Left ),
                                 area_top  + get_size( _rect.Top ),
                                 area_left + get_size( _rect.Right ) + 1,
                                 area_top  + get_size( _rect.Bottom ) + 1 ) ;
    if assigned( FFocusControl ) and
       ( FFocusControl.ResizeElement = _element ) then
    begin
      FFocusControl.Left := _element.Left ;
      FFocusControl.Top  := _element.Top ;
      FFocusControl.Width  := _element.Width ;
      FFocusControl.Height := _element.Height ;
    end;
  end ;

  procedure T_ControlPrintTemplate.MoveFocusElementLeft ;
  begin
    if assigned( FFocusControl ) then
      FFocusControl.MoveLeft ;
  end ;

  procedure T_ControlPrintTemplate.MoveFocusElementRight ;
  begin
    if assigned( FFocusControl ) then
      FFocusControl.MoveRight ;
  end ;

  procedure T_ControlPrintTemplate.MoveFocusElementUp ;
  begin
    if assigned( FFocusControl ) then
      FFocusControl.MoveUp ;
  end ;

  procedure T_ControlPrintTemplate.MoveFocusElementDown ;
  begin
    if assigned( FFocusControl ) then
      FFocusControl.MoveDown ;
  end ;
{$ENDREGION}

{$REGION 'T_TemplateObjectInspector'}
//==============================================================================
// T_TemplateObjectInspector
//==============================================================================

  procedure T_TemplateObjectInspector.clearFields ;
  var
    i : Integer ;
    cntrl : TControl ;
  begin
    for i := ControlCount-1 downto 0 do begin
      cntrl := Controls[i] ;
      RemoveControl( cntrl ) ;
      FreeObject( cntrl ) ;
    end ;
  end;

  procedure T_TemplateObjectInspector.doMouseMove(
    _sender : TObject ;
    _shift  : TShiftState ;
    _x, _y  : Integer
  ) ;
  begin
    if _sender is TEdit then
      TEdit(_sender).Hint := TEdit(_sender).Text
    else
    if _sender is TGIS_FontStylePanel then
      TGIS_FontStylePanel(_sender).Hint := TGIS_FontStylePanel(_sender).Caption ;
  end ;

  procedure T_TemplateObjectInspector.doResize(
    _sender : TObject
  ) ;
  var
    i      : Integer ;
    cntrl  : TControl ;
    column : Integer ;
  begin
    column := 0 ;
    for i := 0 to ControlCount-1 do begin
      cntrl := Controls[i] ;
      if Self.BiDiMode = bdRightToLeft then begin
        if ( cntrl is TLabel ) then begin
          column := cntrl.Width ;
          PlaceControl( BiDiMode, nil, cntrl, 0, column ) ;
        end else
          PlaceControl( BiDiMode, nil, cntrl, column, ClientWidth - column - 1 ) ;
      end else
        if not ( cntrl is TLabel ) then begin
          cntrl.Width := ClientWidth - cntrl.Left ;
      end;
    end ;
  end;

  procedure T_TemplateObjectInspector.cmbDrawItem(
    _control : TWinControl ;
    _idx     : Integer ;
    _rect    : TRect   ;
    _state   : TOwnerDrawState
  ) ;
  var
    tf  : TTextFormat ;
    cmb : TComboBox ;
    txt : String ;
    rct : TRect ;
  begin
    if BiDiMode = TBiDiMode.bdRightToLeft then
      tf := [tfRtlReading,tfRight]
    else
      tf := [] ;

    cmb := TComboBox( _control ) ;
    cmb.Canvas.FillRect( _rect ) ;

    txt := cmb.Items[_idx] ;
    rct := Rect( _rect.Left+2, _rect.Top+1, _rect.Right-2, _rect.Bottom-1 ) ;
    cmb.Canvas.TextRect( rct, txt, tf ) ;
  end ;

  constructor T_TemplateObjectInspector.Create(
    _owner : TComponent
  ) ;
  begin
    inherited ;
    FOnInit     := nil ;
    FOnChange   := nil ;
    FOnClick    := nil ;
    FOnKeyPress := nil ;
    FOnEnter    := nil ;
    FOnExit     := nil ;
    FOnCustom   := nil ;
    anchorCache := TGIS_AnchorCache.Create ;
    OnResize := doResize ;
    HorzScrollBar.Visible := false ;
  end ;

  destructor T_TemplateObjectInspector.Destroy ;
  begin
    FreeObject( anchorCache ) ;
    inherited ;
  end ;

  procedure T_TemplateObjectInspector.Clear ;
  begin
    clearFields ;
  end ;

  procedure T_TemplateObjectInspector.ShowElement(
    _element : TGIS_PrintLayoutElement
  ) ;
  var
    frm : TCustomForm ;
    t : Integer ;
    rc_left : Integer ;

    function add_label(
      _name    : String ;
      _caption : String
    ) : TLabel ;
    begin
      Result := TLabel.Create( Self ) ;
      Result.Parent := Self ;
      Result.Name := 'lbl' + _name ;
      Result.Caption := _caption ;
      PlaceControl( BiDiMode, nil, Result, 0, rc_left ) ;
    end;

    procedure add_info(
      _name  : String ;
      _label : String ;
      _value : String
    ) ;
    var
      lbl : TLabel ;
      edt : TEdit ;
    begin
      lbl := add_label( _name, _label ) ;

      edt := TEdit.Create( Self );
      edt.Parent := Self ;
      edt.Name := _name ;
      edt.Top := t ;
      lbl.Top := t + (edt.Height - lbl.Height) div 2 ;
      PlaceControl( BiDiMode, nil, edt, rc_left, ClientWidth - rc_left - 1 ) ;
      lbl.FocusControl := edt ;
      edt.BevelKind := bkNone ;
      edt.Text := _value ;
      t := t + edt.Height + 2 ;

      if ( _name = GIS_TPL_DESIGNER_FLD_NAME ) then begin
        edt.OnKeyPress := FOnKeyPress ;
        edt.OnEnter := FOnEnter ;
        edt.OnExit := FOnExit ;
      end ;
      if ( _name = GIS_TPL_DESIGNER_FLD_TEXT ) then begin
        edt.OnKeyPress := FOnKeyPress ;
        edt.OnEnter := FOnEnter ;
        edt.OnExit := FOnExit ;
        edt.ShowHint := True ;
        edt.OnMouseMove := doMouseMove ;
        if assigned( FOnInit ) then
          FOnInit( edt, _element ) ;
      end;
    end;

    procedure add_index(
      _name     : String ;
      _label    : String ;
      _ctrlname : String ;
      _value    : Integer
    ) ;
    var
      lbl : TLabel ;
      cmb : TComboBox ;
    begin
      lbl := add_label( _name, _label ) ;

      cmb := TComboBox.Create( Self ) ;
      cmb.Parent := Self ;
      cmb.Name := _ctrlname ;
      cmb.Top := t ;
      PlaceControl( BiDiMode, nil, cmb, rc_left, ClientWidth - rc_left - 1 ) ;
      lbl.Top := t + (cmb.Height - lbl.Height) div 2 ;
      cmb.Style := csOwnerDrawFixed ;
      if assigned( FOnInit ) then
        FOnInit( cmb, _element ) ;
      lbl.FocusControl := cmb ;
      cmb.OnChange := FOnChange ;
      cmb.OnDrawItem := cmbDrawItem ;
      t := t + cmb.Height + 2 ;
    end ;

    procedure add_position(
      _name  : String ;
      _label : String ;
      _value : String
    ) ;
    var
      lbl : TLabel ;
      edt : TEdit ;
      pnl : TGIS_PositionPanel ;
    begin
      lbl := add_label( _name, _label ) ;

      pnl := TGIS_PositionPanel.Create( Self ) ;
      pnl.Parent := Self ;
      pnl.Name := _name ;
      pnl.Top := t ;
      PlaceControl( BiDiMode, nil, pnl, rc_left, ClientWidth - rc_left - 1 ) ;
      pnl.Caption := '' ;
      pnl.BorderStyle := bsSingle ;
      pnl.BevelKind := bkNone ;
      pnl.BevelOuter  := bvNone ;
      pnl.BevelInner  := bvNone ;
      pnl.Ctl3D := False ;
      pnl.ParentBackground := False ;
      edt := TEdit.Create( Self ) ;
      pnl.Color := edt.Color ;
      pnl.Height := edt.Height ;
      lbl.Top := t + (pnl.Height - lbl.Height) div 2 ;
      FreeObject( edt ) ;
      lbl.FocusControl := pnl ;
      pnl.AnchorCache := anchorCache ;
      pnl.Value := _value ;
      pnl.OnClick := FOnClick ;
      t := t + pnl.Height + 2 ;
    end ;

    procedure add_color(
      _name  : String ;
      _label : String ;
      _value : String ;
      _add_none : Boolean
    ) ;
    var
      lbl : TLabel ;
      edt : TEdit ;
      cmb : TGIS_ColorComboBox ;
    begin
      lbl := add_label( _name, _label ) ;

      cmb := TGIS_ColorComboBox.Create( Self );
      cmb.Parent := Self ;
      cmb.Name := _name ;
      cmb.Top := t ;
      PlaceControl( BiDiMode, nil, cmb, rc_left, ClientWidth - rc_left - 1 ) ;
      edt := TEdit.Create( Self ) ;
      cmb.Height := edt.Height ;
      cmb.ItemHeight := edt.Height ;
      lbl.Top := t + (cmb.Height - lbl.Height) div 2 ;
      FreeObject( edt ) ;
      lbl.FocusControl := cmb ;
      cmb.Fill( False, False ) ;
      if _add_none then
        cmb.Value := TGIS_PrintUtils.ColorToText( TGIS_Color.None ) ;
      cmb.Value := _value ;
      cmb.OnChange := FOnChange ;
      cmb.CustomEvent := FOnCustom ;
      t := t + cmb.Height + 2 ;
    end ;

    procedure add_size(
      _name  : String ;
      _label : String ;
      _value : String
    ) ;
    var
      lbl : TLabel ;
      cmb : TGIS_SizeComboBox ;
    begin
      lbl := add_label( _name, _label ) ;

      cmb := TGIS_SizeComboBox.Create( Self ) ;
      cmb.Parent := Self ;
      cmb.Name := _name ;
      cmb.Top := t ;
      PlaceControl( BiDiMode, nil, cmb, rc_left, ClientWidth - rc_left - 1 ) ;
      lbl.Top := t + (cmb.Height - lbl.Height) div 2 ;
      lbl.FocusControl := cmb ;
      cmb.Fill( True, False, False, False ) ;
      cmb.Value := _value ;
      cmb.OnChange    := FOnChange ;
      cmb.CustomEvent := FOnCustom ;
      t := t + cmb.Height + 2 ;
    end ;

    procedure add_font(
      _name  : String ;
      _label : String ;
      _value : String
    ) ;
    var
      lbl : TLabel ;
      cmb : TGIS_FontNameComboBox ;
    begin
      lbl := add_label( _name, _label ) ;

      cmb := TGIS_FontNameComboBox.Create( Self ) ;
      cmb.Parent := Self ;
      cmb.Name := _name ;
      cmb.Top := t ;
      PlaceControl( BiDiMode, nil, cmb, rc_left, ClientWidth - rc_left - 1 ) ;
      lbl.Top := t + (cmb.Height - lbl.Height) div 2 ;
      lbl.FocusControl := cmb ;
      cmb.Fill;
      cmb.Value := _value ;
      cmb.OnChange := FOnChange ;
      t := t + cmb.Height + 2 ;
    end ;

    procedure add_textalign(
      _name  : String ;
      _label : String ;
      _value : TGIS_LabelAlignment
    ) ;
    var
      lbl : TLabel ;
      cmb : TComboBox ;
    begin
      lbl := add_label( _name, _label ) ;

      cmb := TComboBox.Create( Self ) ;
      cmb.Parent := Self ;
      cmb.Name := _name ;
      cmb.Top := t ;
      PlaceControl( BiDiMode, nil, cmb, rc_left, ClientWidth - rc_left - 1 ) ;
      lbl.Top := t + (cmb.Height - lbl.Height) div 2 ;
      cmb.Style := csOwnerDrawFixed ;
      cmb.Items.BeginUpdate ;
      cmb.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_LEFTJUSTIFY ) ) ;
      cmb.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_CENTER ) ) ;
      cmb.Items.Add( _rsrcna( GIS_RS_LEGEND_PRM_RIGHTJUSTIFY ) ) ;
      cmb.Items.EndUpdate ;
      lbl.FocusControl := cmb ;
      cmb.OnChange := FOnChange ;
      cmb.OnDrawItem := cmbDrawItem ;
      case _value of
        TGIS_LabelAlignment.LeftJustify:  cmb.ItemIndex := 0 ;
        TGIS_LabelAlignment.Center:       cmb.ItemIndex := 1 ;
        TGIS_LabelAlignment.RightJustify: cmb.ItemIndex := 2 ;
      end;
      t := t + cmb.Height + 2 ;
    end ;

    procedure add_yesno(
      _name  : String ;
      _label : String ;
      _value : Boolean
    ) ;
    var
      lbl : TLabel ;
      cmb : TComboBox ;
    begin
      lbl := add_label( _name, _label ) ;

      cmb := TComboBox.Create( Self ) ;
      cmb.Parent := Self ;
      cmb.Name := _name ;
      cmb.Top := t ;
      PlaceControl( BiDiMode, nil, cmb, rc_left, ClientWidth - rc_left - 1 ) ;
      lbl.Top := t + (cmb.Height - lbl.Height) div 2 ;
      cmb.Style := csOwnerDrawFixed ;
      cmb.Items.BeginUpdate ;
      cmb.Items.Add( _rsrcna( GIS_RS_TPL_DESIGNER_FIELD_NO ) ) ;
      cmb.Items.Add( _rsrcna( GIS_RS_TPL_DESIGNER_FIELD_YES ) ) ;
      cmb.Items.EndUpdate ;
      lbl.FocusControl := cmb ;
      cmb.OnChange := FOnChange ;
      cmb.OnDrawItem := cmbDrawItem ;
      case _value of
        False :  cmb.ItemIndex := 0 ;
        True  :  cmb.ItemIndex := 1 ;
      end ;
      t := t + cmb.Height + 2 ;
    end ;

    procedure add_dividers(
      _name  : String ;
      _label : String ;
      _value : Integer
    ) ;
    var
      lbl : TLabel ;
      cmb : TComboBox ;
    begin
      lbl := add_label( _name, _label ) ;

      cmb := TComboBox.Create( Self ) ;
      cmb.Parent := Self ;
      cmb.Name := _name ;
      cmb.Top := t ;
      PlaceControl( BiDiMode, nil, cmb, rc_left, ClientWidth - rc_left - 1 ) ;
      lbl.Top := t + (cmb.Height - lbl.Height) div 2 ;
      cmb.Style := csOwnerDrawFixed ;
      cmb.Items.BeginUpdate ;
      cmb.Items.Add( '1' ) ;
      cmb.Items.Add( '2' ) ;
      cmb.Items.Add( '3' ) ;
      cmb.Items.Add( '4' ) ;
      cmb.Items.Add( '5' ) ;
      cmb.Items.Add( '6' ) ;
      cmb.Items.Add( '7' ) ;
      cmb.Items.Add( '8' ) ;
      cmb.Items.Add( '9' ) ;
      cmb.Items.Add( '10' ) ;
      cmb.Items.EndUpdate ;
      lbl.FocusControl := cmb ;
      cmb.OnChange := FOnChange ;
      cmb.OnDrawItem := cmbDrawItem ;
      if ( _value >= 1 ) and ( _value <= cmb.Items.Count ) then
        cmb.ItemIndex := _value - 1
      else
        cmb.ItemIndex := 5 - 1 ;
      t := t + cmb.Height + 2 ;
    end ;

    procedure add_nstyle(
      _name  : String ;
      _label : String ;
      _value : TGIS_ControlNorthArrowStyle
    ) ;
    var
      lbl : TLabel ;
      cmb : TComboBox ;
    begin
      lbl := add_label( _name, _label ) ;

      cmb := TComboBox.Create( Self ) ;
      cmb.Parent := Self ;
      cmb.Name := _name ;
      cmb.Top := t ;
      PlaceControl( BiDiMode, nil, cmb, rc_left, ClientWidth - rc_left - 1 ) ;
      lbl.Top := t + (cmb.Height - lbl.Height) div 2 ;
      cmb.Style := csOwnerDrawFixed ;
      cmb.Items.BeginUpdate ;
      cmb.Items.Add( ConstructParamNorthArrowStyle(TGIS_ControlNorthArrowStyle.Arrow1) ) ;
      cmb.Items.Add( ConstructParamNorthArrowStyle(TGIS_ControlNorthArrowStyle.Arrow2) ) ;
      cmb.Items.Add( ConstructParamNorthArrowStyle(TGIS_ControlNorthArrowStyle.Needle1) ) ;
      cmb.Items.Add( ConstructParamNorthArrowStyle(TGIS_ControlNorthArrowStyle.Needle2) ) ;
      cmb.Items.Add( ConstructParamNorthArrowStyle(TGIS_ControlNorthArrowStyle.Needle3) ) ;
      cmb.Items.Add( ConstructParamNorthArrowStyle(TGIS_ControlNorthArrowStyle.Rose1) ) ;
      cmb.Items.Add( ConstructParamNorthArrowStyle(TGIS_ControlNorthArrowStyle.Rose2) ) ;
      cmb.Items.Add( ConstructParamNorthArrowStyle(TGIS_ControlNorthArrowStyle.Rose3) ) ;
      cmb.Items.Add( ConstructParamNorthArrowStyle(TGIS_ControlNorthArrowStyle.Disk1) ) ;
      cmb.Items.Add( ConstructParamNorthArrowStyle(TGIS_ControlNorthArrowStyle.Disk2) ) ;
      cmb.Items.Add( ConstructParamNorthArrowStyle(TGIS_ControlNorthArrowStyle.Disk3) ) ;
      cmb.Items.Add( ConstructParamNorthArrowStyle(TGIS_ControlNorthArrowStyle.Triangle1) ) ;
      cmb.Items.EndUpdate ;
      lbl.FocusControl := cmb ;
      cmb.OnChange := FOnChange ;
      cmb.OnDrawItem := cmbDrawItem ;
      cmb.ItemIndex := Ord( _value ) ;
      t := t + cmb.Height + 2 ;
    end ;

    procedure add_legendiconstyle(
      _name  : String ;
      _label : String ;
      _value : TGIS_LegendIconStyle
    ) ;
    var
      lbl : TLabel ;
      cmb : TComboBox ;
    begin
      lbl := add_label( _name, _label ) ;

      cmb := TComboBox.Create( Self ) ;
      cmb.Parent := Self ;
      cmb.Name := _name ;
      cmb.Top := t ;
      PlaceControl( BiDiMode, nil, cmb, rc_left, ClientWidth - rc_left - 1 ) ;
      lbl.Top := t + (cmb.Height - lbl.Height) div 2 ;
      cmb.Style := csOwnerDrawFixed ;
      cmb.Items.BeginUpdate ;
      cmb.Items.Add( ConstructParamLegendIconStyle(TGIS_LegendIconStyle.Default) ) ;
      cmb.Items.Add( ConstructParamLegendIconStyle(TGIS_LegendIconStyle.Rectangular) ) ;
      cmb.Items.EndUpdate ;
      lbl.FocusControl := cmb ;
      cmb.OnChange := FOnChange ;
      cmb.OnDrawItem := cmbDrawItem ;
      cmb.ItemIndex := Ord( _value ) ;
      t := t + cmb.Height + 2 ;
    end ;

    procedure add_fstyle(
      _name  : String ;
      _label : String ;
      _value : String
    ) ;
    var
      lbl : TLabel ;
      edt : TEdit ;
      pnl : TGIS_FontStylePanel ;
    begin
      lbl := add_label( _name, _label ) ;

      pnl := TGIS_FontStylePanel.Create( Self ) ;
      pnl.Parent := Self ;
      pnl.Name := _name ;
      pnl.Top := t ;
      PlaceControl( BiDiMode, nil, pnl, rc_left, ClientWidth - rc_left - 1 ) ;
      pnl.Caption := _value ;
      pnl.Alignment := taLeftJustify ;
      pnl.BorderStyle := bsSingle ;
      pnl.BevelKind := bkNone ;
      pnl.BevelOuter  := bvNone ;
      pnl.BevelInner  := bvNone ;
      pnl.Ctl3D := False ;
      pnl.ParentBackground := False ;
      edt := TEdit.Create( Self ) ;
      pnl.Color := edt.Color ;
      pnl.Height := edt.Height ;
      lbl.Top := t + (pnl.Height - lbl.Height) div 2 ;
      FreeObject( edt ) ;
      lbl.FocusControl := pnl ;
      pnl.ShowHint := True ;
      pnl.OnClick := FOnClick ;
      pnl.OnMouseMove := doMouseMove ;
      t := t + pnl.Height + 2 ;
    end;

    procedure add_path(
      _name   : String ;
      _label  : String ;
      _button : String ;
      _value  : String
    ) ;
    var
      lbl : TLabel ;
      edt : TEdit ;
      btn : T_PathButton ;
    begin
      lbl := add_label( _name, _label ) ;

      edt := TEdit.Create( Self );
      edt.Parent := Self ;
      edt.Name := _name ;
      edt.Top := t ;
      lbl.Top := t + (edt.Height - lbl.Height) div 2 ;
      lbl.FocusControl := edt ;
      edt.BevelKind := bkNone ;
      edt.Text := _value ;

      btn := T_PathButton.Create( Self ) ;
      btn.Parent := Self ;
      btn.Name := _button ;
      btn.Top := t - 1 ;
      btn.Height := edt.Height + 2 ;
      btn.Caption := '...' ;
      btn.Edit := edt ;
      PlaceControl( BiDiMode, nil, edt, rc_left, ClientWidth - rc_left - btn.Height ) ;
      PlaceControl( BiDiMode, edt, btn, 0, btn.Height ) ;
      t := t + edt.Height + 2 ;

      edt.OnKeyPress := FOnKeyPress ;
      edt.OnEnter := FOnEnter ;
      edt.OnExit := FOnExit ;
      edt.ShowHint := True ;
      edt.OnMouseMove := doMouseMove ;
      btn.OnClick := FOnClick ;

      if assigned( FOnInit ) then begin
        FOnInit( edt, _element ) ;
        FOnInit( btn, _element ) ;
      end ;
    end ;

    procedure add_edit( _name: String; _label: String ) ;
    var
      lbl : TLabel ;
      edt : TEdit ;
    begin
      lbl := add_label(_name, _label);

      edt := TEdit.Create(Self);
      edt.Parent := Self ;
      edt.Name := _name ;
      edt.Top := t ;
      PlaceControl(BiDiMode, nil, edt, rc_left, ClientWidth - rc_left - 1);
      lbl.Top := t + (edt.Height - lbl.Height) div 2;
      lbl.FocusControl := edt;
      t := t + edt.Height + 2;

      edt.OnKeyPress := FOnKeyPress ;
      edt.OnEnter := FOnEnter ;
      edt.OnExit := FOnExit ;
      edt.ShowHint := True ;
      edt.OnMouseMove := doMouseMove ;

      if assigned( FOnInit ) then
        FOnInit( edt, _element ) ;
  end ;

  begin
    frm := GetParentForm( Self ) ;
    if assigned( frm ) and ( frm is TGIS_ControlPrintTemplateDesignerForm ) then
      TGIS_ControlPrintTemplateDesignerForm(frm).lockWindow
    else
      frm := nil ;
    try
      clearFields ;
      if not assigned( _element ) then exit ;
      t := 0 ;
      rc_left := 90 ;
      add_info( GIS_TPL_DESIGNER_FLD_NAME,  _rsrc( GIS_RS_TPL_DESIGNER_FIELD_NAME ),
                _element.Name ) ;

      case _element.ElementType of
        TGIS_PrintLayoutElementType.Map : begin
          add_index( GIS_TPL_DESIGNER_FLD_INDEX, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_INDEX ),
                     GIS_TPL_DESIGNER_FLD_INDEX_MAP, _element.Index ) ;
        end ;

        TGIS_PrintLayoutElementType.Legend : begin
          add_index( GIS_TPL_DESIGNER_FLD_INDEX, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_INDEX ),
                     GIS_TPL_DESIGNER_FLD_INDEX_LEGEND, _element.Index ) ;
        end ;

        TGIS_PrintLayoutElementType.Scale : begin
          add_index( GIS_TPL_DESIGNER_FLD_INDEX, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_INDEX ),
                     GIS_TPL_DESIGNER_FLD_INDEX_SCALE, _element.Index ) ;
        end ;

        TGIS_PrintLayoutElementType.NorthArrow : begin
          add_index( GIS_TPL_DESIGNER_FLD_INDEX, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_INDEX ),
                     GIS_TPL_DESIGNER_FLD_INDEX_NORTHARROW, _element.Index ) ;
        end ;

        TGIS_PrintLayoutElementType.Text : begin
          add_index( GIS_TPL_DESIGNER_FLD_INDEX, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_INDEX ),
                     GIS_TPL_DESIGNER_FLD_INDEX_TEXT, _element.Index ) ;
          add_info( GIS_TPL_DESIGNER_FLD_TEXT, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_TEXT ),
                    TGIS_PrintLayoutText(_element).Text ) ;
        end ;

        TGIS_PrintLayoutElementType.Graphic : begin
          add_index( GIS_TPL_DESIGNER_FLD_INDEX, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_INDEX ),
                     GIS_TPL_DESIGNER_FLD_INDEX_GRAPHICS, _element.Index ) ;
          add_path( GIS_TPL_DESIGNER_FLD_GRAPHICSPATH, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_PATH ),
                    GIS_TPL_DESIGNER_FLD_GRAPHICSPATH_BTN,
                    TGIS_PrintLayoutGraphic(_element).Path ) ;
        end ;

      end ;

      add_position( GIS_TPL_DESIGNER_FLD_LEFT,   _rsrc( GIS_RS_TPL_DESIGNER_FIELD_LEFT ),
                    _element.Location.Left.AsText
                  ) ;
      add_position( GIS_TPL_DESIGNER_FLD_TOP,    _rsrc( GIS_RS_TPL_DESIGNER_FIELD_TOP ),
                    _element.Location.Top.AsText
                  ) ;
      add_position( GIS_TPL_DESIGNER_FLD_RIGHT,  _rsrc( GIS_RS_TPL_DESIGNER_FIELD_RIGHT ),
                    _element.Location.Right.AsText
                  ) ;
      add_position( GIS_TPL_DESIGNER_FLD_BOTTOM, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_BOTTOM ),
                    _element.Location.Bottom.AsText
                  ) ;

      case _element.ElementType of
        TGIS_PrintLayoutElementType.Map:
          begin
            add_edit( GIS_TPL_DESIGNER_FLD_XMIN,
                      _rsrc( GIS_RS_TPL_DESIGNER_FIELD_XMIN ) ) ;
            add_edit( GIS_TPL_DESIGNER_FLD_YMIN,
                      _rsrc( GIS_RS_TPL_DESIGNER_FIELD_YMIN ) ) ;
            add_edit( GIS_TPL_DESIGNER_FLD_XMAX,
                      _rsrc( GIS_RS_TPL_DESIGNER_FIELD_XMAX ) ) ;
            add_edit( GIS_TPL_DESIGNER_FLD_YMAX,
                      _rsrc( GIS_RS_TPL_DESIGNER_FIELD_YMAX ) ) ;
          end ;

        TGIS_PrintLayoutElementType.Legend: begin
          add_yesno( GIS_TPL_DESIGNER_FLD_COMPACTVIEW, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_COMPACTVIEW ),
                     TGIS_PrintLayoutLegend(_element).CompactView
                   ) ;
          add_yesno( GIS_TPL_DESIGNER_FLD_REVERSEORDER, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_REVERSEORDER ),
                     TGIS_PrintLayoutLegend(_element).ReverseOrder
                   ) ;
          add_legendiconstyle
                   ( GIS_TPL_DESIGNER_FLD_DRAWICONSTYLE, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_DRAWICONSTYLE ),
                     TGIS_PrintLayoutLegend(_element).DrawIconStyle
                   ) ;
          add_font( GIS_TPL_DESIGNER_FLD_FONTNAME, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_FONT ),
                    TGIS_PrintLayoutLegend(_element).Font
                  ) ;
          add_size( GIS_TPL_DESIGNER_FLD_FONTSIZE, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_SIZE ),
                    TGIS_PrintUtils.PointsToText( TGIS_PrintLayoutLegend(_element).FontSize )
                  ) ;
          add_color( GIS_TPL_DESIGNER_FLD_TEXTCOLOR, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_COLOR ),
                     TGIS_PrintUtils.ColorToText( TGIS_PrintLayoutLegend(_element).FontColor ), False
                   ) ;
        end ;

        TGIS_PrintLayoutElementType.Scale: begin
          add_dividers(
                    GIS_TPL_DESIGNER_FLD_DIVIDERS, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_DIVIDERS ),
                    TGIS_PrintLayoutScale(_element).Dividers
                  ) ;
          add_color( GIS_TPL_DESIGNER_FLD_DIVIDERCOLOR1, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_DIVIDERCOLOR1 ),
                     TGIS_PrintUtils.ColorToText( TGIS_PrintLayoutScale(_element).DividerColor1 ), False
                   ) ;
          add_color( GIS_TPL_DESIGNER_FLD_DIVIDERCOLOR2, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_DIVIDERCOLOR2 ),
                     TGIS_PrintUtils.ColorToText( TGIS_PrintLayoutScale(_element).DividerColor2 ), False
                   ) ;
          add_font( GIS_TPL_DESIGNER_FLD_FONTNAME, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_FONT ),
                    TGIS_PrintLayoutScale(_element).Font
                  ) ;
          add_size( GIS_TPL_DESIGNER_FLD_FONTSIZE, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_SIZE ),
                    TGIS_PrintUtils.PointsToText( TGIS_PrintLayoutScale(_element).FontSize )
                  ) ;
          add_color( GIS_TPL_DESIGNER_FLD_TEXTCOLOR, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_COLOR ),
                     TGIS_PrintUtils.ColorToText( TGIS_PrintLayoutScale(_element).FontColor ), False
                   ) ;
        end ;

        TGIS_PrintLayoutElementType.NorthArrow: begin
          add_color( GIS_TPL_DESIGNER_FLD_COLOR1, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_COLOR1 ),
                     TGIS_PrintUtils.ColorToText( TGIS_PrintLayoutNorthArrow(_element).Color1 ), False
                   ) ;
          add_color( GIS_TPL_DESIGNER_FLD_COLOR2, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_COLOR2 ),
                     TGIS_PrintUtils.ColorToText( TGIS_PrintLayoutNorthArrow(_element).Color2 ), False
                   ) ;
          add_nstyle( GIS_TPL_DESIGNER_FLD_NARROWSTYLE, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_STYLE ),
                      TGIS_PrintLayoutNorthArrow(_element).Style
                    ) ;
          add_path( GIS_TPL_DESIGNER_FLD_NARROWPATH, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_PATH ),
                    GIS_TPL_DESIGNER_FLD_NARROWPATH_BTN,
                    TGIS_PrintLayoutNorthArrow(_element).Path
                  ) ;
        end ;

        TGIS_PrintLayoutElementType.Box: begin
          add_color( GIS_TPL_DESIGNER_FLD_BOXCOLOR, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_COLOR ),
                     TGIS_PrintUtils.ColorToText( TGIS_PrintLayoutBox(_element).Color ), True
                   ) ;
          add_color( GIS_TPL_DESIGNER_FLD_BOXFRAME, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_FRAME ),
                     TGIS_PrintUtils.ColorToText( TGIS_PrintLayoutBox(_element).FrameColor ), False
                   ) ;
          add_size( GIS_TPL_DESIGNER_FLD_BOXWIDTH, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_WIDTH ),
                    TGIS_PrintUtils.WidthToText( TGIS_PrintLayoutBox(_element).FrameWidth )
                  ) ;
        end ;

        TGIS_PrintLayoutElementType.Text: begin
          add_font( GIS_TPL_DESIGNER_FLD_FONTNAME, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_FONT ),
                    TGIS_PrintLayoutText(_element).Font
                  ) ;
          add_size( GIS_TPL_DESIGNER_FLD_FONTSIZE, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_SIZE ),
                    TGIS_PrintUtils.PointsToText( TGIS_PrintLayoutText(_element).Size )
                  ) ;
          add_fstyle( GIS_TPL_DESIGNER_FLD_FONTSTYLE, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_STYLE ),
                      TGIS_PrintUtils.FontStyleToText( TGIS_PrintLayoutText(_element).Style )
                    ) ;
          add_textalign( GIS_TPL_DESIGNER_FLD_TEXTALIGN, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_ALIGN ),
                         TGIS_PrintLayoutText(_element).Align
                       ) ;
          add_color( GIS_TPL_DESIGNER_FLD_TEXTCOLOR, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_COLOR ),
                     TGIS_PrintUtils.ColorToText( TGIS_PrintLayoutText(_element).Color ), False
                   ) ;
          add_color( GIS_TPL_DESIGNER_FLD_TEXTBGCOLOR, _rsrc( GIS_RS_TPL_DESIGNER_FIELD_BACKGROUND ),
                     TGIS_PrintUtils.ColorToText( TGIS_PrintLayoutText(_element).BackgroundColor ), True
                   ) ;
        end ;

      end;
    finally
      if assigned( frm ) then
        TGIS_ControlPrintTemplateDesignerForm(frm).unlockWindow ;
    end ;
  end ;

  function T_TemplateObjectInspector.FindAttribute(
    _attribute : String
  ) : TControl ;
  var
    i : Integer ;
    c : TControl ;
  begin
    Result := nil ;
    for i := 0 to ControlCount - 1 do begin
      c := Controls[i] ;
      if c.Name = _attribute then begin
        Result := c ;
        break ;
      end ;
    end ;
  end ;

  procedure T_TemplateObjectInspector.VisibleAttribute(
    _attribute : String ;
    _visible   : Boolean
  ) ;
  var
    c : TControl ;
  begin
    c := FindAttribute( 'lbl' + _attribute ) ;
    if assigned( c ) then
      c.Visible := _visible ;
    c := FindAttribute( _attribute ) ;
    if assigned( c ) then
      c.Visible := _visible ;
  end ;

  procedure T_TemplateObjectInspector.EnableAttribute(
    _attribute : String ;
    _enable    : Boolean
  ) ;
  var
    c : TControl ;
  begin
    c := FindAttribute( _attribute ) ;
    if assigned( c ) then
      c.Enabled := _enable ;
  end ;

  function T_TemplateObjectInspector.HasActiveControl(
    _activeControl : TControl
  ) : Boolean ;
  var
    i : Integer ;
  begin
    Result := False ;
    for i := 0 to Self.ControlCount - 1 do
      if Controls[i] = _activeControl then begin
        Result := True ;
        break ;
      end ;
    if Self = _activeControl then
      Result := True ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_ControlPrintTemplateDesignerForm'}
//==============================================================================
// TGIS_ControlPrintTemplateDesignerForm
//==============================================================================

  constructor TGIS_ControlPrintTemplateDesignerForm.Create(
    _owner : TComponent
  ) ;
  begin
    Create( _owner, True ) ;
  end;

  constructor TGIS_ControlPrintTemplateDesignerForm.Create(
    _owner    : TComponent ;
    _sizeable : Boolean
  ) ;
  begin
    inherited Create( _owner, _sizeable ) ;
    viewersCreated := False ;
    FViewers := nil ;
    FViewersEx := nil ;

    // set default snap
    Settings.Snap := TGIS_PrintLayoutSnap.CreateDefault ;
    Snap := '2mm' ;
  end;

  procedure TGIS_ControlPrintTemplateDesignerForm.initForm ;
  begin
    Self.BorderIcons := [biSystemMenu,biMaximize] ;
    Self.Caption := _rsrc( GIS_RS_TPL_DESIGNER ) ;
    Self.ClientHeight := 764 ;
    Self.ClientWidth := 882 ;
    Self.Name := 'TGIS_ControlPrintTemplateDesignerForm' ;
    Self.KeyPreview := True ;
    Self.Position := poScreenCenter ;
    Self.ScalingFlags := Self.ScalingFlags - [sfFont] ;
    Self.OnCloseQuery := FormCloseQuery ;
    Self.OnDestroy := FormDestroy ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.initControls ;
  var
    anchors   : TAnchors ;
    anchorsR  : TAnchors ;
    anchorsB  : TAnchors ;
    anchorsRB : TAnchors ;
    anchorsLR : TAnchors ;
    right_width  : Integer ;
    client_width : Integer ;
    lc_width : Integer ;
    btn_size : Integer ;
    bmp : TBitmap ;
    nd : TTreeNode ;

    procedure loadRes( const _name : String ) ;
    var
      rstm  : TResourceStream ;
    begin
      rstm := TResourceStream.Create(hInstance, _name, RT_RCDATA) ;
      try
        bmp.LoadFromStream( rstm ) ;
        imgList.AddMasked( bmp, bmp.TransparentColor ) ;
        imgList.DrawingStyle := TDrawingStyle.dsTransparent ;
      finally
        FreeObject( rstm ) ;
      end ;
    end ;

  begin
    imgList := TImageList.Create( Self ) ;
    imgListScld := TImageList.Create( Self ) ;
    bmp := TBitmap.Create ;
    try
      loadRes( 'TGIS_TEMPLATEDESIGNER_DOWN'       ) ;
      loadRes( 'TGIS_TEMPLATEDESIGNER_UP'         ) ;
      loadRes( 'TGIS_TEMPLATEDESIGNER_FRONT'      ) ;
      loadRes( 'TGIS_TEMPLATEDESIGNER_BACK'       ) ;
      loadRes( 'TGIS_TEMPLATEDESIGNER_DEL'        ) ;
      loadRes( 'TGIS_TEMPLATEDESIGNER_VIEWERWND'  ) ;
      loadRes( 'TGIS_TEMPLATEDESIGNER_LEGEND'     ) ;
      loadRes( 'TGIS_TEMPLATEDESIGNER_SCALE'      ) ;
      loadRes( 'TGIS_TEMPLATEDESIGNER_NORTHARROW' ) ;
      loadRes( 'TGIS_TEMPLATEDESIGNER_BOX'        ) ;
      loadRes( 'TGIS_TEMPLATEDESIGNER_TEXT'       ) ;
      loadRes( 'TGIS_TEMPLATEDESIGNER_GRAPHICS'   ) ;
      loadRes( 'TGIS_TEMPLATEDESIGNER_CHANGEEXT'  ) ;
      loadRes( 'TGIS_TEMPLATEDESIGNER_CHANGEEXTDISABLE'  ) ;
    finally
      FreeObject( bmp ) ;
    end ;

    if BiDiMode = bdRightToLeft then begin
      anchors   := [akRight, akTop] ;
      anchorsR  := [akLeft,  akTop] ;
      anchorsB  := [akRight, akBottom] ;
      anchorsRB := [akLeft,  akBottom] ;
    end else begin
      anchors   := [akLeft,  akTop] ;
      anchorsR  := [akRight, akTop] ;
      anchorsB  := [akLeft,  akBottom] ;
      anchorsRB := [akRight, akBottom] ;
    end ;
    anchorsLR := [akLeft, akTop, akRight] ;

    right_width := 190 ;
    client_width := 185 ;
    btn_size := 22 ;

    pnlTop := TPanel.Create( Self ) ;
    pnlTop.Parent := Self ;
    pnlTop.BevelOuter := bvNone ;
    pnlTop.Align  := alTop ;
    pnlTop.Height := 25 ;
    pnlTop.Anchors := [ akLeft, akTop, akRight ] ;
    pnlTop.TabOrder := 1 ;

    cmbPrinters := TComboBox.Create( pnlTop ) ;
    cmbPrinters.Parent := pnlTop ;
    cmbPrinters.Top := 3 ;
    PlaceControl( BiDiMode, nil, cmbPrinters, 8, 180 ) ;
    cmbPrinters.Anchors := anchors ;
    cmbPrinters.Style := csDropDownList ;
    cmbPrinters.OnChange := cmbPrintersChange ;

    chkLandscape := TCheckBox.Create( pnlTop ) ;
    chkLandscape.Parent := pnlTop ;
    chkLandscape.Top := 5 ;
    chkLandscape.Anchors := anchors ;
    chkLandscape.Caption := _rsrc( GIS_RS_TPL_DESIGNER_LANDSCAPE ) ;
    PlaceControl( BiDiMode, cmbPrinters, chkLandscape, 15, 70 ) ;
    chkLandscape.Checked := False ;
    chkLandscape.OnClick := chkLandscapeClick ;

    chkWireframe := TCheckBox.Create( pnlTop ) ;
    chkWireframe.Parent := pnlTop ;
    chkWireframe.Top := 5 ;
    chkWireframe.Anchors := anchors ;
    chkWireframe.Caption := _rsrc( GIS_RS_TPL_DESIGNER_WIREFRAME ) ;
    PlaceControl( BiDiMode, chkLandscape, chkWireframe, 10, 100 ) ;
    chkWireframe.Checked := False ;
    chkWireframe.OnClick := chkWireframeClick ;

    btnPreview := TButton.Create( pnlTop ) ;
    btnPreview.Parent := pnlTop ;
    btnPreview.Top := cmbPrinters.Top - 1 ;
    btnPreview.Height := cmbPrinters.Height - 3 ;
    PlaceControl( BiDiMode, chkWireframe, btnPreview, 10, 100 ) ;
    btnPreview.Anchors := anchors ;
    btnPreview.Caption := _rsrc( GIS_RS_TPL_DESIGNER_PREVIEW ) ;
    btnPreview.OnMouseDown := btnPreviewMouseDown ;
    btnPreview.OnMouseUp := btnPreviewMouseUp ;

    lblProgress := TLabel.Create( pnlTop ) ;
    lblProgress.Parent := pnlTop ;
    lblProgress.Top  := chkWireframe.Top ;
    lblProgress.Caption := '' ;
    PlaceControl( BiDiMode, btnPreview, lblProgress, 20, 100 ) ;
    lblProgress.Anchors := anchors ;

    btnStructureUp := T_SpeedButton.Create(pnlTop) ;
    btnStructureUp.Parent := pnlTop ;
    btnStructureUp.Top := 3 ;
    btnStructureUp.Height := btn_size ;
    PlaceControl( BiDiMode, nil, btnStructureUp, -(right_width-btn_size), btn_size ) ;
    btnStructureUp.Anchors := anchorsR ;
    T_SpeedButton(btnStructureUp).Images := imgListScld ;
    T_SpeedButton(btnStructureUp).ImageIndex := 1 ;
    T_SpeedButton(btnStructureUp).DisabledImageIndex := 1 ;
    btnStructureUp.ShowHint := True ;
    btnStructureUp.Hint := _rsrc( GIS_RS_TPL_DESIGNER_MOVEUP );
    btnStructureUp.OnClick := btnStructureUpClick ;

    btnStructureDown := T_SpeedButton.Create(pnlTop) ;
    btnStructureDown.Parent := pnlTop ;
    btnStructureDown.Top := 3 ;
    btnStructureDown.Height := btn_size ;
    PlaceControl( BiDiMode, nil, btnStructureDown, -(right_width-2*btn_size), btn_size ) ;
    btnStructureDown.Anchors := anchorsR ;
    T_SpeedButton(btnStructureDown).Images := imgListScld ;
    T_SpeedButton(btnStructureDown).ImageIndex := 0 ;
    T_SpeedButton(btnStructureDown).DisabledImageIndex := 0 ;
    btnStructureDown.ShowHint := True ;
    btnStructureDown.Hint := _rsrc( GIS_RS_TPL_DESIGNER_MOVEDOWN );
    btnStructureDown.OnClick := btnStructureDownClick ;

    btnStructureFront := T_SpeedButton.Create(pnlTop) ;
    btnStructureFront.Parent := pnlTop ;
    btnStructureFront.Top := 3 ;
    btnStructureFront.Height := btn_size ;
    PlaceControl( BiDiMode, nil, btnStructureFront, -(right_width-3*btn_size), btn_size ) ;
    btnStructureFront.Anchors := anchorsR ;
    T_SpeedButton(btnStructureFront).Images := imgListScld ;
    T_SpeedButton(btnStructureFront).ImageIndex := 2 ;
    T_SpeedButton(btnStructureFront).DisabledImageIndex := 2 ;
    btnStructureFront.ShowHint := True ;
    btnStructureFront.Hint := _rsrc( GIS_RS_TPL_DESIGNER_MOVEFRONT );
    btnStructureFront.OnClick := btnStructureFrontClick ;

    btnStructureBack := T_SpeedButton.Create(pnlTop) ;
    btnStructureBack.Parent := pnlTop ;
    btnStructureBack.Top := 3 ;
    btnStructureBack.Height := btn_size ;
    PlaceControl( BiDiMode, nil, btnStructureBack, -(right_width-4*btn_size), btn_size ) ;
    btnStructureBack.Anchors := anchorsR ;
    T_SpeedButton(btnStructureBack).Images := imgListScld ;
    T_SpeedButton(btnStructureBack).ImageIndex := 3 ;
    T_SpeedButton(btnStructureBack).DisabledImageIndex := 3 ;
    btnStructureBack.ShowHint := True ;
    btnStructureBack.Hint := _rsrc( GIS_RS_TPL_DESIGNER_MOVEBACK );
    btnStructureBack.OnClick := btnStructureBackClick ;

    btnStructureDel := T_SpeedButton.Create(pnlTop) ;
    btnStructureDel.Parent := pnlTop ;
    btnStructureDel.Top := 3 ;
    btnStructureDel.Height := btn_size ;
    PlaceControl( BiDiMode, nil, btnStructureDel, -(right_width-5*btn_size), btn_size ) ;
    btnStructureDel.Anchors := anchorsR ;
    T_SpeedButton(btnStructureDel).Images := imgListScld ;
    T_SpeedButton(btnStructureDel).ImageIndex := 4 ;
    T_SpeedButton(btnStructureDel).DisabledImageIndex := 4 ;
    btnStructureDel.ShowHint := True ;
    btnStructureDel.Hint := _rsrc( GIS_RS_TPL_DESIGNER_DELETE );
    btnStructureDel.OnClick := btnStructureDelClick ;

    btnStructureWnd := T_SpeedButton.Create(pnlTop) ;
    btnStructureWnd.Parent := pnlTop ;
    btnStructureWnd.Top := 3 ;
    btnStructureWnd.Height := btn_size ;
    PlaceControl( BiDiMode, nil, btnStructureWnd, -(right_width-6*btn_size), btn_size ) ;
    btnStructureWnd.Anchors := anchorsR ;
    T_SpeedButton(btnStructureWnd).Images := imgListScld ;
    T_SpeedButton(btnStructureWnd).ImageIndex := 12 ;
    T_SpeedButton(btnStructureWnd).DisabledImageIndex := 13 ;
    btnStructureWnd.ShowHint := True ;
    btnStructureWnd.Hint := _rsrc( GIS_RS_TPL_DESIGNER_VIEWERWND );
    btnStructureWnd.OnClick := btnStructureWndClick ;

    pnlRight := TPanel.Create( Self );
    pnlRight.Parent := Self ;
    if BiDiMode = bdRightToLeft then
      pnlRight.Align := alLeft
    else
      pnlRight.Align := alRight ;
    pnlRight.Width := right_width ;
    if BiDiMode = bdRightToLeft then
      pnlRight.Anchors := [ akLeft, akTop, akBottom ]
    else
      pnlRight.Anchors := [ akRight, akTop, akBottom ] ;
    pnlRight.BevelOuter  := bvNone ;
    pnlRight.BevelInner  := bvNone ;
    pnlRight.BorderStyle := bsNone ;

    pnlRightRight := TPanel.Create( pnlRight ) ;
    pnlRightRight.Parent := pnlRight ;
    pnlRightRight.Width := right_width - client_width ;
    if BiDiMode = bdRightToLeft then
      pnlRightRight.Align := alLeft
    else
      pnlRightRight.Align := alRight ;
    pnlRightRight.BevelOuter  := bvNone ;
    pnlRightRight.BevelInner  := bvNone ;
    pnlRightRight.BorderStyle := bsNone ;

    pnlRightClient := T_ScaleablePanel.Create( pnlRight );
    pnlRightClient.Parent := pnlRight ;
    pnlRightClient.Align := alClient ;
    pnlRightClient.BevelOuter  := bvNone ;
    pnlRightClient.BevelInner  := bvNone ;
    pnlRightClient.BorderStyle := bsNone ;
    T_ScaleablePanel(pnlRightClient).PanelTop := nil ;
    T_ScaleablePanel(pnlRightClient).PanelBottom := nil ;

    splitter := TSplitter.Create( Self ) ;
    splitter.Parent := Self ;
    if BiDiMode = bdRightToLeft then begin
      splitter.Align := alLeft ;
      splitter.Left := pnlRight.Width ;
    end else begin
      splitter.Align := alRight ;
      splitter.Left := 0 ;
    end ;
    splitter.MinSize := 195 ;
    splitter.OnMoved := splitterMoved ;
    splitter.AutoSnap := False ;
    splitter.Beveled := False ;
    splitter.ResizeStyle := rsPattern ;

    pnlStructure := TPanel.Create( pnlRightClient );
    pnlStructure.Parent := pnlRightClient ;
    pnlStructure.Height := 148 ;
    pnlStructure.Align := alTop ;
    pnlStructure.Constraints.MinHeight := 100 ;
    pnlStructure.BevelOuter  := bvNone ;
    pnlStructure.BevelInner  := bvNone ;
    pnlStructure.BorderStyle := bsNone ;

    pnlStructureTop := TPanel.Create( pnlStructure );
    pnlStructureTop.Parent := pnlStructure ;
    pnlStructureTop.Height := 18 ;
    pnlStructureTop.Align := alTop ;
    pnlStructureTop.FullRepaint := False ;
    pnlStructureTop.BevelOuter  := bvNone ;
    pnlStructureTop.BevelInner  := bvNone ;
    pnlStructureTop.BorderStyle := bsNone ;

    lblStructure := TLabel.Create( pnlStructureTop );
    lblStructure.Parent := pnlStructureTop ;
    lblStructure.Top  := 4 ;
    PlaceControl( BiDiMode, nil, lblStructure, 0, client_width ) ;
    lblStructure.Caption := _rsrc( GIS_RS_TPL_DESIGNER_STRUCTURE ) ;

    tvStructure := TTreeView.Create( pnlStructure ) ;
    tvStructure.Parent := pnlStructure ;
    tvStructure.Align := alClient ;
    tvStructure.Images := imgListScld ;
    tvStructure.ReadOnly := True ;
    tvStructure.ShowButtons := False ;
    tvStructure.HideSelection := False ;
    tvStructure.OnChange := tvStructureChange ;

    pnlObjectInspector := TPanel.Create( pnlRightClient );
    pnlObjectInspector.Parent := pnlRightClient ;
    pnlObjectInspector.Top := 148 ;
    pnlObjectInspector.Height := 335 ;
    pnlObjectInspector.Align := alClient ;
    pnlObjectInspector.Constraints.MinHeight := 150 ;
    pnlObjectInspector.BevelOuter  := bvNone ;
    pnlObjectInspector.BevelInner  := bvNone ;
    pnlObjectInspector.BorderStyle := bsNone ;

    pnlObjectInspectorBg := TPanel.Create( pnlObjectInspector );
    pnlObjectInspectorBg.Parent := pnlObjectInspector ;
    pnlObjectInspectorBg.Top := 0 ;
    pnlObjectInspectorBg.Height := 73 ;
    pnlObjectInspectorBg.Align := alTop ;
    pnlObjectInspectorBg.FullRepaint := False ;
    pnlObjectInspectorBg.BevelOuter  := bvNone ;
    pnlObjectInspectorBg.BevelInner  := bvNone ;
    pnlObjectInspectorBg.BorderStyle := bsNone ;

    lblObjectInspector := TLabel.Create( pnlObjectInspectorBg );
    lblObjectInspector.Parent := pnlObjectInspectorBg ;
    lblObjectInspector.Top := 6 ;
    PlaceControl( BiDiMode, nil, lblObjectInspector, 0, client_width ) ;
    lblObjectInspector.Caption := _rsrc( GIS_RS_TPL_DESIGNER_OBJECTINSPECTOR ) ;

    // snapping data
    lc_width := 90 ;
    lblObjectInspectorSnap := TLabel.Create( pnlObjectInspectorBg );
    lblObjectInspectorSnap.Parent := pnlObjectInspectorBg ;
    PlaceControl( BiDiMode, nil, lblObjectInspectorSnap, 0, lc_width ) ;
    lblObjectInspectorSnap.Caption := _rsrc( GIS_RS_TPL_DESIGNER_OBJECTINSPECTOR_SNAP ) ;
    cmbObjectInspectorSnap := TGIS_SizeComboBox.Create( pnlObjectInspectorBg );
    cmbObjectInspectorSnap.Parent := pnlObjectInspectorBg ;
    cmbObjectInspectorSnap.Top := 22 ;
    PlaceControl( BiDiMode, nil, cmbObjectInspectorSnap, lc_width, client_width - lc_width - 1 ) ;
    lblObjectInspectorSnap.Top := cmbObjectInspectorSnap.Top +
                                  ( cmbObjectInspectorSnap.Height - lblObjectInspectorSnap.Height ) div 2 ;
    lblObjectInspectorSnap.FocusControl := cmbObjectInspectorSnap ;
    cmbObjectInspectorSnap.FillSnap ;
    cmbObjectInspectorSnap.OnChange := cmbObjectInspectorSnapChange ;
    cmbObjectInspectorSnap.CustomEvent := cmbObjectInspectorSnapCustom ;

    cmbObjectInspector := TComboBox.Create( pnlObjectInspectorBg );
    cmbObjectInspector.Parent := pnlObjectInspectorBg ;
    cmbObjectInspector.Top  := 49 ;
    PlaceControl( BiDiMode, nil, cmbObjectInspector, 0, client_width ) ;
    cmbObjectInspector.Style := TComboBoxStyle.csDropDownList ;
    cmbObjectInspector.BevelOuter := bvNone ;
    cmbObjectInspector.BevelInner := bvNone ;
    cmbObjectInspector.TabStop := False ;
    cmbObjectInspector.OnChange := cmbObjectInspectorChange ;

    tplObjectInspector := T_TemplateObjectInspector.Create( pnlObjectInspector ) ;
    tplObjectInspector.Parent := pnlObjectInspector ;
    tplObjectInspector.Top := 73 ;
    tplObjectInspector.Height := 400 ;
    tplObjectInspector.Align := alClient ;
    tplObjectInspector.BevelOuter  := bvNone ;
    tplObjectInspector.BevelInner  := bvNone ;
    tplObjectInspector.BorderStyle := bsNone ;
    tplObjectInspector.InitEvent   := tplObjectInspectorInit ;
    tplObjectInspector.ChangeEvent := tplObjectInspectorChange ;
    tplObjectInspector.ClickEvent  := tplObjectInspectorClick ;
    tplObjectInspector.KeyPressEvent
                                   := tplObjectInspectorKeyPress ;
    tplObjectInspector.EnterEvent  := tplObjectInspectorEnter ;
    tplObjectInspector.ExitEvent   := tplObjectInspectorExit ;
    tplObjectInspector.CustomEvent := tplObjectInspectorCustom ;
    tplObjectInspector.TabStop := False ;

    pnlToolPalette := TPanel.Create( pnlRightClient );
    pnlToolPalette.Parent := pnlRightClient ;
    pnlToolPalette.Top := 505 ;
    pnlToolPalette.Height := 217 ;
    pnlToolPalette.Align := alBottom ;
    pnlToolPalette.Constraints.MinHeight := 100 ;
    pnlToolPalette.BevelOuter  := bvNone ;
    pnlToolPalette.BevelInner  := bvNone ;
    pnlToolPalette.BorderStyle := bsNone ;

    pnlToolPaletteTop := TPanel.Create( pnlToolPalette ) ;
    pnlToolPaletteTop.Parent := pnlToolPalette ;
    pnlToolPaletteTop.Height := 22 ;
    pnlToolPaletteTop.Align := alTop ;
    pnlToolPaletteTop.FullRepaint := False ;
    pnlToolPaletteTop.BevelOuter  := bvNone ;
    pnlToolPaletteTop.BevelInner  := bvNone ;
    pnlToolPaletteTop.BorderStyle := bsNone ;

    lblToolPalette := TLabel.Create( pnlToolPaletteTop );
    lblToolPalette.Parent := pnlToolPaletteTop ;
    lblToolPalette.Top  := 6 ;
    PlaceControl( BiDiMode, nil, lblToolPalette, 0, client_width ) ;
    lblToolPalette.Caption := _rsrc( GIS_RS_TPL_DESIGNER_TOOLPALETTE ) ;

    pnlToolPaletteBottom := TPanel.Create( pnlToolPalette ) ;
    pnlToolPaletteBottom.Parent := pnlToolPalette ;
    pnlToolPaletteBottom.Height := 4 ;
    pnlToolPaletteBottom.Align := alBottom ;
    pnlToolPaletteBottom.BevelOuter  := bvNone ;
    pnlToolPaletteBottom.BevelInner  := bvNone ;
    pnlToolPaletteBottom.BorderStyle := bsNone ;

    tvToolPalette := TTreeView.Create( pnlToolPalette ) ;
    tvToolPalette.Parent := pnlToolPalette ;
    tvToolPalette.Top  := 22 ;
    tvToolPalette.Height := 175 ;
    tvToolPalette.Images := imgListScld ;
    tvToolPalette.ReadOnly := True ;
    tvToolPalette.Align := alClient ;
    tvToolPalette.ShowButtons := False ;
    tvToolPalette.ShowLines := False ;
    tvToolPalette.HideSelection := False ;
    tvToolPalette.RowSelect := True ;
    tvToolPalette.Items.BeginUpdate;
    nd := tvToolPalette.Items.Add( nil, _rsrc( GIS_RS_TPL_DESIGNER_TOOLPALETTE_MAP ) ) ;
    nd.ImageIndex := 5 ;
    nd.SelectedIndex := 5 ;
    nd := tvToolPalette.Items.Add( nil, _rsrc( GIS_RS_TPL_DESIGNER_TOOLPALETTE_LEGEND ) ) ;
    nd.ImageIndex := 6 ;
    nd.SelectedIndex := 6 ;
    nd := tvToolPalette.Items.Add( nil, _rsrc( GIS_RS_TPL_DESIGNER_TOOLPALETTE_SCALE ) ) ;
    nd.ImageIndex := 7 ;
    nd.SelectedIndex := 7 ;
    nd := tvToolPalette.Items.Add( nil, _rsrc( GIS_RS_TPL_DESIGNER_TOOLPALETTE_NORTHARROW ) ) ;
    nd.ImageIndex := 8 ;
    nd.SelectedIndex := 8 ;
    nd := tvToolPalette.Items.Add( nil, _rsrc( GIS_RS_TPL_DESIGNER_TOOLPALETTE_BOX ) ) ;
    nd.ImageIndex := 9 ;
    nd.SelectedIndex := 9 ;
    nd := tvToolPalette.Items.Add( nil, _rsrc( GIS_RS_TPL_DESIGNER_TOOLPALETTE_TEXT ) ) ;
    nd.ImageIndex := 10 ;
    nd.SelectedIndex := 10 ;
    nd := tvToolPalette.Items.Add( nil, _rsrc( GIS_RS_TPL_DESIGNER_TOOLPALETTE_GRAPHIC ) ) ;
    nd.ImageIndex := 11 ;
    nd.SelectedIndex := 11 ;
    tvToolPalette.Items.EndUpdate ;
    tvToolPalette.OnMouseDown := tvToolPaletteMouseDown ;

    pnlPageControl := TPanel.Create( Self ) ;
    pnlPageControl.Parent := Self ;
    pnlPageControl.Align := alClient ;
    pnlPageControl.BevelOuter  := bvNone ;
    pnlPageControl.BevelInner  := bvNone ;
    pnlPageControl.BorderStyle := bsNone ;

    pageControl := TPageControl.Create( pnlPageControl ) ;
    pageControl.Parent := pnlPageControl ;
    pageControl.Align := alClient ;
    pageControl.Name := 'pageControl' ;
    pageControl.TabPosition := tpBottom ;
    pageControl.OnChange := pageControlChange ;
    Settings.PageControl := pageControl ;

    tabModel := TTabSheet.Create( pageControl ) ;
    tabModel.Parent := pageControl ;
    tabModel.Caption := _rsrc( GIS_RS_TPL_DESIGNER_MODEL ) ;
    tabModel.PageControl := pageControl ;
    tabModel.TabVisible := False ;
    pageControl.ActivePage := tabModel ;

    gisModel := T_ControlPrintTemplate.Create( tabModel );
    gisModel.Parent := tabModel ;
    gisModel.Align := alClient ;
    gisModel.ParentBackground := False ;
    gisModel.CreateShapeEvent := doCreateShape ;
    gisModel.AddShapeEvent := doAddShape ;
    gisModel.CancelShapeEvent := doCancelShape ;

    tabPreview := TTabSheet.Create( pageControl ) ;
    tabPreview.Parent := pageControl ;
    tabPreview.Caption := _rsrc( GIS_RS_TPL_DESIGNER_PREVIEW ) ;
    tabPreview.TabVisible := False ;
    tabPreview.PageControl := pageControl ;

    gisPreview := TGIS_ControlPrintPreviewTemplateDesigner.Create( tabPreview ) ;
    gisPreview.Parent := tabPreview ;
    gisPreview.Align := alClient ;
    gisPreview.BevelOuter := bvNone ;
    gisPreview.Color := clGray ;

    pnlBottom := TPanel.Create( Self) ;
    pnlBottom.Parent := Self ;
    pnlBottom.BevelOuter := bvNone ;
    pnlBottom.Align  := alBottom ;
    pnlBottom.Height := 40 ;
    pnlBottom.Anchors := [ akLeft, akBottom, akRight ] ;
    pnlBottom.TabOrder := 0 ;

    btnHelp.Parent := pnlBottom ;
    btnHelp.Top  := pnlBottom.Height - btnHelp.Height - 8 ;
    PlaceControl( BiDiMode, nil, btnHelp, 8, 80 ) ;

    btnCancel.Parent := pnlBottom ;
    btnCancel.Top  := pnlBottom.Height - btnCancel.Height - 8 ;
    PlaceControl( BiDiMode, nil, btnCancel, -8, 80 ) ;

    btnOK.Parent := pnlBottom ;
    btnOK.Top  := pnlBottom.Height - btnOK.Height - 8 ;
    PlaceControl( BiDiMode, btnCancel, btnOK, -8, 80 ) ;
    btnOK.Default := False ;

    btnSaveAs    := TButton.Create( pnlBottom ) ;
    btnSaveAs.Parent := pnlBottom ;
    btnSaveAs.Top  := pnlBottom.Height - btnOK.Height - 8 ;
    btnSaveAs.Caption := _rsrc( GIS_RS_TPL_DESIGNER_SAVEAS ) ;
    PlaceControl( BiDiMode, btnOK, btnSaveAs, -16, 80 ) ;
    btnSaveAs.Anchors := anchorsRB ;
    btnSaveAs.OnClick := btnSaveAsClick ;

    dlgSave := TSaveDialog.Create( Self ) ;
    dlgSave.Filter := _rsrcna( GIS_RS_TPL_DESIGNER_FILTER_SAVE ) ;
    dlgSave.DefaultExt := '*' + GIS_TTKTEMPLATE_EXT ;
    dlgSave.Options := [ofOverwritePrompt, ofHideReadOnly, ofEnableSizing] ;

    btnOpen := TButton.Create( pnlBottom ) ;
    btnOpen.Parent := pnlBottom ;
    btnOpen.Top  := pnlBottom.Height - btnOK.Height - 8 ;
    btnOpen.Caption := _rsrc( GIS_RS_TPL_DESIGNER_OPEN ) ;
    PlaceControl( BiDiMode, btnSaveAs, btnOpen, -8, 80 ) ;
    btnOpen.Anchors := anchorsRB ;
    btnOpen.OnClick := btnOpenClick ;

    dlgOpen := TOpenDialog.Create( Self ) ;
    dlgOpen.Filter := _rsrcna( GIS_RS_TPL_DESIGNER_FILTER_OPEN ) ;

    btnNew := TButton.Create( pnlBottom ) ;
    btnNew.Parent := pnlBottom ;
    btnNew.Top  := pnlBottom.Height - btnOK.Height - 8 ;
    btnNew.Caption := _rsrc( GIS_RS_TPL_DESIGNER_NEW ) ;
    PlaceControl( BiDiMode, btnOpen, btnNew, -8, 80 ) ;
    btnNew.Anchors := anchorsRB ;
    btnNew.OnClick := btnNewClick ;

    btnHelp.TabOrder := 0 ;
    btnNew.TabOrder := 1 ;
    btnOpen.TabOrder := 2 ;
    btnSaveAs.TabOrder := 3 ;
    btnOK.TabOrder := 4 ;
    btnCancel.TabOrder := 5 ;

    tmrShowForm := TTimer.Create( Self ) ;
    tmrShowForm.Interval := 1 ;
    tmrShowForm.OnTimer := doShowForm ;
    tmrShowForm.Enabled := False ;

    tmrLandscape := TTimer.Create( Self ) ;
    tmrLandscape.Interval := 1 ;
    tmrLandscape.OnTimer := doLandscape ;
    tmrLandscape.Enabled := False ;

    tmrPaint := TTimer.Create( Self ) ;
    tmrPaint.Interval := 1 ;
    tmrPaint.OnTimer := doPaint ;
    tmrPaint.Enabled := False ;

    T_ScaleablePanel(pnlRightClient).PanelTop := pnlStructure ;
    T_ScaleablePanel(pnlRightClient).PanelBottom := pnlToolPalette ;
    ActiveControl := pageControl ;

  end ;

  function TGIS_ControlPrintTemplateDesignerForm.fget_Snap
    : String ;
  begin
    Result := Settings.Snap.AsText ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.fset_Snap(
    const _value : String
  ) ;
  begin
    Settings.Snap.SetValues( _value ) ;
    cmbObjectInspectorSnap.Value := TGIS_PrintUtils.SnapToText( Settings.Snap ) ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.openTemplate ;
  var
    elm : TGIS_PrintLayoutElement ;
    shp : T_ResizeElement ;
    i   : Integer ;
  begin
    if not IsStringEmpty( FTemplate.TemplatePath ) then
      setTemplateName( GetFileName(FTemplate.TemplatePath) )
    else
      setTemplateName( 'untitled' + GIS_TTKTEMPLATE_EXT ) ;

    FBuilder.ClearElements ;
    gisModel.ClearElements ;

    tvStructure.Items.Clear ;
    cmbObjectInspector.Clear ;
    cmbObjectInspector.OnChange( Self ) ;
    tplObjectInspector.Clear ;

    FBuilder.ProcessTemplate(
      FPrinter.PPI,
      FPrinter.PrintArea,
      FPrinter.PageSize.X,
      FPrinter.PageSize.Y
    ) ;

    shp := nil ;
    for i := 0 to FBuilder.ElementsCount - 1  do begin
      elm := FBuilder.Elements[i];
      addElementToObjectInspector( elm ) ;
      shp := gisModel.AddElement ;
      gisModel.ApplyBounds( shp, elm.Location.Rectangle ) ;
      shp.RealBoundsChangeEvent := doRealBoundsChange ;
      shp.RealBoundsChangeExEvent := doRealBoundsChangeEx ;
      shp.ClickEvent := doClickShape ;
      shp.DrawMapEvent := doDrawMap ;
      shp.DrawControlEvent := doDrawControl ;
      shp.DrawGraphicEvent := doDrawGraphic ;
      shp.UserData := elm ;
      shp.Template := FTemplate ;
      shp.Caption  := cmbObjectInspector.Items[i] ;
      update_values( elm ) ;
    end;
    if FBuilder.ElementsCount > 0 then
      setElementInObjectInspector( gisModel.IndexOf( shp ) ) ;
    if pageControl.ActivePage = tabPreview then
      pageControl.ActivePage := tabModel ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.update_values(
    _element : TGIS_PrintLayoutElement
  ) ;
    procedure update_legend ;
    var
      idx : Integer ;
      legend : TGIS_PrintLayoutLegend ;
    begin
      idx := _element.Index ;
      legend := TGIS_PrintLayoutLegend( _element ) ;
      if assigned( FTemplate.GIS_Legend[idx] ) then begin
        legend.CompactView   := TGIS_ControlLegend(FTemplate.GIS_Legend[idx]).CompactView ;
        legend.DrawIconStyle := TGIS_ControlLegend(FTemplate.GIS_Legend[idx]).DrawIconStyle ;
        legend.ReverseOrder  := TGIS_ControlLegend(FTemplate.GIS_Legend[idx]).ReverseOrder ;
        legend.Font          := TGIS_ControlLegend(FTemplate.GIS_Legend[idx]).Font.Name ;
        legend.FontSize      := TGIS_ControlLegend(FTemplate.GIS_Legend[idx]).Font.Size ;
        legend.FontColor     := GisColor( TGIS_ControlLegend(FTemplate.GIS_Legend[idx]).Font.Color ) ;
      end ;
      legend.CompactView   := ParamBoolean( legend.CompactViewStr, legend.CompactView ) ;
      legend.DrawIconStyle := ParamLegendIconStyle( legend.DrawIconStyleStr, legend.DrawIconStyle ) ;
      legend.ReverseOrder  := ParamBoolean( legend.ReverseOrderStr, legend.ReverseOrder ) ;
      legend.Font          := ParamString( legend.FontStr, legend.Font ) ;
      legend.FontSize      := ParamInteger( legend.FontSizeStr, legend.FontSize ) ;
      legend.FontColor     := ParamColor( legend.FontColorStr, legend.FontColor ) ;
    end ;

    procedure update_scale ;
    var
      idx : Integer ;
      scale : TGIS_PrintLayoutScale ;
    begin
      idx := _element.Index ;
      scale := TGIS_PrintLayoutScale( _element ) ;
      if assigned( FTemplate.GIS_Scale[idx] ) then begin
        scale.Dividers      := TGIS_ControlScale( FTemplate.GIS_Scale[idx] ).Dividers ;
        scale.DividerColor1 := GisColor( TGIS_ControlScale( FTemplate.GIS_Scale[idx] ).DividerColor1 ) ;
        scale.DividerColor2 := GisColor( TGIS_ControlScale( FTemplate.GIS_Scale[idx] ).DividerColor2 ) ;
        scale.Font          := TGIS_ControlScale( FTemplate.GIS_Scale[idx] ).Font.Name ;
        scale.FontSize      := TGIS_ControlScale( FTemplate.GIS_Scale[idx] ).Font.Size ;
        scale.FontColor     := GisColor( TGIS_ControlScale( FTemplate.GIS_Scale[idx] ).Font.Color ) ;
      end ;
      scale.Dividers := ParamInteger( scale.DividersStr, scale.Dividers ) ;
      scale.DividerColor1 := ParamColor( scale.DividerColor1Str, scale.DividerColor1 ) ;
      scale.DividerColor2 := ParamColor( scale.DividerColor2Str, scale.DividerColor2 ) ;
      scale.Font          := ParamString( scale.FontStr, scale.Font ) ;
      scale.FontSize      := ParamInteger( scale.FontSizeStr, scale.FontSize ) ;
      scale.FontColor     := ParamColor( scale.FontColorStr, scale.FontColor ) ;
    end ;

    procedure update_northarrow ;
    var
      idx : Integer ;
      narrow : TGIS_PrintLayoutNorthArrow ;
    begin
      idx := _element.Index ;
      narrow := TGIS_PrintLayoutNorthArrow( _element ) ;
      if assigned( FTemplate.GIS_NorthArrow[idx] ) then begin
        narrow.Color1 := GisColor( TGIS_ControlNorthArrow(FTemplate.GIS_NorthArrow[idx]).Color1 ) ;
        narrow.Color2 := GisColor( TGIS_ControlNorthArrow(FTemplate.GIS_NorthArrow[idx]).Color2 ) ;
        narrow.Style  := TGIS_ControlNorthArrow(FTemplate.GIS_NorthArrow[idx]).Style ;
        narrow.Path   := TGIS_ControlNorthArrow(FTemplate.GIS_NorthArrow[idx]).Path ;
      end ;
      narrow.Color1 := ParamColor( narrow.Color1Str, narrow.Color1 ) ;
      narrow.Color2 := ParamColor( narrow.Color2Str, narrow.Color2 ) ;
      narrow.Style  := ParamNorthArrowStyle( narrow.StyleStr, narrow.Style ) ;
      narrow.Path   := ParamString( narrow.PathStr, narrow.Path ) ;
    end;

  begin
    case _element.ElementType of
      TGIS_PrintLayoutElementType.Legend     : update_legend ;
      TGIS_PrintLayoutElementType.Scale      : update_scale ;
      TGIS_PrintLayoutElementType.NorthArrow : update_northarrow ;
    end;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.prepare_default_values ;
  var
    i      : Integer ;
    legend : TGIS_PrintLayoutLegend ;
    scale  : TGIS_PrintLayoutScale ;
    narrow : TGIS_PrintLayoutNorthArrow ;
  begin
    SetLength( FViewers, FTemplate.GIS_ViewerCount + 1 ) ;
    for i := 0 to FTemplate.GIS_ViewerCount do
      FViewers[i] := nil ;
    SetLength( FViewersEx, FTemplate.GIS_ViewerCount + 1 ) ;
    for i := 0 to FTemplate.GIS_ViewerCount do
      FViewersEx[i] := nil ;
    if assigned( FTemplate.GIS_Legend[1] ) and
       ( FTemplate.GIS_Legend[1] is TGIS_ControlLegend ) then begin
      FDefaultLegend.CompactView   := TGIS_ControlLegend( FTemplate.GIS_Legend[1] ).CompactView ;
      FDefaultLegend.DrawIconStyle := TGIS_ControlLegend( FTemplate.GIS_Legend[1] ).DrawIconStyle ;
      FDefaultLegend.ReverseOrder  := TGIS_ControlLegend( FTemplate.GIS_Legend[1] ).ReverseOrder ;
      FDefaultLegend.Font          := TGIS_ControlLegend( FTemplate.GIS_Legend[1] ).Font.Name ;
      FDefaultLegend.FontSize      := TGIS_ControlLegend( FTemplate.GIS_Legend[1] ).Font.Size ;
      FDefaultLegend.FontColor     := GisColor(TGIS_ControlLegend( FTemplate.GIS_Legend[1] ).Font.Color) ;
    end else begin
      legend := TGIS_PrintLayoutLegend( TGIS_PrintLayoutLegend.CreateDefault ) ;
      try
        FDefaultLegend.CompactView   := legend.CompactView ;
        FDefaultLegend.DrawIconStyle := legend.DrawIconStyle ;
        FDefaultLegend.ReverseOrder  := legend.ReverseOrder ;
        FDefaultLegend.Font          := legend.Font ;
        FDefaultLegend.FontSize      := legend.FontSize ;
        FDefaultLegend.FontColor     := legend.FontColor ;
      finally
        FreeObject( legend ) ;
      end;
    end ;
    if assigned( FTemplate.GIS_Scale[1] ) and
       ( FTemplate.GIS_Scale[1] is TGIS_ControlScale ) then begin
      FDefaultScale.Dividers      := TGIS_ControlScale( FTemplate.GIS_Scale[1] ).Dividers ;
      FDefaultScale.DividerColor1 := GisColor(TGIS_ControlScale( FTemplate.GIS_Scale[1] ).DividerColor1) ;
      FDefaultScale.DividerColor2 := GisColor(TGIS_ControlScale( FTemplate.GIS_Scale[1] ).DividerColor2) ;
      FDefaultScale.Font          := TGIS_ControlScale( FTemplate.GIS_Scale[1] ).Font.Name ;
      FDefaultScale.FontSize      := TGIS_ControlScale( FTemplate.GIS_Scale[1] ).Font.Size ;
      FDefaultScale.FontColor     := GisColor(TGIS_ControlScale( FTemplate.GIS_Scale[1] ).Font.Color) ;
    end else begin
      scale := TGIS_PrintLayoutScale( TGIS_PrintLayoutScale.CreateDefault ) ;
      try
        FDefaultScale.Dividers      := scale.Dividers ;
        FDefaultScale.DividerColor1 := scale.DividerColor1 ;
        FDefaultScale.DividerColor2 := scale.DividerColor2 ;
        FDefaultScale.Font          := scale.Font ;
        FDefaultScale.FontSize      := scale.FontSize ;
        FDefaultScale.FontColor     := scale.FontColor ;
      finally
        FreeObject( scale ) ;
      end;
    end ;
    if assigned( FTemplate.GIS_NorthArrow[1] ) and
       ( FTemplate.GIS_NorthArrow[1] is TGIS_ControlNorthArrow ) then begin
      FDefaultNorthArrow.Style  := TGIS_ControlNorthArrow( FTemplate.GIS_NorthArrow[1] ).Style ;
      FDefaultNorthArrow.Color1 := GisColor(TGIS_ControlNorthArrow( FTemplate.GIS_NorthArrow[1] ).Color1) ;
      FDefaultNorthArrow.Color2 := GisColor(TGIS_ControlNorthArrow( FTemplate.GIS_NorthArrow[1] ).Color2) ;
      FDefaultNorthArrow.Path   := TGIS_ControlNorthArrow( FTemplate.GIS_NorthArrow[1] ).Path ;
    end else begin
      narrow := TGIS_PrintLayoutNorthArrow( TGIS_PrintLayoutNorthArrow.CreateDefault ) ;
      try
        FDefaultNorthArrow.Style  := narrow.Style ;
        FDefaultNorthArrow.Color1 := narrow.Color1 ;
        FDefaultNorthArrow.Color2 := narrow.Color2 ;
        FDefaultNorthArrow.Path   := narrow.Path ;
      finally
        FreeObject( narrow ) ;
      end;
    end;
  end ;


  procedure TGIS_ControlPrintTemplateDesignerForm.doShowForm(
    _sender : TObject
  ) ;
  var
    cr  : SmallInt ;
    i   : Integer ;
    elm : TGIS_PrintLayoutElement ;
  begin
    tmrShowForm.Enabled := False ;
    cr := Screen.Cursor ;
    try
      Settings.AppStarting := True ;
      Screen.Cursor := crHourglass ;
      prepare_template_bmp( -1, True ) ;
      for i := 0 to FBuilder.ElementsCount - 1  do begin
        elm := FBuilder.Elements[i];
        if elm.ElementType = TGIS_PrintLayoutElementType.Map then begin
          if chkLandscape.Checked then begin
            if assigned( FViewersEx ) and
               ( elm.Index > 0 ) and ( elm.Index < Length( FViewersEx ) ) and
               assigned( FViewersEx[elm.Index] ) then
              gisModel.Elements[i].Invalidate ;
          end else begin
            if assigned( FViewers ) and
               ( elm.Index > 0 ) and ( elm.Index < Length( FViewers ) ) and
               assigned( FViewers[elm.Index] ) then
              gisModel.Elements[i].Invalidate ;
          end ;
        end ;
      end;
    finally
      Settings.AppStarting := False ;
      Screen.Cursor := cr ;
    end;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.doLandscape(
    _sender : TObject
  ) ;
  var
    cr  : SmallInt ;
    i   : Integer ;
    elm : TGIS_PrintLayoutElement ;
  begin
    tmrLandscape.Enabled := False ;
    cr := Screen.Cursor ;
    try
      Settings.AppStarting := True ;
      Screen.Cursor := crHourglass ;
      prepare_template_bmp( -1, False ) ;
      for i := 0 to FBuilder.ElementsCount - 1  do begin
        elm := FBuilder.Elements[i];
        if elm.ElementType = TGIS_PrintLayoutElementType.Map then begin
          if chkLandscape.Checked then begin
            if assigned( FViewersEx ) and
               ( elm.Index > 0 ) and ( elm.Index < Length( FViewersEx ) ) and
               assigned( FViewersEx[elm.Index] ) then
              gisModel.Elements[i].Invalidate ;
          end else begin
            if assigned( FViewers ) and
               ( elm.Index > 0 ) and ( elm.Index < Length( FViewers ) ) and
               assigned( FViewers[elm.Index] ) then
              gisModel.Elements[i].Invalidate ;
          end ;
        end ;
      end;
    finally
      Settings.AppStarting := False ;
      Screen.Cursor := cr ;
    end;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.doPaint(
    _sender : TObject
  ) ;
  begin
    tmrPaint.Enabled := False ;
    Settings.DrawingPaused := False ;
    updateElementsEx ;
    gisModel.InvalidateElements;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.initElement(
    const _type : TGIS_PrintLayoutElementType
  ) ;
  begin
    FNewElement.Created := True ;
    FNewElement.EType   := _type ;
    gisModel.CreatingElement := True ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.updateElements ;
  begin
    if gisModel.Visible then
      gisModel.Resize ;
    updateElementsEx ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.updateElementsEx ;
  var
    idx : Integer ;
    elm : TGIS_PrintLayoutElement ;
  begin
    for idx := 0 to cmbObjectInspector.Items.Count-1 do begin
      elm := cmbObjectInspector.Items.Objects[idx] as TGIS_PrintLayoutElement ;
      elm.Location.UpdateRectangle( FPrinter.PrintArea,
                                    FPrinter.PageSize.X,
                                    FPrinter.PageSize.Y,
                                    FPrinter.PPI
                                  ) ;
      gisModel.ApplyBounds( gisModel.Elements[idx], elm.Location.Rectangle ) ;
    end ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.updateElementsWithDelay ;
  begin
    if pageControl.ActivePage = tabPreview then begin
      updateElements ;
      showPreview ;
    end else begin
      if Settings.WireframeMode then
        updateElements
      else begin
        Settings.DrawingPaused := True ;
        if gisModel.Visible then
          gisModel.Resize ;
        tmrPaint.Enabled := True ;
      end ;
    end ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.updateElementsLandscape ;
  begin
    if Settings.WireframeMode then
      updateElements
    else begin
      Settings.DrawingPaused := True ;
      if gisModel.Visible then
        gisModel.Resize ;
      doPaint( nil ) ;
      tmrLandscape.Enabled := True ;
    end ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.updateElementsWireFrame ;
  begin
    gisModel.InvalidateElements ;
    if not Settings.WireframeMode then
      tmrLandscape.Enabled := True ;
  end ;

  function TGIS_ControlPrintTemplateDesignerForm.createUniqueName(
    _name : String
  ) : String ;
  var
    i : Integer ;
    idx : Integer ;
    found : Boolean ;
  begin
    idx := 1 ;
    while (True) do begin
      found := False ;
      for i := 0 to cmbObjectInspector.Items.Count-1 do
        if cmbObjectInspector.Items[i] = _name + idx.ToString then begin
          found := True ;
          break ;
        end ;
      if found then
        inc(idx)
      else
        break ;
    end ;
    Result := _name + idx.ToString ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.doCreateShape(
    _sender : TObject
  ) ;
  var
    shp : T_ResizeElement ;
    elm : TGIS_PrintLayoutElement ;
    legend : TGIS_PrintLayoutLegend ;
    scale  : TGIS_PrintLayoutScale ;
    narrow : TGIS_PrintLayoutNorthArrow ;
  begin
    if FNewElement.Created then begin
      elm := FBuilder.NewElement( FNewElement.EType ) ;
      elm.Name := createUniqueName( elm.Name ) ;

      shp := T_ResizeElement( _sender ) ;
      shp.RealBoundsChangeEvent := doRealBoundsChange ;
      shp.RealBoundsChangeExEvent := doRealBoundsChangeEx ;
      shp.ClickEvent := doClickShape ;
      shp.DrawMapEvent := doDrawMap ;
      shp.DrawControlEvent := doDrawControl ;
      shp.DrawGraphicEvent := doDrawGraphic ;
      shp.UserData := elm ;
      shp.Template := FTemplate ;

      case shp.UserData.ElementType of
        TGIS_PrintLayoutElementType.Legend:
          begin
            legend := TGIS_PrintLayoutLegend( shp.UserData ) ;
            legend.CompactView   := FDefaultLegend.CompactView ;
            legend.DrawIconStyle := FDefaultLegend.DrawIconStyle ;
            legend.ReverseOrder  := FDefaultLegend.ReverseOrder ;
            legend.Font          := FDefaultLegend.Font ;
            legend.FontSize      := FDefaultLegend.FontSize ;
            legend.FontColor     := FDefaultLegend.FontColor ;
          end ;
        TGIS_PrintLayoutElementType.Scale:
          begin
            scale := TGIS_PrintLayoutScale( shp.UserData ) ;
            scale.Dividers      := FDefaultScale.Dividers ;
            scale.DividerColor1 := FDefaultScale.DividerColor1 ;
            scale.DividerColor2 := FDefaultScale.DividerColor2 ;
            scale.Font          := FDefaultScale.Font ;
            scale.FontSize      := FDefaultScale.FontSize ;
            scale.FontColor     := FDefaultScale.FontColor ;
          end ;
        TGIS_PrintLayoutElementType.NorthArrow:
          begin
            narrow := TGIS_PrintLayoutNorthArrow( shp.UserData ) ;
            narrow.Style  := FDefaultNorthArrow.Style ;
            narrow.Color1 := FDefaultNorthArrow.Color1 ;
            narrow.Color2 := FDefaultNorthArrow.Color2 ;
            narrow.Path   := FDefaultNorthArrow.Path ;
          end ;
      end ;
    end ;
  end ;

  function TGIS_ControlPrintTemplateDesignerForm.getElementRectangle(
    const _shp : T_ResizeElement
  ) : TRect ;
  begin
    Result := Rect( Trunc( _shp.BoundsRect.Left   * FPrinter.PPI / gisModel.SizeRatio / 96 ) - FPrinter.PrintArea.Left,
                    Trunc( _shp.BoundsRect.Top    * FPrinter.PPI / gisModel.SizeRatio / 96 ) - FPrinter.PrintArea.Top,
                    Trunc( _shp.BoundsRect.Right  * FPrinter.PPI / gisModel.SizeRatio / 96 ) - FPrinter.PrintArea.Left,
                    Trunc( _shp.BoundsRect.Bottom * FPrinter.PPI / gisModel.SizeRatio / 96 ) - FPrinter.PrintArea.Top
                  ) ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.doAddShape(
    _sender : TObject
  ) ;
  var
    shp : T_ResizeElement ;
    elm : TGIS_PrintLayoutElement ;
  begin
    if FNewElement.Created then begin
      FNewElement.Created := False ;
      shp := T_ResizeElement( _sender ) ;
      elm := shp.UserData ;

      elm.Location.UpdateRectangle( getElementRectangle( shp ),
                                    True, True, True, True,
                                    Settings.Snap,
                                    FPrinter.PrintArea,
                                    FPrinter.PageSize.X,
                                    FPrinter.PageSize.Y,
                                    FPrinter.PPI
                                  ) ;

      gisModel.CreatingElement := False ;

      addElementToObjectInspector( shp.UserData ) ;
      setElementInObjectInspector( gisModel.IndexOf( shp ) ) ;
      shp.Caption  := cmbObjectInspector.Items[cmbObjectInspector.ItemIndex] ;
      if Settings.Snap.Value <> 0 then
        // applies changes to shp.BoundsRect
        gisModel.ApplyBounds( shp, elm.Location.Rectangle ) ;
    end ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.doCancelShape(
    _sender : TObject
  ) ;
  begin
    FNewElement.Created := False ;
    gisModel.CreatingElement := False ;
    pageControl.SetFocus ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.doRealBoundsChange(
    _sender : TObject ;
    _left   : Boolean ;
    _top    : Boolean ;
    _right  : Boolean ;
    _bottom : Boolean
  ) ;
  var
    shp : T_ResizeElement ;
    elm : TGIS_PrintLayoutElement ;
  begin
    shp := T_ResizeElement( _sender ) ;
    elm := shp.UserData ;

    // new shp.BoundsRect changes to new elm.Location
    elm.Location.UpdateRectangle( getElementRectangle( shp ),
                                  _left, _top, _right, _bottom,
                                  Settings.Snap,
                                  FPrinter.PrintArea,
                                  FPrinter.PageSize.X,
                                  FPrinter.PageSize.Y,
                                  FPrinter.PPI
                                ) ;
    tplObjectInspector.ShowElement( elm ) ;
    if Settings.Snap.Value <> 0 then
      // applies changes to shp.BoundsRect
      gisModel.ApplyBounds( shp, elm.Location.Rectangle ) ;
  end;

  procedure TGIS_ControlPrintTemplateDesignerForm.doRealBoundsChangeEx(
    _sender : TObject ;
    _left   : Double ;
    _top    : Double ;
    _right  : Double ;
    _bottom : Double
  ) ;
  var
    shp : T_ResizeElement ;
    elm : TGIS_PrintLayoutElement ;
  begin
    shp := T_ResizeElement( _sender ) ;
    elm := shp.UserData ;

    // sets Location according to coordinates' changes and current snap
    elm.Location.UpdateRectangle( _left, _top, _right, _bottom,
                                  Settings.Snap,
                                  FPrinter.PrintArea,
                                  FPrinter.PageSize.X,
                                  FPrinter.PageSize.Y,
                                  FPrinter.PPI
                                ) ;
    tplObjectInspector.ShowElement( elm ) ;
    // applies changes to shp.BoundsRect
    gisModel.ApplyBounds( shp, elm.Location.Rectangle ) ;
  end;

  procedure TGIS_ControlPrintTemplateDesignerForm.doClickShape(
    _sender : TObject
  ) ;
  var
    shp : T_ResizeElement ;
  begin
    shp := T_ResizeElement( _sender ) ;
    setElementInObjectInspector( gisModel.IndexOf( shp ) ) ;
    pageControl.SetFocus ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.doDrawMap(
    _sender     : TObject ;
    _element    : TGIS_PrintLayoutElement ;
    _width      : Integer ;
    _height     : Integer ;
    var _bitmap : TBitmap
  ) ;
  var
    idx : Integer ;
  begin
    idx := _element.Index ;
    if not assigned( FTemplate.GIS_Viewer[idx] ) or
       not viewersCreated then begin
      _bitmap := nil ;
      exit ;
    end ;
    if chkLandscape.Checked then begin
       if not assigned( FViewersEx ) or
          not assigned( FViewersEx[idx] ) then begin
         _bitmap := nil ;
         exit ;
       end;
      if ( _width  <> FViewersEx[idx].Width  ) or
         ( _height <> FViewersEx[idx].Height ) then
        FViewersEx[idx].SetSize( _width, _height ) ;
      FViewersEx[idx].Lock ;
      FViewersEx[idx].CustomPPI := 0 ;
      FViewersEx[idx].VisibleExtent := FTemplate.GIS_ViewerExtent[idx] ;
      if FTemplate.GIS_ViewerScale[idx] <> 0 then
        FViewersEx[idx].Scale := FTemplate.GIS_ViewerScale[idx] ;
      FViewersEx[idx].CustomPPI := FPrinter.PPI ;
      FViewersEx[idx].RestrictedDrag := False ;
      FViewersEx[idx].Unlock ;
      _bitmap := TBitmap(FViewersEx[idx].Bitmap) ;
    end else begin
      if not assigned( FViewers ) or
         not assigned( FViewers[idx] ) then begin
         _bitmap := nil ;
         exit ;
       end ;
      if ( _width  <> FViewers[idx].Width  ) or
         ( _height <> FViewers[idx].Height ) then
        FViewers[idx].SetSize( _width, _height ) ;
      FViewers[idx].Lock ;
      FViewers[idx].CustomPPI := 0 ;
      FViewers[idx].VisibleExtent := FTemplate.GIS_ViewerExtent[idx] ;
      if FTemplate.GIS_ViewerScale[idx] <> 0 then
        FViewers[idx].Scale := FTemplate.GIS_ViewerScale[idx] ;
      FViewers[idx].CustomPPI := FPrinter.PPI ;
      FViewers[idx].RestrictedDrag := False ;
      FViewers[idx].Unlock ;
      _bitmap := TBitmap(FViewers[idx].GIS_Bitmap.NativeBitmap) ;
    end ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.doDrawControl(
    _sender  : TObject ;
    _element : TGIS_PrintLayoutElement ;
    _bitmap  : TGIS_Bitmap
  ) ;
  var
    idx : Integer ;
    legend : TGIS_ControlLegend ;
    scale  : TGIS_ControlScale ;
    narrow : TGIS_ControlNorthArrow ;
    path   : String ;
  begin
    idx := _element.Index ;
    case _element.ElementType of
      TGIS_PrintLayoutElementType.Legend:
        begin
          legend := TGIS_ControlLegend(FTemplate.GIS_Legend[idx]) ;
          if chkLandscape.Checked then begin
            if assigned( FViewersEx ) and assigned( FViewersEx[idx] ) then
              legend.GIS_Viewer.TemporaryScaleInternal :=
                FViewersEx[idx].Scale * FPrinter.PPI / CurrentPPI ;
          end else begin
            if assigned( FViewers ) and assigned( FViewers[idx] ) then
              legend.GIS_Viewer.TemporaryScaleInternal :=
                FViewers[idx].Scale * FPrinter.PPI / CurrentPPI ;
          end;
          try
            legend.CompactView   := TGIS_PrintLayoutLegend(_element).CompactView ;
            legend.DrawIconStyle := TGIS_PrintLayoutLegend(_element).DrawIconStyle ;
            legend.ReverseOrder  := TGIS_PrintLayoutLegend(_element).ReverseOrder ;
            legend.Font.Name     := TGIS_PrintLayoutLegend(_element).Font ;
            legend.Font.Size     := TGIS_PrintLayoutLegend(_element).FontSize ;
            legend.Font.Color    := VCLColor( TGIS_PrintLayoutLegend(_element).FontColor ) ;
            legend.PrintBmp( _bitmap ) ;
          finally
            legend.GIS_Viewer.TemporaryScaleInternal := 0 ;
          end ;
        end ;
      TGIS_PrintLayoutElementType.Scale:
        begin
          scale := TGIS_ControlScale(FTemplate.GIS_Scale[idx]) ;
          if chkLandscape.Checked then begin
            if assigned( FViewersEx ) and assigned( FViewersEx[idx] ) then
              scale.GIS_Viewer.TemporaryScaleInternal :=
                FViewersEx[idx].Scale * FPrinter.PPI / CurrentPPI ;
          end else begin
            if assigned( FViewers ) and assigned( FViewers[idx] ) then
              scale.GIS_Viewer.TemporaryScaleInternal :=
                FViewers[idx].Scale * FPrinter.PPI / CurrentPPI ;
          end ;
          try
            scale.Dividers := TGIS_PrintLayoutScale(_element).Dividers ;
            scale.DividerColor1 := VCLColor( TGIS_PrintLayoutScale(_element).DividerColor1 ) ;
            scale.DividerColor2 := VCLColor( TGIS_PrintLayoutScale(_element).DividerColor2 ) ;
            scale.Font.Name  := TGIS_PrintLayoutScale(_element).Font ;
            scale.Font.Size  := TGIS_PrintLayoutScale(_element).FontSize ;
            scale.Font.Color := VCLColor( TGIS_PrintLayoutScale(_element).FontColor ) ;
            scale.PrintBmp( _bitmap ) ;
          finally
            scale.GIS_Viewer.TemporaryScaleInternal := 0 ;
          end ;
        end ;
      TGIS_PrintLayoutElementType.NorthArrow:
        begin
          narrow := TGIS_ControlNorthArrow(FTemplate.GIS_NorthArrow[idx]) ;
          narrow.Style  := TGIS_PrintLayoutNorthArrow(_element).Style ;
          narrow.Color1 := VCLColor( TGIS_PrintLayoutNorthArrow(_element).Color1 ) ;
          narrow.Color2 := VCLColor( TGIS_PrintLayoutNorthArrow(_element).Color2 ) ;
          if IsStringEmpty(FTemplate.TemplatePath) then
            path := GetPathAbsolute( FWorkingFolder,
                                     TGIS_PrintLayoutNorthArrow(_element).Path )
          else
            path := GetPathAbsolute( GetFilePath(FTemplate.TemplatePath),
                                     TGIS_PrintLayoutNorthArrow(_element).Path ) ;
          if FileExists( path ) then
            narrow.Path := path
          else
            narrow.Path := '' ;
          narrow.PrintBmp( _bitmap ) ;
        end ;
    end;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.doDrawGraphic(
    _sender    : TObject ;
    _element   : TGIS_PrintLayoutElement ;
    _canvas    : TCanvas ;
    _rect      : TRect ;
    var _drawn : Boolean
  ) ;
  var
    graphic : TGIS_TemplateGraphic ;
    path    : String ;
    ext     : String ;
    sym     : TGIS_SymbolAbstract ;

    procedure draw_graphic_using_renderer ;
    var
      ctx    : TGIS_RendererContext ;
      viewer : TGIS_ViewerBmp ;
      rnd    : TGIS_RendererAbstract ;
      w, h   : Integer ;
      cache  : TGIS_Bitmap ;
      f      : Double ;
    begin
      ctx := TGIS_RendererContext.Create ;
      try
        viewer := TGIS_ViewerBmp.Create ;
        rnd := TGIS_RendererVclGdiPlus.Create ;

        w := _rect.Width ;
        h := _rect.Height ;
        cache := TGIS_Bitmap.Create( w, h ) ;
        ctx.AssignBaseMap( cache, True ) ;

        rnd.CreateContext( viewer, viewer, ctx, Point( 0, 0 ), w, h, FPrinter.PPI, 100 ) ;
        try
          rnd.PrepareDraw ;

          if assigned( graphic ) then begin
            _drawn := graphic.Draw( rnd, Rect( 0, 0, w, h ) ) ;
          end
          else if assigned( sym ) then begin
            if ( sym.Width = 0 ) or ( sym.Height = 0 ) then exit ;
            f := Min( w/sym.Width, h/sym.Height ) ;
            sym.Prepare( viewer,
                         -Max( RoundS( sym.Width * f ), RoundS( sym.Height * f ) ), // size in pixels
                         TGIS_Color.Black, // .Color,
                         TGIS_Color.Black, // .OutlineColor,
                         0,                // .SymbolRotate,
                         0,
                         TGIS_LabelPosition.MiddleCenter,
                         True,
                         rnd
                       ) ;
            sym.Draw( w div 2, h div 2 ) ;
            sym.Unprepare ;
            _drawn := True ;
          end ;

        finally
          rnd.AfterDraw ;
          rnd.ReleaseContext ;
          FreeObject( rnd ) ;
          FreeObject( viewer ) ;
        end ;
        _canvas.Draw( _rect.Left, _rect.Top,
                      VCL.Graphics.TBitmap(TGIS_Bitmap(cache).NativeBitmap), 255 ) ;
      finally
        FreeObject( ctx ) ;
      end ;
    end ;

    procedure draw_graphic ;
    var
      grph : TPicture ;
    begin
      grph := TPicture.Create ;
      try
        grph.LoadFromFile( path );
        _canvas.StretchDraw( _rect, grph.Graphic ) ;
        _drawn := True ;
      finally
        FreeObject( grph ) ;
      end ;
    end ;

  begin
    _drawn := False ;
    graphic := nil ;
    sym := nil ;
    if _element.Index = -1 then exit ;
    if _element.Index > 0 then begin
      graphic := FTemplate.Graphic[ _element.Index ] ;
      // draw bitmap from Template structure
      if assigned( graphic ) then begin
        try
          draw_graphic_using_renderer ;
        except
        end ;
      end
      else
        _element.Index := 0 ;
    end ;
    if _element.Index = 0 then begin
      if IsStringEmpty(FTemplate.TemplatePath) then
        path := GetPathAbsolute( FWorkingFolder,
                                 TGIS_PrintLayoutGraphic(_element).Path )
      else
        path := GetPathAbsolute( GetFilePath(FTemplate.TemplatePath),
                                 TGIS_PrintLayoutGraphic(_element).Path ) ;
      try
        ext := UpperCase( GetFileExt( path ) ) ;
        if ( ext = '.WMF' ) or ( ext = '.EMF' ) then
          // special case
          draw_graphic
        else begin
          sym := SymbolList.Prepare( path ) ;
          if assigned( sym ) then
            draw_graphic_using_renderer ;
        end ;
      except
      end ;
    end ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.addElementToObjectInspector(
    _element : TGIS_PrintLayoutElement
  );
  var
    node : TTreeNode ;

    procedure add_image(
      _node : TTreeNode ;
      _elm  : TGIS_PrintLayoutElement
    ) ;
    begin
      case _elm.ElementType of
        TGIS_PrintLayoutElementType.Map        : _node.ImageIndex := 5 ;
        TGIS_PrintLayoutElementType.Legend     : _node.ImageIndex := 6 ;
        TGIS_PrintLayoutElementType.Scale      : _node.ImageIndex := 7 ;
        TGIS_PrintLayoutElementType.NorthArrow : _node.ImageIndex := 8 ;
        TGIS_PrintLayoutElementType.Box        : _node.ImageIndex := 9 ;
        TGIS_PrintLayoutElementType.Text       : _node.ImageIndex := 10 ;
        TGIS_PrintLayoutElementType.Graphic    : _node.ImageIndex := 11 ;
      end;
      _node.SelectedIndex := _node.ImageIndex ;
    end ;

  begin
    if _element = nil then exit ;
    tvStructure.Items.BeginUpdate;
    node := tvStructure.Items.Add( nil, _element.Name ) ;
    add_image( node, _element ) ;
    tvStructure.Items.EndUpdate ;
    cmbObjectInspector.AddItem( _element.Name, _element ) ;
  end;

  procedure TGIS_ControlPrintTemplateDesignerForm.setElementInObjectInspector(
    _idx : Integer
  ) ;
  begin
    cmbObjectInspector.ItemIndex := _idx ;
    cmbObjectInspectorChange( nil ) ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.deleteElementFromObjectInspector(
    _idx : Integer
  ) ;
  var
    nd : TTreeNode ;
  begin
    if ( _idx < 0 ) or ( _idx >= cmbObjectInspector.Items.Count ) then exit ;
    if not assigned( tvStructure.Selected ) or
       ( tvStructure.Selected.Index <> _idx ) then exit ;

    tvStructure.OnChange := nil ;
    try
      tvStructure.Items.BeginUpdate;
        nd := tvStructure.Selected ;
        nd.Delete ;
      tvStructure.Items.EndUpdate ;
    finally
      tvStructure.OnChange := tvStructureChange ;
    end ;

    cmbObjectInspector.Items.Delete( _idx ) ;
    if cmbObjectInspector.Items.Count > _idx  then
      setElementInObjectInspector( _idx )
    else
      setElementInObjectInspector( _idx - 1 ) ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.updateGraphicsPaths(
    _path   : String ;
    _update : Boolean
  ) ;
  var
    i : Integer ;
    elm : TGIS_PrintLayoutElement ;
    apath : String ;
    rpath : String ;
  begin
    for i := 0 to cmbObjectInspector.Items.Count-1 do begin
      elm := cmbObjectInspector.Items.Objects[i] as TGIS_PrintLayoutElement ;
      if elm is TGIS_PrintLayoutGraphic then begin
        if IsStringEmpty( FTemplate.TemplatePath ) then
          apath := GetPathAbsolute( FWorkingFolder,
                                    TGIS_PrintLayoutGraphic(elm).Path )
        else
          apath := GetPathAbsolute( GetFilePath( FTemplate.TemplatePath ),
                                    TGIS_PrintLayoutGraphic(elm).Path ) ;
        if IsStringEmpty( _path ) then
          rpath := GetPathRelative( FWorkingFolder, apath )
        else
          rpath := GetPathRelative( _path, apath ) ;
        TGIS_PrintLayoutGraphic(elm).Path := rpath ;
        if _update and ( i = cmbObjectInspector.ItemIndex ) then
          tplObjectInspector.ShowElement( elm ) ;
      end
      else
      if elm is TGIS_PrintLayoutNorthArrow then begin
        if IsStringEmpty( FTemplate.TemplatePath ) then
          apath := GetPathAbsolute( FWorkingFolder,
                                    TGIS_PrintLayoutNorthArrow(elm).Path )
        else
          apath := GetPathAbsolute( GetFilePath( FTemplate.TemplatePath ),
                                    TGIS_PrintLayoutNorthArrow(elm).Path ) ;
        if IsStringEmpty( _path ) then
          rpath := GetPathRelative( FWorkingFolder, apath )
        else
          rpath := GetPathRelative( _path, apath ) ;
        TGIS_PrintLayoutNorthArrow(elm).Path := rpath ;
        if _update and ( i = cmbObjectInspector.ItemIndex ) then
          tplObjectInspector.ShowElement( elm ) ;
      end ;
    end ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.finishChangingExtent ;
  begin
    if Settings.ChangeExtent then
      btnStructureWndClick( nil ) ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.setWorkingFolder(
    _folder : String
  ) ;
  var
    path : String ;
  begin
    path := _folder ;
    if not IsStringEmpty( path ) then begin
      if path[Length(path)-1] <> '\' then
        path := path + '\' ;
      if ExtractFilePath(path) = path then
        FWorkingFolder := path
      else
        FWorkingFolder := GetCurrentDir + '\' ;
    end else
      FWorkingFolder := GetCurrentDir + '\';
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.setTemplateName(
    _name : String
  ) ;
  begin
    FTemplateName := _name ;
    Self.Caption := _rsrc( GIS_RS_TPL_DESIGNER ) + ' - ' + FTemplateName ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.recoverValues ;
  var
    idx : Integer ;
  begin
    for idx := 1 to FTemplate.GIS_LegendCount do begin
      if assigned( FTemplate.GIS_Legend[idx] ) then begin
        TGIS_ControlLegend(FTemplate.GIS_Legend[idx]).CompactView :=
          TGIS_ControlLegend(FTemplateOrig.GIS_Legend[idx]).CompactView ;
        TGIS_ControlLegend(FTemplate.GIS_Legend[idx]).DrawIconStyle :=
          TGIS_ControlLegend(FTemplateOrig.GIS_Legend[idx]).DrawIconStyle ;
        TGIS_ControlLegend(FTemplate.GIS_Legend[idx]).ReverseOrder :=
          TGIS_ControlLegend(FTemplateOrig.GIS_Legend[idx]).ReverseOrder ;
        TGIS_ControlLegend(FTemplate.GIS_Legend[idx]).Font.Name :=
          TGIS_ControlLegend(FTemplateOrig.GIS_Legend[idx]).Font.Name ;
        TGIS_ControlLegend(FTemplate.GIS_Legend[idx]).Font.Size :=
          TGIS_ControlLegend(FTemplateOrig.GIS_Legend[idx]).Font.Size ;
        TGIS_ControlLegend(FTemplate.GIS_Legend[idx]).Font.Color :=
          TGIS_ControlLegend(FTemplateOrig.GIS_Legend[idx]).Font.Color ;
      end;
    end ;
    for idx := 1 to FTemplate.GIS_ScaleCount do begin
      if assigned( FTemplate.GIS_Scale[idx] ) then begin
        TGIS_ControlScale(FTemplate.GIS_Scale[idx]).Dividers :=
          TGIS_ControlScale(FTemplateOrig.GIS_Scale[idx]).Dividers ;
        TGIS_ControlScale(FTemplate.GIS_Scale[idx]).DividerColor1 :=
          TGIS_ControlScale(FTemplateOrig.GIS_Scale[idx]).DividerColor1 ;
        TGIS_ControlScale(FTemplate.GIS_Scale[idx]).DividerColor2 :=
          TGIS_ControlScale(FTemplateOrig.GIS_Scale[idx]).DividerColor2 ;
        TGIS_ControlScale(FTemplate.GIS_Scale[idx]).Font.Name :=
          TGIS_ControlScale(FTemplateOrig.GIS_Scale[idx]).Font.Name ;
        TGIS_ControlScale(FTemplate.GIS_Scale[idx]).Font.Size :=
          TGIS_ControlScale(FTemplateOrig.GIS_Scale[idx]).Font.Size ;
        TGIS_ControlScale(FTemplate.GIS_Scale[idx]).Font.Color :=
          TGIS_ControlScale(FTemplateOrig.GIS_Scale[idx]).Font.Color ;
      end ;
    end ;
    for idx := 1 to FTemplate.GIS_NorthArrowCount do begin
      if assigned( FTemplate.GIS_NorthArrow[idx] ) then begin
        TGIS_ControlNorthArrow(FTemplate.GIS_NorthArrow[idx]).Style :=
          TGIS_ControlNorthArrow(FTemplateOrig.GIS_NorthArrow[idx]).Style ;
        TGIS_ControlNorthArrow(FTemplate.GIS_NorthArrow[idx]).Color1 :=
          TGIS_ControlNorthArrow(FTemplateOrig.GIS_NorthArrow[idx]).Color1 ;
        TGIS_ControlNorthArrow(FTemplate.GIS_NorthArrow[idx]).Color2 :=
          TGIS_ControlNorthArrow(FTemplateOrig.GIS_NorthArrow[idx]).Color2 ;
        TGIS_ControlNorthArrow(FTemplate.GIS_NorthArrow[idx]).Path :=
          TGIS_ControlNorthArrow(FTemplateOrig.GIS_NorthArrow[idx]).Path ;
      end ;
    end ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.showPreview ;
  var
    prn : TGIS_PrinterTemplate ;
    extents : array of TGIS_Extent ;

    procedure store_extents ;
    var
      i : Integer ;
    begin
      SetLength( extents, FTemplate.GIS_ViewerExtentCount + 1 ) ;
      for i := 1 to FTemplate.GIS_ViewerExtentCount do
        extents[i] := FTemplate.GIS_ViewerExtent[i] ;
    end ;

    procedure restore_extents ;
    var
      i : Integer ;
    begin
      for i := 1 to FTemplate.GIS_ViewerExtentCount do
        FTemplate.GIS_ViewerExtent[i] := extents[i] ;
      SetLength( extents, 0 ) ;
    end ;

  begin
    prn := TGIS_PrinterTemplate.Create( FPrinter.PageSize,
                                        FPrinter.PrintArea,
                                        FPrinter.PPI ) ;
    try
      try
        recoverValues ;
        store_extents ;
        gisPreview.Preview( 1, FPrintManager, prn ) ;
      except
        on e : Exception do
        begin
          ShowMessage( e.Message ) ;
        end ;
      end;
    finally
      restore_extents ;
      FreeObject( prn ) ;
    end;
  end ;

  function TGIS_ControlPrintTemplateDesignerForm.saveTemplate
    : Boolean ;
  var
    filename : String ;
  begin
    Result := False ;
    if IsStringEmpty( FTemplate.TemplatePath ) then begin
      btnSaveAsClick( nil ) ;
      Result := True ;
    end else begin
      filename := GetFilePath( FTemplate.TemplatePath ) +
                  GetFileNameNoExt( FTemplate.TemplatePath ) + GIS_TTKTEMPLATE_EXT ;
      updateGraphicsPaths( GetFilePath( filename ), True ) ;
      try
        FBuilder.SaveToFile( filename ) ;
        FTemplate.TemplatePath := filename ;
        formResult := mrOK ;
        Result := True ;
      except
        on e : Exception do
        begin
          ShowMessage( e.Message ) ;
        end ;
      end;
    end;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.btnOKClick(
    _sender : TObject
  ) ;
  begin
    finishChangingExtent ;
    if saveTemplate then begin
      OnCloseQuery := nil ;
      inherited ;
    end;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.btnCancelClick(
    _sender : TObject
  ) ;
  begin
    finishChangingExtent ;
    OnCloseQuery := nil ;
    inherited ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.btnNewClick(
    _sender : TObject
  ) ;
  begin
    finishChangingExtent ;
    FTemplate.TemplatePath := '' ;
    openTemplate ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.btnOpenClick(
    _sender : TObject
  ) ;
  var
    old_path : String ;
  begin
    finishChangingExtent ;
    dlgOpen.FileName := '';
    dlgOpen.InitialDir := FWorkingFolder ;
    if dlgOpen.Execute then begin
      old_path := FTemplate.TemplatePath ;
      try
        FTemplate.TemplatePath := dlgOpen.FileName ;
        openTemplate ;
      except
        on e : Exception do
        begin
          ShowMessage( e.Message ) ;
          FTemplate.TemplatePath := old_path ;
          openTemplate ;
        end ;
      end;
    end ;
  end;

  procedure TGIS_ControlPrintTemplateDesignerForm.btnSaveAsClick(
    _sender : TObject
  ) ;
  var
    filename : String ;
    olddir : String ;
  begin
    finishChangingExtent ;
    if not IsStringEmpty( FTemplate.TemplatePath ) then begin
      olddir := GetFilePath( FTemplate.TemplatePath ) ;
      filename := GetFilePath( FTemplate.TemplatePath ) +
                  GetFileNameNoExt( FTemplate.TemplatePath ) + GIS_TTKTEMPLATE_EXT ;
      dlgSave.FileName := GetFileName( filename ) ;
      dlgSave.InitialDir := GetFilePath( filename ) ;
    end else begin
      olddir := FWorkingFolder ;
      dlgSave.FileName := FTemplateName ;
      dlgSave.InitialDir := FWorkingFolder ;
    end ;
    if dlgSave.Execute then begin
      try
        updateGraphicsPaths( GetFilePath( dlgSave.FileName ), True ) ;
        FBuilder.SaveToFile( dlgSave.FileName ) ;
        FTemplate.TemplatePath := dlgSave.FileName ;
        setTemplateName( GetFilename(FTemplate.TemplatePath) ) ;
        formResult := mrOK ;
      except
        on e : Exception do
        begin
          ShowMessage( e.Message ) ;
          updateGraphicsPaths( olddir, True ) ;
        end ;
      end;
    end;
  end;

  procedure TGIS_ControlPrintTemplateDesignerForm.cmbPrintersInit(
    _selected : String
  ) ;
  var
    i : Integer ;
    idx : Integer ;
  begin
    cmbPrinters.Items.BeginUpdate ;
    cmbPrinters.Items.Add( _rsrc( GIS_RS_TPL_DESIGNER_PRINTERS ) ) ;
    for i := 0 to High( TGIS_PageSizeTable.Table ) do
      cmbPrinters.Items.Add( TGIS_PageSizeTable.Table[i].Name ) ;
    idx := cmbPrinters.Items.IndexOf( _selected ) ;
    if idx >= 0 then
      cmbPrinters.ItemIndex := idx
    else begin
      cmbPrinters.Items.Insert( 1, _selected ) ;
      cmbPrinters.ItemIndex := 1 ;
    end ;
    cmbPrinters.Items.EndUpdate ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.cmbPrintersUpdate(
    _printer : String
  ) ;
  var
    i : Integer ;
  begin
    i := cmbPrinters.Items.IndexOf( _printer ) ;
    if i >= 0 then
      cmbPrinters.ItemIndex := i
    else begin
      cmbPrinters.Items.Insert( 1, _printer ) ;
      cmbPrinters.ItemIndex := 1 ;
    end;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.cmbPrintersChange(
    _sender : TObject
  ) ;
  var
    dlg : TPrinterSetupDialog ;
    old_printer : String ;
    prn : String ;
  begin
    finishChangingExtent ;
    old_printer := FPrinter.Name ;
    if cmbPrinters.ItemIndex = 0 then begin
      dlg := TPrinterSetupDialog.Create( Self ) ;
      try
        if dlg.Execute then begin
          prn := Printer.Printers[Printer.PrinterIndex] ;
          cmbPrintersUpdate( prn ) ;
          setPrinter( prn, True ) ;
        end else
          cmbPrintersUpdate( FPrinter.Name ) ;
      finally
        FreeObject( dlg ) ;
      end;
      updateElementsWithDelay ;
    end else begin
      setPrinter( cmbPrinters.Items[cmbPrinters.ItemIndex], False )  ;
      if FPrinter.Name <> old_printer then
        updateElementsWithDelay ;
    end;
  end;

  procedure TGIS_ControlPrintTemplateDesignerForm.chkLandscapeClick(
    _sender : TObject
  ) ;
  begin
    finishChangingExtent ;
    if ( chkLandscape.Checked and
         ( FPrinter.PageSize.Y > FPrinter.PageSize.X ) ) or
       ( not chkLandscape.Checked and
         ( FPrinter.PageSize.X > FPrinter.PageSize.Y ) ) then begin
      FPrinter.PageSize := Point( FPrinter.PageSize.Y, FPrinter.PageSize.X ) ;
      FPrinter.PrintArea := Rect( FPrinter.PrintArea.Top, FPrinter.PrintArea.Left,
                                  FPrinter.PrintArea.Bottom, FPrinter.PrintArea.Right ) ;
      gisModel.SetPrinter( FPrinter.PageSize,
                           FPrinter.PrintArea,
                           FPrinter.PPI ) ;
      updateElementsLandscape ;
    end ;
  end;

  procedure TGIS_ControlPrintTemplateDesignerForm.chkWireframeClick(
    _sender : TObject
  ) ;
  begin
    finishChangingExtent ;
    Settings.WireframeMode := chkWireframe.Checked ;
    updateElementsWireFrame ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.btnPreviewMouseDown(
    _sender : TObject ;
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x, _y  : Integer
  ) ;
  begin
    finishChangingExtent ;
    pageControl.ActivePage := tabPreview ;
    pageControlChange( _sender ) ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.btnPreviewMouseUp(
    _sender : TObject ;
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x, _y  : Integer
  ) ;
  begin
    pageControl.ActivePage := tabModel ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.doBusy(_sender: TObject; _pos, _end: Integer; var _abort: Boolean);
  begin
    if _end <= 0 then
      lblProgress.Caption := ''
    else
      lblProgress.Caption := Format('%s %d%%', [_rsrc(GIS_RS_BUSY_PAINT), TruncS(_pos / _end * 100)]);

    Application.ProcessMessages;
  end;

  procedure TGIS_ControlPrintTemplateDesignerForm.prepare_template_bmp(
    _idx  : Integer ;
    _mode : Boolean
  ) ;
  var
    idx : Integer ;

    procedure do_pixelexport( _idx : Integer ; _first : Boolean ) ;
    var
      old_scale : Double ;
      old_cl    : TGIS_Color ;
      size  : TSize ;
      scale : Double ;
      lp    : TGIS_LayerBmp ;

      procedure export_bmp(
        _gis : IGIS_Viewer ;
        _gisExtent : TGIS_Extent ;
        _width : Integer ;
        _height : Integer ;
        _dpi : Integer
      ) ;
      var
        subformat : TGIS_LayerPixelSubFormat ;
        expExtent : TGIS_Extent ;
        wExtent  : Double ;
        hExtent  : Double ;
        wExtent2 : Double ;
        hExtent2 : Double ;
        w, h     : Integer ;
        exp      : TGIS_PixelExportManager ;
        tile     : TGIS_ViewerBmp ;
      begin
        lp := TGIS_LayerBmp.Create ;
        subformat := TGIS_LayerPixelSubFormat.Create(
                       TGIS_PixelFormat.RGB,
                       False,
                       TGIS_PixelSubFormat.None,
                       TGIS_CompressionType.None,
                       100
                     ) ;
        w := _width ;
        h := _height ;
        wExtent := _gisExtent.XMax - _gisExtent.XMin ;
        hExtent := _gisExtent.YMax - _gisExtent.YMin ;
        if hExtent <= wExtent * h / w then begin
          wExtent2 := wExtent ;
          hExtent2 := wExtent * h / w ;
          expExtent.XMin := _gisExtent.XMin ;
          expExtent.XMax := _gisExtent.XMax ;
          expExtent.YMin := _gisExtent.YMin - ( hExtent2 - hExtent ) / 2 ;
          expExtent.YMax := _gisExtent.YMax + ( hExtent2 - hExtent ) / 2 ;
          if expExtent.YMin < _gis.Extent.YMin then
            expExtent.YMin := _gis.Extent.YMin ;
          if expExtent.YMax > _gis.Extent.YMax then
            expExtent.YMax := _gis.Extent.YMax ;
        end else begin
          wExtent2 := hExtent * w / h ;
          hExtent2 := hExtent ;
          expExtent.XMin := _gisExtent.XMin - ( wExtent2 - wExtent ) / 2 ;
          expExtent.XMax := _gisExtent.XMax + ( wExtent2 - wExtent ) / 2 ;
          expExtent.YMin := _gisExtent.YMin ;
          expExtent.YMax := _gisExtent.YMax ;
          if expExtent.XMin < _gis.Extent.XMin then
            expExtent.XMin := _gis.Extent.XMin ;
          if expExtent.XMax > _gis.Extent.XMax then
            expExtent.XMax := _gis.Extent.XMax ;
        end;
        if w < h then
          h := 0
        else
          w := 0 ;
        lp.Build( lp.Path, False, _gis.CS, expExtent, w, h, subformat ) ;
        try
          exp := TGIS_PixelExportManager.Create( lp ) ;
          exp.BusyEvent := doBusy;
          tile := TGIS_ViewerBmp.Create;
          tile.Renderer := TGIS_ViewerWnd( _gis ).Renderer.CreateInstance ;
          tile.DrawBasemapOnly := True ;
          try
            exp.ExportFrom( _gis, tile, expExtent, _dpi ) ;
          except
            on ex : Exception do
              raise ex;
          end ;
          FreeObject( tile ) ;
        finally
          FreeObject( exp ) ;
        end;
      end ;

      function get_bitmap_size( _idx : Integer ; var _scale : Double ) : TSize ;
      var
        w, h : Integer ;
        i    : Integer ;
        elm  : TGIS_PrintLayoutElement ;
      begin
        _scale := 1 ;
        if FTemplate.GIS_Viewer[_idx] is TGIS_ViewerWnd then begin
          w := TGIS_ViewerWnd( FTemplate.GIS_Viewer[_idx] ).Width ;
          h := TGIS_ViewerWnd( FTemplate.GIS_Viewer[_idx] ).Height ;
        end else begin
          w := 0 ;
          h := 0 ;
        end;
        for i := 0 to FBuilder.ElementsCount - 1  do begin
          elm := FBuilder.Elements[i];
          if ( elm.ElementType = TGIS_PrintLayoutElementType.Map ) and
             ( elm.Index = _idx ) then begin
            w := elm.Location.Rectangle.Size.cx ;
            h := elm.Location.Rectangle.Size.cy ;
            break ;
          end ;
        end ;
        w := RoundS( w * 96 / FPrinter.PPI / gisModel.SizeRatio ) ;
        h := RoundS( h * 96 / FPrinter.PPI / gisModel.SizeRatio ) ;
        if w > h then begin
          if w > 2000 then begin
            _scale := 2000 / w ;
            w := 2000 ;
            h := RoundS( h * _scale ) ;
          end ;
        end else begin
          if h > 2000 then begin
            _scale := 2000 / h ;
            h := 2000 ;
            w := RoundS( w * _scale ) ;
          end ;
        end ;
        Result := TSize.Create( w, h ) ;
      end ;

    begin
      if assigned( FTemplate.GIS_Viewer[_idx] ) and
         ( not FTemplate.GIS_Viewer[_idx].IsEmpty ) then begin
        lp := nil ;
        size := get_bitmap_size( _idx, scale ) ;

        if not chkLandscape.Checked then begin

          // portrait
          if assigned( FViewers[_idx] ) then exit ;
          old_scale := FTemplate.GIS_Viewer[_idx].Scale ;
          old_cl := FTemplate.GIS_Viewer[_idx].Color ;
          try
            FTemplate.GIS_Viewer[_idx].Lock ;
            FTemplate.GIS_Viewer[_idx].Color := TGIS_Color.None;
            if FTemplate.GIS_ViewerScale[_idx] > 0 then
              FTemplate.GIS_Viewer[_idx].Scale := FTemplate.GIS_ViewerScale[_idx] ;
            try
              export_bmp( FTemplate.GIS_Viewer[_idx],
                          FTemplate.GIS_ViewerExtent[_idx],
                          size.cx, size.cy,
                          RoundS( scale * CurrentPPI * old_scale / FTemplate.GIS_Viewer[_idx].Scale )
                        ) ;
            except
            end ;
          finally
            FTemplate.GIS_Viewer[_idx].Color := old_cl ;
            if FTemplate.GIS_ViewerScale[_idx] > 0 then
              FTemplate.GIS_Viewer[_idx].Scale := old_scale ;
            FTemplate.GIS_Viewer[_idx].Unlock ;
          end ;
          FViewers[_idx] := TGIS_ViewerBmp.Create( 100, 100 ) ;
          FViewers[_idx].Lock ;
          if assigned( lp ) then
            FViewers[_idx].Add( lp ) ;
          FViewers[_idx].Renderer := TGIS_ViewerWnd(FTemplate.GIS_Viewer[_idx]).Renderer.CreateInstance ;
          if FTemplate.GIS_UseViewerColor[_idx] then
            FViewers[_idx].Color := old_cl
          else
            FViewers[_idx].Color := TGIS_Color.None ;
          FViewers[_idx].Unlock ;
          FViewers[_idx].Lock ;
          FViewers[_idx].CustomPPI := 0 ;
          if FTemplate.GIS_ViewerScale[_idx] <> 0 then
            FViewers[_idx].Scale := FTemplate.GIS_ViewerScale[_idx] ;
          FViewers[_idx].CustomPPI := FPrinter.PPI ;
          FViewers[_idx].RestrictedDrag := False ;
          FViewers[_idx].Unlock ;

        end else begin

          // landscape
          if assigned( FViewersEx[_idx] ) then exit ;
          old_scale := FTemplate.GIS_Viewer[_idx].Scale ;
          old_cl := FTemplate.GIS_Viewer[_idx].Color ;
          try
            FTemplate.GIS_Viewer[_idx].Lock ;
            FTemplate.GIS_Viewer[_idx].Color := TGIS_Color.None;
            if FTemplate.GIS_ViewerScale[_idx] > 0 then
              FTemplate.GIS_Viewer[_idx].Scale := FTemplate.GIS_ViewerScale[_idx] ;
            try
              export_bmp( FTemplate.GIS_Viewer[_idx],
                          FTemplate.GIS_ViewerExtent[_idx],
                          size.cx, size.cy,
                          RoundS( scale * CurrentPPI * old_scale / FTemplate.GIS_Viewer[_idx].Scale )
                        ) ;
            except
            end ;
          finally
            FTemplate.GIS_Viewer[_idx].Color := old_cl ;
            if FTemplate.GIS_ViewerScale[_idx] > 0 then
              FTemplate.GIS_Viewer[_idx].Scale := old_scale ;
            FTemplate.GIS_Viewer[_idx].Unlock ;
          end ;
          FViewersEx[_idx] := TGIS_ViewerBmp.Create( 100, 100 ) ;
          FViewersEx[_idx].Lock ;
          if assigned( lp ) then
            FViewersEx[_idx].Add( lp ) ;
          FViewersEx[_idx].Renderer := TGIS_ViewerWnd(FTemplate.GIS_Viewer[_idx]).Renderer.CreateInstance ;
          if FTemplate.GIS_UseViewerColor[_idx] then
            FViewersEx[_idx].Color := old_cl
          else
            FViewersEx[_idx].Color := TGIS_Color.None ;
          FViewersEx[_idx].Unlock ;
          FViewersEx[_idx].Lock ;
          FViewersEx[_idx].CustomPPI := 0 ;
          if FTemplate.GIS_ViewerScale[_idx] <> 0 then
            FViewersEx[_idx].Scale := FTemplate.GIS_ViewerScale[_idx] ;
          FViewersEx[_idx].CustomPPI := FPrinter.PPI ;
          FViewersEx[_idx].RestrictedDrag := False ;
          FViewersEx[_idx].Unlock ;

        end ;
      end ;
    end ;

  begin
    if _idx = -1 then begin

      for idx := 1 to FTemplate.GIS_ViewerCount do
        if assigned( FTemplate.GIS_Viewer[idx] ) and
           ( not FTemplate.GIS_Viewer[idx].IsEmpty ) then begin
          if _mode then begin
            FViewers[idx] := nil ;
            FViewersEx[idx] := nil ;
          end ;
          do_pixelexport( idx, True ) ;
        end ;
      viewersCreated := True ;

    end
    else begin

      if ( _idx < 1 ) or ( _idx > FTemplate.GIS_ViewerCount ) then exit ;
      if assigned( FTemplate.GIS_Viewer[_idx] ) and
         ( not FTemplate.GIS_Viewer[_idx].IsEmpty ) then begin
        if _mode then begin
          if assigned( FViewers[_idx] ) then
            FreeObject( FViewers[_idx] ) ;
          if assigned( FViewersEx[_idx] ) then
            FreeObject( FViewersEx[_idx] ) ;
        end ;
        do_pixelexport( _idx, False ) ;
      end ;

    end ;
  end ;

  function TGIS_ControlPrintTemplateDesignerForm.resolvePrinter(
    _custom : String
  ) : String ;
  var
    found : Boolean ;
    i : Integer ;
  begin
    Result := 'A4' ;
    try
      if IsStringEmpty( _custom ) then exit ;

      FCustomPrinterSettings := TGIS_PrintUtils.ResolveCustomPrinterSettings( _custom ) ;
      if not assigned( FCustomPrinterSettings ) then exit ;

      if not IsStringEmpty( FCustomPrinterSettings.Name ) then begin
        found := false ;
        for i := 0 to High( TGIS_PageSizeTable.Table ) do begin
          if TGIS_PageSizeTable.Table[i].Name = FCustomPrinterSettings.Name then begin
            Result := FCustomPrinterSettings.Name ;
            found := True ;
            break ;
          end ;
        end ;
        if not found then begin
          for i := 0 to Printer.Printers.Count - 1 do begin
            if Printer.Printers[i] = FCustomPrinterSettings.Name then begin
              Result := FCustomPrinterSettings.Name ;
              break ;
            end ;
          end ;
        end ;
      end ;
      if FCustomPrinterSettings.Landscape then
        chkLandscape.Checked := True ;
      if FCustomPrinterSettings.Portrait then
        chkLandscape.Checked := False ;
      if ( FCustomPrinterSettings.PageSize.X > 0 ) and
         ( FCustomPrinterSettings.PageSize.Y > 0 ) then begin
        if FCustomPrinterSettings.PageSize.X > FCustomPrinterSettings.PageSize.Y then
          chkLandscape.Checked := True
        else
          chkLandscape.Checked := False ;
        Result := 'Custom' ;
      end;
    finally
      cmbPrintersInit( Result ) ;
    end;
  end;

  procedure TGIS_ControlPrintTemplateDesignerForm.setPrinter(
    _printer      : String ;
    _setLandscape : Boolean
  ) ;
  var
    found : Boolean ;
    i     : Integer ;
    prn   : TGIS_Printer ;
  begin
    found := False ;
    if _printer = 'Custom' then begin
      FPrinter.Name      := _printer ;
      FPrinter.PageSize  := FCustomPrinterSettings.PageSize ;
      FPrinter.PrintArea := FCustomPrinterSettings.PrintArea ;
      FPrinter.PPI       := FCustomPrinterSettings.PPI ;
      found := True ;
    end ;
    if not found then begin
      i := TGIS_PageSizeTable.IndexOf( _printer ) ;
      if i >= 0 then begin
        TGIS_PageSizeTable.GetPage( i, chkLandscape.Checked,
                                    FPrinter.Name, FPrinter.PageSize,
                                    FPrinter.PrintArea, FPrinter.PPI ) ;
        found := True ;
      end;
    end ;
    if not found then begin
      if _printer <> Printer.Printers[Printer.PrinterIndex] then
        for i := 0 to Printer.Printers.Count - 1 do begin
          if Printer.Printers[i] = cmbPrinters.Items[cmbPrinters.ItemIndex] then begin
            Printer.PrinterIndex := i ;
            break ;
          end ;
        end ;
      prn := TGIS_Printer.Create( Printer ) ;
      try
        FPrinter.Name := Printer.Printers[Printer.PrinterIndex] ;
        if _setLandscape then begin
          FPrinter.PageSize  := prn.PageSize ;
          FPrinter.PrintArea := prn.PrintArea ;
          if prn.PageSize.X > prn.PageSize.Y then
            chkLandscape.Checked := True
          else
            chkLandscape.Checked := False ;
        end else begin
          if ( chkLandscape.Checked and
               ( prn.PageSize.X < prn.PageSize.Y ) ) or
             ( not chkLandscape.Checked and
               ( prn.PageSize.X > prn.PageSize.Y ) ) then begin
            FPrinter.PageSize  := Point( prn.PageSize.Y, prn.PageSize.X ) ;
            FPrinter.PrintArea := Rect( prn.PrintArea.Top, prn.PrintArea.Left,
                                        prn.PrintArea.Bottom, prn.PrintArea.Right ) ;
          end else begin
            FPrinter.PageSize  := prn.PageSize ;
            FPrinter.PrintArea := prn.PrintArea ;
          end ;
        end ;
        FPrinter.PPI := prn.PPI ;
      finally
        FreeObject( prn ) ;
      end;
      found := True ;
    end;
    assert( found ) ;
    gisModel.SetPrinter( FPrinter.PageSize,
                         FPrinter.PrintArea,
                         FPrinter.PPI ) ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.initDesigner(
    _template : TGIS_TemplatePrint ;
    _workingFolder : String
  ) ;
    procedure save_tiledpaint_flags ;
    var
      i, j, k : Integer ;
    begin
      SetLength(  FViewerTiledPaints, FTemplate.GIS_ViewerCount + 1 ) ;
      for i := 1 to FTemplate.GIS_ViewerCount do begin
        k := -1 ;
        for j := 1 to i - 1 do begin
          if FTemplate.GIS_Viewer[j] = FTemplate.GIS_Viewer[i] then begin
            FViewerTiledPaints[i] := FViewerTiledPaints[j] ;
            k := j ;
            break ;
          end ;
        end ;
        if k = -1 then begin
          FViewerTiledPaints[i] := FTemplate.GIS_Viewer[i].TiledPaint ;
          FTemplate.GIS_Viewer[i].TiledPaint := False ;
        end;
      end;
    end;

  begin
    Settings.Designer := Self ;
    FTemplateOrig := _template ;
    FTemplate := FTemplateOrig.MakeCopy ;
    save_tiledpaint_flags ;
    prepare_default_values ;
    FBuilder := TGIS_TemplatePrintBuilder.Create( FTemplate ) ;
    setWorkingFolder( _workingFolder ) ;

    FPrintManager := TGIS_PrintManager.Create ;
    FPrintManager.Template := FTemplate ;
    FNewElement.Created := False ;
    Settings.WireframeMode := False ;
    Settings.DrawingPaused := False ;
    openTemplate ;
    if FPrinter.PageSize.X > FPrinter.PageSize.Y then
      chkLandscape.Checked := True
    else
      chkLandscape.Checked := False ;
    tmrShowForm.Enabled := True ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.updateInputTemplate(
    _template : TGIS_TemplatePrint
  ) ;
  var
    i : Integer ;
  begin
    _template.TemplatePath := FTemplate.TemplatePath ;
    for i := 1 to _template.GIS_ViewerCount do
      _template.GIS_ViewerExtent[i] := FTemplate.GIS_ViewerExtent[i] ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.cleanupDesigner ;
  var
    i : Integer ;
    s : TGIS_ControlScale ;
    n : TGIS_ControlNorthArrow ;

    procedure restore_tiledpaint_flags ;
    var
      i : Integer ;
    begin
      for i := 1 to FTemplate.GIS_ViewerCount do
        FTemplate.GIS_Viewer[i].TiledPaint := FViewerTiledPaints[i] ;
    end ;

  begin
    for i := 0 to Length( FViewers ) - 1 do
      FreeObject( FViewers[i] ) ;
    for i := 0 to Length( FViewersEx ) - 1 do
      FreeObject( FViewersEx[i] ) ;
    restore_tiledpaint_flags ;
    FTemplateOrig.SaveCopy( FTemplate ) ;
    FreeObject( FPrintManager ) ;
    FreeObject( FBuilder ) ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.FormCloseQuery(
         _sender   : TObject ;
     var _canClose : Boolean
  ) ;
  var
    frm : T_CloseForm ;
    res : Integer ;
  begin
    finishChangingExtent ;
    frm := T_CloseForm.Create( Self ) ;
    try
      res := frm.ShowModal ;
      _canClose := False ;
      if res = mrOk then begin
        _canClose := True ;
        if frm.Save then
          if not saveTemplate then
            _canClose := False ;
      end ;
    finally
      FreeObject( frm ) ;
    end;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.FormDestroy(
    _sender : TObject
  ) ;
  begin
    FreeObject( imgList ) ;
    FreeObject( imgListScld ) ;
    FreeObject( FCustomPrinterSettings ) ;
  end;

  procedure TGIS_ControlPrintTemplateDesignerForm.splitterMoved(
    _sender : TObject
  ) ;
  var
    r_width : Integer ;
  begin
    r_width := pnlRightClient.Width ;
    PlaceControl( BiDiMode, nil, btnStructureUp,
                  -(r_width-btnStructureUp.Width),
                  btnStructureUp.Width ) ;
    PlaceControl( BiDiMode, nil, btnStructureDown,
                  -(r_width-2*btnStructureDown.Width),
                  btnStructureDown.Width ) ;
    PlaceControl( BiDiMode, nil, btnStructureFront,
                  -(r_width-3*btnStructureFront.Width),
                  btnStructureFront.Width ) ;
    PlaceControl( BiDiMode, nil, btnStructureBack,
                  -(r_width-4*btnStructureBack.Width),
                  btnStructureBack.Width ) ;
    PlaceControl( BiDiMode, nil, btnStructureDel,
                  -(r_width-5*btnStructureDel.Width),
                  btnStructureDel.Width ) ;
    PlaceControl( BiDiMode, nil, btnStructureWnd,
                  -(r_width-6*btnStructureWnd.Width),
                  btnStructureWnd.Width ) ;

    PlaceControl( BiDiMode, nil, lblStructure, 0, r_width ) ;
    PlaceControl( BiDiMode, nil, lblObjectInspector, 0, r_width ) ;
    PlaceControl( BiDiMode, nil, cmbObjectInspector, 0, r_width ) ;
    PlaceControl( BiDiMode, nil, lblToolPalette, 0, r_width ) ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.pageControlChange(
    _sender : TObject
  ) ;
  var
    path : String ;
    cdir : String ;
  begin
    if pageControl.ActivePage = tabPreview then begin
      path := FTemplate.TemplatePath ;
      try
        cdir := GetTempFolder ;
        updateGraphicsPaths( cdir, False ) ;
        FBuilder.SaveToFile(  cdir + '\temp' + GIS_TTKTEMPLATE_EXT ) ;
        FTemplate.TemplatePath := cdir + '\temp' + GIS_TTKTEMPLATE_EXT ;
        showPreview ;
      finally
        updateGraphicsPaths( GetFilePath( path ), False ) ;
        FTemplate.TemplatePath := path ;
      end;
    end ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.btnStructureDownClick(
    _sender : TObject
  ) ;
  var
    idx : Integer ;
    nd  : TTreeNode ;
  begin
    finishChangingExtent ;
    idx := cmbObjectInspector.ItemIndex ;
    if ( idx > -1 ) and ( idx < cmbObjectInspector.Items.Count - 1 ) then begin
      tvStructure.Items.BeginUpdate ;
        nd := tvStructure.Items.Item[idx+1] ;
        nd.MoveTo( nd.getPrevSibling, TNodeAttachMode.naInsert ) ;
      tvStructure.Items.EndUpdate ;
      cmbObjectInspector.Items.Move( idx, idx + 1 ) ;
      cmbObjectInspector.ItemIndex := idx + 1 ;
      FBuilder.MoveElement( idx, 1 ) ;
      gisModel.MoveElementDown( idx ) ;
    end ;
    ActiveControl := tvStructure ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.btnStructureUpClick(
    _sender : TObject
  ) ;
  var
    idx : Integer ;
    nd  : TTreeNode ;
  begin
    finishChangingExtent ;
    idx := cmbObjectInspector.ItemIndex ;
    if ( idx > 0 ) and ( idx < cmbObjectInspector.Items.Count ) then begin
      tvStructure.Items.BeginUpdate ;
        nd := tvStructure.Items.Item[idx] ;
        nd.MoveTo( nd.getPrevSibling, TNodeAttachMode.naInsert ) ;
      tvStructure.Items.EndUpdate ;
      cmbObjectInspector.Items.Move( idx, idx - 1 ) ;
      cmbObjectInspector.ItemIndex := idx - 1 ;
      FBuilder.MoveElement( idx, -1 ) ;
      gisModel.MoveElementUp( idx ) ;
    end ;
    ActiveControl := tvStructure ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.btnStructureBackClick(
    _sender : TObject
  ) ;
  var
    idx : Integer ;
    nd  : TTreeNode ;
  begin
    finishChangingExtent ;
    idx := cmbObjectInspector.ItemIndex ;
    if ( idx > 0 ) and ( idx < cmbObjectInspector.Items.Count ) then begin
      tvStructure.Items.BeginUpdate ;
        nd := tvStructure.Items.Item[idx] ;
        nd.MoveTo( nd.getPrevSibling, TNodeAttachMode.naAddFirst ) ;
      tvStructure.Items.EndUpdate ;
      cmbObjectInspector.Items.Move( idx, 0 ) ;
      cmbObjectInspector.ItemIndex := 0 ;
      FBuilder.MoveElement( idx, -idx ) ;
      gisModel.MoveElementBack( idx ) ;
    end ;
    ActiveControl := tvStructure ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.btnStructureFrontClick(
    _sender : TObject
  ) ;
  var
    idx : Integer ;
    new_idx : Integer ;
    nd  : TTreeNode ;
  begin
    finishChangingExtent ;
    idx := cmbObjectInspector.ItemIndex ;
    if ( idx > -1 ) and ( idx < tvStructure.Items.Count - 1 ) then begin
      new_idx := tvStructure.Items.Count - 1 ;
      tvStructure.Items.BeginUpdate ;
        nd := tvStructure.Items.Item[idx] ;
        nd.MoveTo( nd.getNextSibling, TNodeAttachMode.naAdd ) ;
      tvStructure.Items.EndUpdate ;
      cmbObjectInspector.Items.Move( idx, new_idx ) ;
      cmbObjectInspector.ItemIndex := new_idx ;
      FBuilder.MoveElement( idx, new_idx - idx ) ;
      gisModel.MoveElementFront( idx ) ;
    end ;
    ActiveControl := tvStructure ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.btnStructureDelClick(
    _sender : TObject
  ) ;
  var
    idx : Integer ;
  begin
    finishChangingExtent ;
    idx := cmbObjectInspector.ItemIndex ;
    try
      if ( idx < 0 ) or ( idx >= cmbObjectInspector.Items.Count ) then exit ;
      if not gisModel.IsFocused( idx ) then exit ;

      FBuilder.RemoveElement( idx ) ;
      gisModel.DeleteElement( idx ) ;

      deleteElementFromObjectInspector( idx ) ;
    finally
      ActiveControl := tvStructure ;
    end;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.btnStructureWndClick(
    _sender : TObject
  ) ;
  var
    idx  : Integer ;
    vwr  : IGIS_Viewer ;
    gis  : TGIS_ViewerWnd ;
    elm  : TGIS_PrintLayoutElement ;
    elmP : T_ResizeElement ;
    i    : Integer ;
    ext  : TGIS_Extent ;
  begin
    gis := nil ;

    if Settings.ChangeExtent then
      idx := Settings.MapElement
    else
      idx := cmbObjectInspector.ItemIndex ;

    if ( idx < 0 ) or ( idx >= cmbObjectInspector.Items.Count ) then exit ;
    if not gisModel.IsFocused( idx ) then exit ;

    elm := FBuilder.Elements[idx] ;
    if not ( elm.ElementType = TGIS_PrintLayoutElementType.Map ) then exit ;

    elmP := gisModel.Elements[ idx ] ;

    if Settings.ChangeExtent then begin

      try

        Settings.ChangeExtent := false ;

        if not assigned( elmP.ElementObject ) then exit ;
        if not ( elmP.ElementObject is TGIS_ViewerWnd ) then exit ;

        vwr := FTemplate.GIS_Viewer[elm.Index];
        gis := TGIS_ViewerWnd( elmP.ElementObject ) ;
        if extentChanged then begin
          ext := GisCommonExtent(
                   gis.VisibleExtent,
                   FTemplate.GIS_Viewer[elm.Index].Extent
                 ) ;
          if not GisIsEmptyExtent( ext ) then
            FTemplate.GIS_ViewerExtent[elm.Index] := gis.VisibleExtent
          else
            FTemplate.GIS_ViewerExtent[elm.Index] := oldViewerExtent ;
        end ;
        vwr.Lock ;
        for i := 0 to gis.Items.Count -1 do
          vwr.AttachLayer( TGIS_Layer( gis.Items[i] ) ) ;
        vwr.Unlock ;

        prepare_template_bmp( elm.Index, True ) ;
        elmP.ElementObject := nil ;
        elmP.InvalidateCache ;
        FreeObject( gis ) ;

      except
        elmP.ElementObject := nil ;
        FreeObject( gis ) ;
      end;

    end
    else begin

      try

        Settings.ChangeExtent := True ;
        Settings.MapElement := idx ;

        vwr := FTemplate.GIS_Viewer[elm.Index];
        gis := TGIS_ViewerWnd.Create( elmP.Parent ) ;
        gis.Parent := elmP.Parent ;
        gis.BoundsRect := elmP.BoundsRect ;
        gis.Items.OwnsObjects := False ;
        gis.TiledPaint := vwr.TiledPaint ;
        gis.Renderer := TGIS_RendererVclGdiPlus.Create ;
        gis.Lock ;

          for i := 0 to vwr.Items.Count -1 do begin
            gis.Add( TGIS_Layer( vwr.Items[i] ) );
            gis.AttachLayer( TGIS_Layer( vwr.Items[i] ) ) ;
          end ;
          gis.FullExtent ;
          gis.Mode := TGIS_ViewerMode.Drag;
          gis.CustomPPI := 0 ;
          gis.RestrictedDrag := False ;
          gis.VisibleExtent := FTemplate.GIS_ViewerExtent[elm.Index] ;
          if FTemplate.GIS_ViewerScale[elm.Index] <> 0 then
            gis.Scale := FTemplate.GIS_ViewerScale[elm.Index] * gisModel.SizeRatio ;
          gis.CustomPPI := RoundS( 96 * gisModel.SizeRatio ) ;
          gis.VisibleExtentChangeEvent := gisExtentChanged ;
          {$IFDEF LEVEL_RX101_VCL}
            gis.OnMouseWheelUp := gisMouseWheelUp ;
            gis.OnMouseWheelDown := gisMouseWheelDown ;
          {$ELSE}
            OnMouseWheelUp := gisMouseWheelUp ;
            OnMouseWheelDown := gisMouseWheelDown ;
          {$ENDIF}
          extentChanged := False ;
          oldViewerExtent := FTemplate.GIS_ViewerExtent[elm.Index] ;

        gis.Unlock ;
        elmP.ElementObject := gis ;

      except
        FreeObject( gis ) ;
        elmP.ElementObject := nil ;
      end ;

    end ;
    if assigned( _sender ) then
      tplObjectInspector.ShowElement( elm ) ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.gisExtentChanged(
    _sender : TObject
  ) ;
  var
    idx : Integer ;
    elm : TGIS_PrintLayoutElement ;
    gis : TGIS_ViewerWnd ;
    elmP : T_ResizeElement ;
  begin
    idx := cmbObjectInspector.ItemIndex ;
    if ( idx < 0 ) or ( idx >= cmbObjectInspector.Items.Count ) then exit ;

    elm := FBuilder.Elements[idx] ;
    if not ( elm.ElementType = TGIS_PrintLayoutElementType.Map ) then exit ;

    elmP := gisModel.Elements[ idx ] ;

    if not Settings.ChangeExtent then exit ;

    gis := TGIS_ViewerWnd( elmP.ElementObject ) ;
    FTemplate.GIS_ViewerExtent[elm.Index] := GisCommonExtent(
                                               gis.VisibleExtent,
                                               FTemplate.GIS_Viewer[elm.Index].Extent
                                             ) ;
    extentChanged := True ;

    tplObjectInspector.ShowElement( elm ) ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.gisMouseWheelUp(
        _sender   : TObject ;
        _shift    : TShiftState ;
        _mousePos : TPoint ;
    var _handled  : Boolean
  ) ;
  var
    idx : Integer ;
    elm : TGIS_PrintLayoutElement ;
    gis : TGIS_ViewerWnd ;
    elmP : T_ResizeElement ;
    pt  : TPoint ;
  begin
    idx := cmbObjectInspector.ItemIndex ;
    if ( idx < 0 ) or ( idx >= cmbObjectInspector.Items.Count ) then exit ;

    elm := FBuilder.Elements[idx] ;
    if not ( elm.ElementType = TGIS_PrintLayoutElementType.Map ) then exit ;

    elmP := gisModel.Elements[ idx ] ;

    if not Settings.ChangeExtent then exit ;

    gis := TGIS_ViewerWnd( elmP.ElementObject ) ;
    pt := gis.ScreenToClient( _mousePos ) ;

    gis.ZoomBy( 4/5, pt.X, pt.Y ) ;
    _handled := True ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.gisMouseWheelDown(
        _sender   : TObject ;
        _shift    : TShiftState ;
        _mousePos : TPoint ;
    var _handled  : Boolean
  ) ;
  var
    idx : Integer ;
    elm : TGIS_PrintLayoutElement ;
    gis : TGIS_ViewerWnd ;
    elmP : T_ResizeElement ;
    pt  : TPoint ;
  begin
    idx := cmbObjectInspector.ItemIndex ;
    if ( idx < 0 ) or ( idx >= cmbObjectInspector.Items.Count ) then exit ;

    elm := FBuilder.Elements[idx] ;
    if not ( elm.ElementType = TGIS_PrintLayoutElementType.Map ) then exit ;

    elmP := gisModel.Elements[ idx ] ;

    if not Settings.ChangeExtent then exit ;

    gis := TGIS_ViewerWnd( elmP.ElementObject ) ;
    pt := gis.ScreenToClient( _mousePos ) ;

    gis.ZoomBy( 5/4, pt.X, pt.Y ) ;
    _handled := True ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.tvStructureChange(
    _sender : TObject ;
    _node   : TTreeNode
  ) ;
  begin
    if tvStructure.Items.Count = 0 then exit ;
    if cmbObjectInspector.ItemIndex <> _node.Index then
      setElementInObjectInspector( _node.Index ) ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.cmbObjectInspectorSnapChange(
    _sender : TObject
  ) ;
  var
    typ   : String ;
    field : String ;
    value : String ;
    units : String ;
  begin
    SplitNumberAsText( TGIS_SizeComboBox(_sender).Value,
                       typ, field, value, units ) ;
    Settings.Snap.SetValues( value + units ) ;
  end ;

  function TGIS_ControlPrintTemplateDesignerForm.cmbObjectInspectorSnapCustom(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    dlg_size : TGIS_ControlSizeForm ;
    on_help  : TGIS_HelpEvent ;
    proc  : TGIS_Proc ;
    val : String ;
  begin
    dlg_size := TGIS_ControlSizeForm.Create( Self ) ;
    on_help := nil ;
    dlg_size.FillSnapUnits ;
    proc := procedure(_modal_result: TGIS_PvlModalResult)
      begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit;
        if dlg_size.isRotation then
          val := GIS_PARAMTXT_TYPE_ANGLE + ':' + dlg_size.spnFactor.Text + ' ' +
            dlg_size.cmbUnits.Text
        else
          val := GIS_PARAMTXT_TYPE_SIZE + ':' + dlg_size.spnFactor.Text + ' ' +
            dlg_size.cmbUnits.Text
      end;

      dlg_size.Execute( on_help, proc ) ;

      Result := val ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.cmbObjectInspectorChange(
    _sender : TObject
  ) ;
  var
    idx : Integer ;
    elm : TGIS_PrintLayoutElement ;
  begin
    finishChangingExtent ;
    idx := cmbObjectInspector.ItemIndex ;
    if idx = -1 then begin
      tvStructure.Selected := nil ;
      tplObjectInspector.ShowElement( nil ) ;
    end
    else
    begin
      if ( tvStructure.Selected = nil ) or
         ( tvStructure.Selected.Index <> idx ) then
        tvStructure.Selected := tvStructure.Items.Item[idx];

      elm := cmbObjectInspector.Items.Objects[idx] as TGIS_PrintLayoutElement ;
      if elm.ElementType = TGIS_PrintLayoutElementType.Map then
        btnStructureWnd.Enabled := True
      else
        btnStructureWnd.Enabled := False ;
      if assigned( elm ) then
        tplObjectInspector.ShowElement( elm ) ;
    end;
    gisModel.SetFocused( idx ) ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.tplObjectInspectorInit(
    _sender  : TObject ;
    _element : TGIS_PrintLayoutElement
  ) ;
  var
    idx  : Integer ;
    name : String ;
    cmb  : TComboBox ;
    edt  : TEdit ;
    i    : Integer ;
  begin
    idx := cmbObjectInspector.ItemIndex ;
    if idx = -1 then exit ;

    name := TControl(_sender).Name ;
    if name = GIS_TPL_DESIGNER_FLD_INDEX_MAP then begin
      cmb := TComboBox( _sender ) ;
      cmb.Items.BeginUpdate ;
      cmb.Items.Add( _rsrc( GIS_RS_TPL_DESIGNER_FIELD_NONE ) ) ;
      for i := 1 to FTemplate.GIS_ViewerCount do
        if assigned( FTemplate.GIS_Viewer[i] ) and
           ( FTemplate.GIS_Viewer[i] is TControl ) then begin
          if not IsStringEmpty( FTemplate.GIS_ViewerName[i] ) then
            cmb.Items.Add( FTemplate.GIS_ViewerName[i] )
          else
            cmb.Items.Add( TControl( FTemplate.GIS_Viewer[i] ).Name )
        end else
          cmb.Items.Add( '' ) ;
      cmb.Items.EndUpdate ;
      if ( _element.Index >= 0 ) and ( _element.Index <= FTemplate.GIS_ViewerCount ) then
        cmb.ItemIndex := _element.Index
      else
        cmb.ItemIndex := 0 ;
    end;
    if name = GIS_TPL_DESIGNER_FLD_XMIN then begin
      edt := TEdit( _sender ) ;
      if _element.Index = 0 then begin
        tplObjectInspector.VisibleAttribute( GIS_TPL_DESIGNER_FLD_XMIN, False ) ;
      end
      else begin
        tplObjectInspector.VisibleAttribute( GIS_TPL_DESIGNER_FLD_XMIN, True ) ;
        if Settings.ChangeExtent then
          tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_XMIN, True )
        else
          tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_XMIN, False ) ;
        edt.Text := Format( '%.4f', [FTemplate.GIS_ViewerExtent[_element.Index].XMin] ) ;
      end ;
    end;
    if name = GIS_TPL_DESIGNER_FLD_XMAX then begin
      edt := TEdit( _sender ) ;
      if _element.Index = 0 then begin
        tplObjectInspector.VisibleAttribute( GIS_TPL_DESIGNER_FLD_XMAX, False ) ;
      end
      else begin
        tplObjectInspector.VisibleAttribute( GIS_TPL_DESIGNER_FLD_XMAX, True ) ;
        if Settings.ChangeExtent then
          tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_XMAX, True )
        else
          tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_XMAX, False ) ;
        edt.Text := Format( '%.4f', [FTemplate.GIS_ViewerExtent[_element.Index].XMax] ) ;
      end ;
    end;
    if name = GIS_TPL_DESIGNER_FLD_YMIN then begin
      edt := TEdit( _sender ) ;
      if _element.Index = 0 then begin
        tplObjectInspector.VisibleAttribute( GIS_TPL_DESIGNER_FLD_YMIN, False ) ;
      end
      else begin
        tplObjectInspector.VisibleAttribute( GIS_TPL_DESIGNER_FLD_YMIN, True ) ;
        if Settings.ChangeExtent then
          tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_YMIN, True )
        else
          tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_YMIN, False ) ;
        edt.Text := Format( '%.4f', [FTemplate.GIS_ViewerExtent[_element.Index].YMin] ) ;
      end;
    end;
    if name = GIS_TPL_DESIGNER_FLD_YMAX then begin
      edt := TEdit( _sender ) ;
      if _element.Index = 0 then begin
        tplObjectInspector.VisibleAttribute( GIS_TPL_DESIGNER_FLD_YMAX, False ) ;
      end
      else begin
        tplObjectInspector.VisibleAttribute( GIS_TPL_DESIGNER_FLD_YMAX, True ) ;
        if Settings.ChangeExtent then
          tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_YMAX, True )
        else
          tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_YMAX, False ) ;
        edt.Text := Format( '%.4f', [FTemplate.GIS_ViewerExtent[_element.Index].YMax] ) ;
      end;
    end;
    if name = GIS_TPL_DESIGNER_FLD_INDEX_LEGEND then begin
      cmb := TComboBox( _sender ) ;
      cmb.Items.BeginUpdate ;
      cmb.Items.Add( _rsrc( GIS_RS_TPL_DESIGNER_FIELD_NONE ) ) ;
      for i := 1 to FTemplate.GIS_LegendCount do
        if assigned( FTemplate.GIS_Legend[i] ) and
           ( FTemplate.GIS_Legend[i] is TControl ) then
          cmb.Items.Add( IGIS_PrintableControl( FTemplate.GIS_Legend[i] ).InternalName )
        else
          cmb.Items.Add( '' ) ;
      cmb.Items.EndUpdate ;
      if ( _element.Index >= 0 ) and ( _element.Index <= FTemplate.GIS_LegendCount ) then
        cmb.ItemIndex := _element.Index
      else
        cmb.ItemIndex := 0 ;
    end;
    if name = GIS_TPL_DESIGNER_FLD_INDEX_SCALE then begin
      cmb := TComboBox( _sender ) ;
      cmb.Items.BeginUpdate ;
      cmb.Items.Add( _rsrc( GIS_RS_TPL_DESIGNER_FIELD_NONE ) ) ;
      for i := 1 to FTemplate.GIS_ScaleCount do
        if assigned( FTemplate.GIS_Scale[i] ) and
           ( FTemplate.GIS_Scale[i] is TControl ) then
          cmb.Items.Add( IGIS_PrintableControl( FTemplate.GIS_Scale[i] ).InternalName )
        else
          cmb.Items.Add( '' ) ;
      cmb.Items.EndUpdate ;
      if ( _element.Index >= 0 ) and ( _element.Index <= FTemplate.GIS_ScaleCount ) then
        cmb.ItemIndex := _element.Index
      else
        cmb.ItemIndex := 0 ;
    end;
    if name = GIS_TPL_DESIGNER_FLD_INDEX_NORTHARROW then begin
      cmb := TComboBox( _sender ) ;
      cmb.Items.BeginUpdate ;
      cmb.Items.Add( _rsrc( GIS_RS_TPL_DESIGNER_FIELD_NONE ) ) ;
      for i := 1 to FTemplate.GIS_NorthArrowCount do
        if assigned( FTemplate.GIS_NorthArrow[i] ) and
           ( FTemplate.GIS_NorthArrow[i] is TControl ) then
          cmb.Items.Add( IGIS_PrintableControl( FTemplate.GIS_NorthArrow[i] ).InternalName )
        else
          cmb.Items.Add( '' ) ;
      cmb.Items.EndUpdate ;
      if ( _element.Index >= 0 ) and ( _element.Index <= FTemplate.GIS_NorthArrowCount ) then
        cmb.ItemIndex := _element.Index
      else
        cmb.ItemIndex := 0 ;
    end;
    if name = GIS_TPL_DESIGNER_FLD_INDEX_TEXT then begin
      cmb := TComboBox( _sender ) ;
      cmb.Items.BeginUpdate ;
      cmb.Items.Add( _rsrc( GIS_RS_TPL_DESIGNER_FIELD_NONE ) ) ;
      cmb.Items.Add( _rsrc( GIS_RS_TPL_DESIGNER_FIELD_USER ) ) ;
      for i := 1 to FTemplate.TextCount do begin
        if not IsStringEmpty( FTemplate.Text[i] ) then
          cmb.Items.Add( FTemplate.Text[i] )
        else
          cmb.Items.Add( '[empty]' ) ;
      end ;
      cmb.Items.EndUpdate ;
      if _element.Index = -1 then
        // no data
        cmb.ItemIndex := 0
      else if _element.Index = 0 then
        // data from file
        cmb.ItemIndex := 1
      else if ( _element.Index > 0 ) and
              ( _element.Index <= FTemplate.TextCount ) and
              not IsStringEmpty( FTemplate.Text[ _element.Index ] ) then
        // data from Template structure
        cmb.ItemIndex := _element.Index + 1
      else begin
        // user defined text
        _element.Index := 0 ;
        cmb.ItemIndex := 1 ;
      end ;
    end ;
    if name = GIS_TPL_DESIGNER_FLD_INDEX_GRAPHICS then begin
      cmb := TComboBox( _sender ) ;
      cmb.Items.BeginUpdate ;
      cmb.Items.Add( _rsrc( GIS_RS_TPL_DESIGNER_FIELD_NONE ) ) ;
      cmb.Items.Add( _rsrc( GIS_RS_TPL_DESIGNER_FIELD_USER ) ) ;
      for i := 1 to FTemplate.GraphicCount do begin
        if assigned( FTemplate.Graphic[i] ) then
          cmb.Items.Add( 'Graphic[ ' + i.ToString + ' ]' )
        else
          cmb.Items.Add( '[not defined]' ) ;
      end ;
      cmb.Items.EndUpdate ;
      if _element.Index = -1 then
        // no data
        cmb.ItemIndex := 0
      else if _element.Index = 0 then
        // data from file
        cmb.ItemIndex := 1
      else if ( _element.Index > 0 ) and
              ( _element.Index <= FTemplate.GraphicCount ) and
              assigned( FTemplate.Graphic[ _element.Index ] ) then
        // data from Template structure
        cmb.ItemIndex := _element.Index + 1
      else begin
        // user defined data
        _element.Index := 0 ;
        cmb.ItemIndex := 1 ;
      end ;
    end ;
    if name = GIS_TPL_DESIGNER_FLD_TEXT then begin
      if _element.Index <> 0 then
        tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_TEXT, False ) ;
    end ;
    if name = GIS_TPL_DESIGNER_FLD_GRAPHICSPATH then begin
      if _element.Index <> 0 then
        tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_GRAPHICSPATH, False ) ;
    end ;
    if name = GIS_TPL_DESIGNER_FLD_GRAPHICSPATH_BTN then begin
      if _element.Index <> 0 then
        tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_GRAPHICSPATH_BTN, False ) ;
    end ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.tplObjectInspectorChange(
    _sender : TObject
  ) ;
  var
    idx    : Integer ;
    idx1   : Integer ;
    elm    : TGIS_PrintLayoutElement ;
    name   : String ;
    typ    : String ;
    field  : String ;
    value  : String ;
    units  : String ;
  begin
    idx := cmbObjectInspector.ItemIndex ;
    if idx = -1 then exit ;

    elm := cmbObjectInspector.Items.Objects[idx] as TGIS_PrintLayoutElement ;
    name := TControl(_sender).Name ;

    if name = GIS_TPL_DESIGNER_FLD_INDEX_MAP then begin
      if elm.Index <> TComboBox(_sender).ItemIndex then begin
        elm.Index := TComboBox(_sender).ItemIndex ;
        tplObjectInspectorInit(
          tplObjectInspector.FindAttribute( GIS_TPL_DESIGNER_FLD_XMIN ),
          elm ) ;
        tplObjectInspectorInit(
          tplObjectInspector.FindAttribute( GIS_TPL_DESIGNER_FLD_XMAX ),
          elm ) ;
        tplObjectInspectorInit(
          tplObjectInspector.FindAttribute( GIS_TPL_DESIGNER_FLD_YMIN ),
          elm ) ;
        tplObjectInspectorInit(
          tplObjectInspector.FindAttribute( GIS_TPL_DESIGNER_FLD_YMAX ),
          elm ) ;
        gisModel.Elements[idx].Invalidate ;
      end ;
    end
    else
    if ( name = GIS_TPL_DESIGNER_FLD_INDEX_LEGEND ) or
       ( name = GIS_TPL_DESIGNER_FLD_INDEX_SCALE ) or
       ( name = GIS_TPL_DESIGNER_FLD_INDEX_NORTHARROW ) then begin
      elm.Index := TComboBox(_sender).ItemIndex ;
      gisModel.Elements[idx].InvalidateCache ;
    end
    else
    if ( name = GIS_TPL_DESIGNER_FLD_INDEX_TEXT ) then begin
      idx1 := TComboBox(_sender).ItemIndex ;
      if idx1 = 0 then begin
        elm.Index := -1 ;
        tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_TEXT, False ) ;
      end else if idx1 = 1 then begin
        elm.Index := 0 ;
        tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_TEXT, True ) ;
      end else begin
        if not IsStringEmpty( FTemplate.Text[idx1-1] ) then begin
          elm.Index := idx1 - 1 ;
          tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_TEXT, False ) ;
        end
        else begin
          TComboBox(_sender).ItemIndex := 1 ;
          elm.Index := 0 ;
          tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_TEXT, True ) ;
        end ;
      end ;
      gisModel.Elements[idx].Caption := elm.Name ;
      gisModel.Elements[idx].Invalidate ;
    end
    else
    if ( name = GIS_TPL_DESIGNER_FLD_INDEX_GRAPHICS ) then begin
      idx1 := TComboBox(_sender).ItemIndex ;
      if idx1 = 0 then begin
        elm.Index := -1 ;
        tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_GRAPHICSPATH, False ) ;
        tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_GRAPHICSPATH_BTN, False ) ;
      end else if idx1 = 1 then begin
        elm.Index := 0 ;
        tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_GRAPHICSPATH, True ) ;
        tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_GRAPHICSPATH_BTN, True ) ;
      end else begin
        if assigned( FTemplate.Graphic[idx1-1] ) then begin
          elm.Index := TComboBox(_sender).ItemIndex - 1 ;
          tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_GRAPHICSPATH, False ) ;
          tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_GRAPHICSPATH_BTN, False ) ;
        end
        else begin
          TComboBox(_sender).ItemIndex := 1 ;
          elm.Index := 0 ;
          tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_GRAPHICSPATH, True ) ;
          tplObjectInspector.EnableAttribute( GIS_TPL_DESIGNER_FLD_GRAPHICSPATH_BTN, True ) ;
        end ;
      end ;
      gisModel.Elements[idx].Caption := elm.Name ;
      gisModel.Elements[idx].Invalidate ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_BOXCOLOR then begin
      TGIS_PrintLayoutBox(elm).Color :=
        TGIS_PrintUtils.TextToColor( TGIS_ColorComboBox(_sender).Value ) ;
      gisModel.Elements[idx].Invalidate ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_BOXFRAME then begin
      TGIS_PrintLayoutBox(elm).FrameColor :=
        TGIS_PrintUtils.TextToColor( TGIS_ColorComboBox(_sender).Value ) ;
      gisModel.Elements[idx].Invalidate ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_BOXWIDTH then begin
      SplitNumberAsText( TGIS_SizeComboBox(_sender).Value,
                         typ, field, value, units ) ;
      TGIS_PrintLayoutBox(elm).FrameWidth.SetValues(
        StrToFloat( value ), units
      ) ;
      gisModel.Elements[idx].Invalidate ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_FRAMECOLOR then begin
      TGIS_PrintLayoutFrame(elm).Color :=
        TGIS_PrintUtils.TextToColor( TGIS_ColorComboBox(_sender).Value ) ;
      gisModel.Elements[idx].Invalidate ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_TEXTCOLOR then begin
      case elm.ElementType of
        TGIS_PrintLayoutElementType.Box: ;
        TGIS_PrintLayoutElementType.Map: ;
        TGIS_PrintLayoutElementType.Legend:
          begin
            TGIS_PrintLayoutLegend(elm).FontColor :=
              TGIS_PrintUtils.TextToColor( TGIS_ColorComboBox(_sender).Value ) ;
            gisModel.Elements[idx].InvalidateCache ;
          end ;
        TGIS_PrintLayoutElementType.Scale:
          begin
            TGIS_PrintLayoutScale(elm).FontColor :=
              TGIS_PrintUtils.TextToColor( TGIS_ColorComboBox(_sender).Value ) ;
            gisModel.Elements[idx].InvalidateCache ;
          end ;
        TGIS_PrintLayoutElementType.NorthArrow: ;
        TGIS_PrintLayoutElementType.Graphic: ;
        TGIS_PrintLayoutElementType.Text:
          begin
            TGIS_PrintLayoutText(elm).Color :=
              TGIS_PrintUtils.TextToColor( TGIS_ColorComboBox(_sender).Value ) ;
            gisModel.Elements[idx].Invalidate ;
          end ;
        TGIS_PrintLayoutElementType.Frame: ;
        TGIS_PrintLayoutElementType.Page: ;
      end;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_TEXTBGCOLOR then begin
      TGIS_PrintLayoutText(elm).BackgroundColor :=
        TGIS_PrintUtils.TextToColor( TGIS_ColorComboBox(_sender).Value ) ;
      gisModel.Elements[idx].Invalidate ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_FRAMEWIDTH then begin
      SplitNumberAsText( TGIS_SizeComboBox(_sender).Value,
                         typ, field, value, units ) ;
      TGIS_PrintLayoutFrame(elm).Width.SetValues(
        StrToFloat( value ), units
      ) ;
      gisModel.Elements[idx].Invalidate ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_TEXTBGWIDTH then begin
      SplitNumberAsText( TGIS_SizeComboBox(_sender).Value,
                         typ, field, value, units ) ;
      TGIS_PrintLayoutText(elm).BackgroundWidth.SetValues(
        StrToFloat( value ), units
      ) ;
      gisModel.Elements[idx].Invalidate ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_FONTNAME then begin
      case elm.ElementType of
        TGIS_PrintLayoutElementType.Box: ;
        TGIS_PrintLayoutElementType.Map: ;
        TGIS_PrintLayoutElementType.Legend:
          begin
            TGIS_PrintLayoutLegend(elm).Font := TGIS_FontNameComboBox(_sender).Value ;
            gisModel.Elements[idx].InvalidateCache ;
          end ;
        TGIS_PrintLayoutElementType.Scale:
          begin
            TGIS_PrintLayoutScale(elm).Font := TGIS_FontNameComboBox(_sender).Value ;
            gisModel.Elements[idx].InvalidateCache ;
          end ;
        TGIS_PrintLayoutElementType.NorthArrow: ;
        TGIS_PrintLayoutElementType.Graphic: ;
        TGIS_PrintLayoutElementType.Text:
          begin
            TGIS_PrintLayoutText(elm).Font := TGIS_FontNameComboBox(_sender).Value ;
            gisModel.Elements[idx].Invalidate ;
          end ;
        TGIS_PrintLayoutElementType.Frame: ;
        TGIS_PrintLayoutElementType.Page: ;
      end;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_FONTSIZE then begin
      case elm.ElementType of
        TGIS_PrintLayoutElementType.Box: ;
        TGIS_PrintLayoutElementType.Map: ;
        TGIS_PrintLayoutElementType.Legend:
          begin
            SplitNumberAsText( TGIS_SizeComboBox(_sender).Value,
                               typ, field, value, units ) ;
            TGIS_PrintLayoutLegend(elm).FontSize := StrToInt( value ) ;
            gisModel.Elements[idx].InvalidateCache ;
          end;
        TGIS_PrintLayoutElementType.Scale:
          begin
            SplitNumberAsText( TGIS_SizeComboBox(_sender).Value,
                               typ, field, value, units ) ;
            TGIS_PrintLayoutScale(elm).FontSize := StrToInt( value ) ;
            gisModel.Elements[idx].InvalidateCache ;
          end;
        TGIS_PrintLayoutElementType.NorthArrow: ;
        TGIS_PrintLayoutElementType.Graphic: ;
        TGIS_PrintLayoutElementType.Text:
          begin
            SplitNumberAsText( TGIS_SizeComboBox(_sender).Value,
                               typ, field, value, units ) ;
            TGIS_PrintLayoutText(elm).Size := StrToInt( value ) ;
            gisModel.Elements[idx].Invalidate ;
          end;
        TGIS_PrintLayoutElementType.Frame: ;
        TGIS_PrintLayoutElementType.Page: ;
      end;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_FONTSTYLE then begin
      // field already set
      gisModel.Elements[idx].Invalidate ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_TEXTALIGN then begin
      case TComboBox(_sender).ItemIndex of
        0: TGIS_PrintLayoutText(elm).Align := TGIS_LabelAlignment.LeftJustify ;
        1: TGIS_PrintLayoutText(elm).Align := TGIS_LabelAlignment.Center ;
        2: TGIS_PrintLayoutText(elm).Align := TGIS_LabelAlignment.RightJustify ;
      end ;
      gisModel.Elements[idx].Invalidate ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_GRAPHICSPATH then begin
      // field already set
      gisModel.Elements[idx].Invalidate ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_COMPACTVIEW then begin
      case TComboBox(_sender).ItemIndex of
        0 : TGIS_PrintLayoutLegend(elm).CompactView := False ;
        1 : TGIS_PrintLayoutLegend(elm).CompactView := True ;
      end ;
      gisModel.Elements[idx].InvalidateCache ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_DRAWICONSTYLE then begin
      case TComboBox(_sender).ItemIndex of
        0 : TGIS_PrintLayoutLegend(elm).DrawIconStyle := TGIS_LegendIconStyle.Default ;
        1 : TGIS_PrintLayoutLegend(elm).DrawIconStyle := TGIS_LegendIconStyle.Rectangular ;
      end ;
      gisModel.Elements[idx].InvalidateCache ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_REVERSEORDER then begin
      case TComboBox(_sender).ItemIndex of
        0 : TGIS_PrintLayoutLegend(elm).ReverseOrder := False ;
        1 : TGIS_PrintLayoutLegend(elm).ReverseOrder := True ;
      end ;
      gisModel.Elements[idx].InvalidateCache ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_DIVIDERS then begin
      TGIS_PrintLayoutScale(elm).Dividers := TComboBox(_sender).ItemIndex + 1 ;
      gisModel.Elements[idx].InvalidateCache ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_DIVIDERCOLOR1 then begin
      TGIS_PrintLayoutScale(elm).DividerColor1 :=
        TGIS_PrintUtils.TextToColor( TGIS_ColorComboBox(_sender).Value ) ;
      gisModel.Elements[idx].InvalidateCache ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_DIVIDERCOLOR2 then begin
      TGIS_PrintLayoutScale(elm).DividerColor2 :=
        TGIS_PrintUtils.TextToColor( TGIS_ColorComboBox(_sender).Value ) ;
      gisModel.Elements[idx].InvalidateCache ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_COLOR1 then begin
      TGIS_PrintLayoutNorthArrow(elm).Color1 :=
        TGIS_PrintUtils.TextToColor( TGIS_ColorComboBox(_sender).Value ) ;
      gisModel.Elements[idx].InvalidateCache ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_COLOR2 then begin
      TGIS_PrintLayoutNorthArrow(elm).Color2 :=
        TGIS_PrintUtils.TextToColor( TGIS_ColorComboBox(_sender).Value ) ;
      gisModel.Elements[idx].InvalidateCache ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_NARROWSTYLE then begin
      TGIS_PrintLayoutNorthArrow(elm).Style :=
        ParamNorthArrowStyle(
          TComboBox(_sender).Items[TComboBox(_sender).ItemIndex],
          TGIS_ControlNorthArrowStyle.Arrow1
        ) ;
      gisModel.Elements[idx].InvalidateCache ;
    end
    else
    if ( name = GIS_TPL_DESIGNER_FLD_NARROWPATH ) then begin
      gisModel.Elements[idx].InvalidateCache ;
    end;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.tplObjectInspectorClick(
    _sender : TObject
  ) ;
  var
    name : String ;
    idx  : Integer ;
    elm  : TGIS_PrintLayoutElement ;
    posPanel  : TGIS_PositionPanel ;
    dlg_pos   : TGIS_ControlPrintPositionForm ;
    dlg_path  : TOpenDialog ;
    dlg_style : TGIS_ControlFontStyleForm ;
  begin
    name := TControl(_sender).Name ;

    idx := cmbObjectInspector.ItemIndex ;
    if idx = -1 then exit ;

    elm := cmbObjectInspector.Items.Objects[idx] as TGIS_PrintLayoutElement ;

    if ( name = GIS_TPL_DESIGNER_FLD_LEFT ) or
       ( name = GIS_TPL_DESIGNER_FLD_TOP ) or
       ( name = GIS_TPL_DESIGNER_FLD_RIGHT ) or
       ( name = GIS_TPL_DESIGNER_FLD_BOTTOM ) then begin
      posPanel := TGIS_PositionPanel(_sender) ;
      dlg_pos := TGIS_ControlPrintPositionForm.Create( Self ) ;
      try
        dlg_pos.BiDiMode := BiDiMode ;
        if name = GIS_TPL_DESIGNER_FLD_LEFT then
          dlg_pos.Caption := _rsrc( GIS_RS_TPL_DESIGNER_POSITION ) + ': ' +
                             _rsrc( GIS_RS_TPL_DESIGNER_FIELD_LEFT )
        else if name = GIS_TPL_DESIGNER_FLD_TOP then
          dlg_pos.Caption := _rsrc( GIS_RS_TPL_DESIGNER_POSITION ) + ': ' +
                             _rsrc( GIS_RS_TPL_DESIGNER_FIELD_TOP )
        else if name = GIS_TPL_DESIGNER_FLD_RIGHT then
          dlg_pos.Caption := _rsrc( GIS_RS_TPL_DESIGNER_POSITION ) + ': ' +
                             _rsrc( GIS_RS_TPL_DESIGNER_FIELD_RIGHT )
        else if name = GIS_TPL_DESIGNER_FLD_BOTTOM then
          dlg_pos.Caption := _rsrc( GIS_RS_TPL_DESIGNER_POSITION ) + ': ' +
                             _rsrc( GIS_RS_TPL_DESIGNER_FIELD_BOTTOM ) ;
        dlg_pos.PPI := FPrinter.PPI ;
        dlg_pos.PrintArea := FPrinter.PrintArea ;
        dlg_pos.PageWidth := FPrinter.PageSize.X ;
        dlg_pos.PageHeight := FPrinter.PageSize.Y ;
        dlg_pos.AnchorCache := posPanel.AnchorCache ;
        if name = GIS_TPL_DESIGNER_FLD_LEFT then begin
          dlg_pos.MaxValue := FPrinter.PageSize.X ;
          dlg_pos.SetValues( elm.Location.Left.Value,
                             TGIS_PrintUtils.UnitToText( elm.Location.Left.Units ),
                             elm.Location.Left.Anchor ) ;
        end
        else if name = GIS_TPL_DESIGNER_FLD_TOP then begin
          dlg_pos.MaxValue := FPrinter.PageSize.Y ;
          dlg_pos.SetValues( elm.Location.Top.Value,
                             TGIS_PrintUtils.UnitToText( elm.Location.Top.Units ),
                             elm.Location.Top.Anchor ) ;
        end
        else if name = GIS_TPL_DESIGNER_FLD_RIGHT then begin
          dlg_pos.MaxValue := FPrinter.PageSize.X ;
          dlg_pos.SetValues( elm.Location.Right.Value,
                             TGIS_PrintUtils.UnitToText( elm.Location.Right.Units ),
                             elm.Location.Right.Anchor ) ;
        end
        else if name = GIS_TPL_DESIGNER_FLD_BOTTOM then begin
          dlg_pos.MaxValue := FPrinter.PageSize.Y ;
          dlg_pos.SetValues( elm.Location.Bottom.Value,
                             TGIS_PrintUtils.UnitToText( elm.Location.Bottom.Units ),
                             elm.Location.Bottom.Anchor ) ;
        end ;

        if dlg_pos.Execute( pOnHelp ) = mrOK then begin
          if name = GIS_TPL_DESIGNER_FLD_LEFT then begin
            elm.Location.Left.SetValues(
              dlg_pos.GetValue,
              dlg_pos.GetUnit,
              dlg_pos.GetAnchor
             ) ;
            posPanel.Value := elm.Location.Left.AsText ;
          end
          else if name = GIS_TPL_DESIGNER_FLD_RIGHT then begin
            elm.Location.Right.SetValues(
              dlg_pos.GetValue,
              dlg_pos.GetUnit,
              dlg_pos.GetAnchor
             ) ;
            posPanel.Value := elm.Location.Right.AsText ;
          end
          else if name = GIS_TPL_DESIGNER_FLD_TOP then begin
            elm.Location.Top.SetValues(
              dlg_pos.GetValue,
              dlg_pos.GetUnit,
              dlg_pos.GetAnchor
             ) ;
            posPanel.Value := elm.Location.Top.AsText ;
          end
          else if name = GIS_TPL_DESIGNER_FLD_BOTTOM then begin
            elm.Location.Bottom.SetValues(
              dlg_pos.GetValue,
              dlg_pos.GetUnit,
              dlg_pos.GetAnchor
             ) ;
            posPanel.Value := elm.Location.Bottom.AsText ;
          end;
          posPanel.Invalidate ;

          elm.Location.UpdateRectangle( FPrinter.PrintArea,
                                        FPrinter.PageSize.X,
                                        FPrinter.PageSize.Y,
                                        FPrinter.PPI ) ;
          gisModel.ApplyBounds( gisModel.Elements[idx], elm.Location.Rectangle ) ;
        end ;
      finally
        FreeObject( dlg_pos ) ;
      end ;
    end
    else
    if ( name = GIS_TPL_DESIGNER_FLD_GRAPHICSPATH_BTN ) then begin
      dlg_path := TOpenDialog.Create( Self ) ;
      try
        dlg_path.Filter := _rsrcna( GIS_RS_TPL_DESIGNER_FILTER_IMG ) ;
        dlg_path.InitialDir := GetFilePath( FTemplate.TemplatePath ) ;
        if dlg_path.Execute then begin
          if not IsStringEmpty( FTemplate.TemplatePath ) then
            TGIS_PrintLayoutGraphic(elm).Path := GetPathRelative( GetFilePath( FTemplate.TemplatePath ),
                                                                  dlg_path.FileName )
          else
            TGIS_PrintLayoutGraphic(elm).Path := GetPathRelative( FWorkingFolder,
                                                                  dlg_path.FileName ) ;
          T_PathButton(_sender).Edit.Text := TGIS_PrintLayoutGraphic(elm).Path ;
          tplObjectInspectorChange( T_PathButton(_sender).Edit ) ;
        end;
      finally
        FreeObject( dlg_path ) ;
      end ;
    end
    else
    if ( name = GIS_TPL_DESIGNER_FLD_NARROWPATH_BTN ) then begin
      dlg_path := TOpenDialog.Create( Self ) ;
      try
        dlg_path.Filter := _rsrcna( GIS_RS_TPL_DESIGNER_FILTER_IMG ) ;
        dlg_path.InitialDir := GetFilePath( FTemplate.TemplatePath ) ;
        if dlg_path.Execute then begin
          if not IsStringEmpty( FTemplate.TemplatePath ) then
            TGIS_PrintLayoutNorthArrow(elm).Path := GetPathRelative( GetFilePath( FTemplate.TemplatePath ),
                                                                     dlg_path.FileName )
          else
            TGIS_PrintLayoutNorthArrow(elm).Path := GetPathRelative( FWorkingFolder,
                                                                     dlg_path.FileName ) ;
          T_PathButton(_sender).Edit.Text := TGIS_PrintLayoutNorthArrow(elm).Path ;
          tplObjectInspectorChange( T_PathButton(_sender).Edit ) ;
        end;
      finally
        FreeObject( dlg_path ) ;
      end ;
    end
    else
    if ( name = GIS_TPL_DESIGNER_FLD_FONTSTYLE ) then begin
      dlg_style := TGIS_ControlFontStyleForm.Create( Self ) ;
      try
        dlg_style.BiDiMode := BiDiMode ;
        case elm.ElementType of
          TGIS_PrintLayoutElementType.Text :
            dlg_style.FontStyle := TGIS_PrintLayoutText(elm).Style ;
        end ;
        if dlg_style.Show = mrOk then begin
          case elm.ElementType of
            TGIS_PrintLayoutElementType.Text :
              begin
                TGIS_PrintLayoutText(elm).Style := dlg_style.FontStyle ;
                TGIS_FontStylePanel(_sender).Caption :=
                  TGIS_PrintUtils.FontStyleToText(
                    TGIS_PrintLayoutText(elm).Style ) ;
                tplObjectInspectorChange( _sender ) ;
              end ;
          end ;
        end ;
      finally
        FreeObject( dlg_style ) ;
      end;
    end ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.tplObjectInspectorKeyPress(
    _sender  : TObject ;
    var _key : Char
  ) ;
  var
    name : String ;
  begin

    name := TControl(_sender).Name ;
    if ( name = GIS_TPL_DESIGNER_FLD_NAME ) or
       ( name = GIS_TPL_DESIGNER_FLD_TEXT ) or
       ( name = GIS_TPL_DESIGNER_FLD_NARROWPATH ) or
       ( name = GIS_TPL_DESIGNER_FLD_XMIN ) or
       ( name = GIS_TPL_DESIGNER_FLD_XMAX ) or
       ( name = GIS_TPL_DESIGNER_FLD_YMIN ) or
       ( name = GIS_TPL_DESIGNER_FLD_YMAX ) or
       ( name = GIS_TPL_DESIGNER_FLD_GRAPHICSPATH ) then begin
      if _key = #13 then begin
        if _sender is TEdit then
          TEdit(_sender).SelectAll ;
        tplObjectInspectorExit( _sender ) ;
        _key := #0 ;
      end;
    end;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.tplObjectInspectorEnter(
    _sender : TObject
  ) ;
  begin
    if _sender is TEdit then
      PostMessage( TEdit(_sender).Handle, EM_SETSEL, 0, -1 ) ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.tplObjectInspectorExit(
    _sender : TObject
  ) ;
  var
    idx  : Integer ;
    elm  : TGIS_PrintLayoutElement ;
    name : String ;
    txt  : String ;
    val  : Double ;
    ext  : TGIS_Extent ;
  begin
    idx := cmbObjectInspector.ItemIndex ;
    if idx = -1 then exit ;

    elm := cmbObjectInspector.Items.Objects[idx] as TGIS_PrintLayoutElement ;
    name := TControl(_sender).Name ;

    if name = GIS_TPL_DESIGNER_FLD_NAME then begin
      elm.Name := TEdit(_sender).Text ;

      txt := elm.Name ;
      tvStructure.Items.Item[idx].Text := txt ;
      tvStructure.Selected := tvStructure.Items.Item[idx] ;
      cmbObjectInspector.Items[idx] := txt ;
      // refresh object inspector combobox
      cmbObjectInspector.ItemIndex := idx ;
      gisModel.Elements[idx].Caption := txt ;
      gisModel.Elements[idx].Invalidate ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_TEXT then begin
      TGIS_PrintLayoutText(elm).Text := TEdit(_sender).Text ;
      gisModel.Elements[idx].Invalidate ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_NARROWPATH then begin
      TGIS_PrintLayoutNorthArrow(elm).Path := TEdit(_sender).Text ;
      gisModel.Elements[idx].InvalidateCache ;
    end
    else
    if ( name = GIS_TPL_DESIGNER_FLD_XMIN ) then begin
      ext := TGIS_ViewerWnd( gisModel.Elements[idx].ElementObject ).VisibleExtent ;
      if TryStrToFloat( TEdit(_sender).Text, val ) then
        TGIS_ViewerWnd( gisModel.Elements[idx].ElementObject ).VisibleExtent := GisExtent( val, ext.YMin, ext.XMax, ext.YMax ) ;
    end
    else
    if ( name = GIS_TPL_DESIGNER_FLD_XMAX ) then begin
      ext := TGIS_ViewerWnd( gisModel.Elements[idx].ElementObject ).VisibleExtent ;
      if TryStrToFloat( TEdit(_sender).Text, val ) then
        TGIS_ViewerWnd( gisModel.Elements[idx].ElementObject ).VisibleExtent := GisExtent( ext.XMin, ext.YMin, val, ext.YMax ) ;
    end
    else
    if ( name = GIS_TPL_DESIGNER_FLD_YMIN ) then begin
      ext := TGIS_ViewerWnd( gisModel.Elements[idx].ElementObject ).VisibleExtent ;
      if TryStrToFloat( TEdit(_sender).Text, val ) then
        TGIS_ViewerWnd( gisModel.Elements[idx].ElementObject ).VisibleExtent := GisExtent( ext.XMin, val, ext.XMax, ext.YMax ) ;
    end
    else
    if ( name = GIS_TPL_DESIGNER_FLD_YMAX ) then begin
      ext := TGIS_ViewerWnd( gisModel.Elements[idx].ElementObject ).VisibleExtent ;
      if TryStrToFloat( TEdit(_sender).Text, val ) then
        TGIS_ViewerWnd( gisModel.Elements[idx].ElementObject ).VisibleExtent := GisExtent( ext.XMin, ext.YMin, ext.XMax, val ) ;
    end
    else
    if name = GIS_TPL_DESIGNER_FLD_GRAPHICSPATH then begin
      TGIS_PrintLayoutGraphic(elm).Path := TEdit(_sender).Text ;
      gisModel.Elements[idx].Invalidate ;
    end;
  end ;

  function TGIS_ControlPrintTemplateDesignerForm.tplObjectInspectorCustom(
    _sender : TObject ;
    _value  : String
  ) : String ;
  var
    name : String ;
    dlg_color : TGIS_ColorDialog ;
    dlg_size  : TGIS_ControlSizeForm ;
    on_help   : TGIS_HelpEvent ;
    proc  : TGIS_Proc ;
    val : String ;
  begin
    if _value = GIS_PARAMTXT_TYPE_CUSTOM then begin
      name := TControl(_sender).Name ;
      if ( name = GIS_TPL_DESIGNER_FLD_DIVIDERCOLOR1 ) or
         ( name = GIS_TPL_DESIGNER_FLD_DIVIDERCOLOR2 ) or
         ( name = GIS_TPL_DESIGNER_FLD_COLOR1 ) or
         ( name = GIS_TPL_DESIGNER_FLD_COLOR2 ) or
         ( name = GIS_TPL_DESIGNER_FLD_BOXCOLOR ) or
         ( name = GIS_TPL_DESIGNER_FLD_BOXFRAME ) or
         ( name = GIS_TPL_DESIGNER_FLD_FRAMECOLOR ) or
         ( name = GIS_TPL_DESIGNER_FLD_TEXTCOLOR ) or
         ( name = GIS_TPL_DESIGNER_FLD_TEXTBGCOLOR ) then begin
        dlg_color := TGIS_ColorDialog.Create( Self ) ;
        try
          if dlg_color.Execute( TGIS_Color.White ) = mrOK then begin
            Result := ConstructParamAsText(
                        GIS_PARAMTXT_TYPE_ARGB,
                        IntToHex( dlg_color.AlphaColor.ARGB, 2 ),
                        ''
                      ) ;
          end ;
        finally
          FreeObject( dlg_color ) ;
        end ;
      end
      else
      if ( name = GIS_TPL_DESIGNER_FLD_BOXWIDTH ) or
         ( name = GIS_TPL_DESIGNER_FLD_FRAMEWIDTH ) or
         ( name = GIS_TPL_DESIGNER_FLD_TEXTBGWIDTH ) then begin
        dlg_size := TGIS_ControlSizeForm.Create( Self ) ;
        on_help := nil ;
        dlg_size.FillPrintWidthUnits ;
        proc := procedure(_modal_result: TGIS_PvlModalResult)
        begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit;
        if dlg_size.isRotation then
          val := GIS_PARAMTXT_TYPE_ANGLE + ':' + dlg_size.spnFactor.Text + ' ' +
            dlg_size.cmbUnits.Text
        else
          val := GIS_PARAMTXT_TYPE_SIZE + ':' + dlg_size.spnFactor.Text + ' ' +
            dlg_size.cmbUnits.Text
        end ;
        dlg_size.Execute( on_help, proc ) ;
        Result := val ;
      end
      else
      if ( name = GIS_TPL_DESIGNER_FLD_FONTSIZE ) then begin
        dlg_size := TGIS_ControlSizeForm.Create( Self ) ;
        on_help := nil ;
        dlg_size.FillPrintFontSizeUnits ;
        //only integer values
        dlg_size.Precision := 0 ;
        proc := procedure(_modal_result: TGIS_PvlModalResult)
        begin
        if _modal_result <> TGIS_PvlModalResult.OK then
          exit;
        if dlg_size.isRotation then
          val := GIS_PARAMTXT_TYPE_ANGLE + ':' + dlg_size.spnFactor.Text + ' ' +
            dlg_size.cmbUnits.Text
        else
          val := GIS_PARAMTXT_TYPE_SIZE + ':' + dlg_size.spnFactor.Text + ' ' +
            dlg_size.cmbUnits.Text
        end ;
        dlg_size.Execute( on_help, proc ) ;
        Result := val ;
      end;
    end ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.tvToolPaletteMouseDown(
    _sender : TObject ;
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x, _y  : Integer
  ) ;
  var
    nd : TTreeNode ;
  begin
    finishChangingExtent ;
    if _button = TMouseButton.mbLeft then begin
      nd := tvToolPalette.GetNodeAt( _x, _y ) ;
      if assigned( nd ) then begin
        if nd.Text = GIS_RS_TPL_DESIGNER_TOOLPALETTE_MAP then
          initElement( TGIS_PrintLayoutElementType.Map ) ;
        if nd.Text = GIS_RS_TPL_DESIGNER_TOOLPALETTE_LEGEND then
          initElement( TGIS_PrintLayoutElementType.Legend ) ;
        if nd.Text = GIS_RS_TPL_DESIGNER_TOOLPALETTE_SCALE then
          initElement( TGIS_PrintLayoutElementType.Scale ) ;
        if nd.Text = GIS_RS_TPL_DESIGNER_TOOLPALETTE_NORTHARROW then
          initElement( TGIS_PrintLayoutElementType.NorthArrow ) ;
        if nd.Text = GIS_RS_TPL_DESIGNER_TOOLPALETTE_BOX then
          initElement( TGIS_PrintLayoutElementType.Box ) ;
        if nd.Text = GIS_RS_TPL_DESIGNER_TOOLPALETTE_TEXT then
          initElement( TGIS_PrintLayoutElementType.Text ) ;
        if nd.Text = GIS_RS_TPL_DESIGNER_TOOLPALETTE_GRAPHIC then
          initElement( TGIS_PrintLayoutElementType.Graphic ) ;
      end ;
    end ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.Paint ;
  begin
    pnlPageControl.Constraints.MinWidth := btnPreview.Left + btnPreview.Width + 10 ;
    // resize image list only if required
    if ( imgListScld.Count <= 0 ) or
       ( CurrentPPI <> PixelsPerInch )
    then
      GisScaleImageList( imgList, imgListScld, CurrentPPI, PixelsPerInch );
    inherited ;
  end ;

  procedure TGIS_ControlPrintTemplateDesignerForm.KeyDown(
    var _key   : Word ;
        _shift : TShiftState
  ) ;
  begin
    finishChangingExtent ;
    case _key of
      VK_DELETE :
        begin
          if not tplObjectInspector.HasActiveControl( ActiveControl ) then
            btnStructureDelClick( nil ) ;
        end ;
      VK_LEFT :
        begin
          if ActiveControl = pageControl then
            gisModel.MoveFocusElementLeft ;
        end ;
      VK_RIGHT :
        begin
          if ActiveControl = pageControl then
            gisModel.MoveFocusElementRight ;
        end ;
      VK_UP :
        begin
          if ActiveControl = pageControl then
            gisModel.MoveFocusElementUp ;
        end ;
      VK_DOWN :
        begin
          if ActiveControl = pageControl then
            gisModel.MoveFocusElementDown ;
        end ;
    end;
  end ;

  function TGIS_ControlPrintTemplateDesignerForm.Execute(
    _template : TGIS_TemplatePrint ;
    _workingFolder : String
  ) : Integer ;
  begin
    Result := Execute( _template, _workingFolder, '' ) ;
  end ;

  function TGIS_ControlPrintTemplateDesignerForm.Execute(
    _template      : TGIS_TemplatePrint ;
    _workingFolder : String ;
    _customPage    : String
  ) : Integer ;
  var
    prn : String ;
  begin
    prn := resolvePrinter( _customPage ) ;
    setPrinter( prn, False ) ;
    initDesigner( _template, _workingFolder ) ;

    try

      btnHelp.Visible := Assigned( pOnHelp ) ;

      formResult := mrCancel ;
      Result := ShowModal ;
      if Result = mrCancel then
        Result := formResult ;
      if ( Result = mrOK ) and not IsStringEmpty( FTemplate.TemplatePath ) then
        updateInputTemplate( _template ) ;

    finally
      cleanupDesigner ;
    end;
  end ;

  class function TGIS_ControlPrintTemplateDesignerForm.ShowPrintTemplateDesigner(
    _template : TGIS_TemplatePrint ;
    _workingFolder : String
  ) : Integer ;
  begin
    Result := ShowPrintTemplateDesigner( _template, _workingFolder, '' ) ;
  end ;

  class function TGIS_ControlPrintTemplateDesignerForm.ShowPrintTemplateDesigner(
    _template      : TGIS_TemplatePrint ;
    _workingFolder : String ;
    _customPage    : String
  ) : Integer ;
  var
    frm : TGIS_ControlPrintTemplateDesignerForm ;
  begin
    frm := TGIS_ControlPrintTemplateDesignerForm.Create( nil, True ) ;
    try
      Result := frm.Execute( _template, _workingFolder, _customPage ) ;
    finally
      frm.Free ;
    end;
  end ;

  class function TGIS_ControlPrintTemplateDesignerForm.ShowPrintTemplateDesigner(
    _template      : TGIS_TemplatePrint ;
    _workingFolder : String ;
    _customPage    : String ;
    _snap          : String
  ) : Integer ;
  var
    frm : TGIS_ControlPrintTemplateDesignerForm ;
  begin
    frm := TGIS_ControlPrintTemplateDesignerForm.Create( nil, True ) ;
    try
      frm.Snap := _snap ;
      Result := frm.Execute( _template, _workingFolder, _customPage ) ;
    finally
      frm.Free ;
    end;
  end ;
{$ENDREGION}

{$REGION 'GisShowPrintTemplateDesigner'}
  function GisShowPrintTemplateDesigner(
    _template : TGIS_TemplatePrint ;
    _workingFolder : String
  ) : Integer ;
  begin
    Result := TGIS_ControlPrintTemplateDesignerForm.ShowPrintTemplateDesigner(
                _template,
                _workingFolder
              ) ;
  end ;

  function GisShowPrintTemplateDesigner(
    _template      : TGIS_TemplatePrint ;
    _workingFolder : String ;
    _customPage    : String
  ) : Integer ;
  begin
    Result := TGIS_ControlPrintTemplateDesignerForm.ShowPrintTemplateDesigner(
                _template,
                _workingFolder,
                _customPage
              ) ;
  end ;
{$ENDREGION}

//==================================== END =====================================
end.

