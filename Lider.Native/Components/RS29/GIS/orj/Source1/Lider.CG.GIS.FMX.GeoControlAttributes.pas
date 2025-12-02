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
  Visual control for displaying and restructure shape attributes.
}

unit Lider.CG.GIS.FMX.GeoControlAttributes ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.FMX.GeoControlAttributes"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Math,
  System.Variants,

  FMX.Types,
  FMX.Forms,
  FMX.Objects,
  FMX.Controls,
  FMX.StdCtrls,
  FMX.ExtCtrls,
  FMX.Layouts,
  FMX.Memo,
  FMX.Edit,
  FMX.ListBox,
  FMX.Pickers,
  {$IFDEF LEVEL_XE5_FMX}
    FMX.Graphics,
  {$ENDIF}
  FMX.DateTimeCtrls,
  FMX.Menus,
  FMX.Platform,

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoInterfaces,
  Lider.CG.GIS.GeoCsBase,
  Lider.CG.GIS.GeoTypes,
  Lider.CG.GIS.GeoTypesUI,
  Lider.CG.GIS.GeoLayerVector,
  Lider.CG.GIS.GeoFieldRules;

type
  /// <summary>
  ///   Two layout types for attributes control.
  /// </summary>
  TGIS_LayoutType = (
    /// <summary>
    ///   Attributes names and values in one column.
    /// </summary>
    OneColumn,

    /// <summary>
    ///   Attributes names on left, attributes values on right.
    /// </summary>
    TwoColumns
  ) ;

  /// <summary>
  ///   Visual Shape attributes component.
  /// </summary>
  [ComponentPlatformsAttribute( pidAllPlatforms )]
  TGIS_ControlAttributes = class( TRectangle, IGIS_Subscribe )
    private

        /// <summary>
        ///   True, if component is Read Only.
        /// </summary>
        FReadOnly : Boolean ;

        /// <summary>
        ///   True, NULL fields are accepted.
        /// </summary>
        FAllowNull : Boolean ;

        /// <summary>
        ///   Unit type used for displaying GIS_LENGTH/GIS_AREA.
        /// </summary>
        FUnits : TGIS_CSUnits ;

        /// <summary>
        ///   When True, 'Fldx' rules are ignored.
        /// </summary>
        FIgnoreFldxDefinition : Boolean ;

        /// <summary>
        ///   Displaying mode.
        /// </summary>
        FLayoutType : TGIS_LayoutType ;

        ///  <summary>
        ///    Emulation of VCL BiDiMode for translation purpose
        ///  </summary>
        FBiDiMode      : TBiDiMode ;

        ///  <summary>
        ///    Decides where use BiDiMode from property or
        ///    from translation
        ///  </summary>
        FBiDiModeFromTranslation      : Boolean ;

        /// <summary>
        ///   True, if fields can be restructured.
        /// </summary>
        FAllowRestructure : Boolean ;

        /// <summary>
        ///   True, if internal fields are shown.
        /// </summary>
        FShowVirtualFields : Boolean ;

        /// <summary>
        ///   The set of virtual fields that are displayed when
        ///   ShowVirtualFields property is active.
        /// </summary>
        FVirtualFields : TGIS_VirtualFields ;

        /// <summary>
        ///   Width of the left column
        /// </summary>
        FFieldNameColumnWidth : Single ;

        /// <summary>
        ///   When True, button 'Ok' will be shown
        /// </summary>
        FShowBtnOk     : Boolean ;

        /// <summary>
        ///   When True, button 'Cancel' will be shown
        /// </summary>
        FShowBtnCancel : Boolean ;

        /// <summary>
        ///   Font used when displaying text
        /// </summary>
        FFont          : TFont ;

        /// <summary>
        ///   Color used when displaying text
        /// </summary>
        FFontColor     : TAlphaColor ;

        FStyledSettings : TStyledSettings ;

        /// <summary>
        ///   Field change event, fired after changing a control value.
        /// </summary>
        FFieldChangeEvent : TNotifyEvent ;

    private
      bCreated     : Boolean        ;
      bCancel      : Boolean        ;
      oGrid        : TScrollBox     ;
      oErrorMessage: TLabel         ;
      oBtnOk       : TButton        ;
      oBtnCancel   : TButton        ;
      uponDestroy  : Boolean        ;

    private
      pOnHelp      : TGIS_HelpEvent ;

    private
      procedure initiateFldList     ( _layer        : TGIS_LayerVector ;
                                      _shape        : TGIS_Shape
                                    ) ;
      procedure initiateStatList    ( _layer        : TGIS_LayerVector
                                    ) ;

    {$IFNDEF OXYGENE} private {$ELSE} assembly {$ENDIF}

      procedure fset_FieldNameColumnWidth
                                    ( const _value  : Single
                                    ) ;
      procedure fset_LayoutType     ( const _value  : TGIS_LayoutType
                                    ) ;
      procedure fset_BiDiMode       ( const _value  : TBiDiMode
                                    );
      procedure fset_BiDiModeFromTranslation( const _value  : Boolean
                                    );
      procedure fset_ReadOnly       ( const _value  : Boolean
                                    ) ;
      procedure fset_ShowBtnCancel  ( const _value  : Boolean
                                    ) ;
      procedure fset_ShowBtnOk      ( const _value  : Boolean
                                    ) ;
      procedure fset_Units          ( const _value  : TGIS_CSUnits
                                    ) ;
      function  fget_UnitsEPSG      : Integer ;
      procedure fset_UnitsEPSG      ( const _value  : Integer
                                    ) ;
      procedure fset_Font           ( const _value  : TFont
                                    ) ;

    {$IFNDEF OXYGENE} private {$ELSE} assembly {$ENDIF}

      /// <summary>
      ///   Button Cancel event handler.
      /// </summary>
      procedure doCancel            ( _sender       : TObject
                                    ) ; dynamic ;

      /// <summary>
      ///   Help event handler.
      /// </summary>
      procedure doHelp              ( _sender       : TObject
                                    ) ; dynamic ;

      /// <summary>
      ///   KeyDown event handler.
      /// </summary>
      procedure doKeyDown           ( _sender       : TObject      ;
                                      var _key      : Word         ;
                                      var _keyChar  : WideChar     ;
                                          _shift    : TShiftState
                                    ) ; dynamic ;

      /// <summary>
      ///   Button OK event handler.
      /// </summary>
      procedure doOk                ( _sender       : TObject
                                    ) ; dynamic ;
      procedure doResize            ( _sender       : TObject
                                    ) ; dynamic ;
      procedure updateLayout        ;

      procedure unsubscribeFromViewer ;
    protected

      /// <summary>
      ///   Paint handler.
      /// </summary>
      procedure Paint               ; override;

    public

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_owner">
      ///   owner of the component
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create            ( _owner       : TComponent
                                    ) ; override;

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      destructor Destroy ; override ;
    public
      /// <summary>
      ///   Handler for Enter event. Only for internal use.
      /// </summary>
      /// <param name="_sender">
      ///   sender of the event
      /// </param>
      procedure DoControlEnter      ( _sender       : TObject
                                    ) ; dynamic ;

      /// <summary>
      ///   Handler for Change event. Only for internal use.
      /// </summary>
      /// <param name="_sender">
      ///   sender of the event
      /// </param>
      procedure DoValueChanged      ( _sender       : TObject
                                    ) ; dynamic ;

    public

      /// <summary>
      ///   Clear attributes display. To avoid errors you should call this
      ///   before removing shapes, layer and closing viewer.
      /// </summary>
      procedure Clear               ;

      /// <summary>
      ///   Rebuild attributes display. TGIS_ControlAttributes does not
      ///   detect changing of layer structure, so after it call
      ///   Invalidate to rebuild data structure.
      /// </summary>
      procedure Invalidate          ;

      /// <summary>
      ///   Show attributes for a new shape.
      /// </summary>
      /// <param name="_shape">
      ///   new shape
      /// </param>
      procedure NewShape            ( const _shape  : TGIS_Shape
                                    ) ;

      /// <summary>
      ///   Show attributes for given _shape.
      /// </summary>
      /// <param name="_shape">
      ///   Shape object to display attribute filed for shape; nil to clear the
      ///   list
      /// </param>
      procedure ShowShape           ( const _shape  : TGIS_Shape
                                    ) ;

      /// <summary>
      ///   Show statistics for selected shapes.
      /// </summary>
      /// <param name="_layer">
      ///   layer taken into account
      /// </param>
      procedure ShowSelected        ( const _layer  : TGIS_LayerVector
                                    ) ;

      /// <summary>
      ///   Cancel changes made in the control.
      /// </summary>
      procedure CancelChanges       ;

      /// <summary>
      ///   Save changes made in the control.
      /// </summary>
      /// <returns>
      ///   True if layer updated successfully.
      /// </returns>
      function  SaveChanges         : Boolean ;

      /// <summary>
      ///   Copy information from the control to clipboard.
      /// </summary>
      procedure CopyToClipboard     ;

      /// <inheritdoc form="IGIS_Subscribe"/>
      procedure SubscribedEvent(       _sender  : TObject ;
                                       _event   : Integer ;
                                       _context : TObject
                               ) ;

    public

        /// <summary>
        ///   Units used for displaying GIS_LENGTH/GIS_AREA.
        /// </summary>
        property Units         : TGIS_CSUnits
                                         read  FUnits
                                         write fset_Units ;

        /// <summary>
        ///   Units EPSG code for displaying GIS_LENGTH and GIS_AREA fields.
        /// </summary>
        property UnitsEPSG     : Integer read  fget_UnitsEPSG
                                         write fset_UnitsEPSG ;

    published // properties defined in this class

        /// <summary>
        ///   True, NULL fields are accepted.
        /// </summary>
        property AllowNull     : Boolean read  FAllowNull
                                         write FAllowNull
                                         default False ;

        /// <summary>
        ///   True, fields can be restructured.
        /// </summary>
        property AllowRestructure
                               : Boolean read  FAllowRestructure
                                         write FAllowRestructure
                                         default True ;

        /// <summary>
        ///   Width of the left column
        /// </summary>
        property FieldNameColumnWidth
                               : Single  read  FFieldNameColumnWidth
                                         write fset_FieldNameColumnWidth ;

        /// <summary>
        ///   When True, 'Fldx' rules are ignored.
        /// </summary>
        property IgnoreFldxDefinition
                               : Boolean read  FIgnoreFldxDefinition
                                         write FIgnoreFldxDefinition
                                         default False ;

        /// <summary>
        ///   Layout type.
        /// </summary>
        property LayoutType    : TGIS_LayoutType
                                         read  FLayoutType
                                         write fset_LayoutType
                                         default
                                         TGIS_LayoutType.TwoColumns ;

        ///  <summary>
        ///    Emulation of VCL BiDiMode for translation purpose
        ///  </summary>
        property BiDiMode      : TBiDiMode
                                         read FBiDiMode
                                         write fset_BiDiMode
                                         default
                                         TBiDiMode.bdLeftToRight ;

        ///  <summary>
        ///    Decides where use BiDiMode from property or
        ///    from translation
        ///  </summary>
        property BiDiModeFromTranslation : Boolean
                                         read FBiDiModeFromTranslation
                                         write fset_BiDiModeFromTranslation
                                         default
                                         False;

        /// <summary>
        ///   Font used when displaying text
        /// </summary>
        property Font          : TFont   read  FFont
                                         write fset_Font ;

        /// <summary>
        ///   Color used when displaying text
        /// </summary>
        property FontColor     : TAlphaColor
                                         read  FFontColor
                                         write FFontColor ;

        /// <summary>
        ///   Set of styled text representation properties.
        /// </summary>
        property StyledSettings : TStyledSettings
                             read  FStyledSettings
                             write FStyledSettings ;

        /// <summary>
        ///   True, if 'Cancel' button comes up
        /// </summary>
        property ShowBtnCancel : Boolean read  FShowBtnCancel
                                         write fset_ShowBtnCancel
                                         default True ;

        /// <summary>
        ///   True, if 'OK' button comes up
        /// </summary>
        property ShowBtnOk     : Boolean read  FShowBtnOk
                                         write fset_ShowBtnOk
                                         default True ;

       /// <summary>
       ///   If true, fields like GIS_AREA, GIS_LENGTH will be displayed.
       /// </summary>
       /// <remarks>
       ///   <note type="important">
       ///     This method is deprecated. Use ShowVirtualFields.
       ///    </note>
       /// </remarks>
       property ShowInternalFields
                               : Boolean read  FShowVirtualFields
                                         write FShowVirtualFields
                                         default True ;

       /// <summary>
       ///   Show or hide virtual layer fields.
       /// </summary>
       property ShowVirtualFields
                               : Boolean read  FShowVirtualFields
                                         write FShowVirtualFields
                                         default True ;

       /// <summary>
       ///   The set of virtual fields that are displayed when
       ///   ShowVirtualFields property is active.
       /// </summary>
       property VirtualFields  : TGIS_VirtualFields read  FVirtualFields
                                                    write FVirtualFields ;

       /// <summary>
       ///   True, if component is Read Only.
       /// </summary>
       property &ReadOnly     : Boolean read  FReadOnly
                                       write fset_ReadOnly ;

    published // properties defined in this class

        /// <event/>
        /// <summary>
        ///   Event fired upon F1 key. Passed to any child form
        /// </summary>
        property HelpEvent      : TGIS_HelpEvent read  pOnHelp
                                                 write pOnHelp ;

        /// <summary>
        ///   Field change event, fired after changing a control value.
        /// </summary>
        property FieldChangeEvent : TNotifyEvent read  FFieldChangeEvent
                                                 write FFieldChangeEvent ;
  end ;

  {#gendoc:hide}
  procedure Register;

//##############################################################################
implementation

{$IFNDEF IOS}
  {$R 'GisControlAttributes_16x16.RES'}
{$ENDIF}

uses
  {$IFNDEF GIS_NOADO_JOIN}
    {$IFDEF ADOINTUNIT}
      Winapi.ADOint,
    {$ELSE}
      Lider.CG.GIS.GeoAdoInt,
    {$ENDIF}
  {$ENDIF}
  System.Generics.Collections,
  System.Generics.Defaults,

  Lider.CG.GIS.GeoInternals,
  Lider.CG.GIS.FMX.GeoControlHelper,
  Lider.CG.GIS.FMX.GeoFramework,
  Lider.CG.GIS.GeoLayer,
  Lider.CG.GIS.FMX.GeoControlAttributesForm;

const
  HGAP = 4 ;
  VGAP = 2 ;
  GGAP = 6 ;

type

  T_Edit = class( FMX.Edit.TEdit )
  public
    Style : TRectangle ;
    procedure SetColor  ( _color : TColor ) ;
    property AdjustType : TAdjustType read  GetAdjustType
                                      write SetAdjustType ;
  end;

  T_Memo = class( FMX.Memo.TMemo )
  public
    Style : TRectangle ;
    procedure SetColor  ( _color : TColor ) ;
    property AdjustType : TAdjustType read  GetAdjustType
                                      write SetAdjustType ;
  end;

  T_CheckBox = class( FMX.StdCtrls.TCheckBox )
  public
    Style : TRectangle ;
    procedure SetColor  ( _color : TColor ) ;
    property AdjustType : TAdjustType read  GetAdjustType
                                      write SetAdjustType ;
  end;

  T_ComboBox = class( FMX.ListBox.TComboBox )
  public
    property AdjustType : TAdjustType read  GetAdjustType
                                      write SetAdjustType ;
  end;

  T_DateEdit = class( FMX.DateTimeCtrls.TDateEdit )
  protected
    oldFormat : String ;
  protected
    procedure DoDateTimeChanged; override ;
  public
    property AdjustType : TAdjustType read  GetAdjustType
                                      write SetAdjustType ;
  end;

  T_ScrollBox = class ( TScrollBox )
    private
      oParent                  : TGIS_ControlAttributes ;
      oLayer                   : TGIS_LayerVector ;
      oLayerName               : String ;
      oShape                   : TGIS_Shape ;
      oField                   : String     ;
      oViewer                  : IGIS_Viewer ;
      eLayout                  : TGIS_LayoutType ;
      oPanel                   : TRectangle ;
      oPanelLeft               : TLayout    ;
      oPanelRight              : TLayout    ;
      oSplitter                : TSplitter  ;
      oPopup1                  : TPopupMenu ;
      oPopup2                  : TPopupMenu ;
      FBiDiMode                : TBiDiMode ;
      pOnHelp                  : TGIS_HelpEvent ;
      iMnuModify               : Integer ;
      iMnuDelete               : Integer ;
    private
      function  getFieldId    ( _name         : String ;
                                var _join     : Boolean
                              ) : Integer ;
      function  getFieldName  ( _name         : String
                              ) : String ;
      function  getFieldFormat( _name         : String
                              ) : String ;
      function  setFieldName  ( _index        : Integer
                              ) : String ;
      function  checkValue    ( _info         : TGIS_FieldInfo ;
                                _value        : String ;
                                _mode         : TGIS_FieldValueCheckMode ;
                                var _msg      : String
                              ) : Boolean ;
      function  getHeaderCaption
                              ( _stat         : Boolean
                              ) : String ;
      procedure addHeader     ;
      function  addLabel      ( _fld_no       : Integer ;
                                _fld          : TGIS_FieldInfo ;
                                _rule         : TGIS_FieldRule
                              ) : TLabel ;
      function  getMemoHeight : Single ;
      procedure addTextBoxStd ( _fld_no       : Integer ;
                                _fld          : TGIS_FieldInfo ;
                                _rule         : TGIS_FieldRule ;
                                _multiline    : Boolean ;
                                var _order    : Integer
                              ) ;
      procedure addTextBox    ( _fld_no       : Integer ;
                                _fld          : TGIS_FieldInfo ;
                                _rule         : TGIS_FieldRule ;
                                _multiline    : Boolean ;
                                var _order    : Integer
                              ) ;
      procedure addComboBox   ( _fld_no       : Integer ;
                                _fld          : TGIS_FieldInfo ;
                                _rule         : TGIS_FieldRule ;
                                var _order    : Integer
                              ) ;
      procedure addDateTime   ( _fld_no       : Integer ;
                                _fld          : TGIS_FieldInfo ;
                                _rule         : TGIS_FieldRule ;
                                var _order    : Integer
                              ) ;
      procedure addCheckBox   ( _fld_no       : Integer ;
                                _fld          : TGIS_FieldInfo ;
                                _rule         : TGIS_FieldRule ;
                                var _order    : Integer
                              ) ;
      function  isPredefined  ( _fld_name     : String
                              ) : Boolean ;
      procedure addPredefined ( _field        : String  ;
                                var _order    : Integer
                              ) ;
      procedure addInternalFields
                              ( const _controlAttributes : TGIS_ControlAttributes ;
                                const _layer  : TGIS_LayerVector ;
                                var   _order  : Integer
                              ) ;

      {$IFNDEF GIS_NODB}
        procedure addJoinDB   ( _fld_no       : Integer ;
                                var _order    : Integer
                              ) ;
      {$ENDIF}
      {$IFNDEF GIS_NOADO_JOIN}
        procedure addJoinADO  ( _fld_no       : Integer ;
                                var _order    : Integer
                              ) ;
      {$ENDIF}
      function  addLabelStat  ( _fld_no       : Integer ;
                                _fld          : TGIS_FieldInfo ;
                                _func         : String
                              ) : TLabel ;
      function  setFieldNameStat
                              ( _index        : Integer ;
                                _func         : String
                              ) : String ;
      function  addTextBoxStat( _fld_no       : Integer ;
                                _fld          : TGIS_FieldInfo ;
                                _func         : String  ;
                                var _order    : Integer
                              ) : TEdit ;

    private
      procedure doLabelClick  ( _sender       : TObject
                              ) ; dynamic ;
      procedure doLabelDblClick
                              ( _sender       : TObject
                              ) ; dynamic ;
      procedure doLabelMouseDown
                              ( _sender       : TObject ;
                                _button       : TMouseButton ;
                                _shift        : TShiftState ;
                                _x, _y        : Single
                              ) ; dynamic ;
      procedure doLabelMouseMove
                              ( _sender       : TObject ;
                                _shift        : TShiftState ;
                                _x, _y        : Single
                              ) ; dynamic ;
      procedure doLabelMouseUp( _sender       : TObject ;
                                _button       : TMouseButton ;
                                _shift        : TShiftState ;
                                _x, _y        : Single
                              ) ; dynamic ;
      procedure doValueClick  ( _sender       : TObject
                              ) ; dynamic ;
      procedure doValueKeyDown(     _sender   : TObject ;
                                var _key      : Word    ;
                                var _keyChar  : WideChar ;
                                    _shift    : TShiftState
                              ) ; dynamic ;
      procedure doValueKeyUp  (     _sender   : TObject ;
                                var _key      : Word    ;
                                var _keyChar  : WideChar ;
                                    _shift    : TShiftState
                              ) ; dynamic ;
      procedure doSplitterMoved
                              ( _sender       : TObject
                              ) ; dynamic ;
      procedure doPanelRightResize
                              ( _sender       : TObject
                              ) ; dynamic ;
      procedure doAddField    ( _sender       : TObject
                              ) ; dynamic ;
      procedure doModifyField ( _sender       : TObject
                              ) ; dynamic ;
      procedure doDeleteField ( _sender       : TObject
                              ) ; dynamic ;
      procedure doCopyToClipboard
                              ( _sender       : TObject
                              ) ; dynamic ;
    public
      constructor Create      ( _parent       : TComponent
                              ) ; override;

    public
      procedure SetAttributesControl( _control      : TGIS_ControlAttributes
                                    ) ;
      procedure ClearFields         ;
      procedure InitiateFieldsForViewing
                                    ( _layer        : TGIS_LayerVector ;
                                      _shape        : TGIS_Shape
                                    ) ;
      procedure InitiateFieldsForEditing
                                    ( _layer        : TGIS_LayerVector ;
                                      _shape        : TGIS_Shape
                                    ) ;
      procedure InitiateStatistics  ( _layer        : TGIS_LayerVector
                                    ) ;
      procedure ResetViewer         ;
      function  UpdateLayout        : Boolean ;
      function  ValidateTextBox     ( _control      : TControl ;
                                      _mode         : TGIS_FieldValueCheckMode ;
                                      var _msg      : String
                                    ) : Boolean ;
      function  ValidateFields      ( var _msg      : String
                                    ) : Boolean ;
      procedure CancelChanges       ;
      procedure UpdateLayer         ;
      procedure CopyToClipboard     ;
      procedure fset_BiDiMode       ( const _value  : TBiDiMode
                                    );

    public
      property  Viewer     : IGIS_Viewer      read oViewer ;
      property  Layer      : TGIS_LayerVector read oLayer ;
      property  LayerName  : String           read oLayerName ;
      property  Shape      : TGIS_Shape       read oShape ;
      property  LayoutType : TGIS_LayoutType  read eLayout ;
      property  BiDiMode   : TBiDiMode
                                       read FBiDiMode
                                       write fset_BiDiMode
                                       default
                                       TBiDiMode.bdLeftToRight ;
  end ;

  function clInvalid
    : TColor ;
  begin
    Result := FMXColor( TGIS_Color.FromRGB( 250, 189, 175 ) ) ;
  end ;

  function clValid
    : TColor ;
  begin
    Result := 16777215 ;
  end ;

//==============================================================================
// T_Edit
//==============================================================================

  procedure T_Edit.SetColor(
    _color : TColor
  ) ;
  begin
    if assigned( Style ) then
      Style.Fill.Color := _color ;
  end;

//==============================================================================
// T_Memo
//==============================================================================

  procedure T_Memo.SetColor(
    _color : TColor
  ) ;
  begin
    if assigned( Style ) then
      Style.Fill.Color := _color ;
  end;

//==============================================================================
// T_CheckBox
//==============================================================================

  procedure T_CheckBox.SetColor(
    _color : TColor
  ) ;
  begin
    if assigned( Style ) then
      Style.Fill.Color := _color ;
  end;

//==============================================================================
// T_DateEdit
//==============================================================================

  procedure T_DateEdit.DoDateTimeChanged;
  begin
    Format := oldFormat ;
    DateTime := VarToDateTime( Text ) ;

    inherited ;
  end ;

//==============================================================================
// T_ScrollBox
//==============================================================================

  constructor T_ScrollBox.Create(
    _parent : TComponent
  ) ;
  var
    new_item : TMenuItem ;
  begin
    {$IFNDEF ACTIVEX}
      inherited Create( _parent );
    {$ELSE}
      inherited CreateParented( _parent.Handle );
    {$ENDIF}

    BiDiMode := TGIS_ControlAttributes(_parent).BiDiMode ;
    Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight] ;
    ShowScrollBars := True ;
    HitTest := False ;

    if assigned( _parent ) then
      Width := TGIS_ControlAttributes(_parent).Width ;

    oPanel := TRectangle.Create( Self ) ;
    with oPanel do begin
      Parent := Self ;
      Sides  := [] ;
      HitTest := False ;
      Position.X := 0 ;
      Position.Y := 0 ;
      Width  := Self.Width ;
      Height := Self.Height ;
    end ;
    oPanelLeft := TLayout.Create( oPanel ) ;
    with oPanelLeft do begin
      Parent  := oPanel ;
      Align   := TAlignLayout.Left ;
      HitTest := True ;
      Width   := oPanel.Width / 2 ;
      OnMouseDown := doLabelMouseDown ;
    end ;

    oSplitter := TSplitter.Create( oPanel ) ;
    with oSplitter do begin
      Parent  := oPanel ;
      Height  := 263;
      MinSize := 30 ;
      Position.X := oPanelLeft.Width;
//      OnResize := doSplitterMoved;
    end ;

    oPanelRight := TLayout.Create( oPanel ) ;
    with oPanelRight do begin
      Parent := oPanel ;
      Align  := TAlignLayout.Client ;
      Width  := oPanel.Width - oPanelLeft.Width - oSplitter.Width ;
      OnResize := doPanelRightResize ;
    end ;

    oPopup1 := TPopupMenu.Create( oPanel ) ;
    with oPopup1 do begin
      Parent := oPanel ;
      Left := 100 ;
      Top  := 100 ;

      new_item := TMenuItem.Create( oPopup1 ) ;
      oPopup1.AddObject( new_item ) ;
      new_item.Text := _rsrc(GIS_RS_ATTRIBUTES_MNUADD) ;
      new_item.OnClick := doAddField ;

      new_item := TMenuItem.Create( oPopup1 ) ;
      oPopup1.AddObject( new_item ) ;
      iMnuModify := 1 ;
      new_item.Text := _rsrc(GIS_RS_ATTRIBUTES_MNUMODIFY) ;
      new_item.OnClick := doModifyField ;

      new_item := TMenuItem.Create( oPopup1 ) ;
      oPopup1.AddObject( new_item ) ;
      iMnuDelete := 2 ;
      new_item.Text := _rsrc(GIS_RS_ATTRIBUTES_MNUDELETE) ;
      new_item.OnClick := doDeleteField ;

      new_item := TMenuItem.Create( oPopup1 ) ;
      oPopup1.AddObject( new_item ) ;
      new_item.Text := _rsrcna( GIS_RS_ATTRIBUTES_MNUCOPYTOCLIPBOARD ) ;
      new_item.OnClick := doCopyToClipboard ;
    end ;

    oPopup2 := TPopupMenu.Create( oPanel ) ;
    with oPopup2 do begin
      Parent := oPanel ;
      Left := 100 ;
      Top  := 100 ;
      new_item := TMenuItem.Create( oPopup2 ) ;
      oPopup2.AddObject( new_item ) ;
      new_item.Text := _rsrcna( GIS_RS_ATTRIBUTES_MNUCOPYTOCLIPBOARD ) ;
      new_item.OnClick := doCopyToClipboard ;
    end ;

    Visible   := False ;
    oParent   := nil ;

    eLayout   := TGIS_LayoutType.TwoColumns ;
    oLayer    := nil ;
    oShape    := nil ;
    pOnHelp   := nil ;
  end ;

  procedure T_ScrollBox.SetAttributesControl(
    _control : TGIS_ControlAttributes
  ) ;
  begin
    oParent := _control ;
  end ;

  procedure T_ScrollBox.ClearFields ;
  var
    i : Integer ;
    cntrl : TControl ;
  begin
    oLayer := nil ;
    oLayerName := '' ;
    oShape := nil ;

    oPanelLeft.DeleteChildren ;
    oPanelRight.DeleteChildren ;
  end ;

  function T_ScrollBox.getFieldId(
        _name : String;
    var _join : Boolean
  ) : Integer ;
  var
    no      : Integer ;
    prefix  : String ;
    wasjoin : Boolean ;
  begin
    no := 0 ;

    wasjoin := False ;
    {$IFNDEF GIS_NODB}
      if assigned( oLayer.JoinDB ) then begin
        prefix := System.Copy( _name, StringFirst, 5 ) ;
        if prefix = 'valdb' then
          wasjoin := True ;
      end ;
    {$ENDIF}
    {$IFNDEF GIS_NOADO_JOIN}
      if not wasjoin and assigned( oLayer.JoinADO ) then begin
        prefix := System.Copy( _name, StringFirst, 5 ) ;
        if prefix = 'valdb' then
          wasjoin := True ;
      end ;
    {$ENDIF}

    if not wasjoin then
      no := StrToInt( System.Copy( _name, StringFirst + 3, Length(_name) - 3 ) )
    else
      no := StrToInt( System.Copy( _name, StringFirst + 5, Length(_name) - 5 ) ) ;

    Result := no ;
    _join  := wasjoin ;
  end ;

  function T_ScrollBox.getFieldFormat(
    _name : String
  ) : String ;
var
    no : Integer ;
    wasjoin : Boolean ;
  begin
    Result := '' ;
    try
      no := getFieldId( _name, wasjoin ) ;
      if not wasjoin then begin
        if assigned( oLayer.FieldInfo( no ).Rules ) then
          Result := TGIS_FieldRule(oLayer.FieldInfo(no).Rules).ValueFormat ;
      end
    except

    end;
  end ;

  function T_ScrollBox.getFieldName(
    _name : String
  ) : String ;
  var
    no : Integer ;
    prefix : String ;
    wasjoin : Boolean ;
  begin
    Result := '' ;
    no := getFieldId( _name, wasjoin ) ;

    if not wasjoin then
      Result := oLayer.FieldInfo( no ).NewName
    else begin
      {$IFNDEF GIS_NODB}
        if assigned( oLayer.JoinDB ) then begin
          Result := ToJoinFieldName(oLayer.JoinDB.Fields[no].DisplayName)
        end ;
      {$ENDIF}
      {$IFNDEF GIS_NOADO_JOIN}
        if  assigned( oLayer.JoinADO ) then begin
          Result := ToJoinFieldName(_Recordset(oLayer.JoinADO).Fields.Item[no].Name) ;
        end ;
      {$ENDIF}
    end;
  end ;

  function T_ScrollBox.setFieldName(
    _index : Integer
  ) : String ;
  begin
    Result := 'val' + IntToStr( _index ) ;
  end ;

  function T_ScrollBox.checkValue(
    _info    : TGIS_FieldInfo ;
    _value   : String ;
    _mode    : TGIS_FieldValueCheckMode ;
    var _msg : String
  ) : Boolean ;
  var
    oval : Variant ;
    rval : Variant ;
    i64  : Int64 ;
    fmt  : String ;
  begin
    Result := False ;
    with _info do begin
      try
        case FieldType of

          TGIS_FieldType.String :
            Result := True ;

          TGIS_FieldType.Number :
            begin
              if not IsStringEmpty( _value ) then begin
                oval := DotStrToFloat( _value ) ;
                rval := Double(oval) ;

                if NewDecimal = 0 then begin
                  if Frac( VarToDouble( rval ) ) <> 0 then Abort ;

                  i64 := VarToInt64( oval ) ;
                  if Abs( i64 ) > Abs( High( Integer ) ) then
                    rval := i64
                  else
                    rval := VarToInt32( oval )  ;
                end ;
              end ;
              Result := True ;
            end ;
          TGIS_FieldType.Float :
            begin
              if not IsStringEmpty( _value ) then begin
                oval := DotStrToFloat( _value ) ;
                rval := Double(oval) ;
              end ;
              Result := True ;
            end ;
          TGIS_FieldType.Boolean :
            begin
              if System.Length( _value ) >= 1 then
                case UpCase( _value[StringFirst] ) of
                  'T', '1', 'Y' : Result := True  ;
                  'F', '0', 'N' : Result := True ;
                end ;
            end ;
          TGIS_FieldType.Date :
            begin
              rval := VarAsType( _value, varDate ) ;
              Result := True ;
            end ;
        end ;
      except
        case FieldType of
          TGIS_FieldType.Number  :
            fmt := _rsrc( GIS_RS_ATTR_INVALID_FLOAT ) ;
          TGIS_FieldType.Float   :
            fmt := _rsrc( GIS_RS_ATTR_INVALID_NUMERIC ) ;
          TGIS_FieldType.Boolean :
            fmt := _rsrc( GIS_RS_ATTR_INVALID_LOGICAL ) ;
          TGIS_FieldType.Date    :
            fmt := _rsrc( GIS_RS_ATTR_INVALID_DATE ) ;
          else
            fmt := _rsrc( GIS_RS_ATTR_INVALID_ALPHANUM ) ;
        end;
        _msg := Format( fmt, [ _info.NewName ] ) ;
        exit ;
      end;
    end ;
  end ;

  function T_ScrollBox.ValidateTextBox(
    _control : TControl ;
    _mode    : TGIS_FieldValueCheckMode ;
    var _msg : String
  ) : Boolean ;
  var
    fld  : TGIS_FieldInfo ;
    rule : TGIS_FieldRule ;
    wasjoin : Boolean ;

    procedure set_control_color(
      _color : TColor
    ) ;
    begin
      if _control is T_Edit then
        T_Edit(_control).SetColor( _color )
      else if _control is T_Memo then
        T_Memo(_control).SetColor( _color )
      else if _control is T_CheckBox then
        T_CheckBox(_control).SetColor( _color ) ;
    end ;

  begin
    Result := True ;
    fld := oLayer.FieldInfo( getFieldId( _control.Name, wasjoin ) ) ;
    rule := TGIS_FieldRule( fld.Rules ) ;
    if Assigned( rule ) and not oParent.IgnoreFldxDefinition then begin
      if _control is TEdit then begin
        if not rule.Check( TEdit(_control).Text, _mode, _msg ) then
          Result := False ;
      end else if _control is TMemo then begin
        if not rule.Check( TMemo(_control).Text, _mode, _msg ) then
          Result := False ;
      end else if _control is TComboBox then begin
        if not rule.Check( TComboBox(_control).Selected.Text, _mode, _msg ) then
          Result := False ;
      end  else if _control is TCheckBox then begin
        if not rule.Check( BoolToStr( TCheckBox(_control).IsChecked, True ), _mode, _msg ) then
          Result := False ;
      end
    end else
    begin
      if _control is TEdit then
        Result := checkValue( fld, TEdit(_control).Text, _mode, _msg )
      else if _control is TMemo then
        Result := checkValue( fld, TMemo(_control).Text, _mode, _msg )
      else if _control is TCheckBox then
        Result := checkValue( fld, BoolToStr( TCheckBox(_control).IsChecked, True ), _mode, _msg )
      else if _control is TDateEdit then
        Result := checkValue( fld, DateTimeToStr( TDateEdit(_control).DateTime ), _mode, _msg ) ;
    end ;
    if Result = True then
      set_control_color( clValid )
    else
      set_control_color( clInvalid ) ;
  end ;

  function T_ScrollBox.ValidateFields(
    var _msg : String
  ) : Boolean ;
  var
    i : Integer ;
    cntrl : TControl ;
    fld : TGIS_FieldInfo ;
    msg : String ;
    wasjoin : Boolean ;
  begin
    Result := True ;
    _msg := '' ;
    //validate all fields
    for i := 0 to oPanelRight.Controls.Count - 1 do begin
      cntrl := oPanelRight.Controls[i] ;
      if cntrl is TLabel then
        continue ;
      if isPredefined( cntrl.Name ) then
        continue ;
      fld := oLayer.FieldInfo( getFieldId( cntrl.Name, wasjoin ) ) ;
      if fld.ReadOnly then continue ;
      if cntrl is TEdit then begin
        if not ValidateTextBox( TEdit(cntrl),
                                TGIS_FieldValueCheckMode.Both, msg
                              ) then begin
          Result := False ;
          if IsStringEmpty( _msg ) then
            _msg := msg ;
        end ;
      end else if cntrl is TMemo then begin
        if not ValidateTextBox( TMemo(cntrl),
                                TGIS_FieldValueCheckMode.Both, msg
                              ) then begin
          Result := False ;
          if IsStringEmpty( _msg ) then
            _msg := msg ;
        end ;
      end else if cntrl is TComboBox then begin
        if not ValidateTextBox( TComboBox(cntrl),
                                TGIS_FieldValueCheckMode.Both, msg
                              ) then begin
          Result := False ;
          if IsStringEmpty( _msg ) then
            _msg := msg ;
        end
      end
      else if cntrl is TDateEdit then begin
        if not ValidateTextBox( TDateEdit(cntrl),
                                TGIS_FieldValueCheckMode.Both, msg
                              ) then begin
          Result := False ;
          if IsStringEmpty( _msg ) then
            _msg := msg ;
        end ;
      end ;
    end ;
  end ;

  function T_ScrollBox.getHeaderCaption(
    _stat : Boolean
  ) : String ;
  begin
    if eLayout = TGIS_LayoutType.TwoColumns then begin
      if _stat then
        Result := IntToStr( oLayer.SelectedList.Count )
      else
        Result := IntToStr( oShape.Uid ) ;
    end else begin
      if BiDiMode = bdRightToLeft then
      begin
        if _stat then
          Result := Format( '%s : %s', [ IntToStr( oLayer.SelectedList.Count ), _rsrc(GIS_RS_ATTRIBUTES_DLG_COUNT) ] )
        else
          Result := Format( '%s : %s', [ IntToStr( oShape.Uid ), _rsrc(GIS_RS_ATTRIBUTES_DLG_UID) ] ) ;
      end
      else
      begin
        if _stat then
          Result := Format( '%s : %s', [ _rsrc(GIS_RS_ATTRIBUTES_DLG_COUNT), IntToStr( oLayer.SelectedList.Count ) ] )
        else
          Result := Format( '%s : %s', [ _rsrc(GIS_RS_ATTRIBUTES_DLG_UID), IntToStr( oShape.Uid ) ] ) ;
      end;
    end ;
  end ;

  procedure T_ScrollBox.addHeader ;
  var
    lbl  : TLabel ;
    uid  : TLabel ;
    stat : Boolean ;
  begin
    stat := not assigned( oShape ) ;
    // add label
    if eLayout = TGIS_LayoutType.TwoColumns then begin
      lbl := TLabel.Create( oPanelLeft ) ;
      lbl.Parent := oPanelLeft ;
      if stat then
        lbl.Text := _rsrcna(GIS_RS_ATTRIBUTES_DLG_COUNT)
      else
        lbl.Text := _rsrcna(GIS_RS_ATTRIBUTES_DLG_UID) ;
    end else begin
      lbl := TLabel.Create( oPanelRight ) ;
      lbl.Parent := oPanelRight ;
      lbl.Text := getHeaderCaption( stat ) ;
    end ;
    if stat then
      lbl.Name := 'lbl' + GIS_FIELD_SELECTED
    else
      lbl.Name := 'lbl' + GIS_FIELD_UID ;
    lbl.AutoSize := True ;
    lbl.WordWrap := False ;
    lbl.StyledSettings := oParent.StyledSettings - [TStyledSetting.Style] ;
    lbl.TextSettings.Font.Style := [TFontStyle.fsBold] ;
    lbl.Position.Y := GGAP ;
    if BiDiMode = bdRightToLeft then begin
      lbl.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop];
      lbl.TextAlign := TTextAlign.Trailing
    end
    else
    begin
      lbl.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];
      lbl.TextAlign := TTextAlign.Leading
    end;
    lbl.HitTest := True ;
    lbl.OnMouseDown := doLabelMouseDown ;
    // add content
    uid := TLabel.Create( oPanelRight ) ;
    uid.Parent := oPanelRight ;
    if stat then
      uid.Name := GIS_FIELD_SELECTED
    else
      uid.Name := GIS_FIELD_UID ;
    uid.AutoSize := True ;
    uid.WordWrap := False ;
    uid.StyledSettings := oParent.StyledSettings - [TStyledSetting.Style] ;
    uid.TextSettings.Font.Style := [TFontStyle.fsBold] ;
    if eLayout = TGIS_LayoutType.TwoColumns then begin
      if stat then
        uid.Text := IntToStr( oLayer.SelectedList.Count )
      else
        uid.Text := IntToStr( oShape.Uid ) ;
    end else begin
      uid.Text := '';
    end ;
    if BiDiMode = bdRightToLeft then begin
      uid.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop] ;
      uid.TextAlign := TTextAlign.Trailing
    end
    else
    begin
      uid.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];
      uid.TextAlign := TTextAlign.Leading;
    end;
    uid.Position.Y := GGAP ;
  end ;

  function T_ScrollBox.addLabel(
    _fld_no : Integer ;
    _fld    : TGIS_FieldInfo ;
    _rule   : TGIS_FieldRule
  ) : TLabel ;
  var
    lbl : TLabel ;
  begin
    if eLayout = TGIS_LayoutType.TwoColumns then begin
      lbl := TLabel.Create( oPanelLeft ) ;
      lbl.Parent := oPanelLeft ;
    end else begin
      lbl := TLabel.Create( oPanelRight ) ;
      lbl.Parent := oPanelRight ;
    end ;
    lbl.Name := 'lbl' + IntToStr( _fld_no ) ;
    lbl.StyledSettings := oParent.StyledSettings ;
    lbl.AutoSize := True ;
    lbl.WordWrap := False ;
    if Assigned( _rule ) and not IsStringEmpty( _rule.Caption ) then
      lbl.Text := _rule.Caption
    else
      lbl.Text := _fld.NewName ;
    if BiDiMode = bdRightToLeft then begin
      lbl.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop] ;
      lbl.TextAlign := TTextAlign.Trailing
    end
    else
    begin
      lbl.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];
      lbl.TextAlign := TTextAlign.Leading;
    end;
    lbl.HitTest := True ;
    lbl.OnClick     := doLabelClick     ;
    lbl.OnDblClick  := doLabelDblClick  ;
    lbl.OnMouseDown := doLabelMouseDown ;
    lbl.OnMouseMove := doLabelMouseMove ;
    lbl.OnMouseUp   := doLabelMouseUp   ;
    Result := lbl ;
  end ;

  function T_ScrollBox.getMemoHeight
    : Single ;
  var
    edt : TEdit ;
  begin
    Result := 20 ;
    edt := TEdit.Create( oPanelRight ) ;
    try
      Result := edt.Height * 2 ;
    finally
      FreeObject( edt ) ;
    end;
  end ;

  procedure T_ScrollBox.addTextBoxStd(
    _fld_no    : Integer ;
    _fld       : TGIS_FieldInfo ;
    _rule      : TGIS_FieldRule ;
    _multiline : Boolean ;
    var _order : Integer
  ) ;
  var
    lbl : TLabel ;
    mem : TMemo  ;
    edt : TEdit  ;
  begin
    lbl := addLabel( _fld_no, _fld, _rule ) ;
    if _multiline then begin
      mem := T_Memo.Create( oPanelRight ) ;
      mem.Parent := oPanelRight ;
      mem.Name := setFieldName( _fld_no ) ;
      mem.StyledSettings := oParent.StyledSettings ;
      T_Memo(edt).AdjustType := TAdjustType.None ;
      mem.ReadOnly := True ;
      mem.WordWrap := True ;
      mem.Text := '';
      mem.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
      mem.Height := getMemoHeight ;
      mem.ShowScrollBars := True ;
      if BiDiMode = TBiDiMode.bdRightToLeft then
        mem.TextAlign := TTextAlign.Trailing
      else
        mem.TextAlign := TTextAlign.Leading;
      mem.OnClick := doValueClick ;
      mem.TabOrder := _order ;
      inc( _order ) ;
    end else begin
      edt := T_Edit.Create( oPanelRight ) ;
      edt.Parent := oPanelRight ;
      edt.Name := setFieldName( _fld_no ) ;
      edt.StyledSettings := oParent.StyledSettings ;
      T_Edit(edt).AdjustType := TAdjustType.None ;
      edt.ReadOnly := True ;
      edt.Text := '';
      edt.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
      if BiDiMode = TBiDiMode.bdRightToLeft then
        edt.TextAlign := TTextAlign.Trailing
      else
        edt.TextAlign := TTextAlign.Leading ;
      edt.OnClick := doValueClick ;
      edt.TabOrder := _order ;
      edt.KillFocusByReturn := True ;
      inc( _order ) ;
    end ;
  end ;

  procedure T_ScrollBox.addTextBox(
    _fld_no    : Integer ;
    _fld       : TGIS_FieldInfo ;
    _rule      : TGIS_FieldRule ;
    _multiline : Boolean ;
    var _order : Integer
  ) ;
  var
    lbl : TLabel ;
    mem : T_Memo ;
    edt : T_Edit ;
    obj : TFmxObject ;
  begin
    lbl := addLabel( _fld_no, _fld, _rule ) ;
    if _multiline then begin
      mem := T_Memo.Create( oPanelRight ) ;
      mem.Parent := oPanelRight ;
      mem.ReadOnly := oParent.ReadOnly ;
      mem.WordWrap := True ;
      if not oParent.ReadOnly then begin
        mem.OnEnter := oParent.DoControlEnter ;
        mem.OnChange := oParent.DoValueChanged ;
        mem.OnChangeTracking := oParent.DoValueChanged ;
      end ;
      mem.Name := setFieldName( _fld_no ) ;
      mem.AdjustType := TAdjustType.None ;
      mem.StyledSettings := oParent.StyledSettings ;
      mem.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
      if BiDiMode = TBiDiMode.bdRightToLeft then
        mem.TextAlign := TTextAlign.Trailing
      else
        mem.TextAlign := TTextAlign.Leading;
      mem.Height := getMemoHeight ;
      mem.ShowScrollBars := True ;
      mem.OnClick := doValueClick ;
      if not mem.ReadOnly then begin
        mem.OnKeyDown  := doValueKeyDown  ;
        mem.OnKeyUp    := doValueKeyUp    ;
      end ;
      // style
      mem.StyleLookup := 'memostyle';
      obj := mem.FindStyleResource('content');
      if assigned( obj ) then begin
        mem.Style := TRectangle.Create( obj ) ;
        obj.AddObject( mem.Style ) ;
        mem.Style.Parent := obj ;
        mem.Style.HitTest := False ;
        mem.Style.Align := TAlignLayout.Client ;
        mem.Style.Fill.Color := clValid ;
        mem.Style.Stroke.Kind := TBrushKind.None ;
      end;
      mem.TabOrder := _order ;
      inc( _order ) ;
    end else begin
      edt := T_Edit.Create( oPanelRight ) ;
      edt.Parent := oPanelRight ;
      edt.ReadOnly := oParent.ReadOnly ;
      edt.Name := setFieldName( _fld_no ) ;
      edt.StyledSettings := oParent.StyledSettings ;
      edt.AdjustType := TAdjustType.None ;
      if not oParent.ReadOnly then begin
        edt.OnEnter := oParent.DoControlEnter ;
        edt.OnChange := oParent.DoValueChanged ;
        edt.OnChangeTracking := oParent.DoValueChanged ;
      end ;
      edt.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
      if BiDiMode = TBiDiMode.bdRightToLeft then
        edt.TextAlign := TTextAlign.Trailing
      else
        edt.TextAlign := TTextAlign.Leading ;
      edt.OnClick := doValueClick ;
      if not oParent.ReadOnly then begin
        edt.OnKeyDown  := doValueKeyDown  ;
        edt.OnKeyUp    := doValueKeyUp    ;
      end ;
      // style
      edt.StyleLookup := 'editstyle';
      obj := edt.FindStyleResource('content');
      if assigned( obj ) then begin
        edt.Style := TRectangle.Create( obj ) ;
        obj.AddObject( edt.Style ) ;
        edt.Style.Parent := obj ;
        edt.Style.HitTest := False ;
        edt.Style.Align := TAlignLayout.Client ;
        edt.Style.Fill.Color := clValid ;
        edt.Style.Stroke.Kind := TBrushKind.None ;
      end;
      edt.TabOrder := _order ;
      edt.KillFocusByReturn := True ;
      inc( _order ) ;
    end ;
  end ;

  procedure T_ScrollBox.addComboBox(
    _fld_no    : Integer ;
    _fld       : TGIS_FieldInfo ;
    _rule      : TGIS_FieldRule ;
    var _order : Integer
  ) ;
  var
    lbl  : TLabel ;
    cbx  : TComboBox ;
    i    : Integer ;
  begin
    lbl := addLabel( _fld_no, _fld, _rule ) ;
    cbx := T_ComboBox.Create( oPanelRight ) ;
    cbx.Parent := oPanelRight ;
    cbx.DropDownKind := TDropDownKind.Custom ;
    for i := 0 to _rule.Values.Items.Count-1 do
      cbx.Items.Add( _rule.Values.Items[i] ) ;
    cbx.Name := setFieldName( _fld_no ) ;
    T_ComboBox(cbx).AdjustType := TAdjustType.None ;
    for i := 0 to cbx.ListBox.Count-1 do
      cbx.ListBox.ListItems[0].StyledSettings := oParent.StyledSettings ;
    cbx.Enabled := not oParent.ReadOnly ;
    if not oParent.ReadOnly then begin
      cbx.OnEnter := oParent.DoControlEnter ;
      cbx.OnChange := oParent.DoValueChanged ;
    end ;
    cbx.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
    cbx.OnClick := doValueClick ;
    if not oParent.ReadOnly then begin
      cbx.OnKeyDown  := doValueKeyDown  ;
      cbx.OnKeyUp    := doValueKeyUp    ;
    end ;
    cbx.TabOrder := _order ;
    inc( _order ) ;
  end ;

  procedure T_ScrollBox.addDateTime(
    _fld_no    : Integer ;
    _fld       : TGIS_FieldInfo ;
    _rule      : TGIS_FieldRule ;
    var _order : Integer
  ) ;
  var
    attr : TGIS_ControlAttributes ;
    lbl  : TLabel ;
    dtp  : TDateEdit ;
    i    : Integer ;
  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    lbl := addLabel( _fld_no, _fld, _rule ) ;
    dtp := T_DateEdit.Create( oPanelRight ) ;
    dtp.Parent := oPanelRight ;
    dtp.DateFormatKind := TDTFormatKind.Short ;
    dtp.Format := FormatSettings.ShortDateFormat + ' ' +
                  FormatSettings.LongTimeFormat ;
    {$IFDEF LEVEL_RX11_VCL}
      // fix QC bug in TFormatSettings
      dtp.Format := StringReplaceAll( dtp.Format, 'mm', 'MM' ) ;
      dtp.Format := StringReplaceAll( dtp.Format, 'nn', 'mm' ) ;
    {$ENDIF}

    T_DateEdit(dtp).oldFormat := dtp.Format ;
    dtp.Name := setFieldName( _fld_no ) ;
    dtp.StyledSettings := oParent.StyledSettings ;
    T_DateEdit(dtp).AdjustType := TAdjustType.None ;
    dtp.Enabled := not attr.ReadOnly ;
    if not attr.ReadOnly then begin
      dtp.OnEnter := attr.doControlEnter ;
      dtp.OnChange := attr.doValueChanged ;
    end ;
    dtp.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop, TAnchorKind.akRight] ;
    dtp.OnClick := doValueClick ;

    if not attr.ReadOnly then begin
      dtp.OnKeyDown  := doValueKeyDown  ;
      dtp.OnKeyUp    := doValueKeyUp    ;
    end ;
    dtp.TabOrder := _order ;
    inc( _order ) ;
  end ;

  procedure T_ScrollBox.addCheckBox(
    _fld_no    : Integer ;
    _fld       : TGIS_FieldInfo ;
    _rule      : TGIS_FieldRule ;
    var _order : Integer
  ) ;
  var
    lbl : TLabel ;
    chk : T_CheckBox ;
    obj : TFmxObject ;
  begin
    lbl := addLabel( _fld_no, _fld, _rule ) ;
    chk := T_CheckBox.Create( oPanelRight ) ;
    chk.Parent := oPanelRight ;
    chk.Enabled := not oParent.ReadOnly ;
    chk.Name := setFieldName( _fld_no ) ;
    chk.StyledSettings := oParent.StyledSettings ;
    chk.AdjustType := TAdjustType.None ;
    chk.Text := '' ;
    if not oParent.ReadOnly then begin
      chk.OnEnter := oParent.DoControlEnter ;
      chk.OnClick := oParent.DoValueChanged ;
    end ;

    if BiDiMode = TBiDiMode.bdRightToLeft then
      chk.TextAlign := TTextAlign.Trailing
    else
      chk.TextAlign := TTextAlign.Leading;

//    chk.OnClick := doValueClick ;
    if not oParent.ReadOnly then begin
      chk.OnKeyDown  := doValueKeyDown  ;
      chk.OnKeyUp    := doValueKeyUp    ;
    end ;
    //style
    chk.StyleLookup := 'checkboxstyle';
    obj := chk.FindStyleResource('content');
    if assigned( obj ) then begin
      chk.Style := TRectangle.Create( obj ) ;
      obj.AddObject( chk.Style ) ;
      chk.Style.Parent := obj ;
      chk.Style.HitTest := False ;
      chk.Style.Align := TAlignLayout.Client ;
      chk.Style.Fill.Color := clValid ;
      chk.Style.Stroke.Kind := TBrushKind.None ;
    end;
    chk.TabOrder := _order ;
    inc( _order ) ;
  end ;

  function T_ScrollBox.isPredefined(
    _fld_name : String
  ) : Boolean ;
  begin
    Result := Pos( _fld_name, GIS_FIELDS_PREDEFINED ) >= StringFirst ;
  end ;

  procedure T_ScrollBox.addPredefined(
        _field : String ;
    var _order : Integer
  ) ;
  var
    lbl : TLabel ;
    tbx : TEdit ;
  begin
    // add label
    if eLayout = TGIS_LayoutType.TwoColumns then begin
      lbl := TLabel.Create( oPanelLeft ) ;
      lbl.Parent := oPanelLeft ;
    end else begin
      lbl := TLabel.Create( oPanelRight ) ;
      lbl.Parent := oPanelRight ;
    end ;
    lbl.Name := 'lbl' + _field ;
    lbl.AutoSize := True ;
    lbl.WordWrap := False ;
    lbl.StyledSettings := oParent.StyledSettings ;
    lbl.Text := _field ;
    if BiDiMode = bdRightToLeft then begin
      lbl.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop];
      lbl.TextAlign := TTextAlign.Trailing
    end
    else
    begin
      lbl.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];
      lbl.TextAlign := TTextAlign.Leading;
    end;

    lbl.HitTest := True ;
    lbl.OnClick     := doLabelClick     ;
    lbl.OnDblClick  := doLabelDblClick  ;
    lbl.OnMouseDown := doLabelMouseDown ;
    lbl.OnMouseMove := doLabelMouseMove ;
    lbl.OnMouseUp   := doLabelMouseUp   ;
    // add content
    tbx := T_Edit.Create( oPanelRight ) ;
    tbx.Parent := oPanelRight ;
    tbx.Name := _field ;
    tbx.StyledSettings := oParent.StyledSettings ;
    T_Edit(tbx).AdjustType := TAdjustType.None ;
    tbx.ReadOnly := True ;
    tbx.Text := '';

    if BiDiMode = bdRightToLeft then begin
      tbx.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop];
      tbx.TextAlign := TTextAlign.Trailing
    end
    else
    begin
      tbx.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];
      tbx.TextAlign := TTextAlign.Leading;
    end;

    tbx.OnClick := doValueClick ;
    tbx.TabOrder := _order ;
    inc( _order ) ;
  end ;

  {$IFNDEF GIS_NODB}
    procedure T_ScrollBox.addJoinDB(
          _fld_no   : Integer ;
      var _order    : Integer
    ) ;
    var
      fname : String ;
      lbl : TLabel ;
      tbx : TEdit ;
    begin
      fname := ToJoinFieldName(
                 oLayer.JoinDB.Fields[_fld_no].DisplayName
               ) ;
      // add label
      if eLayout = TGIS_LayoutType.TwoColumns then begin
        lbl := TLabel.Create( oPanelLeft ) ;
        lbl.Parent := oPanelLeft ;
      end else begin
        lbl := TLabel.Create( oPanelRight ) ;
        lbl.Parent := oPanelRight ;
      end ;
      lbl.Name := 'lbldb' + IntToStr( _fld_no ) ;
      lbl.AutoSize := True ;
      lbl.WordWrap := False ;
      lbl.StyledSettings := oParent.StyledSettings ;
      lbl.Text := fname ;
      if BiDiMode = bdRightToLeft then
        lbl.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
      else
        lbl.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];

      lbl.HitTest := True ;
      lbl.OnClick     := doLabelClick     ;
      lbl.OnDblClick  := doLabelDblClick  ;
      lbl.OnMouseDown := doLabelMouseDown ;
      lbl.OnMouseMove := doLabelMouseMove ;
      lbl.OnMouseUp   := doLabelMouseUp   ;
      // add content
      tbx := T_Edit.Create( oPanelRight ) ;
      tbx.Parent := oPanelRight ;
      tbx.Name := 'valdb' + IntToStr( _fld_no ) ;
      tbx.StyledSettings := oParent.StyledSettings ;
      T_Edit(tbx).AdjustType := TAdjustType.None ;
      tbx.ReadOnly := True ;
      tbx.Text := '';
      tbx.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
      tbx.OnClick := doValueClick ;
      tbx.TabOrder := _order ;
      inc( _order ) ;
    end ;
  {$ENDIF}

  {$IFNDEF GIS_NOADO_JOIN}
    procedure T_ScrollBox.addJoinADO(
          _fld_no   : Integer ;
      var _order    : Integer
    ) ;
    var
      fname : String ;
      lbl : TLabel ;
      tbx : TEdit ;
    begin
      fname := ToJoinFieldName(
                 _Recordset(oLayer.JoinADO).Fields.Item[_fld_no].Name
               ) ;
      // add label
      if eLayout = TGIS_LayoutType.TwoColumns then begin
        lbl := TLabel.Create( oPanelLeft ) ;
        lbl.Parent := oPanelLeft ;
      end else begin
        lbl := TLabel.Create( oPanelRight ) ;
        lbl.Parent := oPanelRight ;
      end ;
      lbl.Name := 'lbldb' + IntToStr( _fld_no ) ;
      lbl.AutoSize := True ;
      lbl.WordWrap := False ;
      lbl.StyledSettings := oParent.StyledSettings ;
      lbl.Text := fname ;
      if BiDiMode = bdRightToLeft then
        lbl.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
      else
        lbl.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];

      lbl.HitTest := True ;
      lbl.OnClick     := doLabelClick     ;
      lbl.OnDblClick  := doLabelDblClick  ;
      lbl.OnMouseDown := doLabelMouseDown ;
      lbl.OnMouseMove := doLabelMouseMove ;
      lbl.OnMouseUp   := doLabelMouseUp   ;
      // add content
      tbx := T_Edit.Create( oPanelRight ) ;
      tbx.Parent := oPanelRight ;
      tbx.Name := 'valdb' + IntToStr( _fld_no ) ;
      tbx.StyledSettings := oParent.StyledSettings ;
      T_Edit(tbx).AdjustType := TAdjustType.None ;
      tbx.ReadOnly := True ;
      tbx.Text := '';
      tbx.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop] ;
      tbx.OnClick := doValueClick ;
      tbx.TabOrder := _order ;
      inc( _order ) ;
    end ;
  {$ENDIF}

  procedure T_ScrollBox.InitiateFieldsForViewing(
    _layer : TGIS_LayerVector ;
    _shape : TGIS_Shape
  ) ;
  var
    order   : Integer ;
    i       : Integer ;
    fld     : TGIS_FieldInfo ;
    rule    : TGIS_FieldRule ;
    wasjoin : Boolean ;
    {$IFNDEF GIS_NODB}
      ii    : Integer ;
    {$ELSE}
      {$IFNDEF GIS_NOADO}
        ii  : Integer ;
      {$ENDIF}
    {$ENDIF}

  begin
    if assigned( _layer.Viewer ) then
      oViewer := _layer.Viewer.Ref
    else
      oViewer := nil ;
    oLayer  := _layer ;
    oLayerName := _layer.Name ;
    oShape  := _shape ;
    eLayout := oParent.LayoutType ;
    addHeader ;
    order := 0 ;

    addInternalFields( oParent, oLayer, order ) ;

    for i := 0 to oLayer.Fields.Count-1 do begin
      fld := oLayer.FieldInfo(i) ;
      if not (TGIS_FieldFlags.Visible in fld.Flags) then continue ;
      rule := TGIS_FieldRule(fld.Rules) ;
      if not oParent.IgnoreFldxDefinition and ( rule <> nil ) then begin
        if rule.Values.Mode = TGIS_FieldValuesMode.MultiLine then
          addTextBoxStd( i, fld, rule, True, order )
        else if rule.Values.Mode <> TGIS_FieldValuesMode.Hidden then
          addTextBoxStd( i, fld, rule, False, order ) ;
      end else begin
        rule := nil ;
        addTextBoxStd( i, fld, rule, False, order ) ;
      end ;
    end ;
    wasjoin := False ;
    {$IFNDEF GIS_NODB}
      if assigned( oLayer.JoinDB ) then begin
        wasjoin := True ;
        for ii := 0 to oLayer.JoinDB.FieldCount - 1 do begin
          if JoinFieldInfo( oLayer.JoinDB.Fields[ii] ) then
            addJoinDB( ii, order ) ;
        end ;
      end ;
    {$ENDIF}
    {$IFNDEF GIS_NOADO_JOIN}
      if ( not wasjoin ) and assigned( oLayer.JoinADO ) then begin
        for ii := 0 to _Recordset(oLayer.JoinADO).Fields.Count-1 do begin
          if JoinFieldInfo( _Recordset(oLayer.JoinADO).Fields[ii] ) then
            addJoinADO( ii, order ) ;
        end ;
      end ;
    {$ENDIF}
  end ;

  procedure T_ScrollBox.addInternalFields(
    const _controlAttributes : TGIS_ControlAttributes ;
    const _layer : TGIS_LayerVector ;
    var   _order : Integer
  ) ;
  var
    fld     : TGIS_FieldInfo ;
    rule    : TGIS_FieldRule ;
    vFields : TGIS_VirtualFields ;
  begin
    if not _controlAttributes.ShowVirtualFields then
      exit ;

    vFields := _controlAttributes.VirtualFields;

    if TGIS_VirtualField.GisSelected    in vFields then begin
      fld := _layer.FieldInfo( GIS_FIELD_ID_SELECTED ) ;
      rule := TGIS_FieldRule( fld.Rules ) ;
      addCheckBox( GIS_FIELD_ID_SELECTED, fld, rule, _order ) ;
    end ;
    if TGIS_VirtualField.GisHidden      in vFields then begin
      fld := _layer.FieldInfo( GIS_FIELD_ID_HIDDEN );
      rule := TGIS_FieldRule( fld.Rules ) ;
      addCheckBox( GIS_FIELD_ID_HIDDEN, fld, rule, _order ) ;
    end ;
    // read-only
    if TGIS_VirtualField.GisArea        in vFields then addPredefined( GIS_FIELD_AREA, _order ) ;
    if TGIS_VirtualField.GisLength      in vFields then addPredefined( GIS_FIELD_LENGTH, _order ) ;
    if TGIS_VirtualField.GisCoordZ      in vFields then addPredefined( GIS_FIELD_COORD_Z, _order ) ;
    if TGIS_VirtualField.GisCoordM      in vFields then addPredefined( GIS_FIELD_COORD_M, _order ) ;
    if TGIS_VirtualField.GisNow         in vFields then addPredefined( GIS_FIELD_NOW, _order ) ;
    if TGIS_VirtualField.GisMinX        in vFields then addPredefined( GIS_FIELD_MIN_X, _order ) ;
    if TGIS_VirtualField.GisMinY        in vFields then addPredefined( GIS_FIELD_MIN_Y, _order ) ;
    if TGIS_VirtualField.GisMinZ        in vFields then addPredefined( GIS_FIELD_MIN_Z, _order ) ;
    if TGIS_VirtualField.GisMinM        in vFields then addPredefined( GIS_FIELD_MIN_M, _order ) ;
    if TGIS_VirtualField.GisMaxX        in vFields then addPredefined( GIS_FIELD_MAX_X, _order ) ;
    if TGIS_VirtualField.GisMaxY        in vFields then addPredefined( GIS_FIELD_MAX_Y, _order ) ;
    if TGIS_VirtualField.GisMaxZ        in vFields then addPredefined( GIS_FIELD_MAX_Z, _order ) ;
    if TGIS_VirtualField.GisMaxM        in vFields then addPredefined( GIS_FIELD_MAX_M, _order ) ;
    if TGIS_VirtualField.GisCenterX     in vFields then addPredefined( GIS_FIELD_CENTER_X, _order ) ;
    if TGIS_VirtualField.GisCenterY     in vFields then addPredefined( GIS_FIELD_CENTER_Y, _order ) ;
    if TGIS_VirtualField.GisCenterZ     in vFields then addPredefined( GIS_FIELD_CENTER_Z, _order ) ;
    if TGIS_VirtualField.GisCenterM     in vFields then addPredefined( GIS_FIELD_CENTER_M, _order ) ;
    if TGIS_VirtualField.GixCentroidX   in vFields then addPredefined( GIS_FIELD_CENTROID_X, _order ) ;
    if TGIS_VirtualField.GixCentroidY   in vFields then addPredefined( GIS_FIELD_CENTROID_Y, _order ) ;
    if TGIS_VirtualField.GisNumPoints   in vFields then addPredefined( GIS_FIELD_NUM_POINTS, _order ) ;
    if TGIS_VirtualField.GisNumParts    in vFields then addPredefined( GIS_FIELD_NUM_PARTS, _order ) ;
    if TGIS_VirtualField.GisShapeType   in vFields then addPredefined( GIS_FIELD_SHAPE_TYPE, _order ) ;
    if TGIS_VirtualField.Lider.CG.GIS.GeoViewerScale in vFields then addPredefined( GIS_FIELD_VIEWER_SCALE, _order ) ;
    if TGIS_VirtualField.Lider.CG.GIS.GeoViewerLevel in vFields then addPredefined( GIS_FIELD_VIEWER_LEVEL, _order ) ;
  end;

  procedure T_ScrollBox.InitiateFieldsForEditing(
    _layer : TGIS_LayerVector ;
    _shape : TGIS_Shape
  ) ;
  var
    order : Integer ;
    i     : Integer ;
    fld   : TGIS_FieldInfo ;
    rule  : TGIS_FieldRule ;
  begin
    if assigned( _layer.Viewer ) then
      oViewer := _layer.Viewer.Ref
    else
      oViewer := nil ;
    oLayer  := _layer ;
    oLayerName := _layer.Name ;
    oShape  := _shape ;
    eLayout := oParent.LayoutType ;
    addHeader ;
    order := 0 ;

    addInternalFields( oParent, oLayer, order ) ;

    for i := 0 to oLayer.Fields.Count-1 do begin
      fld := oLayer.FieldInfo(i) ;
      if not (TGIS_FieldFlags.Visible in fld.Flags) then continue ;
      rule := TGIS_FieldRule(fld.Rules) ;
      if not oParent.IgnoreFldxDefinition and ( rule <> nil ) then begin
        if fld.ReadOnly then begin
          if rule.Values.Mode = TGIS_FieldValuesMode.MultiLine then
            addTextBoxStd( i, fld, rule, True, order )
          else
            addTextBoxStd( i, fld, rule, False, order ) ;
        end else begin
          if rule.Values.Mode <> TGIS_FieldValuesMode.Hidden then begin
            if rule.Values.Mode = TGIS_FieldValuesMode.MultiLine then
              addTextBox( i, fld, rule, True, order )
            else if rule.Values.Mode = TGIS_FieldValuesMode.SelectList then
              addComboBox( i, fld, rule, order )
            else if fld.FieldType = TGIS_FieldType.Boolean then
              addCheckBox( i, fld, rule, order )
            else if fld.FieldType = TGIS_FieldType.Date then
              addDateTime( i, fld, rule, order )
            else
              addTextBox( i, fld, rule, False, order ) ;
          end ;
        end ;
      end else begin
        rule := nil ;
        if fld.ReadOnly then
          addTextBoxStd( i, fld, rule, False, order )
        else begin
          if fld.FieldType = TGIS_FieldType.Boolean then
            addCheckBox( i, fld, rule, order )
          else if fld.FieldType = TGIS_FieldType.Date then
            addDateTime( i, fld, rule, order )
          else
            addTextBox( i, fld, rule, False, order ) ;
        end ;
      end ;
    end ;
  end ;

  function T_ScrollBox.addLabelStat(
    _fld_no : Integer ;
    _fld    : TGIS_FieldInfo ;
    _func   : String
  ) : TLabel ;
  var
    lbl : TLabel ;
  begin
    if eLayout = TGIS_LayoutType.TwoColumns then begin
      lbl := TLabel.Create( oPanelLeft ) ;
      lbl.Parent := oPanelLeft ;
    end else begin
      lbl := TLabel.Create( oPanelRight ) ;
      lbl.Parent := oPanelRight ;
    end ;
    lbl.AutoSize := True ;
    lbl.WordWrap := False ;
    lbl.StyledSettings := oParent.StyledSettings ;
    lbl.Name := 'lbl' + _func + IntToStr( _fld_no ) ;
    if _func = 'avg' then
      lbl.Text :=  Format( GIS_RS_ATTRIBUTES_DLG_AVG,
                           [ _fld.NewName ]
                         )
    else if _func = 'min' then
      lbl.Text :=  Format( GIS_RS_ATTRIBUTES_DLG_MIN,
                           [ _fld.NewName ]
                         )
    else if _func = 'max' then
      lbl.Text :=  Format( GIS_RS_ATTRIBUTES_DLG_MAX,
                           [ _fld.NewName ]
                         )
    else if _func = 'sum' then
      lbl.Text :=  Format( GIS_RS_ATTRIBUTES_DLG_SUM,
                           [ _fld.NewName ]
                         ) ;
    if BiDiMode = bdRightToLeft then
      lbl.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
    else
      lbl.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];

    lbl.HitTest := True ;
    lbl.OnClick     := doLabelClick     ;
    lbl.OnDblClick  := doLabelDblClick  ;
    lbl.OnMouseDown := doLabelMouseDown ;
    lbl.OnMouseMove := doLabelMouseMove ;
    lbl.OnMouseUp   := doLabelMouseUp   ;
    if BiDiMode = TBiDiMode.bdRightToLeft then
      lbl.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
    else
      lbl.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];
    Result := lbl ;
  end;

  function T_ScrollBox.setFieldNameStat(
    _index : Integer ;
    _func  : String
  ) : String ;
  begin
    Result := 'val' + _func + IntToStr( _index ) ;
  end ;

  function T_ScrollBox.addTextBoxStat(
    _fld_no    : Integer ;
    _fld       : TGIS_FieldInfo ;
    _func      : String  ;
    var _order : Integer
  ) : TEdit ;
  var
    lbl : TLabel ;
    mem : TMemo ;
    edt : TEdit ;
  begin
    lbl := addLabelStat( _fld_no, _fld, _func ) ;
    edt := T_Edit.Create( oPanelRight ) ;
    edt.Parent := oPanelRight ;
    edt.Name := setFieldNameStat( _fld_no, _func ) ;
    edt.StyledSettings := oParent.StyledSettings ;
    T_Edit(edt).AdjustType := TAdjustType.None ;
    edt.ReadOnly := True ;
    edt.Text := '';
    if BiDiMode = TBiDiMode.bdRightToLeft then
      edt.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop]
    else
      edt.Anchors := [TAnchorKind.akLeft, TAnchorKind.akTop];

    edt.OnClick := doValueClick ;
    edt.TabOrder := _order ;
    edt.KillFocusByReturn := True ;
    Result := edt ;
  end ;

  procedure T_ScrollBox.InitiateStatistics(
    _layer : TGIS_LayerVector
  ) ;
  var
    attr   : TGIS_ControlAttributes ;
    order  : Integer ;
    i      : Integer ;
    k      : Integer ;
    fldcnt : Integer ;
    fld    : TGIS_FieldInfo ;
    tp     : Array of Integer ;
    ar     : Array of Double ;
    vals   : Array of TEdit ;
    frm    : Array of String ;
    first  : Boolean ;
    shp    : TGIS_Shape ;
    oval   : Variant ;
    fld_format : String ;
  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    if assigned( _layer.Viewer ) then
      oViewer := _layer.Viewer.Ref
    else
      oViewer := nil ;
    oLayer  := _layer ;
    oLayerName := _layer.Name ;
    oShape  := nil ;

    if oLayer.SelectedList.Count = 0 then exit ;

    eLayout := attr.LayoutType ;
    addHeader ;
    order := 0 ;
    fldcnt := 0 ;
    SetLength( tp, oLayer.Fields.Count * 4 ) ;
    SetLength( vals, oLayer.Fields.Count * 4 ) ;
    for i := 0 to oLayer.Fields.Count-1 do begin
      fld := oLayer.FieldInfo(i) ;
      if not (TGIS_FieldFlags.Visible in fld.Flags) then continue ;

      case fld.FieldType of
        TGIS_FieldType.Number  ,
        TGIS_FieldType.Float   :
          begin
            // AVG
            tp[fldcnt] := 1 ;
            vals[fldcnt] := addTextBoxStat( i, fld, 'avg', order ) ;
            Inc( fldcnt ) ;
            // MIN
            tp[fldcnt] := 0 ;
            vals[fldcnt] := addTextBoxStat( i, fld, 'min', order ) ;
            Inc( fldcnt ) ;
            // MAX
            tp[fldcnt] := 0 ;
            vals[fldcnt] := addTextBoxStat( i, fld, 'max', order ) ;
            Inc( fldcnt ) ;
            // SUM
            tp[fldcnt] := 0 ;
            vals[fldcnt] := addTextBoxStat( i, fld, 'sum', order ) ;
            Inc( fldcnt ) ;
          end ;
        TGIS_FieldType.Date    :
          begin
            // AVG
            tp[fldcnt] := 1 + 2 ;
            vals[fldcnt] := addTextBoxStat( i, fld, 'avg', order ) ;
            Inc( fldcnt ) ;
            // MIN
            tp[fldcnt] := 0 + 2 ;
            vals[fldcnt] := addTextBoxStat( i, fld, 'min', order ) ;
            Inc( fldcnt ) ;
            // MAX
            tp[fldcnt] := 0 + 2 ;
            vals[fldcnt] := addTextBoxStat( i, fld, 'max', order ) ;
            Inc( fldcnt ) ;
          end ;
        TGIS_FieldType.Boolean :
          begin
            // AVG
            tp[fldcnt] := 1 ;
            vals[fldcnt] := addTextBoxStat( i, fld, 'avg', order ) ;
            Inc( fldcnt ) ;
          end ;
      end ;
    end ;
    SetLength( tp, fldcnt ) ;
    SetLength( vals, fldcnt ) ;
    SetLength( ar, fldcnt ) ;
    SetLength( frm, fldcnt ) ;

    // all selected
    first := True ;
    for k := 0 to oLayer.SelectedList.Count - 1 do begin
      shp := oLayer.GetShape( oLayer.SelectedList[k] ) ;

      assert( shp.IsSelected ) ;

      try

        fldcnt := 0 ;
        for i := 0 to oLayer.Fields.Count-1 do begin
          fld := oLayer.FieldInfo( i ) ;
          if not (TGIS_FieldFlags.Visible in fld.Flags) then continue ;

          oval := shp.GetField( fld.NewName ) ;
          if VarIsNull( oval ) then continue ;

          if assigned( fld.Rules ) then
            fld_format := TGIS_FieldRule(fld.Rules).ValueFormat
          else
            fld_format := '' ;

          case fld.FieldType of
            TGIS_FieldType.Number  ,
            TGIS_FieldType.Float   :
              begin
                // AVG
                if first then ar[ fldcnt ] := VarToDouble( oval )
                         else ar[ fldcnt ] := ar[ fldcnt ] +
                                              VarToDouble( oval ) ;
                frm[fldcnt] := fld_format ;
                Inc( fldcnt ) ;
                // MIN
                if first then ar[ fldcnt ] := VarToDouble( oval )
                         else ar[ fldcnt ] := Min( ar[ fldcnt ],
                                                   VarToDouble( oval )
                                                 ) ;
                frm[fldcnt] := fld_format ;
                Inc( fldcnt ) ;
                // MAX
                if first then ar[ fldcnt ] := VarToDouble( oval )
                         else ar[ fldcnt ] := Max( ar[ fldcnt ],
                                                   VarToDouble( oval )
                                                 ) ;
                frm[fldcnt] := fld_format ;
                Inc( fldcnt ) ;
                // SUM
                if first then ar[ fldcnt ] := VarToDouble( oval )
                         else ar[ fldcnt ] := ar[ fldcnt ] +
                                              VarToDouble( oval ) ;
                frm[fldcnt] := fld_format ;
                Inc( fldcnt ) ;
              end ;
            TGIS_FieldType.Date    :
              begin
                // AVG
                if first then ar[ fldcnt ] := Double( oval )
                         else ar[ fldcnt ] := ar[ fldcnt ] +
                                              Double( oval ) ;
                frm[fldcnt] := fld_format ;
                Inc( fldcnt ) ;
                // MIN
                if first then ar[ fldcnt ] := VarToDouble( oval )
                         else ar[ fldcnt ] := Min( ar[ fldcnt ],
                                                   Double( oval )
                                                 ) ;
                frm[fldcnt] := fld_format ;
                Inc( fldcnt ) ;
                // MAX
                if first then ar[ fldcnt ] := VarToDouble( oval )
                         else ar[ fldcnt ] := Max( ar[ fldcnt ],
                                                   Double( oval )
                                                 ) ;
                frm[fldcnt] := fld_format ;
                Inc( fldcnt ) ;
              end ;
            TGIS_FieldType.Boolean :
              begin
                // AVG
                if first then ar[ fldcnt ] := Abs( Integer( oval ) )
                         else ar[ fldcnt ] := ar[ fldcnt ] +
                                              Integer( oval ) ;

                frm[fldcnt] := fld_format ;
                Inc( fldcnt ) ;
              end ;
          end ;
        end ;
      finally
        first := False ;
      end;
    end ;

    for i := 0 to fldcnt - 1 do begin
      if ( tp[i] and $02 ) = 0 then begin
        if ( tp[i] and $01 ) <> 0 then begin
          if frm[i] = '' then
            vals[ i ].Text := FloatToStr( ar[i]/oLayer.SelectedList.Count )
          else
            vals[ i ].Text := Format( frm[i], [ar[i]/oLayer.SelectedList.Count] )
        end
        else begin
          if frm[i] = '' then
            vals[ i ].Text := FloatToStr( ar[i] )
          else
            vals[ i ].Text := Format( frm[i], [ ar[i] ] ) ;
        end
      end ;
      if ( tp[i] and $02 ) <> 0 then begin
        if ( tp[i] and $01 ) <> 0
          then vals[ i ].Text := VarAsType( ar[i]/oLayer.SelectedList.Count, varDate )
          else vals[ i ].Text := VarAsType( ar[i], varDate ) ;
      end ;
    end ;

  end ;

  procedure T_ScrollBox.ResetViewer ;
  begin
    oViewer := nil ;
  end ;

  function T_ScrollBox.UpdateLayout : Boolean ;
  var
    font   : TFont ;
    fcolor : TAlphaColor ;
    i      : Integer ;
    lbl    : TLabel ;
    cntrl  : TControl ;
    h      : Single ;
    left_column_x : Single ;
    left_column_width : Single ;
    right_column_x : Single ;
    min_width : Integer ;

    procedure set_font( _control : TControl ) ;
    var
      i : Integer ;
      cbx : TComboBox ;
    begin
      if _control is TLabel then begin
        TLabel(_control).TextSettings.Font.Family := font.Family ;
        TLabel(_control).TextSettings.Font.Style  := font.Style ;
        TLabel(_control).TextSettings.Font.Size   := font.Size ;
        TLabel(_control).TextSettings.FontColor   := fcolor ;
      end
      else if _control is TEdit then begin
        TEdit(_control).TextSettings.Font.Family := font.Family ;
        TEdit(_control).TextSettings.Font.Style  := font.Style ;
        TEdit(_control).TextSettings.Font.Size   := font.Size ;
        TEdit(_control).Height := font.Size + 10 ;
        TEdit(_control).TextSettings.FontColor   := fcolor ;
      end
      else if _control is TMemo then begin
        TMemo(_control).TextSettings.Font.Family := font.Family ;
        TMemo(_control).TextSettings.Font.Style  := font.Style ;
        TMemo(_control).TextSettings.Font.Size   := font.Size ;
        TMemo(_control).Height := 2 * (font.Size + 10) ;
        TMemo(_control).TextSettings.FontColor   := fcolor ;
      end
      else if _control is TCheckBox then begin
        TCheckBox(_control).TextSettings.Font.Family := font.Family ;
        TCheckBox(_control).TextSettings.Font.Style  := font.Style ;
        TCheckBox(_control).TextSettings.Font.Size   := font.Size ;
        TCheckBox(_control).TextSettings.FontColor   := fcolor ;
      end
      else if _control is TCheckBox then begin
        cbx := TComboBox(_control);
        for i := 0 to cbx.ListBox.Count-1 do begin
          cbx.ListBox.ListItems[i].TextSettings.Font.Family := font.Family ;
          cbx.ListBox.ListItems[i].TextSettings.Font.Style  := font.Style ;
          cbx.ListBox.ListItems[i].TextSettings.Font.Size   := font.Size ;
          cbx.ListBox.ListItems[i].TextSettings.FontColor   := fcolor ;
          cbx.ListBox.ListItems[i].Height := font.Size + 7;
        end;
        cbx.Height := font.Size + 10 ;
      end
      else if _control is TDateEdit then begin
        TDateEdit(_control).TextSettings.Font.Family := font.Family ;
        TDateEdit(_control).TextSettings.Font.Style  := font.Style ;
        TDateEdit(_control).TextSettings.Font.Size   := font.Size ;
        TDateEdit(_control).TextSettings.FontColor   := fcolor ;
        TDateEdit(_control).Height := font.Size + 10 ;
      end;
    end;
  begin
    Result := False ;

    if BiDiMode = bdRightToLeft then begin
      oPanelLeft.Align := TAlignLayout.Right;
      oPanelRight.Align := TAlignLayout.Client;
    end
    else
    begin
      oPanelLeft.Align := TAlignLayout.Left;
      oPanelRight.Align := TAlignLayout.Client;
    end;

    font := oParent.Font ;
    fcolor := oParent.FontColor ;

    if ( assigned( oShape ) or assigned( oLayer ) ) and
       ( ( ( eLayout = TGIS_LayoutType.OneColumn ) and
           ( oPanelRight.ComponentCount > 0 ) ) or
         ( ( eLayout = TGIS_LayoutType.TwoColumns ) and
           ( oPanelLeft.ComponentCount > 0 ) and
           ( oPanelRight.ComponentCount > 0 ) ) ) then begin
      if eLayout = TGIS_LayoutType.OneColumn then begin
        cntrl := oPanelRight.Controls[0] ;
        TLabel(cntrl).TextSettings.Font.Family := font.Family ;
        TLabel(cntrl).TextSettings.Font.Size   := font.Size ;
        TLabel(cntrl).TextSettings.FontColor   := fcolor ;
        PlaceControl(BiDiMode, nil, cntrl, GGAP, cntrl.Width);
      end else begin
        cntrl := oPanelLeft.Controls[0] ;
        TLabel(cntrl).TextSettings.Font.Family := font.Family ;
        TLabel(cntrl).TextSettings.Font.Size   := font.Size ;
        TLabel(cntrl).TextSettings.FontColor   := fcolor ;
        PlaceControl(BiDiMode, nil, cntrl, GGAP, cntrl.Width);

        cntrl := oPanelRight.Controls[0] ;
        TLabel(cntrl).TextSettings.Font.Family := font.Family ;
        TLabel(cntrl).TextSettings.Font.Size   := font.Size ;
        TLabel(cntrl).TextSettings.FontColor   := fcolor ;
        PlaceControl(BiDiMode, nil, cntrl, GGAP, cntrl.Width);
      end ;
      h := cntrl.Position.Y + cntrl.Height + 2*VGAP ;

      Result := True ;
      // locate controls on the panel
      min_width := 30 ;
      left_column_x := GGAP ;
      left_column_width := oParent.FieldNameColumnWidth ;
      if eLayout = TGIS_LayoutType.OneColumn then
        right_column_x := left_column_x + min_width
      else
        right_column_x := left_column_x + left_column_width + GGAP ;

      if eLayout = TGIS_LayoutType.OneColumn then begin
        for i := 2 to oPanelRight.Controls.Count - 1 do begin
          cntrl := oPanelRight.Controls[i] ;
          set_font( cntrl ) ;
          if not ( cntrl is TLabel )  then
            continue ;
          cntrl.Position.Y := h ;
          PlaceControl(BiDiMode, nil, cntrl, GGAP, -1);
          h := h + cntrl.Height ;
          cntrl := oPanelRight.Controls[i+1] ;
          cntrl.Position.Y := h ;
          h := h + cntrl.Height + VGAP ;
        end ;
        if oParent.ReadOnly then
          h := h + GGAP - VGAP ;
        oPanel.Height := h ;
        oPanel.Width := Max( Width, right_column_x + min_width + GGAP ) ;
        oPanelLeft.Width := GGAP + oParent.FieldNameColumnWidth +
                            GGAP - oSplitter.Width ;
        oPanelRight.Width := oPanel.Width - oPanelLeft.Width - oSplitter.Width ;
      end else begin
        for i := 1 to oPanelLeft.Controls.Count - 1 do begin
          lbl   := TLabel(oPanelLeft.Controls[i]) ;
          set_font( lbl ) ;
          cntrl := oPanelRight.Controls[i] ;
          set_font( cntrl ) ;
          PlaceControl(BiDiMode, nil, lbl, GGAP, lbl.Width);
          // field name
          if cntrl.Height > lbl.Height then
            lbl.Position.Y := h + (cntrl.Height - lbl.Height) / 2
          else
            lbl.Position.Y := h ;
          // field value
          PlaceControl(BiDiMode, nil, cntrl, GGAP, oPanelRight.Width - GGAP - cntrl.Position.X);
          cntrl.Position.Y := h ;
          h := h + cntrl.Height ;
        end ;
        if oParent.ReadOnly then
          h := h + GGAP ;
        oPanel.Height := h ;
        oPanel.Width := Max( Width, right_column_x + min_width + GGAP ) ;
        oPanelLeft.Width := right_column_x - oSplitter.Width ;
      end ;
      if eLayout = TGIS_LayoutType.OneColumn then begin
        oPanelLeft.Visible := False ;
        oSplitter.Visible := False ;
      end else begin
        oPanelLeft.Visible := True ;
        oSplitter.Visible := True ;
      end ;

      if BiDiMode = bdRightToLeft then
      begin
        oSplitter.Position.X := oPanel.Width - oPanelLeft.Width ;
        oSplitter.Align := TAlignLayout.Right
      end
      else
      begin
        oSplitter.Position.X := oPanelLeft.Width;
        oSplitter.Align := TAlignLayout.Left;
      end;
    end ;
  end ;

  procedure T_ScrollBox.doLabelClick(
    _sender : TObject
  ) ;
  begin
    if assigned( oParent.OnClick ) then
      oParent.OnClick( Self ) ;
  end ;

  procedure T_ScrollBox.doLabelDblClick(
    _sender : TObject
  ) ;
  begin
    if assigned( oParent.OnDblClick ) then
      oParent.OnDblClick( Self ) ;
  end ;

  procedure T_ScrollBox.doLabelMouseDown(
    _sender : TObject ;
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x, _y  : Single
  ) ;
  var
    fld_name : String ;
    new_item : TMenuItem ;
    pt : TPointF ;
    x, y : Single ;

    function get_fld_name : String ;
    var
      i : Integer ;
      y : Single ;
      lbl  : TLabel ;
      cntrl : TControl ;
    begin
      if _sender is TLabel then begin
        Result := TLabel(_sender).Text ;
        if eLayout = TGIS_LayoutType.OneColumn then begin
          if BiDiMode = TBiDiMode.bdRightToLeft then begin
            if Result.Contains( ' : ' + _rsrc(GIS_RS_ATTRIBUTES_DLG_UID) ) then
              Result := _rsrc(GIS_RS_ATTRIBUTES_DLG_UID) ;
          end else begin
            if Result.Contains( _rsrc(GIS_RS_ATTRIBUTES_DLG_UID) + ' : ' ) then
              Result := _rsrc(GIS_RS_ATTRIBUTES_DLG_UID) ;
          end;
        end
      end else begin
        y := oPanelRight.Controls[1].Position.Y ;
        if _y < y then
          Result := _rsrc(GIS_RS_ATTRIBUTES_DLG_UID)
        else
          for i := 1 to oPanelLeft.Controls.Count - 1 do begin
            lbl := TLabel(oPanelLeft.Controls[i]) ;
            cntrl := oPanelRight.Controls[i] ;
            if ( y + cntrl.Height ) >= _y then begin
              Result := lbl.Text ;
              break ;
            end;
            y := y + cntrl.Height ;
          end;
      end;
    end ;

  begin
    if assigned( oParent.OnMouseDown ) then
      oParent.OnMouseDown( Self, _button, _shift, _x, _y ) ;

    oField := '' ;

    if _button = TMouseButton.mbRight then
    begin

      fld_name := get_fld_name ;
      x := TControl(_sender).Position.X ;
      y := TControl(_sender).Position.Y ;
      pt := oParent.LocalToScreen( PointF( _x + x - ViewportPosition.X,
                                           _y + y - ViewportPosition.Y ) );

      if TGIS_ControlAttributes(Parent).AllowRestructure and
         not TGIS_ControlAttributes(Parent).ReadOnly     and
         assigned( oShape )                              and
         ( String.Compare( fld_name, _rsrc(GIS_RS_ATTRIBUTES_DLG_UID) ) <> 0 ) then begin

        oField := fld_name ;

        if isPredefined( fld_name ) then begin
          TMenuItem(oPopup1.Items[iMnuModify]).Enabled := False ;
          TMenuItem(oPopup1.Items[iMnuDelete]).Enabled := False ;
        end else begin
          TMenuItem(oPopup1.Items[iMnuModify]).Enabled := True ;
          TMenuItem(oPopup1.Items[iMnuDelete]).Enabled := True ;
        end ;

        oPopup1.Popup( pt.X, pt.Y ) ;
      end else
        oPopup2.Popup( pt.X, pt.Y ) ;
    end ;
  end ;

  procedure T_ScrollBox.doLabelMouseUp(
    _sender : TObject ;
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x, _y  : Single
  ) ;
  begin
    if assigned( oParent.OnMouseUp ) then
      oParent.OnMouseUp( Self, _button, _shift, _x, _y ) ;
  end ;

  procedure T_ScrollBox.doLabelMouseMove(
    _sender : TObject ;
    _shift  : TShiftState ;
    _x, _y  : Single
  ) ;
  begin
    if assigned( oParent.OnMouseMove ) then
      oParent.OnMouseMove( Self, _shift, _x, _y ) ;
  end ;

  procedure T_ScrollBox.doValueClick(
    _sender : TObject
  ) ;
  begin
    if assigned( oParent.OnClick ) then
      oParent.OnClick( Self ) ;
  end;

  procedure T_ScrollBox.doValueKeyDown(
        _sender  : TObject ;
    var _key     : Word    ;
    var _keyChar : WideChar ;
        _shift   : TShiftState
  ) ;
  begin
    if assigned( oParent.OnKeyDown ) then
      oParent.OnKeyDown( Self, &_key, &_keyChar, _shift ) ;
  end ;

  procedure T_ScrollBox.doValueKeyUp(
        _sender  : TObject ;
    var _key     : Word    ;
    var _keyChar : WideChar ;
        _shift   : TShiftState
  ) ;
  begin
    if assigned( oParent.OnKeyUp ) then
      oParent.OnKeyUp( Self, &_key, &_keyChar, _shift ) ;
  end ;

  procedure T_ScrollBox.doSplitterMoved(
    _sender : TObject
  ) ;
  begin
    oParent.FieldNameColumnWidth :=
      oPanelLeft.Width - GGAP - (GGAP-oSplitter.Width) ;
  end ;

  procedure T_ScrollBox.doPanelRightResize(
    _sender : TObject
  ) ;
  var
    cntrl : TControl ;
    i     : Integer ;
  begin
    for i := 1 to oPanelRight.Controls.Count - 1 do begin
      cntrl := oPanelRight.Controls[i] ;
      cntrl.Width := oPanelRight.Width - GGAP ;
    end ;
    oParent.FieldNameColumnWidth :=
      oPanelLeft.Width - GGAP - (GGAP-oSplitter.Width) ;
  end ;

  procedure T_ScrollBox.doAddField(
    _sender : TObject
  ) ;
  var
    frm : TGIS_ControlAttributesForm ;
  begin
    if not Assigned( oShape ) then exit ;
    if not TGIS_ControlAttributes( Parent ).AllowRestructure then exit ;

    frm := TGIS_ControlAttributesForm.Create( Self ) ;
    frm.ShowAdd( oLayer, pOnHelp,
                 procedure( _modal_result : TModalResult )
                 begin
                   if _modal_result <> mrOK then
                     exit ;

                   oLayer.AddField( frm.XField.NewName , frm.XField.FieldType,
                                    frm.XField.NewWidth, frm.XField.NewDecimal
                                  ) ;
                   TGIS_ControlAttributes( Parent ).Invalidate ;
                 end
    ) ;
  end ;

  procedure T_ScrollBox.doModifyField(
    _sender : TObject
  ) ;
  var
    frm : TGIS_ControlAttributesForm ;
  begin
    if not Assigned( oShape ) then exit ;
    if not TGIS_ControlAttributes( Parent ).AllowRestructure then exit ;

    frm := TGIS_ControlAttributesForm.Create( Self ) ;
    frm.ShowModify( oLayer, oField, pOnHelp,
                    procedure( _modal_result : TModalResult )
                    begin
                      if _modal_result <> mrOK then
                        exit ;

                      oLayer.RenameField( oField,
                                          frm.XField.NewName ,
                                          frm.XField.NewWidth,
                                          frm.XField.NewDecimal
                                        ) ;
                      TGIS_ControlAttributes( Parent ).Invalidate ;
                    end
    ) ;
  end ;

  procedure T_ScrollBox.doDeleteField(
    _sender : TObject
  ) ;
  var
    frm : TGIS_ControlAttributesForm ;
  begin
    if not Assigned( oShape ) then exit ;
    if not TGIS_ControlAttributes( Parent ).AllowRestructure then exit ;

    frm := TGIS_ControlAttributesForm.Create( Self ) ;
    frm.ShowDelete( oLayer, oField, pOnHelp,
                    procedure( _modal_result : TModalResult )
                    begin
                      if _modal_result <> mrOK then
                        exit ;

                      oLayer.DeleteField( oField );
                      TGIS_ControlAttributes( Parent ).Invalidate ;
                    end
    ) ;
  end ;

  procedure T_ScrollBox.doCopyToClipboard(
    _sender : TObject
  ) ;
  begin
    CopyToClipboard ;
  end ;

  procedure T_ScrollBox.CancelChanges ;
  var
    i          : Integer ;
    cntrl      : TControl ;
    v          : Variant ;
    fld_name   : String ;
    fld_format : String ;
    fld_id     : Integer ;
    fld        : TGIS_FieldInfo ;
    attr       : TGIS_ControlAttributes ;
    wasjoin    : Boolean ;
    cntrl_name : String ;

    procedure set_text(
      _caption : String
    ) ;
    var
      i   : Integer ;
      cbx : TComboBox ;
    begin
      if cntrl is TCheckBox then
        TCheckBox(cntrl).Text := _caption
      else if cntrl is TEdit then
        TEdit(cntrl).Text := _caption
      else if cntrl is TMemo then
        TMemo(cntrl).Text := _caption
      else if cntrl is TComboBox then begin
        cbx := TComboBox( cntrl ) ;
        for i := 0 to cbx.Items.Count-1 do
          if cbx.Items[i] = _caption then begin
            cbx.ItemIndex := i ;
            break ;
          end ;
      end
      else if cntrl is TDateEdit then begin
        if not IsStringEmpty( _caption ) then
          TDateEdit(cntrl).DateTime := StrToDateTime( _caption )
        else begin
          TDateEdit(cntrl).DateTime := GisDefaultField( TGIS_FieldType.Date ) ;
          TDateEdit(cntrl).Format := ' ';
        end;
      end
      else
        Assert( False ) ;
    end ;

    procedure set_value( const _v : Variant ) ;
    var
      i   : Integer ;
      cbx : TComboBox ;
      str : String ;
    begin
      if cntrl is TDateEdit then begin
        TDateEdit(cntrl).Format   := T_DateEdit(cntrl).oldFormat ;
        TDateEdit(cntrl).DateTime := _v
      end
      else if cntrl is TEdit then begin
        TEdit(cntrl).Text := DateTimeToStr( VarToDateTime( v ) ) ;
      end
      else if cntrl is TComboBox then begin
        cbx := TComboBox( cntrl ) ;
        str := DateTimeToStr( VarToDateTime( v ) ) ;
        for i := 0 to cbx.Items.Count-1 do
          if cbx.Items[i] = str then begin
            cbx.ItemIndex := i ;
            break ;
          end ;
      end
    end;

  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    // filling cells
    for i := 0 to oPanelRight.Controls.Count - 1 do begin
      cntrl := oPanelRight.Controls[i] ;
      if cntrl is TLabel then
        continue ;

      cntrl_name := cntrl.Name;
      if isPredefined( cntrl_name ) then begin
        v := oShape.GetField( cntrl_name ) ;

        if ( cntrl_name = GIS_FIELD_AREA ) and ( oShape.Layer.CS.EPSG <> 0 ) and ( VarToDouble( v ) <> 0 ) then
          v := attr.Units.AsAreal( VarToDouble( v ), True, '%s2' )
        else if ( cntrl_name = GIS_FIELD_LENGTH ) and ( oShape.Layer.CS.EPSG <> 0 ) and ( VarToDouble( v ) <> 0 ) then
          v := attr.Units.AsLinear( VarToDouble( v ), True ) ;

        if VarIsNull( v ) then
          set_text( '-' )
        else
          set_text( VarToString( v ) ) ;
      end
      else begin
        fld_name   := getFieldName( cntrl_name ) ;
        fld_format := getFieldFormat( cntrl_name ) ;
        fld_id     := getFieldId( cntrl_name, wasjoin ) ;

        v := oShape.GetFieldEx( fld_name, True ) ;
        if v = Unassigned then
          v := oShape.GetField( fld_name ) ;
        if cntrl is TCheckBox then begin
          if VarIsNull( v ) then
            TCheckBox(cntrl).IsChecked := false
          else
            TCheckBox(cntrl).IsChecked := Boolean(v) ;
        end
        else begin
          if VarIsNull( v ) then
            set_text( '' )
          else begin
            if not wasjoin then begin
              fld := oLayer.FieldInfo(fld_id) ;
              if (attr.ReadOnly or fld.ReadOnly) and (fld_format <> '') then begin
                if fld.FieldType  = TGIS_FieldType.String then
                  set_text( Format( fld_format, [VarToString( v )] ) )
                else if (fld.FieldType  = TGIS_FieldType.Number) or
                        (fld.FieldType  = TGIS_FieldType.Float ) then begin
                  if fld.Decimal = 0 then
                    set_text( Format( fld_format, [VarToInt64( v )] ) )
                  else
                    set_text( Format( fld_format, [DotStrToFloat( VarToString(v) ) ] ) )
                end
                else if fld.FieldType  = TGIS_FieldType.Date then
                  set_text( FormatDateTime( fld_format, VarToDateTime( v ) ) )
                else
                  set_text( Format( fld_format, [VarToString( v )] ) ) ;
              end
              else begin
                if fld.FieldType  = TGIS_FieldType.Date then
                  set_value( v )
              else
                set_text( VarToString( v ) )
              end
            end
            else
              set_text( VarToString( v ) )
          end
        end ;
        if cntrl is T_Edit then
          T_Edit(cntrl).SetColor( clValid )
        else if cntrl is T_Memo then
          T_Memo(cntrl).SetColor( clValid ) ;
      end ;
    end ;
  end ;

  procedure T_ScrollBox.UpdateLayer ;
  var
    i : Integer ;
    cntrl : TControl ;
    fld : TGIS_FieldInfo ;
    wasjoin : Boolean ;
  begin
    // update the layer
    if oShape.Uid <> -1 then
      oShape := oShape.MakeEditable ;
    for i := 0 to oPanelRight.Controls.Count - 1 do begin

      cntrl := oPanelRight.Controls[i] ;
      if cntrl is TLabel then
        continue ;
      if isPredefined( cntrl.Name ) then
        continue ;
      fld := oLayer.FieldInfo( getFieldId( cntrl.Name, wasjoin ) ) ;
      if fld.ReadOnly then continue ;
      if cntrl is TEdit then begin
        if ( ( fld.FieldType = TGIS_FieldType.Number ) or
             ( fld.FieldType = TGIS_FieldType.Float  ) ) and
           IsStringEmpty( TEdit(cntrl).Text ) then
          oShape.SetField( fld.NewName, Null )
        else
          oShape.SetField( fld.NewName, TEdit(cntrl).Text )
      end
      else if cntrl is TMemo then
        oShape.SetField( fld.NewName, TMemo(cntrl).Text )
      else if cntrl is TComboBox then
        oShape.SetField( fld.NewName, TComboBox(cntrl).Selected.Text )
      else if cntrl is TCheckBox then
        oShape.SetField( fld.NewName, TCheckBox(cntrl).IsChecked )
      else if cntrl is TDateEdit then begin
        if TDateEdit(cntrl).DateTime <> GisDefaultField( TGIS_FieldType.Date ) then
          oShape.SetField( fld.NewName, TDateEdit(cntrl).DateTime ) ;
      end;
    end ;
    if oShape.Uid <> -1 then
      oShape.Invalidate ;
  end ;

  procedure T_ScrollBox.CopyToClipboard ;
  const
    TAB = #9 ;
    CR  = #13;
  var
    i  : Integer ;
    sb : TStringBuilder ;
    cntrl : TControl ;
    clp : IFMXClipboardService;

    function try_get_clipboard_service(
      out _clp : IFMXClipboardService
    ) : Boolean;
    begin
      Result := TPlatformServices.Current.SupportsPlatformService(
                  IFMXClipboardService );
      if Result then
        _clp := IFMXClipboardService(
                  TPlatformServices.Current.GetPlatformService(
                    IFMXClipboardService ) ) ;
    end ;

  begin
    if oPanelLeft.ControlsCount > 1 then
    begin
      sb := TStringBuilder.Create ;
      try
        for i := 1 to oPanelLeft.ControlsCount - 1 do
        begin
          sb.Append( TLabel(oPanelLeft.Controls[i]).Text ) ;
          sb.Append( TAB ) ;
          cntrl := oPanelRight.Controls[i] ;
          if cntrl is TEdit then
            sb.Append( TEdit(cntrl).Text )
          else if cntrl is TMemo then
            sb.Append( TMemo(cntrl).Text )
          else if cntrl is TComboBox then
            sb.Append( TComboBox(cntrl).Selected.Text )
          else if cntrl is TCheckBox then
          begin
            if TCheckBox(cntrl).IsChecked then
              sb.Append( '1' )
            else
              sb.Append( '0' ) ;
          end;
          sb.Append( CR ) ;
        end ;
        if try_get_clipboard_service(clp) then
          clp.SetClipboard(sb.ToString);
      finally
        FreeObject( sb ) ;
      end;
    end ;
  end ;

  procedure T_ScrollBox.fset_BiDiMode(
    const _value : TBiDiMode
  );
  begin
    if _value <> FBiDiMode then
      FBiDiMode := _value ;
  end;
//==============================================================================
// TGIS_ControlAttributes
//==============================================================================

  procedure TGIS_ControlAttributes.initiateFldList(
    _layer : TGIS_LayerVector ;
    _shape : TGIS_Shape
  ) ;
  begin

    if FBiDiModeFromTranslation then
    begin
      if _rsbidi then
      begin
        if BiDiMode <> TBiDiMode.bdRightToLeft then
          BiDiMode := TBiDiMode.bdRightToLeft
      end
      else
      begin
        if BiDiMode <> TBiDiMode.bdLeftToRight then
          BiDiMode := TBiDiMode.bdLeftToRight
      end;

    end;

    T_Scrollbox(oGrid).BiDiMode := Self.BiDiMode ;

    if assigned( T_ScrollBox(oGrid).Viewer ) then
      T_ScrollBox(oGrid).Viewer.UnSubscribe( Self ) ;

    T_ScrollBox(oGrid).Visible := False ;
    T_ScrollBox(oGrid).ClearFields ;
    if FReadOnly then
      T_ScrollBox(oGrid).InitiateFieldsForViewing(
        _layer, _shape
      )
    else
      T_ScrollBox(oGrid).InitiateFieldsForEditing(
        _layer, _shape
      ) ;

    if assigned( T_ScrollBox(oGrid).Viewer ) then
      T_ScrollBox(oGrid).Viewer.Subscribe( Self ) ;
  end ;

  procedure TGIS_ControlAttributes.initiateStatList(
    _layer : TGIS_LayerVector
  ) ;
  begin
    if assigned( T_ScrollBox(oGrid).Viewer ) then
      T_ScrollBox(oGrid).Viewer.UnSubscribe( Self ) ;

    T_ScrollBox(oGrid).Visible := False ;
    T_ScrollBox(oGrid).ClearFields ;
    T_ScrollBox(oGrid).InitiateStatistics(
      _layer
    ) ;

    if assigned( T_ScrollBox(oGrid).Viewer ) then
      T_ScrollBox(oGrid).Viewer.Subscribe( Self ) ;
  end ;

  procedure TGIS_ControlAttributes.fset_FieldNameColumnWidth(
    const _value : Single
  ) ;
  begin
    if _value <> FFieldNameColumnWidth then
      FFieldNameColumnWidth := _value ;
  end ;

  procedure TGIS_ControlAttributes.fset_LayoutType(
    const _value : TGIS_LayoutType
  ) ;
  begin
    if _value <> FLayoutType then
      FLayoutType := _value ;
  end ;

  procedure TGIS_ControlAttributes.fset_BiDiMode(
    const _value : TBiDiMode
  );
  begin
    if _value <> FBiDiMode then
      FBiDiMode := _value ;
  end;

  procedure TGIS_ControlAttributes.fset_BiDiModeFromTranslation(
    const _value : Boolean
  );
  begin
    if _value <> FBiDiModeFromTranslation then
      FBiDiModeFromTranslation := _value ;
  end;

  procedure TGIS_ControlAttributes.fset_ReadOnly(
    const _value : Boolean
  ) ;
  begin
    if _value <> FReadOnly then
      FReadOnly := _value ;
  end ;

  procedure TGIS_ControlAttributes.fset_ShowBtnCancel(
    const _value : Boolean
  ) ;
  begin
    if _value <> FShowBtnCancel then begin
      FShowBtnCancel := _value ;
      Repaint ;
    end ;
  end ;

  procedure TGIS_ControlAttributes.fset_ShowBtnOk(
    const _value : Boolean
  ) ;
  begin
    if _value <> FShowBtnOk then begin
      FShowBtnOk := _value ;
      Repaint ;
    end ;
  end ;

  procedure TGIS_ControlAttributes.fset_Units(
    const _value : TGIS_CSUnits
  ) ;
  begin
    if Assigned( _value ) then
      FUnits := _value
    else
      FUnits := CSUnitsList.ByEPSG( 9001 ) ;
    Repaint ;
  end ;

  function TGIS_ControlAttributes.fget_UnitsEPSG
    : Integer ;
  begin
    Result := FUnits.EPSG ;
  end ;

  procedure TGIS_ControlAttributes.fset_UnitsEPSG(
    const _value : Integer
  ) ;
  var
    unt : TGIS_CSUnits ;
  begin
    unt := CSUnitsList.ByEPSG( _value ) ;

    if Assigned( unt ) then
      FUnits := unt
    else
      FUnits := CSUnitsList.ByEPSG( 9001 ) ;

    Repaint ;
  end ;

  procedure TGIS_ControlAttributes.fset_Font(
    const _value : TFont
  ) ;
  begin
    if assigned( _value ) then
      FFont.Assign( _value ) ;
  end;

  procedure TGIS_ControlAttributes.doCancel(
    _sender : TObject
  ) ;
  begin
    CancelChanges ;
  end ;

  procedure TGIS_ControlAttributes.doHelp(
    _sender : TObject
  ) ;
  begin
    if Assigned( pOnHelp ) then HelpEvent( _sender, Name ) ;
  end ;

  procedure TGIS_ControlAttributes.doKeyDown(
    _sender      : TObject  ;
    var _key     : Word     ;
    var _keyChar : WideChar ;
        _shift   : TShiftState
  ) ;
  begin
    if      _key = vkEscape then doCancel( _sender )
    else if _key = vkReturn then doOk    ( _sender )
    else if _key = vkF1     then doHelp  ( _sender ) ;
  end ;

  procedure TGIS_ControlAttributes.doOk(
    _sender : TObject
  ) ;
  begin
    SaveChanges ;
  end ;

  procedure TGIS_ControlAttributes.doResize(
    _sender : TObject
  ) ;
  begin
    updateLayout ;
    Repaint ;
  end ;

  procedure TGIS_ControlAttributes.updateLayout ;
  var
    v  : Boolean ;
    ww : Single ;
    hh : Single ;
    any_button : Boolean ;
    any_shape  : Boolean ;
    w  : Single ;
    sb : T_ScrollBox ;
  begin

    if csDesigning in ComponentState then
    begin
      T_ScrollBox(oGrid).Visible := False ;
      oErrorMessage.Visible := False ;
      oBtnOk.Visible := False ;
      oBtnCancel.Visible := False ;
      exit ;
    end ;

    sb := T_ScrollBox(oGrid) ;
    any_button := FShowBtnOk or FShowBtnCancel ;

    if any_button then begin

      ww := Max( oBtnOk.Width, oBtnCancel.Width ) ;
      hh := Max( oBtnOk.Height, oBtnCancel.Height ) ;

      if FShowBtnOk then begin
        oBtnOk.Width := ww ;
        oBtnOk.Height := hh ;
      end else begin
        oBtnOk.Width := 0 ;
        oBtnOk.Height := 0 ;
      end ;
      if FShowBtnCancel then begin
        oBtnCancel.Width := ww ;
        oBtnCancel.Height := hh ;
      end else begin
        oBtnCancel.Width := 0 ;
        oBtnCancel.Height := 0 ;
      end ;

      w := oBtnCancel.Width;
      if FShowBtnOk and FShowBtnCancel then
        w := w + HGAP ;
      w := w + oBtnOk.Width ;

      if FShowBtnOk then
        PlaceControl( BiDiMode, nil, oBtnOk, ( Width - w ) / 2, oBtnOk.Width ) ;
      if FShowBtnCancel then
        PlaceControl( BiDiMode, nil, oBtnCancel,
                      ( Width - w ) / 2 + w - oBtnCancel.Width,
                      oBtnCancel.Width ) ;

    end else begin
      hh := 0 ;
      oBtnOk.Width := 0 ;
      oBtnOk.Height := 0 ;
      oBtnCancel.Width := 0 ;
      oBtnCancel.Height := 0 ;
    end ;

    PlaceControl( BiDiMode, nil, oErrorMessage, HGAP div 2, -1 ) ;

    any_shape := False ;
    v := sb.Visible ;
    try
      sb.Visible := False ;
      sb.Position.X := 0 ;
      sb.Position.Y := 0 ;
      // cannot set Height
      sb.Width := Width ;
      if FReadOnly then
        sb.Height := Height
      else if any_button then
        sb.Height := Height - VGAP - hh - VGAP -
                                     oErrorMessage.Height - 2*VGAP
      else
        sb.Height := Height - VGAP - oErrorMessage.Height - 2*VGAP ;
      any_shape := sb.UpdateLayout ;
    finally
      if any_shape then
        sb.Visible := True
      else
        sb.Visible := v ;
    end ;

    oErrorMessage.Visible := not FReadOnly ;
    oBtnCancel.Visible    := FShowBtnCancel and not FReadOnly ;
    oBtnOk.Visible        := FShowBtnOk and not FReadOnly ;

    oBtnCancel.Position.Y := Height - hh - VGAP ;
    oBtnOk.Position.Y     := oBtnCancel.Position.Y ;
    if any_button then
      oErrorMessage.Position.Y := oBtnOk.Position.Y - VGAP - oErrorMessage.Height
    else
      oErrorMessage.Position.Y := Height - VGAP - oErrorMessage.Height ;
  end ;

  procedure TGIS_ControlAttributes.unsubscribeFromViewer ;
  begin
    if assigned( T_ScrollBox(oGrid).Viewer ) then
      T_ScrollBox(oGrid).Viewer.UnSubscribe( Self ) ;
  end ;

  procedure TGIS_ControlAttributes.Paint ;
  begin
    if not bCreated then exit ; // ActiveX bug

    inherited ;

  end ;

  constructor TGIS_ControlAttributes.Create( _owner : TComponent ) ;
  begin
    BiDiModeFromTranslation := false;
    bCreated := False ;
    bCancel  := False ;

    inherited Create( _owner ) ;

    FFont := TFont.Create ;
    FFontColor := TAlphaColorRec.Black ;
    FStyledSettings := DefaultStyledSettings ;

    if GisIsMetricSystem then
      FUnits  := CSUnitsList.ByEPSG( 904201 )
    else
      FUnits  := CSUnitsList.ByEPSG( 904202 ) ;

    FLayoutType := TGIS_LayoutType.TwoColumns ;
    FFieldNameColumnWidth := 80 ;

    Width  := 150 ;
    Height := 100 ;
    Sides  := []  ;
    HitTest := False ;
    OnResize := doResize ;

    oGrid := T_ScrollBox.Create( Self ) ;
    if not ( csDesigning in ComponentState ) then begin
      T_ScrollBox(oGrid).Parent    := Self ;
      T_ScrollBox(oGrid).SetAttributesControl( Self ) ;
      OnKeyDown := doKeyDown ;
    end ;

    oErrorMessage := TLabel.Create( Self ) ;
    with oErrorMessage do begin
      Parent    := Self  ;
      Visible   := False ;
      Stored    := False ;
      Text      := ''    ;
      StyledSettings := StyledSettings - [TStyledSetting.FontColor] ;
      TextSettings.FontColor := TAlphaColorRec.Red ;
      Visible   := False ;
    end ;

    {$IFNDEF ACTIVEX}
      oBtnOk := TButton.Create( Self );
    {$ELSE}
      oBtnOk := TButton.CreateParented( Self.Handle );
    {$ENDIF}
    with oBtnOk do begin
      Parent  := Self  ;
      OnClick := doOk  ;
      Enabled := False ;
      Stored  := False ;
      Visible := False ;
      Text    := _rsrc( GIS_RS_BTN_OK ) ;
    end ;
    FShowBtnOk := True ;

    {$IFNDEF ACTIVEX}
      oBtnCancel := TButton.Create( Self );
    {$ELSE}
      oBtnCancel := TButton.CreateParented( Self.Handle );
    {$ENDIF}
    with oBtnCancel do begin
      Parent  := Self     ;
      OnClick := doCancel ;
      Enabled := False    ;
      Stored  := False ;
      Visible := False    ;
      Text    := _rsrc( GIS_RS_BTN_CANCEL ) ;
    end ;
    FShowBtnCancel := True     ;

    FReadOnly         := False ;
    FAllowNull        := False ;
    FAllowRestructure := False ;

    FShowVirtualFields := True ;
    FVirtualFields := [ TGIS_VirtualField.GisSelected,
                        TGIS_VirtualField.GisHidden,
                        TGIS_VirtualField.GisArea,
                        TGIS_VirtualField.GisLength ] ;

    pOnHelp := nil ;

    bCreated := True ;
    uponDestroy := False ;
  end ;

  destructor TGIS_ControlAttributes.Destroy ;
  begin
    {$IFDEF GIS_PDK}
      RemoveFreeNotifications ;
    {$ENDIF}

    unsubscribeFromViewer ;
    FreeObject( FFont) ;
    inherited ;
  end;

  procedure TGIS_ControlAttributes.DoControlEnter(
    _sender : TObject
  ) ;
  var
    msg   : String  ;
    cntrl : TControl ;
  begin
    if bCancel = True then exit ;

    if ( _sender is TEdit ) or
       ( _sender is TMemo ) then begin
      cntrl := TControl(_sender) ;
      if not T_ScrollBox(oGrid).ValidateTextBox(
        cntrl, TGIS_FieldValueCheckMode.OnEdit, msg ) then begin
        oErrorMessage.Text := msg ;
        exit ;
      end ;
    end ;
    oErrorMessage.Text := '' ;
  end ;

  procedure TGIS_ControlAttributes.DoValueChanged(
    _sender : TObject
  ) ;
  var
    msg   : String  ;
    cntrl : TControl ;
  begin
    if bCancel = True then exit ;

    oBtnCancel.Enabled := True ;

    if ( _sender is TEdit ) or
       ( _sender is TMemo )  or
       ( _sender is TCheckBox ) then begin
      cntrl := TControl(_sender) ;
      if not T_ScrollBox(oGrid).ValidateTextBox(
        cntrl, TGIS_FieldValueCheckMode.OnEdit, msg ) then begin
        oErrorMessage.Text := msg ;
        oBtnOk.Enabled := False ;
        exit ;
      end ;
    end ;

    if (_sender is TCheckBox) then
      T_ScrollBox(oGrid).doValueClick( _sender ) ;

    oErrorMessage.Text := '' ;
    //check all fields
    if not T_ScrollBox(oGrid).ValidateFields( msg ) then
      oBtnOk.Enabled := False
    else
      oBtnOk.Enabled := True ;

    if assigned( FFieldChangeEvent ) then
      FFieldChangeEvent( _sender ) ;
  end ;

  procedure TGIS_ControlAttributes.Invalidate ;
  begin
    if not assigned( T_ScrollBox(oGrid).Layer  ) then exit ;
    if assigned( T_ScrollBox(oGrid).Shape ) then
      ShowShape( T_ScrollBox(oGrid).Shape )
    else
      ShowSelected( T_ScrollBox(oGrid).Layer ) ;
  end ;

  procedure TGIS_ControlAttributes.Clear ;
  begin
    T_ScrollBox(oGrid).Visible := False ;
    T_ScrollBox(oGrid).ClearFields ;
    oErrorMessage.Visible      := False ;
    oBtnOk.Enabled             := False ;
    oBtnCancel.Enabled         := False ;
    doResize( nil ) ;
  end ;

  procedure TGIS_ControlAttributes.NewShape(
    const _shape : TGIS_Shape
  ) ;
  begin
    if _shape = nil then
      Clear
    else begin
      if not assigned( _shape.Layer ) then exit ;

      initiateFldList( _shape.Layer, _shape ) ;

      _shape.SetFieldsDefaulRuleValue ;
      CancelChanges ;

      oErrorMessage.Visible := True ;

      doResize( nil ) ;
    end ;
  end ;

  procedure TGIS_ControlAttributes.ShowShape(
    const _shape : TGIS_Shape
  ) ;
  begin
    if _shape = nil then
      Clear
    else begin
      if not assigned( _shape.Layer ) then exit ;

      initiateFldList( _shape.Layer, _shape ) ;

      CancelChanges ;

      if not FReadOnly then
        oErrorMessage.Visible := True ;

      doResize( nil ) ;
    end ;
  end ;

  procedure TGIS_ControlAttributes.ShowSelected(
    const _layer  : TGIS_LayerVector
  ) ;
  begin
    if not assigned( _layer ) then begin
      Clear ;
    end else begin
      if _layer.SelectedList.Count = 0 then begin
        Clear ;
        exit ;
      end;
      initiateStatList( _layer ) ;
      doResize( nil ) ;
    end ;
  end ;

  procedure TGIS_ControlAttributes.CancelChanges ;
  begin
    oBtnOk.Text     := _rsrcna( GIS_RS_BTN_OK     ) ;
    oBtnCancel.Text := _rsrcna( GIS_RS_BTN_CANCEL ) ;

    bCancel := True ;
    try
      if not Assigned( T_ScrollBox(oGrid).Shape ) then exit ;
      T_ScrollBox(oGrid).CancelChanges ;
    finally
      bCancel := False ;
      oBtnOk.Enabled := False ;
      oBtnCancel.Enabled := False ;
      oErrorMessage.Text := '';
    end ;
  end ;

  function TGIS_ControlAttributes.SaveChanges
    : Boolean ;
  var
    msg : String ;

    function check_if_save : Boolean ;
    var
      ll  : TGIS_LayerAbstract ;
      shp : TGIS_Shape ;
    begin
      Result := False ;
      if not assigned( T_ScrollBox(oGrid).Shape  ) or
         not assigned( T_ScrollBox(oGrid).Layer  ) then exit ;

      if assigned( T_ScrollBox(oGrid).Viewer ) then begin
        ll := T_ScrollBox(oGrid).Viewer.Get( T_ScrollBox(oGrid).LayerName ) ;
        if not assigned( ll ) or not( ll is TGIS_LayerVector ) then exit ;
        if T_ScrollBox(oGrid).Shape.Uid <> -1 then begin
          shp := TGIS_LayerVector(ll).GetShape( T_ScrollBox(oGrid).Shape.Uid ) ;
          if not assigned( shp ) then exit ;
        end
      end
      else begin
        if T_ScrollBox(oGrid).Shape.Uid <> -1 then begin
          shp := T_ScrollBox(oGrid).Layer.GetShape( T_ScrollBox(oGrid).Shape.Uid ) ;
          if not assigned( shp ) then exit ;
        end ;
      end;
      Result := True ;
    end;

  begin
    Result := False ;

    if check_if_save = False then begin
      oBtnOk.Enabled := False ;
      oBtnCancel.Enabled := False ;
      exit ;
    end ;
    if not T_ScrollBox(oGrid).ValidateFields( msg ) then begin
      oErrorMessage.Text := msg ;
      oBtnOk.Enabled := False ;
      exit ;
    end ;
    oErrorMessage.Text := '' ;
    T_ScrollBox(oGrid).UpdateLayer ;
    Result := True ;
    oBtnOk.Enabled     := False ;
    oBtnCancel.Enabled := False ;
  end ;

  procedure TGIS_ControlAttributes.CopyToClipboard ;
  begin
    T_ScrollBox(oGrid).CopyToClipboard ;
  end ;

  procedure TGIS_ControlAttributes.SubscribedEvent(
    _sender  : TObject ;
    _event   : Integer ;
    _context : TObject
  ) ;
  begin
    case _event of
      GIS_SUBSCRIBED_DESTROY :
        begin
          unsubscribeFromViewer ;
          T_ScrollBox(oGrid).ResetViewer ;
        end;
      GIS_SUBSCRIBED_PROJECT_CLOSE :
        begin
          Clear ;
        end ;
    end;
  end ;

  procedure Register ;
  begin
    RegisterComponents( 'TatukGIS', [ TGIS_ControlAttributes ] ) ;
  end ;

//==================================== END =====================================

end.

