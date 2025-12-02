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

unit Lider.CG.GIS.VCL.GeoControlAttributes ;
{$HPPEMIT '#pragma link "Lider.CG.GIS.VCL.GeoControlAttributes"'}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

uses
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
  {$ENDIF}
  System.SysUtils,
  System.Classes,
  System.Math,
  System.Variants,
  VCL.Graphics,
  VCL.Forms,
  VCL.Controls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  VCL.ComCtrls,
  VCL.Menus,
  VCL.ClipBrd,

  System.UITypes,
  {$IFDEF GIS_XDK}
    XDK.Splitter,
    XDK.Core,
  {$ENDIF}

  Lider.CG.GIS.GeoRtl,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoInterfaces,
  Lider.CG.GIS.GeoCsBase,
  Lider.CG.GIS.GeoTypes,
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
  [ComponentPlatformsAttribute( pfidWindows )]
  TGIS_ControlAttributes = class( TCustomPanel, IGIS_Subscribe )
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
        ///    Decides where use BiDiMode from property or
        ///    from translation
        ///  </summary>
        FBiDiModeFromTranslation : Boolean ;

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
        FFieldNameColumnWidth : Integer ;

        /// <summary>
        ///   When True, button 'Ok' will be shown
        /// </summary>
        FShowBtnOk     : Boolean ;

        /// <summary>
        ///   When True, button 'Cancel' will be shown
        /// </summary>
        FShowBtnCancel : Boolean ;

        /// <summary>
        ///   Field change event, fired after changing a control value.
        /// </summary>
        FFieldChangeEvent : TNotifyEvent ;

    private
      bCreated     : Boolean        ;
      bCancel      : Boolean        ;
      oGrid        : TObject        ;
      oErrorMessage: TLabel         ;
      oBtnOk       : TButton        ;
      oBtnCancel   : TButton        ;
      uponDestroy  : Boolean        ;
      recreate     : Boolean        ;
      iPPI         : Integer        ;

    private
      pOnHelp      : TGIS_HelpEvent ;

    private
      function  rebuildFldList      ( _layer        : TGIS_LayerVector
                                    ) : Boolean ;
      function  rebuildStatList     ( _layer        : TGIS_LayerVector
                                    ) : Boolean ;
      procedure initiateFldList     ( _layer        : TGIS_LayerVector ;
                                      _shape        : TGIS_Shape
                                    ) ;
      procedure initiateStatList    ( _layer        : TGIS_LayerVector
                                    ) ;

    {$IFNDEF OXYGENE} private {$ELSE} assembly {$ENDIF}

      procedure fset_FieldNameColumnWidth
                                    ( const _value  : Integer
                                    ) ;
      procedure fset_ShowVirtualFields
                                    ( const _value  : Boolean
                                    ) ;
      procedure fset_IgnoreFldxDefinition
                                    ( const _value  : Boolean
                                    ) ;
      procedure fset_LayoutType     ( const _value  : TGIS_LayoutType
                                    ) ;
      procedure fset_BiDiModeFromTranslation( const _value  : Boolean
                                    ) ;
      procedure fset_ParentBiDiMode ( const _value  : Boolean
                                    ) ;
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
                                      _shift        : TShiftState
                                    ) ; dynamic ;

      /// <summary>
      ///   Button OK event handler.
      /// </summary>
      procedure doOk                ( _sender       : TObject
                                    ) ; dynamic ;
      procedure doResize            ( _sender       : TObject
                                    ) ; dynamic ;
      procedure updateLayout        ;

      /// <summary>
      ///   Calculates the scaling factor according to current PPI.
      /// </summary>
      procedure updatePPI     ;

      function  apScale         ( const _size : Integer
                                ) : Integer ;

      procedure doMouseWheel( _sender      : TObject ;
                              _shift       : TShiftState ;
                              _wheelDelta  : Integer ;
                              _mousePos    : TPoint ;
                              var _handled : Boolean
                             ) ;
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
      /// <param name="_aowner">
      ///   owner of the component
      /// </param>
      constructor Create            ( _aowner       : TComponent
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
      ///   True, if changes were saved.
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

        /// <summary>
        ///   The set of virtual fields that are displayed when
        ///   ShowVirtualFields property is active.
        /// </summary>
        property VirtualFields : TGIS_VirtualFields
                                         read  FVirtualFields
                                         write FVirtualFields ;

    published // properties derived from base class

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property ParentBiDiMode write fset_ParentBiDiMode ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property BiDiMode       ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property Align          ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property Anchors        ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property BevelInner     ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property BevelOuter     default bvNone ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property BevelWidth     ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property BorderStyle    ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property BorderWidth    ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property Color          ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property Ctl3D          ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property Enabled        ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property Font           ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property HelpContext    ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property Hint           ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property ParentColor    ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property ParentCtl3D    ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property ParentFont     ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property ParentShowHint ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property PopupMenu      ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property TabStop        ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property TabOrder       ;

        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property Visible        ;

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
                               : Integer read  FFieldNameColumnWidth
                                         write fset_FieldNameColumnWidth
                                         default 80 ;

        /// <summary>
        ///   When True, 'Fldx' rules are ignored.
        /// </summary>
        property IgnoreFldxDefinition
                               : Boolean read  FIgnoreFldxDefinition
                                         write fset_IgnoreFldxDefinition
                                         default False ;

        /// <summary>
        ///   Layout type.
        /// </summary>
        property LayoutType    : TGIS_LayoutType
                                         read  FLayoutType
                                         write fset_LayoutType
                                         default TGIS_LayoutType.TwoColumns ;

        ///  <summary>
        ///    Decides where use BiDiMode from property or
        ///    from translation
        ///  </summary>
        property BiDiModeFromTranslation : Boolean
                                              read FBiDiModeFromTranslation
                                              write fset_BiDiModeFromTranslation
                                              default False ;

        /// <summary>
        ///   True, if component is Read Only.
        /// </summary>
        property &ReadOnly     : Boolean read  FReadOnly
                                         write fset_ReadOnly
                                         default False ;

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
                                         write fset_ShowVirtualFields
                                         default True ;

        /// <summary>
        ///   Show or hide virtual layer fields.
        /// </summary>
        property ShowVirtualFields
                               : Boolean read  FShowVirtualFields
                                         write fset_ShowVirtualFields
                                         default True ;

    published // events derived from base class

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property OnClick
                 {$IFDEF GENDOC}
                   : TNotifyEvent read dummy write dummy
                 {$ENDIF} ;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property OnDblClick
                 {$IFDEF GENDOC}
                   : TNotifyEvent read dummy write dummy
                 {$ENDIF} ;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property OnDragDrop     ;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property OnDragOver     ;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property OnEndDrag      ;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property OnEnter        ;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property OnExit         ;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property OnKeyDown
                 {$IFDEF GENDOC}
                   : TKeyEvent read dummy write dummy
                 {$ENDIF} ;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property OnKeyPress
                 {$IFDEF GENDOC}
                   : TKeyPressEvent read dummy write dummy
                 {$ENDIF} ;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property OnKeyUp
                 {$IFDEF GENDOC}
                   : TKeyEvent read dummy write dummy
                 {$ENDIF} ;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property OnMouseDown
                 {$IFDEF GENDOC}
                   : TMouseEvent read dummy write dummy
                 {$ENDIF} ;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property OnMouseMove
                 {$IFDEF GENDOC}
                   : TMouseMoveEvent read dummy write dummy
                 {$ENDIF} ;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property OnMouseUp
                 {$IFDEF GENDOC}
                   : TMouseEvent read dummy write dummy
                 {$ENDIF} ;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property OnMouseWheel
                 {$IFDEF GENDOC}
                   : TMouseWheelEvent read dummy write dummy
                 {$ENDIF} ;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property OnMouseWheelDown
                 {$IFDEF GENDOC}
                   : TMouseWheelUpDownEvent read dummy write dummy
                 {$ENDIF} ;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property OnMouseWheelUp
                 {$IFDEF GENDOC}
                   : TMouseWheelUpDownEvent read dummy write dummy
                 {$ENDIF} ;

        /// <event/>
        /// <summary>
        ///   See documentation for TCustomControl in Delphi help.
        /// </summary>
        property OnStartDrag    ;

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

    {$IFDEF GIS_XDK}
      public
        {#gendoc:hide:GENXDK}
        XDK : TGIS_ControlXDK ;
    {$ENDIF}

  end ;

  {#gendoc:hide}
  procedure Register;

//##############################################################################
implementation

{$R 'GisControlAttributes_16x16.RES'}

uses
  System.Generics.Collections,
  System.Generics.Defaults,
  {$IFNDEF GIS_NOADO_JOIN}
    {$IFDEF ADOINTUNIT}
      Winapi.ADOint,
    {$ELSE}
      Lider.CG.GIS.GeoAdoInt,
    {$ENDIF}
  {$ENDIF}

  Lider.CG.GIS.GeoInternals,
  Lider.CG.GIS.GeoLayer,
  Lider.CG.GIS.VCL.GeoControlHelper,
  Lider.CG.GIS.VCL.GeoControlAttributesForm ;

const
  HGAP = 4 ;
  VGAP = 2 ;
  GGAP = 6 ;

{$IFDEF GIS_XDK}
  // patch to buggy windowless ActiveX context.

  type
    TEdit = class( Vcl.StdCtrls.TEdit )
      public
       procedure SetFocus; override ;
    end;

    TMemo = class( Vcl.StdCtrls.TMemo )
      public
       procedure SetFocus; override ;
    end;

    TComboBox = class( Vcl.StdCtrls.TComboBox )
      public
       procedure SetFocus; override ;
    end;

  procedure TEdit.SetFocus;
  begin
    if ( GetParentForm(Self) <> nil )
       or
       ( ParentWindow <> 0 )
    then
      inherited;
  end;

  procedure TMemo.SetFocus;
  begin
    if ( GetParentForm(Self) <> nil )
       or
       ( ParentWindow <> 0 )
    then
      inherited;
  end;

  procedure TComboBox.SetFocus;
  begin
    if ( GetParentForm(Self) <> nil )
       or
       ( ParentWindow <> 0 )
    then
      inherited;
  end;
{$ENDIF}

type

  T_ScrollBox = class ( TScrollBox )
    private
      oLayer      : TGIS_LayerVector ;
      oLayerName  : String ;
      oShape      : TGIS_Shape ;
      oField      : String ;
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      oViewer     : IGIS_Viewer ;
      eLayout     : TGIS_LayoutType ;
      bReadOnly   : Boolean    ;
      oPanel      : TPanel ;
      oPanelLeft  : TPanel ;
      oPanelRight : TPanel ;
      oSplitter   : TSplitter ;
      oPopup      : TPopupMenu ;
      pOnHelp     : TGIS_HelpEvent ;
      iPPI        : Integer ;
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
      procedure setHeader     ;
      function  addLabel      ( _fld_no       : Integer ;
                                _fld          : TGIS_FieldInfo ;
                                _rule         : TGIS_FieldRule
                              ) : TLabel ;
      function  getMemoHeight : Integer ;
      procedure addTextBoxStd ( _fld_no       : Integer ;
                                _fld          : TGIS_FieldInfo ;
                                _rule         : TGIS_FieldRule ;
                                _multiline    : Boolean ;
                                var _order    : Integer
                              ) ;
      procedure addTextBox    ( _fld_no    : Integer ;
                                _fld       : TGIS_FieldInfo ;
                                _rule      : TGIS_FieldRule ;
                                _multiline : Boolean ;
                                var _order : Integer
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
      function  apScale       ( const _size   : Integer
                              ) : Integer ;
      procedure addInternalFields
                              ( const _controlAttributes : TGIS_ControlAttributes ;
                                const _layer  : TGIS_LayerVector ;
                                var   _order  : Integer
                              ) ;

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
                                _x, _y        : Integer
                              ) ; dynamic ;
      procedure doLabelMouseMove
                              ( _sender       : TObject ;
                                _shift        : TShiftState ;
                                _x, _y        : Integer
                              ) ; dynamic ;
      procedure doLabelMouseUp( _sender       : TObject ;
                                _button       : TMouseButton ;
                                _shift        : TShiftState ;
                                _x, _y        : Integer
                              ) ; dynamic ;
      procedure doValueClick  ( _sender       : TObject
                              ) ; dynamic ;
      procedure doValueContextPopup
                              (     _sender   : TObject ;
                                    _mpos     : TPoint ;
                                var _handled  : Boolean
                              ) ; dynamic ;
      procedure doValueKeyDown(     _sender   : TObject ;
                                var _key      : Word    ;
                                    _shift    : TShiftState
                              ) ; dynamic ;
      procedure doValueKeyPress
                              (     _sender   : TObject ;
                                var _key      : Char
                              ) ; dynamic ;
      procedure doValueKeyUp  (     _sender   : TObject ;
                                var _key      : Word    ;
                                    _shift    : TShiftState
                              ) ; dynamic ;
      procedure doSplitterMoved
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
      procedure ClearFields         ;
      procedure InitiateFieldsForViewing
                                    ( _layer        : TGIS_LayerVector ;
                                      _shape        : TGIS_Shape
                                    ) ;
      procedure InitiateFieldsForEditing
                                    ( _layer        : TGIS_LayerVector ;
                                      _shape        : TGIS_Shape
                                    ) ;
      procedure InitiateStatistics  ( _layer        : TGIS_LayerVector ;
                                      _init         : Boolean
                                    ) ;
      procedure SetShape            ( _shape        : TGIS_Shape
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

    public
      property  Viewer     : IGIS_Viewer      read oViewer ;
      property  Layer      : TGIS_LayerVector read oLayer ;
      property  LayerName  : String           read oLayerName ;
      property  Shape      : TGIS_Shape       read oShape ;
      property  LayoutType : TGIS_LayoutType  read eLayout ;
      property  PPI        : Integer          read iPPI write iPPI ;
  end ;

  T_DateTimePicker = class (TDateTimePicker)
  protected
    oldFormat : String ;
  protected
    procedure Change ; override ;
  end ;

  function clInvalid
    : TColor ;
  begin
    Result := ( 175 shl 16 ) + ( 189 shl 8 ) + 250
  end ;

  function clValid
    : TColor ;
  begin
    Result := 16777215 ;
  end ;

{ T_DateTimePicker }

  procedure T_DateTimePicker.Change ;
  begin
    // restore format
    Format   := oldFormat ;
    DateTime := VarToDateTime( Caption ) ;
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

    Anchors := [akLeft, akTop, akRight] ;
    BorderStyle := bsNone  ;
    BevelOuter  := bvNone  ;
    DoubleBuffered := not IsWin11 ;
    BiDiMode := TGIS_ControlAttributes(_parent).BiDiMode ;
    ParentColor := True ;
    iPPI := 96 ;

    oPanel := TPanel.Create( Self ) ;
    with oPanel do
    begin
      Parent := Self ;
      BorderStyle := bsNone ;
      BevelOuter  := bvNone ;
      DoubleBuffered := not IsWin11 ;
      oPanel.Left := 0 ;
      oPanel.Top  := 0 ;
    end ;

    oSplitter := TSplitter.Create( oPanel ) ;
    with oSplitter do
    begin
      Parent := oPanel ;
      if BiDiMode = bdRightToLeft then
        Left := 0
      else
        Left := 210 ;
      Top := 1;
      Height := 263;
      AutoSnap := False ;
      Beveled := False ;
      MinSize := 30 ;
      ResizeStyle := rsPattern ;
      if BiDiMode = bdRightToLeft then
        Align := alRight
      else
        Align := alLeft ;
      OnMoved := doSplitterMoved ;
    end ;

    oPanelLeft := TPanel.Create( oPanel ) ;
    with oPanelLeft do
    begin
      Parent := oPanel ;
      if BiDiMode = bdRightToLeft then
        Align := alRight
      else
        Align := alLeft ;
      BorderStyle := bsNone ;
      BevelOuter  := bvNone ;
      DoubleBuffered := not IsWin11 ;
      OnMouseDown := doLabelMouseDown ;
    end ;

    oPanelRight := TPanel.Create( oPanel ) ;
    with oPanelRight do
    begin
      Parent := oPanel ;
      Align := alClient ;
      BorderStyle := bsNone ;
      BevelOuter  := bvNone ;
      DoubleBuffered := not IsWin11 ;
    end ;

    oPopup := TPopupMenu.Create( Self ) ;

    Visible := False   ;
    DoubleBuffered := not IsWin11 ;

    eLayout   := TGIS_LayoutType.TwoColumns ;
    bReadOnly := False ;
    oLayer    := nil ;
    oShape    := nil ;
    pOnHelp   := nil ;
  end ;

  procedure T_ScrollBox.ClearFields ;
  var
    i : Integer ;
    cntrl : TControl ;
  begin
    oLayer := nil ;
    oLayerName := '' ;
    oShape := nil ;

    for i := oPanelLeft.ControlCount-1 downto 0 do begin
      cntrl := oPanelLeft.Controls[i] ;
      oPanelLeft.RemoveControl( cntrl ) ;
      FreeObject( cntrl ) ;
    end ;
    for i := oPanelRight.ControlCount-1 downto 0 do begin
      cntrl := oPanelRight.Controls[i] ;
      oPanelRight.RemoveControl( cntrl ) ;
      FreeObject( cntrl ) ;
    end ;
  end;

  function T_ScrollBox.getFieldId(
        _name : String ;
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
        if assigned( oLayer.JoinADO ) then begin
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
      if _control is TEdit then
        TEdit(_control).Color := _color
      else if _control is TMemo then
        TMemo(_control).Color := _color
      else if _control is TCheckBox then
        TCheckBox(_control).Color := _color
      else if _control is TComboBox then
        TComboBox(_control).Color := _color
      else if _control is TDateTimePicker then
        TDateTimePicker(_control).Color := _color ;
    end ;

    function get_control_text : String ;
    begin
      if _control is TEdit then
        Result := TEdit(_control).Text
      else if _control is TMemo then
        Result := TMemo(_control).Text
      else if _control is TComboBox then
        Result := TComboBox(_control).Text
      else if _control is TCheckBox then
        Result := BoolToStr( TCheckBox(_control).Checked )
      else if _control is TDateTimePicker then
        Result := DateTimeToStr( TDateTimePicker(_control).DateTime )
    end;

  begin
    Result := True ;
    fld := oLayer.FieldInfo( getFieldId( _control.Name, wasjoin ) ) ;
    rule := TGIS_FieldRule( fld.Rules ) ;
    if Assigned( rule ) and
       not TGIS_ControlAttributes(Parent).IgnoreFldxDefinition then begin

      if not rule.Check( get_control_text, _mode, _msg ) then
        Result := False ;
    end
    else
      Result := checkValue( fld, get_control_text, _mode, _msg ) ;

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
    for i := 0 to oPanelRight.ControlCount - 1 do begin

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
        end ;
      end else if cntrl is TDateTimePicker then begin
        if not ValidateTextBox( TDateTimePicker(cntrl),
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
    end
    else begin
      if BiDiMode = TBiDiMode.bdRightToLeft then
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
        lbl.Caption := _rsrc(GIS_RS_ATTRIBUTES_DLG_COUNT)
      else
        lbl.Caption := _rsrc(GIS_RS_ATTRIBUTES_DLG_UID) ;
    end else begin
      lbl := TLabel.Create( oPanelRight ) ;
      lbl.Parent := oPanelRight ;
      lbl.Caption := getHeaderCaption( stat ) ;
    end ;
    if stat then
      lbl.Name := 'lbl' + GIS_FIELD_SELECTED
    else
      lbl.Name := 'lbl' + GIS_FIELD_UID ;
    lbl.Font.Style := [fsBold] ;
    if BiDiMode = bdRightToLeft then
      lbl.Anchors := [akRight, akTop]
    else
      lbl.Anchors := [akLeft, akTop] ;
    lbl.Top  := GGAP ;
    lbl.OnMouseDown := doLabelMouseDown ;
    // add content
    uid := TLabel.Create( oPanelRight ) ;
    uid.Parent := oPanelRight ;
    if stat then
      uid.Name := GIS_FIELD_SELECTED
    else
      uid.Name := GIS_FIELD_UID ;
    uid.Font.Style := [fsBold] ;
    if eLayout = TGIS_LayoutType.TwoColumns then begin
      if stat then
        uid.Caption := IntToStr( oLayer.SelectedList.Count )
      else
        uid.Caption := IntToStr( oShape.Uid ) ;
    end else begin
      uid.Caption := '';
    end ;
    if BiDiMode = bdRightToLeft then
      uid.Anchors := [akRight, akTop]
    else
      uid.Anchors := [akLeft, akTop] ;
    uid.Top  := GGAP ;
  end ;

  procedure T_ScrollBox.setHeader ;
  var
    lbl  : TLabel ;
    stat : Boolean ;
  begin
    stat := not assigned( oShape ) ;
    lbl := TLabel(oPanelRight.Controls[0]) ;
    lbl.Caption := getHeaderCaption( stat ) ;
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
    end
    else begin
      lbl := TLabel.Create( oPanelRight ) ;
      lbl.Parent := oPanelRight ;
    end ;
    lbl.Name := 'lbl' + IntToStr( _fld_no ) ;
    if Assigned( _rule ) and not IsStringEmpty( _rule.Caption ) then
      lbl.Caption := _rule.Caption
    else
      lbl.Caption := _fld.NewName ;
    if BiDiMode = bdRightToLeft then
      lbl.Anchors := [akRight, akTop]
    else
      lbl.Anchors := [akLeft, akTop] ;
    lbl.OnClick     := doLabelClick     ;
    lbl.OnDblClick  := doLabelDblClick  ;
    lbl.OnMouseDown := doLabelMouseDown ;
    lbl.OnMouseMove := doLabelMouseMove ;
    lbl.OnMouseUp   := doLabelMouseUp   ;
    Result := lbl ;
  end;

  function T_ScrollBox.getMemoHeight
    : Integer ;
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
    mem : TMemo ;
    edt : TEdit ;
  begin
    lbl := addLabel( _fld_no, _fld, _rule ) ;
    if _multiline then
    begin
      mem := TMemo.Create( oPanelRight ) ;
      mem.Parent := oPanelRight ;
      mem.Name := setFieldName( _fld_no ) ;
      mem.ReadOnly := True ;
      mem.WordWrap := True ;
      mem.Text := '';
      mem.Anchors := [akLeft, akTop, akRight] ;
      mem.Height := getMemoHeight ;
      mem.ScrollBars := ssVertical ;
      mem.OnClick := doValueClick ;
      mem.OnContextPopup := doValueContextPopup ;
      lbl.FocusControl := mem ;
      mem.TabOrder := _order ;
    end else
    begin
      edt := TEdit.Create( oPanelRight ) ;
      edt.Parent := oPanelRight ;
      edt.Name := setFieldName( _fld_no ) ;
      edt.ReadOnly := True ;
      edt.Text := '';
      edt.BorderStyle := bsNone ;
      if not TGIS_ControlAttributes( Parent ).ReadOnly then
        edt.BevelKind   := bkTile
      else
        edt.BevelKind   := bkNone ;
      edt.Anchors := [akLeft, akTop, akRight] ;
      edt.OnClick := doValueClick ;
      edt.OnContextPopup := doValueContextPopup ;
      lbl.FocusControl := edt ;
      edt.TabOrder := _order ;
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
    attr : TGIS_ControlAttributes ;
    lbl  : TLabel ;
    mem  : TMemo ;
    edt  : TEdit ;
    readonly : Boolean ;
  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    readonly := attr.ReadOnly ;
    lbl := addLabel( _fld_no, _fld, _rule ) ;
    if _multiline then begin
      mem := TMemo.Create( oPanelRight ) ;
      mem.Parent := oPanelRight ;
      mem.ReadOnly :=readonly ;
      mem.WordWrap := True ;
      mem.Name := setFieldName( _fld_no ) ;
      mem.Anchors := [akLeft, akTop, akRight] ;
      mem.Height := getMemoHeight ;
      mem.ScrollBars := ssVertical ;
      mem.OnClick := doValueClick ;
      mem.OnContextPopup := doValueContextPopup ;
      if not readonly then begin
        mem.OnKeyDown  := doValueKeyDown  ;
        mem.OnKeyPress := doValueKeyPress ;
        mem.OnKeyUp    := doValueKeyUp    ;
        mem.OnEnter := attr.doControlEnter ;
        mem.OnChange := attr.doValueChanged ;
      end ;
      lbl.FocusControl := mem ;
      mem.TabOrder := _order ;
    end
    else begin
      edt := TEdit.Create( oPanelRight ) ;
      edt.Parent := oPanelRight ;
      edt.ReadOnly := readonly ;
      edt.Name := setFieldName( _fld_no ) ;
      edt.Anchors := [akLeft, akTop, akRight] ;
      edt.OnClick := doValueClick ;
      edt.OnContextPopup := doValueContextPopup ;
      edt.BorderStyle := bsNone ;
      edt.BevelKind   := bkTile ;
      if not readonly then begin
        edt.OnKeyDown  := doValueKeyDown  ;
        edt.OnKeyPress := doValueKeyPress ;
        edt.OnKeyUp    := doValueKeyUp    ;
        edt.OnEnter := attr.doControlEnter ;
        edt.OnChange := attr.doValueChanged ;
      end
      else
        edt.BevelKind := bkNone ;
      lbl.FocusControl := edt ;
      edt.TabOrder := _order ;
    end ;
    inc( _order ) ;
  end ;

  procedure T_ScrollBox.addComboBox(
    _fld_no    : Integer ;
    _fld       : TGIS_FieldInfo ;
    _rule      : TGIS_FieldRule ;
    var _order : Integer
  ) ;
  var
    attr : TGIS_ControlAttributes ;
    lbl  : TLabel ;
    cbx  : TComboBox ;
    i    : Integer ;
  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    lbl := addLabel( _fld_no, _fld, _rule ) ;
    cbx := TComboBox.Create( oPanelRight ) ;
    cbx.Parent := oPanelRight ;
    cbx.Style := csDropDownList ;
    for i := 0 to _rule.Values.Items.Count-1 do
      cbx.Items.Add( _rule.Values.Items[i] ) ;
    cbx.Name := setFieldName( _fld_no ) ;
    cbx.Enabled := not attr.ReadOnly ;
    if not attr.ReadOnly then begin
      cbx.OnEnter := attr.doControlEnter ;
      cbx.OnChange := attr.doValueChanged ;
    end ;
    cbx.Anchors := [akLeft, akTop, akRight] ;
    cbx.OnClick := doValueClick ;
    cbx.OnContextPopup := doValueContextPopup ;
    if not attr.ReadOnly then begin
      cbx.OnKeyDown  := doValueKeyDown  ;
      cbx.OnKeyPress := doValueKeyPress ;
      cbx.OnKeyUp    := doValueKeyUp    ;
    end ;
    lbl.FocusControl := cbx ;
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
    attr     : TGIS_ControlAttributes ;
    lbl      : TLabel ;
    dtp      : TDateTimePicker ;
    fs       : TFormatSettings ;
    readonly : Boolean ;
  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    readonly := attr.ReadOnly ;

    lbl := addLabel( _fld_no, _fld, _rule ) ;
    dtp := T_DateTimePicker.Create( oPanelRight ) ;
    dtp.Parent := oPanelRight ;
    dtp.Kind := dtkDate ;
    fs := TFormatSettings.Create( GetUserDefaultLCID ) ;
    // fix QC bug in FixDateSeparator
    dtp.Format := StringReplaceAll( fs.ShortDateFormat, '/', fs.DateSeparator ) +
                  ' ' + fs.LongTimeFormat ;
    {$IFDEF LEVEL_RX11_VCL}
      // fix QC bug in TFormatSettings
      dtp.Format := StringReplaceAll( dtp.Format, 'mm', 'MM' ) ;
      dtp.Format := StringReplaceAll( dtp.Format, 'nn', 'mm' ) ;
    {$ENDIF}
    // remember original format
    T_DateTimePicker(dtp).oldFormat := dtp.Format ;

    dtp.Name := setFieldName( _fld_no ) ;
    dtp.Enabled := not readonly ;
    dtp.Anchors := [akLeft, akTop, akRight] ;
    dtp.OnClick := doValueClick ;
    dtp.OnContextPopup := doValueContextPopup ;
    if not attr.ReadOnly then begin
      dtp.OnKeyDown  := doValueKeyDown  ;
      dtp.OnKeyPress := doValueKeyPress ;
      dtp.OnKeyUp    := doValueKeyUp    ;
      dtp.OnEnter := attr.doControlEnter ;
      dtp.OnChange := attr.doValueChanged ;
    end ;
    lbl.FocusControl := dtp ;
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
    attr     : TGIS_ControlAttributes ;
    lbl      : TLabel ;
    chk      : TCheckBox ;
    readonly : Boolean ;
  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    readonly := attr.ReadOnly ;

    lbl := addLabel( _fld_no, _fld, _rule ) ;
    chk := TCheckBox.Create( oPanelRight ) ;
    chk.Parent := oPanelRight ;
    chk.Enabled := not readonly ;
    chk.Name := setFieldName( _fld_no ) ;
    chk.Caption := '' ;
    chk.Anchors := [akLeft, akTop, akRight] ;
//    chk.OnClick := doValueClick ;
    chk.OnContextPopup := doValueContextPopup ;
    if not readonly then begin
      chk.OnEnter := attr.doControlEnter ;
      chk.OnClick := attr.doValueChanged ;
      chk.OnKeyDown  := doValueKeyDown  ;
      chk.OnKeyPress := doValueKeyPress ;
      chk.OnKeyUp    := doValueKeyUp    ;
    end ;
    lbl.FocusControl := chk ;
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
    end
    else begin
      lbl := TLabel.Create( oPanelRight ) ;
      lbl.Parent := oPanelRight ;
    end ;
    lbl.Name := 'lbl' + _field ;
    lbl.Caption := _field ;
    if BiDiMode = bdRightToLeft then
      lbl.Anchors := [akRight, akTop]
    else
      lbl.Anchors := [akLeft, akTop] ;
    lbl.OnClick     := doLabelClick     ;
    lbl.OnDblClick  := doLabelDblClick  ;
    lbl.OnMouseDown := doLabelMouseDown ;
    lbl.OnMouseMove := doLabelMouseMove ;
    lbl.OnMouseUp   := doLabelMouseUp   ;
    // add content
    tbx := TEdit.Create( oPanelRight ) ;
    tbx.Parent := oPanelRight ;
    tbx.Name := _field ;
    tbx.ReadOnly := True ;
    tbx.Text := '';
    tbx.BorderStyle := bsNone ;
    if not TGIS_ControlAttributes( Parent ).ReadOnly then
      tbx.BevelKind   := bkTile
    else
      tbx.BevelKind   := bkNone ;
    tbx.Anchors := [akLeft, akTop, akRight] ;
    tbx.OnClick := doValueClick ;
    tbx.OnContextPopup := doValueContextPopup ;
    lbl.FocusControl := tbx ;
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
      end
      else begin
        lbl := TLabel.Create( oPanelRight ) ;
        lbl.Parent := oPanelRight ;
      end ;
      lbl.Name := 'lbldb' + IntToStr( _fld_no ) ;
      lbl.Caption := fname ;
      if BiDiMode = bdRightToLeft then
        lbl.Anchors := [akRight, akTop]
      else
        lbl.Anchors := [akLeft, akTop] ;
      lbl.OnClick     := doLabelClick     ;
      lbl.OnDblClick  := doLabelDblClick  ;
      lbl.OnMouseDown := doLabelMouseDown ;
      lbl.OnMouseMove := doLabelMouseMove ;
      lbl.OnMouseUp   := doLabelMouseUp   ;
      // add content
      tbx := TEdit.Create( oPanelRight ) ;
      tbx.Parent := oPanelRight ;
      tbx.Name := 'valdb' + IntToStr( _fld_no ) ;
      tbx.ReadOnly := True ;
      tbx.Text := '';
      tbx.BorderStyle := bsNone ;
      tbx.BevelKind   := bkNone ;
      tbx.Anchors := [akLeft, akTop, akRight] ;
      tbx.OnClick := doValueClick ;
      tbx.OnContextPopup := doValueContextPopup ;
      lbl.FocusControl := tbx ;
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
      end
      else begin
        lbl := TLabel.Create( oPanelRight ) ;
        lbl.Parent := oPanelRight ;
      end ;
      lbl.Name := 'lbldb' + IntToStr( _fld_no ) ;
      lbl.Caption := fname ;
      if BiDiMode = bdRightToLeft then
        lbl.Anchors := [akRight, akTop]
      else
        lbl.Anchors := [akLeft, akTop] ;
      lbl.OnClick     := doLabelClick     ;
      lbl.OnDblClick  := doLabelDblClick  ;
      lbl.OnMouseDown := doLabelMouseDown ;
      lbl.OnMouseMove := doLabelMouseMove ;
      lbl.OnMouseUp   := doLabelMouseUp   ;
      // add content
      tbx := TEdit.Create( oPanelRight ) ;
      tbx.Parent := oPanelRight ;
      tbx.Name := 'valdb' + IntToStr( _fld_no ) ;
      tbx.ReadOnly := True ;
      tbx.Text := '';
      tbx.BorderStyle := bsNone ;
      tbx.BevelKind   := bkNone ;
      tbx.Anchors := [akLeft, akTop, akRight] ;
      tbx.OnClick := doValueClick ;
      tbx.OnContextPopup := doValueContextPopup ;
      lbl.FocusControl := tbx ;
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
    attr    : TGIS_ControlAttributes ;
    i       : Integer ;
    fld     : TGIS_FieldInfo ;
    rule    : TGIS_FieldRule ;
    wasjoin : Boolean ;
    {$IFNDEF GIS_NOADO}
      ii    : Integer ;
    {$ENDIF}

  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    if assigned( _layer.Viewer ) then
      oViewer := _layer.Viewer.Ref
    else
      oViewer := nil ;
    oLayer  := _layer ;
    oLayerName := _layer.Name ;
    oShape  := _shape ;
    eLayout := attr.LayoutType ;
    bReadOnly := True ;
    addHeader ;
    order := 0 ;

    addInternalFields( attr, oLayer, order ) ;

    for i := 0 to oLayer.Fields.Count-1 do begin
      fld := oLayer.FieldInfo(i) ;
      if not (TGIS_FieldFlags.Visible in fld.Flags) then continue ;
      rule := TGIS_FieldRule(fld.Rules) ;
      if not attr.IgnoreFldxDefinition and ( rule <> nil ) then begin
        if rule.Values.Mode = TGIS_FieldValuesMode.MultiLine then
          addTextBoxStd( i, fld, rule, True, order )
        else if rule.Values.Mode <> TGIS_FieldValuesMode.Hidden then
          addTextBoxStd( i, fld, rule, False, order ) ;
      end
      else begin
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
    vFields : TGIS_VirtualFields;
    fld    : TGIS_FieldInfo ;
    rule   : TGIS_FieldRule ;
  begin
    if not _controlAttributes.ShowVirtualFields then
      exit ;

    vFields := _controlAttributes.VirtualFields;

    // editable
    if TGIS_VirtualField.GisSelected    in vFields then
      begin
        fld := _layer.FieldInfo( GIS_FIELD_ID_SELECTED ) ;
        rule := TGIS_FieldRule( fld.Rules ) ;
        addCheckBox( GIS_FIELD_ID_SELECTED, fld, rule, _order ) ;
      end ;
    if TGIS_VirtualField.GisHidden      in vFields then
      begin
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
    if TGIS_VirtualField.GisViewerScale in vFields then addPredefined( GIS_FIELD_VIEWER_SCALE, _order ) ;
    if TGIS_VirtualField.GisViewerLevel in vFields then addPredefined( GIS_FIELD_VIEWER_LEVEL, _order ) ;
  end;

  procedure T_ScrollBox.InitiateFieldsForEditing(
    _layer       : TGIS_LayerVector ;
    _shape       : TGIS_Shape
  ) ;
  var
    order  : Integer ;
    attr   : TGIS_ControlAttributes ;
    i      : Integer ;
    fld    : TGIS_FieldInfo ;
    rule   : TGIS_FieldRule ;
  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    if assigned( _layer.Viewer ) then
      oViewer := _layer.Viewer.Ref
    else
      oViewer := nil ;
    oLayer  := _layer ;
    oLayerName := _layer.Name ;
    oShape  := _shape ;
    eLayout := attr.LayoutType ;
    bReadOnly := False ;
    addHeader ;
    order := 0 ;

    addInternalFields( attr, oLayer, order ) ;

    for i := 0 to oLayer.Fields.Count-1 do begin
      fld := oLayer.FieldInfo(i) ;
      if not (TGIS_FieldFlags.Visible in fld.Flags) then continue ;
      rule := TGIS_FieldRule(fld.Rules) ;
      if not attr.IgnoreFldxDefinition and ( rule <> nil ) then begin
        if fld.ReadOnly then begin
          if rule.Values.Mode = TGIS_FieldValuesMode.MultiLine then
            addTextBoxStd( i, fld, rule, True, order )
          else
            addTextBoxStd( i, fld, rule, False, order ) ;
        end
        else begin
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
      end
      else begin
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
    end
    else begin
      lbl := TLabel.Create( oPanelRight ) ;
      lbl.Parent := oPanelRight ;
    end ;
    lbl.Name := 'lbl' + _func + IntToStr( _fld_no ) ;
    if _func = 'avg' then
      lbl.Caption :=  Format( GIS_RS_ATTRIBUTES_DLG_AVG,
                              [ _fld.NewName ]
                            )
    else if _func = 'min' then
      lbl.Caption :=  Format( GIS_RS_ATTRIBUTES_DLG_MIN,
                              [ _fld.NewName ]
                            )
    else if _func = 'max' then
      lbl.Caption :=  Format( GIS_RS_ATTRIBUTES_DLG_MAX,
                              [ _fld.NewName ]
                            )
    else if _func = 'sum' then
      lbl.Caption :=  Format( GIS_RS_ATTRIBUTES_DLG_SUM,
                              [ _fld.NewName ]
                            ) ;;
    if BiDiMode = bdRightToLeft then
      lbl.Anchors := [akRight, akTop]
    else
      lbl.Anchors := [akLeft, akTop] ;
    lbl.OnClick     := doLabelClick     ;
    lbl.OnDblClick  := doLabelDblClick  ;
    lbl.OnMouseDown := doLabelMouseDown ;
    lbl.OnMouseMove := doLabelMouseMove ;
    lbl.OnMouseUp   := doLabelMouseUp   ;
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
    edt := TEdit.Create( oPanelRight ) ;
    edt.Parent := oPanelRight ;
    edt.Name := setFieldNameStat( _fld_no, _func ) ;
    edt.ReadOnly := True ;
    edt.Text := '';
    edt.Anchors := [akLeft, akTop, akRight] ;
    edt.OnClick := doValueClick ;
    edt.BorderStyle := bsNone ;
    edt.BevelKind   := bkTile ;
    edt.OnContextPopup := doValueContextPopup ;
    lbl.FocusControl := edt ;
    edt.TabOrder := _order ;
    Result := edt ;
  end ;

  procedure T_ScrollBox.InitiateStatistics(
    _layer : TGIS_LayerVector ;
    _init  : Boolean
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
    elm    : TPair<TGIS_Uid,Boolean>;
    shp    : TGIS_Shape ;
    oval   : Variant ;
    fld_format : String ;

    function set_fld(
      _idx   : Integer ;
      _fld   : TGIS_FieldInfo ;
      _func  : String  ;
      _order : Integer
    ) : TEdit ;
    var
      i : Integer ;
      cntr : TControl ;
    begin
      if _init then
        Result := addTextBoxStat( _idx, _fld, _func, _order )
      else begin
        Result := nil ;
        for i := 0 to oPanelRight.ControlCount -1 do begin
          cntr := oPanelRight.Controls[i] ;
          if ( cntr is TEdit ) and
             ( cntr.Name = setFieldNameStat( _idx, _func ) ) then begin
            Result := TEdit( cntr ) ;
            break ;
          end ;
        end ;
        assert( assigned( Result ) ) ;
      end;
    end ;

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
    if _init then
      addHeader
    else
      setHeader ;
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
            vals[fldcnt] := set_fld( i, fld, 'avg', order ) ;
            Inc( fldcnt ) ;
            // MIN
            tp[fldcnt] := 0 ;
            vals[fldcnt] := set_fld( i, fld, 'min', order ) ;
            Inc( fldcnt ) ;
            // MAX
            tp[fldcnt] := 0 ;
            vals[fldcnt] := set_fld( i, fld, 'max', order ) ;
            Inc( fldcnt ) ;
            // SUM
            tp[fldcnt] := 0 ;
            vals[fldcnt] := set_fld( i, fld, 'sum', order ) ;
            Inc( fldcnt ) ;
          end ;
        TGIS_FieldType.Date    :
          begin
            // AVG
            tp[fldcnt] := 1 + 2 ;
            vals[fldcnt] := set_fld( i, fld, 'avg', order ) ;
            Inc( fldcnt ) ;
            // MIN
            tp[fldcnt] := 0 + 2 ;
            vals[fldcnt] := set_fld( i, fld, 'min', order ) ;
            Inc( fldcnt ) ;
            // MAX
            tp[fldcnt] := 0 + 2 ;
            vals[fldcnt] := set_fld( i, fld, 'max', order ) ;
            Inc( fldcnt ) ;
          end ;
        TGIS_FieldType.Boolean :
          begin
            // AVG
            tp[fldcnt] := 1 ;
            vals[fldcnt] := set_fld( i, fld, 'avg', order ) ;
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
      if not assigned( shp ) then begin
        if assigned( oLayer.ParentLayer ) and
           ( oLayer.ParentLayer is TGIS_LayerVector ) then
        shp := TGIS_LayerVector(oLayer.ParentLayer).GetShape( oLayer.SelectedList[k] ) ;
      end ;
      if not assigned( shp ) then Continue ;

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
        if ( tp[i] and $01 ) <> 0 then
          vals[ i ].Text := VarAsType( ar[i]/oLayer.SelectedList.Count, varDate )
        else
          vals[ i ].Text := VarAsType( ar[i], varDate ) ;
      end ;
    end ;

  end ;

  procedure T_ScrollBox.SetShape(
    _shape : TGIS_Shape
  ) ;
  begin
    oShape := _shape ;
    setHeader ;
  end ;

  procedure T_ScrollBox.ResetViewer ;
  begin
    oViewer := nil ;
  end ;

  function T_ScrollBox.UpdateLayout : Boolean ;
  var
    attr : TGIS_ControlAttributes ;
    i   : Integer ;
    lbl   : TLabel   ;
    cntrl : TControl ;
    left_column_x : Integer ;
    left_column_width : Integer ;
    right_column_x : Integer ;
    min_width : Integer ;
    h   : Integer ;
  begin
    if BiDiMode = bdRightToLeft then
      oPanelLeft.Align := alRight
    else
      oPanelLeft.Align := alLeft ;

    oPanelRight.Align := alClient ;

    Result := False ;
    attr := TGIS_ControlAttributes( Parent ) ;
    // size & style
    Ctl3d := attr.Ctl3D ;
    Font.Assign( attr.Font ) ;
    oPanel.Ctl3d := attr.Ctl3D ;
    oPanel.Font.Assign( attr.Font ) ;
    oPanelLeft.Ctl3d := attr.Ctl3D ;
    oPanelLeft.Font.Assign( attr.Font ) ;
    oPanelRight.Ctl3d := attr.Ctl3D ;
    oPanelRight.Font.Assign( attr.Font ) ;

    if ( assigned( oShape ) or assigned( oLayer ) ) and
       ( ( ( eLayout = TGIS_LayoutType.OneColumn ) and
           ( oPanelRight.ComponentCount > 0 ) ) or
         ( ( eLayout = TGIS_LayoutType.TwoColumns ) and
           ( oPanelLeft.ComponentCount > 0 ) and
           ( oPanelRight.ComponentCount > 0 ) ) ) then begin

      if eLayout = TGIS_LayoutType.OneColumn then begin
        cntrl := oPanelRight.Controls[0] ;
        TLabel(cntrl).Font.Style := [ fsBold ] ;
        PlaceControl(BiDiMode, nil, cntrl, GGAP, cntrl.Width);
      end
      else
      begin
        cntrl := oPanelLeft.Controls[0] ;
        TLabel(cntrl).Font.Style := [ fsBold ] ;
        PlaceControl(BiDiMode, nil, cntrl, GGAP, cntrl.Width);

        cntrl := oPanelRight.Controls[0] ;
        TLabel(cntrl).Font.Style := [ fsBold ] ;
        PlaceControl(BiDiMode, nil, cntrl, GGAP, cntrl.Width)

      end ;
      h := cntrl.Top + cntrl.Height + 2*VGAP ;

      Result := True ;
      // locate controls on the panel
      min_width := 30 ;
      left_column_x := GGAP ;
      left_column_width := attr.FieldNameColumnWidth ;
      if eLayout = TGIS_LayoutType.OneColumn then
        right_column_x := left_column_x + min_width
      else
        right_column_x := left_column_x + left_column_width + GGAP ;
      if eLayout = TGIS_LayoutType.OneColumn then begin
        for i := 2 to oPanelRight.ControlCount div 2 do begin
          cntrl := oPanelRight.Controls[i] ;
          if not ( cntrl is TLabel )  then
            continue ;
          cntrl.Top := h ;
          PlaceControl(BiDiMode, nil, cntrl, GGAP, -1);
          h := h + cntrl.Height ;
          cntrl := TLabel(cntrl).FocusControl ;
          cntrl.Top := h ;
          h := h + cntrl.Height + VGAP ;
        end ;
        if bReadOnly then
          h := h + GGAP - VGAP ;
        oPanel.Height := h ;
        oPanel.Width := Max( ClientWidth, apScale(right_column_x + min_width + GGAP) ) ;
        oPanelLeft.Width := apScale(GGAP + attr.FieldNameColumnWidth +
                                    GGAP - oSplitter.Width) ;
      end
      else begin
        for i := 1 to oPanelLeft.ControlCount - 1 do begin
          lbl   := TLabel(oPanelLeft.Controls[i]) ;
          cntrl := oPanelRight.Controls[i] ;
          PlaceControl(BiDiMode, nil, lbl, GGAP, lbl.Width);
          // field name
          if cntrl.Height > lbl.Height then
            lbl.Top  := h + (cntrl.Height - lbl.Height) div 2
          else
            lbl.Top  := h ;
          // field value
          cntrl.Top  := h ;
          PlaceControl(BiDiMode, nil, cntrl, GGAP, oPanelRight.ClientWidth - (2 * GGAP));
          h := h + cntrl.Height + apScale(1) ;
        end ;
        if bReadOnly then
          h := h + GGAP ;
        oPanel.Height := h ;
        oPanel.Width := Max( ClientWidth, apScale(right_column_x + min_width + GGAP) ) ;
        oPanelLeft.Width := apScale(GGAP + attr.FieldNameColumnWidth +
                                    GGAP - oSplitter.Width) ;
      end ;
      if eLayout = TGIS_LayoutType.OneColumn then begin
        oPanelLeft.Visible := False ;
        oSplitter.Visible := False ;
      end
      else begin
        oPanelLeft.Visible := True ;
        oSplitter.Visible := True ;
      end ;
    end ;

  end ;

  procedure T_ScrollBox.doLabelClick(
    _sender : TObject
  ) ;
  var
    attr : TGIS_ControlAttributes ;
  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    if assigned( attr.OnClick ) then
      attr.OnClick( Self ) ;
  end ;

  procedure T_ScrollBox.doLabelDblClick(
    _sender : TObject
  ) ;
  var
    attr : TGIS_ControlAttributes ;
  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    if assigned( attr.OnDblClick ) then
      attr.OnDblClick( Self ) ;
  end ;

  procedure T_ScrollBox.doLabelMouseDown(
    _sender : TObject ;
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x, _y  : Integer
  ) ;
  var
    attr : TGIS_ControlAttributes ;
    fld_name : String ;
    new_item : TMenuItem ;
    pt : TPoint ;

    function get_fld_name : String ;
    var
      i : Integer ;
      y : Integer ;
      lbl  : TLabel ;
      cntrl : TControl ;
    begin
      if _sender is TLabel then begin
        Result := TLabel(_sender).Caption ;
        if eLayout = TGIS_LayoutType.OneColumn then begin
          if BiDiMode = TBiDiMode.bdRightToLeft then begin
            if Result.Contains( ' : ' + _rsrc(GIS_RS_ATTRIBUTES_DLG_UID) ) then
              Result := _rsrc(GIS_RS_ATTRIBUTES_DLG_UID) ;
          end
          else begin
            if Result.Contains( _rsrc(GIS_RS_ATTRIBUTES_DLG_UID) + ' : ' ) then
              Result := _rsrc(GIS_RS_ATTRIBUTES_DLG_UID) ;
          end;
        end
      end
      else begin
        y := oPanelRight.Controls[1].Top ;
        if _y < y then
          Result := _rsrc(GIS_RS_ATTRIBUTES_DLG_UID)
        else
          for i := 1 to oPanelLeft.ControlCount - 1 do begin
            lbl := TLabel(oPanelLeft.Controls[i]) ;
            cntrl := oPanelRight.Controls[i] ;
            if ( y + cntrl.Height ) >= _y then begin
              Result := lbl.Caption ;
              break ;
            end;
            y := y + cntrl.Height ;
          end;
      end;
    end ;

  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    if assigned( attr.OnMouseDown ) then
      attr.OnMouseDown( Self, _button, _shift, _x, _y ) ;
    oField := '' ;
    if _button = mbRight then
    begin
      fld_name := get_fld_name ;
      oPopup.Items.Clear ;
      if attr.AllowRestructure and
         not attr.ReadOnly     and
         assigned( oShape )    and
         ( String.Compare( fld_name, _rsrc(GIS_RS_ATTRIBUTES_DLG_UID) ) <> 0 ) then begin

        oField := fld_name ;

        new_item := TMenuItem.Create( oPopup ) ;
        oPopup.Items.Add( new_item ) ;
        new_item.Caption := _rsrc(GIS_RS_ATTRIBUTES_MNUADD) ;
        new_item.OnClick := doAddField ;

        new_item := TMenuItem.Create( oPopup ) ;
        oPopup.Items.Add( new_item ) ;
        new_item.Caption := _rsrc(GIS_RS_ATTRIBUTES_MNUMODIFY) ;
        if isPredefined( fld_name ) then
          new_item.Enabled := False
        else begin
          new_item.Enabled := True ;
          new_item.OnClick := doModifyField ;
        end ;

        new_item := TMenuItem.Create( oPopup ) ;
        oPopup.Items.Add( new_item ) ;
        new_item.Caption := _rsrc(GIS_RS_ATTRIBUTES_MNUDELETE) ;
        if isPredefined( fld_name ) then
          new_item.Enabled := False
        else begin
          new_item.Enabled := True ;
          new_item.OnClick := doDeleteField ;
        end ;

        new_item := TMenuItem.Create( oPopup ) ;
        new_item.Caption := '-' ;
        oPopup.Items.Add( new_item ) ;

        new_item := TMenuItem.Create( oPopup ) ;
        oPopup.Items.Add( new_item ) ;
        new_item.Caption := _rsrc(GIS_RS_ATTRIBUTES_MNUCOPYTOCLIPBOARD) ;
        new_item.OnClick := doCopyToClipboard ;
        oPopup.InsertComponent( TMenuItem.Create( nil ) );

      end
      else begin

        new_item := TMenuItem.Create( oPopup ) ;
        oPopup.Items.Add( new_item ) ;
        new_item.Caption := _rsrc(GIS_RS_ATTRIBUTES_MNUCOPYTOCLIPBOARD) ;
        new_item.OnClick := doCopyToClipboard ;
        oPopup.InsertComponent( TMenuItem.Create( nil ) );

      end;
      pt := TControl(_sender).ClientToScreen( Point( _x, _y ) ) ;
      oPopup.Popup( pt.x, pt.y ) ;
    end ;
  end ;

  procedure T_ScrollBox.doLabelMouseUp(
    _sender : TObject ;
    _button : TMouseButton ;
    _shift  : TShiftState ;
    _x, _y  : Integer
  ) ;
  var
    attr : TGIS_ControlAttributes ;
  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    if assigned( attr.OnMouseUp ) then
      attr.OnMouseUp( Self, _button, _shift, _x, _y ) ;
  end ;

  procedure T_ScrollBox.doLabelMouseMove(
    _sender : TObject ;
    _shift  : TShiftState ;
    _x, _y  : Integer
  ) ;
  var
    attr : TGIS_ControlAttributes ;
  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    if assigned( attr.OnMouseMove ) then
      attr.OnMouseMove( Self, _shift, _x, _y ) ;
  end ;

  procedure T_ScrollBox.doValueClick(
    _sender : TObject
  ) ;
  var
    attr : TGIS_ControlAttributes ;
  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    if assigned( attr.OnClick ) then
      attr.OnClick( Self ) ;
  end;

  procedure T_ScrollBox.doValueContextPopup(
        _sender  : TObject ;
        _mpos    : TPoint ;
    var _handled : Boolean
  ) ;
  var
    attr : TGIS_ControlAttributes ;
  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    if assigned( attr.OnContextPopup ) then
      attr.OnContextPopup( Self, _mpos, &_handled ) ;
  end ;

  procedure T_ScrollBox.doValueKeyDown(
        _sender : TObject ;
    var _key    : Word    ;
        _shift  : TShiftState
  ) ;
  var
    attr : TGIS_ControlAttributes ;
  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    if assigned( attr.OnKeyDown ) then
      attr.OnKeyDown( Self, &_key, _shift ) ;
  end ;

  procedure T_ScrollBox.doValueKeyPress(
        _sender : TObject ;
    var _key    : Char
  ) ;
  var
    attr : TGIS_ControlAttributes ;
  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    if assigned( attr.OnKeyPress ) then
      attr.OnKeyPress( Self, &_key ) ;
  end ;

  procedure T_ScrollBox.doValueKeyUp(
        _sender : TObject ;
    var _key    : Word    ;
        _shift  : TShiftState
  ) ;
  var
    attr : TGIS_ControlAttributes ;
  begin
    attr := TGIS_ControlAttributes( Parent ) ;
    if assigned( attr.OnKeyUp ) then
      attr.OnKeyUp( Self, &_key, _shift ) ;
  end ;

  procedure T_ScrollBox.doSplitterMoved(
    _sender : TObject
  ) ;
  var
    pnl_width_96 : Integer ;
  begin
    if iPPI <> 96 then begin
      pnl_width_96 := Floor( oPanelLeft.ClientWidth * 96 / iPPI ) ;
      TGIS_ControlAttributes( Parent ).FieldNameColumnWidth :=
        pnl_width_96 - GGAP - (GGAP-oSplitter.Width)
    end else
      TGIS_ControlAttributes( Parent ).FieldNameColumnWidth :=
        oPanelLeft.ClientWidth - GGAP - (GGAP-oSplitter.Width);
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
    try
      if frm.ShowAdd( oLayer, pOnHelp ) = mrOK then begin
        oLayer.AddField( frm.XField.NewName , frm.XField.FieldType,
                         frm.XField.NewWidth, frm.XField.NewDecimal
                       ) ;
        TGIS_ControlAttributes( Parent ).Invalidate ;
      end ;
    finally
      FreeObject( frm ) ;
    end ;
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
    try
      if frm.ShowModify( oLayer, oField, pOnHelp ) = mrOK then begin
        oLayer.RenameField( oField,
                            frm.XField.NewName ,
                            frm.XField.NewWidth, frm.XField.NewDecimal
                          ) ;
        TGIS_ControlAttributes( Parent ).Invalidate ;
      end ;

    finally
      FreeObject( frm ) ;
    end ;
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
    try
      if frm.ShowDelete( oLayer, oField, pOnHelp ) = mrOK then begin
        oLayer.DeleteField( oField );
        TGIS_ControlAttributes( Parent ).Invalidate ;
      end ;

    finally
      FreeObject( frm ) ;
    end ;
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
    cntrl_name : String ;
    fld_name   : String ;
    fld_format : String ;
    fld_id     : Integer ;
    fld        : TGIS_FieldInfo ;
    attr       : TGIS_ControlAttributes ;
    wasjoin    : Boolean ;

    procedure set_text(
      const _caption : String
    ) ;
    var
      i   : Integer ;
      cbx : TComboBox ;
    begin
      if cntrl is TCheckBox then begin
        TCheckBox(cntrl).Caption := _caption ;
      end
      else if cntrl is TEdit then begin
        TEdit(cntrl).Text := _caption ;
      end
      else if cntrl is TMemo then begin
        TMemo(cntrl).Text := _caption ;
      end
      else if cntrl is TComboBox then begin
        cbx := TComboBox( cntrl ) ;
        for i := 0 to cbx.Items.Count-1 do
          if cbx.Items[i] = _caption then begin
            cbx.ItemIndex := i ;
            break ;
          end ;
      end
      else if cntrl is TDateTimePicker then begin
        if not IsStringEmpty( _caption ) then begin
          TDateTimePicker(cntrl).DateTime := StrToDateTime( _caption ) ;
        end
        else begin
          TDateTimePicker(cntrl).DateTime := GisDefaultField( TGIS_FieldType.Date ) ;
          TDateTimePicker(cntrl).Format := ' ';
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
      if cntrl is TDateTimePicker then begin
        TDateTimePicker(cntrl).Format := T_DateTimePicker(cntrl).oldFormat ;
        TDateTimePicker(cntrl).DateTime := _v
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
    for i := 0 to oPanelRight.ControlCount - 1 do begin

      cntrl := oPanelRight.Controls[i] ;
      if cntrl is TLabel then
        continue ;

      cntrl_name := cntrl.Name;
      if isPredefined(cntrl_name) then begin
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
            TCheckBox(cntrl).Checked := false
          else
            TCheckBox(cntrl).Checked := Boolean(v) ;
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
        if cntrl is TEdit then
          TEdit(cntrl).Color := clValid
        else if cntrl is TMemo then
          TMemo(cntrl).Color := clValid ;
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
    for i := 0 to oPanelRight.ControlCount - 1 do begin

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
          oShape.SetField( fld.NewName, TEdit(cntrl).Text ) ;
      end
      else if cntrl is TMemo then
        oShape.SetField( fld.NewName, TMemo(cntrl).Text )
      else if cntrl is TComboBox then
        oShape.SetField( fld.NewName, TComboBox(cntrl).Text )
      else if cntrl is TCheckBox then
        oShape.SetField( fld.NewName, TCheckBox(cntrl).Checked )
      else if cntrl is TDateTimePicker then begin
        if TDateTimePicker(cntrl).DateTime <> GisDefaultField( TGIS_FieldType.Date ) then
          oShape.SetField( fld.NewName, TDateTimePicker(cntrl).DateTime ) ;
      end ;
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
  begin
    if oPanelLeft.ControlCount > 1 then
    begin
      sb := TStringBuilder.Create ;
      try
        for i := 1 to oPanelLeft.ControlCount - 1 do
        begin
          sb.Append( TLabel(oPanelLeft.Controls[i]).Caption ) ;
          sb.Append( TAB ) ;
          cntrl := oPanelRight.Controls[i] ;
          if cntrl is TEdit then
            sb.Append( TEdit(cntrl).Text )
          else if cntrl is TMemo then
            sb.Append( TMemo(cntrl).Text )
          else if cntrl is TComboBox then
            sb.Append( TComboBox(cntrl).Text )
          else if cntrl is TCheckBox then
          begin
            if TCheckBox(cntrl).Checked then
              sb.Append( '1' )
            else
              sb.Append( '0' ) ;
          end;
          sb.Append( CR ) ;
        end ;
        Clipboard.AsText := sb.ToString ;
      finally
        FreeObject( sb ) ;
      end;
    end ;
  end ;

  function T_ScrollBox.apScale(
    const _size : Integer
  ) : Integer ;
  begin
    {$IFDEF LEVEL_RX101_VCL}
      Result := Floor( _size*iPPI/96.0 ) ;
    {$ELSE}
      Result := _size ;
    {$ENDIF}
  end ;


//==============================================================================
// TGIS_ControlAttributes
//==============================================================================

  function TGIS_ControlAttributes.rebuildFldList(
    _layer : TGIS_LayerVector
  ) : Boolean ;
  begin
    if ( T_ScrollBox(oGrid).Viewer = nil ) or
       ( T_ScrollBox(oGrid).Layer  = nil ) or
       ( T_ScrollBox(oGrid).Shape  = nil ) or
       ( _layer <> T_ScrollBox(oGrid).Layer ) or
       recreate then begin
      Result := True ;
      recreate := False ;
    end else
      Result := False ;
  end ;

  function TGIS_ControlAttributes.rebuildStatList(
    _layer : TGIS_LayerVector
  ) : Boolean ;
  begin
    if ( T_ScrollBox(oGrid).Viewer = nil ) or
       ( T_ScrollBox(oGrid).Layer  = nil ) or
       ( T_ScrollBox(oGrid).Shape <> nil ) or
       ( _layer <> T_ScrollBox(oGrid).Layer ) or
       recreate then begin
      Result := True ;
      recreate := False ;
    end else
      Result := False ;
  end ;

  procedure TGIS_ControlAttributes.initiateFldList(
    _layer : TGIS_LayerVector ;
    _shape : TGIS_Shape
  ) ;
  begin
     if FBiDiModeFromTranslation then begin
      if _rsbidi then begin
        Self.BiDiMode := TBiDiMode.bdRightToLeft
      end
      else
      begin
        Self.BiDiMode := TBiDiMode.bdLeftToRight
      end;
    end;

    T_Scrollbox(oGrid).BiDiMode := BiDiMode ;

    if assigned( T_ScrollBox(oGrid).Viewer ) then
      T_ScrollBox(oGrid).Viewer.UnSubscribe( Self ) ;

    if rebuildFldList( _layer ) then begin
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
    end else
      T_ScrollBox(oGrid).SetShape( _shape ) ;

    if assigned( T_ScrollBox(oGrid).Viewer ) then
      T_ScrollBox(oGrid).Viewer.Subscribe( Self ) ;
  end ;

  procedure TGIS_ControlAttributes.initiateStatList(
    _layer : TGIS_LayerVector
  ) ;
  begin
    if assigned( T_ScrollBox(oGrid).Viewer ) then
      T_ScrollBox(oGrid).Viewer.UnSubscribe( Self ) ;

    if rebuildStatList( _layer ) then begin
      T_ScrollBox(oGrid).Visible := False ;
      T_ScrollBox(oGrid).ClearFields ;
      T_ScrollBox(oGrid).InitiateStatistics(
        _layer, True
      )
    end else
      T_ScrollBox(oGrid).InitiateStatistics(
        _layer, False
      ) ;

    if assigned( T_ScrollBox(oGrid).Viewer ) then
      T_ScrollBox(oGrid).Viewer.Subscribe( Self ) ;
  end ;

  procedure TGIS_ControlAttributes.fset_FieldNameColumnWidth(
    const _value : Integer
  ) ;
  begin
    if _value <> FFieldNameColumnWidth then begin
      FFieldNameColumnWidth := _value ;
    end ;
  end ;

  procedure TGIS_ControlAttributes.fset_IgnoreFldxDefinition(
    const _value  : Boolean
  ) ;
  begin
    if _value <> FIgnoreFldxDefinition then begin
      FIgnoreFldxDefinition := _value ;
      recreate := True ;
    end ;
  end ;

  procedure TGIS_ControlAttributes.fset_ShowVirtualFields(
    const _value : Boolean
  ) ;
  begin
    if _value <> FShowVirtualFields then begin
      FShowVirtualFields := _value ;
      recreate := True ;
    end ;
  end ;

  procedure TGIS_ControlAttributes.fset_LayoutType(
    const _value : TGIS_LayoutType
  ) ;
  begin
    if _value <> FLayoutType then begin
      FLayoutType := _value ;
      recreate := True ;
    end ;
  end ;

  procedure TGIS_ControlAttributes.fset_BiDiModeFromTranslation(
    const _value : Boolean
  );
  begin
    if _value <> FBiDiModeFromTranslation then begin
      FBiDiModeFromTranslation := _value ;
      ParentBiDiMode := not FBiDiModeFromTranslation ;
    end;
  end;

  procedure TGIS_ControlAttributes.fset_ParentBiDiMode(
    const _value : Boolean
  );
  begin
    if _value <> ParentBiDiMode then begin
      inherited ParentBiDiMode := _value ;
      if ParentBiDiMode = true then
        FBiDiModeFromTranslation := not ParentBiDiMode ;
    end;
  end;

  procedure TGIS_ControlAttributes.fset_ReadOnly(
    const _value : Boolean
  ) ;
  begin
    if _value <> FReadOnly then begin
      FReadOnly := _value ;
      recreate := True ;
    end ;
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
    Update ;
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

    Update ;
  end ;

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
    _sender  : TObject      ;
    var _key : Word         ;
    _shift   : TShiftState
  ) ;
  begin
    if      _key = VK_ESCAPE then doCancel( _sender )
    else if _key = VK_RETURN then doOk    ( _sender )
    else if _key = VK_F1     then doHelp  ( _sender ) ;
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
  end ;

  procedure TGIS_ControlAttributes.doMouseWheel(
    _sender      : TObject ;
    _shift       : TShiftState ;
    _wheelDelta  : Integer ;
    _mousePos    : TPoint ;
    var _handled : Boolean
  ) ;
  var
    new_val : Integer  ;
    old_val : Integer  ;
  begin
    _handled := false;
    if PtInRect( ClientRect, ScreenToClient( _mousePos ) ) then begin
      _handled := True;

      old_val := T_ScrollBox(oGrid).VertScrollBar.Position ;
      new_val := old_val - _wheelDelta ;

      // scroll window
      if new_val > T_ScrollBox(oGrid).VertScrollBar.Range then
        new_val := T_ScrollBox(oGrid).VertScrollBar.Range ;
      if new_val < 0 then
        new_val := 0 ;

      T_ScrollBox(oGrid).VertScrollBar.Position := new_val ;
    end ;
  end ;

  procedure TGIS_ControlAttributes.updateLayout ;
  var
    v   : Boolean ;
    ww  : Integer ;
    hh  : Integer ;
    any_button : Boolean ;
    any_shape  : Boolean ;
    w   : Integer ;
  begin
    if not bCreated then exit ;

    updatePPI ;

    any_button := FShowBtnOk or FShowBtnCancel ;

    if any_button then begin

      ww := Max( Canvas.TextWidth( oBtnOk.Caption     ),
                 Canvas.TextWidth( oBtnCancel.Caption )
               ) + apScale(14) ;
      hh := Max( Canvas.TextHeight( oBtnOk.Caption     ),
                 Canvas.TextHeight( oBtnCancel.Caption )
               ) + apScale(6) ;

      if FShowBtnOk then begin
        oBtnOk.Width  := ww  ;
        oBtnOk.Height := hh  ;
      end
      else begin
        oBtnOk.Width  := 0 ;
        oBtnOk.Height := 0 ;
      end ;
      if FShowBtnCancel then begin
        oBtnCancel.Width  := ww ;
        oBtnCancel.Height := hh ;
      end
      else begin
        oBtnCancel.Width  := 0 ;
        oBtnCancel.Height := 0 ;
      end ;

      w := oBtnCancel.Width;
      if FShowBtnOk and FShowBtnCancel then
        w := w + apScale( HGAP ) ;
      w := w + oBtnOk.Width ;

      if FShowBtnOk then
        PlaceControl( BiDiMode, nil, oBtnOk, ( ClientWidth - w ) div 2, oBtnOk.Width ) ;
      if FShowBtnCancel then
        PlaceControl( BiDiMode, nil, oBtnCancel,
                      ( ClientWidth - w ) div 2 + w - oBtnCancel.Width,
                      oBtnCancel.Width ) ;

    end
    else begin
      hh := 0 ;
      oBtnOk.Width := 0 ;
      oBtnOk.Height := 0 ;
      oBtnCancel.Width := 0 ;
      oBtnCancel.Height := 0 ;
    end ;

    oErrorMessage.Font.Size := 9 ;
    PlaceControl( BiDiMode, nil, oErrorMessage, apScale(HGAP) div 2, -1 ) ;

    any_shape := False ;
    v := T_ScrollBox(oGrid).Visible ;
    try
      T_ScrollBox(oGrid).Top := 0 ;
      // cannot set Height
      if FReadOnly then
        T_ScrollBox(oGrid).Height := ClientHeight
      else if any_button then
        T_ScrollBox(oGrid).Height := ClientHeight - apScale(VGAP) - hh - apScale(VGAP) -
                                     oErrorMessage.Height - 2*apScale(VGAP)
      else
        T_ScrollBox(oGrid).Height := ClientHeight - apScale(VGAP) -
                                     oErrorMessage.Height - 2*apScale(VGAP) ;
      T_ScrollBox(oGrid).Width := ClientWidth ;
      any_shape := T_ScrollBox(oGrid).UpdateLayout ;
    finally
      if any_shape then
        T_ScrollBox(oGrid).Visible := True
      else
        T_ScrollBox(oGrid).Visible := v ;
    end ;

    oErrorMessage.Visible := not FReadOnly ;
    oBtnCancel.Visible    := FShowBtnCancel and not FReadOnly ;
    oBtnOk.Visible        := FShowBtnOk and not FReadOnly ;

    oBtnCancel.Top := ClientHeight - hh - apScale(VGAP) ;

    oBtnOk.Top     := oBtnCancel.Top ;
    if any_button then
      oErrorMessage.Top := oBtnOk.Top - apScale(VGAP) - oErrorMessage.Height
    else
      oErrorMessage.Top := ClientHeight - apScale(VGAP) - oErrorMessage.Height ;
  end ;

  function TGIS_ControlAttributes.apScale(
    const _size : Integer
  ) : Integer ;
  begin
    {$IFDEF LEVEL_RX101_VCL}
      Result := Floor( _size*iPPI/96.0 ) ;
    {$ELSE}
      Result := _size ;
    {$ENDIF}
  end ;

  procedure TGIS_ControlAttributes.updatePPI ;
  {$IFDEF LEVEL_RX101_VCL}
    var
      frm : TCustomForm ;
      ppi : Integer ;
    begin
      ppi := Screen.PixelsPerInch;
      frm := GetParentForm( self, True ) ;
      if Assigned( frm ) then
        ppi := frm.Monitor.PixelsPerInch;

      if ppi <> iPPI then  begin
        iPPI := ppi ;
        T_ScrollBox(oGrid).PPI := ppi ;
      end ;
    end ;
  {$ELSE}
    begin
      // do nothing
    end ;
  {$ENDIF}

  procedure TGIS_ControlAttributes.unsubscribeFromViewer ;
  begin
    if assigned( T_ScrollBox(oGrid).Viewer ) then
      T_ScrollBox(oGrid).Viewer.UnSubscribe( Self ) ;
  end ;

  procedure TGIS_ControlAttributes.Paint ;
  begin
    if not bCreated then exit ; // ActiveX bug

    // size & style
    oBtnOk.Font.Assign( Font ) ;
    oBtnCancel.Font.Assign( Font ) ;

    inherited ;

  end ;

  constructor TGIS_ControlAttributes.Create( _aowner : TComponent ) ;
  begin
    bCreated := False ;
    bCancel  := False ;
    iPPI     := 96 ;

    inherited Create( _aowner ) ;

    BiDiMode := TBiDiMode.bdLeftToRight;

    if GisIsMetricSystem then
      FUnits  := CSUnitsList.ByEPSG( 904201 )
    else
      FUnits  := CSUnitsList.ByEPSG( 904202 ) ;

    FLayoutType := TGIS_LayoutType.TwoColumns ;
    FFieldNameColumnWidth := 80 ;

    DoubleBuffered := not IsWin11 ;
    OnResize       := doResize ;
    ParentColor := False ;
    ParentBackground := False ;

    BorderStyle := bsNone ;
    BevelOuter  := bvNone ;
    ShowCaption := False ;

    oGrid := T_ScrollBox.Create( Self ) ;
    T_ScrollBox(oGrid).Parent    := Self ;
    OnKeyDown := doKeyDown ;

    oErrorMessage := TLabel.Create( Self ) ;
    with oErrorMessage do begin
      Parent      := Self  ;
      Visible     := False ;
      Text        := ''    ;
      ParentColor := False ;
      ParentFont  := False ;
      Font.Color  := clRed ;
      Color       := clRed ;
      Font.Size   := 9 ;
      // classic windows theme error
      Transparent := False ;
      Transparent := True  ;
    end ;

    {$IFNDEF ACTIVEX}
      oBtnOk := TButton.Create( Self );
    {$ELSE}
      oBtnOk := TButton.CreateParented( Self.Handle );
    {$ENDIF}
    with oBtnOk do begin
      Parent     := Self  ;
      OnClick    := doOk  ;
      Enabled    := False ;
      ParentFont := False ;
      Caption    := _rsrc( GIS_RS_BTN_OK ) ;
    end ;
    FShowBtnOk := True ;

    {$IFNDEF ACTIVEX}
      oBtnCancel := TButton.Create( Self );
    {$ELSE}
      oBtnCancel := TButton.CreateParented( Self.Handle );
    {$ENDIF}
    with oBtnCancel do begin
      Parent     := Self     ;
      OnClick    := doCancel ;
      Enabled    := False    ;
      ParentFont := False    ;
      Caption    := _rsrc( GIS_RS_BTN_CANCEL ) ;
    end ;
    FShowBtnCancel := True     ;

    FReadOnly         := False ;
    FAllowNull        := False ;
    FAllowRestructure := True  ;

    FShowVirtualFields := True ;
    FVirtualFields := [ TGIS_VirtualField.GisSelected,
                        TGIS_VirtualField.GisHidden,
                        TGIS_VirtualField.GisArea,
                        TGIS_VirtualField.GisLength ];

    Width  := 150 ;
    Height := 100 ;

    pOnHelp := nil ;

    bCreated := True ;
    recreate := False ;
    uponDestroy := False ;
    OnMouseWheel := doMouseWheel ;
  end ;

  destructor TGIS_ControlAttributes.Destroy ;
  begin
    unsubscribeFromViewer ;
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
        oErrorMessage.Caption := msg ;
        exit ;
      end ;
    end ;
    oErrorMessage.Caption := '' ;
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
       ( _sender is TMemo ) then begin
      cntrl := TControl(_sender) ;
      if not T_ScrollBox(oGrid).ValidateTextBox(
        cntrl, TGIS_FieldValueCheckMode.OnEdit, msg ) then begin
        oErrorMessage.Caption := msg ;
        oBtnOk.Enabled := False ;
        exit ;
      end ;
    end ;

    if (_sender is TCheckBox) then
      T_ScrollBox(oGrid).doValueClick( _sender ) ;

    oErrorMessage.Caption := '' ;
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
    recreate := True ;
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
    if _shape = nil then begin
      Clear ;
    end
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
    const _shape    : TGIS_Shape
  ) ;
  begin
    if _shape = nil then begin
      Clear ;
    end
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
    end
    else begin
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
    oBtnOk.Caption     := _rsrc( GIS_RS_BTN_OK     ) ;
    oBtnCancel.Caption := _rsrc( GIS_RS_BTN_CANCEL ) ;

    bCancel := True ;
    try
      if not Assigned( T_ScrollBox(oGrid).Shape ) then exit ;
      T_ScrollBox(oGrid).CancelChanges ;
    finally
      bCancel := False ;
      oBtnOk.Enabled := False ;
      oBtnCancel.Enabled := False ;
      oErrorMessage.Caption := '';
    end ;
  end ;

  function TGIS_ControlAttributes.SaveChanges : Boolean ;
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
        end ;
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
      oErrorMessage.Caption := msg ;
      oBtnOk.Enabled := False ;
      exit ;
    end ;
    oErrorMessage.Caption := '' ;
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

