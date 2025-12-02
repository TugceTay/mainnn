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
  Combo box helper.
}

unit FMX.GisComboBoxHelper;
{$HPPEMIT '#pragma link "FMX.GisComboBoxHelper"}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Types,
  System.UITypes,
  System.Math,
  FMX.Types,
  FMX.Forms,
  FMX.Graphics,
  FMX.Objects,
  FMX.Styles.Objects,
  FMX.Controls,
  FMX.Pickers,
  FMX.ListBox,

  GisTypesUI,
  FMX.GisComboBox;

type

  /// <summary>
  ///   Interface for persistent form operations
  /// </summary>
  IGIS_PersistentForm = interface

    /// <summary>
    ///   Return item key.
    /// </summary>
    /// <returns>
    ///   item key
    /// </returns>
    /// <remarks>
    ///   Each registered persistent item has an unique key which represent
    ///   item storage.
    /// </remarks>
    function itemKey   : String ;

    /// <summary>
    ///   Return item value .
    /// </summary>
    /// <returns>
    ///   item value
    /// </returns>
    function itemValue : String ;
  end ;

  /// <summary>
  ///   Event to be fired upon saving object state.
  /// </summary>
  /// <param name="_key">
  ///   unique object identifier
  /// </param>
  /// <param name="_value">
  ///   object state as a string
  /// </param>
  TGIS_PersistentFormSaveEvent = procedure(
    const _key   : String ;
    const _value : String
  ) of object ;

  /// <summary>
  ///   Event to be fired upon loading object state.
  /// </summary>
  /// <param name="_key">
  ///   unique object identifier
  /// </param>
  /// <returns>
  ///   state value
  /// </returns>
  TGIS_PersistentFormLoadEvent = function(
    const _key   : String
  ) : String of object ;


  TGIS_PersistentFormHelper = class ;


  /// <summary>
  ///   Helper for centralized storage of form objects.
  /// </summary>
  /// <remarks>
  ///   To be used within TGIS_ControlLegendForm
  /// </remarks>
  TGIS_PersistentForm = class
    private
      eloadEvent : TGIS_PersistentFormLoadEvent ;
      esaveEvent : TGIS_PersistentFormSaveEvent ;
    private
      lstSubscribers : TList<TGIS_PersistentFormHelper> ;
    public

      /// <summary>
      ///   Saving state of all registered (inherited from
      ///   TGIS_PersistentFormHelper) objects.
      /// </summary>
      procedure SaveState ;

      /// <summary>
      ///   Global Instance of object.
      /// </summary>
      /// <returns>
      ///   instance
      /// </returns>
      class function Instance : TGIS_PersistentForm ;

      /// <summary>
      ///   Destroy an instance
      /// </summary>
      destructor Destroy ; override;

    published //events
      /// <event/>
      /// <summary>
      ///   Event to be fired upon loading state.
      /// </summary>
      property LoadEvent : TGIS_PersistentFormLoadEvent
                           read  eloadEvent
                           write eloadEvent ;

      /// <event/>
      /// <summary>
      ///   Event to be fired upon saving state.
      /// </summary>
      property SaveEvent : TGIS_PersistentFormSaveEvent
                           read  esaveEvent
                           write esaveEvent ;
  end ;

  /// <summary>
  ///   Base class for form object persistency helpers. Manage relationship
  ///   with global TGIS_PersistentFormHelper for centralized store/restore of
  ///   persistent state
  /// </summary>
  TGIS_PersistentFormHelper = class( TInterfacedObject, IGIS_PersistentForm )
    private
      spersistancyKey : String ;
    private
      function itemKey   : String ;
      function itemValue : String ;
    protected
      function  fget_State  : String ; virtual; abstract;
      procedure fset_State  ( const _value : String ) ; virtual; abstract;
    public
      /// <summary>
      ///   Create an instance
      /// </summary>
      /// <param name="_key">
      ///   unique key under which object will be identified in a storage.
      /// </param>
      constructor Create    ( const _key : String ) ; virtual;
      /// <summary>
      ///   Destroy an instance
      /// </summary>
      destructor  Destroy   ; override;
    public
      /// <summary>
      ///   Procedure to be called to restore state.
      /// </summary>
      procedure   LoadState ;
    public
      /// <summary>
      ///   Unique key under which object will be identified in a storage.
      /// </summary>
      property PeristencyKey : String read spersistancyKey ;
      /// <summary>
      ///   State text.
      /// </summary>
      property State         : String read fget_State write fset_State ;
  end ;

  /// <summary>
  ///   Event to be fired upon setting a Value.
  /// </summary>
  /// <param name="_sender">
  ///   event originator
  /// </param>
  /// <param name="_sender">
  ///   event originator
  /// </param>
  /// <param name="_value">
  ///   value to be set
  /// </param>
  /// <returns>
  ///   Canonical string prepared by PrepareItem.
  /// </returns>
  TGIS_ComboBoxHelperValueEvent = function(
    _sender  : TObject ;
    _value   : String
  ) : String of object ;

  /// <summary>
  ///   Event to be fired upon selecting a custom item.
  /// </summary>
  /// <param name="_sender">
  ///   event originator
  /// </param>
  /// <param name="_sender">
  ///   event originator
  /// </param>
  /// <param name="_value">
  ///   custom event identifier
  /// </param>
  /// <returns>
  ///   Canonical string prepared by PrepareItem.
  /// </returns>
  TGIS_ComboBoxHelperCustomEvent = function(
    _sender  : TObject ;
    _value   : String
  ) : String of object ;

  /// <summary>
  ///   Event to be fired upon selecting a bitmap item.
  /// </summary>
  /// <param name="_sender">
  ///   event originator
  /// </param>
  /// <param name="_value">
  ///   event identifier
  /// </param>
  /// <param name="_color">
  ///   foreground color
  /// </param>
  /// <param name="_font">
  ///   font to be used
  /// </param>
  /// <param name="_width">
  ///   bitmap width
  /// </param>
  /// <param name="_height">
  ///   bitmap height
  /// </param>
  /// <returns>
  ///   Canonical string prepared by PrepareItem.
  /// </returns>
  TGIS_ComboBoxHelperGetBitmapEvent = function(
          _sender  : TObject     ;
    const _value   : String      ;
    const _color   : TAlphaColor ;
    const _font    : TFont       ;
    const _width   : Integer     ;
    const _height  : Integer
  ) : TGIS_Bitmap of object ;

  /// <summary>
  ///   Event to be fired upon drawing a custom item.
  /// </summary>
  /// <param name="_item">
  ///   list box item
  /// </param>
  /// <param name="_canvas">
  ///   canvas to draw on
  /// </param>
  /// <param name="_rect">
  ///   drawing area
  /// </param>
  /// <param name="_font">
  ///   font to be used
  /// </param>
  /// <param name="_color">
  ///   color to be used
  /// </param>
  /// <param name="_class">
  ///   rendering class to be used
  /// </param>
  /// <param name="_caption">
  ///   preferred caption to be displayed
  /// </param>
  /// <param name="_value">
  ///   textual value of an item
  /// </param>
  TGIS_ComboBoxHelperRenderEvent = procedure(
     _item      : TListBoxItem ;
     _canvas    : TCanvas      ;
     _rect      : TRectF       ;
     _font      : TFont        ;
     _color     : TAlphaColor  ;
     _class     : Char         ;
     _caption   : String       ;
     _value     : String
  ) of object ;

  /// <summary>
  ///   Item placement within Combo Box
  /// </summary>
  TGIS_ComboBoxHelperPosition = (
    /// <summary>
    ///   item to be placed at the top (for standard predefined values)
    /// </summary>
    Top,
    /// <summary>
    ///   item should be placed within LRU zone (below standard top items)
    /// </summary>
    Lru,
    /// <summary>
    ///   item (only one at a time) to be place above bottom items)
    /// </summary>
    Custom,
    /// <summary>
    ///   item should be placed at the bottom
    /// </summary>
    Bottom ) ;


  /// <summary>
  ///   Helper for TComboBox to provide simplified interface for parameters
  ///   selection.
  /// </summary>
  /// <remarks>
  ///   To be used within TGIS_ControlLegendForm .        f
  ///   <list type="table">
  ///     <listheader>
  ///       <term>Event</term>
  ///       <description>New behavior</description>
  ///     </listheader>
  ///     <item>
  ///       <term>OnChange</term>
  ///       <description>Will be called one upon drop box collapse is a
  ///         new value has been selected</description>
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_ComboBoxHelper = class( TGIS_PersistentFormHelper )
    private
      oparentControl  : TGIS_ComboBox  ;
      oeventTimer     : TTimer         ;
      oeventTimerVal  : String         ;
      ilruLimit       : Integer        ;

      evalueEvent     : TGIS_ComboBoxHelperValueEvent  ;
      ecustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      erenderEvent    : TGIS_ComboBoxHelperRenderEvent ;

      eorgOnChange    : TNotifyEvent   ;
      eorgOnKeyDown   : TKeyEvent      ;
      eorgOnCloseUp   : TNotifyEvent   ;
      eorgOnPainting  : TOnPaintEvent  ;

      bFill           : Boolean        ;

      sValue          : String         ;
      bEsc            : Boolean        ;  // do not change anything
    public
      /// <summary>
      ///   Helper constructor
      /// </summary>
      /// <param name="_parent">
      ///   TComboBox object to be linked with
      /// </param>
      /// <param name="_lruclass">
      ///   identifier for Last Recently Used storage. If empty then item will
      ///   bot be persistent. Suggested form is 'Form.Component'
      /// </param>
      /// <param name="_lrulimit">
      ///   how many items will be presented on last recently used list
      /// </param>
      constructor Create     ( const _parent   : TGIS_ComboBox ;
                               const _lruclass : String    ;
                               const _lrulimit : Integer
                             ) ; reintroduce ;

    private
      function  fget_Value   : string ;
      procedure fset_Value   ( const _value : String ) ;

      procedure doKeyDown    (       _sender   : TObject     ;
                               var   _key      : Word        ;
                               var   _keychar  : WideChar    ;
                                     _shift    : TShiftState
                             ) ;
      procedure doClosePopup (       _sender   : TObject     );
      procedure doTimer      (       _sender    : TObject
                             ) ;
      procedure doPainting   (       _sender    : TObject    ;
                                     _canvas    : TCanvas    ;
                               const _rect      : TRectF
                             ) ;
      procedure doStyle      (       _sender    : TObject
                             ) ;
    protected
      function  fget_State   : String ; override;
      procedure fset_State   ( const _value    : String
                             ) ; override;
    public
      /// <summary>
      ///   Construct canonical storage list item.
      /// </summary>
      /// <param name="_custom">
      ///   if True then selecting item will cause a custom action
      /// </param>
      /// <param name="_class">
      ///   rendering class of an item ; to be used upon RenderEvent
      /// </param>
      /// <param name="_position">
      ///   location of an item within list
      /// </param>
      /// <param name="_caption">
      ///   caption to be displayed
      /// </param>
      /// <param name="_value">
      ///   actual value; for custom event an action identifier
      /// </param>
      /// <returns>
      ///   Canonical string to be used internally.
      /// </returns>
      class function  PrepareItem(
                               const _custom   : Boolean                    ;
                               const _class    : Char                       ;
                               const _position : TGIS_ComboBoxHelperPosition ;
                               const _caption  : String                     ;
                               const _value    : String
                             ) : String ;

      /// <summary>
      ///   Set item on the list. Will manage position, lru etc.
      /// </summary>
      /// <param name="_string">
      ///   canonical text as obtained by PrepareItem
      /// </param>
      /// <param name="_selected">
      ///   if true then set item and mark it selected
      /// </param>
      /// <returns>
      ///   Actual position within ComboBox
      /// </returns>
      function  SetItem      ( const _string   : String ;
                               const _selected : Boolean
                             ) : Integer ; overload;
      /// <summary>
      ///   Set item on the list without marking it as selected. Will manage
      ///   position, lru etc.
      /// </summary>
      /// <param name="_string">
      ///   canonical text as obtained by PrepareItem
      /// </param>
      /// <returns>
      ///   Actual position within ComboBox
      /// </returns>
      function  SetItem      ( const _string   : String
                             ) : Integer ; overload;

      /// <summary>
      ///   Set OnChange event handler.
      /// </summary>
      /// <param name="_event">
      ///   event handler
      /// </param>
      procedure SetOnChange( const _event : TNotifyEvent ) ;


      /// <summary>
      ///   Init initial filling of  combo box. Optimized for adding multiple
      ///   elements at once. Items should be added in a natural order.
      /// </summary>
      procedure BeginFill ;

      /// <summary>
      ///   Finalize initial filling of combo box.
      /// </summary>
      procedure EndFill ;

    public

      /// <event/>
      /// <summary>
      ///   Event to be fired upon setting a value which does not exists on a
      ///   list. Event is responsible to prepare a canonical form of an event.
      /// </summary>
      property  ValueEvent  : TGIS_ComboBoxHelperValueEvent
                              read  evalueEvent
                              write evalueEvent ;

      /// <event/>
      /// <summary>
      ///   Event to be fired upon selecting custom item.
      /// </summary>
      property  CustomEvent : TGIS_ComboBoxHelperCustomEvent
                              read  ecustomEvent
                              write ecustomEvent ;

      /// <event/>
      /// <summary>
      ///   Event to be fired upon rendering a single combo box item. If not
      ///   set then standard behavior will be used.
      /// </summary>
      property  RenderEvent : TGIS_ComboBoxHelperRenderEvent
                              read  erenderEvent
                              write erenderEvent ;

      /// <summary>
      ///   Value of selected combo box item. To properly interpreted non
      ///   default values a ValueEvent must be implemented.
      /// </summary>
      property  Value       : String
                              read fget_Value
                              write fset_Value ;
    public

  end ;

//##############################################################################
implementation


uses
  GisRtl ;

function parent_form(
  _owner : TComponent
) :  TCustomForm ;
var
  tmp : TFmxObject ;
begin
  tmp := TFmxObject( _owner ) ;
  while Assigned( tmp ) do begin
    if tmp is TCustomForm then
      break ;
    tmp := tmp.Parent ;
  end;
  Assert( tmp is TCustomForm ) ;
  Result := TCustomForm( tmp ) ;
end;

function split(
  const _str       : String ;
  const _separator : array of Char
) : TArray<String> ;
begin
  {$IFDEF LEVEL_XE8_RTL}
    Result := _str.Split( _separator, '[', ']' )
  {$ELSE}
    Result := _str.Split( _separator ) ;
  {$ENDIF}
end;

{$REGION 'T_ListBoxItem'}
// Just bypassing RAD Studio 10.2 problems of unitlized style
// upon OnPainting event
type
  T_ListBoxItem = class( TListBoxItem )
    procedure DoRealign ; override;
  end;

procedure T_ListBoxItem.DoRealign ;
begin
  if ( ListBox <> nil )
     and
     ( not ListBox.IsUpdating )
     and
     ( ResourceLink = nil )
  then
    ApplyStyleLookup ;

  inherited ;

  if assigned( OnApplyStyleLookup ) then
    OnApplyStyleLookup( self ) ;
end;
{$ENDREGION}


{$REGION 'TGIS_PersistentForm'}
var
  oPersistentForm : TGIS_PersistentForm ;

  procedure TGIS_PersistentForm.SaveState ;
  var
    o    : TGIS_PersistentFormHelper  ;
    skey : String ;
    sval : String ;
  begin
    for o in lstSubscribers do begin
      skey := o.itemKey ;
      sval := o.itemValue ;

      if skey = '' then continue ;

      if Assigned( SaveEvent ) then
        SaveEvent( skey, sval ) ;
    end ;
  end ;

  class function TGIS_PersistentForm.Instance
    : TGIS_PersistentForm ;
  begin
    if not Assigned( oPersistentForm ) then begin
      oPersistentForm := TGIS_PersistentForm.Create ;
      oPersistentForm.lstSubscribers := TList<TGIS_PersistentFormHelper>.Create ;
    end ;

    Result := oPersistentForm ;
  end ;

  destructor TGIS_PersistentForm.Destroy ;
  begin
    FreeObject( lstSubscribers ) ;
  end ;

{$ENDREGION}

{$REGION 'TGIS_PersistentFormHelper'}
  constructor TGIS_PersistentFormHelper.Create( const _key : String ) ;
  begin
    inherited Create;

    spersistancyKey := _key ;

    TGIS_PersistentForm.Instance.lstSubscribers.Add( self ) ;
  end ;

  destructor TGIS_PersistentFormHelper.Destroy;
  begin
    if Assigned( oPersistentForm ) then
      oPersistentForm.lstSubscribers.Remove( self ) ;

    inherited;

  end ;

  function TGIS_PersistentFormHelper.itemKey
    : String ;
  begin
    Result := spersistancyKey ;
  end ;

  function TGIS_PersistentFormHelper.itemValue
    : String ;
  begin
    Result := State ;
  end ;

  procedure TGIS_PersistentFormHelper.LoadState ;
  begin
    if PeristencyKey = '' then
      exit ;

    if assigned( TGIS_PersistentForm.Instance.LoadEvent ) then
      State := TGIS_PersistentForm.Instance.LoadEvent( PeristencyKey ) ;
  end ;
{$ENDREGION}

{$REGION 'TGIS_ComboBoxHelper'}
const
  COMBO_HELPER_TYPE_SELECTABLE   =  '#' ;
  COMBO_HELPER_TYPE_CUSTOMACTION =  '@' ;
  COMBO_HELPER_POS_TOP           =  'T' ;
  COMBO_HELPER_POS_LRU           =  'L' ;
  COMBO_HELPER_POS_CUSTOM        =  'C' ;
  COMBO_HELPER_POS_BOTTOM        =  'B' ;

  constructor TGIS_ComboBoxHelper.Create(
    const _parent   : TGIS_ComboBox ;
    const _lruclass : String    ;
    const _lrulimit : Integer
  );
  begin
    inherited Create( _lruclass ) ;

    oparentControl := _parent ;
    oparentControl.DropDownKind := TDropDownKind.Custom ;

    oparentControl.Items.Clear ;

    ilruLimit := _lrulimit ;

    oeventTimer := TTimer.Create( oparentControl ) ;
    oeventTimer.Enabled  := False ;
    oeventTimer.Interval := {$IFNDEF LINUX}
                              10 ;
                            {$ELSE}
                              500 ;
                            {$ENDIF}
    oeventTimer.OnTimer  := doTimer ;

    eorgOnChange   := oparentControl.OnChange    ;
    eorgOnKeyDown  := oparentControl.OnKeyDown   ;
    eorgOnCloseUp  := oparentControl.OnClosePopup ;
    eorgOnPainting := oparentControl.OnPainting   ;

    oparentControl.OnChange := nil;

    _parent.OnKeyDown  := doKeyDown ;
    _parent.OnClosePopup := doClosePopup ;

    bFill := False ;
  end ;

  function TGIS_ComboBoxHelper.fget_Value
    : string;
  begin
    Result := sValue ;
  end ;

  procedure TGIS_ComboBoxHelper.fset_Value(
    const _value: String
  );
  begin
    SetItem( ValueEvent( self, _value ), True ) ;
  end ;


  procedure TGIS_ComboBoxHelper.doPainting(
          _sender    : TObject    ;
          _canvas    : TCanvas    ;
    const _rect      : TRectF
  );
  var
    ar     : TArray<string>;
    o      : TFmxObject ;
    ocntrl : T_ListBoxItem ;
    otext  : TText      ;
  begin
    if Assigned( eorgOnPainting ) then begin
      eorgOnPainting( _sender, _canvas, _rect ) ;
      exit ;
    end ;

    Assert( _sender is T_ListBoxItem ) ;
    ocntrl := T_ListBoxItem( _sender ) ;

    ar := split(ocntrl.ItemData.Detail, ['|'] ) ;

    if Length( ar ) < 1 then
      exit ;

    o := ocntrl.FindStyleResource( 'text' ) ;


    if not ( o is TText ) then exit ;

    otext := TText( o ) ;

    Assert( Length( ar ) = 3 ) ;
    Assert( Length( ar[0] ) = 3 ) ;

    if Assigned( RenderEvent ) then begin
      otext.Text := '' ;
      RenderEvent( ocntrl, _canvas,
                   otext.ParentedRect, otext.Font, otext.Color, ar[0][2], ar[1], ar[2]
                 ) ;
    end;
  end ;

  procedure TGIS_ComboBoxHelper.doStyle(
          _sender    : TObject
  );
  var
    o      : TFmxObject ;
    ocntrl : T_ListBoxItem ;
    otext  : TText      ;
  begin
    Assert( _sender is T_ListBoxItem ) ;
    ocntrl := T_ListBoxItem( _sender ) ;

    o := ocntrl.FindStyleResource( 'text' ) ;

    if not ( o is TText ) then exit ;

    otext := TText( o ) ;

    if Assigned( RenderEvent ) then
      otext.Text := '' ;
  end ;

  procedure TGIS_ComboBoxHelper.doKeyDown(
        _sender : TObject   ;
    var _key    : Word      ;
    var _keychar : WideChar  ;
        _shift  : TShiftState
  );
  begin
    case _key of
      vkEscape :
        begin
          bEsc := True ;
          exit ;
        end;
      vkDown   :
        begin
          if not oparentControl.DroppedDown then begin
            oparentControl.DropDown ;
            _key := 0 ;
            exit ;
          end
        end;
    end;

    {$IFDEF GIS_MOBILE_DIALOGS}
      case _key of
        vkHome,
        vkEnd,
        vkUp,
        vkDown,
        vkLeft,
        vkRight,
        vkPrior,
        vkNext,
        vkF4,
        vkReturn,
        vkEscape:
          begin
            if not oparentControl.DroppedDown then begin
              oparentControl.DropDown ;
            end ;
            _key := 0 ;
             exit ;
          end;
      end;
    {$ENDIF}

    if Assigned( eorgOnKeyDown ) then
      eorgOnKeyDown( _sender, _key, _keychar, _shift ) ;
  end ;

  procedure TGIS_ComboBoxHelper.doClosePopup(
    _sender: TObject
  );
  var
    ar  : TArray<string>;
    {$IFDEF LEVEL_RX10_FMX}
      frm : TCustomForm ;
    {$ENDIF}
  begin
    if not Assigned( oparentControl.Selected ) then
      exit ;
    if oeventTimer.Enabled then
      exit ;

    ar := split(oparentControl.Selected.ItemData.Detail, ['|'] ) ;

    try
      if bEsc then exit ;

      if Length( ar ) < 1 then
        exit ;

      Assert( Length( ar ) = 3 ) ;
      Assert( Length( ar[0] ) = 3 ) ;

      if ar[0][1] = COMBO_HELPER_TYPE_CUSTOMACTION then begin
        {$IFDEF LEVEL_RX10_FMX}
          frm := parent_form( oparentControl ) ;
          if Assigned( frm ) then
            frm.ShowHint := True ;
        {$ENDIF}

        // call the event via timer t to give a chance
        // to fully close dropdown combobox
        oeventTimerVal       := ar[2] ;
        oeventTimer.Enabled  := True ;
      end
      else
        Value := ar[2] ;
    finally
      bEsc := False ;
      if Assigned( eorgOnCloseUp ) then
        eorgOnCloseUp( _sender ) ;
    end ;
  end ;

  procedure TGIS_ComboBoxHelper.doTimer(
        _sender    : TObject
  ) ;
  var
    str : String ;
    {$IFDEF LEVEL_RX10_FMX}
      frm : TCustomForm ;
    {$ENDIF}
  begin
    oeventTimer.Enabled := False ;

    str := CustomEvent( oparentControl, oeventTimerVal ) ;

    oparentControl.ItemIndex := -1; // force deletion

    {$IFDEF LEVEL_RX10_FMX}
      frm := parent_form( oparentControl ) ;
      if Assigned( frm ) then
        frm.ShowHint := True ;
    {$ENDIF}

    if str <> '' then begin
       SetItem( str, True );
    end
    else
      Value := Value ;
  end;

  function TGIS_ComboBoxHelper.fget_State
    : String ;
  var
    i : Integer ;
    s : String ;
  begin
    Result := '' ;
    for i :=0 to oparentControl.Items.Count - 1 do begin
      s := oparentControl.ListItems[i].ItemData.Detail;
      Assert( Length( s ) >= 0 ) ;

      if s[3] <> COMBO_HELPER_POS_LRU then
        continue ;

      if Result <> '' then
        Result := Result + '|' ;
      Result := Result + split(s, ['|'] )[2] ;
    end ;
  end ;

  procedure TGIS_ComboBoxHelper.fset_State(
    const _value : String
  ) ;
  var
    i  : Integer ;
    ar : TArray<String> ;
  begin
    if _value = '' then exit ;

    ar := split(_value, ['|'] ) ;

    Assert( Assigned( ValueEvent ), 'ValueEvent not assigned' ) ;

    for i:= Length( ar ) -1 downto 0 do begin
      SetItem(  ValueEvent( self, ar[i] ) ) ;
    end ;
  end ;

  class function TGIS_ComboBoxHelper.PrepareItem(
    const _custom   : Boolean                    ;
    const _class    : Char                       ;
    const _position : TGIS_ComboBoxHelperPosition ;
    const _caption  : String                     ;
    const _value    : String
  ) : String ;
  var
    bld : TStringBuilder ;
  begin
    bld := TStringBuilder.Create ;
    try
      case _custom of
        True  : bld.Append( COMBO_HELPER_TYPE_CUSTOMACTION ) ;
        False : bld.Append( COMBO_HELPER_TYPE_SELECTABLE   ) ;
      end ;

      bld.Append( _class ) ;

      case _position of
        TGIS_ComboBoxHelperPosition.Top     :
          bld.Append( COMBO_HELPER_POS_TOP    ) ;
        TGIS_ComboBoxHelperPosition.Lru     :
          bld.Append( COMBO_HELPER_POS_LRU    ) ;
        TGIS_ComboBoxHelperPosition.Custom  :
          bld.Append( COMBO_HELPER_POS_CUSTOM ) ;
        TGIS_ComboBoxHelperPosition.Bottom  :
          bld.Append( COMBO_HELPER_POS_BOTTOM ) ;
      end ;

      bld.Append( '|' ) ;
      bld.Append( _caption ) ;
      bld.Append( '|' ) ;
      bld.Append( _value ) ;

      Result := bld.ToString ;
    finally
      FreeObject( bld ) ;
    end ;
  end ;

  function TGIS_ComboBoxHelper.SetItem(
    const _string   : String ;
    const _selected : Boolean
  ) : Integer ;
  var
    ar         : TArray<string>;
    i          : Integer ;
    j          : Integer ;
    isel       : Integer ;
    oldsel     : Integer ;
    ilru_first : Integer ;
    ilru_last  : Integer ;
    blru       : Boolean ;
    stmp       : String  ;
    scmp1      : String  ;
    scmp2      : String  ;

    procedure select_item( const _idx : Integer ) ;
    begin
      oparentControl.ItemIndex := _idx ;
      sValue := split(oparentControl.ListItems[_idx].ItemData.Detail,['|'])[2] ;

      if Assigned( eorgOnChange ) then
        eorgOnChange( oparentControl ) ;
    end ;

    function create_item( const _string : String ) : TListBoxItem ;
    begin
      Result := T_ListBoxItem.Create( nil ) ;
      Result.DisableDisappear := True ; // FMX bug fix
      Result.Height := oparentControl.ClipRect.Height ;
      Result.Parent := oparentControl ;
      Result.Text := split(_string, ['|'])[1] ;
      Result.ItemData.Detail := _string ;
      oparentControl.ItemHeight := Result.Height ; // xe7 fix
      Result.OnPaint   := doPainting ;
      Result.OnApplyStyleLookup  := doStyle ;
    end ;
  begin
    Result := 0 ;
    ar := split(_string, ['|'] ) ;

    if Length( ar ) < 1 then
      exit ;

    Assert( Length( ar ) = 3 ) ;
    Assert( length( ar[0] ) = 3 ) ;

    if bFill then begin
      create_item( _string ) ;
      Result := oparentControl.Items.Count - 1 ;
      exit ;
    end ;

    isel       := -1 ;
    ilru_first := 9999 ;
    ilru_last  := -1   ;

    // manage selection and lru reorder
    if _selected then begin
      blru := false ;

      scmp1 := StringReplace(
                 Uppercase( ar[2] ),
                 ' ', '',
                 [rfReplaceAll]
               ) ;
      for i := 0 to oparentControl.Items.Count -1 do begin
        stmp := oparentControl.ListItems[i].ItemData.Detail ;
        Assert( Length( stmp ) > 3  );

        if stmp[3] = COMBO_HELPER_POS_LRU then begin
          ilru_first := Min( ilru_first, i ) ;
          ilru_last  := Max( ilru_last, i ) ;
        end ;

        if isel < 0 then begin
          scmp2 := StringReplace(
                     Uppercase( split(stmp, ['|'] )[2] ),
                     ' ', '',
                     [rfReplaceAll]
                   ) ;

          if scmp1 = scmp2 then begin
            isel := i ;
            if stmp[3] = COMBO_HELPER_POS_LRU then
              blru := True ;
          end ;
        end ;
      end ;

      if isel >= 0  then begin
        if ( ilru_last >= 0 ) and blru then begin
          // there is something on LRU list - reorder
          oparentControl.ItemIndex := -1 ;
          oparentControl.ListItems[ isel ].Index := ilru_first ;
          isel := ilru_first ;
        end ;
        Result := isel ;
        select_item( isel ) ;

        exit ;
      end ;
    end ;

    case ar[0][3] of
      COMBO_HELPER_POS_TOP :
        begin
          j := 0 ;
          for i := 0 to oparentControl.Items.Count -1 do begin
            stmp := oparentControl.ListItems[i].ItemData.Detail ;
            Assert( Length( stmp ) > 3  );
            if stmp[3] <> COMBO_HELPER_POS_TOP then
              break ;
            Inc( j ) ;
          end ;
          oldsel := oparentControl.ItemIndex ;
          oparentControl.ItemIndex := -1 ;
          oparentControl.ListItems[create_item( _string ).Index].Index := j ;
          Result := j ;
          if _selected then
            select_item( j )
          else
            oparentControl.ItemIndex := oldsel ;
        end ;
      COMBO_HELPER_POS_BOTTOM :
        begin
        create_item( _string ) ;
          Result := oparentControl.Items.Count - 1 ;
        end ;
      COMBO_HELPER_POS_CUSTOM :
        begin
          j := 0 ;
          for i := 0 to oparentControl.Items.Count -1 do begin
            stmp := oparentControl.ListItems[i].ItemData.Detail ;
            Assert( Length( stmp ) > 3  );
            if stmp[3] = COMBO_HELPER_POS_CUSTOM then
            begin
             oldsel := oparentControl.ItemIndex ;
              oparentControl.ItemIndex := -1 ;
              oparentControl.ListItems[i].ItemData.Detail := _string ;
              Result := j ;
              if _selected then
                select_item( j )
              else
                oparentControl.ItemIndex := oldsel ;
              j := -1 ;
              break ;
            end ;
            if stmp[3] = COMBO_HELPER_POS_BOTTOM then begin
              break ;
            end ;
            Inc( j ) ;
          end ;
          if j >= 0 then begin
            oldsel := oparentControl.ItemIndex ;
            oparentControl.ItemIndex := -1 ;
            oparentControl.ListItems[create_item( _string ).Index].Index := j ;
            Result := j ;
            if _selected then
              select_item( j )
            else
              oparentControl.ItemIndex := oldsel ;
          end ;
        end ;
      COMBO_HELPER_POS_LRU :
        begin
          j := 0 ;
          for i := 0 to oparentControl.Items.Count -1 do begin
            stmp := oparentControl.ListItems[i].ItemData.Detail ;
            Assert( Length( stmp ) > 3  );
            if stmp[3] <> COMBO_HELPER_POS_TOP then
              break ;
            Inc( j ) ;
          end ;
          oldsel := oparentControl.ItemIndex ;
          oparentControl.ItemIndex := -1 ;
          oparentControl.ListItems[create_item( _string ).Index].Index := j ;
          Result := j ;
          if _selected then
            select_item( j )
          else
            oparentControl.ItemIndex := oldsel ;

          j := 0 ;
          for i := 0 to oparentControl.Items.Count -1 do begin
            stmp := oparentControl.ListItems[i].ItemData.Detail ;
            if stmp[3] = COMBO_HELPER_POS_LRU then
              Inc( j ) ;
            if j > ilruLimit then begin
              FreeObjectNotNil( oparentControl.ListItems[i] ) ;
              break ;
            end ;
          end ;

        end ;
      else
        begin
          Assert( False, 'Unexpected case' ) ;
        end ;
    end ;
  end ;

  function TGIS_ComboBoxHelper.SetItem(
    const _string   : String
  ) : Integer ;
  begin
    Result := SetItem( _string, False ) ;
  end ;

  procedure TGIS_ComboBoxHelper.SetOnChange( const _event : TNotifyEvent ) ;
  begin
    eorgOnChange := _event ;
  end;

  procedure TGIS_ComboBoxHelper.BeginFill ;
  begin
    oparentControl.Items.BeginUpdate ;
    bFill := True ;
  end;

  procedure TGIS_ComboBoxHelper.EndFill ;
  begin
    bFill := False ;
    oparentControl.ItemIndex := 0 ;
    oparentControl.Items.EndUpdate ;
  end;

{$ENDREGION}


initialization
  oPersistentForm := nil;
finalization
  FreeObject( oPersistentForm ) ;

//==================================== END =====================================
end.

