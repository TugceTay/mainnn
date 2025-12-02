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
  ComboBox control helper to manipulate items and LRU list.
}

unit VCL.GisComboBoxHelper;
{$HPPEMIT '#pragma link "VCL.GisComboBoxHelper"'}

interface

{$INCLUDE GisInclude.inc}

uses
  System.Classes,
  System.SysUtils,
  System.Generics.Collections,
  System.Types,
  System.Math,
  Winapi.Windows,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Graphics,

  GisTypesUI,
  VCL.GisFramework;

type

  /// <summary>
  ///   Interface for persistent form operations
  /// </summary>
  IGIS_PersistentForm = interface

    /// <summary>
    ///   Return item key.
    /// </summary>
    /// <returns>
    ///   Item key.
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
    ///   Item value.
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
    _key   : String ;
    _value : String
  ) of object ;

  /// <summary>
  ///   Event to be fired upon loading object state.
  /// </summary>
  /// <param name="_key">
  ///   unique object identifier
  /// </param>
  /// <returns>
  ///   State value.
  /// </returns>
  TGIS_PersistentFormLoadEvent = function(
    _key   : String
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
      ///   Instance.
      /// </returns>
      class function Instance : TGIS_PersistentForm ;

      /// <inheritdoc/>
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
      function fget_State  : String ; virtual; abstract;
      procedure fset_State( const _value : String ) ; virtual; abstract;
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
  ///   Event to be fired upon getting a bitmap.
  /// </summary>
  /// <param name="_sender">
  ///   event originator
  /// </param>
  /// <param name="_value">
  ///   event identifier
  /// </param>
  /// <param name="_width">
  ///   bitmap width
  /// </param>
  /// <param name="_height">
  ///   bitmap height
  /// </param>
  TGIS_ComboBoxHelperGetBitmapEvent = function(
    _sender  : TObject ;
    _value   : String  ;
    _width   : Integer ;
    _height  : Integer
  ) : TGIS_Bitmap of object ;

  /// <summary>
  ///   Event to be fired upon drawing a custom item.
  /// </summary>
  /// <param name="_control">
  ///   parent TComboBox
  /// </param>
  /// <param name="_rect">
  ///   same as TComboBox.OnDrawItem
  /// </param>
  /// <param name="_state">
  ///   same as TComboBox.OnDrawItem
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
     _control : TComboBox       ;
     _rect    : TRect           ;
     _state   : TOwnerDrawState ;
     _class   : Char            ;
     _caption : String          ;
     _value   : String
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
  ///   To be used within TGIS_ControlLegendForm .
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
      oparentControl  : TComboBox      ;
      ilruLimit       : Integer        ;

      evalueEvent     : TGIS_ComboBoxHelperValueEvent  ;
      ecustomEvent    : TGIS_ComboBoxHelperCustomEvent ;
      erenderEvent    : TGIS_ComboBoxHelperRenderEvent ;

      eorgOnChange    : TNotifyEvent   ;
      eorgOnKeyDown   : TKeyEvent      ;
      eorgOnCloseUp   : TNotifyEvent   ;
      eorgOnDrawItem  : TDrawItemEvent ;

      lstBatchFill    : TStringList    ;
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
      constructor Create     ( const _parent   : TComboBox ;
                               const _lruclass : String    ;
                               const _lrulimit : Integer
                             ) ; reintroduce ;

    private
      function  fget_Value   : string ;
      procedure fset_Value   ( const _value : String ) ;

      procedure doKeyDown    (       _sender   : TObject     ;
                               var   _key      : Word        ;
                                     _shift    : TShiftState
                             ) ;
      procedure doCloseUp    (       _sender   : TObject     );
      procedure doDrawItem   (       _control  : TWinControl ;
                                     _index    : Integer     ;
                                     _rect     : TRect       ;
                                     _state    : TOwnerDrawState
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

  end ;

implementation

uses
  System.StrUtils,
  VCL.Forms,
  GisRtl,
  GisInternals ;

function split( const _str : String ; const _separator : array of Char ) : TArray<String> ;
var
  res : TStringDynArray ;
  i   : Integer ;
begin
  {$IFDEF LEVEL_XE3_RTL}
    {$IFDEF LEVEL_XE8_RTL}
      Result := _str.Split( _separator, '[', ']' ) ;
    {$ELSE}
      Result := _str.Split( _separator ) ;
    {$ENDIF}
  {$ELSE}
    res := StrUtils.SplitString( _str, _separator ) ;
    SetLength( Result, length( res ) ) ;
    for i := 0 to length( res )-1 do
      Result[i] := res[i] ;
  {$ENDIF}
end;

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
    lstSubscribers.Free ;
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
    const _parent   : TComboBox ;
    const _lruclass : String    ;
    const _lrulimit : Integer
  );
  begin
    inherited Create( _lruclass ) ;

    oparentControl := _parent ;
    oparentControl.Style := csOwnerDrawFixed ;

    {$IFNDEF LEVEL_RX101_VCL}
      oParentControl.ItemHeight := MulDiv( oParentControl.ItemHeight,
                                           Screen.PixelsPerInch,
                                           96
                                         ) ;
    {$ENDIF}

    ilruLimit := _lrulimit ;

    eorgOnChange   := oparentControl.OnChange    ;
    eorgOnKeyDown  := oparentControl.OnKeyDown   ;
    eorgOnCloseUp  := oparentControl.OnCloseUp   ;
    eorgOnDrawItem := oparentControl.OnDrawItem  ;

    oparentControl.OnChange := nil;

    _parent.OnKeyDown  := doKeyDown ;
    _parent.OnCloseUp  := doCloseUp ;
    _parent.OnDrawItem := doDrawItem ;
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

  procedure TGIS_ComboBoxHelper.doDrawItem(
    _control: TWinControl;
    _index: Integer;
    _rect: TRect;
    _state: TOwnerDrawState
  );
  var
    ar : TArray<string>;
    clb, clf : TColor ;
    tf : TTextFormat ;
  begin
    if Assigned( eorgOnDrawItem ) then begin
      eorgOnDrawItem( _control, _index, _rect, _state ) ;
      exit ;
    end ;

    ar := split(oparentControl.Items[ _index ], ['|'] ) ;

    if Length( ar ) < 1 then
      exit ;

    Assert( Length( ar ) = 3 ) ;
    Assert( Length( ar[0] ) = 3 ) ;

    if Assigned( RenderEvent ) then
      RenderEvent( oparentControl, _rect, _state, ar[0][2], ar[1], ar[2] )
    else begin
      if oparentControl.BiDiMode = bdRightToLeft then
        tf := [tfRtlReading,tfRight]
      else
        tf := [] ;

      oparentControl.Canvas.FillRect(_rect);
      _rect.Left  := _rect.Left  + 2 ;
      _rect.Right := _rect.Right - 2 ;
      _rect.Top   := _rect.Top   + 1 ;

      oparentControl.Canvas.TextRect( _rect, ar[1], tf ) ;
    end ;
  end ;

  procedure TGIS_ComboBoxHelper.doKeyDown(
        _sender : TObject   ;
    var _key    : Word      ;
        _shift  : TShiftState
  );
  begin
    case _key of
      VK_ESCAPE :
        begin
          bEsc := True ;
          exit ;
        end ;
      VK_EXECUTE,
      VK_RETURN :
        begin
          _key := 0 ;
          exit ;
        end ;
      VK_DOWN :
        if not oparentControl.DroppedDown then begin
          oparentControl.DroppedDown := True ;
          _key := 0 ;
          exit ;
        end ;
    end;

    if Assigned( eorgOnKeyDown ) then
      eorgOnKeyDown( _sender, _key, _shift ) ;
  end ;

  procedure TGIS_ComboBoxHelper.doCloseUp(
    _sender: TObject
  );
  var
    ar  : TArray<string>;
    str : String ;
  begin
    ar := split(oparentControl.Items[ oparentControl.ItemIndex ], ['|'] ) ;

    try
      if bEsc then exit ;

      if Length( ar ) < 1 then
        exit ;

      Assert( Length( ar ) = 3 ) ;
      Assert( Length( ar[0] ) = 3 ) ;

      if ar[0][1] = COMBO_HELPER_TYPE_CUSTOMACTION then begin
        str := CustomEvent( self, ar[2] ) ;

        if str <> '' then begin
          SetItem( str, True );
        end
        else
          Value := Value ;
      end
      else
        Value := ar[2] ;
    finally
      bEsc := False ;
      if Assigned( eorgOnCloseUp ) then
        eorgOnCloseUp( _sender ) ;
    end ;
  end ;

  function TGIS_ComboBoxHelper.fget_State
    : String ;
  var
    s : String ;
  begin
    Result := '' ;
    for s in oparentControl.Items do begin
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
        TGIS_ComboBoxHelperPosition.Top :
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
      bld.Free ;
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
    ilru_first : Integer ;
    ilru_last  : Integer ;
    blru       : Boolean ;
    stmp       : String  ;
    itms       : TStrings ;

    procedure select_item( const _idx : Integer ) ;
    begin
      Assert( not Assigned( lstBatchFill ) ) ;
      oparentControl.ItemIndex := _idx ;
      sValue := split(itms[_idx],['|'])[2] ;

      if Assigned( eorgOnChange ) then
        eorgOnChange( oparentControl ) ;
    end ;
  begin
    Result := 0 ;
    ar := split(_string, ['|'] ) ;

    Assert( Length( ar ) = 3 ) ;
    Assert( length( ar[0] ) = 3 ) ;

    if Assigned( lstBatchFill ) then begin
      lstBatchFill.Add( _string ) ;
      Result := lstBatchFill.Count -1;
      exit ;
    end ;

    itms := oparentControl.Items ;

    isel       := -1 ;
    ilru_first := 9999 ;
    ilru_last  := -1   ;

    itms.BeginUpdate ;
    try
      // manage selection and lru reorder
      if _selected then begin
        blru := false ;

        for i := 0 to itms.Count -1 do begin
          stmp := itms[i] ;
          Assert( Length( stmp ) > 3  );

          if stmp[3] = COMBO_HELPER_POS_LRU then begin
            ilru_first := Min( ilru_first, i ) ;
            ilru_last  := Max( ilru_last, i ) ;
          end ;

          if isel < 0 then begin
            if split( stmp, ['|'] )[2] = ar[2] then begin
              isel := i ;
              if stmp[3] = COMBO_HELPER_POS_LRU then
                blru := True ;
            end ;
          end ;
        end ;

        if isel >= 0  then begin
          if ( ilru_last >= 0 ) and blru then begin
            // there is something on LRU list - reorder
            stmp := itms[ isel ] ;
            itms.Delete( isel );
            itms.Insert( ilru_first, stmp );
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
            for i := 0 to itms.Count -1 do begin
              Assert( Length( itms[i] ) > 3  );
              if itms[i][3] <> COMBO_HELPER_POS_TOP then
                break ;
              Inc( j ) ;
            end ;
            itms.Insert( j, _string ) ;
            Result := j ;
            if _selected then
              select_item( j ) ;
          end ;
        COMBO_HELPER_POS_BOTTOM :
          begin
            itms.Append( _string );
            Result := itms.Count - 1 ;
          end ;
        COMBO_HELPER_POS_CUSTOM :
          begin
            j := 0 ;
            for i := 0 to itms.Count -1 do begin
              Assert( Length( itms[i] ) > 3  );
              if itms[i][3] = COMBO_HELPER_POS_CUSTOM then begin
                itms[j] := _string ;
                Result := j ;
                if _selected then
                  select_item( j ) ;
                j := -1 ;
                break ;
              end ;
              if itms[i][3] = COMBO_HELPER_POS_BOTTOM then begin
                break ;
              end ;
              Inc( j ) ;
            end ;
            if j >= 0 then begin
              itms.Insert( j, _string );
              Result := j ;
              if _selected then
                select_item( j ) ;
            end ;
          end ;
        COMBO_HELPER_POS_LRU :
          begin
            j := 0 ;
            for i := 0 to itms.Count -1 do begin
              Assert( Length( itms[i] ) > 3  );
              if itms[i][3] <> COMBO_HELPER_POS_TOP then
                break ;
              Inc( j ) ;
            end ;
            itms.Insert( j, _string );
            Result := j ;
            if _selected then
              select_item( j ) ;

            j := 0 ;
            for i := 0 to itms.Count -1 do begin
              if itms[i][3] = COMBO_HELPER_POS_LRU then
                Inc( j ) ;
              if j > ilruLimit then begin
                itms.Delete( i ) ;
                break ;
              end ;
            end ;

          end ;
        else
          begin
            Assert( False, 'Unexpected case' ) ;
          end ;
      end ;
    finally
      itms.EndUpdate ;
    end ;
  end ;

  function TGIS_ComboBoxHelper.SetItem(
    const _string   : String
  ) : Integer ;
  begin
    Result := SetItem( _string, False ) ;
  end ;

  procedure TGIS_ComboBoxHelper.BeginFill ;
  begin
    oparentControl.Items.BeginUpdate ;
    lstBatchFill := TStringList.Create ;
  end;

  procedure TGIS_ComboBoxHelper.EndFill ;
  begin
    oparentControl.Items.Assign( lstBatchFill );
    FreeAndNil( lstBatchFill ) ;
    oparentControl.ItemIndex := 0 ;
    oparentControl.Items.EndUpdate ;
  end;

{$ENDREGION'}

initialization
  oPersistentForm := nil ;
finalization
  FreeObject( oPersistentForm ) ;
end.

