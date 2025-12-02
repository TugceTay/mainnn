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
  Coordinate System. Helper for dialog boxes.
}

unit FMX.GisControlCsHelper ;
{$HPPEMIT '#pragma link "FMX.GisControlCsHelper"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  System.Types,
  System.UITypes,
  FMX.Controls,
  FMX.ComboEdit,
  FMX.Edit,
  FMX.Pickers,
  FMX.Types,
  FMX.Graphics,

  GisTypes,
  GisCsBase;

type

  /// <summary>
  ///   Extender for TComboBox giving a few missed properties
  /// </summary>
  TComboEdit =  class( FMX.ComboEdit.TComboEdit )
    private
      FOnDropDown : TNotifyEvent  ;
      procedure fset_DroppedDown  ( const _value   : Boolean ) ;
      function  fget_DroppedDown  : Boolean ;

      procedure doListBoxKeyDown  (       _sender  : TObject ;
                                    var   _key     : Word    ;
                                    var   _keychar : Char    ;
                                          _shift   : TShiftState
                                  ) ;
    public

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_owner">
      ///   parent control
      /// </param>
      {#ownership:_owner:ownif_empty}
      constructor Create        (       _owner   : TComponent
                                ) ; override;

      /// <summary>
      ///   Override the standard DropDown service.
      /// </summary>
      procedure   DropDown      ; override;

    public

      /// <summary>
      ///   Indicates whether the drop-down list is currently displayed.
      /// </summary>
      property  DroppedDown     : Boolean      read  fget_DroppedDown
                                               write fset_DroppedDown ;

      /// <event/>
      property  DropDownEvent   : TNotifyEvent read  FOnDropDown
                                               write FOnDropDown ;
  end;

  /// <summary>
  ///   TComboBox helper
  /// </summary>
  TGIS_CSAbstractListHelper = class
    private
      bUnitsLinear      : Boolean ;
      bUnitsAngular     : Boolean ;
      bUnitsAreal       : Boolean ;
      bUnitsAuto        : Boolean ;
      bProjRevertible   : Boolean ;
      bProjUnRevertible : Boolean ;
      bProjStandard     : Boolean ;
      bProjGrid         : Boolean ;

      oList       : TGIS_CSAbstractList ;
      oCombo      : TComboEdit          ;
      sSearch     : String              ;
      oSelected   : TGIS_CSAbstract     ;
      oldOnChange : TNotifyEvent        ;
      iForcedEPSG : Integer             ;
      oAreaExtent : TGIS_Extent         ;
      iAreaEPSG   : Integer             ;
    private
      function  doFilter        ( const _obj      : TGIS_CSAbstract
                                ) : Boolean ;
      procedure doPaint         (       _sender   : TObject   ;
                                        _canvas   : TCanvas   ;
                                  const _arect    : TRectF
                                ) ;
      procedure doComboDropDown (       _sender   : TObject
                                ) ;
      procedure doComboKeyPress (       _sender   : TObject   ;
                                  var   _key      : Word      ;
                                  var   _keychar  : WideChar  ;
                                        _shift    : TShiftState
                                 ) ;
      procedure doComboChange   (       _sender   : TObject
                                ) ;

      /// <summary>
      ///   Return clear WKT name (by replacing '_' with space) and
      ///   with EPSG number embedded.
      /// </summary>
      /// <param name="_obj">
      ///   EPSG object
      /// </param>
      /// <returns>
      ///   Cleared object.
      /// <returns>
      function  clearWkt        ( const _obj      : TGIS_CSAbstract
                                ) : String ;
    public

      /// <summary>
      ///   Select item in a connected combo box based on EPSG number.
      ///   with EPSG number embedded.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG number based on which item should be selected
      ///   if not found then default item will be selected
      /// </param>
      procedure ByEPSG          ( const _epsg     : Integer
                                ) ;
    public

      /// <summary>
      ///   Create an instance of a helper
      /// </summary>
      /// <param name="_combobox">
      ///   associated combo box control
      /// </param>
      /// <param name="_wktlist">
      ///   associated EPSG/WKT list
      /// </param>
      constructor Create        ( const _combobox : TComboEdit  ;
                                  const _wktlist  : TGIS_CSAbstractList
                                ) ;
    public

      /// <summary>
      ///   Selected WKT unit.
      /// </summary>
      property SelectedObject       : TGIS_CSAbstract read oSelected ;

      /// <summary>
      ///   If True, show linear units.
      /// </summary>
      property ShowUnitsLinear      : Boolean read  bUnitsLinear
                                              write bUnitsLinear ;

      /// <summary>
      ///   If True, show angular units.
      /// </summary>
      property ShowUnitsAngular     : Boolean read  bUnitsAngular
                                              write bUnitsAngular ;

      /// <summary>
      ///   If True, show areal units.
      /// </summary>
      property ShowUnitsAreal       : Boolean read  bUnitsAreal
                                              write bUnitsAreal ;

      /// <summary>
      ///   If True, show automatic units.
      /// </summary>
      property ShowUnitsAuto        : Boolean read  bUnitsAuto
                                              write bUnitsAuto ;

      /// <summary>
      ///   If True, show revertible projections.
      /// </summary>
      property ShowProjRevertible   : Boolean read  bProjRevertible
                                              write bProjRevertible ;

      /// <summary>
      ///   If True, show unrevertible projections.
      /// </summary>
      property ShowProjUnrevertible : Boolean read  bProjUnRevertible
                                              write bProjUnRevertible ;

      /// <summary>
      ///   If True, show standard projections.
      /// </summary>
      property ShowProjStandard     : Boolean read  bProjStandard
                                              write bProjStandard ;

      /// <summary>
      ///   If True, show grid projections.
      /// </summary>
      property ShowProjGrid         : Boolean read  bProjGrid
                                              write bProjGrid ;

      /// <summary>
      ///   Area extent filter.
      /// </summary>
      property AreaExtent           : TGIS_Extent
                                              read  oAreaExtent
                                              write oAreaExtent ;

      /// <summary>
      ///   Area EPSG filter.
      /// </summary>
      property AreaEPSG             : Integer
                                              read  iAreaEPSG
                                              write iAreaEPSG ;
  end;

//##############################################################################
implementation

uses
  GisRtl,
  GisResource,
  GisFunctions,
  GisCsSystems,
  GisCsProjections ;

const
  MAX_ITEMS = 100 ; // max items on the search list

//==============================================================================
// TComboEdit extender
//==============================================================================

  constructor TComboEdit.Create( _owner: TComponent );
  begin
    inherited Create( _owner ) ;
    self.DropDownKind := TDropDownKind.Custom ;
    { TODO -cReview : Review }
    //ListBox.OnKeyDown := doListBoxKeyDown ;
  end;

  procedure TGIS_CSAbstractListHelper.doPaint(
          _sender : TObject ;
          _canvas : TCanvas ;
    const _arect  : TRectF
  ) ;
  var
    k : Integer ;
  begin
    k := Pos( sSearch, UpperCase( oCombo.Text ) ) ;
    if k>=StringFirst then begin
      oCombo.SelStart := k-1 ;
      oCombo.SelLength := Length( sSearch ) ;
      oCombo.CaretPosition := oCombo.SelStart + oCombo.SelLength ;
    end
    else begin
      oCombo.SelStart  := 0 ;
      oCombo.SelLength := 0 ;
      oCombo.CaretPosition := 0 ;
    end ;
  end;

  procedure TComboEdit.doListBoxKeyDown(
        _sender  : TObject ;
    var _key     : Word    ;
    var _keychar : Char    ;
        _shift   : TShiftState
  ) ;
  begin
    self.KeyDown( _key, _keychar, _shift ) ;
  end;

  procedure TComboEdit.fset_DroppedDown(
    const _value : Boolean
  ) ;
  begin
  { TODO -cReview : doing nothing???? }
   { if _value then begin
      if not TPopup( self.ListBox.Parent ).IsOpen then begin
        if Assigned( FOnDropDown ) then
          FOnDropDown( Self ) ;
        ListBox.ItemIndex := -1 ;
        DropDown ;
      end ;
      ListBox.Repaint ;
    end
    else
      TPopup( self.ListBox.Parent ).IsOpen := False ;  }
  end;

  function TComboEdit.fget_DroppedDown
    : Boolean ;
  begin
  { TODO -cReview : doing nothing???? }
   // Result := TPopup( self.ListBox.Parent ).IsOpen ;
  end;

  procedure TComboEdit.DropDown ;
  begin
    if Assigned( FOnDropDown ) then FOnDropDown( Self ) ;
    inherited DropDown ;
    { TODO -cReview : doing nothing???? }
    //ListBox.SetFocus ;
  end;

//==============================================================================
// TGIS_CSAbstractListHelper
//==============================================================================

  function TGIS_CSAbstractListHelper.doFilter(
    const _obj : TGIS_CSAbstract
  ) : Boolean ;
  var
    unt  : TGIS_CSUnits        ;
    prj  : TGIS_CSProjAbstract ;
    cs   : TGIS_CSCoordinateSystem ;
    area : TGIS_CSArea         ;
  begin
    Result := False ;

    if _obj is TGIS_CSUnits then begin
      unt := _obj as TGIS_CSUnits ;
      if not ShowUnitsLinear then begin
        if unt.UnitsType = TGIS_CSUnitsType.Linear then
          Result := True ;
      end ;
      if not ShowUnitsAngular then begin
        if unt.UnitsType = TGIS_CSUnitsType.Angular then
          Result := True ;
      end;
      if not ShowUnitsAreal then begin
        if unt.UnitsType = TGIS_CSUnitsType.Areal then
          Result := True ;
      end;
      if not ShowUnitsAuto then begin
        if unt.UnitsType = TGIS_CSUnitsType.Auto then
          Result := True ;
      end;
    end
    else if _obj is TGIS_CSProjAbstract then begin
      prj := _obj as TGIS_CSProjAbstract ;
      if not ShowProjRevertible then begin
        if prj.IsRevertible = True then Result := True ;
      end ;
      if not ShowProjUnrevertible then begin
        if prj.IsRevertible = False then Result := True ;
      end ;
      if not ShowProjStandard then begin
        if prj.IsGrid = False then Result := True ;
      end ;
      if not ShowProjGrid then begin
        if prj.IsGrid = True then Result := True ;
      end ;
    end
    else if _obj is TGIS_CSCoordinateSystem then begin
      cs := _obj as TGIS_CSCoordinateSystem ;

      if iAreaEpsg > 1 then begin
        if iAreaEpsg <> cs.AreaEPSG then
          Result := True ;
      end
      else begin
        if GisIsNoWorld( oAreaExtent ) then
          exit
        else
        if ( cs.AreaEPSG = 0 ) and ( iAreaEpsg = 1 ) then
          Result := True
        else
        if cs.AreaEPSG > 0 then begin
          area := CSAreaList.ByEPSG( cs.AreaEPSG ) ;

          if Assigned( area ) then
            if not GisIsContainExtent( oAreaExtent, area.Bounds) then
              Result := True;
        end;
      end;
    end;
  end;

  { TODO -cReview : To review }
  { TComboBox.OnDropDown proxy.
    Fill the combo list based on search criteria.
  }
  procedure TGIS_CSAbstractListHelper.doComboDropDown(
    _sender: TObject
  ) ;
  var
    i   : Integer     ;
    lst : TStringList ;
    s   : String      ;
  begin
    lst := TStringList.Create() ;
    try
      for i:=0 to oList.Count-1 do begin
        if iForcedEPSG <> oList[i].EPSG then
          if oList[i].MasterEPSG <> 0 then begin
            if doFilter( oList.ByEPSG(oList[i].MasterEPSG) ) then continue ;
          end
          else begin
            if doFilter( oList[i] ) then continue ;
          end;

        s := clearWkt( oList[i] ) ;
        if IsStringEmpty( sSearch ) or
           ( Pos( sSearch, UpperCase( s ) ) >= StringFirst )
        then begin
          lst.AddObject( s, oList[i] ) ;
        end;
        if lst.Count > MAX_ITEMS then begin
          lst.AddObject( '    ' + _rsrc( GIS_RS_CS_HELPER_MORE ) , nil ) ;
          break ;
        end;
      end;

      if lst.Count > 0  then begin
        // add empty iterm to fill the scroll box and avoid opening w/o
        // place for all elements in a next search
        if lst.Count <  oCombo.DropDownCount then begin
          for i:=lst.Count to oCombo.DropDownCount-1 do
            lst.Add( ''  ) ;
        end;

        oCombo.Items.Assign(lst);
        oCombo.ItemIndex := 0 ;
      end
      else
        oCombo.Items.Clear ;

    finally
      lst.Free ;
    end;
  end;
  { TODO -cReview : To review }
  { TComboBox.OnChange proxy.
    Update property Object based on selected item
  }
  procedure TGIS_CSAbstractListHelper.doComboChange(
    _sender: TObject
  ) ;
  var
    k : Integer ;
  begin
    if oCombo.ItemIndex >= 0 then begin
      if not Assigned( oCombo.Items.Objects[ oCombo.ItemIndex ] ) then
        oCombo.ItemIndex := 0
      else
        oSelected := TGIS_CSAbstract( oCombo.Items.Objects[ oCombo.ItemIndex ] )
    end else begin
    { TODO -cReview : To review }
//      oSelected := oList[0] ;
      oCombo.OnChange := nil ;
      oCombo.Text := clearWkt( SelectedObject ) ;
      oCombo.OnChange := doComboChange ;
    end;

    k := Pos( sSearch, UpperCase( oCombo.Text ) ) ;
    if k>=StringFirst then begin
      oCombo.SelStart := k-1 ;
      oCombo.SelLength := Length( sSearch ) ;
    end;

    if Assigned( oldOnChange ) then
      oldOnChange( Self ) ;
  end;
  { TODO -cReview : To review }
  { TComboBox.OnKeyPress proxy.
    Manage masic lookup behavior. Select item based on typed phrase.
  }
  procedure TGIS_CSAbstractListHelper.doComboKeyPress(
        _sender  : TObject  ;
    var _key     : Word     ;
    var _keychar : WideChar ;
        _shift   : TShiftState
  ) ;

    procedure find() ;
    var
      i : Integer ;
      k : Integer ;
      s : String  ;
    begin
      if IsStringEmpty( sSearch ) then begin
        oSelected := oList[0] ;
        oCombo.OnChange := nil ;
        oCombo.Text := clearWkt( SelectedObject ) ;
        oCombo.OnChange := doComboChange ;
        if Assigned( oldOnChange ) then
          oldOnChange( Self ) ;
        exit ;
      end;

      for i:=0 to oList.Count-1 do begin
        if iForcedEPSG <> oList[i].EPSG then
          if oList[i].MasterEPSG <> 0 then begin
            if doFilter( oList.ByEPSG(oList[i].MasterEPSG) ) then continue ;
          end
          else begin
            if doFilter( oList[i] ) then continue ;
          end;

        s := clearWkt( oList[i] ) ;
        k := Pos( sSearch, UpperCase( s ) ) ;
        if k>=StringFirst then begin
          oSelected := oList[i] ;
          oCombo.OnChange := nil ;
          oCombo.Text := s ;
          oCombo.OnChange := doComboChange ;
          if Assigned( oldOnChange ) then
            oldOnChange( Self ) ;
          exit ;
        end;
      end;

      // not found - try shorter string
      SetLength( sSearch, Length( sSearch ) -1 ) ;
      find() ;
    end;

  begin
    case _key of
      vkPrior  ,
      vkNext   ,
      vkEnd    ,
      vkHome   ,
      vkLeft   ,
      vkUp     ,
      vkRight  ,
      vkDown   : exit ;
      vkBack   ,
      vkDelete : begin
                   if Length( sSearch ) > 0 then begin
                     SetLength( sSearch, Length( sSearch ) -1 ) ;
                   end ;
                 end ;
      vkEscape : begin // in a case if not interpreted by dialogbox
                   sSearch := '';
                   oCombo.DroppedDown := False ;
                   exit ;
                 end ;
      vkReturn : begin // in a case if not interpreted by dialogbox
                   oCombo.DroppedDown := False ;
                   exit;
                 end ;
    end;

//  if KeyChar = #0 then exit ;

    if _keychar >= ' ' then
      sSearch := sSearch + Uppercase( _keychar ) ;

    find() ;

    _keychar := Char(0) ; // do not display the char
    _key     := 0 ;

     doComboDropDown( self );

     if Length( sSearch ) > 0 then
       oCombo.DroppedDown := True ;

     // fix for bad redrawing of selected item
     oCombo.Repaint  ;
  end;

  procedure TGIS_CSAbstractListHelper.ByEPSG( const _epsg : Integer ) ;
  var
    i : Integer ;
  begin
    iForcedEPSG := _epsg ;
    oCombo.OnChange := nil ;
    try
      for i:=0 to oList.Count-1 do begin
        if iForcedEPSG <> oList[i].EPSG then
          if oList[i].MasterEPSG <> 0 then begin
            if doFilter( oList.ByEPSG(oList[i].MasterEPSG) ) then continue ;
          end
          else begin
            if doFilter( oList[i] ) then continue ;
          end;

        if _epsg = oList[i].EPSG then begin
          oSelected := oList[i] ;
          oCombo.OnChange := nil ;
          oCombo.Text := clearWkt( SelectedObject ) ;
          oCombo.OnChange := doComboChange ;
          sSearch := '' ;
          if Assigned( oldOnChange ) then
            oldOnChange( Self ) ;
          exit ;
        end;
      end ;
    finally
      oCombo.OnChange := doComboChange ;
    end ;
  end;

  constructor TGIS_CSAbstractListHelper.Create(
    const _combobox : TComboEdit;
    const _wktlist : TGIS_CSAbstractList
  ) ;
  var
    c : Char ;
    w : Word ;
  begin
    inherited Create ;

    bUnitsLinear  := True ;
    bUnitsAngular := True ;
    bUnitsAreal   := False ;
    bUnitsAuto    := False ;

    oAreaExtent := GisNoWorld ;
    iAreaEpsg   := 0 ;

    oCombo := _combobox ;
    oList := _wktlist ;

    oCombo.OnPainting := doPaint ;

    oCombo.DropDownEvent     := doComboDropDown ;
    oCombo.OnKeyDown         := doComboKeyPress ;

    oldOnChange              := oCombo.OnChange ;
    oCombo.Text              := '' ;
    oCombo.OnChange          := doComboChange ;

    c := Char(0) ;
    w := 0 ;
    doComboKeyPress( self, w, c, [] ) ;
  end;

  function TGIS_CSAbstractListHelper.clearWkt(
    const _obj : TGIS_CSAbstract
  ) : String ;
  var
    g : TGIS_CSGeographicCoordinateSystem ;
    p : TGIS_CSProjectedCoordinateSystem ;
    e : Integer ;
    a : TGIS_CSArea ;
    t : String ;
    k : Integer ;
  begin
    Result := _obj.FriendlyName ;

    exit ;

    if _obj is TGIS_CSGeographicCoordinateSystem  then begin
      g := _obj as TGIS_CSGeographicCoordinateSystem ;
      if Assigned( g.Datum.Transform ) then begin
        e := g.Datum.Transform.AreaEPSG ;
        a := CSAreaList.ByEPSG( e ) ;
        if Assigned( a ) then begin
          t := a.Description ;
          k := Pos( '-', t ) ;
          if k > 0 then
            t := Copy( t, 1, k-1 ) ;
          k := Pos( ',', t ) ;
          if k > 0 then
            t := Copy( t, 1, k-1 ) ;
          k := Pos( '(', t ) ;
          if k > 0 then
            t := Copy( t, 1, k-1 ) ;
          t := Trim( t ) ;
          Result := Result + ' (' + t  + ')';
        end;
      end ;
    end;
    if _obj is TGIS_CSProjectedCoordinateSystem  then begin
      p := _obj as TGIS_CSProjectedCoordinateSystem ;
      if Assigned( p.Geocs.Datum.Transform ) then begin
        e := p.Geocs.Datum.Transform.AreaEPSG ;
        a := CSAreaList.ByEPSG( e ) ;
        if Assigned( a ) then begin
          t := a.Description ;
          k := Pos( '-', t ) ;
          if k > 0 then
            t := Copy( t, 1, k-1 ) ;
          k := Pos( ',', t ) ;
          if k > 0 then
            t := Copy( t, 1, k-1 ) ;
          k := Pos( '(', t ) ;
          if k > 0 then
            t := Copy( t, 1, k-1 ) ;
          t := Trim( t ) ;
          t := a.Description ;
          Result := Result + ' (' + t  + ')';
        end;
      end ;
    end;

  end ;

{==================================== END =====================================}
end.

