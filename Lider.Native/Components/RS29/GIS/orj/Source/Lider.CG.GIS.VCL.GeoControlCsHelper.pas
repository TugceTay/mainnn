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

unit VCL.GisControlCsHelper ;
{$HPPEMIT '#pragma link "VCL.GisControlCsHelper"'}

{$INCLUDE GisInclude.inc}

interface

uses
  System.Classes,
  System.SysUtils,
  VCL.StdCtrls,

  GisTypes,
  GisCsBase;

type

  /// <summary>
  ///   TComboBox helper
  /// </summary>
  TGIS_CSAbstractListHelper = class
    private
      bLinear           : Boolean ;
      bAngular          : Boolean ;
      bProjRevertible   : Boolean ;
      bProjUnRevertible : Boolean ;
      bProjStandard     : Boolean ;
      bProjGrid         : Boolean ;

      oList       : TGIS_CSAbstractList ;
      oCombo      : TComboBox           ;
      sSearch     : String              ;
      oSelected   : TGIS_CSAbstract     ;
      oldOnChange : TNotifyEvent        ;
      iForcedEPSG : Integer             ;
      oAreaExtent : TGIS_Extent         ;
      iAreaEPSG   : Integer             ;
    private
      function  doFilter        ( const _obj      : TGIS_CSAbstract
                                ) : Boolean ;

      /// <summary>
      ///   TComboBox.OnDropDown proxy.
      ///   Fill the combo list based on search criteria.
      /// </summary>
      procedure doComboDropDown (       _sender   : TObject
                                ) ;

      /// <summary>
      /// TComboBox.OnKeyPress proxy.
      /// Manage masic lookup behavior. Select item based on typed phrase.
      /// </summary>
      procedure doComboKeyPress (       _sender   : TObject   ;
                                  var   _key      : Char

                                ) ;

      /// <summary>
      ///   TComboBox.OnChange proxy.
      ///   Update property Object based on selected item
      /// </summary>
      procedure doComboChange   (       _sender   : TObject
                                ) ;

      /// <summary>
      ///   Return clear WKT name (by replacing '_' with space) and with EPSG number
      ///   embedded.
      /// </summary>
      /// <param name="_obj">
      ///    EPSG.WKT object
      /// </param>
      /// <returns>
      ///   Cleared text.
      /// </returns>
      function  clearWkt        ( const _obj      : TGIS_CSAbstract
                                ) : String ;
    public
      /// <summary>
      ///   Select item in a connected combo box based on EPSG number.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG number based on which item should be selected
      ///   if not found then default item will be selected
      /// </param>
      procedure ByEPSG          ( const _epsg     : Integer
                                ) ;
    public
      /// <summary>
      ///   Create an instance of a helper.
      /// </summary>
      /// <param name="_combobox">
      ///    associated combo box control
      /// </param>
      /// <param name="_wktlist">
      ///    associated EPSG/WKT list
      /// </param>
      constructor Create        ( const _combobox : TComboBox  ;
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
      property ShowUnitsLinear      : Boolean read bLinear
                                              write bLinear ;

      /// <summary>
      ///   If True, show angular units.
      /// </summary>
      property ShowUnitsAngular     : Boolean read  bAngular
                                              write bAngular ;

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

{$IFDEF DCC}
uses
  GisRtl,
  GisFunctions,
  GisCsSystems,
  GisCsProjections ;
{$ENDIF}

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
        end ;
      end ;

      oCombo.Items.Assign(lst);

    finally
      lst.Free ;
    end ;
  end ;

  procedure TGIS_CSAbstractListHelper.doComboChange(
    _sender: TObject
  ) ;
  begin
    if oCombo.ItemIndex >= 0 then begin
      oSelected := TGIS_CSAbstract( oCombo.Items.Objects[ oCombo.ItemIndex ] )
    end else begin
      oSelected := oList[0] ;
      oCombo.Text := clearWkt( SelectedObject ) ;
    end;

    if Assigned( oldOnChange ) then oldOnChange( Self ) ;
  end ;

  procedure TGIS_CSAbstractListHelper.doComboKeyPress(
        _sender : TObject;
    var _key    : Char
  ) ;

    procedure find() ;
    var
      i : Integer ;
      k : Integer ;
      s : String  ;
    begin
      if IsStringEmpty( sSearch ) then begin
        oSelected := oList[0] ;
        oCombo.Text := clearWkt( SelectedObject ) ;

        if Assigned( oldOnChange ) then oldOnChange( Self ) ;

        exit ;
      end ;

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
          oCombo.Text := s ;
          oCombo.SelStart := k-1 ;
          oCombo.SelLength := Length( sSearch ) ;

          if Assigned( oldOnChange ) then oldOnChange( Self ) ;

          exit ;
        end ;
      end ;

      // not found - try shorter string
      SetLength( sSearch, Length( sSearch ) -1 ) ;
      find() ;
    end ;

  begin
    oCombo.DroppedDown := False ;

    if oCombo.SelText = oCombo.Text then
      sSearch := '' ;

    if oCombo.SelLength = 0 then
      sSearch := '' ;

    if _key >= ' ' then
      sSearch := sSearch + UpCase( _key ) ;

    if _key = Char(8) then // backspace
      if Length( sSearch ) > 0 then
        SetLength( sSearch, Length( sSearch ) -1 ) ;

    find() ;

    _key := Char(0) ; // do not display the char
  end ;

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
          oCombo.Text := clearWkt( SelectedObject ) ;
          sSearch := '' ;
          oCombo.SelStart := 0 ;
          oCombo.SelLength := 0 ;

          if Assigned( oldOnChange ) then oldOnChange( Self ) ;

          exit ;
        end ;
      end ;
    finally
      oCombo.OnChange := doComboChange ;
    end ;
  end ;

  constructor TGIS_CSAbstractListHelper.Create(
    const _combobox : TComboBox;
    const _wktlist : TGIS_CSAbstractList
  ) ;
  var
    c : Char ;
  begin
    inherited Create ;

    bLinear  := True ;
    bAngular := True ;

    oAreaExtent := GisNoWorld ;
    iAreaEpsg   := 0 ;

    oCombo := _combobox ;

    oList := _wktlist ;

    oCombo.Sorted := True ;
    oCombo.AutoComplete := False ;

    oCombo.OnDropDown := doComboDropDown ;
    oCombo.OnKeyPress := doComboKeyPress ;
    oldOnChange       := oCombo.OnChange ;
    oCombo.OnChange   := doComboChange ;

    oCombo.Text := '' ;
    c := Char(0) ;

    doComboKeyPress( self, c ) ;
  end ;

  function TGIS_CSAbstractListHelper.clearWkt(
    const _obj : TGIS_CSAbstract
  ) : String ;
//  var
//    o : TGIS_CSCoordinateSystem ;
  begin
    Result := _obj.FriendlyName ;

    { TODO -cCritical : Add proper code for DK11 }
//    if _obj is TGIS_CSCoordinateSystem  then begin
//      o := _obj as TGIS_CSCoordinateSystem ;
//      if Assigned( o.Area ) then begin
//        Result := Result + ' : ' + o.Area.Description ;
//      end;
//    end;

  end ;

{==================================== END =====================================}
end.

