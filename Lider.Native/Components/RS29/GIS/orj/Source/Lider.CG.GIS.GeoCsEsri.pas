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
  Old ESRI PRJ files support (like used in ADF).
  Supported projections are UTM, GEOGRAPHICS, STATEPLANE,GREATBRITIAN_GRID
}

{$IFDEF DCC}
  unit GisCsEsri ;
  {$HPPEMIT '#pragma link "GisCsEsri"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF COCOA}
  namespace TatukGIS.OSDK ;
{$ENDIF}
{$IFDEF ISLAND}
  namespace TatukGIS ;
{$ENDIF}

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,

    GisCsSystems ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF COCOA}
  uses
    TatukGIS.OSDK ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    remobjects.elements.rtl.*,
    TatukGIS.RTL ;
{$ENDIF}

type

  {#gendoc:hide:GENXDK}
  /// <summary>
  ///   Class to create Coordinate System Object from ESRI like projection file.
  /// </summary>
  TGIS_CSFactoryEsri = {$IFDEF OXYGENE} public {$ENDIF} class
    public
      /// <summary>
      ///   Construct CS form ESRI like projection file.
      /// </summary>
      /// <param name="_file">
      ///   path to the file
      /// </param>
      /// <returns>
      ///   Constructed/Found coordinate system or nil.
      /// </returns>
      class function BuildCsFromFile( const   _file : String
                                    ) : TGIS_CSCoordinateSystem ;
                                    {$IFDEF GIS_STATIC} static; {$ENDIF}
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisRtl,
    GisClasses,
    GisCsBase,
    GisCsFactory ;
{$ENDIF}

  class function TGIS_CSFactoryEsri.BuildCsFromFile(
     const  _file : String
  ) : TGIS_CSCoordinateSystem ;
  var
    i      : Integer        ;
    lst1   : TStringList    ;
    lst2   : TStringList    ;
    tkn    : TGIS_Tokenizer ;
    txt    : String         ;
    code   : Integer        ;
    epsg   : Integer        ;
    unt    : TGIS_CSUnits   ;
    gcs    : TGIS_CSGeographicCoordinateSystem ;
    pcs    : TGIS_CSProjectedCoordinateSystem  ;

    function parseYshift : Double ;
    var
      tmp : String ;
    begin
      tmp := lst2.Values[ 'YSHIFT' ] ;

      if IsStringEmpty( tmp ) then
        Result := 0
      else
        Result := DotStrToFloat( tmp ) ;
    end;

    function parseZone : Integer ;
    var
      tmp : String ;
    begin
      tmp := lst2.Values[ 'ZONE' ] ;

      Result := StrToInt( tmp ) ;
    end;

    function isUnits : Boolean ;
    var
      tmp : String ;
    begin
      tmp := lst2.Values[ 'UNITS' ] ;

      Result := not IsStringEmpty( tmp ) ;
    end;

    function parseUnits : TGIS_CSUnits ;
    var
      tmp : String ;
    begin
      tmp := lst2.Values[ 'UNITS' ] ;

      if      tmp = 'DD'     then  Result := CSUnitsList.ByEPSG( 9122 )
      else if tmp = 'METERS' then  Result := CSUnitsList.ByEPSG( 9001 )
      else if tmp = 'FEET'   then  Result := CSUnitsList.ByEPSG( 9003 )
      else                         Result := nil  ;
    end;

    function isDatum : Boolean ;
    var
      tmp : String ;
    begin
      tmp := lst2.Values[ 'DATUM' ] ;

      Result := not IsStringEmpty( tmp ) ;
    end;

    function parseDatum : TGIS_CSGeographicCoordinateSystem ;
    var
      tmp : String ;
    begin
      tmp := lst2.Values[ 'DATUM' ] ;

      Result := CSGeographicCoordinateSystemList.ByWKT( tmp ) ;

      if not assigned( Result ) then
        Result := CSGeographicCoordinateSystemList.ByEPSG( 4326 ) ;
    end;

  begin
    Result := nil ;
    try
      lst2 := TStringList.Create ;
      try
        lst1 := TStringList.Create ;
        try
          lst1.LoadFromFile( _file ) ;

          for i:= 0 to lst1.Count -1 do begin
            txt := lst1[i] ;

            tkn := TGIS_Tokenizer.Create ;
            try
              tkn.Execute( UpperCase( txt ), [' '] );

              if      tkn.Result.Count = 2  then
                      lst2.Values[ tkn.Result[0] ] := tkn.Result[1]
              else if tkn.Result.Count = 1 then
                      lst2.Values[ tkn.Result[0] ] := '' ;
            finally
              FreeObject( tkn ) ;
            end;
          end;
        finally
          FreeObject( lst1 )
        end;

        unt := nil ;
        txt := lst2.Values[ 'PROJECTION' ] ;

        if      txt = 'GEOGRAPHIC' then begin
                gcs := parseDatum ;
                unt := parseUnits ;

                if not assigned( unt ) then exit ;

                if unt.EPSG <> 9122  then exit ;

                Result := gcs ;
        end
        else if txt = 'UTM'        then begin
                epsg := 0 ;

                if parseYshift = 0 then begin
                  case parseZone of
                    01 : epsg := 32601 ;
                    02 : epsg := 32602 ;
                    03 : epsg := 32603 ;
                    04 : epsg := 32604 ;
                    05 : epsg := 32605 ;
                    06 : epsg := 32606 ;
                    07 : epsg := 32607 ;
                    08 : epsg := 32608 ;
                    09 : epsg := 32609 ;
                    10 : epsg := 32610 ;
                    11 : epsg := 32611 ;
                    12 : epsg := 32612 ;
                    13 : epsg := 32613 ;
                    14 : epsg := 32614 ;
                    15 : epsg := 32615 ;
                    16 : epsg := 32616 ;
                    17 : epsg := 32617 ;
                    18 : epsg := 32618 ;
                    19 : epsg := 32619 ;
                    20 : epsg := 32620 ;
                    21 : epsg := 32621 ;
                    22 : epsg := 32622 ;
                    23 : epsg := 32623 ;
                    24 : epsg := 32624 ;
                    25 : epsg := 32625 ;
                    26 : epsg := 32626 ;
                    27 : epsg := 32627 ;
                    28 : epsg := 32628 ;
                    29 : epsg := 32629 ;
                    30 : epsg := 32630 ;
                    31 : epsg := 32631 ;
                    32 : epsg := 32632 ;
                    33 : epsg := 32633 ;
                    34 : epsg := 32634 ;
                    35 : epsg := 32635 ;
                    36 : epsg := 32636 ;
                    37 : epsg := 32637 ;
                    38 : epsg := 32638 ;
                    39 : epsg := 32639 ;
                    40 : epsg := 32640 ;
                    41 : epsg := 32641 ;
                    42 : epsg := 32642 ;
                    43 : epsg := 32643 ;
                    44 : epsg := 32644 ;
                    45 : epsg := 32645 ;
                    46 : epsg := 32646 ;
                    47 : epsg := 32647 ;
                    48 : epsg := 32648 ;
                    49 : epsg := 32649 ;
                    50 : epsg := 32650 ;
                    51 : epsg := 32651 ;
                    52 : epsg := 32652 ;
                    53 : epsg := 32653 ;
                    54 : epsg := 32654 ;
                    55 : epsg := 32655 ;
                    56 : epsg := 32656 ;
                    57 : epsg := 32657 ;
                    58 : epsg := 32658 ;
                    59 : epsg := 32659 ;
                    60 : epsg := 32660 ;
                  end
                end
                else begin
                  case parseZone of
                    01 : epsg := 32701 ;
                    02 : epsg := 32702 ;
                    03 : epsg := 32703 ;
                    04 : epsg := 32704 ;
                    05 : epsg := 32705 ;
                    06 : epsg := 32706 ;
                    07 : epsg := 32707 ;
                    08 : epsg := 32708 ;
                    09 : epsg := 32709 ;
                    10 : epsg := 32710 ;
                    11 : epsg := 32711 ;
                    12 : epsg := 32712 ;
                    13 : epsg := 32713 ;
                    14 : epsg := 32714 ;
                    15 : epsg := 32715 ;
                    16 : epsg := 32716 ;
                    17 : epsg := 32717 ;
                    18 : epsg := 32718 ;
                    19 : epsg := 32719 ;
                    20 : epsg := 32720 ;
                    21 : epsg := 32721 ;
                    22 : epsg := 32722 ;
                    23 : epsg := 32723 ;
                    24 : epsg := 32724 ;
                    25 : epsg := 32725 ;
                    26 : epsg := 32726 ;
                    27 : epsg := 32727 ;
                    28 : epsg := 32728 ;
                    29 : epsg := 32729 ;
                    30 : epsg := 32730 ;
                    31 : epsg := 32731 ;
                    32 : epsg := 32732 ;
                    33 : epsg := 32733 ;
                    34 : epsg := 32734 ;
                    35 : epsg := 32735 ;
                    36 : epsg := 32736 ;
                    37 : epsg := 32737 ;
                    38 : epsg := 32738 ;
                    39 : epsg := 32739 ;
                    40 : epsg := 32740 ;
                    41 : epsg := 32741 ;
                    42 : epsg := 32742 ;
                    43 : epsg := 32743 ;
                    44 : epsg := 32744 ;
                    45 : epsg := 32745 ;
                    46 : epsg := 32746 ;
                    47 : epsg := 32747 ;
                    48 : epsg := 32748 ;
                    49 : epsg := 32749 ;
                    50 : epsg := 32750 ;
                    51 : epsg := 32751 ;
                    52 : epsg := 32752 ;
                    53 : epsg := 32753 ;
                    54 : epsg := 32754 ;
                    55 : epsg := 32755 ;
                    56 : epsg := 32756 ;
                    57 : epsg := 32757 ;
                    58 : epsg := 32758 ;
                    59 : epsg := 32759 ;
                    60 : epsg := 32760 ;
                  end ;
                end ;

                pcs := TGIS_CSProjectedCoordinateSystem(
                         TGIS_CSFactory.ByEPSG( epsg )
                       ) ;

                if not assigned( pcs ) then exit ;
                if pcs.EPSG = 0        then exit ;

                unt := parseUnits ;
                gcs := parseDatum ;

                if not assigned( unt ) then exit ;

                Result := CSProjectedCoordinateSystemList.Prepare(
                  -1,
                  pcs.WKT + ' ' + parseUnits.WKT,
                  gcs.EPSG,
                  unt.EPSG,
                  pcs.Projection.EPSG,
                  pcs.Projection.Parameters
                ) ;
        end
        else if txt = 'STATEPLANE' then begin
                code := 0 ;
                epsg := 0 ;

                txt := lst2.Values[ 'ZONE' ] ;
                if not IsStringEmpty( txt ) then begin
                  code := parseZone ;

                  case code of
                    3101 : code :=  101 ;
                    3126 : code :=  102 ;
                    3151 : code :=  201 ;
                    3176 : code :=  202 ;
                    3201 : code :=  203 ;
                    3226 : code :=  301 ;
                    3251 : code :=  302 ;
                    3276 : code :=  401 ;
                    3301 : code :=  402 ;
                    3326 : code :=  403 ;
                    3351 : code :=  404 ;
                    3376 : code :=  405 ;
                    3401 : code :=  406 ;
                    3426 : code :=  407 ;
                    3451 : code :=  501 ;
                    3476 : code :=  502 ;
                    3501 : code :=  503 ;
                    3526 : code :=  600 ;
                    3551 : code :=  700 ;
                    3576 : code :=  903 ;
                    3601 : code :=  901 ;
                    3626 : code :=  902 ;
                    3651 : code := 1001 ;
                    3676 : code := 1002 ;
                    3701 : code := 1101 ;
                    3726 : code := 1102 ;
                    3751 : code := 1103 ;
                    3776 : code := 1201 ;
                    3801 : code := 1202 ;
                    3826 : code := 1301 ;
                    3851 : code := 1302 ;
                    3876 : code := 1401 ;
                    3901 : code := 1402 ;
                    3926 : code := 1501 ;
                    3951 : code := 1502 ;
                    3976 : code := 1601 ;
                    4001 : code := 1602 ;
                    4026 : code := 1701 ;
                    4051 : code := 1702 ;
                    4076 : code := 1801 ;
                    4101 : code := 1802 ;
                    4126 : code := 1900 ;
                    4151 : code := 2001 ;
                    4176 : code := 2002 ;
                    4201 : code := 2101 ;
                    4226 : code := 2102 ;
                    4251 : code := 2103 ;
                    4276 : code := 2201 ;
                    4301 : code := 2202 ;
                    4326 : code := 2203 ;
                    4351 : code := 2301 ;
                    4376 : code := 2302 ;
                    4401 : code := 2401 ;
                    4426 : code := 2402 ;
                    4451 : code := 2403 ;
                    4476 : code := 2501 ;
                    4501 : code := 2502 ;
                    4526 : code := 2503 ;
                    4551 : code := 2601 ;
                    4576 : code := 2602 ;
                    4601 : code := 2701 ;
                    4626 : code := 2702 ;
                    4651 : code := 2703 ;
                    4676 : code := 2800 ;
                    4701 : code := 2900 ;
                    4726 : code := 3001 ;
                    4751 : code := 3002 ;
                    4776 : code := 3003 ;
                    4801 : code := 3101 ;
                    4826 : code := 3102 ;
                    4851 : code := 3103 ;
                    4876 : code := 3104 ;
                    4901 : code := 3200 ;
                    4926 : code := 3301 ;
                    4951 : code := 3302 ;
                    4976 : code := 3401 ;
                    5001 : code := 3402 ;
                    5026 : code := 3501 ;
                    5051 : code := 3502 ;
                    5076 : code := 3601 ;
                    5101 : code := 3602 ;
                    5126 : code := 3701 ;
                    5151 : code := 3702 ;
                    5176 : code := 3800 ;
                    5201 : code := 3901 ;
                    5226 : code := 3902 ;
                    5251 : code := 4001 ;
                    5276 : code := 4002 ;
                    5301 : code := 4100 ;
                    5326 : code := 4201 ;
                    5351 : code := 4202 ;
                    5376 : code := 4203 ;
                    5401 : code := 4204 ;
                    5426 : code := 4205 ;
                    5451 : code := 4301 ;
                    5476 : code := 4302 ;
                    5501 : code := 4303 ;
                    5526 : code := 4400 ;
                    5551 : code := 4501 ;
                    5576 : code := 4502 ;
                    5601 : code := 4601 ;
                    5626 : code := 4602 ;
                    5651 : code := 4701 ;
                    5676 : code := 4702 ;
                    5701 : code := 4801 ;
                    5726 : code := 4802 ;
                    5751 : code := 4803 ;
                    5776 : code := 4901 ;
                    5801 : code := 4902 ;
                    5826 : code := 4903 ;
                    5851 : code := 4904 ;
                    5876 : code := 5101 ;
                    5901 : code := 5102 ;
                    5926 : code := 5103 ;
                    5951 : code := 5104 ;
                    5976 : code := 5105 ;
                    6001 : code := 5201 ;
                    6026 : code := 5200 ;
                    6051 : code := 5201 ;
                    //   : code := 5202 ;
                    6076 : code := 5200 ;
                    6101 : code := 5001 ;
                    6126 : code := 5002 ;
                    6151 : code := 5003 ;
                    6176 : code := 5004 ;
                    6201 : code := 5005 ;
                    6226 : code := 5006 ;
                    6251 : code := 5007 ;
                    6276 : code := 5008 ;
                    6301 : code := 5009 ;
                    6326 : code := 5010 ;
                    6351 : code := 2111 ;
                    6376 : code := 2112 ;
                    6401 : code := 2113 ;
                    6426 : code := 1703 ;
                  end ;

                end;

                txt := lst2.Values[ 'FIPSZONE' ] ;
                if not IsStringEmpty( txt ) then begin
                  code := StrToInt( txt ) ;
                end ;

                case code of
                  0101 : epsg := 102629 ;
                  0102 : epsg := 102630 ;
                  0201 : epsg := 102648 ;
                  0202 : epsg := 102649 ;
                  0203 : epsg := 102650 ;
                  0301 : epsg := 102650 ;
                  0302 : epsg := 102652 ;
                  0401 : epsg := 102641 ;
                  0402 : epsg := 102642 ;
                  0403 : epsg := 102643 ;
                  0404 : epsg := 102644 ;
                  0405 : epsg := 102645 ;
                  0406 : epsg := 102646 ;
                  0407 : epsg := 0      ;
                  0501 : epsg := 102653 ;
                  0502 : epsg := 102654 ;
                  0503 : epsg := 102655 ;
                  0600 : epsg := 102656 ;
                  0700 : epsg := 102657 ;
                  0901 : epsg := 102658 ;
                  0902 : epsg := 102659 ;
                  0903 : epsg := 102600 ;
                  1001 : epsg := 102666 ;
                  1002 : epsg := 102667 ;
                  1101 : epsg := 102668 ;
                  1102 : epsg := 102669 ;
                  1103 : epsg := 102670 ;
                  1201 : epsg := 102671 ;
                  1202 : epsg := 102672 ;
                  1301 : epsg := 102673 ;
                  1302 : epsg := 102674 ;
                  1401 : epsg := 102675 ;
                  1402 : epsg := 102676 ;
                  1501 : epsg := 102677 ;
                  1502 : epsg := 102679 ;
                  1601 : epsg := 102679 ;
                  1602 : epsg := 102680 ;
                  1701 : epsg := 102681 ;
                  1702 : epsg := 102682 ;
                  1801 : epsg := 102684 ;
                  1802 : epsg := 102684 ;
                  1900 : epsg := 102685 ;
                  2001 : epsg := 102686 ;
                  2002 : epsg := 102687 ;
                  2111 : epsg := 102688 ;
                  2112 : epsg := 102689 ;
                  2113 : epsg := 102690 ;
                  2201 : epsg := 102691 ;
                  2202 : epsg := 102692 ;
                  2203 : epsg := 102693 ;
                  2301 : epsg := 102694 ;
                  2302 : epsg := 102695 ;
                  2401 : epsg := 102696 ;
                  2402 : epsg := 102697 ;
                  2403 : epsg := 102698 ;
                  2500 : epsg := 102700 ;
                  2501 : epsg := 0      ;
                  2502 : epsg := 0      ;
                  2503 : epsg := 0      ;
                  2600 : epsg := 102704 ;
                  2601 : epsg := 0      ;
                  2602 : epsg := 0      ;
                  2701 : epsg := 102707 ;
                  2702 : epsg := 102708 ;
                  2703 : epsg := 102709 ;
                  2800 : epsg := 102710 ;
                  2900 : epsg := 102711 ;
                  3001 : epsg := 102712 ;
                  3002 : epsg := 102713 ;
                  3003 : epsg := 102714 ;
                  3101 : epsg := 102715 ;
                  3102 : epsg := 102716 ;
                  3103 : epsg := 102717 ;
                  3104 : epsg := 102718 ;
                  3200 : epsg := 102719 ;
                  3301 : epsg := 102720 ;
                  3302 : epsg := 102721 ;
                  3401 : epsg := 102722 ;
                  3402 : epsg := 102723 ;
                  3501 : epsg := 102724 ;
                  3502 : epsg := 102725 ;
                  3601 : epsg := 102726 ;
                  3602 : epsg := 102727 ;
                  3701 : epsg := 102728 ;
                  3702 : epsg := 102729 ;
                  3800 : epsg := 102730 ;
                  3900 : epsg := 102733 ;
                  3901 : epsg := 0      ;
                  3902 : epsg := 0      ;
                  4001 : epsg := 102734 ;
                  4002 : epsg := 102735 ;
                  4100 : epsg := 102736 ;
                  4201 : epsg := 102737 ;
                  4202 : epsg := 102738 ;
                  4203 : epsg := 102739 ;
                  4204 : epsg := 102740 ;
                  4205 : epsg := 102741 ;
                  4301 : epsg := 102742 ;
                  4302 : epsg := 102743 ;
                  4303 : epsg := 102744 ;
                  4400 : epsg := 102745 ;
                  4501 : epsg := 102746 ;
                  4502 : epsg := 102747 ;
                  4601 : epsg := 102748 ;
                  4602 : epsg := 102749 ;
                  4701 : epsg := 102750 ;
                  4702 : epsg := 102751 ;
                  4801 : epsg := 102751 ;
                  4802 : epsg := 102753 ;
                  4803 : epsg := 102754 ;
                  4901 : epsg := 102755 ;
                  4902 : epsg := 102756 ;
                  4903 : epsg := 102757 ;
                  4904 : epsg := 102758 ;
                  5001 : epsg := 102631 ;
                  5002 : epsg := 102632 ;
                  5003 : epsg := 102633 ;
                  5004 : epsg := 102634 ;
                  5005 : epsg := 102635 ;
                  5006 : epsg := 102636 ;
                  5007 : epsg := 102637 ;
                  5008 : epsg := 102638 ;
                  5009 : epsg := 102639 ;
                  5010 : epsg := 102640 ;
                  5101 : epsg := 102661 ;
                  5102 : epsg := 102662 ;
                  5103 : epsg := 102663 ;
                  5104 : epsg := 102664 ;
                  5105 : epsg := 102665 ;
                  5200 : epsg := 102761 ;
                  5201 : epsg := 0      ;
                  5202 : epsg := 0      ;
                  5300 : epsg := 0      ;
                  5400 : epsg := 65161  ;
                end;

                pcs := TGIS_CSProjectedCoordinateSystem(
                         TGIS_CSFactory.ByEPSG( epsg )
                       ) ;

                if not assigned( pcs ) then exit ;
                if pcs.EPSG = 0        then exit ;

                unt := parseUnits ;
                gcs := parseDatum ;

                if not assigned( unt ) then exit ;

                Result := CSProjectedCoordinateSystemList.Prepare(
                  -1,
                  pcs.WKT + ' ' + parseUnits.WKT,
                  gcs.EPSG,
                  unt.EPSG,
                  pcs.Projection.EPSG,
                  pcs.Projection.Parameters
                ) ;
        end
        else if txt = 'GREATBRITIAN_GRID' then begin
                epsg := 27700 ;
                pcs := TGIS_CSProjectedCoordinateSystem(
                         TGIS_CSFactory.ByEPSG( epsg )
                       ) ;

                if not assigned( pcs ) then exit ;
                if pcs.EPSG = 0        then exit ;

                if not isDatum then
                  gcs := pcs.Geocs
                else
                  gcs := parseDatum ;

                if not isUnits then
                  unt := pcs.Units
                else
                  unt := parseUnits ;

                if not assigned( unt ) then exit ;

                Result := CSProjectedCoordinateSystemList.Prepare(
                  -1,
                  pcs.WKT + ' ' + parseUnits.WKT,
                  gcs.EPSG,
                  unt.EPSG,
                  pcs.Projection.EPSG,
                  pcs.Projection.Parameters
                )
        end ;
      finally
        FreeObject( lst2 )
      end;
    except
    end ;

  end;

//==================================== END =====================================
end.

