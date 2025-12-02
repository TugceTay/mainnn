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
  Coordinate System factory objects
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoCsFactory ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoCsFactory"'}
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

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoCsSystems ;
{$ENDIF}
{$IFDEF CLR}
  uses
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    remobjects.elements.rtl.*,
    TatukGIS.RTL ;
{$ENDIF}

type

  /// <summary>
  ///   Class to create Coordinate System Object from WKT or EPSG codes.
  /// </summary>
  TGIS_CSFactory = {$IFDEF OXYGENE} public {$ENDIF} class

    public
      /// <summary>
      ///   Create (or find existing) definition from provided WKT string.
      /// </summary>
      /// <param name="_wkt">
      ///   WKT string; full WKT or just a name of entity like 'GRS_1980'
      /// </param>
      /// <returns>
      ///   Found definition, newly created one or nil.
      /// </returns>
      class function ByWKT    ( const _wkt  : String
                              ) : TGIS_CSCoordinateSystem ;
                              {$IFDEF GIS_STATIC} static; {$ENDIF}
      /// <summary>
      ///   Find definition matching provided authority string.
      /// </summary>
      /// <param name="_wkt">
      ///   authority name string like EPSG number or CRS name
      /// </param>
      /// <returns>
      ///   Found definition or nil.
      /// </returns>
      class function Authority( const _wkt  : String
                              ) : String ;
                              {$IFDEF GIS_STATIC} static; {$ENDIF}
      /// <summary>
      ///   Find definition matching provided EPSG number.
      /// </summary>
      /// <param name="_epsg">
      ///   EPSG code to be found
      /// </param>
      /// <returns>
      ///   Found definition or nil.
      /// </returns>
      class function ByEPSG   ( const _epsg : Integer
                              ) : TGIS_CSCoordinateSystem ;
                              {$IFDEF GIS_STATIC} static; {$ENDIF}
      /// <summary>
      ///   Create  (or find existing) definition from provided file with WKT 
      ///   (like .prj file).
      /// </summary>
      /// <param name="_path">
      ///   path to file with WKT string
      /// </param>
      /// <returns>
      ///   Found definition, newly created one or nil.
      /// </returns>
      class function ByWKTFile( const _path : String
                              ) : TGIS_CSCoordinateSystem ;
                              {$IFDEF GIS_STATIC} static; {$ENDIF}
    {$IFNDEF OXYGENE} private {$ELSE} assembly {$ENDIF}
      class procedure byWKTInternal(
                               const _wkt       : String                  ;
                               var   _cs        : TGIS_CSCoordinateSystem ;
                               var   _authority : String
                              ) ;
                              {$IFDEF GIS_STATIC} static; {$ENDIF}
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Math,
    System.Variants,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoCsBase,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoCsProjections;
{$ENDIF}

type
  T_TokenType = ( ValString,
                  ValFloat,
                  Section,
                  EndOfSection,
                  Eof,
                  Unknown
                ) ;

  T_Token = record
    SubType             : T_TokenType ;
    Value               : Variant     ;
  end ;

  T_ToWgs84 = record
    Any                 : Boolean ;
    A                   : Double  ;
    B                   : Double  ;
    C                   : Double  ;
    D                   : Double  ;
    E                   : Double  ;
    F                   : Double  ;
    G                   : Double  ;
  end ;

  T_Datum = record
    WKT                 : String    ;
    EPSG                : Integer   ;
    TOWGS84             : T_ToWgs84 ;
  end ;

  T_Primem = record
    WKT                 : String  ;
    EPSG                : Integer ;
    Longitude           : Double  ;
  end ;

  T_Spheroid = record
    WKT                 : String  ;
    EPSG                : Integer ;
    SemiMajor           : Double  ;
    InverseFlattering   : Double  ;
  end ;

  T_Units = record
    WKT                 : String  ;
    EPSG                : Integer ;
    Factor              : Double  ;
  end ;

  T_Geogcs = record
    WKT                 : String     ;
    EPSG                : Integer    ;
    Datum               : T_Datum    ;
    Primem              : T_Primem   ;
    Spheroid            : T_Spheroid ;
    Units               : T_Units    ;
  end ;

  T_Projection = record
    WKT                 : String  ;
    EPSG                : Integer ;
  end ;

  T_Projcs = record
    WKT                 : String                ;
    EPSG                : Integer               ;
    Projection          : T_Projection          ;
    Parameters          : TGIS_CSProjParameters ;
    Units               : T_Units               ;
  end ;

  T_Parser = class
    srcWKT : String   ;
    uUid   : Integer  ;
    iPos   : Integer  ;
    Geogcs : T_Geogcs ;
    Projcs : T_Projcs ;

    // Generate new unique temporary EPSG code for
    // items not recognized in an EPSG database.
    // return temporary EPSG code
    function  genEPSG                          : Integer ;

    // Extract single token from the WKT string.
    // return token
    function  getToken                         : T_Token ;

    // Parse GEOGCS section of the WKT.
    procedure parseGEOGCS                      ;

    // Parse AUTHORITY section of the WKT GEOGCS section.
    procedure parseGEOGCS_AUTHORITY            ;

    // Parse DATUM section of the WKT GEOGCS section.
    procedure parseGEOGCS_DATUM                ;

    // Parse AUTHORITY section of the WKT GEOGCS DATUM section.
    procedure parseGEOGCS_DATUM_AUTHORITY      ;

    // Parse SPHEROID section of the WKT GEOGCS section.
    procedure parseGEOGCS_SPHEROID             ;

    // Parse SPHEROID section of the WKT GEOGCS SPHEROID section.
    procedure parseGEOGCS_SPHEROID_AUTHORITY   ;

    // Parse TOWGS84 section of the WKT GEOGCS section.
    procedure parseGEOGCS_TOWGS84              ;

    // Parse PRIMEM section of the WKT GEOGCS section.
    procedure parseGEOGCS_PRIMEM               ;

    // Parse AUTHORITY section of the WKT GEOGCS PRIMEM section.
    procedure parseGEOGCS_PRIMEM_AUTHORITY     ;

    // Parse UNIT section of the WKT GEOGCS section.
    procedure parseGEOGCS_UNIT                 ;

    // Parse AUTHORITY section of the WKT GEOGCS UNIT section.
    procedure parseGEOGCS_UNIT_AUTHORITY       ;

    // Parse COMPD_CS section of the WKT.
    procedure parseCOMPD_CS                    ;

    // Parse PROJCS section of the WKT.
    procedure parsePROJCS                      ;

    // Parse AUTHORITY section of the WKT GEOGCS DATUM section.
    procedure parsePROJCS_AUTHORITY            ;

    // Parse PROJECTION section of the WKT PROJCS section.
    procedure parsePROJCS_PROJECTION           ;

    // Parse AUTHORITY section of the WKT PROJCS PROJECTION section.
    procedure parsePROJCS_PROJECTION_AUTHORITY ;

    // Parse PARAMETER section of the WKT PROJCS section.
    procedure parsePROJCS_PARAMETER            ;

    // Parse AUTHORITY section of the WKT PROJCS PROJECTION section.
    procedure parsePROJCS_PARAMETER_AUTHORITY  ;

    // Parse UNIT section of the WKT PROJCS section.
    procedure parsePROJCS_UNIT                 ;

    // Parse AUTHORITY section of the WKT PROJCS UNIT section.
    procedure parsePROJCS_UNIT_AUTHORITY       ;

    // Parse any unrecognized section of the WKT.
    procedure parseUNKNOWN                     ;

  public

    // Parse projection WKT.
    // _wkt WKT to be parsed
    procedure Parse       ( const _wkt  : String
                          ) ;
  end;

//==============================================================================
//  T_Parser
//==============================================================================

  function T_Parser.getToken : T_Token ;
  var
    c     : Char            ;
    state : Integer         ;
    val   : TStringBuilder  ;
  begin
    val := TStringBuilder.Create( 255 ) ;
    try
      state := 0 ;
      while iPos <= StringLast(srcWKT) do begin
        c := srcWKT[ iPos ] ;

        case ord(c) of
          $201c,  // Left Double Quotation Mark
          $201d:  // Right Double Quotation Mark
            begin
              c := '"' ;
            end ;
        end;

        case state of
          0 : begin // skip spaces and periods etc.
                case c of
                  ' ',
                  ',',
                  #10,
                  #13 : inc( iPos ) ;
                  else  state := 1 ;
                end ;
              end ;
          1 : begin // collect name
                case c of
                  '"' : begin
                          inc( iPos )  ;
                          {$IFDEF JAVA}
                            val.setLength(0) ;
                          {$ELSE}
                            val.Length := 0 ;
                          {$ENDIF}
                          state := 2  ;
                        end ;
                  ']' : begin
                          inc( iPos ) ;
                          Result.SubType := T_TokenType.EndOfSection ;
                          exit ;
                        end ;
                  '+',
                  '-',
                  '0',
                  '1',
                  '2',
                  '3',
                  '4',
                  '5',
                  '6',
                  '7',
                  '8',
                  '9' : state := 3 ;
                  else  state := 4 ;

                end ;
              end ;
          2 : begin // collect string
                case c of
                  '"' : begin // end of string
                          inc( iPos ) ;
                          Result.SubType := T_TokenType.ValString ;
                          Result.Value := val.ToString() ;
                          exit ;
                        end ;
                  else  begin // collect string
                          inc( iPos ) ;
                          val.Append( c ) ;
                        end ;
                end ;
              end ;
          3 : begin // collect number
                case c of
                  ' ' : inc( iPos ) ; // skip spaces
                  '+',
                  '-',
                  '.',
                  'E',
                  'e',
                  '0',
                  '1',
                  '2',
                  '3',
                  '4',
                  '5',
                  '6',
                  '7',
                  '8',
                  '9' : begin // collect value
                          inc( iPos ) ;
                          val.Append( c ) ;
                        end ;
                  ',',
                  ')',
                  ']',
                  #10,
                  #13 : begin // end of value
                          Result.SubType := T_TokenType.ValFloat ;
                          try
                            Result.Value := DotStrToFloat( val.ToString() ) ;
                          except
                            Result.SubType := T_TokenType.Unknown ;
                          end ;
                          exit ;
                        end ;
                   else begin // end of value
                          Result.SubType := T_TokenType.Unknown ;
                          exit ;
                        end ;
                end ;
              end ;
          4 : begin // collect segment name
                case c of
                  ' ',
                  #10,
                  #13 : inc( iPos ) ; // skip spaces etc.
                  '[' : begin      // collected name
                          inc( iPos ) ;
                          Result.SubType := T_TokenType.Section ;
                          Result.Value   := val.ToString    ;
                          exit ;
                        end ;
                  ']' : begin      // collected unquoted string
                          Result.SubType := T_TokenType.ValString  ;
                          Result.Value   := val.ToString    ;
                          exit ;
                        end ;
                  else  begin // collect segment
                          inc( iPos ) ;
                          val.Append( UpCase( c ) ) ;
                        end ;
                end ;
              end ;
          5 : begin
                assert( False, 'untested case' ) ;
              end ;
        end ;
      end ;

      Result.SubType := T_TokenType.Eof ;

    finally
      FreeObject( val ) ;
    end;
  end ;

  function T_Parser.genEPSG : Integer ;
  begin
    inc( uUid ) ;
    Result := uUid ;
  end ;

  procedure T_Parser.parseGEOGCS ;
  var
    t     : T_Token              ;
    dat   : TGIS_CSDatum         ;
    sph   : TGIS_CSEllipsoid     ;
    pri   : TGIS_CSPrimeMeridian ;
    unt   : TGIS_CSUnits         ;
    gcs   : TGIS_CSGeographicCoordinateSystem  ;
  begin

    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                Geogcs.WKT := VarToString( t.Value ) ;
              end
      else if t.SubType = T_TokenType.Section then begin
                if      VarEqual( t.Value, 'AUTHORITY' ) then
                  parseGEOGCS_AUTHORITY
                else if VarEqual( t.Value, 'DATUM'     ) then
                  parseGEOGCS_DATUM
                else if VarEqual( t.Value, 'PRIMEM'    ) then
                  parseGEOGCS_PRIMEM
                else if VarEqual( t.Value, 'UNIT'      ) then
                  parseGEOGCS_UNIT
                else
                  parseUNKNOWN ;
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end;

    until t.SubType = T_TokenType.Eof ;

    if Geogcs.EPSG > 0 then begin
      gcs := CSGeographicCoordinateSystemList.ByEPSG( Geogcs.EPSG ) ;

      if assigned( gcs ) then begin
        Geogcs.EPSG := gcs.EPSG ;
        Geogcs.WKT  := gcs.WKT ;
        exit ;
      end;
    end;

    // recognize units
    if Geogcs.Units.EPSG > 0 then
      unt := CSUnitsList.ByEPSG( Geogcs.Units.EPSG )
    else
      unt := nil ;

    if not assigned( unt ) then begin
      if ( IsStringEmpty( Geogcs.Units.WKT ) ) or
         ( Geogcs.Units.Factor <= 0  )
      then begin
        // default is decimal degree
        unt := CSUnitsList.ByEPSG( 9102 ) ;
      end
      else begin
        unt := CSUnitsList.Prepare(
                 Geogcs.Units.EPSG,
                 Geogcs.Units.WKT,
                 '',
                 TGIS_CSUnitsType.Angular,
                 Geogcs.Units.Factor
               ) ;
      end ;
    end ;
    Geogcs.Units.EPSG   := unt.EPSG   ;
    Geogcs.Units.WKT    := unt.WKT    ;
    Geogcs.Units.Factor := unt.Factor ;

    // recognize spheroid
    sph := CSEllipsoidList.ByWKT( Geogcs.Spheroid.WKT ) ;
    if assigned( sph ) then begin
      if ( Abs( sph.SemiMajor         - Geogcs.Spheroid.SemiMajor         ) > 1E-3 )
         or
         ( Abs( sph.InverseFlattering - Geogcs.Spheroid.InverseFlattering ) > 1E-5 )
      then
        sph := nil ;
    end;


    if not assigned( sph ) then begin
      if Geogcs.Spheroid.EPSG > 0 then
        sph := CSEllipsoidList.ByEPSG( Geogcs.Spheroid.EPSG )
      else
        sph := nil ;
    end;

    if not assigned( sph ) then begin
      if ( IsStringEmpty( Geogcs.Spheroid.WKT )   ) or
         ( Geogcs.Spheroid.SemiMajor         <= 0 ) or
         ( Geogcs.Spheroid.InverseFlattering <  0 )
      then
        exit ;

      sph := CSEllipsoidList.Prepare(
               Geogcs.Spheroid.EPSG,
               Geogcs.Spheroid.WKT,
               Geogcs.Spheroid.SemiMajor,
               Geogcs.Spheroid.InverseFlattering
             ) ;
    end ;
    Geogcs.Spheroid.EPSG := sph.EPSG ;
    Geogcs.Spheroid.WKT  := sph.WKT  ;

    // recognize PrimeMeridian
    if Geogcs.Primem.EPSG > 0 then
      pri := CSPrimeMeridianList.ByEPSG( Geogcs.Primem.EPSG )
    else
      pri := nil ;

    if not assigned( pri ) then begin
      if IsStringEmpty( Geogcs.Primem.WKT )
      then
        exit ;

      Geogcs.Primem.Longitude := DegToRad( Geogcs.Primem.Longitude ) ;

      pri := CSPrimeMeridianList.Prepare(
               Geogcs.Primem.EPSG,
               Geogcs.Primem.WKT,
               Geogcs.Primem.Longitude
             ) ;
    end ;

    Geogcs.Primem.EPSG := pri.EPSG ;
    Geogcs.Primem.WKT  := pri.WKT  ;

    // recognize datum
    if Geogcs.Datum.EPSG > 0 then
      dat := CSDatumList.ByEPSG( Geogcs.Datum.EPSG )
    else
      dat := nil ;

    if not assigned( dat ) then begin
      if IsStringEmpty( Geogcs.Datum.WKT ) or
         ( Geogcs.Spheroid.EPSG  <= 0    )
      then
        exit ;

      if Geogcs.Datum.TOWGS84.Any then
        dat := CSDatumList.PrepareEx(
                 Geogcs.Datum.EPSG,
                 Geogcs.Datum.WKT,
                 Geogcs.Spheroid.EPSG,
                 9606,
                 -1,
                 Geogcs.Datum.TOWGS84.A,
                 Geogcs.Datum.TOWGS84.B,
                 Geogcs.Datum.TOWGS84.C,
                 Geogcs.Datum.TOWGS84.D,
                 Geogcs.Datum.TOWGS84.E,
                 Geogcs.Datum.TOWGS84.F,
                 Geogcs.Datum.TOWGS84.G,
                 0,
                 0,
                 0
               )
      else
        dat := CSDatumList.Prepare(
                 Geogcs.Datum.EPSG,
                 Geogcs.Datum.WKT,
                 Geogcs.Spheroid.EPSG,
                 -1
               ) ;
    end ;

    Geogcs.Datum.EPSG := dat.EPSG ;
    Geogcs.Datum.WKT  := dat.WKT  ;

    Geogcs.Spheroid.EPSG := dat.Ellipsoid.EPSG ;
    Geogcs.Spheroid.WKT  := dat.Ellipsoid.WKT  ;

    gcs := CSGeographicCoordinateSystemList.Prepare(
             Geogcs.EPSG, Geogcs.WKT,
             Geogcs.Datum.EPSG,
             Geogcs.Primem.EPSG,
             Geogcs.Units.EPSG
           ) ;

    Geogcs.EPSG := gcs.EPSG ;
    Geogcs.WKT  := gcs.WKT ;
  end ;

  procedure T_Parser.parseGEOGCS_AUTHORITY ;
  var
    t : T_Token ;
  begin
    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                if VarEqual( t.Value, 'EPSG' ) then begin
                  t := getToken ;
                  if ( t.SubType = T_TokenType.ValString ) or
                     ( t.SubType = T_TokenType.ValFloat  )
                  then
                    Geogcs.EPSG := VarToInt32( t.Value ) ;
                end;
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end;

    until t.SubType = T_TokenType.Eof ;
  end ;

  procedure T_Parser.parseGEOGCS_DATUM ;
  var
    t : T_Token ;
  begin
    Geogcs.Datum.TOWGS84.Any := False ;

    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                Geogcs.Datum.WKT := VarToString( t.Value ) ;
              end
      else if t.SubType = T_TokenType.Section then begin
                if      VarEqual( t.Value, 'AUTHORITY' ) then
                  parseGEOGCS_DATUM_AUTHORITY
                else if VarEqual( t.Value, 'SPHEROID'  ) then
                  parseGEOGCS_SPHEROID
                else if VarEqual( t.Value, 'TOWGS84'   ) then
                  parseGEOGCS_TOWGS84
                else
                  parseUNKNOWN ;
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end;

    until t.SubType = T_TokenType.Eof ;

  end ;

  procedure T_Parser.parseGEOGCS_DATUM_AUTHORITY ;
  var
    t : T_Token ;
  begin
    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                if VarEqual( t.Value, 'EPSG' ) then begin
                  t := getToken ;
                  if ( t.SubType = T_TokenType.ValString ) or
                     ( t.SubType = T_TokenType.ValFloat  )
                  then
                    Geogcs.Datum.EPSG := VarToInt32( t.Value ) ;
                end;
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end;

    until t.SubType = T_TokenType.Eof ;
  end ;

  procedure T_Parser.parseGEOGCS_SPHEROID ;
  var
    t : T_Token ;
  begin
    Geogcs.Spheroid.SemiMajor         := 0 ;
    Geogcs.Spheroid.InverseFlattering := 0 ;

    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                Geogcs.Spheroid.WKT := VarToString( t.Value ) ;
                t := getToken ;
                if t.SubType = T_TokenType.ValFloat then
                  Geogcs.Spheroid.SemiMajor := VarToDouble( t.Value )
                else
                  Abort ;

                t := getToken ;
                if t.SubType = T_TokenType.ValFloat then
                  Geogcs.Spheroid.InverseFlattering := VarToDouble( t.Value )
                else
                  Abort ;
              end
      else if t.SubType = T_TokenType.Section then begin
                if VarEqual( t.Value, 'AUTHORITY' ) then
                  parseGEOGCS_SPHEROID_AUTHORITY
                else
                  parseUNKNOWN
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end;

    until t.SubType = T_TokenType.Eof ;

  end ;

  procedure T_Parser.parseGEOGCS_TOWGS84 ;
  var
    t : T_Token ;
    i : Integer ;
    d : Double  ;
  begin
    Geogcs.Datum.TOWGS84.Any := False ;
    Geogcs.Datum.TOWGS84.A   := 0 ;
    Geogcs.Datum.TOWGS84.B   := 0 ;
    Geogcs.Datum.TOWGS84.C   := 0 ;
    Geogcs.Datum.TOWGS84.D   := 0 ;
    Geogcs.Datum.TOWGS84.E   := 0 ;
    Geogcs.Datum.TOWGS84.F   := 0 ;
    Geogcs.Datum.TOWGS84.G   := 0 ;
    i := 0 ;

    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValFloat then begin
                d := VarToDouble( t.Value ) ;
                case i of
                  0  : Geogcs.Datum.TOWGS84.A := d ;
                  1  : Geogcs.Datum.TOWGS84.B := d ;
                  2  : Geogcs.Datum.TOWGS84.C := d ;
                  3  : Geogcs.Datum.TOWGS84.D := DegToRad( d / 3600 ) ;
                  4  : Geogcs.Datum.TOWGS84.E := DegToRad( d / 3600 ) ;
                  5  : Geogcs.Datum.TOWGS84.F := DegToRad( d / 3600 ) ;
                  6  : Geogcs.Datum.TOWGS84.G := d / 1000000;
                  else begin
                         assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) );
                       end ;
                end;
                inc( i ) ;
              end
      else if t.SubType = T_TokenType.Unknown then begin
                Abort ;
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end;

    until t.SubType = T_TokenType.Eof ;

    Geogcs.Datum.TOWGS84.Any := i > 0 ;
  end ;

  procedure T_Parser.parseGEOGCS_SPHEROID_AUTHORITY ;
  var
    t : T_Token ;
  begin
    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                if VarEqual( t.Value, 'EPSG' ) then begin
                  t := getToken ;
                  if ( t.SubType = T_TokenType.ValString ) or
                     ( t.SubType = T_TokenType.ValFloat  )
                  then
                    Geogcs.Spheroid.EPSG := VarToInt32( t.Value ) ;
                end;
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end;

    until t.SubType = T_TokenType.Eof ;

  end ;

  procedure T_Parser.parseGEOGCS_PRIMEM ;
  var
    t : T_Token ;
  begin
    Geogcs.Primem.Longitude  := 0 ;

    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                Geogcs.Primem.WKT := VarToString( t.Value ) ;
                t := getToken ;
                if t.SubType = T_TokenType.ValFloat then
                  Geogcs.Primem.Longitude := VarToDouble( t.Value )
                else
                  Abort ;
              end
      else if t.SubType = T_TokenType.Section then begin
                if VarEqual( t.Value, 'AUTHORITY' ) then
                  parseGEOGCS_PRIMEM_AUTHORITY
                else
                  parseUNKNOWN
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end ;

    until t.SubType = T_TokenType.Eof ;

  end ;

  procedure T_Parser.parseGEOGCS_PRIMEM_AUTHORITY ;
  var
    t : T_Token ;
  begin
    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                if VarEqual( t.Value, 'EPSG' ) then begin
                  t := getToken ;
                  if ( t.SubType = T_TokenType.ValString ) or
                     ( t.SubType = T_TokenType.ValFloat  )
                  then
                    Geogcs.Primem.EPSG := VarToInt32( t.Value ) ;
                end;
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end;

    until t.SubType = T_TokenType.Eof ;
  end ;

  procedure T_Parser.parseGEOGCS_UNIT ;
  var
    t : T_Token ;
  begin
    Geogcs.Units.Factor := 0 ;

    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                Geogcs.Units.WKT := VarToString( t.Value ) ;
                t := getToken ;
                if t.SubType = T_TokenType.ValFloat then
                  Geogcs.Units.Factor := VarToDouble( t.Value )
                else
                  Abort ;
              end
      else if t.SubType = T_TokenType.Section then begin
                if VarEqual( t.Value, 'AUTHORITY' ) then
                  parseGEOGCS_UNIT_AUTHORITY
                else
                  parseUNKNOWN
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end ;

    until t.SubType = T_TokenType.Eof ;

  end ;

  procedure T_Parser.parseGEOGCS_UNIT_AUTHORITY ;
  var
    t : T_Token ;
  begin
    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                if VarEqual( t.Value, 'EPSG' ) then begin
                  t := getToken ;
                  if ( t.SubType = T_TokenType.ValString ) or
                     ( t.SubType = T_TokenType.ValFloat  )
                  then
                    Geogcs.Units.EPSG := VarToInt32( t.Value ) ;
                end;
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end;

    until t.SubType = T_TokenType.Eof ;
  end ;

  procedure T_Parser.parseCOMPD_CS ;
  var
    t           : T_Token ;
    cmpdcs_name : String  ;
  begin

    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                cmpdcs_name := VarToString( t.Value ) ;
              end
      else if t.SubType = T_TokenType.Section then begin
                if      VarEqual( t.Value, 'PROJCS'     ) then
                  parsePROJCS
                else if VarEqual( t.Value, 'GEOGCS'     ) then
                  parseGEOGCS
                else
                  parseUNKNOWN ;
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end ;

    until t.SubType = T_TokenType.Eof ;
  end ;

  procedure T_Parser.parsePROJCS ;
  var
    t   : T_Token             ;
    unt : TGIS_CSUnits        ;
    prj : TGIS_CSProjAbstract ;
    gcs : TGIS_CSGeographicCoordinateSystem  ;
    pcs : TGIS_CSProjectedCoordinateSystem   ;
    tmp : Integer             ;
  begin

    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                Projcs.WKT := VarToString( t.Value ) ;
              end
      else if t.SubType = T_TokenType.Section then begin
                if      VarEqual( t.Value, 'AUTHORITY'  ) then
                  parsePROJCS_AUTHORITY
                else if VarEqual( t.Value, 'GEOGCS'     ) then
                  parseGEOGCS
                else if VarEqual( t.Value, 'PROJECTION' ) then
                  parsePROJCS_PROJECTION
                else if VarEqual( t.Value, 'PARAMETER'  ) then
                  parsePROJCS_PARAMETER
                else if VarEqual( t.Value, 'UNIT'       ) then
                  parsePROJCS_UNIT
                else
                  parseUNKNOWN ;
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end ;

    until t.SubType = T_TokenType.Eof ;

    if Projcs.EPSG > 0 then begin
      pcs := CSProjectedCoordinateSystemList.ByEPSG( Projcs.EPSG ) ;

      if assigned( pcs ) then begin
        Projcs.EPSG := pcs.EPSG ;
        Projcs.WKT  := pcs.WKT ;
        exit ;
      end;
    end;

    // recognize units
    if Projcs.Units.EPSG > 0 then
      unt := CSUnitsList.ByEPSG( Projcs.Units.EPSG )
    else
      unt := nil ;

    if not assigned( unt ) then begin
      if ( IsStringEmpty( Projcs.Units.WKT ) ) or
         ( Projcs.Units.Factor <= 0 )
      then begin
        // default is meter
        unt := CSUnitsList.ByEPSG( 9001 ) ;
      end
      else begin
        unt := CSUnitsList.Prepare(
                 Projcs.Units.EPSG,
                 Projcs.Units.WKT,
                 '',
                 TGIS_CSUnitsType.Linear,
                 Projcs.Units.Factor
               ) ;
      end ;
    end ;
    Projcs.Units.EPSG   := unt.EPSG   ;
    Projcs.Units.WKT    := unt.WKT    ;
    Projcs.Units.Factor := unt.Factor ;

    // recognize projection
    tmp := Projcs.Projection.EPSG ;
    prj := CSProjList.ByWKT( Projcs.Projection.WKT ) ;
    if assigned( prj ) then begin
      // check parameters
      Projcs.Projection.EPSG := prj.EPSG ;
      Projcs.Projection.WKT  := prj.WKT ;
    end
    else begin
      Projcs.Projection.EPSG := 0 ;
    end ;

    // projection enforced by EPSG
    if ( tmp > 0 ) and ( tmp <> prj.EPSG ) then begin
       if ( tmp = CSPROJ_Hotine_Oblique_Mercator_Variant_B )
          and
          ( prj.EPSG = CSPROJ_Hotine_Oblique_Mercator_Azimuth_Center )
       then
         // exception for old do HD72_EOV definition
         // do nothing
       else begin
         prj := CSProjList.ByEPSG( tmp ) ;
         if assigned( prj ) then begin
           Projcs.Projection.EPSG := prj.EPSG ;
           Projcs.Projection.WKT  := prj.WKT ;
         end;
       end;
    end ;

    if not assigned( prj ) then exit ;

    if Geogcs.EPSG = 0 then begin
      gcs := CSGeographicCoordinateSystemList.ByEPSG( GIS_EPSG_WGS84 ) ;
      Geogcs.EPSG                := gcs.EPSG                              ;
      Geogcs.WKT                 := gcs.WKT                               ;
      Geogcs.Units.EPSG          := gcs.Units.EPSG                        ;
      Geogcs.Units.WKT           := gcs.Units.WKT                         ;
      Geogcs.Units.Factor        := gcs.Units.Factor                      ;
      Geogcs.Primem.EPSG         := gcs.PrimeMeridian.EPSG                ;
      Geogcs.Primem.WKT          := gcs.PrimeMeridian.WKT                 ;
      Geogcs.Primem.Longitude    := gcs.PrimeMeridian.Longitude           ;
      Geogcs.Spheroid.EPSG       := gcs.Datum.Ellipsoid.EPSG              ;
      Geogcs.Spheroid.WKT        := gcs.Datum.Ellipsoid.WKT               ;
      Geogcs.Spheroid.SemiMajor  := gcs.Datum.Ellipsoid.SemiMajor         ;
      Geogcs.Spheroid.InverseFlattering
                                 := gcs.Datum.Ellipsoid.InverseFlattering ;
      Geogcs.Datum.EPSG          := gcs.Datum.EPSG                        ;
      Geogcs.Datum.WKT           := gcs.Datum.WKT                         ;
    end;

    // set projection units & defaults if not set
    if not IsNan( Projcs.Parameters.CentralMeridian ) then
      Projcs.Parameters.CentralMeridian
        := Projcs.Parameters.CentralMeridian          * Geogcs.Units.Factor
    else
      Projcs.Parameters.CentralMeridian
        := prj.Parameters.CentralMeridian ;

    if not IsNan( Projcs.Parameters.LatitudeOfOrigin ) then
      Projcs.Parameters.LatitudeOfOrigin
        := Projcs.Parameters.LatitudeOfOrigin         * Geogcs.Units.Factor
    else
      Projcs.Parameters.LatitudeOfOrigin
        := prj.Parameters.LatitudeOfOrigin  ;

    if not IsNan( Projcs.Parameters.FalseEasting ) then
      Projcs.Parameters.FalseEasting
        := Projcs.Parameters.FalseEasting             * Projcs.Units.Factor
    else
      Projcs.Parameters.FalseEasting
        := prj.Parameters.FalseEasting ;

    if not IsNan( Projcs.Parameters.FalseNorthing ) then
      Projcs.Parameters.FalseNorthing
        := Projcs.Parameters.FalseNorthing            * Projcs.Units.Factor
    else
      Projcs.Parameters.FalseNorthing
        := prj.Parameters.FalseNorthing ;

    if not IsNan( Projcs.Parameters.StandardParallel_1 ) then
      Projcs.Parameters.StandardParallel_1
        := Projcs.Parameters.StandardParallel_1       * Geogcs.Units.Factor
    else
      Projcs.Parameters.StandardParallel_1
        := prj.Parameters.StandardParallel_1 ;

    if not IsNan( Projcs.Parameters.StandardParallel_2 ) then
      Projcs.Parameters.StandardParallel_2
        := Projcs.Parameters.StandardParallel_2       * Geogcs.Units.Factor
    else
      Projcs.Parameters.StandardParallel_2
        := prj.Parameters.StandardParallel_2 ;

    if not IsNan( Projcs.Parameters.PseudoStandardParallel_1 ) then
      Projcs.Parameters.PseudoStandardParallel_1
        := Projcs.Parameters.PseudoStandardParallel_1 * Geogcs.Units.Factor
    else
      Projcs.Parameters.PseudoStandardParallel_1
        := prj.Parameters.PseudoStandardParallel_1 ;

    if Projcs.Parameters.Zone <> 0 then begin
      //no effect
      //Projcs.Parameters.Zone
      //  := Projcs.Parameters.Zone
    end
    else
      Projcs.Parameters.Zone
        := prj.Parameters.Zone ;

    if not IsNan( Projcs.Parameters.ScaleFactor ) then begin
      //no effect
      //Projcs.Parameters.ScaleFactor
      //  := Projcs.Parameters.ScaleFactor
    end
    else
      Projcs.Parameters.ScaleFactor
        := prj.Parameters.ScaleFactor ;

    if not IsNan( Projcs.Parameters.LongitudeOfCenter ) then
      Projcs.Parameters.LongitudeOfCenter
        := Projcs.Parameters.LongitudeOfCenter        * Geogcs.Units.Factor
    else
      Projcs.Parameters.LongitudeOfCenter
        := prj.Parameters.LongitudeOfCenter ;

    if not IsNan( Projcs.Parameters.LatitudeOfCenter ) then
      Projcs.Parameters.LatitudeOfCenter
        := Projcs.Parameters.LatitudeOfCenter         * Geogcs.Units.Factor
    else
      Projcs.Parameters.LatitudeOfCenter
        := prj.Parameters.LatitudeOfCenter ;

    if not IsNan( Projcs.Parameters.Azimuth ) then
      Projcs.Parameters.Azimuth
        := Projcs.Parameters.Azimuth                  * Geogcs.Units.Factor
    else
      Projcs.Parameters.Azimuth
        := prj.Parameters.Azimuth ;

    if not IsNan( Projcs.Parameters.LongitudeOfPoint_1 ) then
      Projcs.Parameters.LongitudeOfPoint_1
        := Projcs.Parameters.LongitudeOfPoint_1       * Geogcs.Units.Factor
    else
      Projcs.Parameters.LongitudeOfPoint_1
        := prj.Parameters.LongitudeOfPoint_1 ;

    if not IsNan( Projcs.Parameters.LatitudeOfPoint_1 ) then
      Projcs.Parameters.LatitudeOfPoint_1
        := Projcs.Parameters.LatitudeOfPoint_1        * Geogcs.Units.Factor
    else
      Projcs.Parameters.LatitudeOfPoint_1
        := prj.Parameters.LatitudeOfPoint_1 ;

    if not IsNan( Projcs.Parameters.LongitudeOfPoint_2 ) then
      Projcs.Parameters.LongitudeOfPoint_2
        := Projcs.Parameters.LongitudeOfPoint_2       * Geogcs.Units.Factor
    else
      Projcs.Parameters.LongitudeOfPoint_2
        := prj.Parameters.LongitudeOfPoint_2 ;

    if not IsNan( Projcs.Parameters.LatitudeOfPoint_2 ) then
      Projcs.Parameters.LatitudeOfPoint_2
        := Projcs.Parameters.LatitudeOfPoint_2        * Geogcs.Units.Factor
    else
      Projcs.Parameters.LatitudeOfPoint_2
        := prj.Parameters.LatitudeOfPoint_2 ;

    if not IsNan( Projcs.Parameters.XScale ) then begin
      // no effect
      //Projcs.Parameters.XScale
      //  := Projcs.Parameters.XScale
    end
    else
      Projcs.Parameters.XScale
        := prj.Parameters.XScale ;

    if not IsNan( Projcs.Parameters.YScale ) then begin
      // no effect
      //Projcs.Parameters.YScale
      //  := Projcs.Parameters.YScale
    end
    else
      Projcs.Parameters.YScale
        := prj.Parameters.YScale ;

    if not IsNan( Projcs.Parameters.XYPlaneRotation ) then
      Projcs.Parameters.XYPlaneRotation
        := Projcs.Parameters.XYPlaneRotation          * Geogcs.Units.Factor
    else begin
      if ( Projcs.Projection.EPSG = 9812 ) and
         not IsNan( Projcs.Parameters.Azimuth )
      then begin
        // fix for HotineObliqueMercatoAzimuthNaturalOrigin optional parameter
        Projcs.Parameters.XYPlaneRotation
          := Projcs.Parameters.Azimuth
      end
      else
        Projcs.Parameters.XYPlaneRotation
          := prj.XYPlaneRotation ;
    end ;

    // Double Stereographic fix for GlobalMapper created files
    // to everride CentalMeridian/LatitudeOfOrigin
    // with LongitudeOfCenter/LatitudeOfCenter
    // Same fo Albers
    if ( not IsNan( Projcs.Parameters.LongitudeOfCenter )
       and
       ( Projcs.Parameters.LongitudeOfCenter <> prj.Parameters.LongitudeOfCenter )
       and
       ( not InCSProjParameterSet( TGIS_CSProjParameter.LongitudeOfCenter,
                                   prj.ParametersSet )
                                 )
       )
       and
       InCSProjParameterSet( TGIS_CSProjParameter.CentralMeridian,
                             prj.ParametersSet
                           )
    then begin
      Projcs.Parameters.CentralMeridian := Projcs.Parameters.LongitudeOfCenter ;
      Projcs.Parameters.LongitudeOfCenter := NaN ;
    end;

    if ( not IsNan( Projcs.Parameters.LatitudeOfCenter )
       and
       ( Projcs.Parameters.LatitudeOfCenter <> prj.Parameters.LatitudeOfCenter )
       and
       ( not InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfCenter,
                                   prj.ParametersSet )
                                 )
       )
       and
       InCSProjParameterSet( TGIS_CSProjParameter.LatitudeOfOrigin,
                             prj.ParametersSet
                           )
    then begin
       Projcs.Parameters.LatitudeOfOrigin := Projcs.Parameters.LatitudeOfCenter ;
       Projcs.Parameters.LatitudeOfCenter := NaN ;
    end ;


    if Geogcs.EPSG > 0 then begin
      pcs := CSProjectedCoordinateSystemList.Prepare(
               -1, Projcs.WKT,
               Geogcs.EPSG,
               Projcs.Units.EPSG,
               Projcs.Projection.EPSG,
               Projcs.Parameters
             ) ;
    end
    else begin
      pcs := CSProjectedCoordinateSystemList.Prepare(
               -1, Projcs.WKT,
               GIS_EPSG_WGS84,
               Projcs.Units.EPSG,
               Projcs.Projection.EPSG,
               Projcs.Parameters
             ) ;
    end;

    Projcs.EPSG := pcs.EPSG ;
    Projcs.WKT  := pcs.WKT ;
  end ;

  procedure T_Parser.parsePROJCS_AUTHORITY ;
  var
    t : T_Token ;
  begin
    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                if VarEqual( t.Value, 'EPSG' ) then begin
                  t := getToken ;
                  if ( t.SubType = T_TokenType.ValString ) or
                     ( t.SubType = T_TokenType.ValFloat  )
                  then
                    Projcs.EPSG := VarToInt32( t.Value ) ;
                end;
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end;

    until t.SubType = T_TokenType.Eof ;
  end ;

  procedure T_Parser.parsePROJCS_PROJECTION ;
  var
    t : T_Token ;
  begin

    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                Projcs.Projection.WKT := VarToString( t.Value ) ;
              end
      else if t.SubType = T_TokenType.Section then begin
                if VarEqual( t.Value, 'AUTHORITY' ) then
                  parsePROJCS_PROJECTION_AUTHORITY
                else
                  parseUNKNOWN
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end ;

    until t.SubType = T_TokenType.Eof ;

  end ;

  procedure T_Parser.parsePROJCS_PROJECTION_AUTHORITY ;
  var
    t : T_Token ;
  begin
    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                if VarEqual( t.Value, 'EPSG' ) then begin
                  t := getToken ;
                  if ( t.SubType = T_TokenType.ValString ) or
                     ( t.SubType = T_TokenType.ValFloat  )
                  then
                    Projcs.Projection.EPSG := VarToInt32( t.Value ) ;
                end;
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end;

    until t.SubType = T_TokenType.Eof ;
  end ;

  procedure T_Parser.parsePROJCS_PARAMETER ;
  var
    t     : T_Token ;
    name  : String  ;
    value : Double  ;

    function _t( const _txt : String ) : Boolean ;
    begin
      Result := CompareText( name, _txt ) = 0 ;
    end ;

  begin
    value  := 0 ;

    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                name := VarToString( t.Value ) ;
                t := getToken ;
                if t.SubType = T_TokenType.ValFloat then
                  value := VarToDouble( t.Value )
                else
                  Abort ;
              end
      else if t.SubType = T_TokenType.Section then begin
                if VarEqual( t.Value, 'AUTHORITY' ) then
                  parsePROJCS_PARAMETER_AUTHORITY
                else
                  parseUNKNOWN
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end;

    until t.SubType = T_TokenType.Eof ;

    if      _t( 'Central_Meridian'           ) then
            Projcs.Parameters.CentralMeridian          := value
    else if _t( 'Latitude_Of_Origin'         ) then
            Projcs.Parameters.LatitudeOfOrigin         := value
    else if _t( 'False_Easting'              ) then
            Projcs.Parameters.FalseEasting             := value
    else if _t( 'False_Northing'             ) then
            Projcs.Parameters.FalseNorthing            := value
    else if _t( 'Standard_Parallel_1'        ) then
            Projcs.Parameters.StandardParallel_1       := value
    else if _t( 'Standard_Parallel_2'        ) then
            Projcs.Parameters.StandardParallel_2       := value
    else if _t( 'Pseudo_Standard_Parallel_1' ) then
            Projcs.Parameters.PseudoStandardParallel_1 := value
    else if _t( 'Zone'                       ) then
            Projcs.Parameters.Zone                     := RoundS( value )
    else if _t( 'Scale_Factor'               ) then
            Projcs.Parameters.ScaleFactor              := value
    else if _t( 'Longitude_Of_Center'        ) then
            Projcs.Parameters.LongitudeOfCenter        := value
    else if _t( 'Latitude_Of_Center'         ) then
            Projcs.Parameters.LatitudeOfCenter         := value
    else if _t( 'Azimuth'                    ) then
            Projcs.Parameters.Azimuth                  := value
    else if _t( 'Longitude_Of_Point_1'       ) then
            Projcs.Parameters.LongitudeOfPoint_1       := value
    else if _t( 'Latitude_Of_Point_1'        ) then
            Projcs.Parameters.LatitudeOfPoint_1        := value
    else if _t( 'Longitude_Of_Point_2'       ) then
            Projcs.Parameters.LongitudeOfPoint_2       := value
    else if _t( 'Latitude_Of_Point_2'        ) then
            Projcs.Parameters.LatitudeOfPoint_2        := value
    else if _t( 'X_Scale'                    ) then
            Projcs.Parameters.XScale                   := value
    else if _t( 'Y_Scale'                    ) then
            Projcs.Parameters.YScale                   := value
    else if _t( 'XY_Plane_Rotation'          ) then
            Projcs.Parameters.XYPlaneRotation          := value
    else if _t( 'Rectified_Grid_Angle'       ) then
            Projcs.Parameters.XYPlaneRotation          := value
    else if _t( 'Central_Parallel'           ) then
            Projcs.Parameters.XYPlaneRotation          := value
    else if _t( 'Longitude_Of_1st_Point'     ) then
            Projcs.Parameters.LongitudeOfPoint_1       := value
    else if _t( 'Latitude_Of_1st_Point'      ) then
            Projcs.Parameters.LatitudeOfPoint_1        := value
    else if _t( 'Longitude_Of_2nd_Point'     ) then
            Projcs.Parameters.LongitudeOfPoint_2       := value
    else if _t( 'Latitude_Of_2nd_Point'      ) then
            Projcs.Parameters.LatitudeOfPoint_2        := value
    else if _t( 'Auxiliary_Sphere_Type'      ) then
            begin
              // ignore
            end
    else begin
            assert( False, name + ' is unknown parameter' ) ;
         end ;

  end ;

  procedure T_Parser.parsePROJCS_PARAMETER_AUTHORITY ;
  var
    t : T_Token ;
  begin
    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                if VarEqual( t.Value, 'EPSG' ) then begin
                  t := getToken ;
                  if ( t.SubType = T_TokenType.ValString ) or
                     ( t.SubType = T_TokenType.ValFloat  )
                  then begin
                    // not supported - do nothing
                  end;
                end;
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end;

    until t.SubType = T_TokenType.Eof ;
  end ;

  procedure T_Parser.parsePROJCS_UNIT ;
  var
    t : T_Token ;
  begin
    Projcs.Units.Factor  := 0 ;

    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                Projcs.Units.WKT := VarToString( t.Value ) ;
                t := getToken ;
                if t.SubType = T_TokenType.ValFloat then
                  Projcs.Units.Factor := VarToDouble( t.Value )
                else
                  Abort ;
              end
      else if t.SubType = T_TokenType.Section then begin
                if VarEqual( t.Value, 'AUTHORITY' ) then
                  parsePROJCS_UNIT_AUTHORITY
                else
                  parseUNKNOWN
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end ;

    until t.SubType = T_TokenType.Eof ;

  end ;

  procedure T_Parser.parsePROJCS_UNIT_AUTHORITY ;
  var
    t : T_Token ;
  begin
    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
                if VarEqual( t.Value, 'EPSG' ) then begin
                  t := getToken ;
                  if ( t.SubType = T_TokenType.ValString ) or
                     ( t.SubType = T_TokenType.ValFloat  )
                  then
                    Projcs.Units.EPSG := VarToInt32( t.Value ) ;
                end;
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end;

    until t.SubType = T_TokenType.Eof ;
  end ;

  procedure T_Parser.parseUNKNOWN ;
  var
    t : T_Token ;
  begin

    repeat
      t := getToken ;

      if      t.SubType = T_TokenType.ValString then begin
              end
      else if t.SubType = T_TokenType.Section then begin
                parseUNKNOWN ;
              end
      else if t.SubType = T_TokenType.EndOfSection then begin
                break ;
              end ;

    until t.SubType = T_TokenType.Eof ;

  end ;

  procedure T_Parser.Parse(
    const _wkt  : String
  ) ;
  var
    t : T_Token ;
  begin
    srcWKT := _wkt ;

    // clear data
    Geogcs.WKT                                 := '' ;
    Geogcs.EPSG                                := 0  ;
    Geogcs.Datum.WKT                           := '' ;
    Geogcs.Datum.EPSG                          := 0  ;
    Geogcs.Primem.WKT                          := '' ;
    Geogcs.Primem.EPSG                         := 0  ;
    Geogcs.Primem.Longitude                    := 0  ;
    Geogcs.Spheroid.WKT                        := '' ;
    Geogcs.Spheroid.EPSG                       := 0  ;
    Geogcs.Spheroid.SemiMajor                  := 0  ;
    Geogcs.Spheroid.InverseFlattering          := 0  ;
    Geogcs.Units.WKT                           := '' ;
    Geogcs.Units.EPSG                          := 0  ;
    Geogcs.Units.Factor                        := 0  ;

    Projcs.WKT                                 := '' ;
    Projcs.EPSG                                := 0  ;
    Projcs.Projection.WKT                      := '' ;
    Projcs.Projection.EPSG                     := 0  ;
    {$IFDEF GIS_NORECORDS}
      Projcs.Parameters := new TGIS_CSProjParameters ;
    {$ENDIF}
    Projcs.Parameters.CentralMeridian          := NaN  ;
    Projcs.Parameters.LatitudeOfOrigin         := NaN  ;
    Projcs.Parameters.FalseEasting             := NaN  ;
    Projcs.Parameters.FalseNorthing            := NaN  ;
    Projcs.Parameters.StandardParallel_1       := NaN  ;
    Projcs.Parameters.StandardParallel_2       := NaN  ;
    Projcs.Parameters.PseudoStandardParallel_1 := NaN  ;
    Projcs.Parameters.Zone                     := 0    ;
    Projcs.Parameters.ScaleFactor              := NaN  ;
    Projcs.Parameters.LongitudeOfCenter        := NaN  ;
    Projcs.Parameters.LatitudeOfCenter         := NaN  ;
    Projcs.Parameters.Azimuth                  := NaN  ;
    Projcs.Parameters.LongitudeOfPoint_1       := NaN  ;
    Projcs.Parameters.LatitudeOfPoint_1        := NaN  ;
    Projcs.Parameters.LongitudeOfPoint_2       := NaN  ;
    Projcs.Parameters.LatitudeOfPoint_2        := NaN  ;
    Projcs.Parameters.XScale                   := NaN  ;
    Projcs.Parameters.YScale                   := NaN  ;
    Projcs.Parameters.XYPlaneRotation          := NaN  ;
    Projcs.Units.WKT                           := '' ;
    Projcs.Units.EPSG                          := 0  ;
    Projcs.Units.Factor                        := 0  ;

    // parse
    iPos := StringFirst  ;
    repeat
      t := getToken ;
      if t.SubType <> T_TokenType.Section then exit ;

      if      VarEqual( t.Value, 'PROJCS'   ) then parsePROJCS
      else if VarEqual( t.Value, 'GEOGCS'   ) then parseGEOGCS
      else if VarEqual( t.Value, 'COMPD_CS' ) then parseCOMPD_CS
      else                                         parseUNKNOWN ;
    until t.SubType = T_TokenType.Eof ;
  end ;

//==============================================================================
//  helper functions
//==============================================================================

  // Retrieve or construct a Coordinate System based on provided WMS AUTO string.
  // _auto     AUTO string
  // _cs       proper Coordinate System object or
  //           TGIS_CSUnknownCoordinateSystem if provided _wkt does not
  //           refer to any existing object or cannot be constructed
  // _autority authority string like AUTO:xx,yy,zz or empty if request can
  //           not be processed

  procedure by_auto(
    const _auto      : String                  ;
    var   _cs        : TGIS_CSCoordinateSystem ;
    var   _authority : String
  ) ;
  var
    tkn   : TGIS_Tokenizer ;
    str   : String ;
    proj  : Integer ;
    units : Integer ;
    rlong : Double ;
    rlat  : Double ;
    zone  : Integer ;

    proj_param : TGIS_CSProjParameters ;
    gcs_map    : TGIS_CSAbstract ;
    unit_map   : TGIS_CSAbstract ;
    proj_map   : TGIS_CSAbstract ;
  begin
    _cs    := nil ;
    _authority := '' ;
    rlong  := 0 ;
    rlat   := 0 ;
    proj   := 0 ;
    units  := 0 ;

    if Pos( 'AUTO:', _auto ) = StringFirst then
      str := Copy( _auto, StringFirst+5, length( _auto ) - 4 )
    else
      str := _auto ;

    tkn := TGIS_Tokenizer.Create ;
    try
      tkn.ExecuteEx( str, ',' ) ;

      // expected: AUTO:proj_id,units_id,ref_long,ref_lat
      //       or: AUTO:proj_id,ref_long,ref_lat
      if tkn.Result.Count = 4 then begin
        proj  := StrToInt( tkn.Result[0] ) ;
        units := StrToInt( tkn.Result[1] ) ;
        rlong := DotStrToFloat( tkn.Result[2] ) ;
        rlat  := DotStrToFloat( tkn.Result[3] ) ;
      end
      else if ( tkn.Result.Count = 3 ) and
              ( StrToInt(tkn.Result[0]) = 42005 ) then begin
        proj  := StrToInt( tkn.Result[0] ) ;
        units := StrToInt( tkn.Result[1] ) ;
        rlong := DotStrToFloat( tkn.Result[2] ) ;
        rlat  := 0 ;
      end
      else if tkn.Result.Count = 3 then begin
        proj  := StrToInt( tkn.Result[0] ) ;
        units := 9001 ;
        rlong := DotStrToFloat( tkn.Result[1] ) ;
        rlat  := DotStrToFloat( tkn.Result[2] ) ;
      end
      else if ( tkn.Result.Count = 2 ) and
              ( StrToInt(tkn.Result[0]) = 42005 ) then begin
        proj  := StrToInt( tkn.Result[0] ) ;
        units := 9001 ;
        rlong := DotStrToFloat( tkn.Result[1] ) ;
        rlat  := 0 ;
      end ;

      case proj of
        42001 : begin // Auto UTM
                  proj_map   := CSProjList.ByEPSG(
                                  CSPROJ_Transverse_Mercator
                                 ) ;
                  proj_param := CSProjectedCoordinateSystemList.DefaultParams(
                                  proj_map.EPSG
                                 ) ;
                  zone := FloorS( (rlong + 180.0) / 6.0 ) + 1 ;
                  proj_param.CentralMeridian     := DegToRad( zone * 6 - 183.0 ) ;
                  proj_param.LatitudeOfOrigin    := DegToRad( 0 ) ;
                  proj_param.ScaleFactor         := 0.9996 ;
                  proj_param.FalseEasting        := 500000.0 ;
                  if rlat >= 0 then
                    proj_param.FalseNorthing     := 0.0
                  else
                    proj_param.FalseNorthing     := 10000000.0 ;
                end ;
        42002 : begin // Auto TM
                  proj_map   := CSProjList.ByEPSG(
                                  CSPROJ_Transverse_Mercator
                                 ) ;
                  proj_param := CSProjectedCoordinateSystemList.DefaultParams(
                                  proj_map.EPSG
                                 ) ;
                  proj_param.CentralMeridian     := DegToRad( rlong ) ;
                  proj_param.LatitudeOfOrigin    := DegToRad( 0 ) ;
                  proj_param.ScaleFactor         := 0.9996 ;
                  proj_param.FalseEasting        := 500000.0 ;
                  if rlat >= 0 then
                    proj_param.FalseNorthing     := 0.0
                  else
                    proj_param.FalseNorthing     := 10000000.0 ;
                end ;
        42003 : begin // Auto Orthographic.
                  proj_map   := CSProjList.ByEPSG(
                                  CSPROJ_Orthographic
                                 ) ;
                  proj_param := CSProjectedCoordinateSystemList.DefaultParams(
                                  proj_map.EPSG
                                 ) ;
                  proj_param.LatitudeOfOrigin    := DegToRad( rlat ) ;
                  proj_param.CentralMeridian     := DegToRad( rlong ) ;
                  proj_param.FalseEasting        := 0.0 ;
                  proj_param.FalseNorthing       := 0.0 ;
                end ;
        42004 : begin // Auto Equirectangular
                  proj_map   := CSProjList.ByEPSG(
                                  CSPROJ_Plate_Carree
                                 ) ;
                  proj_param := CSProjectedCoordinateSystemList.DefaultParams(
                                  proj_map.EPSG
                                 ) ;
                  proj_param.LatitudeOfOrigin    := DegToRad( rlat  ) ;
                  proj_param.CentralMeridian     := DegToRad( rlong ) ;
                  proj_param.FalseEasting        := 0.0 ;
                  proj_param.FalseNorthing       := 0.0 ;
                end ;
        42005 : begin // Mollweide
                  proj_map   := CSProjList.ByEPSG(
                                  CSPROJ_Mollweide
                                 ) ;
                  proj_param := CSProjectedCoordinateSystemList.DefaultParams(
                                  proj_map.EPSG
                                 ) ;
                  proj_param.CentralMeridian     := DegToRad( rlong ) ;
                  proj_param.FalseEasting        := 0.0 ;
                  proj_param.FalseNorthing       := 0.0 ;
                end ;
      else
        proj_map := CSUnknownCoordinateSystem
      end ;

      unit_map := CSUnitsList.ByEPSG( units ) ;
      gcs_map  := CSGeographicCoordinateSystemList.ByEPSG( GIS_EPSG_WGS84 ) ;

      if assigned( gcs_map ) and assigned( unit_map ) and assigned( proj_map )
      then begin
        _cs := CSProjectedCoordinateSystemList.Prepare(
                   -1,
                   Format( 'AUTO_%s_%s',
                           [ CSProjList.ByEPSG( proj_map.EPSG ).WKT,
                             CSGeographicCoordinateSystemList.ByEPSG(
                               gcs_map.EPSG
                             ).WKT
                           ]
                       ),
                  gcs_map.EPSG,
                  unit_map.EPSG,
                  proj_map.EPSG,
                  proj_param
                ) ;
        if _cs.EPSG > 0 then
          _authority := _auto ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  // Retrieve or construct a Coordinate System based on provided OGC URN.
  // _urn      URN string
  // _cs       proper Coordinate System object or
  //           TGIS_CSUnknownCoordinateSystem if provided _wkt does not
  //           refer to any existing object or cannot be constructed
  // _autority authority string like EPSG:4326 or OGC:CRS84 or empty if
  //           request can not be processed

  procedure by_urn(
    const _urn       : String                  ;
    var   _cs        : TGIS_CSCoordinateSystem ;
    var   _authority : String
  ) ;
  var
    pcur : Integer ;
    i    : Integer ;
    auth : String ;
    wkt  : String ;
    epsg : Integer  ;
    cnt  : Integer  ;
    sb   : TStringBuilder ;
  begin
    _cs := nil ;

    if Pos( 'URN:OGC:DEF:CRS:', _urn ) = StringFirst then
      pcur := StringFirst + 16
    else if Pos( 'URN:X-OGC:DEF:CRS:', _urn ) = StringFirst then
      pcur := StringFirst + 18
    else if Pos( 'URN:OPENGIS:DEF:CRS:', _urn ) = StringFirst then
      pcur := StringFirst + 20
    else
      exit ;

    cnt := length( _urn )-1+StringFirst ;

    // authority:[version]:code
    i := 0 ;
    while ((pcur+i) < cnt ) and ( _urn[pcur+i] <> ':' ) do
      inc( i ) ;

    auth := Copy( _urn, pcur, i ) ;

    if _urn[pcur+i] = ':' then
      inc( i ) ;

    pcur := pcur + i ;
    i := 0 ;
    while ((pcur+i) < cnt ) and ( _urn[pcur+i] <> ':' ) do
      inc( i ) ;
    if _urn[pcur+i] = ':' then begin
      inc( i ) ;
      pcur := pcur + i ;
    end ;

    if Pos( 'EPSG', auth ) = StringFirst then begin
      // test if it's the last :
      i := 0 ;
      while ((pcur+i) < cnt ) and ( _urn[pcur+i] <> ':' ) do
        inc( i ) ;
      if _urn[pcur+i] = ':' then begin
        inc( i ) ;
        pcur := pcur + i ;
      end ;

      epsg   := StrToInt( Copy( _urn, pcur, 8 ) ) ;
      _cs := TGIS_CSFactory.ByEPSG( epsg ) ;
      if ( _cs.EPSG > 0 ) and ( _cs.EPSG < GIS_EPSG_AUTO ) then
        _authority := 'EPSG:' + IntToStr( epsg )
       else if _cs.EPSG >= GIS_EPSG_AUTO then
         _authority := 'TMP:' + IntToStr( epsg ) ;
    end
    else if Pos( 'OGC', auth ) = StringFirst then begin
      wkt := Copy( _urn, pcur, cnt-pcur+1 ) ;
      _cs := TGIS_CSFactory.ByWKT( wkt ) ;
      if _cs.EPSG > 0 then
        _authority := 'OGC:'+wkt ;

      if ( _cs = nil ) or ( _cs = CSUnknownCoordinateSystem ) then begin
        if Pos( 'AUTO', wkt ) = StringFirst then begin
          inc( pcur, 4 ) ;
          // convert from format AUTO42001:99:8888 to format AUTO:42001,99,8888
          sb := TStringBuilder.Create ;
          try
            sb.Append( 'AUTO:' ) ;
            sb.Append( Copy( _urn, pcur, cnt-pcur+1 ) ) ;

            for i := 5 to sb.Length-1 do
              if sb[i] = ':' then
                sb[i] := ',' ;

            by_auto( sb.ToString, _cs, _authority ) ;
          finally
            FreeObject( sb ) ;
          end ;
        end ;
      end ;

    end ;
  end ;

  // Retrieve or construct a Coordinate System based on provided URL.
  // _url      URL string
  // _cs       proper Coordinate System object or
  //           TGIS_CSUnknownCoordinateSystem if provided _wkt does not
  //           refer to any existing object or cannot be constructed
  // _autority authority string like EPSG:4326 or OGC:CRS84 or empty if
  //           request can not be processed

  procedure by_url(
    const _url       : String                  ;
    var   _cs        : TGIS_CSCoordinateSystem ;
    var   _authority : String
  ) ;
  var
    lst     : TStringList ;
    r       : TGIS_HttpResponse ;
  begin
    _cs := nil ;

    r.Stream := nil ;
    try
      r := TGIS_WebUtils.HttpFetch( _url ) ;
      if r.Status = GIS_HTTP_OK then begin
        lst := TStringList.Create ;
        try
          lst.LoadFromStream( r.Stream ) ;
          TGIS_CSFactory.byWKTInternal( lst.Text, _cs, _authority ) ;
        finally
          FreeObject( lst ) ;
        end ;
      end ;
    finally
      FreeObject( r.Stream ) ;
    end ;
  end ;

//==============================================================================
//  TGIS_CSFactory
//==============================================================================

class procedure TGIS_CSFactory.byWKTInternal(
  const _wkt : String ;
  var   _cs : TGIS_CSCoordinateSystem ;
  var   _authority : String
) ;
var
  par  : T_Parser ;
  epsg : Integer  ;
  wkt  : String   ;
  uwkt : String   ;
  idx  : Integer  ;
begin
  _cs := nil ;

  _authority := '' ;
  epsg := 0 ;
  try
    par := T_Parser.Create ;
    try
      wkt := Trim( _wkt ) ;

      if IsStringEmpty( _wkt ) then
        _cs := CSUnknownCoordinateSystem
      else begin
        uwkt := UpperCase( wkt ) ;
        if Pos( 'EPSG:', uwkt ) = StringFirst then begin
          try
            epsg   := StrToInt( Copy( wkt, StringFirst + 5, 8 ) ) ;
            _cs := ByEPSG( epsg ) ;
            if ( _cs.EPSG > 0 ) and ( _cs.EPSG < GIS_EPSG_AUTO ) then
              _authority := 'EPSG:' + IntToStr( epsg )
            else if _cs.EPSG >= GIS_EPSG_AUTO then
              _authority := 'TMP:' + IntToStr( epsg ) ;
          except
            //
          end ;
        end
        else if Pos( 'EPSGA:', uwkt ) = StringFirst then begin
          try
            epsg   := StrToInt( Copy( wkt, StringFirst + 6, 8 ) ) ;
            _cs := ByEPSG( epsg ) ;
            if ( _cs.EPSG > 0 ) and ( _cs.EPSG < GIS_EPSG_AUTO ) then
              _authority := 'EPSG:' + IntToStr( epsg )
            else if _cs.EPSG >= GIS_EPSG_AUTO then
              _authority := 'TMP:' + IntToStr( epsg ) ;
          except
            //
          end ;
        end
        else if Pos( 'AUTO:', uwkt ) = StringFirst then begin
          try
            by_auto( uwkt, _cs, _authority ) ;
          except
            //
          end ;
        end
        else if (Pos( 'URN:OGC:DEF:CRS:'    , uwkt ) = StringFirst) or
                (Pos( 'URN:X-OGC:DEF:CRS:'  , uwkt ) = StringFirst) or
                (Pos( 'URN:OPENGIS:DEF:CRS:', uwkt ) = StringFirst) then begin
          try
            by_urn( uwkt, _cs, _authority ) ;
          except
            //
          end ;
        end
        else if Pos( 'HTTP://', uwkt ) = StringFirst then begin
          idx := Pos('HTTP://WWW.OPENGIS.NET/GML/SRS/EPSG.XML#', uwkt ) ;
          if idx = StringFirst then
            byWKTInternal( 'EPSG:' + Copy( uwkt, StringFirst+40, length(uwkt)-40 ),
                           _cs, _authority
                         )
          else begin
            idx := Pos('HTTP://WWW.OPENGIS.NET/DEF/CRS/EPSG/0/', uwkt ) ;
            if idx = StringFirst then
              byWKTInternal( 'EPSG:' + Copy( uwkt, StringFirst+38, length(uwkt)-38 ),
                             _cs, _authority
                           )
            else begin
              try
                by_url( wkt, _cs, _authority ) ;
              except
                //
              end ;
            end ;
          end;
        end ;

        if not assigned( _cs ) then begin
          if not assigned( _cs ) then
            _cs := CSProjectedCoordinateSystemList.ByWKT( wkt ) ;
          if not assigned( _cs ) then
            _cs := CSGeographicCoordinateSystemList.ByWKT( wkt ) ;
          if not assigned( _cs ) then begin
            par.Parse( wkt ) ;

            if par.Projcs.EPSG > 0 then
              _cs := CSProjectedCoordinateSystemList.ByEPSG( par.Projcs.EPSG )
            else if IsStringEmpty( par.Projcs.WKT ) then
              _cs := CSGeographicCoordinateSystemList.ByEPSG( par.Geogcs.EPSG ) ;
          end;

          if assigned( _cs ) then begin
            if ( _cs.EPSG > 0 ) and ( _cs.EPSG < GIS_EPSG_AUTO ) then
              _authority := 'EPSG:' + IntToStr( epsg )
            else if _cs.EPSG >= GIS_EPSG_AUTO then
              _authority := 'TMP:' + IntToStr( epsg ) ;
          end;
        end;
      end ;
      if not assigned( _cs ) then
        _cs := CSUnknownCoordinateSystem ;
    finally
      FreeObject( par ) ;
    end;
  except
    // some error in parsing
  end;
end ;

class function TGIS_CSFactory.ByWKT(
  const _wkt : String
) : TGIS_CSCoordinateSystem ;
var
  str : String   ;
begin
  byWKTInternal( _wkt, Result, str ) ;
end ;

class function TGIS_CSFactory.Authority(
  const _wkt : String
) : String ;
var
  cs : TGIS_CSCoordinateSystem ;
begin
  byWKTInternal( _wkt, cs, Result ) ;
end ;

class function TGIS_CSFactory.ByEPSG(
  const _epsg : Integer
) : TGIS_CSCoordinateSystem ;
begin
  Result := nil ;

  if _epsg = 0  then
    Result := CSUnknownCoordinateSystem ;
  if not assigned( Result ) then
    Result := CSProjectedCoordinateSystemList.ByEPSG( _epsg ) ;
  if not assigned( Result ) then
    Result := CSGeographicCoordinateSystemList.ByEPSG( _epsg ) ;
  if not assigned( Result ) then
    Result := CSUnknownCoordinateSystem ;
end;

class function TGIS_CSFactory.ByWKTFile(
  const _path : String
) : TGIS_CSCoordinateSystem ;
var
  lst : TStringList ;
begin
  Result := CSUnknownCoordinateSystem ;
  lst := TStringList.Create ;
  try
    if SafeFileExists( _path ) then begin
      lst.LoadFromFile( _path ) ;
      Result := TGIS_CSFactory.ByWKT( lst.Text ) ;
    end;
  finally
    FreeObject( lst ) ;
  end;
end;

{==================================== END =====================================}
end.

