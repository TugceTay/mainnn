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
  Support for Importing MapInfo and ArcView project files.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoImportProject ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoImportProject"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}
{$IFDEF ISLAND}
  namespace TatukGIS ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF CLR}
  uses
    System.Drawing,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,

    Lider.CG.GIS.GeoViewer ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL ;
{$ENDIF}

type

  /// <summary>
  ///   Class for importing MapView &amp; MapInfo projects.
  /// </summary>
  TGIS_ImportProject = {$IFDEF OXYGENE} public {$ENDIF} class
    private
      oViewer             : TGIS_Viewer ;
      FTemporaryDir       : String ;
    private

      /// <summary>
      ///   Returns a next token form the line. Line will be eaten to the next
      ///   token position.
      /// </summary>
      /// <param name="_line">
      ///   line to be tokenized
      /// </param>
      function    getToken          ( var   _line   : String
                                    ) : String ;

      /// <summary>
      ///   Change slash for backslash.
      /// </summary>
      /// <param name="_str">
      ///   String to be converted
      /// </param>
      function    slashToBackslash  ( const _str    : String
                                    ) : String ;

      /// <summary>
      ///   Converts MapInfo color representation into DK String color
      ///   representation (r:g:b)
      /// </summary>
      /// <param name="_value">
      ///   MapInfo color
      /// </param>
      /// <returns>
      ///   color string
      /// </returns>
      function    mapInfoToRGBColor ( const _value  : Integer
                                    ) : String ;

      /// <summary>
      ///   Converts ArcView color representation into DK String color
      ///   representation (r:g:b)
      /// </summary>
      /// <param name="_value">
      ///   MapInfo color
      /// </param>
      /// <returns>
      ///   color string
      /// </returns>
      function    arcViewToRGBColor (       _value  : LongWord
                                    ) : String ;
    protected

      /// <summary>
      ///   Import project from MapInfo 6.x project.
      /// </summary>
      /// <param name="_path">
      ///   project file to be opened
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERMISSED if not all layers can be opened
      /// </exception>
      procedure   importMapInfo_6x  ( const _path   : String
                                    ) ;

      /// <summary>
      ///   Import project from ArcView 3.x project.
      /// </summary>
      /// <param name="_path">
      ///   project file to be opened
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT if file is corrupted
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERMISSED if not all layers can be opened
      /// </exception>
      procedure   importArcView_3x  ( const _path   : String
                                    ) ;

      /// <summary>
      ///   Import project from Arc Explorer 2.x project.
      /// </summary>
      /// <param name="_path">
      ///   project file to be opened
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERMISSED if not all layers can be opened
      /// </exception>
      procedure   importFromArcExplorer_2x
                                    ( const _path   : String
                                    ) ;

      /// <summary>
      ///   Import project from MapInfoX project.
      /// </summary>
      /// <param name="_path">
      ///   project file to be opened
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERMISSED if not all layers can be opened
      /// </exception>
      procedure   importMapInfoX    ( const _path   : String
                                    ) ;

      /// <summary>
      ///   Import project from Qgis project.
      /// </summary>
      /// <param name="_path">
      ///   project file to be opened
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERMISSED if not all layers can be opened
      /// </exception>
      procedure   importQgis        ( const _path   : String
                                    ) ;

      {$IFNDEF ISLAND}
      /// <summary>
      ///   Import project from MapInfo seamless project.
      /// </summary>
      /// <param name="_path">
      ///   project file to be opened
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_LAYERMISSED if not all layers can be opened
      /// </exception>
      procedure   importMapInfoSeamless( const _path   : String
                                        ) ;
      {$ENDIF}
      /// <summary>
      ///   Import project from zip file.
      /// </summary>
      /// <param name="_path">
      ///   zip file to be opened
      /// </param>
      procedure   importZip            ( const _path   : String
                                        ) ;
    public

      /// <summary>
      ///   Construct an instance based on provided viewer object.
      /// </summary>
      /// <param name="_viewer">
      ///   viewer which will be associated with this object
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BAD_CALL if the _viewer is not assigned
      /// </exception>
      constructor Create            ( const _viewer : TGIS_Viewer
                                    ) ; overload;

      /// <summary>
      ///   Import project from ArcView 3.x and MapInfo 6.x projects.
      /// </summary>
      /// <param name="_path">
      ///   project file to be opened
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEREAD not existing file
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEEXTENSION unknown file type
      /// </exception>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT bas project structure
      /// </exception>
      procedure   Import            ( const _path   : String
                                    ) ;

      /// <summary>
      ///   Checks if the provided file is supported by TGIS_ImportProject class.
      /// </summary>
      /// <param name="_path">
      ///   path to the file
      /// </param>
      /// <returns>
      ///  True if supported
      /// </returns>
      function    IsSupported       ( const _path   : String
                                    ) : Boolean ;
    public
      /// <summary>
      ///   Temporary directory used to import a project.
      ///   If not empty, should be deleted upon closing a viewer.
      /// </summary>
      property TemporaryDir         : String  read FTemporaryDir ;
  end ;

//#############################################################################
implementation

{$IFDEF DCC}
  uses
    System.IniFiles,
    {$IFDEF LEVEL_XE3_RTL}
      System.UITypes,
    {$ENDIF}

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoCsMapinfo,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoFileTAB,
    Lider.CG.GIS.GeoLayerTAB,
    Lider.CG.GIS.GeoLayerProject,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoLayerPixel,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoProjectQgis,
    Lider.CG.GIS.GeoRegistredLayers,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoConfig,
    Lider.CG.GIS.GeoCompression;
{$ENDIF}

const
  IW_MAX = 999 ; // maximum number of layers in WOR file

//==============================================================================
// TGIS_ImportProject
//==============================================================================

  constructor TGIS_ImportProject.Create(
    const _viewer : TGIS_Viewer
  ) ;
  begin
    {$IFDEF OXYGENE}
      inherited Create ;
    {$ENDIF}
    if not assigned( _viewer ) then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BAD_CALL ), '', 0 ) ;

    oViewer       := _viewer ;
    FTemporaryDir := '' ;
  end ;

  function TGIS_ImportProject.getToken(
    var _line : String
  ) : String ;
  var
    state  : Integer ;
    state2 : Integer ;
    i      : Integer ;
  begin
    i := StringFirst ;
    Result := '';
    state2 := 0 ;

    if IsStringEmpty( _line ) then exit ;

    while i < StringLast( _line ) do begin
      inc( i );
      case _line[ i ] of
        ' ' : begin
                state := 1 ;
                if state2 = 5 then state := 0 ;
              end ;
        '(' : begin
                state := 2 ;
                if state2 = 5 then state := 0 ;
              end ;
        ')' : begin
                state := 3 ;
                if state2 = 5 then state := 0 ;
              end ;
        #9  : state := 4 ;
        '"' : begin
                state  := 5 ;
                state2 := 5 ;
              end ;
        ',' : state := 6 ;
        #0  : state := 7 ;
        ':' : begin
                state := 8 ;  // special for ArcView project file
                case _line[ i+1 ] of
                  '\',
                  '/': state := 0 ;
                end ;
              end ;
        else  state := 0 ;
      end ;

      if ( state > 0 ) and (     IsStringEmpty( Result ) ) then continue ;
      if ( state > 0 ) and ( not IsStringEmpty( Result ) ) then break    ;

      if state = 0 then Result := Result + _line[ i ] ;
    end ;

    if state2 = 5 then i := i + 1 ;

    _line := Copy( _line , i, 8192 ) ;
  end ;

  function TGIS_ImportProject.slashToBackslash(
    const _str : String
  ) : String ;
  var
    i  : Integer ;
    sb : TStringBuilder ;
  begin
    sb := TStringBuilder.Create( _str ) ;
    try
      for i := 0 to sb.Length - 1 do begin
        if sb[ i ] = '/' then
          sb[ i ] := '\' ;
      end ;
      Result := sb.ToString ;
    finally
      FreeObject( sb ) ;
    end ;
  end ;

  function TGIS_ImportProject.mapInfoToRGBColor(
    const _value : Integer
  ) : String ;
  var
    b, g,r : Integer ;
    k      : Integer ;
  begin
    k := _value    ;
    b := k mod 256 ;

    k := k div 256 ;
    g := k mod 256 ;

    r := k div 256 ;

    Result:= Format( '%d:%d:%d', [ r, g, b ] ) ;
  end ;

  function TGIS_ImportProject.arcViewToRGBColor(
    _value : LongWord
  ) : String ;
  var
    b,p1,g,r : Integer ;
  begin
    b    := _value mod 256 ;
    p1   := _value div 256 ;
    g    := p1     mod 256 ;
    r    := p1     div 256 ;
    Result := IntToStr(b) + ':' + IntToStr(g) + ':' + IntToStr(r) ;
  end ;

  type
    t_IProj_render = record
      Typ               : String                    ;
      Pole              : String                    ;
      FontTyp           : String                    ;
      FontSize          : String                    ;
      FontBold          : String                    ;
      FontItalic        : String                    ;
      FontColor         : String                    ;
      BackColor         : String                    ;
      BackOnOff         : String                    ;
    end ;

    t_IProj_symbol = record
      {$IFNDEF OXYGENE}
        x               : array [1..10] of String   ;
      {$ELSE}
        x               : array of String   ;
      {$ENDIF}
    end ;

  procedure TGIS_ImportProject.importFromArcExplorer_2x(
    const _path : String
  ) ;
  var
    dir                 : String                    ;
    aep_file            : TIniFile                  ;
    section             : String                    ;
    base                : String                    ;
    i,j,k               : Integer                   ;
    shp_file            : array of String           ;
    shp_name            : array of String           ;
    ll                  : TGIS_LayerVector          ;
    la                  : TGIS_Layer                ;
    symbol_1            : String                    ;
    symbol              : array of t_IProj_symbol   ;
    render              : array of t_IProj_render   ;
    color_1             : String                    ;
    color_2             : TGIS_Color                ;
    visible             : array of String           ;

    dataDir             : String                    ;
    layerCount          : Integer                   ;
    err                 : String                    ;

    function readValue( const _line : String ;
                        const _jump : Byte
                      ) : String;
    var
      w : String;
    begin
      while j <= StringLast(_line) do begin
        w := w + _line[j] ;
        j := j + 1        ;
        if _line[j] = '^' then begin
          j       := j + _jump  ;
          Result  := w          ;
          break                 ;
        end ;
      end ;
      Result := w ;
    end ;

  begin
    err := '' ;
    try

      dir := GetFileDir( _path ) ;

      if not IsStringEmpty( _path ) then begin
        oViewer.Close ;
        i := 1 ;
        aep_file := TIniFile.Create(_path) ;
        base := aep_file.ReadString('MAP','LAYERCOUNT','') ;
        layerCount := StrToInt(base)+ 1  ;
        while i < layerCount do begin
          SetLength( shp_file, i + 1 ) ;
          SetLength( shp_name, i + 1 ) ;
          SetLength( symbol,   i + 1 ) ;
          SetLength( render,   i + 1 ) ;
          SetLength( visible,  i + 1 ) ;
          section := 'MAPLAYER' + IntToStr( i-1 ) ;
          base := aep_file.ReadString( section, 'BASE', '' ) ;
          dataDir := aep_file.ReadString( section, 'WORKSPACE', '' ) ;
          if dataDir = '\' then
            dataDir := ''
          else
            dataDir := dataDir + '\';

          if IsStringEmpty( base ) then break ;

          j := StringFirst ;
          while j <= StringLast(base) do begin
            if base[j] = '^' then
              break ;
            inc(j) ;
          end ;

          inc(j) ;
          while j <= StringLast(base) do begin
            if base[j] = '^' then begin
              shp_file[i] := dataDir + shp_file[i] ;
              break;
            end
            else
              shp_file[i] := shp_file[i] + base[j] ;
            inc( j ) ;
          end ;

          inc( j ) ;
          while j <= StringLast( base ) do begin
            shp_name[i] := shp_name[i] + base[j] ;
            inc(j) ;
          end ;

          symbol_1 := aep_file.ReadString( section, 'SYMBOL', '' ) ;
          j := StringFirst - 1 ;
          for k := 1 to 10 do begin
            inc( j ) ;
            while j < StringLast( symbol_1 ) do begin
              inc( j ) ;
              {$IFDEF OXYGENE}
                symbol[i].x := new String[11] ;
              {$ENDIF}
              symbol[i].x[k] := symbol[i].x[k] + symbol_1[j-1] ;
              if symbol_1[j] = '^' then begin
                break ;
              end ;
            end ;
          end ;

          visible[i] := aep_file.ReadString( section, 'VISIBLE', '' ) ;
          if ( render[i].Typ = '3' ) or ( render[i].Typ = '4' ) then begin
            render[i].Typ  :=
              aep_file.ReadString( section, 'RENDERERTYPE' , '' )  ;
            render[i].Pole :=
              aep_file.ReadString( section, 'RENDERERFIELD', '' )  ;
            symbol_1       :=
              aep_file.ReadString( section, 'RENDERER'     , '' )  ;
            j := 1 ;
            render[i].FontTyp     := readValue( symbol_1, 1 )      ;
            render[i].FontSize    := readValue( symbol_1, 1 )      ;
            render[i].FontBold    := readValue( symbol_1, 1 )      ;
            render[i].FontItalic  := readValue( symbol_1, 1 )      ;
            render[i].BackColor   := readValue( symbol_1, 1 )      ;
            render[i].BackOnOff   := readValue( symbol_1, 1 )      ;

            if render[i].Typ = '3' then
              render[i].FontColor := readValue( symbol_1, 1 )      ;

            if render[i].Typ = '4' then begin
              readValue( symbol_1, 1 )                             ;
              readValue( symbol_1, 1 )                             ;
              readValue( symbol_1, 1 )                             ;
              render[i].FontColor := readValue( symbol_1, 1 )      ;
            end ;
          end ;

          inc( i ) ;
        end ;

        FreeObject( aep_file ) ;
        for j := i - 1 downto 1 do begin
          la := GisCreateLayer( shp_name[j],
                                GetPathAbsolute( dir, shp_file[j] )
                              ) ;
          if not assigned( la ) then continue ;

          if la is TGIS_LayerPixel then  begin
            try
              oViewer.Add( la ) ;
            except
              on e : Exception do begin
                if oViewer.Items.Remove( ll ) < 0 then
                  FreeObject( ll ) ;
                ll := nil ;
                err := err + #13 + e.Message ;
              end ;
            end ;
            continue;
          end ;

          ll := TGIS_LayerVector( la ) ;
          if symbol[j].x[4] = '2' then begin
            ll.Params.Area.OutlineWidth  :=
              -( StrToInt( symbol[j].x[7] ) )                    ;
            color_1                      :=
              arcViewToRGBColor( StrToInt( symbol[j].x[1] ) )    ;
            color_2                      :=
              ParamColor( color_1, TGIS_Color.Red )       ;
            ll.Params.Area.Color         := color_2              ;
            color_1 :=
              arcViewToRGBColor( StrToInt( symbol[j].x[6] ) )    ;
            color_2 :=
              ParamColor( color_1, TGIS_Color.Red )       ;
            ll.Params.Area.OutlineColor  := color_2              ;
            case StrToInt( symbol[j].x[5] ) of
              0 : ll.Params.Area.Pattern := TGIS_BrushStyle.Solid ;
              1 : ll.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
              2 : ll.Params.Area.Pattern := TGIS_BrushStyle.Horizontal ;
              3 : ll.Params.Area.Pattern := TGIS_BrushStyle.Vertical ;
              4 : ll.Params.Area.Pattern := TGIS_BrushStyle.FDiagonal ;
              5 : ll.Params.Area.Pattern := TGIS_BrushStyle.BDiagonal ;
              6 : ll.Params.Area.Pattern := TGIS_BrushStyle.Cross ;
              7 : ll.Params.Area.Pattern := TGIS_BrushStyle.DiagCross ;
            end ;
          end ;

          if symbol[j].x[4] = '1' then begin
            ll.Params.Line.Width         :=
              -( StrToInt( symbol[j].x[7] ) )                    ;
            color_1                      :=
              arcViewToRGBColor( StrToInt( symbol[j].x[1] ) )    ;
            color_2                      :=
              ParamColor( color_1, TGIS_Color.Red )       ;
            ll.Params.Line.Color         := color_2              ;
            color_1                      :=
              arcViewToRGBColor( StrToInt( symbol[j].x[6] ) )    ;
            color_2                      :=
              ParamColor( color_1, TGIS_Color.Red )       ;
            ll.Params.Line.OutlineColor  := color_2              ;
            case StrToInt( symbol[j].x[5] ) of
              0 : ll.Params.Line.Style := TGIS_PenStyle.Solid ;
              1 : ll.Params.Line.Style := TGIS_PenStyle.Dash ;
              2 : ll.Params.Line.Style := TGIS_PenStyle.Dot ;
              3 : ll.Params.Line.Style := TGIS_PenStyle.DashDot ;
              4 : ll.Params.Line.Style := TGIS_PenStyle.DashDotDot ;
              5 : ll.Params.Line.Style := TGIS_PenStyle.Clear ;
            end ;
          end ;

          if symbol[j].x[4] = '0' then begin
            ll.Params.Marker.Size        :=
              -( StrToInt( symbol[j].x[7] ) )                    ;
            color_1                      :=
              arcViewToRGBColor( StrToInt( symbol[j].x[1] ) )    ;
            color_2                      :=
              ParamColor( color_1, TGIS_Color.Red )       ;
            ll.Params.Marker.Color       := color_2              ;
            case StrToInt(symbol[j].x[5]) of
              0: ll.Params.Marker.Style  := TGIS_MarkerStyle.Circle               ;
              1: ll.Params.Marker.Style  := TGIS_MarkerStyle.Box                  ;
              2: ll.Params.Marker.Style  := TGIS_MarkerStyle.TriangleUp           ;
              3: ll.Params.Marker.Style  := TGIS_MarkerStyle.DiagCross            ;
            end ;
            ll.Params.Marker.OutlineWidth := -1      ;
              ll.Params.Marker.OutlineColor := TGIS_Color.Black ;
          end ;

          if ( render[j].Typ = '3' ) or ( render[j].Typ = '4' ) then begin
              ll.Params.Labels.Font.Name    :=
                render[j].FontTyp                                              ;
              ll.Params.Labels.Font.Size    :=
                - StrToInt( render[j].FontSize )                               ;
            color_1                         :=
              arcViewToRGBColor(StrToInt(render[j].FontColor))                 ;
            color_2                         :=
              ParamColor( color_1, TGIS_Color.Red )                     ;
              ll.Params.Labels.Font.Color   := color_2                         ;
            ll.Params.Labels.Color          := color_2                         ;
            ll.Params.Labels.Field          :=
              render[j].Pole                                                   ;
            ll.Params.Labels.Pattern        := TGIS_BrushStyle.Clear    ;
            ll.Params.Labels.OutlinePattern := TGIS_BrushStyle.Clear    ;
            ll.Params.Labels.OutlineStyle   := TGIS_PenStyle.Clear       ;
            if render[j].FontBold   = '1' then
              ll.Params.Labels.Font.Style := GisGetFontStyle( TGIS_FontStyle.Bold )        ;
            if render[j].FontItalic = '1' then
              ll.Params.Labels.Font.Style := GisAddFontStyle( ll.Params.Labels.Font.Style,
                                                              TGIS_FontStyle.Italic )      ;
            if ( render[j].BackOnOff = '1' ) and ( render[j].Typ = '4' ) then
            begin
              ll.Params.Labels.Pattern := TGIS_BrushStyle.Solid ;
              if not IsStringEmpty( render[j].BackColor ) then begin
                color_1 := arcViewToRGBColor(StrToInt(render[j].BackColor))    ;
                color_2 := ParamColor( color_1, TGIS_Color.Red )        ;
                ll.Params.Labels.Color := color_2                              ;
              end ;
            end ;
          end ;

          try
            oViewer.Add( la ) ;
          except
            on e : Exception do begin
              if oViewer.Items.Remove( ll ) < 0 then
                FreeObject( ll ) ;
              ll := nil ;
              err := err + #13 + e.Message ;
            end ;
          end ;

        end ;

        oViewer.FullExtent  ;
        oViewer.InvalidateWholeMap      ;
      end ;

    finally
      if not IsStringEmpty( err ) then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERMISSED ), err, 0 ) ;
    end ;
  end ;

  type
    t_IProj_arcview = record
      ViewNo : String                   ;
    end ;

    t_IProj_arclayer = record
      Name          : String            ;
      NShpSrc       : String            ;
      NLegend       : String            ;
      FilePath      : String            ;
      LayerType     : TGIS_ShapeType    ;
      ColorR        : String            ;
      ColorG        : String            ;
      ColorB        : String            ;
      OutlineColorR : String            ;
      OutlineColorG : String            ;
      OutlineColorB : String            ;
      OutlineStyle  : String            ;
      OutlineWidth  : String            ;
      LineWidth     : String            ;
      MarkerSize    : String            ;
      Style         : String            ;
    end ;

  procedure TGIS_ImportProject.importArcView_3x(
    const _path : String
  ) ;
  var
    oview      : t_IProj_arcview         ;
    olayer     : t_IProj_arclayer        ;
    fileapr    : TGIS_BufferedFileStream ;
    sline      : String                  ;
    dir        : String                  ;
    i,j        : Integer                 ;
    scolor     : String                  ;
    color      : TGIS_Color              ;
    retval     : Boolean                 ;
    line_no    : Integer                 ;
    snumber    : array of String         ;
    ll         : TGIS_Layer              ;
    lv         : TGIS_LayerVector        ;
    temp       : String                  ;
    dead       : Integer                 ;
    err        : String                  ;
    loop_leave : Boolean                 ;

    function end_of_section( const _line : String ) : Boolean ;
    begin
      Result := _line = ')' ;
    end ;

    procedure read_line ;
    begin
      if fileapr.Eof then begin
        fileapr.Position := 0 ;
        line_no := 0;
      end ;
      sline := fileapr.ReadLine ;
      inc( line_no ) ;
    end ;

    procedure loop_end ;
    begin
      // we read info about layers in first view
          i := 1 ;
          {$IFNDEF OXYGENE}
            while not IsStringEmpty( snumber[i] ) do begin
          {$ELSE}
            while ( not IsStringEmpty( snumber[i] ) ) and
                  ( assigned( snumber[i] )          ) do begin
          {$ENDIF}
            read_line ;
            if fileapr.Eof then begin
              fileapr.Position := 0 ;
              i:= i + 1;
              end ;
            if (Pos( '(FTheme.' + snumber[i], sline ) > StringFirst - 1) OR
               (Pos( '(ITheme.' + snumber[i], sline ) > StringFirst - 1) then begin
              read_line ;

              getToken( sline ) ;
              olayer.Name := getToken( sline ) ;
              read_line ;
              getToken( sline ) ;
              olayer.NShpSrc := getToken( sline ) ;
              read_line ;
              read_line ;
              getToken( sline );
              olayer.NLegend := getToken( sline ) ;  //number of legends
              while Pos( '(FN.' + IntToStr( StrToInt( olayer.NShpSrc ) + 2 ),
                         sline
                       ) = StringFirst - 1 do
              begin
                read_line ;
              end ;
              read_line ;

              // where is shp file
              getToken( sline );
              olayer.FilePath := slashToBackslash( getToken( sline ) ) ;

              while ( Pos( '(BShSym.', sline ) = StringFirst - 1 ) and   // Polygon
                    ( Pos( '(BMkSym.', sline ) = StringFirst - 1 ) and   // Point
                    ( Pos( '(BLnSym.', sline ) = StringFirst - 1 ) do    // Line
              begin
                read_line ;
              end ;

              if ( Pos( '(BShSym.', sline ) > StringFirst - 1 ) then begin
                olayer.LayerType := TGIS_ShapeType.Polygon ;
                read_line ;
                read_line ;
                getToken(sline);
                olayer.OutlineStyle := getToken( sline ) ;   //?
                read_line ;
                read_line ;
                read_line ;
                read_line ;
                getToken(sline);
                olayer.Style        := getToken( sline ) ;
              end ;
              if ( Pos( '(BLnSym.', sline ) > StringFirst - 1 ) then begin
                olayer.LayerType := TGIS_ShapeType.Arc     ;
                while sline <> ')' do
                  begin
                  read_line ;
                  if Pos('Width:',sline) > StringFirst - 1 then
                    begin
                    getToken( sline ) ;
                    olayer.LineWidth := getToken( sline ) ;
                    Break ;
                    end ;
                  end ;
              end ;
              if ( Pos( '(BMkSym.', sline ) > StringFirst - 1 ) then begin
                olayer.LayerType := TGIS_ShapeType.Point   ;
                while sline <> ')' do
                  begin
                  read_line ;
                  if Pos('Size:',sline) > StringFirst - 1 then
                    begin
                    getToken( sline ) ;
                    olayer.MarkerSize := getToken( sline ) ;
                    Break ;
                    end ;
                  end ;
              end ;

              while Pos( '(TClr.', sline ) = StringFirst - 1 do begin
                read_line ;
              end ;
              read_line ;
              olayer.ColorR := '0' ;
              olayer.ColorG := '0' ;
              olayer.ColorB := '0' ;
              while not end_of_section(sline) do begin
                if Pos( #9 + 'Red:' + #9, sline ) > StringFirst - 1 then begin
                  olayer.ColorR :=
                    IntToStr( StrToInt( '$' + Copy( sline, 8+StringFirst, 2 ) ) ) ;
                end ;
                if Pos( #9 + 'Green:' + #9, sline ) > StringFirst - 1 then begin
                  olayer.ColorG :=
                    IntToStr( StrToInt( '$'+ Copy( sline, 10+StringFirst, 2 ) ) ) ;
                end ;
                if Pos( #9 + 'Blue:' + #9, sline ) > StringFirst - 1 then begin
                  olayer.ColorB :=
                    IntToStr( StrToInt( '$'+ Copy( sline, 9+StringFirst, 2 ) ) ) ;
                end ;

                read_line ;
              end ;

              while Pos( '(TClr.', sline ) = StringFirst - 1 do begin
                read_line ;
              end ;
              read_line ;
              olayer.OutlineColorR := '0' ;
              olayer.OutlineColorG := '0' ;
              olayer.OutlineColorB := '0' ;
              if ( not end_of_section(sline) ) and
                 ( Pos( 'Transparent', sline ) = StringFirst - 1 ) then
              begin
                while not end_of_section(sline) do begin
                  if Pos( #9 + 'Red:' + #9, sline ) > StringFirst - 1 then begin
                    olayer.OutlineColorR :=
                      IntToStr( StrToInt( '$' + Copy( sline, 8+StringFirst, 2 ) ) ) ;
                  end ;
                  if Pos( #9 + 'Green:' + #9, sline ) > StringFirst - 1 then begin
                    olayer.OutlineColorG :=
                      IntToStr( StrToInt( '$'+ Copy( sline, 10+StringFirst, 2 ) ) ) ;
                  end ;
                  if Pos( #9 + 'Blue:' + #9, sline ) > StringFirst - 1 then begin
                    olayer.OutlineColorB :=
                      IntToStr( StrToInt( '$'+ Copy( sline, 9+StringFirst, 2 ) ) ) ;
                  end ;

                  read_line ;
                end ;
              end ;

              // read project
              temp := GetFileExt(olayer.FilePath);
              if temp = '.dgn' then olayer.Name:= '';
              ll := GisCreateLayer(
                      '',
                      GetPathAbsolute( dir, olayer.FilePath )
                    ) ;

              if assigned( ll ) then begin
                try
                  oViewer.Add( ll ) ;
                  ll.Move(1000);
                except
                  on e : Exception do begin
                    if oViewer.Items.Remove( ll ) < 0 then
                      FreeObject( ll ) ;
                    ll := nil ;
                    err := err + #13 + e.Message ;
                  end ;
                end ;
              end ;

              if assigned( ll ) and ( ll is TGIS_LayerVector ) then begin
                lv := ll as TGIS_LayerVector ;

                scolor := olayer.ColorR + ':' +
                          olayer.ColorG + ':' +
                          olayer.ColorB ;
                color  := ParamColor( scolor, TGIS_Color.Red ) ;

                case olayer.LayerType of
                  TGIS_ShapeType.Polygon :
                    begin
                      if IsStringEmpty( olayer.Style ) then
                        lv.Params.Area.Pattern := TGIS_BrushStyle.Clear  ;
                      if olayer.Style = '0x01' then begin
                          lv.Params.Area.Color := color ;
                        end ;
                      if olayer.Style = '0x03' then begin
                          lv.Params.Area.Color := color ;
                          lv.Params.Area.Pattern := TGIS_BrushStyle.DiagCross  ;
                        end ;
                      lv.Params.Area.ShowLegend := True ;
                    end ;
                  TGIS_ShapeType.Arc :
                    begin
                      if olayer.LineWidth<>'' then begin
                        if ((DotStrToFloat(olayer.LineWidth)) > 0) and
                           ((DotStrToFloat(olayer.LineWidth)) < 1)
                        then
                          lv.Params.Line.Width := -1
                        else
                          lv.Params.Line.Width :=
                            -RoundS( DotStrToFloat( olayer.LineWidth ) ) ;
                      end ;
                      lv.Params.Line.Color := color ;
                      lv.Params.Line.ShowLegend := True ;
                    end ;
                  TGIS_ShapeType.Point :
                    begin
                      lv.Params.Marker.Color := color ;
                      lv.Params.Marker.ShowLegend := True ;
                    end ;
                end ;

                scolor := olayer.OutlineColorR + ':' +
                          olayer.OutlineColorG + ':' +
                          olayer.OutlineColorB ;
                color  := ParamColor( scolor, TGIS_Color.Red ) ;

                if olayer.LayerType = TGIS_ShapeType.Polygon then begin
                  lv.Params.Area.OutlineColor := color ;
                  {$IFDEF OXYGENE}
                    if not assigned( olayer.OutlineWidth ) then
                      olayer.OutlineWidth := '' ;
                  {$ENDIF}
                  if not IsStringEmpty( olayer.OutlineWidth ) then begin
                    if ( ( DotStrToFloat( olayer.OutlineWidth ) ) > 0) and
                       ( ( DotStrToFloat( olayer.OutlineWidth ) ) < 1)
                    then
                      lv.Params.Area.OutlineWidth := -1
                    else
                      lv.Params.Area.OutlineWidth :=
                        -RoundS( DotStrToFloat( olayer.OutlineWidth ) ) ;
                  end ;
                end ;

                if not IsStringEmpty( olayer.MarkerSize ) then begin
                  lv.Params.Marker.Style := TGIS_MarkerStyle.Circle ;
                  lv.Params.Marker.Size  :=
                    -RoundS( DotStrToFloat( olayer.MarkerSize ) ) ;
                end ;
              end ;

              olayer.Name     := '' ;
              olayer.FilePath := '' ;
              inc( i ) ;

            end ;

          end ;
    end ;

  begin
    err := '' ;
    try

      retval := False ;

      dir := GetFileDir( _path ) ;

      try
        oViewer.Close ;
        // read file apr
        fileapr := TGIS_BufferedFileStream.Create( _path, TGIS_StreamMode.Read ) ;

        try
          line_no := 0;
          // read views
          while not fileapr.Eof do begin
            if retval then break ;
            read_line ;
            if Pos('(Project.', sline ) > StringFirst - 1 then begin
              // this is project file
              while not end_of_section( sline ) do begin
                read_line ;
                if Pos(#9 + 'ActiveDoc:', sline ) > StringFirst - 1 then begin
                  // number of view
                  getToken( sline ) ;
                  oview.ViewNo := getToken( sline ) ;
                  break ;  // only first view ....
                end ;
              end ;
              retval := True ;
            end ;
          end ;

          if IsStringEmpty( oview.ViewNo ) then begin
            fileapr.Position := 0 ;
            line_no := 0;
            // read views
            while not fileapr.Eof do begin
              read_line ;
              if Pos( '(View.', sline ) > StringFirst - 1 then begin
                // number of view
                oview.ViewNo := Copy( sline, Pos( '.', sline ) + 1, 8192 ) ;
                break ;  // only first view ....
              end ;
            end ;
          end ;

          // read data
          dead := 0 ;
          loop_leave := False ;
          while not IsStringEmpty( oview.ViewNo ) do begin
            fileapr.Position := 0 ;
            line_no := 0;
            while not fileapr.Eof do begin
              read_line ;
              inc( dead ) ;
              if dead >= 1000000 then begin
                raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ),
                                             _path,
                                             line_no
                                           ) ;
              end ;

              if Pos( '(View.' + oview.ViewNo, sline ) > StringFirst - 1 then begin
                j := 1 ;
                while not end_of_section( sline ) do begin
                  SetLength(snumber, j + 1);
                  read_line ;
                  if end_of_section( sline ) then begin
                    // because we read only first view
                    loop_leave := True ;
                    break ;
                  end ;
                  if Pos( #9 + 'Theme:' + #9, sline ) > StringFirst - 1 then begin
                    getToken( sline ) ;
                    snumber[j] := getToken( sline ) ;
                    inc(j) ;
                  end ;
                end ;
              end ;
              if loop_leave then break ;
            end ;
            if loop_leave then break ;
          end ;

          loop_end ;

        finally
          FreeObject( fileapr ) ;
        end ;

        oViewer.FullExtent;
        oViewer.InvalidateWholeMap;

      finally
        if not retval then
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ),
                                       _path,
                                       line_no
                                     ) ;
      end ;

    finally
      if not IsStringEmpty( err ) then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERMISSED ), err, 0 ) ;
    end ;
  end ;

  type
    t_IProj_global = record          // Layer properties from ArcView project f.
      SymbolStyle    : String   ;
      SymbolColor    : String   ;
      SymbolSize     : String   ;
      PenStyle       : String   ;
      PenWidth       : String   ;
      PenColor       : String   ;
      BrushStyle     : String   ;
      BrushColor     : String   ;
      BrushBackColor : String   ;
      Display        : String   ;   // info about display layer - MIF or pixel
                                    // with or without properties
      LineWidth      : String   ;
      LineStyle      : String   ;
      LineColor      : String   ;
      DisplayGraphic : Boolean  ;   // true when display without properties
      LayerPixel     : Boolean  ;   // layer is nonMIF (pixel)
      //LayerPixelType : String   ;   // eg. tiff, bmp ...
      //LayerPixelFile : String   ;   // name of pixel pile (with directory)
    end ;

    t_IProj_maplabel = record             // properties of labels
      Line           : String   ;
      Position_1     : String   ;
      Position_2     : String   ;
      FontName       : String   ;
      FontType       : String   ;
      FontSize       : String   ;
      FontColor      : String   ;
      FontBackColor  : String   ;

      Name           : String   ;

      Parallel       : String   ;
      Auto           : String   ;
      Overlap        : String   ;    // visibility of label
      Duplicates     : String   ;
      Offset         : String   ;

      Visibility     : String   ;
    end ;

  procedure TGIS_ImportProject.importMapInfo_6x(
    const _path : String
  ) ;
  var
    dir         : String   ;
    worfile     : TGIS_BufferedFileStream ;
    sline       : String   ;
    artabfile   : array of String ;
    artabname   : array of String ;
    arworopen   : array of String ;
    arworglobal : array of t_IProj_global   ;
    arworlabel  : array of t_IProj_maplabel ;
    ll          : TGIS_LayerVector   ;
    cnt         : Integer            ;
    tmp         : String             ;
    ext         : String             ;
    err         : String             ;
    centerPtg   : TGIS_Point ;
    zoom        : Double ;

    procedure read_open_table ;
    var
      i    : Integer;
    begin
      i := 0 ;
      while not worfile.Eof do begin
        SetLength(artabfile, i + 1);
        SetLength(artabname, i + 1);
        // cut off 'OPEN TABLE' String
        getToken( sline ) ;
        getToken( sline ) ;

        // name of file
        tmp := getToken( sline ) ;
        ext := GetFileExt( tmp ) ;

        if IsStringEmpty( ext ) then begin
          if not SafeFileExists(  GetPathAbsolute( dir, GetPathNoExt( tmp ) + '.tab' ) )
          then
            artabfile[ i ] := GetPathNoExt( tmp ) + '.mif'
          else
            artabfile[ i ] := GetPathNoExt( tmp ) + '.tab' ;
        end
        else
          artabfile[ i ] := tmp ;

        getToken( sline ) ;

        // name of layer in project
        artabname[ i ] := getToken( sline );
        sline := worfile.ReadLine ;

        // end of section ?
        if Pos( 'OPEN TABLE', UpperCase( sline ) ) = StringFirst - 1 then break ;

        inc( i ) ;
      end ;

    end ;

    procedure read_map_from ;
    var
      temp   : String  ;
      i      : Integer ;
    begin
      i :=0 ;
      while not worfile.Eof do begin
        // cut off 'MAP FROM' String
        temp := getToken( sline ) ;
        temp := getToken( sline ) ;

        while UpperCase( temp ) <> 'POSITION' do begin
          SetLength(arworopen, i + 1);
          temp := getToken( sline ) ;
          if not IsStringEmpty( temp ) then
            arworopen[ i ] := temp
          else begin
            sline := worfile.ReadLine ;
            dec( i );

            // end of section?
            if ( Pos( 'POSITION', UpperCase( sline ) ) <> StringFirst - 1 ) or
               ( Pos( 'SET MAP', UpperCase( sline ) ) <> StringFirst - 1 ) then
            begin
              exit ;
            end ;
          end ;
          inc( i ) ;
        end ;
      end ;
    end ;

    procedure read_set_map ;
    var
      tkn     : String ;
      params  : String ;
    begin
      zoom := 0 ;
      centerPtg := GisPoint( 0, 0 ) ;

      while not worfile.Eof do begin
        sline := worfile.ReadLine ;
        params := sline ;
        tkn    := getToken( params ) ;

        if tkn = 'Center' then begin
          // Center (X,Y)
          centerPtg.X := DotStrToFloat( getToken( params ) ) ;
          centerPtg.Y := DotStrToFloat( getToken( params ) ) ;
        end ;

        if tkn = 'Zoom' then begin
          // Zoom z Units "u"
          zoom := DotStrToFloat( getToken( params ) ) ;
        end ;

        if ( Pos( 'SET MAP', UpperCase( sline ) ) <> StringFirst - 1 ) then exit ;

      end ;
    end ;

    function read_set_map_layer : Integer;
    var
      temp     : String  ;
      i        : Integer ;
      slineTmp : String  ;

    begin
      i := 0 ;
      while not worfile.Eof do begin
        sline := worfile.ReadLine ;

        if ( Pos( 'LAYER ' + IntToStr( i+1 ), UpperCase( sline ) ) > StringFirst - 1 ) and
           ( Pos( 'SET MAP',                  UpperCase( sline ) ) = StringFirst - 1 ) then
        begin
          SetLength(arworglobal, i + 1);
          SetLength(arworlabel, i + 1);
          // start reading new layer section
          sline := worfile.ReadLine ;
          temp := getToken( sline ) ;
          if UpperCase( temp ) = 'DISPLAY' then begin
            temp := getToken( sline ) ;
            if UpperCase( temp ) = 'VALUE' then
              begin
              i := i + 1;
              continue;
              end ;
            if ( UpperCase( temp ) = 'ON'      ) or
               ( UpperCase( temp ) = 'OFF'     ) or
               ( UpperCase( temp ) = 'GLOBAL'  ) then
            begin
              arworglobal[i].DisplayGraphic := False  ; // vector layer
              arworglobal[i].Display        := UpperCase( temp ) ; // on/off layer
              arworglobal[i].LayerPixel     := False  ;
            end ;
            if ( UpperCase( temp ) = 'GRAPHIC' ) then begin
               arworglobal[i].DisplayGraphic := True  ; // pixel layer
            end ;
          end ;

          sline := worfile.ReadLine ;
          temp := getToken( sline ) ;

          if UpperCase( temp ) = 'GLOBAL' then begin
            // layer is type point
            slineTmp := UpperCase( sline ) ;
            Delete(slineTmp,StringFirst,Pos('SYMBOL',slineTmp) - 1);
            if Pos( 'SYMBOL', UpperCase( slineTmp ) ) > StringFirst - 1 then begin
              temp := getToken( slineTmp ) ;
              arworglobal[i].SymbolStyle := getToken( slineTmp ) ;
              arworglobal[i].SymbolColor := getToken( slineTmp ) ;
              arworglobal[i].SymbolSize  := getToken( slineTmp ) ;
            end ;

            // layer is type polygon
            slineTmp := UpperCase( sline ) ;
            Delete(slineTmp,StringFirst,Pos('PEN',slineTmp) - 1);
            if Pos( 'PEN', UpperCase( slineTmp ) ) > StringFirst - 1 then begin
              temp := getToken( slineTmp ) ;
              arworglobal[i].PenWidth := getToken( slineTmp ) ;
              arworglobal[i].PenStyle := getToken( slineTmp ) ;
              arworglobal[i].PenColor := getToken( slineTmp ) ;

              slineTmp := UpperCase( sline ) ;
              Delete(slineTmp,StringFirst,Pos('BRUSH',slineTmp) - 1);
              if Pos( 'BRUSH', UpperCase( slineTmp ) ) > StringFirst - 1 then begin
                temp := getToken( slineTmp ) ;
                arworglobal[i].BrushStyle     := getToken( slineTmp ) ;
                arworglobal[i].BrushColor     := getToken( slineTmp ) ;
                arworglobal[i].BrushBackColor := getToken( slineTmp ) ;
              end ;
            end ;

            // layer is type line
            slineTmp := UpperCase( sline ) ;
            Delete(slineTmp,StringFirst,Pos('LINE',slineTmp) - 1);
            if Pos( 'LINE', UpperCase( slineTmp ) ) > StringFirst - 1 then begin
              temp := getToken( slineTmp );
              arworglobal[i].LineWidth  := getToken( slineTmp ) ;
              arworglobal[i].LineStyle  := getToken( slineTmp ) ;
              arworglobal[i].LineColor  := getToken( slineTmp ) ;
            end ;

            sline := worfile.ReadLine ;
          end ;

          temp := getToken( sline ) ;
          if UpperCase( temp ) = 'LABEL' then begin
            temp := getToken( sline );
            arworlabel[i].Line       := getToken( sline );

            temp := getToken( sline ) ;
            arworlabel[i].Position_1 := UpperCase( getToken( sline ) ) ;

            temp := getToken( sline ) ;
            if UpperCase( temp ) <> 'FONT' then
              arworlabel[i].Position_2 := UpperCase( temp ) ;

            //if UpperCase( temp ) <> 'FONT' then
            //  temp := getToken( sline );
            arworlabel[i].FontName   := getToken( sline );
            arworlabel[i].FontType   := getToken( sline );
            arworlabel[i].FontSize   := getToken( sline );
            arworlabel[i].FontColor  := '' ;
            arworlabel[i].Name       := '' ;

            temp := getToken( sline ) ;
            if UpperCase( temp ) <> 'PEN' then
              arworlabel[i].FontBackColor := temp;
          end ;

          sline := worfile.ReadLine ;
          temp := getToken( sline );
          if UpperCase( temp ) = 'WITH' then begin
            { TODO -cReview : not implemented yet }
          end ;
          sline := worfile.ReadLine ;
          temp := getToken( sline );
          if UpperCase( temp ) = 'PARALLEL' then begin
            arworlabel[i].Parallel   := UpperCase( getToken( sline ) ) ;
            temp                     := getToken( sline ) ;
            arworlabel[i].Auto       := UpperCase( getToken( sline ) ) ;
            temp                     := getToken( sline ) ;
            arworlabel[i].Overlap    := UpperCase( getToken( sline ) ) ;
            temp                     := getToken( sline ) ;
            arworlabel[i].Duplicates := UpperCase( getToken( sline ) ) ;
            temp                     := getToken( sline ) ;
            arworlabel[i].Offset     := UpperCase( getToken( sline ) ) ;
          end ;
          sline := worfile.ReadLine ;
          if Pos( 'OBJECT', UpperCase( sline ) ) = StringFirst - 1 then begin
            temp := getToken( sline );
            if UpperCase( temp ) = 'VISIBILITY' then begin
              arworlabel[i].Visibility := getToken( sline );
            end ;
          end ;

        inc( i ) ;
        end ;
      end ;

      Result := i ;
    end ;

    procedure load_project( const _count : Integer ) ;
    var
      j,k  : Integer ;
      itmp : Integer ;
      cl : TGIS_Color  ;
      la   : TGIS_Layer ;
      lp   : TGIS_LayerPixel    ;
    begin
      // reading project
      for j:= high( arworopen ) downto 0 do begin
        if IsStringEmpty( arworopen[ j ] ) then
          continue ;

        la := nil ;

        for k := low( artabname ) to high( artabname ) do begin
          if ( arworopen[ j ] = artabname[ k ] ) then begin
            la := GisCreateLayer(
                    arworopen[ j ],
                    GetPathAbsolute( dir, artabfile[ k ] )
                  ) ;
            break ;
            end ;
          end ;

          if assigned( la ) then begin
            if la is TGIS_LayerVector then begin
              ll := TGIS_LayerVector( la ) ;
            end
            else begin
              ll := nil ;
              lp := TGIS_LayerPixel( la ) ;
              oViewer.Add( lp ) ;
              continue;
            end ;
          end
          else continue ;

        with ll.Params do begin

          // double name
          k := 0 ;
          while k < j do begin
            // if name of layer is double
            if arworopen[ k ] = arworopen[ j ] then begin
              arworopen[ j ] := arworopen[ j ] + '_' + IntToStr( j ) ; // suffix
              break;
            end ;
            inc( k ) ;
          end ;
          ll.Name := arworopen[ j ] ;

          if j < length( arworglobal ) then begin

            if arworglobal[ j ].DisplayGraphic then begin
              // not implemented yet
            end ;

            // polygon
            if not IsStringEmpty( arworglobal[ j ].LineColor ) then begin
              try
                itmp := StrToInt( arworglobal[ j ].LineColor ) ;
              except
                itmp := 0 ;
              end ;

              Line.Color := ParamColor( mapInfoToRGBColor( itmp ), TGIS_Color.Red ) ;
            end ;

            if not IsStringEmpty( arworglobal[ j ].LineWidth ) then begin
              try
                itmp := StrToInt( arworglobal[ j ].LineWidth ) ;
              except
                itmp := 0 ;
              end ;

              if itmp < 10 then Line.Width := - itmp
                           else Line.Width := RoundS( 1.0 * itmp / 10 ) ;
            end ;

            if not IsStringEmpty( arworglobal[ j ].LineStyle ) then begin
              try
                itmp := StrToInt( arworglobal[ j ].LineStyle ) ;
              except
                itmp := 0 ;
              end ;

              if      ( itmp  =  3 ) then Line.Style := TGIS_PenStyle.Dot
              else if ( itmp >=  4 ) and
                      ( itmp <= 13 ) then Line.Style := TGIS_PenStyle.Dash
              else if ( itmp >= 14 ) and
                      ( itmp <= 19 ) then Line.Style := TGIS_PenStyle.DashDot
              else if ( itmp >= 20 ) and
                      ( itmp <= 25 ) then Line.Style := TGIS_PenStyle.DashDotDot  ;
            end ;

            // shapes
            if not IsStringEmpty( arworglobal[ j ].BrushColor ) then begin
              try
                itmp := StrToInt( arworglobal[ j ].BrushColor ) ;
              except
                itmp := 0 ;
              end ;

              Area.Color := ParamColor( mapInfoToRGBColor( itmp ), TGIS_Color.Red ) ;
            end ;

            if not IsStringEmpty( arworglobal[ j ].PenWidth ) then begin
              try
                itmp := StrToInt( arworglobal[ j ].PenWidth ) ;
              except
                itmp := 0 ;
              end ;

              Area.OutlineWidth := - itmp ;
              if arworglobal[ j ].PenColor = '0' then Area.OutlineWidth := 0 ;
            end ;

            if not IsStringEmpty( arworglobal[ j ].PenColor ) then begin
              try
                itmp := StrToInt( arworglobal[ j ].PenColor ) ;
              except
                itmp := 0 ;
              end ;

              Area.OutlineColor := ParamColor( mapInfoToRGBColor( itmp ), TGIS_Color.Red ) ;
            end ;

            if not IsStringEmpty( arworglobal[ j ].BrushStyle ) then begin
              try
                itmp := StrToInt( arworglobal[ j ].BrushStyle ) ;
              except
                itmp := 0 ;
              end ;

              case itmp of
                1 : Area.Pattern := TGIS_BrushStyle.Clear       ;
                2 : Area.Pattern := TGIS_BrushStyle.Solid       ;
                3 : Area.Pattern := TGIS_BrushStyle.Horizontal  ;
                4 : Area.Pattern := TGIS_BrushStyle.Vertical    ;
                5 : Area.Pattern := TGIS_BrushStyle.BDiagonal   ;
                6 : Area.Pattern := TGIS_BrushStyle.FDiagonal   ;
                7 : Area.Pattern := TGIS_BrushStyle.Cross       ;
                8 : Area.Pattern := TGIS_BrushStyle.DiagCross   ;
              end ;
            end ;

            // points
            if not IsStringEmpty( arworglobal[ j ].SymbolColor ) then begin
              try
                itmp := StrToInt( arworglobal[ j ].SymbolColor ) ;
              except
                itmp := 0 ;
              end ;

              ll.Params.Marker.Color := ParamColor( mapInfoToRGBColor( itmp ), TGIS_Color.Red ) ;
            end ;

            if not IsStringEmpty( arworglobal[ j ].SymbolSize ) then begin
              try
                itmp := StrToInt( arworglobal[ j ].SymbolSize ) ;
              except
                itmp := 0 ;
              end ;

              Marker.Size := -itmp ;   //?
            end ;

            if not IsStringEmpty( arworglobal[ j ].SymbolStyle ) then begin
              try
                itmp := StrToInt( arworglobal[ j ].SymbolStyle ) ;
              except
                itmp := 0 ;
              end ;

              case itmp of
                32 : Marker.Style := TGIS_MarkerStyle.Circle       ;
                34 : Marker.Style := TGIS_MarkerStyle.Box          ;
                44 : Marker.Style := TGIS_MarkerStyle.Box          ;
                56 : Marker.Style := TGIS_MarkerStyle.Cross        ;
                50 : Marker.Style := TGIS_MarkerStyle.DiagCross    ;
                36 : Marker.Style := TGIS_MarkerStyle.TriangleUp   ;
                37 : Marker.Style := TGIS_MarkerStyle.TriangleDown ;
              end ;
            end ;

            // labels
            if ( ( arworglobal[ j ].Display='ON'     ) or
                 ( arworglobal[ j ].Display='GLOBAL' )
               ) and
               ( arworlabel[ j ].Auto='ON' )
            then begin
              itmp := 0 ;
              try
                if not IsStringEmpty( arworlabel[ j ].FontColor ) then
                  itmp := StrToInt( arworlabel[ j ].FontColor ) ;
              except
              end ;

                Labels.Font.Color :=
                  ParamColor( mapInfoToRGBColor( itmp ), TGIS_Color.Red ) ;
              Labels.OutlineColor :=
                ParamColor( mapInfoToRGBColor( itmp ), TGIS_Color.Red ) ;
              Labels.Color        :=
                ParamColor( mapInfoToRGBColor( itmp ), TGIS_Color.Red ) ;

              try
                itmp := StrToInt( arworlabel[ j ].FontSize ) ;
              except
                itmp := 0 ;
              end ;

                Labels.Font.Size := -itmp;
              Labels.Field := arworlabel[ j ].Name;

              // color
              if not IsStringEmpty( arworlabel[ j ].FontColor ) then begin
                try
                  itmp := StrToInt( arworlabel[ j ].FontColor ) ;
                except
                  itmp := 0 ;
                end ;

                cl := ParamColor(mapInfoToRGBColor( itmp ), TGIS_Color.Red ) ;
              end
              else
                cl := TGIS_Color.Black ;

                Labels.Font.Name := arworlabel[ j ].FontName ;

              if arworlabel[ j ].Parallel ='ON' then
                Labels.Alignment := TGIS_LabelAlignment.Follow
              else
                Labels.Alignment := TGIS_LabelAlignment.Single ;

              // background color
              try
                itmp := StrToInt( arworlabel[ j ].FontType ) ;
              except
                itmp := 0 ;
              end ;

              if itmp < 256 then begin
                if not IsStringEmpty( arworlabel[ j ].FontBackColor ) then begin
                  Labels.Color          := cl ;
                  Labels.OutlineStyle   := TGIS_PenStyle.Clear  ;
                end
                else begin
                  Labels.Pattern        := TGIS_BrushStyle.Clear  ;
                  Labels.OutlinePattern := TGIS_BrushStyle.Clear  ;
                  Labels.OutlineStyle   := TGIS_PenStyle.Clear  ;
                end ;

                case itmp of
                   1 : Labels.Font.Style  := GisGetFontStyle( TGIS_FontStyle.Bold      ) ;
                   2 : Labels.Font.Style  := GisGetFontStyle( TGIS_FontStyle.Italic    ) ;
                   3 : begin
                        Labels.Font.Style := GisGetFontStyle( TGIS_FontStyle.Bold      ) ;
                        Labels.Font.Style := GisAddFontStyle( Labels.Font.Style, TGIS_FontStyle.Italic    ) ;
                       end ;
                   4 : Labels.Font.Style  := GisGetFontStyle( TGIS_FontStyle.Underline ) ;
                   5 : begin
                        Labels.Font.Style := GisGetFontStyle( TGIS_FontStyle.Bold      ) ;
                        Labels.Font.Style := GisAddFontStyle( Labels.Font.Style, TGIS_FontStyle.Underline ) ;
                       end ;
                   6 : begin
                        Labels.Font.Style := GisGetFontStyle( TGIS_FontStyle.Italic    ) ;
                        Labels.Font.Style := GisAddFontStyle( Labels.Font.Style, TGIS_FontStyle.Underline ) ;
                       end ;
                   7 : begin
                        Labels.Font.Style := GisGetFontStyle( TGIS_FontStyle.Bold      ) ;
                        Labels.Font.Style := GisAddFontStyle( Labels.Font.Style, TGIS_FontStyle.Italic    ) ;
                        Labels.Font.Style := GisAddFontStyle( Labels.Font.Style, TGIS_FontStyle.Underline ) ;
                       end ;
                end ;

              end
              else begin
                // painting of background
                if not IsStringEmpty( arworlabel[j].FontBackColor ) then begin
                  Labels.Color          := cl ;
                  Labels.Pattern        := TGIS_BrushStyle.Solid  ;
                  Labels.OutlineStyle   := TGIS_PenStyle.Clear  ;
                end
                else begin
                  Labels.Pattern        := TGIS_BrushStyle.Clear  ;
                  Labels.OutlinePattern := TGIS_BrushStyle.Clear  ;
                  Labels.OutlineStyle   := TGIS_PenStyle.Clear  ;
                end ;

                case itmp of
                  257 : Labels.Font.Style   := GisGetFontStyle( TGIS_FontStyle.Bold      ) ;
                  258 : Labels.Font.Style   := GisGetFontStyle( TGIS_FontStyle.Italic    ) ;
                  259 : begin
                          Labels.Font.Style := GisGetFontStyle( TGIS_FontStyle.Bold      ) ;
                          Labels.Font.Style := GisAddFontStyle( Labels.Font.Style, TGIS_FontStyle.Italic    ) ;
                        end ;
                  260 : Labels.Font.Style   := GisGetFontStyle( TGIS_FontStyle.Underline ) ;
                  261 : begin
                          Labels.Font.Style := GisGetFontStyle( TGIS_FontStyle.Bold      ) ;
                          Labels.Font.Style := GisAddFontStyle( Labels.Font.Style, TGIS_FontStyle.Underline ) ;
                        end ;
                  262 : begin
                          Labels.Font.Style := GisGetFontStyle( TGIS_FontStyle.Italic    ) ;
                          Labels.Font.Style := GisAddFontStyle( Labels.Font.Style, TGIS_FontStyle.Underline ) ;
                        end ;
                  263 : begin
                          Labels.Font.Style := GisGetFontStyle( TGIS_FontStyle.Bold      ) ;
                          Labels.Font.Style := GisAddFontStyle( Labels.Font.Style, TGIS_FontStyle.Italic    ) ;
                          Labels.Font.Style := GisAddFontStyle( Labels.Font.Style, TGIS_FontStyle.Underline ) ;
                        end ;
                end ;
              end ;

              // position of label
              if      arworlabel[ j ].Position_1 = 'ABOVE' then begin
                if      arworlabel[ j ].Position_2 = 'LEFT'
                        then Labels.Position := [ TGIS_LabelPosition.UpLeft       ]
                else if IsStringEmpty( arworlabel[ j ].Position_2 )
                        then Labels.Position := [ TGIS_LabelPosition.UpCenter     ]
                else if arworlabel[ j ].Position_2 ='RIGHT'
                        then Labels.Position := [ TGIS_LabelPosition.UpRight      ] ;
              end
              else if IsStringEmpty( arworlabel[ j ].Position_1 ) then begin
                if      arworlabel[ j ].Position_2 = 'LEFT'
                        then Labels.Position := [ TGIS_LabelPosition.MiddleLeft   ]
                else if IsStringEmpty( arworlabel[ j ].Position_2 )
                        then Labels.Position := [ TGIS_LabelPosition.MiddleCenter ]
                else if arworlabel[ j ].Position_2 ='RIGHT'
                        then Labels.Position := [ TGIS_LabelPosition.MiddleRight  ] ;
              end
              else if arworlabel[ j ].Position_1 = 'BELOW' then begin
                if      arworlabel[ j ].Position_2 = 'LEFT'
                        then Labels.Position := [ TGIS_LabelPosition.DownLeft     ]
                else if IsStringEmpty( arworlabel[ j ].Position_2 )
                        then Labels.Position := [ TGIS_LabelPosition.DownCenter   ]
                else if arworlabel[ j ].Position_2 ='RIGHT'
                        then Labels.Position := [ TGIS_LabelPosition.DownRight    ] ;
              end ;

            end ;
          end ;

          if not IsStringEmpty( ll.Path ) then begin
            try
              oViewer.Add( ll ) ;
            except
              on e : Exception do begin
                if oViewer.Items.Remove( ll ) < 0 then
                  FreeObject( ll ) ;
                ll := nil ;
                err := err + #13 + e.Message ;
              end ;
            end ;
          end ;

        end ;
      end ;

      oViewer.Lock ;
      try
        oViewer.RecalcExtent ;
        oViewer.FullExtent   ;

        if ( centerPtg.X = 0 ) and ( centerPtg.Y = 0 ) then
        else
          oViewer.Center := centerPtg ;

        // Zoom not implemented yet
      finally
        oViewer.Unlock ;
      end ;
    end ;

  begin
    err := '' ;
    try

      cnt := 0 ;

      dir := GetFileDir( _path ) ;

      if not IsStringEmpty( _path ) then begin
        // close existing gis view
        oViewer.Close;

        // start read work file
        worfile := TGIS_BufferedFileStream.Create( _path, TGIS_StreamMode.Read ) ;
        try
          // first we are looking for Open Table section
          while not worfile.Eof do  begin
            {$IFDEF OXYGENE}
              if not assigned( sline ) then
                sline := '' ;
            {$ENDIF}
            if Pos( 'OPEN TABLE', UpperCase( sline ) ) > StringFirst - 1 then begin
              read_open_table ;
              break;
            end ;
            sline := worfile.ReadLine ;
          end ;

          // now we are looking for section with information about layers
          // to show
          while not worfile.Eof do begin
            if Pos( 'MAP FROM', UpperCase( sline ) ) > StringFirst - 1 then begin
              read_map_from ;
              break;
            end ;
            sline := worfile.ReadLine ;
          end ;

          // first section set map
          while not worfile.Eof do begin
            if Pos( 'SET MAP', UpperCase( sline ) ) > StringFirst - 1 then begin
               read_set_map;
               break;
            end ;
            sline := worfile.ReadLine ;
          end ;

          // second section set map with sub section Layer
          while not worfile.Eof do begin
            if Pos( 'SET MAP', UpperCase( sline ) ) > StringFirst - 1 then begin
              cnt := read_set_map_layer ;
              break;
            end ;
            sline := worfile.ReadLine ;
          end ;
        finally
          FreeObject( worfile ) ;
        end ;
      end ;

      load_project( cnt ) ;

    finally
      if not IsStringEmpty( err ) then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERMISSED ), err, 0 ) ;
    end ;
  end ;

  type
    T_IProj_Table = record
      id          : String ;
      filePath    : String ;
      description : String ;
      isVisible   : Boolean ;
      zoomMin     : Double ;
      zoomMax     : Double ;
    end ;

  procedure TGIS_ImportProject.importMapInfoX(
    const _path : String
  ) ;
  var
      sline       : String ;
      tkn         : TGIS_Tokenizer ;
      tknEx       : TGIS_Tokenizer ;
      fn          : TGIS_BufferedFileStream ;
      i, cnt      : Integer ;
      la          : TGIS_Layer ;
      dir, err    : String ;
      name        : String ;
      projection  : String ;
      center      : TGIS_Point ;
      mbr         : TGIS_Extent ;
      zoomLevel   : Double ;
      mapUnit     : Integer ;
      rotation    : Integer ;
      tables      : array of T_IProj_Table ;

      function parsePoint( const _str : String ) : TGIS_Point ;
      begin
        tknEx.Execute( _str, [','] ) ;
        if tknEx.Result.Count > 1 then begin
          Result := GisPoint( DotStrToFloat( tknEx.Result[ 0 ] ),
                              DotStrToFloat( tknEx.Result[ 1 ] )
                             ) ;
        end ;
      end ;

      procedure parseGEOSET( const _useLine : Boolean ) ;
      var
        ul : Boolean ;
      begin
        ul := _useLine ;
        while not fn.Eof do begin
          if not ul then
            sline := fn.ReadLine
          else
            ul := False ;

          tkn.Execute( sline, [ '\',' ', '=' ], True ) ;

          if tkn.Result.Count > 0 then begin
            if tkn.Result[ 0 ] = '\TABLE' then break ;
            if _useLine and (Pos('\TABLE',tkn.Result[ 0 ])=StringFirst) then break ;

            if tkn.Result[ 0 ] = '\GEOSET\NAME' then
              name := tkn.Result[ 1 ]
            else if tkn.Result[ 0 ] = '\GEOSET\PROJECTION' then
              projection := tkn.Result[ 1 ]
            else if tkn.Result[ 0 ] = '\GEOSET\CENTER' then
              center := parsePoint( tkn.Result[ 1 ] )
            else if tkn.Result[ 0 ] = '\GEOSET\MBR\LOWERLEFT' then begin
              with parsePoint( tkn.Result[ 1 ] ) do begin
                mbr.XMin := X ;
                mbr.YMin := Y ;
              end ;
            end
            else if tkn.Result[ 0 ] = '\GEOSET\MBR\UPPERRIGHT' then begin
              with parsePoint( tkn.Result[ 1 ] ) do begin
                mbr.XMax := X ;
                mbr.YMax := Y ;
              end ;
            end
            else if tkn.Result[ 0 ] = '\GEOSET\ZOOMLEVEL' then
              zoomLevel := DotStrToFloat( tkn.Result[ 1 ] )
            else if tkn.Result[ 0 ] = '\GEOSET\MAPUNIT' then
              mapUnit := StrToInt( tkn.Result[ 1 ] )
            else if tkn.Result[ 0 ] = '\GEOSET\ROTATION' then
              rotation := StrToInt( tkn.Result[ 1 ] )
          end ;
        end ;
      end ;

      procedure parseTABLE( const _useLine : Boolean ) ;
      var
        i1  : Integer ;
        p,t : String ;
        ul  : Boolean ;
      begin
        ul := _useLine ;
        i1 := 0  ;
        t  := '' ;
        while not fn.Eof do begin
          if not ul then
            sline := fn.ReadLine
          else
            ul := False ;

          tkn.Execute( sline, [ '\',' ', '=' ], True ) ;

          if tkn.Result.Count > 0 then begin
            tknEx.Execute( tkn.Result[ 0 ], [ '\' ] ) ;

            if tknEx.Result.Count > 0 then begin
              if tknEx.Result[ 0 ] <> 'TABLE' then exit ;

              p := tknEx.Result[ 1 ] ;
              if p <> t then begin
                inc( i1 ) ;
                t := p ;
                SetLength( tables, i1 ) ;
                tables[i1-1].id := p ;
              end  ;

              if tknEx.Result.Count > 2 then begin
                if tknEx.Result[ 2 ] = 'FILE' then
                tables[i1-1].filePath := tkn.Result[ 1 ]
              else if tknEx.Result[ 2 ] = 'DESCRIPTION' then
                tables[i1-1].description := tkn.Result[ 1 ]
              else if tknEx.Result[ 2 ] = 'ISVISIBLE' then
                tables[i1-1].isVisible := ( tkn.Result[ 1 ] = 'TRUE' ) or
                                          ( tkn.Result[ 1 ] = '1' )
              else if tknEx.Result[ 2 ] = 'ZOOM' then begin
                if tknEx.Result.Count > 3 then begin
                  if tknEx.Result[ 3 ] = 'MIN' then
                    tables[i1-1].zoomMin := DotStrToFloat( tkn.Result[ 1 ] )
                  else if tknEx.Result[ 3 ] = 'MAX' then
                    tables[i1-1].zoomMax := DotStrToFloat( tkn.Result[ 1 ] ) ;
                  end ;

                end ;
              end ;

            end ;
          end ;
        end ;
      end ;

      procedure load( const _path : String ) ;
      begin
        fn := TGIS_BufferedFileStream.Create( _path, TGIS_StreamMode.Read ) ;
        try
          while not fn.Eof do begin
            sline := fn.ReadLine ;

            tkn.Execute( sline, [ '\',' ', '=' ], True ) ;
            if tkn.Result.Count > 0 then begin
              if tkn.Result[ 0 ] = '\GEOSET' then
                parseGEOSET( False )
              else if Pos( '\GEOSET', tkn.Result[ 0 ] ) = StringFirst then
                parseGEOSET( True ) ;
              if tkn.Result[ 0 ] = '\TABLE'  then
                parseTABLE( False )
              else if Pos( '\TABLE', tkn.Result[ 0 ] ) = StringFirst then
                parseTABLE( True ) ;
            end ;
          end ;
        finally
          FreeObject( fn ) ;
        end ;
      end ;

  begin
    err := '' ;
    try
      dir := GetFileDir( _path ) ;

      if not IsStringEmpty( _path ) then begin
        // close existing gis view
        oViewer.Close;

        try
          tkn    := TGIS_Tokenizer.Create ;
          tknEx  := TGIS_Tokenizer.Create ;

          load( _path ) ;
          cnt := length( tables ) ;
          for i := cnt - 1 downto 0 do begin
            la := GisCreateLayer( tables[i].filePath,
                                  GetPathAbsolute( dir, tables[i].filePath )
                                ) ;
            if not assigned( la ) then continue ;

            try
              oViewer.Add( la ) ;

              if IsStringEmpty( tables[i].description ) then
                la.Caption := GetFileNameNoExt( tables[i].filePath )
              else
                la.Caption := tables[i].description ;
              la.Active  := tables[i].isVisible ;
            except
              on e : Exception do begin
                if oViewer.Items.Remove( la ) < 0 then
                  FreeObject( la ) ;
                la := nil ;
                err := err + #13 + e.Message ;
              end ;
            end ;

          end ;
          oViewer.Lock ;
          try
            oViewer.CS := TGIS_CSFactoryMapInfo.BuildCs( projection ) ;
            oViewer.FullExtent ;
            oViewer.VisibleExtent := mbr ;
          finally
            oViewer.Unlock ;
          end ;
        finally
          FreeObject( tkn    ) ;
          FreeObject( tknEx  ) ;
          tables := nil ;
        end ;
      end ;

    finally
      if not IsStringEmpty( err ) then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYERMISSED ), err, 0 ) ;
    end ;
  end ;

  procedure TGIS_ImportProject.importQgis(
    const _path : String
  ) ;
  var
    qgis : TGIS_ProjectQgis ;
  begin
    qgis := TGIS_ProjectQgis.Create( oViewer ) ;
    try
      qgis.Import( _path ) ;
    finally
      FreeObject( qgis ) ;
    end ;
  end ;

  {$IFNDEF ISLAND}
  procedure TGIS_ImportProject.importMapInfoSeamless(
    const _path : String
  ) ;
  var
    ltab      : TGIS_LayerTAB ;
    lp        : TGIS_LayerProject ;
    la        : TGIS_Layer ;
    {$IFDEF DCC}
      shp     : TGIS_Shape ;
    {$ENDIF}
    allpaths  : TStringList ;
    fpath     : String ;
    i         : Integer ;
  begin
    fpath := GetFilePath( _path ) ;

    ltab := TGIS_LayerTAB.Create ;
    try
      ltab.Path := _path ;
      ltab.Open ;

      if ltab.FindField( 'Table' ) = -1 then exit ;

      allpaths := TStringList.Create ;
      try
        for shp in ltab.Loop do
          allpaths.Add( fpath + Trim(VarToString(TGIS_Shape(shp).GetField( 'Table' ))) ) ;

        lp := TGIS_LayerProject.Create ;
        lp.Path := _path ;
        oViewer.Add( lp ) ;

        for i := 0 to allpaths.Count-1 do begin
          fpath := allpaths[i] ;

          if SafeFileExists( fpath ) then begin
            la := GisCreateLayer( fpath, fpath ) ;
            if assigned( la ) then begin
              try
                lp.Add( la ) ;
                if i = 0 then
                  lp.CS := la.CS ;
              except
                on e : Exception do begin
                  if oViewer.Items.Remove( la ) < 0 then
                    FreeObject( la ) ;
                  la := nil ;
                  continue ;
                end ;
              end ;
            end ;
          end ;
        end ;

        oViewer.CS := ltab.CS ;
        lp.RecalcProjectedExtent ;

        oViewer.FullExtent ;
      finally
        FreeObject( allpaths ) ;
      end ;
    finally
      FreeObject( ltab ) ;
    end ;
  end ;
  {$ENDIF}

  procedure TGIS_ImportProject.importZip(
    const _path   : String
  ) ;
  var
    tempPath : String ;
    absPath  : String ;
    files    : TArray<String> ;
    f        : Integer ;
    la       : TGIS_Layer ;
    supFiles : TStringList ;
  begin
    FTemporaryDir := '' ;
    tempPath := GetTempFileName ;
    try
      files := ExtractZipFiles( _path, tempPath ) ;
    except
      // if extracting zip failed, exit
      exit ;
    end ;

    FTemporaryDir := tempPath ;

    for f := 0 to length( files )-1 do begin
      absPath := GetPathAbsolute(tempPath, files[f]) ;
      if TGIS_ConfigFactory.IsProject( absPath ) then begin
        oViewer.Open( absPath ) ;
        exit ;
      end ;
    end ;

    oViewer.Lock ;
    try
      supFiles := TStringList.Create ;
      try
        supFiles.Text := GisSupportedFiles( [TGIS_FileType.All], false, true ) ;
        supFiles.Sort ;
        for f := 0 to length( files )-1 do begin
          absPath := GetPathAbsolute( tempPath, files[f] ) ;
          if supFiles.IndexOfName( GetFileExt( files[f] ) ) = -1 then continue ;

          la := GisCreateLayer( GetFileName( files[f] ), absPath ) ;
          try
            if assigned( la ) then begin
              oViewer.Add( la ) ;
            end ;
          except
            on e : Exception do begin
              if oViewer.Items.Remove( la ) < 0 then
                FreeObject( la ) ;
              la := nil ;
              continue ;
            end ;
          end ;
        end ;
      finally
        FreeObject( supFiles ) ;
      end ;
    finally
      oViewer.FullExtent ;
      oViewer.Unlock ;
    end ;
  end ;

  function  TGIS_ImportProject.IsSupported(
    const _path : String
  ) : Boolean ;
  var
    ext : String  ;
    {$IFNDEF ISLAND}
    tab : TGIS_FileTAB ;
    {$ENDIF}
  begin
    ext := UpperCase( GetFileExt( _path ) ) ;

    Result := False ;
    if      ext = '.WOR' then Result := True
    else if ext = '.APR' then Result := True
    else if ext = '.AEP' then Result := True
    else if ext = '.GST' then Result := True
    else if ext = '.QGS' then Result := True
    else if ext = '.QGZ' then Result := True
    {$IFNDEF ISLAND}
    else if ext = '.TAB' then begin
      tab := TGIS_FileTAB.Create( _path, False ) ;
      try
        Result := tab.Prerecognize = 5 ;
      finally
        FreeObject( tab ) ;
      end ;
    end
    {$ENDIF}
    else if ext = '.ZIP' then Result := True
  end ;

  procedure TGIS_ImportProject.Import(
    const _path : String
  ) ;
  var
    ext : String  ;
  begin
    ext := UpperCase( GetFileExt( _path ) ) ;
    if not SafeFileExists( _path ) then begin
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ), _path, 0 ) ;
    end ;

    if      ext = '.WOR' then importMapInfo_6x( _path )
    else if ext = '.APR' then importArcView_3x( _path )
    else if ext = '.AEP' then importFromArcExplorer_2x( _path )
    else if ext = '.GST' then importMapInfoX( _path )
    else if ext = '.QGS' then importQgis( _path )
    else if ext = '.QGZ' then importQgis( _path )
    {$IFNDEF ISLAND}
    else if ext = '.TAB' then importMapInfoSeamless( _path )
    {$ENDIF}
    else if ext = '.ZIP' then importZip( _path )
    else
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEEXTENSION ), ext, 0 ) ;
  end ;

{==================================== END =====================================}
end.
