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
  Encapsulation of the E00 file access.

  Supported formats: compressed and uncompressed:

  This decompression algorithms are inspired by E00COMPR:

  Copyright (c) 1998, 1999, Daniel Morissette
  All rights reserved. This software may be copied
  or reproduced, in all or in part, without the prior written
  consent of its author, Daniel Morissette
  (danmo@videotron.ca). However, any material copied or
  reproduced must bear the original copyright notice (above),
  this original paragraph, and the original disclaimer (below).

  The entire risk as to the results and performance
  of the software, supporting text and other information
  contained in this file (collectively called the "Software")
  is with the user. Although considerable efforts have been
  used in preparing the Software, the author does not warrant
  the accuracy or completeness of the Software. In no event
  will the author be liable for damages, including loss of
  profits or consequential damages, arising out of the use of
  the Software.

  Sublicensing of this unit is a subject of TatukGIS Developer
  Kernel License
}

{$IFDEF DCC}
  unit GisLayerE00 ;
  {$HPPEMIT '#pragma link "GisLayerE00"'}
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

{$INCLUDE GisInclude.inc}

interface

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Variants,
    System.Generics.Collections,
    System.Generics.Defaults,
    GisTypes,
    GisStreams,

    GisLayerVector ;
{$ENDIF}
{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerE00 = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  /// <summary>
  ///   Layer that can read E00 file.
  /// </summary>
  TGIS_LayerE00 = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private

        /// <summary>
        ///   Internal file name.
        /// </summary>
        sName : String ;

        /// <summary>
        ///   Shape file.
        /// </summary>
        exxStream : TGIS_BufferedFileStream ;

        /// <summary>
        ///   Translated line.
        /// </summary>
        lineNo : Integer ;

        /// <summary>
        ///   Read line.
        /// </summary>
        lineStr : String ;

        /// <summary>
        ///   Buffer for reading.
        /// </summary>
        exxBuffer : String ;

        /// <summary>
        ///   Reading buffer size.
        /// </summary>
        exxBufferSize : Integer ;

        /// <summary>
        ///   Reading buffer position.
        /// </summary>
        exxBufferPos : Integer ;

        /// <summary>
        ///   Eof value.
        /// </summary>
        exxStreamEof : Boolean ;

        /// <summary>
        ///   True, If stream was compressed.
        /// </summary>
        exxStreamCompressed : Boolean ;

        /// <summary>
        ///   Field width array.
        /// </summary>
        widthList : TList<Integer> ;

        /// <summary>
        ///   Arc items list.
        /// </summary>
        arcItems : TGIS_ObjectList ;

        /// <summary>
        ///   Prescan mode?
        /// </summary>
        isPrescan : Boolean ;

        /// <summary>
        ///   Was prescan used?
        /// </summary>
        wasPrescan : Boolean ;

        /// <summary>
        ///   Shapes types existing within the file.
        /// </summary>
        existingShapes : TGIS_ShapeTypes ;

    private // various private routines

      /// <summary>
      ///   Read line form the E00 file. Can read UNIX and DOS files.
      /// </summary>
      procedure exxReadLine  ;

      /// <summary>
      ///   Finish line form the E00 file. Skip all unimportant elements.
      /// </summary>
      procedure exxFinishLine  ;

      /// <summary>
      ///   Read a single token from the E00 file.
      /// </summary>
      /// <param name="_width">
      ///   width of the token
      /// </param>
      function  exxGetToken  ( const _width   : Integer
                             ) : String ;

      /// <summary>
      ///   Decode ARC section.
      /// </summary>
      /// <param name="_param">
      ///   string with the type of numbers (2-single, 3-double)
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doArc        ( const _param   : String
                             ) ;

      /// <summary>
      ///   Decode ARC element.
      /// </summary>
      /// <param name="_mode">
      ///   type of numbers (2-single, 3-double)
      /// </param>
      /// <param name="_count">
      ///   number of elements
      /// </param>
      procedure doArcElement ( const _mode    : Char ;
                               const _count   : Integer
                             ) ;

      /// <summary>
      ///   Decode LAB section.
      /// </summary>
      /// <param name="_param">
      ///   string with type of numbers (2-single, 3-double)
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doLab        ( const _param   : String
                             ) ;

      /// <summary>
      ///   Decode LAB element.
      /// </summary>
      /// <param name="_mode">
      ///   type of numbers (2-single, 3-double)
      /// </param>
      procedure doLabElement ( const _mode    : Char
                             ) ;

      /// <summary>
      ///   Decode PAL section.
      /// </summary>
      /// <param name="_param">
      ///   string with type of numbers (2-single, 3-double)
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doPal        ( const _param   : String
                             ) ;

      /// <summary>
      ///   Create PAL element.
      /// </summary>
      /// <param name="_count">
      ///   number of labels
      /// </param>
      /// <param name="_first">
      ///   True, if first element of the polygon
      /// </param>
      procedure doPalElement ( const _count   : Integer ;
                               const _first   : Boolean
                             ) ;

      /// <summary>
      ///   Decode PRJ section.
      /// </summary>
      /// <param name="_param">
      ///   unused
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_TXTFILESTRUCT
      /// </exception>
      procedure doPrj        ( const _param   : String
        ) ;

      /// <summary>
      ///   Create ARC attribute element.
      /// </summary>
      /// <param name="_type">
      ///   type of file (.AAT, .PAT)
      /// </param>
      /// <param name="_flag">
      ///   file flag (must be 'XX')
      /// </param>
      /// <param name="_items1">
      ///   number of items
      /// </param>
      /// <param name="_items2">
      ///   repeated number of items
      /// </param>
      /// <param name="_length">
      ///   record length
      /// </param>
      /// <param name="_records">
      ///   number of records
      /// </param>
      procedure doAttribute  ( const _type    : String ;
                               const _flag    : String ;
                               const _items1  : String ;
                               const _items2  : String ;
                               const _length  : String ;
                               const _records : String
                             ) ;

      /// <summary>
      ///   Decode attribute element.
      /// </summary>
      function  doAttributeElement
                             ( const _no      : Integer ;
                               const _layer   : TGIS_LayerVector ;
                               const _shp     : TGIS_Shape
                             ) : Boolean ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // for internal use of TGIS_Viewer

         /// <inheritdoc/>
         procedure setUp     ; override;
    protected
      // destructor

         /// <inheritdoc/>
         procedure doDestroy ; override;
    public
      // constructors

         /// <inheritdoc/>
         constructor Create ; override;

         /// <inheritdoc/>
         function  PreRecognize     ( const _path     : String ;
                                        var _new_path : String
                                     ) : Boolean ; override;
    public

        /// <summary>
        ///   Accepted shape types.
        /// </summary>
        ShapeType : TGIS_ShapeType  ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisFunctions,
    GisInternals,
    GisClasses,
    GisResource,
    GisRtl,
    GisRegistredLayers;
{$ENDIF}

const
  // E00 file strings
     E00_EXP          = 'EXP'  ;

     E00_ARC          = 'ARC'  ;
     E00_LAB          = 'LAB'  ;
     E00_PAL          = 'PAL'  ;
     E00_PRJ          = 'PRJ'  ;
     E00_EOP          = 'EOP'  ;

     E00_AAT          = '.AAT' ;
     E00_PAT          = '.PAT' ;

     E00_DATA_DEFAULT = '18000101' ;
  // data size
     E00_FLOAT_SIZE    = 14  ;
     E00_DOUBLE_SIZE   = 21  ;
     E00_INTEGER_SIZE  = 10  ;
     E00_SMALLINT_SIZE =  6  ;
     E00_DATE_SIZE     =  8  ;
     E00_FLOAT_FORMAT  = '2' ;
     E00_DOUBLE_FORMAT = '3' ;

//=============================================================================
// TGIS_LayerE00
//=============================================================================

  constructor TGIS_LayerE00.Create ;
  begin
    inherited ;
    FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                             TGIS_LayerSubType.InMemory
                            ] ;

    arcItems    := nil    ;
    ShapeType   := TGIS_ShapeType.Unknown ;
  end ;

  procedure TGIS_LayerE00.doDestroy ;
  begin
    inherited ;
  end ;

  procedure TGIS_LayerE00.exxReadLine ;
  var
    c       : Char    ;
    abrt    : Boolean ;
    i       : Integer ;
    state   : Integer ;
    cnt     : Integer ;
    num     : Integer ;
    decimal : Integer ;
    oddnum  : Boolean ;
    ex      : Integer ;
    stmp    : String  ;
  begin
    lineStr := '' ;

    num     := 0 ;
    cnt     := 0 ;
    decimal := 0 ;
    ex      := 0 ;
    oddnum  := False ;

    state   := 0 ;

    try

      try
        if exxStreamCompressed then begin
          while not exxStreamEof do begin
            {$IFNDEF OXYGENE}
              if ( exxBufferPos > exxBufferSize  ) or ( exxBufferSize < 0 ) then
            {$ELSE}
              if ( exxBufferPos >= exxBufferSize ) or ( exxBufferSize < 0 ) then
            {$ENDIF}
            begin
              if exxStream.Position < exxStream.Size then begin
                exxBuffer := exxStream.ReadLine + #10;
                exxBufferPos  := StringFirst ;
                exxBufferSize := length( exxBuffer ) ;
              end
              else begin
                exxStreamEof := True ;
                exit ;
              end ;
            end ;

            {$IFNDEF OXYGENE}
              while exxBufferPos <= exxBufferSize do begin
            {$ELSE}
              while exxBufferPos < exxBufferSize  do begin
            {$ENDIF}
              try
                c := exxBuffer[exxBufferPos] ;
                if c < ' ' then continue ;

                case state of
                  0 :  begin // uncompresssed character
                         case c of
                           '~' : state := 1 ;
                           else  lineStr := lineStr + c ;
                         end ;
                       end ;
                  1 :  begin // after copression '~' sign
                         case c of
                           ' ' : state := 2 ;
                           '}' : begin
                                   inc( lineNo ) ;
                                   if lineNo mod 100 = 1 then
                                   if assigned( Viewer ) then begin
                                     abrt := False ;
                                     if isPrescan then
                                       abrt := RaiseBusyShake( Self,
                                                         exxStream.Position,
                                                         exxStream.Size     +
                                                         exxStream.Size
                                                       )
                                     else if wasPrescan then
                                       abrt := RaiseBusyShake( Self,
                                                         exxStream.Position +
                                                         exxStream.Size,
                                                         exxStream.Size     +
                                                         exxStream.Size
                                                       )
                                     else
                                       abrt := RaiseBusyShake( Self,
                                                         exxStream.Position,
                                                         exxStream.Size
                                                       )
                                   end ;
                                   exit ; // end of line
                                 end ;
                           '~' : begin
                                   lineStr := lineStr + c ;
                                   state := 0 ;
                                 end ;
                           '-' : begin
                                    lineStr := lineStr + c ;
                                    state := 0 ;
                                 end ;
                           else  begin
                                   if ( c >= '!' ) and ( c <= 'z' ) then begin
                                     num := ord( c ) - ord( '!' ) ;
                                     decimal   := num mod 15 ;
                                     oddnum := ( num div 45 ) <> 0 ;
                                     num := num div 15 ;
                                     if      ( num mod 3 ) = 1 then ex :=  1
                                     else if ( num mod 3 ) = 2 then ex := -1
                                     else                           ex :=  0 ;
                                     cnt   := 0 ;
                                     state := 3 ;
                                   end
                                   else
                                     assert( False ) ;
                                 end ;
                         end ;
                       end ;
                  2 :  begin // compressed spaces
                         for i := 1 to  ord(c) - ord ( ' ' ) do
                           lineStr := lineStr + ' ' ;
                         state := 0 ;
                       end ;
                  3 :  begin // number
                         if ( c <> ' ' ) and ( c <> '~' ) then begin
                           num := ord( c ) - ord( '!' ) ;
                           if num = 92 then
                             state := 4
                           else begin
                             lineStr := lineStr + Char( ord( '0' ) + num div 10 ) ;
                             inc( cnt ) ;
                             if cnt = decimal then
                               lineStr := lineStr + '.' ;
                             inc( cnt ) ;
                             lineStr := lineStr + Char( ord( '0' ) + num mod 10 ) ;
                             if cnt = decimal then
                               lineStr := lineStr + '.' ;
                           end ;
                         end
                         else begin
                           if oddnum then begin
                              SetLengthStr( lineStr, length( lineStr ) - 1 ) ;
                           end ;
                           if ex <> 0 then begin
                             stmp := Copy( lineStr, length( lineStr ) - 1, 2 ) ;
                              SetLengthStr( lineStr, length( lineStr ) - 2 ) ;
                             if ex > 0 then
                               lineStr := lineStr + 'E+' + stmp
                             else
                               lineStr := lineStr + 'E-' + stmp ;
                           end ;

                           case c of
                             '~': state := 5 ;
                             ' ': begin
                                    lineStr := lineStr + ' ' ;
                                    state := 0 ;
                                  end ;
                             else Abort ;
                           end ;
                         end ;
                       end ;
                  4 :  begin // multicharacter compressed number
                         num := num + ord( c ) - ord( '!' ) ;
                             lineStr := lineStr + Char( ord( '0' ) + num div 10 ) ;
                             inc( cnt ) ;
                             if cnt = decimal then
                               lineStr := lineStr + '.' ;
                             inc( cnt ) ;
                             lineStr := lineStr + Char( ord( '0' ) + num mod 10 ) ;
                             if cnt = decimal then
                               lineStr := lineStr + '.' ;

                         state := 3 ;
                       end ;
                  5 :  begin // end of number
                         case c of
                           ' ' : state := 2 ;
                           '}' : begin
                                   inc( lineNo ) ;
                                   exit ;
                                 end ;
                           else  begin
                                   lineStr := lineStr + c ;
                                   state := 0 ;
                                 end ;
                         end ;
                       end ;
                  else Abort ;
                end ;

              finally
                inc( exxBufferPos ) ;
              end ;
            end ;
          end ;
        end
        else begin
          if exxStream.Position < exxStream.Size then begin
            lineStr := exxStream.ReadLine ;
            inc( lineNo ) ;
            if lineNo mod 100 = 1 then
            if assigned( Viewer ) then begin
              abrt := False ;
              if isPrescan then
                abrt := RaiseBusyShake( Self,
                                  exxStream.Position,
                                  exxStream.Size     +
                                  exxStream.Size
                                )
              else if wasPrescan then
                abrt := RaiseBusyShake( Self,
                                  exxStream.Position +
                                  exxStream.Size,
                                  exxStream.Size     +
                                  exxStream.Size
                                )
              else
                abrt := RaiseBusyShake( Self,
                                  exxStream.Position,
                                  exxStream.Size
                                )
            end ;
          end
          else
            exxStreamEof := True ;

        end ;

      finally
        for i:= length( lineStr ) to 79 do
          lineStr := lineStr + ' ' ;
      end ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), Path, lineNo ) ;
    end ;
  end ;

  procedure TGIS_LayerE00.exxFinishLine ;
  begin
    lineStr := '' ;
  end ;

  function TGIS_LayerE00.exxGetToken( const _width : Integer ) : String ;
  var
    l : Integer ;
  begin
    Result := '' ;
    l := _width ;
    while l > 0 do begin
      if IsStringEmpty( lineStr ) then
        exxReadLine ;
      Result := Result + lineStr[StringFirst] ;
      {$IFDEF OXYGENE}
        lineStr := lineStr.Substring( 1 ) ;
      {$ELSE}
        lineStr := Copy( lineStr, 2, 255 ) ;
      {$ENDIF}
      dec( l ) ;
    end ;
   end ;

  procedure TGIS_LayerE00.doArc( const _param : String ) ;
  var
    hash  : Integer ;
    count : Integer ;
  begin
    existingShapes := GisAddShapeType( existingShapes, TGIS_ShapeType.Arc ) ;
    if isPrescan then exit ;

    if not ( ShapeType in [TGIS_ShapeType.Arc, TGIS_ShapeType.Polygon] ) then exit ;

    try
      if ShapeType in [TGIS_ShapeType.Polygon] then
        arcItems := TGIS_ObjectList.Create ;

      if length(_param) <> 1 then Abort ;
      while not exxStreamEof do begin
        exxReadLine ;

        hash := StrToInt( exxGetToken( E00_INTEGER_SIZE ) ) ;
        exxGetToken( E00_INTEGER_SIZE ) ;
        exxGetToken( E00_INTEGER_SIZE ) ;
        exxGetToken( E00_INTEGER_SIZE ) ;
        exxGetToken( E00_INTEGER_SIZE ) ;
        exxGetToken( E00_INTEGER_SIZE ) ;
        count := StrToInt( exxGetToken( E00_INTEGER_SIZE ) ) ;
        if hash = -1 then exit ;

        doArcElement( _param[StringFirst], count ) ;
      end ;
    except
      raise EGIS_Exception.Create( GIS_RS_ERR_TXTFILESTRUCT,Path,lineNo ) ;
    end ;

  end ;

  procedure TGIS_LayerE00.doArcElement( const _mode : Char ;
                                        const _count : Integer
                                      ) ;
  var
    cnt   : Integer    ;
    ptg   : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ;
    shp   : TGIS_Shape ;
    size  : Integer    ;
  begin

    case _mode of
      E00_FLOAT_FORMAT  : size := E00_FLOAT_SIZE ;
      E00_DOUBLE_FORMAT : size := E00_DOUBLE_SIZE ;
      else                begin
                            size := 0 ; // just to avoid hint
                            Abort ;
                          end ;
    end ;

    if assigned( arcItems ) then begin
      shp := TGIS_ShapeArc.Create( nil, nil, False, 0, self ) ;
      arcItems.Add( shp ) ;
    end
    else
      shp := CreateShape( TGIS_ShapeType.Arc ) ;

    shp.Lock( TGIS_Lock.Extent ) ;
    shp.AddPart ;

    try
      cnt := 0 ;
      exxReadLine ;
      while cnt < _count do begin

        ptg.X := DotStrToFloat( exxGetToken( size ) ) ;
        ptg.Y := DotStrToFloat( exxGetToken( size ) ) ;
        shp.AddPoint( ptg ) ;
        inc( cnt ) ;

        if _mode = E00_FLOAT_FORMAT then begin
          if cnt mod 2 = 0 then
            exxFinishLine ;
        end
        else
          exxFinishLine ;
      end ;
    finally
      shp.Unlock ;
      if Items.Count = 1 then Extent := shp.Extent ;
    end ;
  end ;

  procedure TGIS_LayerE00.doLab( const _param : String ) ;
  var
    hash : Integer ;
  begin
    existingShapes := GisAddShapeType( existingShapes, TGIS_ShapeType.Point ) ;
    if isPrescan then exit ;

    if not( ShapeType in [TGIS_ShapeType.Point, TGIS_ShapeType.MultiPoint] ) then exit ;

    try
      if length(_param) <> 1 then Abort ;

      while not exxStreamEof do begin
        exxReadLine ;
        hash := StrToInt( exxGetToken( E00_INTEGER_SIZE ) ) ;
        if hash = -1 then exit ;

        doLabElement( _param[StringFirst] ) ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ),Path,lineNo ) ;
    end ;

  end ;

  procedure TGIS_LayerE00.doLabElement( const _mode : Char ) ;
  var
    ptg  : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ;
    shp  : TGIS_Shape ;
    size : Integer    ;
  begin

    case _mode of
      E00_FLOAT_FORMAT  : size := E00_FLOAT_SIZE ;
      E00_DOUBLE_FORMAT : size := E00_DOUBLE_SIZE ;
      else                begin
                            size := 0 ;
                            Abort ;
                          end ;
    end ;

    shp := CreateShape( TGIS_ShapeType.Point ) ;
    shp.Lock( TGIS_Lock.Extent ) ;

    shp.AddPart ;

    try
      shp.Tag := StrToInt( exxGetToken( E00_INTEGER_SIZE ) ) ;
      ptg.X := DotStrToFloat( exxGetToken( size ) ) ;
      ptg.Y := DotStrToFloat( exxGetToken( size ) ) ;
      shp.AddPoint( ptg ) ;

      // skip extent
      exxGetToken( size ) ;
      exxGetToken( size ) ;
      if _mode = E00_DOUBLE_FORMAT then exxFinishLine ;

      exxGetToken( size ) ;
      exxGetToken( size ) ;
      exxFinishLine ;

    finally
      shp.Unlock ;
      if Items.Count = 1 then Extent := shp.Extent ;
    end ;
  end ;

  procedure TGIS_LayerE00.doPal( const _param : String ) ;
  var
    size    : Integer ;
    arc_cnt : Integer ;
    first   : Boolean ;
  begin
    existingShapes := GisAddShapeType( existingShapes, TGIS_ShapeType.Polygon ) ;
    if isPrescan then exit ;

    if not( ShapeType in [TGIS_ShapeType.Polygon] ) then exit ;

    try
      case _param[StringFirst] of
        E00_FLOAT_FORMAT  : size := E00_FLOAT_SIZE ;
        E00_DOUBLE_FORMAT : size := E00_DOUBLE_SIZE ;
        else                begin
                              size := 0 ; // just to avoid hint
                              Abort ;
                            end ;
      end ;

      if not assigned(arcItems ) then Abort ;
      first := True ;
      while not exxStreamEof do begin
        exxReadLine ;

        arc_cnt := StrToInt( exxGetToken( E00_INTEGER_SIZE ) ) ;

        // skip extent
        exxGetToken( size ) ;
        exxGetToken( size ) ;
        if _param[StringFirst] = E00_DOUBLE_FORMAT then exxFinishLine ;

        exxGetToken( size ) ;
        exxGetToken( size ) ;
        exxFinishLine ;

        if arc_cnt = -1 then break ;

        doPalElement( arc_cnt, first ) ;
        first := False ;
      end ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), Path, lineNo ) ;
    end ;

  end ;

  procedure TGIS_LayerE00.doPalElement( const _count : Integer ;
                                        const _first : Boolean
                                      ) ;
  var
    i        : Integer    ;
    cnt      : Integer    ;
    shp      : TGIS_Shape ;
    shp_id   : Integer    ;
    tmp_shp  : TGIS_Shape ;
    fpoint   : TGIS_Point ;
    lpoint   : TGIS_Point ;
    fseg     : Boolean    ; // True, if first segment
  begin
    if not _first then begin
                         shp := CreateShape( TGIS_ShapeType.Polygon ) ;
                         shp.Lock( TGIS_Lock.Extent ) ;
                         shp.AddPart ;
                         fseg := True ;
                       end
                  else begin
                         shp  := nil ;
                         fseg := False ;
                       end ;

    try
      for cnt := 1 to _count do begin
        shp_id := StrToInt( exxGetToken( E00_INTEGER_SIZE ) ) ;
        exxGetToken( E00_INTEGER_SIZE ) ;
        exxGetToken( E00_INTEGER_SIZE ) ;
        if cnt mod 2 = 0 then exxFinishLine ;

        if _first then continue ;
        if not assigned( shp ) then continue ;

        if shp_id = 0 then begin
          // in the event of an unclosed object, prepare an arc polygon
          if not GisIsSamePoint( fpoint, lpoint ) then begin
            for i := shp.GetPartSize(shp.GetNumParts-1) - 1 downto 0 do
              shp.AddPoint( shp.GetPoint( shp.GetNumParts-1, i ) ) ;
          end ;

          // next part
          shp.AddPart ;
          fseg := True ;
          continue ;
        end ;

        tmp_shp := TGIS_Shape( arcItems[ Abs(shp_id) - 1 ] ) ;

        if shp_id > 0 then begin
          // is a body
          if fseg then fpoint := tmp_shp.GetPoint( 0, 0  ) ;
          for i := 0 to tmp_shp.GetPartSize(0) - 1 do
            shp.AddPoint( tmp_shp.GetPoint( 0, i ) ) ;
          lpoint := tmp_shp.GetPoint( 0, tmp_shp.GetPartSize(0) -1  ) ;
        end
        else begin
          // is a hole
          if fseg then fpoint := tmp_shp.GetPoint( 0, tmp_shp.GetPartSize(0) -1  ) ;
          for i := tmp_shp.GetPartSize(0) - 1 downto 0 do
            shp.AddPoint( tmp_shp.GetPoint( 0, i ) ) ;
          lpoint := tmp_shp.GetPoint( 0, 0 )
        end ;

        fseg := False ;

      end ;

      // in the event of an unclosed object, prepare an arc polygon
      if assigned( shp ) then
        if not GisIsSamePoint( fpoint, lpoint ) then begin
          for i := shp.GetPartSize(shp.GetNumParts-1) - 1 downto 0 do
            shp.AddPoint( shp.GetPoint( shp.GetNumParts-1, i ) ) ;
        end ;

    finally
      if assigned( shp ) then shp.Unlock ;
      if Items.Count = 1 then Extent := shp.Extent ;
    end ;

  end ;

  procedure TGIS_LayerE00.doPrj( const _param : String ) ;
  begin
    try
      while not exxStreamEof do begin
        exxReadLine ;
        if CompareText( TrimRight( lineStr ), E00_EOP ) = 0 then break  ;
      end ;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), Path, lineNo ) ;
    end ;
  end ;

  procedure TGIS_LayerE00.doAttribute( const _type    : String ;
                                       const _flag    : String ;
                                       const _items1  : String ;
                                       const _items2  : String ;
                                       const _length  : String ;
                                       const _records : String
                                     ) ;
  var
    cnt        : Integer ;
    fwidth     : Integer ;
    fdecimal   : Integer ;
    ftype      : Integer ;
    findex     : Integer ;
    items_cnt1 : Integer ;
    items_cnt2 : Integer ;
    first      : Boolean ;
    layer      : TGIS_LayerVector ;
    ignore     : Boolean ;
    sflag      : String  ;
    sitems1    : String  ;
    sitems2    : String  ;
    slength    : String  ;
    srecords   : String  ;
    done       : Boolean ;
    shp        : TGIS_Shape ;
  begin
    if _flag <> 'XX' then begin
      srecords := _length  ;
      slength  := _items2  ;
      sitems2  := _items1  ;
      sitems1  := _items1  ;
      sflag    := ''       ;
    end
    else begin
      srecords := _records ;
      slength  := _length  ;
      sitems2  := _items2  ;
      sitems1  := _items1  ;
      sflag    := _flag    ;
    end ;

    ignore := False ;

    if ( ShapeType in [TGIS_ShapeType.Arc] )  and
       ( _type <> E00_AAT )
    then ignore := True ;

    if ( ShapeType in [ TGIS_ShapeType.Point,
                        TGIS_ShapeType.MultiPoint,
                        TGIS_ShapeType.Polygon
                      ]
       ) and
       ( _type <> E00_PAT  )
    then ignore := True ;

    if Items.Count <= 0
    then ignore := True ;

    try

      if not ignore then
        layer := Self
      else
        layer := TGIS_LayerVector.Create ;

      try
        with layer do begin
          cnt := 0 ;
          items_cnt2 := StrToInt( sitems2 ) ;

          widthList.Clear ;

          first := ShapeType in [TGIS_ShapeType.Polygon] ;
          while cnt < items_cnt2 do begin
            exxReadLine ;

            if cnt < items_cnt2 then begin

              fname     := Trim(     Copy( lineStr, StringFirst     , 16 ) ) ;
              fwidth    := StrToInt( Copy( lineStr, StringFirst + 16, 3  ) ) ;
              ftype     := StrToInt( Copy( lineStr, StringFirst + 34, 3  ) ) ;
              findex    := StrToInt( Copy( lineStr, StringFirst + 65, 4  ) ) ;
              if Copy( lineStr, StringFirst + 32, 1 ) <> '-' then
                fdecimal := StrToInt( Copy( lineStr, StringFirst + 33, 1 ) )
              else
                fdecimal := 0 ;

              if findex > 0 then begin
                case ftype of
                  10   : begin
                           AddField( fname, TGIS_FieldType.Date,
                                     0, 0
                                   ) ;
                            widthList.Add( E00_DATE_SIZE ) ;
                           // minus because it can be shorter of the end of
                           // the string
                         end ;
                  20   : begin
                           AddField( fname, TGIS_FieldType.String,
                                     fwidth, 0 ) ;
                            widthList.Add( fwidth ) ;
                           // minus because it can be shorter of the end of
                           // the string
                         end ;
                  30   : begin
                           AddField( fname, TGIS_FieldType.Number,
                                     fwidth, 0
                                   ) ;
                            widthList.Add( fwidth ) ;
                         end ;
                  40   : begin
                           AddField( fname, TGIS_FieldType.Float,
                                     0, 0
                                   ) ;
                            widthList.Add( E00_FLOAT_SIZE ) ;
                         end ;
                  50   : begin
                           AddField( fname, TGIS_FieldType.Number,
                                     E00_INTEGER_SIZE + 1, 0 ) ;

                           case fwidth of
                             2 :  widthList.Add( E00_SMALLINT_SIZE ) ;
                             4 :  widthList.Add( E00_INTEGER_SIZE  + 1 ) ;
                             else Abort ;
                           end ;
                           // I don't know why now the integer value must
                           // be 10 digit + sign. In all other places, it is
                           // nine digits + sign
                         end ;
                  60   : begin
                           AddField( fname, TGIS_FieldType.Number,
                                     fwidth, fdecimal ) ;

                           case fwidth of
                             4 :  widthList.Add( E00_FLOAT_SIZE ) ;
                             8 :  widthList.Add( E00_DOUBLE_SIZE + 3 ) ;
                             else Abort ;
                           end ;
                         end ;
                  else   Abort ;
                end ;
              end;

            end ;

            inc( cnt ) ;
          end ;

          if layer = Self  then begin
            // real layer
            cnt := 0 ;
            items_cnt1 := StrToInt( srecords ) ;

            shp := FindFirst( GisWholeWorld, '' ) ;

            while ( cnt < items_cnt1 ) and assigned( shp ) do begin

              inc( cnt ) ;

              if shp.Tag < cnt then
                done := doAttributeElement( cnt, layer, shp )
              else
                done := False ;

              if first then begin
                first := False ;
                continue ;
              end ;

              if done then
                shp := FindNext ;
            end ;
          end
          else begin
            // temporary layer
            cnt := 0 ;
            items_cnt1 := StrToInt( srecords ) ;
            while ( cnt < items_cnt1 ) do begin
              doAttributeElement( cnt, layer, nil )  ;
              inc( cnt ) ;
            end ;
          end ;

        end ;

      finally
        if layer <> Self then
          FreeObject( layer ) ;
      end ;

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), Path, lineNo ) ;
    end ;

  end ;

  function TGIS_LayerE00.doAttributeElement( const _no    : Integer ;
                                             const _layer : TGIS_LayerVector ;
                                             const _shp   : TGIS_Shape
                                           ) : Boolean ;
  var
    fld_no : Integer ;
    tmp    : Variant ;
    fld    : TGIS_FieldInfo ;

  begin
    Result := False ;

    fld_no := 0 ;

    with _layer do begin
      exxReadLine ;
      while fld_no < Fields.Count do begin

        fld := FieldInfo( fld_no ) ;

        tmp := exxGetToken( widthList[ fld_no ] ) ;

        case fld.FieldType of
          TGIS_FieldType.Number :
                    try
                      if IsStringEmpty( Trim( VarToString( tmp ) ) ) then
                        tmp := NullVar
                      else
                        tmp := Format( '%*.*f', [ fld.NewWidth, fld.NewDecimal,
                                                  DotStrToFloat( VarToString( tmp ) )
                                                ]

                                     ) ;
                    except
                      tmp := NullVar ;
                    end ;
          TGIS_FieldType.Date :
                    try
                      if      length( Trim( VarToString( tmp ) ) ) < E00_DATE_SIZE then
                              tmp := E00_DATA_DEFAULT
                      else if VarToString( tmp ) = '00000000' then
                              tmp := E00_DATA_DEFAULT ;
                      tmp := Trim( Copy( VarToString( tmp ), StringFirst     , 4 ) ) + '-' +
                             Trim( Copy( VarToString( tmp ), StringFirst + 4 , 2 ) ) + '-' +
                             Trim( Copy( VarToString( tmp ), StringFirst + 6 , 2 ) ) ;
                    except
                      tmp := NullVar ;
                    end ;
          TGIS_FieldType.String :
                    begin
                      tmp := TrimRight( VarToString( tmp ) ) ;
                    end ;
        end ;

        inc( fld_no ) ;

        // ignore temporary layer
        if _layer <> self then
          continue ;

        // ignore if not matching Tag - to match LAB within PAL
        if ( _shp.Tag > 0 ) and ( _shp.Tag <> _no ) then
          continue ;

        _shp.SetField( fld.NewName, tmp ) ;

        Result := True ;
      end ;
    end ;

  end ;

  procedure TGIS_LayerE00.setUp ;
  var
    par0  : String  ;
    par1  : String  ;
    par2  : String  ;
    par3  : String  ;
    par4  : String  ;
    par5  : String  ;
    par6  : String  ;
    tkn   : TGIS_Tokenizer ;
    stmp  : String  ;

    function _t( const _val : String ) : Boolean ;
    begin
      Result := par0 = _val ;
    end ;

    function _p( const _val : String ) : Boolean ;
    begin
      Result := par0 = sName + _val ;
    end ;

    procedure tokenize ;
    begin
      par0 := '' ;
      par1 := '' ;
      par2 := '' ;
      par3 := '' ;
      par4 := '' ;
      par5 := '' ;
      par6 := '' ;

      if ( lineStr > '' ) and ( lineStr[StringFirst] = ' ' ) then exit ;

      tkn.ExecuteEx( lineStr, ' ' ) ;

      if tkn.Result.Count > 0 then par0 := tkn.Result[0] ;
      if tkn.Result.Count > 1 then par1 := tkn.Result[1] ;
      if tkn.Result.Count > 2 then par2 := tkn.Result[2] ;
      if tkn.Result.Count > 3 then par3 := tkn.Result[3] ;
      if tkn.Result.Count > 4 then par4 := tkn.Result[4] ;
      if tkn.Result.Count > 5 then par5 := tkn.Result[5] ;
      if tkn.Result.Count > 6 then par6 := tkn.Result[6] ;
    end ;

    procedure doscan ;
    begin

      exxStreamCompressed := False ;
      exxBufferPos        := -1    ;
      exxBufferSize       := -1    ;

      exxStream := TGIS_BufferedFileStream.Create( Path, TGIS_StreamMode.Read ) ;
      exxStreamEof  := False ;
      lineNo := 0 ;

      RaiseBusyPrepare( Self, Format( _rsrc( GIS_RS_BUSY_READ ), [Name] ) ) ;
      Lock ;
      try
        existingShapes := GisGetEmptyShapeType ;

        tkn       := TGIS_Tokenizer.Create ;
        {$IFDEF JAVA}
          widthList := TList<Integer>.Create ;
        {$ELSE}
          widthList := TList<Integer>.Create ;
        {$ENDIF}
        try
          while not exxStreamEof do begin
            exxReadLine ;

            tokenize ;

            if lineNo = 1 then begin
              if tkn.Result.Count < 2 then
                raise EGIS_Exception.Create(
                        _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), Path, lineNo
                      ) ;

              if not _t( E00_EXP ) then
                raise EGIS_Exception.Create(
                        _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), Path, lineNo
                      ) ;

              if      par1 = '0' then
                exxStreamCompressed := False
              else if par1 = '1' then
                exxStreamCompressed := True
              else
                raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ),Path,lineNo ) ;

              if tkn.Result.Count < 3 then
                sName := GetFileNameNoExt( UpperCase( Path ) )
              else
                sName := GetFileNameNoExt(
                           Copy( lineStr, Pos( par2, lineStr ), 8192 )
                         ) ;
            end ;

            if IsStringEmpty( par0 ) then continue ;

            if      _t( E00_ARC        ) then doArc      ( par1 )
            else if _t( E00_LAB        ) then doLab      ( par1 )
            else if _t( E00_PAL        ) then doPal      ( par1 )
            else if _t( E00_PRJ        ) then doPrj      ( par1 )

            else if _p( E00_AAT        ) then doAttribute( E00_AAT, par1, par2,
                                                           par3, par4, par5
                                                         )
            else if _p( E00_PAT        ) then doAttribute( E00_PAT, par1, par2,
                                                           par3, par4, par5
                                                         ) ;
          end ;
        finally
          FreeObject( tkn ) ;
        end ;
      finally
        Unlock ;
        RaiseBusyRelease( Self ) ;

        FreeObject( exxStream ) ;
        FreeObject( widthList ) ;
        FreeObject( arcItems ) ;

        if ( not isPrescan ) and GisIsNoWorld( Extent ) then begin
          case ShapeType of
            TGIS_ShapeType.Arc        : stmp := E00_ARC ;
            TGIS_ShapeType.Polygon    : stmp := E00_PAL ;
            else                     stmp := E00_LAB ;
          end ;
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_E00ENTRY ) + stmp,
                                       Path, 0
                                     ) ;
        end ;
      end ;
    end ;

  begin
    inherited ;

    wasPrescan := False ;

    if ShapeType = TGIS_ShapeType.Unknown then begin
      // do prescan
      isPrescan := True ;
      doscan ;

      if      GisTestShapeType( TGIS_ShapeType.Polygon, existingShapes )    then
              ShapeType := TGIS_ShapeType.Polygon
      else if GisTestShapeType( TGIS_ShapeType.Arc, existingShapes )        then
              ShapeType := TGIS_ShapeType.Arc
      else if GisTestShapeType( TGIS_ShapeType.MultiPoint, existingShapes ) then
              ShapeType := TGIS_ShapeType.MultiPoint
      else if GisTestShapeType( TGIS_ShapeType.Point, existingShapes )      then
              ShapeType := TGIS_ShapeType.Point
      else    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_E00ENTRY ) +
                                           E00_ARC +';'+ E00_PAL +';'+ E00_LAB,
                                           Path, 0
                                         ) ;

      wasPrescan := True ;
    end ;

    FSupportedShapes := GisGetShapeType( ShapeType ) ;

    isPrescan := False ;
    doscan ;

    if SafeFileExists( Path ) then
      FAge := GisFileAge( Path ) ;

    FFileInfo := 'Arcinfo Export Format (E00) ' ;
    case ShapeType of
      TGIS_ShapeType.Arc        : FFileInfo := FFileInfo + E00_ARC ;
      TGIS_ShapeType.Polygon    : FFileInfo := FFileInfo + E00_PAT ;
      else                     FFileInfo := FFileInfo + E00_LAB ;
    end ;
  end ;

  function TGIS_LayerE00.PreRecognize( const _path     : String ;
                                         var _new_path : String
                                     ) : Boolean ;
  var
    mode : String   ;
    k    : Integer  ;

    function _m( const _mode : String ) : Boolean ;
    begin
      Result := CompareText( mode, _mode ) = 0 ;
    end ;

  begin
    Result := inherited PreRecognize( _path, _new_path );

    if not IsServerPath( _path ) then begin
      k := Pos( '?', _path ) ;
      if k = 0 then
        mode := ''
      else
        mode := Copy( _path, k+1, MaxInt ) ;
    end
    else
      mode := '' ;

    if      _m( 'arc' ) then
            ShapeType := TGIS_ShapeType.Arc
    else if _m( 'pal' ) then
            ShapeType := TGIS_ShapeType.Polygon
    else if _m( 'lab' ) then
            ShapeType := TGIS_ShapeType.Point ;
  end;

  { Perform initialization section.
  }
  class procedure Unit_GisLayerE00.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-E00', 'Arcinfo Export Format', TGIS_LayerE00, '.e00',
                   TGIS_RegisteredLayerType.Vector, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   True
                 ) ;
  end;

{$IFNDEF OXYGENE}
  initialization
    Unit_GisLayerE00.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.

