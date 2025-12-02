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
  Internal part of SDTS implementation.
  * RPE - Raster Profile and Extensions
  * TVP - Topological Vector Profile

  This unit was partially based on SDTS Translator Implementation:

  Copyright (c) 1999, Frank Warmerdam

  Permission is hereby granted, free of charge, to any person obtaining a
  Copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, Copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included
  in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
  OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

  Sublicensing of this unit is a subject of TatukGIS Developer
  Kernel License
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFileSDTS ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFileSDTS"'}
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoFile8211 ;
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

  { Double array list. }
  {#GENDOC:HIDE}
  TGIS_SDTS_DoubleArray = {$IFDEF OXYGENE} public {$ENDIF} array of Double ;

  { IREF class. }
  {#GENDOC:HIDE}
  TGIS_SDTS_IREF = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Object )
    private
      defSADRFmt  : Boolean ;
    protected
      XAxisName : String ;
      YAxisName : String ;
      coordFmt  : String ;

      dfXScale  : Double ;
      dfYScale  : Double ;
      dfXOffset : Double ;
      dfYOffset : Double ;
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      dfXRes    : Double ;
      dfYRes    : Double ;
    protected
      procedure doDestroy  ; override;
    public
      constructor Create   ;

      function Read        ( const _filename  : String
                            ) : Boolean ;
      function GetSADRCount( const _field     : TGIS_DDFField
                            ) : Integer ;
      function GetSADR     ( const _record    : TGIS_DDFRecord ;
                             const _field     : TGIS_DDFField ;
                             const _nVertices : Integer ;
                             const _padfX     : TGIS_SDTS_DoubleArray ;
                             const _padfY     : TGIS_SDTS_DoubleArray ;
                             const _padfZ     : TGIS_SDTS_DoubleArray
                            ) : Boolean ;
  end ;

  { XREF class. }
  {#GENDOC:HIDE}
  TGIS_SDTS_XREF = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Object )
    public
      sSystemName : String ;
      sDatum      : String ;
      nZone       : Integer ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create ;

      function Read( const _filename : String
                    ) : Boolean ;
  end ;

  { CATD entry class. }
  {#GENDOC:HIDE}
  TGIS_SDTS_CATDEntry = {$IFDEF OXYGENE} public {$ENDIF} class
    public
      sModule       : String ;
      sType         : String ;
      sFileName     : String ;
      sExternalFlag : String ;
      sFullPath     : String ;
  end ;

  { Layer types. }
  {#GENDOC:HIDE}
  TGIS_SDTSLayerType = {$IFDEF OXYGENE} public {$ENDIF} (
    Unknown,
    Point,
    Line,
    Attr,
    Poly,
    Raster
  ) ;

  { CATD class. }
  {#GENDOC:HIDE}
  TGIS_SDTS_CATD = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    private
      sPrefixPath   : String ;
      nEntries      : Integer ;
      arEntries     : array of TGIS_SDTS_CATDEntry ;
    protected
      procedure doDestroy       ; override;
    public
      constructor Create        ;

      function Read             ( const _filename : String
                                 ) : Boolean ;
      function GetModuleFilePath( const _pszModule : String
                                 ) : String ;
      function GetEntryModule   ( const _entry : Integer ) : String ;
      function GetEntryTypeDesc ( const _entry : Integer ) : String ;
      function GetEntryFilePath ( const _entry : Integer ) : String ;
      function GetEntryType     ( const _entry : Integer ) : TGIS_SDTSLayerType ;
    public
      property EntryCount : Integer read nEntries ;
  end ;

  { Module id class. }
  {#GENDOC:HIDE}
  TGIS_SDTSModId = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Object )
    public
      sModule : String  ;
      nRecord : Integer ;
      sOBRP   : String  ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create ;

      procedure SetIt( const _field  : TGIS_DDFField ;
                       const _record : TGIS_DDFRecord
                      ) ;
  end ;

  { Feature class. }
  {#GENDOC:HIDE}
  TGIS_SDTSFeature = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    public
      oModId      : TGIS_SDTSModId ;
      nAttributes : Integer   ;
      arATID      : array of TGIS_SDTSModId ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create ;

      procedure ApplyATID( const _field : TGIS_DDFField ;
                           const _record: TGIS_DDFRecord
                          ) ;
  end ;

  { Indexed reader class. }
  {#GENDOC:HIDE}
  TGIS_SDTSIndexedReader = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    private
      nIndexSize      : Integer ;
      arFeatures      : array of TGIS_SDTSFeature ;
      iCurrentFeature : Integer ;
    protected
      oDDFModule      : TGIS_DDFModule ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create ;

      function  GetNextRawFeature : TGIS_SDTSFeature ; virtual;

      function  GetNextFeature : TGIS_SDTSFeature ;

      procedure Rewind ; virtual;

      procedure FillIndex ;
      procedure ClearIndex ;
      function  IsIndexed : Boolean ;

      function GetIndexedFeatureRef  ( const _iRecordId : Integer
                                     ) : TGIS_SDTSFeature ;
      {$IFDEF OXYGENE}
        function ScanModuleReferences  : TGIS_Strings ;
        function ScanModuleReferences( const _pszFName : String
                                     ) : TGIS_Strings ;
      {$ELSE}
        function ScanModuleReferences( const _pszFName : String = 'ATID'
                                     ) : TStrings ;
      {$ENDIF}
    public
      property DDFModule : TGIS_DDFModule read oDDFModule ;
  end ;

  { Raw line class. }
  {#GENDOC:HIDE}
  TGIS_SDTSRawLine = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_SDTSFeature )
    public
      nVertices   : Integer ;
      padfX       : TGIS_SDTS_DoubleArray ;
      padfY       : TGIS_SDTS_DoubleArray ;
      padfZ       : TGIS_SDTS_DoubleArray ;
      oLeftPoly   : TGIS_SDTSModId ;
      oRightPoly  : TGIS_SDTSModId ;
      oStartNode  : TGIS_SDTSModId ;
      oEndNode    : TGIS_SDTSModId ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create ;

      function Read( const _poIREF  : TGIS_SDTS_IREF ;
                     const _record  : TGIS_DDFRecord
                    ) : Boolean ;
  end ;

  {#GENDOC:HIDE}
  TGIS_SDTSTransfer = {$IFDEF OXYGENE} public {$ENDIF} class ;

  { Line reader. }
  {#GENDOC:HIDE}
  TGIS_SDTSLineReader = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_SDTSIndexedReader )
    private
      poIREF : TGIS_SDTS_IREF ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create( const _poIREFIn : TGIS_SDTS_IREF ) ;

      function  Open( const _filename : String ) : Boolean ;
      function  GetNextLine : TGIS_SDTSRawLine ;

      function  GetNextRawFeature : TGIS_SDTSFeature ; override;

      procedure AttachToPolygons( const _transfer : TGIS_SDTSTransfer ) ;
  end ;

  { Attribute record class. }
  {#GENDOC:HIDE}
  TGIS_SDTSAttrRecord = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_SDTSFeature )
    public
      poWholeRecord : TGIS_DDFRecord ;
      poATTR        : TGIS_DDFField ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create ;
  end ;

  { Attribute reader class. }
  {#GENDOC:HIDE}
  TGIS_SDTSAttrReader = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_SDTSIndexedReader )
    private
      bIsSecondary  : Boolean ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create( const _poIREFIn : TGIS_SDTS_IREF ) ;

      function  Open( const _filename : String ) : Boolean ;
      function  GetNextRecord( const _poModId    : TGIS_SDTSModId ;
                                 var _ppoRecord  : TGIS_DDFRecord ;
                               const _bDuplicate : Boolean
                              ) : TGIS_DDFField ;
      function  GetNextAttrRecord : TGIS_SDTSAttrRecord ;
      procedure Close ;

      function  IsSecondary : Boolean ;

      function  GetNextRawFeature : TGIS_SDTSFeature ; override;
  end ;

  { Raw point class. }
  {#GENDOC:HIDE}
  TGIS_SDTSRawPoint = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_SDTSFeature )
    public
      dfX     : TGIS_SDTS_DoubleArray ;
      dfY     : TGIS_SDTS_DoubleArray ;
      dfZ     : TGIS_SDTS_DoubleArray ;
      oAreaId : TGIS_SDTSModId ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create ;

      function  Read( const _poIREF   : TGIS_SDTS_IREF ;
                      const _record : TGIS_DDFRecord
                     ) : Boolean ;
  end ;

  { Point reader class. }
  {#GENDOC:HIDE}
  TGIS_SDTSPointReader = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_SDTSIndexedReader )
    private
      poIREF : TGIS_SDTS_IREF ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create( const _poIREFIn : TGIS_SDTS_IREF ) ;

      function  Open( const _filename : String ) : Boolean ;
      function  GetNextPoint : TGIS_SDTSRawPoint ;

      function GetNextRawFeature : TGIS_SDTSFeature ; override;
  end ;

  { Raw polygon class. }
  {#GENDOC:HIDE}
  TGIS_SDTSRawPolygon = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_SDTSFeature )
    private
      procedure addEdgeToRing( const _vertex     : Integer               ;
                               const _x          : TGIS_SDTS_DoubleArray ;
                               const _y          : TGIS_SDTS_DoubleArray ;
                               const _z          : TGIS_SDTS_DoubleArray ;
                               const _offset     : Integer               ;
                               const _reverse    : Boolean               ;
                               const _dropvertex : Boolean
                              ) ;
    public
      nEdges        : Integer ;
      papoEdges     : array of TGIS_SDTSRawLine ;
      nRings        : Integer ;
      nVertices     : Integer ;
      arRingStart   : array of Integer ;
      arX           : TGIS_SDTS_DoubleArray ;
      arY           : TGIS_SDTS_DoubleArray ;
      arZ           : TGIS_SDTS_DoubleArray ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create ;

      function  Read( const _record : TGIS_DDFRecord ) : Boolean ;
      procedure AddEdge( const _newline : TGIS_SDTSRawLine ) ;
      function  AssembleRings : Boolean ;
  end ;

  { Polygon reader class. }
  {#GENDOC:HIDE}
  TGIS_SDTSPolygonReader = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_SDTSIndexedReader )
    private
      bRingsAssembled : Boolean ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create ;

      function  Open( const _filename : String ) : Boolean ;
      function  GetNextPolygon : TGIS_SDTSRawPolygon ;
      procedure Close ;
      function  GetNextRawFeature : TGIS_SDTSFeature ; override;
      procedure AssembleRings( const _transfer : TGIS_SDTSTransfer ) ;
  end ;

  { Raster reader class. }
  {#GENDOC:HIDE}
  TGIS_SDTSRasterReader = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    private
      oDDFModule  : TGIS_DDFModule ;
      szModule    : String ;
      nXSize      : Integer ;
      nYSize      : Integer ;
      nXBlockSize : Integer ;
      nYBlockSize : Integer ;
      nXStart     : Integer ;
      nYStart     : Integer ;
      arTransform : TGIS_SDTS_DoubleArray ;
      dMin, dMax, dFill, dVoid : Double ;
    protected
      szINTR  : String ;
      szFMT   : String ;
      szUNITS : String ;
      szLabel : String ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create ;

      function  Open    ( const _poCATD     : TGIS_SDTS_CATD ;
                          const _poIREF     : TGIS_SDTS_IREF ;
                          const _pszModule  : String
                         ) : Boolean ;
      procedure GetExtent( var _xmin       : Double ;
                           var _ymin       : Double ;
                           var _xmax       : Double ;
                           var _ymax       : Double
                          )  ;
      procedure GetScale (  var _x         : Double ;
                            var _y         : Double
                          )  ;
      function  GetBlock ( const _nXOffset  : Integer ;
                           const _nYOffset  : Integer ;
                             var _pData     : TBytes
                          ) : Boolean ;
      function  GetRasterType : Integer ;
    public
      property  Min        : Double  read dMin ;
      property  Max        : Double  read dMax ;
      property  NoData     : Double  read dVoid ;
      property  Fill       : Double  read dFill ;
      property  XSize      : Integer read nXSize ;
      property  Units      : String  read szUNITS ;
      property  YSize      : Integer read nYSize ;
      property  BlockXSize : Integer read nXBlockSize ;
      property  BlockYSize : Integer read nYBlockSize ;
  end ;

  { Transfer class. }
  {#GENDOC:HIDE}
  TGIS_SDTSTransfer = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    private
      oCATD             : TGIS_SDTS_CATD ;
      oIREF             : TGIS_SDTS_IREF ;
      oXREF             : TGIS_SDTS_XREF ;
      nLayers           : Integer ;
      panLayerCATDEntry : array of Integer ;
      papoLayerReader   : array of TGIS_SDTSIndexedReader ;
    protected
      procedure doDestroy             ; override;
    public
      constructor Create              ;

      function  Open                  ( const _filename : String
                                      ) : Boolean ;
      procedure Close ;
      function  FindLayer             ( const _pszModule  : String
                                      ) : Integer ;
      function  GetLayerType          ( const _entry      : Integer
                                      ) : TGIS_SDTSLayerType ;
      function  GetLayerCATDEntry     ( const _entry      : Integer
                                      ) : Integer ;
      function  GetLayerLineReader    ( const _entry      : Integer
                                      ) : TGIS_SDTSLineReader ;
      function  GetLayerPointReader   ( const _entry      : Integer
                                      ) : TGIS_SDTSPointReader ;
      function  GetLayerPolygonReader ( const _entry      : Integer
                                      ) : TGIS_SDTSPolygonReader ;
      function  GetLayerAttrReader    ( const _entry      : Integer
                                      ) : TGIS_SDTSAttrReader ;
      function  GetLayerRasterReader  ( const _entry      : Integer
                                      ) : TGIS_SDTSRasterReader ;
      function  GetLayerModuleReader  ( const _entry      : Integer
                                      ) : TGIS_DDFModule ;
      function  GetLayerIndexedReader ( const _entry      : Integer
                                      ) : TGIS_SDTSIndexedReader ;
      function  GetIndexedFeatureRef  ( const _poModId    : TGIS_SDTSModId ;
                                          var _peType     : TGIS_SDTSLayerType
                                       ) : TGIS_SDTSFeature ;
      function  GetAttr               ( const _poModId    : TGIS_SDTSModId
                                       ) : TGIS_DDFField ;
    public
      property  LayerCount : Integer        read nLayers ;
      property  CATD       : TGIS_SDTS_CATD read oCATD ;
      property  IREF       : TGIS_SDTS_IREF read oIREF ;
      property  XREF       : TGIS_SDTS_XREF read oXREF ;
  end ;

//##############################################################################
implementation

{$IFDEF OXYGENE}
{$ELSE}
  uses
    Lider.CG.GIS.GeoInternals ;
{$ENDIF}

const
  GIS_SDTS_SIZEOF_SADR  = 8 ;
  GIS_SDTS_RT_INT16     = 1 ;
  GIS_SDTS_RT_FLOAT32   = 6 ;

//==============================================================================
// TGIS_SDTS_IREF
//==============================================================================

  constructor TGIS_SDTS_IREF.Create ;
  begin
    inherited ;

    dfXScale  := 1.0 ;
    dfYScale  := 1.0 ;
    dfXOffset := 0.0 ;
    dfYOffset := 0.0 ;
    dfXRes    := 1.0 ;
    dfYRes    := 1.0 ;

    XAxisName := '' ;
    YAxisName := '' ;
    coordFmt  := '' ;
  end ;

  procedure TGIS_SDTS_IREF.doDestroy ;
  begin
    inherited ;
  end ;

  function endian_swap16(
    const _n : SmallInt
  ) : SmallInt ;
  begin
    Result := SmallInt((( _n and $000000ff) shl 8 ) or
                       (( _n and $0000ff00) shr 8 )) ;
  end ;

  function endian_swap32(
    const _n : Integer
  ) : Integer ;
  begin
    Result := Integer((Integer( _n and $000000ff) shl 24) or
                      (Integer( _n and $0000ff00) shl  8) or
                      (Integer( _n and $00ff0000) shr  8) or
                      (Integer( _n and $ff000000) shr 24)) ;
  end ;

  // Read 16-bit integer (2 bytes) from big endian stored bytes.
  // _buf     source buffer
  // _offset  offset in a buffer
  // return   integer value in processor format
  function readInt16BE(
    const _buf    : TBytes  ;
    const _offset : Integer
  ) : SmallInt ;
  begin
    Result := SmallInt( ( _buf[1+_offset]       ) or
                        ( _buf[0+_offset] shl 8 )
                      ) ;
  end ;

  // Read 16-bit integer (2 bytes) from little endian stored bytes.
  // _buf     source buffer
  // _offset  offset in a buffer
  // return   integer value in processor format
  function readInt16LE(
    const _buf    : TBytes  ;
    const _offset : Integer
  ) : SmallInt ;
  begin
    Result := SmallInt( ( _buf[0+_offset]       ) or
                        ( _buf[1+_offset] shl 8 )
                      ) ;
  end ;

  // Read 32-bit integer (4 bytes) from big endian stored bytes.
  // _buf     source buffer
  // _offset  offset in a buffer
  // return   integer value in processor format

  function readInt32BE(
    const _buf    : TBytes  ;
    const _offset : Integer
  ) : Integer ;
  begin
    Result := Integer( ( _buf[3+_offset]        ) or
                       ( _buf[2+_offset] shl  8 ) or
                       ( _buf[1+_offset] shl 16 ) or
                       ( _buf[0+_offset] shl 24 )
                     ) ;
  end ;

  // Read 32-bit integer (4 bytes) from little endian stored bytes.
  // _buf     source buffer
  // _offset  offset in a buffer
  // return   integer value in processor format

  function readInt32LE(
    const _buf    : TBytes  ;
    const _offset : Integer
  ) : Integer ;
  begin
    Result := Integer( ( _buf[0+_offset]        ) or
                       ( _buf[1+_offset] shl  8 ) or
                       ( _buf[2+_offset] shl 16 ) or
                       ( _buf[3+_offset] shl 24 )
                     ) ;
  end ;

  function TGIS_SDTS_IREF.GetSADR(
    const _record    : TGIS_DDFRecord ;
    const _field     : TGIS_DDFField ;
    const _nVertices : Integer ;
    const _padfX     : TGIS_SDTS_DoubleArray ;
    const _padfY     : TGIS_SDTS_DoubleArray ;
    const _padfZ     : TGIS_SDTS_DoubleArray
  ) : Boolean ;
  var
    adfXYZ          : TGIS_SDTS_DoubleArray ;
    pachRawData     : TBytes ;
    iVertex, off    : Integer ;
    poFieldDefn     : TGIS_DDFFieldDefn ;
    nBytesRemaining : Integer ;
    pachFieldData   : TBytes ;
    iEntry,
    nBytesConsumed  : Integer ;
    poSF            : TGIS_DDFSubfieldDefn ;
    pabyBString     : TBytes ;
    nValue          : Integer ;
    sValue          : SmallInt ;
    cValue          : Cardinal ;
    dValue          : Word ;
    fValue          : Double ;
  begin
    if defSADRFmt and ( _field.FieldDefn.SubfieldCount = 2 ) then begin

      pachRawData := Copy( _record.Data, _field.Data, length(_record.Data) ) ;
      off := 0 ;

      for iVertex := 0 to _nVertices-1 do begin
        _padfX[iVertex] := dfXOffset + dfXScale * readInt32BE( pachRawData, off ) ;
        inc( off, 4 ) ;
        _padfY[iVertex] := dfYOffset + dfYScale * readInt32BE( pachRawData, off ) ;
        inc( off, 4 ) ;
        _padfZ[iVertex] := 0.0 ;
      end
    end
    else begin
      poFieldDefn     := _field.FieldDefn ;
      nBytesRemaining := _field.DataSize ;
      pachFieldData   := Copy( _record.Data, _field.Data, length(_record.Data) ) ;

      SetLength( adfXYZ, 3 ) ;
      for iVertex := 0 to _nVertices-1 do begin

        adfXYZ[2] := 0.0 ;

        for iEntry := 0 to poFieldDefn.SubfieldCount-1 do begin
          poSF := poFieldDefn.GetSubfield(iEntry) ;

          case poSF.DataType of
            TGIS_DDFDataType.Int   : adfXYZ[iEntry] := poSF.GetIntData( pachFieldData,
                                                          nBytesRemaining,
                                                          nBytesConsumed
                                                        ) ;
            TGIS_DDFDataType.Float : adfXYZ[iEntry] := poSF.GetFloatData( pachFieldData,
                                                            nBytesRemaining,
                                                            nBytesConsumed
                                                          ) ;
            TGIS_DDFDataType.BinaryString :
              begin
                pabyBString := poSF.GetStringData( pachFieldData,
                                                   nBytesRemaining,
                                                   nBytesConsumed
                                                 ) ;
                if coordFmt = 'BI32' then begin
                   {$IFDEF OXYGENE}
                    nValue := BitConverter.ToInt32( pabyBString, 0 ) ;
                   {$ELSE}
                    nValue := PInteger( @pabyBString[0] )^ ;
                   {$ENDIF}
                   adfXYZ[iEntry] := nValue ;
                end
                else if coordFmt = 'BI16' then begin
                   {$IFDEF OXYGENE}
                   sValue := BitConverter.ToInt16( pabyBString, 0 ) ;
                   {$ELSE}
                   sValue := PSmallInt( @pabyBString[0] )^  ;
                   {$ENDIF}
                   adfXYZ[iEntry] := sValue ;
                end
                else if coordFmt = 'BU32' then begin
                   {$IFDEF OXYGENE}
                   cValue := BitConverter.ToUInt32( pabyBString, 0 ) ;
                   {$ELSE}
                   cValue := PCardinal( @pabyBString[0] )^ ;
                   {$ENDIF}
                   adfXYZ[iEntry] := cValue ;
                end
                else if coordFmt = 'BU16' then begin
                   {$IFDEF OXYGENE}
                   dValue := BitConverter.ToUInt16( pabyBString, 0 ) ;
                   {$ELSE}
                   dValue := PWord( @pabyBString[0] )^ ;
                   {$ENDIF}
                   adfXYZ[iEntry] := dValue ;
                end
                else if coordFmt = 'BFP32' then begin
                   {$IFDEF OXYGENE}
                   fValue := BitConverter.ToDouble( pabyBString, 0 ) ;
                   {$ELSE}
                   fValue := PDouble( @pabyBString[0] )^ ;
                   {$ENDIF}
                   adfXYZ[iEntry] := fValue ;
                end
                else if coordFmt = 'BFP64' then begin
                   {$IFDEF OXYGENE}
                   fValue := BitConverter.ToDouble( pabyBString, 0 ) ;
                   {$ELSE}
                   fValue := PDouble( @pabyBString[0] )^ ;
                   {$ENDIF}
                   adfXYZ[iEntry] := fValue ;
                end
                else
                  adfXYZ[iEntry] := 0 ;
              end ;
          end ;

          pachFieldData := Copy( pachFieldData, nBytesConsumed, length(pachFieldData) ) ;
          dec( nBytesRemaining, nBytesConsumed ) ;
        end ;

        _padfX[iVertex] := dfXOffset + adfXYZ[0] * dfXScale ;
        _padfY[iVertex] := dfYOffset + adfXYZ[1] * dfYScale ;
        _padfZ[iVertex] := adfXYZ[2] ;
      end ;
    end ;

    Result := True ;
  end ;

  function TGIS_SDTS_IREF.GetSADRCount(
    const _field : TGIS_DDFField
  ) : Integer ;
  begin
    if defSADRFmt then
      Result := _field.DataSize div GIS_SDTS_SIZEOF_SADR
    else
      Result := _field.GetRepeatCount(nil) ;
  end ;

  function TGIS_SDTS_IREF.Read(
    const _filename : String
  ) : Boolean ;
  var
    oIREFFile : TGIS_DDFModule ;
    poRecord  : TGIS_DDFRecord ;
  begin
    oIREFFile := TGIS_DDFModule.Create ;
    try
      if not oIREFFile.Open( _filename ) then begin
        Result := FALSE ;
        exit ;
      end ;

      poRecord := oIREFFile.ReadRecord ;
      if ( poRecord = nil ) then begin
        Result := FALSE ;
        exit ;
      end ;

      if IsStringEmpty( poRecord.GetStringSubfield( 'IREF', 0, 'MODN', 0 ) ) then begin
        Result := FALSE ;
        exit ;
      end ;

      XAxisName := poRecord.GetStringSubfield( 'IREF', 0, 'XLBL', 0 ) ;
      YAxisName := poRecord.GetStringSubfield( 'IREF', 0, 'YLBL', 0 ) ;

      coordFmt  := poRecord.GetStringSubfield( 'IREF', 0, 'HFMT', 0 ) ;

      dfXScale  := poRecord.GetFloatSubfield( 'IREF', 0, 'SFAX', 0 ) ;
      dfYScale  := poRecord.GetFloatSubfield( 'IREF', 0, 'SFAY', 0 ) ;

      dfXOffset := poRecord.GetFloatSubfield( 'IREF', 0, 'XORG', 0 ) ;
      dfYOffset := poRecord.GetFloatSubfield( 'IREF', 0, 'YORG', 0 ) ;

      dfXRes    := poRecord.GetFloatSubfield( 'IREF', 0, 'XHRS', 0 ) ;
      dfYRes    := poRecord.GetFloatSubfield( 'IREF', 0, 'YHRS', 0 ) ;

      defSADRFmt:= coordFmt = 'BI32' ;

      Result := True ;
    finally
      FreeObject( oIREFFile ) ;
    end ;
  end ;

//==============================================================================
// TGIS_SDTS_CATD
//==============================================================================

  constructor TGIS_SDTS_CATD.Create ;
  begin
    inherited ;

    nEntries    := 0   ;
    arEntries   := nil ;
    sPrefixPath := ''  ;
  end ;

  procedure TGIS_SDTS_CATD.doDestroy ;
  var
    i : Integer ;
  begin
    for i := 0 to nEntries-1 do
      FreeObject( arEntries[i] ) ;

    nEntries  := 0   ;
    arEntries := nil ;

    inherited ;
  end ;

  function TGIS_SDTS_CATD.GetEntryFilePath(
    const _entry: Integer
  ) : String ;
  begin
    if ( _entry < 0 ) or ( _entry >= nEntries ) then
      Result := ''
    else
     Result := arEntries[_entry].sFullPath ;
  end ;

  function TGIS_SDTS_CATD.GetEntryModule(
    const _entry : Integer
  ) : String ;
  begin
    if ( _entry < 0 ) or ( _entry >= nEntries ) then
      Result := ''
    else
      Result := arEntries[_entry].sModule ;
  end ;

  function TGIS_SDTS_CATD.GetEntryType(
    const _entry : Integer
  ) : TGIS_SDTSLayerType ;

    function _t( const _s1, _s2 : String ; const _n : Integer ) : Boolean ;
    begin
      Result := CompareText( Copy( _s1, StringFirst, _n ), _s2 ) = 0 ;
    end ;

  begin
    if ( _entry < 0 ) or ( _entry >= nEntries ) then
        Result := TGIS_SDTSLayerType.Unknown
    else if ( _t(arEntries[_entry].sType, 'Attribute Primary', 17) ) then
        Result := TGIS_SDTSLayerType.Attr
    else if ( _t(arEntries[_entry].sType, 'Attribute Secondary', 17) ) then
        Result := TGIS_SDTSLayerType.Attr
    else if ( _t(arEntries[_entry].sType, 'Line', MaxInt) or
              _t(arEntries[_entry].sType, 'Line ', 5) ) then
        Result := TGIS_SDTSLayerType.Line
    else if ( _t(arEntries[_entry].sType, 'Point-Node', 10) ) then
        Result := TGIS_SDTSLayerType.Point
    else if ( _t(arEntries[_entry].sType, 'Polygon', 7) ) then
        Result := TGIS_SDTSLayerType.Poly
    else if ( _t(arEntries[_entry].sType, 'Cell', 4) ) then
        Result := TGIS_SDTSLayerType.Raster
    else
        Result := TGIS_SDTSLayerType.Unknown ;
  end ;

  function TGIS_SDTS_CATD.GetEntryTypeDesc(
    const _entry : Integer
  ) : String ;
  begin
    if ( _entry < 0 ) or ( _entry >= nEntries ) then
      Result := ''
    else
      Result := arEntries[_entry].sType ;
  end ;

  function TGIS_SDTS_CATD.GetModuleFilePath(
    const _pszModule : String
  ) : String ;
  var
    i : Integer ;
  begin
    Result := '' ;
    for i := 0 to nEntries-1 do
      if arEntries[i].sModule = _pszModule then begin
        Result := arEntries[i].sFullPath ;
        break ;
      end ;
  end ;

  function TGIS_SDTS_CATD.Read(
    const _filename : String
  ) : Boolean ;
  var
    oCATDFile : TGIS_DDFModule ;
    poRecord  : TGIS_DDFRecord ;
    poEntry   : TGIS_SDTS_CATDEntry ;
  begin
    oCATDFile := TGIS_DDFModule.Create ;
    try
      if not oCATDFile.Open( _filename ) then begin
        Result := False ;
        exit ;
      end ;

      if oCATDFile.FindFieldDefn( 'CATD' ) = nil then begin
        Result := False ;
        exit ;
      end ;

      sPrefixPath := GetFilePath( _filename ) ;

      poRecord := oCATDFile.ReadRecord ;
      while poRecord <> nil do begin

        if IsStringEmpty( poRecord.GetStringSubfield( 'CATD', 0, 'MODN', 0 ) ) then
          continue ;

        poEntry := TGIS_SDTS_CATDEntry.Create ;
        poEntry.sModule       := poRecord.GetStringSubfield( 'CATD', 0, 'NAME', 0 ) ;
        poEntry.sFileName     := poRecord.GetStringSubfield( 'CATD', 0, 'FILE', 0 ) ;
        poEntry.sExternalFlag := poRecord.GetStringSubfield( 'CATD', 0, 'EXTR', 0 ) ;
        poEntry.sType         := poRecord.GetStringSubfield( 'CATD', 0, 'TYPE', 0 ) ;
        poEntry.sFullPath     := sPrefixPath + poEntry.sFileName ;
        inc( nEntries ) ;
        SetLength( arEntries, nEntries ) ;
        arEntries[nEntries-1] := poEntry ;

        poRecord := oCATDFile.ReadRecord ;
      end ;

      Result := nEntries > 0 ;
    finally
      FreeObject( oCATDFile ) ;
    end ;
  end ;

//==============================================================================
// TGIS_SDTS_XREF
//==============================================================================

  constructor TGIS_SDTS_XREF.Create ;
  begin
    inherited ;

    sSystemName := '' ;
    sDatum      := '' ;
    nZone       := 0 ;
  end ;

  procedure TGIS_SDTS_XREF.doDestroy ;
  begin
    inherited ;
  end ;

  function TGIS_SDTS_XREF.Read(
    const _filename : String
  ) : Boolean ;
  var
    omodule : TGIS_DDFModule   ;
    orecord  : TGIS_DDFRecord   ;
  begin
    omodule := TGIS_DDFModule.Create ;
    try
      if not omodule.Open( _filename ) then begin
        Result := False ;
        exit ;
      end ;

      orecord := omodule.ReadRecord ;
      if not assigned( orecord ) then begin
        Result := False ;
        exit ;
      end ;

      if IsStringEmpty( orecord.GetStringSubfield( 'XREF', 0, 'MODN', 0 ) ) then begin
        Result := False ;
        exit ;
      end ;

      sSystemName := orecord.GetStringSubfield( 'XREF', 0, 'RSNM', 0 ) ;
      sDatum      := orecord.GetStringSubfield( 'XREF', 0, 'HDAT', 0 ) ;
      nZone       := orecord.GetIntSubfield( 'XREF', 0, 'ZONE', 0 ) ;

      Result := True ;
    finally
      FreeObject( omodule ) ;
    end ;
  end ;

//==============================================================================
// TGIS_SDTSModId
//==============================================================================

  constructor TGIS_SDTSModId.Create ;
  begin
    inherited ;

    nRecord := -1 ;
  end ;

  procedure TGIS_SDTSModId.doDestroy ;
  begin
    inherited ;
  end ;

  procedure TGIS_SDTSModId.SetIt(
    const _field  : TGIS_DDFField ;
    const _record : TGIS_DDFRecord
  ) ;
  var
    odefn    : TGIS_DDFFieldDefn ;
    ardata   : TBytes ;
    osf      : TGIS_DDFSubfieldDefn ;
    nbytes,
    nc,
    br       : Integer ;
  begin
    ardata := Copy( _record.Data, _field.Data, length(_record.Data) ) ;
    odefn  := _field.FieldDefn ;

    if ( odefn.SubfieldCount >= 2 ) and
       ( odefn.GetSubfield(0).Width = 4 ) then begin

      sModule  := _record.DDFModule.DDFScanString( Copy( ardata, 0, 4 ), 0, 4 )  ;
      nRecord  := _record.DDFModule.DDFScanInt( ardata, 4, MaxInt )  ;
    end
    else begin
      nc       := 0 ;
      osf      := _field.FieldDefn.FindSubfieldDefn( 'MODN' ) ;
      ardata   := Copy( _record.Data,
                        _field.GetSubfieldData( _record, osf, nbytes, nc ),
                        length(_record.Data)
                      ) ;

      sModule := _record.DDFModule.DDFScanString(
                    osf.GetStringData( ardata, nbytes, br ),
                    0,
                    8
                 ) ;

      osf     := _field.FieldDefn.FindSubfieldDefn( 'RCID' ) ;
      ardata  := Copy(
                   _record.Data,
                   _field.GetSubfieldData( _record, osf, nbytes),
                   length(_record.Data)
                 ) ;
      nRecord := osf.GetIntData( ardata, nbytes, br ) ;
    end ;

    if ( odefn.SubfieldCount = 3 ) then begin
      nc := 0 ;
      osf := _field.FieldDefn.FindSubfieldDefn( 'OBRP' ) ;
      if assigned( osf ) then begin
        ardata := Copy(
                    _record.Data,
                    _field.GetSubfieldData( _record, osf, nbytes, nc ),
                    length(_record.Data)
                  ) ;
        sOBRP  := _record.DDFModule.DDFScanString(
                    osf.GetStringData( ardata, nbytes, br),
                    0,
                    8
                  ) ;
      end
    end ;
  end ;

//==============================================================================
// TGIS_SDTSFeature
//==============================================================================

  constructor TGIS_SDTSFeature.Create ;
  begin
    inherited ;

    nAttributes := 0 ;
    arATID      := nil ;
    oModId      := TGIS_SDTSModId.Create ;
  end ;

  procedure TGIS_SDTSFeature.doDestroy ;
  var
    i : Integer ;
  begin
    FreeObject( oModId ) ;
    for i := 0 to nAttributes-1 do
      if assigned( arATID[ i ] ) then
        FreeObject( arATID[ i ] ) ;

    inherited ;
  end ;

  procedure TGIS_SDTSFeature.ApplyATID(
    const _field   : TGIS_DDFField ;
    const _record  : TGIS_DDFRecord
  ) ;
  var
    nRepeatCount : Integer ;
    bUsualFormat : Boolean ;
    poMODN       : TGIS_DDFSubfieldDefn ;
    iRepeat, j   : Integer ;
    pabyData     : TBytes ;
    poModId      : TGIS_SDTSModId ;
  begin
    nRepeatCount := _field.GetRepeatCount( _record ) ;

    poMODN := _field.FieldDefn.FindSubfieldDefn( 'MODN' ) ;
    if ( poMODN = nil ) then exit ;

    bUsualFormat := poMODN.Width = 4 ;

    for iRepeat := 0 to nRepeatCount-1 do begin

      SetLength( arATID, nAttributes+1 ) ;

      arATID[ nAttributes ] := TGIS_SDTSModId.Create ;
      poModId := arATID[ nAttributes ] ;

      if bUsualFormat then begin
        pabyData := Copy( _record.Data,
                          _field.GetSubfieldData( _record, poMODN, j, iRepeat ),
                          length(_record.Data) ) ;

        poModId.sModule  := _record.DDFModule.DDFScanString( pabyData, 0, 4 ) ;
        poModId.nRecord  := _record.DDFModule.DDFScanInt( pabyData, 4, MaxInt )  ;
      end
      else
        poModId.SetIt( _field, _record ) ;

      inc( nAttributes ) ;
    end
  end ;

//==============================================================================
// TGIS_SDTSIndexedReader
//==============================================================================

  constructor TGIS_SDTSIndexedReader.Create ;
  begin
    inherited ;

    nIndexSize      := 0 ;
    arFeatures      := nil ;
    iCurrentFeature := 0 ;
    oDDFModule      := TGIS_DDFModule.Create ;
  end ;

  procedure TGIS_SDTSIndexedReader.doDestroy ;
  begin
    ClearIndex ;
    FreeObject( oDDFModule ) ;

    inherited ;
  end ;

  procedure TGIS_SDTSIndexedReader.ClearIndex ;
  var
    i : Integer ;
  begin
    for i := 0 to nIndexSize-1 do
      if arFeatures[i] <> nil then
        FreeObject( arFeatures[i] ) ;

    arFeatures := nil ;
    nIndexSize   := 0 ;
  end ;

  procedure TGIS_SDTSIndexedReader.FillIndex ;
  var
    ofeature : TGIS_SDTSFeature ;
    recordid : Integer ;
    newsize  : Integer ;
    i        : Integer ;
  begin
    if ( nIndexSize <> 0 ) then exit ;

    Rewind ;

    ofeature := GetNextRawFeature ;
    while assigned( ofeature ) do begin

      recordid := ofeature.oModId.nRecord ;

      if ( recordid >= 1000000 ) then begin
        FreeObject( ofeature ) ;
        continue ;
      end ;

      if ( recordid >= nIndexSize ) then begin
        newsize := TruncS( recordid * 1.25 + 100 ) ;

        SetLength( arFeatures, newsize ) ;

        for i := nIndexSize to newsize - 1 do
          arFeatures[i] := nil ;

        nIndexSize := newsize ;
      end ;

      arFeatures[ recordid ] := ofeature ;
      ofeature := GetNextRawFeature ;
    end
  end ;

  function TGIS_SDTSIndexedReader.GetIndexedFeatureRef(
    const _iRecordId: Integer
  ): TGIS_SDTSFeature ;
  begin
    if ( nIndexSize = 0 ) then
      FillIndex ;

    if ( _iRecordId < 0 ) or ( _iRecordId >= nIndexSize ) then
      Result := nil
    else
      Result := arFeatures[_iRecordId] ;
  end ;

  function TGIS_SDTSIndexedReader.GetNextFeature
    : TGIS_SDTSFeature ;
  begin
    Result := nil ;

    if ( nIndexSize = 0 ) then
      Result := GetNextRawFeature
    else begin
      while ( iCurrentFeature < nIndexSize ) do begin
        if ( arFeatures[iCurrentFeature] <> nil ) then begin
          Result := arFeatures[iCurrentFeature] ;
          inc( iCurrentFeature ) ;
          exit ;
        end ;

        inc( iCurrentFeature ) ;
      end ;
    end ;
  end ;

  function TGIS_SDTSIndexedReader.GetNextRawFeature
    : TGIS_SDTSFeature ;
  begin
    Result := nil ;
  end ;

  function TGIS_SDTSIndexedReader.IsIndexed
    : Boolean ;
  begin
    Result := nIndexSize <> 0 ;
  end ;

  procedure TGIS_SDTSIndexedReader.Rewind ;
  begin
    if ( nIndexSize <> 0 ) then
      iCurrentFeature := 0
    else
      oDDFModule.Rewind ;
  end ;

  {$IFDEF OXYGENE}
    function TGIS_SDTSIndexedReader.ScanModuleReferences
    : TGIS_Strings ;
    begin
      Result := ScanModuleReferences( 'ATID' ) ;
    end;

    function TGIS_SDTSIndexedReader.ScanModuleReferences(
      const _pszFName: String
    ) : TGIS_Strings ;
  {$ELSE}
    function TGIS_SDTSIndexedReader.ScanModuleReferences(
      const _pszFName: String
    ) : TStrings ;
  {$ENDIF}
  var
    poIDField : TGIS_DDFFieldDefn ;
    poMODN    : TGIS_DDFSubfieldDefn ;
    poRecord  : TGIS_DDFRecord ;
    iField, i : Integer ;
    poField   : TGIS_DDFField ;
    pszModName,
    szName    : String ;
    mb        : Integer ;
  begin
    Result := TStringList.Create  ;
    poIDField := oDDFModule.FindFieldDefn( _pszFName ) ;

    if poIDField = nil then exit ;

    poMODN := poIDField.FindSubfieldDefn( 'MODN' ) ;
    if poMODN = nil then exit ;

    oDDFModule.Rewind ;
    poRecord := oDDFModule.ReadRecord ;
    while poRecord <> nil do begin

      for iField := 0 to poRecord.FieldCount -1 do begin
        poField := poRecord.GetField( iField ) ;

        if poField.FieldDefn = poIDField  then begin

          for i := 0 to poField.GetRepeatCount(poRecord)-1 do begin

            pszModName := oDDFModule.DDFScanString( Copy( poRecord.Data,
                poField.GetSubfieldData( poRecord, poMODN, mb, i ), length(poRecord.Data) ),
                0, MaxInt ) ;
            szName     := Copy( pszModName, StringFirst, 4 ) ;

            if Result.IndexOf( szName ) = -1 then
               Result.Add( szName ) ;
          end
        end ;
      end ;
      poRecord := oDDFModule.ReadRecord ;
    end ;

    oDDFModule.Rewind ;
  end ;

//==============================================================================
// TGIS_SDTSRawLine
//==============================================================================

  constructor TGIS_SDTSRawLine.Create ;
  begin
    inherited ;

    nVertices   := 0 ;
    padfX       := nil ;
    padfY       := nil ;
    padfZ       := nil ;
    nAttributes := 0 ;
    oLeftPoly   := TGIS_SDTSModId.Create ;
    oRightPoly  := TGIS_SDTSModId.Create ;
    oStartNode  := TGIS_SDTSModId.Create ;
    oEndNode    := TGIS_SDTSModId.Create ;
  end ;

  procedure TGIS_SDTSRawLine.doDestroy ;
  begin
    FreeObject( oLeftPoly  ) ;
    FreeObject( oRightPoly ) ;
    FreeObject( oStartNode ) ;
    FreeObject( oEndNode   ) ;

    inherited ;
  end ;

  function TGIS_SDTSRawLine.Read(
    const _poIREF: TGIS_SDTS_IREF ;
    const _record: TGIS_DDFRecord): Boolean ;
  var
    iField        : Integer ;
    poField       : TGIS_DDFField ;
    pszFieldName  : String ;
  begin
    for iField := 0 to _record.FieldCount -1 do begin

      poField      := _record.GetField( iField ) ;
      pszFieldName := poField.FieldDefn.Name ;

      if pszFieldName = 'LINE' then
        oModId.SetIt( poField, _record )
      else if pszFieldName = 'ATID' then
        ApplyATID( poField, _record )
      else if pszFieldName = 'PIDL' then
        oLeftPoly.SetIt( poField, _record )
      else if pszFieldName = 'PIDR' then
        oRightPoly.SetIt( poField, _record )
      else if pszFieldName = 'SNID' then
        oStartNode.SetIt( poField, _record )
      else if pszFieldName = 'ENID' then
        oEndNode.SetIt( poField, _record )
      else if pszFieldName = 'SADR' then begin
        nVertices := _poIREF.GetSADRCount( poField ) ;

        SetLength( padfX, nVertices ) ;
        SetLength( padfY, nVertices ) ;
        SetLength( padfZ, nVertices ) ;

        _poIREF.GetSADR( _record, poField, nVertices, padfX, padfY, padfZ ) ;
      end ;
    end ;

    Result := True ;
  end ;

//==============================================================================
// TGIS_SDTSLineReader
//==============================================================================

  constructor TGIS_SDTSLineReader.Create( const _poIREFIn : TGIS_SDTS_IREF ) ;
  begin
    inherited Create ;

    poIREF := _poIREFIn ;
  end ;

  procedure TGIS_SDTSLineReader.doDestroy ;
  begin
    inherited ;
  end ;

  procedure TGIS_SDTSLineReader.AttachToPolygons(
    const _transfer : TGIS_SDTSTransfer
  ) ;
  var
    oline       : TGIS_SDTSRawLine       ;
    opolyreader : TGIS_SDTSPolygonReader ;
    ipolylayer  : Integer                ;
    opoly       : TGIS_SDTSRawPolygon    ;
  begin
    opolyreader := nil ;
    FillIndex ;
    Rewind ;

    oline := TGIS_SDTSRawLine( GetNextFeature ) ;
    while assigned( oline ) do begin
      try
        if ( oline.oLeftPoly.nRecord = oline.oRightPoly.nRecord ) then
          continue ;

        if ( opolyreader = nil ) then begin

          ipolylayer := -1 ;

          if ( oline.oLeftPoly.nRecord <> -1 ) then
            ipolylayer := _transfer.FindLayer( oline.oLeftPoly.sModule )
          else if ( oline.oRightPoly.nRecord <> -1 ) then
            ipolylayer := _transfer.FindLayer( oline.oRightPoly.sModule ) ;

          if ( ipolylayer = -1 ) then
            continue ;

          opolyreader := TGIS_SDTSPolygonReader(
                           _transfer.GetLayerIndexedReader( ipolylayer )
                         ) ;

          if not assigned( opolyreader ) then
            exit ;
        end ;

        if ( oline.oLeftPoly.nRecord <> -1 ) then begin
          opoly := TGIS_SDTSRawPolygon(
                     opolyreader.GetIndexedFeatureRef(
                       oline.oLeftPoly.nRecord
                     )
                   ) ;
          if assigned( opoly ) then
            opoly.AddEdge( oline ) ;
        end ;

        if ( oline.oRightPoly.nRecord <> -1 ) then begin
          opoly := TGIS_SDTSRawPolygon(
                     opolyreader.GetIndexedFeatureRef(
                       oline.oRightPoly.nRecord
                     )
                   ) ;
          if assigned( opoly ) then
            opoly.AddEdge( oline ) ;
        end ;
      finally
        oline := TGIS_SDTSRawLine( GetNextFeature ) ;
      end ;
    end ;
  end ;

  function TGIS_SDTSLineReader.GetNextLine
    : TGIS_SDTSRawLine ;
  var
    poRecord  : TGIS_DDFRecord ;
    poRawLine : TGIS_SDTSRawLine ;
  begin
    Result := nil ;

    if ( oDDFModule.FP = nil ) then exit ;

    poRecord := oDDFModule.ReadRecord ;

    if ( poRecord = nil ) then exit ;

    poRawLine := TGIS_SDTSRawLine.Create ;

    if poRawLine.Read( poIREF, poRecord ) then
      Result := poRawLine
    else
      FreeObject( poRawLine ) ;
  end ;

  function TGIS_SDTSLineReader.GetNextRawFeature
    : TGIS_SDTSFeature ;
  begin
    Result := GetNextLine ;
  end ;

  function TGIS_SDTSLineReader.Open(
    const _filename: String
  ) : Boolean ;
  begin
    Result := oDDFModule.Open( _filename ) ;
  end ;

//==============================================================================
// TGIS_SDTSAttrRecord
//==============================================================================

  constructor TGIS_SDTSAttrRecord.Create ;
  begin
    inherited ;

    poWholeRecord := nil ;
    poATTR        := nil ;
  end ;

  procedure TGIS_SDTSAttrRecord.doDestroy ;
  begin
    if poWholeRecord <> nil then
      FreeObject( poWholeRecord ) ;

    inherited ;
  end ;

//==============================================================================
// TGIS_SDTSAttrReader
//==============================================================================

  constructor TGIS_SDTSAttrReader.Create( const _poIREFIn : TGIS_SDTS_IREF ) ;
  begin
    inherited Create ;
  end ;

  procedure TGIS_SDTSAttrReader.doDestroy ;
  begin
    Close ;

    inherited ;
  end ;

  procedure TGIS_SDTSAttrReader.Close ;
  begin
    ClearIndex ;
  end ;

  function TGIS_SDTSAttrReader.GetNextAttrRecord
    : TGIS_SDTSAttrRecord ;
  var
    poRawRecord : TGIS_DDFRecord ;
    poATTRField : TGIS_DDFField ;
    oModId      : TGIS_SDTSModId ;
    poAttrRecord: TGIS_SDTSAttrRecord ;
  begin
    Result := nil ;
    oModId := TGIS_SDTSModId.Create ;
    try
      poATTRField := GetNextRecord( oModId, poRawRecord, True ) ;
      if ( poATTRField = nil ) then exit ;

      poAttrRecord := TGIS_SDTSAttrRecord.Create ;

      poAttrRecord.poWholeRecord   := poRawRecord    ;
      poAttrRecord.poATTR          := poATTRField    ;
      poAttrRecord.oModId.sModule  := oModId.sModule ;
      poAttrRecord.oModId.nRecord  := oModId.nRecord ;
      poAttrRecord.oModId.sOBRP    := oModId.sOBRP   ;

    finally
      FreeObject( oModId ) ;
    end ;

    Result := poAttrRecord ;
  end ;

  function TGIS_SDTSAttrReader.GetNextRawFeature
    : TGIS_SDTSFeature ;
  begin
    Result := GetNextAttrRecord ;
  end ;

  function TGIS_SDTSAttrReader.GetNextRecord(
    const _poModId    : TGIS_SDTSModId ;
    var   _ppoRecord  : TGIS_DDFRecord ;
    const _bDuplicate : Boolean
  ) : TGIS_DDFField ;
  var
    poRecord : TGIS_DDFRecord ;
    poATTP,
    poATPR   : TGIS_DDFField ;
  begin
    Result     := nil ;
    _ppoRecord := nil ;

    if oDDFModule.FP = nil then exit ;

    poRecord := oDDFModule.ReadRecord ;

    if ( poRecord = nil ) then exit ;

    if ( _bDuplicate ) then
      poRecord := poRecord.Clone ;

    poATTP := poRecord.FindField( 'ATTP', 0 ) ;
    if ( poATTP = nil ) then
      poATTP := poRecord.FindField( 'ATTS', 0 ) ;

    if ( poATTP = nil ) then exit ;

    if ( _poModId <> nil ) then begin
      poATPR := poRecord.FindField( 'ATPR',0 ) ;

      if ( poATPR = nil ) then
        poATPR := poRecord.FindField( 'ATSC',0 ) ;

      if ( poATPR <> nil ) then
        _poModId.SetIt( poATPR, poRecord ) ;
    end ;

    _ppoRecord := poRecord ;

    Result := poATTP ;
  end ;

  function TGIS_SDTSAttrReader.IsSecondary
    : Boolean ;
  begin
    Result := bIsSecondary ;
  end ;

  function TGIS_SDTSAttrReader.Open(
    const _filename : String
  ) : Boolean ;
  var
    bSuccess : Boolean ;
  begin
    bSuccess := oDDFModule.Open( _filename ) ;

    if bSuccess then
      bIsSecondary := oDDFModule.FindFieldDefn('ATTS') <> nil ;

    Result := bSuccess ;
  end ;

//==============================================================================
// TGIS_SDTSRawPoint
//==============================================================================

  constructor TGIS_SDTSRawPoint.Create ;
  begin
    inherited ;

    SetLength( dfX, 1 ) ;
    SetLength( dfY, 1 ) ;
    SetLength( dfZ, 1 ) ;

    oAreaId := TGIS_SDTSModId.Create ;
  end ;

  procedure TGIS_SDTSRawPoint.doDestroy ;
  begin
    FreeObject( oAreaId ) ;

    inherited ;
  end ;

  function TGIS_SDTSRawPoint.Read(
    const _poIREF    : TGIS_SDTS_IREF ;
    const _record  : TGIS_DDFRecord
  ) : Boolean ;
  var
    iField        : Integer ;
    poField       : TGIS_DDFField ;
    pszFieldName  : String ;
  begin
    for iField := 0 to _record.FieldCount-1 do begin

      poField      := _record.GetField( iField ) ;
      pszFieldName := poField.FieldDefn.Name ;

      if pszFieldName = 'PNTS' then
        oModId.SetIt( poField, _record )
      else if pszFieldName = 'ATID' then
        ApplyATID( poField, _record )
      else if pszFieldName = 'ARID' then
        oAreaId.SetIt( poField, _record )
      else if pszFieldName = 'SADR' then
        _poIREF.GetSADR( _record, poField, 1, dfX, dfY, dfZ ) ;
    end ;

    Result := True ;
  end ;

//==============================================================================
// TGIS_SDTSPointReader
//==============================================================================

  constructor TGIS_SDTSPointReader.Create( const _poIREFIn : TGIS_SDTS_IREF ) ;
  begin
    inherited Create ;

    poIREF := _poIREFIn ;
  end ;

  procedure TGIS_SDTSPointReader.doDestroy ;
  begin
    inherited ;
  end ;

  function TGIS_SDTSPointReader.GetNextPoint
    : TGIS_SDTSRawPoint ;
  var
    poRecord    : TGIS_DDFRecord ;
    poRawPoint  : TGIS_SDTSRawPoint ;
  begin
    Result := nil ;

    if oDDFModule.FP = nil then exit ;

    poRecord := oDDFModule.ReadRecord ;

    if poRecord = nil then exit ;

    poRawPoint := TGIS_SDTSRawPoint.Create ;

    if poRawPoint.Read( poIREF, poRecord ) then
      Result := poRawPoint
    else
      FreeObject( poRawPoint ) ;
  end ;

  function TGIS_SDTSPointReader.GetNextRawFeature
    : TGIS_SDTSFeature ;
  begin
    Result := GetNextPoint ;
  end ;

  function TGIS_SDTSPointReader.Open(
    const _filename: String
  ) : Boolean ;
  begin
    Result := oDDFModule.Open( _filename ) ;
  end ;

//==============================================================================
// TGIS_SDTSRawPolygon
//==============================================================================

  constructor TGIS_SDTSRawPolygon.Create ;
  begin
    inherited ;

    nAttributes := 0 ;
    nEdges      := 0 ;
    nRings      := 0 ;
    nVertices   := 0 ;
    papoEdges   := nil ;

    arRingStart := nil ;
    arX := nil ;
    arY := nil ;
    arZ := nil ;
  end ;

  procedure TGIS_SDTSRawPolygon.doDestroy ;
  begin
    inherited ;
  end ;

  procedure TGIS_SDTSRawPolygon.AddEdge(
    const _newline : TGIS_SDTSRawLine
  ) ;
  begin
    inc( nEdges ) ;

    SetLength( papoEdges, nEdges ) ;
    papoEdges[nEdges-1] := _newline ;
  end ;

  procedure TGIS_SDTSRawPolygon.addEdgeToRing(
    const _vertex     : Integer               ;
    const _x          : TGIS_SDTS_DoubleArray ;
    const _y          : TGIS_SDTS_DoubleArray ;
    const _z          : TGIS_SDTS_DoubleArray ;
    const _offset     : Integer               ;
    const _reverse    : Boolean               ;
    const _dropvertex : Boolean
  ) ;
  var
    i,
    istart,
    iend,
    istep   : Integer ;
  begin
    istart := 0 ;
    iend   := _vertex - 1 ;
    istep  := 1 ;

    if ( _dropvertex and _reverse ) then begin
      istart := _vertex - 2 ;
      iend   := 0 ;
      istep  := -1 ;
    end
    else if ( _dropvertex and not _reverse ) then begin
      istart := 1 ;
      iend   := _vertex - 1 ;
      istep  := 1 ;
    end
    else if ( not _dropvertex and not _reverse ) then begin
      istart := 0 ;
      iend   := _vertex - 1 ;
      istep  := 1 ;
    end
    else if ( not _dropvertex and _reverse ) then begin
      istart := _vertex - 1 ;
      iend   := 0 ;
      istep  := -1 ;
    end ;

    i := istart ;
    while i <> ( iend + istep ) do begin
      arX[nVertices] := _x[_offset+i] ;
      arY[nVertices] := _y[_offset+i] ;
      arZ[nVertices] := _z[_offset+i] ;

      inc( nVertices ) ;
      inc( i, istep ) ;
    end ;
  end ;

  function TGIS_SDTSRawPolygon.AssembleRings
    : Boolean ;
  var
    iEdge           : Integer ;
    bSuccess        : Boolean ;
    panEdgeConsumed : array of Boolean ;
    nRemainingEdges : Integer ;
    nStartNode,
    nLinkNode       : Integer ;
    poEdge          : TGIS_SDTSRawLine ;
    bWorkDone       : Boolean ;
    dfMaxArea       : Double ;
    padfRingArea    : array of Double ;
    iRing, iBiggestRing : Integer ;
    dfSum1, dfSum2  : Double ;
    i, nRingVertices : Integer ;
    padfXRaw : TGIS_SDTS_DoubleArray ;
    padfYRaw : TGIS_SDTS_DoubleArray ;
    padfZRaw : TGIS_SDTS_DoubleArray ;
    panRawRingStart : array of Integer ;
    nRawVertices,
    nRawRings  : Integer ;
  begin
    bSuccess := True ;
    Result   := True ;

    if ( nRings > 0 ) then exit ;

    if ( nEdges = 0 ) then begin
      Result := False ;
      exit ;
    end ;

    SetLength( arRingStart, nEdges ) ;

    nVertices := 0 ;
    for iEdge := 0 to nEdges-1 do
      inc( nVertices, papoEdges[iEdge].nVertices ) ;

    SetLength( arX, nVertices ) ;
    SetLength( arY, nVertices ) ;
    SetLength( arZ, nVertices ) ;

    nVertices       := 0 ;
    nRemainingEdges := nEdges ;
    SetLength( panEdgeConsumed, nEdges ) ;

    while ( nRemainingEdges > 0 ) do begin
      iEdge := 0 ;
      while panEdgeConsumed[iEdge] do inc( iEdge ) ;

      poEdge := papoEdges[iEdge] ;

      arRingStart[nRings] := nVertices ;
      inc( nRings ) ;
      addEdgeToRing( poEdge.nVertices,
                     poEdge.padfX, poEdge.padfY, poEdge.padfZ, 0,
                     FALSE, FALSE ) ;

      panEdgeConsumed[iEdge] := True ;
      dec( nRemainingEdges ) ;

      nStartNode := poEdge.oStartNode.nRecord ;
      nLinkNode  := poEdge.oEndNode.nRecord ;
      bWorkDone  := True ;

      while ( nLinkNode <> nStartNode ) and ( nRemainingEdges > 0 ) and bWorkDone do begin
        bWorkDone := False ;
        for iEdge := 0 to nEdges-1 do begin
          if ( panEdgeConsumed[iEdge] ) then continue ;

          poEdge := papoEdges[iEdge] ;
          if ( poEdge.oStartNode.nRecord = nLinkNode ) then begin
              addEdgeToRing( poEdge.nVertices,
                             poEdge.padfX, poEdge.padfY, poEdge.padfZ, 0,
                             FALSE, True ) ;
              nLinkNode := poEdge.oEndNode.nRecord ;
          end
          else if ( poEdge.oEndNode.nRecord = nLinkNode ) then begin
              addEdgeToRing( poEdge.nVertices,
                             poEdge.padfX, poEdge.padfY, poEdge.padfZ, 0,
                             True, True ) ;
              nLinkNode := poEdge.oStartNode.nRecord ;
          end
          else
              continue ;

          panEdgeConsumed[iEdge] := True ;
          dec( nRemainingEdges ) ;
          bWorkDone := True ;
        end ;
      end ;

    if ( nLinkNode <> nStartNode ) then
      bSuccess := FALSE ;
    end ;

    panEdgeConsumed := nil ;

    if ( not bSuccess ) then begin
      Result := bSuccess ;
      exit ;
    end ;

    dfMaxArea     := 0.0 ;
    iBiggestRing  := -1 ;

    SetLength( padfRingArea, nRings ) ;

    for iRing := 0 to nRings-1 do begin
      dfSum1 := 0.0 ;
      dfSum2 := 0.0 ;

      if ( iRing = nRings - 1 ) then
        nRingVertices := nVertices - arRingStart[iRing]
      else
        nRingVertices := arRingStart[iRing+1] - arRingStart[iRing] ;

      for i := arRingStart[iRing] to arRingStart[iRing] + nRingVertices - 2 do
      begin
        dfSum1 := dfSum1 + arX[i] * arY[i+1]  ;
        dfSum2 := dfSum2 + arY[i] * arX[i+1]  ;
      end ;

      padfRingArea[iRing] := (dfSum1 - dfSum2) / 2 ;

      if ( Abs( padfRingArea[iRing] ) > dfMaxArea ) then begin
        dfMaxArea    := Abs( padfRingArea[iRing] ) ;
        iBiggestRing := iRing ;
      end ;
    end ;
    {$IFDEF GIS_NORECORDS}
      padfXRaw := new array of Double(length(arX));
      padfYRaw := new array of Double(length(arY));
      padfZRaw := new array of Double(length(arZ));
    {$ENDIF}
    {$IFNDEF JAVA}
      padfXRaw := Copy( arX, 0, length( arX ) ) ;
      padfYRaw := Copy( arY, 0, length( arY ) ) ;
      padfZRaw := Copy( arZ, 0, length( arZ ) ) ;
    {$ELSE}
      System.arraycopy(arX, 0, padfXRaw,0, length(arX));
      System.arraycopy(arY, 0, padfYRaw,0, length(arY));
      System.arraycopy(arZ, 0, padfZRaw,0, length(arZ));
    {$ENDIF}
    SetLength( panRawRingStart, length( arRingStart ) ) ;
    for i := 0 to length( arRingStart ) - 1 do
      panRawRingStart[ i ] := arRingStart[ i ] ;

    nRawVertices := nVertices ;
    nRawRings    := nRings ;

    SetLength( arX, nVertices ) ;
    SetLength( arY, nVertices ) ;
    SetLength( arZ, nVertices ) ;
    SetLength( arRingStart, nRawRings ) ;
    nVertices := 0 ;
    nRings    := 0 ;

    if ( iBiggestRing = nRawRings - 1 ) then
      nRingVertices := nRawVertices - panRawRingStart[iBiggestRing]
    else
      nRingVertices := panRawRingStart[iBiggestRing+1] - panRawRingStart[iBiggestRing] ;

    arRingStart[nRings] := 0 ;
    inc( nRings ) ;
    addEdgeToRing( nRingVertices,
                   padfXRaw, padfYRaw, padfZRaw, panRawRingStart[iBiggestRing] ,
                   padfRingArea[iBiggestRing] < 0.0, FALSE
                  ) ;

    for iRing := 0 to nRawRings-1 do begin

      if ( iRing = iBiggestRing ) then continue ;

      if ( iRing = nRawRings - 1 ) then
        nRingVertices := nRawVertices - panRawRingStart[iRing]
      else
        nRingVertices := panRawRingStart[iRing+1] - panRawRingStart[iRing] ;

      arRingStart[nRings] := nVertices ;
      inc( nRings ) ;
      addEdgeToRing( nRingVertices,
                     padfXRaw, padfYRaw, padfZRaw, panRawRingStart[iRing],
                     padfRingArea[iRing] > 0.0, FALSE
                    ) ;
    end ;

    papoEdges := nil ;
    nEdges    := 0 ;

    Result := True ;
  end ;

  function TGIS_SDTSRawPolygon.Read(
    const _record : TGIS_DDFRecord
  ) : Boolean ;
  var
    iField        : Integer ;
    poField       : TGIS_DDFField ;
    pszFieldName  : String ;
  begin
    for iField := 0 to _record.FieldCount-1 do begin

      poField      := _record.GetField( iField ) ;
      pszFieldName := poField.FieldDefn.Name ;

      if pszFieldName = 'POLY' then
        oModId.SetIt( poField, _record )
      else if pszFieldName = 'ATID' then
        ApplyATID( poField, _record ) ;
    end ;

    Result := True ;
  end ;

//==============================================================================
// TGIS_SDTSPolygonReader
//==============================================================================

  constructor TGIS_SDTSPolygonReader.Create ;
  begin
    inherited ;

    bRingsAssembled := False ;

  end ;

  procedure TGIS_SDTSPolygonReader.doDestroy ;
  begin
    inherited ;
  end ;

  procedure TGIS_SDTSPolygonReader.AssembleRings(
    const _transfer : TGIS_SDTSTransfer
  ) ;
  var
    iLineLayer    : Integer ;
    poLineReader  : TGIS_SDTSLineReader ;
    poFeature     : TGIS_SDTSFeature ;
    poPoly        : TGIS_SDTSRawPolygon ;
  begin
    if bRingsAssembled then exit ;

    bRingsAssembled := True ;

    for iLineLayer := 0 to _transfer.LayerCount-1 do begin

      if ( _transfer.GetLayerType( iLineLayer ) <> TGIS_SDTSLayerType.Line ) then Continue ;

      poLineReader := TGIS_SDTSLineReader( _transfer.GetLayerIndexedReader( iLineLayer ) ) ;

      if ( poLineReader = nil ) then Continue ;

      poLineReader.AttachToPolygons( _transfer ) ;
      poLineReader.Rewind ;
    end ;

    Rewind ;
    poFeature := GetNextFeature ;
    while ( poFeature <> nil ) do begin
      poPoly := TGIS_SDTSRawPolygon( poFeature ) ;

      poPoly.AssembleRings ;
      poFeature := GetNextFeature ;
    end ;

    Rewind ;
  end ;

  procedure TGIS_SDTSPolygonReader.Close ;
  begin

  end ;

  function TGIS_SDTSPolygonReader.GetNextPolygon
    : TGIS_SDTSRawPolygon ;
  var
    poRecord      : TGIS_DDFRecord ;
    poRawPolygon  : TGIS_SDTSRawPolygon ;
  begin
    Result := nil ;

    if ( oDDFModule.FP = nil ) then exit ;

    poRecord := oDDFModule.ReadRecord ;

    if ( poRecord = nil ) then exit ;

    poRawPolygon := TGIS_SDTSRawPolygon.Create ;

    if poRawPolygon.Read( poRecord ) then
      Result := poRawPolygon
    else
      FreeObject( poRawPolygon ) ;
  end ;

  function TGIS_SDTSPolygonReader.GetNextRawFeature
    : TGIS_SDTSFeature ;
  begin
    Result := GetNextPolygon ;
  end ;

  function TGIS_SDTSPolygonReader.Open(
    const _filename : String
  ) : Boolean ;
  begin
    Result := oDDFModule.Open( _filename ) ;
  end ;

//==============================================================================
// TGIS_SDTSRasterReader
//==============================================================================

  constructor TGIS_SDTSRasterReader.Create ;
  begin
    inherited ;

    oDDFModule := TGIS_DDFModule.Create ;

    nXSize      := 0 ;
    nYSize      := 0 ;
    nXBlockSize := 0 ;
    nYBlockSize := 0 ;
    nXStart     := 0 ;
    nYStart     := 0 ;

    szINTR      := 'CE' ;
  end ;

  procedure TGIS_SDTSRasterReader.doDestroy ;
  begin
    FreeObject( oDDFModule ) ;

    inherited ;
  end ;

  function TGIS_SDTSRasterReader.GetBlock(
    const _nXOffset, _nYOffset : Integer ;
      var _pData : TBytes
    ) : Boolean ;
  var
    poRecord : TGIS_DDFRecord ;
    nBytesPerValue, iTry : Integer ;
    poCVLS : TGIS_DDFField ;
    i : Integer ;
    {$IFDEF OXYGENE}
     sIntVal : SmallInt ;
     IntVal  : Integer ;
    {$ENDIF}
  begin
    Result := False ;
    nBytesPerValue := GetRasterType ;

    for iTry := 0 to 1 do begin
      poRecord := oDDFModule.ReadRecord ;
      while poRecord <> nil  do begin
        if poRecord.GetIntSubfield( 'CELL', 0, 'ROWI', 0 ) = (_nYOffset + nYStart) then
          break ;

        poRecord := oDDFModule.ReadRecord ;
      end ;

      if ( poRecord = nil ) then begin
        if ( iTry = 0 ) then
          oDDFModule.Rewind()
        else
          exit ;
      end
      else break ;
    end ;

    poCVLS := poRecord.FindField( 'CVLS', 0 ) ;
    if ( poCVLS = nil ) then  exit ;

    if ( poCVLS.DataSize  < nBytesPerValue * nXSize ) or

       ( poCVLS.DataSize > nBytesPerValue * nXSize + 1 ) then
      exit ;

    _pData := Copy( poRecord.Data, poCVLS.Data, nXSize * nBytesPerValue ) ;

    if ( nBytesPerValue = 2 ) then begin
      i := 0 ;
      while i < nXSize * nBytesPerValue do begin
        {$IFDEF OXYGENE}
          sIntVal := BitConverter.ToInt16( _pData, i ) ;
          sIntVal := endian_swap16( sIntVal ) ;
          { TODO -cReview : Review }
//*          sIntVal := readInt16BE( _pData, i ) ;
          _pData[i] := BitConverter.GetBytes( sIntVal )[0] ;
          _pData[i+1] := BitConverter.GetBytes( sIntVal )[1] ;
        {$ELSE}
          PSmallInt( @_pData[i] )^ := endian_swap16( PSmallInt( @_pData[i] )^ ) ;
        {$ENDIF}
        inc( i, 2 ) ;
      end ;
    end
    else begin
      i := 0 ;
      while i < nXSize * nBytesPerValue do begin
        {$IFDEF OXYGENE}
          IntVal := BitConverter.ToInt32( _pData, i ) ;
          IntVal := endian_swap32( IntVal ) ;
          { TODO -cReview : Review }
//*          IntVal := readInt32BE( _pData, i ) ;
          _pData[i] := BitConverter.GetBytes( IntVal )[0] ;
          _pData[i+1] := BitConverter.GetBytes( IntVal )[1] ;
          _pData[i+2] := BitConverter.GetBytes( IntVal )[2] ;
          _pData[i+3] := BitConverter.GetBytes( IntVal )[3] ;
        {$ELSE}
         PInteger( @_pData[i] )^ := endian_swap32( PInteger( @_pData[i] )^ ) ;
        {$ENDIF}
        inc( i, 4 ) ;
      end ;
    end ;

  end ;

  procedure TGIS_SDTSRasterReader.GetExtent(
    var _xmin : Double ;
    var _ymin : Double ;
    var _xmax : Double ;
    var _ymax : Double
  ) ;
  begin
    _xmin := arTransform[0] ;
    _xmax := _xmin + XSize * arTransform[1] ;
    _ymax := arTransform[3] ;
    _ymin := _ymax + YSize * arTransform[5] ;
  end ;

  function TGIS_SDTSRasterReader.GetRasterType
    : Integer ;
  begin
    if      ( szFMT = 'I'     ) or ( szFMT = 'BI32'  ) or
            ( szFMT = 'BUI32' ) or ( szFMT = 'BFP32' ) then
            Result := 4
    else if ( szFMT = 'BI8'   ) or ( szFMT = 'BUI8'  ) then
            Result := 1
    else if ( szFMT = 'BI16'  ) or ( szFMT = 'BUI16' ) then
            Result := 2
    else if ( szFMT = 'R'     ) then
            Result := 8
    else    Result := 0 ;
  end ;

  procedure TGIS_SDTSRasterReader.GetScale(
    var _x : Double ;
    var _y : Double
  ) ;
  begin
    _x := arTransform[ 1 ] ;
    _y := arTransform[ 5 ] ;
  end ;

  function TGIS_SDTSRasterReader.Open(
    const _poCATD    : TGIS_SDTS_CATD ;
    const _poIREF    : TGIS_SDTS_IREF ;
    const _pszModule : String
  ) : Boolean ;
  var
    oLDEF       : TGIS_DDFModule   ;
    poRecord    : TGIS_DDFRecord   ;
    nLDEF_RCID  : Integer     ;
    oRSDF       : TGIS_DDFModule   ;
    dfMin       : TGIS_SDTS_DoubleArray      ;
    dfMax       : TGIS_SDTS_DoubleArray      ;
    dfZ         : TGIS_SDTS_DoubleArray      ;
    pszString   : String ;
    oDDSH       : TGIS_DDFModule   ;
    oDDOM       : TGIS_DDFModule   ;
  begin
    szModule := _pszModule ;

    if IsStringEmpty( _poCATD.GetModuleFilePath('LDEF') ) then begin
      Result := False ;
      exit ;
    end ;

    oLDEF := TGIS_DDFModule.Create ;
    try
      if not oLDEF.Open( _poCATD.GetModuleFilePath('LDEF') ) then begin
        Result := False ;
        exit ;
      end ;

      poRecord := oLDEF.ReadRecord ;
      while ( poRecord <> nil ) do begin
        if poRecord.GetStringSubfield( 'LDEF', 0,'CMNM', 0 ) = szModule then
          break ;

        poRecord := oLDEF.ReadRecord ;
      end ;

      if ( poRecord = nil ) then begin
        Result := False ;
        exit ;
      end ;

      nXSize  := poRecord.GetIntSubfield( 'LDEF', 0, 'NCOL', 0 ) ;
      nYSize  := poRecord.GetIntSubfield( 'LDEF', 0, 'NROW', 0 ) ;
      nXStart := poRecord.GetIntSubfield( 'LDEF', 0, 'SOCI', 0 ) ;
      nYStart := poRecord.GetIntSubfield( 'LDEF', 0, 'SORI', 0 ) ;

      szINTR  := poRecord.GetStringSubfield(  'LDEF', 0, 'INTR', 0 ) ;
      if IsStringEmpty( szINTR ) then
        szINTR := 'CE' ;

      if ( szINTR <> 'CE' ) and ( szINTR <> 'TL' ) then
        szINTR := 'CE' ;

      nLDEF_RCID := poRecord.GetIntSubfield( 'LDEF', 0, 'RCID', 0 ) ;
    finally
      FreeObject( oLDEF ) ;
    end ;

    if IsStringEmpty( _poCATD.GetModuleFilePath('RSDF') ) then begin
      Result := False ;
      exit ;
    end ;

    oRSDF := TGIS_DDFModule.Create ;
    try
      if not oRSDF.Open( _poCATD.GetModuleFilePath('RSDF') ) then begin
        Result := False ;
        exit ;
      end ;

      poRecord := oRSDF.ReadRecord ;
      while poRecord <> nil do begin
        if ( poRecord.GetIntSubfield('LYID',0,'RCID',0) = nLDEF_RCID ) then
          break ;

        poRecord := oRSDF.ReadRecord ;
      end ;

      if ( poRecord = nil ) then begin
        Result := False ;
        exit ;
      end ;

      if poRecord.FindField( 'SADR', 0 ) = nil then begin
        Result := False ;
        exit ;
      end ;

      SetLength( dfMin, 1 ) ;
      SetLength( dfMax, 1 ) ;
      SetLength( dfZ, 1   ) ;
      SetLength( arTransform, 6 ) ;
      _poIREF.GetSADR( poRecord, poRecord.FindField( 'SADR', 0 ), 1,
                       dfMin, dfMax, dfZ ) ;

      arTransform[0] := dfMin[0] ;
      arTransform[1] := _poIREF.dfXRes ;
      arTransform[2] := 0.0 ;
      arTransform[3] := dfMax[0] ;
      arTransform[4] := 0.0 ;
      arTransform[5] := -1 * _poIREF.dfYRes ;

      if szINTR = 'CE' then begin
        arTransform[0] := arTransform[0] - arTransform[1] * 0.5 ;
        arTransform[3] := arTransform[3] - arTransform[5] * 0.5 ;
      end ;

      pszString := poRecord.GetStringSubfield( 'RSDF', 0, 'OBRP', 0 ) ;
      assert( pszString = 'G2' ) ;

      pszString := poRecord.GetStringSubfield( 'RSDF', 0, 'SCOR', 0 ) ;
      assert( pszString = 'TL' ) ;
    finally
      FreeObject( oRSDF ) ;
    end ;

    nXBlockSize := nXSize ;
    nYBlockSize := 1 ;

    oDDSH := TGIS_DDFModule.Create ;
    try
      if IsStringEmpty( _poCATD.GetModuleFilePath('DDSH') ) then begin
        Result := False ;
        exit ;
      end ;

      if not oDDSH.Open( _poCATD.GetModuleFilePath('DDSH') ) then begin
        Result := False ;
        exit ;
      end ;

      poRecord := oDDSH.ReadRecord ;
      while poRecord <> nil do begin
        if poRecord.GetStringSubfield('DDSH',0,'NAME',0) = szModule then
          break ;

        poRecord := oDDSH.ReadRecord ;
      end ;

      if ( poRecord = nil ) then begin
        Result := False ;
        exit ;
      end ;

      if not IsStringEmpty( poRecord.GetStringSubfield('DDSH',0,'FMT',0) ) then
        szFMT := poRecord.GetStringSubfield('DDSH',0,'FMT',0)
      else
        szFMT := 'BUI16' ;

      if not IsStringEmpty( poRecord.GetStringSubfield('DDSH',0,'UNIT',0) ) then
         szUNITS := poRecord.GetStringSubfield('DDSH',0,'UNIT',0)
      else
         szUNITS := 'METERS' ;

      if not IsStringEmpty( poRecord.GetStringSubfield('DDSH',0,'ATLB',0) ) then
        szLabel := poRecord.GetStringSubfield('DDSH',0,'ATLB',0)
      else
        szLabel := '' ;
    finally
      FreeObject( oDDSH ) ;
    end ;

    if not IsStringEmpty( _poCATD.GetModuleFilePath('DDOM') ) then begin
      oDDOM := TGIS_DDFModule.Create ;
      try
        if not oDDOM.Open( _poCATD.GetModuleFilePath('DDOM') ) then begin
          Result := False ;
          exit ;
        end ;

        poRecord := oDDOM.ReadRecord ;
        while poRecord <> nil do begin
          pszString := UpperCase( poRecord.GetStringSubfield('DDOM',0,'DVDF',0) ) ;
          if Pos( 'MIN', pszString ) > StringFirst - 1 then
            dMin := poRecord.GetFloatSubfield('DDOM',0,'DVAL',0)
          else if Pos( 'MAX', pszString ) > StringFirst - 1 then
            dMax := poRecord.GetFloatSubfield('DDOM',0,'DVAL',0)
          else if Pos( 'FILL', pszString ) > StringFirst - 1 then
            dFill := poRecord.GetFloatSubfield('DDOM',0,'DVAL',0)
          else if Pos( 'VOID', pszString ) > StringFirst - 1 then
            dVoid := poRecord.GetFloatSubfield('DDOM',0,'DVAL',0) ;

          poRecord := oDDOM.ReadRecord ;
        end ;

      finally
        FreeObject( oDDOM ) ;
      end ;
    end ;

    Result := oDDFModule.Open( _poCATD.GetModuleFilePath(szModule) ) ;
  end ;

//==============================================================================
// TGIS_SDTSTransfer
//==============================================================================

  constructor TGIS_SDTSTransfer.Create ;
  begin
    inherited ;

    nLayers           := 0 ;
    panLayerCATDEntry := nil ;
    papoLayerReader   := nil ;

    oCATD := TGIS_SDTS_CATD.Create ;
    oIREF := TGIS_SDTS_IREF.Create ;
    oXREF := TGIS_SDTS_XREF.Create ;
  end ;

  procedure TGIS_SDTSTransfer.doDestroy ;
  begin
    Close ;

    FreeObject( oCATD ) ;
    FreeObject( oIREF ) ;
    FreeObject( oXREF ) ;

    inherited ;
  end ;

  function TGIS_SDTSTransfer.FindLayer(
    const _pszModule : String
  ) : Integer ;
  var
    iLayer : Integer ;
  begin
    Result := -1 ;

    for iLayer := 0 to nLayers-1 do
      if _pszModule = oCATD.GetEntryModule( panLayerCATDEntry[iLayer] ) then begin
        Result := iLayer ;
        break ;
      end ;
  end ;

  function TGIS_SDTSTransfer.GetAttr(
    const _poModId : TGIS_SDTSModId
  ) : TGIS_DDFField ;
  var
    poAttrRecord : TGIS_SDTSAttrRecord ;
    ft : TGIS_SDTSLayerType ;
  begin
    poAttrRecord := TGIS_SDTSAttrRecord( GetIndexedFeatureRef( _poModId, ft ) ) ;

    if ( poAttrRecord = nil ) then
      Result := nil
    else
      Result := poAttrRecord.poATTR ;
  end ;

  function TGIS_SDTSTransfer.GetIndexedFeatureRef(
    const _poModId  : TGIS_SDTSModId ;
      var _peType   : TGIS_SDTSLayerType
   ) : TGIS_SDTSFeature ;
  var
    iLayer   : Integer ;
    poReader : TGIS_SDTSIndexedReader ;
  begin
    Result := nil ;
    iLayer := FindLayer( _poModId.sModule ) ;
    if ( iLayer = -1 ) then exit ;

    poReader := GetLayerIndexedReader( iLayer ) ;
    if ( poReader = nil ) then exit ;

    _peType := GetLayerType( iLayer ) ;

    Result := poReader.GetIndexedFeatureRef( _poModId.nRecord ) ;
  end ;

  function TGIS_SDTSTransfer.GetLayerAttrReader(
    const _entry : Integer
  ) : TGIS_SDTSAttrReader ;
  var
    poAttrReader : TGIS_SDTSAttrReader ;
  begin
    if ( _entry < 0 ) or ( _entry >= nLayers ) or
       ( oCATD.GetEntryType( panLayerCATDEntry[_entry] ) <> TGIS_SDTSLayerType.Attr ) then begin

        Result := nil ;
        exit ;
    end ;

    poAttrReader := TGIS_SDTSAttrReader.Create( oIREF ) ;

    if poAttrReader.Open( oCATD.GetEntryFilePath( panLayerCATDEntry[_entry] ) ) then
      Result := poAttrReader
    else begin
      FreeObject( poAttrReader ) ;
      Result := nil ;
    end ;
  end ;

  function TGIS_SDTSTransfer.GetLayerCATDEntry(
    const _entry : Integer
  ) : Integer ;
  begin
    if ( _entry < 0 ) or ( _entry >= nLayers ) then
      Result := -1
    else
      Result := panLayerCATDEntry[_entry] ;
  end ;

  function TGIS_SDTSTransfer.GetLayerIndexedReader(
    const _entry : Integer
  ) : TGIS_SDTSIndexedReader ;
  begin
    Result := nil ;
    if _entry = -1 then exit ;

    if papoLayerReader[_entry] = nil then begin

      case oCATD.GetEntryType( panLayerCATDEntry[_entry] ) of
        TGIS_SDTSLayerType.Attr   : papoLayerReader[_entry] := GetLayerAttrReader( _entry ) ;
        TGIS_SDTSLayerType.Point  : papoLayerReader[_entry] := GetLayerPointReader( _entry ) ;
        TGIS_SDTSLayerType.Line   : papoLayerReader[_entry] := GetLayerLineReader( _entry ) ;
        TGIS_SDTSLayerType.Poly   : papoLayerReader[_entry] := GetLayerPolygonReader( _entry ) ;
      end ;
    end ;

    Result := papoLayerReader[_entry] ;
  end ;

  function TGIS_SDTSTransfer.GetLayerLineReader(
    const _entry : Integer
  ) : TGIS_SDTSLineReader ;
  var
    poLineReader : TGIS_SDTSLineReader ;
  begin
    if ( _entry < 0 ) or ( _entry >= nLayers ) or
       ( oCATD.GetEntryType( panLayerCATDEntry[_entry] ) <> TGIS_SDTSLayerType.Line ) then begin
        Result := nil ;
        exit ;
    end ;

    poLineReader := TGIS_SDTSLineReader.Create( oIREF ) ;

    if poLineReader.Open( oCATD.GetEntryFilePath( panLayerCATDEntry[_entry] ) ) then
      Result := poLineReader
    else begin
      Result := nil ;
      exit ;
    end ;
  end ;

  function TGIS_SDTSTransfer.GetLayerModuleReader(
    const _entry : Integer
  ) : TGIS_DDFModule ;
  var
    poModuleReader : TGIS_DDFModule ;
  begin
    Result := nil ;
    if ( _entry < 0 ) or ( _entry >= nLayers ) then
      exit ;

    poModuleReader := TGIS_DDFModule.Create ;

    if not poModuleReader.Open( oCATD.GetEntryFilePath( panLayerCATDEntry[_entry] ) ) then
      FreeObject( poModuleReader )
    else
      Result := poModuleReader ;
  end ;

  function TGIS_SDTSTransfer.GetLayerPointReader(
    const _entry : Integer
  ) : TGIS_SDTSPointReader ;
  var
    poPointReader : TGIS_SDTSPointReader ;
  begin
    if ( _entry < 0 ) or ( _entry >= nLayers ) or
       ( oCATD.GetEntryType( panLayerCATDEntry[_entry] ) <> TGIS_SDTSLayerType.Point ) then begin
        Result := nil ;
        exit ;
    end ;

    poPointReader := TGIS_SDTSPointReader.Create( oIREF ) ;

    if poPointReader.Open( oCATD.GetEntryFilePath( panLayerCATDEntry[_entry] ) )then
      Result := poPointReader
    else begin
      Result := nil ;
      exit ;
    end ;
  end ;

  function TGIS_SDTSTransfer.GetLayerPolygonReader(
    const _entry : Integer
  ) : TGIS_SDTSPolygonReader ;
  var
    poPolyReader : TGIS_SDTSPolygonReader ;
  begin
    if ( _entry < 0 ) or ( _entry >= nLayers ) or
       ( oCATD.GetEntryType( panLayerCATDEntry[_entry] ) <> TGIS_SDTSLayerType.Poly ) then begin
        Result := nil ;
        exit ;
    end ;

    poPolyReader := TGIS_SDTSPolygonReader.Create ;

    if poPolyReader.Open( oCATD.GetEntryFilePath( panLayerCATDEntry[_entry] ) ) then
      Result := poPolyReader
    else begin
      Result := nil ;
      exit ;
    end ;
  end ;

  function TGIS_SDTSTransfer.GetLayerRasterReader(
    const _entry : Integer
  ) : TGIS_SDTSRasterReader ;
  var
    poRasterReader : TGIS_SDTSRasterReader ;
  begin
    Result := nil ;

    if ( _entry < 0 ) or ( _entry >= nLayers ) or
       ( oCATD.GetEntryType( panLayerCATDEntry[_entry] ) <> TGIS_SDTSLayerType.Raster ) then
      exit ;

    poRasterReader := TGIS_SDTSRasterReader.Create ;

    if not poRasterReader.Open( oCATD, oIREF,
                                oCATD.GetEntryModule(panLayerCATDEntry[_entry] )
                               ) then
      FreeObject( poRasterReader )
    else
      Result := poRasterReader ;
  end ;

  function TGIS_SDTSTransfer.GetLayerType(
    const _entry : Integer
  ) : TGIS_SDTSLayerType ;
  begin
    if ( _entry < 0 ) or ( _entry >= nLayers ) then
      Result := TGIS_SDTSLayerType.Unknown
    else
      Result := oCATD.GetEntryType( panLayerCATDEntry[_entry] ) ;
  end ;

  function TGIS_SDTSTransfer.Open(
    const _filename : String
  ) : Boolean ;

    function prepareCATDPath( const _path : String ) : String ;
    var
      fp, fn, fe : String ;
    begin
      fp := GetFilePath( _path ) ;
      fn := GetFileName( _path ) ;
      fe := GetFileExt ( _path ) ;

      Result := fp + Copy( fn, StringFirst, 4 ) + 'CATD' + fe ;
    end ;

  var
    iCATDLayer : Integer ;
  begin
    Result := False ;
    if not oCATD.Read( prepareCATDPath( _filename ) ) then
      exit ;

    if IsStringEmpty( oCATD.GetModuleFilePath( 'IREF' ) ) then
      raise Exception.Create( 'Cannot find IREF module in transfer.' ) ;

    if not oIREF.Read( oCATD.GetModuleFilePath( 'IREF' ) ) then
      exit ;

    if IsStringEmpty( oCATD.GetModuleFilePath( 'XREF' ) ) then
      raise Exception.Create( 'Cannot find XREF module in transfer.' )
    else if not oXREF.Read( oCATD.GetModuleFilePath( 'XREF' ) ) then
      raise Exception.Create( 'Cannot read XREF module in transfer.' ) ;

    SetLength( panLayerCATDEntry, oCATD.EntryCount ) ;

    for iCATDLayer := 0 to oCATD.EntryCount - 1 do begin
      case oCATD.GetEntryType( iCATDLayer ) of
        TGIS_SDTSLayerType.Point,
        TGIS_SDTSLayerType.Line,
        TGIS_SDTSLayerType.Attr,
        TGIS_SDTSLayerType.Poly,
        TGIS_SDTSLayerType.Raster :
          begin
            panLayerCATDEntry[nLayers] := iCATDLayer ;
            inc( nLayers ) ;
          end ;
      end
    end ;

    SetLength( papoLayerReader, oCATD.EntryCount ) ;

    Result := True ;
  end ;

  procedure TGIS_SDTSTransfer.Close ;
  var
    i : Integer ;
  begin
    for i := 0 to nLayers - 1 do
      if papoLayerReader[ i ] <> nil then
        FreeObject( papoLayerReader[ i ] ) ;

    papoLayerReader   := nil ;
    panLayerCATDEntry := nil ;
    nLayers           := 0 ;
  end ;

//==================================== END =====================================
end.
