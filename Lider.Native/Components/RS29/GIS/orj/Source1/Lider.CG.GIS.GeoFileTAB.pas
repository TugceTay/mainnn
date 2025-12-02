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
  Internal part of TAB-file implementation /only ID and MAP/.

  This unit was partially based on MITAB:

  - Copyright (c) 1998-2000, Daniel Morissette
  - Copyright (c) 1998-2000, Frank Warmerdam
  - Copyright (c) 1999,2000, Stephane Villeneuve

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included
  in all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
  THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
  FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
  DEALINGS IN THE SOFTWARE.

  Sublicensing of this unit is a subject of TatukGIS Developer
  Kernel License
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFileTAB ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFileTAB"'}
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

//{$DEFINE TAB_EXPORT_LABELS_AS_POINT} // Export  all labels as point

interface

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Math,
    System.Types,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypesUI,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoLayerPixel ;
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
{$IFDEF ISLAND}
  uses
    TatukGIS.RTL ;
{$ENDIF}

type
  {#GENDOC:HIDE}
  TGIS_RecordTAB_MAP = {$IFDEF OXYGENE} public {$ENDIF}
                       {$IFNDEF OXYGENE} packed {$ENDIF} {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    RecordType   : Byte    ;
    RecordNumber : Int32 ;
    CenterX      : Int32 ;
    CenterY      : Int32 ;
  end ;
  {$IFNDEF OXYGENE}
    {#GENDOC:HIDE}
    PGIS_RecordTAB_MAP = ^TGIS_RecordTAB_MAP ;
  {$ENDIF}

  /// <summary>
  ///   ID-file index record.
  /// </summary>
  {#GENDOC:HIDE}
  TGIS_RecordTAB_ID = {$IFNDEF OXYGENE} packed {$ENDIF} {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    Offset        : Integer ;
  end ;
  {$IFNDEF OXYGENE}
    {#GENDOC:HIDE}
    PGIS_RecordTAB_ID = ^TGIS_RecordTAB_ID ;
  {$ENDIF}

  // TABPenDef - Pen definition information
  {#GENDOC:HIDE}
  TGIS_TABPenDef = {$IFNDEF OXYGENE} packed {$ENDIF} {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    nRefCount     : Integer ;
    nPixelWidth   : Byte;
    nLinePattern  : Byte;
    nPointWidth   : Integer;
    rgbColor      : TGIS_Color ;
  end ;

  // TABBrushDef - Brush definition information
  {#GENDOC:HIDE}
  TGIS_TABBrushDef = {$IFNDEF OXYGENE} packed {$ENDIF} {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    nRefCount         : Integer;
    nFillPattern      : Byte;
    bTransparentFill  : Byte; // 1 = Transparent
    rgbFGColor        : TGIS_Color;
    rgbBGColor        : TGIS_Color ;
  end ;

  // TABSymbolDef - Brush definition information
  {#GENDOC:HIDE}
  TGIS_TABSymbolDef = {$IFNDEF OXYGENE} packed {$ENDIF} {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    nRefCount         : Integer;
    nSymbolNo         : Word;
    nPointSize        : Word;
    nUnknownValue     : Byte ;
    rgbColor          : TGIS_Color ;
  end ;

  // TABFontDef - Font definition information
  {#GENDOC:HIDE}
  TGIS_TABFontDef = {$IFNDEF OXYGENE} packed {$ENDIF} {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    nRefCount   : Integer ;
    szFontName  : String ;
  end ;

  // Custom symbol definition
  {#GENDOC:HIDE}
  TGIS_CustomSymbolDef = {$IFNDEF OXYGENE} packed {$ENDIF} {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    symbolNo  : Word ;
    pointSize : Word ;
    rotation  : Word ;
    rgbColor  : TGIS_Color ;
    rgbColor2 : TGIS_Color ;
  end ;

  /// <summary>
  ///   Internal TAB file header.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    Only for internal purposes of TGIS_LayerTAB.
  ///    </note>
  /// </remarks>
  TGIS_FileTAB_MAPInternal = {$IFDEF OXYGENE} abstract {$ENDIF}
                             class( TGIS_BufferedFileStream )

    private
      FExtent : TGIS_Extent ;
    public
      /// <summary>
      ///   MAP file property.
      /// </summary>
      VersionNumber                : Word        ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      BlockSize                    : Word        ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      Coord2DistUnits              : Double      ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      XMin                         : Int32     ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      YMin                         : Int32     ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      XMax                         : Int32     ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      YMax                         : Int32     ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      FirstIndexBlock              : Cardinal    ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      FirstGarbageBlock            : Cardinal    ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      FirstToolBlock               : Cardinal    ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      NumPointObjects              : Cardinal    ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      NumLineObjects               : Cardinal    ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      NumRegionObjects             : Cardinal    ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      NumTextObjects               : Cardinal    ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      MaxCoordBufSize              : Cardinal    ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      DistUnitsCode                : Byte        ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      MaxSpIndexDepth              : Byte        ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      CoordPrecision               : Byte        ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      CoordOriginQuadrant          : Byte        ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      ReflectXAxisCoord            : Byte        ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      MaxObjLenArrayId             : Byte        ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      NumPenDefs                   : Byte        ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      NumBrushDefs                 : Byte        ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      NumSymbolDefs                : Byte        ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      NmFontDefs                   : Byte        ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      NumMapToolBlocks             : Word        ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      TabPenDef                    : array of TGIS_TABPenDef;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      TabBrushDef                  : array of TGIS_TABBrushDef;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      TabSymbolDef                 : array of TGIS_TABSymbolDef;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      TabFontDef                   : array of TGIS_TABFontDef;

      /// <summary>
      ///   MAP file property.
      /// </summary>
      ProjId                       : Byte        ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      DatumId                      : Word        ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      EllipsoidId                  : Byte        ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      UnitsId                      : Byte        ;

      /// <summary>
      ///   MAP file property.
      /// </summary>
      XScale                       : Double      ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      YScale                       : Double      ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      XDispl                       : Double      ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      YDispl                       : Double      ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      ProjParams                   : array[0..5] of Double ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      DatumShiftX                  : Double      ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      DatumShiftY                  : Double      ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      DatumShiftZ                  : Double      ;
      /// <summary>
      ///   MAP file property.
      /// </summary>
      DatumParams                  : array[0..4] of Double ;
    public
      /// <summary>
      ///   Read file header.
      /// </summary>
      procedure ReadHeader         ; virtual;

      /// <summary>
      ///   Read pen definitions.
      /// </summary>
      procedure ReadPenDef         ;

      /// <summary>
      ///   Read brush definitions.
      /// </summary>
      procedure ReadBrushDef       ;

      /// <summary>
      ///   Read tool definitions.
      /// </summary>
      procedure ReadAllToolDefs    ; virtual; abstract;
    protected

      /// <summary>
      ///   Destroys an instance.
      /// </summary>
      procedure doDestroy ; override;
    public

      /// <summary>
      ///   Creates an instance on the file given by _path.
      /// </summary>
      /// <param name="_path">
      ///   file to open on
      /// </param>
      /// <param name="_write">
      ///   if True, then file will be opened in write-only mode
      /// </param>
      constructor Create  ( const _path  : String  ;
                            const _write : Boolean
                          ) ; reintroduce ; virtual;
    public
      /// <summary>
      ///   Layer Extent.
      /// </summary>
      property Extent : TGIS_Extent read FExtent ;

  end ;

  /// <summary>
  ///   Encapsulation of TAB-file low-level access.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    Only for internal purposes of TGIS_LayerTAB.
  ///    </note>
  /// </remarks>
  TGIS_FileTAB = class( TGIS_ObjectDisposable )
    private
      tabLines         : TStringList  ; // Tab file contents
      tabExtracted     : TStringList  ; // extracted parameters
                                        // (fields or tiepoints)
      tabExtractedLine : Integer      ; // star of extracted section
      tabPath          : String       ; // path to tab file
      tabType          : Integer      ; // 0 - unknown, 1 native, 2 - DBF,
                                        // 3 - raster, 4 - database
      tabCodePage      : Integer      ; // codepage
      tabFilePath      : String       ; // Path to embedded file
      tabCoordSys      : String       ; // coordinate system String
      tabTableName     : String       ; // table name in database file
    private
      importExtent    : TGIS_Extent     ;
      importTrunc     : Boolean         ;
      importShpType   : TGIS_ShapeType  ;
      importLayerObj  : TGIS_LayerVector;
      importCS        : TGIS_CSCoordinateSystem ;
      importFile      : TObject ;
      importDataFile  : TObject ;
      importRtreeFile : TObject ;
      importEndUid    : Int64 ;
      importShapeNo   : Int64 ;
      importRecNo     : Int64 ;
    protected

      /// <summary>
      ///   Parse coordinate system.
      /// </summary>
      /// <param name="_param">
      ///   coordinate system parameters
      /// </param>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      procedure doCoordSys( const _param : String ;
                            const _layer : TGIS_LayerPixel
                           ) ;
    protected

      /// <summary>
      ///   Prepare tab file fields for export ( change a field type and size ).
      /// </summary>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      /// <param name="_version">
      ///   file version
      /// </param>
      /// <returns>
      ///    number of fields
      /// </returns>
      function  prepareTABFields   ( const _layer : TGIS_LayerVector ;
                                       var _version : Integer
                                   ) : Integer ;

      /// <summary>
      ///   Write tab file structure.
      /// </summary>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      procedure writeTABStructure  ( const _layer : TGIS_LayerVector
                                   ) ;

      /// <summary>
      ///   Export shape callback.
      /// </summary>
      /// <param name="_shp">
      ///   shape to export
      /// </param>
      /// <param name="_abort">
      ///   if True, export will be stopped
      /// </param>
      procedure exportForEach         (       _shp          : TGIS_Shape ;
                                        var   _abort        : Boolean
                                      ) ;
    public

      /// <summary>
      ///   Prerecognize TAB format.
      /// </summary>
      /// <returns>
      ///   0 - Unknown,
      ///   1 - NATIVE (MAP),
      ///   2 - DBF,
      ///   3 - Raster,
      ///   4 - ADO,
      ///   5 - Seamless
      /// </returns>
      function  Prerecognize       : Integer ;

      /// <summary>
      ///   Read field structure form TAB.
      /// </summary>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      procedure ReadFieldsStructure( const _layer : TGIS_LayerVector
                                   ) ;

      /// <summary>
      ///   Read raster structure form TAB.
      /// </summary>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      procedure ReadRaster         ( const _layer : TGIS_LayerPixel
                                   ) ;
    protected

      /// <summary>
      ///   Destroys an instance.
      /// </summary>
      procedure doDestroy ; override;

    public

      /// <summary>
      ///   Creates an instance on the file given by _path.
      /// </summary>
      /// <param name="_path">
      ///   file to open on
      /// </param>
      /// <param name="_write">
      ///   if True, then file will be opened in write-only mode
      /// </param>
      constructor Create ( const _path      : String ;
                           const _write     : Boolean
                         ) ;

      /// <summary>
      ///   Prepare TAB file for write.
      /// </summary>
      /// <param name="_self">
      ///   source layer handle
      /// </param>
      /// <param name="_layer">
      ///   layer handle
      /// </param>
      /// <param name="_extent">
      ///   layer extent
      /// </param>
      /// <param name="_type">
      ///   shape type
      /// </param>
      /// <param name="_scope">
      ///   layer scope
      /// </param>
      /// <param name="_shape">
      ///   if not nil, then only shapes matched _de9im matrix with _shape
      ///   will be found
      /// </param>
      /// <param name="_de9im">
      ///   DE-9IM matrix of comparison
      /// </param>
      /// <param name="_truncated">
      ///   skip truncated shapes
      /// </param>
      procedure   Write  ( const _self      : TGIS_LayerVector ;
                           const _layer     : TGIS_LayerVector ;
                           const _extent    : TGIS_Extent ;
                           const _type      : TGIS_ShapeType ;
                           const _scope     : String ;
                           const _shape     : TGIS_Shape ;
                           const _de9im     : String ;
                           const _truncated : Boolean
                         ) ;
    public

      /// <summary>
      ///   Path to linked file (like raster)
      /// </summary>
      property FilePath   : String  read tabFilePath  ;

      /// <summary>
      ///   Code Page
      /// </summary>
      property CodePage   : Integer read tabCodePage  ;

      /// <summary>
      ///   Database table name.
      /// </summary>
      property TableName  : String  read tabTableName ;
  end ;

  {$IFDEF OXYGENE}
    T_cursorStateTAB_MAP nested in TGIS_FileTAB_MAP = {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
      public
        /// <summary>
        ///   Is cursor in use.
        /// </summary>
        curInUse   : Boolean ;

        /// <summary>
        ///   Current shape file record.
        /// </summary>
        currRecord : TGIS_RecordTAB_MAP ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Encapsulation of MAP-file low-level access.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    Only for internal purposes of TGIS_LayerTAB.
  ///    </note>
  /// </remarks>
  TGIS_FileTAB_MAP = class( TGIS_FileTAB_MAPInternal )
    private
      {$IFDEF OXYGENE}
        cursorState : array of T_cursorStateTAB_MAP ;
      {$ELSE}
        cursorState : array of record
          /// <summary>
          ///   Is cursor in use.
          /// </summary>
          curInUse :Boolean ;

          /// <summary>
          ///   Current shape file record.
          /// </summary>
          currRecord  : TGIS_RecordTAB_MAP ;
        end ;
      {$ENDIF}
    public

      /// <summary>
      ///   Allocate new cursor. Cursor should be closes by calling
      ///   CursorClose().
      /// </summary>
      /// <param name="_cursor">
      ///   cursor to be opened
      /// </param>
      procedure CursorOpen   ( const _cursor : Integer
                             ) ;

      /// <summary>
      ///   Free cursor allocated by CursorOpen().
      /// </summary>
      /// <param name="_cursor">
      ///   cursor allocated by CursorOpen()
      /// </param>
      procedure CursorClose  ( const _cursor : Integer
                             ) ;

      /// <summary>
      ///   Move to next block.
      /// </summary>
      /// <param name="_offset">
      ///   if = 0, then unconditionally; if &gt; 0, then only if _offset
      ///   value cannot fit in a block
      /// </param>
      procedure NextBlock     ( const _offset : Integer
                              ) ;

      /// <summary>
      ///   Move to new position with block skipping.
      /// </summary>
      /// <param name="_offset">
      ///   value to be added to a current position
      /// </param>
      procedure Move          ( const _offset : Integer
                              ) ;

      /// <summary>
      ///   Read value from file.
      /// </summary>
      /// <returns>
      ///   Read value.
      /// </returns>
      function  ReadByte      : Byte     ; reintroduce ;

      /// <summary>
      ///   Read value from file.
      /// </summary>
      /// <returns>
      ///   Read value.
      /// </returns>
      function  ReadLongword  : Cardinal ;

      /// <summary>
      ///   Read value from file.
      /// </summary>
      /// <returns>
      ///   Read value.
      /// </returns>
      function  ReadWord      : Word     ;

      /// <summary>
      ///   Read value from file.
      /// </summary>
      /// <returns>
      ///   Read value.
      /// </returns>
      function  ReadLongint   : Int32  ;

      /// <summary>
      ///   Read value from file.
      /// </summary>
      /// <returns>
      ///   Read value.
      /// </returns>
      function  ReadSmallint  : SmallInt ;

      /// <summary>
      ///   Read record (in the meaning of a shape file) value.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   Retrieved record.
      /// </returns>
      function  ReadRecord ( const _cursor : Integer
                           ) : {$IFDEF OXYGENE}
                                 TGIS_RecordTAB_MAP ;
                               {$ELSE}
                                  PGIS_RecordTAB_MAP ;
                               {$ENDIF}

      /// <summary>
      ///   Read coordinate value.
      /// </summary>
      /// <param name="_compression">
      ///   if True, then expected coordinate is 16 bits, otherwise 32 bits
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   Retrieved coordinate.
      /// </returns>
      function  ReadCoordinate( const _compression : Boolean ;
                                const _cursor : Integer
                              ) : TGIS_Point ;

      /// <summary>
      ///   Read and set center point.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   Center point.
      /// </returns>
      function  ReadCenter ( const _cursor : Integer )   : TPoint ;

      /// <summary>
      ///   Read and set center point from block.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   Center point.
      /// </returns>
      function  ReadCenterFromBlock( const _cursor : Integer ) : TPoint ;

      /// <summary>
      ///   Read extent.
      /// </summary>
      /// <param name="_compression">
      ///   True if is in compressed format
      /// </param>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///   Retrieved extent.
      /// </returns>
      function  ReadExtent    ( const _compression : Boolean ;
                                const _cursor : Integer
                              ) : TGIS_Extent ;

      /// <inheritdoc/>
      procedure ReadAllToolDefs;  override;

    end ;

  {$IFDEF OXYGENE}
    T_cursorStateTAB_ID nested in TGIS_FileTAB_ID = record
      public
        /// <summary>
        ///   Is cursor in use.
        /// </summary>
        curInUse   :  Boolean ;

        /// <summary>
        ///   Current index record.
        /// </summary>
        currRecord : TGIS_RecordTAB_ID ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Encapsulation of ID-file (MAP index) low-level access.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    Only for internal purposes of TGIS_LayerTAB.
  ///    </note>
  /// </remarks>
  TGIS_FileTAB_ID = class( TGIS_BufferedFileStream )
    private // property internal values

        /// <summary>
        ///   Number of records.
        /// </summary>
        FRecordsCount : Integer ;
    private // other internal values
      {$IFDEF OXYGENE}
        cursorState : array of T_cursorStateTAB_ID ;
      {$ELSE}
        cursorState : array of record
          /// <summary>
          ///   Is cursor in use.
          /// </summary>
          curInUse   : Boolean ;

          /// <summary>
          ///   Current index record.
          /// </summary>
          currRecord : TGIS_RecordTAB_ID ;
        end ;
      {$ENDIF}
    public

      /// <summary>
      ///   Allocate new cursor. Cursor should be closes by calling
      ///   CursorClose().
      /// </summary>
      /// <param name="_cursor">
      ///   cursor to be opened
      /// </param>
      procedure CursorOpen   ( const _cursor : Integer
                             ) ;

      /// <summary>
      ///   Free cursor allocated by CursorOpen().
      /// </summary>
      /// <param name="_cursor">
      ///   cursor allocated by CursorOpen()
      /// </param>
      procedure CursorClose  ( const _cursor : Integer
                             ) ;

      /// <summary>
      ///   Read file header.
      /// </summary>
      procedure ReadHeader ;
      {$IFDEF OXYGENE}
        /// <summary>
        ///   Read record given by Uid number.
        /// </summary>
        /// <param name="_uid">
        ///   uid to be retrieved
        /// </param>
        /// <param name="_cursor">
        ///   retrieved record
        /// </param>
        /// <returns>
        ///    record structure
        /// </returns>
        function  ReadRecord ( const _uid     : TGIS_Uid ;
                               const _cursor  : Integer
                             ) : TGIS_RecordTAB_ID ;
      {$ELSE}
        /// <summary>
        ///   Read record given by Uid number.
        /// </summary>
        /// <param name="_uid">
        ///   uid to be retrieved
        /// </param>
        /// <param name="_cursor">
        ///   retrieved record
        /// </param>
        /// <returns>
        ///    record structure
        /// </returns>
        function  ReadRecord ( const _uid     : TGIS_Uid ;
                               const _cursor  : Integer
                             ) : PGIS_RecordTAB_ID ;
      {$ENDIF}
    public

        /// <summary>
        ///   Number of records.
        /// </summary>
        property RecordsCount : Integer read FRecordsCount ;
    public

      /// <summary>
      ///   Creates an instance on the file given by _path.
      /// </summary>
      /// <param name="_path">
      ///   file to open on
      /// </param>
      /// <param name="_write">
      ///   if True, then file will be opened in write-only mode
      /// </param>
      constructor Create( const _path  : String  ;
                          const _write : Boolean
                        ) ; reintroduce ; virtual;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoRTree,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoLayerTAB,
    Lider.CG.GIS.GeoCsMapinfo,
    Lider.CG.GIS.GeoFileDBF,
    Lider.CG.GIS.GeoTransform,
    Lider.CG.GIS.GeoResource ;
{$ENDIF}

const

  // TAB file strings
     TAB_DEFINITION    = 'Definition Table' ;
     TAB_TYPE          = 'Type'       ;
     TAB_NATIVE        = 'NATIVE'     ;
     TAB_DBF           = 'DBF'        ;
     TAB_RASTER        = 'RASTER'     ;
     TAB_ACCESS        = 'ACCESS'     ;
     TAB_TABLE         = 'TABLE'      ;
     TAB_LINKED        = 'LINKED'     ;
     TAB_CHARSET       = 'Charset'    ;
     TAB_COORDSYS      = 'CoordSys'   ;
     TAB_FILE          = 'File'       ;
     TAB_FIELDS        = 'Fields '    ;
     TAB_FIELDS_DEF    = 'FIELDS:'    ;
     TAB_DECIMAL       = 'Decimal'    ;
     TAB_LOGICAL       = 'Logical'    ;
     TAB_SMALLINT      = 'SmallInt'   ;
     TAB_INTEGER       = 'Integer'    ;
     TAB_FLOAT         = 'Float'      ;
     TAB_CHAR          = 'Char'       ;
     TAB_DATE          = 'Date'       ;
     TAB_TIME          = 'Time'       ;
     TAB_DATETIME      = 'DateTime'   ;
     TAB_NEUTRAL       = 'Neutral'    ;
     TAB_WINDOWSLATIN1 = 'WINDOWSLATIN1' ;
     TAB_WINDOWSLATIN2 = 'WINDOWSLATIN2' ;
     TAB_SEAMLESS      = '"\IsSeamless" = "TRUE"' ;

     TAB_TOOL_PEN      = 1 ;
     TAB_TOOL_BRUSH    = 2 ;
     TAB_TOOL_FONT     = 3 ;
     TAB_TOOL_SYMBOL   = 4 ;

     TAB_METADATA_SAVEVERSION9           = 'TGIS_LayerTAB.SaveVersion9' ;
     TAB_METADATA_EXPORT_LABELS_AS_POINT = 'TGIS_LayerTAB.ExportLabelAsPoint' ;

    { If set to YES/NO     then field with original uid (ORIG_ID) will be added
                           during layer import
    }
     TAB_METADATA_WRITEORIGINALUID  = 'TGIS_LayerVector.WriteOriginalUid' ;
     TAB_METADATA_DEFAULT_BOUNDS    = 'TGIS_LayerTAB.DefaultBounds' ;


const
  TAB_MAX_ENTRIES_INDEX_BLOCK = 25 ;
  TAB_MAX_NAME_LENGTH = 10 ; // maximum length of DBF field name

type
  T_MAPFile        = class ;
  T_Object         = class ;
  T_BlockRaw       = class ;
  T_BlockCoord     = class ;
  T_TABFile        = class ;

  T_BoundingRect = record
    XMin: Integer ;
    YMin: Integer ;
    XMax: Integer ;
    YMax: Integer ;
  end ;

  // TAB access modes.
  T_TABAccess = ( Read       ,
                  Write      ,
                  ReadWrite
                ) ;

  // TAB feature.
  T_Feature = class
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      FMapInfoType    : Integer           ;
      FFeatureClass   : Integer           ;
      FExtent         : TGIS_Extent       ;
      FBoundingRect   : T_BoundingRect    ;
      FComprOrg       : TPoint            ;
      FShape          : TGIS_Shape        ;
      FPenDefIndex    : Integer           ;
      FPenDef         : TGIS_TABPenDef    ;
      FBrushDefIndex  : Integer           ;
      FBrushDef       : TGIS_TABBrushDef  ;
      FSymbolDefIndex : Integer           ;
      FSymbolDef      : TGIS_TABSymbolDef ;
      FFontDefIndex   : Integer           ;
      FFontDef        : TGIS_TABFontDef   ;

    public
      constructor Create            ; virtual;

      procedure SetExtent           ( const _ext            : TGIS_Extent
                                    ) ;
      procedure Write               ( const _mapfile        : T_MAPFile        ;
                                      const _object         : T_Object
                                    ) ; virtual;
      function  Validate            ( const _mapfile        : T_MAPFile
                                    ) : Integer ; virtual;
      procedure UpdateBoundingRect  ( const _mapfile        : T_MAPFile
                                    ) ; virtual;
      function  ValidateCoordType   ( const _mapfile        : T_MAPFile
                                    ) : Boolean ;
      procedure SetBrushFGColor     ( const _color          : TGIS_Color
                                    ) ;
      procedure SetBrushBGColor     ( const _color          : TGIS_Color
                                    ) ;
      procedure SetBrushPattern     ( const _val            : Byte
                                    ) ;
      procedure SetBrushTransparent ( const _val            : Byte
                                    ) ;
      procedure SetPenColor         ( const _color          : TGIS_Color
                                    ) ;
      procedure SetPenPattern       ( const _val            : Byte
                                    ) ;
      procedure SetPenWidth         ( const _width          : Integer
                                    ) ;
      procedure SetSymbolNo         ( const _no             : Word
                                    ) ;
      procedure SetSymbolSize       ( const _size           : Word
                                    ) ;
      procedure SetSymbolColor      ( const _color          : TGIS_Color
                                    ) ;
    public
      property FeatureClass : Integer read FFeatureClass ;
  end ;

  // TAB point feature.
  T_FeaturePoint = class( T_Feature )
    public
      constructor Create ; override;
      procedure   Write              ( const _mapfile       : T_MAPFile       ;
                                       const _object        : T_Object
                                     ) ; override;
      function    Validate           ( const _mapfile       : T_MAPFile
                                     ) : Integer ; override;
  end ;

  // TAB text feature.
  T_FeatureText = class( T_Feature )
    protected
      FText           : TBytes     ;
      FAngle          : Double     ;
      FHeight         : Double     ;
      FWidth          : Double     ;
      FLineEnd        : TGIS_Point ;
      FLineEndSet     : Boolean    ;
      FForeground     : TGIS_Color ;
      FBackground     : TGIS_Color ;
      FTextAlignment  : SmallInt   ;
      FFontStyle      : SmallInt   ;
    public
      constructor Create             ; override;

      procedure Write                ( const _mapfile       : T_MAPFile        ;
                                       const _object        : T_Object
                                     ) ; override;
      function  Validate             ( const _mapfile       : T_MAPFile
                                     ) : Integer ; override;
      function  GetTextLineEndPoint  : TGIS_Point ;
      procedure SetText              ( const _angle         : Double           ;
                                       const _height        : Double           ;
                                       const _width         : Double           ;
                                       const _fgcolor       : TGIS_Color       ;
                                       const _bgcolor       : TGIS_Color       ;
                                       const _justification : Integer          ;
                                       const _spacing       : Integer          ;
                                       const _linetype      : Integer          ;
                                       const _feature       : TGIS_Shape
                                     ) ;
      procedure UpdateBoundingRect   ( const _mapfile       : T_MAPFile
                                     ) ; override;
  end ;

  // TAB multipoint feature.
  T_FeatureMultiPoint = class( T_Feature )
    private
      FCenterIsSet  : Boolean ;
      FCenter       : TGIS_Point ;
    public
      constructor Create             ; override;
      procedure Write                ( const _mapfile       : T_MAPFile        ;
                                       const _object        : T_Object
                                     ) ; override;
      function  Validate             ( const _mapfile       : T_MAPFile
                                     ) : Integer ; override;
      function  GetCenter            ( var _ptg             : TGIS_Point
                                     ) : Integer ;
  end ;

  // Coordinates header.
  T_Section = {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    FVertices         : Integer        ;
    FHoles            : Word           ;
    FBoundingRect     : T_BoundingRect ;
    FDataOffset       : Integer        ;
    FVertexOffset     : Integer        ;
  end ;
  T_SectionArray = array of T_Section ;

  // TAB polyline feature.
  T_FeaturePolyline = class( T_Feature )
    private
      bCenterIsSet         : Boolean ;
      dCenterX             : Double  ;
      dCenterY             : Double  ;
      bSmooth              : Boolean ;
      bWriteLineAsPolyline : Boolean ;
    public
      constructor Create ; override;
      function Validate              ( const _mapfile       : T_MAPFile
                                     ) : Integer ; override;
      procedure Write                ( const _mapfile       : T_MAPFile        ;
                                       const _object        : T_Object
                                     ) ; override;
      function GetCenter             ( var _ptg             : TGIS_Point
                                     ) : Integer ;
  end ;

  // TAB region feature.
  T_FeatureRegion = class( T_Feature )
    private
      bSmooth      : Boolean ;
    public
      constructor Create ; override;
      function  Validate             ( const _mapfile        : T_MAPFile
                                     ) : Integer ; override;
      procedure Write                ( const _mapfile        : T_MAPFile       ;
                                       const _object         : T_Object
                                     ) ; override;
      function  ComputeNumRings      ( var   _sections       : T_SectionArray  ;
                                       const _mapfile        : T_MAPFile
                                     ) : Integer;
      procedure AppendSections       ( const _shape          : TGIS_Shape      ;
                                       var   _sections       : T_SectionArray  ;
                                       const _mapfile        : T_MAPFile ;
                                       var   _last_ring      : Integer
                                     ) ;
      function  GetCenter            ( var   _ptg            : TGIS_Point
                                     ) : Integer ;
  end ;

//ready

  // Raw binary block.
  T_BlockRaw = class( TGIS_Object )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      nBlockType      : Integer ;
      pabyBuf         : array of Byte ;
      nBlockSize      : Integer ;
      nSizeUsed       : Integer ;
      bHardBlockSize  : Boolean ;
      nFileOffset     : Integer ;
      nCurPos         : Integer ;
      nFirstBlockPtr  : Integer ;
      bModified       : Boolean ;
      fp              : TGIS_BufferedFileStream ;
      eAccess         : T_TABAccess ;
    protected
      procedure doDestroy                ; override;
    public
      constructor Create                 ( const _eAccessMode    : T_TABAccess  ;
                                           const _bHardBlockSize : Boolean
                                         ) ; virtual;
      function InitNewBlock              ( const _fpSrc       : TGIS_BufferedFileStream ;
                                           const _nBlockSize  : Integer ;
                                           const _nFileOffset : Integer
                                         ) : Integer ; virtual;
      procedure GotoByteInBlock          ( const _nOffset : Integer
                                         );
      procedure WriteBytes               ( const _nBytesToWrite : Integer ;
                                           const _pabySrcBuf    : array of Byte
                                         ) ; virtual;
      procedure ReadBytes                ( const _numBytes   : Integer;
                                           const _offset     : Integer ;
                                           var _pabyDstBuf : array of Byte
                                         ) ; virtual;
      procedure WriteInt32               ( const _n32Value      : Integer
                                         ) ;
      function  ReadInt32                 : Integer ;
      procedure WriteInt16               ( const _n16Value      : SmallInt
                                         ) ;
      function  ReadInt16                 : SmallInt ;
      procedure WriteDouble              ( const _dValue        : Double
                                         ) ;
      procedure WriteZeros               ( const _nBytesToWrite : Integer
                                         ) ;
      procedure WriteByte                ( const _byValue       : Byte
                                         ) ;
      procedure GotoByteInFile           ( const _nOffset            : Integer ;
                                           const _bForceReadFromFile : Boolean ;
                                           const _bOffsetIsEndOfData : Boolean
                                         ) ;
      function  TABCreateMAPBlockFromFile( const _fpSrc      : TGIS_BufferedFileStream ;
                                           const _nOffset    : Integer ;
                                           const _nSize      : Integer ;
                                           const _bHardBlockSize : Boolean
                                         ) :  T_BlockRaw ;
      procedure ReadFromFile             ( const _fpSrc      : TGIS_BufferedFileStream ;
                                           const _nOffset    : Integer ;
                                           const _nSize      : Integer
                                         ) ;
      procedure InitBlockFromData        ( const _pabyBuf    : array of Byte ;
                                           const _nBlockSize : Integer ;
                                           const _nSizeUsed  : Integer ;
                                           const _bMakeCopy  : Boolean ;
                                           const _fpSrc      : TGIS_BufferedFileStream ;
                                           const _nOffset    : Integer
                                         ) ; virtual;
      function  GetCurAddress : Integer ;

      // Save block of data to the file.
      procedure CommitToFile ; virtual;
      function  GetBlockClass : Integer ; virtual;
      function  GetNumUnusedBytes : Integer ;
      function  GetFirstUnusedByteOffset : Integer ;
  end ;

  // Projection info.
  T_ProjInfo = record
    nProjId       : Byte ;
    nEllipsoidId  : Byte ;
    nUnitsId      : Byte ;
    adProjParams  : array of Double ;
    nDatumId      : Word ;
    dDatumShiftX  : Double ;
    dDatumShiftY  : Double ;
    dDatumShiftZ  : Double ;
    adDatumParams : array of Double ;
    nAffineFlag   : Byte ;
    nAffineUnits  : Byte ;
    dAffineParamA : Double ;
    dAffineParamB : Double ;
    dAffineParamC : Double ;
    dAffineParamD : Double ;
    dAffineParamE : Double ;
    dAffineParamF : Double ;
  end ;

  // MAP header block.
  T_BlockHeader = class( T_BlockRaw )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      sProj               : T_ProjInfo ;
    public
      nMAPVersionNumber   : Word ;
      nBlockSize          : Word ;
      dCoordsys2DistUnits : Double ;
      hnXMin              : Integer ;
      hnYMin              : Integer ;
      hnXMax              : Integer ;
      hnYMax              : Integer ;
      bBoundsOverflow     : Boolean ;
      nFirstIndexBlock    : Integer ;
      nFirstGarbageBlock  : Integer ;
      nFirstToolBlock     : Integer ;
      numPointObjects     : Integer ;
      numLineObjects      : Integer ;
      numRegionObjects    : Integer ;
      numTextObjects      : Integer ;
      nMaxCoordBufSize    : Integer ;
      nDistUnitsCode      : Byte ;
      nMaxSpIndexDepth    : Byte ;
      nCoordPrecision     : Byte ;
      nCoordOriginQuadrant: Byte ;
      nReflectXAxisCoord  : Byte ;
      nMaxObjLenArrayId   : Byte ;
      numPenDefs          : Byte ;
      numBrushDefs        : Byte ;
      numSymbolDefs       : Byte ;
      numFontDefs         : Byte ;
      numMapToolBlocks    : Word ;
      XScale              : Double ;
      YScale              : Double ;
      XDispl              : Double ;
      YDispl              : Double ;
    public
      constructor Create              ( const _eAccessMode    : T_TABAccess ;
                                        const _bHardBlockSize : Boolean
                                       ) ; override;
      function InitNewBlock           ( const _fpSrc       : TGIS_BufferedFileStream ;
                                        const _nBlockSize  : Integer ;
                                        const _nFileOffset : Integer
                                       ) : Integer ; override;
      procedure SetCoordsysBounds     ( var _ext           : TGIS_Extent
                                       ) ;
      function  Int2Coordsys          ( const _pt : TPoint
                                       ) : TGIS_Point ;
      function  Coordsys2Int          ( const _ptg         : TGIS_Point ;
                                        const _bIgnoreOverflow : Boolean
                                       ) : TPoint ;
      function GetMapObjectSize       ( const _nObjType    : Integer
                                       ) : Integer ;
      function MapObjectUsesCoordBlock( const _nObjType    : Integer
                                       ) : Boolean ;

      // Save block of data to the file.
      procedure CommitToFile ; override;
      procedure InitBlockFromData( const _pabyBuf    : array of Byte ;
                                   const _nBlockSize : Integer ;
                                   const _nSizeUsed  : Integer ;
                                   const _bMakeCopy  : Boolean ;
                                   const _fpSrc      : TGIS_BufferedFileStream ;
                                   const _nOffset    : Integer
                                  ) ; override;
      function  GetBlockClass : Integer ; override;
  end ;

  // Block reference.
  T_BlockRef = class
    nBlockPtr : Integer    ;
    psNext    : T_BlockRef ;

    public
      // Create an instance
      constructor Create ;
  end ;

  // Binary block manager.
  T_BinBlockManager = class
    protected
      nBlockSize          : Integer ;
      nLastAllocatedBlock : Integer ;
      psGarbageBlocks     : T_BlockRef ;
    public
      constructor Create( const _nBlockSize : Integer
                        ) ;
      procedure Reset ;
      function  AllocNewBlock : Integer ;
      function  GetFirstGarbageBlock : Integer ;
      function  PopGarbageBlock : Integer ;
  end ;

  // MAP index entry.
  T_MAPIndexEntry = {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
    BoundingRect : T_BoundingRect ;
    nBlockPtr    : Integer ;
  end ;

  // MAP index block.
  T_BlockIndex = class( T_BlockRaw )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      poBlockManagerRef : T_BinBlockManager ;
      poCurChild        : T_BlockIndex ;
      numEntries        : Integer ;
      nCurChildIndex    : Integer ;
      asEntries         : array [0..TAB_MAX_ENTRIES_INDEX_BLOCK-1] of T_MAPIndexEntry ;
      poParentRef       : T_BlockIndex ;
      inMinX             : Integer ;
      inMinY             : Integer ;
      inMaxX             : Integer ;
      inMaxY             : Integer ;
    protected
      procedure doDestroy             ; override;
    public
      constructor Create              ( const _eAccessMode    : T_TABAccess ;
                                        const _bHardBlockSize : Boolean
                                      ) ; override;
      procedure AddEntry              ( const _br : T_BoundingRect ;
                                        const _nBlockPtr : Integer ;
                                        const _bAddInThisNodeOnly : Boolean
                                       ) ;
      function  GetBoundingRect       : T_BoundingRect ;
      function ChooseSubEntryForInsert( const _br : T_BoundingRect
                                       ) : Integer ;
      procedure SetParentRef          ( const _poParent   : T_BlockIndex
                                       ) ;
      procedure SetMAPBlockManagerRef ( const _poBlockMgr : T_BinBlockManager
                                       ) ;
      procedure SplitRootNode         ( const _new_entry : T_BoundingRect
                                       ) ;
      procedure SplitNode             ( const _new_entry : T_BoundingRect
                                       ) ;
      procedure SetCurChildRef        ( const _poChild      : T_BlockIndex ;
                                        const _nChildIndex  : Integer
                                       ) ;
      procedure InsertEntry           ( const _br : T_BoundingRect ;
                                        _nBlockPtr: Integer
                                       );
      procedure UpdateCurChildMBR     ( const _br : T_BoundingRect ;
                                        const _nBlockPtr : Integer
                                       ) ;
      procedure WriteNextEntry        ( const _psEntry : T_MAPIndexEntry
                                       ) ;
      function  InitNewBlock          ( const _fpSrc       : TGIS_BufferedFileStream ;
                                        const _nBlockSize  : Integer ;
                                        const _nFileOffset : Integer
                                       ) : Integer ; override;
      procedure PickSeedsForSplit     ( const _pasEntries : array of T_MAPIndexEntry ;
                                        const _numEntries : Integer ;
                                        const _nSrcCurChildIndex : Integer ;
                                        const _new_entry : T_BoundingRect ;
                                          var _nSeed1, _nSeed2 : Integer
                                        ) ;
      function  ComputeAreaDiff       (  const _node, _entry : T_BoundingRect
                                       ) : Double ;
      function  GetCurMaxDepth : Integer ;
      function  GetNumFreeEntries : Integer ;
      procedure RecomputeMBR ;

      // Save block of data to the file.
      procedure CommitToFile ; override;
      procedure InitBlockFromData( const _pabyBuf    : array of Byte ;
                                   const _nBlockSize : Integer ;
                                   const _nSizeUsed  : Integer ;
                                   const _bMakeCopy  : Boolean ;
                                   const _fpSrc      : TGIS_BufferedFileStream ;
                                   const _nOffset    : Integer
                                  ) ; override;
      function  GetBlockClass : Integer ; override;
      procedure ReadAllEntries ;
      procedure ReadNextEntry( var _psEntry : T_MAPIndexEntry ) ;
  end ;

  // MAP object block.
  T_BlockObject = class( T_BlockRaw )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      FBoundingRect     : T_BoundingRect ;
      nCurObjectOffset  : Integer ;
      nCurObjectId      : Integer ;
      nCurObjectType    : Integer ;
      numDataBytes      : Integer ;
      nCenterX          : Integer ;
      nCenterY          : Integer ;
      nFirstCoordBlock  : Integer ;
      nLastCoordBlock   : Integer ;
    protected
      procedure doDestroy       ; override;
    public
      constructor Create        ( const _eAccessMode    : T_TABAccess ;
                                  const _bHardBlockSize : Boolean
                                 ) ; override;
      procedure CommitNewObject    ( const _object : T_Object
                                   ) ;
      function  PrepareNewObject   ( const _object : T_Object
                                   ) : Integer ;
      procedure UpdateBoundingRect ( const _pt : TPoint
                                   ) ;
      procedure AddCoordBlockRef   ( const _nCoordBlockAddress : Integer
                                   ) ;
      function InitNewBlock        ( const _fpSrc       : TGIS_BufferedFileStream ;
                                     const _nBlockSize  : Integer ;
                                     const _nFileOffset : Integer
                                   ) : Integer ; override;
      procedure WriteIntCoord      ( const _pt          : TPoint ;
                                     const _compres     : Boolean
                                   ) ;

      // Save block of data to the file.
      procedure CommitToFile         ; override;
      procedure InitBlockFromData  ( const _pabyBuf    : array of Byte ;
                                     const _nBlockSize : Integer ;
                                     const _nSizeUsed  : Integer ;
                                     const _bMakeCopy  : Boolean ;
                                     const _fpSrc      : TGIS_BufferedFileStream ;
                                     const _nOffset    : Integer
                                   ) ; override;
      function  GetBlockClass        : Integer ; override;
      procedure WriteIntMBRCoord   ( const _br : T_BoundingRect ;
                                     const _compres : Boolean
                                   ) ;
  end ;

  // MAP coordinates block.
  T_BlockCoord = class( T_BlockRaw )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      numBlocksInChain  : Integer ;
      poBlockManagerRef : T_BinBlockManager ;
      nNextCoordBlock   : Integer ;
      nFeatureDataSize  : Integer ;
      ComprOrg          : TPoint  ;
      nFeatureXMin      : Integer ;
      nFeatureYMin      : Integer ;
      nFeatureXMax      : Integer ;
      nFeatureYMax      : Integer ;
      nMinX             : Integer ;
      nMinY             : Integer ;
      nMaxX             : Integer ;
      nMaxY             : Integer ;
      numDataBytes      : Integer ;
      nTotalDataSize    : Integer ;
    public
      constructor Create             ( const _eAccessMode    : T_TABAccess ;
                                       const _bHardBlockSize : Boolean
                                      )  ; override;
      procedure SetMAPBlockManagerRef( const _poBlockMgr : T_BinBlockManager
                                      ) ;
      procedure SetNextCoordBlock    ( const _nNextCoordBlockAddress : Integer
                                      ) ;
      procedure StartNewFeature ;
      procedure SetComprCoordOrigin  ( const _pt          : TPoint
                                      ) ;
      procedure WriteIntCoord        ( const _pt          : TPoint ;
                                       const _compres     : Boolean
                                      ) ;
      procedure WriteCoordSecHdrs    ( const _bV450Hdr    : Boolean ;
                                       const _numSections : Integer ;
                                       const _pasHdrs     : T_SectionArray ;
                                       const _compres     : Boolean
                                      ) ;
      procedure WriteBytes           ( const _nBytesToWrite : Integer ;
                                       const _pBuf          : array of Byte
                                      ) ; override;
      function InitNewBlock          ( const _fpSrc       : TGIS_BufferedFileStream ;
                                       const _nBlockSize  : Integer ;
                                       const _nFileOffset : Integer
                                      ) : Integer; override;

      // Save block of data to the file.
      procedure CommitToFile ; override;
      procedure InitBlockFromData( const _pabyBuf    : array of Byte ;
                                   const _nBlockSize : Integer ;
                                   const _nSizeUsed  : Integer ;
                                   const _bMakeCopy  : Boolean ;
                                   const _fpSrc      : TGIS_BufferedFileStream ;
                                   const _nOffset    : Integer
                                  ) ; override;
      procedure ReadBytes         ( const _numBytes   : Integer;
                                    const _offset     : Integer ;
                                    var _pabyDstBuf : array of Byte
                                   ) ; override;
      function  GetBlockClass : Integer ; override;
  end ;

  // MAP tool block.
  T_BlockTool = class( T_BlockRaw )
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      numDataBytes      : Integer ;
      nNextToolBlock    : Integer ;
      numBlocksInChain  : Integer ;
      poBlockManagerRef : T_BinBlockManager ;
    public
      constructor Create             ( const _eAccessMode    : T_TABAccess ;
                                       const _bHardBlockSize : Boolean
                                      )  ; override;
      procedure SetMAPBlockManagerRef( const _poBlockMgr : T_BinBlockManager
                                      ) ;
      procedure CheckAvailableSpace  ( const _nToolType : Integer
                                      ) ;
      procedure SetNextToolBlock     ( const _nNextToolBlockAddress : Integer
                                      ) ;
      function InitNewBlock          ( const _fpSrc       : TGIS_BufferedFileStream ;
                                       const _nBlockSize  : Integer ;
                                       const _nFileOffset : Integer
                                      ): Integer; override;

      // Save block of data to the file.
      procedure CommitToFile ; override;
      procedure WriteBytes           ( const _nBytesToWrite : Integer ;
                                       const _pBuf          : array of Byte
                                      ) ; override;
      procedure InitBlockFromData( const _pabyBuf    : array of Byte ;
                                   const _nBlockSize : Integer ;
                                   const _nSizeUsed  : Integer ;
                                   const _bMakeCopy  : Boolean ;
                                   const _fpSrc      : TGIS_BufferedFileStream ;
                                   const _nOffset    : Integer
                                  ) ; override;
      procedure ReadBytes            ( const _numBytes   : Integer;
                                     const _offset     : Integer ;
                                     var _pabyDstBuf : array of Byte
                                    ) ; override;
      function  GetBlockClass : Integer ; override;

  end ;

  // Tool definition table.
  T_ToolDefTable = class
    {$IFDEF OXYGENE} unit {$ELSE} protected {$ENDIF}
      numPen              : Integer ;
      numAllocatedPen     : Integer ;
      numBrushes          : Integer ;
      numSymbols          : Integer ;
      numAllocatedBrushes : Integer ;
      numFonts            : Integer ;
      numAllocatedFonts   : Integer ;
      numAllocatedSymbols : Integer ;
      papsPen             : array of TGIS_TABPenDef ;
      papsBrush           : array of TGIS_TABBrushDef ;
      papsSymbol          : array of TGIS_TABSymbolDef ;
      papsFont            : array of TGIS_TABFontDef ;
    public
      constructor Create ;
      procedure WriteAllToolDefs( const _poBlock        : T_BlockTool
                                 ) ;
      function  AddPenDefRef    ( const _poNewPenDef    : TGIS_TABPenDef
                                 ) : Integer ;
      function  AddBrushDefRef  ( const _poNewBrushDef  : TGIS_TABBrushDef
                                 ) : Integer ;
      function  AddSymbolDefRef ( const _poNewSymbolDef : TGIS_TABSymbolDef
                                 ) : Integer ;
      function  AddFontDefRef   ( const _poNewFontDef   : TGIS_TABFontDef
                                 ) : Integer ;
  end ;

  // MAP info file.
  T_IMapInfoFile = class( TGIS_LayerVector )
    protected
      poCurFeature  : T_Feature ;
    protected

      // Destructor.
      procedure doDestroy   ; override;
    public
      // Constructor.
      constructor Create    ; override;

      // Open a file.
      function OpenEx       ( const _pszFname : String
                            ) : Integer ; virtual;

      // Close a file.
      procedure Close       ; virtual;

      // Set bounds.
      procedure SetBounds   ( var _ext         : TGIS_Extent
                            ) ; virtual;

      // Set feature.
      procedure SetFeature  ( const _poFeature : T_Feature
                            ) ; virtual;

      // Create feature.
      function CreateFeature( const _feature  : TGIS_Shape
                            ) : T_Feature ;
  end ;

  // ID file.
  T_IDFile = class( TGIS_BufferedFileStream )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      pszFname    : String ;
      fp          : TGIS_BufferedFileStream ;
      poIDBlock   : T_BlockRaw ;
      nBlockSize  : Integer ;
      nMaxId      : Integer ;
      eAccessMode : T_TABAccess ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create  ( const _path    : String              ;
                            const _layer   : TGIS_LayerVector    ;
                            const _mode    : TGIS_StreamMode     ;
                            const _onread  : TGIS_ReadWriteEvent ;
                            const _onwrite : TGIS_ReadWriteEvent
                          ) ; reintroduce ;
      procedure SetObjPtr ( const _nObjId  : Integer ;
                            const _nObjPtr : Integer
                          ) ;
      procedure Close     ; reintroduce ;
  end ;

  // MAP object header.
  T_Object = class
    public
      nType : Byte ;
      nId   : Integer ;
      FBoundingRect : T_BoundingRect ;
    public

      // Create new object.
      class function NewObj      ( const _nNewObjType : Byte ;
                                   const _nId         : Integer
                                 ) : T_Object ; virtual;
      procedure WriteObj         ( const _block       : T_BlockObject
                                 ) ; virtual;
      procedure SetBoundingRect  ( const _br : T_BoundingRect
                                 ) ;
      procedure WriteObjTypeAndId( const _poObjBlock  : T_BlockObject
                                 ) ;
      function  IsCompressedType : Boolean ;
  end ;

  // MAP object header with coordinates.
  T_MAPObjectWithCoord = class( T_Object )
    public
      nCoordBlockPtr : Integer ;
      nCoordDataSize : Cardinal ;
  end ;

  // MAP object none.
  T_MAPObjNone = class( T_Object )
    public
      procedure WriteObj( const _block : T_BlockObject
                         ) ; override;
  end ;

  // MAP object point.
  T_MAPObjPoint = class( T_Object )
    public
      Pos       : TPoint  ;
      nSymbolId : Byte ;
    procedure WriteObj( const _block : T_BlockObject
                       ) ; override;
  end ;

  // MAP object font point.
  T_MAPObjFontPoint = class( T_Object )
    public
      procedure WriteObj( const _block : T_BlockObject
                         ) ; override;
  end ;

  // Custom point
  T_MAPObjCustomPoint = class( T_Object )
    public
      procedure WriteObj( const _block : T_BlockObject
                         ) ; override;
  end ;

  // Line object
  T_MAPObjLine = class( T_Object )
    public
      A     : TPoint ;  // start of line
      B     : TPoint ;  // end of line
      PenId : Byte   ;  // line pen
    public
      procedure WriteObj( const _poObjBlock : T_BlockObject
                        ) ; override;
  end ;

  // Polyline object
  T_MAPObjPLine = class( T_MAPObjectWithCoord )
    public
      LineSections : Word    ;
      LabelPos     : TPoint  ;
      ComprOrg     : TPoint  ;
      PenId        : Byte    ;
      BrushId      : Byte    ;
      Smooth       : Boolean ;
    public
      procedure WriteObj( const _poObjBlock : T_BlockObject
                        ) ; override;
  end ;

  // MAP object arc.
  T_MAPObjArc = class( T_Object )
    public
      procedure WriteObj( const _block : T_BlockObject
                         ) ; override;
  end ;

  // MAP object rectangle ellipse.
  T_MAPObjRectEllipse = class( T_Object )
    public
      procedure WriteObj( const _block : T_BlockObject
                         ) ; override;
  end ;

  // MAP object text.
  T_MAPObjText = class( T_MAPObjectWithCoord )
    public
      nTextAlignment : SmallInt ;
      nAngle         : Integer ;
      nFontStyle     : SmallInt ;
      nFGColorR      : Byte ;
      nFGColorG      : Byte ;
      nFGColorB      : Byte ;
      nBGColorR      : Byte ;
      nBGColorG      : Byte ;
      nBGColorB      : Byte ;
      nLineEndX      : Integer ;
      nLineEndY      : Integer ;
      nHeight        : Integer ;
      nFontId        : Byte ;
      nPenId         : Byte ;

    procedure WriteObj( const _poObjBlock : T_BlockObject
                       ) ; override;
  end ;

  // MAP object multipoint.
  TABMAPObjMultiPoint = class( T_MAPObjectWithCoord )
    public
      nNumPoints : Integer ;
      ComprOrg   : TPoint  ;
      nSymbolId  : Byte    ;
      nLabelX    : Integer ;
      nLabelY    : Integer ;

      procedure WriteObj( const _poObjBlock : T_BlockObject
                         ) ; override;
  end ;

  // MAP object collection.
  T_MAPObjCollection = class( T_Object )
    public
      procedure WriteObj( const _block : T_BlockObject
                         ) ; override;
  end ;

  // MAP file.
  T_MAPFile = class( TGIS_BufferedFileStream )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      nMinTABVersion : Integer ;
      pszFname       : String  ;
      poHeader       : T_BlockHeader ;
      poIdIndex      : T_IDFile ;
      poSpIndex      : T_BlockIndex ;
      poToolDefTable : T_ToolDefTable ;
      oBlockManager  : T_BinBlockManager ;
      fp             : TGIS_BufferedFileStream ;
      XMinFilter     : Integer ;
      YMinFilter     : Integer ;
      XMaxFilter     : Integer ;
      YMaxFilter     : Integer ;
      sMinFilter     : TGIS_Point ;
      sMaxFilter     : TGIS_Point ;
      poCurObjBlock  : T_BlockObject ;
      poCurCoordBlock: T_BlockCoord ;
      nCurObjPtr     : Integer ;
      nCurObjType    : Integer ;
      nCurObjId      : Integer ;
      eAccessMode    : T_TABAccess ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create                    ( const _path    : String              ;
                                              const _layer   : TGIS_LayerVector    ;
                                              const _mode    : TGIS_StreamMode     ;
                                              const _onread  : TGIS_ReadWriteEvent ;
                                              const _onwrite : TGIS_ReadWriteEvent
                                            ) ; reintroduce ;
      procedure SetCoordsysBounds           ( var _ext       : TGIS_Extent
                                             ) ;
      procedure ResetCoordFilter ;
      function  Int2Coordsys          ( const _pt : TPoint
                                       ) : TGIS_Point ;
      procedure PrepareNewObj               ( const _poObjHdr : T_Object
                                             ) ;
      procedure CommitNewObj                ( const _poObjHdr : T_Object
                                             ) ;
      procedure UpdateMapHeaderInfo         ( const _nObjType : Byte
                                             ) ;
      procedure PrepareNewObjViaObjBlock    ( const _poObjHdr : T_Object
                                             ) ;
      procedure PrepareCoordBlock           ( const _nObjType      : Integer ;
                                              const _poObjBlock    : T_BlockObject ;
                                                var _ppoCoordBlock : T_BlockCoord
                                             ) ;
      function  Coordsys2Int                ( const _ptg         : TGIS_Point ;
                                              const _bIgnoreOverflow : Boolean
                                            ) : TPoint ;

      // Close file.
      procedure Close                       ; reintroduce ;
      procedure CommitObjAndCoordBlocks     ( const _bDeleteObjects : Boolean
                                             ) ;
      function  WritePenDef                 ( const _psDef : TGIS_TABPenDef
                                             ) : Integer ;
      function  WriteBrushDef               ( const _psDef : TGIS_TABBrushDef
                                             ) : Integer ;
      function  WriteSymbolDef              ( const _psDef : TGIS_TABSymbolDef
                                             ) : Integer ;
      function  WriteFontDef                ( const _psDef : TGIS_TABFontDef
                                             ) : Integer ;
      procedure CommitSpatialIndex ;
      procedure CommitDrawingTools ;
      procedure InitDrawingTools ;
      function  Coordsys2IntDist( const _ptg         : TGIS_Point
                                 ) : TPoint ;
  end ;


  // TAB file.
  T_TABFile = class( T_IMapInfoFile )
    Layer : TGIS_LayerVector ;
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      papszTABFile        : String ;
      nVersion            : Integer ;
      poMAPFile           : T_MAPFile ;
      nLastFeatureId      : Integer ;
      nFeatureId          : Integer ;
    protected

      function fget_DATPath : String ;
      function fget_MAPPath : String ;
      function fget_IDPath  : String ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create  ; override;

      // Open file.
      function  OpenEx         ( const _pszFname  : String
                                ) : Integer ; override;

      // Close file.
      procedure Close          ; override;

      // Set bounds.
      procedure SetBounds      ( var _ext         : TGIS_Extent
                                ) ; override;

      // Set feature.
      procedure SetFeature     ( const _poFeature : T_Feature
                                ) ; override;
      property  MAPPath : String read fget_MAPPath ;
      property  IDPath  : String read fget_IDPath  ;
  end ;

const
  HDR_VERSION_NUMBER           = 500 ;
  HDR_DATA_BLOCK_SIZE          = 512 ;
  HDR_DEF_ORG_QUADRANT         = 1 ;
  HDR_DEF_REFLECTXAXIS         = 0 ;
  HDR_MAGIC_COOKIE             = 42424242 ;

  HDR_OBJ_LEN_ARRAY_SIZE       = 73 ;

  gabyObjLenArray : array[ 0..HDR_OBJ_LEN_ARRAY_SIZE-1 ] of Byte =
                    {$IFDEF OXYGENE}[{$ELSE}({$ENDIF}
                      $00,$0a,$0e,$15,$0e,$16,$1b,$a2,
                      $a6,$ab,$1a,$2a,$2f,$a5,$a9,$b5,
                      $a7,$b5,$d9,$0f,$17,$23,$13,$1f,
                      $2b,$0f,$17,$23,$4f,$57,$63,$9c,
                      $a4,$a9,$a0,$a8,$ad,$a4,$a8,$ad,
                      $16,$1a,$39,$0d,$11,$37,$a5,$a9,
                      $b5,$a4,$a8,$ad,$b2,$b6,$dc,$bd,
                      $bd,$f4,$2b,$2f,$55,$c8,$cc,$d8,
                      $c7,$cb,$d0,$d3,$d7,$fd,$c2,$c2,
                      $f9
                    {$IFDEF OXYGENE}]{$ELSE}){$ENDIF} ;

  TABFC_NoGeom                 = 0 ;
  TABFC_Point                  = 1 ;
  TABFC_FontPoint              = 2 ;
  TABFC_CustomPoint            = 3 ;
  TABFC_Text                   = 4 ;
  TABFC_Polyline               = 5 ;
  TABFC_Arc                    = 6 ;
  TABFC_Region                 = 7 ;
  TABFC_Rectangle              = 8 ;
  TABFC_Ellipse                = 9 ;
  TABFC_MultiPoint             = 10 ;
  TABFC_Collection             = 11 ;

  TAB_GEONONE                  = 0 ;
  TAB_GEOSYMBOL_C              = $01 ;
  TAB_GEOSYMBOL                = $02 ;
  TAB_GEOLINE_C                = $04 ;
  TAB_GEOLINE                  = $05 ;
  TAB_GEOPLINE_C               = $07 ;
  TAB_GEOPLINE                 = $08 ;
  TAB_GEOARC_C                 = $0a ;
  TAB_GEOARC                   = $0b ;
  TAB_GEOREGION_C              = $0d ;
  TAB_GEOREGION                = $0e ;
  TAB_GEOTEXT_C                = $10 ;
  TAB_GEOTEXT                  = $11 ;
  TAB_GEORECT_C                = $13 ;
  TAB_GEORECT                  = $14 ;
  TAB_GEOROUNDRECT_C           = $16 ;
  TAB_GEOROUNDRECT             = $17 ;
  TAB_GEOELLIPSE_C             = $19 ;
  TAB_GEOELLIPSE               = $1a ;
  TAB_GEOMULTIPLINE_C          = $25 ;
  TAB_GEOMULTIPLINE            = $26 ;
  TAB_GEOFONTSYMBOL_C          = $28 ;
  TAB_GEOFONTSYMBOL            = $29 ;
  TAB_GEOCUSTOMSYMBOL_C        = $2b ;
  TAB_GEOCUSTOMSYMBOL          = $2c ;
  TAB_GEOV450_REGION_C         = $2e ;
  TAB_GEOV450_REGION           = $2f ;
  TAB_GEOV450_MULTIPLINE_C     = $31 ;
  TAB_GEOV450_MULTIPLINE       = $32 ;
  TAB_GEOMULTIPOINT_C          = $34 ;
  TAB_GEOMULTIPOINT            = $35 ;
  TAB_GEOCOLLECTION_C          = $37 ;
  TAB_GEOCOLLECTION            = $38 ;

  MAP_OBJECT_HEADER_SIZE       = 20 ;
  MAP_COORD_HEADER_SIZE        = 8  ;
  TAB_300_MAX_VERTICES         = 32767 ;

  TAB_RAWBIN_BLOCK             = -1 ;
  TABMAP_HEADER_BLOCK          = 0 ;
  TABMAP_INDEX_BLOCK           = 1 ;
  TABMAP_OBJECT_BLOCK          = 2 ;
  TABMAP_COORD_BLOCK           = 3 ;
  TABMAP_GARB_BLOCK            = 4 ;
  TABMAP_TOOL_BLOCK            = 5 ;
  TABMAP_LAST_VALID_BLOCK_TYPE = 5 ;

  TABMAP_TOOL_PEN              = 1 ;
  TABMAP_TOOL_BRUSH            = 2 ;
  TABMAP_TOOL_SYMBOL           = 4 ;

  MAP_TOOL_HEADER_SIZE         = 8 ;

  function IntExtent( const _xmin, _ymin, _xmax, _ymax : Integer ) : T_BoundingRect ;
  begin
    Result.XMin := _xmin ;
    Result.YMin := _ymin ;
    Result.XMax := _xmax ;
    Result.YMax := _ymax ;
  end ;

  function getStreamMode(
    const _boo : Boolean
  ) : TGIS_StreamMode ;
  begin
    if _boo then
      Result := TGIS_StreamMode.&Create
    else
      Result := TGIS_StreamMode.Read ;
  end ;

//==============================================================================
// T_IMapInfoFile
//==============================================================================

  constructor T_IMapInfoFile.Create ;
  begin
    inherited ;

  end ;

  procedure T_IMapInfoFile.doDestroy ;
  begin
    inherited;
  end ;

  function T_IMapInfoFile.OpenEx(
    const _pszFname : String
  ) : Integer ;
  begin
    Result := -1 ;
  end ;

  procedure T_IMapInfoFile.Close ;
  begin
    //
  end ;

  function T_IMapInfoFile.CreateFeature(
    const _feature : TGIS_Shape
  ) : T_Feature ;
  var
    export_as_point : Boolean ;

    function getAreaPattern( const _pattern : TGIS_BrushStyle ) : Integer ;
    begin
      case (_pattern) of
        TGIS_BrushStyle.Clear       : Result := 1 ;
        TGIS_BrushStyle.Solid       : Result := 2 ;
        TGIS_BrushStyle.Horizontal  : Result := 3 ;
        TGIS_BrushStyle.Vertical    : Result := 4 ;
        TGIS_BrushStyle.BDiagonal   : Result := 5 ;
        TGIS_BrushStyle.FDiagonal   : Result := 6 ;
        TGIS_BrushStyle.Cross       : Result := 7 ;
        TGIS_BrushStyle.DiagCross   : Result := 8
      else                            Result := 0;
      end ;
    end ;

    function getLineStyle( const _style : TGIS_PenStyle ) : Integer ;
    begin
      case (_style) of
        TGIS_PenStyle.Solid       : Result := 2 ;
        TGIS_PenStyle.Dot         : Result := 3 ;
        TGIS_PenStyle.Dash        : Result := 5 ;
        TGIS_PenStyle.DashDot     : Result := 14 ;
        TGIS_PenStyle.DashDotDot  : Result := 20 ;
        TGIS_PenStyle.Clear       : Result := 1
      else              Result := 0
      end ;
    end ;

    function twipsToPixels( const _size : Integer ) : Integer ;
    var
      tmp : Double ;
    begin
      if      _size < 0 then Result := -_size // minus so real pixels
      else if _size > 0 then begin
                               tmp := 1.0 * _size * 96 ;
                               if tmp > 1440 then Result := RoundS( tmp /1440 )
                                              else Result := 1 ;
                             end
      else                   Result := 0 ;
    end ;

  begin

    export_as_point := GisMetadataAsBoolean(
                         TAB_METADATA_EXPORT_LABELS_AS_POINT,
                         False
                       );

    case _feature.ShapeType of
      TGIS_ShapeType.Unknown     : Result := T_Feature.Create ;
      TGIS_ShapeType.Deleted     : Result := T_Feature.Create ;
      TGIS_ShapeType.Point       :
        begin
          {$IFDEF TAB_EXPORT_LABELS_AS_POINT}
            Result := TABPoint.Create ;
          {$ELSE}
            if export_as_point then
              Result := T_FeaturePoint.Create
            else begin
              if not IsStringEmpty( _feature.GetLabel ) then
                Result := T_FeatureText.Create
              else
                Result := T_FeaturePoint.Create ;
            end ;
          {$ENDIF}
        end ;
      TGIS_ShapeType.MultiPoint  : Result := T_FeatureMultiPoint.Create ;
      TGIS_ShapeType.Arc         : Result := T_FeaturePolyline.Create ;
      TGIS_ShapeType.Polygon,
      TGIS_ShapeType.MultiPatch  : Result := T_FeatureRegion.Create ;
      TGIS_ShapeType.Complex     : Result := T_Feature.Create ;
      TGIS_ShapeType.Null        : Result := T_Feature.Create
    else                        Result := T_Feature.Create ;
    end ;

    Result.FShape := _feature ;

    case _feature.ShapeType of
      TGIS_ShapeType.Point, TGIS_ShapeType.MultiPoint : begin
        Result.SetSymbolNo( 32 ) ;
        Result.SetSymbolSize( twipsToPixels( _feature.Params.Marker.Size ) ) ;
        Result.SetSymbolColor( _feature.Params.Marker.Color ) ;

        {$IFNDEF TAB_EXPORT_LABELS_AS_POINT}
          if not export_as_point then
            if not IsStringEmpty( _feature.GetLabel ) then begin
              Result.SetPenColor( _feature.Params.Marker.Color ) ;
              T_FeatureText( Result ).SetText( RadToDeg( _feature.Layer.Params.Labels.Rotate ),
                                         0,0,
                                         _feature.Layer.Params.Labels.FontColor,
                                         _feature.Layer.Params.Labels.Color,
                                         -1, -1, -1,
                                         _feature
                                       );
            end ;
        {$ENDIF}

      end ;
      TGIS_ShapeType.Arc : begin
        Result.SetPenColor( _feature.Params.Line.Color ) ;
        Result.SetPenPattern(
          getLineStyle( _feature.Params.Line.Style )
        ) ;
        Result.SetPenWidth( twipsToPixels( _feature.Params.Line.Width ) ) ;
      end ;
      TGIS_ShapeType.Polygon,
      TGIS_ShapeType.MultiPatch : begin
        Result.SetBrushFGColor( _feature.Params.Area.Color ) ;
        Result.SetBrushBGColor( _feature.Params.Area.Color ) ;
        Result.SetBrushPattern(
          getAreaPattern( _feature.Params.Area.Pattern )
        ) ;
        Result.SetBrushTransparent( 0 ) ;

        Result.SetPenColor( _feature.Params.Area.OutlineColor ) ;
        Result.SetPenPattern(
          getLineStyle( _feature.Params.Area.OutlineStyle )
        ) ;
        Result.SetPenWidth( twipsToPixels( _feature.Params.Area.OutlineWidth ) ) ;
      end ;
    end ;
  end ;

  procedure T_IMapInfoFile.SetBounds(
    var _ext : TGIS_Extent
  ) ;
  begin
    //safe inheritance
  end ;

  procedure T_IMapInfoFile.SetFeature(
    const _poFeature : T_Feature
  ) ;
  begin
    //safe inheritance
  end ;

//==============================================================================
// T_TABFile
//==============================================================================

  constructor T_TABFile.Create;
  begin
    inherited;

    papszTABFile    := '' ;
    nVersion        := 300 ;

    poMAPFile       := nil ;
    poCurFeature    := nil ;
    nLastFeatureId  := 0 ;
  end ;

  procedure T_TABFile.doDestroy;
  begin
    FreeObject( poMAPFile ) ;
    inherited;
  end ;

  function T_TABFile.fget_DATPath : String ;
  begin
    Result := GetPathNoExt( poMAPFile.pszFname ) + '.dat' ;
  end ;

  function T_TABFile.fget_IDPath : String ;
  begin
    Result := poMAPFile.poIdIndex.pszFname ;
  end ;

  function T_TABFile.fget_MAPPath : String ;
  begin
    Result := poMAPFile.pszFname ;
  end ;

  function T_TABFile.OpenEx(
    const _pszFname : String
  ) : Integer ;
  begin
    assert( poMAPFile = nil, 'Open() failed: object already contains an open file' ) ;

    nVersion        := 300 ;
    nLastFeatureId  := 0 ;

    nLastFeatureId := 0 ;

    poMAPFile := T_MAPFile.Create( GetPathNoExt( _pszFname ) + '.map',
                                   self, TGIS_StreamMode.&Create, nil, nil
                                  ) ;
    Result := 0 ;
  end ;

  procedure T_TABFile.SetBounds(
    var _ext : TGIS_Extent
  ) ;
  begin
    if ( poMAPFile <> nil ) and ( nLastFeatureId < 1 ) then begin

      poMAPFile.SetCoordsysBounds( _ext ) ;
    end ;
  end ;

  procedure T_TABFile.SetFeature(
    const _poFeature : T_Feature
  ) ;
  var
    poObjHdr : T_Object ;
  begin
    inherited;

    assert( poMAPFile <> nil, 'SetFeature() failed: file is not opened!' ) ;

    if ( nLastFeatureId < 1 ) then begin
      nLastFeatureId := 1 ;
      nFeatureId       := nLastFeatureId ;
    end
    else begin
      inc( nLastFeatureId ) ;
      nFeatureId := nLastFeatureId ;
    end ;

    poObjHdr := T_Object.NewObj( _poFeature.Validate( poMAPFile ), nFeatureId ) ;
    try
      if ( poObjHdr.nType = TAB_GEONONE ) and
         ( _poFeature.FeatureClass = TABFC_NoGeom ) then exit ;

      if ( poObjHdr.nType <> TAB_GEONONE ) then
        poObjHdr.FBoundingRect := _poFeature.FBoundingRect ;

      if assigned( poMAPFile ) then begin
         poMAPFile.PrepareNewObj( poObjHdr ) ;
         _poFeature.Write( poMAPFile, poObjHdr ) ;
         poMAPFile.CommitNewObj( poObjHdr ) ;
      end ;
    finally
      FreeObject( poObjHdr ) ;
    end ;
  end ;

  procedure T_TABFile.Close ;
  begin
    if assigned( poMAPFile ) then begin
      nVersion := Max( nVersion, poMAPFile.nMinTABVersion ) ;
    end ;

    if assigned( poMAPFile ) then begin
      poMAPFile.Close ;
      FreeObject( poMAPFile ) ;
    end ;

    FreeObject( poCurFeature ) ;

    FreeObject( papszTABFile ) ;
  end ;

//==============================================================================
// T_MAPFile
//==============================================================================

  constructor T_MAPFile.Create(
    const _path    : String              ;
    const _layer   : TGIS_LayerVector    ;
    const _mode    : TGIS_StreamMode     ;
    const _onread  : TGIS_ReadWriteEvent ;
    const _onwrite : TGIS_ReadWriteEvent
  ) ;
  var
    poblock : T_BlockRaw ;
  begin
    inherited Create( GetTemporaryName( _path ), _mode, _onread, _onwrite );

    nMinTABVersion := 300 ;
    poHeader       := nil ;
    poIdIndex      := nil ;
    poSpIndex      := nil ;
    poToolDefTable := nil ;
    eAccessMode    := T_TABAccess.Write ;

    oBlockManager := T_BinBlockManager.Create( 512 ) ;
    oBlockManager.Reset ;

    poblock := T_BlockHeader.Create( eAccessMode, True );
    poblock.InitNewBlock( TGIS_BufferedFileStream( self ), 1024, oBlockManager.AllocNewBlock ) ;

    oBlockManager.AllocNewBlock ;

    fp := self ;
    poHeader := T_BlockHeader ( poblock ) ;
    pszFname := _path ;

    poCurObjBlock := nil ;

    poIdIndex := T_IDFile.Create( _path, nil, TGIS_StreamMode.&Create, nil, nil ) ;

    poSpIndex := nil ;

    poToolDefTable := nil ;

    InitDrawingTools ;
  end ;

  procedure T_MAPFile.doDestroy;
  begin
    FreeObject( poIdIndex     ) ;
    FreeObject( oBlockManager ) ;
    FreeObject( poHeader      ) ;

    inherited;
  end ;

  procedure T_MAPFile.Close;
  begin
    if ( fp = nil ) and ( poHeader = nil ) then exit ;

    CommitObjAndCoordBlocks( False ) ;
    CommitDrawingTools ;
    CommitSpatialIndex ;

    if assigned( poHeader ) then begin
      if ( nMinTABVersion >= 450 ) then
        poHeader.nMaxCoordBufSize := Min( poHeader.nMaxCoordBufSize, 512*1024 ) ;

      poHeader.nFirstGarbageBlock := oBlockManager.GetFirstGarbageBlock;
      poHeader.CommitToFile ;
    end ;

    // Delete all structures
    if assigned( poHeader ) then
      FreeObject( poHeader ) ;

    if assigned( poIdIndex ) then begin
      poIdIndex.Close ;
      FreeObject( poIdIndex );
    end ;

    if assigned( poCurObjBlock ) then begin
      FreeObject( poCurObjBlock ) ;
      nCurObjPtr  := -1;
      nCurObjType := -1;
      nCurObjId   := -1;
    end ;

    if assigned( poCurCoordBlock ) then
      FreeObject( poCurCoordBlock ) ;

    if assigned( poSpIndex ) then begin
      FreeObject( poSpIndex ) ;
    end ;

    if assigned( poToolDefTable ) then
      FreeObject( poToolDefTable ) ;

    pszFname := '' ;
  end ;

  procedure T_MAPFile.CommitDrawingTools ;
  var
    poblock : T_BlockTool ;
  begin
    if poHeader = nil then exit ;

    if ( poToolDefTable = nil ) then exit ;

    poblock := T_BlockTool.Create( eAccessMode, True ) ;
    poblock.InitNewBlock( fp, 512, oBlockManager.AllocNewBlock ) ;
    poblock.SetMAPBlockManagerRef( oBlockManager ) ;

    poHeader.nFirstToolBlock  := poblock.nFileOffset ;
    poHeader.numPenDefs       := poToolDefTable.numPen ;
    poHeader.numBrushDefs     := poToolDefTable.numBrushes ;
    poHeader.numFontDefs      := poToolDefTable.numFonts  ;
    poHeader.numSymbolDefs    := poToolDefTable.numSymbols ;

    poToolDefTable.WriteAllToolDefs( poblock ) ;

    poHeader.numMapToolBlocks := poblock.numBlocksInChain ;

    FreeObject( poblock ) ;
  end ;

  procedure T_MAPFile.CommitNewObj(
    const _poObjHdr : T_Object
  ) ;
  begin
    poCurObjBlock.CommitNewObject( _poObjHdr ) ;
  end ;

  procedure T_MAPFile.CommitObjAndCoordBlocks(
    const _bDeleteObjects : Boolean
  ) ;
  var
    nTotalCoordSize : Integer ;
    br : T_BoundingRect ;
  begin
    if ( poCurObjBlock = nil ) then exit ;

    if assigned( poCurCoordBlock ) then begin

      nTotalCoordSize := poCurCoordBlock.numBlocksInChain*512 ;

      if ( nTotalCoordSize > poHeader.nMaxCoordBufSize ) then
        poHeader.nMaxCoordBufSize := nTotalCoordSize ;

      poCurObjBlock.AddCoordBlockRef( poCurCoordBlock.nFileOffset ) ;
      poCurCoordBlock.CommitToFile ;

      if ( _bDeleteObjects ) then
        FreeObject( poCurCoordBlock ) ;
    end ;

    poCurObjBlock.CommitToFile ;

    if ( poSpIndex = nil ) then begin
      poSpIndex := T_BlockIndex.Create( eAccessMode, True ) ;
      poSpIndex.InitNewBlock( fp, 512, oBlockManager.AllocNewBlock ) ;
      poSpIndex.poBlockManagerRef := oBlockManager ;
      poHeader.nFirstIndexBlock   := poSpIndex.nFileOffset ;
    end ;

    br := poCurObjBlock.FBoundingRect ;
    poSpIndex.AddEntry( br, poCurObjBlock.nFileOffset, False ) ;

    poHeader.nMaxSpIndexDepth := Max( poHeader.nMaxSpIndexDepth,
                                          poSpIndex.GetCurMaxDepth+1
                                         ) ;

    if ( _bDeleteObjects ) then
      FreeObject( poCurObjBlock ) ;
  end ;

  procedure T_MAPFile.CommitSpatialIndex;
  var
    br : T_BoundingRect ;
  begin
    if poHeader  = nil then exit ;
    if poSpIndex = nil then exit ;

    poHeader.nMaxSpIndexDepth := Max( poHeader.nMaxSpIndexDepth,
                                          poSpIndex.GetCurMaxDepth+1
                                         ) ;

    br := poSpIndex.GetBoundingRect ;

    poHeader.hnXMin := br.XMin ;
    poHeader.hnYMin := br.YMin ;
    poHeader.hnXMax := br.XMax ;
    poHeader.hnYMax := br.YMax ;

    poSpIndex.CommitToFile ;
  end ;

  function T_MAPFile.Coordsys2Int(
    const _ptg              : TGIS_Point ;
    const _bIgnoreOverflow  : Boolean
  ) : TPoint ;
  begin
    if not assigned( poHeader ) then exit ;

    Result := poHeader.Coordsys2Int( _ptg, _bIgnoreOverflow ) ;
  end ;

  function T_MAPFile.Coordsys2IntDist(
    const _ptg : TGIS_Point
  ) : TPoint ;
  begin
    if ( poHeader = nil ) then exit ;

    Result.X := TruncS( _ptg.X*poHeader.XScale ) ;
    Result.Y := TruncS( _ptg.Y*poHeader.YScale ) ;
  end ;

  procedure T_MAPFile.InitDrawingTools ;
  begin
    if ( poHeader = nil ) then exit ;

    if ( poToolDefTable <> nil ) then exit ;

    poToolDefTable := T_ToolDefTable.Create ;
  end ;

  function T_MAPFile.Int2Coordsys(
    const _pt : TPoint
  ) : TGIS_Point ;
  begin
    if ( poHeader <> nil ) then
      Result := poHeader.Int2Coordsys( _pt ) ;
  end ;

  procedure T_MAPFile.PrepareCoordBlock(
    const _nObjType       : Integer ;
    const _poObjBlock     : T_BlockObject ;
      var _ppoCoordBlock  : T_BlockCoord
  ) ;
  var
    nNewBlockOffset : Integer ;
  begin
    if ( poHeader.MapObjectUsesCoordBlock( _nObjType ) ) then begin

      if ( _ppoCoordBlock = nil ) then begin
        if eAccessMode = T_TABAccess.Write then
          _ppoCoordBlock := T_BlockCoord.Create( T_TABAccess.ReadWrite, True )
        else
          _ppoCoordBlock := T_BlockCoord.Create( eAccessMode, True ) ;

        _ppoCoordBlock.InitNewBlock( fp, 512, oBlockManager.AllocNewBlock ) ;
        _ppoCoordBlock.SetMAPBlockManagerRef( oBlockManager ) ;

        _poObjBlock.AddCoordBlockRef( _ppoCoordBlock.nFileOffset ) ;
      end ;

      if ( _ppoCoordBlock.GetNumUnusedBytes < 4 ) then begin
        nNewBlockOffset := oBlockManager.AllocNewBlock ;
        _ppoCoordBlock.SetNextCoordBlock( nNewBlockOffset ) ;
        _ppoCoordBlock.CommitToFile ;
        _ppoCoordBlock.InitNewBlock( fp, 512, nNewBlockOffset ) ;
      end ;

      _ppoCoordBlock.nCurPos := _ppoCoordBlock.nSizeUsed ;
    end ;
  end ;

  procedure T_MAPFile.PrepareNewObj(
    const _poObjHdr : T_Object
  ) ;
  begin
    nCurObjPtr  := -1 ;
    nCurObjId   := -1 ;
    nCurObjType := -1 ;

    if ( _poObjHdr.nType = TAB_GEONONE ) then begin

      nCurObjType := _poObjHdr.nType ;
      nCurObjId   := _poObjHdr.nId ;
      nCurObjPtr  := 0;
      poIdIndex.SetObjPtr( nCurObjId, 0 ) ;

      exit ;
    end ;

    UpdateMapHeaderInfo( _poObjHdr.nType ) ;

    PrepareNewObjViaObjBlock( _poObjHdr ) ;

    nCurObjPtr  := poCurObjBlock.PrepareNewObject( _poObjHdr ) ;
    nCurObjType := _poObjHdr.nType ;
    nCurObjId   := _poObjHdr.nId ;

    poIdIndex.SetObjPtr( nCurObjId, nCurObjPtr ) ;

    PrepareCoordBlock( nCurObjType, poCurObjBlock, poCurCoordBlock ) ;
  end ;

  procedure T_MAPFile.PrepareNewObjViaObjBlock(
    const _poObjHdr : T_Object
  ) ;
  var
    nObjSize,
    nBlockOffset : Integer ;
  begin
    if ( poCurObjBlock = nil ) then begin
      poCurObjBlock := T_BlockObject.Create( eAccessMode, True ) ;
      nBlockOffset    := oBlockManager.AllocNewBlock ;

      poCurObjBlock.InitNewBlock( fp, 512, nBlockOffset ) ;
      poHeader.nFirstIndexBlock := nBlockOffset ;
    end ;

    nObjSize := poHeader.GetMapObjectSize( _poObjHdr.nType ) ;

    if ( poCurObjBlock.GetNumUnusedBytes < nObjSize ) then begin
      CommitObjAndCoordBlocks( False ) ;

      poCurObjBlock.InitNewBlock( fp, 512, oBlockManager.AllocNewBlock ) ;

      if assigned( poCurCoordBlock ) then
        FreeObject( poCurCoordBlock ) ;
    end ;
  end ;

  procedure T_MAPFile.ResetCoordFilter;

    procedure order_min_max_int( var min, max : Integer ) ;
    var
      temp : Integer ;
    begin
      if max < min then begin
        temp := max  ;
        max  := min  ;
        min  := temp ;
      end ;
    end ;

    procedure order_min_max_dbl( var min, max : Double ) ;
    var
      temp : Double ;
    begin
      if max < min then begin
        temp := max  ;
        max  := min  ;
        min  := temp ;
      end ;
    end ;

  begin
    XMinFilter := poHeader.hnXMin;
    YMinFilter := poHeader.hnYMin;
    XMaxFilter := poHeader.hnXMax;
    YMaxFilter := poHeader.hnYMax;

    sMinFilter := Int2Coordsys( Point( XMinFilter, YMinFilter ) ) ;
    sMaxFilter := Int2Coordsys( Point( XMaxFilter, YMaxFilter ) ) ;

    order_min_max_int( XMinFilter, XMaxFilter ) ;
    order_min_max_int( YMinFilter, YMaxFilter ) ;
    order_min_max_dbl( sMinFilter.X, sMaxFilter.X ) ;
    order_min_max_dbl( sMinFilter.Y, sMaxFilter.Y ) ;
  end ;

  procedure T_MAPFile.SetCoordsysBounds(
    var _ext : TGIS_Extent
  ) ;
  begin
    if poHeader = nil then exit ;

    poHeader.SetCoordsysBounds( _ext ) ;
    ResetCoordFilter ;
  end ;

  procedure T_MAPFile.UpdateMapHeaderInfo(
    const _nObjType : Byte
  ) ;
  begin
    if      ( _nObjType = TAB_GEOSYMBOL             ) or
            ( _nObjType = TAB_GEOFONTSYMBOL         ) or
            ( _nObjType = TAB_GEOCUSTOMSYMBOL       ) or
            ( _nObjType = TAB_GEOMULTIPOINT         ) or
            ( _nObjType = TAB_GEOSYMBOL_C           ) or
            ( _nObjType = TAB_GEOFONTSYMBOL_C       ) or
            ( _nObjType = TAB_GEOCUSTOMSYMBOL_C     ) or
            ( _nObjType = TAB_GEOMULTIPOINT_C       ) then

        inc( poHeader.numPointObjects )

    else if ( _nObjType = TAB_GEOLINE               ) or
            ( _nObjType = TAB_GEOPLINE              ) or
            ( _nObjType = TAB_GEOMULTIPLINE         ) or
            ( _nObjType = TAB_GEOV450_MULTIPLINE    ) or
            ( _nObjType = TAB_GEOARC                ) or
            ( _nObjType = TAB_GEOLINE_C             ) or
            ( _nObjType = TAB_GEOPLINE_C            ) or
            ( _nObjType = TAB_GEOMULTIPLINE_C       ) or
            ( _nObjType = TAB_GEOV450_MULTIPLINE_C  ) or
            ( _nObjType = TAB_GEOARC_C              ) then

        inc( poHeader.numLineObjects )

    else if ( _nObjType = TAB_GEOREGION             ) or
            ( _nObjType = TAB_GEOV450_REGION        ) or
            ( _nObjType = TAB_GEORECT               ) or
            ( _nObjType = TAB_GEOROUNDRECT          ) or
            ( _nObjType = TAB_GEOELLIPSE            ) or
            ( _nObjType = TAB_GEOREGION_C           ) or
            ( _nObjType = TAB_GEOV450_REGION_C      ) or
            ( _nObjType = TAB_GEORECT_C             ) or
            ( _nObjType = TAB_GEOROUNDRECT_C        ) or
            ( _nObjType = TAB_GEOELLIPSE_C          ) then

        inc( poHeader.numRegionObjects )

    else if ( _nObjType = TAB_GEOTEXT               ) or
            ( _nObjType = TAB_GEOTEXT_C             ) then

        inc( poHeader.numTextObjects ) ;

    if      ( nMinTABVersion < 450 ) and (
            (_nObjType = TAB_GEOV450_REGION         ) or
            (_nObjType = TAB_GEOV450_MULTIPLINE     ) or
            (_nObjType = TAB_GEOV450_REGION_C       ) or
            (_nObjType = TAB_GEOV450_MULTIPLINE_C)  ) then

        nMinTABVersion := 450 ;

    if      ( nMinTABVersion < 650 ) and (
            ( _nObjType = TAB_GEOMULTIPOINT         ) or
            ( _nObjType = TAB_GEOMULTIPOINT_C       ) or
            ( _nObjType = TAB_GEOCOLLECTION         ) or
            ( _nObjType = TAB_GEOCOLLECTION_C )     ) then

        nMinTABVersion := 650 ;
  end ;

  function T_MAPFile.WriteBrushDef(
    const _psDef : TGIS_TABBrushDef
  ) : Integer ;
  begin
    Result := poToolDefTable.AddBrushDefRef( _psDef ) ;
  end ;

  function T_MAPFile.WriteFontDef(
    const _psDef : TGIS_TABFontDef
  ) : Integer ;
  begin
    Result := poToolDefTable.AddFontDefRef( _psDef ) ;
  end ;

  function T_MAPFile.WritePenDef(
    const _psDef : TGIS_TABPenDef
  ) : Integer ;
  begin
    Result := poToolDefTable.AddPenDefRef( _psDef ) ;
  end ;

  function T_MAPFile.WriteSymbolDef(
    const _psDef : TGIS_TABSymbolDef
  ) : Integer ;
  begin
    Result := poToolDefTable.AddSymbolDefRef( _psDef ) ;
  end ;

//==============================================================================
// T_IDFile
//==============================================================================

  procedure T_IDFile.Close ;
  begin
    if ( fp = nil ) then exit ;

    poIDBlock.CommitToFile ;
    FreeObject( poIDBlock ) ;

    pszFname := '' ;

    inherited ;
  end ;

  constructor T_IDFile.Create(
     const _path    : String              ;
     const _layer   : TGIS_LayerVector    ;
     const _mode    : TGIS_StreamMode     ;
     const _onread  : TGIS_ReadWriteEvent ;
     const _onwrite : TGIS_ReadWriteEvent
   ) ;
  begin
    inherited Create( GetTemporaryName( GetPathNoExt( _path ) + '.id' ), _mode, _onread, _onwrite );

    nMaxId      := 0;
    nBlockSize  := 1024;

    pszFname    := GetPathNoExt( _path ) + '.id' ;
    eAccessMode := T_TABAccess.ReadWrite;

    fp := self ;

    poIDBlock := T_BlockRaw.Create( eAccessMode, False ) ;
    poIDBlock.bHardBlockSize := False ;
    poIDBlock.InitNewBlock( fp, nBlockSize, 0 ) ;
  end ;

  procedure T_IDFile.doDestroy;
  begin
    FreeObject( poIDBlock ) ;
    inherited;
  end ;

  procedure T_IDFile.SetObjPtr(
    const _nObjId   : Integer ;
    const _nObjPtr  : Integer
  ) ;
  var
    nlastidblock   : Integer ;
    ntargetidblock : Integer ;
  begin
    if ( poIDBlock = nil ) then exit ;

    if ( _nObjId < 1 ) then exit ;

    nlastidblock   := TruncS( ( ( nMaxId-1)*4.0 ) / nBlockSize ) ;
    ntargetidblock := TruncS( ( (_nObjId-1)*4.0 ) / nBlockSize ) ;

    if ( nMaxId > 0 ) and ( ntargetidblock <= nlastidblock ) then
      poIDBlock.GotoByteInFile( (_nObjId-1)*4, True, False )
    else
      poIDBlock.GotoByteInFile( (_nObjId-1)*4, False, False ) ;

    nMaxId := Max( nMaxId, _nObjId ) ;

    poIDBlock.WriteInt32( _nObjPtr );
  end ;

//==============================================================================
// T_BinBlockManager
//==============================================================================

  constructor T_BinBlockManager.Create(
    const _nBlockSize : Integer
  ) ;
  begin
    inherited Create ;

    nBlockSize          := _nBlockSize ;
    nLastAllocatedBlock := -1 ;
    psGarbageBlocks     := nil ;
  end ;

  function T_BinBlockManager.GetFirstGarbageBlock : Integer ;
  begin
    if psGarbageBlocks <> nil then begin
      Result := psGarbageBlocks.nBlockPtr ;
      exit ;
    end ;

    Result := 0 ;
  end ;

  function T_BinBlockManager.PopGarbageBlock : Integer ;
  var
    nBlockPtr : Integer ;
    psNext    : T_BlockRef ;
  begin
    nBlockPtr := 0 ;

    if psGarbageBlocks <> nil then begin

      nBlockPtr := psGarbageBlocks.nBlockPtr ;
      psNext    := psGarbageBlocks.psNext ;
      FreeObject( psGarbageBlocks ) ;
      psGarbageBlocks := psNext ;
    end ;

    Result := nBlockPtr ;
  end ;

  procedure T_BinBlockManager.Reset ;
  var
    psnext : T_BlockRef ;
  begin
    nLastAllocatedBlock := -1 ;

    while ( psGarbageBlocks <> nil ) do begin
      psnext := psGarbageBlocks.psNext ;
      FreeObject( psGarbageBlocks ) ;
      psGarbageBlocks := psnext ;
    end ;
  end ;

  function T_BinBlockManager.AllocNewBlock : Integer ;
  begin
    if ( GetFirstGarbageBlock > 0 ) then begin
      Result := PopGarbageBlock ;
      exit ;
    end ;

    if ( nLastAllocatedBlock = -1 ) then
      nLastAllocatedBlock := 0
    else
      inc( nLastAllocatedBlock, nBlockSize ) ;

    Result := nLastAllocatedBlock ;
  end ;

//==============================================================================
// T_RawBinBlock
//==============================================================================

  procedure T_BlockRaw.CommitToFile;
  var
    cur_pos     : Integer ;
    c_zero      : Byte ;
    bytes2write : Integer ;
  begin
    if ( fp = nil ) or ( nBlockSize <= 0 ) or ( pabyBuf = nil ) or
       ( nFileOffset < 0 ) then exit ;

    if not bModified then exit ;

    if fp.Size < nFileOffset then begin
      fp.Position := fp.Size ;
      cur_pos := fp.Position ;
      if ( cur_pos < nFileOffset ) then begin
        c_zero := 0 ;
        while ( cur_pos < nFileOffset ) do begin
          {$IFDEF OXYGENE}
          fp.WriteByte( c_zero ) ;
          {$ELSE}
          fp.Write( c_zero, 1 ) ;
          {$ENDIF}
          inc( cur_pos ) ;
        end ;
      end ;
      assert( fp.Position = nFileOffset ) ;
    end
    else
      fp.Position := nFileOffset ;

    if bHardBlockSize then
      bytes2write := nBlockSize
    else
      bytes2write := nSizeUsed ;

    {$IFDEF OXYGENE}
      fp.WriteBuffer( pabyBuf   , bytes2write ) ;
    {$ELSE}
      fp.WriteBuffer( pabyBuf[0], bytes2write ) ;
    {$ENDIF}
    fp.FlushBuffer ;
    bModified := False;
  end ;

  constructor T_BlockRaw.Create(
    const _eAccessMode    : T_TABAccess ;
    const _bHardBlockSize : Boolean
  ) ;
  begin
    inherited Create ;

    pabyBuf         := nil ;
    nFirstBlockPtr  := 0 ;
    nBlockSize      := 0 ;
    nSizeUsed       := 0 ;
    nFileOffset     := 0 ;
    nCurPos         := 0 ;
    bHardBlockSize  := _bHardBlockSize ;

    bModified       := False ;
    eAccess         := _eAccessMode;
  end ;

  procedure T_BlockRaw.doDestroy;
  begin
    pabyBuf := nil ;

    inherited;
  end ;

  function T_BlockRaw.GetBlockClass : Integer ;
  begin
    Result := TAB_RAWBIN_BLOCK ;
  end ;

  function T_BlockRaw.GetCurAddress : Integer ;
  begin
    Result := (nFileOffset + nCurPos) ;
  end ;

  function T_BlockRaw.GetFirstUnusedByteOffset : Integer ;
  begin
    if ( nSizeUsed < nBlockSize ) then
      Result := nFileOffset + nSizeUsed
    else
      Result := -1 ;
  end ;

  function T_BlockRaw.GetNumUnusedBytes : Integer ;
  begin
    Result := ( nBlockSize - nSizeUsed ) ;
  end ;

  procedure T_BlockRaw.GotoByteInBlock(
    const _nOffset : Integer
  ) ;
  begin
    if ( _nOffset < nBlockSize ) and ( _nOffset >= 0 ) then begin

      nCurPos   := _nOffset ;
      nSizeUsed := Max( nSizeUsed, nCurPos ) ;
    end ;
  end ;

  procedure T_BlockRaw.GotoByteInFile(
    const _nOffset: Integer;
    const _bForceReadFromFile,
          _bOffsetIsEndOfData: Boolean);
  var
    nNewBlockPtr : Integer ;
  begin
    if ( _nOffset < 0 ) then exit ;

    nNewBlockPtr := ( TruncS( 1.0*( _nOffset-nFirstBlockPtr)/nBlockSize)*nBlockSize +
                     nFirstBlockPtr );

    if (eAccess = T_TABAccess.Read) then begin
      if  (_nOffset<nFileOffset) or ( _nOffset>=(nFileOffset+nSizeUsed)) then
         ReadFromFile( fp, nNewBlockPtr, nBlockSize ) ;
    end
    else if ( eAccess = T_TABAccess.Write ) then begin
      if ( _nOffset < nFileOffset ) or ( _nOffset >= nFileOffset+nBlockSize ) then begin
        CommitToFile ;
        InitNewBlock( fp, nBlockSize, nNewBlockPtr ) ;
      end ;
    end
    else begin
      if ( _bOffsetIsEndOfData and ( ( _nOffset mod nBlockSize ) = 0 ) ) then begin

        dec( nNewBlockPtr, nBlockSize ) ;

        if ( (_nOffset < nFileOffset ) or
             (_nOffset > (nFileOffset+nBlockSize))) then begin
             CommitToFile;
             if not _bForceReadFromFile then
               InitNewBlock( fp, nBlockSize, nNewBlockPtr )
             else
               ReadFromFile( fp, nNewBlockPtr, nBlockSize ) ;
        end ;
      end
      else begin
        if ( (_nOffset < nFileOffset) or
             (_nOffset >= (nFileOffset+nBlockSize))) then begin
             CommitToFile ;
             if not _bForceReadFromFile then
               InitNewBlock( fp, nBlockSize, nNewBlockPtr )
             else
               ReadFromFile( fp, nNewBlockPtr, nBlockSize ) ;
        end ;
      end ;
    end ;

    nCurPos   := _nOffset - nFileOffset ;
    nSizeUsed := Max( nSizeUsed, nCurPos ) ;
  end ;

  procedure T_BlockRaw.InitBlockFromData(
    const _pabyBuf    : array of Byte ;
    const _nBlockSize : Integer ;
    const _nSizeUsed  : Integer ;
    const _bMakeCopy  : Boolean ;
    const _fpSrc      : TGIS_BufferedFileStream ;
    const _nOffset    : Integer
  );
  begin
    fp          := _fpSrc ;
    nFileOffset := _nOffset ;
    nCurPos     := 0 ;
    bModified   := False ;

    if ( not _bMakeCopy ) then begin

      if ( pabyBuf <> nil ) then
        pabyBuf := nil ;
      SetLength( pabyBuf, _nBlockSize );
      {$IFDEF OXYGENE}
        GisCopyMemory( _pabyBuf, 0, pabyBuf, 0, _nBlockSize ) ;
      {$ELSE}
        Move( _pabyBuf[0], pabyBuf[0], _nBlockSize ) ;
      {$ENDIF}

      nBlockSize  := _nBlockSize;
      nSizeUsed   := _nSizeUsed;
    end
    else if ( pabyBuf = nil ) or ( _nBlockSize <> nBlockSize ) then begin

      SetLength( pabyBuf, _nBlockSize );
      nBlockSize := _nBlockSize;
      nSizeUsed := _nSizeUsed;
      {$IFDEF OXYGENE}
        GisCopyMemory( _pabyBuf, 0, pabyBuf, 0, _nSizeUsed ) ;
      {$ELSE}
        Move( _pabyBuf[0], pabyBuf[0], _nSizeUsed ) ;
      {$ENDIF}
    end ;

    if ( nFileOffset = 0 ) then
      nBlockType := TABMAP_HEADER_BLOCK
    else
      nBlockType := pabyBuf[0] ;
  end ;

  function T_BlockRaw.InitNewBlock(
    const _fpSrc       : TGIS_BufferedFileStream ;
    const _nBlockSize  : Integer ;
    const _nFileOffset : Integer
  ): Integer ;
  {$IFDEF OXYGENE}
  var
    i : Integer ;
  {$ENDIF}
  begin
    fp          := _fpSrc;
    nBlockSize  := _nBlockSize;
    nSizeUsed   := 0;
    nCurPos     := 0;
    bModified   := False;

    if ( _nFileOffset > 0 ) then
      nFileOffset := _nFileOffset
    else
      nFileOffset := 0 ;

    nBlockType := -1 ;

    SetLength( pabyBuf, nBlockSize * sizeOf( Byte ) ) ;

    {$IFDEF OXYGENE}
      for i := 0 to nBlockSize - 1 do
        pabyBuf[ i ] := 0 ;
    {$ELSE}
      FillChar( pabyBuf[0], nBlockSize, 0 ) ;
    {$ENDIF}

    Result := 0 ;
  end ;

  procedure T_BlockRaw.ReadBytes(
     const _numBytes   : Integer;
     const _offset     : Integer ;
       var _pabyDstBuf : array of Byte
    ) ;
  begin
    assert( pabyBuf <> nil ) ;
    assert( (eAccess <> T_TABAccess.Read) and ( eAccess <> T_TABAccess.ReadWrite ) ) ;

    assert( (nCurPos + _numBytes) <= nSizeUsed ) ;
    {$IFDEF OXYGENE}
      GisCopyMemory( pabyBuf, nCurPos, _pabyDstBuf, _offset, _numBytes ) ;
    {$ELSE}
      Move( pabyBuf[nCurPos], _pabyDstBuf[_offset], _numBytes ) ;
    {$ENDIF}
    inc( nCurPos, _numBytes ) ;
  end ;

  procedure T_BlockRaw.ReadFromFile(
    const _fpSrc    : TGIS_BufferedFileStream ;
    const _nOffset  : Integer ;
    const _nSize    : Integer
  ) ;
  var
    buf : TBytes ;
  begin
    if (_fpSrc = nil) or (_nSize = 0) then exit ;

    fp          := _fpSrc ;
    nFileOffset := _nOffset ;
    nCurPos     := 0 ;
    bModified   := False  ;

    SetLength( buf, _nSize*sizeOf(Byte) ) ;

    _fpSrc.Position := _nOffset ;
    {$IFNDEF OXYGENE}
      nSizeUsed := _fpSrc.Read( buf[0], _nSize ) ;
    {$ELSE}
      nSizeUsed := _fpSrc.Read( buf   , _nSize ) ;
    {$ENDIF}

    InitBlockFromData( buf, _nSize, nSizeUsed, False, _fpSrc, _nOffset ) ;
  end ;

  function T_BlockRaw.ReadInt16 : SmallInt ;
  var
    {$IFNDEF OXYGENE}
      buf : array[0..1] of Byte ;
    {$ELSE}
      buf : array of Byte := new Byte[2] ;
    {$ENDIF}
  begin
    ReadBytes( 2, 0, buf ) ;
    {$IFDEF OXYGENE}
      Result := BitConverter.ToInt16( buf, 0 ) ;
    {$ELSE}
      Move( buf[0], Result, 2 ) ;
    {$ENDIF}
  end ;

  function T_BlockRaw.ReadInt32 : Integer ;
  var
    {$IFNDEF OXYGENE}
      buf : array[0..3] of Byte ;
    {$ELSE}
      buf : array of Byte := new Byte[4] ;
    {$ENDIF}
  begin
    ReadBytes( 4, 0, buf ) ;
    {$IFDEF OXYGENE}
      Result := BitConverter.ToInt32( buf, 0 ) ;
    {$ELSE}
      Move( buf[0], Result, 4 ) ;
    {$ENDIF}
  end ;

  function T_BlockRaw.TABCreateMAPBlockFromFile(
    const _fpSrc  : TGIS_BufferedFileStream ;
    const _nOffset: Integer ;
    const _nSize  : Integer ;
    const _bHardBlockSize: Boolean
  ) : T_BlockRaw ;
  var
    poblock : T_BlockRaw ;
    buf     : TBytes ;
  begin
    Result := nil ;
    if ( _fpSrc = nil ) or ( _nSize = 0 ) then exit ;

    SetLength( buf, _nSize*sizeOf(Byte) ) ;

    _fpSrc.Position := _nOffset ;
     {$IFDEF OXYGENE}
      _fpSrc.ReadBuffer( buf   , _nSize ) ;
    {$ELSE}
      _fpSrc.ReadBuffer( buf[0], _nSize ) ;
    {$ENDIF}

    if ( _nOffset = 0 ) then
      poblock := T_BlockHeader.Create( T_TABAccess.Read, True )
    else begin
      case ( buf[0] ) of
        TABMAP_INDEX_BLOCK  : poblock := T_BlockIndex.Create(eAccess, True)  ;
        TABMAP_OBJECT_BLOCK : poblock := T_BlockObject.Create(eAccess, True) ;
        TABMAP_COORD_BLOCK  : poblock := T_BlockCoord.Create(eAccess, True) ;
        TABMAP_TOOL_BLOCK   : poblock := T_BlockTool.Create(eAccess, True) ;
        TABMAP_GARB_BLOCK   : poblock := T_BlockRaw.Create(eAccess, True)
      else                    poblock := T_BlockRaw.Create(eAccess, _bHardBlockSize) ;
      end ;
    end ;

    poblock.InitBlockFromData( buf, _nSize, _nSize, False, _fpSrc, _nOffset ) ;
    Result := poblock ;
  end ;

  procedure T_BlockRaw.WriteByte(const _byValue: Byte);
  {$IFDEF OXYGENE}
    var
      buf : array of Byte ;
  {$ENDIF}
  begin
    {$IFNDEF OXYGENE}
      WriteBytes( 1, _byValue ) ;
    {$ELSE}
      SetLength( buf, 1 ) ;
      buf[0] := _byValue ;
      WriteBytes( 1, buf ) ;
    {$ENDIF}
  end ;

  procedure T_BlockRaw.WriteBytes( const _nBytesToWrite : Integer ;
                                       const _pabySrcBuf    : array of Byte
                                      ) ;
  var
    i : Integer ;
  begin
    if ( pabyBuf = nil ) then exit ;

    if ( (nCurPos + _nBytesToWrite) > nBlockSize ) then exit ;

    for i := 0 to _nBytesToWrite - 1 do
      pabyBuf[ nCurPos + i ] := _pabySrcBuf[ i ] ;

    inc( nCurPos, _nBytesToWrite ) ;

    nSizeUsed := Max( nSizeUsed, nCurPos ) ;
    bModified := True ;
  end ;

  procedure T_BlockRaw.WriteDouble(const _dValue: Double);
  var
    buf : array of Byte ;
  begin
    SetLength( buf, 8 ) ;
    {$IFDEF OXYGENE}
      GisCopyMemory( BitConverter.GetBytes( _dValue ), 0, buf, 0, 8 ) ;
    {$ELSE}
      Move( _dValue, buf[0], 8 ) ;
    {$ENDIF}
    WriteBytes( 8, buf );
  end ;

  procedure T_BlockRaw.WriteInt16(const _n16Value : SmallInt);
  var
    buf : array of Byte ;
  begin
    SetLength( buf, 2 ) ;
    {$IFDEF OXYGENE}
      GisCopyMemory( BitConverter.GetBytes( _n16Value ), 0, buf, 0, 2 ) ;
    {$ELSE}
      Move( _n16Value, buf[0], 2 ) ;
    {$ENDIF}
    WriteBytes( 2, buf );
  end ;

  procedure T_BlockRaw.WriteInt32(const _n32Value: Integer);
  var
    buf : array of Byte ;
  begin
    SetLength( buf, 4 ) ;
    {$IFDEF OXYGENE}
      GisCopyMemory( BitConverter.GetBytes( _n32Value ), 0, buf, 0, 4 ) ;
    {$ELSE}
      Move( _n32Value, buf[0], 4 ) ;
    {$ENDIF}
    WriteBytes( 4, buf );
  end ;

  procedure T_BlockRaw.WriteZeros(const _nBytesToWrite: Integer);
  var
    acZeros : array of Byte ;
    i : Integer ;
  begin
    SetLength( acZeros, 8 ) ;
    {$IFDEF OXYGENE}
      for i := 0 to 7 do
        acZeros[ i ] := 0 ;
    {$ELSE}
      FillChar( acZeros[0], 8, 0 ) ;
    {$ENDIF}

    i := 0 ;
    while ( i < _nBytesToWrite ) do begin
      WriteBytes( Min( 8,(_nBytesToWrite-i)), acZeros ) ;
      inc( i, 8 ) ;
    end ;
  end ;

//==============================================================================
//  T_BlockHeader
//==============================================================================

  procedure T_BlockHeader.CommitToFile;
  var
    i : Integer ;
  begin
    if ( pabyBuf = nil ) or ( nBlockSize <> HDR_DATA_BLOCK_SIZE ) then exit ;

    GotoByteInBlock( $000 ) ;
    {$IFDEF ISLAND}
      var buf : array of Byte := new Byte[HDR_OBJ_LEN_ARRAY_SIZE] ;
      &Array.Copy( gabyObjLenArray, buf, 0, HDR_OBJ_LEN_ARRAY_SIZE ) ;
      WriteBytes( HDR_OBJ_LEN_ARRAY_SIZE, buf ) ;
    {$ELSE}
      WriteBytes( HDR_OBJ_LEN_ARRAY_SIZE, gabyObjLenArray ) ;
    {$ENDIF}
    nMaxObjLenArrayId := HDR_OBJ_LEN_ARRAY_SIZE - 1 ;

    GotoByteInBlock( $100 ) ;
    WriteInt32( HDR_MAGIC_COOKIE ) ;

    if ( sProj.nAffineFlag > 0 ) and ( nMAPVersionNumber < 500 ) then exit ;

    WriteInt16( nMAPVersionNumber ) ;

    WriteInt16( HDR_DATA_BLOCK_SIZE ) ;

    WriteDouble(dCoordsys2DistUnits);
    WriteInt32(hnXMin);
    WriteInt32(hnYMin);
    WriteInt32(hnXMax);
    WriteInt32(hnYMax);

    WriteZeros(16);

    WriteInt32(nFirstIndexBlock);
    WriteInt32(nFirstGarbageBlock);
    WriteInt32(nFirstToolBlock);

    WriteInt32(numPointObjects);
    WriteInt32(numLineObjects);
    WriteInt32(numRegionObjects);
    WriteInt32(numTextObjects);
    WriteInt32(nMaxCoordBufSize);

    WriteZeros(14);

    WriteByte(nDistUnitsCode);
    WriteByte(nMaxSpIndexDepth);
    WriteByte(nCoordPrecision);
    WriteByte(nCoordOriginQuadrant);
    WriteByte(nReflectXAxisCoord);
    WriteByte(nMaxObjLenArrayId);
    WriteByte(numPenDefs);
    WriteByte(numBrushDefs);
    WriteByte(numSymbolDefs);
    WriteByte(numFontDefs);
    WriteInt16(numMapToolBlocks);

    WriteInt16( sProj.nDatumId );
    WriteZeros( 1 );

    WriteByte( sProj.nProjId );
    WriteByte( sProj.nEllipsoidId );
    WriteByte( sProj.nUnitsId );
    WriteDouble( XScale );
    WriteDouble( YScale );
    WriteDouble( XDispl );
    WriteDouble( YDispl );

    for i := 0 to 5 do
      WriteDouble( sProj.adProjParams[ i ] ) ;

    WriteDouble( sProj.dDatumShiftX ) ;
    WriteDouble( sProj.dDatumShiftY ) ;
    WriteDouble( sProj.dDatumShiftZ ) ;

    for i := 0 to 4 do
      WriteDouble( sProj.adDatumParams[ i ] ) ;

    if ( sProj.nAffineFlag > 0 ) then begin

      WriteByte( 1 );
      WriteByte( sProj.nAffineUnits );
      WriteZeros( 6 );
      WriteDouble( sProj.dAffineParamA );
      WriteDouble( sProj.dAffineParamB );
      WriteDouble( sProj.dAffineParamC );
      WriteDouble( sProj.dAffineParamD );
      WriteDouble( sProj.dAffineParamE );
      WriteDouble( sProj.dAffineParamF );

      WriteZeros( 456 );
    end ;

    inherited;
  end ;

  function T_BlockHeader.Coordsys2Int(
    const _ptg             : TGIS_Point ;
    const _bIgnoreOverflow : Boolean
  ) : TPoint ;
  var
    dTempX, dTempY : Double ;
    bIntBoundsOverflow : Boolean ;

    function round_int( const _dx : Double ) : Integer ;
    begin
      if _dx < 0.0 then
        Result := RoundS(_dx-0.5)
      else
        Result := RoundS(_dx+0.5)
    end ;

  begin
    if ( pabyBuf = nil ) then exit ;

    if ( nCoordOriginQuadrant = 2 ) or ( nCoordOriginQuadrant = 3 ) or
       ( nCoordOriginQuadrant = 0 ) then
        dTempX := ( -1.0 * _ptg.X * XScale - XDispl )
    else
        dTempX := ( _ptg.X * XScale + XDispl ) ;

    if ( nCoordOriginQuadrant = 3 ) or ( nCoordOriginQuadrant = 4 ) or
       ( nCoordOriginQuadrant = 0 ) then
        dTempY := ( -1.0 * _ptg.Y * YScale - YDispl )
    else
        dTempY := ( _ptg.Y * YScale + YDispl ) ;

    bIntBoundsOverflow := False ;
    if ( dTempX < -1000000000 ) then begin
      dTempX := -1000000000 ;
      bIntBoundsOverflow := True ;
    end ;

    if ( dTempX > 1000000000 ) then begin
      dTempX := 1000000000 ;
      bIntBoundsOverflow := True ;
    end ;

    if ( dTempY < -1000000000 ) then begin
      dTempY := -1000000000 ;
      bIntBoundsOverflow := True ;
    end ;

    if ( dTempY > 1000000000 ) then begin
      dTempY := 1000000000 ;
      bIntBoundsOverflow := True ;
    end ;

    {$IFDEF GIS_NORECORDS}
      Result := new TPoint(0,0) ;
    {$ENDIF}

    Result.X := round_int( dTempX ) ;
    Result.Y := round_int( dTempY ) ;

    if ( bIntBoundsOverflow and not _bIgnoreOverflow ) then
      bBoundsOverflow := True ;
  end ;

  constructor T_BlockHeader.Create(
    const _eAccessMode    : T_TABAccess ;
    const _bHardBlockSize : Boolean
  ) ;
  var
    i : Integer ;
  begin
    inherited Create( _eAccessMode, True ) ;

    nMAPVersionNumber   := HDR_VERSION_NUMBER ;
    nBlockSize          := HDR_DATA_BLOCK_SIZE ;

    dCoordsys2DistUnits := 1.0 ;
    hnXMin               := -1000000000 ;
    hnYMin               := -1000000000 ;
    hnXMax               := 1000000000 ;
    hnYMax               := 1000000000 ;
    bBoundsOverflow      := False ;

    nFirstIndexBlock    := 0 ;
    nFirstGarbageBlock  := 0 ;
    nFirstToolBlock     := 0 ;

    numPointObjects     := 0 ;
    numLineObjects      := 0 ;
    numRegionObjects    := 0 ;
    numTextObjects      := 0 ;
    nMaxCoordBufSize    := 0 ;

    nDistUnitsCode      := 7 ;
    nMaxSpIndexDepth    := 0 ;
    nCoordPrecision     := 3 ;
    nCoordOriginQuadrant:= HDR_DEF_ORG_QUADRANT ;
    nReflectXAxisCoord  := HDR_DEF_REFLECTXAXIS ;
    nMaxObjLenArrayId   := HDR_OBJ_LEN_ARRAY_SIZE-1 ;
    numPenDefs          := 0 ;
    numBrushDefs        := 0 ;
    numSymbolDefs       := 0 ;
    numFontDefs         := 0 ;
    numMapToolBlocks    := 0 ;

    sProj.nProjId       := 0 ;
    sProj.nEllipsoidId  := 0 ;
    sProj.nUnitsId      := 7 ;
    XScale              := 1000.0 ;
    YScale              := 1000.0 ;
    XDispl              := 0.0 ;
    YDispl              := 0.0 ;

    SetLength( sProj.adProjParams, 6 ) ;
    for i := 0 to 5 do
      sProj.adProjParams[ i ] := 0.0 ;

    sProj.dDatumShiftX := 0.0 ;
    sProj.dDatumShiftY := 0.0 ;
    sProj.dDatumShiftZ := 0.0 ;

    SetLength( sProj.adDatumParams, 5 ) ;
    for i := 0 to 4 do
      sProj.adDatumParams[ i ] := 0.0 ;

    sProj.nAffineFlag   := 0 ;
    sProj.nAffineUnits  := 7 ;
    sProj.dAffineParamA := 0.0 ;
    sProj.dAffineParamB := 0.0 ;
    sProj.dAffineParamC := 0.0 ;
    sProj.dAffineParamD := 0.0 ;
    sProj.dAffineParamE := 0.0 ;
    sProj.dAffineParamF := 0.0 ;

  end ;

  function T_BlockHeader.GetBlockClass: Integer;
  begin
    Result := TABMAP_HEADER_BLOCK ;
  end ;

  function T_BlockHeader.GetMapObjectSize(
    const _nObjType : Integer
  ) : Integer ;
  begin
    if ( pabyBuf = nil ) then begin
      Result := -1 ;
      exit ;
    end ;

    if ( _nObjType < 0 ) or ( _nObjType > 255 ) then begin
      Result := -1 ;
      exit ;
    end ;

    Result := ( pabyBuf[ _nObjType ] and $7f ) ;
  end ;

  procedure T_BlockHeader.InitBlockFromData(
    const _pabyBuf    : array of Byte ;
    const _nBlockSize : Integer ;
    const _nSizeUsed  : Integer ;
    const _bMakeCopy  : Boolean ;
    const _fpSrc      : TGIS_BufferedFileStream ;
    const _nOffset    : Integer
  );
  begin
    assert( False, '' );

  end ;

  function T_BlockHeader.InitNewBlock(
    const _fpSrc       : TGIS_BufferedFileStream ;
    const _nBlockSize  : Integer ;
    const _nFileOffset : Integer
  ) : Integer ;
  var
    i : Integer ;
  begin
    if ( inherited InitNewBlock( _fpSrc, _nBlockSize, _nFileOffset ) <> 0 ) then begin
      Result := -1 ;
      exit ;
    end ;

    nMAPVersionNumber   := HDR_VERSION_NUMBER ;
    nBlockSize          := HDR_DATA_BLOCK_SIZE ;

    dCoordsys2DistUnits := 1.0 ;
    hnXMin               := -1000000000 ;
    hnYMin               := -1000000000 ;
    hnXMax               := 1000000000 ;
    hnYMax               := 1000000000 ;

    nFirstIndexBlock    := 0 ;
    nFirstGarbageBlock  := 0 ;
    nFirstToolBlock     := 0 ;

    numPointObjects     := 0 ;
    numLineObjects      := 0 ;
    numRegionObjects    := 0 ;
    numTextObjects      := 0 ;
    nMaxCoordBufSize    := 0 ;

    nDistUnitsCode      := 7 ;
    nMaxSpIndexDepth    := 0 ;
    nCoordPrecision     := 3 ;
    nCoordOriginQuadrant:= HDR_DEF_ORG_QUADRANT ;
    nReflectXAxisCoord  := HDR_DEF_REFLECTXAXIS ;
    nMaxObjLenArrayId   := HDR_OBJ_LEN_ARRAY_SIZE-1 ;
    numPenDefs          := 0 ;
    numBrushDefs        := 0 ;
    numSymbolDefs       := 0 ;
    numFontDefs         := 0 ;
    numMapToolBlocks    := 0 ;

    sProj.nProjId       := 0 ;
    sProj.nEllipsoidId  := 0 ;
    sProj.nUnitsId      := 7 ;
    sProj.nDatumId      := 0 ;
    XScale              := 1000.0 ;
    YScale              := 1000.0 ;
    XDispl              := 0.0 ;
    YDispl              := 0.0 ;

    for i := 0 to 5 do
      sProj.adProjParams[ i ] := 0.0 ;

    sProj.dDatumShiftX := 0.0 ;
    sProj.dDatumShiftY := 0.0 ;
    sProj.dDatumShiftZ := 0.0 ;

    for i := 0 to 4 do
      sProj.adDatumParams[ i ] := 0.0 ;

    sProj.nAffineFlag := 0 ;

    GotoByteInBlock( $000 ) ;
    {$IFDEF ISLAND}
      var buf : array of Byte := new Byte[HDR_OBJ_LEN_ARRAY_SIZE] ;
      &Array.Copy( gabyObjLenArray, buf, 0, HDR_OBJ_LEN_ARRAY_SIZE ) ;
      WriteBytes( HDR_OBJ_LEN_ARRAY_SIZE, buf ) ;
    {$ELSE}
      WriteBytes( HDR_OBJ_LEN_ARRAY_SIZE, gabyObjLenArray ) ;
    {$ENDIF}

    Result := 0 ;
  end ;

  function T_BlockHeader.Int2Coordsys(
    const _pt : TPoint
  ) : TGIS_Point ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point ;
    {$ENDIF}
    if ( pabyBuf = nil ) then exit ;

    if ( nCoordOriginQuadrant = 2 ) or
       ( nCoordOriginQuadrant = 3 ) or
       ( nCoordOriginQuadrant = 0 ) then
        Result.X := -1.0 * ( _pt.X + XDispl ) / XScale
    else
        Result.X := ( _pt.X - XDispl ) / XScale ;

    if ( nCoordOriginQuadrant = 3 ) or
       ( nCoordOriginQuadrant = 4 ) or
       ( nCoordOriginQuadrant = 0 ) then
        Result.Y := -1.0 * ( _pt.Y + YDispl ) / YScale
    else
        Result.Y := ( _pt.Y - YDispl ) / YScale;
  end ;

  function T_BlockHeader.MapObjectUsesCoordBlock(
    const _nObjType : Integer
  ) : Boolean ;
  begin
    Result := False ;

    if ( pabyBuf = nil ) then exit ;

    if ( _nObjType < 0 ) or ( _nObjType > 255 ) then exit ;

    if ((pabyBuf[_nObjType] and $80) <> 0) then
      Result := True
    else
      Result := False ;
  end ;

  procedure T_BlockHeader.SetCoordsysBounds(
    var _ext : TGIS_Extent
  ) ;
  begin
    if _ext.XMax = _ext.XMin then begin
      _ext.XMin := _ext.XMin - 1.0 ;
      _ext.XMax := _ext.XMax + 1.0 ;
    end ;

    if _ext.YMax = _ext.YMin then begin
      _ext.YMin := _ext.YMin - 1.0 ;
      _ext.YMax := _ext.YMax + 1.0 ;
    end ;

    XScale := 2e9 / ( _ext.XMax - _ext.XMin ) ;
    YScale := 2e9 / ( _ext.YMax - _ext.YMin ) ;

    XDispl := -1.0 * XScale * ( _ext.XMax + _ext.XMin ) / 2 ;
    YDispl := -1.0 * YScale * ( _ext.YMax + _ext.YMin ) / 2 ;

    hnXMin := -1000000000 ;
    hnYMin := -1000000000 ;
    hnXMax :=  1000000000 ;
    hnYMax :=  1000000000 ;
  end ;

//==============================================================================
//  T_BlockRef
//==============================================================================

  constructor T_BlockRef.Create ;
  begin
    inherited ;

    nBlockPtr := 0   ;
    psNext    := nil ;
  end;

//==============================================================================
// T_Feature
//==============================================================================

  constructor T_Feature.Create ;
  begin
    inherited Create ;

    FMapInfoType  := TAB_GEONONE ;
    FFeatureClass := TABFC_NoGeom ;

    FComprOrg           := Point( 0, 0 ) ;

    FBoundingRect.XMin  := 0 ;
    FBoundingRect.YMin  := 0 ;
    FBoundingRect.XMax  := 0 ;
    FBoundingRect.YMax  := 0 ;

    FShape              := nil ;

    FPenDefIndex               := 0  ;
    {$IFDEF GIS_NORECORDS}
      FPenDef := new TGIS_TABPenDef ;
    {$ENDIF}
    FPenDef.nRefCount          := 0  ;
    FPenDef.nPixelWidth        := 1  ;
    FPenDef.nLinePattern       := 2  ;
    FPenDef.nPointWidth        := 0  ;
    FPenDef.rgbColor           := TGIS_Color.Black ;

    FBrushDefIndex             := 0  ;
    {$IFDEF GIS_NORECORDS}
      FBrushDef := new TGIS_TABBrushDef ;
    {$ENDIF}
    FBrushDef.nRefCount        := 0  ;
    FBrushDef.nFillPattern     := 1  ;
    FBrushDef.bTransparentFill := 0  ;
    FBrushDef.rgbFGColor       := TGIS_Color.Black ;
    FBrushDef.rgbBGColor       := TGIS_Color.White ;

    FSymbolDefIndex            := 0  ;
    {$IFDEF GIS_NORECORDS}
      FSymbolDef := new TGIS_TABSymbolDef ;
    {$ENDIF}
    FSymbolDef.nRefCount       := 0  ;
    FSymbolDef.nSymbolNo       := 35 ;
    FSymbolDef.nPointSize      := 12 ;
    FSymbolDef.nUnknownValue   := 0  ;
    FSymbolDef.rgbColor        := TGIS_Color.Black ;

    FFontDefIndex              := 0  ;
    {$IFDEF GIS_NORECORDS}
      FFontDef := new TGIS_TABFontDef ;
    {$ENDIF}
    FFontDef.nRefCount         := 0  ;
    FFontDef.szFontName        := 'Arial' ;

    SetExtent( GisExtent( 0.0, 0.0, 0.0, 0.0 ) ) ;

  end ;

  procedure T_Feature.SetBrushBGColor(
    const _color : TGIS_Color
  ) ;
  begin
    FBrushDef.rgbBGColor := _color ;
  end ;

  procedure T_Feature.SetBrushFGColor(
    const _color : TGIS_Color
  ) ;
  begin
    FBrushDef.rgbFGColor := _color ;
  end ;

  procedure T_Feature.SetBrushPattern(
    const _val : Byte
  ) ;
  begin
    FBrushDef.nFillPattern := _val ;
  end ;

  procedure T_Feature.SetBrushTransparent(
    const _val : Byte
  ) ;
  begin
    FBrushDef.bTransparentFill := _val ;
  end ;

  procedure T_Feature.SetExtent(
    const _ext : TGIS_Extent
  ) ;
  begin
    {$IFDEF GIS_NORECORDS}
      FExtent := new TGIS_Extent ;
    {$ENDIF}
    FExtent.XMin := Min( _ext.XMin, _ext.XMax ) ;
    FExtent.YMin := Min( _ext.YMin, _ext.YMax ) ;
    FExtent.XMax := Max( _ext.XMin, _ext.XMax ) ;
    FExtent.YMax := Max( _ext.YMin, _ext.YMax ) ;
  end ;

  procedure T_Feature.SetPenColor(
    const _color : TGIS_Color
  ) ;
  begin
    FPenDef.rgbColor := _color ;
  end ;

  procedure T_Feature.SetPenPattern(
    const _val : Byte
  ) ;
  begin
    FPenDef.nLinePattern := _val ;
  end ;

  procedure T_Feature.SetPenWidth(
    const _width : Integer
  ) ;
  begin
    if (_width > 10) then begin
      FPenDef.nPointWidth := Min((_width-10), 2037) ;
      FPenDef.nPixelWidth := 0 ;
    end
    else begin
      FPenDef.nPixelWidth := Min(Max(_width, 1), 7) ;
      FPenDef.nPointWidth := 0 ;
    end ;
  end ;

  procedure T_Feature.SetSymbolColor(
    const _color : TGIS_Color
  ) ;
  begin
    FSymbolDef.rgbColor := _color ;
  end ;

  procedure T_Feature.SetSymbolNo(
    const _no : Word
  ) ;
  begin
    FSymbolDef.nSymbolNo := _no ;
  end ;

  procedure T_Feature.SetSymbolSize(
    const _size : Word
  ) ;
  begin
    FSymbolDef.nPointSize := _size ;
  end ;

  procedure T_Feature.UpdateBoundingRect(
    const _mapfile : T_MAPFile
  ) ;
  var
    ext     : TGIS_Extent ;
    pt_tmp  : TPoint      ;
  begin
    if assigned( FShape ) then begin
      ext := FShape.Extent ;

      FExtent := ext ;

      if assigned( _mapfile ) then begin
        pt_tmp := _mapfile.Coordsys2Int( GisPoint( ext.XMin, ext.YMin ), False ) ;
        FBoundingRect.XMin := pt_tmp.X ;
        FBoundingRect.YMin := pt_tmp.Y ;

        pt_tmp := _mapfile.Coordsys2Int( GisPoint( ext.XMax, ext.YMax ), False ) ;
        FBoundingRect.XMax := pt_tmp.X ;
        FBoundingRect.YMax := pt_tmp.Y ;
      end ;
    end ;
  end ;

  function T_Feature.ValidateCoordType(
    const _mapfile : T_MAPFile
  ) : Boolean ;
  var
    bCompr : Boolean ;
  begin
    bCompr := False ;

    UpdateBoundingRect( _mapfile ) ;
    if ( ( FBoundingRect.XMax - FBoundingRect.XMin ) < 65536 ) and
       ( ( FBoundingRect.YMax - FBoundingRect.YMin ) < 65536 )
    then
      bCompr := True ;

    FComprOrg.X := TruncS( 1.0 * ( FBoundingRect.XMin + FBoundingRect.XMax ) / 2 ) ;
    FComprOrg.Y := TruncS( 1.0 * ( FBoundingRect.YMin + FBoundingRect.YMax ) / 2 ) ;

    if          bCompr and ( ( FMapInfoType mod 3 ) = 2 ) then
                dec( FMapInfoType )
    else if not bCompr and ( ( FMapInfoType mod 3 ) = 1 ) then
                inc( FMapInfoType );

    Result := bCompr ;
  end ;

  function T_Feature.Validate(
    const _mapfile : T_MAPFile
  ) : Integer ;
  begin
    FMapInfoType := TAB_GEONONE ;
    Result := FMapInfoType ;
  end ;

  procedure T_Feature.Write(
    const _mapfile    : T_MAPFile   ;
    const _object     : T_Object
  ) ;
  begin

  end ;

//==============================================================================
// TABPoint
//==============================================================================

  constructor T_FeaturePoint.Create ;
  begin
    inherited ;

    FFeatureClass := TABFC_Point ;
  end ;

  function T_FeaturePoint.Validate(
    const _mapfile : T_MAPFile
  ) : Integer ;
  begin
    if FShape.ShapeType = TGIS_ShapeType.Point
      then FMapInfoType := TAB_GEOSYMBOL
      else FMapInfoType := TAB_GEONONE ;

    UpdateBoundingRect( _mapfile ) ;

    Result := FMapInfoType ;
  end ;

  procedure T_FeaturePoint.Write(
    const _mapfile    : T_MAPFile   ;
    const _object     : T_Object
  ) ;
  var
    pt          : TPoint ;
    poPoint     : TGIS_ShapePoint ;
    ptg         : TGIS_Point ;
    poPointHdr  : T_MAPObjPoint ;
  begin
    assert( FMapInfoType = _object.nType ) ;

    poPoint := TGIS_ShapePoint( FShape ) ;

    if assigned( poPoint ) then
      ptg := poPoint.GetPoint( 0, 0 ) ;

    pt := _mapfile.Coordsys2Int( ptg, False ) ;

    poPointHdr := T_MAPObjPoint( _object ) ;

    poPointHdr.Pos := pt ;
    poPointHdr.SetBoundingRect( IntExtent( pt.X, pt.Y, pt.X, pt.Y ) ) ;

    FSymbolDefIndex := _mapfile.WriteSymbolDef( FSymbolDef ) ;
    poPointHdr.nSymbolId := FSymbolDefIndex ;
  end ;

//==============================================================================
// T_TABMultiPoint
//==============================================================================

  constructor T_FeatureMultiPoint.Create;
  begin
    inherited;

    FCenterIsSet  := False ;
    FFeatureClass := TABFC_MultiPoint ;
  end ;

  function T_FeatureMultiPoint.GetCenter(
    var _ptg : TGIS_Point
  ) : Integer ;
  var
    nPoints : Integer ;
    ptg     : TGIS_Point ;
  begin
    if assigned( FShape ) then begin
      nPoints := FShape.GetNumPoints ;
      ptg := FShape.GetPoint( 0, 0 ) ;
    end
    else
      nPoints := 0 ;

    if not FCenterIsSet and ( nPoints > 0 ) then begin
      FCenter      := ptg  ;
      FCenterIsSet := True ;
    end ;

    if not FCenterIsSet then begin
      Result := -1 ;
      exit ;
    end ;

    _ptg := FCenter ;

    Result := 0 ;
  end ;

  function T_FeatureMultiPoint.Validate(
    const _mapfile: T_MAPFile
  ) : Integer ;
  begin
    if FShape.ShapeType = TGIS_ShapeType.MultiPoint
      then FMapInfoType := TAB_GEOMULTIPOINT
      else FMapInfoType := TAB_GEONONE ;

    ValidateCoordType( _mapfile ) ;

    Result := FMapInfoType ;
  end ;

  procedure T_FeatureMultiPoint.Write(
    const _mapfile    : T_MAPFile   ;
    const _object     : T_Object
  ) ;
  var
    pt          : TPoint ;
    pt_tmp      : TPoint ;
    poMPointHdr : TABMAPObjMultiPoint ;
    poMPoint    : TGIS_ShapeMultiPoint ;
    poCoordBlock: T_BlockCoord ;
    bCompressed : Boolean ;
    i           : Integer ;
    ptg         : TGIS_Point ;
    ptgc : TGIS_Point ;
  begin
    assert( FMapInfoType = _object.nType ) ;

    poMPointHdr := TABMAPObjMultiPoint( _object ) ;
    poMPoint    := TGIS_ShapeMultiPoint( FShape ) ;

    poMPointHdr.nNumPoints := poMPoint.GetNumPoints ;

    bCompressed := _object.IsCompressedType ;

    poCoordBlock := _mapfile.poCurCoordBlock ;

    poCoordBlock.StartNewFeature ;
    poMPointHdr.nCoordBlockPtr := poCoordBlock.GetCurAddress ;
    poCoordBlock.SetComprCoordOrigin( FComprOrg ) ;

    for i := 0 to poMPointHdr.nNumPoints - 1 do begin

      ptg := poMPoint.GetPoint( 0, i ) ;
      pt := _mapfile.Coordsys2Int( ptg, False ) ;
      if (i = 0) then begin
        poMPointHdr.nLabelX := pt.X ;
        poMPointHdr.nLabelY := pt.Y ;
      end ;

      poCoordBlock.WriteIntCoord( pt, bCompressed ) ;
    end ;

    poMPointHdr.ComprOrg := FComprOrg ;

    poMPointHdr.nCoordDataSize := poCoordBlock.nFeatureDataSize ;
    poMPointHdr.SetBoundingRect( FBoundingRect ) ;

    if GetCenter( ptgc ) <> -1 then
      pt_tmp := _mapfile.Coordsys2Int( ptgc, False ) ;
      poMPointHdr.nLabelX := pt_tmp.X ;
      poMPointHdr.nLabelY := pt_tmp.Y ;

      FSymbolDefIndex := _mapfile.WriteSymbolDef( FSymbolDef ) ;
      poMPointHdr.nSymbolId := FSymbolDefIndex ;
  end ;

//==============================================================================
// T_TABPolyline
//==============================================================================

  constructor T_FeaturePolyline.Create;
  begin
    inherited;

    bCenterIsSet := False;
    bSmooth      := False;
    bWriteLineAsPolyline := False;
    FFeatureClass := TABFC_Polyline ;
  end ;

  function T_FeaturePolyline.GetCenter(
    var _ptg : TGIS_Point
  ) : Integer ;
  var
    ptgA,
    ptgB : TGIS_Point ;
    i    : Integer ;
  begin
    Result := -1 ;
    if not assigned( FShape ) then exit ;

    if ( not bCenterIsSet) then begin

      if FShape.GetNumPoints > 0 then begin

        i := FShape.GetNumPoints div 2 ;
        if ( FShape.GetNumPoints mod 2 ) = 0 then begin
          ptgA := FShape.GetPoint( 0, i-1 ) ;
          ptgB := FShape.GetPoint( 0, i ) ;

          dCenterX := ( ptgA.X + ptgB.X ) / 2.0 ;
          dCenterY := ( ptgA.Y + ptgB.Y ) / 2.0 ;
        end
        else begin
          ptgB := FShape.GetPoint( 0, i ) ;

          dCenterX := ptgB.X ;
          dCenterY := ptgB.Y ;
        end ;

        bCenterIsSet := TRUE;
      end ;
    end ;

    if ( not bCenterIsSet ) then begin
      Result := -1 ;
      exit ;
    end ;

    {$IFDEF GIS_NORECORDS}
      _ptg := new TGIS_Point ;
    {$ENDIF}
    _ptg.X := dCenterX ;
    _ptg.Y := dCenterY ;

    Result := 0;
  end ;

  function T_FeaturePolyline.Validate(
    const _mapfile : T_MAPFile
  ) : Integer ;
  var
    poMultiLine : TGIS_ShapeArc ;
    poLine      : TGIS_ShapeArc ;
    nParts      : Integer ;
    nPoints     : Integer ;
    numPointsTotal : Integer ;
  begin
    FMapInfoType := TAB_GEONONE ;
    Result := TAB_GEONONE ;

    if not assigned( FShape ) then exit ;

    nParts := FShape.GetNumParts ;

    if      ( FShape.ShapeType = TGIS_ShapeType.Arc ) and ( nParts = 1 ) then begin

            poLine  := TGIS_ShapeArc( FShape ) ;
            nPoints := poLine.GetNumPoints ;

            if      ( nPoints > TAB_300_MAX_VERTICES) then
                    FMapInfoType := TAB_GEOV450_MULTIPLINE
            else if ( nPoints > 2 ) then
                    FMapInfoType := TAB_GEOPLINE
            else if ( nPoints = 2 ) and bWriteLineAsPolyline then
                    FMapInfoType := TAB_GEOPLINE
            else if ( nPoints = 2 ) and ( not bWriteLineAsPolyline ) then
                    FMapInfoType := TAB_GEOLINE
            else    FMapInfoType := TAB_GEONONE
    end
    else if ( FShape.ShapeType = TGIS_ShapeType.Arc ) and ( nParts > 1 ) then begin
            poMultiLine := TGIS_ShapeArc( FShape ) ;
            FMapInfoType := TAB_GEOMULTIPLINE;
            numPointsTotal := poMultiLine.GetNumPoints ;

            if numPointsTotal > TAB_300_MAX_VERTICES then
              FMapInfoType := TAB_GEOV450_MULTIPLINE ;
    end
    else    FMapInfoType := TAB_GEONONE ;

    if FMapInfoType <> TAB_GEOLINE then ValidateCoordType ( _mapfile )
                                   else UpdateBoundingRect( _mapfile ) ;

    Result := FMapInfoType ;
  end ;

  procedure T_FeaturePolyline.Write(
    const _mapfile : T_MAPFile   ;
    const _object  : T_Object
  ) ;
  var
    poLine        : TGIS_ShapeArc ;
    poCoordBlock  : T_BlockCoord ;
    poLineHdr     : T_MAPObjLine ;
    ptg1, ptg2    : TGIS_Point ;
    i, j, numPoints  : Integer ;
    nCoordDataSize: Integer ;
    nCoordBlockPtr: Integer ;
    bCompressed   : Boolean ;
    poPLineHdr    : T_MAPObjPLine ;
    numLines      : Integer ;
    pasSecHdrs    : T_SectionArray ;
    bV450         : Boolean ;
    nTotalHdrSizeUncompressed : Integer ;
    ext           : TGIS_Extent ;
    numPointsTotal: Integer ;
    ptgc : TGIS_Point ;
    pt_tmp : TPoint ;
  begin
    assert( FMapInfoType = _object.nType ) ;

    if ((FMapInfoType = TAB_GEOLINE) or (FMapInfoType = TAB_GEOLINE_C )) and
       ( (FShape.ShapeType = TGIS_ShapeType.Arc) and (FShape.GetNumPoints = 2)) then begin

        poLineHdr := T_MAPObjLine( _object ) ;
        poLine    := TGIS_ShapeArc( FShape ) ;
        ptg1      := poLine.GetPoint( 0, 0 ) ;
        ptg2      := poLine.GetPoint( 0, 1 ) ;

        poLineHdr.A := _mapfile.Coordsys2Int( ptg1, False ) ;

        poLineHdr.B := _mapfile.Coordsys2Int( ptg2, False ) ;

        poLineHdr.SetBoundingRect( IntExtent( poLineHdr.A.X, poLineHdr.A.Y,
                                              poLineHdr.B.X, poLineHdr.B.Y
                                            )
                                  ) ;

        FPenDefIndex     := _mapfile.WritePenDef( FPenDef ) ;
        poLineHdr.PenId := FPenDefIndex ;
    end
    else if ((FMapInfoType = TAB_GEOPLINE) or (FMapInfoType = TAB_GEOPLINE_C )) and
            (FShape.ShapeType = TGIS_ShapeType.Arc) then begin

        bCompressed := _object.IsCompressedType ;

        poLine    := TGIS_ShapeArc( FShape ) ;
        numPoints := poLine.GetNumPoints ;
        assert( numPoints <= TAB_300_MAX_VERTICES ) ;

        poCoordBlock := _mapfile.poCurCoordBlock ;

        poCoordBlock.StartNewFeature ;
        nCoordBlockPtr := poCoordBlock.GetCurAddress ;
        poCoordBlock.SetComprCoordOrigin( FComprOrg ) ;

        for i := 0 to numPoints-1 do begin
          ptg1 := poLine.GetPoint( 0, i ) ;
          poCoordBlock.WriteIntCoord( _mapfile.Coordsys2Int( ptg1, False ), bCompressed ) ;
        end ;

        nCoordDataSize := poCoordBlock.nFeatureDataSize ;

        poPLineHdr := T_MAPObjPLine( _object ) ;

        poPLineHdr.nCoordBlockPtr  := nCoordBlockPtr ;
        poPLineHdr.nCoordDataSize  := nCoordDataSize ;
        poPLineHdr.LineSections := 1 ;
        poPLineHdr.Smooth         := bSmooth ;

        poPLineHdr.SetBoundingRect( FBoundingRect ) ;

        if (GetCenter(ptgc) <> -1) then begin
          poPLineHdr.LabelPos := _mapfile.Coordsys2Int( ptgc, False ) ;
        end
        else begin
          poPLineHdr.LabelPos := FComprOrg ;
        end ;

        poPLineHdr.ComprOrg := FComprOrg ;

        FPenDefIndex := _mapfile.WritePenDef( FPenDef ) ;
        poPLineHdr.PenId := FPenDefIndex ;
    end
    else if ((FMapInfoType = TAB_GEOMULTIPLINE) or
             (FMapInfoType = TAB_GEOMULTIPLINE_C) or
             (FMapInfoType = TAB_GEOV450_MULTIPLINE) or
             (FMapInfoType = TAB_GEOV450_MULTIPLINE_C) ) and
            ( (FShape.ShapeType = TGIS_ShapeType.Arc) ) then begin

        bCompressed := _object.IsCompressedType ;

        poCoordBlock := _mapfile.poCurCoordBlock ;

        poCoordBlock.StartNewFeature ;
        nCoordBlockPtr := poCoordBlock.GetCurAddress ;
        poCoordBlock.SetComprCoordOrigin( FComprOrg ) ;

        numLines := FShape.GetNumParts ;
        SetLength( pasSecHdrs, numLines ) ;

        bV450 := ( FMapInfoType = TAB_GEOV450_MULTIPLINE   ) or
                 ( FMapInfoType = TAB_GEOV450_MULTIPLINE_C ) ;

        if (bV450) then
          nTotalHdrSizeUncompressed := 28 * numLines
        else
          nTotalHdrSizeUncompressed := 24 * numLines ;

        poLine := TGIS_ShapeArc( FShape );
        numPointsTotal := 0 ;

        for i := 0 to numLines-1 do begin
          {$IFDEF GIS_NORECORDS}
            pasSecHdrs[i] := new T_Section ;
          {$ENDIF}
          ext := poLine.Extent ;
          pasSecHdrs[i].FVertices := poLine.GetPartSize(i) ;
          pasSecHdrs[i].FHoles    := 0 ;

          pt_tmp := _mapfile.Coordsys2Int( GisPoint( ext.XMin, ext.YMin ), False ) ;
          pasSecHdrs[i].FBoundingRect.XMin := pt_tmp.X ;
          pasSecHdrs[i].FBoundingRect.YMin := pt_tmp.Y ;

          pt_tmp := _mapfile.Coordsys2Int( GisPoint( ext.XMax, ext.YMax ), False ) ;
          pasSecHdrs[i].FBoundingRect.XMax := pt_tmp.X ;
          pasSecHdrs[i].FBoundingRect.YMax := pt_tmp.X ;

          pasSecHdrs[i].FDataOffset := nTotalHdrSizeUncompressed +
                                           numPointsTotal*4*2;
          pasSecHdrs[i].FVertexOffset := numPointsTotal;

          inc( numPointsTotal, poLine.GetPartSize(i) );
        end ;

        poCoordBlock.WriteCoordSecHdrs( bV450, numLines, pasSecHdrs, bCompressed );

        pasSecHdrs := nil;

        poLine := TGIS_ShapeArc( FShape ) ;
        for i := 0 to numLines-1 do begin

          numPoints := poLine.GetPartSize(i) ;

          for j := 0 to numPoints-1 do begin
            ptg1 := poLine.GetPoint( i, j ) ;
            poCoordBlock.WriteIntCoord( _mapfile.Coordsys2Int( ptg1, False ), bCompressed );
          end ;
        end ;

        nCoordDataSize := poCoordBlock.nFeatureDataSize ;

        poPLineHdr := T_MAPObjPLine( _object ) ;

        poPLineHdr.nCoordBlockPtr   := nCoordBlockPtr ;
        poPLineHdr.nCoordDataSize   := nCoordDataSize ;
        poPLineHdr.LineSections  := numLines ;
        poPLineHdr.Smooth          := bSmooth ;

        poPLineHdr.SetBoundingRect( FBoundingRect ) ;

        if (GetCenter( ptgc ) <> -1) then begin
          poPLineHdr.LabelPos := _mapfile.Coordsys2Int( ptgc, False ) ;
        end
        else begin
          poPLineHdr.LabelPos := FComprOrg ;
        end ;

        poPLineHdr.ComprOrg := FComprOrg ;

        FPenDefIndex := _mapfile.WritePenDef( FPenDef ) ;
        poPLineHdr.PenId := FPenDefIndex ;
    end ;
  end ;

//==============================================================================
// T_TABRegion
//==============================================================================

  procedure T_FeatureRegion.AppendSections(
    const _shape     : TGIS_Shape     ;
    var   _sections  : T_SectionArray ;
    const _mapfile   : T_MAPFile      ;
    var   _last_ring : Integer
  );
  var
    iRing,
    numRingsInPolygon : Integer ;
    ext        : TGIS_Extent ;
    pt_tmp : TPoint ;
  begin
    numRingsInPolygon := _shape.GetNumParts ;
    ext               := _shape.Extent ;

    {$IFDEF GIS_NORECORDS}
      _sections[_last_ring] := new T_Section ;
    {$ENDIF}
    for iRing := 0 to numRingsInPolygon-1 do begin

      _sections[_last_ring].FVertices := _shape.GetPartSize( iRing ) ;

      if (iRing = 0) then
        _sections[_last_ring].FHoles := numRingsInPolygon -1
      else
        _sections[_last_ring].FHoles := 0;

      pt_tmp := _mapfile.Coordsys2Int( GisPoint( ext.XMin, ext.YMin ), False ) ;
      _sections[_last_ring].FBoundingRect.XMin := pt_tmp.X ;
      _sections[_last_ring].FBoundingRect.YMin := pt_tmp.Y ;

      pt_tmp := _mapfile.Coordsys2Int( GisPoint( ext.XMax, ext.YMax ), False ) ;
      _sections[_last_ring].FBoundingRect.XMax := pt_tmp.X ;
      _sections[_last_ring].FBoundingRect.YMax := pt_tmp.Y ;

      inc( _last_ring ) ;
    end ;
  end ;

  function T_FeatureRegion.ComputeNumRings(
    var _sections: T_SectionArray; const _mapfile: T_MAPFile) : Integer ;
  var
    numRingsTotal : Integer ;
    iLastSect     : Integer ;
    numPointsTotal: Integer ;
    iRing         : Integer ;
    nTotalHdrSizeUncompressed : Integer ;
  begin
    numRingsTotal := 0 ;
    iLastSect     := 0 ;

    if assigned(FShape) and (FShape.ShapeType = TGIS_ShapeType.Polygon ) then begin
      numRingsTotal := FShape.GetNumParts ;
      AppendSections( FShape, _sections, _mapfile, iLastSect ) ;
    end ;

    if ( FMapInfoType = TAB_GEOV450_REGION   ) or
       ( FMapInfoType = TAB_GEOV450_REGION_C ) then
        nTotalHdrSizeUncompressed := 28 * numRingsTotal
    else
        nTotalHdrSizeUncompressed := 24 * numRingsTotal;

    numPointsTotal := 0;
    assert( iLastSect = numRingsTotal ) ;
    for iRing := 0 to numRingsTotal-1 do begin
      {$IFDEF GIS_NORECORDS}
        _sections[iRing] := new T_Section ;
      {$ENDIF}
      _sections[iRing].FDataOffset := nTotalHdrSizeUncompressed +
                                             numPointsTotal*4*2;
      _sections[iRing].FVertexOffset := numPointsTotal;

      inc( numPointsTotal , _sections[iRing].FVertices ) ;
    end ;

    Result := numRingsTotal;
  end ;

  constructor T_FeatureRegion.Create;
  begin
    inherited;

    bSmooth       := False;
    FFeatureClass := TABFC_Region ;
  end ;

  function T_FeatureRegion.GetCenter(
    var _ptg : TGIS_Point
  ) : Integer ;
  begin
    Result := -1 ;
    if not assigned( FShape ) then exit ;

    _ptg := FShape.Centroid ;

    Result := 0 ;
  end ;

  function T_FeatureRegion.Validate(
    const _mapfile : T_MAPFile
  ) : Integer ;
  var
    numPointsTotal : Integer ;
  begin
    if (FShape.ShapeType = TGIS_ShapeType.Polygon) then begin

      numPointsTotal := FShape.GetNumPoints ;

      if (numPointsTotal > TAB_300_MAX_VERTICES) then
        FMapInfoType := TAB_GEOV450_REGION
      else
        FMapInfoType := TAB_GEOREGION;
    end
    else
      FMapInfoType := TAB_GEONONE ;

    ValidateCoordType( _mapfile ) ;

    Result := FMapInfoType ;
  end ;

  procedure T_FeatureRegion.Write(
    const _mapfile : T_MAPFile   ;
    const _object  : T_Object
  ) ;
  var
    poCoordBlock    : T_BlockCoord ;
    i, iRing        : Integer ;
    numRingsTotal   : Integer ;
    nCoordDataSize  : Integer ;
    nCoordBlockPtr  : Integer ;
    numPoints       : Integer ;
    pasSecHdrs      : T_SectionArray;
    bCompressed     : Boolean ;
    bV450           : Boolean ;
    ptg             : TGIS_Point ;
    poPLineHdr      : T_MAPObjPLine ;
    ptgc : TGIS_Point ;
  begin
    assert( FMapInfoType = _object.nType ) ;

    if (( FMapInfoType = TAB_GEOREGION ) or
        ( FMapInfoType = TAB_GEOREGION_C ) or
        ( FMapInfoType = TAB_GEOV450_REGION ) or
        ( FMapInfoType = TAB_GEOV450_REGION_C )) and
        ( FShape.ShapeType = TGIS_ShapeType.Polygon ) then
    begin
      pasSecHdrs  := nil ;
      bCompressed := _object.IsCompressedType ;

      poCoordBlock := _mapfile.poCurCoordBlock ;

      poCoordBlock.StartNewFeature ;
      nCoordBlockPtr := poCoordBlock.GetCurAddress ;
      poCoordBlock.SetComprCoordOrigin( FComprOrg ) ;

      SetLength( pasSecHdrs, FShape.GetNumParts ) ;
      numRingsTotal := ComputeNumRings( pasSecHdrs, _mapfile ) ;

      bV450 := ( FMapInfoType = TAB_GEOV450_REGION    ) or
               ( FMapInfoType = TAB_GEOV450_REGION_C  ) ;

      poCoordBlock.WriteCoordSecHdrs( bV450, numRingsTotal, pasSecHdrs, bCompressed );

      pasSecHdrs := nil ;

      for iRing := 0 to numRingsTotal-1 do begin

        numPoints := FShape.GetPartSize( iRing ) ;

        for i := 0 to numPoints-1 do begin
          ptg := FShape.GetPoint( iRing, i ) ;
          poCoordBlock.WriteIntCoord( _mapfile.Coordsys2Int( ptg, False ), bCompressed ) ;
        end ;
      end ;

      nCoordDataSize := poCoordBlock.nFeatureDataSize ;

      poPLineHdr := T_MAPObjPLine( _object );

      poPLineHdr.nCoordBlockPtr   := nCoordBlockPtr ;
      poPLineHdr.nCoordDataSize   := nCoordDataSize ;
      poPLineHdr.LineSections  := numRingsTotal ;

      poPLineHdr.Smooth := bSmooth;

      poPLineHdr.SetBoundingRect( FBoundingRect ) ;

      if ( GetCenter( ptgc ) <> -1) then begin
        poPLineHdr.LabelPos := _mapfile.Coordsys2Int( ptgc, False ) ;
      end
      else begin
        poPLineHdr.LabelPos := FComprOrg ;
      end ;

      poPLineHdr.ComprOrg := FComprOrg ;

      FPenDefIndex       := _mapfile.WritePenDef( FPenDef ) ;
      poPLineHdr.PenId   := FPenDefIndex ;

      FBrushDefIndex     := _mapfile.WriteBrushDef( FBrushDef ) ;
      poPLineHdr.BrushId := FBrushDefIndex ;
    end
  end ;

//==============================================================================
// T_BlockObject
//==============================================================================

  procedure T_BlockObject.AddCoordBlockRef(
    const _nCoordBlockAddress : Integer
  ) ;
  begin
    if ( nFirstCoordBlock = 0 ) then
      nFirstCoordBlock := _nCoordBlockAddress ;

    nLastCoordBlock := _nCoordBlockAddress ;
  end ;

  procedure T_BlockObject.CommitNewObject(
    const _object : T_Object
  ) ;
  begin
    if ( _object.nType = TAB_GEONONE ) then exit ;

    assert( nCurObjectId = _object.nId ) ;

    GotoByteInBlock( nCurObjectOffset ) ;

    _object.WriteObj( self ) ;

    numDataBytes := nSizeUsed - MAP_OBJECT_HEADER_SIZE ;
  end ;

  procedure T_BlockObject.CommitToFile;
  begin
    if ( pabyBuf = nil ) then exit ;

    GotoByteInBlock( $000 ) ;

    WriteInt16( TABMAP_OBJECT_BLOCK ) ;
    numDataBytes := nSizeUsed - MAP_OBJECT_HEADER_SIZE ;
    WriteInt16( numDataBytes ) ;

    WriteInt32( nCenterX ) ;
    WriteInt32( nCenterY ) ;

    WriteInt32( nFirstCoordBlock ) ;
    WriteInt32( nLastCoordBlock  ) ;

    inherited;
  end ;

  constructor T_BlockObject.Create(
    const _eAccessMode    : T_TABAccess ;
    const _bHardBlockSize : Boolean
  ) ;
  begin
    inherited Create( _eAccessMode, True ) ;

  end ;

  procedure T_BlockObject.doDestroy;
  begin
    FBoundingRect := IntExtent(  1000000000,  1000000000,
                                -1000000000, -1000000000
                              ) ;

    inherited;
  end ;

  function T_BlockObject.GetBlockClass: Integer;
  begin
    Result := TABMAP_OBJECT_BLOCK ;
  end ;

  procedure T_BlockObject.InitBlockFromData(
    const _pabyBuf    : array of Byte ;
    const _nBlockSize : Integer ;
    const _nSizeUsed  : Integer ;
    const _bMakeCopy  : Boolean ;
    const _fpSrc      : TGIS_BufferedFileStream ;
    const _nOffset    : Integer
  ) ;
  begin
    inherited InitBlockFromData( _pabyBuf,_nBlockSize, _nSizeUsed, _bMakeCopy,
                                 _fpSrc, _nOffset
                                ) ;

    assert( nBlockType = TABMAP_OBJECT_BLOCK ) ;

    GotoByteInBlock($002);
    numDataBytes := ReadInt16 ;

    nCenterX := ReadInt32 ;
    nCenterY := ReadInt32 ;

    nFirstCoordBlock  := ReadInt32 ;
    nLastCoordBlock   := ReadInt32 ;

    nCurObjectOffset  := -1 ;
    nCurObjectId      := -1 ;
    nCurObjectType    := -1 ;

    nSizeUsed := numDataBytes + MAP_OBJECT_HEADER_SIZE ;
  end ;

  function T_BlockObject.InitNewBlock(
    const _fpSrc        : TGIS_BufferedFileStream ;
    const _nBlockSize   : Integer ;
    const _nFileOffset  : Integer
  ) : Integer ;
  begin
    inherited InitNewBlock( _fpSrc, _nBlockSize, _nFileOffset ) ;

    FBoundingRect := IntExtent(  1000000000,  1000000000,
                                -1000000000, -1000000000
                              ) ;

    nCurObjectId      := -1 ;
    nCurObjectOffset  := -1 ;
    nCurObjectType    := -1 ;

    numDataBytes      := 0 ;
    nCenterX          := 0 ;
    nCenterY          := 0 ;
    nFirstCoordBlock  := 0 ;
    nLastCoordBlock   := 0 ;

    GotoByteInBlock( $000 );

    WriteInt16( TABMAP_OBJECT_BLOCK ) ;
    WriteInt16( 0 );

    WriteInt32( 0 ) ;
    WriteInt32( 0 ) ;

    WriteInt32( 0 ) ;
    WriteInt32( 0 ) ;

    Result := 0 ;
  end ;

  function T_BlockObject.PrepareNewObject(
    const _object : T_Object
  ) : Integer ;
  begin
    Result := 0 ;

    if ( _object.nType = TAB_GEONONE ) then exit ;

    UpdateBoundingRect( Point( _object.FBoundingRect.XMin, _object.FBoundingRect.YMin ) ) ;
    UpdateBoundingRect( Point( _object.FBoundingRect.XMax, _object.FBoundingRect.YMax ) ) ;

    Result := GetFirstUnusedByteOffset ;
    GotoByteInFile( Result, False, False ) ;
    nCurObjectOffset := Result - nFileOffset ;

    nCurObjectType := _object.nType ;
    nCurObjectId   := _object.nId ;
  end ;

  procedure T_BlockObject.UpdateBoundingRect(
    const _pt : TPoint
  ) ;
  begin
    if ( _pt.X < FBoundingRect.XMin ) then
      FBoundingRect.XMin := _pt.X ;

    if ( _pt.X > FBoundingRect.XMax ) then
      FBoundingRect.XMax := _pt.X ;

    if ( _pt.Y < FBoundingRect.YMin ) then
      FBoundingRect.YMin := _pt.Y ;

    if ( _pt.Y > FBoundingRect.YMax ) then
      FBoundingRect.YMax := _pt.Y ;

    nCenterX := TruncS( 1.0 * ( FBoundingRect.XMin + FBoundingRect.XMax ) / 2 ) ;
    nCenterY := TruncS( 1.0 * ( FBoundingRect.YMin + FBoundingRect.YMax ) / 2 ) ;
  end ;

  procedure T_BlockObject.WriteIntCoord(
    const _pt       : TPoint ;
    const _compres  : Boolean
  ) ;
  begin
    if not _compres then begin
     WriteInt32( _pt.X ) ;
     WriteInt32( _pt.Y ) ;
    end
    else begin
      WriteInt16( _pt.X - nCenterX ) ;
      WriteInt16( _pt.Y - nCenterY ) ;
    end ;
  end ;

  procedure T_BlockObject.WriteIntMBRCoord(
    const _br           : T_BoundingRect ;
    const _compres  : Boolean
  ) ;
  begin
    WriteIntCoord( Point( Min( _br.XMin, _br.XMax ),
                          Min( _br.YMin, _br.YMax )
                        ),
                   _compres
                 ) ;
    WriteIntCoord( Point( Max( _br.XMin, _br.XMax ),
                          Max( _br.YMin, _br.YMax )
                         ),
                   _compres
                 ) ;
  end ;

//==============================================================================
// T_MAPObjHdr
//==============================================================================

  function T_Object.IsCompressedType : Boolean ;
  begin
    Result := (( nType mod 3) = 1 ) ;
  end ;

  class function T_Object.NewObj(
    const _nNewObjType  : Byte ;
    const _nId          : Integer
  ) : T_Object ;
  begin
    Result := nil ;

    case _nNewObjType of
      TAB_GEONONE           : Result := T_MAPObjNone.Create ;
      TAB_GEOSYMBOL_C,
      TAB_GEOSYMBOL         : Result := T_MAPObjPoint.Create ;
      TAB_GEOFONTSYMBOL_C,
      TAB_GEOFONTSYMBOL     : Result := T_MAPObjFontPoint.Create ;
      TAB_GEOCUSTOMSYMBOL_C,
      TAB_GEOCUSTOMSYMBOL   : Result := T_MAPObjCustomPoint.Create ;
      TAB_GEOLINE_C,
      TAB_GEOLINE           : Result := T_MAPObjLine.Create ;
      TAB_GEOPLINE_C,
      TAB_GEOPLINE,
      TAB_GEOREGION_C,
      TAB_GEOREGION,
      TAB_GEOMULTIPLINE_C,
      TAB_GEOMULTIPLINE,
      TAB_GEOV450_REGION_C,
      TAB_GEOV450_REGION,
      TAB_GEOV450_MULTIPLINE_C,
      TAB_GEOV450_MULTIPLINE: Result := T_MAPObjPLine.Create ;
      TAB_GEOARC_C,
      TAB_GEOARC            : Result := T_MAPObjArc.Create ;
      TAB_GEORECT_C,
      TAB_GEORECT,
      TAB_GEOROUNDRECT_C,
      TAB_GEOROUNDRECT,
      TAB_GEOELLIPSE_C,
      TAB_GEOELLIPSE        : Result := T_MAPObjRectEllipse.Create ;
      TAB_GEOTEXT_C,
      TAB_GEOTEXT           : Result := T_MAPObjText.Create ;
      TAB_GEOMULTIPOINT_C,
      TAB_GEOMULTIPOINT     : Result := TABMAPObjMultiPoint.Create ;
      TAB_GEOCOLLECTION_C,
      TAB_GEOCOLLECTION     : Result := T_MAPObjCollection.Create ;
    end ;

    if assigned( Result ) then begin
      Result.nType  := _nNewObjType;
      Result.nId    := _nId;
      Result.FBoundingRect := IntExtent( 0, 0, 0, 0 ) ;
    end ;
  end ;

  procedure T_Object.SetBoundingRect(
    const _br : T_BoundingRect
  ) ;
  begin
    FBoundingRect.XMin := Min( _br.XMin, _br.XMax ) ;
    FBoundingRect.YMin := Min( _br.YMin, _br.YMax ) ;
    FBoundingRect.XMax := Max( _br.XMin, _br.XMax ) ;
    FBoundingRect.YMax := Max( _br.YMin, _br.YMax ) ;
  end ;

  {
  }
  procedure T_Object.WriteObj(
    const _block : T_BlockObject
  ) ;
  begin

  end ;

  procedure T_Object.WriteObjTypeAndId(
    const _poObjBlock : T_BlockObject
  ) ;
  begin
    _poObjBlock.WriteByte( nType ) ;
    _poObjBlock.WriteInt32( nId ) ;
  end ;

//==============================================================================
// T_MAPObjNone
//==============================================================================

  {
  }
  procedure T_MAPObjNone.WriteObj(
    const _block : T_BlockObject
  ) ;
  begin
    // do nothing
  end ;

//==============================================================================
// T_MAPObjPoint
//==============================================================================

  procedure T_MAPObjPoint.WriteObj(
    const _block : T_BlockObject
  ) ;
  begin
   // Write object type and id
    WriteObjTypeAndId( _block ) ;

    _block.WriteIntCoord( Pos, IsCompressedType() ) ;
    _block.WriteByte( nSymbolId );
  end ;

//==============================================================================
// T_MAPObjFontPoint
//==============================================================================

  procedure T_MAPObjFontPoint.WriteObj(
    const _block : T_BlockObject
  ) ;
  begin
    assert( False, 'Not implemented' ) ;
  end ;

//==============================================================================
// T_MAPObjCustomPoint
//==============================================================================

  procedure T_MAPObjCustomPoint.WriteObj(const _block: T_BlockObject);
  begin
    assert( False, 'Not implemented' ) ;
  end ;

//==============================================================================
// T_MAPObjLine
//==============================================================================

  procedure T_MAPObjLine.WriteObj(
    const _poObjBlock : T_BlockObject
  ) ;
  begin
    WriteObjTypeAndId( _poObjBlock ) ;

    _poObjBlock.WriteIntCoord( A, IsCompressedType ) ;
    _poObjBlock.WriteIntCoord( B, IsCompressedType ) ;

    _poObjBlock.WriteByte( PenId ) ;
  end ;

//==============================================================================
// T_MAPObjPLine
//==============================================================================

  procedure T_MAPObjPLine.WriteObj(
    const _poObjBlock : T_BlockObject
  ) ;
  begin
    WriteObjTypeAndId( _poObjBlock ) ;

    _poObjBlock.WriteInt32( nCoordBlockPtr ) ;

    if (Smooth) then
      _poObjBlock.WriteInt32( nCoordDataSize or $80000000 )
    else
      _poObjBlock.WriteInt32( nCoordDataSize );

    if (nType <> TAB_GEOPLINE_C) and ( nType <> TAB_GEOPLINE ) then
      _poObjBlock.WriteInt16( LineSections ) ;

    if (IsCompressedType) then begin
      _poObjBlock.WriteInt16( SmallInt( LabelPos.X - ComprOrg.X ) ) ;
      _poObjBlock.WriteInt16( SmallInt( LabelPos.Y - ComprOrg.Y ) ) ;

      _poObjBlock.WriteInt32( ComprOrg.X ) ;
      _poObjBlock.WriteInt32( ComprOrg.Y ) ;
    end
    else begin
      _poObjBlock.WriteInt32( LabelPos.X ) ;
      _poObjBlock.WriteInt32( LabelPos.Y ) ;
    end ;

    if (IsCompressedType) then begin
      _poObjBlock.WriteInt16( SmallInt(FBoundingRect.XMin - ComprOrg.X) ) ;
      _poObjBlock.WriteInt16( SmallInt(FBoundingRect.YMin - ComprOrg.Y) ) ;
      _poObjBlock.WriteInt16( SmallInt(FBoundingRect.XMax - ComprOrg.X) ) ;
      _poObjBlock.WriteInt16( SmallInt(FBoundingRect.YMax - ComprOrg.Y) ) ;
    end
    else begin
      _poObjBlock.WriteInt32( FBoundingRect.XMin ) ;
      _poObjBlock.WriteInt32( FBoundingRect.YMin ) ;
      _poObjBlock.WriteInt32( FBoundingRect.XMax ) ;
      _poObjBlock.WriteInt32( FBoundingRect.YMax ) ;
    end ;

    _poObjBlock.WriteByte( PenId ) ;

    if ( nType = TAB_GEOREGION ) or
       ( nType = TAB_GEOREGION_C ) or
       ( nType = TAB_GEOV450_REGION ) or
       ( nType = TAB_GEOV450_REGION_C ) then
        _poObjBlock.WriteByte( BrushId ) ;
  end ;

//==============================================================================
// T_MAPObjArc
//==============================================================================

  procedure T_MAPObjArc.WriteObj(
    const _block : T_BlockObject
  ) ;
  begin
    assert( False, 'Not implemented' ) ;
  end ;

//==============================================================================
// T_MAPObjRectEllipse
//==============================================================================

  procedure T_MAPObjRectEllipse.WriteObj(
    const _block : T_BlockObject
  ) ;
  begin
    assert( False, 'Not implemented' ) ;
  end ;

//==============================================================================
// T_MAPObjText
//==============================================================================

  procedure T_MAPObjText.WriteObj(
    const _poObjBlock : T_BlockObject
  ) ;
  begin
    inherited WriteObjTypeAndId( _poObjBlock ) ;

    _poObjBlock.WriteInt32( nCoordBlockPtr ) ;     // String position
    _poObjBlock.WriteInt16( nCoordDataSize ) ;     // String length
    _poObjBlock.WriteInt16( nTextAlignment ) ;     // just./spacing/arrow

    _poObjBlock.WriteInt16( nAngle ) ;             // Tenths of degree

    _poObjBlock.WriteInt16( nFontStyle ) ;         // Font style/effect

    _poObjBlock.WriteByte( nFGColorR ) ;
    _poObjBlock.WriteByte( nFGColorG ) ;
    _poObjBlock.WriteByte( nFGColorB ) ;

    _poObjBlock.WriteByte( nBGColorR ) ;
    _poObjBlock.WriteByte( nBGColorG ) ;
    _poObjBlock.WriteByte( nBGColorB ) ;

    _poObjBlock.WriteIntCoord( Point( nLineEndX, nLineEndY ), IsCompressedType ) ;

    if ( IsCompressedType ) then
      _poObjBlock.WriteInt16( nHeight )
    else
      _poObjBlock.WriteInt32( nHeight ) ;

    _poObjBlock.WriteByte( nFontId ) ;

    _poObjBlock.WriteIntMBRCoord( FBoundingRect,
                                  IsCompressedType
                                 ) ;
    _poObjBlock.WriteByte( nPenId ) ;
  end ;

  procedure TABMAPObjMultiPoint.WriteObj(
    const _poObjBlock : T_BlockObject
  ) ;
  begin
    WriteObjTypeAndId( _poObjBlock ) ;

    _poObjBlock.WriteInt32( nCoordBlockPtr ) ;

    _poObjBlock.WriteInt32( nNumPoints ) ;

    _poObjBlock.WriteInt32(0) ;
    _poObjBlock.WriteInt32(0) ;
    _poObjBlock.WriteInt32(0) ;
    _poObjBlock.WriteByte(0) ;
    _poObjBlock.WriteByte(0) ;
    _poObjBlock.WriteByte(0) ;

    _poObjBlock.WriteByte(nSymbolId);
    _poObjBlock.WriteByte(0);

    if ( IsCompressedType ) then begin
      _poObjBlock.WriteInt16(SmallInt(nLabelX - ComprOrg.X));
      _poObjBlock.WriteInt16(SmallInt(nLabelY - ComprOrg.Y));

      _poObjBlock.WriteInt32(ComprOrg.X);
      _poObjBlock.WriteInt32(ComprOrg.Y);

      // MBR relative to object origin (and not object block center)
      _poObjBlock.WriteInt16( SmallInt(FBoundingRect.XMin - ComprOrg.X) ) ;
      _poObjBlock.WriteInt16( SmallInt(FBoundingRect.YMin - ComprOrg.Y) ) ;
      _poObjBlock.WriteInt16( SmallInt(FBoundingRect.XMax - ComprOrg.X) ) ;
      _poObjBlock.WriteInt16( SmallInt(FBoundingRect.YMax - ComprOrg.Y) ) ;
    end
    else begin
      _poObjBlock.WriteInt32(nLabelX);
      _poObjBlock.WriteInt32(nLabelY);

      _poObjBlock.WriteInt32( FBoundingRect.XMin ) ;
      _poObjBlock.WriteInt32( FBoundingRect.YMin ) ;
      _poObjBlock.WriteInt32( FBoundingRect.XMax ) ;
      _poObjBlock.WriteInt32( FBoundingRect.YMax ) ;
    end ;
  end ;

//==============================================================================
// T_MAPObjCollection
//==============================================================================

  procedure T_MAPObjCollection.WriteObj(
    const _block : T_BlockObject
  ) ;
  begin
    assert( False, 'Not implemented' ) ;
  end ;

//==============================================================================
// T_BlockIndex
//==============================================================================

  constructor T_BlockIndex.Create(
    const _eAccessMode    : T_TABAccess;
    const _bHardBlockSize : Boolean
  ) ;
  begin
    inherited Create( _eAccessMode, TRUE ) ;

    numEntries := 0 ;

    inMinX := 1000000000 ;
    inMinY := 1000000000 ;
    inMaxX := -1000000000 ;
    inMaxY := -1000000000 ;

    poCurChild        := nil ;
    nCurChildIndex    := -1 ;
    poParentRef       := nil ;
    poBlockManagerRef := nil ;
  end ;

  procedure T_BlockIndex.doDestroy;
  begin
    if assigned( poCurChild ) then begin
      poCurChild.CommitToFile ;
      FreeObject( poCurChild ) ;
    end ;

    inherited;
  end ;

  procedure T_BlockIndex.AddEntry(
    const _br                 : T_BoundingRect ;
    const _nBlockPtr          : Integer ;
    const _bAddInThisNodeOnly : Boolean
  ) ;
  var
    bFound         : Boolean ;
    bestcandidate : Integer ;
    poblock        : T_BlockRaw ;
  begin
    bFound := False ;

    if ( _bAddInThisNodeOnly ) then
      bFound := True ;

    if ( not bFound and ( numEntries > 0 ) ) then begin

      if assigned( poCurChild ) then begin
        poCurChild.CommitToFile;
        FreeObject( poCurChild ) ;
        nCurChildIndex := -1;
      end ;

      bestcandidate := ChooseSubEntryForInsert( _br ) ;

      assert( bestcandidate <> -1 ) ;

      if ( bestcandidate <> -1 ) then begin
        poblock := nil ;

        poblock := TABCreateMAPBlockFromFile( fp, asEntries[bestcandidate].nBlockPtr,
                                              512, True
                                             ) ;
        if assigned( poblock ) and ( poblock.GetBlockClass = TABMAP_INDEX_BLOCK ) then begin

          poCurChild := T_BlockIndex( poblock ) ;
          poblock := nil;
          nCurChildIndex := bestcandidate;
          poCurChild.SetParentRef( self ) ;
          poCurChild.SetMAPBlockManagerRef( poBlockManagerRef ) ;
          bFound := True ;
        end ;

        if assigned( poblock ) then
          FreeObject( poblock ) ;
      end ;
    end ;

    if ( bFound and not _bAddInThisNodeOnly ) then begin
      poCurChild.AddEntry( _br, _nBlockPtr, False ) ;
    end
    else begin
      if GetNumFreeEntries < 1 then begin
        if ( poParentRef = nil ) then begin
          SplitRootNode( _br ) ;
          assert( assigned( poCurChild ) ) ;
          poCurChild.AddEntry( _br, _nBlockPtr, True ) ;
          exit ;
        end
        else
          SplitNode( _br ) ;
      end ;

      InsertEntry( _br, _nBlockPtr ) ;
    end ;

    RecomputeMBR ;
  end ;

  function MITAB_AREA(
    _br : T_BoundingRect
  ) : Double ;
  var
    xmin : Int64 ;
    ymin : Int64 ;
    xmax : Int64 ;
    ymax : Int64 ;
  begin
    xmin := _br.XMin ;
    ymin := _br.YMin ;
    xmax := _br.XMax ;
    ymax := _br.YMax ;

    Result := ( xmax - xmin ) * ( ymax - ymin ) ;
  end ;

  function T_BlockIndex.ChooseSubEntryForInsert(
    const _br : T_BoundingRect
  ) : Integer ;
  var
    i, bestcandidate : Integer ;
    optimalareadiff  : Double ;
    dNewEntryArea     : Double ;
    areadiff         : Double ;
    dAreaBefore       : Double ;
    bIsContained      : Boolean ;
    br                : T_BoundingRect ;

  begin
    bestcandidate    := -1 ;
    optimalareadiff  := 0 ;

    dNewEntryArea := MITAB_AREA( _br ) ;

    for i := 0 to numEntries-1 do begin
      dAreaBefore := MITAB_AREA( asEntries[i].BoundingRect ) ;
      bIsContained := ( _br.XMin >= asEntries[i].BoundingRect.XMin ) and
                      ( _br.YMin >= asEntries[i].BoundingRect.YMin ) and
                      ( _br.XMax <= asEntries[i].BoundingRect.XMax ) and
                      ( _br.YMax <= asEntries[i].BoundingRect.YMax ) ;

      if ( bIsContained ) then
        areadiff := dNewEntryArea - dAreaBefore
      else begin
        br.XMin := Min( asEntries[i].BoundingRect.XMin, _br.XMin ) ;
        br.YMin := Min( asEntries[i].BoundingRect.YMin, _br.YMin ) ;
        br.XMax := Max( asEntries[i].BoundingRect.XMax, _br.XMax ) ;
        br.YMax := Max( asEntries[i].BoundingRect.YMax, _br.YMax ) ;

        areadiff := MITAB_AREA( br ) - dAreaBefore ;
      end ;

      if ( ( bestcandidate = -1 ) or ((areadiff < 0) and (optimalareadiff >= 0))
          or ((((optimalareadiff < 0) and (areadiff < 0)) or
               ((optimalareadiff > 0) and (areadiff > 0))) and
              (Abs(areadiff) < Abs(optimalareadiff))) ) then begin

        bestcandidate   := i;
        optimalareadiff := areadiff ;
      end ;

    end ;

    Result := bestcandidate ;
  end ;

  procedure T_BlockIndex.CommitToFile;
  var
    i : Integer ;
  begin
    if ( pabyBuf = nil ) then exit ;

    if assigned( poCurChild ) then
      poCurChild.CommitToFile ;

    if ( not bModified ) then exit ;

    GotoByteInBlock($000);

    WriteInt16(TABMAP_INDEX_BLOCK);    // Block type code

    WriteInt16(numEntries);

    for i := 0 to numEntries-1 do
      WriteNextEntry( (asEntries[i]));

    inherited;
  end ;

  function T_BlockIndex.ComputeAreaDiff(
    const _node, _entry : T_BoundingRect
  ) : Double ;
  var
    areadiff, dNodeAreaBefore : Double ;
    bIsContained : Boolean ;
    br : T_BoundingRect ;
  begin
    dNodeAreaBefore := MITAB_AREA( _node );

    bIsContained := (_entry.XMin >= _node.XMin ) and
                    (_entry.YMin >= _node.YMin ) and
                    (_entry.XMax <= _node.XMax ) and
                    (_entry.YMax <= _node.YMax ) ;
    if (bIsContained) then
      areadiff := MITAB_AREA( _entry ) - dNodeAreaBefore
    else begin
      br.XMin := Min(_node.XMin, _entry.XMin) ;
      br.YMin := Min(_node.YMin, _entry.YMin) ;
      br.XMax := Max(_node.XMax, _entry.XMax) ;
      br.YMax := Max(_node.YMax, _entry.YMax) ;

      areadiff := MITAB_AREA( br ) - dNodeAreaBefore;
    end ;

    Result := areadiff ;
  end ;

  function T_BlockIndex.GetBlockClass: Integer;
  begin
    Result := TABMAP_INDEX_BLOCK ;
  end ;

  {
  }
  function T_BlockIndex.GetCurMaxDepth: Integer;
  begin
    if assigned( poCurChild ) then
      Result := poCurChild.GetCurMaxDepth + 1
    else
      Result := 1 ;
  end ;

  function T_BlockIndex.GetBoundingRect :  T_BoundingRect ;
  begin
    Result.XMin := inMinX ;
    Result.YMin := inMinY ;
    Result.XMax := inMaxX ;
    Result.YMax := inMaxY ;
  end ;

  function T_BlockIndex.GetNumFreeEntries: Integer;
  begin
    Result := ( TAB_MAX_ENTRIES_INDEX_BLOCK - numEntries ) ;
  end ;

  procedure T_BlockIndex.InitBlockFromData(
    const _pabyBuf    : array of Byte ;
    const _nBlockSize : Integer ;
    const _nSizeUsed  : Integer ;
    const _bMakeCopy  : Boolean ;
    const _fpSrc      : TGIS_BufferedFileStream ;
    const _nOffset    : Integer
  ) ;
  begin
    inherited InitBlockFromData( _pabyBuf,_nBlockSize, _nSizeUsed, _bMakeCopy,
                                 _fpSrc, _nOffset
                                ) ;
    assert( nBlockType = TABMAP_INDEX_BLOCK ) ;

    GotoByteInBlock($002);
    numEntries := ReadInt16 ;

    if ( numEntries > 0 ) then
      ReadAllEntries ;
  end ;

  function T_BlockIndex.InitNewBlock(
    const _fpSrc      : TGIS_BufferedFileStream;
    const _nBlockSize,
          _nFileOffset: Integer
  ): Integer;
  begin
    inherited InitNewBlock( _fpSrc, _nBlockSize, _nFileOffset ) ;

    numEntries := 0 ;

    inMinX := 1000000000 ;
    inMinY := 1000000000 ;
    inMaxX := -1000000000 ;
    inMaxY := -1000000000 ;

    GotoByteInBlock( $000 ) ;

    WriteInt16( TABMAP_INDEX_BLOCK ) ;
    WriteInt16( 0 ) ;

    Result := 0 ;
  end ;

  procedure T_BlockIndex.InsertEntry(
    const _br : T_BoundingRect;
    _nBlockPtr: Integer
  );
  begin
    if ( GetNumFreeEntries < 1 ) then exit ;

    inc( numEntries ) ;
    assert( numEntries <= TAB_MAX_ENTRIES_INDEX_BLOCK ) ;

    {$IFDEF GIS_NORECORDS}
      asEntries[numEntries-1] := new T_MAPIndexEntry ;
    {$ENDIF}
    asEntries[numEntries-1].BoundingRect := _br        ;
    asEntries[numEntries-1].nBlockPtr    := _nBlockPtr ;

    bModified := True ;
  end ;

  procedure T_BlockIndex.PickSeedsForSplit(
    const _pasEntries : array of T_MAPIndexEntry ;
    const _numEntries : Integer ;
    const _nSrcCurChildIndex : Integer ;
    const _new_entry : T_BoundingRect ;
    var _nSeed1, _nSeed2 : Integer);
  var
    nSrcMinX, nSrcMinY, nSrcMaxX, nSrcMaxY : Integer ;
    nLowestMaxX, nHighestMinX, nLowestMaxY, nHighestMinY : Integer ;
    nLowestMaxXId, nHighestMinXId, nLowestMaxYId, nHighestMinYId : Integer ;
    ientry, nTmp : Integer ;
    nSrcWidth, nSrcHeight : Integer ;
    dX, dY : Double ;
    areadiff1, areadiff2 : Double ;
  begin
    nSrcMinX := 0 ;
    nSrcMinY := 0 ;
    nSrcMaxX := 0 ;
    nSrcMaxY := 0 ;

    nLowestMaxX   := -1 ;
    nHighestMinX  := -1 ;
    nLowestMaxY   := -1 ;
    nHighestMinY  := -1 ;

    nLowestMaxXId := -1 ;
    nHighestMinXId:= -1 ;
    nLowestMaxYId := -1 ;
    nHighestMinYId:= -1 ;

    _nSeed1 := -1 ;
    _nSeed2 := -1 ;

    for ientry := 0 to _numEntries-1 do begin

      if (nLowestMaxXId = -1) or (_pasEntries[ientry].BoundingRect.XMax < nLowestMaxX) then begin
        nLowestMaxX   := _pasEntries[ientry].BoundingRect.XMax ;
        nLowestMaxXId := ientry ;
      end ;

      if (nHighestMinXId = -1) or (_pasEntries[ientry].BoundingRect.XMin > nHighestMinX) then begin
        nHighestMinX   := _pasEntries[ientry].BoundingRect.XMin ;
        nHighestMinXId := ientry ;
      end ;

      if (nLowestMaxYId = -1) or (_pasEntries[ientry].BoundingRect.YMax < nLowestMaxY) then begin
        nLowestMaxY   := _pasEntries[ientry].BoundingRect.YMax ;
        nLowestMaxYId := ientry ;
      end ;

      if (nHighestMinYId = -1) or (_pasEntries[ientry].BoundingRect.YMin > nHighestMinY) then begin
        nHighestMinY   := _pasEntries[ientry].BoundingRect.YMin ;
        nHighestMinYId := ientry ;
      end ;

      if (ientry = 0) then begin
        nSrcMinX := _pasEntries[ientry].BoundingRect.XMin ;
        nSrcMinY := _pasEntries[ientry].BoundingRect.YMin ;
        nSrcMaxX := _pasEntries[ientry].BoundingRect.XMax ;
        nSrcMaxY := _pasEntries[ientry].BoundingRect.YMax ;
      end
      else begin
        nSrcMinX := Min( nSrcMinX, _pasEntries[ientry].BoundingRect.XMin ) ;
        nSrcMinY := Min( nSrcMinY ,_pasEntries[ientry].BoundingRect.YMin ) ;
        nSrcMaxX := Max( nSrcMaxX ,_pasEntries[ientry].BoundingRect.XMax ) ;
        nSrcMaxY := Max( nSrcMaxY ,_pasEntries[ientry].BoundingRect.YMax ) ;
      end ;
    end ;

    nSrcWidth  := Abs(nSrcMaxX - nSrcMinX) ;
    nSrcHeight := Abs(nSrcMaxY - nSrcMinY) ;

    dX := (nHighestMinX - nLowestMaxX) / ( 1.0 * nSrcWidth  ) ;
    dY := (nHighestMinY - nLowestMaxY) / ( 1.0 * nSrcHeight );

    if (dX > dY) then begin
      _nSeed1 := nHighestMinXId ;
      _nSeed2 := nLowestMaxXId ;
    end
    else begin
      _nSeed1 := nHighestMinYId ;
      _nSeed2 := nLowestMaxYId ;
    end ;

    if (_nSeed1 = _nSeed2) then begin
      if (_nSeed1 <> _nSrcCurChildIndex) and (_nSrcCurChildIndex <> -1) then
        _nSeed1 := _nSrcCurChildIndex
      else if (_nSeed1 <> 0) then
        _nSeed1 := 0
      else
        _nSeed1 := 1;
    end ;

    areadiff1 := ComputeAreaDiff( _pasEntries[_nSeed1].BoundingRect,
                                   _new_entry
                                  ) ;
    areadiff2 := ComputeAreaDiff( _pasEntries[_nSeed2].BoundingRect,
                                   _new_entry
                                  ) ;

    if  (_nSeed1 <> _nSrcCurChildIndex) and
        ((areadiff1 > areadiff2) or ( _nSeed2 = _nSrcCurChildIndex)) then begin

      nTmp    := _nSeed1 ;
      _nSeed1 := _nSeed2 ;
      _nSeed2 := nTmp ;
    end ;
  end ;

  procedure T_BlockIndex.ReadAllEntries;
  var
    i : Integer ;
  begin
    assert( numEntries <= TAB_MAX_ENTRIES_INDEX_BLOCK ) ;

    if ( numEntries = 0 ) then exit ;

    GotoByteInBlock( $004 ) ;

    for i := 0 to numEntries-1 do
      ReadNextEntry( asEntries[i] ) ;
  end ;

  procedure T_BlockIndex.ReadNextEntry(
    var _psEntry : T_MAPIndexEntry
  ) ;
  begin
    if ( nCurPos < 4 ) then
      GotoByteInBlock( $004 ) ;

    if ( nCurPos > (4+(20*numEntries) ) ) then exit ;

    {$IFDEF GIS_NORECORDS}
      _psEntry := new T_MAPIndexEntry ;
    {$ENDIF}
    _psEntry.BoundingRect.XMin := ReadInt32 ;
    _psEntry.BoundingRect.YMin := ReadInt32 ;
    _psEntry.BoundingRect.XMax := ReadInt32 ;
    _psEntry.BoundingRect.YMax := ReadInt32 ;
    _psEntry.nBlockPtr         := ReadInt32 ;
  end ;

  {
  }
  procedure T_BlockIndex.RecomputeMBR;
  var
    nMinX, nMinY,
    nMaxX, nMaxY,
    i             : Integer ;
  begin
    nMinX :=  1000000000 ;
    nMinY :=  1000000000 ;
    nMaxX := -1000000000 ;
    nMaxY := -1000000000 ;

    for i := 0 to numEntries - 1 do begin
      if asEntries[i].BoundingRect.XMin < nMinX then
          nMinX := asEntries[i].BoundingRect.XMin ;
      if asEntries[i].BoundingRect.XMax > nMaxX then
          nMaxX := asEntries[i].BoundingRect.XMax ;

      if asEntries[i].BoundingRect.YMin < nMinY then
          nMinY := asEntries[i].BoundingRect.YMin ;
      if asEntries[i].BoundingRect.YMax > nMaxY then
          nMaxY := asEntries[i].BoundingRect.YMax ;
    end ;

    if ( inMinX <> nMinX ) or
       ( inMinY <> nMinY ) or
       ( inMaxX <> nMaxX ) or
       ( inMaxY <> nMaxY ) then begin

        inMinX := nMinX ;
        inMinY := nMinY ;
        inMaxX := nMaxX ;
        inMaxY := nMaxY ;

        bModified := True ;

        if assigned( poParentRef ) then
          poParentRef.UpdateCurChildMBR( IntExtent( inMinX, inMinY, inMaxX, inMaxY ),
                                           nFileOffset
                                          ) ;
    end ;
  end ;

  procedure T_BlockIndex.SetCurChildRef(
    const _poChild    : T_BlockIndex;
    const _nChildIndex: Integer
  ) ;
  begin
    poCurChild     := _poChild;
    nCurChildIndex := _nChildIndex;
  end ;

  procedure T_BlockIndex.SetMAPBlockManagerRef(
    const _poBlockMgr : T_BinBlockManager
  ) ;
  begin
   poBlockManagerRef := _poBlockMgr ;
  end ;

  procedure T_BlockIndex.SetParentRef(
    const _poParent : T_BlockIndex
  ) ;
  begin
    poParentRef := _poParent ;
  end ;

  procedure T_BlockIndex.SplitNode(
    const _new_entry : T_BoundingRect
  );
  var
    ponewnode         : T_BlockIndex ;
    nsrcentries, i    : Integer ;
    pasSrcEntries     : array of T_MAPIndexEntry ;
    nSrcCurChildIndex : Integer ;
    nSeed1, nSeed2    : Integer ;
    ientry            : Integer ;
    areadiff1,
    areadiff2         : Double ;
  begin
    assert( assigned( poBlockManagerRef ) ) ;

    ponewnode := T_BlockIndex.Create( eAccess, True ) ;
    ponewnode.InitNewBlock( fp, 512, poBlockManagerRef.AllocNewBlock ) ;
    ponewnode.SetMAPBlockManagerRef( poBlockManagerRef ) ;

    nsrcentries := numEntries ;
    SetLength( pasSrcEntries, numEntries ) ;

    for i := 0 to numEntries - 1 do begin
      pasSrcEntries[i]           := asEntries[i] ;
      pasSrcEntries[i].nBlockPtr := asEntries[i].nBlockPtr ;
    end ;

    nSrcCurChildIndex := nCurChildIndex ;

    PickSeedsForSplit( pasSrcEntries, nsrcentries, nSrcCurChildIndex,
                       _new_entry,
                       nSeed1, nSeed2
                      ) ;

    numEntries := 0;

    InsertEntry( pasSrcEntries[nSeed1].BoundingRect,
                 pasSrcEntries[nSeed1].nBlockPtr
                );
    ponewnode.InsertEntry( pasSrcEntries[nSeed2].BoundingRect,
                           pasSrcEntries[nSeed2].nBlockPtr
                          );

    if (nSeed1 = nSrcCurChildIndex) then
      nCurChildIndex := numEntries-1;

    for ientry := 0 to nsrcentries -1 do begin

        if (ientry = nSeed1) or (ientry = nSeed2) then
            continue;

        if (ientry = nSrcCurChildIndex) then begin

            InsertEntry(pasSrcEntries[ientry].BoundingRect,
                        pasSrcEntries[ientry].nBlockPtr);

            // Update current child index
            nCurChildIndex := numEntries-1;

            continue;

        end
        else if (numEntries >= TAB_MAX_ENTRIES_INDEX_BLOCK-1) then begin

            ponewnode.InsertEntry(pasSrcEntries[ientry].BoundingRect,
                                   pasSrcEntries[ientry].nBlockPtr);
            continue;
        end
        else if (ponewnode.numEntries >= TAB_MAX_ENTRIES_INDEX_BLOCK-1) then begin

            InsertEntry(pasSrcEntries[ientry].BoundingRect,
                        pasSrcEntries[ientry].nBlockPtr);
            continue;
        end ;

        RecomputeMBR;

        areadiff1 := ComputeAreaDiff( IntExtent( inMinX, inMinY, inMaxX, inMaxY ),
                                     pasSrcEntries[ientry].BoundingRect);

        ponewnode.RecomputeMBR ;

        areadiff2 := ComputeAreaDiff(ponewnode.GetBoundingRect,
                                     pasSrcEntries[ientry].BoundingRect);
        if (areadiff1 < areadiff2) then
          InsertEntry( pasSrcEntries[ientry].BoundingRect,
                       pasSrcEntries[ientry].nBlockPtr)
        else
          ponewnode.InsertEntry( pasSrcEntries[ientry].BoundingRect,
                                 pasSrcEntries[ientry].nBlockPtr);
    end ;

    RecomputeMBR;
    ponewnode.RecomputeMBR ;

    poParentRef.AddEntry( ponewnode.GetBoundingRect,
                            ponewnode.nFileOffset, TRUE);
    ponewnode.CommitToFile;
    FreeObject( ponewnode );

    pasSrcEntries := nil ;
  end ;

  procedure T_BlockIndex.SplitRootNode(
    const _new_entry : T_BoundingRect
  ) ;
  var
    ponewnode   : T_BlockIndex ;
    nsrcentries : Integer ;
    ientry      : Integer ;
  begin
    assert( poBlockManagerRef <> nil );
    assert( poParentRef = nil );

    ponewnode := T_BlockIndex.Create(eAccess, True);

    ponewnode.InitNewBlock(fp, 512, poBlockManagerRef.AllocNewBlock ) ;
    ponewnode.SetMAPBlockManagerRef(poBlockManagerRef);

    nsrcentries  := numEntries;
    numEntries := 0;

    for ientry := 0 to nsrcentries-1 do
      ponewnode.InsertEntry(asEntries[ientry].BoundingRect,
                             asEntries[ientry].nBlockPtr);

    if assigned( poCurChild ) then begin

      ponewnode.SetCurChildRef(poCurChild, nCurChildIndex);
      poCurChild.SetParentRef(ponewnode);
      poCurChild := nil ;
      nCurChildIndex := -1 ;
    end ;

    ponewnode.RecomputeMBR ;
    InsertEntry(ponewnode.GetBoundingRect, ponewnode.nFileOffset);

    ponewnode.SetParentRef(self);
    poCurChild := ponewnode;
    nCurChildIndex := numEntries -1;

    poCurChild.SplitNode( _new_entry );
  end ;

  procedure T_BlockIndex.UpdateCurChildMBR(
    const _br        : T_BoundingRect ;
    const _nBlockPtr : Integer
  ) ;
  var
    i : Integer ;
  begin
    assert( poCurChild <> nil ) ;
    assert( asEntries[nCurChildIndex].nBlockPtr = _nBlockPtr ) ;

    if ( asEntries[nCurChildIndex].BoundingRect.XMin = _br.XMin ) and
       ( asEntries[nCurChildIndex].BoundingRect.YMin = _br.YMin ) and
       ( asEntries[nCurChildIndex].BoundingRect.XMax = _br.XMax ) and
       ( asEntries[nCurChildIndex].BoundingRect.YMax = _br.YMax ) then exit ;

    bModified := True ;

    asEntries[nCurChildIndex].BoundingRect := _br ;

    inMinX :=  1000000000 ;
    inMinY :=  1000000000 ;
    inMaxX := -1000000000 ;
    inMaxY := -1000000000 ;

    for i := 0 to numEntries-1 do begin
      if asEntries[i].BoundingRect.XMin < inMinX then
          inMinX := asEntries[i].BoundingRect.XMin ;
      if asEntries[i].BoundingRect.XMax > inMaxX then
          inMaxX := asEntries[i].BoundingRect.XMax ;

      if asEntries[i].BoundingRect.YMin < inMinY then
          inMinY := asEntries[i].BoundingRect.YMin ;
      if asEntries[i].BoundingRect.YMax > inMaxY then
          inMaxY := asEntries[i].BoundingRect.YMax ;
    end ;

    if assigned( poParentRef ) then begin
      poParentRef.UpdateCurChildMBR( IntExtent( inMinX, inMinY, inMaxX, inMaxY ),
                                       nFileOffset
                                      ) ;
    end ;
  end ;

  procedure T_BlockIndex.WriteNextEntry(
    const _psEntry : T_MAPIndexEntry
  ) ;
  begin
    if ( nCurPos < 4 ) then
      GotoByteInBlock( $004 );

    WriteInt32( _psEntry.BoundingRect.XMin ) ;
    WriteInt32( _psEntry.BoundingRect.YMin ) ;
    WriteInt32( _psEntry.BoundingRect.XMax ) ;
    WriteInt32( _psEntry.BoundingRect.YMax ) ;
    WriteInt32( _psEntry.nBlockPtr ) ;
  end ;

//==============================================================================
// T_BlockCoord
//==============================================================================

  procedure T_BlockCoord.CommitToFile;
  begin
    if ( pabyBuf = nil ) then exit ;

    GotoByteInBlock( $000 ) ;

    WriteInt16( TABMAP_COORD_BLOCK ) ;
    WriteInt16( nSizeUsed - MAP_COORD_HEADER_SIZE ) ;
    WriteInt32( nNextCoordBlock ) ;

    inherited ;
  end ;

  constructor T_BlockCoord.Create(
    const _eAccessMode    : T_TABAccess ;
    const _bHardBlockSize : Boolean
  ) ;
  begin
    inherited Create( _eAccessMode, True ) ;

    {$IFDEF GIS_NORECORDS}
      ComprOrg := new TPoint(0, 0) ;
    {$ELSE}
      ComprOrg.X      := 0 ;
      ComprOrg.Y      := 0 ;
    {$ENDIF}
    nNextCoordBlock := 0 ;
    numDataBytes    := 0 ;

    numBlocksInChain  := 1 ;
    poBlockManagerRef := nil ;
    nTotalDataSize    := 0 ;
    nFeatureDataSize  := 0 ;

    nFeatureXMin := 1000000000;
    nMinX := 1000000000;
    nFeatureYMin := 1000000000;
    nMinY := 1000000000;
    nFeatureXMax := -1000000000;
    nMaxX := -1000000000;
    nFeatureYMax := -1000000000;
    nMaxY := -1000000000;
  end ;

  function T_BlockCoord.GetBlockClass: Integer;
  begin
    Result := TABMAP_COORD_BLOCK ;
  end ;

  procedure T_BlockCoord.InitBlockFromData(
    const _pabyBuf    : array of Byte ;
    const _nBlockSize : Integer ;
    const _nSizeUsed  : Integer ;
    const _bMakeCopy  : Boolean ;
    const _fpSrc      : TGIS_BufferedFileStream ;
    const _nOffset    : Integer
  ) ;
  begin
    inherited InitBlockFromData( _pabyBuf, _nBlockSize, _nSizeUsed,
                                 _bMakeCopy, _fpSrc, _nOffset
                                ) ;
    assert( nBlockType = TABMAP_COORD_BLOCK ) ;

    GotoByteInBlock($002);
    numDataBytes    := ReadInt16 ;
    nNextCoordBlock := ReadInt32 ;

    nSizeUsed := numDataBytes + MAP_COORD_HEADER_SIZE ;

    GotoByteInBlock( MAP_COORD_HEADER_SIZE ) ;
  end ;

  function T_BlockCoord.InitNewBlock(
    const _fpSrc: TGIS_BufferedFileStream;
    const _nBlockSize, _nFileOffset: Integer
  ): Integer;
  begin
    inherited InitNewBlock( _fpSrc, _nBlockSize, _nFileOffset ) ;

    nNextCoordBlock := 0 ;
    numDataBytes    := 0 ;

    nMinX := 1000000000 ;
    nMinY := 1000000000 ;
    nMaxX := -1000000000 ;
    nMaxY := -1000000000 ;

    GotoByteInBlock($000);

    WriteInt16( TABMAP_COORD_BLOCK ) ;
    WriteInt16( 0 ) ;
    WriteInt32( 0 ) ;

    Result := 0 ;
  end ;

  procedure T_BlockCoord.ReadBytes(
     const _numBytes   : Integer;
     const _offset     : Integer ;
       var _pabyDstBuf : array of Byte
    ) ;
  var
    numBytesInThisBlock : Integer ;
  begin
    if assigned( pabyBuf ) and
       ( nCurPos >= (numDataBytes+MAP_COORD_HEADER_SIZE )) and
       ( nNextCoordBlock > 0 ) then begin

        GotoByteInFile( nNextCoordBlock, True, False ) ;
        GotoByteInBlock( MAP_COORD_HEADER_SIZE ) ;
        inc( numBlocksInChain ) ;
    end ;

    if assigned( pabyBuf ) and
       ( nCurPos < (numDataBytes+MAP_COORD_HEADER_SIZE) ) and
       ( (nCurPos+_numBytes) > (numDataBytes+MAP_COORD_HEADER_SIZE) ) and
       ( nNextCoordBlock > 0) then begin

        numBytesInThisBlock := (numDataBytes+MAP_COORD_HEADER_SIZE)-nCurPos ;

        inherited ReadBytes( numBytesInThisBlock, 0, _pabyDstBuf ) ;

        ReadBytes( _numBytes-numBytesInThisBlock, numBytesInThisBlock,
                   _pabyDstBuf
                  ) ;
    end ;

    inherited ReadBytes( _numBytes, 0, _pabyDstBuf ) ;
  end ;

  procedure T_BlockCoord.SetComprCoordOrigin(
    const _pt : TPoint
  ) ;
  begin
    ComprOrg := _pt ;
  end ;

  procedure T_BlockCoord.SetMAPBlockManagerRef(
    const _poBlockMgr: T_BinBlockManager
  ) ;
  begin
    poBlockManagerRef := _poBlockMgr ;
  end ;

  procedure T_BlockCoord.SetNextCoordBlock(
    const _nNextCoordBlockAddress: Integer);
  begin
    nNextCoordBlock := _nNextCoordBlockAddress;
  end ;

  procedure T_BlockCoord.StartNewFeature;
  begin
    nFeatureDataSize := 0 ;

    nFeatureXMin := 1000000000 ;
    nFeatureYMin := 1000000000 ;
    nFeatureXMax := -1000000000 ;
    nFeatureYMax := -1000000000 ;
  end ;

  procedure T_BlockCoord.WriteBytes(
    const _nBytesToWrite  : Integer;
    const _pBuf           : array of Byte
  ) ;
  var
    nNewBlockOffset : Integer ;
  begin
    if assigned( poBlockManagerRef ) and ( (nBlockSize - nCurPos) < _nBytesToWrite) then begin
      if (_nBytesToWrite <= (nBlockSize-MAP_COORD_HEADER_SIZE)) then begin

        if ( nNextCoordBlock <> 0 ) then begin
          CommitToFile ;
          ReadFromFile( fp, nNextCoordBlock, nBlockSize );
        end
        else begin
          nNewBlockOffset := poBlockManagerRef.AllocNewBlock ;
          SetNextCoordBlock( nNewBlockOffset );

          CommitToFile;
          InitNewBlock( fp, nBlockSize, nNewBlockOffset ) ;

          inc( numBlocksInChain ) ;
        end
      end
      else begin
        assert( False, 'Not implemented' ) ;
      end ;
    end ;

    if ( nCurPos >= MAP_COORD_HEADER_SIZE) then begin
      inc( nTotalDataSize, _nBytesToWrite ) ;
      inc( nFeatureDataSize, _nBytesToWrite ) ;
    end ;

    inherited WriteBytes( _nBytesToWrite, _pBuf ) ;
  end ;

  procedure T_BlockCoord.WriteCoordSecHdrs(
    const _bV450Hdr   : Boolean;
    const _numSections: Integer;
    const _pasHdrs    : T_SectionArray;
    const _compres: Boolean
  ) ;
  var
    i : Integer ;
  begin
    for i := 0 to _numSections-1 do begin

      if _bV450Hdr then
        WriteInt32( _pasHdrs[i].FVertices )
      else
        WriteInt16( _pasHdrs[i].FVertices ) ;

      WriteInt16( _pasHdrs[i].FHoles ) ;
      WriteIntCoord( Point( _pasHdrs[i].FBoundingRect.XMin, _pasHdrs[i].FBoundingRect.YMin ), _compres ) ;
      WriteIntCoord( Point( _pasHdrs[i].FBoundingRect.XMax, _pasHdrs[i].FBoundingRect.YMax ), _compres ) ;
      WriteInt32( _pasHdrs[i].FDataOffset ) ;
    end
  end ;

  procedure T_BlockCoord.WriteIntCoord(
    const _pt           : TPoint ;
    const _compres  : Boolean
  ) ;
  begin
    if not _compres then begin
      WriteInt32( _pt.X ) ;
      WriteInt32( _pt.Y ) ;
    end
    else begin
      WriteInt16( _pt.X - ComprOrg.X ) ;
      WriteInt16( _pt.Y - ComprOrg.Y ) ;
    end ;

    if ( _pt.X < nMinX ) then nMinX := _pt.X;
    if ( _pt.X > nMaxX ) then nMaxX := _pt.X;

    if ( _pt.Y < nMinY ) then nMinY := _pt.Y;
    if ( _pt.Y > nMaxY ) then nMaxY := _pt.Y;

    if ( _pt.X < nFeatureXMin ) then nFeatureXMin := _pt.X;
    if ( _pt.X > nFeatureXMax ) then nFeatureXMax := _pt.X;

    if ( _pt.Y < nFeatureYMin ) then nFeatureYMin := _pt.Y;
    if ( _pt.Y > nFeatureYMax ) then nFeatureYMax := _pt.Y;
  end ;

//==============================================================================
// T_BlockTool
//==============================================================================

  procedure T_BlockTool.CheckAvailableSpace(
    const _nToolType : Integer
  ) ;
  var
    nBytesToWrite   : Integer ;
    nNewBlockOffset : Integer ;
  begin
    nBytesToWrite := 0 ;

    case _nToolType of
      TABMAP_TOOL_PEN     : nBytesToWrite := 11 ;
      TABMAP_TOOL_BRUSH   : nBytesToWrite := 13 ;
      TABMAP_TOOL_SYMBOL  : nBytesToWrite := 13 ;
    end ;

    if ( GetNumUnusedBytes < nBytesToWrite ) then begin

      nNewBlockOffset  := poBlockManagerRef.AllocNewBlock ;
      nNextToolBlock := nNewBlockOffset ;

      CommitToFile ;
      InitNewBlock( fp, 512, nNewBlockOffset ) ;
      inc( numBlocksInChain ) ;
    end ;
  end ;

  procedure T_BlockTool.CommitToFile;
  begin
    if ( pabyBuf = nil ) then exit ;

    GotoByteInBlock( $000 ) ;

    WriteInt16( TABMAP_TOOL_BLOCK ) ;
    WriteInt16( nSizeUsed - MAP_TOOL_HEADER_SIZE ) ;
    WriteInt32( nNextToolBlock ) ;

    inherited ;
  end ;

  constructor T_BlockTool.Create(
    const _eAccessMode    : T_TABAccess ;
    const _bHardBlockSize : Boolean
  ) ;
  begin
    inherited Create( _eAccessMode, TRUE ) ;

    nNextToolBlock   := 0 ;
    numDataBytes     := 0 ;
    numBlocksInChain := 1 ;

    poBlockManagerRef := nil ;
  end ;

  function T_BlockTool.GetBlockClass: Integer;
  begin
    Result := TABMAP_TOOL_BLOCK ;
  end ;

  procedure T_BlockTool.InitBlockFromData(
    const _pabyBuf    : array of Byte ;
    const _nBlockSize : Integer ;
    const _nSizeUsed  : Integer ;
    const _bMakeCopy  : Boolean ;
    const _fpSrc      : TGIS_BufferedFileStream ;
    const _nOffset    : Integer
  ) ;
  begin
    inherited InitBlockFromData( _pabyBuf, _nBlockSize, _nSizeUsed,
                                 _bMakeCopy, _fpSrc, _nOffset
                                ) ;
    assert( nBlockType = TABMAP_TOOL_BLOCK ) ;

    GotoByteInBlock($002);
    numDataBytes   := ReadInt16 ;
    nNextToolBlock := ReadInt32 ;

    GotoByteInBlock( MAP_TOOL_HEADER_SIZE ) ;
  end ;

  function T_BlockTool.InitNewBlock(
    const _fpSrc: TGIS_BufferedFileStream;
    const _nBlockSize, _nFileOffset: Integer
  ): Integer;
  begin
    inherited InitNewBlock( _fpSrc, _nBlockSize, _nFileOffset ) ;

    nNextToolBlock := 0 ;
    numDataBytes   := 0 ;

    GotoByteInBlock( $000 ) ;

    WriteInt16( TABMAP_TOOL_BLOCK ) ;
    WriteInt16( 0 ) ;
    WriteInt32( 0 ) ;

    Result := 0 ;
  end ;


  procedure T_BlockTool.ReadBytes(
     const _numBytes   : Integer;
     const _offset     : Integer ;
       var _pabyDstBuf : array of Byte
    ) ;
  begin
    assert( False, '' ) ;

  end ;


  procedure T_BlockTool.SetMAPBlockManagerRef(
    const _poBlockMgr: T_BinBlockManager
  );
  begin
    poBlockManagerRef := _poBlockMgr;
  end ;

  {
  }
  procedure T_BlockTool.SetNextToolBlock(
    const _nNextToolBlockAddress : Integer
  );
  begin
    nNextToolBlock := _nNextToolBlockAddress ;
  end ;

  procedure T_BlockTool.WriteBytes(
    const _nBytesToWrite  : Integer;
    const _pBuf           : array of Byte
  );
  var
    nNewBlockOffset : Integer ;
  begin
    if (eAccess = T_TABAccess.Write) and assigned( poBlockManagerRef ) and
       ( (nBlockSize - nCurPos) < _nBytesToWrite) then begin

      nNewBlockOffset := poBlockManagerRef.AllocNewBlock ;
      SetNextToolBlock( nNewBlockOffset ) ;

      CommitToFile;
      InitNewBlock( fp, 512, nNewBlockOffset ) ;

      inc( numBlocksInChain ) ;
    end ;

    inherited WriteBytes( _nBytesToWrite, _pBuf ) ;
  end ;

//==============================================================================
// T_ToolDefTable
//==============================================================================

  function T_ToolDefTable.AddBrushDefRef(
    const _poNewBrushDef : TGIS_TABBrushDef
  ) : Integer;
  var
    i,
    nNewBrushIndex : Integer ;
    poDef          : TGIS_TABBrushDef ;
  begin
    if ( _poNewBrushDef.nFillPattern < 1 ) then begin
      Result := 0 ;
      exit ;
    end ;

    nNewBrushIndex := 0 ;
    for i := 0 to numBrushes-1 do begin

      poDef := papsBrush[ i ] ;
      if ( poDef.nFillPattern     = _poNewBrushDef.nFillPattern     ) and
         ( poDef.bTransparentFill = _poNewBrushDef.bTransparentFill ) and
         ( poDef.rgbFGColor.ARGB  = _poNewBrushDef.rgbFGColor.ARGB  ) and
         ( poDef.rgbBGColor.ARGB  = _poNewBrushDef.rgbBGColor.ARGB  ) then begin

          nNewBrushIndex := i+1 ;
          inc( poDef.nRefCount ) ;
      end ;
    end ;

    if ( nNewBrushIndex = 0 ) then begin

      if ( numBrushes >= numAllocatedBrushes ) then begin
        inc( numAllocatedBrushes, 20 ) ;
        SetLength( papsBrush, numAllocatedBrushes ) ;
      end ;

      {$IFDEF GIS_NORECORDS}
        papsBrush[numBrushes] := new TGIS_TABBrushDef ;
      {$ENDIF}
      papsBrush[numBrushes].nFillPattern      := _poNewBrushDef.nFillPattern ;
      papsBrush[numBrushes].bTransparentFill  := _poNewBrushDef.bTransparentFill ;
      papsBrush[numBrushes].rgbFGColor        := _poNewBrushDef.rgbFGColor ;
      papsBrush[numBrushes].rgbBGColor        := _poNewBrushDef.rgbBGColor ;
      papsBrush[numBrushes].nRefCount         := 1 ;
      inc( numBrushes ) ;
      nNewBrushIndex := numBrushes;
    end ;

    Result := nNewBrushIndex ;
  end ;

  function T_ToolDefTable.AddFontDefRef(
    const _poNewFontDef: TGIS_TABFontDef
  ): Integer;
  var
    i, nNewFontIndex : Integer ;
    poDef : TGIS_TABFontDef ;
  begin
    nNewFontIndex := 0 ;

    for i := 0 to numFonts-1 do begin

      poDef := papsFont[ i ] ;
      if ( CompareText( poDef.szFontName, _poNewFontDef.szFontName ) = 0 )then begin

        nNewFontIndex := i+1;
        inc( poDef.nRefCount ) ;
      end ;
    end ;

    if ( nNewFontIndex = 0 ) then begin

      if ( numFonts >= numAllocatedFonts ) then begin

        inc( numAllocatedFonts, 20 ) ;
        SetLength( papsFont, numAllocatedFonts ) ;
      end ;

      {$IFDEF GIS_NORECORDS}
        papsFont[numFonts] := new TGIS_TABFontDef ;
      {$ENDIF}
      papsFont[numFonts].szFontName := _poNewFontDef.szFontName ;
      papsFont[numFonts].nRefCount := 1;
      inc( numFonts ) ;
      nNewFontIndex := numFonts ;
    end ;

    Result := nNewFontIndex ;
  end ;

  function T_ToolDefTable.AddPenDefRef(
    const _poNewPenDef: TGIS_TABPenDef
  ) : Integer ;
  var
    i,
    nNewPenIndex  : Integer ;
    poDef         : TGIS_TABPenDef ;
  begin
    if ( _poNewPenDef.nLinePattern < 1 ) then begin
      Result := 0 ;
      exit ;
    end ;

    nNewPenIndex := 0 ;
    for i := 0 to numPen-1 do begin

      poDef := papsPen[ i ] ;
      if ( poDef.nPixelWidth   = _poNewPenDef.nPixelWidth   ) and
         ( poDef.nLinePattern  = _poNewPenDef.nLinePattern  ) and
         ( poDef.nPointWidth   = _poNewPenDef.nPointWidth   ) and
         ( poDef.rgbColor.ARGB = _poNewPenDef.rgbColor.ARGB ) then begin

          nNewPenIndex := i + 1 ;
          inc( poDef.nRefCount ) ;
          break ;
      end ;
    end ;

    if ( nNewPenIndex = 0 ) then begin

      if ( numPen >= numAllocatedPen ) then begin

        inc( numAllocatedPen, 20 ) ;
        SetLength( papsPen, numAllocatedPen ) ;
      end ;

      {$IFDEF GIS_NORECORDS}
        papsPen[numPen] := new TGIS_TABPenDef ;
      {$ENDIF}
      papsPen[numPen].nPixelWidth   := _poNewPenDef.nPixelWidth ;
      papsPen[numPen].nLinePattern  := _poNewPenDef.nLinePattern ;
      papsPen[numPen].nPointWidth   := _poNewPenDef.nPointWidth ;
      papsPen[numPen].rgbColor      := _poNewPenDef.rgbColor ;
      papsPen[numPen].nRefCount     := 1 ;
      inc( numPen ) ;
      nNewPenIndex := numPen ;
    end ;

    Result := nNewPenIndex ;
  end ;

  function T_ToolDefTable.AddSymbolDefRef(
    const _poNewSymbolDef: TGIS_TABSymbolDef
  ): Integer;
  var
    i,
    nNewSymbolIndex : Integer ;
    poDef           : TGIS_TABSymbolDef ;
  begin
    nNewSymbolIndex := 0 ;
    for i := 0 to numSymbols-1 do begin

      poDef := papsSymbol[i];
      if ( poDef.nSymbolNo     = _poNewSymbolDef.nSymbolNo     ) and
         ( poDef.nPointSize    = _poNewSymbolDef.nPointSize    ) and
         ( poDef.nUnknownValue = _poNewSymbolDef.nUnknownValue ) and
         ( poDef.rgbColor.ARGB = _poNewSymbolDef.rgbColor.ARGB ) then begin

          nNewSymbolIndex := i+1;
          inc( poDef.nRefCount ) ;
      end ;
    end ;

    if ( nNewSymbolIndex = 0 ) then begin
      if (numSymbols >= numAllocatedSymbols) then begin

        inc( numAllocatedSymbols, 20 ) ;
        SetLength( papsSymbol, numAllocatedSymbols ) ;
      end ;

      {$IFDEF GIS_NORECORDS}
        papsSymbol[numSymbols] := new TGIS_TABSymbolDef ;
      {$ENDIF}
      papsSymbol[numSymbols].nSymbolNo      := _poNewSymbolDef.nSymbolNo ;
      papsSymbol[numSymbols].nPointSize     := _poNewSymbolDef.nPointSize ;
      papsSymbol[numSymbols].nUnknownValue  := _poNewSymbolDef.nUnknownValue ;
      papsSymbol[numSymbols].rgbColor       := _poNewSymbolDef.rgbColor ;
      papsSymbol[numSymbols].nRefCount      := 1 ;
      inc( numSymbols ) ;
      nNewSymbolIndex := numSymbols ;
    end ;

    Result := nNewSymbolIndex;
  end ;

  constructor T_ToolDefTable.Create;
  begin
    inherited ;

    papsPen             := nil ;
    papsBrush           := nil ;
    numPen              := 0 ;
    numBrushes          := 0 ;
    numFonts            := 0 ;
    numSymbols          := 0 ;
    numAllocatedPen     := 0 ;
    numAllocatedBrushes := 0 ;
    numAllocatedFonts   := 0 ;
    numAllocatedSymbols := 0 ;
  end ;

  procedure T_ToolDefTable.WriteAllToolDefs(
    const _poBlock: T_BlockTool
  );
  var
    i : Integer ;
    byPixelWidth,
    byPointWidth : Byte ;
  begin
    for i := 0 to numPen-1 do begin

      byPixelWidth := 1 ;
      byPointWidth := 0 ;
      if ( papsPen[i].nPointWidth > 0 ) then begin

        byPointWidth := ( papsPen[i].nPointWidth and $ff ) ;
        if ( papsPen[i].nPointWidth >= 255 ) then
          byPixelWidth := 8 + TruncS( 1.0 * papsPen[i].nPointWidth / $100 ) ;
      end
      else
        byPixelWidth := Min( Max(papsPen[i].nPixelWidth, 1), 7 ) ;

      _poBlock.CheckAvailableSpace( TABMAP_TOOL_PEN ) ;
      _poBlock.WriteByte( TABMAP_TOOL_PEN ) ;
      _poBlock.WriteInt32( papsPen[i].nRefCount ) ;

      _poBlock.WriteByte( byPixelWidth ) ;
      _poBlock.WriteByte( papsPen[i].nLinePattern ) ;
      _poBlock.WriteByte( byPointWidth ) ;
      _poBlock.WriteByte( papsPen[i].rgbColor.R ) ;
      _poBlock.WriteByte( papsPen[i].rgbColor.G ) ;
      _poBlock.WriteByte( papsPen[i].rgbColor.B ) ;
    end ;

    for i := 0 to numBrushes-1 do begin

      _poBlock.CheckAvailableSpace( TABMAP_TOOL_BRUSH ) ;

      _poBlock.WriteByte( TABMAP_TOOL_BRUSH ) ;
      _poBlock.WriteInt32( papsBrush[i].nRefCount );

      _poBlock.WriteByte( papsBrush[i].nFillPattern ) ;
      _poBlock.WriteByte( papsBrush[i].bTransparentFill ) ;
      _poBlock.WriteByte( papsBrush[i].rgbFGColor.R ) ;
      _poBlock.WriteByte( papsBrush[i].rgbFGColor.G ) ;
      _poBlock.WriteByte( papsBrush[i].rgbFGColor.B ) ;
      _poBlock.WriteByte( papsBrush[i].rgbBGColor.R ) ;
      _poBlock.WriteByte( papsBrush[i].rgbBGColor.G ) ;
      _poBlock.WriteByte( papsBrush[i].rgbBGColor.B ) ;
    end ;

    for i := 0 to numSymbols-1 do begin

      _poBlock.CheckAvailableSpace( TABMAP_TOOL_SYMBOL ) ;

      _poBlock.WriteByte( TABMAP_TOOL_SYMBOL ) ;
      _poBlock.WriteInt32( papsSymbol[i].nRefCount ) ;

      _poBlock.WriteInt16( papsSymbol[i].nSymbolNo ) ;
      _poBlock.WriteInt16( papsSymbol[i].nPointSize ) ;
      _poBlock.WriteByte( papsSymbol[i].nUnknownValue ) ;
      _poBlock.WriteByte( papsSymbol[i].rgbColor.R ) ;
      _poBlock.WriteByte( papsSymbol[i].rgbColor.G ) ;
      _poBlock.WriteByte( papsSymbol[i].rgbColor.B ) ;
    end ;

    _poBlock.CommitToFile;
  end ;

//==============================================================================
// T_TABText
//==============================================================================

  {
  }
  constructor T_FeatureText.Create;
  begin
    inherited;

    FText          := nil ;
    FAngle         := 0.0 ;
    FHeight        := 0.0 ;
    FLineEnd       := GisPoint( 0.0, 0.0 ) ;
    FLineEndSet    := False ;

    FForeground    := TGIS_Color.Black ;
    FBackground    := TGIS_Color.White ;

    FTextAlignment := 0 ;
    FFontStyle     := 0 ;
    FWidth         := 0 ;

    FFeatureClass  := TABFC_Text
  end ;

  function T_FeatureText.GetTextLineEndPoint : TGIS_Point ;
  var
    ext : TGIS_Extent ;
  begin
    if ( not FLineEndSet ) then begin

      UpdateBoundingRect( nil ) ;
      ext := FExtent ;
      FLineEnd.X  := ( ext.XMin + ext.XMax ) /2.0 ;
      FLineEnd.Y  := ( ext.YMin + ext.YMax ) /2.0 ;
      FLineEndSet := True ;
    end ;

    Result := FLineEnd ;
  end ;

  procedure T_FeatureText.SetText(
    const _angle               : Double ;
    const _height              : Double ;
    const _width               : Double ;
    const _fgcolor             : TGIS_Color ;
    const _bgcolor             : TGIS_Color ;
    const _justification       : Integer ;
    const _spacing             : Integer ;
    const _linetype            : Integer ;
    const _feature             : TGIS_Shape
   ) ;
  begin
    FAngle  := _angle ;
    FHeight := _height ;
    FWidth  := _width ;
    FForeground := _fgcolor ;
    FBackground := _bgcolor ;

    FTextAlignment := FTextAlignment and $0600 ;

    if (_justification = 1) then
      FTextAlignment := FTextAlignment or $0200
    else if (_justification = 2) then
      FTextAlignment := FTextAlignment or $0400 ;

    FTextAlignment := FTextAlignment and $1800;
    FTextAlignment := FTextAlignment and $6000;

    FText := ConvertAnsiString( _feature.GetLabel ) ;

    UpdateBoundingRect( nil ) ;
  end ;

  procedure T_FeatureText.UpdateBoundingRect(
    const _mapfile : T_MAPFile
  );
  var
    dSin, dCos, dX0, dY0, dX1, dY1 : Double ;
    dX, dY : array [0..3] of Double ;
    ptg : TGIS_Point ;
    i : Integer ;
    pt_tmp : TPoint ;
  begin
    ptg := FShape.GetPoint( 0, 0 ) ;
    dX0 := ptg.X ;
    dY0 := ptg.Y ;

    SinCos( FAngle*Pi/180.0, dSin, dCos ) ;

    if ( FWidth = 0.0 ) and ( length( FText ) > 0) then
      FWidth := 0.6 * FHeight * length( FText ) ;

    dX[0] := dX0;
    dY[0] := dY0;
    dX[1] := dX0 + FWidth;
    dY[1] := dY0;
    dX[2] := dX0 + FWidth;
    dY[2] := dY0 + FHeight;
    dX[3] := dX0;
    dY[3] := dY0 + FHeight;

    SetExtent( GisExtent( dX0, dY0, dX0, dY0 ) ) ;

    for i := 0 to 3 do begin

      dX1 := dX0 + (dX[i]-dX0)*dCos - (dY[i]-dY0)*dSin;
      dY1 := dY0 + (dX[i]-dX0)*dSin + (dY[i]-dY0)*dCos;

      if ( dX1 < FExtent.XMin ) then FExtent.XMin := dX1 ;
      if ( dX1 > FExtent.XMax ) then FExtent.XMax := dX1 ;
      if ( dY1 < FExtent.YMin ) then FExtent.YMin := dY1 ;
      if ( dY1 > FExtent.YMax ) then FExtent.YMax := dY1 ;
    end ;

    if assigned( _mapfile ) then begin
      pt_tmp := _mapfile.Coordsys2Int( GisPoint( FExtent.XMin, FExtent.YMin ), False ) ;
      FBoundingRect.XMin := pt_tmp.X ;
      FBoundingRect.YMin := pt_tmp.Y ;

      pt_tmp := _mapfile.Coordsys2Int( GisPoint( FExtent.XMax, FExtent.YMax ), False ) ;
      FBoundingRect.XMax := pt_tmp.X ;
      FBoundingRect.YMax := pt_tmp.Y ;
    end ;
  end ;

  function T_FeatureText.Validate(
    const _mapfile : T_MAPFile
  ) : Integer;
  begin
    if ( FShape.ShapeType = TGIS_ShapeType.Point ) then
      FMapInfoType := TAB_GEOTEXT
    else
      FMapInfoType := TAB_GEONONE ;

    UpdateBoundingRect( _mapfile ) ;

    Result := FMapInfoType ;
  end ;

  procedure T_FeatureText.Write(
    const _mapfile    : T_MAPFile ;
    const _object     : T_Object
  );
  var
    nCoordBlockPtr : Integer ;
    br : T_BoundingRect ;
    poCoordBlock : T_BlockCoord ;
    nStringLen : Integer ;
    poTextHdr : T_MAPObjText ;
    ext : TGIS_Extent ;
    pt_tmp : TPoint ;
  begin
    assert( FMapInfoType = _object.nType ) ;

    poCoordBlock := _mapfile.poCurCoordBlock ;

    poCoordBlock.StartNewFeature ;
    nCoordBlockPtr := poCoordBlock.GetCurAddress ;

    nStringLen := length( FText ) ;

    if ( nStringLen > 0 ) then begin
      poCoordBlock.WriteBytes( nStringLen, FText ) ;
    end
    else
      nCoordBlockPtr := 0 ;

    poTextHdr := T_MAPObjText( _object ) ;

    poTextHdr.nCoordBlockPtr := nCoordBlockPtr;
    poTextHdr.nCoordDataSize := nStringLen;
    poTextHdr.nTextAlignment := FTextAlignment;

    poTextHdr.nAngle := RoundS( FAngle*10.0 ) ;

    poTextHdr.nFontStyle := FFontStyle;

    poTextHdr.nFGColorR := FForeground.R ;
    poTextHdr.nFGColorG := FForeground.G ;
    poTextHdr.nFGColorB := FForeground.B ;

    poTextHdr.nBGColorR := FBackground.R ;
    poTextHdr.nBGColorG := FBackground.G ;
    poTextHdr.nBGColorB := FBackground.B ;

    UpdateBoundingRect( nil ) ;
    ext := FExtent ;

    pt_tmp := _mapfile.Coordsys2Int( GisPoint( ext.XMin, ext.YMin ), False );
    br.XMin := pt_tmp.X ;
    br.YMin := pt_tmp.Y ;

    pt_tmp := _mapfile.Coordsys2Int( GisPoint( ext.XMax, ext.YMax ), False );
    br.XMax := pt_tmp.X ;
    br.YMax := pt_tmp.Y ;

    GetTextLineEndPoint ;
    pt_tmp := _mapfile.Coordsys2Int( FLineEnd, False ) ;
    poTextHdr.nLineEndX := pt_tmp.X ;
    poTextHdr.nLineEndY := pt_tmp.Y ;

    poTextHdr.nHeight := _mapfile.Coordsys2IntDist( GisPoint( 0.0, FHeight ) ).Y;

    FFontDefIndex     := _mapfile.WriteFontDef( FFontDef ) ;
    poTextHdr.nFontId := FFontDefIndex;      // Font name index

    poTextHdr.SetBoundingRect( br );

    FPenDefIndex     := _mapfile.WritePenDef( FPenDef ) ;
    poTextHdr.nPenId := FPenDefIndex;
  end ;

//==============================================================================
// TGIS_FileTAB_MAPInternal
//==============================================================================

  constructor TGIS_FileTAB_MAPInternal.Create(
    const _path  : String  ;
    const _write : Boolean
  ) ;
  begin
    inherited Create( _path, getStreamMode( _write ) ) ;
  end ;

  procedure TGIS_FileTAB_MAPInternal.doDestroy ;
  begin
    inherited ;
  end ;

  procedure TGIS_FileTAB_MAPInternal.ReadHeader ;
  var
    i   : Integer ;
    tmp : Double  ;
  begin
    Position := $104 ;
    {$IFDEF OXYGENE}
    ReadWord( VersionNumber        , sizeOf( Word     ) ) ;
    ReadWord( BlockSize            , sizeOf( Word     ) ) ;

    ReadDouble( Coord2DistUnits      , sizeOf( Double   ) ) ;
    ReadInteger( XMin                 , sizeOf( Int32  ) ) ;
    ReadInteger( YMin                 , sizeOf( Int32  ) ) ;
    ReadInteger( XMax                 , sizeOf( Int32  ) ) ;
    ReadInteger( YMax                 , sizeOf( Int32  ) ) ;
    {$ELSE}
    Read( VersionNumber        , sizeOf( Word     ) ) ;
    Read( BlockSize            , sizeOf( Word     ) ) ;

    Read( Coord2DistUnits      , sizeOf( Double   ) ) ;
    Read( XMin                 , sizeOf( Int32  ) ) ;
    Read( YMin                 , sizeOf( Int32  ) ) ;
    Read( XMax                 , sizeOf( Int32  ) ) ;
    Read( YMax                 , sizeOf( Int32  ) ) ;
    {$ENDIF}

    Position := $130 ;

    {$IFDEF OXYGENE}
    ReadCardinal( FirstIndexBlock      , sizeOf( Cardinal ) ) ;
    ReadCardinal( FirstGarbageBlock    , sizeOf( Cardinal ) ) ;
    ReadCardinal( FirstToolBlock       , sizeOf( Cardinal ) ) ;

    ReadCardinal( NumPointObjects      , sizeOf( Cardinal ) ) ;
    ReadCardinal( NumLineObjects       , sizeOf( Cardinal ) ) ;
    ReadCardinal( NumRegionObjects     , sizeOf( Cardinal ) ) ;
    ReadCardinal( NumTextObjects       , sizeOf( Cardinal ) ) ;
    ReadCardinal( MaxCoordBufSize      , sizeOf( Cardinal ) ) ;
    {$ELSE}
    Read( FirstIndexBlock      , sizeOf( Cardinal ) ) ;
    Read( FirstGarbageBlock    , sizeOf( Cardinal ) ) ;
    Read( FirstToolBlock       , sizeOf( Cardinal ) ) ;

    Read( NumPointObjects      , sizeOf( Cardinal ) ) ;
    Read( NumLineObjects       , sizeOf( Cardinal ) ) ;
    Read( NumRegionObjects     , sizeOf( Cardinal ) ) ;
    Read( NumTextObjects       , sizeOf( Cardinal ) ) ;
    Read( MaxCoordBufSize      , sizeOf( Cardinal ) ) ;
    {$ENDIF}

    Position := $15e ;

    {$IFDEF OXYGENE}
    ReadByte( DistUnitsCode        , sizeOf( Byte     ) ) ;
    ReadByte( MaxSpIndexDepth      , sizeOf( Byte     ) ) ;
    ReadByte( CoordPrecision       , sizeOf( Byte     ) ) ;
    ReadByte( CoordOriginQuadrant  , sizeOf( Byte     ) ) ;
    ReadByte( ReflectXAxisCoord    , sizeOf( Byte     ) ) ;
    ReadByte( MaxObjLenArrayId     , sizeOf( Byte     ) ) ;
    ReadByte( NumPenDefs           , sizeOf( Byte     ) ) ;
    ReadByte( NumBrushDefs         , sizeOf( Byte     ) ) ;
    ReadByte( NumSymbolDefs        , sizeOf( Byte     ) ) ;
    ReadByte( NmFontDefs           , sizeOf( Byte     ) ) ;
    ReadWord( NumMapToolBlocks     , sizeOf( Word     ) ) ;
    {$ELSE}
    Read( DistUnitsCode        , sizeOf( Byte     ) ) ;
    Read( MaxSpIndexDepth      , sizeOf( Byte     ) ) ;
    Read( CoordPrecision       , sizeOf( Byte     ) ) ;
    Read( CoordOriginQuadrant  , sizeOf( Byte     ) ) ;
    Read( ReflectXAxisCoord    , sizeOf( Byte     ) ) ;
    Read( MaxObjLenArrayId     , sizeOf( Byte     ) ) ;
    Read( NumPenDefs           , sizeOf( Byte     ) ) ;
    Read( NumBrushDefs         , sizeOf( Byte     ) ) ;
    Read( NumSymbolDefs        , sizeOf( Byte     ) ) ;
    Read( NmFontDefs           , sizeOf( Byte     ) ) ;
    Read( NumMapToolBlocks     , sizeOf( Word     ) ) ;
    {$ENDIF}

    //Position := $16d ;
    if VersionNumber >= 500 then
      {$IFDEF OXYGENE}
      ReadWord( DatumId, sizeOf( Word ) )
      {$ELSE}
      Read( DatumId, sizeOf( Word ) )
    {$ENDIF}
    else
      DatumId := 0 ;

    Position := $16d ;

    {$IFDEF OXYGENE}
    ReadByte( ProjId               , sizeOf( Byte     ) ) ;
    ReadByte( EllipsoidId          , sizeOf( Byte     ) ) ;
    ReadByte( UnitsId              , sizeOf( Byte     ) ) ;
    ReadDouble( XScale               , sizeOf( Double   ) ) ;
    ReadDouble( YScale               , sizeOf( Double   ) ) ;
    ReadDouble( XDispl               , sizeOf( Double   ) ) ;
    ReadDouble( YDispl               , sizeOf( Double   ) ) ;
    {$ELSE}
    Read( ProjId               , sizeOf( Byte     ) ) ;
    Read( EllipsoidId          , sizeOf( Byte     ) ) ;
    Read( UnitsId              , sizeOf( Byte     ) ) ;
    Read( XScale               , sizeOf( Double   ) ) ;
    Read( YScale               , sizeOf( Double   ) ) ;
    Read( XDispl               , sizeOf( Double   ) ) ;
    Read( YDispl               , sizeOf( Double   ) ) ;
    {$ENDIF}

    if VersionNumber <= 100 then begin
      XScale := Power(10.0, CoordPrecision ) ;
      YScale := XScale ;
      XDispl := 0 ;
      YDispl := XDispl ;
    end ;

    for i:=low( ProjParams ) to high( ProjParams ) do
    {$IFDEF OXYGENE}
      ReadDouble( ProjParams[i], sizeOf( Double ) ) ;
    {$ELSE}
      Read( ProjParams[i], sizeOf( Double ) ) ;
    {$ENDIF}

    {$IFDEF OXYGENE}
    ReadDouble( DatumShiftX, sizeOf( Double ) ) ;
    ReadDouble( DatumShiftY, sizeOf( Double ) ) ;
    ReadDouble( DatumShiftZ, sizeOf( Double ) ) ;
    {$ELSE}
    Read( DatumShiftX, sizeOf( Double ) ) ;
    Read( DatumShiftY, sizeOf( Double ) ) ;
    Read( DatumShiftZ, sizeOf( Double ) ) ;
    {$ENDIF}

    for i:= low( DatumParams ) to high( DatumParams ) do begin
     {$IFDEF OXYGENE}
      ReadDouble( DatumParams[i], sizeOf( Double ) ) ;
     {$ELSE}
      Read( DatumParams[i], sizeOf( Double ) ) ;
      {$ENDIF}
      if VersionNumber <= 200 then DatumParams[i] := 0 ;
    end ;

    {$IFDEF GIS_NORECORDS}
    FExtent := new TGIS_Extent ;
    {$ENDIF}
    // calculate extent
      if (CoordOriginQuadrant=2) or
         (CoordOriginQuadrant=3) or
         (CoordOriginQuadrant=0)
      then begin
        FExtent.XMin := -( XMin + XDispl ) / XScale ;
        FExtent.XMax := -( XMax + XDispl ) / XScale ;
      end
      else begin
        FExtent.XMin :=  ( XMin - XDispl ) / XScale ;
        FExtent.XMax :=  ( XMax - XDispl ) / XScale ;
      end ;

      if FExtent.XMax < FExtent.XMin then begin
        tmp          := FExtent.XMax ;
        FExtent.XMax := FExtent.XMin ;
        FExtent.XMin := tmp          ;
      end ;

      if (CoordOriginQuadrant=3) or
         (CoordOriginQuadrant=4) or
         (CoordOriginQuadrant=0)
      then begin
        FExtent.YMin := -( YMin + YDispl ) / YScale ;
        FExtent.YMax := -( YMax + YDispl ) / YScale ;
      end
      else begin
        FExtent.YMin :=  ( YMin - YDispl ) / YScale ;
        FExtent.YMax :=  ( YMax - YDispl ) / YScale ;
      end ;

      if FExtent.YMax < FExtent.YMin then begin
        tmp          := FExtent.YMax ;
        FExtent.YMax := FExtent.YMin ;
        FExtent.YMin := tmp          ;
      end ;

      if ( XDispl = 0 ) and
         ( XMin   = 0 ) and
         ( XMax   = 0 ) and
         ( YDispl = 0 ) and
         ( YMin   = 0 ) and
         ( YMax   = 0 )
      then
        FExtent := GisNoWorld ;

      ReadPenDef   ;
      ReadBrushDef ;
  end ;

//==============================================================================
// TGIS_FileTAB_MAP
//==============================================================================

  constructor TGIS_FileTAB.Create(
    const _path : String;
    const _write : Boolean
  ) ;
  begin
    {$IFDEF OXYGENE}
      inherited Create ;
    {$ENDIF}
    tabLines     := TStringList.Create ;
    tabExtracted := TStringList.Create ;

    tabPath     := _path ;
    tabCodePage := GisSystemCodePage ;
  end ;

  procedure TGIS_FileTAB.exportForEach(
          _shp    : TGIS_Shape ;
      var _abort  : Boolean
  ) ;
  var
    shp_tmp    : TGIS_Shape ;
    shp_cplx   : TGIS_Shape ;
    poFeature  : T_Feature  ;
    shp_no     : Integer    ;
  begin
    shp_tmp := _shp.PrepareExportShape(
                     importCS, importExtent, importTrunc, True
                   ) ;
    if assigned( shp_tmp ) then
      try
        if ( not shp_tmp.IsDeleted ) and
           ( ( shp_tmp.ShapeType = importShpType   ) or
             ( importShpType = TGIS_ShapeType.Unknown )
           )
        then begin
          if shp_tmp.ShapeType = TGIS_ShapeType.Complex then begin
            for shp_no := 0 to TGIS_ShapeComplex(shp_tmp).ShapesCount-1 do begin
              shp_cplx := TGIS_ShapeComplex(shp_tmp).GetShape(importRecNo) ;

              shp_cplx.Lock( TGIS_Lock.Projection ) ;
              try
                poFeature := T_TABFile(importFile).CreateFeature( shp_cplx ) ;
                try
                  T_TABFile(importFile).SetFeature( poFeature ) ;
                finally
                  FreeObject( poFeature ) ;
                end ;
              finally
                shp_cplx.Unlock ;
              end ;

              if assigned( importRtreeFile ) then
                TGIS_RTree(importRtreeFile).Insert( shp_cplx.Extent, importRecNo ) ;

              TGIS_FileDBF(importDataFile).AddRecord( shp_tmp ) ;
            end ;
          end
          else begin
            shp_tmp.Lock( TGIS_Lock.Projection ) ;
            try
              poFeature := T_TABFile(importFile).CreateFeature( shp_tmp ) ;
              try
                T_TABFile(importFile).SetFeature( poFeature ) ;
              finally
                FreeObject( poFeature ) ;
              end ;
            finally
              shp_tmp.Unlock ;
            end ;

            if assigned( importRtreeFile ) then
              TGIS_RTree(importRtreeFile).Insert( shp_tmp.Extent, importRecNo ) ;

            TGIS_FileDBF(importDataFile).AddRecord( shp_tmp ) ;
          end ;
        end ;
      finally
        inc( importRecNo ) ;
        if _shp <> shp_tmp then
          FreeObject( shp_tmp ) ;
      end ;

    if importShapeNo mod 100 = 1 then begin
      _abort := importLayerObj.RaiseBusyShake( importLayerObj, _shp.Uid, importEndUid ) ;
    end ;
    inc( importShapeNo ) ;
  end ;

  procedure TGIS_FileTAB.Write(
    const _self      : TGIS_LayerVector ;
    const _layer     : TGIS_LayerVector ;
    const _extent    : TGIS_Extent      ;
    const _type      : TGIS_ShapeType   ;
    const _scope     : String           ;
    const _shape     : TGIS_Shape       ;
    const _de9im     : String           ;
    const _truncated : Boolean
  ) ;
  var
    poFile     : T_TABFile ;
    ext        : TGIS_Extent ;
    fe, fn     : Double ;
    bdbounds   : Boolean ;
    end_uid    : TGIS_Uid       ;
    abort      : Boolean        ;
    same_name  : Boolean        ;
    shp_type   : TGIS_ShapeType ;
    old_scope  : String ;
    isrtree    : Boolean        ;
    rtree_file : TGIS_RTree     ;
    map_path   : String ;
    id_path    : String ;
    rtree_path : String ;
    data_file  : TGIS_FileDBF ;
    scale      : Double ;

    procedure set_coordsys( const _cs : TGIS_CSCoordinateSystem ) ;
    var
      i             : Integer ;
      proj_id       : Integer ;
      datum_id      : Integer ;
      units_id      : Integer ;
      ellipsoid_id  : Integer ;
      aparams       : TGIS_DoubleArray ;
      dparams       : TGIS_DoubleArray ;
    begin
      TGIS_CSFactoryMapInfo.ParseCs( _cs, proj_id, datum_id, units_id, ellipsoid_id,
                         aparams, dparams
                        ) ;
      poFile.poMAPFile.poHeader.sProj.nProjId      := proj_id  ;
      poFile.poMAPFile.poHeader.sProj.nDatumId     := datum_id ;
      poFile.poMAPFile.poHeader.sProj.nUnitsId     := units_id ;
      poFile.poMAPFile.poHeader.sProj.nEllipsoidId := ellipsoid_id ;

      for i := 0 to high( aparams ) do
        poFile.poMAPFile.poHeader.sProj.adProjParams[ i ] := aparams[ i ] ;
      for i := high( aparams ) + 1 to 5 do
        poFile.poMAPFile.poHeader.sProj.adProjParams[ i ] := 0 ;

      poFile.poMAPFile.poHeader.sProj.dDatumShiftX     := dparams[ 0 ] ;
      poFile.poMAPFile.poHeader.sProj.dDatumShiftY     := dparams[ 1 ] ;
      poFile.poMAPFile.poHeader.sProj.dDatumShiftZ     := dparams[ 2 ] ;
      poFile.poMAPFile.poHeader.sProj.adDatumParams[0] := dparams[ 3 ] ;
      poFile.poMAPFile.poHeader.sProj.adDatumParams[1] := dparams[ 4 ] ;
      poFile.poMAPFile.poHeader.sProj.adDatumParams[2] := dparams[ 5 ] ;
      poFile.poMAPFile.poHeader.sProj.adDatumParams[3] := dparams[ 6 ] ;

      if _cs is TGIS_CSProjectedCoordinateSystem  then
        poFile.poMAPFile.poHeader.sProj.adDatumParams[4]  :=
           RadToDeg(
             TGIS_CSProjectedCoordinateSystem( _cs ).Geocs.PrimeMeridian.Longitude
           )
      else if _cs is TGIS_CSGeographicCoordinateSystem then
        poFile.poMAPFile.poHeader.sProj.adDatumParams[4]  :=
           RadToDeg(
             TGIS_CSGeographicCoordinateSystem( _cs ).PrimeMeridian.Longitude
           )
      else
        poFile.poMAPFile.poHeader.sProj.adDatumParams[4]  := 0 ;
    end ;

    procedure rename_files ;
    begin
      try
        DeleteFile( GetBackupName( tabPath  ) ) ;
        DeleteFile( GetBackupName( map_path ) ) ;
        DeleteFile( GetBackupName( id_path  ) ) ;
        DeleteFile( GetBackupName( TGIS_LayerTAB( _self ).PathDAT  ) ) ;

        poFile.Close ;

        if isrtree then
          DeleteFile( GetBackupName( rtree_path ) ) ;

        RenameFile( tabPath,  GetBackupName( tabPath  ) ) ;
        RenameFile( map_path, GetBackupName( map_path ) ) ;
        RenameFile( id_path,  GetBackupName( id_path  ) ) ;
        RenameFile( TGIS_LayerTAB( _self ).PathDAT ,
                             GetBackupName( TGIS_LayerTAB( _self ).PathDAT )
                            ) ;
        if isrtree then
          RenameFile( rtree_path, GetBackupName( rtree_path ) ) ;

        try
          if not RenameFile( GetTemporaryName( tabPath ), tabPath   )
          then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ), tabPath,
                                         GetLastError
                                       ) ;
          if not RenameFile( GetTemporaryName( map_path ), map_path   )
          then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ), map_path,
                                         GetLastError
                                       ) ;
          if not RenameFile( GetTemporaryName( id_path ), id_path   )
          then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ), id_path,
                                         GetLastError
                                       ) ;
          if not RenameFile( GetTemporaryName( TGIS_LayerTAB( _self ).PathDAT ),
                                               TGIS_LayerTAB( _self ).PathDAT
                                              )
          then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ),
                                         TGIS_LayerTAB( _self ).PathDAT ,
                                         GetLastError
                                       ) ;
          if isrtree then
            if not RenameFile( GetTemporaryName( rtree_path ), rtree_path )
            then
              raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEWRITE ), rtree_path,
                                           GetLastError
                                         ) ;
        except
          // recover ;
          RenameFile( GetBackupName( tabPath  ), tabPath  ) ;
          RenameFile( GetBackupName( map_path ), map_path ) ;
          RenameFile( GetBackupName( id_path  ), id_path  ) ;
          RenameFile( GetBackupName( TGIS_LayerTAB( _self ).PathDAT  ),
                                     TGIS_LayerTAB( _self ).PathDAT
                                    ) ;
          if isrtree then begin
            RenameFile( GetBackupName( rtree_path ), rtree_path ) ;
            DeleteFile( GetTemporaryName( rtree_path ) ) ;
          end ;

          raise ;
        end ;
      finally
      end ;
    end ;

    // add _DUMMY filed to ensure at leas one column
    procedure add_dummy_field ;
    var
      id  : Integer ;
      cnt : Integer ;
      fld : TGIS_FieldInfo ;
    begin
      cnt := 0 ;
      for id := 0 to _self.Fields.Count -1 do begin
        fld := _self.FieldInfo( id ) ;
        if fld.Temporary then continue ;
        if fld.Deleted   then continue ;
        inc( cnt ) ;
      end;

      if cnt > 0 then exit ;

      id := _self.FindField( GIS_FIELD_DUMMY ) ;
      if id >= 0 then begin
        fld := _self.FieldInfo( id ) ;
        fld.Deleted := False
      end
      else
        _self.AddFieldInternal( GIS_FIELD_DUMMY, TGIS_FieldType.String, 1, 0 ) ;
    end;

    procedure importStructure(
      const _layer : TGIS_LayerVector
    ) ;
    var
      i         : Integer ;
      fld       : TGIS_FieldInfo ;
      fname     : String ;
      borig_uid : Boolean ;
    begin
      borig_uid := GisMetadataAsBoolean( TAB_METADATA_WRITEORIGINALUID, False ) ;

      for i:=0 to _layer.Fields.Count - 1 do begin
        fld := _layer.FieldInfo( i ) ;

        if fld.FileFormat and not fld.IsUID then continue ;
        if fld.Temporary  then continue ;

        if fld.Deleted then begin
          fld := _self.FieldInfo( _self.FindField( fld.NewName )  ) ;
          if assigned( fld ) then
            fld.Deleted := True ;
          continue
        end ;

        if fld.IsUID and borig_uid then
          fname := GIS_SQL_ORIGINAL_UID_NAME
        else
          fname := fld.NewName ;

        if _self.FieldInfo( _self.FindField( fname ) ) = nil then begin
          _self.AddField( fname,
                    fld.FieldType,
                    fld.NewWidth,
                    fld.NewDecimal
                  ) ;
          if _self.Fields.Count > 0 then begin
            with _self.FieldInfo( _self.Fields.Count -1 ) do begin
              IsUID := fld.IsUID ;
              Binary := fld.Binary ;
            end ;
          end ;
        end;
      end ;
    end ;

  begin

    if not assigned( _layer ) then exit ;

    end_uid  := _layer.GetLastUid ;
    abort    := False ;

    _layer.RaiseBusyPrepare( _layer, Format( _rsrc( GIS_RS_BUSY_SAVE ), [tabPath] ) ) ;
    try
      importStructure( _layer ) ;
      add_dummy_field ;
      _self.PrepareExportFieldNames( TAB_MAX_NAME_LENGTH ) ;

      // prepare temporary database
      data_file := TGIS_FileDBF.Create(
        GetTemporaryName( TGIS_LayerTAB( _self ).PathDAT ) ,
        _self, TGIS_StreamMode.&Create, nil, nil //?FOnRead, FOnWrite
      ) ;
      data_file.CodePage    := _layer.CodePage    ;
      data_file.WriteHeader ;

      // prepare new types and size
      writeTABStructure( _self );

      data_file.WriteStructure ;

      same_name := CompareText( GetPathAbsolute( '', tabPath        ),
                                GetPathAbsolute( '', _layer.Path )
                              ) = 0  ;

      shp_type   := _type ;
      ext        := GisCommonExtent( _extent, _layer.Extent ) ;
      isrtree    := _layer.UseRTree ;
      rtree_path := tabPath + GIS_RTREE_EXT ;

      poFile := T_TABFile.Create ;
      poFile.Layer := _layer ;
      try
        // prepare RTRee index
        if isrtree then
          rtree_file := TGIS_RTree.Create( GetTemporaryName( rtree_path ) )
        else
          rtree_file := nil ;

        poFile.OpenEx( tabPath ) ;

        // no assigned CS, use _layer CS
        if _self.CS is TGIS_CSUnknownCoordinateSystem then
          _self.CS := _layer.CS
        else if _layer.CS.CanConvert( _self.CS ) then begin
          ext := _layer.CS.ExtentToCS( _self.CS, ext ) ;
          if not GisIsValidExtent( ext ) then
            raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_PRJ_NOTSOLVABLE ), '', 0 ) ;
        end ;

        // Assign the coordinate system
        set_coordsys( _self.CS ) ;

        bdbounds := GisMetadataAsBoolean( TAB_METADATA_DEFAULT_BOUNDS, False ) ;
        if bdbounds then begin
          // set default bounds for new layer
          if _self.CS is TGIS_CSGeographicCoordinateSystem then
            ext := GisExtent( -1000, -1000, 1000, 1000 )
          else begin
            fe := 0 ;
            fn := 0 ;
            if _self.CS is TGIS_CSProjectedCoordinateSystem then begin
              fe := TGIS_CSProjectedCoordinateSystem(_self.CS).ProjectionParams.FalseEasting ;
              fn := TGIS_CSProjectedCoordinateSystem(_self.CS).ProjectionParams.FalseNorthing ;
            end ;

            ext := GisExtent( -30000000 + fe, -15000000 + fn, 30000000 + fe, 15000000 + fn ) ;
          end ;
        end ;

        poFile.SetBounds( ext ) ;

        old_scope := _layer.Scope ;
        if same_name then
          _layer.Scope := '' ;

        if assigned( _layer.Viewer ) then
          scale := _layer.Viewer.Ref.ScaleAsFloat
        else
          scale := 1 ;

        importExtent    := _extent ;
        importTrunc     := _truncated ;
        importShpType   := shp_type ;
        importLayerObj  := _layer ;
        importCS        := _self.CS ;
        importFile      := poFile ;
        importDataFile  := data_file ;
        importRtreeFile := rtree_file ;
        importEndUid    := end_uid ;
        importShapeNo   := 0 ;
        importRecNo     := 1 ;

        abort := not _layer.ForEach( _extent, _scope, _shape, _de9im, True, scale,
                                     {$IFNDEF OXYGENE}
                                       exportForEach
                                     {$ELSE}
                                       @exportForEach
                                     {$ENDIF}
                                   ) ;
      finally
        _layer.Scope := old_scope ;

        if abort then begin
          FreeObject( data_file  ) ;
          FreeObject( rtree_file ) ;

          DeleteFile( GetTemporaryName( tabPath    ) ) ;
          DeleteFile( GetTemporaryName( map_path   ) ) ;
          DeleteFile( GetTemporaryName( id_path    ) ) ;
          DeleteFile( GetTemporaryName( TGIS_LayerTAB( _self ).PathDAT ) ) ;
          DeleteFile( GetTemporaryName( rtree_path ) ) ;
        end
        else begin
          if assigned( rtree_file ) then begin
            rtree_file.Commit( True ) ;
            FreeObject( rtree_file ) ;
          end ;

          map_path := poFile.MAPPath ;
          id_path  := poFile.IDPath ;

          data_file.WriteHeader ;
          FreeObject( data_file ) ;

          if same_name then
            if _layer is TGIS_LayerTAB then
              TGIS_LayerTAB( _layer ).CloseFiles ;

          rename_files ;
        end ;

        FreeObject( poFile ) ;
      end ;

    finally
      _layer.RaiseBusyRelease( _layer ) ;
    end ;
  end ;

  procedure TGIS_FileTAB.doDestroy ;
  begin
    FreeObject( tabLines     ) ;
    FreeObject( tabExtracted ) ;

    inherited ;
  end ;

  function TGIS_FileTAB.Prerecognize : Integer ;
  var
    tkn     : TGIS_Tokenizer ;
    line_no : Integer ;
    line    : String  ;
    {$IFDEF DCC}
      enc   : TEncoding ;
    {$ENDIF}

    procedure parse_file( const _line : String ) ;
    begin
      tkn.Execute( _line, [' '], True );
      if tkn.Result.Count < 2 then tabFilePath := ''
                              else tabFilePath := tkn.Result[1] ;
    end ;

    procedure parse_type( const _line : String ) ;
    begin
      tabType := 0 ;

      tkn.Execute( UpperCase( _line ), [' '], True );
      if tkn.Result.Count < 2 then exit ;

      if      tkn.Result[1] = TAB_NATIVE then tabType := 1
      else if tkn.Result[1] = TAB_DBF    then tabType := 2
      else if tkn.Result[1] = TAB_RASTER then tabType := 3
      else if tkn.Result[1] = TAB_ACCESS then tabType := 4
      else if tkn.Result[1] = TAB_LINKED then tabType := 1 ;

      if tabType <> 4 then begin
        if tkn.Result.Count < 4 then exit ;
        if      tkn.Result[3] = TAB_WINDOWSLATIN1 then tabCodePage := 1252
        else if tkn.Result[3] = TAB_WINDOWSLATIN2 then tabCodePage := 1250 ;
      end
      else begin
        if tkn.Result[2] <> TAB_TABLE then exit ;
        tabTableName := tkn.Result[3] ;
        if tkn.Result.Count < 5 then exit ;
        if      tkn.Result[5] = TAB_WINDOWSLATIN1 then tabCodePage := 1252
        else if tkn.Result[5] = TAB_WINDOWSLATIN2 then tabCodePage := 1250 ;
      end ;
    end ;

    procedure parse_coords( const _line : String ) ;
    begin
      tabCoordSys := _line ;
    end ;

    procedure parse_tiepoints ;
    begin
      tabExtracted.Clear ;
      tabExtractedLine := line_no ;
      while tabLines.Count > 0  do begin
        line := tabLines[ 0 ] ;

        if Pos( '(', line ) <> StringFirst then exit ;
        tabLines.Delete( 0 );
        tabExtracted.Add( line ) ;
      end ;
    end ;

    procedure parse_fields ;
    var
      count : Integer ;
    begin
      count := 0 ;

      tabExtracted.Clear ;
      while tabLines.Count > 0  do begin
        line := tabLines[ 0 ] ;
        tabLines.Delete( 0 );

        tkn.Execute( UpperCase( line ), [' ', ':' ], False ) ;
        if tkn.Result.Count < 2 then exit ;

        try
          count := StrToInt( tkn.Result[1] ) ;
        except
          exit ;
        end ;

        break ;
      end ;

      tabExtractedLine := line_no ;
      while ( count > 0 ) and ( tabLines.Count > 0 ) do begin
        line := tabLines[ 0 ] ;
        tabLines.Delete( 0 );

        tabExtracted.Add( line ) ;
        dec( count ) ;
      end ;

    end ;

  begin
    Result := 0 ;
    tabType := 1 ;
    if not SafeFileExists( tabPath ) then exit ;

    tkn := TGIS_Tokenizer.Create ;
    try
      line_no := -1 ;
      {$IFDEF DCC}
        enc := TEncoding.GetEncoding( CodePage ) ;
        try
          tabLines.LoadFromFile( tabPath, enc ) ;
        finally
          FreeObject( enc ) ;
        end ;
      {$ELSE}
        tabLines.LoadFromFile( tabPath ) ;
      {$ENDIF}
      try
        for line_no := 0 to tabLines.Count - 1 do begin
           tabLines[ line_no ] := Trim( tabLines[ line_no ] ) ;
        end ;
        line_no := 0 ;

        while tabLines.Count > 0  do begin
          line := tabLines[ 0 ] ;

          if      Pos( UpperCase( TAB_FIELDS_DEF ) + ' ', UpperCase( line ) ) = StringFirst then
                  break ;

          tabLines.Delete( 0 );
          inc( line_no ) ;

          if      Pos( UpperCase( TAB_DEFINITION ), UpperCase( line ) ) = StringFirst then
                  break ;
        end ;

        while tabLines.Count > 0  do begin
          line := tabLines[ 0 ] ;
          tabLines.Delete( 0 );
          inc( line_no ) ;

          if      Pos( UpperCase( TAB_FILE ), UpperCase( line ) ) = StringFirst then
                  begin
                    parse_file( line ) ;
                  end
          else if Pos( UpperCase( TAB_TYPE ), UpperCase( line ) ) = StringFirst then
                  begin
                    parse_type( line ) ;
                    if      tabType = 1 then parse_fields
                    else if tabType = 2 then parse_fields
                    else if tabType = 3 then parse_tiepoints
                    else if tabType = 4 then break ;
                  end
          else if Pos( UpperCase( TAB_FIELDS_DEF), UpperCase( line ) ) = StringFirst then
                  begin
                    tabLines.Insert( 0, line );
                    parse_fields;
                  end
          else if Pos( UpperCase( TAB_COORDSYS ), UpperCase( line ) ) = StringFirst then
                  begin
                    parse_coords( line ) ;
                  end
          else if Pos( UpperCase( TAB_SEAMLESS ), UpperCase( line ) ) = StringFirst then
                  begin
                    tabType := 5 ;
                  end
        end ;
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), tabPath,line_no ) ;
      end ;
    finally
      FreeObject( tkn ) ;
      Result := tabType ;
    end ;

  end ;

  procedure TGIS_FileTAB.ReadFieldsStructure( const _layer : TGIS_LayerVector ) ;
  var
    tkn       : TGIS_Tokenizer ;
    line      : String         ;
    line_no   : Integer        ;
    sname     : String         ;
    stype     : String         ;
    swidth    : String         ;
    sdecimal  : String         ;

    function _t( const _val : String ) : Boolean ;
    begin
      Result := CompareText( stype, _val ) = 0 ;
    end ;

    function get_width : Integer ;
    begin
      try
        if IsStringEmpty( swidth ) then Result := 10
                                   else Result := StrToInt( swidth )
      except
        Result := 10 ;
      end ;
    end ;

    function get_decimal : Integer ;
    begin
      try
        if IsStringEmpty( sdecimal ) then Result := 2
                                     else Result := StrToInt( sdecimal )
      except
        Result := 2 ;
      end ;
    end ;
  begin
    tkn := TGIS_Tokenizer.Create ;
    try
      line_no := tabExtractedLine ;
      try
        // clear any existing field list
          _layer.Fields.Clear ;

        // read field by field
           while tabExtracted.Count > 0 do begin
             line := tabExtracted[ 0 ] ;
             tabExtracted.Delete( 0 ) ;
             inc( line_no ) ;

             sname    := '' ;
             stype    := '' ;
             swidth   := '' ;
             sdecimal := '' ;

             tkn.Execute( line, [' ', '(', ')', ',', ';'], True ) ;

             if tkn.Result.Count > 0 then sname    := tkn.Result[0]
                                     else sname    := '' ;
             if tkn.Result.Count > 1 then stype    := tkn.Result[1]
                                     else stype    := '' ;
             if tkn.Result.Count > 2 then swidth   := tkn.Result[2]
                                     else swidth   := '' ;
             if tkn.Result.Count > 3 then sdecimal := tkn.Result[3]
                                     else sdecimal := '' ;

             with _layer do begin
               if      _t( TAB_DECIMAL  ) then
                 AddFieldInternal( sname, TGIS_FieldType.Number ,
                                   get_width, get_decimal
                                 )
               else if _t( TAB_LOGICAL  ) then
                 AddFieldInternal( sname, TGIS_FieldType.Boolean,
                                   1, 0
                                 )
               else if _t( TAB_SMALLINT ) then
                 AddFieldInternal( sname, TGIS_FieldType.Number ,
                                   4, 0, True, 2
                                 )
               else if _t( TAB_INTEGER  ) then
                 AddFieldInternal( sname, TGIS_FieldType.Number ,
                                   10, 0, True, 4
                                 )
               else if _t( TAB_FLOAT    ) then
                 AddFieldInternal( sname, TGIS_FieldType.Float  ,
                                   20, 5, True, 8
                                 )
               else if _t( TAB_CHAR     ) then
                 AddFieldInternal( sname, TGIS_FieldType.String ,
                                   get_width, 0
                                 )
               else if _t( TAB_DATE     ) then
                 AddFieldInternal( sname, TGIS_FieldType.Date ,
                                   10, 0, True, 4
                                 )
               else if _t( TAB_TIME     ) then
                 AddFieldInternal( sname, TGIS_FieldType.Date ,
                                   8, 0, True, 4
                                 )
               else if _t( TAB_DATETIME     ) then
                 AddFieldInternal( sname, TGIS_FieldType.Date ,
                                   18, 0, True, 8
                                 ) ;
             end ;
           end ;
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), tabPath,line_no ) ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  procedure TGIS_FileTAB.ReadRaster( const _layer : TGIS_LayerPixel ) ;
  var
    tkn     : TGIS_Tokenizer ;
    line    : String ;

    line_no : Integer ;
    ptg     : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
    ptg_s   : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
    ptg_d   : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF};
    gcp     : TGIS_TransformPolynomial ;
    a,d,b,c,e,f : Double ;
    w,h : Double ;
  begin
    tkn := TGIS_Tokenizer.Create ;
    try
      line_no := tabExtractedLine ;
      try
        gcp := TGIS_TransformPolynomial.Create ;
        try
          while tabExtracted.Count > 0 do begin
            line := tabExtracted[ 0 ] ;
            tabExtracted.Delete( 0 ) ;
            inc( line_no ) ;

            tkn.Execute( Copy( line, StringFirst + 1, 1024 ), [' ', '(', ')', ','], True ) ;

            if tkn.Result.Count > 0 then
               ptg_d.X := DotStrToFloat( tkn.Result[0] ) ;
            if tkn.Result.Count > 1 then
               ptg_d.Y := DotStrToFloat( tkn.Result[1] ) ;
            if tkn.Result.Count > 2 then
               ptg_s.X := RoundS( DotStrToFloat( tkn.Result[2] ) ) ;
            if tkn.Result.Count > 3 then
               ptg_s.Y := RoundS( DotStrToFloat( tkn.Result[3] ) ) ;

            gcp.AddPoint( ptg_s, ptg_d, line_no );
          end ;
          gcp.Prepare( TGIS_PolynomialOrder.First ) ;

          w := _layer.BitWidth  ;
          h := _layer.BitHeight ;

          ptg := gcp.Transform( GisPoint( 0   , 0 ) ) ;
          c := ptg.X ;
          f := ptg.Y ;

          ptg := gcp.Transform( GisPoint( w, 0 ) ) ;

          a := ( ptg.X - c) / w ;

          ptg := gcp.Transform( GisPoint( 0, h ) ) ;

          e := ( ptg.Y - f ) / h ;

          ptg := gcp.Transform( GisPoint( w/2, h/2 ) ) ;

          b := ( ptg.X - ( a * w/2 + c ) ) / (h/2) ;
          d := ( ptg.Y - ( e * h/2 + f ) ) / (w/2) ;

        finally
          FreeObject( gcp ) ;
        end ;

        _layer.SetWorld( a,d,b,e,c,f ) ;

        doCoordSys( tabCoordSys, _layer ) ;
      except
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_TXTFILESTRUCT ), tabPath,line_no ) ;
      end ;
    finally
      FreeObject( tkn ) ;
    end ;
  end ;

  procedure TGIS_FileTAB.doCoordSys( const _param : String ;
                                     const _layer : TGIS_LayerPixel
                                    ) ;
  begin
    try
      _layer.CS := TGIS_CSFactoryMapInfo.BuildCs( _param ) ;
    except
      _layer.CS := CSUnknownCoordinateSystem ;
      raise ;
    end ;
  end ;

  function TGIS_FileTAB.prepareTABFields(
    const _layer   : TGIS_LayerVector ;
      var _version : Integer
  ) : Integer ;
  var
    i   : Integer ;
    fld : TGIS_FieldInfo ;
    v9  : Boolean ;
  begin
    Result := 0 ;
    v9 := GisMetadataAsBoolean( TAB_METADATA_SAVEVERSION9, False ) ;

    for i := 0 to _layer.Fields.Count - 1 do begin
      fld := _layer.FieldInfo( i ) ;

      if fld.Temporary then continue ;
      if fld.Deleted   then continue ;

      if fld.FieldType = TGIS_FieldType.Date then begin
        if (fld.Binary = 8) then
          _version := 900
        else if (fld.Binary = 0) then begin
          if not v9 then
            fld.Binary := 4
          else begin
            fld.Binary := 8 ;
            _version := 900 ;
          end ;
        end ;
      end
      else if fld.FieldType = TGIS_FieldType.Float then
        fld.Binary := 8
      else if fld.NewWidth > 254 then
        fld.NewWidth := 254 ;

      inc( Result ) ;
    end ;
  end ;

  procedure TGIS_FileTAB.writeTABStructure(
    const _layer : TGIS_LayerVector
  ) ;
  var
    lst : TStringList ;
    i  : Integer ;

    cnt : Integer ;
    fld : TGIS_FieldInfo ;
    ename : String ;
    tabver : Integer ;

    function prepareType( const _type   : TGIS_FieldType ;
                          const _size   : Integer ;
                          const _prec   : Integer ;
                          const _binary : Integer
                         ) : String ;
    begin
      case _type of
        TGIS_FieldType.String  :
           Result := Format( '%s (%d)', [ TAB_CHAR, _size ] ) ;
        TGIS_FieldType.Number  :
          begin
            if ( _prec = 0 ) then begin
              if _binary = 2 then
                Result := Format( '%s', [ TAB_SMALLINT ] )
              else if _binary = 4 then
                Result := Format( '%s', [ TAB_INTEGER ] )
              else
                Result := Format( '%s (%d,%d)', [ TAB_DECIMAL, _size, 0 ] );
            end
            else if _binary = 8 then
              Result := Format( '%s', [ TAB_FLOAT ] )
            else begin
              // to match dbf rules
              if _size - _prec < 2  then
                Result := Format( '%s (%d,%d)', [ TAB_DECIMAL, _size, _size-2 ] )
              else
                Result := Format( '%s (%d,%d)', [ TAB_DECIMAL, _size, _prec ] );
            end ;
          end ;
        TGIS_FieldType.Float   :
          Result := Format( '%s', [ TAB_FLOAT ] ) ;
        TGIS_FieldType.Boolean :
          Result := Format( '%s', [ TAB_LOGICAL ] ) ;
        TGIS_FieldType.Date    :
          if _binary = 4 then begin
            if _size = 8 then
              Result := Format( '%s', [ TAB_TIME ] )
            else
              Result := Format( '%s', [ TAB_DATE ] )
          end
          else
            Result := Format( '%s', [ TAB_DATETIME ] )
        else
          begin
            assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
          end ;
      end ;
    end ;

  begin
    lst := TStringList.Create ;
    tabver := 300 ;
    try
      cnt := prepareTABFields( _layer, tabver ) ;

      lst.Add( '!table' ) ;
      lst.Add( Format( '!version %d', [ tabver ] ) ) ;
      lst.Add( Format( '!charset %s', [ 'Neutral' ] ) ) ;
      lst.Add( '' ) ;

      lst.Add( TAB_DEFINITION ) ;

      lst.Add( Format( '  %s %s %s "%s"',
                       [ TAB_TYPE,
                         TAB_NATIVE,
                         TAB_CHARSET,
                         TAB_NEUTRAL
                       ]
                     )
             ) ;
      lst.Add( Format( '  %s%d', [ TAB_FIELDS, cnt ] ) ) ;

      for i := 0 to _layer.Fields.Count - 1 do begin
        fld := _layer.FieldInfo( i ) ;

        if fld.Temporary then continue ;
        if fld.Deleted  then continue ;

        {$IFDEF OXYGENE}
        ename := StringReplace( fld.NewName, ' ', '_', [TReplaceFlag.rfReplaceAll] ) ;
        {$ELSE}
        ename := StringReplace( fld.NewName, ' ', '_', [rfReplaceAll] ) ;
        {$ENDIF}
        lst.Add( '    ' + ename + ' ' +
                    prepareType( fld.FieldType,
                                 fld.NewWidth,
                                 fld.NewDecimal,
                                 fld.Binary
                                ) + ' ;'
                    ) ;
      end ;

      lst.SaveToFile( GetTemporaryName( _layer.Path ) ) ;
    finally
      FreeObject( lst ) ;
    end ;
  end ;

//==============================================================================
// TGIS_FileTAB_MAP
//==============================================================================

  procedure TGIS_FileTAB_MAP.CursorOpen(
    const _cursor : Integer
  ) ;
  begin
    if _cursor >= {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( cursorState )  then
      {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( cursorState, _cursor + 1 ) ;

    {$IFDEF GIS_NORECORDS}
      if not assigned( cursorState[_cursor] ) then
        cursorState[_cursor] := new T_cursorStateTAB_MAP ;
      cursorState[ _cursor ].currRecord := new TGIS_RecordTAB_MAP ;
    {$ENDIF}
    cursorState[ _cursor ].curInUse := True ;
    ReadHeader ;
  end ;

  procedure TGIS_FileTAB_MAP.CursorClose(
    const _cursor : Integer
  ) ;
  var
    i : Integer ;
  begin
    cursorState[_cursor].curInUse := False ;

    // truncate cursorState at the tail;
    {$IFNDEF OXYGENE}
      for i := Length( cursorState ) - 1 downto 0 do begin
    {$ELSE}
      for i := cursorState.Length - 1 downto 0 do begin
    {$ENDIF}
      if not cursorState[i].curInUse then begin
        {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( cursorState, i ) ;
      end
      else
        break ;
    end ;
  end ;

  procedure TGIS_FileTAB_MAP.NextBlock(
    const _offset : Integer
  ) ;
  var
    ipos : Cardinal ;
    md   : Integer  ;
    mask : Integer ;
  begin
    mask := BlockSize-1;
    ipos := Position ;

    if      _offset > 0 then begin
              md := ( ipos + Cardinal(_offset ) ) and mask ;

              if (md = 0) or (md > _offset) then exit ;
            end
    else if _offset < 0 then begin
              exit ;
            end ;


    if _offset = 1 then
      Position := ( (ipos) div BlockSize) * BlockSize + 4
    else
      Position := ( (ipos-1) div BlockSize) * BlockSize + 4 ;

    {$IFDEF OXYGENE}
    ReadCardinal( ipos, 4 ) ;
    {$ELSE}
    Read( ipos, 4 ) ;
    {$ENDIF}

    if      ( ipos and $80000000 ) <> 0 then begin
              Position := Position + BlockSize ;
              NextBlock( 0 ) ;
            end
    else if ipos > Size then begin
              // something strange - very big pos value  was here
              // do nothing
            end
    else if ipos = 0 then begin
              // last block
              // do nothing
            end
    else    begin
              Position := ipos + 8 ;
            end ;
  end ;

  procedure TGIS_FileTAB_MAP.Move(
    const _offset : Integer
  ) ;
  var
    ipos : Integer ;
    off  : Integer ;
  begin
    ipos := _offset ;
    while ipos > 0 do begin
      off := Position - (Position div BlockSize) * BlockSize ;
      if ipos + off >= BlockSize then begin
        ipos := ipos - ( ( BlockSize - off ) div 4 ) * 4 ;
        NextBlock( 0 ) ;
      end
      else begin
        Position := Position + ipos ;
        ipos := 0 ;
      end ;
    end ;
  end ;

  procedure TGIS_FileTAB_MAPInternal.ReadPenDef ;
  var
    oldPos    : Int64;
  begin
    if ( NumMapToolBlocks > 0 ) then
    begin
      oldPos := Position ;
      try
        Position := FirstToolBlock + 8;
        ReadAllToolDefs;
      finally
        Position := oldPos;
      end ;
    end ;
  end ;

  procedure TGIS_FileTAB_MAPInternal.ReadBrushDef ;
  var
    oldPos    : Int64;
  begin
    if ( NumMapToolBlocks > 0 ) then
    begin
      oldPos := Position ;
      try
        Position := FirstToolBlock + 8;
        ReadAllToolDefs;
      finally
        Position := oldPos;
      end ;
    end ;
  end ;

  procedure TGIS_FileTAB_MAP.ReadAllToolDefs ;
  var
    defType,
    r, g, b   : Byte;
    size      : Integer ;
    font    : TBytes ;
  begin

    while True do
    begin
      defType := ReadByte;
      case defType of
        TAB_TOOL_PEN    : begin
            {$IFNDEF OXYGENE}
              size := Length( TabPenDef );
              SetLength( TabPenDef, size + 1 );
            {$ELSE}
              if not assigned( TabPenDef ) then begin
                TabPenDef := new TGIS_TABPenDef[1] ;
                size := 0 ;
              end
              else
                size := TabPenDef.Length ;

              {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( TabPenDef, size + 1 ) ;
            {$ENDIF}

            {$IFDEF GIS_NORECORDS}
              TabPenDef[size] := new TGIS_TABPenDef ;
            {$ENDIF}
            TabPenDef[size].nRefCount    := ReadLongint;
            TabPenDef[size].nPixelWidth  := ReadByte;
            TabPenDef[size].nLinePattern := ReadByte;
            TabPenDef[size].nPointWidth  := ReadByte;
            b := ReadByte;
            g := ReadByte;
            r := ReadByte;
            
            TabPenDef[size].rgbColor := TGIS_Color.FromBGR( r, g, b ) ;

            if (TabPenDef[size].nPixelWidth > 7) then begin
                TabPenDef[size].nPointWidth := TabPenDef[size].nPointWidth +
                         (TabPenDef[size].nPixelWidth-8)*$100;
                TabPenDef[size].nPixelWidth := 1;
            end ;
          end ;
        TAB_TOOL_BRUSH  : begin
            {$IFNDEF OXYGENE}
              size := Length( TabBrushDef );
              SetLength( TabBrushDef, size + 1 );
            {$ELSE}
              if not assigned( TabBrushDef ) then begin
                TabBrushDef := new TGIS_TABBrushDef[1] ;
                size := 0 ;
              end
              else
                size := TabBrushDef.Length ;

              {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( TabBrushDef, size + 1 ) ;
            {$ENDIF}

            {$IFDEF GIS_NORECORDS}
              TabBrushDef[size] := new TGIS_TABBrushDef ;
            {$ENDIF}
            TabBrushDef[size].nRefCount        := ReadLongint;
            TabBrushDef[size].nFillPattern     := ReadByte;
            TabBrushDef[size].bTransparentFill := ReadByte;
            b := ReadByte;
            g := ReadByte;
            r := ReadByte;
            TabBrushDef[size].rgbFGColor := TGIS_Color.FromBGR( r, g, b ) ;
            b := ReadByte;
            g := ReadByte;
            r := ReadByte;
            TabBrushDef[size].rgbBGColor := TGIS_Color.FromBGR( r, g, b ) ;
          end ;
        TAB_TOOL_FONT :  begin
            {$IFNDEF OXYGENE}
              size := Length( TabFontDef ) ;
              SetLength( TabFontDef, size + 1 ) ;
            {$ELSE}
              if not assigned( TabFontDef ) then begin
                TabFontDef := new TGIS_TABFontDef[1] ;
                size := 0 ;
              end
              else
                size := TabFontDef.Length ;

                {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( TabFontDef, size + 1 ) ;
            {$ENDIF}

            {$IFDEF GIS_NORECORDS}
              TabFontDef[size] := new TGIS_TABFontDef ;
            {$ENDIF}
            TabFontDef[size].nRefCount := ReadLongint ;
            {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( font, 32 ) ;
            {$IFDEF OXYGENE}
              ReadBuffer( font, 32 ) ;
            {$ELSE}
              ReadBuffer( font[0], 32 ) ;
            {$ENDIF}
            TabFontDef[size].szFontName := ConvertAnsiString( font ) ;
        end ;
        TAB_TOOL_SYMBOL : begin
            {$IFNDEF OXYGENE}
              size := Length( TabSymbolDef ) ;
              SetLength( TabSymbolDef, size + 1 ) ;
            {$ELSE}
              if not assigned( TabSymbolDef ) then begin
                TabSymbolDef := new TGIS_TABSymbolDef[1] ;
                size := 0 ;
              end
              else
                size := TabSymbolDef.Length ;

                {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( TabSymbolDef, size + 1 ) ;
            {$ENDIF}

            {$IFDEF GIS_NORECORDS}
              TabSymbolDef[size] := new TGIS_TABSymbolDef ;
            {$ENDIF}
            TabSymbolDef[ size ].nRefCount  := ReadLongint;
            TabSymbolDef[ size ].nSymbolNo  := ReadWord;
            TabSymbolDef[ size ].nPointSize := ReadWord;
            ReadByte;
            b := ReadByte;
            g := ReadByte;
            r := ReadByte;
            TabSymbolDef[size].rgbColor := TGIS_Color.FromBGR( r, g, b ) ;
        end
        else
            Break;
      end ;
    end ;
  end ;

  function TGIS_FileTAB_MAP.ReadByte
    : Byte ;
  begin
    assert( sizeOf( Byte ) = 1 ) ;
    NextBlock( sizeOf( Byte ) ) ;
    {$IFDEF OXYGENE}
    if ReadByte( Result, sizeOf( Byte ) ) <> sizeOf( Byte ) then
    {$ELSE}
    if Read( Result, sizeOf( Byte ) ) <> sizeOf( Byte ) then
    {$ENDIF}
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), Path, 0 ) ;
    NextBlock( -1 ) ;
  end ;

  function TGIS_FileTAB_MAP.ReadLongword
    : Cardinal ;
  begin
    assert( sizeOf( Cardinal ) = 4 ) ;
    NextBlock( sizeOf( Cardinal ) ) ;
    {$IFDEF OXYGENE}
    if ReadCardinal( Result, sizeOf( Cardinal ) ) <> sizeOf( Cardinal ) then
    {$ELSE}
    if Read( Result, sizeOf( Cardinal ) ) <> sizeOf( Cardinal ) then
    {$ENDIF}
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), Path, 4 ) ;
    NextBlock( -1 ) ;
  end ;

  function TGIS_FileTAB_MAP.ReadWord
    : Word ;
  begin
    assert( sizeOf( Word ) = 2 ) ;
    NextBlock( sizeOf( Word ) ) ;
    {$IFDEF OXYGENE}
    if ReadWord( Result, sizeOf( Word ) ) <> sizeOf( Word ) then
    {$ELSE}
    if Read( Result, sizeOf( Word ) ) <> sizeOf( Word ) then
    {$ENDIF}
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), Path, 2 ) ;
    NextBlock( -1 ) ;
  end ;

  function TGIS_FileTAB_MAP.ReadLongint
    : Int32 ;
  begin
    assert( sizeOf( Int32 ) = 4 ) ;
    NextBlock( sizeOf( Int32 ) ) ;
    {$IFDEF OXYGENE}
    if ReadInteger( Result, sizeOf( Int32 ) ) <> sizeOf( Int32 ) then
    {$ELSE}
    if Read( Result, sizeOf( Int32 ) ) <> sizeOf( Int32 ) then
    {$ENDIF}
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), Path, 4 ) ;
    NextBlock( -1 ) ;
  end ;

  function TGIS_FileTAB_MAP.ReadSmallint
    : SmallInt ;
  begin
    assert( sizeOf( SmallInt ) = 2 ) ;
    NextBlock( sizeOf( Result ) ) ;
    {$IFDEF OXYGENE}
    if ReadSmallInt( Result, sizeOf( Result ) ) <> sizeOf( Result ) then
    {$ELSE}
    if Read( Result, sizeOf( Result ) ) <> sizeOf( Result ) then
    {$ENDIF}
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), Path, 2 ) ;
    NextBlock( -1 ) ;
  end ;

  function TGIS_FileTAB_MAP.ReadRecord (
    const _cursor : Integer
  ) : {$IFDEF OXYGENE}
        TGIS_RecordTAB_MAP ;
      {$ELSE}
        PGIS_RecordTAB_MAP ;
      {$ENDIF}
  begin
    cursorState[ _cursor ].currRecord.RecordType   := ReadByte     ;
    cursorState[ _cursor ].currRecord.RecordNumber := ReadLongword ;
    cursorState[ _cursor ].currRecord.CenterX := 0 ;
    cursorState[ _cursor ].currRecord.CenterY := 0 ;
    {$IFDEF OXYGENE}
      Result := cursorState[ _cursor ].currRecord ;
    {$ELSE}
      Result := @cursorState[ _cursor ].currRecord ;
    {$ENDIF}
  end ;

  function TGIS_FileTAB_MAP.ReadCoordinate(
    const _compression : Boolean ;
    const _cursor : Integer
  ) : TGIS_Point ;
  var
    x, y : Integer ;

    function add_with_rangecheck(
      const _val : Integer ;
      const _add : Integer
    ) : Integer ;
    begin
    if      ( _add >= 0 ) and ( _val > (GIS_MAX_INTEGER - _add) ) then
      Result := GIS_MAX_INTEGER
    else if ( _add = GIS_MIN_INTEGER ) and ( _val < 0 ) then
      Result := GIS_MIN_INTEGER
    else if ( _add <> GIS_MIN_INTEGER ) and ( _add < 0 ) and
            ( _val < (GIS_MIN_INTEGER - _add) ) then
      Result := GIS_MIN_INTEGER
    else
      Result := _val + _add ;
    end ;

  begin
    if _compression then begin
      x := add_with_rangecheck( ReadSmallint, cursorState[ _cursor ].currRecord.CenterX );
      y := add_with_rangecheck( ReadSmallint, cursorState[ _cursor ].currRecord.CenterY );
    end
    else begin
      x := ReadLongint ;
      y := ReadLongint ;
    end ;

    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point ;
    {$ENDIF}
    if (CoordOriginQuadrant=2) or
       (CoordOriginQuadrant=3) or
       (CoordOriginQuadrant=0)
    then
      Result.X := -( x + XDispl ) / XScale
    else
      Result.X :=  ( x - XDispl ) / XScale ;

    if (CoordOriginQuadrant=3) or
       (CoordOriginQuadrant=4) or
       (CoordOriginQuadrant=0)
    then
      Result.Y := -( y + YDispl ) / YScale
    else
      Result.Y :=  ( y - YDispl ) / YScale ;
  end ;

  function TGIS_FileTAB_MAP.ReadCenter(
    const _cursor : Integer
  ) : TPoint ;
  begin
    cursorState[ _cursor ].currRecord.CenterX := ReadLongint ;
    cursorState[ _cursor ].currRecord.CenterY := ReadLongint ;
    {$IFDEF GIS_NORECORDS}
      Result := new TPoint(0,0) ;
    {$ENDIF}
    Result.X := cursorState[ _cursor ].currRecord.CenterX ;
    Result.Y := cursorState[ _cursor ].currRecord.CenterY ;
  end ;

  function TGIS_FileTAB_MAP.ReadCenterFromBlock(
    const _cursor : Integer
  ) : TPoint ;
  var
    ipos    : Integer ;
    old_pos : Integer ;
  begin
    ipos := Position ;
    old_pos := ipos ;

    Position := ( (ipos-1) div 512) * 512 + 2 ;

    ReadWord ;  // no of bytes ;

    cursorState[ _cursor ].currRecord.CenterX := ReadLongint ;
    cursorState[ _cursor ].currRecord.CenterY := ReadLongint ;

    Position := old_pos ;
  end ;

  function TGIS_FileTAB_MAP.ReadExtent(
    const _compression : Boolean ;
    const _cursor : Integer
  ) : TGIS_Extent ;
  var
    ptg : TGIS_Point ;
  begin
    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Extent ;
    {$ENDIF}
    ptg := ReadCoordinate( _compression, _cursor ) ;
    Result.XMin := ptg.X ;
    Result.YMin := ptg.Y ;

    ptg := ReadCoordinate( _compression, _cursor ) ;
    Result.XMax := ptg.X ;
    Result.YMax := ptg.Y ;
  end ;

//==============================================================================
// TGIS_FileTAB_ID
//==============================================================================

  constructor TGIS_FileTAB_ID.Create(
    const _path  : String  ;
    const _write : Boolean
  ) ;
  begin
    inherited Create( _path, getStreamMode( _write ) ) ;

    {$IFDEF JAVA}
      FRecordsCount := Size div sizeOf( Integer ) ;
    {$ELSE}
      FRecordsCount := Size div sizeOf( TGIS_RecordTAB_ID ) ;
    {$ENDIF}
  end ;

  procedure TGIS_FileTAB_ID.ReadHeader ;
  begin
  end ;

  procedure TGIS_FileTAB_ID.CursorOpen(
    const _cursor : Integer
  ) ;
  begin
    if _cursor >= {$IFDEF OXYGENE}RemObjects.Elements.System.{$ENDIF}length( cursorState )  then
      {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( cursorState, _cursor + 1 ) ;
    {$IFDEF GIS_NORECORDS}
      if not assigned( cursorState[_cursor] ) then
        cursorState[_cursor] := new T_cursorStateTAB_ID ;
      cursorState[ _cursor ].currRecord := new TGIS_RecordTAB_ID ;
    {$ENDIF}

    cursorState[ _cursor ].curInUse := True ;
    ReadHeader ;
  end ;

  procedure TGIS_FileTAB_ID.CursorClose(
    const _cursor : Integer
  ) ;
  var
    i : Integer ;
  begin
    cursorState[_cursor].curInUse := False ;

    // truncate cursorState at the tail;
    {$IFNDEF OXYGENE}
      for i := Length( cursorState ) - 1 downto 0 do begin
    {$ELSE}
      for i := cursorState.Length - 1 downto 0 do begin
    {$ENDIF}
      if not cursorState[i].curInUse then begin
        {$IFDEF OXYGENE}SystemUnit.{$ENDIF}SetLength( cursorState, i ) ;
      end
      else
        break ;
    end ;
  end ;

  {$IFDEF OXYGENE}
    function TGIS_FileTAB_ID.ReadRecord(
      const _uid     : TGIS_Uid ;
      const _cursor  : Integer
    ) : TGIS_RecordTAB_ID ;
  {$ELSE}
    function TGIS_FileTAB_ID.ReadRecord(
      const _uid    : TGIS_Uid ;
      const _cursor : Integer
    ) : PGIS_RecordTAB_ID ;
  {$ENDIF}
  begin
    assert( _uid > 0 ) ;
    {$IFDEF JAVA}
      Position := (_uid-1) * sizeOf( Integer ) ;
    {$ELSE}
      Position := (_uid-1) * sizeOf( TGIS_RecordTAB_ID ) ;
    {$ENDIF}
    if _uid <= RecordsCount then begin
      {$IFDEF OXYGENE}
      if ReadInteger( cursorState[ _cursor ].currRecord.Offset,
               sizeOf( Integer )
             ) <>  sizeOf( Integer )
      {$ELSE}
      if Read( cursorState[ _cursor ].currRecord.Offset,
               sizeOf( Integer )
             ) <>  sizeOf( Integer )
      {$ENDIF}
      then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), Path, 0 ) ;
    end
    else
      cursorState[ _cursor ].currRecord.Offset := 0 ;

    {$IFDEF OXYGENE}
      Result := cursorState[ _cursor ].currRecord ;
    {$ELSE}
      Result := @cursorState[ _cursor ].currRecord ;
    {$ENDIF}
  end ;

//==================================== END =====================================
end.

