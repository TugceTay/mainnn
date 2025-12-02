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
  Encapsulation of a DGN file access.

  This unit was inspired by dgnlib:

  Author:   Frank Warmerdam, warmerdam@pobox.com

  Copyright (c) 2001, Avenza Systems Inc, http://www.avenza.com

  Permission is hereby granted, free of charge, to any person obtaining a
  copy of this software and associated documentation files (the "Software"),
  to deal in the Software without restriction, including without limitation
  the rights to use, copy, modify, merge, publish, distribute, sublicense,
  and/or sell copies of the Software, and to permit persons to whom the
  Software is furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included
  in all copies or substantial portions of the Software.

  Sublicensing of this unit is a subject of TatukGIS Developer Kernel License.
}

{$IFDEF DCC}
  unit GisLayerDGN ;
  {$HPPEMIT '#pragma link "GisLayerDGN"'}
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

{$IFDEF CLR}
  uses
    System.Drawing,
    System.Runtime.InteropServices.ComTypes,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,
    System.Math,
    System.Generics.Collections,
    GisTypes,
    GisTypesUI,
    GisStreams,
    GisLayerVector ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    tatukgis.rtl ;
{$ENDIF}

type

  {#gendoc:hide}
  // Initialization section handler
  Unit_GisLayerDGN = class
    public
      class procedure SelfRegisterLayer() ;
  end;

  {$IFDEF OXYGENE}
    T_cursorDgn nested in TGIS_LayerDGN = record
      public
        /// <summary>
        ///   Is cursor in use.
        /// </summary>
        curInUse : Boolean ;
        /// <summary>
        ///   Current shape. Layer access is based on record-by-record access.
        /// </summary>
        currShape : TGIS_Shape ;
        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currPoint : TGIS_ShapePoint ;
        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currArc : TGIS_ShapeArc ;
        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currPolygon : TGIS_ShapePolygon ;
        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currMultipoint : TGIS_ShapeMultiPoint  ;
        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currMultiShape : TGIS_Shape ;
        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///  faster than full Create constructor.
        /// </summary>
        currMultiLine : TGIS_ShapeArc ;
        /// <summary>
        ///   Array of bookmarks to block of 512 shapes. Used for fast GetShape.
        /// </summary>
        arBookmarkPos : array of Integer ;
        /// <summary>
        ///   Array of bookmarks to block of 512 shapes. Used for fast GetShape.
        /// </summary>
        arBookmarkUid : array of Integer ;
    end ;

    T_tabLevels nested in TGIS_LayerDGN = record
      public
          levelId         : Cardinal ;
          parentId        : Cardinal ;
          version         : Word ;
          flags           : Word ;
          externalId      : Integer ;
          levelStyle      : Integer ;
          levelWeight     : Cardinal ;
          levelColor      : Cardinal ;
          levelFillColor  : Cardinal ;
          elmStyle        : Integer ;
          elmWeight       : Cardinal ;
          elmColor        : Cardinal ;
          elmFillColor    : Cardinal ;
          flagIsHidden    : Boolean ;
          levelName       : String ;
          levelDescription: String ;
      end ;

  {$ENDIF}

  /// <summary>
  ///   Layer that can read DGN file.
  /// </summary>
  TGIS_LayerDGN = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private
      /// <summary>
      ///   DGN file.
      /// </summary>
      dgnFile : TGIS_BufferedFileStream ;
    {$IFDEF OXYGENE}
        /// <summary>
        ///   Cursor.
        /// </summary>
        cursorDgn : array of T_cursorDgn ;
    {$ELSE}
        /// <summary>
        ///   Cursor.
        /// </summary>
        cursorDgn : array of record
          /// <summary>
          ///   Is cursor in use.
          /// </summary>
          curInUse : Boolean ;
          /// <summary>
          ///   Current shape. Layer access is based on record-by-record access.
          /// </summary>
          currShape : TGIS_Shape ;
          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster than full Create constructor.
          /// </summary>
          currPoint : TGIS_ShapePoint ;
          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster than full Create constructor.
          /// </summary>
          currArc : TGIS_ShapeArc ;
          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster than full Create constructor.
          /// </summary>
          currPolygon : TGIS_ShapePolygon ;
          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster than full Create constructor.
          /// </summary>
          currMultipoint : TGIS_ShapeMultiPoint  ;
          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster than full Create constructor.
          /// </summary>
          currMultiShape : TGIS_Shape ;
          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///  faster than full Create constructor.
          /// </summary>
          currMultiLine : TGIS_ShapeArc ;
          /// <summary>
          ///   Array of bookmarks to block of 512 shapes. Used for fast GetShape.
          /// </summary>
          arBookmarkPos : array of Integer ;
          /// <summary>
          ///   Array of bookmarks to block of 512 shapes. Used for fast GetShape.
          /// </summary>
          arBookmarkUid : array of Integer ;
        end ;
      {$ENDIF}

      /// <summary>
      ///   Subunits per master unit.
      /// </summary>
      dgnSubunitsPerMaster : Integer ;

      /// <summary>
      ///   User name for master units.
      /// </summary>
      dgnMasterUnits : String ;

      /// <summary>
      ///   User name for subunits.
      /// </summary>
      dgnSubUnits : String ;

      /// <summary>
      ///   UOR per subunit.
      /// </summary>
      dgnUorPerSubunit : Integer ;

      /// <summary>
      ///   UOR's global origin.
      /// </summary>
      dgnGlobalOrigin : TGIS_Point3D ;

      /// <summary>
      ///   True if dgn is 3 dimensional.
      /// </summary>
      dgn3d : Boolean ;

      /// <summary>
      ///   True if file is cell type.
      /// </summary>
      isCellFile : Boolean ;

      /// <summary>
      ///   Scale factor.
      /// </summary>
      dgnScale : Double ;

      /// <summary>
      ///   Table of colors.
      /// </summary>
      dgnColorTable : array[0..767] of Byte ;

      /// <summary>
      ///   Number of attribute entry.
      /// </summary>
      dgnAttrCount : Integer ;

      /// <summary>
      ///   Number of complex elements.
      /// </summary>
      dgnComplexCount : Integer ;

      /// <summary>
      ///   True if complex element found.
      /// </summary>
      dgnComplexFound : Boolean ;

      /// <summary>
      ///   True if complex element found.
      /// </summary>
      dgnComplexType : Integer ;

      /// <summary>
      ///   True if complex element found.
      /// </summary>
      dgnComplexUsed : Boolean ;

      /// <summary>
      ///   Element header properties.
      /// </summary>
      elmSignature    : Cardinal ;
      elmType         : Word ;
      elmFlags        : Word ;
      elmSize         : Cardinal ;
      elmAttrOffset   : Cardinal ;
      elmLevel        : Cardinal ;

      /// <summary>
      ///   Element graphics properties.
      /// </summary>
      elmGraphicGroup : Cardinal ;
      elmPriority     : Cardinal ;
      elmProperties   : Word ;
      elmGraphStyle   : Integer ;
      elmGraphWeight  : Cardinal ;
      elmGraphColor   : Cardinal ;
      elmProperty3D   : Boolean ;

      /// <summary>
      ///   Core element data.
      /// </summary>
      elmBuffer : array of Byte ;

      /// <summary>
      ///   Current offset in core element data.
      /// </summary>
      elmBufferPos : Cardinal ;

      /// <summary>
      ///   Complex element Level: 0-63.
      /// </summary>
      elmMSLevel  : Integer ;

      /// <summary>
      ///   Is element complex?
      /// </summary>
      elmComplex : Boolean ;

      /// <summary>
      ///   size of complex elements.
      /// </summary>
      elmComplexSize : Integer ;

      /// <summary>
      ///   size of complex element attributes.
      /// </summary>
      elmComplexAttrCount : Integer ;

      /// <summary>
      ///   Is element deleted?
      /// </summary>
      elmDeleted : Boolean ;

      /// <summary>
      ///   Is Graphic element?
      /// </summary>
      elmFlagGraphic : Boolean ;

      /// <summary>
      ///   Color index (0-255).
      /// </summary>
      elmColor : Integer ;

      /// <summary>
      ///   Color index for fills (0-255).
      /// </summary>
      elmFillColor : Integer ;

      hasFillColor : Boolean ;

      /// <summary>
      ///   Line Weight (0-31).
      /// </summary>
      elmWeight : Integer ;

      /// <summary>
      ///   Line Style (DGNS_xxx).
      /// </summary>
      elmStyle : Integer ;

      /// <summary>
      ///   Bytes of attribute data, usually zero.
      /// </summary>
      elmAttrSize : Cardinal ;

      /// <summary>
      ///   Offset of attributes data.
      /// </summary>
      elmAttrPos : Integer ;

      /// <summary>
      ///   Element class.
      /// </summary>
      elmClass : Integer;

      /// <summary>
      ///   Is element invisible.
      /// </summary>
      elmInvisible : Boolean ;

      /// <summary>
      ///   Is element a hole.
      /// </summary>
      elmHole : Boolean ;

      /// <summary>
      ///   Count of entity attributes.
      /// </summary>
      elmAttrCount : Integer ;

      /// <summary>
      ///   Entitynum value of attribute linkage.
      /// </summary>
      elmAttrEntity   : array [0..255] of Cardinal ;

      /// <summary>
      ///   MSLink value of attribute linkage.
      /// </summary>
      elmAttrMSLink   : array [0..255] of Cardinal ;

      /// <summary>
      ///   Entitynum value of complex attribute linkage.
      /// </summary>
      elmMSAttrEntity : array [0..255] of Cardinal ;

      /// <summary>
      ///   MSLink value of complex attribute linkage.
      /// </summary>
      elmMSAttrMSLink : array [0..255] of Cardinal ;

      /// <summary>
      ///   Origin of cell object.
      /// </summary>
      cellOrigin : TGIS_Point3D ;

      /// <summary>
      ///   True, if cell is active.
      /// </summary>
      cellActive : Boolean ;

      /// <summary>
      ///   Bookmark Position for cell.
      /// </summary>
      cellBookmarkPos : Integer ;

      /// <summary>
      ///   Bookmark uid for cell.
      /// </summary>
      cellBookmarkUid : Integer ;

      /// <summary>
      ///   Last UID in a shape file. If not set then -1.
      /// </summary>
      lastUid : TGIS_Uid ;

      /// <summary>
      ///   True, if Eof was reached.
      /// </summary>
      isFileEof : Boolean ;

      /// <summary>
      ///   Uid of last read element.
      /// </summary>
      readUid : Integer ;
      quat        : array [0..3] of Double;

      /// <summary>
      ///   V7 Flag.
      /// </summary>
      blV7 : Boolean;

      elmStringNameLinkage : String ;
      elmStringDescLinkage : String ;

      {$IFDEF OXYGENE}
        currStream        : TGIS_MemoryStream ;
      {$ELSE}

        /// <summary>
        ///   Current stream.
        /// </summary>
        currStream        : TStream ;
      {$ENDIF}

      /// <summary>
      ///   Current stream old Position.
      /// </summary>
      oldPos              : Int64;
      rotAngle            : Double  ;
      dgnCellActive       : Boolean ;

      /// <summary>
      ///   Global units and scale.
      /// </summary>
      dgnUORPerStorageUnit    : Double ;
      dgnUORPerStorageSubUnit : Double ;

      /// <summary>
      ///   Element justification.
      /// </summary>
      just                : Word;
      ignoreGraph         : Boolean ;

      /// <summary>
      ///   Complex elements count.
      /// </summary>
      cplxComponentsCnt   : Integer ;

      /// <summary>
      ///   Complex elements attr.
      /// </summary>
      cplxComponentsFillColor : TGIS_Color ;
      cplxComponentsOutColor  : TGIS_Color ;

      /// <summary>
      ///   Complex elements attr.
      /// </summary>
      cplxComponentsType  : TGIS_ShapeType ;

      /// <summary>
      ///   Cell elements count.
      /// </summary>
      cplxCellCnt         : Integer ;

      /// <summary>
      ///   Complex elements list.
      /// </summary>
      cplxList            : TGIS_ShapeList ;

      {$IFDEF OXYGENE}
        tabLevels             : array of T_tabLevels ;
      {$ELSE}
        tabLevels             : array of record
                                  levelId         : Cardinal ;
                                  parentId        : Cardinal ;
                                  version         : Word ;
                                  flags           : Word ;
                                  externalId      : Integer ;
                                  levelStyle      : Integer ;
                                  levelWeight     : Cardinal ;
                                  levelColor      : Cardinal ;
                                  levelFillColor  : Cardinal ;
                                  elmStyle        : Integer ;
                                  elmWeight       : Cardinal ;
                                  elmColor        : Cardinal ;
                                  elmFillColor    : Cardinal ;
                                  flagIsHidden    : Boolean ;
                                  levelName       : String ;
                                  levelDescription: String ;
                                end;
      {$ENDIF}
      dcLevels : TDictionary<Integer, Integer> ;
    private

      /// <summary>
      ///   Fetch element form disk.
      /// </summary>
      function fetchElement : Boolean ;

      /// <summary>
      ///   Convert index to color.
      /// </summary>
      /// <param name="_idx">
      ///   color index
      /// </param>
      /// <returns>
      ///   converted color
      /// </returns>
      function getColor ( const _idx : Integer ) : TGIS_Color ;

      /// <summary>
      ///   Check neighborhood color with background color of viewer.
      /// </summary>
      /// <param name="_idx">
      ///   color index
      /// </param>
      /// <param name="_tol">
      ///   tolerance
      /// </param>
      function isColorNearBackground( const _idx : Integer ;
                                      const _tol : Integer
                                    ) : Boolean ;

      /// <summary>
      ///   Convert line weight value to a weight in twips
      /// </summary>
      /// <param name="_weight">
      ///   line weight
      /// </param>
      function getWeight( const _weight: Integer ) : Integer ;

      /// <summary>
      ///   Convert index to line style.
      /// </summary>
      /// <param name="_idx">
      ///   line style index
      /// </param>
      /// <returns>
      ///   converted line style
      /// </returns>
      function getStyle ( const _idx : Integer ) : TGIS_PenStyle ;

      /// <summary>
      ///   Convert index to label Position.
      /// </summary>
      /// <param name="_idx">
      ///   label Position index
      /// </param>
      /// <returns>
      ///   converted label Position
      /// </returns>
      function getPosition (const _idx : Integer) : TGIS_LabelPosition ;

      /// <summary>
      ///   Read byte value from element.
      /// </summary>
      function readByte : Byte ;

      /// <summary>
      ///   Read String value from element.
      /// </summary>
      /// <param name="_cnt">
      ///   number of character to be read
      /// </param>
      function readString ( const _cnt : Integer ) : String ;

      /// <summary>
      ///   Read word value from element.
      /// </summary>
      function readWord : Word ;

      /// <summary>
      ///   Read integer value from element.
      /// </summary>
      function readInteger : Integer ;

      /// <summary>
      ///   Read double value from element.
      /// </summary>
      function readDouble : Double ;

      /// <summary>
      ///   Read X coordinate from element.
      /// </summary>
      function readX : Double ;

      /// <summary>
      ///   Read Y coordinate from element.
      /// </summary>
      function readY : Double ;

      /// <summary>
      ///   Read Z coordinate. If ISFF is 2d, then function is a dummy.
      /// </summary>
      function readZ : Double ;

      /// <summary>
      ///   Convert quaternion to angle.
      /// </summary>
      /// <param name="_isInt">
      ///   is matrix of integer type
      /// </param>
      /// <param name="_angle">
      ///   converted value
      /// </param>
      procedure convertQueternion( const _isInt : Boolean ;
                                     var _angle : Double
                                  ) ;

      /// <summary>
      ///   Read X coordinate from element extent
      /// </summary>
      function readXExt : Double ; overload;

      /// <summary>
      ///   Read X coordinate from element extent
      /// </summary>
      /// <param name="Extent">
      ///   if extent is true then sign bit is set.
      /// </param>
      function readXExt ( const Extent : Boolean
                        ) : Double ; overload;

      /// <summary>
      ///   Read Y coordinate from element extent
      /// </summary>
      function readYExt : Double ; overload;

      /// <summary>
      ///   Read Y coordinate from element extent
      /// </summary>
      /// <param name="Extent">
      ///   if extent is true then sign bit is set.
      /// </param>
      function readYExt ( const Extent : Boolean
                        ) : Double ; overload;

      /// <summary>
      ///   Read Y coordinate from element extent
      /// </summary>
      function readZExt : Double ; overload;

      /// <summary>
      ///   Read Y coordinate from element extent
      /// </summary>
      /// <param name="Extent">
      ///   if extent is true then sign bit is set.
      /// </param>
      function readZExt ( const Extent : Boolean
                        ) : Double ; overload;

      /// <summary>
      ///   Read a next DGN element from file.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor index
      /// </param>
      function readShape( const _cursor : Integer ) : Boolean ;

      /// <summary>
      ///   Read core attributes from element.
      /// </summary>
      procedure readCore ;

      /// <summary>
      ///   Parse current stream and build shapes.
      /// </summary>
      procedure parseStream ;

      /// <summary>
      ///   Read element header.
      /// </summary>
      procedure readHeader;

      /// <summary>
      ///   Parse element header.
      /// </summary>
      procedure parseHeader;

      /// <summary>
      ///   Read attribute linkages.
      /// </summary>
      procedure readAttributeLinkages;

      /// <summary>
      ///   Read root model header.
      /// </summary>
      procedure readRootModelHeader;

      /// <summary>
      ///   Prepare current shape and its attributes.
      /// </summary>
      /// <param name="_type">
      ///   shape type
      /// </param>
      procedure prepareShape( const _type : TGIS_ShapeType ) ;

      /// <summary>
      ///   Unprepare current shape.
      /// </summary>
      procedure unprepareShape;

      function findLevelIdx( const _level : Integer ) : Integer ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      // property access routines ;

         function fget_PathRTree : String ; override;
      // for internal use of TGIS_Viewer

         /// <inheritdoc/>
         procedure setUp     ; override;

      // cursor access function(s)

         /// <inheritdoc/>
         function  cursorOpen :  Integer ; override;

         /// <inheritdoc/>
         procedure cursorClose( const _cursor      : Integer
                               ) ; override;

         /// <inheritdoc/>
         procedure cursorFirst( const _cursor      : Integer          ;
                                const _viewerCS    : Boolean          ;
                                const _extent      : TGIS_Extent      ;
                                const _query       : String           ;
                                const _shape       : TGIS_Shape       ;
                                const _de9im       : String           ;
                                const _skipDeleted : Boolean
                              ) ; override;

         /// <inheritdoc/>
         procedure cursorNext ( const _cursor      : Integer
                              ) ; override;

         /// <inheritdoc/>
         function  cursorEof  ( const _cursor      : Integer
                              ) : Boolean ; override;

         /// <inheritdoc/>
         function  cursorShape( const _cursor      : Integer
                              ) : TGIS_Shape ; override;
    protected
      // destructor

         /// <summary>
         ///   Destroy layer instance.
         /// </summary>
         procedure doDestroy ; override;
    public
      // constructors

         /// <inheritdoc/>
         constructor Create  ; override;

         /// <inheritdoc/>
         procedure RecalcExtent   ; override;

         /// <inheritdoc/>
         function  GetShape  ( const _uid    : TGIS_Uid ;
                               const _cursor : Integer
                              ) : TGIS_Shape ; override;

         /// <inheritdoc/>
         function  GetLastUid: TGIS_Uid ; override;

         /// <inheritdoc/>
         function  GetNewUid : TGIS_Uid ; override;

         /// <inheritdoc/>
         procedure RevertShapes ; override;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    {$IFDEF MSWINDOWS}
      System.Win.ComObj,
      Winapi.Windows,
      Winapi.ActiveX,
    {$ENDIF}
    System.SyncObjs,
    GisRtl,
    GisFunctions,
    GisInternals,
    GisClasses,
    GisResource,
    GisLayer,
    GisLayerSublayer,
    GisInterfaces,
    GisParams,
    GisTopology,
    {$IFNDEF NEXTGEN}
      GisCompression,
    {$ENDIF}
    GisRegistredLayers ;
{$ENDIF}


  {$IFDEF OXYGENE}
  {$ELSE}
  type
    { Used to keep VAX Double numbers}
    T_IntAndDouble = record
      case Integer of
        0 : (Lo,Hi   :  LongWord) ;
        1 : (Real64  : double) ;
        2 : (ByteArr : array[0..7] of Byte) ;
    end;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  type
  { Storage class declaration. }
  T_Storage = class
    Storage : IStorage ;
  end;

  { Storage file management class declaration. }
  T_StorageFile = class( TGIS_ObjectDisposable )
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FPath   : String      ;
      FRoot   : T_Storage   ;
      FGStg   : T_Storage   ;
      FNStg   : T_Storage   ;
      FStgLst : TGIS_ObjectList ;
    protected
      // Destroy an object.
      procedure doDestroy ; override;
    public
      // Create an object.
      constructor Create ;

      // <summary>
      //   Open storage file.
      // </summary>
      // <param name="_fileName">
      //   name of file to open
      // </param>
      // <exception cref="EGIS_Exception">
      //   GIS_RS_ERR_FILEBADFORMAT
      // </exception>
      procedure OpenStorageFile       ( const _fileName : String
                                      ) ;
      // <summary>
      //   Enumerate elements from storage file.
      // </summary>
      // <param name="_elemList">
      //   list to fill with storage elements
      // </param>
      // <param name="_root">
      //   storage containing elements
      // </param>
      // <exception cref="EGIS_Exception">
      //   GIS_RS_ERR_FILEBADFORMAT
      // </exception>
      procedure EnumElements          ( const _elemList : TStrings ;
                                        const _root     : T_Storage
                                      ) ;
      // <summary>
      //   Open storage.
      // </summary>
      // <param name="_name">
      //   storage name
      // </param>
      // <param name="_root">
      //   storage containing storage
      // </param>
      // <returns>
      //   storage
      // </returns>
      function  OpenStorage           ( const _name     : String ;
                                        const _root     : T_Storage
                                      ) : T_Storage ;
      // <summary>
      //   Open stream.
      // </summary>
      // <param name="_name">
      //   stream name
      // </param>
      // <param name="_root">
      //   storage containing stream
      // </param>
      // <returns>
      //   stream
      // </returns>
      function  OpenStream            ( const _name     : String ;
                                        const _root     : T_Storage
                                      ) : TStream ;
      // <summary>
      //   Decompress stream using deflate algorithm.
      // </summary>
      // <param name="_source">
      //   stream to decompress
      // </param>
      // <returns>
      //   decompressed stream
      // </returns>
      function  DecompressStream      ( const _source   : TStream
                                      ) : TStream ;
      // <summary>
      //   Read model root storage.
      // </summary>
      // <returns>
      //   list of elements
      // </returns>
      function  ReadModelRootStorage  : TStrings ;
      // <summary>
      //   Read root model header.
      // </summary>
      // <returns>
      //   list of header elements
      // </returns>
      function  ReadRootModelHeader   : TStream ;
      // <summary>
      //   Read non model header.
      // </summary>
      // <returns>
      //   list of header elements
      // </returns>
      function  ReadNonModelHeader    : TStrings ;
  end ;
  {$ENDIF}

type

  T_MLSymbology = record
    weight      : Integer ;
    use_weight  : Boolean ;
    style       : Integer ;
    use_style   : Boolean ;
    color       : Integer ;
    use_color   : Boolean ;
    level       : Integer ;
    dist        : Double ;
  end ;

const
   // VAX double flags
     MSB                                = $80000000 ;
     LSB                                = $00000001 ;
     LS20B                              = $000FFFFF ;
     LS3B                               = $00000007 ;
     LS8B                               = $000000FF ;
     LS29B                              = $1FFFFFFF ;

   // ISFF DGN Header Format IDS
     GIS_DGN_HEADER_IDX0                = $08 ;
     GIS_DGN_HEADER_IDX1                = $09 ;
     GIS_DGN_HEADER_IDX2                = $fe ;
     GIS_DGN_HEADER_IDX3                = $02 ;

   // ISFF Cell Header IDS
     GIS_CELL_HEADER_IDX0               = $08 ;
     GIS_CELL_HEADER_IDX1               = $05 ;
     GIS_CELL_HEADER_IDX2               = $17 ;
     GIS_CELL_HEADER_IDX3               = $00 ;

   // data types
     GIS_DGNTT_BYTE                     = 1 ;
     GIS_DGNTT_WORD                     = 2 ;
     GIS_DGNTT_INTEGER                  = 4 ;
     GIS_DGNTT_DOUBLE                   = 8 ;

  // Element types
     GIS_DGNT_CELL_LIBRARY              =   1 ; // Cell Library Header
     GIS_DGNT_CELL_HEADER               =   2 ; // Cell (complex)
     GIS_DGNT_LINE                      =   3 ; // Line
     GIS_DGNT_LINE_STRING               =   4 ; // Line String
     GIS_DGNT_GROUP_DATA                =   5 ; // Group Data
     GIS_DGNT_SHAPE                     =   6 ; // Shape
     GIS_DGNT_TEXT_NODE                 =   7 ; // Text Node (complex)
     GIS_DGNT_DIGITIZER_SETUP           =   8 ; // Digitizer Setup Data
     GIS_DGNT_TCB                       =   9 ; // Design File Header
     GIS_DGNT_LEVEL_SYMBOLOGY           =  10 ; // Level Symbology
     GIS_DGNT_CURVE                     =  11 ; // Curve
     GIS_DGNT_COMPLEX_CHAIN_HEADER      =  12 ; // Complex String (complex)
     GIS_DGNT_CONIC                     =  13 ; // Conic
     GIS_DGNT_COMPLEX_SHAPE_HEADER      =  14 ; // Complex Shape (complex)
     GIS_DGNT_ELLIPSE                   =  15 ; // Ellipse
     GIS_DGNT_ARC                       =  16 ; // Arc
     GIS_DGNT_TEXT                      =  17 ; // Text
     GIS_DGNT_3DSURFACE_HEADER          =  18 ; // Surface (complex)
     GIS_DGNT_3DSOLID_HEADER            =  19 ; // Solid (complex)
     GIS_DGNT_BSPLINE                   =  21 ; // B-Spline Pole
     GIS_DGNT_POINT_STRING              =  22 ; // Point String
     GIS_DGNT_CONE                      =  23 ; // Circular Truncated Cone
     GIS_DGNT_BSPLINE_SURFACE           =  24 ; // B-Spline Surface (complex)
     GIS_DGNT_BSURF_BOUNDARY            =  25 ; // B-Spline Surface boundary
     GIS_DGNT_BSPLINE_KNOT              =  26 ; // B-Spline Knot Element
     GIS_DGNT_BSPLINE_HEADER            =  27 ; // B-Spline Curve (complex)
     GIS_DGNT_BSPLINE_WEIGHT            =  28 ; // B-Spline Weight Factor
     GIS_DGNT_DIMENSION                 =  33 ; // Dimension Element
     GIS_DGNT_SHARED_CELL_DEFN          =  34 ; // Shared Cell Definition Element
     GIS_DGNT_SHARED_CELL_ELEM          =  35 ; // Shared Cell Element
     GIS_DGNT_MULTI_LINE                =  36 ; // Multiline Element
     GIS_DGNT_ATTRIBUTE                 =  37 ; // Attribute Element
     GIS_DGNT_DGNSTORE_COMP             =  38 ; // DgnStore Component
     GIS_DGNT_DGNSTORE_HDR              =  39 ; // DgnStore Header
     GIS_DGNT_TYPE44_ELM                =  44 ;
     GIS_DGNT_APPLICATION_ELEM          =  66 ; // MicroStation Application
     GIS_DGNT_RASTER_HDR                =  87 ; // Raster Header
     GIS_DGNT_RASTER_COMP               =  88 ; // Raster Component
     GIS_DGNT_RASTER_REFERENCE_ELM      =  90 ; // Raster Reference Attachment
     GIS_DGNT_RASTER_REFERENCE_COMP     =  91 ; // Raster Reference Component
     GIS_DGNT_RASTER_HIERARCHY_ELM      =  92 ; // Raster Hierarchy Element
     GIS_DGNT_RASTER_HIERARCHY_COMP     =  93 ; // Raster Hierarchy Component
     GIS_DGNT_RASTER_FRAME_ELEMENT      =  94 ; // Raster Frame Element
     GIS_DGNT_TABLE_ENTRY               =  95 ; // Table Entry Element
     GIS_DGNT_TABLE                     =  96 ; // Table Header Element
     GIS_DGNT_VIEW_GROUP                =  97 ; // View Group Element
     GIS_DGNT_VIEW                      =  98 ; // View Element
     GIS_DGNT_LEVEL_MASK                =  99 ; // Level Mask Element
     GIS_DGNT_REFERENCE_ATTACH          = 100 ; // Reference Attach Element

     GIS_DGNT_MATRIX_HEADER             = 101 ; // Matrix Header
     GIS_DGNT_MATRIX_INT_DATA           = 102 ; // Matrix Int Data
     GIS_DGNT_MATRIX_DOUBLE_DATA        = 103 ; // Matrix Double Data
     GIS_DGNT_MESH_HEADER               = 105 ; // Mesh Header

     GIS_DGNT_REFERENCE_OVERRIDE_ELM    = 108 ; // Reference Override Element

     GIS_DGNT_NAMED_GROUP_HDR_ELM       = 110 ; // Named Group Header
     GIS_DGNT_NAMED_GROUP_COMPONENT_ELM = 111 ; // Named Group Component

  // Line Styles
     GIS_DGNS_SOLID                     =  0 ;
     GIS_DGNS_DOTTED                    =  1 ;
     GIS_DGNS_MEDIUM_DASH               =  2 ;
     GIS_DGNS_LONG_DASH                 =  3 ;
     GIS_DGNS_DOT_DASH                  =  4 ;
     GIS_DGNS_SHORT_DASH                =  5 ;
     GIS_DGNS_DASH_DOUBLE_DOT           =  6 ;
     GIS_DGNS_LONG_DASH_SHORT_DASH      =  7 ;

  // 3D Surface Types
     GIS_DGNSUT_SOLID                    = 0 ;
     GIS_DGNSUT_BOUNDED_PLANE            = 1 ;
     GIS_DGNSUT_BOUNDED_PLANE2           = 2 ;
     GIS_DGNSUT_RIGHT_CIRCULAR_CYLINDER  = 3 ;
     GIS_DGNSUT_RIGHT_CIRCULAR_CONE      = 4 ;
     GIS_DGNSUT_TABULATED_CYLINDER       = 5 ;
     GIS_DGNSUT_TABULATED_CONE           = 6 ;
     GIS_DGNSUT_CONVOLUTE                = 7 ;
     GIS_DGNSUT_SURFACE_OF_REVOLUTION    = 8 ;
     GIS_DGNSUT_WARPED_SURFACE           = 9 ;

  // 3D Solid Types
     GIS_DGNSOT_VOLUME_OF_PROJECTION     = 0 ;
     GIS_DGNSOT_VOLUME_OF_REVOLUTION     = 1 ;
     GIS_DGNSOT_BOUNDED_VOLUME           = 2 ;

  // Class
     GIS_DGNC_PRIMARY                   =  0 ;
     GIS_DGNC_PATTERN_COMPONENT         =  1 ;
     GIS_DGNC_CONSTRUCTION_ELEMENT      =  2 ;
     GIS_DGNC_DIMENSION_ELEMENT         =  3 ;
     GIS_DGNC_PRIMARY_RULE_ELEMENT      =  4 ;
     GIS_DGNC_LINEAR_PATTERNED_ELEMENT  =  5 ;
     GIS_DGNC_CONSTRUCTION_RULE_ELEMENT =  6 ;

  // Group Data level numbers.
  // These are symbolic values for the typ 5 (DGNT_GROUP_DATA)
  // level values that have special meanings.
     GIS_DGN_GDL_COLOR_TABLE            =  1 ;
     GIS_DGN_GDL_FONT_TABLE             =  2 ;
     GIS_DGN_GDL_NAMED_VIEW             =  3 ;
     GIS_DGN_GDL_REF_FILE               =  9 ;

  // some level values for GIS_DGNT_APPLICATION_ELEM
     GIS_DGN_AEL_DATA_STYLE             = 23 ;

  // Attribute 3D flag
     GIS_DGN_3D_HEADER_ID               = $C8 ;

  // Word 17 property flags.
     GIS_DGNPF_HOLE                     =  $8000 ;
     GIS_DGNPF_SNAPPABLE                =  $4000 ;
     GIS_DGNPF_PLANAR                   =  $2000 ;
     GIS_DGNPF_ORIENTATION              =  $1000 ;
     GIS_DGNPF_ATTRIBUTES               =  $0800 ;
     GIS_DGNPF_3D                       =  $0800 ;
     GIS_DGNPF_MODIFIED                 =  $0400 ;
     GIS_DGNPF_NEW                      =  $0200 ;
     GIS_DGNPF_LOCKED                   =  $0100 ;
     GIS_DGNPF_INVISIBLE                =  $0080 ;
     GIS_DGNPF_CLASS                    =  $000f ;

     GIS_DGN_SYMBOLOGY_BYLEVEL_COLOR    = $ffffffff ;
     GIS_DGN_SYMBOLOGY_BYCELL_COLOR     = $fffffffe ;
     GIS_DGN_SYMBOLOGY_BYLEVEL_WEIGHT   = $ffffffff ;
     GIS_DGN_SYMBOLOGY_BYCELL_WEIGHT    = $fffffffe ;
     GIS_DGN_SYMBOLOGY_BYLEVEL_STYLE    = $7fffffff ;
     GIS_DGN_SYMBOLOGY_BYCELL_STYLE     = $7ffffffe ;

  // DGNElementInfo flag values.
     GIS_DGNEIF_DELETED                 =  $01 ;
     GIS_DGNEIF_COMPLEX                 =  $02 ;

  // Justifications
     GIS_DGNJ_LEFT_TOP                  =  0 ;
     GIS_DGNJ_LEFT_CENTER               =  1 ;
     GIS_DGNJ_LEFT_BOTTOM               =  2 ;
     GIS_DGNJ_LEFTMARGIN_TOP            =  3 ;   // text node header only
     GIS_DGNJ_LEFTMARGIN_CENTER         =  4 ;   // text node header only
     GIS_DGNJ_LEFTMARGIN_BOTTOM         =  5 ;   // text node header only
     GIS_DGNJ_CENTER_TOP                =  6 ;
     GIS_DGNJ_CENTER_CENTER             =  7 ;
     GIS_DGNJ_CENTER_BOTTOM             =  8 ;
     GIS_DGNJ_RIGHTMARGIN_TOP           =  9 ;   // text node header only
     GIS_DGNJ_RIGHTMARGIN_CENTER        = 10 ;   // text node header only
     GIS_DGNJ_RIGHTMARGIN_BOTTOM        = 11 ;   // text node header only
     GIS_DGNJ_RIGHT_TOP                 = 12 ;
     GIS_DGNJ_RIGHT_CENTER              = 13 ;
     GIS_DGNJ_RIGHT_BOTTOM              = 14 ;

  // DGN file reading options.
     GIS_DGNO_CAPTURE_RAW_DATA          = $01 ;

  // Known attribute linkage types
     GIS_DGNLT_DMRS                     = $0000 ;
     GIS_DGNLT_INFORMIX                 = $3948 ; // Informix user data signature
     GIS_DGNLT_XBASE                    = $1971 ; // Xbase(dBASE) user data signature
     GIS_DGNLT_DBASE                    = $1971 ; // backwards compatibility to Xbase
     GIS_DGNLT_RIS                      = $71FB ; // RIS user data signature
     GIS_DGNLT_ORACLE                   = $6091 ; // Oracle user data signature
     GIS_DGNLT_INGRES                   = $3a77 ; // INGRES user data signature (ING)
     GIS_DGNLT_SYBASE                   = $4f58 ; // Sybase user data signature
     GIS_DGNLT_ODBC                     = $5e62 ; // ODBC user data signature (ODB)
     GIS_DGNLT_OLEDB                    = $5800 ; // OLEDB user data signature
     GIS_DGNLT_BUDBC                    = $5834 ; // BUDBC user data signature
     GIS_DGNLT_DMRS2                    = $8000 ;
     GIS_DGNLT_PRIMARY                  = $6091 ; // ID for Oracle linkage
     GIS_DGNLT_SECONDARY                = $f81  ; // Secondary ID link (BSI radix 50)

     GIS_DGNLT_SHAPE_FILL               = $0041 ;
     GIS_DGNLT_ASSOC_ID                 = $7D2F ;
     GIS_DGNLT_STRING                   = $56D2 ;
     GIS_DGNLT_LEVEL                    = $56e8 ;
     GIS_DGNLT_HATCH                    = $7d28 ;

  // Built-in fields
     GIS_DGN_FLD_LEVEL_ID               = 'DGN_LEVEL'       ;
     GIS_DGN_FLD_ELEVATION              = 'DGN_ELEVATION'   ;
     GIS_DGN_FLD_LABEL                  = 'DGN_LABEL'       ;
     GIS_DGN_FLD_LABEL_ANGLE            = 'DGN_LABEL_ANGLE' ;
     GIS_DGN_FLD_ENTITY                 = 'DGN_ENT_'        ;
     GIS_DGN_FLD_MSLINK                 = 'DGN_LNK_'        ;
     GIS_DGN_FLD_COLOR                  = 'DGN_COLOR'       ;
     GIS_DGN_FLD_STYLE                  = 'DGN_STYLE'       ;
     GIS_DGN_FLD_WEIGHT                 = 'DGN_WEIGHT'      ;
     GIS_DGN_FLD_TYPE                   = 'DGN_TYPE'        ;

  // other values
     GIS_BOOKMARK_BLOCKSIZE             = 512 ; // size of bookmarked block

     GIS_DGN_PATTERN_CONTROL            = 'Pattern Control Element' ;

    dgnDefaultColorTable : array [0..767] of Byte = (
      0, 0, 0, 255, 255, 255, 0, 0, 255, 0, 255, 0, 255, 0, 0, 255, 255, 0,
      255, 0, 255, 255, 127, 0, 0, 255, 255, 144, 144, 144, 255, 255, 255, 255,
      0, 0, 255, 64, 64, 204, 0, 0, 255, 64, 64, 153, 0, 0, 217, 64, 64, 102,
      0, 0, 166, 64, 64, 51, 0, 0, 115, 64, 64, 255, 63, 0, 255, 127, 64, 204,
      51, 0, 255, 115, 64, 153, 39, 0, 217, 103, 64, 102, 27, 0, 166, 91, 64,
      51, 15, 0, 115, 79, 64, 255, 126, 0, 255, 190, 64, 204, 101, 0, 255, 165,
      64, 153, 76, 0, 217, 140, 64, 102, 51, 0, 166, 115, 64, 51, 26, 0, 115,
      90, 64, 255, 189, 0, 255, 253, 64, 204, 152, 0, 255, 216, 64, 153, 115,
      0, 217, 179, 64, 102, 78, 0, 166, 142, 64, 51, 41, 0, 115, 105, 64, 255,
      255, 0, 255, 255, 64, 204, 204, 0, 255, 255, 64, 153, 153, 0, 217, 217,
      64, 102, 102, 0, 166, 166, 64, 51, 51, 0, 115, 115, 64, 192, 255, 0, 255,
      255, 64, 154, 204, 0, 218, 255, 64, 116, 153, 0, 180, 217, 64, 78, 102, 0,
      142, 166, 64, 40, 51, 0, 104, 115, 64, 129, 255, 0, 193, 255, 64, 104, 204,
      0, 168, 255, 64, 79, 153, 0, 143, 217, 64, 54, 102, 0, 118, 166, 64, 29,
      51, 0, 93, 115, 64, 66, 255, 0, 130, 255, 64, 53, 204, 0, 117, 255, 64, 40,
      153, 0, 104, 217, 64, 27, 102, 0, 91, 166, 64, 14, 51, 0, 78, 115, 64, 0,
      255, 0, 64, 255, 64, 0, 204, 0, 64, 255, 64, 0, 153, 0, 64, 217, 64, 0, 102,
      0, 64, 166, 64, 0, 51, 0, 64, 115, 64, 0, 255, 63, 64, 255, 127, 0, 204, 51,
      64, 255, 115, 0, 153, 39, 64, 217, 103, 0, 102, 27, 64, 166, 91, 0, 51, 15,
      64, 115, 79, 0, 255, 126, 64, 255, 190, 0, 204, 101, 64, 255, 165, 0, 153,
      76, 64, 217, 140, 0, 102, 51, 64, 166, 115, 0, 51, 26, 64, 115, 90, 0, 255,
      189, 64, 255, 253, 0, 204, 152, 64, 255, 216, 0, 153, 115, 64, 217, 179, 0,
      102, 78, 64, 166, 142, 0, 51, 41, 64, 115, 105, 0, 255, 255, 64, 255, 255, 0,
      204, 204, 64, 255, 255, 0, 153, 153, 64, 217, 217, 0, 102, 102, 64, 166, 166,
      0, 51, 51, 64, 115, 115, 0, 192, 255, 64, 255, 255, 0, 154, 204, 64, 218, 255,
      0, 116, 153, 64, 180, 217, 0, 78, 102, 64, 142, 166, 0, 40, 51, 64, 104, 115, 0,
      129, 255, 64, 193, 255, 0, 104, 204, 64, 168, 255, 0, 79, 153, 64, 143, 217, 0,
      54, 102, 64, 118, 166, 0, 29, 51, 64, 93, 115, 0, 66, 255, 64, 130, 255, 0, 53,
      204, 64, 117, 255, 0, 40, 153, 64, 104, 217, 0, 27, 102, 64, 91, 166, 0, 14, 51,
      64, 78, 115, 0, 0, 255, 64, 64, 255, 0, 0, 204, 64, 64, 255, 0, 0, 153, 64, 64,
      217, 0, 0, 102, 64, 64, 166, 0, 0, 51, 64, 64, 115, 63, 0, 255, 127, 64, 255, 51,
      0, 204, 115, 64, 255, 39, 0, 153, 103, 64, 217, 27, 0, 102, 91, 64, 166, 15, 0,
      51, 79, 64, 115, 126, 0, 255, 190, 64, 255, 101, 0, 204, 165, 64, 255, 76, 0,
      153, 140, 64, 217, 51, 0, 102, 115, 64, 166, 26, 0, 51, 90, 64, 115, 189, 0,
      255, 253, 64, 255, 152, 0, 204, 216, 64, 255, 115, 0, 153, 179, 64, 217, 78,
      0, 102, 142, 64, 166, 41, 0, 51, 105, 64, 115, 255, 0, 255, 255, 64, 255, 204,
      0, 204, 255, 64, 255, 153, 0, 153, 217, 64, 217, 102, 0, 102, 166, 64, 166,
      51, 0, 51, 115, 64, 115, 255, 0, 192, 255, 64, 255, 204, 0, 154, 255, 64, 218,
      153, 0, 116, 217, 64, 180, 102, 0, 78, 166, 64, 142, 51, 0, 40, 115, 64, 104,
      255, 0, 129, 255, 64, 193, 204, 0, 104, 255, 64, 168, 153, 0, 79, 217, 64, 143,
      102, 0, 54, 166, 64, 118, 51, 0, 29, 115, 64, 93, 255, 0, 66, 255, 64, 130, 204,
      0, 53, 255, 64, 117, 153, 0, 40, 217, 64, 104, 102, 0, 27, 166, 64, 91, 51, 0, 14,
      115, 64, 78, 51, 51, 51, 102, 102, 102, 153, 153, 153, 204, 204, 204, 192, 192, 192
    ) ;

//==============================================================================
// T_StorageFile
//==============================================================================

  {$IFDEF MSWINDOWS}
  constructor T_StorageFile.Create;
  begin
    inherited Create ;
    FRoot   := T_Storage.Create ;
    FStgLst := TGIS_ObjectList.Create ;
  end;

  procedure T_StorageFile.doDestroy ;
  begin
    FreeObject( FRoot   );
    FreeObject( FGStg   );
    FreeObject( FStgLst );

    inherited;
  end;

  procedure T_StorageFile.OpenStorageFile(
    const _fileName : String
  ) ;
  var
    {$IFDEF CLR}
      tpath : String;
    {$ELSE}
      tpath : PWideChar;
    {$ENDIF}
    rc    : HResult ;
    {$IFNDEF OXYGENE}
      mode  : Integer ;
    {$ELSE}
      mode  : STGM    ;
    {$ENDIF}
  begin
    FPath := _fileName ;

    if IsStringEmpty( _fileName ) then Exit;

    {$IFDEF CLR}
      tpath := _fileName ;
    {$ELSE}
      tpath := StringToOleStr( _fileName ) ;
    {$ENDIF}
    mode  := STGM_READ or STGM_SHARE_DENY_WRITE ;

    try
      try
        // test if file is Microsoft Compound File type
        rc := StgIsStorageFile( tpath ) ;
        if rc <> S_OK then Abort ;

        // open storage file
        rc := StgOpenStorage( tpath, nil, mode, nil, 0, FRoot.Storage ) ;

        if rc = STG_E_FILEALREADYEXISTS then Exit;
        if rc <> S_OK then Abort ;
      finally
        {$IFDEF CLR}
        {$ELSE}
          SysFreeString( tpath );
        {$ENDIF}
      end;
    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), FPath, 0 ) ;
    end;
  end;

  procedure T_StorageFile.EnumElements(
    const _elemList : TStrings ;
    const _root     : T_Storage
  ) ;
  var
    res      : HResult      ;
    oenum    : IEnumSTATSTG ;
    {$IFDEF CLR}
      stat_stg : array [0..0] of TStatStg   ;
    {$ELSE}
      stat_stg : TStatStg   ;
    {$ENDIF}
    {$IFDEF CLR}
      {$IFNDEF OXYGENE}
        ifetched : Longint    ;
      {$ELSE}
        ifetched : UInt32 ;
      {$ENDIF}
    {$ELSE}
      ifetched : Integer    ;
    {$ENDIF}
    sname    : String       ;
  begin
    try
      if not assigned( _root ) or not assigned( _elemList ) then exit ;

      // enumerate elements in storage
      res := _root.Storage.EnumElements( 0, nil, 0, oenum );
      if res <> S_OK then Abort ;

      repeat
        // add elements names to list
        {$IFDEF CLR}
          res := oenum.Next( 1, stat_stg, ifetched  );
        {$ELSE}
          res := oenum.Next( 1, stat_stg, @ifetched );
        {$ENDIF}

        {$IFNDEF OXYGENE}
          if( res <> S_OK ) then continue;
        {$ELSE}
          if( ifetched = 0 ) then continue;
        {$ENDIF}

        {$IFDEF CLR}
          if ifetched = 1 then
            with stat_stg[0] do begin
              sname := pwcsName ;
              _elemList.Add( sname );
            end;
        {$ELSE}
          with stat_stg do begin
            WideCharToStrVar( pwcsName, sName );
            _elemList.Add( sName );
            CoTaskMemFree( pwcsName );
          end;
        {$ENDIF}

      {$IFNDEF OXYGENE}
        until ( res <> S_OK ) ;
      {$ELSE}
        until ( ifetched = 0 ) ;
      {$ENDIF}

    except
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEBADFORMAT ), FPath, 1 ) ;
    end;
  end;

  function T_StorageFile.OpenStorage(
    const _name : String ;
    const _root : T_Storage
  ) : T_Storage;
  var
    {$IFDEF CLR}
      stmname : String ;
    {$ELSE}
      stmname : PWideChar ;
    {$ENDIF}
    {$IFNDEF OXYGENE}
      mode    : Integer ;
    {$ELSE}
      mode    : STGM ;
    {$ENDIF}
    res       : HResult ;
  begin
    Result := T_Storage.Create;
    if ( _root = nil ) or IsStringEmpty( _name ) then Exit;

    mode := STGM_READ or STGM_SHARE_EXCLUSIVE ;

    {$IFDEF CLR}
      stmname := _name ;
    {$ELSE}
      stmname := StringToOleStr( _name );
    {$ENDIF}
    try
      // open storage by name
      res := _root.Storage.OpenStorage(
               stmname, nil, mode, nil, 0, Result.Storage
             ) ;
      if Failed( res ) then
        OleError( res );
    finally
      {$IFDEF CLR}
      {$ELSE}
        SysFreeString( stmname );
      {$ENDIF}
    end;
  end;

  function T_StorageFile.OpenStream(
    const _name : String;
    const _root : T_Storage
  ) : TStream;
  var
    {$IFDEF CLR}
      stmname : String ;
    {$ELSE}
      stmname : PWideChar ;
    {$ENDIF}
    {$IFNDEF OXYGENE}
      mode    : Integer ;
    {$ELSE}
      mode    : STGM ;
    {$ENDIF}
    res     : HResult ;
    strname : String ;
    stm     : IStream ;
  begin
    Result  := nil;
    if ( _root = nil ) or IsStringEmpty( _name ) then Exit;

    strname := _name ;

    mode := STGM_READ or STGM_SHARE_EXCLUSIVE ;

    {$IFDEF CLR}
      stmname := strname ;
    {$ELSE}
      stmname := StringToOleStr( strname );
    {$ENDIF}
    try
      // open stream by name
      res := _root.Storage.OpenStream( stmname, nil, mode, 0, stm );
      if Failed( res ) then
        OleError( res );
      Result := TGIS_OleStream.Create( stm );
    finally
      {$IFDEF CLR}
      {$ELSE}
        SysFreeString( stmname );
      {$ENDIF}
    end;
  end;

  function T_StorageFile.DecompressStream(
    const _source : TStream
  ) : TStream ;
  begin
    Result := DecompressDeflateStream( _source ) ;
  end;

  function T_StorageFile.ReadModelRootStorage
    : TStrings ;
  var
    idx : Integer ;
  begin
    Result := TStringList.Create ;

    // read model root storage
    idx := FStgLst.Add( TObject( OpenStorage( 'Dgn-Md' , FRoot  ) ) );
    // read model root storage substorage
    idx := FStgLst.Add(
             TObject(
               OpenStorage( '#000000', T_Storage( FStgLst.Items[ idx ]  ))
             )
           ) ;
    // open storage that keeps graphics data in streams
    FGStg := OpenStorage( 'Dgn^G', T_Storage( FStgLst.Items[ idx ] ) ) ;

    // enumerate elements in storage, should return $XXX like names
    if assigned( FGStg ) then
      EnumElements( Result, FGStg );
  end;

  function T_StorageFile.ReadRootModelHeader
    : TStream ;
  begin
    // open root model header that keeps global settings
    Result := OpenStream( 'Dgn~Mh', T_Storage( FStgLst.Items[ 2 ] ) ) ;
  end;

  function T_StorageFile.ReadNonModelHeader
    : TStrings;
  var
    i   : Integer ;
  begin
    Result := TStringList.Create ;

    // open non model header that keeps global settings
    i := FStgLst.Add( TObject( OpenStorage( 'Dgn^Nm' , FRoot  ) ) );
    FNStg := T_Storage( FStgLst.Items[ i ] ) ;
    EnumElements( Result, T_Storage( FStgLst.Items[ i ] ) );
  end ;
  {$ENDIF}

//=============================================================================
// TGIS_LayerDGN
//=============================================================================

  constructor TGIS_LayerDGN.Create ;
  begin
    inherited ;

    FSubType := FSubType + [ TGIS_LayerSubType.Persistent ] ;
    dcLevels := TDictionary<Integer, Integer>.Create ;

    SupportsAutoStyle := False ;
  end ;

  procedure TGIS_LayerDGN.doDestroy ;
  begin
    // free temporary shapes
    FreeObject ( dgnFile  ) ;
    FreeObject ( dcLevels ) ;

    inherited ;
  end ;

  function TGIS_LayerDGN.isColorNearBackground(
    const _idx : Integer ;
    const _tol : Integer
  ) : Boolean ;
  var
    clvwr    : TGIS_Color ;
    r,g,b    : Byte   ;
    vr,vg,vb : Byte   ;
  begin
    if ( _idx > 255 ) or ( _idx < 0 ) then begin
      r := 0 ;
      g := 0 ;
      b := 0 ;
    end
    else begin
      r := dgnColorTable[ _idx * 3 + 0 ] ;
      g := dgnColorTable[ _idx * 3 + 1 ] ;
      b := dgnColorTable[ _idx * 3 + 2 ] ;
    end ;

    if assigned( Viewer ) then begin
      clvwr := Viewer.Ref.Color ;
    end
    else begin
      clvwr := TGIS_Color.Black ;
    end ;

    vr := clvwr.R ;
    vg := clvwr.G ;
    vb := clvwr.B ;

    if ( Abs( vr - r ) < _tol ) and
       ( Abs( vg - g ) < _tol ) and
       ( Abs( vb - b ) < _tol )
    then // color very close to the window color
      Result := True
    else
      Result := False ;
  end ;

  function TGIS_LayerDGN.getColor(
    const _idx : Integer
  ) : TGIS_Color ;
  var
    clvwr    : TGIS_Color ;
    r,g,b    : Byte   ;
    vr,vg,vb : Byte    ;
    res      : TGIS_Color ;
    i        : Integer ;
  begin
    if ( _idx > 255 ) or ( _idx < 0 ) then begin
      i := _idx mod 256 ;
      r := dgnColorTable[ i * 3 + 0 ] ;
      g := dgnColorTable[ i * 3 + 1 ] ;
      b := dgnColorTable[ i * 3 + 2 ] ;
    end
    else begin
      r := dgnColorTable[ _idx * 3 + 0 ] ;
      g := dgnColorTable[ _idx * 3 + 1 ] ;
      b := dgnColorTable[ _idx * 3 + 2 ] ;
    end ;

    res := TGIS_Color.FromRGB( r, g, b ) ;

    if assigned( Viewer ) then begin
      clvwr := Viewer.Ref.Color ;
    end
    else begin
      clvwr := TGIS_Color.Black ;
    end ;

    vr := clvwr.R ;
    vg := clvwr.B ;
    vb := clvwr.B ;

    // If color is equal to the window color then the color will be assigned to
    // a first entry in the color table ( inverse color of background )
    if ( res = clvwr ) then begin
      r := dgnColorTable[ 0 ] ;
      g := dgnColorTable[ 1 ] ;
      b := dgnColorTable[ 2 ] ;

      if ( Abs( vr - r ) < 16 ) and
         ( Abs( vg - g ) < 16 ) and
         ( Abs( vb - b ) < 16 )
      then // color very close to the window color
        res := TGIS_Color.FromRGB( r xor vr, g xor vg, b xor vb )
      else
        res := TGIS_Color.FromRGB( r, g, b )  ;
    end ;

     Result := res ;
  end ;

  procedure TGIS_LayerDGN.prepareShape(
    const _type : TGIS_ShapeType
  ) ;
  var
    currShape : TGIS_Shape ;

      function getWeight(
        const _weight : Integer
      ) : Integer ;
      begin
        if _weight >=0 then begin
          if assigned( Viewer ) then
            Result := Viewer.Ref.PixelsToTwips( _weight + 1 )
          else
            Result := RoundS( _weight * 1440.0 / 96.0 ) ;
        end
        else
          Result := 1 ;

        if Result = 0 then
          Result := 1 ;
      end ;

      function getStyle(
        const _idx : Integer
      ) : TGIS_PenStyle ;
      const
         GIS_DGNS_SOLID                =  0 ;
         GIS_DGNS_DOTTED               =  1 ;
         GIS_DGNS_MEDIUM_DASH          =  2 ;
         GIS_DGNS_LONG_DASH            =  3 ;
         GIS_DGNS_DOT_DASH             =  4 ;
         GIS_DGNS_SHORT_DASH           =  5 ;
         GIS_DGNS_DASH_DOUBLE_DOT      =  6 ;
         GIS_DGNS_LONG_DASH_SHORT_DASH =  7 ;
      begin
        case _idx of
          GIS_DGNS_SOLID                :
            Result := TGIS_PenStyle.Solid       ;
          GIS_DGNS_DOTTED               :
            Result := TGIS_PenStyle.Dot         ;
          GIS_DGNS_MEDIUM_DASH          :
            Result := TGIS_PenStyle.Dash        ;
          GIS_DGNS_LONG_DASH            :
            Result := TGIS_PenStyle.Dash        ;
          GIS_DGNS_DOT_DASH             :
            Result := TGIS_PenStyle.DashDot     ;
          GIS_DGNS_SHORT_DASH           :
            Result := TGIS_PenStyle.Dash        ;
          GIS_DGNS_DASH_DOUBLE_DOT      :
            Result := TGIS_PenStyle.DashDotDot  ;
          GIS_DGNS_LONG_DASH_SHORT_DASH :
            Result := TGIS_PenStyle.DashDot     ;
          else
            Result := TGIS_PenStyle.Solid       ;
        end
      end ;

      function getPosition(
        const _idx : Integer
      ) : TGIS_LabelPosition ;
      const
         GIS_DGNJ_LEFT_TOP           =  0 ;
         GIS_DGNJ_LEFT_CENTER        =  1 ;
         GIS_DGNJ_LEFT_BOTTOM        =  2 ;
         GIS_DGNJ_LEFTMARGIN_TOP     =  3 ;
         GIS_DGNJ_LEFTMARGIN_CENTER  =  4 ;
         GIS_DGNJ_LEFTMARGIN_BOTTOM  =  5 ;
         GIS_DGNJ_CENTER_TOP         =  6 ;
         GIS_DGNJ_CENTER_CENTER      =  7 ;
         GIS_DGNJ_CENTER_BOTTOM      =  8 ;
         GIS_DGNJ_RIGHTMARGIN_TOP    =  9 ;
         GIS_DGNJ_RIGHTMARGIN_CENTER = 10 ;
         GIS_DGNJ_RIGHTMARGIN_BOTTOM = 11 ;
         GIS_DGNJ_RIGHT_TOP          = 12 ;
         GIS_DGNJ_RIGHT_CENTER       = 13 ;
         GIS_DGNJ_RIGHT_BOTTOM       = 14 ;
      begin
        case _idx of
         GIS_DGNJ_LEFT_BOTTOM   :  Result := TGIS_LabelPosition.UpLeft ;
         GIS_DGNJ_LEFT_CENTER   :  Result := TGIS_LabelPosition.MiddleLeft ;
         GIS_DGNJ_LEFT_TOP      :  Result := TGIS_LabelPosition.DownLeft ;
         GIS_DGNJ_CENTER_BOTTOM :  Result := TGIS_LabelPosition.UpCenter ;
         GIS_DGNJ_CENTER_CENTER :  Result := TGIS_LabelPosition.MiddleCenter ;
         GIS_DGNJ_CENTER_TOP    :  Result := TGIS_LabelPosition.UpCenter ;
         GIS_DGNJ_RIGHT_BOTTOM  :  Result := TGIS_LabelPosition.UpRight ;
         GIS_DGNJ_RIGHT_CENTER  :  Result := TGIS_LabelPosition.MiddleRight ;
         GIS_DGNJ_RIGHT_TOP     :  Result := TGIS_LabelPosition.DownRight ;
        else                       Result := TGIS_LabelPosition.MiddleCenter;
        end;
      end;

  begin
    if elmProperty3D then
      cursorDgn[0].currShape := CreateShape( _type, TGIS_DimensionType.XYZ )
    else
      cursorDgn[0].currShape := CreateShape( _type ) ;

    currShape := cursorDgn[0].currShape;

    currShape.Lock( TGIS_Lock.Projection ) ;
    currShape.Reset ;
    currShape.AddPart ;

    if not IgnoreShapeParams then begin
      case _type of
        TGIS_ShapeType.Point :
          begin
            currShape.Params.Marker.SmartSize    := 0                 ;
            currShape.Params.Marker.Size         := 90                ;
            currShape.Params.Labels.Field        := GIS_DGN_FLD_LABEL ;
            currShape.Params.Labels.Width        := -1000             ;
            currShape.Params.Labels.Color        := TGIS_Color.Black           ;
            currShape.Params.Labels.Color        := getColor ( elmGraphColor  ) ;
            currShape.Params.Labels.FontColor    := currShape.Params.Labels.Color;
            currShape.Params.Labels.OutlineWidth := 0                 ;
            currShape.Params.Labels.Pattern      := TGIS_BrushStyle.Clear  ;
            currShape.Params.Labels.Position     := GisGetLabelPosition( getPosition( just ) );
            currShape.Params.Labels.Alignment    := TGIS_LabelAlignment.LeftJustify;
            currShape.Params.Labels.Rotate       := - rotAngle;
          end ;
        TGIS_ShapeType.Arc :
          begin
            currShape.Params.Line.SmartSize := 0 ;
            currShape.Params.Line.Width     := getWeight( elmGraphWeight ) ;
            currShape.Params.Line.Style     := getStyle ( elmGraphStyle  ) ;

            if isColorNearBackground( elmGraphColor, 8 ) then begin
              currShape.Params.Line.Color := TGIS_Color.Black ;
              currShape.Params.Line.OutlineColor := getColor( elmGraphColor ) ;

              if ( currShape.Params.Line.Color <>
                   currShape.Params.Line.OutlineColor
                  ) then
                currShape.Params.Line.OutlineWidth := 1 ;
            end
            else
              currShape.Params.Line.Color := getColor( elmGraphColor ) ;
          end ;
        TGIS_ShapeType.Polygon,
        TGIS_ShapeType.MultiPatch :
          begin
            currShape.Params.Area.SmartSize    := 0 ;
            currShape.Params.Area.OutlineColor := getColor ( elmGraphColor  ) ;
            currShape.Params.Area.OutlineWidth := getWeight( elmGraphWeight ) ;
            currShape.Params.Area.OutlineStyle := getStyle ( elmGraphStyle  ) ;

            if not hasFillColor then
              currShape.Params.Area.Pattern   := TGIS_BrushStyle.Clear
            else begin
              currShape.Params.Area.Pattern   := TGIS_BrushStyle.Solid ;
              currShape.Params.Area.Color     := getColor ( elmGraphColor  ) ;
            end ;
          end ;
      end ;
    end ;

  end;

  function TGIS_LayerDGN.findLevelIdx( const _level : Integer ) : Integer ;
  var
    idx : {$IFDEF JAVA} nullable {$ENDIF} Integer ;
  begin
    if dcLevels.TryGetValue( _level, idx ) then
      Result := idx
    else
      Result := -1 ;
  end ;

  procedure TGIS_LayerDGN.unprepareShape ;
  var
    idx : Integer ;
  begin
    cursorDgn[0].currShape.Unlock ;
    if ( elmLevel < SubLayers.Count ) then begin
      idx := findLevelIdx( elmLevel ) ;
      if idx <> -1 then
        cursorDgn[0].currShape.Layer := SubLayers[idx] as TGIS_LayerVector ;
    end ;
  end ;

  procedure TGIS_LayerDGN.readHeader ;
  begin
    elmProperty3D := False ;
    elmInvisible  := False;

    // read primary part of element header
    {$IFDEF OXYGENE}
    currStream.ReadCardinal( elmSignature,  4 ) ;
    currStream.ReadWord    ( elmType,       2 ) ;
    currStream.ReadWord    ( elmFlags,      2 ) ;
    currStream.ReadCardinal( elmSize,       4 ) ;
    currStream.ReadCardinal( elmAttrOffset, 4 ) ;
    currStream.ReadCardinal( elmLevel,      4 ) ;
    {$ELSE}
    currStream.ReadBuffer( elmSignature,  4 ) ;
    currStream.ReadBuffer( elmType,       2 ) ;
    currStream.ReadBuffer( elmFlags,      2 ) ;
    currStream.ReadBuffer( elmSize,       4 ) ;
    currStream.ReadBuffer( elmAttrOffset, 4 ) ;
    currStream.ReadBuffer( elmLevel,      4 ) ;
    {$ENDIF}
    currStream.Position := currStream.Position + 16 ;

    // read graphics part of element header
    elmFlagGraphic := ( elmFlags and $1000 ) = $1000 ;

    if elmFlagGraphic then begin
      {$IFDEF OXYGENE}
      currStream.ReadCardinal( elmGraphicGroup, 4 ) ;
      currStream.ReadCardinal( elmPriority, 4 ) ;
      currStream.ReadWord( elmProperties, 2 ) ;
      currStream.Position := currStream.Position + 2 ;
      currStream.ReadInteger( elmGraphStyle,  4 ) ;
      currStream.ReadCardinal( elmGraphWeight, 4 ) ;
      currStream.ReadCardinal( elmGraphColor,  4 ) ;
      {$ELSE}
      currStream.ReadBuffer( elmGraphicGroup, 4 ) ;
      currStream.ReadBuffer( elmPriority, 4 ) ;
      currStream.ReadBuffer( elmProperties, 2 ) ;
      currStream.Position := currStream.Position + 2 ;
      currStream.ReadBuffer( elmGraphStyle,  4 ) ;
      currStream.ReadBuffer( elmGraphWeight, 4 ) ;
      currStream.ReadBuffer( elmGraphColor,  4 ) ;
      {$ENDIF}
      // skip xyz min, xyz max as Int64
      currStream.Position := currStream.Position + 48 ;

      elmClass      := ( elmProperties and GIS_DGNPF_CLASS     ) ;
      elmProperty3D := ( elmProperties and GIS_DGNPF_3D        ) = GIS_DGNPF_3D ;
      elmInvisible  := ( elmProperties and GIS_DGNPF_INVISIBLE ) = GIS_DGNPF_INVISIBLE ;
      elmHole       := ( elmProperties and GIS_DGNPF_HOLE      ) = GIS_DGNPF_HOLE ;

      parseHeader ;
    end ;
  end ;

  procedure TGIS_LayerDGN.parseHeader ;
  var
    idx : Integer ;
  begin
    elmFillColor := 0 ;
    hasFillColor := False ;
    if ( elmGraphWeight = GIS_DGN_SYMBOLOGY_BYLEVEL_WEIGHT ) or
       ( elmGraphColor  = GIS_DGN_SYMBOLOGY_BYLEVEL_COLOR  ) or
       ( elmGraphStyle  = GIS_DGN_SYMBOLOGY_BYLEVEL_STYLE  ) then begin
      // test if it has a layer style
      idx := findLevelIdx( elmLevel ) ;
      if idx <> -1 then begin
        if ( elmGraphWeight = GIS_DGN_SYMBOLOGY_BYLEVEL_WEIGHT ) then
          elmGraphWeight := tabLevels[ idx ].elmWeight ;
        if ( elmGraphStyle = GIS_DGN_SYMBOLOGY_BYLEVEL_STYLE ) then
          elmGraphStyle := tabLevels[ idx ].elmStyle ;
        if ( elmGraphColor  = GIS_DGN_SYMBOLOGY_BYLEVEL_COLOR ) then begin
          elmGraphColor := tabLevels[ idx ].elmColor  ;
          elmFillColor  := tabLevels[ idx ].elmFillColor ;
        end ;

        elmInvisible := tabLevels[ idx ].flagIsHidden ;
      end
      else begin
        elmGraphWeight := 0 ;
        elmGraphColor  := 0 ;
        elmFillColor   := 0 ;
      end ;
    end
    else if ( elmGraphWeight = GIS_DGN_SYMBOLOGY_BYCELL_WEIGHT ) or
            ( elmGraphColor  = GIS_DGN_SYMBOLOGY_BYCELL_COLOR  ) or
            ( elmGraphStyle  = GIS_DGN_SYMBOLOGY_BYCELL_STYLE  ) then begin
      // should take style from cell ?
      elmGraphWeight := 0 ;
      elmGraphColor := 0 ;
    end ;
    inc( elmGraphColor );

  end ;

  function TGIS_LayerDGN.getWeight(
    const _weight : Integer
  ) : Integer ;
  begin
    if assigned( Viewer ) then
      Result := Viewer.Ref.PixelsToTwips( _weight + 1 )
    else
      Result := RoundS( _weight * 1440.0 / 96.0 )
  end ;

  procedure TGIS_LayerDGN.readRootModelHeader ;
  begin
    // skip empty space
    currStream.Position := $1000;
    readHeader;

    // read global origin
    currStream.Position := $1074 ;
    {$IFDEF OXYGENE}
    currStream.ReadDouble( dgnGlobalOrigin.X, 8 );
    currStream.ReadDouble( dgnGlobalOrigin.Y, 8 );
    currStream.ReadDouble( dgnGlobalOrigin.Z, 8 );
    {$ELSE}
    currStream.ReadBuffer( dgnGlobalOrigin.X, 8 );
    currStream.ReadBuffer( dgnGlobalOrigin.Y, 8 );
    currStream.ReadBuffer( dgnGlobalOrigin.Z, 8 );
    {$ENDIF}

    if dgnGlobalOrigin.Z <> 0 then
      DefaultDimension := TGIS_DimensionType.XYZ ;

    // read storage units
    currStream.Position := $10E4;
    {$IFDEF OXYGENE}
    currStream.ReadDouble( dgnUORPerStorageUnit, 8 );
    currStream.ReadDouble( dgnUORPerStorageSubUnit, 8 );
    {$ELSE}
    currStream.ReadBuffer( dgnUORPerStorageUnit, 8 );
    currStream.ReadBuffer( dgnUORPerStorageSubUnit, 8 );
    {$ENDIF}
    dgnScale := 1.0 / ( dgnUORPerStorageUnit * dgnUORPerStorageSubUnit ) ;
  end;

  procedure TGIS_LayerDGN.readAttributeLinkages;
  const
    GIS_DGN_FLD_ENTITY  = 'DGN_ENT_' ;
    GIS_DGN_FLD_MSLINK  = 'DGN_LNK_' ;
  var
    i         : Integer ;
    len       : Word    ;
    end_pos   : Int64   ;
    old_cnt   : Integer ;
    fldname   : String  ;
    linksize  : Word ;
    primaryid : Word    ;
    buf       : TBytes ;
    key       : Integer ;
    length    : Integer ;
    offset    : Integer ;
    mflags    : Integer ;
    str       : String ;
    fillColor : Integer ;
    br        : TGIS_Bytes ;
    angle1    : Double ;
    angle2    : Double ;
    hasHatch  : Boolean ;

    function readInt32( const _buf : TBytes ; const _offset : Integer ) : Integer ;
    begin
      Result := _buf[_offset] or (_buf[_offset+1] shl 8) or
                (_buf[_offset+2] shl 16) or (_buf[_offset+3] shl 24) ;
    end ;

  begin
    angle1       := 0 ;
    angle2       := 0 ;
    elmAttrCount := 0 ;
    hasHatch     := False ;
    elmStringNameLinkage := '' ;
    elmStringDescLinkage := '' ;

    old_cnt := elmAttrCount ;
    end_pos := oldPos + 2 * elmSize + 4 ;

    if currStream.Position < end_pos then begin
      len := end_pos - currStream.Position ;
      if len >=2 then begin
        SetLength( buf, len ) ;
        {$IFDEF OXYGENE}
          currStream.ReadBuffer( buf, len ) ;
        {$ELSE}
          currStream.ReadBuffer( buf[0], len );
        {$ENDIF}
        try
          i := 0 ;
          while i < len-1 do begin
              // a fixed-size DMRS linkage
              if (buf[i]=0) and (buf[i+1]=0  ) or
                 (buf[i]=0) and (buf[i+1]=$80) then
                linksize := 8
              else if (buf[i+1] and $10) = $10 then
                linksize := buf[i] * 2 + 2
              else
                break ;

              if (buf[i]=0) and (buf[i+1]=0  ) or
                 (buf[i]=0) and (buf[i+1]=$80) then begin
                if i+3 < len then
                  elmAttrEntity[ elmAttrCount ] := Word(buf[i+2] or (buf[i+3] shl 8)) ;
                if i+6 < len then
                  elmAttrMSLink[ elmAttrCount ] := (buf[i+4] or (buf[i+5] shl 8) ) or
                                                   (buf[i+6] shl 16) ;
                inc( i, linksize ) ;
                if ( elmAttrEntity[elmAttrCount] <> 0) and
                   ( elmAttrMSLink[elmAttrCount] <> 0) then
                inc( elmAttrCount );
                continue ;
              end
              else begin
                if i + 3 < len then
                  primaryid := Word(buf[i+2] or (buf[i+3] shl 8))
                else
                  primaryid := 0 ;
              end ;

              if ( primaryid = GIS_DGNLT_INFORMIX ) or
                 ( primaryid = GIS_DGNLT_XBASE    ) or
                 ( primaryid = GIS_DGNLT_RIS      ) or
                 ( primaryid = GIS_DGNLT_ORACLE   ) or
                 ( primaryid = GIS_DGNLT_INGRES   ) or
                 ( primaryid = GIS_DGNLT_SYBASE   ) or
                 ( primaryid = GIS_DGNLT_ODBC     ) or
                 ( primaryid = GIS_DGNLT_BUDBC    ) or
                 ( primaryid = GIS_DGNLT_OLEDB    ) then
              begin
                elmAttrEntity[ elmAttrCount ] := Word(buf[i+6] or (buf[i+7] shl 8)) ;
                elmAttrMSLink[ elmAttrCount ] := buf[i+8] or (buf[i+9] shl 8) or
                                                 (buf[i+10] shl 16) or (buf[i+11] shl 24) ;
                inc( i, linksize ) ;
                if ( elmAttrEntity[elmAttrCount] <> 0) and
                   ( elmAttrMSLink[elmAttrCount] <> 0) then
                inc( elmAttrCount );
              end
              else if ( primaryid = GIS_DGNLT_SHAPE_FILL ) then
              begin
                // if there is color fill
                if buf[i+4] = 8 then begin
                  if (buf[i+6] = 0) then begin
                    fillColor := buf[i+8] ;
                    // by layer
                    if fillColor <> 255 then begin
                      elmFillColor := fillColor ;
                    end ;
                    inc( elmFillColor );
                    hasFillColor := True ;
                  end ;
                end ;
                inc( i, linksize ) ;
              end
              else if ( primaryid = GIS_DGNLT_STRING ) then begin
                key    := readInt32( buf, i+4 ) ;
                length := readInt32( buf, i+8 ) ;
                offset := i+12 ;
                str    := '';

                if ((buf[i+12] = $ff) and (buf[i+13] = $fe) and
                    (buf[i+14] = $01) and (buf[i+15] = $00)) then begin
                    // skip UTF32 LE BOM
                    inc( offset, 4 );
                    length := length - 4;
                    {$IFDEF DCC}
                    str := TEncoding.ANSI.GetString( buf, offset, length );
                    {$ELSE}
                    str := TEncoding.ASCII.GetString( buf, offset, length );
                    {$ENDIF}
                end
                else if ((buf[i+12] = $ff) and (buf[i+13] = $fd)) or
                        ((buf[i+12] = $ff) and (buf[i+13] = $fe)) then
                  {$IFDEF JAVA OR ISLAND}
                    str := TEncoding.UTF16LE.GetString( buf, offset + 2, length - 2 )
                  {$ELSE}
                    str := TEncoding.Unicode.GetString( buf, offset + 2, length - 2 )
                  {$ENDIF}
                else
                  {$IFDEF DCC}
                  str := TEncoding.ANSI.GetString( buf, offset, length );
                  {$ELSE}
                  str := TEncoding.ASCII.GetString( buf, offset, length );
                  {$ENDIF}
                case key of
                  1 : elmStringNameLinkage := str ;
                  2 : elmStringDescLinkage := str ;
                end ;
                inc( i, linksize ) ;
              end
              else if (primaryid = GIS_DGNLT_HATCH) then begin
                offset := 0 ;
                {$IFDEF DCC}
                br := TGIS_Bytes.Create( @buf[i+4], 0, len ) ;
                {$ELSE}
                br := TGIS_Bytes.Create( buf, i+4, len ) ;
                {$ENDIF}
                try
                  mflags := br.ReadInt32( offset ) ;
                  inc( offset, 4 ) ;
                  if (mflags and 1) <> 0 then begin
                    inc( offset, 8 ) ;
                  end ;
                  if (mflags and 2) <> 0 then begin
                    angle1 := RadToDeg( br.ReadDouble( offset ) ) ;
                    inc( offset, 8 ) ;
                    hasHatch := True ;
                  end ;
                  if (mflags and 4) <> 0 then begin
                    inc( offset, 8 ) ;
                  end ;
                  if (mflags and 8) <> 0 then begin
                    angle2 := RadToDeg( br.ReadDouble( offset ) ) ;
                    inc( offset, 8 ) ;
                    hasHatch := True ;
                  end ;
                finally
                  FreeObject( br ) ;
                end ;
                inc( i, linksize ) ;
              end
              else
                inc( i, linksize ) ;
            end
        except

        end;
      end ;
    end ;

    // add global attributes fields to layer
    while elmAttrCount > dgnAttrCount do begin
      fldname := Format( '%s%.2x',[ GIS_DGN_FLD_ENTITY, dgnAttrCount ] ) ;
      if FindField( fldname ) < 0 then
        AddFieldInternal( fldname, TGIS_FieldType.Number,  5, 0 ) ;

      fldname := Format( '%s%.2x',[ GIS_DGN_FLD_MSLINK, dgnAttrCount ] ) ;
      if FindField( fldname ) < 0 then
        AddFieldInternal( fldname, TGIS_FieldType.Number, 10, 0 ) ;

      inc( dgnAttrCount ) ;
    end ;

    // duplicate header elements for its subelements
    if assigned( cursorDgn[0].currShape ) then begin
      if cplxCellCnt > 0 then begin
        if old_cnt > 0 then
          elmAttrCount := old_cnt ;

          dec( cplxCellCnt );
      end;

      if ( cursorDgn[0].currShape.ShapeType = TGIS_ShapeType.Polygon ) then begin
         if hasFillColor then begin
            cursorDgn[0].currShape.Params.Area.Pattern := TGIS_BrushStyle.Solid ;
            cursorDgn[0].currShape.Params.Area.Color   := getColor ( elmFillColor  ) ;
         end
         else if hasHatch then begin
           // emulate pattern based on angles
           if ((angle1 = 180) and (angle2 = 90)) or ((angle2 = 180) and (angle1 = 90)) then
             cursorDgn[0].currShape.Params.Area.Pattern := TGIS_BrushStyle.Cross
           else if ((angle1 > 0) and (angle1 < 90) and (angle2 > 90) and (angle2 < 180)) or
                   ((angle2 > 0) and (angle2 < 90) and (angle1 > 90) and (angle1 < 180)) then
             cursorDgn[0].currShape.Params.Area.Pattern := TGIS_BrushStyle.DiagCross
           else if angle1 = 180 then
             cursorDgn[0].currShape.Params.Area.Pattern := TGIS_BrushStyle.Horizontal
           else if angle1 = 90 then
             cursorDgn[0].currShape.Params.Area.Pattern := TGIS_BrushStyle.Vertical
           else if ((angle1 < 90) or (angle1 > 180) and (angle1 < 270)) then
             cursorDgn[0].currShape.Params.Area.Pattern := TGIS_BrushStyle.BDiagonal
           else if ((angle1 > 90) and (angle1 < 180) or (angle1 > 270) and (angle1 < 360)) then
             cursorDgn[0].currShape.Params.Area.Pattern := TGIS_BrushStyle.FDiagonal
         end ;
      end ;

      // add attributes to current shape
      for i := 0 to elmAttrCount - 1 do begin
        cursorDgn[0].currShape.SetField(
          Format( '%s%.2x', [ GIS_DGN_FLD_ENTITY, i ] ),
          elmAttrEntity[ i ]
        ) ;
        cursorDgn[0].currShape.SetField(
          Format( '%s%.2x', [ GIS_DGN_FLD_MSLINK, i ] ),
          elmAttrMSLink[ i ]
        ) ;
      end ;
    end
    else if ( elmType = GIS_DGNT_COMPLEX_CHAIN_HEADER ) or
            ( elmType = GIS_DGNT_COMPLEX_SHAPE_HEADER ) then
      cplxComponentsFillColor := getColor ( elmFillColor  ) ;

    currStream.Position := end_pos ;
  end;

  procedure TGIS_LayerDGN.parseStream;
  var
    cnt,
    res,
    i, p, b,
    num_poles,
    num_knots,
    iflags      : Integer ;
    compcnt     : Cardinal ;
    attachmentId: Int64 ;
    num         : Word;
    origin,
    axis        : TGIS_Point3D;
    line        : TGIS_Line3D;
    txt_arr     : TBytes  ;
    isUnicode,
    byLev       : Boolean ;
    text        : String ;
    sb          : TStringBuilder ;
    start_angle,
    sweep_angle,
    d           : Double  ;
    topo        : TGIS_Topology ;
    shp         : TGIS_Shape ;
    shpEx       : TGIS_Shape ;
    inp         : Array of TGIS_Point3D ;
    mlsymbarr   : array of T_MLSymbology ;
    nPoints     : Word ;
    version     : Byte ;
    nLines      : Byte ;
    nBreaks     : Byte ;
    zVector     : TGIS_Point3D;
    lm,hm,l,h   : Double ;

     procedure addPointInternal(
       const _ptg : TGIS_Point3D
     ) ;
     begin
       if ( Abs( _ptg.X ) < 1.0e20 ) and
          ( Abs( _ptg.Y ) < 1.0e20 ) then
         cursorDgn[0].currShape.AddPoint3D( _ptg );
     end;

    // Build rotated ellipse
    // Used in rotated lwpolyline
    // _center    center point
    // _axis      axis X and axis Y
    // _roatation rotation angle in radians
    // _start     start angle
    // _stop      stop angle
    procedure draw_arc(
      const _center, _axis           : TGIS_Point3D;
      const _rotation, _start, _stop : Double
    ) ;
    var
      cnt1        : Integer ;
      absarcangle : Double  ;
      arcangle    : Double  ;
      steps       : Integer ;
      step        : Double  ;
      start       : Double  ;
      nx          : Double  ;
      ny          : Double  ;
      rotate_cos  : Double  ;
      rotate_sin  : Double  ;
      start_cos   : Double  ;
      start_sin   : Double  ;
      radius      : Double  ;
      sqr_x       : Double  ;
      sqr_y       : Double  ;
      sqr_xy      : Double  ;
      sqr_y_x     : Double  ;
      mult        : Double  ;
    const
      MAX_STEPS   = 90      ;
    begin
      arcangle    := _stop-_start ;
      absarcangle := Abs( arcangle ) ;

    // calculate number of segments - and minimize it
       if      absarcangle < Pi/4 then steps := MAX_STEPS div 8
       else if absarcangle < Pi/2 then steps := MAX_STEPS div 4
       else if absarcangle < Pi   then steps := MAX_STEPS div 2
       else steps := MAX_STEPS - 1 ;

       step := arcangle / steps ;

       start := _start + Pi/2 ;

       SinCos( _rotation, rotate_sin, rotate_cos ) ;

       sqr_x := _axis.X * _axis.X ;
       sqr_y := _axis.Y * _axis.Y ;
       sqr_xy :=  sqr_y * sqr_x ;
       sqr_y_x := sqr_y - sqr_x  ;
       for cnt1 := 0 to steps do begin
         SinCos( start, start_sin, start_cos ) ;

         mult := ( sqr_x + ( sqr_y_x )* start_sin * start_sin ) ;
         if mult <> 0 then
           radius := Sqrt( ( sqr_xy  ) / mult )
         else
           radius := 0;
         ny :=  radius * ( start_cos * rotate_cos +
               -  start_sin * rotate_sin );
         nx :=  radius * ( start_cos * rotate_sin  +
                 start_sin * rotate_cos ) ;

        addPointInternal(  GisPoint3D( _center.X+nx, _center.Y-ny, _center.Z ) ) ;
        start := start + step ;
       end ;
    end ;

     procedure buildComplex ;
     var
      poly : TGIS_Shape ;
     begin
      try
        // build complex shape
        if cplxComponentsCnt > 0 then begin
          dec( cplxComponentsCnt );
          if assigned( cursorDgn[0].currShape ) then
            cplxList.Add( cursorDgn[0].currShape ) ;

          if cplxComponentsCnt = 0 then begin
            shp := topo.UnionOnList( cplxList ) ;
            if assigned( shp ) then begin
              case cplxComponentsType of
                TGIS_ShapeType.Polygon     :
                  begin
                    poly  := shp.AsPolygon ;
                    shpEx := AddShape( poly ) ;
                    FreeObject( poly ) ;
                  end ;
              else
                shpEx := AddShape( shp ) ;
              end;
              if not IgnoreShapeParams then begin
                if assigned( cursorDgn[0].currShape ) then
                  shpEx.Params.Assign( cursorDgn[0].currShape.Params );
                if cplxComponentsFillColor <> TGIS_Color.Black then begin
                  if cplxComponentsType = TGIS_ShapeType.Polygon then begin
                    shpEx.Params.Area.Pattern := TGIS_BrushStyle.Solid ;
                    shpEx.Params.Area.Color   := cplxComponentsFillColor ;
                  end
                  else
                    shpEx.Params.Line.Color   := cplxComponentsFillColor ;
                end
                else begin
                  shpEx.Params.Area.Pattern := TGIS_BrushStyle.Clear ;
                  shpEx.Params.Area.OutlineColor := cplxComponentsOutColor ;
                end ;
              end ;
              FreeObject( shp );
            end ;
          end ;
        end ;
      except
        // union can fail
      end;
     end ;

     procedure draw_curve( const _inp : array of TGIS_Point3D ;
                           const _len : Integer
                          ) ;
     var
       mx, my,
       d, tx, ty  : TGIS_DoubleArray ;
       k, noutp   : Integer ;
       totald     : Double ;
       stepsize   : Double ;
       fd         : Double ;
       outp       : array of TGIS_Point3D ;
       ax, ay,
       bx, by,
       cx, cy     : Double ;
     begin
        if( _len < 6 ) then exit ;

        totald := 0.0 ;
        SetLength( mx, 5*_len ) ;
        SetLength( my, 5*_len ) ;
        SetLength( d , 5*_len ) ;
        SetLength( tx, 5*_len ) ;
        SetLength( ty, 5*_len ) ;
        SetLength( outp, 5*_len ) ;

        for k := 0 to _len-2 do begin
          d[k] := Sqrt( (_inp[k+1].X-_inp[k].X) * (_inp[k+1].X-_inp[k].X) +
                        (_inp[k+1].Y-_inp[k].Y) * (_inp[k+1].Y-_inp[k].Y)
                       ) ;
          if ( d[k] = 0.0 ) then begin
             d[k] := 0.0001 ;
            mx[k] := 0.0 ;
            my[k] := 0.0 ;
          end
          else begin
            mx[k] := (_inp[k+1].X - _inp[k].X) / d[k] ;
            my[k] := (_inp[k+1].Y - _inp[k].Y) / d[k] ;
          end ;

          if ( k > 1 ) and ( k < _len - 3 ) then
            totald := totald + d[k] ;
        end ;

        for k := 2 to _len - 3 do begin
          if ( Abs(mx[k+1] - mx[k]  ) = 0.0 ) and
             ( Abs(mx[k-1] - mx[k-2]) = 0.0 ) then
            tx[k] := (mx[k] + mx[k-1]) / 2
          else
            tx[k] := (mx[k-1] * Abs( mx[k+1] - mx[k]) + mx[k] * Abs( mx[k-1] - mx[k-2] )) /
                     (Abs(mx[k+1] - mx[k]) + Abs(mx[k-1] - mx[k-2])) ;

          if ( Abs(my[k+1] - my[k]  ) = 0.0 ) and
             ( Abs(my[k-1] - my[k-2]) = 0.0 ) then
            ty[k] := (my[k] + my[k-1]) / 2
          else
            ty[k] := (my[k-1] * Abs( my[k+1] - my[k]) + my[k] * Abs( my[k-1] - my[k-2] )) /
                      (Abs(my[k+1] - my[k]) + Abs(my[k-1] - my[k-2])) ;
        end ;

        stepsize := totald / (5*_len - (_len - 4) - 1) ;

        fd := stepsize;
        noutp := 0 ;
        for k := 2 to _len - 4 do begin
          cx := tx[k] ;
          bx := (3.0 * (_inp[k+1].X - _inp[k].X) / d[k] - 2.0 * tx[k] - tx[k+1]) / d[k] ;
          ax := (tx[k] + tx[k+1] - 2 * (_inp[k+1].X - _inp[k].X) / d[k]) / (d[k] * d[k]) ;

          cy := ty[k] ;
          by := (3.0 * (_inp[k+1].Y - _inp[k].Y) / d[k] - 2.0 * ty[k] - ty[k+1]) / d[k];
          ay := (ty[k] + ty[k+1] - 2 * (_inp[k+1].Y - _inp[k].Y) / d[k]) / (d[k] * d[k]);

          outp[noutp].X := _inp[k].X ;
          outp[noutp].Y := _inp[k].Y ;
          outp[noutp].Z := _inp[k].Z ;
          inc( noutp ) ;

          while ( fd < d[k] ) and ( noutp < 5*_len - (_len-k-1) ) do begin
            outp[noutp].X := ax * fd * fd * fd + bx * fd * fd + cx * fd + _inp[k].X ;
            outp[noutp].Y := ay * fd * fd * fd + by * fd * fd + cy * fd + _inp[k].Y ;
            outp[noutp].Z := _inp[k].Z ;
            inc( noutp ) ;
            fd := fd + stepsize ;
          end ;
          fd := fd - d[k] ;
        end ;

        while ( noutp < 5*_len ) do begin
          outp[noutp].X := _inp[_len-3].X ;
          outp[noutp].Y := _inp[_len-3].Y ;
          outp[noutp].Z := _inp[_len-3].Z ;
          inc( noutp ) ;
        end ;

        for k := 0 to noutp-1 do
          addPointInternal( outp[k] ) ;
     end ;

  begin
    if not assigned( currStream ) then exit ;

    currStream.Position := 0 ;
    cplxList  := TGIS_ShapeList.Create( False ) ;
    topo      := TGIS_Topology.Create ;
    num_poles := 0;

    try
      while currStream.Position < currStream.Size do
      begin
        if num_poles = 0 then
          cursorDgn[0].currShape     := nil ;
        if cplxCellCnt = 0 then
          dgnCellActive := False ;
        oldPos        := currStream.Position ;
        readHeader ;
        if elmInvisible then
          elmType := 255;  // ignore this graphic element
        if ignoreGraph then begin
          if ( elmType <> GIS_DGNT_GROUP_DATA       ) and
             ( elmType <> GIS_DGNT_APPLICATION_ELEM ) and
             ( elmType <> GIS_DGNT_TCB              ) and
             ( elmType <> GIS_DGNT_TABLE_ENTRY      ) and
             ( elmType <> GIS_DGNT_TABLE            ) then
            elmType := 255 ;
        end ;

        // enumerate and read graphics and non graphics elements
        case elmType of

          GIS_DGNT_LINE :
            begin
              line.A.X := readX ;
              line.A.Y := readY ;
              if elmProperty3D then
                line.A.Z := readZ ;

              line.B.X := readX ;
              line.B.Y := readY ;
              if elmProperty3D then
                line.B.Z := readZ ;

              if GisIsSamePoint3D( line.A, line.B ) then
                prepareShape( TGIS_ShapeType.Point )
              else
                prepareShape( TGIS_ShapeType.Arc );

                addPointInternal( line.A );
                addPointInternal( line.B );
              unprepareShape ;

              buildComplex ;

              readAttributeLinkages ;
            end ;

          GIS_DGNT_MULTI_LINE :
            begin
              readDouble ;
              readDouble ;
              readInteger ;
              nPoints  := readWord ;
              version  := readByte ;
              nLines   := readByte ;
              readByte ;
              nBreaks  := readByte ;
              readWord ;
              readByte ;
              readByte ;
              readByte ;
              readByte ;
              readDouble ;
              currStream.Position := currStream.Position + 8 ;
              readDouble ;
              currStream.Position := currStream.Position + 20 ;
              currStream.Position := currStream.Position + 20 ;
              currStream.Position := currStream.Position + 20 ;
              readInteger ;
              readInteger ;
              readInteger ;
              zVector.X := readX ;
              zVector.Y := readY ;
              zVector.Z := readZ ;

              SetLength( mlsymbarr, nLines ) ;

              for p := 0 to nLines-1 do begin
                mlsymbarr[p].dist := readDouble ;
                readInteger ;
                iflags := readInteger ;
                mlsymbarr[p].use_style  := (iflags and (1 shl 16)) <> 0 ;
                mlsymbarr[p].use_weight := (iflags and (1 shl 17)) <> 0 ;
                mlsymbarr[p].use_color  := (iflags and (1 shl 18)) <> 0 ;
                mlsymbarr[p].style      := readInteger ;
                mlsymbarr[p].weight     := readInteger ;
                mlsymbarr[p].color      := readInteger ;
                mlsymbarr[p].level      := readInteger ;

                byLev := False ;
                if (mlsymbarr[p].use_color  and ( mlsymbarr[p].color  = -1  )) or
                   (mlsymbarr[p].use_weight and ( mlsymbarr[p].weight  = -1 )) then begin
                  for i := 0 to length( tabLevels ) - 1 do
                    if tabLevels[ i ].levelId = elmLevel then begin
                      if mlsymbarr[p].color = -1 then
                        mlsymbarr[p].color := tabLevels[ i ].elmColor  ;
                      if mlsymbarr[p].weight = -1 then
                        mlsymbarr[p].weight := tabLevels[ i ].elmWeight  ;
                      byLev := True ;
                      break ;
                    end ;
                  if not byLev then begin
                    if mlsymbarr[p].color = -1 then
                      mlsymbarr[p].color  := elmGraphColor ;
                    if mlsymbarr[p].weight = -1 then
                      mlsymbarr[p].weight := elmGraphWeight ;
                  end;
                end ;
              end ;

              SetLength( inp, nPoints ) ;
              for p := 0 to nPoints-1 do begin
                inp[p].X := readX ;
                inp[p].Y := readY ;
                inp[p].Z := readZ ;
                readWord ;
                readWord ;
                readWord ;
                readWord ;
                readDouble ;
              end ;

              for i := 0 to nLines-1 do begin
                d := mlsymbarr[i].dist * dgnScale ;
                if mlsymbarr[i].dist <> 0 then begin
                  prepareShape( TGIS_ShapeType.Arc );

                  for p := 0 to nPoints-1 do
                    addPointInternal( inp[p] ) ;

                  if not IgnoreShapeParams then begin
                    if mlsymbarr[i].use_weight then
                      cursorDgn[0].currShape.Params.Line.Width := getWeight( mlsymbarr[i].weight ) ;
                    if mlsymbarr[i].use_style then
                      cursorDgn[0].currShape.Params.Line.Style := getStyle ( mlsymbarr[i].style  ) ;
                    if mlsymbarr[i].use_color then
                      cursorDgn[0].currShape.Params.Line.Color := getColor( mlsymbarr[i].color ) ;

                    cursorDgn[0].currShape.Params.Line.WidthAsText := Format( 'SIZE: %fm', [Abs(d)] ) ;
                  end ;
                  unprepareShape ;
                  break ;
                end ;
              end ;

              for b := 0 to nBreaks-1 do begin
                readInteger ;
                readInteger ;
                readDouble ;
                readDouble ;
                readDouble ;
              end ;

              buildComplex ;
              readAttributeLinkages ;

            end ;

          GIS_DGNT_CURVE :
            begin
              {$IFDEF OXYGENE}
                currStream.ReadInteger( cnt, 4 ) ;
                currStream.ReadInteger( res, 4 ) ;
              {$ELSE}
                currStream.ReadBuffer( cnt, 4 ) ;
                currStream.ReadBuffer( res, 4 ) ;
              {$ENDIF}

              if elmType = 6 then
                prepareShape( TGIS_ShapeType.Polygon )
              else
                prepareShape( TGIS_ShapeType.Arc );

              SetLength( inp, cnt ) ;
              for i := 0 to cnt - 1 do begin
                line.A.X := readX ;
                line.A.Y := readY ;
                if elmProperty3D then
                  line.A.Z := readZ
                else
                  line.A.Z := 0 ;
                inp[i] := _TGIS_Point3D( line.A ) ;
              end ;

              if ( cnt < 6 ) then begin
                for i := 0 to cnt - 1 do
                  addPointInternal( inp[i] ) ;
              end
              else
                draw_curve( inp, cnt ) ;

              unprepareShape ;

              buildComplex ;

              readAttributeLinkages;
            end ;

          GIS_DGNT_BSPLINE :
            begin
              {$IFDEF OXYGENE}
                currStream.ReadInteger( cnt, 4 ) ;
                currStream.ReadInteger( res, 4 ) ;
              {$ELSE}
                currStream.ReadBuffer( cnt, 4 ) ;
                currStream.ReadBuffer( res, 4 ) ;
              {$ENDIF}
              SetLength( inp, cnt ) ;
              for i := 0 to cnt - 1 do begin
                line.A.X := readX ;
                line.A.Y := readY ;
                if elmProperty3D then
                  line.A.Z := readZ
                else
                  line.A.Z := 0 ;

                inp[ i ] := _TGIS_Point3D( line.A ) ;
              end;
              if num_poles = 0 then
                num_poles := 4 ;

              if assigned( cursorDgn[0].currShape ) then begin
                for i := 0 to cnt-1 do
                  addPointInternal( inp[ i ] ) ;

                unprepareShape ;
                cursorDgn[0].currShape.Smooth( 25, False ) ;
              end ;
              num_poles := 0;
              buildComplex;
              readAttributeLinkages;
            end ;

          GIS_DGNT_LINE_STRING :
            begin
              {$IFDEF OXYGENE}
                currStream.ReadInteger( cnt, 4 ) ;
                currStream.ReadInteger( res, 4 ) ;
              {$ELSE}
                currStream.ReadBuffer( cnt, 4 ) ;
                currStream.ReadBuffer( res, 4 ) ;
              {$ENDIF}

              if elmType = 6 then
                prepareShape( TGIS_ShapeType.Polygon )
              else
                prepareShape( TGIS_ShapeType.Arc );

              if elmType = 11 then begin
                for i := 0 to cnt - 1 do begin
                  line.A.X := readX ;
                  line.A.Y := readY ;
                  if elmProperty3D then
                    line.A.Z := readZ
                  else
                    line.A.Z := 0 ;

                  if ( i >=2 ) and ( i <= cnt - 3 ) then
                   addPointInternal( line.A );
                end ;
              end
              else begin
                for i := 0 to cnt - 1 do begin
                  line.A.X := readX ;
                  line.A.Y := readY ;
                  if elmProperty3D then
                    line.A.Z := readZ
                  else
                    line.A.Z := 0 ;

                  addPointInternal( line.A )
               end ;
              end ;

              unprepareShape ;
              buildComplex ;

              readAttributeLinkages;
            end ;

          GIS_DGNT_SHAPE :
             begin
              {$IFDEF OXYGENE}
                currStream.ReadInteger( cnt, 4 ) ;
                currStream.ReadInteger( res, 4 ) ;
              {$ELSE}
                currStream.ReadBuffer( cnt, 4 ) ;
                currStream.ReadBuffer( res, 4 ) ;
              {$ENDIF}

               if elmType = 6 then begin
                 if elmProperty3D then
                   prepareShape( TGIS_ShapeType.MultiPatch )
                 else
                   prepareShape( TGIS_ShapeType.Polygon ) ;
               end
               else
                 prepareShape( TGIS_ShapeType.Arc );

               if elmType = 11 then begin
                 for i := 0 to cnt - 1 do begin
                   line.A.X := readX ;
                   line.A.Y := readY ;
                   if elmProperty3D then
                     line.A.Z := readZ
                    else
                      line.A.Z := 0 ;

                   if ( i >=2 ) and ( i <= cnt - 3 ) then
                     addPointInternal( line.A );
                 end ;
               end
               else begin
                 for i := 0 to cnt - 1 do begin
                   line.A.X := readX ;
                   line.A.Y := readY ;
                   if elmProperty3D then
                     line.A.Z := readZ
                    else
                      line.A.Z := 0 ;

                   addPointInternal( line.A );
                end ;
               end ;

               unprepareShape ;

               buildComplex ;

               readAttributeLinkages;
            end ;

          GIS_DGNT_GROUP_DATA :
            begin
              if elmLevel = GIS_DGN_GDL_COLOR_TABLE then begin
                {$IFDEF OXYGENE}
                  currStream.ReadInteger( res, 2 ) ;
                {$ELSE}
                  currStream.ReadBuffer( res, 2 ) ;
                {$ENDIF}
                {$IFDEF OXYGENE}
                  currStream.ReadBuffer( dgnColorTable, 768 );
                {$ELSE}
                  currStream.ReadBuffer( dgnColorTable[ 0 ], 768 );
                {$ENDIF}
              end;
              currStream.Position := oldPos + 2 * elmSize + 4 ;
            end ;

          GIS_DGNT_APPLICATION_ELEM :
            begin
              // non model element
              currStream.Position := oldPos + 2 * elmSize + 4 ;
            end ;

          GIS_DGNT_ELLIPSE :
            begin
              axis.X := readDouble * dgnScale ;
              axis.Y := readDouble * dgnScale ;

              if not elmProperty3D then
                {$IFDEF OXYGENE}
                  currStream.ReadDouble( rotAngle, 8 )
                {$ELSE}
                  currStream.ReadBuffer( rotAngle, 8 )
                {$ENDIF}
              else begin
                {$IFDEF OXYGENE}
                  currStream.ReadDouble( quat[ 0 ], 8 ) ;
                  currStream.ReadDouble( quat[ 1 ], 8 ) ;
                  currStream.ReadDouble( quat[ 2 ], 8 ) ;
                  currStream.ReadDouble( quat[ 3 ], 8 ) ;
                {$ELSE}
                  currStream.ReadBuffer( quat[ 0 ], 8 ) ;
                  currStream.ReadBuffer( quat[ 1 ], 8 ) ;
                  currStream.ReadBuffer( quat[ 2 ], 8 ) ;
                  currStream.ReadBuffer( quat[ 3 ], 8 ) ;
                {$ENDIF}
                convertQueternion( False, rotAngle ) ;
              end;

              origin.X := readX ;
              origin.Y := readY ;
              if elmProperty3D then
                origin.Z := readZ
              else
                origin.Z := 0 ;

              prepareShape( TGIS_ShapeType.Arc );
                draw_arc( origin, axis, rotAngle, 0, 2*Pi ) ;
              unprepareShape ;

              buildComplex ;

              readAttributeLinkages ;
            end ;

          GIS_DGNT_ARC :
            begin
              {$IFDEF OXYGENE}
                currStream.ReadDouble( start_angle, 8 ) ;
                currStream.ReadDouble( sweep_angle, 8 ) ;
              {$ELSE}
                currStream.ReadBuffer( start_angle, 8 ) ;
                currStream.ReadBuffer( sweep_angle, 8 ) ;
              {$ENDIF}
              axis.X := readDouble * dgnScale ;
              axis.Y := readDouble * dgnScale ;
              if not elmProperty3D then
                {$IFDEF OXYGENE}
                  currStream.ReadDouble( rotAngle, 8 )
                {$ELSE}
                  currStream.ReadBuffer( rotAngle, 8 )
                {$ENDIF}
              else begin
                {$IFDEF OXYGENE}
                  currStream.ReadDouble( quat[ 0 ], 8 ) ;
                  currStream.ReadDouble( quat[ 1 ], 8 ) ;
                  currStream.ReadDouble( quat[ 2 ], 8 ) ;
                  currStream.ReadDouble( quat[ 3 ], 8 ) ;
                {$ELSE}
                  currStream.ReadBuffer( quat[ 0 ], 8 ) ;
                  currStream.ReadBuffer( quat[ 1 ], 8 ) ;
                  currStream.ReadBuffer( quat[ 2 ], 8 ) ;
                  currStream.ReadBuffer( quat[ 3 ], 8 ) ;
                {$ENDIF}
                convertQueternion( False, rotAngle ) ;
              end ;
              origin.X := readX ;
              origin.Y := readY ;
              if elmProperty3D then
                origin.Z := readZ
              else
                origin.Z := 0 ;

              prepareShape( TGIS_ShapeType.Arc );
                draw_arc( origin,
                          axis,
                          rotAngle,
                          start_angle,
                          start_angle+sweep_angle
                        ) ;
              unprepareShape ;
              buildComplex ;
              readAttributeLinkages ;
            end ;

          GIS_DGNT_CELL_HEADER :
            begin
              // elements count
              {$IFDEF OXYGENE}
                currStream.ReadInteger( cplxCellCnt, 4 ) ;
                currStream.ReadInteger( res, 2 ) ;
                currStream.ReadInteger( res, 2 ) ;
              {$ELSE}
                currStream.ReadBuffer( cplxCellCnt, 4 ) ;
                currStream.ReadBuffer( res, 2 ) ;
                currStream.ReadBuffer( res, 2 ) ;
              {$ENDIF}
              dgnCellActive := True ;

              line.A.X := readX ;
              line.A.Y := readY ;
              if elmProperty3D then
                line.A.Z := readZ
              else
                line.A.Z := 0 ;

              line.B.X := readX ;
              line.B.Y := readY ;
              if elmProperty3D then
                line.B.Z := readZ
              else
                line.B.Z := 0 ;

              if elmProperty3D then
                currStream.Position := currStream.Position + 72
              else
                currStream.Position := currStream.Position + 32 ;

              // global origin
              cellOrigin.X := readX ;
              cellOrigin.Y := readY ;
              if elmProperty3D then
                cellOrigin.Z := readZ
              else
                cellOrigin.Z := 0 ;

              readAttributeLinkages ;
            end ;

          GIS_DGNT_TEXT :
            begin
              {$IFDEF OXYGENE}
                currStream.ReadInteger( res, 4  ) ;
                currStream.ReadWord( just, 2 ) ;
                currStream.ReadWord( num, 2  ) ;
                currStream.ReadDouble( lm, 8 ) ;
                currStream.ReadDouble( hm, 8 ) ;
                currStream.ReadDouble( l, 8 ) ;
                currStream.ReadDouble( h, 8 ) ;
              {$ELSE}
                currStream.ReadBuffer( res, 4  ) ;
                currStream.ReadBuffer( just, 2 ) ;
                currStream.ReadBuffer( num, 2  ) ;
                currStream.ReadBuffer( lm, 8 ) ;
                currStream.ReadBuffer( hm, 8 ) ;
                currStream.ReadBuffer( l, 8 ) ;
                currStream.ReadBuffer( h, 8 ) ;
              {$ENDIF}

              lm := lm * dgnScale * 6.0 / 1000.0 ;
              hm := hm * dgnScale * 6.0 / 1000.0 ;

              if not elmProperty3D then
                {$IFDEF OXYGENE}
                  currStream.ReadDouble( rotAngle, 8 )
                {$ELSE}
                  currStream.ReadBuffer( rotAngle, 8 )
                {$ENDIF}
              else begin
                {$IFDEF OXYGENE}
                  currStream.ReadDouble( quat[ 0 ], 8 ) ;
                  currStream.ReadDouble( quat[ 1 ], 8 ) ;
                  currStream.ReadDouble( quat[ 2 ], 8 ) ;
                  currStream.ReadDouble( quat[ 3 ], 8 ) ;
                {$ELSE}
                  currStream.ReadBuffer( quat[ 0 ], 8 ) ;
                  currStream.ReadBuffer( quat[ 1 ], 8 ) ;
                  currStream.ReadBuffer( quat[ 2 ], 8 ) ;
                  currStream.ReadBuffer( quat[ 3 ], 8 ) ;
                {$ENDIF}
                convertQueternion( False, rotAngle ) ;
              end;

              line.A.X := readX ;
              line.A.Y := readY ;
              if elmProperty3D then
                line.A.Z := readZ
              else
                line.A.Z := 0 ;

              {$IFDEF OXYGENE}
                currStream.ReadInteger( res, 2 ) ;
              {$ELSE}
                currStream.ReadBuffer( res, 2 ) ;
              {$ENDIF}

              if ( num > 0 ) then begin
                SetLength( txt_arr, num ) ;
                {$IFDEF OXYGENE}
                  currStream.Read( txt_arr, num ) ;
                {$ELSE}
                  currStream.ReadBuffer( txt_arr[0], num ) ;
                {$ENDIF}
              end ;

              isUnicode := False ;
              res := 0 ;
              if ( num > 2 ) then begin
                isUnicode := ( txt_arr[0] = $ff ) and ( txt_arr[1] = $fe ) ;
                if isUnicode and ( num > 4 ) then
                  if ( txt_arr[2] = 1 ) and ( txt_arr[3] = 0 ) then begin
                    isUnicode := False ;
                    res := 4 ;
                  end ;
              end ;

              if isUnicode then begin
                {$IFDEF JAVA}
                  text := TEncoding.UTF16LE.GetString( txt_arr, 2, num-2 ) ;
                {$ELSE}
                  text := TEncoding.Unicode.GetString( txt_arr, 2, num-2 ) ;
                {$ENDIF}
              end
              else begin
                sb := TStringBuilder.Create ;
                try
                  for i := res to num-1 do
                    sb.Append( chr( txt_arr[ i ] ) ) ;
                  text := sb.ToString ;
                finally
                  FreeObject( sb ) ;
                end ;
              end ;

              if text <> GIS_DGN_PATTERN_CONTROL then begin
                prepareShape( TGIS_ShapeType.Point ) ;
                if dgnCellActive then
                  addPointInternal( GisPoint3D( cellOrigin.X, line.A.Y, line.A.Z ) )
                else
                  addPointInternal( line.A ) ;

                if num <> 0 then
                  with cursorDgn[0].currShape do begin
                    if isColorNearBackground( elmGraphColor, 16 ) then begin
                      Params.Labels.Color := getColor ( elmGraphColor ) ;
                      Params.Labels.FontColor := TGIS_Color.Black ;
                    end
                    else begin
                      Params.Labels.Color := getColor ( elmGraphColor ) ;
                      Params.Labels.FontColor
                        := cursorDgn[0].currShape.Params.Labels.Color ;
                    end ;
                    Params.Marker.Size := 0 ;
                    SetField( GIS_DGN_FLD_LABEL      , text      ) ;
                    SetField( GIS_DGN_FLD_LABEL_ANGLE, -rotAngle ) ;

                    Params.Labels.FontSizeAsText := 'SIZE:' + DotFloatToStr( hm *7/10 ) + 'mu' ;
                    Params.Labels.Width := -999999;
                  end ;
                unprepareShape ;
              end ;

              readAttributeLinkages ;
            end ;

          GIS_DGNT_TCB :
            begin
              // design header - non model element
              currStream.Position := oldPos + 2 * elmSize + 4 ;
            end ;

          GIS_DGNT_TABLE :
            begin
                {$IFDEF OXYGENE}
                  currStream.ReadCardinal( compcnt, 4 );
                {$ELSE}
                  currStream.ReadBuffer( compcnt, 4 );
                {$ENDIF}
                if compcnt > 0 then begin
                  currStream.Position := currStream.Position + 4 ;
                  {$IFDEF OXYGENE}
                    currStream.ReadInt64( attachmentId, 8 );
                  {$ELSE}
                    currStream.ReadBuffer( attachmentId, 8 );
                  {$ENDIF}
                  currStream.Position := currStream.Position + 16 ;
                end
                else begin
                  attachmentId := 0 ;
                  currStream.Position := currStream.Position + 28 ;
                end;

                readAttributeLinkages ;
            end;

          GIS_DGNT_TABLE_ENTRY :
            begin
              // ignore other tables and reference attachments
              if ( elmLevel = 1 ) and ( attachmentId = 0 ) then begin

                cnt := length( tabLevels );
                SetLength( tabLevels, cnt + 1 );

                {$IFDEF OXYGENE}
                  currStream.ReadCardinal( tabLevels[ cnt ].levelId, 4 );
                  currStream.ReadCardinal( tabLevels[ cnt ].parentId, 4 );
                  currStream.ReadWord( tabLevels[ cnt ].version, 2 );
                  currStream.ReadWord( tabLevels[ cnt ].flags, 2 );
                  currStream.ReadInteger( tabLevels[ cnt ].externalId, 4 );

                  // level symbology
                  currStream.ReadInteger( tabLevels[ cnt ].levelStyle,  4 );
                  currStream.ReadCardinal( tabLevels[ cnt ].levelWeight, 4 );
                  currStream.ReadCardinal( tabLevels[ cnt ].levelColor,  4 );
                  currStream.ReadCardinal( tabLevels[ cnt ].levelFillColor,  4 );

                  // element symbology
                  currStream.ReadInteger( tabLevels[ cnt ].elmStyle,  4 );
                  currStream.ReadCardinal( tabLevels[ cnt ].elmWeight, 4 );
                  currStream.ReadCardinal( tabLevels[ cnt ].elmColor,  4 );
                  currStream.ReadCardinal( tabLevels[ cnt ].elmFillColor,  4 );
                {$ELSE}
                  currStream.ReadBuffer( tabLevels[ cnt ].levelId, 4 );
                  currStream.ReadBuffer( tabLevels[ cnt ].parentId, 4 );
                  currStream.ReadBuffer( tabLevels[ cnt ].version, 2 );
                  currStream.ReadBuffer( tabLevels[ cnt ].flags, 2 );
                  currStream.ReadBuffer( tabLevels[ cnt ].externalId, 4 );

                  // level symbology
                  currStream.ReadBuffer( tabLevels[ cnt ].levelStyle,  4 );
                  currStream.ReadBuffer( tabLevels[ cnt ].levelWeight, 4 );
                  currStream.ReadBuffer( tabLevels[ cnt ].levelColor,  4 );
                  currStream.ReadBuffer( tabLevels[ cnt ].levelFillColor,  4 );

                  // element symbology
                  currStream.ReadBuffer( tabLevels[ cnt ].elmStyle,  4 );
                  currStream.ReadBuffer( tabLevels[ cnt ].elmWeight, 4 );
                  currStream.ReadBuffer( tabLevels[ cnt ].elmColor,  4 );
                  currStream.ReadBuffer( tabLevels[ cnt ].elmFillColor,  4 );
                {$ENDIF}

                currStream.Position := currStream.Position + 24 ;

                tabLevels[ cnt ].flagIsHidden := ( tabLevels[ cnt ].flags and 32 ) = 0 ;
                readAttributeLinkages ;
                if not IsStringEmpty( elmStringNameLinkage ) then begin
                  tabLevels[ cnt ].levelName := elmStringNameLinkage ;
                  tabLevels[ cnt ].levelDescription := elmStringDescLinkage ;
                end ;
              end ;
              currStream.Position := oldPos + 2 * elmSize + 4 ;
            end;

          GIS_DGNT_COMPLEX_CHAIN_HEADER :
            begin
              {$IFDEF OXYGENE}
              currStream.ReadInteger( cplxComponentsCnt, 4 ) ;
              currStream.ReadInteger( cnt, 4 ) ;
              {$ELSE}
              currStream.ReadBuffer( cplxComponentsCnt, 4 ) ;
              currStream.ReadBuffer( cnt, 4 ) ;
              {$ENDIF}
              cplxList.Clear ;
              cplxComponentsFillColor := TGIS_Color.Black ;
              cplxComponentsOutColor := getColor( elmGraphColor ) ;
              cplxComponentsType := TGIS_ShapeType.Arc ;
              readAttributeLinkages ;
            end ;

          GIS_DGNT_COMPLEX_SHAPE_HEADER :
            begin
              {$IFDEF OXYGENE}
              currStream.ReadInteger( cplxComponentsCnt, 4 ) ;
              currStream.ReadInteger( cnt, 4 ) ;
              {$ELSE}
              currStream.ReadBuffer( cplxComponentsCnt, 4 ) ;
              currStream.ReadBuffer( cnt, 4 ) ;
              {$ENDIF}
              cplxList.Clear ;
              cplxComponentsFillColor := TGIS_Color.Black ;
              cplxComponentsOutColor := getColor( elmGraphColor ) ;
              cplxComponentsType := TGIS_ShapeType.Polygon ;
              readAttributeLinkages ;
            end ;

          GIS_DGNT_BSPLINE_HEADER :
            begin
              {$IFDEF OXYGENE}
              currStream.ReadInteger( cnt, 4 ) ;
              currStream.ReadInteger( res, 4 ) ;

              currStream.ReadInteger( num_poles, 4 ) ;
              currStream.ReadInteger( num_knots, 4 ) ;

              {$ELSE}
              currStream.ReadBuffer( cnt, 4 ) ;
              currStream.ReadBuffer( res, 4 ) ;

              currStream.ReadBuffer( num_poles, 4 ) ;
              currStream.ReadBuffer( num_knots, 4 ) ;
              {$ENDIF}


              // test spline type
              res := res and $1E0000 ;
              if res > 0 then
                inc( num_poles, 2 ) ;
              prepareShape( TGIS_ShapeType.Arc ) ;
              readAttributeLinkages ;
            end ;
          else begin
              // other unsupported
              currStream.Position := oldPos + 2 * elmSize + 4 ;
          end ;
        end ;

        // if any shape created, add its attributes
        if assigned( cursorDgn[0].currShape ) then begin
          with cursorDgn[0].currShape do begin
            SetField( GIS_DGN_FLD_LEVEL_ID , elmLevel     ) ;
            SetField( GIS_DGN_FLD_COLOR    , elmGraphColor      ) ;
            SetField( GIS_DGN_FLD_WEIGHT   , elmGraphWeight + 1 ) ;
            SetField( GIS_DGN_FLD_STYLE    , elmGraphStyle      ) ;
          end ;

        end ;

      end ;
    finally
      FreeObject( cplxList ) ;
      FreeObject( topo     ) ;
    end
  end ;

  function TGIS_LayerDGN.fetchElement
    : Boolean ;
  var
    header : array [0..3] of Byte ;
  begin
    Result := False ;

    // read element header
       {$IFNDEF OXYGENE}
         if ( dgnFile.Size - dgnFile.Position        <  Sizeof(header) ) or
            ( dgnFile.Read( header, Sizeof(header) ) <> Sizeof(header) ) then
       {$ELSE}
         if ( dgnFile.Size - dgnFile.Position <  4 ) or
            ( dgnFile.Read( header, 4 )       <> 4 ) then
       {$ENDIF}
       begin
         isFileEof := True ;
         exit ;
       end ;

    // is this an $ffff end of file marker ?
       if ( header[0] = $ff ) and ( header[1] = $ff ) then begin
         isFileEof := True ;
         exit ;
       end ;

    // get element attributes from header
       elmSize    := ( ( header[3] * 256 ) + header[2] ) * 2  ;
       elmType    := header [1] and $7f ;
       elmLevel   := header [0] and $3f ;
       elmComplex := ( header [0] and $80 ) <> 0 ;
       elmDeleted := ( header [1] and $80 ) <> 0 ;

    // read whole element
        SetLength( elmBuffer, elmSize ) ;
       {$IFDEF OXYGENE}
         dgnFile.Read( elmBuffer, elmSize ) ;
       {$ELSE}
         dgnFile.Read( elmBuffer[0], elmSize ) ;
       {$ENDIF}
       elmBufferPos := 0 ;

    Result := True ;
  end ;

  function TGIS_LayerDGN.getStyle(
    const _idx : Integer
  ) : TGIS_PenStyle ;
  begin
    case _idx of
      GIS_DGNS_SOLID                : Result := TGIS_PenStyle.Solid       ;
      GIS_DGNS_DOTTED               : Result := TGIS_PenStyle.Dot         ;
      GIS_DGNS_MEDIUM_DASH          : Result := TGIS_PenStyle.Dash        ;
      GIS_DGNS_LONG_DASH            : Result := TGIS_PenStyle.Dash        ;
      GIS_DGNS_DOT_DASH             : Result := TGIS_PenStyle.DashDot     ;
      GIS_DGNS_SHORT_DASH           : Result := TGIS_PenStyle.Dash        ;
      GIS_DGNS_DASH_DOUBLE_DOT      : Result := TGIS_PenStyle.DashDotDot  ;
      GIS_DGNS_LONG_DASH_SHORT_DASH : Result := TGIS_PenStyle.DashDot     ;
      else                            Result := TGIS_PenStyle.Solid       ;
    end
  end ;

  function TGIS_LayerDGN.getPosition(
    const _idx : Integer
  ) : TGIS_LabelPosition ;
  begin
    case _idx of
     GIS_DGNJ_LEFT_TOP             :  Result := TGIS_LabelPosition.UpLeft ;
     GIS_DGNJ_LEFT_CENTER          :  Result := TGIS_LabelPosition.MiddleLeft ;
     GIS_DGNJ_LEFT_BOTTOM          :  Result := TGIS_LabelPosition.DownLeft ;
     GIS_DGNJ_CENTER_TOP           :  Result := TGIS_LabelPosition.UpCenter ;
     GIS_DGNJ_CENTER_CENTER        :  Result := TGIS_LabelPosition.MiddleCenter ;
     GIS_DGNJ_CENTER_BOTTOM        :  Result := TGIS_LabelPosition.DownCenter ;
     GIS_DGNJ_RIGHT_TOP            :  Result := TGIS_LabelPosition.UpRight ;
     GIS_DGNJ_RIGHT_CENTER         :  Result := TGIS_LabelPosition.MiddleRight ;
     GIS_DGNJ_RIGHT_BOTTOM         :  Result := TGIS_LabelPosition.DownRight ;
    else                              Result := TGIS_LabelPosition.MiddleCenter;
    end;
  end;

  function TGIS_LayerDGN.readByte
    : Byte ;
  begin
    if blV7 then begin
      if Integer(elmBufferPos + GIS_DGNTT_BYTE) <= length( elmBuffer ) then
        Result := elmBuffer[ elmBufferPos ]
      else
        Result := 0;

      elmBufferPos := elmBufferPos + GIS_DGNTT_BYTE ;
    end
    else
      {$IFDEF OXYGENE}
      currStream.ReadByte( Result, 1 );
      {$ELSE}
      currStream.ReadBuffer( Result, 1 );
      {$ENDIF}
  end ;

  function TGIS_LayerDGN.readString(
    const _cnt : Integer
  ) : String ;
  var
    buf     : TBytes ;
    bufenc  : TBytes ;
    cnt     : Integer ;
    i,j     : Integer ;
    n       : Integer ;
    w       : Word ;
    enc     : TEncoding ;
  begin

      Result := '' ;

      buf := TBytes( Copy( elmBuffer, elmBufferPos, _cnt ) ) ;
      cnt := length( buf ) ;
      if cnt > 0 then begin
        // multi-byte text
        if (cnt > 1) and (buf[0] = $FF) and (buf[1] = $FD) then begin
          n := cnt div 2 ;
          SetLength( bufenc, cnt ) ;
          i := 0 ;
          j := 0 ;
          while j < n-1 do begin
            w := (buf[2+j*2] ) or (buf[2+j*2+1] shl 8) ;
            if w < 256 then begin
              bufenc[i] := w and $FF ;
              inc( i ) ;
            end
            else begin
              bufenc[i] := w shr 8 ;
              inc( i ) ;
              bufenc[i] := w and $FF ;
              inc( i ) ;
            end ;
            inc( j ) ;
          end ;
          SetLength( bufenc, i ) ;

          try
            enc := nil ;
            try
              {$IFDEF JAVA}
                var cp := TCodePageConverter.Convert(CodePage) ;
                if not IsStringEmpty(cp) then
                  enc := TEncoding.GetEncoding( cp ) ;
              {$ELSE}
                enc := TEncoding.GetEncoding( CodePage ) ;
              {$ENDIF}
              if assigned( enc ) then
                Result := enc.GetString( bufenc )
            except
              Result := TEncoding.UTF8.GetString( bufenc ) ;
            end ;
          finally
            FreeObject( enc ) ;
          end ;
        end
        else
          Result := TEncoding.UTF8.GetString( buf ) ;
      end ;

    inc( elmBufferPos, _cnt ) ;
  end ;

  procedure TGIS_LayerDGN.convertQueternion(
    const _isInt : Boolean ;
      var _angle : Double
  ) ;
  var
    q     : array [0..3] of Double ;
    angle,
    sine,
    vz    : Double ;
    mag   : Integer ;
  begin
    if _isInt then
      mag := 31
    else
      mag := 0 ;

    q[ 0 ] := ( 1.0 * quat[ 0 ] / (1 shl mag) ) ;  // w
    q[ 1 ] := ( 1.0 * quat[ 1 ] / (1 shl mag) ) ;  // x
    q[ 2 ] := ( 1.0 * quat[ 2 ] / (1 shl mag) ) ;  // y
    q[ 3 ] := ( 1.0 * quat[ 3 ] / (1 shl mag) ) ;  // z

    angle := 2 * ArcCos( q[ 0 ] ) ;
    sine  := Sqrt( 1 - q[ 0 ] * q[ 0 ] ) ;

    if ( sine < 0.0005 ) then
      vz := q[ 3 ]
    else
      vz := q[ 3 ] / sine;

   if vz <> 0 then
     angle := angle *vz ;

    _angle := -angle ;
  end ;

  function TGIS_LayerDGN.readWord
    : Word ;
  begin
    if blV7 then begin
      if Integer(elmBufferPos + GIS_DGNTT_WORD) <= length( elmBuffer ) then
        Result := Word( ( elmBuffer[ elmBufferPos+0 ]       ) or
                        ( elmBuffer[ elmBufferPos+1 ] shl 8 )
                      )
      else
        Result := 0;

      elmBufferPos := elmBufferPos + GIS_DGNTT_WORD ;
    end
    else
      {$IFDEF OXYGENE}
      currStream.ReadWord( Result, 2 );
      {$ELSE}
      currStream.ReadBuffer( Result, 2 );
      {$ENDIF}
  end ;

  function TGIS_LayerDGN.readInteger
    : Integer ;
  begin
    if blV7 then begin
      if Integer(elmBufferPos + GIS_DGNTT_INTEGER) <= length( elmBuffer ) then
        Result := Integer( (elmBuffer[elmBufferPos + 2]       ) or
                           (elmBuffer[elmBufferPos + 3] shl 8 ) or
                           (elmBuffer[elmBufferPos + 0] shl 16) or
                           (elmBuffer[elmBufferPos + 1] shl 24)
                         )
      else
        Result := 0;

      elmBufferPos := elmBufferPos + GIS_DGNTT_INTEGER ;
    end
    else
      {$IFDEF OXYGENE}
      currStream.ReadInteger( Result, 4 );
      {$ELSE}
      currStream.ReadBuffer( Result, 4 );
      {$ENDIF}
  end ;

  function TGIS_LayerDGN.readDouble
    : Double ;
  var
    {$IFDEF OXYGENE}
      lo,hi     : LongWord ;
      vax_bytes : array [0..7] of Byte ;
    {$ELSE}
      dt        : T_IntAndDouble ;
    {$ENDIF}
    sign      : LongWord ;
    exponent  : LongWord ;
    rndbits   : LongWord ;
  begin
    if blV7 then begin

      {$IFDEF OXYGENE}

        // change bytes order - little endian?
        vax_bytes[7] := elmBuffer[ elmBufferPos+1 ] ;
        vax_bytes[6] := elmBuffer[ elmBufferPos+0 ] ;
        vax_bytes[5] := elmBuffer[ elmBufferPos+3 ] ;
        vax_bytes[4] := elmBuffer[ elmBufferPos+2 ] ;
        vax_bytes[3] := elmBuffer[ elmBufferPos+5 ] ;
        vax_bytes[2] := elmBuffer[ elmBufferPos+4 ] ;
        vax_bytes[1] := elmBuffer[ elmBufferPos+7 ] ;
        vax_bytes[0] := elmBuffer[ elmBufferPos+6 ] ;

        // save sign of double
        hi := BitConverter.ToUInt32( vax_bytes, 4 ) ;
        sign := hi and MSB ;

        exponent := hi shr 23 ;

        //adjust the exponent
        exponent := exponent and LS8B ;
        if ( exponent <> 0 ) then
          exponent := exponent + 1023 - 129 ;

        // save the bits that we are discarding so we can RoundS properly
        lo := BitConverter.ToUInt32( vax_bytes, 0 ) ;
        rndbits := lo and LS3B ;
        lo := lo shr 3 ;
        lo := ( lo and LS29B ) or (hi shl 29) ;
        if ( rndbits <>0 ) then
          lo := (lo or LSB) ;

        // shift the hi-order int over 3 and insert the exponent and sign
        hi := hi shr 3 ;
        hi := hi and LS20B;
        hi := hi or ( exponent shl 20 ) or sign;

        // return the value
        vax_bytes[0] := Byte( lo ) ;
        vax_bytes[1] := Byte( lo shr 8 ) ;
        vax_bytes[2] := Byte( lo shr 16 ) ;
        vax_bytes[3] := Byte( lo shr 24 ) ;
        vax_bytes[4] := Byte( hi ) ;
        vax_bytes[5] := Byte( hi shr 8 ) ;
        vax_bytes[6] := Byte( hi shr 16 ) ;
        vax_bytes[7] := Byte( hi shr 24 ) ;
        Result := BitConverter.ToDouble( vax_bytes, 0 ) ;

      {$ELSE}

        // change bytes order - little endian?
        dt.ByteArr[7] := elmBuffer[ elmBufferPos+1 ] ;
        dt.ByteArr[6] := elmBuffer[ elmBufferPos+0 ] ;
        dt.ByteArr[5] := elmBuffer[ elmBufferPos+3 ] ;
        dt.ByteArr[4] := elmBuffer[ elmBufferPos+2 ] ;
        dt.ByteArr[3] := elmBuffer[ elmBufferPos+5 ] ;
        dt.ByteArr[2] := elmBuffer[ elmBufferPos+4 ] ;
        dt.ByteArr[1] := elmBuffer[ elmBufferPos+7 ] ;
        dt.ByteArr[0] := elmBuffer[ elmBufferPos+6 ] ;

        // save sign of double
        sign := (dt.Hi and MSB) ;
        exponent := dt.Hi shr 23 ;

        //adjust the exponent
        exponent := exponent and LS8B ;
        if ( exponent <> 0 ) then
          exponent := exponent + 1023 - 129 ;

        // save the bits that we are discarding so we can RoundS properly
        rndbits := dt.Lo and LS3B ;
        dt.Lo := dt.Lo shr 3 ;
        dt.Lo := ( dt.Lo and LS29B ) or (dt.Hi shl 29) ;
        if ( rndbits <>0 ) then
          dt.Lo := (dt.Lo or LSB) ;

        // shift the hi-order int over 3 and insert the exponent and sign
        dt.Hi := dt.Hi shr 3 ;
        dt.Hi := dt.Hi and LS20B;
        dt.Hi := dt.Hi or ( exponent shl 20 ) or sign;

        // return the value
        Result := dt.Real64;

      {$ENDIF}

      if ( Abs( Result ) > 1.0e20 ) then
        Result := 0 ;

      elmBufferPos := elmBufferPos + GIS_DGNTT_DOUBLE ;
    end
    else
      {$IFDEF OXYGENE}
      currStream.ReadDouble( Result, 8 );
      {$ELSE}
      currStream.ReadBuffer( Result, 8 );
      {$ENDIF}
  end;

  function TGIS_LayerDGN.readX
    : Double ;
  begin
    if blV7 then
      Result := readInteger
    else
      {$IFDEF OXYGENE}
      currStream.ReadDouble( Result, 8 );
      {$ELSE}
      currStream.ReadBuffer( Result, 8 );
      {$ENDIF}

    Result := ( Result - dgnGlobalOrigin.X ) * dgnScale ;
  end ;

  function TGIS_LayerDGN.readY
    : Double ;
  begin
    if blV7 then
      Result := readInteger
    else
      {$IFDEF OXYGENE}
      currStream.ReadDouble( Result, 8 );
      {$ELSE}
      currStream.ReadBuffer( Result, 8 );
      {$ENDIF}

    Result := ( Result - dgnGlobalOrigin.Y ) * dgnScale ;
  end ;

  function TGIS_LayerDGN.readZ
    : Double ;
  begin
    if blV7 then begin
      if dgn3d then Result := readInteger
               else Result := 0;
      Result := ( Result - dgnGlobalOrigin.Z ) * dgnScale ;
    end
    else begin
      {$IFDEF OXYGENE}
      currStream.ReadDouble( Result, 8 );
      {$ELSE}
      currStream.ReadBuffer( Result, 8 );
      {$ENDIF}
      Result := ( Result - dgnGlobalOrigin.Z ) * dgnScale ;
    end;
  end ;

  function TGIS_LayerDGN.readXExt
  : Double ;
  begin
    Result := readXExt( False ) ;
  end ;

  function TGIS_LayerDGN.readXExt(
    const Extent : Boolean
  ) : Double ;
  const
    mask = $80000000 ;
  begin
    if Extent then
      Result := ( readInteger xor (Integer(mask)) )
    else
      Result := readInteger  ;
    Result := ( Result - dgnGlobalOrigin.X ) * dgnScale ;
  end;

  function TGIS_LayerDGN.readYExt
  : Double ;
  begin
    Result := readYExt( False ) ;
  end ;

  function TGIS_LayerDGN.readYExt(
    const Extent : Boolean
  ) : Double ;
  const
    mask = $80000000 ;
  begin
    if Extent then
      Result := ( readInteger xor (Integer(mask)) )
    else
      Result := readInteger  ;
    Result := ( Result - dgnGlobalOrigin.Y ) * dgnScale ;
  end;

  function TGIS_LayerDGN.readZExt
  : Double ;
  begin
    Result := readZExt( False ) ;
  end ;

  function TGIS_LayerDGN.readZExt(
    const Extent : Boolean
  ) : Double ;
  const
    mask = $80000000 ;
  begin
    if Extent then
      Result := ( readInteger xor (Integer(mask)) )
    else
      Result := readInteger  ;
    Result := ( Result - dgnGlobalOrigin.Z ) * dgnScale ;
  end;

  procedure TGIS_LayerDGN.readCore ;
  var
    i            : Integer ;
    itmp         : Cardinal ;
    wtmp         : Word    ;
    attr_type    : Word    ;
    attr_subtype : Word    ;
    pos_end      : Cardinal ;
    fldname      : String  ;
  begin
    // clear all Entity and MSLink elements
    for i := 0 to elmAttrCount - 1 do begin
      elmAttrEntity[ i ] := 0 ;
      elmAttrMSLink[ i ] := 0 ;
    end ;

    // clear all Entity and MSLink complex elements
    if not dgnComplexFound then begin
      for i := 0 to elmComplexAttrCount - 1 do begin
        elmMSAttrEntity[ i ] := 0 ;
        elmMSAttrMSLink[ i ] := 0 ;
      end ;
    end;

    elmAttrCount := 0 ;

    // read element properties
    if (elmSize >= 32) and not isCellFile then begin

       elmBufferPos    := 24 ;
       elmGraphicGroup := readWord ;

       elmBufferPos  := 28 ;
       elmProperties := readWord ;
       elmClass      := elmProperties and GIS_DGNPF_CLASS ;
       elmInvisible  := (elmProperties and GIS_DGNPF_INVISIBLE) <> 0 ;

       wtmp      := readByte ;
       elmStyle  := wtmp and $07;
       elmWeight := ( wtmp and $F8 ) shr 3 ;
       elmColor  := readByte + 1 ;
    end ;

    if ( elmProperties and GIS_DGNPF_ATTRIBUTES ) > 0 then begin
       elmBufferPos := 26 ;
       itmp := readWord * 2 ;
       elmAttrSize := elmSize - itmp - 28 ;
       elmAttrPos   := itmp + 28 ;
    end
    else begin
      elmAttrSize := 0 ;
      elmAttrPos  := 0 ;
    end ;

    elmFillColor := 0 ;

    elmBufferPos := elmAttrPos ;
    pos_end :=  elmBufferPos + elmAttrSize ;
    while elmBufferPos < pos_end do begin
      attr_type := readWord and $7FFF ;

      case attr_type of
        $0000 :
          begin // 4 words element (one already read)
            elmAttrEntity[elmAttrCount] := readWord ;
            wtmp := readWord ;
            elmAttrMSLink[elmAttrCount] := ( ( readWord and $00FF ) shl 16 ) +
                                           wtmp ;
            if ( elmAttrEntity[elmAttrCount] <> 0) and
               ( elmAttrMSLink[elmAttrCount] <> 0) then
              inc( elmAttrCount );

          end ;
        $1007 :
          begin // 8 words element (one already read)
            attr_subtype := readWord ;
            case attr_subtype of
              $0041 : begin
                        readWord ;
                        readWord ;
                        elmFillColor := readByte ;
                        readByte ;

                        readWord ;
                        readWord ;
                        readWord ;
                      end ;
              else    begin
                        wtmp := readWord ;

                        { TODO  -cCompatibility : Investigate what if not $0F81 }
                        if wtmp <> $0F81 then
                          break ;

                        elmAttrEntity[elmAttrCount] := readWord ;
                        wtmp := readWord ;
                        elmAttrMSLink[elmAttrCount] := ( readWord shl 16 ) +
                                                       wtmp ;
                        readWord ;
                        readWord ;
                        if ( elmAttrEntity[elmAttrCount] <> 0) and
                           ( elmAttrMSLink[elmAttrCount] <> 0) then
                          inc( elmAttrCount );

                      end ;
            end ;
          end ;
      end ;
    end ;

    elmBufferPos := 32 ;

    // remember complex header element attributes for its subelements
    if dgnComplexFound and not elmComplex then begin
      elmMSLevel := elmLevel;
      elmComplexAttrCount := elmAttrCount;
      for i := 0 to elmAttrCount - 1 do begin
        elmMSAttrEntity [ i ] := elmAttrEntity [ i ] ;
        elmMSAttrMSLink [ i ] := elmAttrMSLink [ i ] ;
      end ;
   end;

    while elmAttrCount > dgnAttrCount do begin
      fldname := Format( '%s%.2x',[ GIS_DGN_FLD_ENTITY, dgnAttrCount ] ) ;
      if FindField( fldname ) < 0 then
        AddFieldInternal( fldname, TGIS_FieldType.Number,  5, 0 ) ;

      fldname := Format( '%s%.2x',[ GIS_DGN_FLD_MSLINK, dgnAttrCount ] ) ;
      if FindField( fldname ) < 0 then
        AddFieldInternal( fldname, TGIS_FieldType.Number, 10, 0 ) ;
      inc( dgnAttrCount ) ;
    end ;
  end ;

  function TGIS_LayerDGN.readShape(
    const _cursor : Integer
  ) : Boolean ;
  var
    i         : Integer    ;
    cnt       : Integer    ;
    ptg       : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF} ;
    ptg2      : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF} ;
    {$IFDEF OXYGENE}
    {$ELSE}
      ptrA    : Pointer    ;
      ptrB    : Pointer    ;
    {$ENDIF}
    arcstart  : Double     ;
    arcstop   : Double     ;
    itmp      : Integer    ;
    rotation  : Double     ;
    rot_angle : Integer    ;
    bkmrk_pos : Integer    ;
    bkmrk_uid : Integer    ;
    Position  : Integer    ;
    inp       : array of TGIS_Point3D;

    procedure nextUid ;
    var
      k : Integer ;
    begin
      inc( readUid ) ;

      if readUid mod GIS_BOOKMARK_BLOCKSIZE = 0 then begin
        k := readUid div GIS_BOOKMARK_BLOCKSIZE ;
        if (k+1) > high( cursorDgn[_cursor].arBookmarkPos ) then begin
          SetLength( cursorDgn[_cursor].arBookmarkPos, k + 1 ) ;
          SetLength( cursorDgn[_cursor].arBookmarkUid, k + 1 ) ;
        end ;

        if cellActive then begin
          // bookmark last cell header
          cursorDgn[_cursor].arBookmarkPos[ k ] := cellBookmarkPos ;
          cursorDgn[_cursor].arBookmarkUid[ k ] := cellBookmarkUid ;
        end
        else begin
          if bkmrk_uid = 20480 then
            bkmrk_uid := 20480 ;
            cursorDgn[_cursor].arBookmarkPos[ k ] := bkmrk_pos ;
            cursorDgn[_cursor].arBookmarkUid[ k ] := bkmrk_uid ;
          end ;
      end ;
    end ;

    // set line element with default values
    procedure set_line ;
    begin
      nextUid ;
      if dgn3d then
        cursorDgn[_cursor].currArc.Recreate( nil, nil, False, readUid, self, TGIS_DimensionType.XYZ )
      else
        cursorDgn[_cursor].currArc.Recreate( nil, nil, False, readUid, self ) ;

      cursorDgn[_cursor].currShape := cursorDgn[_cursor].currArc ;

      cursorDgn[_cursor].currShape.Lock( TGIS_Lock.Internal ) ;

      if not IgnoreShapeParams then begin

          cursorDgn[_cursor].currShape.Params.Line.SmartSize := 0 ;
          cursorDgn[_cursor].currShape.Params.Line.Width := getWeight( elmWeight ) ;
          cursorDgn[_cursor].currShape.Params.Line.Style := getStyle ( elmStyle  ) ;

          if isColorNearBackground( elmColor, 8 ) then begin
            cursorDgn[_cursor].currShape.Params.Line.Color := TGIS_Color.Black ;
            cursorDgn[_cursor].currShape.Params.Line.OutlineColor := getColor( elmColor ) ;

            if ( cursorDgn[_cursor].currShape.Params.Line.Color <>
                 cursorDgn[_cursor].currShape.Params.Line.OutlineColor ) then
              cursorDgn[_cursor].currShape.Params.Line.OutlineWidth := 1 ;
          end
          else
            cursorDgn[_cursor].currShape.Params.Line.Color := getColor( elmColor ) ;

      end ;

      cursorDgn[_cursor].currShape.Reset ;
      cursorDgn[_cursor].currShape.AddPart ;
    end ;

    // set point element with default values
    procedure set_point ;
    begin
      nextUid ;
      if dgn3d then
        cursorDgn[_cursor].currPoint.Recreate( nil, nil, False, readUid, self, TGIS_DimensionType.XYZ )
      else
        cursorDgn[_cursor].currPoint.Recreate( nil, nil, False, readUid, self ) ;

      cursorDgn[_cursor].currShape := cursorDgn[_cursor].currPoint ;

      cursorDgn[_cursor].currShape.Lock( TGIS_Lock.Internal ) ;

      if not IgnoreShapeParams then begin
        with cursorDgn[_cursor].currShape.Params do begin
          Marker.SmartSize    := 0                 ;
          Marker.Size         := 90                ;
          Labels.Field        := GIS_DGN_FLD_LABEL ;
          Labels.Width        := -1000             ;
          Labels.Color        := TGIS_Color.Black           ;
          Labels.OutlineWidth := 0                 ;
          Labels.Pattern      := TGIS_BrushStyle.Clear ;
        end ;
      end ;

      cursorDgn[_cursor].currShape.Reset ;
      cursorDgn[_cursor].currShape.AddPart ;
    end ;

    // set polygon element with default values
    procedure set_polygon ;
    begin
      nextUid ;
      if dgn3d then
        cursorDgn[_cursor].currPolygon.Recreate( nil, nil, False, readUid, self, TGIS_DimensionType.XYZ )
      else
        cursorDgn[_cursor].currPolygon.Recreate( nil, nil, False, readUid, self ) ;
      cursorDgn[_cursor].currShape := cursorDgn[_cursor].currPolygon ;

      cursorDgn[_cursor].currShape.Lock( TGIS_Lock.Internal ) ;

      if not IgnoreShapeParams then begin

          cursorDgn[_cursor].currShape.Params.Area.SmartSize := 0 ;
          cursorDgn[_cursor].currShape.Params.Area.OutlineColor := getColor ( elmColor  ) ;
          cursorDgn[_cursor].currShape.Params.Area.OutlineWidth := getWeight( elmWeight ) ;
          cursorDgn[_cursor].currShape.Params.Area.OutlineStyle := getStyle ( elmStyle  ) ;

          if elmFillColor = 0 then
            cursorDgn[_cursor].currShape.Params.Area.Pattern := TGIS_BrushStyle.Clear
          else begin
            cursorDgn[_cursor].currShape.Params.Area.Pattern := TGIS_BrushStyle.Solid ;
            cursorDgn[_cursor].currShape.Params.Area.Color   := getColor ( elmColor  ) ;
          end ;

      end ;

      cursorDgn[_cursor].currShape.Reset ;
      cursorDgn[_cursor].currShape.AddPart ;
    end ;

    // set multiline (chain) element with default values
    procedure set_multiline ;
    begin
      nextUid ;
      if dgn3d then
        cursorDgn[_cursor].currMultiLine.Recreate(
          nil,
          nil,
          False,
          readUid,
          self,
          TGIS_DimensionType.XYZ
        )
      else
        cursorDgn[_cursor].currMultiLine.Recreate(
          nil,
          nil,
          False,
          readUid,
          self
        ) ;

      cursorDgn[_cursor].currMultiLine.Lock( TGIS_Lock.Internal ) ;

      if not IgnoreShapeParams then begin

          cursorDgn[_cursor].currMultiLine.Params.Line.SmartSize := 0 ;
          cursorDgn[_cursor].currMultiLine.Params.Line.Width := getWeight( elmWeight ) ;
          cursorDgn[_cursor].currMultiLine.Params.Line.Style := getStyle ( elmStyle  ) ;

          if isColorNearBackground( elmColor, 8 ) then begin
            cursorDgn[_cursor].currMultiLine.Params.Line.Color := TGIS_Color.Black ;
            cursorDgn[_cursor].currMultiLine.Params.Line.OutlineColor := getColor( elmColor ) ;

            if cursorDgn[_cursor].currMultiLine.Params.Line.Color        <>
               cursorDgn[_cursor].currMultiLine.Params.Line.OutlineColor
            then
              cursorDgn[_cursor].currMultiLine.Params.Line.OutlineWidth := 1 ;
          end
          else
            cursorDgn[_cursor].currMultiLine.Params.Line.Color := getColor( elmColor ) ;


      end ;

      cursorDgn[_cursor].currMultiLine.Reset ;
      cursorDgn[_cursor].currMultiLine.AddPart ;
    end;

    // set multishape element with default values
    procedure set_multishape ;
    begin
      nextUid ;
      if dgn3d then
        cursorDgn[_cursor].currMultiShape.Recreate(
          nil,
          nil,
          False,
          readUid,
          self,
          TGIS_DimensionType.XYZ
        )
      else
        cursorDgn[_cursor].currMultiShape.Recreate(
          nil,
          nil,
          False,
          readUid,
          self
        ) ;
      cursorDgn[_cursor].currMultiShape.Lock( TGIS_Lock.Internal ) ;

      if not IgnoreShapeParams then begin

          cursorDgn[_cursor].currMultiShape.Params.Area.SmartSize := 0 ;
          cursorDgn[_cursor].currMultiShape.Params.Area.OutlineColor := getColor ( elmColor  ) ;
          cursorDgn[_cursor].currMultiShape.Params.Area.OutlineWidth := getWeight( elmWeight ) ;
          cursorDgn[_cursor].currMultiShape.Params.Area.OutlineStyle := getStyle ( elmStyle  ) ;

          if elmFillColor = 0 then
            cursorDgn[_cursor].currMultiShape.Params.Area.Pattern := TGIS_BrushStyle.Clear
          else begin
            cursorDgn[_cursor].currMultiShape.Params.Area.Pattern := TGIS_BrushStyle.Solid ;
            cursorDgn[_cursor].currMultiShape.Params.Area.Color   := getColor ( elmColor  ) ;
          end ;

      end ;

      cursorDgn[_cursor].currMultiShape.Reset ;
      cursorDgn[_cursor].currMultiShape.AddPart ;
    end;

    // Build rotated ellipse
    // Used in rotated lwpolyline
    // _center    center point
    // _axis      axis X and axis Y
    // _roatation rotation angle in radians
    // _start     start angle
    // _stop      stop angle
    procedure draw_arc(
      const _center, _axis           : TGIS_Point3D ;
      const _rotation, _start, _stop : Double
    ) ;
    var
      cnt1        : Integer ;
      absarcangle : Double  ;
      arcangle    : Double  ;
      steps       : Integer ;
      step        : Double  ;
      start       : Double  ;
      nx          : Double  ;
      ny          : Double  ;
      rotate_cos  : Double  ;
      rotate_sin  : Double  ;
      start_cos   : Double  ;
      start_sin   : Double  ;
      radius      : Double  ;
      sqr_x       : Double  ;
      sqr_y       : Double  ;
      sqr_xy      : Double  ;
      sqr_y_x     : Double  ;
      mult        : Double  ;
    const
      MAX_STEPS   = 16      ;
    begin
      arcangle    := _stop-_start ;
      absarcangle := Abs( arcangle ) ;

    // calculate number of segments - and minimize it
       if      absarcangle < Pi/4 then steps := MAX_STEPS div 8
       else if absarcangle < Pi/2 then steps := MAX_STEPS div 4
       else if absarcangle < Pi   then steps := MAX_STEPS div 2
       else steps := MAX_STEPS - 1 ;

       step := arcangle / steps ;

       start := _start + Pi/2 ;
    // rotation
       SinCos( _rotation, rotate_sin, rotate_cos ) ;
    // calculate elliptical arc
       sqr_x := _axis.X * _axis.X ;
       sqr_y := _axis.Y * _axis.Y ;
       sqr_xy :=  sqr_y * sqr_x ;
       sqr_y_x := sqr_y - sqr_x  ;
       for cnt1 := 0 to steps do begin
         SinCos( start, start_sin, start_cos ) ;
         mult := ( sqr_x + ( sqr_y_x )* start_sin * start_sin ) ;
         if mult <> 0 then
           radius := Sqrt( ( sqr_xy  ) / mult )
         else
           radius := 0;
         ny :=  radius * ( start_cos * rotate_cos +
               -  start_sin * rotate_sin );
         nx :=  radius * ( start_cos * rotate_sin  +
                 start_sin * rotate_cos ) ;

        cursorDgn[_cursor].currShape.AddPoint3D(
          GisPoint3D( _center.X+nx, _center.Y-ny, _center.Z )
        ) ;
        start := start + step ;
       end ;
    end ;

  begin // read shape

    Result := False ;

    bkmrk_pos := dgnFile.Position ;
    bkmrk_uid := readUid          ;

    // load raw element
    if not fetchElement then exit ;

    // if element is not deleted
    if not elmDeleted then begin
      case elmType of

        GIS_DGNT_LINE :
          begin
            if elmLevel<>0 then begin
              readCore ;

                ptg.X  := readX ;
                ptg.Y  := readY ;
                if dgn3d then ptg.Z := readZ
                         else ptg.Z := 0 ;
                ptg2.X := readX ;
                ptg2.Y := readY ;
                if dgn3d then ptg2.Z := readZ
                         else ptg2.Z := 0 ;

                if GisIsSamePoint3D( ptg, ptg2 ) then begin
                  set_point ;
                  cursorDgn[_cursor].currShape.AddPoint3D( ptg ) ;
                  cursorDgn[_cursor].currShape.Unlock ;
                  if elmComplex and dgnComplexFound then
                    case dgnComplexType of
                      GIS_DGNT_COMPLEX_SHAPE_HEADER :
                        cursorDgn[_cursor].currMultiShape.AddPoint3D( ptg ) ;
                      GIS_DGNT_COMPLEX_CHAIN_HEADER :
                        cursorDgn[_cursor].currMultiLine.AddPoint3D( ptg ) ;
                    end;

                end
                else begin
                  set_line ;
                  cursorDgn[_cursor].currShape.AddPoint3D( ptg  ) ;
                  cursorDgn[_cursor].currShape.AddPoint3D( ptg2 ) ;
                  if elmComplex and dgnComplexFound then begin
                    case dgnComplexType of
                      GIS_DGNT_COMPLEX_SHAPE_HEADER :
                        begin
                          cursorDgn[_cursor].currMultiShape.AddPoint3D( ptg ) ;
                          cursorDgn[_cursor].currMultiShape.AddPoint3D( ptg2 ) ;
                        end ;
                      GIS_DGNT_COMPLEX_CHAIN_HEADER :
                        begin
                          cursorDgn[_cursor].currMultiLine.AddPoint3D( ptg ) ;
                          cursorDgn[_cursor].currMultiLine.AddPoint3D( ptg2 ) ;
                        end ;
                    end;
                  end;
                  cursorDgn[_cursor].currShape.Unlock ;
                end ;
                if elmComplex then
                  dec (dgnComplexCount);
                if elmComplex and (dgnComplexCount=0) then
                  case dgnComplexType of
                    GIS_DGNT_COMPLEX_SHAPE_HEADER :
                      cursorDgn[_cursor].currMultiShape.Unlock ;
                    GIS_DGNT_COMPLEX_CHAIN_HEADER :
                      cursorDgn[_cursor].currMultiLine.Unlock ;
                  end;
                Result := True ;

            end;
            cellActive := False ;
          end ;

        GIS_DGNT_MULTI_LINE :
          begin
            cellActive := False ;
          end;

        GIS_DGNT_CURVE :
          begin
            if elmLevel<>0 then begin
              readCore ;

               set_line ;
               cnt := readWord ;
               readInteger;
               readInteger;
               readInteger;
               readInteger;
               for i := 0 to cnt -5 do begin
                 if dgn3d then begin
                   ptg.X := readX ;
                   ptg.Y := readY ;
                   ptg.Z := readZ ;
                 end
                 else begin
                   ptg.X := readX ;
                   ptg.Y := readY ;
                 end ;

                 cursorDgn[_cursor].currShape.AddPoint3D( ptg ) ;
                 if elmComplex and dgnComplexFound then
                   case dgnComplexType of
                     GIS_DGNT_COMPLEX_SHAPE_HEADER :
                       cursorDgn[_cursor].currMultiShape.AddPoint3D( ptg ) ;
                     GIS_DGNT_COMPLEX_CHAIN_HEADER :
                       cursorDgn[_cursor].currMultiLine.AddPoint3D( ptg ) ;
                   end;
                 end ;
                 readInteger;
                 readInteger;
                 readInteger;
                 readInteger;
                 cursorDgn[_cursor].currShape.Unlock ;
                 if elmComplex then
                   dec (dgnComplexCount);
                 if elmComplex and (dgnComplexCount=0) then begin
                   case dgnComplexType of
                     GIS_DGNT_COMPLEX_SHAPE_HEADER :
                       cursorDgn[_cursor].currMultiShape.Unlock ;
                     GIS_DGNT_COMPLEX_CHAIN_HEADER :
                       cursorDgn[_cursor].currMultiLine.Unlock ;
                   end;
                 end;
                 Result := True ;

             end;
             cellActive := False ;
          end ;

        GIS_DGNT_BSPLINE :
          begin
            if elmLevel<>0 then begin
              readCore ;

                set_line ;
                cnt := readWord ;
                SetLength(inp,cnt);
                for i := 0 to cnt -1 do begin
                  ptg.X := readX ;
                  ptg.Y := readY ;
                  if dgn3d then ptg.Z := readZ
                           else ptg.Z := 0 ;
                  inp[i] := _TGIS_Point3D( ptg ) ;
                end ;
                for i := 0 to cnt -1 do
                  cursorDgn[_cursor].currShape.AddPoint3D( inp[i] ) ;

                cursorDgn[_cursor].currShape.Unlock ;
                cursorDgn[_cursor].currShape.Smooth( 25, False ) ;
                Result := True ;

            end;
            cellActive := False ;
          end ;

        GIS_DGNT_LINE_STRING :
          begin
            if elmLevel<>0 then begin
               readCore ;

                set_line ;

                cnt := readWord ;
                for i := 0 to cnt -1 do begin
                  ptg.X := readX ;
                  ptg.Y := readY ;
                  if dgn3d then ptg.Z := readZ
                           else ptg.Z := 0 ;
                  cursorDgn[_cursor].currShape.AddPoint3D( ptg ) ;
                  if elmComplex and dgnComplexFound then
                    case dgnComplexType of
                      GIS_DGNT_COMPLEX_SHAPE_HEADER :
                        cursorDgn[_cursor].currMultiShape.AddPoint3D( ptg ) ;
                      GIS_DGNT_COMPLEX_CHAIN_HEADER :
                        cursorDgn[_cursor].currMultiLine.AddPoint3D( ptg ) ;
                    end;
                end ;
                cursorDgn[_cursor].currShape.Unlock ;
                if elmComplex then
                  dec (dgnComplexCount);
                if elmComplex and (dgnComplexCount=0) then begin
                  case dgnComplexType of
                    GIS_DGNT_COMPLEX_SHAPE_HEADER :
                      cursorDgn[_cursor].currMultiShape.Unlock ;
                    GIS_DGNT_COMPLEX_CHAIN_HEADER :
                      cursorDgn[_cursor].currMultiLine.Unlock ;
                  end;
                end;

                Result := True ;

            end;
            cellActive := False ;
          end ;

        GIS_DGNT_SHAPE :
          begin
            if elmLevel<>0 then begin
              readCore ;

                set_polygon ;
                cnt := readWord ;
                for i := 0 to cnt -1 do begin
                  ptg.X := readX ;
                  ptg.Y := readY ;
                  if dgn3d then ptg.Z := readZ
                           else ptg.Z := 0 ;
                  cursorDgn[_cursor].currShape.AddPoint3D( ptg ) ;
                end ;
                cursorDgn[_cursor].currShape.Unlock ;

                Result := True ;

            end;
            cellActive := False ;
          end ;

        GIS_DGNT_GROUP_DATA :
          begin
            if elmLevel = GIS_DGN_GDL_COLOR_TABLE then begin
              {$IFDEF OXYGENE}
                {$IFDEF JAVA}
                  System.arraycopy( elmBuffer, 34,  dgnColorTable, 0, Length( dgnColorTable ) ) ;
                {$ELSE}
                  System.Buffer.BlockCopy( elmBuffer, 34,
                                           dgnColorTable, 0,
                                           length( dgnColorTable )
                                         ) ;
                {$ENDIF}
              {$ELSE}
                ptrA := Pointer( NativeInt( elmBuffer ) + 34) ;
                ptrB := @dgnColorTable[0] ;
                System.Move( ptrA^, ptrB^, sizeOf( dgnColorTable ) ) ;
              {$ENDIF}
            end ;
            cellActive := False ;
          end ;

        GIS_DGNT_APPLICATION_ELEM :
          begin
            cellActive := False ;
          end ;

        GIS_DGNT_ELLIPSE :
          begin
            if elmLevel<>0 then begin
              readCore ;

                set_line ;
                ptg2.X := readDouble * dgnScale ;
                ptg2.Y := readDouble * dgnScale ;
                if not dgn3d then begin
                  rotation :=
                    DegToRad( ( readInteger * 1.0 ) / 360000.0 ) ;
                  ptg.X    :=
                    ( readDouble - dgnGlobalOrigin.X ) * dgnScale ;
                  ptg.Y    :=
                    ( readDouble - dgnGlobalOrigin.Y ) * dgnScale ;
                end
                else begin
                  rotation := 0 ;

                  // skip 16 bytes
                  readInteger ;
                  readInteger ;
                  readInteger ;
                  readInteger ;

                  ptg.X        := ( readDouble - dgnGlobalOrigin.X )
                                  * dgnScale ;
                  ptg.Y        := ( readDouble - dgnGlobalOrigin.Y )
                                  * dgnScale ;
                  ptg.Z        := ( readDouble - dgnGlobalOrigin.Z )
                                  * dgnScale ;
                end ;
                draw_arc( ptg, ptg2, rotation, 0, 2*Pi ) ;
                cursorDgn[_cursor].currShape.Unlock ;

                Result := True ;

            end;
            cellActive := False ;
          end ;

        GIS_DGNT_ARC :
          begin
            if elmLevel<>0 then begin
              readCore ;

                set_line ;

                arcstart := DegToRad( ( readInteger * 1.0 ) /
                                      360000.0
                                    ) ;
                itmp     := readInteger ;

                if ( itmp and $80000000 ) <> 0 then begin
                  itmp := itmp and $7FFFFFFF ;
                  arcstop :=  -DegToRad( ( itmp * 1.0 ) / 360000.0 ) ;
                end
                else
                  arcstop :=  DegToRad( ( itmp * 1.0 ) / 360000.0 ) ;

                if itmp = 0 then
                  arcstop := 2 * Pi ;

                ptg2.X := readDouble * dgnScale ;
                ptg2.Y := readDouble * dgnScale ;
                if not dgn3d then begin
                  rotation :=
                    DegToRad( ( readInteger * 1.0 ) / 360000.0 ) ;
                  ptg.X    :=
                    ( readDouble - dgnGlobalOrigin.X ) * dgnScale ;
                  ptg.Y    :=
                    ( readDouble - dgnGlobalOrigin.Y ) * dgnScale ;
                end
                else begin
                  rotation := 0 ;

                  // skip 16 bytes
                  quat[ 0 ] := readInteger;
                  quat[ 1 ] := readInteger;
                  quat[ 2 ] := readInteger;
                  quat[ 3 ] := readInteger;

                  ptg.X        := ( readDouble - dgnGlobalOrigin.X )
                                  * dgnScale ;
                  ptg.Y        := ( readDouble - dgnGlobalOrigin.Y )
                                  * dgnScale ;
                  ptg.Z        := ( readDouble - dgnGlobalOrigin.Z )
                                  * dgnScale ;
                  convertQueternion( True, rotation ) ;
                end ;
                draw_arc( ptg, ptg2, rotation,
                          arcstart,arcstart +arcstop
                        ) ;
                cursorDgn[_cursor].currShape.Unlock ;

                Result := True ;

            end;
            cellActive := False ;
          end ;

        GIS_DGNT_CELL_HEADER :
          begin
            if not cellActive then begin
              cellBookmarkPos := bkmrk_pos ;
              cellBookmarkUid := bkmrk_uid ;
            end ;
            readCore ;


              if dgn3d then elmBufferPos := 108
                       else elmBufferPos := 80 ;

              ptg.X := readX ;
              ptg.Y := readY ;
              ptg.Z := readZ ;

              cellOrigin := _TGIS_Point3D( ptg ) ;
              Result := False ;

            cellActive := True ;
          end ;

        GIS_DGNT_TEXT :
          begin
            if elmLevel<>0 then begin
              readCore ;

                set_point ;

                { font_id   := } readByte; // unused result

                Position  := readByte;

                { mult_w    := }           // unused result
                  ( readInteger {* dgnscale * 6.0 / 1000.0} );

                { mult_h    := }           // unused result
                  ( readInteger {* dgnscale * 6.0 / 1000.0} );

                if not dgn3d then
                  rot_angle := (readInteger div 360000)
                else begin
                   { TODO : build and transform quaternion to rotation }
                  // leave quaternion
                  quat[ 0 ] := readInteger;
                  quat[ 1 ] := readInteger;
                  quat[ 2 ] := readInteger;
                  quat[ 3 ] := readInteger;
                  rot_angle := 0;
                end ;

                if not IgnoreShapeParams then begin
                  with cursorDgn[_cursor].currShape.Params do begin
                    Marker.Size := 90 ;

                    if isColorNearBackground( elmColor, 16 ) then begin
                      Labels.Color := getColor ( elmColor ) ;
                      Labels.FontColor := TGIS_Color.Black ;
                    end
                    else begin
                      Labels.Color := getColor ( elmColor ) ;
                      Labels.FontColor
                        := cursorDgn[_cursor].currShape.Params.Labels.Color ;
                    end ;
                    Labels.Position :=
                      GisGetLabelPosition( getPosition (Position) );
                    Labels.Alignment  :=
                      TGIS_LabelAlignment.LeftJustify;
                    Labels.Rotate     :=
                      -DegToRad( rot_angle ) ;
                  end ;
                  cursorDgn[_cursor].currShape.SetField(
                    GIS_DGN_FLD_LABEL_ANGLE,
                    -rot_angle
                  ) ;
                end ;

                if dgn3d then elmBufferPos := 58
                         else elmBufferPos := 46 ;

                ptg.X := readX ;
                ptg.Y := readY ;
                ptg.Z := readZ ;

                if not cellActive then
                  cursorDgn[_cursor].currShape.AddPoint3D( ptg )
                else
                  cursorDgn[_cursor].currShape.AddPoint3D( cellOrigin ) ;

                cnt := readByte ;
                readByte ;

                if cnt <> 0 then begin
                  cursorDgn[_cursor].currShape.SetField( GIS_DGN_FLD_LABEL,
                                      readString( cnt )
                                    ) ;
                  cursorDgn[_cursor].currShape.Params.Marker.Size := 0 ;
                end;
                cursorDgn[_cursor].currShape.Unlock ;

                Result := True ;

            end ;
            cellActive := False ;
          end ;

        GIS_DGNT_TCB :
          begin
            readCore ;

            elmBufferPos         := 1108                ;
            dgnSubunitsPerMaster := readInteger         ;
            elmBufferPos         := 1112                ;
            dgnUorPerSubunit     := readInteger         ;

            if ( dgnSubunitsPerMaster <> 0 ) and
               ( dgnUorPerSubunit     <> 0 ) then
            begin
              elmBufferPos         := 1116                ;
              dgnMasterUnits       := readString( 2 )     ;
              dgnSubUnits          := readString( 2 )     ;

              elmBufferPos         := 1236                ;
              {$IFDEF GIS_NORECORDS}
                dgnGlobalOrigin := new TGIS_Point3D ;
              {$ENDIF}
              dgnGlobalOrigin.X    := readDouble          ;
              dgnGlobalOrigin.Y    := readDouble          ;
              dgnGlobalOrigin.Z    := readDouble          ;

              dgnScale := 1.0 / ( dgnUorPerSubunit *
                                  dgnSubunitsPerMaster
                                ) ;
            end ;
            cellActive := False ;
          end ;

        GIS_DGNT_COMPLEX_CHAIN_HEADER :
          begin
            dgnComplexFound := true;
            readCore;

              elmComplexSize  := readWord;
              dgnComplexCount := readWord;
              set_multiline;
              dgnComplexType := GIS_DGNT_COMPLEX_CHAIN_HEADER;
              dgnComplexUsed := false;

            cellActive := False ;
          end ;

        GIS_DGNT_COMPLEX_SHAPE_HEADER :
          begin
            dgnComplexFound := true;
            readCore;

              elmComplexSize  := readWord;
              dgnComplexCount := readWord;
              set_multishape;
              dgnComplexType := GIS_DGNT_COMPLEX_SHAPE_HEADER;
              dgnComplexUsed := false;

            cellActive := False ;
          end ;

        else begin
            // other unsupported
            cellActive := False ;
        end ;
      end ;
    end ;

    // if any shape created, add its attributes
    if assigned( cursorDgn[_cursor].currShape ) then begin
      cursorDgn[_cursor].currShape := getEdited( cursorDgn[_cursor].currShape ) ;
      with cursorDgn[_cursor].currShape do begin
        SetField( GIS_DGN_FLD_LEVEL_ID , elmLevel     ) ;
        SetField( GIS_DGN_FLD_COLOR    , elmColor     ) ;
        SetField( GIS_DGN_FLD_WEIGHT   , elmWeight + 1) ;
        SetField( GIS_DGN_FLD_STYLE    , elmStyle     ) ;

        for i:=0 to elmAttrCount - 1 do begin
          SetField( Format( '%s%.2x', [ GIS_DGN_FLD_ENTITY, i ] ),
                    elmAttrEntity[ i ] ) ;
          SetField( Format( '%s%.2x', [ GIS_DGN_FLD_MSLINK, i ] ),
                    elmAttrMSLink[ i ] ) ;
        end ;
      end ;
      cursorDgn[_cursor].currShape.Params.Ground := Params.Ground ;
    end ;

    // add complex shape as polygon and fill its attributes
    if assigned( cursorDgn[_cursor].currMultiShape ) and
       ( dgnComplexCount = 0 )    and
       elmComplex                 and
       dgnComplexFound            and
       ( dgnComplexType = GIS_DGNT_COMPLEX_SHAPE_HEADER ) then
    begin
      cursorDgn[_cursor].currMultiShape
        := TGIS_ShapePolygon( cursorDgn[_cursor].currMultiShape ) ;

      with cursorDgn[_cursor].currMultiShape do begin
        if elmComplexAttrCount>0 then begin
          SetField( GIS_DGN_FLD_LEVEL_ID , elmMSLevel   ) ;
        end ;
        for i:=0 to elmComplexAttrCount - 1 do begin
          SetField( Format( '%s%.2x', [ GIS_DGN_FLD_ENTITY, i ] ),
                    elmMSAttrEntity[ i ] ) ;
          SetField( Format( '%s%.2x', [ GIS_DGN_FLD_MSLINK, i ] ),
                    elmMSAttrMSLink[ i ] ) ;
        end ;
        dgnComplexUsed := true;
      end ;
      dgnComplexFound     := false;
      elmComplexAttrCount := 0;
    end ;

    // add complex shape as linestring and fill its attributes
    if assigned( cursorDgn[_cursor].currMultiLine ) and
       ( dgnComplexCount = 0   ) and
       elmComplex                and
       dgnComplexFound           and
       ( dgnComplexType = GIS_DGNT_COMPLEX_CHAIN_HEADER ) then
    begin
      cursorDgn[_cursor].currMultiLine
        := TGIS_ShapeArc( cursorDgn[_cursor].currMultiLine ) ;

      with cursorDgn[_cursor].currMultiLine do begin
        if elmComplexAttrCount>0 then begin
          SetField( GIS_DGN_FLD_LEVEL_ID , elmMSLevel   ) ;
        end ;
        for i:=0 to elmComplexAttrCount - 1 do begin
          SetField( Format( '%s%.2x', [ GIS_DGN_FLD_ENTITY, i ] ),
                    elmMSAttrEntity[ i ] ) ;
          SetField( Format( '%s%.2x', [ GIS_DGN_FLD_MSLINK, i ] ),
                    elmMSAttrMSLink[ i ] ) ;
        end ;
        dgnComplexUsed := true;
      end ;
      dgnComplexFound     := false;
      elmComplexAttrCount := 0;
    end ;
  end ;

  function TGIS_LayerDGN.fget_PathRTree
    : String ;
  begin
    Result := ExpandForcedRTreePath(
                Path + GIS_RTREE_EXT
              ) ;
  end ;

  procedure TGIS_LayerDGN.setUp ;
  var
    header  : array [0..3] of Byte ;
    {$IFDEF OXYGENE}
      i     : Integer ;
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    stg     : T_StorageFile ;
    stm_lst : TStrings ;

    procedure loadStorage(
      const _stmLst  : TStrings;
      const _root    : T_Storage
    ) ;
    var
      i1, cnt : Integer ;
      stm_fix : TMemoryStream ;
      stm     : TStream ;
    begin
      stm_fix := TMemoryStream.Create;
      try
        for i1 := 0 to _stmLst.Count - 1 do begin
          stm := TStream( stg.OpenStream( _stmLst[ i1 ], _root ) ) ;
          try
            {$IFNDEF OXYGENE}
              stm_fix.Clear ;
            {$ELSE}
              stm_fix.Position := 0 ;
            {$ENDIF}
            // skip predefined stream header
            {$IFNDEF OXYGENE}
              stm.ReadBuffer( cnt, 4 ) ;
            {$ENDIF}
            stm.Position := $10 ;
            cnt := stm.Size - stm.Position ;
            stm_fix.CopyFrom( stm, stm.Size - stm.Position ) ;
            try
              if cnt > 0 then begin
                // decompress stream block
                {$IFDEF OXYGENE}
                  currStream := TGIS_MemoryStream( stg.DecompressStream( stm_fix ) ) ;
                {$ELSE}
                  currStream := TStream( stg.DecompressStream( stm_fix ) ) ;
                {$ENDIF}
                // parse decompressed stream
                parseStream;
              end ;
            finally
              FreeObject( currStream ) ;
            end;
          finally
            FreeObject( stm );
          end;
        end;
      finally
        FreeObject( stm_fix );
      end;
    end;

    procedure loadStream;
    var
      stm : TStream ;
    begin
      stm := stg.ReadRootModelHeader ;
      try
        {$IFDEF OXYGENE}
          currStream := TGIS_MemoryStream( stg.DecompressStream( stm ) ) ;
        {$ELSE}
          currStream := TStream( stg.DecompressStream( stm ) ) ;
        {$ENDIF}
        readRootModelHeader ;
      finally
        FreeObject( currStream );
        FreeObject( stm        );
      end;
    end;
    {$ENDIF}

    procedure prepareLayers ;
    var
      lv        : TGIS_LayerSublayerVector ;
      clvwr, cl : TGIS_Color ;
      r,g,b     : Byte ;
      vr,vg,vb  : Byte ;
      i         : Integer;
    begin
      if not assigned( SubLayers ) then
        SubLayers := TGIS_LayerAbstractList.Create( False ) ;

      for i := 0 to length( tabLevels ) - 1 do begin
        lv := TGIS_LayerSublayerVector.Create ;

        if not IsStringEmpty( tabLevels[i].levelName ) then begin
          lv.Name := tabLevels[i].levelName ;
          if not IsStringEmpty( tabLevels[i].levelDescription ) then
            lv.Caption := tabLevels[i].levelDescription
          else
            lv.Caption := tabLevels[i].levelName ;
        end
        else begin
          lv.Name    := Format( 'Level %d', [tabLevels[i].levelId] ) ;
          lv.Caption := lv.Name ;
        end ;
        lv.Active  := not tabLevels[i].flagIsHidden ;
        lv.Viewer  := Viewer ;

        lv.SupportedShapes := GisGetEmptyShapeType ;
        lv.SupportedShapes := GisAddShapeType( lv.SupportedShapes, TGIS_ShapeType.Point ) ;
        lv.SupportedShapes := GisAddShapeType( lv.SupportedShapes, TGIS_ShapeType.MultiPoint ) ;
        lv.SupportedShapes := GisAddShapeType( lv.SupportedShapes, TGIS_ShapeType.Arc ) ;
        lv.SupportedShapes := GisAddShapeType( lv.SupportedShapes, TGIS_ShapeType.Polygon ) ;

        lv.Params.Assign( self.Params );
        lv.IgnoreShapeParams  := IgnoreShapeParams ;
        lv.UseParentParams    := False ;
        lv.ParentLayer        := self ;
        SubLayers.Add( lv ) ;
        dcLevels.Add( tabLevels[i].levelId, i ) ;
        if assigned( Viewer ) then
          clvwr :=  Viewer.Ref.Color
        else
          clvwr := TGIS_Color.Black ;

        cl := getColor( tabLevels[ i ].elmColor ) ;
        r := cl.R ;
        g := cl.G ;
        b := cl.B ;

        vr := clvwr.R ;
        vg := clvwr.G ;
        vb := clvwr.B ;

        if ( Abs( vr - r ) < 16 ) and
           ( Abs( vg - g ) < 16 ) and
           ( Abs( vb - b ) < 16 )
        then // color very close to the window color
          cl := TGIS_Color.FromRGB( r xor vr, g xor vg, b xor vb ) ;

        lv.Params.Marker.SmartSize := 0 ;
        lv.Params.Marker.Color      := cl ;
        lv.Params.Marker.Size       := 1;
        lv.Params.Line.Color        := cl;
        lv.Params.Line.Style        := getStyle( tabLevels[ i ].elmStyle );
        lv.Params.Line.Width        := getWeight( tabLevels[ i ].elmWeight );
        lv.Params.Area.Color        := cl;
        lv.Params.Area.OutlineColor := cl;
        lv.Params.Area.OutlineStyle := lv.Params.Line.Style;
        lv.Params.Ground            := TGIS_3DGroundType.AboveZero ;
        lv.Params.Labels.Alignment  := TGIS_LabelAlignment.LeftJustify ;
        lv.Params.Labels.Allocator  := False ;
        lv.Params.Labels.Duplicates := True ;
        lv.Params.Labels.Color      := lv.Params.Marker.Color ;
        lv.Params.Labels.FontColor  := lv.Params.Marker.Color ;
        lv.Params.Labels.Position   := GisGetLabelPosition( TGIS_LabelPosition.UpRight ) ;
        lv.Params.Labels.SmartSize  := 0 ;
        lv.Params.Labels.Field      := GIS_DGN_FLD_LABEL ;
      end ;

    end;

    procedure prepareColorTable ;
    var
      i : Integer ;
    begin
      for i := low(dgnDefaultColorTable) to high(dgnDefaultColorTable) do
        dgnColorTable[i] := dgnDefaultColorTable[i] ;
    end ;

  begin
    inherited ;

    blV7    := True ;
    dgnFile := TGIS_BufferedFileStream.Create( Path, TGIS_StreamMode.Read ) ;

    // test if it is ISFF file (DGN or CELL)
     {$IFNDEF OXYGENE}
       dgnFile.Read( header, SizeOf( header ) ) ;
     {$ELSE}
       dgnFile.Read( header, 4 ) ;
     {$ENDIF}
     if ( ( header[0] and $0F ) <> GIS_DGN_HEADER_IDX0  ) or
        ( ( header[1]           <> GIS_DGN_HEADER_IDX1  ) and
          ( header[1]           <> GIS_CELL_HEADER_IDX1 )
        ) or
        ( ( header[2]           <> GIS_DGN_HEADER_IDX2  ) and
          ( header[2]           <> GIS_CELL_HEADER_IDX2 )
        ) or
        ( ( header[3]           <> GIS_DGN_HEADER_IDX3  ) and
          ( header[3]           <> GIS_CELL_HEADER_IDX3 )
        ) then
     begin
       blV7 := False;
       FreeObject( dgnFile ) ;
     end ;

    // clear color table
     {$IFDEF OXYGENE}
       for i := 0 to high(dgnColorTable) do
        dgnColorTable[i] := 0 ;
     {$ELSE}
       FillChar( dgnColorTable, Sizeof( dgnColorTable ), 0 ) ;
     {$ENDIF}

     dgnAttrCount := 0 ;

    // field set-ip
     if FindField( GIS_DGN_FLD_LEVEL_ID )    < 0 then
       AddFieldInternal( GIS_DGN_FLD_LEVEL_ID   , TGIS_FieldType.Number, 12, 0 ) ;
     if FindField( GIS_DGN_FLD_LABEL ) < 0 then
       AddFieldInternal( GIS_DGN_FLD_LABEL      , TGIS_FieldType.String,  5, 0 ) ;
     if FindField( GIS_DGN_FLD_LABEL_ANGLE ) < 0 then
       AddFieldInternal( GIS_DGN_FLD_LABEL_ANGLE, TGIS_FieldType.Number,  5, 6 ) ;
     if FindField( GIS_DGN_FLD_COLOR )       < 0 then
       AddFieldInternal( GIS_DGN_FLD_COLOR      , TGIS_FieldType.Number,  5, 0 ) ;
     if FindField( GIS_DGN_FLD_STYLE )       < 0 then
       AddFieldInternal( GIS_DGN_FLD_STYLE      , TGIS_FieldType.Number,  5, 0 ) ;
     if FindField( GIS_DGN_FLD_WEIGHT )      < 0 then
       AddFieldInternal( GIS_DGN_FLD_WEIGHT     , TGIS_FieldType.Number,  5, 0 ) ;

    prepareColorTable ;

    if blV7 then begin

      lastUid := -1 ;

      // basic set-up, check if object is 3D type
       dgn3d      := header[0] = GIS_DGN_3D_HEADER_ID ;
       dgnScale   := 1 ;
       isCellFile := ( header[1] = GIS_CELL_HEADER_IDX1) and
                     ( header[2] = GIS_CELL_HEADER_IDX2) and
                     ( header[3] = GIS_CELL_HEADER_IDX3) ;
       if dgn3d then
        DefaultDimension := TGIS_DimensionType.XYZ ;
    end
    else begin
    {$IFDEF MSWINDOWS}
      FSubType := FSubType + [ TGIS_LayerSubType.InMemory ] ;

      Lock ;
      try
        try
          stg := T_StorageFile.Create ;
          try
            stg.OpenStorageFile( Path );

            // parse non-model storage elements
            stm_lst := stg.ReadNonModelHeader;
            try
              ignoreGraph := True ;
              loadStorage( stm_lst, stg.FNStg );
            finally
              ignoreGraph := False ;
              FreeObject( stm_lst );
            end ;

            prepareLayers ;

            stm_lst := stg.ReadModelRootStorage;
            try
              // parse global settings
              loadStream ;
              // parse model storage elements
              loadStorage( stm_lst, stg.FGStg );
            finally
              FreeObject( stm_lst );
            end;
          finally
            FreeObject( stg );
          end;
        except
          raise ;
        end ;
      finally
        Unlock ;
        FIsModified := False ;
      end;
      {$ENDIF}
    end;

    if SafeFileExists( Path ) then
      FAge := GisFileAge( Path ) ;

    FFileInfo := 'MicroStation Native Format (DGN)' ;
  end ;


  function TGIS_LayerDGN.GetShape(
    const _uid    : TGIS_Uid ;
    const _cursor : Integer
  ) : TGIS_Shape ;
  var
    i : Integer ;
  begin
    lockThread ;
    try
      if blV7 then begin
        Result := nil ;
        if _uid <= 0 then exit ;

        // if it is in edited list
        Result := inherited GetShape( _uid, _cursor ) ;
        if Result <> nil then exit ;

        // is it a current shape
        if ( cursorDgn[_cursor].currShape     <> nil ) and
           ( cursorDgn[_cursor].currShape.Uid = _uid ) then
        begin
          Result := cursorDgn[_cursor].currShape ;
          exit ;
        end ;

        // if no index file, traverse normally
        // find closest bookmark
        i := _uid div GIS_BOOKMARK_BLOCKSIZE ;
        if i > high( cursorDgn[_cursor].arBookmarkPos ) then
          i := high( cursorDgn[_cursor].arBookmarkPos ) ;

        // same as MoveFirst
        elmAttrCount     := 0 ;
        dgnFile.Position := cursorDgn[_cursor].arBookmarkPos[ i ] ;
        readUid          := cursorDgn[_cursor].arBookmarkUid[ i ] ;
        cellActive       := False ;
        isFileEof        := False ;

        inherited cursorStateSave( _cursor ) ;
        try
          cursorDgn[_cursor].currShape := nil ;
          inherited cursorFirstInternal( _cursor, False,
                                         GisWholeWorld, '', nil, '', True
                                       ) ;
          cursorNext( _cursor ) ;

          while not cursorEof( _cursor ) do begin
            if cursorShape(_cursor).Uid = _uid then begin
              Result := cursorShape(_cursor) ;
              exit ;
            end ;
            cursorNext(_cursor) ;
          end ;
        finally
          inherited cursorStateRestore( _cursor ) ;
        end;
      end
      else
        Result := inherited GetShape( _uid, _cursor ) ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerDGN.GetLastUid
    : TGIS_Uid ;
  var
    shp       : TGIS_Shape ;
    old_scope : String     ;
  begin
    lockThread ;
    try
      old_scope := Scope ;
      try
        if lastUid < 0 then begin
          shp := nil ;
          cursorFirst( 0, False,
                       GisWholeWorld, '', nil, '', True
                     ) ;

          while not cursorEof(0) do begin // iterate all shapes
            shp := cursorShape(0) ;
            cursorNext(0) ;
          end ;

          if assigned( shp ) then
            lastUid := shp.Uid
          else
            lastUid := 0 ;
        end ;

        Result := lastUid ;
      finally
        Scope := old_scope ;
      end;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerDGN.GetNewUid
    : TGIS_Uid ;
  begin
    lockThread ;
    try
      GetLastUid ;
      inc( lastUid ) ;
      Result := lastUid ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerDGN.RecalcExtent;
  var
    last_ext : TGIS_Extent;

    procedure find_element_extent ;
    var
      ext : TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF} ;
    begin
      if (elmSize >= 32) and not isCellFile then begin
         elmBufferPos := 0;
        // get element range
         ext.XMin := readXExt ( True ) ;
         ext.YMin := readYExt ( True ) ;
         readZExt ( True ) ;
         ext.XMax := readXExt ( True ) ;
         ext.YMax := readYExt ( True ) ;
         readZExt ( True ) ;
         last_ext := GisMaxExtent( ext, last_ext ) ;
      end;
    end;

  begin
    if Items.Count > 0 then
      inherited
    else begin
      if blV7 then begin

      // custom procedure to calculate layer extent
      // inherited;
        dgnFile.Position := 0 ;
        isFileEof        := False ;
        last_ext         := GisNoWorld;

      while True do begin
        if not isFileEof then begin
          // load raw element
          if not fetchElement then break ;

          // if element is not deleted and to draw
          if not elmDeleted then begin
            if elmType in [ GIS_DGNT_LINE, GIS_DGNT_CURVE, GIS_DGNT_BSPLINE,
                            GIS_DGNT_LINE_STRING, GIS_DGNT_SHAPE,
                            GIS_DGNT_ELLIPSE, GIS_DGNT_ARC,
                            GIS_DGNT_CELL_HEADER, GIS_DGNT_TEXT
                          ]
            then // extract its extent from header
              find_element_extent
            else begin
              // find global origin at start
              if elmType = GIS_DGNT_TCB then begin
                readCore ;

                elmBufferPos         := 1108                ;
                dgnSubunitsPerMaster := readInteger         ;
                elmBufferPos         := 1112                ;
                dgnUorPerSubunit     := readInteger         ;

                if ( dgnSubunitsPerMaster <> 0 ) and
                   ( dgnUorPerSubunit     <> 0 ) then
                begin
                  elmBufferPos         := 1116                ;
                  dgnMasterUnits       := readString( 2 )     ;
                  dgnSubUnits          := readString( 2 )     ;
                  elmBufferPos         := 1236                ;
                  {$IFDEF GIS_NORECORDS}
                    dgnGlobalOrigin    := new TGIS_Point3D ;
                  {$ENDIF}
                  dgnGlobalOrigin.X    := readDouble          ;
                  dgnGlobalOrigin.Y    := readDouble          ;
                  dgnScale := 1.0 / ( dgnUorPerSubunit * dgnSubunitsPerMaster ) ;
                end ;
              end ;
            end ;
          end ;
        end ;
      end ;
      Extent := last_ext ;
    end ;
    end;
  end;

  procedure TGIS_LayerDGN.RevertShapes;
  var
    i : Integer ;
  begin
    inherited ;

    for i := 0 to BUILTIN_CURSORS - 1 do
      cursorDgn[i].currShape := nil ;
  end;

  function TGIS_LayerDGN.cursorOpen
    : Integer ;
  begin
    lockThread ;
    try
      Result := inherited cursorOpen ;

      if Result >= length(cursorDgn )  then
        SetLength( cursorDgn, Result + 1 ) ;
      {$IFDEF GIS_NORECORDS}
        if not assigned( cursorDgn[Result] ) then
          cursorDgn[Result] := new T_cursorDgn ;
      {$ENDIF}

      cursorDgn[Result].curInUse := True ;

      cursorDgn[Result].currPoint      := TGIS_ShapePoint.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorDgn[Result].currMultipoint := TGIS_ShapeMultiPoint.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorDgn[Result].currArc        := TGIS_ShapeArc.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorDgn[Result].currPolygon    := TGIS_ShapePolygon.Create(
                                                nil, nil, False, -1, nil
                                              ) ;
      cursorDgn[Result].currMultiShape := TGIS_ShapePolygon.Create   (
                                                nil, nil, False, -1, nil ) ;
      cursorDgn[Result].currMultiLine  := TGIS_ShapeArc.Create       (
                                                nil, nil, False, -1, nil ) ;

      SetLength( cursorDgn[Result].arBookmarkPos, 1 ) ;
      SetLength( cursorDgn[Result].arBookmarkUid, 1 ) ;
      cursorDgn[Result].arBookmarkPos [ 0 ] := 0 ;
      cursorDgn[Result].arBookmarkUid [ 0 ] := 0 ;
    finally
      unlockThread ;
    end ;
  end;

  procedure TGIS_LayerDGN.cursorClose(
    const _cursor : Integer
  ) ;
  var
    i : Integer ;
  begin
    lockThread ;
    try
      cursorDgn[_cursor].curInUse := False ;
      FreeObject( cursorDgn[_cursor].currPoint      ) ;
      FreeObject( cursorDgn[_cursor].currMultipoint ) ;
      FreeObject( cursorDgn[_cursor].currArc        ) ;
      FreeObject( cursorDgn[_cursor].currPolygon    ) ;
      FreeObject( cursorDgn[_cursor].currMultiShape ) ;
      FreeObject( cursorDgn[_cursor].currMultiLine  ) ;

      // truncate cursorState at the tail;
      for i := length( cursorDgn ) - 1 downto 0 do begin
        if not cursorDgn[i].curInUse then begin
          SetLength( cursorDgn, i ) ;
        end
        else
          break ;
      end ;

      inherited cursorClose( _cursor ) ;
    finally
      unlockThread ;
    end ;
  end;

  procedure TGIS_LayerDGN.cursorFirst(
    const _cursor      : Integer     ;
    const _viewerCS    : Boolean     ;
    const _extent      : TGIS_Extent ;
    const _query       : String      ;
    const _shape       : TGIS_Shape  ;
    const _de9im       : String      ;
    const _skipDeleted : Boolean
  ) ;
  begin
    lockThread ;
    try
      elmAttrCount     := 0 ;
      readUid          := 0 ;
      cellActive       := False ;
      isFileEof        := False ;

      if blV7 then begin
        dgnFile.Position := 0 ;

        cursorDgn[_cursor].currShape := nil ;

        if GisIsNoWorld( _extent ) then
          exit ;

        inherited cursorFirstInternal(
                    _cursor, _viewerCS,
                    _extent, _query, _shape, _de9im, _skipDeleted
                  ) ;

        cursorNext( _cursor ) ;
      end
      else
        inherited cursorFirst(
                    _cursor, _viewerCS,
                    _extent, _query, _shape, _de9im, _skipDeleted
                  ) ;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerDGN.cursorNext(
    const _cursor : Integer
  ) ;
  var
    ihglass : Integer ;
  begin
    lockThread ;
    try
      if blV7 then begin
      // read each shape
         ihglass := 0 ;
         while True do begin
           inc( ihglass ) ;
           if ihglass mod GIS_PROGRESS_TRESHOLD = 0 then begin
             if HourglassShake then begin
               cursorDgn[ _cursor ].currShape := nil ;
               break ;
             end;
           end ;

           if ( assigned ( cursorDgn[_cursor].currMultiShape) or
                assigned (cursorDgn[_cursor].currMultiLine)
              ) and
              dgnComplexUsed
           then  begin
                case dgnComplexType of
                  GIS_DGNT_COMPLEX_SHAPE_HEADER :
                    cursorDgn[_cursor].currShape
                      := getEdited( cursorDgn[_cursor].currMultiShape );
                  GIS_DGNT_COMPLEX_CHAIN_HEADER :
                    cursorDgn[_cursor].currShape
                      := getEdited( cursorDgn[_cursor].currMultiLine  );
                  else
                    begin
                      assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                    end;
                end;

                dgnComplexUsed := false;
           end
           else begin
             cursorDgn[_cursor].currShape := nil ;

             if not isFileEof then
               if not readShape(_cursor) then continue ;
           end;

           if cursorDgn[_cursor].currShape = nil then begin
             // using an xxxInternalVersion may be a bit too secure
             // but better is to be too restrictive than too lose
             if not inherited cursorEofInternal( _cursor ) then begin
               cursorDgn[_cursor].currShape
                 := inherited cursorShapeInternal( _cursor ) ;
               inherited cursorNextInternal( _cursor ) ;
             end ;
             if cursorDgn[_cursor].currShape = nil then exit ;
           end;

           if cursorState[_cursor].curSkipDeleted and
              cursorDgn[_cursor].currShape.IsDeleted then
           begin
             continue ;
           end ;

           if not isInScope( cursorDgn[_cursor].currShape, _cursor ) then
             continue
           else
             exit ;
         end ;
      end
      else
        inherited
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerDGN.cursorEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    if blV7 then
      Result := cursorDgn[_cursor].currShape = nil
    else
      Result := inherited cursorEof( _cursor );
  end ;

  function TGIS_LayerDGN.cursorShape(
    const _cursor : Integer
  ) : TGIS_Shape ;
  begin
    if blV7 and assigned( cursorDgn[_cursor].currShape ) then
       Result := cursorDgn[_cursor].currShape
    else
       Result := inherited cursorShape(_cursor) ;

  end ;


//==============================================================================
// Unit_GisLayerDGN
//==============================================================================

  class procedure Unit_GisLayerDGN.SelfRegisterLayer() ;
  begin
    RegisterLayer( 'DK-DGN', 'MicroStation Native Format', TGIS_LayerDGN, '.dgn',
                   TGIS_RegisteredLayerType.Vector, TGIS_RegisteredFormatType.Local,
                   [ TGIS_RegisteredOperationType.Read ],
                   True
                 ) ;
  end ;


//==============================================================================
// initialization/finalization
//==============================================================================

{$IFDEF DCC}
  initialization
    Unit_GisLayerDGN.SelfRegisterLayer() ;
{$ENDIF}

//==================================== END =====================================
end.
