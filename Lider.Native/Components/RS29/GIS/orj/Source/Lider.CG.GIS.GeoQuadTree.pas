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
  Encapsulation of Quad tree building and searching functions.
}

{$IFDEF DCC}
  unit GisQuadTree ;
  {$HPPEMIT '#pragma link "GisQuadTree"'}
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

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Generics.Collections,
    System.Generics.Defaults,

    GisRtl,
    GIsTypes,
    GisStreams ;
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
  ///   Quad tree item stored in nodes.
  /// </summary>
  TGIS_QuadTreeItem =  {$IFDEF OXYGENE} public {$ENDIF} record
    private
      /// <summary>
      ///   Feature id.
      /// </summary>
      FUid    : Integer ;
      /// <summary>
      ///   Feature extent.
      /// </summary>
      FExtent : TGIS_Extent ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_uid">
      ///   feature id
      /// </param>
      /// <param name="_ext">
      ///   feature extent
      /// </param>
      constructor Create ( const _uid : Integer ;
                           const _ext : TGIS_Extent
                         ) ;
      {$IFDEF GIS_XDK}
        {#gendoc:hide}
         procedure AssignValues(
                           const _uid : Integer ;
                           const _ext : TGIS_Extent
                         ) ;
       {$ENDIF}

    public
      /// <summary>
      ///   Feature id.
      /// </summary>
      property Uid    : Integer     read FUid    ;
      /// <summary>
      ///   Feature extent.
      /// </summary>
      property Extent : TGIS_Extent read FExtent ;


  end ;

  /// <summary>
  ///   Event fired for each item.
  /// </summary>
  /// <param name="_itm">
  ///   node feature
  /// </param>
  /// <param name="_data">
  ///   user data
  /// </param>
  /// <param name="_abort">
  ///   if True, iterating will be aborted
  /// </param>
  {$IFDEF GENXDK}
    TGIS_QuadTreeForEachItemEvent = procedure(
      var _translated : Boolean ;
          _itm        : TGIS_QuadTreeItem ;
          _data       : TObject ;
      var _abort      : Boolean
    ) of object ;
  {$ELSE}
    TGIS_QuadTreeForEachItemEvent = {$IFDEF OXYGENE} public {$ENDIF} procedure(
          _itm    : TGIS_QuadTreeItem ;
          _data   : TObject ;
      var _abort  : Boolean
    ) of object ;
  {$ENDIF}


  /// <summary>
  ///   Quad tree node class.
  /// </summary>
  TGIS_QuadTreeNode = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      /// <summary>
      ///   Extent of the quad tree node.
      /// </summary>
      Extent       : TGIS_Extent ;
      /// <summary>
      ///   Total number of stored features.
      /// </summary>
      NumFeatures  : Integer ;
      /// <summary>
      ///   List of stored features.
      /// </summary>
      Features     : TList<TGIS_QuadTreeItem> ;
      /// <summary>
      ///   Number of quad tree subnodes.
      /// </summary>
      NumSubNodes  : Integer ;
      /// <summary>
      ///   Quad tree subnode.
      /// </summary>
      SubNodes     : array [0..3] of TGIS_QuadTreeNode ;
    private
      /// <summary>
      ///   Calculate size in memory of all subnodes.
      /// </summary>
      /// <param name="_node">
      ///   node to calculate
      /// </param>
      /// <returns>
      ///   Size in memory
      /// </returns>
      function  subNodeOffset( const _node : TGIS_QuadTreeNode
                             ) : Integer ;

    protected
      procedure doDestroy ; override;

    {$IFDEF OXYGENE} unit or {$ENDIF}
    protected
      /// <summary>
      ///   Serialize and write a node to stream.
      /// </summary>
      /// <param name="_fs">
      ///   stream to write
      /// </param>
      /// <param name="_node">
      ///   node to write
      /// </param>
      procedure Write( const _fs   : TGIS_Stream ;
                       const _node : TGIS_QuadTreeNode
                     ) ;
    public
      /// <summary>
      ///   Constructor.
      /// </summary>
      /// <param name="_extent">
      ///   quad tree node extent
      /// </param>
      constructor Create( const _extent : TGIS_Extent
                         ) ;
      /// <summary>
      ///   Clear node.
      /// </summary>
      procedure Clear ;

      /// <summary>
      ///   For each item in tree call an event.
      /// </summary>
      /// <param name="_callback">
      ///   event callback function that will be fired for each item
      /// </param>
      /// <param name="_data">
      ///   user data
      /// </param>
      procedure ForEach( const _callback : TGIS_QuadTreeForEachItemEvent ;
                         const _data     : TObject
                       ) ;
  end ;

  /// <summary>
  ///   Quad tree class.
  /// </summary>
  TGIS_QuadTree = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    private
      /// <summary>
      ///   Path to index file.
      /// </summary>
      path        : String ;
      /// <summary>
      ///   True. if index is in-memory based. False, if index is disk-based.
      /// </summary>
      inMemory    : Boolean ;
      /// <summary>
      ///   Stream (memory or disk based).
      /// </summary>
      streamObj   : TGIS_Stream ;
      /// <summary>
      ///   Place for Magic number.
      /// </summary>
      magic       : Integer ;
      /// <summary>
      ///   Place for version. Different version can be incompatible.
      /// </summary>
      version     : Byte ;
      /// <summary>
      ///   Place for subversion. Different sun versions are compatible.
      /// </summary>
      subversion  : Byte ;
      /// <summary>
      ///   Copyright text.
      /// </summary>
      copyright   : String ;
      /// <summary>
      ///   RT_DIRTY if storage was not saved properly, otherwise RT_COMMITED
      /// </summary>
      commited    : Byte ;
      /// <summary>
      ///   Start position of nodes in stream.
      /// </summary>
      startPos    : Int64 ;
      /// <summary>
      ///   Root node.
      /// </summary>
      root        : TGIS_QuadTreeNode ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      /// <summary>
      ///   Total number of stored features.
      /// </summary>
      FNumFeatures    : Integer ;
      /// <summary>
      ///   Maximum depth of the quad tree.
      /// </summary>
      FMaxDepth       : Integer ;
      /// <summary>
      ///   Maximum number of features stored within a single node.
      /// </summary>
      FBucketCapacity : Integer ;
      /// <summary>
      ///   Ratio between extents resulting from splitting.
      /// </summary>
      FSplitRatio     : Double ;
    protected
      /// <summary>
      ///   Split a region into two subregions, cutting along the longest
      ///   dimension.
      /// </summary>
      /// <param name="_split">
      ///   proportion between the resulting extents
      /// </param>
      /// <param name="_in">
      ///   extent to be split
      /// </param>
      /// <param name="_out1">
      ///   first of the two resulting extents
      /// </param>
      /// <param name="_out2">
      ///   second of the two resulting extents
      /// </param>
      procedure splitBounds    ( const _split     : Double ;
                                 const _in        : TGIS_Extent ;
                                   var _out1      : TGIS_Extent ;
                                   var _out2      : TGIS_Extent
                                ) ;

      /// <summary>
      ///   Add feature.
      /// </summary>
      /// <param name="_node">
      ///   quad tree node
      /// </param>
      /// <param name="_feature">
      ///   feature to be added
      /// </param>
      /// <param name="_extent">
      ///   extent of the feature
      /// </param>
      procedure addFeature     ( const _node      : TGIS_QuadTreeNode ;
                                 const _feature   : TGIS_QuadTreeItem ;
                                 const _extent    : TGIS_Extent
                                ) ; overload;

      /// <summary>
      ///   Add feature.
      /// </summary>
      /// <param name="_node">
      ///   quad tree node
      /// </param>
      /// <param name="_feature">
      ///   feature to be added
      /// </param>
      /// <param name="_extent">
      ///   extent of the feature
      /// </param>
      /// <param name="_maxDepth">
      ///   maximum depth of the quad tree
      /// </param>
      procedure addFeature     ( const _node      : TGIS_QuadTreeNode ;
                                 const _feature   : TGIS_QuadTreeItem ;
                                 const _extent    : TGIS_Extent ;
                                 const _maxDepth  : Integer
                               ) ; overload;

      /// <summary>
      ///   Collect features.
      /// </summary>
      /// <param name="_node">
      ///   quad tree node
      /// </param>
      /// <param name="_extent">
      ///   extent of interest
      /// </param>
      /// <param name="_list">
      ///   list of items within the extent
      /// </param>
      procedure collectFeatures( const _node      : TGIS_QuadTreeNode ;
                                 const _extent    : TGIS_Extent ;
                                 const _list      : TGIS_ListOfIntegers
                                ) ;

      /// <summary>
      ///   Search a node and all subnodes within given extent on disc.
      /// </summary>
      /// <param name="_extent">
      ///   extent to search
      /// </param>
      /// <param name="_list">
      ///   list of results
      /// </param>
      /// <returns>
      ///   True if any feature was found within the extent
      /// </returns>
      function  searchOnDiscNode( const _extent : TGIS_Extent ;
                                  const _list   : TGIS_ListOfIntegers
                                 ) : Boolean ;

      /// <summary>
      ///   Trim nodes.
      /// </summary>
      /// <param name="_node">
      ///   quad tree node
      /// </param>
      /// <returns>
      ///   True if the node is empty
      /// </returns>
      function  trimNodes      ( const _node      : TGIS_QuadTreeNode
                                ) : Boolean ;

      /// <summary>
      ///   Open a tree from disc.
      /// </summary>
      procedure openStream        ;

      /// <summary>
      ///   Initialize tree.
      /// </summary>
      procedure doCreate ;

    protected
      procedure doDestroy ; override;

    public

      /// <summary>
      ///   Constructor of in memory tree.
      /// </summary>
      /// <param name="_extent">
      ///   quad tree extent
      /// </param>
      constructor Create ( const _extent     : TGIS_Extent
                         ) ; overload ;

      /// <summary>
      ///   Constructor of file based tree.
      /// </summary>
      /// <param name="_path">
      ///   path to file
      /// </param>
      constructor Create ( const _path     : String
                         ) ; overload ;

      /// <summary>
      ///   Insert a feature into the tree.
      /// </summary>
      /// <param name="_id">
      ///   feature id
      /// </param>
      /// <param name="_extent">
      ///   feature extent
      /// </param>
      procedure Insert ( const _id      : Integer ;
                         const _extent  : TGIS_Extent
                       ) ;

      /// <summary>
      ///   Search a tree within given extent and return a list with feature id.
      ///   If the tree is file based, results will be estimated and must be
      ///   filtered withint extent.
      /// </summary>
      /// <param name="_extent">
      ///   extent to search
      /// </param>
      /// <returns>
      ///   list of features found within the extent
      /// </returns>
      function  Search ( const _extent  : TGIS_Extent
                       ) : TGIS_ListOfIntegers ;

      /// <summary>
      ///   Clean up tree removing empty nodes.
      /// </summary>
      procedure CleanUp ;

      /// <summary>
      ///   Clear tree.
      /// </summary>
      procedure Clear ;

      /// <summary>
      ///   Calculate max depth of a tree based on features count.
      /// </summary>
      /// <param name="_maxFeatures">
      ///   number of features to store
      /// </param>
      procedure EstimateMaxDepth( const _maxFeatures : Integer
                                 ) ;

      /// <summary>
      ///   For each item in tree call an event.
      /// </summary>
      /// <param name="_callback">
      ///   event callback function that will be fired for each item
      /// </param>
      /// <param name="_data">
      ///   user data
      /// </param>
      procedure ForEach( const _callback : TGIS_QuadTreeForEachItemEvent ;
                         const _data     : TObject
                       ) ;

      /// <summary>
      ///   Save a tree on disc.
      /// </summary>
      /// <param name="_fileName">
      ///   file name to write
      /// </param>
      procedure SaveToFile  ( const _fileName : String
                            ) ;
    public
      /// <summary>
      ///   Total number of stored features.
      /// </summary>
      property FeaturesCount  : Integer read  FNumFeatures ;
      /// <summary>
      ///   Maximum depth of the quad tree.
      ///   Default is 0. Maximum is 12. EstimateMaxDepth method can be used to
      ///   calculate proper value based on features count.
      /// </summary>
      property MaxDepth       : Integer read  FMaxDepth
                                        write FMaxDepth ;
      /// <summary>
      ///   Maximum number of features stored within a single node.
      ///   Default is 8.
      /// </summary>
      property BucketCapacity : Integer read  FBucketCapacity
                                        write FBucketCapacity ;
      /// <summary>
      ///   Ratio between extents resulting from splitting.
      ///   0.5  - split in half
      ///   0.55 - each node will contain 55% of the parent node, with 20% overlap.
      ///          this can prevent shifting high small object on the boundary.
      /// </summary>
      property SplitRatio     : Double  read  FSplitRatio
                                        write FSplitRatio   ;
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Math,
    GisFunctions ;
{$ENDIF}

const
  {#gendoc:hide}
  GIS_QT_COPYRIGHT      = 'TatukGIS DK QTree' ;
  {#gendoc:hide}
  GIS_QT_VERSION        = 1 ;
  {#gendoc:hide}
  GIS_QT_SUBVERSION     = 0 ;
  {#gendoc:hide}
  GIS_QT_MAGIC          = 71334573 ;
  {#gendoc:hide}
  GIS_QT_DIRTY          = 0 ;
  {#gendoc:hide}
  GIS_QT_COMMITED       = 73 ;

//==============================================================================
// TGIS_QuadTreeItem
//==============================================================================

  constructor TGIS_QuadTreeItem.Create(
    const _uid : Integer ;
    const _ext : TGIS_Extent
  ) ;
  begin
    FUid    := _uid ;
    FExtent := _TGIS_Extent(_ext) ;
  end ;

  {$IFDEF GIS_XDK}
    procedure TGIS_QuadTreeItem.AssignValues(
      const _uid : Integer ;
      const _ext : TGIS_Extent
    ) ;
    begin
      FUid    := _uid ;
      FExtent := _TGIS_Extent(_ext) ;
    end ;
  {$ENDIF}

//==============================================================================
// TGIS_QuadTreeNode
//==============================================================================

  constructor TGIS_QuadTreeNode.Create(
    const _extent : TGIS_Extent
  ) ;
  begin
    inherited Create ;

    NumFeatures  := 0 ;
    Features     := TList<TGIS_QuadTreeItem>.Create ;
    NumSubNodes  := 0;
    Extent       := _TGIS_Extent(_extent) ;
  end ;

  procedure TGIS_QuadTreeNode.doDestroy ;
  var
    i : Integer ;
  begin
    for i := 0 to NumSubNodes-1 do
      if assigned( SubNodes[ i ] ) then
        FreeObjectNotNil( SubNodes[ i ] ) ;

    FreeObject( Features ) ;

    inherited ;
  end ;

  function TGIS_QuadTreeNode.subNodeOffset(
    const _node : TGIS_QuadTreeNode
  ) : Integer ;
  var
    i      : Integer ;
    offset : Integer ;
  begin
    offset := 0 ;

    for i := 0 to _node.NumSubNodes-1 do begin
      if assigned( _node.SubNodes[i] ) then
        offset := offset + 4 * sizeOf( Single ) +
                  ( _node.SubNodes[i].NumFeatures + 3 ) * sizeOf( Integer ) ;
        offset := offset + subNodeOffset( _node.SubNodes[i] ) ;
    end ;
    Result := offset ;
  end ;

  procedure TGIS_QuadTreeNode.ForEach(
    const _callback : TGIS_QuadTreeForEachItemEvent ;
    const _data     : TObject
  ) ;
  var
    i, j : Integer ;
    abrt : Boolean ;
  begin
    abrt := False ;

    for i := 0 to NumSubNodes-1 do begin
      if assigned( _callback ) then
        _callback( TGIS_QuadTreeItem.Create(-1, Extent ), _data, abrt ) ;
      if abrt then exit ;

      if assigned( SubNodes[ i ] ) then
       SubNodes[ i ].ForEach( _callback, _data ) ;
    end ;

    for j := 0 to NumFeatures -1 do begin
      if assigned( _callback ) then
        _callback( Features[j], _data, abrt ) ;
      if abrt then exit ;
    end ;

  end ;

  procedure TGIS_QuadTreeNode.Clear ;
  var
    i : Integer ;
  begin
    for i := 0 to NumSubNodes-1 do
      if assigned( SubNodes[ i ] ) then
        FreeObjectNotNil( SubNodes[ i ] ) ;

    NumFeatures  := 0 ;
    NumSubNodes  := 0 ;
    if assigned( Features ) then
      Features.Clear ;
  end ;

  procedure TGIS_QuadTreeNode.Write(
    const _fs   : TGIS_Stream ;
    const _node : TGIS_QuadTreeNode
  ) ;
  var
    offset, i : Integer ;
  begin
    offset := subNodeOffset( _node ) ;

    _fs.WriteInteger( offset ) ;
    _fs.WriteSingle( _node.Extent.XMin ) ;
    _fs.WriteSingle( _node.Extent.YMin ) ;
    _fs.WriteSingle( _node.Extent.XMax ) ;
    _fs.WriteSingle( _node.Extent.YMax ) ;

    _fs.WriteInteger( _node.NumFeatures ) ;

    for i := 0 to _node.NumFeatures-1 do
      _fs.WriteInteger( _node.Features[i].Uid ) ;

    _fs.WriteInteger( _node.NumSubNodes ) ;

    for i := 0 to _node.NumSubNodes-1 do begin
      if assigned( _node.SubNodes[i] ) then
        Write( _fs, _node.SubNodes[i] ) ;
    end ;
  end ;


//==============================================================================
// TGIS_QuadTree
//==============================================================================

  constructor TGIS_QuadTree.Create(
    const _extent : TGIS_Extent
  ) ;
  begin
    inherited Create ;

    path      := '' ;
    inMemory  := True ;
    streamObj := nil ;

    root := TGIS_QuadTreeNode.Create( _extent ) ;
    doCreate ;
  end ;

  constructor TGIS_QuadTree.Create(
    const _path : String
  ) ;
  begin
    inherited Create ;

    path      := _path ;
    inMemory  := False ;
    streamObj := nil ;
    root      := nil ;

    doCreate ;
    openStream ;
  end ;

  procedure TGIS_QuadTree.doCreate ;
  begin
    FNumFeatures     := 0 ;
    FMaxDepth       := 0 ;
    FBucketCapacity := 8 ;
    FSplitRatio     := 0.55 ;
  end;

  procedure TGIS_QuadTree.doDestroy ;
  begin
    FreeObject( streamObj ) ;
    FreeObject( root      ) ;

    inherited ;
  end ;

  procedure TGIS_QuadTree.openStream ;
  begin
    streamObj := TGIS_BufferedFileStream.Create( path, TGIS_StreamMode.Read ) ;
    streamObj.ReadInteger( magic      ) ;
    streamObj.ReadByte   ( version    ) ;
    streamObj.ReadByte   ( subversion ) ;
    streamObj.ReadString ( copyright, length( GIS_QT_COPYRIGHT )  ) ;
    streamObj.ReadByte   ( commited     ) ;
    streamObj.ReadInteger( FNumFeatures ) ;
    streamObj.ReadInteger( FMaxDepth    ) ;

    startPos := streamObj.Position ;
  end ;

  procedure TGIS_QuadTree.splitBounds(
    const _split : Double ;
    const _in    : TGIS_Extent ;
      var _out1  : TGIS_Extent ;
      var _out2  : TGIS_Extent
  ) ;
  var
    range : Double ;
  begin
    _out1 := _TGIS_Extent(_in) ;
    _out2 := _TGIS_Extent(_in) ;

    if ( _in.XMax - _in.XMin ) > ( _in.YMax - _in.YMin ) then begin
      // Split in X direction
      range := _in.XMax - _in.XMin ;

      _out1.XMax := _in.XMin + range * _split ;
      _out2.XMin := _in.XMax - range * _split ;
    end
    else begin
      // Otherwise split in Y direction
      range := _in.YMax - _in.YMin ;

      _out1.YMax := _in.YMin + range * _split ;
      _out2.YMin := _in.YMax - range * _split ;
    end ;
  end ;

  function TGIS_QuadTree.trimNodes(
    const _node : TGIS_QuadTreeNode
  ) : Boolean ;
  var
    i : Integer ;
  begin
    i := 0 ;
    while i < _node.NumSubNodes do begin
      if trimNodes( _node.SubNodes[i] ) then begin
        FreeObjectNotNil( _node.SubNodes[i] ) ;
        _node.SubNodes[i] := _node.SubNodes[_node.NumSubNodes-1] ;
        dec( _node.NumSubNodes ) ;
        dec( i ) ;
      end ;
      inc( i ) ;
    end ;
    Result := ( _node.NumSubNodes = 0 ) and ( _node.NumFeatures = 0 ) ;
  end ;

  procedure TGIS_QuadTree.addFeature(
    const _node    : TGIS_QuadTreeNode ;
    const _feature : TGIS_QuadTreeItem ;
    const _extent  : TGIS_Extent
   ) ;
  var
    i              : Integer ;
    half1, half2,
    quad1, quad2,
    quad3, quad4   : TGIS_Extent ;
    oldNumFeatures : Integer ;
    oldFeatures    : TList<TGIS_QuadTreeItem> ;
  begin
    if ( _node.NumSubNodes = 0 ) then begin

      if ( _node.NumFeatures >= FBucketCapacity ) then begin

        splitBounds( FSplitRatio, _node.Extent, half1, half2 ) ;
        splitBounds( FSplitRatio, half1       , quad1, quad2 ) ;
        splitBounds( FSplitRatio, half2       , quad3, quad4 ) ;

        if GisIsContainExtent( _extent, quad1 ) or
           GisIsContainExtent( _extent, quad2 ) or
           GisIsContainExtent( _extent, quad3 ) or
           GisIsContainExtent( _extent, quad4 ) then begin

            _node.NumSubNodes := 4 ;
            _node.SubNodes[0] := TGIS_QuadTreeNode.Create( quad1 ) ;
            _node.SubNodes[1] := TGIS_QuadTreeNode.Create( quad2 ) ;
            _node.SubNodes[2] := TGIS_QuadTreeNode.Create( quad3 ) ;
            _node.SubNodes[3] := TGIS_QuadTreeNode.Create( quad4 ) ;

            oldNumFeatures      := _node.NumFeatures ;
            oldFeatures         := _node.Features ;
            _node.NumFeatures   := 0 ;
            _node.Features := nil ;

            for i := 0 to oldNumFeatures -1 do begin
              addFeature( _node, oldFeatures[i], oldFeatures[i].Extent ) ;
            end ;

            FreeObject( oldFeatures ) ;

            addFeature( _node, _feature, _extent ) ;
            exit ;
        end
      end
    end
    else begin
      for i := 0 to _node.NumSubNodes-1 do begin
        if GisIsContainExtent( _extent, _node.SubNodes[i].Extent ) then begin
          addFeature( _node.SubNodes[i], _feature, _extent ) ;
          exit ;
        end
      end ;
    end ;

    inc( _node.NumFeatures ) ;
    if not assigned( _node.Features ) then
      _node.Features := TList<TGIS_QuadTreeItem>.Create ;

    _node.Features.Add( _feature ) ;
  end ;

  procedure TGIS_QuadTree.addFeature(
    const _node     : TGIS_QuadTreeNode ;
    const _feature  : TGIS_QuadTreeItem ;
    const _extent   : TGIS_Extent ;
    const _maxDepth : Integer
   ) ;
  var
    i             : Integer ;
    half1, half2,
    quad1, quad2,
    quad3, quad4  : TGIS_Extent ;
  begin
    if ( _maxDepth > 1 ) and ( _node.NumSubNodes > 0 ) then begin
      // If there are subnodes, then consider whether this object will fit in them
      for i := 0 to _node.NumSubNodes-1 do begin
        if GisIsContainExtent( _extent, _node.SubNodes[i].Extent ) then begin
          addFeature( _node.SubNodes[i], _feature, _extent, _maxDepth-1 ) ;
          exit ;
        end
      end
    end
    else if ( FMaxDepth > 1 ) and ( _node.NumSubNodes = 0 ) then begin
      // Otherwise, consider creating four subnodes if could fit into
      // them, and adding to the appropriate subnode
      splitBounds( FSplitRatio, _node.Extent, half1, half2 ) ;
      splitBounds( FSplitRatio, half1       , quad1, quad2 ) ;
      splitBounds( FSplitRatio, half2       , quad3, quad4 ) ;

      if GisIsContainExtent( _extent, quad1 ) or
         GisIsContainExtent( _extent, quad2 ) or
         GisIsContainExtent( _extent, quad3 ) or
         GisIsContainExtent( _extent, quad4 ) then begin

          _node.NumSubNodes := 4 ;
          _node.SubNodes[0] := TGIS_QuadTreeNode.Create( quad1 ) ;
          _node.SubNodes[1] := TGIS_QuadTreeNode.Create( quad2 ) ;
          _node.SubNodes[2] := TGIS_QuadTreeNode.Create( quad3 ) ;
          _node.SubNodes[3] := TGIS_QuadTreeNode.Create( quad4 ) ;

          addFeature( _node, _feature, _extent, _maxDepth ) ;
          exit ;
      end
    end ;

    inc( _node.NumFeatures ) ;
    _node.Features.Add( _feature ) ;
  end ;

  procedure TGIS_QuadTree.collectFeatures(
    const _node   : TGIS_QuadTreeNode ;
    const _extent : TGIS_Extent ;
    const _list   : TGIS_ListOfIntegers
  ) ;
  var
    i : Integer ;
  begin
    if not GisIsCommonExtent( _node.Extent, _extent ) then exit ;

    for i := 0 to _node.NumFeatures -1 do begin
        if GisIsCommonExtent( _node.Features[i].Extent, _extent ) then
          _list.Add( _node.Features[i].Uid ) ;
    end ;

    for i := 0 to _node.NumSubNodes - 1 do
      if assigned( _node.SubNodes[i] ) then
        collectFeatures( _node.SubNodes[i], _extent, _list ) ;
  end ;

  function TGIS_QuadTree.searchOnDiscNode(
    const _extent : TGIS_Extent ;
    const _list   : TGIS_ListOfIntegers
  ) : Boolean ;
  var
    offset      : Integer ;
    ext         : TGIS_Extent ;
    i, uid      : Integer ;
    nfeatures   : Integer ;
    nsubnodes   : Integer ;
    val         : Single ;
  begin
    {$IFDEF GIS_NORECORDS}
    ext := new TGIS_Extent ;
    {$ENDIF}
    streamObj.ReadInteger( offset ) ;
    streamObj.ReadSingle( val ) ;
    ext.XMin := val ;
    streamObj.ReadSingle( val ) ;
    ext.YMin := val ;
    streamObj.ReadSingle( val ) ;
    ext.XMax := val ;
    streamObj.ReadSingle( val ) ;
    ext.YMax := val ;

    streamObj.ReadInteger( nfeatures ) ;

    if not GisIsCommonExtent( ext, _extent ) then begin
      offset := offset + nfeatures * sizeOf( Integer ) + sizeOf( Integer ) ;
      streamObj.Position := streamObj.Position + offset ;

      Result := True ;
      exit ;
    end ;

    if nfeatures > 0 then begin
      for i := 0 to nfeatures-1 do begin
        streamObj.ReadInteger( uid ) ;
        _list.Add( uid ) ;
      end ;
    end ;

    streamObj.ReadInteger( nsubnodes ) ;

    for i := 0 to nsubnodes-1 do begin
      if not searchOnDiscNode( _extent, _list ) then begin
        Result := False ;
        exit ;
      end ;
    end ;
    Result := True ;
  end ;

  procedure TGIS_QuadTree.Insert(
    const _id     : Integer ;
    const _extent : TGIS_Extent
  ) ;
  begin
    inc( FNumFeatures ) ;

    if FMaxDepth = 0 then
      addFeature( root, TGIS_QuadTreeItem.Create( _id, _extent ), _extent )
    else
      addFeature( root, TGIS_QuadTreeItem.Create( _id, _extent ), _extent, FMaxDepth ) ;
  end ;

  // Compare uids as required for TList.Sort
  function SortByUid(
    const _p1, _p2 : {$IFDEF JAVA} nullable {$ENDIF} Integer
  ) : Integer ;
  begin
    if      _p1 > _p2 then Result := 1
    else if _p1 < _p2 then Result := -1
    else                   Result := 0 ;
  end ;

  function TGIS_QuadTree.Search(
    const _extent : TGIS_Extent
  ) : TGIS_ListOfIntegers ;
  begin
    Result := TGIS_ListOfIntegers.Create ;

    if inMemory then
      collectFeatures( root, _extent, Result )
    else begin
      streamObj.Position := startPos ;
      searchOnDiscNode( _extent, Result ) ;
    end ;

    {$IFDEF OXYGENE}
      Result.Sort( {$IFDEF OXYGENE}@{$ENDIF}SortByUid ) ;
    {$ELSE}
      Result.Sort( TComparer<Integer>.Construct( SortByUid ) ) ;
    {$ENDIF}
  end ;

  procedure TGIS_QuadTree.EstimateMaxDepth(
    const _maxFeatures : Integer
  ) ;
  var
    max_depth    : Integer ;
    max_node_cnt : Integer ;
  begin
    max_depth := 0;
    max_node_cnt := 1;

    while ( max_node_cnt < _maxFeatures / 4 ) do begin
      max_depth    := max_depth + 1;
      max_node_cnt := max_node_cnt * 2;
    end ;

    FMaxDepth := Min( max_depth, 12 ) ;
  end ;

  procedure TGIS_QuadTree.CleanUp ;
  begin
    trimNodes( root ) ;
  end ;

  procedure TGIS_QuadTree.Clear ;
  begin
    doCreate ;

    root.Clear ;
  end ;

  procedure TGIS_QuadTree.ForEach(
    const _callback : TGIS_QuadTreeForEachItemEvent ;
    const _data     : TObject
  ) ;
  begin
    root.ForEach( _callback, _data ) ;
  end ;

  procedure TGIS_QuadTree.SaveToFile(
    const _fileName : String
  ) ;
  var
    stm : TGIS_BufferedFileStream ;
  begin
    stm := TGIS_BufferedFileStream.Create( _fileName, TGIS_StreamMode.&Create ) ;
    try
      stm.WriteInteger( GIS_QT_MAGIC      ) ;
      stm.WriteByte   ( GIS_QT_VERSION    ) ;
      stm.WriteByte   ( GIS_QT_SUBVERSION ) ;
      stm.WriteString ( GIS_QT_COPYRIGHT  ) ;
      stm.WriteByte   ( GIS_QT_COMMITED   ) ;
      stm.WriteInteger( FNumFeatures      ) ;
      stm.WriteInteger( FMaxDepth         ) ;

      root.Write( stm, root ) ;
    finally
      FreeObject( stm ) ;
    end ;

  end ;

//==================================== END =====================================
end.
