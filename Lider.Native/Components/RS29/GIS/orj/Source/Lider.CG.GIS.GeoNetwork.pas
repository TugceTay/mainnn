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
  Encapsulation of a network storage for Shortest Path algorithms.
}

{$IFDEF DCC}
  unit GisNetwork ;
  {$HPPEMIT '#pragma link "GisNetwork"'}
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
{$IFNDEF OXYGENE}
  {$ALIGN OFF}
{$ENDIF}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Types,
    System.SysUtils,
    System.Generics.Collections,

    GisRtl,
    GisLayerVector,
    GisTypes,
    GisRtree,
    GisStreams  ;
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
  ///   State of node in the list of candidates.
  /// </summary>
  TGIS_NetworkCandidate = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Node is not in the list of candidates.
    /// </summary>
    &Not,
    /// <summary>
    ///   Node has been just added to the list of candidates.
    /// </summary>
    &New,
    /// <summary>
    ///   Node is already in the list of candidates.
    /// </summary>
    Already
  ) ;

  TGIS_Network       = class ;
  TGIS_NetworkObject = class ;
  TGIS_NetworkNode   = class ;
  TGIS_NetworkLink   = class ;

  {$IFDEF OXYGENE}

    /// <summary>
    ///   Provides data for the link event.
    /// </summary>
    TGIS_NetworkLinkEventArgs = public class ( EventArgs )
      private
        FShape    : TGIS_ShapeArc    ;
        FLink     : TGIS_NetworkLink ;
      public
        /// <summary>
        ///   Constructor for TGIS_NetworkLinkEventArgs
        /// </summary>
        /// <param name="_shape">
        ///   shapeobject on which event occurred
        /// </param>
        /// <param name="_link">
        ///   Related link object
        /// </param>
        constructor Create  ( const _shape    : TGIS_ShapeArc ;
                              const _link     : TGIS_NetworkLink
                            ) ;
      public


        /// <summary>
        ///   Shape object on which event occurred.
        /// </summary>
        property Shape      : TGIS_ShapeArc    read FShape ;

        /// <summary>
        ///   Related link object
        /// </summary>
        property Link       : TGIS_NetworkLink read FLink;
    end ;

    /// <summary>
    ///   Event for segment traverse.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_e">
    ///   event parameters
    /// </param>
    TGIS_NetworkLinkEvent = public procedure(
      _sender   : Object ;
      _e        : TGIS_NetworkLinkEventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   Event for the link cost calculation.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_shape">
    ///   shape object on which event occurred
    /// </param>
    /// <param name="_link">
    ///   related link object
    /// </param>
    TGIS_NetworkLinkEvent  = procedure(
      _sender   : TObject ;
      _shape    : TGIS_ShapeArc ;
      _link     : TGIS_NetworkLink
    ) of object ;
  {$ENDIF}

  /// <summary>
  ///   Base class for network storage object. Storage object is responsible
  ///   for physical network storage.
  /// </summary>
  TGIS_NetworkStorageAbstract = {$IFDEF OXYGENE} public abstract {$ENDIF}
                                class ( TGIS_ObjectDisposable )

    protected
      /// <summary>
      ///  True if the storage is ready.
      /// </summary>
      FReady : Boolean ;
    public

      /// <summary>
      ///   Commit changes.
      /// </summary>
      procedure Commit  ; virtual; abstract;

      /// <summary>
      ///   Delete network files (clears the network).
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEDELETE if file can not be deleted
      /// </exception>
      /// <remarks>
      ///   <note type="note">
      ///    After calling Delete network object should be recreated.
      ///    </note>
      /// </remarks>
      procedure Delete  ; virtual; abstract;

      /// <summary>
      ///   Read node from the storage into the memory.
      /// </summary>
      /// <param name="_node">
      ///   node to be read with ObjectId filled to specify object to be read
      /// </param>
      procedure ReadNode( const _node : TGIS_NetworkNode
                        ) ; virtual; abstract;

      /// <summary>
      ///   Save node to the storage
      /// </summary>
      /// <param name="_node">
      ///   node to be saved
      /// </param>
      procedure SaveNode( const _node : TGIS_NetworkNode
                        ) ; virtual; abstract;

      /// <summary>
      ///   Find closest node corresponding to provided location.
      /// </summary>
      /// <param name="_ptg">
      ///   location of the node
      /// </param>
      /// <param name="_prec">
      ///   precision of location in layer units
      /// </param>
      /// <param name="_level">
      ///   node level; if level = 127 then any level will be found
      /// </param>
      /// <returns>
      ///   number of nodes found
      /// </returns>
      function  FindNode( const _ptg   : TGIS_Point ;
                          const _prec  : Double ;
                          const _level : ShortInt
                        ) : Integer ; virtual; abstract;

      /// <summary>
      ///   Read link from the storage into the memory.
      /// </summary>
      /// <param name="_link">
      ///   link to be read with ObjectId filled to specify object to be read
      /// </param>
      procedure ReadLink( const _link : TGIS_NetworkLink
                        ) ; virtual; abstract;

      /// <summary>
      ///   Save link to the storage
      /// </summary>
      /// <param name="_link">
      ///   link to be saved
      /// </param>
      procedure SaveLink( const _link : TGIS_NetworkLink
                        ) ; virtual; abstract;
    public
      /// <summary>
      ///  True if the storage is ready.
      /// </summary>
      property Ready : Boolean read FReady ;
  end ;

  /// <summary>
  ///   Simple file based network storage.
  /// </summary>
  TGIS_NetworkFile = {$IFDEF OXYGENE} public {$ENDIF}
                     class ( TGIS_NetworkStorageAbstract )
    private
      sPath         : String          ;
      strmNodeIndex : TGIS_BufferedStream ;
      strmNodeData  : TGIS_BufferedStream ;
      strmLinkIndex : TGIS_BufferedStream ;
      strmLinkData  : TGIS_BufferedStream ;
      oRtree        : TGIS_RTree      ;
      bLevels       : Boolean         ;
    protected
      procedure doDestroy ; override;

    public
      /// <inheritdoc/>
      procedure Commit  ; override;
      /// <inheritdoc/>
      procedure Delete  ; override;
      /// <inheritdoc/>
      procedure ReadNode( const _node      : TGIS_NetworkNode
                        ) ; override;
      /// <inheritdoc/>
      procedure SaveNode( const _node      : TGIS_NetworkNode
                        ) ; override;
      /// <inheritdoc/>
      function  FindNode( const _ptg       : TGIS_Point       ;
                          const _prec      : Double           ;
                          const _level     : ShortInt
                        ) : Integer ; override;
      /// <inheritdoc/>
      procedure ReadLink( const _link      : TGIS_NetworkLink
                        ) ; override;
      /// <inheritdoc/>
      procedure SaveLink( const _link      : TGIS_NetworkLink
                        ) ; override;
    public

      /// <summary>
      ///   Create an instance
      /// </summary>
      /// <param name="_path">
      ///   path for network storage (including file name)
      /// </param>
      /// <param name="_write">
      ///   if true then new files will be created; otherwise read-only mode
      /// </param>
      constructor Create( const _path      : String ;
                          const _write     : Boolean
                        ) ;
  end ;

  /// <summary>
  ///   List of cached (in-memory) network objects.
  /// </summary>
  TGIS_NetworkObjectList = {$IFDEF OXYGENE} public {$ENDIF}
                           {$IFNDEF GENXDK}
                             class( TGIS_ObjectList )
                           {$ELSE}
                             class
                           {$ENDIF}

    public

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create  () ;

      /// <summary>
      ///   Add provided object to the list
      /// </summary>
      /// <param name="_obj">
      ///   object to be added
      /// </param>
      procedure   Add     ( const _obj     : TGIS_NetworkObject
                          ) ;

      /// <summary>
      ///   Find item on the list.
      /// </summary>
      /// <param name="_id">
      ///   object id (ObjectId value)
      /// </param>
      /// <param name="_ipos">
      ///   found position
      /// </param>
      /// <returns>
      ///   True if the item was found
      /// </returns>
      function    FindItem( const _id      : Cardinal ;
                            var   _ipos    : Integer
                          ) : Boolean ;
                          {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

      /// <summary>
      ///   Get object by identifier.
      /// </summary>
      /// <param name="_id">
      ///   object id (ObjectId value)
      /// </param>
      /// <returns>
      ///   object with specified identifier
      /// </returns>
      function    Get     ( const _id      : Cardinal
                          ) : TGIS_NetworkObject ;
                          {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
  end ;

  /// <summary>
  ///   Base object for network operations.
  /// </summary>
  TGIS_NetworkObject = {$IFDEF OXYGENE} public {$ENDIF}
                       class ( TGIS_Object)

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      FParent   : TGIS_Network ;
      FObjectId : Cardinal     ;
    public

        /// <summary>
        ///   Parent network.
        /// </summary>
        property Parent   : TGIS_Network
                            read   FParent
                            write  FParent ;

        /// <summary>
        ///   Unique node identifier.
        /// </summary>
        property ObjectId : Cardinal
                            read   FObjectId
                            write  FObjectId ;
  end ;

  /// <summary>
  ///   Network node.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    Variables with prefix 'Current' are not serializable. Those variables
  ///    are used only upon actual path calculation.
  ///    </note>
  ///   Each node can contain number of TGIS_NetworkNode (connectors) to other
  ///   nodes.
  /// </remarks>
  TGIS_NetworkNode = {$IFDEF OXYGENE} public {$ENDIF}
                     class( TGIS_NetworkObject )
    // persistent variables
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

        /// <summary>
        ///   List of links associated with the node.
        /// </summary>
        FLinks : array of Cardinal ; // list of elements

        /// <summary>
        ///   Node spatial position.
        /// </summary>
        FPos   : TGIS_Point ;

        /// <summary>
        ///   Node level.
        /// </summary>
        FLevel : ShortInt ;

    // non-persistent variables
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

        /// <summary>
        ///   Path tree link.
        /// </summary>
        FCurrentLink : Cardinal ;

        /// <summary>
        ///   Best cost so far.
        /// </summary>
        FCurrentCost : Double ;

        /// <summary>
        ///   Best cost so far.
        /// </summary>
        FHeuristicCost : Double ;

        /// <summary>
        ///   Node state.
        /// </summary>
        FCurrentState : TGIS_NetworkCandidate;

    private // property access routines
      function fget_LinkCount  : Integer ;
                             {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}
      function fget_Link       ( const _index : Integer
                             ) : TGIS_NetworkLink ;
                             {$IFNDEF GIS_NOINLINE} inline; {$ENDIF}

    protected
      procedure   doDestroy  ; override;

    public // constructors

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor Create     ;

      /// <summary>
      ///   Create a copy of the current object.
      /// </summary>
      /// <returns>
      ///   the copy of the object
      /// </returns>
      function    CreateCopy : TGIS_NetworkNode ;

    public // public methods

      /// <summary>
      ///   Add a link to the list of associated links.
      /// </summary>
      /// <param name="_link">
      ///   link to be added
      /// </param>
      procedure LinkAdd      ( const _link : TGIS_NetworkLink ) ;

    public // public properties

        /// <summary>
        ///   Position of node.
        /// </summary>
        property Pos           : TGIS_Point
                                 read FPos ;

        /// <summary>
        ///   Node level.
        /// </summary>
        property Level         : ShortInt
                                 read FLevel ;

        /// <summary>
        ///   Path tree link.
        /// </summary>
        property CurrentLink   : Cardinal
                                 read  FCurrentLink
                                 write FCurrentLink ;

        /// <summary>
        ///   Best cost so far.
        /// </summary>
        property CurrentCost   : Double
                                 read  FCurrentCost
                                 write FCurrentCost ;

        /// <summary>
        ///   Best heuristic cost so far.
        /// </summary>
        property HeuristicCost : Double
                                 read  FHeuristicCost
                                 write FHeuristicCost ;

        /// <summary>
        ///   Node state.
        /// </summary>
        property CurrentState  : TGIS_NetworkCandidate
                                 read  FCurrentState
                                 write FCurrentState ;

        /// <summary>
        ///   Number of links on the list of associated links.
        /// </summary>
        property LinkCount     : Integer
                                 read fget_LinkCount ;

        /// <summary>
        ///   Access link from the list of associated links. Links list is
        ///   counted from 0.
        /// </summary>
        /// <param name="_index">
        ///   index of the link
        /// </param>
        property Link          [ const _index : Integer
                               ] : TGIS_NetworkLink
                               read fget_Link ;
  end ;

  /// <summary>
  ///   Network Link.
  /// </summary>
  /// <remarks>
  ///   Each link contains id of beginning and ending node of the link.
  /// </remarks>
  TGIS_NetworkLink = {$IFDEF OXYGENE} public {$ENDIF}
                     class( TGIS_NetworkObject )

     public // persistent variables

         /// <summary>
         ///   Shape uid describing this link.
         /// </summary>
         ShapeUid : Integer ;

         /// <summary>
         ///   The endpoint.
         /// </summary>
         NodeA : Cardinal ;

         /// <summary>
         ///   The endpoint.
         /// </summary>
         NodeB : Cardinal ;

         /// <summary>
         ///   Cost of the link.
         /// </summary>
         Cost : Double ;

         /// <summary>
         ///   Reverse cost of the link.
         /// </summary>
         ReverseCost : Double ;

         /// <summary>
         ///   Type of link (0..9).
         /// </summary>
         LinkType : Byte ;
     public // not-persistent variables

         /// <summary>
         ///   Layer describing this link.
         /// </summary>
         Layer : TGIS_LayerVector ;

  end ;

  /// <summary>
  ///   Class for storing and retrieving network graph for TGIS_ShortestPathLink
  ///   purposes and other network analysis classes.
  /// </summary>
  TGIS_Network = {$IFDEF OXYGENE} public {$ENDIF}
                 class ( TGIS_BaseObjectDisposable )

    // property values
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
      /// <summary>
      ///   Event for the link cost calculation.
      /// </summary>
      FOnLink : TGIS_NetworkLinkEvent ;
      /// <summary>
      ///   Path to the network storage file.
      /// </summary>
      FStoragePath : String ;
      /// <summary>
      ///   Associated layer.
      /// </summary>
      FLayer : TGIS_LayerVector ;

    // other private variable
    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

        /// <summary>
        ///   Last generated links id.
        /// </summary>
        linksId : Cardinal ;

        /// <summary>
        ///   Last generated nodes id.
        /// </summary>
        nodesId : Cardinal ;

        /// <summary>
        ///   List of nodes residing in memory.
        /// </summary>
        lstNodes : TGIS_NetworkObjectList ;

        /// <summary>
        ///   array of nodes residing in memory.
        /// </summary>
        arNodes : array of TGIS_NetworkNode ;

        /// <summary>
        ///   List of links resides in memory.
        /// </summary>
        lstLinks : TGIS_NetworkObjectList ;

        /// <summary>
        ///   Storage for temporary link use for getLink operations
        /// </summary>
        tmpLink : TGIS_NetworkLink ;

        /// <summary>
        ///   Persistent network storage file.
        /// </summary>
        oStorage : TGIS_NetworkFile ;

        /// <summary>
        ///   Nodes index (for in memory objects)
        /// </summary>
        oRtree   : TGIS_RTree ;

    protected
      procedure   doDestroy ; override;

    public

      /// <summary>
      ///   Create a new network on provided layer
      /// </summary>
      /// <param name="_layer">
      ///   layer on which network should be constructed
      /// </param>
      /// <param name="_forcedpath">
      ///   if empty then network will be saved in a default location (same as
      ///   _layer.Path); if set then define locations (path with file name
      ///   like 'c:\tmp\myfile' should be provided)
      /// </param>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_BAD_CALL if path has not been provided but should be;
      ///   for example if layer is a SQL database
      /// </exception>
      constructor Create    ( const _layer      : TGIS_LayerVector ;
                              const _forcedpath : String
                            ) ;

    private
      function    fget_Ready  : Boolean ;

    public

      /// <summary>
      ///   Save current network to the underlying storage
      /// </summary>
      procedure   Serialize         ;

      /// <summary>
      ///   Open network (must exists first
      /// </summary>
      /// <exception cref="EGIS_Exception">
      ///   GIS_RS_ERR_FILEBADFORMAT if file can not be opened
      /// </exception>
      procedure   Open              ;

      /// <summary>
      ///   Delete saved network.
      /// </summary>
      procedure   Delete            ;

      /// <summary>
      ///   Clean the network to prepare it for next searching.
      /// </summary>
      procedure   Clean             ;

      /// <summary>
      ///   Clean all dynamic links like created by geocoder.
      /// </summary>
      procedure   CleanDynamicLinks ;

      /// <summary>
      ///   Find closest node corresponding to provided location.
      /// </summary>
      /// <param name="_ptg">
      ///   location of the node
      /// </param>
      /// <param name="_prec">
      ///   precision of location in layer units
      /// </param>
      /// <param name="_level">
      ///   node level; if level = 127 then any level will be found
      /// </param>
      /// <returns>
      ///   found node
      /// </returns>
      function    FindNode          ( const _ptg   : TGIS_Point    ;
                                      const _prec  : Double        ;
                                      const _level : ShortInt
                                    ) : TGIS_NetworkNode ; overload;

      /// <summary>
      ///   Find closest node corresponding to provided location regardless of
      ///   the nide level.
      /// </summary>
      /// <param name="_ptg">
      ///   location of the node
      /// </param>
      /// <param name="_prec">
      ///   precision of location in layer units
      /// </param>
      /// <returns>
      ///   found node
      /// </returns>
      function    FindNode          ( const _ptg   : TGIS_Point    ;
                                      const _prec  : Double
                                    ) : TGIS_NetworkNode ; overload;

      /// <summary>
      ///   Read from network (storage or memory) a node corresponding to
      ///   provided ObjectId
      /// </summary>
      /// <param name="_id">
      ///   node id (ObjectId value)
      /// </param>
      /// <returns>
      ///   retrieved node
      /// </returns>
      function    GetNode           ( const _id    : Cardinal
                                    ) : TGIS_NetworkNode ;

      /// <summary>
      ///   Read from network (storage or memory) a link corresponding to
      ///   provided ObjectId
      /// </summary>
      /// <param name="_id">
      ///   link id (ObjectId value)
      /// </param>
      /// <returns>
      ///   retrieved link
      /// </returns>
      function    GetLink           ( const _id    : Cardinal
                                    ) : TGIS_NetworkLink ;

      /// <summary>
      ///   Get shape object from the network corresponding to provided link
      ///   identifier
      /// </summary>
      /// <param name="_id">
      ///   link id (ObjectId value)
      /// </param>
      /// <returns>
      ///   retrieved shape
      /// </returns>
      function    GetShape          ( const _id    : Cardinal
                                    ) : TGIS_ShapeArc ;

      /// <summary>
      ///   Add new shape to the network. Adding will add a link, nodes etc.
      /// </summary>
      /// <param name="_shp">
      ///   shape to be added
      /// </param>
      /// <param name="_field">
      ///   if provided that field name contains link cost; if empty then
      ///   _shp.LengthCS will be used instead
      /// </param>
      procedure   AddShape          ( const _shp   : TGIS_ShapeArc ;
                                      const _field : String
                                    ) ;

      /// <summary>
      ///   Add a node at the specific location. If node at the specific
      ///   location already exists, then reference to existing node will be
      ///   returned. Otherwise a new object will be added.
      /// </summary>
      /// <param name="_ptg">
      ///   node localization
      /// </param>
      /// <param name="_level">
      ///   node level; if level = 127 then any level will be found
      /// </param>
      /// <returns>
      ///   handle to the added node
      /// </returns>
      function    AddNode           ( const _ptg   : TGIS_Point    ;
                                      const _level : Integer
                                    ) : TGIS_NetworkNode ;

    public

        /// <summary>
        ///   True if network is properly generated.
        /// </summary>
        property Ready : Boolean read fget_Ready ;

    published //events

        /// <summary>
        ///   Network base layer.
        /// </summary>
        property Layer : TGIS_LayerVector read FLayer ;

        {$IFDEF CLR}
          /// <event/>
          /// <summary>
          ///   Event fired upon calling AddShape procedure.
          /// </summary>
          event    LinkEvent : TGIS_NetworkLinkEvent
                               delegate FOnLink ;
        {$ELSE}
          /// <event/>
          /// <summary>
          ///   Event fired upon calling AddShape procedure.
          /// </summary>
          property    LinkEvent : TGIS_NetworkLinkEvent
                                  read  FOnLink
                                  write FOnLink ;
        {$ENDIF}
  end ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisNetworkCandidateNot      = TGIS_NetworkCandidate.Not     ;
      gisNetworkCandidateNew      = TGIS_NetworkCandidate.New     ;
      gisNetworkCandidateAlready  = TGIS_NetworkCandidate.Already ;
  {$ENDIF}

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Variants,
    GisClasses,
    GisFunctions,
    GisInternals,
    GisResource ;
{$ENDIF}


const
   LINKS_DATA_EXT       = '.ntw1' ;
   LINKS_INDEX_EXT      = '.ntw2' ;
   NODES_DATA_EXT       = '.ntw3' ;
   NODES_INDEX_EXT      = '.ntw4' ;
   NODES_RTREE_EXT      = '.ntw5' ;

   NETWORK_HEADER_SIZE  = 256      ;
   NETWORK_HEADER_MAGIC = 75399463 ;
   NETWORK_HEADER_MAJOR = 2        ;
   NETWORK_HEADER_MINOR = 0        ;

   NETWORK_OBJECT_INMEMORY = $40000000 ;

//==============================================================================
// Utilities
//==============================================================================

  // Clear node Currentxxx properties.
  //  _node not to be cleared
  procedure clear_node(
    const _node : TGIS_NetworkNode
  ) ;
  begin
    _node.FCurrentLink   := 0 ;
    _node.FCurrentCost   := GIS_MAX_DOUBLE ; // really far, far away :>
    _node.FHeuristicCost := GIS_MAX_DOUBLE ; // really far, far away :>
    _node.FCurrentState  := TGIS_NetworkCandidate.Not ;
  end;

{$IFDEF OXYGENE}
  //============================================================================
  // TGIS_NetworkLinkEventArgs
  //============================================================================

    constructor TGIS_NetworkLinkEventArgs.Create(
      const _shape    : TGIS_ShapeArc ;
      const _link     : TGIS_NetworkLink
    ) ;
    begin
      inherited Create ;

      FShape := _shape ;
      FLink  := _link ;
    end;
{$ENDIF}

//==============================================================================
// TGIS_NetworkFile
//==============================================================================

  constructor TGIS_NetworkFile.Create(
    const _path      : String ;
    const _write     : Boolean
  ) ;
  var
    mode : TGIS_StreamMode ;

    procedure write_header(
      const _strm : TGIS_BufferedStream ;
      const _type : String
    ) ;
    var
      magic : DWORD ;
      major : Byte  ;
      minor : Byte  ;

      procedure write_text( const _txt : String ) ;
      var
        bt : TBytes  ;
      begin
        bt := ConvertAnsiString( _txt ) ;
        {$IFDEF OXYGENE}
          _strm.Write( bt, length( bt ) ) ;
        {$ELSE}
           _strm.Write( bt[0], Length( bt ) ) ;
        {$ENDIF}
        bt := nil ;
      end;

      procedure write_gap( const _end : Integer ) ;
      var
        i   : Integer ;
        gap : Byte  ;
      begin
        gap   := 0  ;
        for i := _strm.Position to _end - 1 do begin
          _strm.WriteByte( gap ) ;
        end ;
      end;

    begin
      assert( _strm.Size = 0 ) ;

      assert( sizeOf( magic ) = 4 ) ;
      assert( sizeOf( major ) = 1 ) ;
      assert( sizeOf( minor ) = 1 ) ;

      magic := 0 ;
      major := NETWORK_HEADER_MAJOR ;
      minor := NETWORK_HEADER_MINOR ;

      _strm.WriteCardinal( magic ) ; // magic
      _strm.WriteByte    ( major ) ; // version major
      _strm.WriteByte    ( minor ) ; // version minor
      write_gap( 32 ) ;

      write_text( 'TatukGIS Network' ) ;
      write_gap( 32 + 64 ) ;

      write_text( _type ) ;
      write_gap( 32 + 64 + 64 ) ;

      write_gap( NETWORK_HEADER_SIZE ) ;

      assert( _strm.Position = NETWORK_HEADER_SIZE ) ;
    end;

    procedure verify_header(
      const _strm : TGIS_BufferedStream
    ) ;
    var
      magic : DWORD ;
      major : Byte  ;
    begin
      if _strm.Size < NETWORK_HEADER_SIZE then
        Abort ;

      assert( sizeOf( magic ) = 4 ) ;
      assert( sizeOf( major ) = 1 ) ;

      _strm.Position := 0 ;

      _strm.ReadCardinal( magic ) ; // magic

      if magic <> NETWORK_HEADER_MAGIC then
        Abort ;

      _strm.ReadByte( major ) ; // version major

      if major > NETWORK_HEADER_MAJOR then
        Abort ;

      if major < NETWORK_HEADER_MAJOR then
        bLevels := False
      else
        bLevels := True ;
    end;

  begin
    inherited Create ;

    sPath := _path ;

    if _write then
      mode := TGIS_StreamMode.&Create
    else
      mode := TGIS_StreamMode.Read ;

    try
      if not _write then begin
        if not FileExists( _path + NODES_DATA_EXT  ) then
          Abort ;
        if not FileExists( _path + NODES_INDEX_EXT ) then
          Abort ;
        if not FileExists( _path + LINKS_DATA_EXT  ) then
          Abort ;
        if not FileExists( _path + LINKS_INDEX_EXT ) then
          Abort ;
        if not FileExists( _path + NODES_RTREE_EXT ) then
          Abort ;
      end ;

      strmNodeData  := TGIS_BufferedFileStream.Create( _path + NODES_DATA_EXT ,
                                                       mode
                                                     ) ;
      strmNodeIndex := TGIS_BufferedFileStream.Create( _path + NODES_INDEX_EXT,
                                                       mode
                                                     ) ;
      strmLinkData  := TGIS_BufferedFileStream.Create( _path + LINKS_DATA_EXT ,
                                                       mode
                                                     ) ;
      strmLinkIndex := TGIS_BufferedFileStream.Create( _path + LINKS_INDEX_EXT,
                                                       mode
                                                     ) ;
      oRtree        := TGIS_RTree.Create             ( _path + NODES_RTREE_EXT
                                                     ) ;

      FReady := ( not oRtree.IsEmpty ) and ( oRtree.IsCommited ) ;

      if _write then begin
        write_header( strmNodeData  , NODES_DATA_EXT  ) ;
        write_header( strmNodeIndex , NODES_INDEX_EXT ) ;
        write_header( strmLinkData  , LINKS_DATA_EXT  ) ;
        write_header( strmLinkIndex , LINKS_INDEX_EXT ) ;
      end
      else begin
        verify_header( strmNodeData  ) ;
        verify_header( strmNodeIndex ) ;
        verify_header( strmLinkData  ) ;
        verify_header( strmLinkIndex ) ;
      end ;
    except
      FreeObject( strmNodeIndex ) ;
      FreeObject( strmNodeData  ) ;
      FreeObject( strmLinkIndex ) ;
      FreeObject( strmLinkData  ) ;
      FreeObject( oRtree        ) ;

      FReady := False ;
    end;
  end ;

  procedure TGIS_NetworkFile.doDestroy;
  begin
    FreeObject( strmNodeIndex ) ;
    FreeObject( strmNodeData  ) ;
    FreeObject( strmLinkIndex ) ;
    FreeObject( strmLinkData  ) ;

    if assigned( oRtree ) then
      oRtree.Commit( True ) ;
    FreeObject( oRtree ) ;

    inherited ;
  end;

  procedure TGIS_NetworkFile.Commit ;

    procedure commit_header( const _strm : TGIS_BufferedStream ) ;
    var
      magic : DWORD ;
    begin
      assert( _strm.Size > NETWORK_HEADER_SIZE ) ;
      _strm.Position:= 0 ;

      assert( sizeOf( magic ) = 4 ) ;
      magic := NETWORK_HEADER_MAGIC ;
      _strm.WriteCardinal( magic ) ; // magic
    end;

  begin
    assert( assigned( strmNodeIndex ) ) ;
    commit_header( strmNodeIndex ) ;

    assert( assigned( strmNodeIndex ) ) ;
    commit_header( strmNodeIndex ) ;

    assert( assigned( strmNodeData  ) ) ;
    commit_header( strmNodeData  ) ;

    assert( assigned( strmLinkIndex ) ) ;
    commit_header( strmLinkIndex ) ;

    assert( assigned( strmLinkData  ) ) ;
    commit_header( strmLinkData  ) ;
  end;

  procedure TGIS_NetworkFile.Delete ;
    procedure delete_file( const _path : String ) ;
    begin
      if FileExists( _path ) then
        if not DeleteFile( _path ) then
          raise EGIS_Exception.Create(
                  _rsrc( GIS_RS_ERR_FILEDELETE ),
                  _path,
                  0
                ) ;
    end;
  begin
    FReady := False ;

    FreeObject( strmNodeIndex ) ;
    FreeObject( strmNodeData  ) ;
    FreeObject( strmLinkIndex ) ;
    FreeObject( strmLinkData  ) ;
    FreeObject( oRtree        ) ;

    delete_file( sPath + NODES_DATA_EXT  ) ;
    delete_file( sPath + NODES_INDEX_EXT ) ;
    delete_file( sPath + LINKS_DATA_EXT  ) ;
    delete_file( sPath + LINKS_INDEX_EXT ) ;
    delete_file( sPath + NODES_RTREE_EXT ) ;
  end;

  procedure TGIS_NetworkFile.ReadNode(
    const _node : TGIS_NetworkNode
  ) ;
  var
    i    : Integer  ;
    ipos : Cardinal ;
    cnt  : Word     ;
  begin
    assert( sizeOf( ipos ) = 4 ) ;
    assert( sizeOf( cnt  ) = 2 ) ;
    assert( sizeOf( _node.FPos.X ) = 8 ) ;
    assert( sizeOf( _node.FPos.Y ) = 8 ) ;
    assert( sizeOf( _node.FLevel ) = 1 ) ;

    strmNodeIndex.Position := NETWORK_HEADER_SIZE +
                              sizeOf( ipos ) * _node.ObjectId ;

    strmNodeIndex.ReadCardinal( ipos ) ;
    strmNodeData.Position := ipos ;

    strmNodeData.ReadDouble( _node.FPos.X ) ;
    strmNodeData.ReadDouble( _node.FPos.Y ) ;

    if bLevels then
      strmNodeData.ReadShortInt( _node.FLevel )
    else
      _node.FLevel := 0  ;

    strmNodeData.ReadWord( cnt ) ;
    SetLength( _node.FLinks, cnt ) ;

    for i := 0 to cnt - 1 do begin
      assert( sizeOf( _node.FLinks[i] ) = 4 ) ;
      strmNodeData.ReadCardinal( _node.FLinks[i] ) ;
    end;
  end;

  procedure TGIS_NetworkFile.SaveNode(
    const _node : TGIS_NetworkNode
  ) ;
  var
    i    : Integer  ;
    ipos : Cardinal ;
    cnt  : Word     ;
  begin
    assert( sizeOf( ipos ) = 4 ) ;
    assert( sizeOf( cnt  ) = 2 ) ;
    assert( sizeOf( _node.FPos.X ) = 8 ) ;
    assert( sizeOf( _node.FPos.Y ) = 8 ) ;
    assert( sizeOf( _node.FLevel ) = 1 ) ;

    ipos := strmNodeData.Position ;

    strmNodeIndex.WriteCardinal( ipos ) ;

    strmNodeData.WriteDouble( _node.FPos.X ) ;
    strmNodeData.WriteDouble( _node.FPos.Y ) ;
    strmNodeData.WriteShortInt( _node.FLevel ) ;

    cnt := _node.LinkCount ;
    strmNodeData.WriteWord( cnt ) ;

    for i := 0 to _node.LinkCount - 1 do begin
      assert( sizeOf( _node.FLinks[i] ) = 4 ) ;
      strmNodeData.WriteCardinal( _node.FLinks[i] ) ;
    end;

    oRtree.Insert( GisExtent(
                     _node.Pos.X, _node.Pos.Y, _node.Pos.X, _node.Pos.Y
                   ),
                   _node.ObjectId
                 ) ;
  end;

  function TGIS_NetworkFile.FindNode(
    const _ptg   : TGIS_Point ;
    const _prec  : Double     ;
    const _level : ShortInt
  ) : Integer ;
  var
    i    : TGIS_Uid ;
    tmp  : Double  ;
    dist : Double  ;
    lv   : Integer ;
    nd   : TGIS_NetworkNode ;
    k    : Integer ;
  begin
    nd :=  TGIS_NetworkNode.Create ;
    try
      dist := GIS_MAX_DOUBLE  ; // really far away
      lv   := GIS_MAX_INTEGER ;

      k := -1 ;

      i := oRtree.FindFirst( 0, GisExtent( _ptg.X - _prec, _ptg.Y - _prec,
                                           _ptg.X + _prec, _ptg.Y + _prec
                                         )
                           ) ;
      while i >= 0 do begin
        nd.ObjectId := i ;
        ReadNode( nd ) ;
        if ( _level = 127 ) or ( nd.FLevel = _level ) then begin
          tmp := GisPoint2Point( nd.FPos, _ptg ) ;
          if tmp < dist  then begin
            lv   := Abs( nd.FLevel ) ;
            dist := tmp ;
            k := i ;
          end
          else if tmp = dist then begin
            // select node base level
            if Abs( nd.FLevel ) < lv then begin
              lv   := Abs( nd.FLevel ) ;
              dist := tmp ;
              k := i ;
            end ;
          end ;
        end;

        i := oRtree.FindNext( 0 ) ;
      end ;
    finally
      FreeObject( nd ) ;
    end;

    Result := k ;
  end;

  procedure TGIS_NetworkFile.ReadLink(
    const _link : TGIS_NetworkLink
  ) ;
  var
    ipos : Cardinal ;
  begin
    assert( sizeOf( ipos              ) = 4 ) ;
    assert( sizeOf( _link.ShapeUid    ) = 4 ) ;
    assert( sizeOf( _link.NodeA       ) = 4 ) ;
    assert( sizeOf( _link.NodeB       ) = 4 ) ;
    assert( sizeOf( _link.Cost        ) = 8 ) ;
    assert( sizeOf( _link.LinkType    ) = 1 ) ;
    assert( sizeOf( _link.ReverseCost ) = 8 ) ;

    _link.Layer := _link.Parent.FLayer ;

    strmLinkIndex.Position := NETWORK_HEADER_SIZE +
                              sizeOf( ipos ) * _link.ObjectId ;

    strmLinkIndex.ReadCardinal( ipos ) ;
    strmLinkData.Position := ipos ;

    strmLinkData.ReadInteger  ( _link.ShapeUid    ) ;
    strmLinkData.ReadCardinal ( _link.NodeA       ) ;
    strmLinkData.ReadCardinal ( _link.NodeB       ) ;
    strmLinkData.ReadDouble   ( _link.Cost        ) ;
    strmLinkData.ReadDouble   ( _link.ReverseCost ) ;
    strmLinkData.ReadByte     ( _link.LinkType    ) ;
  end;

  procedure TGIS_NetworkFile.SaveLink(
    const _link : TGIS_NetworkLink
  ) ;
  var
    ipos : Cardinal ;
  begin
    assert( sizeOf( ipos              ) = 4 ) ;
    assert( sizeOf( _link.ShapeUid    ) = 4 ) ;
    assert( sizeOf( _link.NodeA       ) = 4 ) ;
    assert( sizeOf( _link.NodeB       ) = 4 ) ;
    assert( sizeOf( _link.Cost        ) = 8 ) ;
    assert( sizeOf( _link.LinkType    ) = 1 ) ;
    assert( sizeOf( _link.ReverseCost ) = 8 ) ;

    ipos := strmLinkData.Position ;
    strmLinkIndex.WriteCardinal( ipos ) ;

    strmLinkData.WriteInteger  ( _link.ShapeUid    ) ;
    strmLinkData.WriteCardinal ( _link.NodeA       ) ;
    strmLinkData.WriteCardinal ( _link.NodeB       ) ;
    strmLinkData.WriteDouble   ( _link.Cost        ) ;
    strmLinkData.WriteDouble   ( _link.ReverseCost ) ;
    strmLinkData.WriteByte     ( _link.LinkType    ) ;
  end;

//==============================================================================
// TGIS_NetworkNode
//==============================================================================

  constructor TGIS_NetworkNode.Create;
  begin
    inherited ;

    {$IFDEF GIS_NORECORDS}
    FPos := new TGIS_Point() ;
    {$ENDIF}
    clear_node( Self ) ;
    SetLength( FLinks, 0 ) ;
  end;

  function TGIS_NetworkNode.CreateCopy
    : TGIS_NetworkNode ;
  begin
    Result := TGIS_NetworkNode.Create ;

    Result.FParent        := self.FParent        ;
    Result.FObjectId      := self.FObjectId      ;
    Result.FLinks         := self.FLinks         ;
    Result.FPos           := _TGIS_Point(self.FPos) ;
    Result.FLevel         := self.FLevel         ;
    Result.FCurrentLink   := self.FCurrentLink   ;
    Result.FCurrentCost   := self.FCurrentCost   ;
    Result.FHeuristicCost := self.FHeuristicCost ;
    Result.FCurrentState  := self.FCurrentState  ;
  end;

  procedure TGIS_NetworkNode.doDestroy ;
  begin
    SetLength( FLinks, 0 ) ;
    FLinks := nil ;

    inherited ;
  end;

  function TGIS_NetworkNode.fget_LinkCount : Integer ;
  begin
    Result := length( FLinks ) ;
  end;

  function TGIS_NetworkNode.fget_Link(const _index : Integer): TGIS_NetworkLink;
  begin
    Result := FParent.GetLink( FLinks[ _index ] ) ;
  end;

  procedure TGIS_NetworkNode.LinkAdd(
    const _link : TGIS_NetworkLink
  );
  var
    i : Integer ;
  begin
    // avoid looped shapes (with same beginning and end) to be added
    // more the once
    for i := 0 to high( FLinks ) do begin
      if FLinks[i] = _link.FObjectId then begin
        exit ;
      end ;
    end;

    SetLength( FLinks, length( FLinks ) + 1 ) ;
    FLinks[ high( FLinks )] := _link.FObjectId ;
  end;

//==============================================================================
// TGIS_Network
//==============================================================================

  function TGIS_Network.AddNode(
    const _ptg   : TGIS_Point ;
    const _level : Integer
  ) : TGIS_NetworkNode ;
  var
    i  : Integer          ;
    nd : TGIS_NetworkNode ;
  begin
    nd := FindNode( _ptg, 0, _level ) ;
    if assigned( nd ) then begin
      if Ready then begin
        if lstNodes.FindItem( nd.FObjectId, i ) then begin
          Result := TGIS_NetworkNode( lstNodes[ i ] ) ;
        end
        else begin
          Result := nd.CreateCopy ;
          lstNodes.Add( Result ) ;
        end;
      end
      else
        Result := nd ;

      exit ;
    end ;

    nd  := TGIS_NetworkNode.Create ;

    nd.FObjectId := nodesId ;
    inc( nodesId ) ;

    nd.FParent := Self ;
    nd.FPos    := _ptg ;
    nd.FLevel := _level ;

    lstNodes.Add( nd ) ;
    oRtree.Insert( GisExtent( nd.FPos.X, nd.FPos.Y, nd.FPos.X, nd.FPos.Y ),
                   nd.FObjectId
                 ) ;

    Result := nd ;
  end ;

  procedure TGIS_Network.AddShape(
    const _shp   : TGIS_ShapeArc ;
    const _field : String
  ) ;
  var
    new_node  : TGIS_NetworkNode ;
    new_link  : TGIS_NetworkLink ;
    ptg1      : TGIS_Point       ;
    ptg2      : TGIS_Point       ;
    cost      : Double           ;
  begin
    if _shp.Length = 0 then exit ;

    new_link := TGIS_NetworkLink.Create;
    new_link.FObjectId := linksId;
    inc( linksId ) ;
    lstLinks.Add( new_link );
    new_link.FParent := Self ;

    with _shp do begin
      Lock( TGIS_Lock.Projection ) ;
      ptg1 := GetPoint( 0, 0 ) ;
      ptg2 := GetPoint( GetNumParts-1, GetPartSize(GetNumParts-1)-1 ) ;
      Unlock ;
    end ;

    if not IsStringEmpty( _field ) then
      cost := VarToDouble( _shp.GetField( _field ) )
    else
      cost := 0 ;

    new_link.NodeA       := 0             ;
    new_link.NodeB       := 0             ;
    new_link.ShapeUid    := _shp.Uid      ;
    new_link.Layer       := _shp.Layer    ;
    new_link.Cost        := cost          ;
    new_link.ReverseCost := new_link.Cost ;
    new_link.LinkType    := 0             ;

    if assigned( FOnLink ) then begin
      {$IFDEF OXYGENE}
        FOnLink( self, TGIS_NetworkLinkEventArgs.Create( _shp, new_link ) ) ;
      {$ELSE}
        FOnLink( self, _shp, new_link ) ;
      {$ENDIF}
    end ;

    // Events store levels temporary in NodeA & NodeB

    new_node := AddNode( ptg1, new_link.NodeA ) ;
    new_link.NodeA := new_node.FObjectId ;
    new_node.LinkAdd( new_link ) ;

    new_node := AddNode( ptg2, new_link.NodeB ) ;
    new_link.NodeB := new_node.FObjectId ;
    new_node.LinkAdd(new_link ) ;

  end ;

  procedure TGIS_Network.Delete;
  begin
    FreeObject( oStorage ) ;
    oStorage := TGIS_NetworkFile.Create( FStoragePath, False ) ;
    if assigned( oStorage ) then
      oStorage.Delete ;

    Clean ;
  end;

  procedure TGIS_Network.Clean;
  var
    i: Integer;
  begin
    for i := 0 to lstNodes.Count - 1 do
      clear_node( TGIS_NetworkNode( lstNodes[ i ] ) ) ;
  end;

  procedure TGIS_Network.CleanDynamicLinks;
  var
    i : Integer          ;
    o : TGIS_NetworkLink ;
  begin
    for i := lstLinks.Count - 1 downto 0 do begin
      o := TGIS_NetworkLink( lstLinks[ i ] ) ;
      if ( ( o.ObjectId and NETWORK_OBJECT_INMEMORY ) <> 0 ) then begin
        o.Cost := -1 ;
        o.ReverseCost := -1 ;
      end
      else
        break ;
    end ;
  end;

  constructor TGIS_Network.Create(
    const _layer      : TGIS_LayerVector ;
    const _forcedpath : String
  ) ;
  begin
    inherited Create ;
    FLayer := _layer ;
    tmpLink := TGIS_NetworkLink.Create ;
    tmpLink.FParent := Self ;

    oRtree := TGIS_RTree.Create(  '' ) ;

    lstNodes        := TGIS_NetworkObjectList.Create ;
    lstLinks        := TGIS_NetworkObjectList.Create ;

    if IsStringEmpty( _forcedpath ) then begin
      if IsServerPath( FLayer.Path ) then
        raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_BAD_CALL ), '', 0 ) ;
      FStoragePath := FLayer.Path ;
    end
    else
      FStoragePath := _forcedpath ;

    oStorage := TGIS_NetworkFile.Create( FStoragePath, False ) ;

    if not Ready then
      FreeObject( oStorage ) ;

    linksId := 0 ;
    nodesId := 0 ;
  end;

  procedure TGIS_Network.doDestroy ;
  begin
    FreeObject( tmpLink ) ;

    FreeObject( oRtree          ) ;

    FreeObject( lstNodes        ) ;
    FreeObject( lstLinks        ) ;

    FreeObject( oStorage        ) ;

    inherited ;
  end;

  function TGIS_Network.fget_Ready : Boolean ;
  begin
    Result := assigned( oStorage ) and oStorage.Ready ;
  end ;

  function TGIS_Network.FindNode(
    const _ptg  : TGIS_Point ;
    const _prec : Double ;
    const _level : ShortInt
  ) : TGIS_NetworkNode ;
  var
    i    : TGIS_Uid ;
    tmp  : Double  ;
    dist : Double  ;
    nd   : TGIS_NetworkNode ;
    k    : Integer ;
    fnd  : Boolean ;
    lv   : Integer ;
  begin
    fnd    := False ;

    dist := GIS_MAX_DOUBLE  ; // really far away
    lv   := GIS_MAX_INTEGER ;

    k := -1 ;

    i := oRtree.FindFirst( 0, GisExtent( _ptg.X - _prec, _ptg.Y - _prec,
                                         _ptg.X + _prec, _ptg.Y + _prec
                                       )
                         ) ;
    while i >= 0 do begin
      nd := GetNode( i ) ;
      if ( _level = 127 ) or ( nd.FLevel = _level ) then begin
        tmp := GisPoint2Point( nd.FPos, _ptg ) ;
        if tmp < dist  then begin
          lv   := Abs( nd.FLevel ) ;
          dist := tmp ;
          fnd := True ;
          k := i ;
        end
        else if tmp = dist then begin
          // select node base level
          if Abs( nd.FLevel ) < lv then begin
            lv   := Abs( nd.FLevel ) ;
            dist := tmp ;
            fnd := True ;
            k := i ;
          end ;
        end ;
      end;
      i := oRtree.FindNext( 0 ) ;
    end ;

    Result := nil ;

    if fnd then
      Result := GetNode( k )
    else begin
      if assigned( oStorage ) then begin
        k := oStorage.FindNode( _ptg, _prec, _level )  ;
        if k >= 0  then
          Result := GetNode( k );
      end ;
    end;
  end;

  function TGIS_Network.FindNode(
    const _ptg  : TGIS_Point ;
    const _prec : Double
  ) : TGIS_NetworkNode ;
  begin
    Result := FindNode( _ptg, _prec, 127 ) ;
  end;

  function TGIS_Network.GetLink(
    const _id : Cardinal
  ) : TGIS_NetworkLink ;
  var
    l : TGIS_NetworkLink ;
  begin
    // get from in-memory list
    Result := TGIS_NetworkLink( lstLinks.Get( _id ) ) ;
    if assigned( Result ) then exit ;

    // not in-memory - get from disk

    if not assigned( oStorage ) then
      oStorage := TGIS_NetworkFile.Create( FStoragePath, False ) ;

    tmpLink.FObjectId := _id ;
    oStorage.ReadLink(tmpLink);

    l := TGIS_NetworkLink.Create ;
    l.ShapeUid    := tmpLink.ShapeUid ;
    l.NodeA       := tmpLink.NodeA ;
    l.NodeB       := tmpLink.NodeB ;
    l.Cost        := tmpLink.Cost ;
    l.ReverseCost := tmpLink.ReverseCost ;
    l.LinkType    := tmpLink.LinkType ;
    l.Layer       := tmpLink.Layer ;
    l.Parent      := tmpLink.Parent ;
    l.ObjectId    := tmpLink.ObjectId ;

    lstLinks.Add( l );

    Result := tmpLink ;
  end ;

  function TGIS_Network.GetNode(
    const _id : Cardinal
  ) : TGIS_NetworkNode ;
  var
    nd  : TGIS_NetworkNode ;
  begin
    // already a working object?

    Result := nil ;

    if _id < NETWORK_OBJECT_INMEMORY then
      if _id < Cardinal( length( arNodes ) ) then
         Result := arNodes[ _id ] ;
    if assigned( Result ) then exit ;

    // not working object - get from in-memory list
    Result := TGIS_NetworkNode( lstNodes.Get( _id ) ) ;
    if assigned( Result ) then exit ;

    // not in-memory - fetch from disk
    nd := TGIS_NetworkNode.Create ;
    nd.FObjectId := _id ;
    nd.FParent := self ;

    if not assigned( oStorage ) then
      oStorage := TGIS_NetworkFile.Create( FStoragePath, False ) ;

    nd.FObjectId := _id ;
    oStorage.ReadNode( nd ) ;

    lstNodes.Add( nd );
    Result := nd ;

    if _id < NETWORK_OBJECT_INMEMORY then begin
      if _id >= Cardinal( length( arNodes ) ) then
        SetLength( arNodes, _id + 1 ) ;
      arNodes[ _id ] := nd ;
    end;
  end ;

  function TGIS_Network.GetShape(
    const _id: Cardinal
  ) : TGIS_ShapeArc ;
  var
    lnk : TGIS_NetworkLink ;
  begin
    lnk := GetLink( _id ) ;

    Result := TGIS_ShapeArc( lnk.Layer.GetShape( lnk.ShapeUid ) ) ;
  end;

  procedure TGIS_Network.Serialize;
  var
    i   : Integer ;
    lk  : TGIS_NetworkLink ;
    nd  : TGIS_NetworkNode ;
    stg : TGIS_NetworkFile ;
  begin
    stg := TGIS_NetworkFile.Create( FStoragePath, True ) ;
    try
      for i := 0 to lstLinks.Count - 1 do begin
        lk := TGIS_NetworkLink( lstLinks[i] ) ;
        stg.SaveLink( lk );
      end;
      for i := 0 to lstNodes.Count - 1 do begin
        nd := TGIS_NetworkNode( lstNodes[i] ) ;
        stg.SaveNode( nd );
      end;
      stg.Commit ;
    finally
      FreeObject( stg ) ;
    end;

    SetLength( arNodes, 0 );
    lstNodes.Clear ;
    lstLinks.Clear ;

    FreeObject( oRtree ) ;
    oRtree := TGIS_RTree.Create(  '' ) ;

    oStorage := TGIS_NetworkFile.Create( FStoragePath, False ) ;

  end;

  procedure TGIS_Network.Open;
  begin
    FreeObject( oStorage ) ;
    oStorage := TGIS_NetworkFile.Create( FStoragePath, False ) ;

    assert( oStorage.Ready ) ;

    if not oStorage.Ready then
       raise EGIS_Exception.Create(
               _rsrc( GIS_RS_ERR_FILEBADFORMAT ),
               FStoragePath,
               0
             );

    nodesId := NETWORK_OBJECT_INMEMORY ;
    linksId := NETWORK_OBJECT_INMEMORY ;
  end;

//==============================================================================
// TGIS_NetworkObjectList
//==============================================================================

  constructor TGIS_NetworkObjectList.Create;
  begin
    inherited Create ;
  end;

  procedure TGIS_NetworkObjectList.Add(
    const _obj : TGIS_NetworkObject
  ) ;
  var
    idx : Integer ;
  begin
    if FindItem( _obj.FObjectId, idx ) then begin
      // ignore duplicated objects
    end
    else begin
      Self.Insert( idx, _obj );
    end;
  end;

  function TGIS_NetworkObjectList.FindItem(
    const _id   : Cardinal ;
    var   _ipos : Integer
  ) : Boolean ;
  var
    ilow  : Integer  ;
    ihigh : Integer  ;
    id    : Cardinal ;
  begin
    Result := False ;

    _ipos := 0 ;
    ilow  := 0 ;
    ihigh := Count - 1 ;

    while ilow <= ihigh do begin
      _ipos  := ( ihigh + ilow ) div 2 ;

      id := TGIS_NetworkObject( Items[ _ipos ] ).FObjectId ;
      if      id < _id then begin
                              ilow  := _ipos + 1 ;
                              _ipos := ilow ; // for finding a best
                                              // fit position
    end
      else if id > _id then ihigh := _ipos - 1
    else begin
                              Result := True ;
                              break ;
    end;
  end ;
  end ;

  function TGIS_NetworkObjectList.Get(
    const _id: Cardinal
  ) : TGIS_NetworkObject ;
  var
    idx : Integer ;
  begin
    if FindItem( _id, idx ) then
      Result := TGIS_NetworkObject( Items[idx] )
    else
      Result := nil ;
  end;

//==================================== END =====================================
end.
