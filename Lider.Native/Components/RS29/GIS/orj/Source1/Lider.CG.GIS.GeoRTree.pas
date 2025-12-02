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
  Encapsulation of the Rtree index.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoRTree ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoRTree"'}
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

{$IFNDEF OXYGENE}
  {$ALIGN OFF}
{$ENDIF}

interface

{$IFDEF CLR}
  uses
    System.Runtime.InteropServices,
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Classes,
    System.Math,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoClasses ;
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

const
  {$IFNDEF GIS_BIGRTREEPAGE}
    {#gendoc:hide}
    GIS_RT_PAGESIZE     = 512  ;
    {#gendoc:hide}
    GIS_RT_PAGECNT      =  14  ;
  {$ELSE}
    // experimental only
    GIS_RT_PAGESIZE     = 8192 ;
    GIS_RT_PAGECNT      =  227 ;
  {$ENDIF}
  {#gendoc:hide}
  GIS_RT_COPYRIGHT      = 'LicadGIS DK RTree' ; // ilker deðiþtirme
  {#gendoc:hide}
  GIS_RT_VERSION        = 6 ;
  {#gendoc:hide}
  GIS_RT_SUBVERSION     = 2 ;
  {#gendoc:hide}
  GIS_RT_MAGIC          = 71334573 ;
  {#gendoc:hide}
  GIS_RT_PAGE_NOTLEAF   = 1 ;
  {#gendoc:hide}
  GIS_RT_PAGE_LEAF      = 2 ;
  {#gendoc:hide}
  GIS_RT_DIRTY          = 0 ;
  {#gendoc:hide}
  GIS_RT_COMMITED       = 73 ;
  {#gendoc:hide}
  GIS_RT_ERROR_INMEMORY = 'In-memory RTree' ;

type

  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential)]
  {$ENDIF}

  /// <summary>
  ///   For internal use only. Item (entry) in rtree index.
  /// </summary>
  /// <remarks>
  ///   <note type="note">
  ///    For internal use of TGIS_RTree.
  ///    </note>
  /// </remarks>
  TGIS_RTreeItem = {$IFDEF OXYGENE} public {$ENDIF}
                   {$IFDEF GIS_PACKED} packed {$ENDIF}
                   {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}

      /// <summary>
      ///   Extent of entry. Treated as a key.
      /// </summary>
      Extent : TGIS_Extent {$IFDEF GIS_NORECORDS} := new TGIS_Extent {$ENDIF} ;

      /// <summary>
      ///   Identifier of entry. Typically for storing Uid.
      /// </summary>
      Uid : Int32 ;
    {$IFDEF OXYGENE}
      /// <summary>
      ///   Constructor for TGIS_RTreeItem
      /// </summary>
      constructor Create ; overload;
      /// <summary>
      ///   Constructor for TGIS_RTreeItem
      /// </summary>
      /// <param name="_ex">
      ///   Extent to be passed
      /// </param>
      /// <param name="_uid">
      ///   Uid to be passed
      ///  </param>
      constructor Create(const _ex : TGIS_Extent; const _uid : Int32); overload;
      /// <summary>
      ///   Function that gives us size of RTreeItem
      /// </summary>
      /// <returns>
      ///   returns size
      /// </returns>
      class function  SizeOf           : Integer ;
                                       {$IFDEF GIS_STATIC} static; {$ENDIF}
      /// <summary>
      ///   convert bytes to structure
      /// </summary>
      /// <param name="_bytes">
      ///   Array of bytes
      /// </param>
      /// <param name="_offset">
      ///   offset to start at
      /// </param>
      procedure       BytesToStructure ( const _bytes : TBytes; const _offset : Integer ) ;
      /// <summary>
      ///   Converts structure to bytes
      /// </summary>
      /// <returns>
      ///   returns array of bytes
      /// </returns>
      function        StructureToBytes : TBytes ;
      {$IFDEF GIS_NORECORDS}
        function MakeCopy : TGIS_RTreeItem ;
      {$ENDIF}
    {$ENDIF}
  end ;

  {$IFNDEF GIS_NORECORDS}
    {#gendoc:hide}
    /// <summary>
    ///   Helper to ensure RTree item copying on non record enabled platforms.
    ///   For internal use only.
    /// </summary>
    _TGIS_RTreeItem = TGIS_RTreeItem ;
  {$ELSE}
    {#gendoc:hide}
    /// <summary>
    ///   Helper to ensure RTree item copying on non record enabled platforms.
    ///   For internal use only.
    /// </summary>
    function _TGIS_RTreeItem( const _rti : TGIS_RTreeItem ) : TGIS_RTreeItem ;
    type
  {$ENDIF}

  /// <summary>
  ///   For internal use only. Header page.
  /// </summary>
  TGIS_RTreeHeader = {$IFDEF OXYGENE} public {$ENDIF}
                     {$IFDEF GIS_PACKED} packed {$ENDIF}
                     {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}

      /// <summary>
      ///   Place for Magic number.
      /// </summary>
      Magic     : Int32 ;

      /// <summary>
      ///   Place for version. Different version can be incompatible.
      /// </summary>

      Version   : Int32 ;

      /// <summary>
      ///   Place for subversion. Different sun versions are compatible.
      /// </summary>
      Subversion : Int32 ;

      {$IFDEF OXYGENE}
        /// <summary>
        ///   Copyright text.
        /// </summary>
        Copyright : String ;
      {$ELSE}

        /// <summary>
        ///   Copyright text.
        /// </summary>
        Copyright : Array[0..255] of Byte ;
      {$ENDIF}

      /// <summary>
      ///   RT_DIRTY if storage was not saved properly, otherwise RT_COMMITED
      /// </summary>
      Committed  : Int32 ;

      /// <summary>
      ///   Number of pages.
      /// </summary>
      PageCount : Int32 ;

      /// <summary>
      ///   Number of items.
      /// </summary>
      ItemCount : Int32 ;

      /// <summary>
      ///   Number of last free page (if &gt;0).
      /// </summary>
      LastFree  : Int32 ;

      /// <summary>
      ///   Number of root page.
      /// </summary>
      Root      : Int32 ;

      /// <summary>
      ///   To fill page size.
      /// </summary>
      Gap :
        {$IFDEF OXYGENE}
          TBytes  ;
        {$ELSE}
          array[0..GIS_RT_PAGESIZE-288-1] of Byte ;
        {$ENDIF}

    {$IFDEF OXYGENE}
      /// <summary>
      ///   Clear RTreeHeader.
      /// </summary>
      procedure       Clear            ;
      /// <summary>
      ///   Function that gives us size of RTreeHeader
      /// </summary>
      /// <returns>
      ///   returns size
      /// </returns>
      class function  SizeOf           : Integer ;
                                       {$IFDEF GIS_STATIC} static; {$ENDIF}
      /// <summary>
      ///   convert bytes to structure
      /// </summary>
      /// <param name="_bytes">
      ///   Array of bytes
      /// </param>
      procedure       BytesToStructure ( const _bytes : TBytes
                                       ) ;
      /// <summary>
      ///   Converts structure to bytes
      /// </summary>
      /// <returns>
      ///   returns array of bytes
      /// </returns>
      function        StructureToBytes : TBytes ;
    {$ENDIF}
  end ;

  {$IFDEF CLR}
    [StructLayout(LayoutKind.Sequential)]
  {$ENDIF}

  /// <summary>
  ///   For internal use only. Data page.
  /// </summary>
  TGIS_RTreePageData = {$IFDEF OXYGENE} public {$ENDIF}
                       {$IFDEF GIS_PACKED} packed {$ENDIF}
                       {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}

      /// <summary>
      ///   Page type represented by integer
      /// </summary>
      PageType : Int32 ;

      /// <summary>
      ///   Number of entries.
      /// </summary>
      Count : Int32 ;

      /// <summary>
      ///   Array of entries.
      /// </summary>
      {$IFDEF OXYGENE}
      Entries  : array of TGIS_RTreeItem ;
      {$ELSE}
      Entries  : array[0..GIS_RT_PAGECNT-1] of TGIS_RTreeItem ;
      {$ENDIF}
    {$IFDEF GIS_BIGRTREEPAGE}
        {$IFDEF CLR}
          [MarshalAs(UnmanagedType.ByValArray, SizeConst=12)]
        {$ENDIF}
        /// <summary>
        ///   To fill page size.
        /// </summary>
        Gap : Array [1..12] of byte ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      /// <summary>
      ///   Constructor for TGIS_RTreePageData
      /// </summary>
      constructor Create ; overload;
      /// <summary>
      ///   Constructor for TGIS_RTreePageData
      /// </summary>
      /// <param name="_page_type">
      ///   Type of page represented by Integer
      /// </param>
      /// <param name="_count">
      ///   Number of entries
      /// </param>
      /// <param name="_entries">
      ///   Array of RTreeItems
      /// </param>
      constructor Create(const _page_type : Int32 ; const _count :Int32 ; const _entries : array of TGIS_RTreeItem); overload;
    {$ENDIF}
    {$IFDEF OXYGENE}
      /// <summary>
      ///   Read from stream
      /// </summary>
      /// <param name="_stream">
      ///   Stream to be read from
      /// </param>
      /// <returns>
      ///   returns position
      /// </returns>
      function  Read  ( _stream : TStream ) : Integer ;
      /// <summary>
      ///   Write to stream
      /// </summary>
      /// <param name="_stream">
      ///   Stream to be write in
      /// </param>
      /// <returns>
      ///   returns position
      /// </returns>
      function  Write ( _stream : TStream ) : Integer ;
      {$IFDEF GIS_NORECORDS}
        function MakeCopy : TGIS_RTreePageData ;
      {$ENDIF}
    {$ENDIF}
  end ;
  {$IFNDEF GIS_NORECORDS}
    {#gendoc:hide}
    /// <summary>
    ///   Helper to ensure RTree page data copying on non record enabled platforms.
    ///   For internal use only.
    /// </summary>
    _TGIS_RTreePageData = TGIS_RTreePageData ;
  {$ELSE}
    {#gendoc:hide}
    /// <summary>
    ///   Helper to ensure RTree page data copying on non record enabled platforms.
    ///   For internal use only.
    /// </summary>
    function _TGIS_RTreePageData( const _rtpd : TGIS_RTreePageData ) : TGIS_RTreePageData ;
    type
  {$ENDIF}

  /// <summary>
  ///   For internal use only. Encapsulation of page-level operations.
  /// </summary>
  TGIS_RTreePage = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Object )

    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}

        /// <summary>
        ///   Data of page.
        /// </summary>
        Data : TGIS_RTreePageData ;

        /// <summary>
        ///   Number of this page.
        /// </summary>
        PageNo : Int32 ;

        /// <summary>
        ///   Stack with a page dependencies.
        /// </summary>
        Stack : TGIS_StackInteger ;

    protected // property access routines

      function  fget_Count      : Integer ;
      procedure fset_Count      ( const _val : Integer ) ;
      function  fget_Uid        ( _idx : Integer ): TGIS_Uid ;
      function  fget_Extent     ( _idx : Integer ) : TGIS_Extent  ;
      function  fget_Item       ( _idx : Integer ) : TGIS_RTreeItem ;
      procedure fset_Item       ( _idx : Integer; const _val : TGIS_RTreeItem ) ;
      function  fget_IsLeaf     : Boolean ;
      function  fget_IsRoot     : Boolean ;
      function  fget_IsFull     : Boolean ;
      function  fget_IsHalfFull : Boolean ;
    private

      /// <summary>
      ///   Cost of growing an item by extent.
      /// </summary>
      /// <param name="_pos">
      ///   position of an item to be grown
      /// </param>
      /// <param name="_ext">
      ///   extent by which an item will be grown
      /// </param>
      function  itemGrowCost   ( const _pos  : Integer ;
                                 const _ext  : TGIS_Extent
                               ) : Double ;

    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy      ; override;
    public

      /// <summary>
      ///   Create an instance.
      /// </summary>
      constructor  Create      ;

      /// <summary>
      ///   Insert new item into the page. Item will be inserted before any
      ///   entry that contains or is equal to the extent of the inserted item.
      /// </summary>
      /// <param name="_item">
      ///   item to be inserted
      /// </param>
      procedure Insert         ( const _item  : TGIS_RTreeItem
                               ) ;

      /// <summary>
      ///   Insert new item into the page. Item will be inserted based on the
      ///   position provided by pos
      /// </summary>
      /// <param name="_item">
      ///   item to be inserted
      /// </param>
      /// <param name="_pos">
      ///   position before which the item must be inserted
      /// </param>
      procedure InsertOnPos    ( const _item  : TGIS_RTreeItem ;
                                 const _pos   : Integer
                               ) ;

      /// <summary>
      ///   Delete a new item from the page.
      /// </summary>
      /// <param name="_item">
      ///   item to be deleted
      /// </param>
      procedure Delete         ( const _item  : TGIS_RTreeItem
                               ) ;

      /// <summary>
      ///   Delete an item from page.
      /// </summary>
      /// <param name="_pos">
      ///   position of an item within entries.
      /// </param>
      procedure DeleteOnPos    ( const _pos   : Integer
                               ) ;

      /// <summary>
      ///   Find the position of an item with a given extent.
      /// </summary>
      /// <param name="_ext">
      ///   extent to be found
      /// </param>
      /// <returns>
      ///   returns position on page
      /// </returns>
      function  Find           ( const _ext   : TGIS_Extent
                               ) : Integer ;

      /// <summary>
      ///   Find the best position to insert a new item based on the cost of
      ///   growing.
      /// </summary>
      /// <param name="_item">
      ///   item we want to insert
      /// </param>
      /// <returns>
      ///   returns position of insert point
      /// </returns>
      function  FindInsertPoint( const _item  : TGIS_RTreeItem
                               ) : Integer ;

      /// <summary>
      ///   Find a position of an item based on its Uid
      /// </summary>
      /// <param name="_item">
      ///   item we want to find
      /// </param>
      /// <returns>
      ///   returns uid
      /// </returns>
      function  FindUid        ( const _item  : TGIS_RTreeItem
                               ) : TGIS_Uid ;

      /// <summary>
      ///   Calculate an extent which encompass all entries.
      /// </summary>
      /// <returns>
      ///   returns max extent
      /// </returns>
      function  MaxExtent      : TGIS_Extent ;

      /// <summary>
      ///   Guttman's poly-time split algorithm. Self page becomes the left
      ///   node. Given page becomes right node.
      /// </summary>
      /// <param name="_right">
      ///   right node.
      /// </param>
      procedure Split          ( const _right : TGIS_RTreePage ) ;

    public

        /// <summary>
        ///   Number of entries in this page.
        /// </summary>
        property Count : Integer read fget_Count write fset_Count ;

        /// <summary>
        ///   Uid for an item given by the index.
        /// </summary>
        /// <param name="_idx">
        ///   id of item
        /// </param>
        property Uid[ _idx : Integer ] : TGIS_Uid read fget_Uid ;

        /// <summary>
        ///   Extent for an item given by the index.
        /// </summary>
        /// <param name="_idx">
        ///   id of item
        /// </param>
        property Extent[ _idx : Integer ] : TGIS_Extent read fget_Extent ;

        /// <summary>
        ///   Item given by the index.
        /// </summary>
        /// <param name="_idx">
        ///   id of item
        /// </param>
        property Item[ _idx : Integer ] : TGIS_RTreeItem read fget_Item
                                                         write fset_Item;

        /// <summary>
        ///   True if page is a leaf.
        /// </summary>
        property IsLeaf : Boolean read fget_IsLeaf ;

        /// <summary>
        ///   True if page is a root.
        /// </summary>
        property IsRoot : Boolean read fget_IsRoot ;

        /// <summary>
        ///   True if page is full (must be split).
        /// </summary>
        property IsFull : Boolean read fget_IsFull ;

      /// <summary>
      ///   True if page is partially empty (must be joined.).
      /// </summary>
      property IsHalfFull : Boolean read fget_IsHalfFull ;
  end ;

  {$IFDEF OXYGENE}
    T_cursorState_RTree nested in TGIS_RTree = {$IFDEF GIS_NORECORDS} class {$ELSE} record {$ENDIF}
      public
        lastUid : TGIS_Uid ;

        /// <summary>
        ///   Stack with a "history" of last Find operation.
        /// </summary>
        searchStack : TGIS_StackInteger ;

        /// <summary>
        ///   Item on which FindNext operation will be performed.
        /// </summary>
        searchItm : TGIS_RTreeItem ;

        /// <summary>
        ///   Page used for search operation. Preallocated to speed-up
        //    operations.
        /// </summary>
        searchPage : TGIS_RTreePage ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Encapsulation of tree-level operations.
  /// </summary>
  TGIS_RTree = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )

    private // property internal value

        /// <summary>
        ///   Path to index file.
        /// </summary>
        FPath : String ;

        /// <summary>
        ///   To fill page size.
        /// </summary>
        FAge :
          {$IFDEF CLR}
            DateTime ;
          {$ELSE}
            TDateTime ;
          {$ENDIF}

        /// <summary>
        ///   True if index read only.
        /// </summary>
        FIsReadOnly : Boolean ;

        /// <summary>
        ///   True. if index is in-memory based. False, if index is disk-based.
        /// </summary>
        FInMemory : Boolean ;

    private // property access routines

      function  fget_IsEmpty  : Boolean ;
      function  fget_IsCommited : Boolean ;

    private // other private values

        /// <summary>
        ///   Stream (memory or disk based).
        /// </summary>
        streamObj : TStream ;

        /// <summary>
        ///   Header object.
        /// </summary>
        headerObj : TGIS_RTreeHeader ;

        /// <summary>
        ///   True if Header object was touched.
        /// </summary>
        headerTouched : Boolean ;

        /// <summary>
        ///   True if Header object was touched.
        /// </summary>
        rtreeTouched : Boolean ;

      {$IFDEF OXYGENE}
        cursorState : array of T_cursorState_RTree ;
      {$ELSE}
        cursorState : array of record
          lastUid : TGIS_Uid ;

          /// <summary>
          ///   Stack with a "history" of last Find operation.
          /// </summary>
          searchStack : TGIS_StackInteger ;

          /// <summary>
          ///   Item on which FindNext operation will be performed.
          /// </summary>
          searchItm : TGIS_RTreeItem ;

          /// <summary>
          ///   Page used for search operation. Preallocated to speed-up
          //   operations.
          /// </summary>
          searchPage : TGIS_RTreePage ;
        end ;
      {$ENDIF}

    private // other private routines

      /// <summary>
      ///   Do internal Rtree creation.
      /// </summary>
      procedure reCreate     ;

      /// <summary>
      ///   Raise exception if res value &lt;&gt; RTGIS_PAGESIZE. To be used as
      ///   a checker for page read/write operations.
      /// </summary>
      /// <param name="_res">
      ///   resulting value from read/write operation
      /// </param>
      procedure checkRW      ( const _res : Integer ) ;

      /// <summary>
      ///   Initialize the stream. Create a header page and a root page.
      /// </summary>
      procedure streamInit   ;

      /// <summary>
      ///   Open the stream and check the header page.
      /// </summary>
      procedure streamOpen   ;

      /// <summary>
      ///   Assign a page from the deleted page chain. Contents the of page is
      ///   undefined.
      /// </summary>
      function  pageFromFree : Integer ;

      /// <summary>
      ///   Assign a new page as a standard leaf page.
      /// </summary>
      /// <param name="_root">
      ///   if True, then a new root page will be created
      /// </param>
      function  pageAssign   ( const _root : Boolean
                             ) : Integer ;

      /// <summary>
      ///   Read a page from the stream.
      /// </summary>
      /// <param name="_page_obj">
      ///   page object (must be allocated elsewhere)
      /// </param>
      /// <param name="_page_no">
      ///   number of the page to be read
      /// </param>
      procedure pageRead     ( var   _page_obj : TGIS_RTreePage ;
                               const _page_no  : Integer
                             ) ;

      /// <summary>
      ///   Write a page to the stream.
      /// </summary>
      /// <param name="_page_obj">
      ///   page object (must be allocated elsewhere)
      /// </param>
      procedure pageWrite    ( var   _page_obj : TGIS_RTreePage
                             ) ;

      /// <summary>
      ///   Mark a page as unused.
      /// </summary>
      /// <param name="_page_no">
      ///   number of the page to be marked
      /// </param>
      procedure pageMarkFree ( const _page_no  : Integer ) ;

      /// <summary>
      ///   Split a page into the left part, which become a page_It, and the
      ///   right part based on item.
      /// </summary>
      /// <param name="_item">
      ///   item treated as the "threshold" in splitting.
      /// </param>
      procedure pageSplit    ( const _item     : TGIS_RTreeItem ;
                               var   _page_lt  : TGIS_RTreePage
                             ) ;

      /// <summary>
      ///   Delete an item from the page and propagate changes to the parent
      ///   pages.
      /// </summary>
      /// <param name="_page_obj">
      ///   page will bust be propagated
      /// </param>
      procedure pageDelete   ( var   _page_obj : TGIS_RTreePage ;
                               const _item     : TGIS_RTreeItem ) ;

      /// <summary>
      ///   Propagate extent changes to the parent pages.
      /// </summary>
      /// <param name="_page_obj">
      ///   page, that will be propagated
      /// </param>
      procedure pagePropagate( var   _page_obj : TGIS_RTreePage ) ;

      /// <summary>
      ///   Read header page.
      /// </summary>
      procedure headerRead  ;

      /// <summary>
      ///   Write header page - but only if was modified.
      /// </summary>
      procedure headerWrite  ;

      /// <summary>
      ///   Mark RTREE as committed.
      /// </summary>
      procedure markCommited ;

      /// <summary>
      ///   Mark RTREE as dirty.
      /// </summary>
      procedure markDirty    ;

      /// <summary>
      ///   Allocate and initialize cursor.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor number to be allocated
      /// </param>
      procedure cursorOpen   ( const _cursor : Integer ) ;

      /// <summary>
      ///   Close and free all cursor.
      /// </summary>
      procedure cursorCloseAll ;

      function  pathForError : String ;
    protected

      /// <summary>
      ///   Destroy instance.
      /// </summary>
      procedure doDestroy    ; override;
    public // public routines

      /// <summary>
      ///   Create an instance. If path is not provided, then in-memory index
      ///   will be created. If disk-based file has a the status "not
      ///   committed", then it will be cleared.
      /// </summary>
      /// <param name="_path">
      ///   Path to a disk based index
      /// </param>
      constructor  Create    ( const _path : String ) ;

      /// <summary>
      ///   Set commit state.
      /// </summary>
      /// <param name="_state">
      ///   if True, then Rtree will be destroyed in state "Committed"; it means
      ///   that the content of index is consistent with layer; if False means
      ///   that "Committed" state will not be touched
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Use just before destroy the Rtree
      ///    </note>
      /// </remarks>
      procedure   Commit     ( const _state : Boolean ) ;

      /// <summary>
      ///   Clear a whole RTree.
      /// </summary>
      procedure   Clear      ;

      /// <summary>
      ///   Insert a new item into the tree.
      /// </summary>
      /// <param name="_ext">
      ///   extent of the item to be inserted
      /// </param>
      /// <param name="_uid">
      ///   uid of the item to be inserted
      /// </param>
      procedure   Insert     ( const _ext     : TGIS_Extent ;
                               const _uid     : TGIS_Uid
                             ) ;

      /// <summary>
      ///   Delete an item from the tree.
      /// </summary>
      /// <param name="_ext">
      ///   extent of item to be deleted
      /// </param>
      /// <param name="_uid">
      ///   uid of item to be deleted
      /// </param>
      /// <returns>
      ///   returns true if deleted
      /// </returns>
      function    Delete     ( const _ext     : TGIS_Extent ;
                               const _uid     : TGIS_Uid
                             ) : Boolean ;
      /// <summary>
      ///   Find the first occurrence of an item that meets the extent and
      ///   uid parameters.
      /// </summary>
      /// <param name="_cursor">
      ///   starting point
      /// </param>
      /// <param name="_ext">
      ///   extent of item to be found
      /// </param>
      /// <returns>
      ///   uid of item to be found; if uid =-1 then it will be meaningless
      /// </returns>
      function  FindFirst    ( const _cursor  : Integer ;
                               const _ext     : TGIS_Extent
                             ) : TGIS_Uid ; overload;

      /// <summary>
      ///   Find the first occurrence of an item that meets the extent and uid
      ///   parameters.
      /// </summary>
      /// <param name="_cursor">
      ///   starting point
      /// </param>
      /// <param name="_ext">
      ///   extent of item to be found
      /// </param>
      /// <param name="_uid">
      ///   uid of item to be found; if uid =-1 then it will be meaningless
      /// </param>
      /// <returns>
      ///   uid of item to be found; if uid =-1 then it will be meaningless
      /// </returns>
      function    FindFirst  ( const _cursor  : Integer ;
                               const _ext     : TGIS_Extent ;
                               const _uid     : TGIS_Uid
                             ) : TGIS_Uid ; overload;

      /// <summary>
      ///   Find the next occurrence of the item defined in FindFirst.
      /// </summary>
      /// <param name="_cursor">
      ///   starting point
      /// </param>
      /// <returns>
      ///   uid of item to be found; if uid =-1 then it will be meaningless
      /// </returns>
      function    FindNext   ( const _cursor  : Integer
                             ) : TGIS_Uid ;

      /// <summary>
      ///   Verify if RTREE is valid in a sense of multiuser access.
      /// </summary>
      /// <returns>
      ///   False, if RTee was modified by other user.
      /// </returns>
      function    Valid : Boolean ;

    public // properties

        /// <summary>
        ///   Path to index file.
        /// </summary>
        property Path : String  read FPath ;

        /// <summary>
        ///   Date of file.
        /// </summary>
        property Age :
          {$IFDEF CLR}
            DateTime
          {$ELSE}
            TDateTime
          {$ENDIF}
          read FAge ;

        /// <summary>
        ///   True if index is read only.
        /// </summary>
        property IsReadOnly : Boolean read FIsReadOnly ;

        /// <summary>
        ///   True if index is in-memory based. False, if index is disk-based.
        /// </summary>
        property InMemory : Boolean read FInMemory ;

        /// <summary>
        ///   True if index is empty.
        /// </summary>
        property IsEmpty : Boolean read fget_IsEmpty ;
        /// <summary>
        ///   True if committed.
        /// </summary>
        property IsCommited : Boolean read fget_IsCommited ;
    end ;

//##############################################################################
implementation

{$IFDEF CLR}
  uses
    System.IO;
{$ENDIF}

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoResource ;
{$ENDIF}

const
  EPS = 1E-12 ; // extent enhancement

{$IFDEF OXYGENE}

//==============================================================================
// TGIS_RTreeItem
//==============================================================================
  {$IFDEF OXYGENE}
    constructor TGIS_RTreeItem.Create ;
    begin
      Extent := new TGIS_Extent  ;
    end;

    constructor TGIS_RTreeItem.Create(const _ex : TGIS_Extent; const _uid : Int32) ;
    begin
      Extent := _TGIS_Extent(_ex);
      Uid := _uid;
    end;
  {$ENDIF}

  class function TGIS_RTreeItem.SizeOf : Integer ;
  begin
    {$IFDEF ISLAND}
      Result := 4 * RemObjects.Elements.System.sizeOf( Double ) div RemObjects.Elements.System.sizeOf(Byte) +
                RemObjects.Elements.System.sizeOf( Integer ) div RemObjects.Elements.System.sizeOf(Byte) ;
    {$ELSE}
      {$IFDEF JAVA}
        Result := 4 * Double.SIZE div java.lang.Byte.SIZE +
                  Integer.SIZE div java.lang.Byte.SIZE ;
      {$ELSE}
        Result := 4 * Marshal.SizeOf( typeOf(Double) ) +
                  Marshal.SizeOf( typeOf(Int32) ) ;
      {$ENDIF}
    {$ENDIF}
  end ;

  procedure TGIS_RTreeItem.BytesToStructure( const _bytes : TBytes; const _offset : Integer ) ;
  var
    offset : Integer ;
  begin
    offset :=    _offset ;
    Extent.XMin := BitConverter.ToDouble( _bytes, offset ) ;
    inc( offset, 8 ) ;

    Extent.YMin := BitConverter.ToDouble( _bytes, offset ) ;
    inc( offset, 8 ) ;

    Extent.XMax := BitConverter.ToDouble( _bytes, offset ) ;
    inc( offset, 8 ) ;

    Extent.YMax := BitConverter.ToDouble( _bytes, offset ) ;
    inc( offset, 8 ) ;

    Uid         := BitConverter.ToInt32( _bytes, offset ) ;
  end ;

  function TGIS_RTreeItem.StructureToBytes : TBytes ;
  var
    bytes  : array of Byte ;
    offset : Integer ;
  begin
    SetLength( Result, SizeOf ) ;
    bytes  := BitConverter.GetBytes( Extent.XMin ) ;
    offset := 0 ;
    GisCopyMemory( bytes, 0, Result, offset, 8 ) ;
    offset := 8 ;
    bytes  := BitConverter.GetBytes( Extent.YMin ) ;
    GisCopyMemory( bytes, 0, Result, offset, 8 ) ;
    inc( offset, 8 ) ;
    bytes  := BitConverter.GetBytes( Extent.XMax ) ;
    GisCopyMemory( bytes, 0, Result, offset, 8 ) ;
    inc( offset, 8 ) ;
    bytes  := BitConverter.GetBytes( Extent.YMax ) ;
    GisCopyMemory( bytes, 0, Result, offset, 8 ) ;
    inc( offset, 8 ) ;
    Result[offset+0] := Uid and $FF;
    Result[offset+1] := Byte(Uid shr 8) and $FF;
    Result[offset+2] := Byte(Uid shr 16) and $FF;
    Result[offset+3] := Byte((Uid shr 24) and $FF);
  end ;

  {$IFDEF GIS_NORECORDS}
    function TGIS_RTreeItem.MakeCopy : TGIS_RTreeItem ;
    begin
      Result := new TGIS_RTreeItem(Extent, Uid);
    end;
    function _TGIS_RTreeItem( const _rti : TGIS_RTreeItem ) : TGIS_RTreeItem ;
    begin
      Result := _rti.MakeCopy() ;
    end ;
  {$ENDIF}
//==============================================================================
// T_RTreeHeader
//==============================================================================

  procedure TGIS_RTreeHeader.Clear ;
  var
    i, cnt : Integer ;
  begin
    Magic      := 0 ;
    Version    := 0 ;
    Subversion := 0 ;
    Copyright  := nil ;
    Committed   := 0 ;
    PageCount  := 0 ;
    ItemCount  := 0 ;
    LastFree   := 0 ;
    Root       := 0 ;

    cnt := GIS_RT_PAGESIZE-288 ;
    {$IFDEF OXYGENE}
      if not assigned( Gap ) then
        SetLength( Gap, cnt ) ;
    {$ENDIF}
    for i := 0 to cnt-1 do
      Gap[i] := 0 ;
  end ;

  class function  TGIS_RTreeHeader.SizeOf : Integer ;
  begin
    {$IFDEF ISLAND}
      Result := 3 * RemObjects.Elements.System.sizeOf( Integer ) div RemObjects.Elements.System.sizeOf(Byte) + 256 +
                5 * RemObjects.Elements.System.sizeOf( Integer ) div RemObjects.Elements.System.sizeOf(Byte) + GIS_RT_PAGESIZE-288 ;
    {$ELSE}
      {$IFDEF JAVA}
        Result := 3 * Integer.SIZE div java.lang.Byte.SIZE + 256 +
                  5 * Integer.SIZE div java.lang.Byte.SIZE + GIS_RT_PAGESIZE-288 ;
      {$ELSE}
        Result := 3 * Marshal.SizeOf( typeOf(Int32) ) + 256 +
                  5 * Marshal.SizeOf( typeOf(Int32) ) + GIS_RT_PAGESIZE-288 ;
      {$ENDIF}
    {$ENDIF}
  end ;

  procedure TGIS_RTreeHeader.BytesToStructure( const _bytes : TBytes ) ;
  var
    cright : array of Byte ;
    offset : Integer ;
  begin
    offset := 0 ;
    Magic      := BitConverter.ToInt32( _bytes, offset ) ;
    inc( offset, 4 ) ;

    Version    := BitConverter.ToInt32( _bytes, offset ) ;
    inc( offset, 4 ) ;

    Subversion := BitConverter.ToInt32( _bytes, offset ) ;
    inc( offset, 4 ) ;

    SetLength( cright, 256 ) ;
    GisCopyMemory( _bytes, offset, cright, 0, 256 ) ;
    Copyright := ConvertAnsiString( cright ) ;
    inc( offset, 256 ) ;

    Committed   := BitConverter.ToInt32( _bytes, offset ) ;
    inc( offset, 4 ) ;

    PageCount  := BitConverter.ToInt32( _bytes, offset ) ;
    inc( offset, 4 ) ;

    ItemCount :=  BitConverter.ToInt32( _bytes, offset ) ;
    inc( offset, 4 ) ;

    LastFree :=  BitConverter.ToInt32( _bytes, offset ) ;
    inc( offset, 4 ) ;

    Root := BitConverter.ToInt32( _bytes, offset ) ;
    inc( offset, 4 ) ;

    {$IFDEF OXYGENE}
      if not assigned( Gap ) then
        SetLength( Gap, GIS_RT_PAGESIZE-288 ) ;
    {$ENDIF}
    GisCopyMemory( _bytes, offset, Gap, 0, GIS_RT_PAGESIZE-288 ) ;
  end ;

  function TGIS_RTreeHeader.StructureToBytes : TBytes ;
  var
    offset : Integer ;
    i      : Integer ;
  begin
    SetLength( Result, GIS_RT_PAGESIZE ) ;
    offset := 0 ;
    Result[offset+0] :=   Magic               and $FF;
    Result[offset+1] := ( Magic      shr  8 ) and $FF;
    Result[offset+2] := ( Magic      shr 16 ) and $FF;
    Result[offset+3] := ( Magic      shr 24 ) and $FF;
    inc( offset, 4 ) ;
    Result[offset+0] :=   Version             and $FF;
    Result[offset+1] := ( Version    shr  8 ) and $FF;
    Result[offset+2] := ( Version    shr 16 ) and $FF;
    Result[offset+3] := ( Version    shr 24 ) and $FF;
    inc( offset, 4 ) ;
    Result[offset+0] :=   Subversion          and $FF;
    Result[offset+1] := ( Subversion shr  8 ) and $FF;
    Result[offset+2] := ( Subversion shr 16 ) and $FF;
    Result[offset+3] := ( Subversion shr 24 ) and $FF;
    inc( offset, 4 ) ;
    if not IsStringEmpty( Copyright ) then
      for i := 0 to Min( Copyright.Length - 1, 255 ) do
        Result[ offset + i ] := Byte( Copyright[ i + StringFirst ] ) ;
    inc( offset, 256 ) ;
    Result[offset+0] :=   Committed            and $FF;
    Result[offset+1] := ( Committed   shr  8 ) and $FF;
    Result[offset+2] := ( Committed   shr 16 ) and $FF;
    Result[offset+3] := ( Committed   shr 24 ) and $FF;
    inc( offset, 4 ) ;
    Result[offset+0] :=   PageCount           and $FF;
    Result[offset+1] := ( PageCount  shr  8 ) and $FF;
    Result[offset+2] := ( PageCount  shr 16 ) and $FF;
    Result[offset+3] := ( PageCount  shr 24 ) and $FF;
    inc( offset, 4 ) ;
    Result[offset+0] :=   ItemCount           and $FF;
    Result[offset+1] := ( ItemCount  shr  8 ) and $FF;
    Result[offset+2] := ( ItemCount  shr 16 ) and $FF;
    Result[offset+3] := ( ItemCount  shr 24 ) and $FF;
    inc( offset, 4 ) ;
    Result[offset+0] :=   LastFree            and $FF;
    Result[offset+1] := ( LastFree   shr  8 ) and $FF;
    Result[offset+2] := ( LastFree   shr 16 ) and $FF;
    Result[offset+3] := ( LastFree   shr 24 ) and $FF;
    inc( offset, 4 ) ;
    Result[offset+0] :=   Root                and $FF;
    Result[offset+1] := ( Root       shr  8 ) and $FF;
    Result[offset+2] := ( Root       shr 16 ) and $FF;
    Result[offset+3] := ( Root       shr 24 ) and $FF;
    inc( offset, 4 ) ;
    GisCopyMemory( Gap, 0, Result, offset, GIS_RT_PAGESIZE-288 ) ;
  end ;

//==============================================================================
// T_RTreePageData
//==============================================================================

  {$IFDEF OXYGENE}
    constructor TGIS_RTreePageData.Create ;
    begin
      Entries := new TGIS_RTreeItem[GIS_RT_PAGECNT]  ;
      {$IFDEF JAVA}
        var i : Integer ;
        for i := 0 to GIS_RT_PAGECNT-1 do
          Entries[i] := new TGIS_RTreeItem ;
      {$ENDIF}
    end;

    constructor TGIS_RTreePageData.Create(const _page_type : Int32 ; const _count :Int32 ; const _entries : array of TGIS_RTreeItem) ;
    begin
      PageType := _page_type;
      Count := _count;
      for i : Integer := 0 to length(_entries) - 1 do begin
        Entries[i] := _entries[i];
      end;
    end;
  {$ENDIF}

  function  TGIS_RTreePageData.Read( _stream : TStream ) : Integer ;
  var
    i        : Integer ;
    offset   : Integer ;
    byte_arr : TBytes  ;
    item_len : Integer ;
    {$IFDEF OXYGENE}
      strm   : TGIS_BaseStream ;
    {$ELSE}
      strm   : TStream ;
    {$ENDIF}
  begin
    {$IFDEF OXYGENE}
      strm := TGIS_BaseStream.Create( _stream ) ;
      try
    {$ELSE}
      strm := _stream ;
    {$ENDIF}
    SetLength( byte_arr, GIS_RT_PAGESIZE ) ;
    {$IFDEF OXYGENE}
      Result := strm.Read( byte_arr, GIS_RT_PAGESIZE ) ;
    {$ELSE}
      Result := strm.Read( byte_arr[0], GIS_RT_PAGESIZE ) ;
    {$ENDIF}

    offset := 0 ;

    PageType := BitConverter.ToInt32( byte_arr, offset ) ;
    inc( offset, 4 ) ;

    Count    := BitConverter.ToInt32( byte_arr, offset ) ;
    inc( offset, 4 ) ;

    item_len := TGIS_RTreeItem.SizeOf ;

    for i := 0 to GIS_RT_PAGECNT-1 do begin
      Entries[i].BytesToStructure( byte_arr, offset ) ;
      inc( offset, item_len ) ;
    end ;
    {$IFDEF GIS_BIGRTREEPAGE}
      Result := Result + strm.Read( Gap, Length( Gap ) ) ;
    {$ENDIF}
    {$IFDEF OXYGENE}
      finally
        FreeObject( strm ) ;
      end ;
    {$ENDIF}
  end ;

  function  TGIS_RTreePageData.Write( _stream : TStream ) : Integer ;
  var
    i    : Integer ;
    {$IFDEF OXYGENE}
      strm   : TGIS_BaseStream ;
    {$ELSE}
      strm   : TStream ;
    {$ENDIF}
  begin
    {$IFDEF OXYGENE}
      strm := TGIS_BaseStream.Create( _stream ) ;
      try
    {$ELSE}
      strm := _stream ;
    {$ENDIF}
      Result := strm.WriteInteger( PageType ) +
                strm.WriteInteger( Count    ) ;
      for i := 0 to GIS_RT_PAGECNT-1 do begin
        strm.Write( Entries[i].StructureToBytes,
                    Entries[i].SizeOf ) ;
        Result := Result + Entries[i].SizeOf ;
      end ;
      {$IFDEF GIS_BIGRTREEPAGE}
        Result := Result + strm.Write( Gap, Length( Gap ) ) ;
      {$ENDIF}
    {$IFDEF OXYGENE}
      finally
        FreeObject( strm ) ;
      end ;
    {$ENDIF}
  end ;
  {$IFDEF GIS_NORECORDS}
    function TGIS_RTreePageData.MakeCopy : TGIS_RTreePageData ;
    begin
      Result := new TGIS_RTreePageData(PageType, Count, Entries);
    end;
    function _TGIS_RTreePageData( const _rtpd : TGIS_RTreePageData ) : TGIS_RTreePageData ;
    begin
      Result := _rtpd.MakeCopy() ;
    end ;
  {$ENDIF}
{$ENDIF}


//==============================================================================
// T_RTreePage
//==============================================================================

  constructor TGIS_RTreePage.Create ;
  begin
    inherited ;

    Stack := TGIS_StackInteger.Create ;
    {$IFDEF OXYGENE}
      Data := new TGIS_RTreePageData ;
    {$ENDIF}
    Data.PageType := GIS_RT_PAGE_LEAF ;
  end ;

  procedure TGIS_RTreePage.doDestroy ;
  begin
    FreeObject( Stack ) ;
    inherited ;
  end ;

  function TGIS_RTreePage.fget_Count : Integer ;
  begin
    Result := Data.Count ;
  end ;

  procedure TGIS_RTreePage.fset_Count(
    const _val : Integer
  ) ;
  begin
    Data.Count := _val ;
  end ;

  function TGIS_RTreePage.fget_Uid(
    _idx : Integer
  ) : TGIS_Uid ;
  begin
    Result := Data.Entries[ _idx ].Uid ;
  end ;

  function TGIS_RTreePage.fget_Extent(
    _idx : Integer
  ) : TGIS_Extent  ;
  begin
    Result := _TGIS_Extent(Data.Entries[ _idx ].Extent)
  end ;

  function TGIS_RTreePage.fget_Item(
    _idx : Integer
  ) : TGIS_RTreeItem ;
  begin
    Result := _TGIS_RTreeItem(Data.Entries[ _idx ]) ;
  end ;

  procedure TGIS_RTreePage.fset_Item(
          _idx : Integer;
    const _val : TGIS_RTreeItem
  ) ;
  begin
    Data.Entries[ _idx ] := _TGIS_RTreeItem(_val) ;
  end ;

  function TGIS_RTreePage.fget_IsLeaf
    : Boolean ;
  begin
    Result := Data.PageType = GIS_RT_PAGE_LEAF ;
  end ;

  function TGIS_RTreePage.fget_IsRoot
    : Boolean ;
  begin
    Result := Stack.Count < 1 ;
  end ;

  function TGIS_RTreePage.fget_IsFull
    : Boolean ;
  begin
    Result := Count = GIS_RT_PAGECNT ;
  end ;

  function TGIS_RTreePage.fget_IsHalfFull
    : Boolean ;
  begin
    Result := Count < ( GIS_RT_PAGECNT div 2 ) ;
  end ;

  function TGIS_RTreePage.itemGrowCost( const _pos : Integer     ;
                                        const _ext : TGIS_Extent
                                      ) : Double ;
  var
    ex : TGIS_Extent ;
  begin
    ex := _TGIS_Extent(Extent[_pos]) ;

    Result := GisExtentArea( GisMaxExtent( ex, _ext ) ) -
              GisExtentArea( ex )  ;
  end ;

  procedure TGIS_RTreePage.Insert( const _item : TGIS_RTreeItem ) ;
  var
    i,j,k    : Integer ;

    tmp,dist : Double  ;
  begin
    assert( Count < GIS_RT_PAGECNT ) ;

    i := 0 ;
    k := 0 ;
    while i <= Count - 1 do begin
      if GisIsContainExtent( _item.Extent, Extent[i] ) then begin
        k := i ;
      end;
      inc( i ) ;
    end ;

    if k = 0 then begin
      i := 0 ;

      dist := GIS_MAX_DOUBLE ;
      while i <= Count - 1 do begin
        tmp := GisPoint2Point( GisCenterPoint( _item.Extent ) , GisCenterPoint( Extent[i] ) ) ;
        if tmp <= dist then begin
          k := i ;
          dist := tmp ;
        end;
        inc( i ) ;
      end ;
    end ;

    for j:=Count-1 downto k do
      Item[j+1] := Item[j] ;
    Item[k] := _item  ;

    Count := Count + 1 ;
  end ;

  procedure TGIS_RTreePage.InsertOnPos( const _item : TGIS_RTreeItem ;
                                        const _pos  : Integer
                                      ) ;
  var
    i : Integer ;
  begin
    assert( Count < GIS_RT_PAGECNT ) ;
    assert( _pos  < GIS_RT_PAGECNT ) ;

    for i:=Count-1 downto _pos do
      Item[i+1] := Item[i] ;
    Item[_pos] := _item  ;

    Count := Count + 1 ;
  end ;

  procedure TGIS_RTreePage.Delete( const _item : TGIS_RTreeItem ) ;
  var
    i,j : Integer ;
    fnd : Boolean ;
  begin
    assert( Count < GIS_RT_PAGECNT ) ;

    fnd := False ;
    i   := 0     ;
    while i <= Count - 1 do begin
      if ( _item.Uid = Uid[i] ) and
         GisIsSameExtent( _item.Extent, Extent[i])
      then begin
        fnd := True ;
        break ;
      end ;
      inc( i ) ;
    end ;

    if not fnd then exit ;

    for j:=i to Count - 2 do
      Item[j] := Item[j+1] ;

    Count := Count - 1 ;
  end ;

  procedure TGIS_RTreePage.DeleteOnPos( const _pos : Integer ) ;
  var
    i : Integer ;
  begin
    assert( _pos <  GIS_RT_PAGECNT ) ;
    assert( _pos <  Count          ) ;
    assert( _pos >= 0              ) ;

    for i:= _pos to Count - 2 do
      Item[i] := Item[i+1] ;

    Count := Count - 1 ;
  end ;

  function TGIS_RTreePage.Find( const _ext : TGIS_Extent ) : Integer ;
  var
    i : Integer ;
  begin
    Result := -1 ;
    for i:= 0 to Count - 1 do begin
      if GisIsCommonExtent( _ext, Extent[i] ) then begin
        Result := i ;
        break ;
      end ;
    end ;
  end ;

  function TGIS_RTreePage.FindInsertPoint( const _item : TGIS_RTreeItem
                                       ) : Integer ;
  var
    i         : Integer ;
    cosTGIS_tmp  : Double  ;
    cosTGIS_min  : Double  ;
  begin
    Result := 0 ;

    cosTGIS_min := GIS_MAX_DOUBLE ;
    for i:= 0 to Count - 1 do begin
      cosTGIS_tmp := itemGrowCost( i, _item.Extent ) ;
      if cosTGIS_tmp < cosTGIS_min then begin
        Result := i ;
        cosTGIS_min := cosTGIS_tmp ;
      end ;
    end ;

  end ;

  function TGIS_RTreePage.FindUid( const _item : TGIS_RTreeItem
                              ) : TGIS_Uid ;
  var
    i : Integer ;
  begin
    Result := -1 ;
    for i:= 0 to Count - 1 do begin
      if Uid[i] = _item.Uid then begin
        Result := i ;
        exit ;
      end ;
    end ;
  end ;

  function TGIS_RTreePage.MaxExtent : TGIS_Extent ;
  var
    i : Integer ;
  begin
    assert( Count >0 ) ;

    Result := GisNoWorld ;

    for i := 0 to Count - 1 do
      Result := GisMaxExtent( Result, Extent[i] ) ;
  end ;

  procedure TGIS_RTreePage.Split( const _right : TGIS_RTreePage ) ;
  var
    i : Integer ;
  begin
    // copy from left to right
       for i:=0 to Count -1 do begin
         if i >= Count div 2  then begin
           _right.Item[ _right.Count ] := Item[i] ;
           _right.Count := _right.Count + 1 ;
         end ;
       end ;
    // delete left
       Count := Count div 2 ;
  end ;

//==============================================================================
// TGIS_RTree
//==============================================================================

  constructor TGIS_RTree.Create( const _path : String ) ;
  begin
     inherited Create ;

     FPath := _path ;
     reCreate ;

     rtreeTouched := False ;
  end ;

  procedure TGIS_RTree.doDestroy ;
  begin
    headerWrite ;

    FreeObject( streamObj   ) ;

    cursorCloseAll ;

    cursorState := nil ;

    if headerObj.Committed <> GIS_RT_COMMITED then begin
      if not IsStringEmpty( FPath ) then begin
        if SafeFileExists( FPath ) then begin
          DeleteFile( FPath) ;
        end;
      end;
    end;


    inherited ;
  end ;

  function TGIS_RTree.fget_IsEmpty : Boolean ;
  begin
    Result := headerObj.ItemCount <= 0 ;
  end ;

  function TGIS_RTree.fget_IsCommited : Boolean ;
  begin
    Result := headerObj.Committed = GIS_RT_COMMITED ;
  end ;

  procedure TGIS_RTree.reCreate ;
  {$IFDEF DEBUG}
    {$IFDEF OXYGENE}
      var
        size : Integer ;
    {$ENDIF}
  {$ENDIF}
  begin
    {$IFDEF DEBUG}
      {$IFDEF OXYGENE}
        // TGIS_RTreeHeader must have GIS_RT_PAGESIZE bytes
        // if GIS_RT_PAGESIZE = 512 then
        //  all significant fields take 288 bytes,
        //  so TGIS_RTreeHeader.Gap must be a 224-byte field
        assert( GIS_RT_PAGESIZE-288 = 224 ) ;
        size := TGIS_RTreeHeader.SizeOf ;
        assert( size = GIS_RT_PAGESIZE,
                Format( 'TGIS_RTreeHeader size %d',
                        [ size ]
                      )
              ) ;
      {$ELSE}
        assert( SizeOf( TGIS_RTreeHeader ) = GIS_RT_PAGESIZE,
                Format( 'TGIS_RTreeHeader size %d',
                        [ SizeOf( TGIS_RTreeHeader ) ]
                      )
              ) ;
      {$ENDIF}
      {$IFDEF OXYGENE}
        size := TGIS_RTreeItem.SizeOf ;
        assert( 8 + 14 * size = GIS_RT_PAGESIZE,
                Format( 'TGIS_RTreePageData size %d',
                        [ size ]
                      )
              ) ;
      {$ELSE}
        assert( SizeOf( TGIS_RTreePageData ) = GIS_RT_PAGESIZE,
                Format( 'TGIS_RTreePageData size %d',
                        [ SizeOf( TGIS_RTreePageData ) ]
                      )
              ) ;
      {$ENDIF}
    {$ENDIF}

    streamObj   := nil ;
    {$IFDEF GIS_NORECORDS}
      headerObj := new TGIS_RTreeHeader ;
    {$ENDIF}

    cursorCloseAll ;

    if not IsStringEmpty( FPath ) then begin
      if SafeFileExists( FPath ) then begin
        FAge := GisFileAge( FPath ) ;
        try
          streamObj := TGIS_FileStream.Create( FPath,
                                               fmOpenReadWrite or
                                               fmShareDenyWrite
                                             ) ;
          TGIS_FileStream(streamObj).PatrolRead := True ;
          streamObj.Seek( 0, soBeginning ) ;
          FIsReadOnly := False ;
        except
          streamObj := TGIS_FileStream.Create( FPath,
                                               fmOpenRead or
                                               fmShareDenyNone
                                             ) ;
          TGIS_FileStream(streamObj).PatrolRead := True ;
          FIsReadOnly := True ;
        end ;
      end
      else begin
        streamObj := TGIS_FileStream.Create( FPath,
                                             fmCreate or
                                             fmShareDenyWrite
                                           ) ;
        FAge := Now ;
        streamInit ;
        FIsReadOnly := False ;
      end ;

      try
        streamOpen ;
      except
        on e : EGIS_Exception do
          raise ;
        on e : Exception do begin
          FAge := Now ;
          streamInit ;
          FIsReadOnly := False ;
          streamOpen ;
        end ;
      end ;

      FInMemory := False ;
    end
    else begin
      streamObj := TMemoryStream.Create ;
      FAge := Now ;
      streamInit ;
      FInMemory := True ;
      FIsReadOnly := False ;
    end ;

    cursorOpen( 0 ) ;
  end ;

  procedure TGIS_RTree.checkRW( const _res : Integer ) ;
  begin
    if _res <> GIS_RT_PAGESIZE then
      raise EGIS_Exception.Create(
              _rsrc( GIS_RS_ERR_FILEWRITE ), pathForError, 0
            ) ;
  end ;

  procedure TGIS_RTree.streamInit ;
  {$IFDEF DCC}
    var
      ar : TBytes  ;
      i  : Integer ;
  {$ENDIF}
  begin
    {$IFDEF OXYGENE}
      headerObj.Clear ;
    {$ELSE}
      FillChar( headerObj, SizeOf( headerObj ), 0  ) ;
    {$ENDIF}
    pageAssign( False ) ;
    headerObj.Magic      := GIS_RT_MAGIC      ;
    headerObj.Version    := GIS_RT_VERSION    ;
    headerObj.Subversion := GIS_RT_SUBVERSION ;
    {$IFDEF DCC}
      ar := ConvertAnsiString(GIS_RT_COPYRIGHT) ;
      for i:= 0 to Length( ar ) - 1 do
        headerObj.Copyright[i] := ar[i] ;
      SetLength( ar, 0 );
    {$ELSE}
      headerObj.Copyright  := GIS_RT_COPYRIGHT  ;
    {$ENDIF}
    headerObj.Committed   := GIS_RT_DIRTY      ;
    headerObj.PageCount  := 1                 ; // because we already have a root
    headerObj.ItemCount  := 0                 ;
    headerObj.LastFree   := 0                 ;
    headerTouched := True ;
    pageAssign( True ) ; // first data page

    markCommited ;
  end ;

  procedure TGIS_RTree.streamOpen ;
  begin
    headerRead ;

    headerTouched := False  ;

    with headerObj do begin
      if ( Magic           <> GIS_RT_MAGIC ) or
         ( streamObj.Size < 2 * GIS_RT_PAGESIZE ) or
         ( streamObj.Size mod GIS_RT_PAGESIZE <> 0 )
      then
        raise EGIS_Exception.Create(
                _rsrc( GIS_RS_ERR_FILEBADFORMAT ), pathForError, 0
              ) ;
      if Version           > GIS_RT_VERSION then
        raise EGIS_Exception.Create(
                _rsrc( GIS_RS_ERR_FILENEWERVERSION ), pathForError, 0
              ) ;

      // try to recreate old indexes
      if ( Version < GIS_RT_VERSION ) or ( Subversion < GIS_RT_SUBVERSION )
      then begin
        try
          {$IFDEF OXYGENE}
            streamObj.SetSize( 0 ) ;
          {$ELSE}
            streamObj.Size := 0 ;
          {$ENDIF}
        except
        end ;
        Abort ;
      end ;

      if Committed = GIS_RT_DIRTY then begin
        try
          {$IFDEF OXYGENE}
            streamObj.SetSize( 0 ) ;
          {$ELSE}
            streamObj.Size := 0 ;
          {$ENDIF}
        except
          raise EGIS_Exception.Create(
                  _rsrc( GIS_RS_ERR_FILEWRITE ), pathForError, 0
                );
        end ;
        Abort ;
      end ;
    end ;
  end ;

  function TGIS_RTree.pageFromFree : Integer ;
  var
    page_obj : TGIS_RTreePage ;
  begin
    page_obj := TGIS_RTreePage.Create ;
      Result := headerObj.LastFree ;
      if Result <> 0 then begin
        pageRead( page_obj, Result ) ;
        headerObj.LastFree := page_obj.Data.PageType ;
        headerTouched := True ;
      end ;
    FreeObject( page_obj ) ;
  end ;

  function TGIS_RTree.pageAssign( const _root : Boolean ) : Integer ;
  var
    page_obj : TGIS_RTreePage ;
  begin
    inc( headerObj.PageCount ) ;
    headerTouched := True ;
    page_obj := TGIS_RTreePage.Create ;
    try
      Result := pageFromFree ;

      if Result = 0 then begin
        Result := headerObj.PageCount - 1 ;

        if FInMemory then
          TMemoryStream( streamObj ).SetSize( Int64( streamObj.Size + GIS_RT_PAGESIZE ) )
        else begin
          if headerObj.PageCount * GIS_RT_PAGESIZE <= streamObj.Size then exit ;

          page_obj.Data.PageType := GIS_RT_PAGE_LEAF ;
          streamObj.Seek ( 0, soEnd ) ;
          {$IFDEF OXYGENE}
            checkRW( page_obj.Data.Write( streamObj ) ) ;
          {$ELSE}
            streamObj.Write( page_obj.Data, GIS_RT_PAGESIZE ) ;
          {$ENDIF}
        end ;
      end ;
    finally
      FreeObject( page_obj ) ;
    end ;
    if _root then begin
      headerObj.Root := Result ;
      headerTouched := True ;
    end ;

    markDirty ;
  end ;

  procedure TGIS_RTree.pageRead( var   _page_obj : TGIS_RTreePage ;
                                 const _page_no  : Integer
                               ) ;
  begin
    assert( (_page_no > 0) ) ;
    streamObj.Seek( _page_no * GIS_RT_PAGESIZE, soBeginning ) ;
    {$IFDEF OXYGENE}
      checkRW( _page_obj.Data.Read( streamObj ) ) ;
    {$ELSE}
      checkRW( streamObj.Read( _page_obj.Data, GIS_RT_PAGESIZE ) ) ;
    {$ENDIF}
    _page_obj.PageNo := _page_no ;
  end ;

  procedure TGIS_RTree.pageWrite( var _page_obj : TGIS_RTreePage ) ;
  begin
    assert( _page_obj.PageNo >= 1) ;
    streamObj.Seek( _page_obj.PageNo * GIS_RT_PAGESIZE, soBeginning ) ;
    {$IFDEF OXYGENE}
      checkRW( _page_obj.Data.Write( streamObj ) ) ;
    {$ELSE}
      checkRW( streamObj.Write( _page_obj.Data, GIS_RT_PAGESIZE ) ) ;
    {$ENDIF}
  end ;

  procedure TGIS_RTree.pageMarkFree( const _page_no : Integer ) ;
  var
    page_obj : TGIS_RTreePage ;
  begin
    page_obj := TGIS_RTreePage.Create ;
    {$IFNDEF MANAGED}
      FillChar( page_obj.Data, SizeOf( page_obj.Data ), 0 ) ;
    {$ENDIF}
    page_obj.PageNo := _page_no ;
    page_obj.Data.PageType := headerObj.LastFree ;
    pageWrite( page_obj ) ;
    FreeObject( page_obj ) ;

    headerObj.LastFree := _page_no ;
    dec( headerObj.PageCount ) ;
    headerTouched := True ;
  end ;

  procedure TGIS_RTree.pageSplit( const _item    : TGIS_RTreeItem ;
                                  var   _page_lt : TGIS_RTreePage
                                ) ;
  var
    page_rt  : TGIS_RTreePage ;
    page_tmp : TGIS_RTreePage ;
    pos_tmp  : Integer     ;
    item_tmp : TGIS_RTreeItem ;
    item_lt  : TGIS_RTreeItem ;
    item_rt  : TGIS_RTreeItem ;
  begin
    {$IFDEF GIS_NORECORDS}
      item_tmp := new TGIS_RTreeItem ;
      item_lt  := new TGIS_RTreeItem ;
      item_rt  := new TGIS_RTreeItem ;
    {$ENDIF}
    page_rt := TGIS_RTreePage.Create ;
    page_rt.Data.PageType := _page_lt.Data.PageType ;

      // make the low level splitting

         page_rt.PageNo := pageAssign( False ) ;
         page_rt.Stack.Assign( _page_lt.Stack ) ;
         _page_lt.Split( page_rt ) ;
         pageWrite( _page_lt ) ;
         pageWrite( page_rt   ) ;

     // calculate new pages params
        item_lt.Extent := _page_lt.MaxExtent ;
        item_lt.Uid    := _page_lt.PageNo    ;
        item_rt.Extent := page_rt.MaxExtent  ;
        item_rt.Uid    := page_rt.PageNo ;

     // update parent
        if _page_lt.IsRoot then begin
          page_tmp := TGIS_RTreePage.Create ;
            page_tmp.Data.PageType := GIS_RT_PAGE_NOTLEAF ;
            page_tmp.PageNo := pageAssign( True ) ;
            page_tmp.InsertOnPos( item_rt, 0 ) ;
            page_tmp.InsertOnPos( item_lt, 0 ) ;
            pageWrite( page_tmp ) ;

            page_tmp.Stack.Push( page_tmp.PageNo );

            _page_lt.Stack.Clear ;
            _page_lt.Stack.Push( page_tmp.PageNo ) ;

            page_rt.Stack.Clear ;
            page_rt.Stack.Push( page_tmp.PageNo );

          FreeObject( page_tmp ) ;
        end
        else begin
          page_tmp := TGIS_RTreePage.Create ;
            page_tmp.Data.PageType := GIS_RT_PAGE_NOTLEAF ;
            page_tmp.Stack.Assign( _page_lt.Stack ) ;

            pageRead( page_tmp, page_tmp.Stack.Pop ) ;
            assert( page_tmp.Data.PageType = GIS_RT_PAGE_NOTLEAF ) ;

            // pointer to the current page
               item_tmp.Uid := _page_lt.PageNo ;
               pos_tmp := page_tmp.FindUid( item_tmp ) ;
               assert( pos_tmp >= 0 ) ;
               assert( pos_tmp < GIS_RT_PAGECNT ) ;

            // remove current reference and add a reference to left/rightpage
               page_tmp.DeleteOnPos( pos_tmp ) ;
               page_tmp.InsertOnPos( item_rt, pos_tmp ) ;
               page_tmp.InsertOnPos( item_lt, pos_tmp ) ;

            // if parent full - do recurtion
               if page_tmp.IsFull then begin
                 // is the inserted item in left page?
                 if _page_lt.FindUid( _item ) >= 0 then
                   pageSplit( item_lt, page_tmp )
                 else
                   pageSplit( item_rt, page_tmp ) ;

                 // adjust stacks
                 _page_lt.Stack.Assign( page_tmp.Stack ) ;
                 _page_lt.Stack.Push( _page_lt.PageNo ) ;

                 page_rt.Stack.Assign( page_tmp.Stack ) ;
                 page_rt.Stack.Push( page_rt.PageNo ) ;
               end
               else begin
                 pageWrite( page_tmp ) ;
                 pagePropagate( page_tmp ) ;
               end ;
          FreeObject( page_tmp ) ;
        end ;

    FreeObject( page_rt ) ;
  end ;

  procedure TGIS_RTree.pageDelete( var   _page_obj : TGIS_RTreePage ;
                                   const _item     : TGIS_RTreeItem ) ;
  var
    new_page_no : Integer     ;
    old_page_no : Integer     ;
    ext         : TGIS_Extent ;
    itm         : TGIS_RTreeItem ;
    ipos        : Integer     ;
  begin
    {$IFDEF GIS_NORECORDS}
      itm := new TGIS_RTreeItem ;
    {$ENDIF}
    ext := _page_obj.MaxExtent ;

    ipos := _page_obj.FindUid( _item ) ;
    assert( ipos >= 0 ) ;

    _page_obj.DeleteOnPos( ipos ) ;

    pageWrite( _page_obj ) ;

    if _page_obj.Count <= 0 then begin
      if _page_obj.IsRoot then begin
        _page_obj.Data.PageType := GIS_RT_PAGE_LEAF ;
        pageWrite( _page_obj ) ;
        exit ;
      end ;
      old_page_no := _page_obj.PageNo ;

      new_page_no := _page_obj.Stack.Pop ;
      if new_page_no = old_page_no then
        new_page_no := _page_obj.Stack.Pop ;

      pageRead( _page_obj, new_page_no ) ;
      pageMarkFree( old_page_no ) ;

      itm.Extent := _TGIS_Extent(ext) ;
      itm.Uid    := old_page_no ;

      pageDelete( _page_obj, itm ) ;
    end
    else
      pagePropagate( _page_obj ) ;
  end ;

  procedure TGIS_RTree.pagePropagate( var _page_obj : TGIS_RTreePage ) ;
  var
    new_page_no : Integer     ;
    old_page_no : Integer     ;
    ext         : TGIS_Extent ;
    itm         : TGIS_RTreeItem ;
    ipos        : Integer     ;
  begin
    if _page_obj.IsRoot then exit ;

    old_page_no := _page_obj.PageNo ;

    new_page_no := _page_obj.Stack.Pop ;
    if new_page_no = old_page_no then exit ;

    ext := _page_obj.MaxExtent ;

    pageRead( _page_obj, new_page_no ) ;

    {$IFDEF GIS_NORECORDS}
      itm := new TGIS_RTreeItem ;
    {$ENDIF}
    itm.Extent := _TGIS_Extent(ext) ;
    itm.Uid    := old_page_no ;
    ipos := _page_obj.FindUid( itm ) ; // _page_obj.FindUid( itm ) < 0
    assert( ipos >= 0 ) ;
    assert( ipos < GIS_RT_PAGECNT ) ;

    if GisIsSameExtent( _page_obj.Extent[ ipos ], ext ) then exit ;

    _page_obj.DeleteOnPos( ipos );
    _page_obj.InsertOnPos( itm, ipos );

    pageWrite( _page_obj ) ;

    pagePropagate( _page_obj ) ;
  end ;

  procedure TGIS_RTree.headerRead ;
  {$IFDEF OXYGENE}
    var
      len   : Integer ;
      bytes : TBytes  ;
  {$ENDIF}
  begin
    if assigned( streamObj ) then
    begin
      streamObj.Seek( 0, soBeginning ) ;

      {$IFDEF OXYGENE}
        len := headerObj.SizeOf ;
        SetLength( bytes, len ) ;
        checkRW( streamObj.Read( bytes, len ) ) ;
        headerObj.BytesToStructure( bytes ) ;
      {$ELSE}
        checkRW( streamObj.Read( headerObj, GIS_RT_PAGESIZE ) ) ;
      {$ENDIF}
    end ;
  end ;

  procedure TGIS_RTree.headerWrite ;
  begin
    if headerTouched         and
       ( not IsReadOnly    ) and
       assigned( streamObj ) then
    begin
      try
        streamObj.Seek( 0, soBeginning ) ;
        {$IFDEF OXYGENE}
            streamObj.Write( headerObj.StructureToBytes,
                                      headerObj.SizeOf
                            ) ;
        {$ELSE}
          streamObj.Write( headerObj, GIS_RT_PAGESIZE  ) ;
        {$ENDIF}
      except
      end ;
      headerTouched := False ;
    end ;
  end ;

  procedure TGIS_RTree.markCommited ;
  begin
    if not rtreeTouched then exit ;
    headerObj.Committed := GIS_RT_COMMITED ;
    headerTouched := True ;
    headerWrite ;
  end ;

  procedure TGIS_RTree.markDirty ;
  begin
    headerObj.Committed := GIS_RT_DIRTY ;
    headerTouched := True ;
    headerWrite ;

    rtreeTouched := True ;
  end ;

  procedure TGIS_RTree.cursorOpen(const _cursor: Integer);
  begin
    assert( _cursor >= 0 ) ;

    SetLength( cursorState, Max( Integer(length( cursorState )), _cursor + 1 ) ) ;
    {$IFDEF GIS_NORECORDS}
      if not assigned( cursorState[_cursor] ) then
        cursorState[_cursor] := new T_cursorState_RTree ;
    {$ENDIF}

    FreeObject( cursorState[ _cursor ].searchStack ) ;
    cursorState[ _cursor ].searchStack := TGIS_StackInteger.Create ;

    FreeObject( cursorState[ _cursor ].searchPage ) ;
    cursorState[ _cursor ].searchPage  := TGIS_RTreePage.Create ;

    {$IFDEF GIS_NORECORDS}
      FreeObject( cursorState[ _cursor ].searchItm ) ;
      cursorState[ _cursor ].searchItm  := new TGIS_RTreeItem ;
    {$ENDIF}
  end;

  procedure TGIS_RTree.cursorCloseAll;
  var
    i : Integer ;
  begin
    for i := 0 to high( cursorState ) do begin
      FreeObject( cursorState[ i ].searchStack ) ;
      FreeObject( cursorState[ i ].searchPage  ) ;
      {$IFDEF GIS_NORECORDS}
        FreeObject( cursorState[ i ].searchItm   ) ;
      {$ENDIF}
    end ;
  end;

  function TGIS_RTree.pathForError
    : String ;
  begin
    if IsStringEmpty( FPath ) then
      Result := GIS_RT_ERROR_INMEMORY
    else
      Result := FPath ;
  end;

  procedure TGIS_RTree.Commit( const _state : Boolean ) ;
  begin
    if _state then markCommited
  end ;

  procedure TGIS_RTree.Clear ;
  begin
    if IsEmpty then exit ;
    if FIsReadOnly then exit ;

    markCommited ;

    FreeObject( streamObj   ) ;

    cursorCloseAll ;

    if SafeFileExists( FPath ) then begin
      if not {$IFDEF DCC}System.SysUtils.{$ENDIF}DeleteFile( FPath ) then FPath := '' ;
    end ;

    reCreate ;
  end ;

  procedure TGIS_RTree.Insert( const _ext : TGIS_Extent ;
                               const _uid : TGIS_Uid
                             ) ;
  var
    page_obj : TGIS_RTreePage ;
    page_no  : TGIS_Uid     ;
    itm      : TGIS_RTreeItem ;
  begin
    if FIsReadOnly then exit ;

    {$IFDEF GIS_NORECORDS}
      itm := new TGIS_RTreeItem ;
    {$ENDIF}
    itm.Extent := _TGIS_Extent( _ext ) ;

    with itm.Extent do begin
      XMin := XMin - Abs( XMin * EPS ) ;
      XMax := XMax + Abs( XMax * EPS ) ;
      YMin := YMin - Abs( YMin * EPS ) ;
      YMax := YMax + Abs( YMax * EPS ) ;
    end ;

    itm.Uid    := _uid ;


    page_obj := TGIS_RTreePage.Create ;

      page_no := headerObj.Root ;
      while True do begin
        pageRead( page_obj, page_no ) ;
        if page_obj.IsLeaf then break ;
        page_obj.Stack.Push( page_no );
        page_no := page_obj.Uid[ page_obj.FindInsertPoint( itm ) ] ;
      end ;

      page_obj.Insert( itm );

      if page_obj.IsFull then
        // split full page
        pageSplit( itm, page_obj )
      else
        pageWrite( page_obj ) ;

      // propagate changes
      pagePropagate( page_obj ) ;

    FreeObject( page_obj ) ;

    inc( headerObj.ItemCount ) ;
    headerTouched := True ;

    markDirty ;
  end ;

  function TGIS_RTree.Delete( const _ext : TGIS_Extent ;
                              const _uid : TGIS_Uid
                            ) : Boolean ;
  var
    itm      : TGIS_RTreeItem ;
  begin
    Result := False ;

    if FIsReadOnly then exit ;

    if not FindFirst( 0, _ext, _uid ) >= 0 then exit ;

    {$IFDEF GIS_NORECORDS}
      itm := new TGIS_RTreeItem ;
    {$ENDIF}
    itm.Extent := _TGIS_Extent( _ext ) ;

    with itm.Extent do begin
      XMin := XMin - Abs( XMin * EPS ) ;
      XMax := XMax + Abs( XMax * EPS ) ;
      YMin := YMin - Abs( YMin * EPS ) ;
      YMax := YMax + Abs( YMax * EPS ) ;
    end ;

    itm.Uid    := _uid ;

    cursorState[ 0 ].searchPage.Stack.Assign( cursorState[ 0 ].searchStack ) ;
    cursorState[ 0 ].searchPage.Stack.Pop ;
    cursorState[ 0 ].searchPage.Stack.Pop ;
    pageDelete( cursorState[ 0 ].searchPage, itm ) ;

    dec( headerObj.ItemCount ) ;
    headerTouched := True ;

    markDirty ;

    Result := True ;
  end ;

  function TGIS_RTree.FindFirst(
    const _cursor : Integer ;
    const _ext    : TGIS_Extent
  ) : TGIS_Uid ;
  begin
    Result := FindFirst( _cursor, _ext, -1 ) ;
  end ;

  function TGIS_RTree.FindFirst(
    const _cursor : Integer ;
    const _ext    : TGIS_Extent ;
    const _uid    : TGIS_Uid
  ) : TGIS_Uid ;
  begin
    cursorOpen( _cursor ) ;
    cursorState[ _cursor ].searchStack.Clear ;
    cursorState[ _cursor ].searchItm.Extent := _TGIS_Extent( _ext ) ;
    cursorState[ _cursor ].searchItm.Uid    := _uid ;

    cursorState[ _cursor ].searchStack.Push( headerObj.Root ) ;
    cursorState[ _cursor ].searchStack.Push( -1              ) ;

    cursorState[ _cursor ].lastUid := -1 ;

    Result := FindNext(_cursor) ;

    cursorState[ _cursor ].lastUid := Result ;
  end ;

  function TGIS_RTree.FindNext( const _cursor : Integer ) : TGIS_Uid ;
  var
    itm  : TGIS_RTreeItem ;
    tmp  : TGIS_RTreeItem ;
    ipos : Integer ;
    page : Integer ;
    lmt  : Integer ;
  begin
    if headerObj.ItemCount <= 0 then begin
      Result := -1 ;
      exit ;
    end ;

    lmt := 0 ;
    while True do begin
      inc( lmt ) ;
      if lmt >= GIS_MAX_INTEGER then
        raise EGIS_Exception.Create(
                _rsrc( GIS_RS_ERR_FILEBADFORMAT ), pathForError, 0
              ) ;

      Result := -1 ;

      ipos  := cursorState[ _cursor ].searchStack.Pop ;
      page :=  cursorState[ _cursor ].searchStack.Top ;

      if cursorState[ _cursor ].searchStack.IsEmpty then exit ;

      pageRead( cursorState[ _cursor ].searchPage, page ) ;

      {$IFDEF GIS_NORECORDS}
        itm := new TGIS_RTreeItem ;
      {$ENDIF}
      itm.Uid := ipos ;
      for ipos := cursorState[ _cursor ].searchPage.FindUid( itm ) + 1
          to cursorState[ _cursor ].searchPage.Count - 1
      do begin
        tmp := cursorState[ _cursor ].searchPage.Item[ ipos ] ;
        if GisIsCommonExtent( tmp.Extent,
                              cursorState[ _cursor ].searchItm.Extent
                            )
        then begin
          if cursorState[ _cursor ].searchPage.IsLeaf and
             (
               ( cursorState[ _cursor ].searchItm.Uid >= 0       ) and
               ( tmp.Uid <> cursorState[ _cursor ].searchItm.Uid )
             )
          then continue ;
          Result := tmp.Uid ;
          if cursorState[ _cursor ].searchPage.IsLeaf then begin
            cursorState[ _cursor ].searchStack.Push( Result ) ;

            if Result = cursorState[ _cursor ].lastUid then
              raise EGIS_Exception.Create(
                      _rsrc( GIS_RS_ERR_FILEBADFORMAT ), pathForError, 0
                    ) ;

            cursorState[ _cursor ].lastUid := Result ;
            exit ;
          end ;
          cursorState[ _cursor ].searchStack.Push( Result ) ;
          cursorState[ _cursor ].searchStack.Push( -1      ) ;

          if cursorState[ _cursor ].searchPage.IsLeaf then exit
                               else break ;
        end ;
      end ;
    end ;
  end ;

  function TGIS_RTree.Valid : Boolean ;
  var
    current_age : {$IFDEF CLR}
                    DateTime ;
                  {$ELSE}
                    TDateTime ;
                  {$ENDIF}
  begin
    Result := True ;
    if not IsReadOnly then exit ;

    current_age := GisFileAge( FPath ) ;

    Result := current_age = Age ;
  end;

{==================================== END =====================================}
end.

