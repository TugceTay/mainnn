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
  Encapsulation of the Shortest Path Algorithm.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoShortestPath ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoShortestPath"'}
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
    System.Math,
    System.Generics.Collections,
    System.Generics.Defaults,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoLayerVector,
    Lider.CG.GIS.GeoNetwork ;
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
  {$IFDEF OXYGENE}

    /// <summary>
    ///   Provides data for the link event.
    /// </summary>
    TGIS_LinkEventArgs = public class ( EventArgs )
      private
        FShape    : TGIS_ShapeArc ;
        FReverse  : Boolean ;
        FCrossEnd : Boolean ;
      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_shape">
        ///   shape related to the event
        /// </param>
        /// <param name="_reverse">
        ///   true if link will be traversed in a reverse direction
        /// </param>
        /// <param name="_crossend">
        ///   true if end of the segment is connected to more then one
        ///   line segment (so it is a crossroad)
        /// </param>
        constructor Create  ( const _shape    : TGIS_ShapeArc ;
                              const _reverse  : Boolean ;
                              const _crossend : Boolean
                            ) ;
      public

        /// <summary>
        ///   Shape related to the event.
        /// </summary>
        property Shape      : TGIS_ShapeArc read FShape ;

        /// <summary>
        ///   True if link will be traversed in a reverse direction.
        /// </summary>
        property Reverse    : Boolean       read FReverse ;


        /// <summary>
        ///   True if end of the segment is connected to more then one
        ///   line segment (so it is a crossroad). }
        /// </summary>
        property CrossEnd   : Boolean       read FCrossEnd ;
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
    TGIS_LinkEvent = public procedure(
      _sender   : Object ;
      _e        : TGIS_LinkEventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   Event for segment traverse.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_shape">
    ///   shape related to the event
    /// </param>
    /// <param name="_reverse">
    ///   true if link will be traversed in a reverse direction
    /// </param>
    /// <param name="_crossend">
    ///   true if end of the segment is connected to more then one line
    ///   segment (so it is a crossroad)
    /// </param>
    TGIS_LinkEvent = procedure(
       _sender   : TObject ;
       _shape    : TGIS_ShapeArc ;
       _reverse  : Boolean ;
       _crossend : Boolean
    ) of object ;
  {$ENDIF}
  {$IFDEF OXYGENE}

    /// <summary>
    ///   Provides data for the link cost event.
    /// </summary>
    TGIS_LinkCostEventArgs = public class ( EventArgs )
      private
        FShape    : TGIS_ShapeArc ;
        FCost     : Double ;
        FRevCost  : Double ;
      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_shape">
        ///   shape related to the event
        /// </param>
        /// <param name="_cost">
        ///   forward cost of the link (natural, established by node order);
        ///   provide -1 to block the link in this direction.
        /// </param>
        /// <param name="_revcost">
        ///   reverse cost of the link; provide -1 to block the link in
        ///   this direction
        /// </param>
        constructor Create  ( const _shape   : TGIS_ShapeArc ;
                              const _cost    : Double ;
                              const _revcost : Double
                            ) ;
      public

        /// <summary>
        ///    Shape related to the event.
        /// </summary>
        property Shape      : TGIS_ShapeArc read  FShape ;

        /// <summary>
        ///   Forward cost of the link (natural, established by node order);
        ///   provide -1 to block the link in this direction. }
        /// </summary>
        property Cost       : Double        read  FCost
                                            write FCost ;

        /// <summary>
        ///   Reverse cost of the link; provide -1 to block the link in
        ///   this direction.
        /// </summary>
        property RevCost    : Double        read  FRevCost
                                            write FRevCost ;
    end ;

    /// <summary>
    ///   Event for the link cost calculation.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_e">
    ///   event parameters
    /// </param>
    TGIS_LinkCostEvent = public procedure(
      _sender   : Object ;
      _e        : TGIS_LinkCostEventArgs
    ) of object ;

    /// <summary>
    ///   Provides data for the link dynamic event.
    /// </summary>
    TGIS_LinkDynamicEventArgs = public class ( EventArgs )
      private
        FUid     : TGIS_Uid ;
        FCost    : Double  ;
        FRevCost : Double  ;
      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_uid">
        ///   uid of shape used to create this link
        /// </param>
        /// <param name="_cost">
        ///   forward cost of the link (natural, established by node order);
        ///   provide -1 to block the link in this direction.
        /// </param>
        /// <param name="_revcost">
        ///   reverse cost of the link; provide -1 to block the link in
        ///   this direction
        /// </param>
        constructor Create  ( const _uid     : Integer ;
                              const _cost    : Double  ;
                              const _revcost : Double
                            ) ;
      public

        /// <summary>
        ///   Uid of shape used to create this link.
        /// </summary>
        property Uid        : TGIS_Uid         read  FUid ;

        /// <summary>
        ///   Forward cost of the link (natural, established by node order);
        ///   provide -1 to block the link in this direction.
        /// </summary>
        property Cost       : Double        read  FCost
                                            write FCost ;

        /// <summary>
        ///   Reverse cost of the link; provide -1 to block the link in
        ///   this direction.
        /// </summary>
        property RevCost    : Double        read  FRevCost
                                            write FRevCost ;
    end ;

    /// <summary>
    ///   Event for the dynamic link cost calculation.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_e">
    ///   event parameters
    /// </param>
    TGIS_LinkDynamicEvent = public procedure(
      _sender   : Object ;
      _e        : TGIS_LinkDynamicEventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   Event for the link cost calculation.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_shape">
    ///   shape related to the event
    /// </param>
    /// <param name="_cost">
    ///   forward cost of the link (natural, established by node order);
    ///   provide -1 to block the link in this direction
    /// </param>
    /// <param name="_revcost">
    ///   reverse cost of the link; provide -1 to block the link in this
    ///   direction
    /// </param>
    TGIS_LinkCostEvent    = procedure(
            _sender  : TObject ;
            _shape   : TGIS_ShapeArc ;
      var   _cost    : Double ;
      var   _revcost : Double
    ) of object ;

    /// <summary>
    ///   Event for the dynamic link cost calculation.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_uid">
    ///   uid of shape used to create this link
    /// </param>
    /// <param name="_cost">
    ///   forward cost of the link (natural, established by node order);
    ///   provide -1 to block the link in this direction
    /// </param>
    /// <param name="_revcost">
    ///   reverse cost of the link; provide -1 to block the link in this
    ///   direction
    /// </param>
    TGIS_LinkDynamicEvent = procedure(
          _sender  : TObject ;
          _uid     : Integer ;
      var _cost    : Double ;
      var _revcost : Double
    ) of object ;
  {$ENDIF}
  {$IFDEF OXYGENE}

    /// <summary>
    ///   Provides data for the link type event.
    /// </summary>
    TGIS_LinkTypeEventArgs = public class ( EventArgs )
      private
        FShape : TGIS_ShapeArc ;
        FType  : Integer ;
      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_shape">
        ///   shape related to the event
        /// </param>
        /// <param name="_type">
        ///   classification kind of link; number 0..255 which meaning
        ///   is user dependent
        /// </param>
        constructor Create  ( const _shape : TGIS_ShapeArc ;
                              const _type  : Integer
                            ) ;
      public

          /// <summary>
          ///   Shape related to the event.
          /// </summary>
          property Shape      : TGIS_ShapeArc read  FShape ;

          /// <summary>
          ///   Classification kind of link; number 0..255 which meaning
          ///   is user dependent
          /// </summary>
          property LinkType   : Integer       read  FType
                                              write FType ;
    end ;

    /// <summary>
    ///   Event for the link cost calculation.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_e">
    ///   event parameters
    /// </param>
    TGIS_LinkTypeEvent = public procedure(
      _sender   : Object ;
      _e        : TGIS_LinkTypeEventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   Event for the link cost calculation.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_shape">
    ///   shape related to the event
    /// </param>
    /// <param name="_type">
    ///   classification kind of link; number 0..255 which meaning is user
    ///   dependent
    /// </param>
    TGIS_LinkTypeEvent = procedure(
          _sender   : TObject ;
          _shape    : TGIS_ShapeArc ;
      var _type     : Integer
    ) of object ;
  {$ENDIF}

  {$IFDEF OXYGENE}

    /// <summary>
    ///   Provides data for the link levels event.
    /// </summary>
    TGIS_LinkLevelEventArgs = public class ( EventArgs )
      private
        FShape : TGIS_ShapeArc ;
        FFrom  : ShortInt ;
        FTo    : ShortInt ;
      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_shape">
        ///   shape related to the event
        /// </param>
        /// <param name="_from">
        ///   level at the first point of the shape
        /// </param>
        /// <param name="_to">
        ///   level at the last point of the shape
        /// </param>
        constructor Create  ( const _shape : TGIS_ShapeArc ;
                              const _from  : ShortInt      ;
                              const _to    : ShortInt
                            ) ;
      public

         /// <summary>
         ///   Shape related to the event.
         /// </summary>
         property Shape      : TGIS_ShapeArc read  FShape ;

         /// <summary>
         ///   Level at the first point of the shape.
         /// </summary>
         property FromLevel  : ShortInt      read  FFrom
                                             write FFrom ;

         /// <summary>
         ///   Level at the last point of the shape.
         /// </summary>
         property ToLevel     : ShortInt     read  FTo
                                              write FTo ;
    end ;

    /// <summary>
    ///   Event for the link levels calculation.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_e">
    ///   event parameters
    /// </param>
    TGIS_LinkLevelEvent = public procedure(
      _sender   : Object ;
      _e        : TGIS_LinkLevelEventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   Event for the link levels calculation.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_shape">
    ///   shape related to the event
    /// </param>
    /// <param name="_from">
    ///   level at the first point of the shape
    /// </param>
    /// <param name="_to">
    ///   level at the last point of the shape
    /// </param>
    TGIS_LinkLevelEvent = procedure(
          _sender : TObject       ;
          _shape  : TGIS_ShapeArc ;
      var _from   : ShortInt      ;
      var _to     : ShortInt
    ) of object ;
  {$ENDIF}

  {$IFDEF OXYGENE}

    /// <summary>
    ///   Provides data for the heuristic cost event.
    /// </summary>
    TGIS_NodeHeuristicCostEventArgs = public class ( EventArgs )
      private
        FFrom : TGIS_NetworkNode ;
        FTo   : TGIS_NetworkNode ;
        FCost : Double  ;
      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_from">
        ///   first node for cost calculation
        /// </param>
        /// <param name="_to">
        ///   second node for cost calculation
        /// </param>
        /// <param name="_cost">
        ///   calculated average cost for straight distance calculations
        /// </param>
        constructor Create  ( const _from : TGIS_NetworkNode ;
                              const _to   : TGIS_NetworkNode ;
                              const _cost : Double
                            ) ;
      public

          /// <summary>
          ///   First node for cost calculation.
          /// </summary>
          property FromNode   : TGIS_NetworkNode read  FFrom ;

          /// <summary>
          ///   Second node for cost calculation.
          /// </summary>
          property ToNode     : TGIS_NetworkNode read  FTo ;

          /// <summary>
          ///   Calculated average cost for straight distance calculations.
          /// </summary>
          property Cost       : Double read  FCost
                                       write FCost ;
    end ;

    /// <summary>
    ///   Event for the heuristic cost calculation.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_e">
    ///   event parameters
    /// </param>
    TGIS_NodeHeuristicCostEvent = public procedure(
      _sender   : Object ;
      _e        : TGIS_NodeHeuristicCostEventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   Event for the heuristic cost calculation.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_from">
    ///   first node for cost calculation
    /// </param>
    /// <param name="_to">
    ///   second node for cost calculation
    /// </param>
    /// <param name="_cost">
    ///   calculated average cost for straight distance calculations
    /// </param>
    TGIS_NodeHeuristicCostEvent = procedure(
          _sender : TObject ;
          _from   : TGIS_NetworkNode ;
          _to     : TGIS_NetworkNode ;
      var _cost   : Double
    ) of object ;
  {$ENDIF}

  /// <summary>
  ///   The shortest path link.
  /// </summary>
  TGIS_ShortestPathLink = {$IFDEF OXYGENE} public {$ENDIF} class
    {$IFDEF OXYGENE} assembly or {$ENDIF} protected
      /// <summary>
      ///   Layer to which the link belongs.
      /// </summary>
      FLayer        : TGIS_LayerVector ;
      /// <summary>
      ///   Unique shape identifier.
      /// </summary>
      FUid          : TGIS_Uid ;
      /// <summary>
      ///   Name of the segment (street name, etc.).
      /// </summary>
      FName         : String  ;
      /// <summary>
      ///   Absolute course (in radians).
      /// </summary>
      FCourse       : Double  ;
      /// <summary>
      ///   Compass segment, see TGIS_ShortestPath.CompassBlocks.
      /// </summary>
      FCompass      : Integer ;
      /// <summary>
      ///   Cost value for the segment.
      /// </summary>
      FCost         : Double  ;
      /// <summary>
      ///   Length of the segment.
      /// </summary>
      FLength       : Double  ;
    public
      /// <summary>
      ///   Layer to which the link belongs.
      /// </summary>
      property Layer        : TGIS_LayerVector read FLayer        ;
      /// <summary>
      ///   Unique shape identifier.
      /// </summary>
      property Uid          : TGIS_Uid         read FUid          ;
      /// <summary>
      ///   Name of the segment (street name, etc.).
      /// </summary>
      property Name         : String           read FName         ;
      /// <summary>
      ///   Absolute course (in radians).
      /// </summary>
      property Course       : Double           read FCourse       ;
      /// <summary>
      ///   Compass segment, see TGIS_ShortestPath.CompassBlocks.
      /// </summary>
      property Compass      : Integer          read FCompass      ;
      /// <summary>
      ///   Cost value for the segment.
      /// </summary>
      property Cost         : Double           read FCost         ;
      /// <summary>
      ///   Length of the segment.
      /// </summary>
      property Length       : Double           read FLength       ;
  end ;

  /// <summary>
  ///   Heuristic mode for path calculations
  /// </summary>
  TGIS_HeuristicMode = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   Heuristic based on simple closest distance approach.
      /// </summary>
      Distance,

      /// <summary>
      ///   Heuristic based on assumption that cost of achieving destination
      ///   node depends on a cost of the current link.
      /// </summary>
      Adaptive
  ) ;

  /// <summary>
  ///   Encapsulation of shortest path algorithm.
  /// </summary>
  TGIS_ShortestPath = {$IFDEF OXYGENE} public {$ENDIF}
                      class( TGIS_BaseObjectDisposable )

    private // Property internal variables

        /// <summary>
        ///   Field for the feature name.
        /// </summary>
        FName : String ;

        /// <summary>
        ///   Field name used to calculate segment length. Empty means 0.
        ///   Default is 'GIS_LENGTH
        /// </summary>
        FLength : String  ;

        /// <summary>
        ///   Layer assigned to the shortest path.
        /// </summary>
        FLayer : TGIS_LayerVector ;

        /// <summary>
        ///   Network path (including base file name).
        /// </summary>
        FStoragePath  : String ;

        /// <summary>
        ///   Items in the shortest path.
        /// </summary>
        FItems : TObjectList<TGIS_ShortestPathLink> ;

        /// <summary>
        ///   Extent
        /// </summary>
        FExtent : TGIS_Extent ;

        /// <summary>
        ///   Compass divided to a number of blocks (4 by default).
        /// </summary>
        FCompassBlocks : Integer ;

        /// <summary>
        ///   Total cost of the path.
        /// </summary>
        FTotalCost : Double ;

        /// <summary>
        ///   Array of cost modifiers (related to the link type).
        /// </summary>
        FCostModifiers : array[0..255] of Double ;

        /// <summary>
        ///   Cost modifier for heuristic cost calculation.
        /// </summary>
        FHeuristicCostModifier : Double ;

        /// <summary>
        ///   Mode of heuristic operation.
        /// </summary>
        FHeuristicMode : TGIS_HeuristicMode ;

        /// <summary>
        ///   Threshold of starting node seeking area.
        /// </summary>
        FHeuristicThreshold : Double ;

        /// <summary>
        ///   Online routing switch.
        /// </summary>
        FOSMRouting : Boolean ;

    {$IFDEF OXYGENE}
      assembly or protected // property events
    {$ENDIF}

        /// <summary>
        ///   Event which will be fired on each link of the discovered shortest
        ///   path. But will be reported in reverse order.
        /// </summary>
        FOnPath     : TGIS_LinkEvent     ;

        /// <summary>
        ///   Event which will be fired upon creating a network.
        /// </summary>
        FOnLinkCost : TGIS_LinkCostEvent ;

        /// <summary>
        ///   Event which will be fired upon traversing link.
        /// </summary>
        FOnLinkDynamic : TGIS_LinkDynamicEvent ;

        /// <summary>
        ///   Event which will be fired upon getting the type of each link.
        ///   Type is value between 0 and 255.
        /// </summary>
        FOnLinkType : TGIS_LinkTypeEvent ;

        /// <summary>
        ///   Event which will be fired upon getting the level of the endings
        ///   of each link.
        /// </summary>
        FOnLinkLevel : TGIS_LinkLevelEvent ;

        /// <summary>
        ///   Event which will be fired upon calculation of heuristic cost.
        /// </summary>
        FOnNodeHeuristicCost : TGIS_NodeHeuristicCostEvent ;

    protected // Property access routines
      function  fget_CostModifier (       _idx : Integer
                                  ) : Double ;
      procedure fset_CostModifier (       _idx : Integer ;
                                    const _value : Double
                                  ) ;
      function  fget_ItemsCount   : Integer ;
      function  fget_Items        (       _idx : Integer
                                  ) : TGIS_ShortestPathLink  ;

      function  fget_Name          : String ;
      function  fget_Length        : String ;
      function  fget_Layer         : TGIS_LayerVector ;
      function  fget_Extent        : TGIS_Extent ;
      function  fget_CompassBlocks : Integer ;
      procedure fset_CompassBlocks ( const _val : Integer
                                   ) ;
      function  fget_TotalCost     : Double ;

    private // Private variables
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      viewerObj : IGIS_Viewer ;
      networkObj: TGIS_Network ;
      startNode : TGIS_NetworkNode ;
      endNode   : TGIS_NetworkNode ;
      osmRtg    : TObject ;

    private // private routines

      /// <summary>
      ///   Free all items in path.
      /// </summary>
      procedure clearItems   ;

      /// <summary>
      ///   Free all allocated data.
      /// </summary>
      procedure freeData     ;

      /// <summary>
      ///   Find the shortest path tree.
      /// </summary>
      procedure findPathTree ;

      /// <summary>
      ///   Find the shortest path from startNode to endNode.
      /// </summary>
      procedure findPath     ;

      {$IFDEF OXYGENE}
        procedure doOnLink2  ( const _sender : TObject ;
                               const _e      : TGIS_NetworkLinkEventArgs
                             ) ;
      {$ENDIF}
      procedure doOnLink     (       _sender : TObject ;
                                     _shape  : TGIS_ShapeArc ;
                                     _link   : TGIS_NetworkLink
                             ) ;
    protected
      procedure doDestroy ; override;

    public // public methods

      /// <summary>
      ///   Create an instance.
      /// </summary>
      /// <param name="_viewer">
      ///   viewer on which class will be operating.
      /// </param>
      constructor Create     ( const _viewer : IGIS_Viewer ) ;

      /// <summary>
      ///   Recreate network (including network storage files).
      /// </summary>
      /// <param name="_layer">
      ///   layer that is the source of the data
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Internally calls LoadTheData.
      ///    </note>
      /// </remarks>
      procedure   Regenerate ( const _layer  : TGIS_LayerVector
                             ) ;

      /// <summary>
      ///   Load data from the layer. Upon first run create a persistent
      ///   storage (files responsible to store  network)
      /// </summary>
      /// <param name="_layer">
      ///   layer that is the source of the data
      /// </param>
      /// <remarks>
      ///   See Create for example.
      /// </remarks>
      procedure   LoadTheData( const _layer  : TGIS_LayerVector
                             ) ;

      /// <summary>
      ///   Update the data with geocoded addresses from geocoder
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Function will update network with geocoded addresses from layer
      ///    named 'TGIS_Geocoding'. See TGIS_Geocoding.
      ///    </note>
      ///   See Create for example.
      /// </remarks>
      procedure   UpdateTheData ;

      /// <summary>
      ///   Set a start position for the shortest path.
      /// </summary>
      /// <param name="_ptg">
      ///   start position
      /// </param>
      /// <param name="_prec">
      ///   precision of localization
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Obsolete function. Use Find() instead.
      ///    </note>
      /// </remarks>
      procedure   StartPos   ( const _ptg    : TGIS_Point ;
                               const _prec   : Double
                             ) ;

      /// <summary>
      ///   Set a stop position for the shortest path and run shortest path.
      /// </summary>
      /// <param name="_ptg">
      ///   start position
      /// </param>
      /// <param name="_prec">
      ///   precision of localization
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Obsolete function. Use Find() instead.
      ///    </note>
      ///   See StartPos method for example.
      /// </remarks>
      procedure   StopPos    ( const _ptg    : TGIS_Point ;
                               const _prec   : Double
                             ) ;

      /// <summary>
      ///   Find path between two existing nodes.
      /// </summary>
      /// <param name="_from">
      ///   start node
      /// </param>
      /// <param name="_to">
      ///   end node
      /// </param>
      /// <returns>
      ///   True if a path has been found.
      /// </returns>
      /// <remarks>
      ///   <note type="note">
      ///    Function will update network with geocoded addresses from layer
      ///    named 'TGIS_Geocoding' . See TGIS_Geocoding.
      ///    </note>
      /// </remarks>
      function    Find       ( const _from   : TGIS_Point ;
                               const _to     : TGIS_Point
                             ) : Boolean ;
    public // public properties

      /// <summary>
      ///   Forced storage path.
      /// </summary>
      /// <remarks>
      ///   Provide full path name (including file name) if you want to force
      ///   different network storage location then default (which is same as
      ///   layer's Path).
      /// </remarks>
      property StoragePath : String read FStoragePath write FStoragePath ;

      /// <summary>
      ///   Field for the feature name.
      /// </summary>
      /// <remarks>
      ///   Default is 'name'. See Create method for example.
      /// </remarks>
      property RoadName : String read fget_Name write FName ;

      /// <summary>
      ///   Field name used to calculate segment length. Empty means 0.
      ///   Default is 'GIS_LENGTH'.
      /// </summary>
      property RoadLength : String read fget_Length write FLength ;

      /// <summary>
      ///   Cost modifier.
      /// </summary>
      /// <param name="_idx">
      ///   index of the modifier
      /// </param>
      /// <remarks>
      ///   An array 1..255. Each value describes how much the cost of a
      ///   link of the selected type must be multiplied.
      /// </remarks>
      property CostModifiers[ _idx : Integer] : Double
                                                read  fget_CostModifier
                                                write fset_CostModifier ;

      /// <summary>
      ///   Heuristic cost modifier.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    By providing a negative value all calculation will be done based
      ///    on layers coordinate system (w/o use of projection code) which
      ///    is generally much faster but will work only if layer is
      ///    equidistant.
      ///    </note>
      ///   <para>
      ///      Cost modifier for heuristic cost calculation for built in
      ///     heuristic algorithm.
      ///   </para>
      ///   <para>
      ///     Set 0 to disable heuristic calculations. General formula for
      ///     cost will be an average cost for traversing in a straight (as
      ///     the crow flies) line between current node and the end node.
      ///   </para>
      ///   <para>
      ///
      ///   </para>
      /// </remarks>
      property HeuristicCostModifier : Double
                                       read  FHeuristicCostModifier
                                       write FHeuristicCostModifier ;

      /// <summary>
      ///   Mode of heuristic operation.
      /// </summary>
      property HeuristicMode : TGIS_HeuristicMode
                               read  FHeuristicMode
                               write FHeuristicMode ;

      /// <summary>
      ///   Threshold of starting node seeking area. Treat it as "seeking
      ///   circle" for better route at a staring node. It will allow to seek
      ///   for better alternative paths at a starting location.
      /// </summary>
      /// <remarks>
      ///   <note type="note">
      ///    Valid only for HeuristicCostMode = TGIS_HeuristicMode.Adaptive
      ///    </note>
      /// </remarks>
      property HeuristicThreshold : Double
                                   read  FHeuristicThreshold
                                   write FHeuristicThreshold ;

      /// <summary>
      ///   Layer assigned to the shortest path.
      /// </summary>
      property Layer : TGIS_LayerVector read fget_Layer ;

      /// <summary>
      ///   Instance of Network class (responsible for low level network
      ///   operations)
      /// </summary>
      property Network : TGIS_Network read networkObj ;

      /// <summary>
      ///   Number of items in the shortest path.
      /// </summary>
      property ItemsCount : Integer read fget_ItemsCount ;

      /// <summary>
      ///   Items in the shortest path.
      /// </summary>
      /// <param name="_idx">
      ///   index of the path
      /// </param>
      property Items[ _idx : Integer ] : TGIS_ShortestPathLink
                                         read fget_Items ;

      /// <summary>
      ///   Path extent. Encompasses a whole path.
      /// </summary>
      property Extent : TGIS_Extent read fget_Extent ;

      /// <summary>
      ///   Compass divided to a number of blocks (4 by default). It will be
      ///   used for computing courses like N, NE, E etc. If a value is
      ///   negative, then the course will be absolute; or else the course
      ///   will be computed based on a link (relative). Course will be a
      ///   number ranging from negative to positive integers to reflect
      ///   courses in the scope of -Pi..Pi
      /// </summary>
      property CompassBlocks : Integer read  fget_CompassBlocks
                                       write fset_CompassBlocks ;

      /// <summary>
      ///   Total cost of the path.
      /// </summary>
      property TotalCost : Double read fget_TotalCost ;

      /// <summary>
      ///   <para>
      ///     If true the local routing resource is ignored and the requests
      ///     are send to the TatukGIS online routing service.
      ///   </para>
      ///   <para>
      ///     Internally uses the TGIS_OSMRouting class.
      ///   </para>
      /// </summary>
      property OSMRouting : Boolean read FOSMRouting write FOSMRouting ;

    published //events
      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   Event which will be fired on each link of the discovered shortest
        ///   path. But will be reported in reverse order.
        /// </summary>
        event    PathEvent         : TGIS_LinkEvent
                                     delegate FOnPath ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   Event which will be fired on each link of the discovered
        ///   shortest path. But will be reported in reverse order.
        /// </summary>
        property   PathEvent         : TGIS_LinkEvent
                                       read  FOnPath
                                       write FOnPath ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   Event which will be fired upon creating a network.
        ///   Fired upon building a network.
        /// </summary>
        event    LinkCostEvent     : TGIS_LinkCostEvent
                                     delegate FOnLinkCost ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   Event which will be fired upon creating a network.
        /// </summary>
        property   LinkCostEvent     : TGIS_LinkCostEvent
                                       read  FOnLinkCost
                                       write FOnLinkCost ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   Event which will be fired upon traversing link.
        /// </summary>
        event    LinkDynamicEvent  : TGIS_LinkDynamicEvent
                                     delegate FOnLinkDynamic ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   Event which will be fired upon traversing link.
        /// </summary>
        property   LinkDynamicEvent  : TGIS_LinkDynamicEvent
                                       read  FOnLinkDynamic
                                       write FOnLinkDynamic ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   Event which will be fired upon getting the type of each link.
        ///   Type is value between 0 and 255.
        ///   Fired upon building a network.
        /// </summary>
        event    LinkTypeEvent     : TGIS_LinkTypeEvent
                                     delegate FOnLinkType ;
      {$ELSE}
         /// <event/>
         /// <summary>
         ///   Event which will be fired upon getting the type of each link.
         ///   Type is value between 0 and 255.
         /// </summary>
         property   LinkTypeEvent     : TGIS_LinkTypeEvent
                                        read  FOnLinkType
                                        write FOnLinkType ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   Event which will be fired upon getting the endings levels of each
        ///   link.
        ///   Fired upon building a network.
        ///
        /// </summary>
        event    LinkLevelEvent    : TGIS_LinkLevelEvent
                                     delegate FOnLinkLevel ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   Event which will be fired upon getting the level of the endings
        ///   of each link.
        /// </summary>
        property   LinkLevelEvent    : TGIS_LinkLevelEvent
                                       read  FOnLinkLevel
                                       write FOnLinkLevel ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/>
        /// <summary>
        ///   Event which will be fired upon calculation of heuristic cost.
        /// </summary>
        event    NodeHeuristicCostEvent : TGIS_NodeHeuristicCostEvent
                                          delegate FOnNodeHeuristicCost ;
      {$ELSE}
        /// <event/>
        /// <summary>
        ///   Event which will be fired upon calculation of heuristic cost.
        /// </summary>
        property   NodeHeuristicCostEvent : TGIS_NodeHeuristicCostEvent
                                            read  FOnNodeHeuristicCost
                                            write FOnNodeHeuristicCost ;
      {$ENDIF}
  end ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisHeuristicModeDistance  = TGIS_HeuristicMode.Distance ;
      gisHeuristicModeAdaptive  = TGIS_HeuristicMode.Adaptive ;
  {$ENDIF}

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.Variants,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoCsBase,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoOSMServices ;
{$ENDIF}

type

  T_shortestPathLink = class ( TGIS_ShortestPathLink )
    private
      function calcCourse  ( const _shape  : TGIS_Shape ;
                             const _end    : Boolean
                           ) : Double ;
      function calcCompass ( const _shape  : TGIS_Shape ;
                             const _blocks : Integer
                           ) : Integer ;
    public
      constructor Create ( const _shape  : TGIS_Shape ;
                           const _blocks : Integer
                         ) ;
  end ;


//==============================================================================
// T_shortestPathLink
//==============================================================================

  constructor T_shortestPathLink.Create(
    const _shape  : TGIS_Shape ;
    const _blocks : Integer
  ) ;
  begin
    inherited Create ;

    FLayer := _shape.Layer ;
    FUid := _shape.Uid ;
    FName := VarToString( _shape.GetField( GIS_RTR_OSM_FIELD_NAME ) ) ;
    FCourse := calcCourse( _shape, False ) ;
    FCompass := calcCompass( _shape, _blocks ) ;
    FCost := DotStrToFloat( VarToString(
               _shape.GetField( GIS_RTR_OSM_FIELD_TIME ) ) ) ;
    FLength := DotStrToFloat( VarToString(
                 _shape.GetField( GIS_RTR_OSM_FIELD_DISTANCE ) ) ) ;
  end ;


  function T_shortestPathLink.calcCourse(
    const _shape : TGIS_Shape ;
    const _end   : Boolean
  ) : Double ;
  var
    siz : Integer ;
    p0  : TGIS_Point ;
    p1  : TGIS_Point ;
  begin
    if not _end then begin
      p0 := _shape.GetPoint( 0, 0 ) ;
      p1 := _shape.GetPoint( 0, 1 ) ;
    end
    else begin
      siz := _shape.GetPartSize( 0 ) ;
      p0 := _shape.GetPoint( 0, siz - 2 ) ;
      p1 := _shape.GetPoint( 0, siz - 1 ) ;
    end ;

    Result := -ArcTan2( p1.X - p0.X, p1.Y - p0.Y ) ;

    if Result < 0 then Result := 2.0*Pi + Result ;
  end ;


  function T_shortestPathLink.calcCompass(
    const _shape  : TGIS_Shape ;
    const _blocks : Integer
  ) : Integer ;
  var
    prev   : TGIS_Shape ;
    dangle : Double  ;
    dstep  : Double  ;
    i      : Integer ;
  begin
    Result := 0 ;

    if _blocks > 0 then begin
      if Uid > 1 then begin
        prev := Layer.GetShape( Uid - 1 ) ;
        dangle := RadToDeg( calcCourse( prev, True ) - Course  ) ;
      end
      else
        dangle := 0.0 ;
    end
    else
      dangle := RadToDeg( Course ) ;

    if dangle < 0 then dangle := 360.0 + dangle ;

    dstep := 360.0 / Abs( _blocks * 2.0 ) ;

    for i := 0 to Abs( _blocks * 2 ) do begin
      if dangle < dstep * ( i + 0.5 ) then begin
        Result := i ;
        break ;
      end ;
    end ;

    if Result > Abs( _blocks ) then
      Result := Result - Abs( _blocks * 2 ) ;
  end ;

type

  /// <summary>
  ///   Wrapper of the TGIS_OSMRouting class.
  /// </summary>
  T_osmRouting = class( TGIS_ObjectDisposable )
    private
      FLayer : TGIS_LayerVector ;
      FItems : TObjectList<TGIS_ShortestPathLink> ;
      FCompassBlocks : Integer ;
    protected
      function  fget_ItemsCount : Integer ;
      function  fget_Items      (       _idx : Integer
                                ) : TGIS_ShortestPathLink  ;
    protected
      function  fget_RoadName      : String ;
      function  fget_RoadLength    : String ;
      function  fget_Extent        : TGIS_Extent ;
      function  fget_CompassBlocks : Integer ;
      procedure fset_CompassBlocks ( const _val : Integer
                                   ) ;
      function  fget_TotalCost     : Double ;

    private
      ptStart : TGIS_Point ;
    private
      function  findInternal ( const _from : TGIS_Point ;
                               const _to   : TGIS_Point
                             ) : Boolean ;
    protected
      procedure doDestroy ; override ;
    public
      constructor Create ( const _viewer : IGIS_Viewer
                         ) ;
    public
      procedure   StartPos ( const _ptg  : TGIS_Point ;
                             const _prec : Double
                           ) ;
      procedure   StopPos  ( const _ptg  : TGIS_Point ;
                             const _prec : Double
                           ) ;
      function    Find     ( const _from : TGIS_Point ;
                             const _to   : TGIS_Point
                           ) : Boolean ;
    public
      property RoadName      : String
                               read  fget_RoadName ;
      property RoadLength    : String
                               read  fget_RoadLength ;
      property Layer         : TGIS_LayerVector
                               read  FLayer ;
      property ItemsCount    : Integer
                               read  fget_ItemsCount ;
      property Items[_idx : Integer]
                             : TGIS_ShortestPathLink
                               read  fget_Items ;
      property Extent        : TGIS_Extent
                               read  fget_Extent ;
      property CompassBlocks : Integer
                               read  fget_CompassBlocks
                               write fset_CompassBlocks ;
      property TotalCost     : Double
                               read  fget_TotalCost ;
  end ;

//==============================================================================
// T_osmRouting
//==============================================================================

  constructor T_osmRouting.Create(
    const _viewer : IGIS_Viewer
  ) ;
  begin
    inherited Create ;

    FItems := TObjectList<TGIS_ShortestPathLink>.Create ; ;
    FCompassBlocks := 4 ;
  end ;


  procedure T_osmRouting.doDestroy ;
  begin
    FreeObject( FItems ) ;
    FreeObject( FLayer ) ;

    inherited ;
  end ;


  function T_osmRouting.fget_ItemsCount
    : Integer ;
  begin
    Result := FItems.Count ;
  end ;


  function T_osmRouting.fget_Items(
    _idx : Integer
  ) : TGIS_ShortestPathLink  ;
  begin
    Result := FItems[_idx] ;
  end ;


  function T_osmRouting.fget_RoadName
    : String ;
  begin
    Result := GIS_RTR_OSM_FIELD_NAME ;
  end ;


  function T_osmRouting.fget_RoadLength
    : String ;
  begin
    Result := GIS_RTR_OSM_FIELD_DISTANCE ;
  end ;


  function T_osmRouting.fget_Extent
    : TGIS_Extent ;
  begin
    Result := FLayer.Extent ;
  end ;


  function T_osmRouting.fget_CompassBlocks
    : Integer ;
  begin
    Result := FCompassBlocks ;
  end ;


  procedure T_osmRouting.fset_CompassBlocks(
    const _val : Integer
  ) ;
  begin
    FCompassBlocks := _val ;
  end ;


  function T_osmRouting.fget_TotalCost
    : Double ;
  var
    {$IFNDEF OXYGENE}
      shp  : TGIS_Shape ;
    {$ENDIF}
    dist : Integer ;
  begin
    dist := 0 ;
    for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF}
        in FLayer.Loop( FLayer.Extent ) do
      inc( dist, VarToInt32( shp.GetField( GIS_RTR_OSM_FIELD_TIME ) ) ) ;

    Result := 1.0*dist ;
  end ;


  function T_osmRouting.findInternal(
    const _from : TGIS_Point ;
    const _to   : TGIS_Point
  ) : Boolean ;
  var
    ortg : TGIS_OSMRouting ;
    {$IFNDEF OXYGENE}
      shp  : TGIS_Shape ;
    {$ENDIF}
  begin
    Result := False ;

    FreeObject( FLayer ) ;
    FItems.Clear ;

    ortg := TGIS_OSMRouting.Create ;
    try
      FLayer := ortg.Route( _from, _to ) ;

      if not assigned( FLayer ) then
        exit ;

      for shp {$IFDEF OXYGENE} : TGIS_Shape {$ENDIF} in
          FLayer.Loop( FLayer.Extent, GIS_RTR_OSM_ROUTE_QUERY ) do
        FItems.Add( T_shortestPathLink.Create( shp, CompassBlocks ) ) ;

    finally
      FreeObject( ortg ) ;
    end ;

    Result := True ;
  end ;


  procedure T_osmRouting.StartPos(
    const _ptg  : TGIS_Point;
    const _prec : Double
  ) ;
  begin
    ptStart := _ptg ;
  end ;


  procedure T_osmRouting.StopPos(
    const _ptg  : TGIS_Point;
    const _prec : Double
  ) ;
  begin
    findInternal( ptStart, _ptg ) ;
  end ;


  function T_osmRouting.Find(
    const _from : TGIS_Point ;
    const _to   : TGIS_Point
  ) : Boolean ;
  begin
    Result := findInternal( _from, _to ) ;
  end ;


//==============================================================================

function ptgCompare( const _itemA, _itemB : TGIS_NetworkNode ) : Integer ;
begin
  if      _itemA.Pos.X < _itemB.Pos.X then Result := -1
  else if _itemA.Pos.X > _itemB.Pos.X then Result :=  1
  else if _itemA.Pos.Y < _itemB.Pos.Y then Result := -1
  else if _itemA.Pos.Y > _itemB.Pos.Y then Result :=  1
  else                                     Result :=  0 ;
end ;

{$IFDEF OXYGENE}
  constructor TGIS_LinkEventArgs.Create(
    const _shape    : TGIS_ShapeArc ;
    const _reverse  : Boolean ;
    const _crossend : Boolean
  ) ;
  begin
    inherited Create ;
    FShape    := _shape ;
    FReverse  := _reverse ;
    FCrossEnd := _crossend ;
  end ;

  constructor TGIS_LinkCostEventArgs.Create(
    const _shape   : TGIS_ShapeArc ;
    const _cost    : Double ;
    const _revcost : Double
  ) ;
  begin
    inherited Create ;
    FShape   := _shape   ;
    FCost    := _cost    ;
    FRevCost := _revcost ;
  end ;

  constructor TGIS_LinkDynamicEventArgs.Create(
    const _uid     : Integer ;
    const _cost    : Double  ;
    const _revcost : Double
  ) ;
  begin
    inherited Create ;
    FUid     := _uid     ;
    FCost    := _cost    ;
    FRevCost := _revcost ;
  end;

  constructor TGIS_LinkTypeEventArgs.Create(
    const _shape : TGIS_ShapeArc ;
    const _type  : Integer
  ) ;
  begin
    inherited Create ;
    FShape := _shape ;
    FType  := _type  ;
  end ;

  constructor TGIS_NodeHeuristicCostEventArgs.Create(
    const _from : TGIS_NetworkNode ;
    const _to   : TGIS_NetworkNode ;
    const _cost : Double
  ) ;
  begin
    inherited Create ;

    FFrom  := _from ;
    FTo    := _to   ;
    FCost  := _cost ;
  end ;

  constructor TGIS_LinkLevelEventArgs.Create(
    const _shape : TGIS_ShapeArc ;
    const _from  : ShortInt      ;
    const _to    : ShortInt
  ) ;
  begin
    inherited Create ;

    FShape := _shape ;
    FFrom  := _from  ;
    FTo    := _to    ;
  end ;
{$ENDIF}

constructor TGIS_ShortestPath.Create(
  const _viewer : IGIS_Viewer
) ;
var
  i : Integer ;
begin
  inherited Create ;

  for i:= low( FCostModifiers ) to high( FCostModifiers ) do
    FCostModifiers[i] := 1 ;

  FHeuristicCostModifier := 0.0 ;

  viewerObj := _viewer ;
  FCompassBlocks := 4 ;
  FItems    := TObjectList<TGIS_ShortestPathLink>.Create ;
  startNode := nil ;
  endNode   := nil ;

  FName   := 'name'           ;
  FLength := GIS_FIELD_LENGTH ;

  FOSMRouting := False ;

  osmRtg := T_osmRouting.Create( _viewer ) ;
end;

procedure TGIS_ShortestPath.doDestroy ;
begin
  clearItems ;
  FreeObject( FItems ) ;
  freeData ;

  FreeObject( osmRtg ) ;

  inherited ;
end;

function  TGIS_ShortestPath.fget_CostModifier(
  _idx : Integer
) : Double ;
begin
  Result := 0 ;
  if _idx < low ( FCostModifiers ) then exit ;
  if _idx > high( FCostModifiers ) then exit ;

  Result := FCostModifiers[ _idx ]
end ;

procedure TGIS_ShortestPath.fset_CostModifier(
        _idx   : Integer ;
  const _value : Double
) ;
begin
  if _idx < low ( FCostModifiers ) then exit ;
  if _idx > high( FCostModifiers ) then exit ;

   FCostModifiers[ _idx ] := _value ;
end ;

function TGIS_ShortestPath.fget_ItemsCount
  : Integer ;
begin
  if OSMRouting then
    Result := T_osmRouting( osmRtg ).ItemsCount
  else
    Result := FItems.Count ;
end ;

function TGIS_ShortestPath.fget_Items(
  _idx : Integer
) : TGIS_ShortestPathLink ;
begin
  if OSMRouting then
    Result := T_osmRouting( osmRtg ).Items[_idx]
  else
    Result := TGIS_ShortestPathLink( FItems[_idx] ) ;
end ;

function TGIS_ShortestPath.fget_Name : String ;
begin
  if OSMRouting then
    Result := T_osmRouting( osmRtg ).RoadName
  else
    Result := FName ;
end ;

function TGIS_ShortestPath.fget_Length : String ;
begin
  if OSMRouting then
    Result := T_osmRouting( osmRtg ).RoadLength
  else
    Result := FLength ;
end ;

function TGIS_ShortestPath.fget_Layer : TGIS_LayerVector ;
begin
  if OSMRouting then
    Result := T_osmRouting( osmRtg ).Layer
  else
    Result := FLayer ;
end ;

function TGIS_ShortestPath.fget_Extent : TGIS_Extent ;
begin
  if OSMRouting then
    Result := T_osmRouting( osmRtg ).Extent
  else
    Result := FExtent ;
end ;

function TGIS_ShortestPath.fget_CompassBlocks : Integer ;
begin
  if OSMRouting then
    Result := T_osmRouting( osmRtg ).CompassBlocks
  else
    Result := FCompassBlocks ;
end ;

procedure TGIS_ShortestPath.fset_CompassBlocks(
  const _val : Integer
) ;
begin
  T_osmRouting( osmRtg ).CompassBlocks := _val ;
  FCompassBlocks := _val ;
end ;

function TGIS_ShortestPath.fget_TotalCost : Double ;
begin
  if OSMRouting then
    Result := T_osmRouting( osmRtg ).TotalCost
  else
    Result := FTotalCost ;
end ;

procedure TGIS_ShortestPath.clearItems ;
begin
  FItems.Clear ;
end ;

procedure TGIS_ShortestPath.freeData ;
begin
  FreeObject( networkObj ) ;
end;

procedure TGIS_ShortestPath.findPathTree ;
var
  node_num,
  link_num     : Integer ;
  best_node    : Integer ;
  best_cost    : Double  ;
  new_cost     : Double  ;
  node         : TGIS_NetworkNode ;
  to_node      : TGIS_NetworkNode ;
  link         : TGIS_NetworkLink ;
  candidates   : TGIS_ObjectList   ;
  cost_tmp     : Double  ;
  i1,i2        : Integer ;
  cost_direct  : Double ;
  cost_reverse : Double ;

  {$IFDEF OXYGENE}
    evnt     : TGIS_LinkDynamicEventArgs ;
  {$ENDIF}
  {$IFDEF GIS_SHORTESTPATH_TRACE}
    shp_tmp  : TGIS_Shape ;
  {$ENDIF}
  bpotential : Boolean ;

  ellps      : TGIS_CSEllipsoid        ;
  lcs        : TGIS_CSCoordinateSystem ;

  function heuristic_cost(
    const _a        : TGIS_NetworkNode ;
    const _b        : TGIS_NetworkNode ;
    const _linkcost : Double
  ) : Double ;
  var
    res  : Double     ;
    dst  : Double     ;
    {$IFDEF OXYGENE}
      heuristic_evnt : TGIS_NodeHeuristicCostEventArgs ;
    {$ENDIF}

    function distance(
      const _ptga : TGIS_Point ;
      const _ptgb : TGIS_Point
      ) : Double ;
    var
      ptg1 : TGIS_Point ;
      ptg2 : TGIS_Point ;
    begin
      Result := 0 ;

      if      FHeuristicCostModifier > 0 then begin
                if assigned( ellps ) then begin
                  ptg1 := lcs.ToWGS( _ptga )  ;
                  ptg2 := lcs.ToWGS( _ptgb)  ;
                  if (ptg1.X < 1e30) and (ptg1.Y < 1e30) and
                     (ptg1.X < 1e30) and (ptg1.Y < 1e30)
                  then begin
                    Result := ellps.Distance( ptg1, ptg2 ) ;
                  end ;
                end
                else begin
                  Result := GisLineLength( _ptga, _ptgb ) ;
                end;
              end
      else if FHeuristicCostModifier < 0 then begin
                Result := GisLineLength( _ptga, _ptgb ) ;
              end ;
    end ;

  begin

    res := distance( _a.Pos, _b.Pos ) ;

    if res > 0 then begin
      if FHeuristicMode = TGIS_HeuristicMode.Adaptive then begin

        dst := distance( networkObj.GetNode( link.NodeB ).Pos,
                         networkObj.GetNode( link.NodeA ).Pos
                       ) ;
        res := res / dst * _linkcost ;

        dst := distance( _a.Pos, startNode.Pos ) ;
        if ( dst < FHeuristicThreshold ) and ( dst > 0 ) then begin
          res := res * Sqrt( dst / FHeuristicThreshold ) ;
        end ;
      end;

      res := res * Abs( FHeuristicCostModifier ) ;
    end ;

    {$IFDEF OXYGENE}
      if assigned( FOnNodeHeuristicCost ) then begin
        heuristic_evnt := TGIS_NodeHeuristicCostEventArgs.Create(
                            _a,
                            _b,
                            res
                          ) ;
        try
          FOnNodeHeuristicCost( self, heuristic_evnt ) ;
        finally
          res := heuristic_evnt.Cost ;
          FreeObject( heuristic_evnt ) ;
        end;
      end ;
    {$ELSE}
      if assigned( NodeHeuristicCostEvent ) then
        NodeHeuristicCostEvent(
          self,
          _a,
          _b,
          res
        ) ;
    {$ENDIF}

    Result := res ;
  end;

begin
  clearItems ;

  // Do no mode if startNode = nil or endNode = nil.
    if ( not assigned(startNode) ) or ( not assigned(endNode) ) then exit ;

  FExtent := GisNoWorld ;

  i1 := startNode.ObjectId ;
  i2 := endNode.ObjectId ;

  // Clear previous path tree data.
  networkObj.Clean ;

  // and ensure that startNode & endNode are still in-memory
  startNode := networkObj.GetNode( i1 ) ;
  endNode   := networkObj.GetNode( i2 ) ;

  // calculate ellipsoid and end node position

  lcs := Layer.CS ;

  if      lcs is TGIS_CSProjectedCoordinateSystem  then
          ellps := TGIS_CSProjectedCoordinateSystem(
                     lcs
                   ).Geocs.Datum.Ellipsoid
  else if lcs is TGIS_CSGeographicCoordinateSystem then
          ellps := TGIS_CSGeographicCoordinateSystem(
                     lcs
                   ).Datum.Ellipsoid
  else    ellps := nil ;

  // Create the candidate list and add the root to it.
  candidates := TGIS_ObjectList.Create( False ) ;
  candidates.Add(startNode) ;
  startNode.CurrentCost   := 0 ;
  startNode.CurrentState  := TGIS_NetworkCandidate.New ;
  startNode.HeuristicCost := startNode.CurrentCost ;

  // While the candidate list is not empty, process it.
  node := nil ;
  while candidates.Count > 0 do begin
    if assigned( viewerObj ) then
      if viewerObj.HourglassShake then break ;

    // Find the node with the FSCORE.
    best_cost := GIS_MAX_DOUBLE ;
    best_node := 0 ;
    for node_num := 0 to candidates.Count - 1 do begin
      node := TGIS_NetworkNode(candidates.Items[node_num]) ;

      if node.HeuristicCost < best_cost then begin
        best_cost := node.HeuristicCost ;
        best_node := node_num ;
      end ;
    end ;

    // Remove this node from the candidate list.
    node := TGIS_NetworkNode( candidates.Items[best_node] ) ;
    candidates.Remove(node) ;
    node.CurrentState := TGIS_NetworkCandidate.Already ;

    if node.ObjectId = endNode.ObjectId then begin
       // end of search
       break ;
    end;

    // Add the node's neighbors to the candidate list.
    for link_num := 0 to node.LinkCount - 1 do begin

      // Get the neighbor node.
      link := node.Link[ link_num ] ;

      if link.NodeA = node.ObjectId
        then to_node := networkObj.GetNode( link.NodeB )
        else to_node := networkObj.GetNode( link.NodeA ) ;

      // See if the node has been on the
      // candidate list before.
      if to_node.CurrentState <> TGIS_NetworkCandidate.Already then begin
        // dynamic cost calculations
        cost_direct := link.Cost;
        cost_reverse := link.ReverseCost;

        {$IFDEF OXYGENE}
          if assigned( FOnLinkDynamic ) then begin
            evnt := TGIS_LinkDynamicEventArgs.Create(
                      link.ShapeUid,
                      cost_direct,
                      cost_reverse
                    ) ;
            try
              FOnLinkDynamic( self, evnt ) ;
            finally
              cost_direct  := evnt.Cost    ;
              cost_reverse := evnt.RevCost ;
              FreeObject( evnt ) ;
            end;
          end ;
        {$ELSE}
          if assigned( LinkDynamicEvent ) then
            LinkDynamicEvent(
              self,
              link.ShapeUid,
              cost_direct,
              cost_reverse
            ) ;
        {$ENDIF}

        // which direction?
        if link.NodeB = to_node.ObjectId
          then cost_tmp := cost_direct
          else cost_tmp := cost_reverse ;

        // is the route possible (unidirectional?, closed?)
        if cost_tmp < 0 then continue ;

        cost_tmp := cost_tmp * FCostModifiers[ link.LinkType ] ;

        // See if we can improve its best_cost.
        new_cost := node.CurrentCost + cost_tmp ;

        if to_node.CurrentState = TGIS_NetworkCandidate.Not then begin
          to_node.CurrentState := TGIS_NetworkCandidate.New ;
          candidates.Add( to_node ) ;
          bpotential := True ;
        end
        else
        if new_cost < to_node.CurrentCost then
          bpotential := True
        else
          bpotential := False ;

        if bpotential then begin
          to_node.CurrentLink   := link.ObjectId ;
          to_node.CurrentCost   := new_cost ;
          to_node.HeuristicCost := new_cost +
                                   heuristic_cost(
                                     to_node,
                                     endNode,
                                     cost_tmp
                                   ) ;
        end ;

        {$IFDEF GIS_SHORTESTPATH_TRACE}
          Layer.IgnoreShapeParams := False ;
          shp_tmp := link.Layer.GetShape( link.ShapeUid ) ;
          shp_tmp := shp_tmp.MakeEditable() ;
          shp_tmp.Params.Line.Color := ConvertColor($FF0000) ;
          shp_tmp.Params.Line.Width := -3 ;
        {$ENDIF}
      end ;
    end ;
  end ;

  if not assigned( node ) or
    ( node.ObjectId <> endNode.ObjectId ) then
  begin
    // path not found
    startNode := nil ;
    endNode   := nil ;
  end;

  // Free the candidate list.
  candidates.Clear ;
  FreeObject( candidates ) ;
end;

procedure TGIS_ShortestPath.findPath ;
var
  node     : TGIS_NetworkNode ;
  link     : TGIS_NetworkLink ;
  shp      : TGIS_ShapeArc ;
  plink    : TGIS_ShortestPathLink ;
  crs      : Double ;
  last_crs : Double ;
  cost     : Double ;
  rev      : Boolean ;
  prv_link : TGIS_ShortestPathLink ;

  function get_compass( const _angle : Double ) : Integer ;
  var
    i      : Integer ;
    dangle : Double  ;
    dstep  : Double  ;
  begin
    dangle := RadToDeg( _angle ) ;

    if dangle < 0  then dangle := 360 + dangle ;

    dstep := 360.0 / Abs( CompassBlocks * 2 ) ;

    Result := 0 ;
    for i:=0 to Abs(CompassBlocks * 2) do begin
      if dangle < dstep * ( i + 1.0/2 ) then begin
        Result := i ;
        break ;
      end ;
    end ;
    if Result > Abs(CompassBlocks) then
      Result := Result - Abs(CompassBlocks * 2) ;
  end ;

begin
  prv_link := nil ;

  // Do no mode if startNode = nil or endNode = nil.
  if ( not assigned(startNode) ) or ( not assigned(endNode) ) then exit ;

  // Trace the path from endNode back to startNode
  // marking the links' in_path fields.
  FTotalCost := 0 ;
  node       := endNode ;

  last_crs := GIS_MAX_DOUBLE ;

  while node <> startNode do begin
    if assigned( viewerObj ) then
    if viewerObj.HourglassShake then break ;
    link := networkObj.GetLink( node.CurrentLink ) ;
    if not assigned( link ) then exit ;

    rev := link.NodeA <> node.ObjectId ;

    if rev then node := networkObj.GetNode( link.NodeA )
           else node := networkObj.GetNode( link.NodeB ) ;

    if rev then
      cost := link.Cost * FCostModifiers[ link.LinkType ]
    else
      cost := link.ReverseCost * FCostModifiers[ link.LinkType ] ;
    FTotalCost := TotalCost + cost ;

    // Add node
    shp := networkObj.GetShape( link.ObjectId ) ;
    FExtent := GisMaxExtent( shp.Extent, Extent ) ;


    if CompassBlocks > 0 then begin
      if last_crs < 100 then crs := last_crs - shp.GetAngle( rev )
                        else crs := 0 ;
      if crs >  Pi then crs := crs  - 2*Pi ;
      if crs < -Pi then crs := 2*Pi + crs  ;
    end
    else
      crs := shp.GetAngle( not rev ) ;

    last_crs := shp.GetAngle( not rev ) - Pi ;

    plink := TGIS_ShortestPathLink.Create ;
    plink.FLayer   := shp.Layer   ;
    plink.FUid     := shp.Uid     ;
    plink.FName    := VarToString( shp.GetField( FName ) ) ;
    plink.FCourse  := shp.GetAngle( not rev ) ;
    plink.FCost    := cost ;

    if CompassBlocks < 0 then
      plink.FCompass := get_compass( crs )
    else begin
      plink.FCompass := 0 ;
      if assigned( prv_link ) then
        prv_link.FCompass := get_compass( crs ) ;
    end;

    if shp.TagInternal <> 2 then begin
      if not IsStringEmpty( FLength ) then
        plink.FLength := VarToDouble( shp.GetField( FLength ) )
      else
        plink.FLength := 0 ;
      end
    else
      plink.FLength  := 0 ;


    FItems.Insert( 0, plink ) ;

    prv_link := plink ;

    {$IFDEF OXYGENE}
      if assigned( FOnPath ) then FOnPath( self,
                                           TGIS_LinkEventArgs.Create(
                                             shp,
                                             link.NodeA <> node.ObjectId ,
                                             node.LinkCount > 2
                                           )
                                         ) ;
    {$ELSE}
      if assigned( PathEvent  ) then PathEvent( self,
                                                shp,
                                                link.NodeA <> node.ObjectId,
                                                node.LinkCount > 2
                                              ) ;
    {$ENDIF}
  end;


  // calculate extent
  if GisIsNoWorld( Extent ) then
    FExtent := Layer.Extent ;
end;

{$IFDEF OXYGENE}
  procedure TGIS_ShortestPath.doOnLink2(
    const _sender   : TObject ;
    const _e        : TGIS_NetworkLinkEventArgs
  ) ;
  begin
    doOnLink( _sender, _e.Shape, _e.Link ) ;
  end;
{$ENDIF}

procedure TGIS_ShortestPath.doOnLink(
  _sender   : TObject ;
  _shape    : TGIS_ShapeArc ;
  _link     : TGIS_NetworkLink
) ;
var
  shp       : TGIS_ShapeArc ;
  cost,
  revcost   : Double ;
  from_lvl,
  to_lvl    : ShortInt ;
  link_type : Integer  ;
  wascost   : Boolean  ;
  {$IFDEF OXYGENE}
    cost_evnt  : TGIS_LinkCostEventArgs ;
    type_evnt  : TGIS_LinkTypeEventArgs ;
    level_evnt : TGIS_LinkLevelEventArgs ;
  {$ENDIF}

  function helper_shapetype(
    const _shape2 : TGIS_Shape
  ) : Integer ;
  var
    n : Integer ;
  begin
    Result := 0 ;
    if _shape2.Layer = Layer then
      exit  // original
    else begin
      n := VarToInt32( _shape2.GetField( GIS_GEO_SPLITTYPE ) ) ;

      case n of
        1 :  Result := 1 ; // left
        2 :  Result := 2 ; // right
        3 :  Result := 3 ; // connector
        else begin
               assert( False, GIS_RS_ERR_UNTESTED ) ;
             end ;
      end;
    end;
  end ;

  procedure helper_cost(
    const _shape2   : TGIS_Shape ;
    const _cost     : Double     ;
    const _revcost  : Double     ;
    var   _cost2    : Double     ;
    var   _revcost2 : Double
  ) ;
  var
    d : Double ;
  begin
    case helper_shapetype( _shape2 ) of
      0 :  begin
             _cost2    := _cost    ;
             _revcost2 := _revcost ;
           end ;
      1 :  begin
             d := VarToDouble( _shape2.GetField( GIS_GEO_SPLITVALUE ) ) ;

             if _cost > 0    then _cost2    := _cost    * Max( 0, d   )
                             else _cost2    := _cost                    ;
             if _revcost > 0 then _revcost2 := _revcost * Max( 0, d   )
                             else _revcost2 := _revcost                 ;
           end ;
      2 :  begin
             d := VarToDouble( _shape2.GetField( GIS_GEO_SPLITVALUE ) ) ;

             if _cost > 0    then _cost2    := _cost    * Max( 0, 1-d )
                             else _cost2    := _cost                    ;
             if _revcost > 0 then _revcost2 := _revcost * Max( 0, 1-d )
                             else _revcost2 := _revcost                 ;
           end ;
      3 :  begin
             _cost2    := _cost    ;
             _revcost2 := _revcost ;
           end ;
      else begin
             assert( False, GIS_RS_ERR_UNTESTED ) ;
           end ;
    end;
  end ;

  procedure helper_linktype(
    const _shape2   : TGIS_Shape ;
    const _type     : Integer    ;
    var   _type2    : Integer
  ) ;
  begin
    case helper_shapetype( _shape2 ) of
      0 :  begin
             _type2    := _type  ;
           end ;
      1 :  begin
             _type2    := _type  ;
           end ;
      2 :  begin
             _type2    := _type  ;
           end ;
      3 :  begin
             _type2 := high( FCostModifiers ) ;
           end ;
      else begin
             assert( False, GIS_RS_ERR_UNTESTED ) ;
           end ;
    end;
  end ;

  procedure helper_level(
    const _shape2 : TGIS_Shape ;
    const _from   : ShortInt   ;
    const _to     : ShortInt   ;
    var   _from2  : ShortInt   ;
    var   _to2    : ShortInt
  ) ;
  begin
    case helper_shapetype( _shape2 ) of
      0 :  begin
             _from2 := _from ;
             _to2   := _to   ;
           end ;
      1 :  begin
             _from2 := _from ;
             _to2   := _to   ;
           end ;
      2 :  begin
             _from2 := _to   ;
             _to2   := _to   ;
           end ;
      3 :  begin
             _from2 := _to   ;
             _to2   := _to   ;
           end ;
      else begin
             assert( False, GIS_RS_ERR_UNTESTED ) ;
           end ;
    end;
  end;

begin
  if _shape.Layer <> networkObj.Layer then begin
    if helper_shapetype( _shape ) = 3 then
      shp := _shape
    else
      shp := TGIS_ShapeArc(
                  networkObj.Layer.GetShape(
                    Integer( _shape.GetField( GIS_GEO_SPLITMASTER ) )
                  )
             ) ;
  end
  else
    shp := _shape ;

  assert( assigned( shp ) ) ;

  cost      := _link.Cost        ;
  revcost   := _link.ReverseCost ;
  link_type := _link.LinkType    ;
  from_lvl  := _link.NodeA       ;
  to_lvl    := _link.NodeB       ;

  wascost := False ;
  {$IFDEF OXYGENE}
    if assigned( FOnLinkCost ) then begin
      cost_evnt := TGIS_LinkCostEventArgs.Create( shp, cost, revcost ) ;
      wascost := True ;
      try
        FOnLinkCost( _sender, cost_evnt ) ;
      finally
        cost    := cost_evnt.Cost    ;
        revcost := cost_evnt.RevCost ;
        FreeObject( cost_evnt ) ;
      end;
    end ;
  {$ELSE}
    if assigned( LinkCostEvent ) then begin
      LinkCostEvent( _sender, shp, cost, revcost ) ;
      wascost := True ;
    end ;
  {$ENDIF}

  // fix cost for geocoding results
  // but only if cost was recalculated for the master shape
  if wascost then
    helper_cost( _shape, cost, revcost, cost, revcost ) ;

  {$IFDEF OXYGENE}
    if assigned( FOnLinkType ) then begin
      type_evnt := TGIS_LinkTypeEventArgs.Create( shp, link_type ) ;
      try
        FOnLinkType( _sender, type_evnt ) ;
      finally
        link_type := type_evnt.LinkType ;
        FreeObject( type_evnt ) ;
      end;
    end ;
  {$ELSE}
    if assigned( LinkTypeEvent ) then
      LinkTypeEvent( _sender, shp, link_type ) ;
  {$ENDIF}

  // fix type for geocoding results
  helper_linktype( _shape, link_type, link_type ) ;

  {$IFDEF OXYGENE}
    if assigned( FOnLinkLevel ) then begin
      level_evnt := TGIS_LinkLevelEventArgs.Create( shp, from_lvl, to_lvl ) ;
      try
        FOnLinkLevel( _sender, level_evnt ) ;
      finally
        from_lvl := level_evnt.FromLevel ;
        to_lvl   := level_evnt.ToLevel   ;
        FreeObject( level_evnt ) ;
      end;
    end ;
  {$ELSE}
    if assigned( LinkLevelEvent ) then
      LinkLevelEvent( shp, _shape, from_lvl, to_lvl ) ;
  {$ENDIF}

  // fix level for geocoding results
  helper_level( _shape, from_lvl, to_lvl, from_lvl, to_lvl ) ;

  _link.Cost        := cost      ;
  _link.ReverseCost := revcost   ;
  _link.LinkType    := link_type ;
  _link.NodeA       := from_lvl  ;
  _link.NodeB       := to_lvl    ;

end;

procedure TGIS_ShortestPath.Regenerate( const _layer : TGIS_LayerVector ) ;
begin
  FreeObject( networkObj ) ;
  networkObj := TGIS_Network.Create( _layer, FStoragePath ) ;
  networkObj.Delete ;
  FreeObject( networkObj ) ;

  LoadTheData( _layer ) ;
end;

procedure TGIS_ShortestPath.LoadTheData( const _layer : TGIS_LayerVector ) ;
var
  end_uid : TGIS_Uid    ;
  abort   : Boolean    ;
  en      : TGIS_LayerVectorEnumerator ;
  shp     : TGIS_Shape ;
begin
  // Delete the old data if any.
  freeData;

  // set params
  FLayer := _layer ;
  if not assigned( Layer ) then exit ;

  FreeObject( networkObj ) ;
  networkObj := TGIS_Network.Create( self.Layer, FStoragePath ) ;

  {$IFDEF OXYGENE}
    {$IFDEF JAVA OR ISLAND}
      networkObj.LinkEvent := @doOnLink2 ;
    {$ELSE}
      networkObj.LinkEvent += doOnLink2 ;
    {$ENDIF}
  {$ELSE}
    networkObj.LinkEvent := doOnLink ;
  {$ENDIF}

  // Traverse layer
  if assigned( viewerObj ) then
    viewerObj.BusyPrepare( self, _rsrc( GIS_RS_BUSY_ROUTE_BUILD ) ) ;
  try
    if not networkObj.Ready then begin
      abort := False ;
      if assigned( viewerObj ) then
        end_uid := _layer.GetLastUid
      else
        end_uid := 1 ;
      en := Layer.Loop( GisWholeWorld, '' ).GetEnumerator ;
      try
        while en.MoveNext do begin
          shp := en.GetCurrent;

          if assigned( viewerObj ) then begin
            viewerObj.BusyShake( self, shp.Uid, end_uid, abort );
            if abort then break
          end;

          if shp is TGIS_ShapeArc then
            networkObj.AddShape( TGIS_ShapeArc( shp ), FLength ) ;
        end;

      finally
        FreeObject( en ) ;
      end;
      networkObj.Serialize ;
    end ;
    networkObj.Open ;
    UpdateTheData ;
  finally
    if assigned( viewerObj ) then
      viewerObj.BusyRelease( self );
  end ;

  startNode := nil ;
  endNode   := nil ;
end;

procedure TGIS_ShortestPath.UpdateTheData ;
var
  geo  : TGIS_LayerVector ;
  shp0 : TGIS_Shape    ;
  shp  : TGIS_ShapeArc ;
  en   : TGIS_LayerVectorEnumerator ;
begin
  networkObj.CleanDynamicLinks ;

  if assigned( viewerObj ) then
    viewerObj.BusyPrepare( self, _rsrc( GIS_RS_BUSY_ROUTE_UPDATE ) ) ;
  try
    // Traverse Geocoded layer
    if assigned( viewerObj ) then begin
      geo := TGIS_LayerVector( viewerObj.Get( GIS_GEO_LAYERNAME ) ) ;
      if assigned( geo ) then begin
        en := geo.Loop( GisWholeWorld, '' ).GetEnumerator ;
        try
          while en.MoveNext do begin
            shp0 := en.GetCurrent;

            if assigned( viewerObj ) then begin
              if viewerObj.HourglassShake then
                break ;
            end ;

            shp := TGIS_ShapeArc( shp0 ) ;
            if shp.TagInternal <= 2 then begin
              networkObj.AddShape( shp, FLength ) ;
              shp.TagInternal := 999 ;
            end ;
          end;
        finally
          FreeObject( en ) ;
        end;
      end ;
    end ;

    startNode := nil ;
    endNode   := nil ;
  finally
    if assigned( viewerObj ) then
      viewerObj.BusyRelease( self ) ;
  end;
end;

procedure TGIS_ShortestPath.StartPos(
  const _ptg  : TGIS_Point;
  const _prec : Double
) ;
begin
  if OSMRouting then begin
    T_osmRouting( osmRtg ).StartPos( _ptg, _prec ) ;
    exit ;
  end ;

  if not assigned( Layer ) then exit ;

  startNode := networkObj.FindNode( _ptg, _prec ) ;

  if assigned( viewerObj ) then
    viewerObj.BusyPrepare( self, _rsrc( GIS_RS_BUSY_ROUTE_FIND ) ) ;
  try
    findPathTree ;
    findPath;
  finally
    if assigned( viewerObj ) then
      viewerObj.BusyRelease( self ) ;
  end;
end ;

procedure TGIS_ShortestPath.StopPos(
  const _ptg  : TGIS_Point;
  const _prec : Double
) ;
begin
  if OSMRouting then begin
    T_osmRouting( osmRtg ).StopPos( _ptg, _prec ) ;
    exit ;
  end ;

  if not assigned( Layer ) then exit ;

  endNode := networkObj.FindNode( _ptg, _prec ) ;

  if assigned( viewerObj ) then
    viewerObj.BusyPrepare( self, _rsrc( GIS_RS_BUSY_ROUTE_FIND ) ) ;
  try
    findPathTree ;
    findPath;
    networkObj.Clean
  finally
    if assigned( viewerObj ) then
      viewerObj.BusyRelease( self ) ;
  end;
end ;

function TGIS_ShortestPath.Find(
  const _from : TGIS_Point ;
  const _to   : TGIS_Point
) : Boolean ;
begin
  if OSMRouting then begin
    Result := T_osmRouting( osmRtg ).Find( _from, _to ) ;
    exit ;
  end ;

  Result := False ;

  startNode := networkObj.FindNode( _from, 0 ) ;
  if not assigned( startNode ) then
    exit ;

  endNode := networkObj.FindNode( _to, 0 ) ;
  if not assigned( endNode ) then
    exit ;

  if assigned( viewerObj ) then
    viewerObj.BusyPrepare( self, _rsrc( GIS_RS_BUSY_ROUTE_FIND ) ) ;
  try
    findPathTree ;
    findPath;

    Result := ItemsCount > 0 ;
  finally
    if assigned( viewerObj ) then
      viewerObj.BusyRelease( self ) ;
  end;
end;

//==================================== END =====================================
end.
