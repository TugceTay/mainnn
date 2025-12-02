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
  Encapsulation of User Defined Vector layer.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerVectorUDF ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerVectorUDF"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}
{$IFDEF JAVA}
  namespace tatukgis.jdk ;
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

interface

{$IFDEF DCC}
  uses
    System.Classes,
    System.Variants,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoLayerVector;
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
  {$IFDEF OXYGENE}

    /// <summary>
    ///   Provides data to the GetShapeGeometry event.
    /// </summary>
    TGIS_GetShapeGeometryEventArgs = {$IFDEF OXYGENE} public {$ENDIF}
                                     class( EventArgs )
      private
        FUid      : TGIS_Uid   ;
        FInMemory : Boolean    ;
        FCursor   : Integer    ;
        FShape    : TGIS_Shape ;
      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_uid">
        ///   shape uid
        /// </param>
        /// <param name="_inMemory">
        ///   is shape in memory
        /// </param>
        /// <param name="_cursor">
        ///   cursor id
        /// </param>
        constructor Create  ( const _uid          : TGIS_Uid ;
                              const _inMemory     : Boolean ;
                              const _cursor       : Integer
                            ) ;
      public

        /// <summary>
        ///   Shape uid.
        /// </summary>
        property Uid      : TGIS_Uid         read  FUid      ;

        /// <summary>
        ///   Is shape in memory.
        /// </summary>
        property InMemory : Boolean         read  FInMemory ;

        /// <summary>
        ///   Cursor id.
        /// </summary>
        property Cursor   : Integer         read  FCursor   ;

        /// <summary>
        ///   Handle to created shape.
        /// </summary>
        property Shape    : TGIS_Shape      read  FShape
                                              write FShape    ;
    end ;

    /// <summary>
    ///   Event fired to get shape geometry.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_e">
    ///   event arguments
    /// </param>
    TGIS_GetShapeGeometryEvent = public procedure(
      _sender : TObject ;
      _e      : TGIS_GetShapeGeometryEventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   Event fired to get shape geometry.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_uid">
    ///   shape uid
    /// </param>
    /// <param name="_inMemory">
    ///   is shape in memory
    /// </param>
    /// <param name="_cursor">
    ///   cursor id
    /// </param>
    /// <param name="_shp">
    ///   handle to created shape
    /// </param>
    TGIS_GetShapeGeometryEvent = procedure(
            _sender     : TObject    ;
            _uid        : TGIS_Uid    ;
            _inMemory   : Boolean    ;
            _cursor     : Integer    ;
      var   _shp        : TGIS_Shape
    ) of object ;
  {$ENDIF}

  {$IFDEF OXYGENE}

    /// <summary>
    ///   Provides data to the GetLayerExtent event.
    /// </summary>
    TGIS_GetLayerExtentEventArgs = {$IFDEF OXYGENE} public {$ENDIF}
                                   class ( EventArgs )
      private
        FExtent : TGIS_Extent ;
      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_extent">
        ///   layer extent
        /// </param>
        constructor Create  ( const _extent       : TGIS_Extent
                            ) ;
      public

        /// <summary>
        ///   Layer extent.
        /// </summary>
        property Extent   : TGIS_Extent     read  FExtent
                                              write FExtent ;
    end ;

    /// <summary>
    ///   Event fired to get layer extent.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_e">
    ///   event arguments
    /// </param>
    TGIS_GetLayerExtentEvent = public procedure(
      _sender : TObject ;
      _e      : TGIS_GetLayerExtentEventArgs
    ) of object ;
  {$ELSE}
    /// <summary>
    ///   Event fired to get layer extent.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_extent">
    ///   layer extent
    /// </param>
    TGIS_GetLayerExtentEvent = procedure(
          _sender    : TObject           ;
      var _extent    : TGIS_Extent
    ) of object ;
  {$ENDIF}

  {$IFDEF OXYGENE}

    /// <summary>
    ///   Event fired to get shape field value.
    /// </summary>
    TGIS_GetShapeFieldEventArgs = {$IFDEF OXYGENE} public {$ENDIF}
                                  class( EventArgs )
      private
        FField      : String  ;
        FUid        : TGIS_Uid ;
        FCursor     : Integer ;
        FValue      : Variant ;
      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_field">
        ///   field name
        /// </param>
        /// <param name="_uid">
        ///   shape uid
        /// </param>
        /// <param name="_cursor">
        ///   cursor id
        /// </param>
        constructor Create  ( const _field        : String  ;
                              const _uid          : TGIS_Uid ;
                              const _cursor       : Integer
                            ) ;
      public

        /// <summary>
        ///   Field name.
        /// </summary>
        property Field    : String          read FField  ;

        /// <summary>
        ///   Shape uid.
        /// </summary>
        property Uid      : TGIS_Uid         read FUid    ;

        /// <summary>
        ///   Cursor id.
        /// </summary>
        property Cursor   : Integer         read FCursor ;

        /// <summary>
        ///   Cursor id.
        /// </summary>
        property Value    : Variant         read  FValue
                                              write FValue ;
    end ;

    /// <summary>
    ///   Event fired to get shape field value.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_e">
    ///   event arguments
    /// </param>
    TGIS_GetShapeFieldEvent = public procedure(
      _sender : TObject ;
      _e      : TGIS_GetShapeFieldEventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   Event fired to get shape field value.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_field">
    ///   field name
    /// </param>
    /// <param name="_uid">
    ///   shape uid
    /// </param>
    /// <param name="_cursor">
    ///   cursor id
    /// </param>
    /// <param name="_value">
    ///   field value
    /// </param>
    TGIS_GetShapeFieldEvent = procedure(
            _sender     : TObject ;
            _field      : String  ;
            _uid        : TGIS_Uid ;
            _cursor     : Integer ;
      var   _value      : Variant
    ) of object ;
  {$ENDIF}

  {$IFDEF OXYGENE}

    /// <summary>
    ///   Event fired when layer executes MoveFirst.
    /// </summary>
    /// <param name="_cursor">
    ///   cursor identifier (see TGIS_LayerVector.cursorOpen)
    /// </param>
    /// <param name="_extent">
    ///   extent of item to be found
    /// </param>
    /// <param name="_viewerCS">
    ///   if True an the layer has been attached to the Viewer
    ///   then expected _extent units are in a Viewer coordinate
    ///   space; otherwise expected _extent units are in a Layer
    ///   coordinate space
    /// </param>
    /// <param name="_query">
    ///   query which must be matched by item; closely mimics
    ///   SQL WHERE clause; for examples you can use 'AGE >= 18';
    ///   empty (default) means that no all items will match.
    /// </param>
    /// <param name="_shape">
    ///   if not nil, then only shapes matched dm9 matrix with
    ///   _shape will be found
    /// </param>
    /// <param name="_de9i">
    ///   DM9 matrix of comparison
    /// </param>
    /// <param name="_skipDeleted">
    ///   set a skip deleted treatment; by default deleted items
    ///   will be ignored
    /// </param>
    TGIS_LayerMoveFirstEventArgs =  {$IFDEF OXYGENE} public {$ENDIF}
                                    class(EventArgs )
      private
        FCursor      : Integer     ;
        FExtent      : TGIS_Extent ;
        FQuery       : String      ;
        FShape       : TGIS_Shape  ;
        FDe9i        : String      ;
        FSkipDeleted : Boolean     ;
      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_cursor">
        ///   cursor identifier (see TGIS_LayerVector.cursorOpen)
        /// </param>
        /// <param name="_extent">
        ///   extent of item to be found
        /// </param>
        /// <param name="_query">
        ///   query which must be matched by item; closely mimics SQL WHERE
        ///   clause; for examples you can use 'AGE &gt;= 18'; empty (default)
        ///   means that no all items will match.
        /// </param>
        /// <param name="_shape">
        ///   if not nil, then only shapes matched dm9 matrix with _shape will be
        ///   found
        /// </param>
        /// <param name="_de9i">
        ///   DM9 matrix of comparison
        /// </param>
        /// <param name="_skipDeleted">
        ///   set a skip deleted treatment; by default deleted items will be
        ///   ignored
        /// </param>
        constructor Create  ( const _cursor       : Integer     ;
                              const _extent       : TGIS_Extent ;
                              const _query        : String      ;
                              const _shape        : TGIS_Shape  ;
                              const _de9i         : String      ;
                              const _skipDeleted  : Boolean
                            ) ;
      public

        /// <summary>
        ///   Cursor identifier (see TGIS_LayerVector.cursorOpen).
        /// </summary>
        property Cursor       : Integer     read FCursor ;

        /// <summary>
        ///   Extent of item to be found.
        /// </summary>
        property Extent       : TGIS_Extent read FExtent ;

        /// <summary>
        ///   Query which must be matched by item; closely mimics
        ///   SQL WHERE clause; for examples you can use 'AGE >= 18';
        ///   empty (default) means that no all items will match..
        /// </summary>
        property Query        : String      read FQuery  ;

        /// <summary>
        ///   if not nil, then only shapes matched dm9 matrix with
        ///   _shape will be found.
        /// </summary>
        property Shape        : TGIS_Shape  read FShape  ;

        /// <summary>
        ///   DM9 matrix of comparison.
        /// </summary>
        property De9i         : String      read FDe9i   ;

        /// <summary>
        ///   Set a skip deleted treatment; by default deleted items
        ///   will be ignored.
        /// </summary>
        property SkipDeleted  : Boolean     read FSkipDeleted ;
      end ;

      /// <summary>
      ///   Event fired when layer executes MoveFirst.
      /// </summary>
      /// <param name="_sender">
      ///   sender object from a layer
      /// </param>
      /// <param name="_e">
      ///   event arguments
      /// </param>
      TGIS_LayerMoveFirstEvent = public procedure(
        _sender : TObject ;
        _e      : TGIS_LayerMoveFirstEventArgs
      ) of object ;
  {$ELSE}
    /// <summary>
    ///   Event fired when layer executes MoveFirst.
    /// </summary>
    /// <param name="_sender">
    ///   sender object
    /// </param>
    /// <param name="_cursor">
    ///   cursor identifier (see TGIS_LayerVector.cursorOpen)
    /// </param>
    /// <param name="_extent">
    ///   extent of item to be found
    /// </param>
    /// <param name="_query">
    ///   query which must be matched by item; closely mimics SQL WHERE
    ///   clause; for examples you can use 'AGE &gt;= 18'; empty (default)
    ///   means that no all items will match.
    /// </param>
    /// <param name="_shape">
    ///   if not nil, then only shapes matched dm9 matrix with _shape will be
    ///   found
    /// </param>
    /// <param name="_de9i">
    ///   DM9 matrix of comparison
    /// </param>
    /// <param name="_skipDeleted">
    ///   set a skip deleted treatment; by default deleted items will be
    ///   ignored
    /// </param>
    TGIS_LayerMoveFirstEvent = procedure(
            _sender      : TObject           ;
            _cursor      : Integer           ;
      const _extent      : TGIS_Extent       ;
            _query       : String            ;
            _shape       : TGIS_Shape        ;
            _de9i        : String            ;
            _skipDeleted : Boolean
    ) of object ;
  {$ENDIF}

  {$IFDEF OXYGENE}

    /// <summary>
    ///   Event fired when layer executes MoveNext.
    /// </summary>
    TGIS_LayerMoveNextEventArgs = {$IFDEF OXYGENE} public {$ENDIF}
                                  class( EventArgs )
      private
        FCursor : Integer ;
      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_cursor">
        ///   cursor identifier 
        /// </param>
        constructor Create  ( const _cursor   : Integer
                            ) ;
      public

          /// <summary>
          ///   Cursor identifier (see TGIS_LayerVector.cursorOpen).
          /// </summary>
          property Cursor   : Integer         read FCursor ;
    end ;

    /// <summary>
    ///   Event fired when layer executes MoveNext.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_e">
    ///   event arguments
    /// </param>
    TGIS_LayerMoveNextEvent = public procedure(
      _sender : TObject ;
      _e      : TGIS_LayerMoveNextEventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   Event fired when layer executes MoveNext.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_cursor">
    ///   cursor identifier 
    /// </param>
    TGIS_LayerMoveNextEvent = procedure(
      _sender      : TObject ;
      _cursor      : Integer
    ) of object ;
  {$ENDIF}

  {$IFDEF OXYGENE}

    /// <summary>
    ///   Event fired to get layer structure.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_e">
    ///   event arguments
    /// </param>
    TGIS_LayerGetStructureEvent = public procedure(
      _sender : TObject ;
      _e      : EventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   Event fired to get layer structure.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    TGIS_LayerGetStructureEvent = procedure(
      _sender      : TObject
    ) of object ;
  {$ENDIF}

  {$IFDEF OXYGENE}

    /// <summary>
    ///   Event fired to get layer eof.
    /// </summary>
    TGIS_LayerEofEventArgs = {$IFDEF OXYGENE} public {$ENDIF}
                             class( EventArgs )
      private
        FCursor : Integer ;
        FEof    : Boolean ;
      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_cursor">
        ///   cursor id
        /// </param>
        constructor Create  ( const _cursor   : Integer
                            ) ;
      public

        /// <summary>
        ///   Cursor id.
        /// </summary>
        property Cursor   : Integer         read FCursor ;

        /// <summary>
        ///   Is layer eof True.
        /// </summary>
        property Eof      : Boolean         read  FEof
                                            write FEof   ;
    end ;

    /// <summary>
    ///   Event fired to get layer eof.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_e">
    ///   event arguments
    /// </param>
    TGIS_LayerEofEvent = public procedure(
      _sender : TObject ;
      _e      : TGIS_LayerEofEventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   Event fired to get layer eof.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_cursor">
    ///   cursor id
    /// </param>
    /// <param name="_eof">
    ///   is layer eof True
    /// </param>
    TGIS_LayerEofEvent = procedure(
          _sender    : TObject ;
          _cursor    : Integer ;
      var _eof       : Boolean
    ) of object ;
  {$ENDIF}

  {$IFDEF OXYGENE}

    /// <summary>
    ///   Event fired to get layer shape.
    /// </summary>
    TGIS_GetShapeObjectEventArgs = {$IFDEF OXYGENE} public {$ENDIF}
                                   class( EventArgs )
      private
        FUid    : TGIS_Uid    ;
        FCursor : Integer    ;
        FShape  : TGIS_Shape ;
      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_uid">
        ///   shape uid
        /// </param>
        /// <param name="_cursor">
        ///   cursor id
        /// </param>
        constructor Create  ( const _uid      : TGIS_Uid ;
                              const _cursor   : Integer
                            ) ;
      public

        /// <summary>
        ///   Shape uid.
        /// </summary>
        property Uid      : TGIS_Uid         read FUid    ;

        /// <summary>
        ///   Cursor id.
        /// </summary>
        property Cursor   : Integer         read FCursor ;

        /// <summary>
        ///   Shape object.
        /// </summary>
        property Shape    : TGIS_Shape      read  FShape
                                            write FShape    ;
    end ;

    /// <summary>
    ///   Event fired to get layer shape.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_e">
    ///   event arguments
    /// </param>
    TGIS_GetShapeObjectEvent = public procedure(
      _sender : TObject ;
      _e      : TGIS_GetShapeObjectEventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   Event fired to get layer shape.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_uid">
    ///   shape uid
    /// </param>
    /// <param name="_cursor">
    ///   cursor id
    /// </param>
    /// <param name="_shape">
    ///   shape object
    /// </param>
    TGIS_GetShapeObjectEvent = procedure(
          _sender  : TObject ;
          _uid     : TGIS_Uid ;
          _cursor  : Integer ;
      var _shape   : TGIS_Shape
    ) of object ;
  {$ENDIF}

  {$IFDEF OXYGENE}

    /// <summary>
    ///   Event fired to get last layer shape uid.
    /// </summary>
    /// <param name="_uid">
    ///   shape uid
    /// </param>
    TGIS_GetLayerLastShapeUidEventArgs  = {$IFDEF OXYGENE} public {$ENDIF}
                                          class( EventArgs )
      private
        FUid : TGIS_Uid ;
      public

        /// <summary>
        ///   Create an object.
        /// </summary>
        /// <param name="_uid">
        ///   shape uid
        /// </param>
        constructor Create  ( const _uid      : TGIS_Uid
                            ) ;
      public

        /// <summary>
        ///   Shape uid.
        /// </summary>
        property Uid      : TGIS_Uid         read  FUid
                                            write FUid ;
    end ;

    /// <summary>
    ///   Event fired to get last layer shape uid.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_e">
    ///   event arguments
    /// </param>
    TGIS_GetLayerLastShapeUidEvent  = public procedure(
      const _sender : TObject ;
      const _e      : TGIS_GetLayerLastShapeUidEventArgs
    ) of object ;
  {$ELSE}

    /// <summary>
    ///   Event fired to get last layer shape uid.
    /// </summary>
    /// <param name="_sender">
    ///   sender object from a layer
    /// </param>
    /// <param name="_uid">
    ///   shape uid
    /// </param>
    TGIS_GetLayerLastShapeUidEvent  = procedure(
            _sender  : TObject ;
        var _uid     : TGIS_Uid
    ) of object ;
  {$ENDIF}

  {$IFDEF OXYGENE}
    T_cursorUdf nested in TGIS_LayerVectorUDF = public record
      public

        /// <summary>
        ///   Is cursor in use.
        /// </summary>
        curInUse       : Boolean ;

        /// <summary>
        ///   Eof marker.
        /// </summary>
        isEof          : Boolean ;

        /// <summary>
        ///   Current shape. Layer access is based on record by record access.
        /// </summary>
        currShape      : TGIS_Shape ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currPoint      : TGIS_ShapePoint ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currArc        : TGIS_ShapeArc ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currPolygon    : TGIS_ShapePolygon ;

        /// <summary>
        ///   Preallocated shape. Recreating it on each MoveNext call is much
        ///   faster than full Create constructor.
        /// </summary>
        currMultipoint : TGIS_ShapeMultiPoint  ;

        /// <summary>
        ///   Current UID in a shape file.
        /// </summary>
        currUid        : TGIS_Uid ;
    end ;
  {$ENDIF}

  /// <summary>
  ///   Encapsulation of User Defined Vector layer.
  /// </summary>
  /// <remarks>
  ///   Based on this layer user can read different formats and using events
  ///   can fill the layer with proper parameters and values.
  /// </remarks>
  TGIS_LayerVectorUDF = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_LayerVector )
    private

      /// <summary>
      ///   Event handler fired to get shape geometry.
      /// </summary>
      FOnGetShapeGeometry     : TGIS_GetShapeGeometryEvent ;

      /// <summary>
      ///   Event handler fired to get layer extent.
      /// </summary>
      FOnGetLayerExtent       : TGIS_GetLayerExtentEvent ;

      /// <summary>
      ///   Event handler fired to get shape field value.
      /// </summary>
      FOnGetShapeField        : TGIS_GetShapeFieldEvent ;

      /// <summary>
      ///   Event handler fired when a layer executes MoveFirst.
      /// </summary>
      FOnLayerMoveFirst       : TGIS_LayerMoveFirstEvent ;

      /// <summary>
      ///   Event handler fired when layer executes MoveNext.
      /// </summary>
      FOnLayerMoveNext        : TGIS_LayerMoveNextEvent ;

      /// <summary>
      ///   Event handler fired to get layer eof.
      /// </summary>
      FOnLayerEof             : TGIS_LayerEofEvent ;

      /// <summary>
      ///   Event handler fired to get layer shape.
      /// </summary>
      FOnGetShapeObject       : TGIS_GetShapeObjectEvent ;

      /// <summary>
      ///   Event fired to get last layer shape uid.
      /// </summary>
      FOnGetLayerLastShapeUid : TGIS_GetLayerLastShapeUidEvent ;

      /// <summary>
      ///   Event fired to get layer structure.
      /// </summary>
      FOnGetLayerStructure    : TGIS_LayerGetStructureEvent ;
    protected
      {$IFDEF OXYGENE}
        /// <summary>
        ///   Cursor definition.
        /// </summary>
        cursorUdf : array of T_cursorUdf ;
      {$ELSE}
        /// <summary>
        ///   Cursor definition.
        /// </summary>
        cursorUdf : array of record

          /// <summary>
          ///   Is cursor in use.
          /// </summary>
          curInUse       : Boolean ;

          /// <summary>
          ///   Eof marker.
          /// </summary>
          isEof          : Boolean ;

          /// <summary>
          ///   Current shape. Layer access is based on record by record access.
          /// </summary>
          currShape      : TGIS_Shape ;

          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster than full Create constructor.
          /// </summary>
          currPoint      : TGIS_ShapePoint ;

          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster than full Create constructor.
          /// </summary>
          currArc        : TGIS_ShapeArc ;

          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster than full Create constructor.
          /// </summary>
          currPolygon    : TGIS_ShapePolygon ;

          /// <summary>
          ///   Preallocated shape. Recreating it on each MoveNext call is much
          ///   faster than full Create constructor.
          /// </summary>
          currMultipoint : TGIS_ShapeMultiPoint  ;

          /// <summary>
          ///   Current UID in a shape file.
          /// </summary>
          currUid        : TGIS_Uid ;
        end ;
      {$ENDIF}

      /// <summary>
      ///   Last UID in a shape file. If not set then -1.
      /// </summary>
      lastUid         : TGIS_Uid ;

      /// <summary>
      ///   In memory flag.
      /// </summary>
      FInMemory       : Boolean ;

    {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}

      /// <summary>
      ///   Read layer shape.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>      
      procedure readShape       ( const _cursor      : Integer
                                ) ;
      // for internal use of TGIS_Viewer

      /// <inheritdoc/>
      function  getFieldInternal( const _uid         : TGIS_Uid          ;
                                  const _name        : String           ;
                                  const _cursor      : Integer
                                ) : Variant ; override;

      /// <inheritdoc/>
      procedure setUp           ; override;

      // cursor access function(s)

      /// <inheritdoc/>
      function  cursorOpen      : Integer ; override;

      /// <inheritdoc/>
      procedure cursorClose     ( const _cursor      : Integer
                                ) ; override;

      /// <inheritdoc/>
      procedure cursorFirst     ( const _cursor      : Integer          ;
                                  const _viewerCS    : Boolean          ;
                                  const _extent      : TGIS_Extent      ;
                                  const _query       : String           ;
                                  const _shape       : TGIS_Shape       ;
                                  const _de9im       : String           ;
                                  const _skipDeleted : Boolean
                                ) ; override;

      /// <inheritdoc/>
      procedure cursorNext      ( const _cursor      : Integer
                                ) ; override;

      /// <inheritdoc/>
      function  cursorEof       ( const _cursor      : Integer
                                ) : Boolean ; override;

      /// <inheritdoc/>
      function  cursorShape     ( const _cursor      : Integer
                                ) : TGIS_Shape ; override;
    protected

      /// <inheritdoc/>
      procedure doDestroy       ; override;
    public
      // various public routines

      /// <inheritdoc/>
      constructor Create        ; override;

      /// <inheritdoc/>
      procedure RecalcExtent    ; override;

      /// <inheritdoc/>
      function  GetShape        ( const _uid         : TGIS_Uid          ;
                                  const _cursor      : Integer
                                ) : TGIS_Shape ; override;

      /// <inheritdoc/>
      function  GetLastUid      : TGIS_Uid ; override;

    public // properties

        /// <summary>
        ///   If True, all shapes will be kept in memory, otherwise will
        ///   disc-based.
        /// </summary>
        property InMemory               : Boolean
                                          read  FInMemory
                                          write FInMemory
                                          {$IFNDEF OXYGENE}
                                            default True
                                          {$ENDIF} ;
    published // events
      {$IFDEF CLR}
        /// <event/> 
        /// <summary>
        ///   GetShapeGeometry event. Fired to get layer shape geometry.
        /// </summary>
        event    GetShapeGeometryEvent : TGIS_GetShapeGeometryEvent
                                         delegate FOnGetShapeGeometry ;
      {$ELSE}
        /// <event/> 
        /// <summary>
        ///   GetShapeGeometry event. Fired to get layer shape geometry.
        /// </summary>
        property   GetShapeGeometryEvent : TGIS_GetShapeGeometryEvent
                                           read  FOnGetShapeGeometry
                                           write FOnGetShapeGeometry ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/> 
        /// <summary>
        ///   GetLayerExtent event. Fired to get layer extent.
        /// </summary>
        event    GetLayerExtentEvent : TGIS_GetLayerExtentEvent
                                       delegate FOnGetLayerExtent ;
      {$ELSE}
        /// <event/> 
        /// <summary>
        ///   GetLayerExtent event. Fired to get layer extent.
        /// </summary>
        property   GetLayerExtentEvent : TGIS_GetLayerExtentEvent
                                         read  FOnGetLayerExtent
                                         write FOnGetLayerExtent ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/> 
        /// <summary>
        ///   GetShapeField event. Fired to get shape field value.
        /// </summary>
        event    GetShapeFieldEvent : TGIS_GetShapeFieldEvent
                                      delegate FOnGetShapeField ;
      {$ELSE}
        /// <event/> 
        /// <summary>
        ///   GetShapeField event. Fired to get shape field value.
        /// </summary>
        property   GetShapeFieldEvent : TGIS_GetShapeFieldEvent
                                        read  FOnGetShapeField
                                        write FOnGetShapeField ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/> 
        /// <summary>
        ///   LayerMoveFirst event. Fired when a layer executes MoveFirst.
        /// </summary>
        event    LayerMoveFirstEvent : TGIS_LayerMoveFirstEvent
                                       delegate FOnLayerMoveFirst ;
      {$ELSE}
        /// <event/> 
        /// <summary>
        ///   LayerMoveFirst event. Fired when a layer executes MoveFirst.
        /// </summary>
        property   LayerMoveFirstEvent : TGIS_LayerMoveFirstEvent
                                         read  FOnLayerMoveFirst
                                         write FOnLayerMoveFirst ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/> 
        /// <summary>
        ///   LayerMoveNext event. Fired when a layer executes MoveNext.
        /// </summary>
        event    LayerMoveNextEvent : TGIS_LayerMoveNextEvent
                                      delegate FOnLayerMoveNext ;
      {$ELSE}
        /// <event/> 
        /// <summary>
        ///   LayerMoveNext event. Fired when layer executes MoveNext.
        /// </summary>
        property   LayerMoveNextEvent : TGIS_LayerMoveNextEvent
                                        read  FOnLayerMoveNext
                                        write FOnLayerMoveNext ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/> 
        /// <summary>
        ///   LayerEof event. Fired to get layer eof.
        /// </summary>
        event    LayerEofEvent      : TGIS_LayerEofEvent
                                      delegate FOnLayerEof ;
      {$ELSE}
        /// <event/> 
        /// <summary>
        ///   LayerEof event. Fired to get layer eof.
        /// </summary>
        property   LayerEofEvent      : TGIS_LayerEofEvent
                                        read  FOnLayerEof
                                        write FOnLayerEof ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/> 
        /// <summary>
        ///   GetShape event. Fired to get layer shape.
        /// </summary>
        event    GetShapeObjectEvent : TGIS_GetShapeObjectEvent
                                       delegate FOnGetShapeObject ;
      {$ELSE}
        /// <event/> 
        /// <summary>
        ///   GetShape event. Fired to get layer shape.
        /// </summary>
        property   GetShapeObjectEvent : TGIS_GetShapeObjectEvent
                                         read  FOnGetShapeObject
                                         write FOnGetShapeObject ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/> 
        /// <summary>
        ///   GetLayerLastShapeUid event. Fired to get last layer shape uid.
        /// </summary>
        event    GetLayerLastShapeUidEvent : TGIS_GetLayerLastShapeUidEvent
                                             delegate FOnGetLayerLastShapeUid ;
      {$ELSE}
        /// <event/> 
        /// <summary>
        ///   GetLayerLastShapeUid event. Fired to get last layer shape uid.
        /// </summary>
        property   GetLayerLastShapeUidEvent : TGIS_GetLayerLastShapeUidEvent
                                        read  FOnGetLayerLastShapeUid
                                        write FOnGetLayerLastShapeUid ;
      {$ENDIF}

      {$IFDEF CLR}
        /// <event/> 
        /// <summary>
        ///   Event fired to get layer structure.
        /// </summary>
        event    GetLayerStructureEvent : TGIS_LayerGetStructureEvent
                                          delegate FOnGetLayerStructure ;
      {$ELSE}
        /// <event/> 
        /// <summary>
        ///   Event fired to get layer structure.
        /// </summary>
        property   GetLayerStructureEvent : TGIS_LayerGetStructureEvent
                                            read  FOnGetLayerStructure
                                            write FOnGetLayerStructure ;
      {$ENDIF}
  end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SyncObjs,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoInternals,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoClasses ;
{$ENDIF}

//==============================================================================
// TGIS_GetShapeGeometryEventArgs
//==============================================================================

  {$IFDEF OXYGENE}
    constructor TGIS_GetShapeGeometryEventArgs.Create(
      const _uid      : TGIS_Uid ;
      const _inMemory : Boolean ;
      const _cursor   : Integer
    ) ;
    begin
      inherited Create ;

      FUid      := _uid      ;
      FInMemory := _inMemory ;
      FCursor   := _cursor   ;
      FShape    := nil       ;
    end ;
  {$ENDIF}

//==============================================================================
// TGIS_GetLayerExtentEventArgs
//==============================================================================

  {$IFDEF OXYGENE}
    constructor TGIS_GetLayerExtentEventArgs.Create(
      const _extent : TGIS_Extent
    ) ;
    begin
      inherited Create ;

      FExtent := _extent ;
    end;
  {$ENDIF}

//==============================================================================
// TGIS_GetShapeFieldEventArgs
//==============================================================================

  {$IFDEF OXYGENE}
    constructor TGIS_GetShapeFieldEventArgs.Create(
      const _field  : String  ;
      const _uid    : TGIS_Uid ;
      const _cursor : Integer
    ) ;
    begin
      inherited Create ;

      FField  := _field     ;
      FUid    := _uid       ;
      FCursor := _cursor    ;
      FValue  := Unassigned ;
    end;
  {$ENDIF}

//==============================================================================
// TGIS_LayerMoveFirstEventArgs
//==============================================================================

  {$IFDEF OXYGENE}
    constructor TGIS_LayerMoveFirstEventArgs.Create(
      const _cursor      : Integer     ;
      const _extent      : TGIS_Extent ;
      const _query       : String      ;
      const _shape       : TGIS_Shape  ;
      const _de9i        : String      ;
      const _skipDeleted : Boolean
    ) ;
    begin
      inherited Create ;

      FCursor      := _cursor      ;
      FExtent      := _extent      ;
      FQuery       := _query       ;
      FShape       := _shape       ;
      FDe9i        := _de9i        ;
      FSkipDeleted := _skipDeleted ;
    end;
  {$ENDIF}

//==============================================================================
// TGIS_LayerMoveNextEventArgs
//==============================================================================

  {$IFDEF OXYGENE}
    constructor TGIS_LayerMoveNextEventArgs.Create(
      const _cursor : Integer
    ) ;
    begin
      inherited Create ;

      FCursor := _cursor ;
    end;
  {$ENDIF}

//==============================================================================
// TGIS_LayerEofEventArgs
//==============================================================================

  {$IFDEF OXYGENE}
    constructor TGIS_LayerEofEventArgs.Create(
      const _cursor : Integer
    ) ;
    begin
      inherited Create ;

      FCursor := _cursor ;
      FEof    := False   ;
    end;
  {$ENDIF}

//==============================================================================
// TGIS_GetShapeObjectEventArgs
//==============================================================================

  {$IFDEF OXYGENE}
    constructor TGIS_GetShapeObjectEventArgs.Create(
      const _uid    : TGIS_Uid ;
      const _cursor : Integer
    ) ;
    begin
      inherited Create ;

      FUid    := _uid    ;
      FCursor := _cursor ;
      FShape  := nil     ;
    end;
  {$ENDIF}

//==============================================================================
// TGIS_GetLayerLastShapeUidEventArgs
//==============================================================================

  {$IFDEF OXYGENE}
    constructor TGIS_GetLayerLastShapeUidEventArgs.Create(
      const _uid : TGIS_Uid
    ) ;
    begin
      inherited Create ;

      FUid := _uid ;
    end;
  {$ENDIF}

//==============================================================================
// TGIS_LayerVectorUDF
//==============================================================================

  constructor TGIS_LayerVectorUDF.Create;
  begin
    inherited;

    FInMemory            := True ;
    FSupportedDimensions := GisGetEmptyDimensionType ;
    FSupportedDimensions := GisAddDimensionType( FSupportedDimensions, TGIS_DimensionType.Unknown ) ;
    FSupportedDimensions := GisAddDimensionType( FSupportedDimensions, TGIS_DimensionType.XY      ) ;
    FSupportedDimensions := GisAddDimensionType( FSupportedDimensions, TGIS_DimensionType.XYZ     ) ;
    FSupportedDimensions := GisAddDimensionType( FSupportedDimensions, TGIS_DimensionType.XYM     ) ;
    FSupportedDimensions := GisAddDimensionType( FSupportedDimensions, TGIS_DimensionType.XYZM    ) ;
  end;

  procedure TGIS_LayerVectorUDF.doDestroy ;
  begin

    inherited ;
  end;

  function  TGIS_LayerVectorUDF.cursorOpen
    : Integer ;
  begin
    lockThread ;
    try
      Result := inherited cursorOpen ;

      if Result >= length( cursorUdf )  then
        SetLength( cursorUdf, Result + 1 ) ;
      {$IFDEF GIS_NORECORDS}
        if not assigned( cursorUdf[Result] ) then
          cursorUdf[Result] := new T_cursorUdf ;
      {$ENDIF}
      cursorUdf[Result].curInUse := True ;

      cursorUdf[Result].currPoint      := TGIS_ShapePoint.Create(
                                                nil, nil, False, -1, nil,
                                                TGIS_DimensionType.XYZM
                                              ) ;
      cursorUdf[Result].currMultipoint := TGIS_ShapeMultiPoint.Create(
                                                nil, nil, False, -1, nil,
                                                TGIS_DimensionType.XYZM
                                              ) ;
      cursorUdf[Result].currArc        := TGIS_ShapeArc.Create(
                                                nil, nil, False, -1, nil,
                                                TGIS_DimensionType.XYZM
                                              ) ;
      cursorUdf[Result].currPolygon    := TGIS_ShapePolygon.Create(
                                                nil, nil, False, -1, nil,
                                                TGIS_DimensionType.XYZM
                                              ) ;
    finally
      unlockThread ;
    end ;
  end;

  procedure TGIS_LayerVectorUDF.cursorClose(
    const _cursor      : Integer
  ) ;
  var
    i : Integer ;
  begin
    lockThread ;
    try
      cursorUdf[_cursor].curInUse := False ;
      FreeObject( cursorUdf[_cursor].currPoint ) ;
      FreeObject( cursorUdf[_cursor].currMultipoint ) ;
      FreeObject( cursorUdf[_cursor].currArc ) ;
      FreeObject( cursorUdf[_cursor].currPolygon ) ;

      // truncate cursorState at the tail;
      for i := length( cursorUdf ) - 1 downto 0 do begin
        if not cursorUdf[i].curInUse then begin
          SetLength( cursorUdf, i ) ;
        end
        else
          break ;
      end ;

      inherited cursorClose( _cursor ) ;
    finally
      unlockThread ;
    end ;
  end;

  procedure TGIS_LayerVectorUDF.readShape(
    const _cursor   : Integer
  );
  var
    shp : TGIS_Shape ;
    {$IFDEF OXYGENE}
      args : TGIS_GetShapeGeometryEventArgs ;
    {$ENDIF}
  begin
    shp := nil ;

    try
      if assigned( FOnGetShapeGeometry ) then begin
        {$IFDEF OXYGENE}
          args := TGIS_GetShapeGeometryEventArgs.Create(
                    cursorUdf[_cursor].currUid, FInMemory, _cursor
                  ) ;
          try
            FOnGetShapeGeometry( self, args ) ;
          finally
            shp := args.Shape ;
            FreeObject( args ) ;
          end;
        {$ELSE}
          FOnGetShapeGeometry( self, cursorUdf[_cursor].currUid,
                               FInMemory, _cursor, shp
                             ) ;
        {$ENDIF}
      end;
    except
      FreeObject( shp );
    end;

    if not assigned( shp ) then exit ;

    if InMemory then begin
      cursorUdf[_cursor].currShape := AddShape( shp ) ;
      FreeObject( shp ) ;
      lastUid := cursorUdf[_cursor].currShape.Uid ;
    end
    else begin
      case shp.ShapeType of
        TGIS_ShapeType.Point :
           begin
             cursorUdf[_cursor].currPoint.Recreate     ( shp, nil, True,
                          cursorUdf[_cursor].currUid, self, TGIS_DimensionType.XYZM ) ;
             FreeObject( shp ) ;
             cursorUdf[_cursor].currShape := cursorUdf[_cursor].currPoint ;
           end ;
        TGIS_ShapeType.MultiPoint :
           begin
             cursorUdf[_cursor].currMultipoint.Recreate( shp, nil, True,
                          cursorUdf[_cursor].currUid, self, TGIS_DimensionType.XYZM ) ;
             FreeObject( shp ) ;
             cursorUdf[_cursor].currShape := cursorUdf[_cursor].currMultipoint ;
           end ;
        TGIS_ShapeType.Arc :
           begin
             cursorUdf[_cursor].currArc.Recreate       ( shp, nil, True,
                          cursorUdf[_cursor].currUid, self, TGIS_DimensionType.XYZM ) ;
             FreeObject( shp ) ;
             cursorUdf[_cursor].currShape := cursorUdf[_cursor].currArc ;
           end ;
        TGIS_ShapeType.Polygon :
           begin
             cursorUdf[_cursor].currPolygon.Recreate   ( shp, nil, True,
                          cursorUdf[_cursor].currUid, self, TGIS_DimensionType.XYZM ) ;
             FreeObject( shp ) ;
             cursorUdf[_cursor].currShape := cursorUdf[_cursor].currPolygon ;
           end ;
        else
           begin
             cursorUdf[_cursor].currShape := nil ;
           end ;
      end ;

      cursorUdf[_cursor].currShape := getEdited( cursorUdf[_cursor].currShape ) ;
    end;

    inc( cursorUdf[_cursor].currUid ) ;
  end;

  procedure TGIS_LayerVectorUDF.setUp ;
  var
    i      : Integer ;
    val    : Variant ;
    {$IFDEF OXYGENE}
      args  : TGIS_GetShapeFieldEventArgs ;
      args2 : TGIS_LayerEofEventArgs ;
    {$ENDIF}
  begin
    FUseRTree := False ;
    lastUid   := -1 ;

    inherited ;

    if assigned( FOnGetLayerStructure ) then
      {$IFDEF OXYGENE}
        FOnGetLayerStructure( self, EventArgs.Create ) ;
      {$ELSE}
        FOnGetLayerStructure( self ) ;
      {$ENDIF}

    if InMemory then begin
      FSubType := FSubType + [ TGIS_LayerSubType.Persistent,
                               TGIS_LayerSubType.InMemory
                              ] ;

      cursorUdf[0].currShape := nil ;
      cursorUdf[0].isEof     := False ;
      cursorUdf[0].currUid   := 1 ;
      lastUid   := 0 ;

      if assigned( FOnLayerMoveFirst ) then begin
        {$IFDEF OXYGENE}
          FOnLayerMoveFirst( self,
                             TGIS_LayerMoveFirstEventArgs.Create(
                                0, GisWholeWorld, '', nil, '', True
                             )
                           ) ;

        {$ELSE}
          FOnLayerMoveFirst( self, 0, GisWholeWorld,
                             '', nil, '', True
                           ) ;
        {$ENDIF}
      end ;

      while not cursorUdf[0].isEof do begin
        cursorUdf[0].currShape := nil  ;
        if assigned( FOnLayerMoveNext ) then
          {$IFDEF OXYGENE}
            FOnLayerMoveNext( self, TGIS_LayerMoveNextEventArgs.Create( 0 ) ) ;
          {$ELSE}
            FOnLayerMoveNext( self, 0 ) ;
          {$ENDIF}

        readShape(0) ;

        if cursorUdf[0].currShape = nil then begin
          cursorUdf[0].isEof := True ;
          break ;
        end ;

        for i := 0 to Fields.Count - 1 do  begin
          if assigned( FOnGetShapeField ) then begin
            {$IFDEF OXYGENE}
              args := TGIS_GetShapeFieldEventArgs.Create(
                        TGIS_FieldInfo( Fields[ i ] ).Name,
                        cursorUdf[0].currShape.Uid, 0
                      ) ;
              try
                FOnGetShapeField( self, args ) ;
              finally
                val := args.Value ;
                FreeObject( args ) ;
              end;
            {$ELSE}
              FOnGetShapeField( self, TGIS_FieldInfo( Fields[ i ] ).Name,
                                cursorUdf[0].currShape.Uid, 0, val )
            {$ENDIF}
          end else
            val := Unassigned ;

          cursorUdf[0].currShape.SetField( TGIS_FieldInfo( Fields[ i ] ).Name, val );
        end;

        if assigned( FOnLayerEof ) then begin
          {$IFDEF OXYGENE}
            args2 := TGIS_LayerEofEventArgs.Create( 0 ) ;
            try
              FOnLayerEof( self, args2 ) ;
            finally
              cursorUdf[0].isEof := args2.Eof ;
              FreeObject( args2 ) ;
            end;
          {$ELSE}
            FOnLayerEof( self, 0, cursorUdf[0].isEof )
          {$ENDIF}
        end;
      end;
    end
    else begin
      FSubType := FSubType + [ TGIS_LayerSubType.Persistent ] ;

      for i := 0 to Fields.Count - 1 do
        TGIS_FieldInfo( Fields[ i ] ).Saved := True ;
    end;

    FFileInfo := 'TatukGIS User Defined Vector layer' ;
  end;

  procedure TGIS_LayerVectorUDF.cursorFirst(
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
      cursorUdf[_cursor].currShape := nil ;

      if GisIsNoWorld( _extent ) then
        exit ;

      cursorUdf[_cursor].isEof     := False ;
      cursorUdf[_cursor].currUid   := 1 ;

      if InMemory then begin
        inherited cursorFirst( _cursor, _viewerCS,
                               _extent, _query, _shape, _de9im, _skipDeleted
                             ) ;
      end
      else begin
        inherited cursorFirstInternal(
                    _cursor, _viewerCS,
                    _extent, _query, _shape, _de9im, _skipDeleted
                  ) ;

        if assigned( FOnLayerMoveFirst ) then begin
          {$IFDEF OXYGENE}
            FOnLayerMoveFirst( self,
                               TGIS_LayerMoveFirstEventArgs.Create(
                                  _cursor, _extent, _query, _shape,
                                  _de9im, _skipDeleted
                               )
                             ) ;

          {$ELSE}
            FOnLayerMoveFirst( self, _cursor, _extent,
                               _query, _shape, _de9im, _skipDeleted
                             ) ;
          {$ENDIF}
        end;

        cursorNext(_cursor) ;
      end;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerVectorUDF.cursorNext(
    const _cursor : Integer
  ) ;
  var
    ihglass : Integer ;
  begin
    lockThread ;
    try
      try
        if InMemory then
          inherited
        else begin
          if assigned( FOnLayerMoveNext ) then
            {$IFDEF OXYGENE}
              FOnLayerMoveNext( self,
                                TGIS_LayerMoveNextEventArgs.Create( _cursor )
                              ) ;
            {$ELSE}
              FOnLayerMoveNext( self, _cursor ) ;
            {$ENDIF}

          ihglass := 0 ;
          while not cursorEof(_cursor) do begin
            inc( ihglass ) ;
            if ihglass mod GIS_PROGRESS_TRESHOLD = 0 then begin
              if HourglassShake then begin
                cursorUdf[_cursor].currShape := nil ;
                break ;
              end;
            end ;

            cursorUdf[_cursor].currShape := nil   ;

            readShape(_cursor) ;

            if cursorUdf[_cursor].currShape = nil then begin
              cursorUdf[_cursor].isEof := True ;
              exit ;
            end ;

           if cursorState[_cursor].curSkipDeleted and
              cursorUdf[_cursor].currShape.IsDeleted then
           begin
             continue ;
           end ;

          if ( cursorUdf[_cursor].currShape.IsEditable
               or assigned( cursorState[_cursor].curShape )
               or ( assigned( Viewer )
                    and Viewer.Ref.InPaint
                    and ( cursorUdf[_cursor].currShape.SmartSize <> 0 )
                  )
             )
             and ( not isInScope( cursorUdf[_cursor].currShape, _cursor ) )
          then
            continue
          else
            exit ;

          end ;
        end;
      except
        cursorUdf[_cursor].isEof := True ;
      end ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerVectorUDF.cursorEof(
    const _cursor : Integer
  ) : Boolean ;
  {$IFDEF OXYGENE}
    var
      args : TGIS_LayerEofEventArgs ;
  {$ENDIF}
  begin
    if InMemory then
      Result := inherited cursorEof(_cursor)
    else begin
      if assigned( FOnLayerEof ) then begin
        {$IFDEF OXYGENE}
          args := TGIS_LayerEofEventArgs.Create( _cursor ) ;
          try
            FOnLayerEof( self, args ) ;
          finally
            Result := args.Eof or cursorUdf[_cursor].isEof ;
            FreeObject( args ) ;
          end;
        {$ELSE}
          FOnLayerEof( self, _cursor, Result ) ;
          Result := Result or cursorUdf[_cursor].isEof ;
        {$ENDIF}
      end
      else
        Result := cursorUdf[_cursor].isEof ;
    end;
  end ;

  function TGIS_LayerVectorUDF.cursorShape(
    const _cursor : Integer
  ) : TGIS_Shape ;
  begin
    if InMemory then
      Result := inherited cursorShape(_cursor)
    else begin
      if assigned( cursorUdf[_cursor].currShape ) then
         Result := cursorUdf[_cursor].currShape
      else
        Result := inherited cursorShape(_cursor) ;
    end;
  end ;

  function TGIS_LayerVectorUDF.GetShape(
    const _uid     : TGIS_Uid ;
    const _cursor  : Integer
  ) : TGIS_Shape ;
  {$IFDEF OXYGENE}
    var
      args : TGIS_GetShapeObjectEventArgs ;
  {$ENDIF}
  begin
    lockThread ;
    try
      if InMemory then
        Result := inherited GetShape( _uid, _cursor )
      else begin
        Result := nil ;

        if _uid <= 0 then exit ;

        if assigned( FOnGetShapeObject ) then begin
          {$IFDEF OXYGENE}
            args := TGIS_GetShapeObjectEventArgs.Create( _uid, _cursor ) ;
            try
              FOnGetShapeObject( self, args ) ;
            finally
              Result := args.Shape ;
              FreeObject( args ) ;
            end;
          {$ELSE}
            FOnGetShapeObject( self, _uid, _cursor, Result  ) ;
          {$ENDIF}
        end;

        if Result <> nil then exit ;

        // if it is in edited list
        Result := inherited GetShape( _uid, _cursor ) ;
        if Result <> nil then exit ;

        // is it a current shape
        if ( cursorUdf[_cursor].currShape    <> nil ) and
          ( cursorUdf[_cursor].currShape.Uid = _uid ) then begin
          Result := cursorUdf[_cursor].currShape ;
          exit ;
        end ;

        inherited cursorStateSave( _cursor ) ;
        try
          cursorFirst( _cursor, False,
                       GisWholeWorld, '', nil, '', True
                     ) ;

          while not cursorEof(_cursor) do begin
            if cursorShape(_cursor).Uid = _uid then begin
              Result := cursorShape(_cursor) ;
              exit ;
            end ;
            cursorNext(_cursor) ;
          end ;
        finally
          inherited cursorStateRestore( _cursor ) ;
        end;
      end;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerVectorUDF.GetLastUid
    : TGIS_Uid ;
  var
    shp       : TGIS_Shape ;
    old_scope : String     ;
    {$IFDEF OXYGENE}
      args : TGIS_GetLayerLastShapeUidEventArgs ;
    {$ENDIF}
  begin
    lockThread ;
    try
      if assigned( FOnGetLayerLastShapeUid ) then begin
      {$IFDEF OXYGENE}
        args := TGIS_GetLayerLastShapeUidEventArgs.Create( lastUid ) ;
        try
          FOnGetLayerLastShapeUid( self, args )
        finally
          lastUid := args.Uid ;
          FreeObject( args ) ;
        end;
      {$ELSE}
        FOnGetLayerLastShapeUid( self, lastUid )
      {$ENDIF}
      end else begin
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
        finally
          Scope := old_scope ;
        end;
      end;
      Result := lastUid ;
    finally
      unlockThread ;
    end ;
  end ;

  function TGIS_LayerVectorUDF.getFieldInternal(
    const _uid    : TGIS_Uid;
    const _name   : String ;
    const _cursor : Integer
  ) : Variant ;
  {$IFDEF OXYGENE}
    var
      args : TGIS_GetShapeFieldEventArgs ;
  {$ENDIF}
  begin
    lockThread ;
    try
      if InMemory then
        Result := inherited getFieldInternal( _uid, _name, _cursor )
      else begin
        if assigned( FOnGetShapeField ) then begin
          {$IFDEF OXYGENE}
            args := TGIS_GetShapeFieldEventArgs.Create(
                      _name, _uid, _cursor
                    ) ;
            try
              FOnGetShapeField( self, args ) ;
            finally
              Result := args.Value ;
              FreeObject( args ) ;
            end;
          {$ELSE}
            FOnGetShapeField( self, _name, _uid, _cursor, Result )
          {$ENDIF}
        end else
          Result := Unassigned ;
      end;
    finally
      unlockThread ;
    end ;
  end ;

  procedure TGIS_LayerVectorUDF.RecalcExtent;
  var
    ext    : TGIS_Extent       ;
    {$IFDEF OXYGENE}
      args : TGIS_GetLayerExtentEventArgs ;
    {$ENDIF}
  begin
    ext := GisNoWorld ;
    if Items.Count > 0 then
      inherited
    else begin
      if assigned( FOnGetLayerExtent ) then begin
        {$IFDEF OXYGENE}
          args := TGIS_GetLayerExtentEventArgs.Create( ext ) ;
          try
            FOnGetLayerExtent( self, args ) ;
          finally
            ext := args.Extent ;
            FreeObject( args ) ;
          end;
        {$ELSE}
          FOnGetLayerExtent( self, ext ) ;
        {$ENDIF}
        if GisIsSameExtent( ext, GisNoWorld ) then
          inherited
        else
          Extent := ext ;
      end
      else
        inherited
    end;
  end;

//==================================== END =====================================
end.

