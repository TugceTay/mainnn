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
  GCP (Ground Control Point) operations.

  This unit was partially based on CRS.C:

   CRS.C - Center for Remote Sensing rectification routines

   Written By: Brian J. Buckley

           At: The Center for Remote Sensing
               Michigan State University
               302 Berkey Hall
               East Lansing, MI  48824
               (517)353-7195

   Written: 12/19/91

   Last Update: 12/26/91 Brian J. Buckley
   Last Update:  1/24/92 Brian J. Buckley
     Added printout of trnfile. Triggered by BDEBUG.
   Last Update:  1/27/92 Brian J. Buckley
     Fixed bug so that only the active control points were used.

   Copyright (c) 1992, Michigan State University

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
  unit GisTransform ;
  {$HPPEMIT '#pragma link "GisTransform"'}
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

{$INCLUDE GisInclude.inc}

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.SysUtils,
    System.StrUtils,
    System.Classes,

    GisRtl,
    GisTypes ;
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
  /// <summary>
  ///   Clear GCP
  /// </summary>
  ERROR_CLEAR_GCP           =   0 ;
  /// <summary>
  ///   Unknown internal problems
  /// </summary>
  ERROR_INTERNAL_GCP        =   1 ;
  /// <summary>
  ///   Transformation can not be solved
  /// </summary>
  ERROR_UNSOLVABLE_GCP      = 100 ; 
  /// <summary>
  ///   Not enough points for transformation
  /// </summary>
  ERROR_NOTENOUGHPOINTS_GCP = 101 ; 
  /// <summary>
  ///   Transformation has not been prepared.
  /// </summary>
  ERROR_NOTPREPARED_GCP     = 102 ; 

type

  /// <summary>
  ///   Polynomial order.
  /// </summary>
  TGIS_PolynomialOrder = {$IFDEF OXYGENE} public {$ENDIF} (

      /// <summary>
      ///   First polynomial order; at least 3 GCPs required.
      /// </summary>
      First,

      /// <summary>
      ///   Second polynomial order; at least 6 GCPs required.
      /// </summary>
      Second,

      /// <summary>
      ///   Third polynomial order; at least 10 GCPs required.
      /// </summary>
      Third
  ) ;

  {$IFDEF GIS_DK10VCL_COMPATIBILITY}
    const
      gisPolynomialOrderFirst  = TGIS_PolynomialOrder.First  ;
      gisPolynomialOrderSecond = TGIS_PolynomialOrder.Second ;
      gisPolynomialOrderThird  = TGIS_PolynomialOrder.Third  ;
  {$ENDIF}

type

  /// <summary>
  ///   Base class for layer transformations. A sample class that can inherits
  ///   form TGIS_Transform is polynomial transformation which can transform
  ///   layer according to GCP points.
  /// </summary>
  TGIS_Transform = {$IFDEF OXYGENE} public abstract{$ENDIF} class( TGIS_Object )
    protected
      /// <summary>
      ///   Is active.
      /// </summary>
      bActive : Boolean ;
      /// <summary>
      ///   Is modified.
      /// </summary>
      bModified : Boolean ;
      /// <summary>
      ///   Cutting polygon string.
      /// </summary>
      sCuttingPolygon : String ;
    public

      /// <summary>
      ///   Create instance.
      /// </summary>
      constructor Create  ;
    public
      /// <summary>
      ///   Transform point.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be transformed
      /// </param>
      /// <returns>
      ///   transformed point
      /// </returns>
      function    Transform        ( const _ptg   : TGIS_Point
                                   ) : TGIS_Point ; virtual; abstract;
      /// <summary>
      ///   Untransform point.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be untransformed
      /// </param>
      /// <returns>
      ///   untransformed point
      /// </returns>
      function    Untransform      ( const _ptg   : TGIS_Point
                                   ) : TGIS_Point ; virtual; abstract;
      /// <summary>
      ///   Transform point 3D by reference.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be transformed
      /// </param>
      procedure   Transform3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _ptg     : TGIS_Point3D
                                   ) ; virtual; abstract;
      /// <summary>
      ///   Untransform point 3D by reference.
      /// </summary>
      /// <param name="_ptg">
      ///   point to be transformed
      /// </param>
      procedure   Untransform3D_Ref( {$IFNDEF JAVA} var {$ENDIF} _ptg     : TGIS_Point3D
                                   ) ; virtual; abstract;

      /// <summary>
      ///   Load transformation definitions from the file. By default
      ///   TGIS_Layer save/load transformation to/from file named:
      ///   file_name.ext.trn
      /// </summary>
      /// <param name="_path">
      ///   - path to the file
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Default implementation do nothing
      ///    </note>
      /// </remarks>
      procedure   LoadFromFile     ( const _path  : String
                                   ) ; virtual;

      /// <summary>
      ///   Save transformation definitions to the file. By default
      ///   TGIS_Layer save/load transformation to/from file named:
      ///   file_name.ext.trn
      /// </summary>
      /// <param name="_path">
      ///   - path to the file
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Default implementation do nothing
      ///    </note>
      /// </remarks>
      procedure   SaveToFile       ( const _path  : String
                                   ) ; virtual;

      /// <summary>
      ///   Check if transformation was modified by editing.
      /// </summary>
      /// <returns>
      ///   true if transformation was modified
      /// </returns>
      function    MustSave         : Boolean ; virtual;
    public

      /// <summary>
      ///   WKT of cutting polygon. Polygon vertices should be in untransformed
      ///   units.
      /// </summary>
      property    CuttingPolygon   : String
                                     read  sCuttingPolygon
                                     write sCuttingPolygon ;
      /// <summary>
      ///   True if transformation is active.
      /// </summary>
      property    Active           : Boolean
                                     read  bActive
                                     write bActive ;
  end;

  /// <summary>
  ///   Base class for layer transformations based on GCP (Ground Control
  ///   Points). A sample class that can inherits from TGIS_TransformGCP is a
  ///   polynomial transformation.
  /// </summary>
  TGIS_TransformGCP = {$IFDEF OXYGENE} public abstract {$ENDIF}
                      class( TGIS_Transform )
    protected
      /// <summary>
      ///   Error code.
      /// </summary>
      FError       : Integer ;
      /// <summary>
      ///   Root mean square value.
      /// </summary>
      FRMS         : Double ;
      /// <summary>
      ///   Is prepared.
      /// </summary>
      bPrepared    : Boolean ;
      /// <summary>
      ///   List control points.
      /// </summary>
      lstGCPPoints : TGIS_ObjectList ; // list of control points
    protected

      function  fget_PointsCount : Integer ; virtual;
      function  fget_PointsSrc   ( const _index : Integer
                                 ) : TGIS_Point ; virtual;
      procedure fset_PointsSrc   ( const _index : Integer ;
                                   const _value : TGIS_Point
                                 ) ; virtual;
      function  fget_PointsDst   ( const _index : Integer
                                 ) : TGIS_Point; virtual;
      procedure fset_PointsDst   ( const _index : Integer ;
                                   const _value : TGIS_Point
                                 ) ; virtual;
      function  fget_PointsActive( const _index : Integer
                                 ) : Boolean ; virtual;
      procedure fset_PointsActive( const _index : Integer ;
                                   const _value : Boolean
                                 ) ; virtual;
      function  fget_PointsUid   ( const _index : Integer
                                 ) : Integer ; virtual;
      procedure fset_PointsUid   ( const _index : Integer ;
                                   const _value : Integer
                                 ) ; virtual;
    protected

      /// <summary>
      ///   Destroy instance.
      /// </summary>
      procedure doDestroy ; override;
    public

      /// <inheritdoc/>
      constructor Create  ;
    public

      /// <summary>
      ///   Clear all GCP points.
      /// </summary>
      procedure Clear            ; virtual;

      /// <summary>
      ///   Add Ground Control Point (GCP) will be used by Prepare method to
      ///   compute transformation matrix. Added point is marked as active.
      /// </summary>
      /// <param name="_src">
      ///   source point (in a source coordinates)
      /// </param>
      /// <param name="_dst">
      ///   destination point (matching point in a destination coordinates)
      /// </param>
      /// <param name="_uid">
      ///   point identifier (reserved for future use)
      /// </param>
      procedure AddPoint         ( const _src     : TGIS_Point ;
                                   const _dst     : TGIS_Point ;
                                   const _uid     : Integer
                                 ) ; overload;

      /// <summary>
      ///   Add Ground Control Point (GCP) will be used by Prepare method to
      ///   compute transformation matrix.
      /// </summary>
      /// <param name="_src">
      ///   source point (in a source coordinates)
      /// </param>
      /// <param name="_dst">
      ///   destination point (matching point in a destination coordinates)
      /// </param>
      /// <param name="_uid">
      ///   point identifier (reserved for future use)
      /// </param>
      /// <param name="_active">
      ///   is point active
      /// </param>
      procedure AddPoint         ( const _src     : TGIS_Point ;
                                   const _dst     : TGIS_Point ;
                                   const _uid     : Integer    ;
                                   const _active  : Boolean
                                 ) ; overload;
    public

      /// <summary>
      ///   Error number.
      /// </summary>
      /// <remarks>
      ///   <list type="table">
      ///     <listheader>
      ///       <term>Value</term>
      ///       <description>Description</description>
      ///     </listheader>
      ///     <item>
      ///       <term>CLEAR (0)</term>
      ///       <description>no errors</description>
      ///     </item>
      ///     <item>
      ///       <term>INTERNAL (1)</term>
      ///       <description>unknown internal problems</description>
      ///     </item>
      ///     <item>
      ///       <term>UNSOLVABLE (100)</term>
      ///       <description>transformation can not be solved</description>
      ///     </item>
      ///     <item>
      ///       <term>NOTENOUGHPOINTS (101)</term>
      ///       <description>not enough point for transformation</description>
      ///     </item>
      ///     <item>
      ///       <term>NOTPREPARED (102)</term>
      ///       <description>transformation has not been prepared</description>
      ///     </item>
      ///   </list>
      /// </remarks>
      property Error          : Integer
                                read FError ;

      /// <summary>
      ///   RMS error for current transformation.
      /// </summary>
      property RMS            : Double
                                read FRMS ;
      /// <summary>
      ///   Number of points.
      /// </summary>
      /// <value>
      ///   points count
      /// </value>
      property PointsCount    : Integer read fget_PointsCount ;
      /// <summary>
      ///   Source point of transformation.
      /// </summary>
      /// <param name="_index">
      /// index of the point</param>
      /// <value>
      ///   points src
      /// </value>
      property PointsSrc      [ const _index : Integer ]
                              : TGIS_Point
                                read  fget_PointsSrc
                                write fset_PointsSrc ;
      /// <summary>
      ///   End point of the transformation.
      /// </summary>
      /// <param name="_index">
      ///   index of the point
      /// </param>
      /// <value>
      ///   points dst
      /// </value>
      property PointsDst      [ const _index : Integer ]
                              : TGIS_Point
                                read  fget_PointsDst
                                write fset_PointsDst ;
      /// <summary>
      ///   Gives us information if point is active.
      /// </summary>
      /// <param name="_index">
      ///   index of the point
      /// </param>
      /// <value>
      ///   points active
      /// </value>
      property PointsActive   [ const _index : Integer ]
                              : Boolean
                                read  fget_PointsActive
                                write fset_PointsActive ;
      /// <summary>
      ///  Gives us Uid of the point.
      /// </summary>
      /// <param name="_index">
      ///   index of the point
      /// </param>
      /// <value>
      ///   points uid
      /// </value>
      property PointsUid      [ const _index : Integer ]
                              : Integer
                                read  fget_PointsUid
                                write fset_PointsUid ;
  end;

  /// <summary>
  ///   Ground control point (GCP) computations involving polynomial.
  /// </summary>
  /// <remarks>
  ///   To compute any coordinate within the space you first must:
  ///   <list type="bullet">
  ///     <item>
  ///       add series of point of corresponding source-destination
  ///       coordinates
  ///     </item>
  ///     <item>
  ///       call Prepare() to compute polynomials
  ///     </item>
  ///     <item>
  ///       call Transform() &amp;or Untransform() to compute
  ///       forward/reverse transformation <br />
  ///     </item>
  ///   </list>
  /// </remarks>
  TGIS_TransformPolynomial = {$IFDEF OXYGENE} public {$ENDIF}
                             class( TGIS_TransformGCP )
    private
      orderVal   : TGIS_PolynomialOrder ;
      arFwd      : array[0..10] of TGIS_Point ;
      arRev      : array[0..10] of TGIS_Point ;
    protected

      /// <summary>
      ///   Destroy instance.
      /// </summary>
      procedure doDestroy ; override;
    public

      /// <inheritdoc/>
      constructor Create  ;
    public

      /// <summary>
      ///   Prepare transformation based on current set of GCP points.
      /// </summary>
      /// <param name="_order">
      ///   polynomial order; first order requires at least 3 point; second order requires
      ///   at least 6 points; third order requires at least 10 points;
      /// </param>
      /// <returns>
      ///   True if prepared
      /// </returns>
      function  Prepare          ( const _order   : TGIS_PolynomialOrder
                                 ) : Boolean ;

      /// <inheritdoc/>
      function  Transform        ( const _ptg     : TGIS_Point
                                 ) : TGIS_Point ; override;

      /// <inheritdoc/>
      function  Untransform      ( const _ptg     : TGIS_Point
                                 ) : TGIS_Point ; override;

      /// <inheritdoc/>
      procedure Transform3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF}   _ptg     : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure Untransform3D_Ref( {$IFNDEF JAVA} var {$ENDIF}   _ptg     : TGIS_Point3D
                                 ) ; override;

      /// <inheritdoc/>
      procedure LoadFromFile     ( const _path    : String
                                 ) ; override;

      /// <inheritdoc/>
      procedure SaveToFile       ( const _path    : String
                                 ) ; override;
    public

      /// <summary>
      ///   Polynomial order.
      /// </summary>
      property PolynomialOrder : TGIS_PolynomialOrder read orderVal ;
  end ;

  /// <summary>
  ///   Custom transformation.
  /// </summary>
  /// <param name="_forward">
  ///   if true the transformation is forward; if else transformation is reverse
  /// </param>
  /// <param name="_ptg">
  ///   point coordinate to be transformed
  /// </param>
  TGIS_TransformCustomProc = {$IFDEF OXYGENE} public {$ENDIF}
                            procedure ( const _forward : Boolean      ;
                                        {$IFNDEF JAVA} var {$ENDIF}   _ptg     : TGIS_Point3D
                                       ) of object ;

  /// <summary>
  ///   Custom transform.
  /// </summary>
  TGIS_TransformCustom = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_Transform )
    private
      FEventProc : TGIS_TransformCustomProc ;
    public

      /// <summary>
      ///   Perform forward point transformation.
      /// </summary>
      /// <param name="_ptg">
      ///   source point
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Call procedure assigned EventProc to perform forward/reverse
      ///    transformation
      ///    </note>
      /// </remarks>
      function    Transform        ( const _ptg   : TGIS_Point
                                   ) : TGIS_Point ; override;

      /// <summary>
      ///   Perform reverse point transformation.
      /// </summary>
      /// <param name="_ptg">
      ///   source point
      /// </param>
      /// <remarks>
      ///   <note type="note">
      ///    Call procedure assigned EventProc to perform forwards/reverse
      ///    transformation
      ///    </note>
      /// </remarks>
      function    Untransform      ( const _ptg   : TGIS_Point
                                   ) : TGIS_Point ; override;

      /// <inheritdoc/>
      procedure   Transform3D_Ref  ( {$IFNDEF JAVA} var {$ENDIF} _ptg     : TGIS_Point3D
                                   ) ; override;

      /// <inheritdoc/>
      procedure   Untransform3D_Ref( {$IFNDEF JAVA} var {$ENDIF} _ptg     : TGIS_Point3D
                                   ) ; override;

   published //events
      /// <summary>
      ///   Event for custom transformation.
      /// </summary>
      /// <value>
      ///   event proc
      /// </value>
      /// <event/>
      property CustomTransformEvent : TGIS_TransformCustomProc 
                                      read  FEventProc
                                      write FEventProc ;
  end;

  /// <summary>
  ///   Construct proper transformation object base on parameters file.
  /// </summary>
  /// <param name="_path">
  ///   path to transformation parameters file
  /// </param>
  /// <returns>
  ///   transformation
  /// </returns>
  function GisTransformFactory( const _path : String ) :  TGIS_Transform ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisClasses,
    GisResource,
    GisConfig,
    GisConfigIni,
    GisParams ;
{$ENDIF}

type
  T_matrixGCP = array of array of Double ;
  T_vectorGCP = array of Double ;

  { Ground control point (GCP) structure.
  }
  T_pointGCP = class
    public
        Src     : TGIS_Point ;  // Point in a source coordinates.
        Dst     : TGIS_Point ;  // Point in a destination coordinates.
        Uid     : Integer    ;  // Unique identifier.
        Active  : Boolean    ;  // Is point active?
  end;

//==============================================================================
// TGIS_Transform
//==============================================================================

  constructor TGIS_Transform.Create  ;
  begin
    inherited ;

    bModified := False ;
  end ;

  function TGIS_Transform.MustSave
    : Boolean ;
  begin
    Result := bModified ;
  end;

  procedure TGIS_Transform.LoadFromFile(
    const _path : String
  ) ;
  begin
    // do nothing
  end ;

  procedure TGIS_Transform.SaveToFile(
    const _path : String
  ) ;
  begin
    // do nothing
  end ;

//==============================================================================
// TGIS_TransformGCP
//==============================================================================

  constructor TGIS_TransformGCP.Create  ;
  begin
    inherited ;

    lstGCPPoints := TGIS_ObjectList.Create ;

    bPrepared := False ;
    bModified := True  ;
  end ;

  procedure TGIS_TransformGCP.doDestroy ;
  begin
    Clear ;
    FreeObject( lstGCPPoints ) ;

    inherited ;
  end ;

  function TGIS_TransformGCP.fget_PointsCount
    : Integer ;
  begin
    Result := lstGCPPoints.Count ;
  end;

  function TGIS_TransformGCP.fget_PointsSrc(
    const _index : Integer
  ) : TGIS_Point ;
  var
    gcp : T_pointGCP ;
  begin
    gcp := T_pointGCP( lstGCPPoints[_index] ) ;
    Result := _TGIS_Point(gcp.Src) ;
  end ;

  procedure TGIS_TransformGCP.fset_PointsSrc(
    const _index : Integer ;
    const _value : TGIS_Point
  ) ;
  var
    gcp : T_pointGCP ;
  begin
    gcp := T_pointGCP( lstGCPPoints[_index] ) ;
    gcp.Src := _TGIS_Point(_value) ;

    bPrepared := False ;
    bModified := True  ;
  end ;

  function TGIS_TransformGCP.fget_PointsDst(
    const _index : Integer
  ) : TGIS_Point ;
  var
    gcp : T_pointGCP ;
  begin
    gcp := T_pointGCP( lstGCPPoints[_index] ) ;
    Result := _TGIS_Point(gcp.Dst) ;
  end;

  procedure TGIS_TransformGCP.fset_PointsDst(
    const _index : Integer ;
    const _value : TGIS_Point
  ) ;
  var
    gcp : T_pointGCP ;
  begin
    gcp := T_pointGCP( lstGCPPoints[_index] ) ;
    gcp.Dst := _TGIS_Point(_value) ;

    bPrepared := False ;
    bModified := True  ;
  end ;

  function TGIS_TransformGCP.fget_PointsActive(
    const _index : Integer
  ) : Boolean ;
  var
    gcp : T_pointGCP ;
  begin
    gcp := T_pointGCP( lstGCPPoints[_index] ) ;
    Result := gcp.Active ;
  end;

  procedure TGIS_TransformGCP.fset_PointsActive(
    const _index : Integer ;
    const _value : Boolean
  ) ;
  var
    gcp : T_pointGCP ;
  begin
    gcp := T_pointGCP( lstGCPPoints[_index] ) ;
    gcp.Active := _value ;

    bPrepared := False ;
    bModified := True  ;
  end;

  function TGIS_TransformGCP.fget_PointsUid(
    const _index : Integer
  ) : Integer ;
  var
    gcp : T_pointGCP ;
  begin
    gcp := T_pointGCP( lstGCPPoints[_index] ) ;
    Result := gcp.Uid ;
  end;

  procedure TGIS_TransformGCP.fset_PointsUid(
    const _index : Integer ;
    const _value : Integer
  ) ;
  var
    gcp : T_pointGCP ;
  begin
    gcp := T_pointGCP( lstGCPPoints[_index] ) ;
    gcp.Uid := _value ;

    bPrepared := False ;
    bModified := True  ;
  end;

  procedure TGIS_TransformGCP.Clear ;
  begin
    lstGCPPoints.Clear ;

    bPrepared := False ;
  end ;

  procedure TGIS_TransformGCP.AddPoint(
    const _src : TGIS_Point ;
    const _dst : TGIS_Point ;
    const _uid : Integer
  ) ;
  begin
    AddPoint( _src, _dst, _uid, True ) ;
  end ;

  procedure TGIS_TransformGCP.AddPoint(
    const _src    : TGIS_Point ;
    const _dst    : TGIS_Point ;
    const _uid    : Integer    ;
    const _active : Boolean
  ) ;
  var
    gcp : T_pointGCP ;
  begin
    gcp := T_pointGCP.Create ;

    gcp.Src    := _TGIS_Point(_src) ;
    gcp.Dst    := _TGIS_Point(_dst) ;
    gcp.Uid    := _uid ;
    gcp.Active := _active ;

    lstGCPPoints.Add( gcp ) ;

    bPrepared := False ;
    bModified := True  ;
  end ;

//==============================================================================
// TGIS_TransformPolynomial
//==============================================================================

  constructor TGIS_TransformPolynomial.Create  ;
  begin
    inherited ;
    orderVal := TGIS_PolynomialOrder.First ;
    {$IFDEF JAVA}
      for i : Integer := 0 to 10 do begin
        arFwd[i] := new TGIS_Point ;
        arRev[i] := new TGIS_Point ;
      end ;
    {$ENDIF}
  end ;

  procedure TGIS_TransformPolynomial.doDestroy ;
  begin
    inherited ;
  end ;

  function TGIS_TransformPolynomial.Prepare(
    const _order : TGIS_PolynomialOrder
  ) : Boolean ;
  var
    mtrx      : T_matrixGCP ;
    mtrx_size : Integer  ;
    a         : T_vectorGCP ;
    b         : T_vectorGCP ;
    reverse   : Boolean  ;
    exact     : Boolean  ;

    function get_gcp(
      const _idx : Integer
    ) : T_pointGCP ;
    begin
      Result := T_pointGCP( lstGCPPoints[_idx] ) ;
    end ;

    { Calculate RMS value (in destination coordinates)
    }
    function calculate_rms
      : Double ;
    var
      i     : Integer    ;
      fwd   : TGIS_Point ;
      vrms  : Double     ;
      tmp   : Double     ;
      cnt   : Integer    ;
    begin
      vrms := 0 ;
      cnt  := 0 ;
      for i:=0 to lstGCPPoints.Count - 1 do begin
        if not get_gcp(i).Active then
          continue ;

        inc( cnt ) ;

        fwd := Transform( get_gcp(i).Src ) ;
        tmp := Sqr( fwd.X - get_gcp(i).Dst.X ) +
               Sqr( fwd.Y - get_gcp(i).Dst.Y ) ;
        vrms := vrms + tmp ;
      end ;
      Result := Sqrt( vrms / cnt ) ;
    end ;

    { CALCULATE THE X/Y TERM BASED ON THE TERM NUMBER

      ORDER\TERM   1    2    3    4    5    6    7    8    9   10
      1        e0n0 e1n0 e0n1
      2        e0n0 e1n0 e0n1 e2n0 e1n1 e0n2
      3        e0n0 e1n0 e0n1 e2n0 e1n1 e0n2 e3n0 e2n1 e1n2 e0n3
    }
    function term( const _term : Integer; const e, n : Double ) : Double ;
    begin
     case _term of
        1  : Result := 1      ;
        2  : Result := e      ;
        3  : Result := n      ;
        4  : Result := e*e    ;
        5  : Result := e*n    ;
        6  : Result := n*n    ;
        7  : Result := e*e*e  ;
        8  : Result := e*e*n  ;
        9  : Result := e*n*n  ;
        10 : Result := n*n*n  ;
        else Result := 0      ;
     end ;
    end ;

    { SOLVE FOR THE 'E' AND 'N' COEFFICIENTS BY USING A SOMEWHAT MODIFIED
      GAUSSIAN ELIMINATION METHOD.

      | M11 M12 ... M1n | | E0   |   | a0   |
      | M21 M22 ... M2n | | E1   | = | a1   |
      |  .   .   .   .  | | .    |   | .    |
      | Mn1 Mn2 ... Mnn | | En-1 |   | an-1 |

      and

      | M11 M12 ... M1n | | N0   |   | b0   |
      | M21 M22 ... M2n | | N1   | = | b1   |
      |  .   .   .   .  | | .    |   | .    |
      | Mn1 Mn2 ... Mnn | | Nn-1 |   | bn-1 |
    }
    procedure solvemat ;
    var
      i, j, i2, j2, imark : Integer ;
      factor, temp        : Double  ;
      pivot               : Double  ; // ACTUAL VALUE OF THE LARGEST PIVOT CANDIDATE
    begin
      for i := 1 to mtrx_size do begin
        j := i ;

        // find row with largest magnitude value for pivot value
        pivot := mtrx[i-1][j-1] ;
        imark := i;
        for i2 := i+1 to mtrx_size do begin
          temp := Abs( mtrx[i2-1][j-1] ) ;
          if temp > Abs(pivot) then begin
            pivot := mtrx[i2-1][j-1] ;
            imark := i2         ;
          end ;
        end ;

        // if the pivot is very small then the points are nearly co-linear
        // co-linear points result in an undefined matrix, and nearly
        // co-linear points results in a solution with rounding error
        //pivot := 0 ;
        if pivot = 0 then begin
          FError := ERROR_UNSOLVABLE_GCP ;
          Abort ;
        end;

        // if row with highest pivot is not the current row, switch them
        if imark <> i then begin
          for j2 := 1 to mtrx_size do begin
            temp                := mtrx[imark-1][j2-1] ;
            mtrx[imark-1][j2-1] := mtrx[i-1][j2-1]     ;
            mtrx[i-1][j2-1]     := temp                ;
          end ;
          temp       := a[imark-1] ;
          a[imark-1] := a[i-1]     ;
          a[i-1]     := temp       ;

          temp       := b[imark-1] ;
          b[imark-1] := b[i-1]     ;
          b[i-1]     := temp       ;
        end ;

        // compute zeros above and below the pivot, and compute
        // values for the rest of the row as well */
        for i2 := 1 to mtrx_size do begin
          if i2 <> i then begin
            factor := mtrx[i2-1][j-1] / pivot ;
            for j2 := j to mtrx_size do begin
              mtrx[i2-1][j2-1] := mtrx[i2-1][j2-1] - factor * mtrx[i-1][j2-1] ;
            end ;
            a[i2-1] := a[i2-1] - factor * a[i-1] ;
            b[i2-1] := b[i2-1] - factor * b[i-1] ;
          end ;
        end ;
      end ;

      // SINCE ALL OTHER VALUES IN THE MATRIX ARE ZERO NOW, CALCULATE THE
      // COEFFICIENTS BY DIVIDING THE COLUMN VECTORS BY THE DIAGONAL VALUES. */
      for i := 1 to mtrx_size do begin
        if not reverse then begin
          arFwd[i-1].X := a[i-1] / mtrx[i-1][i-1] ;
          arFwd[i-1].Y := b[i-1] / mtrx[i-1][i-1] ;
        end
        else begin
          arRev[i-1].X := a[i-1] / mtrx[i-1][i-1] ;
          arRev[i-1].Y := b[i-1] / mtrx[i-1][i-1] ;
        end ;
      end ;
    end ;

    { CALCULATE THE TRANSFORMATION COEFFICIENTS WITH MORE THAN THE MINIMUM
      NUMBER OF CONTROL POINTS REQUIRED FOR THIS TRANSFORMATION.  THIS
      ROUTINE USES THE LEAST SQUARES METHOD TO COMPUTE THE COEFFICIENTS.
    }
    procedure calcls ;
    var
      i, j, n, cnt : Integer ;
    begin
      // INITIALIZE THE UPPER HALF OF THE MATRIX AND THE TWO COLUMN VECTORS
      for i := 1 to mtrx_size do begin
        for j := i to mtrx_size do begin
          mtrx[i-1][j-1] := 0 ;
          a[i-1] := 0 ;
          b[i-1] := 0 ;
        end ;
      end ;

      // SUM THE UPPER HALF OF THE MATRIX AND THE COLUMN VECTORS ACCORDING TO
      // THE LEAST SQUARES METHOD OF SOLVING OVER DETERMINED SYSTEMS

      cnt := 0 ;
      for n := 0 to lstGCPPoints.Count-1 do begin
        if not get_gcp(n).Active then
          continue ;

        inc( cnt ) ;

        for i := 1 to mtrx_size do begin
          for j := i to mtrx_size do begin
            if not reverse then
              mtrx[i-1][j-1] := mtrx[i-1][j-1] +
                                term( i, get_gcp(n).Src.X, get_gcp(n).Src.Y ) *
                                term( j, get_gcp(n).Src.X, get_gcp(n).Src.Y )
            else
              mtrx[i-1][j-1] := mtrx[i-1][j-1] +
                                term( i, get_gcp(n).Dst.X, get_gcp(n).Dst.Y ) *
                                term( j, get_gcp(n).Dst.X, get_gcp(n).Dst.Y )
          end ;

          if not reverse then begin
            a[i-1] := a[i-1] +
                      get_gcp(n).Dst.X *
                      term( i, get_gcp(n).Src.X, get_gcp(n).Src.Y ) ;
            b[i-1] := b[i-1] +
                      get_gcp(n).Dst.Y *
                      term( i, get_gcp(n).Src.X, get_gcp(n).Src.Y ) ;
          end
          else begin
            a[i-1] := a[i-1] +
                      get_gcp(n).Src.X *
                      term( i, get_gcp(n).Dst.X, get_gcp(n).Dst.Y ) ;
            b[i-1] := b[i-1] +
                      get_gcp(n).Src.Y *
                      term( i, get_gcp(n).Dst.X, get_gcp(n).Dst.Y ) ;
          end ;
        end ;
      end ;

      if cnt <= mtrx_size then begin
        FError := ERROR_INTERNAL_GCP ;
        Abort ;
      end;

      // TRANSPOSE VALUES IN UPPER HALF OF M TO OTHER HALF
      for i := 2 to mtrx_size do begin
        for j := 1 to i do
          mtrx[i-1][j-1] := mtrx[j-1][i-1] ;
      end ;

      solvemat ;
    end ;

    { CALCULATE THE TRANSFORMATION COEFFICIENTS WITH EXACTLY THE MINIMUM
      NUMBER OF CONTROL POINTS REQUIRED FOR THIS TRANSFORMATION.
    }
    procedure exactdet ;
    var
      currow : Integer ;
      i,j    : Integer ;
    begin
      currow := 0 ;

      for i := 0 to lstGCPPoints.Count-1 do begin
        if not get_gcp(i).Active then
          continue ;

        inc( currow ) ;

        // POPULATE MATRIX M
        for j := 1 to mtrx_size do
          if not reverse then
            mtrx[currow-1][j-1] :=
              term( j , get_gcp(i).Src.X, get_gcp(i).Src.Y )
          else
            mtrx[currow-1][j-1] :=
              term( j , get_gcp(i).Dst.X, get_gcp(i).Dst.Y ) ;

        // POPULATE MATRIX A AND B
        if not reverse then begin
          a[currow-1] := get_gcp(i).Dst.X ;
          b[currow-1] := get_gcp(i).Dst.Y ;
        end
        else begin
          a[currow-1] := get_gcp(i).Src.X ;
          b[currow-1] := get_gcp(i).Src.Y ;
        end ;

      end ;

      if currow <> mtrx_size then begin
        FError := ERROR_INTERNAL_GCP ;
        Abort ;
      end;

      solvemat ;
    end ;

    { COMPUTE THE GEOREFFERENCING COEFFICIENTS BASED ON A SET OF CONTROL POINTS
    }
    function calccoef : Boolean ;
    var
      i   : Integer ;
      cnt : Integer ;
    begin
      // CALCULATE THE MINIMUM NUMBER OF CONTROL POINTS NEEDED TO DETERMINE
      // A TRANSFORMATION OF THIS ORDER */

      case orderVal of
        TGIS_PolynomialOrder.First  : mtrx_size := ((1 + 1) * (1 + 2)) div 2 ;
        TGIS_PolynomialOrder.Second : mtrx_size := ((2 + 1) * (2 + 2)) div 2 ;
        TGIS_PolynomialOrder.Third  : mtrx_size := ((3 + 1) * (3 + 2)) div 2 ;
        else                          begin
                                        assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
                                      end ;
      end ;

      cnt := 0 ;
      for i := 0 to lstGCPPoints.Count - 1 do begin
        if not get_gcp(i).Active then
          continue ;

        inc( cnt ) ;
      end ;

      if cnt < mtrx_size then begin
        FError := ERROR_NOTENOUGHPOINTS_GCP ;
        Abort ;
      end;

      // INITIALIZE MATRIX
      SetLength( mtrx , mtrx_size ) ;
      for i := 0 to mtrx_size-1 do
        SetLength( mtrx[i], mtrx_size ) ;

      SetLength( a , mtrx_size ) ;
      SetLength( b , mtrx_size ) ;

      if cnt = mtrx_size then exactdet
                         else calcls ;

      Result := cnt = mtrx_size ;
    end ;

  begin
    Result := False ;
    FRMS   := GIS_MAX_DOUBLE ;

    bPrepared := True ;

    try
      orderVal := _order ;

      // forward coefficients
         reverse := False ;
         calccoef ;

      // reverse coefficients
         reverse := True ;
         exact := calccoef ;

      // RMS calculations
         if exact then FRMS := 0
                  else FRMS := calculate_rms ;

      FError := ERROR_CLEAR_GCP ;
      Result := True ;
    except
      bPrepared := False ;
    end ;
    bModified := True ;
  end ;

  function TGIS_TransformPolynomial.Transform(
    const _ptg  : TGIS_Point
  ) : TGIS_Point ;
  var
    xx   : Double ;
    yy   : Double ;
    xy   : Double ;
    xxx  : Double ;
    yyy  : Double ;
    xxy  : Double ;
    yyx  : Double ;
  begin
    if not bPrepared then begin
      FError := ERROR_NOTPREPARED_GCP ;
      Result := _TGIS_Point(_ptg) ;
      exit ;
    end
    else
      FError := ERROR_CLEAR_GCP ;

    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point ;
    {$ENDIF}
    case orderVal of
      TGIS_PolynomialOrder.First :
        begin
          Result.X := arFwd[0].X          +
                      arFwd[1].X * _ptg.X +
                      arFwd[2].X * _ptg.Y ;
          Result.Y := arFwd[0].Y          +
                      arFwd[1].Y * _ptg.X +
                      arFwd[2].Y * _ptg.Y ;
        end ;
      TGIS_PolynomialOrder.Second :
        begin
          xx := _ptg.X * _ptg.X ;
          yy := _ptg.Y * _ptg.Y ;
          xy := _ptg.X * _ptg.Y ;
          Result.X := arFwd[0].X          +
                      arFwd[1].X * _ptg.X +
                      arFwd[2].X * _ptg.Y +
                      arFwd[3].X * xx     +
                      arFwd[4].X * xy     +
                      arFwd[5].X * yy ;
          Result.Y := arFwd[0].Y          +
                      arFwd[1].Y * _ptg.X +
                      arFwd[2].Y * _ptg.Y +
                      arFwd[3].Y * xx     +
                      arFwd[4].Y * xy     +
                      arFwd[5].Y * yy     ;
        end ;
      TGIS_PolynomialOrder.Third :
        begin
          xx  := _ptg.X * _ptg.X ;
          yy  := _ptg.Y * _ptg.Y ;
          xy  := _ptg.X * _ptg.Y ;
          xxx := xx * _ptg.X ;
          yyy := yy * _ptg.Y ;
          xxy := xx * _ptg.Y ;
          yyx := yy * _ptg.X ;

          Result.X := arFwd[0].X          +
                      arFwd[1].X * _ptg.X +
                      arFwd[2].X * _ptg.Y +
                      arFwd[3].X * xx     +
                      arFwd[4].X * xy     +
                      arFwd[5].X * yy     +
                      arFwd[6].X * xxx    +
                      arFwd[7].X * xxy    +
                      arFwd[8].X * yyx    +
                      arFwd[9].X * yyy    ;
          Result.Y := arFwd[0].Y          +
                      arFwd[1].Y * _ptg.X +
                      arFwd[2].Y * _ptg.Y +
                      arFwd[3].Y * xx     +
                      arFwd[4].Y * xy     +
                      arFwd[5].Y * yy     +
                      arFwd[6].Y * xxx    +
                      arFwd[7].Y * xxy    +
                      arFwd[8].Y * yyx    +
                      arFwd[9].Y * yyy    ;
        end ;
      else
        assert( False, GIS_RS_ERR_UNTESTED ) ;
    end ;
  end ;

  function TGIS_TransformPolynomial.Untransform(
    const _ptg : TGIS_Point
  ) : TGIS_Point ;
  var
    xx   : Double ;
    yy   : Double ;
    xy   : Double ;
    xxx  : Double ;
    yyy  : Double ;
    xxy  : Double ;
    yyx  : Double ;
  begin
    if not bPrepared then begin
      FError := ERROR_NOTPREPARED_GCP ;
      Result := _TGIS_Point(_ptg) ;
      exit ;
    end
    else
      FError := ERROR_CLEAR_GCP ;

    {$IFDEF GIS_NORECORDS}
      Result := new TGIS_Point ;
    {$ENDIF}
    case orderVal of
      TGIS_PolynomialOrder.First :
        begin
          Result.X := arRev[0].X          +
                      arRev[1].X * _ptg.X +
                      arRev[2].X * _ptg.Y ;
          Result.Y := arRev[0].Y          +
                      arRev[1].Y * _ptg.X +
                      arRev[2].Y * _ptg.Y ;
        end ;
      TGIS_PolynomialOrder.Second :
        begin
          xx := _ptg.X * _ptg.X ;
          yy := _ptg.Y * _ptg.Y ;
          xy := _ptg.X * _ptg.Y ;
          Result.X := arRev[0].X          +
                      arRev[1].X * _ptg.X +
                      arRev[2].X * _ptg.Y +
                      arRev[3].X * xx     +
                      arRev[4].X * xy     +
                      arRev[5].X * yy ;
          Result.Y := arRev[0].Y          +
                      arRev[1].Y * _ptg.X +
                      arRev[2].Y * _ptg.Y +
                      arRev[3].Y * xx     +
                      arRev[4].Y * xy     +
                      arRev[5].Y * yy     ;
        end ;
      TGIS_PolynomialOrder.Third :
        begin
          xx  := _ptg.X * _ptg.X ;
          yy  := _ptg.Y * _ptg.Y ;
          xy  := _ptg.X * _ptg.Y ;
          xxx := xx * _ptg.X ;
          yyy := yy * _ptg.Y ;
          xxy := xx * _ptg.Y ;
          yyx := yy * _ptg.X ;

          Result.X := arRev[0].X          +
                      arRev[1].X * _ptg.X +
                      arRev[2].X * _ptg.Y +
                      arRev[3].X * xx     +
                      arRev[4].X * xy     +
                      arRev[5].X * yy     +
                      arRev[6].X * xxx    +
                      arRev[7].X * xxy    +
                      arRev[8].X * yyx    +
                      arRev[9].X * yyy    ;
          Result.Y := arRev[0].Y          +
                      arRev[1].Y * _ptg.X +
                      arRev[2].Y * _ptg.Y +
                      arRev[3].Y * xx     +
                      arRev[4].Y * xy     +
                      arRev[5].Y * yy     +
                      arRev[6].Y * xxx    +
                      arRev[7].Y * xxy    +
                      arRev[8].Y * yyx    +
                      arRev[9].Y * yyy    ;
        end ;
      else
        assert( False, GIS_RS_ERR_UNTESTED ) ;
    end ;
  end ;

  procedure TGIS_TransformPolynomial.Transform3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg  : TGIS_Point3D
  ) ;
  var
    ptg : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ;
  begin
    ptg.X := _ptg.X ;
    ptg.Y := _ptg.Y ;

    ptg := Transform( ptg ) ;

    _ptg.X := ptg.X ;
    _ptg.Y := ptg.Y ;
  end;

  procedure TGIS_TransformPolynomial.Untransform3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg  : TGIS_Point3D
  ) ;
  var
    ptg : TGIS_Point {$IFDEF GIS_NORECORDS} := new TGIS_Point {$ENDIF} ;
  begin
    ptg.X := _ptg.X ;
    ptg.Y := _ptg.Y ;

    ptg := Untransform( ptg ) ;

    _ptg.X := ptg.X ;
    _ptg.Y := ptg.Y ;
  end;

  procedure TGIS_TransformPolynomial.LoadFromFile(
    const _path : String
  ) ;
  var
    ini  : TGIS_Config ;
    tkn  : TGIS_Tokenizer ;
    i    : Integer     ;
    id   : Integer     ;
    sact : String      ;
    act  : Boolean     ;
    src  : TGIS_Point  ;
    dst  : TGIS_Point  ;
  begin
    ini := TGIS_ConfigIni.Create( nil, _path ) ;
    try
      ini.Section := GIS_TRN_HEADER ;

      Active := ini.ReadBoolean( GIS_TRN_ACTIVE, True ) ;

      tkn := TGIS_Tokenizer.Create ;
      try
        Clear ;
        for i :=1 to 999 do begin
          tkn.Execute(
            ini.ReadString( Format( GIS_TRN_POINT, [i] ), '' ), [';']
          ) ;
          if tkn.Result.Count <> 6 then break ;

          id := StrToInt( Trim( tkn.Result[ 0 ] ) ) ;

          {$IFDEF GIS_NORECORDS}
            src := new TGIS_Point ;
          {$ENDIF}
          src.X := DotStrToFloat( Trim( tkn.Result[ 1 ] ) ) ;
          src.Y := DotStrToFloat( Trim( tkn.Result[ 2 ] ) ) ;
          
          {$IFDEF GIS_NORECORDS}
            dst := new TGIS_Point ;
          {$ENDIF}
          dst.X := DotStrToFloat( Trim( tkn.Result[ 3 ] ) ) ;
          dst.Y := DotStrToFloat( Trim( tkn.Result[ 4 ] ) ) ;

          sact  := Trim( tkn.Result[ 5 ] ) ;

          act := ParamBoolean( sact, TRUE )
                 or
                 ( CompareText( sact, GIS_TRN_ACTIVE_OLD ) = 0 ) ;

          AddPoint( src, dst, id, act );
        end;

        case ini.ReadInteger( GIS_TRN_ORDER, 0 ) of
          1 : Prepare( TGIS_PolynomialOrder.First  ) ;
          2 : Prepare( TGIS_PolynomialOrder.Second ) ;
          3 : Prepare( TGIS_PolynomialOrder.Third  ) ;
        end;

        CuttingPolygon := ini.ReadString( GIS_TRN_CUTTINGPOLYGON, '' ) ;
      finally
        FreeObject( tkn ) ;
      end;
    finally
      FreeObject( ini ) ;
    end;

    bModified := False ;
  end;

  procedure TGIS_TransformPolynomial.SaveToFile(
    const _path : String
  ) ;
  var
    ini : TGIS_ConfigIni   ;
    i   : Integer    ;
    gcp : T_pointGCP ;
    act : String     ;
    key : String     ;
  begin
    ini := TGIS_ConfigIni.Create( nil, _path ) ;
    try
      ini.Section := GIS_TRN_HEADER ;

      ini.WriteBoolean( GIS_TRN_ACTIVE, Active, True ) ;
      ini.WriteString( GIS_TRN_METHOD, GIS_TRN_POLYNOMIAL, '' );

      case orderVal of
        TGIS_PolynomialOrder.First  : i := 1 ;
        TGIS_PolynomialOrder.Second : i := 2 ;
        TGIS_PolynomialOrder.Third  : i := 3 ;
        else                          begin
                                        i := 0 ;
                                        assert( False, GIS_RS_ERR_UNTESTED );
                                      end;
      end;
      ini.WriteInteger( GIS_TRN_ORDER, i, 0 );

      for i := 0 to lstGCPPoints.Count -1 do begin
        gcp := T_pointGCP( lstGCPPoints[i] ) ;

        act := ConstructParamBoolean( gcp.Active ) ;

        key := Format( GIS_TRN_POINT, [i+1] ) ;

        ini.WriteString(
          key,
          Format( '%d;%s;%s;%s;%s;%s',
                  [ gcp.Uid,
                    DotFloatToStr( gcp.Src.X ), DotFloatToStr( gcp.Src.Y ),
                    DotFloatToStr( gcp.Dst.X ), DotFloatToStr( gcp.Dst.Y ),
                    act
                  ]
                ),
          ''
        );
        ini.WriteString( GIS_TRN_CUTTINGPOLYGON, CuttingPolygon, '' ) ;
      end ;

      // clear points left from previous save
      for i := lstGCPPoints.Count to 999 do begin
        key := Format( GIS_TRN_POINT, [i+1] ) ;
        if IsStringEmpty( ini.ReadString( key, '' ) ) then
          break ;
        ini.IniObj.DeleteKey( ini.Section, key );
      end;

      ini.Save ;
    finally
      FreeObject( ini ) ;
    end;

    bModified := False ;
  end;

//==============================================================================
// TGIS_TransformCustom
//==============================================================================

  function TGIS_TransformCustom.Transform(
    const _ptg   : TGIS_Point
  ) : TGIS_Point ;
  var
    ptg : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF} ;
  begin
    ptg.X := _ptg.X ;
    ptg.Y := _ptg.Y ;
    ptg.Z := 0      ;
    ptg.M := 0      ;

    Transform3D_Ref( ptg ) ;

    Result.X := ptg.X ;
    Result.Y := ptg.Y ;
  end;

  function TGIS_TransformCustom.Untransform(
    const _ptg   : TGIS_Point
  ) : TGIS_Point ;
  var
    ptg : TGIS_Point3D {$IFDEF GIS_NORECORDS} := new TGIS_Point3D {$ENDIF} ;
  begin
    ptg.X := _ptg.X ;
    ptg.Y := _ptg.Y ;
    ptg.Z := 0      ;
    ptg.M := 0      ;

    Untransform3D_Ref( ptg ) ;

    Result.X := ptg.X ;
    Result.Y := ptg.Y ;
  end;

  procedure TGIS_TransformCustom.Transform3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg   : TGIS_Point3D
  ) ;
  begin
    if assigned( CustomTransformEvent ) then
      CustomTransformEvent( True, _ptg ) ;
  end;

  procedure TGIS_TransformCustom.Untransform3D_Ref(
    {$IFNDEF JAVA} var {$ENDIF} _ptg   : TGIS_Point3D
  ) ;
  begin
    if assigned( CustomTransformEvent ) then
      CustomTransformEvent( False, _ptg ) ;
  end;

//==============================================================================
// TGIS_TransformCustom
//==============================================================================

  function GisTransformFactory(
    const _path : String
  ) :  TGIS_Transform ;
  var
    ini : TGIS_ConfigIni ;
  begin
    ini := TGIS_ConfigIni.Create( nil, _path );
    try
      ini.Section := GIS_TRN_HEADER ;

      if CompareText(
           ini.ReadString( GIS_TRN_METHOD, '' ),
           GIS_TRN_POLYNOMIAL
         ) = 0 then
      begin
        Result := TGIS_TransformPolynomial.Create ;
        Result.LoadFromFile( _path );
      end
      else
        Result := nil ;
    finally
      FreeObject( ini ) ;
    end;
  end;

//==================================== END =====================================
end.

