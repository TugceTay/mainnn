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
  Compound layer encapsulation.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoLayerCompound ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoLayerCompound"'}
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

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,

    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoLayer,
    Lider.CG.GIS.GeoLayerVector ;
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
    ///   General compound layer class is a container for other layers. Used to
    ///   manage sublayers and their behavior like drawing, properties, etc.
    /// </summary>
    TGIS_LayerCompoundAbstract = {$IFDEF OXYGENE} public {$ENDIF}
                                  class( TGIS_Layer )
      {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
        // for internal use of TGIS_Viewer

        /// <inheritdoc/>
        procedure setUp              ; override;

        /// <inheritdoc/>
        procedure setUp3             ; override;

        procedure fset_Viewer        ( const _value : TGIS_ViewerRef
                                     )
                                     ; override;
      protected

        function fget_LayersCount    : Integer ;
        function fget_Layer          ( const _idx : Integer
                                     ) : TGIS_Layer ;
      protected

        procedure doDestroy ; override;

        function  fget_FileCopyrights  : String ; override;
      public // constructors

        /// <inheritdoc/>
        constructor Create ; override;

      public
        /// <inheritdoc/>
        function  Draw              : Boolean ; override;

        /// <inheritdoc/>
        procedure DrawFlash         ; override;

        /// <inheritdoc/>
        function  DrawSelectedEx    ( const _extent : TGIS_Extent
                                    ) : Boolean ; override;

        /// <inheritdoc/>
        procedure RecalcExtent      ; override;

        /// <inheritdoc/>
        procedure RecalcProjectedExtent ; override;
      public

        /// <summary>
        ///   Add a layer to the list.
        /// </summary>
        /// <param name="_layer">
        ///   layer to be added
        /// </param>
        procedure Add              ( const _layer : TGIS_Layer
                                    ) ;

        /// <summary>
        ///   Retrieve the layer identified by a name.
        /// </summary>
        /// <param name="_name">
        ///   name of layer found
        /// </param>
        /// <returns>
        ///   found layer or nil
        /// </returns>
        function  Get              ( const _name : String
                                    ) : TGIS_Layer ; overload;
      public

         /// <summary>
         ///   Sub layers count.
         /// </summary>
         property LayersCount : Integer read fget_LayersCount ;

         /// <summary>
         ///   All sub layers.
         /// </summary>
         /// <param name="_idx">
         ///   index of a sublayer
         /// </param>
         property Layer[const _idx : Integer] : TGIS_Layer read fget_Layer ;
    end ;

    /// <summary>
    ///   Vector compound layer class.
    /// </summary>
    TGIS_LayerCompoundVector = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_LayerCompoundAbstract )
     public
        /// <inheritdoc/>
        constructor Create ; override;

       /// <summary>
       ///   Locate a shape (more precise).
       /// </summary>
       /// <param name="_ptg">
       ///   reference point /searching point/ in a layer;
       /// </param>
       /// <param name="_prec">
       ///   precision /not a longer distance than/;
       /// </param>
       /// <param name="_uid">
       ///   Uid of reference shape or -1; thanks to this, is possible to find
       ///   nearest shape, excluding shape given by _uid.
       /// </param>
       /// <param name="_dist">
       ///   reached distance between _pt and shape in Result;
       /// </param>
       /// <param name="_part">
       ///   number of the part closest to a given point
       /// </param>
       /// <param name="_proj">
       ///   point projected to the nearest element of a shape
       /// </param>
       /// <param name="_visible">
       ///   if true the only visible shapes will be evaluated;
       /// </param>
       /// <returns>
       ///   found shape or nil
       /// </returns>
       function  LocateEx         ( const _ptg     : TGIS_Point ;
                                    const _prec    : Double     ;
                                    const _uid     : Integer    ;
                                    var   _dist    : Double     ;
                                    var   _part    : Integer    ;
                                    var   _proj    : TGIS_Point ;
                                    const _visible : Boolean
                                  ) : TGIS_Shape ; overload; virtual;

       /// <summary>
       ///   Locate a shape (more precise).
       /// </summary>
       /// <param name="_ptg">
       ///   reference point /searching point/ in a layer;
       /// </param>
       /// <param name="_prec">
       ///   precision /not a longer distance than/;
       /// </param>
       /// <param name="_uid">
       ///   Uid of reference shape or -1; thanks to this, is possible to find
       ///   nearest shape, excluding shape given by _uid.
       /// </param>
       /// <param name="_dist">
       ///   reached distance between _pt and shape in Result;
       /// </param>
       /// <param name="_part">
       ///   number of the part closest to a given point
       /// </param>
       /// <param name="_proj">
       ///   point projected to the nearest element of a shape
       /// </param>
       /// <param name="_dist_prefer">
       ///   preferred distance (including shape type priority :
       ///   points over lines and polygons)
       /// </param>
       /// <param name="_visible">
       ///   if true the only visible shapes will be evaluated;
       /// </param>
       /// <returns>
       ///   found shape or nil
       /// </returns>
       function  LocateEx         ( const _ptg         : TGIS_Point ;
                                    const _prec        : Double     ;
                                    const _uid         : Integer    ;
                                    var   _dist        : Double     ;
                                    var   _part        : Integer    ;
                                    var   _proj        : TGIS_Point ;
                                    var   _dist_prefer : Double     ;
                                    const _visible     : Boolean
                                  ) : TGIS_Shape ; overload; virtual;
    end ;

    /// <summary>
    ///   Pixel compound layer class.
    /// </summary>
    TGIS_LayerCompoundPixel = {$IFDEF OXYGENE} public {$ENDIF}
                                class( TGIS_LayerCompoundAbstract )
      public
        /// <inheritdoc/>
        constructor Create ; override;

        /// <summary>
        ///   If given point is located in image area, return true and set
        ///   variables:
        /// </summary>
        /// <param name="_ptg">
        ///   reference point
        /// </param>
        /// <param name="_nativesVals">
        ///   dynamic array of double with original values in _ptg point (R, G,
        ///   B), Length(_nativesVals) is equal of bands number (=1 for float
        ///   (single) pixels otherwise min = 3 for 24 bits per pixel /native
        ///   format DK/ )
        /// </param>
        /// <param name="_rgbMapped">
        ///   color in _ptg point after mapping (the same as _nativesVals if no
        ///   mapping, in this case _rgbMapped.Blue = Byte(_nativesVals[0] and
        ///   so on).
        /// </param>
        /// <param name="_transparency">
        ///   return True if point is transparent
        /// </param>
        /// <returns>
        ///   True if the point is located in image area
        /// </returns>
        function Locate           ( const _ptg          : TGIS_Point       ;
                                    var   _rgbMapped    : TGIS_RGBTriple   ;
                                    var   _nativesVals  : TGIS_DoubleArray ;
                                    var   _transparency : Boolean
                                  ) : Boolean ;
    end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoParams,
    Lider.CG.GIS.GeoResource,
    Lider.CG.GIS.GeoRendererAbstract,
    Lider.CG.GIS.GeoClasses ;
{$ENDIF}

//=============================================================================
// TGIS_LayerCompoundAbstract
//=============================================================================

  constructor TGIS_LayerCompoundAbstract.Create ;
  begin
    inherited ;

    SubLayers   := TGIS_LayerAbstractList.Create( False ) ;
    Path        := '' ;
    Name        := '' ;

    FIsModified := False ;
  end ;

  procedure TGIS_LayerCompoundAbstract.doDestroy ;
  begin

    inherited ;
  end ;

  function TGIS_LayerCompoundAbstract.fget_FileCopyrights : String ;
  var
    i  : Integer ;
    ll : TGIS_Layer ;
  begin
    Result := '' ;

    for i := 0 to SubLayers.Count -1 do begin
      ll := TGIS_Layer( SubLayers[i] ) ;

      if not ll.Active then
        continue ;

      if IsStringEmpty( ll.FileCopyrights ) then
        continue ;

      if not IsStringEmpty( Result ) then
        Result := Result + #13#10 ;

      Result := Result + ll.Caption + ': ' + ll.FileCopyrights ;
    end ;
  end ;

  function TGIS_LayerCompoundAbstract.fget_Layer(
    const _idx : Integer
  ) : TGIS_Layer ;
  begin
    if ( _idx >=0 ) and ( _idx < SubLayers.Count ) then
      Result := TGIS_Layer( SubLayers[_idx] )
    else
      Result := nil ;
  end ;

  function TGIS_LayerCompoundAbstract.fget_LayersCount : Integer ;
  begin
    Result := SubLayers.Count ;
  end ;

  procedure TGIS_LayerCompoundAbstract.setUp ;
  begin
    inherited ;

    FFileInfo := 'Generic Compound Layer' ;
  end ;

  procedure TGIS_LayerCompoundAbstract.setUp3 ;
  begin
    inherited ;

    View3D.Mode := TGIS_3DLayerType.Off ;
  end ;

  procedure TGIS_LayerCompoundAbstract.fset_Viewer(
    const _value : TGIS_ViewerRef
  ) ;
  var
    i : Integer ;
  begin
    inherited fset_Viewer( _value ) ;

    for i := 0 to SubLayers.Count - 1 do
      TGIS_Layer(SubLayers[i]).Viewer := _value ;
  end ;

  function TGIS_LayerCompoundAbstract.Draw
    : Boolean ;
  var
    i         : Integer    ;
    ll        : TGIS_Layer ;
    start     : Integer    ;
    stop      : Integer    ;
  begin
    Result := False ;
    if Viewer.Ref.Zoom <= 0 then exit ;
    if Viewer.Ref.IsEmpty then exit  ;

    Result  := True ;
    start   := 0 ;
    stop    := SubLayers.Count -1 ;

    for i:= start to stop do begin // all layers
      ll := TGIS_Layer( SubLayers[i] ) ;
      TGIS_RendererAbstract(Self.Renderer).LockTransparent( ll.Transparency ) ;
      try
        ll.Renderer := Self.Renderer ;
        ll.Draw ;
        if HourglassShake then exit ;
      finally
        TGIS_RendererAbstract(Self.Renderer).UnlockTransparent ;
      end ;
    end ;
  end ;


  procedure TGIS_LayerCompoundAbstract.RecalcProjectedExtent ;
  var
    i   : Integer ;
    ext : TGIS_Extent ;
  begin
    ext := GisNoWorld ;

    for i := 0 to SubLayers.Count -1 do begin
      TGIS_Layer(SubLayers[i]).RecalcProjectedExtent ;

      if GisIsNoWorld( TGIS_Layer(SubLayers[i]).ProjectedExtent ) then
        continue ;

      ext := GisMaxExtent( ext, TGIS_Layer(SubLayers[i]).ProjectedExtent ) ;
    end ;

    FProjectedExtent := ext ;
  end ;

  procedure TGIS_LayerCompoundAbstract.RecalcExtent ;
  var
    i   : Integer ;
    ext : TGIS_Extent ;
  begin
    ext := GisNoWorld ;

    for i := 0 to SubLayers.Count -1 do begin
      TGIS_Layer(SubLayers[i]).RecalcExtent ;

      if GisIsNoWorld( TGIS_Layer(SubLayers[i]).Extent ) then
        continue ;

      ext := GisMaxExtent( ext, TGIS_Layer(SubLayers[i]).Extent ) ;
    end ;

    Extent := ext ;
  end ;

  procedure TGIS_LayerCompoundAbstract.Add(
    const _layer : TGIS_Layer
  ) ;
  begin
    _layer.Viewer := Viewer ;
    _layer.ParentLayer := self ;

    if not _layer.IsOpened then
      _layer.Open ;

    if IsStringEmpty( _layer.Caption ) then
      _layer.Caption := _layer.Name ;

    if Get( _layer.Name ) <> nil then
      raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_LAYEREXIST ), _layer.Name, 0 ) ;

    SubLayers.Add( _layer ) ;

    RecalcExtent ;

    _layer.ReadConfig ;

    if _layer.IsPersistent then
      FIsModified := True ;
  end ;

  function TGIS_LayerCompoundAbstract.Get(
    const _name : String
  ) : TGIS_Layer ;
  var
    i : Integer ;
  begin
    Result := nil ;

    for i := 0 to (SubLayers.Count - 1) do begin
      if CompareText( TGIS_Layer(SubLayers[i]).Name, _name ) = 0 then begin
        Result := TGIS_Layer(SubLayers[i]) ;
        exit ;
      end ;
    end ;
  end ;

  procedure TGIS_LayerCompoundAbstract.DrawFlash ;
  var
    i   : Integer ;
  begin
    for i := 0 to SubLayers.Count -1 do
      TGIS_Layer(SubLayers[i]).DrawFlash ;
  end;

  function TGIS_LayerCompoundAbstract.DrawSelectedEx(
    const _extent : TGIS_Extent
  ) : Boolean ;
  var
    i   : Integer ;
    res : Boolean ;
  begin
    Result := False ;
    res := False ;
    for i := 0 to SubLayers.Count -1 do begin
      res := TGIS_Layer(SubLayers[i]).DrawSelectedEx( _extent ) ;
      Result := Result or res ;
    end ;
  end ;

//=============================================================================
// TGIS_LayerCompoundVector
//=============================================================================

  constructor TGIS_LayerCompoundVector.Create ;
  begin
    inherited ;

    ParamsList.SetUp( TGIS_ParamsSectionVector.Create ) ;
  end ;

  function TGIS_LayerCompoundVector.LocateEx(
    const _ptg     : TGIS_Point ;
    const _prec    : Double     ;
    const _uid     : Integer    ;
    var   _dist    : Double     ;
    var   _part    : Integer    ;
    var   _proj    : TGIS_Point ;
    const _visible : Boolean
  ) : TGIS_Shape ;
  var
    distp : Double ;
  begin
    Result := LocateEx( _ptg, _prec, _uid, _dist, _part, _proj, distp, True ) ;
  end ;

  function TGIS_LayerCompoundVector.LocateEx(
    const _ptg         : TGIS_Point ;
    const _prec        : Double     ;
    const _uid         : Integer    ;
    var   _dist        : Double     ;
    var   _part        : Integer    ;
    var   _proj        : TGIS_Point ;
    var   _dist_prefer : Double     ;
    const _visible     : Boolean
  ) : TGIS_Shape ;
  var
    i         : Integer ;
    tmp_dist  : Double ;              // temporary values
    tmp_distp : Double     ;
    tmp_part  : Integer ;             // temporary values
    tmp_shape : TGIS_Shape ;
    shape     : TGIS_Shape ;          // found values
    dist      : Double ;
    proj      : TGIS_Point ;
    ll        : TGIS_LayerVector ;
    lc        : TGIS_LayerCompoundVector ;
  begin
    Result     := nil ;

    if assigned( Viewer ) and
       Viewer.Ref.InPaint
    then
      exit ;

    if Basemap then
      exit ;

    if not IsOpened then
      Open ;

    shape := nil ;            // assume we find nothing
    dist  := GIS_MAX_DOUBLE ; // assume really huge starting distance

    for i:= SubLayers.Count -1 downto 0 do begin // all layers
      if TGIS_Layer( SubLayers[i] ).Active then begin // but only visible
        try
          if ( SubLayers[i] is TGIS_LayerVector ) then begin

            ll := SubLayers[i] as TGIS_LayerVector ;
            tmp_shape := ll.LocateEx( _ptg, _prec, -1, tmp_dist, tmp_part, proj,
                                      tmp_distp, _visible
                                    ) ;
          end
          else if ( SubLayers[i] is TGIS_LayerCompoundVector ) then begin
            lc := SubLayers[i] as TGIS_LayerCompoundVector ;
            tmp_shape := lc.LocateEx( _ptg, _prec, -1, tmp_dist, tmp_part, proj,
                                      tmp_distp, _visible
                                    ) ;
          end
          else
            tmp_shape := nil ;

          if tmp_shape <> nil then begin
            if tmp_distp <= dist then begin
              // is closer then previous (or lay out on top)
              shape := tmp_shape ;
              dist  := tmp_distp ;
            end ;
          end ;

        except
        end ;
      end ;
    end ;
    Result := shape ;
  end ;

//=============================================================================
// TGIS_LayerCompoundPixel
//=============================================================================

  constructor TGIS_LayerCompoundPixel.Create ;
  begin
    inherited ;

    ParamsList.SetUp( TGIS_ParamsSectionPixel.Create ) ;
  end ;

  function TGIS_LayerCompoundPixel.Locate(
    const _ptg          : TGIS_Point       ;
    var   _rgbMapped    : TGIS_RGBTriple   ;
    var   _nativesVals  : TGIS_DoubleArray ;
    var   _transparency : Boolean
  ) : Boolean ;
  begin
    Result := False ;
  end ;

//==================================== END =====================================
end.

