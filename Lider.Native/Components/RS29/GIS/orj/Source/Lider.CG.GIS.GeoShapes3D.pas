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
  Encapsulation of 3D shapes like PointCloud or Mesh.
}

{$IFDEF DCC}
  unit GisShapes3D ;
  {$HPPEMIT '#pragma link "GisShapes3D"'}
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
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Types,

    GisTypes,
    GisLayerVector,
    GisTypesUI ;
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
   ///   Encapsulation of PointCloud.
   /// </summary>
   TGIS_ShapePointCloud = {$IFDEF OXYGENE} public {$ENDIF} class ( TGIS_ShapeMultiPoint )
     private
       FVertexColors : TGIS_Pixels ;
       FVertexCount  : Integer ;
     private
       procedure allocVertexColors ;
     {$IFDEF OXYGENE} assembly or protected {$ELSE} protected {$ENDIF}
       /// <inheritdoc/>
       function  getIntersectInternal3D
                                 ( const _extent        : TGIS_Extent ;
                                   const _lockInternal2 : Boolean
                                 ) : TGIS_Shape ; override;
     public // API - Shape Level

        /// <inheritdoc/>
        constructor Create     ; overload;
                              {$IFNDEF CLR} override; {$ENDIF}

        /// <inheritdoc/>
        constructor Create       ( const _dim    : TGIS_DimensionType
                                 ) ; overload; override;

       /// <inheritdoc/>
       constructor Create       ( const _source : TGIS_Shape ;
                                  {$IFDEF MANAGED}
                                    const _ptr  : TGIS_Bytes ;
                                  {$ELSE}
                                    const _ptr  : Pointer    ;
                                  {$ENDIF}
                                  const _mapped : Boolean    ;
                                  const _uid    : TGIS_Uid      ;
                                  const _layer  : TGIS_LayerVector ;
                                  const _dim    : TGIS_DimensionType
                                ) ; overload;
                                override;

       /// <inheritdoc/>
       function CreateCopy : TGIS_Shape ; override;

       /// <inheritdoc/>
       procedure Reset ; override;

       /// <inheritdoc/>
       procedure Draw           ( const _mode : TGIS_RendererMultipassMode
                                ) ; override;

       /// <summary>
       ///  Add a vertex.
       /// </summary>
       /// <param name="_ptg">
       ///   vertex coordinate
       /// </param>
       /// <param name="_color">
       ///   vertex color
       /// </param>
       procedure AddVertex      ( const _ptg    : TGIS_Point3D ;
                                  const _color  : TGIS_Color
                                ) ;
     public
      /// <summary>
      ///  Array of vertex colors
      /// </summary>
      property VertexColors : TGIS_Pixels read FVertexColors ;
   end ;

//##############################################################################
implementation

{$IFDEF DCC}
  uses
    GisRtl,
    GisResource,
    GisInterfaces,
    GisRendererAbstract ;
{$ENDIF}


//==============================================================================
// TGIS_ShapePointCloud
//==============================================================================

  constructor TGIS_ShapePointCloud.Create ;
  begin
     inherited Create ;

    FVertexCount  := 0 ;
    FVertexColors := nil ;
  end ;


  constructor TGIS_ShapePointCloud.Create(
    const _dim : TGIS_DimensionType
  ) ;
  begin
    inherited Create( _dim ) ;

    FVertexCount  := 0 ;
    FVertexColors := nil ;
  end ;

  constructor TGIS_ShapePointCloud.Create(
    const _source : TGIS_Shape       ;
    {$IFDEF MANAGED}
      const _ptr  : TGIS_Bytes       ;
    {$ELSE}
      const _ptr  : Pointer          ;
    {$ENDIF}
    const _mapped : Boolean          ;
    const _uid    : TGIS_Uid            ;
    const _layer  : TGIS_LayerVector ;
    const _dim    : TGIS_DimensionType
  ) ;
  begin
    inherited Create( _source, _ptr, _mapped, _uid, _layer, _dim ) ;

    FVertexCount  := 0 ;
    FVertexColors := nil ;
  end ;

  procedure TGIS_ShapePointCloud.allocVertexColors ;
  begin
    {$IFDEF DCC}
      if (FVertexCount >= System.Length(FVertexColors) ) then
        SetLength( FVertexColors, FVertexCount + 256 ) ;
    {$ELSE}
        if (FVertexCount >= RemObjects.Elements.System.length( FVertexColors ) ) then
          SetLength( FVertexColors, FVertexCount + 256 ) ;
    {$ENDIF}
  end ;

  function TGIS_ShapePointCloud.getIntersectInternal3D(
    const _extent        : TGIS_Extent ;
    const _lockInternal2 : Boolean
  ) : TGIS_Shape ;
  var
    k, c     : Integer ;
    point    : TGIS_Point3D ;
  begin
    Result := nil ;

    if IsInsideExtent(_extent, TGIS_InsideType.Full) then begin
      Result := TGIS_ShapePointCloud.Create(nil, nil, False, Uid, Layer, Dimension) ;

      copyInternalFields( self, Result ) ;
      copyInternalParams( self, Result ) ;

      if _lockInternal2 then Result.Lock( TGIS_Lock.Internal2 )
                        else Result.Lock( TGIS_Lock.Internal  ) ;

      c := 0 ;
      Result.AddPart ;
      for k := 0 to GetPartSize(0) -1 do begin
        if FVertexCount > 0 then begin
          TGIS_ShapePointCloud(Result).AddVertex(
            GetPoint3D(0, k),
            TGIS_Color.FromRGB( Cardinal(FVertexColors[c]) )
          ) ;
          inc( c ) ;
        end
        else
          Result.AddPoint3D( GetPoint3D(0, k) ) ;
      end ;

      Result.Unlock ;

      exit ;
    end ;

    if IsInsideExtent(_extent, TGIS_InsideType.Partial) then begin
      Result := TGIS_ShapePointCloud.Create(nil, nil, False, Uid, Layer, Dimension) ;

      copyInternalFields( self, Result ) ;
      copyInternalParams( self, Result ) ;

      if _lockInternal2 then Result.Lock( TGIS_Lock.Internal2 )
                        else Result.Lock( TGIS_Lock.Internal  ) ;

      c := 0 ;
      Result.AddPart ;
      for k := 0 to GetPartSize(0) -1 do begin
        point := GetPoint3D(0, k);
        if ( point.X <= _extent.XMax ) and
           ( point.X >= _extent.XMin ) and
           ( point.Y <= _extent.YMax ) and
           ( point.Y >= _extent.YMin )
        then begin
          if FVertexCount > 0 then begin
            TGIS_ShapePointCloud(Result).AddVertex(
              point,
              TGIS_Color.FromRGB( Cardinal(FVertexColors[c]) )
            ) ;
          end
          else
            Result.AddPoint3D( point ) ;
        end ;
        inc( c ) ;
      end ;

      if not Result.IsEmpty then begin
        Result.Unlock ;
        exit ;
      end
      else
        FreeObject( Result ) ;
    end ;
  end ;

  procedure TGIS_ShapePointCloud.AddVertex(
    const _ptg    : TGIS_Point3D ;
    const _color  : TGIS_Color
  ) ;
  begin
    allocVertexColors ;

    AddPoint3D( _ptg ) ;

    FVertexColors[FVertexCount] := Integer(_color.ARGB) ;
    inc( FVertexCount ) ;
  end ;

  function TGIS_ShapePointCloud.CreateCopy : TGIS_Shape ;
  begin
    Result := inherited CreateCopy() ;

    TGIS_ShapePointCloud(Result).FVertexColors := FVertexColors ;
    TGIS_ShapePointCloud(Result).FVertexCount  := FVertexCount ;
  end ;

  procedure TGIS_ShapePointCloud.Draw(
    const _mode : TGIS_RendererMultipassMode
  ) ;
  var
    pt    : TPoint ;
    {$IFDEF DCC}
      [weak]
    {$ENDIF}
    vwr   : IGIS_Viewer ;
    i, j  : Integer ;
    c     : Integer ;
    rdr   : TGIS_RendererAbstract ;
    rct   : TRect ;
    w     : Integer ;
  begin
    if not assigned( Layer ) or not assigned( Layer.Viewer ) then exit ;

    vwr := Layer.Viewer.Ref ;
    rdr := TGIS_RendererAbstract( Layer.Renderer ) ;
    c   := -1 ;

    w := rdr.TwipsToPixels( Params.Marker.Size ) ;
    if w = 0 then
      w := 1 ;
    rdr.CanvasPen.Width := w ;
    rdr.CanvasPen.Color := Params.Marker.Color ;

    if FVertexCount = 0 then begin
      rdr.CanvasPen.Style   := TGIS_PenStyle.Clear ;
      rdr.CanvasBrush.Color := Params.Marker.Color ;
    end ;

    rct := Rect( 0, 0,
                 vwr.ViewerParent.ControlCanvasWidth,
                 vwr.ViewerParent.ControlCanvasHeight
               ) ;

    for i := 0 to GetNumParts-1 do
      for j := 0 to GetPartSize(i)-1 do begin
        pt := vwr.MapToScreen( GetPoint(i, j) ) ;

        inc( c ) ;
        if (pt.X < rct.Left) or (pt.X > rct.Right) or (pt.Y < rct.Top) or
           (pt.Y > rct.Bottom) then continue ;

        if c < FVertexCount then
          rdr.CanvasPen.Color := TGIS_Color.FromRGB( Cardinal(FVertexColors[c]) ) ;

        rdr.CanvasDrawRectangle( Rect(pt.X, pt.Y, pt.X+1, pt.Y+1) ) ;

        if j mod GIS_PROGRESS_TRESHOLD = 0 then begin
          if Layer.HourglassShake then
            break ;
        end ;

      end ;
  end ;

  procedure TGIS_ShapePointCloud.Reset ;
  begin
    inherited ;

    FVertexCount := 0 ;
  end ;



{==================================== END =====================================}
end.
