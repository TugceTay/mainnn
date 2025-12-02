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
  The "fly over" functionality for 3D view.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.Geo3DFlyOver ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.Geo3DFlyOver"'}
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

interface

{$IFDEF CLR}
  uses
    System.Collections.Generic,
    TatukGIS.RTL,
    TatukGIS.RTL.XML ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.Generics.Collections,

    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoTypes3D,
    Lider.CG.GIS.GeoInterfaces,
    Lider.CG.GIS.GeoInterpolation,
    Lider.CG.GIS.GeoLayerVector ;
{$ENDIF}
{$IFDEF JAVA}
  uses
    remobjects.elements.rtl.*,
    java.util,
    tatukgis.rtl,
    tatukgis.rtl.xml ;
{$ENDIF}

type

  /// <summary>
  ///   Interpolation method for a 3D path.
  /// </summary>
  TGIS_3DPathType = {$IFDEF OXYGENE} public {$ENDIF} (
    /// <summary>
    ///   Linear interpolation.
    /// </summary>
    Linear,
    /// <summary>
    ///   Cubic splines interpolation.
    /// </summary>
    CubicSplines
  ) ;


  /// <summary>
  ///   Implementation of the "fly over" functionality for 3D view.
  /// </summary>
  TGIS_3DFlyOver = {$IFDEF OXYGENE} public {$ENDIF}
                   class ( TGIS_BaseObjectDisposable )
    private
      {$IFDEF DCC}
        camList : TList<TGIS_3DCamera> ;
        actList : TList<TGIS_3DCamera> ;
        spdList : TList<Double> ;
      {$ENDIF}
      {$IFDEF CLR}
        camList : List<TGIS_3DCamera> ;
        actList : List<TGIS_3DCamera> ;
        spdList : List<Double> ;
      {$ENDIF}
      {$IFDEF JAVA}
        camList : ArrayList<TGIS_3DCamera> ;
        actList : ArrayList<TGIS_3DCamera> ;
        spdList : ArrayList<Double> ;
      {$ENDIF}
      camPosEx  : TGIS_InterpolatedPath ;
      camRot    : TGIS_InterpolatedPath ;
      timer     : TGIS_Timer ;
      interval  : Integer ;
      time      : Double ;
      focal     : Double ;
      showRef   : Boolean ;
      locked    : Boolean ;
      tolerance : Double ;
    private
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      FViewer    : IGIS_Viewer ;
      {$IFDEF DCC}
        [weak]
      {$ENDIF}
      FViewerWnd : IGIS_ViewerWnd ;
      FPathType  : TGIS_3DPathType ;
      FTimeSpan  : Double ;
      FLooped    : Boolean ;
      FFPS       : Integer ;
      FSpeed     : Double ;
      FAutoSpeed : Boolean ;
    private
      FOnFinished : TNotifyEvent ;
    private
      function  fget_Running  : Boolean ;
      function  fget_Count    : Integer ;
      function  fget_Camera   ( const _idx : Integer
                              ) : TGIS_3DCamera ;
      function  fget_CameraActive
                              ( const _idx : Integer
                              ) : Boolean ;
      procedure fset_CameraActive
                              ( const _idx : Integer ;
                                const _val : Boolean
                              ) ;
      function  fget_CameraName
                              ( const _idx : Integer
                              ) : String ;
      procedure fset_CameraName
                              ( const _idx : Integer ;
                                const _val : String
                              ) ;
      function  fget_FPS      : Integer ;
      procedure fset_FPS      ( const _val : Integer
                              ) ;
      function  fget_TimeSpan : Double ;
      procedure fset_TimeSpan ( const _val : Double
                              ) ;
      function  fget_Speed    : Double ;
      procedure fset_Speed    ( const _val : Double
                              ) ;
    private
      procedure doStep        (       _sender : TObject
                              ) ;
    private
      procedure init          ;
      procedure lockUpdates   ;
      procedure unlockUpdates ;
      function  ready         : Boolean ;
    protected
      procedure doDestroy ; override ;
    public
      /// <summary>
      ///   Creates an instance.
      /// </summary>
      constructor Create ;
    public
      /// <summary>
      ///   Records the current view/camera of the attached 3D viewer.
      /// </summary>
      procedure GrabCamera     ;
      /// <summary>
      ///   Adds a camera to the animation path.
      /// </summary>
      /// <param name="_cam">
      ///   camera to be added
      /// </param>
      procedure AddCamera      ( const _cam   : TGIS_3DCamera
                               ) ;
      /// <summary>
      ///   Deletes camera at a given index.
      /// </summary>
      /// <param name="_idx">
      ///   index of the camera
      /// </param>
      procedure DeleteCamera   ( const _idx   : Integer
                               ) ;
      /// <summary>
      ///   Moves a camera in the order.
      /// </summary>
      /// <param name="_from">
      ///   current index of the camera
      /// </param>
      /// <param name="_to">
      ///   destination index of the camera
      /// </param>
      procedure MoveCamera     ( const _from  : Integer ;
                                 const _to    : Integer
                               ) ;
      /// <summary>
      ///   Moves a camera one position up in the order.
      /// </summary>
      /// <param name="_idx">
      ///   index of the camera
      /// </param>
      procedure MoveCameraUp   ( const _idx   : Integer
                               ) ;
      /// <summary>
      ///   Moves a camera one position down in the order.
      /// </summary>
      /// <param name="_idx">
      ///   index of the camera
      /// </param>
      procedure MoveCameraDown ( const _idx   : Integer
                               ) ;
      /// <summary>
      ///   Removes all the cameras.
      /// </summary>
      procedure Clear          ;
      /// <summary>
      ///   Prepares the animation path using the current active cameras.
      /// </summary>
      procedure PreparePath    ;
      /// <summary>
      ///   Sets the view to the specified camera.
      /// </summary>
      /// <param name="_idx">
      ///   index of the camera
      /// </param>
      procedure MoveToCamera   ( const _idx   : Integer
                               ) ;
      /// <summary>
      ///   Sets the view to the first camera (the camera from which the
      ///   animation path starts).
      /// </summary>
      procedure MoveToStart    ;
      /// <summary>
      ///   Starts the animation.
      /// </summary>
      procedure Start          ;
      /// <summary>
      ///   Stops the animation.
      /// </summary>
      procedure Stop           ;
      /// <summary>
      ///   Pauses the animation.
      /// </summary>
      procedure Pause          ;
      /// <summary>
      ///   Restarts the animation (must be paused).
      /// </summary>
      procedure Restart        ;
      /// <summary>
      ///   Resets the animation (must be paused).
      /// </summary>
      procedure Reset          ;
      /// <summary>
      ///   Saves the cameras to a vector layer.
      /// </summary>
      /// <param name="_layer">
      ///   destination layer
      /// </param>
      procedure SaveToLayer    ( const _layer : TGIS_LayerVector
                               ) ;
      /// <summary>
      ///   Loads the cameras from a vector layer.
      /// </summary>
      /// <param name="_layer">
      ///   source layer
      /// </param>
      procedure LoadFromLayer  ( const _layer : TGIS_LayerVector
                               ) ;
      /// <summary>
      ///   Saves the settings and cameras to an XML file.
      /// </summary>
      /// <param name="_path">
      ///   path to the file
      /// </param>
      procedure SaveToFile     ( const _path : String
                               ) ;
      /// <summary>
      ///   Loads the settings and cameras from an XML file.
      /// </summary>
      /// <param name="_path">
      ///   path to the file
      /// </param>
      procedure LoadFromFile   ( const _path : String
                               ) ;
    public
      /// <summary>
      ///   True if the animation is running.
      /// </summary>
      property  Running    : Boolean
                             read  fget_Running ;
      /// <summary>
      ///   Camera count.
      /// </summary>
      property  Count      : Integer
                             read  fget_Count ;
      /// <summary>
      ///   Access to the Active property of a camera.
      /// </summary>
      /// <param name="_idx">
      ///   index
      /// </param>
      property  Camera[const _idx : Integer]
                           : TGIS_3DCamera
                             read  fget_Camera ;
      /// <summary>
      ///   Access to the Active property of a camera.
      /// </summary>
      /// <param name="_idx">
      ///   index
      /// </param>
      property  CameraActive[const _idx : Integer]
                           : Boolean
                             read  fget_CameraActive
                             write fset_CameraActive ;
      /// <summary>
      ///   Access to the Name property of a camera.
      /// </summary>
      /// <param name="_idx">
      ///   index
      /// </param>
      property  CameraName[const _idx : Integer]
                           : String
                             read  fget_CameraName
                             write fset_CameraName ;
      /// <summary>
      ///   Interpolation method used to prepare the animation path.
      /// </summary>
      property  PathType   : TGIS_3DPathType
                             read  FPathType
                             write FPathType ;
      /// <summary>
      ///   Target FPS (frames per second).
      /// </summary>
      property  FPS        : Integer
                             read  fget_FPS
                             write fset_FPS ;
      /// <summary>
      ///   Timespan of the animation (in seconds).
      /// </summary>
      property  TimeSpan   : Double
                             read  fget_TimeSpan
                             write fset_TimeSpan ;
      /// <summary>
      ///   Animation speed; 1.0 is default, set to less than 1.0 to slow down
      ///   and greater than 1.0 to speed up.
      /// </summary>
      property  SpeedFactor : Double
                             read  fget_Speed
                             write fset_Speed ;
      /// <summary>
      ///   Animation speed; 1.0 is default, set to less than 1.0 to slow down
      ///   and greater than 1.0 to speed up.
      /// </summary>
      property  AutoSpeed   : Boolean
                              read  FAutoSpeed
                              write FAutoSpeed ;
      /// <summary>
      ///   True if the animation should loop.
      /// </summary>
      property  Looped     : Boolean
                             read  FLooped
                             write FLooped ;
      /// <summary>
      ///   Attached 3D viewer.
      /// </summary>
      property  Viewer     : IGIS_Viewer
                             read  FViewer
                             write FViewer ;
      /// <summary>
      ///   Attached 3D viewer.
      /// </summary>
      property  ViewerWnd  : IGIS_ViewerWnd
                             read  FViewerWnd
                             write FViewerWnd ;
    public //events
      /// <summary>
      ///   Indicates the end of a nonlooped animation.
      /// </summary>
      {$IFDEF CLR}
        event    FinishedEvent : TNotifyEvent
                                 delegate FOnFinished ;
      {$ELSE}
        /// <event/>
        property FinishedEvent : TNotifyEvent
                                 read  FOnFinished
                                 write FOnFinished ;
      {$ENDIF}
  end ;


//##############################################################################
implementation

{$IFDEF DCC}
  uses
    System.SysUtils,
    System.Variants,
    System.Math,
    Lider.CG.GIS.GeoClasses,
    Lider.CG.GIS.GeoFunctions,
    Lider.CG.GIS.GeoStreams,
    Lider.CG.GIS.GeoCsFactory,
    Lider.CG.GIS.GeoCsSystems,
    Lider.CG.GIS.GeoXmlSax ;
{$ENDIF}

const
  LOCAL_DEF_PREFIX   : String = 'Camera ' ;

  LOCAL_FIELD_NAME   : String = 'NAME' ;
  LOCAL_FIELD_ACTIVE : String = 'ACTIVE' ;
  LOCAL_FIELD_ROT_X  : String = 'ROT_X' ;
  LOCAL_FIELD_ROT_Y  : String = 'ROT_Y' ;
  LOCAL_FIELD_ROT_Z  : String = 'ROT_Z' ;
  LOCAL_FIELD_REF_X  : String = 'REF_X' ;
  LOCAL_FIELD_REF_Y  : String = 'REF_Y' ;
  LOCAL_FIELD_REF_Z  : String = 'REF_Z' ;

  LOCAL_XML_FLYOVER    : String = 'LicadGIS_FlyOver' ; // ilker deðiþtirme
  LOCAL_XML_SETTINGS   : String = 'Settings' ;
  LOCAL_XML_PATHTYPE   : String = 'PathType' ;
  LOCAL_XML_PATHTYPE_LINEAR
                       : String = 'Linear' ;
  LOCAL_XML_PATHTYPE_CUBICSPLINES
                       : String = 'CubicSplines' ;
  LOCAL_XML_FPS        : String = 'FPS' ;
  LOCAL_XML_TIMESPAN   : String = 'TimeSpan' ;
  LOCAL_XML_AUTOSPEED  : String = 'AutoSpeed' ;
  LOCAL_XML_LOOPED     : String = 'Looped' ;
  LOCAL_XML_CAMERAS    : String = 'Cameras' ;
  LOCAL_XML_CAMERA     : String = 'Camera' ;
  LOCAL_XML_POSITION   : String = 'Position' ;
  LOCAL_XML_POSITIONEX : String = 'PositionEx' ;
  LOCAL_XML_ROTATION   : String = 'Rotation' ;
  LOCAL_XML_REFERENCE  : String = 'Reference' ;
  LOCAL_XML_ATT_NAME   : String = 'name' ;
  LOCAL_XML_ATT_ACTIVE : String = 'active' ;
  LOCAL_XML_YES        : String = 'yes' ;
  LOCAL_XML_NO         : String = 'no' ;
  LOCAL_XML_VERSION    : String = '1.0' ;
  LOCAL_XML_ENCODING   : String = 'UTF-8' ;

  LOCAL_TOLERANCE_DIV  : Double = 1000000.0 ;

type

  T_3DFlyOverContentHandler = class( TGIS_SAXContentHandler )
    private
      camera  : TGIS_3DCamera ;
      segment : Integer ;
      feature : Integer ;
    public
      constructor Create ;
    {$IFNDEF MANAGED}
      public
        destructor Destroy ;  override;
    {$ENDIF}
    public
      procedure StartElement     ( const _uri     : String ;
                                   const _lname   : String ;
                                   const _qname   : String ;
                                   const _attribs : IVBSAXAttributes
                                 ) ; override;
      procedure Characters       ( const _chars   : String
                                 ) ; override;
      procedure EndElement       ( const _uri     : String ;
                                   const _lname   : String ;
                                   const _qname   : String
                                 ) ; override;
    public
      FlyOver : TGIS_3DFlyOver ;
  end ;


  T_3DFlyOverFile = class
    public
      constructor Create ;
    {$IFNDEF MANAGED}
      public
        destructor Destroy ; override;
    {$ENDIF}
    public
      procedure Save ( const _path : String
                     ) ;
      procedure Load ( const _path : String
                     ) ;
    public
      FlyOver : TGIS_3DFlyOver ;
  end ;


//==============================================================================
// T_3DFlyOverContentHandler
//==============================================================================

  constructor T_3DFlyOverContentHandler.Create ;
  begin
    inherited ;

  end ;


  {$IFNDEF MANAGED}
    destructor T_3DFlyOverContentHandler.Destroy ;
    begin

      inherited ;
    end ;
  {$ENDIF}


  procedure T_3DFlyOverContentHandler.StartElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String ;
    const _attribs : IVBSAXAttributes
  ) ;
  begin
    if CompareText( _lname, LOCAL_XML_SETTINGS ) = 0 then begin
      segment := 1 ;
      exit ;
    end
    else
    if CompareText( _lname, LOCAL_XML_CAMERAS ) = 0 then begin
      segment := 2 ;
      exit ;
    end ;

    if segment = 1 then begin

      if CompareText( _lname, LOCAL_XML_PATHTYPE ) = 0 then
        feature := 1
      else
      if CompareText( _lname, LOCAL_XML_FPS ) = 0 then
        feature := 2
      else
      if CompareText( _lname, LOCAL_XML_TIMESPAN ) = 0 then
        feature := 3
      else
      if CompareText( _lname, LOCAL_XML_AUTOSPEED ) = 0 then
        feature := 4
      else
      if CompareText( _lname, LOCAL_XML_LOOPED ) = 0 then
        feature := 5
      else
        feature := 0 ;

    end
    else
    if segment = 2 then begin

      if CompareText( _lname, LOCAL_XML_CAMERA ) = 0 then begin
        camera.Name := _attribs.GetValue( 0 ) ;
        if CompareText( _attribs.GetValue( 1 ), LOCAL_XML_YES ) = 0 then
          camera.Active := True
        else
          camera.Active := False ;
      end
      else
      if CompareText( _lname, LOCAL_XML_POSITION ) = 0 then
        feature := 1
      else
      if CompareText( _lname, LOCAL_XML_POSITIONEX ) = 0 then
        feature := 2
      else
      if CompareText( _lname, LOCAL_XML_ROTATION ) = 0 then
        feature := 3
      else
      if CompareText( _lname, LOCAL_XML_REFERENCE ) = 0 then
        feature := 4
      else
        feature := 0 ;

    end ;
  end ;


  procedure T_3DFlyOverContentHandler.Characters(
    const _chars   : String
  ) ;
  var
    tkn : TGIS_Tokenizer ;
    pt  : TGIS_Point3D ;
  begin
    if segment = 1 then begin

      case feature of
        1 :
          begin
            if CompareText( _chars, LOCAL_XML_PATHTYPE_LINEAR ) = 0 then
              FlyOver.PathType := TGIS_3DPathType.Linear
            else
              FlyOver.PathType := TGIS_3DPathType.CubicSplines ;
          end ;
        2 : FlyOver.FPS := StrToInt( _chars ) ;
        3 : FlyOver.TimeSpan := DotStrToFloat( _chars ) ;
        4 :
          begin
            if CompareText( _chars, LOCAL_XML_YES ) = 0 then
              FlyOver.AutoSpeed := True
            else
              FlyOver.AutoSpeed := False
          end ;
        5 :
          begin
            if CompareText( _chars, LOCAL_XML_YES ) = 0 then
              FlyOver.Looped := True
            else
              FlyOver.Looped := False
          end ;
      end ;

    end
    else
    if segment = 2 then begin

      tkn := TGIS_Tokenizer.Create ;
      try
        tkn.Execute( _chars, [ ' ' ] ) ;
        pt.X := DotStrToFloat( tkn.Result[0] ) ;
        pt.Y := DotStrToFloat( tkn.Result[1] ) ;
        pt.Z := DotStrToFloat( tkn.Result[2] ) ;
        pt.M := DotStrToFloat( tkn.Result[3] ) ;
      finally
        FreeObject( tkn ) ;
      end ;

      case feature of
        1 : camera.Position   := pt ;
        2 : camera.PositionEx := pt ;
        3 : camera.Rotation   := pt ;
        4 : camera.Reference  := pt ;
      end ;

    end ;
  end ;


  procedure T_3DFlyOverContentHandler.EndElement(
    const _uri     : String ;
    const _lname   : String ;
    const _qname   : String
  ) ;
  begin
    if CompareText( _lname, LOCAL_XML_CAMERA ) = 0 then
      FlyOver.AddCamera( camera ) ;
  end ;


//==============================================================================
// T_3DFlyOverFile
//==============================================================================

  constructor T_3DFlyOverFile.Create ;
  begin
    inherited ;

  end ;


  {$IFNDEF MANAGED}
    destructor T_3DFlyOverFile.Destroy ;
    begin

      inherited ;
    end ;
  {$ENDIF}


  procedure T_3DFlyOverFile.Save(
    const _path : String
  ) ;
  var
    strm : TGIS_FileStream ;
    wrtr : TGIS_SAXWriter ;
    atts : TGIS_SAXAttributes ;
    cam  : TGIS_3DCamera ;
    sb   : TStringBuilder ;
    i    : Integer ;
  begin
    if not assigned( FlyOver ) then
      exit ;

    strm := TGIS_FileStream.Create( _path, fmCreate ) ;
    wrtr := TGIS_SAXWriter.Create( strm ) ;
    atts := TGIS_SAXAttributes.Create ;
    sb := TStringBuilder.Create ;
    try
      // start document, XMLDecl and document node
      wrtr.StartDocument ;
      wrtr._XMLDecl( LOCAL_XML_VERSION, LOCAL_XML_ENCODING, LOCAL_XML_YES ) ;
      wrtr.StartElement( '', LOCAL_XML_FLYOVER, LOCAL_XML_FLYOVER, atts ) ;

      // Settings
      wrtr.StartElement( '', LOCAL_XML_SETTINGS, LOCAL_XML_SETTINGS, atts ) ;

        wrtr.StartElement( '', LOCAL_XML_PATHTYPE, LOCAL_XML_PATHTYPE, atts ) ;
        case FlyOver.PathType of
          TGIS_3DPathType.Linear :
            wrtr.Characters( LOCAL_XML_PATHTYPE_LINEAR ) ;
          TGIS_3DPathType.CubicSplines :
            wrtr.Characters( LOCAL_XML_PATHTYPE_CUBICSPLINES ) ;
        end ;
        wrtr.EndElement( '', LOCAL_XML_PATHTYPE, LOCAL_XML_PATHTYPE ) ;

        wrtr.StartElement( '', LOCAL_XML_FPS, LOCAL_XML_FPS, atts ) ;
        wrtr.Characters( IntToStr( FlyOver.FPS ) ) ;
        wrtr.EndElement( '', LOCAL_XML_FPS, LOCAL_XML_FPS ) ;

        wrtr.StartElement( '', LOCAL_XML_TIMESPAN, LOCAL_XML_TIMESPAN, atts ) ;
        wrtr.Characters( DotFloatToStr( FlyOver.TimeSpan ) ) ;
        wrtr.EndElement( '', LOCAL_XML_TIMESPAN, LOCAL_XML_TIMESPAN ) ;

        wrtr.StartElement( '', LOCAL_XML_AUTOSPEED, LOCAL_XML_AUTOSPEED, atts ) ;
        if FlyOver.AutoSpeed then
          wrtr.Characters( UpperCase( LOCAL_XML_YES ) )
        else
          wrtr.Characters( UpperCase( LOCAL_XML_NO ) ) ;
        wrtr.EndElement( '', LOCAL_XML_AUTOSPEED, LOCAL_XML_AUTOSPEED ) ;

        wrtr.StartElement( '', LOCAL_XML_LOOPED, LOCAL_XML_LOOPED, atts ) ;
        if FlyOver.Looped then
          wrtr.Characters( UpperCase( LOCAL_XML_YES ) )
        else
          wrtr.Characters( UpperCase( LOCAL_XML_NO ) ) ;
        wrtr.EndElement( '', LOCAL_XML_LOOPED, LOCAL_XML_LOOPED ) ;

      wrtr.EndElement( '', LOCAL_XML_SETTINGS, LOCAL_XML_SETTINGS ) ;

      // Cameras
      wrtr.StartElement( '', LOCAL_XML_CAMERAS, LOCAL_XML_CAMERAS, atts ) ;

      for i := 0 to FlyOver.Count - 1 do begin
        cam := FlyOver.Camera[i] ;

        atts.Clear ;
        atts.AddAttribute( '', LOCAL_XML_ATT_NAME, LOCAL_XML_ATT_NAME,
                           '', cam.Name ) ;
        if cam.Active then
          atts.AddAttribute( '', LOCAL_XML_ATT_ACTIVE, LOCAL_XML_ATT_ACTIVE,
                             '', LOCAL_XML_YES )
        else
          atts.AddAttribute( '', LOCAL_XML_ATT_ACTIVE, LOCAL_XML_ATT_ACTIVE,
                             '', LOCAL_XML_NO ) ;

        wrtr.StartElement( '', LOCAL_XML_CAMERA, LOCAL_XML_CAMERA, atts ) ;
        atts.Clear ;

          wrtr.StartElement( '', LOCAL_XML_POSITION, LOCAL_XML_POSITION, atts ) ;

            sb.Append( DotFloatToStr( cam.Position.X ) ) ;
            sb.Append( ' ' ) ;
            sb.Append( DotFloatToStr( cam.Position.Y ) ) ;
            sb.Append( ' ' ) ;
            sb.Append( DotFloatToStr( cam.Position.Z ) ) ;
            sb.Append( ' ' ) ;
            sb.Append( DotFloatToStr( cam.Position.M ) ) ;
            wrtr.Characters( sb.ToString ) ;
            sb.Clear ;

          wrtr.EndElement( '', LOCAL_XML_POSITION, LOCAL_XML_POSITION ) ;

          wrtr.StartElement( '', LOCAL_XML_POSITIONEX, LOCAL_XML_POSITIONEX, atts ) ;

            sb.Append( DotFloatToStr( cam.PositionEx.X ) ) ;
            sb.Append( ' ' ) ;
            sb.Append( DotFloatToStr( cam.PositionEx.Y ) ) ;
            sb.Append( ' ' ) ;
            sb.Append( DotFloatToStr( cam.PositionEx.Z ) ) ;
            sb.Append( ' ' ) ;
            sb.Append( DotFloatToStr( cam.PositionEx.M ) ) ;
            wrtr.Characters( sb.ToString ) ;
            sb.Clear ;

          wrtr.EndElement( '', LOCAL_XML_POSITIONEX, LOCAL_XML_POSITIONEX ) ;

          wrtr.StartElement( '', LOCAL_XML_ROTATION, LOCAL_XML_ROTATION, atts ) ;

            sb.Append( DotFloatToStr( cam.Rotation.X ) ) ;
            sb.Append( ' ' ) ;
            sb.Append( DotFloatToStr( cam.Rotation.Y ) ) ;
            sb.Append( ' ' ) ;
            sb.Append( DotFloatToStr( cam.Rotation.Z ) ) ;
            sb.Append( ' ' ) ;
            sb.Append( DotFloatToStr( cam.Rotation.M ) ) ;
            wrtr.Characters( sb.ToString ) ;
            sb.Clear ;

          wrtr.EndElement( '', LOCAL_XML_ROTATION, LOCAL_XML_ROTATION ) ;

          wrtr.StartElement( '', LOCAL_XML_REFERENCE, LOCAL_XML_REFERENCE, atts ) ;

            sb.Append( DotFloatToStr( cam.Reference.X ) ) ;
            sb.Append( ' ' ) ;
            sb.Append( DotFloatToStr( cam.Reference.Y ) ) ;
            sb.Append( ' ' ) ;
            sb.Append( DotFloatToStr( cam.Reference.Z ) ) ;
            sb.Append( ' ' ) ;
            sb.Append( DotFloatToStr( cam.Reference.M ) ) ;
            wrtr.Characters( sb.ToString ) ;
            sb.Clear ;

          wrtr.EndElement( '', LOCAL_XML_REFERENCE, LOCAL_XML_REFERENCE ) ;

        wrtr.EndElement( '', LOCAL_XML_CAMERA, LOCAL_XML_CAMERA ) ;
      end ;

      wrtr.EndElement( '', LOCAL_XML_CAMERAS, LOCAL_XML_CAMERAS ) ;

      // close document node and document
      wrtr.EndElement( '', LOCAL_XML_FLYOVER, LOCAL_XML_FLYOVER ) ;
      wrtr.EndDocument ;
    finally
      FreeObject( sb ) ;
      FreeObject( atts ) ;
      FreeObject( wrtr ) ;
      FreeObject( strm ) ;
    end ;
  end ;


  procedure T_3DFlyOverFile.Load(
    const _path : String
  ) ;
  var
    rdr : T_3DFlyOverContentHandler ;
  begin
    if not assigned( FlyOver ) then
      exit ;

    rdr := T_3DFlyOverContentHandler.Create ;
    try
      rdr.FlyOver := FlyOver ;
      rdr.LoadFromFile( _path ) ;
    finally
      FreeObject( rdr ) ;
    end ;
  end ;


//==============================================================================
// TGIS_3DFlyOver
//==============================================================================

  constructor TGIS_3DFlyOver.Create ;
  begin
    inherited ;

    init ;
  end ;


  procedure TGIS_3DFlyOver.doDestroy ;
  begin
    FreeObject( camList ) ;
    FreeObject( actList ) ;
    FreeObject( spdList ) ;
    FreeObject( camPosEx ) ;
    FreeObject( camRot ) ;
    FreeObject( timer ) ;

    inherited ;
  end ;


  procedure TGIS_3DFlyOver.init ;
  begin
    {$IFDEF DCC}
      camList := TList<TGIS_3DCamera>.Create ;
      actList := TList<TGIS_3DCamera>.Create ;
      spdList := TList<Double>.Create ;
    {$ENDIF}
    {$IFDEF CLR}
      camList := List<TGIS_3DCamera>.Create ;
      actList := List<TGIS_3DCamera>.Create ;
      spdList := List<Double>.Create ;
    {$ENDIF}
    {$IFDEF JAVA}
      camList := ArrayList<TGIS_3DCamera>.Create ;
      actList := ArrayList<TGIS_3DCamera>.Create ;
      spdList := ArrayList<Double>.Create ;
    {$ENDIF}

    timer := TGIS_Timer.Create ;
    timer.Enabled := False ;
    {$IFDEF OXYGENE}
      timer.OnTimer += doStep ;
    {$ELSE}
      timer.OnTimer := doStep ;
    {$ENDIF}

    FPathType := TGIS_3DPathType.CubicSplines ;
    FTimeSpan := 10.0 ;
    FLooped := False ;
    FFPS := 30 ;
    FSpeed := 1.0 ;
    FAutoSpeed := False ;

    time := 0.0 ;
    locked := False ;
  end ;


  procedure TGIS_3DFlyOver.lockUpdates ;
  begin
    ViewerWnd.Viewer3D.LockUpdates ;

    showRef := ViewerWnd.Viewer3D.ShowReferencePoint ;
    ViewerWnd.Viewer3D.ShowReferencePoint := False ;
  end ;


  procedure TGIS_3DFlyOver.unlockUpdates ;
  begin
    ViewerWnd.Viewer3D.ShowReferencePoint := showRef ;

    ViewerWnd.Viewer3D.UnlockUpdates ;
  end ;


  function TGIS_3DFlyOver.ready : Boolean ;
  begin
    Result := False ;

    if not assigned( ViewerWnd ) then
      exit ;

    if not ViewerWnd.View3D then
      exit ;

    if not assigned( camPosEx ) then
      exit ;

    if not camPosEx.Closed then
      exit ;

    Result := True ;
  end ;


  function TGIS_3DFlyOver.fget_Running : Boolean ;
  begin
    Result := timer.Enabled ;
  end ;


  function TGIS_3DFlyOver.fget_Count : Integer ;
  begin
    Result := camList.Count ;
  end ;


  function TGIS_3DFlyOver.fget_Camera(
    const _idx : Integer
  ) : TGIS_3DCamera ;
  begin
    Result := camList[_idx] ;
  end ;


  function TGIS_3DFlyOver.fget_CameraActive(
    const _idx : Integer
  ) : Boolean ;
  begin
    Result := camList[_idx].Active ;
  end ;


  procedure TGIS_3DFlyOver.fset_CameraActive(
    const _idx : Integer ;
    const _val : Boolean
  ) ;
  var
    cam : TGIS_3DCamera ;
  begin
    cam := camList[_idx] ;
    cam.Active := _val ;
    camList[_idx] := cam ;
  end ;


  function TGIS_3DFlyOver.fget_CameraName(
    const _idx : Integer
  ) : String ;
  begin
    Result := camList[_idx].Name ;
  end ;


  procedure TGIS_3DFlyOver.fset_CameraName(
    const _idx : Integer ;
    const _val : String
  ) ;
  var
    cam : TGIS_3DCamera ;
  begin
    cam := camList[_idx] ;
    cam.Name := _val ;
    camList[_idx] := cam ;
  end ;


  function TGIS_3DFlyOver.fget_FPS : Integer ;
  begin
    Result := FFPS ;
  end ;


  procedure TGIS_3DFlyOver.fset_FPS(
    const _val : Integer
  ) ;
  begin
    if ( _val > 0 ) and ( _val < 120 ) then
      FFPS := _val ;
  end ;


  function TGIS_3DFlyOver.fget_TimeSpan : Double ;
  begin
    Result := FTimeSpan ;
  end ;


  procedure TGIS_3DFlyOver.fset_TimeSpan(
    const _val : Double
  ) ;
  begin
    if _val <= 0.0 then
      exit ;

    FTimeSpan := _val ;
  end ;


  function TGIS_3DFlyOver.fget_Speed : Double ;
  begin
    Result := FSpeed ;
  end ;


  procedure TGIS_3DFlyOver.fset_Speed(
    const _val : Double
  ) ;
  begin
    if _val <= 0.0 then
      exit ;

    FSpeed := _val ;
  end ;


  procedure TGIS_3DFlyOver.doStep(
    _sender : TObject
  ) ;
  var
    pt   : TGIS_Point3D ;
    ref  : TGIS_Point3D ;
    rot  : TGIS_Point3D ;
    dst  : Double ;
    cam1 : TGIS_3DCamera ;
    cam2 : TGIS_3DCamera ;
    spd  : Double ;
    t    : Double ;
    i    : Integer ;

    procedure time_step ;
    begin
      if AutoSpeed then
        spd := spdList[i]
      else
        spd := 1.0 ;

      {$IFDEF DCC}
        // 1.4 corrects for total time
        time := time + FSpeed*spd*interval*1.4 ;
      {$ELSE}
        time := time + FSpeed*spd*interval ;
      {$ENDIF}
    end ;

    function eq_pts( const _p1 : TGIS_Point3D ;
                     const _p2 : TGIS_Point3D
                   ) : Boolean ;
    begin
      Result := False ;
      if ( Abs( _p1.X - _p2.X ) <= tolerance ) and
         ( Abs( _p1.Y - _p2.Y ) <= tolerance ) and
         ( Abs( _p1.Z - _p2.Z ) <= tolerance ) then
        Result := True ;
    end ;

  begin
    t := time ;

    i := camPosEx.NextPoint( t ) ;

    if locked then begin
      time_step ;
      exit ;
    end ;

    locked := True ;
    ViewerWnd.Viewer3D.Lock ;

    cam2 := actList[i] ;
    if Looped and ( i = 0 ) then
      cam1 := actList[actList.Count-1]
    else
      cam1 := actList[i-1] ;

    if eq_pts( cam1.PositionEx, cam2.PositionEx ) then
      pt := cam2.PositionEx
    else
      pt := camPosEx.Calculate( t ) ;

    ViewerWnd.Viewer3D.CameraPositionEx := pt ;

    if eq_pts( cam1.Reference, cam2.Reference ) then begin
      ref := cam2.Reference ;
      rot := ViewerWnd.Viewer3D.CameraRotation ;
      dst := Sqrt( Sqr( pt.X - ref.X ) + Sqr( pt.Y - ref.Y ) ) ;
      rot.X := ArcTan2( pt.Z - ref.Z, dst ) ;
      rot.Z := ArcTan2( ref.X - pt.X, ref.Y - pt.Y ) ;
      if rot.Z < 0 then
        rot.Z := rot.Z + 2*Pi ;
      ViewerWnd.Viewer3D.CameraRotation := rot ;
    end
    else begin
      pt := camRot.Calculate( t ) ;
      pt.M := focal ;
      ViewerWnd.Viewer3D.CameraRotation := pt ;
    end ;

    ViewerWnd.Viewer3D.Unlock ;
    locked := False ;

    time_step ;

    if time > camPosEx.Span then begin
      if Looped then
        time := time - camPosEx.Span
      else begin
        Pause ;
        Reset ;
        if assigned( FOnFinished ) then
          FOnFinished( Self ) ;
      end ;
    end ;
  end ;


  procedure TGIS_3DFlyOver.GrabCamera ;
  var
    cam : TGIS_3DCamera ;
  begin
    if not assigned( ViewerWnd ) then
      exit ;

    if not ViewerWnd.View3D then
      exit ;

    {$IFDEF GIS_NORECORDS}
      cam := new TGIS_3DCamera ;
    {$ENDIF}
    cam.Name := LOCAL_DEF_PREFIX + IntToStr( camList.Count+1 ) ;
    cam.Active := True ;
    cam.Position := ViewerWnd.Viewer3D.CameraPosition ;
    cam.PositionEx := ViewerWnd.Viewer3D.CameraPositionEx ;
    cam.Rotation := ViewerWnd.Viewer3D.CameraRotation ;
    cam.Reference := ViewerWnd.Viewer3D.ReferencePoint ;

    AddCamera( cam ) ;
  end ;


  procedure TGIS_3DFlyOver.AddCamera(
    const _cam : TGIS_3DCamera
  ) ;
  var
    ext  : TGIS_Extent ;
    prev : TGIS_3DCamera ;

    function eq_pts( const _p1 : TGIS_Point3D ;
                     const _p2 : TGIS_Point3D
                   ) : Boolean ;
    begin
      Result := False ;
      if ( Abs( _p1.X - _p2.X ) <= tolerance ) and
         ( Abs( _p1.Y - _p2.Y ) <= tolerance ) and
         ( Abs( _p1.Z - _p2.Z ) <= tolerance ) then
        Result := True ;
    end ;

  begin
    if assigned( Viewer ) then begin
      ext := Viewer.Extent ;
      tolerance := Sqrt( Sqr( ext.XMax - ext.XMin ) +
                         Sqr( ext.YMax - ext.YMin ) )/LOCAL_TOLERANCE_DIV ;
    end
    else
      tolerance := 0.0 ;

    if camList.Count > 0 then begin
      prev := camList[camList.Count-1] ;
      if eq_pts( _cam.PositionEx, prev.PositionEx ) and
         eq_pts( _cam.Rotation  , prev.Rotation   ) then
        exit ;
    end ;

    camList.Add( _cam ) ;
  end ;


  procedure TGIS_3DFlyOver.DeleteCamera(
    const _idx : Integer
  ) ;
  begin
    if ( _idx < 0 ) or ( _idx >= camList.Count ) then
      exit ;

    {$IFDEF DCC}
      camList.Delete( _idx ) ;
    {$ENDIF}
    {$IFDEF CLR}
      camList.RemoveAt( _idx ) ;
    {$ENDIF}
    {$IFDEF JAVA}
      camList.Remove( _idx ) ;
    {$ENDIF}
  end ;


  procedure TGIS_3DFlyOver.MoveCamera(
    const _from : Integer ;
    const _to   : Integer
  ) ;
  {$IFDEF CLR}
    var
      cam : TGIS_3DCamera ;
  {$ENDIF}
  begin
    if ( _from < 0 ) or ( _from >= camList.Count ) then
      exit ;

    if ( _to   < 0 ) or ( _to   >= camList.Count ) then
      exit ;

    if _from = _to then
      exit ;

    {$IFDEF DCC}
      camList.Move( _from, _to ) ;
    {$ENDIF}
    {$IFDEF CLR}
      cam := camList[_from] ;
      camList.RemoveAt( _from ) ;
      camList.Insert( _to, cam ) ;
    {$ENDIF}
    {$IFDEF JAVA}
      cam := camList[_from] ;
      camList.Remove( _from ) ;
      camList.Add( _to, cam ) ;
    {$ENDIF}
  end ;


  procedure TGIS_3DFlyOver.MoveCameraUp(
    const _idx : Integer
  ) ;
  begin
    MoveCamera( _idx, _idx-1 ) ;
  end ;


  procedure TGIS_3DFlyOver.MoveCameraDown(
    const _idx : Integer
  ) ;
  begin
    MoveCamera( _idx, _idx+1 ) ;
  end ;


  procedure TGIS_3DFlyOver.Clear ;
  begin
    camList.Clear ;
  end ;


  procedure TGIS_3DFlyOver.PreparePath ;
  var
    dl   : TGIS_ListOfDoubles ;
    dt   : Double ;
    i    : Integer ;

    function dist( const _i : Integer ;
                   const _j : Integer
                 ) : Double ;
    var
      pp0 : TGIS_Point3D ;
      pp1 : TGIS_Point3D ;
    begin
      pp0 := actList[_i].PositionEx ;
      pp1 := actList[_j].PositionEx ;
      Result := GisPoint2Point3D( pp0, pp1 ) ;
    end ;

    procedure add_cam ;
    var
//?      cpe : TGIS_Point3D ;
      bp  : Boolean ;
      br  : Boolean ;
      dd  : Double ;
    begin
//?      cpe := actList[i].PositionEx ;
//?      if Viewer.CS is TGIS_CSGeographicCoordinateSystem then
//?        cpe.Z := 2*PI*cpe.Z/400000 ;
//?      bp := camPosEx.AddPoint( cpe ) ;
      bp := camPosEx.AddPoint( actList[i].PositionEx ) ;
      br := camRot.AddPoint( actList[i].Rotation ) ;

      if bp and br then begin
        camPosEx.DeleteLast ;
        camRot.DeleteLast ;
      end
      else
      if i > 0 then begin
        dd := GisPoint2Point3D( actList[i].Reference, actList[i-1].Reference ) ;
        if camPosEx.LastInterval < dd then
          camPosEx.LastInterval := dd ;
      end ;
    end ;

  begin
    if Running then
      exit ;

    Reset ;

    actList.Clear ;
    for i := 0 to camList.Count - 1 do begin
      if not camList[i].Active then
        continue ;

      actList.Add( camList[i] ) ;
    end ;

    FreeObject( camPosEx ) ;
    FreeObject( camRot ) ;

    case FPathType of
      TGIS_3DPathType.Linear :
        begin
          camPosEx := TGIS_LinearPath.Create ;
          camRot := TGIS_LinearPath.Create ;
        end ;
      TGIS_3DPathType.CubicSplines :
        begin
          camPosEx := TGIS_CubicSplines.Create ;
          camRot := TGIS_CubicSplines.Create ;
        end ;
    end ;

    camPosEx.AllowDuplicates := True ;
    camPosEx.Tolerance := tolerance ;
    camPosEx.Span := 1000.0*FTimeSpan ;
    camPosEx.Looped := Looped ;
    camPosEx.LevelIntervals := True ;

    camRot.AllowDuplicates := True ;
    camRot.Tolerance := 2*Pi/LOCAL_TOLERANCE_DIV ;
    camRot.Span := camPosEx.Span ;
    camRot.Looped := camPosEx.Looped ;
    camRot.LevelIntervals := True ;
    camRot.Rotation := True ;

    for i := 0 to actList.Count - 1 do
      add_cam ;

    camPosEx.Close ;
    camRot.Close( camPosEx ) ;

    if actList.Count < 2 then
      exit ;

    spdList.Clear ;
    dl := TGIS_ListOfDoubles.Create ;
    try
      camPosEx.GetIntervals( dl, dt ) ;

      if Looped then begin
        spdList.Add( ( dl.Count * dl[dl.Count-1] )/dt ) ;
        for i := 1 to dl.Count - 2 do
          spdList.Add( ( dl.Count * dl[i] )/dt ) ;
      end
      else begin
        spdList.Add( 0.0 ) ;
        for i := 1 to dl.Count - 1 do
          spdList.Add( ( dl.Count * dl[i] )/dt ) ;
      end ;
    finally
      FreeObject( dl ) ;
    end ;
  end ;


  procedure TGIS_3DFlyOver.MoveToCamera(
    const _idx : Integer
  ) ;
  begin
    if not ViewerWnd.View3D then
      exit ;

    if Running then
      exit ;

    if locked then
      exit ;

    ViewerWnd.Viewer3D.Lock ;
    ViewerWnd.Viewer3D.CameraPositionEx := camList[_idx].PositionEx ;
    ViewerWnd.Viewer3D.CameraRotation   := camList[_idx].Rotation ;
    ViewerWnd.Viewer3D.Unlock ;
  end ;


  procedure TGIS_3DFlyOver.MoveToStart ;
  begin
    if not ready then
      exit ;

    if Running then
      exit ;

    if actList.Count = 0 then
      exit ;

    if locked then
      exit ;

    ViewerWnd.Viewer3D.Lock ;
    ViewerWnd.Viewer3D.CameraPositionEx := actList[0].PositionEx ;
    ViewerWnd.Viewer3D.CameraRotation   := actList[0].Rotation ;
    ViewerWnd.Viewer3D.Unlock ;
  end ;


  procedure TGIS_3DFlyOver.Start ;
  begin
    if not ready then
      exit ;

    if Running then
      exit ;

    if time > 0.0 then
      exit ;

    if actList.Count < 2 then begin
      MoveToStart ;
      exit ;
    end ;

    focal := ViewerWnd.Viewer3D.CameraRotation.M ;

    lockUpdates ;

    interval := RoundS( 1000.0/FPS ) ;
    timer.Interval := interval ;
    timer.Enabled := True ;
  end ;


  procedure TGIS_3DFlyOver.Stop ;
  begin
    if Running then
      Pause ;

    Reset ;
  end ;


  procedure TGIS_3DFlyOver.Pause ;
  begin
    if not Running then
      exit ;

    timer.Enabled := False ;
    unlockUpdates ;
  end ;


  procedure TGIS_3DFlyOver.Restart ;
  begin
    if not ready then
      exit ;

    if Running then
      exit ;

    if time = 0.0 then
      exit ;

    lockUpdates ;
    timer.Enabled := True ;
  end ;


  procedure TGIS_3DFlyOver.Reset ;
  begin
    if Running then
      exit ;

    time := 0.0 ;
  end ;


  procedure TGIS_3DFlyOver.SaveToLayer(
    const _layer : TGIS_LayerVector
  ) ;
  var
    shp : TGIS_Shape ;
    cam : TGIS_3DCamera ;
    i   : Integer ;
  begin
    if camList.Count = 0 then
      exit ;

    _layer.Open ;

    if _layer.FindField( LOCAL_FIELD_NAME ) < 0 then
      _layer.AddField( LOCAL_FIELD_NAME, TGIS_FieldType.String, 64, 0 ) ;

    if _layer.FindField( LOCAL_FIELD_ACTIVE ) < 0 then
      _layer.AddField( LOCAL_FIELD_ACTIVE, TGIS_FieldType.Boolean, 0, 0 ) ;

    if _layer.FindField( LOCAL_FIELD_ROT_X ) < 0 then
      _layer.AddField( LOCAL_FIELD_ROT_X, TGIS_FieldType.Float, 0, 0 ) ;

    if _layer.FindField( LOCAL_FIELD_ROT_Y ) < 0 then
      _layer.AddField( LOCAL_FIELD_ROT_Y, TGIS_FieldType.Float, 0, 0 ) ;

    if _layer.FindField( LOCAL_FIELD_ROT_Z ) < 0 then
      _layer.AddField( LOCAL_FIELD_ROT_Z, TGIS_FieldType.Float, 0, 0 ) ;

    if _layer.FindField( LOCAL_FIELD_REF_X ) < 0 then
      _layer.AddField( LOCAL_FIELD_REF_X, TGIS_FieldType.Float, 0, 0 ) ;

    if _layer.FindField( LOCAL_FIELD_REF_Y ) < 0 then
      _layer.AddField( LOCAL_FIELD_REF_Y, TGIS_FieldType.Float, 0, 0 ) ;

    if _layer.FindField( LOCAL_FIELD_REF_Z ) < 0 then
      _layer.AddField( LOCAL_FIELD_REF_Z, TGIS_FieldType.Float, 0, 0 ) ;

    for i := 0 to camList.Count - 1 do begin

      cam := camList[i] ;

      shp := _layer.CreateShape(
               TGIS_ShapeType.Point,
               TGIS_DimensionType.XYZM
             ) ;
      shp.AddPart ;

      shp.AddPoint3D( cam.PositionEx ) ;

      shp.SetField( LOCAL_FIELD_NAME, cam.Name ) ;

      shp.SetField( LOCAL_FIELD_ACTIVE, cam.Active ) ;

      shp.SetField( LOCAL_FIELD_ROT_X, cam.Rotation.X ) ;
      shp.SetField( LOCAL_FIELD_ROT_Y, cam.Rotation.Y ) ;
      shp.SetField( LOCAL_FIELD_ROT_Z, cam.Rotation.Z ) ;

      shp.SetField( LOCAL_FIELD_REF_X, cam.Reference.X ) ;
      shp.SetField( LOCAL_FIELD_REF_Y, cam.Reference.Y ) ;
      shp.SetField( LOCAL_FIELD_REF_Z, cam.Reference.Z ) ;

    end ;

  end ;


  procedure TGIS_3DFlyOver.LoadFromLayer(
    const _layer : TGIS_LayerVector
  ) ;
  var
    en  : TGIS_LayerVectorEnumerator ;
    shp : TGIS_Shape ;
    cam : TGIS_3DCamera ;
    pt  : TGIS_Point3D ;
  begin
    Clear ;

    _layer.Open ;

    en := _layer.Loop.GetEnumerator ;
    while en.MoveNext do begin

      shp := en.GetCurrent ;

      {$IFDEF GIS_NORECORDS}
        cam := new TGIS_3DCamera ;
      {$ENDIF}

      cam.PositionEx := shp.GetPoint3D( 0, 0 ) ;

      cam.Name := VarToString( shp.GetField( LOCAL_FIELD_NAME ) ) ;

      cam.Active := VarToBoolean( shp.GetField( LOCAL_FIELD_ACTIVE ) ) ;

      pt.X := VarToDouble( shp.GetField( LOCAL_FIELD_ROT_X ) ) ;
      pt.Y := VarToDouble( shp.GetField( LOCAL_FIELD_ROT_Y ) ) ;
      pt.Z := VarToDouble( shp.GetField( LOCAL_FIELD_ROT_Z ) ) ;
      cam.Rotation := pt ;

      pt.X := VarToDouble( shp.GetField( LOCAL_FIELD_REF_X ) ) ;
      pt.Y := VarToDouble( shp.GetField( LOCAL_FIELD_REF_Y ) ) ;
      pt.Z := VarToDouble( shp.GetField( LOCAL_FIELD_REF_Z ) ) ;
      cam.Reference := pt ;

      AddCamera( cam ) ;

    end ;

  end ;


  procedure TGIS_3DFlyOver.SaveToFile(
    const _path : String
  ) ;
  var
    f : T_3DFlyOverFile ;
  begin
    if camList.Count = 0 then
      exit ;

    f := T_3DFlyOverFile.Create ;
    try
      f.FlyOver := Self ;
      f.Save( _path ) ;
    finally
      FreeObject( f ) ;
    end ;
  end ;


  procedure TGIS_3DFlyOver.LoadFromFile(
    const _path : String
  ) ;
  var
    f : T_3DFlyOverFile ;
  begin
    Clear ;

    f := T_3DFlyOverFile.Create ;
    try
      f.FlyOver := Self ;
      f.Load( _path ) ;
    finally
      FreeObject( f ) ;
    end ;
  end ;


//==================================== END =====================================
end.

