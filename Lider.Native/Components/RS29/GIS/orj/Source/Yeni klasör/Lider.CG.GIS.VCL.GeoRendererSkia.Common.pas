//=============================================================================
// This source code is a part of TatukGIS Developer Kernel.
// DKv100.1.37476
// (c)2000-2025 TatukGIS. ALL RIGHTS RESERVED.
//
// This file is uniquely watermarked for licensed user:
//    ILKER#LIDERYAZILIM.COM-481078-KSVX7UYN-1D12B8B5
// Any unauthorized use this file can be traced back to the licensed user,
// who may be held accountable.
//=============================================================================
{
  Skia based renderer. Stub file if SKIA is not declared.
}

unit VCL.GisRendererSkia.Common ;
{$HPPEMIT '#pragma link "VCL.GisRendererSkia.Common"'}

interface

uses
  GisRtl ;

type
  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  ///   OpenGL connection object for SKIA.
  /// </summary>
  TGIS_OpenGLSkiaConnection = class( TGIS_BaseObjectDisposable )
      /// <summary>
      ///   Do nothing.
      /// </summary>
      /// <param name="_width">
      ///   canvas width
      /// </param>
      /// <param name="_height">
      ///   canvas height
      /// </param>
      /// <returns>
      ///   nothing
      /// </returns>
      function  CreateCanvas ( const _width  : Integer ;
                               const _height : Integer
                             ) : TObject ; virtual; abstract;

      /// <summary>
      ///   Do nothing.
      /// </summary>
      /// <param name="_canvas">
      ///   canvas to free
      /// </param>
      procedure FreeCanvas   ( var   _canvas : TObject
                             ) ; virtual; abstract;

      /// <summary>
      ///   Do nothing.
      /// </summary>
      procedure Flush        ; virtual; abstract;
  end ;

type
  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  ///   OpenGL connection factory.
  /// </summary>
  TGIS_OpenGLSkiaConnectionFactory = class
    public
      /// <summary>
      ///   Create connection object base don Windows handle.
      /// </summary>
      /// <param name="_handle">
      ///   Windows handle on which OpenGL must be created.
      /// </param>
      /// <returns>
      ///   OpenGL Connection object.
      /// </returns>
      function CreateConnection( const _handle : THandle
                               ) : TGIS_OpenGLSkiaConnection; virtual; abstract;
  end;

var
  {#gendoc:hide:GENXDK}
  {#gendoc:hide:GENSCR}
  /// <summary>
  ///   Global pointer to OpenGL connection factory. Not nil only if SKIA were enabled
  /// </summary>
  OpenGLSkiaConnectionFactory : TGIS_OpenGLSkiaConnectionFactory;

implementation

initialization
  OpenGLSkiaConnectionFactory := nil;

//==================================== END ====================================
end.


