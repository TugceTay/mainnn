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
  Procedures to read LAS.
}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFileLAS ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFileLAS"'}
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

interface

{$IFDEF CLR}
  uses
    TatukGIS.RTL ;
{$ENDIF}
{$IFDEF DCC}
  uses
    System.Classes,
    System.SysUtils,

    Lider.CG.GIS.GeoTypes,
    Lider.CG.GIS.GeoRtl,
    Lider.CG.GIS.GeoCsSystems;
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
  ///   The class which encapsulates the reading of LAS files.
  /// </summary>
  TGIS_FileLAS = {$IFDEF OXYGENE} public {$ENDIF} class( TGIS_ObjectDisposable )
    private
      FPath   : String ;
      FReader : TObject ;
    protected

      /// <summary>
      ///   Destroy an instance.
      /// </summary>
      procedure doDestroy ; override;
    public // internal

      /// <summary>
      ///   Create and open a new file.
      /// </summary>
      /// <param name="_path">
      ///   path to a file
      /// </param>
      constructor Create                  ( const _path   : String
                                          ) ;

      /// <summary>
      ///   Open a file.
      /// </summary>
      /// <returns>
      ///    True if file can be opened
      /// </returns>
      function  Open                      : Boolean ;

      /// <summary>
      ///   Move to first point.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <param name="_extent">
      ///   extent to process
      /// </param>
      procedure MoveFirst                 ( const _cursor : Integer ;
                                            const _extent : TGIS_Extent
                                          ) ;

      /// <summary>
      ///   Move to next point.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    True if there is next point
      /// </returns>
      function  MoveNext                  ( const _cursor : Integer
                                          ): Boolean ;

      /// <summary>
      ///   Get point data.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    point
      /// </returns>
      function  GetPoint                  ( const _cursor : Integer
                                          ): TGIS_Point3D ;

      /// <summary>
      ///   Locate point in stream based on uid.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <param name="_uid">
      ///   point uid
      /// </param>
      /// <returns>
      ///    True if point was found
      /// </returns>
      function  LocatePoint               ( const _cursor : Integer ;
                                            const _uid    : TGIS_Uid
                                          ) : Boolean ;

      /// <summary>
      ///   Get file extent.
      /// </summary>
      /// <returns>
      ///    file extent
      /// </returns>
      function  Extent                    : TGIS_Extent3D ;

      /// <summary>
      ///   Get coordinate system.
      /// </summary>
      /// <returns>
      ///    file coordinate system
      /// </returns>
      function  CS                        : TGIS_CSCoordinateSystem ;

      /// <summary>
      ///   Check if we have eof in stream.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    True if eof is reached
      /// </returns>
      function  Eof                       ( const _cursor : Integer
                                          ) : Boolean ;

      /// <summary>
      ///   Allocate new cursor. Cursor should be closes by calling
      ///   CursorClose().
      /// </summary>
      /// <param name="_cursor">
      ///   cursor to be opened
      /// </param>
      procedure CursorOpen                ( const _cursor : Integer
                                          ) ;

      /// <summary>
      ///   Free cursor allocated by CursorOpen().
      /// </summary>
      /// <param name="_cursor">
      ///   cursor allocated by CursorOpen()
      /// </param>
      procedure CursorClose               ( const _cursor : Integer
                                          ) ;

      /// <summary>
      ///   Get info about a file.
      /// </summary>
      /// <returns>
      ///    file info text
      /// </returns>
      function  GetInfo                   : String ;

      /// <summary>
      ///   Check if points have color RGB values.
      /// </summary>
      /// <returns>
      ///    True if point has color
      /// </returns>
      function  HasPointColor             : Boolean ;
    public

      /// <summary>
      ///   Get point property.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    point info text
      /// </returns>
      function  GetPointInfo              ( const _cursor : Integer
                                          ) : String ;

      /// <summary>
      ///   Get number of points in file.
      /// </summary>
      /// <returns>
      ///    number of points
      /// </returns>
      function  GetPointsCount            : Integer ;

      /// <summary>
      ///   Get current point index.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    number of points
      /// </returns>
      function  GetPointIndex            ( const _cursor : Integer
                                         ) : TGIS_Uid ;

      /// <summary>
      ///   Get point property.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    point color
      /// </returns>
      function  GetPointColor             ( const _cursor : Integer
                                          ) : Cardinal ;

      /// <summary>
      ///   Get point property.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    point classification
      /// </returns>
      function  GetPointClassification    ( const _cursor : Integer
                                          ) : Cardinal ;

      /// <summary>
      ///   Get point property.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    point Intensity
      /// </returns>
      function  GetPointIntensity         ( const _cursor : Integer
                                          ) : Word ;

      /// <summary>
      ///   Get point property.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    point ReturnNumber
      /// </returns>
      function  GetPointReturnNumber      ( const _cursor : Integer
                                          ) : Byte ;

      /// <summary>
      ///   Get point property.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    point NumberOfReturns
      /// </returns>
      function  GetPointNumberOfReturns   ( const _cursor : Integer
                                          ) : Byte ;

      /// <summary>
      ///   Get point property.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    point ScanDirectionFla
      /// </returns>
      function  GetPointScanDirectionFlag ( const _cursor : Integer
                                          ) : Byte ;

      /// <summary>
      ///   Get point property.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    point EdgeOfFlightLine
      /// </returns>
      function  GetPointEdgeOfFlightLine  ( const _cursor : Integer
                                          ) : Byte ;

      /// <summary>
      ///   Get point property.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    point ScanAngleRank
      /// </returns>
      function  GetPointScanAngleRank     ( const _cursor : Integer
                                          ) : SmallInt ;

      /// <summary>
      ///   Get point property.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    point GPSTime
      /// </returns>
      function  GetPointGPSTime           ( const _cursor : Integer
                                          ) : Double ;

      /// <summary>
      ///   Get point property.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    point ClassificationFlag
      /// </returns>
      function  GetPointClassificationFlag( const _cursor : Integer
                                          ) : Byte ;

      /// <summary>
      ///   Get point property.
      /// </summary>
      /// <param name="_cursor">
      ///   cursor id
      /// </param>
      /// <returns>
      ///    point ScannerChannel
      /// </returns>
      function  GetPointScannerChannel    ( const _cursor : Integer
                                          ) : Byte ;
    public

      /// <summary>
      ///   Check if file can be opened.
      /// </summary>
      /// <returns>
      ///    True if file is supported
      /// </returns>
      function PreRecognize               : Boolean ;
    public
      /// <summary>
      ///   File path.
      /// </summary>
      property Path                       : String read FPath ;
  end ;

  {$IFDEF JAVA}
    {$IFNDEF ANDROID}
    ByReference nested in laszip_header
      = public class(
          laszip_header, com.sun.jna.Structure.ByReference
        )
    end;

    ByValue nested in laszip_header
      = public class(
          laszip_header, com.sun.jna.Structure.ByValue
        )
    end;

    {#gendoc:hide}
    laszip_header = public class( com.sun.jna.Structure )
      public
        var file_source_ID                                  : laszip_U16;
        var global_encoding                                 : laszip_U16;
        var project_ID_GUID_data_1                          : laszip_U32;
        var project_ID_GUID_data_2                          : laszip_U16;
        var project_ID_GUID_data_3                          : laszip_U16;
        var project_ID_GUID_data_4                          : array[0..7] of  laszip_CHAR;
        var version_major                                   : laszip_U8;
        var version_minor                                   : laszip_U8;
        var system_identifier                               : array[0..31] of  laszip_CHAR;
        var generating_software                             : array[0..31] of  laszip_CHAR;
        var file_creation_day                               : laszip_U16;
        var file_creation_year                              : laszip_U16;
        var header_size                                     : laszip_U16;
        var offset_to_point_data                            : laszip_U32;
        var number_of_variable_length_records               : laszip_U32;
        var point_data_format                               : laszip_U8;
        var point_data_record_length                        : laszip_U16;
        var number_of_point_records                         : laszip_U32;
        var number_of_points_by_return                      : array[0..4] of  laszip_U32;
        var x_scale_factor                                  : laszip_F64;
        var y_scale_factor                                  : laszip_F64;
        var z_scale_factor                                  : laszip_F64;
        var x_offset                                        : laszip_F64;
        var y_offset                                        : laszip_F64;
        var z_offset                                        : laszip_F64;
        var max_x                                           : laszip_F64;
        var min_x                                           : laszip_F64;
        var max_y                                           : laszip_F64;
        var min_y                                           : laszip_F64;
        var max_z                                           : laszip_F64;
        var min_z                                           : laszip_F64;
        var start_of_waveform_data_packet_record            : laszip_U64;
        var start_of_first_extended_variable_length_record  : laszip_U64;
        var number_of_extended_variable_length_records      : laszip_U32;
        var extended_number_of_point_records                : laszip_U64;
        var extended_number_of_points_by_return             : array[0..14] of  laszip_U64;
        var user_data_in_header_size                        : laszip_U32;
        var user_data_in_header                             : laszip_PTR;
        var vlrs                                            : laszip_PTR;
        var user_data_after_header_size                     : laszip_U32;
        var user_data_after_header                          : laszip_PTR;
      public
        constructor Create ;
        constructor Create( _peer : com.sun.jna.Pointer ) ;
      protected
        method getFieldOrder: java.util.List<String>; override;
        property FieldOrder: java.util.List<String> read getFieldOrder ;
    end ;

    ByReference nested in laszip_vlr
      = public class(
          laszip_vlr, com.sun.jna.Structure.ByReference
        )
    end;

    ByValue nested in laszip_vlr
      = public class(
          laszip_vlr, com.sun.jna.Structure.ByValue
        )
    end;

    {#gendoc:hide}
    laszip_vlr = public class( com.sun.jna.Structure )
      public
        var reserved                    : laszip_U16 ;
        var user_id                     : array[0..15] of laszip_CHAR ;
        var record_id                   : laszip_U16 ;
        var record_length_after_header  : laszip_U16 ;
        var description                 : array[0..31] of laszip_CHAR ;
        var data                        : laszip_PTR ;
      public
        constructor Create ;
        constructor Create( _peer : com.sun.jna.Pointer ) ;
      protected
        method getFieldOrder: java.util.List<String>; override;
        property FieldOrder: java.util.List<String> read getFieldOrder ;
    end ;

    ByReference nested in laszip_point
      = public class(
          laszip_point, com.sun.jna.Structure.ByReference
        )
    end;

    ByValue nested in laszip_point
      = public class(
          laszip_point, com.sun.jna.Structure.ByValue
        )
    end;

    {#gendoc:hide}
    laszip_point = public class( com.sun.jna.Structure )
      public
        var X                       : laszip_I32;
        var Y                       : laszip_I32;
        var Z                       : laszip_I32;
        var intensity               : laszip_U16;
        var &flags                  : laszip_U8;
        var flags_c                 : laszip_U8 ;
        var scan_angle_rank         : laszip_I8;
        var user_data               : laszip_U8 ;
        var point_source_ID         : laszip_U16;
        var extended_scan_angle     : laszip_I16;
        var extended_flags_point    : laszip_U8 ;
        var extended_classification : laszip_U8;
        var extended_flags_return   : laszip_U8;
        var dummy                   : array[0..6] of laszip_U8;
        var gps_time                : laszip_F64;
        var rgb                     : array[0..3] of laszip_U16;
        var wave_packet             : array[0..28] of laszip_U8;
        var num_extra_bytes         : laszip_I32;
        var extra_bytes             : laszip_PTR ;
      public
        constructor Create ;
        constructor Create( _peer : com.sun.jna.Pointer ) ;
      protected
        method getFieldOrder: java.util.List<String>; override;
        property FieldOrder: java.util.List<String> read getFieldOrder ;
    end ;
    {$ENDIF}
  {$ENDIF}

//##############################################################################
implementation

{$IFDEF OXYGENE}
  {$IFDEF CLR}
  uses
    System.Security,
    System.IO,
    System.Runtime.InteropServices ;
  {$ENDIF}
{$ELSE}
uses
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
  {$ENDIF}
  System.Math,

  Lider.CG.GIS.GeoFunctions,
  Lider.CG.GIS.GeoInternals,
  Lider.CG.GIS.GeoClasses,
  Lider.CG.GIS.GeoStreams,
  Lider.CG.GIS.GeoResource,
  Lider.CG.GIS.GeoCsBase,
  Lider.CG.GIS.GeoCsProjections,
  Lider.CG.GIS.GeoCsFactory ;
{$ENDIF}

type

  // PUBLIC HEADER BLOCK
  TLAS_PublicHeaderBlock = class
    public
      // version 1.0-1.2
      FileSignature                             : TBytes ;
      FileSourceID                              : Word ;
      GlobalEncoding                            : Word ;
      ProjectIDGUIDdata1                        : Cardinal ;
      ProjectIDGUIDdata2                        : Word ;
      ProjectIDGUIDdata3                        : Word ;
      ProjectIDGUIDdata4                        : array [0..7] of Byte ;
      VersionMajor                              : Byte ;
      VersionMinor                              : Byte ;
      SystemIdentifier                          : TBytes ;
      GeneratingSoftware                        : TBytes ;
      FileCreationDayofYear                     : Word ;
      FileCreationYear                          : Word ;
      HeaderSize                                : Word ;
      OffsetToPointData                         : Cardinal ;
      NumberofVariableLengthRecords             : Cardinal ;
      PointDataFormatID                         : Byte ;
      PointDataRecordLength                     : Word ;
      Numberofpointrecords                      : Cardinal ;
      Numberofpointsbyreturn                    : array [0..4] of Cardinal ;
      Xscalefactor                              : Double ;
      Yscalefactor                              : Double ;
      Zscalefactor                              : Double ;
      Xoffset                                   : Double ;
      Yoffset                                   : Double ;
      Zoffset                                   : Double ;
      MaxX                                      : Double ;
      MinX                                      : Double ;
      MaxY                                      : Double ;
      MinY                                      : Double ;
      MaxZ                                      : Double ;
      MinZ                                      : Double ;
      // version 1.3
      StartofWaveformDataPacketRecord           : UInt64 ;
      // version 1.4
      StartoffirstExtendedVariableLengthRecord  : UInt64 ;
      NumberofExtendedVariableLengthRecords     : Cardinal ;
      ExNumberofpointrecords                    : UInt64 ;
      ExNumberofpointsbyreturn                  : array [0..14] of UInt64 ;
      // extra
      UserData                                  : TBytes ;
    public
      // Constructor.
      constructor Create ;

      // Clean header.
      procedure Clean ;

      // Read header.
      // _stream file stream handle
      procedure Read  ( const _stream : TGIS_Stream
                       ) ;

      // Check header data.
      // return True if header is valid
      function  Check   : Boolean ;

      // Get info.
      // return info text
      function  GetInfo : String ;
  end ;

  // VLR Key Entry.
  TLAS_VLRKeyEntry = record
    KeyID           : Word ;
    TIFFTagLocation : Integer ;
    Count           : Word ;
    ValueOffset     : Word ;
  end ;

  // VLR Geo Keys.
  TLAS_VLRGeoKeys = class
    public
      KeyDirectoryVersion : Word ;
      KeyRevision         : Word ;
      MinorRevision       : Word ;
      NumberOfKeys        : Word ;
      FKeys               : array of TLAS_VLRKeyEntry ;
    public

      // Read geo keys.
      // _data  buffer
      procedure Read  ( const _data : TBytes
                       ) ;
      // Get geo keys info.
      // text info
      function GetInfo : String ;
  end ;

  // VLR Classification.
  TLAS_VLRClassification = record
    ClassNumber : Byte ;
    Description : TBytes ;
  end ;

  // Variable Length Record definition.
  TLAS_VariableLengthRecord = class( TGIS_Object )
    public
      RecordSignatureAABB     : Word ;
      UserID                  : TBytes ;
      RecordID                : Word ;
      RecordLengthAfterHeader : UInt64 ;
      Description             : TBytes ;
      Data                    : TBytes ;
      GeoKeys                 : TLAS_VLRGeoKeys ;
      GeoDoubleParams         : TBytes ;
      GeoAsciiParams          : TBytes ;
      OGSCSWKT                : TBytes ;
      OGSMTWKT                : TBytes ;
      Classification          : TLAS_VLRClassification ;
    protected
      // Destroy an instance.
      procedure doDestroy ; override;

    public
      // Create an instance.
      constructor Create ;

      // Read vlr.
      // _stream      file stream handle
      // _extended    if True, evlr definition will be read
      procedure Read  ( const _stream   : TGIS_Stream ;
                        const _extended : Boolean
                       ) ;
      // Get vlr info.
      // return text info
      function  GetInfo : String ;

      // Parse geo keys.
      procedure ParseGeoKeys ;
  end ;

  // Point data record format.
  TLAS_PointDataRecordFormat = class
    public
      X               : Integer ;
      Y               : Integer ;
      Z               : Integer ;
      Intensity       : Word ;
    public
      // Read point record.
      // _stream  file stream handle
      procedure Read    ( const _stream   : TGIS_Stream
                        ) ; virtual;

      // Get Point
      // _xscale
      // _yscale
      // _zscale
      // _xoffset
      // _yoffset
      // _zoffset
      // return point
      function  GetPoint( const _xscale   : Double ;
                          const _yscale   : Double ;
                          const _zscale   : Double ;
                          const _xoffset  : Double ;
                          const _yoffset  : Double ;
                          const _zoffset  : Double
                         ) : TGIS_Point3D ; virtual;
      // Get Color
      function  GetColor                    : Cardinal ; virtual;

      // Get Info
      function  GetInfo                     : String ; virtual;

      // Get Classification
      function  GetClassification           : Byte ; virtual;

      // Get Intensity
      function  GetIntensity                : Word ; virtual;

      // Get Return Number
      function  GetReturnNumber             : Byte ; virtual;

      // Get Number Of Returns
      function  GetNumberOfReturns          : Byte ; virtual;

      // Get Scan Direction Flag
      function  GetScanDirectionFlag        : Byte ; virtual;

      // Get Edge Of Flight Line
      function  GetEdgeOfFlightLine         : Byte ; virtual;

      // Get Scan Angle Rank
      function  GetScanAngleRank            : SmallInt ; virtual;

      // Get GPSTime
      function  GetGPSTime                  : Double ; virtual;

      // Get Classification Flag
      function  GetClassificationFlag       : Byte ; virtual;

      // Get Scanner Channel
      function  GetScannerChannel           : Byte ; virtual;
  end ;

  // Point data record 0 format.
  TLAS_PointDataRecordFormat0 = class( TLAS_PointDataRecordFormat )
      Flags           : Byte ;
      Classification  : Byte;
      ScanAngleRank   : ShortInt ;
      UserData        : Byte ;
      PointSourceID   : Word ;
    public

      // Read point record.
      // _stream  file stream handle
      procedure Read( const _stream : TGIS_Stream
                            ) ; override;

      // Get Classification
      function  GetClassification         : Byte ; override;

      // Get Info
      function  GetInfo                           : String ; override;

      // Get Return Number
      function  GetReturnNumber             : Byte ; override;

      // Get Number Of Returns
      function  GetNumberOfReturns        : Byte ; override;

      // Get Scan Direction Flag
      function  GetScanDirectionFlag    : Byte ; override;

      // Get Edge Of Flight Line
      function  GetEdgeOfFlightLine     : Byte ; override;

      // Get Scan Angle Rank
      function  GetScanAngleRank          : SmallInt ; override;
  end ;

  // Point data record 1 format.
  TLAS_PointDataRecordFormat1 = class( TLAS_PointDataRecordFormat0 )
    public
      GPSTime : Double ;
    public

      // Read point record.
      // _stream  file stream handle
      procedure Read( const _stream : TGIS_Stream
                     ) ; override;

      // Get Info
      function  GetInfo         : String ; override;

      // Get GPSTime
      function  GetGPSTime      : Double ; override;
  end ;

  // Point data record 2 format.
  TLAS_PointDataRecordFormat2 = class( TLAS_PointDataRecordFormat0 )
    public
      Red   : Word ;
      Green : Word ;
      Blue  : Word ;
    public

      // Read point record.
      // _stream  file stream handle
      procedure Read( const _stream : TGIS_Stream
                    ) ; override;

      // Get Color
      function  GetColor        : Cardinal ; override;

      // Get Info
      function  GetInfo         : String ; override;
  end ;

  // Point data record 3 format.
  TLAS_PointDataRecordFormat3 = class( TLAS_PointDataRecordFormat0 )
    public
      GPSTime : Double ;
      Red     : Word ;
      Green   : Word ;
      Blue    : Word ;
    public

      // Read point record.
      // _stream  file stream handle
      procedure Read( const _stream : TGIS_Stream
                     ) ; override;

      // Get Color
      function  GetColor        : Cardinal ; override;

      // Get Info
      function  GetInfo         : String ; override;
  end ;

  // Point data record 4 format.
  TLAS_PointDataRecordFormat4 = class( TLAS_PointDataRecordFormat1 )
    public
      WavePacketDescriptorIndex   : Byte ;
      Byteoffsettowaveformdata    : UInt64 ;
      Waveformpacketsizeinbytes   : Cardinal ;
      ReturnPointWaveformLocation : Single ;
      Xt                          : Single ;
      Yt                          : Single ;
      Zt                          : Single ;
    public

      // Read point record.
      // _stream  file stream handle
      procedure Read( const _stream : TGIS_Stream
                    ) ; override;

      // Get Info
      function  GetInfo : String ; override;
  end ;

  // Point data record 5 format.
  TLAS_PointDataRecordFormat5 = class( TLAS_PointDataRecordFormat3 )
    public
      WavePacketDescriptorIndex   : Byte ;
      Byteoffsettowaveformdata    : UInt64 ;
      Waveformpacketsizeinbytes   : Cardinal ;
      ReturnPointWaveformLocation : Single ;
      Xt                          : Single ;
      Yt                          : Single ;
      Zt                          : Single ;
    public

      // Read point record.
      // _stream  file stream handle
      procedure Read( const _stream : TGIS_Stream
                    ) ; override;

      // Get Info
      function  GetInfo : String ; override;
  end ;

  // Point data record 6 format.
  TLAS_PointDataRecordFormat6 = class( TLAS_PointDataRecordFormat )
    public
      Flags           : Word ;
      Classification  : Byte;
      UserData        : Byte ;
      ScanAngle       : SmallInt ;
      PointSourceID   : Word ;
      GPSTime         : Double ;
    public

      // Read point record.
      // _stream  file stream handle
      procedure Read( const _stream : TGIS_Stream
                    ) ; override;

      // Get Classification
      function  GetClassification : Byte ; override;

      // Get Info
      function  GetInfo                 : String ; override;

      // Get Return Number
      function  GetReturnNumber         : Byte ; override;

      // Get Number Of Returns
      function  GetNumberOfReturns      : Byte ; override;

      // Get Scan Direction Flag
      function  GetScanDirectionFlag    : Byte ; override;

      // Get Edge Of Flight Line
      function  GetEdgeOfFlightLine     : Byte ; override;

      // Get Scan Angle Rank
      function  GetScanAngleRank        : SmallInt ; override;

      // Get Classification
      function  GetClassificationFlag   : Byte ; override;
      function  GetScannerChannel       : Byte ; override;

      // Get GPSTime
      function  GetGPSTime              : Double ; override;
  end ;

  // Point data record 7 format.
  TLAS_PointDataRecordFormat7 = class( TLAS_PointDataRecordFormat6 )
    public
      Red   : Word ;
      Green : Word ;
      Blue  : Word ;
    public

      // Read point record.
      procedure Read( const _stream : TGIS_Stream
                    ) ; override;

      // Get Color
      function  GetColor        : Cardinal ; override;

      // Get Info
      function  GetInfo         : String ; override;
  end ;

  // Point data record 8 format.
  TLAS_PointDataRecordFormat8 = class( TLAS_PointDataRecordFormat7 )
    public
      NIR   : Word ;
    public

      // Read point record.
      // _stream  file stream handle
      procedure Read( const _stream : TGIS_Stream
                     ) ; override;

      // Get Color
      function  GetColor        : Cardinal ; override;

      // Get Info
      function  GetInfo         : String ; override;
  end ;

  // Point data record 9 format.
  TLAS_PointDataRecordFormat9 = class( TLAS_PointDataRecordFormat6 )
    public
      WavePacketDescriptorIndex   : Byte ;
      Byteoffsettowaveformdata    : UInt64 ;
      Waveformpacketsizeinbytes   : Cardinal ;
      ReturnPointWaveformLocation : Single ;
      Xt                          : Single ;
      Yt                          : Single ;
      Zt                          : Single ;
    public

      // Read point record.
      // _stream  file stream handle
      procedure Read( const _stream : TGIS_Stream
                    ) ; override;

      // Get Info
      function  GetInfo : String ; override;
  end ;

  // Point data record 10 format.
  TLAS_PointDataRecordFormat10 = class( TLAS_PointDataRecordFormat8 )
    public
      WavePacketDescriptorIndex   : Byte ;
      Byteoffsettowaveformdata    : UInt64 ;
      Waveformpacketsizeinbytes   : Cardinal ;
      ReturnPointWaveformLocation : Single ;
      Xt                          : Single ;
      Yt                          : Single ;
      Zt                          : Single ;
    public

      // Read point record.
      // _stream  file stream handle
      procedure Read( const _stream : TGIS_Stream
                    ) ; override;

      // Get Info
      function  GetInfo : String ; override;
  end ;

  {$IFDEF OXYGENE}
    T_cursorStateLAS nested in TLAS_Reader = record
      public

        // Is cursor in use.
        curInUse    : Boolean ;

        // Current file offset.
        curPosition : Int64 ;

        // Current file offset.
        curPointIndex : TGIS_Uid ;

        // Current point.
        curPoint    : TLAS_PointDataRecordFormat ;
    end ;
  {$ENDIF}

//==============================================================================
// LASZip API wrappers
//==============================================================================

const
  LASZIP_DLL_NAME_NOEXT = 'LASzip' ;
  {$IFDEF MSWINDOWS_OS}
    LASZIP_DLL_NAME       = 'LASzip.dll' ;
  {$ENDIF}
  {$IFDEF ANDROID_OS}
    LASZIP_DLL_NAME = 'liblaszip.so';
  {$ENDIF}
  {$IFDEF MACOSX_OS}
    {$IFDEF IOS_OS}
      {$IFDEF IOS_DEVICE}
        LASZIP_DLL_NAME = 'liblaszip.a';
      {$ELSE}
        LASZIP_DLL_NAME = 'liblaszip.dylib';
      {$ENDIF}
    {$ELSE}
      LASZIP_DLL_NAME = 'liblaszip.dylib';
    {$ENDIF}
  {$ENDIF}
  {$IFDEF LINUX_OS}
    LASZIP_DLL_NAME = 'liblaszip.so';
  {$ENDIF}
  {$IFDEF UNKNOWN_OS}
    LASZIP_DLL_NAME = '?';
  {$ENDIF}

var
  FLASLib            : TObject = nil;
  FLASInstanceCount  : Integer = 0 ;

type
  {$IFNDEF OXYGENE}
    {#GENDOC:HIDE}
    laszip_PTR = Pointer ;
  {$ELSE}
    {#GENDOC:HIDE}
    {$IFDEF JAVA}
      {$IFDEF ANDROID}
        laszip_PTR  = IntPtr ;
      {$ELSE}
        laszip_PTR  = com.sun.jna.Pointer ;
      {$ENDIF}
    {$ELSE}
      laszip_PTR = IntPtr ;
    {$ENDIF}
  {$ENDIF}

  {#GENDOC:HIDE}
  laszip_BOOL = Integer ;
  {#GENDOC:HIDE}
  laszip_U8   = Byte ;
  {#GENDOC:HIDE}
  laszip_U16  = Word ;
  {#GENDOC:HIDE}
  laszip_U32  = Cardinal ;
  {#GENDOC:HIDE}
  laszip_U64  = UInt64 ;
  {#GENDOC:HIDE}
  laszip_I8   = ShortInt ;
  {#GENDOC:HIDE}
  laszip_I16  = SmallInt ;
  {#GENDOC:HIDE}
  laszip_I32  = Integer ;
  {#GENDOC:HIDE}
  laszip_I64  = Int64 ;
  {#GENDOC:HIDE}
  laszip_CHAR = Byte ;
  {#GENDOC:HIDE}
  laszip_F32  = Single ;
  {#GENDOC:HIDE}
  laszip_F64  = Double ;

  {$IFDEF JAVA}

  {$ELSE}
    {$IFDEF CLR}
      [StructLayout(LayoutKind.Sequential)]
    {$ENDIF}
    laszip_vlr = record
      reserved                    : laszip_U16 ;
      {$IFDEF CLR}[MarshalAs(UnmanagedType.ByValArray, SizeConst=16)]{$ENDIF}
      user_id                     : array[0..15] of laszip_CHAR ;
      record_id                   : laszip_U16 ;
      record_length_after_header  : laszip_U16 ;
      {$IFDEF CLR}[MarshalAs(UnmanagedType.ByValArray, SizeConst=32)]{$ENDIF}
      description                 : array[0..31] of laszip_CHAR ;
      data                        : laszip_PTR ;
    end ;
  {$ENDIF}
  {$IFDEF OXYGENE}
  {$ELSE}
    plaszip_vlr = ^laszip_vlr ;
  {$ENDIF}

  {$IFDEF JAVA}

  {$ELSE}
    {$IFDEF CLR}
      [StructLayout(LayoutKind.Sequential)]
    {$ENDIF}
    laszip_header = record
    public
      file_source_ID                                  : laszip_U16;
      global_encoding                                 : laszip_U16;
      project_ID_GUID_data_1                          : laszip_U32;
      project_ID_GUID_data_2                          : laszip_U16;
      project_ID_GUID_data_3                          : laszip_U16;
      {$IFDEF CLR}[MarshalAs(UnmanagedType.ByValArray, SizeConst=8)]{$ENDIF}
      project_ID_GUID_data_4                          : array[0..7] of  laszip_CHAR;
      version_major                                   : laszip_U8;
      version_minor                                   : laszip_U8;
      {$IFDEF CLR}[MarshalAs(UnmanagedType.ByValArray, SizeConst=32)]{$ENDIF}
      system_identifier                               : array[0..31] of  laszip_CHAR;
      {$IFDEF CLR}[MarshalAs(UnmanagedType.ByValArray, SizeConst=32)]{$ENDIF}
      generating_software                             : array[0..31] of  laszip_CHAR;
      file_creation_day                               : laszip_U16;
      file_creation_year                              : laszip_U16;
      header_size                                     : laszip_U16;
      offset_to_point_data                            : laszip_U32;
      number_of_variable_length_records               : laszip_U32;
      point_data_format                               : laszip_U8;
      point_data_record_length                        : laszip_U16;
      number_of_point_records                         : laszip_U32;
      {$IFDEF CLR}[MarshalAs(UnmanagedType.ByValArray, SizeConst=5)]{$ENDIF}
      number_of_points_by_return                      : array[0..4] of  laszip_U32;
      x_scale_factor                                  : laszip_F64;
      y_scale_factor                                  : laszip_F64;
      z_scale_factor                                  : laszip_F64;
      x_offset                                        : laszip_F64;
      y_offset                                        : laszip_F64;
      z_offset                                        : laszip_F64;
      max_x                                           : laszip_F64;
      min_x                                           : laszip_F64;
      max_y                                           : laszip_F64;
      min_y                                           : laszip_F64;
      max_z                                           : laszip_F64;
      min_z                                           : laszip_F64;
      start_of_waveform_data_packet_record            : laszip_U64;
      start_of_first_extended_variable_length_record  : laszip_U64;
      number_of_extended_variable_length_records      : laszip_U32;
      extended_number_of_point_records                : laszip_U64;
      {$IFDEF CLR}[MarshalAs(UnmanagedType.ByValArray, SizeConst=15)]{$ENDIF}
      extended_number_of_points_by_return             : array[0..14] of  laszip_U64;
      user_data_in_header_size                        : laszip_U32;
      user_data_in_header                             : laszip_PTR;
      vlrs                                            : laszip_PTR;
      user_data_after_header_size                     : laszip_U32;
      user_data_after_header                          : laszip_PTR;
    end ;
  {$ENDIF}
  {$IFDEF OXYGENE}
  {$ELSE}
    plaszip_header = ^laszip_header ;
  {$ENDIF}

  {$IFDEF JAVA}

  {$ELSE}
    {$IFDEF CLR}
      [StructLayout(LayoutKind.Sequential)]
    {$ENDIF}
    laszip_point = record
      X                       : laszip_I32;
      Y                       : laszip_I32;
      Z                       : laszip_I32;
      intensity               : laszip_U16;
      flags                   : laszip_U8;
      flags_c                 : laszip_U8 ;
      //laszip_U8 return_number : 3;
      //laszip_U8 number_of_returns : 3;
      //laszip_U8 scan_direction_flag : 1;
      //laszip_U8 edge_of_flight_line : 1;
      //laszip_U8 classification : 5;
      //laszip_U8 synthetic_flag : 1;
      //laszip_U8 keypoint_flag  : 1;
      //laszip_U8 withheld_flag  : 1;
      scan_angle_rank         : laszip_I8;
      user_data               : laszip_U8 ;
      point_source_ID         : laszip_U16;
      extended_scan_angle     : laszip_I16;
      extended_flags_point    : laszip_U8 ;
      //laszip_U8 extended_point_type : 2;
      //laszip_U8 extended_scanner_channel : 2;
      //laszip_U8 extended_classification_flags : 4;
      extended_classification : laszip_U8;
      extended_flags_return   : laszip_U8;
      //laszip_U8 extended_return_number : 4;
      //laszip_U8 extended_number_of_returns : 4;
      {$IFDEF CLR}[MarshalAs(UnmanagedType.ByValArray, SizeConst=7)]{$ENDIF}
      dummy                   : array[0..6] of laszip_U8;
      gps_time                : laszip_F64;
      {$IFDEF CLR}[MarshalAs(UnmanagedType.ByValArray, SizeConst=4)]{$ENDIF}
      rgb                     : array[0..3] of laszip_U16;
      {$IFDEF CLR}[MarshalAs(UnmanagedType.ByValArray, SizeConst=29)]{$ENDIF}
      wave_packet             : array[0..28] of laszip_U8;
      num_extra_bytes         : laszip_I32;
      extra_bytes             : laszip_PTR ;
    end ;
  {$ENDIF}
  {$IFDEF OXYGENE}
  {$ELSE}
    plaszip_point = ^laszip_point ;
  {$ENDIF}

  // Point data record Laz format.
  {$IFNDEF ANDROID}
  TLAS_PointDataRecordFormatLaz = class( TLAS_PointDataRecordFormat )
      {$IFDEF OXYGENE}
        ptr    : laszip_PTR ;
        ppoint : laszip_point ;
      {$ELSE}
        ppoint : plaszip_point ;
      {$ENDIF}
    private
      procedure marshalPoint ;
    public
      constructor Create( const _ptr : laszip_PTR ) ;

      // Read point record.
      // _stream  file stream handle
      procedure Read( const _stream : TGIS_Stream
                            ) ; override;

      // Get Point
      // _xscale
      // _yscale
      // _zscale
      // _xoffset
      // _yoffset
      // _zoffset
      // return point
      function  GetPoint( const _xscale   : Double ;
                          const _yscale   : Double ;
                          const _zscale   : Double ;
                          const _xoffset  : Double ;
                          const _yoffset  : Double ;
                          const _zoffset  : Double
                         ) : TGIS_Point3D ; override;
      // Get Color
      function  GetColor                    : Cardinal ; override;

      // Get Info
      function  GetInfo                     : String ; override;

      // Get Classification
      function  GetClassification           : Byte ; override;

      // Get Intensity
      function  GetIntensity                : Word ; override;

      // Get Return Number
      function  GetReturnNumber             : Byte ; override;

      // Get Number Of Returns
      function  GetNumberOfReturns          : Byte ; override;

      // Get Scan Direction Flag
      function  GetScanDirectionFlag        : Byte ; override;

      // Get Edge Of Flight Line
      function  GetEdgeOfFlightLine         : Byte ; override;

      // Get Scan Angle Rank
      function  GetScanAngleRank            : SmallInt ; override;

      // Get GPSTime
      function  GetGPSTime                  : Double ; override;

      // Get Classification Flag
      function  GetClassificationFlag       : Byte ; override;

      // Get Scanner Channel
      function  GetScannerChannel           : Byte ; override;
  end ;
  {$ENDIF}

  {$IFDEF JAVA}
    {$IFNDEF ANDROID}
    ILASZipLibrary = interface (com.sun.jna.Library)
        function laszip_get_version       ( _version_major      : com.sun.jna.ptr.ByteByReference ;
                                            _version_minor      : com.sun.jna.ptr.ByteByReference ;
                                            _version_revision   : com.sun.jna.ptr.IntByReference ;
                                            _version_build      : com.sun.jna.ptr.IntByReference
                                           ) : laszip_I32 ;
        function laszip_create            ( _cPointer           : com.sun.jna.ptr.PointerByReference
                                          ) : laszip_I32 ;
        function laszip_destroy           ( _cPointer           : laszip_PTR
                                          ) : laszip_I32 ;
        function laszip_get_error         ( _cPointer           : laszip_PTR;
                                            _error              : com.sun.jna.ptr.PointerByReference
                                          ) : laszip_I32 ;
        function laszip_open_reader       ( _cPointer           : laszip_PTR ;
                                            _file_name          : String ;
                                            _is_compressed      : com.sun.jna.ptr.IntByReference
                                          ) : laszip_I32 ;
        function laszip_close_reader      ( _cPointer           : laszip_PTR
                                          ) : laszip_I32 ;
        function laszip_get_header_pointer( _cPointer           : laszip_PTR ;
                                            _header_pointer     : com.sun.jna.ptr.PointerByReference
                                          ) : laszip_I32 ;
        function laszip_get_point_pointer ( _cPointer           : laszip_PTR ;
                                            _point_pointer      : com.sun.jna.ptr.PointerByReference
                                          ) : laszip_I32 ;
        function laszip_read_point        ( _cPointer           : laszip_PTR
                                          ) : laszip_I32 ;
        function laszip_seek_point        ( _cPointer           : laszip_PTR;
                                            _index              : laszip_I64
                                          ) : laszip_I32 ;
        function laszip_read_inside_point ( _cPointer           : laszip_PTR ;
                                            _is_done            : com.sun.jna.ptr.IntByReference
                                          ) : laszip_I32 ;
        function laszip_inside_rectangle  ( _cPointer           : laszip_PTR ;
                                            _min_x              : laszip_F64 ;
                                            _min_y              : laszip_F64 ;
                                            _max_x              : laszip_F64 ;
                                            _max_y              : laszip_F64 ;
                                            _is_empty           : com.sun.jna.ptr.IntByReference
                                          ) : laszip_I32 ;
        function laszip_get_point_count   ( _cPointer           : laszip_PTR;
                                            _count              : com.sun.jna.ptr.IntByReference
                                          ) : laszip_I32 ;
        function laszip_exploit_spatial_index( _cPointer        : laszip_PTR ;
                                               _exploit         : laszip_BOOL
                                              ) : laszip_I32 ;
    end ;
    {$ENDIF}
  {$ENDIF}

  {$IFNDEF ANDROID}
  T_LASZipLib = {$IFDEF OXYGENE} static {$ENDIF}class
    private
      {$IFDEF JAVA}
        DLLHandle : ILASZipLibrary ;
      {$ELSE}
        DLLHandle : THandle ;
      {$ENDIF}
      DLLLoaded : Boolean ;
      PReader   : laszip_PTR ;
      PHeader   : laszip_PTR ;
      PPoint    : laszip_PTR ;
      IsDone    : laszip_BOOL ;
    public
      {$IFDEF CLR}
        [ SuppressUnmanagedCodeSecurity,
          DllImport( LASZIP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function laszip_get_version       ( var _version_major   : laszip_U8 ;
                                            var _version_minor   : laszip_U8 ;
                                            var _version_revision: laszip_U16 ;
                                            var _version_build   : laszip_U32
                                          ) : laszip_I32 ; external ;
        [ SuppressUnmanagedCodeSecurity,
          DllImport( LASZIP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function laszip_create            ( var _cPointer       : laszip_PTR
                                          ) : laszip_I32 ; external ;
        [ SuppressUnmanagedCodeSecurity,
          DllImport( LASZIP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function laszip_destroy           ( _cPointer           : laszip_PTR
                                          ) : laszip_I32 ; external ;
        [ SuppressUnmanagedCodeSecurity,
          DllImport( LASZIP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function laszip_get_error         (     _cPointer       : laszip_PTR;
                                            var _error          : laszip_PTR
                                          ) : laszip_I32 ; external ;
        [ SuppressUnmanagedCodeSecurity,
          DllImport( LASZIP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function laszip_open_reader       (     _cPointer       : laszip_PTR ;
                                                _file_name      : String ;
                                            var _is_compressed  : laszip_BOOL
                                          ) : laszip_I32 ; external ;
        [ SuppressUnmanagedCodeSecurity,
          DllImport( LASZIP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function laszip_close_reader      ( _cPointer           : laszip_PTR
                                          ) : laszip_I32 ; external ;
        [ SuppressUnmanagedCodeSecurity,
          DllImport( LASZIP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function laszip_get_header_pointer(     _cPointer       : laszip_PTR ;
                                            var _header_pointer : laszip_PTR
                                          ) : laszip_I32 ; external ;
        [ SuppressUnmanagedCodeSecurity,
          DllImport( LASZIP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function laszip_get_point_pointer (     _cPointer       : laszip_PTR ;
                                            var _point_pointer  : laszip_PTR
                                          ) : laszip_I32 ; external ;
        [ SuppressUnmanagedCodeSecurity,
          DllImport( LASZIP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function laszip_read_point        ( _cPointer           : laszip_PTR
                                          ) : laszip_I32 ; external ;
        [ SuppressUnmanagedCodeSecurity,
          DllImport( LASZIP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function laszip_seek_point        ( _cPointer           : laszip_PTR ;
                                            _index              : laszip_I64
                                          ) : laszip_I32 ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( LASZIP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function laszip_read_inside_point (    _cPointer        : laszip_PTR ;
                                           var _is_done         : laszip_BOOL
                                          ) : laszip_I32 ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( LASZIP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function laszip_inside_rectangle  (     _cPointer       : laszip_PTR ;
                                                _min_x          : laszip_F64 ;
                                                _min_y          : laszip_F64 ;
                                                _max_x          : laszip_F64 ;
                                                _max_y          : laszip_F64 ;
                                            var _is_empty       : laszip_BOOL
                                          ) : laszip_I32 ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( LASZIP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function laszip_exploit_spatial_index(  _cPointer       : laszip_PTR ;
                                                _exploit        : laszip_BOOL
                                              ) : laszip_I32 ; external ;

        [ SuppressUnmanagedCodeSecurity,
          DllImport( LASZIP_DLL_NAME, CallingConvention = CallingConvention.Cdecl,
                     CharSet = CharSet.Ansi
                    )
        ]
        function laszip_get_point_count      (
                                                   _cPointer    : laszip_PTR;
                                               var _count       : laszip_I64
                                             ) : laszip_I32 ; external ;
        {$ENDIF}
      {$IFDEF DCC}
        laszip_get_version          : function (
                                        var _version_major   : laszip_U8 ;
                                        var _version_minor   : laszip_U8 ;
                                        var _version_revision: laszip_U16 ;
                                        var _version_build   : laszip_U32
                                      ) : laszip_I32 ; cdecl ;
        laszip_create               : function (
                                        var _cPointer       : laszip_PTR
                                      ) : laszip_I32 ; cdecl ;
        laszip_destroy              : function (
                                        _cPointer : laszip_PTR
                                      ) : laszip_I32 ; cdecl ;
        laszip_get_error            : function (
                                            _cPointer       : laszip_PTR;
                                        var _error          : laszip_PTR
                                      ) : laszip_I32 ; cdecl ;
        laszip_open_reader          : function (
                                            _cPointer       : laszip_PTR ;
                                            _file_name      : laszip_PTR ;
                                        var _is_compressed  : laszip_BOOL
                                      ) : laszip_I32 ; cdecl ;
        laszip_close_reader         : function (
                                        _cPointer : laszip_PTR
                                      ) : laszip_I32 ; cdecl ;
        laszip_get_header_pointer   : function (
                                            _cPointer       : laszip_PTR ;
                                        var _header_pointer : laszip_PTR
                                      ) : laszip_I32 ; cdecl ;
        laszip_get_point_pointer    : function (
                                            _cPointer       : laszip_PTR ;
                                        var _point_pointer  : laszip_PTR
                                      ) : laszip_I32 ; cdecl ;
        laszip_read_point           : function (
                                        _cPointer           : laszip_PTR
                                      ) : laszip_I32 ; cdecl ;
        laszip_seek_point           : function (
                                        _cPointer           : laszip_PTR ;
                                        _index              : laszip_I64
                                      ) : laszip_I32 ; cdecl ;
        laszip_read_inside_point    : function (
                                            _cPointer       : laszip_PTR ;
                                        var _is_done        : laszip_BOOL
                                      ) : laszip_I32 ; cdecl ;
        laszip_inside_rectangle     : function (
                                            _cPointer       : laszip_PTR ;
                                            _min_x          : laszip_F64 ;
                                            _min_y          : laszip_F64 ;
                                            _max_x          : laszip_F64 ;
                                            _max_y          : laszip_F64 ;
                                        var _is_empty       : laszip_BOOL
                                      ) : laszip_I32 ; cdecl ;
        laszip_exploit_spatial_index: function (
                                        _cPointer           : laszip_PTR ;
                                        _exploit            : laszip_BOOL
                                      ) : laszip_I32 ; cdecl ;
        laszip_get_point_count      : function (
                                            _cPointer       : laszip_PTR;
                                        var _count          : laszip_I64
                                      ) : laszip_I32 ; cdecl ;
      {$ENDIF}
      {$IFDEF JAVA}
        {$IFNDEF ANDROID}
        function laszip_get_version       ( var _version_major   : laszip_U8 ;
                                            var _version_minor   : laszip_U8 ;
                                            var _version_revision: laszip_U16 ;
                                            var _version_build   : laszip_U32
                                           ) : laszip_I32 ;
        function laszip_create            ( var _cPointer       : laszip_PTR
                                          ) : laszip_I32 ;
        function laszip_destroy           ( _cPointer           : laszip_PTR
                                          ) : laszip_I32 ;
        function laszip_get_error         (     _cPointer       : laszip_PTR;
                                            var _error          : laszip_PTR
                                          ) : laszip_I32 ;
        function laszip_open_reader       (     _cPointer       : laszip_PTR ;
                                                _file_name      : String ;
                                            var _is_compressed  : laszip_BOOL
                                          ) : laszip_I32 ;
        function laszip_close_reader      ( _cPointer           : laszip_PTR
                                          ) : laszip_I32 ;
        function laszip_get_header_pointer(     _cPointer       : laszip_PTR ;
                                            var _header_pointer : laszip_PTR
                                          ) : laszip_I32 ;
        function laszip_get_point_pointer (     _cPointer       : laszip_PTR ;
                                            var _point_pointer  : laszip_PTR
                                          ) : laszip_I32 ;
        function laszip_read_point        ( _cPointer           : laszip_PTR
                                          ) : laszip_I32 ;
        function laszip_seek_point        ( _cPointer           : laszip_PTR;
                                            _index              : laszip_I64
                                          ) : laszip_I32 ;
        function laszip_read_inside_point (    _cPointer        : laszip_PTR ;
                                           var _is_done         : laszip_BOOL
                                          ) : laszip_I32 ;
        function laszip_inside_rectangle  (     _cPointer       : laszip_PTR ;
                                                _min_x          : laszip_F64 ;
                                                _min_y          : laszip_F64 ;
                                                _max_x          : laszip_F64 ;
                                                _max_y          : laszip_F64 ;
                                            var _is_empty       : laszip_BOOL
                                          ) : laszip_I32 ;
        function laszip_exploit_spatial_index(  _cPointer       : laszip_PTR ;
                                                _exploit        : laszip_BOOL
                                              ) : laszip_I32 ;
        function laszip_get_point_count      (
                                                   _cPointer    : laszip_PTR;
                                               var _count       : laszip_I64
                                             ) : laszip_I32 ;
        {$ENDIF}
      {$ENDIF}
    {$IFNDEF OXYGENE} private {$ELSE} unit {$ENDIF}
      function check( const _status : Integer ) : String ;
    public
      constructor Create ;
      {$IFNDEF OXYGENE}
        destructor Destroy ; override;
      {$ENDIF}

      function  LoadDLL           ( const _dllPath : String
                                  ) : Boolean ;
      function  UnLoadDLL         : Boolean ;
      function CreateReader       : Integer ;
      function EnableSpatialIndex : Integer ;
      function OpenReader         ( const _path : String
                                  ) : Integer ;
      function ReadHeader         ( const _header : TLAS_PublicHeaderBlock ;
                                    const _fvlr   : TGIS_ObjectList
                                  ) : Integer ;
      function SeekPoint          ( const _point : Integer
                                  ) : Integer ;
      function InitPointPtr       : Integer ;
      function ReadPoint          : Integer ;
      function ReadPointInside    : Integer ;
      function ReadEof            : Boolean ;
      function SetRectangle       ( const _extent : TGIS_Extent
                                  ) : Integer ;
      function GetPointPtr        : laszip_PTR ;
      function GetPointIndex      : Int64 ;
  end ;
  {$ENDIF}

//==============================================================================

  // LAS reader.
  TLAS_Reader = class( TGIS_ObjectDisposable )
    private
      FHeader     : TLAS_PublicHeaderBlock ;
      FVLR        : TGIS_ObjectList ;
      FStream     : TGIS_BufferedStream ;
      FMaxPOffset : Int64 ;
      FMaxPoints  : Int64 ;
      FIsLaz      : Boolean ;
      FIsLazInit  : Boolean ;
      FPath       : String ;
    private
      {$IFDEF OXYGENE}
        cursorState : array of T_cursorStateLAS ;
      {$ELSE}
        cursorState   : array of record

          // Is cursor in use.
          curInUse      : Boolean ;

          // Current file offset.
          curPosition   : Int64 ;

          // Current file offset.
          curPointIndex : TGIS_Uid ;

          // Current point.
          curPoint      : TLAS_PointDataRecordFormat ;
        end ;
      {$ENDIF}
    private
      procedure check           ( const _status : Integer
                                ) ;

      // Prepare coordinate system.
      // _vlr
      function prepareCS        ( const _vlr          : TLAS_VLRGeoKeys ;
                                  const _asciiparams  : String          ;
                                  const _doubleparams : TGIS_DoubleArray
                                ) : TGIS_CSCoordinateSystem ;
      // Prepare a point.
      // _cursor  cursor id
      procedure preparePoint    ( const _cursor : Integer
                                ) ;
    protected
      procedure doDestroy ; override;
    public
      constructor Create                                  ( const _path   : String
                                                          ) ;

      // Open a cursor.
      // _cursor  cursor id
      procedure CursorOpen                                ( const _cursor : Integer
                                                          ) ;

      // Open a cursor.
      // _cursor  cursor id
      procedure CursorClose                               ( const _cursor : Integer
                                                          ) ;

      // Open a reader.
      // return True if succeeded
      function  Open                                      : Boolean ;

      // Move to first point.
      // _cursor  cursor id
      procedure MoveFirst                                 ( const _cursor : Integer ;
                                                            const _extent : TGIS_Extent
                                                          ) ;

      // Move to next point.
      // _cursor  cursor id
      function  MoveNext                                  ( const _cursor : Integer
                                                          ) : Boolean ;

      // Get point.
      // _cursor  cursor id
      // return   point data
      function  GetPoint                                  ( const _cursor : Integer
                                                          ) : TGIS_Point3D ;

      // Locate point in stream based on uid
      // _cursor  cursor id
      // _uid     point uid
      // return   True if found
      function  LocatePoint                               ( const _cursor : Integer ;
                                                            const _uid    : TGIS_Uid
                                                          ) : Boolean ;

      // Get reader extent
      function  GetExtent                                 : TGIS_Extent3D ;

      // Get coordinate system
      function  GetCS                                     : TGIS_CSCoordinateSystem ;

      // Do we have eof and must stop reading.
      // _cursor  cursor id
      function  GetEof                                    ( const _cursor : Integer
                                                          ) : Boolean ;

      // Get reader info
      function  GetInfo                                   : String ;

      // Get point property
      // _cursor  cursor id
      function  GetPointInfo                              ( const _cursor : Integer
                                                          ) : String ;

      // Get point property
      // _cursor  cursor id
      function  GetPointsCount                            : Integer ;

      // Get point property
      // _cursor  cursor id
      function  GetPointIndex                            ( const _cursor : Integer
                                                          ) : TGIS_Uid ;

      // Get point property
      // _cursor  cursor id
      function  GetPointColor                             ( const _cursor : Integer
                                                          ) : Cardinal ;

      // Get point property
      // _cursor  cursor id
      function  HasPointColor                             : Boolean ;

      // Get point property
      // _cursor  cursor id
      function  GetPointClassification                    ( const _cursor : Integer
                                                          ) : Cardinal ;

      // Get point property
      // _cursor  cursor id
      function  GetPointIntensity                         ( const _cursor : Integer
                                                          ) : Word ;

      // Get point property
      // _cursor  cursor id
      function  GetPointReturnNumber                      ( const _cursor : Integer
                                                          ) : Byte ;

      // Get point property
      // _cursor  cursor id
      function  GetPointNumberOfReturns                   ( const _cursor : Integer
                                                          ) : Byte ;
      function  GetPointScanDirectionFlag                 ( const _cursor : Integer
                                                          ) : Byte ;

      // Get point property
      // _cursor  cursor id
      function  GetPointEdgeOfFlightLine                  ( const _cursor : Integer
                                                          ) : Byte ;

      // Get point property
      // _cursor  cursor id
      function  GetPointScanAngleRank                     ( const _cursor : Integer
                                                          )  : SmallInt ;

      // Get point property
      // _cursor  cursor id
      function  GetPointGPSTime                           ( const _cursor : Integer
                                                          ) : Double ;

      // Get point property
      // _cursor  cursor id
      function  GetPointClassificationFlag                ( const _cursor : Integer
                                                          ) : Byte ;

      // Get point property
      // _cursor  cursor id
      function  GetPointScannerChannel                    ( const _cursor : Integer
                                                          ) : Byte ;
  end ;

  // Convert LAS color to RGB.
  function convertLASColor(
    const _r, _g, _b : Word
  ) : Cardinal ;
  var
    r, g, b : Word ;
  begin
    if _r > 255 then begin
      r := Integer(_r) shr 8 ;
      if r > 255 then
        r := 255 ;
    end
    else
      r := _r ;

    if _g > 255 then begin
      g := Integer(_g) shr 8 ;
      if g > 255 then
        g := 255 ;
    end
    else
      g := _g ;

    if _b > 255 then begin
      b := Integer(_b) shr 8 ;
      if b > 255 then
        b := 255 ;
    end
    else
      b := _b ;

    {$IFDEF DCC}
      Result := (r or (g shl 8) or (b shl 16));
    {$ELSE}
      Result := RGB( r, g, b ) and $ffffff;
    {$ENDIF}
  end ;

  function decodeString(
    const _buf : TBytes
  ) : String ;
  begin
    Result := TEncoding.UTF8.GetString( _buf ) ;
  end ;

//==============================================================================
// helper functions
//==============================================================================

  { Convert string buffer to string }
 {$IFDEF CLR}
    function asLASString( const _buf : IntPtr ) : String ;
    begin
      Result := Marshal.PtrToStringAnsi( _buf ) ;
    end ;
 {$ENDIF}
 {$IFDEF JAVA}
    {$IFNDEF ANDROID}
    function asLASString( const _buf : laszip_PTR ) : String ;
    begin
      Result := _buf.getString(0) ; ;
    end ;
    {$ENDIF}
 {$ENDIF}
 {$IFDEF DCC}
    {$IFNDEF ANDROID}
    function asLASString( const _buf : PAnsiChar ) : String ;
    begin
      Result := String( AnsiString( _buf ) ) ;
    end ;
   {$ENDIF}
 {$ENDIF}


//==============================================================================
// TLAS_PublicHeaderBlock
//==============================================================================

  constructor TLAS_PublicHeaderBlock.Create ;
  begin
    inherited ;
  end ;

  function TLAS_PublicHeaderBlock.GetInfo: String ;
  var
    i : Integer ;
  begin
    Result := 'Header : ' + #13#10 +
      Format( ' FileSignature : %s', [ ConvertAnsiString( FileSignature ) ] ) +
              #13#10 +
      Format( ' FileSourceID : %d', [Integer(FileSourceID)] ) + #13#10 +
      Format( ' GlobalEncoding : %d', [Integer(GlobalEncoding)] ) + #13#10 +
      Format( ' ProjectIDGUID : %u-%d-%d-x', [Integer(ProjectIDGUIDdata1),
              Integer(ProjectIDGUIDdata2), Integer(ProjectIDGUIDdata3) ] ) + #13#10 +
      Format( ' VersionMajor : %d.%d', [Integer(VersionMajor), Integer(VersionMinor)] ) + #13#10 +
      Format( ' SystemIdentifier : %s ',
              [ConvertAnsiString(SystemIdentifier)] ) + #13#10 +
      Format( ' GeneratingSoftware : %s ',
              [ConvertAnsiString(GeneratingSoftware)] ) + #13#10 +
      Format( ' FileCreationDayofYear : %d/%d',
              [Integer(FileCreationDayofYear),Integer(FileCreationYear)] ) + #13#10 +
      Format( ' NumberofVariableLengthRecords : %u',
              [Integer(NumberofVariableLengthRecords)] ) + #13#10 +
      Format( ' PointDataFormatID : %d', [Integer(PointDataFormatID)] ) + #13#10 +
      Format( ' Numberofpointrecords : %u', [Integer(Numberofpointrecords)] ) + #13#10 +
      ' Numberofpointsbyreturn : ' ;
      for i := low(Numberofpointsbyreturn) to high(Numberofpointsbyreturn) do
        Result := Result + IntToStr(Numberofpointsbyreturn[i]) + ' ' ;
      Result := Result + #13#10 +
      Format( ' Xscalefactor : %f', [Xscalefactor] ) + #13#10 +
      Format( ' Yscalefactor : %f', [Yscalefactor] ) + #13#10 +
      Format( ' Zscalefactor : %f', [Zscalefactor] ) + #13#10 +
      Format( ' Xoffset : %f', [Xoffset] ) + #13#10 +
      Format( ' Yoffset : %f', [Yoffset] ) + #13#10 +
      Format( ' Zoffset : %f', [Zoffset] ) + #13#10 +
      Format( ' MinX MinY MinZ : %f %f %f', [MinX, MinY, MinZ] ) + #13#10 +
      Format( ' MaxX MaxY MaxZ : %f %f %f', [MaxX, MaxY, MaxZ] ) ;
  end ;

  function TLAS_PublicHeaderBlock.Check : Boolean ;
  begin
    Result := False ;
    if ConvertAnsiString( FileSignature, 4 ) <> 'LASF' then exit ;
    if ( VersionMajor <> 1 ) or ( VersionMinor > 4 ) then exit ;
    if ( HeaderSize < 227 ) then exit ;
    if ( OffsetToPointData < HeaderSize ) then exit ;

    assert( Xscalefactor <> 0, 'x scale factor is zero' ) ;
    assert( Yscalefactor <> 0, 'y scale factor is zero' ) ;
    assert( Zscalefactor <> 0, 'z scale factor is zero' ) ;
    assert( (MaxX >= MinX) or (MaxY >= MinY) or (MaxZ >= MinZ), 'invalid bounding box' ) ;

    Result := True ;
  end ;

  procedure TLAS_PublicHeaderBlock.Clean ;
  begin
    SetLength( FileSignature, 4 ) ;
    FileSourceID                              := 0 ;
    GlobalEncoding                            := 0 ;
    ProjectIDGUIDdata1                        := 0 ;
    ProjectIDGUIDdata2                        := 0 ;
    ProjectIDGUIDdata3                        := 0 ;
    VersionMajor                              := 0 ;
    VersionMinor                              := 0 ;
    SetLength( SystemIdentifier, 32 ) ;
    SetLength( GeneratingSoftware, 32 ) ;
    FileCreationDayofYear                     := 0 ;
    FileCreationYear                          := 0 ;
    HeaderSize                                := 0 ;
    OffsetToPointData                         := 0 ;
    NumberofVariableLengthRecords             := 0 ;
    PointDataFormatID                         := 0 ;
    PointDataRecordLength                     := 0 ;
    Numberofpointrecords                      := 0 ;
    Xscalefactor                              := 0 ;
    Yscalefactor                              := 0 ;
    Zscalefactor                              := 0 ;
    Xoffset                                   := 0 ;
    Yoffset                                   := 0 ;
    Zoffset                                   := 0 ;
    MaxX                                      := 0 ;
    MinX                                      := 0 ;
    MaxY                                      := 0 ;
    MinY                                      := 0 ;
    MaxZ                                      := 0 ;
    MinZ                                      := 0 ;
    StartofWaveformDataPacketRecord           := 0 ;
    StartoffirstExtendedVariableLengthRecord  := 0 ;
    NumberofExtendedVariableLengthRecords     := 0 ;
    ExNumberofpointrecords                    := 0 ;
    UserData                                  := nil ;
  end ;

  procedure TLAS_PublicHeaderBlock.Read(
    const _stream : TGIS_Stream
  ) ;
  var
    i     : Integer ;
    usize : Int64 ;
  begin
    // version 1.0-1.2
    _stream.ReadBytesCnt( FileSignature             , 4  ) ;
    {$IFDEF OXYGENE}
    _stream.ReadWord( FileSourceID                  , 2  ) ;
    _stream.ReadWord( GlobalEncoding                , 2  ) ;
    _stream.ReadCardinal( ProjectIDGUIDdata1        , 4  ) ;
    _stream.ReadWord( ProjectIDGUIDdata2            , 2  ) ;
    _stream.ReadWord( ProjectIDGUIDdata3            , 2  ) ;
    _stream.ReadBytesCnt( ProjectIDGUIDdata4        , 8  ) ;
    _stream.ReadByte( VersionMajor                  , 1  ) ;
    _stream.ReadByte( VersionMinor                  , 1  ) ;
    _stream.ReadBytesCnt( SystemIdentifier          , 32 ) ;
    _stream.ReadBytesCnt( GeneratingSoftware        , 32 ) ;
    _stream.ReadWord( FileCreationDayofYear         , 2  ) ;
    _stream.ReadWord( FileCreationYear              , 2  ) ;
    _stream.ReadWord( HeaderSize                    , 2  ) ;
    _stream.ReadCardinal( OffsetToPointData             , 4  ) ;
    _stream.ReadCardinal( NumberofVariableLengthRecords , 4  ) ;
    _stream.ReadByte( PointDataFormatID                 , 1  ) ;
    _stream.ReadWord( PointDataRecordLength             , 2  ) ;
    _stream.ReadCardinal( Numberofpointrecords          , 4  ) ;
    for i := 0 to 4 do
      _stream.ReadCardinal( Numberofpointsbyreturn[i] , 4  ) ;
    _stream.ReadDouble( Xscalefactor                  , 8  ) ;
    _stream.ReadDouble( Yscalefactor                  , 8  ) ;
    _stream.ReadDouble( Zscalefactor                  , 8  ) ;
    _stream.ReadDouble( Xoffset                       , 8  ) ;
    _stream.ReadDouble( Yoffset                       , 8  ) ;
    _stream.ReadDouble( Zoffset                       , 8  ) ;
    _stream.ReadDouble( MaxX                          , 8  ) ;
    _stream.ReadDouble( MinX                          , 8  ) ;
    _stream.ReadDouble( MaxY                          , 8  ) ;
    _stream.ReadDouble( MinY                          , 8  ) ;
    _stream.ReadDouble( MaxZ                          , 8  ) ;
    _stream.ReadDouble( MinZ                          , 8  ) ;
    {$ELSE}
    _stream.Read( FileSourceID                  , 2  ) ;
    _stream.Read( GlobalEncoding                , 2  ) ;
    _stream.Read( ProjectIDGUIDdata1            , 4  ) ;
    _stream.Read( ProjectIDGUIDdata2            , 2  ) ;
    _stream.Read( ProjectIDGUIDdata3            , 2  ) ;
    _stream.Read( ProjectIDGUIDdata4            , 8  ) ;
    _stream.Read( VersionMajor                  , 1  ) ;
    _stream.Read( VersionMinor                  , 1  ) ;
    _stream.ReadBytesCnt( SystemIdentifier      , 32 ) ;
    _stream.ReadBytesCnt( GeneratingSoftware    , 32 ) ;
    _stream.Read( FileCreationDayofYear         , 2  ) ;
    _stream.Read( FileCreationYear              , 2  ) ;
    _stream.Read( HeaderSize                    , 2  ) ;
    _stream.Read( OffsetToPointData             , 4  ) ;
    _stream.Read( NumberofVariableLengthRecords , 4  ) ;
    _stream.Read( PointDataFormatID             , 1  ) ;
    _stream.Read( PointDataRecordLength         , 2  ) ;
    _stream.Read( Numberofpointrecords          , 4  ) ;
    for i := 0 to 4 do
      _stream.Read( Numberofpointsbyreturn[i]   , 4  ) ;
    _stream.Read( Xscalefactor                  , 8  ) ;
    _stream.Read( Yscalefactor                  , 8  ) ;
    _stream.Read( Zscalefactor                  , 8  ) ;
    _stream.Read( Xoffset                       , 8  ) ;
    _stream.Read( Yoffset                       , 8  ) ;
    _stream.Read( Zoffset                       , 8  ) ;
    _stream.Read( MaxX                          , 8  ) ;
    _stream.Read( MinX                          , 8  ) ;
    _stream.Read( MaxY                          , 8  ) ;
    _stream.Read( MinY                          , 8  ) ;
    _stream.Read( MaxZ                          , 8  ) ;
    _stream.Read( MinZ                          , 8  ) ;
    {$ENDIF}

    // version 1.3
    if ( VersionMajor = 1 ) and ( VersionMinor >=3 ) then begin
      //assert( HeaderSize >= 235, 'Header size should be at least 235 bytes.' ) ;
      if HeaderSize >= 235 then
        {$IFDEF OXYGENE}
        _stream.ReadUInt64( StartofWaveformDataPacketRecord, 8 ) ;
        {$ELSE}
        _stream.Read( StartofWaveformDataPacketRecord, 8 ) ;
        {$ENDIF}
    end ;

    // version 1.4
    if ( VersionMajor = 1 ) and ( VersionMinor >=4 ) then begin
      //assert( HeaderSize >= 375, 'Header size should be at least 375 bytes.' ) ;
      if HeaderSize >= 375 then begin
        {$IFDEF OXYGENE}
        _stream.ReadUInt64( StartoffirstExtendedVariableLengthRecord, 8 ) ;
        _stream.ReadCardinal( NumberofExtendedVariableLengthRecords   , 4 ) ;
        _stream.ReadUInt64( ExNumberofpointrecords                   , 8 ) ;
        {$ELSE}
        _stream.Read( StartoffirstExtendedVariableLengthRecord, 8 ) ;
        _stream.Read( NumberofExtendedVariableLengthRecords   , 4 ) ;
        _stream.Read( ExNumberofpointrecords                   , 8 ) ;
        {$ENDIF}

        if ( Numberofpointrecords = 0                ) and
           ( ExNumberofpointrecords < high(Cardinal) ) then
          Numberofpointrecords := ExNumberofpointrecords ;

        for i := 0 to 14 do begin
          {$IFDEF OXYGENE}
          _stream.ReadUInt64( ExNumberofpointsbyreturn[i], 8 ) ;
          {$ELSE}
          _stream.Read( ExNumberofpointsbyreturn[i], 8 ) ;
          {$ENDIF}
          if i < 5 then begin
            if ( Numberofpointsbyreturn[i] = 0                ) and
               ( ExNumberofpointsbyreturn[i] < high(Cardinal) ) then
              Numberofpointsbyreturn[i] := ExNumberofpointsbyreturn[i] ;
          end ;
        end ;
      end ;
    end ;

    usize := HeaderSize - _stream.Position ;
    if usize > 0 then begin
      SetLength( UserData, usize ) ;
      _stream.ReadBytesCnt( UserData, usize ) ;
    end ;

  end ;

//==============================================================================
// TLAS_VariableLengthRecord
//==============================================================================

  constructor TLAS_VariableLengthRecord.Create ;
  begin
    inherited ;

    RecordSignatureAABB     := 0 ;
    SetLength( UserID, 16 ) ;
    RecordID                := 0 ;
    RecordLengthAfterHeader := 0 ;
    SetLength( Description, 32 ) ;

    Data            := nil ;
    GeoKeys         := nil ;
    GeoDoubleParams := nil ;
    GeoAsciiParams  := nil ;
  end ;

  procedure TLAS_VariableLengthRecord.doDestroy ;
  begin
    FreeObject( GeoKeys ) ;

    inherited ;
  end ;

  procedure TLAS_VariableLengthRecord.Read(
    const _stream   : TGIS_Stream ;
    const _extended : Boolean
  ) ;
  var
    val : Word ;
  begin
    {$IFDEF OXYGENE}
    _stream.ReadWord( RecordSignatureAABB     , 2  ) ;
    _stream.ReadBytesCnt( UserID              , 16 ) ;
    _stream.ReadWord( RecordID                , 2  ) ;
    {$ELSE}
    _stream.Read( RecordSignatureAABB     , 2  ) ;
    _stream.ReadBytesCnt( UserID          , 16 ) ;
    _stream.Read( RecordID                , 2  ) ;
    {$ENDIF}

    if not _extended then begin
      {$IFDEF OXYGENE}
      _stream.ReadWord( val, 2  ) ;
      {$ELSE}
      _stream.Read( val, 2  ) ;
      {$ENDIF}
      RecordLengthAfterHeader := val ;
    end
    else
      {$IFDEF OXYGENE}
      _stream.ReadUInt64( RecordLengthAfterHeader, 8  ) ;
      {$ELSE}
      _stream.Read( RecordLengthAfterHeader, 8  ) ;
      {$ENDIF}

    _stream.ReadBytesCnt( Description, 32 ) ;
    if RecordLengthAfterHeader > 0 then begin
      SetLength( Data, RecordLengthAfterHeader ) ;

      _stream.ReadBytesCnt( Data, RecordLengthAfterHeader ) ;
    end ;
  end ;

  function TLAS_VariableLengthRecord.GetInfo : String ;
  var
    i : Integer ;
  begin
    Result := Format( ' UserID      : %s',
                      [ConvertAnsiString( UserID, 16 )] ) + #13#10 +
              Format( ' RecordID    : %d',
                      [Integer(RecordID)] ) + #13#10 +
              Format( ' Description : %s',
                      [ConvertAnsiString( Description, 32 )] ) + #13#10 ;

              if assigned( GeoKeys ) then
                Result := Result + GeoKeys.GetInfo + #13#10 ;

              if assigned( GeoDoubleParams ) then begin
                Result := Result + ' GeoDoubleParams : ' ;
                for i := low( GeoDoubleParams ) to high( GeoDoubleParams ) do
                  Result := Result + IntToStr( GeoDoubleParams[i]) + ' ' ;
                Result := Result + #13#10 ;
              end ;

              if assigned( GeoAsciiParams ) then
                Result := Result + ' GeoAsciiParams : ' +
                          ConvertAnsiString( GeoAsciiParams ) + #13#10 ;
              if assigned( OGSCSWKT ) then
                Result := Result + ' OGSCSWKT : ' +

                          decodeString( OGSCSWKT ) + #13#10 ;

              if assigned( OGSMTWKT ) then
                Result := Result + ' OGSMTWKT : ' +
                          decodeString( OGSMTWKT ) + #13#10 ;

              if assigned( Classification.Description ) then
                Result := Result + ' Classification : ' +
                          ConvertAnsiString( Classification.Description ) + #13#10 ;
  end ;

  procedure TLAS_VariableLengthRecord.ParseGeoKeys ;
  var
    user : String ;
  begin
    user := ConvertAnsiString( UserID ) ;
    if user = 'LASF_Projection' then begin
      if assigned( Data ) then begin
        if RecordID = 34735 then begin // GeoKeyDirectoryTag
          GeoKeys := TLAS_VLRGeoKeys.Create ;
          try
            GeoKeys.Read( Data ) ;
          except
            FreeObject( GeoKeys ) ;
          end ;
        end
        else if RecordID = 34736 then begin // GeoDoubleParamsTag
          GeoDoubleParams := Data ;
        end
        else if RecordID = 34737 then begin // GeoAsciiParamsTag
          GeoAsciiParams := Data ;
        end
        else if RecordID = 2111 then begin // OGC Math Transform WKT
          OGSMTWKT := Data ;
        end
        else if RecordID = 2112 then begin // OGR CS WKT
          OGSCSWKT := Data ;
        end ;
      end ;
    end
    else if user = 'LASF_Spec' then begin
      if assigned( Data ) then begin
        if RecordID = 0 then begin // ClassificationLookup
          Classification.ClassNumber := Data[0] ;
          SetLength( Classification.Description, 16 ) ;
          GisCopyMemory( Data, 1, Classification.Description, 0, 16 ) ;
        end
        else if RecordID = 2 then begin // Histogram

        end
        else if RecordID = 3 then begin // TextAreaDescription

        end
        else if RecordID = 4 then begin // ExtraBytes

        end
        else if RecordID = 7 then begin // Superseded

        end
      end ;

    end ;

  end ;

//==============================================================================
// TLAS_VLRGeoKeys
//==============================================================================

  procedure TLAS_VLRGeoKeys.Read(
    const _data   : TBytes
  ) ;
  var
    i   : Integer ;
    off : Integer ;
  begin
    off := 0 ;
    {$IFDEF OXYGENE}
      KeyDirectoryVersion := BitConverter.ToUInt16( _data, off ) ;
    {$ELSE}
      Move( _data[off], KeyDirectoryVersion, 2 ) ;
    {$ENDIF}
    inc( off, 2 ) ;
    {$IFDEF OXYGENE}
      KeyRevision := BitConverter.ToUInt16( _data, off ) ;
    {$ELSE}
      Move( _data[off], KeyRevision, 2 ) ;
    {$ENDIF}
    inc( off, 2 ) ;
    {$IFDEF OXYGENE}
      MinorRevision := BitConverter.ToUInt16( _data, off ) ;
    {$ELSE}
      Move( _data[off], MinorRevision, 2 ) ;
    {$ENDIF}
    inc( off, 2 ) ;
    {$IFDEF OXYGENE}
      NumberOfKeys := BitConverter.ToUInt16( _data, off ) ;
    {$ELSE}
      Move( _data[off], NumberOfKeys, 2 ) ;
    {$ENDIF}
    inc( off, 2 ) ;
    SetLength( FKeys, NumberOfKeys ) ;
    for i := 0 to NumberOfKeys-1 do begin
      {$IFDEF GIS_NORECORDS}
        FKeys[i] := new TLAS_VLRKeyEntry ;
      {$ENDIF}
      {$IFDEF OXYGENE}
        FKeys[i].KeyID := BitConverter.ToUInt16( _data, off ) ;
      {$ELSE}
        Move( _data[off], FKeys[i].KeyID, 2 ) ;
      {$ENDIF}
      inc( off, 2 ) ;
      {$IFDEF OXYGENE}
        FKeys[i].TIFFTagLocation := BitConverter.ToUInt16( _data, off ) ;
      {$ELSE}
        Move( _data[off], FKeys[i].TIFFTagLocation, 2 ) ;
      {$ENDIF}
      inc( off, 2 ) ;
      {$IFDEF OXYGENE}
        FKeys[i].Count := BitConverter.ToUInt16( _data, off ) ;
      {$ELSE}
        Move( _data[off], FKeys[i].Count, 2 ) ;
      {$ENDIF}
      inc( off, 2 ) ;
      {$IFDEF OXYGENE}
        FKeys[i].ValueOffset := BitConverter.ToUInt16( _data, off ) ;
      {$ELSE}
        Move( _data[off], FKeys[i].ValueOffset, 2 ) ;
      {$ENDIF}
      inc( off, 2 ) ;
    end ;
  end ;

  function TLAS_VLRGeoKeys.GetInfo : String ;
  var
    i : Integer ;
  begin
    Result := Format( ' KeyDirectory Version : %d.%d.%d',
                      [Integer(KeyDirectoryVersion), Integer(KeyRevision), Integer(MinorRevision)] ) + #13#10 +
              Format( ' NumberOfKeys : %d', [Integer(NumberOfKeys)] ) + #13#10 ;
              for i := 0 to NumberOfKeys-1 do begin
                Result := Result +
                          Format( '  KeyID : %d, TIFFTagLocation : %d, Count : %d,'+
                                  ' ValueOffset : %d',
                          [ Integer(FKeys[i].KeyID), FKeys[i].TIFFTagLocation,Integer(FKeys[i].Count),
                            Integer(FKeys[i].ValueOffset) ] ) + #13#10  ;
              end ;
  end ;

//==============================================================================
// TLAS_PointDataRecordFormat
//==============================================================================

  function TLAS_PointDataRecordFormat.GetReturnNumber : Byte ;
  begin
    Result := 0 ;
  end ;

  function TLAS_PointDataRecordFormat.GetNumberOfReturns : Byte ;
  begin
    Result := 0 ;
  end ;

  function TLAS_PointDataRecordFormat.GetScanDirectionFlag : Byte ;
  begin
    Result := 0 ;
  end ;

  function TLAS_PointDataRecordFormat.GetEdgeOfFlightLine : Byte ;
  begin
    Result := 0 ;
  end ;

  function TLAS_PointDataRecordFormat.GetClassification: Byte;
  begin
    Result := 0 ;
  end ;

  function TLAS_PointDataRecordFormat.GetIntensity : Word ;
  begin
    Result := Intensity ;
  end ;

  function TLAS_PointDataRecordFormat.GetColor: Cardinal;
  begin
    Result := 0 ;
  end ;

  function TLAS_PointDataRecordFormat.GetScanAngleRank  : SmallInt ;
  begin
    Result := 0 ;
  end ;

  function TLAS_PointDataRecordFormat.GetGPSTime : Double ;
  begin
    Result := 0 ;
  end ;

  function TLAS_PointDataRecordFormat.GetClassificationFlag : Byte ;
  begin
    Result := 0 ;
  end ;

  function TLAS_PointDataRecordFormat.GetScannerChannel : Byte ;
  begin
    Result := 0 ;
  end ;

  function TLAS_PointDataRecordFormat.GetInfo: String;
  begin
    Result := Format( ' X : %d', [X] ) + #13#10 +
              Format( ' Y : %d', [Y] ) + #13#10 +
              Format( ' Z : %d', [Z] ) + #13#10 +
              Format( ' Intensity : %d', [Intensity] ) + #13#10 ;
  end ;

  function TLAS_PointDataRecordFormat.GetPoint(
    const _xscale  : Double ;
    const _yscale  : Double ;
    const _zscale  : Double ;
    const _xoffset : Double ;
    const _yoffset : Double ;
    const _zoffset : Double
   ) : TGIS_Point3D ;
  begin
    Result := GisPoint3D( ( X * _xscale ) + _xoffset,
                          ( Y * _yscale ) + _yoffset,
                          ( Z * _zscale ) + _zoffset
                         ) ;
  end ;

  procedure TLAS_PointDataRecordFormat.Read(
    const _stream : TGIS_Stream
   ) ;
  begin
    {$IFDEF OXYGENE}
    _stream.ReadInteger( X               , 4 ) ;
    _stream.ReadInteger( Y               , 4 ) ;
    _stream.ReadInteger( Z               , 4 ) ;
    _stream.ReadWord( Intensity       , 2 ) ;
    {$ELSE}
    _stream.Read( X               , 4 ) ;
    _stream.Read( Y               , 4 ) ;
    _stream.Read( Z               , 4 ) ;
    _stream.Read( Intensity       , 2 ) ;
    {$ENDIF}
  end ;

//==============================================================================
// TLAS_PointDataRecordFormat0
//==============================================================================

  procedure TLAS_PointDataRecordFormat0.Read(
    const _stream : TGIS_Stream
  ) ;
  begin
    inherited Read( _stream ) ;

    {$IFDEF OXYGENE}
    _stream.ReadByte( Flags           , 1 ) ;
    _stream.ReadByte( Classification  , 1 ) ;
    _stream.ReadShortInt( ScanAngleRank   , 1 ) ;
    _stream.ReadByte( UserData        , 1 ) ;
    _stream.ReadWord( PointSourceID   , 2 ) ;
    {$ELSE}
    _stream.Read( Flags           , 1 ) ;
    _stream.Read( Classification  , 1 ) ;
    _stream.Read( ScanAngleRank   , 1 ) ;
    _stream.Read( UserData        , 1 ) ;
    _stream.Read( PointSourceID   , 2 ) ;
    {$ENDIF}
  end ;

  function TLAS_PointDataRecordFormat0.GetClassification: Byte;
  begin
    Result := Classification ;
  end ;

  function TLAS_PointDataRecordFormat0.GetReturnNumber : Byte ;
  begin
    Result := Flags and 7 ;
  end ;

  function TLAS_PointDataRecordFormat0.GetNumberOfReturns : Byte ;
  begin
    Result := Flags and 56 ;
  end ;

  function TLAS_PointDataRecordFormat0.GetScanDirectionFlag : Byte ;
  begin
    Result := Flags and 64 ;
  end ;

  function TLAS_PointDataRecordFormat0.GetEdgeOfFlightLine : Byte ;
  begin
    Result := Flags and 128 ;
  end ;

  function TLAS_PointDataRecordFormat0.GetScanAngleRank  : SmallInt ;
  begin
    Result := ScanAngleRank ;
  end ;

  function TLAS_PointDataRecordFormat0.GetInfo: String;
  begin
    Result := inherited GetInfo +
              Format( ' Return number : %d', [Flags and 7] ) + #13#10 +
              Format( ' Number of Returns : %d', [Flags and 56] ) + #13#10 +
              Format( ' Scan Direction Flag : %d', [Flags and 64] ) + #13#10 +
              Format( ' Edge of Flight Line : %d', [Flags and 128] ) + #13#10 +
              Format( ' Classification : %d', [Classification] ) + #13#10 +
              Format( ' Scan Angle Rank : %d', [ScanAngleRank] ) + #13#10 +
              Format( ' User Data : %d', [UserData] ) + #13#10 +
              Format( ' Point Source ID : %d', [PointSourceID] ) + #13#10 ;
  end ;

//==============================================================================
// TLAS_PointDataRecordFormat1
//==============================================================================

  function TLAS_PointDataRecordFormat1.GetGPSTime : Double ;
  begin
    Result := GPSTime ;
  end ;

  function TLAS_PointDataRecordFormat1.GetInfo : String ;
  begin
    Result := inherited GetInfo +
              Format( ' GPSTime : %f', [GPSTime] ) + #13#10 ;
  end ;

  procedure TLAS_PointDataRecordFormat1.Read(
    const _stream : TGIS_Stream
  ) ;
  begin
    inherited Read( _stream ) ;

    {$IFDEF OXYGENE}
    _stream.ReadDouble( GPSTime, 8 ) ;
    {$ELSE}
    _stream.Read( GPSTime, 8 ) ;
    {$ENDIF}
  end ;

//==============================================================================
// TLAS_PointDataRecordFormat2
//==============================================================================

  function TLAS_PointDataRecordFormat2.GetInfo : String ;
  begin
    Result := inherited GetInfo +
              Format( ' Red : %d', [Red] ) + #13#10 +
              Format( ' Green : %d', [Green] ) + #13#10 +
              Format( ' Blue : %d', [Blue] ) + #13#10 ;
  end ;

  procedure TLAS_PointDataRecordFormat2.Read(
    const _stream : TGIS_Stream
  ) ;
  begin
    inherited Read( _stream ) ;

    {$IFDEF OXYGENE}
    _stream.ReadWord( Red, 2 ) ;
    _stream.ReadWord( Green, 2 ) ;
    _stream.ReadWord( Blue, 2 ) ;
    {$ELSE}
    _stream.Read( Red, 2 ) ;
    _stream.Read( Green, 2 ) ;
    _stream.Read( Blue, 2 ) ;
    {$ENDIF}
  end ;

  function TLAS_PointDataRecordFormat2.GetColor : Cardinal ;
  begin
    Result := convertLASColor( Red, Green, Blue ) ;
  end ;

  function TLAS_PointDataRecordFormat3.GetInfo : String ;
  begin
    Result := inherited GetInfo +
              Format( ' GPS Time : %f', [GPSTime] ) + #13#10 +
              Format( ' Red : %d', [Red] ) + #13#10 +
              Format( ' Green : %d', [Green] ) + #13#10 +
              Format( ' Blue : %d', [Blue] ) + #13#10 ;
  end ;

//==============================================================================
// TLAS_PointDataRecordFormat3
//==============================================================================

  procedure TLAS_PointDataRecordFormat3.Read(
    const _stream : TGIS_Stream
  ) ;
  begin
    inherited Read( _stream ) ;

    {$IFDEF OXYGENE}
    _stream.ReadDouble( GPSTime , 8 ) ;
    _stream.ReadWord( Red, 2 ) ;
    _stream.ReadWord( Green, 2 ) ;
    _stream.ReadWord( Blue, 2 ) ;
    {$ELSE}
    _stream.Read( GPSTime , 8 ) ;
    _stream.Read( Red     , 2 ) ;
    _stream.Read( Green   , 2 ) ;
    _stream.Read( Blue    , 2 ) ;
    {$ENDIF}
  end ;

  function TLAS_PointDataRecordFormat3.GetColor : Cardinal ;
  begin
    Result := convertLASColor( Red, Green, Blue ) ;
  end ;

//==============================================================================
// TLAS_PointDataRecordFormat4
//==============================================================================

  function TLAS_PointDataRecordFormat4.GetInfo : String ;
  begin
    Result := inherited GetInfo +
              Format( ' Wave Packet Descriptor Index : %d',
                        [WavePacketDescriptorIndex] ) + #13#10 +
              Format( ' Byte offset to waveform data : %u',
                        [Byteoffsettowaveformdata] ) + #13#10 +
              Format( ' Waveform packet size in bytes : %u',
                        [Waveformpacketsizeinbytes] ) + #13#10 +
              Format( ' Return Point Waveform Location : %f',
                        [ReturnPointWaveformLocation] ) + #13#10 +
              Format( ' Xt : %f', [Xt] ) + #13#10 +
              Format( ' Yt : %f', [Yt] ) + #13#10 +
              Format( ' Zt : %f', [Zt] ) + #13#10 ;
  end ;

  procedure TLAS_PointDataRecordFormat4.Read(
    const _stream : TGIS_Stream
  ) ;
  begin
    inherited Read( _stream ) ;

    {$IFDEF OXYGENE}
    _stream.ReadByte( WavePacketDescriptorIndex   , 1 ) ;
    _stream.ReadUInt64( Byteoffsettowaveformdata    , 8 ) ;
    _stream.ReadCardinal( Waveformpacketsizeinbytes   , 4 ) ;
    _stream.ReadSingle( ReturnPointWaveformLocation , 4 ) ;
    _stream.ReadSingle( Xt                          , 4 ) ;
    _stream.ReadSingle( Yt                          , 4 ) ;
    _stream.ReadSingle( Zt                          , 4 ) ;
    {$ELSE}
    _stream.Read( WavePacketDescriptorIndex   , 1 ) ;
    _stream.Read( Byteoffsettowaveformdata    , 8 ) ;
    _stream.Read( Waveformpacketsizeinbytes   , 4 ) ;
    _stream.Read( ReturnPointWaveformLocation , 4 ) ;
    _stream.Read( Xt                          , 4 ) ;
    _stream.Read( Yt                          , 4 ) ;
    _stream.Read( Zt                          , 4 ) ;
    {$ENDIF}
  end ;

//==============================================================================
// TLAS_PointDataRecordFormat5
//==============================================================================

  function TLAS_PointDataRecordFormat5.GetInfo : String ;
  begin
    Result := inherited GetInfo +
              Format( ' WavePacketDescriptorIndex : %d',
                      [WavePacketDescriptorIndex] ) + #13#10 +
              Format( ' Byteoffsettowaveformdata : %u',
                      [Byteoffsettowaveformdata] ) + #13#10 +
              Format( ' Waveformpacketsizeinbytes : %u',
                      [Waveformpacketsizeinbytes] ) + #13#10 +
              Format( ' ReturnPointWaveformLocation : %f',
                      [ReturnPointWaveformLocation] ) + #13#10 +
              Format( ' Xt : %f', [Xt] ) + #13#10 +
              Format( ' Yt : %f', [Yt] ) + #13#10 +
              Format( ' Zt : %f', [Zt] ) + #13#10 ;
  end ;

  procedure TLAS_PointDataRecordFormat5.Read(
    const _stream : TGIS_Stream
  ) ;
  begin
    inherited Read( _stream ) ;

    {$IFDEF OXYGENE}
    _stream.ReadByte( WavePacketDescriptorIndex   , 1 ) ;
    _stream.ReadUInt64( Byteoffsettowaveformdata    , 8 ) ;
    _stream.ReadCardinal( Waveformpacketsizeinbytes   , 4 ) ;
    _stream.ReadSingle( ReturnPointWaveformLocation , 4 ) ;
    _stream.ReadSingle( Xt                          , 4 ) ;
    _stream.ReadSingle( Yt                          , 4 ) ;
    _stream.ReadSingle( Zt                          , 4 ) ;
    {$ELSE}
    _stream.Read( WavePacketDescriptorIndex   , 1 ) ;
    _stream.Read( Byteoffsettowaveformdata    , 8 ) ;
    _stream.Read( Waveformpacketsizeinbytes   , 4 ) ;
    _stream.Read( ReturnPointWaveformLocation , 4 ) ;
    _stream.Read( Xt                          , 4 ) ;
    _stream.Read( Yt                          , 4 ) ;
    _stream.Read( Zt                          , 4 ) ;
    {$ENDIF}
  end ;

//==============================================================================
// TLAS_PointDataRecordFormat6
//==============================================================================

  function TLAS_PointDataRecordFormat6.GetClassification : Byte ;
  begin
    Result := Classification ;
  end ;

  function TLAS_PointDataRecordFormat6.GetInfo : String ;
  begin
    Result := inherited GetInfo +
              Format( ' Return Number : %d',      [Flags and 15] ) + #13#10 +
              Format( ' Number of Returns : %d',  [Flags and 240] ) + #13#10 +
              Format( ' Classification Flag : %d',[Flags and 3840] ) + #13#10 +
              Format( ' Scanner Channel : %d',    [Flags and 12288] ) + #13#10 +
              Format( ' Scan Direction Flag : %d',[Flags and 16384] ) + #13#10 +
              Format( ' Edge of Flight Line : %d',[Flags and 32768] ) + #13#10 +
              Format( ' Classification : %d',     [Classification] ) + #13#10 +
              Format( ' User Data : %d',          [UserData] ) + #13#10 +
              Format( ' Scan Angle : %d',         [ScanAngle] ) + #13#10 +
              Format( ' PointSourceID : %d',      [PointSourceID] ) + #13#10 +
              Format( ' GPS Time : %f',           [GPSTime] ) + #13#10 ;
  end ;

  function TLAS_PointDataRecordFormat6.GetReturnNumber : Byte ;
  begin
    Result := Flags and 15 ;
  end ;

  function TLAS_PointDataRecordFormat6.GetNumberOfReturns : Byte ;
  begin
    Result := Flags and 240 ;
  end ;

  function TLAS_PointDataRecordFormat6.GetScanDirectionFlag : Byte ;
  begin
    Result := Flags and 16384 ;
  end ;

  function TLAS_PointDataRecordFormat6.GetEdgeOfFlightLine : Byte ;
  begin
    Result := Flags and 32768 ;
  end ;

  function TLAS_PointDataRecordFormat6.GetScanAngleRank  : SmallInt ;
  begin
    Result := ScanAngle ;
  end ;

  function TLAS_PointDataRecordFormat6.GetClassificationFlag : Byte ;
  begin
    Result := Flags and 3840 ;
  end ;

  function TLAS_PointDataRecordFormat6.GetScannerChannel : Byte ;
  begin
    Result := Flags and 12288 ;
  end ;

  function TLAS_PointDataRecordFormat6.GetGPSTime : Double ;
  begin
    Result := Classification ;
  end ;

  procedure TLAS_PointDataRecordFormat6.Read(
    const _stream : TGIS_Stream
   ) ;
  begin
    inherited Read( _stream ) ;

    {$IFDEF OXYGENE}
    _stream.ReadWord( Flags           , 2 ) ;
    _stream.ReadByte( Classification  , 1 ) ;
    _stream.ReadByte( UserData        , 1 ) ;
    _stream.ReadSmallInt( ScanAngle       , 2 ) ;
    _stream.ReadWord( PointSourceID   , 2 ) ;
    _stream.ReadDouble( GPSTime         , 8 ) ;
    {$ELSE}
    _stream.Read( Flags           , 2 ) ;
    _stream.Read( Classification  , 1 ) ;
    _stream.Read( UserData        , 1 ) ;
    _stream.Read( ScanAngle       , 2 ) ;
    _stream.Read( PointSourceID   , 2 ) ;
    _stream.Read( GPSTime         , 8 ) ;
    {$ENDIF}
  end ;

//==============================================================================
// TLAS_PointDataRecordFormat7
//==============================================================================

  function TLAS_PointDataRecordFormat7.GetColor : Cardinal ;
  begin
    Result := convertLASColor( Red, Green, Blue ) ;
  end ;

  function TLAS_PointDataRecordFormat7.GetInfo : String ;
  begin
    Result := inherited GetInfo +
              Format( ' Red : %d',   [Red]   ) + #13#10 +
              Format( ' Green : %d', [Green] ) + #13#10 +
              Format( ' Blue : %d',  [Blue]  ) + #13#10 ;
  end ;

  procedure TLAS_PointDataRecordFormat7.Read(
    const _stream : TGIS_Stream
   ) ;
  begin
    inherited Read( _stream ) ;

    {$IFDEF OXYGENE}
    _stream.ReadWord( Red     , 2 ) ;
    _stream.ReadWord( Green   , 2 ) ;
    _stream.ReadWord( Blue    , 2 ) ;
    {$ELSE}
    _stream.Read( Red     , 2 ) ;
    _stream.Read( Green   , 2 ) ;
    _stream.Read( Blue    , 2 ) ;
    {$ENDIF}
  end ;

//==============================================================================
// TLAS_PointDataRecordFormat8
//==============================================================================

  function TLAS_PointDataRecordFormat8.GetColor : Cardinal ;
  begin
    Result := inherited GetColor or (NIR shl 24) ;
  end ;

  function TLAS_PointDataRecordFormat8.GetInfo : String ;
  begin
    Result := inherited GetInfo +
              Format( ' NIR : %d', [NIR] ) ;
  end ;

  procedure TLAS_PointDataRecordFormat8.Read(
    const _stream : TGIS_Stream
  ) ;
  begin
    inherited Read( _stream ) ;

    {$IFDEF OXYGENE}
    _stream.ReadWord( NIR, 2 ) ;
    {$ELSE}
    _stream.Read( NIR, 2 ) ;
    {$ENDIF}
  end ;

//==============================================================================
// TLAS_PointDataRecordFormat9
//==============================================================================

  function TLAS_PointDataRecordFormat9.GetInfo : String ;
  begin
    Result := inherited GetInfo +
              Format( ' Wave Packet Descriptor Index : %d',
                      [WavePacketDescriptorIndex] ) + #13#10 +
              Format( ' Byte offset to waveform data : %u',
                      [Byteoffsettowaveformdata] ) + #13#10 +
              Format( ' Waveform packet size in bytes : %u',
                      [Waveformpacketsizeinbytes] ) + #13#10 +
              Format( ' Return Point Waveform Location : %f',
                      [ReturnPointWaveformLocation] ) + #13#10 +
              Format( ' Xt : %f', [Xt] ) + #13#10 +
              Format( ' Yt : %f', [Yt] ) + #13#10 +
              Format( ' Zt : %f', [Zt] ) + #13#10 ;
  end ;

  procedure TLAS_PointDataRecordFormat9.Read(
    const _stream : TGIS_Stream
  ) ;
  begin
    inherited Read( _stream ) ;

    {$IFDEF OXYGENE}
    _stream.ReadByte( WavePacketDescriptorIndex   , 1 ) ;
    _stream.ReadUInt64( Byteoffsettowaveformdata    , 8 ) ;
    _stream.ReadCardinal( Waveformpacketsizeinbytes   , 4 ) ;
    _stream.ReadSingle( ReturnPointWaveformLocation , 4 ) ;
    _stream.ReadSingle( Xt                          , 4 ) ;
    _stream.ReadSingle( Yt                          , 4 ) ;
    _stream.ReadSingle( Zt                          , 4 ) ;
    {$ELSE}
    _stream.Read( WavePacketDescriptorIndex   , 1 ) ;
    _stream.Read( Byteoffsettowaveformdata    , 8 ) ;
    _stream.Read( Waveformpacketsizeinbytes   , 4 ) ;
    _stream.Read( ReturnPointWaveformLocation , 4 ) ;
    _stream.Read( Xt                          , 4 ) ;
    _stream.Read( Yt                          , 4 ) ;
    _stream.Read( Zt                          , 4 ) ;
    {$ENDIF}
  end ;

//==============================================================================
// TLAS_PointDataRecordFormat10
//==============================================================================

  function TLAS_PointDataRecordFormat10.GetInfo : String ;
  begin
    Result := inherited GetInfo +
              Format( ' Wave Packet Descriptor Index : %d',
                      [WavePacketDescriptorIndex] ) + #13#10 +
              Format( ' Byte offset to waveform data : %u',
                      [Byteoffsettowaveformdata] ) + #13#10 +
              Format( ' Waveform packet size in bytes : %u',
                      [Waveformpacketsizeinbytes] ) + #13#10 +
              Format( ' Return Point Waveform Location : %f',
                      [ReturnPointWaveformLocation] ) + #13#10 +
              Format( ' Xt : %f', [Xt] ) + #13#10 +
              Format( ' Yt : %f', [Yt] ) + #13#10 +
              Format( ' Zt : %f', [Zt] ) + #13#10 ;
  end ;

  procedure TLAS_PointDataRecordFormat10.Read(
    const _stream : TGIS_Stream
  ) ;
  begin
    inherited Read( _stream ) ;

    {$IFDEF OXYGENE}
    _stream.ReadByte( WavePacketDescriptorIndex   , 1 ) ;
    _stream.ReadUInt64( Byteoffsettowaveformdata    , 8 ) ;
    _stream.ReadCardinal( Waveformpacketsizeinbytes   , 4 ) ;
    _stream.ReadSingle( ReturnPointWaveformLocation , 4 ) ;
    _stream.ReadSingle( Xt                          , 4 ) ;
    _stream.ReadSingle( Yt                          , 4 ) ;
    _stream.ReadSingle( Zt                          , 4 ) ;
    {$ELSE}
    _stream.Read( WavePacketDescriptorIndex   , 1 ) ;
    _stream.Read( Byteoffsettowaveformdata    , 8 ) ;
    _stream.Read( Waveformpacketsizeinbytes   , 4 ) ;
    _stream.Read( ReturnPointWaveformLocation , 4 ) ;
    _stream.Read( Xt                          , 4 ) ;
    _stream.Read( Yt                          , 4 ) ;
    _stream.Read( Zt                          , 4 ) ;
    {$ENDIF}
  end ;

//==============================================================================
// TLAS_PointDataRecordFormatLaz
//==============================================================================
  {$IFNDEF ANDROID}
  constructor TLAS_PointDataRecordFormatLaz.Create(
    const _ptr : laszip_PTR
  ) ;
  begin
    inherited Create ;

    {$IFDEF OXYGENE}
      ptr := _ptr ;
    {$ELSE}
      ppoint := plaszip_point(_ptr) ;
    {$ENDIF}
  end ;

  procedure TLAS_PointDataRecordFormatLaz.marshalPoint ;
  begin
    {$IFDEF OXYGENE}
      ppoint := laszip_point( Marshal.PtrToStructure( ptr, typeOf(laszip_point) ) ) ;
    {$ELSE}
    {$ENDIF}
  end;

  function TLAS_PointDataRecordFormatLaz.GetClassification: Byte;
  begin
    Result := ppoint.flags_c and 31 ;
  end ;

  function TLAS_PointDataRecordFormatLaz.GetClassificationFlag: Byte;
  begin
    Result := ppoint.flags_c ;
  end ;

  function TLAS_PointDataRecordFormatLaz.GetColor: Cardinal;
  begin
    Result := convertLASColor( ppoint.rgb[0], ppoint.rgb[1], ppoint.rgb[2] ) ;
  end ;

  function TLAS_PointDataRecordFormatLaz.GetEdgeOfFlightLine: Byte;
  begin
    Result := ppoint.flags and 128 ;
  end ;

  function TLAS_PointDataRecordFormatLaz.GetGPSTime: Double;
  begin
    Result := ppoint.gps_time ;
  end ;

  function TLAS_PointDataRecordFormatLaz.GetInfo: String;
  begin
    Result := Format( ' X : %d', [ppoint.X] ) + #13#10 +
              Format( ' Y : %d', [ppoint.Y] ) + #13#10 +
              Format( ' Z : %d', [ppoint.Z] ) + #13#10 +
              Format( ' Intensity : %d', [ppoint.intensity] ) + #13#10 +
              Format( ' Return number : %d', [ppoint.flags and 7] ) + #13#10 +
              Format( ' Number of Returns : %d', [ppoint.flags and 56] ) + #13#10 +
              Format( ' Scan Direction Flag : %d', [ppoint.flags and 64] ) + #13#10 +
              Format( ' Edge of Flight Line : %d', [ppoint.flags and 128] ) + #13#10 +
              Format( ' Classification : %d', [ppoint.flags_c and 31] ) + #13#10 +
              Format( ' Scan Angle Rank : %d', [ppoint.scan_angle_rank] ) ;
  end ;

  function TLAS_PointDataRecordFormatLaz.GetIntensity: Word;
  begin
    Result := ppoint.intensity ;
  end ;

  function TLAS_PointDataRecordFormatLaz.GetNumberOfReturns: Byte;
  begin
    Result := ppoint.flags and 56 ;
  end ;

  function TLAS_PointDataRecordFormatLaz.GetPoint(
    const _xscale   : Double ;
    const _yscale   : Double ;
    const _zscale   : Double ;
    const _xoffset  : Double ;
    const _yoffset  : Double ;
    const _zoffset  : Double
  ) : TGIS_Point3D ;
  begin
    Result := GisPoint3D( ( ppoint.X * _xscale ) + _xoffset,
                          ( ppoint.Y * _yscale ) + _yoffset,
                          ( ppoint.Z * _zscale ) + _zoffset
                         ) ;
  end ;

  function TLAS_PointDataRecordFormatLaz.GetReturnNumber: Byte;
  begin
    Result := ppoint.flags and 7 ;
  end ;

  function TLAS_PointDataRecordFormatLaz.GetScanAngleRank: SmallInt;
  begin
    Result := ppoint.scan_angle_rank ;
  end ;

  function TLAS_PointDataRecordFormatLaz.GetScanDirectionFlag: Byte;
  begin
    Result := ppoint.flags and 64 ;
  end ;

  function TLAS_PointDataRecordFormatLaz.GetScannerChannel: Byte;
  begin
    Result := 0 ;
  end ;

  procedure TLAS_PointDataRecordFormatLaz.Read(
    const _stream : TGIS_Stream
  ) ;
  begin
    marshalPoint ;
  end ;
  {$ENDIF}

  {$IFDEF JAVA}
    {$IFNDEF ANDROID}

    constructor laszip_header.Create ;
    begin
      inherited ;
    end;

    constructor laszip_header.Create( _peer : com.sun.jna.Pointer ) ;
    begin
      inherited Create( _peer ) ;
      allocateMemory() ;
      &read() ;
    end;

    method laszip_header.getFieldOrder: java.util.List<String>;
    begin
      exit java.util.Arrays.asList(
        'file_source_ID','global_encoding','project_ID_GUID_data_1','project_ID_GUID_data_2',
        'project_ID_GUID_data_3','project_ID_GUID_data_4',
        'version_major','version_minor','system_identifier','generating_software',
        'file_creation_day','file_creation_year','header_size',
        'offset_to_point_data','number_of_variable_length_records','point_data_format',
        'point_data_record_length','number_of_point_records',
        'number_of_points_by_return','x_scale_factor','y_scale_factor','z_scale_factor',
        'x_offset','y_offset','z_offset',
        'max_x','min_x','max_y','min_y','max_z','min_z','start_of_waveform_data_packet_record',
        'start_of_first_extended_variable_length_record',
        'number_of_extended_variable_length_records','extended_number_of_point_records',
        'extended_number_of_points_by_return',
        'user_data_in_header_size','user_data_in_header','vlrs',
        'user_data_after_header_size','user_data_after_header'
      );
    end;

    constructor laszip_vlr.Create ;
    begin
      inherited ;
    end;

    constructor laszip_vlr.Create( _peer : com.sun.jna.Pointer ) ;
    begin
      inherited Create( _peer ) ;
      allocateMemory() ;
      &read() ;
    end;

    method laszip_vlr.getFieldOrder: java.util.List<String>;
    begin
      exit java.util.Arrays.asList(
        'reserved','user_id','record_id','record_length_after_header','description','data'
      );
    end;

    constructor laszip_point.Create ;
    begin
      inherited ;
    end;

    constructor laszip_point.Create( _peer : com.sun.jna.Pointer ) ;
    begin
      inherited Create( _peer ) ;
      allocateMemory() ;
      &read() ;
    end;

    method laszip_point.getFieldOrder: java.util.List<String>;
    begin
      exit java.util.Arrays.asList(
        "X","Y","Z",'intensity','flags','flags_c','scan_angle_rank','user_data',
        'point_source_ID','extended_scan_angle',
        'extended_flags_point','extended_classification','extended_flags_return',
        'dummy','gps_time','rgb','wave_packet',
        'num_extra_bytes','extra_bytes'
      );
    end;
    {$ENDIF}

  {$ENDIF}

//=============================================================================
// T_LASZipLib
//=============================================================================
  {$IFNDEF ANDROID}
  constructor T_LASZipLib.Create ;
  begin
    {$IFNDEF OXYGENE}
    inherited Create ;
    {$ENDIF}

    DLLHandle := 0 ;
    DLLLoaded := False ;
  end ;

  {$IFNDEF OXYGENE}
    destructor T_LASZipLib.Destroy ;
    begin
      if DLLLoaded and ( DLLHandle <> 0 ) then begin
        if AssignedPtr( PReader ) then begin
          laszip_close_reader( PReader ) ;
          laszip_destroy( PReader ) ;
        end ;
        FreeLibrary( DLLHandle ) ;
        DLLHandle := 0 ;
        FLASInstanceCount := 0 ;
        DLLLoaded := False ;
      end ;

      inherited ;
    end ;
  {$ENDIF}

  function T_LASZipLib.LoadDLL(
    const _dllPath : String
  ) : Boolean ;
  var
    dllok : Boolean ;

    {$IFNDEF JAVA}
      procedure mapFnc( {$IFDEF LEVEL_XE2_RTL}
                          var _fnc : Pointer ;
                        {$ELSE}
                          var _fnc : IntPtr ;
                        {$ENDIF}
                         const _name : String
                       ) ;
      begin
        {$IFDEF OXYGENE}
        _fnc := GetProcAddress( DLLHandle, _name ) ;
        {$ELSE}
        _fnc := GetProcAddress( DLLHandle, PChar( _name ) ) ;
        {$ENDIF}
        assert( _fnc <> nil ) ;
      end ;
    {$ENDIF}
  begin
    Result := DLLLoaded ;

    try
      DLLLoaded := False ;
      try
        {$IFDEF JAVA}
          if DLLLoaded or (DLLHandle <> nil ) then exit ;

          DLLHandle := ILASZipLibrary(com.sun.jna.Native.loadLibrary( _dllPath, typeOf(ILASZipLibrary) ) );
          dllok := DLLHandle <> nil ;
        {$ELSE}
          if DLLLoaded or (DLLHandle <> 0 ) then exit ;

          DLLHandle := LoadLibraryWithinHinstance( _dllPath ) ;
          dllok := DLLHandle <> 0 ;
        {$ENDIF}

        if not dllok then
          raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEMAPPING ),
                                       _dllPath + #13#10 + SystemErrorMessage,
                                       0
                                      ) ;
        {$IFDEF OXYGENE}
        {$ELSE}
          mapFnc( @laszip_get_version,          'laszip_get_version'           ) ;
          mapFnc( @laszip_create,               'laszip_create'                ) ;
          mapFnc( @laszip_get_error,            'laszip_get_error'             ) ;
          mapFnc( @laszip_open_reader,          'laszip_open_reader'           ) ;
          mapFnc( @laszip_close_reader,         'laszip_close_reader'          ) ;
          mapFnc( @laszip_get_header_pointer,   'laszip_get_header_pointer'    ) ;
          mapFnc( @laszip_get_point_pointer,    'laszip_get_point_pointer'     ) ;
          mapFnc( @laszip_get_point_count,      'laszip_get_point_count'       ) ;
          mapFnc( @laszip_seek_point,           'laszip_seek_point'            ) ;
          mapFnc( @laszip_read_point,           'laszip_read_point'            ) ;
          mapFnc( @laszip_read_inside_point,    'laszip_read_inside_point'     ) ;
          mapFnc( @laszip_destroy,              'laszip_destroy'               ) ;
          mapFnc( @laszip_inside_rectangle,     'laszip_inside_rectangle'      ) ;
          mapFnc( @laszip_exploit_spatial_index,'laszip_exploit_spatial_index' ) ;
        {$ENDIF}
      except

      end ;
    finally
      DLLLoaded := DLLHandle <> 0
    end ;

    Result := DLLLoaded ;
  end ;

  function T_LASZipLib.UnLoadDLL : Boolean ;
  begin
    {$IFDEF JAVA}
      if DLLLoaded and ( DLLHandle <> nil ) then begin
        if PReader <> nil then begin
          laszip_close_reader( PReader ) ;
          laszip_destroy( PReader ) ;
        end ;
        DLLHandle := nil ;
        FLASInstanceCount := 0 ;
        DLLLoaded := False ;
      end ;
    {$ENDIF}
    {$IFDEF CLR}
      if DLLLoaded and ( DLLHandle <> 0 ) then begin
        if AssignedPtr( PReader ) then begin
          laszip_close_reader( PReader ) ;
          laszip_destroy( PReader ) ;
        end ;
        FreeLibrary( DLLHandle ) ;
        DLLHandle := 0 ;
        FLASInstanceCount := 0 ;
        DLLLoaded := False ;
      end ;
    {$ENDIF}
    Result := True ;
  end ;

  function T_LASZipLib.check(
    const _status : Integer
  ) : String ;
  var
    ptr : laszip_PTR ;
  begin
    laszip_get_error( PReader, ptr ) ;
    Result := asLASString( ptr ) ;
  end ;

  function T_LASZipLib.CreateReader : Integer ;
  begin
    PReader := nil ;
    Result := laszip_create( PReader ) ;
  end ;

  function T_LASZipLib.EnableSpatialIndex : Integer ;
  var
    exploit : laszip_BOOL ;
  begin
    exploit := 1 ;
    Result  := laszip_exploit_spatial_index( PReader, exploit ) ;
  end ;

  function T_LASZipLib.OpenReader(
    const _path : String
  ) : Integer ;
  var
    {$IFDEF OXYGENE}
      cmd : String ;
    {$ELSE}
      cmd : laszip_PTR ;
    {$ENDIF}
    is_compressed : laszip_BOOL ;
  begin
   {$IFDEF OXYGENE}
      cmd := _path ;
   {$ELSE}
      cmd := PAnsiChar( AnsiString( _path ) ) ;
   {$ENDIF}
    is_compressed := 0;
    Result := laszip_open_reader( PReader, cmd, is_compressed ) ;
  end ;

  function T_LASZipLib.ReadHeader(
    const _header : TLAS_PublicHeaderBlock ;
    const _fvlr   : TGIS_ObjectList
  ) : Integer ;
  var
    {$IFDEF OXYGENE}
      hdr  : laszip_header ;
    {$ELSE}
      hdr  : plaszip_header ;
    {$ENDIF}
    i,j  : Integer ;
    vlr  : TLAS_VariableLengthRecord ;
    {$IFDEF OXYGENE}
      vlrs : laszip_vlr;
      {$IFDEF CLR}
      vptr : IntPtr ;
      {$ENDIF}
      {$IFDEF JAVA}
      vptr : laszip_PTR ;
      vsize : Integer ;
      {$ENDIF}
    {$ELSE}
      vlrs : plaszip_vlr;
    {$ENDIF}
  begin
    PHeader := nil ;
    Result := laszip_get_header_pointer( PReader, PHeader ) ;
    if Result <> 0 then exit ;

    {$IFDEF OXYGENE}
      hdr := laszip_header( Marshal.PtrToStructure( PHeader, typeOf(laszip_header) ) ) ;
    {$ELSE}
      hdr := plaszip_header(PHeader) ;
    {$ENDIF}

    _header.Clean ;
    _header.FileSignature                 := ConvertAnsiString( 'LASF' ) ;
    _header.FileSourceID                  := hdr.file_source_ID ;
    _header.GlobalEncoding                := hdr.global_encoding ;
    _header.ProjectIDGUIDdata1            := hdr.project_ID_GUID_data_1 ;
    _header.ProjectIDGUIDdata2            := hdr.project_ID_GUID_data_2 ;
    _header.ProjectIDGUIDdata3            := hdr.project_ID_GUID_data_3 ;
    for i := 0 to 7 do
      _header.ProjectIDGUIDdata4[i]       := hdr.project_ID_GUID_data_4[i] ;
    _header.VersionMajor                  := hdr.version_major ;
    _header.VersionMinor                  := hdr.version_minor ;
    for i := 0 to 31 do
      _header.SystemIdentifier[i]         := hdr.system_identifier[i] ;
    for i := 0 to 31 do
      _header.GeneratingSoftware[i]       := hdr.generating_software[i] ;
    _header.FileCreationDayofYear         := hdr.file_creation_day ;
    _header.FileCreationYear              := hdr.file_creation_year ;
    _header.HeaderSize                    := hdr.header_size ;
    _header.OffsetToPointData             := hdr.offset_to_point_data ;
    _header.NumberofVariableLengthRecords := hdr.number_of_variable_length_records ;
    _header.PointDataFormatID             := hdr.point_data_format ;
    _header.PointDataRecordLength         := hdr.point_data_record_length ;
    _header.Numberofpointrecords          := hdr.number_of_point_records ;
    for i := 0 to 4 do
      _header.Numberofpointsbyreturn[i]   := hdr.number_of_points_by_return[i] ;
    _header.Xscalefactor                  := hdr.x_scale_factor ;
    _header.Yscalefactor                  := hdr.y_scale_factor ;
    _header.Zscalefactor                  := hdr.z_scale_factor ;
    _header.Xoffset                       := hdr.x_offset ;
    _header.Yoffset                       := hdr.y_offset ;
    _header.Zoffset                       := hdr.z_offset ;
    _header.MaxX                          := hdr.max_x ;
    _header.MinX                          := hdr.min_x ;
    _header.MaxY                          := hdr.max_y ;
    _header.MinY                          := hdr.min_y ;
    _header.MaxZ                          := hdr.max_z ;
    _header.MinZ                          := hdr.min_z ;

    if _header.NumberofVariableLengthRecords > 0 then begin
      {$IFDEF JAVA}
        vsize := Marshal.SizeOf( new laszip_vlr() ) ;
      {$ENDIF}
      for i := 0 to _header.NumberofVariableLengthRecords-1 do begin
        {$IFDEF OXYGENE}
          {$IFDEF CLR}
            vptr := New IntPtr( NativeInt(hdr.vlrs) + i * Marshal.SizeOf( typeOf(laszip_vlr)) ) ;
            vlrs := laszip_vlr(Marshal.PtrToStructure( vptr, typeOf(laszip_vlr) )) ;
          {$ENDIF}
          {$IFDEF JAVA}
            vptr := hdr.vlrs.share( i * vsize ) ;
            vlrs := laszip_vlr(Marshal.PtrToStructure( vptr, typeOf(laszip_vlr) )) ;
          {$ENDIF}
        {$ELSE}
          vlrs := plaszip_vlr( NativeInt( hdr.vlrs ) + i * sizeOf(laszip_vlr) ) ;
        {$ENDIF}
        vlr := TLAS_VariableLengthRecord.Create ;
        try
          vlr.RecordSignatureAABB := vlrs.reserved ;
          for j := 0 to 15 do
            vlr.UserID[j] := vlrs.user_id[j] ;
          vlr.RecordID := vlrs.record_id ;
          vlr.RecordLengthAfterHeader := vlrs.record_length_after_header  ;
          for j := 0 to 31 do
            vlr.Description[j] := vlrs.description[j] ;
          if vlr.RecordLengthAfterHeader > 0 then begin
            SetLength( vlr.Data, vlr.RecordLengthAfterHeader ) ;
            {$IFDEF OXYGENE}
              Marshal.Copy( vlrs.data, vlr.Data, 0, vlr.RecordLengthAfterHeader );
            {$ELSE}
              Move( vlrs.data^, vlr.Data[0], vlr.RecordLengthAfterHeader ) ;
            {$ENDIF}
          end ;
          vlr.ParseGeoKeys ;
          _fvlr.Add( vlr ) ;
        except
          FreeObject( vlr ) ;
        end ;
      end ;
    end ;

    if _header.NumberofExtendedVariableLengthRecords > 0 then begin
     { FStream.Position := FHeader.StartoffirstExtendedVariableLengthRecord ;
      for i := 0 to FHeader.NumberofExtendedVariableLengthRecords-1 do begin
        vlr := TLAS_VariableLengthRecord.Create ;
        try
          vlr.Read( FStream, True ) ;
          vlr.ParseGeoKeys ;
          _fvlr.Add( vlr ) ;
        except
          FreeObject( vlr ) ;
        end ;
      end ;}
    end ;
  end ;

  function T_LASZipLib.ReadPoint : Integer ;
  begin
    Result := laszip_read_point( PReader ) ;
  end ;

  function T_LASZipLib.ReadPointInside : Integer ;
  begin
    Result := laszip_read_inside_point( PReader, IsDone ) ;
  end ;

  function T_LASZipLib.SeekPoint(
    const _point : Integer
  ) : Integer ;
  begin
    Result := laszip_seek_point( PReader, _point ) ;
  end ;

  function T_LASZipLib.InitPointPtr : Integer ;
  begin
    PPoint := nil ;
    Result := laszip_get_point_pointer( PReader, PPoint ) ;
  end ;

  function T_LASZipLib.GetPointPtr : laszip_PTR ;
  begin
    Result := PPoint ;
  end ;

  function T_LASZipLib.GetPointIndex : Int64 ;
  begin
    Result := 0 ;
    check( laszip_get_point_count( PReader, Result ) ) ;
  end ;

  function T_LASZipLib.SetRectangle (
    const _extent : TGIS_Extent
  ) : Integer ;
  var
    is_empty : laszip_BOOL ;
  begin
    is_empty := 0 ;
    Result := laszip_inside_rectangle(
                PReader, _extent.XMin, _extent.YMin, _extent.XMax, _extent.YMax,
                is_empty
              ) ;
    IsDone := is_empty ;
  end ;

  function T_LASZipLib.ReadEof : Boolean ;
  begin
    Result := IsDone = 1 ;
  end ;

  {$IFDEF JAVA}
    {$IFNDEF ANDROID}
    function T_LASZipLib.laszip_get_version(
      var _version_major   : laszip_U8 ;
      var _version_minor   : laszip_U8 ;
      var _version_revision: laszip_U16 ;
      var _version_build   : laszip_U32
      ) : laszip_I32 ;
    var
      version_major    : com.sun.jna.ptr.ByteByReference ;
      version_minor    : com.sun.jna.ptr.ByteByReference ;
      version_revision : com.sun.jna.ptr.IntByReference ;
      version_build    : com.sun.jna.ptr.IntByReference ;
    begin
      version_major    := new com.sun.jna.ptr.ByteByReference() ;
      version_minor    := new com.sun.jna.ptr.ByteByReference() ;
      version_revision := new com.sun.jna.ptr.IntByReference() ;
      version_build    := new com.sun.jna.ptr.IntByReference() ;

      Result := DLLHandle.laszip_get_version( version_major, version_minor, version_revision, version_build ) ;

      _version_major   := version_major.Value ;
      _version_minor   := version_minor.Value ;
      _version_revision:= version_revision.Value ;
      _version_build   := version_build.Value ;
    end ;

    function T_LASZipLib.laszip_create(
      var _cPointer : laszip_PTR
    ) : laszip_I32;
    var
      ptr : com.sun.jna.ptr.PointerByReference ;
    begin
      ptr := new com.sun.jna.ptr.PointerByReference() ;

      Result := DLLHandle.laszip_create( ptr ) ;

      _cPointer := ptr.Value ;
    end;

    function T_LASZipLib.laszip_destroy(
      _cPointer : laszip_PTR
    ) : laszip_I32;
    begin
      Result := DLLHandle.laszip_destroy( _cPointer ) ;
    end;

    function T_LASZipLib.laszip_get_error(
      _cPointer : laszip_PTR;
      var _error : laszip_PTR
    ) : laszip_I32;
    var
      ptr : com.sun.jna.ptr.PointerByReference ;
    begin
      ptr := new com.sun.jna.ptr.PointerByReference() ;

      Result := DLLHandle.laszip_get_error( _cPointer, ptr ) ;

      _error := ptr.Value ;
    end;

    function T_LASZipLib.laszip_open_reader(
      _cPointer : laszip_PTR;
      _file_name : String;
      var _is_compressed : laszip_BOOL
    ) : laszip_I32;
    var
      is_compressed : com.sun.jna.ptr.IntByReference ;
    begin
      is_compressed := new com.sun.jna.ptr.IntByReference() ;

      Result := DLLHandle.laszip_open_reader( _cPointer, _file_name, is_compressed ) ;

      _is_compressed := is_compressed.Value ;
    end;

    function T_LASZipLib.laszip_close_reader(
      _cPointer : laszip_PTR
    ) : laszip_I32;
    begin
      Result := DLLHandle.laszip_close_reader( _cPointer ) ;
    end;

    function T_LASZipLib.laszip_get_header_pointer(
      _cPointer : laszip_PTR;
      var _header_pointer : laszip_PTR
    ) : laszip_I32;
    var
      ptr : com.sun.jna.ptr.PointerByReference ;
    begin
      ptr := new com.sun.jna.ptr.PointerByReference() ;

      Result := DLLHandle.laszip_get_header_pointer( _cPointer, ptr ) ;

      _header_pointer := ptr.Value ;
    end;

    function T_LASZipLib.laszip_get_point_pointer(
      _cPointer : laszip_PTR;
      var _point_pointer : laszip_PTR
    ) : laszip_I32;
    var
      ptr : com.sun.jna.ptr.PointerByReference ;
    begin
      ptr := new com.sun.jna.ptr.PointerByReference() ;

      Result := DLLHandle.laszip_get_point_pointer( _cPointer, ptr ) ;

      _point_pointer := ptr.Value ;
    end;

    function T_LASZipLib.laszip_read_point(
      _cPointer : laszip_PTR
    ) : laszip_I32;
    begin
      Result := DLLHandle.laszip_read_point( _cPointer ) ;
    end;

    function T_LASZipLib.laszip_seek_point(
      _cPointer : laszip_PTR;
      _index : laszip_I64
    ) : laszip_I32;
    begin
      Result := DLLHandle.laszip_seek_point( _cPointer, _index ) ;
    end;

    function T_LASZipLib.laszip_read_inside_point (
          _cPointer        : laszip_PTR ;
      var _is_done         : laszip_BOOL
    ) : laszip_I32 ;
    var
      is_done : com.sun.jna.ptr.IntByReference ;
    begin
      is_done := new com.sun.jna.ptr.IntByReference() ;

      Result := DLLHandle.laszip_read_inside_point( _cPointer, is_done ) ;

      _is_done := is_done.Value ;
    end ;

    function T_LASZipLib.laszip_inside_rectangle(
          _cPointer       : laszip_PTR ;
          _min_x          : laszip_F64 ;
          _min_y          : laszip_F64 ;
          _max_x          : laszip_F64 ;
          _max_y          : laszip_F64 ;
      var _is_empty       : laszip_BOOL
    ) : laszip_I32 ;
    var
      is_empty : com.sun.jna.ptr.IntByReference ;
    begin
      is_empty := new com.sun.jna.ptr.IntByReference() ;

      Result := DLLHandle.laszip_inside_rectangle( _cPointer, _min_x, _min_y, _max_x, _max_y, is_empty ) ;

      _is_empty := is_empty.Value ;
    end ;

    function T_LASZipLib.laszip_exploit_spatial_index(
      _cPointer       : laszip_PTR ;
      _exploit        : laszip_BOOL
    ) : laszip_I32 ;
    begin
      Result := DLLHandle.laszip_exploit_spatial_index( _cPointer, _exploit ) ;
    end ;

    function T_LASZipLib.laszip_get_point_count(
          _cPointer    : laszip_PTR;
      var _count       : laszip_I64
    ) : laszip_I32 ;
    var
      count : com.sun.jna.ptr.IntByReference ;
    begin
      count := new com.sun.jna.ptr.IntByReference() ;

      Result := DLLHandle.laszip_get_point_count( _cPointer, count ) ;

      _count := count.Value ;
    end ;

    {$ENDIF}
  {$ENDIF}
  {$ENDIF}

//==============================================================================
// TLAS_Reader
//==============================================================================

  constructor TLAS_Reader.Create(
    const _path : String
  ) ;
  var
    sdll : String ;
  begin
    inherited Create ;

    FHeader := TLAS_PublicHeaderBlock.Create ;
    FVLR    := TGIS_ObjectList.Create( True ) ;
    FPath   := _path ;
    FIsLaz  := CompareText( GetFileExt( _path ), '.laz' ) = 0 ;
    FIsLazInit := False ;

    if FIsLaz then begin
      FStream := nil ;
      {$IFDEF OXYGENE}
      {$ELSE}
        {$IFNDEF ANDROID}
        if not assigned( FLASLib ) then
          FLASLib := T_LASZipLib.Create ;
        {$ENDIF}
      {$ENDIF}

      {$IFDEF JAVA}
        sdll := LASZIP_DLL_NAME_NOEXT ;
      {$ELSE}
        sdll := LASZIP_DLL_NAME ;
      {$ENDIF}

      {$IFNDEF ANDROID}
      FIsLazInit := T_LASZipLib(FLASLib).LoadDLL( sdll ) ;
      {$ENDIF}
      if FIsLazInit then
        inc( FLASInstanceCount ) ;
    end
    else
      FStream := TGIS_BufferedFileStream.Create( _path, TGIS_StreamMode.Read ) ;
  end ;

  procedure TLAS_Reader.doDestroy ;
  var
    i : Integer ;
  begin
    FreeObject( FHeader ) ;
    FreeObject( FStream ) ;
    FreeObject( FVLR    ) ;

    dec( FLASInstanceCount ) ;

    if FLASInstanceCount <= 0 then begin
      {$IFDEF OXYGENE}
        {$IFNDEF ANDROID}
          T_LASZipLib(FLASLib).UnLoadDLL ;
        {$ENDIF}
      {$ELSE}
        FreeObject( FLASLib ) ;
      {$ENDIF}
    end ;

    for i := 0 to high( cursorState ) do
      CursorClose( i ) ;

    inherited ;
  end ;

  procedure TLAS_Reader.check(
    const _status : Integer
  ) ;
  var
    err : String ;
  begin
    if _status = 0 then exit ;
    {$IFNDEF ANDROID}
    err := T_LASZipLib(FLASLib).check( _status ) ;
    {$ENDIF}
    raise EGIS_Exception.Create( _rsrc( GIS_RS_ERR_FILEREAD ) + #13#10 +
                                  Format( '%s', [err] ),
                                  FPath, 0
                                ) ;

  end ;

  {$IFNDEF OXYGENE}
    {$HINTS OFF} // we know that few variables were not used
  {$ENDIF}
  function TLAS_Reader.prepareCS(
    const _vlr          : TLAS_VLRGeoKeys ;
    const _asciiparams  : String          ;
    const _doubleparams : TGIS_DoubleArray

  ): TGIS_CSCoordinateSystem;
  var
    i : Integer ;
  var
    lun  : TGIS_CSUnits ;
    aun  : TGIS_CSUnits ;
    flat : Double ;
    elp  : TGIS_CSEllipsoid ;
    pmd  : TGIS_CSPrimeMeridian ;
    dat  : TGIS_CSDatum ;
    gcs  : TGIS_CSGeographicCoordinateSystem ;
    pcs  : TGIS_CSProjectedCoordinateSystem  ;
    key  : TLAS_VLRKeyEntry ;

    keyGTModelType              : Integer ;
    keyGTRasterType             : Integer ;
    keyGTCitation               : String  ;
    keyGeographicType           : Integer ;
    keyGeogCitation             : String  ;
    keyGeogGeodeticDatum        : Integer ;
    keyGeogPrimeMeridian        : Integer ;
    keyGeogPrimeMeridianLong    : Double  ;
    keyGeogLinearUnits          : Integer ;
    keyGeogLinearUnitSize       : Double  ;
    keyGeogAngularUnits         : Integer ;
    keyGeogAngularUnitSize      : Double  ;
    keyGeogEllipsoid            : Integer ;
    keyGeogSemiMajorAxis        : Double  ;
    keyGeogSemiMinorAxis        : Double  ;
    keyGeogInvFlattering        : Double  ;
    keyGeogAzimuth              : Integer ;
    keyGeogTOWGS84              : Double  ;
    keyProjectedCSType          : Integer ;
    keyPCSCitation              : String  ;
    keyProjected                : Integer ;
    keyProjCoordTrans           : Integer ;
    keyProjLinearUnits          : Integer ;
    keyProjLinearUnitSize       : Double  ;
    keyProjStdParallel1         : Double  ;
    keyProjStdParallel2         : Double  ;
    keyProjNatOriginLong        : Double  ;
    keyProjNatOriginLat         : Double  ;
    keyProjFalseEasting         : Double  ;
    keyProjFalseNorthing        : Double  ;
    keyProjFalseOriginLong      : Double  ;
    keyProjFalseOriginLat       : Double  ;
    keyProjFalseOriginEasting   : Double  ;
    keyProjFalseOriginNorthing  : Double  ;
    keyProjCenterLong           : Double  ;
    keyProjCenterLat            : Double  ;
    keyProjCenterEasting        : Double  ;
    keyProjCenterNorthing       : Double  ;
    keyProjScaleAtNatOrigin     : Double  ;
    keyProjScaleAtCenter        : Double  ;
    keyProjAzimuthAngle         : Double  ;
    keyProjStraightVertPoleLong : Double  ;
    // keyGeogAzimuthUnits
    keyVerticalCSType           : SmallInt ;
    keyVerticalCitation         : String  ;
    keyVerticalDatum            : SmallInt ;
    keyVerticalUnits            : SmallInt ;

    is_lun                      : Boolean ; // exact angular units by TAG
    is_aun                      : Boolean ; // exact linear units by TAG
    is_elp                      : Boolean ; // exact ellipsoid by TAG
    is_pmd                      : Boolean ; // exact prime meridian by TAG
    is_dat                      : Boolean ; // exact datum by TAG
    is_gcs                      : Boolean ; // exact gcs by TAG

  const
    USER_DEFINED = 32767 ;

    function key_value : Variant ;
    begin
      case key.TIFFTagLocation of
        0     : Result := key.ValueOffset ;
        34736 : Result := _doubleparams[key.ValueOffset] ;
        34737 : Result := Copy( _asciiparams,
                                key.ValueOffset + StringFirst,
                                key.Count - 1
                              ) ;

      else
        assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
      end ;
    end ;

    function key_string : String ;
    begin
      Result := VarToString( key_value ) ;
    end ;

    function key_double : Double ;
    begin
      Result := VarToDouble( key_value ) ;
    end ;

    function key_short : Word ;
    begin
      Result := VarToUInt16( key_value ) ;
    end ;

    function defval(
      const _default : Double ;
      const _value1  : Double ;
      const _value2  : Double = NaN ;
      const _value3  : Double = NaN
    ) : Double ;
    begin
      if IsNan( _value1 ) then begin
        if IsNan( _value2 ) then begin
          if IsNan( _value3 ) then begin
            Result := _default ;
          end
          else begin
            Result := _value3 ;
          end ;
        end
        else begin
          Result := _value2 ;
        end
      end
      else begin
        Result := _value1 ;
      end ;
    end ;

    function defdeg(
      const _default : Double ;
      const _value1  : Double ;
      const _value2  : Double = NaN ;
      const _value3  : Double = NaN
    ) : Double ;
    begin
      Result := DegToRad( defval( _default, _value1, _value2, _value3 ) ) ;
    end ;

    function extract_ESRI_PE_String(
      const _txt : String
    ) : String ;
    var
      k   : Integer ;
      fnd : Boolean ;
      sleft  : TStringBuilder ;
      sright : TStringBuilder ;
    begin
      Result := '' ;

      sleft  :=  TStringBuilder.Create ;
      sright :=  TStringBuilder.Create ;
      try
        fnd := False ;

        for k := StringFirst to StringLast( _txt ) do begin
          if _txt[k] = '=' then begin
            if not fnd then begin
              fnd := True ;
              continue ;
            end
            else begin
              // unexpected
              sright.Clear ;
              break ;
            end ;
          end ;

          if not fnd then sleft.Append( _txt[k] )
                     else sright.Append( _txt[k] )

        end ;

        if Trim( sleft.ToString ) = 'ESRI PE String' then
          Result := sright.ToString ;
      finally
        FreeObject( sleft  ) ;
        FreeObject( sright ) ;
      end ;
    end ;

    function pcs_by_citation(
      const _txt : String
    ) : TGIS_CSProjectedCoordinateSystem ;
    var
      wkt : String ;
      cs  : TGIS_CSCoordinateSystem ;
    begin
      Result := nil ;

      cs := TGIS_CSFactory.ByWKT( _txt ) ;
      if cs is TGIS_CSProjectedCoordinateSystem then begin
        Result := TGIS_CSProjectedCoordinateSystem( cs ) ;
        exit ;
      end ;

      wkt := extract_ESRI_PE_String( _txt ) ;
      cs := TGIS_CSFactory.ByWKT( wkt ) ;
      if cs is TGIS_CSProjectedCoordinateSystem then begin
        Result := TGIS_CSProjectedCoordinateSystem( cs ) ;
        exit ;
      end ;
    end ;

    function setup_projection(
      const _gcs : TGIS_CSGeographicCoordinateSystem ;
      const _unt : TGIS_CSUnits
    ) : TGIS_CSProjectedCoordinateSystem ;
    var
      prm : TGIS_CSProjParameters ;
      prj : Integer ;
      tx  : String ;
    begin
      Result := nil ;

      case keyProjCoordTrans of
         1 : // CT_TransverseMercator
           begin
             prj := CSPROJ_Transverse_Mercator ;
             tx  := 'CT_Transverse_Mercator' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong     ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat      ) ;
             prm.ScaleFactor        := defval( 0, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         2 : // CT_TransvMercator_Modified_Alaska,
           begin
             prj := -1 ;//?
             tx  := 'CT_TransvMercator_Modified_Alaska' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defval( 0, keyProjNatOriginLong     ) ;
             prm.LatitudeOfOrigin   := defval( 0, keyProjNatOriginLat      ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;

             assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
           end ;
         3 : // CT_ObliqueMercator,
           begin
             prj := -1 ; //?9815 ; //?
             tx  := 'CT_ObliqueMercator' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjCenterLong        ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjCenterLat         ) ;
             prm.Azimuth            := defdeg( 0, keyProjAzimuthAngle      ) ;
             //?RectifiedGridAngle recitified_grid_angle Angular
             prm.ScaleFactor        := defval( 1, keyProjScaleAtCenter     ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         4 : // CT_ObliqueMercator_Laborde,
           begin
             prj := -1 ; //?
             tx  := 'CT_ObliqueMercator_Laborde' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjCenterLong        ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjCenterLat         ) ;
             prm.Azimuth            := defdeg( 0, keyProjAzimuthAngle      ) ;
             //?RectifiedGridAngle recitified_grid_angle Angular (90)
             prm.ScaleFactor        := defval( 1, keyProjScaleAtCenter     ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         5 : // CT_ObliqueMercator_Rosenmund,
           begin
             prj := -1 ; //?
             tx  := 'CT_ObliqueMercator_Rosenmund' ;
             // not implemented
             end ;
         6 : // CT_ObliqueMercator_Spherical,
           begin
           assert( False ) ;
             prj := -1 ;
             // not implemented
           end ;
         7 : // CT_Mercator,
           begin
             if IsNan( keyProjCenterLat    ) and
                IsNan( keyProjNatOriginLat ) and
                IsNan( keyProjStdParallel1 )
             then begin
               prj := CSPROJ_Mercator_1SP ;
               tx  := 'CT_Mercator' ;
             end
             else begin
               prj := CSPROJ_Mercator_2SP ;
               tx  := 'CT_Mercator_SP2' ;
             end ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;

             prm.CentralMeridian    := defdeg( 0,
                                               keyProjCenterLong      ,
                                               keyProjNatOriginLong
                                             ) ;
             prm.StandardParallel_1 := defdeg( 0,
                                               keyProjCenterLat       ,
                                               keyProjNatOriginLat    ,
                                               keyProjStdParallel1
                                             ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0,
                                               keyProjCenterEasting   ,
                                               keyProjFalseEasting
                                             ) ;
             prm.FalseNorthing      := defval( 0,
                                               keyProjCenterNorthing  ,
                                               keyProjFalseNorthing
                                             ) ;
           end ;
         8 : // CT_LambertConfConic_2SP,
           begin
             prj := CSPROJ_Lambert_Conformal_Conic_2SP ;
             tx  := 'CT_LambertConfConic_2SP' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0,
                                               keyProjFalseOriginLong ,
                                               keyProjNatOriginLong
                                             ) ;
             prm.LatitudeOfOrigin   := defdeg( 0,
                                               keyProjFalseOriginLat  ,
                                               keyProjNatOriginLat
                                             ) ;
             prm.StandardParallel_1 := defdeg( 0, keyProjStdParallel1      ) ;
             prm.StandardParallel_2 := defdeg( 0, keyProjStdParallel2      ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         9 : // CT_LambertConfConic_1SP
           begin
             prj := CSPROJ_Lambert_Conformal_Conic_1SP ;
             tx  := 'CT_LambertConfConic_1SP' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defval( 0, keyProjNatOriginLong     ) ;
             prm.LatitudeOfOrigin   := defval( 0, keyProjNatOriginLat      ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         10 : // CT_LambertAzimEqualArea,
           begin
             prj := CSPROJ_Azimuthal_Equal_Area ;
             tx  := 'CT_LambertAzimEqualArea' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0,
                                               keyProjCenterLong      ,
                                               keyProjNatOriginLong
                                             ) ;
             prm.LatitudeOfOrigin   := defdeg( 0,
                                               keyProjCenterLat       ,
                                               keyProjNatOriginLat
                                             ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         11 : // CT_AlbersEqualArea,
           begin
             prj := CSPROJ_Albers ;
             tx  := 'CT_AlbersEqualArea' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong     ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat      ) ;
             prm.StandardParallel_1 := defdeg( 29.5, keyProjStdParallel1   ) ;
             prm.StandardParallel_2 := defdeg( 45.4, keyProjStdParallel2   ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         12 : // CT_AzimuthalEquidistant,
           begin
             prj := CSPROJ_Azimuthal_Equidistance ;
             tx  := 'CT_AzimuthalEquidistant' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.LongitudeOfCenter  := defdeg( 0, keyProjCenterLong        ) ;
             prm.LatitudeOfCenter   := defdeg( 0, keyProjCenterLat         ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         13 : // CT_EquidistantConic,
           begin
             prj := CSPROJ_Equidistant_Conic ;
             tx  := 'CT_EquidistantConic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.StandardParallel_1 := defdeg( 20.0, keyProjStdParallel1   ) ;
             prm.StandardParallel_2 := defdeg( 60.0, keyProjStdParallel2   ) ;
             prm.LongitudeOfCenter  := defdeg( 0, keyProjCenterLong        ) ;
             prm.LatitudeOfCenter   := defdeg( 0, keyProjCenterLat         ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         14 : // CT_Stereographic,
           begin
             prj := CSPROJ_Double_Stereographic ;
             tx  := 'CT_Stereographic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0,
                                               keyProjCenterLong      ,
                                               keyProjNatOriginLong
                                             ) ;
             prm.LatitudeOfOrigin   := defdeg( 0,
                                               keyProjCenterLat       ,
                                               keyProjNatOriginLat
                                             ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         15 : // CT_PolarStereographic,
           begin
             prj := CSPROJ_Polar_Stereographic ;
             tx  := 'CT_PolarStereographic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0,
                                               keyProjStraightVertPoleLong,
                                               keyProjNatOriginLong
                                             ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat      ) ;
             prm.StandardParallel_1 := defdeg( 0,
                                               keyProjCenterLat       ,
                                               keyProjNatOriginLat
                                             ) ;
             prm.StandardParallel_1 := defdeg( 0, prm.StandardParallel_1   ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         16 : // CT_ObliqueStereographic,
           begin
             prj := 9809 ;
             tx  := 'CT_ObliqueStereographic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong     ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat      ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
          end ;
         17 : // CT_Equirectangular,
           begin
             prj := CSPROJ_Plate_Carree ;
             tx  := 'CT_Equirectangular' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjCenterLong        ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjCenterLat         ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
             prm.StandardParallel_1 := defdeg( 0, keyProjStdParallel1      ) ;
           end ;
         18 : // CT_CassiniSoldner,
           begin
             prj := CSPROJ_Cassini_Soldner ;
             tx  := 'CT_CassiniSoldner' ;

             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong     ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat      ) ;
             prm.ScaleFactor        := defval( 0, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         19 : // CT_Gnomonic,
           begin
             prj := CSPROJ_Gnomic ;
             tx  := 'CT_Gnomic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjCenterLong        ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjCenterLat         ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         20 : // CT_MillerCylindrical,
           begin
             prj := CSPROJ_Miller_Cylindrical ;
             tx  := 'CT_MillerCylindrical' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0,
                                               keyProjCenterLong      ,
                                               keyProjNatOriginLong
                                             ) ;
             prm.LatitudeOfOrigin   := defdeg( 0,
                                               keyProjCenterLat       ,
                                               keyProjNatOriginLat
                                             ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         21 : // CT_Orthographic,
           begin
             prj := CSPROJ_Orthographic ;
             tx  := 'CT_Orthographic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjCenterLong        ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjCenterLat         ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         22 : // CT_Polyconic,
           begin
             prj := CSPROJ_Polyconic ;
             tx  := 'CT_Polyconic' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong     ) ;
             prm.LatitudeOfOrigin   := defdeg( 0, keyProjNatOriginLat      ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         23 : // CT_Robinson,
           begin
             prj := CSPROJ_Robinson ;
             tx  := 'CT_Robinson' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0,
                                               keyProjCenterLong      ,
                                               keyProjNatOriginLong
                                             ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         24 : // CT_Sinusoidal,
           begin
             prj := CSPROJ_Sinusoidal ;
             tx  := 'CT_Sinusoidal' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0,
                                               keyProjCenterLong      ,
                                               keyProjNatOriginLong
                                             ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         25 : // CT_VanDerGrinten,
           begin
             prj := CSPROJ_van_der_Grinten_I ;
             tx  := 'CT_VanDerGrinten' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0,
                                               keyProjCenterLong      ,
                                               keyProjNatOriginLong
                                             ) ;
             prm.CentralMeridian    := defdeg( 0, prm.CentralMeridian      ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         26 : // CT_NewZealandMapGrid,
           begin
             prj := CSPROJ_New_Zealand_Map_Grid ;
             tx  := 'CT_NewZealandMapGrid' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 173.0, keyProjNatOriginLong   ) ;
             prm.LatitudeOfOrigin   := defdeg( -41.0, keyProjNatOriginLat    ) ;
             prm.FalseEasting       := defval( 2510000, keyProjFalseEasting  ) ;
             prm.FalseNorthing      := defval( 6023150, keyProjFalseNorthing ) ;
           end ;
         27 : // CT_TransvMercator_SouthOrientated,
           begin
             prj := CSPROJ_Oblique_Stereographic ;
             tx  := 'CT_TransvMercator_SouthOrientated' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defval( 0, keyProjNatOriginLong     ) ;
             prm.LatitudeOfOrigin   := defval( 0, keyProjNatOriginLat      ) ;
             prm.LatitudeOfOrigin   := defval( 0, keyProjNatOriginLat      ) ;
             prm.ScaleFactor        := defval( 1, keyProjScaleAtNatOrigin  ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         28 : // CT_CylindricalEqualArea,
           begin
             prj := CSPROJ_Cylindrical_Equal_Area ;
             tx  := 'CT_CylindricalEqualArea' ;

             prm := CSProjectedCoordinateSystemList.DefaultParams( prj ) ;
             prm.CentralMeridian    := defdeg( 0, keyProjNatOriginLong     ) ;
             prm.StandardParallel_1 := defdeg( 0, keyProjStdParallel1      ) ;
             prm.FalseEasting       := defval( 0, keyProjFalseEasting      ) ;
             prm.FalseNorthing      := defval( 0, keyProjFalseNorthing     ) ;
           end ;
         -1  : // Undefined
           begin
             exit ;
           end ;
         USER_DEFINED :
           begin
             exit ;
           end
         else
           begin
             assert( False, _rsrc( GIS_RS_ERR_UNTESTED ) ) ;
             exit ;
           end ;
      end ;

      if prj < 0 then exit ;

      if (length( keyPCSCitation ) > 5) and (length( keyPCSCitation ) < 100) then
      begin
        // some significant citation
        tx := StringReplace( keyPCSCitation, #13, ' ', [{$IFDEF OXYGENE}TReplaceFlag.{$ENDIF}rfReplaceAll] ) ;
        tx := StringReplace( tx, #10, '', [{$IFDEF OXYGENE}TReplaceFlag.{$ENDIF}rfReplaceAll] ) ;
      end
      else
        tx := tx + '_' + _gcs.WKT ;

      Result := CSProjectedCoordinateSystemList.Prepare(
                  -1, tx, _gcs.EPSG, _unt.EPSG, prj, prm
                ) ;
    end ;

  begin
    Result := CSUnknownCoordinateSystem ;

    if not assigned( _vlr ) then exit ;

    keyGTModelType                 := -1   ;
    keyGTRasterType                := -1   ;
    keyGTCitation                  := ''   ;
    keyGeographicType              := -1   ;
    keyGeogCitation                := ''   ;
    keyGeogGeodeticDatum           := -1   ;
    keyGeogPrimeMeridian           := 8901 ;
    keyGeogPrimeMeridianLong       := NaN  ;
    keyGeogLinearUnits             := 9001 ;
    keyGeogLinearUnitSize          := NaN  ;
    keyGeogAngularUnits            := 9102 ;
    keyGeogAngularUnitSize         := NaN  ;
    keyGeogEllipsoid               := -1   ;
    keyGeogSemiMajorAxis           := NaN  ;
    keyGeogSemiMinorAxis           := NaN  ;
    keyGeogInvFlattering           := NaN  ;
    keyGeogAzimuth                 := -1   ;
    keyGeogTOWGS84                 := NaN  ;
    keyProjectedCSType             := -1   ;
    keyPCSCitation                 := ''   ;
    keyProjected                   := -1   ;
    keyProjCoordTrans              := -1   ;
    keyProjLinearUnits             := 9001 ;
    keyProjLinearUnitSize          := NaN  ;
    keyProjStdParallel1            := NaN  ;
    keyProjStdParallel2            := NaN  ;
    keyProjNatOriginLong           := NaN  ;
    keyProjNatOriginLat            := NaN  ;
    keyProjFalseEasting            := NaN  ;
    keyProjFalseNorthing           := NaN  ;
    keyProjFalseOriginLong         := NaN  ;
    keyProjFalseOriginLat          := NaN  ;
    keyProjFalseOriginEasting      := NaN  ;
    keyProjFalseOriginNorthing     := NaN  ;
    keyProjCenterLong              := NaN  ;
    keyProjCenterLat               := NaN  ;
    keyProjCenterEasting           := NaN  ;
    keyProjCenterNorthing          := NaN  ;
    keyProjScaleAtNatOrigin        := NaN  ;
    keyProjScaleAtCenter           := NaN  ;
    keyProjAzimuthAngle            := NaN  ;
    keyProjStraightVertPoleLong    := NaN  ;
    keyVerticalCSType              := -1   ;
    keyVerticalCitation            := ''   ;
    keyVerticalDatum               := -1   ;
    keyVerticalUnits               := -1   ;

    for i := 0 to _vlr.NumberOfKeys-1 do begin
      key := _vlr.FKeys[i] ;
      case key.KeyID of
        0    : // Empty key
          begin
          end ;
        1024 : // GTModelType
          begin
            keyGTModelType := key_short ;
          end ;
        1025 : // GTRasterType
          begin
            keyGTRasterType := key_short ;
          end ;
        1026 : // GTCitation
          begin
            keyGTCitation := key_string ;
          end ;
        2048 : // GeographicType
          begin
            keyGeographicType := key_short ;
          end ;
        2049 : // GeogCitation
          begin
            keyGeogCitation := key_string ;
          end ;
        2050 : // GeogGeodeticDatum
          begin
            keyGeogGeodeticDatum := key_short ;
          end ;
        2051 : // GeogPrimeMeridian
          begin
            keyGeogPrimeMeridian := key_short ;
          end ;
        2061 : // GeogPrimeMeridianLong
          begin
            keyGeogPrimeMeridianLong := key_double ;
          end ;
        2052 : // GeogLinearUnits
          begin
            keyGeogLinearUnits := key_short ;
          end ;
        2053 : // GeogLinearUnitSize
          begin
            keyGeogLinearUnitSize := key_double ;
          end ;
        2054 : // GeogAngularUnits
          begin
            keyGeogAngularUnits := key_short ;
          end ;
        2055 : // GeogAngularUnitSize
          begin
            keyGeogAngularUnitSize := key_double ;
          end ;
        2056 : // GeogEllipsoid
          begin
            keyGeogEllipsoid := key_short ;
          end ;
        2057 : // GeogSemiMajorAxis
          begin
            keyGeogSemiMajorAxis := key_double ;
          end ;
        2058 : // GeogSemiMinorAxis
          begin
            keyGeogSemiMinorAxis := key_double ;
          end ;
        2059 : // GeogInvFlattering
          begin
            keyGeogInvFlattering := key_double ;
          end ;
        2060 : // GeogAzimuth
          begin
            keyGeogAzimuth := key_short ;
          end ;
        2062 : // GeogTOWGS84GeoKey
          begin
            keyGeogTOWGS84 := key_double ;
          end ;
        3072 : // ProjectedCSType
          begin
            keyProjectedCSType := key_short ;
          end ;
        3073 : // PCSCitation
          begin
            keyPCSCitation := key_string ;
          end ;
        3074 : // Projected
          begin
            keyProjected := key_short ;
          end ;
        3075 : // ProjCoordTrans
          begin
            keyProjCoordTrans := key_short ;
            if keyProjCoordTrans = USER_DEFINED then
              exit ;
          end ;
        3076 : // ProjLinearUnits
          begin
            keyProjLinearUnits := key_short ;
          end ;
        3077 : // ProjLinearUnitSize
          begin
            keyProjLinearUnitSize := key_double ;
          end ;
        3078 : // ProjStdParallel1
          begin
            keyProjStdParallel1 := key_double ;
          end ;
        3079 : // ProjStdParallel2
          begin
            keyProjStdParallel2 := key_double ;
          end ;
        3080 : // ProjNatOriginLong
          begin
            keyProjNatOriginLong := key_double ;
          end ;
        3081 : // ProjNatOriginLat
          begin
            keyProjNatOriginLat := key_double ;
          end ;
        3082 : // ProjFalseEasing
          begin
            keyProjFalseEasting := key_double ;
          end ;
        3083 : // ProjFalseNorthing
          begin
            keyProjFalseNorthing := key_double ;
          end ;
        3084 : // ProjFalseOriginLong
          begin
            keyProjFalseOriginLong := key_double ;
          end ;
        3085 : // ProjFalseOriginLat
          begin
            keyProjFalseOriginLat := key_double ;
          end ;
        3086 : // ProjFalseOriginEasting
          begin
            keyProjFalseOriginEasting := key_double ;
          end ;
        3087 : // ProjFalseOriginNorthing
          begin
            keyProjFalseOriginNorthing := key_double ;
          end ;
        3088 : // ProjCenterLong
          begin
            keyProjCenterLong := key_double ;
          end ;
        3089 : // ProjCenterLat
          begin
            keyProjCenterLat := key_double ;
          end ;
        3090 : // ProjCenterEasting
          begin
            keyProjCenterEasting := key_double ;
          end ;
        3091 : // ProjCenterNorthing
          begin
            keyProjCenterNorthing := key_double ;
          end ;
        3092 : // ProjScaleAtNatOrigin
          begin
            keyProjScaleAtNatOrigin := key_double ;
          end ;
        3093 : // ProjScaleAtCenter
          begin
            keyProjScaleAtCenter := key_double ;
          end ;
        3094 : // ProjAzimuthAngle
          begin
            keyProjAzimuthAngle := key_double ;
          end ;
        3095 : // ProjStraightVertPoleLong
          begin
            keyProjStraightVertPoleLong := key_double ;
          end ;
        4096 : // VerticalCSType
          begin
            keyVerticalCSType := key_short ;
          end ;
        4097 : // VerticalCitation
          begin
            keyVerticalCitation := key_string ;
          end ;
        4098 : // VerticalDatum
          begin
            keyVerticalDatum := key_short ;
          end ;
        4099 : // VerticalUnits
          begin
            keyVerticalUnits := key_short ;
          end ;
      end ;
    end ;

    is_aun := False ;
    is_lun := False ;
    is_elp := False ;
    is_pmd := False ;
    is_dat := False ;
    is_gcs := False ;

    // construct angular units
       aun := nil ;
       if ( keyGeogAngularUnits > 0            ) and
          ( keyGeogAngularUnits < USER_DEFINED )
       then begin
         aun := CSUnitsList.ByEPSG( keyGeogAngularUnits ) ;
         is_aun := True ;
       end
       else begin
         if not IsNan( keyGeogAngularUnitSize ) then begin
           aun := CSUnitsList.Prepare(
                    -1, 'Custom_Angular', '',
                    TGIS_CSUnitsType.Angular, keyGeogAngularUnitSize
                  )
         end ;
       end ;

    // construct linear units
       lun := nil ;
       if ( keyProjLinearUnits > 0            ) and
          ( keyProjLinearUnits < USER_DEFINED )
       then begin
         lun := CSUnitsList.ByEPSG( keyProjLinearUnits ) ;
         is_lun := True ;
       end
       else begin
         if not IsNan( keyProjLinearUnitSize ) then begin
           lun := CSUnitsList.Prepare(
                    -1, 'Custom_Linear', '',
                    TGIS_CSUnitsType.Linear, keyProjLinearUnitSize
                  )
         end ;
       end ;

    // construct ellipsoid
      elp := nil ;
      if ( keyGeogEllipsoid > 0            ) and
         ( keyGeogEllipsoid < USER_DEFINED )
      then begin
        elp := CSEllipsoidList.ByEPSG( keyGeogEllipsoid ) ;
      end
      else begin
        if not IsNan( keyGeogSemiMajorAxis ) then begin
          flat := 0;

          if not IsNan( keyGeogSemiMinorAxis ) and
             not GisIsSameValue( keyGeogSemiMajorAxis, keyGeogSemiMinorAxis ) then
          begin
            flat := keyGeogSemiMajorAxis /
                    ( keyGeogSemiMajorAxis - keyGeogSemiMinorAxis ) ;
          end
          else
          if not IsNan( keyGeogInvFlattering ) then
          begin
            flat := keyGeogInvFlattering ;
          end ;

          elp := CSEllipsoidList.Prepare(
                   -1, 'Custom_Ellipsoid', keyGeogSemiMajorAxis, flat )
        end ;
      end ;

    // construct primem
       pmd := nil ;
       if ( keyGeogPrimeMeridian > 0            ) and
          ( keyGeogPrimeMeridian < USER_DEFINED )
       then begin
         pmd := CSPrimeMeridianList.ByEPSG( keyGeogPrimeMeridian ) ;
         is_pmd := True ;
       end
       else begin
         if not IsNan( keyGeogPrimeMeridianLong ) then begin
           pmd := CSPrimeMeridianList.Prepare(
                    -1, 'Custom_Primem', DegToRad( keyGeogPrimeMeridianLong )
                  ) ;
         end ;
       end ;

    // construct datum
       dat := nil ;
       if ( keyGeogGeodeticDatum > 0            ) and
          ( keyGeogGeodeticDatum < USER_DEFINED )
       then begin
         dat := CSDatumList.ByEPSG( keyGeogGeodeticDatum ) ;
         is_dat := True ;
       end
       else begin
         if assigned( elp ) then begin
           dat := CSDatumList.Prepare(
                    -1, 'Based_on_' + elp.WKT, elp.EPSG, 0
                  ) ;
         end ;
       end ;

    // construct GCS
       gcs := nil ;
       if ( keyGeographicType > 0            ) and
          ( keyGeographicType < USER_DEFINED )
       then begin
         gcs := CSGeographicCoordinateSystemList.ByEPSG( keyGeographicType ) ;
         is_gcs := True ;
       end ;

       if assigned( dat ) and
          assigned( pmd ) and
          assigned( aun ) then
       begin
         if assigned( gcs ) and
            (
              ( ( gcs.Datum.EPSG         <> dat.EPSG ) and is_dat ) or
              ( ( gcs.PrimeMeridian.EPSG <> pmd.EPSG ) and is_pmd ) or
              ( ( gcs.Units.EPSG         <> aun.EPSG ) and is_aun )
            )
         then
           gcs := nil ;

         if not assigned( gcs ) then
           gcs := CSGeographicCoordinateSystemList.Prepare(
                    -1, 'Custom_GCS_' + dat.WKT , dat.EPSG, pmd.EPSG, aun.EPSG
                  )
       end ;

    // construct PCS
       pcs := nil ;
       if ( keyProjectedCSType > 0            ) and
          ( keyProjectedCSType < USER_DEFINED )
       then
         pcs := CSProjectedCoordinateSystemList.ByEPSG( keyProjectedCSType ) ;

       if assigned( pcs ) and assigned( gcs ) then begin
         if ( pcs.Geocs.EPSG <> gcs.EPSG ) and is_gcs then begin
           pcs := CSProjectedCoordinateSystemList.Prepare(
                    -1, 'Custom_PCS_' + pcs.WKT ,
                    gcs.EPSG,
                    pcs.Units.EPSG,
                    pcs.Projection.EPSG,
                    pcs.Projection.Parameters
                  )
         end ;
       end ;

       if assigned( pcs ) and assigned( lun ) then begin
         if ( pcs.Units.EPSG <> lun.EPSG ) and is_lun then begin
           pcs := CSProjectedCoordinateSystemList.Prepare(
                    -1, 'Custom_PCS_' + pcs.WKT ,
                    pcs.Geocs.EPSG,
                    lun.EPSG,
                    pcs.Projection.EPSG,
                    pcs.Projection.Parameters
                  )
         end ;
       end ;

       if not assigned( pcs ) then begin
         if assigned( gcs ) and assigned( lun ) then begin
           pcs := setup_projection( gcs, lun ) ;
         end ;
       end ;

       if not assigned( pcs ) then begin
         if keyProjectedCSType = USER_DEFINED then begin
           pcs := pcs_by_citation( keyPCSCitation ) ;
         end ;
       end ;

    // final assignments
      case keyGTModelType  of
        1 :  begin
               if assigned( pcs ) then
                 Result := pcs
               else
                 Result := CSUnknownCoordinateSystem ;
             end ;
        2 :  begin
               if assigned( gcs ) then
                 Result := gcs
               else
                 Result := CSUnknownCoordinateSystem ;
             end ;
        else begin
               Result := CSUnknownCoordinateSystem
             end ;
      end ;

  end ;
  {$IFNDEF OXYGENE}
    {$HINTS ON}
  {$ENDIF}

  procedure TLAS_Reader.preparePoint(
    const _cursor : Integer
  ) ;
  begin
    if not assigned( cursorState[_cursor].curPoint ) then begin
      if FIsLazInit then begin
        {$IFNDEF ANDROID}
        check( T_LASZipLib(FLASLib).InitPointPtr ) ;
        cursorState[_cursor].curPoint := TLAS_PointDataRecordFormatLaz.Create(
                                           T_LASZipLib(FLASLib).GetPointPtr
                                         ) ;
        {$ENDIF}
      end
      else begin
        case FHeader.PointDataFormatID of
          0  : cursorState[_cursor].curPoint := TLAS_PointDataRecordFormat0.Create ;
          1  : cursorState[_cursor].curPoint := TLAS_PointDataRecordFormat1.Create ;
          2  : cursorState[_cursor].curPoint := TLAS_PointDataRecordFormat2.Create ;
          3  : cursorState[_cursor].curPoint := TLAS_PointDataRecordFormat3.Create ;
          4  : cursorState[_cursor].curPoint := TLAS_PointDataRecordFormat4.Create ;
          5  : cursorState[_cursor].curPoint := TLAS_PointDataRecordFormat5.Create ;
          6  : cursorState[_cursor].curPoint := TLAS_PointDataRecordFormat6.Create ;
          7  : cursorState[_cursor].curPoint := TLAS_PointDataRecordFormat7.Create ;
          8  : cursorState[_cursor].curPoint := TLAS_PointDataRecordFormat8.Create ;
          9  : cursorState[_cursor].curPoint := TLAS_PointDataRecordFormat9.Create ;
          10 : cursorState[_cursor].curPoint := TLAS_PointDataRecordFormat10.Create
        else  assert( False, 'Unsupported point format' ) ;
        end ;
      end ;
    end ;
  end ;

  procedure TLAS_Reader.CursorOpen(
    const _cursor : Integer
  ) ;
  begin
    if _cursor >= length( cursorState )  then
      SetLength( cursorState, _cursor + 1 ) ;
    {$IFDEF GIS_NORECORDS}
      if not assigned( cursorState[_cursor] ) then
        cursorState[_cursor] := new T_cursorStateLAS ;
    {$ENDIF}
    cursorState[ _cursor ].curInUse := True ;
  end ;

  procedure TLAS_Reader.CursorClose(
    const _cursor : Integer
  ) ;
  var
    i : Integer ;
  begin
    cursorState[_cursor].curInUse := False ;

    // truncate cursorState at the tail;
    {$IFNDEF OXYGENE}
      for i := Length( cursorState ) - 1 downto 0 do begin
    {$ELSE}
      for i := cursorState.Length - 1 downto 0 do begin
    {$ENDIF}
      if not cursorState[i].curInUse then begin
        FreeObject( cursorState[i].curPoint  ) ;
        SetLength( cursorState, i ) ;
      end
      else
        break ;
    end ;
  end ;

  function TLAS_Reader.Open : Boolean ;
  var
    i          : Integer ;
    vlr        : TLAS_VariableLengthRecord ;
    mpo        : Int64 ;
    v_major    : laszip_U8 ;
    v_minor    : laszip_U8 ;
    v_revision : laszip_U16 ;
    v_build    : laszip_U32 ;
  begin
    Result := False ;

    if FIsLaz then begin
      if not FIsLazInit then exit ;
      {$IFNDEF ANDROID}
      check( T_LASZipLib(FLASLib).laszip_get_version(
               v_major, v_minor, v_revision, v_build
             )
      ) ;
      check( T_LASZipLib(FLASLib).CreateReader ) ;
      check( T_LASZipLib(FLASLib).EnableSpatialIndex ) ;
      check( T_LASZipLib(FLASLib).OpenReader( FPath ) ) ;
      check( T_LASZipLib(FLASLib).ReadHeader( FHeader, FVLR ) ) ;
      {$ENDIF}
      if not FHeader.Check then exit ;

      if FHeader.StartofWaveformDataPacketRecord > 0 then
        FMaxPOffset := FHeader.StartofWaveformDataPacketRecord
      else if FHeader.StartoffirstExtendedVariableLengthRecord > 0 then
        FMaxPOffset := FHeader.StartoffirstExtendedVariableLengthRecord
      else begin
        mpo := Int64(FHeader.Numberofpointrecords)*FHeader.PointDataRecordLength +
               FHeader.OffsetToPointData ;
        FMaxPOffset := mpo ;
      end ;
      if FHeader.Numberofpointrecords > 0 then
        FMaxPoints := FHeader.Numberofpointrecords
      else
        FMaxPoints := FHeader.ExNumberofpointrecords ;
    end
    else begin
      FHeader.Clean ;
      FHeader.Read( FStream ) ;

      if not FHeader.Check then exit ;

      if FHeader.StartofWaveformDataPacketRecord > 0 then
        FMaxPOffset := FHeader.StartofWaveformDataPacketRecord
      else if FHeader.StartoffirstExtendedVariableLengthRecord > 0 then
        FMaxPOffset := FHeader.StartoffirstExtendedVariableLengthRecord
      else begin
        mpo := Int64(FHeader.Numberofpointrecords)*FHeader.PointDataRecordLength +
               FHeader.OffsetToPointData ;
        if mpo < FStream.Size then
          FMaxPOffset := mpo
        else
          FMaxPOffset := FStream.Size ;
      end ;

      if FHeader.NumberofVariableLengthRecords > 0 then begin
        for i := 0 to FHeader.NumberofVariableLengthRecords-1 do begin
          vlr := TLAS_VariableLengthRecord.Create ;
          try
            vlr.Read( FStream, False ) ;
            vlr.ParseGeoKeys ;
            FVLR.Add( vlr ) ;
          except
            FreeObject( vlr ) ;
          end ;
        end ;
      end ;

      if FHeader.NumberofExtendedVariableLengthRecords > 0 then begin
        FStream.Position := FHeader.StartoffirstExtendedVariableLengthRecord ;
        for i := 0 to FHeader.NumberofExtendedVariableLengthRecords-1 do begin
          vlr := TLAS_VariableLengthRecord.Create ;
          try
            vlr.Read( FStream, True ) ;
            vlr.ParseGeoKeys ;
            FVLR.Add( vlr ) ;
          except
            FreeObject( vlr ) ;
          end ;
        end ;
      end ;
    end ;

    Result := True ;
  end ;

  procedure TLAS_Reader.MoveFirst(
    const _cursor : Integer ;
    const _extent : TGIS_Extent
  ) ;
  begin
    if FIsLaz then begin
      if not FIsLazInit then exit ;
      {$IFNDEF ANDROID}
      check( T_LASZipLib(FLASLib).SeekPoint( 0 ) ) ;
      check( T_LASZipLib(FLASLib).SetRectangle( _extent ) ) ;
      cursorState[_cursor].curPointIndex := T_LASZipLib(FLASLib).GetPointIndex ;
      {$ENDIF}
    end
    else begin
      FStream.Position := FHeader.OffsetToPointData ;
      cursorState[_cursor].curPosition := FStream.Position ;
      cursorState[_cursor].curPointIndex := 0 ;
    end ;
    preparePoint( _cursor ) ;
  end ;

  function TLAS_Reader.MoveNext(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := not GetEof( _cursor ) ;

    if FIsLaz and Result then begin
      if not FIsLazInit then exit ;
      {$IFNDEF ANDROID}
      check( T_LASZipLib(FLASLib).ReadPointInside ) ;
      cursorState[_cursor].curPoint.Read( FStream ) ;
      cursorState[_cursor].curPointIndex := T_LASZipLib(FLASLib).GetPointIndex ;
      {$ENDIF}
    end
    else begin
      if Result then begin
        FStream.Position := cursorState[_cursor].curPosition ;
        cursorState[_cursor].curPoint.Read( FStream ) ;
        cursorState[_cursor].curPosition := FStream.Position ;
        inc( cursorState[_cursor].curPointIndex ) ;
      end ;
    end ;
  end ;

  function TLAS_Reader.GetEof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    if FIsLaz then begin
      Result := True ;
      if not FIsLazInit then exit ;
      {$IFNDEF ANDROID}
      Result := T_LASZipLib(FLASLib).ReadEof ;
      {$ENDIF}
    end
    else
      Result := (cursorState[_cursor].curPosition +
                 FHeader.PointDataRecordLength) > FMaxPOffset ;
  end ;

  function TLAS_Reader.GetCS: TGIS_CSCoordinateSystem;
  var
    i, j, k, l   : Integer ;
    geokeys      : TLAS_VLRGeoKeys ;
    asciiparams  : String          ;
    doubleparams : TGIS_DoubleArray ;
    wkt          : String ;
  begin
    Result := nil ;
    i := 0 ;
    geokeys := nil ;
    asciiparams := '' ;
    doubleparams := nil ;
    while i < FVLR.Count do begin
      if assigned( TLAS_VariableLengthRecord(FVLR[i]).GeoKeys ) then
        geokeys := TLAS_VariableLengthRecord(FVLR[i]).GeoKeys ;

      if assigned( TLAS_VariableLengthRecord(FVLR[i]).GeoDoubleParams ) then begin
        k := length( TLAS_VariableLengthRecord(FVLR[i]).GeoDoubleParams ) div 8 ;
        SetLength( doubleparams, k ) ;
        j := 0 ;
        l := 0 ;
        while j < k do begin
          {$IFDEF OXYGENE}
            doubleparams[j] := BitConverter.ToDouble(
                                  TLAS_VariableLengthRecord(FVLR[i]).GeoDoubleParams,
                                  l
                                 ) ;
          {$ELSE}
            Move( TLAS_VariableLengthRecord(FVLR[i]).GeoDoubleParams[l],
                  doubleparams[j],
                  8
                 ) ;
          {$ENDIF}
          inc( l, 8 ) ;
          inc( j ) ;
        end ;
      end ;

      if assigned( TLAS_VariableLengthRecord(FVLR[i]).GeoAsciiParams ) then
        asciiparams := ConvertAnsiString( TLAS_VariableLengthRecord(FVLR[i]).GeoAsciiParams ) ;

      if assigned( TLAS_VariableLengthRecord(FVLR[i]).OGSCSWKT ) then
        wkt := decodeString( TLAS_VariableLengthRecord(FVLR[i]).OGSCSWKT ) ;

      inc( i ) ;
    end ;

    if not IsStringEmpty( wkt )  then
      Result := TGIS_CSFactory.ByWKT( wkt ) ;

    if not assigned( Result ) then
      Result := prepareCS( geokeys, asciiparams, doubleparams ) ;
  end ;

  function TLAS_Reader.GetPoint(
    const _cursor : Integer
  ) : TGIS_Point3D ;
  begin
    Result := cursorState[_cursor].curPoint.GetPoint(
                FHeader.Xscalefactor,
                FHeader.Yscalefactor,
                FHeader.Zscalefactor,
                FHeader.Xoffset,
                FHeader.Yoffset,
                FHeader.Zoffset
              ) ;
  end ;

  function TLAS_Reader.LocatePoint(
    const _cursor : Integer ;
    const _uid    : TGIS_Uid
   ): Boolean ;
   var
     uid : TGIS_Uid ;
  begin
    Result := False ;
    uid := _uid ;
    if (uid > 0) and ( uid <= FHeader.Numberofpointrecords ) then begin
      preparePoint( _cursor ) ;
      if FIsLaz then begin
        if not FIsLazInit then exit ;
        {$IFNDEF ANDROID}
        check( T_LASZipLib(FLASLib).SeekPoint( _uid-1 ) ) ;
        check( T_LASZipLib(FLASLib).ReadPoint ) ;
        cursorState[_cursor].curPoint.Read( FStream ) ;
        cursorState[_cursor].curPointIndex := _uid ;
        {$ENDIF}
      end
      else begin
        FStream.Position := FHeader.OffsetToPointData +
                            Cardinal((_uid-1)*FHeader.PointDataRecordLength) ;
        cursorState[_cursor].curPoint.Read( FStream ) ;
        cursorState[_cursor].curPointIndex := _uid ;
      end ;
      Result := True ;
    end
    else
      Result := False ;
  end ;

  function TLAS_Reader.GetPointClassification(
    const _cursor : Integer
  ) : Cardinal ;
  begin
    Result := cursorState[_cursor].curPoint.GetClassification ;
  end ;

  function TLAS_Reader.GetPointColor(
    const _cursor : Integer
  ) : Cardinal ;
  begin
    Result := cursorState[_cursor].curPoint.GetColor ;
  end ;

  function TLAS_Reader.GetPointInfo(
    const _cursor : Integer
  ) : String ;
  begin
    Result := cursorState[_cursor].curPoint.GetInfo ;
  end ;

  function TLAS_Reader.GetPointIntensity(
    const _cursor : Integer
  ): Word;
  begin
    Result := cursorState[_cursor].curPoint.GetIntensity ;
  end ;

  function TLAS_Reader.GetPointReturnNumber(
    const _cursor : Integer
  ) : Byte ;
  begin
    Result := cursorState[_cursor].curPoint.GetReturnNumber ;
  end ;

  function TLAS_Reader.GetPointNumberOfReturns(
    const _cursor : Integer
  ) : Byte ;
  begin
    Result := cursorState[_cursor].curPoint.GetNumberOfReturns ;
  end ;

  function TLAS_Reader.GetPointScanDirectionFlag(
    const _cursor : Integer
  ) : Byte ;
  begin
    Result := cursorState[_cursor].curPoint.GetScanDirectionFlag ;
  end ;

  function TLAS_Reader.GetPointEdgeOfFlightLine(
    const _cursor : Integer
  ) : Byte ;
  begin
    Result := cursorState[_cursor].curPoint.GetEdgeOfFlightLine ;
  end ;

  function TLAS_Reader.GetPointScanAngleRank(
    const _cursor : Integer
  )  : SmallInt ;
  begin
    Result := cursorState[_cursor].curPoint.GetScanAngleRank ;
  end ;

  function TLAS_Reader.GetPointGPSTime(
    const _cursor : Integer
  ) : Double ;
  begin
    Result := cursorState[_cursor].curPoint.GetGPSTime ;
  end ;

  function TLAS_Reader.GetPointClassificationFlag(
    const _cursor : Integer
  ) : Byte ;
  begin
    Result := cursorState[_cursor].curPoint.GetClassificationFlag ;
  end ;

  function TLAS_Reader.GetPointScannerChannel(
    const _cursor : Integer
  ) : Byte ;
  begin
    Result := cursorState[_cursor].curPoint.GetScannerChannel ;
  end ;

  function TLAS_Reader.GetPointsCount : Integer ;
  begin
    Result := FHeader.Numberofpointrecords ;
  end ;

  function TLAS_Reader.GetPointIndex(
    const _cursor : Integer
  ) : TGIS_Uid ;
  begin
    Result := cursorState[_cursor].curPointIndex ;
  end ;

  function TLAS_Reader.HasPointColor : Boolean ;
  begin
    Result := FHeader.PointDataFormatID in [2,3,5,7,8,10] ;
  end ;

  function TLAS_Reader.GetExtent : TGIS_Extent3D ;
  begin
    Result := GisExtent3D( FHeader.MinX, FHeader.MinY, FHeader.MinZ,
                           FHeader.MaxX, FHeader.MaxY, FHeader.MaxZ
                          ) ;
  end ;

  function TLAS_Reader.GetInfo : String ;
  var
    i : Integer ;
  begin
    Result := FHeader.GetInfo + #13#10 + #13#10 ;
    if FVLR.Count > 0 then
      Result := Result + 'Variable Length Records :'+ #13#10 ;
    for i := 0 to FVLR.Count-1 do
      Result := Result + TLAS_VariableLengthRecord( FVLR[i] ).GetInfo ;
  end ;

//==============================================================================
// TGIS_FileLAS
//==============================================================================

  constructor TGIS_FileLAS.Create(
    const _path : String
  ) ;
  begin
    inherited Create ;

    FPath   := _path  ;
    FReader := nil ;
  end ;

  procedure TGIS_FileLAS.doDestroy ;
  begin
    FreeObject( FReader ) ;

    inherited ;
  end ;

  function TGIS_FileLAS.PreRecognize : Boolean ;
  begin
    Result := False ;
  end ;

  function TGIS_FileLAS.Open : Boolean ;
  begin
    if not assigned( FReader ) then
      FReader := TLAS_Reader.Create( FPath ) ;

    Result := TLAS_Reader(FReader).Open ;
  end ;

  procedure TGIS_FileLAS.MoveFirst(
    const _cursor : Integer ;
    const _extent : TGIS_Extent
  ) ;
  begin
    TLAS_Reader(FReader).MoveFirst( _cursor, _extent ) ;
  end ;

  function TGIS_FileLAS.MoveNext(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := TLAS_Reader(FReader).MoveNext( _cursor )
  end ;

  function TGIS_FileLAS.Eof(
    const _cursor : Integer
  ) : Boolean ;
  begin
    Result := TLAS_Reader(FReader).GetEof( _cursor ) ;
  end ;

  procedure TGIS_FileLAS.CursorOpen(
    const _cursor : Integer
  ) ;
  begin
    TLAS_Reader(FReader).CursorOpen(_cursor) ;
  end ;

  function TGIS_FileLAS.CS : TGIS_CSCoordinateSystem ;
  begin
    Result := TLAS_Reader(FReader).GetCS ;
  end ;

  procedure TGIS_FileLAS.CursorClose(
    const _cursor : Integer
  ) ;
  begin
    TLAS_Reader(FReader).CursorClose(_cursor);
  end ;

  function TGIS_FileLAS.GetPoint(
    const _cursor : Integer
  ) : TGIS_Point3D ;
  begin
    Result := TLAS_Reader(FReader).GetPoint( _cursor ) ;
  end ;

  function TGIS_FileLAS.LocatePoint(
    const _cursor : Integer ;
    const _uid    : TGIS_Uid
   ): Boolean ;
  begin
    Result := TLAS_Reader(FReader).LocatePoint( _cursor, _uid ) ;
  end ;

  function TGIS_FileLAS.GetPointClassification(
    const _cursor : Integer
  ) : Cardinal ;
  begin
    Result := TLAS_Reader(FReader).GetPointClassification( _cursor )
  end ;

  function TGIS_FileLAS.GetPointColor(
    const _cursor : Integer
  ) : Cardinal ;
  begin
    Result := TLAS_Reader(FReader).GetPointColor( _cursor )
  end ;

  function TGIS_FileLAS.GetPointInfo(
    const _cursor : Integer
  ) : String ;
  begin
    Result := TLAS_Reader(FReader).GetPointInfo( _cursor ) ;
  end ;

  function TGIS_FileLAS.GetPointIntensity(
    const _cursor : Integer
  ) : Word ;
  begin
    Result := TLAS_Reader(FReader).GetPointIntensity( _cursor )
  end ;

  function TGIS_FileLAS.GetPointReturnNumber(
    const _cursor : Integer
  ) : Byte ;
  begin
    Result := TLAS_Reader(FReader).GetPointReturnNumber( _cursor )
  end ;

  function TGIS_FileLAS.GetPointNumberOfReturns(
    const _cursor : Integer
  ) : Byte ;
  begin
    Result := TLAS_Reader(FReader).GetPointNumberOfReturns( _cursor )
  end ;

  function TGIS_FileLAS.GetPointScanDirectionFlag(
    const _cursor : Integer
  ) : Byte ;
  begin
    Result := TLAS_Reader(FReader).GetPointScanDirectionFlag( _cursor )
  end ;

  function TGIS_FileLAS.GetPointEdgeOfFlightLine(
    const _cursor : Integer
  ) : Byte ;
  begin
    Result := TLAS_Reader(FReader).GetPointEdgeOfFlightLine( _cursor )
  end ;

  function TGIS_FileLAS.GetPointScanAngleRank(
    const _cursor : Integer
  )  : SmallInt ;
  begin
    Result := TLAS_Reader(FReader).GetPointScanAngleRank( _cursor )
  end ;

  function TGIS_FileLAS.GetPointGPSTime(
    const _cursor : Integer
  ) : Double ;
  begin
    Result := TLAS_Reader(FReader).GetPointGPSTime( _cursor )
  end ;

  function TGIS_FileLAS.GetPointClassificationFlag(
    const _cursor : Integer
  ) : Byte ;
  begin
    Result := TLAS_Reader(FReader).GetPointClassificationFlag( _cursor )
  end ;

  function TGIS_FileLAS.GetPointScannerChannel(
    const _cursor : Integer
  ) : Byte ;
  begin
    Result := TLAS_Reader(FReader).GetPointScannerChannel( _cursor )
  end ;

  function TGIS_FileLAS.Extent : TGIS_Extent3D ;
  begin
    Result := TLAS_Reader(FReader).GetExtent ;
  end ;

  function TGIS_FileLAS.GetInfo : String ;
  begin
    Result := TLAS_Reader(FReader).GetInfo ;
  end ;

  function TGIS_FileLAS.GetPointsCount : Integer ;
  begin
    Result := TLAS_Reader(FReader).GetPointsCount ;
  end ;

  function TGIS_FileLAS.GetPointIndex(
    const _cursor : Integer
  ) : TGIS_Uid ;
  begin
    Result := TLAS_Reader(FReader).GetPointIndex( _cursor ) ;
  end ;

  function TGIS_FileLAS.HasPointColor : Boolean ;
  begin
    Result := TLAS_Reader(FReader).HasPointColor ;
  end ;


//==================================== END =====================================
end.
