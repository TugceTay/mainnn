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
  FMEObjects interface
}
{$IFNDEF GENDOC}

{$IFDEF DCC}
  unit Lider.CG.GIS.GeoFMEInt ;
  {$HPPEMIT '#pragma link "Lider.CG.GIS.GeoFMEInt"'}
{$ENDIF}
{$IFDEF CLR}
  namespace TatukGIS.NDK ;
  {$DELPHICOMPATIBILITY ON}
{$ENDIF}

{$INCLUDE Lider.CG.GIS.GeoInclude.inc}

// ************************************************************************ //
// WARNING
// -------
// The types declared in this file were generated from data read from a
// Type Library. If this type library is explicitly or indirectly (via
// another type library referring to this type library) re-imported, or the
// 'Refresh' command of the Type Library Editor activated while editing the
// Type Library, the contents of this file will be regenerated and all
// manual modifications will be lost.
// ************************************************************************ //

// PASTLWTR : 1.2
// File generated on 2006-11-10 07:54:56 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\Program Files\Common Files\Safe Software Shared\FME Objects\FMEObjects.dll (1)
// LIBID: {2C554B30-2363-11D3-8474-00104B222BDB}
// LCID: 0
// Helpfile:
// HelpString: FMEObjects 1.0 Type Library
// DepndLst:
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// ************************************************************************ //
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers.
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
interface

{$IFDEF MSWINDOWS}
  (*$HPPEMIT '#include <fme.h>' *)
{$ENDIF}

uses
  {$IFDEF MSWINDOWS}
    Winapi.Windows,
  {$ENDIF}
  System.Variants,
  System.Classes ;

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:
//   Type Libraries     : LIBID_xxxx
//   CoClasses          : CLASS_xxxx
//   DISPInterfaces     : DIID_xxxx
//   Non-DISP interfaces: IID_xxxx
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  FMEOBJECTSLibMajorVersion = 1;
  FMEOBJECTSLibMinorVersion = 0;

  LIBID_FMEOBJECTSLib: TGUID = '{2C554B30-2363-11D3-8474-00104B222BDB}';

  IID_IFMEOStringArray: TGUID = '{2C554B3D-2363-11D3-8474-00104B222BDB}';
  CLASS_FMEOStringArray: TGUID = '{DA1A4133-2360-11D3-8474-00104B222BDB}';
  IID_IFMEOFeature: TGUID = '{2C554B3E-2363-11D3-8474-00104B222BDB}';
  CLASS_FMEOFeature: TGUID = '{DA1A4137-2360-11D3-8474-00104B222BDB}';
  IID_IFMEOFeatureVector: TGUID = '{BB8665A1-54CE-11D3-8482-00104B222BDB}';
  IID_IFMEOPartIterator: TGUID = '{C38ECD73-91DA-4506-96C2-80E152B90AF1}';
  IID_IFMEOLogFile: TGUID = '{2C554B3F-2363-11D3-8474-00104B222BDB}';
  CLASS_FMEOLogFile: TGUID = '{DA1A4135-2360-11D3-8474-00104B222BDB}';
  IID_IFMEOPipeline: TGUID = '{2C554B40-2363-11D3-8474-00104B222BDB}';
  CLASS_FMEOPipeline: TGUID = '{DA1A4139-2360-11D3-8474-00104B222BDB}';
  IID_IFMEOFactory: TGUID = '{45CAD141-81E7-11D3-BA34-0010A40791EB}';
  IID_IFMEOReader: TGUID = '{2C554B41-2363-11D3-8474-00104B222BDB}';
  CLASS_FMEOReader: TGUID = '{DA1A413B-2360-11D3-8474-00104B222BDB}';
  IID_IFMEOWriter: TGUID = '{2C554B42-2363-11D3-8474-00104B222BDB}';
  CLASS_FMEOWriter: TGUID = '{DA1A413D-2360-11D3-8474-00104B222BDB}';
  IID_IFMEOSession: TGUID = '{2C554B43-2363-11D3-8474-00104B222BDB}';
  CLASS_FMEOSession: TGUID = '{DA1A413F-2360-11D3-8474-00104B222BDB}';
  IID_IFMEODialog: TGUID = '{A9BA8BE1-5EF5-11D3-8486-00104B222BDB}';
  IID_IFMEOCoordSysManager: TGUID = '{05A48C32-7C33-11D3-95BF-00105A1286A0}';
  IID_IFMEOSpatialIndex: TGUID = '{15BF7170-04F6-11D4-A79D-0090273EB14A}';
  IID_IFMEORectangleVector: TGUID = '{EF9C62A4-D0E3-4267-8A9C-E888EF262DD6}';
  IID_IFMEORectangle: TGUID = '{A31F911F-B2A8-4FB6-849A-E6B1A678FB68}';
  IID_IFMEOFeatureVectorOnDisk: TGUID = '{014AE541-CFF4-4763-8DCC-4DDF52CB62C9}';
  IID_IFMEOPersistentCache: TGUID = '{3DEB950E-C524-4F54-B0D7-E2BF96E920A6}';
  CLASS_FMEOFeatureVector: TGUID = '{B5DF2725-4F3C-11D3-8481-00104B222BDB}';
  CLASS_FMEODialog: TGUID = '{A9BA8BE2-5EF5-11D3-8486-00104B222BDB}';
  CLASS_FMEOCoordSysManager: TGUID = '{05A48C33-7C33-11D3-95BF-00105A1286A0}';
  CLASS_FMEOFactory: TGUID = '{45CAD142-81E7-11D3-BA34-0010A40791EB}';
  CLASS_FMEOSpatialIndex: TGUID = '{15BF7171-04F6-11D4-A79D-0090273EB14A}';
  CLASS_FMEORectangle: TGUID = '{D1F61A3E-DA89-4FA6-AEAF-E026289C6FFF}';
  CLASS_FMEORectangleVector: TGUID = '{50AE6563-7F18-45BA-89FB-90672FA303E8}';
  CLASS_FMEOFeatureVectorOnDisk: TGUID = '{AA500F4F-9862-4E43-B5A4-E020A9B2E67F}';
  CLASS_FMEOPersistentCache: TGUID = '{0924BCEE-0BAC-4C19-A169-9F8365A303FE}';
  CLASS_FMEOPartIterator: TGUID = '{69B9D3CA-3285-4369-90B2-697784DC8701}';
type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary
// *********************************************************************//
  IFMEOStringArray = interface;
  IFMEOFeature = interface;
  IFMEOFeatureVector = interface;
  IFMEOPartIterator = interface;
  IFMEOLogFile = interface;
  IFMEOPipeline = interface;
  IFMEOFactory = interface;
  IFMEOReader = interface;
  IFMEOWriter = interface;
  IFMEOSession = interface;
  IFMEODialog = interface;
  IFMEOCoordSysManager = interface;
  IFMEOSpatialIndex = interface;
  IFMEORectangleVector = interface;
  IFMEORectangle = interface;
  IFMEOFeatureVectorOnDisk = interface;
  IFMEOPersistentCache = interface;

  IFMEOPartIteratorDisp = dispinterface;
  IFMEOFeatureVectorDisp = dispinterface;
  IFMEOFeatureDisp = dispinterface;
  IFMEOStringArrayDisp = dispinterface;
  IFMEODialogDisp = dispinterface;
  IFMEOSessionDisp = dispinterface;
  IFMEOWriterDisp = dispinterface;
  IFMEOFactoryDisp = dispinterface;
  IFMEOPipelineDisp = dispinterface;
  IFMEOLogFileDisp = dispinterface;
  IFMEOReaderDisp = dispinterface;
  IFMEOPersistentCacheDisp = dispinterface;
  IFMEOCoordSysManagerDisp = dispinterface;
  IFMEOSpatialIndexDisp = dispinterface;
  IFMEORectangleVectorDisp = dispinterface;
  IFMEORectangleDisp = dispinterface;
  IFMEOFeatureVectorOnDiskDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library
// (NOTE: Here we map each CoClass to its Default Interface)
// *********************************************************************//
  FMEOStringArray = IFMEOStringArray;
  FMEOFeature = IFMEOFeature;
  FMEOLogFile = IFMEOLogFile;
  FMEOPipeline = IFMEOPipeline;
  FMEOReader = IFMEOReader;
  FMEOWriter = IFMEOWriter;
  FMEOSession = IFMEOSession;
  FMEOFeatureVector = IFMEOFeatureVector;
  FMEODialog = IFMEODialog;
  FMEOCoordSysManager = IFMEOCoordSysManager;
  FMEOFactory = IFMEOFactory;
  FMEOSpatialIndex = IFMEOSpatialIndex;
  FMEORectangle = IFMEORectangle;
  FMEORectangleVector = IFMEORectangleVector;
  FMEOFeatureVectorOnDisk = IFMEOFeatureVectorOnDisk;
  FMEOPersistentCache = IFMEOPersistentCache;
  FMEOPartIterator = IFMEOPartIterator;

// *********************************************************************//
// Declaration of structures, unions and aliases.
// *********************************************************************//
  POleVariant1 = ^OleVariant; {*}

// *********************************************************************//
// Interface: IFMEOStringArray
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2C554B3D-2363-11D3-8474-00104B222BDB}
// *********************************************************************//
  IFMEOStringArray = interface (IDispatch)
    ['{2C554B3D-2363-11D3-8474-00104B222BDB}']
    function get_element(position: Integer): WideString;  safecall ;
    procedure append(const string_: WideString);  safecall ;
    function get_entries: Integer;  safecall ;
    procedure clear;  safecall ;
    procedure contains(const string_: WideString; out contained: Integer);  safecall ;
    procedure appendCSVString(const csvString: WideString; const delimChars: WideString);  safecall ;
    procedure appendTokenizeString(const stringWithTokens: WideString;
                                   const tokenSepChars: WideString);  safecall ;
    procedure removeAll(const string_: WideString);  safecall ;
    property element[position: Integer]: WideString read get_element;
    property entries: Integer read get_entries;
  end;

// *********************************************************************//
// DispIntf:  IFMEOStringArrayDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2C554B3D-2363-11D3-8474-00104B222BDB}
// *********************************************************************//
  IFMEOStringArrayDisp = dispinterface
    ['{2C554B3D-2363-11D3-8474-00104B222BDB}']
    property element[position: Integer]: WideString readonly dispid 1;
    procedure append(const string_: WideString); dispid 2;
    property entries: Integer readonly dispid 3;
    procedure clear; dispid 4;
    procedure contains(const string_: WideString; out contained: Integer); dispid 5;
    procedure appendCSVString(const csvString: WideString; const delimChars: WideString); dispid 6;
    procedure appendTokenizeString(const stringWithTokens: WideString;
                                   const tokenSepChars: WideString); dispid 7;
    procedure removeAll(const string_: WideString); dispid 8;
  end;

// *********************************************************************//
// Interface: IFMEOFeature
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2C554B3E-2363-11D3-8474-00104B222BDB}
// *********************************************************************//
  IFMEOFeature = interface (IDispatch)
    ['{2C554B3E-2363-11D3-8474-00104B222BDB}']
    function get_attribute(const attrName: WideString): WideString;  safecall ;
    procedure set_attribute(const attrName: WideString; const pVal: WideString);  safecall ;
    function get_int32Attribute(const attrName: WideString): Integer;  safecall ;
    procedure set_int32Attribute(const attrName: WideString; pVal: Integer);  safecall ;
    function get_real64Attribute(const attrName: WideString): Double;  safecall ;
    procedure set_real64Attribute(const attrName: WideString; pVal: Double);  safecall ;
    procedure reset;  safecall ;
    procedure removeAttribute(const attrName: WideString);  safecall ;
    procedure allAttributeNames(const pVal: IFMEOStringArray);  safecall ;
    procedure set_sequencedAttribute(const attrName: WideString; const Param2: WideString);  safecall ;
    function get_featureType: WideString;  safecall ;
    procedure set_featureType(const pVal: WideString);  safecall ;
    function get_geometryType: Integer;  safecall ;
    procedure set_geometryType(pVal: Integer);  safecall ;
    function get_dimension: Smallint;  safecall ;
    procedure set_dimension(pVal: Smallint);  safecall ;
    function get_numCoords: Integer;  safecall ;
    procedure resetCoords;  safecall ;
    function get_xCoordinate(index: Integer): Double;  safecall ;
    function get_yCoordinate(index: Integer): Double;  safecall ;
    function get_zCoordinate(index: Integer): Double;  safecall ;
    procedure addCoordinate(xCoord: Double; yCoord: Double; zCoord: Double);  safecall ;
    function get_coordSys: WideString;  safecall ;
    procedure set_coordSys(const pVal: WideString);  safecall ;
    procedure performFunction(const function_: WideString; out result: WideString);  safecall ;
    function attributeExists(const attrName: WideString): Integer;  safecall ;
    procedure addReal32Coordinate(xCoord: Single; yCoord: Single; zCoord: Single);  safecall ;
    function get_xCoordReal32(index: Integer): Single;  safecall ;
    function get_yCoordReal32(index: Integer): Single;  safecall ;
    function get_zCoordReal32(index: Integer): Single;  safecall ;
    procedure clone(const targetFeature: IFMEOFeature);  safecall ;
    procedure cloneAttributes(const targetFeature: IFMEOFeature);  safecall ;
    procedure mergeAttributes(const targetFeature: IFMEOFeature);  safecall ;
    procedure rotate2D(originX: Double; originY: Double; degreeAngle: Double);  safecall ;
    procedure scale(scaleX: Double; scaleY: Double; scaleZ: Double);  safecall ;
    procedure offset(offsetX: Double; offsetY: Double; offsetZ: Double);  safecall ;
    procedure convertArcToPoints(centerX: Double; centerY: Double; semiPrimaryAxis: Double;
                                 semiSecondaryAxis: Double; origNumSamps: Integer;
                                 startAngle: Double; endAngle: Double; rotation: Double);  safecall ;
    procedure convertPointsToArc(const radiusAttrName: WideString;
                                 const startAngleAttrName: WideString;
                                 const sweepAngleAttrName: WideString);  safecall ;
    procedure interpolateSpline(numPointsPerSegment: Integer; calcPhantomPoints: Integer);  safecall ;
    procedure boundingBox(out minX: Double; out maxX: Double; out minY: Double; out maxY: Double);  safecall ;
    procedure boundingCube(out minX: Double; out maxX: Double; out minY: Double; out maxY: Double;
                           out minZ: Double; out maxZ: Double);  safecall ;
    procedure generatePointInPolygon(pretty: Integer; out internalPointX: Double;
                                     out internalPointY: Double; out internalPointZ: Double);  safecall ;
    function chopUp(vertexThreshold: Integer): Integer;  safecall ;
    procedure getListAttribute(const attrName: WideString; const attrValues: IFMEOStringArray);  safecall ;
    procedure setListAttribute(const attrName: WideString; const attrValues: IFMEOStringArray);  safecall ;
    procedure buildAggregateFeature(const aggregateParts: IFMEOFeatureVector);  safecall ;
    procedure splitAggregate(const outputAggregateParts: IFMEOFeatureVector; recurse: Integer);  safecall ;
    procedure getDonutParts(const outputDonutParts: IFMEOFeatureVector);  safecall ;
    procedure makeDonuts(const inputDonutParts: IFMEOFeatureVector; keepHoles: Integer);  safecall ;
    procedure getSequencedAttributeList(const attrNames: IFMEOStringArray);  safecall ;
    function getOrientation: Integer;  safecall ;
    procedure setOrientation(rightHandRule: Integer);  safecall ;
    function get_int16Attribute(const attrName: WideString): Smallint;  safecall ;
    procedure set_int16Attribute(const attrName: WideString; pVal: Smallint);  safecall ;
    function get_real32Attribute(const attrName: WideString): Single;  safecall ;
    procedure set_real32Attribute(const attrName: WideString; pVal: Single);  safecall ;
    procedure reproject(const coordSys: WideString);  safecall ;
    procedure buffer(width: Double; sampleAngle: Double);  safecall ;
    procedure outerShell;  safecall ;
    function exportGeometryToOGCWKT: WideString;  safecall ;
    procedure importGeometryFromOGCWKT(const OGCWKT: WideString);  safecall ;
    function get_booleanAttribute(const attrName: WideString): Integer;  safecall ;
    procedure set_booleanAttribute(const attrName: WideString; pVal: Integer);  safecall ;
    procedure processFeatures(var featArray: IFMEOFeatureVector; const parms: IFMEOStringArray);  safecall ;
    function get_numVertices: Integer;  safecall ;
    function get_numParts(flatten: Integer; splitDonuts: Integer): Integer;  safecall ;
    function getPartIterator(flatten: Integer; splitDonuts: Integer; out partIter: IFMEOPartIterator): Integer;  safecall ;
    procedure matrixTransform(m11: Double; m12: Double; m21: Double; m22: Double);  safecall ;
    procedure matrixTransform3D(m11: Double; m12: Double; m13: Double; m21: Double; m22: Double;
                                m23: Double; m31: Double; m32: Double; m33: Double);  safecall ;
    procedure removeListAttribute(const attrName: WideString);  safecall ;
    procedure addCoordinates(xCoords: OleVariant; yCoords: OleVariant; zCoords: OleVariant);  safecall ;
    procedure addReal32Coordinates(xCoordinates: OleVariant; yCoordinates: OleVariant;
                                   zCoordinates: OleVariant);  safecall ;
    procedure getAllCoordinates(var xCoordinates: OleVariant; var yCoordinates: OleVariant;
                                var zCoordinates: OleVariant);  safecall ;
    procedure getAllCoordinatesReal32(var xCoordinates: OleVariant; var yCoordinates: OleVariant;
                                      var zCoordinates: OleVariant);  safecall ;
    property attribute[const attrName: WideString]: WideString read get_attribute write set_attribute;
    property int32Attribute[const attrName: WideString]: Integer read get_int32Attribute write Set_int32Attribute;
    property real64Attribute[const attrName: WideString]: Double read get_real64Attribute write Set_real64Attribute;
    property sequencedAttribute[const attrName: WideString]: WideString write set_sequencedAttribute;
    property featureType: WideString read get_featureType write set_featureType;
    property geometryType: Integer read get_geometryType write set_geometryType;
    property dimension: Smallint read get_dimension write set_dimension;
    property numCoords: Integer read get_numCoords;
    property xCoordinate[index: Integer]: Double read get_xCoordinate;
    property yCoordinate[index: Integer]: Double read get_yCoordinate;
    property zCoordinate[index: Integer]: Double read get_zCoordinate;
    property coordSys: WideString read get_coordSys write set_coordSys;
    property xCoordReal32[index: Integer]: Single read get_xCoordReal32;
    property yCoordReal32[index: Integer]: Single read get_yCoordReal32;
    property zCoordReal32[index: Integer]: Single read get_zCoordReal32;
    property int16Attribute[const attrName: WideString]: Smallint read get_int16Attribute write set_int16Attribute;
    property real32Attribute[const attrName: WideString]: Single read get_real32Attribute write set_real32Attribute;
    property booleanAttribute[const attrName: WideString]: Integer read get_booleanAttribute write set_booleanAttribute;
    property numVertices: Integer read get_numVertices;
    property numParts[flatten: Integer; splitDonuts: Integer]: Integer read get_numParts;
  end;

// *********************************************************************//
// DispIntf:  IFMEOFeatureDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2C554B3E-2363-11D3-8474-00104B222BDB}
// *********************************************************************//
  IFMEOFeatureDisp = dispinterface
    ['{2C554B3E-2363-11D3-8474-00104B222BDB}']
    property attribute[const attrName: WideString]: WideString dispid 1;
    property int32Attribute[const attrName: WideString]: Integer dispid 2;
    property real64Attribute[const attrName: WideString]: Double dispid 3;
    procedure reset; dispid 4;
    procedure removeAttribute(const attrName: WideString); dispid 5;
    procedure allAttributeNames(const pVal: IFMEOStringArray); dispid 6;
    property sequencedAttribute[const attrName: WideString]: WideString writeonly dispid 7;
    property featureType: WideString dispid 8;
    property geometryType: Integer dispid 9;
    property dimension: Smallint dispid 10;
    property numCoords: Integer readonly dispid 11;
    procedure resetCoords; dispid 12;
    property xCoordinate[index: Integer]: Double readonly dispid 15;
    property yCoordinate[index: Integer]: Double readonly dispid 16;
    property zCoordinate[index: Integer]: Double readonly dispid 17;
    procedure addCoordinate(xCoord: Double; yCoord: Double; zCoord: Double); dispid 18;
    property coordSys: WideString dispid 19;
    procedure performFunction(const function_: WideString; out result: WideString); dispid 20;
    function attributeExists(const attrName: WideString): Integer; dispid 21;
    procedure addReal32Coordinate(xCoord: Single; yCoord: Single; zCoord: Single); dispid 22;
    property xCoordReal32[index: Integer]: Single readonly dispid 23;
    property yCoordReal32[index: Integer]: Single readonly dispid 24;
    property zCoordReal32[index: Integer]: Single readonly dispid 25;
    procedure clone(const targetFeature: IFMEOFeature); dispid 26;
    procedure cloneAttributes(const targetFeature: IFMEOFeature); dispid 27;
    procedure mergeAttributes(const targetFeature: IFMEOFeature); dispid 28;
    procedure rotate2D(originX: Double; originY: Double; degreeAngle: Double); dispid 29;
    procedure scale(scaleX: Double; scaleY: Double; scaleZ: Double); dispid 30;
    procedure offset(offsetX: Double; offsetY: Double; offsetZ: Double); dispid 31;
    procedure convertArcToPoints(centerX: Double; centerY: Double; semiPrimaryAxis: Double;
                                 semiSecondaryAxis: Double; origNumSamps: Integer;
                                 startAngle: Double; endAngle: Double; rotation: Double); dispid 32;
    procedure convertPointsToArc(const radiusAttrName: WideString;
                                 const startAngleAttrName: WideString;
                                 const sweepAngleAttrName: WideString); dispid 33;
    procedure interpolateSpline(numPointsPerSegment: Integer; calcPhantomPoints: Integer); dispid 34;
    procedure boundingBox(out minX: Double; out maxX: Double; out minY: Double; out maxY: Double); dispid 35;
    procedure boundingCube(out minX: Double; out maxX: Double; out minY: Double; out maxY: Double;
                           out minZ: Double; out maxZ: Double); dispid 36;
    procedure generatePointInPolygon(pretty: Integer; out internalPointX: Double;
                                     out internalPointY: Double; out internalPointZ: Double); dispid 37;
    function chopUp(vertexThreshold: Integer): Integer; dispid 38;
    procedure getListAttribute(const attrName: WideString; const attrValues: IFMEOStringArray); dispid 39;
    procedure setListAttribute(const attrName: WideString; const attrValues: IFMEOStringArray); dispid 40;
    procedure buildAggregateFeature(const aggregateParts: IFMEOFeatureVector); dispid 41;
    procedure splitAggregate(const outputAggregateParts: IFMEOFeatureVector; recurse: Integer); dispid 42;
    procedure getDonutParts(const outputDonutParts: IFMEOFeatureVector); dispid 43;
    procedure makeDonuts(const inputDonutParts: IFMEOFeatureVector; keepHoles: Integer); dispid 44;
    procedure getSequencedAttributeList(const attrNames: IFMEOStringArray); dispid 45;
    function getOrientation: Integer; dispid 46;
    procedure setOrientation(rightHandRule: Integer); dispid 47;
    property int16Attribute[const attrName: WideString]: Smallint dispid 48;
    property real32Attribute[const attrName: WideString]: Single dispid 49;
    procedure reproject(const coordSys: WideString); dispid 50;
    procedure buffer(width: Double; sampleAngle: Double); dispid 51;
    procedure outerShell; dispid 52;
    function exportGeometryToOGCWKT: WideString; dispid 53;
    procedure importGeometryFromOGCWKT(const OGCWKT: WideString); dispid 54;
    property booleanAttribute[const attrName: WideString]: Integer dispid 55;
    procedure processFeatures(var featArray: IFMEOFeatureVector; const parms: IFMEOStringArray); dispid 56;
    property numVertices: Integer readonly dispid 57;
    property numParts[flatten: Integer; splitDonuts: Integer]: Integer readonly dispid 58;
    function getPartIterator(flatten: Integer; splitDonuts: Integer; out partIter: IFMEOPartIterator): Integer; dispid 59;
    procedure matrixTransform(m11: Double; m12: Double; m21: Double; m22: Double); dispid 60;
    procedure matrixTransform3D(m11: Double; m12: Double; m13: Double; m21: Double; m22: Double;
                                m23: Double; m31: Double; m32: Double; m33: Double); dispid 61;
    procedure removeListAttribute(const attrName: WideString); dispid 62;
    procedure addCoordinates(xCoords: OleVariant; yCoords: OleVariant; zCoords: OleVariant); dispid 63;
    procedure addReal32Coordinates(xCoordinates: OleVariant; yCoordinates: OleVariant;
                                   zCoordinates: OleVariant); dispid 64;
    procedure getAllCoordinates(var xCoordinates: OleVariant; var yCoordinates: OleVariant;
                                var zCoordinates: OleVariant); dispid 65;
    procedure getAllCoordinatesReal32(var xCoordinates: OleVariant; var yCoordinates: OleVariant;
                                      var zCoordinates: OleVariant); dispid 66;
  end;

// *********************************************************************//
// Interface: IFMEOFeatureVector
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BB8665A1-54CE-11D3-8482-00104B222BDB}
// *********************************************************************//
  IFMEOFeatureVector = interface (IDispatch)
    ['{BB8665A1-54CE-11D3-8482-00104B222BDB}']
    function get_entries: Integer;  safecall ;
    procedure append(const feature: IFMEOFeature);  safecall ;
    procedure insertAt(pos: Integer; const feature: IFMEOFeature);  safecall ;
    function element(pos: Integer): IFMEOFeature;  safecall ;
    function removeLast: IFMEOFeature;  safecall ;
    procedure clear;  safecall ;
    function contains(const feature: IFMEOFeature): Integer;  safecall ;
    procedure clearAndDestroy;  safecall ;
    property entries: Integer read get_entries;
  end;

// *********************************************************************//
// DispIntf:  IFMEOFeatureVectorDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {BB8665A1-54CE-11D3-8482-00104B222BDB}
// *********************************************************************//
  IFMEOFeatureVectorDisp = dispinterface
    ['{BB8665A1-54CE-11D3-8482-00104B222BDB}']
    property entries: Integer readonly dispid 1;
    procedure append(const feature: IFMEOFeature); dispid 2;
    procedure insertAt(pos: Integer; const feature: IFMEOFeature); dispid 3;
    function element(pos: Integer): IFMEOFeature; dispid 4;
    function removeLast: IFMEOFeature; dispid 5;
    procedure clear; dispid 6;
    function contains(const feature: IFMEOFeature): Integer; dispid 7;
    procedure clearAndDestroy; dispid 8;
  end;

// *********************************************************************//
// Interface: IFMEOPartIterator
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C38ECD73-91DA-4506-96C2-80E152B90AF1}
// *********************************************************************//
  IFMEOPartIterator = interface (IDispatch)
    ['{C38ECD73-91DA-4506-96C2-80E152B90AF1}']
    function next: Integer;  safecall ;
    function getPartIterator(flatten: Integer; splitDonuts: Integer; out partIter: IFMEOPartIterator): Integer;  safecall ;
    function get_xCoordinate(index: Integer): Double;  safecall ;
    procedure set_xCoordinate(index: Integer; pVal: Double);  safecall ;
    function get_yCoordinate(index: Integer): Double;  safecall ;
    procedure set_yCoordinate(index: Integer; pVal: Double);  safecall ;
    function get_zCoordinate(index: Integer): Double;  safecall ;
    procedure set_zCoordinate(index: Integer; pVal: Double);  safecall ;
    function get_xCoordReal32(index: Integer): Single;  safecall ;
    procedure set_xCoordReal32(index: Integer; pVal: Single);  safecall ;
    function get_yCoordReal32(index: Integer): Single;  safecall ;
    procedure set_yCoordReal32(index: Integer; pVal: Single);  safecall ;
    function get_zCoordReal32(index: Integer): Single;  safecall ;
    procedure set_zCoordReal32(index: Integer; pVal: Single);  safecall ;
    procedure reset;  safecall ;
    function get_numVertices: Integer;  safecall ;
    function get_dimension: Smallint;  safecall ;
    function get_geomType: Integer;  safecall ;
    function get_xCoords: OleVariant;  safecall ;
    function get_yCoords: OleVariant;  safecall ;
    function get_zCoords: OleVariant;  safecall ;
    function get_xCoordsReal32: OleVariant;  safecall ;
    function get_yCoordsReal32: OleVariant;  safecall ;
    function get_zCoordsReal32: OleVariant;  safecall ;
    property xCoordinate[index: Integer]: Double read get_xCoordinate write set_xCoordinate;
    property yCoordinate[index: Integer]: Double read get_yCoordinate write set_yCoordinate;
    property zCoordinate[index: Integer]: Double read get_zCoordinate write set_zCoordinate;
    property xCoordReal32[index: Integer]: Single read get_xCoordReal32 write set_xCoordReal32;
    property yCoordReal32[index: Integer]: Single read get_yCoordReal32 write set_yCoordReal32;
    property zCoordReal32[index: Integer]: Single read get_zCoordReal32 write set_zCoordReal32;
    property numVertices: Integer read get_numVertices;
    property dimension: Smallint read get_dimension;
    property geomType: Integer read get_geomType;
    property xCoords: OleVariant read get_xCoords;
    property yCoords: OleVariant read get_yCoords;
    property zCoords: OleVariant read get_zCoords;
    property xCoordsReal32: OleVariant read get_xCoordsReal32;
    property yCoordsReal32: OleVariant read get_yCoordsReal32;
    property zCoordsReal32: OleVariant read get_zCoordsReal32;
  end;

// *********************************************************************//
// DispIntf:  IFMEOPartIteratorDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C38ECD73-91DA-4506-96C2-80E152B90AF1}
// *********************************************************************//
  IFMEOPartIteratorDisp = dispinterface
    ['{C38ECD73-91DA-4506-96C2-80E152B90AF1}']
    function next: Integer; dispid 1;
    function getPartIterator(flatten: Integer; splitDonuts: Integer; out partIter: IFMEOPartIterator): Integer; dispid 2;
    property xCoordinate[index: Integer]: Double dispid 3;
    property yCoordinate[index: Integer]: Double dispid 4;
    property zCoordinate[index: Integer]: Double dispid 5;
    property xCoordReal32[index: Integer]: Single dispid 6;
    property yCoordReal32[index: Integer]: Single dispid 7;
    property zCoordReal32[index: Integer]: Single dispid 8;
    procedure reset; dispid 9;
    property numVertices: Integer readonly dispid 10;
    property dimension: Smallint readonly dispid 11;
    property geomType: Integer readonly dispid 12;
    property xCoords: OleVariant readonly dispid 13;
    property yCoords: OleVariant readonly dispid 14;
    property zCoords: OleVariant readonly dispid 15;
    property xCoordsReal32: OleVariant readonly dispid 16;
    property yCoordsReal32: OleVariant readonly dispid 17;
    property zCoordsReal32: OleVariant readonly dispid 18;
  end;

// *********************************************************************//
// Interface: IFMEOLogFile
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2C554B3F-2363-11D3-8474-00104B222BDB}
// *********************************************************************//
  IFMEOLogFile = interface (IDispatch)
    ['{2C554B3F-2363-11D3-8474-00104B222BDB}']
    procedure logMessageString(messageLevel: Integer; const message: WideString);  safecall ;
    procedure logFeature(const feature: IFMEOFeature; messageLevel: Integer; maxCoords: Integer);  safecall ;
    procedure setFileName(const logFileName: WideString; append: Integer);  safecall ;
    procedure holdMessages(hold: Integer);  safecall ;
    procedure getHeldMessages(const messages: IFMEOStringArray);  safecall ;
    procedure logMessageByNum(severity: Integer; messageNumber: Integer);  safecall ;
    procedure logMessageByNumParms(severity: Integer; messageNumber: Integer;
                                   const messageParameters: IFMEOStringArray);  safecall ;
    procedure activityMessage(const msg: WideString);  safecall ;
    procedure silent(isSilent: Integer);  safecall ;
    function get_filename: WideString;  safecall ;
    property filename: WideString read get_filename;
  end;

// *********************************************************************//
// DispIntf:  IFMEOLogFileDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2C554B3F-2363-11D3-8474-00104B222BDB}
// *********************************************************************//
  IFMEOLogFileDisp = dispinterface
    ['{2C554B3F-2363-11D3-8474-00104B222BDB}']
    procedure logMessageString(messageLevel: Integer; const message: WideString); dispid 1;
    procedure logFeature(const feature: IFMEOFeature; messageLevel: Integer; maxCoords: Integer); dispid 2;
    procedure setFileName(const logFileName: WideString; append: Integer); dispid 3;
    procedure holdMessages(hold: Integer); dispid 4;
    procedure getHeldMessages(const messages: IFMEOStringArray); dispid 5;
    procedure logMessageByNum(severity: Integer; messageNumber: Integer); dispid 6;
    procedure logMessageByNumParms(severity: Integer; messageNumber: Integer;
                                   const messageParameters: IFMEOStringArray); dispid 7;
    procedure activityMessage(const msg: WideString); dispid 8;
    procedure silent(isSilent: Integer); dispid 9;
    property filename: WideString readonly dispid 10;
  end;

// *********************************************************************//
// Interface: IFMEOPipeline
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2C554B40-2363-11D3-8474-00104B222BDB}
// *********************************************************************//
  IFMEOPipeline = interface (IDispatch)
    ['{2C554B40-2363-11D3-8474-00104B222BDB}']
    procedure addFactoryFromStrArray(const factDef: IFMEOStringArray);  safecall ;
    procedure addFactory(const factoryDef: WideString; const delimChars: WideString);  safecall ;
    procedure addFactories(const filename: WideString);  safecall ;
    procedure processFeature(const feature: IFMEOFeature);  safecall ;
    function getOutputFeature(const feature: IFMEOFeature): Integer;  safecall ;
    procedure allDone;  safecall ;
    procedure configureFunction(const functionConfig: IFMEOStringArray);  safecall ;
    procedure addFactoryObject(const factory: IFMEOFactory);  safecall ;
  end;

// *********************************************************************//
// DispIntf:  IFMEOPipelineDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2C554B40-2363-11D3-8474-00104B222BDB}
// *********************************************************************//
  IFMEOPipelineDisp = dispinterface
    ['{2C554B40-2363-11D3-8474-00104B222BDB}']
    procedure addFactoryFromStrArray(const factDef: IFMEOStringArray); dispid 1;
    procedure addFactory(const factoryDef: WideString; const delimChars: WideString); dispid 2;
    procedure addFactories(const filename: WideString); dispid 3;
    procedure processFeature(const feature: IFMEOFeature); dispid 4;
    function getOutputFeature(const feature: IFMEOFeature): Integer; dispid 5;
    procedure allDone; dispid 6;
    procedure configureFunction(const functionConfig: IFMEOStringArray); dispid 7;
    procedure addFactoryObject(const factory: IFMEOFactory); dispid 8;
  end;

// *********************************************************************//
// Interface: IFMEOFactory
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {45CAD141-81E7-11D3-BA34-0010A40791EB}
// *********************************************************************//
  IFMEOFactory = interface (IDispatch)
    ['{45CAD141-81E7-11D3-BA34-0010A40791EB}']
  end;

// *********************************************************************//
// DispIntf:  IFMEOFactoryDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {45CAD141-81E7-11D3-BA34-0010A40791EB}
// *********************************************************************//
  IFMEOFactoryDisp = dispinterface
    ['{45CAD141-81E7-11D3-BA34-0010A40791EB}']
  end;
// *********************************************************************//
// Interface: IFMEOReader
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2C554B41-2363-11D3-8474-00104B222BDB}
// *********************************************************************//
  IFMEOReader = interface (IDispatch)
    ['{2C554B41-2363-11D3-8474-00104B222BDB}']
    procedure open(const datasetName: WideString; const parameters: IFMEOStringArray);  safecall ;
    procedure abort;  safecall ;
    procedure close;  safecall ;
    function read(const feature: IFMEOFeature): Integer;  safecall ;
    function readSchema(const feature: IFMEOFeature): Integer;  safecall ;
    procedure setConstraints(const feat: IFMEOFeature);  safecall ;
    function getProperties(const property_: WideString; const values: IFMEOStringArray): Integer;  safecall ;
  end;

// *********************************************************************//
// DispIntf:  IFMEOReaderDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2C554B41-2363-11D3-8474-00104B222BDB}
// *********************************************************************//
  IFMEOReaderDisp = dispinterface
    ['{2C554B41-2363-11D3-8474-00104B222BDB}']
    procedure open(const datasetName: WideString; const parameters: IFMEOStringArray); dispid 1;
    procedure abort; dispid 2;
    procedure close; dispid 3;
    function read(const feature: IFMEOFeature): Integer; dispid 4;
    function readSchema(const feature: IFMEOFeature): Integer; dispid 5;
    procedure setConstraints(const feat: IFMEOFeature); dispid 6;
    function getProperties(const property_: WideString; const values: IFMEOStringArray): Integer; dispid 7;
  end;

// *********************************************************************//
// Interface: IFMEOWriter
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2C554B42-2363-11D3-8474-00104B222BDB}
// *********************************************************************//
  IFMEOWriter = interface (IDispatch)
    ['{2C554B42-2363-11D3-8474-00104B222BDB}']
    procedure open(const datasetName: WideString; const parameters: IFMEOStringArray);  safecall ;
    procedure abort;  safecall ;
    procedure close;  safecall ;
    procedure write(const feature: IFMEOFeature);  safecall ;
    procedure addSchema(const schemaFeat: IFMEOFeature);  safecall ;
    procedure startTransaction;  safecall ;
    procedure commitTransaction;  safecall ;
    procedure rollbackTransaction;  safecall ;
    procedure getSchemaFeatures(const schemaFeatures: IFMEOFeatureVector);  safecall ;
  end;

// *********************************************************************//
// DispIntf:  IFMEOWriterDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2C554B42-2363-11D3-8474-00104B222BDB}
// *********************************************************************//
  IFMEOWriterDisp = dispinterface
    ['{2C554B42-2363-11D3-8474-00104B222BDB}']
    procedure open(const datasetName: WideString; const parameters: IFMEOStringArray); dispid 1;
    procedure abort; dispid 2;
    procedure close; dispid 3;
    procedure write(const feature: IFMEOFeature); dispid 4;
    procedure addSchema(const schemaFeat: IFMEOFeature); dispid 5;
    procedure startTransaction; dispid 6;
    procedure commitTransaction; dispid 7;
    procedure rollbackTransaction; dispid 8;
    procedure getSchemaFeatures(const schemaFeatures: IFMEOFeatureVector); dispid 9;
  end;
// *********************************************************************//
// Interface: IFMEOSession
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2C554B43-2363-11D3-8474-00104B222BDB}
// *********************************************************************//
  IFMEOSession = interface (IDispatch)
    ['{2C554B43-2363-11D3-8474-00104B222BDB}']
    procedure init(const directives: IFMEOStringArray);  safecall ;
    function get_logFile: IFMEOLogFile;  safecall ;
    function createReader(const readerName: WideString; cacheFeatures: Integer;
                          const directives: IFMEOStringArray): IFMEOReader;  safecall ;
    function createFeature: IFMEOFeature;  safecall ;
    function createWriter(const writerName: WideString; const directives: IFMEOStringArray): IFMEOWriter;  safecall ;
    function createFactoryPipeline(const pipelineName: WideString;
                                   const directives: IFMEOStringArray): IFMEOPipeline;  safecall ;
    function createStringArray: IFMEOStringArray;  safecall ;
    procedure updateSettings(const keyword: WideString; const value: WideString);  safecall ;
    function get_lastErrorMsg: WideString;  safecall ;
    function get_lastErrorNum: Integer;  safecall ;
    function get_lastErrorStackTrace: WideString;  safecall ;
    function createFeatureVector: IFMEOFeatureVector;  safecall ;
    function createDialog: IFMEODialog;  safecall ;
    function get_fmeHome: WideString;  safecall ;
    function get_coordSysManager: IFMEOCoordSysManager;  safecall ;
    function get_fmeVersion: WideString;  safecall ;
    function get_fmeBuildNumber: Integer;  safecall ;
    function createFactory: IFMEOFactory;  safecall ;
    procedure configure(const configRow: IFMEOStringArray);  safecall ;
    function createSpatialIndex(const filename: WideString; const accessMode: WideString;
                                const directives: IFMEOStringArray): IFMEOSpatialIndex;  safecall ;
    function createRectangle(minX: Double; maxX: Double; minY: Double; maxY: Double): IFMEORectangle;  safecall ;
    function createRectangleVector: IFMEORectangleVector;  safecall ;
    procedure getSettings(const keyword: WideString; const values: IFMEOStringArray);  safecall ;
    procedure logSettings(const keyword: WideString);  safecall ;
    function createFeatureVectorOnDisk(maxEntriesInMem: Integer): IFMEOFeatureVectorOnDisk;  safecall ;
    procedure getSchemaFeatures(const macros: IFMEOStringArray; const prefix: WideString;
                                const schemaFile: WideString;
                                const schemaFeatures: IFMEOFeatureVector);  safecall ;
    function get_persistentCache(const directives: IFMEOStringArray): IFMEOPersistentCache;  safecall ;
    function getProperties(const propertyCategory: WideString; const props: IFMEOStringArray): Integer;  safecall ;
    function getAllProperties(const props: IFMEOStringArray): Integer;  safecall ;
    property logFile: IFMEOLogFile read get_logFile;
    property lastErrorMsg: WideString read get_lastErrorMsg;
    property lastErrorNum: Integer read get_lastErrorNum;
    property lastErrorStackTrace: WideString read get_lastErrorStackTrace;
    property fmeHome: WideString read get_fmeHome;
    property coordSysManager: IFMEOCoordSysManager read get_coordSysManager;
    property fmeVersion: WideString read get_fmeVersion;
    property fmeBuildNumber: Integer read get_fmeBuildNumber;
    property persistentCache[const directives: IFMEOStringArray]: IFMEOPersistentCache read get_persistentCache;
  end;

// *********************************************************************//
// DispIntf:  IFMEOSessionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {2C554B43-2363-11D3-8474-00104B222BDB}
// *********************************************************************//
  IFMEOSessionDisp = dispinterface
    ['{2C554B43-2363-11D3-8474-00104B222BDB}']
    procedure init(const directives: IFMEOStringArray); dispid 1;
    property logFile: IFMEOLogFile readonly dispid 2;
    function createReader(const readerName: WideString; cacheFeatures: Integer;
                          const directives: IFMEOStringArray): IFMEOReader; dispid 3;
    function createFeature: IFMEOFeature; dispid 4;
    function createWriter(const writerName: WideString; const directives: IFMEOStringArray): IFMEOWriter; dispid 5;
    function createFactoryPipeline(const pipelineName: WideString;
                                   const directives: IFMEOStringArray): IFMEOPipeline; dispid 6;
    function createStringArray: IFMEOStringArray; dispid 7;
    procedure updateSettings(const keyword: WideString; const value: WideString); dispid 8;
    property lastErrorMsg: WideString readonly dispid 9;
    property lastErrorNum: Integer readonly dispid 10;
    property lastErrorStackTrace: WideString readonly dispid 11;
    function createFeatureVector: IFMEOFeatureVector; dispid 12;
    function createDialog: IFMEODialog; dispid 13;
    property fmeHome: WideString readonly dispid 14;
    property coordSysManager: IFMEOCoordSysManager readonly dispid 15;
    property fmeVersion: WideString readonly dispid 16;
    property fmeBuildNumber: Integer readonly dispid 17;
    function createFactory: IFMEOFactory; dispid 18;
    procedure configure(const configRow: IFMEOStringArray); dispid 19;
    function createSpatialIndex(const filename: WideString; const accessMode: WideString;
                                const directives: IFMEOStringArray): IFMEOSpatialIndex; dispid 20;
    function createRectangle(minX: Double; maxX: Double; minY: Double; maxY: Double): IFMEORectangle; dispid 21;
    function createRectangleVector: IFMEORectangleVector; dispid 22;
    procedure getSettings(const keyword: WideString; const values: IFMEOStringArray); dispid 23;
    procedure logSettings(const keyword: WideString); dispid 24;
    function createFeatureVectorOnDisk(maxEntriesInMem: Integer): IFMEOFeatureVectorOnDisk; dispid 25;
    procedure getSchemaFeatures(const macros: IFMEOStringArray; const prefix: WideString;
                                const schemaFile: WideString;
                                const schemaFeatures: IFMEOFeatureVector); dispid 26;
    property persistentCache[const directives: IFMEOStringArray]: IFMEOPersistentCache readonly dispid 27;
    function getProperties(const propertyCategory: WideString; const props: IFMEOStringArray): Integer; dispid 28;
    function getAllProperties(const props: IFMEOStringArray): Integer; dispid 29;
  end;

// *********************************************************************//
// Interface: IFMEODialog
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A9BA8BE1-5EF5-11D3-8486-00104B222BDB}
// *********************************************************************//
  IFMEODialog = interface (IDispatch)
    ['{A9BA8BE1-5EF5-11D3-8486-00104B222BDB}']
    procedure about(const applicationName: WideString);  safecall ;
    function generate(out parameterFileName: WideString; out workspaceFileName: WideString): Integer;  safecall ;
    function guessFormat(const filename: WideString): WideString;  safecall ;
    function destPrompt(const defaultDestFormat: WideString; const defaultDestDataset: WideString;
                        out destFormat: WideString; out destDataset: WideString;
                        const userDirectiveArray: IFMEOStringArray): Integer;  safecall ;
    function sourcePrompt(const defaultSourceFormat: WideString;
                          const defaultSourceDataset: WideString; out sourceFormat: WideString;
                          out sourceDataset: WideString; const userDirectiveArray: IFMEOStringArray): Integer;  safecall ;
    function xlatePrompt(const defaultSourceFormat: WideString;
                         const defaultSourceDataset: WideString;
                         const defaultDestFormat: WideString; const defaultDestDataset: WideString;
                         out sourceFormat: WideString; out sourceDataset: WideString;
                         const sourceUserDirectives: IFMEOStringArray; out destFormat: WideString;
                         out destDataset: WideString; const destUserDirectives: IFMEOStringArray): Integer;  safecall ;
    function coordSysPrompt(var coordSysName: WideString): Integer;  safecall ;
    function parameterPrompt(const filename: WideString): Integer;  safecall ;
    function setParentWindow(parentHandle: Integer): Integer;  safecall ;
    function getAvailableFormats(const availableFormatsList: IFMEOStringArray): Integer;  safecall ;
    function getFormatInfo(const format: WideString; const formatInfo: IFMEOStringArray): Integer;  safecall ;
    function getFormatInfoEx(const format: WideString; const formatInfo: IFMEOStringArray): Integer;  safecall ;
  end;

// *********************************************************************//
// DispIntf:  IFMEODialogDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A9BA8BE1-5EF5-11D3-8486-00104B222BDB}
// *********************************************************************//
  IFMEODialogDisp = dispinterface
    ['{A9BA8BE1-5EF5-11D3-8486-00104B222BDB}']
    procedure about(const applicationName: WideString); dispid 1;
    function generate(out parameterFileName: WideString; out workspaceFileName: WideString): Integer; dispid 2;
    function guessFormat(const filename: WideString): WideString; dispid 3;
    function destPrompt(const defaultDestFormat: WideString; const defaultDestDataset: WideString;
                        out destFormat: WideString; out destDataset: WideString;
                        const userDirectiveArray: IFMEOStringArray): Integer; dispid 4;
    function sourcePrompt(const defaultSourceFormat: WideString;
                          const defaultSourceDataset: WideString; out sourceFormat: WideString;
                          out sourceDataset: WideString; const userDirectiveArray: IFMEOStringArray): Integer; dispid 5;
    function xlatePrompt(const defaultSourceFormat: WideString;
                         const defaultSourceDataset: WideString;
                         const defaultDestFormat: WideString; const defaultDestDataset: WideString;
                         out sourceFormat: WideString; out sourceDataset: WideString;
                         const sourceUserDirectives: IFMEOStringArray; out destFormat: WideString;
                         out destDataset: WideString; const destUserDirectives: IFMEOStringArray): Integer; dispid 6;
    function coordSysPrompt(var coordSysName: WideString): Integer; dispid 7;
    function parameterPrompt(const filename: WideString): Integer; dispid 8;
    function setParentWindow(parentHandle: Integer): Integer; dispid 9;
    function getAvailableFormats(const availableFormatsList: IFMEOStringArray): Integer; dispid 10;
    function getFormatInfo(const format: WideString; const formatInfo: IFMEOStringArray): Integer; dispid 11;
    function getFormatInfoEx(const format: WideString; const formatInfo: IFMEOStringArray): Integer; dispid 12;
  end;

// *********************************************************************//
// Interface: IFMEOCoordSysManager
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {05A48C32-7C33-11D3-95BF-00105A1286A0}
// *********************************************************************//
  IFMEOCoordSysManager = interface (IDispatch)
    ['{05A48C32-7C33-11D3-95BF-00105A1286A0}']
    function get_reprojectDirectory: WideString;  safecall ;
    procedure defineCoordSys(const coordSysParms: IFMEOStringArray; var FMECoordSysName: WideString);  safecall ;
    function getCoordSysParms(const FMECoordSysName: WideString): IFMEOStringArray;  safecall ;
    procedure defineCoordSysFromOGCDef(const OGCDef: WideString; var FMECoordSysName: WideString);  safecall ;
    function getCoordSysAsOGCDef(const FMECoordSysName: WideString): WideString;  safecall ;
    function get_unit_(const unitName: WideString): IFMEOStringArray;  safecall ;
    procedure set_unit_(const unitName: WideString; const pVal: IFMEOStringArray);  safecall ;
    function get_datum(const datumName: WideString): IFMEOStringArray;  safecall ;
    procedure set_datum(const datumName: WideString; const pVal: IFMEOStringArray);  safecall ;
    function get_ellipsoid(const ellipsoidName: WideString): IFMEOStringArray;  safecall ;
    procedure set_ellipsoid(const ellipsoidName: WideString; const pVal: IFMEOStringArray);  safecall ;
    procedure readDefsFromFile(const filename: WideString);  safecall ;
    property reprojectDirectory: WideString read get_reprojectDirectory;
    property unit_[const unitName: WideString]: IFMEOStringArray read get_unit_ write set_unit_;
    property datum[const datumName: WideString]: IFMEOStringArray read get_datum write set_datum;
    property ellipsoid[const ellipsoidName: WideString]: IFMEOStringArray read get_ellipsoid write set_ellipsoid;
  end;

// *********************************************************************//
// DispIntf:  IFMEOCoordSysManagerDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {05A48C32-7C33-11D3-95BF-00105A1286A0}
// *********************************************************************//
  IFMEOCoordSysManagerDisp = dispinterface
    ['{05A48C32-7C33-11D3-95BF-00105A1286A0}']
    property reprojectDirectory: WideString readonly dispid 1;
    procedure defineCoordSys(const coordSysParms: IFMEOStringArray; var FMECoordSysName: WideString); dispid 2;
    function getCoordSysParms(const FMECoordSysName: WideString): IFMEOStringArray; dispid 3;
    procedure defineCoordSysFromOGCDef(const OGCDef: WideString; var FMECoordSysName: WideString); dispid 4;
    function getCoordSysAsOGCDef(const FMECoordSysName: WideString): WideString; dispid 5;
    property unit_[const unitName: WideString]: IFMEOStringArray dispid 6;
    property datum[const datumName: WideString]: IFMEOStringArray dispid 7;
    property ellipsoid[const ellipsoidName: WideString]: IFMEOStringArray dispid 8;
    procedure readDefsFromFile(const filename: WideString); dispid 9;
  end;

// *********************************************************************//
// Interface: IFMEOSpatialIndex
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {15BF7170-04F6-11D4-A79D-0090273EB14A}
// *********************************************************************//
  IFMEOSpatialIndex = interface (IDispatch)
    ['{15BF7170-04F6-11D4-A79D-0090273EB14A}']
    procedure open;  safecall ;
    procedure store(const feature: IFMEOFeature);  safecall ;
    procedure close(deleteIndex: Integer);  safecall ;
    function fetch(const feature: IFMEOFeature): Integer;  safecall ;
    procedure setMaxFileSize(maxFileBytes: Integer);  safecall ;
    function get_entries: Integer;  safecall ;
    procedure queryClosest(xCoord: Double; yCoord: Double; maxDist: Double);  safecall ;
    procedure queryEnvelope(const queryFeat: IFMEOFeature);  safecall ;
    procedure queryFarthest(xCoord: Double; yCoord: Double; maxDist: Double);  safecall ;
    procedure queryTotal(xCoord: Double; yCoord: Double; maxDist: Double);  safecall ;
    procedure queryCommonVertex(const queryFeat: IFMEOFeature);  safecall ;
    procedure queryLinesCross(const queryFeat: IFMEOFeature);  safecall ;
    procedure queryCommonSegment(const queryFeat: IFMEOFeature);  safecall ;
    procedure queryCommonVertexOrSegment(const queryFeat: IFMEOFeature);  safecall ;
    procedure queryAreaIntersect(const queryFeat: IFMEOFeature);  safecall ;
    procedure queryContainedBy(const queryFeat: IFMEOFeature);  safecall ;
    procedure queryContains(const queryFeat: IFMEOFeature);  safecall ;
    procedure queryIdentical(const queryFeat: IFMEOFeature);  safecall ;
    procedure queryAll;  safecall ;
    procedure storeUsingIndexRects(const feature: IFMEOFeature;
                                   const indexRects: IFMEORectangleVector);  safecall ;
    procedure boundingBox(out minX: Double; out maxX: Double; out minY: Double; out maxY: Double);  safecall ;
    property entries: Integer read get_entries;
  end;

// *********************************************************************//
// DispIntf:  IFMEOSpatialIndexDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {15BF7170-04F6-11D4-A79D-0090273EB14A}
// *********************************************************************//
  IFMEOSpatialIndexDisp = dispinterface
    ['{15BF7170-04F6-11D4-A79D-0090273EB14A}']
    procedure open; dispid 1;
    procedure store(const feature: IFMEOFeature); dispid 2;
    procedure close(deleteIndex: Integer); dispid 3;
    function fetch(const feature: IFMEOFeature): Integer; dispid 4;
    procedure setMaxFileSize(maxFileBytes: Integer); dispid 5;
    property entries: Integer readonly dispid 6;
    procedure queryClosest(xCoord: Double; yCoord: Double; maxDist: Double); dispid 7;
    procedure queryEnvelope(const queryFeat: IFMEOFeature); dispid 8;
    procedure queryFarthest(xCoord: Double; yCoord: Double; maxDist: Double); dispid 9;
    procedure queryTotal(xCoord: Double; yCoord: Double; maxDist: Double); dispid 10;
    procedure queryCommonVertex(const queryFeat: IFMEOFeature); dispid 11;
    procedure queryLinesCross(const queryFeat: IFMEOFeature); dispid 12;
    procedure queryCommonSegment(const queryFeat: IFMEOFeature); dispid 13;
    procedure queryCommonVertexOrSegment(const queryFeat: IFMEOFeature); dispid 14;
    procedure queryAreaIntersect(const queryFeat: IFMEOFeature); dispid 15;
    procedure queryContainedBy(const queryFeat: IFMEOFeature); dispid 16;
    procedure queryContains(const queryFeat: IFMEOFeature); dispid 17;
    procedure queryIdentical(const queryFeat: IFMEOFeature); dispid 18;
    procedure queryAll; dispid 19;
    procedure storeUsingIndexRects(const feature: IFMEOFeature;
                                   const indexRects: IFMEORectangleVector); dispid 20;
    procedure boundingBox(out minX: Double; out maxX: Double; out minY: Double; out maxY: Double); dispid 21;
  end;
// *********************************************************************//
// Interface: IFMEORectangleVector
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EF9C62A4-D0E3-4267-8A9C-E888EF262DD6}
// *********************************************************************//
  IFMEORectangleVector = interface (IDispatch)
    ['{EF9C62A4-D0E3-4267-8A9C-E888EF262DD6}']
    function get_entries: Integer;  safecall ;
    procedure append(const rectangle: IFMEORectangle);  safecall ;
    procedure insertAt(index: Integer; const rectangle: IFMEORectangle);  safecall ;
    function removeLast: IFMEORectangle;  safecall ;
    procedure clear;  safecall ;
    function contains(const rectangle: IFMEORectangle): Integer;  safecall ;
    procedure clearAndDestroy;  safecall ;
    function retrieve(index: Integer): IFMEORectangle;  safecall ;
    property entries: Integer read get_entries;
  end;

// *********************************************************************//
// DispIntf:  IFMEORectangleVectorDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {EF9C62A4-D0E3-4267-8A9C-E888EF262DD6}
// *********************************************************************//
  IFMEORectangleVectorDisp = dispinterface
    ['{EF9C62A4-D0E3-4267-8A9C-E888EF262DD6}']
    property entries: Integer readonly dispid 1;
    procedure append(const rectangle: IFMEORectangle); dispid 2;
    procedure insertAt(index: Integer; const rectangle: IFMEORectangle); dispid 3;
    function removeLast: IFMEORectangle; dispid 4;
    procedure clear; dispid 5;
    function contains(const rectangle: IFMEORectangle): Integer; dispid 6;
    procedure clearAndDestroy; dispid 7;
    function retrieve(index: Integer): IFMEORectangle; dispid 8;
  end;
// *********************************************************************//
// Interface: IFMEORectangle
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A31F911F-B2A8-4FB6-849A-E6B1A678FB68}
// *********************************************************************//
  IFMEORectangle = interface (IDispatch)
    ['{A31F911F-B2A8-4FB6-849A-E6B1A678FB68}']
    function get_minX: Double;  safecall ;
    procedure set_minX(pVal: Double);  safecall ;
    function get_maxX: Double;  safecall ;
    procedure set_maxX(pVal: Double);  safecall ;
    function get_minY: Double;  safecall ;
    procedure set_minY(pVal: Double);  safecall ;
    function get_maxY: Double;  safecall ;
    procedure set_maxY(pVal: Double);  safecall ;
    function intersects(const otherRect: IFMEORectangle): Integer;  safecall ;
    function contains(const otherRect: IFMEORectangle): Integer;  safecall ;
    function containsStrictly(const otherRect: IFMEORectangle): Integer;  safecall ;
    procedure expand(percentage: Double);  safecall ;
    procedure buffer(bufAmount: Double);  safecall ;
    function get_area: Double;  safecall ;
    procedure bufferXY(xBuf: Double; yBuf: Double);  safecall ;
    procedure clone(const targetRectangle: IFMEORectangle);  safecall ;
    function equals(const otherRect: IFMEORectangle): Integer;  safecall ;
    procedure unionRect(const otherRect: IFMEORectangle);  safecall ;
    property minX: Double read get_minX write set_minX;
    property maxX: Double read get_maxX write set_maxX;
    property minY: Double read get_minY write set_minY;
    property maxY: Double read get_maxY write set_maxY;
    property area: Double read get_area;
  end;

// *********************************************************************//
// DispIntf:  IFMEORectangleDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {A31F911F-B2A8-4FB6-849A-E6B1A678FB68}
// *********************************************************************//
  IFMEORectangleDisp = dispinterface
    ['{A31F911F-B2A8-4FB6-849A-E6B1A678FB68}']
    property minX: Double dispid 1;
    property maxX: Double dispid 2;
    property minY: Double dispid 3;
    property maxY: Double dispid 4;
    function intersects(const otherRect: IFMEORectangle): Integer; dispid 5;
    function contains(const otherRect: IFMEORectangle): Integer; dispid 6;
    function containsStrictly(const otherRect: IFMEORectangle): Integer; dispid 7;
    procedure expand(percentage: Double); dispid 8;
    procedure buffer(bufAmount: Double); dispid 9;
    property area: Double readonly dispid 10;
    procedure bufferXY(xBuf: Double; yBuf: Double); dispid 11;
    procedure clone(const targetRectangle: IFMEORectangle); dispid 12;
    function equals(const otherRect: IFMEORectangle): Integer; dispid 13;
    procedure unionRect(const otherRect: IFMEORectangle); dispid 14;
  end;

// *********************************************************************//
// Interface: IFMEOFeatureVectorOnDisk
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {014AE541-CFF4-4763-8DCC-4DDF52CB62C9}
// *********************************************************************//
  IFMEOFeatureVectorOnDisk = interface (IDispatch)
    ['{014AE541-CFF4-4763-8DCC-4DDF52CB62C9}']
    procedure append(const feature: IFMEOFeature);  safecall ;
    function element(pos: Integer): IFMEOFeature;  safecall ;
    function get_entries: Integer;  safecall ;
    procedure clear;  safecall ;
    property entries: Integer read get_entries;
  end;

// *********************************************************************//
// DispIntf:  IFMEOFeatureVectorOnDiskDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {014AE541-CFF4-4763-8DCC-4DDF52CB62C9}
// *********************************************************************//
  IFMEOFeatureVectorOnDiskDisp = dispinterface
    ['{014AE541-CFF4-4763-8DCC-4DDF52CB62C9}']
    procedure append(const feature: IFMEOFeature); dispid 1;
    function element(pos: Integer): IFMEOFeature; dispid 2;
    property entries: Integer readonly dispid 3;
    procedure clear; dispid 4;
  end;

// *********************************************************************//
// Interface: IFMEOPersistentCache
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3DEB950E-C524-4F54-B0D7-E2BF96E920A6}
// *********************************************************************//
  IFMEOPersistentCache = interface (IDispatch)
    ['{3DEB950E-C524-4F54-B0D7-E2BF96E920A6}']
    function get_property_(const name: WideString): WideString;  safecall ;
    procedure set_property_(const name: WideString; const pVal: WideString);  safecall ;
    procedure clear(const constraints: IFMEOStringArray);  safecall ;
    function get_entries(const readerNames: IFMEOStringArray): Integer;  safecall ;
    property property_[const name: WideString]: WideString read get_property_ write set_property_;
    property entries[const readerNames: IFMEOStringArray]: Integer read get_entries;
  end;

// *********************************************************************//
// DispIntf:  IFMEOPersistentCacheDisp
// Flags:     (4544) Dual NonExtensible OleAutomation Dispatchable
// GUID:      {3DEB950E-C524-4F54-B0D7-E2BF96E920A6}
// *********************************************************************//
  IFMEOPersistentCacheDisp = dispinterface
    ['{3DEB950E-C524-4F54-B0D7-E2BF96E920A6}']
    property property_[const name: WideString]: WideString dispid 1;
    procedure clear(const constraints: IFMEOStringArray); dispid 2;
    property entries[const readerNames: IFMEOStringArray]: Integer readonly dispid 3;
  end;

// *********************************************************************//
// The Class CoFMEOStringArray provides a Create and CreateRemote method to
// create instances of the default interface IFMEOStringArray exposed by
// the CoClass FMEOStringArray. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoFMEOStringArray = class
    class function Create: IFMEOStringArray;
    class function CreateRemote(const MachineName: string): IFMEOStringArray;
  end;

// *********************************************************************//
// The Class CoFMEOFeature provides a Create and CreateRemote method to
// create instances of the default interface IFMEOFeature exposed by
// the CoClass FMEOFeature. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoFMEOFeature = class
    class function Create: IFMEOFeature;
    class function CreateRemote(const MachineName: string): IFMEOFeature;
  end;

// *********************************************************************//
// The Class CoFMEOLogFile provides a Create and CreateRemote method to
// create instances of the default interface IFMEOLogFile exposed by
// the CoClass FMEOLogFile. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoFMEOLogFile = class
    class function Create: IFMEOLogFile;
    class function CreateRemote(const MachineName: string): IFMEOLogFile;
  end;

// *********************************************************************//
// The Class CoFMEOPipeline provides a Create and CreateRemote method to
// create instances of the default interface IFMEOPipeline exposed by
// the CoClass FMEOPipeline. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoFMEOPipeline = class
    class function Create: IFMEOPipeline;
    class function CreateRemote(const MachineName: string): IFMEOPipeline;
  end;

// *********************************************************************//
// The Class CoFMEOReader provides a Create and CreateRemote method to
// create instances of the default interface IFMEOReader exposed by
// the CoClass FMEOReader. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoFMEOReader = class
    class function Create: IFMEOReader;
    class function CreateRemote(const MachineName: string): IFMEOReader;
  end;

// *********************************************************************//
// The Class CoFMEOWriter provides a Create and CreateRemote method to
// create instances of the default interface IFMEOWriter exposed by
// the CoClass FMEOWriter. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoFMEOWriter = class
    class function Create: IFMEOWriter;
    class function CreateRemote(const MachineName: string): IFMEOWriter;
  end;

// *********************************************************************//
// The Class CoFMEOSession provides a Create and CreateRemote method to
// create instances of the default interface IFMEOSession exposed by
// the CoClass FMEOSession. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoFMEOSession = class
    class function Create: IFMEOSession;
    class function CreateRemote(const MachineName: string): IFMEOSession;
  end;

// *********************************************************************//
// The Class CoFMEOFeatureVector provides a Create and CreateRemote method to
// create instances of the default interface IFMEOFeatureVector exposed by
// the CoClass FMEOFeatureVector. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoFMEOFeatureVector = class
    class function Create: IFMEOFeatureVector;
    class function CreateRemote(const MachineName: string): IFMEOFeatureVector;
  end;

// *********************************************************************//
// The Class CoFMEODialog provides a Create and CreateRemote method to
// create instances of the default interface IFMEODialog exposed by
// the CoClass FMEODialog. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoFMEODialog = class
    class function Create: IFMEODialog;
    class function CreateRemote(const MachineName: string): IFMEODialog;
  end;

// *********************************************************************//
// The Class CoFMEOCoordSysManager provides a Create and CreateRemote method to
// create instances of the default interface IFMEOCoordSysManager exposed by
// the CoClass FMEOCoordSysManager. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoFMEOCoordSysManager = class
    class function Create: IFMEOCoordSysManager;
    class function CreateRemote(const MachineName: string): IFMEOCoordSysManager;
  end;

// *********************************************************************//
// The Class CoFMEOFactory provides a Create and CreateRemote method to
// create instances of the default interface IFMEOFactory exposed by
// the CoClass FMEOFactory. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoFMEOFactory = class
    class function Create: IFMEOFactory;
    class function CreateRemote(const MachineName: string): IFMEOFactory;
  end;

// *********************************************************************//
// The Class CoFMEOSpatialIndex provides a Create and CreateRemote method to
// create instances of the default interface IFMEOSpatialIndex exposed by
// the CoClass FMEOSpatialIndex. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoFMEOSpatialIndex = class
    class function Create: IFMEOSpatialIndex;
    class function CreateRemote(const MachineName: string): IFMEOSpatialIndex;
  end;

// *********************************************************************//
// The Class CoFMEORectangle provides a Create and CreateRemote method to
// create instances of the default interface IFMEORectangle exposed by
// the CoClass FMEORectangle. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoFMEORectangle = class
    class function Create: IFMEORectangle;
    class function CreateRemote(const MachineName: string): IFMEORectangle;
  end;

// *********************************************************************//
// The Class CoFMEORectangleVector provides a Create and CreateRemote method to
// create instances of the default interface IFMEORectangleVector exposed by
// the CoClass FMEORectangleVector. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoFMEORectangleVector = class
    class function Create: IFMEORectangleVector;
    class function CreateRemote(const MachineName: string): IFMEORectangleVector;
  end;

// *********************************************************************//
// The Class CoFMEOFeatureVectorOnDisk provides a Create and CreateRemote method to
// create instances of the default interface IFMEOFeatureVectorOnDisk exposed by
// the CoClass FMEOFeatureVectorOnDisk. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoFMEOFeatureVectorOnDisk = class
    class function Create: IFMEOFeatureVectorOnDisk;
    class function CreateRemote(const MachineName: string): IFMEOFeatureVectorOnDisk;
  end;

// *********************************************************************//
// The Class CoFMEOPersistentCache provides a Create and CreateRemote method to
// create instances of the default interface IFMEOPersistentCache exposed by
// the CoClass FMEOPersistentCache. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoFMEOPersistentCache = class
    class function Create: IFMEOPersistentCache;
    class function CreateRemote(const MachineName: string): IFMEOPersistentCache;
  end;

// *********************************************************************//
// The Class CoFMEOPartIterator provides a Create and CreateRemote method to
// create instances of the default interface IFMEOPartIterator exposed by
// the CoClass FMEOPartIterator. The functions are intended to be used by
// clients wishing to automate the CoClass objects exposed by the
// server of this typelibrary.
// *********************************************************************//
  CoFMEOPartIterator = class
    class function Create: IFMEOPartIterator;
    class function CreateRemote(const MachineName: string): IFMEOPartIterator;
  end;
implementation

uses
  System.Win.ComObj;

class function CoFMEOStringArray.Create: IFMEOStringArray;
begin
  Result := CreateComObject(CLASS_FMEOStringArray) as IFMEOStringArray;
end;

class function CoFMEOStringArray.CreateRemote(const MachineName: string): IFMEOStringArray;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FMEOStringArray) as IFMEOStringArray;
end;

class function CoFMEOFeature.Create: IFMEOFeature;
begin
  Result := CreateComObject(CLASS_FMEOFeature) as IFMEOFeature;
end;

class function CoFMEOFeature.CreateRemote(const MachineName: string): IFMEOFeature;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FMEOFeature) as IFMEOFeature;
end;

class function CoFMEOLogFile.Create: IFMEOLogFile;
begin
  Result := CreateComObject(CLASS_FMEOLogFile) as IFMEOLogFile;
end;

class function CoFMEOLogFile.CreateRemote(const MachineName: string): IFMEOLogFile;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FMEOLogFile) as IFMEOLogFile;
end;

class function CoFMEOPipeline.Create: IFMEOPipeline;
begin
  Result := CreateComObject(CLASS_FMEOPipeline) as IFMEOPipeline;
end;

class function CoFMEOPipeline.CreateRemote(const MachineName: string): IFMEOPipeline;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FMEOPipeline) as IFMEOPipeline;
end;

class function CoFMEOReader.Create: IFMEOReader;
begin
  Result := CreateComObject(CLASS_FMEOReader) as IFMEOReader;
end;

class function CoFMEOReader.CreateRemote(const MachineName: string): IFMEOReader;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FMEOReader) as IFMEOReader;
end;

class function CoFMEOWriter.Create: IFMEOWriter;
begin
  Result := CreateComObject(CLASS_FMEOWriter) as IFMEOWriter;
end;

class function CoFMEOWriter.CreateRemote(const MachineName: string): IFMEOWriter;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FMEOWriter) as IFMEOWriter;
end;

class function CoFMEOSession.Create: IFMEOSession;
begin
  Result := CreateComObject(CLASS_FMEOSession) as IFMEOSession;
end;

class function CoFMEOSession.CreateRemote(const MachineName: string): IFMEOSession;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FMEOSession) as IFMEOSession;
end;

class function CoFMEOFeatureVector.Create: IFMEOFeatureVector;
begin
  Result := CreateComObject(CLASS_FMEOFeatureVector) as IFMEOFeatureVector;
end;

class function CoFMEOFeatureVector.CreateRemote(const MachineName: string): IFMEOFeatureVector;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FMEOFeatureVector) as IFMEOFeatureVector;
end;

class function CoFMEODialog.Create: IFMEODialog;
begin
  Result := CreateComObject(CLASS_FMEODialog) as IFMEODialog;
end;

class function CoFMEODialog.CreateRemote(const MachineName: string): IFMEODialog;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FMEODialog) as IFMEODialog;
end;

class function CoFMEOCoordSysManager.Create: IFMEOCoordSysManager;
begin
  Result := CreateComObject(CLASS_FMEOCoordSysManager) as IFMEOCoordSysManager;
end;

class function CoFMEOCoordSysManager.CreateRemote(const MachineName: string): IFMEOCoordSysManager;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FMEOCoordSysManager) as IFMEOCoordSysManager;
end;

class function CoFMEOFactory.Create: IFMEOFactory;
begin
  Result := CreateComObject(CLASS_FMEOFactory) as IFMEOFactory;
end;

class function CoFMEOFactory.CreateRemote(const MachineName: string): IFMEOFactory;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FMEOFactory) as IFMEOFactory;
end;

class function CoFMEOSpatialIndex.Create: IFMEOSpatialIndex;
begin
  Result := CreateComObject(CLASS_FMEOSpatialIndex) as IFMEOSpatialIndex;
end;

class function CoFMEOSpatialIndex.CreateRemote(const MachineName: string): IFMEOSpatialIndex;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FMEOSpatialIndex) as IFMEOSpatialIndex;
end;

class function CoFMEORectangle.Create: IFMEORectangle;
begin
  Result := CreateComObject(CLASS_FMEORectangle) as IFMEORectangle;
end;

class function CoFMEORectangle.CreateRemote(const MachineName: string): IFMEORectangle;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FMEORectangle) as IFMEORectangle;
end;

class function CoFMEORectangleVector.Create: IFMEORectangleVector;
begin
  Result := CreateComObject(CLASS_FMEORectangleVector) as IFMEORectangleVector;
end;

class function CoFMEORectangleVector.CreateRemote(const MachineName: string): IFMEORectangleVector;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FMEORectangleVector) as IFMEORectangleVector;
end;

class function CoFMEOFeatureVectorOnDisk.Create: IFMEOFeatureVectorOnDisk;
begin
  Result := CreateComObject(CLASS_FMEOFeatureVectorOnDisk) as IFMEOFeatureVectorOnDisk;
end;

class function CoFMEOFeatureVectorOnDisk.CreateRemote(const MachineName: string): IFMEOFeatureVectorOnDisk;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FMEOFeatureVectorOnDisk) as IFMEOFeatureVectorOnDisk;
end;

class function CoFMEOPersistentCache.Create: IFMEOPersistentCache;
begin
  Result := CreateComObject(CLASS_FMEOPersistentCache) as IFMEOPersistentCache;
end;

class function CoFMEOPersistentCache.CreateRemote(const MachineName: string): IFMEOPersistentCache;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FMEOPersistentCache) as IFMEOPersistentCache;
end;

class function CoFMEOPartIterator.Create: IFMEOPartIterator;
begin
  Result := CreateComObject(CLASS_FMEOPartIterator) as IFMEOPartIterator;
end;

class function CoFMEOPartIterator.CreateRemote(const MachineName: string): IFMEOPartIterator;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_FMEOPartIterator) as IFMEOPartIterator;
end;

//==================================== END =====================================
end.
{$ENDIF}

