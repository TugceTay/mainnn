unit Lider.CG.Com.SettingsInt;

{$I Lider.CG.Com.Component.inc}

interface

uses
  SysUtils,
  Windows,
  Classes,
  Graphics,
  Controls,
  IniFiles,
  Lider.CG.Com.Base,
  Lider.CG.Com.GeoTypes,
  Lider.CG.Com.DrawToolsInt;

type
  /// <summary>
  ///   Rölöve (Röleve yanlýþ) ve Ölçülendirme
  /// </summary>
  IlicgDimension = interface
    ['{E0B1B9BC-EA01-4199-926B-7766C2AA3636}']
    function GetAlignmentCriteria: Double; stdcall;
    function GetArrowSymbolHeight: Double; stdcall;
    function GetArrowSymbolWidth: Double; stdcall;
    function GetDiameterText: Boolean; stdcall;
    function GetDiameterTextInteractive: Boolean; stdcall;
    function GetExtensionLine: Double; stdcall;
    function GetFontColor: TColor; stdcall;
    function GetLineSpacing: Double; stdcall;
    function GetLineType: Integer; stdcall;
    function GetPenColor: TColor; stdcall;
    function GetPenWidth: Double; stdcall;
    function GetPointNameHeight: Double; stdcall;
    function GetSaveToLayer: Integer; stdcall;
    function GetSymbolHeight: Double; stdcall;
    function GetSymbolNo: Integer; stdcall;
    function GetTextHeight: Double; stdcall;
    function GetTextVertHeight: Double; stdcall;
    function GetTrimZeros: Boolean; stdcall;
    function GetVectorelFontName: string; stdcall;
    procedure SetAlignmentCriteria(const Value: Double); stdcall;
    procedure SetArrowSymbolHeight(const Value: Double); stdcall;
    procedure SetArrowSymbolWidth(const Value: Double); stdcall;
    procedure SetDiameterText(const Value: Boolean); stdcall;
    procedure SetDiameterTextInteractive(const Value: Boolean); stdcall;
    procedure SetExtensionLine(const Value: Double); stdcall;
    procedure SetFontColor(const Value: TColor); stdcall;
    procedure SetLineSpacing(const Value: Double); stdcall;
    procedure SetLineType(const Value: Integer); stdcall;
    procedure SetPenColor(const Value: TColor); stdcall;
    procedure SetPenWidth(const Value: Double); stdcall;
    procedure SetPointNameHeight(const Value: Double); stdcall;
    procedure SetSaveToLayer(const Value: Integer); stdcall;
    procedure SetSymbolHeight(const Value: Double); stdcall;
    procedure SetSymbolNo(const Value: Integer); stdcall;
    procedure SetTextHeight(const Value: Double); stdcall;
    procedure SetTextVertHeight(const Value: Double); stdcall;
    procedure SetTrimZeros(const Value: Boolean); stdcall;
    procedure SetVectorelFontName(const Value: string); stdcall;
    // Rölöve
    /// <summary>
    ///  Rölöve - Dik Yazý Boyu [mm]. Tüm text nesnelerinde kullanýlacak yükseklik
    /// </summary>
    property TextVertHeight: Double read GetTextVertHeight write SetTextVertHeight;
    /// <summary>
    ///  Rölöve - Nokta Ad Boyu [mm]. Nokta adý text yüksekliði
    /// </summary>
    property PointNameHeight: Double read GetPointNameHeight write SetPointNameHeight;
    /// <summary>
    ///  Rölöve - Alýnman Kriteri [m]. Alinman mesafes kriteri
    /// </summary>
    property AlignmentCriteria: Double read GetAlignmentCriteria write SetAlignmentCriteria;
    /// <summary>
    ///  Rölöve - Kutur yazýlarý yazýlsýn mý  Default True;
    /// </summary>
    property DiameterText: Boolean read GetDiameterText write SetDiameterText;
    /// <summary>
    ///  Rölöve - //Cephe/Kutur yazýlarý etkileþimli
    /// </summary>
    property DiameterTextInteractive: Boolean read GetDiameterTextInteractive write SetDiameterTextInteractive;
    /// <summary>
    ///  Rölöve - Ara Mesafelerde 0'larý Kaldýr.
    /// </summary>
    property TrimZeros: Boolean read GetTrimZeros write SetTrimZeros;
    /// <summary>
    ///  Rölöve - Vektörel Font
    /// </summary>
    property VectorelFontName: string read GetVectorelFontName write SetVectorelFontName;
    /// <summary>
    ///  Rölöve - Font Rengi.
    /// </summary>
    property FontColor: TColor read GetFontColor write SetFontColor;
    /// <summary>
    ///  Rölöve - Hat Rengi.
    /// </summary>
    property PenColor: TColor read GetPenColor write SetPenColor;
    /// <summary>
    ///  Rölöve - Hat Geniþiliði
    /// </summary>
    property PenWidth: Double read GetPenWidth write SetPenWidth;
    /// <summary>
    ///  Rölöve - Hat Tipi
    /// </summary>
    property LineType: Integer read GetLineType write SetLineType;
    /// <summary>
    ///  Rölöve - Katmana Kaydet
    /// </summary>
    property SaveToLayer: Integer read GetSaveToLayer write SetSaveToLayer;

    // Ölçülendirme
    /// <summary>
    ///  Ölçülendirme - Çizgi Boþluðu (mm).
    /// </summary>
    property LineSpacing: Double read GetLineSpacing write SetLineSpacing;
    /// <summary>
    ///  Ölçülendirme - Çizgi Fazlalýðý (mm).
    /// </summary>
    property ExtensionLine: Double read GetExtensionLine write SetExtensionLine;
    /// <summary>
    ///  Ölçülendirme - Sembol No.
    /// </summary>
    property SymbolNo: Integer read GetSymbolNo write SetSymbolNo;
    /// <summary>
    ///  Ölçülendirme - Sembol Boyu (mm).
    /// </summary>
    property SymbolHeight: Double read GetSymbolHeight write SetSymbolHeight;
    /// <summary>
    ///  Ölçülendirme - Yazý Boyu.
    /// </summary>
    property TextHeight: Double read GetTextHeight write SetTextHeight;
    /// <summary>
    ///  Ölçülendirme - Ok Sembol Boyu.
    /// </summary>
    property ArrowSymbolHeight: Double read GetArrowSymbolHeight write SetArrowSymbolHeight;
    /// <summary>
    ///  Ölçülendirme - Ok Sembol Eni.
    /// </summary>
    property ArrowSymbolWidth: Double read GetArrowSymbolWidth write SetArrowSymbolWidth;
  end;

  /// <summary>
  ///   Duyarlýlýk (Ondalýk Basamak Sayýsý Yuvarlama)
  /// </summary>
  IlicgPrecision = interface
    ['{6487C8E0-5FAD-4F92-B7BF-7BDB74CBE4ED}']
    function GetAngle: Byte; stdcall;
    function GetArea: Byte; stdcall;
    function GetCoordinate: Byte; stdcall;
    function GetElevation: Byte; stdcall;
    function GetFlow: Byte; stdcall;
    function GetGeneral: Byte; stdcall;
    function GetLength: Byte; stdcall;
    function GetSlope: Byte; stdcall;
    function GetVelocity: Byte; stdcall;
    function GetVolume: Byte; stdcall;
    function GetWeight: Byte; stdcall;
    function GetVerticalBeamHeight: Double; stdcall;
    procedure SetAngle(const Value: Byte); stdcall;
    procedure SetArea(const Value: Byte); stdcall;
    procedure SetCoordinate(const Value: Byte); stdcall;
    procedure SetElevation(const Value: Byte); stdcall;
    procedure SetFlow(const Value: Byte); stdcall;
    procedure SetGeneral(const Value: Byte); stdcall;
    procedure SetLength(const Value: Byte); stdcall;
    procedure SetSlope(const Value: Byte); stdcall;
    procedure SetVelocity(const Value: Byte); stdcall;
    procedure SetVolume(const Value: Byte); stdcall;
    procedure SetWeight(const Value: Byte); stdcall;
    procedure SetVerticalBeamHeight(const Value: Double); stdcall;

    /// <summary>
    ///   Açý
    /// </summary>
    property Angle: Byte read GetAngle write SetAngle;
    /// <summary>
    ///   Alan
    /// </summary>
    property Area: Byte read GetArea write SetArea;
    /// <summary>
    ///   Koordinat (Nokta)
    /// </summary>
    property Coordinate: Byte read GetCoordinate write SetCoordinate;
    /// <summary>
    ///   Kot (Yükseklik)
    /// </summary>
    property Elevation: Byte read GetElevation write SetElevation;
    /// <summary>
    ///   Debi
    /// </summary>
    property Flow: Byte read GetFlow write SetFlow;
    /// <summary>
    ///   Genel
    /// </summary>
    property General: Byte read GetGeneral write SetGeneral;
    /// <summary>
    ///   Uzunluk
    /// </summary>
    property Length: Byte read GetLength write SetLength;
    /// <summary>
    ///   Eðim
    /// </summary>
    property Slope: Byte read GetSlope write SetSlope;
    /// <summary>
    ///   Hýz
    /// </summary>
    property Velocity: Byte read GetVelocity write SetVelocity;
    /// <summary>
    ///   Hacim
    /// </summary>
    property Volume: Byte read GetVolume write SetVolume;
    /// <summary>
    ///   Aðýrlýk
    /// </summary>
    property Weight: Byte read GetWeight write SetWeight;
    property VerticalBeamHeight: Double read GetVerticalBeamHeight write SetVerticalBeamHeight;
  end;

  IlicgUnits = interface
    ['{5937474B-C730-4D1D-AD68-714F0357DDE4}']
    function GetAngle: AnsiString; stdcall;
    function GetArea: AnsiString; stdcall;
    function GetCoordinate: AnsiString; stdcall;
    function GetElevation: AnsiString; stdcall;
    function GetFlow: AnsiString; stdcall;
    function GetKm: AnsiString; stdcall;
    function GetLength: AnsiString; stdcall;
    function GetSlope: AnsiString; stdcall;
    function GetVelocity: AnsiString; stdcall;
    function GetVolume: AnsiString; stdcall;
    function GetWeight: AnsiString; stdcall;
    procedure SetAngle(const Value: AnsiString); stdcall;
    procedure SetArea(const Value: AnsiString); stdcall;
    procedure SetCoordinate(const Value: AnsiString); stdcall;
    procedure SetElevation(const Value: AnsiString); stdcall;
    procedure SetFlow(const Value: AnsiString); stdcall;
    procedure SetKm(const Value: AnsiString); stdcall;
    procedure SetLength(const Value: AnsiString); stdcall;
    procedure SetSlope(const Value: AnsiString); stdcall;
    procedure SetVelocity(const Value: AnsiString); stdcall;
    procedure SetVolume(const Value: AnsiString); stdcall;
    procedure SetWeight(const Value: AnsiString); stdcall;

    /// <summary>
    ///   Açý <br /><list type="table">
    ///     <listheader>
    ///       <term>Kýsaltma</term>
    ///       <description>Açýklama</description>
    ///     </listheader>
    ///     <item>
    ///       <term>grad</term>
    ///       <description>grad</description>
    ///     </item>
    ///     <item>
    ///       <term>deg</term>
    ///       <description>degree <br /></description>
    ///     </item>
    ///     <item>
    ///       <term>rad</term>
    ///       <description>radian <br /></description>
    ///     </item>
    ///     <item>
    ///       <term>dms</term>
    ///       <description>degree minute second <br /></description>
    ///     </item>
    ///   </list>
    ///   <br />
    /// </summary>
    property Angle: AnsiString read GetAngle write SetAngle;
    /// <summary>
    ///   Alan
    /// </summary>
    property Area: AnsiString read GetArea write SetArea;
    /// <summary>
    ///   Koordinat (Nokta)
    /// </summary>
    property Coordinate: AnsiString read GetCoordinate write SetCoordinate;
    /// <summary>
    ///   Yükseklik
    /// </summary>
    property Elevation: AnsiString read GetElevation write SetElevation;
    /// <summary>
    ///   Debi
    /// </summary>
    property Flow: AnsiString read GetFlow write SetFlow;
    /// <summary>
    ///   Km +
    /// </summary>
    property Km: AnsiString read GetKm write SetKm;
    /// <summary>
    ///   Uzunluk
    /// </summary>
    property Length: AnsiString read GetLength write SetLength;
    /// <summary>
    ///   Eðim
    /// </summary>
    property Slope: AnsiString read GetSlope write SetSlope;
    /// <summary>
    ///   Hýz
    /// </summary>
    property Velocity: AnsiString read GetVelocity write SetVelocity;
    /// <summary>
    ///   Hacim
    /// </summary>
    property Volume: AnsiString read GetVolume write SetVolume;
    /// <summary>
    ///   Aðýrlýk
    /// </summary>
    property Weight: AnsiString read GetWeight write SetWeight;
  end;


  IlicgSettings = interface
    ['{DD2EF826-CC2B-4026-A447-1723415CCFC2}']
    procedure ApplyChanges; stdcall;
    procedure Assign(Source: IlicgSettings); stdcall;
    procedure SaveToFile(const FileName: string); stdcall;
    procedure LoadFromFile(const Filename: string); stdcall;
    procedure SaveToRegistry(ARegistryPath: string = ''); stdcall;
    procedure LoadFromRegistry(ARegistryPath: string = ''); stdcall;

    function GetDefaultLineTypeFileName: AnsiString; stdcall;
    procedure SetDefaultLineTypeFileName(const Value: AnsiString); stdcall;

    function GetDefaultSymbolFileName: AnsiString; stdcall;
    procedure SetDefaultSymbolFileName(const Value: AnsiString); stdcall;

    function GetCompression: Boolean; stdcall;
    procedure SetCompression(const Value: Boolean); stdcall;

    function GetDimension: IlicgDimension; stdcall;
    procedure SetDimension(const Value: IlicgDimension); stdcall;
    function GetPrecision: IlicgPrecision; stdcall;
    procedure SetPrecision(const Value: IlicgPrecision); stdcall;
    function GetUnits: IlicgUnits; stdcall;
    procedure SetUnits(const Value: IlicgUnits); stdcall;
    function GetPrint: TlicgPrint; stdcall;
    procedure SetPrint(Value: TlicgPrint); stdcall;

    procedure SetDefPenStyle(const Value: IlicgPenTool); stdcall;
    procedure SetDefBrushStyle(const Value: IlicgBrushTool); stdcall;
    procedure SetDefSymbolStyle(const Value: IlicgSymbolTool); stdcall;
    procedure SetDefBlockStyle(const Value: IlicgBlockTool); stdcall;
    procedure SetFontTool(const Value: IlicgFontTool); stdcall;

    procedure SetSelectionPen(const Value: IlicgPenTool); stdcall;
    procedure SetSelectionBrush(const Value: IlicgBrushTool); stdcall;
    procedure SetSelectionTransparent(const Value: Boolean); stdcall;
    procedure SetSelectionTransparency(const Value: Byte); stdcall;

    procedure SetHilitePen(const Value: IlicgPenTool); stdcall;
    procedure SetHiliteBrush(const Value: IlicgBrushTool); stdcall;
    procedure SetHiliteTransparent(const Value: Boolean); stdcall;
    procedure SetHiliteTransparency(const Value: Byte); stdcall;

    procedure SetBandsBitmapChunkSize(Value: Integer); stdcall;
    procedure SetHintFont(Value: TFont); stdcall;
    function GetAerialMinDrawLimit: integer; stdcall;
    function GetApertureWidth: integer; stdcall;
    function GetArcSegs: Integer; stdcall;
    function GetArcStyle: TlicgArcStyle; stdcall;
    function GetBandsBitmapChunkSize: Integer; stdcall;
    function GetControlPointsColor: TColor; stdcall;
    function GetControlPointsDisabledColor: TColor; stdcall;
    function GetControlPointsDisabledFillColor: TColor; stdcall;
    function GetControlPointsFillColor: TColor; stdcall;
    function GetControlPointsSize: integer; stdcall;
    function GetDefBrushStyle: IlicgBrushTool; stdcall;
    function GetDefPenStyle: IlicgPenTool; stdcall;
    function GetDefSymbolStyle: IlicgSymbolTool; stdcall;
    function GetDefBlockStyle: IlicgBlockTool; stdcall;
    function GetFontTool: IlicgFontTool; stdcall;
    function GetDirectionArrowSize: Double; stdcall;
    function GetEllipseSegs: Integer; stdcall;
    function GetFullViewCursor: Boolean; stdcall;
    function GetFullViewCursorColor: TColor; stdcall;
    function GetHintColor: TColor; stdcall;
    function GetHintFont: TFont; stdcall;
    function GetMaxSavedViews: Integer; stdcall;
    function GetMinDrawLimit: integer; stdcall;
//    function  GetOleContainerForm: TForm; stdcall;
    function GetOleContainerList: TlicgOleContainerList; stdcall;
    function GetPatternPlotterOptimized: Boolean; stdcall;
    function GetPlineGen: Boolean; stdcall;
    function GetPointEntitySize: Integer; stdcall;
    function GetPointEntityNameSize: Integer; stdcall;
    function GetPointEntityKodSize: Integer; stdcall; // ilker ekleme
    function GetPointEntityZSize: Integer; stdcall; // ilker ekleme
    function GetPointEntityLabelSize: Integer; stdcall; // ilker ekleme
    function GetPointEntityPointLabelDistance: Integer; stdcall; // ilker ekleme

    procedure SetPointEntityKodSize(const Value: Integer); stdcall; // ilker ekleme
    procedure SetPointEntityZSize(const Value: Integer); stdcall; // ilker ekleme
    procedure SetPointEntityLabelSize(const Value: Integer); stdcall; // ilker ekleme
    procedure SetPointEntityPointLabelDistance(const Value: Integer); stdcall; // ilker ekleme
    function GetPointEntityNameAll: Boolean; stdcall; // ilker ekleme
    procedure SetPointEntityNameAll(const Value: Boolean); stdcall; // ilker ekleme

    function GetPointCalculateElevation: Boolean; stdcall; // ilker ekleme
    procedure SetPointCalculateElevation(const Value: Boolean); stdcall; // ilker ekleme
    function GetPointCalculateElevationRadius: Double; stdcall; // ilker ekleme
    procedure SetPointCalculateElevationRadius(const Value: Double); stdcall; // ilker ekleme
    function GetPointCalculateElevationFromTriangle: Boolean; stdcall; // ilker ekleme
    procedure SetPointCalculateElevationFromTriangle(const Value: Boolean); stdcall; // ilker ekleme

    function GetPolygonEntityNameSize: Integer; stdcall;
    function GetPolygonEntityNameAll: Boolean; stdcall; // ilker ekleme
    procedure SetPolygonEntityNameAll(const Value: Boolean); stdcall; // ilker ekleme

    function GetPointWidth: Integer; stdcall;
    function GetPreloadedBandedImages: TStrings; stdcall;
    function GetPreloadedBlockNames: TStrings; stdcall;
    function GetPreloadedBlockInts: IInterfaceList; stdcall;
    function GetPreloadedImages: TStrings; stdcall;
    function GetRubberPenColor: TColor; stdcall;
    function GetRubberColorAuxLines: TColor; stdcall;
    function GetRubberCrossFrameColor: TColor; stdcall;
    function GetRubberFillColor: TColor; stdcall;
    function GetRubberFillColorRotate: TColor; stdcall;
    function GetRubberPenWidth: Byte; stdcall;
    function GetSegmentCurvePoints: Integer; stdcall;

    function GetSelectionBrush: IlicgBrushTool; stdcall;
    function GetSelectionPen: IlicgPenTool; stdcall;
    function GetSelectionTransparent: Boolean; stdcall;
    function GetSelectionTransparency: Byte; stdcall;

    function GetHiliteBrush: IlicgBrushTool; stdcall;
    function GetHilitePen: IlicgPenTool; stdcall;
    function GetHiliteTransparent: Boolean; stdcall;
    function GetHiliteTransparency: Byte; stdcall;

    function GetSelectPickingInside: Boolean; stdcall;
    function GetSplineSegs: Integer; stdcall;
    function GetTextAntialias: Boolean; stdcall;
    function GetUsePreloadedBandedImages: Boolean; stdcall;
    function GetUsePreloadedBlocks: Boolean; stdcall;
    procedure SetAerialMinDrawLimit(const Value: integer); stdcall;
    procedure SetApertureWidth(const Value: integer); stdcall;
    procedure SetArcSegs(const Value: Integer); stdcall;
    procedure SetArcStyle(const Value: TlicgArcStyle); stdcall;
    procedure SetControlPointsColor(const Value: TColor); stdcall;
    procedure SetControlPointsDisabledColor(const Value: TColor); stdcall;
    procedure SetControlPointsDisabledFillColor(const Value: TColor); stdcall;
    procedure SetControlPointsFillColor(const Value: TColor); stdcall;
    procedure SetDirectionArrowSize(const Value: Double); stdcall;
    procedure SetEllipseSegs(const Value: Integer); stdcall;
    procedure SetFullViewCursor(const Value: Boolean); stdcall;
    procedure SetFullViewCursorColor(const Value: TColor); stdcall;
    procedure SetGNumPoint(const Value: Integer); stdcall;
    procedure SetGRotatePoint(const Value: TlicgCoor); stdcall;
    procedure SetHintColor(const Value: TColor); stdcall;
    procedure SetMaxSavedViews(const Value: Integer); stdcall;
    procedure SetMinDrawLimit(const Value: integer); stdcall;
    procedure SetPatternPlotterOptimized(const Value: Boolean); stdcall;
    procedure SetPlineGen(const Value: Boolean); stdcall;
    procedure SetPointEntitySize(const Value: Integer); stdcall;
    procedure SetPointEntityNameSize(const Value: Integer); stdcall;
    procedure SetPolygonEntityNameSize(const Value: Integer); stdcall;
    procedure SetPointWidth(const Value: Integer); stdcall;
    procedure SetRubberPenColor(const Value: TColor); stdcall;
    procedure SetRubberColorAuxLines(const Value: TColor); stdcall;
    procedure SetRubberCrossFrameColor(const Value: TColor); stdcall;
    procedure SetRubberFillColor(const Value: TColor); stdcall;
    procedure SetRubberFillColorRotate(const Value: TColor); stdcall;
    procedure SetRubberPenWidth(const Value: Byte); stdcall;
    procedure SetSegmentCurvePoints(const Value: Integer); stdcall;
    procedure SetSelectPickingInside(const Value: Boolean); stdcall;
    procedure SetSplineSegs(const Value: Integer); stdcall;
    procedure SetTextAntialias(const Value: Boolean); stdcall;
    procedure SetUsePreloadedBandedImages(const Value: Boolean); stdcall;
    procedure SetUsePreloadedBlocks(const Value: Boolean); stdcall;
    function GetDrawBoxColor: TColor; stdcall;
    procedure SetDrawBoxColor(const Value: TColor); stdcall;

    function GetGRotatePoint: TlicgCoor; stdcall;
    function GetGNumPoint: Integer; stdcall;
    procedure SetControlPointsSize(Value: integer); stdcall;
    function GetOnChange: TNotifyEvent; stdcall;
    procedure SetOnChange(Value: TNotifyEvent); stdcall;
    function GetSnapEnable: Boolean; stdcall;
    procedure SetSnapEnable(Value: Boolean); stdcall;
    function GetSnapSensitivity: Byte; stdcall;
    procedure SetSnapSensitivity(Value: Byte); stdcall;
    function GetSnapOverrideSnap: Boolean; stdcall;
    procedure SetSnapOverrideSnap(Value: Boolean); stdcall;
    function GetSnapOverrideSetting: TlicgOSNAPSetting; stdcall;
    procedure SetSnapOverrideSetting(Value: TlicgOSNAPSetting); stdcall;
    function GetSnapSetting: TlicgOSNAPSetting; stdcall;
    procedure SetSnapSetting(Value: TlicgOSNAPSetting); stdcall;
    function GetSnapDivisor: Byte; stdcall;
    procedure SetSnapDivisor(Value: Byte); stdcall;
    function GetSnapCtrlShiftSuspend: Boolean; stdcall;
    procedure SetSnapCtrlShiftSuspend(Value: Boolean); stdcall;
    function GetSnapAuto: Boolean; stdcall;
    procedure SetSnapAuto(Value: Boolean); stdcall;
    function GetSnapSpace: Boolean; stdcall;
    procedure SetSnapSpace(Value: Boolean); stdcall;
    function GetSnapVertexSnapEnabled: Boolean; stdcall;
    procedure SetSnapVertexSnapEnabled(Value: Boolean); stdcall;
    function GetSnapCSDisabled: Boolean; stdcall;
    procedure SetSnapCSDisabled(Value: Boolean); stdcall;

    function GetSnapPoint: Boolean; stdcall;
    function GetSnapEndPoint: Boolean; stdcall;
    function GetSnapMidPoint: Boolean; stdcall;
    function GetSnapCenter: Boolean; stdcall;
    function GetSnapIntersect: Boolean; stdcall;
    function GetSnapPerpend: Boolean; stdcall;
    function GetSnapTangent: Boolean; stdcall;
    function GetSnapNearest: Boolean; stdcall;
    function GetSnapOrigin: Boolean; stdcall;
    function GetSnapParallel: Boolean; stdcall;
    function GetSnapKeyPoint: Boolean; stdcall;
    function GetSnapBisector: Boolean; stdcall;

    procedure SetSnapPoint(Value: Boolean); stdcall;
    procedure SetSnapEndPoint(Value: Boolean); stdcall;
    procedure SetSnapMidPoint(Value: Boolean); stdcall;
    procedure SetSnapCenter(Value: Boolean); stdcall;
    procedure SetSnapIntersect(Value: Boolean); stdcall;
    procedure SetSnapPerpend(Value: Boolean); stdcall;
    procedure SetSnapTangent(Value: Boolean); stdcall;
    procedure SetSnapNearest(Value: Boolean); stdcall;
    procedure SetSnapOrigin(Value: Boolean); stdcall;
    procedure SetSnapParallel(Value: Boolean); stdcall;
    procedure SetSnapKeyPoint(Value: Boolean); stdcall;
    procedure SetSnapBisector(Value: Boolean); stdcall;

    function GetOnlyDrawingWithSnap: Boolean; stdcall;
    procedure SetOnlyDrawingWithSnap(Value: Boolean); stdcall;
    function GetAccuDrawXAxisColor: TColor; stdcall;
    procedure SetAccuDrawXAxisColor(Value: TColor); stdcall;
    function GetAccuDrawYAxisColor: TColor; stdcall;
    procedure SetAccuDrawYAxisColor(Value: TColor); stdcall;
    function GetAccuDrawHiliteColor: TColor; stdcall;
    procedure SetAccuDrawHiliteColor(Value: TColor); stdcall;
    function GetAccuDrawFrameColor: TColor; stdcall;
    procedure SetAccuDrawFrameColor(Value: TColor); stdcall;
    function GetAccuDrawSnapColor: TColor; stdcall;
    procedure SetAccuDrawSnapColor(Value: TColor); stdcall;
    function GetAccuDrawEnabled: Boolean; stdcall;
    procedure SetAccuDrawEnabled(Value: Boolean); stdcall;
    function GetAccuDrawSnapToAxis: Boolean; stdcall;
    procedure SetAccuDrawSnapToAxis(Value: Boolean); stdcall;
    function GetAccuDrawSnapUnRotated: Boolean; stdcall;
    procedure SetAccuDrawSnapUnRotated(Value: Boolean); stdcall;
    function GetAccuDrawSnapSameDistance: Boolean; stdcall;
    procedure SetAccuDrawSnapSameDistance(Value: Boolean); stdcall;
    function GetAccuDrawRotateToSegments: Boolean; stdcall;
    procedure SetAccuDrawRotateToSegments(Value: Boolean); stdcall;
    function GetAccuDrawReshapeAdvance: Boolean; stdcall;
    procedure SetAccuDrawReshapeAdvance(Value: Boolean); stdcall;
    function GetAccuDrawWidth: integer; stdcall;
    procedure SetAccuDrawWidth(Value: integer); stdcall;
    function GetAccuDrawTolerance: integer; stdcall;
    procedure SetAccuDrawTolerance(Value: integer); stdcall;
    function GetAccuDrawPenWidth: integer; stdcall;
    procedure SetAccuDrawPenWidth(Value: integer); stdcall;
    function GetFrameStyle: TlicgFrameStyle; stdcall;
    procedure SetFrameStyle(Value: TlicgFrameStyle); stdcall;
    function GetAccuDrawSnapFormShow: Boolean; stdcall;
    procedure SetAccuDrawSnapFormShow(Value: boolean); stdcall;
    procedure SetTTextPenStyle(const Value: IlicgPenTool); stdcall;
    procedure SetTTextBrushStyle(const Value: IlicgBrushTool); stdcall;
    function GetTTextBrushStyle: IlicgBrushTool; stdcall;
    function GetTTextPenStyle: IlicgPenTool; stdcall;
    procedure SetVTextPenStyle(const Value: IlicgPenTool); stdcall;
    procedure SetVTextBrushStyle(const Value: IlicgBrushTool); stdcall;
    function GetVTextBrushStyle: IlicgBrushTool; stdcall;
    function GetVTextPenStyle: IlicgPenTool; stdcall;
    procedure SetFirstPickedEntity(Value: Boolean); stdcall;
    function GetFirstPickedEntity: Boolean; stdcall;
    function GetAutoSave: Boolean; stdcall;
    procedure SetAutoSave(const Value: Boolean); stdcall;
    function GetAutoSaveTime: Integer; stdcall;
    procedure SetAutoSaveTime(Value: Integer); stdcall;
    function GetShowProgress: Boolean; stdcall;
    procedure SetShowProgress(Value: Boolean); stdcall;
    function GetPointNameSize: string; stdcall;
    procedure SetPointNameSize(const Value: string); stdcall;
    function GetCopyMode: Boolean; stdcall;
    procedure SetCopyMode(Value: Boolean); stdcall;
    function GetAlignVertical_PointName: TlicgAlignVerticalText; stdcall;
    procedure SetAlignVertical_PointName(Value: TlicgAlignVerticalText); stdcall;
    function GetAlignHorizantal_PointName: TlicgAlignHorizantalText; stdcall;
    procedure SetAlignHorizantal_PointName(Value: TlicgAlignHorizantalText); stdcall;
    function GetAlignVertical_PointKod: TlicgAlignVerticalText; stdcall;
    procedure SetAlignVertical_PointKod(Value: TlicgAlignVerticalText); stdcall;
    function GetAlignHorizantal_PointKod: TlicgAlignHorizantalText; stdcall;
    procedure SetAlignHorizantal_PointKod(Value: TlicgAlignHorizantalText); stdcall;
    function GetAlignVertical_PointKot: TlicgAlignVerticalText; stdcall;
    procedure SetAlignVertical_PointKot(Value: TlicgAlignVerticalText); stdcall;
    function GetAlignHorizantal_PointKot: TlicgAlignHorizantalText; stdcall;
    procedure SetAlignHorizantal_PointKot(Value: TlicgAlignHorizantalText); stdcall;
    function GetShowPointKotByCoor: boolean; stdcall;
    procedure SetShowPointKotByCoor(Value: boolean); stdcall;
    function GetShowHintEntityRubberVertex: boolean; stdcall;
    procedure SetShowHintEntityRubberVertex(const Value: boolean); stdcall;
    procedure SetBalastroRadius(const Value: Double); stdcall;
    function GetBalastroRadius: Double; stdcall;

    function GetControlMidPointsColor: TColor; stdcall;
    function GetControlMidPointsFillColor: TColor; stdcall;
    function GetControlPointsNumberColor: TColor; stdcall;
    function GetControlPointsNumberSize: Integer; stdcall;
    function GetControlEditPointsColor: TColor; stdcall;
    function GetControlEditPointsFillColor: TColor; stdcall;

    procedure SetControlMidPointsColor(const Value: TColor); stdcall;
    procedure SetControlMidPointsFillColor(const Value: TColor); stdcall;
    procedure SetControlPointsNumberColor(const Value: TColor); stdcall;
    procedure SetControlPointsNumberSize(const Value: Integer); stdcall;
    procedure SetControlEditPointsColor(const Value: TColor); stdcall;
    procedure SetControlEditPointsFillColor(const Value: TColor); stdcall;

    /// <summary>
    ///   Rölöve ve Ölçülendirme
    /// </summary>
    property Dimension: IlicgDimension read GetDimension write SetDimension;
    /// <summary>
    ///   Duyarlýlýk (Ondalýk Basamak Sayýsý Yuvarlama)
    /// </summary>
    property Precision: IlicgPrecision read GetPrecision write SetPrecision;
    /// <summary>
    ///   Ölçü Birimleri
    /// </summary>
    property Units: IlicgUnits read GetUnits write SetUnits;
    property Print: TlicgPrint read GetPrint write SetPrint;

    property DefaultLineTypeFileName: AnsiString read GetDefaultLineTypeFileName write SetDefaultLineTypeFileName;
    property DefaultSymbolFileName: AnsiString read GetDefaultSymbolFileName write SetDefaultSymbolFileName;

    property Compression: Boolean read GetCompression write SetCompression;
    property GRotatePoint: TlicgCoor read GetGRotatePoint write SetGRotatePoint;
    property GNumPoint: Integer read GetGNumPoint write SetGNumPoint;
    property DirectionArrowSize: Double read GetDirectionArrowSize write SetDirectionArrowSize;
    property PointEntitySize: Integer read GetPointEntitySize write SetPointEntitySize;
    property PointEntityNameSize: Integer read GetPointEntityNameSize write SetPointEntityNameSize;
    property PointEntityKodSize: Integer read GetPointEntityKodSize write SetPointEntityKodSize; // ilker ekleme
    property PointEntityZSize: Integer read GetPointEntityZSize write SetPointEntityZSize; // ilker ekleme
    property PointEntityLabelSize: Integer read GetPointEntityLabelSize write SetPointEntityLabelSize; // ilker ekleme
    property PointEntityPointLabelDistance: Integer read GetPointEntityPointLabelDistance write SetPointEntityPointLabelDistance; // ilker ekleme
    property PointEntityNameAll: Boolean read GetPointEntityNameAll write SetPointEntityNameAll; // ilker ekleme

    property PointCalculateElevation: Boolean read GetPointCalculateElevation write SetPointCalculateElevation; // ilker ekleme
    property PointCalculateElevationRadius: Double read GetPointCalculateElevationRadius write SetPointCalculateElevationRadius; // ilker ekleme
    property PointCalculateElevationFromTriangle: Boolean read GetPointCalculateElevationFromTriangle write SetPointCalculateElevationFromTriangle; // ilker ekleme

    property PolygonEntityNameSize: Integer read GetPolygonEntityNameSize write SetPolygonEntityNameSize;
    property PolygonEntityNameAll: Boolean read GetPolygonEntityNameAll write SetPolygonEntityNameAll; // ilker ekleme

    property PreloadedBlockNames: TStrings read GetPreloadedBlockNames;
    property PreloadedBlockInts: IInterfaceList read GetPreloadedBlockInts;
    property PreloadedImages: TStrings read GetPreloadedImages;
    property PreloadedBandedImages: TStrings read GetPreloadedBandedImages;
    property UsePreloadedBlocks: Boolean read GetUsePreloadedBlocks write SetUsePreloadedBlocks;
    property UsePreloadedBandedImages: Boolean read GetUsePreloadedBandedImages write SetUsePreloadedBandedImages;
    property PatternPlotterOptimized: Boolean read GetPatternPlotterOptimized write SetPatternPlotterOptimized;
    property PlineGen: Boolean read GetPlineGen write SetPlineGen;
    property SplineSegs: Integer read GetSplineSegs write SetSplineSegs;
    property EllipseSegs: Integer read GetEllipseSegs write SetEllipseSegs;
    property ArcSegs: Integer read GetArcSegs write SetArcSegs;
    property SegmentCurvePoints: Integer read GetSegmentCurvePoints write SetSegmentCurvePoints;
    property BandsBitmapChunkSize: Integer read GetBandsBitmapChunkSize write SetBandsBitmapChunkSize;
    property DefPenStyle: IlicgPenTool read GetDefPenStyle write SetDefPenStyle;
    property DefBrushStyle: IlicgBrushTool read GetDefBrushStyle write SetDefBrushStyle;
    property DefSymbolStyle: IlicgSymbolTool read GetDefSymbolStyle write SetDefSymbolStyle;
    property DefBlockStyle: IlicgBlockTool read GetDefBlockStyle write SetDefBlockStyle;
    property FontTool: IlicgFontTool read GetFontTool write SetFontTool;

    property SelectionPen: IlicgPenTool read GetSelectionPen write SetSelectionPen;
    property SelectionBrush: IlicgBrushTool read GetSelectionBrush write SetSelectionBrush;
    property SelectionTransparent: Boolean read GetSelectionTransparent write SetSelectionTransparent;
    property SelectionTransparency: Byte read GetSelectionTransparency write SetSelectionTransparency;

    property HilitePen: IlicgPenTool read GetHilitePen write SetHilitePen;
    property HiliteBrush: IlicgBrushTool read GetHiliteBrush write SetHiliteBrush;
    property HiliteTransparent: Boolean read GetHiliteTransparent write SetHiliteTransparent;
    property HiliteTransparency: Byte read GetHiliteTransparency write SetHiliteTransparency;

    property ApertureWidth: integer read GetApertureWidth write SetApertureWidth;
    property MinDrawLimit: integer read GetMinDrawLimit write SetMinDrawLimit;
    property AerialMinDrawLimit: integer read GetAerialMinDrawLimit write SetAerialMinDrawLimit;
    property SelectPickingInside: Boolean read GetSelectPickingInside write SetSelectPickingInside;
    property HintColor: TColor read GetHintColor write SetHintColor;
    property HintFont: TFont read GetHintFont write SetHintFont;
    property MaxSavedViews: Integer read GetMaxSavedViews write SetMaxSavedViews;
    property PointWidth: Integer read GetPointWidth write SetPointWidth;
    property ArcStyle: TlicgArcStyle read GetArcStyle write SetArcStyle;
    property TextAntialias: Boolean read GetTextAntialias write SetTextAntialias;
    // rubber colors
    property FullViewCursor: Boolean read GetFullViewCursor write SetFullViewCursor;
    property FullViewCursorColor: TColor read GetFullViewCursorColor write SetFullViewCursorColor;
    property RubberPenWidth: Byte read GetRubberPenWidth write SetRubberPenWidth;
    property RubberPenColor: TColor read GetRubberPenColor write SetRubberPenColor;
    property RubberFillColor: TColor read GetRubberFillColor write SetRubberFillColor;
    property RubberFillColorRotate: TColor read GetRubberFillColorRotate write SetRubberFillColorRotate;
    property RubberCrossFrameColor: TColor read GetRubberCrossFrameColor write SetRubberCrossFrameColor;
    property RubberColorAuxLines: TColor read GetRubberColorAuxLines write SetRubberColorAuxLines;

    property ControlPointsSize: integer read GetControlPointsSize write SetControlPointsSize;
    property ControlPointsColor: TColor read GetControlPointsColor write SetControlPointsColor;
    property ControlPointsFillColor: TColor read GetControlPointsFillColor write SetControlPointsFillColor;

    property ControlPointsDisabledColor: TColor read GetControlPointsDisabledColor write SetControlPointsDisabledColor;
    property ControlPointsDisabledFillColor: TColor read GetControlPointsDisabledFillColor write SetControlPointsDisabledFillColor;

    property ControlMidPointsColor: TColor read GetControlMidPointsColor write SetControlMidPointsColor;
    property ControlMidPointsFillColor: TColor read GetControlMidPointsFillColor write SetControlMidPointsFillColor;

    property ControlPointsNumberSize: Integer read GetControlPointsNumberSize write SetControlPointsNumberSize;
    property ControlPointsNumberColor: TColor read GetControlPointsNumberColor write SetControlPointsNumberColor;

    property ControlEditPointsColor: TColor read GetControlEditPointsColor write SetControlEditPointsColor;
    property ControlEditPointsFillColor: TColor read GetControlEditPointsFillColor write SetControlEditPointsFillColor;

    property DrawBoxColor: TColor read GetDrawBoxColor write SetDrawBoxColor;
    property SnapEnable: Boolean read GetSnapEnable write SetSnapEnable;
    property SnapSensitivity: Byte read GetSnapSensitivity write SetSnapSensitivity;
    property SnapOverride: Boolean read GetSnapOverrideSnap write SetSnapOverrideSnap;
    property SnapOverrideSetting: TlicgOSNAPSetting read GetSnapOverrideSetting write SetSnapOverrideSetting;
    property SnapSetting: TlicgOSNAPSetting read GetSnapSetting write SetSnapSetting;
    property SnapDivisor: Byte read GetSnapDivisor write SetSnapDivisor;
    property SnapCtrlShiftSuspend: Boolean read GetSnapCtrlShiftSuspend write SetSnapCtrlShiftSuspend;
    property SnapAuto: Boolean read GetSnapAuto write SetSnapAuto;
    property SnapVertexSnapEnabled: Boolean read GetSnapVertexSnapEnabled write SetSnapVertexSnapEnabled;
    property SnapCSDisabled: Boolean read GetSnapCSDisabled write SetSnapCSDisabled;
    property SnapSpace: Boolean read GetSnapSpace write SetSnapSpace;

    property SnapPoint: Boolean read GetSnapPoint write SetSnapPoint;
    property SnapEndPoint: Boolean read GetSnapEndPoint write SetSnapEndPoint;
    property SnapMidPoint: Boolean read GetSnapMidPoint write SetSnapMidPoint;
    property SnapCenter: Boolean read GetSnapCenter write SetSnapCenter;
    property SnapIntersect: Boolean read GetSnapIntersect write SetSnapIntersect;
    property SnapPerpend: Boolean read GetSnapPerpend write SetSnapPerpend;
    property SnapTangent: Boolean read GetSnapTangent write SetSnapTangent;
    property SnapNearest: Boolean read GetSnapNearest write SetSnapNearest;
    property SnapOrigin: Boolean read GetSnapOrigin write SetSnapOrigin;
    property SnapParallel: Boolean read GetSnapParallel write SetSnapParallel;
    property SnapKeyPoint: Boolean read GetSnapKeyPoint write SetSnapKeyPoint;
    property SnapBisector: Boolean read GetSnapBisector write SetSnapBisector;

    property AccuDrawXAxisColor: TColor read GetAccuDrawXAxisColor write SetAccuDrawXAxisColor;
    property AccuDrawYAxisColor: TColor read GetAccuDrawYAxisColor write SetAccuDrawYAxisColor;
    property AccuDrawHiliteColor: TColor read GetAccuDrawHiliteColor write SetAccuDrawHiliteColor;
    property AccuDrawFrameColor: TColor read GetAccuDrawFrameColor write SetAccuDrawFrameColor;
    property AccuDrawSnapColor: TColor read GetAccuDrawSnapColor write SetAccuDrawSnapColor;
    property AccuDrawEnabled: Boolean read GetAccuDrawEnabled write SetAccuDrawEnabled;
    property AccuDrawSnapToAxis: Boolean read GetAccuDrawSnapToAxis write SetAccuDrawSnapToAxis;
    property AccuDrawSnapUnRotated: Boolean read GetAccuDrawSnapUnRotated write SetAccuDrawSnapUnRotated;
    property AccuDrawSnapSameDistance: Boolean read GetAccuDrawSnapSameDistance write SetAccuDrawSnapSameDistance;
    property AccuDrawRotateToSegments: Boolean read GetAccuDrawRotateToSegments write SetAccuDrawRotateToSegments;
    property AccuDrawReshapeAdvance: Boolean read GetAccuDrawReshapeAdvance write SetAccuDrawReshapeAdvance;
    property AccuDrawWidth: integer read GetAccuDrawWidth write SetAccuDrawWidth;
    property AccuDrawTolerance: integer read GetAccuDrawTolerance write SetAccuDrawTolerance;
    property AccuDrawPenWidth: integer read GetAccuDrawPenWidth write SetAccuDrawPenWidth;
    property FrameStyle: TlicgFrameStyle read GetFrameStyle write SetFrameStyle;
    //property AccuDrawSnapFormShow: Boolean read GetAccuDrawSnapFormShow write SetAccuDrawSnapFormShow;
    //property OnlyDrawingWithSnap      : Boolean read GetOnlyDrawingWithSnap write SetOnlyDrawingWithSnap;

    property TTextPenStyle: IlicgPenTool read GetTTextPenStyle write SetTTextPenStyle;
    property TTextBrushStyle: IlicgBrushTool read GetTTextBrushStyle write SetTTextBrushStyle;
    property VTextPenStyle: IlicgPenTool read GetVTextPenStyle write SetVTextPenStyle;
    property VTextBrushStyle: IlicgBrushTool read GetVTextBrushStyle write SetVTextBrushStyle;
    property FirstPickedEntity: Boolean read GetFirstPickedEntity write SetFirstPickedEntity;
    property AutoSave: Boolean read GetAutoSave write SetAutoSave;
    property AutoSaveTime: Integer read GetAutoSaveTime write SetAutoSaveTime;
    property OleContainerList: TlicgOleContainerList read GetOleContainerList;
    property ShowProgress: Boolean read GetShowProgress write SetShowProgress;

    property PointNameSize: string read GetPointNameSize write SetPointNameSize;
    property CopyMode: Boolean read GetCopyMode write SetCopyMode;
    property AlignVertical_PointKot: TlicgAlignVerticalText read GetAlignVertical_PointKot write SetAlignVertical_PointKot;
    property AlignHorizantal_PointKot: TlicgAlignHorizantalText read GetAlignHorizantal_PointKot write SetAlignHorizantal_PointKot;
    property AlignVertical_PointName: TlicgAlignVerticalText read GetAlignVertical_PointName write SetAlignVertical_PointName;
    property AlignHorizantal_PointName: TlicgAlignHorizantalText read GetAlignHorizantal_PointName write SetAlignHorizantal_PointName;
    property AlignVertical_PointKod: TlicgAlignVerticalText read GetAlignVertical_PointKod write SetAlignVertical_PointKod;
    property AlignHorizantal_PointKod: TlicgAlignHorizantalText read GetAlignHorizantal_PointKod write SetAlignHorizantal_PointKod;
    property ShowPointKotByCoor: boolean read GetShowPointKotByCoor write SetShowPointKotByCoor;
    property ShowHintEntityRubberVertex: boolean read GetShowHintEntityRubberVertex write SetShowHintEntityRubberVertex;
    property BalastroRadius: Double read GetBalastroRadius write SetBalastroRadius;
    { events }
    property OnChange: TNotifyEvent read GetOnChange write SetOnChange;
  end;

implementation

end.


