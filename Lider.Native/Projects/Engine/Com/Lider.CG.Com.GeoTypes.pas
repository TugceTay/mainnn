unit Lider.CG.Com.GeoTypes;

interface

uses
  System.Classes;

type // ilker
  { Koordinat Type }
  TlicgCoorPtr = ^TlicgCoor;
  TlicgCoor3DPtr = ^TlicgCoor3D;
  TlicgCoor4DPtr = ^TlicgCoor4D;

  TlicgCoor = record
    X, Y: Double;
    class operator Add(A, B: TlicgCoor): TlicgCoor; // uður ekleme 23.07.2016
    class operator Equal(A, B: TlicgCoor): Boolean; //  uður ekleme 23.07.2016
    class operator Subtract(A, B: TlicgCoor): TlicgCoor; // uður ekleme 23.07.2016

    class operator Add(A: TlicgCoor; B: TlicgCoor3DPtr): TlicgCoor; // uður ekleme 02.03.2020
    class operator Equal(A: TlicgCoor; B: TlicgCoor3DPtr): Boolean; //  uður ekleme 02.03.2020
    class operator Subtract(A: TlicgCoor; B: TlicgCoor3DPtr): TlicgCoor; // uður ekleme 02.03.2020
    class operator Implicit(Value: TlicgCoor3DPtr): TlicgCoor; // uður ekleme 02.03.2020

    class operator Add(A: TlicgCoor; B: TlicgCoor4DPtr): TlicgCoor; // uður ekleme 02.03.2020
    class operator Equal(A: TlicgCoor; B: TlicgCoor4DPtr): Boolean; //  uður ekleme 02.03.2020
    class operator Subtract(A: TlicgCoor; B: TlicgCoor4DPtr): TlicgCoor; // uður ekleme 02.03.2020
    class operator Implicit(Value: TlicgCoor4DPtr): TlicgCoor; // uður ekleme 02.03.2020
  end;
  TlicgCoorArray = array of TlicgCoor;

  (*uður ekleme baþlama*)
  TlicgPaftaScale = (psNone = 0, ps100000 = 100000, ps50000 = 50000, ps25000 = 25000,
    ps10000 = 10000, ps5000 = 5000, ps2000 = 2000, ps1000 = 1000, ps500 = 500);
  (*uður ekleme bitme*)

  TlicgCoor3D = record
    X, Y, Z: Double;
    class operator Add(A, B: TlicgCoor3D): TlicgCoor3D; // uður ekleme 23.07.2016
    class operator Equal(A, B: TlicgCoor3D): Boolean; // uður ekleme 23.07.2016
    class operator Subtract(A, B: TlicgCoor3D): TlicgCoor3D; // uður ekleme 23.07.2016

    class operator Add(A: TlicgCoor3D; B: TlicgCoor): TlicgCoor3D; // uður ekleme 02.03.2020
    class operator Equal(A: TlicgCoor3D; B: TlicgCoor): Boolean; //  uður ekleme 02.03.2020
    class operator Subtract(A: TlicgCoor3D; B: TlicgCoor): TlicgCoor3D; // uður ekleme 02.03.2020
    class operator Implicit(Value: TlicgCoor): TlicgCoor3D; // uður ekleme 02.03.2020

    class operator Add(A: TlicgCoor3D; B: TlicgCoor4DPtr): TlicgCoor3D; // uður ekleme 02.03.2020
    class operator Equal(A: TlicgCoor3D; B: TlicgCoor4DPtr): Boolean; //  uður ekleme 02.03.2020
    class operator Subtract(A: TlicgCoor3D; B: TlicgCoor4DPtr): TlicgCoor3D; // uður ekleme 02.03.2020
    class operator Implicit(Value: TlicgCoor4DPtr): TlicgCoor3D; // uður ekleme 02.03.2020
  end;

  TlicgCoor3DPtrE = ^TlicgCoorE;
  TlicgCoorE = record
    X, Y: Extended;
  end;

  { yazarken ve okurken eskiye uyumluluk için "geçici" olarak kullanýlan type'lar.}
  TlicgTempCoor = packed record
    X: Double; // Vertex x component
    Y: Double; // Vertex y component
  end;

  TlicgTempCoor3D = packed record
    P: TlicgTempCoor;
    Z: Single;
  end;

  TlicgCoorArray3D = array of TlicgCoor3D;
  PlicgCoorArray3D = ^TlicgCoorArray3D;

  TlicgCoor4D = record
    X, Y, Z, N: Double;
    class operator Add(A, B: TlicgCoor4D): TlicgCoor4D; // uður ekleme 23.07.2016
    class operator Equal(A, B: TlicgCoor4D): Boolean; // uður ekleme 23.07.2016
    class operator Subtract(A, B: TlicgCoor4D): TlicgCoor4D; // uður ekleme 23.07.2016

    class operator Add(A: TlicgCoor4D; B: TlicgCoor): TlicgCoor4D; // uður ekleme 02.03.2020
    class operator Equal(A: TlicgCoor4D; B: TlicgCoor): Boolean; //  uður ekleme 02.03.2020
    class operator Subtract(A: TlicgCoor4D; B: TlicgCoor): TlicgCoor4D; // uður ekleme 02.03.2020
    class operator Implicit(Value: TlicgCoor): TlicgCoor4D; // uður ekleme 02.03.2020

    class operator Add(A: TlicgCoor4D; B: TlicgCoor3D): TlicgCoor4D; // uður ekleme 02.03.2020
    class operator Equal(A: TlicgCoor4D; B: TlicgCoor3D): Boolean; //  uður ekleme 02.03.2020
    class operator Subtract(A: TlicgCoor4D; B: TlicgCoor3D): TlicgCoor4D; // uður ekleme 02.03.2020
    class operator Implicit(Value: TlicgCoor3D): TlicgCoor4D; // uður ekleme 02.03.2020
  end;

  TlicgCoorArray4D = array of TlicgCoor4D;
  PlicgCoorArray4D = ^TlicgCoorArray4D;

  { Line Type }
  TlicgLinePtr = ^TlicgLine;
  TlicgLine = array[1..2] of TlicgCoor;
  TlicgLineArray = array of TlicgLine;

  TlicgLine3DPtr = ^TlicgLine3D;
  TlicgLine3D = array[1..2] of TlicgCoor3D;
  TlicgLine3DArray = array of TlicgLine3D;

  { Triangle Type }
  TlicgTrianglePtr = ^TlicgTriangle;
  TlicgTriangle = array[1..3] of TlicgCoor;
  TlicgTriangleArray = array of TlicgTriangle;
  TlicgTriangle3DPtr = ^TlicgTriangle3D;
  TlicgTriangle3D = array[1..3] of TlicgCoor3D;
  TlicgTriangle3DArray = array of TlicgTriangle3D;

  { Extent Type }
  TlicgExtent = record
    case Byte of
      0: (LowerLeft, UpperRight: TlicgCoor);
      1: (X1, Y1, X2, Y2: Double);
  end;

  TlicgExtent4D = record
    case Byte of
      0: (LowerLeft, UpperRight: TlicgCoor4D);
      1: (X1, Y1, Z1, N1, X2, Y2, Z2, N2: Double);
  end;

  TlicgPoint = packed record
    x: Double; // Vertex x component
    y: Double; // Vertex y component
    //z: Single; // Z value
  end;

  TlicgScaleUnits = (suInches, suMs, suKms, suDms, suCms, suMms, suDeg);

  TAffineMatrixElements = record
    HorizantalTraslate, VerticalTranslante: Double;
  end;
  PAffineMatrixElements = ^TAffineMatrixElements;

  TlicgRasterCoor = record
    Pixel, World: TlicgCoor;
    Active: Boolean;
  end;

  TReferenceGISExtent = record
    Xmin, Ymin, XMax, YMax: double;
  end;

const
  { Polygon Parts Status }
  _PART_MAIN_ISLAND =  0;
  _PART_HOLE        =  1;
  _PART_CW          =  1;
  _PART_CCW         = -1;

  // ileride saat yönü hesaplamasýný tüm kodlarda düzenle
  { Rotation constants }
  Clockwise = -1;
  NotImportant = 0;
  CounterClockwise = +1;

  _EPSILON_ULTRA = 1.0E-30;
  _EPSILON       = 1.0E-8; // ilker deðiþtirme 1.0E-16;
  _EPSILON_FUZZ  = 1.0E-6;
  _EPSILON_LOW   = 1.0E-04;

  { Geometric Dimensions }
  _2D = 2;
  _3D = 3;
  _4D = 4;

  TwoPi = 2 * System.Pi;

  _MAXCOOR = 1.0E+100;
  _MINCOOR = -1.0E+100;
  _NULL_COOR: TlicgCoor = (X: 0; Y: 0 );
  _NULL_COOR_3D: TlicgCoor3D = (X: 0; Y: 0; Z: 0);
  _NULL_COOR_4D: TlicgCoor4D = (X: 0; Y: 0; Z: 0; N: 0;);
  _INVALID_COOR: TlicgCoor = (X: _MINCOOR; Y: _MINCOOR);
  _INVALID_COOR_3D: TlicgCoor3D = (X: _MINCOOR; Y: _MINCOOR; Z: _MINCOOR);
  _INVALID_COOR_4D: TlicgCoor4D = (X: _MINCOOR; Y: _MINCOOR; Z: _MINCOOR; N: _MINCOOR);

   //Resim ekle de EqualExtension fonksiyonundan dönen deðer yanlýþtý. Ayrýca taným aþaðýdaki gibi olacaksa
   //Lider.CG.Com. libExt.pas (1824.satýr) if EqualExtension(TmpWin, _NULL_EXTENT) or EqualExtension(TmpWin, _INVALID_EXTENT) gibi bir kod anlamsýz olur.
   //_NULL_EXTENT    : TlicgExtent = (LowerLeft: (X: _MAXCOOR; Y: _MAXCOOR); UpperRight: (X: _MINCOOR; Y: _MINCOOR)); //(LowerLeft: (X: 0; Y: 0); UpperRight: (X: 0; Y: 0));

  _NULL_EXTENT: TlicgExtent = (LowerLeft: (X: 0; Y: 0); UpperRight: (X: 0; Y: 0));
  _INVALID_EXTENT: TlicgExtent = (LowerLeft : (X: _MAXCOOR; Y: _MAXCOOR); UpperRight: (X: _MINCOOR; Y: _MINCOOR));
  _INVALID_EXTENT_4D: TlicgExtent4D = (LowerLeft : (X: _MAXCOOR; Y: _MAXCOOR; Z: _MAXCOOR; N: _MAXCOOR);
                                          UpperRight: (X: _MINCOOR; Y: _MINCOOR; Z: _MINCOOR; N: _MINCOOR));

   //Resim ekle de EqualExtension fonksiyonundan dönen deðer yanlýþtý. Ayrýca taným aþaðýdaki gibi olacaksa
   //   _NULL_EXTENT_4D    : TlicgExtent4D = (LowerLeft: (X: _MAXCOOR; Y: _MAXCOOR; Z: _MAXCOOR; N: _MAXCOOR); UpperRight: (X: _MINCOOR; Y: _MINCOOR; Z: _MINCOOR; N: _MINCOOR)); //(LowerLeft: (X: 0; Y: 0; Z: 0; N: 0); UpperRight: (X: 0; Y: 0; Z: 0; N: 0));
  _NULL_EXTENT_4D: TlicgExtent4D = (LowerLeft : (X: 0; Y: 0; Z: 0; N: 0);
                                       UpperRight: (X: 0; Y: 0; Z: 0; N: 0));
  _DEFAULT_EXTENT: TlicgExtent = (LowerLeft : (X: 28.8304853439331055; Y: 40.2791002671666263);
                                  UpperRight: (X: 28.8794088363647461; Y: 40.3124884049840091));

  PrintPenWidths: array[0..15] of Double =  // nc
   (0.07, 0.14, 0.21, 0.28, 0.35, 0.42, 0.49, 0.56, 0.63, 0.70, 0.77, 0.84, 0.91, 0.98, 1.05, 1.12);

  (* ilker ihtiyaç yok sil yada aç
  _NULL_EXTENT_REFERENCEGIS: TReferenceGISExtent = (
    Xmin: 0;
    Ymin: 0;
    XMax: 0;
    YMax: 0
  ); *)

  _INVALID_EXTENT_REFERENCEGIS: TReferenceGISExtent  = (
    Xmin: _MAXCOOR;
    Ymin: _MAXCOOR;
    XMax: _MINCOOR;
    YMax: _MINCOOR
  );

type
  PlicgMatrix = ^TlicgMatrix;

  TlicgMatrix = packed record
    Matrix: array[0..2, 0..2] of Double;
  end;

  {  The developers guide explains the following enumeration more in depth }
  TlicgGraphicOperator = (
    goWithin,
    goEntirelyWithin,
    goContains,
    goContainsEntire,
    goIntersects,
    goEntirelyWithinNoEdgeTouched,
    goContainsEntireNoEdgeTouched,
    goExtentOverlaps,
    goShareCommonPoint,
    goShareCommonLine,
    goLineCross,
    goCommonPointOrLineCross,
    goEdgeTouch,
    goEdgeTouchOrIntersect,
    goPointInPolygon,
    goCentroidInPolygon,
    goIdentical,
    goDisjoint);

  TlicgArcStyle = (asOpened, asPie, asChord);
  TlicgCurveType = (ctNone = 0, ctFitPoints = 1, ctSpline = 2);
  TlicgControlPointType = (cptNone, cptNode, cptMove, cptRotate, cptSegment, cptBezier, cptDisable);
  TlicgControlPointTypes = set of TlicgControlPointType;
  TlicgCoorsChangedEvent = procedure(Grapher: TObject = nil) of object; stdcall; // parametrelerle daha da zenginleþebilir.

  // TClipper için Make Offset Type
  TlicgJoinType = (jtSquare, jtRound, jtMiter);
  TlicgEndType = (etClosedPolygon, etClosedLine, etOpenButt, etOpenSquare, etOpenRound);
  TlicgClipType = (ClipTypeNoClip, ClipTypeIntersection, ClipTypeUnion, ClipTypeDifference, ClipTypeXor);

  TlicgOperationType = (opIntersection, opUnion, opDifference, opSymmetricalDifference);

  // Units
  TlicgAngleType = (atGrad, atDegree, atRadian, atDms);
  (*TlicgLengthType = (ltMeter, ltMiliMeter, ltCentiMeter, ltDeciMeter,
ltDecameter,
hectometer
kilometer
inch
foot
yard
statute mile
nautical mile *)



  (*uður ekleme baþlama*)
  TlicgTextPos = (tpCenter, tpCenterUp, tpUpperLeft, tpUpperRight, tpCenterLeft,
    tpCenterRight, tpLowerLeft, tpCenterDown, tpLowerRight, tpCenterUpOut, tpUpperLeftOutUp,
    tpUpperRightOutUp, tpCenterLeftOut, tpCenterRightOut, tpLowerLeftOutDown, tpCenterDownOut,
    tpLowerRightOutDown, tpUpperLeftOutLeft, tpUpperRightOutRigth, tpLowerLeftOutLeft,
    tpLowerRightOutRigth, tpUpperLeftCorner, tpUpperRightCorner, tpLowerLeftCorner,
    tpLowerRightCorner);
  (*uður ekleme bitiþ*)

const
  licgColorMap: array[0..255] of Integer = (
  $FFFFFF, $FF0000, $00FF00, $FFFF00, $0000FF, $0080FF, $008080, $00CCFF,
  $C0C0C0, $FF8000, $FF00FF, $80FF80, $FF8080, $8080FF, $00FFFF, $FFFFFF,
  $000000, $DB0000, $00DB00, $DBDB00, $0000DB, $006EDB, $006E6E, $00B3DF,
  $4731B4, $DB6E00, $DB00DB, $6FDF6F, $DB6E6E, $6F6FDF, $00DFDF, $46557D,
  $666666, $B70000, $00B700, $B7B700, $0000B7, $005CB7, $005C5C, $009ABF,
  $433496, $B75C00, $B700B7, $5FBF5F, $B75C5C, $5F5FBF, $00BFBF, $475372,
  $999999, $930000, $009300, $939300, $000093, $004A93, $004A4A, $00819F,
  $3F347D, $934A00, $930093, $509F50, $934A4A, $50509F, $009F9F, $374260,
  $CCCCCC, $6F0000, $006F00, $6F6F00, $00006F, $00386F, $003838, $00687F,
  $3A346A, $6F3800, $6F006F, $407F40, $6F3838, $40407F, $007F7F, $293355,
  $FFFFFF, $5F0000, $004B00, $4B4B00, $00004B, $00264B, $002626, $004F5F,
  $353258, $4B2600, $4B004B, $305F30, $4B2626, $30305F, $005F5F, $242B47,
  $494434, $5C3016, $3D5B00, $6E1952, $2424FF, $1024CE, $19CF9A, $1F81E5,
  $4F1C67, $804D00, $621DA5, $357757, $907A00, $312BD5, $24FFFF, $1E2681,
  $656054, $6A4037, $4A7100, $833F5F, $4848FF, $1F31D6, $49D5A8, $3D8DE8,
  $5F1D7D, $8F5D27, $781CD7, $527E68, $A38C08, $444CDC, $48FFFF, $1F2BA1,
  $898273, $805B51, $5C8100, $9B607C, $6C6CFF, $344CDB, $69DAB3, $599CEB,
  $713B8C, $A16E48, $8746E0, $3E9468, $B8A144, $5866E1, $6CFFFF, $355AB3,
  $979685, $927270, $8AA94D, $BB8C9F, $9090FF, $4C67E0, $81E2C1, $73ADEE,
  $85569E, $B88565, $8E5DD3, $55A378, $D0BC85, $6F7FE6, $90FFFF, $4D71C5,
  $B2AE99, $B59D9D, $A7C177, $C297AC, $B4B4FF, $667FE6, $9BE9D2, $8CC0F2,
  $AE89C0, $D1A084, $9B76D1, $6CB48A, $E4CCB3, $8899EB, $B4FFFF, $6789DA,
  $E8E4D7, $D9C3B6, $C3D3A2, $CBA4B9, $D8D8FF, $8199EB, $C4F1E3, $ACD2F6,
  $CEB1E0, $E0BBA7, $A78BD0, $A4E0BF, $F1EBDC, $A8B8F2, $D8FFFF, $A5C2F3,
  $6367B3, $7566B5, $4A2AD5, $7477E5, $6755E0, $8280B8, $5C516B, $6C538C,
  $BA97F0, $977FB9, $AA9CBD, $845E94, $8B4EB4, $721D8C, $85426E, $6F395D,
  $B0C5F4, $8199EB, $5D77E4, $3D66B2, $44548A, $8DC5F3, $00C5F3, $679AEB,
  $489AEB, $4477E4, $609EBB, $5981B6, $64BFBF, $41494B, $7ADAB3, $28C17B,
  $8F8576, $6B645B, $474440, $5D5D48, $68703C, $7A7B5B, $818D40, $6A8D40,
  $BFAE3C, $998D42, $9CAC92, $C5DBB3, $9DC177, $7B8676, $698675, $68AC92,
  $9C99EC, $59606D, $D3AEB8, $B37F8C, $AF7566, $995C6C, $926D77, $73534A,
  $59474C, $816B5F, $BE7500, $EFC075, $BBAA92, $8F8576, $6B645B, $0000FF );

  { topological operations Const}
    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_EQUALITY       = 'T*F**FFF'  ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_DISJOINT       = 'FF*FF'     ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_INTERSECT      = 'T'         ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_INTERSECT1     = '*T'        ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_INTERSECT2     = '***T'      ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_INTERSECT3     = '****T'     ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_TOUCH_INTERIOR = 'F**T'      ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_TOUCH          = 'F***T'     ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_CROSS          = 'T*T'       ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_CROSS_LINE     = '0'         ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_WITHIN                         = 'T*F**F'    ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_WITHIN_COMPLETELY              = 'TFF*FF' ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_CONTAINS                       = 'T*****FF' ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_CONTAINS_COMPLETELY            = 'T**FF*FF'  ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_OVERLAP                        = 'T*T***T'   ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_OVERLAP_LINE                   = '1*T***T'   ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_LINE_CROSS_POLYGON             = 'T*T'     ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_POLYGON_CROSSED_BY_LINE        = 'T*****T' ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_LINE_CROSS_LINE                = '0'       ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_LINE_TRAVERS_POLYGON           = 'T**F'    ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_POLYGON_TRAVERSED_BY_LINE      = 'TF'      ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_LINE_CROSSTRAVERS_POLYGON      = 'T*TFF'   ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_POLYGON_CROSSTRAVERSED_BY_LINE = 'TF**F*T' ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_INTERSECT_INTERIOR_INTERIOR    = 'T'       ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_INTERSECT_INTERIOR_BOUNDARY    = '*T'      ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_INTERSECT_BOUNDARY_INTERIOR    = '***T'    ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_INTERSECT_BOUNDARY_BOUNDARY    = '****T'   ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_TOUCH_BOUNDARY_INTERIOR        = 'F**T'    ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_TOUCH_INTERIOR_BOUNDARY        = 'FT'      ;

    /// <summary>
    ///   Predefine DE-9IM array.
    /// </summary>
    _RELATE_TOUCH_BOUNDARY_BOUNDARY        = 'F***T'   ;

function AsCoor(X, Y: Double): TlicgCoor;
function AsCoor3D(X, Y, Z: Double): TlicgCoor3D; overload;
function AsCoor3D(ACoor: TlicgCoor; Z: Double): TlicgCoor3D; overload;
function AsCoor4D(ACoor: TlicgCoor; Z, N: Double): TlicgCoor4D; overload;
function AsCoor4D(ACoor: TlicgCoor3D; N: Double): TlicgCoor4D; overload;
function AsCoor4D(X, Y, Z, N: Double): TlicgCoor4D; overload;

implementation

function AsCoor(X, Y: Double): TlicgCoor;
begin
  Result.X := X;
  Result.Y := Y;
end;

function AsCoor3D(X, Y, Z: Double): TlicgCoor3D;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
end;

function AsCoor3D(ACoor: TlicgCoor; Z: Double): TlicgCoor3D;
begin
  Result.X := ACoor.X;
  Result.Y := ACoor.Y;
  Result.Z := Z;
end;

function AsCoor4D(X, Y, Z, N: Double): TlicgCoor4D;
begin
  Result.X := X;
  Result.Y := Y;
  Result.Z := Z;
  Result.N := N;
end;

function AsCoor4D(ACoor: TlicgCoor; Z, N: Double): TlicgCoor4D;
begin
  Result.X := ACoor.X;
  Result.Y := ACoor.Y;
  Result.Z := Z;
  Result.N := N;
end;

function AsCoor4D(ACoor: TlicgCoor3D; N: Double): TlicgCoor4D;
begin
  Result.X := ACoor.X;
  Result.Y := ACoor.Y;
  Result.Z := ACoor.Z;
  Result.N := N;
end;

(* uður ekleme baslama 23.07.2016 *)
{ TlicgCoor }
class operator TlicgCoor.Add(A, B: TlicgCoor): TlicgCoor;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TlicgCoor.Equal(A, B: TlicgCoor): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

class operator TlicgCoor.Subtract(A, B: TlicgCoor): TlicgCoor;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
end;

class operator TlicgCoor.Add(A: TlicgCoor; B: TlicgCoor3DPtr): TlicgCoor;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TlicgCoor.Equal(A: TlicgCoor; B: TlicgCoor3DPtr): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

class operator TlicgCoor.Implicit(Value: TlicgCoor3DPtr): TlicgCoor;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
end;

class operator TlicgCoor.Subtract(A: TlicgCoor; B: TlicgCoor3DPtr): TlicgCoor;
begin
  Result.X := A.X - B.X;
  Result.Y := B.X - B.Y;
end;

class operator TlicgCoor.Add(A: TlicgCoor; B: TlicgCoor4DPtr): TlicgCoor;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
end;

class operator TlicgCoor.Equal(A: TlicgCoor; B: TlicgCoor4DPtr): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

class operator TlicgCoor.Implicit(Value: TlicgCoor4DPtr): TlicgCoor;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
end;

class operator TlicgCoor.Subtract(A: TlicgCoor; B: TlicgCoor4DPtr): TlicgCoor;
begin
  Result.X := A.X - B.X;
  Result.Y := B.X - B.Y;
end;

{ TlicgCoor3D }
class operator TlicgCoor3D.Add(A, B: TlicgCoor3D): TlicgCoor3D;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
end;

class operator TlicgCoor3D.Equal(A, B: TlicgCoor3D): Boolean;
begin
  Result := False;
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

class operator TlicgCoor3D.Subtract(A, B: TlicgCoor3D): TlicgCoor3D;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
end;
(* uður ekleme bitis 23.07.2016 *)

class operator TlicgCoor3D.Add(A: TlicgCoor3D; B: TlicgCoor): TlicgCoor3D;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z;
end;

class operator TlicgCoor3D.Equal(A: TlicgCoor3D; B: TlicgCoor): Boolean;
begin
  Result := False;
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

class operator TlicgCoor3D.Implicit(Value: TlicgCoor): TlicgCoor3D;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
  Result.Z := Result.Z;
end;

class operator TlicgCoor3D.Subtract(A: TlicgCoor3D; B: TlicgCoor): TlicgCoor3D;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z;
end;

class operator TlicgCoor3D.Add(A: TlicgCoor3D; B: TlicgCoor4DPtr): TlicgCoor3D;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z;
end;

class operator TlicgCoor3D.Equal(A: TlicgCoor3D; B: TlicgCoor4DPtr): Boolean;
begin
  Result := False;
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

class operator TlicgCoor3D.Implicit(Value: TlicgCoor4DPtr): TlicgCoor3D;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
  Result.Z := Result.Z;
end;

class operator TlicgCoor3D.Subtract(A: TlicgCoor3D;
  B: TlicgCoor4DPtr): TlicgCoor3D;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z;

end;

{ TlicgCoor4D }

class operator TlicgCoor4D.Add(A: TlicgCoor4D; B: TlicgCoor3D): TlicgCoor4D;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
  Result.N := A.N;
end;

class operator TlicgCoor4D.Add(A: TlicgCoor4D; B: TlicgCoor): TlicgCoor4D;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z;
  Result.N := A.N;
end;

class operator TlicgCoor4D.Add(A, B: TlicgCoor4D): TlicgCoor4D;
begin
  Result.X := A.X + B.X;
  Result.Y := A.Y + B.Y;
  Result.Z := A.Z + B.Z;
  Result.N := A.N + B.N;
end;

class operator TlicgCoor4D.Equal(A: TlicgCoor4D; B: TlicgCoor3D): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y) and (A.Z = B.Z);
end;

class operator TlicgCoor4D.Equal(A: TlicgCoor4D; B: TlicgCoor): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y);
end;

class operator TlicgCoor4D.Equal(A, B: TlicgCoor4D): Boolean;
begin
  Result := (A.X = B.X) and (A.Y = B.Y) and (A.Z = B.Z) and (A.N = B.N);
end;

class operator TlicgCoor4D.Implicit(Value: TlicgCoor): TlicgCoor4D;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
  Result.Z := Result.Z;
  Result.N := Result.N;
end;

class operator TlicgCoor4D.Implicit(Value: TlicgCoor3D): TlicgCoor4D;
begin
  Result.X := Value.X;
  Result.Y := Value.Y;
  Result.Z := Value.Z;
  Result.N := Result.N;
end;

class operator TlicgCoor4D.Subtract(A, B: TlicgCoor4D): TlicgCoor4D;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
  Result.N := A.N - B.N;
end;

class operator TlicgCoor4D.Subtract(A: TlicgCoor4D; B: TlicgCoor): TlicgCoor4D;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z;
  Result.N := A.N;
end;

class operator TlicgCoor4D.Subtract(A: TlicgCoor4D;
  B: TlicgCoor3D): TlicgCoor4D;
begin
  Result.X := A.X - B.X;
  Result.Y := A.Y - B.Y;
  Result.Z := A.Z - B.Z;
  Result.N := A.N;
end;

end.


