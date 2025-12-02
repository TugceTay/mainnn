
{****************************************************************}
{                                                                }
{                        XML Data Binding                        }
{                                                                }
{         Generated on: 14.04.2023 12:20:18                      }
{       Generated from: C:\Users\Developer\Desktop\Ref\Ref.xml   }
{   Settings stored in: C:\Users\Developer\Desktop\Ref\Ref.xdb   }
{                                                                }
{****************************************************************}

unit Lider.CG.Lib.Referens;

interface

uses Xml.xmldom, Xml.XMLDoc, Xml.XMLIntf;

type

{ Forward Decls }

  IXMLReferenceManagerType = interface;
  IXMLGroupsType = interface;
  IXMLGroupType = interface;
  IXMLReferencesType = interface;
  IXMLReferenceType = interface;
  IXMLClipType = interface;
  IXMLPolygonsType = interface;
  IXMLPolygonType = interface;
  IXMLPointsType = interface;
  IXMLCoorType = interface;

{ IXMLReferenceManagerType }

  IXMLReferenceManagerType = interface(IXMLNode)
    ['{12A9DBD6-49C6-482A-A9C5-919681B857E4}']
    { Property Accessors }
    function GetActive: Integer;
    function GetGroups: IXMLGroupsType;
    procedure SetActive(const Value: Integer);
    { Methods & Properties }
    property Active: Integer read GetActive write SetActive;
    property Groups: IXMLGroupsType read GetGroups;
  end;

{ IXMLGroupsType }

  IXMLGroupsType = interface(IXMLNodeCollection)
    ['{16ED1642-CFD8-4FDA-BE5C-CC47E4424873}']
    { Property Accessors }
    function GetGroup(const Index: Integer): IXMLGroupType;
    { Methods & Properties }
    function Add: IXMLGroupType;
    function Insert(const Index: Integer): IXMLGroupType;
    property Group[const Index: Integer]: IXMLGroupType read GetGroup; default;
  end;

{ IXMLGroupType }

  IXMLGroupType = interface(IXMLNode)
    ['{AD19CF96-EF83-48D4-92B2-2C9CCBA19A51}']
    { Property Accessors }
    function GetID: Integer;
    function GetActive: Integer;
    function GetName: UnicodeString;
    function GetReferences: IXMLReferencesType;
    procedure SetID(const Value: Integer);
    procedure SetActive(const Value: Integer);
    procedure SetName(const Value: UnicodeString);
    { Methods & Properties }
    property ID: Integer read GetID write SetID;
    property Active: Integer read GetActive write SetActive;
    property Name: UnicodeString read GetName write SetName;
    property References: IXMLReferencesType read GetReferences;
  end;

{ IXMLReferencesType }

  IXMLReferencesType = interface(IXMLNodeCollection)
    ['{C9037A36-ABA6-4787-B3C6-52FF5D708E1B}']
    { Property Accessors }
    function GetReference(const Index: Integer): IXMLReferenceType;
    { Methods & Properties }
    function Add: IXMLReferenceType;
    function Insert(const Index: Integer): IXMLReferenceType;
    property Reference[const Index: Integer]: IXMLReferenceType read GetReference; default;
  end;

{ IXMLReferenceType }

  IXMLReferenceType = interface(IXMLNode)
    ['{5521400D-06C9-4ECB-B2E5-C18033382940}']
    { Property Accessors }
    function GetID: UnicodeString;
    function GetActive: Integer;
    function GetSnappable: Integer;
    function GetPrintable: Integer;
    function GetShowBorders: Integer;
    function GetTransparent: Integer;
    function GetInterpretation: Integer;
    function GetMinimumScale: Integer;
    function GetMaximumScale: Integer;
    function GetColorsRed: Integer;
    function GetColorsGreen: Integer;
    function GetColorsBlue: Integer;
    function GetColorsBrightness: Integer;
    function GetColorsContrast: Integer;
    function GetColorsInversion: Integer;
    function GetColorsGrayscale: Integer;
    function GetColorsHistogram: Integer;
    function GetColorsContrastEnhanced: Integer;
    function GetBandsRed: Integer;
    function GetBandsGreen: Integer;
    function GetBandsBlue: Integer;
    function GetBandsAlpha: Integer;
    function GetBandsPage: Integer;
    function GetKind: UnicodeString;
    function GetName: UnicodeString;
    function GetDisplayName: UnicodeString;
    function GetFileName: UnicodeString;
    function GetLoadFileName: UnicodeString;
    function GetCS: UnicodeString;
    function GetClip: IXMLClipType;
    procedure SetID(const Value: UnicodeString);
    procedure SetActive(const Value: Integer);
    procedure SetSnappable(const Value: Integer);
    procedure SetPrintable(const Value: Integer);
    procedure SetShowBorders(const Value: Integer);
    procedure SetTransparent(const Value: Integer);
    procedure SetInterpretation(const Value: Integer);
    procedure SetMinimumScale(const Value: Integer);
    procedure SetMaximumScale(const Value: Integer);
    procedure SetColorsRed(const Value: Integer);
    procedure SetColorsGreen(const Value: Integer);
    procedure SetColorsBlue(const Value: Integer);
    procedure SetColorsBrightness(const Value: Integer);
    procedure SetColorsContrast(const Value: Integer);
    procedure SetColorsInversion(const Value: Integer);
    procedure SetColorsGrayscale(const Value: Integer);
    procedure SetColorsHistogram(const Value: Integer);
    procedure SetColorsContrastEnhanced(const Value: Integer);
    procedure SetBandsRed(const Value: Integer);
    procedure SetBandsGreen(const Value: Integer);
    procedure SetBandsBlue(const Value: Integer);
    procedure SetBandsAlpha(const Value: Integer);
    procedure SetBandsPage(const Value: Integer);
    procedure SetKind(const Value: UnicodeString);
    procedure SetName(const Value: UnicodeString);
    procedure SetDisplayName(const Value: UnicodeString);
    procedure SetFileName(const Value: UnicodeString);
    procedure SetLoadFileName(const Value: UnicodeString);
    procedure SetCS(const Value: UnicodeString);
    { Methods & Properties }
    property ID: UnicodeString read GetID write SetID;
    property Active: Integer read GetActive write SetActive;
    property Snappable: Integer read GetSnappable write SetSnappable;
    property Printable: Integer read GetPrintable write SetPrintable;
    property ShowBorders: Integer read GetShowBorders write SetShowBorders;
    property Transparent: Integer read GetTransparent write SetTransparent;
    property Interpretation: Integer read GetInterpretation write SetInterpretation;
    property MinimumScale: Integer read GetMinimumScale write SetMinimumScale;
    property MaximumScale: Integer read GetMaximumScale write SetMaximumScale;
    property ColorsRed: Integer read GetColorsRed write SetColorsRed;
    property ColorsGreen: Integer read GetColorsGreen write SetColorsGreen;
    property ColorsBlue: Integer read GetColorsBlue write SetColorsBlue;
    property ColorsBrightness: Integer read GetColorsBrightness write SetColorsBrightness;
    property ColorsContrast: Integer read GetColorsContrast write SetColorsContrast;
    property ColorsInversion: Integer read GetColorsInversion write SetColorsInversion;
    property ColorsGrayscale: Integer read GetColorsGrayscale write SetColorsGrayscale;
    property ColorsHistogram: Integer read GetColorsHistogram write SetColorsHistogram;
    property ColorsContrastEnhanced: Integer read GetColorsContrastEnhanced write SetColorsContrastEnhanced;
    property BandsRed: Integer read GetBandsRed write SetBandsRed;
    property BandsGreen: Integer read GetBandsGreen write SetBandsGreen;
    property BandsBlue: Integer read GetBandsBlue write SetBandsBlue;
    property BandsAlpha: Integer read GetBandsAlpha write SetBandsAlpha;
    property BandsPage: Integer read GetBandsPage write SetBandsPage;
    property Kind: UnicodeString read GetKind write SetKind;
    property Name: UnicodeString read GetName write SetName;
    property DisplayName: UnicodeString read GetDisplayName write SetDisplayName;
    property FileName: UnicodeString read GetFileName write SetFileName;
    property LoadFileName: UnicodeString read GetLoadFileName write SetLoadFileName;
    property CS: UnicodeString read GetCS write SetCS;
    property Clip: IXMLClipType read GetClip;
  end;

{ IXMLClipType }

  IXMLClipType = interface(IXMLNode)
    ['{D58E9F9D-3D61-4DFC-B8E8-4C9BD47F1F0A}']
    { Property Accessors }
    function GetPolygons: IXMLPolygonsType;
    { Methods & Properties }
    property Polygons: IXMLPolygonsType read GetPolygons;
  end;

{ IXMLPolygonsType }

  IXMLPolygonsType = interface(IXMLNodeCollection)
    ['{99D0797F-6450-47C4-A73B-39C26AF3FBD7}']
    { Property Accessors }
    function GetPolygon(const Index: Integer): IXMLPolygonType;
    { Methods & Properties }
    function Add: IXMLPolygonType;
    function Insert(const Index: Integer): IXMLPolygonType;
    property Polygon[const Index: Integer]: IXMLPolygonType read GetPolygon; default;
  end;

{ IXMLPolygonType }

  IXMLPolygonType = interface(IXMLNode)
    ['{45F2DEDB-FCBF-4DC5-82C4-339CA7DAAA74}']
    { Property Accessors }
    function GetPoints: IXMLPointsType;
    { Methods & Properties }
    property Points: IXMLPointsType read GetPoints;
  end;

{ IXMLPointsType }

  IXMLPointsType = interface(IXMLNodeCollection)
    ['{06B92AA2-948A-4DDF-A6B0-15DB99DFCB5A}']
    { Property Accessors }
    function GetCoor(const Index: Integer): IXMLCoorType;
    { Methods & Properties }
    function Add: IXMLCoorType;
    function Insert(const Index: Integer): IXMLCoorType;
    property Coor[const Index: Integer]: IXMLCoorType read GetCoor; default;
  end;

{ IXMLCoorType }

  IXMLCoorType = interface(IXMLNode)
    ['{CC495195-9F60-4A7C-92AD-8C104E9B3884}']
    { Property Accessors }
    function GetX: Double;
    function GetY: Double;
    function GetZ: Double;
    procedure SetX(const Value: Double);
    procedure SetY(const Value: Double);
    procedure SetZ(const Value: Double);
    { Methods & Properties }
    property X: Double read GetX write SetX;
    property Y: Double read GetY write SetY;
    property Z: Double read GetZ write SetZ;
  end;

{ Forward Decls }

  TXMLReferenceManagerType = class;
  TXMLGroupsType = class;
  TXMLGroupType = class;
  TXMLReferencesType = class;
  TXMLReferenceType = class;
  TXMLClipType = class;
  TXMLPolygonsType = class;
  TXMLPolygonType = class;
  TXMLPointsType = class;
  TXMLCoorType = class;

{ TXMLReferenceManagerType }

  TXMLReferenceManagerType = class(TXMLNode, IXMLReferenceManagerType)
  protected
    { IXMLReferenceManagerType }
    function GetActive: Integer;
    function GetGroups: IXMLGroupsType;
    procedure SetActive(const Value: Integer);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGroupsType }

  TXMLGroupsType = class(TXMLNodeCollection, IXMLGroupsType)
  protected
    { IXMLGroupsType }
    function GetGroup(const Index: Integer): IXMLGroupType;
    function Add: IXMLGroupType;
    function Insert(const Index: Integer): IXMLGroupType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLGroupType }

  TXMLGroupType = class(TXMLNode, IXMLGroupType)
  protected
    { IXMLGroupType }
    function GetID: Integer;
    function GetActive: Integer;
    function GetName: UnicodeString;
    function GetReferences: IXMLReferencesType;
    procedure SetID(const Value: Integer);
    procedure SetActive(const Value: Integer);
    procedure SetName(const Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLReferencesType }

  TXMLReferencesType = class(TXMLNodeCollection, IXMLReferencesType)
  protected
    { IXMLReferencesType }
    function GetReference(const Index: Integer): IXMLReferenceType;
    function Add: IXMLReferenceType;
    function Insert(const Index: Integer): IXMLReferenceType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLReferenceType }

  TXMLReferenceType = class(TXMLNode, IXMLReferenceType)
  protected
    { IXMLReferenceType }
    function GetID: UnicodeString;
    function GetActive: Integer;
    function GetSnappable: Integer;
    function GetPrintable: Integer;
    function GetShowBorders: Integer;
    function GetTransparent: Integer;
    function GetInterpretation: Integer;
    function GetMinimumScale: Integer;
    function GetMaximumScale: Integer;
    function GetColorsRed: Integer;
    function GetColorsGreen: Integer;
    function GetColorsBlue: Integer;
    function GetColorsBrightness: Integer;
    function GetColorsContrast: Integer;
    function GetColorsInversion: Integer;
    function GetColorsGrayscale: Integer;
    function GetColorsHistogram: Integer;
    function GetColorsContrastEnhanced: Integer;
    function GetBandsRed: Integer;
    function GetBandsGreen: Integer;
    function GetBandsBlue: Integer;
    function GetBandsAlpha: Integer;
    function GetBandsPage: Integer;
    function GetKind: UnicodeString;
    function GetName: UnicodeString;
    function GetDisplayName: UnicodeString;
    function GetFileName: UnicodeString;
    function GetLoadFileName: UnicodeString;
    function GetCS: UnicodeString;
    function GetClip: IXMLClipType;
    procedure SetID(const Value: UnicodeString);
    procedure SetActive(const Value: Integer);
    procedure SetSnappable(const Value: Integer);
    procedure SetPrintable(const Value: Integer);
    procedure SetShowBorders(const Value: Integer);
    procedure SetTransparent(const Value: Integer);
    procedure SetInterpretation(const Value: Integer);
    procedure SetMinimumScale(const Value: Integer);
    procedure SetMaximumScale(const Value: Integer);
    procedure SetColorsRed(const Value: Integer);
    procedure SetColorsGreen(const Value: Integer);
    procedure SetColorsBlue(const Value: Integer);
    procedure SetColorsBrightness(const Value: Integer);
    procedure SetColorsContrast(const Value: Integer);
    procedure SetColorsInversion(const Value: Integer);
    procedure SetColorsGrayscale(const Value: Integer);
    procedure SetColorsHistogram(const Value: Integer);
    procedure SetColorsContrastEnhanced(const Value: Integer);
    procedure SetBandsRed(const Value: Integer);
    procedure SetBandsGreen(const Value: Integer);
    procedure SetBandsBlue(const Value: Integer);
    procedure SetBandsAlpha(const Value: Integer);
    procedure SetBandsPage(const Value: Integer);
    procedure SetKind(const Value: UnicodeString);
    procedure SetName(const Value: UnicodeString);
    procedure SetDisplayName(const Value: UnicodeString);
    procedure SetFileName(const Value: UnicodeString);
    procedure SetLoadFileName(const Value: UnicodeString);
    procedure SetCS(const Value: UnicodeString);
  public
    procedure AfterConstruction; override;
  end;

{ TXMLClipType }

  TXMLClipType = class(TXMLNode, IXMLClipType)
  protected
    { IXMLClipType }
    function GetPolygons: IXMLPolygonsType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPolygonsType }

  TXMLPolygonsType = class(TXMLNodeCollection, IXMLPolygonsType)
  protected
    { IXMLPolygonsType }
    function GetPolygon(const Index: Integer): IXMLPolygonType;
    function Add: IXMLPolygonType;
    function Insert(const Index: Integer): IXMLPolygonType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPolygonType }

  TXMLPolygonType = class(TXMLNode, IXMLPolygonType)
  protected
    { IXMLPolygonType }
    function GetPoints: IXMLPointsType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLPointsType }

  TXMLPointsType = class(TXMLNodeCollection, IXMLPointsType)
  protected
    { IXMLPointsType }
    function GetCoor(const Index: Integer): IXMLCoorType;
    function Add: IXMLCoorType;
    function Insert(const Index: Integer): IXMLCoorType;
  public
    procedure AfterConstruction; override;
  end;

{ TXMLCoorType }

  TXMLCoorType = class(TXMLNode, IXMLCoorType)
  protected
    { IXMLCoorType }
    function GetX: Double;
    function GetY: Double;
    function GetZ: Double;
    procedure SetX(const Value: Double);
    procedure SetY(const Value: Double);
    procedure SetZ(const Value: Double);
  end;

{ Global Functions }

function GetReferenceManager(Doc: IXMLDocument): IXMLReferenceManagerType;
function LoadReferenceManager(const FileName: string): IXMLReferenceManagerType;
function NewReferenceManager: IXMLReferenceManagerType;

const
  TargetNamespace = '';

implementation

uses System.Variants, System.SysUtils, Xml.xmlutil;

{ Global Functions }

function GetReferenceManager(Doc: IXMLDocument): IXMLReferenceManagerType;
begin
  Result := Doc.GetDocBinding('ReferenceManager', TXMLReferenceManagerType, TargetNamespace) as IXMLReferenceManagerType;
end;

function LoadReferenceManager(const FileName: string): IXMLReferenceManagerType;
begin
  Result := LoadXMLDocument(FileName).GetDocBinding('ReferenceManager', TXMLReferenceManagerType, TargetNamespace) as IXMLReferenceManagerType;
end;

function NewReferenceManager: IXMLReferenceManagerType;
begin
  Result := NewXMLDocument.GetDocBinding('ReferenceManager', TXMLReferenceManagerType, TargetNamespace) as IXMLReferenceManagerType;
end;

{ TXMLReferenceManagerType }

procedure TXMLReferenceManagerType.AfterConstruction;
begin
  RegisterChildNode('Groups', TXMLGroupsType);
  inherited;
end;

function TXMLReferenceManagerType.GetActive: Integer;
begin
  Result := XmlStrToInt(ChildNodes['Active'].Text);
end;

procedure TXMLReferenceManagerType.SetActive(const Value: Integer);
begin
  ChildNodes['Active'].NodeValue := Value;
end;

function TXMLReferenceManagerType.GetGroups: IXMLGroupsType;
begin
  Result := ChildNodes['Groups'] as IXMLGroupsType;
end;

{ TXMLGroupsType }

procedure TXMLGroupsType.AfterConstruction;
begin
  RegisterChildNode('Group', TXMLGroupType);
  ItemTag := 'Group';
  ItemInterface := IXMLGroupType;
  inherited;
end;

function TXMLGroupsType.GetGroup(const Index: Integer): IXMLGroupType;
begin
  Result := List[Index] as IXMLGroupType;
end;

function TXMLGroupsType.Add: IXMLGroupType;
begin
  Result := AddItem(-1) as IXMLGroupType;
end;

function TXMLGroupsType.Insert(const Index: Integer): IXMLGroupType;
begin
  Result := AddItem(Index) as IXMLGroupType;
end;

{ TXMLGroupType }

procedure TXMLGroupType.AfterConstruction;
begin
  RegisterChildNode('References', TXMLReferencesType);
  inherited;
end;

function TXMLGroupType.GetID: Integer;
begin
  Result := XmlStrToInt(ChildNodes['ID'].Text);
end;

procedure TXMLGroupType.SetID(const Value: Integer);
begin
  ChildNodes['ID'].NodeValue := Value;
end;

function TXMLGroupType.GetActive: Integer;
begin
  Result := XmlStrToInt(ChildNodes['Active'].Text);
end;

procedure TXMLGroupType.SetActive(const Value: Integer);
begin
  ChildNodes['Active'].NodeValue := Value;
end;

function TXMLGroupType.GetName: UnicodeString;
begin
  Result := ChildNodes['Name'].Text;
end;

procedure TXMLGroupType.SetName(const Value: UnicodeString);
begin
  ChildNodes['Name'].NodeValue := Value;
end;

function TXMLGroupType.GetReferences: IXMLReferencesType;
begin
  Result := ChildNodes['References'] as IXMLReferencesType;
end;

{ TXMLReferencesType }

procedure TXMLReferencesType.AfterConstruction;
begin
  RegisterChildNode('Reference', TXMLReferenceType);
  ItemTag := 'Reference';
  ItemInterface := IXMLReferenceType;
  inherited;
end;

function TXMLReferencesType.GetReference(const Index: Integer): IXMLReferenceType;
begin
  Result := List[Index] as IXMLReferenceType;
end;

function TXMLReferencesType.Add: IXMLReferenceType;
begin
  Result := AddItem(-1) as IXMLReferenceType;
end;

function TXMLReferencesType.Insert(const Index: Integer): IXMLReferenceType;
begin
  Result := AddItem(Index) as IXMLReferenceType;
end;

{ TXMLReferenceType }

procedure TXMLReferenceType.AfterConstruction;
begin
  RegisterChildNode('Clip', TXMLClipType);
  inherited;
end;

function TXMLReferenceType.GetID: UnicodeString;
begin
  Result := ChildNodes['ID'].Text;
end;

procedure TXMLReferenceType.SetID(const Value: UnicodeString);
begin
  ChildNodes['ID'].NodeValue := Value;
end;

function TXMLReferenceType.GetActive: Integer;
begin
  Result := XmlStrToInt(ChildNodes['Active'].Text);
end;

procedure TXMLReferenceType.SetActive(const Value: Integer);
begin
  ChildNodes['Active'].NodeValue := Value;
end;

function TXMLReferenceType.GetSnappable: Integer;
begin
  Result := XmlStrToInt(ChildNodes['Snappable'].Text);
end;

procedure TXMLReferenceType.SetSnappable(const Value: Integer);
begin
  ChildNodes['Snappable'].NodeValue := Value;
end;

function TXMLReferenceType.GetPrintable: Integer;
begin
  Result := XmlStrToInt(ChildNodes['Printable'].Text);
end;

procedure TXMLReferenceType.SetPrintable(const Value: Integer);
begin
  ChildNodes['Printable'].NodeValue := Value;
end;

function TXMLReferenceType.GetShowBorders: Integer;
begin
  Result := XmlStrToInt(ChildNodes['ShowBorders'].Text);
end;

procedure TXMLReferenceType.SetShowBorders(const Value: Integer);
begin
  ChildNodes['ShowBorders'].NodeValue := Value;
end;

function TXMLReferenceType.GetTransparent: Integer;
begin
  Result := XmlStrToInt(ChildNodes['Transparent'].Text);
end;

procedure TXMLReferenceType.SetTransparent(const Value: Integer);
begin
  ChildNodes['Transparent'].NodeValue := Value;
end;

function TXMLReferenceType.GetInterpretation: Integer;
begin
  Result := XmlStrToInt(ChildNodes['Interpretation'].Text);
end;

procedure TXMLReferenceType.SetInterpretation(const Value: Integer);
begin
  ChildNodes['Interpretation'].NodeValue := Value;
end;

function TXMLReferenceType.GetMinimumScale: Integer;
begin
  Result := XmlStrToInt(ChildNodes['MinimumScale'].Text);
end;

procedure TXMLReferenceType.SetMinimumScale(const Value: Integer);
begin
  ChildNodes['MinimumScale'].NodeValue := Value;
end;

function TXMLReferenceType.GetMaximumScale: Integer;
begin
  Result := XmlStrToInt(ChildNodes['MaximumScale'].Text);
end;

procedure TXMLReferenceType.SetMaximumScale(const Value: Integer);
begin
  ChildNodes['MaximumScale'].NodeValue := Value;
end;

function TXMLReferenceType.GetColorsRed: Integer;
begin
  Result := XmlStrToInt(ChildNodes['ColorsRed'].Text);
end;

procedure TXMLReferenceType.SetColorsRed(const Value: Integer);
begin
  ChildNodes['ColorsRed'].NodeValue := Value;
end;

function TXMLReferenceType.GetColorsGreen: Integer;
begin
  Result := XmlStrToInt(ChildNodes['ColorsGreen'].Text);
end;

procedure TXMLReferenceType.SetColorsGreen(const Value: Integer);
begin
  ChildNodes['ColorsGreen'].NodeValue := Value;
end;

function TXMLReferenceType.GetColorsBlue: Integer;
begin
  Result := XmlStrToInt(ChildNodes['ColorsBlue'].Text);
end;

procedure TXMLReferenceType.SetColorsBlue(const Value: Integer);
begin
  ChildNodes['ColorsBlue'].NodeValue := Value;
end;

function TXMLReferenceType.GetColorsBrightness: Integer;
begin
  Result := XmlStrToInt(ChildNodes['ColorsBrightness'].Text);
end;

procedure TXMLReferenceType.SetColorsBrightness(const Value: Integer);
begin
  ChildNodes['ColorsBrightness'].NodeValue := Value;
end;

function TXMLReferenceType.GetColorsContrast: Integer;
begin
  Result := XmlStrToInt(ChildNodes['ColorsContrast'].Text);
end;

procedure TXMLReferenceType.SetColorsContrast(const Value: Integer);
begin
  ChildNodes['ColorsContrast'].NodeValue := Value;
end;

function TXMLReferenceType.GetColorsInversion: Integer;
begin
  Result := XmlStrToInt(ChildNodes['ColorsInversion'].Text);
end;

procedure TXMLReferenceType.SetColorsInversion(const Value: Integer);
begin
  ChildNodes['ColorsInversion'].NodeValue := Value;
end;

function TXMLReferenceType.GetColorsGrayscale: Integer;
begin
  Result := XmlStrToInt(ChildNodes['ColorsGrayscale'].Text);
end;

procedure TXMLReferenceType.SetColorsGrayscale(const Value: Integer);
begin
  ChildNodes['ColorsGrayscale'].NodeValue := Value;
end;

function TXMLReferenceType.GetColorsHistogram: Integer;
begin
  Result := XmlStrToInt(ChildNodes['ColorsHistogram'].Text);
end;

procedure TXMLReferenceType.SetColorsHistogram(const Value: Integer);
begin
  ChildNodes['ColorsHistogram'].NodeValue := Value;
end;

function TXMLReferenceType.GetColorsContrastEnhanced: Integer;
begin
  Result := XmlStrToInt(ChildNodes['ColorsContrastEnhanced'].Text);
end;

procedure TXMLReferenceType.SetColorsContrastEnhanced(const Value: Integer);
begin
  ChildNodes['ColorsContrastEnhanced'].NodeValue := Value;
end;

function TXMLReferenceType.GetBandsRed: Integer;
begin
  Result := XmlStrToInt(ChildNodes['BandsRed'].Text);
end;

procedure TXMLReferenceType.SetBandsRed(const Value: Integer);
begin
  ChildNodes['BandsRed'].NodeValue := Value;
end;

function TXMLReferenceType.GetBandsGreen: Integer;
begin
  Result := XmlStrToInt(ChildNodes['BandsGreen'].Text);
end;

procedure TXMLReferenceType.SetBandsGreen(const Value: Integer);
begin
  ChildNodes['BandsGreen'].NodeValue := Value;
end;

function TXMLReferenceType.GetBandsBlue: Integer;
begin
  Result := XmlStrToInt(ChildNodes['BandsBlue'].Text);
end;

procedure TXMLReferenceType.SetBandsBlue(const Value: Integer);
begin
  ChildNodes['BandsBlue'].NodeValue := Value;
end;

function TXMLReferenceType.GetBandsAlpha: Integer;
begin
  Result := XmlStrToInt(ChildNodes['BandsAlpha'].Text);
end;

procedure TXMLReferenceType.SetBandsAlpha(const Value: Integer);
begin
  ChildNodes['BandsAlpha'].NodeValue := Value;
end;

function TXMLReferenceType.GetBandsPage: Integer;
begin
  Result := XmlStrToInt(ChildNodes['BandsPage'].Text);
end;

procedure TXMLReferenceType.SetBandsPage(const Value: Integer);
begin
  ChildNodes['BandsPage'].NodeValue := Value;
end;

function TXMLReferenceType.GetKind: UnicodeString;
begin
  Result := ChildNodes['Kind'].Text;
end;

procedure TXMLReferenceType.SetKind(const Value: UnicodeString);
begin
  ChildNodes['Kind'].NodeValue := Value;
end;

function TXMLReferenceType.GetName: UnicodeString;
begin
  Result := ChildNodes['Name'].Text;
end;

procedure TXMLReferenceType.SetName(const Value: UnicodeString);
begin
  ChildNodes['Name'].NodeValue := Value;
end;

function TXMLReferenceType.GetDisplayName: UnicodeString;
begin
  Result := ChildNodes['DisplayName'].Text;
end;

procedure TXMLReferenceType.SetDisplayName(const Value: UnicodeString);
begin
  ChildNodes['DisplayName'].NodeValue := Value;
end;

function TXMLReferenceType.GetFileName: UnicodeString;
begin
  Result := ChildNodes['FileName'].Text;
end;

procedure TXMLReferenceType.SetFileName(const Value: UnicodeString);
begin
  ChildNodes['FileName'].NodeValue := Value;
end;

function TXMLReferenceType.GetLoadFileName: UnicodeString;
begin
  Result := ChildNodes['LoadFileName'].Text;
end;

procedure TXMLReferenceType.SetLoadFileName(const Value: UnicodeString);
begin
  ChildNodes['LoadFileName'].NodeValue := Value;
end;

function TXMLReferenceType.GetCS: UnicodeString;
begin
  Result := ChildNodes['CS'].Text;
end;

procedure TXMLReferenceType.SetCS(const Value: UnicodeString);
begin
  ChildNodes['CS'].NodeValue := Value;
end;

function TXMLReferenceType.GetClip: IXMLClipType;
begin
  Result := ChildNodes['Clip'] as IXMLClipType;
end;

{ TXMLClipType }

procedure TXMLClipType.AfterConstruction;
begin
  RegisterChildNode('Polygons', TXMLPolygonsType);
  inherited;
end;

function TXMLClipType.GetPolygons: IXMLPolygonsType;
begin
  Result := ChildNodes['Polygons'] as IXMLPolygonsType;
end;

{ TXMLPolygonsType }

procedure TXMLPolygonsType.AfterConstruction;
begin
  RegisterChildNode('Polygon', TXMLPolygonType);
  ItemTag := 'Polygon';
  ItemInterface := IXMLPolygonType;
  inherited;
end;

function TXMLPolygonsType.GetPolygon(const Index: Integer): IXMLPolygonType;
begin
  Result := List[Index] as IXMLPolygonType;
end;

function TXMLPolygonsType.Add: IXMLPolygonType;
begin
  Result := AddItem(-1) as IXMLPolygonType;
end;

function TXMLPolygonsType.Insert(const Index: Integer): IXMLPolygonType;
begin
  Result := AddItem(Index) as IXMLPolygonType;
end;

{ TXMLPolygonType }

procedure TXMLPolygonType.AfterConstruction;
begin
  RegisterChildNode('Points', TXMLPointsType);
  inherited;
end;

function TXMLPolygonType.GetPoints: IXMLPointsType;
begin
  Result := ChildNodes['Points'] as IXMLPointsType;
end;

{ TXMLPointsType }

procedure TXMLPointsType.AfterConstruction;
begin
  RegisterChildNode('Coor', TXMLCoorType);
  ItemTag := 'Coor';
  ItemInterface := IXMLCoorType;
  inherited;
end;

function TXMLPointsType.GetCoor(const Index: Integer): IXMLCoorType;
begin
  Result := List[Index] as IXMLCoorType;
end;

function TXMLPointsType.Add: IXMLCoorType;
begin
  Result := AddItem(-1) as IXMLCoorType;
end;

function TXMLPointsType.Insert(const Index: Integer): IXMLCoorType;
begin
  Result := AddItem(Index) as IXMLCoorType;
end;

{ TXMLCoorType }

function TXMLCoorType.GetX: Double;
begin
  Result := XmlStrToFloatExt(ChildNodes[WideString('X')].Text);
end;

procedure TXMLCoorType.SetX(const Value: Double);
begin
  ChildNodes[WideString('X')].NodeValue := Value;
end;

function TXMLCoorType.GetY: Double;
begin
  Result := XmlStrToFloatExt(ChildNodes[WideString('Y')].Text);
end;

procedure TXMLCoorType.SetY(const Value: Double);
begin
  ChildNodes[WideString('Y')].NodeValue := Value;
end;

function TXMLCoorType.GetZ: Double;
begin
  Result := XmlStrToFloatExt(ChildNodes[WideString('Z')].Text);
end;

procedure TXMLCoorType.SetZ(const Value: Double);
begin
  ChildNodes[WideString('Z')].NodeValue := Value;
end;

end.