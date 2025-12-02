unit Lider.CG.Com.LayerAttributes;

{$I Lider.CG.Com.Component.inc}

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Variants,
  Lider.CG.Com.GIS,
  Lider.CG.Com.Restructure;

type
  TLayerAttributeDataType = (ladText, ladInteger, ladFloat, ladDate, ladBoolean, ladUnknown);

  TLayerAttributeField = class
  public
    Name: string;
    Caption: string;
    DataType: TLayerAttributeDataType;
    DefaultValue: Variant;
    IsSystemField: Boolean;
  end;

  TLayerAttributeSchema = class
  private
    FFields: TObjectList<TLayerAttributeField>;
    function GetFieldByName(const AName: string): TLayerAttributeField;
    procedure EnsurePhysicalField(const ALayer: TlicgBaseLayer;
      const AField: TLayerAttributeField);
    procedure ApplyDefaultValues(const ALayer: TlicgBaseLayer;
      const AField: TLayerAttributeField);
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromLayer(const ALayer: TlicgBaseLayer);
    function AddUserField(const ALayer: TlicgBaseLayer; const AName, ACaption: string;
      ADataType: TLayerAttributeDataType; const ADefault: Variant): TLayerAttributeField;
    procedure RemoveUserField(const ALayer: TlicgBaseLayer; const AName: string);
    property Fields: TObjectList<TLayerAttributeField> read FFields;
    property FieldByName[const AName: string]: TLayerAttributeField read GetFieldByName;
  end;

  TFeatureAttributes = class
  private
    FValues: TDictionary<string, Variant>;
  public
    constructor Create;
    destructor Destroy; override;
    procedure LoadFromLayerRecord(const ALayer: TlicgBaseLayer; const ARecNo: Integer;
      const ASchema: TLayerAttributeSchema);
    procedure SaveToLayerRecord(const ALayer: TlicgBaseLayer; const ARecNo: Integer);
    property Values: TDictionary<string, Variant> read FValues;
  end;

implementation


{ Helpers }

function FieldTypeToDataType(const ACharType: AnsiChar): TLayerAttributeDataType;
var
  C: AnsiChar;
begin
  C := UpCase(ACharType);
  case C of
    'N': Result := ladFloat;
    'I': Result := ladInteger;
    'D': Result := ladDate;
    'L': Result := ladBoolean;
    'C', 'M': Result := ladText;
  else
    Result := ladUnknown;
  end;
end;

function DataTypeToFieldDef(const AField: TLayerAttributeField): string;
begin
  case AField.DataType of
    ladInteger:
      Result := Format('%s;N;18;0', [AField.Name]);
    ladFloat:
      Result := Format('%s;N;20;4', [AField.Name]);
    ladDate:
      Result := Format('%s;D;8;0', [AField.Name]);
    ladBoolean:
      Result := Format('%s;L;1;0', [AField.Name]);
    ladText:
      Result := Format('%s;C;254;0', [AField.Name]);
  else
    Result := Format('%s;C;254;0', [AField.Name]);
  end;
end;

{ TLayerAttributeSchema }

constructor TLayerAttributeSchema.Create;
begin
  inherited Create;
  FFields := TObjectList<TLayerAttributeField>.Create(True);
end;

destructor TLayerAttributeSchema.Destroy;
begin
  FFields.Free;
  inherited Destroy;
end;

function TLayerAttributeSchema.GetFieldByName(const AName: string): TLayerAttributeField;
var
  Field: TLayerAttributeField;
begin
  for Field in FFields do
    if SameText(Field.Name, AName) then
      Exit(Field);
  Result := nil;
end;

 procedure TLayerAttributeSchema.EnsurePhysicalField(const ALayer: TlicgBaseLayer;
  const AField: TLayerAttributeField);
var
  Definitions: TStringList;
begin
  if (ALayer = nil) or (AField = nil) then
    Exit;

  Definitions := TStringList.Create;
  try
    Definitions.Add(DataTypeToFieldDef(AField));
    AddDBFColumn(ALayer, Definitions, False);
  finally
    Definitions.Free;
  end;
end;

procedure TLayerAttributeSchema.ApplyDefaultValues(const ALayer: TlicgBaseLayer;
  const AField: TLayerAttributeField);
begin
  if (ALayer = nil) or (ALayer.DBTable = nil) or (AField = nil) then
    Exit;

  ALayer.First;
  ALayer.StartBuffering;
  try
    while not ALayer.Eof do
    begin
      if VarIsNull(ALayer.DBTable.FieldGet(AField.Name)) or
        (Trim(ALayer.DBTable.FieldGet(AField.Name)) = '') then
      begin
        if not VarIsEmpty(AField.DefaultValue) then
          ALayer.DBTable.FieldPut(AField.Name, AField.DefaultValue);
      end;
      ALayer.Next;
    end;
  finally
    ALayer.EndBuffering;
  end;
end;

procedure TLayerAttributeSchema.LoadFromLayer(const ALayer: TlicgBaseLayer);
var
  I: Integer;
  Field: TLayerAttributeField;
  FieldName: string;
  FieldNo: Integer;
  FieldType: AnsiChar;
  Names: TStringList;
begin
  FFields.Clear;
  if (ALayer = nil) or (ALayer.DBTable = nil) then
    Exit;

  Names := TStringList.Create;
  try
    ALayer.GetFieldList(Names);
    for I := 0 to Names.Count - 1 do
    begin
      FieldName := Names[I];
      FieldNo := ALayer.DBTable.FieldNo(FieldName);
      // tablo alan listesi ile DB alanlar birebir olmad ise (silinmis/eklenmemis)
      // hataya dnmeyelim; geerli alanlar ile devam edelim
      if FieldNo <= 0 then
        Continue;

      Field := TLayerAttributeField.Create;
      FieldType := ALayer.DBTable.FieldType(FieldNo);
      Field.Name := FieldName;
      Field.Caption := FieldName;
      Field.DataType := FieldTypeToDataType(FieldType);
      Field.DefaultValue := Null;
      Field.IsSystemField := SameText(FieldName, 'UID') or SameText(FieldName, 'TYPE')
        or SameText(FieldName, 'ENT') or SameText(FieldName, 'ID');
      FFields.Add(Field);
    end;
  finally
    Names.Free;
  end;
end;

function TLayerAttributeSchema.AddUserField(const ALayer: TlicgBaseLayer; const AName,
  ACaption: string; ADataType: TLayerAttributeDataType; const ADefault: Variant): TLayerAttributeField;
begin
  Result := nil;
  if (ALayer = nil) or (ALayer.DBTable = nil) then
    Exit;

  Result := TLayerAttributeField.Create;
  Result.Name := AName;
  Result.Caption := ACaption;
  Result.DataType := ADataType;
  Result.DefaultValue := ADefault;
  Result.IsSystemField := False;
  FFields.Add(Result);
  // TODO: hook into concrete table implementations when runtime restructuring is needed
  EnsurePhysicalField(ALayer, Result);
  ApplyDefaultValues(ALayer, Result);
  LoadFromLayer(ALayer);
  end;

procedure TLayerAttributeSchema.RemoveUserField(const ALayer: TlicgBaseLayer; const AName: string);
var
  Field: TLayerAttributeField;
begin
  Field := GetFieldByName(AName);
  if Field <> nil then
    FFields.Remove(Field);
  // TODO: drop field from persistent table when the backend supports restructuring
  RemoveDBFColumn(ALayer, AName, False);
  LoadFromLayer(ALayer);
  end;

{ TFeatureAttributes }

constructor TFeatureAttributes.Create;
begin
  inherited Create;
  FValues := TDictionary<string, Variant>.Create;
end;

destructor TFeatureAttributes.Destroy;
begin
  FValues.Free;
  inherited Destroy;
end;

procedure TFeatureAttributes.LoadFromLayerRecord(const ALayer: TlicgBaseLayer; const ARecNo: Integer;
  const ASchema: TLayerAttributeSchema);
var
  Field: TLayerAttributeField;
begin
  FValues.Clear;
  if (ALayer = nil) or (ALayer.DBTable = nil) or (ASchema = nil) then
    Exit;

  ALayer.DBTable.RecNo := ARecNo;
  for Field in ASchema.Fields do
    FValues.AddOrSetValue(Field.Name, ALayer.DBTable.FieldGet(Field.Name));
end;

procedure TFeatureAttributes.SaveToLayerRecord(const ALayer: TlicgBaseLayer; const ARecNo: Integer);
var
  Pair: TPair<string, Variant>;
begin
  if (ALayer = nil) or (ALayer.DBTable = nil) then
    Exit;

  ALayer.DBTable.RecNo := ARecNo;
  for Pair in FValues do
    ALayer.DBTable.FieldPut(Pair.Key, VarToStr(Pair.Value));
  ALayer.DBTable.Post;
end;

end.
