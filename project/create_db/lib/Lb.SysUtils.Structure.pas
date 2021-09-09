unit Lb.SysUtils.Structure;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Lb.Create.DB;

type
  TStructure = class(TObject)
  public type
    TRecordObject = record
      Key: String;             // ключ объект
      ParentKey: String;       // ключ принадлежащий объектов
      Name: String;            // наименование объекта
      Description: String;     // описание объекта
      TypeObject: TTypeObject; // тип объекта
      TimeCreation: TDateTime; // время создание объекта
      TimeUpDate: TDateTime;   // время последнего обновление объекта
      Value: String;           // дополнительный параметр
    end;

    TAttribute = record
      ObjectKey: String;       // ключ объекта
      Name: String;            // наименование атрибута
      Value: String;           // значение атрибута
    private
      function GetAsString: String;
      function GetAsInteger: Integer;
      function GetAsDouble: Double;
      function GetAsDateTime: TDateTime;
      procedure SetAsString(const Value: String);
      procedure SetAsInteger(const Value: Integer);
      procedure SetAsDouble(const Value: Double);
      procedure SetAsDateTime(const Value: TDateTime);
    public
      property AsString: String read GetAsString write SetAsString;
      property AsInteger: Integer read GetAsInteger write SetAsInteger;
      property AsDouble: Double read GetAsDouble write SetAsDouble;
      property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    end;

  private
    FFileName: String;
  protected
    function GetCreateGUID: String;
    procedure SetCreateDataBase;
  protected
    function GetInsertObject(const ARecordObject: TRecordObject): String;
    function GetUpDateObject(const ARecordObject: TRecordObject): String;
    function GetDeleteObject(const AKey: String): Boolean;
    function GetIsObject(const AKey: String): Boolean;
    function GetInsertAttribute(const Attribute: TAttribute): Boolean;
    function GetUpDateAttribute(const Attribute: TAttribute): Boolean;
    function GetDeleteAttribute(const AObjectKey, AName: String): Boolean;
    function GetIsAttribute(const AObjectKey, AName: String): Boolean;
  protected
    function SetObject(const ARecordObject: TRecordObject): String;
    function SetAttribute(const Attribute: TAttribute): Boolean;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    property FileName: String read FFileName write FFileName;
  end;

implementation

uses
  Data.DB,
  Lb.DataModuleDB,
  Lb.Resource, Lb.SysUtils.ISO860;

var
  localDataBase: TDataModuleDB = nil;

function GetDB: TDataModuleDB;
begin
  if not Assigned(localDataBase) then
    localDataBase := TDataModuleDB.Create(nil);
  Result := localDataBase;
end;

procedure SetFinalization;
begin
  if Assigned(localDataBase) then
    FreeAndNil(localDataBase);
  localDataBase := nil;
end;

function GetIsConnect: Boolean;
begin
  Result := False;
  if Assigned(localDataBase) then
    Result := localDataBase.Connected;
end;

{ TStructure.TAttribute }

function TStructure.TAttribute.GetAsString: String;
begin
  Result := Self.Value;
end;

procedure TStructure.TAttribute.SetAsString(const Value: String);
begin
  Self.Value := Value;
end;

function TStructure.TAttribute.GetAsInteger: Integer;
begin
  Result := StrToIntDef(Self.Value,0);
end;

procedure TStructure.TAttribute.SetAsInteger(const Value: Integer);
begin
  Self.Value := IntToStr(Value);
end;

function TStructure.TAttribute.GetAsDouble: Double;
begin
  Result := StrToFloatDef(Self.Value,0);
end;

procedure TStructure.TAttribute.SetAsDouble(const Value: Double);
begin
  Self.Value := FloatToStr(Value);
end;

function TStructure.TAttribute.GetAsDateTime: TDateTime;
begin
  Result := StrToDateTimeDef(Self.Value,0);
end;

procedure TStructure.TAttribute.SetAsDateTime(const Value: TDateTime);
begin
  Self.Value := DateTimeToStr(Value);
end;

{ TStructure }

constructor TStructure.Create;
begin
  FFileName := '';
end;

destructor TStructure.Destroy;
begin
  SetFinalization;
  inherited;
end;

function TStructure.GetCreateGUID: String;
var
  xGUID : TGUID;
begin
  CreateGUID(xGUID);
  Result := GUIDToString(xGUID)
end;

procedure TStructure.Open;
begin
  if not GetIsConnect then
    if not FFileName.IsEmpty then
    begin
      GetDB.DefaultConnection(FFileName);
      SetCreateDataBase;
    end;
end;

procedure TStructure.SetCreateDataBase;
var
  xScript: TStrings;
begin
  xScript :=  TStringList.Create;
  try
    SetResourceParams('data_base',xScript);
    GetDB.GetExecSQL(xScript.Text);
  finally
    FreeAndNil(xScript);
  end;
end;

function TStructure.SetObject(const ARecordObject: TRecordObject): String;
begin
  if Self.GetIsObject(ARecordObject.Key) then
    Result := Self.GetUpDateObject(ARecordObject)
  else
    Result := Self.GetInsertObject(ARecordObject);
end;

procedure TStructure.Close;
begin
  if GetIsConnect then
    SetFinalization;
end;

function TStructure.GetInsertObject(const ARecordObject: TRecordObject): String;
var
  xSQL: String;
  xRecordObject: TRecordObject;
var
  xTypeObject, xTimeCreation, xTimeUpDate: String;
begin
  xRecordObject := ARecordObject;
  xSQL := GetResourceText('insert_object');

  xRecordObject.Key := Self.GetCreateGUID;
  xTypeObject := GetStrToTypeObject(xRecordObject.TypeObject);
  xTimeCreation := GetDateTimeToStrISO860(xRecordObject.TimeCreation);
  xTimeUpDate := GetDateTimeToStrISO860(xRecordObject.TimeUpDate);

  try
    GetDB.GetExecSQL(xSQL,
      [xRecordObject.Key,
       xRecordObject.ParentKey,
       xRecordObject.Name,
       xRecordObject.Description,
       xTypeObject,
       xTimeCreation,
       xTimeUpDate,
       xRecordObject.Value],
      [ftString,ftString,ftString,ftString,ftString,ftString,ftString,ftString]);
    Result := xRecordObject.Key;
  except
    Result := '';
  end;
end;

function TStructure.GetUpDateObject(const ARecordObject: TRecordObject): String;
var
  xSQL: String;
  xRecordObject: TRecordObject;
var
  xTypeObject, xTimeCreation, xTimeUpDate: String;
begin
  xRecordObject := ARecordObject;
  xSQL := GetResourceText('update_object');

  xTypeObject := GetStrToTypeObject(xRecordObject.TypeObject);
  xTimeCreation := GetDateTimeToStrISO860(xRecordObject.TimeCreation);
  xTimeUpDate := GetDateTimeToStrISO860(xRecordObject.TimeUpDate);

  try
    GetDB.GetExecSQL(xSQL,
      [xRecordObject.ParentKey,
       xRecordObject.Name,
       xRecordObject.Description,
       xTypeObject,
       xTimeCreation,
       xTimeUpDate,
       xRecordObject.Value,
       xRecordObject.Key],
      [ftString,ftString,ftString,ftString,ftString,ftString,ftString,ftString]);
    Result := xRecordObject.Key;
  except
    Result := '';
  end;
end;

function TStructure.GetDeleteObject(const AKey: String): Boolean;
var
  xSQL: String;
begin
  xSQL := GetResourceText('delete_status_object');
  try
    GetDB.GetExecSQL(xSQL,[AKey],[ftString]);
    Result := True;
  except
    Result := False;
  end;
end;

function TStructure.GetInsertAttribute(const Attribute: TAttribute): Boolean;
var
  xSQL: String;
begin
  xSQL := GetResourceText('insert_attributes');
  try
    GetDB.GetExecSQL(xSQL,
      [Attribute.ObjectKey,Attribute.Name,Attribute.Value],
      [ftString,ftString,ftString]);
    Result := True;
  except
    Result := False;
  end;
end;

function TStructure.GetUpDateAttribute(const Attribute: TAttribute): Boolean;
var
  xSQL: String;
begin
  xSQL := GetResourceText('update_attributes');
  try
    GetDB.GetExecSQL(xSQL,
      [Attribute.Value,Attribute.ObjectKey,Attribute.Name],
      [ftString,ftString,ftString]);
    Result := True;
  except
    Result := False;
  end;
end;

function TStructure.GetDeleteAttribute(const AObjectKey, AName: String): Boolean;
var
  xSQL: String;
begin
  xSQL := GetResourceText('delete_real_attributes');
  try
    GetDB.GetExecSQL(xSQL,[AObjectKey,AName],[ftString,ftString]);
    Result := True;
  except
    Result := False;
  end;
end;

function TStructure.GetIsAttribute(const AObjectKey, AName: String): Boolean;
var
  xSQL: String;
  xValue: Variant;
begin
  xSQL := GetResourceText('is_attribut');
  try
    xValue := GetDB.GetExecSQLScalar(xSQL,[AObjectKey,AName],[ftString,ftString]);
    if VarIsNull(xValue) then
      Result := False
    else
      Result := xValue > 0
  except
    Result := False;
  end;
end;

function TStructure.GetIsObject(const AKey: String): Boolean;
var
  xSQL: String;
  xValue: Variant;
begin
  xSQL := GetResourceText('is_object');
  try
    xValue := GetDB.GetExecSQLScalar(xSQL,[AKey],[ftString]);
    if VarIsNull(xValue) then
      Result := False
    else
      Result := xValue > 0
  except
    Result := False;
  end;
end;

function TStructure.SetAttribute(const Attribute: TAttribute): Boolean;
begin
  if Self.GetIsAttribute(Attribute.ObjectKey,Attribute.Name) then
    Result := Self.GetUpDateAttribute(Attribute)
  else
    Result := Self.GetInsertAttribute(Attribute);
end;

initialization

finalization
  SetFinalization;

end.
