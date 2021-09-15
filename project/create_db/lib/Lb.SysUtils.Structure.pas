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
      SysName: String;         // Системноение имя объекта
      Description: String;     // описание объекта
      TypeObject: TTypeObject; // тип объекта
      TimeCreation: TDateTime; // время создание объекта
      TimeUpDate: TDateTime;   // время последнего обновление объекта
      Value: String;           // дополнительный параметр
      Status: Integer;         // статус - объекта
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

    TRecordObjectList = TList<TRecordObject>;
    TAttributeList = TList<TAttribute>;

  private
    FFileName: String;
  protected
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
  protected
    procedure SetSelectedRecordObject(const ARecordObjects: TRecordObjectList);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    function GetCreateGUID: String;
    property FileName: String read FFileName write FFileName;
  public {запись данных}
    function SetRecordObject(const AParentKey: String; const AObject: TCustomObjectModule): String;
    procedure SetDomain(const AParentKey: String; const ADomain: TCrDomain);
    procedure SetTableDomain(const AParentKey: String; const ADomains: TCrDomains);
    procedure SetField(const AParentKey: String; const AField: TCrField);
    procedure SetTableField(const AParentKey: String; const AFields: TCrFields);
    procedure SetIndex(const AParentKey: String; const AIndex: TCrIndex);
    procedure SetTableIndex(const AParentKey: String; const AIndexs: TCrIndexs);
    procedure SetTable(const AParentKey: String; const ATable: TCrTable);
    procedure SetTableTables(const AParentKey: String; const ATables: TCrTables);
    procedure SetMethod(const AParentKey: String; const AMethod: TCrMethod);
    procedure SetTableMethods(const AParentKey: String; const AMethods: TCrMethods);
    procedure SetModule(const AModule: TCrModule);
    procedure SetTableModules(const AModules: TCrModules);
  public
    procedure GetTableModules(const AModules: TCrModules);
  end;

function Structure: TStructure;

implementation

uses
  Data.DB,
  Lb.DataModuleDB,
  Lb.Resource,
  Lb.SysUtils.ISO860;

var
  localDataBase: TDataModuleDB = nil;
  localStructure: TStructure = nil;

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

function Structure: TStructure;
begin
  if not Assigned(localStructure) then
  begin
    localStructure := TStructure.Create;
    localStructure.FileName := ExtractFilePath(ParamStr(0)) + 'structure.sb';
    localStructure.Open;
  end;
  Result := localStructure;
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
  localStructure := nil;
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

  // Если ключ не определен
  if xRecordObject.Key.IsEmpty then
    xRecordObject.Key := Self.GetCreateGUID;

  xTypeObject := GetStrToTypeObject(xRecordObject.TypeObject);
  xTimeCreation := GetDateTimeToStrISO860(xRecordObject.TimeCreation);
  xTimeUpDate := GetDateTimeToStrISO860(xRecordObject.TimeUpDate);

  try
    GetDB.GetExecSQL(xSQL,
      [xRecordObject.Key,
       xRecordObject.ParentKey,
       xRecordObject.Name,
       xRecordObject.SysName,
       xRecordObject.Description,
       xTypeObject,
       xTimeCreation,
       xTimeUpDate,
       xRecordObject.Value],
      [ftString,ftString,ftString,ftString,ftString,ftString,ftString,ftString,ftString]);
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
       xRecordObject.SysName,
       xRecordObject.Description,
       xTypeObject,
       xTimeCreation,
       xTimeUpDate,
       xRecordObject.Value,
       xRecordObject.Key],
      [ftString,ftString,ftString,ftString,ftString,ftString,ftString,ftString,ftString]);
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

// ----------------------------------------------------------------------------

function TStructure.SetRecordObject(const AParentKey: String; const AObject: TCustomObjectModule): String;
var
  xObject: TRecordObject;
begin
  xObject.Key          := AObject.ObjectKey;
  xObject.ParentKey    := AParentKey;
  xObject.Name         := AObject.Name;
  xObject.SysName      := AObject.SysName;
  xObject.Description  := AObject.Description;
  xObject.TypeObject   := AObject.TypeObject;
  xObject.TimeCreation := AObject.TimeСreation;
  xObject.TimeUpDate   := AObject.TimeUpdate;
  xObject.Value        := AObject.Value;
  Result := Self.SetObject(xObject);
end;

// ----------------------------------------------------------------------------

procedure TStructure.SetDomain(const AParentKey: String; const ADomain: TCrDomain);
var
  xParentKey: String;
begin
  xParentKey := Self.SetRecordObject(AParentKey,ADomain);
  {Создание доменного типа данных}
end;

procedure TStructure.SetTableDomain(const AParentKey: String; const ADomains: TCrDomains);
var
  xDomain: TCrDomain;
begin
  for xDomain in ADomains do
    SetDomain(AParentKey,xDomain);
end;

// ----------------------------------------------------------------------------

procedure TStructure.SetField(const AParentKey: String; const AField: TCrField);
var
  xParentKey: String;
begin
  xParentKey := Self.SetRecordObject(AParentKey,AField);
  {Поле объекта}
end;

procedure TStructure.SetTableField(const AParentKey: String; const AFields: TCrFields);
var
  xField: TCrField;
begin
  for xField in AFields do
    SetField(AParentKey,xField);
end;

// ----------------------------------------------------------------------------

procedure TStructure.SetIndex(const AParentKey: String; const AIndex: TCrIndex);
var
  xParentKey: String;
begin
  xParentKey := Self.SetRecordObject(AParentKey,AIndex);
  SetTableField(xParentKey,AIndex.Fields);
end;

procedure TStructure.SetTableIndex(const AParentKey: String; const AIndexs: TCrIndexs);
var
  xIndex: TCrIndex;
begin
  for xIndex in AIndexs do
    SetIndex(AParentKey,xIndex);
end;

// ----------------------------------------------------------------------------

procedure TStructure.SetTable(const AParentKey: String; const ATable: TCrTable);
var
  xParentKey: String;
begin
  xParentKey := Self.SetRecordObject(AParentKey,ATable);
  SetTableField(xParentKey,ATable.Fields);
  SetTableIndex(xParentKey,ATable.Indexs);
end;

procedure TStructure.SetTableTables(const AParentKey: String; const ATables: TCrTables);
var
  xTable: TCrTable;
begin
  for xTable in ATables do
    SetTable(AParentKey,xTable);
end;

// ----------------------------------------------------------------------------

procedure TStructure.SetMethod(const AParentKey: String; const AMethod: TCrMethod);
var
  xParentKey: String;
begin
  xParentKey := Self.SetRecordObject(AParentKey,AMethod);
  SetTableField(xParentKey,AMethod.InputFields);
  SetTableTables(xParentKey,AMethod.InputTables);
  SetTableTables(xParentKey,AMethod.OutputTables);
end;

procedure TStructure.SetTableMethods(const AParentKey: String; const AMethods: TCrMethods);
var
  xMethod: TCrMethod;
begin
  for xMethod in AMethods do
    SetMethod(AParentKey, xMethod);
end;

procedure TStructure.SetModule(const AModule: TCrModule);
var
  xParentKey: String;
begin
  xParentKey := Self.SetRecordObject('null',AModule);
  Self.SetTableDomain(xParentKey, AModule.Domains);
  Self.SetTableMethods(xParentKey, AModule.Methods);
  Self.SetTableTables(xParentKey, AModule.Tables);
end;

procedure TStructure.SetTableModules(const AModules: TCrModules);
var
  xModule: TCrModule;
begin
  for xModule in AModules do
    SetModule(xModule);
end;

// ----------------------------------------------------------------------------

procedure TStructure.SetSelectedRecordObject(const ARecordObjects: TRecordObjectList);
var
  xSQL: String;
  xDataSet: TDataSet;
  xRecordObject: TRecordObject;
var
  xTypeObject, xTimeCreation, xTimeUpDate: String;
begin
  xSQL := GetResourceText('select_object');
  xDataSet := GetDB.GetSelectCreateDataSet(xSQL);
  if Assigned(xDataSet) then
  begin
    xDataSet.First;
    while not xDataSet.Eof do
    begin
      with xRecordObject, xDataSet do
      begin
        Key         := FieldByName('key').AsString;         // ключ объект
        ParentKey   := FieldByName('parent_key').AsString;  // ключ принадлежащий объектов
        xRecordObject.Name := FieldByName('name').AsString; // наименование объекта
        SysName     := FieldByName('sys_name').AsString;    //
        Description := FieldByName('description').AsString; // описание объекта
        // ------------------------------------------------
        xTypeObject := FieldByName('type_object').AsString; // тип объекта
        TypeObject  := GetTypeObjectToStr(xTypeObject);     // тип объекта
        // -----------------------------------------------
        xTimeCreation := FieldByName('time_creation').AsString;
        TimeCreation  := GetStrISO860ToDateTime(xTimeCreation);
        // -----------------------------------------------
        xTimeUpDate := FieldByName('time_update').AsString;
        TimeUpDate  := GetStrISO860ToDateTime(xTimeUpDate);  // время последнего обновление объекта
        // -----------------------------------------------
        Value  := FieldByName('value').AsString;             // дополнительный параметр
        Status := FieldByName('status').AsInteger;           // статус - объекта
      end;
      ARecordObjects.Add(xRecordObject);
      xDataSet.Next;
    end;
    FreeAndNil(xDataSet);
  end;
end;

procedure TStructure.GetTableModules(const AModules: TCrModules);
var
  xRecordObject: TRecordObject;
  xRecordObjects: TRecordObjectList;
begin
  if not Assigned(AModules) then
    raise Exception.Create('Error Message: Не определён, объект Modules');
  AModules.Clear;
  xRecordObjects := TRecordObjectList.Create;
  try
    SetSelectedRecordObject(xRecordObjects);
    for xRecordObject in xRecordObjects do
    begin
      var xCrModule := TCrModule.Create(xRecordObject.Key);
      with xCrModule do
      begin
        ParentKey    := xRecordObject.ParentKey;
        Name         := xRecordObject.Name;
        SysName      := xRecordObject.SysName;
        Description  := xRecordObject.Description;
        TypeObject   := xRecordObject.TypeObject;
        TimeСreation := xRecordObject.TimeCreation;
        TimeUpdate   := xRecordObject.TimeUpDate;
        Value        := xRecordObject.Value;
      end;
      AModules.Add(xCrModule);
    end;
  finally
    FreeAndNil(xRecordObjects);
  end;
end;


initialization
  Structure;

finalization
  SetFinalization;

end.
