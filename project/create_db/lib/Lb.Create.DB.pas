
unit Lb.Create.DB;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

type
  ///<summary>Тип объекта</summary>
  TTypeObject = (toNull, toDomain, toField, toIndex, toTable, toMethod, toModule);

  ///<summary>Базовый класс</summary>
  TCustomObjectModule = class(TObject)
  private
    FObjectKey: String;
    FParentKey: String;
    FName: String;
    FSysName: String;
    FDescription: String;
    FTypeObject: TTypeObject;
    FTimeСreation: TDateTime;
    FTimeUpdate: TDateTime;
    FValue: String;
  protected
    procedure SetTypeObject; virtual;
  public
    constructor Create(const AObjectKey: String = ''); virtual;
    destructor Destroy; override;
    procedure Assign(const AModule: TCustomObjectModule); virtual;
    property ObjectKey: String read FObjectKey write FObjectKey;
    property ParentKey: String read FParentKey write FParentKey;
    property Name: String read FName write FName;
    property SysName: String read FSysName write FSysName;
    property Description: String read FDescription write FDescription;
    property TypeObject: TTypeObject read FTypeObject write FTypeObject;
    property TimeСreation: TDateTime read FTimeСreation write FTimeСreation;
    property TimeUpdate: TDateTime read FTimeUpdate write FTimeUpdate;
    property Value: String read FValue write FValue;
  public
    function ToCaption: String;
  end;

  // **************************************************************************
  ///<summary>Описываем пользовательский тип данных</summary>
  TCrDomain = class(TCustomObjectModule)
  private
  protected
    procedure SetTypeObject; override;
  public
    constructor Create(const AObjectKey: String = ''); override;
    destructor Destroy; override;
  end;

  ///<summary>Список пользовательский типов</summary>
  TCrDomains = class(TObjectList<TCrDomain>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  // **************************************************************************
  // Определяем поля
  ///<summary>Поле – таблицы</summary>
  TCrField = class(TCustomObjectModule)
  private
    FTypeField: String;
  protected
    procedure SetTypeObject; override;
  public
    constructor Create(const AObjectKey: String = ''); override;
    destructor Destroy; override;
    ///<summary>Тип полей</summary>
    property TypeField: String read FTypeField write FTypeField;
  end;

  ///<summary>Список полей</summary>
  TCrFields = class(TObjectList<TCrField>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  // **************************************************************************
  // Индекс таблицы

  ///<summary>Индекс таблицы<summary>
  TCrIndex = class(TCustomObjectModule)
  private
    FFields: TCrFields;
  protected
    procedure SetTypeObject; override;
  public
    constructor Create(const AObjectKey: String = ''); override;
    destructor Destroy; override;
    property Fields: TCrFields read FFields;
  end;

  ///<summary>Список индексов</summary>
  TCrIndexs = class(TObjectList<TCrIndex>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  // **************************************************************************
  // Таблицы

  ///<summary>Таблица</summary>
  TCrTable = class(TCustomObjectModule)
  private
    FFields: TCrFields;
    FIndexs: TCrIndexs;
  protected
    procedure SetTypeObject; override;
  public
    constructor Create(const AObjectKey: String = ''); override;
    destructor Destroy; override;
    property Fields: TCrFields read FFields;
    property Indexs: TCrIndexs read FIndexs;
  end;

  ///<summary>Список таблицы</summary>
  TCrTables = class(TObjectList<TCrTable>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  // **************************************************************************
  // Метод

  ///<summary>Описывает хранимую процедуру или скрипт</summary>
  TCrMethod = class(TCustomObjectModule)
  private
    FResultType: String;
    FInputFields: TCrFields;
    FInputTables: TCrTables;
    FOutputTables: TCrTables;
  protected
    procedure SetTypeObject; override;
  public
    constructor Create(const AObjectKey: String = ''); override;
    destructor Destroy; override;
    property ResultType: String read FResultType;
    property InputFields: TCrFields read FInputFields;
    property InputTables: TCrTables read  FInputTables;
    property OutputTables: TCrTables read FOutputTables;
  end;

  ///<summary>Список методов</summary>
  TCrMethods = class(TObjectList<TCrMethod>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  // **************************************************************************
  // Модуль

  ///<summary>Набор методов и таблиц</summary>
  TCrModule = class(TCustomObjectModule)
  private
    FDomains: TCrDomains;
    FMethods: TCrMethods;
    FTables: TCrTables;
  protected
    procedure SetTypeObject; override;
  public
    constructor Create(const AObjectKey: String = ''); override;
    destructor Destroy; override;
    property Methods: TCrMethods read FMethods;
    property Tables: TCrTables read FTables;
    property Domains: TCrDomains read FDomains;
  end;

  ///<summary>Список модулей</summary>
  TCrModules = class(TObjectList<TCrModule>)
  public
    constructor Create;
    destructor Destroy; override;
    function GetCreateModule: TCrModule;
  end;

  // **************************************************************************
  // Системные функции
  TSysConfig = record
    class procedure SetTypeFields(const AFieldTypes: TStrings); static;
    class function GetIndexOfTypeField(const AFieldType: String): Integer; static;
  end;

function GetStrToTypeObject(const ATypeObject: TTypeObject): String; inline;
function GetTypeObjectToStr(const AValue: String): TTypeObject; inline;

function CrModules: TCrModules;

implementation

uses
  Lb.SysUtils;

var
  localModules: TCrModules = nil;

function CrModules: TCrModules;
begin
  if not Assigned(localModules) then
  begin
    localModules := TCrModules.Create;
  end;
  Result := localModules;
end;

procedure SetFinalizationModules;
begin
  if Assigned(localModules) then
    FreeAndNil(localModules);
  localModules := nil;
end;

function GetStrToTypeObject(const ATypeObject: TTypeObject): String;
begin
  case ATypeObject of
    toNull: Result := 'null';
    toDomain: Result := 'domain';
    toField: Result := 'field';
    toIndex: Result := 'index';
    toTable: Result := 'table';
    toMethod: Result := 'method';
    toModule: Result := 'module';
  end;
end;

function GetTypeObjectToStr(const AValue: String): TTypeObject;
var
  xS: String;
begin
  xS := AnsiLowerCase(AValue);
  if SameText(xS,'domain') then
    Result := TTypeObject.toDomain
  else if SameText(xS,'field') then
    Result := TTypeObject.toField
  else if SameText(xS,'index') then
    Result := TTypeObject.toIndex
  else if SameText(xS,'table') then
    Result := TTypeObject.toTable
  else if SameText(xS,'method') then
    Result := TTypeObject.toMethod
  else if SameText(xS,'module') then
    Result := TTypeObject.toModule
  else
    Result := TTypeObject.toNull;
end;


{ TCustomObjectModule }

constructor TCustomObjectModule.Create(const AObjectKey: String = '');
begin
  Self.SetTypeObject;
  FName := '';
  FDescription := '';
  if AObjectKey.IsEmpty then
    FObjectKey := GetCreateObjectKey
  else
    FObjectKey := AObjectKey;
end;

destructor TCustomObjectModule.Destroy;
begin

  inherited;
end;

procedure TCustomObjectModule.SetTypeObject;
begin
  FTypeObject := TTypeObject.toNull;
end;

function TCustomObjectModule.ToCaption: String;
begin
  case FTypeObject of
    toDomain: Result := 'Domain';
    toField: Result := 'Поле';
    toIndex: Result := 'Индекс';
    toTable: Result := 'Таблица';
    toMethod: Result := 'Метод';
    toModule: Result := 'Модуль';
  else
    Result := 'Null'
  end;
end;

procedure TCustomObjectModule.Assign(const AModule: TCustomObjectModule);
begin
  if Assigned(AModule) then
  begin
    Self.ObjectKey := AModule.ObjectKey;
    Self.Name := AModule.Name;
    Self.SysName := AModule.SysName;
    Self.Description := AModule.Description;
    Self.TypeObject := AModule.TypeObject;
    Self.TimeСreation := AModule.TimeСreation;
    Self.TimeUpdate := AModule.TimeUpdate;
    Self.Value := AModule.Value;
  end;
end;

{ TCrDomain }

constructor TCrDomain.Create(const AObjectKey: String = '');
begin
  inherited Create(AObjectKey);

end;

destructor TCrDomain.Destroy;
begin

  inherited Destroy;
end;

procedure TCrDomain.SetTypeObject;
begin
  FTypeObject := TTypeObject.toDomain;
end;

{ TCrDomains }

constructor TCrDomains.Create;
begin
  inherited Create;

end;

destructor TCrDomains.Destroy;
begin

  inherited Destroy;
end;


{ TCrField }

constructor TCrField.Create(const AObjectKey: String = '');
begin
  inherited Create(AObjectKey);
  FTypeField := '';
end;

destructor TCrField.Destroy;
begin

  inherited Destroy;
end;

procedure TCrField.SetTypeObject;
begin
  FTypeObject := TTypeObject.toField;
end;

{ TCrFields }

constructor TCrFields.Create;
begin
  inherited Create;

end;

destructor TCrFields.Destroy;
begin

  inherited Destroy;
end;

{ TCrIndex }

constructor TCrIndex.Create(const AObjectKey: String = '');
begin
  inherited;

end;

destructor TCrIndex.Destroy;
begin

  inherited;
end;

procedure TCrIndex.SetTypeObject;
begin
  FTypeObject := toIndex;
end;

{ TCrIndexs }

constructor TCrIndexs.Create;
begin
  inherited Create;

end;

destructor TCrIndexs.Destroy;
begin

  inherited Destroy;
end;


{ TCrTable }

constructor TCrTable.Create(const AObjectKey: String = '');
begin
  inherited Create(AObjectKey);
  FFields := TCrFields.Create;
  FIndexs := TCrIndexs.Create;
end;

destructor TCrTable.Destroy;
begin
  FreeAndNil(FIndexs);
  FreeAndNil(FFields);
  inherited;
end;

procedure TCrTable.SetTypeObject;
begin
  FTypeObject := toTable;
end;

{ TCrTables }

constructor TCrTables.Create;
begin
  inherited Create;

end;

destructor TCrTables.Destroy;
begin

  inherited;
end;

{ TCrMethod }

constructor TCrMethod.Create(const AObjectKey: String = '');
begin
  inherited Create(AObjectKey);
  FInputFields := TCrFields.Create;
  FInputTables := TCrTables.Create;
  FOutputTables := TCrTables.Create;
end;

destructor TCrMethod.Destroy;
begin
  FreeAndNil(FInputFields);
  FreeAndNil(FOutputTables);
  FreeAndNil(FInputTables);
  inherited Destroy;
end;

procedure TCrMethod.SetTypeObject;
begin
  FTypeObject := toMethod;
end;

{ TCrMethods }

constructor TCrMethods.Create;
begin
  inherited Create;

end;

destructor TCrMethods.Destroy;
begin

  inherited Destroy;
end;

{ TCrModule }

constructor TCrModule.Create(const AObjectKey: String = '');
begin
  inherited Create(AObjectKey);
  FMethods := TCrMethods.Create;
  FTables := TCrTables.Create;
  FDomains := TCrDomains.Create;
end;

destructor TCrModule.Destroy;
begin
  FreeAndNil(FDomains);
  FreeAndNil(FTables);
  FreeAndNil(FMethods);
  inherited Destroy;
end;

procedure TCrModule.SetTypeObject;
begin
  FTypeObject := toModule;
end;

{ TCrModules }

constructor TCrModules.Create;
begin
  inherited Create;

end;

destructor TCrModules.Destroy;
begin

  inherited;
end;

function TCrModules.GetCreateModule: TCrModule;
var
  xModule: TCrModule;
begin
  xModule := CrModules.GetCreateModule;
  xModule.ObjectKey := GetCreateObjectKey;
  Self.Add(xModule);
  Result := xModule;
end;

{ TSysConfig }

class procedure TSysConfig.SetTypeFields(const AFieldTypes: TStrings);
begin
  if Assigned(AFieldTypes) then
  begin
    {todo: загрузить откуда}


  end;
end;

class function TSysConfig.GetIndexOfTypeField(const AFieldType: String): Integer;
begin
  Result := 0;
end;

initialization
  CrModules;

finalization
  SetFinalizationModules;


end.
