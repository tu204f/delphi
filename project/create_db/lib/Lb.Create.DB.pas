
unit Lb.Create.DB;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

type
  ///<summary>Тип объекта</summary>
  TTypeObject = (toNull, toField, toIndex, toTable, toMethod, toModule);

  ///<summary>Базовый класс</summary>
  TCustomObjectModule = class(TObject)
  private
    FID: Integer;
    FName: String;
    FDescription: String;
    FTypeObject: TTypeObject;
  protected
    procedure SetTypeObject; virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property ID: Integer read FID write FID;
    property Name: String read FName write FName;
    property Description: String read FDescription write FDescription;
    property TypeObject: TTypeObject read FTypeObject;
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
    constructor Create; override;
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
    constructor Create; override;
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
    constructor Create; override;
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
    constructor Create; override;
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
    FMethods: TCrMethods;
    FTables: TCrTables;
  protected
    procedure SetTypeObject; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    property Methods: TCrMethods read FMethods;
    property Tables: TCrTables read FTables;
  end;

  ///<summary>Список модулей</summary>
  TCrModules = class(TObjectList<TCrModule>)
  public
    constructor Create;
    destructor Destroy; override;
  end;

  // **************************************************************************
  // Системные функции
  TSysConfig = record
    class procedure SetTypeFields(const AFieldTypes: TStrings); static;
  end;

implementation

{ TCustomObjectModule }

constructor TCustomObjectModule.Create;
begin
  Self.SetTypeObject;
  FName := '';
  FDescription := '';
end;

destructor TCustomObjectModule.Destroy;
begin

  inherited;
end;

procedure TCustomObjectModule.SetTypeObject;
begin
  FTypeObject := TTypeObject.toNull;
end;

{ TCrField }

constructor TCrField.Create;
begin
  inherited Create;
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

constructor TCrIndex.Create;
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

constructor TCrTable.Create;
begin
  inherited;
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

constructor TCrMethod.Create;
begin
  inherited Create;
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

constructor TCrModule.Create;
begin
  inherited Create;
  FMethods := TCrMethods.Create;
  FTables := TCrTables.Create;
end;

destructor TCrModule.Destroy;
begin
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

{ TSysConfig }

class procedure TSysConfig.SetTypeFields(const AFieldTypes: TStrings);
begin
  if Assigned(AFieldTypes) then
  begin
    {todo: загрузить откуда}


  end;
end;

end.
