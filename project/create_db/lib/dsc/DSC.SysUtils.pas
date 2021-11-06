unit DSC.SysUtils;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;

type
  ///<summary>Тип объекта</summary>
  TTypeObject = (
    toNull,
    toDomain,
    toField,
    toIndex,
    toTable,
    toTrigger,
    toMethod,
    toModule,
    toSсhema
  );


  TTypeData = class(TObject)
  public type
    ///<summary>Стандартный - типы значение</summary>
    TTypeValue = (
      tdNull,      //
      // Числовые типы - Целочисленные типы
      tdSmallInt,    // [2] знаковое двухбайтное целое
      tdInteger,     // [4] знаковое четырёхбайтное целое
      tdBigint,      // [8] знаковое целое из 8 байт
      // Числовые типы - Числа с произвольной точностью
      tdDecimal,     // [~] вещественное число с указанной точностью
      tdNumeric,     // [~] NUMERIC(точность, масштаб)
      // Числовые типы - Типы с плавающей точкой
      tdReal,        // [4] число одинарной точности с плавающей точкой (4 байта)
      tdDouble,      // [8] вещественное число с переменной точностью
      {todo: Расмотреть возможную объект}
      // Числовые типы - Последовательные типы
      tdSmallSerial, // [2] небольшое целое с автоувеличением
      tdSerial,      // [4] целое с автоувеличением
      tdBigSerial,   // [8] большое целое с автоувеличением
      // Символьные типы
      tdChar,       // строка ограниченной переменной длины
      tdVarChar,    // строка фиксированной длины, дополненная пробелами
      tdText,       // строка неограниченной переменной длины
      // Типы даты/времени
      tdDate,       // [4] дата (без времени суток)
      tdTime,       // [8] время суток (без даты)
      tdTimeStamp,  // [8] дата и время
      // Логический тип
      tdBoolean     // [1] состояние: истина или ложь
    );

  private
    FTypeValue: TTypeData.TTypeValue;
    FSize: Integer;
    procedure SetTypeData(const Value: TTypeData.TTypeValue);
    function GetValue: String;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property TypeValue: TTypeValue read FTypeValue write SetTypeData;
    property Size: Integer read FSize write FSize;
    property Value: String read GetValue;
  end;

  ///<summary>Базовый интерфейс</summary>
  TDscCustom = class(TObject)
  private
    FObjectKey: String;
    FParentKey: String;
    FName: String;
    FSysName: String;
    FDescription: String;
    FTypeObject: TTypeObject;
    FCreated: TDateTime;
    FUpDated: TDateTime;
  protected
    function GetValue: String; virtual;
  public
    constructor Create; overload; virtual;
    constructor Create(const AObjectKey: String); overload; virtual;
    constructor Create(const AObjectKey: String; const ATypeObject: TTypeObject); overload; virtual;
    destructor Destroy; override;
    property ObjectKey: String read FObjectKey write FObjectKey;
    property ParentKey: String read FParentKey write FParentKey;
  public
    ///<summary>Имя объекта для пользователя</summary>
    property Name: String read FName write FName;
    ///<summary>Системное имя – которое определяет имя (схемы, идекса, таблицы, триггера и хранимых процедур)</summary>
    property SysName: String read FSysName write FSysName;
    ///<summary>Описание объекта</summary>
    property Description: String read FDescription write FDescription;
    ///<summary>Определяет тип объекта</summary>
    property TypeObject: TTypeObject read FTypeObject;
    property Created: TDateTime read FCreated write FCreated;
    property UpDated: TDateTime read FUpDated write FUpDated;
    property Value: String read GetValue;
  end;

type
  TDscSсhema = class;

  TDscCutomObject = class(TDscCustom)
  private
    FSсhema: TDscSсhema;
  public
    constructor Create(ASсhema: TDscSсhema; AObjectKey: String; ATypeObject: TTypeObject); virtual;
    property Sсhema: TDscSсhema read FSсhema;
  end;

  ///<summary>Тип данных</summary>
  TDscDomain = class(TDscCutomObject)
  public type
    TTypeConstraint = (tcNotNull, tcNull, tcCheck);

    TConstraint = class(TObject)
    private
      FName: String;
      FTypeConstraint: TTypeConstraint;
      FChecks: TStrings;
    public
      constructor Create; overload; virtual;
      destructor Destroy; override;
      property Name: String read FName write FName;
      property TypeConstraint: TTypeConstraint read FTypeConstraint write FTypeConstraint;
      property Checks: TStrings read FChecks;
    end;

  private
    FTypeData: TTypeData;
    FDefaultValue: Variant;
  public
    constructor Create(ASсhema: TDscSсhema; AObjectKey: String); reintroduce;
    destructor Destroy; override;
    property TypeData: TTypeData read FTypeData;
    property DefaultValue: Variant read FDefaultValue write FDefaultValue;
  end;

  TDscDomainList = TObjectList<TDscDomain>;

  ///<summary>Поле</summary>
  TDscField = class(TDscCustom)
  public
    constructor Create(const AObjectKey: String); override;
    destructor Destroy; override;
  end;

  ///<summary>Индекс</summary>
  TDscIndex = class(TDscCustom)
  public
    constructor Create(const AObjectKey: String); override;
    destructor Destroy; override;
  end;

(*

В текущей реализации запись:
CREATE TABLE имя_таблицы (
    имя_столбца SERIAL
);

равнозначна следующим командам:
CREATE SEQUENCE имя_таблицы_имя_столбца_seq AS integer;
CREATE TABLE имя_таблицы (
    имя_столбца integer NOT NULL DEFAULT nextval('имя_таблицы_имя_столбца_seq')
);
ALTER SEQUENCE имя_таблицы_имя_столбца_seq OWNED BY имя_таблицы.имя_столбца;

*)

  ///<summary>Таблица</summary>
  TDscTable = class(TDscCustom)
  public
    constructor Create(const AObjectKey: String); override;
    destructor Destroy; override;
  end;

  ///<summary>Событие таблицы</summary>
  TDscTrigger = class(TDscCustom)
  public
    constructor Create(const AObjectKey: String); override;
    destructor Destroy; override;
  end;

  ///<summary>Реализует хранимые процедуры и функции</summary>
  TDscMethod = class(TDscCustom)
  public
    constructor Create(const AObjectKey: String); override;
    destructor Destroy; override;
  end;

  ///<summary>Модуль</summary>
  TDscModule  = class(TDscCustom)
  public
    constructor Create(const AObjectKey: String); override;
    destructor Destroy; override;
  end;
  TDcsModuleList = TObjectList<TDscModule>;

  TDscSсhema = class(TDscCustom)
  public
    constructor Create(const AObjectKey: String); override;
    destructor Destroy; override;
  end;

type
  TDefaultScript = record
    class procedure SetSaveStrings(ASource: TStrings; ASchema: TDscSсhema); static;
    class procedure SetLoadStrings(ASource: TStrings; ASchema: TDscSсhema); static;
    class procedure SetSaveFile(const AFileName: String; ASchema: TDscSсhema); static;
    class procedure SetLoadFile(const AFileName: String; ASchema: TDscSсhema); static;
  end;

implementation

function GetCurrentDateTime: TDateTime;
begin
  Result := System.SysUtils.Date + System.SysUtils.Time;
end;

{ TDefaultScript }

class procedure TDefaultScript.SetSaveFile(const AFileName: String; ASchema: TDscSсhema);
var
  xStr: TStrings;
begin
  xStr := TStringList.Create;
  try
    TDefaultScript.SetSaveStrings(xStr,ASchema);
    xStr.SaveToFile(AFileName);
  finally
    FreeAndNil(xStr);
  end;
end;

class procedure TDefaultScript.SetLoadFile(const AFileName: String; ASchema: TDscSсhema);
var
  xStr: TStrings;
begin
  if FileExists(AFileName) then
  begin
    xStr := TStringList.Create;
    try
      xStr.LoadFromFile(AFileName);
      TDefaultScript.SetLoadStrings(xStr,ASchema);
    finally
      FreeAndNil(xStr);
    end;
  end;
end;

class procedure TDefaultScript.SetSaveStrings(ASource: TStrings; ASchema: TDscSсhema);
begin
  { Парсер объекта }
end;

class procedure TDefaultScript.SetLoadStrings(ASource: TStrings; ASchema: TDscSсhema);
begin
  { Формирование объекта }
end;

{ TTypeData }

constructor TTypeData.Create;
begin
  FSize := -1;
  FTypeValue := TTypeData.TTypeValue.tdNull;
end;

destructor TTypeData.Destroy;
begin

  inherited;
end;

procedure TTypeData.SetTypeData(const Value: TTypeData.TTypeValue);
begin
  FTypeValue := Value;
end;

function TTypeData.GetValue: String;
begin
  case FTypeValue of
    tdNull: raise Exception.Create('Error Message. TTypeValue.GetValue: Типа данных не установлен');
    tdSmallInt   : Result := 'smallint';
    tdInteger    : Result := 'integer';
    tdBigint     : Result := 'bigint';
    // Числовые типы - Числа с произвольной точностью
    //tdDecimal,     // [~] вещественное число с указанной точностью
    //tdNumeric,     // [~] NUMERIC(точность, масштаб)
    tdReal        : Result := 'real';
    tdDouble      : Result := 'double';
    tdSmallSerial : Result := 'smallserial';
    tdSerial      : Result := 'serial';
    tdBigSerial   : Result := 'bigserial';
    // Символьные типы
    tdChar        : begin
      if FSize <= 0 then
        raise Exception.Create('Error Message. Не установлено размер');
      Result := 'char(' + IntToStr(FSize) + ')';
    end;
    tdVarChar     : begin
      if FSize <= 0 then
        raise Exception.Create('Error Message. Не установлено размер');
      Result := 'varchar(' + IntToStr(FSize) + ')';
    end;
    tdText        : Result := 'text';
    // Типы даты/времени
    tdDate        : Result := 'date';
    tdTime        : Result := 'time';
    tdTimeStamp   : Result := 'timestamp';
    // Логический тип
    tdBoolean     : Result := 'boolean';
  else
    raise Exception.Create('Error Message. TTypeValue.GetValue: Типа данных не определен');
  end;
end;

{ TDscCustom }

constructor TDscCustom.Create;
begin
  Create('');
end;

constructor TDscCustom.Create(const AObjectKey: String);
begin
  Create(AObjectKey,TTypeObject.toNull);
end;

constructor TDscCustom.Create(const AObjectKey: String; const ATypeObject: TTypeObject);
begin
  FObjectKey := AObjectKey;
  FParentKey := '';
  FName := '';
  FSysName := '';
  FDescription := '';
  FTypeObject := ATypeObject;
  FCreated := GetCurrentDateTime;
  FUpDated := GetCurrentDateTime;
end;

destructor TDscCustom.Destroy;
begin

  inherited;
end;

function TDscCustom.GetValue: String;
begin
  Result := '';
end;

{ TDscCutomObject }

constructor TDscCutomObject.Create(ASсhema: TDscSсhema; AObjectKey: String; ATypeObject: TTypeObject);
begin
  inherited Create(AObjectKey,TTypeObject.toDomain);
  FSсhema := ASсhema;
  FParentKey := ASсhema.ObjectKey;
end;

{ TDscDomain.TConstraint }

constructor TDscDomain.TConstraint.Create;
begin
  FChecks := TStringList.Create;
end;

destructor TDscDomain.TConstraint.Destroy;
begin
  FreeAndNil(FChecks);
  inherited;
end;

{ TDscDomain }

constructor TDscDomain.Create(ASсhema: TDscSсhema; AObjectKey: String);
begin
  inherited Create(ASсhema,AObjectKey,TTypeObject.toDomain);
  FTypeData := TTypeData.Create;
end;

destructor TDscDomain.Destroy;
begin
  FreeAndNil(FTypeData);
  inherited;
end;


{ TDscField }

constructor TDscField.Create(const AObjectKey: String);
begin
  inherited Create(AObjectKey,TTypeObject.toField);

end;

destructor TDscField.Destroy;
begin

  inherited;
end;

{ TDscIndex }

constructor TDscIndex.Create(const AObjectKey: String);
begin
  inherited Create(AObjectKey,TTypeObject.toIndex);

end;

destructor TDscIndex.Destroy;
begin

  inherited;
end;

{ TDscTable }

constructor TDscTable.Create(const AObjectKey: String);
begin
  inherited Create(AObjectKey,TTypeObject.toTable);

end;

destructor TDscTable.Destroy;
begin

  inherited;
end;

{ TDscTrigger }

constructor TDscTrigger.Create(const AObjectKey: String);
begin
  inherited Create(AObjectKey,TTypeObject.toTrigger);

end;

destructor TDscTrigger.Destroy;
begin

  inherited;
end;

{ TDscMethod }

constructor TDscMethod.Create(const AObjectKey: String);
begin
  inherited Create(AObjectKey,TTypeObject.toMethod);

end;

destructor TDscMethod.Destroy;
begin

  inherited;
end;

{ TDscModule }

constructor TDscModule.Create(const AObjectKey: String);
begin
  inherited Create(AObjectKey,TTypeObject.toModule);

end;

destructor TDscModule.Destroy;
begin

  inherited;
end;

{ TDscSсhema }

constructor TDscSсhema.Create(const AObjectKey: String);
begin
  inherited Create(AObjectKey,TTypeObject.toSсhema);

end;

destructor TDscSсhema.Destroy;
begin

  inherited;
end;



end.
