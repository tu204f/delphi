(*******************************************************************************
  Документа орентированая база

  todo:
    1. Нужно продумать возможность хранения файлов
    2. Для обеспечение занятости при мого поточности, одно
       соединение один запрос.
    3. Каждый тип данных хранить в отдельной талицы 
*******************************************************************************)
unit Lb.SysUtils.DocDB;

interface

uses
  System.SysUtils,
  System.Classes,
  System.Generics.Collections,
  System.Variants,
  Lb.DataModuleDB;

type
  TDcDataBase = class;
  TDcDocument = class;

  /// <summary>
  /// Типы данных, используемы в базе
  /// </summary>
  TTypeValue = (
    tvInteger,
    tvDouble,
    tvDate,
    tvTime,
    tvDateTime,
    tvString,
    tvBoolean);

  /// <summary>
  /// Значение работы
  /// </summary>
  /// <remarks>
  /// Любое значене – это объект
  /// </remarks>
  TDcValue = class(TObject)
    Name: String;
    Description: String;
    Value: String;
    TypeValue: TTypeValue;
  private
    function GetAsDate: TDateTime;
    function GetAsDateTime: TDateTime;
    function GetAsDouble: Double;
    function GetAsInteger: Integer;
    function GetAsString: String;
    function GetAsTime: TDateTime;
    procedure SetAsDate(const AValue: TDateTime);
    procedure SetAsDateTime(const AValue: TDateTime);
    procedure SetAsDouble(const AValue: Double);
    procedure SetAsInteger(const AValue: Integer);
    procedure SetAsString(const AValue: String);
    procedure SetAsTime(const AValue: TDateTime);
    function GetTypeValueInt: Integer;
    procedure SetTypeValueInt(const Value: Integer);
  public
    constructor Create; overload;
    constructor Create(AName, ADescription: String; AValue: Integer); overload;
    constructor Create(AName, ADescription: String; AValue: Double); overload;
    constructor Create(AName, ADescription: String; AValue: String); overload;
    constructor Create(AName, ADescription: String; AValue: TDateTime); overload;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsDate: TDateTime read GetAsDate write SetAsDate;
    property AsTime: TDateTime read GetAsTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsString: String read GetAsString write SetAsString;
  public
    property TypeValueInt: Integer read GetTypeValueInt write SetTypeValueInt;
  end;
  TDcValueList = TObjectList<TDcValue>;

  /// <summary>
  /// Документ
  /// </sumary>
  TDcDocument = class(TObject)
  public type
    TTypeDocument = (tdNew,tdEdit);
  private
    FDcDataBase: TDcDataBase;
  private
    FUniversalID: String;
    FCreated: TDateTime;
    FChanged: TDateTime;
    FDeleted: Integer;
    FStatus: Integer;
    FTypeDocument: TTypeDocument;
    FValues: TDcValueList;
    procedure SetTypeDocument(const Value: TTypeDocument);
  private
    function GetIndexOf(const AName: String): Integer;
    function GetByName(Name: String): TDcValue;
  protected
    procedure SetWriteValueDocument;
    procedure SetReadValueDocument;
    procedure SetInsertDocument;
    procedure SetUpdataDocument;
    procedure SetByName(Name: String; const Value: TDcValue);
    procedure SetUniversalID(const AUniversalID: String);
    property TypeDocument: TTypeDocument read FTypeDocument write SetTypeDocument;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    /// <summary>Принадлежность к базе данных</summary>
    property DataBase: TDcDataBase read FDcDataBase write FDcDataBase;
  public
    /// <summary>Читать документ</summary>
    procedure Read;
    /// <summary>Сохранить документ</summary>
    procedure Write;
    /// <summary>Доступ к данных</summary>
    property ByValue[Name: String]: TDcValue read GetByName write SetByName;
    /// <summary>Массив значения</summary>
    property Values: TDcValueList read FValues;
    /// <summary>Универсальный id - документа</summary>
    /// <remarks>
    ///   Представляет из себя набор цифре, в которых записано дата и
    ///   время создание документа, и три цифры с генерирование случайно
    /// </remarks>
    property UniversalID: String read FUniversalID;
    /// <summary>Дата создания документа</summary>
    property Created: TDateTime read FCreated;
    /// <summary>Время изменение документа</summary>
    property Changed: TDateTime read FChanged;
    /// <summary>Были ранние удален документ</summary>
    property Deleted: Integer read FDeleted write FDeleted;
    /// <summary>Состояние документа</summary>
    /// <remarks>Определяет состояния документа</remarks>
    property Status: Integer read FStatus write FStatus;
  end;

(******************************************************************************)

  /// <summary>
  /// Формирует условия поиска
  /// </summary>
  /// <remarks>
  /// Представляет парсер запроса, на поиск
  /// </remarks>
  TDcSearch = class(TObject)
  private
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

(******************************************************************************)

  /// <summary>
  /// Колекция документов, или точне выборка документов
  /// </summary>
  TDcDocumentCollection = class(TObject)
  private
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  /// <summary>
  /// Структурирование данных
  /// </summary>
  /// <remarks>
  ///   Тоже представляет из себя - выборку документов, в виде таблицы
  ///   TDcView – подборка документа подвобранные условия
  /// </remarks>
  TDcView = class(TObject)
  private
    FName: String;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    property Name: String read FName write FName;
  end;

(******************************************************************************)

  /// <summary>
  /// Базу данных, документов
  /// </summary>
  TDcDataBase = class(TObject)
  private
    FFileName: String;
    FDataModuleDB: TDataModuleDB;
    procedure SetCreateTableDocument;
    procedure SetCreateTableDocumentValue;
  protected
    property DataModuleDB: TDataModuleDB read FDataModuleDB;
    procedure SetCreateTable(const AResourceName: String);
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Open;
    procedure Close;
    property FileName: String read FFileName write FFileName;
  public {Получение доступа к документу}
    /// <summmary>Создание документа</summary>
    function GetCreateDocument: TDcDocument;
    /// <summary>Проверка наличие документа</summary>
    function GetIsDocument(const AUniversalID: String): Boolean;
    /// <summary>Получение документа, по его ID</summary>
    function GetByDocument(const AUniversalID: String): TDcDocument;
    /// <summary>Всего списка документов</summary>
    function GetCreateDocumentCollectionAll: TDcDocumentCollection;
    /// <summary>Получение списка документов, согласно списку документов</summary>
    function GetCreateSearch(const ASource: String): TDcDocumentCollection;
  end;

implementation

uses
  Data.DB,
  Lb.Resource.Script,
  Lb.SysUtils.ISO860;

function GetFloatToStr(const AValue: Double): String;
var
  xOldDecimalSeparator: Char;
begin
  xOldDecimalSeparator := FormatSettings.DecimalSeparator; 
  FormatSettings.DecimalSeparator := '.';
  Result := FloatToStr(AValue);
  FormatSettings.DecimalSeparator := xOldDecimalSeparator;
end;


function GetStrToFloat(const AValue: String): Double;
var
  xIndexPos: Integer;
  xValue: String;
  xOldDecimalSeparator: Char;
begin
  xValue := AValue;
  xIndexPos := Pos(',',xValue);
  if xIndexPos >= 0 then
    xValue[xIndexPos] := '.';    
  xOldDecimalSeparator := FormatSettings.DecimalSeparator; 
  FormatSettings.DecimalSeparator := '.';
  Result := StrToFloatDef(xValue,0);
  FormatSettings.DecimalSeparator := xOldDecimalSeparator;
end;

function GetIsNameNumber(const AUniversalID, AName: String; ADataBase: TDataModuleDB): Boolean;
var
  xSQL: String;
  xDataSet: TDataSet;
begin
  // Проверяем есть поле в таблице
  //  True - поле есть
  //  False - поле нет
  Result := False;
  xSQL := 'select count(*) as cnt from number_values where _id_doc = :id_doc and name = :name';
  if Assigned(ADataBase) then
  begin
    xDataSet := ADataBase.GetSelectCreateDataSet(xSQL,[AUniversalID,AName],[ftString,ftString]);
    if Assigned(xDataSet) then
    begin
      xDataSet.First;
      Result := xDataSet.FieldByName('cnt').AsInteger > 0;
      FreeAndNil(xDataSet);
    end;
  end;
end;

function GetIsNameText(const AUniversalID, AName: String; ADataBase: TDataModuleDB): Boolean;
var
  xSQL: String;
  xDataSet: TDataSet;
begin
  // Проверяем есть поле в таблице
  //  True - поле есть
  //  False - поле нет
  Result := False;
  xSQL := 'select count(*) as cnt from text_values where _id_doc = :id_doc and name = :name';
  if Assigned(ADataBase) then
  begin
    xDataSet := ADataBase.GetSelectCreateDataSet(xSQL,[AUniversalID,AName],[ftString,ftString]);
    if Assigned(xDataSet) then
    begin
      xDataSet.First;
      Result := xDataSet.FieldByName('cnt').AsInteger > 0;
      FreeAndNil(xDataSet);
    end;
  end;
end;

function GetIsNameDateTime(const AUniversalID, AName: String; ADataBase: TDataModuleDB): Boolean;
var
  xSQL: String;
  xDataSet: TDataSet;
begin
  // Проверяем есть поле в таблице
  //  True - поле есть
  //  False - поле нет
  Result := False;
  xSQL := 'select count(*) as cnt from datetime_values where _id_doc = :id_doc and name = :name';
  if Assigned(ADataBase) then
  begin
    xDataSet := ADataBase.GetSelectCreateDataSet(xSQL,[AUniversalID,AName],[ftString,ftString]);
    if Assigned(xDataSet) then
    begin
      xDataSet.First;
      Result := xDataSet.FieldByName('cnt').AsInteger > 0;
      FreeAndNil(xDataSet);
    end;
  end;
end;

function GetIsNameBlob(const AUniversalID, AName: String; ADataBase: TDataModuleDB): Boolean;
var
  xSQL: String;
  xDataSet: TDataSet;
begin
  // Проверяем есть поле в таблице
  //  True - поле есть
  //  False - поле нет
  Result := False;
  xSQL := 'select count(*) as cnt from datetime_values where _id_doc = :id_doc and name = :name';
  if Assigned(ADataBase) then
  begin
    xDataSet := ADataBase.GetSelectCreateDataSet(xSQL,[AUniversalID,AName],[ftString,ftString]);
    if Assigned(xDataSet) then
    begin
      xDataSet.First;
      Result := xDataSet.FieldByName('cnt').AsInteger > 0;
      FreeAndNil(xDataSet);
    end;
  end;
end;

{ TDcValue }

constructor TDcValue.Create;
begin
  Name := '';
  Description := '';
  Value := '';
  TypeValue := TTypeValue.tvString;
end;

constructor TDcValue.Create(AName, ADescription, AValue: String);
begin
  Name := AName;
  Description := ADescription;
  Self.AsString := AValue;
end;

constructor TDcValue.Create(AName, ADescription: String; AValue: Integer);
begin
  Name := AName;
  Description := ADescription;
  Self.AsInteger := AValue;
end;

constructor TDcValue.Create(AName, ADescription: String; AValue: Double);
begin
  Name := AName;
  Description := ADescription;
  Self.AsDouble := AValue;
end;

constructor TDcValue.Create(AName, ADescription: String; AValue: TDateTime);
begin
  Name := AName;
  Description := ADescription;
  Self.AsDateTime := AValue;
end;


function TDcValue.GetAsDate: TDateTime;
begin
  Result := StrToDateDef(Self.Value,0)
end;

function TDcValue.GetAsDateTime: TDateTime;
begin
  Result := StrToDateTimeDef(Self.Value,0)
end;

function TDcValue.GetAsDouble: Double;
begin
  Result := GetStrToFloat(Self.Value);
end;

function TDcValue.GetAsInteger: Integer;
begin
  Result := StrToIntDef(Value,0)
end;

function TDcValue.GetAsString: String;
begin
  Result := Value;
end;

function TDcValue.GetAsTime: TDateTime;
begin
  Result := GetStrISO860ToTime(Value);
end;

function TDcValue.GetTypeValueInt: Integer;
begin
  Result := Integer(Self.TypeValue);
end;

procedure TDcValue.SetAsDate(const AValue: TDateTime);
begin
  TypeValue := tvDate;
  Value := GetDateToStrISO860(AValue);
end;

procedure TDcValue.SetAsDateTime(const AValue: TDateTime);
begin
  TypeValue := tvDateTime;
  Self.Value := GetDateTimeToStrISO860(AValue)
end;

procedure TDcValue.SetAsDouble(const AValue: Double);
begin
  TypeValue := tvDouble;
  Value := GetFloatToStr(AValue)
end;

procedure TDcValue.SetAsInteger(const AValue: Integer);
begin
  TypeValue := tvInteger;
  Value := IntToStr(AValue);
end;

procedure TDcValue.SetAsString(const AValue: String);
begin
  TypeValue := tvString;
  Value := AValue;
end;

procedure TDcValue.SetAsTime(const AValue: TDateTime);
begin
  TypeValue := tvTime;
  Value := TimeToStr(AValue)
end;


procedure TDcValue.SetTypeValueInt(const Value: Integer);
begin
  TypeValue := TTypeValue(Value);
end;

{ TDcDocument }

constructor TDcDocument.Create;
begin
  FUniversalID := '';
  FValues := TDcValueList.Create;
end;

destructor TDcDocument.Destroy;
begin
  FreeAndNil(FValues);
  inherited;
end;

function TDcDocument.GetIndexOf(const AName: String): Integer;
var
  xValue: TDcValue;
  i, Count: Integer;
begin
  Result := -1;
  Count := FValues.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xValue := FValues[i];
      if SameText(xValue.Name,AName) then
      begin
        Result := i;
        Break;
      end;
    end;
end;


function TDcDocument.GetByName(Name: String): TDcValue;
var
  xValue: TDcValue;
  xInd: Integer;
begin
  xInd := GetIndexOf(Name);
  if xInd >= 0 then
  begin
    xValue := FValues[xInd];
  end
  else
  begin
    xValue := TDcValue.Create;
    xValue.Name := Name;
    FValues.Add(xValue);
  end;
  Result := xValue;
end;

procedure TDcDocument.SetByName(Name: String; const Value: TDcValue);
var
  xInd: Integer;
begin
  xInd := GetIndexOf(Name);
  if xInd >= 0 then
    FValues[xInd] := Value
  else
    FValues.Add(Value);
end;

procedure TDcDocument.SetWriteValueDocument;

  function GetIsValue(const AName: String): Boolean;
  var
    xSQL: String;
    xDataSet: TDataSet;
  begin
    // Проверяем есть поле в таблице
    //  True - поле есть
    //  False - поле нет
    Result := False;
    xSQL := 'select count(*) as cnt from document_values where _id_doc = :id_doc and name = :name';
    if Assigned(FDcDataBase) then
    begin
      xDataSet := FDcDataBase.DataModuleDB.GetSelectCreateDataSet(xSQL,[FUniversalID,AName],[ftString,ftString]);
      if Assigned(xDataSet) then
      begin
        xDataSet.First;
        Result := xDataSet.FieldByName('cnt').AsInteger > 0;
        FreeAndNil(xDataSet);
      end;
    end;
  end;

  procedure SetInsertValue(const AValue: TDcValue);
  var
    xSQL: String;
  begin
    xSQL := 'insert into document_values(_id_doc,name,description,value,type) values (?,?,?,?,?)';
    if Assigned(FDcDataBase) then
    begin
      FDcDataBase.FDataModuleDB.GetExecSQL(xSQL,
        [FUniversalID,AValue.Name,AValue.Description,AValue.Value,AValue.TypeValueInt],
        [ftString,ftString,ftString,ftString,ftInteger]);
    end;
  end;

  procedure SetUpDataValue(const AValue: TDcValue);
  var
    xSQL: String;
  begin
    with TStringList.Create do
    begin
      // description,value,type
      Add('update document_values set');
      Add(' description = :description,');
      Add(' value = :value,');
      Add(' type  = :type');
      Add('where _id_doc = :id and name = :name');
      xSQL := Text;
      Free;
    end;
    if Assigned(FDcDataBase) then
    begin
      FDcDataBase.FDataModuleDB.GetExecSQL(xSQL,
        [AValue.Description,AValue.Value,AValue.TypeValueInt,FUniversalID,AValue.Name],
        [ftString,ftString,ftInteger,ftString,ftString]);
    end;
  end;

var
  xValue: TDcValue;
  i, Count: Integer;
begin
  Count := FValues.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xValue := FValues[i];
      if GetIsValue(xValue.Name) then
        SetUpDataValue(xValue)
      else
        SetInsertValue(xValue);
    end;
end;

procedure TDcDocument.SetReadValueDocument;
var
  xSQL: String;
  xDataSet: TDataSet;
  xValue: TDcValue;
begin
  if Assigned(FDcDataBase) then
  begin
    xSQL := 'select name,description,value,type from document_values where  _id_doc = :id_doc';
    xDataSet := FDcDataBase.DataModuleDB.GetSelectCreateDataSet(xSQL,[FUniversalID],[ftString]);
    if Assigned(xDataSet) then
    begin
      xDataSet.First;
      while not xDataSet.Eof do
      begin
        xValue := TDcValue.Create;
        xValue.Name := xDataSet.FieldByName('name').AsString;
        xValue.Description := xDataSet.FieldByName('description').AsString;
        xValue.Value := xDataSet.FieldByName('value').AsString;
        xValue.TypeValueInt := xDataSet.FieldByName('type').AsInteger;
        FValues.Add(xValue);
        xDataSet.Next;
      end;
      FreeAndNil(xDataSet);
    end;
  end;
end;

procedure TDcDocument.SetInsertDocument;
var
  xSQL: String;
  xCreated,xChanged: String;
begin
  if Assigned(FDcDataBase) then
  begin
    xSQL := 'insert into documents (_id,created,changed) values(:id,:created,:changed)';
    xCreated := GetDateTimeToStrISO860(FCreated);
    xChanged := GetDateTimeToStrISO860(FChanged);
    FDcDataBase.DataModuleDB.GetExecSQL(xSQL,[FUniversalID,xCreated,xChanged],[ftString,ftString,ftString]);
  end;
end;

procedure TDcDocument.SetUniversalID(const AUniversalID: String);
begin
  FUniversalID := AUniversalID;
end;

procedure TDcDocument.SetUpdataDocument;
var
  xSQL: String;
  xChanged: String;
begin
  if Assigned(FDcDataBase) then
  begin
    with TStringList.Create do
    begin
      Add('update documents set');
      Add(' changed = :changed,');
      Add(' deleted = :deleted,');
      Add(' status  = :status');
      Add('where _id = :id');
      xSQL := Text;
      Free;
    end;
    xChanged := GetDateTimeToStrISO860(FChanged);
    FDcDataBase.DataModuleDB.GetExecSQL(xSQL,
      [xChanged,FDeleted,FStatus,FUniversalID],
      [ftString,ftInteger,ftInteger,ftInteger,ftString]);
  end;
end;

procedure TDcDocument.SetTypeDocument(const Value: TTypeDocument);

  function GetIntToStr(const AValue, ACount: Integer): String;
  var
    xS: String;
  begin
    xS := IntToStr(AValue);
    while xS.Length < ACount do
      xS := '0' + xS;
    Result := xS;
  end;

  function GetGeneratorUniversalID: String;
  var
    xS: String;
    xYear, xMonth, xDay: Word;
    xHour, xMin, xSec, xMSec: Word;
  begin
    DecodeDate(Date,xYear, xMonth, xDay);
    DecodeTime(Time,xHour, xMin, xSec, xMSec);
    xS := GetIntToStr(xYear,4) +
          GetIntToStr(xMonth,2) +
          GetIntToStr(xDay,2);
    xS := xS +
          GetIntToStr(xHour,2) +
          GetIntToStr(xMin,2) +
          GetIntToStr(xSec,2) +
          GetIntToStr(xMSec,3);
    xS := xS + GetIntToStr(Random(1000),3);
    Result := xS;
  end;

begin
  FTypeDocument := Value;
  case Value of
    tdNew: begin
      FUniversalID := GetGeneratorUniversalID;
      FCreated := System.SysUtils.Date + System.SysUtils.Time;
      FChanged := System.SysUtils.Date + System.SysUtils.Time;
    end;
    tdEdit: begin
      FChanged := System.SysUtils.Date;
    end;
  end;
end;


procedure TDcDocument.Read;
var
  xSQL: String;
  xDataSet: TDataSet;
begin
  if Assigned(FDcDataBase) then
  begin
    if FTypeDocument <> tdNew then
    begin
      xSQL := 'select d.created, d.changed, d.deleted, d.status from documents d where d._id = :id';
      xDataSet := FDcDataBase.DataModuleDB.GetSelectCreateDataSet(xSQL,[FUniversalID],[ftString]);
      if Assigned(xDataSet) then
      begin
        xDataSet.First;
        FCreated := GetStrISO860ToDateTime(xDataSet.FieldByName('created').AsString);
        FChanged := GetStrISO860ToDateTime(xDataSet.FieldByName('changed').AsString);
        FDeleted := xDataSet.FieldByName('deleted').AsInteger;
        FStatus  := xDataSet.FieldByName('status').AsInteger;
        FreeAndNil(xDataSet);
      end;
      SetReadValueDocument;
    end;
  end;
end;

procedure TDcDocument.Write;
begin
  case FTypeDocument of
    tdNew: SetInsertDocument;
    tdEdit: SetUpdataDocument;
  end;
  SetWriteValueDocument;
end;

{ TDcSearch }

constructor TDcSearch.Create;
begin

end;

destructor TDcSearch.Destroy;
begin

  inherited;
end;

{ TDcDocumentCollection }

constructor TDcDocumentCollection.Create;
begin

end;

destructor TDcDocumentCollection.Destroy;
begin

  inherited;
end;

{ TDcView }

constructor TDcView.Create;
begin

end;

destructor TDcView.Destroy;
begin

  inherited;
end;

{ TDcDataBase }

constructor TDcDataBase.Create;
begin
  FDataModuleDB := TDataModuleDB.Create(nil);
end;

destructor TDcDataBase.Destroy;
begin
  FreeAndNil(FDataModuleDB);
  inherited;
end;

procedure TDcDataBase.Open;
begin
  FDataModuleDB.DefaultConnection(FFileName);
  SetCreateTableDocument;
  SetCreateTableDocumentValue;
end;

procedure TDcDataBase.Close;
begin
  FDataModuleDB.Close;
end;

procedure TDcDataBase.SetCreateTable(const AResourceName: String);
var
  xSQL: String;
begin
  if not AResourceName.IsEmpty then
  begin
    xSQL := GetResourceScritp(AResourceName);
    FDataModuleDB.GetExecSQL(xSQL);
  end;
end;

procedure TDcDataBase.SetCreateTableDocument;
begin
  SetCreateTable('CREATE_TABLE_DOCUMENTS');
end;

procedure TDcDataBase.SetCreateTableDocumentValue;
begin
  SetCreateTable('CREATE_NUMBER_VALUES');
  SetCreateTable('CREATE_TEXT_VALUES');
  SetCreateTable('CREATE_DATETIME_VALUES');
  SetCreateTable('CREATE_BLOB_VALUES ');
end;

function TDcDataBase.GetIsDocument(const AUniversalID: String): Boolean;
var
  xSQL: String;
  xDataSet: TDataSet;
begin
  Result := False;
  if not AUniversalID.IsEmpty then
  begin
    xSQL := 'select count(*) as cnt  from documents where _id = :id';
    xDataSet := FDataModuleDB.GetSelectCreateDataSet(xSQL,[AUniversalID],[ftString]);
    if Assigned(xDataSet) then
    begin
      xDataSet.First;
      Result := xDataSet.FieldByName('cnt').AsInteger > 0;
      FreeAndNil(xDataSet);
    end;
  end;
end;

function TDcDataBase.GetByDocument(const AUniversalID: String): TDcDocument;
var
  xDocument: TDcDocument;
begin
  if GetIsDocument(AUniversalID) then
  begin
    xDocument := TDcDocument.Create;
    xDocument.DataBase := Self;
    xDocument.TypeDocument := tdEdit;
    xDocument.SetUniversalID(AUniversalID);
    Result := xDocument;
  end
  else
    Result := nil;
end;

function TDcDataBase.GetCreateDocument: TDcDocument;
var
  xDocument: TDcDocument;
begin
  xDocument := TDcDocument.Create;
  xDocument.DataBase := Self;
  xDocument.TypeDocument := tdNew;
  Result := xDocument;
end;

function TDcDataBase.GetCreateDocumentCollectionAll: TDcDocumentCollection;
begin
  Result := nil;
end;

function TDcDataBase.GetCreateSearch(const ASource: String): TDcDocumentCollection;
begin
  Result := nil;
end;




initialization

finalization

end.
