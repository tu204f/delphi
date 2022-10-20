unit Lb.Candel.StructuresFile;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Data.DB,
  Lb.Candel.SysUtils,
  Lb.DataModuleDB;

type
  ///<summary>Хранит массив структру</summary>
  TStructuresFile = class(TObject)
  private
    FCount: Integer;
    FDB: TDataModuleDB;
    FFileName: String;
    procedure SetFileName(const Value: String);
    procedure SetCreateTable;
    function GetCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    function Add(AStructure: TStructure): String;
    property FileName: String read FFileName write SetFileName;
    property Count: Integer read FCount;
  end;

implementation

const
  SQL_CREATE_STRUCTURES =
    'create table if not exists structures (' +
    ' _id text primary key,     ' +   // уникальный ключ структуры
    ' ind   integer,            ' +   // порядковый номер в массиве
    ' deleted integer default 0,' +   // удаленный объект
    ' status integer default 0  ' +   // статус
    ');';

  SQL_CREATE_INDEX_PK_STRUCTURES =
    'create unique index if not exists index_pk_structures on structures(_id);';

  SQL_CREATE_STRUCTURE_VALUES =
    'create table if not exists structure_values (' +
    ' _id text primary key,     ' +   // уникальный ключ структуры
    ' _structure_id text,       ' +   // ссылка на структура
    ' id integer,               ' +   // Порядковый номер, в массиве
    ' open  double,             ' +   // цена открытие
    ' high  double,             ' +   // максимальная цена
    ' low   double,             ' +   // минимальная цена
    ' close double,             ' +   // Цена закрытия
    ' vol   double,             ' +   // Объем который прошол
    ' deleted integer default 0,' +   // удаленный объект
    ' status integer default 0, ' +   // статус {TTypeValue}
                                      //  1 - исходные данные
                                      //  2 - ожидание
    ' constraint fk_structures     ' +
    '  foreign key (_structure_id) ' +
    '  references structures(_id)  ' +
    '  on delete cascade           ' +

    ');';

  SQL_CREATE_INDEX_PK_STRUCTURE_VALUES =
    'create unique index if not exists index_pk_structure_values on structure_values(_id);';

const
  SQL_INSERT_STRUCTURE =
    'insert into structures(_id,ind) values(:_id, :ind)';

  SQL_INSERT_STRUCTURE_VALUE =
    'insert into structure_values(_id,_structure_id,id,open,high,low,close,vol,status) ' +
    'values(:_id, :_structure_id, :id, :open, :high, :low, :close, :vol, :status)';

const
  SQL_SELECT_CLEAR =
    'delete from structures;';
  SQL_SELECT_COUNT =
    'select count(*) from structures';

{ TStructuresFile }


constructor TStructuresFile.Create;
begin
  FDB := nil;
end;

destructor TStructuresFile.Destroy;
begin
  FreeAndNil(FDB);
  inherited;
end;

function TStructuresFile.GetCount: Integer;
var
  xValue: Variant;
begin
  Result := 0;
  if Assigned(FDB) then
  begin
    xValue := FDB.GetExecSQLScalar(SQL_SELECT_COUNT);
    if not VarIsNull(xValue) then
      Result := xValue;
  end;
end;

procedure TStructuresFile.Clear;
begin
  if Assigned(FDB) then
    FDB.GetExecSQL(SQL_SELECT_CLEAR);
end;

function TStructuresFile.Add(AStructure: TStructure): String;

  function GetCreateGUID: String;

    function GetGeneratorCode: String;
    var
      xC, tmpS: String;
      i: Integer;
    begin
      tmpS := '';
      for i := 1 to 13 do
      begin
        xC := IntToHex(Random(16),1);
        tmpS := tmpS + xC;
      end;
      Result := tmpS;
    end;

  var
    xGUID : TGUID;
    xCode, xS: String;
  begin
    xCode := GetGeneratorCode;
    CreateGUID(xGUID);
    xS := GUIDToString(xGUID);
    Result := Copy(xS,2,xS.Length - 2) + '-' + xCode;
  end;

  function GetCreateObjectKey: String;
  begin
    Result := GetCreateGUID;
  end;

  procedure _Insert_Structure(const AKey: String);
  begin
    if Assigned(FDB) then
      FDB.GetExecSQL(SQL_INSERT_STRUCTURE,
        [AKey,FCount],
        [ftString,ftInteger]
      );
  end;

  procedure _Insert_Structure_Values(
    const AKey, AStructureKey: String;
    const AID: Integer;
    const ACandel: TCandel;
    const ATypeValue: TTypeValue);
  begin
    if Assigned(FDB) then
      FDB.GetExecSQL(SQL_INSERT_STRUCTURE_VALUE,
        [AKey,AStructureKey,AID,ACandel.Open,ACandel.High,ACandel.Low,ACandel.Close,ACandel.Vol,Integer(ATypeValue)],
        [ftString,ftString,ftInteger,ftFloat,ftFloat,ftFloat,ftFloat,ftFloat,ftInteger]);
  end;

  procedure _StructureValues(const AKey: String; const ACandels: TCandelList; const ATypeValue: TTypeValue);
  var
    xCandel: TCandel;
    i, iCount: Integer;
    xValueKey: String;
  begin
    iCount := ACandels.Count;
    if iCount > 0 then
      for i := 0 to iCount - 1 do
      begin
        xValueKey := GetCreateObjectKey;
        xCandel := ACandels[i];
        _Insert_Structure_Values(
          xValueKey,
          AKey,
          i,
          xCandel,
          ATypeValue
        );
      end;
  end;

var
  xStructureKey: String;
begin
  xStructureKey := GetCreateGUID;
  _Insert_Structure(xStructureKey);
  _StructureValues(xStructureKey,AStructure.SourceVectors,TTypeValue.tvSource);
  _StructureValues(xStructureKey,AStructure.FutureVectors,TTypeValue.tvFuture);
  Result := xStructureKey;
  Inc(FCount);
end;

procedure TStructuresFile.SetFileName(const Value: String);
begin
  FFileName := Value;
  if Assigned(FDB) then
    FreeAndNil(FDB);
  FDB := TDataModuleDB.Create(nil);
  FDB.DefaultConnection(FFileName);
  Self.SetCreateTable;
  FCount := GetCount;
end;

procedure TStructuresFile.SetCreateTable;
begin
  if Assigned(FDB) then
  begin
    FDB.GetExecSQL(SQL_CREATE_STRUCTURES);
    FDB.GetExecSQL(SQL_CREATE_INDEX_PK_STRUCTURES);
    FDB.GetExecSQL(SQL_CREATE_STRUCTURE_VALUES);
    FDB.GetExecSQL(SQL_CREATE_INDEX_PK_STRUCTURE_VALUES);
  end;
end;

end.
