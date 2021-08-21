(*******************************************************************************
  Работы с базой данных, где будем хранить - напровление векторов
*******************************************************************************)
unit Lb.Candel.DB;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.Candel.SysUtils;

///<summary>Создание базы</summary>
procedure SetCreateDataBase;
///<summary>Проверка наличие вектора</summary>
function GetIsVector(const ADate, ATime: TDateTime): Boolean;
///<summary>Добавляем запись вектора</summary>
function GetInsertVector(const ADate, ATime: TDateTime): String;
///<summary>Добавляем значения вектора</summary>
procedure SetInsertVectorValue(AVectorID: String; ATypeValue: Integer; AOpen, AHigh, ALow, AClose, AVal: Double);

implementation

uses
  Data.DB,
  Lb.Resource.Script,
  Lb.DataModuleDB,
  Lb.SysUtils.ISO860,
  Lb.Candel.Source;

const
  DB_CANDEL = 'data.db';

var
  localDB: TDataModuleDB = nil;

function GetDB: TDataModuleDB;
begin
  if not Assigned(localDB) then
  begin
    localDB := TDataModuleDB.Create(nil);
    localDB.DefaultConnection(DB_CANDEL);
  end;
  Result := localDB;
end;

procedure SetFinalizaitonDB;
begin
  if Assigned(localDB) then
    FreeAndNil(localDB);
  localDB := nil;
end;

procedure SetCreateDataBase;
var
  xScript: String;
begin
  xScript := GetResourceScritp('DB_SQL');
  GetDB.GetExecSQL(xScript);
end;


function GetGeneratorID: String;
var
  xS: String;
  xGUID: TGUID;
begin
  CreateGUID(xGUID);
  xS := GUIDToString(xGUID);
  Result := xS;
end;

///<summary>Проверка наличие вектора</summary>
function GetIsVector(const ADate, ATime: TDateTime): Boolean;
begin
  Result := False;
  var xDate := GetDateToStrISO860(ADate);
  var xTime := GetTimeToStrISO860(ATime);
  var xSQL  := 'select count(*) from vector where date = :date and time = :time';
  var xValue := GetDB.GetExecSQLScalar(xSQL,[xDate,xTime],[ftString,ftString]);
  if not VarIsNull(xValue) then
    Result := (xValue > 0)
end;

///<summary>Добавляем запись вектора</summary>
function GetInsertVector(const ADate, ATime: TDateTime): String;
var
  xID: String;
  xSQL: String;
  xStrDate, xStrTime: String;
  xResult: Boolean;
begin
  xID := GetGeneratorID;
  xStrDate := GetDateToStrISO860(ADate);
  xStrTime := GetTimeToStrISO860(ATime);
  xSQL := 'insert into vector(date,time,id) values(:date,:time,:id)';
  xResult := GetDB.GetExecSQL(
    xSQL,
    [xStrDate,xStrTime,xID],
    [ftString,ftString,ftString]);
  if xResult then
    Result := xID;
end;

///<summary>Добавляем значения вектора</summary>
procedure SetInsertVectorValue(AVectorID: String; ATypeValue: Integer; AOpen, AHigh, ALow, AClose, AVal: Double);
begin
  var xStr := TStringList.Create;
  xStr.Add('insert into vector_value(vector_id,type_value,open,high,low,close,val)');
  xStr.Add('values(:vector_id,:type_value,:open,:high,:low,:close,:val)');
  GetDB.GetExecSQL(xStr.Text,
    [AVectorID,ATypeValue,AOpen,AHigh,ALow,AClose,AVal],
    [ftString,ftInteger,ftFloat,ftFloat,ftFloat,ftFloat,ftFloat]);
  FreeAndNil(xStr);
end;

initialization

finalization
  SetFinalizaitonDB;

end.
