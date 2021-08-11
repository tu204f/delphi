(*******************************************************************************
  Управляем датами загруженными с базы данных.
*******************************************************************************)
unit Lb.Script.SQL;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections,
  Lb.DB.Connection,
  Lb.Memory.DB;

type
  TScript = record
    Name: String;
    SQL: String;
  public
    function GetSizeSQL: Integer;
  end;
  TScriptList = TList<TScript>;

  TScriptSQL = class(TObject)
  private
    FScripts: TScriptList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TScript;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const AName: String);
    function IndexOf(const ANameScript: String): Integer;
    function GetSQL(const ANameScript: String): String;
    property Items[Index: Integer]: TScript read GetItems;
    property Count: Integer read GetCount;
  public
    /// <summary>
    /// Регистрация заявок
    /// </summary>
    class procedure Registration(const ANameScript: String);
  end;

var
  ScriptSQL: TScriptSQL = nil;

implementation

uses
  Lb.ScriptSQL,
  Data.DB,
  System.SyncObjs,
  Lb.Logger;

var
  Section: TCriticalSection;

{ TScript }

function TScript.GetSizeSQL: Integer;
begin
  Result := Length(SQL);
end;

{ TScriptSQL }

constructor TScriptSQL.Create;
begin
  FScripts := TScriptList.Create;
end;

destructor TScriptSQL.Destroy;
begin
  FreeAndNil(FScripts);
  inherited;
end;

procedure TScriptSQL.Clear;
begin
  FScripts.Clear;
end;

procedure TScriptSQL.Add(const AName: String);
var
  xS: TScript;
  xSQL: string;
begin
  xS.Name := AName;
  xSQL := Lb.ScriptSQL.GetScriptSQL(AName);
  if not xSQL.IsEmpty then
  begin
    xS.SQL := xSQL;
    FScripts.Add(xS);
  end;
end;

function TScriptSQL.GetCount: Integer;
begin
  Result := FScripts.Count;
end;

function TScriptSQL.GetItems(Index: Integer): TScript;
begin
  Result := FScripts.Items[Index];
end;

function TScriptSQL.IndexOf(const ANameScript: String): Integer;
var
  i, Count: Integer;
begin
  Result := -1;
  Count := FScripts.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      if SameStr(FScripts.Items[i].Name,ANameScript) then
      begin
        Result := i;
        Break;
      end;
    end;
end;

function TScriptSQL.GetSQL(const ANameScript: String): String;
var
  xInd: Integer;
  xScript: TScript;
begin
  Result := '';
  xInd := Self.IndexOf(ANameScript);
  if xInd >= 0 then
  begin
    xScript := Self.Items[xInd];
    Result := xScript.SQL;
  end;
end;

class procedure TScriptSQL.Registration(const ANameScript: String);
begin
  if Assigned(ScriptSQL) then
     ScriptSQL.Add(ANameScript);
end;

initialization
  // Загружаем сервер списка данных
  ScriptSQL := TScriptSQL.Create;
  ScriptSQL.Add('patient_ward');    // Запрашиваем пациента в палате
  ScriptSQL.Add('doctor_bypass');   // Расписаие обхода
  ScriptSQL.Add('assigned_exam');   // Назачениные иследования
  ScriptSQL.Add('medications');     // Медецинские препарату
  ScriptSQL.Add('parametric');      // Параметричесие данные
  ScriptSQL.Add('research');        // Распорядок назначения на день
  ScriptSQL.Add('active_knc');      // Принадлежность
  ScriptSQL.Add('acrive_diabetics');// Склонность пациента к диабету

  // Критическая секция
  Section := TCriticalSection.Create;

finalization
  FreeAndNil(ScriptSQL);
  FreeAndNil(Section);

end.
