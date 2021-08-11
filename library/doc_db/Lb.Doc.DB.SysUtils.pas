unit Lb.Doc.DB.SysUtils;

interface

uses
  Lb.Doc.DB;

implementation

(******************************************************************************)
(* Процедуры для работы с API                                                 *)
(******************************************************************************)

function OpenDataBase(const ANameFile: String): TDocFile;
begin
  /// Открыть базу данных
end;

function CloseDataBase(const ADataBaseID: Integer): Boolean;
begin
  // Закрыть базу данных
  Result := False;
end;


(******************************************************************************)
(* Проедуры для работы с папками                                              *)
(******************************************************************************)

function GetNewFolder(const ADocFile: TDocFile; const ANewNameFolder: String): TFolder;
begin
  // В ложить в текущию папку, новую с именим ANewNameFolder
  Result := nil;
end;

function GetReNameFolder(const ADocFile: TDocFile; const ANewNameFolder, AOldNameFolder: String): TFolder;
begin
  // Пере именовать папку
  Result := nil;
end;

function GetDeleteFolder(const ADocFile: TDocFile; const ANewNameFolder: String): Boolean;
begin
  Result := False;
end;

// Открыть
// Зарыть папку


(******************************************************************************)
(* Проедуры для работы с документом                                           *)
(******************************************************************************)



end.
