unit Lb.SearchFile;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IOUtils,
  System.Generics.Collections,
  Lb.Params;

type
  TTypeSearch = (
    tpBegin,   // Начало поиска
    tpEnd,     // Конец поиска
    tpAddFile, // Добавить файл
    tpAddDir   // Добавить папку
  );

  TSearchFile = class(TObject)
  private
  public

  end;

///<summary>Поиск файл: в папке</summary>
procedure SetSearchFile(APath: String; ACallBackParams: TCallBackParams = nil);
///<summary>Остановить поиск файла</summary>
procedure SetStopSearchFile;
///<summary>Создаем отдельный запуска поток</summary>
procedure SetSearchFileThreading(APath: String);


implementation

uses
  System.Threading, Lb.DataModuleDB, Lb.SysUtils.ISO860;

var
  localStopSearch: Boolean = False;

procedure SetStopSearchFile;
begin
  localStopSearch := True;
end;

procedure SetCopy(ADest, ASource: String);
var
  xDest: TFileStream;
  xSource: TFileStream;
begin
  xSource := TFileStream.Create(ASource,fmOpenRead);
  try
    xSource.Position := 0;
    xDest := TFileStream.Create(ADest,fmCreate);
    try
      xDest.CopyFrom(xSource);
    finally
      FreeAndNil(xDest);
    end;
  finally
    FreeAndNil(xSource);
  end;
end;

procedure SetSearchFile(APath: String; ACallBackParams: TCallBackParams);
const
  TMP_FILE_DB = 'tmp_files.sqlite';
  REAL_FILE_DB = 'files.sqlite';
var
  localDB: TDataModuleDB;

  procedure SetBegin;
  begin
    localStopSearch := False;

    var xFileDB := ExtractFilePath(ParamStr(0)) + TMP_FILE_DB;
    localDB := TDataModuleDB.Create(nil);
    localDB.DefaultConnection(xFileDB);
    var xSQL := TStringList.Create;
    try
      with xSQL do
      begin
        Add('create table if not exists files(');
        Add(' file_name text,');
        Add(' path text,');
        Add(' ext text,');
        Add(' write_time text');
        Add(')');
      end;
      localDB.GetExecSQL(xSQL.Text);
      localDB.GetExecSQL('delete from files');
    finally
      FreeAndNil(xSQL);
    end;

    if Assigned(ACallBackParams) then
    begin
      var xParams := TParams.Create;
      try
        with xParams do
        begin
          ParamByName('type').AsInteger := Integer(TTypeSearch.tpBegin);
        end;
        ACallBackParams(xParams);
      finally
        FreeAndNil(xParams);
      end;
    end;
  end;

  procedure SetEnd;
  begin

    if Assigned(localDB) then
      FreeAndNil(localDB);
    localDB := nil;

    var xTmpFile := ExtractFilePath(ParamStr(0)) + TMP_FILE_DB;
    var xRealFile := ExtractFilePath(ParamStr(0)) + REAL_FILE_DB;
    if FileExists(xTmpFile) then
      SetCopy(xRealFile,xTmpFile);

    if Assigned(ACallBackParams) then
    begin
      var xParams := TParams.Create;
      try
        with xParams do
        begin
          ParamByName('type').AsInteger := Integer(TTypeSearch.tpEnd);
        end;
        ACallBackParams(xParams);
      finally
        FreeAndNil(xParams);
      end;
    end;
  end;

  procedure SetAddFile(const AFileName: String; const ALastWriteTime: TDateTime);
  begin

    var xSQL := TStringList.Create;
    with xSQL do
    begin
      Add('insert into files');
      Add('(file_name, path, ext, write_time)');
      Add('values (:file_name, :path, :ext, :write_time)');
    end;

    if Assigned(localDB) then
    begin
      var _FileName := ExtractFileName(AFileName);
      var _Path := ExtractFilePath(AFileName);
      var _Ext := ExtractFileExt(AFileName);
      var _WriteTime := GetDateTimeToStrISO860(ALastWriteTime);
      localDB.GetExecSQL(xSQL.Text,[_FileName, _Path, _Ext, _WriteTime]);
    end;

    if Assigned(ACallBackParams) then
    begin
      var xParams := TParams.Create;
      try
        with xParams do
        begin
          ParamByName('type').AsInteger := Integer(TTypeSearch.tpAddFile);
          ParamByName('file').AsString := AFileName;
          ParamByName('last_write_time').AsDateTime := ALastWriteTime;
        end;
        ACallBackParams(xParams);
      finally
        FreeAndNil(xParams);
      end;
    end;
  end;

  procedure SetAddDir(const ADirName: String);
  begin
    if Assigned(ACallBackParams) then
    begin
      var xParams := TParams.Create;
      try
        with xParams do
        begin
          ParamByName('type').AsInteger := Integer(TTypeSearch.tpAddDir);
          ParamByName('dir').AsString := ADirName;
        end;
        ACallBackParams(xParams);
      finally
        FreeAndNil(xParams);
      end;
    end;
  end;

  procedure SetSearchDir(APath: String);
  var
    xS: String;
    xSDA: TStringDynArray;
  begin
    SetAddDir(APath);
    // Перебираем все фалы
    xSDA := TDirectory.GetFiles(APath, '*.*');
    for xS in xSDA do
    begin
      if localStopSearch then
        Break;
      var xLastWriteTime := TFile.GetLastWriteTime(xS);
      SetAddFile(xS,xLastWriteTime);
    end;
    // Перебираем все папки
    if not localStopSearch then
    begin
      xSDA := TDirectory.GetDirectories(APath);
      for xS in xSDA do
      begin
        if localStopSearch then
          Break;
        SetSearchDir(xS);
      end;
    end;
  end;

begin
  SetBegin;
  SetSearchDir(APath);
  SetEnd;
end;

procedure SetSearchFileThreading(APath: String);
var
  xTaks: ITask;
begin
  xTaks := TTask.Create(
    procedure()
    begin
      SetSearchFile(APath);
    end
  );
  xTaks.Start;
end;

end.
