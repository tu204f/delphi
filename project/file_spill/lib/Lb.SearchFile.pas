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


  ///<summary>Поиск фалов и экспорт их базу данных</summary>
  TSearchFiles = class(TObject)
  private
    FActive: Boolean;
    FPathDir: String;
    FOnStart: TNotifyEvent;
    FOnStop: TNotifyEvent;
  protected
    procedure DoStart;
    procedure DoStop;
    procedure SetSearchPathDir;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;
    property PathDir: String write FPathDir;
    property Active: Boolean read FActive;
    property OnStart: TNotifyEvent write FOnStart;
    property OnStop: TNotifyEvent write FOnStop;
  end;

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

procedure SetSearchFile(APath: String; ASearchFiles: TSearchFiles);
const
  TMP_FILE_DB = 'tmp_files.sqlite';
  REAL_FILE_DB = 'files.sqlite';
var
  localDB: TDataModuleDB;

  procedure SetBegin;
  begin

    // Посылаем сообщение
    TThread.Synchronize(nil,
      procedure ()
      begin
        ASearchFiles.DoStart;
      end
    );

    // Создание базы
    localStopSearch := False;
    var xFileDB := ExtractFilePath(ParamStr(0)) + TMP_FILE_DB;
    localDB := TDataModuleDB.Create(nil);
    localDB.DefaultConnection(xFileDB);
    var xSQL := TStringList.Create;
    try

      // Таблица папок
      with xSQL do
      begin
        Clear;
        Add('create table if not exists files(');
        Add(' file_name text,');
        Add(' path text,');
        Add(' ext text,');
        Add(' write_time text');
        Add(')');
      end;
      localDB.GetExecSQL(xSQL.Text);

      // Таблица файлов
      with xSQL do
      begin
        Clear;
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
  end;

  procedure SetEnd;
  begin
    // Разрыаем соединение
    if Assigned(localDB) then
      FreeAndNil(localDB);
    localDB := nil;

    // Событие завершение загрузки
    TThread.Synchronize(nil,
      procedure ()
      begin
        var xTmpFile := ExtractFilePath(ParamStr(0)) + TMP_FILE_DB;
        var xRealFile := ExtractFilePath(ParamStr(0)) + REAL_FILE_DB;
        if FileExists(xTmpFile) then
          SetCopy(xRealFile,xTmpFile);
        ASearchFiles.DoStop;
      end
    );
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

  end;

  procedure SetAddDir(const ADirName: String);
  begin
    {todo: дабовление ссылки}



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

procedure SetSearchFileThreading(APath: String; ASearchFiles: TSearchFiles);
var
  xTaks: ITask;
begin
  xTaks := TTask.Create(
    procedure()
    begin
      SetSearchFile(APath,ASearchFiles);
    end
  );
  xTaks.Start;
end;

{ TSearchFiles }

constructor TSearchFiles.Create;
begin
  FPathDir := '';
  FActive := False;
end;

destructor TSearchFiles.Destroy;
begin

  inherited;
end;

procedure TSearchFiles.DoStart;
begin
  FActive := True;
  if Assigned(FOnStart) then FOnStart(Self);
end;

procedure TSearchFiles.DoStop;
begin
  if Assigned(FOnStop) then FOnStop(Self);
end;

procedure TSearchFiles.SetSearchPathDir;
begin
  if DirectoryExists(FPathDir) then
    SetSearchFileThreading(FPathDir,Self);
end;

procedure TSearchFiles.Start;
begin
  if not FActive then
  begin
    SetSearchPathDir;
  end;
end;

procedure TSearchFiles.Stop;
begin
  if FActive then
  begin
    FActive := False;
    SetStopSearchFile;
  end;
end;

end.
