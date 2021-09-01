unit Lb.SearchFile;

interface

uses
//  System.SysUtils,
//  System.Variants,
//  System.Classes,
//  System.Generics.Collections,
//  System.IOUtils,

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

  ///<summary>Информация файлов</summary>
  TInfoFile = class(TObject)
  private
    FFileName: String;
    FPath: String;
    FDateTime: TDateTime;
  public
    ///<summary>Параметры файлы</summary>
    procedure SetInfoFile(const APathFileName: String; ADateTime: TDateTime);
    ///<summary>Имя файла</summary>
    property FileName: String read FFileName;
    ///<summary>Путь к файлу</summary>
    property Path: String read FPath;
    ///<summary>Дата и время</summary>
    property DateTime: TDateTime read FDateTime;
  end;

  ///<summary>Список файлов</summary>
  TInfoFiles = class(TObjectList<TInfoFile>)
  private
  public
    function IndexOfDateTime(const ADateTime: TDateTime): Integer;
    procedure SetInfoFile(const APathFileName: String; ADateTime: TDateTime);
  end;

///<summary>Поиск файл: в папке</summary>
procedure SetSearchFile(AInfoFiles: TInfoFiles; APath: String; ACallBackParams: TCallBackParams = nil);

implementation

uses
  System.Threading;

procedure SetSearchFile(AInfoFiles: TInfoFiles; APath: String; ACallBackParams: TCallBackParams = nil);

  procedure SetBegin;
  begin
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


  procedure SetSearchDir(AInfoFiles: TInfoFiles; APath: String);
  var
    xS: String;
    xSDA: TStringDynArray;
  begin
    SetAddDir(APath);
    // Файлы
    xSDA := TDirectory.GetFiles(APath, '*.*');
    for xS in xSDA do
    begin
      var xLastWriteTime := TFile.GetLastWriteTime(xS);
      AInfoFiles.SetInfoFile(xS,xLastWriteTime);
      SetAddFile(xS,xLastWriteTime);
    end;
    // Папки
    xSDA := TDirectory.GetDirectories(APath);
    for xS in xSDA do
      SetSearchDir(AInfoFiles,xS);
  end;


begin
  if not Assigned(AInfoFiles) then
    Exit;
  SetBegin;
  SetSearchDir(AInfoFiles,APath);
  SetEnd;
end;

{ TInfoFile }

procedure TInfoFile.SetInfoFile(const APathFileName: String; ADateTime: TDateTime);
begin
  FPath     := ExtractFilePath(APathFileName);
  FFileName := ExtractFileName(APathFileName);
  FDateTime := ADateTime;
end;

{ TInfoFiles }

function TInfoFiles.IndexOfDateTime(const ADateTime: TDateTime): Integer;
var
  xInfoFile: TInfoFile;
  i, Count: Integer;
begin
  Result := -1;
  Count := Self.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xInfoFile := Self.Items[i];
      if xInfoFile.DateTime < ADateTime then
      begin
        Result := i;
        Break;
      end;
    end;
end;

procedure TInfoFiles.SetInfoFile(const APathFileName: String; ADateTime: TDateTime);
var
  xInfoFile: TInfoFile;
begin
  xInfoFile := TInfoFile.Create;
  xInfoFile.SetInfoFile(APathFileName,ADateTime);
  var xIndex :=  Self.IndexOfDateTime(ADateTime);
  if xIndex >= 0 then
  begin
    xIndex := IndexOfDateTime(ADateTime);
    Self.Insert(xIndex,xInfoFile);
  end
  else
    Self.Add(xInfoFile);
end;

end.
