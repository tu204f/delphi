unit Lb.SysUtils;

interface

uses
  System.SysUtils,
  System.Variants,
  System.Classes,
  System.Generics.Collections;


implementation

uses
  System.Threading,
  Lb.Params,
  Lb.SearchFile;

{todo: Нужно перенести архив хранение файлов}
procedure SetCallBackParams(AParams: TParams);
var
  xS: String;
  xLastWriteTime: TDateTime;
  xType: TTypeSearch;
begin
  xType := TTypeSearch(AParams.ParamByName('type').AsInteger);
  case xType of
    tpBegin: ; //SetLog('begin');   // Начало поиска
    tpEnd: ;//SetLog('end');     // Конец поиска
    tpAddFile: begin
      // Добавить файл
      xS := AParams.ParamByName('file').AsString;
      xLastWriteTime := AParams.ParamByName('last_write_time').AsDateTime;
      //SetLog(xS + ' ' + DateTimeToStr(xLastWriteTime));
    end;
    tpAddDir: begin
      // Добавить папку
      xS := AParams.ParamByName('dir').AsString;
      //SetLog(xS);
    end;
  end;
end;

procedure SetSearchFileThreading(APath: String);
var
  xTaks: ITask;
begin
  xTaks := TTask.Create(
    procedure()
    var
      xInfoFiles: TInfoFiles;
    begin
      xInfoFiles := TInfoFiles.Create;
      try
        SetSearchFile(xInfoFiles,APath,SetCallBackParams);
      finally
        FreeAndNil(xInfoFiles);
      end;
    end
  );
  xTaks := nil;
end;

end.
