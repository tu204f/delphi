unit UnitMainForm;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.IOUtils,
  FMX.Types,
  FMX.Controls,
  FMX.Forms,
  FMX.Graphics,
  FMX.Dialogs,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.ListBox;

type
  TMainForm = class(TForm)
    Button1: TButton;
    ListBox1: TListBox;
    Button2: TButton;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.ApplicationVersion,
  Lb.SearchFile,
  Lb.Params;

procedure TMainForm.Button1Click(Sender: TObject);
var
  SDA: TStringDynArray;
  i: Integer;
  xS: String;
begin
  //xS := TPath.GetDocumentsPath;
  xS := ExtractFilePath(ParamStr(0));
  // GetCreationTime, GetLastAccessTime, GetLastWriteTime
  ListBox1.Items.Add('file');
  SDA := TDirectory.GetFiles(xS, '*.*');
  for i := 0 to High(SDA) do
  begin
    var xFile := SDA[i];
    var xLastWriteTime := TFile.GetLastWriteTime(xFile);
    var xTmp := xFile + ' :: ' + DateTimeToStr(xLastWriteTime);
    Listbox1.Items.Add(xTmp);
  end;

  ListBox1.Items.Add('dir');
  SDA := TDirectory.GetDirectories(xS);
  for xS in SDA do
    Listbox1.Items.Add(xS);
end;

procedure SetCallBackParams(AParams: TParams);

  procedure SetLog(S: String);
  begin
    MainForm.ListBox1.Items.Add(S);
  end;

var
  xS: String;
  xLastWriteTime: TDateTime;
  xType: TTypeSearch;
begin
  xType := TTypeSearch(AParams.ParamByName('type').AsInteger);
  case xType of
    tpBegin: SetLog('begin');   // Начало поиска
    tpEnd: SetLog('end');     // Конец поиска
    tpAddFile: begin
      // Добавить файл
      xS := AParams.ParamByName('file').AsString;
      xLastWriteTime := AParams.ParamByName('last_write_time').AsDateTime;
      SetLog(xS + ' ' + DateTimeToStr(xLastWriteTime));
    end;
    tpAddDir: begin
      // Добавить папку
      xS := AParams.ParamByName('dir').AsString;
      SetLog(xS);
    end;
  end;
  Application.ProcessMessages;
end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  xPath: String;
  xInfoFiles: TInfoFiles;
begin
  xPath := 'd:\work\';
  xInfoFiles := TInfoFiles.Create;
  try
    SetSearchFile(xInfoFiles,xPath,SetCallBackParams);
  finally
    FreeAndNil(xInfoFiles);
  end;
end;

procedure TMainForm.FormShow(Sender: TObject);
var
  xS: String;
begin
  xS := 'Проливка: ' + GetApplicationVersion;
  {$IFDEF DEBUG}
  xS := xS + ' debug';
  {$ENDIF}
  Self.Caption := xS;
end;

end.
