unit UnitMainForm;

interface

uses
  Winapi.Windows,
  Winapi.Messages,
  System.SysUtils,
  System.Variants,
  System.Classes,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.Forms,
  Vcl.Dialogs,
  Vcl.StdCtrls,
  Vcl.ExtCtrls,
  Lb.Sort, Vcl.ComCtrls;

type
  TMainForm = class(TForm)
    OpenDialog: TOpenDialog;
    ButtonOpen: TButton;
    ButtonStart: TButton;
    LabelStatus: TLabel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    EditFileName: TEdit;
    EditFileNameIndex: TEdit;
    EditFileNameSort: TEdit;
    ProgressBar: TProgressBar;
    procedure FormShow(Sender: TObject);
    procedure ButtonStartClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
    Sort: TSort;
    Path: String;
    FileName: String;
    FileNameIndex: String;
    FileNameSort: String;
  public
    { Public declarations }
    procedure SortChange(Sender: TObject; const AParam: TParam);
    procedure EndSort(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Lb.StringsFile,
  Lb.SysUtils;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Sort := TSort.Create;
  Sort.OnEndSort := EndSort;
  Sort.OnSortChange := SortChange;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(Sort);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  Self.Caption := 'Программа сортировки текстового файла';
end;

procedure TMainForm.ButtonOpenClick(Sender: TObject);
var
  xSourceFileName: String;
  //xFileName: String;
  xTmpFileName: String;
  //xFileNameIndex: String;
  //xFileNameSort: String;
  xExt: String;
  xIndex: Integer;
begin
  if OpenDialog.Execute then
  begin
    xSourceFileName := OpenDialog.FileName;

    Self.Path     := ExtractFilePath(xSourceFileName);
    Self.FileName := ExtractFileName(xSourceFileName);
    xExt          := ExtractFileExt(xSourceFileName);


    xIndex := Pos(xExt,Self.FileName);
    if xIndex > 1 then
      xTmpFileName := Copy(Self.FileName,1,xIndex - 1);

    Self.FileNameIndex := xTmpFileName + '_index.sqlite';
    Self.FileNameSort  := xTmpFileName + '_sort.txt';

    EditFileName.Text := Self.FileName;
    EditFileNameIndex.Text := Self.FileNameIndex;
    EditFileNameSort.Text := Self.FileNameSort;

  end;
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
var
  xFileName, xFileNameResult, xFileNameDB: String;
begin
  if Sort.IsActive then
  begin
    Sort.Stop;
    ButtonStart.Caption := 'Стар';
  end
  else
  begin
    xFileName       := Self.Path + Self.FileName;
    xFileNameResult := Self.Path + Self.FileNameSort;
    xFileNameDB     := Self.Path + Self.FileNameIndex;
    Sort.Start(xFileName,xFileNameResult,xFileNameDB);
    ButtonStart.Caption := 'Стоп';
  end;
end;

procedure TMainForm.SortChange(Sender: TObject; const AParam: TParam);
begin
  case AParam.TypeObject of
    0: LabelStatus.Caption := 'Состояние сортировки: Создание индекса ' + IntToStr(AParam.Procent) + '%';
    1: LabelStatus.Caption := 'Состояние сортировки: Создание файла ' + IntToStr(AParam.Procent) + '%';
  end;
  ProgressBar.Position := AParam.Procent;
end;

procedure TMainForm.EndSort(Sender: TObject);
begin
  ButtonStart.Caption := 'Стар';
end;



end.
