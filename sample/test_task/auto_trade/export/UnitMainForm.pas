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
  Vcl.ComCtrls,
  Vcl.StdCtrls, Vcl.ExtCtrls;

type
  TMainForm = class(TForm)
    TreeView: TTreeView;
    Button1: TButton;
    ProgressBar: TProgressBar;
    Timer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    { Private declarations }
    procedure SetSearchFiles;
  public
    { Public declarations }

    procedure ExportFullOnBegin(Sender: TObject);
    procedure ExportFullOnEnd(Sender: TObject);

  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Lb.Search.Files,
  Lb.Export,
  Lb.StringsFile;

var
  localExportFull: TExportCrossBrendFull;


procedure TMainForm.FormShow(Sender: TObject);
begin
  SetSearchFiles;
end;

procedure TMainForm.SetSearchFiles;

  procedure SetTreeNode(ATreeNode: TTreeNode; ADirect: TDirect);
  var
    xFile: TDirect.TFile;
    xDirect: TDirect;
    xTreeNode: TTreeNode;
  begin
    xTreeNode := TreeView.Items.AddChild(ATreeNode,ADirect.Name);

    for xDirect in ADirect.Directs do
      SetTreeNode(xTreeNode,xDirect);

    for xFile in ADirect.Files do
      TreeView.Items.AddChild(xTreeNode,xFile.Name);
  end;

var
  xDirect: TDirect;
begin
  xDirect := TDirect.Create;
  try
    xDirect.Path := ExtractFilePath(ParamStr(0)) + 'FAPI_2017_10';
    TreeView.Items.Clear;
    SetTreeNode(nil,xDirect);
  finally
    FreeAndNil(xDirect);
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
var
  xStringsFile: TStringsFile;
begin
  xStringsFile := TStringsFile.Create('d:\work\git\delphi\sample\test_task\auto_trade\bin\FAPI_2017_10\dump_full\dump_full.txt');
  localExportFull := TExportCrossBrendFull.Create;
  localExportFull.StringsFiles := xStringsFile;
  localExportFull.FileNameDataBase := 'cross_auto.sqlite';

  localExportFull.OnBegin := ExportFullOnBegin;
  localExportFull.OnEnd := ExportFullOnEnd;

  localExportFull.Start;
end;

procedure TMainForm.ExportFullOnBegin(Sender: TObject);
begin
  Timer.Enabled := True;
end;

procedure TMainForm.ExportFullOnEnd(Sender: TObject);
begin
  localExportFull := nil;
end;

procedure TMainForm.TimerTimer(Sender: TObject);
begin
  if Assigned(localExportFull) then
  begin
    ProgressBar.Position := localExportFull.Procent;
  end
  else
    Timer.Enabled := False;
end;

end.
