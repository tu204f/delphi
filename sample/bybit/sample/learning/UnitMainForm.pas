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
  FMX.Layouts,
  FMX.ListBox,
  FMX.Controls.Presentation,
  FMX.StdCtrls,
  FMX.Objects;

type
  TMainForm = class(TForm)
    ListBox1: TListBox;
    ButtonLearning: TButton;
    Timer1: TTimer;
    Text1: TText;
    Text2: TText;
    ButtonTest: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonLearningClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure ButtonTestClick(Sender: TObject);
  private
    { Private declarations }
  public
    CountEra, IndexFile: Integer;
    Files: TStringList;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

uses
  Lb.Source;

var
  Source: TSource = nil;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  Files := TStringList.Create;
  Source := TSource.Create;
end;

procedure TMainForm.Timer1Timer(Sender: TObject);
var
  xS: String;
  xFileName: String;
begin
  xS := '';
  if CountEra > 0 then
  begin
    if (IndexFile >= 0) and (IndexFile < (Files.Count - 1)) then
    begin
      xFileName := Files[Random(Files.Count)];
      Source.SetOpen(xFileName);
      Source.SetDepth(-1);

      xS := xS + 'L:' + Source.LongRSI.ToString + ';';
      xS := xS + 'M:' + Source.MediumRSI.ToString + ';';
      xS := xS + 'S:' + Source.ShortSRI.ToString + ';';

      Inc(IndexFile);
    end
    else
    begin
      IndexFile := 0;
      CountEra := CountEra - 1;
    end;
  end
  else
  begin
    Timer1.Enabled := False;
  end;
  Text1.Text := 'CountEra: ' + CountEra.ToString + '; IndexFile: ' + IndexFile.ToString;
  Text2.Text := xS;
end;

procedure TMainForm.ButtonTestClick(Sender: TObject);
begin
  var xPath := ExtractFilePath(ParamStr(0)) + 'data\';
  SetPathFiles(xPath, Files);

  ListBox1.Items.Add(Files.Text);

end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(Files);
  FreeAndNil(Source);
end;

procedure TMainForm.ButtonLearningClick(Sender: TObject);
var
  xPath: String;
begin
  IndexFile := 0;
  CountEra := 10;
  Timer1.Enabled := True;
  xPath := ExtractFilePath(ParamStr(0)) + 'data\';
  SetPathFiles(xPath, Files);
end;

end.
