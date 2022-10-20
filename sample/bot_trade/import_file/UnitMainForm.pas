unit UnitMainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Lb.Candel.ImportFile,
  Lb.Candel.Source,
  Lb.Candel.SysUtils, Vcl.ComCtrls;

type
  TMainForm = class(TForm)
    MemoLines: TMemo;
    ButtonStart: TButton;
    ButtonLoad: TButton;
    ProgressBar1: TProgressBar;
    procedure ButtonStartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    procedure SetLogCandel(const ACandel: TCandel);
    procedure SetLogCandels(const ACandels: TCandelList);
    procedure SetLogStructure(const AStructure: TStructure);
    { Private declarations }
  public
    { Public declarations }
    procedure SetLog(S: String = '');
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

const
  FILE_NAME = 'd:\work\git\delphi\sample\bot_trade\bin\data\sber\data.csv';

var
  localStructures: TStructureList = nil;

{ TMainForm }

procedure TMainForm.FormCreate(Sender: TObject);
begin
  localStructures := TStructureList.Create;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(localStructures);
end;

procedure TMainForm.SetLog(S: String);
begin
  MemoLines.Lines.Add(S);
end;

procedure TMainForm.SetLogCandel(const ACandel: TCandel);
begin
  SetLog('  >> date time: ' + DateTimeToStr(ACandel.DateTime));
  SetLog('  >> open : ' + ACandel.Open.ToString);
  SetLog('  >> high : ' + ACandel.High.ToString);
  SetLog('  >> low  : ' + ACandel.Low.ToString);
  SetLog('  >> close: ' + ACandel.Close.ToString);
  SetLog('  >> vol  : ' + ACandel.Vol.ToString);
  SetLog();
end;

procedure TMainForm.SetLogCandels(const ACandels: TCandelList);
var
  xCandel: TCandel;
begin
  for xCandel in ACandels do
    SetLogCandel(xCandel);
end;

procedure TMainForm.SetLogStructure(const AStructure: TStructure);
begin
  SetLog('>> -- source');
  SetLogCandels(AStructure.SourceVectors);
  SetLog('>> -- vestor');
  SetLogCandels(AStructure.FutureVectors);
end;

procedure TMainForm.ButtonStartClick(Sender: TObject);
var
  xVectorStructure: TVectorStructure;
  xMemoryStructures: TMemoryStructures;
begin
  try
    xMemoryStructures := TMemoryStructures.Create;
    try
      xMemoryStructures.SourceCount := 20;
      xMemoryStructures.FutureCount := 5;
      xMemoryStructures.FileName := FILE_NAME;

      ProgressBar1.Min := 0;
      ProgressBar1.Max := xMemoryStructures.Count;

      xMemoryStructures.FirstStructure;
      while not xMemoryStructures.EOF do
      begin
        xVectorStructure := TVectorStructure.Create;
        ProgressBar1.Position := xMemoryStructures.Structure.SourceRowID;
        xVectorStructure.Transform(xMemoryStructures.Structure);
        localStructures.Add(xVectorStructure);
        xMemoryStructures.NextStructure;
        Application.ProcessMessages;
      end;
    finally
      FreeAndNil(xMemoryStructures);
    end;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end;

end.
